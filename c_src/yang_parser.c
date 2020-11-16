#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <errno.h>
#include <assert.h>

#include "yang_parser.h"
#include "yang_atom.h"
#include "yang_error.h"

static yang_atom_t am_yang_version;

/* tokenizer state */
struct toks {
    FILE *f;
    struct yang_error_ctx *ectx;
    char *filename;
    int line;
    char *buf;
    size_t bufn;
    char *p; /* pointer into buf */
    bool expect_eof;
    char yang_version;
};

#define isidentifier1(X) (((X) >= 'a' && (X) <= 'z') || \
                          ((X) >= 'A' && (X) <= 'Z') || \
                          ((X) == '_'))
#define isidentifier2(X) (((X) >= 'a' && (X) <= 'z') || \
                          ((X) >= 'A' && (X) <= 'Z') || \
                          ((X) >= '0' && (X) <= '9') || \
                          ((X) == '_') ||               \
                          ((X) == '.') ||               \
                          ((X) == '-'))

#define is_space(X) ((X) == ' ')
#define is_wspace(X) (is_space(X) || (X) == '\t') // RFC 6020 spec
#define is_wspace_lf(X) (is_wspace(X) || (X) == '\n')
#define is_wspace_cr(X) (is_wspace(X) || (X) == '\r')
#define is_crlf(X) (*(X) == '\r' && *(X+1) == '\n')

static bool
tok_readline(struct toks *toks)
{
    char *p = toks->buf;
    size_t n = toks->bufn;
    size_t len = 0;

    while (fgets(p, n, toks->f) != NULL) {
        len += strlen(p);
        if (toks->buf[len-1] == '\n')
            break;
        toks->bufn += BUFSIZ;
        toks->buf = (char *)realloc(toks->buf, toks->bufn * sizeof(char));
        p = &toks->buf[len];
        n = BUFSIZ;
    }
    if (len == 0) {
        if (!toks->expect_eof) {
            yang_add_err_gen(toks->ectx, YANG_ERR_PARSE_EOF,
                             toks->filename, toks->line, -1,
                             "premature end of file");
        }
        return false;
    }
    toks->p = toks->buf;
    toks->line++;
    return true;
}

static bool
tok_init(struct toks *toks, char *filename, struct yang_error_ctx *ectx)
{
    if ((toks->f = fopen(filename, "r")) == NULL) {
        yang_add_err_gen(ectx, YANG_ERR_PARSE_FOPEN, filename, 0, -1,
                         strerror(errno));
        return false;
    }
    toks->ectx = ectx;
    toks->filename = filename;
    toks->line = 0;
    toks->buf = (char *)malloc(BUFSIZ * sizeof(char));
    toks->bufn = BUFSIZ;
    toks->p = toks->buf;
    toks->expect_eof = false;
    toks->yang_version = YANG_VERSION_1;
    if (!tok_readline(toks)) {
        fclose(toks->f);
        free(toks->buf);
        return false;
    }
    return true;
}

static void
tok_free(struct toks *toks)
{
    fclose(toks->f);
    free(toks->buf);
}

static bool
tok_skip(struct toks *toks)
{
    char *p;

    p = toks->p;
    while (1) {
        if (*p == '\0') {
            if (!tok_readline(toks)) {
                return false;
            }
            p = toks->p;
        } else if (is_wspace_lf(*p) || is_crlf(p)) {
            p++;
        }
        else if (*p == '/') {
            if (*(p+1) == '/') {
                // skip line comment
                if (!tok_readline(toks)) {
                    return false;
                }
                p = toks->p;
            } else if (*(p+1) == '*') {
                // skip block comment
                char *c = NULL;
                while (!c) {
                    c = strstr(p, "*/");
                    if (!c) {
                        if (!tok_readline(toks)) {
                            return false;
                        }
                        p = toks->p;
                    }
                }
                p = c + 2;
            } else {
                toks->p = p;
                return true;
            }
        } else {
            toks->p = p;
            return true;
        }
    }
    toks->p = p;
    return true;
}

static bool
looking_at_separator(char *p)
{
    if (*p == '\0') {
        return false;
    }
    if (is_wspace_lf(*p) || *p == ';' || *p == '{' || is_crlf(p) ) {
        return true;
    }
    if (*p == '/' && (*(p+1) == '/' || *(p+1) == '*')) {
        return true;
    }
    return false;
}

static bool
tok_get_keyword(struct toks *toks,
                yang_atom_t *prefix, yang_atom_t *keyword)
{
    char *p, *s;

    p = toks->p;
    s = p;
    if (!(isidentifier1(*p))) {
        yang_add_err_gen(toks->ectx, YANG_ERR_PARSE_BAD_KEYWORD,
                         toks->filename, toks->line, 1 + p - toks->buf,
                         "invalid keyword start character \"%c\"", *p);
        return false;
    }
    p++;
    while (isidentifier2(*p)) {
        p++;
    }
    if (*p == ':') {
        *prefix = yang_make_atom_len(s, p-s);
        p++;
        s = p;
        if (!(isidentifier1(*p))) {
            yang_add_err_gen(toks->ectx, YANG_ERR_PARSE_BAD_KEYWORD,
                             toks->filename, toks->line, 1 + p - toks->buf,
                             "invalid keyword character \"%c\"", *p);
            return false;
        }
        p++;
        while (isidentifier2(*p)) {
            p++;
        }
    } else {
        *prefix = NULL;
    }
    *keyword = yang_make_atom_len(s, p-s);
    toks->p = p;
    if (!looking_at_separator(toks->p)) {
        yang_add_err_gen(toks->ectx, YANG_ERR_PARSE_EXPECTED_SEPARATOR,
                         toks->filename, toks->line, 1 + p - toks->buf,
                         "expected token separator");
        return false;
    }
    return true;
}

static bool
tok_get_string(struct toks *toks, char **str)
{
    char *p, *s=NULL, *q, quotechar, *new, *tmp;
    int indentpos, i, len, curlen, sz;
    bool have_parts;

    if (!tok_skip(toks)) {
        return false;
    }
    p = toks->p;
    if (*p == ';' || *p == '{' || *p == '}') {
        yang_add_err_gen(toks->ectx, YANG_ERR_PARSE_EXPECTED_STRING,
                         toks->filename, toks->line, 1 + p - toks->buf,
                         "expected string");
        return false;
    }
    if (*p != '"' && *p != '\'') {
        // unquoted string
        s = toks->p;
        while (!looking_at_separator(toks->p)
               && *toks->p != '}' && *toks->p != '"' && *toks->p != '\'') {
            toks->p++;
        }
        len = toks->p - s;
        *str = malloc((len+1) * sizeof(char));
        strncpy(*str, s, len);
        (*str)[len] = '\0';
        return true;
    }
    // quoted string
    *str = NULL;
    len = 0;
    have_parts = true;
    while (have_parts) {
        quotechar = *p++;
        indentpos = p - toks->buf;
        for (tmp = toks->buf; tmp < p; tmp++) {
            if (*tmp == '\t') {
                indentpos += 7; // 1 is already accounted for in indentpos
            }
        }
        if (*p == quotechar) {
            /* empty string, need to store the end '\0' */
            new = (char *)realloc(*str, (len+1) * sizeof(char));
            if (!new) {
                goto alloc_error;
            }
            *str = new;
            new[len] = '\0';
        }
        while (*p != quotechar) {
            /* toks->buf contains a line, and p points to the first
               character after indentation */
            s = p;
            curlen = len;
            /* update len so that it stores the number of chars needed */
            if (quotechar == '\'') {
                //p = strchrnul(p, '\'');
                tmp = strchr(p, '\'');
                if (tmp) {
                    p = tmp;
                } else {
                    p += strlen(p);
                }
                len += p-s;
            } else {
                /* double quote; count chars, check for quoted chars */
                for (; *p != '\0' && *p != '"'; p++) {
                    if (*p == '\\') {
                        if (*(p+1) == 'n' || *(p+1) == '"' ||
                            *(p+1) == '\\' || *(p+1) == 't') {
                            p++;
                        }
                    }
                    len++;
                }
            }
            /* end of line, or end of this string */
            if (*p == quotechar) {
                /* make room for the '\0' char */
                sz = len + 1;
            } else {
                if (quotechar == '\"'
                    && p-s > 1 && *(p-1) == '\n'
                    && is_wspace_cr(*(p-2)))
                {
                    bool cr_found = false;
                    /* remove trailing whitespace */
                    p -= 2; // skip ['\r'] '\n' '\0'
                    if (*p == '\r') {
                        cr_found = true;
                        p--;
                    }
                    while (p >= s && is_wspace(*p)) {
                        p--;
                        len--;
                    }
                    /* put back ['\r'] '\n' '\0' */
                    if (cr_found) {
                        *++p = '\r';
                    }
                    *++p = '\n';
                    *++p = '\0';
                }
                sz = len;
            }
            new = (char *)realloc(*str, sz * sizeof(char));
            if (!new) {
                goto alloc_error;
            }
            *str = new;
            /* copy the new data to the result string */
            if (quotechar == '\'') {
                strncpy(new+curlen, s, p-s);
                if (*p == quotechar) {
                    new[len] = '\0';
                }
            } else {
                /* double quote; copy chars, translate quoted chars */
                for (q = new+curlen; s != p; s++, q++) {
                    if (*s == '\\') {
                        if (*(s+1) == 'n') {
                            s++;
                            *q = '\n';
                        } else if (*(s+1) == 't') {
                            s++;
                            *q = '\t';
                        } else if (*(s+1) == '"') {
                            s++;
                            *q = '"';
                        } else if (*(s+1) == '\\') {
                            s++;
                            *q = '\\';
                        } else {
                            if (toks->yang_version == YANG_VERSION_1) {
                                yang_add_err_gen(
                                    toks->ectx,
                                    YANG_WARN_PARSE_ILLEGAL_ESCAPE,
                                    toks->filename, toks->line,
                                    1 + s - toks->buf,
                                    "illegal character after \\");
                            } else {
                                yang_add_err_gen(
                                    toks->ectx,
                                    YANG_ERR_PARSE_ILLEGAL_ESCAPE,
                                    toks->filename, toks->line,
                                    1 + s - toks->buf,
                                    "illegal character after \\");
                            }
                            *q = *s;
                        }
                    } else {
                        *q = *s;
                    }
                }
                if (*p == quotechar) {
                    *q = '\0';
                }
                assert(len == q - new);
            }
            if (*p == '\0') {
                /* end of line, read a new line */
                if (!tok_readline(toks)) {
                    goto error;
                }
                p = toks->p;
                if (quotechar == '"') {
                    int stop = 0;
                    /* skip whitespace used for indentation only */
                    i = 0;
                    while (i < indentpos && !stop) {
                        switch (*p) {
                        case ' ':
                            p++;
                            i++;
                            break;
                        case '\t':
                            /* this might not be correct.  we will
                              remove the tab, but we don't insert
                              additional spaces if needed.  unclear if
                              that is necessery though. */
                            p++;
                            i += 8;
                            break;
                        default:
                            stop = 1;
                        }
                    }
                    if (*p == quotechar) {
                        /* end of string, make room for the '\0' char */
                        sz++;
                        new = (char *)realloc(*str, sz * sizeof(char));
                        if (!new) {
                            goto alloc_error;
                        }
                        new[sz-1] = '\0';
                        *str = new;
                    }
                }
            } else {
                /* end of string */
                assert(*p == quotechar);
            }
        }
        /* p is now quotechar, check for concatenation */
        p++;
        toks->p = p;
        if (!tok_skip(toks)) {
            goto error;
        }
        if (*toks->p == '+') {
            /* string concatenation */
            toks->p++;
            if (!tok_skip(toks)) {
                goto error;
            }
            p = toks->p;
            if (*p == '"' || *p == '\'') {
                continue;
            } else {
                yang_add_err_gen(toks->ectx,
                                 YANG_ERR_PARSE_EXPECTED_QUOTED_STRING,
                                 toks->filename, toks->line, 1 + p - toks->buf,
                                 "expected quoted string after '+' operator");
                goto error;
            }
        } else {
            have_parts = false;
        }
    }
    return true;

alloc_error:
    yang_add_alloc_err(toks->ectx, len + (p-s));
error:
    if (*str) {
        free(*str);
        *str = NULL;
    }
    return false;
}

static bool
parse_statement(struct toks *toks, struct yang_statement **stmt)
{
    yang_atom_t prefix;
    yang_atom_t keyword;
    char *p, *arg;
    int line;
    struct yang_statement *tmp, **next;

    arg = NULL;
    *stmt = NULL;

    if (!tok_skip(toks)) {
        return false;
    }
    line = toks->line;
    if (!tok_get_keyword(toks, &prefix, &keyword)) {
        return false;
    }

    if (!tok_skip(toks)) {
        return false;
    }
    p = toks->p;
    if (!(*p == '{' || *p == ';')) {
        if (!tok_get_string(toks, &arg)) {
            return false;
        }
    }
    if (!tok_skip(toks)) {
        return false;
    }
    *stmt = (struct yang_statement *)malloc(sizeof(struct yang_statement));
    (*stmt)->prefix = prefix;
    (*stmt)->module_name = NULL;
    (*stmt)->keyword = keyword;
    (*stmt)->arg = arg;
    (*stmt)->arg_type = NULL;
    (*stmt)->filename = toks->filename;
    (*stmt)->line = line;
    (*stmt)->next = NULL;
    (*stmt)->substmt = NULL;

    if (keyword == am_yang_version && arg && strcmp(arg, "1.1") == 0) {
        toks->yang_version = YANG_VERSION_1_1;
    }

    if (*toks->p == ';') {
        toks->p++;
    } else if (*toks->p == '{') {
        toks->p++;
        if (!tok_skip(toks)) {
            goto error;
        }
        next = &(*stmt)->substmt;
        while (*toks->p != '}') {
            if (!parse_statement(toks, &tmp)) {
                goto error;
            }
            *next = tmp;
            next = &tmp->next;
            if (!tok_skip(toks)) {
                goto error;
            }
        }
        toks->p++; // consume the '}'
    } else {
        yang_add_err_gen(toks->ectx, YANG_ERR_PARSE_INCOMPLETE_STATEMENT,
                         toks->filename, toks->line, 1 + toks->p - toks->buf,
                         "unterminated statement for keyword \"%s\"",
                         keyword);
        return false;
    }
    return true;
error:
    yang_free_tree(*stmt);
    *stmt = NULL;
    return false;
}

bool
yang_init_parser()
{
    am_yang_version = yang_make_atom("yang-version");
    return true;
}

void
yang_free_statement(struct yang_statement *stmt)
{
    if (stmt) {
        if (stmt->arg) {
            free(stmt->arg);
        }
        free(stmt);
    }
}

void
yang_free_tree(struct yang_statement *stmt)
{
    if (!stmt) {
        return;
    }

    yang_free_tree(stmt->substmt);
    yang_free_tree(stmt->next);
    yang_free_statement(stmt);
}

bool
yang_parse(char *filename,
           struct yang_statement **stmt,
           struct yang_error_ctx *ectx)
{
    struct toks toks;
    bool r;

    if (!tok_init(&toks, filename, ectx)) {
        return false;
    }
    r = parse_statement(&toks, stmt);
    toks.expect_eof = true;
    if (r && tok_skip(&toks)) {
        yang_add_err_gen(ectx, YANG_ERR_PARSE_TRAILING_GARBAGE,
                         filename, toks.line, 1 + toks.p - toks.buf,
                         "trailing garbage after module");
    }
    tok_free(&toks);
    return r;
}
