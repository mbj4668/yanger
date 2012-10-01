#define _GNU_SOURCE // for getline, strndup, strchrnul
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <assert.h>

#include "yang_parser.h"
#include "yang_atom.h"
#include "yang_error.h"

/* tokenizer state */
struct toks {
    FILE *f;
    struct yang_error_ctx *ectx;
    char *filename;
    int line;
    char *buf;
    size_t bufn;
    char *p; /* pointer into buf */
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

static bool
tok_readline(struct toks *toks)
{
    if (getline(&toks->buf, &toks->bufn, toks->f) < 0) {
        yang_add_err_gen(toks->ectx, YANG_ERR_PARSE_EOF,
                         toks->filename, toks->line, -1,
                         "premature end of file");
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
    toks->buf = (char *)malloc(BUFSIZ * sizeof(char *));
    toks->bufn = BUFSIZ;
    toks->p = toks->buf;
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
        } else if (isspace(*p)) {
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
tok_looking_at_separator(struct toks *toks)
{
    char *p;

    p = toks->p;
    if (*p == '\0') {
        return false;
    }
    if (isspace(*p) || *p == ';' || *p == '{') {
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
        if (!(isidentifier1(*p++))) {
            yang_add_err_gen(toks->ectx, YANG_ERR_PARSE_BAD_KEYWORD,
                             toks->filename, toks->line, 1 + p - toks->buf,
                             "invalid keyword character \"%c\"", *(p-1));
            return false;
        }
        while (isidentifier2(*p)) {
            p++;
        }
    } else {
        *prefix = NULL;
    }
    *keyword = yang_make_atom_len(s, p-s);
    toks->p = p;
    if (!tok_looking_at_separator(toks)) {
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
    char *p, *s=NULL, *q, quotechar, *new;
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
        while (!tok_looking_at_separator(toks) && *toks->p != '}') {
            toks->p++;
        }
        *str = strndup(s, toks->p-s);
        return true;
    }
    // quoted string
    *str = NULL;
    len = 0;
    have_parts = true;
    while (have_parts) {
        quotechar = *p++;
        indentpos = p - toks->buf;
        if (*p == quotechar) {
            /* empty string, need to store the end '\0' */
            new = (char *)realloc(*str, (len+1) * sizeof(char));
            if (!new) {
                goto error;
            }
            *str = new;
            new[len] = '\0';
        }
        while (*p != quotechar) {
            s = p;
            curlen = len;
            /* update len so that it stores the number of chars needed */
            if (quotechar == '\'') {
                p = strchrnul(p, '\'');
                len += p-s;
            } else {
                /* double quote; count chars, check for quoted chars */
                for (; *p != '\0' && *p != '"'; p++) {
                    if (*p != '\\') {
                        len++;
                    } else if (*p == 'n' || *p == '"' ||
                               *p == '\\' || *p == 't') {
                        p++;
                    } else {
                        len++;
                    }
                }
            }
            /* end of line, or end of this string */
            if (*p == quotechar) {
                /* make room for the '\0' char */
                sz = len + 1;
            } else {
                sz = len;
            }
            sz++; // FIXME
            new = (char *)realloc(*str, sz * sizeof(char));
            if (!new) {
                goto error;
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
                            *q = *s;
                        }
                    } else {
                        *q = *s;
                    }
                }
                *q = '\0';
                assert(len = q - new);
            }
            if (*p == '\0') {
                /* end of line, read a new line */
                if (!tok_readline(toks)) {
                    goto error;
                }
                p = toks->p;
                if (quotechar == '"') {
                    /* skip whitespace used for indentation only */
                    i = 0;
                    while (isspace(*p) && i < indentpos) {
                        p++;
                        i++;
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
                return false;
            }
        } else {
            have_parts = false;
        }
    }
    return true;

error:
    if (*str) {
        free(*str);
        *str = NULL;
    }
    yang_add_alloc_err(toks->ectx, len + (p-s));
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
                         "unterminated statement");
        return false;
    }
    return true;
error:
    yang_free_tree(*stmt);
    *stmt = NULL;
    return false;
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
        fprintf(stderr, "failed to tok init\n");
        return false;
    }
    r = parse_statement(&toks, stmt);
    tok_free(&toks);
    return r;
}
