#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include "yang_if_feature.h"
#include "yang_atom.h"

#define TOK_SYNTAX_ERROR -1
#define TOK_EOF 0
#define TOK_OR  1
#define TOK_AND 2
#define TOK_NOT 3
#define TOK_LPAREN 4
#define TOK_RPAREN 5
#define TOK_IDENTIFIER 6
#define TOK_NONE 99

#define isidentifier1(X) (((X) >= 'a' && (X) <= 'z') || \
                          ((X) >= 'A' && (X) <= 'Z') || \
                          ((X) == '_'))
#define isidentifier2(X) (((X) >= 'a' && (X) <= 'z') || \
                          ((X) >= 'A' && (X) <= 'Z') || \
                          ((X) >= '0' && (X) <= '9') || \
                          ((X) == '_') ||               \
                          ((X) == '.') ||               \
                          ((X) == '-'))

#define is_wspace(X) ((X) == ' ' || (X) == '\t' ||     \
                      (X) == '\n' || (X) == '\r')

/* we're reading out prefix / keyword; this is strictly speaking
   not necessary b/c it isn't used currently. */
static int
_get_tok(char **s, yang_atom_t *prefix, yang_atom_t *keyword)
{
    char *p;
    char *q;

    p = *s;
    while (is_wspace(*p)) {
        p++;
    }
    if (*p == '\0') {
        *s = p;
        return TOK_EOF;
    }
    if (*p == '(') {
        p++;
        *s = p;
        return TOK_LPAREN;
    }
    if (*p == ')') {
        p++;
        *s = p;
        return TOK_RPAREN;
    }
    if (strncmp(p, "or", 2) == 0) {
        p+=2;
        if (*p == '\0' || is_wspace(*p) || *p == '(' || *p == ')') {
            *s = p;
            return TOK_OR;
        }
        p-=2;
    }
    if (strncmp(p, "and", 3) == 0) {
        p+=3;
        if (*p == '\0' || is_wspace(*p) || *p == '(' || *p == ')') {
            *s = p;
            return TOK_AND;
        }
        p-=3;
    }
    if (strncmp(p, "not", 3) == 0) {
        p+=3;
        if (*p == '\0' || is_wspace(*p) || *p == '(' || *p == ')') {
            *s = p;
            return TOK_NOT;
        }
        p-=3;
    }
    q = p;
    if (!isidentifier1(*p)) {
        *s = p;
        return TOK_SYNTAX_ERROR;
    }
    while (isidentifier2(*p)) {
        p++;
    }
    if (*p == ':') {
        *prefix = yang_make_atom_len(q, p-q);
        p++;
        q = p;
        if (!isidentifier1(*p)) {
            *s = p;
            return TOK_SYNTAX_ERROR;
        }
        p++;
        while (isidentifier2(*p)) {
            p++;
        }
        *keyword = yang_make_atom_len(q, p-q);
        *s = p;
        return TOK_IDENTIFIER;
    } else {
        *prefix = NULL;
        *keyword = yang_make_atom_len(q, p-q);
        *s = p;
        return TOK_IDENTIFIER;
    }
}

struct lexer {
    char *s;
    int peek_tok;
};

static void init_lexer(struct lexer *lex, char *s)
{
    lex->s = s;
    lex->peek_tok = TOK_NONE;
}

static int get_tok(struct lexer *lex)
{
    int tok;
    yang_atom_t prefix, keyword;

    if (lex->peek_tok != TOK_NONE) {
        tok = lex->peek_tok;
        lex->peek_tok = TOK_NONE;
        return tok;
    }
    return _get_tok(&lex->s, &prefix, &keyword);
}

static void push_tok(struct lexer *lex, int tok)
{
    lex->peek_tok = tok;
}

/*
  if-feature-expr     = "(" if-feature-expr ")" /
                        if-feature-expr sep boolean-operator sep
                          if-feature-expr /
                        not-keyword sep if-feature-expr /
                        identifier-ref-arg

Can be rewritten as:
  x = y ("and"/"or" y)*
  y = "not" x /
      "(" x ")"
      identifier

NOTE: the if-feature syntax is context sensitive - the following
      is ok:  "or or and" - meaning the feature 'or' || the feature 'and'

Recursive descent parser follows.
*/

#define RES_ERROR -1
#define RES_OK 1

static int y(struct lexer *lex);

static int x(struct lexer *lex)
{
    int tok;
    int r;

    if ((r = y(lex)) <= 0 ) {
        return r;
    }
    tok = get_tok(lex);
    while (tok == TOK_AND || tok == TOK_OR) {
        if ((r = y(lex)) <= 0 ) {
            return r;
        }
        tok = get_tok(lex);
    }
    push_tok(lex, tok);
    return RES_OK;
}

static int y(struct lexer *lex)
{
    int r;
    struct lexer lex2;

    switch (get_tok(lex)) {
    case TOK_NOT:
        lex2 = *lex;
        r = x(lex);
        if (r == RES_ERROR) {
            /* try to treat 'not' as an identifier, which is ok according
               to the grammar */
            *lex = lex2;
            return RES_OK;
        }
        return r;
    case TOK_LPAREN:
        if ((r = x(lex)) <= 0 ) {
            return r;
        }
        if (get_tok(lex) != TOK_RPAREN) {
            return RES_ERROR;
        }
        return RES_OK;
    case TOK_IDENTIFIER:
    /* context sensitve part - treat 'or' and 'and' as identifiers */
    case TOK_OR:
    case TOK_AND:
        return RES_OK;
    default:
        return RES_ERROR;
    }
}

bool
yang_parse_if_feature_expr(char *str)
{
    struct lexer lex;
    int r;

    init_lexer(&lex, str);

    r = x(&lex);
    if (r == RES_ERROR) {
        return false;
    } else if (get_tok(&lex) != TOK_EOF) {
        return false;
    }
    return true;
}
