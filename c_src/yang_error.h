#ifndef _yang_error_h
#define _yang_error_h

#include <stdio.h>

#define YANG_ERR_INTERNAL                             1
#define YANG_ERR_MEMORY_ERROR                         2

#define YANG_ERR_PARSE_EOF                          100
#define YANG_ERR_PARSE_FOPEN                        102
#define YANG_ERR_PARSE_BAD_KEYWORD                  103
#define YANG_ERR_PARSE_EXPECTED_SEPARATOR           104
#define YANG_ERR_PARSE_EXPECTED_STRING              105
#define YANG_ERR_PARSE_INCOMPLETE_STATEMENT         106
#define YANG_ERR_PARSE_EXPECTED_QUOTED_STRING       107

#define YANG_ERR_GRAMMAR_KEYWORD_ALREADY_FOUND      200
#define YANG_ERR_GRAMMAR_EXPECTED_KEYWORD           201
#define YANG_ERR_GRAMMAR_UNEXPECTED_KEYWORD         202
#define YANG_ERR_GRAMMAR_UNDEFINED_PREFIX           203
#define YANG_ERR_GRAMMAR_DUPLICATE_PREFIX           204
#define YANG_ERR_GRAMMAR_MISSING_ARGUMENT           205
#define YANG_ERR_GRAMMAR_UNEXPECTED_ARGUMENT        206

struct yang_statement;

struct yang_error {
    int code;
    const char *filename;
    int line;
    int col;
    char msg[BUFSIZ];
    struct yang_error *next;
};

struct yang_error_ctx {
    struct yang_error *err;
};

extern struct yang_error_ctx *yang_alloc_err_ctx(void);
extern void yang_free_err_ctx(struct yang_error_ctx *ectx);

extern void yang_add_err_gen(struct yang_error_ctx *ectx,
                             int code,
                             const char *filename,
                             int line,
                             int col,
                             const char *fmt, ...);
extern void yang_add_err(struct yang_error_ctx *ectx,
                         int code,
                         struct yang_statement *stmt,
                         const char *fmt, ...);
extern void yang_add_alloc_err(struct yang_error_ctx *ectx,
                               int nbytes);

#endif
