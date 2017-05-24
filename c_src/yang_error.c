#include <stdlib.h>
#include <stdarg.h>

#include "yang_error.h"
#include "yang_parser.h"


struct yang_error_ctx *
yang_alloc_err_ctx()
{
    struct yang_error_ctx *ectx;

    ectx = (struct yang_error_ctx *)malloc(sizeof(struct yang_error_ctx));
    if (!ectx) {
        return NULL;
    }
    ectx->err = NULL;
    return ectx;
}

void
yang_free_err_ctx(struct yang_error_ctx *ectx)
{
    struct yang_error *p, *tmp;

    p = ectx->err;
    while (p) {
        tmp = p;
        p = p->next;
        free(tmp);
    }
    free(ectx);
}

static struct yang_error *
add_err(struct yang_error_ctx *ectx)
{
    struct yang_error *err;

    err = (struct yang_error *)malloc(sizeof(struct yang_error));
    if (!err) {
        fprintf(stderr, "failed to allocate %ld bytes for error report",
                (long int)sizeof(struct yang_error));
        return NULL;
    }
    err->next = ectx->err;
    ectx->err = err;
    return err;
}

void
yang_add_err_gen(struct yang_error_ctx *ectx,
                 int code,
                 const char *filename,
                 int line,
                 int col,
                 const char *fmt, ...)
{
    va_list args;
    struct yang_error *err;

    err = add_err(ectx);
    if (!err) {
        return;
    }

    err->code = code;
    err->filename = filename;
    err->line = line;
    err->col = col;

    va_start(args, fmt);
    vsnprintf(err->msg, sizeof(err->msg), fmt, args);
    va_end(args);
}

void
yang_add_err(struct yang_error_ctx *ectx,
             int code,
             struct yang_statement *stmt,
             const char *fmt, ...)
{
    va_list args;
    struct yang_error *err;

    err = add_err(ectx);
    if (!err) {
        return;
    }

    err->code = code;
    err->filename = stmt->filename;
    err->line = stmt->line;
    err->col = -1;

    va_start(args, fmt);
    vsnprintf(err->msg, sizeof(err->msg), fmt, args);
    va_end(args);
}

void
yang_add_alloc_err(struct yang_error_ctx *ectx,
                   int nbytes)
{
    yang_add_err_gen(ectx, YANG_ERR_MEMORY_ERROR, NULL, 0, 0,
                     "failed to allocate %d bytes of memory", nbytes);
}
