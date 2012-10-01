#ifndef _yang_parser_h
#define _yang_parser_h

#include <stdbool.h>

#include "yang_atom.h"
#include "yang_error.h"

struct yang_arg_type {
    yang_atom_t name;
    char *regexp;
};

struct yang_statement {
    yang_atom_t prefix;
    yang_atom_t module_name; /* filled in by grammar check */
    yang_atom_t keyword;
    char *arg;
    struct yang_arg_type *arg_type; /* filled in by grammar check */
    char *filename;
    int line;
    struct yang_statement *next;
    struct yang_statement *substmt;
};

extern bool yang_parse(char *filename,
                       struct yang_statement **stmt,
                       struct yang_error_ctx *ectx);
extern void yang_free_tree(struct yang_statement *stmt);
extern void yang_free_statement(struct yang_statement *stmt);

#endif
