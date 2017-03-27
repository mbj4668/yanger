#ifndef _yang_parser_h
#define _yang_parser_h

#include <stdbool.h>

#include "yang_atom.h"
#include "yang_error.h"

#define YANG_VERSION_1    1
#define YANG_VERSION_1_1  2

struct yang_statement;

#define F_ARG_TYPE_SYNTAX_REGEXP (1 << 0)
#define F_ARG_TYPE_SYNTAX_CB     (1 << 1)

struct yang_arg_type {
    yang_atom_t name;
    union {
        char *xsd_regexp;
        struct {
            bool (*validate)(char *arg, void *opaque);
            void *opaque; /* any data needed by the validate function */
        } cb;
    } syntax;
    /* if none of F_ARG_TYPE_SYNTAX_CB and F_ARG_TYPE_SYNTAX_REGEXP is
       set, no validation of the argument is done */
    unsigned int flags;
};

struct yang_statement {
    /* NULL | prefix of keyword */
    yang_atom_t prefix;
    /* NULL | name of module from which keyword is imported */
    yang_atom_t module_name; /* filled in by grammar check */
    yang_atom_t keyword;
    char *arg;
    struct yang_arg_type *arg_type; /* filled in by grammar check */
    /* yang-version of the module where this statement is defined */
    char yang_version;       /* filled in by grammar check */
    char *filename;
    int line;
    struct yang_statement *next;
    struct yang_statement *substmt;
};

extern bool yang_init_parser(void);

extern bool yang_parse(char *filename,
                       struct yang_statement **stmt,
                       struct yang_error_ctx *ectx);
extern void yang_free_tree(struct yang_statement *stmt);
extern void yang_free_statement(struct yang_statement *stmt);

#endif
