#ifndef _yang_grammar_h
#define _yang_grammar_h

#include <stdbool.h>

#include "yang_atom.h"
#include "yang_parser.h"
#include "yang_error.h"

struct yang_statement_rule {
    /* module_name is NULL for core YANG stmts, and
       otherwise the name of the module that defines the
       extension */
    yang_atom_t module_name;
    yang_atom_t keyword;
    char occurance; /* '?', '1', '*', '+' */
};

struct yang_statement_spec {
    yang_atom_t keyword;
    struct yang_arg_type *arg_type;
    struct yang_statement_rule *rules;
    int nrules;
};

extern bool yang_init_grammar(void);
extern bool yang_install_arg_types(struct yang_arg_type types[], int len);
extern bool yang_install_arg_types_str(const char *stypes[]);
extern bool yang_install_grammar(yang_atom_t module_name,
                                 struct yang_statement_spec spec[], int len);
extern bool yang_install_grammar_str(const char *module_name,
                                     const char *stmts[]);
extern struct yang_arg_type *yang_get_arg_type(yang_atom_t name);
extern bool yang_grammar_check_module(struct yang_statement *stmt,
                                      struct yang_error_ctx *ectx);
extern void yang_print_grammar(void);

#endif

