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
    struct yang_statement_spec *spec;
    char occurance; /* '?', '1', '*', '+' */
};

struct yang_statement_spec {
    yang_atom_t keyword;
    /* If arg_type_idx is -1, the keyword does not expect any argument */
    int arg_type_idx;
    /* Rules is an array of the substatements accepted by this statement */
    struct yang_statement_rule *rules;
    int nrules;
};

extern bool yang_init_grammar(void);
extern bool yang_install_arg_types(struct yang_arg_type types[], int len);

/*
  stypes is an array of strings on the form:
    {<arg-typename>, <xsd-regexp>,
     <arg-typename>, <xsd-regexp>,
     ...
     NULL, NULL}

   If <xsd-regexp> is NULL, all values are accepted.
*/
extern bool yang_install_arg_types_str(const char *stypes[]);
extern bool yang_install_grammar(yang_atom_t module_name,
                                 struct yang_statement_spec spec[], int len);

/*
  stmts is an array of string on the form:
    {<keyword>, <arg-typename> | NULL,
         <substmt keyword>, <occurance>,
         <substmt keyword>, <occurance>,
         ...
         NULL, NULL,
     <keyword>, <arg-typename> | NULL,
         <substmt keyword>, <occurance>,
         <substmt keyword>, <occurance>,
         NULL, NULL,
     ...
     NULL, NULL}

  If <arg-typename> is NULL, it means the <keyword> does not expect
  any argument.  If <arg-typename> is non-NULL, it MUST be installed
  by a called to yang_install_arg_types().
*/
extern bool yang_install_grammar_str(const char *module_name,
                                     const char *stmts[]);

extern int yang_get_arg_type_idx(yang_atom_t name);
extern struct yang_arg_type *yang_get_arg_type(yang_atom_t name);
extern bool yang_grammar_check_module(struct yang_statement *stmt,
                                      bool canonical,
                                      struct yang_error_ctx *ectx);
extern void yang_print_grammar(void);

#endif

