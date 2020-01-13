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
    char min_yang_version;
    struct yang_statement_spec *spec;
    char occurance; /* '?', '1', '*', '+' */
};

struct yang_statement_spec {
    yang_atom_t keyword;
    /* If arg_type_idx is -1, the keyword does not expect any argument */
    int arg_type_idx;
    /* If a yang_statement_spec has F_STMT_ARG_MATCH, then there MUST be
       another yang_statement_spec with the same keyword without this
       flags set. */
    unsigned int flags;
    /* Rules is an array of the substatements accepted by this statement */
    struct yang_statement_rule *rules;
    int nrules;
};

#define F_STMT_ARG_MATCH  (1 << 0)

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

  If the grammar for a keyword is different depending on the argument,
  this can be handled by specifiying N multiple stmts with the same keyword
  but different <arg-typename>.  In this case, all but the last <arg-typename>
  MUST start with the character '='.  Also, the statements that use this
  special keyword as a substmt, MUST specify N identical substmts.  For
  example:

     "deviate",          "=not-supported",
        NULL,    NULL,               NULL,
     "deviate",          "=delete",
        "1",     "units",            "?",
        "1",     "must",             "*",
        "1",     "unique",           "*",
        "1",     "default",          "?",
        NULL,    NULL,               NULL,
     "deviate",          "deviate-arg",
        <all deviation substmts here>
        NULL,    NULL,               NULL,

     ...

     "deviation",        "absolute-schema-nodeid",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        "1",     "deviate",          "*",
        "1",     "deviate",          "*",
        "1",     "deviate",          "*",
        NULL,    NULL,               NULL,
*/
extern bool yang_install_grammar_str(const char *module_name,
                                     const char *stmts[]);

extern bool yang_add_rule_to_spec(struct yang_statement_rule *rule,
                                  yang_atom_t modulename,
                                  yang_atom_t keyword);

extern int
yang_get_grammar_module_names(int n, yang_atom_t *module_names);
extern struct yang_statement_spec *
yang_get_statement_spec(yang_atom_t module_name,
                        yang_atom_t keyword);
extern int yang_get_arg_type_idx(yang_atom_t name);
extern struct yang_arg_type *yang_get_arg_type(int arg_type_idx);
extern bool yang_grammar_check_module(struct yang_statement *stmt,
                                      bool canonical,
                                      struct yang_error_ctx *ectx);
extern void yang_print_grammar(void);

#endif

