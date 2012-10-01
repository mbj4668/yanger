#ifndef _yang_h
#define _yang_h

/*
 * FIXME: not yet used
 *   If we're going to do more in C, we probably need this.
 *   Otherwise, we can keep this info on the erlang-side.
 */

#include <stdlib.h>

#include "yang_atom.h"

struct yang_module;
struct yang_ctx;

/* represents an instantiated yang statement */
struct yang_statement {
    yang_atom_t prefix;
    /* non-null for extension statements; the name of the ext. module */
    yang_atom_t module_name; /* filled in after raw parsing */
    yang_atom_t keyword;
    char *arg;
    /* position where the statement was defined, relative def_module */
    int line;
    int col;
    /* pointer to original module / submodule where the statement was defined */
    struct yang_module *def_module;
    /* pointer to module to which the statement belongs (after expansion) */
    struct yang_module *module;
    struct yang_statement *next;
    struct yang_statement *substmt;
};

/* represents a module / submodule */
struct yang_module {
    yang_atom_t module_name;
    char *filename;
    char type; /* 'm'(odule) or 's'(ubmodule) */
    struct prefix_map *prefix_map;
    struct yang_statement *top_stmt;
    /* the local prefix which refers to our own module */
    yang_atom_t prefix;
    struct yang_ctx *ctx;
    struct yang_module *next;
};

struct yang_ctx {
    char *search_path[];
    struct yang_module *modules;
    struct yang_error *err
};


#endif
