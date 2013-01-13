#define _POSIX_C_SOURCE 200809L // for strdup
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <libxml/xmlregexp.h>

#include "yang_atom.h"
#include "yang_parser.h"
#include "yang_grammar.h"
#include "yang_core_grammar.h"

static yang_atom_t am_module;
static yang_atom_t am_submodule;
static yang_atom_t am_yang_version;
static yang_atom_t am_namespace;
static yang_atom_t am_prefix;
static yang_atom_t am_import;
static yang_atom_t am_include;
static yang_atom_t am_belongs_to;
static yang_atom_t am_sp_cut;  /* '$cut' */

struct grammar {
    yang_atom_t module_name;
    struct yang_statement_spec *specs;
    int nspecs;
};

/* array of grammar per extension module (or builtin) */
static struct grammar *grammar = NULL;
static int ngrammar = 0;

/* array of keyword's argument types */
static struct yang_arg_type *types = NULL;
static int ntypes = 0;

static struct grammar *
get_grammar(yang_atom_t module_name)
{
    int i;
    for (i = 0; i < ngrammar; i++) {
        if (grammar[i].module_name == module_name) {
            return &grammar[i];
        }
    }
    return NULL;
}

static void
build_keyword_from_stmt(char *buf, int sz,
                        struct yang_statement *stmt)
{
    int len = 0;
    if (stmt->prefix) {
        len = strlen(stmt->prefix);
        strncpy(buf, stmt->prefix, sz);
        if (len >= sz) {
            buf[sz-1] = '\0';
            return;
        }
        buf[len++] = ':';
    }
    strncpy(buf + len, stmt->keyword, sz - len);
    buf[sz-1] = '\0';
}

static void
build_keyword_from_rule(char *buf, int sz,
                        struct yang_statement_rule *rule)
{
    int len = 0;
    if (rule->module_name) {
        len = strlen(rule->module_name);
        strncpy(buf, rule->module_name, sz);
        if (len >= sz) {
            buf[sz-1] = '\0';
            return;
        }
        buf[len++] = ':';
    }
    strncpy(buf + len, rule->keyword, sz - len);
    buf[sz-1] = '\0';
}

/* assumes that the first element of the struct to compare is an atom */
static int
cmpatom(const void *p1, const void *p2)
{
    yang_atom_t a1, a2;

    a1 = *((yang_atom_t *)p1);
    a2 = *((yang_atom_t *)p2);
    if (a1 < a2) return -1;
    if (a1 > a2) return 1;
    return 0;
}

static bool
match_rule(struct yang_statement *stmt,
           struct yang_statement_rule *rules,
           int *start,
           int nrules,
           struct yang_statement_rule **found,
           bool canonical,
           struct yang_error_ctx *ectx)
{
    int i;
    char buf[BUFSIZ];

    for (i = *start; i < nrules; i++) {
        if (stmt->module_name == rules[i].module_name &&
            stmt->keyword == rules[i].keyword) {
            if (rules[i].occurance == '1' || rules[i].occurance == '?') {
                /* consume this match */
                rules[i].occurance = '0';
                *found = &rules[i];
                return true;
            } else if (rules[i].occurance == '*') {
                *found = &rules[i];
                return true;
            } else if (rules[i].occurance == '+') {
                rules[i].occurance = '*';
                *found = &rules[i];
                return true;
            } else if (rules[i].occurance == '0') {
                build_keyword_from_stmt(buf, BUFSIZ, stmt);
                yang_add_err(ectx, YANG_ERR_GRAMMAR_KEYWORD_ALREADY_FOUND,
                             stmt,
                             "keyword '%s' already given", buf);
                return false;
            } else if (rules[i].occurance == '-') {
                build_keyword_from_stmt(buf, BUFSIZ, stmt);
                yang_add_err(ectx, YANG_ERR_GRAMMAR_KEYWORD_ALREADY_FOUND,
                             stmt,
                             "unexpected keyword '%s'", buf);
                return false;
            }
        } else if (rules[i].keyword == am_sp_cut) {
            /* any non-optional statements left are errors */
            int j;
            for (j = *start; j < i; j++) {
                if (rules[j].occurance == '1' || rules[j].occurance == '+') {
                    /* consume it, so we don't report the same error again */
                    rules[j].occurance = '0';
                    build_keyword_from_rule(buf, BUFSIZ, &rules[j]);
                    yang_add_err(ectx, YANG_ERR_GRAMMAR_EXPECTED_KEYWORD, stmt,
                                 "expected keyword '%s'", buf);
                    return false;
                }
            }
            /* everything before the cut is now done */
            *start = i+1;
        } else if (canonical &&
                   (rules[i].occurance == '1' || rules[i].occurance == '+')) {
            char buf2[BUFSIZ];
            /* consume it, so we don't report the same error again */
            rules[i].occurance = '0';
            build_keyword_from_rule(buf, BUFSIZ, &rules[i]);
            build_keyword_from_stmt(buf2, BUFSIZ, stmt);
            yang_add_err(ectx, YANG_ERR_GRAMMAR_EXPECTED_KEYWORD, stmt,
                         "expected keyword '%s' before '%s'",
                         buf, buf2);
            return false;
        } else if (canonical) {
            /* consume this rule */
            rules[i].occurance = '-';
        }
    }
    /* no statement matched */
    build_keyword_from_stmt(buf, BUFSIZ, stmt);
    yang_add_err(ectx, YANG_ERR_GRAMMAR_UNEXPECTED_KEYWORD, stmt,
                 "unexpected keyword '%s'", buf);
    return false;
}

static struct yang_statement_spec *
get_spec_from_rule(struct grammar *g,
                   struct yang_statement_rule *rule)
{
    int i;

    assert(g->module_name == rule->module_name);

    for (i = 0; i < g->nspecs; i++) {
        if (rule->keyword == g->specs[i].keyword) {
            return &g->specs[i];
        }
    }
    return NULL;
}

static bool
chk_statements(struct yang_statement *stmt,
               struct yang_statement *parent,
               struct yang_statement_rule *rules,
               int nrules,
               bool canonical,
               struct yang_error_ctx *ectx)
{
    struct yang_statement_rule *rule;
    struct yang_statement_rule *subrules;
    struct yang_statement_spec *subspec;
    int start = 0, i;
    char buf[BUFSIZ];
    char buf2[BUFSIZ];
    size_t sz;
    struct grammar *g;

    while (stmt) {
        /* is this a statement known to us? */
        g = get_grammar(stmt->module_name);
        if (g) {
            /* this statement is known to us, verify that it is valid here */
            if (!match_rule(stmt, rules, &start,
                            nrules, &rule, canonical, ectx)) {
                return false;
            }
            /* it is valid here, get its spec */
            subspec = rule->spec;
            /*
            if (!(subspec = get_spec_from_rule(g, rule))) {
                build_keyword_from_rule(buf, BUFSIZ, rule);
                yang_add_err(ectx, YANG_ERR_INTERNAL, stmt,
                             "spec for '%s' not found", buf);
                return false;
            }
            */
            /* check that the argument is there */
            if (subspec->arg_type && stmt->arg) {
                stmt->arg_type = subspec->arg_type;
                if (stmt->arg_type->flags & F_ARG_TYPE_SYNTAX_CB) {
                    if (!stmt->arg_type->syntax.cb.validate(
                            stmt->arg,
                            stmt->arg_type->syntax.cb.opaque)) {
                        yang_add_err(ectx, YANG_ERR_GRAMMAR_BAD_ARGUMENT, stmt,
                                     "bad argument value \"%s\", "
                                     "should be of type %s",
                                     stmt->arg, stmt->arg_type->name);

                        return false;
                    }
                }
            }
            else if (subspec->arg_type && !stmt->arg) {
                build_keyword_from_rule(buf, BUFSIZ, rule);
                yang_add_err(ectx, YANG_ERR_GRAMMAR_MISSING_ARGUMENT, stmt,
                             "missing argument to '%s'", buf);
                return false;
            } else if (!subspec->arg_type && stmt->arg) {
                build_keyword_from_rule(buf, BUFSIZ, rule);
                yang_add_err(ectx, YANG_ERR_GRAMMAR_UNEXPECTED_ARGUMENT, stmt,
                             "did not expect an argument to '%s', got \"%s\"",
                             buf, stmt->arg);
                return false;
            }

            sz = subspec->nrules * sizeof(struct yang_statement_rule);
            subrules = (struct yang_statement_rule *)malloc(sz);
            memcpy(subrules, subspec->rules, sz);
            /* a rule for this statement was found, verify its substmts */
            if (!chk_statements(stmt->substmt, stmt, subrules,
                                subspec->nrules, canonical, ectx)) {
                free(subrules);
                return false;
            }
            free(subrules);

        } else {
            // we can't check this one; skip it.
        }
        /* check next statement */
        stmt = stmt->next;
    }
    /* make sure there are no more mandatory statements on this level */
    for (i = 0; i < nrules; i++) {
        if (rules[i].occurance == '1' || rules[i].occurance == '+') {
            build_keyword_from_rule(buf, BUFSIZ, &rules[i]);
            build_keyword_from_stmt(buf2, BUFSIZ, parent);
            yang_add_err(ectx, YANG_ERR_GRAMMAR_EXPECTED_KEYWORD, parent,
                         "expected keyword '%s' as substatement to '%s'",
                         buf, buf2);
            return false;
        }
    }
    return true;
}

bool
yang_init_grammar()
{
    am_module = yang_make_atom("module");
    am_submodule = yang_make_atom("submodule");
    am_yang_version = yang_make_atom("yang-version");
    am_namespace = yang_make_atom("namespace");
    am_prefix = yang_make_atom("prefix");
    am_import = yang_make_atom("import");
    am_include = yang_make_atom("include");
    am_belongs_to = yang_make_atom("belongs-to");
    am_sp_cut = yang_make_atom("$cut");

    if (!yang_init_core_stmt_grammar()) {
        fprintf(stderr, "%s:%d: init grammar failed\n",
                __FILE__, __LINE__);
        return false;
    }
    return true;
}

static bool
chk_xsd_regexp(char *arg, void *opaque)
{
    xmlRegexpPtr xreg = (xmlRegexpPtr)opaque;
    if (xmlRegexpExec(xreg, (xmlChar *)arg) != 1) {
        return false;
    }
    return true;
}

bool
yang_install_arg_types(struct yang_arg_type new_types[], int len)
{
    int i, j;
    xmlRegexpPtr xreg;

    i = ntypes;
    ntypes += len;
    types =
        (struct yang_arg_type *)
        realloc(types, ntypes * sizeof(struct yang_arg_type));
    if (!types) {
        return false;
    }
    for (j = 0; j < len; i++, j++) {
        memset(&types[i], 0, sizeof(struct yang_arg_type));
        types[i].name = new_types[j].name;
        types[i].flags = new_types[j].flags;
        /* If the arg type has a regexp, compile it and install a validate
           function. */
        if (new_types[j].flags & F_ARG_TYPE_SYNTAX_REGEXP) {
            xreg = xmlRegexpCompile((xmlChar *)new_types[j].syntax.xsd_regexp);
            if (!xreg) {
                printf("c: %s\n", new_types[j].syntax.xsd_regexp);
                return false;
            }
            types[i].syntax.cb.opaque = (void *)xreg;
            types[i].syntax.cb.validate = &chk_xsd_regexp;
            types[i].flags &= ~F_ARG_TYPE_SYNTAX_REGEXP;
            types[i].flags |= F_ARG_TYPE_SYNTAX_CB;
        } else if (new_types[j].flags & F_ARG_TYPE_SYNTAX_CB) {
            types[i].syntax.cb = new_types[j].syntax.cb;
        }
    }
    /* keep the array sorted so we can use binary search */
    qsort(types, ntypes, sizeof(struct yang_arg_type), cmpatom);
    return true;
}

static bool
install_arg_types_str(const char *stypes[], int ntypes)
{
    int i, j;
    struct yang_arg_type ya_type[ntypes];

    for (i = 0, j = 0; j < ntypes; i += 2, j++) {
        ya_type[j].name = yang_make_atom(stypes[i]);
        ya_type[j].flags = F_ARG_TYPE_SYNTAX_REGEXP;
        ya_type[j].syntax.xsd_regexp = (char *)stypes[i+1];
    }

    if (!yang_install_arg_types(ya_type, ntypes)) {
        return false;
    }
    return true;
}

bool
yang_install_arg_types_str(const char *stypes[])
{
    int i, ntypes = 0;

    for (i = 0; stypes[i]; i += 2) {
        ntypes++;
    }
    if (!install_arg_types_str(stypes, ntypes)) {
        return false;
    }
    return true;
}

/* set rule's spec ptr */
static bool
fix_grammar(void)
{
    int i, j, k;
    struct yang_statement_spec *s;
    struct yang_statement_rule *r;
    struct grammar *g;

    for (i = 0; i < ngrammar; i++) {
        for (j = 0; j < grammar[i].nspecs; j++) {
            s = &grammar[i].specs[j];
            for (k = 0; k < s->nrules; k++) {
                r = &s->rules[k];
                if (!r->spec && r->keyword != am_sp_cut) {
                    if (!(g = get_grammar(r->module_name))) {
                        fprintf(stderr, "grammar for %s not found\n",
                                r->module_name?r->module_name:"(null)");
                        return false;
                    }
                    if (!(r->spec = get_spec_from_rule(g, r))) {
                        fprintf(stderr, "spec not found %s %s\n",
                                r->module_name, r->keyword);
                        return false;
                    }
                }
            }
        }
    }
    return true;
}

bool
yang_install_grammar(yang_atom_t module_name,
                     struct yang_statement_spec new_specs[], int len)
{
    int i, j;
    int grammar_start;
    struct grammar *g;

    if (get_grammar(module_name)) {
        /* already installed */
        return false;
    }

    /* make room for one new grammar */
    grammar_start = ngrammar;
    ngrammar++;
    grammar =
        (struct grammar *)
        realloc(grammar, ngrammar * sizeof(struct grammar));
    if (!grammar) {
        return false;
    }
    g = &grammar[grammar_start];

    g->specs =
        (struct yang_statement_spec *)
        malloc(len * sizeof(struct yang_statement_spec));
    g->nspecs = len;

    if (!g->specs) {
        return false;
    }

    /* add the module to the list of known extension modules */
    g->module_name = module_name;

    /* copy the input spec and all rules */
    for (i = 0; i < len; i++) {
        g->specs[i].keyword = new_specs[i].keyword;
        g->specs[i].arg_type = new_specs[i].arg_type;
        g->specs[i].rules =
            (struct yang_statement_rule *)
            malloc(new_specs[i].nrules * sizeof(struct yang_statement_rule));
        for (j = 0; j < new_specs[i].nrules; j++) {
            g->specs[i].rules[j] = new_specs[i].rules[j];
            g->specs[i].rules[j].spec = NULL;
        }
        g->specs[i].nrules = new_specs[i].nrules;
    }
    return fix_grammar();
}

static bool
install_grammar_str(const char *module_name,
                    const char *stmts[],
                    int nspecs,
                    int nrules)
{
    int i, s, r, n;
    struct yang_statement_spec spec[nspecs];
    struct yang_statement_rule rule[nrules];

    i = 0;
    s = 0;
    r = 0;
    while (stmts[i]) {
        spec[s].keyword = yang_make_atom(stmts[i]);
        if (stmts[i+1]) {
            spec[s].arg_type = yang_get_arg_type(yang_make_atom(stmts[i+1]));
            if (!spec[s].arg_type) {
                fprintf(stderr, "%s:%d: arg_type %s not found",
                        __FILE__, __LINE__, stmts[i+1]);
                return false;
            }
        } else {
            spec[s].arg_type = NULL;
        }
        i += 2;
        spec[s].rules = &rule[r];
        n = 0;
        while (stmts[i]) {
            rule[r].module_name = NULL;
            rule[r].keyword = yang_make_atom(stmts[i]);
            rule[r].occurance = stmts[i+1][0];
            i += 2;
            r++;
            n++;
        }
        spec[s].nrules = n;
        /* skip the NULL markers */
        i += 2;
        s++;
    }

    if (!yang_install_grammar(NULL, spec, nspecs)) {
        return false;
    }
    return true;
}

bool
yang_install_grammar_str(const char *module_name, const char *stmts[])
{
    int i, nspecs, nrules;

    nspecs = 0;
    nrules = 0;
    i = 0;
    while (stmts[i]) {
        nspecs++;
        i += 2;
        while (stmts[i]) {
            nrules++;
            i += 2;
        }
        i += 2;
    }
    if (!install_grammar_str(module_name, stmts, nspecs, nrules)) {
        return false;
    }
    return true;
}

struct yang_arg_type *
yang_get_arg_type(yang_atom_t name)
{
    int low = 0;
    int high = ntypes;
    int mid;

    while (low < high) {
        mid = low + (high-low) / 2;
        if (name < types[mid].name) {
            high = mid;
        } else if (name > types[mid].name) {
            low = mid + 1;
        } else {
            return &types[mid];
        }
    }
    return NULL;
}

struct prefix_map {
    yang_atom_t prefix;
    yang_atom_t module_name;
    char *filename;
    int line;
};

static void
set_module_name_from_prefix(struct yang_statement *stmt,
                            struct prefix_map *prefix_map,
                            int nprefixes,
                            struct yang_error_ctx *ectx)
{
    int i;

    if (!stmt) {
        return;
    }
    if (stmt->prefix) {
        for (i = 0; i < nprefixes; i++) {
            if (stmt->prefix == prefix_map[i].prefix) {
                stmt->module_name = prefix_map[i].module_name;
                break;
            }
        }
        if (i == nprefixes) {
            yang_add_err(ectx, YANG_ERR_GRAMMAR_UNDEFINED_PREFIX, stmt,
                         "undefined prefix %s", stmt->prefix);
        }
    }
    set_module_name_from_prefix(stmt->substmt, prefix_map, nprefixes, ectx);
    set_module_name_from_prefix(stmt->next, prefix_map, nprefixes, ectx);
}

static void
add_prefix(struct yang_statement *stmt, yang_atom_t module_name,
           struct prefix_map *prefix_map, int *n,
           struct yang_error_ctx *ectx)
{
    int i;
    yang_atom_t prefix;

    if (!stmt->arg) {
        return;
    }
    prefix = yang_make_atom(stmt->arg);

    for (i = 0; i < *n; i++) {
        if (prefix == prefix_map[i].prefix) {
            yang_add_err(ectx, YANG_ERR_GRAMMAR_DUPLICATE_PREFIX, stmt,
                         "prefix '%s' already defined at %s:%d",
                         prefix,
                         prefix_map[i].filename,
                         prefix_map[i].line);
            return;
        }
    }
    prefix_map[*n].prefix = prefix;
    prefix_map[*n].module_name = module_name;
    prefix_map[*n].filename = stmt->filename;
    prefix_map[*n].line = stmt->line;
    (*n)++;
}


/*
 * NOTE: the grammar is not verified when this function is called.
 */
static void
resolve_module_names_from_prefixes(int nprefixes,
                                   struct yang_statement *stmt,
                                   struct yang_error_ctx *ectx)
{
    struct prefix_map prefix_map[nprefixes];
    struct yang_statement *s, *s2;
    int n = 0;

    /* build prefix map */
    for (s = stmt->substmt; s; s = s->next) {
        if (s->prefix == NULL) {
            if (s->keyword == am_prefix) {
                // this is our own prefix
                add_prefix(s, yang_make_atom(stmt->arg),
                           prefix_map, &n, ectx);
            } else if (s->keyword == am_belongs_to && s->arg) {
                for (s2 = s->substmt; s2; s2 = s2->next) {
                    if (s2->prefix == NULL && s2->keyword == am_prefix) {
                        // this is our own prefix
                        add_prefix(s2,
                                   yang_make_atom(s->arg),
                                   prefix_map, &n, ectx);
                        break;
                    }
                }
            } else if (s->keyword == am_import && s->arg) {
                for (s2 = s->substmt; s2; s2 = s2->next) {
                    if (s2->prefix == NULL && s2->keyword == am_prefix) {
                        add_prefix(s2,
                                   yang_make_atom(s->arg),
                                   prefix_map, &n, ectx);
                        break;
                    }
                }
            }
        }
    }

    /* recurse through all statements and set module_name on all extensions */
    set_module_name_from_prefix(stmt, prefix_map, nprefixes, ectx);
}

bool
yang_grammar_check_module(struct yang_statement *stmt,
                          bool canonical,
                          struct yang_error_ctx *ectx)
{
    struct yang_statement_rule top_rule[1];
    char buf[BUFSIZ];
    int nprefixes;
    struct yang_statement *tmp;

    /* build a pseudo top-rule for the module / submodule */
    top_rule[0].module_name = NULL;
    if (stmt->keyword == am_module) {
        top_rule[0].keyword = am_module;
    } else if (stmt->keyword == am_submodule) {
        top_rule[0].keyword = am_submodule;
    } else {
        build_keyword_from_stmt(buf, BUFSIZ, stmt);
        yang_add_err(ectx, YANG_ERR_GRAMMAR_UNEXPECTED_KEYWORD, stmt,
                     "unexpected keyword '%s'", buf);
        return false;
    }
    top_rule[0].occurance = '1';
    top_rule[0].spec = get_spec_from_rule(get_grammar(NULL), &top_rule[0]);

    /* resolve all prefixes to module names.  first count the prefixes */
    nprefixes = 1; // we always have our own prefix
    tmp = stmt->substmt;
    while (tmp) {
        if (tmp->prefix == NULL) {
            if (tmp->keyword == am_import) {
                nprefixes++;
            } else if (tmp->keyword != am_yang_version &&
                       tmp->keyword != am_namespace &&
                       tmp->keyword != am_prefix &&
                       tmp->keyword != am_include) {
                /* there can be no imports after these; we're done. */
                break;
            }
        }
        tmp = tmp->next;
    }
    resolve_module_names_from_prefixes(nprefixes, stmt, ectx);

    return chk_statements(stmt, NULL, &top_rule[0], 1, canonical, ectx);
}

void
yang_print_grammar()
{
    int g, s, r;
    struct grammar *gp;
    struct yang_statement_spec *sp;
    struct yang_statement_rule *rp;

    for (g = 0; g < ngrammar; g++) {
        gp = &grammar[g];
        printf("grammar for: %s",
               gp->module_name ? gp->module_name : "<builtin>");
        for (s = 0; s < gp->nspecs; s++) {
            sp = &gp->specs[s];
            printf("  %s (%s)\n", sp->keyword,
                   sp->arg_type ? sp->arg_type->name : "null");
            for (r = 0; r < sp->nrules; r++) {
                rp = &sp->rules[r];
                printf("    %s %c (%d)\n",
                       rp->keyword, rp->occurance, (int)rp->spec);
            }
        }
    }
}
