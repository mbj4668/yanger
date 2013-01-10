#define _POSIX_C_SOURCE 200809L // for strdup
#include <string.h>
#include <stdio.h>

#include "erl_nif.h"
#include "yang_parser.h"
#include "yang_grammar.h"
#include "yang_error.h"
#include "yang_atom.h"

static ERL_NIF_TERM am_ok;
static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_undefined;

#define F_ERL_ATOM           (1 << 16)
#define F_ERL_INT            (1 << 17)
#define F_ERL_IDENTIFIER_REF (1 << 18)
#define F_ERL_ATOM_OR_INT    (1 << 19)


static ERL_NIF_TERM
mk_tree(ErlNifEnv *env, struct yang_statement *s, ERL_NIF_TERM fname)
{
    ERL_NIF_TERM kw, arg, line, substmts, stmt;
    struct yang_statement *tmp;
    char *p;

    if (!s) {
        return enif_make_list(env, 0);
    }

    if (s->prefix) {
        // FIXME: maybe send the prefix to erlang, so that erlang
        // can keep track of which prefixes are used etc?
        // It is still useful to fill in the module_name in order for
        // the grammar code to verify extension grammar.
        kw = enif_make_tuple2(
            env,
            enif_make_atom(env, yang_atom_to_str(s->module_name)),
            enif_make_atom(env, yang_atom_to_str(s->keyword)));
    } else {
        kw = enif_make_atom(env, yang_atom_to_str(s->keyword));
    }
    if (s->arg && s->arg_type) {
        if (s->arg_type->flags & F_ERL_ATOM) {
            arg = enif_make_atom(env, s->arg);
        } else if (s->arg_type->flags & F_ERL_INT) {
            arg = enif_make_long(env, strtol(s->arg, NULL, 10));
        } else if (s->arg_type->flags & F_ERL_ATOM_OR_INT) {
            char *end;
            long int i;
            i = strtol(s->arg, &end, 10);
            if (end == s->arg) {
                /* not an integer */
                arg = enif_make_atom(env, s->arg);
            } else {
                /* an integer */
                arg = enif_make_long(env, i);
            }
        } else if (s->arg_type->flags & F_ERL_IDENTIFIER_REF) {
            if ((p = strchr(s->arg, ':'))) {
                arg = enif_make_tuple2(
                    env,
                    enif_make_atom_len(env, s->arg, (p - s->arg)),
                    enif_make_atom(env, p+1));
            } else {
                arg = enif_make_atom(env, s->arg);
            }
        } else {
            arg = enif_make_string(env, s->arg, ERL_NIF_LATIN1);
        }
    } else if (s->arg) {
        arg = enif_make_string(env, s->arg, ERL_NIF_LATIN1);
    } else {
        arg = am_undefined;
    }
    line = enif_make_tuple2(env, fname, enif_make_int(env, s->line));

    substmts = mk_tree(env, s->substmt, fname);
    stmt = enif_make_tuple4(env, kw, arg, line, substmts);
    tmp = s->next;
    yang_free_statement(s);
    return enif_make_list_cell(env, stmt, mk_tree(env, tmp, fname));
}

static ERL_NIF_TERM
mk_error_list(ErlNifEnv *env, struct yang_error *err)
{
    ERL_NIF_TERM error;

    if (!err) {
        return enif_make_list(env, 0);
    }
    error = enif_make_tuple5(
        env,
        enif_make_int(env, err->code),
        enif_make_string(env, err->filename, ERL_NIF_LATIN1),
        enif_make_int(env, err->line),
        enif_make_int(env, err->col),
        enif_make_string(env, err->msg, ERL_NIF_LATIN1));

    return enif_make_list_cell(env, error, mk_error_list(env, err->next));
}

static ERL_NIF_TERM
parse_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    char filename[BUFSIZ];
    struct yang_statement *stmt;
    struct yang_error_ctx *ectx;
    ERL_NIF_TERM fname, res;
    int r;

    if (argc != 1 ||
        enif_get_string(env, argv[0], filename, BUFSIZ, ERL_NIF_LATIN1) <= 0) {
        return enif_make_badarg(env);
    }

    ectx = yang_alloc_err_ctx();
    r = yang_parse(filename, &stmt, ectx);
    if (r) {
        yang_grammar_check_module(stmt, ectx);
    }
    if (ectx->err != NULL) {
        res = enif_make_tuple2(env,
                               enif_make_atom(env, "error"),
                               mk_error_list(env, ectx->err));
        yang_free_err_ctx(ectx);
        return res;
    }
    yang_free_err_ctx(ectx);
    fname = enif_make_string(env, filename, ERL_NIF_LATIN1);

    return enif_make_tuple2(env,
                            enif_make_atom(env, "ok"),
                            mk_tree(env, stmt, fname));
}

static ERL_NIF_TERM
install_arg_types(ErlNifEnv *env, ERL_NIF_TERM etypes, unsigned int len)
{
    unsigned int i;
    ERL_NIF_TERM tmp, head, tail;
    const ERL_NIF_TERM *type_spec;
    char buf[BUFSIZ];
    int arity;
    struct yang_arg_type types[len];
    int r;

    tmp = etypes;
    for (i = 0; i < len; i++) {
        if (!enif_get_list_cell(env, tmp, &head, &tail)) {
            return enif_make_badarg(env);
        }
        tmp = tail;
        if (!enif_get_tuple(env, head, &arity, &type_spec)) {
            return enif_make_badarg(env);
        }
        if (arity != 3) {
            return enif_make_badarg(env);
        }
        if (!enif_get_atom(env, type_spec[0], buf, BUFSIZ, ERL_NIF_LATIN1)) {
            return enif_make_badarg(env);
        }
        types[i].name = yang_make_atom(buf);
        if (enif_get_string(env, type_spec[1], buf, BUFSIZ, ERL_NIF_LATIN1)
            <= 0) {
            if (!enif_get_atom(env, type_spec[1], buf, BUFSIZ,
                               ERL_NIF_LATIN1)) {
                return enif_make_badarg(env);
            } else if (strcmp(buf, "undefined") != 0) {
                return enif_make_badarg(env);
            }
        } else {
            types[i].flags = F_ARG_TYPE_SYNTAX_REGEXP;
            types[i].syntax.xsd_regexp = strdup(buf);
        }
        if (!enif_get_atom(env, type_spec[2], buf, BUFSIZ, ERL_NIF_LATIN1)) {
            return enif_make_badarg(env);
        }
        if (strcmp(buf, "string") == 0) {
            // default, no bit set
        } else if (strcmp(buf, "atom") == 0) {
            types[i].flags |= F_ERL_ATOM;
        } else if (strcmp(buf, "int") == 0) {
            types[i].flags |= F_ERL_INT;
        } else if (strcmp(buf, "atom-or-int") == 0) {
            types[i].flags |= F_ERL_ATOM_OR_INT;
        } else if (strcmp(buf, "identifier-ref") == 0) {
            types[i].flags |= F_ERL_IDENTIFIER_REF;
        } else {
            return enif_make_badarg(env);
        }
    }
    r = yang_install_arg_types(types, len);
    for (i = 0; i < len; i++) {
        if (types[i].flags & F_ARG_TYPE_SYNTAX_REGEXP) {
            free(types[i].syntax.xsd_regexp);
        }
    }
    if (!r) {
        return am_error;
    }
    return am_ok;
}

static ERL_NIF_TERM
install_arg_types_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned int len;

    if (argc != 1 || !enif_get_list_length(env, argv[0], &len)) {
        return enif_make_badarg(env);
    }
    return install_arg_types(env, argv[0], len);
}

static ERL_NIF_TERM
install_grammar(ErlNifEnv *env, ERL_NIF_TERM module, ERL_NIF_TERM specs,
                unsigned int nspecs, unsigned int nrules)
{
    struct yang_statement_spec spec[nspecs];
    struct yang_statement_rule rule[nrules];
    ERL_NIF_TERM tmp, spec_head, spec_tail, rule_head, rule_tail;
    const ERL_NIF_TERM *stmt_spec, *rule_spec, *kw;
    char buf[BUFSIZ];
    unsigned int len;
    int arity;
    unsigned int i, j, rule_idx=0;

    tmp = specs;
    for (i = 0; i < nspecs; i++) {
        if (!enif_get_list_cell(env, tmp, &spec_head, &spec_tail)) {
            fprintf(stderr, "bad grammar %d\n", __LINE__);
            return enif_make_badarg(env);
        }
        if (!enif_get_tuple(env, spec_head, &arity, &stmt_spec) || arity != 3) {
            fprintf(stderr, "bad grammar %d\n", __LINE__);
            return enif_make_badarg(env);
        }
        if (!enif_get_atom(env, stmt_spec[0], buf, BUFSIZ, ERL_NIF_LATIN1)) {
            fprintf(stderr, "bad grammar %d\n", __LINE__);
            return enif_make_badarg(env);
        }
        spec[i].keyword = yang_make_atom(buf);
        if (!enif_get_atom(env, stmt_spec[1], buf, BUFSIZ, ERL_NIF_LATIN1)) {
            if (enif_is_empty_list(env, stmt_spec[1])) {
                spec[i].arg_type = NULL;
            } else
                fprintf(stderr, "bad grammar %d\n", __LINE__);
                return enif_make_badarg(env);
        } else {
            if (!(spec[i].arg_type = yang_get_arg_type(yang_make_atom(buf)))) {
                fprintf(stderr, "bad grammar %d\n", __LINE__);
                return enif_make_badarg(env);
            }
        }
        if (!enif_get_list_length(env, stmt_spec[2], &len)) {
            fprintf(stderr, "bad grammar %d\n", __LINE__);
            return enif_make_badarg(env);
        }
        spec[i].nrules = len;
        spec[i].rules = &rule[rule_idx];
        tmp = stmt_spec[2];
        for (j = 0; j < len; j++, rule_idx++) {
            if (!enif_get_list_cell(env, tmp, &rule_head, &rule_tail)) {
                fprintf(stderr, "bad grammar %d\n", __LINE__);
                return enif_make_badarg(env);
            }
            if (!enif_get_tuple(env, rule_head, &arity, &rule_spec) ||
                arity != 2) {
                fprintf(stderr, "bad grammar %d\n", __LINE__);
                return enif_make_badarg(env);
            }
            if (enif_is_atom(env, rule_spec[0])) {
                enif_get_atom(env, rule_spec[0], buf, BUFSIZ, ERL_NIF_LATIN1);
                rule[rule_idx].module_name = NULL;
                rule[rule_idx].keyword = yang_make_atom(buf);
            } else {
                if (!enif_get_tuple(env, rule_spec[0], &arity, &kw) ||
                    arity != 2) {
                    fprintf(stderr, "bad grammar %d\n", __LINE__);
                    return enif_make_badarg(env);
                }
                if (!enif_get_atom(env, kw[0], buf, BUFSIZ, ERL_NIF_LATIN1)) {
                    fprintf(stderr, "bad grammar %d\n", __LINE__);
                    return enif_make_badarg(env);
                }
                rule[rule_idx].module_name = yang_make_atom(buf);
                if (!enif_get_atom(env, kw[1], buf, BUFSIZ, ERL_NIF_LATIN1)) {
                    fprintf(stderr, "bad grammar %d\n", __LINE__);
                    return enif_make_badarg(env);
                }
                rule[rule_idx].keyword = yang_make_atom(buf);
            }
            if (!enif_get_atom(env, rule_spec[1], buf, BUFSIZ,ERL_NIF_LATIN1)) {
                fprintf(stderr, "bad grammar %d\n", __LINE__);
                return enif_make_badarg(env);
            }
            if ((buf[0] == '?'
                 || buf[0] == '1'
                 || buf[0] == '*'
                 || buf[0] == '+') && buf[1] == '\0') {
                rule[rule_idx].occurance = buf[0];
            } else {
                fprintf(stderr, "bad grammar %d\n", __LINE__);
                return enif_make_badarg(env);
            }
            tmp = rule_tail;
        }

        tmp = spec_tail;
    }

    return am_ok;
}

static ERL_NIF_TERM
install_grammar_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned int nspecs;
    unsigned int nrules;
    ERL_NIF_TERM tmp, head, tail;
    const ERL_NIF_TERM *stmt_spec;
    unsigned int i;
    int arity;
    unsigned int len;

    if (argc != 2 ||
        !enif_get_list_length(env, argv[1], &nspecs)) {
        return enif_make_badarg(env);
    }
    tmp = argv[1];
    nrules = 0;
    for (i = 0; i < nspecs; i++) {
        if (!enif_get_list_cell(env, tmp, &head, &tail)) {
            return enif_make_badarg(env);
        }
        if (!enif_get_tuple(env, head, &arity, &stmt_spec)) {
            return enif_make_badarg(env);
        }
        if (arity != 3) {
            return enif_make_badarg(env);
        }
        if (!enif_get_list_length(env, stmt_spec[2], &len)) {
            return enif_make_badarg(env);
        }
        nrules += len;
        tmp = tail;
    }
    return install_grammar(env, argv[0], argv[1], nspecs, nrules);
}

static void
set_type_bits(void)
{
    struct yang_arg_type *t;
    const char *atom_types[] = {
        "identifier",
        "uri",
        "boolean",
        "ordered-by-arg",
        "enum-arg",
        "deviate-arg",
        "status-arg",
        NULL};
    const char *int_types[] = {
        "non-negative-integer",
        NULL};
    const char *atom_or_int_types[] = {
        "length-arg",
        "max-value",
        NULL};
    int i;

    for (i = 0; atom_types[i]; i++) {
        t = yang_get_arg_type(yang_make_atom(atom_types[i]));
        t->flags |= F_ERL_ATOM;
    }
    for (i = 0; int_types[i]; i++) {
        t = yang_get_arg_type(yang_make_atom(int_types[i]));
        t->flags |= F_ERL_INT;
    }
    for (i = 0; atom_or_int_types[i]; i++) {
        t = yang_get_arg_type(yang_make_atom(atom_or_int_types[i]));
        t->flags |= F_ERL_ATOM_OR_INT;
    }
    t = yang_get_arg_type(yang_make_atom("identifier-ref"));
    t->flags |= F_ERL_IDENTIFIER_REF;
}

static int
load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    am_ok = enif_make_atom(env, "ok");
    am_error = enif_make_atom(env, "error");
    am_undefined = enif_make_atom(env, "undefined");
    yang_init_grammar();
    set_type_bits();
    return 0;
}

/*
static int
reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}
*/

static int
upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,
        ERL_NIF_TERM load_info)
{
    return 0;
}


static ErlNifFunc nif_funcs[] = {
    {"install_arg_types", 1, install_arg_types_nif},
    {"install_grammar", 2, install_grammar_nif},
    {"parse", 1, parse_nif}
};


ERL_NIF_INIT(yang_parser, nif_funcs, load, NULL, upgrade, NULL)
