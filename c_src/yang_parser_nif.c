#include <stdio.h>
#include <string.h>

#include "erl_nif.h"
#include "yang_parser.h"
#include "yang_grammar.h"
#include "yang_error.h"
#include "yang_atom.h"

static ERL_NIF_TERM am_ok;
static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_undefined;
static yang_atom_t yam_identifier;
static yang_atom_t yam_identifier_ref;
static yang_atom_t yam_uri;
static yang_atom_t yam_boolean;
static yang_atom_t yam_ordered_by_arg;
static yang_atom_t yam_enum_arg;
static yang_atom_t yam_deviate_arg;
static yang_atom_t yam_status_arg;

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
        if (s->arg_type->name == yam_identifier
            || s->arg_type->name == yam_uri
            || s->arg_type->name == yam_boolean
            || s->arg_type->name == yam_ordered_by_arg
            || s->arg_type->name == yam_enum_arg
            || s->arg_type->name == yam_deviate_arg
            || s->arg_type->name == yam_status_arg) {
            arg = enif_make_atom(env, s->arg);
        } else if (s->arg_type->name == yam_identifier_ref) {
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
    int arity = 2;
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
        if (!enif_get_atom(env, type_spec[0], buf, BUFSIZ, ERL_NIF_LATIN1)) {
            return enif_make_badarg(env);
        }
        types[i].name = yang_make_atom(buf);
        if (enif_get_string(env, type_spec[1], buf, BUFSIZ, ERL_NIF_LATIN1)
            <= 0) {
            return enif_make_badarg(env);
        }
        if (strlen(buf) == 0) {
            types[i].regexp = NULL;
        } else {
            types[i].regexp = (char *)malloc(sizeof(char) * strlen(buf));
            strcpy(types[i].regexp, buf);
        }
    }
    r = yang_install_arg_types(types, len);
    for (i = 0; i < len; i++) {
        if (types[i].regexp) {
            free(types[i].regexp);
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
get_arg_type_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    char buf[BUFSIZ];
    struct yang_arg_type *t;

    if (argc != 1 ||
        !enif_get_atom(env, argv[0], buf, BUFSIZ, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }
    t = yang_get_arg_type(yang_make_atom(buf));
    if (!t) {
        return am_error;
    }
    return am_ok;
}


static int
load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    am_ok = enif_make_atom(env, "ok");
    am_error = enif_make_atom(env, "error");
    am_undefined = enif_make_atom(env, "undefined");
    yam_identifier = yang_make_atom("identifier");
    yam_identifier_ref = yang_make_atom("identifier-ref");
    yam_uri = yang_make_atom("uri");
    yam_boolean = yang_make_atom("boolean");
    yam_ordered_by_arg = yang_make_atom("ordered-by-arg");
    yam_enum_arg = yang_make_atom("enum-arg");
    yam_deviate_arg = yang_make_atom("deviate-arg");
    yam_status_arg = yang_make_atom("status-arg");
    yang_init_grammar();
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
    {"get_arg_type", 1, get_arg_type_nif},
    {"parse", 1, parse_nif}
};


ERL_NIF_INIT(yang_parser, nif_funcs, load, NULL, upgrade, NULL)
