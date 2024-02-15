/*
 * NIF implementation of the w3cregex module
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <libxml/xmlregexp.h>
#include <libxml/xmlerror.h>

#include "erl_nif.h"


/* ---------------------------------------------------------------------- */

struct regexp {
    xmlRegexpPtr xreg;
    char *string;
};

static ErlNifResourceType *regexp_type = NULL;

/* ---------------------------------------------------------------------- */

/* Commonly used atoms, initialized at load time */
static ERL_NIF_TERM am_true;
static ERL_NIF_TERM am_false;
static ERL_NIF_TERM am_ok;
static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_match;

static void initialize_local_atoms(ErlNifEnv* env)
{
    am_true  = enif_make_atom(env, "true");
    am_false = enif_make_atom(env, "false");
    am_ok    = enif_make_atom(env, "ok");
    am_error = enif_make_atom(env, "error");
    am_match = enif_make_atom(env, "match");
}

/* ---------------------------------------------------------------------- */



static ERL_NIF_TERM compile(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary patternbin;

    if (enif_inspect_iolist_as_binary(env, argv[0], &patternbin)) {
        ERL_NIF_TERM ret;
        xmlRegexpPtr xreg;
        char *patternstr = enif_alloc(patternbin.size + 1);
        patternstr[patternbin.size] = 0;
        memcpy(patternstr, patternbin.data, patternbin.size);

        if ((xreg = xmlRegexpCompile((xmlChar *)patternstr)) != NULL) {
            struct regexp *r;
            r = enif_alloc_resource(regexp_type, sizeof(struct regexp));
            memset(r, 0, sizeof(*r));
            r->xreg = xreg;
            r->string = patternstr;

            /* transfer ownership to calling process */
            ret = enif_make_tuple2(env, am_ok, enif_make_resource(env, r));
            enif_release_resource(r);
        } else {
            enif_free(patternstr);
            ret = enif_make_tuple2(
                env,
                enif_make_atom(env, "error"),
                enif_make_string(env, "Bad Pattern", ERL_NIF_LATIN1));
        }

        return ret;
    }

    return enif_make_badarg(env);
}

static ERL_NIF_TERM run_match(ErlNifEnv *env,
                              int argc, const ERL_NIF_TERM argv[])
{
    struct regexp *r = NULL;
    ErlNifBinary strbin;

    if (!enif_get_resource(env, argv[0], regexp_type, (void **)&r)) {
        return enif_make_badarg(env);
    }

    if (enif_inspect_iolist_as_binary(env, argv[1], &strbin)) {
        char string[strbin.size + 1];
        string[strbin.size] = 0;
        memcpy(string, strbin.data, strbin.size);
        switch (xmlRegexpExec(r->xreg, (xmlChar *)string)) {
        case 1:
            return am_true;
        case 0:
            return am_false;
        default:
            enif_make_tuple2(
                env,
                enif_make_atom(env, "error"),
                enif_make_string(env, "?", ERL_NIF_LATIN1));
        }
    }

    return enif_make_badarg(env);
}

static ERL_NIF_TERM run_match_null(ErlNifEnv *env,
                                   int argc, const ERL_NIF_TERM argv[])
{
    struct regexp *r = NULL;
    ErlNifBinary strbin;

    if (!enif_get_resource(env, argv[0], regexp_type, (void **)&r)) {
        return enif_make_badarg(env);
    }

    if (enif_inspect_iolist_as_binary(env, argv[1], &strbin)) {
        switch (xmlRegexpExec(r->xreg, (xmlChar *)strbin.data)) {
        case 1:
            return am_true;
        case 0:
            return am_false;
        default:
            enif_make_tuple2(
                env,
                enif_make_atom(env, "error"),
                enif_make_string(env, "?", ERL_NIF_LATIN1));
        }
    }

    return enif_make_badarg(env);
}

static ERL_NIF_TERM string(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct regexp *r = NULL;
    if (!enif_get_resource(env, argv[0], regexp_type, (void **)&r)) {
        return enif_make_badarg(env);
    }
    return enif_make_string(env, r->string, ERL_NIF_LATIN1);
}

static ERL_NIF_TERM is_xreg(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct regexp *r = NULL;
    if (enif_get_resource(env, argv[0], regexp_type, (void **)&r)) {
        return am_true;
    }
    return am_false;
}


static ErlNifFunc nif_funcs[] = {
    {"compile", 1, compile},
    {"run_match", 2, run_match},
    {"run_match_null", 2, run_match_null},
    {"string", 1, string},
    {"is_xreg", 1, is_xreg}
};


static void destroy_regexp(ErlNifEnv *env, void *obj)
{
    struct regexp *r = obj;
    xmlRegFreeRegexp(r->xreg);
    enif_free(r->string);
}

/* the default will print to stderr */
static void libxml_error_handler(void *ctx, const char *msg, ...)
{
    return;
}


static int atload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceType *rt;

    initialize_local_atoms(env);

    xmlSetGenericErrorFunc(NULL, libxml_error_handler);

    rt = enif_open_resource_type(env, NULL, "regexp", destroy_regexp,
                                 ERL_NIF_RT_CREATE, NULL);

    if (rt == NULL) return -1;

    regexp_type = rt;

    return 0;
}

ERL_NIF_INIT(w3cregex, nif_funcs, atload, NULL, NULL, NULL)
