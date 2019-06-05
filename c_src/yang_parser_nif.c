#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <inttypes.h>

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
#define F_ERL_UINT           (1 << 18)
#define F_ERL_IDENTIFIER_REF (1 << 19)
#define F_ERL_ATOM_OR_INT    (1 << 20)
#define F_ERL_ATOM_OR_UINT   (1 << 21)

#define STRSIZ 1024
#define FILENAMESIZ 4096

static ERL_NIF_TERM mk_tree_node(ErlNifEnv *env, struct yang_statement *s,
                                 ERL_NIF_TERM fname);

static ERL_NIF_TERM
make_binary(ErlNifEnv *env, const char *str)
{
    ERL_NIF_TERM eb;
    size_t len = strlen(str);
    unsigned char *data = enif_make_new_binary(env, len, &eb);

    memcpy(data, str, len);
    return eb;
}

static ERL_NIF_TERM
mk_tree(ErlNifEnv *env, struct yang_statement *s, ERL_NIF_TERM fname)
{
    int i, cnt;
    ERL_NIF_TERM list, *stmts;
    struct yang_statement *next;

    if (!s) {
        return enif_make_list(env, 0);
    }

    for (cnt = 0, next = s; next; cnt++, next = next->next)
        ;
    stmts = malloc(sizeof(ERL_NIF_TERM) * cnt);

    for (i = 0; s; i++, s = next) {
        next = s->next;
        stmts[i] = mk_tree_node(env, s, fname);
        yang_free_statement(s);
    }

    list = enif_make_list_from_array(env, stmts, cnt);
    free(stmts);

    return list;
}

static ERL_NIF_TERM
mk_tree_node(ErlNifEnv *env, struct yang_statement *s, ERL_NIF_TERM fname)
{
    ERL_NIF_TERM kw, arg, line, substmts;
    char *p;

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
            arg = enif_make_int64(env, strtoll(s->arg, NULL, 10));
        } else if (s->arg_type->flags & F_ERL_UINT) {
            arg = enif_make_uint64(env, strtoull(s->arg, NULL, 10));
        } else if (s->arg_type->flags & F_ERL_ATOM_OR_INT) {
            char *end;
            int64_t i;
            i = strtoll(s->arg, &end, 10);
            if (end == s->arg) {
                /* not an integer */
                arg = enif_make_atom(env, s->arg);
            } else {
                /* an integer */
                arg = enif_make_int64(env, i);
            }
        } else if (s->arg_type->flags & F_ERL_ATOM_OR_UINT) {
            char *end;
            uint64_t i;
            i = strtoull(s->arg, &end, 10);
            if (end == s->arg) {
                /* not an integer */
                arg = enif_make_atom(env, s->arg);
            } else {
                /* an integer */
                arg = enif_make_uint64(env, i);
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
            arg = make_binary(env, s->arg);
        }
    } else if (s->arg) {
        arg = make_binary(env, s->arg);
    } else {
        arg = enif_make_list(env, 0);
    }
    line = enif_make_tuple2(env, fname, enif_make_int(env, s->line));

    substmts = mk_tree(env, s->substmt, fname);
    return enif_make_tuple4(env, kw, arg, line, substmts);
}

static ERL_NIF_TERM
mk_error_list(ErlNifEnv *env, struct yang_error *err)
{
    ERL_NIF_TERM error;
    ERL_NIF_TERM list;
    const char *filename;

    list = enif_make_list(env, 0);
    while (err) {
        if (err->filename) {
            filename = err->filename;
        } else {
            filename = "";
        }
        error =
            enif_make_tuple5(
                env,
                enif_make_int(env, err->code),
                enif_make_string(env, filename, ERL_NIF_LATIN1),
                enif_make_int(env, err->line),
                enif_make_int(env, err->col),
                enif_make_string(env, err->msg, ERL_NIF_LATIN1));
        list = enif_make_list_cell(env, error, list);
        err = err->next;
    }
    return list;
}

static ERL_NIF_TERM
parse_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    char filename[FILENAMESIZ];
    char buf[STRSIZ];
    struct yang_statement *stmt;
    struct yang_error_ctx *ectx;
    struct yang_error *err;
    ERL_NIF_TERM fname, res, errors;
    int r;
    bool canonical, has_error;

    if (argc != 2 ||
        enif_get_string(env, argv[0], filename,
                        FILENAMESIZ, ERL_NIF_LATIN1) <= 0 ||
        enif_get_atom(env, argv[1], buf, STRSIZ, ERL_NIF_LATIN1) <= 0)
    {
        return enif_make_badarg(env);
    }
    if (strcmp(buf, "true") == 0) {
        canonical = true;
    } else if (strcmp(buf, "false") == 0) {
        canonical = false;
    } else {
        return enif_make_badarg(env);
    }

    ectx = yang_alloc_err_ctx();
    r = yang_parse(filename, &stmt, ectx);
    if (r) {
        yang_grammar_check_module(stmt, canonical, ectx);
    }
    err = ectx->err;
    has_error = false;
    while (err && !has_error) {
        if (err->code < YANG_FIRST_WARNING) {
            has_error = true;
        }
        err = err->next;
    }
    errors = mk_error_list(env, ectx->err);
    yang_free_err_ctx(ectx);
    if (!r || has_error) {
        res = enif_make_tuple2(env,
                               enif_make_atom(env, "error"),
                               errors);
        return res;
    }
    fname = enif_make_string(env, filename, ERL_NIF_LATIN1);

    return enif_make_tuple3(env,
                            enif_make_atom(env, "ok"),
                            mk_tree(env, stmt, fname),
                            errors);
}

static ERL_NIF_TERM
mk_kwd(ErlNifEnv *env, yang_atom_t module_name, yang_atom_t keyword)
{
    if (module_name) {
        return enif_make_tuple2(env,
                                enif_make_atom(env, module_name),
                                enif_make_atom(env, keyword));
    } else {
        return enif_make_atom(env, keyword);
    }
}

static ERL_NIF_TERM
mk_spec(ErlNifEnv *env, struct yang_statement_spec *spec)
{
    ERL_NIF_TERM arg, rule, rules;
    struct yang_arg_type *type;
    int i;
    char occurance[2];

    type = yang_get_arg_type(spec->arg_type_idx);
    if (!type) {
        arg = enif_make_list(env, 0);
    } else {
        arg = enif_make_atom(env, type->name);
    }
    occurance[1] = '\0';
    rules = enif_make_list(env, 0);
    for (i = spec->nrules-1; i >= 0; i--) {
        occurance[0] = spec->rules[i].occurance;
        rule = enif_make_tuple2(env,
                                mk_kwd(env,
                                       spec->rules[i].module_name,
                                       spec->rules[i].keyword),
                                enif_make_atom(env, occurance)),
        rules = enif_make_list_cell(env, rule, rules);
    }
    return enif_make_tuple4(env,
                            enif_make_atom(env, spec->keyword),
                            arg,
                            rules,
                            am_undefined);
}

static ERL_NIF_TERM
get_statement_spec_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    char buf0[STRSIZ];
    char buf1[STRSIZ];
    yang_atom_t mod;
    yang_atom_t kwd;
    struct yang_statement_spec *spec;
    int arity;
    const ERL_NIF_TERM *kwd_spec;

    if (argc != 1) {
        return enif_make_badarg(env);
    }
    if (enif_get_atom(env, argv[0], buf0, STRSIZ, ERL_NIF_LATIN1) <= 0) {
        if (!enif_get_tuple(env, argv[0], &arity, &kwd_spec) || arity != 2) {
            return enif_make_badarg(env);
        }
        if (!enif_get_atom(env, kwd_spec[0], buf0, STRSIZ, ERL_NIF_LATIN1)) {
            return enif_make_badarg(env);
        }
        if (!enif_get_atom(env, kwd_spec[1], buf1, STRSIZ, ERL_NIF_LATIN1)) {
            return enif_make_badarg(env);
        }
        mod = yang_make_atom(buf0);
        kwd = yang_make_atom(buf1);
    } else {
        mod = NULL;
        kwd = yang_make_atom(buf0);
    }
    spec = yang_get_statement_spec(mod, kwd);
    if (!spec) {
        return enif_make_atom(env, "not_found");
    }
    return enif_make_tuple2(env,
                            enif_make_atom(env, "value"),
                            mk_spec(env, spec));
}

static ERL_NIF_TERM
get_grammar_module_names_nif(ErlNifEnv *env,
                             int argc,
                             const ERL_NIF_TERM argv[])
{
    int n = yang_get_grammar_module_names(0, NULL);
    int i;
    ERL_NIF_TERM res;
    yang_atom_t module_names[n];

    yang_get_grammar_module_names(n, module_names);
    res = enif_make_list(env, 0);
    for (i = 0; i < n; i++) {
        res = enif_make_list_cell(env,
                                  enif_make_atom(env, module_names[i]),
                                  res);
    }
    return res;
}

static ERL_NIF_TERM
install_arg_types(ErlNifEnv *env, ERL_NIF_TERM etypes, unsigned int len)
{
    unsigned int i;
    ERL_NIF_TERM tmp, head, tail;
    const ERL_NIF_TERM *type_spec;
    char buf[STRSIZ];
    int arity;
    struct yang_arg_type types[len];
    int r;

    memset(types, 0, sizeof(types));
    tmp = etypes;
    for (i = 0; i < len; i++) {
        /* Get the head of the list */
        if (!enif_get_list_cell(env, tmp, &head, &tail)) {
            return enif_make_badarg(env);
        }
        tmp = tail;
        /* Verify that the head is a 3-tuple */
        if (!enif_get_tuple(env, head, &arity, &type_spec) || arity != 3) {
            return enif_make_badarg(env);
        }
        /* Handle elemt 1 - ArgTypeName */
        if (!enif_get_atom(env, type_spec[0], buf, STRSIZ, ERL_NIF_LATIN1)) {
            return enif_make_badarg(env);
        }
        types[i].name = yang_make_atom(buf);
        /* Handle elemt 2 - Regexp | undefined */
        if (enif_get_string(env, type_spec[1], buf, STRSIZ, ERL_NIF_LATIN1)
            <= 0)
        {
            /* It's not a string, make sure it is 'undefined' */
            if (!enif_get_atom(env, type_spec[1], buf, STRSIZ,
                               ERL_NIF_LATIN1)) {
                return enif_make_badarg(env);
            } else if (strcmp(buf, "undefined") != 0) {
                return enif_make_badarg(env);
            }
        } else {
            char *tmp;
            types[i].flags = F_ARG_TYPE_SYNTAX_REGEXP;
            tmp = (char *)malloc((strlen(buf) + 1) * sizeof(char));
            types[i].syntax.xsd_regexp = strcpy(tmp, buf);
        }
        /* Handle elemt 3 - ReturnType */
        if (!enif_get_atom(env, type_spec[2], buf, STRSIZ, ERL_NIF_LATIN1)) {
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

static bool
is_occurance(char *buf)
{
    if ((buf[0] == '?'
         || buf[0] == '1'
         || buf[0] == '*'
         || buf[0] == '+') && buf[1] == '\0') {
        return true;
    } else {
        return false;
    }
}

static bool
get_keyword_from_term(ErlNifEnv *env, ERL_NIF_TERM kw_term,
                      yang_atom_t *module_name,
                      yang_atom_t *keyword)
{
    char buf[STRSIZ];
    int arity;
    const ERL_NIF_TERM *kw;

    if (enif_is_atom(env, kw_term)) {
        /* Core Keyword (plain atom) */
        enif_get_atom(env, kw_term, buf, STRSIZ, ERL_NIF_LATIN1);
        *module_name = NULL;
        *keyword = yang_make_atom(buf);
        return true;
    } else {
        /* Extension Keyword (2-tuple) */
        if (!enif_get_tuple(env, kw_term, &arity, &kw) ||
            arity != 2)
        {
            fprintf(stderr, "bad grammar %d\n", __LINE__);
            return false;
        }
        /* Handle element 1 of the Extension Keyword */
        if (!enif_get_atom(env, kw[0], buf, STRSIZ, ERL_NIF_LATIN1)) {
            fprintf(stderr, "bad grammar %d\n", __LINE__);
            return false;
        }
        *module_name = yang_make_atom(buf);
        /* Handle element 2 of the Extension Keyword */
        if (!enif_get_atom(env, kw[1], buf, STRSIZ, ERL_NIF_LATIN1)) {
            fprintf(stderr, "bad grammar %d\n", __LINE__);
            return false;
        }
        *keyword = yang_make_atom(buf);
        return true;
    }
}

static ERL_NIF_TERM
install_grammar(ErlNifEnv *env, ERL_NIF_TERM module_name, ERL_NIF_TERM specs,
                unsigned int nspecs, unsigned int nrules)
{
    struct yang_statement_spec spec[nspecs];
    struct yang_statement_rule rule[nrules];
    ERL_NIF_TERM tmp, spec_head, spec_tail, rule_head, rule_tail;
    const ERL_NIF_TERM *stmt_spec, *rule_spec, *usein;
    char buf[STRSIZ];
    unsigned int len;
    int arity;
    unsigned int i, j, rule_idx=0;
    yang_atom_t m;

    memset(spec, 0, sizeof(spec));
    memset(rule, 0, sizeof(rule));
    tmp = specs;
    for (i = 0; i < nspecs; i++) {
        /* Get the head of the list */
        if (!enif_get_list_cell(env, tmp, &spec_head, &spec_tail)) {
            fprintf(stderr, "bad grammar %d\n", __LINE__);
            return enif_make_badarg(env);
        }
        /* Verify that the head is a 4-tuple */
        if (!enif_get_tuple(env, spec_head, &arity, &stmt_spec) || arity != 4) {
            fprintf(stderr, "bad grammar %d\n", __LINE__);
            return enif_make_badarg(env);
        }
        /* Handle element 1 - Keyword */
        if (!enif_get_atom(env, stmt_spec[0], buf, STRSIZ, ERL_NIF_LATIN1)) {
            fprintf(stderr, "bad grammar %d\n", __LINE__);
            return enif_make_badarg(env);
        }
        spec[i].keyword = yang_make_atom(buf);
        /* Handle element 2 - ArgType */
        if (!enif_get_atom(env, stmt_spec[1], buf, STRSIZ, ERL_NIF_LATIN1)) {
            if (enif_is_empty_list(env, stmt_spec[1])) {
                spec[i].arg_type_idx = -1;
            } else {
                fprintf(stderr, "bad grammar %d\n", __LINE__);
                return enif_make_badarg(env);
            }
        } else {
            if ((spec[i].arg_type_idx =
                 yang_get_arg_type_idx(yang_make_atom(buf))) == -1) {
                fprintf(stderr, "bad grammar %d\n", __LINE__);
                return enif_make_badarg(env);
            }
        }
        /* Handle element 3 - RuleList */
        if (!enif_get_list_length(env, stmt_spec[2], &len)) {
            fprintf(stderr, "bad grammar %d\n", __LINE__);
            return enif_make_badarg(env);
        }
        spec[i].nrules = len;
        spec[i].rules = &rule[rule_idx];
        tmp = stmt_spec[2];
        for (j = 0; j < len; j++, rule_idx++) {
            /* Get the head of the list */
            if (!enif_get_list_cell(env, tmp, &rule_head, &rule_tail)) {
                fprintf(stderr, "bad grammar %d\n", __LINE__);
                return enif_make_badarg(env);
            }
            /* Verify that the head is a 2-tuple */
            if (!enif_get_tuple(env, rule_head, &arity, &rule_spec) ||
                arity != 2)
            {
                fprintf(stderr, "bad grammar %d\n", __LINE__);
                return enif_make_badarg(env);
            }
            /* Handle element 1 - Substmt Keyword */
            if (!(get_keyword_from_term(env, rule_spec[0],
                                        &rule[rule_idx].module_name,
                                        &rule[rule_idx].keyword))) {
                fprintf(stderr, "bad grammar %d\n", __LINE__);
                return enif_make_badarg(env);
            }
            /* Handle element 2 - Occurance */
            if (!enif_get_atom(env, rule_spec[1], buf, STRSIZ,ERL_NIF_LATIN1)) {
                fprintf(stderr, "bad grammar %d\n", __LINE__);
                return enif_make_badarg(env);
            }
            if (!(is_occurance(&buf[0]))) {
                fprintf(stderr, "bad grammar %d\n", __LINE__);
                return enif_make_badarg(env);
            } else {
                rule[rule_idx].occurance = buf[0];
            }
            tmp = rule_tail;
        }

        tmp = spec_tail;
    }

    if (!enif_get_atom(env, module_name, buf, STRSIZ,ERL_NIF_LATIN1)) {
        fprintf(stderr, "bad grammar %d\n", __LINE__);
        return enif_make_badarg(env);
    }
    m = yang_make_atom(buf);
    if (!yang_install_grammar(m, spec, nspecs)) {
        return am_error;
    }
    /* Now, loop through the list again and handle the 4th element; UseIn */
    /* No need for all the error handling we just did */
    tmp = specs;
    for (i = 0; i < nspecs; i++) {
        struct yang_statement_rule rule;
        yang_atom_t usein_module_name;
        yang_atom_t usein_keyword;
        ERL_NIF_TERM usein_head, usein_tail;

        memset(&rule, 0, sizeof(rule));
        /* Get the head of the list */
        enif_get_list_cell(env, tmp, &spec_head, &spec_tail);
        /* Get the head as a 4-tuple */
        enif_get_tuple(env, spec_head, &arity, &stmt_spec);
        /* Handle element 1 - Keyword */
        enif_get_atom(env, stmt_spec[0], buf, STRSIZ, ERL_NIF_LATIN1);
        rule.module_name = m;
        rule.keyword = yang_make_atom(buf);
        rule.min_yang_version = YANG_VERSION_1;
        /* This spec must exist, since it was added by
           yang_install_grammar() above */
        rule.spec = yang_get_statement_spec(m, rule.keyword);
        assert(rule.spec);
        /* Handle element 4 - UseIn (2-tuple or 'undefined' */
        if (enif_is_atom(env, stmt_spec[3])) {
            /* It's not a string, make sure it is 'undefined' */
            enif_get_atom(env, stmt_spec[3], buf, STRSIZ, ERL_NIF_LATIN1);
            if (strcmp(buf, "undefined") != 0) {
                fprintf(stderr, "bad grammar %d\n", __LINE__);
                return enif_make_badarg(env);
            }
        } else {
            if (!enif_get_tuple(env, stmt_spec[3], &arity, &usein) ||
                arity != 2)
            {
                fprintf(stderr, "bad grammar %d\n", __LINE__);
                return enif_make_badarg(env);
            }
            /* Handle element 1 of UseIn - OccuranceWhenUsed */
            if (!(enif_get_atom(env, usein[0], buf, STRSIZ, ERL_NIF_LATIN1))) {
                fprintf(stderr, "bad grammar %d\n", __LINE__);
                return enif_make_badarg(env);
            }
            if (!(is_occurance(&buf[0]))) {
                fprintf(stderr, "bad grammar %d\n", __LINE__);
                return enif_make_badarg(env);
            }
            rule.occurance = buf[0];
            /* Handle element 2 of UseIn - UseInKeywords */
            if (!enif_get_list_length(env, usein[1], &len)) {
                fprintf(stderr, "bad grammar %d\n", __LINE__);
                return enif_make_badarg(env);
            }
            tmp = usein[1];
            /* Loop through the list of UseInKeywords, and add this rule to
               each keyword's spec. */
            for (j = 0; j < len; j++) {
                /* Get the head of the list */
                if (!enif_get_list_cell(env, tmp, &usein_head, &usein_tail)) {
                    fprintf(stderr, "bad grammar %d\n", __LINE__);
                    return enif_make_badarg(env);
                }
                if (!(get_keyword_from_term(env, usein_head,
                                            &usein_module_name,
                                            &usein_keyword))) {
                    fprintf(stderr, "bad grammar %d\n", __LINE__);
                    return enif_make_badarg(env);
                }
                if (!yang_add_rule_to_spec(&rule, usein_module_name,
                                           usein_keyword)) {
                    return am_error;
                }
                tmp = usein_tail;
            }
            tmp = spec_tail;
        }
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
        fprintf(stderr, "bad grammar %d\n", __LINE__);
        return enif_make_badarg(env);
    }
    tmp = argv[1];
    nrules = 0;
    for (i = 0; i < nspecs; i++) {
        if (!enif_get_list_cell(env, tmp, &head, &tail)) {
            fprintf(stderr, "bad grammar %d\n", __LINE__);
            return enif_make_badarg(env);
        }
        if (!enif_get_tuple(env, head, &arity, &stmt_spec)) {
            fprintf(stderr, "bad grammar %d\n", __LINE__);
            return enif_make_badarg(env);
        }
        if (arity != 4) {
            fprintf(stderr, "bad grammar %d\n", __LINE__);
            return enif_make_badarg(env);
        }
        if (!enif_get_list_length(env, stmt_spec[2], &len)) {
            fprintf(stderr, "bad grammar %d\n", __LINE__);
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
        "integer",
        NULL};
    const char *uint_types[] = {
        "non-negative-integer",
        "fraction-digits-arg",
        NULL};
    const char *atom_or_uint_types[] = {
        "max-value",
        NULL};
    int i;

    for (i = 0; atom_types[i]; i++) {
        t = yang_get_arg_type(
            yang_get_arg_type_idx(yang_make_atom(atom_types[i])));
        t->flags |= F_ERL_ATOM;
    }
    for (i = 0; int_types[i]; i++) {
        t = yang_get_arg_type(
            yang_get_arg_type_idx(yang_make_atom(int_types[i])));
        t->flags |= F_ERL_INT;
    }
    for (i = 0; uint_types[i]; i++) {
        t = yang_get_arg_type(
            yang_get_arg_type_idx(yang_make_atom(uint_types[i])));
        t->flags |= F_ERL_UINT;
    }
    for (i = 0; atom_or_uint_types[i]; i++) {
        t = yang_get_arg_type(
            yang_get_arg_type_idx(yang_make_atom(atom_or_uint_types[i])));
        t->flags |= F_ERL_ATOM_OR_UINT;
    }
    t = yang_get_arg_type(
        yang_get_arg_type_idx(yang_make_atom("identifier-ref")));
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
    {"get_statement_spec", 1, get_statement_spec_nif},
    {"get_grammar_module_names", 0, get_grammar_module_names_nif},
    {"parse", 2, parse_nif}
};


ERL_NIF_INIT(yang_parser, nif_funcs, load, NULL, upgrade, NULL)
