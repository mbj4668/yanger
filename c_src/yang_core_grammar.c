#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <libxml/xmlregexp.h>

#include "yang_core_grammar.h"
#include "yang_grammar.h"
#include "yang_if_feature.h"

static const char *stmts[] = {
    "module",           "identifier",
        "1",     "yang-version",     "?",
        "1",     "namespace",        "1",
        "1",     "prefix",           "1",
        "1",     "$cut",             "*",
        "1",     "import",           "*",
        "1",     "include",          "*",
        "1",     "$cut",             "*",
        "1",     "organization",     "?",
        "1",     "contact",          "?",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        "1",     "$cut",             "*",
        "1",     "revision",         "*",
        "1",     "$cut",             "*",
        "1",     "extension",        "*",
        "1",     "feature",          "*",
        "1",     "identity",         "*",
        "1",     "typedef",          "*",
        "1",     "grouping",         "*",
        "1",     "rpc",              "*",
        "1",     "notification",     "*",
        "1",     "deviation",        "*",
        "1",     "augment",          "*",
        "1",     "container",        "*",
        "1",     "leaf",             "*",
        "1",     "leaf-list",        "*",
        "1",     "list",             "*",
        "1",     "choice",           "*",
        "1",     "anyxml",           "*",
        "1.1",   "anydata",          "*",
        "1",     "uses",             "*",
        NULL,    NULL,               NULL,
    "submodule",        "identifier",
        "1",     "yang-version",     "?",
        "1",     "belongs-to",       "1",
        "1",     "$cut",             "*",
        "1",     "import",           "*",
        "1",     "include",          "*",
        "1",     "$cut",             "*",
        "1",     "organization",     "?",
        "1",     "contact",          "?",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        "1",     "$cut",             "*",
        "1",     "revision",         "*",
        "1",     "$cut",             "*",
        "1",     "extension",        "*",
        "1",     "feature",          "*",
        "1",     "identity",         "*",
        "1",     "typedef",          "*",
        "1",     "grouping",         "*",
        "1",     "rpc",              "*",
        "1",     "notification",     "*",
        "1",     "deviation",        "*",
        "1",     "augment",          "*",
        "1",     "container",        "*",
        "1",     "leaf",             "*",
        "1",     "leaf-list",        "*",
        "1",     "list",             "*",
        "1",     "choice",           "*",
        "1",     "anyxml",           "*",
        "1.1",   "anydata",          "*",
        "1",     "uses",             "*",
        NULL,    NULL,               NULL,
    "yang-version",     "version",
        NULL,    NULL,               NULL,
    "namespace",        "uri",
        NULL,    NULL,               NULL,
    "prefix",           "identifier",
        NULL,    NULL,               NULL,
    "import",           "identifier",
        "1",     "prefix",           "1",
        "1",     "revision-date",    "?",
        "1.1",   "description",      "?",
        "1.1",   "reference",        "?",
        NULL,    NULL,               NULL,
    "include",          "identifier",
        "1",     "revision-date",    "?",
        "1.1",   "description",      "?",
        "1.1",   "reference",        "?",
        NULL,    NULL,               NULL,
    "revision-date",    "date",
        NULL,    NULL,               NULL,
    "organization",     "string",
        NULL,    NULL,               NULL,
    "contact",          "string",
        NULL,    NULL,               NULL,
    "deviation",        "absolute-schema-nodeid",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        "1",     "deviate",          "*",
        NULL,    NULL,               NULL,
    "mandatory",        "boolean",
        NULL,    NULL,               NULL,
    "reference",        "string",
        NULL,    NULL,               NULL,
    "presence",         "string",
        NULL,    NULL,               NULL,
    "when",             "string",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        NULL,    NULL,               NULL,
    "if-feature",       "if-feature-expr",
        NULL,    NULL,               NULL,
    "anyxml",           "identifier",
        "1",     "when",             "?",
        "1",     "if-feature",       "*",
        "1",     "must",             "*",
        "1",     "config",           "?",
        "1",     "mandatory",        "?",
        "1",     "status",           "?",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        NULL,    NULL,               NULL,
    "anydata",          "identifier",
        "1.1",   "when",             "?",
        "1.1",   "if-feature",       "*",
        "1.1",   "must",             "*",
        "1.1",   "config",           "?",
        "1.1",   "mandatory",        "?",
        "1.1",   "status",           "?",
        "1.1",   "description",      "?",
        "1.1",   "reference",        "?",
        NULL,    NULL,               NULL,
    "argument",         "identifier",
        "1",     "yin-element",      "?",
        NULL,    NULL,               NULL,
    "container",        "identifier",
        "1",     "when",             "?",
        "1",     "if-feature",       "*",
        "1",     "must",             "*",
        "1",     "presence",         "?",
        "1",     "config",           "?",
        "1",     "status",           "?",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        "1",     "typedef",          "*",
        "1",     "grouping",         "*",
        "1",     "container",        "*",
        "1",     "leaf",             "*",
        "1",     "leaf-list",        "*",
        "1",     "list",             "*",
        "1",     "choice",           "*",
        "1",     "anyxml",           "*",
        "1.1",   "anydata",          "*",
        "1",     "uses",             "*",
        "1.1",   "action",           "*",
        "1.1",   "notification",     "*",
        NULL,    NULL,               NULL,
    "refine",           "descendant-schema-nodeid",
        "1.1",   "if-feature",       "*",
        "1",     "must",             "*",
        "1",     "presence",         "?",
        "1",     "default",          "?",
        "1",     "config",           "?",
        "1",     "mandatory",        "?",
        "1",     "min-elements",     "?",
        "1",     "max-elements",     "?",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        NULL,    NULL,               NULL,
    "augment",          "schema-nodeid",
        "1",     "when",             "?",
        "1",     "if-feature",       "*",
        "1",     "status",           "?",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        "1",     "case",             "*",
        "1",     "container",        "*",
        "1",     "leaf",             "*",
        "1",     "leaf-list",        "*",
        "1",     "list",             "*",
        "1",     "choice",           "*",
        "1",     "anyxml",           "*",
        "1.1",   "anydata",          "*",
        "1",     "uses",             "*",
        "1.1",   "action",           "*",
        "1.1",   "notification",     "*",
        NULL,    NULL,               NULL,
    "ordered-by",       "ordered-by-arg",
        NULL,    NULL,               NULL,
    "input",            NULL,
        "1.1",   "must",             "*",
        "1",     "status",           "?",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        "1",     "typedef",          "*",
        "1",     "grouping",         "*",
        "1",     "container",        "*",
        "1",     "leaf",             "*",
        "1",     "leaf-list",        "*",
        "1",     "list",             "*",
        "1",     "choice",           "*",
        "1",     "anyxml",           "*",
        "1.1",   "anydata",          "*",
        "1",     "uses",             "*",
        NULL,    NULL,               NULL,
    "typedef",          "identifier",
        "1",     "type",             "1",
        "1",     "units",            "?",
        "1",     "default",          "?",
        "1",     "status",           "?",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        NULL,    NULL,               NULL,
    "length",           "length-arg",
        "1",     "error-message",    "?",
        "1",     "error-app-tag",    "?",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        NULL,    NULL,               NULL,
    "output",           NULL,
        "1.1",   "must",             "*",
        "1",     "status",           "?",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        "1",     "typedef",          "*",
        "1",     "grouping",         "*",
        "1",     "container",        "*",
        "1",     "leaf",             "*",
        "1",     "leaf-list",        "*",
        "1",     "list",             "*",
        "1",     "choice",           "*",
        "1",     "anyxml",           "*",
        "1.1",   "anydata",          "*",
        "1",     "uses",             "*",
        NULL,    NULL,               NULL,
     "pattern",          "string",
        "1.1",   "modifier",         "?",
        "1",     "error-message",    "?",
        "1",     "error-app-tag",    "?",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        NULL,    NULL,               NULL,
     "modifier",         "modifier-arg",
        NULL,    NULL,               NULL,
     "min-elements",     "non-negative-integer",
        NULL,    NULL,               NULL,
     "leaf-list",        "identifier",
        "1",     "when",             "?",
        "1",     "if-feature",       "*",
        "1",     "type",             "1",
        "1",     "units",            "?",
        "1",     "must",             "*",
        "1.1",   "default",          "*",
        "1",     "config",           "?",
        "1",     "min-elements",     "?",
        "1",     "max-elements",     "?",
        "1",     "ordered-by",       "?",
        "1",     "status",           "?",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        NULL,    NULL,               NULL,
     "feature",          "identifier",
        "1",     "if-feature",       "*",
        "1",     "status",           "?",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        NULL,    NULL,               NULL,
     "leaf",             "identifier",
        "1",     "when",             "?",
        "1",     "if-feature",       "*",
        "1",     "type",             "1",
        "1",     "units",            "?",
        "1",     "must",             "*",
        "1",     "default",          "?",
        "1",     "config",           "?",
        "1",     "mandatory",        "?",
        "1",     "status",           "?",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        NULL,    NULL,               NULL,
     "units",            "string",
        NULL,    NULL,               NULL,
     "type",             "identifier-ref",
        "1",     "fraction-digits",  "?",
        "1",     "range",            "?",
        "1",     "length",           "?",
        "1",     "pattern",          "*",
        "1",     "enum",             "*",
        "1",     "bit",              "*",
        "1",     "path",             "?",
        "1",     "require-instance", "?",
        /*
          NOTE: this code allows 1.1 syntax in 1.0 modules (multiple base
          statements) - this needs to be checked by the caller.
        */
        "1",     "base",             "*",
        "1",     "type",             "*",
        NULL,    NULL,               NULL,
     "deviate",          "deviate-arg",
        "1",     "type",             "?",
        "1",     "units",            "?",
        "1",     "must",             "*",
        "1",     "unique",           "*",
        "1",     "default",          "?",
        "1",     "config",           "?",
        "1",     "mandatory",        "?",
        "1",     "min-elements",     "?",
        "1",     "max-elements",     "?",
        NULL,    NULL,               NULL,
     "revision",         "date",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        NULL,    NULL,               NULL,
     "status",           "status-arg",
        NULL,    NULL,               NULL,
     "case",             "identifier",
        "1",     "when",             "?",
        "1",     "if-feature",       "*",
        "1",     "status",           "?",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        "1",     "choice",           "*",
        "1",     "container",        "*",
        "1",     "leaf",             "*",
        "1",     "leaf-list",        "*",
        "1",     "list",             "*",
        "1",     "anyxml",           "*",
        "1.1",   "anydata",          "*",
        "1",     "uses",             "*",
        NULL,    NULL,               NULL,
     "require-instance", "boolean",
        NULL,    NULL,               NULL,
     "description",      "string",
        NULL,    NULL,               NULL,
     "enum",             "enum-arg",
        "1.1",   "if-feature",       "*",
        "1",     "value",            "?",
        "1",     "status",           "?",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        NULL,    NULL,               NULL,
     "choice",           "identifier",
        "1",     "when",             "?",
        "1",     "if-feature",       "*",
        "1",     "default",          "?",
        "1",     "must",             "*",
        "1",     "config",           "?",
        "1",     "mandatory",        "?",
        "1",     "status",           "?",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        "1",     "case",             "*",
        "1.1",   "choice",           "*",
        "1",     "container",        "*",
        "1",     "leaf",             "*",
        "1",     "leaf-list",        "*",
        "1",     "list",             "*",
        "1",     "anyxml",           "*",
        "1.1",   "anydata",          "*",
        NULL,    NULL,               NULL,
     "error-app-tag",    "string",
        NULL,    NULL,               NULL,
     "base",             "identifier-ref",
        NULL,    NULL,               NULL,
     "uses",             "identifier-ref",
        "1",     "when",             "?",
        "1",     "if-feature",       "*",
        "1",     "status",           "?",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        "1",     "refine",           "*",
        "1",     "augment",          "*",
        NULL,    NULL,               NULL,
     "key",              "key-arg",
        NULL,    NULL,               NULL,
     "position",         "non-negative-integer",
        NULL,    NULL,               NULL,
     "path",             "path-arg",
        NULL,    NULL,               NULL,
     "bit",              "identifier",
        "1.1",   "if-feature",       "*",
        "1",     "position",         "?",
        "1",     "status",           "?",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        NULL,    NULL,               NULL,
     "unique",           "unique-arg",
        NULL,    NULL,               NULL,
     "identity",         "identifier",
        "1.1",   "if-feature",       "*",
        /*
          NOTE: this code allows 1.1 syntax in 1.0 modules (multiple base
          statements) - this needs to be checked by the caller.
        */
        "1",     "base",             "*",
        "1",     "status",           "?",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        NULL,    NULL,               NULL,
     "must",             "string",
        "1",     "error-message",    "?",
        "1",     "error-app-tag",    "?",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        NULL,    NULL,               NULL,
     "yin-element",      "boolean",
        NULL,    NULL,               NULL,
     "belongs-to",       "identifier",
        "1",     "prefix",           "1",
        NULL,    NULL,               NULL,
     "max-elements",     "max-value",
        NULL,    NULL,               NULL,
     "error-message",    "string",
        NULL,    NULL,               NULL,
     "extension",        "identifier",
        "1",     "argument",         "?",
        "1",     "status",           "?",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        NULL,    NULL,               NULL,
     "default",          "string",
        NULL,    NULL,               NULL,
     "config",           "boolean",
        NULL,    NULL,               NULL,
     "fraction-digits",  "fraction-digits-arg",
        NULL,    NULL,               NULL,
     "list",             "identifier",
        "1",     "when",             "?",
        "1",     "if-feature",       "*",
        "1",     "must",             "*",
        "1",     "key",              "?",
        "1",     "unique",           "*",
        "1",     "config",           "?",
        "1",     "min-elements",     "?",
        "1",     "max-elements",     "?",
        "1",     "ordered-by",       "?",
        "1",     "status",           "?",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        "1",     "typedef",          "*",
        "1",     "grouping",         "*",
        "1",     "container",        "*",
        "1",     "leaf",             "*",
        "1",     "leaf-list",        "*",
        "1",     "list",             "*",
        "1",     "choice",           "*",
        "1",     "anyxml",           "*",
        "1.1",   "anydata",          "*",
        "1",     "uses",             "*",
        "1.1",   "action",           "*",
        "1.1",   "notification",     "*",
        NULL,    NULL,               NULL,
     "rpc",              "identifier",
        "1",     "if-feature",       "*",
        "1",     "status",           "?",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        "1",     "typedef",          "*",
        "1",     "grouping",         "*",
        "1",     "input",            "?",
        "1",     "output",           "?",
        NULL,    NULL,               NULL,
     "action",              "identifier",
        "1.1",   "if-feature",       "*",
        "1.1",   "status",           "?",
        "1.1",   "description",      "?",
        "1.1",   "reference",        "?",
        "1.1",   "typedef",          "*",
        "1.1",   "grouping",         "*",
        "1.1",   "input",            "?",
        "1.1",   "output",           "?",
        NULL,    NULL,               NULL,
     "value",            "integer",
        NULL,    NULL,               NULL,
     "range",            "range-arg",
        "1",     "error-message",    "?",
        "1",     "error-app-tag",    "?",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        NULL,    NULL,               NULL,
     "notification",     "identifier",
        "1",     "if-feature",       "*",
        "1.1",   "must",             "*",
        "1",     "status",           "?",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        "1",     "typedef",          "*",
        "1",     "grouping",         "*",
        "1",     "container",        "*",
        "1",     "leaf",             "*",
        "1",     "leaf-list",        "*",
        "1",     "list",             "*",
        "1",     "choice",           "*",
        "1",     "anyxml",           "*",
        "1.1",   "anydata",          "*",
        "1",     "uses",             "*",
        NULL,    NULL,               NULL,
     "grouping",         "identifier",
        "1",     "status",           "?",
        "1",     "description",      "?",
        "1",     "reference",        "?",
        "1",     "typedef",          "*",
        "1",     "grouping",         "*",
        "1",     "container",        "*",
        "1",     "leaf",             "*",
        "1",     "leaf-list",        "*",
        "1",     "list",             "*",
        "1",     "choice",           "*",
        "1",     "anyxml",           "*",
        "1.1",   "anydata",          "*",
        "1",     "uses",             "*",
        "1.1",   "action",           "*",
        "1.1",   "notification",     "*",
        NULL,    NULL,               NULL,

    NULL,              NULL
};

static bool
chk_enum_arg(char *arg, void *opaque, char yang_version, char *errbuf, int sz)
{
    int len;
    len = strlen(arg);
    if (len == 0 || isspace((int)arg[0]) || isspace((int)arg[len-1])) {
        return false;
    }
    return true;
}

static bool
get_int64(char *arg, int64_t *i)
{
    char *end;
    long long ll;

    if (isspace((int)arg[0])) {
        return false;
    }
    errno = 0;
    ll = strtoll(arg, &end, 10);
    if (*end != '\0') {
        /* not an integer */
        return false;
    }
    if (errno == ERANGE || ll < INT64_MIN || ll > INT64_MAX) {
        /* over/underflow */
        return false;
    }
    *i = ll;
    return true;
}

static bool
get_uint64(char *arg, uint64_t *i)
{
    char *end;
    unsigned long long ull;

    if (isspace((int)arg[0]) || arg[0] == '-') {
        return false;
    }
    errno = 0;
    ull = strtoull(arg, &end, 10);
    if (*end != '\0') {
        /* not an integer */
        return false;
    }
    if (errno == ERANGE || ull > UINT64_MAX) {
        /* over/underflow */
        return false;
    }
    *i = ull;
    return true;
}

static bool
chk_identifier(char *arg, void *opaque, char yang_version, char *errbuf, int sz)
{
    char *p;
    xmlRegexpPtr xreg = (xmlRegexpPtr)opaque;
    if (xmlRegexpExec(xreg, (xmlChar *)arg) != 1) {
        return false;
    }
    if (yang_version != YANG_VERSION_1) {
        return true;
    }
    /* YANG 1: ensure arg doesn't start with [xX][mM][lL] */
    p = arg;
    if (*p == '\0' || (*p != 'x' && *p != 'X')) {
        return true;
    }
    p++;
    if (*p == '\0' || (*p != 'm' && *p != 'M')) {
        return true;
    }
    p++;
    if (*p == '\0' || (*p != 'l' && *p != 'L')) {
        return true;
    }
    snprintf(errbuf, sz,
             "bad argument value \"%s\", an identifier must not start with"
             " [xX][mM][lL] in YANG version 1", arg);
    return false;
}

static bool
chk_date(char *arg, void *opaque, char yang_version, char *errbuf, int sz)
{
    xmlRegexpPtr xreg = (xmlRegexpPtr)opaque;
    int y,m,d;
    int days = 31;
    if (xmlRegexpExec(xreg, (xmlChar *)arg) != 1) {
        return false;
    }
    sscanf(arg, "%d-%d-%d", &y, &m, &d);

    if (m < 1 || m > 12 || d < 1) {
        return false;
    }
    if (m == 2) {
        days = 28;
        if (y % 400 == 0 || (y % 4 == 0 && y % 100 != 0)) {
            days = 29;
        }
    } else if (m == 4 || m == 6 || m == 9 || m == 11) {
        days = 30;
    }

    if (d > days) {
        return false;
    }
    return true;
}


/*
  We limit to int64_t and uint64_t here,
  because that's what we return to Erlang in
  yang_parser_nif.c - a simple regexp results
  in silent truncation of larger values.
*/
static bool
chk_integer(char *arg, void *opaque, char yang_version, char *errbuf, int sz)
{
    int64_t i;
    return get_int64(arg, &i);
}

static bool
chk_non_negative_integer(char *arg, void *opaque, char yang_version,
                         char *errbuf, int sz)
{
    uint64_t i;
    return get_uint64(arg, &i);
}

static bool
chk_max_value(char *arg, void *opaque, char yang_version, char *errbuf, int sz)
{
    uint64_t i;

    if (strcmp(arg, "unbounded") == 0) {
        return true;
    }
    return get_uint64(arg, &i);
}

static bool
chk_fraction_digits_arg(char *arg, void *opaque, char yang_version,
                        char *errbuf, int sz)
{
    uint64_t i;

    if (get_uint64(arg, &i) == false) {
        return false;
    }
    if (i < 1 || i > 18) {
        return false;
    }
    return true;
}

static bool
chk_if_feature_expr(char *arg, void *opaque, char yang_version,
                    char *errbuf, int sz)
{
    if (yang_version == YANG_VERSION_1) {
        /* YANG version 1: check identifier-ref regexp */
        xmlRegexpPtr xreg = (xmlRegexpPtr)opaque;
        if (xmlRegexpExec(xreg, (xmlChar *)arg) != 1) {
            snprintf(errbuf, sz,
                     "bad argument value \"%s\", should be of type "
                     "identifier-ref in YANG version 1", arg);
            return false;
        } else {
            return true;
        }
    } else {
        return yang_parse_if_feature_expr(arg);
    }
}

#define NTYPES 26

#define STRBUFSIZ 256

int
yang_init_core_stmt_grammar(void)
{
    struct yang_arg_type types[NTYPES];
    int i = 0;
    const char *identifier;
    const char *prefix;
    char keyword[STRBUFSIZ];
    const char *length_str;
    char length_expr[STRBUFSIZ];
    const char *range_str;
    char range_expr[STRBUFSIZ];
    char rel_path_keyexpr[STRBUFSIZ];
    char *node_id;
    char *identifier_ref;
    char path_key_expr[STRBUFSIZ];
    char path_equality_expr[STRBUFSIZ];
    char path_predicate[STRBUFSIZ];
    char absolute_path_arg[2*STRBUFSIZ];
    char descendant_path_arg[3*STRBUFSIZ];
    char relative_path_arg[3*STRBUFSIZ];
    char deref_path_arg[5*STRBUFSIZ];
    char path_arg[9*STRBUFSIZ];
    char strict_path_arg[9*STRBUFSIZ];
    char absolute_schema_nodeid[STRBUFSIZ];
    char descendant_schema_nodeid[STRBUFSIZ];
    char schema_nodeid[STRBUFSIZ];
    char unique_arg[STRBUFSIZ];
    char key_arg[STRBUFSIZ];
    // URI - RFC 3986, Appendix A
    const char *scheme = "[A-Za-z][-+.A-Za-z0-9]*";
    const char *unreserved = "[-._~A-Za-z0-9]";
    const char *pct_encoded = "%[0-9A-F]{2}";
    const char *sub_delims = "[!$&'()*+,;=]";
    const char *dec_octet = "([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])";
    const char *h16 = "[0-9A-F]{1,4}";
    const char *port = "[0-9]*";
    char pchar[STRBUFSIZ];
    char segment[STRBUFSIZ];
    char segment_nz[STRBUFSIZ];
    char userinfo[STRBUFSIZ];
    char ipv4address[STRBUFSIZ];
    char ls32[STRBUFSIZ];
    char ipv6address[6*STRBUFSIZ];
    char ipvfuture[STRBUFSIZ];
    char ipliteral[6*STRBUFSIZ];
    char reg_name[STRBUFSIZ];
    char host[7*STRBUFSIZ];
    char authority[7*STRBUFSIZ];
    char path_abempty[STRBUFSIZ];
    char path_absolute[STRBUFSIZ];
    char path_rootless[STRBUFSIZ];
    char path_empty[STRBUFSIZ];
    char hier_part[8*STRBUFSIZ];
    char query[STRBUFSIZ];
    char *fragment;
    char uri[9*STRBUFSIZ];

#define S(BUF, FMT, ARGS ...) {                                         \
    unsigned int __n = snprintf(BUF, sizeof(BUF), FMT, ARGS);           \
    if (__n > sizeof(BUF)) {                                            \
        fprintf(stderr, "%s:%d sizeof(%s) = %d need %d\n",              \
                __FILE__, __LINE__, #BUF, (int)sizeof(BUF), __n);       \
    }                                                                   \
    assert(__n <= sizeof(BUF));                                         \
}

    identifier = "[_A-Za-z][._\\-A-Za-z0-9]*";
    prefix = identifier;
    S(keyword, "((%s):)?(%s)", prefix, identifier);

    length_str =
        "((min|max|[0-9]+)\\s*"
        "(\\.\\.\\s*"
        "(min|max|[0-9]+)\\s*)?)";
    S(length_expr, "%s(\\|\\s*%s)*", length_str, length_str);

    range_str =
        "((\\-INF|min|max|((\\+|\\-)?[0-9]+(\\.[0-9]+)?))\\s*"
        "(\\.\\.\\s*"
        "(INF|min|max|(\\+|\\-)?[0-9]+(\\.[0-9]+)?)\\s*)?)";
    S(range_expr, "%s(\\|\\s*%s)*", range_str, range_str);


    node_id = keyword;
    identifier_ref = node_id;
    S(rel_path_keyexpr, "(\\.\\./)+(%s/)*%s", node_id, node_id);
    S(path_key_expr, "(current\\s*\\(\\s*\\)/%s)", rel_path_keyexpr);
    S(path_equality_expr, "%s\\s*=\\s*%s", node_id, path_key_expr);
    S(path_predicate, "\\s*\\[\\s*%s\\s*\\]\\s*", path_equality_expr);
    S(absolute_path_arg, "(/%s(%s)*)+",  node_id, path_predicate);
    S(descendant_path_arg, "%s(%s)*(%s)?",
      node_id, path_predicate, absolute_path_arg);
    S(relative_path_arg, "(\\.\\./)*%s", descendant_path_arg);
    S(deref_path_arg, "deref\\s*\\(\\s*(%s)\\s*\\)/\\.\\./%s",
      relative_path_arg, relative_path_arg);
    S(path_arg, "(%s|%s|%s)",
      absolute_path_arg, relative_path_arg, deref_path_arg);
    S(strict_path_arg, "(%s|%s)",
      absolute_path_arg, relative_path_arg);
    S(absolute_schema_nodeid, "(/%s)+", node_id);
    S(descendant_schema_nodeid, "%s(%s)?",
      node_id, absolute_schema_nodeid);
    S(schema_nodeid, "(%s|%s)",
      absolute_schema_nodeid, descendant_schema_nodeid);
    S(unique_arg, "%s(\\s+%s)*",
      descendant_schema_nodeid,  descendant_schema_nodeid);
    S(key_arg, "%s(\\s+%s)*", node_id, node_id);

    S(pchar, "(%s|%s|%s|[:@])", unreserved, pct_encoded, sub_delims);
    S(segment, "%s*", pchar);
    S(segment_nz, "%s+", pchar);
    S(userinfo, "(%s|%s|%s|:)*", unreserved, pct_encoded, sub_delims);
    S(ipv4address, "(%s.){3}%s", dec_octet, dec_octet);
    S(ls32, "(%s:%s|%s)", h16, h16, ipv4address);
    S(ipv6address,
      "((%s:){6}%s"
      "|::(%s:){5}%s"
      "|(%s)?::(%s:){4}%s"
      "|((%s:)?%s)?::(%s:){3}%s"
      "|((%s:){,2}%s)?::(%s:){2}%s"
      "|((%s:){,3}%s)?::%s:%s"
      "|((%s:){,4}%s)?::%s"
      "|((%s:){,5}%s)?::%s"
      "|((%s:){,6}%s)?::)",
      h16, ls32,
      h16, ls32,
      h16, h16, ls32,
      h16, h16, h16, ls32,
      h16, h16, h16, ls32,
      h16, h16, h16, ls32,
      h16, h16, ls32,
      h16, h16, ls32,
      h16, h16);
    S(ipvfuture, "v[0-9A-F]+\\.(%s|%s|:)+", unreserved, sub_delims);
    S(ipliteral, "\\[(%s|%s)\\]", ipv6address, ipvfuture);
    S(reg_name, "(%s|%s|%s)*", unreserved, pct_encoded, sub_delims);
    S(host, "(%s|%s|%s)", ipliteral, ipv4address, reg_name);
    S(authority, "(%s@)?%s(:%s)?", userinfo, host, port);
    S(path_abempty, "(/%s)*", segment);
    S(path_absolute, "/(%s(/%s)*)?", segment_nz, segment);
    S(path_rootless, "%s(/%s)*", segment_nz, segment);
    S(path_empty, "%s{0}", pchar);
    S(hier_part, "(//%s%s|%s|%s|%s)",
      authority, path_abempty, path_absolute, path_rootless, path_empty);
    S(query, "(%s|[/?])*", pchar);
    fragment = query;
    S(uri, "%s:%s(\\?%s)?(#%s)?",
      scheme, hier_part, query, fragment);


    /* Install the core types */
    memset(types, 0, sizeof(types));
    i = 0;

    types[i].name = yang_make_atom("string");
    i++;

    types[i].name = yang_make_atom("identifier");
    types[i].syntax.cb.opaque = (void *)xmlRegexpCompile((xmlChar *)identifier);
    types[i].syntax.cb.validate = &chk_identifier;
    types[i].flags = F_ARG_TYPE_SYNTAX_CB;
    i++;

    types[i].name = yang_make_atom("version");
    types[i].syntax.xsd_regexp = (char *)"1|1\\.1";
    types[i].flags = F_ARG_TYPE_SYNTAX_REGEXP;
    i++;

    types[i].name = yang_make_atom("date");
    types[i].syntax.cb.opaque =
        (void *)xmlRegexpCompile(
            (xmlChar *)
            "[1-2][0-9]{3}-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])");
    types[i].syntax.cb.validate = &chk_date;
    types[i].flags = F_ARG_TYPE_SYNTAX_CB;
    i++;

    types[i].name = yang_make_atom("ordered-by-arg");
    types[i].syntax.xsd_regexp = (char *)"user|system";
    types[i].flags = F_ARG_TYPE_SYNTAX_REGEXP;
    i++;

    types[i].name = yang_make_atom("boolean");
    types[i].syntax.xsd_regexp = (char *)"true|false";
    types[i].flags = F_ARG_TYPE_SYNTAX_REGEXP;
    i++;

    types[i].name = yang_make_atom("max-value");
    types[i].syntax.cb.validate = &chk_max_value;
    types[i].flags = F_ARG_TYPE_SYNTAX_CB;
    i++;

    types[i].name = yang_make_atom("non-negative-integer");
    types[i].syntax.cb.validate = &chk_non_negative_integer;
    types[i].flags = F_ARG_TYPE_SYNTAX_CB;
    i++;

    types[i].name = yang_make_atom("deviate-arg");
    types[i].syntax.xsd_regexp = (char *)"add|delete|replace|not-supported";
    types[i].flags = F_ARG_TYPE_SYNTAX_REGEXP;
    i++;

    types[i].name = yang_make_atom("integer");
    types[i].syntax.cb.validate = &chk_integer;
    types[i].flags = F_ARG_TYPE_SYNTAX_CB;
    i++;

    types[i].name = yang_make_atom("status-arg");
    types[i].syntax.xsd_regexp = (char *)"current|obsolete|deprecated";
    types[i].flags = F_ARG_TYPE_SYNTAX_REGEXP;
    i++;

    types[i].name = yang_make_atom("modifier-arg");
    types[i].syntax.xsd_regexp = (char *)"invert-match";
    types[i].flags = F_ARG_TYPE_SYNTAX_REGEXP;
    i++;

    types[i].name = yang_make_atom("uri");
    types[i].syntax.xsd_regexp = uri;
    types[i].flags = F_ARG_TYPE_SYNTAX_REGEXP;
    i++;

    types[i].name = yang_make_atom("length-arg");
    types[i].syntax.xsd_regexp = length_expr;
    types[i].flags = F_ARG_TYPE_SYNTAX_REGEXP;
    i++;

    S(key_arg, "%s(\\s+%s)*", node_id, node_id);
    types[i].name = yang_make_atom("key-arg");
    types[i].syntax.xsd_regexp = key_arg;
    types[i].flags = F_ARG_TYPE_SYNTAX_REGEXP;
    i++;

    types[i].name = yang_make_atom("descendant-schema-nodeid");
    types[i].syntax.xsd_regexp = descendant_schema_nodeid;
    types[i].flags = F_ARG_TYPE_SYNTAX_REGEXP;
    i++;

    types[i].name = yang_make_atom("absolute-schema-nodeid");
    types[i].syntax.xsd_regexp = absolute_schema_nodeid;
    types[i].flags = F_ARG_TYPE_SYNTAX_REGEXP;
    i++;

    types[i].name = yang_make_atom("enum-arg");
    types[i].syntax.cb.validate = &chk_enum_arg;
    types[i].flags = F_ARG_TYPE_SYNTAX_CB;
    i++;

    types[i].name = yang_make_atom("range-arg");
    types[i].syntax.xsd_regexp = range_expr;
    types[i].flags = F_ARG_TYPE_SYNTAX_REGEXP;
    i++;

    types[i].name = yang_make_atom("identifier-ref");
    types[i].syntax.xsd_regexp = identifier_ref;
    types[i].flags = F_ARG_TYPE_SYNTAX_REGEXP;
    i++;

    types[i].name = yang_make_atom("fraction-digits-arg");
    types[i].syntax.cb.validate = &chk_fraction_digits_arg;
    types[i].flags = F_ARG_TYPE_SYNTAX_CB;
    i++;

    types[i].name = yang_make_atom("unique-arg");
    types[i].syntax.xsd_regexp = (char *)unique_arg;
    types[i].flags = F_ARG_TYPE_SYNTAX_REGEXP;
    i++;

    types[i].name = yang_make_atom("path-arg");
    types[i].syntax.xsd_regexp = (char *)path_arg;
    types[i].flags = F_ARG_TYPE_SYNTAX_REGEXP;
    i++;

    types[i].name = yang_make_atom("strict-path-arg");
    types[i].syntax.xsd_regexp = (char *)strict_path_arg;
    types[i].flags = F_ARG_TYPE_SYNTAX_REGEXP;
    i++;

    types[i].name = yang_make_atom("schema-nodeid");
    types[i].syntax.xsd_regexp = (char *)schema_nodeid;
    types[i].flags = F_ARG_TYPE_SYNTAX_REGEXP;
    i++;

    types[i].name = yang_make_atom("if-feature-expr");
    types[i].syntax.cb.validate = &chk_if_feature_expr;
    types[i].syntax.cb.opaque =
        (void *)xmlRegexpCompile((xmlChar *)identifier_ref);
    types[i].flags = F_ARG_TYPE_SYNTAX_CB;
    i++;

    assert(i == NTYPES);

#undef S

    if (!yang_install_arg_types(types, i)) {
        return 0;
    }

    if (!yang_install_grammar_str(NULL, stmts)) {
        return 0;
    }
    return 1;
}



