#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>

#include "yang_core_grammar.h"
#include "yang_grammar.h"

static const char *stmts[] = {
    "module",           "identifier",
        "yang-version",     "?",
        "namespace",        "1",
        "prefix",           "1",
        "$cut",             "*",
        "import",           "*",
        "include",          "*",
        "$cut",             "*",
        "organization",     "?",
        "contact",          "?",
        "description",      "?",
        "reference",        "?",
        "$cut",             "*",
        "revision",         "*",
        "$cut",             "*",
        "extension",        "*",
        "feature",          "*",
        "identity",         "*",
        "typedef",          "*",
        "grouping",         "*",
        "rpc",              "*",
        "notification",     "*",
        "deviation",        "*",
        "augment",          "*",
        "container",        "*",
        "leaf",             "*",
        "leaf-list",        "*",
        "list",             "*",
        "choice",           "*",
        "anyxml",           "*",
        "uses",             "*",
        NULL,               NULL,
    "submodule",        "identifier",
        "yang-version",     "?",
        "belongs-to",       "1",
        "$cut",             "*",
        "import",           "*",
        "include",          "*",
        "$cut",             "*",
        "organization",     "?",
        "contact",          "?",
        "description",      "?",
        "reference",        "?",
        "$cut",             "*",
        "revision",         "*",
        "$cut",             "*",
        "extension",        "*",
        "feature",          "*",
        "identity",         "*",
        "typedef",          "*",
        "grouping",         "*",
        "rpc",              "*",
        "notification",     "*",
        "deviation",        "*",
        "augment",          "*",
        "container",        "*",
        "leaf",             "*",
        "leaf-list",        "*",
        "list",             "*",
        "choice",           "*",
        "anyxml",           "*",
        "uses",             "*",
        NULL,               NULL,
    "yang-version",     "version",
        NULL,               NULL,
    "namespace",        "uri",
        NULL,               NULL,
    "prefix",           "identifier",
        NULL,               NULL,
    "import",           "identifier",
        "prefix",           "1",
        "revision-date",    "?",
        NULL,               NULL,
    "include",          "identifier",
        "revision-date",    "?",
        NULL,               NULL,
    "revision-date",    "date",
        NULL,               NULL,
    "organization",     "string",
        NULL,               NULL,
    "contact",          "string",
        NULL,               NULL,
    "deviation",        "absolute-schema-nodeid",
        "description",      "?",
        "reference",        "?",
        "deviate",          "*",
        NULL,               NULL,
    "mandatory",        "boolean",
        NULL,               NULL,
    "reference",        "string",
        NULL,               NULL,
    "presence",         "string",
        NULL,               NULL,
    "when",             "string",
        "description",      "?",
        "reference",        "?",
        NULL,               NULL,
    "if-feature",       "identifier-ref",
        NULL,               NULL,
    "anyxml",           "identifier",
        "when",             "?",
        "if-feature",       "*",
        "config",           "?",
        "mandatory",        "?",
        "status",           "?",
        "description",      "?",
        "reference",        "?",
        NULL,               NULL,
    "argument",         "identifier",
        "yin-element",      "?",
        NULL,               NULL,
    "container",        "identifier",
        "when",             "?",
        "if-feature",       "*",
        "must",             "*",
        "presence",         "?",
        "config",           "?",
        "status",           "?",
        "description",      "?",
        "reference",        "?",
        "typedef",          "*",
        "grouping",         "*",
        "container",        "*",
        "leaf",             "*",
        "leaf-list",        "*",
        "list",             "*",
        "choice",           "*",
        "anyxml",           "*",
        "uses",             "*",
        NULL,               NULL,
    "refine",           "descendant-schema-nodeid",
        "must",             "*",
        "presence",         "?",
        "default",          "?",
        "config",           "?",
        "mandatory",        "?",
        "min-elements",     "?",
        "max-elements",     "?",
        "description",      "?",
        "reference",        "?",
        NULL,               NULL,
    "augment",          "schema-nodeid",
        "when",             "?",
        "if-feature",       "*",
        "status",           "?",
        "description",      "?",
        "reference",        "?",
        "case",             "*",
        "container",        "*",
        "leaf",             "*",
        "leaf-list",        "*",
        "list",             "*",
        "choice",           "*",
        "anyxml",           "*",
        "uses",             "*",
        NULL,               NULL,
    "ordered-by",       "ordered-by-arg",
        NULL,               NULL,
    "input",            NULL,
        "status",           "?",
        "description",      "?",
        "reference",        "?",
        "typedef",          "*",
        "grouping",         "*",
        "container",        "*",
        "leaf",             "*",
        "leaf-list",        "*",
        "list",             "*",
        "choice",           "*",
        "anyxml",           "*",
        "uses",             "*",
        NULL,               NULL,
    "typedef",          "identifier",
        "type",             "1",
        "units",            "?",
        "default",          "?",
        "status",           "?",
        "description",      "?",
        "reference",        "?",
        NULL,               NULL,
    "length",           "length-arg",
        "error-message",    "?",
        "error-app-tag",    "?",
        "description",      "?",
        "reference",        "?",
        NULL,               NULL,
    "output",           NULL,
        "status",           "?",
        "description",      "?",
        "reference",        "?",
        "typedef",          "*",
        "grouping",         "*",
        "container",        "*",
        "leaf",             "*",
        "leaf-list",        "*",
        "list",             "*",
        "choice",           "*",
        "anyxml",           "*",
        "uses",             "*",
        NULL,               NULL,
    "pattern",          "string",
        "error-message",    "?",
        "error-app-tag",    "?",
        "description",      "?",
        "reference",        "?",
        NULL,               NULL,
    "min-elements",     "non-negative-integer",
        NULL,               NULL,
    "leaf-list",        "identifier",
        "when",             "?",
        "if-feature",       "*",
        "type",             "1",
        "units",            "?",
        "must",             "*",
        "config",           "?",
        "min-elements",     "?",
        "max-elements",     "?",
        "ordered-by",       "?",
        "status",           "?",
        "description",      "?",
        "reference",        "?",
        NULL,               NULL,
    "feature",          "identifier",
        "if-feature",       "*",
        "status",           "?",
        "description",      "?",
        "reference",        "?",
        NULL,               NULL,
    "leaf",             "identifier",
        "when",             "?",
        "if-feature",       "*",
        "type",             "1",
        "units",            "?",
        "must",             "*",
        "default",          "?",
        "config",           "?",
        "mandatory",        "?",
        "status",           "?",
        "description",      "?",
        "reference",        "?",
        NULL,               NULL,
    "units",            "string",
        NULL,               NULL,
    "type",             "identifier-ref",
        "fraction-digits",  "?",
        "range",            "?",
        "length",           "?",
        "pattern",          "*",
        "enum",             "*",
        "bit",              "*",
        "path",             "?",
        "require-instance", "?",
        "require-instance", "?",
        "base",             "?",
        "type",             "*",
        NULL,               NULL,
    "deviate",          "deviate-arg",
        "type",             "?",
        "units",            "?",
        "must",             "*",
        "unique",           "*",
        "default",          "?",
        "config",           "?",
        "mandatory",        "?",
        "min-elements",     "?",
        "max-elements",     "?",
        NULL,               NULL,
    "revision",         "date",
        "description",      "?",
        "reference",        "?",
        NULL,               NULL,
    "status",           "status-arg",
        NULL,               NULL,
    "case",             "identifier",
        "when",             "?",
        "if-feature",       "*",
        "must",             "*",
        "status",           "?",
        "description",      "?",
        "reference",        "?",
        "choice",           "*",
        "container",        "*",
        "leaf",             "*",
        "leaf-list",        "*",
        "list",             "*",
        "anyxml",           "*",
        "uses",             "*",
        NULL,               NULL,
    "require-instance", "boolean",
        NULL,               NULL,
    "description",      "string",
        NULL,               NULL,
    "enum",             "enum-arg",
        "value",            "?",
        "status",           "?",
        "description",      "?",
        "reference",        "?",
        NULL,               NULL,
    "choice",           "identifier",
        "when",             "?",
        "if-feature",       "*",
        "default",          "?",
        "must",             "*",
        "config",           "?",
        "mandatory",        "?",
        "status",           "?",
        "description",      "?",
        "reference",        "?",
        "case",             "*",
        "container",        "*",
        "leaf",             "*",
        "leaf-list",        "*",
        "list",             "*",
        "anyxml",           "*",
        NULL,               NULL,
    "error-app-tag",    "string",
        NULL,               NULL,
    "base",             "identifier-ref",
        NULL,               NULL,
    "uses",             "identifier-ref",
        "when",             "?",
        "if-feature",       "*",
        "status",           "?",
        "description",      "?",
        "reference",        "?",
        "refine",           "*",
        "augment",          "*",
        NULL,               NULL,
    "key",              "key-arg",
        NULL,               NULL,
    "position",         "non-negative-integer",
        NULL,               NULL,
    "path",             "path-arg",
        NULL,               NULL,
    "bit",              "identifier",
        "position",         "?",
        "status",           "?",
        "description",      "?",
        "reference",        "?",
        NULL,               NULL,
    "unique",           "unique-arg",
        NULL,               NULL,
    "identity",         "identifier",
        "base",             "?",
        "status",           "?",
        "description",      "?",
        "reference",        "?",
        NULL,               NULL,
    "must",             "string",
        "error-message",    "?",
        "error-app-tag",    "?",
        "description",      "?",
        "reference",        "?",
        NULL,               NULL,
    "yin-element",      "boolean",
        NULL,               NULL,
    "belongs-to",       "identifier",
        "prefix",           "1",
        NULL,               NULL,
    "max-elements",     "max-value",
        NULL,               NULL,
    "error-message",    "string",
        NULL,               NULL,
    "extension",        "identifier",
        "argument",         "?",
        "status",           "?",
        "description",      "?",
        "reference",        "?",
        NULL,               NULL,
    "default",          "string",
        NULL,               NULL,
    "config",           "boolean",
        NULL,               NULL,
    "fraction-digits",  "fraction-digits-arg",
        NULL,               NULL,
    "list",             "identifier",
        "when",             "?",
        "if-feature",       "*",
        "must",             "*",
        "key",              "?",
        "unique",           "*",
        "config",           "?",
        "min-elements",     "?",
        "max-elements",     "?",
        "ordered-by",       "?",
        "status",           "?",
        "description",      "?",
        "reference",        "?",
        "typedef",          "*",
        "grouping",         "*",
        "container",        "*",
        "leaf",             "*",
        "leaf-list",        "*",
        "list",             "*",
        "choice",           "*",
        "anyxml",           "*",
        "uses",             "*",
        NULL,               NULL,
    "rpc",              "identifier",
        "if-feature",       "*",
        "status",           "?",
        "description",      "?",
        "reference",        "?",
        "typedef",          "*",
        "grouping",         "*",
        "input",            "?",
        "output",           "?",
        NULL,               NULL,
    "value",            "integer",
        NULL,               NULL,
    "range",            "range-arg",
        "error-message",    "?",
        "error-app-tag",    "?",
        "description",      "?",
        "reference",        "?",
        NULL,               NULL,
    "notification",     "identifier",
        "if-feature",       "*",
        "status",           "?",
        "description",      "?",
        "reference",        "?",
        "typedef",          "*",
        "grouping",         "*",
        "container",        "*",
        "leaf",             "*",
        "leaf-list",        "*",
        "list",             "*",
        "choice",           "*",
        "anyxml",           "*",
        "uses",             "*",
        NULL,               NULL,
    "grouping",         "identifier",
        "status",           "?",
        "description",      "?",
        "reference",        "?",
        "typedef",          "*",
        "grouping",         "*",
        "container",        "*",
        "leaf",             "*",
        "leaf-list",        "*",
        "list",             "*",
        "choice",           "*",
        "anyxml",           "*",
        "uses",             "*",
        NULL,               NULL,

    NULL,              NULL
};

static bool
chk_enum_arg(char *arg, void *opaque)
{
    int len;
    len = strlen(arg);
    if (len == 0 || isspace(arg[0]) || isspace(arg[len-1])) {
        return false;
    }
    return true;
}

static bool
chk_fraction_digits_arg(char *arg, void *opaque)
{
    char *end;
    long int i;
    if (isspace(arg[0])) {
        return false;
    }
    i = strtol(arg, &end, 10);
    if (*end != '\0') {
        /* not an integer */
        return false;
    }
    if (i < 1 || i > 18) {
        return false;
    }
    return true;
}

bool
chk_(char *arg, void *opaque)
{
    return true;
}

#define NTYPES 23

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
    types[i].syntax.xsd_regexp = (char *)identifier;
    types[i].flags = F_ARG_TYPE_SYNTAX_REGEXP;
    i++;

    types[i].name = yang_make_atom("version");
    types[i].syntax.xsd_regexp = (char *)"1";
    types[i].flags = F_ARG_TYPE_SYNTAX_REGEXP;
    i++;

    types[i].name = yang_make_atom("date");
    types[i].syntax.xsd_regexp =
        (char *)"[1-2][0-9]{3}-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])";
    types[i].flags = F_ARG_TYPE_SYNTAX_REGEXP;
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
    types[i].syntax.xsd_regexp = (char *)"unbounded|[1-9][0-9]*";
    types[i].flags = F_ARG_TYPE_SYNTAX_REGEXP;
    i++;

    types[i].name = yang_make_atom("non-negative-integer");
    types[i].syntax.xsd_regexp = (char *)"(0|[1-9])[0-9]*";
    types[i].flags = F_ARG_TYPE_SYNTAX_REGEXP;
    i++;

    types[i].name = yang_make_atom("deviate-arg");
    types[i].syntax.xsd_regexp = (char *)"add|delete|replace|not-supported";
    types[i].flags = F_ARG_TYPE_SYNTAX_REGEXP;
    i++;

    types[i].name = yang_make_atom("integer");
    types[i].syntax.xsd_regexp = (char *)"\\d*";
    types[i].flags = F_ARG_TYPE_SYNTAX_REGEXP;
    i++;

    types[i].name = yang_make_atom("status-arg");
    types[i].syntax.xsd_regexp = (char *)"current|obsolete|deprecated";
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

    types[i].name = yang_make_atom("schema-nodeid");
    types[i].syntax.xsd_regexp = (char *)schema_nodeid;
    types[i].flags = F_ARG_TYPE_SYNTAX_REGEXP;
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



