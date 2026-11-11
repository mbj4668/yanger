-module(xpath_rewrite_tests).

-include_lib("eunit/include/eunit.hrl").

%% Tests for xpath_rewrite:rewrite_expr/2.
%%
%% Names that must NOT exist as atoms are only ever written as strings in
%% this module, and each such name is used in a single test, so that a
%% test which creates an atom (make_atoms) cannot affect another test.
%%
%% Names that must exist as atoms (known_tag, known_attr, known_ns) are
%% written as atoms in the expected results, which makes them existing
%% atoms once this module is loaded.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% not(fail) -> true()

not_of_failing_path_is_true_test() ->
    ?assertEqual(
        {function_call, xp_true, []},
        rewrite("not(/xpr_test_no_atom_not)", #{})
    ).

not_of_non_failing_path_is_kept_test() ->
    ?assertMatch(
        {function_call, xp_not, [{absolute, _}]},
        rewrite("not(/known_tag)", #{})
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Element tags (tag_of/2)

known_tag_test() ->
    ?assertEqual(
        {absolute, [{step, child, {name, known_tag}, []}]},
        rewrite("/known_tag", #{})
    ).

known_tag_with_prefix_test() ->
    ?assertEqual(
        {absolute, [{step, child, {name, known_ns, known_tag}, []}]},
        rewrite("/p:known_tag", prefixes())
    ).

unknown_tag_fails_without_creating_atom_test() ->
    Name = "xpr_test_no_atom_tag",
    ?assertEqual(fail, rewrite("/" ++ Name, #{})),
    assert_not_atom(Name).

unknown_tag_with_prefix_fails_without_creating_atom_test() ->
    Name = "xpr_test_no_atom_ptag",
    ?assertEqual(fail, rewrite("/p:" ++ Name, prefixes())),
    assert_not_atom(Name).

%% Unknown tags in a node test (rewrite_node_test/2), here via the
%% '//' rewrite.
unknown_tag_in_node_test_fails_without_creating_atom_test() ->
    Name = "xpr_test_no_atom_desc",
    ?assertEqual(fail, rewrite("//" ++ Name, #{})),
    assert_not_atom(Name).

known_tag_in_node_test_test() ->
    ?assertMatch(
        {absolute, [
            {step, descendant_or_self, {node_type, node}, [
                {pred, {relative, [{step, child, {name, known_tag}, []}]}}
            ]},
            {step, child, {name, known_tag}, []}
        ]},
        rewrite("//known_tag", #{})
    ).

%% An unknown tag is pruned locally, it doesn't fail the whole expression.
unknown_tag_is_pruned_from_union_test() ->
    Name = "xpr_test_no_atom_union",
    ?assertEqual(
        {absolute, [{step, child, {name, known_tag}, []}]},
        rewrite("/known_tag | /" ++ Name, #{})
    ),
    assert_not_atom(Name).

unknown_tag_in_predicate_test() ->
    Name = "xpr_test_no_atom_pred",
    ?assertEqual(fail, rewrite("/known_tag[" ++ Name ++ " = 1]", #{})),
    assert_not_atom(Name).

unknown_tag_is_created_with_make_atoms_test() ->
    Name = "xpr_test_make_atom_tag",
    assert_not_atom(Name),
    Res = rewrite("/" ++ Name, #{str_type => make_atoms}),
    ?assertEqual(
        {absolute, [{step, child, {name, list_to_existing_atom(Name)}, []}]},
        Res
    ).

unknown_tag_is_created_with_legacy_make_atoms_test() ->
    Name = "xpr_test_make_atom_tag_legacy",
    assert_not_atom(Name),
    Res = rewrite("/" ++ Name, #{make_atoms => true}),
    ?assertEqual(
        {absolute, [{step, child, {name, list_to_existing_atom(Name)}, []}]},
        Res
    ).

%% The legacy {make_atoms, {Prefixes, DefaultNs}} argument.
unknown_tag_is_created_with_legacy_make_atoms_ns_test() ->
    Name = "xpr_test_make_atom_legacy",
    assert_not_atom(Name),
    Res = xpath_rewrite:rewrite_expr(
        parse("/" ++ Name),
        {make_atoms, xpath_rewrite:prefixes_with_no_default_namespace(#{})}
    ),
    ?assertEqual(
        {absolute, [{step, child, {name, list_to_existing_atom(Name)}, []}]},
        Res
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Attributes (attr_of/2)

known_attr_test() ->
    ?assertEqual(
        {absolute, [
            {step, child, {name, known_tag}, []},
            {step, attribute, {name, known_attr}, []}
        ]},
        rewrite("/known_tag/@known_attr", #{})
    ).

known_attr_with_prefix_test() ->
    ?assertEqual(
        {absolute, [
            {step, child, {name, known_tag}, []},
            {step, attribute, {name, known_ns, known_attr}, []}
        ]},
        rewrite("/known_tag/@p:known_attr", prefixes())
    ).

unknown_attr_fails_without_creating_atom_test() ->
    Name = "xpr_test_no_atom_attr",
    ?assertEqual(fail, rewrite("/known_tag/@" ++ Name, #{})),
    assert_not_atom(Name).

unknown_attr_with_prefix_fails_without_creating_atom_test() ->
    Name = "xpr_test_no_atom_pattr",
    ?assertEqual(fail, rewrite("/known_tag/@p:" ++ Name, prefixes())),
    assert_not_atom(Name).

unknown_attr_is_created_with_make_atoms_test() ->
    Name = "xpr_test_make_atom_attr",
    assert_not_atom(Name),
    Res = rewrite("/known_tag/@" ++ Name, #{make_atoms => true}),
    ?assertEqual(
        {absolute, [
            {step, child, {name, known_tag}, []},
            {step, attribute, {name, list_to_existing_atom(Name)}, []}
        ]},
        Res
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Default namespace (default_ns_of/2)

default_ns_atom_test() ->
    ?assertEqual(
        {absolute, [{step, child, {name, known_ns, known_tag}, []}]},
        rewrite("/known_tag", #{default_ns => known_ns})
    ).

default_ns_existing_string_test() ->
    ?assertEqual(
        {absolute, [{step, child, {name, known_ns, known_tag}, []}]},
        rewrite("/known_tag", #{default_ns => "known_ns"})
    ).

default_ns_unknown_string_is_invalid_namespace_test() ->
    Name = "xpr_test_no_atom_ns",
    ?assertExit(
        {invalid_namespace, xpath_rewrite, _, Name},
        rewrite("/known_tag", #{default_ns => Name})
    ),
    assert_not_atom(Name).

%% The legacy {Prefixes, DefaultNs} argument.
default_ns_unknown_string_legacy_test() ->
    Name = "xpr_test_no_atom_ns_legacy",
    ?assertExit(
        {invalid_namespace, xpath_rewrite, _, Name},
        xpath_rewrite:rewrite_expr(
            parse("/known_tag"),
            xpath_rewrite:prefixes_with_default_namespace(#{}, Name)
        )
    ),
    assert_not_atom(Name).

default_ns_unknown_string_is_created_with_make_atoms_test() ->
    Name = "xpr_test_make_atom_ns",
    assert_not_atom(Name),
    Res = rewrite("/known_tag", #{default_ns => Name, make_atoms => true}),
    ?assertEqual(
        {absolute, [{step, child, {name, list_to_existing_atom(Name), known_tag}, []}]},
        Res
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% str_type option

%% existing_atoms (the default), given explicitly.
str_type_existing_atoms_known_tag_test() ->
    ?assertEqual(
        {absolute, [{step, child, {name, known_tag}, []}]},
        rewrite("/known_tag", #{str_type => existing_atoms})
    ).

str_type_existing_atoms_unknown_tag_test() ->
    Name = "xpr_test_no_atom_st_ea_tag",
    ?assertEqual(fail, rewrite("/" ++ Name, #{str_type => existing_atoms})),
    assert_not_atom(Name).

%% make_atoms => false is the same as the default.
legacy_make_atoms_false_unknown_tag_test() ->
    Name = "xpr_test_no_atom_ma_false_tag",
    ?assertEqual(fail, rewrite("/" ++ Name, #{make_atoms => false})),
    assert_not_atom(Name).

%% make_binaries - every name becomes a binary, even known atoms.
str_type_make_binaries_known_tag_test() ->
    ?assertEqual(
        {absolute, [{step, child, {name, <<"known_tag">>}, []}]},
        rewrite("/known_tag", #{str_type => make_binaries})
    ).

str_type_make_binaries_unknown_tag_test() ->
    Name = "xpr_test_no_atom_st_mb_tag",
    ?assertEqual(
        {absolute, [{step, child, {name, list_to_binary(Name)}, []}]},
        rewrite("/" ++ Name, #{str_type => make_binaries})
    ),
    assert_not_atom(Name).

str_type_make_binaries_tag_with_prefix_test() ->
    Name = "xpr_test_no_atom_st_mb_ptag",
    ?assertEqual(
        {absolute, [{step, child, {name, known_ns, list_to_binary(Name)}, []}]},
        rewrite("/p:" ++ Name, (prefixes())#{str_type => make_binaries})
    ),
    assert_not_atom(Name).

%% A prefix mapped to a binary namespace is passed through as is.
str_type_make_binaries_binary_ns_prefix_test() ->
    Tag = "xpr_test_no_atom_st_mb_bns_tag",
    Attr = "xpr_test_no_atom_st_mb_bns_attr",
    NS = <<"urn:xpr-test:binary-ns">>,
    ?assertEqual(
        {absolute, [
            {step, child, {name, NS, list_to_binary(Tag)}, []},
            {step, attribute, {name, NS, list_to_binary(Attr)}, []}
        ]},
        rewrite(
            "/p:" ++ Tag ++ "/@p:" ++ Attr,
            #{prefixes => #{"p" => NS}, str_type => make_binaries}
        )
    ),
    assert_not_atom(Tag),
    assert_not_atom(Attr).

str_type_make_binaries_attr_test() ->
    Name = "xpr_test_no_atom_st_mb_attr",
    ?assertEqual(
        {absolute, [
            {step, child, {name, <<"known_tag">>}, []},
            {step, attribute, {name, list_to_binary(Name)}, []}
        ]},
        rewrite("/known_tag/@" ++ Name, #{str_type => make_binaries})
    ),
    assert_not_atom(Name).

str_type_make_binaries_default_ns_test() ->
    Name = "xpr_test_no_atom_st_mb_ns",
    ?assertEqual(
        {absolute, [{step, child, {name, list_to_binary(Name), <<"known_tag">>}, []}]},
        rewrite("/known_tag", #{default_ns => Name, str_type => make_binaries})
    ),
    assert_not_atom(Name).

%% existing_atoms_or_binaries - known names become atoms, unknown
%% names become binaries.
str_type_existing_atoms_or_binaries_known_tag_test() ->
    ?assertEqual(
        {absolute, [{step, child, {name, known_tag}, []}]},
        rewrite("/known_tag", #{str_type => existing_atoms_or_binaries})
    ).

str_type_existing_atoms_or_binaries_unknown_tag_test() ->
    Name = "xpr_test_no_atom_st_eaob_tag",
    ?assertEqual(
        {absolute, [{step, child, {name, list_to_binary(Name)}, []}]},
        rewrite("/" ++ Name, #{str_type => existing_atoms_or_binaries})
    ),
    assert_not_atom(Name).

str_type_existing_atoms_or_binaries_attr_test() ->
    Name = "xpr_test_no_atom_st_eaob_attr",
    ?assertEqual(
        {absolute, [
            {step, child, {name, known_tag}, []},
            {step, attribute, {name, known_attr}, []},
            {step, attribute, {name, list_to_binary(Name)}, []}
        ]},
        rewrite(
            "/known_tag/@known_attr/@" ++ Name,
            #{str_type => existing_atoms_or_binaries}
        )
    ),
    assert_not_atom(Name).

str_type_existing_atoms_or_binaries_default_ns_test() ->
    Name = "xpr_test_no_atom_st_eaob_ns",
    ?assertEqual(
        {absolute, [{step, child, {name, list_to_binary(Name), known_tag}, []}]},
        rewrite("/known_tag", #{
            default_ns => Name,
            str_type => existing_atoms_or_binaries
        })
    ),
    assert_not_atom(Name),
    ?assertEqual(
        {absolute, [{step, child, {name, known_ns, known_tag}, []}]},
        rewrite("/known_tag", #{
            default_ns => "known_ns",
            str_type => existing_atoms_or_binaries
        })
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers

parse(Str) ->
    {ok, Q} = xpath_parse:parse(xpath_scan:tokens(Str)),
    Q.

rewrite(Str, Opts) ->
    xpath_rewrite:rewrite_expr(parse(Str), Opts).

prefixes() ->
    #{prefixes => #{"p" => known_ns}}.

assert_not_atom(Str) ->
    ?assertError(badarg, list_to_existing_atom(Str)).
