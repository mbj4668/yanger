-module(eng25448_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("yanger/include/yang.hrl").

-import(test_support, [add_stmt_tree/3,
                       add_stmt_tree/4,
                       sn_find_name/2,
                       canonical_sn/1,
                       canonical_type/1,
                       get_canonical_type/3,
                       get_unused_imports/2,
                       get_imports/2,
                       get_default/3,
                       dummy_pos/1]). 


%% This module contains tests for ENG-25448, i.e., that deviated types should
%% be resolved in the deviating module and not in the target module.
%% Nearly all test case make use of a standard target module, which in
%% essence is:
%%
%%   typedef the-type {
%%     type boolean;
%%   }
%%
%%   container master {
%%       leaf item {
%%           type the-type;
%%       }
%%   }
%%
%% Then various deviations are applied, the simplest along the lines of:
%%
%%  deviation '/target:master/target:item' {
%%      deviate replace {
%%          type boolean;
%%      }
%%  }
%%

can_deviate_replace_builtin_type_test() ->
    {true, Ctx1, _} = add_stmt_tree(undefined, "target",
                                    [{container, master,
                                      [{leaf, item, [{type, string}]}]}]),

    Code = [{deviation, <<"/target:master/target:item">>,
             [{deviate, replace, [{type, int32}]}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1, "builtin-dev",
                                    [{imports, ["target"]}],
                                    Code),

    ?assertEqual([], Ctx2#yctx.errors),
    ?assertEqual(['builtin-dev'], get_imports("target", Ctx2)),
    ?assertEqual(['builtin-dev'], get_unused_imports("target", Ctx2)),
    ?assertEqual(int32, get_canonical_type("target", item, Ctx2)),
    ok.

can_deviate_replace_typedef_type_test() ->
    %% This test case checks that the type name is resolved in
    %% correct (bultin-dev) scope.
    {true, Ctx1, _} = add_stmt_tree(undefined, "target",
                                    [{container, master,
                                      [{leaf, item, [{type, string}]}]}]),

    Code = [
            {typedef, the_type, [{type, int32}]},
            {deviation, <<"/target:master/target:item">>,
             [{deviate, replace, [{type, the_type}]}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1, "builtin-dev",
                                    [{imports, ["target"]}],
                                    Code),

    ?assertEqual([], Ctx2#yctx.errors),
    ?assertEqual(['builtin-dev'], get_imports("target", Ctx2)),
    ?assertEqual([], get_unused_imports("target", Ctx2)),
    ?assertEqual(int32, get_canonical_type("target", item, Ctx2)),
    ok.

can_deviate_replace_typedef_from_target_test() ->
    %% This is a legacy test that checks that the type can also be evaluated
    %% in target scope. We emit a warning for that because it should really
    %% be solved with an import in the deviating module.
    {true, Ctx0, _} = add_stmt_tree(undefined, "imported",
                                    [{typedef, the_type, [{type, int32}]}]),

    {true, Ctx1, _} = add_stmt_tree(Ctx0, "target",
                                    [{imports, ["imported"]}],
                                    [{container, master,
                                      [{leaf, item, [{type, string}]}]}]),

    Code = [
            {deviation, <<"/target:master/target:item">>,
             [{deviate, replace, [{type, {imported, the_type}}]}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1, "builtin-dev",
                                    [{imports, ["target"]}],
                                    Code),

    ?assertMatch([#yerror{level = warning,
                          code = 'YANG_ERR_PREFIX_NOT_FOUND'}],
                 Ctx2#yctx.errors),
    ?assertEqual(['builtin-dev', imported], get_imports("target", Ctx2)),
    ?assertEqual(['builtin-dev'], get_unused_imports("target", Ctx2)),
    ?assertEqual(int32, get_canonical_type("target", item, Ctx2)),
    ok.

true_prefix_not_found_test() ->
    %% Similar to can_deviate_replace_typedef_from_target_test above, this
    %% checks when prefix cannot be found at all.
    {true, Ctx1, _} = add_stmt_tree(undefined, "target",
                                    [{container, master,
                                      [{leaf, item, [{type, string}]}]}]),

    Code = [
            {deviation, <<"/target:master/target:item">>,
             [{deviate, replace, [{type, {imported, the_type}}]}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1, "builtin-dev",
                                    [{imports, ["target"]}],
                                    Code),

    ErrCode = 'YANG_ERR_PREFIX_NOT_FOUND',

    ?assertMatch([#yerror{level = error, code = ErrCode,
                          pos = {"./builtin-dev.yang", _}}
                 ],
                 Ctx2#yctx.errors),
    ?assertEqual(['builtin-dev'], get_imports("target", Ctx2)),
    ?assertEqual(['builtin-dev'], get_unused_imports("target", Ctx2)),
    ok.

can_deviate_replace_prefixed_local_typedef_type_test() ->
    %% This test case checks that the type name is resolved in
    %% correct (bultin-dev) scope, even if so prefixed.
    {true, Ctx1, _} = add_stmt_tree(undefined, "target",
                                    [{container, master,
                                      [{leaf, item, [{type, string}]}]}]),

    Code = [
            {typedef, the_type, [{type, int32}]},
            {deviation, <<"/target:master/target:item">>,
             [{deviate, replace, [{type, {'builtin-dev', the_type}}]}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1, "builtin-dev",
                                    [{imports, ["target"]}],
                                    Code),

    ?assertEqual([], Ctx2#yctx.errors),
    ?assertEqual(['builtin-dev'], get_imports("target", Ctx2)),
    ?assertEqual([], get_unused_imports("target", Ctx2)),
    ?assertEqual(int32, get_canonical_type("target", item, Ctx2)),
    ok.

can_deviate_replace_local_typedef_type_test() ->
    %% This test case checks that the type name is resolved in
    %% correct (bultin-dev) scope, even though the same name exist in target.
    {true, Ctx1, _} = add_stmt_tree(undefined, "target",
                                    [{typedef, the_type, [{type, string}]},
                                     {container, master,
                                      [{leaf, item, [{type, the_type}]}]}]),

    Code = [
            {typedef, the_type, [{type, int32}]},
            {deviation, <<"/target:master/target:item">>,
             [{deviate, replace, [{type, the_type}]}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1, "builtin-dev",
                                    [{imports, ["target"]}],
                                    Code),

    ?assertEqual([], Ctx2#yctx.errors),
    ?assertEqual(['builtin-dev'], get_imports("target", Ctx2)),
    ?assertEqual([], get_unused_imports("target", Ctx2)),
    ?assertEqual(int32, get_canonical_type("target", item, Ctx2)),
    ok.

can_deviate_replace_typedef_in_target_test() ->
    %% This test case checks that types from the target module can also be
    %% resolved if needed. This would typically be the case where properties
    %% are added, like ranges or length restrictions
    {true, Ctx1, _} = add_stmt_tree(undefined, "target",
                                    [{typedef, the_type, [{type, int32}]},
                                     {container, master,
                                      [{leaf, item, [{type, the_type}]}]}]),

    Code = [
            {typedef, the_type, [{type, int64}]},
            {deviation, <<"/target:master/target:item">>,
             [{deviate, replace, [{type, {target, the_type}}]}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1, "builtin-dev",
                                    [{imports, ["target"]}],
                                    Code),

    ?assertEqual([], Ctx2#yctx.errors),
    ?assertEqual(['builtin-dev'], get_imports("target", Ctx2)),
    ?assertEqual(['builtin-dev'], get_unused_imports("target", Ctx2)),
    ?assertEqual(int32, get_canonical_type("target", item, Ctx2)),
    ok.

can_deviate_replace_local_typedef_in_target_test() ->
    %% This test case checks that types from the target module can also be
    %% resolved without prefix if needed. This would typically be the case
    %% where properties are added, like ranges or length restrictions. Note
    %% that if the type name (the_type) exists in both deviating and target
    %% modules then the deviating one takes precedence.
    {true, Ctx1, _} = add_stmt_tree(undefined, "target",
                                    [{typedef, the_type, [{type, int32}]},
                                     {container, master,
                                      [{leaf, item, [{type, the_type}]}]}]),

    Code = [
            {deviation, <<"/target:master/target:item">>,
             [{deviate, replace, [{type, the_type}]}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1, "builtin-dev",
                                    [{imports, ["target"]}],
                                    Code),

    ?assertMatch([#yerror{level = warning,
                          code = 'YANG_ERR_DEFINITION_NOT_FOUND'}],
                 Ctx2#yctx.errors),
    ?assertEqual(['builtin-dev'], get_imports("target", Ctx2)),
    ?assertEqual(['builtin-dev'], get_unused_imports("target", Ctx2)),
    ?assertEqual(int32, get_canonical_type("target", item, Ctx2)),
    ok.

true_typedef_cannot_be_found_test() ->
    %% Similar to can_deviate_replace_local_typedef_in_target_test, this
    %% checks when the type cannot be found at all.
    {true, Ctx1, _} = add_stmt_tree(undefined, "target",
                                    [%{typedef, the_type, [{type, int32}]},
                                     {container, master,
                                      [{leaf, item, [{type, the_type}]}]}]),

    Code = [
            {deviation, <<"/target:master/target:item">>,
             [{deviate, replace, [{type, the_type}]}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1, "builtin-dev",
                                    [{imports, ["target"]}],
                                    Code),

    ErrCode = 'YANG_ERR_DEFINITION_NOT_FOUND',

    %% This results in two errors, one for each reference to the
    %% non-existent type.
    ?assertMatch([#yerror{level = error, code = ErrCode,
                          pos = {"./builtin-dev.yang", _}},
                  #yerror{level = error, code = ErrCode,
                          pos = {"./target.yang", _}}
                 ],
                 Ctx2#yctx.errors),
    ?assertEqual(['builtin-dev'], get_imports("target", Ctx2)),
    ?assertEqual(['builtin-dev'], get_unused_imports("target", Ctx2)),
    ok.

can_deviate_replace_imported_typedef_type_test() ->
    %% This test case checks that imports from deviating module (builtin-dev)
    %% can be accessed in the deviation. Note that the imports are not done
    %% in the target module.
    {true, Ctx1, _} = add_stmt_tree(undefined, "target",
                                    [{container, master,
                                      [{leaf, item, [{type, string}]}]}]),

    {true, Ctx1b, _} = add_stmt_tree(Ctx1, "imported",
                                     [{typedef, the_type, [{type, int32}]}]),

    Code = [{deviation, <<"/target:master/target:item">>,
             [{deviate, replace, [{type, {imported, the_type}}]}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1b, "builtin-dev",
                                    [{imports, ["target", "imported"]}],
                                    Code),

    ?assertEqual([], Ctx2#yctx.errors),
    ?assertEqual(['builtin-dev', imported], get_imports("target", Ctx2)),
    ?assertEqual(['builtin-dev'], get_unused_imports("target", Ctx2)),
    ?assertEqual(int32, get_canonical_type("target", item, Ctx2)),
    ok.

can_deviate_replace_imported_nested_typedef_type_test() ->
    %% This test case checks that imports from deviating module (builtin-dev)
    %% can be accessed in the deviation. Note that the imports are not done
    %% in the target module.
    {true, Ctx1, _} = add_stmt_tree(undefined, "target",
                                    [{container, master,
                                      [{leaf, item, [{type, string}]}]}]),

    {true, Ctx1b, _} = add_stmt_tree(Ctx1, "imported",
                                     [{typedef, the_type, [{type, int32}]}]),

    Code = [{typedef, the_type, [{type, {imported, the_type}}]},
            {deviation, <<"/target:master/target:item">>,
             [{deviate, replace, [{type, the_type}]}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1b, "builtin-dev",
                                    [{imports, ["target", "imported"]}],
                                    Code),

    ?assertEqual([], Ctx2#yctx.errors),
    ?assertEqual(['builtin-dev', imported], get_imports("target", Ctx2)),
    ?assertEqual([imported], get_unused_imports("target", Ctx2)),
    ?assertEqual(int32, get_canonical_type("target", item, Ctx2)),
    ok.

can_use_grouping_with_typedef_test() ->
    %%
    {true, Ctx1, _} = add_stmt_tree(undefined, "imported",
                                    [{typedef, the_type, [{type, int32}]},
                                     {grouping, master,
                                      [{leaf, item, [{type, the_type}]}]}]),

    Code = [
            {container, master, [{uses, {imported, master}}]}
           ],

    {true, Ctx2, _} = add_stmt_tree(Ctx1, "builtin-dev",
                                    [{imports, ["imported"]}],
                                    Code),

    ?assertEqual([], Ctx2#yctx.errors),
    ?assertEqual(int32, get_canonical_type("builtin-dev", item, Ctx2)),
    ok.

can_deviate_replace_local_typedef_through_imported_grouping_test() ->
    %%
    {true, Ctx1, _} = add_stmt_tree(undefined, "imported",
                                    [{grouping, master,
                                      [{leaf, item, [{type, string}]}]}]),

    Code = [{typedef, the_type, [{type, int32}]},
            {container, master, [{uses, {imported, master}}]},

            {deviation, <<"/master/item">>,
             [{deviate, replace, [{type, the_type}]}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1, "builtin-dev",
                                    [{imports, ["imported"]}],
                                    Code),

    ?assertEqual([], Ctx2#yctx.errors),
    ?assertEqual([imported], get_imports("builtin-dev", Ctx2)),
    ?assertEqual([], get_unused_imports("builtin-dev", Ctx2)),
    ?assertEqual(int32, get_canonical_type("builtin-dev", item, Ctx2)),
    ok.

can_deviate_add_new_property_test() ->
    {true, Ctx1, _} = add_stmt_tree(undefined, "target",
                                    [{container, master,
                                      [{leaf, item, [{type, string}]}]}]),

    Code = [{deviation, <<"/target:master/target:item">>,
             [{deviate, add, [{default, "joe"}]}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1, "builtin-dev",
                                    [{imports, ["target"]}],
                                    Code),

    ?assertEqual([], Ctx2#yctx.errors),
    ?assertEqual(['builtin-dev'], get_imports("target", Ctx2)),
    ?assertEqual(['builtin-dev'], get_unused_imports("target", Ctx2)),
    ?assertMatch({_, "joe"}, get_default("target", item, Ctx2)),
    ok.

can_deviate_delete_property_test() ->
    {true, Ctx1, _} = add_stmt_tree(undefined, "target",
                                    [{container, master,
                                      [{leaf, item, [{type, string},
                                                     {default, "joe"}]}]}]),

    Code = [{deviation, <<"/target:master/target:item">>,
             [{deviate, delete, [{default, "joe"}]}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1, "builtin-dev",
                                    [{imports, ["target"]}],
                                    Code),

    ?assertEqual([], Ctx2#yctx.errors),
    ?assertMatch(undefined, get_default("target", item, Ctx2)),
    ok.

can_follow_leafref_test() ->
    %% This test case checks that imports from deviating module (builtin-dev)
    %% can be accessed in the deviation. Note that the imports are not done
    %% in the target module.
    {true, Ctx1, _} = add_stmt_tree(undefined, "target",
                                    [{container, master,
                                      [{leaf, item, [{type, string}]}]}]),

    {true, Ctx1b, _} = add_stmt_tree(Ctx1, "imported",
                                     [{container, top,
                                       [{leaf, thing, [{type, int32}]}]}]),

    Code = [{deviation, <<"/target:master/target:item">>,
             [{deviate, replace,
               [{type, leafref,
                 [{path, <<"/imported:top/imported:thing">>}]}]}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1b, "builtin-dev",
                                    [{imports, ["target", "imported"]}],
                                    Code),

    ?assertEqual([], Ctx2#yctx.errors),
    ?assertEqual(['builtin-dev', imported], get_imports("target", Ctx2)),
    ?assertEqual(['builtin-dev'], get_unused_imports("target", Ctx2)),
    ?assertEqual(leafref, get_canonical_type("target", item, Ctx2)),
    ok.

%% -------------------------------------
%% Test cases to span the different error messages

can_NOT_deviate_add_existing_property_test() ->
    {true, Ctx1, _} = add_stmt_tree(undefined, "target",
                                    [{container, master,
                                      [{leaf, item, [{type, string}]}]}]),

    Code = [{deviation, <<"/target:master/target:item">>,
             [{deviate, add, [{type, int32}]}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1, "builtin-dev",
                                    [{imports, ["target"]}],
                                    Code),

    ?assertMatch([{yerror, error, _,
                   'YANG_ERR_BAD_DEVIATE_ADD', ["type"]}],
                 Ctx2#yctx.errors),
    ok.

can_deviate_add_existing_non_singleton_property_test() ->
    {true, Ctx1, _} = add_stmt_tree(undefined, "target",
                                    [{list, master,
                                      [{key, "name"},
                                       {unique, "ip"},
                                       {leaf, name, [{type, string}]},
                                       {leaf, ip, [{type, int32}]},
                                       {leaf, port, [{type, int32}]}]}]),

    Code = [{deviation, <<"/target:master">>,
             [{deviate, add, [{unique, "port"}]}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1, "builtin-dev",
                                    [{imports, ["target"]}],
                                    Code),

    ?assertMatch([], Ctx2#yctx.errors),
    ok.

can_NOT_deviate_replace_non_existing_property_test() ->
    {true, Ctx1, _} = add_stmt_tree(undefined, "target",
                                    [{container, master,
                                      [{leaf, item, [{type, string}]}]}]),

    Code = [{deviation, <<"/target:master/target:item">>,
             [{deviate, replace, [{default, int32}]}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1, "builtin-dev",
                                    [{imports, ["target"]}],
                                    Code),

    ?assertMatch([{yerror, error, _,
                   'YANG_ERR_BAD_DEVIATE_REPLACE_NOT_FOUND', ["default"]}],
                 Ctx2#yctx.errors),
    ok.

can_NOT_deviate_replace_non_singular_property_test() ->
    {true, Ctx1, _} = add_stmt_tree(undefined, "target",
                                    [{list, master,
                                      [{key, "name"},
                                       {unique, "ip"},
                                       {leaf, name, [{type, string}]},
                                       {leaf, ip, [{type, int32}]},
                                       {leaf, port, [{type, int32}]}]}]),

    Code = [{deviation, <<"/target:master">>,
             [{deviate, replace, [{unique, "port"}]}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1, "builtin-dev",
                                    [{imports, ["target"]}],
                                    Code),

    ?assertMatch([{yerror, error, _,
                   'YANG_ERR_BAD_DEVIATE_REPLACE', ["unique"]}],
                 Ctx2#yctx.errors),
    ok.

can_NOT_deviate_away_list_key_test() ->
    {true, Ctx1, _} = add_stmt_tree(undefined, "target",
                                    [{list, master,
                                      [{key, "name"},
                                       {unique, "ip"},
                                       {leaf, name, [{type, string}]},
                                       {leaf, ip, [{type, int32}]},
                                       {leaf, port, [{type, int32}]}]}]),

    Code = [{deviation, <<"/target:master/target:name">>,
             [{deviate, 'not-supported'}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1, "builtin-dev",
                                    [{imports, ["target"]}],
                                    Code),

    ?assertMatch([{yerror, error, _,
                   'YANG_ERR_BAD_KEY', [name]}],
                 Ctx2#yctx.errors),
    ok.

can_NOT_deviate_delete_non_existing_property_test() ->
    {true, Ctx1, _} = add_stmt_tree(undefined, "target",
                                    [{container, master,
                                      [{leaf, item, [{type, string},
                                                     {default, "joe"}]}]}]),

    Code = [{deviation, <<"/target:master/target:item">>,
             [{deviate, delete, [{units, "ms"}]}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1, "builtin-dev",
                                    [{imports, ["target"]}],
                                    Code),

    ?assertMatch([{yerror, error, _,
                   'YANG_ERR_BAD_DEVIATE_DELETE', ["units", "ms"]}],
                 Ctx2#yctx.errors),
    ok.

can_NOT_deviate_non_deviatable_property_test() ->
    {true, Ctx1, _} = add_stmt_tree(undefined, "target",
                                    [{container, master,
                                      [{leaf, item, [{type, string},
                                                     {default, "joe"}]}]}]),

    Code = [{deviation, <<"/target:master/target:item">>,
             [{deviate, add, [{larry, "ms"}]}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1, "builtin-dev",
                                    [{imports, ["target"]}],
                                    Code),

    ?assertMatch([{yerror, error, _,
                   'YANG_ERR_BAD_DEVIATION', ["larry", leaf]}],
                 Ctx2#yctx.errors),
    ok.

can_NOT_deviate_default_case_in_choice_test() ->
    TargetCode = [{container, master,
                   [{choice, pick_one,
                     [{default, <<"two">>},
                      {'case', one, [{leaf, one, [{type, int32}]}]},
                      {'case', two, [{leaf, two, [{type, string}]}]}
                     ]}]}],
    {true, Ctx1, _} = add_stmt_tree(undefined, "target", TargetCode),

    Code = [{deviation, <<"/target:master/target:pick_one/target:two">>,
             [{deviate, 'not-supported'}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1, "builtin-dev",
                                    [{imports, ["target"]}],
                                    Code),

    ?assertMatch([{yerror, error, _,
                   'YANG_ERR_DEFAULT_CASE_NOT_FOUND', [<<"two">>]}],
                 Ctx2#yctx.errors),
    ok.

can_deviate_default_case_in_choice_if_default_removed_first_test() ->
    {true, Ctx1, _} = add_stmt_tree(undefined, "target",
                                    [{container, master,
                                      [{choice, pick_one,
                                        [{default, <<"two">>},
                                         {'case', one, [{leaf, one,
                                                         [{type, int32}]}]},
                                         {'case', two, [{leaf, two,
                                                         [{type, string}]}]}
                                        ]}]}]),

    Code = [{deviation, <<"/target:master/target:pick_one">>,
             [{deviate, delete, [{default, <<"two">>}]}]},
            {deviation, <<"/target:master/target:pick_one/target:two">>,
             [{deviate, 'not-supported'}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1, "builtin-dev",
                                    [{imports, ["target"]}],
                                    Code),

    ?assertMatch([], Ctx2#yctx.errors),
    ok.

can_NOT_deviate_replace_same_prefix_scope_test() ->
    %% Check that the same prefix is not used in deviating and target modules
    {true, Ctx0, _} = add_stmt_tree(undefined, "types1",
                                    [{typedef, the_type, [{type, boolean}]}]),

    {true, Ctx0b, _} = add_stmt_tree(Ctx0, "types2",
                                     [{typedef, the_type, [{type, int32}]}]),

    {true, Ctx1, _} = add_stmt_tree(Ctx0b, "target",
                                    [{imports, [{"types1", "types"}]}],
                                    [{container, master,
                                      [{leaf, item, [{type, {types, the_type}}]
                                       }]}]),

    Code = [
            {deviation, <<"/target:master/target:item">>,
             [{deviate, replace, [{type, {types, the_type}}]}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1, "builtin-dev",
                                    [{imports, ["target",
                                                {"types2", "types"}]}],
                                    Code),

    ?assertMatch([{yerror, error, _, 'YANG_ERR_PREFIX_MOD_MISMATCH',
                   _}], Ctx2#yctx.errors),
    ok.
