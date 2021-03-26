-module(deviation_update_modules_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("yanger/include/yang.hrl").

-import(test_support, [add_stmt_tree/3,
                       add_stmt_tree/4,
                       sn_find_name/2,
                       canonical_sn/1,
                       canonical_type/1,
                       dummy_pos/1,
                       get_modrevs/1]).


update_target_base_test() ->
    Ctx2 = add_target_and_deviating_modules(),

    TargetM = yang:get_module_from_filename("target", Ctx2),
    M = yang:get_module_from_filename("deviating", Ctx2),

    DeviatedModules = [TargetM],
    DeviatedChildren = [],
    Ignored = ignore_marker,

    CtxRes = yang:deviation_update_modules(DeviatedModules, M, TargetM,
                                           DeviatedChildren, Ignored, Ctx2),

    %% Check that ignored got assigned and that ModRevs are correct
    ?assertMatch([{{deviating, _}, #module{deviated_by = []}},
                  {{target, _}, #module{deviated_by = [{deviating, _}],
                                        ignored = ignore_marker}}],
                 get_modrevs(CtxRes)),
    ok.

update_target_no_deviated_by_duplicates_test() ->
    %% Checks that multiples are not added to the deviated_by list
    Ctx2 = add_target_and_deviating_modules(),

    TargetM0 = yang:get_module_from_filename("target", Ctx2),
    TargetM = TargetM0#module{deviated_by = [{deviating, undefined}]},
    M = yang:get_module_from_filename("deviating", Ctx2),

    DeviatedModules = [TargetM],
    DeviatedChildren = [],
    Ignored = ignore_marker,

    CtxRes = yang:deviation_update_modules(DeviatedModules, M, TargetM,
                                           DeviatedChildren, Ignored, Ctx2),

    ?assertMatch([{{deviating, _}, #module{deviated_by = []}},
                  {{target, _}, #module{deviated_by = [{deviating, _}],
                                        ignored = ignore_marker}}],
                 get_modrevs(CtxRes)),
    ok.

update_target_deviated_by_maintained_test() ->
    %% Checks that devaited_by is added to (and not set to)
    Ctx2 = add_target_and_deviating_modules(),

    TargetM0 = yang:get_module_from_filename("target", Ctx2),
    TargetM = TargetM0#module{deviated_by = [{john_wayne, undefined}]},
    M = yang:get_module_from_filename("deviating", Ctx2),

    DeviatedModules = [TargetM],
    DeviatedChildren = [],
    Ignored = ignore_marker,

    CtxRes = yang:deviation_update_modules(DeviatedModules, M, TargetM,
                                           DeviatedChildren, Ignored, Ctx2),

    ?assertMatch([{{deviating, _}, #module{deviated_by = []}},
                  {{target, _}, #module{deviated_by = [{deviating, _},
                                                       {john_wayne, _}],
                                        ignored = ignore_marker}}],
                 get_modrevs(CtxRes)),
    ok.

update_other_and_target_1_test() ->
    %% Check that two deviated modules can be updated, target first
    Ctx2 = add_target_and_deviating_and_other_modules(),

    OtherM = yang:get_module_from_filename("other", Ctx2),
    TargetM = yang:get_module_from_filename("target", Ctx2),
    M = yang:get_module_from_filename("deviating", Ctx2),

    DeviatedModules = [TargetM, OtherM],
    DeviatedChildren = [],
    Ignored = ignore_marker,

    CtxRes = yang:deviation_update_modules(DeviatedModules, M, TargetM,
                                           DeviatedChildren, Ignored, Ctx2),

    ?assertMatch([{{deviating, _}, #module{deviated_by = []}},
                  {{other, _}, #module{deviated_by = [{deviating, _}],
                                       ignored = []}},
                  {{target, _}, #module{deviated_by = [{deviating, _}],
                                        ignored = ignore_marker}}],
                 get_modrevs(CtxRes)).

deviate_remote_augmented_node_test() ->
    %% This checks that target and augmneting modules gets updated
    {true, Ctx1a, _} = add_stmt_tree(undefined, "target",
                                     [{container, master,
                                       [{leaf, boll, [{type, string}]}]}]),
    {true, Ctx1, _} = add_stmt_tree(Ctx1a, "augmenting",
                                    [{imports, ["target"]}],
                                    [{augment, <<"/target:master">>,
                                      [{leaf, item, [{type, string}]}]}]),

    Code = [{deviation, <<"/target:master/augmenting:item">>,
             [{deviate, replace, [{type, int32}]}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1, "deviating",
                                    [{imports, ["target", "augmenting"]}],
                                    Code),

    ?assertMatch([{{augmenting, _}, #module{deviated_by = [{deviating, _}],
                                            remote_augments = [{target, _}]}},
                  {{deviating, _}, #module{deviated_by = [],
                                           remote_augments = []}},
                  {{target, _}, #module{deviated_by = [{deviating, _}],
                                        remote_augments = []}}],
                 get_modrevs(Ctx2)).

deviate_local_augmented_node_test() ->
    %% This checks that target and augmneting modules gets updated
    {true, Ctx1, _} = add_stmt_tree(undefined, "target",
                                    [{container, master,
                                      [{leaf, boll, [{type, string}]}]},
                                     {augment, <<"/target:master">>,
                                      [{leaf, item, [{type, string}]}]}]),

    Code = [{deviation, <<"/target:master/target:item">>,
             [{deviate, replace, [{type, int32}]}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1, "deviating",
                                    [{imports, ["target"]}],
                                    Code),

    ?assertMatch([{{deviating, _}, #module{deviated_by = [],
                                           remote_augments = []}},
                  {{target, _}, #module{deviated_by = [{deviating, _}],
                                        remote_augments = [],
                                        local_augments = [_]}}],
                 get_modrevs(Ctx2)).

deviate_submodule_test() ->
    %% When a submodule deviates it will be recorded as its parent node.
    %% This test case uses the subm.yang file.
    {true, Ctx1, _} = add_stmt_tree(undefined, "target",
                                    [{container, master,
                                      [{leaf, item, [{type, string}]}]}]),

    Code = [],

    {true, Ctx2, _} = add_stmt_tree(Ctx1, "deviating",
                                    [{imports, ["target"]},
                                     {includes, ["subm"]}],
                                    Code),

    %% Deviating in the subm submodule gets recorded as if from deviating, 
    %% but the actual deviation is still in subm
    ?assertMatch([{{deviating, _}, #module{deviated_by = [],
                                           local_deviations = [],
                                           remote_deviations = []}},
                  {{subm, _}, #module{local_deviations = [],
                                      remote_deviations = [_]}},
                  {{target, _}, #module{deviated_by = [{deviating, _}]}}],
                 get_modrevs(Ctx2)).

%% -------------------------------------
%% Support stuff

add_target_and_deviating_and_other_modules() ->
    {true, Ctx, _} = add_stmt_tree(undefined, "other",
                                   [{container, master,
                                     [{leaf, item, [{type, string}]}]}]),
    add_target_and_deviating_modules(Ctx).

add_target_and_deviating_modules() ->
    add_target_and_deviating_modules(undefined).
add_target_and_deviating_modules(Ctx0) ->
    {true, Ctx1, _} = add_stmt_tree(Ctx0, "target",
                                    [{container, master,
                                      [{leaf, item, [{type, string}]}]}]),

    Code = [{deviation, <<"/target:master/target:item">>,
             [{deviate, replace, [{type, int32}]}]}],

    {true, Ctx2, _} = add_stmt_tree(Ctx1, "deviating",
                                    [{imports, ["target"]}],
                                    Code),
    Ctx2.
