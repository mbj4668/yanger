%%% File    : xpath_rewrite.erl
%%% Author  :  <thomasl@metis.tail-f.com>
%%% Description :
%%% Created : 24 May 2006 by  <thomasl@metis.tail-f.com>
%%
%% Rewrite xpath queries to internal form. This includes introducing
%% optimized location steps when we can do direct lookups, know the
%% descendants, etc.
%%   We also resolve names of tags and simplify paths as appropriate.
%% For example, if the tag t occurs in a path P but not in the schema,
%% then P will fail (ie, generate no solutions) and can be pruned.

-module(xpath_rewrite).
-export([q/1, q/2,
         prefixes_with_no_default_namespace/1,
         prefixes_with_default_namespace/2,
         preds_contain_position/1,
         pred_contains_last/1,
         get_position_or_last/2,
         rewrite_expr/2,
         find_variables_expr/1, find_variables_expr/2,
         is_node_set/1,
         map_expr/2, fold_expr/3
        ]).

-define(xp_exit(Tag, Rsn), exit({(Tag), ?MODULE, ?LINE, (Rsn)})).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

q(Q) ->
    q(Q, empty_namespace_map()).

q(Q, NS_map) ->
    case rewrite_expr(Q, NS_map) of
        {absolute, _}=Path ->
            Path;
        {relative, _}=Path ->
            Path;
        {union, _}=Paths ->
            Paths;
        Paths when is_list(Paths) ->
            Paths;
        {path, filter, {_,_}}=E ->
            E;
        fail ->
            fail;
        _Other ->
            %% rewritten to fail or to a non-path expression => fail
            ?xp_exit(not_a_path_expression, Q)
    end.

%% The following tasks are required:
%% 1. resolve namespace prefixes into namespaces
%% 2. resolve tags into tag names
%%
%% NB: namespace::foo should NOT try to resolve foo as a tag, but as
%% a namespace (or a namespace prefix?)
%% = which should it be?

rewrite_expr({absolute, Path}, NS_map) ->
    mk_absolute(rewrite_path(Path, NS_map));
rewrite_expr({relative, Path}, NS_map) ->
    mk_relative(rewrite_path(Path, NS_map));
rewrite_expr({union, Paths}, NS_map) ->
    mk_union([ rewrite_expr(Path, NS_map) || Path <- Paths ]);
rewrite_expr({comp, Op, E1, E2}, NS_map) ->
    mk_comp(Op, rewrite_expr(E1, NS_map), rewrite_expr(E2, NS_map));
rewrite_expr({arith, Op, E1, E2}, NS_map) ->
    case {rewrite_expr(E1, NS_map), rewrite_expr(E2, NS_map)} of
        {{number,_} = N1, {number,_} = N2} ->
            {succ, E3} = xpath_eval:arith(Op, N1, N2),
            E3;
        {R1, R2} ->
            {arith, Op, R1, R2}
    end;
rewrite_expr({bool, 'or', E1, E2}, NS_map) ->
    mk_or(rewrite_expr(E1, NS_map), rewrite_expr(E2, NS_map));
rewrite_expr({bool, 'and', E1, E2}, NS_map) ->
    mk_and(rewrite_expr(E1, NS_map), rewrite_expr(E2, NS_map));
rewrite_expr({function_call, 'xp_sort-by', [E1, E2]}, NS_map) ->
    Pred =
        {pred, {comp,'=', {bool, true},
                {function_call,'xp_sort-by', [rewrite_expr(E2, NS_map)]}}},
    insert_sort_order_pred(rewrite_expr(E1, NS_map), Pred);
rewrite_expr({function_call, Fun, [E1, E2]}, NS_map)
  when Fun == 'xp_derived-from' orelse Fun == 'xp_derived-from-or-self' ->
    %% This is unfortunate.  If E2 is a string literal (which it will be
    %% in most cases), we resolve the prefix and call xp_derived-from/3.
    %% If it is not a string literal, then hopefully it is a node-set
    %% with an identity-node; in this case we use the internal representation
    %% to do the right thing.  In all other cases, we give up.  For example:
    %%   derived-from(., concat("x:", name))
    %% in order to implement this correctly, we'd have to keep the prefix map
    %% from the YANG module...
    case rewrite_expr(E2, NS_map) of
        {literal, Str} ->
            case xpath_scan:tokens(Str) of
                [{name, _, {Prefix, LocalName}}, {'$end', _, '$end'}] ->
                    case NS_map of
                        {make_atoms, NS_map0} ->
                            ok;
                        NS_map0 ->
                            ok
                    end,
                    NS = case NS_map0 of
                             {_, 0} when Prefix == [] ->
                                 undefined;
                             {_, DefaultNs} when Prefix == [] ->
                                 DefaultNs;
                             _ ->
                                 prefix_to_namespace(Prefix, NS_map)
                         end,
                    {function_call, Fun,
                     [rewrite_expr(E1, NS_map),
                      {literal, NS},
                      {literal, LocalName}]};
                _ ->
                    fail
            end;
        _E2_RW ->
            ?xp_exit(nyi2,
                     "currently second argument must be a literal string")
            %{function_call, Fun, [rewrite_expr(E1, NS_map), E2_RW]}
    end;
rewrite_expr({function_call, F, Xs}, NS_map) ->
    case F of
        {undefined_function, F_name} ->
            %% was fail
            ?xp_exit(undefined_function, {F_name, length(Xs)});
        _ ->
            %% known function
            case xpath_bif:arity_check(F, length(Xs)) of
                true ->
                    ok;
                false ->
                    %% was run-time error
                    ?xp_exit(invalid_bif_arity,
                             {lists:nthtail(3, atom_to_list(F)), length(Xs)})
            end,
            NewXs = [ rewrite_expr(X, NS_map) || X <- Xs ],
            if
                F == 'not' ->
                    case NewXs of
                        [fail] ->
                            {function_call, xp_true, []};
                        _ ->
                            {function_call, F, NewXs}
                    end;
                true ->
                    {function_call, F, NewXs}
            end
    end;
rewrite_expr({negative, X}, NS_map) ->
    case rewrite_expr(X, NS_map) of
        {number, N} ->
            {number, 0 - N};
        E ->
            {arith, '-', {number, 0}, E}
    end;
rewrite_expr({number, _}=E, _NS_map) ->
    E;
rewrite_expr({literal, _}=E, _NS_map) ->
    E;
rewrite_expr({bool, _}=E, _NS_map) ->
    E;
rewrite_expr({var, _}=E, _NS_map) ->
    E;
rewrite_expr({path_expr, {function_call, {undefined_function, F}, Xs}}, _) ->
    ?xp_exit(undefined_function, {F, length(Xs)});
rewrite_expr({path_expr, E = {function_call, F, _}}, NS_map) ->
    case xpath_bif:bif_type(F) of
        nodeset ->
            %% Make us evaluate this as a path expression
            rewrite_expr([E], NS_map);
        _ ->
            rewrite_expr(E, NS_map)
    end;
rewrite_expr({path_expr, E}, NS_map) ->
    rewrite_expr(E, NS_map);
rewrite_expr([E0|Steps0], NS_map) ->
    case rewrite_path(Steps0, NS_map) of
        fail ->
            fail;
        Steps ->
            case rewrite_expr(E0, NS_map) of
                fail ->
                    fail;
                E ->
                    [E | Steps]
            end
    end;
rewrite_expr({path, filter, {Expr, Pred}}, NS_map) ->
    {path, filter, {rewrite_expr(Expr, NS_map), rewrite_pred(Pred, NS_map)}};
rewrite_expr(Other, _NS_map) ->
    ?xp_exit(internal_error, {unhandled_expr, Other}).

%% position or last
preds_contain_position(Preds) ->
    lists:any(fun({pred, P}) -> pred_contains_position(P) end,
              Preds).

pred_contains_position({absolute, _Path}) ->
    false;
pred_contains_position({relative, _Path}) ->
    false;
pred_contains_position({union, _Paths}) ->
    false;
pred_contains_position({comp, _Op, E1, E2}) ->
    pred_contains_position(E1) or pred_contains_position(E2);
pred_contains_position({arith, _Op, E1, E2}) ->
    pred_contains_position(E1) or pred_contains_position(E2);
pred_contains_position({bool, 'or', E1, E2}) ->
    pred_contains_position(E1) or pred_contains_position(E2);
pred_contains_position({bool, 'and', E1, E2}) ->
    pred_contains_position(E1) or pred_contains_position(E2);
pred_contains_position({function_call, xp_position, []}) ->
    true;
pred_contains_position({function_call, xp_last, []}) ->
    true;
pred_contains_position({function_call, _F, Xs}) ->
    lists:any(fun(X) -> pred_contains_position(X) end,
              Xs);
pred_contains_position({negative, X}) ->
    pred_contains_position(X);
pred_contains_position({number, _}) ->
    false;
pred_contains_position({literal, _}) ->
    false;
pred_contains_position({bool, _}) ->
    false;
pred_contains_position({var, _}) ->
    false;
pred_contains_position([_|_]) ->
    false;
pred_contains_position(_Other) ->
    false.

%% Ret: false | {last,N} | position
%% Where N is the first predicate that contains last().  Returns
%% 'last' only if a predicate after the first one contains last()
get_position_or_last([], _) ->
    false;
get_position_or_last([{pred, Pred}|Preds], _SkipFirstPredIfLast = true) ->
    get_position_or_last1(Preds, 2, get_position(Pred, false));
get_position_or_last(Preds, _SkipFirstPredIfLast = false) ->
    get_position_or_last1(Preds, 1, false).

get_position_or_last1(Preds, Start, Res) ->
    try
        {_, R} =
            lists:foldl(fun({pred, P}, {N, Res0}) ->
                                {N+1, get_position_or_last(P, N, Res0)}
                        end,
                        {Start, Res}, Preds),
        R
    catch
        throw:RR ->
            RR
    end.

pred_contains_last({pred, P}) ->
    try
        get_position_or_last(P, 1, false),
        false
    catch
        throw:_ ->
            true
    end.


get_position({function_call, xp_position, []}, _Res) ->
    position;
get_position({comp, _Op, E1, E2}, Res0) ->
    Res1 = get_position(E1, Res0),
    get_position(E2, Res1);
get_position({arith, _Op, E1, E2}, Res0) ->
    Res1 = get_position(E1, Res0),
    get_position(E2, Res1);
get_position({bool, _Op, E1, E2}, Res0) ->
    Res1 = get_position(E1, Res0),
    get_position(E2, Res1);
get_position({function_call, _F, Xs}, Res0) ->
    lists:foldl(fun(X, R) -> get_position(X, R) end,
                Res0, Xs);
get_position({negative, X}, Res) ->
    get_position(X, Res);
get_position(_, Res) ->
    Res.

get_position_or_last({function_call, xp_last, []}, N, _) ->
    throw({last, N});
get_position_or_last({function_call, xp_position, []}, _, _Res) ->
    position;
get_position_or_last({comp, _Op, E1, E2}, N, Res0) ->
    Res1 = get_position_or_last(E1, N, Res0),
    get_position_or_last(E2, N, Res1);
get_position_or_last({arith, _Op, E1, E2}, N, Res0) ->
    Res1 = get_position_or_last(E1, N, Res0),
    get_position_or_last(E2, N, Res1);
get_position_or_last({bool, _Op, E1, E2}, N, Res0) ->
    Res1 = get_position_or_last(E1, N, Res0),
    get_position_or_last(E2, N, Res1);
get_position_or_last({function_call, _F, Xs}, N, Res0) ->
    lists:foldl(fun(X, R) -> get_position_or_last(X, N, R) end,
                Res0, Xs);
get_position_or_last({negative, X}, N, Res) ->
    get_position_or_last(X, N, Res);
get_position_or_last(_, _, Res) ->
    Res.


%%

mk_comp(_Op, fail, _) ->
    fail;
mk_comp(_Op, _, fail) ->
    fail;

%% rewrite "count(E) != 0" to "boolean(E)" (including variations)
mk_comp('>',  {function_call, xp_count, [E]}, {number, 0}) ->
    mk_fn_boolean(E);
mk_comp('!=', {function_call, xp_count, [E]}, {number, 0}) ->
    mk_fn_boolean(E);
mk_comp('=',  {function_call, xp_count, [E]}, {number, 0}) ->
    mk_fn_not(mk_fn_boolean(E));
mk_comp('>',  {number, 0}, {function_call, xp_count, [E]}) ->
    mk_fn_boolean(E);
mk_comp('!=', {number, 0}, {function_call, xp_count, [E]}) ->
    mk_fn_boolean(E);
mk_comp('=',  {number, 0}, {function_call, xp_count, [E]}) ->
    mk_fn_not(mk_fn_boolean(E));

mk_comp(Op, X, Y) ->
    {comp, Op, X, Y}.

mk_fn_boolean(E) ->
    {function_call, 'xp_nodeset-as-boolean', [E, {literal, "count"}]}.
mk_fn_not(E) ->
    {function_call, xp_not, [E]}.

%%

mk_and(fail, _) ->
    fail;
mk_and(_, fail) ->
    fail;
mk_and(X, Y) ->
    {bool, 'and', X, Y}.

%%

mk_or(fail, Y) ->
    Y;
mk_or(X, fail) ->
    X;
mk_or(X, Y) ->
    {bool, 'or', X, Y}.

%%

mk_absolute(fail) ->
    fail;
mk_absolute(Path) ->
    {absolute, Path}.

%%

mk_relative(fail) ->
    fail;
mk_relative(Path) ->
    {relative, Path}.

%% Note: the paths inside a union should be
%%   {absolute,LocSteps} | {relative, LocSteps}

mk_union([]) ->
    fail;
mk_union([P]) ->
    P;
mk_union(Ps) ->
    case [ P || P <- Ps, P =/= fail] of
        [] ->
            fail;
        [P] ->
            P;
        Paths ->
            {union, Paths}
    end.

%%

rewrite_path(LocSteps, NS_map) ->
    try rewrite_location_steps(LocSteps, NS_map) of
        NewLocSteps ->
            NewLocSteps
    catch
        exit:{loc_step_predicates_fail, _, _, _} ->
            fail;
        exit:{tag_not_in_any_schema, _, _, _} ->
            fail;
        exit:{attribute_does_not_exist, _, _, _} ->
            fail
    end.

%% Rewrite and check the location step.
%%
%% We special-case attribute and namespace axes since their "name tags"
%% aren't ordinary XML element tags (the latter are resolved by tag_of/1).
%%
%% NB: this can fail

maybe_list_to_atom(Str, false) ->
    catch list_to_existing_atom(Str);
maybe_list_to_atom(Str, true) ->
    list_to_atom(Str).

rewrite_location_steps([{step, attribute, Name, Prs}|Steps], NS_map0) ->
    case NS_map0 of
        {make_atoms, NS_map} ->
            Make = true;
        NS_map ->
            Make = false
    end,
    NewName =
        case Name of
            {name, NS_prefix, Tag_str} ->
                case maybe_list_to_atom(Tag_str, Make) of
                    Attr when is_atom(Attr) ->
                        NS = prefix_to_namespace(NS_prefix, NS_map),
                        {name, NS, Attr};
                    _ ->
                        ?xp_exit(attribute_does_not_exist, Tag_str)
                end;
            {name, Attr_str} ->
                case maybe_list_to_atom(Attr_str, Make) of
                    A when is_atom(A) ->
                        {name, A};
                    _ ->
                        ?xp_exit(attribute_does_not_exist, Attr_str)
                end;
            NodeTest ->
                rewrite_node_test(NodeTest, NS_map)
        end,
    case rewrite_preds(Prs, NS_map) of
        fail ->
            ?xp_exit(loc_step_predicates_fail, Prs);
        NewPrs ->
            [{step, attribute, NewName, NewPrs}|
             rewrite_location_steps(Steps, NS_map0)]
    end;
rewrite_location_steps([{step, namespace, Name, Prs}|Steps], NS_map) ->
    NewName =
        case Name of
            {name, NS_prefix, Tag_str} ->
                %% as far as we know, "namespace::ns1:ns2" has no meaning
                %% Answer: experiment with xsltproc indicates that the syntax
                %% is allowed, and ns1 must be a valid prefix but is
                %% otherwise ignored, making it equivalent to namespace::ns2.
                ?xp_exit(namespace_does_not_exist, {name, NS_prefix, Tag_str});
            {name, NS_str} ->
                NS = prefix_to_namespace(NS_str, NS_map),
                {name, NS};
            NodeTest ->
                rewrite_node_test(NodeTest, NS_map)
        end,
    case rewrite_preds(Prs, NS_map) of
        fail ->
            ?xp_exit(loc_step_predicates_fail, Prs);
        NewPrs ->
            [{step, namespace, NewName, NewPrs}|
             rewrite_location_steps(Steps, NS_map)]
    end;
rewrite_location_steps([{step, Axis, {name, NS_prefix, Tag_str}, Prs}|Steps],
                       NS_map) ->
    ensure_axis_permitted(Axis),
    NS = prefix_to_namespace(NS_prefix, NS_map),
    Tag = tag_of(Tag_str),
    NewName = {name, NS, Tag},
    case rewrite_preds(Prs, NS_map) of
        fail ->
            ?xp_exit(loc_step_predicates_fail, Prs);
        NewPrs ->
            [{step, Axis, NewName, NewPrs}|
             rewrite_location_steps(Steps, NS_map)]
    end;
rewrite_location_steps([{step, Axis, {name, Tag_str}, Prs}|Steps], NS_map0) ->
    ensure_axis_permitted(Axis),
    case NS_map0 of
        {make_atoms, NS_map} ->
            Make = true;
        NS_map ->
            Make = false
    end,
    Tag = tag_of(Tag_str),
    NewName = case NS_map of
                  {_, 0} ->
                      {name, Tag};
                  {_, DefaultNs} ->
                      {name, maybe_list_to_atom(DefaultNs, Make), Tag}
              end,
    case rewrite_preds(Prs, NS_map) of
        fail ->
            ?xp_exit(loc_step_predicates_fail, Prs);
        NewPrs ->
            [{step, Axis, NewName, NewPrs}|
             rewrite_location_steps(Steps, NS_map)]
    end;
rewrite_location_steps([{step, descendant_or_self,
                         {node_type, node} = NodeTest, []}|Steps], NS_map) ->
    Prs =
        case Steps of
            [{step,child,NodeTest0,_}|_] when element(1,NodeTest0) == name ->
                NodeTest1 = rewrite_node_test(NodeTest0, NS_map),
                [{pred,{relative,[{step,child,NodeTest1,[]}]}}];
            _ ->
                []
        end,
    [{step, descendant_or_self, NodeTest, Prs}|
     rewrite_location_steps(Steps, NS_map)];
rewrite_location_steps([{step, Axis, NodeTest, Prs}|Steps], NS_map) ->
    ensure_axis_permitted(Axis),
    case rewrite_preds(Prs, NS_map) of
        fail ->
            ?xp_exit(loc_step_predicates_fail, Prs);
        NewPrs ->
            NewNodeTest = rewrite_node_test(NodeTest, NS_map),
            [{step, Axis, NewNodeTest, NewPrs}|
             rewrite_location_steps(Steps, NS_map)]
    end;
rewrite_location_steps([], _NS_map) ->
    [];
rewrite_location_steps([OtherStep|_], _NS_map) ->
    ?xp_exit(internal_error, {unknown_location_step, OtherStep}).

%% The following node tests can appear:
%%   {node_type, Type}         text(), node()    for our subset (text|node)
%%   wildcard                  *
%%   {namespace, NS_lst}       NS:*
%%   {name, NS_lst, Tag}       NS:Tag
%%   {name, Tag}               Tag
%%
%% Note that we can use this function only for element nodes -- for others,
%% the "tag" mapping is different (e.g., attribute names are not stored
%% among the tags).

rewrite_node_test({name, NS_lst, Tag_lst}, NS_map) ->
    NS = prefix_to_namespace(NS_lst, NS_map),
    Tag = tag_of(Tag_lst),
    {name, NS, Tag};
rewrite_node_test({name, Tag_lst}, _NS_map) ->
    Tag = tag_of(Tag_lst),
    {name, Tag};
rewrite_node_test({namespace, NS_lst}, NS_map) ->
    NS = prefix_to_namespace(NS_lst, NS_map),
    {namespace, NS};
rewrite_node_test(wildcard, _NS_map) ->
    wildcard;
rewrite_node_test({node_type, _T}=NodeTest, _NS_map) ->
    NodeTest;
rewrite_node_test(NodeTest, _NS_map) ->
    ?xp_exit(unknown_node_test, NodeTest).

%% If any predicate fails, the location step fails.

rewrite_preds([P|Ps], NS_map) ->
    case rewrite_pred(P, NS_map) of
        fail ->
            fail;
        NewP ->
            case rewrite_preds(Ps, NS_map) of
                fail ->
                    fail;
                NewPs ->
                    [NewP|NewPs]
            end
    end;
rewrite_preds([], _NS_map) ->
    [].

%%

rewrite_pred({pred, P}, NS_map) ->
    case rewrite_expr(P, NS_map) of
        fail ->
            fail;
        {function_call, F, _Args} = NewP ->
            case xpath_bif:bif_type(F) of
                number -> %% mbj: trial-and-error fix
                    {pred, {comp,'=',
                            {function_call,xp_position,[]},
                            NewP}};
                literal ->
                    {pred, {comp,'!=',
                            {literal, ""},
                            NewP}};
                boolean ->
                    {pred, {comp,'=',
                            {bool, true},
                            NewP}};
                _ ->
                    {pred, NewP}
            end;
        {number,_N} = NewP -> %% mbj: trial-and-error fix
            {pred, {comp,'=',
                    {function_call,xp_position,[]},
                    NewP}};
        {arith,_,_,_} = Expr -> %% mbj: trial-and-error fix
            {pred, {comp,'=',
                    {function_call,xp_position,[]},
                    Expr}};
        NewP ->
            {pred, NewP}
    end.

%% Check that the axes used in the expression are supported.
%% Note: this error is caught and converted into failure, 'fail'.

ensure_axis_permitted(following) ->
    ?xp_exit(axis_not_permitted, "following");
ensure_axis_permitted(preceding) ->
    ?xp_exit(axis_not_permitted, "preceding");
ensure_axis_permitted(following_sibling) ->
    ?xp_exit(axis_not_permitted, "following-sibling");
ensure_axis_permitted(preceding_sibling) ->
    ?xp_exit(axis_not_permitted, "preceding-sibling");
ensure_axis_permitted(_Axis) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Note that we consult all the available namespaces. In principle
%% we could narrow this down by looking at 'nearby' tags. (However,
%% when ancestor is used, we can in principle switch to another namespace
%% at the root.)

tag_of(Tag_str) ->
    %% FIXME confd_server is *not* a good test, what we want is "are
    %% we running confdc or confd"
    case whereis(confd_server) of
        undefined ->
            %% Assume we are compiling, anything goes
            list_to_atom(Tag_str);
        _ ->
            case catch list_to_existing_atom(Tag_str) of
                {'EXIT', _} ->
                    ?xp_exit(tag_not_in_any_schema, Tag_str);
                Tag ->
                    case catch cs:tag2htag(Tag) of
                        {'EXIT', _} ->
                            ?xp_exit(tag_not_in_any_schema, Tag_str);
                        _ ->
                            Tag
                    end
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_sort_order_pred(false = E, _Pred) ->
    E;
insert_sort_order_pred({absolute, Path}, Pred) ->
    {absolute, isop_path(Path, Pred)};
insert_sort_order_pred({relative, Path}, Pred) ->
    {relative, isop_path(Path, Pred)};
insert_sort_order_pred({union, Paths}, Pred) ->
    {union, [insert_sort_order_pred(Path, Pred) || Path <- Paths]};
insert_sort_order_pred({path, filter, _} = E, Pred) ->
    %% XXX This can't be right...
    {path, filter, E, Pred};
insert_sort_order_pred([Expr|Steps], Pred) ->
    [Expr|isop_path(Steps, Pred)];
insert_sort_order_pred(_Expr, _Pred) ->
    ?xp_exit(invalid_sort_order_expr, "").

isop_path([{step, Axis, NodeTest, Preds}], Pred) ->
    [{step, Axis, NodeTest, [Pred|Preds]}];
isop_path([Step|Steps], Pred) ->
    [Step|isop_path(Steps, Pred)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The following provides the functions for resolving namespace
%% prefixes "foo" into actual namespaces 'http://tail-f.com/test/dhcp/1.0'

prefix_to_namespace(Prefix, NS_map0) ->
    case NS_map0 of
        {make_atoms, NS_map} ->
            Make = true;
        NS_map ->
            Make = false
    end,
    {NS_prefixes, _Dflt} = NS_map,
    case lists:keysearch(Prefix, 1, NS_prefixes) of
        {value, {_, NS_lst}} ->
            case maybe_list_to_atom(NS_lst, Make) of
                NS when is_atom(NS) ->
%                   case lists:member(NS, cs_server:get_exported_namespaces(
%                                           netconf)) of
%                       true ->
%                           NS;
%                       false ->
%                           ?xp_exit(invalid_namespace, NS)
%                   end;
%% FIXME: is this correct??
                    NS;
                _ ->
                    ?xp_exit(invalid_namespace, NS_lst)
            end;
        _ ->
            ?xp_exit(invalid_namespace_prefix, Prefix)
    end.

-ifdef(not_used).
has_default_namespace({_, 0}) ->
    false;
has_default_namespace({_, A}) when list(A) ->
    %% otherwise
    {true, A}.
-endif.

prefixes_with_default_namespace(NS_prefixes, Dflt) when is_list(Dflt) ->
    {NS_prefixes, Dflt}.

prefixes_with_no_default_namespace(NS_prefixes) ->
    No_dflt = 0,
    {NS_prefixes, No_dflt}.

empty_namespace_map() ->
    prefixes_with_no_default_namespace([]).


%% Find variables in expressions
find_variables_expr(E) ->
    lists:usort(find_variables_expr(E, [])).

find_variables_expr({absolute, Path}, Acc) ->
    find_variables_path(Path, Acc);
find_variables_expr({relative, Path}, Acc) ->
    find_variables_path(Path, Acc);
find_variables_expr({union, Paths}, Acc) ->
    lists:foldl(fun find_variables_expr/2, Acc, Paths);
find_variables_expr({comp, _Op, E1, E2}, Acc) ->
    find_variables_expr(E2, find_variables_expr(E1, Acc));
find_variables_expr({arith, _Op, E1, E2}, Acc) ->
    find_variables_expr(E2, find_variables_expr(E1, Acc));
find_variables_expr({bool, 'or', E1, E2}, Acc) ->
    find_variables_expr(E2, find_variables_expr(E1, Acc));
find_variables_expr({bool, 'and', E1, E2}, Acc) ->
    find_variables_expr(E2, find_variables_expr(E1, Acc));
find_variables_expr({function_call, _F, Xs}, Acc) ->
    lists:foldl(fun find_variables_expr/2, Acc, Xs);
find_variables_expr({negative, X}, Acc) ->
    find_variables_expr(X, Acc);
find_variables_expr({number, _}, Acc) ->
    Acc;
find_variables_expr({literal, _}, Acc) ->
    Acc;
find_variables_expr({bool, _}, Acc) ->
    Acc;
find_variables_expr({var, VariableName}, Acc) ->
    [list_to_binary(VariableName)|Acc];
find_variables_expr({path_expr, {function_call, {undefined_function, _F}, Xs}},
                    Acc) ->
    lists:foldl(fun find_variables_expr/2, Acc, Xs);
find_variables_expr({path_expr, {function_call, _F, Xs}}, Acc) ->
    lists:foldl(fun find_variables_expr/2, Acc, Xs);
find_variables_expr({path_expr, E}, Acc) ->
    find_variables_expr(E, Acc);
find_variables_expr([E0|Steps0], Acc) ->
    find_variables_expr(E0, find_variables_path(Steps0, Acc));
find_variables_expr({path, filter, {Expr, Pred}}, Acc) ->
    find_variables_pred(Pred, find_variables_expr(Expr, Acc));
find_variables_expr(_, Acc) ->
    Acc.

%% Find variables in Preds
%%
find_variables_preds([], Acc) ->
    Acc;
find_variables_preds([P|Ps], Acc) ->
    find_variables_preds(Ps, find_variables_pred(P, Acc)).

find_variables_pred({pred, P}, Acc) ->
    find_variables_expr(P, Acc).

%% Find variables in paths
%%
find_variables_path([], Acc) ->
    Acc;
find_variables_path([{step, _, _, Preds}|Steps], Acc) ->
    find_variables_path(Steps, find_variables_preds(Preds, Acc)).

is_node_set([{step, _, _, _}|_]) ->
    true;
is_node_set({absolute, _Path}) ->
    true;
is_node_set({relative, _Path}) ->
    true;
is_node_set({union, _Paths}) ->
    true;
is_node_set({comp, _Op, _E1, _E2}) ->
    false;
is_node_set({arith, _Op, _E1, _E2}) ->
    false;
is_node_set({bool, _Op, _E1, _E2}) ->
    false;
is_node_set({function_call, F, _Xs}) ->
    case xpath_bif:bif_type(F) of
        nodeset ->
            true;
        _ ->
            false
    end;
is_node_set({negative, _X}) ->
    false;
is_node_set({number, _}) ->
    false;
is_node_set({literal, _}) ->
    false;
is_node_set({bool, _}) ->
    false;
is_node_set({var, _VariableName}) ->
    false;
is_node_set({path_expr, _}) ->
    true;
is_node_set([E0|_Steps0]) ->
    is_node_set(E0);
is_node_set({path, filter, _}) ->
    true;
is_node_set(_) ->
    false.

map_expr(F, Expr) ->
    case Expr of
        {absolute, Path} ->
            F({absolute, map_path(F, Path)});
        {relative, Path} ->
            F({relative, map_path(F, Path)});
        {union, UnionExprs} ->
            F({union, [map_expr(F, UnionExpr) || UnionExpr <- UnionExprs]});
        {comp, Op, E1, E2} ->
            F({comp, Op, map_expr(F, E1), map_expr(F, E2)});
        {arith, Op, E1, E2} ->
            F({arith, Op, map_expr(F, E1), map_expr(F, E2)});
        {bool, Op, E1, E2} ->
            F({bool, Op, map_expr(F, E1), map_expr(F, E2)});
        {function_call, Func, Args} ->
            F({function_call, Func, [map_expr(F, Arg) || Arg <- Args]});
        {negative, E} ->
            F({negative, map_expr(F, E)});
        {path_expr, E} ->
            F({path_expr, map_expr(F, E)});
        {path, filter, {E, Pred}} ->
            F({path, filter, {map_expr(F, E), map_expr(F, Pred)}});
        [E | Steps] ->
            [map_expr(F, E) | map_path(F, Steps)];
        _ -> % literal, number, ...
            F(Expr)
    end.

map_path(F, [{step, Axis, NodeTest, Preds} | T]) ->
    [F({step, Axis, NodeTest, map_preds(F, Preds)}) | map_path(F, T)];
map_path(_, []) ->
    [].

map_preds(F, [{pred, E} | T]) ->
    [{pred, map_expr(F, E)} | map_preds(F, T)];
map_preds(F, [H | T]) -> % can be 'fail'
    [H | map_preds(F, T)];
map_preds(_, []) ->
    [].

%% F :: fun(Expr, Acc) -> Acc'
fold_expr(F, Acc, Expr) ->
    FoldExprF = fun(Expr1, Acc1) -> fold_expr(F, Acc1, Expr1) end,
    case Expr of
        {absolute, Path} ->
            F(Expr, fold_path(F, Acc, Path));
        {relative, Path} ->
            F(Expr, fold_path(F, Acc, Path));
        {union, UnionExprs} ->
            F(Expr, lists:foldl(FoldExprF, Acc, UnionExprs));
        {comp, _Op, E1, E2} ->
            F(Expr, fold_expr(F, fold_expr(F, Acc, E1), E2));
        {arith, _Op, E1, E2} ->
            F(Expr, fold_expr(F, fold_expr(F, Acc, E1), E2));
        {bool, _Op, E1, E2} ->
            F(Expr, fold_expr(F, fold_expr(F, Acc, E1), E2));
        {function_call, _Func, Args} ->
            F(Expr, lists:foldl(FoldExprF, Acc, Args));
        {negative, E} ->
            F(Expr, fold_expr(F, Acc, E));
        {path_expr, E} ->
            F(Expr, fold_expr(F, Acc, E));
        {path, filter, {E, Pred}} ->
            F(Expr, fold_expr(F, fold_expr(F, Acc, Pred), E));
        [E | Steps] ->
            fold_expr(F, fold_path(F, Acc, Steps), E);
        _ -> % literal, number, ...
            F(Expr, Acc)
    end.

fold_path(F, Acc, [{step, _Axis, _NodeTest, Preds} = H | T]) ->
    fold_path(F, F(H, fold_preds(F, Acc, Preds)), T);
fold_path(_, Acc, []) ->
    Acc.

fold_preds(F, Acc, [{pred, E} | T]) ->
    fold_preds(F, fold_expr(F, Acc, E), T);
fold_preds(F, Acc, [_H | T]) -> % can be 'fail'
    fold_preds(F, Acc, T);
fold_preds(_, Acc, []) ->
    Acc.

