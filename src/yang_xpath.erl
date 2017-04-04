-module(yang_xpath).

-export([mk_ns_map/1,
         compile/2, compile/5,
         set_default_namespace/2,
         get_dep_paths/2, v_dep_path/6]).

-ifdef(debug).
-export([prefixes/1]).
-endif.

-include_lib("yanger/include/yang.hrl").

%%% XPath functions.
%%% Currently uses Tail-f XPath code.  Could use xmerl instead.

%% NOTE: we map prefix to modulename, not to the module's namespace.  The
%% reason for this is that we always want to check these expressions towards
%% modules, not their namespaces, and the modulename is always locally present,
%% for example in a submodule.  If we used the namespace, we'd have to find
%% the module, parse it, and use its namespace.  Using the modulename also
%% removes one level of indirection.
mk_ns_map(#module{prefix_map = PrefixMap, modulename = Ns} = M) ->
    NsList = mk_ns_list(yang:map_to_list(PrefixMap), M),
    make_prefix_map(NsList, Ns).

mk_ns_list([{Prefix, '$self'} | T], M) ->
    [{?a2l(Prefix), ?a2l(M#module.modulename)} | mk_ns_list(T, M)];
mk_ns_list([{Prefix, ModuleName} | T], M) ->
    [{?a2l(Prefix), ?a2l(ModuleName)} | mk_ns_list(T, M)];
mk_ns_list([], _) ->
    [].

-spec make_prefix_map(PrefixNsList :: [{Prefix :: atom(), Ns :: atom()}],
                      DefaultNs :: atom()) ->
        PrefixNsMap :: any().
%% Called once per module.  Build some structure for
%% prefix mapping.  This will be passed to xpath_compile() and
%% xpath_set_default_namespace() later.
%% NOTE: We use the modulename instead of the namespace URI in the map.
make_prefix_map(PrefixNsList, DefaultNs) ->
    {{make_atoms,
      xpath_rewrite:prefixes_with_no_default_namespace(PrefixNsList)},
     DefaultNs}.

-spec compile(Str :: binary(), PrefixNsMap :: any()) ->
        {ok, Expr :: any()}
      | {error, ErrStr :: list()}.
%% Called when an XPath expression is first defined.  Should detect
%% unknown prefixes, and when unknown functions are
%% called.
%% This function must not bind un-prefixed names to the default namespace.
compile(Str, {PrefixNsMap, _}) ->
    case xpath_compile(?b2l(Str), PrefixNsMap) of
        {ok, CompiledXPath} ->
            {ok, CompiledXPath};
        {error, {Mod, Err}} ->
            {error, Mod:strerror(Err)}
    end.

-spec compile(Str :: binary(), yang:pos(), #module{},
              Strict :: boolean(), #yctx{}) ->
        {ok, Expr :: any(), #yctx{}}
      | {error, #yctx{}}.
%% Called when an XPath expression is first defined.  Should detect
%% unknown prefixes, and when unknown functions are called.
%% This function must not bind un-prefixed names to the default namespace.
compile(Str, Pos, M, Strict, Ctx0) ->
    #module{name = Name, xpath_ns_map = {PrefixNsMap, _},
            yang_version = YangVersion} = M,
    case xpath_compile0(?b2l(Str)) of
        {ok, Q} ->
            Prefixes = xpath_rewrite:fold_expr(fun prefix/2, [], Q),
            Ctx2 = lists:foldl(
                     fun(P, Ctx1) ->
                             yang:mark_import_as_used(Name, P, Ctx1)
                     end, Ctx0, Prefixes),
            case xpath_compile1(Q, PrefixNsMap) of
                {ok, CompiledXPath} ->
                    if Strict ->
                            try
                                chk_strict(CompiledXPath, YangVersion),
                                {ok, CompiledXPath, Ctx2}
                            catch
                                throw:{illegal_function, Fun, Arity} ->
                                    ErrStr = illegal_function(Fun, Arity),
                                    {error, add_error(Ctx2, Pos,
                                                      'YANG_ERR_XPATH',
                                                      [ErrStr])}
                            end;
                       true ->
                            {ok, CompiledXPath, Ctx2}
                    end;
                {error, {Mod, Err}} ->
                    {error, add_error(Ctx2, Pos, 'YANG_ERR_XPATH',
                                      [Mod:strerror(Err)])}
            end;
        {error, {Mod, Err}} ->
            {error, add_error(Ctx0, Pos, 'YANG_ERR_XPATH',
                              [Mod:strerror(Err)])}
    end.

add_error(Ctx, Pos, ErrCode, Args) ->
    yang_error:add_error(Ctx, Pos, ErrCode, Args).

illegal_function(Fun, Arity) ->
    io_lib:format("function ~p/~p is illegal in strict YANG",
                  [Fun, Arity]).

prefix({step, child, {name, Prefix, _}, _}, Prefixes) ->
    add_to_list(?l2a(Prefix), Prefixes);
prefix(_, Prefixes) ->
    Prefixes.

add_to_list(X, [X | _] = L) ->
    L;
add_to_list(X, [H | T]) ->
    [H | add_to_list(X, T)];
add_to_list(X, []) ->
    [X].


-define(err(Rsn), {xpath_error, (Rsn)}).

xpath_compile(String, NS_map) ->
    case xpath_compile0(String) of
        {ok, AbsQuery} ->
            xpath_compile1(AbsQuery, NS_map);
        Else ->
            Else
    end.

%% @doc Only first step of compile (no rewriting).
xpath_compile0(String) ->
    try xpath_scan:tokens(String) of
        Tokens ->
            case xpath_parse:parse(Tokens) of
                {ok, Q} ->
                    {ok, Q};
                {error, Err} ->
                    {error, ?err(Err)}
            end
    catch
        _:Err ->
            {error, ?err({xpath_scan, Err})}
    end.

%% @doc Second step of compile (rewriting)
xpath_compile1(AbsQuery, NS_map) ->
    try
        {ok, xpath_rewrite:rewrite_expr(AbsQuery, NS_map)}
    catch
        exit:Rsn ->
            {error, ?err(Rsn)}
    end.



-spec set_default_namespace(Expr :: any(), PrefixNsMap :: any()) ->
        Expr1 :: any().
%% Called when an XPath expression is finally used.
set_default_namespace(Expr, {_, DefaultNs}) ->
    F = fun({step, child, {name, Name}, Preds}) ->
                {step, child, {name, DefaultNs, Name}, Preds};
           (E) ->
                E
        end,
    xpath_rewrite:map_expr(F, Expr).


chk_strict(Expr, YangVersion) ->
    F = fun({function_call, Func, Args} = E) ->
                case is_illegal(Func, YangVersion) of
                    {true, Name} ->
                        throw({illegal_function, Name, length(Args)});
                    _ ->
                        E
                end;
           (E) ->
                E
        end,
    xpath_rewrite:map_expr(F, Expr).

%% Ugly.  We should rewrite xpath to not use xp_, and then check
%% for the core functions (like xmerl does).
is_illegal('xp_deref', '1') -> {true, 'deref'};
is_illegal('xp_re-match', '1') -> {true, 're-match'};
is_illegal('xp_derived-from', '1') -> {true, 'derived-from'};
is_illegal('xp_derived-from-or-self', '1') -> {true, 'derived-from-or-self'};
is_illegal('xp_enum-value', '1') -> {true, 'enum-value'};
is_illegal('xp_bit-is-set', '1') -> {true, 'bit-is-set'};
is_illegal('xp_string-compare', _) -> {true, 'string-compare'};
is_illegal('xp_compare', _) -> {true, 'compare'};
is_illegal('xp_min', _) -> {true, 'min'};
is_illegal('xp_max', _) -> {true, 'max'};
is_illegal('xp_avg', _) -> {true, 'avg'};
is_illegal('xp_band', _) -> {true, 'band'};
is_illegal('xp_bor', _) -> {true, 'bor'};
is_illegal('xp_bxor', _) -> {true, 'bxor'};
is_illegal('xp_bnot', _) -> {true, 'bnot'};
is_illegal('xp_sort-by', _) -> {true, 'sort-by'};
is_illegal(_, _) -> false.


get_dep_paths(XPathExpr, CurNs) ->
    try
        %% FIXME: we always add '.' here - this is probably not correct.
        %% if the XPath expression reads '.' somehow, we'll figure it out
        %% anyway.
        {_, Deps} = deps(XPathExpr, ['.'], CurNs, []),
        [normalize_path(Dep) || Dep <- Deps]
    catch
        throw:false ->
            false;
        _:Error ->
            %% FIXME: report if verbose?
            ?liof("internal error: ~p\n~p\n", [Error, erlang:get_stacktrace()]),
            false
    end.

deps(Expr, CurPath, CurNs, DepsAcc) ->
    RecF =
        fun(Exprs) ->
                {CurPath,
                 lists:foldl(fun(E, DepsAcc1) ->
                                     {_, DepsAcc2} =
                                         deps(E, CurPath, CurNs, DepsAcc1),
                                     DepsAcc2
                             end, DepsAcc, Exprs)}
        end,
    case Expr of
        {absolute, Path} ->
            {CurPath, deps_path(Path, [], CurNs, DepsAcc)};
        {relative, Path} ->
            {CurPath, deps_path(Path, CurPath, CurNs, DepsAcc)};
        {union, PathExprs} ->
            RecF(PathExprs);
        {comp, _Op, E1, E2} ->
            RecF([E1, E2]);
        {arith, _Op, E1, E2} ->
            RecF([E1, E2]);
        {bool, _Op, E1, E2} ->
            RecF([E1, E2]);
        %% NOTE: any function that returns a node-set must be treated
        %% specially
        {function_call, xp_current, []} ->
            {['.'], DepsAcc};
        [{function_call, xp_deref, Args} | _Steps] ->
            %% We can't check the _Steps after deref without following
            %% the deref target itself.  If it is an i-i we can't do it at
            %% all; it is is a leafref we _could_ do it.
            RecF(Args);
        {function_call, _F, Args} ->
            RecF(Args);
        {negative, E} ->
            deps(E, CurPath, CurNs, DepsAcc);
        {path_expr, E} ->
            deps(E, CurPath, CurNs, DepsAcc);
        {path, filter, {E, Pred}} ->
            RecF([E, Pred]);
        [E | Steps] ->
            {CurPath1, DepsAcc1} = deps(E, CurPath, CurNs, DepsAcc),
            {CurPath, deps_path(Steps, CurPath1, CurNs, DepsAcc1)};
        _ -> % literal, number, ...
            {CurPath, DepsAcc}
    end.

deps_path([Step | T], CurPath, CurNs, DepsAcc) ->
    case Step of
        {step, Axis, NodeTest, Preds} ->
            CurPath1 =
                case {Axis, NodeTest} of
                    {parent, {node_type, node}} ->
                        Ns = '$undefined', % force ns change after ..
                        if CurPath == ['.'] ->
                                ['..'];
                           true ->
                                ['..' | CurPath]
                        end;
                    {self, {node_type, node}} ->
                        Ns = CurNs,
                        CurPath;
                    {child, {name, CurNs, Tag}} when CurPath /= [] ->
                        Ns = CurNs,
                        [Tag | CurPath];
                    {child, {name, Tag}} when CurPath == [] ->
                        Ns = CurNs,
                        [[CurNs | Tag]];
                    {child, {name, Tag}} ->
                        Ns = CurNs,
                        [Tag | CurPath];
                    {child, {name, Ns, Tag}} ->
                        [[Ns | Tag] | CurPath];
                    _X ->
                        Ns = undefined,
                        %?liof("deps_path: unhandled axis and node test: ~p\n",
                        %      [_X]),
                        throw(false)
                end,
            DepsAcc1 =
                lists:foldl(
                  fun({pred, P}, DepsAcc_0) ->
                          {_, DepsAcc_1} = deps(P, CurPath1, Ns, DepsAcc_0),
                          DepsAcc_1
                  end, DepsAcc, Preds),
            deps_path(T, CurPath1, Ns, DepsAcc1);
        _X ->
            %?liof("deps_path: unhandled step: ~p\n", [_X]),
            throw(false)
    end;
deps_path([], CurPath, _CurNs, DepsAcc) ->
    Dep =
        case lists:last(CurPath) of
            Last when Last == '.'; Last == '..' ->
                %% relative path
                lists:reverse(CurPath);
            _ ->
                %% absolute path
                CurPath
        end,
    [Dep | DepsAcc].

normalize_path(P) ->
    normalize_path(P, is_dep_path_absolute(P), []).

normalize_path([X, '..', Y | T], true, Acc) when Y /= '..' ->
    X1 = case X of
             [_Ns|_] ->
                 X;
             '..' ->
                 X;
             _ ->
                 case Y of
                     [Ns|_] ->
                         [Ns|X];
                     _ ->
                         X
                 end
         end,
    normalize_path(lists:reverse(Acc, [X1 | T]));
normalize_path([X, '..', Y | T], false, Acc) when X /= '..' ->
    Y1 = case Y of
             [_Ns|_] ->
                 Y;
             '..' ->
                 '..';
             _ ->
                 case X of
                     [Ns|_] ->
                         [Ns|Y];
                     _ ->
                         Y
                 end
         end,
    normalize_path(lists:reverse(Acc, [Y1 | T]));
normalize_path([H | T], IsAbs, Acc) ->
    normalize_path(T, IsAbs, [H | Acc]);
normalize_path([], _, Acc) ->
    lists:reverse(Acc).

%% Validates a dependency path
%%
%% Note: Returns only a list of errors, this has to be
%% appended to the yctx that is used when calling this function
v_dep_path(Dep, {_, _, Pos, _}, Ctx, Sn, M, Ancestors) ->
    Path = dep_path_to_cursor_path(Dep, Ctx),
    InitCursor =
        case is_dep_path_absolute(Dep) of
            true  -> yang:mk_cursor(undefined, [], Pos, M, data);
            false -> yang:mk_cursor(Sn, Ancestors, Pos, M, data)
        end,
    case follow_dep_path(Path, InitCursor, Ctx#yctx{errors = []}) of
        true ->
            true;
        {false, #yctx{errors = Errors}} ->
            {false, Errors}
    end.

dep_path_to_cursor_path(DepPath, _Ctx) ->
    case is_dep_path_absolute(DepPath) of
        true ->
            dep_path_to_cursor_path0(lists:reverse(DepPath), undefined);
        false ->
            dep_path_to_cursor_path0(DepPath, undefined)
    end.

dep_path_to_cursor_path0(['..' | T], _) ->
    [parent | dep_path_to_cursor_path0(T, undefined)];
dep_path_to_cursor_path0(['.' | T], CurModName) ->
    dep_path_to_cursor_path0(T, CurModName);
dep_path_to_cursor_path0([[ModName|Name] | T], _) ->
    [{child, {ModName, Name}} |
     dep_path_to_cursor_path0(T, ModName)];
dep_path_to_cursor_path0([Name | T], undefined) ->
    [{child, Name} | dep_path_to_cursor_path0(T, undefined)];
dep_path_to_cursor_path0([Name | T], CurModName) ->
    [{child, {CurModName, Name}} | dep_path_to_cursor_path0(T, CurModName)];
dep_path_to_cursor_path0([], _) ->
    [].

%% Follow the dep path and check that it exists.
follow_dep_path(Path, InitCursor, Ctx) ->
    case yang:cursor_follow_path(Path, InitCursor, Ctx) of
        {true, _} ->
            true;
        {false, ErrCtx} ->
            {false, ErrCtx}
    end.

is_dep_path_absolute(['..' | _]) -> false;
is_dep_path_absolute(['.' | _]) -> false;
is_dep_path_absolute(_) -> true.


-ifdef(debug).
prefixes(Str) ->
    case xpath_compile0(Str) of
        {ok, Expr} ->
            {ok, xpath_rewrite:fold_expr(fun prefix/2, [], Expr)};
        Error ->
            Error
    end.
-endif.
