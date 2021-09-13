%%%----------------------------------------------------------------%%%
%%% @doc Yanger Yin Plugin                                         %%%
%%% @author Lisa Bevemyr, lbevemyr@cisco.com (summer intern 2021)  %%%
%%% Prints YIN from a YANG model according to the rules in RFC     %%%
%%% 7950. Inspiration for this plugin came from a the similar      %%%
%%% plugin for Pyang and the YANG plugin for Yanger.               %%%
%%%----------------------------------------------------------------%%%

-module(yanger_yin).
-behaviour(yanger_plugin).

-export([init/1]).

-include_lib("yanger/include/yang.hrl").

-spec help() -> binary().
help() ->
    <<"Translates a YANG module to YIN \n\n"
      "The following options can be used to modify the output:\n\n"
      "  --yin-canonical       Will sort the output in canonical order.\n"
      "  --yin-pretty-strings  Texts will be printed on multiple lines.\n"
    >>.
init(Ctx0) ->
    Ctx1 = yanger_plugin:register_output_format(Ctx0, yin, fun emit/3),
    Ctx2 = yanger_plugin:register_hook(
             Ctx1, #hooks.post_init_ctx, fun post_init_ctx/1),
    Ctx3 = yanger_plugin:register_option_specs(Ctx2, opts()),
    Ctx3.

opts() ->
    [{"YANG output specific options:",
      [{yin_canonical, undefined, "yin-canonical", {boolean, false},
        "Print in canonical order"},
       {yin_comment, undefined, "yin-pretty-strings", {boolean, false},
        "Pretty print strings"},
       {yin_help, undefined, "yin-help", boolean,
        "Prints help on YIN plugin"}
      ]}].

-record(state, {module,
                module_prefixes,
                outf,
                canonical = false,
                pretty_strings = false,
                columns = 80,
                rps,
                yin_map,
                ctx,
                order_map = #{}
               }).

post_init_ctx(Ctx) ->
    case proplists:get_value(yin_help, Ctx#yctx.options, false) of
        true ->
            io:put_chars(help()),
            halt();
        false ->
            ok
    end,
    Ctx.

-spec emit(Ctx::#yctx{}, [Modules::#module{}], Fd::io:device()) -> [].
emit(Ctx, Modules, Fd) ->
    OutF = fun (Str) -> file:write(Fd, Str) end,
    State =
        #state{ outf = OutF,
                canonical =
                    proplists:get_value(yin_canonical, Ctx#yctx.options),
                pretty_strings =
                    proplists:get_value(yin_comment, Ctx#yctx.options),
                rps = [{binary:compile_pattern(RP), RWith} ||
                          {RP, RWith} <- replace_patterns()],
                yin_map = get_yin_map(),
                ctx = Ctx
              },
    lists:foreach(
      fun (M) ->
              ModuleMap =
                  yang:map_foldl(fun (Px, '$self', Map0) ->
                                         Mod = M#module.name,
                                         yang:map_insert(Mod, Px, Map0);
                                     (Px, Mod, Map0) ->
                                         yang:map_update(Mod, Px, Map0)
                                 end,
                                 yang:map_new(),
                                 M#module.prefix_map),
              State1 = State#state{
                         module = M,
                         module_prefixes = ModuleMap
                        },
              emit_module(M, State1)
      end, Modules),
    %% If this format plugin will need to produce warnings or errors
    %% in the future, these warnings and errors need to be returned here.
    Errors = [],
    Errors.

emit_module(M, State) ->
    OutF = State#state.outf,
    OutF(["<?xml version=\"1.0\" encoding=\"UTF-8\"?>", nl()]),
    emit_stmts([M#module.stmt], 0, State).

emit_stmts([], _Lvl, _S) ->
    ok;
emit_stmts([Stmt|Stmts], Lvl, S) ->
    OutF = S#state.outf,
    Ctx = S#state.ctx,
    Keyword = yang:stmt_keyword(Stmt),
    emit_stmt_keyword(Keyword, Lvl, S),
    YinElement = emit_stmt_arg(yang:stmt_arg(Stmt), Lvl, S, Keyword),
    module_arg(Lvl, S, Keyword),
    case yang:stmt_substmts(Stmt) of
        [] when not YinElement ->
            NewS = S,
            OutF([fs(),ct(),nl()]);
        SubStmts0 ->
            OutF([ct(),nl()]),
            {NewS, SubStmts} =
                if S#state.canonical ->
                        sort_canonical(Keyword, SubStmts0, S);
                   true ->
                        {S, SubStmts0}
                end,
            emit_stmts(SubStmts, Lvl+1, NewS),
            KeywordStr = case is_tuple(Keyword) of
                true ->
                    {ModuleName, Key} = Keyword,
                    Prefix  = search_for_prefix(ModuleName,Ctx),
                    make_str({Prefix,Key});
                _ ->
                    make_str(Keyword)
            end,
            OutF([indent(Lvl),ot(),fs(),KeywordStr,ct(),nl()])
    end,
    emit_stmts(Stmts, Lvl, NewS).

emit_stmt_keyword(Kwd, Lvl, S) ->
    OutF = S#state.outf,
    OutF(indent(Lvl)),
    case Kwd of
        {Module, Keyword} ->
            Prefix = yang:map_get(Module, S#state.module_prefixes),
            OutF([ot(), make_str({Prefix, Keyword})]);
        Keyword ->
            OutF([ot(), make_str(Keyword)])
    end.

emit_stmt_arg([], _, _, _) ->
    false;
emit_stmt_arg(Arg, Lvl, S, Keyword) ->
    OutF = S#state.outf,
    ArgStr = make_str(Arg),
    Ctx = S#state.ctx,
    if is_tuple(Keyword) ->
            {Type, YinElement} = search_for_extension(Keyword, Ctx),
            if YinElement ->
                    {ModuleName, _} = Keyword,
                    Prefix  = search_for_prefix(ModuleName,Ctx),
                    TypeStr = make_str({Prefix, Type});
               true ->
                    TypeStr = make_str(Type)
            end;
       true ->
            {Type, YinElement} = maps:get(Keyword, S#state.yin_map),
            TypeStr = make_str(Type)
    end,
    case YinElement of
        true ->
            Indent = indent(Lvl+1),
            Strs = binary:split(
                     ArgStr, binary:compile_pattern(<<"\n">>), [global]),
            Acc = print_yin_element(Indent, TypeStr, Strs, S),
            OutF(Acc);
        false ->
            OutF([s(), TypeStr, $=, qm(),
                  escape(ArgStr, S#state.rps), qm()])
    end,
    YinElement.

search_for_extension({Mod, Keyword}, Ctx) ->
    {_, TargetMod} = yang:get_module(Mod, undefined, Ctx),
    ExtensionMap = yang:map_get(Keyword, TargetMod#module.extensions),
    {extension, Keyword, _Pos, Substmt} = ExtensionMap#extension.stmt,
    case yang:search_one_stmt('argument', Substmt) of
        {argument,Type,_,ArgSubstmt} ->
            case yang:search_one_stmt('yin-element', ArgSubstmt) of
                {'yin-element', Bool,_,_} ->
                    {Type, Bool};
                false ->
                    {Type, false}
            end;
        false ->
            {undefined, false}
    end.

print_yin_element(Indent, TypeStr, [Str1|Strs], S) ->
    Pretty = S#state.pretty_strings,
    case Pretty of
        true ->
            Acc = [ct(),nl(), Indent, ot(), TypeStr, ct(),nl(), Indent, s(),s(),
                   escape(Str1, S#state.rps)];
        false ->
            Acc = [ct(), nl(), Indent, ot(), TypeStr, ct(),
                   escape(Str1, S#state.rps)]
    end,
    print_yin_element1(Indent, TypeStr, Strs, S, Pretty, Acc).

print_yin_element1(Indent, TypeStr, [], _S, true = _Pretty, Acc) ->
    [Acc, nl(), Indent, ot(), fs(), TypeStr];

print_yin_element1(_Indent, TypeStr, [], _S, false = _Pretty, Acc) ->
    [Acc, ot(), fs(), TypeStr];

print_yin_element1(Indent, TypeStr, [Str1|Strs], S, Pretty, Acc) ->
    Acc1 = [Acc, nl(), escape(Str1, S#state.rps)],
    print_yin_element1(Indent, TypeStr, Strs, S, Pretty, Acc1).

module_arg(Lvl, S, Module) when Module == module orelse Module == submodule ->
    Mod = S#state.module,
    Ctx = S#state.ctx,
    OutF = S#state.outf,
    Indent = case Module of
                 module -> indent(Lvl+4);
                 submodule -> indent(Lvl+5,1)
             end,
    OutF([nl(), Indent, "xmlns=\"urn:ietf:params:xml:ns:yang:yin:1\""]),
    Namespace = search_for_namespace(Mod#module.namespace, Mod, Ctx),
    NamespaceStr = make_str(Namespace),
    PrefixStr = make_str(Mod#module.prefix),
    case Namespace of
        undefined ->
            skip;
        _ ->
            OutF([nl(), Indent, "xmlns:", PrefixStr, $=,
                  qm(), NamespaceStr, qm()])
    end,
    Imports = Mod#module.imports,
    module_arg_imports(S, Ctx, Imports, Indent);

module_arg(_Lvl, _S, _) -> skip.

search_for_namespace(undefined, Mod, Ctx) ->
    {_, TargetName} = Mod#module.xpath_ns_map,
    Target = yang:search_module(Ctx, TargetName, undefined),
    case Target of
        {true, _, TargetMod} ->
            make_str(TargetMod#module.namespace);
        _ ->
            undefined
    end;

search_for_namespace(Namespace, _Mod, _Ctx) ->
    Namespace.


search_for_prefix(ModuleName, Ctx) ->
    Target = yang:search_module(Ctx, ModuleName, undefined),
    case Target of
        {true, _, TargetMod} ->
            make_str(TargetMod#module.prefix);
        _ ->
            undefined
    end.





module_arg_imports(_S, _Ctx, [], _Indent) ->
    skip;
module_arg_imports(S, Ctx, [Import|Imports], Indent) ->
    OutF = S#state.outf,
    {Namespace, _, Prefix, _} = Import,
    PrefixStr = make_str(Prefix),
    {_Value, TargetM} = yang:get_module(Namespace, undefined, Ctx),
    TargetMNamespaceStr = make_str(TargetM#module.namespace),
    OutF([nl(), Indent, "xmlns:", PrefixStr, $=,
          qm(), TargetMNamespaceStr, qm()]),
    module_arg_imports(S, Ctx, Imports, Indent).

sort_canonical(Keyword, SubStmts, S) ->
    OrderMap = S#state.order_map,
    case maps:get(Keyword, OrderMap, undefined) of
        undefined ->
            case yang_parser:get_statement_spec(Keyword) of
                {value, {_, _, Rules, _}} ->
                    NewOrderMap = maps:put(Keyword, mk_order(Rules), OrderMap),
                    NewS = S#state{order_map = NewOrderMap},
                    sort_canonical(Keyword, SubStmts, NewS);
                _ ->
                    {S, SubStmts}
            end;
        Order ->
            {S, lists:sort(
              fun(A, B) ->
                      ordinal(yang:stmt_keyword(A), Order) =<
                          ordinal(yang:stmt_keyword(B), Order)
              end, SubStmts)}
    end.

mk_order(Rules) ->
    mk_order0(Rules, 0, gb_trees:empty()).

mk_order0([{'$cut', _} | T], N, Tree) ->
    mk_order0(T, N+1, Tree);
mk_order0([{Keyword, '?'} | T], N, Tree) ->
    mk_order0(T, N+1, gb_trees:enter(Keyword, N, Tree));
mk_order0([{Keyword, '1'} | T], N, Tree) ->
    mk_order0(T, N+1, gb_trees:enter(Keyword, N, Tree));
mk_order0([{Keyword, '*'} | T], N, Tree) ->
    mk_order0(T, N, gb_trees:enter(Keyword, N, Tree));

mk_order0([], _, Tree) ->
    Tree.

ordinal(Keyword, Order) ->
    case gb_trees:lookup(Keyword, Order) of
        {value, N} ->
            N;
        _ ->
            undefined
    end.

s()  -> $\ .
nl() -> $\n.
ot() -> $<.
fs() -> $/.
ct() -> $>.
qm() -> $".

indent(Lvl) -> lists:duplicate(Lvl*2, s()).
indent(Lvl,1) -> C = Lvl*2+1, lists:duplicate(C, s()).

make_str({Prefix, Name}) ->
    iolist_to_binary([make_str(Prefix), ":", make_str(Name)]);
make_str(A) when is_atom(A) ->
    atom_to_binary(A, unicode);
make_str(Str) when is_binary(Str) ->
    Str;
make_str(Int) when is_integer(Int) ->
    integer_to_binary(Int);
make_str(Str) when is_list(Str) ->
    iolist_to_binary(Str).

escape(BStr, RPs) ->
    lists:foldl(
      fun({RPq, RWith}, BStr1) ->
              binary:replace(BStr1, RPq, RWith, [global])
      end, BStr, RPs).

replace_patterns() ->
    [{<<"\\">>, <<"\\\\">>},
     {<<"\"">>, <<"\\\"">>},
     {<<"\t">>, <<"\\t">>},
     {<<"&">>, <<"&amp;">>},
     {<<"<">>, <<"&lt;">>},
     {<<">">>, <<"&gl;">>},
     {<<"\"">>, <<"&quot;">>},
     {<<"'">>, <<"&apos;">>}].



%% Mapping of statements to the YIN representation of their arguments.
%% The values are pairs whose first component specifies whether the
%% argument is stored in a subelement and the second component is the
%% name of the attribute or subelement carrying the argument. See YANG
%% specification.
get_yin_map() ->
    #{'action'=>          {'name',        false},
      'anydata'=>          {'name',        false},
      'anyxml'=>          {'name',        false},
      'argument'=>        {'name',        false},
      'augment'=>         {'target-node', false},
      'base'=>            {'name',        false},
      'belongs-to'=>      {'module',      false},
      'bit'=>             {'name',        false},
      'case'=>            {'name',        false},
      'choice'=>          {'name',        false},
      'config'=>          {'value',       false},
      'contact'=>         {'text',        true},
      'container'=>       {'name',        false},
      'default'=>         {'value',       false},
      'description'=>     {'text',        true},
      'deviate'=>         {'value',       false},
      'deviation'=>       {'target-node', false},
      'enum'=>            {'name',        false},
      'error-app-tag'=>   {'value',       false},
      'error-message'=>   {'value',       true},
      'extension'=>       {'name',        false},
      'feature'=>         {'name',        false},
      'fraction-digits'=> {'value',       false},
      'grouping'=>        {'name',        false},
      'identity'=>        {'name',        false},
      'if-feature'=>      {'name',        false},
      'import'=>          {'module',      false},
      'include'=>         {'module',      false},
      'input'=>           {undefined,     undefined},
      'key'=>             {'value',       false},
      'leaf'=>            {'name',        false},
      'leaf-list'=>       {'name',        false},
      'length'=>          {'value',       false},
      'list'=>            {'name',        false},
      'mandatory'=>       {'value',       false},
      'max-elements'=>    {'value',       false},
      'min-elements'=>    {'value',       false},
      'modifier'=>        {'value',       false},
      'module'=>          {'name',        false},
      'must'=>            {'condition',   false},
      'namespace'=>       {'uri',         false},
      'notification'=>    {'name',        false},
      'ordered-by'=>      {'value',       false},
      'organization'=>    {'text',        true},
      'output'=>          {undefined,     undefined},
      'path'=>            {'value',       false},
      'pattern'=>         {'value',       false},
      'position'=>        {'value',       false},
      'presence'=>        {'value',       false},
      'prefix'=>          {'value',       false},
      'range'=>           {'value',       false},
      'reference'=>       {'text',        true},
      'refine'=>          {'target-node', false},
      'require-instance'=>{'value',       false},
      'revision'=>        {'date',        false},
      'revision-date'=>   {'date',        false},
      'rpc'=>             {'name',        false},
      'status'=>          {'value',       false},
      'submodule'=>       {'name',        false},
      'type'=>            {'name',        false},
      'typedef'=>         {'name',        false},
      'unique'=>          {'tag',         false},
      'units'=>           {'name',        false},
      'uses'=>            {'name',        false},
      'value'=>           {'value',       false},
      'when'=>            {'condition',   false},
      'yang-version'=>    {'value',       false},
      'yin-element'=>     {'value',       false}
     }.
