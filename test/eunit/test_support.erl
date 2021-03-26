-module(test_support).

-include_lib("eunit/include/eunit.hrl").
-include_lib("yanger/include/yang.hrl").

-compile(export_all).


%% To get a canonical type from a name in the tree for a module
get_canonical_type(ModName, Name, Ctx) ->
    canonical_type((get_item(ModName, Name, Ctx))#sn.type).

get_default(ModName, Name, Ctx) ->
    (get_item(ModName, Name, Ctx))#sn.default.

get_item(ModName, Name, Ctx) ->
    M = yang:get_module_from_filename(ModName, Ctx),
    sn_find_name(Name, M#module.children).

get_imports(ModName, Ctx) ->
    M = yang:get_module_from_filename(ModName, Ctx),
    first_of_all(M#module.imports).

get_unused_imports(ModName, Ctx) ->
    {_, Unused} = lists:keyfind(list_to_atom(ModName), 1, Ctx#yctx.unused_imports),
    first_of_all(Unused).

first_of_all(L) when is_list(L) -> lists:sort([element(1, X) || X <- L]);
first_of_all(X) -> X.

get_modrevs(Ctx) ->
    lists:sort(gb_trees:to_list(Ctx#yctx.modrevs)).

sn_find_name(Name, Sn = #sn{name = Name}) -> Sn;
sn_find_name(Name, [Sn = #sn{name = Name} | _]) -> Sn;
sn_find_name(Name, [#sn{children = Children} | Sns]) ->
    %% Depth first search
    case sn_find_name(Name, Children) of
        undefined ->
            sn_find_name(Name, Sns);
        Sn ->
            Sn
    end;
sn_find_name(_Name, #sn{}) -> undefined;
sn_find_name(_Name, []) -> undefined.

%% Shorten an SN tree into a canonical form, where only kind and name
%% (and any children) are visible.
canonical_sn(#sn{kind = Kind, name = Name, children = Children})
  when Children /= [] ->
    {Kind, Name, canonical_sn(Children)};
canonical_sn(#sn{kind = Kind, name = Name}) ->
    {Kind, Name};
canonical_sn(L) when is_list(L) ->
    compact([canonical_sn(X) || X <- L]);
canonical_sn(_) ->
    undefined.

canonical_type(#typedef{type = Type}) -> canonical_type(Type);
canonical_type(#type{base = Base}) -> canonical_type(Base);
canonical_type(X) -> X.

%% Ruby compact in Erlang form
compact(L) when is_list(L) -> [X || X <- L, X /= undefined];
compact(L) -> L.

%% -------------------------------------
%% Creating a stmt tree and calling yang
add_stmt_tree(Ctx, Name, Stmt0) ->
    add_stmt_tree(Ctx, Name, [], Stmt0).
add_stmt_tree(Ctx0, Name, Opts, Stmt0) ->
    Ctx1 = if Ctx0 == undefined -> yang:init_ctx(yang:new_ctx([]), []);
              true -> Ctx0
           end,
    Stmt = mk_yang(Name, Opts, Stmt0),
    %%    io:format("yang: ~p\n", [Stmt]),
    yang:add_parsed_stmt_tree(Ctx1, Stmt, Name, primary, undefined, undefined,
                              warning, undefined).

mk_yang(Module, Opts, Stmts) ->
    reset_line_no(Module),
    Pos1 = next_pos(),
    Hdr = mk_yang_hdr(Module),
    Imports = mk_yang_imports(get_opt(imports, Opts)),
    Includes = mk_yang_includes(get_opt(includes, Opts)),
    ModKeyword = get_opt(mod_keyword, Opts, module),
    io:format("using ModKeyword: ~p\n", [ModKeyword]),
    Body = mk_yang_body(Stmts),
    [{ModKeyword, ?l2a(Module), Pos1, Hdr ++ Imports ++ Includes ++Body}].

mk_yang_hdr(Module) ->
    NameSpace = ?l2a("urn:"++Module),
    [{'yang-version', <<"1.1">>, next_pos(), []},
     {namespace, NameSpace, next_pos(), []},
     {prefix, ?l2a(Module), next_pos(), []}
    ].

mk_yang_imports(undefined) ->
    [];
mk_yang_imports(NameList) ->
    [mk_yang_one_import(Name) || Name <- NameList].

mk_yang_one_import({Name, Prefix}) ->
    {import, ?l2a(Name), next_pos(), [{prefix, ?l2a(Prefix), next_pos(), []}]};
mk_yang_one_import(Name) ->
    mk_yang_one_import({Name, Name}).

mk_yang_includes(undefined) ->
    [];
mk_yang_includes(NameList) ->
    [mk_yang_one_include(Name) || Name <- NameList].

mk_yang_one_include(Name) ->
    {include, ?l2a(Name), next_pos(), []}.

mk_yang_body(Stmts) ->
    add_pos_to_all(Stmts).

add_pos_to_all(L) when is_list(L) ->
    [add_pos_to_all(X) || X <- L];

add_pos_to_all({A, B}) -> % This is short for something without children
    {A, B, next_pos(), []};
add_pos_to_all({A, B, C}) -> % This is short for something with children
    {A, B, next_pos(), add_pos_to_all(C)}.

%% Dirty position functions, not reentrant
next_pos() ->
    {"./"++line_no_module()++".yang", next_line_no()}.

next_line_no() ->
    put(dirty_line_no, get(dirty_line_no) + 1).

line_no_module() ->
    get(dirty_line_no_module).

reset_line_no(Module) ->
    put(dirty_line_no_module, Module),
    put(dirty_line_no, 2).

%% Standard option handling
get_opt(Key, Opts) ->
    get_opt(Key, Opts, undefined).
get_opt(Key, Opts, Default) ->
    case lists:keyfind(Key, 1, Opts) of
        {_, Val} -> Val;
        _ -> Default
    end.

dummy_pos(SN = #sn{}) -> SN#sn{stmt = {x, x, stmt_pos, x}}.
