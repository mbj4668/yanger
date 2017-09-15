%%%-------------------------------------------------------------------
%%% @doc Yanger YANG Output Plugin
%%% Prints a YANG statement tree.
%%%-------------------------------------------------------------------
-module(yanger_yang).
-behaviour(yanger_plugin).

-export([init/1]).

-include_lib("yanger/include/yang.hrl").

init(Ctx0) ->
    Ctx1 = yanger_plugin:register_output_format(Ctx0, yang, fun emit/3),
    Ctx2 = yanger_plugin:register_option_specs(Ctx1, opts()),
    Ctx2.

opts() ->
    [{"YANG output specific options:",
      [
       {yang_canonical, undefined, "yang-canonical", {boolean, false},
        "Print in canonical order"},
       {yang_comment, undefined, "yang-path-comment", {boolean, false},
        "Print a comment with path to current node"}
      ]}].

-record(state, {module,
                module_prefixes,
                outf,
                canonical = false,
                path_comment = false,
                columns = 80,
                rps,
                qp = binary:compile_pattern(quote_pattern()),
                nlp = binary:compile_pattern(<<"\n">>)
               }).

emit(Ctx, Modules, Fd) ->
    OutF = fun (Str) -> file:write(Fd, Str) end,
    State =
        #state{ outf = OutF,
                canonical =
                    proplists:get_value(yang_canonical, Ctx#yctx.options),
                path_comment =
                    proplists:get_value(yang_comment, Ctx#yctx.options),
                rps =
                    [{binary:compile_pattern(RP), RWith} ||
                        {RP, RWith} <- replace_patterns()]
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
    emit_stmts([M#module.stmt], 0, undefined, [], State).

emit_stmts([], _Lvl, _PrevKwdClass, _Path, _S) ->
    ok;
emit_stmts([Stmt|Stmts], Lvl, PrevKwdClass, Path0, S) ->
    OutF = S#state.outf,
    Path = update_path(Stmt, Path0),
    Keyword = yang:stmt_keyword(Stmt),
    case path_kw(Keyword) of
        yes when (S#state.path_comment == true) andalso (Path /= undefined) ->
            OutF([nl(), indent(Lvl), "// ", path2str(Path), nl()]);
        _ ->
            ok
    end,
    case Keyword of
        '_comment' ->
            emit_comment(yang:stmt_arg(Stmt), Lvl, S),
            NewKwdClass = PrevKwdClass;
        _ ->
            NewKwdClass = emit_stmt_keyword(Keyword, Lvl, PrevKwdClass, S),
            emit_stmt_arg(yang:stmt_arg(Stmt), Lvl, S, Keyword),
            case yang:stmt_substmts(Stmt) of
                [] ->
                    OutF([sc(),nl()]);
                SubStmts0 ->
                    OutF([s(),ob(),nl()]),
                    SubStmts =
                        if S#state.canonical ->
                                sort_canonical(Keyword, SubStmts0);
                           true ->
                                SubStmts0
                        end,
                    emit_stmts(SubStmts, Lvl+1, NewKwdClass, Path, S),
                    OutF([indent(Lvl),cb(),nl()])
            end
    end,
    emit_stmts(Stmts, Lvl, NewKwdClass, Path0, S).

emit_stmt_keyword(Kwd, Lvl, PrevKwdClass, S) ->
    OutF = S#state.outf,
    KwdClass = kwd_class(Kwd),
    ExtraNL =
        ((Lvl == 1) andalso (KwdClass /= PrevKwdClass)
         andalso (KwdClass /= 'extension'))
        orelse
        kwd_with_trailing_nl(Kwd),
    if
        ExtraNL ->
            OutF([nl(), indent(Lvl)]);
        true ->
            OutF(indent(Lvl))
    end,
    case Kwd of
        {Module, Keyword} ->
            Prefix = yang:map_get(Module, S#state.module_prefixes),
            OutF(make_str({Prefix, Keyword}));
        Keyword ->
            OutF(make_str(Keyword))
    end,
    if
        Lvl == 0 ->
            'header';
        true ->
            KwdClass
    end.

emit_stmt_arg([], _, _, _) ->
    ok;
emit_stmt_arg(Arg, Lvl, S, Keyword) ->
    OutF = S#state.outf,
    ArgStr = make_str(Arg),
    case classify_quoting(Arg, ArgStr, S#state.qp, Keyword) of
        prefer_squote ->
            case binary:match(ArgStr, <<"'">>) of
                nomatch ->
                    OutF([s(), $', ArgStr, $']);
                _ ->
                    %% contains single quote, must use double quotes
                    OutF([s(), $", escape(ArgStr, S#state.rps), $"])
            end;
        dquote ->
            ForceNL = force_newline_arg(Keyword),
            case do_nl(ArgStr, S#state.nlp, ForceNL) of
                true ->
                    Indent = indent(Lvl+1),
                    [Str1|Strs] = binary:split(ArgStr, S#state.nlp, [global]),
                    OutF([nl(), Indent, $", escape(Str1, S#state.rps)]),
                    LastLineWasEmpty =
                        lists:foldl(
                          fun (<<>>, _) ->
                                  OutF([nl()]),
                                  true;
                              (BStr, _) ->
                                  OutF([nl(), Indent, s(),
                                        escape(BStr, S#state.rps)]),
                                  false
                          end, false, Strs),
                    if
                        LastLineWasEmpty ->
                            OutF([Indent, $"]);
                        true ->
                            OutF([$"])
                    end;
                false ->
                    OutF([s(), $", escape(ArgStr, S#state.rps), $"])
            end;
        noquote ->
            OutF([s(), ArgStr])
    end.

emit_comment(StrBin, Lvl, S) ->
    OutF = S#state.outf,
    Str = ?b2l(StrBin),
    Lines = string:tokens(Str, [$\n]),
    Indent = indent(Lvl),
    lists:foreach(
      fun(Line) ->
              case Line of
                  [$\* | _] = SLine ->
                      OutF([Indent, " ", SLine, "\n"]);
                  SLine ->
                      OutF([Indent, SLine, "\n"])
              end
      end, Lines),
    %% add additional newline after multi-line comments
    case Str of
        "/*" ++ _ ->
            OutF("\n");
        _ ->
            true
    end.

classify_quoting(Arg, ArgStr, QP, Keyword) ->
    %% If the underlying type is a string, it is represented as a binary,
    %% and it should be quoted, except if it is a 'date'.
    %% Some other types/keywords are also forced to be double quoted.
    case yang_parser:get_statement_spec(Keyword) of
        {value, {_, ArgTypeName, _, _}} ->
            ok;
        _ ->
            ArgTypeName = []
    end,
    if Keyword == 'pattern' orelse
       Keyword == 'when' orelse
       Keyword == 'must' ->
            prefer_squote;
       Keyword == 'yang-version' ->
            noquote;
       ArgTypeName == 'identifier-ref' ->
            noquote;
       (is_binary(Arg) andalso not(ArgTypeName == 'date'))
       orelse Keyword == 'default'
       orelse Keyword == 'enum'
       orelse ArgTypeName == 'uri' ->
            dquote;
       Arg == '' ->
            dquote;
       true ->
            %% To be safe, we check any other argument for characters that
            %% must be quoted.
            case binary:match(ArgStr, QP) of
                nomatch ->
                    noquote;
                _ ->
                    dquote
            end
    end.

sort_canonical(Keyword, SubStmts) ->
    case get({order, Keyword}) of
        undefined ->
            case yang_parser:get_statement_spec(Keyword) of
                {value, {_, _, Rules, _}} ->
                    put({order, Keyword}, mk_order(Rules)),
                    sort_canonical(Keyword, SubStmts);
                _ ->
                    SubStmts
            end;
        Order ->
            lists:sort(
              fun(A, B) ->
                      ordinal(yang:stmt_keyword(A), Order) =<
                      ordinal(yang:stmt_keyword(B), Order)
              end, SubStmts)
    end.

mk_order(Rules) ->
    mk_order0(Rules, 0, gb_trees:empty()).

mk_order0([{Keyword, _} | T], N, Tree) ->
    mk_order0(T, N+1, gb_trees:enter(Keyword, N, Tree));
mk_order0([], _, Tree) ->
    Tree.

ordinal(Keyword, Order) ->
    case gb_trees:lookup(Keyword, Order) of
        {value, N} ->
            N;
        _ ->
            undefined
    end.

update_path(_Stmt, undefined) ->
    undefined;
update_path(Stmt, CurrP) ->
    case path_kw(Keyword = yang:stmt_keyword(Stmt)) of
        stop ->
            undefined;
        yes ->
            [yang:stmt_arg(Stmt)|CurrP];
        keyword ->
            [Keyword|CurrP];
        no ->
            CurrP
    end.

path_kw('grouping') -> stop;
path_kw('leaf') -> yes;
path_kw('leaf-list') -> yes;
path_kw('container') -> yes;
path_kw('list') -> yes;
path_kw('anydata') -> yes;
path_kw('anyxml') -> yes;
path_kw('notification') -> yes;
path_kw('rpc') -> yes;
path_kw('action') -> yes;
path_kw('input') -> keyword;
path_kw('output') -> keyword;
path_kw({'tailf-common', 'action'}) -> yes;
path_kw(_) -> no.

path2str([]) ->
    [$/];
path2str(Path) ->
    [[$/, case _PE of
              {Mod,Stmt} -> [?a2l(Mod),$:,?a2l(Stmt)];
              Stmt when is_atom(Stmt) -> ?a2l(Stmt)
          end] || _PE <- lists:reverse(Path)].

s()  -> $\ .
nl() -> $\n.
sc() -> $;.
ob() -> ${.
cb() -> $}.

indent(Lvl) -> lists:duplicate(Lvl*2, s()).

make_str({Prefix, Name}) ->
    iolist_to_binary([make_str(Prefix), ":", make_str(Name)]);
make_str(A) when is_atom(A) ->
    list_to_binary(atom_to_list(A));
make_str(Str) when is_binary(Str) ->
    Str;
make_str(Int) when is_integer(Int) ->
    list_to_binary(integer_to_list(Int));
make_str(Str) when is_list(Str) ->
    iolist_to_binary(Str).

do_nl(_Str, _, true) ->
    true;
do_nl(Str, NLP, _) ->
    case binary:match(Str, NLP) of
        nomatch ->
            false;
        _ ->
            true
    end.

escape(BStr, RPs) ->
    lists:foldl(
      fun({RPq, RWith}, BStr1) ->
              binary:replace(BStr1, RPq, RWith, [global])
      end, BStr, RPs).

%% Substrings which requires us to quote a string
quote_pattern() ->
    [
     %% Required by the RFC
     <<" ">>, <<"\t">>, <<"{">>, <<"}">>, <<";">>,
     <<"//">>, <<"/*">>, <<"*/">>,

     %% Looks nicer if we quote a string that contains any of these:
     <<"\\">>, <<".">>, <<"/">>, <<"=">>, <<"urn:">>,

     %% These also look nicer, and quoting is *necessary* when they're
     %% the first character (otherwise it's taken as the start of a
     %% quoted string), and it probably *will* be required for YANG 1.1
     <<"\"">>, <<"'">>
    ].

replace_patterns() ->
    [{<<"\\">>, <<"\\\\">>},
     {<<"\"">>, <<"\\\"">>},
     {<<"\t">>, <<"\\t">>}].

force_newline_arg(description) -> true;
force_newline_arg(organization) -> true;
force_newline_arg(contact) -> true;
force_newline_arg({'tailf-common', info}) -> true;
force_newline_arg(_) -> false.

kwd_class('yang-version') -> 'header';
kwd_class('namespace') -> 'header';
kwd_class('prefix') -> 'header';
kwd_class('belongs-to') -> 'header';
kwd_class('organization') -> 'meta';
kwd_class('contact') -> 'meta';
kwd_class('description') -> 'meta';
kwd_class('reference') -> 'meta';
kwd_class('import') -> 'linkage';
kwd_class('include') -> 'linkage';
kwd_class('revision') -> 'revision';
kwd_class('typedef') -> 'defs';
kwd_class('grouping') -> 'defs';
kwd_class('identity') -> 'defs';
kwd_class('feature') -> 'defs';
kwd_class('extension') -> 'defs';
kwd_class('module') -> undefined;
kwd_class('submodule') -> undefined;
kwd_class({_, _}) -> 'extension';
kwd_class(_) -> 'body'.

kwd_with_trailing_nl('typedef') -> true;
kwd_with_trailing_nl('grouping') -> true;
kwd_with_trailing_nl('identity') -> true;
kwd_with_trailing_nl('feature') -> true;
kwd_with_trailing_nl('extension') -> true;
kwd_with_trailing_nl(_) -> false.
