%%%-------------------------------------------------------------------
%%% @doc Yanger YANG Output Plugin
%%% Prints SN tree (debug).
%%%-------------------------------------------------------------------
-module(yanger_sn).
-behaviour(yanger_plugin).

-export([init/1]).

-include_lib("yanger/include/yang.hrl").

init(Ctx0) ->
    Ctx1 = yanger_plugin:register_output_format(Ctx0, sn, fun emit/3),
    Ctx2 = yanger_plugin:register_option_specs(Ctx1, opts()),
    Ctx2.

opts() ->
    [{"SN output specific options:",
      [
       {sn_depth, undefined, "sn-depth", {integer, 7}, "Print Depths"},
       {sn_cols,  undefined, "sn-columns", {integer, 80}, "Print Width"}
      ]}].

-record(state, {outf,
                ppf,
                columns = 80,
                depth = 7
               }).

emit(Ctx, Modules, Fd) ->
    OutF = fun (Str) -> io:put_chars(Fd, Str) end,
    State = #state{outf = OutF,
                   ppf = fun pp_sn_record/5,
                   depth = proplists:get_value(sn_depth, Ctx#yctx.options),
                   columns = proplists:get_value(sn_cols, Ctx#yctx.options)
                  },
    lists:foreach(fun (M) -> pp_module(M, State) end, Modules),
    %% If this format plugin will need to produce warnings or errors
    %% in the future, these warnings and errors need to be returned here.
    Errors = [],
    Errors.

pp_module(M, State) ->
    (State#state.outf)(["%% module: ", ?a2l(M#module.name), nl()]),
    print_record(M, 0, State),
    LastF = fun () -> ok end,
    foreach(M#module.children, 1, [], State, LastF).

foreach([], _Lvl, _Path, _State, NxtF) ->
    NxtF();
foreach([H|T], Lvl, Path, State, NxtF0) ->
    NxtF = fun () -> foreach(T, Lvl, Path, State, NxtF0) end,
    (State#state.ppf)(H, Lvl, Path, State, NxtF).


pp_path_comment(SN = #sn{kind = NodeType}, Lvl, P, #state{outf=OutF})
  when
      (NodeType == 'leaf') orelse
      (NodeType == 'leaf-list') orelse
      (NodeType == 'container') orelse
      (NodeType == 'list') orelse
      (NodeType == 'operation') orelse
      (NodeType == 'input') orelse (NodeType == 'output') orelse
      (NodeType == 'notification') orelse (NodeType == anyxml) ->
    CurrP =
        if
            (NodeType == 'operation') orelse (NodeType == 'notification') ->
                [SN#sn.name,NodeType|P];
            true ->
                [SN#sn.name|P]
        end,
    OutF([indent(Lvl),"%% ",path2str(CurrP),nl()]),
    CurrP;
pp_path_comment(_, _, P, _) ->
    P.

path2str(Path) ->
    [[$/, ?a2l(_PE)] || _PE <- lists:reverse(Path)].

pp_sn_record(SN, Lvl, P, S, NxtF0) ->
    CurrP = pp_path_comment(SN, Lvl, P, S),
    print_record(SN, Lvl, S),
    if
        SN#sn.children /= [] ->
            foreach(SN#sn.children, Lvl+1, CurrP, S, NxtF0);
        true ->
            NxtF0()
    end.

print_record(R, Lvl, S) ->
    OutF = S#state.outf,
    OutF([indent(Lvl), "#", ?a2l(element(1,R)), ${, nl()]),
    print_record_fields(R, Lvl+1, S),
    OutF([indent(Lvl), $}, nl()]).

-define(prf(Lvl, S, R, Field),
        print_record_fields(Lvl, S,
                            tl(tuple_to_list(R)), record_info(fields, Field))).

print_record_fields(R, Lvl, S) when is_record(R, type) ->
    ?prf(Lvl, S, R, type);
print_record_fields(R, Lvl, S) when is_record(R, typedef) ->
    ?prf(Lvl, S, R, typedef);
print_record_fields(R, Lvl, S) when is_record(R, typedefs) ->
    ?prf(Lvl, S, R, typedefs);
print_record_fields(R, Lvl, S) when is_record(R, sn) ->
    ?prf(Lvl, S, R, sn);
print_record_fields(R, Lvl, S) when is_record(R, module) ->
    ?prf(Lvl, S, R, module).

-define(ffrec(R, NAME),
        io_lib_pretty:print(R,
                            fun (NAME, _) -> record_info(fields, NAME);
                                (_, _) -> no
                            end)).
-define(ffrec(R, NAME1, NAME2),
        io_lib_pretty:print(R,
                            fun (NAME1, _) -> record_info(fields, NAME1);
                                (NAME2, _) -> record_info(fields, NAME2);
                                (_, _) -> no
                            end)).

print_record_fields(_Lvl, _S, [], []) ->
    ok;
print_record_fields(Lvl, S, [Value0|Values], [Name|Names]) ->
    Pre = lists:flatten(io_lib:format("~s~p = ", [indent(Lvl), Name])),
    OutF = S#state.outf,
    OutF(Pre),
    Value =
        if
            Name == module ->
                io_lib:format("~p ...", [Value0#module.name]);
            (Name == prefix_map) orelse (Name == pmap) ->
                case yang:map_to_list(Value0) of
                    [] ->
                        "[]";
                    List when length(List) > 3 ->
                        io_lib:format("[... ~w elements ...]", [length(List)]);
                    List ->
                        io_lib:write(List, S#state.depth)
                end;
            Name == stmt ->
                "...";
            (Name == groupings) andalso is_record(Value0, groupings) ->
                Gs = [G || {G,_} <- yang:map_to_list(Value0#groupings.map)],
                Col = length(Pre),
                io_lib:print(Gs, Col, S#state.columns, S#state.depth);
            (Name == type) andalso is_record(Value0, type) ->
                OutF([nl()]),
                print_record(Value0, Lvl+1, S),
                ok;
            (Name == typedefs) andalso is_record(Value0, typedefs) ->
                if
                    Value0#typedefs.same_as_parent == true ->
                        "#typedef{same_as_parent = true}";
                    true ->
                        OutF([nl()]),
                        print_record(Value0, Lvl+1, S),
                        ok
                end;
            (Name == parent) andalso is_record(Value0, typedefs) ->
                OutF([nl()]),
                print_record(Value0, Lvl+1, S),
                ok;
            (Name == base) andalso is_record(Value0, typedef) ->
                OutF([nl()]),
                print_record(Value0, Lvl+1, S),
                ok;
            (Name == map) ->
                TDefs = [TDName || {TDName, _} <-
                                       yang:map_to_list(Value0)],
                Col = length(Pre),
                io_lib:print(TDefs, Col, S#state.columns, S#state.depth);
            (Name == filename) ->
                [$",Value0,$"];
            Value0 == [] ->
                "[]";
            is_list(Value0), length(Value0) > 2 ->
                io_lib:format("[... ~w elements ...]", [length(Value0)]);
            true ->
                Col = length(Pre),
                io_lib:print(Value0, Col, S#state.columns, S#state.depth)
        end,
    if
        Value == ok ->
            ok;
        true ->
            OutF([Value, nl()])
    end,
    print_record_fields(Lvl, S, Values, Names).

s()  -> $\ .
nl() -> $\n.

indent(1) -> "  ";
indent(2) -> "  " "  ";
indent(3) -> "  " "  " "  ";
indent(Lvl) -> lists:duplicate(Lvl*2, s()).
