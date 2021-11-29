%% feature, identity

%%%-------------------------------------------------------------------
%%% @doc Yanger YANG Output Plugin
%%% Replaces the stmt tree with an expanded stmt tree
%%%-------------------------------------------------------------------
-module(yanger_expand).
-behaviour(yanger_plugin).

-export([init/1]).

-include_lib("yanger/include/yang.hrl").

init(Ctx0) ->
    Ctx1 = yanger_plugin:register_transform(Ctx0, expand, fun expand/2),
    Ctx2 = yanger_plugin:register_option_specs(Ctx1, opts()),
    Ctx2.

opts() ->
    [{"expand transform specific options:",
      [
       {expand_exclude, undefined, "expand-exclude", string,
        "When expanding, exclude this path from output"},
       {expand_include, undefined, "expand-include", string,
        "When expanding, include this path from output"}
      ]
    }].

-record(state, {module,
                pos,
                rprefix_map,
                exclude = [],
                include = []
               }).

expand(_Ctx, []) ->
    [];
expand(Ctx, [Module|Modules]) ->
    %% Modules1 =
    %%     case proplists:get_value(expand_combine, Ctx#yctx.options) of
    %%         true ->
    %%             combine(lists:reverse(Modules0));
    %%         _ ->
    %%             Modules0
    %%     end,
    Exclude = get_paths_from_option(Ctx, expand_exclude),
    Include = get_paths_from_option(Ctx, expand_include),
    State = #state{module = Module, exclude = Exclude, include = Include,
                   pos = {Module#module.filename,1}},
    [expand_module(Module, State)|Modules].

get_paths_from_option(Ctx, OptionName) ->
    [lists:reverse([?l2a(Step) || Step <- string:tokens(Path,"/")]) ||
        Path <- proplists:get_all_values(OptionName, Ctx#yctx.options)].


%% ------------------------------------------------------------------------
%% combine([Module|Modules]) ->
%%     io:format(standard_error,
%%               "~p: combining ~p modules\n", [?MODULE, length(Modules)+1]),
%%     combine(Module, Modules).

%% combine(Module, []) ->
%%     [Module];
%% combine(Left, [Right|Rest]) ->
%%     Module = combine_module(Left, Right),
%%     combine(Module, Rest).

%% combine_module(Left, _Right) ->
%%     %% NYI
%%     Left.

% Stmt is the raw stmt in the module record.
expand_module(M, State0) ->
    PxMap    = create_prefix_map(M),
    State    = State0#state{rprefix_map = PxMap},
    TypeDefs = yang:map_to_list((M#module.typedefs)#typedefs.map),
    Features = yang:map_to_list(M#module.features),
    Identities = yang:map_to_list(M#module.identities),
    Extensions = yang:map_to_list(M#module.extensions),
    F0 = fun () ->
                 filtermap(M#module.children,fun sn/3, [], State)
         end,
    F1 = fun () ->
                 map(Identities, fun identity/2, State, F0)
         end,
    F2 = fun () ->
                 map(Features, fun feature/2, State, F1)
         end,
    F3 = fun () ->
                 map(Extensions, fun extension/2, State, F2)
         end,
    Stmt     =
        {module, M#module.modulename, pos(State),
         [{namespace, M#module.namespace, pos(State), []},
          {prefix, M#module.prefix, pos(State), []} |
          case yang:search_one_substmt('yang-version', M#module.stmt) of
              false  -> [];
              YVStmt -> [YVStmt]
          end ++
          import(State,
                 fun () ->
                     filter_meta(yang:stmt_substmts(M#module.stmt)) ++
                     map(TypeDefs, fun typedef/2, State, F3)
                 end)]},
    M#module{stmt           = Stmt,
             prefix_map     = reverse_map(PxMap),
             local_augments = [] % reset the local augments,
                                 % they should be expanded now
            }.

%% include meta information
%% and revision history, as defined in RFC6020
%% (header information should go before import)
filter_meta(Subs) ->
    Keepers = ['organization',
               'contact', 'description', 'reference', 'revision'],
    lists:filter(fun(Stmt) ->
                     lists:member(yang:stmt_keyword(Stmt), Keepers)
                 end,Subs).

reverse_map(Map0) ->
    yang:map_foldl(
      fun (Key, Value, Map) ->
              yang:map_insert(Value, Key, Map)
      end, yang:map_new(), Map0).

map([], _F, _State, NxtF) ->
    NxtF();
map([H|T], F, State, NxtF) ->
    [F(H, State)|map(T, F, State, NxtF)].

filtermap(L, F, Path, State) ->
    filtermap(L, F, Path, State, fun () -> [] end).

filtermap([], _F, _Path, _State, NxtF) ->
    NxtF();
filtermap([H|T], F, Path, State, NxtF) ->
    case F(H, Path, State) of
        true ->
            [H|filtermap(T, F, Path, State, NxtF)];
        {true, Value} ->
            [Value|filtermap(T, F, Path, State, NxtF)];
        false ->
            filtermap(T, F, Path, State, NxtF)
    end.

pos(S) ->
    S#state.pos.

import(S = #state{rprefix_map = PxMap}, NxtF) ->
    %% Issue an import statment for each module in the map
    import(yang:map_next(yang:map_iterator(PxMap)), S, NxtF).

import(none, _S, NxtF) ->
    NxtF();
import({'$self', _Prefix, I}, S, NxtF) ->
    import(yang:map_next(I), S, NxtF);
import({Module, Prefix, I}, S, NxtF) ->
    [{import, Module, pos(S), [{prefix, Prefix, pos(S), []}]}|
     import(yang:map_next(I), S, NxtF)].


%% Need to decide on a prefix for each imported module
%%
%% FIXME this doesn't quite work if we have used different prefixes
%% for the same module in different submodules. The reason it doesn't
%% work is that we just grab the 'type' statement from the #sn
%% node. One solution could be to issue one import statement for each
%% prefix (i.e. allowing multiple imports of the same module but with
%% different prefixes, which I think is legal). The other would be to
%% go through all stmts we grab and replace with correct prefix...
%%
create_prefix_map(M) ->
    create_prefix_map(M, yang:map_new()).

create_prefix_map(M, PxMap0) ->
    PxMap1 =
        yang:map_foldl(fun (Px, Mod, Map0) ->
                               case yang:map_lookup(Mod, Map0) of
                                   {value, Px} ->
                                       Map0;
                                   {value, _Px1} ->
                                       %% Hmm
                                       Map0;
                                   none ->
                                       yang:map_insert(Mod, Px, Map0)
                               end
                       end,
                       PxMap0, M#module.prefix_map),
    %% Now recurse into sub-modules
    lists:foldl(
      fun ({SM, _}, PxMap) ->
              create_prefix_map(SM, PxMap)
      end, PxMap1, M#module.submodules).


typedef({_Name, TD}, _S) ->
    TD#typedef.stmt.

feature({_Name, F}, _S) ->
    F#feature.stmt.

identity({_Name, Id}, _S) ->
    Id#identity.stmt.

extension({_Name, E}, _S) ->
    E#extension.stmt.

sn(SN, P, S) ->
    %% FIXME: Right now typedefs are added in the function with the same name...
    %% Need to solve local typedefs: make up unique name, and output in global
    %% scope, when outputing types need to check this translation tab.
    CurrP = update_path(SN, P),
    case proceedp(CurrP, SN, S) of
        true ->
            {true, sn2(SN, CurrP, S)};
        false ->
            false
    end.

proceedp(_, #sn{kind = Kind, children = []}, _S)
  when Kind == 'input' orelse Kind == 'output' ->
    %% do not output the automatically added node for input/output
    false;
proceedp(_, #sn{name = {_Module, _Name}}, _S) ->
    %% do not output remote augments from other modules (may be caused
    %% e.g. by a --deviation-module importing augmenting modules
    false;
proceedp(CurrP, SN, S) ->
    case lists:member(CurrP, S#state.exclude) of
        true ->
            false;
        false when S#state.include == [] ->
            true;
        false ->
            case match_path(CurrP, S#state.include) of
                exact ->
                    N = SN#sn.kind,
                    if
                        (N == leaf) orelse (N == 'leaf-list') orelse
                        (N == container) orelse (N == list) ->
                            true;
                        true ->
                            false
                    end;
                Bool ->
                    Bool
            end
    end.

match_path(_Path, []) ->
    false;
match_path(Path, [Path|_Paths]) ->
    exact;
match_path(Path, [MatchPath|Paths]) ->
    case prefix_match(Path, MatchPath) of
        true ->
            true;
        false ->
            match_path(Path, Paths)
    end.

prefix_match(Path, MatchPath) when length(Path) < length(MatchPath) ->
    prefix_match1(MatchPath, Path);
prefix_match(Path, MatchPath) ->
    prefix_match1(Path, MatchPath).

prefix_match1(Path, Path) ->
    true;
prefix_match1([], _) ->
    false;
prefix_match1([_|T], MatchPath) ->
    prefix_match1(T, MatchPath).

sn2(#sn{kind = Kind, name = Name, children = Ch, stmt = Stmt} = SN, P, S) ->
    F0  = fun () -> filtermap(Ch, fun sn/3, P, S) end,
    F1  = fun () -> default(SN, S, F0) end,
    F2  = fun () -> type(SN, S, F1) end,
    F3  = fun () -> extensions(SN, P, S, F2) end,
    F4  = fun () -> if_feature(SN, S, F3) end,
    F5  = fun () -> must(SN, S, F4) end,
    F6  = fun () -> expand_when(SN, S, F5) end,
    F7  = fun () -> keys(SN, S, F6) end,
    F8  = fun () -> config(SN, S, F7) end,
    F9  = fun () -> typedefs(SN, S, F8) end,
    F10 = fun () -> presence(SN, S, F9) end,
    F11 = fun () -> mandatory(SN, S, F10) end,
    F12 = fun () -> min_elements(SN, S, F11) end,
    F13 = fun () -> max_elements(SN, S, F12) end,
    F14 = fun () -> description(SN, S, F13) end,

    Arg = if Kind == 'input' orelse Kind == 'output' ->
                  [];
             true ->
                  Name
          end,
    {yang:stmt_keyword(Stmt), Arg, pos(S), F14()}.

update_path(SN = #sn{kind = NodeType}, P)
  when (NodeType == 'leaf') orelse
       (NodeType == 'leaf-list') orelse
       (NodeType == 'container') orelse
       (NodeType == 'list') ->
    [SN#sn.name|P];
update_path(_, P) ->
    P.

config(#sn{config = false}, State, NxtF) ->
    [{config, false, pos(State), []}|NxtF()];
config(_SN, _State, NxtF) ->
    NxtF().

keys(#sn{keys = undefined}, _State, NxtF) ->
    NxtF();
keys(#sn{keys = Keys}, State, NxtF) ->
    KeysStr = string:join([?a2l(Name) || Name <- Keys], " "),
    [{key, KeysStr, pos(State), []}|NxtF()].

type(#sn{type = undefined}, _State, NxtF) ->
    NxtF();
type(#sn{type = Type}, _State, NxtF) ->
    [Type#type.stmt|NxtF()].

default(#sn{default = {Stmt, _Value}}, _S, NxtF) ->
    [Stmt|NxtF()];
default(_SN, _S, NxtF) ->
    NxtF().

typedefs(SN, _State, NxtF) ->
    NxtF() ++ yang:search_all_stmts(typedef, SN#sn.stmt).

if_feature(SN, _State, NxtF) ->
    yang:search_all_stmts('if-feature', SN#sn.stmt) ++ NxtF().

must(SN, _State, NxtF) ->
    yang:search_all_stmts(must, SN#sn.stmt) ++ NxtF().

expand_when(SN, _State, NxtF) ->
    yang:search_all_stmts('when', SN#sn.stmt) ++ NxtF().

presence(SN, _State, NxtF) ->
    yang:search_all_stmts(presence, SN#sn.stmt) ++ NxtF().

mandatory(SN, _State, NxtF) ->
    yang:search_all_stmts(mandatory, SN#sn.stmt) ++ NxtF().

description(SN, _State, NxtF) ->
    yang:search_all_stmts(description, SN#sn.stmt) ++ NxtF().


min_elements(SN, _State, NxtF) ->
    yang:search_all_stmts('min-elements', SN#sn.stmt) ++ NxtF().

max_elements(SN, _State, NxtF) ->
    yang:search_all_stmts('max-elements', SN#sn.stmt) ++ NxtF().

extensions(SN, P, S, NxtF) ->
    filtermap(yang:stmt_substmts(SN#sn.stmt), fun extensions/3, P, S, NxtF).
extensions(Stmt, _P, _State) ->
    case yang:stmt_keyword(Stmt) of
        {_, _} ->
            true;
        _ ->
            false
    end.
