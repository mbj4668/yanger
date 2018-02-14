-module(mksn).
-behaviour(yanger_plugin).

-export([init/1,
         emit/3]).

-include_lib("yanger/include/yang.hrl").

init(Ctx) ->
    yanger_plugin:register_hook(Ctx, #hooks.pre_mk_sn, fun pre_mk_sn/5).

emit(_YCtx, _Modules, _Fd) ->
    ok.

pre_mk_sn(Ctx, Sn, Mode, UsesPos, Ancestors) ->
    UsesStr = if UsesPos == undefined -> "";
                 true -> " " ++ yang_error:fmt_pos(UsesPos)
              end,
    XStr = case yang:search_one_substmt(description, Sn#sn.stmt) of
               {_, Arg, _, _} ->
                   " " ++ Arg;
               false ->
                   ""
           end,
    ?iof("mk_sn: ~p ~p~s~s\n",
         [fmt_path([Sn | Ancestors]), Mode, UsesStr, XStr]),
    {Ctx, Sn}.

fmt_path([#sn{name = {ModName, Name}, kind = Kind} | T]) ->
    fmt_path(T) ++ "/" ++ ?a2l(ModName) ++ ":" ++ ?a2l(Name) ++
        "(" ++ ?a2l(Kind) ++ ")";
fmt_path([#sn{name = Name, kind = Kind} | T]) ->
    fmt_path(T) ++ "/" ++ ?a2l(Name) ++ "(" ++ ?a2l(Kind) ++ ")";
fmt_path([#module{}]) ->
    "ROOT: ";
fmt_path([]) ->
    "NONE: ".


