%%%-------------------------------------------------------------------
%%% @doc Yanger Tree Output Plugin
%%% Prints the YANG data tree in ascii.
%%%-------------------------------------------------------------------
-module(yanger_tree).
-behaviour(yanger_plugin).

-export([init/1,
         emit/3]).

-include("yang.hrl").

init(Ctx0) ->
    Ctx1 = yanger_plugin:register_output_format(Ctx0, tree, ?MODULE),
    Ctx2 = yanger_plugin:register_option_specs(Ctx1, opts()),
    Ctx2.

opts() ->
    %% same opts as pyang?
    [{"Tree output specific options:",
      [{tree_depth, undefined, "tree-depth", integer,
        "Number of levels to print"}]}].

emit(Ctx, Modules, Fd) ->
    Depth = proplists:get_value(tree_depth, Ctx#yctx.options),
    io:format(Fd, "tree emit! depth=~p mods=~p\n",
              [Depth, [M#module.name || M <- Modules]]),
    ok.
