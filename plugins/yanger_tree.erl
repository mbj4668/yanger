-module(yanger_tree).
-behaviour(yanger_plugin).

-export([init/1,
         emit/3]).

-include("yang.hrl").

init(Ctx0) ->
    Ctx1 = yanger_plugin:register_option_specs(Ctx0, opts()),
    Ctx2 = yanger_plugin:register_output_format(Ctx0, tree, ?MODULE),
    Ctx2.

opts() ->
    [{tree_depth, undefined, "--tree-depth", integer,
      "Number of levels to print"}].

emit(Ctx, Modules, Fd) ->
    ok.
