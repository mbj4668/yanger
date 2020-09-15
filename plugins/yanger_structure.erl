-module(yanger_structure).
-behaviour(yanger_plugin).

-export([init/1]).

-import(yanger_plugin, [add_error/4]).

-include_lib("yanger/include/yang.hrl").

-define(SX_MODULE_NAME, 'ietf-yang-structure-ext').

init(Ctx) ->
    yanger_plugin:install_grammar(?SX_MODULE_NAME, grammar()),
    Ctx1 = yanger_plugin:register_data_definition_stmt(
             Ctx, {?SX_MODULE_NAME, 'structure'}, 'structure'),
    Ctx2 = yanger_plugin:register_conditional_hook(
             Ctx1, #hooks.pre_mk_sn, [{imports, ?SX_MODULE_NAME}],
             fun pre_mk_sn/5),
    Ctx2.

grammar() ->
    %% {<keyword>,<argument type name | []>,
    %%  <substmts>,
    %%  {<occurrence when used>, <list of keywords where <keyword> can occur>}}
    [
     {'structure', 'identifier',
      [{'must', '*'},
       {'status', '?'},
       {'description', '?'},
       {'reference', '?'},
       {'typedef', '*'},
       {'grouping', '*'},
       {'leaf', '*'},
       {'leaf-list', '*'},
       {'container', '*'},
       {'list', '*'},
       {'choice', '*'},
       {'anydata', '*'},
       {'anyxml', '*'},
       {'uses', '*'}
      ],
      {'*', ['module', 'submodule']}}
    ].

pre_mk_sn(Ctx, #sn{kind = 'structure'} = Sn, _, _, _) ->
    {Ctx, Sn#sn{config = ignore}};
pre_mk_sn(Ctx, Sn, _, _, _) ->
    {Ctx, Sn}.
