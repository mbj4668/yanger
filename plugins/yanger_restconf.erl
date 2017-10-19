%%%-------------------------------------------------------------------
%%% @doc Yanger RESTCONF Plugin
%%% Verifies RESTCONF YANG statements as defined in RFC 8040,
%%% Section 8. RESTCONF Module.
%%%-------------------------------------------------------------------
-module(yanger_restconf).
-behaviour(yanger_plugin).

-export([init/1]).

-import(yanger_plugin, [add_error/4]).

-include_lib("yanger/include/yang.hrl").

-define(RESTCONF, 'ietf-restconf').

init(Ctx0) ->
    yanger_plugin:install_grammar(?RESTCONF, grammar()),
    Ctx1 = yanger_plugin:register_data_definition_stmt(
             Ctx0,
             {?RESTCONF, 'yang-data'},
             'tailf:structure'),
    Ctx2 = yanger_plugin:register_error_codes(
             Ctx1,
             [{'RESTCONF_YANG_DATA_CHILDREN', error,
               "yang-data must have a single container as a child"}]),
    Ctx3 = yanger_plugin:register_hook(
             Ctx2,
             #hooks.pre_mk_sn,
             fun pre_mk_sn/5),
    _Ctx4 = yanger_plugin:register_hook(
             Ctx3,
             #hooks.post_mk_sn,
             fun post_mk_sn/5).

grammar() ->
    %% {<keyword>,<argument type name | []>,
    %%  <substmts>,
    %%  {<occurance when used>, <list of keywords where <keyword> can occur>}}
    [
     {'yang-data', 'identifier',
      [{'uses', '?'},
       {'container', '?'}],
      {'*', ['module', 'submodule']}}
    ].

pre_mk_sn(Ctx, #sn{stmt = {Keyword, _, _, _}} = Sn,
          final, _UsesPos, _Ancestors)
  when Keyword == {?RESTCONF, 'yang-data'} ->
    {Ctx, Sn#sn{config = 'ignore'}};
pre_mk_sn(Ctx, Sn, _Mode, _UsesPos, _Ancestors) ->
    {Ctx, Sn}.

%% Verify that there is only one child data node, i.e. manually check that
%% groupings doesn't contain more than one container.
post_mk_sn(Ctx,
           #sn{children = [_, #sn{stmt = {_, _, ChildPos, _}} | _ ],
               stmt = {Keyword, _, _Pos, _}} = Sn,
           final, _UsesPos, _Ancestors)
  when (Keyword == {?RESTCONF, 'yang-data'}) ->
    {add_error(Ctx, ChildPos, 'RESTCONF_YANG_DATA_CHILDREN', []), Sn};
post_mk_sn(Ctx,
           #sn{children = [#sn{stmt = {ChildKeyword, _, ChildPos, _}}],
               stmt = {Keyword, _, _, _}} = Sn,
           final, _UsesPos, _Ancestors)
  when (Keyword == {?RESTCONF, 'yang-data'})
       andalso
       (ChildKeyword /= 'container') ->
    {add_error(Ctx, ChildPos, 'RESTCONF_YANG_DATA_CHILDREN', []), Sn};
post_mk_sn(Ctx, Sn, _Mode, _UsesPos, _Ancestors) ->
    {Ctx, Sn}.
