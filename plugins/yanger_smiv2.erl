%%%-------------------------------------------------------------------
%%% @doc Yanger SMIv2 Plugin
%%% Verifies SMIv2 YANG statements as defined in RFC 6643.
%%%-------------------------------------------------------------------
-module(yanger_smiv2).
-behaviour(yanger_plugin).

-export([init/1]).

-include("yang.hrl").

-define(SMI_MODULE_NAME, 'ietf-yang-smiv2').

init(Ctx) ->
    yanger_plugin:install_arg_types(arg_types()),
    %yanger_plugin:install_grammar(?SMI_MODULE_NAME, grammar()).
    Ctx.

arg_types() ->
    [
     {'smi-oid',
      "(([0-1](\.[1-3]?[0-9]))|(2\.(0|([1-9]\d*))))(\.(0|([1-9]\d*)))*",
      string},
     {'smi-max-access',
      "not-accessible|accessible-for-notify|read-only|read-write|read-create",
      atom}
    ].

grammar() ->
    %% {<keyword>, <occurance when used>,
    %%    {<argument type name | []>, <substmts>},
    %%  <list of keywords where <keyword> can occur>}

    [
     {'display-hint', '?', 'string', [],
      ['leaf', 'typedef']},

     {'max-access', '?', 'smi-max-access', [],
      ['leaf', 'typedef']},

     {'defval', '?', 'string', [],
      ['leaf']},

     {'implied', '?', 'identifier', [],
      ['list']},

     {'alias', '*', 'identifier',
      [{'status', '?'},
       {'description', '?'},
       {'reference', '?'},
       {{?SMI_MODULE_NAME, 'oid'}, '1'}],
      ['module', 'submodule']},

     {'oid', '?', 'smi-oid', [],
      ['leaf', 'list', 'container', 'augment', 'notification', 'identity']},

     {'subid', '?', 'non-negative-integer', [],
      ['leaf', 'list', 'container', 'augment', 'notification']}
    ].
