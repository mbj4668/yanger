%%%-------------------------------------------------------------------
%%% @doc Yanger SMIv2 Plugin
%%% Verifies SMIv2 YANG statements as defined in RFC 6643.
%%% Implements a strict interpretation of the RFC 6643 rules.  For
%%% example, does not allow a smiv2:subid if an ancestor has a
%%% smiv2:oid or smiv2:subid statement, like pyang does.
%%%-------------------------------------------------------------------
-module(yanger_smiv2).
-behaviour(yanger_plugin).

-export([init/1]).

-import(yanger_plugin, [add_error/4]).

-include_lib("yanger/include/yang.hrl").

-define(SMI_MODULE_NAME, 'ietf-yang-smiv2').

init(Ctx) ->
    yanger_plugin:install_arg_types(arg_types()),
    yanger_plugin:install_grammar(?SMI_MODULE_NAME, grammar()),
    Ctx1 = yanger_plugin:register_error_codes(
             Ctx,
             [{'SMIv2_ERR_SUBID', error,
               "smiv2:subid needs a parent with an smiv2:oid or smiv2:subid"},
              {'SMIv2_ERR_DUPLICATE_OID', error,
               "OID ~s already defined at ~s"},
              {'SMIv2_ERR_SUBID_AND_OID', error,
               "smiv2:subid and smiv2:oid cannot be given at the same time"}]),
    Ctx2 = yanger_plugin:register_conditional_hook(
             Ctx1, #hooks.pre_mk_sn, [{imports, ?SMI_MODULE_NAME}],
             fun pre_mk_sn/5),
    Ctx3 = yanger_plugin:register_hook(
             Ctx2, #hooks.post_init_ctx, fun post_init_ctx/1),
    yanger_plugin:register_option_specs(Ctx3, option_specs()).

option_specs() ->
    [{"SMIv2 output specific options:",
      [{smiv2_dup_oids, undefined, "smiv2-detect-duplicate-oids", boolean,
        "Detect and give error for duplicate OIDs"}]}].

post_init_ctx(#yctx{pmap = PMap} = Ctx) ->
    %% Keep a dict of all OIDs, in order to detect duplicates.
    case proplists:get_value(smiv2_dup_oids, Ctx#yctx.options, false) of
        false ->
            Ctx;
        true ->
            Ctx#yctx{pmap = yang:map_insert(smiv2, dict:new(), PMap)}
    end.

arg_types() ->
    [
     {'smi-oid',
      "(([0-1](\\.[1-3]?[0-9]))|(2\\.(0|([1-9]\\d*))))(\\.(0|([1-9]\\d*)))*",
      string},
     {'smi-max-access',
      "not-accessible|accessible-for-notify|read-only|read-write|read-create",
      atom}
    ].

grammar() ->
    %% {<keyword>,<argument type name | []>,
    %%  <substmts>,
    %%  {<occurance when used>, <list of keywords where <keyword> can occur>}}
    [
     {'display-hint', 'string', [],
      {'?', ['leaf', 'typedef']}},

     {'max-access', 'smi-max-access', [],
      {'?', ['leaf', 'typedef']}},

     {'defval', 'string', [],
      {'?', ['leaf']}},

     {'implied', 'identifier', [],
      {'?', ['list']}},

     {'alias', 'identifier',
      [{'status', '?'},
       {'description', '?'},
       {'reference', '?'},
       {{?SMI_MODULE_NAME, 'oid'}, '1'}],
      {'*', ['module', 'submodule']}},

     {'oid', 'smi-oid', [],
      {'?',
       ['leaf', 'list', 'container', 'augment', 'notification', 'identity']}},

     {'subid', 'non-negative-integer', [],
      {'?', ['leaf', 'list', 'container', 'augment', 'notification']}}
    ].

pre_mk_sn(Ctx, Sn, final, UsesPos, Ancestors) ->
    case yang:search_one_substmt({?SMI_MODULE_NAME, 'oid'}, Sn#sn.stmt) of
        {_, OIDStr, Pos, _} ->
            ErrPos = {uses, UsesPos, Pos},
            case
                yang:search_one_substmt({?SMI_MODULE_NAME, 'subid'}, Sn#sn.stmt)
            of
                {_, _, _, _} ->
                    {add_error(Ctx, ErrPos, 'SMIv2_ERR_SUBID_AND_OID', []), Sn};
                _ ->
                    OID = parse_oid(OIDStr),
                    {set_oid_in_ctx(Ctx, OID, ErrPos), set_oid_in_sn(Sn, OID)}
            end;
        false ->
            case
                yang:search_one_substmt({?SMI_MODULE_NAME, 'subid'}, Sn#sn.stmt)
            of
                {_, SubId, Pos, _} ->
                    ErrPos = {uses, UsesPos, Pos},
                    case Ancestors of
                        [#sn{pmap = ParentPMap} | _] ->
                            case yang:map_lookup(smiv2_oid, ParentPMap) of
                                {value, ParentOID} ->
                                    OID = ParentOID ++ [SubId],
                                    {set_oid_in_ctx(Ctx, OID, ErrPos),
                                     set_oid_in_sn(Sn, OID)};
                                none ->
                                    {add_error(Ctx, ErrPos,
                                               'SMIv2_ERR_SUBID', []),
                                     Sn}
                            end;
                        _ ->
                            {add_error(Ctx, ErrPos, 'SMIv2_ERR_SUBID', []), Sn}
                    end;
                _ ->
                    {Ctx, Sn}
            end
    end;
pre_mk_sn(Ctx, Sn, _Mode, _UsesPos,  _Ancestors) ->
    {Ctx, Sn}.

set_oid_in_sn(#sn{pmap = Pmap} = Sn, OID) ->
    Sn#sn{pmap = yang:map_insert(smiv2_oid, OID, Pmap)}.

set_oid_in_ctx(#yctx{pmap = Pmap} = Ctx, OID, ErrPos) ->
    case yang:map_lookup(smiv2, Pmap) of
        none -> % oid duplication disabled
            Ctx;
        {value, Dict} ->
            case dict:is_key(OID, Dict) of
                false ->
                    Dict1 = dict:store(OID, ErrPos, Dict),
                    Ctx#yctx{pmap = yang:map_update(smiv2, Dict1, Pmap)};
                true ->
                    OtherPos = dict:fetch(OID, Dict),
                    add_error(Ctx, ErrPos, 'SMIv2_ERR_DUPLICATE_OID',
                              [fmt_oid(OID), yang_error:fmt_pos(OtherPos)])
            end
    end.

%% Parse <<"1.3.6.1.1">>  into [1,3,6,1,1]
parse_oid(S) ->
    lists:map(fun(I) -> list_to_integer(I) end,
              re:split(S, <<"\\.">>, [{return, list}])).

fmt_oid(OID) ->
    string:join([integer_to_list(SubId) || SubId <- OID], ".").
