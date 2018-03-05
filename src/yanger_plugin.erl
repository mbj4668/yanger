%%%-------------------------------------------------------------------
%%% @doc Yanger Plugin API
%%% A Yanger Plugin is defined using the 'yanger_plugin' behavior.
%%% This behavior defines a set of callback functions that the plugin
%%% implements.
%%%
%%% This module contains the API functions a plugin should use to
%%% interact with Yanger.  The header file "yang.hrl" is also
%%% considered part of the API.  Specifically, "yang.hrl"
%%% specifies a set of records and types used by Yanger and the
%%% plugins.
%%%
%%% A plugin can register command-line options, output formats,
%%% YANG grammar, YANG types, and various hooks.
%%%
%%% All YANG processing is done in a single erlang process.  State
%%% is passed in the #yctx{}.  Plugins can use the .pmap field in
%%% the #yctx{} and other records to store plugin-specfic state.
%%% The .pmap structure can be modified with the yang:map_* functions.
%%%-------------------------------------------------------------------
-module(yanger_plugin).
-export([install_arg_types/1, install_grammar/2]).
-export([register_option_specs/2, register_output_format/3,
         register_output_format/4,
         register_transform/3,
         register_error_codes/2, register_data_definition_stmt/3,
         register_copy_from_grouping_stmt/2,
         register_refinement_fun/3,
         register_deviate/4,
         register_hook/3, register_conditional_hook/4]).
-export([add_error/4]).

-export_type([emit_fun/0, transform_fun/0]).

-include_lib("yanger/include/yang.hrl").

%% @doc Initialize the plugin.
%% This function is called to let the plugin register its options,
%% grammar, etc.  Note that the Ctx is not yet completely initialized.
-callback init(#yctx{}) -> #yctx{}.


-spec install_arg_types(
        [{ArgTypeName :: atom(),
          XsdRegexp :: string() | undefined,
          ErlType :: yang_parser:arg_erl_type()}]) ->
        ok | error.
%% @doc Install type definitions for YANG extension statements's arguments.
%% Must be called from the plugin's init() function, or in a post_init_ctx
%% hook.
%%
%% XsdRegexp is used to make the parser validate the argument.  'undefined'
%% means that any string is accepted.
%%
%% The parser converts each argument of type ArgTypeName to the erlang
%% type in specified in ErlType.
install_arg_types(Types) ->
    yang_parser:install_arg_types(Types).

-spec install_grammar(ModuleName :: atom(),
                      [yang_parser:yang_statement_spec()]) ->
        ok | error.
%% @doc Install grammar for YANG extension statements for a particular module.
%% Must be called from the plugin's init() function, or in a post_init_ctx
%% hook.
install_grammar(ModuleName, Specs) ->
    yang_parser:install_grammar(ModuleName, Specs).

-spec register_option_specs(#yctx{}, [getopt:option_spec()]) ->
                                   #yctx{}.
%% @doc Register command line options needed by the plugin.
%% Must be called from the plugin's init() function.
register_option_specs(Ctx, OptSpecs) ->
    Ctx#yctx{option_specs = Ctx#yctx.option_specs ++ OptSpecs}.

-type emit_fun() :: fun((#yctx{}, [#module{}], io:device()) -> [#yerror{}]).

-spec register_output_format(#yctx{}, FmtName::atom(), emit_fun()) -> #yctx{}.
%% @doc Register the output format implemented by the plugin.
%% Must be called from the plugin's init() function.
register_output_format(Ctx, Fmt, EmitFun) ->
    register_output_format(Ctx, Fmt, false, EmitFun).

-spec register_output_format(#yctx{}, FmtName::atom(),
                             AllowErrors :: boolean(), emit_fun()) -> #yctx{}.
%% @doc Register the output format implemented by the plugin.
%% Must be called from the plugin's init() function.
register_output_format(Ctx, Fmt, AllowErrors, EmitFun) ->
    Ctx#yctx{fmts = [{Fmt, AllowErrors, EmitFun} | Ctx#yctx.fmts]}.

-type transform_fun() :: fun ((#yctx{}, [#module{}]) -> [#module{}]).
-spec register_transform(#yctx{}, FmtName::atom(), transform_fun()) -> #yctx{}.
%% @doc Register a transform implemented by the plugin.
%% Must be called from the plugin's init() function.
register_transform(Ctx, Name, TransfFun) ->
    Ctx#yctx{transforms = Ctx#yctx.transforms ++ [{Name, TransfFun}]}.

-spec register_error_codes(
        #yctx{},
        [{ErrCode :: atom(),
          Level :: 'error' | 'warning',
          Fmt :: string()}]) -> #yctx{}.
%% @doc Register a plugin-specific error code.
%% Must be called from the plugin's init() function.
register_error_codes(Ctx, ErrorCodes) ->
    Ctx#yctx{error_codes = ErrorCodes ++ Ctx#yctx.error_codes}.

-spec register_data_definition_stmt(
        #yctx{},
        yang:keyword(),
        Kind :: atom()) -> #yctx{}.
%% @doc Registers an extension statement as a data definition statement.
%% This means that the validation code builds #sn{} records of this
%% statement.
%% Must be called from the plugin's init() function.
%% NOTE: use with care - normally extensions should NOT define new data
%% definition statements!
register_data_definition_stmt(Ctx, Keyword, Kind) ->
    Env0 = Ctx#yctx.env,
    Env1 = Env0#env{data_definition_stmts =
                    yang:map_insert(Keyword, Kind,
                                    Env0#env.data_definition_stmts)},
    Ctx#yctx{env = Env1}.

%% @doc Instructs the compiler to copy any occurances of the Keyword
%% extension statement in a grouping to the statement that uses the
%% grouping.
register_copy_from_grouping_stmt(Ctx, Keyword) ->
    Env0 = Ctx#yctx.env,
    Env1 = Env0#env{copy_from_grouping_stmts =
                    yang:map_insert(Keyword, true,
                                    Env0#env.copy_from_grouping_stmts)},
    Ctx#yctx{env = Env1}.

-spec register_refinement_fun(
        #yctx{},
        Keyword :: yang:keyword(),
        fun((yang:stmt(), {#sn{}, #yctx{}}) -> {#sn{}, #yctx{}})) ->
                                 #yctx{}.
%% @doc Add a special function for a refinement.
%% Normally, nothing needs to be done to handle refinements.  If the
%% grammar allows refinement, the compiler will handle it.  But in some
%% cases the #sn{} needs special treatment after a refinement.  This
%% function registers a callback function that the compiler will invoke
%% in this case.
register_refinement_fun(Ctx, Keyword, F) ->
    Env0 = Ctx#yctx.env,
    Env1 = Env0#env{refinements =
                    yang:map_insert(Keyword, F, Env0#env.refinements)},
    Ctx#yctx{env = Env1}.

-spec register_deviate(
        #yctx{},
        Keyword :: yang:keyword(),
        IsSingleton :: boolean(),
        AllowedParents :: [yang:keyword()]) ->
                              #yctx{}.
%% @doc Allows Keyword to be used in deviate.
register_deviate(Ctx, Keyword, IsSingleton, AllowedParents) ->
    Env0 = Ctx#yctx.env,
    Env1 = Env0#env{deviates =
                        yang:map_insert(Keyword, {IsSingleton, AllowedParents},
                                        Env0#env.deviates)},
    Ctx#yctx{env = Env1}.

-spec register_hook(
        #yctx{},
        HookField :: integer(),
        yang:hookfun()) -> #yctx{}.
%% @doc Register a hook function.
%% Must be called from the plugin's init() function.
register_hook(Ctx, HookField, Fun) ->
    Hooks = Ctx#yctx.hooks,
    Funs = element(HookField, Hooks),
    case lists:member(Fun, Funs) of
        false ->
            NewHooks = setelement(HookField, Hooks, [Fun | Funs]),
            Ctx#yctx{hooks = NewHooks};
        true ->
            Ctx
    end.

-spec register_conditional_hook(
        #yctx{},
        HookField :: integer(),
        Conditions :: [primary |
                       {imports, ModuleName :: atom()} |
                       {imports_any, [ModuleName :: atom()]}],
        yang:hookfun()) -> #yctx{}.
%% @doc Register a conditional hook function.
%% The following hooks can be registered with this function:
%%    #hooks.mk_sn
%% Must be called from the plugin's init() function.
register_conditional_hook(Ctx, HookField, Conditions, Fun) ->
    Hooks = Ctx#yctx.hooks,
    Funs = element(HookField, Hooks),
    case lists:member({Conditions, Fun}, Funs) of
        false ->
            NewHooks = setelement(HookField, Hooks,
                                  [{Conditions, Fun} | Funs]),
            Ctx#yctx{hooks = NewHooks};
        true ->
            Ctx
    end.

-spec add_error(#yctx{},
                ErrPos :: yang:pos()
                        | {UsesPos :: yang:pos(), OrigPos :: yang:pos()},
                ErrCode :: atom(),
                Args :: list()) ->
        #yctx{}.
%% @doc Report an error.
%% This function can be called during module processing to report an
%% error or warning to the context.
%% ErrCode should be registered  by a called to
%% yanger_plugin:register_error_code() by this plugin, or optionally
%% a code registered by some other plugin or one of the built-in error codes.
add_error(Ctx, ErrPos, ErrCode, Args) ->
    yang_error:add_error(Ctx, ErrPos, ErrCode, Args).
