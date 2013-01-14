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
%%%-------------------------------------------------------------------
-module(yanger_plugin).
-export([behaviour_info/1]).
-export([install_arg_types/1, install_grammar/2]).
-export([register_option_specs/2, register_output_format/3,
         register_error_code/4]).
-export([add_error/4]).

-include("yang.hrl").

behaviour_info(callbacks) ->
    [
     %% -spec init(#yctx{}) -> #yctx{}
     %% @doc Initialize the plugin.
     %% This function is called to let the plugin register its options,
     %% grammar, etc.  Note that the Ctx is not yet completely initialized.
     {init, 1},
     %% -spec emit(#yctx{}, [#module{}], Fd :: file:io_device()) -> ??
     {emit, 3}
    ];
behaviour_info(_) ->
    undefined.

-spec install_arg_types(
        [{ArgTypeName :: atom(),
          XsdRegexp :: string() | undefined,
          ErlType :: yang_parser:return_type()}]) ->
        ok | error.
%% @doc Install type definitions for YANG extension statements's arguments.
%% Must be called from the plugin's init() function.
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
install_grammar(ModuleName, Specs) ->
    yang_parser:install_grammar(ModuleName, Specs).

-spec register_option_specs(#yctx{}, [getopt:option_spec()]) -> #yctx{}.
%% @doc Register command line options needed by the plugin.
%% Must be called from the plugin's init() function.
register_option_specs(Ctx, OptSpecs) ->
    Ctx#yctx{option_specs = Ctx#yctx.option_specs ++ OptSpecs}.

%% @doc Register the output format implemented by the plugin.
%% Must be called from the plugin's init() function.
register_output_format(Ctx, Fmt, PluginModule) ->
    Ctx#yctx{fmts = [{Fmt, PluginModule} | Ctx#yctx.fmts]}.

-spec register_error_code(
        #yctx{},
        ErrCode :: atom(),
        Level :: 'error' | 'warning',
        Fmt :: string()) -> #yctx{}.
%% @doc Register an error code.
%% Must be called from the plugin's init() function.
register_error_code(Ctx, ErrCode, Level, Fmt) ->
    Ctx#yctx{error_codes = [{ErrCode, Level, Fmt} | Ctx#yctx.error_codes]}.

-spec add_error(#yctx{}, yang:pos(), ErrCode :: atom(), Args :: list()) ->
        #yctx{}.
%% @doc Report an error
add_error(Ctx, Pos, ErrCode, Args) ->
    yang_error:add_error(Ctx, Pos, ErrCode, Args).
