-module(yang_parser).

-export([install_arg_types/1]).
-export([install_grammar/2]).
-export([get_grammar_module_names/0, get_statement_spec/1]).
-export([parse/2]).

-on_load(init/0).

-export_type([arg_erl_type/0, yang_statement_spec/0]).

-type arg_erl_type() :: 'string'
                      | 'atom'
                      | 'int'
                      | 'atom-or-int'
                      | 'identifier-ref'.
%% @doc The erlang type that the parser will use for parsed arguments.
%% An 'identifier-ref' argument is returned as
%% {Prefix :: atom(), Name :: atom()}.

-type occurence() :: '?' | '1' | '*' | '+'.

-type yang_statement_spec() ::
        {Keyword :: atom(),
         ArgTypeName :: atom() | [],
         Rules :: [{Substmt :: yang:keyword(), Occurence :: occurence()}],
         UseIn :: {OccurenceWhenUsed :: occurence(), [yang:keyword()]}
                | undefined
        }.

-type error() :: {Code :: integer(), FName :: string(),
                  LineNo :: integer(), ColumnNo :: integer(),
                  Str :: string()}.

%% @doc A grammar specification for the YANG extension statement Keyword.
%% The ArgTypeName is either one of the built-in types (see
%% yang_core_grammar.c), or installed by a call to install_arg_types().
-spec install_arg_types(
        [{ArgTypeName :: atom(),
          XsdRegexp :: string() | undefined,
          ErlType :: arg_erl_type()}]) ->
        ok | error.
%% @doc Install type definitions for YANG extension statements's arguments.
%% XsdRegexp is used to make the parser validate the argument.  'undefined'
%% means that any string is accepted.
%%
%% The parser converts each argument of type ArgTypeName to the erlang
%% type in specified in ErlType.
install_arg_types(_Types) ->
    erlang:nif_error(nif_library_not_loaded).

-spec install_grammar(ModuleName :: atom(), [yang_statement_spec()]) ->
        ok | error.
%% @doc Install grammar for YANG extension statements for a particular module.
install_grammar(_ModuleName, _Specs) ->
    erlang:nif_error(nif_library_not_loaded).

-spec parse(FileName :: string(), Canonical :: boolean()) ->
        {ok, [yang:stmt()], [Warnings :: error()]}
      | {error, [error()]}.
%% Returns {error, Errors} if non-recoverable errors are found.
parse(_FileName, _Canonical) ->
    erlang:nif_error(nif_library_not_loaded).

-spec get_grammar_module_names() -> [ModuleName :: atom()].
%% Returns the module names for which grammar has been installed.
get_grammar_module_names() ->
    erlang:nif_error(nif_library_not_loaded).

-spec get_statement_spec(Keyword :: yang:keyword()) ->
        {value, yang_statement_spec()} | not_found.
%% @doc Returns the yang_statement_spec() for a given keyword.
%% The 'UseIn' field the in yang_statement_spec() is always 'undefined',
%% since the spec is fully expanded (e.g., 'container' will have
%% smiv2:oid as substmt, if the smiv2 grammar is loaded.)
get_statement_spec(_Keyword) ->
    erlang:nif_error(nif_library_not_loaded).

%%% Internal functions

init() ->
    Nif = filename:join(code:priv_dir(yanger),"yang_parser_nif"),
    ok = erlang:load_nif(Nif, 0).

