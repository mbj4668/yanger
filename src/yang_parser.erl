-module(yang_parser).

-export([install_arg_types/1]).
-export([install_grammar/2]).
-export([parse/2]).

-on_load(init/0).

% dbg
-export([a/0]).
a() ->
    yang_parser:parse("big.yang", false),
    ok.

-type return_type() :: 'string'
                     | 'atom'
                     | 'int'
                     | 'atom-or-int'
                     | 'identifier-ref'.
%% @doc The erlang type that the parser will use for parsed arguments.
%% An 'idenitfier-ref' argument is returned as
%% {Modulename :: atom(), Name :: atom()}.

-type yang_statement_spec() ::
        {Keyword :: atom(), ArgTypeName :: atom() | [],
         [{Substmt :: yang:keyword(), Occurance :: '?' | '1' | '*' | '+'}]}.
%% @doc A grammar specification for the YANG extension statement Keyword.
%% The ArgTypeName is either one of the built-in types (see
%% yang_core_grammar.c), or installed by a call to install_arg_types().

-spec install_arg_types(
        [{ArgTypeName :: atom(),
          XsdRegexp :: string() | undefined,
          ErlType :: return_type()}]) ->
        ok | error.
%% @doc Install type definitions for YANG extension statements's arguments.
%% XsdRegexp is used to make the parser validate the argument.  'undefined'
%% means that any string is accepted.
%%
%% The parser converts each argument of type ArgTypeName to the erlang
%% type in specified in ErlType.
install_arg_types(_Types) ->
    exit(nif_library_not_loaded).

-spec install_grammar(ModuleName :: atom(), [yang_statement_spec()]) ->
        ok | error.
%% @doc Install grammar for YANG extension statements for a particular module.
install_grammar(_ModuleName, _Specs) ->
    exit(nif_library_not_loaded).

-spec parse(FileName :: string(), Canonical :: boolean()) ->
        {ok, [yang:stmt()]}
      | {error, [{Code :: integer(), FName :: string(),
                  LineNo :: integer(), Offset :: integer(), Str :: string()}]}.
parse(_FileName, _Canonical) ->
    exit(nif_library_not_loaded).

%%% Internal functions

init() ->
    Nif = filename:join(code:priv_dir(yanger),"yang_parser_nif"),
    ok = erlang:load_nif(Nif, 0).

