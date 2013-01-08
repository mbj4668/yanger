-module(yang_parser).

-export([parse/1,a/0]).
-export([install_arg_types/1]).
-export([install_grammar/2]).
-on_load(init/0).

a() ->
    yang_parser:parse("big.yang"),
    ok.

-type erl_type() :: 'string'
                  | 'atom'
                  | 'int'
                  | 'atom-or-int'
                  | 'identifier-ref'. % {Modulename :: atom(), Name :: atom()}

init() ->
    Nif = filename:join(code:priv_dir(epyang),"yang_parser_nif"),
    ok = erlang:load_nif(Nif, 0).

-spec install_arg_types([{TypeName :: atom(),
                          XsdRegexp :: string() | undefined,
                          ErlType :: erl_type()}]) ->
          ok | error.
%% @doc Install type definitions for YANG extension statetement's arguments.
%% XsdRegexp 'undefined' means that any string is accepted.
install_arg_types(_Types) ->
    exit(nif_library_not_loaded).

install_grammar(_Module, _Rules) ->
    exit(nif_library_not_loaded).

parse(_X) ->
    exit(nif_library_not_loaded).
