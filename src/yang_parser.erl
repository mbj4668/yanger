-module(yang_parser).

-export([parse/1,a/0]).
-export([install_arg_types/1, get_arg_type/1]).
-export([install_grammar/2]).
-on_load(init/0).

a() ->
    yang_parser:parse("big.yang"),
    ok.

init() ->
    Nif = filename:join(code:priv_dir(epyang),"yang_parser_nif"),
    ok = erlang:load_nif(Nif, 0).

get_arg_type(_Types) ->
    exit(nif_library_not_loaded).

install_arg_types(_Types) ->
    exit(nif_library_not_loaded).

install_grammar(_Module, _Rules) ->
    exit(nif_library_not_loaded).

parse(_X) ->
    exit(nif_library_not_loaded).
