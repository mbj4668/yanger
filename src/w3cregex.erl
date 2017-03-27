%%% ------------------------------------------------------------------------
%%% @copyright 2012 Tail-f Systems AB
%%% @doc NIF implementation of W3C regular expressions
%%% ------------------------------------------------------------------------
-module(w3cregex).

-export([compile/1, match/2, cmatchp/2]).
-export([string/1, is_xreg/1]).

-on_load(on_load/0).

-opaque xreg() :: <<>>.
-type binstring() :: iolist() | binary().

-export_type([xreg/0]).


-spec cmatchp(Pattern::binstring(), String::binstring()) ->
                     boolean() | {'error', string()}.
cmatchp(Pattern, String) ->
    case compile(Pattern) of
        {ok, XReg} ->
            match(XReg, String);
        Err ->
            Err
    end.


-spec compile(Pattern::binstring()) -> {'ok', xreg()} | {'error', string()}.
compile(_Pattern) ->
    nif_only().

-spec match(xreg(), binstring()) -> boolean() | {'error', string()}.
match(CompiledRegex, String) when is_binary(String), (size(String) < 64) ->
    run_match(CompiledRegex, String);
match(CompiledRegex, String) ->
    run_match_null(CompiledRegex, [String, 0]).

run_match(_CompiledRegex, _String) ->
    nif_only().

run_match_null(_CompiledRegex, _String) ->
    nif_only().


-spec is_xreg(term()) -> boolean().
%% @doc Returns true if term is a compiled regular expression (as
%% returned by compile())
is_xreg(_CompiledRegex) ->
    nif_only().


-spec string(xreg()) -> string().
string(_CompiledRegex) ->
    nif_only().


on_load() ->
%Sligtly ugly if we do not return ok module_info does not work and
%Also cover does not handle internal calls there for no internal function
%calls here please!
    case os:type() of
        {win32, _} ->
            ok;
        _ ->
            case code:priv_dir(yanger) of
                {error,_} ->
                    ok;
                PrivDir ->
                    File = filename:append(PrivDir, "w3cregex_nif"),
                    erlang:load_nif(File, 0)
            end
    end.

nif_only() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).
