-module(yang_error).

-export([codes/0]).
-export([add_error/4, add_error/5]).
-export([print_errors/2]).
-export([fmt_error/2, fmt_pos/1, fmt_yang_identifier/1]).
-export([print_error_codes/1]).

-include("yang.hrl").

-define(ilf, io_lib:format).

-spec add_error(#yctx{}, yang:pos(), ErrCode :: atom(), Args :: list()) ->
        #yctx{}.
%% @doc Report an error
add_error(Ctx, Pos, ErrCode, Args) ->
    {WarningsAsErrors, _NoPrintWarnings, TreatAsWarning, TreatAsError} =
        Ctx#yctx.warnings,
    Level =
        case lists:keyfind(ErrCode, 1, Ctx#yctx.error_codes) of
            {ErrCode, warning, _Fmt} ->
                if WarningsAsErrors ->
                        %% it's a warning, but treat is all warnings as errors
                        %% except if they are explicitly mentioned
                        case lists:member(ErrCode, TreatAsWarning) of
                            true ->
                                warning;
                            false ->
                                error
                        end;
                   true ->
                        %% it's warning, but treat it as an error if it is
                        %% explicitly mentioned.
                        case lists:member(ErrCode, TreatAsError) of
                            true ->
                                error;
                            false ->
                                warning
                        end
                end;
            _ ->
                error
        end,
    add_error(Level, Ctx, Pos, ErrCode, Args).

add_error(Level, Ctx, Pos, ErrCode, Args) ->
    Ctx#yctx{errors = [#yerror{level = Level,
                               pos = Pos,
                               code = ErrCode,
                               args = Args} | Ctx#yctx.errors]}.


print_errors(Ctx, DoPrintCode) ->
    {_WarningsAsErrors, NoPrintWarnings, _TreatAsWarning, _TreatAsError} =
        Ctx#yctx.warnings,
    F = fun(#yerror{pos = PosA}, #yerror{pos = PosB}) ->
                if element(1, PosA) == element(1, PosB) ->
                        PosA =< PosB;
                   %element(1, PosA) == FileName ->
                   %     true;
                   true ->
                        PosA =< PosB
                end
        end,
    SortedErrors = lists:sort(F, Ctx#yctx.errors),
    lists:foldl(
      fun(E, ErrorsFound) ->
              if NoPrintWarnings andalso E#yerror.level == warning ->
                      ErrorsFound;
                 DoPrintCode ->
                      io:format("~s: ~s\n",
                                [fmt_pos(E#yerror.pos),
                                 E#yerror.code]),
                      ErrorsFound orelse E#yerror.level == error;
                 true ->
                      io:format("~s\n", [fmt_error(Ctx, E)]),
                      ErrorsFound orelse E#yerror.level == error
              end
      end, false, SortedErrors).

fmt_error(#yctx{error_codes = Codes},
          #yerror{level = Level,
                  pos = Pos,
                  code = Code,
                  args = Args}) ->
    ?ilf("~s: ~w: ~s", [fmt_pos(Pos), Level, fmt_code(Codes, Code, Args)]).

fmt_pos({Filename, Line}) ->
    [Filename, $:, ?i2l(Line)];
fmt_pos({Filename, Line, Col}) ->
    [Filename, $:, ?i2l(Line), $:, ?i2l(Col)].

fmt_yang_identifier({ModuleName, Name}) ->
    ?ilf("'~s' in module '~s'", [Name, ModuleName]);
fmt_yang_identifier(Name) ->
    ?ilf("'~s'", [Name]).

fmt_code(Codes, Code, Args) ->
    case lists:keyfind(Code, 1, Codes) of
        {Code,_Level, Fmt} ->
            ?ilf(Fmt, Args);
        false ->
            ?ilf("unknown error code: ~s ~p", [Code, Args])
    end.

codes() ->
    [
     {'YANG_ERR_CIRCULAR_DEPENDENCY', error, "circular dependency for ~s '~s'"},
     {'YANG_ERR_MODULE_NOT_FOUND', error, "module '~s' not found"},
     {'YANG_ERR_MODULE_REV_NOT_FOUND', error,
      "module '~s', revision '~p' not found"},
     {'YANG_ERR_MODULENAME_MISMATCH', error,
      "expected module name '~s', got '~s'"},
     {'YANG_ERR_REVISION_MISMATCH', error, "expected revision '~s', got '~s'"},
     {'YANG_ERR_BAD_INCLUDE', error, "expected submodule, found module at ~s"},
     {'YANG_ERR_BAD_BELONGS_TO', error,
      "'~s' includes '~s', but it does not specifiy a correct belongs-to"},
     {'YANG_ERR_DUPLICATE_DEFINITION', error, "~s '~s' already defined at ~s"},
     {'YANG_ERR_DUPLICATE_SCHEMA_NODE', error,
      "schema node '~s' already defined at ~s"},
     {'YANG_ERR_MISSING_INCLUDE', error,
      "submodule '~s' not included by module '~s'"},
     {'YANG_ERR_DEFINITION_NOT_FOUND', error, "~s ~s not found"},
     {'YANG_ERR_PREFIX_NOT_FOUND', error, "prefix '~s' not found"},
     {'YANG_ERR_REVISION_ORDER', warning,
      "revision statements not given in reverse chronological order"},
     {'YANG_ERR_INVALID_CONFIG', error,
      "config true cannot be set when the parent is config false"},
     {'YANG_ERR_INVALID_CONFIG_USES', error,
      "config false uses refers to grouping with config true node"
      " defined at ~s"},
     {'YANG_ERR_REFINE_NOT_FOUND', error,
      "the refinement node ~s is not found"},
     {'YANG_ERR_ILLEGAL_REFINE', error,
      "the statement '~s' cannot be refined in a '~s'"},
     {'YANG_ERR_BAD_REFINE', error, "the statement '~s' cannot be refined"},
     {'YANG_ERR_BAD_AUGMENT_NODE_TYPE', error, "cannot augment ~s node ~s"},
     {'YANG_ERR_NODE_NOT_FOUND', error, "node ~s not found"}
    ].

print_error_codes(#yctx{error_codes = Errors}) ->
    lists:foreach(fun({Code, Level, Fmt}) ->
                          LevelStr = case Level of
                                         error -> "Error:";
                                         warning -> "Warning:"
                                     end,
                          io:format("~-8s ~s\n", [LevelStr, Code]),
                          io:format("Message: ~s\n\n", [Fmt])
                  end, Errors).
