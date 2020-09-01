-module(yang_error).

-export([codes/0]).
-export([build_error/4]).
-export([add_error/2, add_error/4, add_error/5]).
-export([has_errors/1]).
-export([print_error/3, print_errors/2]).
-export([fmt_error/2, fmt_pos/1, fmt_yang_identifier/1, fmt_code/3,
         fmt_keyword/1]).
-export([print_error_codes/1]).

-include_lib("yanger/include/yang.hrl").

-define(ilf, io_lib:format).

-spec add_error(#yerror{}, #yctx{}) -> #yctx{}.
add_error(#yerror{level = none}, Ctx) ->
    Ctx;
add_error(Err, Ctx) ->
    Ctx#yctx{errors = [Err | Ctx#yctx.errors]}.

-spec add_error(#yctx{}, yang:pos(), ErrCode :: atom(), Args :: list()) ->
        #yctx{}.
%% @doc Report an error or warning to the context.
add_error(Ctx, Pos, ErrCode, Args) ->
    Error = build_error(Ctx, Pos, ErrCode, Args),
    add_error(Error, Ctx).

add_error(ExpectedLevel, Ctx, Pos, ErrCode, Args) ->
    Error = build_error(ExpectedLevel, Ctx, Pos, ErrCode, Args),
    add_error(Error, Ctx).

build_error(Ctx, Pos, ErrCode, Args) ->
    ExpectedLevel =
        case lists:keyfind(ErrCode, 1, Ctx#yctx.error_codes) of
            {_ErrCode, warning, _Fmt} ->
                warning;
            _ ->
                error
        end,
    build_error(ExpectedLevel, Ctx, Pos, ErrCode, Args).

build_error(ExpectedLevel, Ctx, Pos, ErrCode, Args) ->
    {WarningsAsErrors, NoPrintWarnings,
     TreatAsNone, TreatAsWarning, TreatAsError} =
        Ctx#yctx.warnings,
    DoTreatAsNone = lists:member(ErrCode, TreatAsNone),
    Level =
        case ExpectedLevel of
            warning when NoPrintWarnings % -W none
                         orelse DoTreatAsNone -> % -w <ErrCode>
                none;
            warning when WarningsAsErrors ->
                %% it's a warning, but treat all warnings as errors
                %% except if they are explicitly mentioned.
                case lists:member(ErrCode, TreatAsWarning) of
                    true ->
                        warning;
                    false ->
                        error
                end;
            warning ->
                %% it's a warning, but treat it as an error if it is
                %% explicitly mentioned.
                case lists:member(ErrCode, TreatAsError) of
                    true ->
                        error;
                    false ->
                        warning
                end;
            _ ->
                ExpectedLevel
        end,
    #yerror{level = Level,
            pos = Pos,
            code = ErrCode,
            args = Args}.

print_error(Ctx, E, DoPrintCode) ->
    print_errors0(Ctx, [E], DoPrintCode).

print_errors(Ctx, DoPrintCode) ->
    F = fun(#yerror{pos = PosA}, #yerror{pos = PosB}) ->
                cmp_pos(PosA, PosB)
        end,
    SortedErrors = lists:sort(F, Ctx#yctx.errors),
    print_errors0(Ctx, SortedErrors, DoPrintCode).

print_errors0(Ctx, Errors, DoPrintCode) ->
    lists:foldl(
      fun(E, ErrorsFound) ->
              if DoPrintCode ->
                      io:format(standard_error, "~s: ~s\n",
                                [fmt_pos(E#yerror.pos),
                                 E#yerror.code]),
                      ErrorsFound orelse E#yerror.level == error;
                 true ->
                      io:format(standard_error, "~s\n", [fmt_error(Ctx, E)]),
                      ErrorsFound orelse E#yerror.level == error
              end
      end, false, Errors).

has_errors(Ctx) ->
    lists:keymember(error, #yerror.level, Ctx#yctx.errors).

cmp_pos({uses, UsesPos, PosA}, {uses, UsesPos, PosB}) ->
    cmp_pos(PosA, PosB);
cmp_pos(PosA, PosB) ->
    get_pos(PosA) =< get_pos(PosB).

get_pos({uses, undefined, Pos}) ->
    get_pos(Pos);
get_pos({uses, UsesPos, _}) ->
    get_pos(UsesPos);
get_pos({F,L}) ->
    {F,L,-1}; % for comparison; use col -1
get_pos(Pos) ->
    Pos.

fmt_error(#yctx{error_codes = Codes},
          #yerror{level = Level,
                  pos = Pos,
                  code = Code,
                  args = Args}) ->
    ?ilf("~s: ~w: ~s", [fmt_pos(Pos), Level, fmt_code(Codes, Code, Args)]).

fmt_pos({uses, undefined, OrigPos}) ->
    fmt_pos(OrigPos);
fmt_pos({uses, UsesPos, OrigPos}) ->
    [fmt_pos(UsesPos), ": (from ", fmt_pos(OrigPos), ")"];
fmt_pos({Filename, Line}) ->
    [Filename, $:, ?i2l(Line)];
fmt_pos({Filename, Line, Col}) ->
    [Filename, $:, ?i2l(Line), $:, ?i2l(Col)].

fmt_keyword({ModuleName, Name}) ->
    ?ilf("~s:~s", [ModuleName, Name]);
fmt_keyword(Name) ->
    ?a2l(Name).

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
    [{'YANG_ERR_BAD_ARGUMENT', error,
      "bad argument value \"~s\", should be of type ~s"}
     , {'YANG_ERR_BAD_ARGUMENT2', error,
        "bad argument value \"~s\": ~s"}
     , {'YANG_ERR_BAD_AUGMENT_NODE_TYPE', error,
        "cannot augment ~s node ~s"}
     , {'YANG_ERR_BAD_AUGMENT_NODE_TYPE2', error,
        "cannot augment a ~s into the ~s node ~s"}
     , {'YANG_ERR_BAD_BELONGS_TO', error,
        "'~s' includes '~s', but it does not specifiy a correct belongs-to"}
     , {'YANG_ERR_BAD_DEFAULT_VALUE', error,
        "the type \"~s\" cannot have a default value"}
     , {'YANG_ERR_BAD_IMPORT', error,
        "expected module, found submodule at ~s"}
     , {'YANG_ERR_BAD_IMPORT_YANG_VERSION', error,
        "a version ~s module cannot import a version ~s module by revision"}
     , {'YANG_ERR_BAD_INCLUDE', error,
        "expected submodule, found module at ~s"}
     , {'YANG_ERR_BAD_INCLUDE_YANG_VERSION', error,
        "cannot include a version ~s submodule in a version ~s module"}
     , {'YANG_ERR_BAD_KEY', error,
        "the key ~s does not reference an existing leaf"}
     , {'YANG_ERR_BAD_LENGTH', error,
        "the length range \"~s\" is not valid for the base type"}
     , {'YANG_ERR_BAD_LENGTH_BOUNDS', error,
        "the length value ~s is not larger than ~s"}
     , {'YANG_ERR_BAD_LENGTH_VALUE', error,
        "the length value ~s is not valid for the base type"}
     , {'YANG_ERR_BAD_PATTERN', error,"invalid pattern: \"~s\""}
     , {'YANG_ERR_BAD_RANGE', error,
        "the range \"~s\" is not valid for the base type"}
     , {'YANG_ERR_BAD_RANGE_BOUNDS', error,
        "the range value ~s is not larger than ~s"}
     , {'YANG_ERR_BAD_RANGE_VALUE', error,
        "the range value ~s is not valid for the base type"}
     , {'YANG_ERR_BAD_RESTRICTION', error,
        "restriction '~s' is not valid for the base type~s"}
     , {'YANG_ERR_BAD_TYPE_IN_UNION', error,
        "the type ~s (defined at ~s) cannot be part of a union"}
     , {'YANG_ERR_MULTIPLE_BASES', error,
        "YANG version 1 does not support multiple 'base' statements"}
     %% FIXME: remove for 1.1
     , {'YANG_ERR_UNSUPPORTED_TYPE_IN_UNION', warning,
        "the type ~s (defined at ~s) is converted to 'string' "
        "when part of a union"}
     , {'YANG_ERR_BAD_UNIQUE_CONFIG', error,
        "some nodes in the unique argument are config and some are not"}
     , {'YANG_ERR_BAD_UNIQUE_LEAF', error,
        "node ~s in the unique argument is not a leaf"}
     , {'YANG_ERR_BAD_UNIQUE_PART', error,
        "node ~s in the unique argument is a ~s; this is not legal"}
     , {'YANG_ERR_BIT_POSITION', error,
        "the position ~w is not an unsigned 32 bit integer"}
     , {'YANG_ERR_CIRCULAR_DEPENDENCY', error,
        "circular dependency for ~s '~s'"}
     , {'YANG_ERR_CIRCULAR_DEPENDENCY_LEAFREF', error,
        "circular dependency for leafref path of ~s ~s"}
     , {'YANG_ERR_DEFAULT_AND_MANDATORY', error,
        "a 'default' value cannot be given when 'mandatory' is \"true\""}
     , {'YANG_ERR_DEFAULT_AND_MIN_ELEMENTS', error,
        "a 'default' value cannot be given when 'min-elements' "
        "is greater than zero"}
     , {'YANG_ERR_DUPLICATE_DEFAULT_VALUE', error,
        "the same default value has already been given at ~s"}
     , {'YANG_ERR_DEFAULT_CASE_NOT_FOUND', error,
        "the default case \"~s\" is not found"}
     , {'YANG_ERR_DEFINITION_NOT_FOUND', error, "~s ~s not found"}
     , {'YANG_ERR_DUPLICATE_BIT_NAME', error,
        "the bit name \"~s\" has already been used for the bits type at ~s"}
     , {'YANG_ERR_DUPLICATE_BIT_POSITION', error,
        "the position \"~s\" has already been used for the bits type at ~s"}
     , {'YANG_ERR_DUPLICATE_DEFINITION', error,
        "~s '~s' already defined at ~s"}
     , {'YANG_ERR_DUPLICATE_ENUM_NAME', error,
        "the enum name \"~s\" has already been used for the enumeration at ~s"}
     , {'YANG_ERR_DUPLICATE_ENUM_VALUE', error,
        "the enum value \"~s\" has already been used for the enumeration at ~s"}
     , {'YANG_ERR_DUPLICATE_SCHEMA_NODE', error,
        "schema node '~s' already defined at ~s"}
     , {'YANG_ERR_DUPLICATE_UNIQUE_LEAF', error,
        "leaf ~s in the unique argument is given multiple times"}
     , {'YANG_ERR_DUPLICATE_NAMESPACE', error,
        "duplicate namespace '~s' found in module '~s'"}
     , {'YANG_ERR_ENUM_VALUE', error,
        "the enum value ~w is not a 32 bit integer"}
     , {'YANG_ERR_ENUM_NAME_MISMATCH', error,
        "the enum ~s is not defined in the base type"}
     , {'YANG_ERR_ENUM_VALUE_MISMATCH', error,
        "the value ~w is different than base type value ~w"}
     , {'YANG_ERR_BIT_NAME_MISMATCH', error,
        "the bit ~s is not defined in the base type"}
     , {'YANG_ERR_BIT_POSITION_MISMATCH', error,
        "the position ~w is different than base type position ~w"}
     , {'YANG_ERR_ILLEGAL_REFINE', error,
        "the statement '~s' cannot be refined in a '~s'"}
     , {'YANG_ERR_INVALID_CONFIG', error,
        "config true when the parent is config false"}
     , {'YANG_ERR_DEREF_NOT_LEAFREF', error,
        "the deref argument refers to non-leafref leaf ~s at ~s"}
     , {'YANG_ERR_LEAFREF_NOT_LEAF', error,
        "the leafref refers to non-leaf and non-leaf-list node ~s at ~s"}
     , {'YANG_ERR_LEAFREF_NOT_LEAF0', error,
        "the leafref refers to non-leaf node ~s at ~s"}
     , {'YANG_ERR_LEAFREF_BAD_CONFIG', error,
        "the node is config, but refers to a "
        "non-config leaf ~s defined at ~s"}
     , {'YANG_ERR_MANDATORY_NODE_IN_AUGMENT', error,
        "augmenting a mandatory node is illegal"}
     , {'YANG_ERR_MANDATORY_NODE_IN_DEFAULT_CASE', error,
        "the default case cannot have mandatory nodes"}
     , {'YANG_ERR_MISSING_INCLUDE', error,
        "submodule '~s' not included by module '~s'"}
     , {'YANG_ERR_MISSING_TYPE_SPEC', error,
        "a type ~s must have at least one ~s statement"}
     , {'YANG_ERR_MISSING_TYPE_SPEC_1', error,
        "a type ~s must have a ~s statement"}
     , {'YANG_ERR_MODULENAME_MISMATCH', error,
        "expected module name '~s', got '~s'"}
     , {'YANG_ERR_MODULE_NOT_FOUND', error,"module '~s' not found"}
     , {'YANG_ERR_MODULE_REV_NOT_FOUND', error,
        "module '~s', revision '~s' not found"}
     , {'YANG_ERR_NEED_KEY', error,
        "the list needs at least one key"}
     , {'YANG_ERR_NEED_KEY_USES', error,
        "the list needs at least one key, because it is used as config"}
     , {'YANG_ERR_NODE_NOT_FOUND', error,"node ~s not found"}
     , {'YANG_ERR_NODE_NOT_FOUND2', error,
        "the node ~s from module '~s' is not found"}
     , {'YANG_ERR_NODE_NOT_FOUND3', error,
        "the node ~s from module '~s' (in node ~s from '~s') is not found"}
     , {'YANG_ERR_REFER_TOP_NODE', error, "the path refers to top node"}
     , {'YANG_ERR_PREFIX_MOD_MISMATCH', error,
        "prefix '~s' conflicts with prefix ~s"}
     , {'YANG_ERR_PREFIX_NOT_FOUND', error,"prefix '~s' not found"}
     , {'YANG_ERR_REFINE_NOT_FOUND', error,
        "the refinement node ~s is not found"}
     , {'YANG_ERR_REVISION_MISMATCH', error,
        "expected revision '~s', got '~s'"}
     , {'YANG_ERR_REVISION_ORDER', warning,
        "the revision statements are not given in reverse chronological order"}
     , {'YANG_ERR_TOO_MANY_UP', error,"the path has too many '..'"}
     , {'YANG_ERR_TYPE_EMPTY_IN_KEY', error,
        "the key leaf '~s' cannot have type empty"}
     , {'YANG_ERR_TYPE_EMPTY_IN_LEAF_LIST', error,
        "a leaf-list cannot have type empty"}
     , {'YANG_ERR_TYPE_VALUE', error,
        "the value \"~s\" is not valid for the base type - ~s"}
     , {'YANG_ERR_UNIQUE_IS_KEY', warning,
        "all keys in the list are redundantly present in the unique statement"}
     , {'YANG_ERR_KEY_BAD_SUBSTMT', error,
        "the statement '~s' cannot be given for a key"}
     , {'YANG_ERR_KEY_HAS_DEFAULT', warning,
        "the default value for a key leaf is ignored"}
     , {'YANG_ERR_XPATH_BAD_REF', warning,
        "The XPath expression references an undefined node: ~s"}
     , {'YANG_ERR_XPATH_REF_BAD_CONFIG', warning,
        "the node is config, but refers to a "
        "non-config node ~s defined at ~s"}
     , {'YANG_ERR_XPATH', error,"XPath error: ~s"}
     , {'YANG_ERR_XPATH_FAIL', error,
        "The XPath expression will always fail"}
     , {'YANG_ERR_EXPECTED_DATA_DEF', error,
        "expected a data definition statement as child to '~s'"}
     , {'YANG_ERR_BAD_DEVIATION', error,
        "the '~s' property is not valid for a ~s"}
     , {'YANG_ERR_BAD_DEVIATE_ADD', error,
        "the '~s' property already exists in the target node"}
     , {'YANG_ERR_BAD_DEVIATE_REPLACE', error,
        "the '~s' property can not be replaced"}
     , {'YANG_ERR_BAD_DEVIATE_REPLACE_NOT_FOUND', error,
        "the '~s' property does not exist in the target node"}
     , {'YANG_ERR_BAD_DEVIATE_DELETE', error,
        "the '~s' property with argument '~s' "
        "does not exist in the target node"}
     , {'YANG_BAD_STATUS_REFERENCE', error,
       'the "~s" definition is ~s, but the "~s" it references is ~s'}
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
