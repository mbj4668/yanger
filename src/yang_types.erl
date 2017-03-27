-module(yang_types).

-export([register_builtin_types/1,
         register_type/3,
         lookup_type/2,
         mk_type_spec/4,
         mk_default/4,
         parse_value/4,
         parse_range/6,
         is_in_range/2
        ]).

-export([validate_leafref_path/5,
         xpath_to_extended_leafref_path/1, xpath_to_leafref_path/1,
         follow_leafref_path/3, follow_leafref_path/4,
         follow_leafref_path_int/3, follow_leafref_path_int/4]).

-include_lib("yanger/include/yang.hrl").

-import(yang_error, [add_error/4]).

-export_type([type_spec/0, type_spec_fun/0, max_length_type/0,
              extended_leafref_path/0]).

%% #integer_type_spec{} | #string_type_spec{} | ... | plugin-defined
-type type_spec() :: term().

-type type_spec_fun() ::
        fun(({derive, yang:stmt()} | {parse, binary(), yang:pos()},
             #type{}, #module{}, #yctx{}) ->
                   {type_spec(), #yctx{}}                 % derive
                       | {'ok', term()}                   % parse ok
                       | {'error', iodata()}              % parse error
                       | {'error', iodata(), yang:pos()}  % parse error
                       | {term(), #yctx{}}                % parse ok
                       | {undefined, #yctx{}}).           % parse ok

-type max_length_type() :: integer() | 'infinity'.

-type range() :: [{Min :: term(), Max :: term()} | term()].

-type name_and_value() :: {atom(), integer()}.

-record(nvdata, {
          keep = true :: boolean(),
          name :: atom(),
          npos :: yang:pos(),
          value :: integer(),
          vpos :: yang:pos(),
          stmt :: yang:stmt()
         }).

-define(INT32_MIN, -2147483648).
-define(INT32_MAX, 2147483647).
-define(UINT32_MAX, 4294967295).
-define(INT64_MIN, -9223372036854775808).
-define(INT64_MAX, 9223372036854775807).

-spec register_type(TypeName :: yang:builtin_type_name()
                              | {ModuleName :: atom(), TypeName :: atom()},
                    #type{},
                    #yctx{}) -> #yctx{}.
register_type(TypeName, Type, Ctx) ->
    TypeMap = yang:map_insert(TypeName, Type, Ctx#yctx.typemap),
    Ctx#yctx{typemap = TypeMap}.

-spec lookup_type(TypeName :: yang:builtin_type_name()
                            | {ModuleName :: atom(), TypeName :: atom()},
                  #yctx{}) -> {value, #type{}} | none.
lookup_type(TypeName, Ctx) ->
    yang:map_lookup(TypeName, Ctx#yctx.typemap).


-spec mk_type_spec(TypeS :: yang:stmt(),
                   #type{} | #typedef{} | [#type{}],
                   #module{} | 'undefined',
                   #yctx{}) -> {type_spec_fun(), type_spec(), #yctx{}}.
mk_type_spec(TypeS, #typedef{type = Type}, M, Ctx) ->
    mk_type_spec(TypeS, Type, M, Ctx);
mk_type_spec(_TypeS, #type{type_spec_fun = undefined}, _M, Ctx) ->
    %% earlier derivation failed
    {undefined, undefined, Ctx};
mk_type_spec(TypeS, #type{type_spec_fun = BaseTypeSpecF} = BaseType, M, Ctx) ->
    %% Apply restrictions
    {TypeSpec, Ctx1} = BaseTypeSpecF({derive, TypeS}, BaseType, M, Ctx),
    {BaseTypeSpecF, TypeSpec, Ctx1};
mk_type_spec(TypeS, TypeList, M, Ctx) when is_list(TypeList) ->
    %% special case for union - we need the types passed in
    {value, BaseType} = lookup_type('union', Ctx),
    BaseTypeSpec = #union_type_spec{types = TypeList},
    mk_type_spec(TypeS, BaseType#type{type_spec = BaseTypeSpec}, M, Ctx).


-spec mk_default(DefaultS :: yang:stmt(), #type{}, #module{}, #yctx{}) ->
                        {undefined | invalid | {yang:stmt(), term()}, #yctx{}}.
mk_default(_, #type{type_spec_fun = undefined}, _M, Ctx) ->
    %% mk_type() failed to create type e.g. due to failed import
    {undefined, Ctx};
mk_default(false, #type{base = #typedef{default = BaseDefault}} = Type,M,Ctx) ->
    %% no default statement, try to inherit from base
    %% - this will also validate the value against restrictions for this type
    case BaseDefault of
        {Stmt, _Value} ->
            mk_default(Stmt, Type, M, Ctx);
        undefined ->
            {undefined, Ctx};
        invalid ->
            {invalid, Ctx}
    end;
mk_default(false, _, _M, Ctx) ->
    {undefined, Ctx};
mk_default(Stmt, #type{type_spec = #leafref_type_spec{}}, _M, Ctx) ->
    %% leafref default is validated when all #sn{} have been expanded
    {{Stmt, undefined}, Ctx};
mk_default(Stmt, Type, M, Ctx) ->
    case parse_value(Stmt, Type, M, Ctx) of
        {undefined, Ctx1} ->
            {invalid, Ctx1};
        {Value, Ctx1} ->
            {{Stmt, Value}, Ctx1}
    end.

-spec parse_value(yang:stmt(), #type{}, #module{}, #yctx{}) ->
                         {undefined | term(), #yctx{}}.
parse_value(Stmt, #type{type_spec_fun = TypeSpecF} = Type, M, Ctx) ->
    {_, String, Pos, _} = Stmt,
    case TypeSpecF({parse, String, Pos}, Type, M, Ctx) of
        {ok, Value} ->
            {Value, Ctx};
        {error, EStr} ->
            {undefined, add_error(Ctx, Pos, 'YANG_ERR_TYPE_VALUE',
                                  [String, EStr])};
        {error, EStr, EPos} ->
            {undefined,
             add_error(Ctx, Pos, 'YANG_ERR_TYPE_VALUE',
                       [String,
                        [EStr, " defined at ", yang_error:fmt_pos(EPos)]])};
        Res ->
            Res
    end.


%% all integer types
integer_type_spec_fun({derive, TypeS}, #type{type_spec = TypeSpec} = Base,
                      M, Ctx0) ->
    %% this function is called to create a new derived type
    %% 1. loop through all substatements, and make sure only range is
    %%    present.  the grammar has already verified that there is at
    %%    most one range stmt present
    {Restrictions, Ctx1} = get_substmts(['range'], TypeS, Ctx0),
    {New, Ctx2} =
        lists:foldl(
          fun({'range', Arg, Pos, _} = Stmt, {Cur, Ctx}) ->
                  %% parse the range arg, check each value against
                  %% the base type, build new #integer_type_spec{}
                  %% also figure out min & max
                  ParseF =
                      fun (Val) ->
                          integer_type_spec_fun({parse, Val, Pos}, Base, M, Ctx)
                      end,
                  BaseRange = TypeSpec#integer_type_spec.range,
                  case parse_range(Arg, ParseF, BaseRange, range, Ctx, Pos) of
                      {true, Range, Min, Max} ->
                          {#integer_type_spec{range_stmt = Stmt, range = Range,
                                              min = Min, max = Max},
                           Ctx};
                      {false, NewCtx} ->
                          {Cur, NewCtx}
                  end
          end, {#integer_type_spec{}, Ctx1}, Restrictions),
    if New == #integer_type_spec{} ->
            %% not modified; use the same typespec
            {TypeSpec, Ctx2};
       true ->
            {New, Ctx2}
    end;
integer_type_spec_fun({parse, Val, _Pos}, #type{type_spec = TypeSpec},
                      _M, _Ctx) ->
    #integer_type_spec{min = Min, max = Max, range = Range} = TypeSpec,
    case Val of
        <<"min">> ->
            {ok, Min};
        <<"max">> ->
            {ok, Max};
        _ ->
            try
                RE= "^\\s*([+-])?(0x([0-9a-fA-F]+)|(0[0-7]*)|([1-9]\\d*))\\s*$",
                {match, [Sign, Hex, Oct, Dec]} =
                    re:run(Val, RE, [{capture, [1, 3, 4, 5], list}]),
                Int = if Hex /= "" ->
                              list_to_integer(Sign ++ Hex, 16);
                         Oct /= "" ->
                              list_to_integer(Sign ++ Oct, 8);
                         Dec /= "" ->
                              list_to_integer(Sign ++ Dec, 10)
                      end,
                case is_in_range(Range, Int) of
                    true ->
                        {ok, Int};
                    false ->
                        case TypeSpec#integer_type_spec.range_stmt of
                            undefined ->
                                %% range error for builtin type
                                {error, "range error"};
                            RangeStmt ->
                                {error, "range error for range",
                                 stmt_pos(RangeStmt)}
                        end
                end
            catch
                _:_ ->
                    {error, "not an integer"}
            end
    end.


%% string
string_type_spec_fun({derive, TypeS}, #type{type_spec = TypeSpec} = Base,
                     _M, Ctx0) ->
    %% Create a new derived type
    %% 1. loop through all substatements, and make sure only patterns and
    %%    length are present.  the grammar has already verified that there
    %%    is at most one length stmt present
    {Restrictions, Ctx1} = get_substmts(['length', 'pattern'], TypeS, Ctx0),
    #string_type_spec{min = BMin, max = BMax, length = BLength} = TypeSpec,
    DefaultTypeSpec = #string_type_spec{min = BMin, max = BMax},
    {New, Ctx2} =
        lists:foldl(
          fun({'length', Arg, Pos, _} = Stmt, {Cur, Ctx}) ->
                  %% parse the length arg, check each value against
                  %% the base type's length, build new #string_type_spec{}
                  %% also figure out min & max
                  ParseF = fun (Val) ->
                                   parse_length(Val, BMin, BMax, BLength)
                           end,
                  case parse_range(Arg, ParseF, BLength, length, Ctx, Pos) of
                      {true, Length, Min, Max} ->
                          {Cur#string_type_spec{length_stmt = Stmt,
                                                length = Length,
                                                min = Min, max = Max},
                           Ctx};
                      {false, NewCtx} ->
                          {Cur, NewCtx}
                  end;
             ({'pattern', Arg, Pos, PSub} = Stmt, {Cur, Ctx}) ->
                  case w3cregex:compile(Arg) of
                      {ok, Re} ->
                          Inv =
                              case lists:keyfind('modifier', 1, PSub) of
                                  {_, <<"invert-match">>, _, _} ->
                                      true;
                                  _ ->
                                      false
                              end,
                          #string_type_spec{patterns = Ps,
                                            pattern_stmts = Ss} = Cur,
                          {Cur#string_type_spec{patterns = [{Re, Arg, Inv}|Ps],
                                                pattern_stmts = [Stmt | Ss]},
                           Ctx};
                      {error, Error} ->
                          {Cur, add_error(Ctx, Pos,
                                          'YANG_ERR_BAD_PATTERN', [Error])}
                  end
          end, {DefaultTypeSpec, Ctx1}, Restrictions),
    if New == DefaultTypeSpec ->
            %% not modified; use the same typespec
            {TypeSpec, Ctx2};
       Base#type.base == builtin ->
            {New, Ctx2};
       true ->
            {New#string_type_spec{parent = Base}, Ctx2}
    end;
string_type_spec_fun({parse, Val, Pos}, #type{type_spec = TypeSpec} = Type,
                     M, Ctx) ->
    %% Parse a value for the type
    #string_type_spec{parent = Parent, length = Length,
                      patterns = Patterns, pattern_stmts = Stmts} = TypeSpec,
    LengthRes =
        if Length == undefined ->
                true;
           true ->
                try
                    Len = length(unicode:characters_to_list(Val)),
                    case is_in_range(Length, Len) of
                        true ->
                            true;
                        false  ->
                            LPos =
                                stmt_pos(TypeSpec#string_type_spec.length_stmt),
                            {error, "length error for length", LPos}
                    end
                catch
                    _:_ ->
                        TPos = stmt_pos(Type#type.stmt),
                        {error, "invalid syntax for type", TPos}
                end
        end,
    Res = if LengthRes == true ->
                  while(fun({{Re, _Str, Inv}, Stmt}) ->
                                case w3cregex:match(Re, Val) of
                                    true when Inv == false ->
                                        true;
                                    false when Inv == true ->
                                        true;
                                    _ ->
                                        PPos = stmt_pos(Stmt),
                                        {error,
                                         "pattern mismatch for pattern",
                                         PPos}
                                end
                        end, lists:zip(Patterns, Stmts));
             true ->
                  LengthRes
          end,
    if Res == true, Parent /= undefined ->
            %% FIXME this will unnecessarily check parent's length restriction
            %% - but we need to check its patterns
            (Parent#type.type_spec_fun)({parse, Val, Pos}, Parent, M, Ctx);
       Res == true ->
            {ok, Val};
       true ->
            Res
    end.


%% binary
binary_type_spec_fun({derive, TypeS}, #type{type_spec = TypeSpec}, _M, Ctx0) ->
    {Restrictions, Ctx1} = get_substmts(['length'], TypeS, Ctx0),
    #binary_type_spec{min = BMin, max = BMax, length = BLength} = TypeSpec,
    {New, Ctx2} =
        lists:foldl(
          fun({'length', Arg, Pos, _} = Stmt, {Cur, Ctx}) ->
                  ParseF = fun (Val) ->
                                   parse_length(Val, BMin, BMax, BLength)
                           end,
                  case parse_range(Arg, ParseF, BLength, length, Ctx, Pos) of
                      {true, Length, Min, Max} ->
                          {Cur#binary_type_spec{length_stmt = Stmt,
                                                length = Length,
                                                min = Min, max = Max},
                           Ctx};
                      {false, NewCtx} ->
                          {Cur, NewCtx}
                  end
          end, {#binary_type_spec{}, Ctx1}, Restrictions),
    if New == #binary_type_spec{} ->
            %% not modified; use the same typespec
            {TypeSpec, Ctx2};
       true ->
            {New, Ctx2}
    end;
binary_type_spec_fun({parse, Val, _Pos}, #type{type_spec = TypeSpec},
                     _M, _Ctx) ->
    #binary_type_spec{length = Length} = TypeSpec,
    try
        Decoded = base64:decode(Val),
        if Length == undefined ->
                {ok, Decoded};
           true ->
                Len = size(Decoded),
                case is_in_range(Length, Len) of
                    true ->
                        {ok, Decoded};
                    false  ->
                        Pos = stmt_pos(TypeSpec#binary_type_spec.length_stmt),
                        {error, "length error for length", Pos}
                end
        end
    catch
        _:_ ->
            {error, "not a base64-encoded string"}
    end.


%% decimal64
decimal64_type_spec_fun({derive, TypeS},
                        #type{base = builtin, type_spec = TypeSpec0} = Base0,
                        M, Ctx0) ->
    {Substmts, Ctx1} = get_substmts(['fraction-digits', 'range'], TypeS, Ctx0),
    case lists:keytake('fraction-digits', 1, Substmts) of
        {value, {_, Arg, _, _}, Restrictions} ->
            TypeSpec = TypeSpec0#decimal64_type_spec{fraction_digits = Arg},
            Base = Base0#type{type_spec = TypeSpec},
            parse_decimal64_restrictions(Restrictions, TypeSpec, Base, M, Ctx1);
        false ->
            {_, _, Pos, _} = TypeS,
            {TypeSpec0, add_error(Ctx1, Pos, 'YANG_ERR_MISSING_TYPE_SPEC_1',
                                  ["decimal64", "fraction-digits"])}
    end;
decimal64_type_spec_fun({derive, _TypeS},
                        #type{type_spec =
                                  #decimal64_type_spec{
                                fraction_digits = undefined} = TypeSpec},
                        _M, Ctx) ->
    %% no fraction-digits, can't verify derivation, error already reported
    {TypeSpec, Ctx};
decimal64_type_spec_fun({derive, TypeS}, #type{type_spec = TypeSpec} = Base,
                        M, Ctx0) ->
    {Restrictions, Ctx1} = get_substmts(['range'], TypeS, Ctx0),
    parse_decimal64_restrictions(Restrictions, TypeSpec, Base, M, Ctx1);
decimal64_type_spec_fun({parse, _Val, _Pos},
                        #type{type_spec =
                                  #decimal64_type_spec{fraction_digits =
                                                           undefined}},
                        _M, Ctx) ->
    %% no fraction-digits, can't parse, error already reported
    {undefined, Ctx};
decimal64_type_spec_fun({parse, Val, _Pos}, #type{type_spec = TypeSpec},
                        _M, _Ctx) ->
    #decimal64_type_spec{fraction_digits = FD,
                         min = Min, max = Max, range = Range} = TypeSpec,
    case Val of
        <<"min">> ->
            {ok, Min};
        <<"max">> ->
            {ok, Max};
        _ ->
            try
                RE = ["^\\s*([+-]?\\d+)(\\.(\\d+))?\\s*$"],
                {match, [I, F]} = re:run(Val, RE, [{capture, [1,3], list}]),
                if length(F) > FD ->
                        {error, "too many fraction digits"};
                   true ->
                        Int =
                            ?l2i(I ++ F ++ lists:duplicate(FD - length(F), $0)),
                        case is_in_range(Range, Int) of
                            true ->
                                {ok, Int};
                            false ->
                                case TypeSpec#decimal64_type_spec.range_stmt of
                                    undefined ->
                                        %% range error for builtin type
                                        {error, "range error"};
                                    RangeStmt ->
                                        {error, "range error for range",
                                         stmt_pos(RangeStmt)}
                                end
                        end
                end
            catch
                _:_ ->
                    {error, "not a decimal value"}
            end
    end.

parse_decimal64_restrictions(Restrictions, TypeSpec, Base, M, Ctx1) ->
    {New, Ctx2} =
        lists:foldl(
          fun({'range', Arg, Pos, _} = Stmt, {Cur, Ctx}) ->
                  ParseF =
                      fun (Val) ->
                          decimal64_type_spec_fun({parse, Val, Pos},
                                                  Base, M, Ctx)
                      end,
                  BaseRange = TypeSpec#decimal64_type_spec.range,
                  case parse_range(Arg, ParseF, BaseRange, range, Ctx, Pos) of
                      {true, Range, Min, Max} ->
                          {Cur#decimal64_type_spec{range_stmt = Stmt,
                                                   range = Range,
                                                   min = Min, max = Max},
                           Ctx};
                      {false, NewCtx} ->
                          {Cur, NewCtx}
                  end
          end, {TypeSpec, Ctx1}, Restrictions),
    if New == TypeSpec ->
            %% not modified; use the same typespec
            {TypeSpec, Ctx2};
       true ->
            {New, Ctx2}
    end.


parse_length(Val, Min, Max, Length) ->
    case Val of
        <<"min">> ->
            {ok, Min};
        <<"max">> ->
            {ok, Max};
        _ ->
            try
                Int = ?l2i(?b2l(Val)),
                if Length == undefined ->
                        {ok, Int};
                   true ->
                        case is_in_range(Length, Int) of
                            true ->
                                {ok, Int};
                            false ->
                                error
                        end
                end
            catch
                _:_ ->
                    error
            end
    end.

-spec parse_range(binary(), fun((binary()) -> {ok, term()} | term()),
                  range(), 'length' | 'range', #yctx{}, yang:pos()) ->
                         {true, ParsedRange :: range(),
                          Min :: term(), Max :: term()} |
                         {false , #yctx{}}.
parse_range(Range, ParseF, BaseRange, LengthOrRange, Ctx, Pos) ->
    NoWS = re:replace(Range, "\\s+", "", [global]),
    Elems = re:split(NoWS, "\\|"),
    parse_range(Elems, ParseF, BaseRange, LengthOrRange, Ctx, Pos, []).

parse_range([Elem|Elems], ParseF, BaseRange, LengthOrRange, Ctx, Pos, Acc) ->
    case re:split(Elem, "\\.\\.", [{return, binary}]) of
        [MinS, MaxS] ->
            case {ParseF(MinS), ParseF(MaxS)} of
                {{ok, Min}, {ok, Max}}  ->
                    case
                        {is_range_increasing(Max, Min, LengthOrRange, Ctx, Pos),
                         is_range_increasing(Min, Acc, LengthOrRange, Ctx, Pos)}
                    of
                        {true, true} ->
                            case is_range_in_range(BaseRange, Min, Max) of
                                true ->
                                    parse_range(Elems, ParseF, BaseRange,
                                                LengthOrRange, Ctx, Pos,
                                                [{Min, Max}|Acc]);
                                false ->
                                    Ctx1 = add_range_error(Elem, LengthOrRange,
                                                           Ctx, Pos),
                                    {false, Ctx1}
                            end;
                        {true, False} ->
                            False;
                        {False, _} ->
                            False
                    end;
                {MinRes, MaxRes} ->
                    %% silly reversal of min/max to get correct printing order
                    Ctx1 = case MaxRes of
                               {ok, _} ->
                                   Ctx;
                               _ ->
                                   add_range_value_error(MaxS, LengthOrRange,
                                                         Ctx, Pos)
                           end,
                    Ctx2 = case MinRes of
                               {ok, _} ->
                                   Ctx1;
                               _ ->
                                   add_range_value_error(MinS, LengthOrRange,
                                                         Ctx1, Pos)
                           end,
                    {false, Ctx2}
            end;
        [ValS] ->
            case ParseF(ValS) of
                {ok, Val} ->
                    case
                        is_range_increasing(Val, Acc, LengthOrRange, Ctx, Pos)
                    of
                        true ->
                            parse_range(Elems, ParseF, BaseRange,
                                        LengthOrRange, Ctx, Pos, [Val|Acc]);
                        False ->
                            False
                    end;
                _ ->
                    Ctx1 = add_range_value_error(ValS, LengthOrRange, Ctx, Pos),
                    {false, Ctx1}
            end
    end;
parse_range([], _ParseF, _BaseRange, _LengthOrRange, _Ctx, _Pos, Acc) ->
    Range = lists:reverse(Acc),
    {true, Range, range_min(Range), range_max(Acc)}.

range_min([{Min, _Max}|_]) -> Min;
range_min([Min|_])         -> Min.

range_max([{_Min, Max}|_]) -> Max;
range_max([Max|_])         -> Max.

is_range_increasing(_Val, [], _LengthOrRange, _Ctx, _Pos) ->
    true;
is_range_increasing(Val, [{_Min, Max}|_], LengthOrRange, Ctx, Pos) ->
    is_range_increasing(Val, Max, LengthOrRange, Ctx, Pos);
is_range_increasing(Val, [Single|_], LengthOrRange, Ctx, Pos) ->
    is_range_increasing(Val, Single, LengthOrRange, Ctx, Pos);
is_range_increasing(Val, 'infinity', LengthOrRange, Ctx, Pos) ->
    Ctx1 = add_range_bounds_error(range_val_str(Val), "max",
                                  LengthOrRange, Ctx, Pos),
    {false, Ctx1};
is_range_increasing('infinity', _Prev, _LengthOrRange, _Ctx, _Pos) ->
    true;
is_range_increasing(Val, Prev, _LengthOrRange, _Ctx, _Pos) when Val > Prev ->
    true;
is_range_increasing(Val, Prev, LengthOrRange, Ctx, Pos) ->
    Ctx1 = add_range_bounds_error(range_val_str(Val), range_val_str(Prev),
                                  LengthOrRange, Ctx, Pos),
    {false, Ctx1}.

range_val_str(Val) -> io_lib:format("~w", [Val]).

is_range_in_range(undefined, _Min, _Max) ->
    true;
is_range_in_range([{BaseMin, BaseMax}|T], Min, Max) ->
    if Min < BaseMin ->
            false;
       Min > BaseMax ->
            is_range_in_range(T, Min, Max);
       BaseMax == 'infinity' ->
            true;
       Max == 'infinity' ->
            false;
       Max > BaseMax ->
            false;
       true ->
            true
    end;
is_range_in_range([_Single|T], Min, Max) ->
    is_range_in_range(T, Min, Max).

add_range_value_error(Value, range, Ctx, Pos) ->
    add_error(Ctx, Pos, 'YANG_ERR_BAD_RANGE_VALUE', [Value]);
add_range_value_error(Value, length, Ctx, Pos) ->
    add_error(Ctx, Pos, 'YANG_ERR_BAD_LENGTH_VALUE', [Value]).

add_range_bounds_error(Value2, Value1, range, Ctx, Pos) ->
    add_error(Ctx, Pos, 'YANG_ERR_BAD_RANGE_BOUNDS',
              [Value2, Value1]);
add_range_bounds_error(Value2, Value1, length, Ctx, Pos) ->
    add_error(Ctx, Pos, 'YANG_ERR_BAD_LENGTH_BOUNDS',
              [Value2, Value1]).

add_range_error(Range, range, Ctx, Pos) ->
    add_error(Ctx, Pos, 'YANG_ERR_BAD_RANGE', [Range]);
add_range_error(Range, length, Ctx, Pos) ->
    add_error(Ctx, Pos, 'YANG_ERR_BAD_LENGTH', [Range]).

-spec is_in_range(range(), term()) -> boolean().
is_in_range([{Min, Max} | T], Val) ->
    if (Val /= 'infinity') andalso (Val < Min) ->
            false;
       Max == 'infinity' -> % we support any length
            true;
       (Val == 'infinity') orelse (Val > Max) ->
            is_in_range(T, Val);
       true ->
            true
    end;
is_in_range([Val | _], Val) ->
    true;
is_in_range([Single | T], Val) ->
    if Single == 'infinity' ->
            false;
       (Val /= 'infinity') andalso (Val < Single) ->
            false;
       true ->
            is_in_range(T, Val)
    end;
is_in_range([], _Val) ->
    false.

while(F, [H | T]) ->
    case F(H) of
        true ->
            while(F, T);
        Else ->
            Else
    end;
while(_, []) ->
    true.


%% boolean
boolean_type_spec_fun({derive, TypeS}, #type{type_spec = TypeSpec}, _M, Ctx0) ->
    %% only identity derivation is possible
    {_, Ctx1} = get_substmts([], TypeS, Ctx0),
    {TypeSpec, Ctx1};
boolean_type_spec_fun({parse, <<"true">>, _Pos}, _Type, _M, _Ctx) ->
    {ok, true};
boolean_type_spec_fun({parse, <<"false">>, _Pos}, _Type, _M, _Ctx) ->
    {ok, false};
boolean_type_spec_fun({parse, _Val, _Pos}, _Type, _M, _Ctx) ->
    {error, "not a boolean"}.


%% enumeration
enumeration_type_spec_fun({derive, TypeS},
                          #type{base = builtin, type_spec = TypeSpec},
                          M, Ctx0) ->
    case get_substmts(['enum'], TypeS, Ctx0) of
        {[], Ctx1} ->
            {_, _, Pos, _} = TypeS,
            {TypeSpec, add_error(Ctx1, Pos, 'YANG_ERR_MISSING_TYPE_SPEC',
                                 ["enumeration", "enum"])};
        {Enumstmts, Ctx1} ->
            {Enums, Stmts, AllEnums, AllStmts, Ctx2} =
                parse_enums(Enumstmts, M, Ctx1, undefined),
            {#enumeration_type_spec{enums = Enums,
                                    enum_stmts = Stmts,
                                    all_enums = AllEnums,
                                    all_enum_stmts = AllStmts},
             Ctx2}
    end;
enumeration_type_spec_fun({derive, TypeS}, #type{type_spec = TypeSpec},
                          M, Ctx0) ->
    %% only identity derivation is possible in YANG 1
    Kwds =
        if M#module.yang_version == '1' ->
                [];
           true ->
                ['enum']
        end,
    case get_substmts(Kwds, ['enum'], TypeS, Ctx0) of
        {[], Ctx1} ->
            %% identity derivation
            {TypeSpec, Ctx1};
        {Enumstmts, Ctx1} ->
            {Enums, Stmts, AllEnums, AllStmts, Ctx2} =
                parse_enums(Enumstmts, M, Ctx1, TypeSpec),
            {#enumeration_type_spec{enums = Enums,
                                    enum_stmts = Stmts,
                                    all_enums = AllEnums,
                                    all_enum_stmts = AllStmts},
             Ctx2}
    end;
enumeration_type_spec_fun({parse, _Val, _Pos},
                          #type{type_spec = #enumeration_type_spec{enums = []}},
                          _M, Ctx) ->
    %% no emums, parsing will always fail, error already reported
    {undefined, Ctx};
enumeration_type_spec_fun({parse, Val, _Pos},
                          #type{type_spec =
                                  #enumeration_type_spec{enums = Enums}} = Type,
                          _M, _Ctx) ->
    case lists:keyfind(?b2a(Val), 1, Enums) of
        {_, IntVal} ->
            {ok, IntVal};
        false  ->
            {error,
             "enum not defined for enumeration",
             stmt_pos(Type#type.stmt)}
    end.

parse_enums(Enumstmts, M, Ctx, undefined) ->
    parse_enums(Enumstmts, M, Ctx, -1, undefined);
parse_enums(Enumstmts, M, Ctx,
            #enumeration_type_spec{enums = BaseEnums,
                                   all_enums = AllBaseEnums,
                                   all_enum_stmts = AllBaseStmts}) ->
    parse_enums(Enumstmts, M, Ctx, -1, {BaseEnums, AllBaseEnums, AllBaseStmts}).

parse_enums(Enumstmts, M, Ctx, PrevV, BaseEnumsAndStmts) ->
    parse_names_and_values(
      Enumstmts, M, Ctx, PrevV, BaseEnumsAndStmts, 'value',
      'YANG_ERR_ENUM_NAME_MISMATCH', 'YANG_ERR_ENUM_VALUE_MISMATCH',
      'YANG_ERR_DUPLICATE_ENUM_NAME', 'YANG_ERR_DUPLICATE_ENUM_VALUE',
      ?INT32_MIN, ?INT32_MAX, 'YANG_ERR_ENUM_VALUE', []).

-spec parse_names_and_values([yang:stmt()], #module{}, #yctx{}, integer(),
                             {[name_and_value()], [name_and_value()],
                              [yang:stmt()]} | undefined,
                             atom(), atom(), atom(), atom(), atom(),
                             integer(), integer(), atom(), [#nvdata{}]) ->
                                    {[name_and_value()], [yang:stmt()],
                                     [name_and_value()], [yang:stmt()],
                                     #yctx{}}.
parse_names_and_values([{Kwd, Name, NPos, Substmts0}|Stmts], M, Ctx0, PrevV,
                       BaseNVsAndStmts, ValueSubstmt,
                       NameMismatchError, ValueMismatchError,
                       DuplicateNameError, DuplicateValueError,
                       MinValue, MaxValue, ValueError, Acc) ->
    {BaseValue, BaseSubstmts, Keep0, Ctx1} =
        chk_base_name(Name, NPos, BaseNVsAndStmts,
                      NameMismatchError, Ctx0),
    Substmts = inherit_stmts(ValueSubstmt, BaseSubstmts, Substmts0),
    Stmt = {Kwd, Name, NPos, Substmts},
    if Keep0 ->
            {Keep, Ctx2} = chk_if_features(Substmts, M, Ctx1);
       true ->
            Keep = Keep0,
            Ctx2 = Ctx1
    end,
    case lists:keyfind(Name, #nvdata.name, Acc) of
        #nvdata{npos = OldNPos} ->
            Ctx4 = add_error(Ctx2, NPos, DuplicateNameError,
                             [Name, yang_error:fmt_pos(OldNPos)]),
            NewPrevV = PrevV,
            NewAcc = Acc;
        false ->
            case yang:search_one_stmt(ValueSubstmt, Substmts) of
                {_, Value0, VPos, _} ->
                    case
                        chk_base_value(Value0, VPos, BaseValue,
                                       ValueMismatchError, Ctx2)
                    of
                        {Value, Ctx3} when Value /= undefined,
                                           Value > PrevV ->
                            NextPrevV = Value;
                        {Value, Ctx3} ->
                            NextPrevV = PrevV
                    end;
                false when BaseValue /= undefined, BaseValue > PrevV ->
                    %% keep base value
                    VPos = NPos,
                    Value = BaseValue,
                    NextPrevV = Value,
                    Ctx3 = Ctx2;
                false when BaseValue /= undefined ->
                    %% keep base value
                    VPos = NPos,
                    Value = BaseValue,
                    NextPrevV = PrevV,
                    Ctx3 = Ctx2;
                false ->
                    VPos = NPos,
                    NextPrevV = PrevV + 1,
                    Value = NextPrevV,
                    Ctx3 = Ctx2
            end,
            if Value == undefined ->
                    Ctx4 = Ctx3,
                    NewPrevV = PrevV;
               MinValue =< Value, Value =< MaxValue ->
                    case lists:keyfind(Value, #nvdata.value, Acc) of
                        #nvdata{vpos = OldVPos} ->
                            Ctx4 =
                                add_error(
                                  Ctx3, VPos, DuplicateValueError,
                                  [?i2l(Value), yang_error:fmt_pos(OldVPos)]),
                            NewPrevV = PrevV;
                        false ->
                            Ctx4 = Ctx3,
                            NewPrevV = NextPrevV
                    end;
                true ->
                    Ctx4 = add_error(Ctx3, VPos, ValueError, [Value]),
                    NewPrevV = PrevV
            end,
            NewAcc = add_name(Keep, Stmt, Name, NPos, Value, VPos,  Acc)
    end,
    parse_names_and_values(Stmts, M, Ctx4, NewPrevV,
                           BaseNVsAndStmts, ValueSubstmt,
                           NameMismatchError, ValueMismatchError,
                           DuplicateNameError, DuplicateValueError,
                           MinValue, MaxValue, ValueError, NewAcc);
parse_names_and_values([], _M, Ctx0, _PrevV,
                       _BaseNVsAndStmts, _ValueSubstmt,
                       _NameMismatchError, _ValueMismatchError,
                       _DuplicateNameError, _DuplicateValueError,
                       _MinValue, _MaxValue, _ValueError, Acc) ->
    lists:foldl(
      fun (#nvdata{keep = Keep, name = Name, value = Value, stmt = Stmt},
           {NVs, Stmts, AllNVs, AllStmts, Ctx}) ->
              NV = {Name, Value},
              if Keep ->
                      {[NV|NVs], [Stmt|Stmts], [NV|AllNVs], [Stmt|AllStmts],
                       Ctx};
                 true ->
                      {NVs, Stmts, [NV|AllNVs], [Stmt|AllStmts],
                       Ctx}
              end
      end, {[], [], [], [], Ctx0}, Acc).

add_name(Keep, Stmt, Name, NPos, Value, VPos, Acc) ->
    [#nvdata{keep = Keep, name = Name, npos = NPos,
             value = Value, vpos = VPos, stmt = Stmt}|Acc].

chk_base_name(_Name, _Pos, undefined, _NameMismatchError, Ctx) ->
    {undefined, [], true, Ctx};
chk_base_name(Name, Pos, {BaseNames, AllBaseNames, AllBaseStmts},
              NameMismatchError, Ctx) ->
    chk_base_name(Name, Pos, AllBaseNames, AllBaseStmts, BaseNames,
                  NameMismatchError, Ctx).

chk_base_name(Name, _Pos, [{Name, BaseValue}|_], [BaseStmt|_], BaseNames,
              _NameMismatchError, Ctx) ->
    {BaseValue, yang:stmt_substmts(BaseStmt),
     lists:keymember(Name, 1, BaseNames), Ctx};
chk_base_name(Name, Pos, [_|AllBaseNames], [_|AllBaseStmts], BaseNames,
              NameMismatchError, Ctx) ->
    chk_base_name(Name, Pos, AllBaseNames, AllBaseStmts, BaseNames,
                  NameMismatchError, Ctx);
chk_base_name(Name, Pos, [], _AllBaseStmts, BaseNames,
              NameMismatchError, Ctx) ->
    {undefined, [], lists:keymember(Name, 1, BaseNames),
     add_error(Ctx, Pos, NameMismatchError, [Name])}.

chk_base_value(Value, _Pos, undefined, _ValueMismatchError, Ctx) ->
    {Value, Ctx};
chk_base_value(Value, _Pos, Value, _ValueMismatchError, Ctx) ->
    {Value, Ctx};
chk_base_value(Value, Pos, BaseValue, ValueMismatchError, Ctx) ->
    {undefined, add_error(Ctx, Pos, ValueMismatchError, [Value, BaseValue])}.


inherit_stmts(ExceptKwd, [{'if-feature', _, _, _}|BaseSubstmts], Substmts) ->
    inherit_stmts(ExceptKwd, BaseSubstmts, Substmts);
inherit_stmts(ExceptKwd, [{ExceptKwd, _, _, _}|BaseSubstmts], Substmts) ->
    inherit_stmts(ExceptKwd, BaseSubstmts, Substmts);
inherit_stmts(ExceptKwd, [{Kwd, _, _, _} = Substmt|BaseSubstmts], Substmts) ->
    case lists:keymember(Kwd, 1, Substmts) of
        true ->
            inherit_stmts(ExceptKwd, BaseSubstmts, Substmts);
        false ->
            inherit_stmts(ExceptKwd, BaseSubstmts, [Substmt|Substmts])
    end;
inherit_stmts(_ExceptKwd, [], Substmts) ->
    Substmts.

chk_if_features(Substmts, M, Ctx0) ->
    {FeatureL, [] = _WhenL, [] = _MustL, Ctx1} =
        yang:common_substmts(Substmts, 'local', M, Ctx0),
    Keep = yang:check_if_features(FeatureL, Ctx1),
    {Keep, Ctx1}.


%% bits
bits_type_spec_fun({derive, TypeS},
                   #type{base = builtin, type_spec = TypeSpec}, M, Ctx0) ->
    case get_substmts(['bit'], TypeS, Ctx0) of
        {[], Ctx1} ->
            {_, _, Pos, _} = TypeS,
            {TypeSpec, add_error(Ctx1, Pos, 'YANG_ERR_MISSING_TYPE_SPEC',
                                 ["bits", "bit"])};
        {Bitstmts, Ctx1} ->
            {Bits, Stmts, AllBits, AllStmts, Ctx2} =
                parse_bits(Bitstmts, M, Ctx1, undefined),
            {#bits_type_spec{bits = Bits,
                             bit_stmts = Stmts,
                             all_bits = AllBits,
                             all_bit_stmts = AllStmts},
             Ctx2}
    end;
bits_type_spec_fun({derive, TypeS}, #type{type_spec = TypeSpec}, M, Ctx0) ->
    %% only identity derivation is possible in YANG 1
    Kwds =
        if M#module.yang_version == '1' ->
                [];
           true ->
                ['bit']
        end,
    case get_substmts(Kwds, ['bit'], TypeS, Ctx0) of
        {[], Ctx1} ->
            %% identity derivation
            {TypeSpec, Ctx1};
        {Bitstmts, Ctx1} ->
            {Bits, Stmts, AllBits, AllStmts, Ctx2} =
                parse_bits(Bitstmts, M, Ctx1, TypeSpec),
            {#bits_type_spec{bits = Bits,
                             bit_stmts = Stmts,
                             all_bits = AllBits,
                             all_bit_stmts = AllStmts},
             Ctx2}
    end;
bits_type_spec_fun({parse, _Val, _Pos},
                   #type{type_spec = #bits_type_spec{bits = []}},
                   _M, Ctx) ->
    %% no bits, parsing will always fail, error already reported
    {undefined, Ctx};
bits_type_spec_fun({parse, Val, _Pos},
                   #type{type_spec = #bits_type_spec{bits = Bits}} = Type,
                   _M, _Ctx) ->
    BinNames = re:split(Val, "\\s+", [{return, binary}]),
    F = fun (<<>>, Acc) ->
                Acc;
            (Name, Acc) ->
                {_, BitPos} = lists:keyfind(?b2a(Name), 1, Bits),
                Acc bor (1 bsl BitPos)
        end,
    try
        {ok, lists:foldl(F, 0, BinNames)}
    catch
        _:_ ->
            {error, "bit not defined for bits type", stmt_pos(Type#type.stmt)}
    end.


parse_bits(Bitstmts, M, Ctx, undefined) ->
    parse_bits(Bitstmts, M, Ctx, -1, undefined);
parse_bits(Bitstmts, M, Ctx,
           #bits_type_spec{bits = BaseBits,
                           all_bits = AllBaseBits,
                           all_bit_stmts = AllBaseStmts}) ->
    parse_bits(Bitstmts, M, Ctx, -1, {BaseBits, AllBaseBits, AllBaseStmts}).

parse_bits(Bitstmts, M, Ctx, PrevV, BaseBitsAndStmts) ->
    parse_names_and_values(
      Bitstmts, M, Ctx, PrevV, BaseBitsAndStmts, 'position',
      'YANG_ERR_BIT_NAME_MISMATCH', 'YANG_ERR_BIT_POSITION_MISMATCH',
      'YANG_ERR_DUPLICATE_BIT_NAME', 'YANG_ERR_DUPLICATE_BIT_POSITION',
      0, ?UINT32_MAX, 'YANG_ERR_BIT_POSITION', []).


%% union
union_type_spec_fun({derive, TypeS},
                    #type{base = builtin, type_spec = TypeSpec}, M, Ctx0) ->
    %% #type{} with member list in type_spec
    case get_substmts(['type'], TypeS, Ctx0) of
        {[], Ctx1} ->
            {_, _, Pos, _} = TypeS,
            {TypeSpec, add_error(Ctx1, Pos, 'YANG_ERR_MISSING_TYPE_SPEC',
                                 ["union", "type"])};
        {Typestmts, Ctx1} ->
            if M#module.yang_version == '1' ->
                    reject_invalid_union_types(Typestmts, TypeSpec, Ctx1);
               true ->
                    %% FIXME: no restriction for 1.1
                    %% {TypeSpec, Ctx1}
                    temp_handle_union_types(Typestmts, TypeSpec, Ctx1)
            end
    end;
union_type_spec_fun({derive, TypeS}, #type{type_spec = TypeSpec}, _M, Ctx0) ->
    %% only identity derivation is possible
    {_, Ctx1} = get_substmts([], TypeS, Ctx0),
    {TypeSpec, Ctx1};
union_type_spec_fun({parse, _Val, _Pos},
                    #type{type_spec = #union_type_spec{types = []}},
                    _M, Ctx) ->
    %% no member types, parsing will always fail, error already reported
    {undefined, Ctx};
union_type_spec_fun({parse, Val, Pos},
                    #type{type_spec = #union_type_spec{types = Types}} = Type,
                    M, Ctx0) ->
    %% try each member type
    parse_union_type(Types, Val, Pos, Type, M, Ctx0).

parse_union_type([#type{type_spec_fun = TypeSpecF} = Type|T],
                 Str, Pos, UType, M, Ctx) ->
    case TypeSpecF({parse, Str, Pos}, Type, M, Ctx) of
        {ok, Val} = Res ->
            case Type#type.type_spec of
                #union_type_spec{} ->
                    Res;
                _ ->
                    {ok, {Type, Val}}
            end;
        _ ->
            parse_union_type(T, Str, Pos, UType, M, Ctx)
    end;
parse_union_type([], _Str, _Pos, Type, _M, _Ctx) ->
    {error, "no member type matched for union", stmt_pos(Type#type.stmt)}.

reject_invalid_union_types(Typestmts, TypeSpec, Ctx0) ->
    #union_type_spec{types = Types} = TypeSpec,
    case
        lists:partition(fun ({_, #type{type_spec = #empty_type_spec{}}}) ->
                                true;
                            ({_, #type{type_spec = #leafref_type_spec{}}}) ->
                                true;
                            (_) ->
                                false
                        end, lists:zip(Typestmts, Types))
    of
        {[], _} ->
            {TypeSpec, Ctx0};
        {Invalid, Valid} ->
            Ctx1 = lists:foldl(
                     fun({{_, _, TPos, _}, Type}, Ctx) ->
                             Name = case Type#type.type_spec of
                                        #empty_type_spec{}   -> 'empty';
                                        #leafref_type_spec{} -> 'leafref'
                                    end,
                             DefPos = get_typedef_pos(Name, Type, TPos),
                             add_error(
                               Ctx, TPos, 'YANG_ERR_BAD_TYPE_IN_UNION',
                               [Name, yang_error:fmt_pos(DefPos)])
                     end, Ctx0, Invalid),
            RemTypes = [Type || {_Stmt, Type} <- Valid],
            {#union_type_spec{types = RemTypes}, Ctx1}
    end.

%% FIXME: remove for 1.1
temp_handle_union_types(Typestmts, TypeSpec, Ctx0) ->
    #union_type_spec{types = Types0} = TypeSpec,
    {RemTypes, Ctx1} =
        lists:foldl(fun ({{_, _, TPos, _},
                          #type{type_spec = #empty_type_spec{}} = Type},
                         {Types, Ctx}) ->
                            Name = 'empty',
                            DefPos = get_typedef_pos(Name, Type, TPos),
                            {Types,
                             add_error(
                               Ctx, TPos, 'YANG_ERR_BAD_TYPE_IN_UNION',
                               [Name, yang_error:fmt_pos(DefPos)])};
                        ({{_, _, TPos, _} = Stmt,
                          #type{type_spec = #leafref_type_spec{}} = Type},
                         {Types, Ctx}) ->
                            {value, StringType} = lookup_type('string', Ctx),
                            Name = 'leafref',
                            DefPos = get_typedef_pos(Name, Type, TPos),
                            {[StringType#type{base = 'string', stmt = Stmt} |
                              Types],
                             add_error(
                               Ctx, TPos, 'YANG_ERR_UNSUPPORTED_TYPE_IN_UNION',
                               [Name, yang_error:fmt_pos(DefPos)])};
                        ({_Stmt, Type}, {Types, Ctx}) ->
                            {[Type | Types], Ctx}
                    end, {[], Ctx0}, lists:zip(Typestmts, Types0)),
    {#union_type_spec{types = RemTypes}, Ctx1}.

get_typedef_pos(Base, #type{base = Base}, Pos) ->
    Pos;
get_typedef_pos(Base, #type{base = TypeDef}, _Pos0) ->
    #typedef{type = Type, stmt = {_, _, Pos, _}} = TypeDef,
    get_typedef_pos(Base, Type, Pos).


%% instance-identifier
instance_identifier_type_spec_fun({derive, TypeS},
                                  #type{type_spec = TypeSpec},
                                  _M, Ctx0) ->
    case get_substmts(['require-instance'], TypeS, Ctx0) of
        {[{_, Val, _, _}], Ctx1} ->
            New =
                TypeSpec#instance_identifier_type_spec{require_instance = Val};
        {_, Ctx1} ->
            New = TypeSpec
    end,
    if New == TypeSpec ->
            %% not modified; use the same typespec
            {TypeSpec, Ctx1};
       true ->
            {New, Ctx1}
    end;
instance_identifier_type_spec_fun({parse, _Val, _Pos}, Type, _M, _Ctx) ->
    %% FIXME: Verify syntax - or more?
    {error, "invalid syntax for type", stmt_pos(Type#type.stmt)}.


%% identityref
identityref_type_spec_fun({derive, TypeS},
                          #type{base = builtin, type_spec = TypeSpec},
                          #module{identities = Identities} = M, Ctx0) ->
    case get_substmts(['base'], TypeS, Ctx0) of
        {[], Ctx1} ->
            {TypeSpec, add_error(Ctx1, stmt_pos(TypeS),
                                 'YANG_ERR_MISSING_TYPE_SPEC_1',
                                 ["identityref", "base"])};
        {[_, {_, _, Pos, _}| _], Ctx1} when M#module.yang_version == '1' ->
            {TypeSpec, add_error(Ctx1, Pos, 'YANG_ERR_MULTIPLE_BASES', [])};
        {BaseStmts, Ctx1} ->
            {Bases, Ctx4} =
                lists:foldl(
                  fun({_, Val, Pos, _}, {Acc, Ctx2}) ->
                          case
                              yang:get_identity(Val, Pos, Identities, M, Ctx2)
                          of
                              {#identity{} = Identity, Ctx3} ->
                                  {[Identity | Acc], Ctx3};
                              {undefined, Ctx3} ->
                                  %% error already reported
                                  {Acc, Ctx3}
                          end
                  end, {[], Ctx1}, BaseStmts),
            {#identityref_type_spec{bases = Bases}, Ctx4}
    end;
identityref_type_spec_fun({derive, TypeS},
                          #type{type_spec = TypeSpec}, _M, Ctx0) ->
    %% only identity derivation is possible
    {_, Ctx1} = get_substmts([], TypeS, Ctx0),
    {TypeSpec, Ctx1};
identityref_type_spec_fun({parse, _Val, _Pos},
                          #type{type_spec =
                                    #identityref_type_spec{bases = []}},
                          _M, Ctx) ->
    %% no base, can't parse, error already reported
    {undefined, Ctx};
identityref_type_spec_fun({parse, Val, Pos},
                          #type{type_spec =
                                    #identityref_type_spec{bases = Bases}},
                          #module{identities = Identities} = M, Ctx0) ->
    case yang:parse_idref(Val, Pos, Ctx0) of
        {ok, RawRef, Ctx1} ->
            case yang:get_identity(RawRef, Pos, Identities, M, Ctx1) of
                {#identity{} = Identity, Ctx2} ->
                    IsDerived =
                        fun(Base) -> is_identity_derived(Identity, Base) end,
                    case all(IsDerived, Bases) of
                        true ->
                            {Identity, Ctx2};
                        {false, Base} ->
                            {undefined,
                             add_error(Ctx2, Pos, 'YANG_ERR_TYPE_VALUE',
                                       [Val, ["identityref not derived from ",
                                              ?a2l(Base#identity.name)]])}
                    end;
                {undefined, _Ctx2} = Res ->
                    %% error already reported
                    Res
            end;
        {error, Ctx1} ->
            %% error already reported
            {undefined, Ctx1}
    end.

%% like lists:all but returns faulty element
all(_Pred, []) ->
    true;
all(Pred, [H | T]) ->
    case Pred(H) of
        true ->
            all(Pred, T);
        false ->
            {false, H}
    end.

is_identity_derived(#identity{bases = IdBases}, Base) ->
    is_any_identity_derived(IdBases, Base).

is_identity_derived_or_same(#identity{name = Name, moduleref = MRef},
                            #identity{name = Name, moduleref = MRef}) ->
    true;
is_identity_derived_or_same(#identity{bases = IdBases}, Base) ->
    is_any_identity_derived(IdBases, Base).

is_any_identity_derived([], _) ->
    false;
is_any_identity_derived([IdBase | T], Base) ->
    case is_identity_derived_or_same(IdBase, Base) of
        true ->
            true;
        false ->
            is_any_identity_derived(T, Base)
    end.

%% empty
empty_type_spec_fun({derive, TypeS}, #type{type_spec = TypeSpec}, _M, Ctx0) ->
    %% only identity derivation is possible
    {_, Ctx1} = get_substmts([], TypeS, Ctx0),
    {TypeSpec, Ctx1};
empty_type_spec_fun({parse, _Val, Pos}, _Type, _M, Ctx) ->
    {undefined, add_error(Ctx, Pos, 'YANG_ERR_BAD_DEFAULT_VALUE', ["empty"])}.


%% leafref
leafref_type_spec_fun({derive, TypeS},
                      #type{base = builtin, type_spec = TypeSpec}, M, Ctx0) ->
    Kwds =
        if M#module.yang_version == '1' ->
                ['path'];
           true ->
                ['path', 'require-instance']
        end,
    {Substmts, Ctx1} = get_substmts(Kwds, ['require-instance'], TypeS, Ctx0),
    case lists:keytake('path', 1, Substmts) of
        {value, {_, Val, PathPos, _} = Stmt, Restrictions} ->
            case
                yang_xpath:compile(Val, PathPos, M, Ctx1#yctx.strict, Ctx1)
            of
                {ok, Q, Ctx2} ->
                    LeafrefPath = xpath_to_extended_leafref_path(Q),
                    case lists:keytake('require-instance', 1, Restrictions) of
                        {value, {_, ReqVal, _, _}, _} ->
                            ok;
                        _ ->
                            ReqVal = true
                    end,
                    {#leafref_type_spec{parsed_xpath = Q,
                                        path = LeafrefPath,
                                        path_stmt = Stmt,
                                        require_instance = ReqVal},
                     Ctx2};
                {error, Ctx2} ->
                    {TypeSpec, Ctx2}
            end;
        false ->
            {TypeSpec, add_error(Ctx1, stmt_pos(TypeS),
                                 'YANG_ERR_MISSING_TYPE_SPEC_1',
                                 ["leafref", "path"])}
    end;
leafref_type_spec_fun({derive, TypeS}, #type{type_spec = TypeSpec}, M, Ctx0) ->
    %% only identity derivation is possible in YANG 1
    Kwds =
        if M#module.yang_version == '1' ->
                [];
           true ->
                ['require-instance']
        end,
    case get_substmts(Kwds, ['require-instance'], TypeS, Ctx0) of
        {[], Ctx1} ->
            %% identity derivation
            {TypeSpec, Ctx1};
        {[{_, Val, _, _}], Ctx1} ->
            New = TypeSpec#leafref_type_spec{require_instance = Val},
            if New == TypeSpec ->
                    %% not modified; use the same typespec
                    {TypeSpec, Ctx1};
               true ->
                    {New, Ctx1}
            end
    end.
%% Can't parse here - let it fail w function_clause if someone tries somehow
%% leafref_type_spec_fun({parse, _Val, _Pos}, Type, _M, _Ctx) ->

-type leafref_path_int() ::
        ['parent'
       | {'child', yang:yang_identifier(),
          [KeyEq :: {KeyName :: yang:yang_identifier(),
                     Leaf :: leafref_path_int()}]}].

-type leafref_path() ::
        {'relative' | 'absolute', leafref_path_int()}.

%% also supports deref()
-type extended_leafref_path() ::
        {'deref',
         DerefPath :: leafref_path_int(),
         RestPath :: leafref_path_int()}
      | leafref_path().

xpath_to_extended_leafref_path([{function_call, xp_deref,
                                 [{relative, DerefXPath}]} |
                                RestXPath]) ->
    {deref, xpath_to_leafref_path_int(DerefXPath),
     xpath_to_leafref_path_int(RestXPath)};
xpath_to_extended_leafref_path(XPath) ->
    xpath_to_leafref_path(XPath).

xpath_to_leafref_path({AbsOrRel, XPath}) ->
    {AbsOrRel, xpath_to_leafref_path_int(XPath)}.

xpath_to_leafref_path_int([{step, parent, _, _} | T]) ->
    [parent | xpath_to_leafref_path_int(T)];
xpath_to_leafref_path_int([{step, child, Name, Preds} | T]) ->
    [{child, name(Name), preds(Preds)} |
     xpath_to_leafref_path_int(T)];
xpath_to_leafref_path_int([]) ->
    [].

preds([]) ->
    [];
preds([{pred, {comp, '=', {relative, [{step, child, Name, []}]},
               [{function_call, xp_current, []} | RestXPath]}} | T]) ->
    [{name(Name), xpath_to_leafref_path_int(RestXPath)} |
     preds(T)].

name({name, Name}) ->
    Name;
name({name, ModuleName, Name}) ->
    {ModuleName, Name}.

-spec validate_leafref_path(#leafref_type_spec{}, #sn{} | undefined,
                            #module{}, [#sn{}], #yctx{}) ->
                                   {true, TargetSn :: #sn{}, FinalSn :: #sn{}} |
                                   {false, #yctx{}}.
validate_leafref_path(#leafref_type_spec{path = undefined},
                      _Sn, _M, _Ancestors, Ctx) ->
    %% path undefined means an error in the leafref definition.
    {false, Ctx};
validate_leafref_path(#leafref_type_spec{path_stmt = Stmt} = TypeSpec,
                      Sn, M, Ancestors, Ctx) ->
    PathPos = yang:stmt_pos(Stmt),
    InitCursor = yang:mk_cursor(Sn, Ancestors, PathPos, M, data),
    case validate_leafref_path0(TypeSpec, InitCursor, [Sn], Ctx) of
        {circular, Ctx1} ->
            {false, Ctx1};
        {true,  #sn{kind = Kind} = TargetSn, FinalSn}
          when (Kind == 'leaf') orelse (Kind == 'leaf-list') ->
            {true, TargetSn, FinalSn};
        {true, #sn{name = TargetName, stmt = TargetStmt}, _FinalSn} ->
            {false, add_error(Ctx, PathPos, 'YANG_ERR_LEAFREF_NOT_LEAF',
                              [yang_error:fmt_yang_identifier(TargetName),
                               yang_error:fmt_pos(stmt_pos(TargetStmt))])};
        Else ->
            Else
    end.

validate_leafref_path0(#leafref_type_spec{path = Path},
                       InitCursor, Visited, Ctx) ->
    case
        follow_leafref_path(Path, InitCursor, InitCursor,
                            _ValidateKeys = true, Ctx)
    of
        {true, #cursor{cur = #sn{
                         type = #type{
                           type_spec = #leafref_type_spec{
                             path_stmt =
                                 PathStmt} = TypeSpec}} = TargetSn} = Cursor} ->
            case lists:member(TargetSn, Visited) of
                true ->
                    {circular, add_error(Ctx,
                                         yang:stmt_pos(PathStmt),
                                         'YANG_ERR_CIRCULAR_DEPENDENCY_LEAFREF',
                                         [?a2l(TargetSn#sn.kind),
                                          yang_error:fmt_yang_identifier(
                                            TargetSn#sn.name)])};
                false ->
                    %% make sure that the target's leafref doesn't
                    %% refer to us (directly or indirectly)
                    InitCursor1 = yang:cursor_reset(TargetSn, Cursor),
                    case
                        validate_leafref_path0(TypeSpec, InitCursor1,
                                               [TargetSn | Visited], Ctx)
                    of
                        {circular, Ctx1} ->
                            {circular, Ctx1};
                        {true, _, FinalSn} ->
                            {true, TargetSn, FinalSn};
                        {false, _Ctx1} ->
                            %% ignore other errors, if any; they are
                            %% reported when the leafref is checked
                            {false, Ctx}
                    end
            end;
        %% If pointing to a direct leafref, TargetSn
        %% and FinalSn will be the same
        {true, #cursor{cur = Sn}} ->
            {true, Sn, Sn};
        {false, Ctx1} ->
            {false, Ctx1}
    end.

follow_leafref_path(Path, InitCursor, Ctx) ->
    follow_leafref_path(Path, InitCursor, InitCursor, false, Ctx).

follow_leafref_path(Path, InitCursor, ValidateKeys, Ctx) ->
    follow_leafref_path(Path, InitCursor, InitCursor, ValidateKeys, Ctx).

follow_leafref_path({deref, DerefPath, Rest}, Cursor0, InitCursor,
                    ValidateKeys, Ctx0) ->
    case follow_leafref_path_int(DerefPath, Cursor0, InitCursor,
                                 ValidateKeys, Ctx0) of
        {true, #cursor{cur = #sn{kind = 'leaf', type = Type}} = Cursor1}
          when is_record(Type#type.type_spec, leafref_type_spec) ->
            %% Ok, deref argument is a leafref leaf.  Continue from here.
            Path2 = (Type#type.type_spec)#leafref_type_spec.path,
            case
                follow_leafref_path(Path2, Cursor1, Cursor1,
                                    ValidateKeys, Ctx0)
            of
                {true, Cursor2} ->
                    follow_leafref_path_int(Rest, Cursor2, InitCursor,
                                            ValidateKeys, Ctx0);
                {false, _} ->
                    %% This error will be reported when we validate the
                    %% target leafref.
                    {false, Ctx0}
            end;
        {true, #cursor{cur = #sn{name = Name, stmt = Stmt}}} ->
            %% Not ok, deref argument not a leafref
            {false,
             add_error(Ctx0, Cursor0#cursor.pos, 'YANG_ERR_DEREF_NOT_LEAFREF',
                       [yang_error:fmt_yang_identifier(Name),
                        yang_error:fmt_pos(yang:stmt_pos(Stmt))])};
        {false, Ctx1} ->
            {false, Ctx1}
    end;
follow_leafref_path({relative, Path}, Cursor, InitCursor, ValidateKeys, Ctx) ->
    follow_leafref_path_int(Path, Cursor, InitCursor, ValidateKeys, Ctx);
follow_leafref_path({absolute, Path}, Cursor, InitCursor, ValidateKeys, Ctx) ->
    follow_leafref_path_int(Path,
                            Cursor#cursor{cur = undefined, ancestors = []},
                            InitCursor,
                            ValidateKeys,
                            Ctx).

follow_leafref_path_int(Path, InitCursor, Ctx) ->
    follow_leafref_path_int(Path, InitCursor, InitCursor, false, Ctx).

follow_leafref_path_int(Path, InitCursor, ValidateKeys, Ctx) ->
    follow_leafref_path_int(Path, InitCursor, InitCursor, ValidateKeys, Ctx).

follow_leafref_path_int([parent | T], C0, InitCursor, ValidateKeys, Ctx0) ->
    case yang:cursor_move(parent, C0, Ctx0) of
        {true, C1} ->
            follow_leafref_path_int(T, C1, InitCursor, ValidateKeys, Ctx0);
        False ->
            False
    end;
follow_leafref_path_int([{child, Name, KeyEqL} | T], C0, InitCursor,
                        ValidateKeys, Ctx) ->
    case yang:cursor_move({child, Name}, C0, Ctx) of
        {true, #cursor{cur = #sn{keys = Keys,
                                 module = #module{modulename = Mod}}} = C1} ->
            case
                if ValidateKeys ->
                        check_keys(KeyEqL, Mod, Keys, C1, InitCursor, Ctx);
                   true ->
                        true
                end
            of
                true ->
                    follow_leafref_path_int(T, C1, InitCursor,
                                            ValidateKeys, Ctx);
                False ->
                    False
            end;
        False ->
            False
    end;
follow_leafref_path_int([], C, _, _, _) ->
    {true, C}.

check_keys([], _, _, _, _, _) ->
    true;
check_keys([{{OtherMod, _} = KeyId, _} | _], Mod, _, C1, _, Ctx)
  when OtherMod /= Mod ->
    {false, add_error(Ctx, C1#cursor.pos,
                      'YANG_ERR_NODE_NOT_FOUND',
                      [yang_error:fmt_yang_identifier(KeyId)])};
check_keys([{KeyName, _} | _], Mod, _, C1, _, Ctx)
  when is_atom(KeyName),
       Mod /= C1#cursor.init_modulename ->
    {false, add_error(Ctx, C1#cursor.pos,
                      'YANG_ERR_NODE_NOT_FOUND2',
                      [yang_error:fmt_yang_identifier(KeyName),
                       C1#cursor.init_modulename])};
check_keys([{KeyId, LeafPath} | T], Mod, Keys, C1, InitCursor, Ctx)
  when is_list(Keys) ->
    case KeyId of
        {Mod, KeyName} ->
            ok;
        KeyName ->
            ok
    end,
    case lists:member(KeyName, Keys) of
        true ->
            %% OK, the key is valid
            %% LeafPath is the path after current().
            case
                follow_leafref_path_int(LeafPath, InitCursor, InitCursor,
                                        _ValidateKeys = true, Ctx)
            of
                {true, #cursor{cur = #sn{kind = 'leaf'}}} ->
                    %% Ok, refers to a leaf.
                    %% FIXME: pyang actually verifies that this leaf is also
                    %% a leafref, and that this leafref refers to the key called
                    %% KeyId.  Do we have to do that?
                    check_keys(T, Mod, Keys, C1, InitCursor, Ctx);
                {true, #cursor{cur = #sn{name = Name, stmt = Stmt}}} ->
                    %% Not ok, refers to something else than a leaf.
                    {false,
                     add_error(Ctx, C1#cursor.pos, 'YANG_ERR_LEAFREF_NOT_LEAF0',
                               [yang_error:fmt_yang_identifier(Name),
                                yang_error:fmt_pos(yang:stmt_pos(Stmt))])};
                False ->
                    False
            end;
        false ->
            {false, add_error(Ctx, C1#cursor.pos,
                              'YANG_ERR_NODE_NOT_FOUND',
                              [yang_error:fmt_yang_identifier(KeyId)])}
    end;
check_keys([{KeyId, _LeafPath} | _], _, _, C1, _, Ctx) ->
    {false, add_error(Ctx, C1#cursor.pos,
                      'YANG_ERR_NODE_NOT_FOUND',
                      [yang_error:fmt_yang_identifier(KeyId)])}.


stmt_pos({_, _, Pos, _}) -> Pos;
stmt_pos(_)              -> undefined.

get_substmts(Kwds, Stmt, Ctx) ->
    get_substmts(Kwds, [], Stmt, Ctx).

get_substmts(Kwds, Kwds_1_1_only, {_, _, _, Substmts}, Ctx0) ->
    {Wanted, Ctx1} =
        lists:foldl(
          fun ({{_ModuleName, _ExtensionKeyword}, _, _, _}, Acc) ->
                  %% ignore extensions
                  Acc;
              ({Kwd, _, Pos, _} = Substmt, {Wanted0, Ctx}) ->
                  case lists:member(Kwd, Kwds) of
                      true ->
                          {[Substmt|Wanted0], Ctx};
                      false ->
                          Arg = case lists:member(Kwd, Kwds_1_1_only) of
                                    true  -> " in YANG version 1";
                                    false -> ""
                                end,
                          {Wanted0, add_error(Ctx, Pos,
                                              'YANG_ERR_BAD_RESTRICTION',
                                              [Kwd, Arg])}
                  end
          end, {[], Ctx0}, Substmts),
    {lists:reverse(Wanted), Ctx1}.


register_builtin_types(Ctx) ->
    lists:foldl(fun ({Name, Type}, Ctx0) ->
                        register_type(Name, Type#type{base = builtin}, Ctx0)
                end, Ctx, builtin_types()).

builtin_types() ->
    [
     {'binary', #type{type_spec_fun = fun binary_type_spec_fun/4,
                      type_spec = #binary_type_spec{min = 0,
                                                    max = 'infinity'}}},
     {'bits', #type{type_spec_fun = fun bits_type_spec_fun/4,
                    type_spec = #bits_type_spec{}}},
     {'boolean', #type{type_spec_fun = fun boolean_type_spec_fun/4,
                       type_spec = #boolean_type_spec{}}},
     {'decimal64', #type{type_spec_fun = fun decimal64_type_spec_fun/4,
                         type_spec = #decimal64_type_spec{
                           min = ?INT64_MIN,
                           max = ?INT64_MAX,
                           range = [{?INT64_MIN, ?INT64_MAX}]}}},
     {'empty', #type{type_spec_fun = fun empty_type_spec_fun/4,
                     type_spec = #empty_type_spec{}}},
     {'enumeration', #type{type_spec_fun = fun enumeration_type_spec_fun/4,
                           type_spec = #enumeration_type_spec{}}},
     {'identityref', #type{type_spec_fun = fun identityref_type_spec_fun/4,
                           type_spec = #identityref_type_spec{}}},
     {'instance-identifier',
      #type{type_spec_fun = fun instance_identifier_type_spec_fun/4,
            type_spec =
                #instance_identifier_type_spec{require_instance = true}}},
     {'int8', mk_integer_type(-128, 127)},
     {'int16', mk_integer_type(-32768, 32767)},
     {'int32', mk_integer_type(?INT32_MIN, ?INT32_MAX)},
     {'int64', mk_integer_type(?INT64_MIN, ?INT64_MAX)},
     {'leafref', #type{type_spec_fun = fun leafref_type_spec_fun/4,
                       type_spec = #leafref_type_spec{}}},
     {'string', #type{type_spec_fun = fun string_type_spec_fun/4,
                      type_spec = #string_type_spec{min = 0,
                                                    max = 'infinity'}}},
     {'uint8', mk_integer_type(0, 255)},
     {'uint16', mk_integer_type(0, 65535)},
     {'uint32', mk_integer_type(0, ?UINT32_MAX)},
     {'uint64', mk_integer_type(0, 18446744073709551615)},
     {'union', #type{type_spec_fun = fun union_type_spec_fun/4,
                     type_spec = #union_type_spec{}}}
    ].

mk_integer_type(Min, Max) ->
    #type{type_spec_fun = fun integer_type_spec_fun/4,
          type_spec = #integer_type_spec{min = Min,
                                         max = Max,
                                         range = [{Min, Max}]}}.
