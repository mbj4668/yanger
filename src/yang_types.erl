-module(yang_types).

%%% UNFINSHED!!  maybe completely rewrite.


-include("yang.hrl").

-type type_spec_fun() :: fun(({derive, yang:stmt()} | {parse, binary()},
                              TypeSpec :: term(),
                              #yctx{}) ->
                                    TypeSpec :: term() | boolean()).

-record(integer_type_spec, {
          %% this type's min value; for the 'min' keyword
          min,
          %% this type's max value; for the 'max' keyword
          max,
          range :: [{Min :: integer(), Max :: integer()} | (Val :: integer())]
         }).

-record(string_type_spec, {
          %% this type's min length; for the 'min' keyword
          min,
          %% this type's max length; for the 'max' keyword
          max,
          length :: [{Min :: integer(), Max :: integer()} | (Val :: integer())],
          patterns = [] :: [CompiledRegexp :: term()]
         }).

-record(union_type_spec, {
          types :: [#type{}]
         }).

-spec register_type_spec(TypeName :: yang:builtin_type_name()
                                   | {ModuleName :: atom(), TypeName :: atom()},
                         type_spec_fun(),
                         #yctx{}) -> #yctx{}.
%% Registers
register_type_spec(TypeName, TypeSpecF, Ctx) ->
    TypeMap = yang:map_insert(TypeName, TypeSpecF, Ctx#yctx.typemap),
    #yctx{typemap = TypeMap}.

%% -spec validate_value(#type{}, Val :: term(), #yctx{}) ->
%%     {boolean(), #yctx{}}.
%% validate_value(Type, Val, Ctx) ->
%%     TypeMap = Ctx#yctx.typemap,
%%     {_Restrictions, ParseValF} =
%%         get_type_registration(Type, Ctx#yctx.typemap),
%%     nyi.


%% get_type_registration(#type{base = Base}, TypeMap) ->
%%     case yang:map_lookup(TypeName, TypeMap) of
%%         {value, Registration} ->
%%             Registration;
%%         none ->
%%             if is_atom(Base) ->
%%                     yang:map_get(Base, TypeMap);
%%                true ->
%%                     get_type_registration(Base#typedef.type, TypeMap)
%%             end
%%     end.

-spec mk_type_spec(Type :: yang:stmt(),
                   yang:builtin_type_name() | #typedef{},
                   #yctx{}) -> {type_spec_fun(), TypeSpec :: term()}.
mk_type_spec(TypeS, BaseType, Ctx) when is_atom(BaseType) ->
    %% Simple case, make a core typespec
    {_, _, Pos, Substmts} = TypeS,
    {BaseTypeSpec, BaseTypeSpecF} =
        case BaseType of
            'int32' ->
                mk_integer_type_spec(-2147483648, 2147483647);
            'string' ->
                {#string_type_spec{},
                 fun string_type_spec_fun/3}
        end,
    %% Apply restrictions
    {BaseTypeSpecF, BaseTypeSpecF({derive, TypeS}, BaseTypeSpec, Ctx)}.

mk_integer_type_spec(Min, Max) ->
    {#integer_type_spec{min = Min,
                        max = Max,
                        range = [{Min, Max}]},
     fun integer_type_spec_fun/3}.

integer_type_spec_fun({derive, TypeS}, TypeSpec, Ctx0) ->
    %% this function is called to create a new derived type
    %% 1. loop through all substatements, and make sure only range is
    %%    present.  the grammar has already verified that there is just one
    %%    range stmt present
    {_, _, _, Substmts} = TypeS,
    {New, Ctx2} =
        lists:foldl(
          fun({'range', Arg, Pos, _}, {_Old, Ctx1}) ->
                  %% parse the range arg, check each value against
                  %% the base type, build new #integer_type_spec_fun{}
                  {#integer_type_spec{}, Ctx1};
             ({{_ModuleName, _ExtensionKeyword}, _, _, _}, Acc) ->
                  %% ignore extensions
                  Acc;
             ({Other, _, Pos, _}, {Cur, Ctx1}) ->
                  {Cur,
                   yang:add_error(Ctx1, Pos,
                                  'YANG_ERR_BAD_RESTRICTION', [Other])}
          end, {undefined, Ctx0}, Substmts),
    if New == undefined ->
            %% not modified; use the same typespec
            TypeSpec;
       true ->
            New
    end;
integer_type_spec_fun({parse, Val}, TypeSpec, Ctx0) ->
    #integer_type_spec{min = Min, max = Max, range = Range} = TypeSpec,
    case Val of
        <<"min">> ->
            {true, Min};
        <<"max">> ->
            {true, Max};
        _ ->
            try
                Int = ?l2i(?b2l(Val)),
                case is_in_range(Range, Int) of
                    true ->
                        {true, Int};
                    false ->
                        false
                end
            catch
                _:_ ->
                    false
            end
    end.

is_in_range([Val | _], Val) ->
    true;
is_in_range([{Min, Max} | T], Val) ->
    if Val < Min ->
            false;
       Max == 'infinity' -> % we support any length
            true;
       Val > Max ->
            is_in_range(T, Val);
       true ->
            true
    end;
is_in_range([], _Val) ->
    false.

string_type_spec_fun({derive, TypeS}, TypeSpec, Ctx0) ->
    %% this function is called to create a new derived type
    %% 1. loop through all substatements, and make sure only patterns and
    %%    length are
    %%    present.  the grammar has already verified that there is just one
    %%    range stmt present
    {_, _, _, Substmts} = TypeS,
    {New, Ctx2} =
        lists:foldl(
          fun({'length', Arg, Pos, _}, {Cur, Ctx1}) ->
                  %% parse the length arg, check each value against
                  %% the base type's length
                  NewLength = 'FIXME',
                  %% also figure out min & max
                  {Cur#string_type_spec{length = NewLength}, Ctx1};
             ({'pattern', Arg, Pos, _}, {Cur, Ctx1}) ->
                  case w3c_regex:compile(Arg) of
                      {ok, Re} ->
                          P = Cur#string_type_spec.patterns,
                          {Cur#string_type_spec{patterns = [{Re, Pos} | P]},
                           Ctx1};
                      {error, Error} ->
                          {Cur,
                           yang:add_error(Ctx1, Pos,
                                          'YANG_ERR_BAD_PATTERN', [Error])}
                  end;
             ({{_ModuleName, _ExtensionKeyword}, _, _, _}, Acc) ->
                  %% ignore extensions
                  Acc;
             ({Other, _, Pos, _}, {Cur, Ctx1}) ->
                  {Cur,
                   yang:add_error(Ctx1, Pos,
                                  'YANG_ERR_BAD_RESTRICTION', [Other])}
          end, {undefined, Ctx0}, Substmts),
    if New == undefined ->
            %% not modified; use the same typespec
            TypeSpec;
       true ->
            New
    end;
string_type_spec_fun({parse, Val}, TypeSpec, Ctx0) ->
    #string_type_spec{length = Length, patterns = Patterns} = TypeSpec,
    LengthOk =
        if Length == undefined ->
                true;
           true ->
                Len = length(unicode:characters_to_binary(Val)),
                is_in_range(Length, Len)
        end,
    if LengthOk ->
            while(fun({Re, Pos}) ->
                          case w3c_regex:first_match(Val, Re) of
                              {match, _, _} ->
                                  true;
                              nomatch ->
                                  false
                          end
                  end, Patterns);
       true ->
            false
    end.

while(F, [H | T]) ->
    case F(H) of
        true ->
            while(F, T);
        Else ->
            Else
    end;
while(_, []) ->
    true.
