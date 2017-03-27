%%% File    : xpath_bif.erl
%%% Author  :  <thomasl@metis.tail-f.com>
%%% Description :
%%% Created : 11 May 2006 by  <thomasl@metis.tail-f.com>
%%
%% XPath 1.0 builtin functions (http://www.w3.org/TR/xpath)
%%
%% NOTES:
%%   Some of the operations, such as comparisons, arithmetic, ... are
%% implemented in xpath_eval_expr.
%%
%%   The functions are named xp_*. This is to avoid clashes with
%% erlang builtins (e.g., 'round') as well as provide some uniformity.
%%   If an operation exits, the test is assumed to have failed. This must be
%% handled by the CALLER (e.g., xpath_confd_eval).
%%
%%   The internal datatypes are those of XPath:
%% {bool, B} | {number, N} | {literal, Str}
%%
%%   Strings are currently implemented as lists. Perhaps string values
%% instead should remain as binaries?
%%   Strings are not UTF-8 converted.

-module(xpath_bif).

-export(
   [string_to_bif/1,
    bif_type/1,
    arity_check/2
   ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The builtins of XPath 1.0, from the spec. In the cases where there
%% are optional arguments, we show all the versions. (except for concat,
%% which is an infinite(!) family of functions)

%% (these are used during parsing/rewriting to map strings to the
%% appropriate atoms, or otherwise 'fail')

string_to_bif(Str) ->
    try
        XP_Bif = list_to_existing_atom("xp_" ++ Str),
        bif_type(XP_Bif),
        XP_Bif
    catch _:_ ->
            {undefined_function, Str}
    end.

bif_type(xp_last) ->
    number;
bif_type(xp_position) ->
    number;
bif_type(xp_count) ->
    number;
bif_type(xp_id) ->
    nodeset;
bif_type('xp_local-name') ->
    literal;
bif_type('xp_namespace-uri') ->
    literal;
bif_type(xp_name) ->
    literal;
bif_type(xp_string) ->
    literal;
bif_type(xp_concat) ->
    literal;
bif_type('xp_starts-with') ->
    boolean;
bif_type(xp_contains) ->
    boolean;
bif_type('xp_substring-before') ->
    literal;
bif_type('xp_substring-after') ->
    literal;
bif_type(xp_substring) ->
    literal;
bif_type('xp_string-length') ->
    number;
bif_type('xp_normalize-space') ->
    literal;
bif_type(xp_translate) ->
    literal;
bif_type('xp_re-match') ->
    boolean;
bif_type('xp_string-compare') ->
    number;
bif_type('xp_derived-from') ->
    boolean;
bif_type('xp_derived-from-or-self') ->
    boolean;
bif_type('xp_enum-value') ->
    number;
bif_type('xp_bit-is-set') ->
    boolean;
bif_type(xp_compare) ->
    number;
bif_type(xp_boolean) ->
    boolean;
bif_type('xp_nodeset-as-boolean') ->
    boolean;
bif_type(xp_false) ->
    boolean;
bif_type(xp_true) ->
    boolean;
bif_type(xp_not) ->
    boolean;
bif_type(xp_lang) ->
    boolean;
bif_type(xp_number) ->
    number;
bif_type(xp_sum) ->
    number;
bif_type(xp_min) ->
    number;
bif_type(xp_max) ->
    number;
bif_type(xp_avg) ->
    number;
bif_type(xp_floor) ->
    number;
bif_type(xp_ceiling) ->
    number;
bif_type(xp_round) ->
    number;
bif_type(xp_band) ->
    number;
bif_type(xp_bor) ->
    number;
bif_type(xp_bxor) ->
    number;
bif_type(xp_bnot) ->
    number;
bif_type(xp_current) ->
    nodeset;
bif_type(xp_deref) ->
    nodeset;
bif_type('xp_sort-by') ->
    nodeset.

arity_check(xp_last, 0) ->
    true;
arity_check(xp_position, 0) ->
    true;
arity_check(xp_count, 1) ->
    true;
arity_check(xp_id, 1) ->
    true;
arity_check('xp_local-name', 0) ->
    true;
arity_check('xp_local-name', 1) ->
    true;
arity_check('xp_namespace-uri', 0) ->
    true;
arity_check('xp_namespace-uri', 1) ->
    true;
arity_check(xp_name, 0) ->
    true;
arity_check(xp_name, 1) ->
    true;
arity_check(xp_string, 0) ->
    true;
arity_check(xp_string, 1) ->
    true;
arity_check(xp_concat, A) when A > 1 ->
    true;
arity_check('xp_starts-with', 2) ->
    true;
arity_check(xp_contains, 2) ->
    true;
arity_check('xp_substring-before', 2) ->
    true;
arity_check('xp_substring-after', 2) ->
    true;
arity_check(xp_substring, 2) ->
    true;
arity_check(xp_substring, 3) ->
    true;
arity_check('xp_string-length', 0) ->
    true;
arity_check('xp_string-length', 1) ->
    true;
arity_check('xp_normalize-space', 0) ->
    true;
arity_check('xp_normalize-space', 1) ->
    true;
arity_check(xp_translate, 3) ->
    true;
arity_check('xp_re-match', 2) ->
    true;
arity_check('xp_string-compare', 2) ->
    true;
arity_check('xp_derived-from', 2) ->
    true;
arity_check('xp_derived-from-or-self', 2) ->
    true;
arity_check('xp_enum-value', 1) ->
    true;
arity_check('xp_bit-is-set', 2) ->
    true;
arity_check(xp_compare, 2) ->
    true;
arity_check(xp_boolean, 1) ->
    true;
arity_check('xp_nodeset-as-boolean', 1) ->
    true;
arity_check(xp_false, 0) ->
    true;
arity_check(xp_true, 0) ->
    true;
arity_check('xp_not', 1) ->
    true;
arity_check(xp_lang, 1) ->
    true;
arity_check(xp_number, 0) ->
    true;
arity_check(xp_number, 1) ->
    true;
arity_check(xp_sum, 1) ->
    true;
arity_check(xp_min, 1) ->
    true;
arity_check(xp_max, 1) ->
    true;
arity_check(xp_avg, 1) ->
    true;
arity_check(xp_floor, 1) ->
    true;
arity_check(xp_ceiling, 1) ->
    true;
arity_check(xp_round, 1) ->
    true;
arity_check(xp_band, 2) ->
    true;
arity_check(xp_bor, 2) ->
    true;
arity_check(xp_bxor, 2) ->
    true;
arity_check(xp_bnot, 1) ->
    true;
arity_check(xp_current, 0) ->
    true;
arity_check(xp_deref, 1) ->
    true;
arity_check('xp_sort-by', 2) ->
    true;
arity_check(_, _) ->
    false.
