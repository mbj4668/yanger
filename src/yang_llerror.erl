-module(yang_llerror).
-export([code2err/1, codes/0]).

%% FIXME: generate this file from yang_error.h

code2err(1) -> 'YANG_ERR_INTERNAL';
code2err(2) -> 'YANG_ERR_MEMORY_ERROR';
code2err(100) -> 'YANG_ERR_PARSE_EOF';
code2err(102) -> 'YANG_ERR_PARSE_FOPEN';
code2err(103) -> 'YANG_ERR_PARSE_BAD_KEYWORD';
code2err(104) -> 'YANG_ERR_PARSE_EXPECTED_SEPARATOR';
code2err(105) -> 'YANG_ERR_PARSE_EXPECTED_STRING';
code2err(106) -> 'YANG_ERR_PARSE_INCOMPLETE_STATEMENT';
code2err(107) -> 'YANG_ERR_PARSE_EXPECTED_QUOTED_STRING';
code2err(200) -> 'YANG_ERR_GRAMMAR_KEYWORD_ALREADY_FOUND';
code2err(201) -> 'YANG_ERR_GRAMMAR_EXPECTED_KEYWORD';
code2err(202) -> 'YANG_ERR_GRAMMAR_UNEXPECTED_KEYWORD';
code2err(203) -> 'YANG_ERR_GRAMMAR_UNDEFINED_PREFIX';
code2err(204) -> 'YANG_ERR_GRAMMAR_DUPLICATE_PREFIX';
code2err(205) -> 'YANG_ERR_GRAMMAR_MISSING_ARGUMENT';
code2err(206) -> 'YANG_ERR_GRAMMAR_UNEXPECTED_ARGUMENT';
code2err(_) -> undefined.

codes() ->
    [
     {'YANG_ERR_INTERNAL', error, "~s"},
     {'YANG_ERR_MEMORY_ERROR', error, "~s"},
     {'YANG_ERR_PARSE_EOF', error, "~s"},
     {'YANG_ERR_PARSE_FOPEN', error, "~s"},
     {'YANG_ERR_PARSE_BAD_KEYWORD', error, "~s"},
     {'YANG_ERR_PARSE_EXPECTED_SEPARATOR', error, "~s"},
     {'YANG_ERR_PARSE_EXPECTED_STRING', error, "~s"},
     {'YANG_ERR_PARSE_INCOMPLETE_STATEMENT', error, "~s"},
     {'YANG_ERR_PARSE_EXPECTED_QUOTED_STRING', error, "~s"},
     {'YANG_ERR_GRAMMAR_KEYWORD_ALREADY_FOUND', error, "~s"},
     {'YANG_ERR_GRAMMAR_EXPECTED_KEYWORD', error, "~s"},
     {'YANG_ERR_GRAMMAR_UNEXPECTED_KEYWORD', error, "~s"},
     {'YANG_ERR_GRAMMAR_UNDEFINED_PREFIX', error, "~s"},
     {'YANG_ERR_GRAMMAR_DUPLICATE_PREFIX', error, "~s"},
     {'YANG_ERR_GRAMMAR_MISSING_ARGUMENT', error, "~s"},
     {'YANG_ERR_GRAMMAR_UNEXPECTED_ARGUMENT', error, "~s"}
    ].
