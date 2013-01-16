%%
%% A Rebar plugin that compiles Yanger plugins
%%
-module(yanger_plugins_rebar_plugin).

-export([post_compile/2, post_clean/2]).


%% Compile yanger plugins into priv
post_compile(Config0, AppFile) ->
    ErlOpts0 = rebar_config:get_list(Config0, erl_opts, []),
    ErlOpts1 = [{src_dirs,["plugins"]}, {outdir,"priv"}|
                proplists:delete(src_dirs,
                                 proplists:delete(outdir, ErlOpts0))],
    Config1 = rebar_config:set(Config0, erl_opts, ErlOpts1),
    rebar_erlc_compiler:compile(Config1, AppFile),
    ok.

%% Remove compiled yanger plugins
post_clean(_Config, _AppFile) ->
    rebar_file_utils:delete_each(
      rebar_utils:find_files("priv", "^.*\\.beam\$")),
    ok.
