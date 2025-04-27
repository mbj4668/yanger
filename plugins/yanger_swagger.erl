%%%----------------------------------------------------------------%%%
%%% @doc Yanger Swagger Output Plugin                              %%%
%%% Converts a YANG schema to RESTCONF Swagger.                    %%%
%%%----------------------------------------------------------------%%%


-module(yanger_swagger).
-behaviour(yanger_plugin).

-export([init/1, help/0]).

-include_lib("yanger/include/yang.hrl").

-define(search_one,    yang:search_one_stmt).
-define(stmt_arg,      yang:stmt_arg).
-define(stmt_kw,       yang:stmt_keyword).
-define(stmt_substmts, yang:stmt_substmts).
-define(in,            lists:member).
-define(rev,           lists:reverse).

-define(SWAGGER_VERSION, <<"2.0">>).

%%
%% internal storage definitions
%%
-define(SWAGGER_DEFS,   '$definitions').
-define(SWAGGER_PARAMS, '$parameters').
-define(SWAGGER_RESPS,  '$responses').


-spec help() -> binary().
help() ->
<<"Each node is printed as an HTTP end-point in a Swagger document.

Depending on the nodes flags a collection of HTTP methods are generated as
possible.
">>.

init(Ctx0) ->
    Ctx1 = yanger_plugin:register_output_format(Ctx0,
                                                swagger,
                                                _AllowErrors = false,
                                                fun emit/3),
    Ctx2 = yanger_plugin:register_error_codes(
             Ctx1,
             [{'SWAGGER_NO_DATA_MODULES', error,
               "No data modules given, at least one "
               "data module is required for Swagger."},
              {'SWAGGER_TOO_MANY_DATA_MODULES', error,
               "Too many modules given, only one "
               "data module is supported for Swagger."}]),

    yanger_plugin:register_option_specs(Ctx2, option_specs()).


-record(options, {
          host,
          path,
          version,
          tag_mode,
          terms,
          contact_email,
          contact_url,
          contact_name,
          license_url,
          license_name,
          top_resource,
          omit_query_params,
          omit_body_params,
          omit_form_params,
          omit_header_params,
          omit_path_params,
          omit_standard_statuses,
          methods,
          path_filter,
          int64_as_string
         }).

option_specs() ->
    [{"Swagger output specific options:",
      [
       %% --swagger-host <host>
       {host, undefined, "swagger-host", string,
        "Add host to the Swagger output"},

       %% --swagger-basepath <path> typically "/restconf"
       %% NOTE: this corresponds to configurable RESTCONF root resource
       {basepath, undefined, "swagger-basepath", string,
        "Add basePath to the Swagger output"},

       %% --swagger-version <version>
       %% NOTE: this will override any versions/revisions in the yang file
       {version, undefined, "swagger-version", string,
        "Add version url to the Swagger output. NOTE: this will override any "
        "revision in the yang file"},

       %% --swagger-tag-mode <mode>
       {tag_mode, undefined, "swagger-tag-mode", {atom, all},
        "Set tag mode to group resources. "
        "Valid values are: methods, resources, all"},

       %% --swagger-terms <terms>
       {terms, undefined, "swagger-terms", string,
        "Add termsOfService to the Swagger output"},

       %% --swagger-contact-name <name>
       {contact_name, undefined, "swagger-contact-name", string,
        "Add contact name to the Swagger output"},

       %% --swagger-contact-url <url>
       {contact_url, undefined, "swagger-contact-url", string,
        "Add contact url to the Swagger output"},

       %% --swagger-contact-email <email>
       {contact_email, undefined, "swagger-contact-email", string,
        "Add contact email to the Swagger output"},

       %% --swagger-license-name <name>
       {license_name, undefined, "swagger-license-name", string,
        "Add license name to the Swagger output"},

       %% --swagger-license-url <url>
       {license_url, undefined, "swagger-license-url", string,
        "Add license url to the Swagger output"},

       %% --swagger-top-resource <resource>
       {top_resource, undefined, "swagger-top-resource", {atom, all},
        "Generate only swagger resources from this top resource. "
        "Valid values are: root, data, operations, all"},

       %% --swagger-omit-query-params <boolean>
       {omit_query_params, undefined,
        "swagger-omit-query-params",
        {boolean, false},
        "Omit RESTCONF query parameters"},

       %% --swagger-omit-body-params <boolean>
       {omit_body_params, undefined,
        "swagger-omit-body-params",
        {boolean, false},
        "Omit RESTCONF body parameters"},

       %% --swagger-omit-form-params <boolean>
       {omit_form_params, undefined,
        "swagger-omit-form-params",
        {boolean, false},
        "Omit RESTCONF form parameters"},

       %% --swagger-omit-header-params <boolean>
       {omit_header_params, undefined,
        "swagger-omit-header-params",
        {boolean, false},
        "Omit RESTCONF header parameters"},

       %% --swagger-omit-path-params <boolean>
       {omit_path_params, undefined,
        "swagger-omit-path-params",
        {boolean, false},
        "Omit RESTCONF path parameters"},

       %% --swagger-omit-standard-statuses <boolean>
       {omit_standard_statuses, undefined,
        "swagger-omit-standard-statuses",
        {boolean, false},
        "Omit standard HTTP response statuses. "
        "NOTE: at least one successful HTTP status will still be included"},

       %% --swagger-methods <method1>, ... <methodn>
       {methods, undefined,  "swagger-methods",
        {string, "get, post, put, patch, delete"},
        "HTTP methods to include. Example: --swagger-methods \"get, post\""},

       %% --swagger-path <path-filter>
       {path_filter, undefined, "swagger-path-filter", string,
        "Filter out paths matching a path filter. "
        "Example: --swagger-path-filter \"/data/example-jukebox/jukebox\""},

       %% --swagger-int64-as-string <boolean>
       {int64_as_string, undefined, "swagger-int64-as-string",
        {boolean, false},
        "Format 64-bit integer types as JSON strings"}

      ]
     }].

mk_options(Ctx) ->
    Host = proplists:get_value(host, Ctx#yctx.options),
    BasePath = proplists:get_value(basepath, Ctx#yctx.options, "/restconf"),
    Path = [?l2a(Name) || Name <- string:tokens(BasePath, "/")],
    Version = proplists:get_value(version, Ctx#yctx.options),
    TagMode = proplists:get_value(tag_mode, Ctx#yctx.options, all),
    TUrl = proplists:get_value(terms, Ctx#yctx.options),
    CUrl = proplists:get_value(contact_url, Ctx#yctx.options),
    CEmail = proplists:get_value(contact_email, Ctx#yctx.options),
    CName = proplists:get_value(contact_name, Ctx#yctx.options),
    LUrl = proplists:get_value(license_url, Ctx#yctx.options),
    LName = proplists:get_value(license_name, Ctx#yctx.options),
    TopResource = proplists:get_value(top_resource, Ctx#yctx.options, all),
    OmitQP = proplists:get_value(omit_query_params, Ctx#yctx.options),
    OmitBP = proplists:get_value(omit_body_params, Ctx#yctx.options),
    OmitFP = proplists:get_value(omit_form_params, Ctx#yctx.options),
    OmitHP = proplists:get_value(omit_header_params, Ctx#yctx.options),
    OmitPP = proplists:get_value(omit_path_params, Ctx#yctx.options),
    OmitStatuses = proplists:get_value(omit_standard_statuses,
                                       Ctx#yctx.options),
    Methods = parse_input_methods(
                proplists:get_value(methods, Ctx#yctx.options)),
    PathFilter = proplists:get_value(path_filter, Ctx#yctx.options),
    Int64AsString = proplists:get_value(int64_as_string, Ctx#yctx.options),
    #options{host                   = Host,
             path                   = Path,
             version                = Version,
             tag_mode               = TagMode,
             terms                  = TUrl ,
             contact_url            = CUrl,
             contact_email          = CEmail,
             contact_name           = CName,
             license_url            = LUrl,
             license_name           = LName,
             top_resource           = TopResource,
             omit_query_params      = OmitQP,
             omit_body_params       = OmitBP,
             omit_form_params       = OmitFP,
             omit_header_params     = OmitHP,
             omit_path_params       = OmitPP,
             omit_standard_statuses = OmitStatuses,
             methods                = Methods,
             path_filter            = PathFilter,
             int64_as_string        = Int64AsString}.


%% parse a comma separated HTTP method string, eg: "post, get"
parse_input_methods(undefined) ->
    %% default [get, post, put, patch, delete]
    all_http_methods() -- [head, options];
parse_input_methods(MethodsStr) when is_list(MethodsStr) ->
    All = all_http_methods(),
    F = fun(S, Acc) ->
                M = ?l2a(string:lowercase(string:trim(S))),
                %% omit invalid methods
                case lists:member(M, All) of
                    true ->
                        [M | Acc];
                    _ ->
                        Acc
                end
        end,
    lists:foldl(F, [], string:split(MethodsStr, ",", all)).


-spec has_data_sns(#yctx{}, #module{}) -> boolean().
has_data_sns(Ctx, Mod) ->
    Chs     = [C || C <- Mod#module.children, is_data_def(C#sn.kind, Ctx)],
    Rpcs    = [C || C <- Mod#module.children, C#sn.kind == 'operation',
                    element(1, C#sn.stmt) == 'rpc'],
    Actions = [C || C <- Mod#module.children, C#sn.kind == 'operation',
                    element(1, C#sn.stmt) == 'action'],
    Notifs  = [C || C <- Mod#module.children, C#sn.kind == 'notification'],

    [] =/= Chs ++ Rpcs ++ Actions ++ Notifs.


-type io_device() :: pid() | integer().
-spec emit(#yctx{}, [#module{}], io_device()) -> [#yerror{}].
emit(Ctx, Mods, Fd) ->
    DataSnsMods = [M || M <- Mods, has_data_sns(Ctx, M)],

    %% Verify that we are only supplied with one data module.
    if length(DataSnsMods) =:= 0 ->
            [#module{filename = Filename}|_] = Mods,
            CtxE = yanger_plugin:add_error(Ctx,
                                           _ChildPos = {Filename, 0},
                                           'SWAGGER_NO_DATA_MODULES',
                                           []),
            CtxE#yctx.errors;

       length(DataSnsMods) > 1 ->
            [#module{filename = Filename}|_] = Mods,
            CtxE = yanger_plugin:add_error(Ctx,
                                           _ChildPos = {Filename, 0},
                                           'SWAGGER_TOO_MANY_DATA_MODULES',
                                           []),
            CtxE#yctx.errors;

       true ->
            %% Create tables for collecting definitions, parameters, and
            %% responses.
            ets:new(?SWAGGER_DEFS,[ordered_set,public,named_table,compressed]),
            ets:new(?SWAGGER_PARAMS, [ordered_set, public, named_table]),
            ets:new(?SWAGGER_RESPS,  [ordered_set, public, named_table]),

            Opts = mk_options(Ctx),

            ok = emit_tree(Ctx, Mods, Mods, Fd, Opts),
            %% No errors
            []
    end.


data_sns_partition(Children, Ctx) ->
    Chs = [],
    Rpcs = [],
    Actions = [],
    Notifs = [],
    data_sns_partition(Children, Ctx, Chs, Rpcs, Actions, Notifs).

data_sns_partition([#sn{kind = Kind} = Child|Cs], Ctx,
                  Chs, Rpcs, Actions, Notifs) ->
    if
        Kind == 'operation' ->
            case element(1, Child#sn.stmt) of
                'rpc' ->
                    data_sns_partition(Cs, Ctx,
                                       Chs, [Child|Rpcs], Actions, Notifs);
                'action' ->
                    data_sns_partition(Cs, Ctx,
                                       Chs, Rpcs, [Child|Actions], Notifs);
                _ ->
                    data_sns_partition(Cs, Ctx,
                                       Chs, Rpcs, Actions, Notifs)
            end;
        Kind == 'notification' ->
            data_sns_partition(Cs, Ctx, Chs, Rpcs, Actions, [Child|Notifs]);
        true ->
            IsDataDef = is_data_def(Kind, Ctx),
            if
                IsDataDef ->
                    data_sns_partition(Cs, Ctx,
                                       [Child|Chs], Rpcs, Actions, Notifs);
                true  ->
                    data_sns_partition(Cs, Ctx, Chs, Rpcs, Actions, Notifs)
            end
    end;
data_sns_partition([], _Ctx, ChsRev, RpcsRev, ActionsRev, NotifsRev) ->
    ActionNotifs = ?rev(ActionsRev, ?rev(NotifsRev)),
    {Rpcs, RpcsActionNotifs} = rpcs_partition(RpcsRev, ActionNotifs, []),
    DataSns = ?rev(ChsRev, RpcsActionNotifs),
    {DataSns, Rpcs}.

rpcs_partition([Rpc|RpcsRev], ActionNotifs, Rpcs) ->
    rpcs_partition(RpcsRev, [Rpc|ActionNotifs], [Rpc|Rpcs]);
rpcs_partition([], RpcsActionNotifs, Rpcs) ->
    {Rpcs, RpcsActionNotifs}.

-spec emit_tree(#yctx{}, [#module{}], [#module{}], io_device(),
                #options{}) -> ok.
%% @doc Main emit function. Works through a list of modules and calls
%%      other sub emit functions appropriately.
emit_tree(_Ctx, _Mods = [], _AllMods, Fd, _Opts) ->
    %% end json doc
    io:format(Fd, "\n}\n", []);
emit_tree(Ctx, [Mod|Mods], AllMods, Fd, Opts) ->
    Lvl = 1,
    Indent2 = indent(Lvl),

    {DataSns, Rpcs} = data_sns_partition(Mod#module.children, Ctx),

    %% At the moment we only allow one YANG module to be compiled at the time,
    %% it is possible to augment this module. If we already have emitted the
    %% Swagger doc header, we have more than one YANG module and need to bail.
    if DataSns == [] ->
            %% Ensure we only print sections once.
            skip;
       true ->
            %% start json doc
            io:format(Fd, "{\n", []),

            %% header
            print_header(Mod, Fd, Opts),

            %% paths
            PropertyName = ?a2l(paths),
            io:format(Fd, "~s\"~s\": {\n", [Indent2, PropertyName]),

            RootPath = tokpath2origpath([]),
            OpPath = tokpath2origpath([operations]),
            VerPath = tokpath2origpath(['yang-library-version']),
            DataPath = tokpath2origpath([data]),

            MatchRoot = match_path(Opts#options.path_filter, RootPath),
            MatchOp = match_path(Opts#options.path_filter, OpPath),
            MatchVer = match_path(Opts#options.path_filter, VerPath),
            MatchData = match_path(Opts#options.path_filter, DataPath),

            HasOpNode   = has_path_node(Rpcs, Mod, OpPath, true, Opts),
            HasDataNode = has_path_node(DataSns, Mod, DataPath, true, Opts),

            if Opts#options.top_resource == root
               orelse
               Opts#options.top_resource == all ->
                    print_property([], Mod, Fd, RootPath, root, Opts),
                    io:format(Fd, "~s",
                              [delimiter_nl((HasOpNode orelse MatchOp orelse
                                             MatchVer orelse
                                             HasDataNode orelse MatchData)
                                            andalso MatchRoot)]);
               true ->
                    skip
            end,

            if Opts#options.top_resource == operations
               orelse
               Opts#options.top_resource == all ->
                    print_property(Rpcs, Mod, Fd, OpPath, operations, Opts),
                    io:format(Fd, "~s",
                              [delimiter_nl((MatchVer orelse
                                             HasDataNode orelse MatchData)
                                            andalso
                                            (MatchOp orelse HasOpNode)
                                            andalso
                                            (Opts#options.top_resource == all))]);
               true ->
                    skip
            end,

            %% generate this together with root resource
            if Opts#options.top_resource == root
               orelse
               Opts#options.top_resource == all ->
                    print_property([], Mod, Fd, VerPath,
                                   'yang-library-version', Opts),
                    io:format(Fd, "~s",
                              [delimiter_nl((HasDataNode orelse MatchData)
                                            andalso MatchVer
                                            andalso
                                            (Opts#options.top_resource == all))]);
               true ->
                    skip
            end,

            if Opts#options.top_resource == data
               orelse
               Opts#options.top_resource == all ->
                    print_property(DataSns , Mod, Fd, DataPath, data, Opts);
               true ->
                    skip
            end,

            io:format(Fd, "\n~s},\n", [Indent2]),

            %% parameters
            print_parameters(Fd, Opts),
            io:format(Fd, ",\n", []),

            %% responses
            print_responses(Fd),
            io:format(Fd, ",\n", []),

            %% security definitions
            print_security_definitions(Fd),
            io:format(Fd, ",\n", []),

            %% definitions
            print_definitions(Fd, Opts)
    end,

    emit_tree(Ctx, Mods, AllMods, Fd, Opts).

-define(no_leading_comma, false).
-define(leading_comma, true).

print_parameters(Fd, Opts) ->
    Lvl = 1,
    Indent2 = indent(Lvl),
    io:format(Fd, "~s\"parameters\": {", [Indent2]),

    Params = query_param_defs(Lvl + 1),
    io:format(Fd, "~s", [lists:join($, , Params)]),

    F = fun ({_N, P}, Acc) ->
                io:format(Fd, "~s", [[$,|P]]),
                Acc;
            ({_N, Path, P}, Acc) ->
                case match_path(Opts#options.path_filter, Path) of
                    true ->
                        io:format(Fd, "~s", [[$,|P]]),
                        Acc;
                    _ ->
                        Acc
                end
        end,
    ets:foldl(F, true, ?SWAGGER_PARAMS),

    io:format(Fd, "\n~s}", [Indent2]).


print_responses(Fd) ->
    Lvl = 1,
    Indent2 = indent(Lvl),

    io:format(Fd, "~s\"responses\": {", [Indent2]),
    Resps =
        standard_success_defs(Lvl + 1) ++
        standard_error_defs(Lvl + 1),
    io:format(Fd, "~s", [lists:join($, , Resps)]),

    F = fun({_N,R}, Acc) ->
                io:format(Fd, "~s", [[$,|R]]),
                Acc
        end,
    ets:foldl(F, true, ?SWAGGER_RESPS),

    io:format(Fd, "\n~s}", [Indent2]).


print_definitions(Fd, Opts) ->
    Lvl = 1,
    Indent2 = indent(Lvl),
    F = fun({_N, Path, D}, Acc) ->
                case match_path(Opts#options.path_filter, Path) of
                    true ->
                        if (Acc == ?no_leading_comma) ->
                                io:format(Fd, "~s", [D]),
                                ?leading_comma;
                           true ->
                                io:format(Fd, "~s", [[$,|D]]),
                                Acc
                        end;
                    _ ->
                        Acc
                end
        end,
    io:format(Fd, "~s\"definitions\": {", [Indent2]),
    ets:foldl(F, ?no_leading_comma, ?SWAGGER_DEFS),
    io:format(Fd, "\n~s}", [Indent2]).


print_security_definitions(Fd) ->
    Lvl = 1,
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),
    Indent6 = indent(Lvl + 2),
    SecDefs =
        [
         [
          Indent4, "\"basicAuth\": {\n",
          Indent6, "\"type\": \"basic\"\n",
          Indent4, "}"
         ]
        ],
    io:format(Fd, "~s\"securityDefinitions\": {\n", [Indent2]),
    io:format(Fd, "~s", [lists:join($, , SecDefs)]),
    io:format(Fd, "\n~s}", [Indent2]).


api_version(#module{stmt = {_, _, _, StmtL}}) ->
    case lists:keyfind(revision, 1, StmtL) of
        {revision, Version, _, _} ->
            %% use first revision in file
            Version;
        _ ->
            %% FIXME: come up with a better module/api revision date/version
            {{Y,M,D}, {_, _, _}} = erlang:universaltime(),
            io_lib:format("~w-~2..0w-~2..0w", [Y,M,D])
    end.


print_header(Module, Fd, Opts) ->
    Lvl = 1,
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),


    {_, ModuleName, _, StmtL} = Module#module.stmt,
    Title = ?a2b(ModuleName),

    Version = case Opts#options.version of
                  undefined ->
                      api_version(Module);
                  _ ->
                      Opts#options.version
              end,

    Terms = case Opts#options.terms of
                undefined ->
                    "";
                _ ->
                    [Indent4, "\"termsOfService\": \"",
                     Opts#options.terms, "\",\n"]
            end,

    Description = description(StmtL),

    HostHdr =
        if Opts#options.host =:= undefined ->
                "";
           true ->
                Host = Opts#options.host,
                [Indent2, "\"host\": \"", Host, "\",\n"]
        end,

    BasePathBin = ?l2b(tokpath2origpath(Opts#options.path)),

    Tags = case Opts#options.tag_mode of
               methods ->
                   all_http_method_tag_defs(Lvl + 1, Opts#options.methods);
               resources ->
                   all_resource_tag_defs(Lvl + 1, Opts#options.top_resource);
               _ ->
                   %% default to all
                   all_tags_defs(Lvl + 1,
                                 Opts#options.top_resource,
                                 Opts#options.methods)
           end,

    Contact =  case contact(Lvl + 1,
                            Opts#options.contact_name,
                            Opts#options.contact_url,
                            Opts#options.contact_email) of
                   [] ->
                       "";
                   ContactInfo ->
                       [ContactInfo, ",\n"]
               end,

    License =  case license(Lvl + 1,
                            Opts#options.license_name,
                            Opts#options.license_url) of
                   [] ->
                       "";
                   LicInfo ->
                       [LicInfo, ",\n"]
               end,

    SwaggerHdr =
        [
         Indent2, "\"swagger\": \"", ?SWAGGER_VERSION, "\",\n",
         Indent2, "\"info\": {\n",
         Indent4, "\"title\": \"", Title, "\",\n",
         Indent4, "\"description\": \"", Description, "\",\n",
         Terms,
         Contact,
         License,
         Indent4, "\"version\": \"", Version, "\"\n",
         Indent2, "},\n",
         HostHdr,
         Indent2, "\"basePath\": \"", BasePathBin, "\",\n",
         Indent2, "\"tags\": [", lists:join($, , Tags), "\n", Indent2, "],\n",
         Indent2, "\"schemes\": [\n",
         Indent4, "\"http\",\n",
         Indent4, "\"https\"\n",
         Indent2, "],\n",
         fmt_produces(Lvl), ",\n",
         fmt_consumes(Lvl), ",\n"
        ],

    io:format(Fd, "~s", [SwaggerHdr]).

all_http_methods() ->
    [get, head, post, patch, put, options, delete].

all_http_method_tag_defs(Lvl, ValidMethods) ->
    lists:map(fun(M) -> tag_definition(Lvl, M) end,
              filter_http_methods(all_http_methods(), ValidMethods)).

all_resource_tag_defs(Lvl, root) ->
    [tag_definition(Lvl, root)];
all_resource_tag_defs(Lvl, operations) ->
    [tag_definition(Lvl, operations)];
all_resource_tag_defs(Lvl, data) ->
    [tag_definition(Lvl, data)];
all_resource_tag_defs(Lvl, _) ->
    [tag_definition(Lvl, root),
     tag_definition(Lvl, operations),
     tag_definition(Lvl, data)].

all_tags_defs(Lvl, TopResource, ValidMethods) ->
    all_resource_tag_defs(Lvl, TopResource) ++
        all_http_method_tag_defs(Lvl, ValidMethods).

tag_definition(Lvl, Tag) ->
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),
    TagStr = ?a2l(Tag),
    [
     "\n",
     Indent2, "{\n",
     Indent4, "\"name\": \"", TagStr, "\",\n",
     Indent4, "\"description\": \"", TagStr, " resources\"\n",
     Indent2, "}"
    ].

tag(Lvl, Tag) ->
    TagStr = ?a2l(Tag),
    ["\n", indent(Lvl), "\"", TagStr, "\""].


contact(_Lvl, undefined, undefined, undefined) ->
    [];
contact(Lvl, Name, Url, Email) ->
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),
    [
     Indent2, "\"contact\": {\n",
     if Name /= undefined ->
             [Indent4, "\"name\": \"", Name, "\",\n"];
        true ->
             ""
     end,
     if Url /= undefined ->
             [Indent4, "\"url\": \"", Url, "\",\n"];
        true ->
             ""
     end,
     if Email /= undefined ->
             [Indent4, "\"email\": \"", Email, "\""];
        true ->
             ""
     end,
     "\n", Indent2, "}"
    ].


license(_Lvl, undefined, undefined) ->
    [];
license(Lvl, Name, Url) ->
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),
    [
     Indent2, "\"license\": {\n",
     if Name /= undefined ->
             [Indent4, "\"name\": \"", Name, "\",\n"];
        true -> ""
     end,
     if Url /= undefined ->
             [Indent4, "\"url\": \"", Url,  "\""];
        true ->
             ""
     end,
     "\n", Indent2, "}"
    ].


print_property(Chs, Mod, Fd, PathStr, Mode, Opts) ->
    Lvl = 1,

    %% print top path since this also is a valid resource
    PathParams = path_params(Mod, base, Mode, Lvl + 4, Opts),
    Methods = methods(Mod, Mod, PathParams, Mode, base, PathStr, Opts, Lvl + 2),
    Match = match_path(Opts#options.path_filter, PathStr),

    if Match ->
            print_methods(Methods, PathStr, Fd, Lvl + 1);
       true ->
            skip
    end,

    io:format(Fd, "~s",
              [delimiter_nl(has_path_node(Chs, Mod, PathStr, true, Opts)
                            andalso Match)]),
    print_paths(Chs, Mod, Fd, Lvl,
                PathStr, PathParams, Mode, _IsTopNode = true, Opts).


has_path_node([], _Mod, _Path, _IsTopNode, _Opts) ->
    false;
has_path_node(_All = [#sn{kind = Kind, children = Children} = Child | Chs],
              Mod, Path, IsTopNode, Opts) ->
    BasePath = base_path(Child, Mod, Path, IsTopNode),
    Match = match_path(Opts#options.path_filter, BasePath),
    if Kind /= 'choice' andalso Kind /= 'case' andalso Match ->
            %% stop - we found a data node which matches the filter
            true;
       true ->
            ChildPath = child_path(BasePath, Child),
            NextPath = if ChildPath == [] ->
                               BasePath;
                          true ->
                               ChildPath
                       end,
            case has_path_node(Children, Mod, NextPath, false, Opts) of
                true ->
                    %% stop - we found a data node which matches the path filter
                    true;
                _ ->
                    has_path_node(Chs, Mod, Path, IsTopNode, Opts)
            end
    end.


%%
%% run filter on path
%%
match_path(undefined, _Path) ->
    %% NOTE: optional filter not set ... match everything
    true;
match_path([], _Path) ->
    %% NOTE: empty match string -> match nothing
    false;
match_path(MatchStr, Path) ->
    binary:match(iolist_to_binary(Path), iolist_to_binary(MatchStr)) /= nomatch.


print_paths([], _, _, _, _, _, _, _, _) ->
    skip;
print_paths([#sn{kind = SkipKind, children = Children}],
            Mod, Fd, Lvl, Path, PathParams, Mode, _IsTopNode, Opts)
  when SkipKind == 'choice'; SkipKind == 'case' ->
    %% NOTE: skip nodes, but continue with children
    print_paths(Children, Mod, Fd, Lvl, Path, PathParams, Mode, false, Opts);
print_paths([#sn{kind = SkipKind, children = Children} | Chs],
            Mod, Fd, Lvl, Path, PathParams, Mode, IsTopNode, Opts)
  when SkipKind == 'choice'; SkipKind == 'case' ->
    %% NOTE: skip nodes, but continue with children
    print_paths(Children, Mod, Fd, Lvl, Path, PathParams, Mode, false, Opts),

    %% need to peek forward to see if we have (path) nodes that will be written
    %% only print delimiter if we have written something and will write
    %% something
    HasPathNode =
        has_path_node(Children, Mod, Path, false, Opts) andalso
        has_path_node(Chs, Mod, Path, IsTopNode, Opts),
    io:format(Fd, "~s",[delimiter_nl(HasPathNode)]),

    print_paths(Chs, Mod, Fd, Lvl, Path, PathParams, Mode, IsTopNode, Opts);
print_paths([Child|Chs], Mod, Fd, Lvl, Path, PathParams,
            Mode, IsTopNode, Opts) ->
    Children = Child#sn.children,

    %% node content
    BasePath = base_path(Child, Mod, Path, IsTopNode),
    MatchBase = match_path(Opts#options.path_filter, BasePath),

    %% a schema node may have an extra child path, eg. .../list and .../list=key
    ChildPath = child_path(BasePath, Child),
    MatchChild = match_path(Opts#options.path_filter, ChildPath),

    BasePathParams = if Opts#options.omit_path_params ->
                             PathParams;
                        true ->
                             PathParams ++ path_params(Child, base, Mode,
                                                       Lvl + 4, Opts)
                     end,
    BaseMethods = methods(Child, Mod, BasePathParams,
                          Mode, base, BasePath, Opts, Lvl + 2),

    ChildPathParams = if Opts#options.omit_path_params ->
                              PathParams;
                         true ->
                              PathParams ++ path_params(Child, child, Mode,
                                                        Lvl + 4, Opts)
                      end,
    ChildMethods =
        if ChildPath /= [] ->
                methods(Child, Mod, ChildPathParams,
                        Mode, child, ChildPath, Opts, Lvl + 2);
           true ->
                []
        end,

    if MatchBase ->
            print_methods(BaseMethods, BasePath, Fd, Lvl + 1);
       true ->
            skip
    end,

    if ChildMethods /= [], MatchChild ->
            io:format(Fd, "~s", [delimiter_nl(MatchBase)]),
            print_methods(ChildMethods, ChildPath, Fd, Lvl + 1);
       true ->
            skip
    end,

    %% determing next path and next path params
    {NextPath, NextPathParams, MatchNext} =
        if ChildPath == [] ->
                {BasePath, BasePathParams, MatchBase};
           true ->
                {ChildPath, ChildPathParams, MatchChild orelse MatchBase}
        end,

    HasPathNodeChildren = has_path_node(Children, Mod, NextPath, false, Opts),
    io:format(Fd, "~s", [delimiter_nl(HasPathNodeChildren andalso MatchNext)]),

    print_paths(Children, Mod, Fd, Lvl, NextPath,
                NextPathParams, Mode, false, Opts),


    HasPathNodeChs = has_path_node(Chs, Mod, Path, IsTopNode, Opts),
    io:format(Fd, "~s",
              [delimiter_nl(HasPathNodeChs
                            andalso
                              (MatchNext orelse HasPathNodeChildren))]),

    %% continue with other children
    print_paths(Chs, Mod, Fd, Lvl, Path, PathParams, Mode, IsTopNode, Opts).


delimiter_nl([]) ->    "";
delimiter_nl(false) -> "";
delimiter_nl(_)  ->    ",\n".


%% FIXME: don't print empty paths, hmmm. Tricky.
%%% print_methods([], _Path, _Fd, _Lvl) ->
%%%     skip;
print_methods(Methods, Path, Fd, Lvl) ->
    Indent2 = indent(Lvl),
    io:format(Fd, "~s\"~s\": {", [Indent2, Path]),
    io:format(Fd, "~s\n~s}", [lists:join($, , Methods), Indent2]).


base_path(Child, Mod, Path, IsTopNode) ->
    [Path, "/", name(IsTopNode, Child, Mod)].


child_path(BasePath, #sn{name = Name, kind = 'list', keys = Keys}) ->
    [BasePath, $=, fmt_keys(Name, Keys)];
child_path(BasePath,
           #sn{name = Name, kind = 'leaf-list', type = #type{} = _T}) ->
    [BasePath, $=, fmt_key(Name, id)];
child_path(_BasePath, #sn{}) ->
    %% no extra child path exist for this base path
    [].


fmt_keys(_Name, undefined) -> [];
fmt_keys(Name, [K])        -> [fmt_key(Name, K)];
fmt_keys(Name, Keys)       -> fmt_keys(Name, Keys, []).

fmt_keys(_Name, [], Acc)    ->  Acc;
fmt_keys(Name, [K], [])     ->  [fmt_key(Name, K)];
fmt_keys(Name, [K], Acc)    ->  [Acc | [fmt_key(Name, K)]];
fmt_keys(Name, [K|Ks], Acc) ->
    [fmt_key(Name, K), $, | fmt_keys(Name, Ks, Acc)].

fmt_key(Name, K) ->
    %% Mangle module:node same way for both path and param with ref_name2/1.
    [${, ref_name2(fmt_node_name(Name, prefix)), $-, ?a2l(K), $}].


%% Format nodes and augmented nodes
fmt_node_name(Node) -> fmt_node_name(Node, noprefix).

fmt_node_name({ModName, Name}, prefix)    -> ?a2l(ModName) ++ ":" ++ ?a2l(Name);
fmt_node_name({_ModName, Name}, noprefix) -> ?a2l(Name);
fmt_node_name(Name, _)                    -> ?a2l(Name).


-spec is_data_def(atom(), #yctx{}) -> boolean().
is_data_def(Keyword, #yctx{env = #env{data_definition_stmts = D}}) ->
    Keyword /= 'rpc'
        andalso Keyword /= 'action'
        andalso Keyword /= 'notification'
        andalso yang:map_is_key(Keyword, D).


%% In case of a submodule we need to use modulename not name.
name(true = _IsTopNode, #sn{module = Module} = Sn, _Mod) ->
    ModuleName = Module#module.modulename,
    LocalName = local_name(Sn),
    ?a2l(ModuleName) ++ ":" ++ LocalName;
name(false = _IsTopNode, #sn{module = Module} = Sn, Mod) ->
    LocalName = local_name(Sn),
    if Module#module.modulename == Mod#module.modulename ->
            LocalName;
       true ->
            ?a2l(Module#module.modulename) ++ ":" ++ LocalName
    end.


local_name(#sn{name = {_, B}}) -> ?a2l(B);
local_name(#sn{name = A}) -> ?a2l(A).


http_methods(#sn{kind = Kind, config = Config}, Mode, PathType) ->
    http_methods(Kind, Config, Mode, PathType).


http_methods(module = _Kind, undefined, root = _Mode, _PathType) ->
    %% root resource methods
    [get, head];
http_methods(module = _Kind, undefined, 'yang-library-version' = _Mode,
             _PathType) ->
    %% yang-library-version resource methods
    [get, head];
http_methods(module = _Kind, undefined, operations = _Mode, _PathType) ->
    %% top operations resource methods
    [get];
http_methods(module = _Kind, undefined, data = _Mode, _PathType) ->
    %% top data resource methods
    [get, head, post, put, patch];
http_methods(leaf = _Kind, true = _Config, data = _Mode, _PathType) ->
    %% leaf - exclude POST on leaf
    [get, head, put, patch, delete, options];
http_methods(list = _Kind, true = _Config, data = _Mode, base = _PathType) ->
    %% list - list top path, eg. .../<list>
    %% NOTE: exclude everything except OPTIONS for base path
    [options];
http_methods(list = _Kind, true = _Config, data = _Mode, child = _PathType) ->
    %% list - list top path, eg. .../<list>=<key>
    %% NOTE: exclude POST
    [get, head, put, patch, delete, options];
http_methods('leaf-list' = _Kind, true = _Config, data = _Mode, base) ->
    %% leaf-list - top path, eg. .../<leaf-list>
    %% NOTE: exclude everything except OPTIONS for base path
    [options];
http_methods('leaf-list' = _Kind, true = _Config, data = _Mode, child) ->
    %% leaf-list - top path, eg. .../<leaf-list>=<instance>
    %% NOTE: exclude POST for base path
    [get, head, put, patch, delete, options];
http_methods(_Kind, true = _Config, data =  _Mode, _PathType) ->
    %% all other configurable nodes: container, ...
    [get, head, post, put, patch, delete, options];
http_methods(operation = _Kind, _Config, Mode, _PathType)
  when Mode == data; Mode == operations ->
    %% NOTE: operation - both rpcs and actions
    [post];
http_methods(notification = _Kind, _Config, data = _Mode, _PathType) ->
    %% notifications
    %% FIXME: add notification support?
    [];
http_methods(_Kind, false = _Config, _Mode, _PathType) ->
    %% oper data
    [get, head];
http_methods(_Kind, _Config, _Mode, _PathType) ->
    [].


filter_http_methods(Methods, ValidMethods) ->
    Pred = fun(M) -> lists:member(M, ValidMethods) end,
    lists:filter(Pred, Methods).


methods(#module{kind = Kind, stmt = {_, _ModuleName, _, StmtL}} = Mod, Mod,
        PathParams, Mode, PathType, Path, Opts, Lvl) ->
    %% root and top level methods
    HttpMethods = filter_http_methods(
                    http_methods(Kind, undefined, Mode, PathType),
                    Opts#options.methods),
    Description = description(StmtL),
    methods(HttpMethods, Description, Description,
            Path, PathType, PathParams, Mode, Mod, Mod, Opts, Lvl);
methods(#sn{module = _Mod, stmt = {_, _, _, StmtL}} = Sn, Mod,
        PathParams, Mode, PathType, Path, Opts, Lvl) ->
    %% schema node methods
    HttpMethods = filter_http_methods(http_methods(Sn, Mode, PathType),
                                      Opts#options.methods),
    Description = description(StmtL),
    methods(HttpMethods, Description, Description,
            Path, PathType, PathParams, Mode, Sn, Mod, Opts, Lvl).


%% NOTE: Schema Node (Sn) may also be a module
methods(HttpMethods, Summary, Description,
        Path, PathType, PathParams, Mode, Sn, Mod, Opts, Lvl) ->
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),
    F = fun(HttpMethod) ->
                BodyParams =
                    if Opts#options.omit_body_params ->
                            [];
                       true ->
                            body_params(HttpMethod, Path, PathType,
                                        Mode, Sn, Mod, Lvl + 2, Opts)
                    end,
                QueryParams =
                    if Opts#options.omit_query_params ->
                            [];
                       true ->
                            query_params(HttpMethod, Path, PathType,
                                         Mode, Sn, Mod, Lvl + 2)
                    end,
                HeaderParams =
                    if Opts#options.omit_header_params ->
                            [];
                       true ->
                            header_params(HttpMethod, Path, PathType,
                                          Mode, Sn, Mod, Lvl + 2)
                    end,
                FormParams =
                    if Opts#options.omit_form_params ->
                            [];
                       true ->
                            form_params(HttpMethod, Path, PathType,
                                        Mode, Sn, Mod, Lvl + 2)
                    end,
                ConcatParams =
                    PathParams ++ BodyParams ++ QueryParams ++
                    HeaderParams ++ FormParams,
                Responses = responses(HttpMethod, Path, PathType,
                                      Mode, Opts, Sn, Mod, Lvl + 2),
                Security = security(HttpMethod, PathType, Mode, Sn, Lvl + 2),
                Tags = case Opts#options.tag_mode of
                           methods   -> [tag(Lvl + 2, HttpMethod)];
                           resources -> [tag(Lvl + 2, Mode)];
                           _         -> [tag(Lvl + 2, Mode),
                                         tag(Lvl + 2, HttpMethod)]
                       end,
                OperationId = operation_id(Path, HttpMethod),
                [
                 "\n",
                 Indent2, "\"", ?a2l(HttpMethod), "\": {\n",
                 Indent4, "\"tags\": [", lists:join($, , Tags), "\n",
                 Indent4, "],\n",
                 Indent4, "\"summary\": \"", Summary, "\",\n",
                 Indent4, "\"description\": \"", Description, "\",\n",
                 Indent4, "\"operationId\": \"", OperationId, "\",\n",
                 fmt_produces(Lvl + 1), ",\n",
                 fmt_params(Lvl + 1, ConcatParams), ",\n",
                 Indent4, "\"responses\": {",
                 lists:join($, , Responses),
                 "\n", Indent4, "},\n",
                 Indent4, "\"security\": [\n",
                 lists:join($, , Security),
                 Indent4, "]\n",
                 Indent2, "}"
                ]
        end,
    [F(M) || M <- HttpMethods].


%% FIXME: more security options/support needed?
security(_HttpMethod, _PathType, _Mode, _Sn, Lvl) ->
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),
    [
     [Indent2, "{\n", Indent4, "\"basicAuth\": []\n", Indent2, "}\n"]
    ].


%% FIXME: customize description depending on method, etc ...
%% NOTE: need to support both #sn{} and #module{}
responses(post = HttpMethod, Path, PathType, Mode, Opts,
          #sn{kind = operation,
              stmt = {_, _, _, StmtL},
              children = Chs} = Sn, Mod, Lvl) ->
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),
    DefLvl = 2,
    DIndent2 = indent(DefLvl),
    DIndent4 = indent(DefLvl + 1),
    Body = body(json, _IsTop = true, response, HttpMethod, PathType,
                Mode, Sn, Mod, DefLvl + 2, Opts),
    OutDesc0 =  case [C || C <- Chs, C#sn.kind == output] of
                    [] ->
                        description(StmtL);
                    [#sn{stmt = {_, _, _, OutStmtL}}] ->
                        description(OutStmtL)
                end,
    HttpStatus = http_status(HttpMethod, PathType, Sn),
    OutDesc = case OutDesc0 of
                  [] ->
                      description(HttpStatus);
                  _ ->
                      OutDesc0
              end,

    ParamName = ref_name([Path, $-, ?a2l('post-output')]),
    BodyDefinition =
        [
         "\n",
         DIndent2, "\"", ParamName, "\": {\n",
         %% type & format
         fmt_type(DefLvl + 1, {object, undefined}), ",\n",
         DIndent4, "\"properties\": {",
         Body, "\n",
         DIndent4, "}\n",
         DIndent2, "}"
        ],
    store_defs(?SWAGGER_DEFS, [{ParamName, Path, BodyDefinition}]),

    [
     [
      "\n",
      Indent2, "\"", HttpStatus, "\": {\n",
      Indent4, "\"description\": \"", OutDesc, "\",\n",
      Indent4, "\"schema\": {",
      fmt_ref(Lvl + 2, definitions, ParamName), "\n",
      Indent4, "}\n",
      Indent2, "}"
     ]
    ] ++ if Opts#options.omit_standard_statuses ->
                 [];
            true ->
                 fmt_refs(Lvl,
                          responses, ["204" | standard_error_statuses()])
         end;
responses(HttpMethod, _Path, PathType, _Mode, Opts,
          #sn{name = Name, kind = Kind, stmt = {_, _, _, _StmtL}} = Sn,
          _Mod, Lvl)
  when HttpMethod == post; HttpMethod == patch; HttpMethod == put ->
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),
    {DescEffect, StdErrors}
        = case HttpMethod of
              post ->  {"created", ["204" | standard_error_statuses()]};
              patch -> {"updated", standard_error_statuses()};
              put ->   {"created or replaced",
                        ["204" | standard_error_statuses()]}
          end,
    [
     [
      "\n",
      Indent2, "\"", http_status(HttpMethod, PathType, Sn), "\": {\n",
      Indent4, "\"description\": \"", ?a2l(Kind), " ", fmt_node_name(Name), " ",
      DescEffect, "\"\n",
      Indent2, "}"
     ]
    ] ++ if Opts#options.omit_standard_statuses ->
                 [];
            true ->
                 fmt_refs(Lvl, responses, StdErrors)
         end;
responses(get = HttpMethod, Path, PathType, Mode, Opts, SnOrMod, Mod, Lvl) ->
    StmtL = case SnOrMod of
                #sn{stmt = {_, _, _, StmtList}} -> StmtList;
                #module{stmt = {_, _, _, StmtList}} -> StmtList
            end,
    %% FIXME: make method specific descriptions?
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),
    DefLvl = 2,
    DIndent2 = indent(DefLvl),
    DIndent4 = indent(DefLvl + 1),
    Body = body(json, _IsTop = true, response, HttpMethod, PathType,
                Mode, SnOrMod, Mod, DefLvl + 2, Opts),
    HttpStatus = http_status(HttpMethod, PathType, SnOrMod),
    Desc = case description(StmtL) of
               [] ->
                   description(HttpStatus);
               D ->
                   D
           end,

    ParamName = ref_name(Path),
    BodyDefinition =
        [
         "\n",
         DIndent2, "\"", ParamName, "\": {\n",
         %% type & format
         fmt_type(DefLvl + 1, {object, undefined}), ",\n",
         DIndent4, "\"properties\": {",
         Body, "\n",
         DIndent4, "}\n",
         DIndent2, "}"
        ],
    store_defs(?SWAGGER_DEFS, [{ParamName, Path, BodyDefinition}]),

    [
     [
      "\n",
      Indent2, "\"", HttpStatus, "\": {\n",
      Indent4, "\"description\": \"", Desc, "\",\n",
      Indent4, "\"schema\": {",
      fmt_ref(Lvl + 2, definitions, ParamName), "\n",
      Indent4, "}\n",
      Indent2, "}"
     ]
    ] ++ if Opts#options.omit_standard_statuses ->
                 [];
            true ->
                 fmt_refs(Lvl,
                          responses, ["204" | standard_error_statuses()])
         end;
responses(delete, _Path, PathType, _Mode, _Opts, SnOrMod, _Mod, Lvl) ->
    fmt_refs(Lvl, responses, [http_status(delete, PathType, SnOrMod)]);
responses(HttpMethod, _Path, PathType, _Mode, _Opts, SnOrMod, _Mod, Lvl)
  when HttpMethod == options; HttpMethod == head ->
    fmt_refs(Lvl, responses, [http_status(HttpMethod, PathType, SnOrMod)]);
responses(HttpMethod, _Path, PathType, _Mode, Opts, SnOrMod, _Mod, Lvl) ->
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),
    StmtL = case SnOrMod of
                #sn{stmt = {_, _, _, StmtList}} -> StmtList;
                #module{stmt = {_, _, _, StmtList}} -> StmtList
            end,
    HttpStatus = http_status(HttpMethod, PathType, SnOrMod),
    Desc = case description(StmtL) of
               [] ->
                   description(HttpStatus);
               D ->
                   D
           end,
    [
     [
      "\n",
      Indent2, "\"", HttpStatus, "\": {\n",
      Indent4, "\"description\": \"", Desc, "\"\n",
      Indent2, "}"
     ]
    ] ++ if Opts#options.omit_standard_statuses ->
                 [];
            true ->
                 fmt_standard_error_refs(Lvl)
         end.


fmt_params(Lvl, Params) ->
    Indent2 = indent(Lvl),
    [Indent2, "\"parameters\": [", lists:join($, , Params), "\n", Indent2, "]"].


operation_id(Path, HttpMethod) ->
    operation_id2(lists:flatten(Path), HttpMethod).

%% PRE: given Path need to be a "flatten" list
operation_id2([$/], HttpMethod) ->
    operation_id2("root", HttpMethod);
operation_id2([$/ | T], HttpMethod) when T /= [] ->
    operation_id2(T, HttpMethod);
operation_id2(Path, HttpMethod) ->
    F = fun($/) -> [$_];
           ($:) -> [$_];
           (${) -> [];
           ($}) -> [];
           ($=) -> [$_];
           ($-) -> [$_];
           ($,) -> [$_];
           (C) ->  [C]
        end,
    lists:flatmap(F, lists:flatten([Path, $_, ?a2l(HttpMethod)])).


%%
%% fixing reference names since some codegen tools don't support:
%%   i.   Json pointers (RFC 6901), i.e. don't like '~0' and '~1' in references
%%   ii.  '/' in references
%%   iii. ':' in references
%%   iv.  ... and more erlang stuff ...
%%
%% NOTE: this is a side effect from that we are using the 'path' as ref name
%%
ref_name(Name) ->
    ref_name2(lists:flatten(Name)).

%% PRE: given Name need to be a "flatten" list
ref_name2([$/]) ->
    ref_name2("root");
ref_name2([$/ | T]) when T /= [] ->
    ref_name2(T);
ref_name2(Name) ->
    F = fun($/) -> [$_];
           ($:) -> [$_];
           (${) -> [];
           ($}) -> [];
           ($=) -> [$_];
           ($,) -> [$_];
           (C) ->  [C]
        end,
    lists:flatmap(F, lists:flatten(Name)).

%%
%% RFC 6901 - Json pointer:
%%   o  '~' encoded with '~0'
%%   o  '/' encoded with '~1'
%%
json_pointer(Pointer) when is_list(Pointer) ->
    F = fun($~) -> [$~,$0];
           ($/) -> [$~,$1];
           (C) ->  [C]
        end,
    lists:flatmap(F, lists:flatten(Pointer)).


fmt_refs(Lvl, RefKind, RefNames) ->
    lists:map(fun(N) -> fmt_ref(Lvl, RefKind, N) end, RefNames).


fmt_ref(Lvl, definitions = RefKind, RefName) ->
    %% these references are injected as is
    Name = json_pointer(to_string(RefName)),
    ["\n", indent(Lvl), "\"$ref\": \"#/", ?a2l(RefKind), "/", Name, "\""];
fmt_ref(Lvl, parameters = RefKind, RefName) ->
    %% these references are included in a array
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),
    Name = json_pointer(to_string(RefName)),
    ["\n",
     Indent2, "{\n",
     Indent4, "\"$ref\": \"#/", ?a2l(RefKind), "/", Name, "\"\n",
     Indent2, "}"];
fmt_ref(Lvl, responses = RefKind, RefName) ->
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),
    %% these references are json key/value pairs
    Name = json_pointer(to_string(RefName)),
    [
     "\n",
     Indent2, "\"", Name, "\": ", "{\n",
     Indent4, "\"$ref\": \"#/", ?a2l(RefKind), "/", Name, "\"\n",
     Indent2, "}"
    ].


to_string(Name) ->
    if is_list(Name) -> Name;
       is_atom(Name) -> ?a2l(Name)
    end.


http_status(get, _PathType, _Sn)              -> "200";
http_status(post, _PathType, _Sn)             -> "201";
http_status(put, _PathType, _Sn)              -> "201";
http_status(patch, _PathType, _Sn)            -> "204";
http_status(delete, _PathType, _Sn)           -> "204";
http_status(options, _PathType, _Sn)          -> "200";
http_status(head, _PathType, _Sn)             -> "200".


standard_success_statuses() -> ["200", "201", "204"].
standard_error_statuses()   -> ["400", "401", "404", "405", "409"].

standard_success_defs(Lvl) ->  fmt_statuses(Lvl, standard_success_statuses()).
standard_error_defs(Lvl) ->    fmt_statuses(Lvl, standard_error_statuses()).

fmt_standard_error_refs(Lvl) ->
    fmt_refs(Lvl, responses, standard_error_statuses()).

fmt_statuses(Lvl, Statuses) ->
    lists:map(fun(S) -> fmt_status(Lvl, S) end, Statuses).

fmt_status(Lvl, Status) ->
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),
    [
     "\n",
     Indent2, "\"", Status, "\": {\n",
     Indent4, "\"description\": \"", description(Status), "\"\n",
     Indent2, "}"
    ].

description("200") -> "OK";
description("201") -> "Created";
description("204") -> "No Content";
description("400") -> "Bad Request";
description("401") -> "Unauthorized";
description("404") -> "Not Found";
description("405") -> "Method Not Allowed";
description("409") -> "Conflict";
description(StmtL) ->
    try
        {description, Description, _, _} = lists:keyfind(description, 1, StmtL),
        escape(Description)
    catch
        _:_ ->
            ""
    end.

fmt_produces(Lvl) ->
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),
    [
     Indent2, "\"produces\": [\n",
%%%     Indent4, "\"application/yang-data+xml\",\n",
     Indent4, "\"application/yang-data+json\"\n",
     Indent2, "]"
    ].

fmt_consumes(Lvl) ->
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),
    [
     Indent2, "\"consumes\": [\n",
%%%     Indent4, "\"application/yang-data+xml\",\n",
     Indent4, "\"application/yang-data+json\"\n",
     Indent2, "]"
    ].



fmt_type(Lvl, {Type, undefined}) when is_atom(Type) ->
    %% already converted swagger type, no formatting
    Indent2 = indent(Lvl),
    [Indent2, "\"type\": \"", ?a2l(Type), "\""];
fmt_type(Lvl, {Type, Format}) when is_atom(Type) ->
    %% already converted swagger type, has formatting
    Indent2 = indent(Lvl),
    Fmt = fmt_format(Lvl, Format),
    [
     Indent2, "\"type\": \"", ?a2l(Type), "\"", delimiter_nl(Fmt), Fmt
    ].


fmt_format(Lvl, Format) when is_atom(Format) ->
    Indent2 = indent(Lvl),
    [Indent2, "\"format\": \"", ?a2l(Format), "\""];
fmt_format(Lvl, {enumeration = Format, Enums}) ->
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),
    DefaultStr =
        case Enums of
            [] -> [];
            [{Enum, _} | _] -> [Indent2, "\"default\": \"", Enum, "\",\n"]
        end,
    EnumsStr =
        lists:join(
          $, , [["\n", Indent4, "\"", E, "\""] ||
                  {E,_V} <- lists:keysort(2, Enums)]),
    [Indent2, "\"format\": \"", ?a2l(Format), "\",\n",
     DefaultStr,
     Indent2, "\"enum\": [", EnumsStr, "\n", Indent2, "]"];
fmt_format(_Lvl, _Format) ->
    [].

%%
%% convert to Swagger types with potential specific formats
%%
%% NOTE: argument is #type.base
%%
type_and_format(Type, false) ->
    type_and_format(Type);
type_and_format(Type, true) ->
    case type_and_format(Type) of
        {integer, int64} ->
            {string, int64};
        {integer, uint64} ->
            {string, uint64};
        Else ->
            Else
    end.

type_and_format(boolean) ->                {boolean, undefined};
type_and_format(leafref) ->                {string, leafref};
type_and_format(union) ->                  {string, union};
type_and_format(int64) ->                  {integer, int64};
type_and_format(int32) ->                  {integer, int32};
type_and_format(int16) ->                  {integer, int16};
type_and_format(int8) ->                   {integer, byte};
type_and_format('instance-identifier') ->  {string, 'instance-identifier'};
type_and_format(identityref) ->            {string, identityref};
type_and_format(enumeration) ->            {string, enumeration};
type_and_format(empty) ->                  {string, '[null]'};
type_and_format(binary) ->                 {string, binary};
type_and_format(decimal64) ->              {number, double};
type_and_format(uint8) ->                  {integer, byte};
type_and_format(uint16) ->                 {integer, uint16};
type_and_format(uint32) ->                 {integer, uint32};
type_and_format(uint64) ->                 {integer, uint64};
type_and_format(#typedef{type = #type{} = T}) ->  type_and_format(T);
type_and_format(T = #type{base = enumeration}) -> type_and_format_enum(T);
type_and_format(#type{base = Base}) ->            type_and_format(Base);
type_and_format(YangType) ->                      {string, YangType}.

type_and_format_enum(#type{base = enumeration,
                           type_spec = #enumeration_type_spec{
                             enums = Enums}}) ->
    type_and_format_enum(Enums);
type_and_format_enum(Enums) when is_list(Enums) ->
    {string, {enumeration, escape_enums(Enums)}}.

escape_enums(Enums) ->
    F = fun({E, V}) when is_atom(E) ->
                {escape(E), V};
           ({E, V}) when is_list(E) ->
                {escape(E), V};
           (Enum) ->
                Enum
        end,
    lists:map(F, Enums).


%% NOTE: schema node (Sn) may also be a module
%% PRE:  name is unique for the whole schema
path_params(#sn{name = Name, kind = 'list', keys = Keys, children = Chs,
                module = Mod},
            child = _PathType, _Mode, Lvl, Opts) ->
    %% Find list key children; filter out 'false', i.e. not found nodes.
    [P || P <- [path_param(Name, find_sn(Key, Mod, Chs), Lvl, Opts) ||
                   Key <- Keys],
          P /= false];
path_params(Sn = #sn{name = Name, kind = 'leaf-list'},
            child = _PathType, _Mode, Lvl, Opts) ->
    %% NOTE: using leaf-list base type as instance value
    [path_param(Name, Sn, Lvl, Opts)];
path_params(_Sn, _PathType, _Mode, _Lvl, _Opts) ->
    [].


%% Need to handle keys (atoms) and augmented keys (tuples: {module-name, key}).
-spec find_sn(atom(), #module{}, [#sn{}]) -> #sn{} | [].
find_sn(Key, Mod, Chs) ->
    case lists:keyfind(Key, #sn.name, Chs) of
        Sn = #sn{} -> Sn;
        false      -> lists:keyfind({Mod#module.name, Key}, #sn.name, Chs)
    end.



%%
%% POST: resulting parameter name need to be unique
%%
path_param(ParentName,
           #sn{name = Name0, kind = Kind,
               type = #type{} = T, stmt = {_, _, _, StmtL}},
           Lvl, Opts) ->
    DefLvl = 2,
    DIndent2 = indent(DefLvl),
    DIndent4 = indent(DefLvl + 1),
    Name = case Kind of
               'leaf-list' ->
                   id;
               _ ->
                   Name0
           end,

    %% NOTE: this does not make parameter name globally unique, only unique
    %%       in context of parent, ... hopefully enough for path params?
    ParamName = ref_name([fmt_node_name(ParentName, prefix), $-,
                          fmt_node_name(Name)]),
    PathParam =
        [
         "\n",
         DIndent2, "\"", ParamName, "\": {\n",
         DIndent4, "\"name\": \"", ParamName, "\",\n",
         DIndent4, "\"in\": \"path\",\n",
         DIndent4, "\"description\": \"", description(StmtL), "\",\n",
         DIndent4, "\"required\": true,\n",
         fmt_type(DefLvl + 1,
                  type_and_format(T, Opts#options.int64_as_string)), "\n",
         DIndent2, "}"
        ],
    store_defs(?SWAGGER_PARAMS, [{ParamName, PathParam}]),
    fmt_ref(Lvl, parameters, ParamName).



%%% required(StmtL) ->
%%%     case yang:search_one_stmt('mandatory', StmtL) of
%%%         {_, true, _, _} ->
%%%             true;
%%%         _ ->
%%%             false
%%%     end.


has_presence(StmtL) ->
    case yang:search_one_stmt('presence', StmtL) of
        {_, _, _, _} ->
            true;
        _ ->
            false
    end.

format_is_presence(true) ->  "presence";
format_is_presence(false) -> "non-presence".


%% NOTE: schema node (Sn) may also be a module
body_params(HttpMethod, _Path, _PathType, _Mode, _Sn, _Mod, _Lvl, _Opts)
  when HttpMethod == options; HttpMethod == get;
       HttpMethod == head; HttpMethod == delete ->
    %% NOTE: exclude body from OPTIONS, GET, HEAD and DELETE
    [];
body_params(HttpMethod, _Path, _PathType, _Mode, #sn{kind = Kind},
            _Mod, _Lvl, _Opts)
  when Kind == leaf, HttpMethod == post ->
    %% NOTE: exclude POST on leaf
    [];
body_params(HttpMethod, Path, PathType, Mode, #sn{} = Sn, Mod, Lvl, Opts)
  when HttpMethod == patch; HttpMethod == put; HttpMethod == post ->
    [body_param(HttpMethod, Path, PathType, Mode, Sn, Mod, Lvl, Opts)];
body_params(HttpMethod, Path, PathType, data = Mode, #module{} = Mod,
            Mod, Lvl, Opts)
  when HttpMethod == patch; HttpMethod == put; HttpMethod == post ->
    [body_param(HttpMethod, Path, PathType, Mode, Mod, Mod, Lvl, Opts)];
body_params(_HttpMethod, _Path, _PathType, _Mode, _Sn, _Mod, _Lvl, _Opts) ->
    [].


body_param(HttpMethod, Path, PathType, Mode, SnOrMod, Mod, Lvl, Opts) ->
    {Name, Kind, StmtL, Chs} =
        case SnOrMod of
            #sn{name = SnName, kind = SnKind,
                stmt = {_, _, _, SnStmtL}, children = SnChs} ->
                {SnName, SnKind, SnStmtL, SnChs};
            #module{kind = SnKind,
                    stmt = {_, _, _, SnStmtL}, children = SnChs} ->
                %% NOTE: this can only be /restconf/data POST, PUT, PATCH
                %% in case of PUT or PATCH top node in body need to be 'data'
                {data, SnKind, SnStmtL, SnChs}
        end,

    %% NOTE: definition indention is not relative from Lvl
    DefLvl = 2,
    DIndent2 = indent(DefLvl),
    DIndent4 = indent(DefLvl + 1),

    Body = case HttpMethod of
               post ->
                   lists:join(
                     $, ,
                     [C2 || C2 <- [body(json, _IsTop = true, param, HttpMethod,
                                        PathType, Mode, C, Mod, DefLvl + 2,
                                        Opts) ||
                                      C <- Chs], C2 /= []]);
               _ ->
                   body(json, _IsTop = true, param, HttpMethod, PathType, Mode,
                        SnOrMod, Mod, DefLvl + 2, Opts)
           end,

    %% NOTE: unique body param name: <path>[[-post[-input]]|[-put-patch]],
    %%       since POST bodies differ from other methods
    ParamName0 = case {HttpMethod, Kind} of
                     {post, operation} ->
                         [Path, $-, ?a2l('post-input')];
                     {post, _} ->
                         [Path, $-, ?a2l(HttpMethod)];
                     _ when is_record(SnOrMod, module), HttpMethod == patch;
                            is_record(SnOrMod, module), HttpMethod == put ->
                         %% NOTE: need to distuingish from GET /restconf/data
                         %%       response definition name.
                         [Path, $-, "put-patch"];
                     _ ->
                         Path
                 end,
    ParamName = ref_name(ParamName0),

    BodyDefinition =
        [
         "\n",
         DIndent2, "\"", ParamName, "\": {\n",
         %% type & format
         fmt_type(DefLvl + 1, {object, undefined}), ",\n",
         %% required properties
         %% FIXME: 'required' should be taken from body properties, hmmm.
         %%        if/when we add required properties for body params we can
         %%        not share definition between body param and response body
         %%        which we do today, hmmm
         %% properties
         DIndent4, "\"properties\": {",
         Body, "\n",
         DIndent4, "}\n",
         DIndent2, "}"
        ],
    store_defs(?SWAGGER_DEFS, [{ParamName, Path, BodyDefinition}]),

    BodyParam =
        [
         "\n",
         DIndent2, "\"", ParamName, "\": {\n",
         DIndent4, "\"name\": \"", fmt_node_name(Name), "\",\n",
         DIndent4, "\"in\": \"body\",\n",
         DIndent4, "\"description\": \"", description(StmtL), "\",\n",
         DIndent4, "\"required\": true,\n",
         DIndent4, "\"schema\": {",
         fmt_ref(DefLvl + 2, definitions, ParamName), "\n",
         DIndent4, "}\n",
         DIndent2, "}"
        ],
    store_defs(?SWAGGER_PARAMS, [{ParamName, Path, BodyParam}]),
    fmt_ref(Lvl, parameters, ParamName).


%%
%% generate a body from a schema node. Used in body/form params and responses
%%
body(json, IsTop, Context, HttpMethod, PathType, Mode,
     #sn{kind = choice, children = Chs}, Mod, Lvl, Opts) ->
    %% choice - include all cases, even though only one is valid
    ChoiceProps =
        [C || C <- [body(json, IsTop, Context, HttpMethod, PathType,
                         Mode, Child, Mod, Lvl, Opts) ||
                       Child <- Chs], C /= []],
    lists:join($, , ChoiceProps);

body(json, IsTop, Context, HttpMethod, PathType, Mode,
     #sn{kind = 'case', children = Chs}, Mod, Lvl, Opts) ->
    %% case - continue with case children
    CaseProps =
        [C || C <- [body(json, IsTop, Context, HttpMethod, PathType,
                         Mode, Child, Mod, Lvl, Opts) ||
                       Child <- Chs], C /= []],
    lists:join($, , CaseProps);
body(json, IsTop, _Context, _HttpMethod, _PathType, _Mode,
     #sn{kind = leaf,
         type = #type{} = T,
         stmt = {_, _, _, StmtL}} = Sn, Mod, Lvl, Opts) ->
    %% leaf - json format of a leaf
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),
    Indent6 = indent(Lvl + 2),
    TypeAndFormat = type_and_format(T, Opts#options.int64_as_string),
    Desc = case description(StmtL) of
               [] -> ["(leaf)"];
               D ->  [D, " (leaf)"]
           end,
    [
     "\n",
     Indent2, "\"", name(IsTop, Sn, Mod), "\": {\n",
     Indent4, "\"description\": \"", Desc, "\",\n",
     Indent4, "\"x-yang\": {\n",
     Indent6, "\"type\": \"leaf\"\n",
     Indent4, "},\n",
     fmt_type(Lvl + 1, TypeAndFormat), "\n",
     Indent2, "}"
    ];
body(json, IsTop, _Context, _HttpMethod, _PathType, _Mode,
     #sn{kind = 'leaf-list',
         type = #type{} = T,
         stmt = {_, _, _, StmtL}} = Sn, Mod, Lvl, Opts) ->
    %% leaf-list - json format of a leaf-list
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),
    Indent6 = indent(Lvl + 2),
    TypeAndFormat = type_and_format(T, Opts#options.int64_as_string),
    Desc = case description(StmtL) of
               [] -> ["(leaf-list)"];
               D ->  [D, " (leaf-list)"]
           end,
    [
     "\n",
     Indent2, "\"", name(IsTop, Sn, Mod), "\": {\n",
     fmt_type(Lvl + 1, {array, undefined}), ",\n",
     Indent4, "\"x-yang\": {\n",
     Indent6, "\"type\": \"leaf-list\"\n",
     Indent4, "},\n",
     Indent4, "\"items\": {\n",
     Indent6, "\"description\": \"", Desc, "\",\n",
     fmt_type(Lvl + 2, TypeAndFormat), "\n",
     Indent4, "}\n",
     Indent2, "}"
    ];
body(json, IsTop, Context, HttpMethod, PathType, Mode,
     #sn{kind = list,
         children = Chs,
         stmt = {_, _, _, StmtL}} = Sn, Mod, Lvl, Opts) ->
    %% list - json format of a list with children
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),
    Indent6 = indent(Lvl + 2),
    Desc = case description(StmtL) of
               [] -> ["(list)"];
               D ->  [D, " (list)"]
           end,
    ChildProperties =
        [C || C <- [body(json, false, Context, HttpMethod, PathType,
                         Mode, Child, Mod, Lvl + 3, Opts) ||
                       Child <- Chs], C /= []],
    [
     "\n",
     Indent2, "\"", name(IsTop, Sn, Mod), "\": {\n",
     fmt_type(Lvl + 1, {array, undefined}), ",\n",
     Indent4, "\"description\": \"", Desc, "\",\n",
     Indent4, "\"x-yang\": {\n",
     Indent6, "\"type\": \"list\"\n",
     Indent4, "},\n",
     Indent4, "\"items\": {\n",
     %% children
     fmt_type(Lvl + 2, {object, undefined}), ",\n",
     Indent6, "\"properties\": {",
     %% child properties
     lists:join($, , ChildProperties), "\n",
     Indent6, "}\n",
     Indent4, "}\n",
     Indent2, "}"
    ];
body(json, IsTop, Context, HttpMethod, PathType, Mode,
     #sn{kind = container,
         children = Chs,
         stmt = {_, _, _, StmtL}} = Sn, Mod, Lvl, Opts) ->
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),
    Indent6 = indent(Lvl + 2),
    IsPresence = has_presence(StmtL),
    Presence = format_is_presence(IsPresence),
    Desc = case description(StmtL) of
               [] -> ["(", Presence, ")"];
               D ->  [D, " (", Presence, ")"]
           end,

    %% container - json format of container with children
    ChildProperties =
        [C || C <- [body(json, false, Context, HttpMethod, PathType,
                         Mode, Child, Mod, Lvl + 2, Opts) ||
                       Child <- Chs], C /= []],
    [
     "\n",
     Indent2, "\"", name(IsTop, Sn, Mod), "\": {\n",
     Indent4, "\"description\": \"", Desc, "\",\n",
     fmt_type(Lvl + 1, {object, undefined}), ",\n",
     Indent4, "\"x-yang\": {\n",
     Indent6, "\"type\": \"container\",\n",
     Indent6, "\"is_presence\": \"", ?a2l(IsPresence), "\"\n",
     Indent4, "},\n",
     %% children
     Indent4, "\"properties\": {",
     %% child properties
     lists:join($, , ChildProperties), "\n",
     Indent4, "}\n",
     Indent2, "}"
    ];
body(json, _IsTop, _Context, HttpMethod, _PathType, _Mode,
     #sn{kind = operation}, _Mod, _Lvl, _Opts)
  when HttpMethod /= post, HttpMethod /= get ->
    %% let through operation for a POST and GET - omit the rest
    [];
body(json, IsTop, response = _Context, get = _HttpMethod, _PathType,
     operations = _Mode, #sn{kind = operation} = Sn, Mod, Lvl, _Opts) ->
    %% GET response on /restconf/operations request
    Indent2 = indent(Lvl),
    TypeAndFormat = {_Type, _Format} = type_and_format(empty),
    %% FIXME: maybe consider rpc path, hmmm.
    [
     "\n",
     Indent2, "\"", name(IsTop, Sn, Mod), "\": {\n",
     fmt_type(Lvl + 1, TypeAndFormat), "\n",
     Indent2, "}"
    ];
body(json, _IsTop, response = _Context, get = _HttpMethod, _PathType,
     data = _Mode, #sn{kind = operation} = _Sn, _Mod, _Lvl, _Opts) ->
    %% GET response on /restconf/data/.../<action>
    [];
body(json, IsTop, response = Context, post = HttpMethod, PathType, Mode,
     #sn{kind = operation, children = Chs}, Mod, Lvl, Opts) ->
    %% response operation for a POST method - continue with 'output' children
    OutputChs = [C || C <- Chs, C#sn.kind == output],
    OutPs = [C || C <- [body(json, IsTop, Context, HttpMethod, PathType,
                             Mode, OutC, Mod, Lvl, Opts) || OutC <- OutputChs],
                  C /= []],
    lists:join($, , OutPs);
body(json, IsTop, param = Context, post = HttpMethod, PathType, Mode,
     #sn{kind = operation, children = Chs}, Mod, Lvl, Opts) ->
    %% body param operation for a POST method - continue with 'input' children
    InputChs = [C || C <- Chs, C#sn.kind == input],
    InPs = [C || C <- [body(json, IsTop, Context, HttpMethod, PathType,
                            Mode, InC, Mod, Lvl, Opts) ||
                          InC <- InputChs], C /= []],
    lists:join($, , InPs);
body(json, IsTop, response = Context, post = HttpMethod, PathType, Mode,
     #sn{kind = output, children = Chs}, Mod, Lvl, Opts) ->
    %% output for POST method - continue with children
    Ps = [C || C <- [body(json, IsTop, Context, HttpMethod, PathType,
                          Mode, P, Mod, Lvl, Opts) || P <- Chs], C /= []],
    lists:join($, , Ps);
body(json, IsTop, param = Context, post = HttpMethod, PathType, Mode,
     #sn{kind = input, children = Chs}, Mod, Lvl, Opts) ->
    %% input for POST method - continue with children
    Ps = [C || C <- [body(json, IsTop, Context, HttpMethod, PathType,
                          Mode, P, Mod, Lvl, Opts) || P <- Chs], C /= []],
    lists:join($, , Ps);
body(json, _IsTop, _Context, post, _PathType, _Mode, #sn{kind = Kind},
     _Mod, _Lvl, _Opts)
  when Kind == input; Kind == output ->
    %% remove all other combination with operation for POST method
    [];
body(json, IsTop, _Context, _HttpMethod, _PathType, _Mode,
     #sn{kind = AnyKind} = Sn, Mod, Lvl, _Opts) when AnyKind == anydata;
                                                     AnyKind == anyxml ->
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),
    Indent6 = indent(Lvl + 2),
    [
     "\n",
     Indent2, "\"", name(IsTop, Sn, Mod),"\": {\n",
     fmt_type(Lvl + 1, {object, undefined}), ",\n",
     Indent4, "\"x-yang\": {\n",
     Indent6, "\"type\": \"", ?a2l(AnyKind), "\"\n",
     Indent4, "},\n",
     Indent4, "\"description\": \"", ?a2l(AnyKind), " data object\",\n",
     Indent4, "\"properties\": {\n",
     Indent4, "}\n",
     Indent2, "}"
    ];
body(json, _IsTop, _Context, _HttpMethod, _PathType, _Mode,
     #sn{kind = notification}, _Mod, _Lvl, _Opts) ->
    %% FIXME: add event notification support
    [];
body(json, _IsTop, _Context, _HttpMethod, _PathType, _Mode,
     #sn{kind = _Kind} = _Sn, _Mod, _Lvl, _Opts) ->
    %% NOTE: Unknown and unhandled schema node(s), ignore for now.
    [];
body(json, _IsTop, _Context, _HttpMethod, _PathType, root = _Mode,
     #module{} = Mod, Mod, Lvl, _Opts) ->
    %% root resource module - /restconf
    %% NOTE: not building complete ietf-restconf:data, referring to RFC
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),
    Indent6 = indent(Lvl + 2),
    Indent8 = indent(Lvl + 3),

    Description =
        "This is the RESTCONF root resource for the RESTCONF "
        "datastore and operation resources. See RESTCONF RFC 8040 for further "
        "information.",

    %% FIXME: add path to body() everywhere, arghhhh fix name here!!!
    Name = "ietf-restconf:restconf",
    [
     "\n",
     Indent2, "\"", Name, "\": {\n",
     fmt_type(Lvl + 1, {object, undefined}), ",\n",
     Indent4, "\"x-yang\": {\n",
     Indent6, "\"type\": \"root\"\n",
     Indent4, "},\n",
     Indent4, "\"description\": \"", Description, "\",\n",
     Indent4, "\"properties\": {\n",
     Indent6, "\"data\": {\n",
     fmt_type(Lvl + 3, {object, undefined}), ",\n",
     Indent8, "\"properties\": {\n",
     Indent8, "}\n",
     Indent6, "},\n",
     Indent6, "\"operations\": {\n",
     fmt_type(Lvl + 3, {object, undefined}), ",\n",
     Indent8, "\"properties\": {\n",
     Indent8, "}\n",
     Indent6, "},\n",
     Indent6, "\"yang-library-version\": {\n",
     fmt_type(Lvl + 3, {string, undefined}), "\n",
     Indent6, "}\n",
     Indent4, "}\n",
     Indent2, "}"
    ];
body(json, IsTop, response = Context, get = HttpMethod, PathType,
     operations = Mode, #module{} = Mod, Mod, Lvl, Opts) ->
    %% top operations resource module - /restconf/operations
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),
    Indent6 = indent(Lvl + 2),
    Name = "ietf-restconf:operations",
    Rpcs = [C || C <- Mod#module.children, C#sn.kind == 'operation',
                 element(1, C#sn.stmt) == 'rpc'],
    ChildProperties =
        [C || C <- [body(json, IsTop, Context, HttpMethod, PathType,
                         Mode, Child, Mod, Lvl + 2, Opts) ||
                       Child <- Rpcs], C /= []],
    Description =
        "This resource is a container that provides access to the "
        "data-model-specific RPC operations supported by the server. See "
        "RESTCONF RFC 8040 for further information.",
    [
     "\n",
     Indent2, "\"", Name, "\": {\n",
     fmt_type(Lvl + 1, {object, undefined}), ",\n",
     Indent4, "\"x-yang\": {\n",
     Indent6, "\"type\": \"operations\"\n",
     Indent4, "},\n",
     Indent4, "\"description\": \"", Description, "\",\n",
     Indent4, "\"properties\": {\n",
     %% child properties
     lists:join($, , ChildProperties), "\n",
     Indent4, "}\n",
     Indent2, "}"
    ];
body(json, _IsTop, response = _Context, get = _HttpMethod, _PathType,
     'yang-library-version' = _Mode, #module{} = Mod, Mod, Lvl, _Opts) ->
    %% GET on top data resource module - /restconf/yang-library-version
    %% NOTE: not building complete ietf-restconf:yang-library-version,
    %%       referring to RFC
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),
    Indent6 = indent(Lvl + 2),
    Name = "ietf-restconf:yang-library-version",
    Description =
        "This leaf identifies the revision date of the 'ietf-yang-library' "
        "YANG module that is implemented by this server. See RESTCONF RFC 8040 "
        "for further information.",
    [
     "\n",
     Indent2, "\"", Name, "\": {\n",
     fmt_type(Lvl + 1, {object, undefined}), ",\n",
     Indent4, "\"description\": \"", Description, "\",\n",
     Indent4, "\"x-yang\": {\n",
     Indent6, "\"type\": \"leaf\"\n",
     Indent4, "},\n",
     Indent4, "\"properties\": {\n",
     Indent4, "}\n",
     Indent2, "}"
    ];
body(json, _IsTop, response = _Context, get = _HttpMethod, _PathType,
     data = _Mode, #module{} = Mod, Mod, Lvl, _Opts) ->
    %% GET on top data resource module - /restconf/data
    %% NOTE: not building complete ietf-restconf:data, referring to RFC
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),
    Indent6 = indent(Lvl + 2),
    Name = "ietf-restconf:data",
    Description =
        "This resource represents the combined configuration and state data "
        "resources that can be accessed by a client and cannot be created or "
        "deleted by the client. See RESTCONF RFC 8040 for further information.",
    [
     "\n",
     Indent2, "\"", Name, "\": {\n",
     fmt_type(Lvl + 1, {object, undefined}), ",\n",
     Indent4, "\"description\": \"", Description, "\",\n",
     Indent4, "\"x-yang\": {\n",
     Indent6, "\"type\": \"datastore\"\n",
     Indent4, "},\n",
     Indent4, "\"properties\": {\n",
     Indent4, "}\n",
     Indent2, "}"
    ];
body(json, IsTop, param = Context, HttpMethod, PathType, data = Mode,
     #module{stmt = {_, _, _, StmtL}, children  = Chs} = Mod, Mod, Lvl, Opts)
  when HttpMethod == post; HttpMethod == put; HttpMethod == patch ->
    %% PATCH, PUT, (POST) on top data resource module - /restconf/data
    %% NOTE: POST will not go here since the body will be on a child resource

    Name = "ietf-restconf:data",
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),
    Indent6 = indent(Lvl + 2),
    Desc = description(StmtL),
    ChildProperties =
        [C || C <- [body(json, IsTop, Context, HttpMethod, PathType,
                         Mode, Child, Mod, Lvl + 2, Opts) ||
                       Child <- Chs], C /= []],
    [
     "\n",
     Indent2, "\"", Name, "\": {\n",
     Indent4, "\"description\": \"", Desc, "\",\n",
     fmt_type(Lvl + 1, {object, undefined}), ",\n",
     Indent4, "\"x-yang\": {\n",
     Indent6, "\"type\": \"datastore\"\n",
     Indent4, "},\n",
     %% children
     Indent4, "\"properties\": {",
     %% child properties
     lists:join($, , ChildProperties), "\n",
     Indent4, "}\n",
     Indent2, "}"
    ].


%%
%% abstractation for underlying storage
%%
store_defs(Tab, Defs) when is_list(Defs) ->
    %% NOTE: store new, but do not replace existing
    lists:foreach(fun(D) -> ets:insert_new(Tab, D) end, Defs).


%%
%% global static query params. use $ref to reference them in paths
%%
query_param_defs(Lvl) ->
    %% content - GET & HEAD
    ContentDesc = "controlling descendant nodes in response",
    ContentEnums = [{config, 1}, {nonconfig, 2}, {all, 3}],
    ContentTypeAndFormat = type_and_format_enum(ContentEnums),

    %% depth - GET & HEAD
    DepthDesc = "limit the depth of nodes in response",
    DepthTypeAndFormat = type_and_format(uint16),

    %% fields - GET & HEAD
    FieldsDesc = "optionally identify specific data nodes in response",
    FieldsTypeAndFormat = type_and_format(string),

    %% filter - GET & HEAD
    FilterDesc = "xpath expression to filter data nodes in response",
    FilterTypeAndFormat = type_and_format(string),

    %% with-defaults - GET & HEAD
    WDefaultsDesc = "controlling default values in response",
    WDefaultsEnums = [{'report-all', 1}, {trim, 2}, {explicit, 3},
                      {'report-all-tagged', 4}],
    WDefaultsTypeAndFormat = type_and_format_enum(WDefaultsEnums),

    %% insert - POST and PUT
    InsertDesc = "controlling the order when adding new list elements",
    InsertEnums = [{first, 1}, {last, 2}, {before, 3}, {'after', 4}],
    InsertTypeAndFormat = type_and_format_enum(InsertEnums),

    %% point - POST and PUT
    PointDesc = "used to specify the insertion point",
    PointTypeAndFormat = type_and_format(string),

    %% FIXME: event notification resource: start-time, stop-time

    [
     query_param_def(Lvl, content, ContentDesc, ContentTypeAndFormat),
     query_param_def(Lvl, depth, DepthDesc, DepthTypeAndFormat),
     query_param_def(Lvl, fields, FieldsDesc, FieldsTypeAndFormat),
     query_param_def(Lvl, filter, FilterDesc, FilterTypeAndFormat),
     query_param_def(Lvl, 'with-defaults', WDefaultsDesc,
                     WDefaultsTypeAndFormat),
     query_param_def(Lvl, insert, InsertDesc, InsertTypeAndFormat),
     query_param_def(Lvl, point, PointDesc, PointTypeAndFormat)
    ].

query_param_def(Lvl, Name0, Description, TypeAndFormat) ->
    %% NOTE: no query param name collision in RESTCONF RFC, yet ...
    %% Format: <query-definition-name>: {<query-definition>}
    Indent2 = indent(Lvl),
    Indent4 = indent(Lvl + 1),
    Name = ?a2l(Name0),
    [
     "\n",
     Indent2, "\"", Name, "\": {\n",
     Indent4, "\"name\": \"", Name, "\",\n",
     Indent4, "\"in\": \"query\",\n",
     Indent4, "\"description\": \"", Description, "\",\n",
     Indent4, "\"required\": false,\n",
     %% type & format
     fmt_type(Lvl + 1, TypeAndFormat), "\n",
     Indent2, "}"
    ].


%% NOTE: Schema Node (Sn) may also be a module
query_params(HttpMethod, _Path, _PathType, _Mode, #sn{}, _Mod, Lvl)
  when HttpMethod == get; HttpMethod == head ->
    [
     query_param(Lvl, content),
     query_param(Lvl, depth),
     query_param(Lvl, fields),
     query_param(Lvl, filter),
     query_param(Lvl, 'with-defaults')
    ];
query_params(HttpMethod, _Path, _PathType, _Mode, #sn{kind = Kind}, _Mod, Lvl)
  when Kind == list, HttpMethod == post;
       Kind == list, HttpMethod == put;
       Kind == 'leaf-list', HttpMethod == post;
       Kind == 'leaf-list', HttpMethod == put ->
    [
     query_param(Lvl, insert),
     query_param(Lvl, point)
    ];
query_params(_HttpMethod, _Path, _PathType, _Mode, _Sn, _Mod, _Lvl)  ->
    %% FIXME: event notification resource: start-time, stop-time
    [].


query_param(Lvl, Name) ->
    %% NOTE: no query param name collision in RESTCONF RFC, yet ...
    fmt_ref(Lvl, parameters, ?a2l(Name)).



%% NOTE: Schema Node (Sn) may also be a module
form_params(_HttpMethod, _Path, _PathType, _Mode, _Sn, _Mod, _Lev)   -> [].
header_params(_HttpMethod, _Path, _PathType, _Mode, _Sn, _Mod, _Lev) -> [].


%% @doc convert tokenized path to URL.
-spec tokpath2origpath([atom()]) -> string().
tokpath2origpath(TokPath) ->
    "/" ++ lists:join($/, [?a2l(T) || T <- TokPath]).


escape(AStr) when is_atom(AStr) ->
    escape(?l2b(?a2l(AStr)));
escape(LStr) when is_list(LStr) ->
    escape(?l2b(LStr));
escape(BStr) when is_binary(BStr) ->
    lists:foldl(
      fun({RPq, RWith}, BStr1) ->
              binary:replace(BStr1, RPq, RWith, [global])
      end, BStr, replace_patterns()).

replace_patterns() ->
    [{<<"\\">>, <<"\\\\">>},
     {<<"\"">>, <<"\\\"">>},
     {<<"\t">>, <<"\\t">>},
     {<<"\r\n">>, <<"\\n">>},
     {<<"\r">>, <<"\\n">>},
     {<<"\n">>, <<"\\n">>}].


indent(Lvl) when is_integer(Lvl), Lvl > 0  -> lists:duplicate(Lvl * 2, $\s).
