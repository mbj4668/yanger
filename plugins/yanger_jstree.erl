%%%----------------------------------------------------------------%%%
%%% @doc Yanger JS-Tree Output Plugin                              %%%
%%% @author tfroberg@cisco.com (summer intern 2021)                %%%
%%% @author jorendel@cisco.com (summer intern 2021)                %%%
%%% Prints schema nodes (children) of the YANG data tree.          %%%
%%%----------------------------------------------------------------%%%

-module(yanger_jstree).
-behaviour(yanger_plugin).

-export([init/1, help/0, do/1]).

-include_lib("yanger/include/yang.hrl").
-include_lib("inets/include/httpd.hrl").

-define(stmt_arg,      yang:stmt_arg).
-define(search_one,    yang:search_one_stmt).
-define(stmt_substmts, yang:stmt_substmts).
-define(standard_io,   standard_io).
-define(index,         index).
-define(no_standard,   no_standard).
-define(module,        module).
-define(rpcs,          rpcs).
-define(notifs,        notifs).

-spec help() -> binary().
%% @doc Standard help function.
help() ->
    <<"Yanger plugin that generates HTML/JS for a YANG data tree. Do this
    by typing yanger -f jstree YANG_MODEL.yang in the terminal.

        Available options:
            --jstree-help:
                Prints this text in the terminal.

            --jstree-server <host>:<portNr>:
                Hosts the generated html on the specified host + portNr.

            --jstree-server-imp-mod <host>:<portNr>:
                Host the generated html on the specified host + portNr.
                Also generates html for all of the imported modules.

        Table header name info:
            schema contains the kind of the node

            type is the name of the type for leafs and leaf-lists.
                Hover over a type to see if the type is limited by
                pattern, range or length

            flags is one of:
                Config  for configuration data
                Non config  for non-configuration data

            opts is one of:
                ?  for an optional leaf or choice
                !  for a presence container
                *  for a leaf-list or list
                [<keys>] for a list's keys

            status is one of:
                current  for current
                deprecated  for deprecated
                obsolete  for obsolete
    ">>.

%% @doc Initialize the plugin.
init(Ctx0) ->
    Ctx1 = yanger_plugin:register_output_format(Ctx0,
                                                jstree,
                                                true,
                                                fun emit/3),
    Ctx2 = yanger_plugin:register_hook(Ctx1,
                                       #hooks.post_init_ctx,
                                       fun post_init_ctx/1),
    yanger_plugin:register_option_specs(Ctx2, option_specs()).

%% @doc Function that returns the option specs of JStree.
option_specs() ->
    [{"JStree output specific options:",
        [
         %% --jstree-help
         {jstree_help, undefined, "jstree-help", boolean,
          "Print help function and exit"},

         %% --jstree-server <host>:<portNr>
         {jstree_server, undefined, "jstree-server", string,
          "Host the generated HTML using the given host:portNr"},

         %% --jstree-server-imp-mod <host>:<portNr>
         {jstree_server_imp_mod, undefined, "jstree-server-imp-mod", string,
          "Host the generated HTML using the given host:portnr.
          Also generate HTML for all imported modules"}]
    }].

%% @doc Function that handles the jstree_help option.
post_init_ctx(Ctx) ->
    case proplists:get_value(jstree_help, Ctx#yctx.options, false) of
        true ->
            io:put_chars(help()),
            halt();
        false ->
            ok
    end,
    Ctx.

-spec emit(#yctx{}, [#module{}], io:device()) -> [#yerror{}].
%% @doc Main function that handles option flags
emit(Ctx, [Mod], _Fd) ->

    ServerOpt = handle_options(Ctx, Mod),
    case ServerOpt of
        undefined -> ok;
        _         -> start_server(ServerOpt)
    end,

    %% If this format plugin will need to produce warnings or errors
    %% in the future, these warnings and errors need to be returned here.
    _Errors = [].

%% @doc Calls emit_html/5 correctly based on the option flags.
handle_options(Ctx, Mod) ->
    Imp_mod = proplists:get_value(jstree_server_imp_mod, Ctx#yctx.options),
    case proplists:get_value(jstree_server, Ctx#yctx.options) of
        undefined -> case Imp_mod of
                      undefined -> emit_html(Ctx, [Mod], ?standard_io,
                                      undefined, []),
                                   undefined;
                      Server1   -> ModNames = [M || {M, _, _, _}
                                      <- Mod#module.imports],
                                   Mods = get_imports(ModNames, [], Mod, Ctx),
                                   io:format("Wait for HTML to be generated\n"),
                                   emit_html(Ctx, [Mod], ?index,
                                      Server1, ModNames),
                                   emit_html(Ctx, Mods, ?no_standard,
                                      Server1, ModNames),
                                   Server1
                     end;
        Server    -> io:format("Wait for HTML to be generated\n"),
                     emit_html(Ctx, [Mod], ?index, Server, []),
                     Server
    end.

%% @doc Creates a server using the given host and port.
start_server(ServerOpt) ->
    try
        [Host, Port] = string:split(ServerOpt, ":"),
        inets:start(),
        {ok, _Pid} = inets:start(httpd, [{port, ?l2i(Port)},
        {server_name,"httpd_test"}, {server_root,"."}, {document_root,"."},
        {bind_address, Host}, {directory_index, ["index.html"]},
       {modules,[mod_alias,mod_auth,mod_esi,mod_actions,mod_cgi,mod_dir,mod_get,
        mod_head, mod_log, mod_disk_log, yanger_jstree]}]),
        io:format("HTML generated, open ~s:~p in your browser~n",
            [Host, ?l2i(Port)]),
        loop()
    catch error:_ ->
        io:format("Invalid host and/or port\n"),
        halt()
    end.

%% @doc Function where the functions that generates the HTML are called from.
emit_html(_, [], _, _, _) ->
    ok;
emit_html(Ctx, [Mod|Mods], Fd, ServerOpt, Imp_mod_names) ->

    IO = determine_io_device(Fd, Mod),

    ExistingChildren = existing_children(Mod#module.children),
    Chs    = [C || C <- ExistingChildren, is_data_def(C#sn.kind, Ctx)],
    Rpcs   = [C || C <- ExistingChildren, C#sn.kind == 'operation'],
    Notifs = [C || C <- ExistingChildren, C#sn.kind == 'notification'],

    print_start_html(IO, Imp_mod_names, ServerOpt),

    print_children({Chs, ?module}, IO, Mod),
    print_children({Rpcs, ?rpcs}, IO, Mod),
    print_children({Notifs, ?notifs}, IO, Mod),

    print_end_html(IO, ServerOpt),

    emit_html(Ctx, Mods, IO, ServerOpt, Imp_mod_names).

%% @doc Determines if the children should generate HTML or not.
print_children({Children, Type}, IO, Mod) ->
    if
        length(Children) == 0 ->
            skip_chs;
        true ->
            children_start_html(Type, IO, Mod),
            print_list(Children, Mod, undefined, IO),
            io:format(IO,"</ul></li>",[])
    end.

%% @doc Print the needed start HTML for each children type (Chs, Rpcs, Notifs).
children_start_html(?module, IO, Mod) ->
    io:format(IO, "<li>\n"
                  " <span class=\"caret\">\n"
                  "     <div class=\"container\">\n"
                  "         <div class=\"name\">module: ~s</div>\n"
                  "         <table><tr><td>Module</td></tr></table>\n"
                  "     </div>\n"
                  " </span>\n"
                  " <ul class=\"nested\">\n", [Mod#module.name]);
children_start_html(Type, IO, _) ->
    io:format(IO, "<li>\n"
                  " <span class=\"caret\">~s:</span>\n"
                  " <ul class=\"nested\">\n", [Type]).

%% @doc Determines where the html should be written.
determine_io_device(Fd, Mod) ->
    case Fd of
        ?standard_io -> ?standard_io;
        ?index       -> {ok, IoDevice} = file:open("index.html", [write]),
                        IoDevice;
        _            -> {ok, IoDevice} = file:open(?a2l(Mod#module.name)
                                                   ++".html", [write]),
                        IoDevice
    end.

%% @doc Function that outputs the entire tree structure in HTML.
print_list([], _Mod, _Pkey, _IO) ->
    skip;
print_list([Ch|_], _, _, _) when is_tuple(Ch#sn.name) ->
    skip;
print_list([Ch|Chs], Mod, PKey, IO) ->
    Children = Ch#sn.children,
    Key = Ch#sn.keys,
    print_open_tag(Ch#sn.name,
                   get_config(Ch#sn.config),
                   Ch#sn.kind,
                   get_type_name(Ch#sn.type),
                   Children,
                   get_opts(Ch#sn.stmt, Mod, Ch, PKey),
                   get_desc(Ch),
                   get_status(Ch#sn.stmt),
                   get_type_string(Ch#sn.type),
                   IO),
    print_list(Children, Mod, Key, IO),
    print_close_tag(Children, IO),
    print_list(Chs, Mod, Key, IO).


%% @doc Function that prints the opening tags for a node in the tree structure.
print_open_tag(Name,Config,Kind,Type,Chs,Opts,Desc,Status,Typedesc,IO) ->
    io:format(IO,
            get_curnode_start_tags(Chs)++
            "<div class=\"container\">\n"
                ++get_img(Kind)++
                Desc++
            "   <table>\n"
            "       <tr>\n"
                    ++table_cell()++
            "       <td>\n"
                    ++get_type_html(Typedesc)++
            "       </td>\n"
                    ++table_cell()
                    ++table_cell()
                    ++table_cell()++
            "       </tr>\n"
            "   </table>\n"
            "</div>"++get_curnode_end_tags(Chs)++"\n",
            [Name, Kind, Type, Config, Opts, Status]).

table_cell() ->
    "<td class=\"of\">\n~s</td>".

%% @doc Function that returns the first tag based on whether the node
%%      has children or not.
get_curnode_start_tags([]) ->
    "<li>";
get_curnode_start_tags(_) ->
    "<li><span class=\"caret\">".

%% @doc Function that returns the last tag based on whether the
%%      node has children or not.
get_curnode_end_tags([]) ->
    "";
get_curnode_end_tags(_) ->
    "</span><ul class=\"nested\">".

%% @doc Function that returns HTML based on whether the node has a
%%      type description or not.
get_type_html([]) ->
    "<div class=\"of\">~s</div>";
get_type_html(Typedesc) ->
    "<div class=\"description\">\n"
    "   <div class=\"of dotted_line\">~s</div>\n"
    "       <span class=\"descriptiontext\">\n"
    "       <pre>"++Typedesc++"</pre>\n"
    "    </span>\n"
    "</div>\n".

%% @doc Function that returns a list containing a module's imported modules
get_imports([], _, _, _) ->
    [];
get_imports([Name], List, Mod, Ctx) ->
    {_, MoreMods} = yang:get_imported_module(Name, Mod, Ctx),
    lists:reverse([MoreMods|List]);
get_imports([Name|Tail], List, Mod, Ctx) ->
    {_, MoreMods} = yang:get_imported_module(Name, Mod, Ctx),
    get_imports(Tail, [MoreMods|List], Mod, Ctx).

%% @doc Function that outputs the close tag based on whether the
%%      node has children or not.
print_close_tag([], IO) ->
    io:format(IO, "</li>", []);
print_close_tag(_, IO) ->
    io:format(IO, "</ul></li>", []).

%% @doc Function that returns a string based on whether the
%%      node has config or not.
get_config(true) ->
    "Config";
get_config(Config) when Config =:= false orelse Config =:= ignore ->
    "Non config";
get_config(undefined) ->
    %% Used if config is undefined.
    "".

-spec is_data_def(atom(), #yctx{}) -> boolean().
is_data_def(Keyword, #yctx{env = #env{data_definition_stmts = D}}) ->
    Keyword /= 'rpc'
        andalso Keyword /= 'action'
        andalso Keyword /= 'notification'
        andalso yang:map_is_key(Keyword, D).

existing_children(Chs) ->
    [C || #sn{if_feature_result = true} = C <- Chs].

%% @doc Function that returns a string represantation of the type.
get_type_name(undefined) ->
    "";
get_type_name(Type) ->
    case ?stmt_arg(Type#type.stmt) of
        {X,Y} -> ?a2l(X) ++ ":" ++ ?a2l(Y);
        X     -> ?a2l(X)
    end.

%% @doc Function that returns a string representation of the options.
get_opts({list,_,_,_}, _, Ch, _) ->
    String = "*",
    {_, _, _, Subs}= Ch#sn.stmt,
    case ?search_one(key, Subs) of
        false ->
            String;
        Key ->
            String ++ " [" ++ ?b2l(?stmt_arg(Key)) ++ "]"
    end;
get_opts({'leaf-list',_,_,_}, _, _, _) ->
    "*";
get_opts({leaf,_,_,Subs}, _, Ch, PKey) ->
    LocalName =
                case Ch#sn.name of
                    {_, B} -> B;
                    A -> A
                end,
    case PKey /= undefined andalso lists:member(LocalName, PKey) of
        false ->
            Mand = ?search_one(mandatory, Subs),
            case (Mand == false) orelse (?stmt_arg(Mand) == false) of
                true  -> "?";
                _     -> ""
            end;
        true  -> ""
    end;
get_opts({choice, _, _, Subs}, Mod, _, _) ->
    MArg = ?stmt_arg(Mod#module.stmt),
    case (?search_one(mandatory, Subs) == false) orelse (MArg == false) of
        true -> "?";
        _    -> ""
    end;
get_opts({container, _, _, Subs}, _, _, _) ->
    case ?search_one(presence,Subs) of
        false -> "";
        _     -> "!"
    end;
get_opts(_, _, _, _) ->
    "".

%% @doc function that returns the description.
get_desc(Ch) ->
    {_, _, _, Subs}= Ch#sn.stmt,
    Desc = ?search_one(description, Subs),
    case Desc of
        false -> "<div class=\"name\">~s</div>";
        _     -> "<div class=\"description\">\n"
                 "  <div class=\"name\">~s</div>\n"
                 "      <span class=\"descriptiontext\">\n"
                 "      <pre>\n"
                        ++?b2l(?stmt_arg(Desc))++
                 "      </pre>\n"
                 " </span>\n"
                 "</div>\n"
    end.

%% @doc Function that returns the status of a node.
get_status(Stmt) ->
    case ?search_one(status, ?stmt_substmts(Stmt)) of
        {_, deprecated, _, _} -> "Deprecated";
        {_, obsolete, _, _}   -> "Obsolete";
        _                     -> "Current"
    end.

%% @doc Function that check if sn.type isn't undefined.
%%      If not, it calls type_string.
get_type_string(Type) ->
    case Type of
        undefined -> "";
        _         -> Type_spec = Type#type.type_spec,
                     type_string(Type_spec)
    end.

%% @doc Function that returns info about if the data type has a pattern,
%%      length or range constraint. Otherwise it returns an empty string
type_string(#integer_type_spec{range_stmt=Stmt}) ->
    range(Stmt);
type_string(#decimal64_type_spec{range_stmt=Stmt}) ->
    range(Stmt);
type_string(#string_type_spec{length_stmt = L_Stmt, pattern_stmts = P_Stmt}) ->
    S1 = case L_Stmt of
        undefined -> "";
        _         -> "{length = " ++ ?b2l(?stmt_arg(L_Stmt)) ++ "}"
    end,
    S2 = case P_Stmt of
        [] -> "";
        _  -> pattern(P_Stmt, "")
    end,
    S1 ++ S2;
type_string(#binary_type_spec{length_stmt = Stmt}) ->
    case Stmt of
        undefined -> "";
        _         -> "{length = " ++ ?b2l(?stmt_arg(Stmt)) ++ "}"
    end;
type_string(_) ->
    "".

%% @doc Function that returns the pattern constraint of the node.
pattern([], String) ->
    String;
pattern([H|T], String) ->
    NewString = "{pattern = " ++ ?b2l(?stmt_arg(H)) ++ "}" ++ String,
    pattern(T, NewString).

%% @doc Function that returns a string with information about if the
%%      node has a range constraint.
range(Stmt) ->
    case Stmt of
        undefined -> "";
        _         -> "[" ++ ?b2l(?stmt_arg(Stmt)) ++ "]"
    end.

%% @doc Function that returns a leaf icon if the node is leaf.
get_img(Kind) when Kind =:= leaf orelse Kind =:= 'leaf-list' ->
    "&#127808;";
get_img(_) ->
    "".

%% @doc Function that returns HTML code to create a button if the
%%      web server should be enabled.
get_stop_button_html(?standard_io) ->
    "";
get_stop_button_html(_) ->
    "<button onclick=\"stopFunction()\">Close server</button>".

%% @doc Function that returns HTML code for a drop down menu with all
%%      imported modules if they are present.
get_import_select_html([], _) ->
    "";
get_import_select_html(Imp_mod_names, ServerOpt) ->
    "<span class=\"padding\">\n"
    "   <select onchange=\"window.location.href=value\">\n"
    "     <option selected=\"true\" disabled=\"true\">Select Import</option>\n"
    "     <option value=\"http://"++ServerOpt++"\">Main Module</option>\n"
          ++get_option_html(Imp_mod_names, ServerOpt, "")++
    "   </select>\n"
    "</span>\n".

%% @doc Function that returns imported modules as options to the select tag.
get_option_html([], _, String) ->
    String;
get_option_html([Name|Names], ServerOpt, String) ->
    NewString = String++"<option value=\"http://"++ServerOpt
                ++"/"++?a2l(Name)++".html\">"++?a2l(Name)++"</option>\n",
    get_option_html(Names, ServerOpt, NewString).

%% @doc Function that keeps the server up and running.
loop() ->
    timer:sleep(1000),
    loop().

%% @doc Function that is called everytime a http request is made.
%% If the data sent is stop, close the server/this program.
do(ModData) ->
    [{_,{_,Data,_}}|_] = ModData#mod.data,
    case Data of
        "/stop" -> halt();
        _       -> done
    end.

%% @doc Function that returns the CSS and start HTML code.
print_start_html(IO, Imp_mod_names, ServerOpt) ->
    io:format(IO, "<!DOCTYPE html>
            <html>
                <head><meta name=\"viewport\" content=\"width=device-width,
                initial-scale=1\">
                    <style>
                        ul, #myUL {
                            list-style-type: none;
                        }
                        #myUL {
                            margin: 0;padding: 0;
                        }
                        .caret {
                            cursor: pointer;
                            -webkit-user-select: none;
                            -moz-user-select: none;
                            -ms-user-select: none;user-select: none;
                        }
                        .caret::before {
                            content: \"\\25B6\";
                            color: black;
                            display: inline-block;
                            margin-right: 6px;
                        }
                        .caret-down::before {
                            -ms-transform: rotate(90deg);
                            -webkit-transform: rotate(90deg);
                            transform: rotate(90deg);
                            }
                        .nested {
                            display: none;
                        }
                        .active {
                            display: block;
                        }
                        table {
                            table-layout:fixed;
                            font-size: 100%;
                            position: absolute;
                            left: 42vw;
                            width: 58vw;
                            }
                        .container {
                            display: inline-flex;
                        }
                        .description {
                            width: 100%;
                            position: relative;
                            display: inline-block;
                        }
                        .name {
                            padding-top: 0.4vh;
                        }
                        .description .descriptiontext {
                            visibility: hidden;
                            background-color: #eee;
                            color: black;
                            text-align: left;
                            font-size: 80%;
                            padding: 15px;
                            border-radius: 6px;
                            position: absolute;
                            z-index: 1;
                            top: -5px;
                            left: 105%;
                            border: 1px solid black;
                        }
                        .description:hover .descriptiontext {
                            visibility: visible;
                        }
                        .headers {
                            background-color: #fff;
                            border: 1px solid black;
                            z-index: 1;
                        }
                        .of {
                            text-overflow: ellipsis;
                            overflow: hidden;
                            white-space: nowrap;
                        }
                        .dotted_line {
                            text-decoration:underline;
                            text-decoration-style: dotted;
                        }
                        .padding {
                            padding-left: 3px;
                        }
                    </style>
                </head>
            <body>
                <h2>Yang tree view</h2>
                <div class=\"container\">
                    <table class=\"headers\" style=\"text-align: left;
                                                          top: 30px;\">
                        <tr class=\"headers\" style=\"border: 1px
                                                        solid black;\">
                            <th class=\"headers\">Schema</th>
                            <th class=\"headers\">Type</th>
                            <th class=\"headers\">Flags</th>
                            <th class=\"headers\">Opts</th>
                            <th class=\"headers\">Status</th>
                        </rt>
                    </table>
                </div>
                <button onclick=\"expandFunction()\">Expand all</button>
                <button onclick=\"closeFunction()\">Collapse all</button>
                "++get_stop_button_html(IO)++
                get_import_select_html(Imp_mod_names, ServerOpt)++"
                <ul id=\"myUL\">",[]).

%% @doc Function that returns the last piece of html needed.
print_end_html(IO, ServerOpt) ->
    io:format(IO, "</ul>
            <script>
                var toggler = document.getElementsByClassName(\"caret\");
                var i;  for (i = 0; i < toggler.length; i++) {
                    toggler[i].addEventListener(\"click\", function() {
                        this.parentElement.querySelector(\".nested\")
                                        .classList.toggle(\"active\");
                        this.classList.toggle(\"caret-down\");
                    });
                }
                function expandFunction() {
                    for (i = 0; i < toggler.length; i++) {
                        toggler[i].parentElement.querySelector(\".nested\")
                                                .classList.add(\"active\");
                        toggler[i].classList.add(\"caret-down\");
                    }
                }
                function closeFunction() {
                    for (i = 0; i < toggler.length; i++) {
                            toggler[i].parentElement.querySelector(\".nested\")
                                                  .classList.remove(\"active\");
                            toggler[i].classList.remove(\"caret-down\");
                        }
                    }
                function stopFunction() {
                    var http = new XMLHttpRequest();
                    http.open(\"GET\", 'http://~s/stop');
                    http.send();
                    }
            </script>
        </body>
    </html>", [ServerOpt]).