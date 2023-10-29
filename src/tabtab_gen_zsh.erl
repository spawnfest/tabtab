%% @doc Autocompletion file generator functions
%% @end

-module(tabtab_gen_zsh).

-behaviour(tabtab_gen).

-export([generate/2]).

-spec generate([tabtab_core:tt_command()], tabtab_core:tt_opts()) -> string().
generate(Commands, #{shell:=zsh}=TTOpts) ->
    lists:flatten([%%tabtab_gen_utils:header(TTOpts),
        main(Commands, TTOpts),
        io_lib:nl()]).

main(Commands, TTOpts) ->
    "#compdef _rebar3 rebar3\n" ++
    "\n" ++
    cmd_to_fun(#{name=>"rebar3", help=>"Erlang build tool", commands=>Commands},
                [], TTOpts).

%% Hack to support key=value arguments in rebar3 new TEMPLATE
cmd_to_fun(#{name:=Name}=Cmd, ["new","rebar3"]=Prev, TTOpts) ->
    %% treat template variables as commands
    Args = maps:get(arguments, Cmd, []),
    Nested = [#{name=>L,
                help=>H,
                commands=>[],
                arguments=>[]} || #{long:=L, help:=H} <- Args],
    CmdFun = lists:concat([
        "function "++function_name(Prev, Name)++" {\n",
        "  local -a commands\n",
        "\n",
        "   _arguments \\\n",
        "   \"*: :->cmnds\"\n",
        "\n",
        nested_cmds(Nested,[Name|Prev],TTOpts),
        "}\n",
        "\n"]),
    CmdFun;
cmd_to_fun(#{name:=Name}=Cmd, Prev, TTOpts) ->
    Nested = maps:get(commands, Cmd, []),
    CmdFun = lists:concat([
        "function "++function_name(Prev, Name)++" {\n",
        "  local -a commands\n",
        "\n",
        args(Cmd, TTOpts),
        "\n",
        nested_cmds(Nested,[Name|Prev],TTOpts),
        "}\n",
        "\n"]),
    CmdFun ++ lists:concat([cmd_to_fun(C, [Name|Prev], TTOpts) || C <- Nested]).

function_name(Prev,Name) ->
    "_" ++ string:join(
            lists:reverse([Name | Prev]),
            "_").

nested_cmds([],_,_) ->
    "\n";
nested_cmds(Cmds,Prev,TTOpts) ->
    lists:concat([
        "  case $state in\n",
        "  cmnds)\n",
        "    commands=(\n",
        lists:concat(["      "++cmd_str(Cmd, TTOpts)++"\n" || Cmd <- Cmds]),
        "    )\n",
        "    _describe \"command\" commands\n",
        "    ;;\n",
        "  esac\n",
        "\n",
        "  case \"$words[1]\" in\n",
        lists:concat([cmd_call_case(Cmd, Prev, TTOpts) || Cmd <- Cmds]),
        "  esac\n"]).

cmd_str(#{name:=N,help:=H}, _TTOpts) ->
    "\""++N++":"++help(H)++"\"";
cmd_str(#{name:=N}, _TTOpts) ->
    "\""++N++"\"".

cmd_call_case(#{name:=Name}, Prev, _TTOpts) ->
    "  "++Name++")\n"
    "    "++function_name(Prev, Name)++"\n"
    "    ;;\n".    

args(Cmd, TTOpts) ->
    Args = maps:get(arguments, Cmd, []),
    Cmds = maps:get(commands, Cmd, []),
    NoMore = (Args=:=[]) and (Cmds=:=[]),
    case NoMore of
        true ->
            "   _message 'no more arguments'\n";
        false ->
            "  _arguments \\\n"++
            lists:concat([arg_str(Arg) || Arg <- Args]) ++
            case Cmds of
                [] ->
                    "";
                _ ->
                    "   \"1: :->cmnds\" \\\n"
                    "   \"*::arg:->args\"\n"
            end
    end.

arg_str(#{short:=undefined,long:=undefined,help:=H,type:=T}) ->
    "   "++"'1:"++H++":' \\\n"; 
arg_str(#{help:=H,type:=T}=Arg) ->
    "   "++spec(Arg) ++ "[" ++ help(H) ++ "]'" ++ hint(T) ++ " \\\n".

spec(#{short:=undefined,long:=L}) ->
    "'(--"++[L]++")--"++L++"";
spec(#{short:=S,long:=undefined}) ->
    "'(-"++[S]++")-"++S++"";
spec(#{short:=S,long:=L}) ->
    "'(-"++[S]++" --"++[L]++")'{-"++[S]++",--"++L++"}'".

help(undefined) -> "";
help(H) -> help_escape(H).

help_escape([]) ->
    [];
%% "("
help_escape([40 | Rest]) ->
    "\\("++help_escape(Rest);
%% ")"
help_escape([41 | Rest]) ->
    "\\)"++help_escape(Rest);
%% doubling single quotes by doubling them
help_escape([$' | Rest]) ->
    "''"++help_escape(Rest);
help_escape([C | Rest]) ->
    [C | help_escape(Rest)].

%% TODO
hint(_) ->
    "".
