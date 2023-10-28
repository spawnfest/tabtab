%% @doc Autocompletion file generator functions
%% @end

-module(tabtab_gen_zsh).

-behaviour(tabtab_gen).

-export([generate/2]).

-define(str(N), integer_to_list(N)).

-spec generate([tabtab_core:tt_command()], tabtab_core:tt_opts()) -> string().
generate(Commands, #{shell:=zsh}=TTOpts) ->
    lists:flatten([tabtab_gen_utils:header(TTOpts),
        main(Commands, TTOpts),
        io_lib:nl()]).

main(Commands, TTOpts) ->
    lists:concat(["#compdef rebar3\n",
    "_rebar3 () {\n",
    "    typeset -A opt_args\n",
    "    local curcontext=\"$curcontext\" state line\n",
    "\n",
    "    local ret=1\n",
    "\n",
    "    _arguments -C \\\n",
    "        '1: :_rebar3_tasks' \\\n",
    "        '*::arg:->args' \\\n",
    "    && ret=0\n",
    "\n",
    "    case $state in\n",
    "        (args)\n",
    "            curcontext=\"${curcontext%:*:*}:rebar-task-$words[1]:\"\n",
    "            case $line[1] in\n",
    lists:concat([cmd_to_case(Cmd, TTOpts) || Cmd <- Commands]),
    "            esac\n",
    "    esac\n",
    "}\n",
    "\n",
    "(( $+functions[_rebar3_tasks] )) ||\n",
    "_rebar3_tasks() {\n",
    "    local tasks; tasks=(\n",
    "        'as:Higher order provider for running multiple tasks in a sequence as a certain profiles.'\n",
    "        'clean:Remove compiled beam files from apps.'\n",
    "    )\n",
    "    _describe -t tasks 'rebar3 tasks' tasks \"$@\"\n",
    "}\n",
    "\n",
    "(( $+functions[_rebar3_list_deps] )) ||\n",
    "_rebar3_list_deps() {\n",
    "    local -a cmd packages deps_long\n",
    "    deps_long=($PWD/_build/default/lib/*(/))\n",
    "    packages=( ${${deps_long#$PWD/_build/default/lib/}%-*-*} )\n",
    "    compadd \"$@\" -a packages\n",
    "}\n",
    "\n",
    "_rebar3 \"$@\"\n",
    "\n",
    "# Local variables:\n",
    "# mode: shell-script\n",
    "# sh-basic-offset: 2\n",
    "# sh-indent-comment: t\n",
    "# indent-tabs-mode: nil\n",
    "# End:\n",
    "# ex: sw=2 ts=2 et filetype=sh\n"]).

cmd_to_case(#{name:=Name}=Cmd,_TTOpts)->
    Args = maps:get(arguments, Cmd, []),
    Def =#{short=>undefined,long=>undefined,help=>"",type=>string},
    lists:concat(["("++Name++")\n",
                "   _arguments \\\n",
                %% TODO FIX!!
                [arg_string(maps:merge(Def,Arg)) || Arg <- Args],
                "                    && ret=0\n",
                "                ;;\n"]).

arg_string(#{short:=undefined,long:=undefined,help:=H,type:=T}) ->
    "'1:"++H++":' \\\n"; 
arg_string(#{help:=H,type:=T}=Arg) ->
    spec(Arg) ++ "[" ++ H ++ "]" ++ hint(T) ++ " \\\n".

spec(#{short:=undefined,long:=L}) ->
    "'(--"++[L]++")--"++L++"";
spec(#{short:=S,long:=undefined}) ->
    "'(-"++[S]++")-"++S++"";
spec(#{short:=S,long:=L}) ->
    "'(-"++[S]++" --"++[L]++")'{-"++[S]++" --"++L++"}'".

hint(_) ->
    "".
