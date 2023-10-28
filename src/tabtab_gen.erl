%% @doc Autocompletion file generator functions
%% @end

-module(tabtab_gen).

-export([generate/2]).

-spec generate([tabtab_core:tt_command()], tabtab_core:tt_opts()) -> iolist().
generate(Commands, #{shell:=bash}=TTOpts) ->
    lists:flatten([header(TTOpts),
        main(Commands, TTOpts),
        complete(TTOpts),
        io_lib:nl()]).

header(#{shell:=Shell}) ->
    ["# ",atom_to_list(Shell)," completion file for rebar3 (autogenerated by tabtab plugin).\n"].

cmd_clause(#{name:=Name}=Cmd) ->
    Args = maps:get(arguments, Cmd, []),
    Cmds = maps:get(commands, Cmd, []),
    Opts = lists:map(fun(#{short:=S, long:=L}) -> {S,L} end,Args),
    {Shorts0,Longs0} = lists:unzip(Opts),
    Defined = fun(Opt) -> Opt =/= undefined end,
    Shorts = lists:filter(Defined, Shorts0),
    Longs = lists:filter(Defined, Longs0),
    SOpts = string:join(
                lists:map(fun(S) -> [$-,S] end, Shorts),
                " "),
    LOpts = string:join(
                lists:map(fun(L) -> "--"++L end, Longs),
                " "),
    Cmdsnvars = string:join(
                lists:map(fun(#{name:=N})->N end, Cmds),
                " "),
    ClauseBody = "sopts=\""++SOpts++"\"\n"++
                "lopts=\""++LOpts++"\"\n"++
                "cmdsnvars=\""++Cmdsnvars++"\"\n",
    ClauseHead = "elif [[ ${prev} == "++Name++" ]] ; then\n",
    ClauseHead++ClauseBody.

main(Commands, #{shell:=bash, aliases:=Aliases}) ->
    CmdNames = [Name || #{name:=Name} <- Commands],
    Triggers = ["rebar3" | Aliases],
    TriggerConds = lists:map(fun(T) -> "${prev} == \""++T++"\"" end, Triggers),
    Trigger = string:join(TriggerConds," || "),
    IfTriggerThen = "if [[ "++Trigger++" ]] ; then\n",
    ["_rebar3(){\n",
    "   local cur prev sopts lopts cmdsnvars\n",
    "   COMPREPLY=()\n",
    "   cur=\"${COMP_WORDS[COMP_CWORD]}\"\n",
    "   prev=\"${COMP_WORDS[COMP_CWORD-1]}\"\n",
    "       ",IfTriggerThen,
    "           sopts=\"-h -v\"\n"
    "           lopts=\"--help --version\"\n",
    "           cmdsnvars=\"",string:join(CmdNames," \\\n"),"\"\n",
    "       ",[cmd_clause(Cmd) || Cmd <- Commands],
    "       fi\n"
    "    COMPREPLY=( $(compgen -W \"${sopts} ${lopts} ${cmdsnvars}\" -- ${cur}) )\n"
    "    if [ -n \"$COMPREPLY\" ] ; then\n"
    "        # append space if matched\n"
    "       COMPREPLY=\"${COMPREPLY} \"\n"
    "        # remove trailing space after equal sign\n"
    "        COMPREPLY=${COMPREPLY/%= /=}\n"
    "    fi\n"
    "    return 0\n"
    "}\n"].

complete(#{shell:=bash, aliases:=Aliases}) ->
    Triggers = ["rebar3" | Aliases],
    ["complete -o nospace -F _rebar3 "++ Trigger ++"\n" || Trigger <- Triggers].
