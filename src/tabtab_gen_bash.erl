%% @doc Completion file generator for bash
%% @end

-module(tabtab_gen_bash).

-behavior(tabtab_gen).

-export([generate/2]).

-define(str(N), integer_to_list(N)).

-spec generate([tabtab_core:tt_command()], tabtab_core:tt_opts()) -> string().
generate(Commands, #{shell:=bash}=TTOpts) ->
    lists:flatten([tabtab_gen_utils:header(TTOpts),
        main(Commands, TTOpts),
        complete(TTOpts),
        io_lib:nl()]).

cmd_clause(Cmd) ->
    nested_cmd_clause(Cmd, [], 1).

-spec nested_cmd_clause(tabtab_core:tt_command(), [string()], pos_integer()) -> string().
nested_cmd_clause(#{name:=Name}=Cmd,Prevs,Depth) ->
    Args0 = maps:get(arguments, Cmd, []),
    Cmds = maps:get(commands, Cmd, []),
    {Args, KeyValArgs} = lists:partition(fun(#{type:=T}) -> T=/=keyval end, Args0),
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
    KVs = string:join(
            lists:map(fun(#{long:=L})->L end, KeyValArgs),
            " "),
    IfBody = match_prev_if_body([Name | Prevs]),
    ClauseHead = "elif [[ "++IfBody++" ]] ; then\n",
    ClauseBody = "      sopts=\""++SOpts++"\"\n"++
                "       lopts=\""++LOpts++"\"\n"++
                "       cmdsnvars=\""++Cmdsnvars++"\"\n"++
                "       kvs=\""++KVs++"\"\n",
    Nested = lists:concat([nested_cmd_clause(C, [Name | Prevs], Depth+1) || C <- Cmds]),
    ClauseHead++ClauseBody++Nested.

match_prev_if_body([P | Rest]) ->
    string:join(
        do_match_prev_if_body([P | Rest],1),
        " && ").

do_match_prev_if_body([],_) ->
    [];
do_match_prev_if_body([P | Rest],Cnt) ->
    ["${prev"++?str(Cnt)++"} == "++P | do_match_prev_if_body(Rest,Cnt+1)].

main(Commands, #{shell:=bash, aliases:=Aliases}) ->
    MaxDepth=cmd_depth(Commands,1,0),
    CmdNames = [Name || #{name:=Name} <- Commands],
    Triggers = ["rebar3" | Aliases],
    TriggerConds = lists:map(fun(T) -> "${prev1} == \""++T++"\"" end, Triggers),
    Trigger = string:join(TriggerConds," || "),
    IfTriggerThen = "if [[ "++Trigger++" ]] ; then\n",
    ["_rebar3_ref_idx() {\n",
    "   if [[ ${COMP_WORDS[1]} == new && COMP_CWORD -gt 3 ]] ; then\n",
    "       return $((2 + (COMP_CWORD-3)))\n",
    "   fi\n",
    "   startc=$2\n",
    "   # is flag\n",
    "    if [[ ${COMP_WORDS[COMP_CWORD-${startc}-1+$1]} == -* ]] ; then\n",
    "        startc=$((startc+1))\n",
    "        _rebar3_ref_idx $1 $startc\n",
    "    fi\n",
    "    return $startc\n",
    "}\n",
    "\n",
    "_rebar3(){\n",
    "   local cur sopts lopts cmdsnvars refidx kvs\n",
    "   local "++string:join(["prev"++?str(I) || I <- lists:seq(1, MaxDepth)], " ")++"\n",
    "   COMPREPLY=()\n",
    "   _rebar3_ref_idx "++?str(MaxDepth)++" "++?str(MaxDepth)++"\n",
    "   refidx=$?\n",
    "   cur=\"${COMP_WORDS[COMP_CWORD]}\"\n",
    lists:concat(prev_definitions(MaxDepth,1)),
    "       ",IfTriggerThen,
    "           sopts=\"-h -v\"\n"
    "           lopts=\"--help --version\"\n",
    "           cmdsnvars=\"",string:join(CmdNames," \\\n"),"\"\n",
    "       ",lists:concat([cmd_clause(Cmd) || Cmd <- Commands]),
    "       fi\n"
    "    COMPREPLY=( $(compgen -W \"${sopts} ${lopts} ${cmdsnvars} ${kvs}\" -- ${cur}) )\n"
    "    if [ -n \"$COMPREPLY\" ] ; then\n"
    "        # append space if matched\n"
    "       COMPREPLY=\"${COMPREPLY} \"\n"
    "        # remove trailing space after equal sign\n"
    "        COMPREPLY=${COMPREPLY/%= /=}\n"
    "    fi\n"
    "    return 0\n"
    "}\n"].

prev_definitions(MaxDepth, Cnt) when (Cnt-1)=:=MaxDepth ->
    [];
prev_definitions(MaxDepth, Cnt) ->
    P = "   prev"++?str(Cnt)++"=\"${COMP_WORDS[COMP_CWORD-${refidx}+"++?str((MaxDepth-Cnt))++"]}\"\n",
    [P | prev_definitions(MaxDepth,Cnt+1)].

cmd_depth([], _, Max) ->
    Max;
cmd_depth([#{commands:=[]} | Rest],Depth,Max) ->
    cmd_depth(Rest,Depth,max(Depth,Max));
cmd_depth([#{commands:=Cmds} | Rest],Depth, Max) ->
    D = cmd_depth(Cmds, Depth+1, Max),
    cmd_depth(Rest, Depth, max(D,Max));
cmd_depth([_ | Rest],Depth,Max) ->
    cmd_depth(Rest,Depth,max(Depth,Max)).

complete(#{shell:=bash, aliases:=Aliases}) ->
    Triggers = ["rebar3" | Aliases],
    ["complete -o nospace -F _rebar3 "++ Trigger ++"\n" || Trigger <- Triggers].