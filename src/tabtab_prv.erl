%% @doc Rebar3 provider that creates autocompletion files.
%% @end
-module(tabtab_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, tabtab).
-define(DEPS, [app_discovery]).
%% TODO definirani fileovi neka se zovu "_rebar3" i nek se spreme u _build/profile/completion/<shell>/_rebar3

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 tabtab"}, % How to use the plugin
            %% TODO opts
            {opts, [
                {shell, $s, "shell", {atom,detect_shell()}, "Shell type, bash or zsh"},
                {hints, $h, "hints", {boolean, true}, "Describe arguments/commands in completion (defaults to true)"},
                {type_hints, $t, "type_hints", {boolean, false}, "Hint argument types in completion (defaults to false)"}
            ]},                   % list of options understood by the plugin
            {short_desc, "Rebar3 plugin for generating completion files"},
            {desc, "Rebar3 plugin for generating completion files"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {CliOpts, _} = rebar_state:command_parsed_args(State),
    TTOpts = #{shell=>proplists:get_value(shell, CliOpts),
                hints=>proplists:get_value(hints, CliOpts),
                type_hints=>proplists:get_value(type_hints, CliOpts),
                %% TODO expose opts via config
                aliases=>[],
                file=>"_rebar3"},
    
    Providers0 = rebar_state:providers(State),
    BareProviders = lists:filter(fun tabtab_prv_utils:bare/1, Providers0),
    ByNamespace = maps:groups_from_list(fun tabtab_prv_utils:namespace/1, BareProviders),
    Cmds0 = maps:fold(
                fun(NS,Ps,CmdAcc) -> namespace_to_tt_commands(NS, Ps, TTOpts)++CmdAcc end,
                [],
                ByNamespace),
    Cmds = lists:map(fun (Cmd) -> oracle(Cmd, TTOpts, State) end, Cmds0),
    Compl = tabtab_gen:generate(Cmds, TTOpts),

    write_completion(Compl,State,TTOpts),
    {ok, State}.

namespace_to_tt_commands(default,Providers,TTOpts) ->
    lists:map(fun(P)->provider_to_tt_command(P,TTOpts) end,Providers);
namespace_to_tt_commands(Namespace,Providers,TTOpts) ->
    Name = atom_to_list(Namespace),
    [#{name=>Name,
      commands=>lists:map(fun(P)->provider_to_tt_command(P,TTOpts) end, Providers),
      arguments=>[],
      help=>Name++" namespace"}].

provider_to_tt_command(Provider,TTOpts) ->
    Opts = providers:opts(Provider),
    Name = providers:impl(Provider),
    Cmd = tabtab_core:command(getopt,atom_to_list(Name),Opts,TTOpts),
    Help = tabtab_prv_utils:short_desc(Provider),
    Cmd#{help=>Help}.

%% ad-hoc injection of data for some known providers!
oracle(#{name:="new"}=Cmd, _TTOpts, State)->
    %% template completion
    Cmd#{commands=>templates_as_commands(State)};
oracle(#{name:="as"}=Cmd, _TTOpts, State) ->
    %% profile completion
    ConfigProfiles = rebar_opts:get(rebar_state:opts(State), profiles, []),
    Args = [#{short=>undefined,
            long=>atom_to_list(ProfileName),
            help=>undefined,
            type=>keyval} || {ProfileName,_} <- ConfigProfiles],
    Cmd#{arguments=>Args};
oracle(Cmd,_,_) ->
    Cmd.

templates_as_commands(State) ->
    Templates = rebar_templater:list_templates(State),
    lists:map(fun template_as_command/1, Templates).

template_as_command({Name, _Type, _File, _Desc, Vars}) ->
    Args = [template_var_to_tt_arg(Var) || Var <- Vars],
    #{name=>Name,
    arguments=>Args,
    commands=>[],
    help=>Name++" template"}.

template_var_to_tt_arg({Name, Default})->
    template_var_to_tt_arg({Name, Default,undefined});
template_var_to_tt_arg({Name, _Default,Help})->
    #{short=>undefined,
    long=>atom_to_list(Name)++"=",
    help=>Help,
    type=>keyval}.

detect_shell() ->
    case os:getenv("SHELL") of 
        false ->
            rebar_log:log(diagnostic,
                "SHELL variable not set, using default shell: ~p~n",[bash]),
            bash;
        Path  -> 
            list_to_existing_atom(
                lists:last(string:split(Path, "/", all)))
    end.

write_completion(CompletionStr, State, #{file:=Filename}) ->
    Dir = rebar_dir:base_dir(State),
    Dest = filename:join(Dir, Filename),
    rebar_log:log(diagnostic,"Writing completion file to: ~p~n",[Dest]),
    %% TODO handle errors
    ok = file:write_file(Dest, CompletionStr, [raw]).

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).