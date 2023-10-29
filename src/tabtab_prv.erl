%% TODO COLISIONS in namespaces!!!!!!!
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
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "A rebar plugin"},
            {desc, "A rebar plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    DefaultOpts = #{shell=>detect_shell(),
                    aliases=>[],
                    hints=>false,
                    integration=>undefined,
                    type_hints=>false},
    %% TODO expose opts via config and merge them with defaults!
    TTOpts = DefaultOpts#{shell=>zsh},
    Providers = rebar_state:providers(State),
    %% This is a hack because "providers" (for some reason) is not exporting getter for "bare" attr.
    Bare = fun(Provider) -> element(5, Provider)=:=true end,
    BareProviders = lists:filter(Bare, Providers),
    ByNamespace = maps:groups_from_list(fun providers:namespace/1, BareProviders),
    Cmds0 = maps:fold(
                fun(NS,Ps,CmdAcc) -> namespace_to_tt_commands(NS, Ps, TTOpts)++CmdAcc end,
                [],
                ByNamespace),
    Cmds = lists:map(fun (Cmd) -> oracle(Cmd, TTOpts, State) end, Cmds0),
    Compl = tabtab_gen:generate(Cmds, TTOpts),
    ok = file:write_file(filename:join(rebar_dir:root_dir(State), "rebar3.compl"), Compl),
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
    Help = tabtab_prv_utils:get(short_desc, Provider),
    Cmd#{help=>Help}.

oracle(#{name:="new"}=Cmd, _TTOpts, State)->
    %% template autocomplete
    NewCmds=templates_as_commands(State),
    Cmd#{commands=>NewCmds};
oracle(#{name:="as"}=Cmd, _TTOpts, State) ->
    ConfigProfiles = rebar_opts:get(rebar_state:opts(State), profiles, []),
    %% TODO keyval nije dobro ime
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
    arguments=>Args}.

template_var_to_tt_arg({Name, Default})->
    template_var_to_tt_arg({Name, Default,undefined});
template_var_to_tt_arg({Name, _Default,Help})->
    #{short=>undefined,
    long=>atom_to_list(Name)++"=",
    help=>Help,
    type=>keyval}.

detect_shell() ->
    case os:cmd("SHELL") of 
        false -> bash;
        Path  -> hd(lists:reverse(string:split(Path, "/", all)))
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).