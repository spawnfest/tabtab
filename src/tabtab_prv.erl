-module(tabtab_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, tabtab).
-define(DEPS, [app_discovery]).

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
    TTOpts = #{shell=>bash, aliases=>["r3","rebar"]},
    Providers = rebar_state:providers(State),
    %% This is a hack because "providers" (for some reason) is not exporting getter for "bare" attr.
    Bare = fun(Provider) -> element(5, Provider)=:=true end,
    BareProviders = lists:filter(Bare, Providers),
    ByNamespace = maps:groups_from_list(fun providers:namespace/1, BareProviders),
    Cmds = maps:fold(
                fun(NS,Ps,CmdAcc) -> namespace_to_tt_commands(NS, Ps)++CmdAcc end,
                [],
                ByNamespace),
    Compl = tabtab_gen:generate(Cmds, TTOpts),
    ok = file:write_file(filename:join(rebar_dir:root_dir(State), "rebar3.compl"), Compl),
    embedd(State,TTOpts),
    {ok, State}.

namespace_to_tt_commands(default,Providers) ->
    lists:map(fun provider_to_tt_command/1,Providers);
namespace_to_tt_commands(Namespace,Providers) ->
    [#{name=>atom_to_list(Namespace),
      commands=>lists:map(fun provider_to_tt_command/1, Providers),
      arguments=>[]}].

provider_to_tt_command(Provider) ->
    Opts = providers:opts(Provider),
    Name = providers:impl(Provider),
    tabtab_core:command(getopt,atom_to_list(Name),Opts).

embedd(State,#{shell:=bash}) ->
    FN = filename:join(rebar_dir:root_dir(State), "rebar3.compl"),
    os:cmd("source <(cat "++FN++")").

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).