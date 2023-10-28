%% @doc Core utility of tabtab
%% @end

-module(tabtab_core).

%% Simplified argparse structures
-type tt_argument() :: #{short => char() | undefined,
                        long => string() | undefined,
                        type => atom | binary | boolean | float | integer | string | keyval,
                        help => string()}.

-type tt_command() :: #{name := string(),
                        arguments := [tt_argument()],
                        commands => [tt_command()]}.

-type tt_opts() :: #{shell => bash | zsh | fish,
                    hints => boolean(),
                    type_hints => boolean(),
                    integration => function() | undefined,
                    aliases => [string()]}.

-export_type([tt_opts/0, tt_command/0, tt_argument/0]).

-export([command/4]).

%% @doc Convert one of the supported data structures to tt_command().
-spec command(argparse, CmdName::string(),argparse:command(), tt_opts()) -> tt_command();
            (getopt, CmdName::string(), [getopt:option_spec()], tt_opts()) -> tt_command().
            %% TODO ovo ne treba biti tu nego negdje drugdje! loadaj templejte negdje drugdje (koristi ?TEMPLEATE_RE iz rebar.hrl)
            %% (rebar3_template, {TName::string(),[{atom(),term()}]}, tt_opts()) -> tt_command().
command(argparse, CmdName, _ArgParse, _TTOpts) ->
    %% TODO
    #{name => CmdName,
    arguments => []};
command(getopt, CmdName, CmdOpts, _TTOpts) ->
    Args = [#{short=>S,
            long=>L,
            type=>tt_arg_type(Spec),
            help=>H} || {_,S,L,Spec,H} <- CmdOpts],
    #{name => CmdName,
    arguments => Args}.

tt_arg_type({Type,_Default}) ->
    Type;
tt_arg_type(Type) ->
    Type.