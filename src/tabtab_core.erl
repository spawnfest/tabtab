%% @doc Core functionality of tabtab
%% @end

-module(tabtab_core).

%% Simplified argparse structures
-type tt_argument() :: #{short => char() | undefined,
                        long => string() | undefined,
                        type => atom | binary | boolean | float | integer | string | keyval,
                        help => string()}.

-type tt_command() :: #{name := string(),
                        help := string() | undefined,
                        arguments := [tt_argument()],
                        commands => [tt_command()]}.

%% TODO support fish and maybe some more shells
-type tt_shell() :: bash | zsh.

-type tt_opts() :: #{shell => tt_shell(),
                    file => file:filename(),
                    aliases => [string()],
                    hints => boolean(),
                    type_hints => boolean()}.

-export_type([tt_opts/0, tt_shell/0, tt_command/0, tt_argument/0]).

-export([command/4]).

%% @doc Convert one of the supported data structures to tt_command().
-spec command(argparse, CmdName::string(), argparse:command(), tt_opts()) -> tt_command();
            (getopt, CmdName::string(), [getopt:option_spec()], tt_opts()) -> tt_command().
command(argparse, CmdName, _ArgParse, _TTOpts) ->
    %% TODO this is currently not important, but should be trivial
    #{name => CmdName,
    arguments => [],
    commands => [],
    help => undefined};
command(getopt, CmdName, CmdOpts, _TTOpts) ->
    Args = [#{short=>S,
            long=>L,
            type=>tt_arg_type(Spec),
            help=>H} || {_,S,L,Spec,H} <- CmdOpts],
    #{name => CmdName,
    arguments => Args,
    commands => [],
    help => undefined}.

tt_arg_type({Type,_Default}) ->
    Type;
tt_arg_type(Type) ->
    Type.