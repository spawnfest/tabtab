%% @doc Autocompletion file generator functions
%% @end

-module(tabtab_gen).

-export([generate/2]).

-callback generate([tabtab_core:tt_command()], tabtab_core:tt_opts()) -> string().

-spec generate([tabtab_core:tt_command()], tabtab_core:tt_opts()) -> string().
generate(Commands, #{shell:=bash}=TTOpts) ->
    tabtab_gen_bash:generate(Commands,TTOpts);
generate(Commands, #{shell:=zsh}=TTOpts) ->
    tabtab_gen_zsh:generate(Commands,TTOpts).