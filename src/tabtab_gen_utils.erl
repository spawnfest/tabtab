%% @doc TODO
%% @end

-module(tabtab_gen_utils).

-export([header/1]).

header(#{shell:=Shell}) ->
    "# "++atom_to_list(Shell)++" completion file for rebar3 (autogenerated by tabtab plugin).\n".