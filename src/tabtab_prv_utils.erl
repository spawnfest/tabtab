%% @doc Wrapper for accessing provider attributes -
%%      for some reason providers don't expose some of their attributes via API!
%% @end

-module(tabtab_prv_utils).

-export([name/1, bare/1, short_desc/1, opts/1, namespace/1]).

name(Provider) ->
    element(2, Provider).
bare(Provider) ->
    element(5, Provider).
short_desc(Provider) ->
    element(8, Provider).
opts(Provider) ->
    element(10, Provider).
namespace(Provider) ->
    element(12, Provider).