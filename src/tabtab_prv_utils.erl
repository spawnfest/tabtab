%% @doc Access funcs for 
%% @end

-module(tabtab_prv_utils).

-define(bare, 5).
-define(short_desc,8).

-export([get/2]).

get(bare,Provider) ->
    element(?bare, Provider);
get(short_desc,Provider) ->
    element(?short_desc, Provider).

