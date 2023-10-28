# Tabtab
-----
Autocompletion generator for `rebar3`.

## Current drawbacks
1. There is no autocomplete for `rebar3` plugins!
2. There is no autocomplete for `rebar3` templates!
3. Current `rebar3` is static and handwritten - each update to providers may demand update in autocomplete file.

## Why is this needed?
Is there something more frustrating than a typo in the CLI? Also it gets your brain free of the following questions while developing:
   1.   what is the command name for some plugin?
   2.   what flags are accepted for some command?
   3.   what templates are available?
   4.   what variables are available for some template?
You can get *1* and *2* with `help` command, *3* with `rebar3 new` command and *4* by manually inspecting template file (:D). Or you can just press `tab` twice. Just like code autocomplete, shell autocompletion makes your life easier.

There is also [this issue](https://github.com/erlang/rebar3/issues/2077) from one of the maintainers so I guess this will be welcome, although the issue doesn't mention autocompleting plugins/templates. 

Goal: Create a provider that creates an autocompletion file based on the current configuration. This can lead to different autocompletion in different shell sessions, so basic mechanism for integration should also be supported.

Name: You usually press `tab` twice to trigger autocomplete listing, hence `tabtab`.

## Implementation

-   Implement it as plugin or as part of `rebar3`?
    -   I decided to go with the plugin because I can develop it on my own if `rebar3` maintainers decide not to include it in `rebar3`. Also, it should be easy to convert plugin to core components, but not the other way around.