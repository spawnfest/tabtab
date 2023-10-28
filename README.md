# Tabtab
-----
Autocompletion generator for `rebar3`.

## Current drawbacks
1. There is no autocomplete for `rebar3` plugins! (nor providers, nor their flags)
2. There is no autocomplete for `rebar3` templates! (nor template name, nor variables!)
3. There is no autocomplete for `rebar3` aliases defined in `rebar.config`
4. There is no autocomplete for profiles when performing `rebar3 as`
5. (**!**)Whenever autocompleting flags, after you type the first flag, autocomplete is gone!
6. There is no autocomplete for tasks when performing `rebar3 do`
7. Current `rebar3` is static and handwritten - each update to providers may demand update in autocomplete **files** (update for each shell type!).
8. No support for `rebar3` aliases. You wan't your autocompletion to trigger on `r3` or `rebar`? You need to modify the autocompletion file yourself :D
9. No type hints for arguments
10. Limited nesting support - you cant have a task that has tasks, etc.
    -   only 1 previous word can be seen which could also cause collisions in nested commands!

## Why is this needed?
Is there something more frustrating than a typo in the CLI? Also it gets your brain free of the following questions while developing:
   1.   what is the command name for some plugin?
   2.   what flags are accepted for some command?
   3.   what templates are available?
   4.   what variables are available for some template?
You can get *1* and *2* with `help` command, *3* with `rebar3 new` command and *4* by manually inspecting template file (:D). Or you can just press `tab` twice. Just like code autocomplete, shell autocompletion makes your life easier.

There is also [this issue](https://github.com/erlang/rebar3/issues/2077) from one of the maintainers so I guess this will be welcome, although the issue doesn't mention autocompleting plugins/templates. 

## Goal
Create a plugin that creates an autocompletion file based on the current configuration. This can lead to different autocompletion in different shell sessions, so basic mechanism for integration should also be supported.

Name: You usually press `tab` twice to trigger autocomplete listing, hence `tabtab`.

## Implementation

-   Implement it as plugin or as part of `rebar3`?
    -   I decided to go with the plugin because I can develop it on my own if `rebar3` maintainers decide not to include it in `rebar3`. Also, it should be easy to convert plugin to core components, but not the other way around.
-   Core part must support both `argparse` and `getopt` argument specification
    -   Reason 1: `rebar3` may switch to `argparse` somewhere in the future
    -   Reason 2: Core part could be used to autocomplete `escripts`.

## Features

-   [x] **Dynamically generated providers autocomplete**
-   [x] **Autocomplete for plugins** providers and their args
-   [x] **Autocomplete for templates** and their variables
-   [x] **Preserve autocomplete after first flag/arg**
-   [x] Autocomplete for `rebar3` aliases defined in `rebar.config`
-   [x] Autocomplete profile when running `rebar3 as`
-   [x] Unlimited nesting possibilities supported! 
-   [ ] Automatic integration
-   [ ] Type hints
-   [ ] Autodetect shell type
-   [x] Support for OS-level aliases