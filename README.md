# Tabtab
Rebar3 plugin - autocompletion generator for `rebar3`.

## Drawbacks of current completion files
1. There is no completion for `rebar3` plugins! (nor providers, nor their flags)
2. There is no completion for `rebar3` templates! (nor template name, nor variables!)
3. There is no completion for `rebar3` aliases defined in `rebar.config`
4. There is no completion for profiles when performing `rebar3 as`
5. Whenever autocompleting flags, after you type the first flag, completion is gone!
6. There is no completion for tasks when performing `rebar3 do`
7. Current `rebar3` is static and handwritten - each update to providers may demand update in autocomplete **files** (update for each shell type!).
8. No support for `rebar3` aliases. You wan't your autocompletion to trigger on `r3` or `rebar`? You need to modify the autocompletion file yourself :D
9. No type hints for arguments
10. Limited nesting support - you cant have a task that has tasks, etc.
    -   also only 1 previous word can be seen which could also cause collisions in nested commands!

There actually exist [an issue](https://github.com/erlang/rebar3/issues/2077) from one of the `rebar3` maintainers so I guess this will be welcome, although the issue doesn't mention autocompleting plugins/templates and other stuff. I believe that, if proven useful, this may become integrated into `rebar3`.

## Goal
Create a `rebar3` plugin that generates an completion file based on the current configuration of the project. This can lead to different completions in different shell sessions, so basic mechanism for integration should also be supported. Without good autocomplete it's hard to use tools that have a lot of (nested) commands (e.g. kubernetes, docker, 1password, ...) - like programming without LSP.

## Non-goals (currently)
-   Efficiency - I've abused `lists:concat/1` and strings *a lot*!
-   Error handling - It's very minimal currently
-   Testing

## Features
-   [x] **Dynamically generated providers autocomplete**
-   [x] **Autocomplete for plugin** providers and their args
-   [x] **Autocomplete for templates** and **their variables**
-   [x] **Preserve autocomplete after first flag/arg**
-   [x] Autocomplete for `rebar3` aliases defined in `rebar.config`
-   [x] Autocomplete profile when running `rebar3 as`
-   [ ] Autocomplete tasks when running `rebar3 do`
-   [x] **Unlimited command nesting supported!** 
    -  Ready to support `argparse` one day!
-   [x] Autodetect shell type
-   [ ] Automatic integration
    -  Probably doable via rebar hooks!
-   [ ] Type hints
-   [x] Support for OS-level aliases
    -   Out-of-the-box only in `bash`, 
    -   For `zsh` read [this](#zsh-trigger-autocomplete-on-alias).

## Demo
### Prerequisites:
-   some Unix/MacOS (tested on Ubuntu 20.04 with both shells)
-   `bash` or `zsh` shell (tested on `bash 5.0.17` and `zsh 5.8`)
-   `rebar3` (tested on `3.22.0`)
-   `erlang` version >= `25.0`  (tested on `26.1.2`)

### Steps:
1.  Include `tabtab` in `project_plugins` in `rebar3.config`
    -   **1. method** (recommended if you want to modify something in the program)
        1.   Clone `tabtab`
        2.   Put `{project_plugins, [tabtab]}` in `rebar.config`
        3.   `cd` to some rebar3 project and run the following:
        ```shell
        mkdir -p _checkouts
        ln -s $PATH_TO_TABTAB_DIR _checkouts/tabtab 
        ```
        
    -   **2. method**
        -   Put `{project_plugins, [{rebar3_gen_pkgs, {git, "git@github.com:spawnfest/tabtab.git", {branch, "master"}}}]}` in `rebar.config`
2.  Run `rebar3 compile` or `rebar3 plugins list` to compile `tabtab` plugin
3.  Run `rebar3 tabtab` to generate a completion file
    -   shell type is autodetected via `$SHELL` variable, but you can specify it via `-s` flag
    -   By default, completion file is named `_rebar3` and is located in `_build/<profile>/`
        -   You can override this with `file` [config value](#configuration-and-options)
4.  Integrate the completion file
    -   **bash**
        -   run `source <(cat $PATH_TO_COMPLETION_FILE)`
    -   **zsh**
        -   If first time doing `zsh` autocompletion, read [this](#zsh-tutorial)
        -   run `eval "$(cat $PATH_TO_COMPLETION_FILE)"; compdef _rebar3 rebar3`
            -   or `source ~/.zshrc` (if your file is somewhere in `$fpath`)
5.  **Enjoy** not trying to remember all plugin commands and flags :D

## Future work
-   Support more shells types
-   Style improvements
-   Utilize `tabtab` to generate autocompletion for `escript`s
-   Automatic integration of completion files



## Extras

### Configuration and options
Configuration is in `rebar.config` and options are passed in CLI when running `rebar3 tabtab`.

Configuration:
```erlang
    {tabtab, [{file,file:filename_all()},
              {aliases, [string()]}]}
```
-   `file` - absolute or relative (to `_build/<profile/`)
-   `aliases` - list of strings on which you want to trigger autocomplete

Options:
|   Short   |   Long      |   Value   | Default |
|-----------|-------------|-----------|---------|
|   `-s`    |   `--shell`| `bash` or `zsh` | autodetected (`bash` if detection fails) |
|   `-h`    |   `--hints`| boolean | `true` |
|   `-t`    |   `--type_hints`| boolean | `false` |

### Implementation
-   Implement it as plugin or as part of `rebar3`?
    -   I decided to go with the plugin because I can develop it on my own if `rebar3` maintainers decide not to include it in `rebar3`. Also, it should be easy to convert plugin to core components, but not the other way around.
-   Core part must support both `argparse` and `getopt` argument specification
    -   Reason 1: `rebar3` may switch to `argparse` somewhere in the future
    -   Reason 2: Core part could be used to autocomplete `escripts`.

### ZSH tutorial
I've had some troubles with zsh autocompletion because I've done it first time, so here are steps to save you some time:

1. Install `zsh`
2. Create `.zshrc` file in your `$HOME` directory and add the following:
   -   ```shell
        fpath=( $DIR_WHERE_YOU_STORE_COMPL_FILES "${fpath[@]}" )

        autoload -U compinit; compinit
        ```

**!** With `zsh` it is important that the completion file is named exactly as completion function it introduces! (`_rebar3` in this case)

#### ZSH trigger autocomplete on alias
For alias e.g. `r3` you need file `_r3` somewhere in your `fpath` with the following contents:
```shell
#compdef _r3 r3

function _r3 {
    _rebar3
}   
```
and then load that file as the original completion file.

### Inspo
Got inspired by 1Password completion generation. [Instructions for integrations](https://developer.1password.com/docs/cli/reference/commands/completion/) are also mostly theirs.
