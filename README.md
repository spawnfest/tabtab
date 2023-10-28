# Tabtab
-----
Autocompletion generator for `rebar_3`

Why is this needed?
1. There is no autocomplete for `rebar_3` plugins!
2. There is no autocomplete for `rebar_3` templates!
3. Current `rebar_3` is static and handwritten - each update to providers may demand update in autocomplete file.

Goal: Create a provider that creates an autocompletion file based on the current configuration. This can lead to different autocompletion in different shell sessions, so basic mechanism for integration should also be supported.
