# 2016

Solutions in SWI-Prolog.

Usage: Your `init.pl` must have the following:
```prolog
:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(reif)).
:- use_module(library(yall)).
:- set_prolog_flag(back_quotes, string).
:- set_prolog_flag(double_quotes, chars).
```
To get `library(reif)`, do `pack_install(reif).` in `swipl`.