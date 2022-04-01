# 2016

Solutions in SWI-Prolog.

Usage: Your `init.pl` must have the following:
```prolog
:- set_prolog_flag(back_quotes, codes).
:- set_prolog_flag(double_quotes, chars).
:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(func)).
:- use_module(library(reif)).
:- use_module(library(yall)).
```
To get `library(func)` and `library(reif)`, execute the following in `swipl` and choose an install location.
```prolog
pack_install(reif).
pack_install(func).
```