# 2016

Solutions in SWI-Prolog.

Usage: `make`

Install the following packages.

```prolog
:- pack_install(edcg).
:- pack_install(func).
:- pack_install(reif).
```

Use the following `init.pl`.

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
