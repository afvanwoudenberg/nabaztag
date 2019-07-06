#!/usr/bin/env swipl

:- set_prolog_flag(verbose,silent).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(broadcast)).

:- consult(nabaztag).

:- initialization http_daemon.

%:- listen(http(post_server_start), prolog_server(4000,[])).

