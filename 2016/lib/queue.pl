% Queue implementation based on the one from Richard O'Keefe - The Craft of Prolog
% A queue is a term q(N, H-T), where
% N is the number of elements in successor notation and
% H-T is the difference list representing the queue.

% This allows for fast appends by filling the hole from T.

:- module(queue,
       [empty_queue/1,
        singleton_queue/2,
        queue_head_/3,
        queue_tail_/3,
        list_queue/2]).

empty_queue(q(0, H-H)).

singleton_queue(X, q(s(0), [X|T]-T)).

% queue_head_(Q0, X, Q) where Q has X as its head.
queue_head_(q(N, H-T), X, q(s(N), [X|H]-T)).

% queue_tail_(Q0, X, Q) where Q has X as its tail.
queue_tail_(q(N, H-[X|T]), X, q(s(N), H-T)).

% Bidirectional conversion between a list and queue.
list_queue([], q(0, T-T)).
list_queue([X|Xs], q(s(N), [X|H]-T)) :-
    list_queue(Xs, q(N, H-T)).