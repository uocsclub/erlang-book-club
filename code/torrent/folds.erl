-module(folds).
-export([foldl/3, foldr/3]).

foldl(_F, Init, []) ->
    Init;
foldl(F, Init, [H|T]) ->
    foldl(F, F(H, Init), T).

foldr(_F, Init, []) ->
    Init;
foldr(F, Init, [H|T]) ->
    F(H, foldr(F, Init, T)).

