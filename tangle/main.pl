riffle([], [], []).
riffle([A|As], [], [A|As]).
riffle([], [B|Bs], [B|Bs]).

riffle([A|As], [B|Bs], [A|Os]) :-
    riffle(As, [B|Bs], Os).

riffle([A|As], [B|Bs], [B|Os]) :-
    riffle([A|As], Bs, Os).



proof_step([], [], [], [], []).
proof_step([Step|Steps], 
           [Hleft|Hlefts],
           [Hright|Hrights],
           [C|Cbs],
           [Hout|Houts]) :-
    is_proof_step(Step, Hleft, Hright, C, Hout),
    proof_step(Steps, Hlefts, Hrights, Cbs, Houts).

is_proof_step(0, 0, 0, o, o).
is_proof_step(1, 0, 0, l, r1).
is_proof_step(1, 0, 1, o, b1).
is_proof_step(0, 1, 0, r, r2).
is_proof_step(0, 1, 1, o, b2).
is_proof_step(1, 1, 1, o, b).


proves(H, _, []) :- any_less_than_three(H).
proves(H, Cbs, [Step|Steps]) :-
    riffle([H1, H2], Hrest, H),
    riffle([HL], [HR], [H1, H2]),
    riffle([C], Crest, Cbs),
    proof_step(Step, HL, HR, C, Hout),
    filter_cbs(C, Crest, NewCbs),
    proves([Hout|Hrest], NewCbs, Steps).


any_less_than_three([H|_]) :- less_than_three(H).
any_less_than_three([_|Hs]) :- any_less_than_three(Hs).

less_than_three(X) :- size(X, z).
less_than_three(X) :- size(X, s(z)).
less_than_three(X) :- size(X, s(s(z))).
                           
size([], z).
size([0|Xs], N) :- size(Xs, N).
size([1|Xs], s(N)) :- size(Xs, N).


filter_cbs(_, [], []).
filter_cbs(X, [L|Ls], [L|Os]) :-
    overlaps(X, L, f),
    filter_cbs(X, Ls, Os).
filter_cbs(X, [L|Ls], Os) :-
    overlaps(X, L, t),
    filter_cbs(X, Ls, Os).

overlaps([], [], f).
overlaps([l|_], [l|_], t).
overlaps([l|_], [r|_], t).
overlaps([r|_], [l|_], t).
overlaps([r|_], [r|_], t).

overlaps([o|Xs], [Y|Ys], B) :-
    overlaps(Xs, Ys, B).

overlaps([X|Xs], [o|Ys], B) :-
    overlaps(Xs, Ys, B).

k4_hypergraph([
	[1, 1, 1, 0],
	[0, 1, 1, 0]]).

k4_cbs([
	[l, o, o, r]]).

