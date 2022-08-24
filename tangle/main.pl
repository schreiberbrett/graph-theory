% File declaration for any translations into Prolog
window([], []).

window([CarL|CdrL], [CarO|CdrO]) :-
    CarO = [[], CarL, CdrL],
    mapShove(CarL, CdrLZipperLists, CdrO),
    window(CdrL, CdrLZipperLists).
    
shove(A, L, O) :-
    L = [Left, X, Right],
    O = [[A|Left], X, Right].
window2(L, O) :-
    window(L, ZipperLists),
    flatmapWindow2Helper(ZipperLists, O).
    
window2Helper(ZipperList, IndexedPairs) :-
    ZipperList = [L, X, R],
    window(R, Triples),
    mapMakeIndexedPair(L, X, Triples, IndexedPairs).
    
makeIndexedPair(L, X, [M, Y, R], [L, X, M, Y, R]).
flatmapWindow2Helper([], []).
flatmapWindow2Helper([CarL|CdrL], O) :-
    window2Helper(CarL, OLeft),
    append(OLeft, ORight, O),
    flatmapWindow2Helper(CdrL, ORight).
    
mapShove(_, [], []).
mapShove(A, [CarL|CdrL], [CarO|CdrO]) :-
    shove(A, CarL, CarO),
    mapShove(A, CdrL, CdrO).
    
mapMakeIndexedPair(_, _, [], []).
mapMakeIndexedPair(L, X, [CarTriples|CdrTriples], [CarO, CdrO]) :-
    makeIndexedPair(L, X, CarTriples, CarO),
    mapMakeIndexedPair(L, X, CdrTriples, CdrO).
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
    is_proof_step(Hleft, Hright, Hout, C, Step),
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
filter_cbs(X, [L|Ls], Os) :-
    does_overlap(X, L),
    filter_cbs(X, Ls, Os).
filter_cbs(X, [L|Ls], [L|Os]) :-
    no_overlap(X, L),
    filter_cbs(X, Ls, Os).

overlaps([], [], f).
overlaps([l|_], [l|_], t).
overlaps([l|_], [r|_], t).
overlaps([r|_], [l|_], t).
overlaps([r|_], [r|_], t).

overlaps([o|Xs], [_|Ys], B) :-
    overlaps(Xs, Ys, B).

overlaps([_|Xs], [o|Ys], B) :-
    overlaps(Xs, Ys, B).


does_overlap([l|_], [l|_]).
does_overlap([l|_], [r|_]).
does_overlap([r|_], [l|_]).
does_overlap([r|_], [r|_]).
does_overlap([_|X], [_|Y]) :- does_overlap(X, Y).

no_overlap([], []).
no_overlap[[o|X], [o|Y]) :- no_overlap(X, Y).
no_overlap([o|X], [l|Y]) :- no_overlap(X, Y).
no_overlap([o|X], [r|Y]) :- no_overlap(X, Y).
no_overlap([l|X], [o|Y]) :- no_overlap(X, Y).
no_overlap([r|X], [o|Y]) :- no_overlap(X, Y).


k4_hypergraph([
	[1, 1, 1, 0],
	[0, 1, 1, 1]]).

k4_cbs([
	[l, o, o, r]]).

abc_hypergraph([
	[1, 0, 1, 1, 0, 0, 0, 0, 0],
    [0, 1, 0, 0, 1, 1, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 1, 1, 1]]).

abc_cbs([
    [l, r, o, o, o, o, o, o, o],
    [o, o, o, o, o, l, r, o, o],
    [o, o, o, o, l, o, r, o, o],
    [o, o, o, o, l, l, r, o, o],
    [o, o, o, l, o, o, r, o, o],
    [o, o, o, l, o, l, r, o, o],
    % [o, o, o, l, l, o, r, o, o],
    [o, o, l, l, l, l, r, o, o]]).
    
abc_proof(Proof) :- 
    abc_hypergraph(X),
    abc_cbs(Y),
    proves(X, Y, Proof).

