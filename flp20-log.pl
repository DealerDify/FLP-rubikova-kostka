/** 
 * FLP 2020/21 - Projekt 2 (logicky projekt)
 * Havlicek Lukas (xhavli46)
*/

%------------------------Prevzato z input2.pl (vzorovy soubor)------------------------

/** cte radky ze standardniho vstupu, konci na LF nebo EOF */
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).


/** testuje znak na EOF nebo LF */
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).


read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).


/** rozdeli radek na podseznamy */
split_line([],[[]]) :- !.
split_line([' '|T], [[]|S1]) :- !, split_line(T,S1).
split_line([32|T], [[]|S1]) :- !, split_line(T,S1).    % aby to fungovalo i s retezcem na miste seznamu
split_line([H|T], [[H|G]|S1]) :- split_line(T,[G|S1]). % G je prvni seznam ze seznamu seznamu G|S1


/** vstupem je seznam radku (kazdy radek je seznam znaku) */
split_lines([],[]).
split_lines([L|Ls],[H|T]) :- split_lines(Ls,T), split_line(L,H).

/** vypise seznam radku (kazdy radek samostatne) */
write_lines2([]).
write_lines2([H|T]) :- writeln(H), write_lines2(T). %(writeln je "knihovni funkce")

%------------------------Konec z input2.pl (vzorovy soubor)------------------------


%finalni kostka (vsechny strany jsou stejne barvy)
final([
    [[W,W,W]],
    [[W,W,W]],
    [[W,W,W]],
    [[R,R,R],[B,B,B],[O,O,O],[G,G,G]],
    [[R,R,R],[B,B,B],[O,O,O],[G,G,G]],
    [[R,R,R],[B,B,B],[O,O,O],[G,G,G]],
    [[Y,Y,Y]],
    [[Y,Y,Y]],
    [[Y,Y,Y]]   ]).

%Vsechny tahy jsou provadeny ze zakladni pozice pri pohledu "zepredu" na cervenou stranu (nahore bila dole zlura)
%jedno pismenko znaci smer po smeru hodinovych rucicek, 2 pismenka protismeru hodin. rucicek (zde z pohledu dane strany ne zepredu)
%Viz tahy na strance: https://www.youcandothecube.com/solve-it/3x3-solution

%tah U (otoceni vrchni bile strany doleva)
move([
    [[W1,W2,W3]],
    [[W4,W5,W6]],
    [[W7,W8,W9]],
    [[R1,R2,R3],[B1,B2,B3],[O1,O2,O3],[G1,G2,G3]],
    [[R4,R5,R6],[B4,B5,B6],[O4,O5,O6],[G4,G5,G6]],
    [[R7,R8,R9],[B7,B8,B9],[O7,O8,O9],[G7,G8,G9]],
    [[Y1,Y2,Y3]],
    [[Y4,Y5,Y6]],
    [[Y7,Y8,Y9]]],

    [[[W7,W4,W1]],
    [[W8,W5,W2]],
    [[W9,W6,W3]],
    [[B1,B2,B3],[O1,O2,O3],[G1,G2,G3],[R1,R2,R3]],
    [[R4,R5,R6],[B4,B5,B6],[O4,O5,O6],[G4,G5,G6]],
    [[R7,R8,R9],[B7,B8,B9],[O7,O8,O9],[G7,G8,G9]],
    [[Y1,Y2,Y3]],
    [[Y4,Y5,Y6]],
    [[Y7,Y8,Y9]]]).

%tah UU (otoceni vrchni bile strany doprava)
move([
    [[W1,W2,W3]],
    [[W4,W5,W6]],
    [[W7,W8,W9]],
    [[R1,R2,R3],[B1,B2,B3],[O1,O2,O3],[G1,G2,G3]],
    [[R4,R5,R6],[B4,B5,B6],[O4,O5,O6],[G4,G5,G6]],
    [[R7,R8,R9],[B7,B8,B9],[O7,O8,O9],[G7,G8,G9]],
    [[Y1,Y2,Y3]],
    [[Y4,Y5,Y6]],
    [[Y7,Y8,Y9]]],

    [[[W3,W6,W9]],
    [[W2,W5,W8]],
    [[W1,W4,W7]],
    [[G1,G2,G3],[R1,R2,R3],[B1,B2,B3],[O1,O2,O3]],
    [[R4,R5,R6],[B4,B5,B6],[O4,O5,O6],[G4,G5,G6]],
    [[R7,R8,R9],[B7,B8,B9],[O7,O8,O9],[G7,G8,G9]],
    [[Y1,Y2,Y3]],
    [[Y4,Y5,Y6]],
    [[Y7,Y8,Y9]]]).


%tah D (otoceni spodni zlute strany doprava)
move([
    [[W1,W2,W3]],
    [[W4,W5,W6]],
    [[W7,W8,W9]],
    [[R1,R2,R3],[B1,B2,B3],[O1,O2,O3],[G1,G2,G3]],
    [[R4,R5,R6],[B4,B5,B6],[O4,O5,O6],[G4,G5,G6]],
    [[R7,R8,R9],[B7,B8,B9],[O7,O8,O9],[G7,G8,G9]],
    [[Y1,Y2,Y3]],
    [[Y4,Y5,Y6]],
    [[Y7,Y8,Y9]]],

    [[[W1,W2,W3]],
    [[W4,W5,W6]],
    [[W7,W8,W9]],
    [[R1,R2,R3],[B1,B2,B3],[O1,O2,O3],[G1,G2,G3]],
    [[R4,R5,R6],[B4,B5,B6],[O4,O5,O6],[G4,G5,G6]],
    [[G7,G8,G9],[R7,R8,R9],[B7,B8,B9],[O7,O8,O9]],
    [[Y7,Y4,Y1]],
    [[Y8,Y5,Y2]],
    [[Y9,Y6,Y3]]]).

%tah DD (otoceni spodni zlute strany doleva)
move([
    [[W1,W2,W3]],
    [[W4,W5,W6]],
    [[W7,W8,W9]],
    [[R1,R2,R3],[B1,B2,B3],[O1,O2,O3],[G1,G2,G3]],
    [[R4,R5,R6],[B4,B5,B6],[O4,O5,O6],[G4,G5,G6]],
    [[R7,R8,R9],[B7,B8,B9],[O7,O8,O9],[G7,G8,G9]],
    [[Y1,Y2,Y3]],
    [[Y4,Y5,Y6]],
    [[Y7,Y8,Y9]]],

    [[[W1,W2,W3]],
    [[W4,W5,W6]],
    [[W7,W8,W9]],
    [[R1,R2,R3],[B1,B2,B3],[O1,O2,O3],[G1,G2,G3]],
    [[R4,R5,R6],[B4,B5,B6],[O4,O5,O6],[G4,G5,G6]],
    [[B7,B8,B9],[O7,O8,O9],[G7,G8,G9],[R7,R8,R9]],
    [[Y3,Y6,Y9]],
    [[Y2,Y5,Y8]],
    [[Y1,Y4,Y7]]]).


%tah L (otoceni leve zelene strany dolu)
move([
    [[W1,W2,W3]],
    [[W4,W5,W6]],
    [[W7,W8,W9]],
    [[R1,R2,R3],[B1,B2,B3],[O1,O2,O3],[G1,G2,G3]],
    [[R4,R5,R6],[B4,B5,B6],[O4,O5,O6],[G4,G5,G6]],
    [[R7,R8,R9],[B7,B8,B9],[O7,O8,O9],[G7,G8,G9]],
    [[Y1,Y2,Y3]],
    [[Y4,Y5,Y6]],
    [[Y7,Y8,Y9]]],

    [[[O9,W2,W3]],
    [[O6,W5,W6]],
    [[O3,W8,W9]],
    [[W1,R2,R3],[B1,B2,B3],[O1,O2,Y7],[G7,G4,G1]],
    [[W4,R5,R6],[B4,B5,B6],[O4,O5,Y4],[G8,G5,G2]],
    [[W7,R8,R9],[B7,B8,B9],[O7,O8,Y1],[G9,G6,G3]],
    [[R1,Y2,Y3]],
    [[R4,Y5,Y6]],
    [[R7,Y8,Y9]]]).

%tah LL (otoceni leve zelene strany nahoru)
move([
    [[W1,W2,W3]],
    [[W4,W5,W6]],
    [[W7,W8,W9]],
    [[R1,R2,R3],[B1,B2,B3],[O1,O2,O3],[G1,G2,G3]],
    [[R4,R5,R6],[B4,B5,B6],[O4,O5,O6],[G4,G5,G6]],
    [[R7,R8,R9],[B7,B8,B9],[O7,O8,O9],[G7,G8,G9]],
    [[Y1,Y2,Y3]],
    [[Y4,Y5,Y6]],
    [[Y7,Y8,Y9]]],

    [[[R1,W2,W3]],
    [[R4,W5,W6]],
    [[R7,W8,W9]],
    [[Y1,R2,R3],[B1,B2,B3],[O1,O2,W7],[G3,G6,G9]],
    [[Y4,R5,R6],[B4,B5,B6],[O4,O5,W4],[G2,G5,G8]],
    [[Y7,R8,R9],[B7,B8,B9],[O7,O8,W1],[G1,G4,G7]],
    [[O9,Y2,Y3]],
    [[O6,Y5,Y6]],
    [[O3,Y8,Y9]]]).


%tah R (otoceni prave modre strany nahoru)
move([
    [[W1,W2,W3]],
    [[W4,W5,W6]],
    [[W7,W8,W9]],
    [[R1,R2,R3],[B1,B2,B3],[O1,O2,O3],[G1,G2,G3]],
    [[R4,R5,R6],[B4,B5,B6],[O4,O5,O6],[G4,G5,G6]],
    [[R7,R8,R9],[B7,B8,B9],[O7,O8,O9],[G7,G8,G9]],
    [[Y1,Y2,Y3]],
    [[Y4,Y5,Y6]],
    [[Y7,Y8,Y9]]],

    [[[W1,W2,R3]],
    [[W4,W5,R6]],
    [[W7,W8,R9]],
    [[R1,R2,Y3],[B7,B4,B1],[W9,O2,O3],[G1,G2,G3]],
    [[R4,R5,Y6],[B8,B5,B2],[W6,O5,O6],[G4,G5,G6]],
    [[R7,R8,Y9],[B9,B6,B3],[W3,O8,O9],[G7,G8,G9]],
    [[Y1,Y2,O7]],
    [[Y4,Y5,O4]],
    [[Y7,Y8,O1]]]).

%tah RR (otoceni prave modre strany dolu)
move([
    [[W1,W2,W3]],
    [[W4,W5,W6]],
    [[W7,W8,W9]],
    [[R1,R2,R3],[B1,B2,B3],[O1,O2,O3],[G1,G2,G3]],
    [[R4,R5,R6],[B4,B5,B6],[O4,O5,O6],[G4,G5,G6]],
    [[R7,R8,R9],[B7,B8,B9],[O7,O8,O9],[G7,G8,G9]],
    [[Y1,Y2,Y3]],
    [[Y4,Y5,Y6]],
    [[Y7,Y8,Y9]]],

    [[[W1,W2,O7]],
    [[W4,W5,O4]],
    [[W7,W8,O1]],
    [[R1,R2,W3],[B3,B6,B9],[Y9,O2,O3],[G1,G2,G3]],
    [[R4,R5,W6],[B2,B5,B8],[Y6,O5,O6],[G4,G5,G6]],
    [[R7,R8,W9],[B1,B4,B7],[Y3,O8,O9],[G7,G8,G9]],
    [[Y1,Y2,R3]],
    [[Y4,Y5,R6]],
    [[Y7,Y8,R9]]]).


%tah F (otoceni predni cervene strany doprava)
move([
    [[W1,W2,W3]],
    [[W4,W5,W6]],
    [[W7,W8,W9]],
    [[R1,R2,R3],[B1,B2,B3],[O1,O2,O3],[G1,G2,G3]],
    [[R4,R5,R6],[B4,B5,B6],[O4,O5,O6],[G4,G5,G6]],
    [[R7,R8,R9],[B7,B8,B9],[O7,O8,O9],[G7,G8,G9]],
    [[Y1,Y2,Y3]],
    [[Y4,Y5,Y6]],
    [[Y7,Y8,Y9]]],

    [[[W1,W2,W3]],
    [[W4,W5,W6]],
    [[G9,G6,G3]],
    [[R7,R4,R1],[W7,B2,B3],[O1,O2,O3],[G1,G2,Y1]],
    [[R8,R5,R2],[W8,B5,B6],[O4,O5,O6],[G4,G5,Y2]],
    [[R9,R6,R3],[W9,B8,B9],[O7,O8,O9],[G7,G8,Y3]],
    [[B7,B4,B1]],
    [[Y4,Y5,Y6]],
    [[Y7,Y8,Y9]]]).

%tah FF (otoceni spodni zlute strany doleva)
move([
    [[W1,W2,W3]],
    [[W4,W5,W6]],
    [[W7,W8,W9]],
    [[R1,R2,R3],[B1,B2,B3],[O1,O2,O3],[G1,G2,G3]],
    [[R4,R5,R6],[B4,B5,B6],[O4,O5,O6],[G4,G5,G6]],
    [[R7,R8,R9],[B7,B8,B9],[O7,O8,O9],[G7,G8,G9]],
    [[Y1,Y2,Y3]],
    [[Y4,Y5,Y6]],
    [[Y7,Y8,Y9]]],

    [[[W1,W2,W3]],
    [[W4,W5,W6]],
    [[B1,B4,B7]],
    [[R3,R6,R9],[Y3,B2,B3],[O1,O2,O3],[G1,G2,W9]],
    [[R2,R5,R8],[Y2,B5,B6],[O4,O5,O6],[G4,G5,W8]],
    [[R1,R4,R7],[Y1,B8,B9],[O7,O8,O9],[G7,G8,W7]],
    [[G3,G6,G9]],
    [[Y4,Y5,Y6]],
    [[Y7,Y8,Y9]]]).


%tah B (otoceni zadni oranzove strany doleva)
move([
    [[W1,W2,W3]],
    [[W4,W5,W6]],
    [[W7,W8,W9]],
    [[R1,R2,R3],[B1,B2,B3],[O1,O2,O3],[G1,G2,G3]],
    [[R4,R5,R6],[B4,B5,B6],[O4,O5,O6],[G4,G5,G6]],
    [[R7,R8,R9],[B7,B8,B9],[O7,O8,O9],[G7,G8,G9]],
    [[Y1,Y2,Y3]],
    [[Y4,Y5,Y6]],
    [[Y7,Y8,Y9]]],

    [[[B3,B6,B9]],
    [[W4,W5,W6]],
    [[W7,W8,W9]],
    [[R1,R2,R3],[B1,B2,Y9],[O7,O4,O1],[W3,G2,G3]],
    [[R4,R5,R6],[B4,B5,Y8],[O8,O5,O2],[W2,G5,G6]],
    [[R7,R8,R9],[B7,B8,Y7],[O9,O6,O3],[W1,G8,G9]],
    [[Y1,Y2,Y3]],
    [[Y4,Y5,Y6]],
    [[G1,G4,G7]]]).

%tah BB (otoceni zadni oranzove strany doprava)
move([
    [[W1,W2,W3]],
    [[W4,W5,W6]],
    [[W7,W8,W9]],
    [[R1,R2,R3],[B1,B2,B3],[O1,O2,O3],[G1,G2,G3]],
    [[R4,R5,R6],[B4,B5,B6],[O4,O5,O6],[G4,G5,G6]],
    [[R7,R8,R9],[B7,B8,B9],[O7,O8,O9],[G7,G8,G9]],
    [[Y1,Y2,Y3]],
    [[Y4,Y5,Y6]],
    [[Y7,Y8,Y9]]],

    [[[G7,G4,G1]],
    [[W4,W5,W6]],
    [[W7,W8,W9]],
    [[R1,R2,R3],[B1,B2,W1],[O3,O6,O9],[Y7,G2,G3]],
    [[R4,R5,R6],[B4,B5,W2],[O2,O5,O8],[Y8,G5,G6]],
    [[R7,R8,R9],[B7,B8,W3],[O1,O4,O7],[Y9,G8,G9]],
    [[Y1,Y2,Y3]],
    [[Y4,Y5,Y6]],
    [[B9,B6,B3]]]).

%projde pole ulozenych kostek v poli (od slozene k zadane) a vytiskne jej
%v poli je jako prvni kostka slozena a posledni zadana (proto se tiskne od konce)
outputRes([]).
outputRes([L|LL]):- outputRes(LL),outputCube(L),write('\n').

%vytisknuti dle formatu v zadani
outputCube([
    [W1],
    [W2],
    [W3],
    [R1,B1,O1,G1],
    [R2,B2,O2,G2],
    [R3,B3,O3,G3],
    [Y1],
    [Y2],
    [Y3]]) :- 
    atomic_list_concat(W1, WW1), writeln(WW1),
    atomic_list_concat(W2, WW2), writeln(WW2),
    atomic_list_concat(W3, WW3), writeln(WW3),

    atomic_list_concat(R1, RR1), atomic_list_concat([' '|B1], BB1), atomic_list_concat([' '|O1], OO1), atomic_list_concat([' '|G1], GG1), 
    write(RR1),write(BB1),write(OO1),writeln(GG1),

    atomic_list_concat(R2, RR2), atomic_list_concat([' '|B2], BB2), atomic_list_concat([' '|O2], OO2), atomic_list_concat([' '|G2], GG2), 
    write(RR2),write(BB2),write(OO2),writeln(GG2),

    atomic_list_concat(R3, RR3), atomic_list_concat([' '|B3], BB3), atomic_list_concat([' '|O3], OO3), atomic_list_concat([' '|G3], GG3), 
    write(RR3),write(BB3),write(OO3),writeln(GG3),

    atomic_list_concat(Y1, YY1), writeln(YY1),
    atomic_list_concat(Y2, YY2), writeln(YY2),
    atomic_list_concat(Y3, YY3), writeln(YY3).
    

%Nema cenu hledat tahy dale nez 20 tahu, jelikoz jakkoliv zamichana kostka ma reseni do 20 tahu (tzn gods number) viz http://www.cube20.org/
%postupne zanorovani, aby cesta k nalezene kostce byla nejkratsi
solve(Cube,Res,Depth) :- Depth =< 20, find(Cube,[],Res,0,Depth).
solve(Cube,Res,Depth) :- DDepth is Depth+1, solve(Cube,Res,DDepth).
    

%Zkusi se udelat pohyb, a pokud uz neni v ceste od zadane kostky po tuto kostku, tak se prida do seznamu a pokracuje se dalsim tahem
find(K,Arr,[K|Arr],_,_) :- final(K),!.
find(K,Arr,R,Len,MaxLen) :- Len =< MaxLen, move(K,Y), \+ member(Y,Arr), LLen is Len +1, find(Y,[K|Arr],R,LLen,MaxLen),!.


start :-
    prompt(_, ''),
    read_lines(LL),
    split_lines(LL,Cube),
    !,%pro pripad ze by solve nenaslo reseni
    solve(Cube,Res,0),
    outputRes(Res),%vypis reseni
    halt.
