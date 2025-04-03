:- [scrabble].
:- [trie].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Assignment 10
% Due: 4/7
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This is a test predicate to check if guess is working.
% guess(Tiles,Word,Pattern) is true if
% we can pick an Extra tile, and we can make Word out of the Tiles + our Extra
% and Pattern is the pattern of the Word with the Extra tile.
%
% example:
% guess([a,n,m,d],[s,a,n,d],[s,' ',' ',' ']).
% guess([a,n,m,d],[d,a,m,p],[' ',' ',' ',p]).
%
% You can find all of the possible words and patterns using
% guess([a,n,m,d],W,P).
guess(Tiles,Word,Pattern) 
   :- make_word([Extra|Tiles],e0,Word), 
      Extra\=e0, 
      pattern(Word,Extra,Pattern).




% Base case: if Part is a valid word, return it as Word
make_word(_, Part, Part) :-
    Part \= e0,
    is_word(Part).

% Start building the word from e0
make_word(Tiles, e0, Word) :-
    make_word(Tiles, [], Word).

% Recursive case: select a tile, append it to Part, and recurse
make_word(Tiles, Part, Word) :-
    select(R, Tiles, RemainingTiles),
    append(Part, [R], NewPart),
    make_word(RemainingTiles, NewPart, Word).

% Pattern: highlight Letter in its position
pattern([Letter | Rest], Letter, [Letter | RestPattern]) :-
    replace_with_spaces(Rest, RestPattern).

pattern([Head | Rest], Letter, [' ' | RestPattern]) :-
    pattern(Rest, Letter, RestPattern).

% Helper: replace all elements with spaces
replace_with_spaces([], []).
replace_with_spaces([_ | Rest], [' ' | RestPattern]) :-
    replace_with_spaces(Rest, RestPattern).














% % Base case: if Part is a valid word, return it as Word
% make_word(_, Part, Part) :-
%    Part \= e0,
%    is_word(Part).

% make_word(Tiles, e0, Word) :-
%    make_word(Tiles, [], Word).

% % Recursive case: select a tile, append it to Part, and recurse
% make_word(Tiles, Part, Word) :-
%    Part \= e0,
%    select(R, Tiles, RemainingTiles),
%    append(Part, [R], NewPart),
%    make_word(RemainingTiles, NewPart, Word).

% % base case
% % make_word(Tiles, Part, Word) :- 
% %    Part \= e0,
% %    is_word(Part).

% % make_word(Tiles, e0, Word) :-
% %    Tiles = [Fst|Rest],
% %    select(R, Tiles, RemainingTiles),
% %    make_word(RemainingTiles, [R], Word),
% %    make_word(Rest, [Fst], Word).

% % % recursive case
% % make_word(Tiles, Part, Word) :-
% %    Tiles = [Fst|Rest],
% %    select(R, Tiles, RemainingTiles),
% %    make_word(RemainingTiles, [Part|R], Word),
% %    make_word(Rest, [Part|Fst], Word).


% % main predicate
% pattern(Tiles, Letter, Pattern) :-
%    member(Letter, Tiles),
%    member(Letter, Pattern)

% % base case
% pattern([], _, []).

% pattern([Letter | Rest], Letter, Pattern) :-
%     Pattern = [Letter | RestPattern],
%     replace_with_spaces(Rest, RestPattern).

% pattern([Head | Rest], Letter, Pattern) :-
%     Pattern = [' ' | RestPattern],
%     pattern(Rest, Letter, RestPattern).

% replace_with_spaces([], []).
% replace_with_spaces([_ | Rest], [' ' | RestPattern]) :-
%     replace_with_spaces(Rest, RestPattern).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Assignment 11
% Due: 4/14
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This is the full guess predicate.
% This is the same as before, but this time we also have
% a Board
% a Row, Col, Dir for the guess,
% and the NewBoard which is the result of putting Word at position Row,Col,Dir on Board
guess(Tiles,Board,Row,Col,Dir,Word,NewBoard) 
  :- make_word([X|Tiles],e0,Word),
     X \= e0,
     pattern(Word,X,Pattern),
     place_word(Row,Col,Dir,Word,Pattern,Board,NewBoard).

place_word(R,C,Dir,Tiles,Pattern,Board,NewBoard) :- fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This comes from Prologs clpfd library.
% I didn't want you to have to use an external library for this one predicate.
%
% transpose(M,T) is true if M is a matrix, and T is the transpose of M.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transpose([], []).
transpose([F|Fs], Ts) :- transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :- lists_firsts_rests(Ms, Ts, Ms1),
                                   transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Predicates for finding possible guesses.
% In order for these to work, you need guess working.
%
% run(Tiles,Board) will print out every possible position to put tiles on the
% board to form a word.
%
% runVerbose(Tiles,Board) does the same thing, but it prints out the board
% after putting the tiles on it.
% This is a lot more output, but you can actually see what happens.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run(Tiles,Board) :- findall([R,C,D,W], guess(Tiles,Board,R,C,D,W,_), Guesses),
                    writeBoard(Board),
                    write("Options:"), nl,
                    writeOptions(Guesses).
writeOptions([]).
writeOptions([[Row,Col,Dir,Word]|Guesses]) 
  :- pos(R,Row), pos(C, Col), dir(Dir,D),
     atom_chars(W,Word),
     write(R), write(C), write(D), write(' '), write(W), nl,
     writeOptions(Guesses).

runVerbose(Tiles,Board) :- findall([R,C,D,W,B], guess(Tiles,Board,R,C,D,W,B), Guesses),
                           write("Options:"), nl,
                           writeVerbose(Guesses).

writeVerbose([]).
writeVerbose([[Row,Col,Dir,Word,Board]|Guesses]) 
  :- pos(R,Row), pos(C, Col), dir(Dir,D),
     atom_chars(W,Word),
     write(R), write(C), write(D), write(' '), write(W), nl,
     writeBoard(Board), nl,
     writeVerbose(Guesses).

dir(right,'>').
dir(down,'V').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% valid_board(Board) is true if all of the words on Board are valid words.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

valid_board(Board) :- transpose(Board,BoardT),
                      append(Board,BoardT,BigBoard),
                      maplist(words,BigBoard,WSS),
                      append(WSS,WS),
                      maplist(is_word,WS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Split a list into a list of words seperated by spaces
%
% words(CS,WS) is true if concatenating WS with spaces gives you CS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

words([],[]).
words([C|CS],[W|WS]) :- C \= ' ', oneword([C|CS],W,Rest), words(Rest,WS).
words([' '|CS],WS) :- words(CS,WS).

oneword([],[],[]).
oneword([C|CS],[C|W],Rest) :- C \= ' ', oneword(CS,W,Rest).
oneword([' '|CS],[],[' '|CS]).

