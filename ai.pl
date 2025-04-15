% Author: Steven Libby, Joshua Krasnogorov
% Homework 10 and 11: Scrabble AI

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


% Base case: Found a word in the trie
make_word(_Tiles, Part, Word) :-
    atom_chars(Part, Word),
    is_word(Word).

% Recusive case
make_word(Tiles, Part, Word) :-
    select(Tile, Tiles, RestTiles),      % select arbitrary letter
    trie(Part, Tile, NewPart),           % move on with that letter in the trie
    make_word(RestTiles, NewPart, Word). % make the rest of the word

   
% Pattern: highlight Letter in its position
pattern([Letter|Rest], Letter, [Letter|RestPattern]) :-
    replace_with_spaces(Rest, RestPattern).

pattern([_Head|Rest], Letter, [' '|RestPattern]) :-
    pattern(Rest, Letter, RestPattern).

% Helper: replace all elements with spaces
replace_with_spaces([], []).
replace_with_spaces([_|Rest], [' '|RestPattern]) :-
    replace_with_spaces(Rest, RestPattern).

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

% Dir == right case
place_word(R, C, right, Word, Pattern, Board, NewBoard) :-
    append(BeforeRows, [Row|AfterRows], Board), % Find a Row in the Board
    length(BeforeRows, R),                      % Make sure Row is at index R
    append(BeforeCols, RestOfRow, Row),         % Find a starting position in the Row
    length(BeforeCols, C),                      % Make sure Column is at index C
    append(Pattern, AfterCols, RestOfRow),      % Match the Pattern and get suffix
    append(BeforeCols, Word, TempRow),          % Construct the start of the new row
    append(TempRow, AfterCols, NewRow),         % Construct the full new row
    append(BeforeRows, [NewRow|AfterRows], NewBoard), % Construct the new board
    valid_board(NewBoard).                      % Check if the resulting board is valid

% Dir == down case: Transpose, place right, transpose back.
place_word(R, C, down, Word, Pattern, Board, NewBoard) :-
    transpose(Board, TBoard),
    % Transposed board has swapped Rows and Columns
    place_word(C, R, right, Word, Pattern, TBoard, TempNewTBoard),
    transpose(TempNewTBoard, NewBoard).

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

