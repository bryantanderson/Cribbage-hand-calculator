% This program calculates the score of a hand based on the rules of Cribbage,
% and chooses the most optimal hand from a list of cards dealt to a player.

% -----------------------------------------------------------------------------

% The program calculates the score of a hand by assessing it against each 
% criteria from the Cribbage rules, and sums the scores up. To choose the 
% most optimal hand, it generates every possible hand, and calculates the 
% expected value of each hand by taking the average value over all possible 
% start cards. The hand with the highest expected value is chosen.

% -----------------------------------------------------------------------------

% Calculates the scores for each criteria based on the hand, and sums them up.

hand_value(Hand, card(StartRank, StartSuit), Value) :-
    fifteen_score([card(StartRank, StartSuit)|Hand], FifteenScore),
    convert_values([card(StartRank, StartSuit)|Hand], HandWithStart),
    pairs_score(HandWithStart, PairScore),
    run_score(HandWithStart, RunScore),
    flushes_score(Hand, StartSuit, FlushScore),
    nob_score(Hand, StartSuit, NobScore),
    Value is FifteenScore + PairScore + RunScore + FlushScore + NobScore.

% Recurses through the given hand, and for each card in the list "Hand" (ground)
% and converts the card's rank into an integer based on its order in a run. The 
% the result is returned in a separate list "Converted"

convert_values(Hand, Converted) :-
    convert_values(Hand, [], Converted).
convert_values([], Converted, Converted).
convert_values([card(Rank, Suit)|Hand], Acc, Converted) :-
    value(Rank, Value),
    convert_values(Hand, [card(Value, Suit)|Acc], Converted).

% Extracts the ranks of each card in the list "Hand"(ground), and returns it in
% a separate list "Extracted".

extract_ranks(Hand, Extracted) :-
    extract_ranks(Hand, [], Extracted).
extract_ranks([], Extracted, Extracted).
extract_ranks([card(Rank, _)|Rest], Acc, Extracted) :-
    original_value(Rank, Value),
    extract_ranks(Rest, [Value|Acc], Extracted).

% Takes in a list of cards "OriginalHand"(ground), and first extracts
% the ranks of each card in the list. It computes all possible combinations
% of sublists that have a sum of 15, and calculates the score received 
% from the number of sublists found. 

fifteen_score(OriginalHand, Score) :-
    extract_ranks(OriginalHand, Hand),
    findall(Combination, fifteen_combination(Hand, Combination), Combinations),
    length(Combinations, Count),
    Score is 2 * Count.

% Computes a sublist from the list "Hand"(ground) with a total sum of 15. 
% The sublist is binded to "Combination". 

fifteen_combination(Hand, Combination) :-
    sublist(Combination, Hand),
    sum_list(Combination, 15).
    
% Predicate from Assignment 1, used to generate all sublists.

sublist([], _).
sublist([X|Xs], [X|Ys]) :-
    sublist(Xs, Ys).
sublist(X, [_|Ys]) :-
    X = [_|_],
    sublist(X, Ys).

% Given a list "Hand"(ground), finds all pairs of cards present in "Hand". It 
% then calculates the score received from the number of pairs found.

pairs_score(Hand, Score) :-
    findall(2, (select(Card, Hand, Rest), has_pair(Card, Rest)), Pairs),
    length(Pairs, Score).

% Given a card (ground), this predicate recurses through a given list until a 
% card of the same rank is found, or until the list is empty.

has_pair(card(Rank, _), [card(Rank, _)|_]).
has_pair(card(Rank, _), [_|Hand]) :-
    has_pair(card(Rank, _), Hand).

% Given a list "Hand"(ground), calculates the score received from the 
% number of runs in "Hand". It does this by first sorting "Hand", and using a 
% helper predicate check_run.

run_score(Hand, Score) :-
    msort(Hand, [card(R, _)|SortedHand]),
    check_run(SortedHand, R, 1, 1, 0, Score).
     
% The base case for the check_run predicate. Any run that is not at least 3 
% cards long is invalid.

check_run([], _, Dup, _, Longest, Score) :-
    (  Longest >= 3
    -> Score is Dup * Longest
    ;  Score is 0
    ).

% The recursive case for the check_run predicate. "Rank" represents the rank of
% the current card. "PrevRank" represents the rank of the previous card in the
% list. "Dup" represents the number of duplicate numbers we have in the run.
% "Consecutive" represents the length of the current run. "Longest" represents
% the length of the longest run that we have so far. This predicate works by 
% increasing "Consecutive" whenever it finds a number in correct order, and 
% when it finds a number in wrong order, it resets "Consecutive", and sets 
% "Longest" to be the maximum between "Consecutive" and "Longest".

check_run([card(Rank, _)|Hand], PrevRank, Dup, Consecutive, Longest, Score) :-
    % Double the value of "Dup" whenever we find a duplicate number in the run.
    % The number of possible runs doubles whenever a duplicate number is
    % encountered in the run.
    (  Rank =:= PrevRank
    -> Dup1 is Dup * 2,
       check_run(Hand, Rank, Dup1, Consecutive, Longest, Score)
    ;  Rank =:= PrevRank + 1
    -> Consecutive1 is Consecutive + 1,
       Longest1 is max(Consecutive1, Longest),
       check_run(Hand, Rank, Dup, Consecutive1, Longest1, Score)
    % A rank is in incorrect order.
    ;  check_run(Hand, Rank, Dup, 1, max(Consecutive, Longest), Score)
    ).
    
% Calculates the total score from flushes. A helper predicate check_flush is
% used to determine whether the given list is a valid flush. If the flush is
% valid, it checks whether the starting suit "SS" is the same as the suits
% in the hand. All terms are ground besides "Score".

flushes_score([card(_, Suit)|Hand], SS, Score) :-
    (  check_flush([card(_, Suit)|Hand], Suit)
    -> (  Suit == SS
       -> Score = 5
       ;  Score = 4
       )
    ; Score = 0
    ).

% Given a list of cards, this predicate checks whether all the cards have the
% same suit. All terms are ground.

check_flush([], _).
check_flush([card(_, Suit)|Hand], Suit) :-
    check_flush(Hand, Suit).
    
% Calculates the score of "Hand"(ground) for the "one for his nob" criteria.
% "SS"(ground) represents the suit of the start card. 

nob_score(Hand, SS, Score) :-
    (  check_nob(Hand, SS)
    -> Score is 1
    ;  Score is 0
    ).
    
% Given a list of cards (ground), this predicate checks whether there is a card 
% that has the same suit as the starting suit "SS"(ground), except with the 
% rank of Jack.

check_nob([card(jack, SS)|_], SS).
check_nob([_|Hand], SS) :-
    check_nob(Hand, SS).
    
% Given a list of cards "Cards", this predicate chooses the hand with the 
% greatest expected value. 

select_hand(Cards, Hand, Cribcards) :-
    findall(Avg-H, hand_expected_value(Cards, H, Avg), Pairs),
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, Hands),
    last(Hands, Hand),
    subtract(Cards, Hand, Cribcards).

% Given two lists of cards "Cards" and "Hand", this predicate calculates the
% expected value of "Hand". All terms except "Avg" are ground.

hand_expected_value(Cards, Hand, Avg) :-
    length(Hand, 4),
    length(Cards, Length),
    sublist(Hand, Cards),
    % "Max" represents the number of cards that we need to consider as start
    % cards, excluding all the cards from "Cards".
    Max is 52 - Length,
    sum_over_all_starts(Cards, Hand, ace, clubs, 1, 0, Max, Avg).

% The base case for the "sum_over_all_starts" predicate. It calculates
% the expected value "Avg" by dividing the total value of the hand over all 
% possible start cards "V", by the number of possible start cards "Max".

sum_over_all_starts(_, _, _, _, Max, V, Max, Avg) :-
    Avg is V / Max.

% The recursive case for the "sum_over_all_starts" predicate. "Rank" and "Suit"
% represent the rank and suit of the current start card being evaluated. "N" is
% the number of start cards that have been tried, and "V" is the total value 
% of the hand over all the past start cards. The predicate exhaustively goes 
% through every possible start card, summing up the "Value"s it obtains using
% the helper predicate "hand_value". All terms except "Avg" are ground.

sum_over_all_starts(Cards, Hand, Rank, Suit, N, V, Max, Avg) :-
    StartCard = card(Rank, Suit),
    increment_card(Rank, Rank1, Suit, Suit1), 
    (  \+ member(StartCard, Cards)
    -> hand_value(Hand, StartCard, Value),
       V1 is V + Value,
       N1 is N + 1,
       sum_over_all_starts(Cards, Hand, Rank1, Suit1, N1, V1, Max, Avg)
    ;  sum_over_all_starts(Cards, Hand, Rank1, Suit1, N, V, Max, Avg)
    ). 

% Given the "Rank"(ground) and "Suit"(ground) of a card, this predicate 
% calculates the next card in a predetermined order, and returns the rank and 
% suit of this subsequent card in "Rank1" and "Suit1", respectively. 

increment_card(Rank, Rank1, Suit, Suit1) :-
    value(Rank, ValueFromRank),
    (  ValueFromRank =:= 13
    -> increment_rank(Rank, Rank1),
       increment_suit(Suit, Suit1)
    ;  increment_rank(Rank, Rank1),
       Suit1 = Suit
    ).

% These predicates represents the predetermined order of cards that is used in 
% the calculation of the expected value of a hand. In each of the below
% predicates, the right argument is what comes after the left argument in this
% predetermined order.

increment_suit(clubs, diamonds).
increment_suit(diamonds, hearts).
increment_suit(hearts, spades).
increment_rank(ace, 2).
increment_rank(2, 3).
increment_rank(3, 4).
increment_rank(4, 5).
increment_rank(5, 6).
increment_rank(6, 7).
increment_rank(7, 8).
increment_rank(8, 9).
increment_rank(9, 10).
increment_rank(10, jack).
increment_rank(jack, queen).
increment_rank(queen, king).
increment_rank(king, ace).

% This predicate represents the order a given rank would have if it was to be
% used in a run of cards.
value(2, 2).
value(3, 3).
value(4, 4).
value(5, 5).
value(6, 6).
value(7, 7).
value(8, 8).
value(9, 9).
value(10, 10).
value(jack, 11).
value(queen, 12).
value(king, 13).
value(ace, 1).
% This predicate represents the original "value" of each rank based on the
% actual Cribbage game.
original_value(2, 2).
original_value(3, 3).
original_value(4, 4).
original_value(5, 5).
original_value(6, 6).
original_value(7, 7).
original_value(8, 8).
original_value(9, 9).
original_value(10, 10).
original_value(jack, 10).
original_value(queen, 10).
original_value(king, 10).
original_value(ace, 1).