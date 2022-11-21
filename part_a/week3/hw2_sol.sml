(* Dan Grossman, Coursera PL, HW2 Provided Code *)
(* Toby Burns, Extended to full solution*)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* a tail recursive, order is weird but not specified in question *)
fun all_except_option (x, ys) =
   let
      fun all_except (ys, ans) =
         case ys 
         of [] => NONE
         | y::ys' => if same_string (x, y) 
                     then SOME (ans @ ys')
                     else all_except (ys', y::ans)
   in
      all_except(ys, [])
   end

(* b *)
fun get_substitutions1 (xs, y) =
   case xs
   of [] => []
   | x::xs' => case (all_except_option(y, x)) 
               of SOME z => z @ get_substitutions1(xs', y)
               | NONE => get_substitutions1(xs', y)


(* c *)
fun get_substitutions2 (xs, y) =
   let
      fun tail_helper (xs, ans) =
         case xs
         of [] => ans
         | x::xs' => case (all_except_option(y, x)) 
                     of SOME z => tail_helper(xs', ans @ z)
                     | NONE => tail_helper(xs', ans)
   in
      tail_helper(xs, [])
   end

(* d append to keep order but copies list each time even though it's tail recursive *)
(* fun similar_names (substitutions, fullName) = 
   let
      val {first=firstName, middle=middleName, last=lastName} = fullName
      fun build_answer(validFirstNames, ans) =
         case validFirstNames
         of [] => ans
         | x::xs => build_answer(xs, ans @ [{first=x, middle=middleName, last=lastName}])
   in
      build_answer(firstName :: get_substitutions2(substitutions, firstName), [])
   end *)

(* d non tail recursive but keeps order using cons *)
(* fun similar_names (substitutions, fullName) = 
   let
      val {first=firstName, middle=middleName, last=lastName} = fullName
      fun build_answer(validFirstNames) =
         case validFirstNames
         of [] => []
         | x::xs => {first=x, middle=middleName, last=lastName} :: build_answer(xs)
   in
      build_answer(firstName :: get_substitutions2(substitutions, firstName))
   end *)

fun rev toReverse = 
   let
      fun rev_tail (toReverse, reverse) =
         case toReverse
         of [] => reverse
         | head::rest => rev_tail (rest, head::reverse)
   in
      rev_tail (toReverse, [])
   end

(* d tail recursive using cons and just reverse at the end *)
fun similar_names (substitutions, fullName) = 
   let
      val {first=firstName, middle=middleName, last=lastName} = fullName

      fun build_answer(validFirstNames, ans) =
         case validFirstNames
         of [] => rev ans
         | x::xs => build_answer(xs, {first=x, middle=middleName, last=lastName}::ans)
   in
      build_answer(firstName::get_substitutions2(substitutions, firstName), [])
   end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* a *)
fun card_color thisCard =
   case thisCard
   of (Clubs, _) => Black
   | (Spades, _) => Black
   | (Diamonds, _) => Red
   | (Hearts, _) => Red

(* b *)
fun card_value thisCard =
   case thisCard
   of (_, Num x) => x
   | (_, Jack) => 10
   | (_, Queen) => 10
   | (_, King) => 10
   | (_, Ace) => 11

(* c order is weird but not specified in question *)
fun remove_card (cs, c, e) =
   let
      fun all_except_c (cs, ans) =
         case cs 
         of [] => raise e
         | frontCard::cs' => if frontCard = c
                     then ans @ cs'
                     else all_except_c (cs', frontCard::ans)
   in
      all_except_c(cs, [])
   end

(* d *)
fun all_same_color cards = 
   case cards
   of [] => true
   | c::[] => true
   | c1::(c2::cs') => (card_color(c1) = card_color(c2)) andalso all_same_color(c2::cs')

(* e *)
fun sum_cards cards =
   let
      fun sum_using_acc (cs, ans) =
         case cs
         of [] => ans
         | c::cs' => sum_using_acc(cs', ans + card_value(c))
   in
      sum_using_acc(cards, 0)
   end

(* f *)
fun score (hand, goal) =
   let
      val sumHand = sum_cards hand
      val prelim = if sumHand > goal
                     then 3 * (sumHand - goal)
                     else goal - sumHand
   in
      if all_same_color hand
      then prelim div 2
      else prelim
   end

(* f with case, think I prefer the other way *)
(* fun score(hand, goal) =
   let
      val sumHand = sum_cards hand
   in
      case (sumHand > goal, all_same_color hand)
      of (true, true) => (3 * (sumHand - goal)) div 2
      | (true, false) => (3 * (sumHand - goal))
      | (false, true) => (goal - sumHand) div 2
      | (false, false) => (goal - sumHand)
   end *)

(* g *)
fun officiate (cards, moves, goal) =
   let
      fun traverse_moves (cards, moves, hand) =
         case (cards, moves, goal < sum_cards hand)
         of (_, _, true) => hand
         | (_, [], _) => hand
         | ([], Draw::_, _) => hand
         | (cs, (Discard c)::moves, _) => traverse_moves(cs, moves, remove_card(hand, c, IllegalMove))
         | (c::cs, Draw::moves, _) => traverse_moves(cs, moves, c::hand)
   in
      score(traverse_moves(cards, moves, []), goal)
   end

(* 13 *)
(* a *)

fun count_rank (cs, rankToFind) = 
   let
      fun count_rank_tail (cs, ans) =
         case cs 
         of [] => ans
         | (curSuit, curRank)::cs' => if rankToFind = curRank 
                                       then count_rank_tail (cs', 1 + ans)
                                       else count_rank_tail (cs', ans)
   in
      count_rank_tail(cs, 0)
   end

(* I feel like this isn't the nicest way of doing this, but I didn't want to spend too much time *)
fun score_challenge (hand, goal) =
   let
      fun get_best_sum_hand (sumHand, aces) =
      (* Keeps checking down by 10 until within 2 of the goal, this is the crossover point of the score being worse by removing 10 more *)
         case (aces, sumHand < goal + 2)
         of (0, _) => sumHand
         | (_, true) => sumHand
         | (_, false) => get_best_sum_hand(sumHand - 10, aces - 1)
      val aces = count_rank (hand, Ace)
      val sumHand = get_best_sum_hand (sum_cards hand, aces)
      val prelim = if sumHand > goal
            then 3 * (sumHand - goal)
            else goal - sumHand
   in
      if all_same_color hand
      then prelim div 2
      else prelim
   end

fun officiate_challenge (cards, moves, goal) =
   let
      fun traverse_moves (cards, moves, hand) =
         (* I could make a sum_cards for ace low but I went for speed of coding not niceness of code *)
         case (cards, moves, goal < sum_cards hand - (count_rank (hand, Ace) * 10))
         of (_, _, true) => hand
         | (_, [], _) => hand
         | ([], Draw::_, _) => hand
         | (cs, (Discard c)::moves, _) => traverse_moves(cs, moves, remove_card(hand, c, IllegalMove))
         | (c::cs, Draw::moves, _) => traverse_moves(cs, moves, c::hand)
   in
      score_challenge (traverse_moves(cards, moves, []), goal)
   end


(* b *)
exception DoesNotExist
fun get_card_of_value (cs, valueToFind) =
         case cs 
         of [] => raise DoesNotExist
         | c::cs' => if (card_value c) = valueToFind
            then c
            else get_card_of_value (cs', valueToFind)

fun careful_player (cards, goal) =
   let 
      fun generate_moves (cards, moves, hand, curGoal) =
         case (cards, 10 < curGoal, curGoal = 0)
         of (_, _, true) => rev moves
         | ([], true, _) => rev (Draw::moves)
         | ([], false, _) => rev moves
         | (c::cs, true, _) => generate_moves (cs, Draw::moves, c::hand, curGoal - card_value c)
         (* This is a little weird, I could have done it better *)
         | (c::_, false, _) => if curGoal < card_value c
                                 then 
                                    case (get_card_of_value (hand, card_value c - curGoal) handle DoesNotExist => (Hearts, Num 0))
                                    of (_, Num 0) => rev moves
                                    | oldCard => rev (Draw::((Discard oldCard)::moves))
                                 else rev moves
   in
      generate_moves (cards, [], [], goal)
   end