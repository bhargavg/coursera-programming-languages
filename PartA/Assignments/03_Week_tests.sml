use "PartA/Assignments/03_Week.sml";
use "PartA/Assignments/Helpers.sml";

test_string_option_list(all_except_option("string", ["string"]),        SOME []);
test_string_option_list(all_except_option("d", ["a", "b", "c"]),        NONE);
test_string_option_list(all_except_option("Fred", ["Fred","Fredrick"]), SOME["Fredrick"]);

test_string_list(get_substitutions1([["a","b","c"]], "a"), ["b","c"]);
test_string_list(get_substitutions1([["a","b","c"]], "d"), []);
test_string_list(get_substitutions1([["a","b"],["c","d"],["e","a","f"]], "a"), ["b","e","f"]);

test_string_list(get_substitutions2([["a","b","c"]], "a"), ["b","c"]);
test_string_list(get_substitutions2([["a","b","c"]], "d"), []);
test_string_list(get_substitutions2([["a","b"],["c","d"],["e","a","f"]], "a"), ["b","e","f"]);


test_first_middle_last_string_records(similar_names([["Fred","Fredrick"],
                                                     ["Elizabeth","Betty"],
                                                     ["Freddie","Fred","F"]], 
                                                    {first="Fred", middle="W", last="Smith"}),

                                                    [{first="Fred", last="Smith", middle="W"}, 
                                                     {first="Fredrick", last="Smith", middle="W"}, 
                                                     {first="Freddie", last="Smith", middle="W"}, 
                                                     {first="F", last="Smith", middle="W"}]);


val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = [];

val test2d1 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true;
val test2d2 = all_same_color [(Hearts, Ace), (Diamonds, Num 5)] = true;
val test2d3 = all_same_color [(Hearts, King), (Hearts, Jack)] = true;
val test2d4 = all_same_color [(Clubs, Queen), (Spades, Num 10)] = true;

val test2f1 = score([(Hearts, Num 2),(Diamonds, Num 4)],10) = 4; 

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4;

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw], 42) = 3;

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)], 42); false) handle
                         IllegalMove => true);
