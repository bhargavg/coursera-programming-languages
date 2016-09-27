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


val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
