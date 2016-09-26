use "PartA/Assignments/03_Week.sml";
use "PartA/Assignments/Helpers.sml";

test_string_option_list(all_except_option("string", ["string"]),        SOME []);
test_string_option_list(all_except_option("d", ["a", "b", "c"]),        NONE);
test_string_option_list(all_except_option("Fred", ["Fred","Fredrick"]), SOME["Fredrick"]);

test_string_list(get_substitutions1([["a","b","c"]], "a"), ["b","c"]);
test_string_list(get_substitutions1([["a","b","c"]], "d"), []);
test_string_list(get_substitutions1([["a","b"],["c","d"],["e","a","f"]], "a"), ["b","e","f"]);
