open Editor
open OUnit2

let make_test name expected_output input = 
  name >:: (fun _ -> 
      assert_equal expected_output input)

let tests = [
  make_test "simple" "" (doc_to_str []);
  make_test "abc" "a" (doc_to_str [{text = 'a'};]);
]

let suite = "suite" >::: tests

let () = run_test_tt_main suite