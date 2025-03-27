open OUnit2
open Final_project

let requests_tests = "http test requests interface" >::: []
let _ = run_test_tt_main requests_tests
