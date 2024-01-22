(** This file should be used to write your tests of your other code. *)

open Batteries;;

open OUnit2;;

open TestUtils;;

let all_tests =
  [
    test_success "test_code/4.bird" "4";
    test_success "test_code/after_4.bird" "5";
    test_success "test_code/arithmetic.bird" "-9";
    test_success "test_code/before_4.bird" "3";
    test_success "test_code/before_after_4.bird" "4";
    test_success "test_code/declare.bird" "2";
    test_success "test_code/declare_easy.bird" "30";
    test_success "test_code/declare_medium.bird" "5";
    test_success "test_code/declare_negatives.bird" "-14";
    test_success "test_code/minus1.bird" "3";
    test_success "test_code/minus2.bird" "-2";
    test_success "test_code/minus3.bird" "2";
    test_success "test_code/plus1.bird" "2";
    test_success "test_code/plus2.bird" "6";
    test_success "test_code/plus3.bird" "6";
    test_success "test_code/times1.bird" "8";
    test_success "test_code/times2.bird" "15";
    test_success "test_code/lab_arithmetic.bird" "14";
    test_success "test_code/declare_plus.bird" "-4";
    test_success "test_code/declare_times.bird" "-16";
    test_success "test_code/declare_lecture.bird" "5";
    test_success "test_code/true.bird" "true";
    test_success "test_code/false.bird" "false";
    test_success "test_code/isbool_1.bird" "true";
    test_success "test_code/isbool_2.bird" "true";
    test_success "test_code/isbool_3.bird" "false";
    test_success "test_code/isint_1.bird" "true";
    test_success "test_code/isint_2.bird" "true";
    test_success "test_code/isint_3.bird" "false";
    test_success "test_code/isint_4.bird" "false";
    test_success "test_code/and_1.bird" "false";
    test_success "test_code/and_2.bird" "true";
    test_success "test_code/or_1.bird" "true";
    test_success "test_code/or_2.bird" "true";
    test_success "test_code/or_3.bird" "false";
    test_success "test_code/less_1.bird" "true";
    test_success "test_code/less_2.bird" "true";
    test_success "test_code/less_3.bird" "false";
    test_success "test_code/less_4.bird" "false";
    test_success "test_code/greater_1.bird" "true";
    test_success "test_code/greater_2.bird" "true";
    test_success "test_code/greater_3.bird" "false";
    test_success "test_code/greater_4.bird" "false";
    test_success "test_code/if_1.bird" "1";
    test_success "test_code/if_2.bird" "true";
    test_success "test_code/if_3.bird" "true";
    test_success "test_code/print_1.bird" "1\n1";
    test_success "test_code/print_2.bird" "4\nfalse\nfalse";
    test_success "test_code/stack_mem.bird" "3";
    test_runtime_failure "test_code/fail_int_1.bird" 1;
    test_runtime_failure "test_code/fail_int_2.bird" 1;
    test_runtime_failure "test_code/fail_int_3.bird" 1;
    test_runtime_failure "test_code/pass_int_1.bird" 0;
    test_runtime_failure "test_code/pass_int_2.bird" 0;
    test_runtime_failure "test_code/fail_bool_1.bird" 2;
    test_runtime_failure "test_code/fail_bool_2.bird" 2;
    test_runtime_failure "test_code/fail_bool_3.bird" 1;
    test_runtime_failure "test_code/pass_bool_1.bird" 0;
    test_runtime_failure "test_code/pass_bool_2.bird" 0;
    test_runtime_failure "test_code/fail_bool_4.bird" 2;
    test_runtime_failure "test_code/fail_bool_5.bird" 2;
    test_success "test_code/def_1.bird" "1";
     test_success "test_code/def_2.bird" "true\ntrue";
    
    (*test_success "test_code/def_3.bird" "15";
    test_success "test_code/def_4.bird" "1";
    test_success "test_code/def_5.bird" "true"; *)
    test_compile_failure "test_code/compile_1.bird" "Unbound variable y.";
    test_compile_failure "test_code/compile_2.bird" "Unbound variable g.";

    test_compile_failure "test_code/compile_4.bird" "Duplicate definition of function f.";
    test_compile_failure "test_code/compile_6.bird" "Function f declares a duplicate parameter x.";
    test_compile_failure "test_code/compile_7.bird" "Unbound variable g.\nUnbound variable y.\nUnbound variable z.";
    test_compile_failure "test_code/compile_8.bird" "Unbound variable x.\nUnbound variable y.";

    test_compile_failure "test_code/compile_11.bird" "Unbound variable g."; 
    test_success "test_code/is_tuple_1.bird" "true";
    test_success "test_code/is_tuple_2.bird" "false";
    test_success "test_code/is_tuple_3.bird" "true";
    test_success "test_code/is_tuple_4.bird" "true";
    test_success "test_code/is_tuple_5.bird" "true"; 
    test_success "test_code/is_tuple_6.bird" "false";
    test_success "test_code/is_tuple_7.bird" "false";
    test_success "test_code/is_tuple_8.bird" "1";
    test_success "test_code/tindex_1.bird" "1";
    test_success "test_code/tindex_2.bird" "3";
    test_success "test_code/tindex_3.bird" "2";
    (* test_success "test_code/tindex_4.bird" "true"; *)
    test_runtime_failure "test_code/fail_tuple1.bird" 4;
    test_runtime_failure "test_code/fail_tuple2.bird" 4;
    test_runtime_failure "test_code/fail_tuple3.bird" 1;
    test_runtime_failure "test_code/fail_tuple4.bird" 3;
    test_runtime_failure "test_code/fail_tuple5.bird" 1;
    test_runtime_failure "test_code/fail_tuple6.bird" 4;
    test_runtime_failure "test_code/fail_tuple7.bird" 3;
    test_runtime_failure "test_code/fail_tuple8.bird" 3;
    test_runtime_failure "test_code/fail_tuple9.bird" 3;
    test_runtime_failure "test_code/fail_tuple10.bird" 4;
    test_success "test_code/tindex_5.bird" "4";
    test_runtime_failure "test_code/fail_tuple11.bird" 3;
    test_runtime_failure "test_code/fail_tuple12.bird" 4;
    test_success "test_code/falcon_3.bird" "1";
    test_success "test_code/falcon_4.bird" "28";
    test_success "test_code/falcon_5.bird" "18";
    test_success "test_code/falcon_6.bird" "true";
    test_success "test_code/falcon_7.bird" "false";
    test_success "test_code/falcon_8.bird" "true";
    test_runtime_failure "test_code/falcon_10.bird" 3;
    test_success "test_code/falcon_11.bird" "7";
    test_success "test_code/gull_1.bird" "12";
    test_success "test_code/gull_3.bird" "12";
    test_success "test_code/hoopoe_1.bird" "1";
    test_success "test_code/hoopoe_2.bird" "9";
    test_success "test_code/hoopoe_4.bird" "12";
    
    test_success "test_code/gull_4.bird" "(1, 2)\n(1, 2)";
    test_success "test_code/gull_5.bird" "1";

  ];;

let suite = "compiler test suite" >::: all_tests;;

run_test_tt_main suite;;
