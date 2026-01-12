open! Hardcaml
module Day1 = Fpga_2025_lib.Day1
module Sim = Cyclesim.With_interface(Day1.I)(Day1.O)

let modulo a b = let tmp = a mod b in
    if tmp < 0 then tmp + b
    else tmp

let rotation_test start rot =
    let expected = modulo (start + rot) 100 in
    let result = Day1.apply_rotation (Signal.of_int ~width:32 start) (Signal.of_int ~width:32 rot) in
    print_string "Expected value: ";
    print_int expected;
    print_endline "";
    print_string "Actual value: ";
    print_int (Signal.to_sint result);
    print_endline "";
    Int.equal (Signal.to_sint result) expected

let%test "Apply positive rotation test" = rotation_test 50 99

let%test "Apply negative rotation test" = rotation_test 10 (-40)

let%test "Apply large positive rotation test" = rotation_test 0 220

let%test "Apply large negative rotation test" = rotation_test 10 (-940)

let%test "Get hex digit of 32bit integer" =
    let n = 0xABCDEF01 in
    let result = Day1.get_hex_digit (Signal.of_int ~width:32 n) (Signal.of_int ~width:3 0) in 
    Char.equal (Signal.to_char result) 'A'

let%test "Get hex digit of 32bit integer" =
    let n = 0xABCDEF01 in
    let result = Day1.get_hex_digit (Signal.of_int ~width:32 n) (Signal.of_int ~width:3 1) in 
    Char.equal (Signal.to_char result) 'B'

let%test "Get hex digit of 32bit integer" =
    let n = 0xABCDEF01 in
    let result = Day1.get_hex_digit (Signal.of_int ~width:32 n) (Signal.of_int ~width:3 7) in 
    Char.equal (Signal.to_char result) '1'

let%test "Testing negation" =
    let acc = 50 in
    let n = 68 in
    let result = Day1.apply_rotation (Signal.of_int ~width:10 acc) (Signal.negate (Signal.of_int ~width:10 n)) in
    Int.equal (Signal.to_int result) 82

let%test "Day1 on example input" =
    let input = {|L68
L30
R48
L5
R60
L55
L1
L99
R14
L82 |} in
    let sim = Sim.create Day1.create in
    let inputs = Cyclesim.inputs sim in
    let outputs = Cyclesim.outputs sim in
    let step c =
        inputs.clear := Bits.gnd;
        inputs.ready_rx := Bits.vdd;
        inputs.ready_tx := Bits.vdd;
        inputs.data := Bits.of_char c;
        Cyclesim.cycle sim; 
        (* print_string "sign: "; *)
        (* print_int (Bits.to_int outputs.sign.contents); *)
        (* print_endline ""; *)
        (**)
        (* print_string "num: "; *)
        (* print_int (Bits.to_int outputs.num.contents); *)
        (* print_endline ""; *)
        (**)
        (* print_string "acc: "; *)
        (* print_int (Bits.to_int outputs.acc.contents); *)
        (* print_endline ""; *)
    in
    (* Step for start state *)
    step '\x00';
    for i=0 to (String.length input -1) do
        step (String.get input i)
    done;
    step '\x00';
    while (Bits.equal (outputs.done_.contents) (Bits.zero 1)) do
        step '\x00';
        let c = Bits.to_char outputs.out_data.contents in
        print_char c;
    done;
    false

(* let%test "test" =  *)
(*     let sim = Sim.create Day1.create in *)
(*     let inputs : _ Day1.I.t = Cyclesim.inputs sim in *)
(*     let outputs : _ Day1.O.t = Cyclesim.outputs sim in *)
(*     let step (n : int)= *)
(*         inputs.clear := Bits.gnd; *)
(*         inputs.ready := Bits.gnd; *)
(*         inputs.clock := Bits.gnd; *)
(*         inputs.data  := Bits.of_int ~width:8 n; *)
(*         Cyclesim.cycle sim; *)
(*         inputs.ready := Bits.vdd; *)
(*         inputs.clock := Bits.vdd; *)
(*         Cyclesim.cycle sim *)
(*     in *)
(*     (step 5); *)
(*     (step 100); *)
(*     Bits.equal outputs.result.contents (Bits.of_int ~width:32 105) *)
