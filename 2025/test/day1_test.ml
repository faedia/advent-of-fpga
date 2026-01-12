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

let %test "Apply large positive rotation test" = rotation_test 0 220

let %test "Apply large negative rotation test" = rotation_test 10 (-940)

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
