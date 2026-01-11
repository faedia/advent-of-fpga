open! Hardcaml
module Day1 = Fpga_2025_lib.Day1
module Sim = Cyclesim.With_interface(Day1.I)(Day1.O)

let%test "test" = 
    let sim = Sim.create Day1.create in
    let inputs : _ Day1.I.t = Cyclesim.inputs sim in
    let outputs : _ Day1.O.t = Cyclesim.outputs sim in
    let step (n : int)=
        inputs.clear := Bits.gnd;
        inputs.ready := Bits.gnd;
        inputs.clock := Bits.gnd;
        inputs.data  := Bits.of_int ~width:8 n;
        Cyclesim.cycle sim;
        inputs.ready := Bits.vdd;
        inputs.clock := Bits.vdd;
        Cyclesim.cycle sim
    in
    (step 5);
    (step 100);
    Bits.equal outputs.result.contents (Bits.of_int ~width:32 105)
