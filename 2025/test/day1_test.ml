open! Hardcaml
module Day1 = Fpga_2025_lib.Day1
module Sim = Cyclesim.With_interface(Day1.I)(Day1.O)

let%expect_test "Day1 on example input" =
    let input = {|L68
L30
R48
L5
R60
L55
L1
L99
R14
L82|} in
    let scope = Scope.create ~flatten_design:true () in
    let sim = Sim.create (Day1.create scope) in
    let inputs = Cyclesim.inputs sim in
    let outputs = Cyclesim.outputs sim in
    let step c =
        inputs.clear := Bits.gnd;
        inputs.ready_rx := Bits.vdd;
        inputs.ready_tx := Bits.vdd;
        inputs.data := Bits.of_char c;
        Cyclesim.cycle sim; 
    in
    (* Step for start state *)
    let i = ref 0 in
    let buffer = Buffer.create 200 in
    while (Bits.equal (outputs.done_.contents) (Bits.zero 1)) do
        if !i < String.length input then
            step (String.get input !i)
        else
            step '\x00';

        if (Bits.to_bool outputs.rx_done.contents) && (!i < String.length input) then
            i := !i + 1;

        if (Bits.to_bool outputs.tx.contents) then
            let c = Bits.to_char outputs.out_data.contents in
            Buffer.add_char buffer c;
    done;
    print_endline (Buffer.contents buffer);
    [%expect {|
      00000003
      00000006
      |}]

