open Fpga_2025_lib
open Hardcaml
open! Core

module Sim = Cyclesim.With_interface (Day1.I) (Day1.O)


let () =
    (* We run the simulation on the input until the circuit reports done *)
    (* We copy all the bytes being written out toa buffer that is then transformed to integers to print *)
    if Array.length (Sys.get_argv ()) <> 2 then exit 1;
    let input_file = Array.get (Sys.get_argv ()) 1 in
    let data = In_channel.read_all input_file in
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
    let i = ref 0 in
    let buffer = Buffer.create 200 in
    while (Bits.equal (outputs.done_.contents) (Bits.zero 1)) do
        if !i < String.length data then
            step (String.get data !i)
        else
            step '\x00';

        if (Bits.to_bool outputs.rx_done.contents) && (!i < String.length data) then
            i := !i + 1;

        if (Bits.to_bool outputs.tx.contents) then
            let c = Bits.to_char outputs.out_data.contents in
            Buffer.add_char buffer c
    done;
    let part1_result = List.nth_exn (String.split_lines (Buffer.contents buffer)) 0 in
    let part2_result = List.nth_exn (String.split_lines (Buffer.contents buffer)) 1 in
    Printf.printf "part1 result: %d\n" (int_of_string ("0x" ^ (String.strip part1_result)));
    Printf.printf "part2 result: %d\n" (int_of_string ("0x" ^ part2_result));

