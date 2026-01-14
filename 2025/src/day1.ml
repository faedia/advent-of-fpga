open Hardcaml
open Hardcaml.Signal

module I = struct
    type 'a t = {
        clock : 'a;
        clear : 'a;
        (* indicates there is a byte ready to receive *)
        ready_rx : 'a;
        (* indicates that a byte can be sent *)
        ready_tx : 'a;
        data : 'a[@bits 8];
    }
    [@@deriving hardcaml]
end

module O = struct
    type 'a t = {
        (* indicates the computation and result output is complete *)
        done_ : 'a[@rtlname "done"];
        (* indicates that the value on data in was handled *)
        rx_done : 'a;
        (* indicates that a byte is being output for tx *)
        tx : 'a;
        (* this outputs the result byte by byte once computation is done! *)
        out_data : 'a[@bits 8];
    }
    [@@deriving hardcaml]
end

module States = struct
    type t =
        | Start
        | SignByte
        | ReadNumber
        | TxPart1
        | TxPart2
        | Done
    [@@deriving sexp_of, compare, enumerate]
end

let abs_s n = mux2 (n <+. 0) (negate n) n

(* Helpers to perform multiplication of specific constants without *: *)
let mul_10 n = (sll n 3) +: (sll n 1)
let mul_100 n = (sll n 6) +: (sll n 5) +: (sll n 2)
let mul_5243 n = (sll n 12) +: (sll n 10) +: (sll n 6) +: (sll n 5) +: (sll n 4) +: (sll n 3) +: (sll n 1) +: n

(* Divide value n by 100, note n must be of width 100 *)
let div_100 n =
    (* This code is translated from https://godbolt.org/z/c385vcYo4 for fast division *)
    (* TODO: I'm unsure if hardcaml/later synthesis will optimise this kind
       of usecase but I wanted to be sure I had something that did not actually
       make a divisor*)
    (* If not this could be done automatically for known values (i.e. something
       like a function `%:.` similar to `+:.`) could generate a circuit like
       this automatically for (width n) *)
    (* NOTE: Because out range of values is so small after addition -999 to 1098
       we could just make a look up system to see in which 100 it is and just
       subtract by that *)
    let mult = mul_5243 n in
    let tmp_a = srl mult 31 in
    let tmp_b  = sra mult 19 in
    tmp_a +: tmp_b

(* Gets a specific nibble at nibble_index, nibbles are indexed by most significant nibble at 0 *)
let get_nibble signal nibble_index =
    let total_nibbles = Int.shift_right (width signal) 2 in
    let shift_by_nibbles = (of_int ~width:(width signal) (total_nibbles - 1)) -: (uresize nibble_index (width signal)) in
    let total_shift_amount = sll shift_by_nibbles 2 in
    log_shift srl signal total_shift_amount

(* Converts a nibble to the ascii character for the digit *)
let to_hex_digit nibble =
    let nibble_byte = uresize nibble 8 in
    mux2 (nibble <:. 10)
            (nibble_byte +:. (Char.code '0'))
            (nibble_byte +:. ((Char.code 'A' - 10)))

(* Gets a single digit from the signal n, most significant digit is 0 indexed *)
let get_hex_digit n digit = 
    let nibble = get_nibble n digit in
    to_hex_digit (sel_bottom nibble 4)

(* This returns 1 if when num is applied to acc we pass through 0 on the dial *)
let get_if_passed_0 acc num =
    let tmp_acc = acc +: num in
    uresize (mux2 ((acc ==:. 0) &: (num <+. 0)) (zero 1) ((tmp_acc <=+. 0) |: (tmp_acc >=:. 100))) (width acc)

(* This takes a number in the range -99 to 199 and gets the actual position on the dial *)
(* due to the range limitation n must have had the mod operation performed on it *)
let normalise n =
    mux2 (n <+. 0) (n +:. 100) (mux2 (n >=+. 100) (n -:. 100) n)

(* Prints the value in input over out_data digit by digit *)
(* Each time we are ready we put the digit on the out_data and increment the index *)
(* When we are at the end of the input we do the on_done command *)
let print_value ready input (index : Always.Variable.t) out_data tx_wire on_done =
    Always.(
        if_ (ready) [
            if_ (index.value ==:. 8) [
                out_data <-- of_char '\n';
                proc on_done
            ] [
                out_data <-- get_hex_digit input index.value;
                index <-- index.value +:. 1
            ];
            tx_wire <-- vdd
        ]
        []
    )

let create (i : _ I.t) = 
    let r_sync = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = Always.State_machine.create (module States) ~enable:vdd r_sync in

    (* The state of the number being received *)
    let sign = Always.Variable.reg ~enable:vdd ~width:1 r_sync in
    let num = Always.Variable.reg ~enable:vdd ~width:32 r_sync in

    (* Indicates that we have finished calculating and outputing the result *)
    let done_wire = Always.Variable.wire ~default:gnd in 

    (* Used to output the result over the serial connection *)
    let data = Always.Variable.reg ~enable:vdd ~width:8 r_sync in
    let hex_digit_index = Always.Variable.reg ~enable:vdd ~width:4 r_sync in
    let tx = Always.Variable.reg ~enable:vdd ~width:1 r_sync in
    let rx_done = Always.Variable.reg ~enable:vdd ~width:1 r_sync in
    
    (* Current value on the dial *)
    let acc = Always.Variable.reg ~enable:vdd ~width:32 r_sync in
    (* Current result values *)
    let part1_result = Always.Variable.reg ~enable:vdd ~width:32 r_sync in
    let part2_result = Always.Variable.reg ~enable:vdd ~width:32 r_sync in

    (* Used to calculate the new acc and how many times zero is seen *)
    let signed_num = mux2 sign.value num.value (negate num.value) in
    let winds = div_100 (signed_num) in
    (* This offset is effectively `mod signed_num 100` *)
    let offset = signed_num -: mul_100 winds in

    Always.(
        compile [
            sm.switch [
                (
                    States.Start,
                    [
                        sign <-- gnd;
                        num <-- zero 32;
                        acc <-- Signal.of_int ~width:32 50;
                        sm.set_next States.SignByte;
                        rx_done <-- gnd;
                        tx <-- gnd;
                    ]
                );
                (
                    States.SignByte,
                    [
                        if_ i.ready_rx [
                            sign <-- (i.data ==: (of_char 'R'));
                            num <-- zero 32;
                            sm.set_next States.ReadNumber;
                            rx_done <-- vdd;
                        ]
                        [
                            rx_done <-- gnd;
                        ];
                        tx <-- gnd;
                    ]
                );
                (
                    States.ReadNumber,
                    [
                        if_ i.ready_rx [
                            rx_done <-- vdd;
                            (* this means we have recieved a full number and now need to process it *)
                            if_ ((i.data ==: (of_char '\n')) ||: (i.data ==: (of_char '\x00'))) [
                                (* Part 2 is how many we could do full rotations around the dial, and then if on the last partial rotation we passed (or ended on) 0 *)
                                (* it's important to remember here that offset is `mod signed_num 100` *)
                                part2_result <-- part2_result.value +: (abs_s winds) +: (get_if_passed_0 acc.value offset);
                                acc <-- normalise (acc.value +: offset);

                                (* Part 1 is trivial, we are at zero we increment the counter *)
                                if_ (acc.value ==:. 0) [
                                    part1_result <-- part1_result.value +:. 1
                                ] [];


                                (* The null byte is our end of input marker, all our processing is done within this clock so now we just need to start outputting *)
                                if_ (i.data ==: (of_char '\x00')) [
                                    sm.set_next States.TxPart1;
                                    hex_digit_index <-- zero 4
                                ] [
                                    (* We still have more numbers to get! *)
                                    sm.set_next States.SignByte
                                ]
                            ] [
                                (* continue to gather the number! *)
                                num <-- (mul_10 num.value) +: (uresize (i.data -: (of_char '0')) 32); 
                            ];

                        ]
                        [
                            rx_done <-- gnd;
                        ];
                        tx <-- gnd;
                    ]
                );
                (
                    States.TxPart1,
                    [
                        print_value i.ready_tx part1_result.value hex_digit_index data tx [
                            sm.set_next States.TxPart2;
                            hex_digit_index <-- zero (width hex_digit_index.value)
                        ];
                        rx_done <-- gnd;
                    ]
                );
                (
                    States.TxPart2,
                    [
                        print_value i.ready_tx part2_result.value hex_digit_index data tx [sm.set_next States.Done];
                        rx_done <-- gnd;
                    ]
                );
                (
                    States.Done,
                    [
                        done_wire <-- vdd;
                        rx_done <-- gnd;
                        tx <-- gnd;
                    ]
                )
            ]
        ]
    );
    {
        O.done_ = done_wire.value;
        O.tx = tx.value;
        O.rx_done = rx_done.value;
        O.out_data = data.value;
    }

