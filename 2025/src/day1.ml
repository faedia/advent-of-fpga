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
        (* indicates that a byte is being output for tx *)
        tx : 'a;
        (* this outputs the result byte by byte once computation is done! *)
        out_data : 'a[@bits 8];
        acc : 'a[@bits 32];
        num : 'a[@bits 32];
        sign : 'a;
        result : 'a[@bits 32];
        state : 'a[@bits 8];
    }
    [@@deriving hardcaml]
end

module States = struct
    type t =
        | Start
        | SignByte
        | ReadNumber
        | TxPart1
        (* | TxPart2 *)
        | Done
    [@@deriving sexp_of, compare, enumerate]
end

let apply_rotation n rot = 
    let mod_100 n =
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
        let mult = sresize (n *+ (of_int ~width:32 5243)) 32 in
        let tmp_a = srl mult 31 in
        let tmp_b  = sra mult 19 in
        let div = tmp_a +: tmp_b in
        (* once we have the division we can then multiply by 100 and see the difference is the remainder *)
        let lower_bound = sresize (div *+ (of_int ~width:32 100)) 32 in
        n -: sresize lower_bound (width n) 
        in
    let modulo_100 n =
        let tmp = mod_100 n in
        (* If it is negative (msb is set) then shift it back up by 100 to get the actual modulo (as if winding properly) *)
        mux2 (msb tmp) (tmp +:. 100) tmp in
    modulo_100 (n +: rot)

let get_nibble signal nibble =
    let total_nibbles = Int.shift_right (width signal) 2 in
    let shift_by_nibbles = (of_int ~width:(width signal) (total_nibbles - 1)) -: (uresize nibble (width signal)) in
    let total_shift_amount = sll shift_by_nibbles 2 in
    log_shift srl signal total_shift_amount

let to_hex_digit nibble =
    let nibble_byte = uresize nibble 8 in
    mux2 (nibble <:. 10)
            (nibble_byte +:. (Char.code '0'))
            (nibble_byte +:. ((Char.code 'A' - 10)))

let get_hex_digit n digit = 
    let nibble = get_nibble n digit in
    to_hex_digit (sel_bottom nibble 4)

let create (i : _ I.t) = 
    (* multiplying by 10 is the same multiply by 8 and then adding the it multiplied twice *)
    let mul_10 (n : Reg_spec.signal) = (sll n 3) +: (sll n 1) in
    let r_sync = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sign = Always.Variable.reg ~enable:vdd ~width:1 r_sync in
    let num = Always.Variable.reg ~enable:vdd ~width:32 r_sync in
    let acc = Always.Variable.reg ~enable:vdd ~width:32 r_sync in
    let sm = Always.State_machine.create (module States) ~enable:vdd r_sync in
    let done_wire = Always.Variable.wire ~default:gnd in 
    let tx_wire = Always.Variable.wire ~default:gnd in
    let data = Always.Variable.reg ~enable:vdd ~width:8 r_sync in
    let part1_result = Always.Variable.reg ~enable:vdd ~width:32 r_sync in
    let hex_digit = Always.Variable.reg ~enable:vdd ~width:3 r_sync in
    Always.(
        compile [
            sm.switch [
                (
                    States.Start,
                    [
                        sign <-- gnd;
                        num <-- zero 32;
                        acc <-- Signal.of_int ~width:32 50;
                        sm.set_next States.SignByte
                    ]
                );
                (
                    States.SignByte,
                    [
                        if_ i.ready_rx [
                            sign <-- (i.data ==: (of_char 'R'));
                            num <-- zero 32;
                            sm.set_next States.ReadNumber
                        ]
                        []
                    ]
                );
                (
                    States.ReadNumber,
                    [
                        if_ i.ready_rx [
                            if_ ((i.data ==: (of_char '\n')) ||: (i.data ==: (of_char '\x00'))) [
                                (* Do the computation! *)
                                (* add (or subtract) the current value to to the current accumulator *)
                                acc <-- apply_rotation acc.value (mux2 sign.value num.value (negate num.value));

                                if_ (acc.value ==:. 0) [
                                    part1_result <-- part1_result.value +:. 1
                                ] [];

                                if_ (i.data ==: (of_char '\x00')) [
                                    sm.set_next States.TxPart1;
                                    hex_digit <-- zero 3
                                ] [
                                    sm.set_next States.SignByte
                                ]
                            ] [
                                (* continue to gather the number! *)
                                num <-- (mul_10 num.value) +: (uresize (i.data -: (of_char '0')) 32); 
                            ]
                        ]
                        []
                    ]
                );
                (
                    States.TxPart1,
                    [
                        if_ (i.ready_tx) [
                            data <-- get_hex_digit part1_result.value hex_digit.value;
                            tx_wire <-- vdd;
                            if_ (hex_digit.value ==: ones (width hex_digit.value)) [
                                sm.set_next States.Done 
                            ][
                                hex_digit <-- hex_digit.value +:. 1
                            ]
                        ] []
                    ]
                );
                (
                    States.Done,
                    [
                        done_wire <-- vdd
                    ]
                )
            ]
        ]
    );
    {
        O.done_ = done_wire.value;
        O.tx = tx_wire.value;
        O.out_data = data.value;
        O.acc = acc.value;
        O.num = num.value;
        O.sign = sign.value;
        O.result = part1_result.value;
        O.state = uresize sm.current 8;
    }

