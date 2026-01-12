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
        data : 'a[@bits 8];
    }
    [@@deriving hardcaml]
end

module States = struct
    type t =
        | SignByte
        | ReadNumber
        | TxByte
        | WaitForReady
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
        n -: lower_bound
        in
    let modulo_100 n =
        let tmp = mod_100 n in
        (* If it is negative (msb is set) then shift it back up by 100 to get the actual modulo (as if winding properly) *)
        mux2 (msb tmp) (tmp +:. 100) tmp in
    modulo_100 (n +: rot)

let create (i : _ I.t) = 
    (* multiplying by 10 is the same multiply by 8 and then adding the it multiplied twice *)
    let mul_10 (n : Reg_spec.signal) = (sll n 3) +: (sll n 1) in
    (* The magic numbers are taken from godbolt to prevent me from having to figure it out myself *)
    let r_sync = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sign = Always.Variable.reg ~enable:vdd ~width:1 r_sync in
    let num = Always.Variable.reg ~enable:vdd ~width:10 r_sync in
    let acc = Always.Variable.reg ~enable:vdd ~width:32 r_sync in
    let sm = Always.State_machine.create (module States) ~enable:vdd r_sync in
    Always.(
        compile [
            sm.switch [
                (
                    States.SignByte,
                    [
                        if_ i.ready_rx [
                            sign <-- (i.data ==: (of_char 'R'));
                            num <-- zero 10;
                            sm.set_next States.ReadNumber
                        ]
                        []
                    ]
                );
                (
                    States.ReadNumber,
                    [
                        if_ i.ready_rx [
                            if_ (i.data ==: (of_char '\n')) [
                                (* Do the computation! *)
                                (* add (or subtract) the current value to to the current accumulator *)
                                acc <-- apply_rotation num.value (mux2 sign.value num.value (negate num.value))
                            ] [
                                (* continue to gather the number! *)
                                num <-- (mul_10 num.value) +: (i.data -: (of_char '0')); 
                            ]
                        ]
                        []
                    ]
                )
            ]
        ]
    );

