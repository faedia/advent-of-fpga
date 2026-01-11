open! Core
open! Hardcaml
open! Hardcaml.Signal

module I = struct
    type 'a t = {
        clock : 'a;
        clear : 'a;
        ready : 'a;
        data  : 'a[@bits 8];
    }
    [@@deriving hardcaml]
end

module O = struct
    type 'a t = {
        result : 'a[@bits 32]
    }
    [@@deriving hardcaml]
end

let create (i : _ I.t) = 
    let acc = reg_fb
        (Reg_spec.create ~clock:i.clock ~clear:i.clear ())
        ~enable:i.ready
        ~width:32
        ~f:(fun n -> n +: uresize i.data 32) in
    {
        O.result = acc
    }

