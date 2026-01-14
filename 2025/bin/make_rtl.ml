open Fpga_2025_lib
open! Hardcaml

module Circuit = Hardcaml.Circuit.With_interface (Day1.I) (Day1.O)

let () =
    let scope = Scope.create ~flatten_design:true () in
    let circuit = Circuit.create_exn ~name:"Day1" (Day1.create scope) in
    Hardcaml.Rtl.print Hardcaml.Rtl.Language.Verilog circuit
