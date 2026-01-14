# Advent of FPGA 2025

This solves some (currently 1) of the problems for Advent of Code in an FPGA using `hardcaml`.

Problems solved:

- Day 1
    - Part 1
    - Part 2

I have verfied the result using my input to the Advent of Code problem and got the correct result.

## Build and Run

To build the designs, along with the simulation and the `verilog` output make sure you have ocaml, and hardcaml installed.
Then run:

```sh
dune build
```

You can then run the generated `run_sim` and `make_rtl` exectuables.
Run sim will run the design given an input file containing some problem input (there should be no empty lines at the bottom of the file).

```sh
./run_sim.exe input.txt
```

The result will then be output on the terminal like so:

```
part1 result: 3
part2 result: 6
```

Or to get some verilog to integrate into a large design you can run:

```
make_rtl.exe
```

Which will output `verilog` of the design over standard out.

I do not have an FPGA, so I have not been able to run the result on actual hardware.
So it may not work due to unknown reasons, however the simulation does work and output the correctly result for me.

## Test

A test can be found in the `test/` directory that runs the simulation of the design with the example input from Advent of Code, and verifies its output.

Not being a hardware engineer, I am not used to testbenches, so went with a unittest approach which I'm more familar with, where the unit under test is the whole design provided.

## I/O

Having not done any FPGA work since university, and not having much time to get into the weeds of things I've gone for a simple IO mechansim.
It is assumed that for the solver bytes can be provided to it via some kind of serial interface (such as UART) and that the result can be output as ASCII again over UART.
Although this is bring your own UART as one is not implemented here due to the aforementioned time constraint.
In the design result is output in hexadecimal rather than decimal, so some conversion to decmial will be needed before submitting the result on Advent of Code.
However the simulation provided already does this conversion and outputs the result in decimal.

## Design

My solution I did in `Fortran` when doing the problem in software at the start of Advent of Code had divisions that I wanted to avoid.
So my main goal was to get a solution that only used shifts and adds (that to my knowledge are much nicer for FPGA to handle).
And since the divisions were a division by constant, I was able to use some optimisation techniques to convert it to a multiplication and some shifts.
Then I transformed the multiplication into the shifts and additions that would actually represent that multiplication since we have a multiplication by constant as well.
Therefore while the idea of the design is based around division the actual `hardcaml` code contains no division operator and instead the division functions are adds and shifts.

From that the core premise of the design is receiving the input data as bytes over UART and processing them immediately in the clock cycle they are received.
That way the time taken to finish executing the problem input is the time taken for the input to be transferred to the device, and for the result to be transmitted out.



This was a very fun task to undertake, and very different from my usual day-to-day of writing software.
Since I was trying to get this done in-time for a deadline I only had time to complete one of the problems, but I will endevour to solve some more now that there is no time preasure!

