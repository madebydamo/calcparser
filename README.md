# calcparser
Haskell Parser parsing an input calculation and returns its result with correct order of operation.

To use run `ghci Calc.hs` and the use it with `calc [input]` where input is a valid calculation consisting of [0-9.+-/^ ].

## Example
calc "3.4 + -34 * 2 ^ 0.3 - 2.2321 / 3"
-> -39.20294338706049
