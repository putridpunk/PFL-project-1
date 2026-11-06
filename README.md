# PFL 2026/27 — First Project: Logical Calculator

Repository for my first project for PFL @ L.EIC FEUP.

## Submission

**Submit only `Main.hs`.** The Codex environment provides the `Parsing.hs`
module from Lecture 11. The copy in this folder is only a local reconstruction
so that the project can be compiled and tested outside Codex.

`Main.hs` uses only these names from the lecture library:

```haskell
Parser, parse, char, satisfy, many
```

These match the combinators shown in the Lecture 11 slides (`parse`,
`satisfy`, `char`, `many`, `many1`, `<|>`).

## Files

- `Main.hs` — the file to submit.
- `Parsing.hs` — local copy of the lecture parser library (not submitted).
- `tests/` — additional local test cases.

## Build and run

With GHC:

```bash
ghc -O2 Main.hs -o main
./main < input.txt
```

Or with the interpreter:

```bash
runghc Main.hs < input.txt
```

## Behaviour

Each non-blank input line is parsed as a proposition and classified as:

- `TAUTOLOGY` — true for every assignment of truth values to its variables;
- `CONTRADICTION` — false for every assignment;
- `CONTINGENT` — true for some assignments and false for others.

Whitespace is ignored, so `a & (b | c)` is accepted. A line that is not a
valid proposition prints `INVALID`; this case is not specified by the project
statement, which only gives valid inputs.

## Implementation notes

- `prop` and `term` use `many` to collect the operands separated by `|` and
  `&`, then combine them with `foldl`. This gives conjunction higher
  precedence than disjunction and makes both operators left associative.
- `factor` reads the first character and dispatches on it:
  `!` for negation, `(` for parentheses, `T`/`F` for constants, and a
  lowercase letter for a variable.
- `parseProp` accepts only a parse that consumes the whole line, i.e.
  `[(p, "")]`.
- `substs` generates all `2^n` truth assignments for the `n` distinct
  variables of a proposition, and `truthTable` evaluates the proposition for
  each assignment.
- `classify` checks whether the result column is all `True`, all `False`, or
  mixed.

## Tests

`tests/cases.in` contains 65 valid and invalid cases with expected output in
`tests/cases.out`. `tests/whitespace.in` checks blank lines, spaces and tabs.
On a working GHC installation:

```bash
runghc Main.hs < tests/cases.in > tests/cases.actual
diff tests/cases.out tests/cases.actual
```

## Sample

Input:

```text
T
a&F
a
a|!a
!p&!!p
p&T|p
p|!p&q
p&!p&p
!(p&q)|p
```

Output:

```text
TAUTOLOGY
CONTRADICTION
CONTINGENT
TAUTOLOGY
CONTRADICTION
CONTINGENT
CONTINGENT
CONTRADICTION
TAUTOLOGY
```
