# Katas

Six small functions in `Katas.java`, each a stub, each with its own test in `KatasTest.java`. Run one kata's test with
`java KatasTest.java <kata>` (for example `java KatasTest.java roman`), or all with `java KatasTest.java`. Java 25; no
build tool.

- `roman(int n)`: `n` (1–3999) in Roman numerals, subtractive form: 4 is `IV`, 1994 is `MCMXCIV`.
- `runLength(String s)`: run-length encoding, each run as its count and its character: `aaabcc` is `3a1b2c`; empty is empty.
- `balanced(String s)`: whether the brackets `()`, `[]` and `{}` in `s` are balanced and properly nested; other
  characters are ignored.
- `wordFrequencies(String text)`: each word (letters and apostrophes, lowercased) with its count, most frequent first,
  ties in alphabetical order, as `word=count` joined by commas: `"the cat the end"` is `the=2,cat=1,end=1`.
- `luhn(String number)`: whether `number` (digits and spaces; at least two digits once spaces are removed; anything
  else is invalid) passes the Luhn check.
- `spiral(int n)`: the `n`×`n` matrix of 1 to `n²` filled clockwise from the top-left, rows joined by `/`, numbers by
  spaces: `spiral(3)` is `1 2 3/8 9 4/7 6 5`; `spiral(0)` is empty.
