# Arc

The programming language of Arc (Array Reversible Combinatorics) builds on the primitive interpretation of [this](https://github.com/Victoriast8/arc/blob/main/report.pdf) (and not [this](https://en.wikipedia.org/wiki/Arc_(programming_language))) somewhat poorly written project report, which explores concrete interpretations of reversible second-order array combinatorics, such as reversible `scan` and `reduce`.

As described in the report, Arc is the attempt of intersecting the programming language [Futhark](https://futhark-lang.org/) with the reversible, injective semantics of [Janus](https://topps.diku.dk/pirc/?id=janus).

## Limitations

This interpreter is a proof of concept. Keep in mind that some of the advantages of reversible programming languages (such as mitigation of heat dispersion) is lost, due to the interpreter being written in Haskell, which is *not* reversible.
