## Collect3

`Collect3[expr, {x, y, ...}]` collects terms involving the same powers of monomials $x^{n_1}$, $y^{n_2}$, ...

The option `Factor` can bet set to `True` or `False`, which factors the coefficients.

The option `Head` (default `Plus`) determines the applied function to the list of monomials  multiplied by their coefficients.

### See also

[Overview](Extra/FeynCalc.md), [Collect2](Collect2.md), [Isolate](Isolate.md).

### Examples

```mathematica
Collect3[2 a (b - a) (h - 1) - b^2 (e a - c) + b^2, {a, b}]
```

$$-2 a^2 h+2 a^2-a b^2 e+2 a b h-2 a b+b^2 c+b^2$$

```mathematica
Collect3[Expand[(a - b - c - d)^5], {a}]
```

$$a^5-5 a^4 b-5 a^4 c-5 a^4 d+10 a^3 b^2+20 a^3 b c+20 a^3 b d+10 a^3 c^2+20 a^3 c d+10 a^3 d^2-10 a^2 b^3-30 a^2 b^2 c-30 a^2 b^2 d-30 a^2 b c^2-60 a^2 b c d-30 a^2 b d^2-10 a^2 c^3-30 a^2 c^2 d-30 a^2 c d^2-10 a^2 d^3+5 a b^4+20 a b^3 c+20 a b^3 d+30 a b^2 c^2+60 a b^2 c d+30 a b^2 d^2+20 a b c^3+60 a b c^2 d+60 a b c d^2+20 a b d^3+5 a c^4+20 a c^3 d+30 a c^2 d^2+20 a c d^3+5 a d^4-b^5-5 b^4 c-5 b^4 d-10 b^3 c^2-20 b^3 c d-10 b^3 d^2-10 b^2 c^3-30 b^2 c^2 d-30 b^2 c d^2-10 b^2 d^3-5 b c^4-20 b c^3 d-30 b c^2 d^2-20 b c d^3-5 b d^4-c^5-5 c^4 d-10 c^3 d^2-10 c^2 d^3-5 c d^4-d^5$$
