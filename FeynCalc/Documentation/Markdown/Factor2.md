## Factor2

`Factor2[poly]` factors a polynomial in a standard way.

`Factor2` works sometimes better than Factor on polynomials involving rationals with sums in the denominator.

`Factor2` uses `Factor` internally and is in general slower than `Factor`. There are four possible settings of the option `Method` (`0`,`1`,`2`,`3`). In general, `Factor` will work faster than `Factor2`.

### See also

[Overview](Extra/FeynCalc.md), [Collect2](Collect2.md).

### Examples

```mathematica
(a - x) (b - x)
{Factor2[%], Factor[%]}
```

$$(a-x) (b-x)$$

$$\{(a-x) (b-x),-((a-x) (x-b))\}$$

```mathematica
ex = Expand[(a - b) (a + b)]
```

$$a^2-b^2$$

```mathematica
Factor[ex]
```

$$(a-b) (a+b)$$

```mathematica
Factor2[ex]
```

$$a^2-b^2$$

```mathematica
Factor2[ex, FactorFull -> True]
```

$$(a-b) (a+b)$$
