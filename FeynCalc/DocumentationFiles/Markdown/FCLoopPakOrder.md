## FCLoopPakOrder

`FCLoopPakOrder[poly, {x1, x2, ...}]` determines a canonical ordering of the Feynman parameters `x1, x2, ...` in the polynomial `poly`.

The function uses the algorithm of Alexey Pak [arXiv:1111.0868](https://arxiv.org/abs/1111.0868). Cf. also the PhD thesis of Jens Hoff [10.5445/IR/1000047447](https://doi.org/10.5445/IR/1000047447) for the detailed description of a possible implementation.

The current implementation is based on the `PolyOrdering` function from FIRE 6 [arXiv:1901.07808](https://arxiv.org/abs/1901.07808)

The function can also directly perform the renaming of the Feynman parameter variables returning the original polynomial in the canonical form. This is done by setting the option `Rename` to `True`.

### See also

[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCLoopToPakForm](FCLoopToPakForm.md), [FCLoopPakOrder](FCLoopPakOrder.md).

### Examples

#### Canonicalizing a polynomial

Let us consider the following product of `U` and `F` polynomials of some loop integral

```mathematica
poly = (x[1]*x[2] + x[1]*x[3] + x[2]*x[3] + x[2]*x[4] + x[3]*x[4] + x[1]*x[5] + x[2]*x[5] + x[4]*x[5])*
   (m1^2*x[1]^2*x[2] + m3^2*x[1]*x[2]^2 + m1^2*x[1]^2*x[3] + m1^2*x[1]*x[2]*x[3] + m2^2*x[1]*x[2]*x[3] + 
      m3^2*x[1]*x[2]*x[3] + m3^2*x[2]^2*x[3] + m2^2*x[1]*x[3]^2 + m2^2*x[2]*x[3]^2 + m1^2*x[1]*x[2]*x[4] - 
      SPD[q, q]*x[1]*x[2]*x[4] + m3^2*x[2]^2*x[4] + m1^2*x[1]*x[3]*x[4] - SPD[q, q]*x[1]*x[3]*x[4] + 
      m2^2*x[2]*x[3]*x[4] + m3^2*x[2]*x[3]*x[4] - SPD[q, q]*x[2]*x[3]*x[4] + m2^2*x[3]^2*x[4] + m1^2*x[1]^2*x[5] + 
      m1^2*x[1]*x[2]*x[5] + m3^2*x[1]*x[2]*x[5] - SPD[q, q]*x[1]*x[2]*x[5] + m3^2*x[2]^2*x[5] + m2^2*x[1]*x[3]*x[5] - 
      SPD[q, q]*x[1]*x[3]*x[5] + m2^2*x[2]*x[3]*x[5] - SPD[q, q]*x[2]*x[3]*x[5] + m1^2*x[1]*x[4]*x[5] - 
      SPD[q, q]*x[1]*x[4]*x[5] + m3^2*x[2]*x[4]*x[5] + m2^2*x[3]*x[4]*x[5] - SPD[q, q]*x[3]*x[4]*x[5])
```

$$(x(1) x(2)+x(3) x(2)+x(4) x(2)+x(5) x(2)+x(1) x(3)+x(3) x(4)+x(1) x(5)+x(4) x(5)) \left(\text{m1}^2 x(1)^2 x(2)+\text{m1}^2 x(1)^2 x(3)+\text{m1}^2 x(1) x(2) x(3)+\text{m1}^2 x(1) x(2) x(4)+\text{m1}^2 x(1) x(3) x(4)+\text{m1}^2 x(1)^2 x(5)+\text{m1}^2 x(1) x(2) x(5)+\text{m1}^2 x(1) x(4) x(5)+\text{m2}^2 x(1) x(3)^2+\text{m2}^2 x(2) x(3)^2+\text{m2}^2 x(1) x(2) x(3)+\text{m2}^2 x(3)^2 x(4)+\text{m2}^2 x(2) x(3) x(4)+\text{m2}^2 x(1) x(3) x(5)+\text{m2}^2 x(2) x(3) x(5)+\text{m2}^2 x(3) x(4) x(5)+\text{m3}^2 x(1) x(2)^2+\text{m3}^2 x(2)^2 x(3)+\text{m3}^2 x(1) x(2) x(3)+\text{m3}^2 x(2)^2 x(4)+\text{m3}^2 x(2) x(3) x(4)+\text{m3}^2 x(2)^2 x(5)+\text{m3}^2 x(1) x(2) x(5)+\text{m3}^2 x(2) x(4) x(5)-q^2 x(1) x(2) x(4)-q^2 x(1) x(3) x(4)-q^2 x(2) x(3) x(4)-q^2 x(1) x(2) x(5)-q^2 x(1) x(3) x(5)-q^2 x(2) x(3) x(5)-q^2 x(1) x(4) x(5)-q^2 x(3) x(4) x(5)\right)$$

Using `FCLoopPakOrder` we can obtain a canonical ordering for this polynomial

```mathematica
sigma = FCLoopPakOrder[poly, x]
```

$$\left(
\begin{array}{ccccc}
 1 & 3 & 2 & 5 & 4 \\
\end{array}
\right)$$

This output implies that the polynomial will become canonically ordered upon renaming the Feynman parameter variables as follows

```mathematica
fpVars = Table[x[i], {i, 1, 5}]
```

$$\{x(1),x(2),x(3),x(4),x(5)\}$$

```mathematica
repRule = Thread[Rule[Extract[fpVars, List /@ First[sigma]], fpVars]]
```

$$\{x(1)\to x(1),x(3)\to x(2),x(2)\to x(3),x(5)\to x(4),x(4)\to x(5)\}$$

This way we obtain the canonical form of our polynomial `poly`

```mathematica
poly /. repRule
```

$$(x(1) x(2)+x(3) x(2)+x(5) x(2)+x(1) x(3)+x(1) x(4)+x(3) x(4)+x(3) x(5)+x(4) x(5)) \left(\text{m1}^2 x(1)^2 x(2)+\text{m1}^2 x(1)^2 x(3)+\text{m1}^2 x(1) x(2) x(3)+\text{m1}^2 x(1)^2 x(4)+\text{m1}^2 x(1) x(3) x(4)+\text{m1}^2 x(1) x(2) x(5)+\text{m1}^2 x(1) x(3) x(5)+\text{m1}^2 x(1) x(4) x(5)+\text{m2}^2 x(1) x(2)^2+\text{m2}^2 x(2)^2 x(3)+\text{m2}^2 x(1) x(2) x(3)+\text{m2}^2 x(1) x(2) x(4)+\text{m2}^2 x(2) x(3) x(4)+\text{m2}^2 x(2)^2 x(5)+\text{m2}^2 x(2) x(3) x(5)+\text{m2}^2 x(2) x(4) x(5)+\text{m3}^2 x(1) x(3)^2+\text{m3}^2 x(2) x(3)^2+\text{m3}^2 x(1) x(2) x(3)+\text{m3}^2 x(3)^2 x(4)+\text{m3}^2 x(1) x(3) x(4)+\text{m3}^2 x(3)^2 x(5)+\text{m3}^2 x(2) x(3) x(5)+\text{m3}^2 x(3) x(4) x(5)-q^2 x(1) x(2) x(4)-q^2 x(1) x(3) x(4)-q^2 x(2) x(3) x(4)-q^2 x(1) x(2) x(5)-q^2 x(1) x(3) x(5)-q^2 x(2) x(3) x(5)-q^2 x(1) x(4) x(5)-q^2 x(2) x(4) x(5)\right)$$

#### Checking equivalence

Let us consider the following two polynomials

```mathematica
poly1 = -1/4*(x[2]^2*x[3]) - (x[1]^2*x[4])/4 - 
   (x[1]^2*x[5])/4 + (x[1]*x[2]*x[5])/2 - (x[2]^2*x[5])/4 + x[3]*x[4]*x[5]
```

$$-\frac{1}{4} x(4) x(1)^2-\frac{1}{4} x(5) x(1)^2+\frac{1}{2} x(2) x(5) x(1)-\frac{1}{4} x(2)^2 x(3)-\frac{1}{4} x(2)^2 x(5)+x(3) x(4) x(5)$$

```mathematica
poly2 = -1/4*(x[1]^2*x[2]) - (x[1]^2*x[3])/4 + 
   x[2]*x[3]*x[4] + (x[1]*x[3]*x[5])/2 - (x[3]*x[5]^2)/4 - (x[4]*x[5]^2)/4
```

$$-\frac{1}{4} x(2) x(1)^2-\frac{1}{4} x(3) x(1)^2+\frac{1}{2} x(3) x(5) x(1)-\frac{1}{4} x(3) x(5)^2-\frac{1}{4} x(4) x(5)^2+x(2) x(3) x(4)$$

These polynomials are not identical

```mathematica
poly1 === poly2
```

$$\text{False}$$

However, one can easily recognize that they are actually the same upon renaming Feynman parameters
`x[i]` in a suitable way. `FCLoopPakOrder` can do such renamings automatically

```mathematica
canoPoly1 = FCLoopPakOrder[poly1, x, Rename -> True]
canoPoly2 = FCLoopPakOrder[poly2, x, Rename -> True]
```

$$-\frac{1}{4} x(3) x(1)^2-\frac{1}{4} x(5) x(1)^2+\frac{1}{2} x(2) x(3) x(1)-\frac{1}{4} x(2)^2 x(3)-\frac{1}{4} x(2)^2 x(4)+x(3) x(4) x(5)$$

$$-\frac{1}{4} x(3) x(1)^2-\frac{1}{4} x(5) x(1)^2+\frac{1}{2} x(2) x(3) x(1)-\frac{1}{4} x(2)^2 x(3)-\frac{1}{4} x(2)^2 x(4)+x(3) x(4) x(5)$$

When comparing the canonicalized versions of both polynomials we see that they are indeed identical

```mathematica
canoPoly1 === canoPoly2
```

$$\text{True}$$
