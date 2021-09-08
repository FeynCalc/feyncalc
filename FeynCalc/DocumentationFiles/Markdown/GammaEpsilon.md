## GammaEpsilon

`GammaEpsilon[exp]` gives a series expansion of `Gamma[exp]` in `Epsilon` up to order `6` (where `EulerGamma` is neglected).

### See also

[Overview](Extra/FeynCalc.md), [GammaExpand](GammaExpand.md), [Series2](Series2.md).

### Examples

If the argument is of the form `(1+a Epsilon)` the result is not calculated but tabulated.

```mathematica
GammaEpsilon[1 + a Epsilon]
```

$$\varepsilon ^5 \left(-\frac{a^5 \zeta (5)}{5}-\frac{1}{36} \pi ^2 a^5 \zeta (3)\right)+\frac{1}{160} \pi ^4 a^4 \varepsilon ^4-\frac{1}{3} a^3 \varepsilon ^3 \zeta (3)+\frac{1}{12} \pi ^2 a^2 \varepsilon ^2+\text{C\$15868} \varepsilon ^6+1$$

```mathematica
GammaEpsilon[1 - Epsilon/2]
```

$$\text{C\$15906} \varepsilon ^6+\frac{\pi ^4 \varepsilon ^4}{2560}+\frac{\pi ^2 \varepsilon ^2}{48}+\varepsilon ^5 \left(\frac{\pi ^2 \zeta (3)}{1152}+\frac{\zeta (5)}{160}\right)+\frac{\varepsilon ^3 \zeta (3)}{24}+1$$

For other arguments the expansion is calculated.

```mathematica
GammaEpsilon[Epsilon]
```

$$\text{C\$17051} \varepsilon ^6+\frac{\pi ^4 \varepsilon ^3}{160}+\frac{\pi ^2 \varepsilon }{12}+\frac{1}{\varepsilon }+\frac{1}{720} \varepsilon ^5 \left(\frac{61 \pi ^6}{168}+10 \psi ^{(2)}(1)^2\right)+\frac{\varepsilon ^2 \psi ^{(2)}(1)}{6}+\frac{\varepsilon ^6 \left(-84 \pi ^2 \zeta (5)+\frac{21 \pi ^4 \psi ^{(2)}(1)}{4}+\psi ^{(6)}(1)\right)}{5040}+\frac{1}{120} \varepsilon ^4 \left(\frac{5 \pi ^2 \psi ^{(2)}(1)}{3}-24 \zeta (5)\right)$$

```mathematica
GammaEpsilon[x]
```

$$\Gamma (x)$$
