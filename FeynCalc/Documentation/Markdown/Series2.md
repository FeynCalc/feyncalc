## Series2

`Series2` performs a series expansion around `0`. `Series2` is (up to the `Gamma`-bug in Mathematica versions smaller than 5.0) equivalent to `Series`, except that it applies `Normal` on the result and has an option `FinalSubstitutions`.

`Series2[f, e, n]` is equivalent to `Series2[f, {e, 0, n}]`.

### See also

[Overview](Extra/FeynCalc.md), [Series3](Series3.md).

### Examples

```mathematica
Series2[(x (1 - x))^(\[Delta]/2), \[Delta], 1]
```

$$\frac{1}{2} \delta  \log (1-x)+\frac{1}{2} \delta  \log (x)+1$$

```mathematica
Series2[Gamma[x], x, 1]
```

$$\frac{1}{2} \zeta (2) x+\frac{1}{x}$$

```mathematica
Series[Gamma[x], {x, 0, 1}]
```

$$\frac{1}{x}-\gamma +\frac{1}{12} \left(6 \gamma ^2+\pi ^2\right) x+O\left(x^2\right)$$

```mathematica
Series2[Gamma[x], x, 2]
```

$$-\frac{x^2 \zeta (3)}{3}+\frac{1}{2} \zeta (2) x+\frac{1}{x}$$

```mathematica
Series2[Gamma[x], x, 2, FinalSubstitutions -> {}] // FullSimplify
```

$$\frac{1}{6} \left(-3 \gamma  \left(\zeta (2) x^2+2\right)-2 x^2 \zeta (3)-\gamma ^3 x^2+3 \zeta (2) x+3 \gamma ^2 x+\frac{6}{x}\right)$$

```mathematica
Series[Gamma[x], {x, 0, If[$VersionNumber < 5, 4, 2]}] // Normal // Expand // FullSimplify
```

$$\frac{1}{12} \left(-2 \gamma ^3 x^2-\gamma  \left(\pi ^2 x^2+12\right)+x \left(\pi ^2-4 x \zeta (3)\right)+6 \gamma ^2 x+\frac{12}{x}\right)$$

There is a table of expansions of special hypergeometric functions.

```mathematica
Series2[HypergeometricPFQ[{1, OPEm - 1, Epsilon/2 + OPEm}, {OPEm, OPEm + Epsilon}, 1], Epsilon, 1]
```

$$-\frac{2}{\varepsilon }+\frac{2 m}{\varepsilon }+\frac{1}{2} \varepsilon  m \psi ^{(1)}(m)-\frac{\varepsilon  \psi ^{(1)}(m)}{2}+1$$

```mathematica
Series2[HypergeometricPFQ[{1, OPEm, Epsilon/2 + OPEm}, {1 + OPEm, Epsilon + OPEm},  1], Epsilon, 1]
```

$$\frac{1}{4} \varepsilon  \zeta (2) m+\frac{2 m}{\varepsilon }+\frac{1}{4} \varepsilon  m \psi ^{(0)}(m)^2+\frac{3}{4} \varepsilon  m \psi ^{(1)}(m)-\frac{1}{2} \varepsilon  m S_{11}(m-1)$$

```mathematica
Hypergeometric2F1[1, Epsilon, 1 + 2 Epsilon, x]
Series2[%, Epsilon, 3]
```

$$\, _2F_1(1,\varepsilon ;2 \varepsilon +1;x)$$

$$-2 \varepsilon ^2 \zeta (2)+2 \varepsilon ^3 \;\text{Li}_3(1-x)+2 \varepsilon ^2 \;\text{Li}_2(1-x)-4 \varepsilon ^3 \;\text{Li}_2(1-x) \log (x)-4 \varepsilon ^3 S_{12}(1-x)-2 \varepsilon ^3 \zeta (2) \log (1-x)+4 \varepsilon ^3 \zeta (2) \log (x)-\frac{1}{6} \varepsilon ^3 \log ^3(1-x)-2 \varepsilon ^3 \log (1-x) \log ^2(x)+\varepsilon ^3 \log ^2(1-x) \log (x)-\frac{1}{2} \varepsilon ^2 \log ^2(1-x)+2 \varepsilon ^2 \log (1-x) \log (x)-\varepsilon  \log (1-x)+2 \varepsilon ^3 \zeta (3)+1$$

There are over 100 more special expansions of ${}_2 F_1$ tabulated in `Series2.m`. The interested user can consult the source code (search for HYPERLIST).
