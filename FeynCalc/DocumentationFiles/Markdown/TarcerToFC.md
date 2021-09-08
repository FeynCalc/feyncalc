## TarcerToFC

`TarcerToFC[expr, {q1, q2}]` translates loop integrals in Tarcer-notation to the FeynCalc notation.

See `TFI` for details on the convention.

As in the case of `ToTFI`, the \frac{1}{\pi^D} and \frac{1}{\pi^{D/2}} prefactors are implicit, i.e. `TarcerToFC` doesn't add them.

To recover momenta from scalar products use the option `ScalarProduct` e.g. as in `TarcerToFC[TBI[D, pp^2, {{1, 0}, {1, 0}}], {q1, q2}, ScalarProduct -> {{pp^2, p1}}]`

### See also

[Overview](Extra/FeynCalc.md), [ToFI](ToFI.md).

### Examples

```mathematica
Tarcer`TFI[D, Pair[Momentum[p, D], Momentum[p, D]], {0, 0, 3, 2, 0}, 
  {{4, 0}, {2, 0}, {1, 0}, {0, 0}, {1, 0}}]
```

$$\text{Tarcer$\grave{ }$TFI}\left(D,p^2,\{0,0,3,2,0\},\left(
\begin{array}{cc}
 4 & 0 \\
 2 & 0 \\
 1 & 0 \\
 0 & 0 \\
 1 & 0 \\
\end{array}
\right)\right)$$

```mathematica
TarcerToFC[%, {q1, q2}]
```

$$\frac{(p\cdot \;\text{q1})^3 (p\cdot \;\text{q2})^2}{\left(\text{q1}^2\right)^4.\left(\text{q2}^2\right)^2.(\text{q1}-p)^2.(\text{q1}-\text{q2})^2}$$

```mathematica
a1 Tarcer`TBI[D, pp^2, {{1, 0}, {1, 0}}] + b1 Tarcer`TBI[D, mm1, {{1, 0}, {1, 0}}]
```

$$\text{a1} \;\text{Tarcer$\grave{ }$TBI}\left(D,\text{pp}^2,\left(
\begin{array}{cc}
 1 & 0 \\
 1 & 0 \\
\end{array}
\right)\right)+\text{b1} \;\text{Tarcer$\grave{ }$TBI}\left(D,\text{mm1},\left(
\begin{array}{cc}
 1 & 0 \\
 1 & 0 \\
\end{array}
\right)\right)$$

```mathematica
TarcerToFC[%, {q1, q2}, ScalarProduct -> {{pp^2, p1}, {mm1, p1}}, FCE -> True]
```

$$\frac{\text{a1}}{\text{q1}^2.(\text{q1}-\text{p1})^2}+\frac{\text{b1}}{\text{q1}^2.(\text{q1}-\text{p1})^2}$$
