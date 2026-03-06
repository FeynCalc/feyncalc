## CompleteSquare

`CompleteSquarep[exp, x]` completes the square of a second order polynomial in the momentum x.

### See also

[Overview](Extra/FeynCalc.md), [ExpandScalarProduct](ExpandScalarProduct.md).

### Examples

```mathematica
CompleteSquare[4 SP[p] + SP[b, p] + c, p]
```

$$4 (\frac{\overline{b}}{8}+\overline{p})^2-\frac{\overline{b}^2}{16}+c$$

```mathematica
CompleteSquare[4 SP[p] + SP[b, p] + c, p, q]
```

$$\left\{-\frac{\overline{b}^2}{16}+4 \overline{q}^2+c,\overline{q}\to \frac{\overline{b}}{8}+\overline{p}\right\}$$

```mathematica
ex1 = 5 SP[2 p + 3 r, p + r]
```

$$5 \left((\overline{p}+\overline{r})\cdot (2 \overline{p}+3 \overline{r})\right)$$

```mathematica
ex2 = CompleteSquare[ex1, p]
```

$$10 (\overline{p}+\frac{5 \overline{r}}{4})^2-\frac{5 \overline{r}^2}{8}$$

```mathematica
ex1 - ex2 // ScalarProductExpand // Expand
```

$$0$$

```mathematica
CompleteSquare[5 SP[2 p + 3 r, p + r], p, q]
```

$$\left\{10 \overline{q}^2-\frac{5 \overline{r}^2}{8},\overline{q}\to \overline{p}+\frac{5 \overline{r}}{4}\right\}$$

```mathematica
SPD[a] + 2 SPD[a, b] 
 
ex = CompleteSquare[%, a]
```

$$2 (a\cdot b)+a^2$$

$$(a+b)^2-b^2$$

```mathematica
ex // StandardForm

(*-Pair[Momentum[b, D], Momentum[b, D]] + Pair[Momentum[a, D] + Momentum[b, D], Momentum[a, D] + Momentum[b, D]]*)
```