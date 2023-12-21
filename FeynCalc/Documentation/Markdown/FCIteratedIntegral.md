```mathematica
 
```

## FCIteratedIntegral

`FCIteratedIntegral[f,x,a,b]` is a special head indicating that the function $f$ represents an iterated integral or a linear combination thereof and that it should be integrated in $x$ from $a$ to $b$. This notation is understood by the function `FCIteratedIntegralEvaluate` that does the actual integration.

Notice that before applying `FCIteratedIntegralEvaluate` all rational functions of $x$ in $f$ should be converted to the `FCPartialFractionForm`representation.

### See also

[Overview](Extra/FeynCalc.md), [FCIteratedIntegralEvaluate](FCIteratedIntegralEvaluate.md), [ToFCPartialFractionForm](ToFCPartialFractionForm.md)

### Examples

```mathematica
fun = 1/(1 + x)
```

$$\frac{1}{x+1}$$

```mathematica
int = FCIteratedIntegral[ToFCPartialFractionForm[fun, x], x, a, b]
```

$$\text{FCIteratedIntegral}\left(\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \{x+1,-1\} & 1 \\
\end{array}
\right),x\right),x,a,b\right)$$

```mathematica
FCIteratedIntegralEvaluate[int]
```

$$G(-1; b)-G(-1; a)$$