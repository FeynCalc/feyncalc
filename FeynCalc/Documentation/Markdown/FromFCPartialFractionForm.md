```mathematica
 
```

## FromFCPartialFractionForm

`FromFCPartialFractionForm[exp]` converts all `FCPartialFractionForm` symbols present in `exp` back into the standard representation.

### See also

[Overview](Extra/FeynCalc.md), [ToFCPartialFractionForm](ToFCPartialFractionForm.md), [FCPartialFractionForm](FCPartialFractionForm.md).

### Examples

```mathematica
FromFCPartialFractionForm[FCPartialFractionForm[x, {}, x]]
```

$$x$$

```mathematica
FromFCPartialFractionForm[FCPartialFractionForm[0, {{{x - 1, -2}, 1}}, x]]
```

$$\frac{1}{(x-1)^2}$$

```mathematica
FromFCPartialFractionForm[FCPartialFractionForm[0, {{{x + 1, -1}, 1}, {{x - y, -2}, c}}, x]]
```

$$\frac{c}{(x-y)^2}+\frac{1}{x+1}$$

```mathematica
FromFCPartialFractionForm[FCPartialFractionForm[0, {{{x + 1, -1}, 1}, {{x - y, -2}, c}}, x], Factoring -> Together]
```

$$\frac{c x+c+x^2-2 x y+y^2}{(x+1) (x-y)^2}$$