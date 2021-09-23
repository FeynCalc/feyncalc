## Expand2

`Expand2[exp, x]` expands all sums containing `x`.

`Expand2[exp, {x1, x2, ...}]`  expands all sums containing `x1, x2, ...`.

### See also

[Overview](Extra/FeynCalc.md), [ExpandAll2](ExpandAll2.md).

### Examples

```mathematica
Expand2[(x1 + x2 + x3) (2 x1 + 3 x2) + (y1 + y2 + y3) (2 y1 + 3 y2)]
```

$$2 \;\text{x1}^2+5 \;\text{x1} \;\text{x2}+2 \;\text{x1} \;\text{x3}+3 \;\text{x2}^2+3 \;\text{x2} \;\text{x3}+2 \;\text{y1}^2+5 \;\text{y1} \;\text{y2}+2 \;\text{y1} \;\text{y3}+3 \;\text{y2}^2+3 \;\text{y2} \;\text{y3}$$

```mathematica
Expand2[(x1 + x2 + x3) (2 x1 + 3 x2) + (y1 + y2 + y3) (2 y1 + 3 y2), {y1, y2}]
```

$$(2 \;\text{x1}+3 \;\text{x2}) (\text{x1}+\text{x2}+\text{x3})+2 \;\text{y1}^2+5 \;\text{y1} \;\text{y2}+2 \;\text{y1} \;\text{y3}+3 \;\text{y2}^2+3 \;\text{y2} \;\text{y3}$$

```mathematica
Expand2[(x1 + x2 + x3) (2 x1 + 3 x2) + (y1 + y2 + y3) (2 y1 + 3 y2), {x1, x2}]
```

$$2 \;\text{x1}^2+5 \;\text{x1} \;\text{x2}+2 \;\text{x1} \;\text{x3}+3 \;\text{x2}^2+3 \;\text{x2} \;\text{x3}+(2 \;\text{y1}+3 \;\text{y2}) (\text{y1}+\text{y2}+\text{y3})$$
