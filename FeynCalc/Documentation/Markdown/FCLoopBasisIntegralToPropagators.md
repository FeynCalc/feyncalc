## FCLoopBasisIntegralToPropagators

`FCLoopBasisIntegralToPropagators[int, {q1, q2, ...}]` is an auxiliary function that converts the loop integral `int` that depends on the loop momenta `q1, q2, ...` to a list of propagators and scalar products. 

All propagators and scalar products that do not depend on the loop momenta are discarded, unless the `Rest` option is set to `True`.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
SFAD[p1] 
 
FCLoopBasisIntegralToPropagators[%, {p1}]
```

$$\frac{1}{(\text{p1}^2+i \eta )}$$

$$\left\{\frac{1}{(\text{p1}^2+i \eta )}\right\}$$

```mathematica
SFAD[p1, p2] 
 
FCLoopBasisIntegralToPropagators[%, {p1, p2}]
```

$$\frac{1}{(\text{p1}^2+i \eta ).(\text{p2}^2+i \eta )}$$

$$\left\{\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )}\right\}$$

If the integral contains propagators raised to integer powers, only one propagator will appear in the output.

```mathematica
int = SPD[q, p] SFAD[q, q - p, q - p]
```

$$\frac{p\cdot q}{(q^2+i \eta ).((q-p)^2+i \eta )^2}$$

```mathematica
FCLoopBasisIntegralToPropagators[int, {q}]
```

$$\left\{(p\cdot q+i \eta ),\frac{1}{(q^2+i \eta )},\frac{1}{((q-p)^2+i \eta )}\right\}$$

However, setting the option `Tally` to `True` will count the powers of the appearing propagators.

```mathematica
FCLoopBasisIntegralToPropagators[int, {q}, Tally -> True]
```

$$\left(
\begin{array}{cc}
 \frac{1}{(q^2+i \eta )} & 1 \\
 (p\cdot q+i \eta ) & 1 \\
 \frac{1}{((q-p)^2+i \eta )} & 2 \\
\end{array}
\right)$$

Here is a more realistic 3-loop example

```mathematica
int = SFAD[{{-k1, 0}, {mc^2, 1}, 1}] SFAD[{{-k1 - k2, 0}, {mc^2, 1}, 1}] SFAD[{{-k2, 0}, {0, 1}, 1}] SFAD[{{-k2, 0}, {0, 1}, 2}] SFAD[{{-k3, 0}, {mc^2, 1}, 1}] *SFAD[{{k1 - k3 - p1, 0}, {0, 1}, 1}] SFAD[{{-k1 - k2 + k3 + p1, 0}, {0, 1}, 1}] SFAD[{{-k1 - k2 + k3 + p1, 0}, {0, 1}, 2}]
```

$$\frac{1}{(\text{k2}^2+i \eta )^3 (\text{k1}^2-\text{mc}^2+i \eta ) (\text{k3}^2-\text{mc}^2+i \eta ) ((-\text{k1}-\text{k2})^2-\text{mc}^2+i \eta ) ((\text{k1}-\text{k3}-\text{p1})^2+i \eta ) ((-\text{k1}-\text{k2}+\text{k3}+\text{p1})^2+i \eta )^3}$$

```mathematica
FCLoopBasisIntegralToPropagators[int, {k1, k2, k3}, Tally -> True]
```

$$\left(
\begin{array}{cc}
 \frac{1}{(\text{k2}^2+i \eta )} & 3 \\
 \frac{1}{(\text{k3}^2-\text{mc}^2+i \eta )} & 1 \\
 \frac{1}{(\text{k1}^2-\text{mc}^2+i \eta )} & 1 \\
 \frac{1}{((\text{k1}-\text{k3}-\text{p1})^2+i \eta )} & 1 \\
 \frac{1}{((-\text{k1}-\text{k2}+\text{k3}+\text{p1})^2+i \eta )} & 3 \\
 \frac{1}{((-\text{k1}-\text{k2})^2-\text{mc}^2+i \eta )} & 1 \\
\end{array}
\right)$$