## Factor3

`Factor3[exp]` factors a rational function `exp` over the field of complex numbers.

`Factor3` is primarily meant to be used on matrices from differential equations and Feynman parametric
representations of loop integrals. Its main goal is to rewrite all denominators such, that they can be integrated in terms of HPLs or GPLs (when possible).

To avoid performance bottlenecks, in the case of ration functions only the denominator will be factored by default. This can be changed by setting the option `Numerator` to `True`.

### See also

[Overview](Extra/FeynCalc.md), [FCIntegratePolyLogs](FCIntegratePolyLogs.md).

### Examples

```mathematica
Factor3[(1 - 4 x) (1 + 3 y)]
```

$$-12 \left(x-\frac{1}{4}\right) \left(y+\frac{1}{3}\right)$$

```mathematica
Factor3[16*(1 - 2*eps)^2*x^2]
```

$$64 \left(\text{eps}-\frac{1}{2}\right)^2 x^2$$

```mathematica
Factor3[2*(32904490323 + 164521613783*eps + 1256744*eps^2)*(11 - 5*eps - 47*eps^2 + 44*eps^3)]
```

$$110593472 \left(\text{eps}+\frac{1}{264} \left(1-i \sqrt{3}\right) \sqrt[3]{-137143+198 i \sqrt{122615}}+\frac{2869 \left(1+i \sqrt{3}\right)}{264 \sqrt[3]{-137143+198 i \sqrt{122615}}}-\frac{47}{132}\right) \left(\text{eps}+\frac{1}{264} \left(1+i \sqrt{3}\right) \sqrt[3]{-137143+198 i \sqrt{122615}}+\frac{2869 \left(1-i \sqrt{3}\right)}{264 \sqrt[3]{-137143+198 i \sqrt{122615}}}-\frac{47}{132}\right) \left(\text{eps}-\frac{628374}{-1570927-\sqrt{2467796558401}}\right) \left(\text{eps}-\frac{104729 \left(-1570927-\sqrt{2467796558401}\right)}{2513488}\right) \left(\text{eps}+\frac{1}{132} \left(-47-\frac{2869}{\sqrt[3]{-137143+198 i \sqrt{122615}}}-\sqrt[3]{-137143+198 i \sqrt{122615}}\right)\right)$$

```mathematica
mat = {{(2 - 2*eps)/x, 0, 0, 0, 0}, {0, (2 - 2*eps)/(2*x), 0, 0, 0}, 
    {0, (-2 + 2*eps)/(x - 4*x^2), (6 - 2*(4 - 2*eps))/(1 - 4*x), 0, 0}, 
    {(-2 + 2*eps)/(x - 4*x^2), 0, 0, (2 - 2*eps + 4*(5 - 2*(4 - 2*eps))*x)/(2*(1 - 
          4*x)*x), 0},  {(2 - 2*eps)^2/(16*(1 - x)*x^2), -1/8*(2 - 2*eps)^2/((1 - x)*x^2), 
     0, 0, -((7 - 2*(4 - 2*eps) - 13*x + 4*(4 - 2*eps)*x)/(2*x - 2*x^2))}};
```

```mathematica
Factor3[mat]
```

$$\left(
\begin{array}{ccccc}
 \frac{2-2 \;\text{eps}}{x} & 0 & 0 & 0 & 0 \\
 0 & \frac{2-2 \;\text{eps}}{2 x} & 0 & 0 & 0 \\
 0 & -\frac{2 \;\text{eps}-2}{4 \left(x-\frac{1}{4}\right) x} & -\frac{6-2 (4-2 \;\text{eps})}{4 \left(x-\frac{1}{4}\right)} & 0 & 0 \\
 -\frac{2 \;\text{eps}-2}{4 \left(x-\frac{1}{4}\right) x} & 0 & 0 & -\frac{4 (5-2 (4-2 \;\text{eps})) x-2 \;\text{eps}+2}{8 \left(x-\frac{1}{4}\right) x} & 0 \\
 -\frac{(2-2 \;\text{eps})^2}{16 (x-1) x^2} & \frac{(2-2 \;\text{eps})^2}{8 (x-1) x^2} & 0 & 0 & -\frac{-4 (4-2 \;\text{eps}) x+2 (4-2 \;\text{eps})+13 x-7}{2 (x-1) x} \\
\end{array}
\right)$$