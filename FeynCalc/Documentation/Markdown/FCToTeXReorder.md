## FCToTeXReorder

`FCToTeXReorder[exp, {{v1, v2, ... }, {a1, a2, ... }, {b1, b2, ... }}]` is an auxiliary function that helps to bring the given Mathematica expression `exp` into a form suitable for being inserted into a LaTeX document.

To override the built-in ordering of `Plus` and `Times`, the expression is converted into a nested list made of elements of the form `{a, b, ... , Plus}` or `{a, b, ... ,Times}` for a sum or a product respectively.

Then, the option `SortBy` allows to specify two sorting functions that will be used to reorder the terms in both groups.

Most importantly, `FCToTeXReorder` can be  applied to the output of a previous function call. This allows for arbitrarily deep nesting.

Finally, you can check if the final result satisfies your expectations by using `FCToTeXPreviewTermOrder`.

### See also

[Overview](Extra/FeynCalc.md), [FCToTeXPreviewTermOrder](FCToTeXPreviewTermOrder.md).

### Examples

```mathematica
exp = (-13629 - 4452*L1 + 24*L2 + 380*NH + 75*L1*NH + 130*NL + 150*L1*NL + 
      130*NV + 150*L1*NV + 20*Sqrt[3]*Pi - 75*Sqrt[3]*NH*Pi + 360*Pi^2 + 66300*z + 
      20628*L1*z + 648*L2*z + 450*NL*z + 900*NV*z + 72*Pi^2*z + 2592*z*Log[z])/81;
```

```mathematica
aux1 = FCToTeXReorder[exp, {{z}, {Log, L1, L2}, {Log, L1, L2}}]
```

$$\left\{\left\{\frac{2}{27} z \left(75 \;\text{NL}+150 \;\text{NV}+12 \pi ^2+11050\right),\{8 z,\text{L2},\text{Times}\},\{32 z,\log (z),\text{Times}\},\left\{\frac{764 z}{3},\text{L1},\text{Times}\right\},\text{Plus}\right\},\left\{\frac{1}{81} \left(-75 \sqrt{3} \pi  \;\text{NH}+380 \;\text{NH}+130 \;\text{NL}+130 \;\text{NV}+360 \pi ^2+20 \sqrt{3} \pi -13629\right),\left\{\frac{8}{27},\text{L2},\text{Times}\right\},\left\{\frac{1}{27} (25 \;\text{NH}+50 \;\text{NL}+50 \;\text{NV}-1484),\text{L1},\text{Times}\right\},\text{Plus}\right\},\text{Plus}\right\}$$

```mathematica
aux1 // FCToTeXPreviewTermOrder
```

$$\left(\frac{2}{27} \left(11050+75 \;\text{NL}+150 \;\text{NV}+12 \pi ^2\right) z+8 z \;\text{L2}+32 z \log (z)+\frac{764 z \;\text{L1}}{3}\right)+\left(\frac{1}{81} \left(-13629+380 \;\text{NH}+130 \;\text{NL}+130 \;\text{NV}+20 \sqrt{3} \pi -75 \sqrt{3} \;\text{NH} \pi +360 \pi ^2\right)+\frac{8 \;\text{L2}}{27}+\frac{1}{27} (-1484+25 \;\text{NH}+50 \;\text{NL}+50 \;\text{NV}) \;\text{L1}\right)$$

```mathematica
aux1 // InputForm
```

```mathematica
{{(2*(11050 + 75*NL + 150*NV + 12*Pi^2)*z)/27, {8*z, L2, Times}, 
  {32*z, Log[z], Times}, {(764*z)/3, L1, Times}, Plus}, 
 {(-13629 + 380*NH + 130*NL + 130*NV + 20*Sqrt[3]*Pi - 
    75*Sqrt[3]*NH*Pi + 360*Pi^2)/81, {8/27, L2, Times}, 
  {(-1484 + 25*NH + 50*NL + 50*NV)/27, L1, Times}, Plus}, Plus}
```

```mathematica
res = FCToTeXReorder[aux1, {{L1, L2}, {NH, NV, NL}, {NH, NV, NL}}]
```

$$\left\{\left\{\left\{\frac{4}{27} \left(5525+6 \pi ^2\right) z,\left\{\frac{50 z}{9},\text{NL},\text{Times}\right\},\left\{\frac{100 z}{9},\text{NV},\text{Times}\right\},\text{Plus}\right\},\{8 z,\text{L2},\text{Times}\},\{32 z,\log (z),\text{Times}\},\left\{\frac{764 z}{3},\text{L1},\text{Times}\right\},\text{Plus}\right\},\left\{\left\{\frac{1}{81} \left(-13629+20 \sqrt{3} \pi +360 \pi ^2\right),\left\{\frac{130}{81},\text{NL},\text{Times}\right\},\left\{\frac{130}{81},\text{NV},\text{Times}\right\},\left\{\frac{5}{81} \left(76-15 \sqrt{3} \pi \right),\text{NH},\text{Times}\right\},\text{Plus}\right\},\left\{\frac{8}{27},\text{L2},\text{Times}\right\},\left\{\left\{-\frac{1484}{27},\left\{\frac{25}{27},\text{NH},\text{Times}\right\},\left\{\frac{50}{27},\text{NL},\text{Times}\right\},\left\{\frac{50}{27},\text{NV},\text{Times}\right\},\text{Plus}\right\},\text{L1},\text{Times}\right\},\text{Plus}\right\},\text{Plus}\right\}$$

```mathematica
res // FCToTeXPreviewTermOrder
```

$$\left(\left(\frac{4}{27} \left(5525+6 \pi ^2\right) z+\frac{50 z \;\text{NL}}{9}+\frac{100 z \;\text{NV}}{9}\right)+8 z \;\text{L2}+32 z \log (z)+\frac{764 z \;\text{L1}}{3}\right)+\left(\left(\frac{1}{81} \left(-13629+20 \sqrt{3} \pi +360 \pi ^2\right)+\frac{130 \;\text{NL}}{81}+\frac{130 \;\text{NV}}{81}+\frac{5}{81} \left(76-15 \sqrt{3} \pi \right) \;\text{NH}\right)+\frac{8 \;\text{L2}}{27}+\left(-\frac{1484}{27}+\frac{25 \;\text{NH}}{27}+\frac{50 \;\text{NL}}{27}+\frac{50 \;\text{NV}}{27}\right) \;\text{L1}\right)$$