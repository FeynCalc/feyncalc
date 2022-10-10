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

```mathematica
exp = ((L2*(-5 + nc)*(1 + nc)*(-32*nc - 32*nc^2))/nc^3 + (L1*(1 + nc)*(672*nc + 256*nc^2 + 
          32*nc^3 - 40*nc^2*NH - 80*nc^2*NL - 80*nc^2*NV))/(3*nc^3) + ((1 + nc)*(14544*nc + 
          7872*nc^2 - 1440*nc^3 - 1216*nc^2*NH - 416*nc^2*NL - 416*nc^2*NV - 192*Sqrt[3]*nc*Pi + 
          240*Sqrt[3]*nc^2*NH*Pi - 384*nc^3*Pi^2 - 1440*nc^2*NV*z))/(36*nc^3) + 
      ((1 + nc)*(14544*nc + 7872*nc^2 - 1440*nc^3 - 1216*nc^2*NH - 416*nc^2*NL - 
           416*nc^2*NV - 192*Sqrt[3]*nc*Pi + 240*Sqrt[3]*nc^2*NH*Pi - 384*nc^3*Pi^2 + 
           11520*nc*z + 15984*nc^2*z + 3312*nc^3*z - 1440*nc^2*NL*z - 2880*nc^2*NV*z - 
           768*nc^3*Pi^2*z))/(36*nc^3))/2
```

$$\frac{1}{2} \left(\frac{\text{L1} (\text{nc}+1) \left(32 \;\text{nc}^3-40 \;\text{nc}^2 \;\text{NH}-80 \;\text{nc}^2 \;\text{NL}-80 \;\text{nc}^2 \;\text{NV}+256 \;\text{nc}^2+672 \;\text{nc}\right)}{3 \;\text{nc}^3}+\frac{\text{L2} (\text{nc}-5) (\text{nc}+1) \left(-32 \;\text{nc}^2-32 \;\text{nc}\right)}{\text{nc}^3}+\frac{1}{36 \;\text{nc}^3}(\text{nc}+1) \left(-384 \pi ^2 \;\text{nc}^3-1440 \;\text{nc}^3-1216 \;\text{nc}^2 \;\text{NH}+240 \sqrt{3} \pi  \;\text{nc}^2 \;\text{NH}-416 \;\text{nc}^2 \;\text{NL}-1440 \;\text{nc}^2 \;\text{NV} z-416 \;\text{nc}^2 \;\text{NV}+7872 \;\text{nc}^2-192 \sqrt{3} \pi  \;\text{nc}+14544 \;\text{nc}\right)+\frac{1}{36 \;\text{nc}^3}(\text{nc}+1) \left(-768 \pi ^2 \;\text{nc}^3 z+3312 \;\text{nc}^3 z-384 \pi ^2 \;\text{nc}^3-1440 \;\text{nc}^3-1216 \;\text{nc}^2 \;\text{NH}+240 \sqrt{3} \pi  \;\text{nc}^2 \;\text{NH}-1440 \;\text{nc}^2 \;\text{NL} z-416 \;\text{nc}^2 \;\text{NL}-2880 \;\text{nc}^2 \;\text{NV} z-416 \;\text{nc}^2 \;\text{NV}+15984 \;\text{nc}^2 z+7872 \;\text{nc}^2+11520 \;\text{nc} z-192 \sqrt{3} \pi  \;\text{nc}+14544 \;\text{nc}\right)\right)$$

Split into pieces that depend on `L1`, `L2` and those then don' t . Then collect terms in the first group w.r.t `L1, L2` . Collect terms in the second group `w.r.t. `z` . Use `ExpandAll` as the factoring function in both groups . Sort the resulting terms in the first group such, that terms containing `L1` come first, then those with `L2` and finally all the rest . Put terms that depend on `z` in the second group first .

```mathematica
out1 = FCToTeXReorder[exp, {{L1, L2}, {L1, L2}, {z}}, Split -> True, Factoring -> {Function[x, 
      ExpandAll[x]], Function[x, ExpandAll[x]]}, SortBy -> {Function[x, Which[! FreeQ2[x, {L1}], 1, 
       ! FreeQ2[x, {L2}], 2, True, 30]], Function[x, Which[! FreeQ2[x, {z}], 1, True, 3]]}]
```

$$\left\{\left\{\left\{\frac{112}{\text{nc}^2}-\frac{20 \;\text{NH}}{3 \;\text{nc}}-\frac{40 \;\text{NL}}{3 \;\text{nc}}-\frac{40 \;\text{NV}}{3 \;\text{nc}}+\frac{16 \;\text{nc}}{3}+\frac{464}{3 \;\text{nc}}-\frac{20 \;\text{NH}}{3}-\frac{40 \;\text{NL}}{3}-\frac{40 \;\text{NV}}{3}+48,\text{L1},\text{Times}\right\},\left\{\frac{80}{\text{nc}^2}-16 \;\text{nc}+\frac{144}{\text{nc}}+48,\text{L2},\text{Times}\right\},\text{Plus}\right\},\left\{\left\{\frac{160}{\text{nc}^2}-\frac{20 \;\text{NL}}{\text{nc}}-\frac{60 \;\text{NV}}{\text{nc}}-\frac{32 \pi ^2 \;\text{nc}}{3}+46 \;\text{nc}+\frac{382}{\text{nc}}-20 \;\text{NL}-60 \;\text{NV}-\frac{32 \pi ^2}{3}+268,z,\text{Times}\right\},-\frac{16 \pi }{\sqrt{3} \;\text{nc}^2}+\frac{404}{\text{nc}^2}-\frac{304 \;\text{NH}}{9 \;\text{nc}}+\frac{20 \pi  \;\text{NH}}{\sqrt{3} \;\text{nc}}-\frac{104 \;\text{NL}}{9 \;\text{nc}}-\frac{104 \;\text{NV}}{9 \;\text{nc}}-\frac{32 \pi ^2 \;\text{nc}}{3}-40 \;\text{nc}-\frac{16 \pi }{\sqrt{3} \;\text{nc}}+\frac{1868}{3 \;\text{nc}}-\frac{304 \;\text{NH}}{9}+\frac{20 \pi  \;\text{NH}}{\sqrt{3}}-\frac{104 \;\text{NL}}{9}-\frac{104 \;\text{NV}}{9}-\frac{32 \pi ^2}{3}+\frac{536}{3},\text{Plus}\right\},\text{Plus}\right\}$$

Now work with the innermost brackets and put terms that contain `z` first . All the other terms should be sorted, such that `NH`, `NV` and `NL` terms appear in this order.

```mathematica
out2 = FCToTeXReorder[out1, {{}, {}, {}}, Split -> False, Factoring -> {Function[x, ExpandAll[x]], 
     Function[x, ExpandAll[x]]}, SortBy -> {Function[x, Which[! FreeQ2[x, {z}], 1, ! FreeQ2[x, {NH}], 
       2, ! FreeQ2[x, {NV}], 3, ! FreeQ2[x, {NL}], 4, True, 5]], Function[x, Which[! FreeQ2[x, {z}], 
       1, ! FreeQ2[x, {NH}], 2, ! FreeQ2[x, {NV}], 3, ! FreeQ2[x, {NL}], 4, True, 5]]}]
```

$$\left\{\left\{\left\{\left\{-\frac{20 \;\text{NH}}{3},-\frac{20 \;\text{NH}}{3 \;\text{nc}},-\frac{40 \;\text{NV}}{3},-\frac{40 \;\text{NV}}{3 \;\text{nc}},-\frac{40 \;\text{NL}}{3},-\frac{40 \;\text{NL}}{3 \;\text{nc}},48,\frac{112}{\text{nc}^2},\frac{464}{3 \;\text{nc}},\frac{16 \;\text{nc}}{3},\text{Plus}\right\},\text{L1},\text{Times}\right\},\left\{\left\{48,\frac{80}{\text{nc}^2},\frac{144}{\text{nc}},-16 \;\text{nc},\text{Plus}\right\},\text{L2},\text{Times}\right\},\text{Plus}\right\},\left\{\left\{\left\{-60 \;\text{NV},-\frac{60 \;\text{NV}}{\text{nc}},-20 \;\text{NL},-\frac{20 \;\text{NL}}{\text{nc}},268,\frac{160}{\text{nc}^2},\frac{382}{\text{nc}},46 \;\text{nc},-\frac{32 \pi ^2}{3},-\frac{32 \pi ^2 \;\text{nc}}{3},\text{Plus}\right\},z,\text{Times}\right\},\left\{-\frac{304 \;\text{NH}}{9},-\frac{304 \;\text{NH}}{9 \;\text{nc}},\frac{20 \pi  \;\text{NH}}{\sqrt{3}},\frac{20 \pi  \;\text{NH}}{\sqrt{3} \;\text{nc}},-\frac{104 \;\text{NV}}{9},-\frac{104 \;\text{NV}}{9 \;\text{nc}},-\frac{104 \;\text{NL}}{9},-\frac{104 \;\text{NL}}{9 \;\text{nc}},\frac{536}{3},\frac{404}{\text{nc}^2},\frac{1868}{3 \;\text{nc}},-40 \;\text{nc},-\frac{16 \pi }{\sqrt{3} \;\text{nc}^2},-\frac{16 \pi }{\sqrt{3} \;\text{nc}},-\frac{32 \pi ^2}{3},-\frac{32 \pi ^2 \;\text{nc}}{3},\text{Plus}\right\},\text{Plus}\right\},\text{Plus}\right\}$$

```mathematica
FCToTeXPreviewTermOrder[out2]
```

$$\left(\left(-\frac{20 \;\text{NH}}{3}-\frac{20 \;\text{NH}}{3 \;\text{nc}}-\frac{40 \;\text{NV}}{3}-\frac{40 \;\text{NV}}{3 \;\text{nc}}-\frac{40 \;\text{NL}}{3}-\frac{40 \;\text{NL}}{3 \;\text{nc}}+48+\frac{112}{\text{nc}^2}+\frac{464}{3 \;\text{nc}}+\frac{16 \;\text{nc}}{3}\right) \;\text{L1}+\left(48+\frac{80}{\text{nc}^2}+\frac{144}{\text{nc}}-16 \;\text{nc}\right) \;\text{L2}\right)+\left(\left(-60 \;\text{NV}-\frac{60 \;\text{NV}}{\text{nc}}-20 \;\text{NL}-\frac{20 \;\text{NL}}{\text{nc}}+268+\frac{160}{\text{nc}^2}+\frac{382}{\text{nc}}+46 \;\text{nc}-\frac{32 \pi ^2}{3}-\frac{32 \;\text{nc} \pi ^2}{3}\right) z+\left(-\frac{304 \;\text{NH}}{9}-\frac{304 \;\text{NH}}{9 \;\text{nc}}+\frac{20 \;\text{NH} \pi }{\sqrt{3}}+\frac{20 \;\text{NH} \pi }{\sqrt{3} \;\text{nc}}-\frac{104 \;\text{NV}}{9}-\frac{104 \;\text{NV}}{9 \;\text{nc}}-\frac{104 \;\text{NL}}{9}-\frac{104 \;\text{NL}}{9 \;\text{nc}}+\frac{536}{3}+\frac{404}{\text{nc}^2}+\frac{1868}{3 \;\text{nc}}-40 \;\text{nc}-\frac{16 \pi }{\sqrt{3} \;\text{nc}^2}-\frac{16 \pi }{\sqrt{3} \;\text{nc}}-\frac{32 \pi ^2}{3}-\frac{32 \;\text{nc} \pi ^2}{3}\right)\right)$$