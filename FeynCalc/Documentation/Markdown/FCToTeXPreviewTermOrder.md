## FCToTeXPreviewTermOrder

`FCToTeXPreviewTermOrder[exp]` displays the output of `FCToTeXReorder` using the built-in Plus and Times but preserving the original ordering.

Use `ReleaseHold` or `FRH` to allow Mathematica return to its original ordering.

Notice that the output of `FCToTeXPreviewTermOrder` is not suitable for algebraic manipulations but should be understood as an intermediate expression form created to serve as an input for `TeXForm`

### See also

[Overview](Extra/FeynCalc.md), [FCToTeXReorder](FCToTeXReorder.md).

### Examples

```mathematica
ex = {(2*z*(11050 + 3438*L1 + 108*L2 + 75*NL + 150*NV + 12*Pi^2 + 432*Log[z]))/27, 
   (-13629 - 4452*L1 + 24*L2 + 380*NH + 75*L1*NH + 130*NL + 150*L1*NL + 130*NV + 
      150*L1*NV + 20*Sqrt[3]*Pi - 75*Sqrt[3]*NH*Pi + 360*Pi^2)/81, Plus}
```

$$\left\{\frac{2}{27} z \left(3438 \;\text{L1}+108 \;\text{L2}+75 \;\text{NL}+150 \;\text{NV}+432 \log (z)+12 \pi ^2+11050\right),\frac{1}{81} \left(75 \;\text{L1} \;\text{NH}+150 \;\text{L1} \;\text{NL}+150 \;\text{L1} \;\text{NV}-4452 \;\text{L1}+24 \;\text{L2}+380 \;\text{NH}-75 \sqrt{3} \pi  \;\text{NH}+130 \;\text{NL}+130 \;\text{NV}+360 \pi ^2+20 \sqrt{3} \pi -13629\right),\text{Plus}\right\}$$

```mathematica
FCToTeXPreviewTermOrder[ex]
```

$$\frac{2}{27} z \left(11050+3438 \;\text{L1}+108 \;\text{L2}+75 \;\text{NL}+150 \;\text{NV}+12 \pi ^2+432 \log (z)\right)+\frac{1}{81} \left(-13629-4452 \;\text{L1}+24 \;\text{L2}+380 \;\text{NH}+75 \;\text{L1} \;\text{NH}+130 \;\text{NL}+150 \;\text{L1} \;\text{NL}+130 \;\text{NV}+150 \;\text{L1} \;\text{NV}+20 \sqrt{3} \pi -75 \sqrt{3} \;\text{NH} \pi +360 \pi ^2\right)$$