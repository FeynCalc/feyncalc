## FCLoopAddAuxiliaryMass

`FCLoopAddAuxiliaryMass[expr, {k1, k2, ...},-m^2,n]` adds auxiliary mass term $m^2$ to the propagators in the list that depend on loop momenta `k1, k2, ...`. For $n=0$ the mass is added directly.

For $n>0$ the function applies the exact identity from [arXiv:hep-ph/9711266](https://arxiv.org/abs/hep-ph/9711266), known as  infrared rearrangement, $n$ times. The option `Last` allows to add a flag to the last term in the expression as a check that it does not contribute to the physical results.

### See also

[Overview](Extra/FeynCalc.md), [FeynAmpDenominatorCombine](FeynAmpDenominatorCombine.md).

### Examples

```mathematica
FCLoopAddAuxiliaryMass[{FAD[k + p]}, {k}, -M^2, 2]
```

$$\left\{\frac{1}{(k+p)^2}\to -\frac{M^2}{((k+p)^2-M^2+i \eta )^2}+\frac{1}{((k+p)^2-M^2+i \eta )}+\frac{M^4}{((k+p)^2+i \eta ).((k+p)^2-M^2+i \eta )^2}\right\}$$

```mathematica
FCLoopAddAuxiliaryMass[{denHead[FAD[k + p]]}, {k}, -M^2, 2, Head -> denHead]
```

$$\left\{\text{denHead}\left(\frac{1}{(k+p)^2}\right)\to -\frac{M^2}{((k+p)^2-M^2+i \eta )^2}+\frac{1}{((k+p)^2-M^2+i \eta )}+\frac{M^4}{((k+p)^2+i \eta ).((k+p)^2-M^2+i \eta )^2}\right\}$$

```mathematica
FCLoopAddAuxiliaryMass[{denHead[FAD[k + p]]}, {k}, -M^2, 2, Head -> denHead, "MassHead" -> auxM]
```

$$\left\{\text{denHead}\left(\frac{1}{(k+p)^2}\right)\to \frac{\text{auxM}\left(-M^2\right)^2}{((k+p)^2+i \eta ).((k+p)^2-M^2+i \eta )^2}+\frac{\text{auxM}\left(-M^2\right)}{((k+p)^2-M^2+i \eta )^2}+\frac{1}{((k+p)^2-M^2+i \eta )}\right\}$$

```mathematica
FCLoopAddAuxiliaryMass[{denHead[FAD[k + p]]}, {k}, -M^2, 3, Head -> denHead, "MassHead" -> auxM, Last -> flag]
```

$$\left\{\text{denHead}\left(\frac{1}{(k+p)^2}\right)\to \frac{\text{flag} \;\text{auxM}\left(-M^2\right)^3}{((k+p)^2+i \eta ).((k+p)^2-M^2+i \eta )^3}+\frac{\text{auxM}\left(-M^2\right)^2}{((k+p)^2-M^2+i \eta )^3}+\frac{\text{auxM}\left(-M^2\right)}{((k+p)^2-M^2+i \eta )^2}+\frac{1}{((k+p)^2-M^2+i \eta )}\right\}$$

```mathematica
FCLoopAddAuxiliaryMass[{denHead[FAD[{k + p, M}]]}, {k}, -M^2, 0, Head -> denHead]
```

$$\left\{\text{denHead}\left(\frac{1}{(k+p)^2-M^2}\right)\to \frac{1}{(k+p)^2-M^2}\right\}$$