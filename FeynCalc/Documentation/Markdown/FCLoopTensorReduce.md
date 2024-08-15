## FCLoopTensorReduce

`FCLoopTensorReduce[exp, topos]` performs tensor reduction for the numerators of multi-loop integrals present in `exp`. Notice that `exp` is expected to be the output of `FCLoopFindTopologies` where all loop integrals have been written as `fun[num, GLI[...]]` with `num` being the numerator to be acted upon.

The reduction is done only for loop momenta contracted with Dirac matrices, polarization vectors or Levi-Civita tensors. Scalar products with external momenta are left untouched. The goal is to rewrite everything in terms of scalar products involving only loop momenta and external momenta appearing in the given topology. These quantities can be then rewritten in terms of inverse propagators (`GLI`s with negative indices), so that the complete dependence on loop momenta will go into the `GLI`s.

Unlike `FCMultiLoopTID`, this function does not perform any partial fractioning or shifts in the loop momenta.

The default value for `fun` is  FCGV["GLIProduct"] set by the option `Head`

### See also

[Overview](Extra/FeynCalc.md), [FCLoopFindTopologies](FCLoopFindTopologies.md).

### Examples

1-loop tadpole topology

```mathematica
topo1 = FCTopology["tad1l", {SFAD[{q, m^2}]}, {q}, {}, {}, {}]
```

$$\text{FCTopology}\left(\text{tad1l},\left\{\frac{1}{(q^2-m^2+i \eta )}\right\},\{q\},\{\},\{\},\{\}\right)$$

```mathematica
amp1 = FCGV["GLIProduct"][GSD[q] . GAD[\[Mu]] . GSD[q], GLI["tad1l", {1}]]
```

$$\text{FCGV}(\text{GLIProduct})\left((\gamma \cdot q).\gamma ^{\mu }.(\gamma \cdot q),G^{\text{tad1l}}(1)\right)$$

```mathematica
amp1Red = FCLoopTensorReduce[amp1, {topo1}]
```

$$\text{FCGV}(\text{GLIProduct})\left(\frac{(2-D) q^2 \gamma ^{\mu }}{D},G^{\text{tad1l}}(1)\right)$$

```mathematica
topo2 = FCTopology[prop1l, {SFAD[{q, m^2}, {q - p, m^2}]}, {q}, {p}, {}, {}]
```

$$\text{FCTopology}\left(\text{prop1l},\left\{\frac{1}{(q^2-m^2+i \eta ).((q-p)^2-m^2+i \eta )}\right\},\{q\},\{p\},\{\},\{\}\right)$$

1-loop self-energy topology

```mathematica
amp2 = gliProduct[GSD[q] . GAD[\[Mu]] . GSD[q], GLI[prop1l, {1, 2}]]
```

$$\text{gliProduct}\left((\gamma \cdot q).\gamma ^{\mu }.(\gamma \cdot q),G^{\text{prop1l}}(1,2)\right)$$

```mathematica
amp2Red = FCLoopTensorReduce[amp2, {topo2}, Head -> gliProduct]
```

$$\text{gliProduct}\left(-\frac{2 D p^{\mu } \gamma \cdot p (p\cdot q)^2-2 p^2 \gamma ^{\mu } (p\cdot q)^2-D p^4 q^2 \gamma ^{\mu }+3 p^4 q^2 \gamma ^{\mu }-2 p^2 q^2 p^{\mu } \gamma \cdot p}{(1-D) p^4},G^{\text{prop1l}}(1,2)\right)$$

If the loop momenta are contracted with some external momenta that do not appear in the given integral topologies,
they should be listed via the option `Uncontract`

```mathematica
amp3 = gliProduct[SPD[q, x], GLI[prop1l, {1, 2}]]
```

$$\text{gliProduct}\left(q\cdot x,G^{\text{prop1l}}(1,2)\right)$$

```mathematica
FCLoopTensorReduce[amp3, {topo2}, Uncontract -> {x}, Head -> gliProduct]
```

$$\text{gliProduct}\left(\frac{(p\cdot q) (p\cdot x)}{p^2},G^{\text{prop1l}}(1,2)\right)$$