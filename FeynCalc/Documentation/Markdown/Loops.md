## Loops

### See also

[Overview](Extra/FeynCalc.md).

### Propagators

All propagators (and products thereof) appearing in loop diagrams and integrals are represented via `FeynAmpDenominator`

This container is capable of representing different propagator types, where the familiar quadratic propagators of the form $1/(q^2 - m^2 + i \eta)$ are described using `PropagatorDenominator`

```mathematica
FeynAmpDenominator[PropagatorDenominator[Momentum[q, D], m]]
```

$$\frac{1}{q^2-m^2}$$

Again, for the external input we always use a shortcut

```mathematica
FAD[{q, m}]
```

$$\frac{1}{q^2-m^2}$$

```mathematica
FAD[{q, m0}, {q + p1, m1}, {q + p2, m1}]
```

$$\frac{1}{\left(q^2-\text{m0}^2\right).\left((\text{p1}+q)^2-\text{m1}^2\right).\left((\text{p2}+q)^2-\text{m1}^2\right)}$$

There is also a more versatile symbol called `StandardFeynAmpDenominator` or `SFAD` that allows entering eikonal propagators as well

```mathematica
SFAD[{{p, 0}, m^2}]
```

$$\frac{1}{(p^2-m^2+i \eta )}$$

```mathematica
SFAD[{{p, 0}, {-m^2, -1}}]
```

$$\frac{1}{(p^2+m^2-i \eta )}$$

```mathematica
SFAD[{{0, p . q}, m^2}]
```

$$\frac{1}{(p\cdot q-m^2+i \eta )}$$

```mathematica
SFAD[{{p, p . q}, m^2}]
```

$$\frac{1}{(p^2+p\cdot q-m^2+i \eta )}$$

The presence of `FeynAmpDenominator` in an expression does not automatically mean that it is a loop amplitude. `FeynAmpDenominator` can equally appear in tree level amplitudes, where it stands for the usual 4-dimensional propagator.

In FeynCalc there is no explicit way to distinguish between loop amplitudes and tree-level amplitudes. When you use functions that manipulate loop integrals, you need to tell them explicitly what is your loop momentum.

### Manipulations of FeynAmpDenominators

There are several functions, that are useful both for tree- and loop-level amplitudes, depending on what we want to do

For example, one can split one `FeynAmpDenominator` into many

```mathematica
FAD[{k1 - k2}, {k1 - p2, m}, {k2 + p2, m}]
FeynAmpDenominatorSplit[%]
% // FCE // StandardForm
```

$$\frac{1}{(\text{k1}-\text{k2})^2.\left((\text{k1}-\text{p2})^2-m^2\right).\left((\text{k2}+\text{p2})^2-m^2\right)}$$

$$\frac{1}{(\text{k1}-\text{k2})^2 \left((\text{k1}-\text{p2})^2-m^2\right) \left((\text{k2}+\text{p2})^2-m^2\right)}$$

```mathematica
(*FAD[k1 - k2] FAD[{k1 - p2, m}] FAD[{k2 + p2, m}]*)
```

or combine several into one

```mathematica
FeynAmpDenominatorCombine[FAD[k1 - k2] FAD[{k1 - p2, m}] FAD[{k2 + p2, m}]]
% // FCE // StandardForm
```

$$\frac{1}{(\text{k1}-\text{k2})^2.\left((\text{k1}-\text{p2})^2-m^2\right).\left((\text{k2}+\text{p2})^2-m^2\right)}$$

```mathematica
(*FAD[k1 - k2, {k1 - p2, m}, {k2 + p2, m}]*)
```

At the tree-level we often do not need the `FeynAmpDenominators` but rather want to express everything in terms of explicit scalar products, in order to exploit kinematic simplifications. This is handled by `FeynAmpDenominatorExplicit`

```mathematica
FeynAmpDenominatorExplicit[FAD[{k2 + p2, m}, k1 - k2, {k1 - p2, m}]]
```

$$\frac{1}{\left(-2 (\text{k1}\cdot \;\text{k2})+\text{k1}^2+\text{k2}^2\right) \left(-2 (\text{k1}\cdot \;\text{p2})+\text{k1}^2-m^2+\text{p2}^2\right) \left(2 (\text{k2}\cdot \;\text{p2})+\text{k2}^2-m^2+\text{p2}^2\right)}$$

### One-loop tensor reduction

1-loop tensor reduction using Passarino-Veltman method is handled by `TID`

```mathematica
FVD[q, \[Mu]] FVD[q, \[Nu]] FAD[{q, m}]
TID[%, q]
```

$$\frac{q^{\mu } q^{\nu }}{q^2-m^2}$$

$$\frac{m^2 g^{\mu \nu }}{D \left(q^2-m^2\right)}$$

```mathematica
int = FVD[q, \[Mu]] SPD[q, p] FAD[{q, m0}, {q + p, m1}]
TID[%, q]
```

$$\frac{q^{\mu } (p\cdot q)}{\left(q^2-\text{m0}^2\right).\left((p+q)^2-\text{m1}^2\right)}$$

$$\frac{p^{\mu } \left(\text{m0}^2-\text{m1}^2+p^2\right)^2}{4 p^2 \left(q^2-\text{m0}^2\right).\left((q-p)^2-\text{m1}^2\right)}-\frac{p^{\mu } \left(\text{m0}^2-\text{m1}^2+p^2\right)}{4 p^2 \left(q^2-\text{m0}^2\right)}+\frac{p^{\mu } \left(\text{m0}^2-\text{m1}^2+3 p^2\right)}{4 p^2 \left(q^2-\text{m1}^2\right)}$$

By default, `TID` tries to reduce everything to scalar integrals with unit denominators.
However, if it encounters zero Gram determinants, it automatically switches to the coefficient functions

```mathematica
FCClearScalarProducts[]
SPD[p, p] = 0;
```

```mathematica
TID[int, q]
```

$$\frac{p^{\mu }}{2 \left(q^2-\text{m1}^2\right)}-\frac{1}{2} i \pi ^2 \left(\text{m0}^2-\text{m1}^2\right) p^{\mu } \;\text{B}_1\left(0,\text{m0}^2,\text{m1}^2\right)$$

If we want the result to be express entirely in terms of Passarino-Veltman function, i. e. without `FAD`s, we can use `ToPaVe`

```mathematica
TID[int, q, ToPaVe -> True]
```

$$\frac{1}{2} i \pi ^2 p^{\mu } \;\text{A}_0\left(\text{m1}^2\right)-\frac{1}{2} i \pi ^2 \left(\text{m0}^2-\text{m1}^2\right) p^{\mu } \;\text{B}_1\left(0,\text{m0}^2,\text{m1}^2\right)$$

`ToPaVe` is actually also a standalone function, so it can be used independently of `TID`

```mathematica
FCClearScalarProducts[]
FAD[q, {q + p1}, {q + p2}]
ToPaVe[%, q]
```

$$\frac{1}{q^2.(\text{p1}+q)^2.(\text{p2}+q)^2}$$

$$i \pi ^2 \;\text{C}_0\left(\text{p1}^2,\text{p2}^2,\text{p1}^2-2 (\text{p1}\cdot \;\text{p2})+\text{p2}^2,0,0,0\right)$$

Even if there are no Gram determinants, for some tensor integrals the full result in terms of scalar integrals is just too large

```mathematica
int = FVD[q, \[Mu]] FVD[q, \[Nu]] FAD[q, {q + p1}, {q + p2}]
res = TID[int, q];
```

$$\frac{q^{\mu } q^{\nu }}{q^2.(\text{p1}+q)^2.(\text{p2}+q)^2}$$

```mathematica
res // Short
```

$$-\frac{\langle\langle 1\rangle\rangle }{4 (2-D)\langle\langle 1\rangle\rangle \langle\langle 1\rangle\rangle ^2\langle\langle 1\rangle\rangle \langle\langle 1\rangle\rangle  \left(\langle\langle 1\rangle\rangle ^2-\langle\langle 1\rangle\rangle \right)^2}-\frac{\langle\langle 1\rangle\rangle }{\langle\langle 1\rangle\rangle }+\langle\langle 1\rangle\rangle -\frac{\langle\langle 44\rangle\rangle +\langle\langle 1\rangle\rangle }{4 \langle\langle 3\rangle\rangle  \langle\langle 1\rangle\rangle ^2}$$

Of course we collect with respect to `FAD` and isolate the prefactors, but the full result still remains messy

```mathematica
Collect2[res, FeynAmpDenominator, IsolateNames -> KK]
```

$$-\frac{\text{KK}(864)}{4 q^2.(-\text{p1}+\text{p2}+q)^2}+\frac{\text{KK}(868)}{4 q^2.(q-\text{p1})^2.(q-\text{p2})^2}-\frac{\text{KK}(866)}{4 q^2.(q-\text{p1})^2}+-\frac{\text{KK}(861)}{4 q^2.(q-\text{p2})^2}$$

In such cases, we can get a much more compact results , if we stick to coefficient functions and do not demand the full reduction to scalars. To do so, use the option `UsePaVeBasis`

```mathematica
res = TID[int, q, UsePaVeBasis -> True]
```

$$i \pi ^2 g^{\mu \nu } \;\text{C}_{00}\left(\text{p1}^2,\text{p2}^2,-2 (\text{p1}\cdot \;\text{p2})+\text{p1}^2+\text{p2}^2,0,0,0\right)+i \pi ^2 \;\text{p1}^{\mu } \;\text{p1}^{\nu } \;\text{C}_{11}\left(\text{p1}^2,-2 (\text{p1}\cdot \;\text{p2})+\text{p1}^2+\text{p2}^2,\text{p2}^2,0,0,0\right)+i \pi ^2 \;\text{p2}^{\mu } \;\text{p2}^{\nu } \;\text{C}_{11}\left(\text{p2}^2,-2 (\text{p1}\cdot \;\text{p2})+\text{p1}^2+\text{p2}^2,\text{p1}^2,0,0,0\right)+i \pi ^2 \left(\text{p1}^{\nu } \;\text{p2}^{\mu }+\text{p1}^{\mu } \;\text{p2}^{\nu }\right) \;\text{C}_{12}\left(\text{p1}^2,-2 (\text{p1}\cdot \;\text{p2})+\text{p1}^2+\text{p2}^2,\text{p2}^2,0,0,0\right)$$

The resulting coefficient functions can be further reduced with `PaVeReduce`

```mathematica
pvRes = PaVeReduce[res];
```

```mathematica
pvRes // Short
```

$$\langle\langle 5\rangle\rangle +\frac{i \langle\langle 1\rangle\rangle \langle\langle 1\rangle\rangle \langle\langle 1\rangle\rangle  (\langle\langle 1\rangle\rangle )}{4 (2-D) \langle\langle 1\rangle\rangle ^2}$$

### Multi-loop tensor reduction

In the case of multi-loop integrals (but also 1-loop integrals with linear propagators) one should use `FCMultiLoopTID`

```mathematica
FVD[q, \[Mu]] FVD[q, \[Nu]] SFAD[{q, m^2}, {{0, 2 l . q}}]
FCMultiLoopTID[%, {q}]
```

$$\frac{q^{\mu } q^{\nu }}{(q^2-m^2+i \eta ).(2 (l\cdot q)+i \eta )}$$

$$\frac{m^2 \left(l^{\mu } l^{\nu }-l^2 g^{\mu \nu }\right)}{(1-D) l^2 (q^2-m^2+i \eta ).(2 (l\cdot q)+i \eta )}$$

### Working with GLI and FCTopology symbols

Integral families are encoded in form of `FCTopology[id, {props}, {lmoms}, {extmoms}, kinematics, reserved]` symbols

```mathematica
topos = {
   FCTopology["topoBox1L", {FAD[{q, m0}], FAD[{q + p1, m1}], FAD[{q + p2, m2}], FAD[{q + p2, m3}]}, 
    {q}, {p1, p2, p3}, {}, {}], 
   FCTopology["topoTad2L", {FAD[{q1, m1}], FAD[{q2, m2}], FAD[{q1 - q2, 0}]}, {q1, q2}, {}, {}, {}]}
```

$$\left\{\text{FCTopology}\left(\text{topoBox1L},\left\{\frac{1}{q^2-\text{m0}^2},\frac{1}{(\text{p1}+q)^2-\text{m1}^2},\frac{1}{(\text{p2}+q)^2-\text{m2}^2},\frac{1}{(\text{p2}+q)^2-\text{m3}^2}\right\},\{q\},\{\text{p1},\text{p2},\text{p3}\},\{\},\{\}\right),\text{FCTopology}\left(\text{topoTad2L},\left\{\frac{1}{\text{q1}^2-\text{m1}^2},\frac{1}{\text{q2}^2-\text{m2}^2},\frac{1}{(\text{q1}-\text{q2})^2}\right\},\{\text{q1},\text{q2}\},\{\},\{\},\{\}\right)\right\}$$

The loop integrals belonging to these topologies are written as `GLI[id, {powers}]` symbols

```mathematica
exp = a1 GLI["topoBox1L", {1, 1, 1, 1}] + a2 GLI["topoTad2L", {1, 2, 2}]
```

$$\text{a1} G^{\text{topoBox1L}}(1,1,1,1)+\text{a2} G^{\text{topoTad2L}}(1,2,2)$$

Using `FCLoopFromGLI` we can convert `GLI`s into explicit propagator notation

```mathematica
FCLoopFromGLI[exp, topos]
```

$$\frac{\text{a1}}{\left(q^2-\text{m0}^2\right) \left((\text{p1}+q)^2-\text{m1}^2\right) \left((\text{p2}+q)^2-\text{m2}^2\right) \left((\text{p2}+q)^2-\text{m3}^2\right)}+\frac{\text{a2}}{\left(\text{q1}^2-\text{m1}^2\right) \left(\text{q2}^2-\text{m2}^2\right)^2 (\text{q1}-\text{q2})^4}$$

### Topology identification

The very first step is usually to identify the occurring topologies in the amplitude (without attempting to minimize their number)

Find topologies occurring in the 2-loop ghost self-energy amplitude

```mathematica
amp = Get[FileNameJoin[{$FeynCalcDirectory, "Documentation", "Examples", 
      "Amplitudes", "Gh-Gh-2L.m"}]];
```

```mathematica
res = FCLoopFindTopologies[amp, {q1, q2}];
```

$$\text{FCLoopFindTopologies: Number of the initial candidate topologies: }3$$

$$\text{FCLoopFindTopologies: Number of the identified unique topologies: }3$$

$$\text{FCLoopFindTopologies: Number of the preferred topologies among the unique topologies: }0$$

$$\text{FCLoopFindTopologies: Number of the identified subtopologies: }0$$

$$\text{FCLoopFindTopologies: }\;\text{Your topologies depend on the follwing kinematic invariants that are not all entirely lowercase: }\{\text{Pair[Momentum[p, D], Momentum[p, D]]}\}$$

$$\text{FCLoopFindTopologies: }\;\text{This may lead to issues if these topologies are meant to be processed using tools such as FIRE, KIRA or Fermat.}$$

```mathematica
res // Last
```

$$\left\{\text{FCTopology}\left(\text{fctopology1},\left\{\frac{1}{(\text{q2}^2+i \eta )},\frac{1}{(\text{q1}^2+i \eta )},\frac{1}{((\text{q1}+\text{q2})^2+i \eta )},\frac{1}{((p+\text{q1})^2+i \eta )},\frac{1}{((p-\text{q2})^2+i \eta )}\right\},\{\text{q1},\text{q2}\},\{p\},\{\},\{\}\right),\text{FCTopology}\left(\text{fctopology2},\left\{\frac{1}{(\text{q2}^2+i \eta )},\frac{1}{(\text{q1}^2+i \eta )},\frac{1}{((p+\text{q2})^2+i \eta )},\frac{1}{((p-\text{q1})^2+i \eta )}\right\},\{\text{q1},\text{q2}\},\{p\},\{\},\{\}\right),\text{FCTopology}\left(\text{fctopology3},\left\{\frac{1}{(\text{q2}^2+i \eta )},\frac{1}{(\text{q1}^2+i \eta )},\frac{1}{((p-\text{q1})^2+i \eta )},\frac{1}{((p-\text{q1}+\text{q2})^2+i \eta )}\right\},\{\text{q1},\text{q2}\},\{p\},\{\},\{\}\right)\right\}$$

The amplitude is the written as a linear combination of special products, where numerators with explicit loop momenta are multiplied by denominators written as `GLI`s

```mathematica
res[[1]][[1 ;; 5]]
```

$$\text{FCGV}(\text{GLIProduct})\left(\xi  g_s^4 \;\text{q2}^{\text{Lor1}} p^{\text{Lor2}} \;\text{q2}^{\text{Lor2}} \;\text{q1}^{\text{Lor4}} g^{\text{Lor3}\;\text{Lor4}} (\text{q1}+\text{q2})^{\text{Lor1}} (\text{q2}-p)^{\text{Lor3}} f^{\text{Glu1}\;\text{Glu3}\;\text{Glu6}} f^{\text{Glu2}\;\text{Glu4}\;\text{Glu7}} f^{\text{Glu3}\;\text{Glu4}\;\text{Glu5}} f^{\text{Glu5}\;\text{Glu6}\;\text{Glu7}},G^{\text{fctopology1}}(2,1,1,1,1)\right)+\text{FCGV}(\text{GLIProduct})\left(-\frac{\xi  g_s^4 \;\text{q1}^{\text{Lor1}} p^{\text{Lor2}} p^{\text{Lor3}} \;\text{q2}^{\text{Lor4}} g^{\text{Lor3}\;\text{Lor4}} (\text{q1}-p)^{\text{Lor1}} (p-\text{q1})^{\text{Lor2}} f^{\text{Glu1}\;\text{Glu6}\;\text{Glu7}} f^{\text{Glu2}\;\text{Glu4}\;\text{Glu5}} f^{\text{Glu3}\;\text{Glu4}\;\text{Glu5}} f^{\text{Glu3}\;\text{Glu6}\;\text{Glu7}}}{p^2},G^{\text{fctopology2}}(1,1,1,2)\right)+\text{FCGV}(\text{GLIProduct})\left(-\frac{\xi  g_s^4 \;\text{q1}^{\text{Lor1}} p^{\text{Lor2}} p^{\text{Lor3}} \;\text{q2}^{\text{Lor4}} g^{\text{Lor1}\;\text{Lor2}} (p+\text{q2})^{\text{Lor3}} (-p-\text{q2})^{\text{Lor4}} f^{\text{Glu1}\;\text{Glu6}\;\text{Glu7}} f^{\text{Glu2}\;\text{Glu4}\;\text{Glu5}} f^{\text{Glu3}\;\text{Glu4}\;\text{Glu5}} f^{\text{Glu3}\;\text{Glu6}\;\text{Glu7}}}{p^2},G^{\text{fctopology2}}(1,1,2,1)\right)+\text{FCGV}(\text{GLIProduct})\left(-\frac{\xi ^2 g_s^4 \;\text{q1}^{\text{Lor1}} p^{\text{Lor2}} p^{\text{Lor3}} \;\text{q2}^{\text{Lor4}} (\text{q1}-p)^{\text{Lor1}} (p-\text{q1})^{\text{Lor2}} (p+\text{q2})^{\text{Lor3}} (-p-\text{q2})^{\text{Lor4}} f^{\text{Glu1}\;\text{Glu6}\;\text{Glu7}} f^{\text{Glu2}\;\text{Glu4}\;\text{Glu5}} f^{\text{Glu3}\;\text{Glu4}\;\text{Glu5}} f^{\text{Glu3}\;\text{Glu6}\;\text{Glu7}}}{p^2},G^{\text{fctopology2}}(1,1,2,2)\right)+\text{FCGV}(\text{GLIProduct})\left(-\frac{g_s^4 \;\text{q1}^{\text{Lor1}} p^{\text{Lor2}} p^{\text{Lor3}} \;\text{q2}^{\text{Lor4}} g^{\text{Lor1}\;\text{Lor2}} g^{\text{Lor3}\;\text{Lor4}} f^{\text{Glu1}\;\text{Glu6}\;\text{Glu7}} f^{\text{Glu2}\;\text{Glu4}\;\text{Glu5}} f^{\text{Glu3}\;\text{Glu4}\;\text{Glu5}} f^{\text{Glu3}\;\text{Glu6}\;\text{Glu7}}}{p^2},G^{\text{fctopology2}}(1,1,1,1)\right)$$

### Finding topology mappings

Here we have a set of 5 topologies

```mathematica
topos1 = {
    FCTopology[fctopology1, {SFAD[{{p3, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {0, 1}, 1}], 
      SFAD[{{p1, 0}, {0, 1}, 1}], SFAD[{{p2 + p3, 0}, {0, 1}, 1}], SFAD[{{p2 - Q, 0}, {0, 1}, 1}], 
      SFAD[{{p1 - Q, 0}, {0, 1}, 1}], SFAD[{{p2 + p3 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}], 
      SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}]}, {p1, p2, p3}, {Q}, {}, {}], 
    FCTopology[fctopology2, {SFAD[{{p3, 0}, {0, 1}, 1}], 
      SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1, 0}, {0, 1}, 1}], SFAD[{{p2 + p3, 0}, {0, 1}, 1}], 
      SFAD[{{p2 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 - Q, 0}, {0, 1}, 1}], 
      SFAD[{{p2 + p3 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p2 - Q, 0}, {0, 1}, 1}], 
      SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}]}, {p1, p2, p3}, {Q}, {}, {}], 
    FCTopology[fctopology3, {SFAD[{{p3, 0}, {0, 1}, 1}], 
      SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1, 0}, {0, 1}, 1}], 
      SFAD[{{p2 + p3, 0}, {0, 1}, 1}], SFAD[{{p1 + p3, 0}, {0, 1}, 1}], 
      SFAD[{{p2 - Q, 0}, {0, 1}, 1}], SFAD[{{p2 + p3 - Q, 0}, {0, 1}, 1}], 
      SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}]}, 
     {p1, p2, p3}, {Q}, {}, {}], 
    FCTopology[fctopology4, {SFAD[{{p3, 0}, {0, 1}, 1}], 
      SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1, 0}, {0, 1}, 1}], 
      SFAD[{{p2 + p3, 0}, {0, 1}, 1}], SFAD[{{p1 + p3, 0}, {0, 1}, 1}], 
      SFAD[{{p2 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 - Q, 0}, {0, 1}, 1}], 
      SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}]}, 
     {p1, p2, p3}, {Q}, {}, {}], 
    FCTopology[fctopology5, {SFAD[{{p3, 0}, {0, 1}, 1}], 
      SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1, 0}, {0, 1}, 1}], 
      SFAD[{{p1 + p3, 0}, {0, 1}, 1}], SFAD[{{p2 - Q, 0}, {0, 1}, 1}],
      SFAD[{{p1 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}], 
      SFAD[{{p1 + p2 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}]}, 
     {p1, p2, p3}, {Q}, {}, {}]};
```

where 3 of them can be mapped to the other 2

```mathematica
mappings1 = FCLoopFindTopologyMappings[topos1];
```

$$\text{FCLoopFindTopologyMappings: }\;\text{Found }3\text{ mapping relations }$$

$$\text{FCLoopFindTopologyMappings: }\;\text{Final number of independent topologies: }2$$

```mathematica
mappings1[[1]]
```

$$\left(
\begin{array}{ccc}
 \;\text{FCTopology}\left(\text{fctopology3},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p1}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right) & \{\text{p1}\to -\text{p1}-\text{p3}+Q,\text{p2}\to -\text{p2}-\text{p3}+Q\} & G^{\text{fctopology3}}(\text{n1$\_$},\text{n7$\_$},\text{n8$\_$},\text{n5$\_$},\text{n6$\_$},\text{n4$\_$},\text{n2$\_$},\text{n3$\_$},\text{n9$\_$}):\to G^{\text{fctopology1}}(\text{n1},\text{n2},\text{n3},\text{n4},\text{n5},\text{n6},\text{n7},\text{n8},\text{n9}) \\
 \;\text{FCTopology}\left(\text{fctopology4},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p1}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right) & \{\text{p1}\to Q-\text{p2},\text{p2}\to Q-\text{p1},\text{p3}\to -\text{p3}\} & G^{\text{fctopology4}}(\text{n1$\_$},\text{n6$\_$},\text{n5$\_$},\text{n8$\_$},\text{n7$\_$},\text{n3$\_$},\text{n2$\_$},\text{n4$\_$},\text{n9$\_$}):\to G^{\text{fctopology1}}(\text{n1},\text{n2},\text{n3},\text{n4},\text{n5},\text{n6},\text{n7},\text{n8},\text{n9}) \\
 \;\text{FCTopology}\left(\text{fctopology5},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p1}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right) & \{\text{p1}\to \;\text{p2},\text{p2}\to \;\text{p1}\} & G^{\text{fctopology5}}(\text{n1$\_$},\text{n3$\_$},\text{n2$\_$},\text{n4$\_$},\text{n6$\_$},\text{n5$\_$},\text{n7$\_$},\text{n8$\_$},\text{n9$\_$}):\to G^{\text{fctopology2}}(\text{n1},\text{n2},\text{n3},\text{n4},\text{n5},\text{n6},\text{n7},\text{n8},\text{n9}) \\
\end{array}
\right)$$

And these are the final topologies

```mathematica
mappings1[[2]]
```

$$\left\{\text{FCTopology}\left(\text{fctopology1},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right),\text{FCTopology}\left(\text{fctopology2},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right)\right\}$$

### Tensor reductions with GLIs

Tensor reduction for topologies that have already been processed with `FCLoopFindTopologies` can be done using `FCLoopTensorReduce`

```mathematica
topo2 = FCTopology[prop1l, {SFAD[{q, m^2}, {q - p, m^2}]}, {q}, {p}, {}, {}]
```

$$\text{FCTopology}\left(\text{prop1l},\left\{\frac{1}{(q^2-m^2+i \eta ).((q-p)^2-m^2+i \eta )}\right\},\{q\},\{p\},\{\},\{\}\right)$$

```mathematica
amp2 = gliProduct[GSD[q] . GAD[\[Mu]] . GSD[q], GLI[prop1l, {1, 2}]]
```

$$\text{gliProduct}\left((\gamma \cdot q).\gamma ^{\mu }.(\gamma \cdot q),G^{\text{prop1l}}(1,2)\right)$$

```mathematica
amp2Red = FCLoopTensorReduce[amp2, {topo2}, Head -> gliProduct]
```

$$\text{gliProduct}\left(-\frac{2 D p^{\mu } \gamma \cdot p (p\cdot q)^2-2 p^2 \gamma ^{\mu } (p\cdot q)^2-D p^4 q^2 \gamma ^{\mu }+3 p^4 q^2 \gamma ^{\mu }-2 p^2 q^2 p^{\mu } \gamma \cdot p}{(1-D) p^4},G^{\text{prop1l}}(1,2)\right)$$

### Applying topology mappings

This is a trial expression representing some loop amplitude that has already been processed using `FCFindTopologies`

```mathematica
ex = gliProduct[cc6*SPD[p1, p1], GLI[fctopology1, {1, 1, 2, 1, 1, 1, 1, 1, 1}]] + 
   gliProduct[cc2*SPD[p1, p2], GLI[fctopology2, {1, 1, 1, 1, 1, 1, 1, 1, 1}]] + 
   gliProduct[cc4*SPD[p1, p2], GLI[fctopology4, {1, 1, 1, 1, 1, 1, 1, 1, 1}]] + 
   gliProduct[cc1*SPD[p1, Q], GLI[fctopology1, {1, 1, 1, 1, 1, 1, 1, 1, 1}]] + 
   gliProduct[cc3*SPD[p2, p2], GLI[fctopology3, {1, 1, 1, 1, 1, 1, 1, 1, 1}]] + 
   gliProduct[cc5*SPD[p2, Q], GLI[fctopology5, {1, 1, 1, 1, 1, 1, 1, 1, 1}]]
```

$$\text{gliProduct}\left(\text{cc1} (\text{p1}\cdot Q),G^{\text{fctopology1}}(1,1,1,1,1,1,1,1,1)\right)+\text{gliProduct}\left(\text{cc2} (\text{p1}\cdot \;\text{p2}),G^{\text{fctopology2}}(1,1,1,1,1,1,1,1,1)\right)+\text{gliProduct}\left(\text{cc3} \;\text{p2}^2,G^{\text{fctopology3}}(1,1,1,1,1,1,1,1,1)\right)+\text{gliProduct}\left(\text{cc4} (\text{p1}\cdot \;\text{p2}),G^{\text{fctopology4}}(1,1,1,1,1,1,1,1,1)\right)+\text{gliProduct}\left(\text{cc5} (\text{p2}\cdot Q),G^{\text{fctopology5}}(1,1,1,1,1,1,1,1,1)\right)+\text{gliProduct}\left(\text{cc6} \;\text{p1}^2,G^{\text{fctopology1}}(1,1,2,1,1,1,1,1,1)\right)$$

These mapping rules describe how the 3 topologies "fctopology3", "fctopology4" and "fctopology5" are mapped to the topologies "fctopology1" and "fctopology2"

```mathematica
mappings = {
   {FCTopology[fctopology3, {SFAD[{{p3, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {0, 1}, 1}], 
      SFAD[{{p1, 0}, {0, 1}, 1}], SFAD[{{p2 + p3, 0}, {0, 1}, 1}], SFAD[{{p1 + p3, 0}, {0, 1}, 1}], 
      SFAD[{{p2 - Q, 0}, {0, 1}, 1}], SFAD[{{p2 + p3 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}], 
      SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}]}, {p1, p2, p3}, {Q}, {}, {}], {p1 -> -p1 - p3 + Q, p2 -> 
      -p2 - p3 + Q, p3 -> p3}, 
    GLI[fctopology3, {n1_, n7_, n8_, n5_, n6_, n4_, n2_, n3_, n9_}] :>
     GLI[fctopology1, {n1, n2, n3, n4, n5, n6, n7, n8, n9}]}, 
   
   {FCTopology[fctopology4, {SFAD[{{p3, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1, 0}, 
        {0, 1}, 1}], 
      SFAD[{{p2 + p3, 0}, {0, 1}, 1}], SFAD[{{p1 + p3, 0}, {0, 1}, 1}], SFAD[{{p2 - Q, 0}, {0, 1}, 1}], 
      SFAD[{{p1 - Q, 0}, {0, 1}, 1}], 
      SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}]}, {p1, p2, p3}, {Q}, {}, {}], 
    {p1 -> -p2 + Q, p2 -> -p1 + Q, p3 -> -p3}, 
    GLI[fctopology4, {n1_, n6_, n5_, n8_, n7_, n3_, n2_, n4_, n9_}] :>
     GLI[fctopology1, {n1, n2, n3, n4, n5, n6, n7, n8, n9}]}, 
   
   {FCTopology[fctopology5, {SFAD[{{p3, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1, 0}, 
        {0, 1}, 1}], 
      SFAD[{{p1 + p3, 0}, {0, 1}, 1}], SFAD[{{p2 - Q, 0}, {0, 1}, 1}],SFAD[{{p1 - Q, 0}, {0, 1}, 1}], 
      SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}], 
      SFAD[{{p1 + p2 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}]}, {p1, p2, p3}, {Q}, {},
     {}], {p1 -> p2, p2 -> p1, p3 -> p3}, 
    GLI[fctopology5, {n1_, n3_, n2_, n4_, n6_, n5_, n7_, n8_, n9_}] :>
     GLI[fctopology2, {n1, n2, n3, n4, n5, n6, n7, n8, n9}]}}
```

$$\left(
\begin{array}{ccc}
 \;\text{FCTopology}\left(\text{fctopology3},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p1}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right) & \{\text{p1}\to -\text{p1}-\text{p3}+Q,\text{p2}\to -\text{p2}-\text{p3}+Q,\text{p3}\to \;\text{p3}\} & G^{\text{fctopology3}}(\text{n1$\_$},\text{n7$\_$},\text{n8$\_$},\text{n5$\_$},\text{n6$\_$},\text{n4$\_$},\text{n2$\_$},\text{n3$\_$},\text{n9$\_$}):\to G^{\text{fctopology1}}(\text{n1},\text{n2},\text{n3},\text{n4},\text{n5},\text{n6},\text{n7},\text{n8},\text{n9}) \\
 \;\text{FCTopology}\left(\text{fctopology4},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p1}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right) & \{\text{p1}\to Q-\text{p2},\text{p2}\to Q-\text{p1},\text{p3}\to -\text{p3}\} & G^{\text{fctopology4}}(\text{n1$\_$},\text{n6$\_$},\text{n5$\_$},\text{n8$\_$},\text{n7$\_$},\text{n3$\_$},\text{n2$\_$},\text{n4$\_$},\text{n9$\_$}):\to G^{\text{fctopology1}}(\text{n1},\text{n2},\text{n3},\text{n4},\text{n5},\text{n6},\text{n7},\text{n8},\text{n9}) \\
 \;\text{FCTopology}\left(\text{fctopology5},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p1}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right) & \{\text{p1}\to \;\text{p2},\text{p2}\to \;\text{p1},\text{p3}\to \;\text{p3}\} & G^{\text{fctopology5}}(\text{n1$\_$},\text{n3$\_$},\text{n2$\_$},\text{n4$\_$},\text{n6$\_$},\text{n5$\_$},\text{n7$\_$},\text{n8$\_$},\text{n9$\_$}):\to G^{\text{fctopology2}}(\text{n1},\text{n2},\text{n3},\text{n4},\text{n5},\text{n6},\text{n7},\text{n8},\text{n9}) \\
\end{array}
\right)$$

These are the two topologies onto which everything is mapped

```mathematica
finalTopos = {
   FCTopology[fctopology1, {SFAD[{{p3, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {0, 1}, 1}], 
     SFAD[{{p1, 0}, {0, 1}, 1}], SFAD[{{p2 + p3, 0}, {0, 1}, 1}], SFAD[{{p2 - Q, 0}, {0, 1}, 1}], 
     SFAD[{{p1 - Q, 0}, {0, 1}, 1}], SFAD[{{p2 + p3 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}], 
     SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}]}, {p1, p2, p3}, {Q}, {}, {}], 
   FCTopology[fctopology2, {SFAD[{{p3, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {0, 1}, 1}], 
     SFAD[{{p1, 0}, {0, 1}, 1}], SFAD[{{p2 + p3, 0}, {0, 1}, 1}], SFAD[{{p2 - Q, 0}, {0, 1}, 1}], 
     SFAD[{{p1 - Q, 0}, {0, 1}, 1}], SFAD[{{p2 + p3 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p2 - Q, 0}, {0, 1}, 1}], 
     SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}]}, {p1, p2, p3}, {Q}, {}, {}]}
```

$$\left\{\text{FCTopology}\left(\text{fctopology1},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right),\text{FCTopology}\left(\text{fctopology2},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right)\right\}$$

`FCLoopApplyTopologyMappings`  applies the given mappings to the expression creating an output that is ready to be processed further

```mathematica
FCLoopApplyTopologyMappings[ex, {mappings, finalTopos}, Head -> gliProduct]
```

$$\frac{1}{2} G^{\text{fctopology1}}(1,1,1,1,1,1,1,1,1) \left(\text{cc1} Q^2+\text{cc4} Q^2+2 \;\text{cc6}\right)+\frac{1}{2} (\text{cc1}-\text{cc4}) G^{\text{fctopology1}}(1,1,0,1,1,1,1,1,1)-\frac{1}{2} (\text{cc1}-\text{cc4}) G^{\text{fctopology1}}(1,1,1,1,1,0,1,1,1)+\frac{1}{2} Q^2 (\text{cc2}+\text{cc5}) G^{\text{fctopology2}}(1,1,1,1,1,1,1,1,1)-\frac{1}{2} (\text{cc2}+\text{cc5}) G^{\text{fctopology2}}(1,1,1,1,1,0,1,1,1)-\frac{1}{2} \;\text{cc2} G^{\text{fctopology2}}(1,1,1,1,0,1,1,1,1)+\frac{1}{2} \;\text{cc2} G^{\text{fctopology2}}(1,1,1,1,1,1,1,0,1)+\text{cc3} G^{\text{fctopology1}}(1,1,1,1,1,1,0,1,1)+\frac{1}{2} \;\text{cc4} G^{\text{fctopology1}}(0,1,1,1,1,1,1,1,1)-\frac{1}{2} \;\text{cc4} G^{\text{fctopology1}}(1,1,1,0,1,1,1,1,1)-\frac{1}{2} \;\text{cc4} G^{\text{fctopology1}}(1,1,1,1,1,1,1,0,1)+\frac{1}{2} \;\text{cc4} G^{\text{fctopology1}}(1,1,1,1,1,1,1,1,0)+\frac{1}{2} \;\text{cc5} G^{\text{fctopology2}}(1,1,0,1,1,1,1,1,1)$$

The resulting `GLI`s in the expression are loop integrals that can be IBP-reduced

### Mappings between integrals

To find one-to-one mappings between loop integrals use `FCLoopFindIntegralMappings`

```mathematica
FCClearScalarProducts[]
ClearAll[topo1, topo2]
```

```mathematica
topos = {FCTopology[topo1, {SFAD[{p1, m^2}], SFAD[{p2, m^2}]}, {p1, p2}, {}, {}, {}], 
   FCTopology[topo2, {SFAD[{p3, m^2}], SFAD[{p4, m^2}]}, {p3, p4}, {}, {}, {}]}
```

$$\left\{\text{FCTopology}\left(\text{topo1},\left\{\frac{1}{(\text{p1}^2-m^2+i \eta )},\frac{1}{(\text{p2}^2-m^2+i \eta )}\right\},\{\text{p1},\text{p2}\},\{\},\{\},\{\}\right),\text{FCTopology}\left(\text{topo2},\left\{\frac{1}{(\text{p3}^2-m^2+i \eta )},\frac{1}{(\text{p4}^2-m^2+i \eta )}\right\},\{\text{p3},\text{p4}\},\{\},\{\},\{\}\right)\right\}$$

```mathematica
glis = {GLI[topo1, {1, 1}], GLI[topo1, {1, 2}], GLI[topo1, {2, 1}], 
   GLI[topo2, {1, 1}], GLI[topo2, {2, 2}]}
```

$$\left\{G^{\text{topo1}}(1,1),G^{\text{topo1}}(1,2),G^{\text{topo1}}(2,1),G^{\text{topo2}}(1,1),G^{\text{topo2}}(2,2)\right\}$$

```mathematica
mappings = FCLoopFindIntegralMappings[glis, topos]
```

$$\left\{\left\{G^{\text{topo2}}(1,1)\to G^{\text{topo1}}(1,1),G^{\text{topo1}}(2,1)\to G^{\text{topo1}}(1,2)\right\},\left\{G^{\text{topo1}}(1,1),G^{\text{topo1}}(1,2),G^{\text{topo2}}(2,2)\right\}\right\}$$
