## FermionSpinSum

`FermionSpinSum[exp]` converts products of closed spinor chains in `exp` into Dirac traces. Both Dirac and Majorana particles are supported. It is understood, that `exp` represents a squared amplitude.

### See also

[Overview](Extra/FeynCalc.md), [Spinor](Spinor.md), [ComplexConjugate](ComplexConjugate.md), [DiracTrace](DiracTrace.md).

### Examples

FeynCalc uses the customary relativistic normalization of the spinors.

```mathematica
SpinorUBar[Momentum[p], m] . SpinorU[Momentum[p], m]
FermionSpinSum[%]
DiracSimplify[%]
```

$$\bar{u}\left(\overline{p},m\right).u\left(\overline{p},m\right)$$

$$\text{tr}\left(\bar{\gamma }\cdot \overline{p}+m\right)$$

$$4 m$$

```mathematica
SpinorVBar[Momentum[p], m] . SpinorV[Momentum[p], m]
FermionSpinSum[%]
DiracSimplify[%]
```

$$\bar{v}\left(\overline{p},m\right).v\left(\overline{p},m\right)$$

$$\text{tr}\left(\bar{\gamma }\cdot \overline{p}-m\right)$$

$$-4 m$$

```mathematica
amp = SpinorUBar[k1, m] . GS[p] . GA[5] . SpinorU[p1, m]
ampSq = amp ComplexConjugate[amp]
```

$$\bar{u}(\text{k1},m).\left(\bar{\gamma }\cdot \overline{p}\right).\bar{\gamma }^5.u(\text{p1},m)$$

$$\bar{u}(\text{k1},m).\left(\bar{\gamma }\cdot \overline{p}\right).\bar{\gamma }^5.u(\text{p1},m) \left(-\left(\varphi (\overline{\text{p1}},m)\right).\bar{\gamma }^5.\left(\bar{\gamma }\cdot \overline{p}\right).\left(\varphi (\overline{\text{k1}},m)\right)\right)$$

```mathematica
FermionSpinSum[ampSq]
DiracSimplify[%] 
  
 

```

$$-\text{tr}\left(\left(\bar{\gamma }\cdot \overline{\text{k1}}+m\right).\left(\bar{\gamma }\cdot \overline{p}\right).\bar{\gamma }^5.\left(\bar{\gamma }\cdot \overline{\text{p1}}+m\right).\bar{\gamma }^5.\left(\bar{\gamma }\cdot \overline{p}\right)\right)$$

$$-4 \overline{p}^2 \left(\overline{\text{k1}}\cdot \overline{\text{p1}}\right)+8 \left(\overline{\text{k1}}\cdot \overline{p}\right) \left(\overline{p}\cdot \overline{\text{p1}}\right)-4 m^2 \overline{p}^2$$
