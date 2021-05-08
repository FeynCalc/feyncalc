##  DiracGamma 

DiracGamma[x, dim] is the head of all Dirac matrices and slashes (in the internal representation). Use GA, GAD, GS or GSD for manual (short) input.DiracGamma[x, 4] simplifies to DiracGamma[x].DiracGamma[5] is $gamma ^5$DiracGamma[6] is $left.left(1+gamma ^5right)right/2.$ DiracGamma[7] is $left.left(1-gamma ^5right)right/2.$.

###  See also 

DiracGammaExpand, GA, DiracSimplify, GS, DiracTrick.

###  Examples 

```mathematica
DiracGamma[5] 
 
DiracGamma[LorentzIndex[\[Alpha]]]
```

$$\bar{\gamma }^5$$

$$\bar{\gamma }^{\alpha }$$

A Dirac-slash, i.e., $gamma ^{mu }q_{mu }$, is displayed as $gamma cdot q$.

```mathematica
DiracGamma[Momentum[q]] 
 
DiracGamma[Momentum[q]] . DiracGamma[Momentum[p - q]] 
 
DiracGamma[Momentum[q, D], D] 
 
GS[p - q] . GS[p] 
 
DiracGammaExpand[%] 
 
GAD[\[Mu]] . GSD[p - q] . GSD[q] . GAD[\[Mu]] 
 
DiracTrick[%] 
 
DiracSimplify[%%]
```

$$\bar{\gamma }\cdot \overline{q}$$

$$\left(\bar{\gamma }\cdot \overline{q}\right).\left(\bar{\gamma }\cdot \left(\overline{p}-\overline{q}\right)\right)$$

$$\gamma \cdot q$$

$$\left(\bar{\gamma }\cdot \left(\overline{p}-\overline{q}\right)\right).\left(\bar{\gamma }\cdot \overline{p}\right)$$

$$\left(\bar{\gamma }\cdot \overline{p}-\bar{\gamma }\cdot \overline{q}\right).\left(\bar{\gamma }\cdot \overline{p}\right)$$

$$\gamma ^{\mu }.(\gamma \cdot (p-q)).(\gamma \cdot q).\gamma ^{\mu }$$

$$4 ((p-q)\cdot q)+(D-4) (\gamma \cdot (p-q)).(\gamma \cdot q)$$

$$D (\gamma \cdot p).(\gamma \cdot q)-D q^2-4 (\gamma \cdot p).(\gamma \cdot q)+4 (p\cdot q)$$

DiracGamma may also carry Cartesian indices or appear contracted with Cartesian momenta.

```mathematica
DiracGamma[CartesianIndex[i]] 
 
DiracGamma[CartesianIndex[i, D - 1], D] 
 
DiracGamma[CartesianMomentum[p]] 
 
DiracGamma[CartesianMomentum[p, D - 1], D]
```

$$\overline{\gamma }^i$$

$$\gamma ^i$$

$$\overline{\gamma }\cdot \overline{p}$$

$$\gamma \cdot p$$

Temporal indices are represented using ExplicitLorentzIndex[0]

```mathematica
DiracGamma[ExplicitLorentzIndex[0]]
```

$$\bar{\gamma }^0$$