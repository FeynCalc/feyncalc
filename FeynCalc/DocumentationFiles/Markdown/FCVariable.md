##  FCVariable 

FCVariable  is a data type. E.g. DataType[z, FCVariable] = True..

###  See also 

If we want to introduce constants c1 and c2, the naive way doesn't lead to the desired result

###  Examples 

```mathematica
SPD[c1 p1 + c2 p2, q] // ExpandScalarProduct
```

$$\text{c1} \text{p1}\cdot q+\text{c2} \text{p2}\cdot q$$

The solution is to declare c1 and c2 as FCVariable so that FeynCalc can distinguish them from the 4-momenta

```mathematica
DataType[c1, FCVariable] = True;
DataType[c2, FCVariable] = True;
SPD[c1 p1 + c2 p2, q] // ExpandScalarProduct
```

$$\text{c1} (\text{p1}\cdot q)+\text{c2} (\text{p2}\cdot q)$$

This works also for propagator denominators and matrices

```mathematica
FCI[SFAD[{q + c1 p1, m}]]
% // StandardForm
```

$$![0xpuc63zyjj2i](img/0xpuc63zyjj2i.png)$$

```
(*FeynAmpDenominator[StandardPropagatorDenominator[c1 Momentum[p1, D] + Momentum[q, D], 0, -m, {1, 1}]]*)
```

```mathematica
GAD[\[Mu]] . (GSD[c1 p] + m) . GAD[\[Nu]] // FCI
% // StandardForm
```

$$\gamma ^{\mu }.(\text{c1} \gamma \cdot p+m).\gamma ^{\nu }$$

```
(*DiracGamma[LorentzIndex[\[Mu], D], D] . (m + c1 DiracGamma[Momentum[p, D], D]) . DiracGamma[LorentzIndex[\[Nu], D], D]*)
```

```mathematica
CSI[i] . CSIS[c1 p] . CSI[j] // FCI
% // StandardForm

```

$$\overline{\sigma }^i.\left(\text{c1} \overline{\sigma }\cdot \overline{p}\right).\overline{\sigma }^j$$

```
(*PauliSigma[CartesianIndex[i]] . (c1 PauliSigma[CartesianMomentum[p]]) . PauliSigma[CartesianIndex[j]]*)
```

To undo the declarations use

```mathematica
DataType[c1, FCVariable] = False
DataType[c2, FCVariable] = False 
 

```

$$\text{False}$$

$$\text{False}$$