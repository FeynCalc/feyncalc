## ChangeDimension

`ChangeDimension[exp, dim]` changes all `LorentzIndex` and `Momentum` symbols in `exp` to dimension `dim` (and also Levi-Civita-tensors, Dirac slashes and Dirac matrices).

Notice that the dimension of `CartesianIndex` and `CartesianMomentum` objects will be changed to `dim-1`, not `dim`.

### See also

[Overview](Extra/FeynCalc.md), [LorentzIndex](LorentzIndex.md), [Momentum](Momentum.md), [DiracGamma](DiracGamma.md), [Eps](Eps.md).

### Examples

Remember that `LorentzIndex[mu, 4]` is simplified to `LorentzIndex[mu]` and `Momentum[p, 4]` to `Momentum[p]`. Thus the following objects are defined in four dimensions.

```mathematica
{LorentzIndex[\[Mu]], Momentum[p]}
ChangeDimension[%, D]
% // StandardForm
```

$$\left\{\mu ,\overline{p}\right\}$$

$$\{\mu ,p\}$$

```
(*{LorentzIndex[\[Mu], D], Momentum[p, D]}*)
```

This changes all non-4-dimensional objects to 4-dimensional ones

```mathematica
ChangeDimension[%%, 4] // StandardForm

(*{LorentzIndex[\[Mu]], Momentum[p]}*)
```

Consider the following list of 4- and D-dimensional objects

```mathematica
{GA[\[Mu], \[Nu]] MT[\[Mu], \[Nu]], GAD[\[Mu], \[Nu]] MTD[\[Mu], \[Nu]] f[D]}
DiracTrick /@ Contract /@ %
DiracTrick /@ Contract /@ ChangeDimension[%%, n]
```

$$\left\{\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu } \bar{g}^{\mu \nu },f(D) \gamma ^{\mu }.\gamma ^{\nu } g^{\mu \nu }\right\}$$

$$\{4,D f(D)\}$$

$$\{n,n f(D)\}$$

Any explicit occurrence of $D$ (like in $f(D)$) is not replaced by `ChangeDimension`.

```mathematica
LC[\[Mu], \[Nu], \[Rho], \[Sigma]]
ChangeDimension[%, D]
Factor2[Contract[%^2]]
```

$$\bar{\epsilon }^{\mu \nu \rho \sigma }$$

$$\overset{\text{}}{\epsilon }^{\mu \nu \rho \sigma }$$

$$(1-D) (2-D) (3-D) D$$

```mathematica
Contract[LC[\[Mu], \[Nu], \[Rho], \[Sigma]]^2]
```

$$-24$$
