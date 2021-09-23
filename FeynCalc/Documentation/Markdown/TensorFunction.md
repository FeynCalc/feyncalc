## TensorFunction

`TensorFunction[t, mu, nu, ...]` transform into `t[LorentzIndex[mu], LorentzIndex[nu], ...]`, i.e., it can be used as an unspecified tensorial function `t`.

A symmetric tensor can be obtained by `TensorFunction[{t, "S"}, mu, nu, ...]`, and an antisymmetric one by `TensorFunction[{t, "A"}, mu, nu, ...]`.

### See also

[Overview](Extra/FeynCalc.md), [FCSymmetrize](FCSymmetrize.md), [FCAntiSymmetrize](FCAntiSymmetrize.md).

### Examples

```mathematica
TensorFunction[t, \[Mu], \[Nu], \[Tau]]
% // StandardForm
```

$$t(\mu ,\nu ,\tau )$$

```
(*t[LorentzIndex[\[Mu]], LorentzIndex[\[Nu]], LorentzIndex[\[Tau]]]*)
```

```mathematica
Contract[FV[p, \[Mu]] %]
% // StandardForm
```

$$t\left(\overline{p},\nu ,\tau \right)$$

```
(*t[Momentum[p], LorentzIndex[\[Nu]], LorentzIndex[\[Tau]]]*)
```

```mathematica
TensorFunction[{f, "S"}, \[Alpha], \[Beta]]
```

$$f(\alpha ,\beta )$$

```mathematica
TensorFunction[{f, "S"}, \[Beta], \[Alpha]]
% // StandardForm
```

$$f(\alpha ,\beta )$$

```
(*f[LorentzIndex[\[Alpha]], LorentzIndex[\[Beta]]]*)
```

```mathematica
Attributes[f]
ClearAttributes[f, Orderless]
```

$$\{\text{Orderless}\}$$
