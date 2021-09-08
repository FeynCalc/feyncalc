## LeviCivita

`LeviCivita[mu, nu, rho, si]` is an input function for the totally antisymmetric Levi-Civita tensor. It evaluates automatically to the internal representation `Eps[LorentzIndex[mu], LorentzIndex[nu], LorentzIndex[rho], LorentzIndex[si]]` (or with a second argument in `LorentzIndex` for the `Dimension`, if the option `Dimension` of `LeviCivita` is changed).

`LeviCivita[mu , nu, ...][p, ...]` evaluates to `Eps[LorentzIndex[mu], LorentzIndex[nu], ..., Momentum[p], ...]`.

The shortcut `LeviCivita` is deprecated, please use `LC` instead!

### See also

[Overview](Extra/FeynCalc.md), [LC](LC.md), [FCI](FCI.md).

### Examples

```mathematica
LeviCivita[\[Alpha], \[Beta], \[Gamma], \[Delta]]
```

$$\bar{\epsilon }^{\alpha \beta \gamma \delta }$$

```mathematica
LeviCivita[][p, q, r, s]
```

$$\bar{\epsilon }^{\overline{p}\overline{q}\overline{r}\overline{s}}$$

```mathematica
LeviCivita[\[Alpha], \[Beta]][p, q]
StandardForm[%]
```

$$\bar{\epsilon }^{\alpha \beta \overline{p}\overline{q}}$$

```
(*Eps[LorentzIndex[\[Alpha]], LorentzIndex[\[Beta]], Momentum[p], Momentum[q]]*)
```

`LeviCivita` is scheduled for removal in the future versions of FeynCalc. The safe alternative is to use `LC`.

```mathematica
LC[\[Alpha], \[Beta], \[Gamma], \[Delta]]
```

$$\bar{\epsilon }^{\alpha \beta \gamma \delta }$$

```mathematica
LC[][p, q, r, s]
```

$$\bar{\epsilon }^{\overline{p}\overline{q}\overline{r}\overline{s}}$$

```mathematica
LC[\[Alpha], \[Beta]][p, q]
```

$$\bar{\epsilon }^{\alpha \beta \overline{p}\overline{q}}$$

```mathematica
LCD[\[Alpha], \[Beta], \[Gamma], \[Delta]]
```

$$\overset{\text{}}{\epsilon }^{\alpha \beta \gamma \delta }$$

```mathematica
LCD[][p, q, r, s]
```

$$\overset{\text{}}{\epsilon }^{pqrs}$$

```mathematica
LCD[\[Alpha], \[Beta]][p, q]
```

$$\overset{\text{}}{\epsilon }^{\alpha \beta pq}$$

```mathematica
FCI[LC[\[Alpha], \[Beta], \[Gamma], \[Delta]]] === LeviCivita[\[Alpha], \[Beta], \[Gamma], \[Delta]]
```

$$\text{True}$$

```mathematica
FCI[LCD[\[Alpha], \[Beta], \[Gamma], \[Delta]]] === LeviCivita[\[Alpha], \[Beta], \[Gamma], \[Delta], Dimension -> D]
```

$$\text{True}$$
