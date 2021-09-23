## Spinor

`Spinor[p, m, o]` is the head of Dirac spinors. Which of the spinors $u$, $v$, $\bar{u}$ or $\bar{v}$ is understood, depends on the sign of the momentum argument `p` and the relative position of `Spinor` in the chain.

- `Spinor[Momentum[p], m]` means $\bar{u}$ if it stands at the beginning of the chain.

- `Spinor[Momentum[p], m]` means $u$ if it stands at the end of the chain.

- `Spinor[-Momentum[p], m]` means $\bar{v}$ if it stands at the beginning of the chain.

- `Spinor[-Momentum[p], m]` means $v$ if it stands at the end of the chain.

Spinors of fermions of mass $m$ are normalized to have $\bar{u} u=2 m$ and  $\bar{v} v=-2 m$.

The optional argument `o` can be used for additional degrees of freedom. If no optional argument `o` is supplied, a `1` is substituted in.

### See also

[Overview](Extra/FeynCalc.md), [FermionSpinSum](FermionSpinSum.md), [DiracSimplify](DiracSimplify.md), [SpinorU](SpinorU.md), [SpinorV](SpinorV.md), [SpinorUBar](SpinorUBar.md), [SpinorVBar](SpinorVBar.md), [SpinorUBarD](SpinorUBarD.md), [SpinorUD](SpinorUD.md), [SpinorVD](SpinorVD.md), [SpinorVBarD](SpinorVBarD.md).

### Examples

```mathematica
Spinor[Momentum[p]]
```

$$\varphi (\overline{p})$$

```mathematica
Spinor[Momentum[p], m]
```

$$\varphi (\overline{p},m)$$

FeynCalc uses covariant normalization (as opposed to e.g. the normalization used in Bjorken & Drell).

```mathematica
Spinor[Momentum[p], m] . Spinor[Momentum[p], m] // DiracSimplify
```

$$2 m$$

```mathematica
DiracSimplify[Spinor[-Momentum[p], m] . GS[p]]
```

$$-m \left(\varphi (-\overline{p},m)\right)$$

```mathematica
Spinor[Momentum[p]] // StandardForm

(*Spinor[Momentum[p], 0, 1]*)
```

```mathematica
ChangeDimension[Spinor[Momentum[p]], D] // StandardForm

(*Spinor[Momentum[p, D], 0, 1]*)
```

```mathematica
Spinor[Momentum[p], m] // StandardForm

(*Spinor[Momentum[p], m, 1]*)
```

`SmallVariable`s are discarded by `Spinor`.

```mathematica
Spinor[Momentum[p], SmallVariable[m]] // StandardForm

(*Spinor[Momentum[p], 0, 1]*)
```