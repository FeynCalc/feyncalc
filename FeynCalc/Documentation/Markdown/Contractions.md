## Contractions

### See also

[Overview](Extra/FeynCalc.md).

### Simplifications of tensorial expressions

Now that we have some basic understanding of FeynCalc objects, let us do something with them. Contractions of Lorentz indices are one of the most essential operations in symbolic QFT calculations. In FeynCalc the corresponding function is called `Contract`

```mathematica
FV[p, \[Mu]] MT[\[Mu], \[Nu]]
Contract[%]
```

$$\text{FV}(p,\mu ) \;\text{MT}(\mu ,\nu )$$

$$\text{Contract}(\text{FV}(p,\mu ) \;\text{MT}(\mu ,\nu ))$$

```mathematica
FV[p, \[Alpha]] FV[q, \[Alpha]]
Contract[%]
```

$$\text{FV}(p,\alpha ) \;\text{FV}(q,\alpha )$$

$$\text{Contract}(\text{FV}(p,\alpha ) \;\text{FV}(q,\alpha ))$$

Notice that when we enter noncommutative objects, such as Dirac matrices, we use `Dot` (`.`) and not `Times` (`*`) 

```mathematica
FV[p, \[Alpha]] MT[\[Beta], \[Gamma]] GA[\[Alpha]] . GA[\[Beta]] . GA[\[Gamma]]
Contract[%]
```

$$\text{FV}(p,\alpha ) \;\text{MT}(\beta ,\gamma ) \;\text{GA}(\alpha ).\text{GA}(\beta ).\text{GA}(\gamma )$$

$$\text{Contract}(\text{FV}(p,\alpha ) \;\text{MT}(\beta ,\gamma ) \;\text{GA}(\alpha ).\text{GA}(\beta ).\text{GA}(\gamma ))$$

This is because `Times` is commutative, so writing something like

```mathematica
GA[\[Delta]] GA[\[Beta]] GA[\[Alpha]]
```

$$\text{GA}(\alpha ) \;\text{GA}(\beta ) \;\text{GA}(\delta )$$

will give you completely wrong results. It is also a very common beginner's mistake!

It might be surprising that FeynCalc does not seem to distinguish between upper and lower Lorentz indices. 

In fact, FeynCalc tacitly assumes that all your expressions with Lorentz indices are manifestly Lorentz covariant and respect Einstein's summation. In particular, this implies that

In an equality, if a free Lorentz index appears upstairs on the right hand side, it must also appear upstairs
on the left hand side. Something like $p^{\mu} = c q_{\mu}$ would violate manifest Lorentz covariance. Hence,

```mathematica
FV[p, \[Mu]] == c FV[q, \[Mu]]
```

$$\text{FV}(p,\mu )=c \;\text{FV}(q,\mu )$$

could equally stand for $p^{\mu} = c q^{\mu}$ or $p_{\mu} = c q_{\mu}$. 

For the sake of definiteness, we impose that a free Lorentz should be always understood to be an upper index. This becomes important when dealing with nonrelativistic expressions involving Cartesian indices, where there's no manifest Lorentz covariance.

Since FeynCalc assumes that the expressions you enter are mathematically sensible, it will not check your input or complain, even if the expression you provided is obviously incorrect

```mathematica
MT[\[Mu], \[Nu]] FV[p, \[Mu]] FV[q, \[Mu]]
Contract[%]
```

$$\text{FV}(p,\mu ) \;\text{FV}(q,\mu ) \;\text{MT}(\mu ,\nu )$$

$$\text{Contract}(\text{FV}(p,\mu ) \;\text{FV}(q,\mu ) \;\text{MT}(\mu ,\nu ))$$

When it comes to products of Levi-Civita tensors (`Eps`), `Contract` will by default apply the product formula with the determinant of metric tensors

```mathematica
LC[\[Mu], \[Nu]][p, q] LC[\[Rho], \[Sigma]][r, s] FV[x, \[Mu]]
Contract[%]
```

$$\text{FV}(x,\mu ) \;\text{LC}(\mu ,\nu )(p,q) \;\text{LC}(\rho ,\sigma )(r,s)$$

$$\text{Contract}(\text{FV}(x,\mu ) \;\text{LC}(\mu ,\nu )(p,q) \;\text{LC}(\rho ,\sigma )(r,s))$$

This is, however, not always what we want and can be inhibited via the option `EpsContract`

```mathematica
LC[\[Mu], \[Nu]][p, q] LC[\[Rho], \[Sigma]][r, s] FV[x, \[Mu]]
Contract[%, EpsContract -> False]
```

$$\text{FV}(x,\mu ) \;\text{LC}(\mu ,\nu )(p,q) \;\text{LC}(\rho ,\sigma )(r,s)$$

$$\text{Contract}(\text{FV}(x,\mu ) \;\text{LC}(\mu ,\nu )(p,q) \;\text{LC}(\rho ,\sigma )(r,s),\text{EpsContract}\to \;\text{False})$$
