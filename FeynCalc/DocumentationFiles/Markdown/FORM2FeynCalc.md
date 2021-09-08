## FORM2FeynCalc

`FORM2FeynCalc[exp]` translates the FORM expression `exp` into FeynCalc notation.

`FORM2FeynCalc[file]`  translates the FORM expressions in `file` into FeynCalc notation.   

`FORM2FeynCalc[file, x1, x2, ...]` reads in a file in FORM-format and translates the assignments for the variables $a, b, \ldots$ into FeynCalc syntax.

If the option `Set` is `True`, the variables `x1`, `x2` are assigned to the right hand sides defined in the FORM-file.The capabilities of this function are very limited, so that you should not expect it to easily handle large and complicated expressions.

### See also

[Overview](Extra/FeynCalc.md), [FeynCalc2FORM](FeynCalc2FORM.md).

### Examples

```mathematica
FORM2FeynCalc["p.q + 2*x m^2"]
% // StandardForm
```

$$\overline{p}\cdot \overline{q}+2 m^2.x$$

```
(*2 m^2 . x + SP[p, q]*)
```

Functions are automatically converted in a proper way, but bracketed expressions need to be substituted explicitly.

```mathematica
FORM2FeynCalc["x +f(z)+ log(x)^2+[li2(1-x)]", Replace -> {"[li2(1-x)]" -> "PolyLog[2,1-x]"}]
% // StandardForm
```

$$f(z)+\text{Li}_2(1-x)+x+\log ^2(x)$$

```
(*x + f[z] + Log[x]^2 + PolyLog[2, 1 - x]*)
```

```mathematica
FORM2FeynCalc["x + [(1)]*y -[(-1)^m]"]
ReleaseHold[%]
```

$$-\text{Hold}\left[(-1)^m\right]+\text{Hold}[1].y+x$$

$$(-1)^{m+1}+x+1.y$$

```mathematica
FORM2FeynCalc["p(mu)*q(nu)+d_(mu,nu)"]
% // StandardForm
```

$$\bar{g}^{\text{mu}\;\text{nu}}+p(\text{mu}).q(\text{nu})$$

```
(*p[mu] . q[nu] + MT[mu, nu]*)
```

```mathematica
FORM2FeynCalc["p(mu)*q(nu)+d_(mu,nu)", Replace -> {mu -> \[Mu], nu -> \[Nu]}]
```

$$\bar{g}^{\mu \nu }+p(\mu ).q(\nu )$$

```mathematica
FORM2FeynCalc["i_*az*bz*aM^2*D1*[(1)]*b_G1 * ( 4*eperp(mu,nu)*avec.bvec*blam )"]
```

$$(4 i).\text{az}.\text{bz}.\text{aM}^2.\text{D1}.\text{Hold}[1].\text{b\$G1}.\text{eperp}(\text{mu},\text{nu}).\left(\overline{\text{avec}}\cdot \overline{\text{bvec}}\right).\text{blam}$$
