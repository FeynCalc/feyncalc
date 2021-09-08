## FeynCalc2FORM

`FeynCalc2FORM[exp]` displays `exp` in `FORM` syntax.

`FeynCalc2FORM[file, x]` writes `x` in FORM syntax to a file.

`FeynCalc2FORM[file, x == y]` writes $x=y$ to a file in FORM syntax.

The capabilities of this function are very limited, so you should not expect it to easily handle large and complicated expressions.

### See also

[Overview](Extra/FeynCalc.md), [FeynCalc2FORM](FeynCalc2FORM.md).

### Examples

```mathematica
FORM2FeynCalc
```

$$\text{FORM2FeynCalc}$$

```mathematica
MT[\[Mu], \[Nu]] FV[p, \[Rho]] y^2/d
FeynCalc2FORM[%];
```

$$\frac{y^2 \overline{p}^{\rho } \bar{g}^{\mu \nu }}{d}$$

(y^2*d_(mu,nu)*p(ro))/d

```mathematica
LC[\[Alpha], \[Beta], \[Delta], \[Rho]]
FeynCalc2FORM[%];
```

$$\bar{\epsilon }^{\alpha \beta \delta \rho }$$

(-i_)*e_(al,be,de,ro)

```mathematica
DiracTrace[GA[\[Mu], \[Nu], \[Rho], \[Sigma]]]
FeynCalc2FORM[%];
```

$$\text{tr}\left(\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }.\bar{\gamma }^{\sigma }\right)$$

g_(0,mu)*g_(0,nu)*g_(0,ro)*g_(0,si)

```mathematica
DiracTrace[GA[\[Mu], \[Nu]]] DiracTrace[GA[\[Mu], \[Rho]]]
FeynCalc2FORM[%];
```

$$\text{tr}\left(\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }\right) \;\text{tr}\left(\bar{\gamma }^{\mu }.\bar{\gamma }^{\rho }\right)$$

g_(0,mu)*g_(0,nu)*g_(1,mu)*g_(1,ro)

```mathematica
t = DiracSimplify[DiracTrace[GA[\[Mu], \[Nu], \[Rho], \[Sigma]] . GS[p, q]]]
```

$$4 \overline{p}^{\nu } \overline{q}^{\mu } \bar{g}^{\rho \sigma }-4 \overline{p}^{\mu } \overline{q}^{\nu } \bar{g}^{\rho \sigma }-4 \overline{p}^{\rho } \overline{q}^{\mu } \bar{g}^{\nu \sigma }+4 \overline{p}^{\rho } \overline{q}^{\nu } \bar{g}^{\mu \sigma }+4 \overline{p}^{\mu } \overline{q}^{\rho } \bar{g}^{\nu \sigma }-4 \overline{p}^{\nu } \overline{q}^{\rho } \bar{g}^{\mu \sigma }+4 \overline{p}^{\sigma } \overline{q}^{\mu } \bar{g}^{\nu \rho }-4 \overline{p}^{\sigma } \overline{q}^{\nu } \bar{g}^{\mu \rho }+4 \overline{p}^{\sigma } \overline{q}^{\rho } \bar{g}^{\mu \nu }-4 \overline{p}^{\mu } \overline{q}^{\sigma } \bar{g}^{\nu \rho }+4 \overline{p}^{\nu } \overline{q}^{\sigma } \bar{g}^{\mu \rho }-4 \overline{p}^{\rho } \overline{q}^{\sigma } \bar{g}^{\mu \nu }+4 \bar{g}^{\mu \nu } \bar{g}^{\rho \sigma } \left(\overline{p}\cdot \overline{q}\right)+4 \bar{g}^{\mu \sigma } \bar{g}^{\nu \rho } \left(\overline{p}\cdot \overline{q}\right)-4 \bar{g}^{\mu \rho } \bar{g}^{\nu \sigma } \left(\overline{p}\cdot \overline{q}\right)$$

```mathematica
FeynCalc2FORM["fc2ftest.f", L == t];
```

```mathematica
TableForm[ReadList[If[$OperatingSystem === "MacOS", ":", ""] <> "fc2ftest.f", String]]
```

$$\begin{array}{l}
 \;\text{Indices $\backslash \backslash $[Mu],$\backslash \backslash $[Nu],$\backslash \backslash $[Rho],$\backslash \backslash $[Sigma];} \\
 \;\text{Vectors OPEDelta,p,q;} \\
 \;\text{write statistics;} \\
 \;\text{Local L = ( } \\
 \;\text{4*d$\_$(mu,si)*d$\_$(nu,ro)*q.p-4*d$\_$(mu,ro)*d$\_$(nu,si)*q.p+4*d$\_$(mu,nu)*d$\_$(ro,si)*q.p+} \\
 \;\text{4*d$\_$(ro,si)*p(nu)*q(mu)-4*d$\_$(nu,si)*p(ro)*q(mu)+4*d$\_$(nu,ro)*p(si)*q(mu)-} \\
 \;\text{4*d$\_$(ro,si)*p(mu)*q(nu)+4*d$\_$(mu,si)*p(ro)*q(nu)-4*d$\_$(mu,ro)*p(si)*q(nu)+} \\
 \;\text{4*d$\_$(nu,si)*p(mu)*q(ro)-4*d$\_$(mu,si)*p(nu)*q(ro)+4*d$\_$(mu,nu)*p(si)*q(ro)-} \\
 \;\text{4*d$\_$(nu,ro)*p(mu)*q(si)+4*d$\_$(mu,ro)*p(nu)*q(si)-4*d$\_$(mu,nu)*p(ro)*q(si) ); } \\
 \;\text{   } \\
 \;\text{print;} \\
 \;\text{.end} \\
\end{array}$$

```mathematica
If[FileNames["fc2ftest.f"] =!= {}, DeleteFile["fc2ftest.f"]];
```

```mathematica
Clear[t];
```
