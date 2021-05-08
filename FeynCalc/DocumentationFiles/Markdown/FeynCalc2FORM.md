##  FeynCalc2FORM 

FeynCalc2FORM[exp] displays exp in FORM syntax.   FeynCalc2FORM[file, x] writes $\text{x}$ in FORM syntax to a file.   FeynCalc2FORM[file, x == y] writes $x=y$ to a file in FORM syntax.The capabilities of this function are very limited, so that you should not expect it to easily handle large and compicated expressions..

###  Examples 

```mathematica
FORM2FeynCalc
MT[\[Mu], \[Nu]] FV[p, \[Rho]] y^2/d 
 
FeynCalc2FORM[%];
(y^2*d_ (mu, nu)*p (ro))/d
LC[\[Alpha], \[Beta], \[Delta], \[Rho]] 
 
FeynCalc2FORM[%];
(-i_)*e_ (al, be, de, ro)
DiracTrace[GA[\[Mu], \[Nu], \[Rho], \[Sigma]]] 
 
FeynCalc2FORM[%];
g_ (0, mu)*g_ (0, nu)*g_ (0, ro)*g_ (0, si)
DiracTrace[GA[\[Mu], \[Nu]]] DiracTrace[GA[\[Mu], \[Rho]]] 
 
FeynCalc2FORM[%];
g_ (0, mu)*g_ (0, nu)*g_ (1, mu)*g_ (1, ro)
FeynCalc2FORM["fc2ftest.f", MT[\[Mu], \[Nu]] FV[p, \[Mu]]];
ReadList[If[$OperatingSystem === "MacOS", ":", ""] <> "fc2ftest.f", String] 
 
t = DiracSimplify[DiracTrace[GA[\[Mu], \[Nu], \[Rho], \[Sigma]] . GS[p, q]]] 
 
FeynCalc2FORM["fc2ftest.f", L == t];
TableForm[ReadList[If[$OperatingSystem === "MacOS", ":", ""] <> "fc2ftest.f", String]] 
 
If[FileNames["fc2ftest.f"] =!= {}, DeleteFile["fc2ftest.f"]];
Clear[t];
```

$$\text{FORM2FeynCalc}$$

$$\frac{y^2 \overline{p}^{\rho } \bar{g}^{\mu \nu }}{d}$$

(y^2*d_(mu,nu)*p(ro))/d