##  SMPToSymbol 

SMPToSymbol[exp] converts objects of type SMP["sth"] in exp to symbols using ToExpression["sth"]. The option StringReplace can be used to specify string replacement rules that will take care of characters (e.g. ^ or _) that cannot appear in valid expressions. SMPToSymbol is useful when exporting FeynCalc expressions to other tools, e.g. FORM..

###  Examples 

```mathematica
SMP
SP[p] - SMP["m_e"]^2 
 
SMPToSymbol[%]
```

$$\text{SMP}$$

$$\overline{p}^2-m_e^2$$

$$\overline{p}^2-\text{me}^2$$