##  Li4 

Li4 is an abbreviation for the weight 4 polylogarithm function, i.e., Li4 = PolyLog[4, #]&..

###  Examples 

```mathematica
Li4[x] 
 
Li4 // StandardForm 
 
D[Li4[x], x] 
 
Integrate[Li3[x]/x, x]
```

$$\text{Li}_4(x)$$

```
(*PolyLog[4, #1] &*)
```

$$\frac{\text{Li}_3(x)}{x}$$

$$\text{Li}_4(x)$$