##  Isolate 

Isolate[expr] substitutes abbreviations KK[i] for all Plus[...] (sub-sums) in expr. The inserted KK[i] have head HoldForm. Isolate[expr, varlist] substitutes KK[i] for all subsums in expr which are free of any occurence of a member of the list varlist. Instead of KK any other head or a list of names of the abbreviations may be specified with the option IsolateNames..

###  Examples 

```mathematica
t0 = Isolate[a + b] 
 
t1 = Isolate[(a + b) f + (c + d) f + e, f] 
 
StandardForm[t1] 
 
{t0, t1, ReleaseHold[t1]} 
 
Isolate[a[z] (b + c (y + z)) + d[z] (y + z), {a, d}, IsolateNames -> fF] 
 
?? fF 
    Global`fF 
    fF[26] = y + z 
      
      fF[27] = b + c HoldForm[fF[26]] 
      
      
      Isolate[a - b - c - d - e, IsolateNames -> l, IsolateSplit -> 15] 
      
      Clear[t0, t1, l, fF]
```

$$\text{KK}(24)$$

$$e+f \text{KK}(24)+f \text{KK}(25)$$

![1dz86pnxmm6gy](img/1dz86pnxmm6gy.png)

$$\{\text{KK}(24),e+f \text{KK}(24)+f \text{KK}(25),f (a+b)+f (c+d)+e\}$$

$$\text{fF}(27) a(z)+\text{fF}(26) d(z)$$

$$![1hpjlkzra9kie](img/1hpjlkzra9kie.png)$$

$$![0pe9fhu04ruhz](img/0pe9fhu04ruhz.png)$$

$$\text{Information}[b+c \text{fF}(26) l(29) \text{Null},\text{LongForm}\to \text{True}]$$