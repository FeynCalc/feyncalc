##  Map2 

Map2[f, exp] is equivalent to Map if Nterms[exp] > 0, otherwise Map2[f, exp] gives f[exp]..

###  See also 

NTerms.

###  Examples 

```mathematica
Map2[f, a - b] 
 
Map2[f, x] 
 
Map2[f, {a, b, c}] 
 
Map2[f, 1]
```

$$f(a)+f(-b)$$

$$f(x)$$

$$f(\{a,b,c\})$$

$$f(1)$$