##  SelectFree 

SelectFree[expr, a, b, ...] is equivalent to Select[expr, FreeQ2[#, {a,b, ...}]&], except the special cases: SelectFree[a, b] returns a and SelectFree[a,a] returns 1 (where a is not a product or a sum)..

###  See also 

FreeQ2, SelectNotFree.

###  Examples 

```mathematica
SelectFree[a + b + f[a] + d, a] 
 
SelectFree[x y, x] 
 
SelectFree[2 x y z f[x], {x, y}] 
 
SelectFree[a, b] 
 
SelectFree[a, a] 
 
SelectFree[1, c] 
 
SelectFree[f[x], x]
```

$$\text{SelectFree}(f(a)+a+b+d,a)$$

$$\text{SelectFree}(x y,x)$$

$$\text{SelectFree}(2 x y z f(x),\{x,y\})$$

$$\text{SelectFree}(a,b)$$

$$\text{SelectFree}(a,a)$$

$$\text{SelectFree}(1,c)$$

$$\text{SelectFree}(f(x),x)$$