##  SelectNotFree 

SelectNotFree[expr, x] returns that part of expr which is not free of any occurance of $\text{x}$.SelectNotFree[expr, a, b, ...] is equivalent to Select[expr, !FreeQ2[#, {a, b, ...}]&], except the special cases: SelectNotFree[a, b] returns 1 and SelectNotFree[a, a] returns a (where a is not a product or a sum)..

###  See also 

FreeQ2, SelectFree.

###  Examples 

```mathematica
SelectNotFree[a + b + f[a], a] 
 
SelectNotFree[2 x y f[x] z, {x, y}] 
 
SelectNotFree[a, b] 
 
SelectNotFree[a + x, b] 
 
SelectNotFree[a, a] 
 
SelectNotFree[1, c] 
 
SelectNotFree[f[x], x]
```

$$f(a)+a$$

$$x y f(x)$$

$$1$$

$$0$$

$$a$$

$$1$$

$$f(x)$$