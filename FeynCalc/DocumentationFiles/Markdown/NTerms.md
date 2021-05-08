##  NTerms 

NTerms[x] is equivalent to Length if x is a sum; otherwise NTerms[x] returns 1, except NTerms[0] -> 0..

###  Examples 

```mathematica
NTerms[a - b] 
 
NTerms[a b c] 
 
NTerms[9] 
 
NTerms[0]
```

$$2$$

$$1$$

$$1$$

$$0$$