##  SelectSplit 

SelectSplit[l, p] constructs list of mutually exclusive subsets from l in which every element li satisfies a criterium pj[li] with pj from p and appends the subset of remaining unmatched elements..

###  Examples 

```mathematica
SelectSplit[{a^2, b^3, c^4, d^5, e^6, f + g, h^4}, {MatchQ[#, _^2] &, MatchQ[#, _^4] &, FreeQ[#, Power] &}] 
 
SelectSplit[{a^2, b^3, c^4, d^5, e^6, f + g, h^4}, {FreeQ[#, Plus] &, FreeQ[#, Power] &}]
```

$$\left\{\left\{a^2\right\},\left\{c^4,h^4\right\},\{f+g\},\left\{b^3,d^5,e^6\right\}\right\}$$

$$\left\{\left\{a^2,b^3,c^4,d^5,e^6,h^4\right\},\{f+g\},\{\}\right\}$$