## SelectSplit

`SelectSplit[l, p]` constructs list of mutually exclusive subsets from `l` in which every element `li` satisfies a criterion `pj[li]` with `pj` from `p` and appends the subset of remaining unmatched elements.

### See also

[Overview](Extra/FeynCalc.md), [SelectFree](SelectFree.md).

### Examples

```mathematica
SelectSplit[{a^2, b^3, c^4, d^5, e^6, f + g, h^4}, {MatchQ[#, _^2] &, MatchQ[#, _^4] &, FreeQ[#, Power] &}]
```

$$\left\{\left\{a^2\right\},\left\{c^4,h^4\right\},\{f+g\},\left\{b^3,d^5,e^6\right\}\right\}$$

```mathematica
SelectSplit[{a^2, b^3, c^4, d^5, e^6, f + g, h^4}, {FreeQ[#, Plus] &, FreeQ[#, Power] &}]
```

$$\left\{\left\{a^2,b^3,c^4,d^5,e^6,h^4\right\},\{f+g\},\{\}\right\}$$
