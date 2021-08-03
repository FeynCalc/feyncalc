## ExplicitSUNFIndex

`ExplicitSUNFIndex[ind]` is a specific $SU(N)$ index in the fundamental representation, i.e. `ind` is an integer.

### See also

[SUNIndex](SUNIndex), [SUNFIndex](SUNFIndex).

### Examples

```mathematica
ExplicitSUNFIndex[1]
```

$$1$$

```mathematica
SUNTF[a, 1, 2]
% // FCI // StandardForm
```

$$T_{12}^a$$

```
(*SUNTF[{SUNIndex[a]}, ExplicitSUNFIndex[1], ExplicitSUNFIndex[2]]*)
```