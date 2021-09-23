## DotExpand

`DotExpand[exp]` expands dot products in `exp`.

### See also

[Overview](Extra/FeynCalc.md), [DOT](DOT.md), [DotSimplify](DotSimplify.md), [DeclareNonCommutativeUnDeclareNonCommutative](DeclareNonCommutativeUnDeclareNonCommutative.md).

### Examples

```mathematica
DOT[a x + b y + c z, d + e + f]
DotExpand[%]
```

$$(a x+b y+c z).(d+e+f)$$

$$a d x+a e x+a f x+b d y+b e y+b f y+c d z+c e z+c f z$$

```mathematica
DeclareNonCommutative /@ {a, b, c, d, e, f};
DotExpand[DOT[a x + b y + c z, d + e + f]]
```

$$x a.d+x a.e+x a.f+y b.d+y b.e+y b.f+z c.d+z c.e+z c.f$$

```mathematica
UnDeclareNonCommutative /@ {a, b, c, d, e, f};
DotExpand[DOT[a x + b y + c z, d + e + f]]
```

$$a d x+a e x+a f x+b d y+b e y+b f y+c d z+c e z+c f z$$
