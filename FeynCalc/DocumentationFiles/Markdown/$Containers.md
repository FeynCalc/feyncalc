##  $Containers

`$Containers` is a set of heads over which `FieldDerivative` should distribute, in the following sense: Let `c` be a member of `$Containers`. Then `FieldDerivative[c[f, g, h][x], x, {mu}] -> c[FieldDerivative[f[x], x, {mu}], FieldDerivative[f[x], x, {mu}], FieldDerivative[f[x], x, {mu}]]`.

### See also

[FCCheckVersion](FCCheckVersion).

### Examples

```mathematica
$Containers
```

$$\{\}$$