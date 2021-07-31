## TraceOfOne

`TraceOfOne` is an option for `Tr` and `DiracTrace`. Its setting determines the value of the unit trace.

### See also

[DiracSimplify](DiracSimplify), [DiracTrace](DiracTrace).

### Examples

```mathematica
DiracTrace[1]
DiracSimplify[%]
```

$$\text{tr}(1)$$

$$4$$

```mathematica
DiracTrace[1, TraceOfOne -> tr1]
DiracSimplify[%]
```

$$\text{tr}(1)$$

$$\text{tr1}$$