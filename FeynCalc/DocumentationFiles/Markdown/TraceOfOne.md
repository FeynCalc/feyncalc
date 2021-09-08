## TraceOfOne

`TraceOfOne` is an option for `Tr` and `DiracTrace`. Its setting determines the value of the unit trace.

### See also

[Overview](Extra/FeynCalc.md), [DiracSimplify](DiracSimplify.md), [DiracTrace](DiracTrace.md).

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
