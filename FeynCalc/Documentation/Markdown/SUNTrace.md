## SUNTrace

`SUNTrace[exp]` calculates the color-trace.

### See also

[Overview](Extra/FeynCalc.md), [SUNTrace](SUNTrace.md), [SUNT](SUNT.md), [SUNTF](SUNTF.md), [SUNF](SUNF.md), [SUND](SUND.md).

### Examples

```mathematica
SUNT[a, b] 
 
SUNTrace[%]
```

$$T^a.T^b$$

$$\frac{\delta ^{ab}}{2}$$

```mathematica
SUNTrace[SUNT[a, b, c]]
```

$$\text{tr}(T^a.T^b.T^c)$$

```mathematica
SUNTrace[SUNT[a, b, c], Explicit -> True]
```

$$\frac{d^{abc}}{4}+\frac{1}{4} i f^{abc}$$

```mathematica
SUNTrace[SUNT[a, b, c, d]]
```

$$\text{tr}(T^a.T^b.T^c.T^d)$$

```mathematica
SUNTrace[SUNT[a, b, c, d], Explicit -> True] 
 
SUNSimplify[%, Explicit -> True]
```

$$-\frac{1}{8} i f^{ad\text{FCGV}(\text{e19})} d^{\text{FCGV}(\text{e19})bc}+\frac{1}{8} i d^{\text{FCGV}(\text{e19})ad} f^{bc\text{FCGV}(\text{e19})}+\frac{1}{8} d^{\text{FCGV}(\text{e19})ad} d^{\text{FCGV}(\text{e19})bc}-\frac{1}{8} d^{\text{FCGV}(\text{e19})bd} d^{\text{FCGV}(\text{e19})ac}+\frac{1}{8} d^{\text{FCGV}(\text{e19})cd} d^{\text{FCGV}(\text{e19})ab}+\frac{\delta ^{ad} \delta ^{bc}}{4 N}-\frac{\delta ^{ac} \delta ^{bd}}{4 N}+\frac{\delta ^{ab} \delta ^{cd}}{4 N}$$

$$\text{tr}(T^a.T^b.T^c.T^d)$$

```mathematica
SUNTrace[SUNT[a, b, c, d, e], Explicit -> True] 
 
SUNSimplify[%, Explicit -> True]
```

$$\frac{1}{2} d^{\text{c22}ab} \left(\frac{1}{8} i f^{cd\text{FCGV}(\text{e23})} d^{\text{FCGV}(\text{e23})\text{c22}e}-\frac{1}{8} i d^{\text{FCGV}(\text{e23})cd} f^{\text{c22}e\text{FCGV}(\text{e23})}-\frac{1}{8} d^{\text{FCGV}(\text{e23})\text{c22}d} d^{\text{FCGV}(\text{e23})ce}+\frac{1}{8} d^{\text{FCGV}(\text{e23})cd} d^{\text{FCGV}(\text{e23})\text{c22}e}+\frac{1}{8} d^{\text{FCGV}(\text{e23})de} d^{\text{FCGV}(\text{e23})c\text{c22}}-\frac{\delta ^{ce} \delta ^{\text{c22}d}}{4 N}+\frac{\delta ^{cd} \delta ^{\text{c22}e}}{4 N}+\frac{\delta ^{c\text{c22}} \delta ^{de}}{4 N}\right)+\frac{1}{2} i f^{ab\text{c22}} \left(\frac{1}{8} i f^{cd\text{FCGV}(\text{e24})} d^{\text{FCGV}(\text{e24})\text{c22}e}-\frac{1}{8} i d^{\text{FCGV}(\text{e24})cd} f^{\text{c22}e\text{FCGV}(\text{e24})}-\frac{1}{8} d^{\text{FCGV}(\text{e24})\text{c22}d} d^{\text{FCGV}(\text{e24})ce}+\frac{1}{8} d^{\text{FCGV}(\text{e24})cd} d^{\text{FCGV}(\text{e24})\text{c22}e}+\frac{1}{8} d^{\text{FCGV}(\text{e24})de} d^{\text{FCGV}(\text{e24})c\text{c22}}-\frac{\delta ^{ce} \delta ^{\text{c22}d}}{4 N}+\frac{\delta ^{cd} \delta ^{\text{c22}e}}{4 N}+\frac{\delta ^{c\text{c22}} \delta ^{de}}{4 N}\right)+\frac{\delta ^{ab} \left(\frac{d^{cde}}{4}+\frac{1}{4} i f^{cde}\right)}{2 N}$$

$$\text{tr}(T^a.T^b.T^c.T^d.T^e)$$