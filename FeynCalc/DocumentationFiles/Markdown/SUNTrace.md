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

$$-\frac{1}{8} i f^{ad\text{FCGV}(\text{e24})} d^{\text{FCGV}(\text{e24})bc}+\frac{1}{8} i d^{\text{FCGV}(\text{e24})ad} f^{bc\text{FCGV}(\text{e24})}+\frac{1}{8} d^{\text{FCGV}(\text{e24})ad} d^{\text{FCGV}(\text{e24})bc}-\frac{1}{8} d^{\text{FCGV}(\text{e24})bd} d^{\text{FCGV}(\text{e24})ac}+\frac{1}{8} d^{\text{FCGV}(\text{e24})cd} d^{\text{FCGV}(\text{e24})ab}+\frac{\delta ^{ad} \delta ^{bc}}{4 N}-\frac{\delta ^{ac} \delta ^{bd}}{4 N}+\frac{\delta ^{ab} \delta ^{cd}}{4 N}$$

$$\text{tr}(T^a.T^b.T^c.T^d)$$

```mathematica
SUNTrace[SUNT[a, b, c, d, e], Explicit -> True]
SUNSimplify[%, Explicit -> True]
```

$$\frac{1}{2} d^{\text{c27}ab} \left(\frac{1}{8} i f^{cd\text{FCGV}(\text{e28})} d^{\text{FCGV}(\text{e28})\text{c27}e}-\frac{1}{8} i d^{\text{FCGV}(\text{e28})cd} f^{\text{c27}e\text{FCGV}(\text{e28})}-\frac{1}{8} d^{\text{FCGV}(\text{e28})\text{c27}d} d^{\text{FCGV}(\text{e28})ce}+\frac{1}{8} d^{\text{FCGV}(\text{e28})cd} d^{\text{FCGV}(\text{e28})\text{c27}e}+\frac{1}{8} d^{\text{FCGV}(\text{e28})de} d^{\text{FCGV}(\text{e28})c\text{c27}}-\frac{\delta ^{ce} \delta ^{\text{c27}d}}{4 N}+\frac{\delta ^{cd} \delta ^{\text{c27}e}}{4 N}+\frac{\delta ^{c\text{c27}} \delta ^{de}}{4 N}\right)+\frac{1}{2} i f^{ab\text{c27}} \left(\frac{1}{8} i f^{cd\text{FCGV}(\text{e29})} d^{\text{FCGV}(\text{e29})\text{c27}e}-\frac{1}{8} i d^{\text{FCGV}(\text{e29})cd} f^{\text{c27}e\text{FCGV}(\text{e29})}-\frac{1}{8} d^{\text{FCGV}(\text{e29})\text{c27}d} d^{\text{FCGV}(\text{e29})ce}+\frac{1}{8} d^{\text{FCGV}(\text{e29})cd} d^{\text{FCGV}(\text{e29})\text{c27}e}+\frac{1}{8} d^{\text{FCGV}(\text{e29})de} d^{\text{FCGV}(\text{e29})c\text{c27}}-\frac{\delta ^{ce} \delta ^{\text{c27}d}}{4 N}+\frac{\delta ^{cd} \delta ^{\text{c27}e}}{4 N}+\frac{\delta ^{c\text{c27}} \delta ^{de}}{4 N}\right)+\frac{\delta ^{ab} \left(\frac{d^{cde}}{4}+\frac{1}{4} i f^{cde}\right)}{2 N}$$

$$\text{tr}(T^a.T^b.T^c.T^d.T^e)$$
