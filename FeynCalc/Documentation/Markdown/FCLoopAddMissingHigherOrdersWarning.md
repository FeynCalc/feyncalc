## FCLoopAddMissingHigherOrdersWarning

`FCLoopAddMissingHigherOrdersWarning[expr, ep, fun]` determines the highest `ep`-power $n$ in the given expression and adds a warning flag of order $\textrm{ep}^n+1$. This is meant to prevent incorrect results stemming insufficient high expansions of `expr` in `ep`

### See also

[Overview](Extra/FeynCalc.md), [FCFeynmanParametrize](FCFeynmanParametrize.md), [FCFeynmanPrepare](FCFeynmanPrepare.md).

### Examples

```mathematica
FCLoopAddMissingHigherOrdersWarning[1/ep^2 cc1 + 1/ep cc2, ep, epHelp]
```

$$\frac{\text{cc1}}{\text{ep}^2}+\frac{\text{cc2}}{\text{ep}}+(1+i) \;\text{epHelp}$$

```mathematica
FCLoopAddMissingHigherOrdersWarning[cc1, ep, epHelp]
```

$$\text{cc1}+(1+i) \;\text{ep} \;\text{epHelp}$$

```mathematica
FCLoopAddMissingHigherOrdersWarning[cc1, ep, epHelp, Complex -> False]
```

$$\text{cc1}+\text{ep} \;\text{epHelp}$$

```mathematica
FCLoopAddMissingHigherOrdersWarning[cc1, ep, epHelp, Names -> False]
```

$$\text{cc1}+(1+i) \;\text{ep} \;\text{epHelp}$$

```mathematica
FCLoopAddMissingHigherOrdersWarning[GLI[topo1, {1, 1, 1, 1, 1}] -> cc1/ep^2 + cc2/ep + cc3 , ep, epHelp]
```

$$G^{\text{topo1}}(1,1,1,1,1)\to \frac{\text{cc1}}{\text{ep}^2}+\frac{\text{cc2}}{\text{ep}}+\text{cc3}+(1+i) \;\text{ep} \;\text{epHelp}(\text{topo1X11111})$$