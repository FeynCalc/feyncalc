`TimedIntegrate[exp, vars]` is like `Integrate`, but stops after the number of seconds specified by the option `Timing`. Options of `Integrate` can be given and are passed on.

### See also

### Examples

This should reach to be done

```mathematica
TimedIntegrate[Log[x^5], {x, 0, 1}, Timing -> 1]
```

$$-5$$

This shouldn't

```mathematica
TimedIntegrate[Log[Cos[x^5]], {x, 0, 1}, Timing -> 10, Integrate -> int]
```

$$\text{int}\left(\log \left(\cos \left(x^5\right)\right),\{x,0,1\},\text{Assumptions}\to \varepsilon >0\right)$$