##  SUNT 

SUNT[a] is the SU(N) $T^a$ generator in the fundamental representation. The fundamental indices are implicit..

###  See also 

CA, CF, SUND, SUNDelta, SUNF, SUNSimplify.

###  Examples 

```mathematica
SUNT[a]
```

$$T^a$$

Since $T_a$ is a noncommutative object, products have to separated by a dot (.).

```mathematica
SUNT[a] . SUNT[b] . SUNT[c] 
 
SUNT[a, b, c, d] 
 
SUNSimplify[SUNT[a, b, a], SUNNToCACF -> False] 
 
SUNSimplify[SUNT[a, b, b, a]] 
 
SUNSimplify[SUNT[a, b, a]] 
 
SUNSimplify[SUNT[a, b, a], SUNNToCACF -> False]
```

$$T^a.T^b.T^c$$

$$T^a.T^b.T^c.T^d$$

$$-\frac{T^b}{2 N}$$

$$C_F^2$$

$$-\frac{1}{2} T^b \left(C_A-2 C_F\right)$$

$$-\frac{T^b}{2 N}$$

The normalizaton of the generators is chosen in the standard way, therefore $text{tr}left(T_aT_bright) = 1/2 delta _{text{ab}}.$

```mathematica
SUNTrace[SUNT[a, b]]
```

$$\frac{\delta ^{ab}}{2}$$

In case you want $T_f$, you need to include a factor 2Tf inside the trace.

```mathematica
SUNTrace[2 Tf SUNT[a, b]] 
 
SUNTrace[SUNT[a, b]] // StandardForm 
 
SUNT[a] // FCI // StandardForm 
 
SUNT[a] // FCI // FCE // StandardForm
```

$$T_f \delta ^{ab}$$

![0t0anv5mnsmup](img/0t0anv5mnsmup.png)

```
(*SUNT[SUNIndex[a]]*)

(*SUNT[a]*)
```