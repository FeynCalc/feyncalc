## Write2

`Write2[file, val1 = expr1, val2 = expr2, ...]` writes the settings `val1 = expr1, val2 = expr2` in sequence followed by a newline, to the specified output file. Setting the option `FormatType` of `Write2` to `FortranForm` results in Fortran syntax output.

### See also

[Overview](Extra/FeynCalc.md), [Isolate](Isolate.md), [PaVeReduce](PaVeReduce.md).

### Examples

```mathematica
FullForm[$FortranContinuationCharacter]
```

$$\&$$

```mathematica
t = Collect[((a - c)^2 + (a - b)^2)^2, a, Factor]
```

$$4 a^4-8 a^3 (b+c)+8 a^2 \left(b^2+b c+c^2\right)-4 a (b+c) \left(b^2+c^2\right)+\left(b^2+c^2\right)^2$$

This writes the assignment r=t to a file.

```mathematica
tempfilename = ToString[$SessionID] <> ".s"; 
 
Write2[tempfilename, r = t];
```

This shows the contents of the file.

```mathematica
TableForm[ReadList[If[$OperatingSystem === "MacOS", ":", ""] <> tempfilename, String]]
```

$$\begin{array}{l}
 \;\text{r = ( 4*a${}^{\wedge}$4 - 8*a${}^{\wedge}$3*(b + c) - 4*a*(b + c)*(b${}^{\wedge}$2 + c${}^{\wedge}$2) + } \\
 \;\text{ (b${}^{\wedge}$2 + c${}^{\wedge}$2)${}^{\wedge}$2 + 8*a${}^{\wedge}$2*(b${}^{\wedge}$2 + b*c + c${}^{\wedge}$2)} \\
 \;\text{       );} \\
\end{array}$$

```mathematica
DeleteFile[If[$OperatingSystem === "MacOS", ":", ""] <> tempfilename]
```

```mathematica
t2 = x + Isolate[t, a, IsolateNames -> w]
```

$$4 a^4-8 a^3 w(19)+8 a^2 w(21)-4 a w(19) w(20)+w(20)^2+x$$

```mathematica
Write2[tempfilename, r = t2];
```

```mathematica
TableForm[ReadList[If[$OperatingSystem === "MacOS", ":", ""] <> tempfilename, String]]
```

$$\begin{array}{l}
 \;\text{w[19] = (b + c} \\
 \;\text{       );} \\
 \;\text{w[20] = (b${}^{\wedge}$2 + c${}^{\wedge}$2} \\
 \;\text{       );} \\
 \;\text{w[21] = (b${}^{\wedge}$2 + b*c + c${}^{\wedge}$2} \\
 \;\text{       );} \\
 \;\text{r = ( 4*a${}^{\wedge}$4 + x - 8*a${}^{\wedge}$3*HoldForm[w[19]] - 4*a*HoldForm[w[19]]*} \\
 \;\text{  HoldForm[w[20]] + HoldForm[w[20]]${}^{\wedge}$2 + 8*a${}^{\wedge}$2*HoldForm[w[21]]} \\
 \;\text{       );} \\
\end{array}$$

```mathematica
DeleteFile[If[$OperatingSystem === "MacOS", ":", ""] <> tempfilename]
```

This is how to write out the expression `t2` in Fortran format.

```mathematica
Write2[tempfilename, r = t2, FormatType -> FortranForm];
```

```mathematica
TableForm[ReadList[If[$OperatingSystem === "MacOS", ":", ""] <> tempfilename, String]]
```

$$\begin{array}{l}
 \;\text{        w(19)= b + c} \\
 \;\text{        w(20)= b**2 + c**2} \\
 \;\text{        w(21)= b**2 + b*c + c**2} \\
 \;\text{        r = x + a**4*4D0 - a**3*8D0*w(19) - a*4D0*w(19)*w(20) + } \\
 \;\text{     $\&$  w(20)**2 + a**2*8D0*w(21)} \\
 \;\text{                  } \\
\end{array}$$

```mathematica
DeleteFile[If[$OperatingSystem === "MacOS", ":", ""] <> tempfilename]; 
 
Clear[w, t, t2, r, tempfilename];
```