## PaVeReduce

`PaVeReduce[expr]` reduces all Passarino-Veltman integrals (i.e. all PaVe's) in expr down to scalar `A0`, `B0`, `C0` and `D0`.

### See also

[Overview](Extra/FeynCalc.md), [FRH](FRH.md), [PaVeOrder](PaVeOrder.md).

### Examples

```mathematica
PaVeReduce[PaVe[1, 2, {s, m^2, m^2}, {m^2, m^2, M^2}], IsolateNames -> FF]
FRH[%]
```

$$\text{FF}(36)$$

$$\left(\frac{2 (2 D-3) M^2}{(D-2) \left(4 m^2-s\right)^2}-\frac{M^2 s}{2 m^2 \left(4 m^2-s\right)^2}-\frac{4 m^2}{\left(4 m^2-s\right)^2}+\frac{s}{\left(4 m^2-s\right)^2}\right) \;\text{B}_0\left(m^2,m^2,M^2\right)+\left(-\frac{2 (D-1) M^2}{(D-2) \left(4 m^2-s\right)^2}+\frac{4 m^2}{(D-2) \left(4 m^2-s\right)^2}-\frac{s}{(D-2) \left(4 m^2-s\right)^2}\right) \;\text{B}_0\left(s,m^2,m^2\right)+\left(-\frac{2 (D-1) M^4}{(D-2) \left(4 m^2-s\right)^2}-\frac{D M^2 s}{(D-2) \left(4 m^2-s\right)^2}+\frac{4 D m^2 M^2}{(D-2) \left(4 m^2-s\right)^2}\right) \;\text{C}_0\left(m^2,m^2,s,m^2,M^2,m^2\right)-\frac{\text{A}_0\left(M^2\right)}{2 m^2 \left(4 m^2-s\right)}+\frac{\text{A}_0\left(m^2\right)}{2 m^2 \left(4 m^2-s\right)}$$

The reduction results can be saved to a Mathematica file

```mathematica
PaVeReduce[PaVe[2, {SmallVariable[me2], mw2, t}, {SmallVariable[me2], 0, mw2}], 
  WriteOutPaVe -> "p"]
TableForm[ReadList["pPaVe1Cmw2tsmame2C0mw2smame2.s", String]]
DeleteFile /@ FileNames["pPaVe1Cmw2tsmame2C0mw2smame2.s"];
```

$$\frac{\text{B}_0(\text{mw2},0,\text{mw2})}{\text{mw2}-t}-\frac{\text{B}_0(t,\text{mw2},\text{me2})}{\text{mw2}-t}$$

$$\begin{array}{l}
 \;\text{( PaVe[0, $\{$mw2$\}$, $\{$0, mw2$\}$]/(mw2 - t) - PaVe[0, $\{$t$\}$, $\{$mw2, SmallVariable[me2]$\}$]/} \\
 \;\text{  (mw2 - t)} \\
 \;\text{  ) } \\
\end{array}$$

Fortran export is also available

```mathematica
se = SmallVariable[ME2];
d122 = PaVeReduce[PaVe[1, 2, 2, {se, MW2, MW2, se, S, T}, {0, se, 0, se}], Mandelstam -> {S, T, U, 2 MW2}, IsolateNames -> F] // FRH
Write2["fctd122.for", d122res == d122, FormatType -> FortranForm];
TableForm[ReadList["fctd122.for", String]]
DeleteFile /@ FileNames["fctd122.for"]; Clear[d122, se]; 
  
 

```

$$-\frac{D (\text{MW2}-S) T^2 \;\text{D}_0(\text{MW2},\text{MW2},\text{ME2},\text{ME2},T,S,\text{ME2},0,\text{ME2},0) S^3}{8 (3-D) \left(\text{MW2}^2-S U\right)^3}-\frac{D (\text{MW2}-S)^2 T \;\text{C}_0(\text{MW2},S,\text{ME2},\text{ME2},0,0) S^2}{4 (3-D) \left(\text{MW2}^2-S U\right)^3}+\frac{D (\text{MW2}-S) T^2 \;\text{C}_0(T,\text{ME2},\text{ME2},\text{ME2},\text{ME2},0) S^2}{8 (3-D) \left(\text{MW2}^2-S U\right)^3}-\frac{\left(2 \;\text{MW2}^2-D S T-2 S U\right) \;\text{B}_0(S,0,0) S}{2 (2-D) (\text{MW2}-S) \left(\text{MW2}^2-S U\right)^2}+\frac{(\text{MW2}+U) \;\text{A}_0(\text{ME2})}{2 \;\text{MW2} (4 \;\text{MW2}-T) \left(\text{MW2}^2-S U\right)}+\left(\left(-2 D \;\text{MW2}^5+16 \;\text{MW2}^5-3 D S \;\text{MW2}^4+2 S \;\text{MW2}^4-3 D U \;\text{MW2}^4+10 U \;\text{MW2}^4-8 D S^2 \;\text{MW2}^3+4 D S U \;\text{MW2}^3-16 S U \;\text{MW2}^3+2 D S^3 \;\text{MW2}^2+4 D S U^2 \;\text{MW2}^2-8 S U^2 \;\text{MW2}^2-2 D S^2 U \;\text{MW2}^2+2 D S^4 \;\text{MW2}+4 D S^3 U \;\text{MW2}+D S^2 U^3-2 S^2 U^3+D S^3 U^2-2 S^3 U^2\right) \;\text{B}_0(\text{MW2},0,\text{ME2})\right)/\left(2 (2-D) (\text{MW2}-S) (4 \;\text{MW2}-T)^2 \left(\text{MW2}^2-S U\right)^2\right)-\left(\left(20 \;\text{MW2}^4-2 D S \;\text{MW2}^3-12 S \;\text{MW2}^3-6 T \;\text{MW2}^3-2 D U \;\text{MW2}^3-4 D S^2 \;\text{MW2}^2+2 S T \;\text{MW2}^2-20 S U \;\text{MW2}^2+D S^3 \;\text{MW2}+3 D S U^2 \;\text{MW2}+12 S^2 U \;\text{MW2}+6 S T U \;\text{MW2}+D S^4+D S^2 U^2+2 D S^3 U-2 S^2 T U\right) \;\text{B}_0(T,\text{ME2},\text{ME2})\right)/\left(2 (2-D) (4 \;\text{MW2}-T)^2 \left(\text{MW2}^2-S U\right)^2\right)-\left(\left(128 D \;\text{MW2}^7+48 \;\text{MW2}^7-608 D S \;\text{MW2}^6-128 D T \;\text{MW2}^6-144 U \;\text{MW2}^6-4 D^2 S^2 \;\text{MW2}^5+1152 D S^2 \;\text{MW2}^5+28 D T^2 \;\text{MW2}^5-4 D^2 U^2 \;\text{MW2}^5+888 D S T \;\text{MW2}^5-8 D^2 S U \;\text{MW2}^5-96 S U \;\text{MW2}^5+48 T U \;\text{MW2}^5-16 D^2 S^3 \;\text{MW2}^4-1088 D S^3 \;\text{MW2}^4-420 D S T^2 \;\text{MW2}^4+288 S U^2 \;\text{MW2}^4-1840 D S^2 T \;\text{MW2}^4+6 D^2 S^4 \;\text{MW2}^3+512 D S^4 \;\text{MW2}^3+64 D S T^3 \;\text{MW2}^3+10 D^2 S U^3 \;\text{MW2}^3+1116 D S^2 T^2 \;\text{MW2}^3+18 D^2 S^2 U^2 \;\text{MW2}^3+48 S^2 U^2 \;\text{MW2}^3-96 S T U^2 \;\text{MW2}^3+1568 D S^3 T \;\text{MW2}^3-2 D^2 S^3 U \;\text{MW2}^3+8 D^2 S^5 \;\text{MW2}^2-96 D S^5 \;\text{MW2}^2-324 D S^2 T^3 \;\text{MW2}^2-144 S^2 U^3 \;\text{MW2}^2-716 D S^3 T^2 \;\text{MW2}^2+8 D^2 S^3 U^2 \;\text{MW2}^2-528 D S^4 T \;\text{MW2}^2+16 D^2 S^4 U \;\text{MW2}^2-D^2 S^6 \;\text{MW2}+38 D S^2 T^4 \;\text{MW2}-7 D^2 S^2 U^4 \;\text{MW2}+100 D S^3 T^3 \;\text{MW2}-6 D^2 S^3 U^3 \;\text{MW2}+48 S^2 T U^3 \;\text{MW2}+120 D S^4 T^2 \;\text{MW2}+40 D S^5 T \;\text{MW2}-2 D^2 S^5 U \;\text{MW2}-D^2 S^7+2 D S^3 T^4-D^2 S^3 U^4-4 D^2 S^4 U^3-6 D^2 S^5 U^2-4 D^2 S^6 U\right) \;\text{C}_0(\text{MW2},\text{MW2},T,\text{ME2},0,\text{ME2})\right)/\left(8 (2-D) (3-D) (4 \;\text{MW2}-T)^2 \left(\text{MW2}^2-S U\right)^3\right)$$

$$\begin{array}{l}
 \;\text{        d122res = ((MW2 + U)*5.D-1*PaVe(0D0,List(),List(ME2)))/} \\
 \;\text{     $\&$   (MW2*(MW2**2 - S*U*1D0)*(-(T*1D0) + MW2*4D0)) + } \\
 \;\text{     $\&$  (5.D-1*(D*S**3*U**2 + D*S**2*U**3 - D*MW2**5*2D0 + } \\
 \;\text{     $\&$       MW2**4*S*2D0 + D*MW2**2*S**3*2D0 + } \\
 \;\text{     $\&$       D*MW2*S**4*2D0 - D*MW2**2*S**2*U*2D0 - } \\
 \;\text{     $\&$       S**3*U**2*2D0 - S**2*U**3*2D0 - D*MW2**4*S*3D0 - } \\
 \;\text{     $\&$       D*MW2**4*U*3D0 + D*MW2**3*S*U*4D0 + } \\
 \;\text{     $\&$       D*MW2*S**3*U*4D0 + D*MW2**2*S*U**2*4D0 - } \\
 \;\text{     $\&$       D*MW2**3*S**2*8D0 - MW2**2*S*U**2*8D0 + } \\
 \;\text{     $\&$       MW2**4*U*1.D1 + MW2**5*1.6D1 - MW2**3*S*U*1.6D1)*} \\
 \;\text{     $\&$     PaVe(0D0,List(MW2),List(0D0,ME2)))/} \\
 \;\text{     $\&$   ((MW2 - S*1D0)*(MW2**2 - S*U*1D0)**2*} \\
 \;\text{     $\&$     (-(D*1D0) + 2D0)*(-(T*1D0) + MW2*4D0)**2) - } \\
 \;\text{     $\&$  (S*5.D-1*(-(D*S*T*1D0) + MW2**2*2D0 - S*U*2D0)*} \\
 \;\text{     $\&$     PaVe(0D0,List(S),List(0D0,0D0)))/} \\
 \;\text{     $\&$   ((MW2 - S*1D0)*(MW2**2 - S*U*1D0)**2*} \\
 \;\text{     $\&$     (-(D*1D0) + 2D0)) - } \\
 \;\text{     $\&$  (5.D-1*(D*MW2*S**3 + D*S**4 + D*S**2*U**2 - } \\
 \;\text{     $\&$       D*MW2**3*S*2D0 + MW2**2*S*T*2D0 - } \\
 \;\text{     $\&$       D*MW2**3*U*2D0 + D*S**3*U*2D0 - S**2*T*U*2D0 + } \\
 \;\text{     $\&$       D*MW2*S*U**2*3D0 - D*MW2**2*S**2*4D0 - } \\
 \;\text{     $\&$       MW2**3*T*6D0 + MW2*S*T*U*6D0 - MW2**3*S*1.2D1 + } \\
 \;\text{     $\&$       MW2*S**2*U*1.2D1 + MW2**4*2.D1 - MW2**2*S*U*2.D1)} \\
 \;\text{     $\&$      *PaVe(0D0,List(T),List(ME2,ME2)))/} \\
 \;\text{     $\&$   ((MW2**2 - S*U*1D0)**2*(-(D*1D0) + 2D0)*} \\
 \;\text{     $\&$     (-(T*1D0) + MW2*4D0)**2) - } \\
 \;\text{     $\&$  (1.25D-1*(-(D**2*MW2*S**6*1D0) - D**2*S**7*1D0 - } \\
 \;\text{     $\&$       D**2*S**3*U**4*1D0 + D*S**3*T**4*2D0 - } \\
 \;\text{     $\&$       D**2*MW2**3*S**3*U*2D0 - D**2*MW2*S**5*U*2D0 - } \\
 \;\text{     $\&$       D**2*MW2**5*S**2*4D0 - D**2*S**6*U*4D0 - } \\
 \;\text{     $\&$       D**2*MW2**5*U**2*4D0 - D**2*S**4*U**3*4D0 + } \\
 \;\text{     $\&$       D**2*MW2**3*S**4*6D0 - D**2*S**5*U**2*6D0 - } \\
 \;\text{     $\&$       D**2*MW2*S**3*U**3*6D0 - } \\
 \;\text{     $\&$       D**2*MW2*S**2*U**4*7D0 + D**2*MW2**2*S**5*8D0 - } \\
 \;\text{     $\&$       D**2*MW2**5*S*U*8D0 + } \\
 \;\text{     $\&$       D**2*MW2**2*S**3*U**2*8D0 + } \\
 \;\text{     $\&$       D**2*MW2**3*S*U**3*1.D1 - } \\
 \;\text{     $\&$       D**2*MW2**4*S**3*1.6D1 + } \\
 \;\text{     $\&$       D**2*MW2**2*S**4*U*1.6D1 + } \\
 \;\text{     $\&$       D**2*MW2**3*S**2*U**2*1.8D1 + } \\
 \;\text{     $\&$       D*MW2**5*T**2*2.8D1 + D*MW2*S**2*T**4*3.8D1 + } \\
 \;\text{     $\&$       D*MW2*S**5*T*4.D1 + MW2**7*4.8D1 + } \\
 \;\text{     $\&$       MW2**5*T*U*4.8D1 + MW2**3*S**2*U**2*4.8D1 + } \\
 \;\text{     $\&$       MW2*S**2*T*U**3*4.8D1 + D*MW2**3*S*T**3*6.4D1 - } \\
 \;\text{     $\&$       D*MW2**2*S**5*9.6D1 - MW2**5*S*U*9.6D1 - } \\
 \;\text{     $\&$       MW2**3*S*T*U**2*9.6D1 + D*MW2*S**3*T**3*1.D2 + } \\
 \;\text{     $\&$       D*MW2*S**4*T**2*1.2D2 + D*MW2**7*1.28D2 - } \\
 \;\text{     $\&$       D*MW2**6*T*1.28D2 - MW2**6*U*1.44D2 - } \\
 \;\text{     $\&$       MW2**2*S**2*U**3*1.44D2 + MW2**4*S*U**2*2.88D2 - } \\
 \;\text{     $\&$       D*MW2**2*S**2*T**3*3.24D2 - } \\
 \;\text{     $\&$       D*MW2**4*S*T**2*4.2D2 + D*MW2**3*S**4*5.12D2 - } \\
 \;\text{     $\&$       D*MW2**2*S**4*T*5.28D2 - D*MW2**6*S*6.08D2 - } \\
 \;\text{     $\&$       D*MW2**2*S**3*T**2*7.16D2 + } \\
 \;\text{     $\&$       D*MW2**5*S*T*8.88D2 - D*MW2**4*S**3*1.088D3 + } \\
 \;\text{     $\&$       D*MW2**3*S**2*T**2*1.116D3 + } \\
 \;\text{     $\&$       D*MW2**5*S**2*1.152D3 + } \\
 \;\text{     $\&$       D*MW2**3*S**3*T*1.568D3 - D*MW2**4*S**2*T*1.84D3)} \\
 \;\text{     $\&$      *PaVe(0D0,List(MW2,MW2,T),List(ME2,0D0,ME2)))/} \\
 \;\text{     $\&$   ((MW2**2 - S*U*1D0)**3*(-(D*1D0) + 2D0)*} \\
 \;\text{     $\&$     (-(D*1D0) + 3D0)*(-(T*1D0) + MW2*4D0)**2) - } \\
 \;\text{     $\&$  (D*S**2*T*2.5D-1*(MW2 - S*1D0)**2*} \\
 \;\text{     $\&$     PaVe(0D0,List(MW2,S,ME2),List(ME2,0D0,0D0)))/} \\
 \;\text{     $\&$   ((MW2**2 - S*U*1D0)**3*(-(D*1D0) + 3D0)) + } \\
 \;\text{     $\&$  (D*S**2*T**2*1.25D-1*(MW2 - S*1D0)*} \\
 \;\text{     $\&$     PaVe(0D0,List(T,ME2,ME2),List(ME2,ME2,0D0)))/} \\
 \;\text{     $\&$   ((MW2**2 - S*U*1D0)**3*(-(D*1D0) + 3D0)) - } \\
 \;\text{     $\&$  (D*S**3*T**2*1.25D-1*(MW2 - S*1D0)*} \\
 \;\text{     $\&$     PaVe(0D0,List(MW2,MW2,ME2,ME2,T,S),} \\
 \;\text{     $\&$      List(ME2,0D0,ME2,0D0)))/} \\
 \;\text{     $\&$   ((MW2**2 - S*U*1D0)**3*(-(D*1D0) + 3D0))} \\
\end{array}$$