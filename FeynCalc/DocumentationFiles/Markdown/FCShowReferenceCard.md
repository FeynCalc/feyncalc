##  FCShowReferenceCard 

FCShowReferenceCard[{name}]  shows the reference card that corresponds to "name". Reference cards are stored in Tables/ReferenceCards inside the FeynCalc main directory. FCShowReferenceCard[] lists available reference cards..

###  Examples 

```mathematica
FCShowReferenceCard[] 
 
FCShowReferenceCard[{"FeynArts"}]
class	self - conj .	indices	members	mass
F[1]
(neutrinos)
```

$$\left(
\begin{array}{c}
 \text{FeynArts} \\
\end{array}
\right)$$

$$![03npjzt7mks8w](img/03npjzt7mks8w.png)$$

$$\text{class} \text{self}-\text{mass} \text{members} \text{conj}.\text{indices}$$

$$F(1)$$

$$\text{neutrinos}$$

```mathematica
	no 
 
```

$$\text{no}$$

```mathematica
	Generation 
 
```

$$\text{Generation}$$

```mathematica
	F[1, {1}]    Subscript[\[Nu], e]  
  F[1, {2}]   Subscript[\[Nu], \[Mu]]  
  F[1, {3}]   Subscript[\[Nu], \[Tau]]  
  
 	0 
  0 
  0
```

$$\nu _e F(1,\{1\})$$

$$\nu _{\mu } F(1,\{2\})$$

$$\nu _{\tau } F(1,\{3\})$$

$$0$$

$$0$$

$$0$$

```mathematica
F[2]
(massive leptons)
```

$$F(2)$$

$$\text{leptons} \text{massive}$$

```mathematica
	no 
 
```

$$\text{no}$$

```mathematica
	Generation 
 
```

$$\text{Generation}$$

```mathematica
	F[2, {1}]  e  
  F[2, {2}]  \[Mu]  
  F[2, {3}]  \[Tau]  
  
 	ME 
  MM 
  ML
```

$$e F(2,\{1\})$$

$$\mu  F(2,\{2\})$$

$$\tau  F(2,\{3\})$$

$$\text{ME}$$

$$\text{MM}$$

$$\text{ML}$$

```mathematica
F[3]
(up - type quarks)
```

$$F(3)$$

$$\text{up}-\text{quarks} \text{type}$$

```mathematica
	no 
 
```

$$\text{no}$$

```mathematica
	Generation 
  Color
```

$$\text{Generation}$$

$$\text{Color}$$

```mathematica
	F[3, {1, o}]  u  
  F[3, {2, o}]  c  
  F[3, {3, o}]  t  
  
 	MU 
  MC 
  MT
```

$$u F(3,\{1,o\})$$

$$c F(3,\{2,o\})$$

$$t F(3,\{3,o\})$$

$$\text{MU}$$

$$\text{MC}$$

$$\text{MT}$$

```mathematica
F[4]
(down - type quarks)
```

$$F(4)$$

$$\text{down}-\text{quarks} \text{type}$$

```mathematica
	no 
 
```

$$\text{no}$$

```mathematica
	Generation 
  Color
```

$$\text{Generation}$$

$$\text{Color}$$

```mathematica
	F[4, {1, o}]  d  
  F[4, {2, o}]  s  
  F[4, {3, o}]  b  
  
 	MD 
  MS 
  MB
```

$$d F(4,\{1,o\})$$

$$s F(4,\{2,o\})$$

$$b F(4,\{3,o\})$$

$$\text{MD}$$

$$\text{MS}$$

$$\text{MB}$$

```mathematica
V[1]
V[2]
V[3]
V[4] (mixing field) 
 
	yes
yes
no
yes 
  
 	

```

$$V(1)$$

$$V(2)$$

$$V(3)$$

$$\text{field} \text{mixing} V(4)$$

$$\text{yes}$$

$$\text{yes}$$

$$\text{no}$$

$$\text{yes}$$

```mathematica
	V[1]  \[Gamma]  
  V[2]  Z  
  V[3]  W^-  
      V[4]  \[Gamma] - Z  
  
 	0 
  MZ 
  MW 
  MAZ
```

$$\gamma  V(1)$$

$$V(2) Z$$

$$\gamma  V(3) W^{-V(4)}-Z$$

$$0$$

$$\text{MZ}$$

$$\text{MW}$$

$$\text{MAZ}$$

```mathematica
S[1]
S[2]
S[3] 
 
	yes
yes
no 
  
 	

```

$$S(1)$$

$$S(2)$$

$$S(3)$$

$$\text{yes}$$

$$\text{yes}$$

$$\text{no}$$

```mathematica
	S[1]  H  
  S[2]  G^0  
  S[3]  G^-  
     
    	MH 
  MG0 
  MGp
```

$$H S(1)$$

$$S(2)$$

$$S(3) G^{-\text{MH}}$$

$$\text{MG0}$$

$$\text{MGp}$$

```mathematica
U[1]
U[2]
U[3]
U[4] 
 
	no
no
no
no 
  
 	
 

```

$$U(1)$$

$$U(2)$$

$$U(3)$$

$$U(4)$$

$$\text{no}$$

$$\text{no}$$

$$\text{no}$$

$$\text{no}$$

```mathematica
	U[1]  Subscript[u, \[Gamma]]  
  U[2]  Subscript[u, Z]  
  U[3]  Subscript[u, -]  
  U[4]  Subscript[u, +]  
  
 	0 
  MZ 
  MW 
  MW
```

$$U(1) u_{\gamma }$$

$$U(2) u_Z$$

```mathematica
SV[2] (mixing field)
SV[3] (mixing field) 
 
	yes
no 
 
	
```

$$\text{field} \text{mixing} \text{SV}(2)$$

$$\text{field} \text{mixing} \text{SV}(3)$$

$$\text{yes}$$

$$\text{no}$$

```mathematica
	SV[2]  G^0 - Z  
  SV[3]  G^--W^-  
      
     	MZ 
  MW
```

$$\text{SV}(2)-Z$$

$$![0u6dwhs3h34cm](img/0u6dwhs3h34cm.png)$$

$$\text{SV}(3) G^{(\text{--}W)^{-\text{MZ}}}$$

$$\text{MW}$$

```mathematica
The following fields are avaialble via the SMQCD extension :	\[SpanFromLeft]	\[SpanFromLeft]	\[SpanFromLeft]	\[SpanFromLeft]
V[5]
U[5] 
 
	yes
no 
 
	Gluon
Gluon 
 
	V[5, {i}]  Subscript[g, i]  
U[5, {i}]  Subscript[u, Subscript[g, i]]  
 
	0
0
```

$$\text{are} \text{avaialble} \text{fields} \text{following} \text{SMQCD} \text{the} \text{The} \text{via} \left(\text{extension}:^4\right)$$

$$V(5)$$

$$U(5)$$

$$\text{yes}$$

$$\text{no}$$

$$\text{Gluon}$$

$$\text{Gluon}$$

$$g_i V(5,\{i\})$$

$$u_{g_i} U(5,\{i\})$$

$$0$$

$$0$$

```mathematica
Comments : V[4] is commented out by default in SM . mod;
	SV[2] and SV[3] must be enabled with $SVMixing = True .	\[SpanFromLeft]	\[SpanFromLeft]	\[SpanFromLeft]	\[SpanFromLeft]

```

$$![0l8tozj01q81p](img/0l8tozj01q81p.png)$$

$$^3 \text{True}.$$