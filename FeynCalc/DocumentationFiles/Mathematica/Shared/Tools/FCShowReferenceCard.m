 
(* ::Section:: *)
(* FCShowReferenceCard *)
(* ::Text:: *)
(*FCShowReferenceCard[{name}]  shows the reference card that corresponds to "name". Reference cards are stored in Tables/ReferenceCards inside the FeynCalc main directory. FCShowReferenceCard[] lists available reference cards..*)


(* ::Subsection:: *)
(* Examples *)
FCShowReferenceCard[]

FCShowReferenceCard[{"FeynArts"}]
class	self-conj.	indices	members	mass
F[1]
(neutrinos)


	no



	Generation



	F[1, {1}]    Subscript[\[Nu], e]  
F[1, {2}]   Subscript[\[Nu], \[Mu]]  
F[1, {3}]   Subscript[\[Nu], \[Tau]]  

	0
0
0


F[2]
(massive leptons)


	no



	Generation



	F[2, {1}]  e  
F[2, {2}]  \[Mu]  
F[2, {3}]  \[Tau]  

	ME
MM
ML


F[3]
(up-type quarks)


	no



	Generation
Color


	F[3, {1, o}]  u  
F[3, {2, o}]  c  
F[3, {3, o}]  t  

	MU
MC
MT


F[4]
(down-type quarks)


	no



	Generation
Color


	F[4, {1, o}]  d  
F[4, {2, o}]  s  
F[4, {3, o}]  b  

	MD
MS
MB


V[1]
V[2]
V[3]
V[4] (mixing field)

	yes
yes
no
yes

	



	V[1]  \[Gamma]  
V[2]  Z  
V[3]  W^-  
V[4]  \[Gamma]-Z  

	0
MZ
MW
MAZ


S[1]
S[2]
S[3]

	yes
yes
no

	



	S[1]  H  
S[2]  G^0  
S[3]  G^-  

	MH
MG0
MGp


U[1]
U[2]
U[3]
U[4]

	no
no
no
no

	




	U[1]  Subscript[u, \[Gamma]]  
U[2]  Subscript[u, Z]  
U[3]  Subscript[u, -]  
U[4]  Subscript[u, +]  

	0
MZ
MW
MW


SV[2] (mixing field)
SV[3] (mixing field)

	yes
no

	


	SV[2]  G^0-Z  
SV[3]  G^--W^-  

	MZ
MW


The following fields are avaialble via the SMQCD extension:	\[SpanFromLeft]	\[SpanFromLeft]	\[SpanFromLeft]	\[SpanFromLeft]
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


Comments: V[4] is commented out by default in SM.mod;
	SV[2] and SV[3] must be enabled with $SVMixing = True.	\[SpanFromLeft]	\[SpanFromLeft]	\[SpanFromLeft]	\[SpanFromLeft]

