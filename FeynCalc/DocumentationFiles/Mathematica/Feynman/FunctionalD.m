 
(* ::Section:: *)
(* FunctionalD *)
(* ::Text:: *)
(*FunctionalD[exp, {QuantumField[name, LorentzIndex[mu], ..., SUNIndex[a]][p], ...}] calculates the functional derivative of exp with respect to the QuantumField list (with incoming momenta $\text{p}$, etc.) and does the Fourier transform.   FunctionalD[expr, {QuantumField[name, LorentzIndex[mu], ... SUNIndex[a]], ...}] calculates the functional derivative and does partial integration but omits the $\text{x}$-space delta functions.FunctionalD is a low level function used in FeynRule..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*FeynRule, QuantumField.*)



(* ::Subsection:: *)
(* Examples *)
(* ::Text:: *)
(*Instead of the usual $delta phi (x)/delta phi (y)=delta ^{(D)}(x-y)$ the arguments and the $delta$ function are omitted, i.e., for the program for simplicity: $delta phi /delta phi =1$.*)


FunctionalD[QuantumField[\[Phi]],QuantumField[\[Phi]]]

FunctionalD[QuantumField[\[Phi]]^2,QuantumField[\[Phi]]]


(* ::Text:: *)
(*Instead of the usual  $left.left(delta  partial _{mu }phi (x)right)right/delta phi (y)=partial _{mu }delta ^{(D)}(x-y)$ the arguments are omitted, and the $text{cell}left(text{TextData}left[text{cell}left(partial _{mu },text{Input}right)right],text{InlineFormula}right)$ operator is specified by default to be an integration by parts operator, i.e., the right hand side will be just $text{Null}$ or, more precisely, (by default)$-vec{partial }_{mu }$.*)


FunctionalD[QuantumField[FCPartialD[\[Mu]],\[Phi]],QuantumField[\[Phi]]]

$S[\phi ] =1/2\int  d^Dx \left[ \partial _ {\mu }\phi (x) \partial ^{\mu }\phi (x)-m^2\phi (x) \phi (y)\r ight]$
(QuantumField[FCPartialD[\[Mu]],\[Phi]].QuantumField[FCPartialD[\[Mu]],\[Phi]]-m^2 QuantumField[\[Phi]].QuantumField[\[Phi]])/2

FunctionalD[%,QuantumField[\[Phi]]]

$S[A] = -\int  d^Dx \frac{1}{4} F_a^{\mu \n u }(x) F_{\text{$\mu \n u $a}}(x)$

(* ::Text:: *)
(*First approach:*)


F1=FieldStrength[\[Mu],\[Nu],a,{A,b,c},1,Explicit->True]

F2=FieldStrength[\[Mu],\[Nu],a,{A,d,e},1,Explicit->True]

S[A]=-1/4F1.F2


(* ::Text:: *)
(*In order to derive the equation of motion, the functional derivative of $text{S}$ with respect to $A_{sigma }^g$ has to be set to zero. Bearing in mind that for FeynCalc we have to be precise as to where which operators (coming from the substitution of the derivative of the delta function) act.*)



(* ::Text:: *)
(*Act with the functional derivative operator on the first field strength:*)


$\left.0 = (\text{$\delta $S})\left/\left(\delta A_ {\sigma }^g(y)\r ight)\r ight.=-2/4\int d^Dx \left(\delta \left/\left(\delta A_ {\sigma }^g(y)\r ight)\r ight.\right. F_{\text{$\mu \n u $a}}(x)\r ight) F_a^{,\mu \n u }(x)$

(* ::Text:: *)
(* See what happens with just $(text{$delta $S}[A])left/left(delta A_{sigma }^gright)right.$.*)


Ag=QuantumField[A,{\[Sigma]},{g}]

FunctionalD[F1,Ag]


(* ::Text:: *)
(*Use FCCanonicalizeDummyIndices to minimize the number of dummy indices.*)


t1=FCCanonicalizeDummyIndices[%,SUNIndexNames->{c1}]/.c1->c


(* ::Text:: *)
(*Instead of inserting the definition for the second $F_a^{mu nu }$, introduce a QuantumField object with antisymmetry built into the Lorentz indices:*)


F/: QuantumField[pard___,F,\[Beta]_,\[Alpha]_,s_]:=-QuantumField[pard,F,\[Alpha],\[Beta],s]/;!OrderedQ[{\[Beta],\[Alpha]}]
QuantumField[F,{\[Mu],\[Nu]},{a}]

%/.{\[Mu]:>\[Nu],\[Nu]:>\[Mu]}

t2=Contract[ExpandPartialD[-1/2 t1.QuantumField[F,LorentzIndex[\[Mu]],LorentzIndex[\[Nu]],SUNIndex[a]]]]/.Dot->Times

t3=FCCanonicalizeDummyIndices[t2,LorentzIndexNames->{mu},SUNIndexNames->{aa,cc}]/.{mu->\[Mu],aa->a,cc->c}

t4=FCE[t3]/. SUNF[a,c,g]->-SUNF[g,c,a]


(* ::Text:: *)
(*Since the variational derivative vanishes $text{t4}$ implies that $0=D_{mu }F_g^{mu sigma }$.*)



(* ::Text:: *)
(*Second approach:*)



(* ::Text:: *)
(*It is of course also possible to do the functional deriviate on the $S[A]$ with both field strength tensors inserted.*)


S[A]

r1=FunctionalD[S[A],Ag]


(* ::Text:: *)
(*This is just funcional derivation and partial integration and simple contraction of indices. At first no attempt is made to rename dummy indices (since this is difficult in general).*)



(* ::Text:: *)
(*With a general replacement rule only valid for commuting fields the color indices can be canonicalized a bit more. The idea is to use the commutative properties of the vector fields, and canonicalize the color indices by a trick.*)


Commutator[QuantumField[aaa___FCPartialD,A,bbb__],QuantumField[ccc___FCPartialD,A,ddd__]]=0;
r2=r1//DotSimplify//FCCanonicalizeDummyIndices[#,SUNIndexNames->{a1,b1,c1,d1},LorentzIndexNames->{mu,nu,rho}]&//ReplaceAll[#,{a1->a,b1->b,c1->c,d1->d,mu->\[Mu],nu->\[Nu],rho->\[Rho]}]&//Collect2[#,SUNF]&


(* ::Text:: *)
(*Inspection reveals that still terms are the same. Gather the terms with two $f$'s:*)


twof=Select[r2//Expand,Count[#,SUNF[__]]===2&]

twofnew=((twof[[1]]/.{a->b,b->a})+twof[[2]])//DotSimplify

r3 = r2-twof+twofnew


(* ::Text:: *)
(*Check that this is now indeed the same as the $text{t4}$ result from the first attempt.*)


t4

w0=RightPartialD[\[Mu]].FieldStrength[\[Mu],\[Sigma],g,{A,a,b},1]+QuantumField[A,LorentzIndex[\[Mu]],SUNIndex[c]].FieldStrength[\[Mu],\[Sigma],a,{A,b,d},1]SUNF[g,c,a]

w1=Explicit[w0]

w2=ExpandPartialD[w1]//DotSimplify

dif1 = w2-r3


(* ::Text:: *)
(*As expected:*)


dif1//FCCanonicalizeDummyIndices


(* ::Text:: *)
(*Finally, unset the commutator of the bosonic fields.*)


UnDeclareCommutator[QuantumField[aaa___FCPartialD,A,bbb__],QuantumField[ccc___FCPartialD,A,ddd__]]=0;