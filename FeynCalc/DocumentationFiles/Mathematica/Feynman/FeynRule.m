 
(* ::Section:: *)
(* FeynRule *)
(* ::Text:: *)
(*FeynRule[lag, {fields}] derives the Feynman rule corresponding to the field configuration $\text{fields}$ of the lagrangian $\text{lag}$.FeynRule does not calculate propagator Feynman rules.The option ZeroMomentumInsertion can be used for twist-2 and higher twist operators.FeynRule is not very versatile and was primarily developed for QCD calculations. It is often more useful when dealing with bosonic fields than with fermions. If you need a more powerful and universal solution for deriving Feynman rules, have a look at the standalone Mathematica Package FeynRules (not related to FeynCalc)..*)


(* ::Subsection:: *)
(* Examples *)
Lagrangian

(* ::Text:: *)
(*Derive the Feyman rule for the $phi ^4$ theory.*)


- \[Lambda]/4! QuantumField[\[Phi]]^4

FeynRule[%,{QuantumField[\[Phi]][p1],QuantumField[\[Phi]][p2],QuantumField[\[Phi]][p3],QuantumField[\[Phi]][p4]}]


(* ::Text:: *)
(*Derive the Feyman rule for the quark-gluon vertex*)


I QuantumField[AntiQuarkField].GA[\[Mu]].CovariantD[\[Mu]].QuantumField[QuarkField]

FeynRule[%,{QuantumField[GaugeField,{\[Mu]},{a}][p1],QuantumField[QuarkField][p2],QuantumField[AntiQuarkField][p3]}]


(* ::Text:: *)
(*Derive the Feyman rule for the 4-gluon vertex.*)


-(1/4) FieldStrength[\[Alpha],\[Beta],i].FieldStrength[\[Alpha],\[Beta],i]

FeynRule[%,{QuantumField[GaugeField,{\[Mu]},{a}][p1],QuantumField[GaugeField,{\[Nu]},{b}][p2],QuantumField[GaugeField,{\[Rho]},{c}][p3],QuantumField[GaugeField,{\[Sigma]},{d}][p4]}]

GluonVertex[{p,\[Mu],a},{q,\[Nu],b},{r,\[Rho],c},{s,\[Sigma],d},Dimension->4,Explicit->True]

FCCanonicalizeDummyIndices[%-%%]//Factor


(* ::Text:: *)
(*Derive the Feyman rule for the 3-gluon vertex.*)


-(1/4) FieldStrength[\[Alpha],\[Beta],i].FieldStrength[\[Alpha],\[Beta],i]

FeynRule[%,{QuantumField[GaugeField,{\[Mu]},{a}][p],QuantumField[GaugeField,{\[Nu]},{b}][q],QuantumField[GaugeField,{\[Rho]},{c}][r]}]

GluonVertex[{p,\[Mu],a},{q,\[Nu],b},{r,\[Rho],c},Dimension->4,Explicit->True]

ExpandScalarProduct[%-%%]//Factor


(* ::Text:: *)
(*Derive the Feynman rules for the Higgs EFT.*)


-(1/4) CH FieldStrength[mu,nu,a].FieldStrength[mu,nu,a].QuantumField[H]


(* ::Text:: *)
(*Higgs-gg vertex.*)


FeynRule[%,{QuantumField[GaugeField,{i},{a}][p1],QuantumField[GaugeField,{j},{b}][p2],QuantumField[H][p3]}]


(* ::Text:: *)
(*Higgs-ggg vertex.*)


FeynRule[%%,{QuantumField[GaugeField,{i},{a}][p1],
	QuantumField[GaugeField,{j},{b}][p2],QuantumField[GaugeField,{k},{c}][p3],QuantumField[H][p4]}]//Simplify


(* ::Text:: *)
(*Higgs-gggg vertex.*)


FeynRule[%%%,{QuantumField[GaugeField,{i},{a}][p1],QuantumField[GaugeField,{j},{b}][p2],QuantumField[GaugeField,{k},{c}][p3],
QuantumField[GaugeField,{l},{d}][p4],QuantumField[H][p5]}]//
FCCanonicalizeDummyIndices[#,SUNIndexNames->{e}]&//Collect2[#,SUNF,
FCFactorOut-> I CH SMP["g_s"]^2]&


(* ::Text:: *)
(*Some OPE-related examples:*)



(* ::Text:: *)
(*2-gluon Feynman rules (unpolarized).*)


Lagrangian["ogu"]

FeynRule[%,{QuantumField[GaugeField,{\[Mu]},{a}][p],QuantumField[GaugeField,{\[Nu]},{b}][q]},ZeroMomentumInsertion->False]//Factor


(* ::Text:: *)
(*2-gluon Feynman rules (polarized).*)


Lagrangian["ogp"]

FeynRule[%,{QuantumField[GaugeField,{\[Mu]},{a}][p],QuantumField[GaugeField,{\[Nu]},{b}][q]},ZeroMomentumInsertion->False]//Factor

Factor2[Calc[%/.p->-q,Assumptions->Automatic]]


(* ::Text:: *)
(*Compare with the Feynman rule tabulated in Twist2GluonOperator.*)


Twist2GluonOperator[q,{\[Mu],a},{\[Nu],b},Polarization->1,Explicit->True]


(* ::Text:: *)
(*quark-quark -gluon-gluon Feynman rule (unpolarized).*)


Lagrangian["oqu"]

FeynRule[%,{QuantumField[QuarkField][p],QuantumField[AntiQuarkField][q],QuantumField[GaugeField,{\[Mu]},{a}][r],QuantumField[GaugeField,{\[Nu]},{b}][s]},ZeroMomentumInsertion->True,InitialFunction->Identity]

Twist2QuarkOperator[{p},{q},{r,\[Mu],a},{s,\[Nu],b},Polarization->0]

Calc[%-%%/.OPEm->5/.s->-p-q-r/.D->4]
