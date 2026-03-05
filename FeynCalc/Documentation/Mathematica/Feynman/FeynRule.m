(* ::Package:: *)

 


(* ::Section:: *)
(*FeynRule*)


(* ::Text:: *)
(*`FeynRule[lag, {fields}]` derives the Feynman rule corresponding to the field configuration `fields` of the Lagrangian `lag`.*)


(* ::Text:: *)
(*`FeynRule` does not calculate propagator Feynman rules.*)


(* ::Text:: *)
(*`FeynRule` is not very versatile and was primarily developed for QCD calculations. It is often more useful when dealing with bosonic fields than with fermions. If you need a more powerful and universal solution for deriving Feynman rules, have a look at the standalone Mathematica Package FeynRules (not related to FeynCalc).*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*$\phi ^4$ Feynman rule*)


- \[Lambda]/4! QuantumField[\[Phi]]^4

FeynRule[%,{QuantumField[\[Phi]][p1],QuantumField[\[Phi]][p2],
QuantumField[\[Phi]][p3],QuantumField[\[Phi]][p4]}]


(* ::Text:: *)
(*Quark-gluon vertex Feynman rule*)


I QuantumField[AntiQuarkField] . GA[\[Mu]] . CovariantD[\[Mu]] . QuantumField[QuarkField]

FeynRule[%,{QuantumField[GaugeField,{\[Mu]},{a}][p1],
QuantumField[QuarkField][p2],QuantumField[AntiQuarkField][p3]}]


(* ::Text:: *)
(*4-gluon vertex Feynman rule*)


-(1/4) FieldStrength[\[Alpha],\[Beta],i] . FieldStrength[\[Alpha],\[Beta],i]

FeynRule[%,{QuantumField[GaugeField,{\[Mu]},{a}][p1],QuantumField[GaugeField,{\[Nu]},{b}][p2],
QuantumField[GaugeField,{\[Rho]},{c}][p3],QuantumField[GaugeField,{\[Sigma]},{d}][p4]}]

GluonVertex[{p,\[Mu],a},{q,\[Nu],b},{r,\[Rho],c},{s,\[Sigma],d},Dimension->4,Explicit->True]

FCCanonicalizeDummyIndices[%-%%]//Factor


(* ::Text:: *)
(*3-gluon vertex Feynman rule*)


-(1/4) FieldStrength[\[Alpha],\[Beta],i] . FieldStrength[\[Alpha],\[Beta],i]

FeynRule[%,{QuantumField[GaugeField,{\[Mu]},{a}][p],QuantumField[GaugeField,{\[Nu]},{b}][q],
QuantumField[GaugeField,{\[Rho]},{c}][r]}]

GluonVertex[{p,\[Mu],a},{q,\[Nu],b},{r,\[Rho],c},Dimension->4,Explicit->True]

ExpandScalarProduct[%-%%]//Factor


(* ::Text:: *)
(*Higgs EFT interaction vertex*)


heftInt=-(1/4) CH FieldStrength[mu,nu,a] . FieldStrength[mu,nu,a] . QuantumField[H]


(* ::Text:: *)
(*$Hgg$ vertex Feynman rules*)


FeynRule[heftInt,{QuantumField[GaugeField,{i},{a}][p1],QuantumField[GaugeField,
{j},{b}][p2],QuantumField[H][p3]}]


(* ::Text:: *)
(*$Hggg$ vertex Feynman rules*)


FeynRule[heftInt,{QuantumField[GaugeField,{i},{a}][p1],QuantumField[GaugeField,
{j},{b}][p2],QuantumField[GaugeField,{k},{c}][p3],QuantumField[H][p4]}]//Simplify


(* ::Text:: *)
(*$Hgggg$ vertex Feynman rules*)


FeynRule[heftInt,{QuantumField[GaugeField,{i},{a}][p1],QuantumField[GaugeField,{j},
{b}][p2],QuantumField[GaugeField,{k},{c}][p3],
QuantumField[GaugeField,{l},{d}][p4],QuantumField[H][p5]}]//
FCCanonicalizeDummyIndices[#,SUNIndexNames->{e}]&//Collect2[#,SUNF,
FCFactorOut-> I CH SMP["g_s"]^2]&
