(* ::Package:: *)

 


(* ::Section:: *)
(*SplittingFunction*)


(* ::Text:: *)
(*`SplittingFunction[pxy]` is a database of splitting functions in the $\overline{\textrm{MS}}$ scheme.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[AnomalousDimension](AnomalousDimension).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*Unpolarized case:*)


(* ::Text:: *)
(*In general the argument should be a string, but if the variables Pqq etc. have no value, you can omit the "".*)


SplittingFunction[Pqq,Polarization->0]/.FCGV[z_]:>ToExpression[z]


SplittingFunction[Pqg,Polarization->0]/.FCGV[z_]:>ToExpression[z]


SplittingFunction[Pgq,Polarization->0]/.FCGV[z_]:>ToExpression[z]


SplittingFunction[Pgg,Polarization->0]/.FCGV[z_]:>ToExpression[z]


SplittingFunction[aqq,Polarization->0]/.FCGV[z_]:>ToExpression[z]


SplittingFunction[agq,Polarization->0]/.FCGV[z_]:>ToExpression[z]


SplittingFunction[aqg,Polarization->0]/.FCGV[z_]:>ToExpression[z]


SplittingFunction[agg,Polarization->0]/.FCGV[z_]:>ToExpression[z]


(* ::Text:: *)
(*Polarized case:*)


SplittingFunction[Pqq,Polarization->1]/.FCGV[z_]:>ToExpression[z]


SplittingFunction[Pqg,Polarization->1]/.FCGV[z_]:>ToExpression[z]


SplittingFunction[Pgq,Polarization->1]/.FCGV[z_]:>ToExpression[z]


SplittingFunction[Pgg,Polarization->1]/.FCGV[z_]:>ToExpression[z]


SplittingFunction[aqq,Polarization->1]/.FCGV[z_]:>ToExpression[z]


SplittingFunction[agq,Polarization->1]/.FCGV[z_]:>ToExpression[z]


SplittingFunction[agqd,Polarization->1]/.FCGV[z_]:>ToExpression[z]


SplittingFunction[aqg,Polarization->1]/.FCGV[z_]:>ToExpression[z]


SplittingFunction[aqgd,Polarization->1]/.FCGV[z_]:>ToExpression[z]


SplittingFunction[agg,Polarization->1]/.FCGV[z_]:>ToExpression[z]


SplittingFunction[aggd,Polarization->1]/.FCGV[z_]:>ToExpression[z]


SplittingFunction[PQQS,Polarization->1]/.FCGV[z_]:>ToExpression[z]


SplittingFunction[PQQNS,Polarization->1]/.FCGV[z_]:>ToExpression[z]


SplittingFunction[PQG,Polarization->1]/.FCGV[z_]:>ToExpression[z]


SplittingFunction[PGQ,Polarization->1]/.FCGV[z_]:>ToExpression[z]


SplittingFunction[PGG,Polarization->1]/.FCGV[z_]:>ToExpression[z]
