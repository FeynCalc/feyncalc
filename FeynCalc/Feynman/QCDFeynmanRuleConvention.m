(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: QCDFeynmanRuleConvention											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Feynman rule convention for QCD								*)

(* ------------------------------------------------------------------------ *)

QCDFeynmanRuleConvention::usage =
"QCDFeynmanRuleConvention fixes the sign convention in the QCD Feynman rules
for the ghost propagator and the ghost-gluon vertex.This is done by setting
the value of QCDFeynmanRuleConvention[GhostPropagator] and
QCDFeynmanRuleConvention[GluonGhostVertex].

The default values are 1 for both, which corresponds to the convention used in
most books. Setting them to -1 enforces the convention that can be found e.g.
in the book \"Applications of Perturbative QCD\" by R. Field.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`QCDFeynmanRuleConvention`Private`"]

QCDFeynmanRuleConvention[GhostPropagator] = 1;
QCDFeynmanRuleConvention[GluonGhostVertex] = 1;

FCPrint[1,"QCDFeynmanRuleConvention.m loaded"];
End[]
