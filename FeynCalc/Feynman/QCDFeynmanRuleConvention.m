(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: QCDFeynmanRuleConvention											*)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 1990-2015 Rolf Mertig
	Copyright (C) 1997-2015 Frederik Orellana
	Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary: Feynman rule convention for QCD								*)

(* ------------------------------------------------------------------------ *)

QCDFeynmanRuleConvention::usage =
"QCDFeynmanRuleConvention[GluonPropagator], \
QCDFeynmanRuleConvention[GluonGhostVertex], etc. can be set to \
different values. The default is the Itzykson-Zuber convention. ";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`QCDFeynmanRuleConvention`Private`"]

QCDFeynmanRuleConvention[_] = 1;

FCPrint[1,"QCDFeynmanRuleConvention.m loaded"];
End[]
