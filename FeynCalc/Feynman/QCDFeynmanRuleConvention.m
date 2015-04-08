(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: QCDFeynmanRuleConvention *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: added Sept. 28th 2003 in order to adapt to various conventions of
factors of I etc. *)
(* ------------------------------------------------------------------------ *)

(* :Summary: QCDFeynmanRuleConvention *)

(* ------------------------------------------------------------------------ *)

QCDFeynmanRuleConvention::usage =
"QCDFeynmanRuleConvention[GluonPropagator],
QCDFeynmanRuleConvention[GluonGhostVertex], etc. can be set to
different values. The default is the Itzykson-Zuber convention. ";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`QCDFeynmanRuleConvention`Private`"]

QCDFeynmanRuleConvention[_] = 1;

FCPrint[1,"QCDFeynmanRuleConvention.m loaded"];
End[]
