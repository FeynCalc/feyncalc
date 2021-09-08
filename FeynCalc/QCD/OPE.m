(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: OPEi*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  the m *)

(* ------------------------------------------------------------------------ *)

OPEi::usage=
"OPEi etc. are variables with DataType PositiveInteger which are used in
functions relating to the operator product expansion.";

OPEj::usage=
"OPEj is a dummy index in OPESum.";

OPEk::usage=
"OPEk is a dummy index in OPESum.";

OPEl::usage=
"OPEl is a dummy index in OPESum.";

OPEm::usage=
"OPEm is a dummy index in OPESum.";

OPEn::usage=
"OPEn is a dummy index in OPESum.";

OPEo::usage=
"OPEo is a dummy index in OPESum.";

OPEDelta::usage=
"OPEDelta is a lightlike axial vector as used e.g. in the operator product
expansion in QCD.";

(* ------------------------------------------------------------------------ *)

Begin["`OPE`Package`"]
End[]

Begin["`OPE`Private`"]

DataType[OPEi, PositiveInteger] = True;
DataType[OPEj, PositiveInteger] = True;
DataType[OPEk, PositiveInteger] = True;
DataType[OPEl, PositiveInteger] = True;
DataType[OPEm, PositiveInteger] = True;
DataType[OPEn, PositiveInteger] = True;
DataType[OPEo, PositiveInteger] = True;

ScalarProduct[OPEDelta, OPEDelta] = 0;

(* Since we set a new permanent SP here, we need to update
all the saved DownValues to ensure that this setting will
not be lost after FCClearScalarProducts[] *)

(* 	TODO There should be a dedicated function for that. This
	functionality will be often needed by add-ons*)

initialPairDownValues = DownValues[Pair];
initialScalarProductDownValues = DownValues[ScalarProduct];
initialScalarProductUpValues = UpValues[ScalarProduct];
initialSPDownValues = DownValues[SP];
initialSPDDownValues = DownValues[SPD];

(* that is only for Partial* stuff *)
LorentzIndex[Momentum[OPEDelta]^p_.] := Momentum[OPEDelta]^p;

(* :Summary:  the variable selecting out the ope-insertions *)
OPEDelta/:
	MakeBoxes[OPEDelta, TraditionalForm] := "\[CapitalDelta]";

OPEi /:
	MakeBoxes[OPEi ,TraditionalForm] :=
		"i";

OPEj /:
	MakeBoxes[OPEj ,TraditionalForm] :=
		"j";

OPEk /:
	MakeBoxes[OPEk ,TraditionalForm] :=
		"k";

OPEl /:
	MakeBoxes[OPEl ,TraditionalForm] :=
		"l";

OPEm /:
	MakeBoxes[OPEm ,TraditionalForm] :=
		"m";

OPEn /:
	MakeBoxes[OPEn ,TraditionalForm] :=
		"n";
OPEo /:
	MakeBoxes[OPEo ,TraditionalForm] :=
		"o";

FCPrint[1,"OPE loaded."];
End[]


