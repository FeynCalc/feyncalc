(* Wolfram Language package *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Channels (Phi)													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Utilities for kinematics, isospin, etc			    		*)

(* ------------------------------------------------------------------------ *)

MassArguments::usage =
"MassArguments is an option for AmplitudeProjection relevant when \
OnMassShell is set to True, specifying the optional extra arguments supplied \
to ParticleMass.  Default value : {RenormalizationState[0]}.";

FieldProjection::usage =
"FieldProjection[IsoVector[QuantumField[Particle[p]]],opts] returns the \
field specified by  the option Channel as a linear combination of the \
isospin components of QuantumField[Particle[p]].";

AmplitudeProjection::usage =
"AmplitudeProjection[amp,opts], where amp[i1,i2,...] is a function of the \
isospin indices i1, i2, ... of the external particles, returns the amplitude \
of the channel specified by the option Channel.  AmplitudeProjection uses the \
rules specified in $IsoSpinProjectionRules.  NOTICE:  For the particles under \
consideration, PHI must know their anti-particles.  That is, e.g. \
ChargeConjugate[PionPlus] is PionMinus.";


$IsoSpinProjectionRules::usage =
"$IsoSpinProjectionRules is a set of rules used by FieldProjection and \
AmplitudeProjection to project out in channels.  Changing the default setting \
of this quantity should be done with care.  Notice that the setting of \
$IsoSpinProjectionRules is related to the values of \
WriteOutUmatrices[UGeneratorMatrix[i,opts]], where i is an integer.  The \
default set of rules is not very large, but more rules in the same syntax may \
easily be added.";

$SUNCompletenessRules::usage =
"$SUNCompletenessRules is an environment \
variable used by SUNReduce to (hopefully) simplify expressions involving products of \
traces of products of generator matrices (UMatrix[UGenerator[SUNIndex[j]]]) \
with other matrices.";

Channel::usage =
"Channel is an option of FieldProjection and AmplitudeProjection, \
specifying which channel the field or amplitude should be projected out in.  \
For FieldProjection, the possible settings are listed in $Particles.  For \
AmplitudeProjection, the possible settings are \
{{p1,p2,...}->{pp1,pp2,...},{i,...}}, where  p1,.. and pp1,... are generic \
particles from $Particles like Pion etc. and i is the isospin, or simply \
{{p1,p2,...}->{pp1,pp2,...}}, where p1,.. and pp1,... now have to be \
iso-eigenstates like PionPlus etc..  Default value : PionPlus for \
FieldProjection and {{Pion,Pion}->{Pion,Pion},{2}} for AmplitudeProjection.";

USum::usage =
"USum is a summation that works like Sum.  It may be faster that Sum for \
multiple summations where each summation reduces the number of terms.  This \
is typically the case in isospin indices summations.";

USumHeld::usage =
"USumHeld is a symbol substituted for USum when the option HoldSums of \
SUNReduce is set to True.";

SUNReduce::usage =
"SUNReduce[a] finds SU(n) objects, simplifies using SU(2) or SU(3) rules \
and sums over pairs of indices not in $ConstantIsoIndices.  Indices are \
summed over only if they have head SUNIndex.  NOTICE: With the option \
Explicit set to True, large expressions that can be expanded to \
a sum, should be expanded before applying SUNReduce (this will reduce \
computation time dramatically).";

SUDFSymmetrize::usage =
"SUDFSymmetrize[exp] renames factors multiplying SUND[i,j,k], \
SU3D[i,j,k], SUNF[i,j,k], SU2F[i,j,k] or SU3F[i,j,k] in an attempt to \
reduce exp.";

IsoFunction::usage =
"IsoFunction is a head recognized by SUNReduce, so that for e.g. \
IsoFunction[f][SUNIndex[i]*SUNDelta[SUNIndex[i],SUNIndex[j]] ocurring in an \
expression will imply a sum over SUNIndex[i].";

HoldSums::usage =
"HoldSums is an option for SUNReduce relevant \
when the option Explicit is set to True.  When set to True, the \
isospin summations are not performed and USum is substituted with USumHeld.  \
Default value : True.";

IndicesCleanup::usage =
"IndicesCleanup[expr] renames dummy indices in expr in a systematic way \
in order to get cancellations and a simpler expression. The expression expr \
should be in PHI notation, that is, involving the products NM and/or Times, \
not DOT or NonCommutativeMultiply.\n
NOTICE : IndicesCleanup does not work \
properly when the indices are nested more than one level down in factors.  \
The only exceptions to this are terms like NM[UTrace[NM[a]],UTrace[NM[b]]] with \
a and b having isospin or Lorentz index dependence.  For these however, it may \
be necessary to apply IndicesCleanup repeatedly.\n
NOTICE : IndicesCleanup does not work for D-dimensional Lorentz indices like \
LorentzIndex[li, D].";

CNM::usage =
"CNM[a,b] renames contracted Lorentz and SU(N) indices.";

ExtendedCleanup::usage =
"ExtendedCleanup is an option for IndicesCleanup.  When set to True, \
contractions within a mixing of Times and NM will also be renamed, so that in \
a mixed product there will be no multiple pairs of the same index.  When set \
to False,  there will be multiple pairs of the same index in mixed products.  \
This will give wrong results with e.g. FeynRule.  Default value : True.";

FCleanup::usage =
"FCleanup is an option for IndicesCleanup.  When set to True, special \
attention is given to renaming the indices of SU2F, SU3F and SUNF in order to \
get cancellations.  Default value : False.";

IsoDummy::usage =
"IsoDummy is a head used by IndicesCleanup for temporary renaming of \
indices.";

NM1::usage =
"NM1 is a head used by IndicesCleanup for temporary renaming of Times.";

NM2::usage =
"NM2 is a head used by IndicesCleanup for temporary renaming of DOT.";

IsoExternDummy::usage =
"IsoExternDummy is a head used by IndicesCleanup for temporary renaming \
of indices.";

IsoInternDummy::usage =
"IsoInternDummy is a head used by IndicesCleanup for temporary renaming \
of indices.";

LorentzDummy::usage =
"LorentzDummy is a head used by IndicesCleanup for temporary renaming of \
indices.";

LorentzExternDummy::usage =
"LorentzExternDummy is a head used by IndicesCleanup for temporary \
renaming of indices.";

LorentzInternDummy::usage =
"LorentzInternDummy is a head used by IndicesCleanup for temporary \
renaming of indices.";

DerivativeExternDummy::usage =
"DerivativeExternDummy is a head used by IndicesCleanup for temporary \
renaming of indices.";

DerivativeInternDummy::usage =
"DerivativeInternDummy is a head used by IndicesCleanup for temporary \
renaming of indices.";

IsoDummys::usage =
"IsoDummys is an option for IndicesCleanup and CNM.  It should be a list of two \
three strings.  Default value : {\"j\",\"k\",\"l\"}.";

LorentzDummys::usage =
"LorentzDummys is an option for IndicesCleanup and CNM.  It should be a list of \
five strings.  Default value : {\"\[Xi]\",\"\[Rho]\",\"\[Sigma]\",\"\[Tau]\",\
\"\[Omega]\"}.";


Begin["`Package`"]
End[]

Begin["`Channels`Private`"];

(*
Boxes
*)

sumstart[{_, ss_, _, _}] :=
	ss;

sumstart[{_, ss_, _}] :=
	ss;

sumstart[{_, _}] :=
	1;

sumstart[{_}] :=
	1;

sumvar[{ss_, __}] :=
	MakeBoxes[TraditionalForm[ss]];

sumvar[{_}] :=
	".";

sv[bb__, rr_] :=
	sumvar[{bb}[[rr]]];

sumeq[{_}] :=
	".";

sumeq[{_, __}] :=
	"=.";

USumHeld /:
	MakeBoxes[USumHeld[a__, b : _List ..], TraditionalForm] :=
		RowBox[Join[Table[UnderoverscriptBox["\[CapitalSigma]", RowBox[{sv[b, rep], sumeq[{b}[[rep]]],
		sumstart[{b}[[rep]]]}], {b}[[rep]][[-1]]], {rep, Length[{b}]}], {MakeBoxes[TraditionalForm[a]]}]];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

fccombs :=
	fccombs = CombinationLists;

(* FeynArts functions *)
faso :=
	faso = FeynArts`SumOver;

(* Defaults *)
$IsoSpinProjectionRules = {
	PionPlus -> (Iso[PhiMeson, {1}] - I*Iso[PhiMeson, {2}])/Sqrt[2],
	PionMinus -> (Iso[PhiMeson, {1}] + I*Iso[PhiMeson, {2}])/Sqrt[2],
	PionZero -> Iso[PhiMeson, {3}],
	KaonPlus -> (Iso[PhiMeson, {4}] - I*Iso[PhiMeson, {5}])/Sqrt[2],
	KaonMinus -> (Iso[PhiMeson, {4}] + I*Iso[PhiMeson, {5}])/Sqrt[2],
	KaonZero -> (Iso[PhiMeson, {6}] - I*Iso[PhiMeson, {7}])/Sqrt[2],
	KaonZeroBar -> (Iso[PhiMeson, {6}] + I*Iso[PhiMeson, {7}])/Sqrt[2],
	EtaMeson -> Iso[PhiMeson, {8}]
};

Options[FieldProjection] = {
	Channel -> PionPlus
};

Options[AmplitudeProjection] = {
	Channel -> {{Pion, Pion} -> {Pion, Pion}, 2},
	OnMassShell -> True,
	MassArguments -> {RenormalizationState[0]},
	MomentumVariablesString -> "p"
};

Options[SUNReduce] = {
	HoldSums -> True,
	CommutatorReduce -> False,
	Explicit -> False,
	FullReduce -> False,
	SUNN -> 2,
	UDimension -> Automatic
};

Options[IndicesCleanup] :=
	{IsoDummys -> {"j", "k", "l"},
	LorentzDummys -> {"\[Xi]", "\[Rho]", "\[Sigma]", "\[Tau]", "\[Omega]"},
	ExtendedCleanup -> True,
	FCleanup -> False,
	CommutatorReduce -> True
};

Options[CNM] = {
	IsoDummys -> {"p", "q", "r", "s"},
	LorentzDummys -> {"\[Zeta]", "\[Eta]", "\[Theta]", "\[Kappa]"}
};


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Isospin simplification and summation *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* A summation function for multiple sums that remembers previously calculated
	sums and uses these for the ensuing summations: *)

USum[a_, b___List] :=
	(
	sumlims[i_] :=
		({b}[[i]]);
	recfun[0] = a;

	Do[	recfun[rep] = Sum1[recfun[rep - 1], sumlims[rep]] /. Sum1 -> Sum,
		{rep, 1,Length[{b}]}
	];

	recfun[Length[{b}]]
	);

USum[a_, {}] :=
	a;

USumHeld[a_] :=
	a;



(* Finding pairs in a list of indices (ignoring integers and elements from
	$ConstantIsoIndices): *)

su3summationindices[{a__}, gennr2_] :=
	(
	us = (Complement[Union[Complement[Sort[Flatten[{a}]], Sort[Flatten[{a}]] //. {cc___, b_, b_, c___} -> {cc, c}]],
	Join[$ConstantIsoIndices, SUNIndex/@ $ConstantIsoIndices]]) //. {{cc___, _Integer, c___} -> {cc, c},
	{cc___, (fcsuni|ExplicitSUNIndex)[_Integer], c___} -> {cc, c}};

	Table[{removesun[us[[icount]]], gennr2}, {icount, Length[us]}]
	);

su3summationindices[{}, ___] :=
	{};

(* Support functions for SUNReduce for removing SUNIndex and UIndex heads: *)

removesun[(SUNIndex| ExplicitSUNIndex | UIndex)[iii_]] :=
	iii;

removesun[iii /; FreeQ[iii, (SUNIndex| ExplicitSUNIndex | UIndex)]] :=
	iii;

(* Support functions for SUNReduce for removing SUNIndex and UIndex heads from constants: *)

removesunc[(SUNIndex| ExplicitSUNIndex | UIndex)[iii_]] /; (IntegerQ[iii] || (!FreeQ[$ConstantIsoIndices, iii])) :=
	tmpsuni[iii];

removesunc[(SUNIndex| ExplicitSUNIndex)[iii_]] /; (!IntegerQ[iii] && (FreeQ[$ConstantIsoIndices, iii])) :=
	fcsuni[iii];

removesunc[UIndex[iii_]] /; (!IntegerQ[iii] && (FreeQ[$ConstantIsoIndices, iii])) :=
	UIndex[iii];

removesunc[iii_] /; FreeQ[{iii}, (SUNIndex| ExplicitSUNIndex | UIndex)] :=
	iii;


(* Adding a head to the inner index of PhiProjection: *)
(* This PhiProjection business is not necessary. Dropped, 11/5-2003 *)
(*NTo3Rules1 = (PhiProjection[pa_, ___][pb_]) :> SUNDelta[pa, pb];*)
NTo3Rules3 = {
	(SUNDelta|SU2Delta|SU3Delta)[i_, j:((fcsuni)[_])] QuantumField[a___, j_, b___][x_]  -> QuantumField[a, fcsuni[i], b][x]
};


(* Removing heads from constants and changing from FeynCalc to PHI functions: *)

NTo3Rules2[2] = {
	SU2D[_, _, _] :> 0,
	SU2F[x1_, x2_, x3_] :> SU2F[removesunc[x1], removesunc[x2], removesunc[x3]],
	SU2Delta[x1_, x2_] :> SU2Delta[removesunc[x1], removesunc[x2]],
	SUND[_, _, _] :> 0,
	SUNF[x1_, x2_, x3_] :> SU2F[removesunc[x1], removesunc[x2], removesunc[x3]],
	SUNDelta[x1_, x2_] :> SU2Delta[removesunc[x1], removesunc[x2]]
};

NTo3Rules2[3] = {
	SU3D[x1_, x2_, x3_] :> SU3D[removesunc[x1], removesunc[x2], removesunc[x3]],
	SU3F[x1_, x2_, x3_] :> SU3F[removesunc[x1], removesunc[x2], removesunc[x3]],
	SU3Delta[x1_, x2_] :> SU3Delta[removesunc[x1], removesunc[x2]],
	SUND[x1_, x2_, x3_] :> SU3D[removesunc[x1], removesunc[x2], removesunc[x3]],
	SUNF[x1_, x2_, x3_] :> SU3F[removesunc[x1], removesunc[x2], removesunc[x3]],
	SUNDelta[x1_, x2_] :> SU3Delta[removesunc[x1], removesunc[x2]]
};

NTo3iRules2[2] = {
	SUND[_, _, _] :> 0,
	SUNF :> SU2F,
	SUNDelta :> SU2Delta
};

NTo3iRules2[3] = {
	SUND :> SU3D,
	SUNF :> SU3F,
	SUNDelta :> SU3Delta
};

NTo3Rules2[_] =
	{};

NTo3iRules2[_] =
	{};

(* A function for selecting the members of a list with head SUNIndex: *)

su3test[a_] :=
	{} /; Head[a]=!=SUNIndex;

su3test[a_SUNIndex] :=
	a;

su3list[{a___}] :=
	Flatten[su3test /@ Flatten[{a}]];

(* Distributivity: *)

SUNReduce1[aa_, OptionsPattern[]] /; (FreeQ[aa, SUNIndex| ExplicitSUNIndex]) :=
	aa;

SUNReduce1[aa_ bb_, opts:OptionsPattern[]] /; (FreeQ[aa, SUNIndex| ExplicitSUNIndex]) :=
	aa*SUNReduce1[bb, opts];

SUNReduce1[aa_Plus, opts___] :=
	SUNReduce1[#, opts] & /@ aa;

SUNReduce1[aa_, opts___] /; (((Explicit /. Flatten[{opts}] /. Options[SUNReduce]) == True) && FreeQ[{aa}, Plus]) :=
	SUNReduce2[aa, opts];

SUNReduce1[aa_, opts___] /; ((Explicit /. Flatten[{opts}] /. Options[SUNReduce]) == False) :=
	SUNReduce2[aa, opts];


(* Explicit summation: *)

SUNReduce2[aa_, opts___] /; (Explicit /. Flatten[{opts}] /. Options[SUNReduce]) == True :=
	(
	FCPrint[2, "Finding indices to sum over"];
	gennr1 = If[ FreeQ[aa, _SU3F | _SU3D | _SU3Delta] && FreeQ[aa, SUNN -> 3],
				(SUNN^2 - 1 /. Flatten[{opts}] /.
				Options[SUNReduce]),
				8
			];

	flatlist = List1[(aa /. NTo3Rules2[(SUNN /. Flatten[{opts}] /. Options[SUNReduce])]) /.
		{ NM -> List1, Times -> List1}(*writing out powers*)/. ((at_ /; !FreeQ[at, fcsuni])^ex_Integer :>
		Table[NM[at], {ex}])];

	flatlist1 = Flatten[flatlist /. List1 -> List];

	sumlist =
		su3summationindices[
			su3list[Flatten[(flatlist1 //. {
				ff_[a___, fcsuni[ind_], b___][_] /; FreeQ[ff, List] -> {ff[a, b], fcsuni[ind]},
				ff_[fcsuni[ind_], b___][_] /; FreeQ[ff, List] -> {ff[b], fcsuni[ind]},
				ff_[a___, fcsuni[ind_]][_] /; FreeQ[ff, List] -> {ff[a], fcsuni[ind]},
				ff_[a___, fcsuni[ind_], b___] /; FreeQ[ff, List] -> {ff[a, b], fcsuni[ind]},
				ff_[fcsuni[ind_], b___] /; FreeQ[ff, List] -> {ff[b], fcsuni[ind]},
				ff_[a___, fcsuni[ind_]] /; FreeQ[ff, List] -> {ff[a], fcsuni[ind]}})]], gennr1
		];

	FCPrint[3, "Found:\n", sumlist];
	FCPrint[2, "Summing"];
	tmpres = USum1[(aa  /. NTo3Rules2[(SUNN /. Flatten[{opts}] /. Options[SUNReduce])]), ##] & @@ sumlist /.
			If[ (HoldSums /. Flatten[{opts}] /. Options[SUNReduce]),
				USum1 -> USumHeld,
				USum1 -> USum
			];

	If[ (CommutatorReduce /. Flatten[{opts}] /. Options[SUNReduce]),
		FCPrint[2,"Applying CommutatorReduce"];
		CommutatorReduce[tmpres,opts],
		tmpres
	]
	);



(* To determine if there are identical indices in the two f-functions of a
product, we have to compare them all pairwise, that is,  construct all
possible combinations with Outer: *)

twoselect[{a_, b_, c_}, {d_, e_, f_}] :=
	twoselect[{a, b, c}, {d, e, f}] =
		Select[Join @@ Outer[List, {a, b, c}, {d, e, f}], (#[[1]] == #[[2]] && Head[#[[1]]] === SUNIndex&& Head[#[[2]]] === fcsuni) &];

(* This function gives the signature of the permutations needed to turn one e.g.
{a,b,cc},{c,a,b} into {a,b,cc},{a,b,c}: *)

sig1[{___, a_, mm___, b_, ___}, {___, a_, m___, b_, ___}] :=
	(-1)^(Length[{mm}] - Length[{m}]);

sig1[{___, a_, mm___, b_, ___}, {___, b_, m___, a_, ___}] :=
	(-1)^(Length[{mm}] - Length[{m}] + 1);


(* This function gives the signature of the permutations needed to turn one e.g.
{a,b,c},{c,aa,bb} into {a,b,c},{aa,bb,c}: *)
(*Bug fixed 25/5-2001: There might be an identical constant index in
	the two argument lists besides the index to be permuted*)

sig2[{a___, m_?((!IntegerQ[#] && Head[#] =!= tmpsuni)&), ___}, {aa___, m_?((!IntegerQ[#] && Head[#] =!= tmpsuni)&), ___}] :=
	(-1)^(Length[{a}] - Length[{aa}]);



(* Implicit summation: *)

(*the delta functions are orderless, so we need not be carefull with the order*)
$SUNDeltaRules = {
	SU3Delta[fcsuni[i_], fcsuni[i_]] :> 8,
	SU3Delta[i_, fcsuni[j_]]*SU3Delta[fcsuni[j_], k_] :> SU3Delta[i, k],
	SU3Delta[i_Integer|(i:tmpsuni[_Integer]), i_Integer|(i:tmpsuni[_Integer])] :> 1,
	SU3Delta[i:tmpsuni[_]|_Integer, j:tmpsuni[_]|_Integer]^_ :> SU3Delta[i, j],
	SU3Delta[i:tmpsuni[_Integer]|_Integer, k_] SU3Delta[j:tmpsuni[_Integer]|_Integer, k_] :> 0/; ((i/.tmpsuni->Identity) =!= (j/.tmpsuni->Identity)),
	SU3Delta[tmpsuni[_], fcsuni[_]]^n_ /; EvenQ[n] :> 1,
	SU3Delta[fcsuni[_], tmpsuni[_]]^n_ /; EvenQ[n] :> 1,
	SU3Delta[_Integer, fcsuni[_]]^n_ /; EvenQ[n] :> 1,
	SU3Delta[fcsuni[_], _Integer]^n_ /; EvenQ[n] :> 1,
	SU3Delta[i_?((IntegerQ[#] || Head[#] === tmpsuni) &), j_?((IntegerQ[#] || Head[#] === tmpsuni) &)]^_ :> SU3Delta[i, j],
	SU3Delta[fcsuni[_], fcsuni[_]]^n_ /; EvenQ[n] :> 8^(n/2),
	SU3Delta[i_, j_SUNIndex] expr_ :> (expr /. j -> i) /; (!FreeQ[expr, j] &&
		(FreeQ[expr, QuantumField[___, j, ___]] || !(IntegerQ[i]||Head[i]===tmpsuni&&IntegerQ[i[[1]]]))),
	SU2Delta[fcsuni[i_], fcsuni[i_]] :> 3,
	SU2Delta[i_, fcsuni[j_]]*SU2Delta[fcsuni[j_], k_] :> SU2Delta[i, k],
	SU2Delta[i_Integer|(i:tmpsuni[_Integer]), i_Integer|(i:tmpsuni[_Integer])] :> 1,
	SU2Delta[i:tmpsuni[_]|_Integer, j:tmpsuni[_]|_Integer]^_ :> SU2Delta[i, j],
	SU2Delta[i:tmpsuni[_Integer]|_Integer, k_] SU2Delta[j:tmpsuni[_Integer]|_Integer, k_] :> 0 /; ((i/.tmpsuni->Identity) =!= (j/.tmpsuni->Identity)),
	SU2Delta[tmpsuni[_], fcsuni[_]]^n_ /; EvenQ[n] :> 1,
	SU2Delta[fcsuni[_], tmpsuni[_]]^n_ /; EvenQ[n] :> 1,
	SU2Delta[_Integer, fcsuni[_]]^n_ /; EvenQ[n] :> 1,
	SU2Delta[fcsuni[_], _Integer]^n_ /; EvenQ[n] :> 1,
	SU2Delta[i_?(IntegerQ[#] || Head[#] === tmpsuni), j_?(IntegerQ[#] || Head[#] === tmpsuni)]^_ :> SU2Delta[i, j],
	SU2Delta[fcsuni[_], fcsuni[_]]^n_ /; EvenQ[n] :> 3^(n/2),
	SU2Delta[i_, j_SUNIndex] expr_ :> (expr /. j -> i) /; (!FreeQ[expr, j] &&
		(FreeQ[expr, QuantumField[___, j, ___]] || !(IntegerQ[i]||Head[i]===tmpsuni&&IntegerQ[i[[1]]])))
};

$SUNDFRules = {
	(*contraction of three indices*)
	SU2F[_SUNIndex, _SUNIndex, _SUNIndex]^2 :> 6,
	SU3F[_SUNIndex, _SUNIndex, _SUNIndex]^2 :> 24,
	SU3D[_SUNIndex, _SUNIndex, _SUNIndex]^2 :> 40/3,

	(*contraction of two indices*)
	SU2F[a_, b_, c_]^2 /; (Sort[Head /@ ({a, b, c}/.tmpsuni->Identity)] == {Integer, fcsuni, fcsuni}) :> 2,
	SU3F[a_, b_, c_]^2 /; (Sort[Head /@ ({a, b, c}/.tmpsuni->Identity)] == {Integer, fcsuni, fcsuni}) :> 3,
	SU3D[a_, b_, c_]^2 /; (Sort[Head /@ ({a, b, c}/.tmpsuni->Identity)] == {Integer, fcsuni, fcsuni}) :> 5/3,
	(SU2F[a_, b_, c_]*SU2F[d_, e_, f_]*rest___) /; (Length[twoselect[{a, b, c}, {d, e, f}]] == 2) :>
			2 (SU2Delta @@ Complement[{a, b, c, d, e, f}, (Join @@ twoselect[{a, b, c}, {d, e, f}])])*
			(sig1[{a, b, c}, {d, e, f}]) rest,
	(SU3F[a_, b_, c_]*SU3F[d_, e_, f_] rest___) /; (Length[twoselect[{a, b, c}, {d, e, f}]] == 2) :>
			3*(SU3Delta @@ Complement[{a, b, c, d, e, f}, (Join @@ twoselect[{a, b, c}, {d, e, f}])])*
			(sig1[{a, b, c}, {d, e, f}]) rest,
	(SU3D[a_, b_, c_]*SU3D[d_, e_, f_] rest___) /; (Length[twoselect[{a, b, c}, {d, e, f}]] == 2) :>
			rest*5/3*SU3Delta @@Complement[{a, b, c, d, e, f}, (Join @@ twoselect[{a, b, c}, {d, e, f}])],
			(SU3D[a_, b_, c_]*SU3F[d_, e_, f_]*___) /; (Length[twoselect[{a, b, c}, {d, e, f}]] == 2) :> 0,
	(*contraction of one index - is this formula true for SU(3)?*)
	(*NO it's not - fixed, but correct formula not really useful, 16/12/1999*)
	(SU2F[a_, b_, c_]*SU2F[d_, e_, f_]*rest___) /; (Length[twoselect[{a, b, c}, {d, e, f}]] == 1) :>
			(((SU2Delta[#1, #3]*SU2Delta[#2, #4] - SU2Delta[#2, #3]*SU2Delta[#1, #4])& @@
			Join[Complement[{a, b, c}, {twoselect[{a, b, c}, {d, e, f}][[1, 1]]}], Complement[{d, e, f},
			{twoselect[{a, b, c}, {d, e, f}][[1, 1]]}]]) sig2[{a, b, c}, {d, e, f}]) rest,
	(SU2F[a_, b_, c_]^2) /; (Length[twoselect[{a, b, c}, {a, b, c}]] == 1) :>
			(((SU2Delta[#1, #3]*SU2Delta[#2, #4] - SU2Delta[#2, #3]*SU2Delta[#1, #4])& @@
			Join[Complement[{a, b, c}, {twoselect[{a, b, c}, {a, b, c}][[1, 1]]}],
			Complement[{a, b, c}, {twoselect[{a, b, c}, {a, b, c}][[1, 1]]}]])*sig2[{a, b, c}, {a, b, c}]),
	(*	Integers should already be sorted and put first - the following should also take care of SU2F[a_integer, c_integer,
		d_SUNIndex]^2, ...	*)
	SU2F[a_, b_Integer, c_Integer] :> (-1)^(Complement[{1, 2, 3}, {b, c}][[1]] + 1)*(SU2Delta[#, a] & @@ (Complement[{1, 2, 3}, {b, c}])),
	SU2F[a_Integer, b_Integer, c_] :> (-1)^(Complement[{1, 2, 3}, {a, b}][[1]] + 1)*(SU2Delta[#, c] & @@ (Complement[{1, 2, 3}, {a, b}])),
	SU2F[a_, (b:tmpsuni[_Integer]), (c:tmpsuni[_Integer])] :> (-1)^(Complement[{1, 2, 3}, {b[[1]], c[[1]]}][[1]] + 1)*
			(SU2Delta[#, a] & @@ (Complement[tmpsuni/@{1, 2, 3}, {b, c}])),
	SU2F[(a:tmpsuni[_Integer]), (b:tmpsuni[_Integer]), c_] :> (-1)^(Complement[{1, 2, 3}, {a[[1]], b[[1]]}][[1]] + 1)*
			(SU2Delta[#, c] & @@ (Complement[tmpsuni/@{1, 2, 3}, {a, b}]))
};

$SUNRules =
	Join[$SUNDeltaRules, $SUNDFRules, $SU3FReduceList, $SU3DReduceList];

(*Completeness of SU(N) - see Gasser & Leutwyler 1985*)
$SUNCompletenessRules = {
	HoldPattern[NM[UTrace1[NM[f___, UMatrix[UGenerator[SUNIndex[j_]]], a___]], UTrace1[NM[g___, UMatrix[UGenerator[SUNIndex[j_]]], b___]]]] :>
			2 UTrace[NM[a, f, b, g]] - 2/SUNN NM[UTrace[NM[a, f]], UTrace[NM[b, g]]],
			HoldPattern[Times[UTrace1[NM[f___, UMatrix[UGenerator[SUNIndex[j_]]], a___]],
	UTrace1[NM[g___, UMatrix[UGenerator[SUNIndex[j_]]], b___]]]] :>
			2 UTrace[NM[a, f, b, g]] + -2/SUNN NM[UTrace[NM[a, f]], UTrace[NM[b, g]]],

	HoldPattern[UTrace1[NM[f___, UMatrix[UGenerator[SUNIndex[_]]], a___]]^2] :>
			2 UTrace[NM[a, f, a, f]] - 2/SUNN NM[UTrace[NM[a, f]], UTrace[NM[a, f]]],

	HoldPattern[UTrace1[NM[f___, UMatrix[UGenerator[SUNIndex[j_]]], a___, UMatrix[UGenerator[SUNIndex[j_]]], b___]]] :>
			-2/SUNN UTrace[NM[a, b, f]] + 2NM[UTrace[NM[a]], UTrace[NM[b, f]]]
};

applyCompletenessRules[expr_, opts___Rule] :=
	expr /. $SUNCompletenessRules /. SUNN -> (SUNN /. Flatten[{opts}] /. Options[SUNReduce]) /.
	(UTrace1[]|UTrace[]) -> Phi`Objects`Private`gaugedimcheck[SUNReduce, opts, expr];

SUNReduce2[aa_, opts___] /; ((Explicit /. Flatten[{opts}] /. Options[SUNReduce]) == False) :=
	Block[ {bb, cc},
		FCPrint[2, "Applying reduction rules on ", StandardForm[aa]];
		FCPrint[3, "Reduction rules are:\n", $SUNRules];
		bb = aa  /. NTo3Rules3 /. NTo3Rules2[(SUNN /. Flatten[{opts}] /.Options[SUNReduce])];
		FCPrint[2, "Entered SUNReduce2; expression is:\n", StandardForm[bb]];
		cc = bb /. NTo3iRules2[(SUNN /. Flatten[{opts}] /. Options[SUNReduce])];
		FCPrint[2, "After NTo3iRules2, expression is:\n", StandardForm[cc]];
		(*Changed below, 13/5-2003, to have $SU3FReduceList, $SU3DReduceList from $SUNRules catch ExplicitSUNIndex[1], ... *)
		cc /. ($SUNRules/.(SUNIndex| ExplicitSUNIndex | UIndex)[iii_Integer]->tmpsuni[iii])
	];

SUNReduce[aa_, opts___] /; (((Explicit /. Flatten[{opts}] /. Options[SUNReduce]) == False) && (FullReduce /. Flatten[{opts}] /. Options[SUNReduce]) === True) :=
	Block[ {op},
		op = Join[{FullReduce->False},
		Select[Flatten[{opts}],FreeQ[#,FullReduce,Infinity,Heads->True]]];
		FCPrint[2, "Iterating with options ", op];
		FixedPoint[(SUNReduce[applyCompletenessRules[ExpandAll[#],opts], Sequence@@op]&),aa]
	];

SUNReduce[aa_Plus, opts___] :=
	SUNReduce[#, opts] & /@ aa;


SUNReduce[aa_, opts___] /; ((Explicit /. Flatten[{opts}] /. Options[SUNReduce]) == False && (FullReduce /. Flatten[{opts}] /. Options[SUNReduce]) =!= True) :=
	(
	FCPrint[2,
	"Will use reduction rules"];
	FCPrint[2, "Collecting"];
	Collect[SUNReduce1[Expand[aa]/.
		SUNDelta[	fcsuni[i_?((NumberQ[#] == False && FreeQ[$ConstantIsoIndices, #]) &)],
					fcsuni[i_?((NumberQ[#] == False && FreeQ[$ConstantIsoIndices, #]) &)]] :>
			(SUNN /. Flatten[{opts}] /. Options[SUNReduce])^2 - 1, opts], {_fcfad, _SU2Delta, _SU2F, _SU2D}] /.
				tmpsuni -> SUNIndex/.{faso[_Integer,___] -> 1, faso[(fcsuni|ExplicitSUNIndex)[_Integer],___] -> 1}
	);

SUNReduce[aa_, opts___] /; ((Explicit /. Flatten[{opts}] /. Options[SUNReduce]) == True) :=
	(
	FCPrint[2,
	"Will sum explicitly"];
	FCPrint[2, "Collecting"];
	Collect[SUNReduce1[Expand[aa /. Plus[bb_, bbb__] /; FreeQ[{bb, bbb}, fcsuni] -> tempplus[ttt[Plus[bb, bbb]] /.
	Plus -> ppl]], opts] /. Plus -> ppl /. tempplus[cc___] -> cc /. ppl -> Plus /.
	ttt -> Together, {_fcfad, _SU2Delta, _SU3Delta, _SU2F, _SU3F, _SU3D}] /. tmpsuni -> SUNIndex/. faso[_Integer,___]-> 1
	);


$SymSUNDFRules1 = {
	Times[dd___, (sud:(SUND|SU3D))[a_, b_, c_], d___] :>
		Block[ {inds0, originds, originds0, originds1, inds},
			originds0 = {a, b, c} /. SUNIndex-> Identity;
			(*Select instead of Intersection is used to preserve the order*)
			inds0 = Select[Sort[Cases[{dd, d}, QuantumField[___, fcsuni[_], ___], Infinity, Heads -> True]] /.
				QuantumField[___, fcsuni[e_], ___] -> e, (MemberQ[originds0, #]) &
			];
			originds1 = Select[originds0, (MemberQ[inds0, #]) &];
			originds = Join[Complement[originds0, originds1], originds1];
			inds = Join[Complement[originds, inds0], inds0];
			If[ Length[inds0] > 1 && Length[inds] < 4,
				sud[a, b, c]*(Times[dd, d] /. ((Rule @@ #) & /@ Transpose[{originds, inds}])),
				sud[a, b, c]*Times[dd, d]
			]
		],
	Times[dd___, (suf:(SUNF|SU2F|SU3F))[a_, b_, c_], d___] :>
	Block[ {inds0, originds, originds0, originds1, inds},
		originds0 = {a, b, c} /. SUNIndex-> Identity;
		inds0 =(*Select instead of Intersection is used to preserve the order*)
				Select[Sort[Cases[{dd, d}, QuantumField[___, fcsuni[_], ___], Infinity,
					Heads -> True]] /. QuantumField[___, fcsuni[e_], ___] -> e, (MemberQ[originds0, #]) &];
		originds1 = Select[originds0, (MemberQ[inds0, #]) &];
		originds = Join[Complement[originds0, originds1], originds1];
		inds = Join[Complement[originds, inds0], inds0];
		If[ Length[inds0] > 1 && Length[inds] < 4,
			sig1[originds, inds]*
			suf[a, b, c]*
				(Times[dd, d] /. ((Rule @@ #) & /@ Transpose[{originds, inds}])),
			suf[a, b, c]*Times[dd, d]
		]
	]
				};

$SymSUNDFRules2 = {
	Times[dd___, (sud:(SUND|SU3D))[a_, b_, c_], d___] :>
	Block[ {inds0, originds, originds0, originds1, inds},
		originds0 = {a, b, c} /. SUNIndex-> Identity;
		(*Select instead of Intersection is used to preserve the order*)
		inds0 = Select[Cases[{dd, d}, UMatrix[UGenerator[fcsuni[_]]], Infinity, Heads -> True] /.
			UMatrix[UGenerator[fcsuni[e_]]] -> e, (MemberQ[originds0, #]) &];
		originds1 = Select[originds0, (MemberQ[inds0, #]) &];
		originds = Join[Complement[originds0, originds1], originds1];
		inds = Join[Complement[originds, inds0], inds0];
		If[ Length[inds0] > 1 && Length[inds] < 4,
			sud[a, b, c]*(Times[dd, d] /. ((Rule @@ #) & /@ Transpose[{originds, inds}])),
			sud[a, b, c]*Times[dd, d]
		]
	],
	Times[dd___, (suf:(SUNF|SU2F|SU3F))[a_, b_, c_], d___] :>
		Block[ {inds0, originds, originds0, originds1, inds},
			originds0 = {a, b, c} /. SUNIndex-> Identity;
			(*Select instead of Intersection is used to preserve the order*)
			inds0 = Select[Cases[{dd, d}, UMatrix[UGenerator[fcsuni[_]]], Infinity, Heads -> True] /.
				UMatrix[UGenerator[fcsuni[e_]]] -> e, (MemberQ[originds0, #]) &];
			originds1 = Select[originds0, (MemberQ[inds0, #]) &];
			originds = Join[Complement[originds0, originds1], originds1];
			inds = Join[Complement[originds, inds0], inds0];
			If[ Length[inds0] > 1 && Length[inds] < 4,
				sig1[originds, inds] suf[a, b, c] (Times[dd, d] /. ((Rule @@ #) & /@ Transpose[{originds, inds}])),
				suf[a, b, c] Times[dd, d]
			]
		]
};

SUDFSymmetrize[exp_] :=
	exp /. $SymSUNDFRules1 /. $SymSUNDFRules2;



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Cleaning up indices *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(* Sorting indices *)

(*Clean up contracted indices*)
sortRules1 = {(nm : (Times | NM))[a__, b__] :>
	((nm[a, b] /. ((Rule @@ #) & /@Transpose[{Cases[{a}, LorentzIndex[__], Infinity, Heads -> True],
											Sort[Cases[{a}, LorentzIndex[__], Infinity,
													Heads -> True]]}]))) /; (Sort[
								Cases[{a}, LorentzIndex[__], Infinity, Heads -> True]] ===
							Sort[Cases[{b}, LorentzIndex[__], Infinity, Heads -> True]] &&
						Cases[{a}, LorentzIndex[__], Infinity, Heads -> True] =!=
							Sort[Cases[{a}, LorentzIndex[__], Infinity,
									Heads -> True]]), (nm : (Times | NM))[a__,
				b__] :> (ablors =
						Intersection[
							alors = Cases[{a}, LorentzIndex[__], Infinity, Heads -> True],
							blors = Cases[{b}, LorentzIndex[__], Infinity, Heads -> True]];
						test = (alors =!= {} && blors =!= {} && ablors === {} &&
									Join[alors, blors] =!= Sort[Join[alors, blors]] &&
									Sort[alors] === Sort[Join[Union[alors], Union[alors]]] &&
									Sort[blors] === Sort[Join[Union[blors], Union[blors]]]);
						If[ test,
							nm[a, b] /. ((Rule @@ #) & /@
										Transpose[{Join[alors, blors], Sort[Join[alors, blors]]}]),
							nm[a, b]
						]) /; (Cases[{a}, LorentzIndex[__], Infinity,
								Heads -> True] =!= {} &&
						Cases[{b}, LorentzIndex[__], Infinity, Heads -> True] =!= {} &&
						Join[Cases[{a}, LorentzIndex[__], Infinity, Heads -> True],
								Cases[{b}, LorentzIndex[__], Infinity, Heads -> True]] =!=
							Sort[Join[Cases[{a}, LorentzIndex[__], Infinity, Heads -> True],
									Cases[{b}, LorentzIndex[__], Infinity,
										Heads -> True]]] && ((Sort[#] ===
											Sort[Join[Union[#], Union[#]]]) &[
								Cases[{a}, LorentzIndex[__], Infinity,
									Heads ->
										True]]) && ((Sort[#] ===
											Sort[Join[Union[#], Union[#]]]) &[
								Cases[{b}, LorentzIndex[__], Infinity, Heads -> True]]))};

sortRules2 =
	sortRules1 //. {HoldPattern[LorentzIndex] -> fcsuni, LorentzIndex -> fcsuni};

sortRules3 =
	sortRules1 //. {HoldPattern[LorentzIndex] -> UIndex, LorentzIndex -> UIndex};

(* Cleaning up contracted Lorentz indices. co is the number of factors that will be
	considered; in e.g. u[mu2] gamma[mu1] gamma[mu1] u[mu2], two will suffice. *)

lorentzCleanup[exp_] :=
	(exp1 = exp;
	Do[ders =
		fccombs[
		Union[Cases[exp, LorentzIndex[__], Infinity, Heads -> True]], co];
		ruls = ((RuleDelayed1[Power1[PatternTest1[a_, func[
		And @@ Table[Count1[yo, #[[i]], Infinity, Heads -> True] == 1, {i, co}]]], 2],
		Condition1[ReplaceAll1[a^2, Map1[(Rule1 @@ #) &,
		Transpose1[{Cases1[{a}, LorentzIndex[__], Infinity, Heads -> True], Sort[#]}]]],
		SameQ1[Sort1[Cases1[{a}, LorentzIndex[__], Infinity, Heads -> True]], Sort1[#]] &&
		UnsameQ1[Cases1[{a}, LorentzIndex[__], Infinity, Heads -> True],
		Sort1[#]]]]) & /@ ders) /. PatternTest1 -> PatternTest /.
		Count1 -> Count /. yo -> # /. func -> Function /.
		RuleDelayed1 -> RuleDelayed /. ReplaceAll1 -> ReplaceAll /.
		Condition1 -> Condition /. UnsameQ1 -> UnsameQ /. SameQ1 -> SameQ /.
		Cases1 -> Cases /. Sort1 -> Sort /. ReplaceAll1 -> ReplaceAll /.
		Transpose1 -> Transpose /. Rule1 -> Rule /. Map1 -> Map /. Power1 -> Power;
		exp1 = exp1 /. ruls, {co, 2, 2}];
	exp1);


(* Cleaning up contracted SU(N) indices. co is the number of factors that will be
	considered; in e.g. u[mu2] gamma[mu1] gamma[mu1] u[mu2], two will suffice. *)

sunCleanup[exp_] :=
	(exp1 = exp;
	Do[ders =
		fccombs[
		Union[Cases[exp, fcsuni[__], Infinity, Heads -> True]], co];
		ruls = ((RuleDelayed1[Power1[PatternTest1[a_, func[
		And @@ Table[Count1[yo, #[[i]], Infinity, Heads -> True] == 1, {i, co}]]], 2],
		Condition1[ReplaceAll1[a^2, Map1[(Rule1 @@ #) &,
		Transpose1[{Cases1[{a}, fcsuni[__], Infinity, Heads -> True], Sort[#]}]]],
		SameQ1[Sort1[Cases1[{a}, fcsuni[__], Infinity, Heads -> True]], Sort1[#]] &&
		UnsameQ1[Cases1[{a}, fcsuni[__], Infinity, Heads -> True],
		Sort1[#]]]]) & /@ ders) /. PatternTest1 -> PatternTest /.
		Count1 -> Count /. yo -> # /. func -> Function /.
		RuleDelayed1 -> RuleDelayed /. ReplaceAll1 -> ReplaceAll /.
		Condition1 -> Condition /. UnsameQ1 -> UnsameQ /. SameQ1 -> SameQ /.
		Cases1 -> Cases /. Sort1 -> Sort /. ReplaceAll1 -> ReplaceAll /.
		Transpose1 -> Transpose /. Rule1 -> Rule /. Map1 -> Map /. Power1 -> Power;
		exp1 = exp1 /. ruls, {co, 2, 2}];
	exp1);

(* Sort differentation Lorentz indices. *)

SetAttributes[nmm, Flat];

sortDiff :=
	(# //.
						FieldDerivative[f_, x_, LorentzIndex[li_]] :>
							nmm[Derivativex[x, LorentzIndex[li]], f] /.
					nmm[dl : (Derivativex[_, LorentzIndex[_]] ..)] :>
						nmm @@ Sort[{dl}] //.
				nmm[Derivativex[x_, LorentzIndex[li_]], f_] :>
					FieldDerivative[f, x, LorentzIndex[li]] /;
						FreeQ[f, Derivativex, Infinity] /. nmm -> NM)&;

(* Change order of differentation if appropriate. *)

lorentzDerCleanupRules = {HoldPattern[
				a__ UTrace1[
						NM[b___,
							FieldDerivative[
								fd : (FieldDerivative[f_, x_, LorentzIndex[li2_]] |
											Adjoint[
												FieldDerivative[f_, x_, LorentzIndex[li2_]]]), x_,
								LorentzIndex[li1_]],
							c___]]] :> (a UTrace1[
								NM[b, FieldDerivative[
										fd /. {li1 -> li2, li2 -> li1}, x, LorentzIndex[li2]],
									c]] /. {li1 -> li2, li2 -> li1}) /;
				Cases[{a, b, c}, LorentzIndex[__], Infinity,
						Heads -> True] === {LorentzIndex[li1], LorentzIndex[li2]},
		HoldPattern[
				a__ UTrace1[
						NM[b___,
							Adjoint[FieldDerivative[
									fd : (FieldDerivative[f_, x_, LorentzIndex[li2_]] |
												Adjoint[
													FieldDerivative[f_, x_, LorentzIndex[li2_]]]),
									x_, LorentzIndex[li1_]]],
							c___]]] :> (a UTrace1[
								NM[b, Adjoint[
										FieldDerivative[
											fd /. {li1 -> li2, li2 -> li1}, x, LorentzIndex[li2]]],
									c]] /. {li1 -> li2, li2 -> li1}) /;
				Cases[{a, b, c}, LorentzIndex[__], Infinity,
						Heads -> True] === {LorentzIndex[li1], LorentzIndex[li2]},
		HoldPattern[
				NM[a__, UTrace1[
						NM[b___,
							FieldDerivative[
								fd : (FieldDerivative[f_, x_, LorentzIndex[li2_]] |
											Adjoint[
												FieldDerivative[f_, x_, LorentzIndex[li2_]]]), x_,
								LorentzIndex[li1_]], c___]]]] :> (NM[a,
							UTrace1[NM[b,
									FieldDerivative[
										fd /. {li1 -> li2, li2 -> li1}, x, LorentzIndex[li2]],
									c]]] /. {li1 -> li2, li2 -> li1}) /;
				Cases[{a, b, c}, LorentzIndex[__], Infinity,
						Heads -> True] === {LorentzIndex[li1], LorentzIndex[li2]},
		HoldPattern[
				NM[a__, UTrace1[
						NM[b___,
							Adjoint[FieldDerivative[
									fd : (FieldDerivative[f_, x_, LorentzIndex[li2_]] |
												Adjoint[
													FieldDerivative[f_, x_, LorentzIndex[li2_]]]),
									x_, LorentzIndex[li1_]]], c___]]]] :> (NM[a,
							UTrace1[NM[b,
									Adjoint[
										FieldDerivative[
											fd /. {li1 -> li2, li2 -> li1}, x, LorentzIndex[li2]]], c]]] /.
					{li1 -> li2, li2 -> li1}) /;
				Cases[{a, b, c}, LorentzIndex[__], Infinity,
						Heads -> True] === {LorentzIndex[li1], LorentzIndex[li2]}}

lorentzDerCleanup :=
	((# /. lorentzDerCleanupRules) &);

(* Field strength tensors *)

fstrules = {n :
					(FieldStrengthTensorFull|FieldStrengthTensor)[LorentzIndex[mu1_],
						QuantumField[__,
								LorentzIndex[mu2_]][
							x_], x_, ___] :> (-n /. {mu1 -> mu2, mu2 -> mu1}) /;
					Sort[LorentzIndex /@ {mu1,
									mu2}] =!= \
(LorentzIndex /@ {mu1, mu2}),
			n : NM[a___,
						(FieldStrengthTensorFull|FieldStrengthTensor)[LorentzIndex[mu1_],
							QuantumField[__,
									LorentzIndex[mu2_]][
								x_], x_, ___], b___] :> (n /. {mu1 -> mu2, mu2 -> mu1}) /;
					Sort[Cases[{a, b},
									LorentzIndex[
										mu1 | mu2], Infinity, Heads -> True]] ==
							Sort[LorentzIndex /@ \
{mu1, mu2}] &&
						Cases[{a, b},
								LorentzIndex[
									mu1 | mu2], Infinity, Heads -> True] =!=
							Sort[LorentzIndex /@ \
{mu1, mu2}]};

fstCleanup :=
	((# //. fstrules) &);

(* Final sorting function *)

SortIndices = ((Expand[NMExpand[#]] /. sortRules1 /. sortRules2 /. sortRules3 //
								sortDiff// lorentzCleanup // sunCleanup // lorentzDerCleanup // fstCleanup)&);

(*------------------------------------------------------------------------------*)


(* These rules substitute "real" index names instead of the internal code names: *)

dummynames[li_List,
			ll_List] :=
	{fcsuni[IsoDummy][n_] :>
	fcsuni[ToExpression[li[[1]] <> ToString[n]]],
	fcsuni[IsoExternDummy][n_] :> fcsuni[ToExpression[li[[2]] <> ToString[
	n]]], fcsuni[IsoInternDummy][n_] :> fcsuni[ToExpression[li[[3]] <> ToString[
	n]]], LorentzIndex[LorentzDummy][n_] :> LorentzIndex[ToExpression[ll[[1]] <> ToString[
	n]]],
	LorentzIndex[LorentzExternDummy][n_] :> LorentzIndex[ToExpression[ll[[2]] <> ToString[
	n]]], LorentzIndex[LorentzInternDummy][n_] :>
	LorentzIndex[ToExpression[ll[[3]] <> ToString[n]]],
	LorentzIndex[DerivativeExternDummy][n_] :> LorentzIndex[ToExpression[ll[[4]] <> ToString[
	n]]], LorentzIndex[DerivativeInternDummy][n_] :>
	LorentzIndex[ToExpression[ll[[5]] <> ToString[n]]]};



(* The aim is to make a unique dummy substitution ending with as few dummys as
possible.  The uniqueness should then make terms cancel... *)


(* We have to be carefull with the iso f function, since it is not orderless: *)

dummyrulesf3 = {ff_[a_, b_, c_]*
						facc_  :> (res =
						ff[a, b, c]*facc /.
							Map[Apply[Rule, #] &,
								Transpose[{Select[{a, b, c}, ! FreeQ[facc, #] &],
										Table[fcsuni[IsoDummy][rep], {rep, isodummycounter,
												isodummycounter + 2}]}]];
								isodummycounter += 3;
								res)/;
			(Count[{! FreeQ[facc, a], ! FreeQ[facc, b],
				!FreeQ[facc, c]}, True] == 3 &&
							Count[Select[{a, b, c}, ! FreeQ[facc, #] &],
_SUNIndex] == 3 &&
							FreeQ[Select[{a, b, c}, ! FreeQ[facc, #] &], _Integer] &&
							FreeQ[{a, b, c}, IsoDummy] &&
							FreeQ[Select[{a, b, c}, ! FreeQ[facc, #] &],
								Alternatives @@ $ConstantIsoIndices])};

dummyrulesf2 = {ff_[a_, b_, c_]*
						facc_ :> (res =
						ff[a, b, c]*facc /.
							Map[Apply[Rule, #] &,
								Transpose[{Select[{a, b, c}, ! FreeQ[facc, #] &],
										Table[fcsuni[IsoDummy][rep], {rep, isodummycounter,
												isodummycounter + 1}]}]];
								isodummycounter += 2;
								res)/;
			(Count[{! FreeQ[facc, a], ! FreeQ[facc, b],
				!FreeQ[facc, c]}, True] == 2 &&
							Count[Select[{a, b, c}, ! FreeQ[facc, #] &],
_SUNIndex] == 2 &&
							FreeQ[Select[{a, b, c}, ! FreeQ[facc, #] &], _Integer] &&
							FreeQ[{a, b, c}, IsoDummy] &&
							FreeQ[Select[{a, b, c}, ! FreeQ[facc, #] &],
								Alternatives @@ $ConstantIsoIndices]) };

dummyrulesf1 = {ff_[a_, b_, c_]*
						facc_ :> (res =
						ff[a, b, c]*facc /.
							Map[Apply[Rule, #] &,
								Transpose[{Select[{a, b, c}, ! FreeQ[facc, #] &],
										Table[fcsuni[IsoDummy][rep], {rep, isodummycounter,
												isodummycounter + 0}]}]];
								isodummycounter += 1;
								res)/; (Count[{! FreeQ[facc, a], ! FreeQ[facc, b],
				!FreeQ[facc, c]}, True] == 1 &&
							Count[Select[{a, b,c}, ! FreeQ[facc, #] &],
_SUNIndex] == 1 &&
							FreeQ[Select[{a, b, c}, ! FreeQ[facc, #] &], _Integer] &&
							FreeQ[{a, b, c}, IsoDummy] &&
							FreeQ[Select[{a, b, c}, ! FreeQ[facc, #] &],
								Alternatives @@ $ConstantIsoIndices])};



(* Now we can deal with general functions, which are assumed to be orderless in
iso-indices, derivative indices and Lorentz indices.  This last point means
that we may miss cancellation of e.g. some space-time tensors... Again we
device a unique substitution of dummys. *)

(* Label factors in nested structures like NM[NM[..]+NM[..], NM[..]+NM[..]].
	The problem is that we cannot use identical indices on both sides. Thus,
	we label the NM's sequentially and take one label out on each run. *)

(*Put on an isowait[..] tag if an NM product is of the above form*)
(*Applied only once*)
isowaitrules1 =
(aa : HoldPattern[
(NM | NM1 | NM2)[(Plus(*|UTrace1*))[
	(___*(NM | NM1 | NM2)[__?(FreeQ[#, _(NM | NM1 | NM2)] &)] |
(NM | NM1 | NM2)[__?(FreeQ[#, (NM | NM1 | NM2)[__]] &)] |
	_?(FreeQ[#, (NM | NM1 | NM2)[__]] &)) ..] ..]]) :>
(waitcounter = 0;
((waitcounter++;
Replace[#, (nm1 : NM | NM1 | NM2)[b__] :>
nm1[b, isowait[waitcounter]] /; FreeQ[{b}, isowait[__]], {1, 3}]) &) /@ aa)/;
!FreeQ[aa,fcsuni[_]];


(*remove the highest tag*)
(*Applied only once*)
isowaitrules0 =
aa : HoldPattern[
(NM | NM1 | NM2)[(Plus(*|UTrace1*))[(___*(NM | NM1 | NM2)[__?(FreeQ[#, (NM | NM1 | NM2)[__]] &), isowait[__]] |
(NM | NM1 | NM2)[__?(FreeQ[#, (NM | NM1 | NM2)[__]] &), isowait[__]] |
	_?(FreeQ[#, (NM | NM1 | NM2)[__]] &)) ..] ..,___]] :>
(aa /. (isowait[(Max[(#[[1]]) & /@ Cases[aa, isowait[__], {3, 4}]])] ->
																		(seq[])) /. seq -> Sequence)/;
!FreeQ[aa,fcsuni[_]];

(*If an NM product is of the above form, already has a tag and ends with
a factor that has no NM's, remove the highest tag and increment isoexterndummycounter by one*)
(*Applied repeatdly (with IndicesCleanup1)*)
isowaitrules2 =
aa : HoldPattern[
(NM | NM1 | NM2)[___,
(Plus(*|UTrace1*))[(___*(NM | NM1 | NM2)[__?(FreeQ[#, (NM | NM1 | NM2)[__]] &), isowait[__],___] |
(NM | NM1 | NM2)[__?(FreeQ[#, (NM | NM1 | NM2)[__]] &),
	isowait[__],___] | _?(FreeQ[#, (NM | NM1 | NM2)[__]] &)) ..] ..,
	(_?((FreeQ[#,isowait[__]] &&
	FreeQ[#,fcsuni[_?(FreeQ[#,IsoDummy|IsoExternDummy|IsoInternDummy]&)]])&))..]] :>
(aa /. (isowait[(Max[(#[[1]]) & /@ Cases[aa, isowait[__], {3, 4}]])] ->
(isoexterndummycounter = Max[Union[{0},Cases[{aa}, fcsuni[IsoExternDummy][_], Infinity,
												Heads -> True]] /.
											fcsuni[IsoExternDummy][na_] -> na]+1;
seq[])) /. seq -> Sequence)/;
!FreeQ[aa,fcsuni[_]];

(*Same as above but no check fo last factor NM's*)
(*Hmm, the way it is applied, it seems to be redundant. 26/8-2001*)
isowaitrules3 =
aa : HoldPattern[
(NM | NM1 | NM2)[___,
(Plus(*|UTrace1*))[(___*(NM | NM1 | NM2)[__?(FreeQ[#, (NM | NM1 | NM2)[__]] &), isowait[__],___] |
(NM | NM1 | NM2)[__?(FreeQ[#, (NM | NM1 | NM2)[__]] &),
	isowait[__],___] | _?(FreeQ[#, (NM | NM1 | NM2)[__]] &)) ..] ..,
	(_?((FreeQ[#,isowait[__]])&))..]] :>
(aa /. (isowait[(Max[(#[[1]]) & /@ Cases[aa, isowait[__], {3, 4}]])] ->
(isoexterndummycounter = Max[Union[{0},Cases[{aa}, fcsuni[IsoExternDummy][_], Infinity,
												Heads -> True]] /.
											fcsuni[IsoExternDummy][na_] -> na]+1;
seq[])) /. seq -> Sequence)/;
!FreeQ[aa,fcsuni[_]];

(* Rules for UTraces. This is the one exception to only allowing one level of nesting.
	The strategy is to take out SUNIndices in UTraces via SUNDeltas, leaving a dummy
	index without SUNIndex head in the UTrace and then later substitute back in the index *)

tracerule1 = {

NM[a___, UTrace1[b_?(!FreeQ[#,LorentzIndex]&)], c___]*d_ :>
	(Times @@ (inxlist = (SUNDelta[#, fcsuni[suninx[Unique["dum"]]]] & /@
					Select[
						Cases[Cases[{a,UTrace1[b],c,d}, UTrace1[_], Infinity, Heads->True],
							fcsuni[_], Infinity, Heads->True],
					FreeQ[#,suninx]&])))*
	(*Lorentz stuff added 23/9-2001*)
	(Times @@ (linxlist = (Pair[#, LorentzIndex[linx[Unique["dum"]]]] & /@
					Select[
						Cases[Cases[{a,UTrace1[b],c,d}, UTrace1[_], Infinity, Heads->True],
							LorentzIndex[_], Infinity, Heads->True],
					FreeQ[#,linx]&])))(**)*
			(NM[a, UTrace1[b], c] /.
			(UTrace1[e_] :>
				UTrace1[e/.((Rule[#[[1]], suninx0@@#[[2,1]]])& /@ inxlist)/.
									(*FCPartialD automatically put on head LorentzIndex which screws up things*)
									((Rule[FCPartialD[#[[1]]],partd[linx0@@#[[2,1]]]])& /@ linxlist)/.
									((Rule[#[[1]],linx0@@#[[2,1]]])& /@ linxlist)]))*(d),

(nm:(NM|Times))[a___, UTrace1[b_?(!FreeQ[#,LorentzIndex]&)], c___] :>
	(Times @@ (inxlist = (SUNDelta[#, fcsuni[suninx[Unique["dum"]]]] & /@
					Select[
						Cases[Cases[{a,UTrace1[b],c}, UTrace1[_], Infinity, Heads->True],
							fcsuni[_], Infinity, Heads -> True],
					FreeQ[#,suninx]&])))*
	(Times @@ (linxlist = (Pair[#, LorentzIndex[linx[Unique["dum"]]]] & /@
					Select[
						Cases[Cases[{a,UTrace1[b],c}, UTrace1[_], Infinity, Heads->True],
							LorentzIndex[_], Infinity, Heads -> True],
					FreeQ[#,linx]&])))*
			(nm[a, UTrace1[b], c]/.
			UTrace1[e_]:>
				UTrace1[e/.((Rule[#[[1]],suninx0@@#[[2,1]]])& /@ inxlist)/.
									((Rule[FCPartialD[#[[1]]],partd[linx0@@#[[2,1]]]])& /@ linxlist)/.
									((Rule[#[[1]],linx0@@#[[2,1]]])& /@ linxlist)])};

tracerule2 = {SUNDelta[fcsuni[suninx[f_]], j_]*
		a_?((!FreeQ[#, suninx0[f_]])&) :> (a /. suninx0[f] -> j),
		Pair[LorentzIndex[linx[f_]], j_]*a_?((!FreeQ[#, linx0[f_]])&) :>
		(a /. linx0[f] -> j)};

tracerule3 = {SUNDelta[fcsuni[suninx[_]], _]->1, Pair[LorentzIndex[linx[_]], _]->1, partd->FCPartialD};

(* Comparison between factors: *)

(*Iso - indices*)
	dummyrulesiso1a =
{(nm1 : NM | NM1 | NM2)[
	fac___, gg_[fi___, a_SUNIndex, la___][x_],
	facc___] :>
		(( (nm1[fac, gg[fi, a, la][x], facc] /.
						a -> fcsuni[IsoExternDummy][isoexterndummycounter]) /;
							(!FreeQ[{fac, facc}, a] &&  FreeQ[{fac, facc}, IsoExternDummy] &&
							FreeQ[{fac, facc}, _isowait] &&
							FreeQ[{fac, facc},
								(NM | NM1 | NM2)[___, _?((FreeQ[{##}, isomult] &&
															FreeQ[{##}, _SUNIndex] ===
															False)&), ___]
							]
					)  ) )
		};
(*Iso - indices*)
	dummyrulesiso1b = {(nm1 : NM | NM1 | NM2)[fac___,
						gg_[fi___, a_SUNIndex, la___],
						facc___] :> (((nm1[fac,
							gg[fi, a, la], facc] /.
						a -> fcsuni[IsoExternDummy][isoexterndummycounter]) /; (! FreeQ[{fac, facc}, a] &&
							FreeQ[{fac, facc}, IsoExternDummy] && FreeQ[{fac, facc}, _isowait] &&
							FreeQ[{fac, facc},
		(NM | NM1 | NM2)[___, _?((FreeQ[{##}, isomult] &&
									FreeQ[{##}, _SUNIndex] ===
									False) &), ___]

		]) ))};
(*Iso - indices*)
	dummyrulesiso2 = {(nm1 : NM | NM1 | NM2)[fac___,
						gg_[fi___, a_SUNIndex, la___][
							x_], facc___] :> (nm1[fac,
							gg[fi, a, la][x], facc] /.
							a -> fcsuni[IsoExternDummy][
							Max[Union[{0},Cases[{fac, facc}, fcsuni[IsoExternDummy][_], Infinity,
												Heads -> True]] /.
											fcsuni[IsoExternDummy][na_] -> na] + 1]) /;
		(! FreeQ[{fac, facc}, a]  &&
					FreeQ[{fac, facc}, _isowait] && FreeQ[{fac, facc},
		(NM | NM1 | NM2)[___?(FreeQ[{##},isomult]&)]]),
				(nm1 : NM | NM1 | NM2)[fac___,
						gg_[fi___, a_SUNIndex, la___],
						facc___] :> (nm1[fac,
							gg[fi, a, la], facc] /.
						a -> fcsuni[IsoExternDummy][
								Max[Union[{0},Cases[{fac, facc}, fcsuni[IsoExternDummy][_], Infinity,
												Heads -> True]] /.
											fcsuni[IsoExternDummy][na_] -> na] + 1]) /;
		(! FreeQ[{fac, facc}, a]  &&
					FreeQ[{fac, facc}, _isowait] && FreeQ[{fac, facc},
		(NM | NM1 | NM2)[___?(FreeQ[{##},isomult]&)]]) };

(*Derivative indices*)
	dummyrulesder1a = {(nm1 : NM | NM1 | NM2)[fac___,
						gg_[fi___,
								FCPartialD[a_LorentzIndex],
								la___][x_],
						facc___] :> (nm1[fac,
							gg[fi, FCPartialD[a], la][x], facc] /.
						a -> LorentzIndex[DerivativeExternDummy][1]) /; (! FreeQ[{fac, facc}, a] &&
							FreeQ[{fac, facc}, DerivativeExternDummy] &&
							FreeQ[{fac, facc},
		(NM | NM1 | NM2)[___?(FreeQ[{##},lorentzmult]&)]]),
	(ff_[FCPartialD[LorentzIndex[a_]], fi___,LorentzIndex[a_],if___][x_]) :> (ff[
		FCPartialD[LorentzIndex[DerivativeInternDummy][0]],
		fi,LorentzIndex[DerivativeInternDummy][0],if][x])/;
		(FreeQ[a, DerivativeInternDummy])};
(*Derivative indices*)
	dummyrulesder1b = {(nm1 : NM | NM1 | NM2)[fac___,
						gg_[fi___,
							FCPartialD[a_LorentzIndex],
							la___], facc___] :> (nm1[fac,
							gg[fi, FCPartialD[a], la], facc] /.
						a -> LorentzIndex[DerivativeExternDummy][1])/; (! FreeQ[{fac, facc}, a] &&
							FreeQ[{fac, facc}, DerivativeExternDummy] &&
							FreeQ[{fac, facc},
		(NM | NM1 | NM2)[___?(FreeQ[{##},lorentzmult]&)]])};
(*Derivative indices*)
	dummyrulesder2 = {(nm1 : NM | NM1 | NM2)[fac___,
						gg_[fi___,
								FCPartialD[a_LorentzIndex],
								la___][x_],
						facc___] :> (nm1[fac,
							gg[fi, FCPartialD[a], la][x], facc] /.
						a -> LorentzIndex[DerivativeExternDummy][
								Max[Cases[{fac, facc}, LorentzIndex[DerivativeExternDummy][_],
												Infinity, Heads -> True] /.
											LorentzIndex[DerivativeExternDummy][na_] -> na] + 1])/;
					(! FreeQ[{fac, facc}, a] && !FreeQ[{fac, facc}, DerivativeExternDummy] &&
							FreeQ[{fac, facc},
		(NM | NM1 | NM2)[___?(FreeQ[{##},lorentzmult]&)]]),
				(nm1 : NM | NM1 | NM2)[fac___,
						gg_[fi___,
							FCPartialD[a_LorentzIndex],
							la___], facc___] :> (nm1[fac,
							gg[fi, FCPartialD[a], la], facc] /.
						a -> LorentzIndex[DerivativeExternDummy][
								Max[Cases[{fac, facc}, LorentzIndex[DerivativeExternDummy][_],
												Infinity, Heads -> True] /.
											LorentzIndex[DerivativeExternDummy][na_] -> na] + 1])/;
					(! FreeQ[{fac, facc}, a] && !FreeQ[{fac, facc}, DerivativeExternDummy] &&
							FreeQ[{fac, facc},
		(NM | NM1 | NM2)[___?(FreeQ[{##},lorentzmult]&)]])};
(*Lorentz indices*)
	dummyruleslor1 = {(nm1 : NM | NM1 | NM2)[fac___,
						gg_[fi___, a_LorentzIndex,
								la___][x_],
						facc___] :> (nm1[fac,
							gg[fi, a, la][x], facc] /.
						a -> LorentzIndex[LorentzExternDummy][1])/; (! FreeQ[{fac, facc}, a] &&
							FreeQ[{fac, facc}, LorentzExternDummy] &&
							FreeQ[{fac, facc},
		(NM | NM1 | NM2)[___?(FreeQ[{##},lorentzmult]&)]]),
				(nm1 : NM | NM1 | NM2)[fac___,
						gg_[fi___, a_LorentzIndex,
							la___], facc___] :> (nm1[fac,
							gg[fi, a, la], facc] /.
						a -> LorentzIndex[LorentzExternDummy][1])/; (! FreeQ[{fac, facc}, a] &&
							FreeQ[{fac, facc}, LorentzExternDummy] &&
							FreeQ[{fac, facc},
		(NM | NM1 | NM2)[___?(FreeQ[{##},lorentzmult]&)]])};
(*Lorentz indices*)
	dummyruleslor2 = {(nm1 : NM | NM1 | NM2)[fac___,
						gg_[fi___, a_LorentzIndex,
								la___][x_],
						facc___] :> (nm1[fac,
							gg[fi, a, la][x], facc] /.
						a -> LorentzIndex[LorentzExternDummy][
								Max[Cases[{fac, facc}, LorentzIndex[LorentzExternDummy][_], Infinity,
												Heads -> True] /.
											LorentzIndex[LorentzExternDummy][na_] -> na] + 1])/;
					(!FreeQ[{fac, facc}, a] && !FreeQ[{fac, facc}, LorentzExternDummy] &&
							FreeQ[{fac, facc},
		(NM | NM1 | NM2)[___?(FreeQ[{##},lorentzmult]&)]]),
				(nm1 : NM | NM1 | NM2)[fac___,
						gg_[fi___, a_LorentzIndex,
							la___],
						facc___] :> (nm1[fac,
							gg[fi, a, la], facc] /.
						a ->
							LorentzIndex[LorentzExternDummy][
								Max[Cases[{fac, facc}, LorentzIndex[LorentzExternDummy][_], Infinity,
												Heads -> True] /.
											LorentzIndex[LorentzExternDummy][na_] -> na] + 1])/;
					(! FreeQ[{fac, facc}, a] && !FreeQ[{fac, facc}, LorentzExternDummy] &&
							FreeQ[{fac, facc},
		(NM | NM1 | NM2)[___?(FreeQ[{##},lorentzmult]&)]])};


(* Comparison internally in functions: *)

(*Iso - indices*)
	dummyrulesisoint1 = {(nm1 : NM | NM1 | NM2)[fac___,
						gg_[fi___, a_SUNIndex, la___][
							x_], facc___] :> (nm1[fac, gg[fi, a, la][x],
							facc] /.
						a -> fcsuni[IsoInternDummy][1])/; (! FreeQ[{fac, facc}, a] &&
							FreeQ[{fi, la}, IsoInternDummy]), (nm1 : NM | NM1 | NM2)[fac___,
						gg_[fi___, a_SUNIndex, la___],
						facc___] :> (nm1[fac, gg[fi, a, la],
							facc] /. a -> fcsuni[IsoInternDummy][1])/; (! FreeQ[{fac, facc}, a] &&
							FreeQ[{fi, la}, IsoInternDummy])};
(*Iso - indices*)
	dummyrulesisoint2 = {(nm1 : NM | NM1 | NM2)[fac___,
						gg_[fi___, a_SUNIndex, la___][
							x_], facc___] :> (nm1[fac,
							gg[fi, a, la][x], facc] /.
						a -> fcsuni[IsoInternDummy][
								Max[Cases[{fi, la}, fcsuni[IsoInternDummy][_], Infinity,
												Heads -> True] /.
											fcsuni[IsoInternDummy][na_] -> na] + 1])/; (! FreeQ[{fac, facc}, a] &&
											!FreeQ[{fi, la}, IsoInternDummy]), (nm1 :
								NM | NM1 | NM2)[fac___,
						gg_[fi___, a_SUNIndex, la___],
						facc___] :> (nm1[fac, gg[fi, a, la],
							facc] /.
						a -> fcsuni[IsoInternDummy][
								Max[Cases[{fi, la}, fcsuni[IsoInternDummy][_], Infinity,
												Heads -> True] /.
											fcsuni[IsoInternDummy][na_] -> na] + 1])/; (! FreeQ[{fac, facc}, a] &&
											!FreeQ[{fi, la}, IsoInternDummy])};

(*Lorentz indices*)
	dummyruleslorint1 = {(nm1 : NM | NM1 | NM2)[fac___,
						gg_[fi___, a_LorentzIndex,
								la___][x_],
						facc___] :> (nm1[fac,
							gg[fi, a, la][x], facc] /.
						a -> LorentzIndex[LorentzInternDummy][1]) /; (! FreeQ[{fac, facc}, a] &&
							FreeQ[{fi, la}, LorentzInternDummy]), (nm1 : NM | NM1 | NM2)[fac___,
						gg_[fi___, a_LorentzIndex,
							la___],
						facc___] :> (nm1[fac, gg[fi, a, la],
							facc] /. a -> LorentzIndex[LorentzInternDummy][1])/; (! FreeQ[{fac, facc}, a] &&
							FreeQ[{fi, la}, LorentzInternDummy])};
(*Lorentz indices*)
	dummyruleslorint2 = {(nm1 : NM | NM1 | NM2)[fac___,
						gg_[fi___, a_LorentzIndex,
								la___][x_],
						facc___] :> (nm1[fac,
							gg[fi, a, la][x], facc] /.
						a ->
							LorentzIndex[LorentzInternDummy][
								Max[Cases[{fi, la}, LorentzIndex[LorentzInternDummy][_], Infinity,
												Heads -> True] /.
											LorentzIndex[LorentzInternDummy][na_] -> na] + 1])/;
					(! FreeQ[{fac, facc}, a] && !FreeQ[{fi, la}, LorentzInternDummy]), (nm1 :
								NM | NM1 | NM2)[fac___,
						gg_[fi___, a_LorentzIndex,
							la___],
						facc___] :> (nm1[fac,
							gg[fi, a, la], facc] /.
						a -> LorentzIndex[LorentzInternDummy][
								Max[Cases[{fi, la}, LorentzIndex[LorentzInternDummy][_], Infinity,
												Heads -> True] /.
											LorentzIndex[LorentzInternDummy][na_] -> na] + 1])/; (! FreeQ[{fac, facc}, a] &&
											!FreeQ[{fi, la}, LorentzInternDummy])};



(* Squared objects will not be caught by the previous rules (and we don't have
higher powers with one iso-spin index), so we need a special set of rules: *)

dummyrulessq1 = {
	(ff_[fi___, a_SUNIndex,
	la___][x_])^2 :> (res = (ff[fi, fcsuni[IsoDummy][isodummycounter], la][x])^2;
					isodummycounter += 1;
					res) /; (FreeQ[a, IsoDummy]),

	(ff_[fi___, a_SUNIndex,
	la___])^2 :> (res = (ff[fi, fcsuni[IsoDummy][isodummycounter], la])^2;
				isodummycounter += 1;
				res)/; (FreeQ[a, IsoDummy]),

	(ff_[fi___, a_LorentzIndex,
	la___][x_])^2 :> (ff[fi, LorentzIndex[LorentzDummy][1], la][x])^2 /; (FreeQ[a, LorentzDummy]),

	(ff_[fi___, a_LorentzIndex,
	la___])^2 :> (ff[fi, LorentzIndex[LorentzDummy][1], la])^2/; (FreeQ[a, LorentzDummy]),

	(ff_[FCPartialD[LorentzIndex[a_]], fi__][x_])^2 :> (ff[
		FCPartialD[LorentzIndex[DerivativeExternDummy][1]], fi][x])^2/; (FreeQ[a, DerivativeExternDummy]),

	(ff_[FCPartialD[LorentzIndex[a_]], fi__])^2 :>
	(ff[FCPartialD[LorentzIndex[DerivativeExternDummy][1]], fi])^2 /; (FreeQ[a, DerivativeExternDummy])};

dummyrulessq2 = {
	(ff_[fi___, fcsuni[ld_][ll_], fcsuni[ln_], la___][x_])^2 :>
	(ff[fi, fcsuni[ld][ll], fcsuni[ln][ll + 1], la][x])^2,
	(ff_[fi___, fcsuni[ld_][ll_], fcsuni[ln_], la___])^2 :>
	(ff[fi, fcsuni[ld][ll], fcsuni[ln][ll + 1], la])^2,
	(ff_[fi___, LorentzIndex[ld_][ll_], LorentzIndex[ln_], la___][x_])^2 :>
	(ff[fi, LorentzIndex[ld][ll], LorentzIndex[ln][ll + 1], la][x])^2,
	(ff_[fi___, LorentzIndex[ld_][ll_], LorentzIndex[ln_], la___])^2 :>
	(ff[fi, LorentzIndex[ld][ll], LorentzIndex[ln][ll + 1], la])^2,
	(ff_[FCPartialD[LorentzIndex[ld_][ll_]], FCPartialD[LorentzIndex[ln_]], fi__][x_])^2 :>
	(ff[FCPartialD[LorentzIndex[ld][ll]], FCPartialD[LorentzIndex[ln][ll + 1]], fi][x])^2,
	(ff_[FCPartialD[LorentzIndex[ld_][ll_]], FCPartialD[LorentzIndex[ln_]], fi__])^2 :>
	(ff[FCPartialD[LorentzIndex[ld][ll]], FCPartialD[LorentzIndex[ln][ll + 1]], fi])^2};

(* This is to clean up products of QuantumField[
		FCPartialD[LorentzIndex[DerivativeInternDummy][0]], p,
		LorentzIndex[DerivativeInternDummy][0]][x] *)
fixderindices1 = {NM->nmm1, NM1->nmm1};
fixderindices2 = {nmm1[a__]?((FreeQ[#, nmm1[__?(!FreeQ[#,
		LorentzIndex[DerivativeInternDummy][0]]&)]] && !FreeQ[#, DerivativeInternDummy])&) :>
		(derindcounter = Max[Union[{0},
		Cases[{a}, LorentzIndex[DerivativeInternDummy][_], Infinity, Heads->True] /.
			LorentzIndex[DerivativeInternDummy] -> Identity]];
		nmm1 @@ ((# /. QuantumField[
		FCPartialD[LorentzIndex[DerivativeInternDummy][0]], p__,
		LorentzIndex[DerivativeInternDummy][0], r___] :>
		(++derindcounter;
		QuantumField[FCPartialD[LorentzIndex[
		DerivativeInternDummy][derindcounter]], p, LorentzIndex[
		DerivativeInternDummy][derindcounter], r]))& /@ {a}))};
fixderindices3 = {nmm1->NM1};

allpatterns = (Blank | BlankSequence | BlankNullSequence | Pattern);

(*Expand traces of products of sums and products of sums of traces*)
fixtraceplus = {aa:HoldPattern[UTrace1[NM[___,
	Plus[___,_?(!FreeQ[#,SUNIndex,
	Heads->True]&),___],___]]] :>
(FCPrint[1,"Found UTrace of NM Product of sums. Applying NMExpand"];
NMExpand[aa]),
aa:(NM[___, _?((!FreeQ[#, UTrace1, Heads -> True] &&
	!FreeQ[#,SUNIndex] &&
			!MatchQ[#, UTrace1[_] | _*UTrace1[_]]) &), ___]) :>
	(FCPrint[1,"Found NM Product of sums of UTraces. Applying NMExpand"];
	NMExpand[aa]),
aa:(Times[___, _?((!FreeQ[#, UTrace1, Heads -> True] &&
	!FreeQ[#,SUNIndex] &&
			!MatchQ[#, UTrace1[_] | _*UTrace1[_]]) &), ___]) :>
	(FCPrint[1,"Found Product of sums of UTraces. Applying Expand"];
	Expand[aa])};

(*Very cheap hacks to catch a few nested indices*)
cheapnesthackrule = {f_[LorentzIndex[li__]][x_] :> f[x, lihusk][LorentzIndex[li]] /; !MatchQ[x,LorentzIndex[__]],
HoldPattern[FieldStrengthTensor[LorentzIndex[li1__],
	QuantumField[p__, l:(LorentzIndex[__]..)][x_], x_]] :>
	FieldStrengthTensor[LorentzIndex[li1], l, QuantumField[p][x], x]};
cheapnesthackruleback = {f_[x_, lihusk][LorentzIndex[li__]] :> f[LorentzIndex[li]][x],
HoldPattern[FieldStrengthTensor[LorentzIndex[li1_], l:(LorentzIndex[__]..),
QuantumField[p__][x_], x_]] :> FieldStrengthTensor[LorentzIndex[li1],
	QuantumField[p, l][x], x]
};

(* The final cleanup function *)

IndicesCleanup1[w_, OptionsPattern[]] :=
	w /.
			dummyrulesiso1a /. dummyrulesiso1b /. dummyrulesiso2 /.
						dummyrulesder1a /. dummyrulesder1b /. dummyrulesder2 /.
	dummyruleslor1 /. dummyruleslor2 //.
		fixderindices1 //. fixderindices2 //. fixderindices3 /.
		{(nm1:NM|NM1|NM2)[fac___] :> nm1[fac,isomult] /;
			FreeQ[{fac},fcsuni[_?((FreeQ[#,IsoDummy|IsoExternDummy|IsoInternDummy]&&!UScalarQ[#])&)]] &&
			FreeQ[{fac},isomult],
		(nm1:NM|NM1|NM2)[fac___] :> nm1[fac,lorentzmult] /;
			FreeQ[{fac},fcsuni[_?((FreeQ[#,LorentzDummy|LorentzExternDummy|LorentzInternDummy|
			DerivativeExternDummy]&&!UScalarQ[#])&)]] && FreeQ[{fac},lorentzmult]}  /. isowaitrules2;


SetAttributes[NM1, Flat];
SetAttributes[NM2, Flat];

IndicesCleanup[ww_, opts___] :=
	(

	declutr = False;
	w = ww /.

	(* If UTrace1 has been declared  UScalar, NMPower's of it will be Power's
		and contracted indices woun't be seen. Hack to fix this *)
	If[ ExtendedCleanup /. Flatten[{opts}] /. Options[IndicesCleanup],
		If[ UScalarQ[UTrace1],
			declutr = True;
			FCPrint[2,"UndeclareUScalar[UTrace1]"];
			UndeclareUScalar[UTrace1]
		];
		Power[utr_UTrace1, po_] :> NM@@Table[utr,{po}],
		Power[utr_UTrace1, po_] :> NM@@Table[utr,{po}]
	] /.

	cheapnesthackrule/.fixtraceplus//.tracerule1/.isowaitrules1/.isowaitrules0;
	larul = {{}, {}, {}};
	Which[
	!FreeQ[w,LorentzIndex[_, D] | Momentum[_, D]],

		If[ ExtendedCleanup /. Flatten[{opts}] /. Options[IndicesCleanup],
			FCPrint[1,
				"Found occurence of D, using this as number of \
space-time dimensions for all momenta, Dirac gamma matrices and Lorentz \
indices"];
			larul = {LorentzIndex[llii_] -> LorentzIndex[llii, D],
				dg[dig_] -> dg[dig, D],
				Momentum[mo_] -> Momentum[mo, D]}
		];,


				!FreeQ[w, LorentzIndex[_, D] | Momentum[_, D]],
				If[ ExtendedCleanup /. Flatten[{opts}] /. Options[IndicesCleanup],
					FCPrint[1,
					"Found occurence of D, using this as number of space-time \
dimensions for all momenta, Dirac gamma matrices and Lorentz indices"];
					larul = {LorentzIndex[llii_] -> LorentzIndex[llii, D], dg[dig_] -> dg[dig, D],
						Momentum[mo_] -> Momentum[mo, D]}
				];
	];
	isodummycounter = 1;
	isoexterndummycounter = 1;
	w1 = If[ ExtendedCleanup /. Flatten[{opts}] /. Options[IndicesCleanup],
			FCPrint[1, "Using ExtendedCleanup->True"];
			w /. {SU2Delta -> su2delta1, SU2F -> su2f1,
					SU3Delta -> su3delta1, SU3F -> su3f1, SU3D -> su3d1,
					SUNDelta -> sundelta1, SUNF -> sunf1,
					SUND -> sund1},
			FCPrint[1,
					"Using ExtendedCleanup->False\nWill not work if mixed Times \
and NM products are present"]w
		] /.
	(*Cleaned up a bit below. 20/6-2003*)
	(((fcsuni[#] -> protectisoconstant[#])&) /@ (((#[[1]])&) /@
								Cases[w, faso[_?((Head[#]=!=fcsuni)&),___], Infinity])) /.

			faso[fcsuni[a_], b___] -> faso[protectisoconstant[a], b];
	FCPrint[3,"Doing reduction on ", w1//StandardForm];
	subres = FixedPoint[
	(If[ FreeQ[#,isowait],
		w2 = #/.isowaitrules3,
		w2 = #
	];
	FixedPoint[(FCPrint[2, "Applying renaming rules"];
				IndicesCleanup1[#, opts]) &,
	If[ FCleanup /. Flatten[{opts}] /. Options[IndicesCleanup],
		FCPrint[2, "Using FCleanup->True"];
		FCPrint[2,
			"Renaming product functions and protecting constants"];
		w2 /. {fcsuni[a_] :> protectisoconstant[a]/; (UScalarQ[a] ||
			!FreeQ[$ConstantIsoIndices, a]),
								LorentzIndex[a_] :> protectliconstant[a] /; UScalarQ[a]} /.
						dummyrulesf3 /. dummyrulesf2 /.
				dummyrulesf1 /. {NM -> NM1,
				Times -> NM1,
				DOT -> NM2,(*change 19/1 - 1999*)
					Power[a_, b_ /; b > 0 && IntegerQ[b]] :>
					NM1 @@ Table[a, {ddum, 1, b}]},
		FCPrint[2,
			"Renaming product functions and protecting constants"];
		w2 /. {fcsuni[a_/; (UScalarQ[a] || !FreeQ[$ConstantIsoIndices, a])]  ->
						protectisoconstant[a],
					LorentzIndex[a_] :> protectliconstant[a] /; UScalarQ[a] } /. {NM ->
					NM1, Times -> NM1, DOT -> NM2,
					Power[a_, b_ /; b > 0 && IntegerQ[b]] :>
					NM1 @@ Table[a, {ddum, 1, b}]}
	]])&,w1];
	FCPrint[2,
				"Putting back product function names and constants"];
	subres /. {isomult->Sequence[],lorentzmult->Sequence[]} /.
	{NM1 ->NM, NM2 -> DOT} /. {protectisoconstant -> fcsuni,
											protectliconstant -> LorentzIndex} /.
	(*Commented out 20/6-2003*)(*faso[fcsuni[ii_]] -> faso[ii] /.*) {su2delta1 -> SU2Delta,
									su2f1 -> SU2F, su3delta1 -> SU3Delta, su3f1 -> SU3F,
									su3d1 -> SU3D, sundelta1 -> SUNDelta, sunf1 -> SUNF,
									sund1 -> SUND} /.
							dummynames[
								IsoDummys /. Flatten[{opts}] /. Options[IndicesCleanup],
								LorentzDummys /. Flatten[{opts}] /.
									Options[IndicesCleanup]] /.
					DiracGamma -> dg /. Flatten[{larul[[1]], larul[[3]]}] /.
			larul[[2]] /. dg -> DiracGamma /. Null->Sequence[] //.
			tracerule2 /. tracerule3 /. cheapnesthackruleback //
					(If[ declutr,
						FCPrint[2, "DeclareUScalar[UTrace1]"];
						DeclareUScalar[UTrace1]
					];
					If[ (CommutatorReduce /. Flatten[{opts}] /. Options[IndicesCleanup]),
						FCPrint[2, "Applying CommutatorReduce"];
						# // ( CommutatorReduce[#,opts])&,
						#
					])& //
					If[ (ExtendedCleanup /. Flatten[{opts}] /. Options[IndicesCleanup]),
						FCPrint[2, "Applying SortIndices"];
						# // ( SortIndices[#,opts])&,
						#
					]&);



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Multiplication of factors containing contracted indices *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(* Replace NM with CNM and contracted indices in factors will be renamed *)

CNM[a_, b_, opts___Rule] :=
	Block[ {ais = {}, bis = {}, ais1 = {}, bis1 = {}, isos,
			lors, is, dum, j, jj, nm, nma, nmb},
		siCNM = 0;
		liCNM = 0;
		a /. ((NM | Times)[___,
							j_?(! FreeQ[#, fcsuni[__] | LorentzIndex[__]] &), ___,
							jj_?(! FreeQ[#, fcsuni[__] | LorentzIndex[__]] &), ___]) :>
				dum /; ((ais =
									Intersection[
										Cases[j, fcsuni[__] | LorentzIndex[__], Infinity,
											Heads -> True],
										Cases[jj, fcsuni[__] | LorentzIndex[__], Infinity,
											Heads -> True]]) =!= {});
		ais1 = Cases[
				Cases[{a}, ((_?(! FreeQ[#, fcsuni[__] | LorentzIndex[__]] &))^2),
					Infinity, Heads -> True], fcsuni[_], Infinity, Heads -> True];
		b /. ((NM | Times)[___,
							j_?(! FreeQ[#, fcsuni[__] | LorentzIndex[__]] &), ___,
							jj_?(! FreeQ[#, fcsuni[__] | LorentzIndex[__]] &), ___]) :>
				dum /; ((bis =
									Intersection[
										Cases[j, fcsuni[__] | LorentzIndex[__], Infinity,
											Heads -> True],
										Cases[jj, fcsuni[__] | LorentzIndex[__], Infinity,
											Heads -> True]]) =!= {});
		bis1 = Cases[
				Cases[{b}, ((_?(! FreeQ[#, fcsuni[__] | LorentzIndex[__]] &))^2),
					Infinity, Heads -> True], fcsuni[__] | LorentzIndex[__],
				Infinity, Heads -> True];
		If[ (is = Intersection[Union[ais, ais1], Union[bis, bis1]]) =!= {},
			isos = IsoDummys /. {opts} /. Options[CNM];
			lors = LorentzDummys /. {opts} /. Options[CNM];
			NM[a, b /. (Rule[#,
									If[ Head[#] === fcsuni,
										++siCNM;
										fcsuni[isos[[siCNM]]],
										++liCNM;
										LorentzIndex[lors[[liCNM]]]
									]] &/@ is)],
			NM[a, b]
		]
	];

CNM[aa_, bb_, cc__?(!MatchQ[#,Rule]&),opts___Rule] :=
	Block[ {isos = IsoDummys /. {opts} /. Options[CNM],
	lors = LorentzDummys /. {opts} /. Options[CNM]},
		CNM[CNM[aa, bb, opts], cc, IsoDummys -> Drop[isos, siCNM],
													LorentzDummys -> Drop[lors, siCNM]]
	];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Isospin projection *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(*!!This whole section should be reworked if it's to be of any use,
	e.g. using ClebschGordan.
	Think it even might be wrong!!*)

(* Projecting out IsoVectors in particle channels: *)

FieldProjection[
			IsoVector[
				QuantumField[ders___, Particle[p : Alternatives @@ $Particles, pp___],
					la___], ___],
			opts___] :=
	(Select[$IsoSpinProjectionRules, (!FreeQ[#, (Channel /. Flatten[{opts}] /.
								Options[FieldProjection]) -> _] &)])[[1, 2]] /.
	Iso[par_, {i_}] -> QuantumField[ders, Particle[par, pp], fcsuni[i], la];
FieldProjection[
			IsoVector[
					QuantumField[ders___, Particle[p : Alternatives @@ $Particles, pp___],
						la___], ___][x_],
			opts___] :=
	(Select[$IsoSpinProjectionRules, (!FreeQ[#, (Channel /. Flatten[{opts}] /.
								Options[FieldProjection]) -> _] &)])[[1, 2]] /.
	Iso[par_, {i_}] -> QuantumField[ders, Particle[par, pp], fcsuni[i], la][x];



(* Support functions for AmplitudeProjection: *)


(* The in and out particles are arranged as {tou,in}: *)

processfieldproduct[a_List -> b_List] :=
	Join[ChargeConjugate /@ Reverse[b], a];



(* On-mass-shell rules: *)

processmasses[a_List -> b_List, opts___][
			i_] :=
	(ParticleMass[#,
			Sequence @@ (MassArguments /. Flatten[{opts}] /.
						Options[AmplitudeProjection])] & /@ Join[a, b])[[i]];
momentarules[
			opts___] :=
	(mv[i_] :=
		ToExpression[(MomentumVariablesString /. Flatten[{opts}] /.
						Options[AmplitudeProjection]) <> ToString[i]];
	Flatten[Join[{
					If[ (OnMassShell /. Flatten[{opts}] /.
							Options[AmplitudeProjection]),
						Table[Pair[Momentum[mv[irep], ___],
									Momentum[mv[
											irep], ___]] -> (processmasses[(Channel /.
																Flatten[{opts}] /.
															Options[AmplitudeProjection])[[1]]][
											irep])^2, {irep,
								Length[(Channel /. Flatten[{opts}] /.
													Options[AmplitudeProjection])[[1, 1]]] +
									Length[(Channel /. Flatten[{opts}] /.
													Options[AmplitudeProjection])[[1,
												2]]]}],
						{}
					]}]]);



(* Projecting amplitudes out in particle channels: *)

AmplitudeProjection[amp_,
			opts___Rule] :=
	(Expand[
			NMExpand[
				NM @@ (processfieldproduct[(Channel /. Flatten[{opts}] /.
												Options[
													AmplitudeProjection])[[1]]] /. \
	($IsoSpinProjectionRules /. Iso -> iso1)) /.
						ChargeConjugate[Plus[a_, b___]] :>
						Plus @@ ChargeConjugate /@ {a, b}]] /.
		NM -> amp1) /. {iso1[PhiMeson, {aa_Integer|(aa:ExplicitSUNIndex[_Integer])}] -> aa,
	iso1[Pion, {aa_Integer|(aa:ExplicitSUNIndex[_Integer])}] -> aa} /.
	iso1[_, {aa_Integer|(aa:ExplicitSUNIndex[_Integer])}] -> aa /. momentarules[opts] /. amp1 -> amp;



(* PionKaonIsoSpin->3/2: *)

AmplitudeProjection[amp_,
				opts___Rule] /; ((Channel /. Flatten[{opts}] /.
							Options[AmplitudeProjection]) === {{Pion, Kaon} -> {Kaon, Pion},
						3/2}) :=
	AmplitudeProjection[amp,
		Channel -> {{PionPlus, KaonPlus} -> {KaonPlus, PionPlus}},
		Sequence[opts]];



(* PionKaonIsoSpin->1/2: *)

AmplitudeProjection[amp_,
				opts___Rule] /; ((Channel /. Flatten[{opts}] /.
							Options[AmplitudeProjection]) === {{Pion, Kaon} -> {Kaon, Pion},
						1/2}) :=
	-1/2*
	AmplitudeProjection[amp,
	Channel -> {{PionPlus, KaonPlus} -> {KaonPlus, PionPlus}},
	Sequence[opts]] +
	3/2*AmplitudeProjection[amp,
	Channel -> {{KaonPlus, PionMinus} -> {KaonPlus, PionMinus}},
	Sequence[opts]];



(* PionPionIsoSpin->2: *)

AmplitudeProjection[amp_,
				opts___Rule] /; ((Channel /. Flatten[{opts}] /.
							Options[AmplitudeProjection])) === {{Pion, Pion} -> {Pion,
							Pion}, 2} :=
	AmplitudeProjection[amp,
		Channel -> {{PionPlus, PionPlus} -> {PionPlus, PionPlus}},
		Sequence[opts]];



(* PionPionIsoSpin->1: *)

AmplitudeProjection[amp_,
				opts___Rule] /; ((Channel /. Flatten[{opts}] /.
							Options[AmplitudeProjection]) === {{Pion, Pion} -> {Pion, Pion},
						1}) :=
	2*
	AmplitudeProjection[amp,
	Channel -> {{PionPlus, PionZero} -> {PionPlus, PionZero}},
	Sequence[opts]] -
	AmplitudeProjection[amp,
	Channel -> {{PionPlus, PionPlus} -> {PionPlus, PionPlus}},
	Sequence[opts]];



(* PionPionIsoSpin->0: *)

AmplitudeProjection[amp_, opts___Rule] /; ((Channel /. Flatten[{opts}] /. Options[AmplitudeProjection]) === {{Pion, Pion} -> {Pion, Pion}, 0}) :=
	3*	AmplitudeProjection[amp, Channel -> {{PionZero, PionZero} -> {PionZero, PionZero}},
	Sequence[opts]] - 2*AmplitudeProjection[amp, Channel -> {{PionPlus, PionPlus} -> {PionPlus, PionPlus}}, Sequence[opts]];


FCPrint[1,"PHI: Channels.m loaded."];
End[];
