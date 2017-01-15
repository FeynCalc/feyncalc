(* Wolfram Language package *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Utilities (Phi)													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:	Utilities for kinematics and Dirac algebra					*)

(* ------------------------------------------------------------------------ *)

MandelstamReduce::usage =
"MandelstamReduce[amp,opts] gives the amplitude amp with all momenta \
variables given by the option MomentumVariablesString substituted by \
Mandelstam variables MandelstamS, MandelstamT and MandelstamU.  When the \
option OnMassShell is set to True, the masses specified in the option Masses \
are used.  The substituted Mandelstam variables depend on the value of the \
option MomentaSumLeft.  E.g. For MomentaSumLeft set to All, the following \
convention is used: s=(p1+p2)^2, t=(p2+p3)^2, u=(p1+p3)^2 with all particles \
incoming.\n
When the option Cancel is set to one of \
MandelstamS, MandelstamT or MandelstamU, this variable is cancelled using the \
Mandelstam relation.  The cancellation is disabled by setting \
Cancel to None.  Default value of Cancel : MandelstamU";

MandelstamS::usage =
"MandelstamS is the Mandelstam s-variable";

MandelstamT::usage =
"MandelstamT is the Mandelstam t-variable";

MandelstamU::usage =
"MandelstamU is the Mandelstam u-variable";

$LorentzIndicesCounter::usage =
"$LorentzIndicesCounter is a variable numbering the indices supplied by \
LorentzIndicesSupply.  To start at 1 simply set $LorentzIndicesCounter = 0";

LorentzIndicesSupply::usage =
"LorentzIndicesSupply replaces ScalarProduct's and Pair's of momenta and \
gamma matrices with products with written out Lorentz indices";

FourPoint::usage =
"FourPoint[q,exp] expresses a rank four tensor integral (exp is the \
integrand, q is the four-momentum to be integrated with respect to) in terms \
of lower rank PaVe's according to the formula on page 22 in the FeynCalc 1.0 \
manual.  Any lower rank subexpressions resulting from the expansion of exp or \
already in exp are passed to OneLoop, and FourPoint accepts the options \
of OneLoop and passes them on";

GammaSort::usage =
"GammaSort[exp] returns the expression with DOT products of gamma \
matrices sorted according to the ordering function specified";

Gamma5AntiCommute::usage =
"Gamma5AntiCommute is an option of GammaSort specifying whether or not \
the gamma-5 matrix should be taken to anticommute with the other gamma \
matrices.  Default value : False";

OrderingFunction::usage =
"OrderingFunction is an option of GammaSort specifying which function \
should be used for the ordering of the gamma matrices.  Default value : \
OrderedQ";

DiscardMomenta::usage =
"DiscardMomenta is an option for DiscardOrders.  When set to True, any \
ocurring momentum will be considered an expansion quantity, and higher powers \
will be discarded.  Default value : True";

DiscardOrders::usage =
"DiscardOrders[m,opts] discards products with the total order of momenta \
and/or any other quantities from $ExpansionQuantities higher than the \
PerturbationOrder specified in opts or Options[DiscardOrders].";

FCToTracer::usage =
"FCToTracer[exp] translates the expression exp involving unevaluated \
FeynCalc DiracTrace's to Tracer notation with the Tracer line indices set to \
variable names determined by the setting of the option TracerIndicesString.";

$TracerIndicesCounter::usage =
"$TracerIndicesCounter is a variable used by FCToTracer for generating \
fermion line indices.";

TracerIndicesString::usage =
"TracerIndicesString is an option of FCToTracer specifying the index used \
for the fermion line index of Tracer.  Default value : \"l\".";

TracerToFC::usage =
"TracerToFC[exp] translates the expression exp traced with Tracer to \
FeynCalc notation.";

CharacteristicCoefficient::usage =
"CharacteristicCoefficient[a,opts][i] returns the i'th coefficient of the \
characteristic polynomial of the square matrix a expressed in terms of traces of powers \
of a. The dimension of a is specified by the option UDimension.";

CayleyHamilton::usage =
"CayleyHamilton[m1,m2, ...] returns the Cayley-Hamilton identity applied to the sum of \
the square matrices m1, m2, ... If the option Eliminate is set to True and the number of \
matrices agrees with the dimension of the matrices (specified by the option UDimension), \
terms with products of identical matrices are recursively eliminated.";

CayleyHamiltonTrick::usage =
"CayleyHamiltonTrick[exp] applies Cayley-Hamilton identities obtained with the matrices \
specified by the setting of the option UMatrices to exp.";

CayleyHamiltonRules::usage =
"CayleyHamiltonRules[mats] returns rules usable for reduction using Cayley-Hamilton and \
the matrices mats. mats must be a list of lists of matrices.";

UMatrices::usage =
"UMatrices is an option of CayleyHamiltonTrick.";

UReduce::usage =
"UReduce[exp] applies different matrix identities for MM[x] and SMM[x] to the expression \
exp attempting to bring exp into a more compact form. Notice that MM[x] and SMM[x] should \
be given without options, that is, it is assumed that SetOptions[MM, Explicit->False] \
and SetOptions[SMM, Explicit->False] have been evaluated.  UReduce is also an option for \
CayleyHamiltonRules and CayleyHamiltonTrick with default value False And True respectively.";

UOrder::usage =
"UOrder arranges products of U and d_mu U to have U first.";

UOrder1::usage =
"UOrde1r arranges products of U and d_mu U to have U last.";

UGammaTrick::usage =
"UGammaTrick[exp] applies the identity (7.14) and (7.20) from Gasser and Leutwyler \
(1985) to FieldDerivative's of UGamma[_][_] in exp.";

SMMToMM::usage =
"SMMToMM[expr] replaces NM[SMM[x],SMM[x]] with MM[x]. SMMToMM is also an option \
of UReduce which can be either True of False.  The better use is as an option of UReduce \
because UReduce also tries several reduction rules for getting SMM[x]'s next to each other.";

SurfaceReduce::usage =
"SurfaceReduce[exp] reduces NM products involving FieldDerivative in exp using \
that total derivatives vanish upon integration. The option DifferenceOrder controls \
at which difference in differentation order reduction will be attempted. Setting \
DifferenceOrder smaller than 2 will result in a transformation that do not close.";

EOMTrick::usage =
"EOMTrick[exp] applies the equations of motion as given by $EOMRules to the \
expression exp.";

$EOMRules::usage =
"$EOMRules are equation of motion reduction rules specified in the model \
specific configuration files.";

UPerturb::usage =
"UPerturb[exp] expands instances of MM and some composed objects around the \
solution to the equations of motion up to the order specified by the option ExpansionOrder.  \
The field used for the perturbation is UPerturbation. The expansion is done using the \
convention of Bijnens, Colangelo and Ecker 1999, u^2 = u e^(i xi Sqrt[2]/f) u.";

UFields::usage =
"UFields is an option of UPerturb specifying which fields will be expanded.  \
UFields is also an option of SurfaceReduce restricting the reduction of derivatives to \
terms containing this pattern.  The pattern can be e.g. _ or UPerturbation (doing NMExpand and \
Expand first is then a good idea).  \
Default value : {USmall, UChiPlus, UChiMinus, UFPLus, UFMinus, MM} for UPerturb.  \
UPerturbation for SurfaceReduce.";

UCoefficient::usage =
"UCoefficient[q][i][args_] is the i'th coefficient of the expansion of the quantity \
q in the perturbation field around the solution to the equations of motion used by UPerturb.  \
If new quantities are introduced (functions of SMM), the corresponding UCoefficient's need to \
be defined in order to get the expansion using UPerturb.";

$GellmannOkubo::usage =
"$GellmannOkubo is a substitution rule for the eta-meson mass using the Gell-mann-Okubo \
mass formula.";

GellmannOkubo::usage =
"GellmannOkubo[exp] applies $GellmannOkubo to exp.";

$GellmannOkuboInv::usage =
"$GellmannOkubo is a substitution rule putting back the eta-meson mass using the \
Gell-mann-Okubo mass formula.";

PhiToLaTeX::usage =
"PhiToLaTeX[expr] constructs LaTeX from the expression expr.";

FixFermionAdjoints::usage =
"FixFermionAdjoints[expr] substitutes DiracBars in expr with Adjoints, applies \
DiracSimplify and substitutes back.";

Begin["`Package`"]
End[]

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

Begin["`Utilities`Private`"];


(* Errors *)

CayleyHamilton::baddim =
"Warning: You are requesting using Cayley-Hamilton on a sum with \
a number of terms that does not agree with the dimension of the matrices. The \
result may not be very useful.";

UPerturb::"nocoeff" =
"Warning: Yor are requesting expanding in UPerturbation to order `1`. \
Only up to order 2 is implemented in terms of USmall and CovariantNabla.  (If \
you have the energy, please do work out the expansion and send it to \
feyncalc@feyncalc.org).";

UPerturb::"badlim" = "Error: `1` is not a valid summation limit.";

DiscardOrders::wffac = "Warning: Expression contains unknown renormalization factor(s).";


(* Boxes *)

MandelstamS /: MakeBoxes[MandelstamS, TraditionalForm] :=
	MakeBoxes[StyleForm["s", FontSlant -> "Italic"]][[1]];

MandelstamT /: MakeBoxes[MandelstamT, TraditionalForm] :=
	MakeBoxes[StyleForm["t", FontSlant -> "Italic"]][[1]];

MandelstamU /: MakeBoxes[MandelstamU, TraditionalForm] :=
	MakeBoxes[StyleForm["u", FontSlant -> "Italic"]][[1]];


(* FeynCalc functions *)

fccombs :=
	fccombs = MakeFeynCalcPrivateContext["CombinationLists"];

(* Tracer functions *)

trtr :=
	trtr = Tracer`GammaTrace;

trid = Tracer`TrU;

trsp :=
	trsp = Tracer`Spur;

trdot :=
	trdot = Tracer`TrS;

vecd :=
	vecd = Tracer`VectorDimension;

trsig :=
	trsig = Tracer`Sigma;

treps :=
	treps = Tracer`TrEps;



(* Defaults *)

Options[MandelstamReduce] = {
	MomentaSumLeft -> All,
	OnMassShell -> True,
	Cancel -> MandelstamU,
	MomentumVariablesString -> "p",
	MomentaSumRule -> True,
	Masses ->
		{	ParticleMass[Pion, RenormalizationState[1]],
			ParticleMass[Pion, RenormalizationState[1]],
			ParticleMass[Pion, RenormalizationState[1]],
			ParticleMass[Pion, RenormalizationState[1]]}
};

DeclareUScalar[MandelstamS];
DeclareUScalar[MandelstamT];
DeclareUScalar[MandelstamU];

Options[LorentzIndicesSupply] = {
	LorentzIndicesString -> "\[Nu]"
};

Options[GammaSort] = {
	Gamma5AntiCommute -> False,
	OrderingFunction -> OrderedQ
};

Options[DiscardOrders] = {
	PerturbationOrder -> 4,
	DiscardMomenta -> True,
	ScalarProductForm -> MomentaScalarProduct
};

(*TODO := -> = *)
Options[FCToTracer] := {
		TracerIndicesString -> "l"
};

Options[SurfaceReduce] = {
	DifferenceOrder -> 2,
	UFields -> UPerturbation
};

Options[UReduce] = {
	SMMToMM -> False,
	FullReduce -> True,
	SUNN -> 2,
	UDimension -> Automatic
};

Options[SMMToMM] = {
	SUNN -> 2
};

Options[UPerturb] = {
	ExpansionOrder -> {1},
	SUNN -> 2,
	UFields -> {USmall, UChiPlus, UChiMinus, UFPlus, UFMinus, MM}
};

Options[CharacteristicCoefficient] = {
	UDimension -> 2
};

Options[CayleyHamilton] = {
	UDimension -> 2,
	Eliminate -> True
};

Options[CayleyHamiltonRules] = {
	SUNN -> 2,
	UDimension -> 2,
	CommutatorReduce->True,
	UReduce -> False
};

tmpoptscdr = Options[CovariantFieldDerivative];
tmpoptscn = Options[CovariantNabla];
tmpoptsmm = Options[MM];
tmpoptssmm = Options[SMM];

SetOptions[CovariantFieldDerivative, Explicit -> False];
SetOptions[MM, Explicit -> False];

Options[CayleyHamiltonTrick] = {
	SUNN -> 2,
	UDimension -> 2,
	CommutatorReduce -> True,
	UReduce -> True,
	UMatrices :> {
		{	I NM[Adjoint[CovariantFieldDerivative[MM[Global`x_], Global`x_, {Global`\[Rho]1_}]], MM[Global`x_]],
			I NM[Adjoint[MM[Global`x_]], CovariantFieldDerivative[MM[Global`x_], Global`x_, {Global`\[Rho]2_}]],
			NM[Adjoint[CovariantFieldDerivative[MM[Global`x_], Global`x_, {Global`\[Rho]1_}]],
			CovariantFieldDerivative[MM[Global`x_], Global`x_, {Global`\[Rho]2_}]]
		}
	}
};

SetOptions[CovariantFieldDerivative, Sequence@@tmpoptscdr];
SetOptions[MM, Sequence@@tmpoptsmm];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Mandelstam simplification *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

mv[opts___][i_] :=
	ToExpression[(MomentumVariablesString /. Flatten[{opts}] /. Options[MandelstamReduce]) <> ToString[i]];

mss[opts___][i_] :=
	Pair[Momentum[mv[opts][i]], Momentum[mv[opts][i]]];

masses1[opts___][i_] :=
	(Masses /. Flatten[{opts}] /. Options[MandelstamReduce])[[i]];

manrul[opts___] :=
	{
		MandelstamS + MandelstamU + MandelstamT -> Sum[mss[opts][i], {i,1,4}],
		-MandelstamS - MandelstamU - MandelstamT -> -Sum[mss[opts][i], {i,1,4}],
		MandelstamU + MandelstamT -> -MandelstamS + Sum[mss[opts][i], {i,1,4}],
		-MandelstamU - MandelstamT -> MandelstamS - Sum[mss[opts][i], {i,1,4}],
		MandelstamS + MandelstamT -> -MandelstamU + Sum[mss[opts][i], {i,1,4}],
		-MandelstamS - MandelstamT -> MandelstamU - Sum[mss[opts][i], {i,1,4}],
		MandelstamS + MandelstamU -> -MandelstamT + Sum[mss[opts][i], {i,1,4}],
		-MandelstamS - MandelstamU -> MandelstamT - Sum[mss[opts][i], {i,1,4}],
		a_*MandelstamS + b_*MandelstamT + b_*MandelstamU -> (a - b)*MandelstamS + b*Sum[mss[opts][i], {i,1,4}],
		b_*MandelstamS + a_*MandelstamT + b_*MandelstamU -> (a - b)*MandelstamT + b*Sum[mss[opts][i], {i,1,4}],
		b_*MandelstamS + b_*MandelstamT + a_*MandelstamU -> (a - b)*MandelstamU + b*Sum[mss[opts][i], {i,1,4}]
	};

(*	The convention used is:  s=(p1+p2)^2, t=(p2+p3)^2, u=(p1+p3)^2 with all
	particles incoming (All) as in FeynCalc, that is s=(p1+p2)^2, t=(p2-p3)^2,
	u=(p1-p3)^2 (FirstHalf) where particles 1 and 2 are incoming and p3 and p4 are outgoing: *)

sturules[opts___] /; ((MomentaSumLeft /. Flatten[{opts}] /. Options[MandelstamReduce]) === All) :=
	{
		Pair[Momentum[mv[opts][1], ___], Momentum[mv[opts][2], ___]] ->
			MandelstamS/2 - (Pair[Momentum[mv[opts][1]], Momentum[mv[opts][1]]] +
			Pair[Momentum[mv[opts][2]], Momentum[mv[opts][2]]])/2,
		Pair[Momentum[mv[opts][2], ___], Momentum[mv[opts][3], ___]] ->
			MandelstamT/2 - (Pair[Momentum[mv[opts][2]], Momentum[mv[opts][2]]] +
			Pair[Momentum[mv[opts][3]], Momentum[mv[opts][3]]])/2,
		Pair[Momentum[mv[opts][1], ___], Momentum[mv[opts][3], ___]] ->
			MandelstamU/2 - (Pair[Momentum[mv[opts][1]], Momentum[mv[opts][1]]] +
		Pair[Momentum[mv[opts][3]], Momentum[mv[opts][3]]])/2
	};

sturules[opts___] /; (MomentaSumLeft /. Flatten[{opts}] /. Options[MandelstamReduce]) === Phi`Objects`FirstHalf :=
	{
		Pair[Momentum[mv[opts][1], ___], Momentum[mv[opts][2], ___]] ->
			MandelstamS/2 - (Pair[Momentum[mv[opts][1]], Momentum[mv[opts][1]]] +
			Pair[Momentum[mv[opts][2]], Momentum[mv[opts][2]]])/2,
		Pair[Momentum[mv[opts][2], ___], Momentum[mv[opts][3], ___]] ->
			-MandelstamT/2 + (Pair[Momentum[mv[opts][2]], Momentum[mv[opts][2]]] +
			Pair[Momentum[mv[opts][3]], Momentum[mv[opts][3]]])/2,
		Pair[Momentum[mv[opts][1], ___], Momentum[mv[opts][3], ___]] ->
			-MandelstamU/2 + (Pair[Momentum[mv[opts][1]], Momentum[mv[opts][1]]] +
			Pair[Momentum[mv[opts][3]], Momentum[mv[opts][3]]])/2
	};

sturules[opts___] /; (MomentaSumLeft /. Flatten[{opts}] /. Options[MandelstamReduce]) === Phi`Objects`Odd :=
	{
		Pair[Momentum[mv[opts][1], ___], Momentum[mv[opts][2], ___]] ->
			-MandelstamS/2 + (Pair[Momentum[mv[opts][1]], Momentum[mv[opts][1]]] +
			Pair[Momentum[mv[opts][2]], Momentum[mv[opts][2]]])/2,
		Pair[Momentum[mv[opts][2], ___], Momentum[mv[opts][3], ___]] ->
			-MandelstamT/2 + (Pair[Momentum[mv[opts][2]], Momentum[mv[opts][2]]] +
			Pair[Momentum[mv[opts][3]], Momentum[mv[opts][3]]])/2,
		Pair[Momentum[mv[opts][1], ___], Momentum[mv[opts][3], ___]] ->
			MandelstamU/2 - (Pair[Momentum[mv[opts][1]], Momentum[mv[opts][1]]] +
			Pair[Momentum[mv[opts][3]], Momentum[mv[opts][3]]])/2
	};

strules[opts___] :=
	(Cancel /. Flatten[{opts}] /. Options[MandelstamReduce]) ->
		mss[opts][1] + mss[opts][2] + mss[opts][3] + mss[opts][4] -
	Complement[{MandelstamS, MandelstamT, MandelstamU}, {(Cancel /. Flatten[{opts}] /. Options[MandelstamReduce])}][[1]] -
	Complement[{MandelstamS, MandelstamT, MandelstamU}, {(Cancel /. Flatten[{opts}] /. Options[MandelstamReduce])}][[2]];

MandelstamReduce1[amp_, opts___Rule] :=
	ExpandScalarProduct[amp /.
	(*We don't want a polarization vector Polarization[p1] to
	be replaced with Polarization[-p2-p3-p4]*)
	Polarization[a__] :> ToString/@Polarization[a] /.
	If[ MomentaSumRule /. Flatten[{opts}] /. Options[MandelstamReduce],
		MomentaSumRule@@OptionsSelect[MomentaSumRule,opts,Options[MandelstamReduce]],
		{}
	] /.
	Polarization[a__] :> ToExpression/@Polarization[a]] /. sturules[opts] /.
	If[ (OnMassShell /. Flatten[{opts}] /. Options[MandelstamReduce]),
		Table[Pair[Momentum[mv[opts][irep], ___], Momentum[mv[opts][irep], ___]] ->
				masses1[opts][irep]^2, {irep, 4}],
		{}
	] /.
	If[ !(Cancel /. Flatten[{opts}] /.
		Options[MandelstamReduce]) === None,
		strules[opts],
		{}
	] /.
	If[ (OnMassShell /. Flatten[{opts}] /. Options[MandelstamReduce]),
		Table[Pair[Momentum[mv[opts][irep], ___], Momentum[mv[opts][irep], ___]] ->
				masses1[opts][irep]^2, {irep, 4}],
		{}
	];

MandelstamReduce[amp_, opts___Rule] :=
	MandelstamReduce1[MandelstamReduce1[amp, opts] /. manrul[opts], MomentaSumRule->False, opts];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Lorentz indices *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* Support functions for LorentzIndicesSupply.  Index numerators, increasing by
one each time applied: *)

id[x_, OptionsPattern[]] :=
	x;

$LorentzIndicesCounter = 0;

lin[opts___] :=
	(
	++$LorentzIndicesCounter;
	ToExpression[(LorentzIndicesString /. Flatten[{opts}] /. Options[LorentzIndicesSupply]) <> ToString[$LorentzIndicesCounter]]
	);


(* Step one in the supplial of the extra Lorentz index dependence: *)
indicesdotrule1[optss___] :=
	((Pair | ScalarProduct)[a_, b_] /; (!FreeQ[a, Momentum] && ! FreeQ[b, Momentum]) :> indsuppdot[a, b, lin[optss]]);

indicesdotrule2[optss___] :=
	(DiracGamma[a : Momentum[_, ___] | ___*Momentum[_, ___] | HoldPattern[Plus[((___*Momentum[_,___]) | Momentum[_, ___]) ..]], dim___] :>
		indsuppdot[a, fcdiga1[lorentzdummy, dim], lin[optss]]);



(* Step two in the supplial of the extra Lorentz index dependence: *)

indsuppdot[a_, b_, i_] /; (!FreeQ[a, Momentum] && ! FreeQ[b, Momentum]) :=
	a*b /. Momentum[c_, dim___] -> Pair[Momentum[c, dim], loritemp[i, dim]];

indsuppdot[a : Momentum[_, ___] | ___*Momentum[_, ___] | HoldPattern[Plus[((___*Momentum[_, ___]) |	Momentum[_, ___])..]],
fcdiga1[lorentzdummy, dim___], i_] :=
	a*DiracGamma[loritemp[i, dim], dim] /.	Momentum[c_, dimm___] -> Pair[Momentum[c, dimm], loritemp[i, dimm]];


(* The function that supplies indices to expressions involving IsoDots,
IsoCrosses and IsoSymmetricCrosses of iso-spin vectors: *)

LorentzIndicesSupply[aa_, (optss___Rule | optss___List)] :=
	(aa /.	(Pair | ScalarProduct)[a_, b_]^n_ :> times1 @@ Table[DOT[a, b], {rep, n}] /.
	Power[a_, b_ /; b > 0 && IntegerQ[b]] :> times1 @@ Table[a, {ddum, 1, b}]/.
	{indicesdotrule1[optss], indicesdotrule2[optss]} /. loritemp -> LorentzIndex /.	times1 -> Times);



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Rank four tensor integrals *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* A four point tensor integral of rank four can not be handled by FeynCalc.  We
	reduce it to lower rank PaVe's. *)

(* The stuff below is just to have constants taken out and sums expanded: *)

FourPoint[q_, a_Plus, opts___] :=
	(FourPoint[q, #, opts] & /@ a);

FourPoint[q_, a_*b_, opts___] /; FreeQ[a, q | PD] :=
	a*FourPoint[q, b, opts];


FourPoint[q_, aa_*FeynAmpDenominator[f___, PD[Momentum[q1_, d___] + Momentum[q2_, d___] + m___, m1_], l___], opts___] :=
	FourPoint[q, aa*MomentumCombine[FeynAmpDenominator[f, PD[Momentum[q1, d] + Momentum[q2, d] + m, m1], l]], opts];

FourPoint[q_,	FeynAmpDenominator[PD[Momentum[q1_, d___], m1_], PD[Momentum[q2_, d___], m2_], PD[Momentum[q3_, d___], m3_], PD[Momentum[q4_, d___], m4_]]*
				Pair[LorentzIndex[l1_, d___], Momentum[qq1_, d___]]*
				Pair[LorentzIndex[l2_, d___], Momentum[qq2_, d___]]*
				Pair[LorentzIndex[l3_, d___], Momentum[qq3_, d___]]*
				Pair[LorentzIndex[l4_, d___], Momentum[qq4_, d___]], opts___] /;
				(!FreeQ[qq1, q] && ! FreeQ[qq2, q] && !FreeQ[qq3, q] && ! FreeQ[qq4, q]) && (Head[qq1] == Plus || Head[qq2] == Plus || Head[qq3] == Plus || Head[qq4] == Plus) :=
	FourPoint[q, Expand[FeynAmpDenominator[PD[Momentum[q1, d], m1], PD[Momentum[q2, d], m2], PD[Momentum[q3, d], m3], PD[Momentum[q4, d], m4]]*
					MomentumExpand[Pair[LorentzIndex[l1, d], Momentum[qq1, d]]*
						Pair[LorentzIndex[l2, d], Momentum[qq2, d]]*
						Pair[LorentzIndex[l3, d], Momentum[qq3, d]]*
						Pair[LorentzIndex[l4, d], Momentum[qq4, d]]]], opts];

FourPoint[q_, aa : HoldPattern[Times[___, (_[PD[Momentum[_, ___], _] ..]), a_]], opts___] /;
	!FreeQ[{a},q] && Head[a]===Times && !FreeQ[Head/@List@@a, Plus] :=
	FourPoint[q, Times[aa]//ExpandAll, opts];

(* Formula taken from the FeynCalc1.0 manual (don't know why Rolf didn't
	implement it himself...): *)

FourPoint[q_, FeynAmpDenominator[PD[Momentum[q_, d___], m0_], PD[Momentum[q_ + _, d___], m1_], PD[Momentum[q_ + _, d___], m2_], PD[Momentum[q_ + _, d___], m3_]]*
				Pair[LorentzIndex[l1_, d___], Momentum[q_, d___]]*
				Pair[LorentzIndex[l2_, d___], Momentum[q_, d___]]*
				Pair[LorentzIndex[l3_, d___], Momentum[q_, d___]]*
				Pair[LorentzIndex[l4_, d___], Momentum[q_, d___]], OptionsPattern[]] :=
	(
	pp[1] = p1;
	pp[2] = p2;
	pp[3] = p3;
	pp[4] = p4;
	I*Pi^2*((	Pair[LorentzIndex[l1], LorentzIndex[l2]]*Pair[LorentzIndex[l3], LorentzIndex[l4]] +
				Pair[LorentzIndex[l1], LorentzIndex[l3]]*Pair[LorentzIndex[l2], LorentzIndex[l4]] +
				Pair[LorentzIndex[l1], LorentzIndex[l4]]*Pair[LorentzIndex[l2], LorentzIndex[l3]])*
					PaVe[0, 0, 0,
						0, {Pair[Momentum[p1, d], Momentum[p1, d]],
							Pair[Momentum[p1 + p2, d], Momentum[p1 + p2, d]],
							Pair[Momentum[p2 + p3, d], Momentum[p2 + p3, d]],
							Pair[Momentum[p3, d], Momentum[p3, d]],
							Pair[Momentum[p2, d], Momentum[p2, d]],
							Pair[Momentum[p1 + p3, d], Momentum[p1 + p3, d]]}, {m0^2, m1^2,
							m2^2, m3^2}] +
				Sum[(Pair[LorentzIndex[l1], LorentzIndex[l2]]*Pair[Momentum[pp[i]], LorentzIndex[l3]]*
								Pair[Momentum[pp[j]], LorentzIndex[l4]] +
								Pair[LorentzIndex[l1], LorentzIndex[l3]]*
								Pair[Momentum[pp[i]], LorentzIndex[l2]]*
								Pair[Momentum[pp[j]], LorentzIndex[l4]] +
								Pair[LorentzIndex[l1], LorentzIndex[l4]]*Pair[Momentum[pp[i]], LorentzIndex[l2]]*
									Pair[Momentum[pp[j]], LorentzIndex[l3]] +
								Pair[LorentzIndex[l2], LorentzIndex[l3]]*Pair[Momentum[pp[i]], LorentzIndex[l1]]*
									Pair[Momentum[pp[j]], LorentzIndex[l4]] +
								Pair[LorentzIndex[l2], LorentzIndex[l4]]*Pair[Momentum[pp[i]], LorentzIndex[l1]]*
									Pair[Momentum[pp[j]], LorentzIndex[l3]] +
								Pair[LorentzIndex[l3], LorentzIndex[l4]]*Pair[Momentum[pp[i]], LorentzIndex[l1]]*
									Pair[Momentum[pp[j]], LorentzIndex[l2]])*
						PaVe[0, 0, i, j, {Pair[Momentum[p1, d], Momentum[p1, d]],
								Pair[Momentum[p1 + p2, d], Momentum[p1 + p2, d]],
								Pair[Momentum[p2 + p3, d], Momentum[p2 + p3, d]],
								Pair[Momentum[p3, d], Momentum[p3, d]],
								Pair[Momentum[p2, d], Momentum[p2, d]],
								Pair[Momentum[p1 + p3, d], Momentum[p1 + p3, d]]}, {m0^2, m1^2,
								m2^2, m3^2}], {i, 3}, {j, 3}] +
				Sum[(Pair[Momentum[pp[i]], LorentzIndex[l1]]*Pair[Momentum[pp[j]], LorentzIndex[l3]]*
								Pair[Momentum[pp[k]], LorentzIndex[l1]]*
								Pair[Momentum[pp[l]], LorentzIndex[l3]])*
						PaVe[i, j, k, l, {Pair[Momentum[p1, d], Momentum[p1, d]],
								Pair[Momentum[p1 + p2, d], Momentum[p1 + p2, d]],
								Pair[Momentum[p2 + p3, d], Momentum[p2 + p3, d]],
								Pair[Momentum[p3, d], Momentum[p3, d]],
								Pair[Momentum[p2, d], Momentum[p2, d]],
								Pair[Momentum[p1 + p3, d], Momentum[p1 + p3, d]]}, {m0^2, m1^2,
								m2^2, m3^2}], {i, 3}, {j, 3}, {k, 3}, {l, 3}]));



(* Tensor integrals of rank lower than four are simply handed to OneLoop: *)

FourPoint[q_, aa : HoldPattern[Times[___, (_[PD[Momentum[_, d___], _] ..]), a___,
b:((Pair[_[_, d___], Momentum[q_, d___]]|Pair[Momentum[q_, d___], _[_, d___]]) ..), ___]], opts___] :=
	OneLoop[q, Times[aa], opts] /; Length[{b}] < 4 && FreeQ[{a},q];

FourPoint[q_, aa : HoldPattern[Times[___, (_[ PD[Momentum[_, ___], _] ..]), a___]], opts___] /; FreeQ[{a},q] :=
	OneLoop[q, Times[aa], opts];

FourPoint[q_, aa : HoldPattern[_[PD[Momentum[_, ___], _] ..]], opts___] :=
	OneLoop[q, Times[aa], opts];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Ordering gamma matrices *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


gaso[DiracGamma[LorentzIndex[mu_, r1___], rr1___], DiracGamma[LorentzIndex[nu_, r2___], rr2___], opts___] :=
	(
	orderfkt = (OrderingFunction /. Flatten[{opts}] /. Options[GammaSort]);
	If[ orderfkt[{DiracGamma[LorentzIndex[mu, r1], rr1], DiracGamma[LorentzIndex[nu, r2], rr2]}],
		tl[DiracGamma[LorentzIndex[mu, r1], rr1], DiracGamma[LorentzIndex[nu, r2], rr2]],
		tl[DiracGamma[LorentzIndex[nu, r2], rr2], DiracGamma[LorentzIndex[mu, r1], rr1]] - 2*Pair[LorentzIndex[mu, r1], LorentzIndex[nu, r2]]
	]
	);

par[x_, i_] :=
	(
	pp = Cases[x, DiracGamma[__], Infinity, Heads -> True];
	If[ pp === 0,
		0,
		pp[[i]]
	]
	);

sortrules[opts___] :=
	(
	orderfkt = (OrderingFunction /. Flatten[{opts}] /. Options[GammaSort]);
		{
			tl[f___, fi_, red[fii_], fiii___] /; Head[fi] =!= Plus :>
			(
			iso = gaso[fi, fii, opts]; fiso = iso /. Pair[__] -> 0; giso = iso - fiso;
			If[ Sort[{f, fi, fii}][[-1]] =!= fii,
				tl[f, red[par[fiso, 1]], par[fiso, 2], fiii] + tl[f, giso, fiii],
				tl[f, red[fi], fii, fiii]
			]
			),
			tl[a___, b_, c__] /; FreeQ[b, DiracGamma] -> b*tl[a, c],
			tl[a__, b_, c___] /; FreeQ[b, DiracGamma] -> b*tl[a, c],
			tl[a___, Plus[b_, bb__], c__] :> Plus @@ (tl[a, #, c] & /@ {b, bb}),
			tl[f___, fi_, fii_] /; (! orderfkt[{f, fi, fii}] && FreeQ[{f, fi, fii}, red]) -> tl[f, fi, red[fii]],
			tl[red[f_], fi___, fii_] /; (! orderfkt[{f, fi, fii}]) -> tl[f, fi, red[fii]]
		}
	);

gammasort[xx__, opts___Rule | opts___List] :=
	tl[xx] //. sortrules[opts] /. tl -> DOT /. red[x_] -> x;

GammaSort[DOT[exp_, ex__], opts___] :=
	(
	exp1 =
		If[ (Gamma5AntiCommute /. Flatten[{opts}] /. Options[GammaSort]) && !FreeQ[tl[exp, ex], DiracGamma[LorentzIndex[5, ___], ___]],

			pos = Position[ tl[exp, ex], _?(!FreeQ[#, DiracGamma[LorentzIndex[5, ___], __]] &), {1}];
			(-1)^pos*Delete[DOT @@ Join[{tl[exp, ex][[pos]]}, {exp}], pos + 1],

			tl[exp, ex]
		];
	exp2 = exp1 /. {

		tl[aa___, a_, b__, c_, cc___] /; (!FreeQ[a, DiracGamma[LorentzIndex[5, ___], __] | gsort] &&
		!FreeQ[c, DiracGamma[LorentzIndex[5, ___], __] | gsort] && FreeQ[{b}, DiracGamma[LorentzIndex[5, ___], __] | gsort]) ->
			tl[aa, a, gsort[b], c, cc],

		tl[aa___, c_, cc___] /; (! FreeQ[c, DiracGamma[LorentzIndex[5, ___], __] | gsort] && FreeQ[{aa}, DiracGamma[LorentzIndex[5, ___], __] | gsort]) ->
			tl[gsort[aa], c, cc],

		tl[aa___, c_, cc___] /; (! FreeQ[c, DiracGamma[LorentzIndex[5, ___], __] | gsort] &&
		FreeQ[{cc}, DiracGamma[LorentzIndex[5, ___], __] | gsort]) ->
			tl[aa, c, gsort[cc]],

		tl[a__] /; FreeQ[{a}, DiracGamma[LorentzIndex[5, ___], __] | gsort] :>
			tl[gsort[a]]
	};
	DOT @@ Flatten[{exp2} /. {gsort -> gammasort, tl -> Sequence}]
	);

GammaSort[Plus[x_, y__], OptionsPattern[]] :=
	Plus @@ GammaSort /@ {x, y};

GammaSort[x_, OptionsPattern[]] /; FreeQ[x, DiracGamma] :=
	x;

GammaSort[DiracGamma[x__], OptionsPattern[]] :=
	DiracGamma[x];

GammaSort[DiracTrace[x_, op___], opts___] :=
	DiracTrace[GammaSort[x, opts], op];

GammaSort[Times[x_, y__], OptionsPattern[]] :=
	Times @@ GammaSort /@ {x, y};


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Gamma traces with Tracer *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


(* FeynCalc's DiracTrace[DOT[DiracGamma[momentum[p,D],D],DiracGamma[LorentzIndex[mu,D],D],...]],
DiracSigma[DiracGamma[LorentzIndex[mu]],DiracGamma[LorentzIndex[nu]]] are substituted with
Tracer's GammaTrace[l1,p,{mu},...], Sigma[{mu},{nu}] etc. *)

$TracerIndicesCounter = 0;
tin[opts___] :=
	(
	++$TracerIndicesCounter;
	ToExpression[(TracerIndicesString /. Flatten[{opts}] /. Options[FCToTracer]) <> ToString[$TracerIndicesCounter]]);

FCToTracer[exp_, opts___] :=
	(
	res1 = exp /. DiracTrace[DOT[a__], ___] :> trtr[tin[opts],
		DOT[a] /. {
			DOT[LorentzIndex[mu_, ___], LorentzIndex[nu_, ___]] -> trdot[{LorentzIndex[mu]}, {LorentzIndex[nu]}],
			fcsig -> trsig,
			DiracGamma[LorentzIndex[mu_, ___], ___] -> {LorentzIndex[mu]},
			Plus[aa___, b_, c___] /; FreeQ[b, DiracGamma] -> Plus[aa, trid*b, c],
			Eps[aa__] -> (treps[aa] /. LorentzIndex[mu_] -> {LorentzIndex[mu]})
		} /. DOT -> Sequence] /. DiracGamma[p_, ___] /; (! FreeQ[p, Momentum]) :> (p /. Momentum[pp_, ___] -> pp));

TracerToFC[exp_] :=
	(
	If[ Tracer`Private`d === 4,
		curdim = Sequence[],
		curdim = SpaceDimensions
	];
	trsp /@ Table[trl[i], {i, $TracerIndicesCounter}];
	res2 = exp /. Table[ToExpression[(TracerIndicesString /. Flatten[{opts}] /. Options[FCToTracer]) <> ToString[i]] -> trl[i], {i, $TracerIndicesCounter}];

	If[ !FreeQ[res2, trtr],
		Print["Expression not traced"];
		Abort[]
	];

	res2 /. {
		Momentum[p_] -> Momentum[p, curdim],
		{LorentzIndex[mu_]} -> LorentzIndex[mu],
		trdot -> Pair,
		trsig :> fcsig,
		treps[a__] :> (Eps[a] /. {LorentzIndex[mu_]} -> LorentzIndex[mu, curdim])
	}
	);

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Discarding higher orders *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(*Commented out 16/9-2002. Will look at DOT instead*)
(*SetAttributes[ditchmom, NumericFunction];*)
DiscardOrders[am_, opts___] :=
	(
	If[ !FreeQ[am, Phi`Couplings`WFFactor1],
		Message[DiscardOrders::wffac]
	];
	(spf = (Phi`Objects`ScalarProductForm /. Flatten[{opts}] /. Options[DiscardOrders]);
	Cancel[ExpandAll[ditchmom[]^2*am /.
		If[ (DiscardMomenta /. Flatten[{opts}] /. Options[DiscardOrders]),
			{
				ParticleMass[a__] -> ditchmom[]*ParticleMass[a, temprec],
				MandelstamS -> ditchmom[]^2*MandelstamS[temprec],
				MandelstamT -> ditchmom[]^2*MandelstamT[temprec],
				MandelstamU -> ditchmom[]^2*MandelstamU[temprec],
				Momentum[a__] /; FreeQ[{a}, Polarization] -> ditchmom[]*Momentum[a, temprec]
			},
			{}
		] /. (Rule[at : # /; FreeQ[at, temprec], ditchmom[]*at]&/@$ExpansionQuantities) //.
			{
				Pair[ditchmom[]*a_, b_] -> ditchmom[]*Pair[a, b],
				spf[ditchmom[]*a_, b_] -> ditchmom[]*spf[a, b]
			} /. ff_[temprec] -> ff /. temprec -> Sequence[]]] /. ditchmom[]^i_ /;
					i > (PerturbationOrder + 2 /. Flatten[{opts}] /. Options[DiscardOrders]) -> 0 /.
					ditchmom[] -> 1)
	);

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Characteristic polynomial and Cayley-Hamilton *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(*Se Karakteristisk polynomium og Newtonformler i H.A. Nielsen*)

CharacteristicCoefficient[a_, opts___Rule][i_] /; Phi`Objects`Private`gaugedimcheck[CharacteristicCoefficient, opts, a] === i :=
	(-1)^(Phi`Objects`Private`gaugedimcheck[CharacteristicCoefficient, opts, a]);

CharacteristicCoefficient[a_, opts___Rule][i_] :=
	Block[ {n = 	Phi`Objects`Private`gaugedimcheck[CharacteristicCoefficient, opts, a]},
		1/(i - n)Sum[UTrace[NMPower[a, k]]CharacteristicCoefficient[a, opts][i + k], {k, 1, n - i}]
	];

CayleyHamilton[m__, opts___Rule] :=
	CayleyHamilton[m, opts] =
		Block[ {n = Phi`Objects`Private`gaugedimcheck[CharacteristicCoefficient, opts, a], coms, len = Length[{m}],
				ch, el, submat},
			If[ len =!= n || (Eliminate /. {opts} /. Options[CayleyHamilton]) =!= True,
				If[ len =!= n,
					Message[CayleyHamilton::baddim]
				];
				Sum[CharacteristicCoefficient[UMatrix[b], opts][i] NMPower[UMatrix[b], i], {i, 0, n}] /. UMatrix[b] -> Plus[m] // CycleUTraces //
				NMExpand //	Expand // CycleUTraces,
				Do[
					coms = fccombs[{m}, k];
					ch[k] = Sum[
								Sum[CharacteristicCoefficient[UMatrix[b], opts][i]NMPower[UMatrix[b], i], {i, 0, n}] /.
								UMatrix[b] -> (Plus @@ coms[[l]]) // CycleUTraces // NMExpand // Expand, {l, Length[coms]}]//CycleUTraces;
					FCPrint[2, "Doing ", k, "-term Cayley-Hamilton"];
					el[k] = ch[k] -
					If[ k === 1,
						0,
						Plus@@Table[submat =
							NM[Sequence@@Table[{m}[[1]], {dum, 1, len - sp + 1}], Sequence@@ Drop[{m}, len - sp + 1]];
							FCPrint[2, "Eliminating ", submat, " with coefficients", " ", Coefficient[ch[k], submat], " ", Coefficient[el[sp], submat]];
							Coefficient[ch[k], submat]/Coefficient[el[sp], submat]el[sp], {sp, 1, k - 1}]
					] // Expand, {k, 1, len}
				];
				el[len]
			]
		];

(*Support function for calhamSort*)
(*Count number of adjacent Lorentz vectors*)
licount =
	(
	Count[# /. UTrace1 -> tr /. Power -> NMPower /. NM -> nm //.
		{
			nm[a___, l_?((!FreeQ[#, LorentzIndex[__]] && FreeQ[#, _nm]) &), ll_?((!FreeQ[#, LorentzIndex[__]] && FreeQ[#, _nm]) &), b___] :>
					nm[a, pp[Unique[pp]], b] /; Cases[l, LorentzIndex[__], Infinity] === Cases[ll, LorentzIndex[__], Infinity],
			nm[l_?((!FreeQ[#, LorentzIndex[__]] && FreeQ[#, _nm]) &), a__, ll_?((!FreeQ[#, LorentzIndex[__]] && FreeQ[#, _nm]) &)] :>
					nm[a, pp[Unique[pp]]] /; Cases[l, LorentzIndex[__], Infinity] === Cases[ll, LorentzIndex[__], Infinity]}, _pp, Infinity] &
	);

(*Function to sort matrices to find left-hand side*)
calhamSort =
	Block[ {tmp = # . UTrace1 -> tr},
	(
	Count[#1, _tr, Infinity] < Count[#2, _tr, Infinity] || Count[#1, _tr, Infinity] === Count[#2, _tr, Infinity] && licount[#1] < licount[#2] ||
	Count[#1, _tr, Infinity] === Count[#2, _tr, Infinity] && licount[#1] == licount[#2] && LeafCount[#1] > LeafCount[#2])
	]&;

CayleyHamiltonRules[mats_List, opts___Rule] :=
	Block[ {},
		calhamrules = {};
		FCPrint[3, Length[mats], " sets of matrices"];
		Do[
			FCPrint[2, j];
			len = Length[mats[[j]]];
			submats = Table[UMatrix[a[i]], {i, 1, len - 1}];
			calham =
				(
				subres = (UTrace[NM[CayleyHamilton[
				If[ j===1,
					FCPrint[2, "Calling CayleyHamilton on ", submats, " (first time only)"]
				];
				Sequence @@ submats, Sequence@@OptionsSelect[CayleyHamilton, Eliminate -> True, opts, Options[CayleyHamiltonRules]]],
				UMatrix[a[len]]]] //
				(FCPrint[3, "Expanding"]; #)& // NMExpand // Expand // CycleUTraces) /. (FCPrint[3, "Substituting matrices"];
				(*Get scalars out of UTrace1*)
				(Rule @@ #) & /@ Transpose[{Append[submats, UMatrix[a[len]]], mats[[j]]}]) /. Pattern -> pat /. pat -> Pattern)//
				(FCPrint[3, "Reducing ", subres]; #)& //
					If[ (UReduce /. {opts} /. Options[CayleyHamiltonRules])=!=False,
						FCPrint[1, "Doing UReduce"];
						UReduce[#, Sequence@@OptionsSelect[UReduce,
						Options[CayleyHamiltonRules],opts]] // CommutatorReduce // CycleUTraces,
						If[ (CommutatorReduce/.{opts}/.Options[CayleyHamiltonRules])=!=False,
							# //NMExpand // CommutatorReduce // CycleUTraces,
							#
						]
					]&;
				FCPrint[2, "Finding left-hand side of ", calham];
			scalham =
				Sort[List @@ Expand[calham /. UTrace1 -> tr /. Power -> NMPower], calhamSort ];
			fac = scalham[[1]] /. _tr -> 1;
			rightside = (-(Plus @@ Drop[scalham/fac, 1]) /. Pattern -> (({##}[[1]]) &));
			leftside = Cancel[scalham[[1]]/fac];
			FCPrint[2, "Left-hand side is ", leftside];
			calhamrules =
				Append[calhamrules, leftside -> rightside], {j, 1, Length[mats]}
		];

		If[ (CommutatorReduce/.{opts}/.Options[CayleyHamiltonRules])=!=False,
			calhamrules /. tr -> UTrace // CommutatorReduce,
			calhamrules /. tr -> UTrace
		]
	];


CayleyHamiltonTrick[exp_, opts___Rule] :=
	Block[ {ruls},
		ruls =
		CayleyHamiltonRules[
			FCPrint[1, "Building Cayley-Hamilton rules..."];
			UMatrices /. {opts} /. Options[CayleyHamiltonTrick], Sequence@@OptionsSelect[CayleyHamiltonRules, opts, Options[CayleyHamiltonTrick]]
		];
		FCPrint[1, "Applying rules"];
		exp /. ruls
	];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Reduction of lagrangians *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(* Here we deal symbolically with SMM[x] and MM[x], which should thus not
	be expanded *)

SetOptions[MM, Explicit -> False];
SetOptions[SMM, Explicit -> False];
SetOptions[CovariantFieldDerivative, Explicit -> False];

(* With no scalar and pseudo-scalar sources, \[Chi] is self-adjoined: *)

idRules = {
	nm[ff___, SMM[x_], Adjoint[SMM[x_]], ll___] -> nm[ff, ll],
	nm[ff___, Adjoint[SMM[x_]], SMM[x_], ll___] -> nm[ff, ll],
	nm[ff___, MM[x_], Adjoint[MM[x_]], ll___] -> nm[ff, ll],
	nm[ff___, Adjoint[MM[x_]], MM[x_], ll___] -> nm[ff, ll],
	nm[ff___, SMM[x_], Adjoint[MM[x_]], ll___] -> nm[ff, Adjoint[SMM[x]], ll],
	nm[ff___, Adjoint[SMM[x_]], MM[x_], ll___] -> nm[ff, SMM[x], ll],
	nm[ff___, Adjoint[MM[x_]], SMM[x_], ll___] -> nm[ff, Adjoint[SMM[x]], ll],
	nm[ff___, MM[x_], Adjoint[SMM[x_]], ll___] -> nm[ff, SMM[x], ll],
	UTrace1[nm[SMM[x_], ff___, Adjoint[SMM[x_]]]] :> UTrace1[nm[ff]],
	UTrace1[nm[Adjoint[SMM[x_]], ff___, SMM[x_]]] :> UTrace1[nm[ff]],
	UTrace1[nm[MM[x_], ff___, Adjoint[MM[x_]]]] :> UTrace1[nm[ff]],
	UTrace1[nm[Adjoint[MM[x_]], ff___, MM[x_]]] :> UTrace1[nm[ff]],
	Trace1[nm[SMM[x_], ff___, Adjoint[MM[x_]]]] :> UTrace1[nm[ff, Adjoint[SMM[x]]]],
	Trace1[nm[MM[x_], ff___, Adjoint[SMM[x_]]]] :> UTrace1[nm[ff, SMM[x]]],
	UTrace1[nm[Adjoint[MM[x_]], ff___, SMM[x_]]] :>	UTrace1[nm[ff, Adjoint[SMM[x]]]],
	UTrace1[nm[Adjoint[SMM[x_]], ff___, MM[x_]]] :> UTrace1[nm[ff, SMM[x]]]
};


UIdTrick[expr_,opts___Rule] :=
	expr /. NM -> nm /. idRules /. nm -> NM /. UTrace1[] :> (Phi`Objects`Private`gaugedimcheck[UReduce,opts,expr]);

(* At least with the exponential representation these traces are 0. Follows also
	from det u =1. *)

traceRules0 = {
	UTrace1[NM[SMM[x_], Adjoint[FieldDerivative[SMM[x_], _, LorentzIndex[_]]]]] -> 0,
	UTrace1[NM[Adjoint[SMM[x_]], FieldDerivative[SMM[x_], _, LorentzIndex[_]]]] -> 0,
	UTrace1[NM[Adjoint[FieldDerivative[SMM[x_], _, LorentzIndex[_]]], SMM[x_]]] -> 0,
	UTrace1[NM[FieldDerivative[SMM[x_], _, LorentzIndex[_]], Adjoint[SMM[x_]]]] -> 0,
	UTrace1[NM[SMM[x_],	Adjoint[CovariantFieldDerivative[SMM[x_], _, LorentzIndex[_]]]]] -> 0,
	UTrace1[NM[Adjoint[SMM[x_]], CovariantFieldDerivative[SMM[x_], _, LorentzIndex[_]]]] -> 0,
	UTrace1[NM[Adjoint[CovariantFieldDerivative[SMM[x_], _, LorentzIndex[_]]], SMM[x_]]] -> 0,
	UTrace1[NM[CovariantFieldDerivative[SMM[x_], _, LorentzIndex[_]], Adjoint[SMM[x_]]]] -> 0,
	UTrace1[NM[FieldDerivative[FieldDerivative[SMM[x_], x_, LorentzIndex[li2_]], x_, LorentzIndex[li1_]], Adjoint[SMM[x_]]]] ->
		-UTrace1[NM[FieldDerivative[SMM[x], x, LorentzIndex[li1]], Adjoint[FieldDerivative[SMM[x], x, LorentzIndex[li2]]]]],
	UTrace1[NM[SMM[x_],	Adjoint[FieldDerivative[FieldDerivative[SMM[x_], x_, LorentzIndex[li2_]], x_, LorentzIndex[li1_]]]]] ->
		-UTrace1[NM[FieldDerivative[SMM[x], x, LorentzIndex[li2]], Adjoint[FieldDerivative[SMM[x], x, LorentzIndex[li1]]]]]
	};

traceRules =
	Join[traceRules0, traceRules0 /. SMM -> MM];

UTraceTrick :=
	((# /. traceRules) &);

(* We will need to simplify using that total derivatives vanish: *)

uDagRul =
	NM[SMM[x_], Adjoint[FieldDerivative[FieldDerivative[SMM[x_], x_, LorentzIndex[li2_]], x_, LorentzIndex[li1_]]]] :>
		-(NM[FieldDerivative[SMM[x], x, LorentzIndex[li2]], Adjoint[FieldDerivative[SMM[x], x, LorentzIndex[li1]]]] +
		NM[FieldDerivative[FieldDerivative[SMM[x], x, LorentzIndex[li2]], x, LorentzIndex[li1]], Adjoint[SMM[x]]] +
		NM[FieldDerivative[SMM[x], x, LorentzIndex[li1]], Adjoint[FieldDerivative[SMM[x], x, LorentzIndex[li2]]]]);

UDagRul = uDagRul /. SMM -> MM;

(* The derivative of the product of U and Adjoint[U] is 0. And the derived rules works
also for the covariant derivative. That is, NM[CovariantFieldDerivative[U],Adjoint[U]] =
				-NM[U,Adjoint[CovariantFieldDerivative[U]]] *)

du[li1_][x_] =
	FieldDerivative[SMM[x], x, LorentzIndex[li1]];

dua[li1_][x_] =
	Adjoint[FieldDerivative[SMM[x], x, LorentzIndex[li1]]];

ddu[li1_][x_] =
	CovariantFieldDerivative[SMM[x], x, LorentzIndex[li1]];

ddua[li1_][x_] =
	Adjoint[CovariantFieldDerivative[SMM[x], x, LorentzIndex[li1]]];

uRules10 = {
	rul[nm[f___, duu[li1_][x_], Adjoint[SMM[x_]], g___],
	cond[-nm[f, SMM[x], duaa[li1][x], g], (mq[pt[{dum, f}, -1], SMM[_] | Adjoint[SMM[_]]] || mq[pt[{dum, dum, f}, {-2, -1}],
	{duu[_][_] | duaa[_][_] | SMM[_] | Adjoint[SMM[_]], duu[_][_] | duaa[_][_] | SMM[_] | Adjoint[SMM[_]]}] ||
	eq[{f}, {}] && (mq[pt[{dum, g}, -1],  SMM[_] | Adjoint[SMM[_]]] || mq[pt[{dum, dum, g}, {-2, -1}], {duu[_][_] |
	duaa[_][_] | SMM[_] | Adjoint[SMM[_]], duu[_][_] | duaa[_][_] | SMM[_] | Adjoint[SMM[_]]}]))]], rul[nm[f___, duaa[li1_][x_], SMM[x_], g___],
	cond[-nm[f, Adjoint[SMM[x]], duu[li1][x], g], (mq[pt[{dum, f}, -1],	SMM[_] | Adjoint[SMM[_]]] || mq[pt[{dum, dum, f}, {-2, -1}],
	{duu[_][_] | duaa[_][_] | SMM[_] | Adjoint[SMM[_]],	duu[_][_] | duaa[_][_] | SMM[_] | Adjoint[SMM[_]]}] || eq[{f}, {}] &&
	(mq[pt[{dum, g}, -1], SMM[_] | Adjoint[SMM[_]]] || mq[pt[{dum, dum, g}, {-2, -1}], {duu[_][_] | duaa[_][_] | SMM[_] |
	Adjoint[SMM[_]], duu[_][_] | duaa[_][_] | SMM[_] | Adjoint[SMM[_]]}]))]]
} /. {{duu -> du, duaa -> dua}, {duu -> ddu, duaa -> ddua}} /. rul -> RuleDelayed /. cond -> Condition /. {mq -> MatchQ, eq -> SameQ} /. pt -> Part // Flatten;

uRules1 =
	Join[uRules10, uRules10 /. SMM -> MM];

applyuRules1 =
	(# /. NM -> nm /. uRules1 /. nm -> NM) &;

UOrder =
	(# /. NM -> nm /. (uRules1 /. Condition -> cc /. cc[a_, _] -> a) /. nm -> NM) &;

uRules20 = {

	rul[nm[f___, SMM[x_], duaa[li1_][x_], g___], cond[-nm[f, duu[li1][x], Adjoint[SMM[x]], g],
	(mq[pt[{g, dum}, 1], SMM[_] | Adjoint[SMM[_]]] || mq[pt[{g, dum, dum}, {1, 2}], {duu[_][_] | duaa[_][_] |
	SMM[_] | Adjoint[SMM[_]], duu[_][_] | duaa[_][_] | SMM[_] |	Adjoint[SMM[_]]}] || eq[{g}, {}] && (mq[pt[{f, dum}, 1],
	SMM[_] | Adjoint[SMM[_]]] || mq[pt[{f, dum, dum}, {1, 2}], {duu[_][_] |	duaa[_][_] | SMM[_] | Adjoint[SMM[_]],
	duu[_][_] | duaa[_][_] | SMM[_] | Adjoint[SMM[_]]}]))]],

	rul[nm[f___, Adjoint[SMM[x_]], duu[li1_][x_], g___],
	cond[-nm[f, duaa[li1][x], SMM[x], g], (mq[pt[{g, dum}, 1], SMM[_] | Adjoint[SMM[_]]] ||
	mq[pt[{g, dum, dum}, {1, 2}], {duu[_][_] | duaa[_][_] | SMM[_] | Adjoint[SMM[_]], duu[_][_] | duaa[_][_] |
	SMM[_] | Adjoint[SMM[_]]}] || eq[{g}, {}] && (mq[pt[{f, dum}, 1], SMM[_] | Adjoint[SMM[_]]] ||
	mq[pt[{f, dum, dum}, {1, 2}], {duu[_][_] | duaa[_][_] | SMM[_] | Adjoint[SMM[_]], duu[_][_] | duaa[_][_] |
	SMM[_] | Adjoint[SMM[_]]}]))]]

} /. {{duu -> du,  duaa -> dua}, {duu -> ddu, duaa -> ddua}} /. rul -> RuleDelayed /. cond -> Condition /.
	{mq -> MatchQ, eq -> SameQ} /. pt -> Part // Flatten;

uRules2 =
	Join[uRules20, uRules20 /. SMM -> MM];

applyuRules2 =
	(# /. NM -> nm /. uRules2 /. nm -> NM) &;

UOrder1 =
	(# /. NM -> nm /. (uRules2 /. Condition -> cc /. cc[a_, _] -> a) /. nm -> NM) &;

applyuRules12 =
	(# /. NM -> nm /. uRules1 /. uRules2 /. nm -> NM) &;

applyuRules12n =
	(# /. NM -> nm /. (uRules1 /. Condition -> cc /. cc[a_, _] -> a) /. (uRules2 /. Condition -> cc /. cc[a_, _] -> a) /. nm -> NM) &;

applyuRules21 =
	(# /. NM -> nm /. uRules2 /. uRules1 /. nm -> NM) &;

applyuRules21n =
	(# /. NM -> nm /. (uRules2 /. Condition -> cc /. cc[a_, _] -> a) /. (uRules1 /. Condition -> cc /. cc[a_, _] -> a) /. nm -> NM) &;

(* Use this to get u's side by side: *)

usurules = {
	(NM[f___, SMM[x_] | Adjoint[SMM[x_]], SMM[x_] | Adjoint[SMM[x_]], l___] | NM[f___, MM[x_] | Adjoint[MM[x_]],
	MM[x_] | Adjoint[MM[x_]], l___]) ->
		NM[f, usu, l], (UTrace1[NM[SMM[x_] | Adjoint[SMM[x_]], f__, SMM[x_] | Adjoint[SMM[x_]]]] |
		UTrace1[NM[MM[x_] | Adjoint[MM[x_]], f__, MM[x_] | Adjoint[MM[x_]]]]) -> NM[f, usu]
};

UPair[exp_, opts___Rule] :=
	(exp /. (a : HoldPattern[NM[__]]) :>
		(tmpa = a // applyuRules1;
		If[ Count[a /. usurules, usu, Infinity] > Count[tmpa /. usurules, usu, Infinity],
			a,
			tmpa
		]
		) /. (a : HoldPattern[NM[__]]) :> (tmpa = a // applyuRules2;
		If[ Count[a /. usurules, usu, Infinity] > Count[tmpa /. usurules, usu, Infinity],
			a,
			tmpa
		]) /. (a : HoldPattern[NM[__]]) :> (tmpa = a // applyuRules12;
		If[ Count[a /. usurules, usu, Infinity] > Count[tmpa /. usurules, usu, Infinity],
			a,
			tmpa
		]) /. (a : HoldPattern[NM[__]]) :> (tmpa = a // applyuRules21;
		If[ Count[a /. usurules, usu, Infinity] > Count[tmpa /. usurules, usu, Infinity],
			a,
			tmpa
		]) // UIdTrick[#,opts]&
	);

UDrop[exp_, opts___Rule] :=
	Block[ {(*tmpexp,tmpexp1,res*)},
		tmpexp = exp // UOrder //  UIdTrick[#,opts]& // CycleUTraces;
		tmpexp1 = exp /. UTrace1 -> (UTrace[RotateLeft[#]] &) // UOrder //  UIdTrick[#,opts]& // CycleUTraces;
		res = {exp,tmpexp,tmpexp1}[[Ordering[LeafCount/@{exp,tmpexp,tmpexp1}][[1]]]];
		tmpexp = res // UOrder1 //  UIdTrick[#,opts]& // CycleUTraces;
		tmpexp1 = res /. UTrace1 -> (UTrace[RotateLeft[#]] &) // UOrder1 //  UIdTrick[#,opts]& // CycleUTraces;
		res = {exp,tmpexp,tmpexp1}[[Ordering[LeafCount/@{exp,tmpexp,tmpexp1}][[1]]]];
		tmpexp = res // applyuRules12n //  UIdTrick[#,opts]& // CycleUTraces;
		tmpexp1 = res /. UTrace1 -> (UTrace[RotateLeft[#]] &) // applyuRules12n //  UIdTrick[#,opts]& // CycleUTraces;
		res = {exp,tmpexp,tmpexp1}[[Ordering[LeafCount/@{exp,tmpexp,tmpexp1}][[1]]]];
		tmpexp = res // applyuRules21n //  UIdTrick[#,opts]& // CycleUTraces;
		tmpexp1 = res /. UTrace1 -> (UTrace[RotateLeft[#]] &) // applyuRules21n //  UIdTrick[#,opts]& // CycleUTraces;
		res = {exp,tmpexp,tmpexp1}[[Ordering[LeafCount/@{exp,tmpexp,tmpexp1}][[1]]]];
		res
	];


(*SU(2) rules for tr(d_mu U d_mu U^(+) d_nu U d_nu U^(+)) - not Cayley-Hamilton, but follow from
writing out the exponentials and using sigma.dphi sigma.dphi = dphi.dphi in SU(2) (no SU2D)
- or from inserting U U^(+) and decomposing the traceless matrices dU U^(+) in Pauli matrices*)
(*See Dobado,Gomez-Nicola,Maroto,Pelaez p. 149*)

SUNURules[2] :=
	If[ (Explicit/.Options[MM])===False && (Explicit/.Options[SMM])===False,
		{
			UTrace1[NM[Adjoint[CovariantFieldDerivative[MM[x_], x_, LorentzIndex[mu1_]]],
			CovariantFieldDerivative[MM[x_], x_, LorentzIndex[mu2_]],
			Adjoint[CovariantFieldDerivative[MM[x_], x_, LorentzIndex[mu2_]]],
			CovariantFieldDerivative[MM[x_], x_, LorentzIndex[mu1_]]]] ->
				1/2*UTrace[NM[Adjoint[CovariantFieldDerivative[MM[x], x, LorentzIndex[mu1]]],
				CovariantFieldDerivative[MM[x], x, LorentzIndex[mu1]]]]UTrace[NM[Adjoint[
				CovariantFieldDerivative[MM[x], x, LorentzIndex[mu2]]],
				CovariantFieldDerivative[MM[x], x, LorentzIndex[mu2]]]],

			UTrace1[NM[Adjoint[CovariantFieldDerivative[MM[x_], x_, LorentzIndex[mu1_]]],
					CovariantFieldDerivative[MM[x_], x_, LorentzIndex[mu1_]],
					Adjoint[CovariantFieldDerivative[MM[x_], x_, LorentzIndex[mu2_]]],
					CovariantFieldDerivative[MM[x_], x_, LorentzIndex[mu2_]]]] ->
				1/2*UTrace[	NM[Adjoint[CovariantFieldDerivative[MM[x], x, LorentzIndex[mu1]]],
					CovariantFieldDerivative[MM[x], x, LorentzIndex[mu1]]]]UTrace[NM[Adjoint[
					CovariantFieldDerivative[MM[x], x, LorentzIndex[mu2]]],
					CovariantFieldDerivative[MM[x], x, LorentzIndex[mu2]]]]
		},
		{}
	];


SUNURules[_] :=
	{};


(*Going from u to U *)

(*WRONG?!*)
(*SU(2) rules like above*)
(* diffBigURules[2] :=
	{NM[CovariantFieldDerivative[SMM[x_], x_, LorentzIndex[mu1_]],
			Adjoint[CovariantFieldDerivative[SMM[x_], x_, LorentzIndex[mu1_]]]] :>
	1/4 NM[CovariantFieldDerivative[MM[x], x, LorentzIndex[mu1]],
			Adjoint[CovariantFieldDerivative[MM[x], x, LorentzIndex[mu1]]]],
	UTrace1[NM[Adjoint[CovariantFieldDerivative[SMM[x_], x_, LorentzIndex[mu1_]]],
							m__,CovariantFieldDerivative[SMM[x_], x_, LorentzIndex[mu1_]]]] :>
	1/4 UTrace1[NM[Adjoint[CovariantFieldDerivative[MM[x], x, LorentzIndex[mu1]]],
			m,CovariantFieldDerivative[MM[x], x, LorentzIndex[mu1]]]],
	NM[Adjoint[CovariantFieldDerivative[SMM[x_], x_, LorentzIndex[mu1_]]],
		CovariantFieldDerivative[SMM[x_], x_, LorentzIndex[mu1_]]] :>
	1/4 NM[Adjoint[CovariantFieldDerivative[MM[x], x, LorentzIndex[mu1]]],
					CovariantFieldDerivative[MM[x], x, LorentzIndex[mu1]]],
	UTrace1[NM[CovariantFieldDerivative[SMM[x_], x_, LorentzIndex[mu1_]],
	m__,Adjoint[CovariantFieldDerivative[SMM[x_], x_, LorentzIndex[mu1_]]]]] :>
	1/4 UTrace1[NM[CovariantFieldDerivative[MM[x], x, LorentzIndex[mu1]],
			m,Adjoint[CovariantFieldDerivative[MM[x], x, LorentzIndex[mu1]]]]]};    *)

diffBigURules[_] :=
	{};


diffURules = {
	NM[FieldDerivative[SMM[x_], x_, LorentzIndex[li1_]], SMM[x_]] ->
		FieldDerivative[MM[x], x, LorentzIndex[li1]] - NM[SMM[x], FieldDerivative[SMM[x], x, LorentzIndex[li1]]],
	NM[Adjoint[FieldDerivative[SMM[x_], x_, LorentzIndex[li1_]]], Adjoint[SMM[x_]]] ->
		Adjoint[FieldDerivative[MM[x], x, LorentzIndex[li1]]] - NM[Adjoint[SMM[x]], Adjoint[FieldDerivative[SMM[x], x, LorentzIndex[li1]]]]
	};

bigURules = {
	nm[f___, SMM[x_], SMM[x_], l___] -> nm[f, MM[x], l],
	nm[f___, Adjoint[SMM[x_]], Adjoint[SMM[x_]], l___] ->
		nm[f, Adjoint[MM[x]], l],
	nm[SMM[x_], m__, SMM[x_]] -> nm[MM[x], m],
	nm[Adjoint[SMM[x_]], m__, Adjoint[SMM[x_]]] ->
		nm[m, Adjoint[MM[x]]]
};

SMMToMM[exp_,opts___Rule] :=
	(
	max = Max[(Length /@ Cases[exp, _NM, Infinity])];
	FixedPoint[(# /. bigURules /. diffURules  /. diffBigURules[(SUNN /. {opts} /. Options[UReduce])]// NMExpand) &, exp /. NM -> nm, max] /. nm -> NM
	);

(* Have tr(d_mu U d_mu U^(+) d_nu U d_nu U^(+)) replaced with
	tr(d_mu U^(+)  d_mu Ud_nu U^(+) d_nu U).
	Follows from writing in terms of u_mu's. If anyone should want to do higher order
	calculations than p^8, well change the 8 below accordingly :-)*)

ddURules =
	Table[(ex = (
		cut[UTrace1[nm@@Table[seq[idd[CovariantFieldDerivative[MM[pat[x,_]],pat[x,_], LorentzIndex[mu[i]]]],
			adj[CovariantFieldDerivative[MM[pat[x,_]],pat[x,_], LorentzIndex[mu[i+1]]]]],{i,1,n,2}]]]/.seq->Sequence
		);
		rull[(ex/.mu:>(pat[ToExpression["mu"<>ToString[#]], Blank[]]&)/.pat->Pattern/.{idd-> Identity,adj->Adjoint}),
		condd[(ex/.mu:>(ToExpression["mu"<>ToString[#]]&)/.{idd->Adjoint, adj->Identity}/.pat[_,_]->x),
		usq[sor[{mu1,mu2}],{mu1,mu2}]]]),{n,2,8,2}]/.{nm->NM, rull->RuleDelayed,condd->Condition,usq->SameQ, sor->Sort, cut->CycleUTraces};

ddURules1 =
		Table[(ex = (cut[UTrace1[nm@@Table[
			seq[idd[CovariantFieldDerivative[MM[pat[x,_]],pat[x,_], LorentzIndex[mu[i]]]],
			adj[CovariantFieldDerivative[MM[pat[x,_]],pat[x,_], LorentzIndex[mu[i+1]]]]],{i,1,n,2}]]]/.seq->Sequence);
			rull[(ex/.mu:>(pat[ToExpression["mu"<>ToString[#]], Blank[]]&)/.pat->Pattern/.{idd->Adjoint, adj->Identity}),
				condd[(ex/.mu:>(ToExpression["mu"<>ToString[#]]&)/.{idd->Identity, adj->Adjoint}/.pat[_,_]->x),
					usq[sor[{mu1,mu2}],{mu1,mu2}]]]), {n,2,8,2}
		]/. {nm->NM, rull->RuleDelayed,condd->Condition,usq->SameQ, sor->Sort, cut->CycleUTraces};

UReduce[exp_, opts___Rule] :=
	Block[ {res,opsMM,opsSMM,end},
		opsMM = Options[MM];
		opsSMM = Options[SMM];
		SetOptions[MM, Explicit -> False];
		SetOptions[SMM, Explicit -> False];
		res =
			If[	(SMMToMM /. {opts} /. Options[UReduce]) =!= True,
				(*Added inner UPair in order to force cancellation in SU(3) CayleyHamiltonRules*)
				FixedPoint[CycleUTraces[UPair[UTraceTrick[UIdTrick[UPair[NMExpand[# /. uDagRul /. UDagRul  /. ddURules /. ddURules1]],opts] /.
				SUNURules[(SUNN /. {opts} /. Options[UReduce])]],opts]]&, exp, 10],
				FixedPoint[CycleUTraces[SMMToMM[UPair[UTraceTrick[UIdTrick[UPair[NMExpand[# /. uDagRul /. UDagRul /. ddURules /.  ddURules1]],opts] /.
				SUNURules[(SUNN /. {opts} /. Options[UReduce])]],opts],opts]]&, exp, 10]
			];
		end =
			If[	(FullReduce /. {opts} /. Options[UReduce]) === True,
				FixedPoint[CycleUTraces[UDrop[UTraceTrick[UIdTrick[NMExpand[# /. uDagRul /. UDagRul],opts]],opts]]&, res, 10],
				res
			];
		Options[MM] = opsMM;
		Options[SMM] = opsSMM;
		end
	];



(* Total derivatives vanish upon integration *)

(* Should be cleaned up and generalized to traces of products
	involving covariant derivatives *)

(*Disables checks for differentation orders all together*)
(*surfaceRules[0]:=(surfaceRules[1]/. Condition -> cond /.
cond[a_, __] -> (*a*) Condition[a,usq[fq[dd, ufis, Heads -> True],True]]);

surfaceRules1[0]:=(surfaceRules1[1]/. Condition -> cond /.
cond[a_, __] -> (*a*) Condition[a,usq[fq[dd, ufis, Heads -> True],True]]);*)

surfaceRules[0] :=
	(surfaceRules[1] /. sr0 -> True);

surfaceRules1[0] :=
	(surfaceRules1[1] /. sr0 -> True);

surfaceRules[n_] :=
	{
	surdum + nm[f___,FieldDerivative[dd_,x_,LorentzIndex[li1_]],r___] :>
		surdum + idd[-nm[fdr[nm[f,r],x,LorentzIndex[li1]],dd]]/; (FreeQ[dd, ufis, Heads->True] =!=True)&&
	(sr0 || Length[{f,r}]>0&&((m1 = Max[Depth/@Union[Cases[{f,r},_FCPartialD| _FieldDerivative,Infinity,Heads->True]]]) <
	(m2 = Max[Depth/@Union[Cases[{dd},_FCPartialD| _FieldDerivative,Infinity,Heads->True]]])+n || m1==-Infinity&&m2==-Infinity&&n<=1)),

	surdum + nm[uo___,NM[f___,FieldDerivative[dd_,x_,LorentzIndex[li1_]],r___], ou___] :>
		surdum + idd[nm[nm[uo,ou,-nm[NM[fdr[NM[f],x,LorentzIndex[li1]],dd,r]]- nm[NM[f,dd,fdr[NM[r],x,LorentzIndex[li1]]]]]-
		nm[fdr[nm[uo,ou],x,LorentzIndex[li1]],NM[f,dd,r]]]]/; (FreeQ[dd, ufis, Heads->True] =!=True) &&
		(sr0 || (m1 = Max[Depth/@Union[Cases[{f,r,ou},_FCPartialD|_FieldDerivative,Infinity, Heads->True]]])<(m2 = Max[Depth/@
		Union[Cases[{dd},_FCPartialD|_FieldDerivative,Infinity, Heads->True]]])+n || m1==-Infinity&&m2==-Infinity&&n<=1),

	surdum + nm[uo___, utr[NM[f___,FieldDerivative[dd_,x_, LorentzIndex[li1_]], r___]],ou___]:>
		surdum + idd[-nm[uo,ou,utr[NM[fdr[NM[f],x,LorentzIndex[li1]],dd,r]+ NM[f,dd,fdr[NM[r],x,LorentzIndex[li1]]]]]-
		nm[fdr[nm[uo,ou],x,LorentzIndex[li1]], utr[NM[f,dd,r]]]]/;
		((FreeQ[dd, ufis, Heads->True] =!=True)) && (sr0 || (m1 = Max[Depth/@Union[Cases[{f,r,ou},_FCPartialD| _FieldDerivative,Infinity,
		Heads->True]]])<(m2 = Max[Depth/@Union[Cases[{dd},_FCPartialD|	_FieldDerivative,Infinity, Heads->True]]])+n || m1==-Infinity&&m2==-Infinity&&n<=1),

	surdum + nm[uo___,NM[f___,Adjoint[FieldDerivative[dd_,x_,LorentzIndex[li1_]]],r___], ou___] :>
		surdum + idd[nm[nm[uo,ou,-NM[fdr[NM[f],x,LorentzIndex[li1]],Adjoint[dd],r]- NM[f,Adjoint[dd],fdr[NM[r],x,LorentzIndex[li1]]]]-
		nm[fdr[nm[uo,ou],x,LorentzIndex[li1]],NM[f,Adjoint[dd],r]]]]/; (FreeQ[dd, ufis, Heads->True] =!=True)&& (sr0 || (m1 = Max[Depth/@
		Union[Cases[{f,r,ou},_FCPartialD|_FieldDerivative,Infinity, Heads->True]]])<(m2 = Max[Depth/@Union[Cases[{dd},_FCPartialD|_FieldDerivative,Infinity,
		Heads->True]]])+n || m1==-Infinity&&m2==-Infinity&&n<=1),

	surdum + nm[uo___,utr[NM[f___,Adjoint[FieldDerivative[dd_,x_, LorentzIndex[li1_]]], r___]],ou___]:>
		surdum + idd[-nm[uo,ou,	utr[NM[fdr[NM[f],x,LorentzIndex[li1]],Adjoint[dd],r] +NM[f,Adjoint[dd],fdr[NM[r],x,LorentzIndex[li1]]]]]-
		nm[fdr[nm[uo,ou],x,LorentzIndex[li1]],	utr[NM[f,Adjoint[dd],r]]]]/; (FreeQ[dd, ufis, Heads->True] =!=True)&& (sr0 || (m1 = Max[Depth/@
		Union[Cases[{f,r,ou},_FCPartialD| _FieldDerivative,Infinity, Heads->True]]])<= (m2 = Max[Depth/@ Union[Cases[{dd},_FCPartialD|_FieldDerivative,Infinity,
		Heads->True]]])+n-1 || m1==-Infinity&&m2==-Infinity&&n<=1)
	};

surfaceRules1[n_] :=
	{
	surdum + nm[f___,dd:(QuantumField[FCPartialD[li1_,___], _, ___][x_] | (IsoVector | UVector | UMatrix)[QuantumField[FCPartialD[li1_,___], __]][x_] |
	IsoDot[IsoVector[QuantumField[FCPartialD[li1_,___], __]][x_], IsoVector[UMatrix[UGenerator[___], ___], ___]]),r___] :>
		surdum + idd[-nm[fdr[nm[f,r],x,LorentzIndex[li1]],dd/.FCPartialD[__]->Sequence[]]]/;(FreeQ[dd, ufis, Heads->True] =!=True)&& (sr0 || Length[{f,r}]>0&&((m1 = Max[Depth/@
		Union[Cases[{f,r},_FCPartialD| _FieldDerivative,Infinity,Heads->True]]])< (m2 = Max[Depth/@	Union[Cases[{dd},_FCPartialD| _FieldDerivative,Infinity,Heads->True]]])+n ||
		m1==-Infinity&&m2==-Infinity&&n<=1)),

	surdum + nm[uo___,NM[f___,dd:(QuantumField[FCPartialD[li1_,___], _, ___][x_] | (IsoVector | UVector | UMatrix)[QuantumField[FCPartialD[li1_,___], __]][x_] |
	IsoDot[IsoVector[QuantumField[FCPartialD[li1_,___], __]][x_], IsoVector[UMatrix[UGenerator[___], ___], ___]]),r___], ou___] :>
		surdum + idd[nm[nm[uo,ou,-nm[NM[fdr[NM[f],x,LorentzIndex[li1]],dd/.FCPartialD[__]->Sequence[],r]]- nm[NM[f,dd,fdr[NM[r],x,LorentzIndex[li1]]]]]-
		nm[fdr[nm[uo,ou],x,LorentzIndex[li1]],NM[f,dd/.FCPartialD[__]->Sequence[],r]]]]/; (FreeQ[dd, ufis, Heads->True] =!=True)&& (sr0 || (m1 = Max[Depth/@
		Union[Cases[{f,r,ou},_FCPartialD|_FieldDerivative,Infinity, Heads->True]]])< (m2 = Max[Depth/@ Union[Cases[{dd},_FCPartialD| _FieldDerivative,Infinity,
		Heads->True]]])+n || m1==-Infinity&&m2==-Infinity&&n<=1),

	surdum + nm[uo___, utr[NM[f___,dd:(QuantumField[FCPartialD[li1_,___], _, ___][x_] | (IsoVector | UVector | UMatrix)[QuantumField[FCPartialD[li1_,___], __]][x_] |
	IsoDot[IsoVector[QuantumField[FCPartialD[li1_,___], __]][x_], IsoVector[UMatrix[UGenerator[___], ___], ___]]), r___]],ou___]:>
		surdum + idd[-nm[uo,ou,utr[	NM[fdr[NM[f],x,LorentzIndex[li1]],dd/.FCPartialD[__]->Sequence[],r]+ NM[f,dd/.FCPartialD[__]->Sequence[],fdr[NM[r],x,LorentzIndex[li1]]]]]-
		nm[fdr[nm[uo,ou],x,LorentzIndex[li1]], utr[NM[f,dd/.FCPartialD[__]->Sequence[],r]]]]/; (FreeQ[dd, ufis, Heads->True] =!=True)&& (sr0 || (m1 = Max[Depth/@
		Union[Cases[{f,r,ou},_FCPartialD|_FieldDerivative,Infinity,	Heads->True]]])< (m2 = Max[Depth/@ Union[Cases[{dd},_FCPartialD| _FieldDerivative,Infinity,
		Heads->True]]])+n || m1==-Infinity&&m2==-Infinity&&n<=1),

	surdum + nm[uo___,NM[f___,Adjoint[dd:(QuantumField[FCPartialD[li1_,___], _, ___][x_] | (IsoVector | UVector | UMatrix)[QuantumField[FCPartialD[li1_,___], __]][x_] |
	IsoDot[IsoVector[QuantumField[FCPartialD[li1_,___], __]][x_], IsoVector[UMatrix[UGenerator[___], ___], ___]])],r___], ou___] :>
		surdum + idd[nm[nm[uo,ou,-NM[fdr[NM[f],x,LorentzIndex[li1]],Adjoint[dd/.FCPartialD[__]->Sequence[]],r]-	NM[f,Adjoint[dd/.FCPartialD[__]->Sequence[]],fdr[NM[r],x,LorentzIndex[li1]]]]-
		nm[fdr[nm[uo,ou],x,LorentzIndex[li1]],NM[f,Adjoint[dd/.FCPartialD[__]->Sequence[]],r]]]]/; (FreeQ[dd, ufis, Heads->True] =!=True)&& (sr0 || (m1 = Max[Depth/@ Union[Cases[{f,r,ou},_FCPartialD|
		_FieldDerivative,Infinity, Heads->True]]])<(m2 = Max[Depth/@Union[Cases[{dd},_FCPartialD|_FieldDerivative,Infinity, Heads->True]]])+n || m1==-Infinity&&m2==-Infinity&&n<=1),

	surdum + nm[uo___,utr[NM[f___,Adjoint[dd:(QuantumField[FCPartialD[li1_,___], _, ___][x_] | (IsoVector | UVector | UMatrix)[QuantumField[FCPartialD[li1_,___], __]][x_] |
	IsoDot[IsoVector[QuantumField[FCPartialD[li1_,___], __]][x_], IsoVector[UMatrix[UGenerator[___], ___], ___]])], r___]],ou___]:>
		surdum + idd[-nm[uo,ou, utr[NM[fdr[NM[f],x,LorentzIndex[li1]],Adjoint[dd/.FCPartialD[__]->Sequence[]],r] +NM[f,Adjoint[dd/.FCPartialD[__]->Sequence[]],fdr[NM[r],x,LorentzIndex[li1]]]]]-
		nm[fdr[nm[uo,ou],x,LorentzIndex[li1]], utr[NM[f,Adjoint[dd/.FCPartialD[__]->Sequence[]],r]]]]/; (FreeQ[dd, ufis, Heads->True] =!=True)&& (sr0 || (m1 = Max[Depth/@ Union[Cases[{f,r,ou},_FCPartialD|
		_FieldDerivative,Infinity, Heads->True]]])<= (m2 = Max[Depth/@Union[Cases[{dd},_FCPartialD| _FieldDerivative,Infinity, Heads->True]]])+n-1 || m1==-Infinity&&m2==-Infinity&&n<=1)
	};

SurfaceReduce[expr_,opts___Rule] :=
	Block[{n = DifferenceOrder/.{opts}/.Options[SurfaceReduce],re,res,qf,pd,r,x,isod},
		ufis = UFields/.{opts}/.Options[SurfaceReduce];
		re = Expand[surdum + expr];

		res = surdum + Expand[NMExpand[
			If[ Head[#]===Times,
				nm@@#,
				nm[#]
			]&/@re]] /. UTrace1[a_]^n_ :> (Sequence@@Table[utr[a],{n}])  /. UTrace1->utr //.
			(surfaceRules1[n]/.sr0->False/.fq->FreeQ/.usq->UnsameQ) //.
			(surfaceRules[n]/.sr0->False/.fq->FreeQ/.usq->UnsameQ) /. idd -> Identity /. nm -> Times /. surdum -> 0/.
			qf -> QuantumField /.isod -> IsoDot /. fdr -> FieldDerivative /. utr -> UTrace/. FieldDerivative[_,LorentzIndex[_]]->0 //
			UTraceTrick
	];

(* Return to defaults *)
SetOptions[MM, Sequence@@tmpoptsmm];
SetOptions[SMM, Sequence@@tmpoptssmm];
SetOptions[CovariantFieldDerivative, Sequence@@tmpoptscdr];


(* (7.14) and (7.20) from Gasser and Leutwyler (1985) *)

gammaRule =
	FieldDerivative[UGamma[LorentzIndex[li1_],opts___Rule][x_], x_, LorentzIndex[li2_]] /; Sort[{li1,li2}] =!= {li1,li2} :>
		FieldDerivative[UGamma[LorentzIndex[li2],opts][x], x, LorentzIndex[li1]] +
		UCommutator[UGamma[LorentzIndex[li1],opts][x], UGamma[LorentzIndex[li2],opts][x]] -
		1/4 UCommutator[USmall[LorentzIndex[li1],Sequence@@OptionsSelect[USmall,opts]][x],
		USmall[LorentzIndex[li2],Sequence@@OptionsSelect[USmall,opts]][x]] -
		1/2 I NM[Adjoint[SMM[x,Sequence@@OptionsSelect[SMM,opts]]], FieldStrengthTensorFull[{li1},
		UGeneratorMatrixIsoDotFull[QuantumField[Particle[LeftComponent[0,Sequence@@OptionsSelect[RightComponent,opts]]],
		{li2}][x]], x, I,Sequence@@OptionsSelect[FieldStrengthTensorFull,opts]], SMM[x,Sequence@@OptionsSelect[SMM,opts]]] -
		1/2 I NM[SMM[x,Sequence@@OptionsSelect[SMM,opts]], FieldStrengthTensorFull[{li1},
		UGeneratorMatrixIsoDotFull[QuantumField[Particle[RightComponent[0,Sequence@@OptionsSelect[LeftComponent,opts]]],
		{li2}][x]], x, I,Sequence@@OptionsSelect[FieldStrengthTensorFull,opts]],
		Adjoint[SMM[x,Sequence@@OptionsSelect[SMM,opts]]]];


UGammaTrick[exp_] :=
	exp /. gammaRule;


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Reduction using equations of motion *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

$EOMRules :=
	{
	UTrace1[NM[Adjoint[MM[x_]], CovariantFieldDerivative[MM[x_], x_, LorentzIndex[mu_]],
	Adjoint[UMatrix[UChi[chopts___]][x_]], CovariantFieldDerivative[MM[x_], x_, LorentzIndex[mu_]]]] ->
		-UTrace1[NM[MM[x], Adjoint[CovariantFieldDerivative[MM[x], x, LorentzIndex[mu]]], UMatrix[UChi[chopts]][x],
		Adjoint[CovariantFieldDerivative[MM[x], x, LorentzIndex[mu]]]]]- Det[Adjoint[UMatrix[UChi[chopts]][x]]]/2 -
		Det[UMatrix[UChi[chopts]][x]]/2 - UTrace1[NM[Adjoint[CovariantFieldDerivative[MM[x], x, LorentzIndex[mu]]],
		CovariantFieldDerivative[UMatrix[UChi[chopts]][x], x, LorentzIndex[mu]]]] -
		UTrace1[NM[Adjoint[CovariantFieldDerivative[UMatrix[UChi[chopts]][x], x, LorentzIndex[mu]]],
		CovariantFieldDerivative[MM[x], x, LorentzIndex[mu]]]] + UTrace1[NM[Adjoint[MM[x]], UMatrix[UChi[chopts]][x]]]^2/4 +
		(UTrace1[NM[Adjoint[MM[x]], UMatrix[UChi[chopts]][x]]] UTrace1[NM[Adjoint[UMatrix[UChi[chopts]][x]], MM[x]]])/2 +
		UTrace1[NM[Adjoint[UMatrix[UChi[chopts]][x]], MM[x]]]^2/4 - UTrace1[NM[Adjoint[UMatrix[UChi[chopts]][x]], UMatrix[UChi[chopts]][x]]],

	UTrace1[NM[Adjoint[CovariantFieldDerivative[MM[x_], x_, LorentzIndex[mu_]]], CovariantFieldDerivative[MM[x_], x_, LorentzIndex[mu_]],
	Adjoint[UMatrix[UChi[chopts___]][x_]], MM[x_]]] ->
		-(-UTrace1[NM[MM[x], Adjoint[CovariantFieldDerivative[MM[x], x, LorentzIndex[mu]]], UMatrix[UChi[chopts]][x],
		Adjoint[CovariantFieldDerivative[MM[x], x, LorentzIndex[mu]]]]]-
		Det[Adjoint[UMatrix[UChi[chopts]][x]]]/2 - Det[UMatrix[UChi[chopts]][x]]/2 -
		UTrace1[NM[Adjoint[CovariantFieldDerivative[MM[x], x, LorentzIndex[mu]]], CovariantFieldDerivative[UMatrix[UChi[chopts]][x], x, LorentzIndex[mu]]]] -
		UTrace1[NM[Adjoint[CovariantFieldDerivative[UMatrix[UChi[chopts]][x], x, LorentzIndex[mu]]], CovariantFieldDerivative[MM[x], x, LorentzIndex[mu]]]] +
		UTrace1[NM[Adjoint[MM[x]], UMatrix[UChi[chopts]][x]]]^2/4 + (UTrace1[NM[Adjoint[MM[x]], UMatrix[UChi[chopts]][x]]] UTrace1[NM[Adjoint[UMatrix[UChi[chopts]][x]], MM[x]]])/2 +
		UTrace1[NM[Adjoint[UMatrix[UChi[chopts]][x]], MM[x]]]^2/4 - UTrace1[NM[Adjoint[UMatrix[UChi[chopts]][x]], UMatrix[UChi[chopts]][x]]])
	};


EOMTrick[expr_] :=
	expr /. Join[$EOMRules, (Expand[a_ * #[[1]]] -> a * #[[2]])& /@ $EOMRules];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Expansion of the fields around the solution to the equations of motion *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(* Auxiliary functions*)

xi[x_] :=
	IsoDot[IsoVector[QuantumField[Particle[PseudoScalar[12]]]][x], IsoVector[UMatrix[UGenerator[]]]];

(*See Ecker 1992, CERN-TH-6660/92*)

uExpRight[x_,a___RenormalizationState,b___RenormalizationScheme, c___ExpansionState,opts___Rule] :=
	NM[SMM[x,Sequence@@OptionsSelect[SMM,opts]],
		UFieldMatrix[
			DecayConstant[UPerturbation,a,b,c]/DecayConstant[pm,a,b,c]/Sqrt[2],
			QuantumField[Particle[UPerturbation,a,b,c]][x],
			Sequence@@OptionsSelect[UFieldMatrix,opts]
		]
	];

uExpLeftAdj[x_,a___RenormalizationState,b___RenormalizationScheme, c___ExpansionState,opts___Rule] :=
	NM[UFieldMatrix[
		DecayConstant[UPerturbation,a,b,c]/DecayConstant[pm,a,b,c]/ Sqrt[2],
		QuantumField[Particle[UPerturbation,a,b,c]][x], Sequence@@OptionsSelect[UFieldMatrix,opts]
		], SMM[x,Sequence@@OptionsSelect[SMM,opts]]
	];

(* The u_mu field *)

(*Keep things compact*)
SetOptions[CovariantNabla, Explicit -> False];

UCoefficient[USmall][0][li_, x_] =
	USmall[li][x];

UCoefficient[USmall][1][li_, x_] =
	-Sqrt[2]/DecayConstant[pm] CovariantNabla[xi[x], x, {li}];

UCoefficient[USmall][2][li_, x_] =
	1/4/DecayConstant[pm]^2 UCommutator[xi[x], UCommutator[USmall[li][x], xi[x]]];

UCoefficient[USmall][do_?((# > 2) &)][li_, x_] :=
	UCoefficient[USmall][do][li, x] =
		(
		Message[UPerturb::"nocoeff", do];
		DiscardTerms[I*NM[Adjoint[uExpRight[x, ExpansionOrder -> do]],
			CDr[NM[uExpRight[x, ExpansionOrder -> do],
			uExpLeftAdj[x, ExpansionOrder -> do]], x, {li}, Explicit -> True],
			Adjoint[uExpLeftAdj[x, ExpansionOrder -> do]]] // NMExpand // Expand,
			Retain -> {Particle[UPerturbation] -> do}]
		);

(*Return to defaults*)
SetOptions[CovariantNabla, Sequence@@tmpoptscn];


(* The chi_+ field*)

UCoefficient[UChiPlus][0][x_] =
	UChiPlus[x];
UCoefficient[UChiPlus][1][x_] =
	-I/Sqrt[2]/DecayConstant[pm]UAntiCommutator[xi[x], UChiMinus[x]];

UCoefficient[UChiPlus][2][x_] =
		-1/4/DecayConstant[pm]^2UAntiCommutator[xi[x], UAntiCommutator[xi[x], UChiPlus[x]]];

UCoefficient[UChiPlus][do_?((# > 2) &)][x_] :=
	UCoefficient[UChiPlus][do][x] =
	(
	Message[UPerturb::"nocoeff", do];
	DiscardTerms[
		NM[Adjoint[uExpRight[x, ExpansionOrder -> do]],
		UMatrix[UChi[]][x], Adjoint[uExpLeftAdj[x, ExpansionOrder -> do]]] +
		NM[uExpLeftAdj[x, ExpansionOrder -> do], Adjoint[UMatrix[UChi[]][x]],
		uExpRight[x, ExpansionOrder -> do]] // NMExpand // Expand,
		Retain -> {Particle[UPerturbation] -> do}
	]);

(* The chi_- field*)

UCoefficient[UChiMinus][0][x_] =
	UChiMinus[x];

UCoefficient[UChiMinus][1][x_] =
	-I/Sqrt[2]/DecayConstant[pm]UAntiCommutator[xi[x], UChiPlus[x]];

UCoefficient[UChiMinus][2][x_] =
	-1/4/DecayConstant[pm]^2 UAntiCommutator[xi[x], UAntiCommutator[xi[x], UChiMinus[x]]];

UCoefficient[UChiMinus][do_?((# > 2) &)][x_] :=
	UCoefficient[UChiMinus][do][x] = (
	Message[UPerturb::"nocoeff", do];
	DiscardTerms[
		NM[Adjoint[uExpRight[x, ExpansionOrder -> do]], UMatrix[UChi[]][x],
		Adjoint[uExpLeftAdj[x, ExpansionOrder -> do]]] - NM[uExpLeftAdj[x, ExpansionOrder -> do],
		Adjoint[UMatrix[UChi[]][x]], uExpRight[x, ExpansionOrder -> do]] // NMExpand // Expand,
		Retain -> {Particle[UPerturbation] -> do}
	]);

(* The f_+ field*)

UCoefficient[UFPlus][0][li1_, li2_, x_] =
	UFPlus[li1, li2][x];

UCoefficient[UFPlus][1][li1_, li2_, x_] =
	I/Sqrt[2]/DecayConstant[pm]UCommutator[xi[x], UFMinus[li1, li2][x]];

UCoefficient[UFPlus][2][li1_, li2_, x_] =
	-1/4/DecayConstant[pm]^2 UCommutator[xi[x], UCommutator[xi[x], UFPlus[li1, li2][x]]];

UCoefficient[UFPlus][do_?((# > 2) &)][li1_, li2_, x_] :=
	UCoefficient[UChiPlus][do][li1, li2, x] = (
	Message[UPerturb::"nocoeff", do];
	DiscardTerms[
		NM[uExpLeftAdj[x, ExpansionOrder -> do],
		FieldStrengthTensorFull[{li1},
		UGeneratorMatrixIsoDot[QuantumField[Particle[LeftComponent[0]], {li2}][x]], x, -I],
		Adjoint[uExpLeftAdj[x, ExpansionOrder -> do]]] + NM[Adjoint[uExpRight[x, ExpansionOrder -> do]],
		FieldStrengthTensorFull[{li1}, UGeneratorMatrixIsoDot[QuantumField[Particle[RightComponent[0]], {li2}][x]], x, -I],
		uExpRight[x, ExpansionOrder -> do]] // NMExpand // Expand, Retain -> {Particle[UPerturbation] -> do}
	]);

(* The f_- field*)

UCoefficient[UFMinus][0][li1_, li2_, x_] =
	UFMinus[li1, li2][x];

UCoefficient[UFMinus][1][li1_, li2_, x_] =
	I/Sqrt[2]/DecayConstant[pm]UCommutator[xi[x], UFPlus[li1, li2][x]];

UCoefficient[UFMinus][2][li1_, li2_, x_] =
	-1/4/DecayConstant[pm]^2UCommutator[xi[x], UCommutator[xi[x], UFMinus[li1, li2][x]]];

UCoefficient[UFPlus][do_?((# > 2) &)][li1_, li2_, x_] :=
	UCoefficient[UFMinus][do][li1, li2, x] =
	(
	Message[UPerturb::"nocoeff", do];
	DiscardTerms[
		NM[uExpLeftAdj[x, ExpansionOrder -> do],
		FieldStrengthTensorFull[{li1},
		UGeneratorMatrixIsoDot[QuantumField[Particle[LeftComponent[0]], {li2}][x]], x, -I],
		Adjoint[uExpLeftAdj[x, ExpansionOrder -> do]]] - NM[Adjoint[uExpRight[x, ExpansionOrder -> do]],
		FieldStrengthTensorFull[{li1}, UGeneratorMatrixIsoDot[QuantumField[Particle[RightComponent[0]], {li2}][x]], x, -I],
		uExpRight[x, ExpansionOrder -> do]] // NMExpand // Expand, Retain -> {Particle[UPerturbation] -> do}
	]
	);

(* The U-field*)

UCoefficient[MM][do_][x_] :=
	UCoefficient[MM][do][x] =
	(
	DiscardTerms[
		NM[uExpRight[x, ExpansionOrder -> do],
		uExpLeftAdj[x, ExpansionOrder -> do]]//NMExpand // Expand,
		Retain -> {Particle[UPerturbation] -> do}
	]
	);

UPerturb[exp_, opts___Rule] :=
	Block[ {or, lim, quants, ruls, subs, a, b, i, summ, UCoeff},
		or = ExpansionOrder /. {opts} /. Options[UPerturb];
		lim = Which[
				NumericQ[or],
					{i, 0, or},
				Head[or] === List && (Length[or] === 1 || Length[or] === 2) && (And @@ (NumericQ /@ or)),
					{i,	Sequence @@ or},
				True,
				Message[UPerturb::"badlim", or];
				Return[]
		];
		quants = UFields /. {opts} /. Options[UPerturb];
		subs = (#[a__][b__] :> #[a, b]) & /@ quants;
		ruls = ((#[a__] -> ((summ[UCoeff[#][i][a], lim])) & /@ quants) /. summ -> Sum);
		exp /. subs /. ruls /. UCoeff -> UCoefficient /. pm ->
			If[ (SUNN /. {opts} /. Options[UPerturb]) === 2,
				Pion,
				PhiMeson
			]
	];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Conversion to LaTeX *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

removeBlankSpace[x_String] :=
	FixedPoint[StringReplace[#, "  " -> " "] &, x];

(*First sketch of a LaTeX output functions; - should be abstracted*)
PhiToLaTeX[x_] :=
	StringReplace[
		ToString[
			x /. _RenormalizationState -> Sequence[] /.
				SUNIndex | ExplicitSUNIndex -> Identity /.
				(SU2Delta|SU3Delta|SUNDelta)[a_, b_] :> "\\delta_{" <> ToString[a] <> ToString[b] <> "}" /.
				Pair[Momentum[p2], Momentum[p2]]^i_ :> "q^" <> ToString[2i] //.
				(*Phi`Renormalization` is not in $ContextPath *)
				{
					Phi`Renormalization`LeutwylerJBar[a__, __Rule] :> Phi`Renormalization`LeutwylerJBar[a],
					QuarkCondensate[___] -> "B_0",
					Pi -> "\\pi",
					Log -> "\\log",
					Pair[_LorentzIndex, ___] -> Sequence[],
					_DecayConstant -> "f",
					CouplingConstant[Phi`Objects`ChPTW3[2], 1] -> "c_2",
					CouplingConstant[Phi`Objects`ChPTW3[2], 2] -> "c_5",
					CouplingConstant[Phi`Objects`ChPT2[4], i_, ___] :> "L_{" <> ToString[i] <> "}",
					CouplingConstant[Phi`Objects`ChPT3[4], i_, ___] :> "L_{" <> ToString[i] <> "}",
					CouplingConstant[Phi`Objects`ChPTW3[4], i_, ___] :> "n_{" <> ToString[i] <> "}",
					MandelstamT -> "t",
					MandelstamS -> "s",
					MandelstamU -> "u",
					ParticleMass[Pion] -> "m_{\\rm \\pi}",
					ParticleMass[Kaon] -> "m_{\\rm K}",
					ParticleMass[EtaMeson] -> "m_{\\rm \\eta}",
					ScaleMu -> "\\mu",
					Pair[Momentum[p2], Momentum[p2]] -> "q^2"
				},

				FormatType -> InputForm, PageWidth -> 120], {
				"\"" -> "",
				"I" -> "i",
				"[" -> "(",
				"]" -> ")",
				"*" -> " ",
				"\n" -> "",
				"LeutwylerJBar" -> "\\overline{J}"}
	] // removeBlankSpace // StandardForm // InputForm;

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Miscellaneous *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(* The Gell-mann Okubo mass formula *)

$GellmannOkubo = {ParticleMass[EtaMeson, r___]^n_ -> ((-ParticleMass[PionZero, r]^2 + 4ParticleMass[KaonZero, r]^2)/3)^(n/2)};

GellmannOkubo[exp_] :=
	Block[ {l, s, pm},
		exp /.
		(l : (Phi`Renormalization`LeutwylerJBar | Log))[s__] :> (l[s] /. ParticleMass -> pm) /. $GellmannOkubo /. pm -> ParticleMass
	];

$GellmannOkuboInv = {
	ParticleMass[PseudoScalar[2], r___]^2 - 4 ParticleMass[PseudoScalar[6], r___]^2 :> -3*ParticleMass[PseudoScalar[11],r]^2,
	-ParticleMass[PseudoScalar[2], r___]^2 + 4 ParticleMass[PseudoScalar[6], r___]^2 :> 3*ParticleMass[PseudoScalar[11], r]^2,
	4/3 - ParticleMass[Pion]^2/(3 ParticleMass[Kaon]^2) :> ParticleMass[EtaMeson]^2/(ParticleMass[Kaon]^2)
};


(* Substitute DiracBars with Adjoints, permute and substitute back *)
(*First implementation - should be improved...*)
FixFermionAdjoints[ex_] :=
	(
	(ex /.
	{	QuantumField[d___, DiracBar[Particle[Fermion[f_], r___]], rr___][x_] :>
			DOT[Adjoint[QuantumField[d, Particle[Fermion[f], r], rr][x]], DiracGamma[ExplicitLorentzIndex[0]]], QuantumField[d___,
			Adjoint[DiracBar[Particle[Fermion[f_], r___]]], rr___][x_] :>
		DOT[DiracGamma[ExplicitLorentzIndex[0]], QuantumField[d, Particle[Fermion[f], r], rr][x]]
	} // DiracSimplify) /.
		DOT[Adjoint[DiracGamma[LorentzIndex[mu_]]], DiracGamma[ExplicitLorentzIndex[0]]] :>
			DOT[DiracGamma[ExplicitLorentzIndex[0]], DiracGamma[LorentzIndex[mu]]] // DiracSimplify
	) /. DOT[Adjoint[QuantumField[d___, Particle[Fermion[f_], r___], rr___][x_]], DiracGamma[ExplicitLorentzIndex[0]]] :>
	QuantumField[d, DiracBar[Particle[Fermion[f], r]], rr][x] // DiracSimplify;

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
FCPrint[1,"PHI: Utilities.m loaded"];
End[];
