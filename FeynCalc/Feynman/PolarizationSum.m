(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PolarizationSum                                                   *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Polarization sums for massive and massless vector
		bosons, e.g. photons, W's, Z's and gluons. The formulas
		used here can be found e.g. in M. Boehm, A. Denner and H. Joos,
		Gauge Theories of the Strong and Electroweak Interaction,
		Eq. A.1.45 and Eq. A.1.46.	*)

(* ------------------------------------------------------------------------ *)


PolarizationSum::usage =
"PolarizationSum[\[Mu], \[Nu], ... ] represents the sum over a polarization
vector and its complex conjugate with two free indices. Depending on its
arguments the function returns different polarization sums for massive or
massless vector bosons.

- PolarizationSum[\[Mu], \[Nu], k] returns $-g^{\\mu \\nu}+\\frac{k^{\\mu}
k^{\\nu}}{k^2}$, i.e. the sum over the 3 physical polarizations of a massive
on-shell vector boson with $m = k^2$.
- PolarizationSum[\[Mu], \[Nu]] or PolarizationSum[\[Mu], \[Nu], k, 0] gives
$-g^{mu nu }$. This corresponds to the summation over all $4$ polarizations of
a massless vector boson, $2$ of which are unphysical if the particle is
on-shell.
 - PolarizationSum[\[Mu], \[Nu], k, n] yields $-g^{\\mu \\nu}+\\frac{k^{\\mu
}n^{\\nu}+k^{\\nu }n^{\\mu }}{k \\cdot n} - \\frac{n^2 k^{\\mu}k^{\\nu}}{(k \\cdot
n)^2}$ which is the so-called axial-gauge polarization sum that picks up only
the two physical polarizations of a massless vector boson. Here $n$ is an
auxiliary vector that must satisfy $n \\cdot k \\neq 0$. The physical results
will not depend on $n$, yet in practice it is often convenient to
identify $n$ with one of the 4-vectors already present in the calculation. For
example, in a final state with multiple gluons denoted by their momenta $k_i$,
the vector $n$ for the $i$-th gluon could be a $k_j$ with $j \\neq i$. Notice
that when using this polarization sum in a QCD calculation, one doesn't have
to consider diagrams with ghosts in the final states.

To obtain a $D$-dimensional polarization sum use the option Dimension.

If you need to calculate a polarization sum depending on a 4-momentum that is
not on-shell, use the option VirtualBoson.";

PolarizationSum::notmassless=
"Warning! You are inserting a polarization sum for massless vector bosons, \
but the momentum of the external boson `1` is not on-shell. Please put it on-shell \
via ScalarProduct[`1`,`1`]=0";

PolarizationSum::notmassive=
"Warning! You are inserting a polarization sum for massive vector bosons, \
but the momentum of the external boson `1` corresponds to a massless particle. \
Please define the proper mass via ScalarProduct[`1`,`1`]=mass^2. Otherwise, the result \
is not well defined.";

PolarizationSum::auxerror=
"Warning! You are inserting a polarization sum for massive vector bosons, using \
the auxiliary vector `1`. However, the scalar product between the momentum of the \
external boson `2` and `1` is zero, which shouldn't be the case.";

PolarizationSum::failmsg =
"Error! PolarizationSum has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`PolarizationSum`Private`"]

Options[PolarizationSum] = {
	Dimension -> 4,
	VirtualBoson -> False,
	Heads -> {LorentzIndex, LorentzIndex}
};

indSelect[type_, name_, dim_]:=
	Switch[type,
		LorentzIndex,
		LorentzIndex[name,dim],
		CartesianIndex,
		CartesianIndex[name,dim-1],
		ExplicitLorentzIndex[0],
		ExplicitLorentzIndex[0],
		_,
		Message[PolarizationSum::failmsg,"Unknown head."];
		Abort[]
	];


(*    Polarization sum for massless vector bosons with gauge terms omitted,
	e.g. for photons in QED.     *)
PolarizationSum[mu_,nu_, OptionsPattern[]] :=
	-Pair[indSelect[OptionValue[Heads][[1]], mu , OptionValue[Dimension]],
		indSelect[OptionValue[Heads][[2]], nu , OptionValue[Dimension]]];

(*	Same as above. Putting the auxiliary vector n^mu to zero essentially
	omits the gauge terms.  *)
PolarizationSum[mu_,nu_, k_, 0, OptionsPattern[]] :=
	Block[{dim = OptionValue[Dimension]},

	If[	Pair[Momentum[k,dim],Momentum[k,dim]]=!=0 && !OptionValue[VirtualBoson],
		Message[PolarizationSum::notmassless, k]
	];

	-Pair[indSelect[OptionValue[Heads][[1]], mu , OptionValue[Dimension]],
		indSelect[OptionValue[Heads][[1]], nu , OptionValue[Dimension]]]

	]/; k=!=0;

(*     Polarization sum for massive vector bosons, e.g. W's and Z's in the
	Electroweak Theory. Note that the particle mass enters as k^2, where k
	is the four-momentum of the boson.    *)
PolarizationSum[mu_,nu_, k_, OptionsPattern[]] :=
	Block[ {dim = OptionValue[Dimension],ind1, ind2},

		If[	Pair[Momentum[k,dim],Momentum[k,dim]]===0  && !OptionValue[VirtualBoson],
			Message[PolarizationSum::notmassive, k]
		];

		ind1 = indSelect[OptionValue[Heads][[1]], mu , dim];
		ind2 = indSelect[OptionValue[Heads][[2]], nu , dim];

		-Pair[ind1,ind2] +
		(Pair[Momentum[k,dim],ind1] Pair[Momentum[k,dim],ind2])/
		Factor2[ExpandScalarProduct[Pair[Momentum[k,dim],Momentum[k,dim]]],FCI->True]
	]/; k=!=0;

(*    Polarization sum for massless vector bosons with gauge terms included,
	e.g. for gluons in QCD. The auxiliary four-vector n^mu must satisfy
	n^mu eps_mu = 0 and n^mu k_mu != 0. Note that FeynCalc doesn't enforce
	these two conditions by itself, i.e. the user must ensure that they are
	satisfied.    *)
PolarizationSum[mu_,nu_, k_, n_, OptionsPattern[]] :=
	Block[ {dim = OptionValue[Dimension],ind1,ind2},
		If[	Pair[Momentum[k,dim],Momentum[k,dim]]=!=0 && !OptionValue[VirtualBoson],
			Message[PolarizationSum::notmassless, k]
		];
		If[	Pair[Momentum[n,dim],Momentum[k,dim]]===0,
			Message[PolarizationSum::auxerror,n, k]
		];
		ind1 = indSelect[OptionValue[Heads][[1]], mu , dim];
		ind2 = indSelect[OptionValue[Heads][[2]], nu , dim];


		(-Pair[ind1,ind2] -
		(Pair[Momentum[k,dim],ind1] Pair[Momentum[k,dim],ind2] *
		Factor2[ExpandScalarProduct[Pair[Momentum[n,dim],Momentum[n,dim]],FCI->True]])/
		Factor2[ExpandScalarProduct[Pair[Momentum[k,dim],Momentum[n,dim]],FCI->True]^2] +
		(Pair[Momentum[n,dim],ind1] Pair[Momentum[k,dim],ind2] +
		Pair[Momentum[k,dim],ind1] Pair[Momentum[n,dim],ind2])/
		Factor2[ExpandScalarProduct[Pair[Momentum[k,dim],Momentum[n,dim]],FCI->True]])//Collect2[#,Pair]&
	]/; k=!=0 && n=!=0;

FCPrint[1,"PolarizationSum.m loaded."];
End[]
