(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Uncontract														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Uncontracts Lorentz tensors									*)

(* ------------------------------------------------------------------------ *)

Uncontract::usage = "Uncontract[exp,q1,q2, ...] uncontracts Eps \
and DiracGamma. Uncontract[exp,q1,q2, Pair->{p}] uncontracts \
also p.q1 and p.q2; Pair -> All uncontracts all except \
OPEDelta. Dimension -> Automatic leaves dimensions unchanged.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Uncontract`Private`"]

Options[Uncontract] = {
	Dimension -> Automatic,
	DimensionalReduction -> False,
	Pair -> {},
	Unique   -> True
};

Uncontract[x_Plus, q__] :=
	Map[Uncontract[#,q]&, x];

Uncontract[x_List, q__] :=
	Map[Uncontract[#,q]&, x];

Uncontract[x_, q1__, q2:Except[_?OptionQ], opt:OptionsPattern[]] :=
	Uncontract[Uncontract[x,q2,opt],q1, opt];

Uncontract[ex_, q:Except[_?OptionQ], OptionsPattern[]] :=
	Block[{	exp,eeps,nex,li,li1,li2,dim,par,dummy,inc,
			a$AL,dr, lidr,seq,dimSelect,times},

		par = OptionValue[Pair];
		dim = OptionValue[Dimension];

		lidr[z_] :=
			If[ OptionValue[DimensionalReduction],
				z/.{LorentzIndex[aa_,_]:>LorentzIndex[aa]/. Momentum[bb_,_]  :> Momentum[bb]},
				z
			];
		dimSelect[z_]:=
			If[ dim===Automatic,
				seq[z],
				dim
			];

		exp = FeynCalcInternal[ex];

		exp = Expand2[ExpandScalarProduct[exp,Momentum->{q}],q];

		If[ OptionValue[Unique] === True,
			a$AL = Unique[$AL],
			a$AL = $AL
		];

		If[ par===All,
			par = Map[First, SelectFree[Cases2[exp, Momentum], OPEDelta]]
		];

		If[ (par === {} && FreeQ2[exp, {Eps, DiracGamma}]) || (Head[dummy exp] =!= Times),
			exp,

			nex = exp;
			If[ FreeQ[nex, a$AL],
				inc = 0,
				inc = (Max @@ Map[First, Cases2[nex, a$AL]]);
			];

			If[ !FreeQ[nex,Eps],
				nex = nex /. Eps -> eeps;
				nex = nex /. eeps[aa__]^n_Integer?Positive  :>
					Apply[times, Table[eeps[aa], {j,Abs[n]}]]^Sign[n];

				nex = nex //. {eeps[a___,Momentum[(c: q | Polarization[q,__]),d_:4],b___] :>
					(li = LorentzIndex[a$AL[inc = inc+1], dimSelect[d]];
					Pair[Momentum[c, dimSelect[d]], li] eeps[a,lidr[li],b])} /. eeps -> Eps /. times -> Times;
			];
			If[ !FreeQ[nex, DiracGamma],
				(* The momentum that we want to uncontract might be inside a slash like GS[a+b+c]!	*)
				nex = nex /. DiracGamma[xxx_, dd_:4]/;!FreeQ2[xxx,{q}] :> DiracGammaExpand[DiracGamma[xxx,dd]];
				nex = nex /. DiracGamma -> dirg;

				nex = nex //. dirg[Momentum[(c: q | Polarization[q,__]),d_:4],___] :>
					(li = LorentzIndex[a$AL[inc = inc+1],dimSelect[d]];
					Pair[Momentum[c,dimSelect[d]], li] dirg[lidr[li],dimSelect[d]]) /. dirg -> DiracGamma;

				nex = DotSimplify[nex,Expanding -> False];
			];
			If[ par=!={} && Length[par]>0 && Head[par]===List,

				(*Uncontract denominators also. Change by F.Orellana. 3/11-2002*)
				(*Reverted, RM 06/22-2011 *)

				nex = nex /. tf_[aa__/;!FreeQ2[{aa}, par]]^n_Integer?Positive/; (MemberQ[$FCTensorList,tf] && tf=!=Eps)  :>
					Apply[times, Table[tf[aa], {j,Abs[n]}]]^Sign[n];

				If[ MemberQ[par, q],
					nex = nex //. Pair[Momentum[(c1: q | Polarization[q,__]),d_:4], Momentum[(c2: q | Polarization[q,__]),d_:4]] :>
						(li1 = LorentzIndex[a$AL[inc = inc+1], dimSelect[d]];
						li2 = LorentzIndex[a$AL[inc = inc+1], dimSelect[d]];
						Pair[Momentum[c1, dimSelect[d]], li1] Pair[Momentum[c2, dimSelect[d]], li2] Pair[li1, li2]);
					par = SelectFree[par, q];
				];
				nex = nex //.{Pair[Momentum[(c: q | Polarization[q,__]),d_:4], Momentum[pe_,___]] :>
						(li = LorentzIndex[a$AL[inc = inc+1],dimSelect[d]];
						Pair[Momentum[c,dimSelect[d]], li] Pair[Momentum[pe,dimSelect[d]],lidr[li]])/;MemberQ[par,pe]} ;

				(* Uncontract tensor functions *)
				If[ !FreeQ[nex, (tf_/;MemberQ[$FCTensorList,tf] && tf=!=Pair  && tf=!=Eps)[___,Momentum[(q | Polarization[q,__]),___],___]],
					nex = nex //. { (tf_/; MemberQ[$FCTensorList,tf] && tf=!=Pair  && tf=!=Eps)[a___,Momentum[(c: q | Polarization[q,__]),d_:4],b___] :>
						(li = LorentzIndex[a$AL[inc = inc+1],dimSelect[d]];
						tf[a, li, b] Pair[Momentum[c,dimSelect[d]],lidr[li]])}
				];

			];
			nex/. times -> Times/.dummy->1/.seq:>Sequence
		]
	];

FCPrint[1,"Uncontract.m loaded."];
End[]
