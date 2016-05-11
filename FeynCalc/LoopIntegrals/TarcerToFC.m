(* ::Package:: *)



(* :Title: TarcerToFC                                                       *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:	Conversion of TARCER integrals to FeynCalc ones				*)

(* ------------------------------------------------------------------------ *)

TarcerToFC::usage =
"TarcerToFC[expr, q1, q2] translates loop integrals \
in Tarcer-notation to the FeynCalc notation. \
See TFI for details on the convention. \
As in case of ToTFI, the 1/Pi^D and 1/Pi^D/2 prefactors are implicit, i.e. \
TarcerToFC doesn't add them.
";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`TarcerToFC`Private`"]

(* Care about TVi and TJi later *)

Options[TarcerToFC]  = {};

momconv[0]:=
	0;

momconv[p_^2]:=
	MemSet[momconv[p^2],p];

momconv[Pair[Momentum[p_,d_:4],Momentum[p_,d_:4]]]:=
	MemSet[momconv[Pair[Momentum[p,d],Momentum[p,d]]],p];

momconv[SP[p_,p_]]:=
	MemSet[momconv[SP[p,p]],p]

momconv[SPD[p_,p_]]:=
	MemSet[momconv[SPD[p,p]],p]

momconv[pp_Symbol]:=
	MemSet[momconv[pp],Sqrt[pp]];


TarcerToFC[exp_, {q1_, q2_}, OptionsPattern[]] :=
	Block[ {out, rules},
		rules = {
			Tarcer`TFI[d_Symbol, pp_, {u_,v_,r_,s_,t_}, {{nu1_,m1_},{nu2_,m2_},{nu3_,m3_},{nu4_,m4_},{nu5_,m5_}}] :>
				Pair[Momentum[q1,d],Momentum[q1,d]]^u*
				Pair[Momentum[q2,d],Momentum[q2,d]]^v*
				Pair[Momentum[momconv[pp],d],Momentum[q1,d]]^r*
				Pair[Momentum[momconv[pp],d],Momentum[q2,d]]^s*
				Pair[Momentum[q1,d],Momentum[q2,d]]^t*
				FCI[FAD[{q1,m1,nu1},{q2,m2,nu2},{q1-momconv[pp],m3,nu3},{q2-momconv[pp],m4,nu4},{q1-q2,m5,nu5},Dimension->d]],

			Tarcer`TFI[d_Symbol, pp_, {{nu1_,m1_},{nu2_,m2_},{nu3_,m3_},{nu4_,m4_},{nu5_,m5_}}] :>
				FCI[FAD[{q1,m1,nu1},{q2,m2,nu2},{q1-momconv[pp],m3,nu3},{q2-momconv[pp],m4,nu4},{q1-q2,m5,nu5},Dimension->d]],

			Tarcer`TFI[d_Symbol, pp_, {nu1_,nu2_,nu3_,nu4_,nu5_}] :>
				FCI[FAD[{q1,0,nu1},{q2,0,nu2},{q1-momconv[pp],0,nu3},{q2-momconv[pp],0,nu4},{q1-q2,0,nu5},Dimension->d]],

			Tarcer`TVI[d_Symbol, pp_, {{nu1_,m1_},{nu2_,m2_},{nu3_,m3_},{nu4_,m4_}}] :>
				FCI[FAD[{q1-q2,m1,nu1},{q2,m2,nu2},{q1-momconv[pp],m3,nu3},{q2-momconv[pp],m4,nu4},Dimension->d]],

			Tarcer`TJI[d_Symbol, pp_, {{nu1_,m1_},{nu2_,m2_},{nu3_,m3_}}] :>
				FCI[FAD[{q1,m1,nu1},{q1-q2,m2,nu2},{q2-momconv[pp],m3,nu3},Dimension->d]],

			Tarcer`TBI[d_Symbol, pp_, {{nu1_,m1_},{nu2_,m2_}}] :>
				FCI[FAD[{q1,m1,nu1},{q1-momconv[pp],m2,nu2},Dimension->d]],

			Tarcer`TAI[d_Symbol, 0, {{nu1_,m1_}}] :>
				FCI[FAD[{q1,m1,nu1},Dimension->d]]


		};

		out = exp /. Dispatch[rules];
		out
	];

FCPrint[1,"TarcerToFC.m loaded."];
End[]
