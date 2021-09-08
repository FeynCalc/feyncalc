(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Factor2 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 28 January '99 at 23:32 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Factor2 *)

(* ------------------------------------------------------------------------ *)

Factor1::usage=
"Factor1[poly] factorizes common terms  in the summands of poly. It uses
basically PolynomialGCD.";

Factor2::usage =
"Factor2[poly] factors a polynomial in a standard way.

Factor2 works sometimes better than Factor on polynomials involving rationals
with sums in the denominator.

Factor2 uses Factor internally and is in general slower than Factor. There are
four possible settings of the option Method (0,1,2,3). In general, Factor will
work faster than Factor2.";

FactorFull::usage=
"FactorFull is an option of Factor2 (default False). If set to False, products
like (a-b) (a+b) will be replaced by a^2-b^2.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"];
End[]

Begin["`Factor`Private`"];

Options[Factor2] = {
	FactorFull	-> False,
	Method 		-> 3
};

Factor1[x_] :=
	Block[{factor1t1,factor1t2,factor1t3,mt,mi,m1,mp1,nx=x,iIii},
		mt = (((# /. Plus -> mi /. mi -> Plus) /. m1 -> (-1)/.mp1 -> (-Plus[##]&)) /. iIii -> I)&;
		mi[y_, z__] := (m1 mp1[y,z] )/; (If[ Head[#] === Complex, False,
			If[# < 0, True, False]]& @ NumericalFactor[y]);
		nx = x /. Complex[0, b_] -> (b iIii);
		If[Head[nx] =!= Plus,
			mt[nx /. Plus -> (Factor1[Plus[##]]&)],
			factor1t1 = Apply[ List, Map[# /. Plus -> factor1t3&, nx]];
			factor1t2 = (PolynomialGCD @@ factor1t1) /. factor1t3 -> Plus;
			mt[(factor1t2 Apply[Plus, Map[((# /. factor1t3 -> Plus) / factor1t2)&, factor1t1]])]
		]
	];

fc5[y_] :=
	If[Head[y] === Times,
		Map[fc5, y],
		(*MemSet[fc5[y],*)
		Block[{te},
			If[Head[y] === Power,
				fc5[y[[1]]]^y[[2]],
				(*FCPrint[3,"factoring ", Length[te], " terms in Factor2 " ];*)
				te = Factor[Expand[y]];
				(* te = Factor[y]; *)
				(*FCPrint[3,"factoring in Factor2  done "];*)
				te
			]
		](*]*)
	];

Factor2[x_Integer,___] :=
	x;
Factor2[x_Symbol,___] :=
	x;
Factor2[ix_, r___Rule] :=
	Block[{fc,mt,mi,m1,mp1,cm,ff,pr,pp,h,iI,x,tx,factor55,pl5,me,comb},
		If[FreeQ[ix,Complex],
			x = ix,
			x = ix /. Complex[0,in_] :> iI in
		];
		(*
		If[0 <= (Method /. {r} /. Options[Factor2]) < 4,
			expand = Expand2, expand = Identity];
		*)
		If[(FactorFull /. {r} /. Options[Factor2]) === True,
			fc = mt[fc5[Numerator[#]]/fc5[Denominator[#]]]&,
			fc = mt[pp[fc5[Numerator[#]]]/pp[fc5[Denominator[#]]]]&
		];

		mt = ((# /. Plus -> mi /. mi -> Plus) /. m1 -> (-1) /. mp1 -> (-Plus[##]&))&;
		mi[y_, z__] :=
			(m1 mp1[y,z] )/; (	If[Head[#] === Complex,
									False,
									If[ # < 0,
										True,
										False
									]
								]& @ NumericalFactor[y]);

		pr = { fa_. pc[a_, b_]^n_. pc[a_, c_]^n_. :>
				(fa pc[a^2, -b^2]^n) /; (((b + c) === 0) && IntegerQ[n]),
				fa_. pc[a_, b_]^n_. pc[c_, b_]^n_. :>
				(fa pc[b^2, -a^2]^n) /; (((a + c) === 0) && IntegerQ[n])};

		pp =
			If[(FactorFull/. {r} /. Options[Factor2] ) =!= True,
				(((Numerator[#] /. Plus -> pc) //. pr) /. pc -> Plus /. pc -> Plus)/
				(((Denominator[#] /. Plus -> pc) //. pr) /.	pc -> Plus /. pc -> Plus),#
			]&;

		If[0< (Method /. {r} /. Options[Factor2])<3,
			tx = vsu[x, 42000],
			tx = {x, {}}
		];

		If[(Method /. {r} /. Options[Factor2])=!=1,
			factor55[z_] := z,
			factor55[z_] := If[Head[z] === Plus,
								fc[Map[# /. Plus -> pl5&, z]] /. pl5 -> Plus,
								If[Head[z] === Times,
										Map[factor55, z],
								z]
							];
		];
		If[(Method /. {r} /. Options[Factor2])===0,
			comb[yy_,___] := yy,
			comb = Combine[#,Expanding -> False]&
		];

		If[(Method /. {r} /. Options[Factor2]) > 2,
			fc[comb[x]],
			(fc[comb[factor55[comb[tx[[1]]]]] ] /. tx[[2]])
		]/.iI->I
	];

	(* maybe this is not so good in the end; but it fixed a weird Factor-bug *)
	vsu[y_, les_:50000] := Block[{vv, xX, vs, iv, yr, vb, ly = LeafCount[y]},
		If[ly > les,
			yr = y;
			vb = {},
			vv = Variables[y];
			vs = Table[vv[[iv]] -> ToExpression[StringJoin["xX", iv//ToString]], {iv, Length[vv]}];
			vb = Map[Reverse, vs];
			yr = y /. vs
		];
		{yr, vb}
	];

FCPrint[1,"Factor.m loaded."];
End[]
