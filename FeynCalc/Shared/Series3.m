(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Series3 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  Series3 is like Series, but including Normal and
							bug-fixes
*)

(* ------------------------------------------------------------------------ *)



Series3::usage=
"Series3 performs a series expansion around 0. Series3 is equivalent to Series,
except that it applies Normal on the result and that some Series bugs are
fixed.

Series3[f, e, n] is equivalent to  Series3[f, {e, 0, n}].";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"];
End[]

Begin["`Private`"];

Options[Series3] = {
	Factoring 			-> True,
	FinalSubstitutions	-> {}
};

Series3[a_, e_, n_Integer, ops___Rule] :=
	Series3[a, {e, 0, n}, ops];

Series3[exp_, a_List, b__List] :=
	Series3[Normal[Series3[exp, a]], b];

Series3[a_, {x_, 0, nn_}, ops___Rule] :=
	Block[{re, rec, polysub, polysimp, finsub, res, n=nn,
			hypfix, hyp, hyb, hyphyp, hyback, hypex},

		hypex = {Hypergeometric2F1[p1_,p2_,p3_,z_] :>
			Apply[Hypergeometric2F1, Append[Expand/@{p1,p2,p3},Factor2[z]]]};

		hyphyp[z_,xx_] :=
			If[FreeQ2[z, {HypergeometricPFQ,
				Hypergeometric2F1, HypergeometricPFQRegularized}],
				z,
				If[xx === Epsilon,
					z/.hypex/.hypexpansion/.hypfix,
					z/.hypfix
				]
			];

		hypfix = HypergeometricPFQ[nu_List, de_List, z_] :>
			(hyp[Length[nu],Length[de]] @@ Join[nu,de,{z}]);

		hyback = {hyp[la_, ld_][c__] :>
			HypergeometricPFQ[Take[{c}, la], Take[{c}, {la+1,la+ld}], Last[{c}]],
			Derivative[de__][hyp[la_, ld_]][c__] :> Derivative[Take[{de}, la],
			Take[{de},{la+1, la+ld}], Last[{de}]][HypergeometricPFQ[ Take[{c}, la],
			Take[{c}, {la+1,la+ld}], Last[{c}]]]};



		polysub = {
			PolyGamma[0, (OPEm+h_Integer?OddQ)/2] :>
				PolyGamma[0,OPEm/2+h/2-1/2] - 2 (-1)^OPEm (-1)^(h+1) Log[2] -
				2 (-1)^OPEm (-1)^(h+1) SumT[1, OPEm + h - 2],
			PolyGamma[0, OPEm/2+aa_.] :> (
				PolyGamma[0,OPEm/2+aa-1/2] - 2 (-1)^OPEm (-1)^(2aa+1) Log[2] -
				2 (-1)^OPEm (-1)^(2aa+1) SumT[1, OPEm + 2aa - 2]) /; Head[aa] === Rational
		};

		polysimp =
			If[FreeQ[#, PolyGamma],
				#,
				#/.polysub /. PolyGamma[0, aa_] :> PolyGamma[0, Expand[aa]]
			]&;

		(* Following lines differ from Series2 *)
		finsub  = FinalSubstitutions/. {ops} /. Options[Series3];
		re  = Series[Collect2[hyphyp[a,x], x,Factoring->False], {x, 0, n}
								];
		If[!FreeQ[a,Gamma] || !FreeQ[a, PolyGamma],
			re = re + O[x]^n;
			If[nn>-1,
				re = re + O[x]^(n-1)
			]
		];
		(* ----------------------------------- *)


		re = (re // Normal) /. hyback;
		If[Head[re]===Plus && n<0,
			re = SelectNotFree[re, x]
		];
		res = Collect2[polysimp[re] /. finsub, x, Factoring -> False];
		If[nn === -1,
			res = Collect2[res - Coefficient[res, x, 0], x,Factoring->False];
		];
		(*
		rec = (Series[a, {x, 0, n+1}]//Normal)/.x^(n+1) :> 0;
		If[re === rec,
			res = Collect[Expand[re /. finsub], x],
			Print["goin higher in Series "];
			If[n < 13, res = Series[a, {x,0,n+1}, ops]];
			];
		If[!FreeQ[res,Gamma], res = SimplifyGamma[res]]; *)

		(* Following lines differ from Series2 *)
		If[!FreeQ[res,PolyGamma],
			res = FunctionExpand[res]
		];

		If[Factoring /. {ops} /. Options[Series],
			Collect[res,x,Factor2],
			Collect[res,x]
		];
		res/.finsub
		(* ----------------------------------- *)
	];

F21CHECK :=
	Unique[F21CHECKIT];


(*XXX*)
hypexpansion={
	Hypergeometric2F1[-Epsilon, 1, 1 - Epsilon/2, x_] :>
		1 + Epsilon*Log[1 - x] - (Epsilon^2*
				(2*Zeta2 - Log[1 - x]^2 - 2*Log[1 - x]*Log[x] -
	2*PolyLog[2, 1 - x]))/4 +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[1 - Epsilon, 1 - Epsilon/2, 2 - (3*Epsilon)/2, x_
			] :>
		-(Log[1 - x]/x) + (3*Epsilon*
	(Zeta2 + Log[1 - x] - Log[1 - x]*Log[x] - PolyLog[2, 1 - x]))/(2*x) -
			(Epsilon^2*(9*Zeta2 + 2*Zeta2*Log[1 - x] - 9*Log[1 - x]*Log[x] -
					9*PolyLog[2, 1 - x] + 2*Log[1 - x]*PolyLog[2, 1 - x] -
					4*PolyLog[3, 1 - x] - 9*PolyLog[3, x] + 4*Zeta[3]))/(4*x)+
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[1 + 2*Epsilon, 1 + Epsilon, 2 + 2*Epsilon, x_] :>
	-(Log[1 - x]/x) - (Epsilon*(4*Zeta2 + 4*Log[1 - x] - Log[1 - x]^2 -
					4*Log[1 - x]*Log[x] - 4*PolyLog[2, 1 - x]))/(2*x) -
			(Epsilon^2*(24*Zeta2 - 6*Log[1 - x]^2 + Log[1 - x]^3 -
					24*Log[1 - x]*Log[x] + 6*Log[1 - x]^2*Log[x] -
					24*PolyLog[2, 1 - x] + 12*Log[1 - x]*PolyLog[2, 1 - x] -
	12*PolyLog[3, 1 - x] - 24*PolyLog[3, x] + 12*Zeta[3]))/(6*x) +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[-Epsilon, -Epsilon/2, 1 - (3*Epsilon)/2, x_] :>
		1 + (Epsilon^2*(Zeta2 - Log[1 - x]*Log[x] -
				PolyLog[2, 1 - x]))/2  +  (3*Epsilon^3*PolyLog[3, x])/4 +
	Epsilon^4 F21CHECK
	,
		Hypergeometric2F1[Epsilon/2, Epsilon, 1 + (3*Epsilon)/2, x_] :>
		1 + (Epsilon^2*(Zeta2 - Log[1 - x]*Log[x] -
				PolyLog[2, 1 - x]))/2 - (3*Epsilon^3*PolyLog[3, x])/4 +
	Epsilon^4 F21CHECK
	,
	Hypergeometric2F1[Epsilon, 1 + Epsilon, 1 + 2*Epsilon, x_] :>
		1 - Epsilon*Log[1 - x] - Epsilon^2*
			(Zeta2 - Log[1 - x]*Log[x] - PolyLog[2, 1 - x]) +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[1 + Epsilon, 1 + Epsilon, 2 + 2*Epsilon, x_] :>
		-(Log[1 - x]/x) - (2*Epsilon*
	(Zeta2 + Log[1 - x] - Log[1 - x]*Log[x] - PolyLog[2, 1 - x]))/x -
			(Epsilon^2*(4*Zeta2 + Zeta2*Log[1 - x] - 4*Log[1 - x]*Log[x] -
					4*PolyLog[2, 1 - x] + Log[1 - x]*PolyLog[2, 1 - x] -
					2*PolyLog[3, 1 - x] - 4*PolyLog[3, x] + 2*Zeta[3]))/x +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[1, 1 - Epsilon, 1 - Epsilon/2, x_] :>
		(1 - x)^(-1) + (Epsilon*Log[1 - x])/(2*(1 - x)) -
			(Epsilon^2*(2*Zeta2 - Log[1 - x]^2 - 2*Log[1 - x]*Log[x] -
					2*PolyLog[2, 1 - x]))/(8*(1 - x)) +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[1 - Epsilon/2, -Epsilon, 2, x_] :>
		1 - (Epsilon*(x + Log[1 - x] - x*Log[1 - x]))/x +
			(Epsilon^2*(4*x + 2*x*Zeta2 + 4*Log[1 - x] - 4*x*Log[1 - x] -
					3*Log[1 - x]^2 + 3*x*Log[1 - x]^2 - 2*x*Log[1 - x]*Log[x] -
					2*x*PolyLog[2, 1 - x]))/(4*x) +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[2 - Epsilon/2, -Epsilon, 3, x_] :>
		1 - (Epsilon*(2*x + x^2 + 2*Log[1 - x] - 2*x^2*Log[1 - x]))/(2*x^2) +
	(Epsilon^2*(4*x + 3*x^2 + 2*x^2*Zeta2 + 4*Log[1 - x] - 2*x*Log[1 - x] -
					2*x^2*Log[1 - x] - 3*Log[1 - x]^2 + 3*x^2*Log[1 - x]^2 -
					2*x^2*Log[1 - x]*Log[x] - 2*x^2*PolyLog[2, 1 - x]))/(4*x^2)+
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[-1 + Epsilon, 1 + (3*Epsilon)/2, 2 + 2*Epsilon, x_] :>
		(2 - x)/2 + (Epsilon*(1 - x)*(x + Log[1 - x] - x*Log[1 - x]))/(2*x) -
			(Epsilon^2*(10*x - 4*x^2 - 8*Zeta2 + 4*x*Zeta2 - 2*x^2*Zeta2 +
	2*Log[1 - x] + 2*x*Log[1 - x] - 4*x^2*Log[1 - x] + Log[1 - x]^2 -
					2*x*Log[1 - x]^2 + x^2*Log[1 - x]^2 + 8*Log[1 - x]*Log[x] -
					4*x*Log[1 - x]*Log[x] + 2*x^2*Log[1 - x]*Log[x] +
					8*PolyLog[2, 1 - x] - 4*x*PolyLog[2, 1 - x] +
					2*x^2*PolyLog[2, 1 - x]))/(8*x)+
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[1 - Epsilon, 1 - Epsilon/2, 2, x_] :>
		-(Log[1 - x]/x) - (3*Epsilon*Log[1 - x]^2)/(4*x) -
			(Epsilon^2*(4*Zeta2*Log[1 - x] + 3*Log[1 - x]^3 +
	4*Log[1 - x]*PolyLog[2, 1 - x] - 8*PolyLog[3, 1 - x] + 8*Zeta[3]))/
			(8*x) +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[1 - Epsilon, 1 - Epsilon/2, 5/2 - Epsilon/2,
		-(1 - x_Symbol)^2/(4*x_Symbol)] :>
		(-6*x*(2 - 2*x + Log[x] + x*Log[x]))/(1 - x)^3 +
	(Epsilon*x*(56 - 56*x - 12*Zeta2 - 12*x*Zeta2 + 40*Log[x] + 40*x*Log[x] +
					9*Log[x]^2 + 9*x*Log[x]^2 - 24*Log[x]*Log[1 + x] -
					24*x*Log[x]*Log[1 + x] + 12*PolyLog[2, 1 - x] +
	12*x*PolyLog[2, 1 - x] - 24*PolyLog[2, -x] - 24*x*PolyLog[2, -x]))/
			(2*(1 - x)^3) +
	Epsilon^2 F21CHECK
	,
	(*hammerhart, ganz hammerhart...*)
	Hypergeometric2F1[2 - Epsilon, 1 - Epsilon/2, 5/2 - Epsilon/2,
		-(1 - x_Symbol)^2/(4*x_Symbol)] :>
	6/(1 - x)^2 - 6/(1 - x) + (6*Log[x])/(1 - x)^3 - (9*Log[x])/(1 - x)^2 +
		(3*Log[x])/(2*(1 - x)) + (3*Log[x])/(2*(1 + x)) +
		Epsilon*(-((x*(2 - 2*Pi^2*x - 2*x^2 + 3*Log[x] + 10*x*Log[x] +
							3*x^2*Log[x] + 9*x*Log[x]^2 - 24*x*Log[x]*Log[1 + x]))/
					((1 - x)^3*(1 + x))) -
				(12*x^2*PolyLog[2, 1 - x])/((1 - x)^3*(1 + x)) +
				(24*x^2*PolyLog[2, -x])/((1 - x)^3*(1 + x))) +
	Epsilon^2*((x*(3 + x)*(1 + 3*x)*PolyLog[2, 1 - x])/((1 - x)^3*(1 + x)) -
				(2*x*(3 + x)*(1 + 3*x)*PolyLog[2, -x])/((1 - x)^3*(1 + x)) -
				(36*x^2*PolyLog[3, 1 - x])/((1 - x)^3*(1 + x)) -
				(36*x^2*PolyLog[3, -x])/((1 - x)^3*(1 + x)) -
				(18*x^2*PolyLog[3, x])/((1 - x)^3*(1 + x)) -
				(72*x^2*PolyLog[3, 1 + x])/((1 - x)^3*(1 + x)) -
				(24*x^2*PolyLog[3, -((1 + x)/(1 - x))])/((1 - x)^3*(1 + x)) +
				(24*x^2*PolyLog[3, (1 + x)/(1 - x)])/((1 - x)^3*(1 + x)) -
				(x*(6*Pi^2 + 20*Pi^2*x + 6*Pi^2*x^2 - 144*Pi^2*x*Log[1 - x] -
						144*I*Pi*x*Log[1 - x]^2 - 12*Log[x] - 24*x*Log[x] -
						24*Pi^2*x*Log[x] - 12*x^2*Log[x] + 144*x*Zeta2*Log[x] -
						9*Log[x]^2 - 90*x*Log[x]^2 - 45*x^2*Log[x]^2 +
						108*x*Log[1 - x]*Log[x]^2 - 42*x*Log[x]^3 -
						72*Pi^2*x*Log[1 + x] + 288*I*Pi*x*Log[1 - x]*Log[1 + x] +
						72*Log[x]*Log[1 + x] + 240*x*Log[x]*Log[1 + x] +
						72*x^2*Log[x]*Log[1 + x] + 216*x*Log[x]^2*Log[1 + x] +
	288*I*Pi*x*Log[1 + x]^2 - 648*x*Zeta[3]))/(12*(1 - x)^3*(1 + x))) +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[2 - Epsilon, -Epsilon/2, 5/2 - Epsilon/2,
		-(1 - x_Symbol)^2/(4*x_Symbol)] :>
		1 + Epsilon*(-5/6 + 2/(1 - x)^2 - 2/(1 - x) + Log[x]/2 +
				(2*Log[x])/(1 - x)^3 - (3*Log[x])/(1 - x)^2) +
		Epsilon^2*(1/6 - 2/(3*(1 - x)^2) + 2/(3*(1 - x)) + Zeta2/2 +
				(2*Zeta2)/(1 - x)^3 - (3*Zeta2)/(1 - x)^2 + Log[1 - x]^2/2 +
				(2*Log[1 - x]^2)/(1 - x)^3 - (3*Log[1 - x]^2)/(1 - x)^2 -
				(5*Log[x])/12 - (8*Log[x])/(3*(1 - x)^3) + (4*Log[x])/(1 - x)^2 -
				Log[x]/(2*(1 - x)) - (5*Log[x]^2)/8 - (3*Log[x]^2)/(2*(1 - x)^3) +
				(9*Log[x]^2)/(4*(1 - x)^2) + Log[1 - x]*Log[1 + x] +
				(4*Log[1 - x]*Log[1 + x])/(1 - x)^3 -
				(6*Log[1 - x]*Log[1 + x])/(1 - x)^2 + Log[x]*Log[1 + x] +
				(4*Log[x]*Log[1 + x])/(1 - x)^3 - (6*Log[x]*Log[1 + x])/(1 - x)^2 +
				Log[1 + x]^2/2 + (2*Log[1 + x]^2)/(1 - x)^3 -
				(3*Log[1 + x]^2)/(1 - x)^2 - (Log[1 - x]*Log[1 - x^2])/2 -
				(2*Log[1 - x]*Log[1 - x^2])/(1 - x)^3 +
				(3*Log[1 - x]*Log[1 - x^2])/(1 - x)^2 - (Log[1 + x]*Log[1 - x^2])/2 -
				(2*Log[1 + x]*Log[1 - x^2])/(1 - x)^3 +
				(3*Log[1 + x]*Log[1 - x^2])/(1 - x)^2 - PolyLog[2, 1 - x]/2 -
				(2*PolyLog[2, 1 - x])/(1 - x)^3 + (3*PolyLog[2, 1 - x])/(1 - x)^2 +
				PolyLog[2, -x] + (4*PolyLog[2, -x])/(1 - x)^3 -
				(6*PolyLog[2, -x])/(1 - x)^2) +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[3 - Epsilon, 1 - Epsilon/2, 7/2 - Epsilon/2,
		-(1 - x_Symbol)^2/(4*x_Symbol)] :>
		(5*x*(-1 + 8*x - 8*x^3 + x^4 + 12*x^2*Log[x]))/((-1 + x)^5*(1 + x)) +
		(Epsilon*x*(16 + 2*x*(-49 + x*((49 - 8*x)*x + 180*Zeta2)) -
					3*Log[x]*(-5 + x*(40 + x*(94 - 5*(-8 + x)*x)) +
						30*x^2*(3*Log[x] - 8*Log[1 + x])) -
					360*x^2*(PolyLog[2, 1 - x] - 2*PolyLog[2, -x])))/
			(6*(-1 + x)^5*(1 + x)) + Epsilon^2*
			((-360*x^3*Zeta2*Log[1 - x])/((1 - x)^5*(1 + x)) -
				(60*I*Pi*x^3*Log[1 - x]^2)/((1 - x)^5*(1 + x)) +
				(x*(4 - 17*x - 51*x^2 - 17*x^3 + 4*x^4)*Log[x])/(3*(1 - x)^5*(1 + x)) +
				(x*(5 - 40*x - 282*x^2 - 200*x^3 + 25*x^4)*Log[x]^2)/
				(8*(1 - x)^5*(1 + x)) + (45*x^3*Log[1 - x]*Log[x]^2)/
				((1 - x)^5*(1 + x)) - (35*x^3*Log[x]^3)/(2*(1 - x)^5*(1 + x)) -
				(180*x^3*Zeta2*Log[1 + x])/((1 - x)^5*(1 + x)) +
				(120*I*Pi*x^3*Log[1 - x]*Log[1 + x])/((1 - x)^5*(1 + x)) -
				(x*(5 - 40*x - 94*x^2 - 40*x^3 + 5*x^4)*Log[x]*Log[1 + x])/
				((1 - x)^5*(1 + x)) + (90*x^3*Log[x]^2*Log[1 + x])/
				((1 - x)^5*(1 + x)) + (120*I*Pi*x^3*Log[1 + x]^2)/
				((1 - x)^5*(1 + x)) + (x*(5 - 40*x - 94*x^2 - 40*x^3 + 5*x^4)*
					PolyLog[2, 1 - x])/(2*(1 - x)^5*(1 + x)) -
				(x*(5 - 40*x - 94*x^2 - 40*x^3 + 5*x^4)*PolyLog[2, -x])/
				((1 - x)^5*(1 + x)) + (180*x^3*PolyLog[3, 1 - x])/
				((1 - x)^5*(1 + x)) + (180*x^3*PolyLog[3, -x])/((1 - x)^5*(1 + x)) +
				(90*x^3*PolyLog[3, x])/((1 - x)^5*(1 + x)) +
				(360*x^3*PolyLog[3, 1 + x])/((1 - x)^5*(1 + x)) +
				(120*x^3*PolyLog[3, -((1 + x)/(1 - x))])/((1 - x)^5*(1 + x)) -
				(120*x^3*PolyLog[3, (1 + x)/(1 - x)])/((1 - x)^5*(1 + x)) +
				(x*(2 - 15*x + 15*x^3 - 2*x^4 - 15*Zeta2 + 120*x*Zeta2 +
						282*x^2*Zeta2 + 120*x^3*Zeta2 - 15*x^4*Zeta2 - 1620*x^2*Zeta[3]))/
				(6*(1 - x)^5*(1 + x))) +
	Epsilon^3 F21CHECK
	,
	(* for graph[11,14] *)
	Hypergeometric2F1[1 - Epsilon, 1 - Epsilon/2, 5/2 - Epsilon/2,
		-(1 - x_)^2/(4*x_)] ->
		-12/(1 - x)^2 + 12/(1 - x) - (12*Log[x])/(1 - x)^3 +
		(18*Log[x])/(1 - x)^2 - (6*Log[x])/(1 - x) +
		Epsilon*(28/(1 - x)^2 - 28/(1 - x) - (12*Zeta2)/(1 - x)^3 +
				(18*Zeta2)/(1 - x)^2 - (6*Zeta2)/(1 - x) + (40*Log[x])/(1 - x)^3 -
				(60*Log[x])/(1 - x)^2 + (20*Log[x])/(1 - x) + (9*Log[x]^2)/(1 - x)^3 -
				(27*Log[x]^2)/(2*(1 - x)^2) + (9*Log[x]^2)/(2*(1 - x)) -
				(24*Log[x]*Log[1 + x])/(1 - x)^3 + (36*Log[x]*Log[1 + x])/(1 - x)^2 -
				(12*Log[x]*Log[1 + x])/(1 - x) + (12*PolyLog[2, 1 - x])/(1 - x)^3 -
				(18*PolyLog[2, 1 - x])/(1 - x)^2 + (6*PolyLog[2, 1 - x])/(1 - x) -
				(24*PolyLog[2, -x])/(1 - x)^3 + (36*PolyLog[2, -x])/(1 - x)^2 -
				(12*PolyLog[2, -x])/(1 - x)) +
	Epsilon^2 F21CHECK
	,

	(* nonanalytic at x = 1 ... *)
	Hypergeometric2F1[1 - Epsilon, 1 - Epsilon/2, 1 - (3*Epsilon)/2, x_] ->
		(1 - x)^(-1) + Epsilon^2*(Pi^2/(12*(1 - x)) -
	(Log[1 - x]*Log[x])/(2*(1 - x)) - PolyLog[2, 1 - x]/(2*(1 - x))) +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[1 - Epsilon, -Epsilon/2, 3/2 - Epsilon/2,
		-(1 - x_)^2/(4*x_)] ->
		1 - (Epsilon*(2 - 2*x + Log[x] + x*Log[x]))/(2*(1 - x)) +
	(Epsilon^2*(Pi^2 - Pi^2*x - 18*Zeta2 - 6*x*Zeta2 - 12*Log[1 - x]^2 -
					12*x*Log[1 - x]^2 + 12*Log[x] + 12*x*Log[x] + 3*Log[x]^2 +
					15*x*Log[x]^2 - 12*Log[1 - x]*Log[1 + x] -
					12*x*Log[1 - x]*Log[1 + x] - 24*Log[x]*Log[1 + x] -
					24*x*Log[x]*Log[1 + x] + 12*Log[1 - x]*Log[1 - x^2] +
					12*x*Log[1 - x]*Log[1 - x^2] + 12*PolyLog[2, 1 - x] +
	12*x*PolyLog[2, 1 - x] - 24*PolyLog[2, -x] - 24*x*PolyLog[2, -x]))/
			(24*(1 - x)) +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[-Epsilon, -Epsilon/2, (-3*Epsilon)/2, x_] ->
		1 + (Epsilon*Log[1 - x])/3 + (Epsilon^3*
				(Zeta2*Log[1 - x] + Log[1 - x]*PolyLog[2, 1 - x] -
					2*PolyLog[3, 1 - x] + 2*Zeta[3]))/6 +
	Epsilon^4 F21CHECK
	,
	Hypergeometric2F1[1 + Epsilon/2, 1 + Epsilon, 2 + (3*Epsilon)/2,
								x_] ->
	-(Log[1 - x]/x) + Epsilon*((-3*Zeta2)/(2*x) - (3*Log[1 - x])/(2*x) +
				(3*Log[1 - x]*Log[x])/(2*x) + (3*PolyLog[2, 1 - x])/(2*x)) +
		Epsilon^2*((-9*Zeta2)/(4*x) - (Zeta2*Log[1 - x])/(2*x) +
				(9*Log[1 - x]*Log[x])/(4*x) + (9*PolyLog[2, 1 - x])/(4*x) -
				(Log[1 - x]*PolyLog[2, 1 - x])/(2*x) + PolyLog[3, 1 - x]/x +
				(9*PolyLog[3, x])/(4*x) - Zeta[3]/x) +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[-Epsilon, -Epsilon/2, 3/2 - Epsilon/2,
			-(1 - x_)^2/(4*x_)] ->
		1 + Epsilon^2*(-2 + Log[x] -
	(2*Log[x])/(1 - x) - Log[x]^2/4) +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[-Epsilon, -Epsilon/2, 3/2 - Epsilon/2,
			-(1 - x_)^2/(4*x_)] ->
	1 + Epsilon^2*(-2 + Log[x] - (2*Log[x])/(1 - x) - Log[x]^2/4)+
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[-1 - Epsilon/2, 1, 1 + Epsilon/2, x_] ->
		1 - x + Epsilon*(x/2 + Log[1 - x]/2 - (x*Log[1 - x])/2) +
			Epsilon^2*(-x/2 + Zeta2/4 - (x*Zeta2)/4 - Log[1 - x]/4 +
				(x*Log[1 - x])/4 + Log[1 - x]^2/4 - (x*Log[1 - x]^2)/4 -
				(Log[1 - x]*Log[x])/4 + (x*Log[1 - x]*Log[x])/4 -
				PolyLog[2, 1 - x]/4 + (x*PolyLog[2, 1 - x])/4) +
			Epsilon^3*(x/2 - Zeta2/8 + (x*Zeta2)/8 + Log[1 - x]/4 -
	(x*Log[1 - x])/4 + (Zeta2*Log[1 - x])/4 - (x*Zeta2*Log[1 - x])/4 -
				Log[1 - x]^2/8 + (x*Log[1 - x]^2)/8 + Log[1 - x]^3/12 -
				(x*Log[1 - x]^3)/12 + (Log[1 - x]*Log[x])/8 -
				(x*Log[1 - x]*Log[x])/8 - (Log[1 - x]^2*Log[x])/8 +
				(x*Log[1 - x]^2*Log[x])/8 + PolyLog[2, 1 - x]/8 -
				(x*PolyLog[2, 1 - x])/8 - PolyLog[3, 1 - x]/4 +
	(x*PolyLog[3, 1 - x])/4 - PolyLog[3, x]/8 + (x*PolyLog[3, x])/8 +
				Zeta[3]/4 - (x*Zeta[3])/4)+
	Epsilon^4 F21CHECK
	,
	Hypergeometric2F1[1 + Epsilon/2, 1 + Epsilon, 2 + Epsilon/2, x_] ->
		-(Log[1 - x]/x) + Epsilon*(-Zeta2/(2*x) - Log[1 - x]/(2*x) +
				Log[1 - x]^2/(2*x) + (Log[1 - x]*Log[x])/(2*x) +
				PolyLog[2, 1 - x]/(2*x)) +
	Epsilon^2*(-Zeta2/(4*x) + Log[1 - x]^2/(4*x) - Log[1 - x]^3/(6*x) +
				(Log[1 - x]*Log[x])/(4*x) - (Log[1 - x]^2*Log[x])/(4*x) +
	PolyLog[2, 1 - x]/(4*x) - (Log[1 - x]*PolyLog[2, 1 - x])/(2*x) +
	PolyLog[3, 1 - x]/(2*x) + PolyLog[3, x]/(4*x) - Zeta[3]/(2*x)) +
	Epsilon^3 F21CHECK
	,
		Hypergeometric2F1[Epsilon/2, 1 + Epsilon, 1 + Epsilon/2, x_] ->
		1 - (Epsilon*Log[1 - x])/2 +
			Epsilon^2*(Zeta2/4 + Log[1 - x]^2/4 - (Log[1 - x]*Log[x])/4 -
				PolyLog[2, 1 - x]/4) +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[1 - Epsilon/2, 2 + Epsilon/2, 4 - Epsilon/2, u_] ->
		6/u^2 - 3/u + (6*Log[1 - u])/u^3 - (6*Log[1 - u])/u^2 +
			Epsilon*(-4/u^2 + 17/(4*u) - (3*Zeta2)/u^3 + (3*Zeta2)/u^2 -
	(7*Log[1 - u])/u^3 + (17*Log[1 - u])/(2*u^2) - (3*Log[1 - u])/(2*u) -
				(3*Log[1 - u]^2)/(2*u^3) + (3*Log[1 - u]^2)/(2*u^2) +
				(3*Log[1 - u]*Log[u])/u^3 - (3*Log[1 - u]*Log[u])/u^2 +
				(3*PolyLog[2, 1 - u])/u^3 - (3*PolyLog[2, 1 - u])/u^2) +
			Epsilon^2*(31/(8*u^2) - 23/(8*u) + (7*Zeta2)/(2*u^3) -
				(23*Zeta2)/(4*u^2) + (3*Zeta2)/(2*u) + (47*Log[1 - u])/(8*u^3) -
				(8*Log[1 - u])/u^2 + (17*Log[1 - u])/(8*u) +
				(7*Log[1 - u]^2)/(4*u^3) - (17*Log[1 - u]^2)/(8*u^2) +
	(3*Log[1 - u]^2)/(8*u) + Log[1 - u]^3/(4*u^3) - Log[1 - u]^3/(4*u^2) -
				(7*Log[1 - u]*Log[u])/(2*u^3) + (23*Log[1 - u]*Log[u])/(4*u^2) -
				(3*Log[1 - u]*Log[u])/(2*u) - (3*Log[1 - u]^2*Log[u])/(4*u^3) +
				(3*Log[1 - u]^2*Log[u])/(4*u^2) - (7*PolyLog[2, 1 - u])/(2*u^3) +
				(23*PolyLog[2, 1 - u])/(4*u^2) - (3*PolyLog[2, 1 - u])/(2*u) -
				(3*Log[1 - u]*PolyLog[2, 1 - u])/(2*u^3) +
				(3*Log[1 - u]*PolyLog[2, 1 - u])/(2*u^2) +
				(3*PolyLog[3, 1 - u])/(2*u^3) - (3*PolyLog[3, 1 - u])/(2*u^2) -
				(3*PolyLog[3, u])/(2*u^3) + (3*PolyLog[3, u])/(2*u^2) -
				(3*Zeta[3])/(2*u^3) + (3*Zeta[3])/(2*u^2)) +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[2 - Epsilon, -Epsilon/2, 2 - (3*Epsilon)/2, x_] ->
		1 + (Epsilon*Log[1 - x])/2 +
	Epsilon^2*(-1/4 - Zeta2/4 - Log[1 - x]/(4*x) + (Log[1 - x]*Log[x])/4 +
				PolyLog[2, 1 - x]/4) +
	Epsilon^3 F21CHECK
	,
	(* highly nontrivial ... *)
	Hypergeometric2F1[1 + Epsilon/2, -1 + Epsilon, (3*Epsilon)/2, y_] ->
		(-2*y)/(3*Epsilon) + (3 + y + 2*y*Log[1 - y])/3 -
		(Epsilon*(-12*y*Zeta2 + 6*Log[1 - y] + 6*y*Log[1 - y] +
					12*y*Log[1 - y]*Log[y] + 12*y*PolyLog[2, 1 - y]))/18 +
	(Epsilon^2*y*(-6*Zeta2 + 6*Zeta2*Log[1 - y] + 6*Log[1 - y]*Log[y] +
					6*PolyLog[2, 1 - y] + 6*Log[1 - y]*PolyLog[2, 1 - y] -
					12*PolyLog[3, 1 - y] - 18*PolyLog[3, y] + 12*Zeta[3]))/18 +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[3, 3 + Epsilon, 4 + 2*Epsilon, u_] ->
	3/(2*(1 - u)^2) - 3/(2*(1 - u)) - 3/u^2 - 3/(2*u) - (3*Log[1 - u])/u^3 +
			Epsilon*(7/(4*(1 - u)^2) - 31/(4*(1 - u)) - 19/(2*u^2) - 31/(4*u) -
				(6*Zeta2)/u^3 + (3*Log[1 - u])/(2*(1 - u)^2) -
				(3*Log[1 - u])/(2*(1 - u)) - (31*Log[1 - u])/(2*u^3) -
				(3*Log[1 - u])/u^2 - (3*Log[1 - u])/(2*u) -
				(3*Log[1 - u]^2)/(2*u^3) + (6*Log[1 - u]*Log[u])/u^3 +
				(6*PolyLog[2, 1 - u])/u^3) +
	Epsilon^2*(-23/(8*(1 - u)^2) - 81/(8*(1 - u)) - 29/(4*u^2) - 81/(8*u) +
				(3*Zeta2)/(1 - u)^2 - (3*Zeta2)/(1 - u) - (31*Zeta2)/u^3 -
				(6*Zeta2)/u^2 - (3*Zeta2)/u + (7*Log[1 - u])/(4*(1 - u)^2) -
				(43*Log[1 - u])/(4*(1 - u)) - (105*Log[1 - u])/(4*u^3) -
				(25*Log[1 - u])/(2*u^2) - (43*Log[1 - u])/(4*u) -
				(6*Zeta2*Log[1 - u])/u^3 + (3*Log[1 - u]^2)/(4*(1 - u)^2) -
				(3*Log[1 - u]^2)/(4*(1 - u)) - (31*Log[1 - u]^2)/(4*u^3) -
				(3*Log[1 - u]^2)/(2*u^2) - (3*Log[1 - u]^2)/(4*u) -
				Log[1 - u]^3/(2*u^3) - (3*Log[1 - u]*Log[u])/(1 - u)^2 +
				(3*Log[1 - u]*Log[u])/(1 - u) + (31*Log[1 - u]*Log[u])/u^3 +
				(6*Log[1 - u]*Log[u])/u^2 + (3*Log[1 - u]*Log[u])/u +
				(3*Log[1 - u]^2*Log[u])/u^3 - (3*PolyLog[2, 1 - u])/(1 - u)^2 +
				(3*PolyLog[2, 1 - u])/(1 - u) + (31*PolyLog[2, 1 - u])/u^3 +
				(6*PolyLog[2, 1 - u])/u^2 + (3*PolyLog[2, 1 - u])/u +
	(6*PolyLog[3, 1 - u])/u^3 + (12*PolyLog[3, u])/u^3 - (6*Zeta[3])/u^3)+
	Epsilon^3 F21CHECK
	,
		Hypergeometric2F1[1 + Epsilon/2, -Epsilon/2, 3 - Epsilon/2, u_] ->
		1 + Epsilon*(-3/4 + 1/(2*u) + Log[1 - u]/2 + Log[1 - u]/(2*u^2) -
				Log[1 - u]/u) + Epsilon^2*
			(1/8 + 1/(8*u) - Zeta2/2 - Zeta2/(4*u^2) + Zeta2/(2*u) -
				(3*Log[1 - u])/8 - Log[1 - u]/(8*u^2) + Log[1 - u]/(2*u) -
				Log[1 - u]^2/8 - Log[1 - u]^2/(8*u^2) + Log[1 - u]^2/(4*u) +
				(Log[1 - u]*Log[u])/2 + (Log[1 - u]*Log[u])/(4*u^2) -
				(Log[1 - u]*Log[u])/(2*u) + PolyLog[2, 1 - u]/2 +
				PolyLog[2, 1 - u]/(4*u^2) - PolyLog[2, 1 - u]/(2*u))+
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[2 - Epsilon, -Epsilon/2, 1 - (3*Epsilon)/2, x_] ->
		1 - (Epsilon*(x - Log[1 - x] + x*Log[1 - x]))/(2*(1 - x)) +
			(Epsilon^2*(-6*x - 3*Zeta2 + 3*x*Zeta2 + 3*Log[1 - x] -
	3*x*Log[1 - x] + 3*Log[1 - x]*Log[x] - 3*x*Log[1 - x]*Log[x] +
					3*PolyLog[2, 1 - x] - 3*x*PolyLog[2, 1 - x]))/(12*(1 - x))+
	Epsilon^3 F21CHECK
	,
		Hypergeometric2F1[2 + Epsilon/2, Epsilon, 1 + (3*Epsilon)/2, x_] ->
		1 + (Epsilon*(x - Log[1 - x] + x*Log[1 - x]))/(1 - x) +
			(Epsilon^2*(-6*x - 12*Zeta2 + 12*x*Zeta2 + 12*Log[1 - x] -
	12*x*Log[1 - x] + 12*Log[1 - x]*Log[x] - 12*x*Log[1 - x]*Log[x] +
	12*PolyLog[2, 1 - x] - 12*x*PolyLog[2, 1 - x]))/(12*(1 - x))+
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[1 + Epsilon/2, Epsilon, 2 + (3*Epsilon)/2, x_] ->
		1 + (Epsilon*(x + Log[1 - x] - x*Log[1 - x]))/x -
			(Epsilon^2*(6*x - 18*Zeta2 + 12*x*Zeta2 - 12*Log[1 - x] +
	12*x*Log[1 - x] + 18*Log[1 - x]*Log[x] - 12*x*Log[1 - x]*Log[x] +
					18*PolyLog[2, 1 - x] - 12*x*PolyLog[2, 1 - x]))/(12*x)+
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[-Epsilon, -1 - Epsilon/2, (-3*Epsilon)/2, x_] ->
		(3 - 2*x)/3 + (Epsilon*(1 - x)*Log[1 - x])/3 +
	(Epsilon^2*x*PolyLog[2, x])/6 +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[1 - Epsilon, -Epsilon/2, 1 - (3*Epsilon)/2, x_] ->
		1 + (Epsilon*Log[1 - x])/2 +
	(Epsilon^2*( - 3*Zeta2 + 3*Log[1 - x]*Log[x] + 3*PolyLog[2, 1 - x])
	)/12 +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[1 + Epsilon/2, Epsilon, 1 + (3*Epsilon)/2, x_] ->
		1 - Epsilon*Log[1 - x] + (Epsilon^2*
	(- 12*Zeta2 + 12*Log[1 - x]*Log[x] + 12*PolyLog[2, 1 - x]))/12+
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[2 + Epsilon/2, 2 + Epsilon, 3 + (3*Epsilon)/2, x_] ->
		(2*(x + Log[1 - x] - x*Log[1 - x]))/((1 - x)*x^2) +
			(3*Epsilon*(x + 2*Zeta2 - 2*x*Zeta2 + 3*Log[1 - x] - 3*x*Log[1 - x] -
					2*Log[1 - x]*Log[x] + 2*x*Log[1 - x]*Log[x] -
					2*PolyLog[2, 1 - x] + 2*x*PolyLog[2, 1 - x]))/(2*(1 - x)*x^2) -
			(Epsilon^2*(12*x - 2*Pi^2*x - 81*Zeta2 + 81*x*Zeta2 - 15*Log[1 - x] -
					2*Pi^2*Log[1 - x] + 15*x*Log[1 - x] + 2*Pi^2*x*Log[1 - x] +
					81*Log[1 - x]*Log[x] - 69*x*Log[1 - x]*Log[x] +
					81*PolyLog[2, 1 - x] - 69*x*PolyLog[2, 1 - x] -
					12*Log[1 - x]*PolyLog[2, 1 - x] +
					12*x*Log[1 - x]*PolyLog[2, 1 - x] + 24*PolyLog[3, 1 - x] -
					24*x*PolyLog[3, 1 - x] + 54*PolyLog[3, x] - 54*x*PolyLog[3, x] -
					24*Zeta[3] + 24*x*Zeta[3]))/(12*(1 - x)*x^2)+
	Epsilon^3 F21CHECK
	,
		Hypergeometric2F1[-Epsilon, -Epsilon/2, -1 - (3*Epsilon)/2, x_] ->
		1 + Epsilon^2*(1/2 - 1/(2*(1 - x))) +
			Epsilon*(-1/3 + 1/(3*(1 - x)) + Log[1 - x]/3) +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[-2 - Epsilon/2, 1, 1 + Epsilon/2, x_] ->
		1 - 2*x + x^2 + Epsilon*(x - (3*x^2)/4 + Log[1 - x]/2 -
		x*Log[1 - x] +
				(x^2*Log[1 - x])/2) + Epsilon^2*
		((-7*x)/8 + (3*x^2)/4 + Zeta2/4 - (x*Zeta2)/2 + (x^2*Zeta2)/4 -
			(3*Log[1 - x])/8 + (3*x*Log[1 - x])/4 - (3*x^2*Log[1 - x])/8 +
				Log[1 - x]^2/4 - (x*Log[1 - x]^2)/2 + (x^2*Log[1 - x]^2)/4 -
				(Log[1 - x]*Log[x])/4 + (x*Log[1 - x]*Log[x])/2 -
				(x^2*Log[1 - x]*Log[x])/4 - PolyLog[2, 1 - x]/4 +
				(x*PolyLog[2, 1 - x])/2 - (x^2*PolyLog[2, 1 - x])/4) +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[-1 - Epsilon/2, 1, 2 + Epsilon/2, x_] ->
		(2 - x)/2 - (Epsilon*(1 - x)*(x + Log[1 - x] - x*Log[1 - x]))/(4*x) +
	(Epsilon^2*(1 - x)*(2*x - Zeta2 + x*Zeta2 + Log[1 - x] - x*Log[1 - x] -
					Log[1 - x]^2 + x*Log[1 - x]^2 + Log[1 - x]*Log[x] -
	x*Log[1 - x]*Log[x] + PolyLog[2, 1 - x] - x*PolyLog[2, 1 - x]))/(8*x)+
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[1, -Epsilon/2, 2 + Epsilon/2, x_] ->
		1 - (Epsilon*(x + Log[1 - x] - x*Log[1 - x]))/(2*x) +
		(Epsilon^2*(2*x - Zeta2 + x*Zeta2 + Log[1 - x] - x*Log[1 - x] -
					Log[1 - x]^2 + x*Log[1 - x]^2 + Log[1 - x]*Log[x] -
	x*Log[1 - x]*Log[x] + PolyLog[2, 1 - x] - x*PolyLog[2, 1 - x]))/(4*x)+
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[1, 3 + Epsilon, 3 + 2*Epsilon, u_] ->
		(1 - u)^(-1) + Epsilon*(3/(2*(1 - u)) + u^(-1) + Log[1 - u]/(1 - u) +
				Log[1 - u]/u^2 + Log[1 - u]/u) +
	Epsilon^2*(-3/(4*(1 - u)) - 1/(2*u) + (2*Zeta2)/(1 - u) + (2*Zeta2)/u^2 +
				(2*Zeta2)/u + (3*Log[1 - u])/(2*(1 - u)) + (3*Log[1 - u])/(2*u^2) +
				(3*Log[1 - u])/(2*u) + Log[1 - u]^2/(2*(1 - u)) +
				Log[1 - u]^2/(2*u^2) + Log[1 - u]^2/(2*u) -
				(2*Log[1 - u]*Log[u])/(1 - u) - (2*Log[1 - u]*Log[u])/u^2 -
				(2*Log[1 - u]*Log[u])/u - (2*PolyLog[2, 1 - u])/(1 - u) -
				(2*PolyLog[2, 1 - u])/u^2 - (2*PolyLog[2, 1 - u])/u) +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[1, 1 + Epsilon, 2 + 2*Epsilon, x_] ->
		-(Log[1 - x]/x) - (Epsilon*(4*Zeta2 + 4*Log[1 - x] + Log[1 - x]^2 -
					4*Log[1 - x]*Log[x] - 4*PolyLog[2, 1 - x]))/(2*x) -
		(Epsilon^2*(24*Zeta2 + 12*Zeta2*Log[1 - x] + 6*Log[1 - x]^2 +
					Log[1 - x]^3 - 24*Log[1 - x]*Log[x] - 6*Log[1 - x]^2*Log[x] -
					24*PolyLog[2, 1 - x] - 12*PolyLog[3, 1 - x] - 24*PolyLog[3, x] +
					12*Zeta[3]))/(6*x) +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[2, Epsilon, 2 + 2*Epsilon, u_] ->
		1 - Epsilon*Log[1 - u] - (Epsilon^2*
				(4*u + 4*u*Zeta2 + 4*Log[1 - u] + u*Log[1 - u]^2 -
					4*u*Log[1 - u]*Log[u] - 4*u*PolyLog[2, 1 - u]))/(2*u)+
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[Epsilon, 1 + 2*Epsilon, 2 + 2*Epsilon, u_] ->
		1 + (Epsilon*(u + Log[1 - u] - u*Log[1 - u]))/u -
		(Epsilon^2*(2*u - 4*Zeta2 - 2*Log[1 - u] + 2*u*Log[1 - u] +
					Log[1 - u]^2 - u*Log[1 - u]^2 + 4*Log[1 - u]*Log[u] +
					4*PolyLog[2, 1 - u]))/(2*u) +
		(Epsilon^3*(6*u + 12*Zeta2 - 6*Log[1 - u] + 6*u*Log[1 - u] -
	3*Log[1 - u]^2 + 3*u*Log[1 - u]^2 + Log[1 - u]^3 - u*Log[1 - u]^3 -
					12*Log[1 - u]*Log[u] + 6*Log[1 - u]^2*Log[u] -
					12*PolyLog[2, 1 - u] + 12*Log[1 - u]*PolyLog[2, 1 - u] -
	12*PolyLog[3, 1 - u] - 24*PolyLog[3, u] + 12*Zeta[3]))/(6*u)+
	Epsilon^4 F21CHECK
	,
	Hypergeometric2F1[Epsilon, 2*Epsilon, 1 + 2*Epsilon, u_] ->
		1 + 2*Epsilon^2*(Zeta2 - Log[1 - u]*Log[u] - PolyLog[2, 1 - u]) +
		Epsilon^3*(Log[1 - u]^2*Log[u] + 2*Log[1 - u]*PolyLog[2, 1 - u] -
				2*PolyLog[3, 1 - u] - 4*PolyLog[3, u] + 2*Zeta[3]) +
	Epsilon ^4 FC21CHECK
	,
	Hypergeometric2F1[1, 1 + Epsilon, 1 + 2*Epsilon, u_] ->
		(1 - u)^(-1) + (Epsilon*Log[1 - u])/(1 - u) +
		(Epsilon^2*(Pi^2 + 6*Zeta2 + 3*Log[1 - u]^2 - 12*Log[1 - u]*Log[u] -
					12*PolyLog[2, 1 - u]))/(6*(1 - u)) +
	Epsilon^3 FC21CHECK
	,
	Hypergeometric2F1[-Epsilon/2, Epsilon, 1 + Epsilon/2, x_] ->
	1 - (Epsilon^2*(Zeta2 - Log[1 - x]*Log[x] - PolyLog[2, 1 - x]))/2+
	Epsilon^3 FC21CHECK
	,
	Hypergeometric2F1[-Epsilon/2, Epsilon, 1 + (3*Epsilon)/2, x_] ->
	1 - (Epsilon^2*(Zeta2 - Log[1 - x]*Log[x] - PolyLog[2, 1 - x]))/2+
	Epsilon^3 FC21CHECK
	,
	Hypergeometric2F1[Epsilon/2, 1, 1 + Epsilon, z_] ->
		1 - (Epsilon*Log[1 - z])/2 - (Epsilon^2*
	(4*Zeta2 + Log[1 - z]^2 - 4*Log[1 - z]*Log[z]-4*PolyLog[2, 1 - z]))/8 -
	(Epsilon^3*(12*Zeta2*Log[1 - z] + Log[1 - z]^3 - 6*Log[1 - z]^2*Log[z] -
					12*PolyLog[3, 1 - z] - 24*PolyLog[3, z] + 12*Zeta[3]))/48+
	Epsilon^4 F21CHECK
	,
	Hypergeometric2F1[1 - Epsilon/2, 1 + Epsilon/2, 2 - Epsilon/2, z_] ->
		-(Log[1 - z]/z) + (Epsilon*(2*Zeta2 + 2*Log[1 - z] + Log[1 - z]^2 -
					2*Log[1 - z]*Log[z] - 2*PolyLog[2, 1 - z]))/(4*z) -
		(Epsilon^2*(6*Zeta2 + 3*Log[1 - z]^2 + Log[1 - z]^3 - 6*Zeta2*Log[z] -
					6*Log[1 - z]*Log[z] + 3*Log[1 - z]*Log[z]^2 +
					6*Nielsen[1, 2, 1 - z] - 6*Nielsen[1, 2, z] - 6*PolyLog[2, 1 - z] +
					6*Log[z]*PolyLog[2, 1 - z] - 6*Zeta[3]))/(24*z) +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[-Epsilon/2, Epsilon/2, 1 - Epsilon/2, z_] ->
		1 - (Epsilon^2*(3*Zeta2 - 3*Log[1 - z]*Log[z] - 3*PolyLog[2, 1 - z])
				)/12 +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[1 + Epsilon/2, 1 - Epsilon/2, 2 - Epsilon/2, z_] ->
		-(Log[1 - z]/z) + (Epsilon*(2*Zeta2 + 2*Log[1 - z] + Log[1 - z]^2 -
					2*Log[1 - z]*Log[z] - 2*PolyLog[2, 1 - z]))/(4*z) -
			(Epsilon^2*(6*Zeta2 + 3*Log[1 - z]^2 + Log[1 - z]^3 -
					6*Log[1 - z]*Log[z] - 3*Log[1 - z]^2*Log[z] - 6*PolyLog[2, 1 - z] -
					6*Log[1 - z]*PolyLog[2, 1 - z] + 6*PolyLog[3, 1 - z] -
					6*PolyLog[3, z] - 6*Zeta[3]))/(24*z) +
		Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[Epsilon/2, Epsilon, 1 + Epsilon, x_] ->
		1 + (Epsilon^2*Zeta2)/2 - (Epsilon^2*Log[1 - x]*Log[x])/2 -
		(Epsilon^2*PolyLog[2, 1 - x])/2 +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[Epsilon/2, -Epsilon/2, 1 - Epsilon/2, z_] ->
		1 - (Epsilon^2*(Zeta2 - Log[1 - z]*Log[z] - PolyLog[2, 1 - z]))/4 -
			(Epsilon^3*(Log[1 - z]^2*Log[z] + 2*Log[1 - z]*PolyLog[2, 1 - z] -
					2*PolyLog[3, 1 - z] + 2*PolyLog[3, z] + 2*Zeta[3]))/16 +
			Epsilon^4 F21CHECK
	,
	Hypergeometric2F1[1 + Epsilon/2, Epsilon, 1 + Epsilon, x_] ->
		1 - Epsilon*Log[1-x] + (Epsilon^2*
	(2*EulerGamma*Log[1-x] + Log[1-x]^2 - 2*PolyLog[2, x]))/4+
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[2, 1 - Epsilon, 2 - Epsilon/2, x_] ->
		(1 - x)^(-1) + Epsilon*(-EulerGamma/2 - 1/(2*(1 - x)) +
	EulerGamma/(2*(1 - x)) + Log[1 - x]/(2*(1 - x)) - Log[1 - x]/(2*x))+
	Epsilon^2(-(6*Zeta2 - 6*Zeta2*Log[1 - x] - 3*Log[1 - x]^2 + Log[1 - x]^3 -
			6*Log[1 - x]*Log[x] + 3*Log[1 - x]^2*Log[x] - 6*PolyLog[2, 1 - x] +
			6*PolyLog[3, 1 - x] - 6*PolyLog[3, x] - 6*Zeta[3])/(24*x)) +
	Epsilon^3 F21CHECK
	,

	Hypergeometric2F1[1 - Epsilon, -Epsilon/2, 1 - Epsilon/2, z_] ->
		1 + (Epsilon*Log[1 - z])/2 +
			Epsilon^2*(Zeta2/4 + Log[1 - z]^2/4 - (Log[1 - z]*Log[z])/4 -
				PolyLog[2, 1 - z]/4) + Epsilon^3*
			(Log[1 - z]^3/12 - (Log[1 - z]^2*Log[z])/8 -
				(Log[1 - z]*PolyLog[2, 1 - z])/4 + PolyLog[3, 1 - z]/4 +
				PolyLog[3, z]/8 - Zeta[3]/4) +
	Epsilon^4 F21CHECK
	,
	Hypergeometric2F1[1 - Epsilon, 1 - Epsilon/2, 2 - Epsilon/2, z_] ->
		-(Log[1 - z]/z) + Epsilon*(Zeta2/(2*z) + Log[1 - z]/(2*z) -
				Log[1 - z]^2/(2*z) - (Log[1 - z]*Log[z])/(2*z) -
				PolyLog[2, 1 - z]/(2*z)) +
			Epsilon^2*(-Zeta2/(4*z) + Log[1 - z]^2/(4*z) - Log[1 - z]^3/(6*z) +
				(Log[1 - z]*Log[z])/(4*z) - (Log[1 - z]^2*Log[z])/(4*z) +
				PolyLog[2, 1 - z]/(4*z) - (Log[1 - z]*PolyLog[2, 1 - z])/(2*z) +
				PolyLog[3, 1 - z]/(2*z) + PolyLog[3, z]/(4*z) - Zeta[3]/(2*z)) +
			Epsilon^3*(Log[1 - z]^3/(12*z) + (Log[1 - z]^2*Log[z])/(8*z) +
				(Log[1 - z]*PolyLog[2, 1 - z])/(4*z) - PolyLog[3, 1 - z]/(4*z) -
				PolyLog[3, z]/(8*z) + Zeta[3]/(4*z)) +
	Epsilon^4 F21CHECK

	,
	Hypergeometric2F1[-Epsilon, -Epsilon/2, 1 - Epsilon/2, z_] ->
		1 + (Epsilon^2*(Pi^2 - 6*Log[1 - z]*Log[z] - 6*PolyLog[2, 1 - z]))/12 -
		(Epsilon^3*(Log[1 - z]^2*Log[z] + 2*Log[1 - z]*PolyLog[2, 1 - z] -
					2*PolyLog[3, 1 - z] - PolyLog[3, z] + 2*Zeta[3]))/4 +
	Epsilon ^4 F21CHECK
	,
	Hypergeometric2F1[1 + Epsilon/2, -Epsilon/2, 1 - Epsilon/2, x_] ->
		1 + (Epsilon*Log[1 - x])/2 - (Epsilon^2*
	(4*Zeta2 + Log[1 - x]^2 - 4*Log[1 - x]*Log[x] - 4*PolyLog[2, 1 - x])
																)/8 +
	(Epsilon^3*(Log[1 - x]^3 - 6*Log[1 - x]^2*Log[x] -
					12*Log[1 - x]*PolyLog[2, 1 - x] + 12*PolyLog[3, 1 - x] -
					12*PolyLog[3, x] - 12*Zeta[3]))/48 +
	Epsilon^4  F21CHECK
	,
	Hypergeometric2F1[1 + Epsilon/2, 1 - Epsilon/2, 3 - Epsilon/2, x_] ->
		(2*(x + Log[1 - x] - x*Log[1 - x]))/x^2 +
		(Epsilon*(x - 2*Zeta2 + 2*x*Zeta2 - Log[1 - x] + x*Log[1 - x] -
					Log[1 - x]^2 + x*Log[1 - x]^2 + 2*Log[1 - x]*Log[x] -
	2*x*Log[1 - x]*Log[x] + 2*PolyLog[2, 1 - x] - 2*x*PolyLog[2, 1 - x])
	)/(2*x^2) + (Epsilon^2*(18*x + 6*Zeta2 - 18*x*Zeta2 + 12*Log[1 - x] -
					12*x*Log[1 - x] + 3*Log[1 - x]^2 - 3*x*Log[1 - x]^2 +
					2*Log[1 - x]^3 - 2*x*Log[1 - x]^3 - 6*Log[1 - x]*Log[x] +
					18*x*Log[1 - x]*Log[x] - 6*Log[1 - x]^2*Log[x] +
					6*x*Log[1 - x]^2*Log[x] - 6*PolyLog[2, 1 - x] +
					18*x*PolyLog[2, 1 - x] - 12*Log[1 - x]*PolyLog[2, 1 - x] +
					12*x*Log[1 - x]*PolyLog[2, 1 - x] + 12*PolyLog[3, 1 - x] -
					12*x*PolyLog[3, 1 - x] - 12*PolyLog[3, x] + 12*x*PolyLog[3, x] -
					12*Zeta[3] + 12*x*Zeta[3]))/(24*x^2) +
	Epsilon^3  F21CHECK
	,
	Hypergeometric2F1[Epsilon/2 + 1, -(Epsilon/2), 2 - Epsilon/2, u_] ->
	(Epsilon^2*Zeta2)/(4*u) + (2 - Epsilon - Epsilon^2*Zeta2)/2 +
		((2 - Epsilon)*Epsilon*Log[1 - u])/4 -
	((2 - Epsilon)*Epsilon*Log[1 - u])/(4*u) - (Epsilon^2*Log[1 - u]^2)/8 +
		(Epsilon^2*Log[1 - u]^2)/(8*u) + (Epsilon^2*Log[1 - u]*Log[u])/2 -
	(Epsilon^2*Log[1 - u]*Log[u])/(4*u) + (Epsilon^2*PolyLog[2, 1 - u])/2 -
		(Epsilon^2*PolyLog[2, 1 - u])/(4*u) +
	Epsilon^3  F21CHECK
	,

	Hypergeometric2F1[Epsilon/2, Epsilon, 1 + Epsilon, x_] ->
		1 + (Epsilon^2*Zeta2)/2 - (Epsilon^2*Log[1 - x]*Log[x])/2 -
		(Epsilon^2*PolyLog[2, 1 - x])/2 +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[1 + Epsilon/2, Epsilon, 1 + Epsilon, x_] ->
		1 - Epsilon*Log[1-x] + (Epsilon^2*
	(2*EulerGamma*Log[1-x] + Log[1-x]^2 - 2*PolyLog[2, x]))/4+
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[2, 1 - Epsilon, 2 - Epsilon/2, x_] ->
		(1 - x)^(-1) + Epsilon*(-EulerGamma/2 - 1/(2*(1 - x)) +
	EulerGamma/(2*(1 - x)) + Log[1 - x]/(2*(1 - x)) - Log[1 - x]/(2*x))+
	Epsilon^2(-(6*Zeta2 - 6*Zeta2*Log[1 - x] - 3*Log[1 - x]^2 + Log[1 - x]^3 -
			6*Log[1 - x]*Log[x] + 3*Log[1 - x]^2*Log[x] - 6*PolyLog[2, 1 - x] +
			6*PolyLog[3, 1 - x] - 6*PolyLog[3, x] - 6*Zeta[3])/(24*x)) +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[1, 1 - Epsilon, 2 - Epsilon/2, x_] :>
		-(Log[1 - x]/x) - (Epsilon*(2*EulerGamma*x - 2*Zeta2 - 2*Log[1 - x] +
					2*EulerGamma*Log[1 - x] + Log[1 - x]^2 + 2*Log[1 - x]*Log[x] +
					2*PolyLog[2, 1 - x]))/(4*x) +
	Epsilon^2 F21CHECK + Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[1, -Epsilon, 1 - Epsilon/2, x_] :>
	1 + Epsilon*Log[1 - x] + (Epsilon^2*Log[1 - x]^2)/4 -
		(Epsilon^2*PolyLog[2, x])/2 +
	Epsilon^3  (
	-(Pi^2*Log[1 - x])/24 + Log[1 - x]^3/24 + (Log[1 - x]^2*Log[x])/8 +
		PolyLog[3, 1 - x]/4 - PolyLog[3, x]/4 - Zeta[3]/4
	) +
	Epsilon^4 F21CHECK
	,
	Hypergeometric2F1[1, -1 - Epsilon, 1 - Epsilon/2, x_] :>
	1 - x - (Epsilon*(x - 2*(1 - x)*Log[1 - x]))/2 +
		(Epsilon^2*(x + 2*(1 - x)*Log[1 - x] + (1 - x)*Log[1 - x]^2 -
				2*(1 - x)*PolyLog[2, x]))/4 +
	Epsilon^3 (-3*x - 6*Log[1 - x] - Pi^2*Log[1 - x] + 6*x*Log[1 - x] +
	Pi^2*x*Log[1 - x] + 3*Log[1 - x]^2 - 3*x*Log[1 - x]^2 + Log[1 - x]^3 -
	x*Log[1 - x]^3 + 3*Log[1 - x]^2*Log[x] - 3*x*Log[1 - x]^2*Log[x] -
	6*PolyLog[2, x] + 6*x*PolyLog[2, x] + 6*PolyLog[3, 1 - x] -
	6*x*PolyLog[3, 1 - x] - 6*PolyLog[3, x] + 6*x*PolyLog[3, x] -
	6*Zeta[3] + 6*x*Zeta[3])/24 +
	Epsilon^4 F21CHECK
	,
	Hypergeometric2F1[-Epsilon/2, 1 + Epsilon, 1, z_] :>
		1 + (Epsilon*Log[1 - z])/2 - (Epsilon^2*
				(2*Pi^2 + 6*EulerGamma*Log[1 - z] + 3*Log[1 - z]^2 -
					12*Log[1 - z]*Log[z] - 12*PolyLog[2, 1 - z]))/24 +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[1, 1 + Epsilon/2, 1 - Epsilon/2, 1 - x_] :>
	x^(-1) - (Epsilon*Log[x])/x +
	Epsilon^2 F21CHECK
	,
	Hypergeometric2F1[1,Epsilon/2,Epsilon+1,-1] :>
	1 - (Epsilon*Log[2])/2 + (Epsilon^2*(Pi^2 - 3*Log[2]^2))/24+
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[1,Epsilon/2-1,Epsilon,-1] :>
	Epsilon^(-1) + (1 - Log[2])/2 +
	(Epsilon*(Pi^2 + 6*Log[2] - 3*Log[2]^2))/24 + F21CHECK1 Epsilon^2
	,
	Hypergeometric2F1[1, Epsilon, 1 + Epsilon/2, x_] :>
	1 - Epsilon*Log[1 - x] +
	(Epsilon^2*(Log[1 - x]^2 - 2*PolyLog[2, x]))/4 +
	Epsilon^3 F21CHECK
	,
	Hypergeometric2F1[1, -1 + Epsilon, 1 + Epsilon/2, x_] :>
		1 - x + (Epsilon*(x + 2*(-1 + x)*Log[1 - x]))/2 +
		(Epsilon^2*(x + 2*(-1 + x)*Zeta2 +
					(1 - x)*Log[1 - x]*(2 + Log[1 - x] + 2*Log[x]) +
					(2 - 2*x)*PolyLog[2, 1 - x]))/4 +
	Epsilon^3 F21CHECK
	,
	HypergeometricPFQ[{1, opem_ - 1, Epsilon/2 + opem_},
			{opem_, opem_ + Epsilon}, 1] :>
	1 + (2*(-1 + opem))/Epsilon +
	(Epsilon*(-1 + opem)*PolyGamma[1, opem])/2 +
		Epsilon^2 F32CHECK1
	,
	HypergeometricPFQ[{1, opem_, Epsilon/2 + opem_},
										{1 + opem_, Epsilon + opem_}, 1] :>
		(2*opem)/Epsilon + (Epsilon*opem*
				(6*EulerGamma^2 + Pi^2 +
					6*PolyGamma[0, opem]*(2*EulerGamma + PolyGamma[0, opem]) +
					18*PolyGamma[1, opem] - 12*SumS[1, 1, -1 + opem]))/24 +
			Epsilon^2 F32CHECK2
							}//Dispatch;
(*
0 ==
-2*Pi^2 + 6*EulerGamma*x + 3*Pi^2*x + (Pi^2*x^2)/2 +
	Log[1 - x]*(-6 + 6*EulerGamma + Pi^2 +
		x*(12 - 12*EulerGamma - Pi^2 + 6*EulerGamma*x) +
		(-1 + x)*Log[1 - x]*(-6 + 3*x + Log[1 - x]) + 12*Log[x] +
		3*(2*(-3 + x)*x + (-1 + x)*Log[1 - x])*Log[x]) +
	6*(-2 + x)*(-1 + x)*PolyLog[2, 1 - x] + 6*(-1 + x)*PolyLog[3, 1 - x] +
	6*PolyLog[3, x] - 3*(1 - x)*x*
	sum[x^Nu*PolyGamma[0, Nu]^2, {Nu, 1, Infinity}] -
	9*(1 - x)*x*sum[x^Nu*PolyGamma[1, Nu], {Nu, 1, Infinity}] +
	6*Zeta[3] - 6*x*(PolyLog[3, x] + Zeta[3])

0 ==
(4*EulerGamma*Pi^2 + 6*EulerGamma^2*Log[1 - u] +
			Pi^2*Log[1 - u] + 6*EulerGamma*Log[1 - u]^2 + 2*Log[1 - u]^3 -
			24*EulerGamma*Log[1 - u]*Log[u] - 12*Log[1 - u]^2*Log[u] -
			24*EulerGamma*PolyLog[2, 1 - u] - 24*Log[1 - u]*PolyLog[2, 1 - u] +
			24*PolyLog[3, 1 - u] + 24*
				sum[(u^nu*PolyGamma[0, nu])/nu^2, {nu, 1, Infinity}] +
			6*sum[(u^nu*PolyGamma[0, nu]^2)/nu, {nu, 1, Infinity}] +
			6*sum[(u^nu*PolyGamma[1, nu])/nu, {nu, 1, Infinity}] - 24*Zeta[3]
)

*)

(*
Hallo Rolf,

hier wie versprochen. Trotzdem kannst du immer nochmal am Ende per
Entwicklung um y=1-x checken.

Rainer.



Hypergeometric2F1[a_, b_, c_, -(1-x_)^2/(4 x_)] :>
2^(2*b)*Gamma[c]/(Gamma[b] Gamma[c - b])*
Hold[Integrate][
	((1 - u)^(-1 - 2*b + 2*c)*u^(-1 + b)*(1 + u)^(1 + 2*a - 2*c))/
	((1 + u/x)^a*(1 + u*x)^a),
			{u, 0, 1}];

*)

FCPrint[1,"Series3.m loaded."];
End[]
