(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Gamma2*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

Gamma2::usage=
"Gamma2[x, y] is a special product of Gamma functions expanded up to order
Epsilon^3 when positive integer arguments are given.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Gamma2`Private`"]

(*
This is
Integrate[z^(Epsilon/2 + x) (1-z)^(Epsilon/2 + y),{z,0,1}]
*)

Options[Gamma2] = {EpsilonOrder -> 3};

Gamma2[x_ /;Head[x]=!=Integer, y_,___Rule] :=
(Gamma[Epsilon/2 + x]*Gamma[Epsilon/2 + y])/Gamma[Epsilon + x + y];

Gamma2[a_Integer, b_Integer, opt___Rule] :=
Which[(EpsilonOrder /. {opt} /. Options[Gamma2]) === 3,
			gamma2[a, b],
			(EpsilonOrder /. {opt} /. Options[Gamma2]) < 3 ,
			gamma2[a,b] /. Epsilon^3 -> 0,
			(EpsilonOrder /. {opt} /. Options[Gamma2]) > 3 ,
			SimplifyPolyGamma[Expand[
			Normal[(Gamma[Epsilon/2 + a]*Gamma[Epsilon/2 + b])/
							Gamma[Epsilon + a + b] +
							O[Epsilon]^(1+(EpsilonOrder /. {opt} /. Options[Gamma2]))
						]          ]      ]
		];

gamma2[1,-1] =
	-1 + 2/Epsilon + Epsilon*(1 - Zeta2/2) +
	Epsilon^3*(1 - Zeta2/4 - (9*Zeta2^2)/80 - Zeta[3]/4) +
	Epsilon^2*(-1 + Zeta2/4 + Zeta[3]/2);

gamma2[2,-1] =
	-3/2 + 2/Epsilon + Epsilon*(3/2 - Zeta2/2) +
	Epsilon^3*(3/2 - (3*Zeta2)/8 - (9*Zeta2^2)/80 - (3*Zeta[3])/8) +
	Epsilon^2*(-3/2 + (3*Zeta2)/8 + Zeta[3]/2);

gamma2[3,-1] =
	-11/6 + 2/Epsilon + Epsilon*(67/36 - Zeta2/2) +
	Epsilon^3*(607/324 - (67*Zeta2)/144 - (9*Zeta2^2)/80 - (11*Zeta[3])/24) +
	Epsilon^2*(-101/54 + (11*Zeta2)/24 + Zeta[3]/2);

gamma2[0,0] =
4/Epsilon - Epsilon*Zeta2 - (9*Epsilon^3*Zeta2^2)/40 + Epsilon^2*Zeta[3];

gamma2[1,0] =
2/Epsilon - (Epsilon*Zeta2)/2 - (9*Epsilon^3*Zeta2^2)/80 +
	(Epsilon^2*Zeta[3])/2;

gamma2[2,0] =
	-1 + 2/Epsilon + Epsilon*(1 - Zeta2/2) +
	Epsilon^3*(1 - Zeta2/4 - (9*Zeta2^2)/80 - Zeta[3]/4) +
	Epsilon^2*(-1 + Zeta2/4 + Zeta[3]/2)

gamma2[3,0] =
	-3/2 + 2/Epsilon + (Epsilon*(3 - Zeta2))/2 +
	(Epsilon^3*(1440 - 5*Pi^4 - 360*Zeta2 + 72*Zeta2^2 - 360*Zeta[3]))/960 -
	(Epsilon^2*(12 - 3*Zeta2 - 4*Zeta[3]))/8;

gamma2[1,1] =
1 - Epsilon + (Epsilon^2*(4 - Zeta2))/4 -
	(Epsilon^3*(4 - Zeta2 - Zeta[3]))/4;

gamma2[2,1] =
	1/2 - Epsilon/2 + (Epsilon^2*(4 - Zeta2))/8 -
	(Epsilon^3*(4 - Zeta2 - Zeta[3]))/8

gamma2[3,1] =
	1/3 - (13*Epsilon)/36 + (Epsilon^2*(40 - 9*Zeta2))/108 -
	(Epsilon^3*(484 - 117*Zeta2 - 108*Zeta[3]))/1296;

gamma2[0,1] =
	2/Epsilon - (Epsilon*Zeta2)/2 - (9*Epsilon^3*Zeta2^2)/80 +
	(Epsilon^2*Zeta[3])/2;

gamma2[0,-1]=
	-2 + 4/Epsilon - Epsilon*(1 + Zeta2) -
	(Epsilon^2*(1 - Zeta2 - 2*Zeta[3]))/2 -
	(Epsilon^3*(10 - 10*Zeta2 + 9*Zeta2^2 + 20*Zeta[3]))/40;

gamma2[-1,-1] =
	-4 + 8/Epsilon - 2*Epsilon*(1 + Zeta2) -
	Epsilon^2*(1 - Zeta2 - 2*Zeta[3]) -
	(Epsilon^3*(10 - 10*Zeta2 + 9*Zeta2^2 + 20*Zeta[3]))/20;

gamma2[0 ,yy_ /; yy > 0] := gx0[yy];
gamma2[xx_ /; xx > 0, 0] := gx0[xx];

grules = Dispatch[{PolyGamma[0,1] -> -EulerGamma,
									PolyGamma[1,1] -> Zeta2,
									PolyGamma[1,2] -> Zeta2-1,
									PolyGamma[2,1] -> (-2 Zeta[3]),
									PolyGamma[2,2] -> (2(1- Zeta[3])),
									Pi^2           -> (6 Zeta2)
								}];

toholdform[a_Plus] := Map[toholdform, a];
toholdform[b_Times]:= SelectNotFree[b,Epsilon] Apply[HoldForm,
											{SelectFree[b,Epsilon]}];
toholdform[c_ /; Head[c] =!= Plus && Head[c]=!=Times] := c;

simp[z_] := toholdform[Collect[Expand[z/.grules], Epsilon]];

gx0[y_] := simp[
2/Epsilon - PolyGamma[0, y] +
	(Epsilon*(Zeta2 + PolyGamma[0, y]^2 - 3*PolyGamma[1, y]))/4 +
(Epsilon^3*(27*Zeta2^2 + 30*Zeta2*PolyGamma[0, y]^2 + 5*PolyGamma[0, y]^4 -
			90*Zeta2*PolyGamma[1, y] - 90*PolyGamma[0, y]^2*PolyGamma[1, y] +
			135*PolyGamma[1, y]^2 - 20*PolyGamma[0, y]*PolyGamma[2, 1] +
			140*PolyGamma[0, y]*PolyGamma[2, y] - 75*PolyGamma[3, y]))/960 -
	(Epsilon^2*(3*Zeta2*PolyGamma[0, y] + PolyGamma[0, y]^3 -
9*PolyGamma[0, y]*PolyGamma[1, y] + 7*PolyGamma[2, y] + 2*Zeta[3]))/24
			];

gamma2[x_/; x>0, y_ /; y>0] :=
simp[
( (Gamma[x]*Gamma[y])/Gamma[x + y] +
	(Epsilon*Gamma[x]*Gamma[y]*(PolyGamma[0, x] + PolyGamma[0, y] -
			2*PolyGamma[0, x + y]))/(2*Gamma[x + y]) +
(Epsilon^2*Gamma[x]*Gamma[y]*
		((PolyGamma[0, x] + PolyGamma[0, y] - 2*PolyGamma[0, x + y])^2 +
			PolyGamma[1, x] + PolyGamma[1, y] - 4*PolyGamma[1, x + y]))/
	(8*Gamma[x + y]) +
(Epsilon^3*Gamma[x]*Gamma[y]*
		((PolyGamma[0, x] + PolyGamma[0, y] - 2*PolyGamma[0, x + y])*
			((PolyGamma[0, x] + PolyGamma[0, y] - 2*PolyGamma[0, x + y])^2 +
				3*(PolyGamma[1, x] + PolyGamma[1, y] - 4*PolyGamma[1, x + y])) +
			PolyGamma[2, x] + PolyGamma[2, y] - 8*PolyGamma[2, x + y]))/
	(48*Gamma[x + y])
			)];

FCPrint[1,"Gamma2.m loaded."];
End[]
