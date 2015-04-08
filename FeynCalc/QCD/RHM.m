(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: RHM*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  Gamma functions for (3C.22) and (3C.23) *)

(* ------------------------------------------------------------------------ *)

RHM::usage= "RHM[] is like RHI[], gives Gamma functions.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`RHM`Private`"]

Options[RHM] = {Momentum -> FCGV["p"]};

(* 3C.23*)

RHM[{a_,b_,c_,d_ /;d>0, e_},
		{al_, be_Integer?Positive, ga_, 0, ep_Integer?Positive}, opt___Rule
	] := Block[{p},
		p = Momentum /. {opt} /. Options[RHM];
-((Gamma[be + ep + -2 - Epsilon/2]*Gamma[2 + b - be + Epsilon/2]*
			Gamma[2 + e - ep + Epsilon/2]*
			Gamma[4 + a - al + b - be + e - ep + Epsilon]*
			Gamma[2 + c + Epsilon/2 - ga]*Gamma[-4 + al + be + ep - Epsilon + ga]*
			HypergeometricPFQ[{-d, 2 + b - be + Epsilon/2,
				4 + a - al + b - be + e - ep + Epsilon},
			{4 + b - be + e - ep + Epsilon,
				6 + a - al + b - be + c + e - ep + (3*Epsilon)/2 - ga}, 1]*
			Power2[SO[p], a + b + c + d]*Power2[-SP[p, p], Epsilon2]*
			Power2[SP[p, p], 4 - al - be - ep - ga])/
		(Gamma[ga]*Gamma[be]*Gamma[ep]*Gamma[al + be + ep + (-4 - Epsilon)/2]*
			Gamma[4 + b - be + e - ep + Epsilon]*
			Gamma[6 + a - al + b - be + c + e - ep + (3*Epsilon)/2 - ga]))
						];

RHM[{a_, b_, c_, 0, e_},
		{al_, be_Integer?Positive, ga_, 0, ep_Integer?Positive}, opt___Rule
		] := Block[{p},
	p = Momentum /. {opt} /. Options[RHM];
		-((Gamma[-2 + be + ep - Epsilon/2]*Gamma[2 + b - be + Epsilon/2]*
			Gamma[2 + e - ep + Epsilon/2]*
			Gamma[4 + a - al + b - be + e - ep + Epsilon]*
			Gamma[2 + c + Epsilon/2 - ga]*Gamma[-4 + al + be + ep - Epsilon + ga]*
			Power2[SO[p], a + b + c]*Power2[-SP[p, p], Epsilon2]*
			Power2[SP[p, p], 4 - al - be - ep - ga])/
		(Gamma[ga]*Gamma[be]*Gamma[ep]*Gamma[-2 + al + be + ep - Epsilon/2]*
			Gamma[4 + b - be + e - ep + Epsilon]*
			Gamma[6 + a - al + b - be + c + e - ep + (3*Epsilon)/2 - ga]))
							];

(* 3C.22*)
RHM[{a_Integer, b_Integer, c_Integer, d_Integer, e_Integer?Positive},
		{al_Integer?Positive, be_Integer?Positive, ga_Integer?Positive,
			de_Integer?Positive, 0
		}, opt___Rule
		] := Block[{p},
	p = Momentum /. {opt} /. Options[RHM];
Sum[-(((-1)^ie*Binomial[e, ie]*Gamma[-2 + be + de - Epsilon/2]*
			Gamma[2 + d - de + Epsilon/2]*Gamma[2 + c + Epsilon/2 - ga]*
			Gamma[-2 + al - Epsilon/2 + ga]*
			Gamma[2 + a - al + e + Epsilon/2 - ie]*
			Gamma[2 + b - be + Epsilon/2 + ie]*Power2[SO[p], a + b + c + d + e]*
			Power2[-SP[p, p], Epsilon2]*Power2[SP[p, p], 4 - al - be - de - ga])/
		(Gamma[al]*Gamma[be]*Gamma[de]*Gamma[ga]*
			Gamma[4 + a - al + c + e + Epsilon - ga - ie]*
			Gamma[4 + b - be + d - de + Epsilon + ie])), {ie, 0, e}]
							];

RHM[{a_Integer, b_Integer, c_Integer, d_Integer, 0},
		{al_Integer?Positive, be_Integer?Positive, ga_Integer?Positive,
			de_Integer?Positive, 0
		}, opt___Rule
		] := Block[{p},
	p = Momentum /. {opt} /. Options[RHM];
-((Gamma[be + de + (-4 - Epsilon)/2]*Gamma[2 + a - al + Epsilon/2]*
			Gamma[2 + b - be + Epsilon/2]*Gamma[2 + d - de + Epsilon/2]*
			Gamma[2 + c + Epsilon/2 - ga]*Gamma[al + (-4 - Epsilon)/2 + ga]*
			Power2[SO[p], a + b + c + d]*Power2[-SP[p, p], Epsilon2]*
			Power2[SP[p, p], 4 - al - be - de - ga])/
		(Gamma[al]*Gamma[be]*Gamma[de]*Gamma[4 + b - be + d - de + Epsilon]*
			Gamma[4 + a - al + c + Epsilon - ga]*Gamma[ga]))
							];

FCPrint[1,"RHM.m loaded"];
End[]
