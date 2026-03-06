(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SplittingFunction *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 20 February '98 at 16:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:   Gribov-Lipatov-Altarelli-Parisi splitting functions *)

(* ------------------------------------------------------------------------ *)

SplittingFunction::usage=
"SplittingFunction[pxy] is a database of splitting functions in the
$\\overline{\\textrm{MS}}$ scheme.

SplittingFunction[\"Pqq\", x], SplittingFunction[\"Pqg\", x],
SplittingFunction[\"Pgq\", x]  and SplittingFunction[\"Pgg\", x] yield the
lowest order splitting functions.

SplittingFunction[\"PQQS\",x], SplittingFunction[\"PQQNS\",x] and
SplittingFunction[\"PQG\",x] are the next to leading order splitting
functions.

SplittingFunction has an option Polarization.

SplittingFunction[\"Pqq\", x, Polarization -> 0] returns the unpolarized and
SplittingFunction[\"Pqq\", x, Polarization -> 1] the polarized splitting
functions.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`SplittingFunction`Private`"]

Options[SplittingFunction] = {Polarization -> 1};

SplittingFunction[ijij_,ops___Rule] := SplittingFunction[ijij,FCGV["x"],ops];

SplittingFunction[ijij_, z_, ops___Rule] := Block[{re, pol, pld,ij},
ij = ToString[ijij];
pld = PlusDistribution;
pol = Polarization /. {ops} /. Options[SplittingFunction];
re =
If[pol === 1,
(* polarized case *)
	Which[
				ij === "PQQS",
													Tf CF (-16(1+z) Log[z]^2+(48z-16)Log[z]+16(1-z)),
				ij === "PQQPSU",
													CF Tf (
(8*(-10*(1 - z) - 4*(1 + z)*Log[z]))/Epsilon^2 +
	(8*(5*(1 - z) - 10*(1 - z)*Log[1 - z] - (7 - 5*z)*Log[z] -
			4*(1 + z)*Log[1 - z]*Log[z] - 3*(1 + z)*Log[z]^2 -
			4*(1 + z)*PolyLog[2, 1 - z]))/Epsilon +
	8*(-5*(1 - z) - (5*(1 - z)*Zeta2)/2 + 5*(1 - z)*Log[1 - z] -
		5*(1 - z)*Log[1 - z]^2 + ((5 - z)*Log[z])/2 - 3*(1 + z)*Zeta2*Log[z] -
		(7 - 5*z)*Log[1 - z]*Log[z] - 2*(1 + z)*Log[1 - z]^2*Log[z] -
		((11 - 5*z)*Log[z]^2)/4 - (1 + z)*Log[1 - z]*Log[z]^2 -
		(7*(1 + z)*Log[z]^3)/6 - 2*PolyLog[2, 1 - z] -
		4*(1 + z)*Log[1 - z]*PolyLog[2, 1 - z] -
		2*(1 + z)*Log[z]*PolyLog[2, 1 - z] + 2*(1 + z)*Log[z]*PolyLog[2, z] +
		4*(1 + z)*PolyLog[3, 1 - z] -
		2*(1 + z)*((Log[1 - z]*Log[z]^2)/2 + Log[z]*PolyLog[2, z] -
				PolyLog[3, z] + Zeta[3]))
																)
											,
(* this is (2.36) of N.P B359 (1991) 343-405*)
				ij === "PQQNS",
													Nf CF ( DeltaFunction[1-z] (-2/3-16/3 Zeta2)-
																	80/9 PlusDistribution[1/(1-z)] -
																	8/3 (1+z^2)/(1-z) Log[z]-8/9+88/9z
																) +
													CF^2 (  DeltaFunction[1-z] (3-24Zeta2+48Zeta[3])-
																	16 (1+z^2)/(1-z) Log[z] Log[1-z]-
																	4(1+z) Log[z]^2 -
																	8(2z+3/(1-z)) Log[z]-40 (1-z)
															) +
												CA CF ( DeltaFunction[1-z] (17/3+88/3 Zeta2-
																24 Zeta[3]) +
																(536/9-16Zeta2) PlusDistribution[1/(1-z)]+
																4 (1+z^2)/(1-z) Log[z]^2+8(1+z) Zeta2-
																4/3(5+5z-22/(1-z)) Log[z] + 4/9(53-187z)
															) -
(*
,
				ij === "PQQ-",
*)
											8 CF (CF - 1/2 CA) (
																(1+z^2)/(1+z) (Log[z]^2-4Log[z] Log[1+z]-
																								4 PolyLog[2,-z]-2 Zeta2
																							)+
																2(1+z) Log[z] + 4(1-z)
																					),
				ij === "PQG",
	4 CA Tf (48 - 44*z - 8*Zeta2 + (-16 + 16*z)*Log[1 - z] +
		(4 - 8*z)*Log[1 - z]^2 + (4 + 32*z)*Log[z] + (-4 - 8*z)*Log[z]^2 +
		(-8 - 16*z)*Log[z]*Log[1 + z] + (-8 - 16*z)*PolyLog[2, -z]) +
	4 CF Tf (-44 + 54*z + 8*Zeta2 - 16*z*Zeta2 + (16 - 16*z)*Log[1 - z] +
		(-4 + 8*z)*Log[1 - z]^2 - 18*Log[z] + (8 - 16*z)*Log[1 - z]*Log[z] +
		(-2 + 4*z)*Log[z]^2)
			,
				ij === "PGQ",
DOT[(CA*CF) , (328/9 + (280*z)/9 + 16*z*Zeta2 + (80/3 + (8*z)/3)*Log[1 - z] +
		(16 - 8*z)*Log[1 - z]^2 + (32 - 104*z)*Log[z] +
		(-32 + 16*z)*Log[1 - z]*Log[z] + (16 + 8*z)*Log[z]^2 +
		(32 + 16*z)*Log[z]*Log[1 + z] + (32 + 16*z)*PolyLog[2, -z])] +
	DOT[CF^2 , (-68 + 32*z + (-16 - 8*z)*Log[1 - z] + (-16 + 8*z)*Log[1 - z]^2 -
		(64 + 32*z)*Log[z] + (48 + 36*z)*Log[z] + (8 - 4*z)*Log[z]^2)] +
	DOT[(CF*Tf) , (-128/9 - (32*z)/9 + (-64/3 + (32*z)/3)*Log[1 - z])]
									,
				ij === "PGG",
(CA^2)  (-148/9 + 536/9 pld[1/(1 - z)] - (388*z)/9 +
		(-16 pld[1/(1 - z)] + 64*z + 16/(1 + z))*Zeta2 +
				(64*DeltaFunction[1 - z])/3 +
		(232/3 - (536*z)/3)*Log[z] +
		(-32 - 32/(1 - z) + 64*z)*Log[1 - z]*Log[z] +
		(32 + 8/(1 - z) - 8/(1 + z))*Log[z]^2 +
		(32 + 64*z + 32/(1 + z))*Log[z]*Log[1 + z] +
		(32 + 64*z + 32/(1 + z))*PolyLog[2, -z] +
		24*DeltaFunction[1 - z]*Zeta[3]) +
	(CA*Tf)  (-448/9 - 160/9 pld[1/(1 - z)] + (608*z)/9 -
		(32*DeltaFunction[1 - z])/3 + (-32/3 - (32*z)/3)*Log[z]) +
	(CF*Tf)  (-80 + 80*z - 8*DeltaFunction[1 - z] + (-80 + 16*z)*Log[z] +
		(-16 - 16*z)*Log[z]^2)
										,
				ij === "Pqq",
														CF ( 8 pld[1/(1-z)] - 4 - 4 z +
																6 DeltaFunction[1-z] ) ,
				ij === "Pqg",
														Tf (16 z - 8),
				ij === "Pgq",
														CF (8 - 4 z),
				ij === "Pgg",
														CA (8 pld[1/(1-z)] + 8 - 16 z +
																DeltaFunction[1-z] 22/3
															) - 8/3 Tf Nf DeltaFunction[1-z] ,
				ij === "aqq",
													CF (  - 4 PlusDistribution[Log[1-z]/(1-z)]
																-4 Log[z] PlusDistribution[1/(1-z)] +
																(  2 + 2 z ) Log[z (1-z)] -
																	4 + 2 z +  8 (1-z) +
																	DeltaFunction[1-z] (7 - 4 Zeta2)
															),
				ij === "agq",
													CF ((-4 + 2 z) Log[z (1-z)] + 2 - 4 z),
				ij === "agqd",
													CF ((-4 + 2 z) Log[z (1-z)] -2  ),
				ij === "aqg",
													Tf ( (4 - 8 z) Log[z (1-z)] -4 ),
				ij === "aqgd",
													Tf ( (4 - 8 z) Log[z (1-z)] -4 ),
				ij === "agg",
													CA ( -4 PlusDistribution[Log[z(1-z)]/(1-z)] +
																(8 z - 4) Log[z (1-z)] + 2 +
																DeltaFunction[1-z] (67/9 - 4 Zeta2)
															) - Tf DeltaFunction[1-z] (20/9),
				ij === "aggd",
													CA ( -4 PlusDistribution[Log[z(1-z)]/(1-z)] +
																(8 z - 4) Log[z (1-z)] + 2 +
																DeltaFunction[1-z] (67/9 - 4 Zeta2)
															) - Tf DeltaFunction[1-z] (20/9)
				],
(* unpolarized *)
	Which[ij === "Pqq",
														CF (8 pld[1/(1-z)] - 4 - 4 z +
																6 DeltaFunction[1-z]),
				ij === "Pqg",
														Tf (8 - 16 z + 16 z^2),
				ij === "Pgq",
														CF (8/z - 8 + 4 z),
				ij === "Pgg",
													8 CA (pld[1/(1-z)] + 1/z - 2 + z - z^2 +
																DeltaFunction[1-z] 11/12
															) - 8/3 Tf Nf DeltaFunction[1-z],
				ij === "aqq",
													CF (( 2 + 2z) Log[z(1-z)]-
																4 PlusDistribution[Log[z (1-z)]/(1-z)]-
																4 + 2 z + DeltaFunction[1-z] (7-4 Zeta2)
															) ,
				ij === "b_qq^(1)",
													CF ( -4 pld[Log[1-z]^2/(1-z)] -
																4 pld[Log[1-z]/(1-z)] +
																2 (1+z) Log[1-z]^2 +
																2 (1+z) Log[1-z] +
																DeltaFunction[1-z] (-4)
															),
				ij === "P_qq^{(1),(+)}",
													Nf CF Tf *
																(-160/9 pld[1/(1-z)] -
																	16/3 (1+z^2)/(1-z) Log[z] -
																	16/9 + 176/9 z +
																	DeltaFunction[1-z] (-4/3 - 32/3 Zeta2) ) +
													CF^2 (-16 (1+z^2)/(1-z) Log[z] Log[1-z] -
																	4 (1+z) Log[z]^2 - 16 z Log[z] -
																	24/(1-z) Log[z] - 40 (1-z) +
																	DeltaFunction[1-z]*
																	(3 - 24 Zeta2 + 48 Zeta3 ) ) +
													CA CF ( (536/9 - 16 Zeta2) pld[1/(1-z)] +
																	4 (1+z^2)/(1-z) Log[z]^2 +
																	8 (1+z) Zeta2 - 20/3 (1+z) Log[z] +
																	88/3 Log[z]/(1-z) + 212/9 - 748/9 z +
																	DeltaFunction[1-z]*
																	(17/3 + 88/3 Zeta2 - 24 Zeta3) ) ,
				ij === "P_qq^{(1),(-)}",
													8 CF (CF - CA/2) *
																( (1+z^2)/(1+z) *
																		(Log[z]^2 - 4 Log[z] Log[1+z] -
																		4 PolyLog[2, -z] - 2 Zeta2) +
																	2 (1+z) Log[z] + 4 (1-z) ) ,
				ij === "agq",
													CF ( (-4/z + 4 - 2 z) Log[z (1-z)] -
																4/z + 2 - 2 z),
				ij === "aqg",
													Tf ( (-4 + 8 z - 8 z^2) Log[z (1-z)]-4),
				ij === "cg1",
													Tf ( (4-8 z + 8 z^2) (Log[1-z]-Log[z])-
																4 + 32 z - 32 z^2
															)
(*
,
				ij === "agg",
*)

				]
	];
re];


FCPrint[1,"SplittingFunction.m loaded."];
End[]
