(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CovariantD *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 16 March '98 at 11:52 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Covariant derivative *)

(* ------------------------------------------------------------------------ *)

CovariantD::usage =
"CovariantD[\[Mu]] is a generic covariant derivative with Lorentz index $\\mu$.

CovariantD[x, \[Mu]] is a generic covariant derivative with respect to $x^{\\mu
}$.

CovariantD[\[Mu], a, b] is a covariant derivative for a bosonic field that
acts on QuantumField[f, {}, {a, b}], where f is some field name and a and b
are two $SU(N)$ indices in the adjoint representation.

CovariantD[OPEDelta, a, b] is a short form for CovariantD[\[Mu], a, b]
FV[OPEDelta, \[Mu]].

CovariantD[{OPEDelta, a, b}, {n}] yields the product of n operators, where n
is an integer.   

CovariantD[OPEDelta, a, b, {m, n}] gives the expanded form of
CovariantD[OPEDelta, a, b]^m up to order $g^n$ for the gluon, where $n$ is an
integer and $g$ the coupling constant indicated by the setting of the option
CouplingConstant.

CovariantD[OPEDelta, {m, n}] gives the expanded form of CovariantD[OPEDelta]^m
up to order $g^n$ of the fermionic field. To obtain the explicit expression
for a particular covariant derivative, the option Explicit must be set to
True.";

DummyIndex::usage =
"DummyIndex is an option of CovariantD specifying an index to use as dummy
summation index. If set to Automatic, unique indices are generated.";

(* ------------------------------------------------------------------------ *)


Begin["`Package`"]
End[]

Begin["`CovariantD`Private`"]

DeclareNonCommutative[CovariantD];

Options[CovariantD] = { CouplingConstant -> SMP["g_s"],
						DummyIndex -> Automatic,
						Explicit -> False,
						FCPartialD -> RightPartialD,
						QuantumField -> GaugeField
						};

isunt[a_] :=
	FeynCalcInternal[I SUNT[a]];
subsit[un_][in_] :=
	If[ $Notebooks,
		Subscript[un,in],
		un[in]
	];

Unique2[x_] :=
	If[ $Notebooks === True,
		subsit[Unique[x]],
		Unique[x]
	];

CovariantD[Momentum[OPEDelta], a__] :=
	CovariantD[OPEDelta, a];

CovariantD[OPEDelta, a_, b_, {m_Integer?Positive,gc_Integer},
			ru___Rule ] :=
	Block[ {g},
		g  = CouplingConstant /. {ru} /. Options[CovariantD];
		Expand[Trick[DotSimplify[CovariantD[OPEDelta, a, b, {m},ru]]]
				] /. (g^w_ /; w > gc) :> 0
	];

CovariantD[OPEDelta, a_, b_, {m_Integer?Positive}, ru___Rule ] :=
	Block[ {sui, fui, i, aA, g,partial},
		aA = QuantumField /. {ru} /. Options[CovariantD];
		g  = CouplingConstant /. {ru} /. Options[CovariantD];
		partial = FCPartialD /. {ru} /. Options[CovariantD];
		sui = Table[ Unique2["c"], {m - 1}];
		fui = Table[ Unique2["e"], {m}];
		(DOT @@ Join[{SUNDelta[a, sui[[1]]] partial[Momentum[OPEDelta]] -
						g SUNF[a, sui[[1]], fui[[1]]] *
						QuantumField[ aA, Momentum[OPEDelta], SUNIndex[fui[[1]]] ]
					},
					Table[CovariantD[OPEDelta, sui[[i-1]], sui[[i]],
										QuantumField -> aA, CouplingConstant -> g,
										DummyIndex -> fui[[i]],
										FCPartialD -> partial,
										Explicit->True],
							{i, 2, m-1}
							],
					{SUNDelta[sui[[m-1]], b] partial[Momentum[OPEDelta]] -
					g SUNF[sui[[m-1]], b, fui[[m]] ] *
					QuantumField[ aA, Momentum[OPEDelta], SUNIndex[fui[[m]]] ]
					}
					]
		)/.subsit -> Identity
	];
(* /; (Explicit /. {ru} /. Options[CovariantD]); *)

$dummycount = 1;

CovariantD[al_, ru___Rule ] :=
	Block[ {aA, g, cC, du, partial},
		partial = FCPartialD /. {ru} /. Options[CovariantD];
		aA = QuantumField     /. {ru}  /. Options[CovariantD];
		g = CouplingConstant /. {ru}  /. Options[CovariantD];
		du = DummyIndex /. {ru}  /. Options[CovariantD];
		If[ du === Automatic,
			cC = Unique["c"],
			cC = du
		];
		(
			partial[al] - g I (DOT[SUNT[SUNIndex[cC]] ,
		If[ (al === OPEDelta) || (Head[al]=== Momentum),
			QuantumField[aA, Momentum[al], SUNIndex[cC]],
			QuantumField[aA, LorentzIndex[al], SUNIndex[cC]]
		]]                 )
		)] /;(Explicit /. {ru} /. Options[CovariantD]);

CovariantD[al_, a_, b_, ru___Rule ] :=
	Block[ {aA, cC, du, partial},
		aA = QuantumField     /. {ru}  /. Options[CovariantD];
		g = CouplingConstant /. {ru}  /. Options[CovariantD];
		partial = FCPartialD /. {ru} /. Options[CovariantD];
		du = DummyIndex /. {ru}  /. Options[CovariantD];
		If[ du === Automatic,
			cC = Unique["c"],
			cC = du
		];
		SUNDelta[a, b] partial[al] -
		g SUNF[a,b,cC] *
		If[ (al === OPEDelta) || (Head[al]=== Momentum),
			QuantumField[aA, Momentum[al], SUNIndex[cC]],
			QuantumField[aA, LorentzIndex[al], SUNIndex[cC]]
		]
	] /;
	(Explicit /. {ru} /. Options[CovariantD]) && Head[a]=!=SUNFIndex;



CovariantD[al_, a_SUNFIndex, b_SUNFIndex, OptionsPattern[]] :=
	Block[ {aA, cC, du, partial,g},
		aA = OptionValue[QuantumField];
		g = OptionValue[CouplingConstant];
		partial = OptionValue[FCPartialD];
		du = OptionValue[DummyIndex];
		If[ du === Automatic,
			cC = Unique["c"],
			cC = du
		];
		SUNFDelta[a, b] partial[al] -
		I g SUNTF[cC,a,b] *
		If[ (al === OPEDelta) || (Head[al]=== Momentum),
			QuantumField[aA, Momentum[al], SUNIndex[cC]],
			QuantumField[aA, LorentzIndex[al], SUNIndex[cC]]
		]
	] /; OptionValue[Explicit];


CovariantD[OPEDelta, a___,
			{m_ /; (Head[m] =!= Integer), n_Integer}, ru___Rule
			] :=
	Block[ {geen},
		(Sum[geen[j, m, a,
				QuantumField /. {ru} /. Options[CovariantD],
				CouplingConstant /. {ru} /. Options[CovariantD]
				], {j, 0, n}
			] /. geen -> gen[Join[{ru}, Options[CovariantD]]]
		)
	];

(* for the quarks *)
(* o is the order of g; m is like m *)
gen[ruli_List][o_, m_, aA_, g_] :=
	Block[ {partiaL},
		partiaL = FCPartialD /. ruli;
		( 	e = Unique2["e"];
			c = Unique2["c"];
			i = Unique2["i"];
			j = Unique2["j"];
			g^o Expand[Trick[
				If[ o=!=0,
					0,
					partiaL[OPEDelta]^m
				] +
				If[ (m o) === 0,
					0,
					If[ m === o,
						(-1)^o DOT[isunt[c[1]] ,  QuantumField[aA, Momentum[OPEDelta],
															SUNIndex[c[1]]
														] ,
						DOT@@ (Table @@
						{DOT[isunt[c[j]] ,
							QuantumField[aA, Momentum[OPEDelta], SUNIndex[c[j]]]],
							{j, 2, o}
						}    )],
						(-1)^o *
						(* wie kan dat in FORM doen ??? *)
						(Fold[summ, DOT @@ Join[{partiaL[OPEDelta]^i[1],
										DOT[isunt[c[1]] ,
										QuantumField[aA, Momentum[OPEDelta],
															SUNIndex[c[1]]
													]]
										},
										Flatten[Table[{partiaL[OPEDelta]^(i[j]-i[j-1]),
														DOT[isunt[c[j]] ,
														QuantumField[aA, Momentum[OPEDelta],
															SUNIndex[c[j]] ]]
														},
														{j, 2, o}
														]
												],
										{partiaL[OPEDelta]^(m-i[o]-o)}
										],
						Append[Table[{i[k], 0, i[k+1]}, {k,1,o-1}],
								{i[o],0,m-o}
							]
						] /. {i[1] :> OPEi, i[2] :> OPEj, i[3] :> OPEk, i[4] :> OPEl
							}
						)
					]
				]   ] /. summ -> opesum  ])
	];

opesum[a_, b__] :=
	NumericalFactor[a] OPESum[a/NumericalFactor[a],b];

(* o is the order of g; m is like m *)
gen[ruli_List][o_, m_, a_, b_, aA_, g_] :=
	Block[ {partiaL},
		partiaL = FCPartialD /. ruli;
	(		e = Unique2["e"];
			c = Unique2["c"];
			i = Unique2["i"];
			j = Unique2["j"];
			g^o Expand[Trick[
				If[ o=!=0,
					0,
					SUNDelta[a, b] partiaL[OPEDelta]^m
				] +
				SUNDelta[a, e[0]] SUNDelta[b, e[o]] *
				If[ (m o) === 0,
					0,
					If[ m === o,
						(-1)^o SUNF[e[0], e[1], c[1]] *
										QuantumField[aA, Momentum[OPEDelta],
															SUNIndex[c[1]]
													]*
						Product @@ {SUNF[e[j-1], e[j], c[j]]*
									QuantumField[aA,Momentum[OPEDelta],SUNIndex[c[j]]],
									{j, 2, o}
									},
						(-1)^o *
						(* wie kan dat in FORM doen ??? *)
						(Fold[summ, DOT @@ Join[{partiaL[OPEDelta]^i[1],
										SUNF[e[0], e[1], c[1]] *
										QuantumField[aA, Momentum[OPEDelta],
															SUNIndex[c[1]]
													]
										},
										Flatten[Table[{partiaL[OPEDelta]^(i[j]-i[j-1]),
														SUNF[e[j-1], e[j], c[j]]*
														QuantumField[aA, Momentum[OPEDelta],
															SUNIndex[c[j]] ]
														},
														{j, 2, o}
														]
												],
										{partiaL[OPEDelta]^(m-i[o]-o)}
										],
						Append[Table[{i[k], 0, i[k+1]}, {k,1,o-1}],
								{i[o],0,m-o}
							]
						] /. {i[1] :> OPEi, i[2] :> OPEj, i[3] :> OPEk, i[4] :> OPEl
							}
						)
					]
				]   ] /. summ -> opesum       ])
	];

CovariantD /:
	MakeBoxes[CovariantD[mud_], TraditionalForm] :=
		RowBox[{SubscriptBox["D",TBox[mud]]}];

CovariantD /:
	MakeBoxes[CovariantD[mud_, a_, b_], TraditionalForm] :=
		SubsuperscriptBox["D", TBox[mud], TBox[a, b]]/; Head[mud] =!= List;

CovariantD /:
	MakeBoxes[CovariantD[x_, LorentzIndex[mu__]], TraditionalForm] :=
		RowBox[{"\[PartialD]", "/", "D", SuperscriptBox[ToBoxes[x,TraditionalForm], ToBoxes[LorentzIndex[mu],TraditionalForm]]}];

FCPrint[1,"CovariantD.m loaded."];
End[]
