(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SquareAmplitude *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 21 December '98 at 0:52 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: square amplitudes *)

(* ------------------------------------------------------------------------ *)

EnergyMomentumConservation::usage =
"EnergyMomentumConservation is an option of SquareAmplitude (experimental).";

SquareAmplitude::usage =
"SquareAmplitude[amp] squares the amplitude amp. EXPERIMENTAL!!! \
Don't rely on it.";

SpinSumExternalMomentum::usage =
"SpinSumExternalMomentum is an option of SquareAmplitude (experimental).";

SelectedGraphs::usage =
"SelectedGraphs is an option of SquareAmplitude (experimental).";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`SquareAmplitude`Private`"]

(* ************************************************************* *)
duM = Unique[System`C];
gamcount[x_] :=
	Length[Position[x, DiracGamma]];

specsir[a_] :=
	Block[ {tem, nosp},
		nosp = Select[a, FreeQ[#, Spinor]&]/.$MU-> duM;
		tem = (a/nosp)/.$MU-> duM;
		If[ !FreeQ[tem, duM[2]],
			tem = Contract[DiracOrder[tem,
							{duM[1], Momentum[_], duM[2]}
										]//DiracSimplify]
		];
		If[ Length[Position[tem, Spinor]] > 2,
			FCPrint[2,"entering specsir2 with ",tem//FeynCalcForm];
			tem = specsir2[tem];
			FCPrint[2,"exiting specsir2 with", tem//FeynCalcForm];
		];
		Expand[tem nosp]
	];
ChisholmSave[x_] :=
	MemSet[ChisholmSave[x], Chisholm[x]];
EpsChisholmSave[x_] :=
	MemSet[EpsChisholmSave[x], EpsChisholm[x]];
specsir2[x_] :=
	specsir2[x] =
	Block[ {ste, ste1, ste2,ste1r,ste2r,duMy},
		ste = x /. $MU -> duMy;
	(* there are two possibilities ... *)
		ste1r = {DOT[Spinor[pe1__] , g1__ , Spinor[pe2__]] *
				DOT[Spinor[pe3__] , g2__ , Spinor[pe4__]] *
				DOT[Spinor[pe5__] , g3__ , Spinor[pe6__]] :>
		(Expand[DiracSimplify[
			DiracOrder[DiracSimplify[Contract[DOT[Spinor[pe1] , g1 , Spinor[pe2]] *
				EpsChisholmSave[ DOT[Spinor[pe3] , g2 , Spinor[pe4]] *
									ChisholmSave[DOT[Spinor[pe5] , g3 , Spinor[pe6]]]
								] ] ], {$MU[1], Momentum[_], $MU[2]} ]
				]] /. $MU -> duMy
		)/;
			(Length[{g1}] < 3) && (Length[{g2}] < 3) && (Length[{g3}] > 2)
				};
		ste2r = {DOT[Spinor[pe1__] , g1__ , Spinor[pe2__]] *
				DOT[Spinor[pe3__] , g2__ , Spinor[pe4__]] *
				DOT[Spinor[pe5__] , g3__ , Spinor[pe6__]] :>
			(Expand[DiracSimplify[
				DiracOrder[DiracSimplify[
					Contract[DOT[Spinor[pe3] , g2 , Spinor[pe4]] *
							EpsChisholm[ DOT[Spinor[pe1] , g1 , Spinor[pe2]] *
										Chisholm[DOT[Spinor[pe5] , g3 , Spinor[pe6]]]
								] ] ],{$MU[1], Momentum[_], $MU[2]} ]
					]] /. $MU -> duMy
			)/;
				(Length[{g1}] < 3) && (Length[{g2}] < 3) && (Length[{g3}] > 2)
				};
		ste1 = ste/.ste1r;
		If[ (!FreeQ[ste1, DOT[DiracMatrix[duMy[_]],DiracGamma[Momentum[_]],
						DiracMatrix[duMy[_]]]
				]) && (ste1=!= ste),
			ste1 = ste1 /. ste2r /. ste1r
		];
		If[ (ste1 =!= 0) &&
			(!FreeQ[ste1, DOT[DiracMatrix[duMy[_]],DiracGamma[Momentum[_]],
								DiracMatrix[duMy[_]]]
			]),
			ste2 = ste /. ste2r;
			If[ (!FreeQ[ste2, DOT[DiracMatrix[duMy[_]],DiracGamma[Momentum[_]],
							DiracMatrix[duMy[_]]]
					]) && (ste2=!= ste),
				ste2 = ste2 /. ste1r /. ste2r
			],
			ste2 = ste1
		];
		If[ ste1 =!= 0,
			ste1 = DiracSimplify[DiracOrder[DiracSimplify[ste1]]]//Expand;
			ste2 = DiracSimplify[DiracOrder[DiracSimplify[ste2]]]//Expand;
			If[ !FreeQ[ste1, DOT[DiracMatrix[duMy[_]],DiracGamma[Momentum[_]],
						DiracMatrix[duMy[_]]]],
				ste1 = First[Sort[Select[{ste1, ste2}, FreeQ[#,RuleDelayed]&],
						(LeafCount[{##}[[1]]] < LeafCount[{##}[[2]]] )& ] ]
			];
		];
		ste1
	];
(* end of specsir2 *)

(* **************************************************************** *)
(* SquareAmplitudedef *)
(* **************************************************************** *)
(* careful: this function is still under development!!! *)

(* ----------------------------------------------------------------- *)
susa[xxx_] :=
	susa[xxx] = SUNSimplify[xxx, Factoring->True];
(* /. SUNTrace -> ST; *)
susm[nuLL] :=
	0;
susm[xx_Plus] :=
	Map[susm, xx];
susm[xx_ /; FreeQ[xx,SUNIndex]] :=
	xx;
susm[xx_ /;( (Head[xx] =!= Plus) && (!FreeQ[xx, SUNIndex]))
	] :=
	Expand[
	Select[xx, FreeQ[#1, SUNIndex] & ]*
	susa[Select[xx, !FreeQ[#1, SUNIndex] & ]] (*, SUNIndex*)];

Options[SquareAmplitude] = {
					Dimension -> 4,
				EnergyMomentumConservation -> {},
				EpsDiscard -> False,
				ExtraFactor -> 1,
					Collecting -> False,
					Factoring -> False,
					FinalSubstitutions -> {},
					InitialSubstitutions -> {},
					IntermediateSubstitutions -> {},
					IsolateNames -> KK,
(*                  IsolateSplit -> 4711 I,*)
					Mandelstam -> {FCGV["s"], FCGV["t"]},
					SelectedGraphs -> All,
				SpinPolarizationSum -> 1,
				SpinSumExternalMomentum -> Automatic,
					WriteOut -> False
							};

(* the FeynArts convention is to  put the Process -> {}  rule
	as the last argument in FeynAmpList
*)
SquareAmplitude[ FeynAmpList[he___,
				process_Rule][amps__],
	opts___Rule] :=
	Block[ {sli = {}, amp = FeynCalcInternal[{amps}], proctype, dim, nam,
	colorpart = 1,pluiso,gluON,den1,numfaN,enmomsubst,
	enmomback,inisubst,pli,
	mand,factoring,exmom,exm, es,te, sel, scalP,
	p1,p2,k1,k2,k3,k4,m12,m22,m32,m42,m52,m62,
	finsubst,proc,extrafact},

		If[	!FreeQ2[{he,process}, FeynCalc`Package`NRStuff],
			Message[FeynCalc::nrfail];
			Abort[]
		];

		dim       = Dimension   /. {opts} /. Options[SquareAmplitude];
		mand      = Mandelstam  /. {opts} /. Options[SquareAmplitude];
		inisubst  = FeynCalcInternal[InitialSubstitutions /.  {opts} /. Options[SquareAmplitude]];
		collecting = Collecting /. {opts} /. Options[SquareAmplitude];
		factoring = Factoring   /. {opts} /. Options[SquareAmplitude];
		finsubst  = FeynCalcInternal[FinalSubstitutions /. {opts} /. Options[SquareAmplitude]];
		isolhead  = IsolateNames /. {opts} /. Options[SquareAmplitude];
		exmom     = SpinSumExternalMomentum /. {opts} /. Options[SquareAmplitude];
		extrafact = ExtraFactor /. {opts} /. Options[SquareAmplitude];
		extrafact = FeynCalcInternal[extrafact];
		{es, te} = {mand[[1]], mand[[2]]};
		If[ Length[mand === 3],
			uu = mand[[3]]
		];
		amp = amp /. SUNDelta -> SUNDeltaContract /.
					SUNDeltaContract -> SUNDelta;
		sel = SelectedGraphs /. {opts} /. Options[SquareAmplitude];
		If[ sel === All,
			sli = amp,
			For[jj = 1, jj <= Length[amp], jj++,
			If[ MemberQ[sel, jj],
				AppendTo[sli, amp[[jj]]]
			] ]
		];
		plsI[xx__] :=
			Isolate[Plus[xx], {DiracGamma, Spinor, LorentzIndex, SUNIndex},
							IsolateNames -> isolhead, IsolateSplit->555I];

		(* *********************************** *)
		{es, te} = {mand[[1]], mand[[2]]};
		If[ Length[mand===3],
			uu = mand[[3]]
		];
		proc  = Last[process];
		proctype = Map[Length, proc];
		Which[
				proctype === (2 -> 1),
				proc = proc[[1]] -> Join[proc[[2]], {{0,0,0}, {0,0,0}, {0,0,0}}],
				proctype === (1 -> 2),
				proc = Join[proc[[1]], {{0,0,0}}] ->
						Join[proc[[2]], {{0,0,0}, {0,0,0}, {0,0,0}}],
				proctype === (2 -> 2),
				proc = proc[[1]] -> Join[proc[[2]], {{0,0,0}, {0,0,0}}],
				proctype === (2 ->3),
				proc = proc[[1]] -> Join[proc[[2]], {{0,0,0}}]
			];
		p1m1 = {proc[[1, 1, 2]], proc[[1, 1, 3]]^2};
		p2m2 = {proc[[1, 2, 2]], proc[[1, 2, 3]]^2};
		k1m3 = {proc[[2, 1, 2]], proc[[2, 1, 3]]^2};
		k2m4 = {proc[[2, 2, 2]], proc[[2, 2, 3]]^2};
		k3m5 = {proc[[2, 3, 2]], proc[[2, 3, 3]]^2};
		k4m6 = {proc[[2, 4, 2]], proc[[2, 4, 3]]^2};
		fields = { proc[[1,1,1]], proc[[1,2,1]],
					proc[[2,1,1]], proc[[2,2,1]], proc[[2,3,1]], proc[[2,4,1]]
					};

		(*
		(* this has to be thought over again ... *)
		extrafact = extrafact ( NF^0 (*( (Length[Position[fields, tmp`F[I]]] +
									Length[Position[fields, tmp`F[-I]]])/2
									)*)
								);
		*)
		extrafact = ExpandScalarProduct[extrafact]//Expand;
		{p1, p2, k1, k2, k3, k4} = #[[1]]& /@ {p1m1, p2m2, k1m3, k2m4, k3m5, k4m6};
		{m12, m22, m32, m42, m52, m62} = #[[2]]& /@
												{p1m1, p2m2, k1m3, k2m4, k3m5, k4m6};
		If[ MemberQ[{1 -> 2, 2 -> 1, 2 -> 2}, proctype],
			enmomsubst = k1 -> p1+p2-k2-k3-k4;
			enmomback = {p1+p2-k2-k3-k4->k1,
						Momentum[p1]+Momentum[p2]-Momentum[k2]-
						Momentum[k3]-Momentum[k4] -> Momentum[k1],
						-Momentum[p1]-Momentum[p2]+Momentum[k2]+
						Momentum[k3]+Momentum[k4] -> (-Momentum[k1])
						},
			enmomsubst = {};
			enmomback  = {};
		];

		(* Oh je *)
		pair2 = FeynCalc`Contract`Private`pair2;
		scalP[a_, b_, c_] :=
			If[ dim =!= 4,
				Apply[ Set, {ScalarProduct[a,b, Dimension -> dim], c//Expand}];
				Apply[ Set, {ScalarProduct[a,b, Dimension -> 4], c//Expand} ];
				Apply[ Set, {pair2[Momentum[a, dim], Momentum[b, dim]], c//Expand} ];
				Apply[ Set, {pair2[Momentum[a ], Momentum[b ]], c//Expand} ],
				Apply[ Set, {ScalarProduct[a,b, Dimension -> 4], c//Expand} ];
				Apply[ Set, {pair2[Momentum[a], Momentum[b]], c//Expand}];
			];
		sCP[a_] :=
			ScalarProduct[a, a, Dimension -> dim];
		sCP[a_,b_] :=
			ScalarProduct[a, b, Dimension -> dim];
		If[ Head[Pair[Momentum[p1], Momentum[p1]]] === Pair,
			If[ proctype  === (1 -> 2),
				scalP[p1, p1, m12];
				scalP[k1, k1, m32];
				scalP[k2, k2, m42];
				scalP[k1, k2, 1/2 (sCP[p1] - sCP[k1] - sCP[k2])];
				scalP[p1, k1, 1/2 (sCP[p1] - sCP[k2] + sCP[k1])];
				scalP[p1, k2, 1/2 (sCP[p1] - sCP[k1] + sCP[k2])];
			];
			If[ proctype  === (2 -> 1),
				scalP[p1,p1, m12];
				scalP[p2,p2, m22];
				scalP[k1,k1, m32];
				scalP[p1,p2, 1/2 (sCP[k1] - sCP[p1] - sCP[p2])];
				scalP[p1,k1,-1/2 (sCP[p2] - sCP[p1] - sCP[k1])];
				scalP[p2,k1,-1/2 (sCP[p1] - sCP[p2] - sCP[k1])];
			];
			uUu = Expand[m12 + m22 + m32 + m42 - es - te];
			If[ proctype === (2 -> 2),
				scalP[p1, p1, m12];
				scalP[p1, p2, - m12/2 - m22/2 + es/2];
				scalP[p1, k1,   m12/2 + m32/2 - te/2];
				scalP[p1, k2,   m12/2 + m42/2 - uUu/2];
				scalP[p2, p2, m22];
				scalP[p2, k1,  m22/2 + m32/2 - uUu/2];
				scalP[p2, k2,  m22/2 + m42/2 - te/2];
				scalP[k1, k1, m32];
				scalP[k1, k2, -m32/2 - m42/2 + es/2];
				scalP[k2, k2, m42];
			];
			If[ proctype === (2 ->3),
				scalP[p1, p1, m12];
				scalP[p2, p2, m22];
				scalP[k1, k1, m32];
				scalP[k2, k2, m42];
				scalP[k3, k3, m52];
				scalP[p1, p2, tmp`P[p1, p2]/2 - m12/2 - m22/2];
				scalP[p1, k1,-tmp`P[p1,-k1]/2 + m12/2 + m32/2];
				scalP[p1, k2,-tmp`P[p1,-k2]/2 + m12/2 + m42/2];
				scalP[p1, k3,-tmp`P[p1,-k3]/2 + m12/2 + m52/2];
				scalP[p2, k1,-tmp`P[p2,-k1]/2 + m22/2 + m32/2];
				scalP[p2, k2,-tmp`P[p2,-k2]/2 + m22/2 + m42/2];
				scalP[p2, k3,-tmp`P[p2,-k3]/2 + m22/2 + m52/2];
				scalP[k1, k2, tmp`P[k1, k2]/2 - m32/2 - m42/2];
				scalP[k1, k3, tmp`P[k1, k3]/2 - m32/2 - m52/2];
				scalP[k2, k3, tmp`P[k2, k3]/2 - m42/2 - m52/2];
			];
		];

		(*
		If[(k3 =!= 0) && (k4 =!= 0),
			SetMandelstam[tmp`P,  {p1, p2, -k1, -k2, -k3, -k4},
									{p1m1[[2]], p2m2[[2]], k1m3[[2]],
										k2m4[[2]], k3m5[[2]], k4m6[[2]]}
						]
			];
		*)

		(* *********************************** *)

		(* sum the amplitudes *)
		amp = Sum[ propdenexp[Last[sli[[i]]],
							{p1,p2,-k1,-k2,-k3,-k4}], {i,1, Length[sli]}
				] /. inisubst;
		amp0 = amp;
		If[ dim =!= 4,
			amp = ChangeDimension[amp, dim];
			extrafact = ChangeDimension[extrafact, dim];
		];

		(*tmp`am = amp;*)
		FCPrint[2,"amp = ",amp];
		FCPrint[1,"extrafact = ",extrafact];
		If[ !FreeQ[amp, DiracGamma],
			amp = DiracSimplify[amp /. enmomsubst /.
								Pair -> pPpPpP, Expanding -> False
								] /. enmomback /. pPpPpP->Pair,
			amp = ExpandScalarProduct[Contract[amp/.enmomsubst]] /. enmomback
		];
		FCPrint[1,"checK"];
		sund[xxxx_] :=
			1;
		sund[glu_, xxxx_] :=
			SUNDeltaContract[glu, SUNIndex[xxxx]];
		gluON[] = Sequence[];
		mU = Unique[System`C];
		sU = Unique[System`C];
		gL = Unique[System`C];
(*
mU = Unique[mUUn]; sU = Unique[sUUu]; gL = Unique[glLLu];
*)
		gluON[_SUNIndex] = gL;
		FCPrint[1,"Length of amp = " , Length[amp]];
		(* a list of rules for substituting indices for the polarization momenta *)
		momtolor =
		{Momentum[Polarization[p1, _, glui___SUNIndex], ___] :>
					(LorentzIndex[mU[1, gluON[glui]],dim] sund[glui, sU[1]]),
			Momentum[Polarization[p2, _, glui___SUNIndex], ___] :>
					(LorentzIndex[mU[2, gluON[glui]],dim] sund[glui, sU[2]]),
			Momentum[Polarization[k1, _, glui___SUNIndex], ___] :>
					(LorentzIndex[mU[3, gluON[glui]],dim]  sund[glui, sU[3]]),
			Momentum[Polarization[k2, _, glui___SUNIndex], ___] :>
					(LorentzIndex[mU[4, gluON[glui]],dim]  sund[glui, sU[4]]),
			Momentum[Polarization[k3, _, glui___SUNIndex], ___] :>
					(LorentzIndex[mU[5, gluON[glui]],dim]  sund[glui, sU[5]]),
			Momentum[Polarization[k4, _, glui___SUNIndex], ___] :>
					(LorentzIndex[mU[6, gluON[glui]],dim]  sund[glui, sU[6]])
		};
		pairdel[ c___,  n_ b_LorentzIndex, d___] :=
			pairdel[c,n b, d] = n pairdel[c,b, d];
		dirdel[n_ b_LorentzIndex, d___] :=
			dirdel[n b, d] = n DiracGamma[b, d];
		If[ !FreeQ[amp, SUNIndex],
			(* collecting w.r.t. to SUNIndex *)
			FCPrint[1,"collecting w.r.t. SUNIndex"];
			If[ $FeynContract === True,
				pair2PAIR[a_, b_] :=
					If[ FreeQ[{a,b}, SUNIndex],
						tmp`PAIR[a,b],
						Pair[a, b]
					];
				FCPrint[1,"substing PAIR"];
				amp = amp /. Pair -> pair2PAIR;
				amp00 = amp;
				FCPrint[1,"substing PAIR done"];
				amp = tmp`CCollect[amp, {Pair, SUNF, SUNT, SUNDelta}];
				FCPrint[1,"back from CCollect"];
				amp = amp /. tmp`PAIR -> Pair,
				amp = Collect2[amp, SUNIndex,Factoring -> False];
			];
			amp11 = amp;
			FCPrint[1,"SUNSimplifying"];
			amp = SUNSimplify[amp, Explicit -> False];
			If[ !FreeQ[amp,SUNF],
				amp = SUNSimplify[amp, Explicit -> True]
			];
			If[ !FreeQ[amp, SUNTrace],
				amp = Collect2[amp, SUNTrace]
			];
		];
		oldamp = amp;
		amp = amp /. momtolor;
		If[ !FreeQ[amp, SUNIndex],
			FCPrint[2,"pairdel"];
			amp = amp/. Pair -> pairdel /. pairdel->Pair/.
						DiracGamma->dirdel/.dirdel->DiracGamma;
			amp = ChangeDimension[amp, dim];
			FCPrint[2,"contracting"];
			amp = Contract[ amp/.enmomsubst, Expanding -> False ];
			amp = ExpandScalarProduct[amp] /. enmomback;
			(*
			amp = ChangeDimension[amp, dim];
			*)
			FCPrint[1,"collecting w.r.t. SUNIndex"];
			If[ $FeynContract === True,
				FCPrint[1,"substing PAIR( 2 )"];
				amp = amp /. Pair -> pair2PAIR;
				FCPrint[1,"substing PAIR( 2 ) done"];
				amp00 = amp;
				amp = tmp`CCollect[amp, {Pair, SUNF, SUNT, SUNDelta}];
				FCPrint[1,"back from CCollect"];
				amp = amp /. tmp`PAIR -> Pair,
				amp = Collect2[amp, SUNIndex,Factoring -> False];
			];
			amp22 = amp;
			amppp = amp;
			If[ Head[amp] === Times,
				colorpart = Select[amp, !FreeQ[#, SUNIndex]&];
				amp = Select[amp, FreeQ[#, SUNIndex]&],
				colorpart = 1;
				iii = 0;
			(*
				amp = Map[#/.Plus-> plUUU&, amp + nuLL1 + nuLL2];
				amp = Sum[(Print[iii++];susa[amp[[ij]]]), {ij, Length[amp]}];
				amp = Expand[amp /. nuLL1 -> 0 /. nuLL2->0,SUNIndex] /. plUUU -> Plus;
			*)
				];
			If[ !FreeQ[colorpart, SUNIndex],
				FCPrint[2,"colorpart = ",colorpart]
			];
		];
		amp44 = amp;
		FCPrint[2,"Length of amp44 = ",Length[amp44]];
		FCPrint[2,"leafcount of amp44 = ",LeafCount[amp44]];
		If[ Head[amp44] === Plus,
			namp = 0;
			lamp44 = Length[amp44];
			For[ia = 1, ia <= lamp44, ia++,
				FCPrint[2,"contracting amp44[[",ia,"]]  out of ", lamp44];
				namp = namp + ExpandScalarProduct[Contract[amp44[[ia]]]];
				];
			amp = namp,
			amp = Contract[amp]//ExpandScalarProduct;
		];
		amp45 = amp;
		FCPrint[2,"Length of amp45 = ",Length[amp45]];
		nuLL = Unique[System`C];
		(*
		nuLL = Unique[nuLlllllL];
		*)
		collect4[x_ /; (Head[x] =!= Plus), _] :=
			x;
		collect4[su_Plus, a_Symbol] :=
			Block[ {suntr, ll, j = 0, vp, new = 0,cc,
					temp = su},
				FCPrint[1,"this cridder is  ", Length[temp], "  terms long"];
				While[ (ll = Length[temp]) > 0,
						j++;
						FCPrint[2,"j = ",j, "    length of temp = ",ll];
						If[ Head[temp] === Plus,
							part1 = First[temp],
							part1 = temp
						];
						vp = Select[part1 DUM, !FreeQ[#, a]&];
						FCPrint[2,"vp = ", vp//FeynCalcForm];
						cc = Coefficient[temp, vp];
						new = new + vp cc;
						temp = temp - ((#vp& /@ (cc + nuLL))/.nuLL->0)
					];
				new
			];
		If[ $FeynContract === True,
			FCPrint[1,"substing PAIR( 3 )"];
			amp = amp /. Pair -> pair2PAIR;
			FCPrint[1,"substing PAIR( 3 ) done"];
			amp00 = amp;
			amp = tmp`CCollect[amp, {Pair, SUNF, SUNT, SUNDelta}];
			FCPrint[1,"back from CCollect"];
			amp = amp /. tmp`PAIR -> Pair,
			amp = Collect2[amp, SUNIndex,Factoring -> False];
		];
		amp55 = amp;

		(* CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC *)
		(* construct the tensor from the polarization sums *)
		(* CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC *)
		pli = {p1, p2, k1, k2, k3, k4};
		pli2 = ExpandScalarProduct[ScalarProduct[#, #]]& /@ pli;
		prod = extrafact;
		exm = {0,0,0,0,0,0};
		If[ exmom =!= Automatic,
			If[ Head[exmom] === List,
				exm = exmom
			];
			If[ (Head[exmom] === Symbol) || (exmom === 0),
				exm = {exmom, exmom,exmom,exmom,exmom,exmom}
			];
		];
		(*
		mom4set[xp_Symbol] := Apply[SetDelayed, {Momentum[xp,___], Momentum[xp]}];
		*)
		mom4set[xp_Symbol] :=
			Apply[Set, {Momentum[xp,___], Momentum[xp]}];
		Map[mom4set, exm];
		FCPrint[2,"exm = ",exm];
		FCPrint[2,"fields = ", fields];
		For[ij = 1, ij < 7, ij++,
			If[ pli =!= 0,
			(* check for photon *)
			(*Dropped ComplexIndex. This should be adjusted accordingly. F.Orellana, 20/2-2003*)
				If[ (fields[[ij]] === tmp`V[1]) &&  !FreeQ[amp, mU[ij]],
					prod = prod (- MetricTensor[mU[ij], ComplexIndex[mU[ij]],
										Dimension -> dim])
				];
				(* check for gluon*)
				If[ (fields[[ij]] === tmp`G[1]) && !FreeQ[amp, mU[ij,gL]],
					If[ exmom === Automatic,
						Which[
							proctype === (2->3),
							exm = {pli[[2]], pli[[1]], pli[[5]], pli[[3]], pli[[4]]},
							proctype === (2->2),
							exm = {pli[[2]], pli[[1]], pli[[4]], pli[[3]]},
							proctype === (2->1),
							exm = {pli[[3]], pli[[1]], pli[[2]]},
							proctype === (1->2),
							exm = {pli[[3]], pli[[1]], pli[[2]]}
							];
					];
					prod = prod PolarizationSum[mU[ij, gL], ComplexIndex[mU[ij, gL]],
												pli[[ij]], exm[[ij]], Dimension -> dim];
				];
				(* check for massive Vectorbosons *)
				If[ MatchQ[fields[[ij]],
							tmp`V[iii_ /; iii=!=1]] && !FreeQ[amp, mU[ij]],
					prod = prod PolarizationSum[mU[ij], ComplexIndex[mU[ij]],
									pli[[ij]], Dimension -> dim]
				];
				(* check for ghosts *)
				If[ MatchQ[fields[[ij]], tmp`U[_]],
					Print["GHOSTCHECK"];
					prod = I prod
				];
			];
	];
		FCPrint[1,"polarization sums =  ", prod//FeynCalcForm];
		If[ dim =!= 4,
			prod = prod/.{Pair[LorentzIndex[a_], LorentzIndex[b_]]:>
						Pair[LorentzIndex[a, dim], LorentzIndex[b, dim]],
						Pair[LorentzIndex[a_], Momentum[b_]]:>
						Pair[LorentzIndex[a, dim], Momentum[b, dim]],
						Pair[Momentum[a_], Momentum[b_]]:>
						Pair[Momentum[a, dim], Momentum[b,dim]],
						DiracGamma[LorentzIndex[a_]] :>
						DiracGamma[LorentzIndex[a,dim],dim],
						DiracGamma[Momentum[a_]] :>
						DiracGamma[Momentum[a, dim], dim]
						};
		];
		amp = Contract[ amp, Expanding -> False ];
		(*
		pluli[x__] := Collect2[ Plus[x], LorentzIndex , Factoring -> False ];
		*)
		pluli[x__] :=
			Collect2[ Plus[x], LorentzIndex , Factoring -> True ];
		amp66 = amp;
		FCPrint[1,Length[amp66]];
		frhdot[xx__] :=
			FRH[DOT[xx]];
		amp = ((# /. Plus -> plsI&) /@ amp)/.DOT -> frhdot /.
		PropagatorDenominator[aa__]:> FRH[PropagatorDenominator[aa]];
		FCPrint[1,"leafcount = ",LeafCount[amp]];
		FCPrint[1,"combining"];
		amp1 = Combine[amp];
		FCPrint[1,"combining done "];
		den1 = Denominator[amp1];
		FCPrint[2,"denominator = ",den1//FRH];
		FCPrint[3,"LeafCount = ",LeafCount[amp1]];
		colorfactor = susa[colorpart ComplexConjugate[colorpart]];
		If[ colorfactor =!= 1,
			FCPrint[1,"the global colorfactor of the squared amplitudes is ",colorfactor];
		];
		nam = Numerator[amp1];
		If[ LeafCount[nam] < 1000,
			nam = Factor1[nam]
		];
		numfaN = NumericalFactor[nam];
		FCPrint[1,"numfaN = " , numfaN];
		nam = nam/numfaN;
		den1 = den1/numfaN;

		(*
		If[FreeQ2[nam, {SUNIndex, DiracGamma}],
			nam = pluli[nam](*,
			nam = Map[#/.Plus->pluli&, nam]; *)
			];
		*)

		(*
		tmp`$ZWISCHEN = True;
		If[tmp`$ZWISCHEN === True, nam >> "nam.s";
		den1 >> "den1.s"; prod >>"prod.s" ];
		*)
		prod = Collect2[prod, LorentzIndex];
		amp = SquareAmplitude[nam, ExtraFactor -> (prod colorfactor),
									IsolateNames -> isolhead,
									Mandelstam -> {}, Factoring -> False,
									IntermediateSubstitutions -> enmomsubst,
									FinalSubstitutions -> enmomback,
									EnergyMomentumConservation ->
									{-p1,-p2,k1,k2,k3,k4 }
							];
		(* ++++++++++++++++++++++++++++++++++ *)
		FCPrint[1,"collecing "];
		amp = amp  /. enmomsubst;
		amp77 = amp;
		$CheckCollect = False;
		If[ $CheckCollect === True,
			amp = Collect2[ amp, isolhead ]/.Plus -> plsI;
		];
		amp = amp / den1  ComplexConjugate[1/den1];
		If[ collecting === True,
			amp = Collect2[ amp, isolhead ]
		];
		If[ (proctype === (2->1) ) || (proctype === (1->2) ),
			factoring = True
		];
		If[ factoring =!= True,
			pluisol[xx__] :=
				Isolate[Plus[xx],
				IsolateNames -> isolhead, IsolateSplit->888];
			amp = Isolate[amp /. Plus -> pluisol, IsolateNames -> isolhead,
						IsolateSplit -> 888],
			amp = amp /. finsubst;
			amp = Collect2[ amp, isolhead ]//FRH//Factor2;
			amp = amp /. finsubst;
			amp = Factor2[amp];
			If[ (proctype === (2->2)) && (Length[mand] === 4),
				amp = TrickMandelstam[amp, mand]
			];
		];
		amp
	];


(* XFX *)
SquareAmplitude[ exp_ /; FreeQ[exp, FeynAmp], ops___Rule ] :=
	Block[ {ts,sps,amp2,cts,plu3,neamp,intermed,amp,
	amps1, amps2,pamp,lis, saveminamp, file,finsubst2, isolhead2,
	nwres, mand2, extrafact2,factoring2,epsAway,myfile},
		ts = FeynCalcInternal[exp] /. SUNDelta -> SUNDeltaContract /.
							SUNDeltaContract ->SUNDelta;
		enmo = EnergyMomentumConservation /. {ops} /. Options[SquareAmplitude];
		intermed = FeynCalcInternal[IntermediateSubstitutions /. {ops} /.
						Options[SquareAmplitude]];
		isolhead2 = IsolateNames /. {ops} /. Options[SquareAmplitude];
		saveminamp = WriteOut    /. {ops} /. Options[SquareAmplitude];
		extrafact2 = FeynCalcInternal[ExtraFactor /. {ops} /. Options[SquareAmplitude]];
		factoring2 = Factoring   /. {ops} /. Options[SquareAmplitude];
		finsubst2  = FeynCalcInternal[FinalSubstitutions  /. {ops} /.
						Options[SquareAmplitude]];
		epsAway    = EpsDiscard /. {ops} /. Options[SquareAmplitude];

		(*Mac fix, 18/9-2000, F.Orellana. Ditto for FileType's below*)
		If[ StringQ[saveminamp],
			myfile = FileType[saveminamp];
			Which[myfile === File,
				file = {saveminamp};,
	myfile === None,
	file = {};,
	True,
	Print["There was a problem:\n",saveminamp," is inaccessible"];
	Return[]
	],
			file = {}
		];
		(*If[StringQ[saveminamp], file = FileNames @@ {saveminamp}, file = {}];*)
		plu3[y__] :=
			Isolate[Plus[y],
			{DiracGamma, LorentzIndex,Spinor,Eps,SUNIndex},
				IsolateNames -> isolhead2,
				IsolateSplit -> 444I];
		etl[x__] :=
			plu3[x] /; FreeQ[{x}, Eps];
		nuLL1 = Unique[System`C];
		nuLL2 = Unique[System`C];
(*
nuLL1 = Unique[cCU]; nuLL2 = Unique[cCU];
*)
		etl[y__] :=
			Block[ {ee},
				ee = PartitHead[Plus[y], Eps];
				(Collect2[ee[[2]]+nuLL1 + nuLL2, Eps,Factoring->False
						] /. nuLL1 -> 0 /. nuLL2 -> 0(*/.Plus->plu2*)) +
				(ee[[1]]/.Plus->plu3)
			(*
(Factor2[ee[[1]], FactorTime->242]/.Plus->plu3)
*)
				] /; !FreeQ[{y}, Eps];
		(* ++++++++++++++++++++++++++++++++++++ *)
		FCPrint[2,"enmo = ", enmo];
		If[ file =!= {},
			FCPrint[2,"loading previous result ", file];
			(Get @@ {saveminamp});
			ts = tmp`$TSAMP;
			lis = Length[ts];
			nuLlLL = Unique[System`C];(*
		nuLlLL = Unique[cCUn];
		 *)
		(*
		FCPrint[2,"colling2insquareamplitudeagain"];
			ts = ((#/.Plus->etl)& /@ (ts + nuLlLL)) /. nuLlLL->0
		*)
		,
			ts = propdenexp[ts, enmo];
			FCPrint[2,"diracsimplifyinginsquareamplitude"];
			ts = DiracSimplify[ts];
			ots = ts;
			FCPrint[2,LeafCount[ots]];
			If[ Head[ts] === Plus,
				FCPrint[2,"colling2insquareamplitude"];
				ts = Collect2[ts, {Spinor,SUNIndex}, Factoring -> False];
				ots2 = ts;
			(*
					ts = Collect2[ts, {Spinor,SUNIndex}, Factoring -> True];
				*)
					];

			(* ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss *)
			spmin[xxxx_, enmomcon_List] :=
				Block[ {xx = xxxx,
				enms = (Map[Momentum, enmomcon]//ExpandScalarProduct), smmin,sol, sba,
				check, res = 0, simP},
					If[ Length[enmomcon] === 0,
						res = xxxx,
						sol[pe_,___] :=
							Solve[(Plus@@enms)==0,
							PowerExpand[Sqrt[pe^2]]][[1,1]];
						sba[pe_,___] :=
							{Reverse[sol[pe]], Map[-#&,Reverse[sol[pe]]]};
						check[pe_, __, a_List] :=
							!FreeQ[a, Last[sol[pe][[2]]]/
							NumericalFactor[Last[sol[pe][[2]]]]];
						smmin[Spinor[pe1__], a___, Spinor[pe2__]] :=
							smmin[Spinor[pe1] . a . Spinor[pe2]] =
							If[ FreeQ[{a}, Momentum],
								Spinor[pe1] . a . Spinor[pe2],
								If[ check[pe1, {a}],
									DiracSimplify[(Spinor[pe1]/.sol[pe1]) . a . Spinor[pe2]] /. sba[pe1],
									If[ check[pe2, {a}],
										DiracSimplify[Spinor[pe1] . a . (Spinor[pe2]/.sol[pe2])]/.sba[pe2],
										Spinor[pe1] . a . Spinor[pe2]
									],
									Spinor[pe1] . a . Spinor[pe2]
								]
							];
						specsu[x_] :=
							specsu2[Expand[x, Spinor]];
						specsu2[x_Plus] :=
							Map[specsu2, x];
						simP[xy_] := (*simP[xy]=*)
							specsu[
							FixedPoint[Expand[DiracSimplify[
										Contract[#(* /.DOT->smmin /. smmin -> DOT*)]]]&, xy, 7]
									] /. specsu2 -> specsir;
						xx = DiracSimplify[xx];
						xx = Collect2[xx, Spinor, Factoring->False];
						xx = Collect2[specsu[xx]/.specsu2 -> specsir, Spinor,Factoring->False];
						If[ Head[xx] =!= Plus,
							res = simP[xx],
							For[iji = 1, iji <= Length[xx], iji++,
								FCPrint[2,"iji = ",iji, " out of ", Length[xx]];
								res = res + (nP = simP[xx[[iji]]]);
								FCPrint[2,"nP = ",nP//FeynCalcForm];
							];
						];
					];
					res
				];
			(* sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss *)
			If[ (!FreeQ[ts, Spinor]) && (enmo=!= {}),
				FCPrint[2,"minimizing SME's"];
				ts = spmin[ts, enmo];
				FCPrint[2,"colling2insquareamplitudeagain"];
				ts = (Collect2[ts, {Spinor, SUNIndex} ] + nuLlLL) /. nuLlLL->0;
			];
			FCPrint[1,"ts//Length = ",ts//Length];
			ts = ((#/.Plus->etl)& /@ (ts + nuLlLL)) /. nuLlLL->0;
			If[ StringQ[saveminamp],
				Write2 @@ {saveminamp, tmp`$TSAMP == FRH[ts]}
			];
		];
		(* ++++++++++++++++++++++++ *)
		If[ ValueQ[tmp`ENM],
			enmLI = tmp`ENM,
			enmLI = {}
		];
		(*
		epsi[x_] := x /. Eps -> epsimP;
		epsimP[x__] := EpsEvaluate[Eps @@ ({x} /. enmLI)];
		*)
		extrafact2 = Collect2[ extrafact2 , LorentzIndex];

		(* for the non-fermionic case *)
		contractP[a_,b_] :=
			Block[ {tem, tem1, tem2},
				FCPrint[2,"entering contractP"];
				tem = FeynCalc`Package`contract2[a, b, Collecting -> False];
				FCPrint[2,"exiting contractP"];
				tem
			];
		ijj = 1;
		(*
		mulL[a_,a_]:=1;
		mulL[a_,b_]:=2;
		*)
		mulL[__] :=
			1;
		neamp = 0;
		FCPrint[2,"ts[[1]] = ", ts[[1]]//FeynCalcForm];

		(* Select w.r.t. to the structure of the amplitude *)
		If[ !FreeQ[ts, Spinor],
			cts  = ComplexConjugate[ts//FRH] /. Plus -> plu3;
			cts = ((#/.Plus->etl)& /@ (cts + nuLlLL)) /. nuLlLL->0;
			FCPrint[2,"length of cts = ", cts//Length];
			If[ Head[ts] =!= Plus,
				neamp = FermionSpinSum[susm[ts cts],
										ExtraFactor -> extrafact2] /. enmLI,
			(* somehow very very weird ... *)
				For[i = 1, i<=Length[ts], i++,
				(*
					For[j = i, j <= Length[ts], j++,
				(* ] *)
				*)
					For[j = 1, j <= Length[ts], j++,
				FCPrint[2,"i = ",i, "  j = ",j, " out of ",Length[ts]];
				calc = mulL[i, j] (FermionSpinSum[susm[ts[[i]] cts[[j]]],
										ExtraFactor -> extrafact2] /. enmLI);
				If[ (epsAway===True) && ((Length[Position[calc, DiracGamma[5]]] +
										Length[Position[calc, DiracGamma[6]]] +
										Length[Position[calc, DiracGamma[7]]]
										)) === 1,
					calc = calc /. DOT -> dOOt;
					calc = calc /. x:dOOt[DiracGamma[__]..] :>
									(DOT[x] /. DiracGamma[7] -> (1/2) /.
												DiracGamma[6] -> (1/2) /.
												DiracGamma[5] -> 0
									);
					calc = DiracSimplify[calc, Expanding -> False];
				];
				If[ calc===0&&lis>2,
					FCPrint[2,"this one vanishes ..."],
					FCPrint[2,"LeafCount newtrace = ", LeafCount[calc]];
					neamp = neamp + calc;
					FCPrint[2,"Length of neamp = ",Length[neamp]];
				];
						]
					]
			],
			pluIS[xx__] :=
				If[ FreeQ[{xx}, SUNIndex],
					pluIS2[xx],
					Plus[xx]
				];
			(*
			ts = FRH[ts] /. Plus -> pluIS;
			*)
			ts = FRH[ts];
			cts = ComplexConjugate[ts] /. Plus -> plu3;
			If[ Head[ts] === Plus,
				samp = susm[Expand[susa[ts cts /. Plus -> pluIS] ]],
				samp = susm[susa[ts cts/. Plus -> pluIS ]]
			];
			samp = samp /. pluIS2 -> Plus;
			(* WWW *)
			If[ Head[samp] === Times,
				sampfa = Select[samp, FreeQ[#, LorentzIndex]&];
				FCPrint[2,"sampfa = ",sampfa];
				newsamp = samp/sampfa,
				sampfa = 1;
				newsamp = samp
			];
			FCPrint[2,"sampfa = ",sampfa];
			FCPrint[2,"extrafact2 = ",extrafact2];
			(* FCPrint[2," newsamp = ",newsamp]; *)
			ti = Timing[
			If[ Head[newsamp]===Plus,
				samp = {};
				For[iis = 1, iis<=Length[newsamp], iis++,
					FCPrint[2,"contract samp ", iis," out of ",Length[newsamp]];
					nsamp = contractP[newsamp[[iis]], extrafact2];
			(*
					nsamp = Contract3[newsamp[[iis]] extrafact2];
			*)
					AppendTo[samp, nsamp];
					];
				samp = Apply[Plus, samp],
				samp = contractP[newsamp, extrafact2];
			(*
	samp = Contract3[newsamp extrafact2];
*)

	];
			FCPrint[2,"Length of samp = ",samp//Length];
						];
			FCPrint[1, "checccckkkkkk  ", TimeUsed[]];
			neamp = samp sampfa;
			FCPrint[1,"time needed for polarization sums = ",
				ti[[1]]//FeynCalcForm];
			FCPrint[1,"    collecting now "];
		];
		FCPrint[2,"after expanding the length of the amplitude is ",
				Length[neamp + neuladsg]-1];
		If[ FreeQ[neamp, DiracTrace],
			amps2 = neamp,
(*
If[Head[neamp] === Times,
	pamp  = Select[neamp, !FreeQ2[#, {DiracGamma,Complex}]&];
	amps2 = neamp / pamp,
	pamp = neamp; amps2 = 1
	];
*)
			SetOptions[DiracTrace, PairCollect -> True];
			SetOptions[TR, PairCollect -> True];
			mom4[xyx_,___] :=
				Momentum[xyx];
			lastsimp[yyy_] :=
				Block[ {rel, mul = 1, yy = yyy},
					If[ Head[yy] === Times,
						mul = Select[yy, FreeQ2[#,{Eps, LorentzIndex}]&];
						yy = yy/mul
					];
					rel =  Contract[yy/.intermed,
								EpsContract -> True];
					If[ !FreeQ[rel,Pair],
						rel = rel /. Momentum  -> mom4;
						tmp`REL = rel;
						rel = ExpandScalarProduct[rel];
					];
					FCPrint[2,"beforefactor3"];
					If[ LeafCount[rel]<9200,
						rel = Factor2[rel]
					];
					FCPrint[2,"afterfactor3"];
					If[ epsAway===True,
						rel = rel /. Eps[__]->0
					];
					If[ rel ===0,
						FCPrint[2,"TRS00000000"],
						rel = rel mul;
						If[ FreeQ[rel, Eps],
							rel = rel/.Plus->plu3,
							rel = rel /. Plus-> etl
						];
					];
					If[ rel =!= 0,
						FCPrint[2,"TRESULT = ", rel//FeynCalcForm]
					];
					rel
				];
			TRS[xyz__] :=
				MemSet[TRS[xyz] , TR[xyz]];
			epsimp[x__] :=
				If[ !FreeQ[{x},Eps],
					Schouten[Plus[x], 4444],
					Plus[x]
				];
			If[ !FreeQ[ neamp, DiracGamma],
				If[ Head[neamp]=!=Plus,
					nwres = { SUNSimplify[neamp/.DiracTrace->TRS]},
					nwres = {};
					For[ij = 1, ij <= Length[neamp], ij++, FCPrint[2,"ij = ",ij,
								" out of ", Length[neamp](*,
								"|| length of nwres = ",Length[nwres]*)];
														AppendTo[nwres, (ww = lastsimp[neamp[[ij]] /. DiracTrace -> TRS])];
					],
					nwres = {SUNSimplify[neamp/.DiracTrace->TRS]}
				];
			];
			FCPrint[2,"changing list to sum"];
			amps2 = (Plus@@nwres);
		];

		(*tmp`AMPS2=amps2;*)
		If[ factoring2 === True,
			amps2 = Factor2[ExpandScalarProduct[
						(amps2//FRH) /. intermed] /. Momentum -> mom4],
			amps2 = Expand[ExpandScalarProduct[
						(amps2//FRH) /. intermed] /. Momentum -> mom4]
		];
		amps2 = ExpandScalarProduct[amps2/.intermed] /. finsubst2;
		If[ Length[mand2] ===4,
			amps2 = TrickMandelstam[amps2, mand2]
		];
		amps2
	];

(* ********************************************************************** *)


propdenex[a_, m_] :=
	propdenex[a, m] = 1/Factor2[
	FixedPoint[ExpandScalarProduct, ScalarProduct[a, a] - m^2] ];

HoldPattern[propdp[su_][a_, m_]] :=
	propdenex[a,m] /; Length[su] <= 2;

propdp[su_][a_, m_] :=
	Block[ {na = ExpandScalarProduct[
			If[ FreeQ[a,Momentum],
				Momentum[a],
				a
			]], i, vn, tte = {na}},
		vn = Variables[na];
		tte = Join[tte,
				Table[ Expand[na /. (Solve[ su==0, vn[[i]] ][[1]])], {i, Length[vn]}]
					];
		tte = Sort[Union[tte], nsortQ][[1]];
		propdenex[tte, m]
	] /; Length[su] > 2;

fdpsave[x__] :=
	fdpsave2 @@ ({x}/.PropagatorDenominator->pdsave);

propdenexp[x_ /;FreeQ[x,PropagatorDenominator],___] :=
	x;
propdenexp[x_] :=
	PropagatorDenominatorExplicit[x];
propdenexp[x_, {}] :=
	propdenexp[x];
propdenexp[x_, {en__}] :=
	x /.
	FeynAmpDenominator -> fdpsave /.
	PropagatorDenominator -> propdp[Plus@@ExpandScalarProduct[Momentum/@{en}]
		] /. pdsave -> PropagatorDenominator/.
			fdpsave2 -> FeynAmpDenominator;

FCPrint[1,"SquareAmplitude.m loaded."];
End[]
