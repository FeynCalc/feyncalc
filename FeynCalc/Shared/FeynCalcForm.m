(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: FeynCalcForm[expr] formats expr in a short form.
						In FeynCalc.m  $PrePrint can be set to
						$PrePrint = FeynCalcForm
*)

(* ------------------------------------------------------------------------ *)

FCF::usage=
"FCF[exp] is a short form for FeynCalcForm[exp].";

FeynCalcForm::usage=
"FeynCalcForm[expr] changes the printed output to a an easy-to-read form. It
allows a readable output also when running a terminal based Mathematica
session. Whether the result of FeynCalcForm[expr] is displayed or not, depends
on the setting of $PrePrint.

$PrePrint = FeynCalcForm forces displaying everything after applying
FeynCalcForm. In order to change to the normal (internal) Mathematica
OutputForm, do: $PrePrint=..";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"];
End[]

Begin["`FeynCalcForm`Private`"];

fcdot2::usage="";
hold::usage="";

FCF = FeynCalcForm;

bra = "(";
ket = ")";

Options[FeynCalcForm] = {FinalSubstitutions -> {}};

(* for future changes ... *)
cdf = Symbol["CommonDefaultFormatTypes"];

Off[Rule::rhs];
FeynCalcForm[x_,opts___] :=
	Block[{re},
		Off[Rule::rhs];
		re =
			If[$Notebooks === True,
				If[$PrePrint === FeynCalcForm,
					If[MemberQ[{TraditionalForm, StandardForm, InputForm},
						"Output" /. (cdf /. Options[$FrontEnd, "CommonDefaultFormatTypes"])],
						Unset[$PrePrint]; (*Print["UNSET"]; *)
						x,
						(*i.e., in OutputForm one can have $PrePrint=FeynCalcForm *)
						feynCalcForm[x,opts]
					],
					x
				],
			feynCalcForm[x,opts]
			];
		On[Rule::rhs];
		re
	];

(* timefixdef : a more physics - like timing function *)
tim[a_, b_] :=
	If[$Notebooks===True,
		SequenceForm[StringInsert[ToString[Floor[10 a]],".",-2]," ",b],
		a b
	];

(* due to Dave Withoff ... *)
feynCalcForm[InputForm[f_]]:=
	InputForm[f];

SetAttributes[feynCalcForm, HoldAll];
SetAttributes[FeynCalcForm, HoldAll];

HoldPattern[feynCalcForm[TimeUsed[]]] :=
	timefix[TimeUsed[]];

timefix[n_]:=
	Which[
		0.<=n<0.02,
		tim[" < 0.02","s"],
		0.02<=n<9.5,   tim[N[n,2], "s"],
		9.5<=n<59.5,  tim[N[n,2], "s"],
		59.5<=n<600,  tim[N[n/60,2], "min"],
		600<=n<3570,  tim[N[n/60,2], "min"],
		3569<n<36000, tim[N[n/3600,2], "h"],
		36000<n,      tim[N[n/3600,4], "h"]
	];

sunfuser[a_,b_,c_,___]:=
	fsunU[a, b, c]/.fsunU->"f";
sumst[x_Plus]:=
	SequenceForm["(",x,")"];  sumst[y_]:=y;

diracsldi[di_][x__,Dimension -> di_] :=
	DiracSlash[x, Dimension -> di];

diracmadi[di_][x__,Dimension -> di_] :=
	If[!FreeQ[{x}, Rule],
		DiracMatrix[x],
		DiracMatrix[x, Dimension -> di]
	];

diracmadi[di_][x__] :=
	If[!FreeQ[{x}, Rule],
		DiracMatrix[x],
		DiracMatrix[x, Dimension -> di]
	];

iDentity[a_,___] :=
	a;
sunident[a_] :=
	a;

didm[x_,___]:=
	x;
didl[x_,___]:=
	x;
	(*Format[fcdot2[a_,b__]] := Infix[fcdot2[a,b], " ", 320];*)
	(*Not an allowed syntax in mma 4.1. F.Orellana*)

Format[fcdot2[a_,b__]] := Infix[fcdot2[a,b], " "];
(* ??? *)
(*fcdot2[x-y,x-rd];*)
Format[fcdot2[a_]] := a;

diF[x_-4]:=
	StringJoin[ToString[x],"-4"];
diF[x_]:=
	x;

double[{a___, x_, x_, b___}] :=
	{a,x,x,b};

dea[yy__] :=
	double[Map[denfa,{yy}]] /. double -> Identity;

denfa[_[Subscript[x_, s_],0]] :=
	SequenceForm["(",x[s]^2,")"];

denfa[_[Momentum[Subscript[x_, s_],___],0]] :=
	SequenceForm["(",x[s]^2,")"];

denfa[_[x_]] :=
	SequenceForm["(",x^2,")"];

denfa[_[x_,0]] :=
	SequenceForm["(",x^2,")"];

denfa[_[x_,y_]] :=
	SequenceForm["(",x^2,"- ",y^2,")"];

feynden[x__] :=
	1 / fcdot2 @@ ( dea @@ {x} );

ditr[x_,___] :=
	"tr"[x];

fdprop[a__] :=
	1 / denfa[hold[a]];
compind[a_] :=
	If[Head[a] === Symbol,
		StringJoin[ToString[a],"*"], a "*"];
		myscriptsbox[x_] := x;


epsd[a___, (b_/;(Head[b] ===LorentzIndex) || (Head[b] === Momentum))[c_,di_], d___] :=
	Subscript["eps", di // diF][a,b[c,di],d];
epsd[a__] :=
	"eps"[a];

(* display only one dimension (for readability) *)
ni[di_]:=
	ToString[di];
ni[di__]:=
	ToString[{di}[[1]]];

diracslm[a_] :=
	DiracSlash[a];

diracslm[a_, rul___Rule] :=
	DiracSlash[a, rul];

diracslm[a_, b__, rul_Rule] :=
	SequenceForm @@Map[DiracSlash[#, rul]&, {a, b}];

diracslm[a_, b__] :=
	SequenceForm @@ Map[DiracSlash[#]&, {a, b}];

plusdi[a_] :=
	Subscript[SequenceForm["(", a, ")"], " + "];

feynCalcForm[x_,opt___Rule]:=
	Block[{xxxx = Evaluate[x], subs, fcdot, diracsldid},
		subs = FinalSubstitutions /. {opt} /. Options[FeynCalcForm];
		xxxx = xxxx /. subs;
		xxxx = xxxx/.(n_Real Second)->timefix[n];
		xxxx = (xxxx/.
				DOT:>fcdot /. SUNN :> "N"/. SUNTrace :> "tr"  /. LeviCivita[lv__] :> epsd[lv] /.
				Eps[vl__] :> epsd[vl] /. MetricTensor[v_, w_, OptionsPattern[]] :> "g"[v, w]  /.
				ScalarProduct[ v_,v_ ] :> v^2 /. ScalarProduct[v_ w_] :>
				(SequenceForm@@Flatten[ {v//sumst ,{"."},w//sumst} ]) /.
				(* PolarizationVector[ka_, mu_, ___] :> "ep"[ka, mu]  /. *)
				{
					Pair[Momentum[Polarization[v_,-I,sun___]],
					LorentzIndex[w_] ] :> ("ep(*)"[v,w,sun] ),
					Pair[ Momentum[Polarization[v_,-I,___]] ,
					LorentzIndex[w_] ]:> "ep(*)"[v, w] ,
					Pair[ Momentum[Polarization[v_,I,sun___]],
					LorentzIndex[w_] ]:> ("ep"[v,w,sun])
				}  /.
				{Pair[ LorentzIndex[v_],LorentzIndex[w_] ] :> "g"[v, w],
				Pair[ LorentzIndex[v_,di_],LorentzIndex[w_,di_] ] :>
					(Subscript["g", di // diF][v, w])} /.
				Pair[ Momentum[v_,___],Momentum[v_,___] ] :>
					v^2 /.
				Pair[ Momentum[v_,___],Momentum[w_,___] ] :>
					(SequenceForm@@Flatten[ {v//sumst ,{"."},w//sumst} ]) /.
				Pair[ Momentum[v_,di_Symbol-4],Momentum[w_,di_Symbol-4] ] :>
					Subscript[(SequenceForm@@Flatten[{"(",v//sumst ,{"."},w//sumst,")"}]), di//diF]/.
				Pair[ LorentzIndex[w_,___],Momentum[Subscript[v_,s_],___ ]]:>
					(SequenceForm@@Flatten[ {sumst[v[s]],"[",w,"]"} ]) /.
				Pair[ LorentzIndex[w_, ___],Momentum[v_, ___] ] :>
					(SequenceForm@@Flatten[ {sumst[v],"[",w,"]"} ])/.
					{Polarization[ka_,-I,___]:>"ep(*)"[ka],	Polarization[ka_,I,___]:>"ep"[ka]} /.
				ChiralityProjector[+1] :>
					DiracGamma[6] /.
				ChiralityProjector[-1] :>
					DiracGamma[7] /.
				OPEDelta :>
					"De"/.
				DiracMatrix[6] :> DiracGamma[6] /.
				{
					DiracGamma[LorentzIndex[v_]] :>
						DiracMatrix[v, Dimension -> 4],
					DiracGamma[LorentzIndex[v_,di_],di_] :>
						DiracMatrix[v, Dimension -> 4],
					DiracGamma[Momentum[v_]] :>
						DiracSlash[v, Dimension -> 4],
					DiracGamma[Momentum[v_,di_],di_] :>
						DiracSlash[v, Dimension -> 4]
				} /.
				{
					DiracGamma[5] :> "ga[5]",
					DiracGamma[6] :> "ga[6]",
					DiracGamma[7] :> "ga[7]"
				} /.
				If[(Dimension /. Options[DiracMatrix]) =!= 4,
					DiracMatrix[v_] :>
					diracmadi[(Dimension /. Options[DiracMatrix])][v],
					{}
				]/.
				DiracMatrix[dmv__, Dimension -> 4] :> DiracMatrix[dmv]/.
				DiracSlash[vsv__, Dimension -> 4] :> DiracSlash[vsv] /.
				{
					DiracSigma[_[a_], _[b_]]:> "Sigma"[a,b],
					DiracSigma[_[a_, b_]]   :> "Sigma"[a,b]
				}/.
				{
					DiracMatrix[v__, Dimension -> di_] :> Subscript["ga"[v],di],
					DiracMatrix[v__]  :> "ga"[v]
				} /.
				If[(Dimension /. Options[DiracSlash]) =!= 4,
					DiracSlash[v__]:>
						diracsldid[(Dimension /. Options[DiracSlash])][v]/.
					diracsldid :> diracsldi,
							{}
				]/.
				DiracSlash[aa_] :>
					diracslm[aa] /.
				{
					DiracSlash[v_, Dimension -> di_] :>
						Subscript[ToString["gs"], di//diF ][v] ,
					DiracSlash[Subscript[v_,s_]] :> "gs"[v[s]] ,
						DiracSlash[v_]:> ToString["gs"[v]]
				} /.
				{
				fcdot[Spinor[-p_, 0, ___], a__] :>
					DOT["v"[-p/.Momentum->iDentity], a],
				fcdot[Spinor[p_, 0, ___], a__]  :>
					DOT["u"[p/.Momentum->iDentity], a],
				fcdot[a__,Spinor[-p_, 0, ___] ] :>
					DOT["v"[-p/.Momentum->iDentity], a],
				fcdot[a__, Spinor[p_, 0, ___]]  :>
					DOT[a, "u"[p/.Momentum->iDentity]]
				} /.
				{
				fcdot[Spinor[-p_, mas_, _], a__] :>
					DOT["v"[-p/.Momentum->iDentity,mas], a],
				fcdot[Spinor[p_, mas_, _], a__]  :>
					DOT["u"[p/.Momentum->iDentity,mas], a],
				fcdot[a__,Spinor[-p_, mas_, _] ] :>
					DOT[a, "v"[-p/.Momentum->iDentity,mas]],
				fcdot[a__, Spinor[p_, mas_, _]]  :>
					DOT[a, "u"[p/.Momentum->iDentity,mas]]
				} /.
				{
					Spinor[-p_,0,___] :> "v"[p /. Momentum -> iDentity],
					Spinor[p_,0,___]  :> "u"[p /. Momentum -> iDentity],
					Spinor[-p_,ma_,_] :> "v"[p /. Momentum -> iDentity,ma],
					Spinor[p_,ma_,_]  :> "u"[p /. Momentum -> iDentity,ma]
				} /.
				SUNDelta[a_, b_] :> "d"[a, b] /.
				SUND[a_, b_, c_] :> "d"[a, b, c] /.
				SUNF[a_, b_, c_] :>  "f"[a, b, c]/.
				{
				SUNT[a_] :>  "T"[a],
				SUNT[a_,b__] :> (fcdot2 @@ Map["T"[#]&,{a, b}])
				} /.
				OPEm :> "m" /.
				OPEi :> "i" /.
				OPEj :> "j" /.
				OPEl :> "l" /.
				OPEk :> "k" /.
				{
					QuantumField[a_] :>
						a,
					QuantumField[a_, lori___Momentum, suni___SUNIndex][p___] :>
						"Q"[a, lori,suni][p],
					QuantumField[a_, lori___LorentzIndex, suni___SUNIndex][p___] :>
						"Q"[a, lori,suni][p],
					QuantumField[a_, lori___LorentzIndex, suni___SUNIndex] :>
						"Q"[a, lori,suni],
					QuantumField[a_, lori___Momentum, suni___SUNIndex] :>
						"Q"[a, lori,suni],
					QuantumField[pa:FCPartialD[_].., a_, lori___LorentzIndex, suni___SUNIndex][p___] :>
						"Q"[pa, a, lori, suni][p],
					QuantumField[pa:FCPartialD[_].., a_, lori___Momentum, suni___SUNIndex][p___] :>
						"Q"[pa, a, lori, suni][p],
					QuantumField[pa:FCPartialD[_].., a_, lori___LorentzIndex, suni___SUNIndex]  :>
						("Q"[pa, a, lori, suni]/.FCPartialD -> "P"),
					QuantumField[pa:FCPartialD[_].., a_, lori___Momentum, suni___SUNIndex] :>
						("Q"[pa, a, lori, suni]/.FCPartialD -> "P")
				}/.
				FCPartialD[a_] :> "
					P"[a]/.
				fcdot:>
					fcdot2/. (*fcdot2 -> DOT /.*)
				DiracTrace[v__] :>
					ditr[v] /.
				LorentzIndex[vv__] :>
					didl[vv]  /.
				QuantumField[v__] :>
					"Q"[v] /.
				FCPartialD[v_] :> "P"[v] /.
				PlusDistribution[v_] :>
					plusdi[v] /.
				SUNIndex[i_] :>
					sunident[i]/.
				OPESum :>
					"OPESum"/.
				DeltaFunction :>
					"delta" /.
				Twist2GluonOperator:>
					"GO"/.
				sunident :>
					SUNIndex /.
				FeynAmpDenominator[v__] :>
					feynden[v]/.
				PropagatorDenominator[v__] :>
					fdprop[v]/.
				Momentum[v__] :>
					didm[v]  /.
				LorentzIndex[v__] :>
					didl[v]  /.
				{didm :> Momentum, didl :> LorentzIndex}
			);
		xxxx
	];

FCPrint[1,"FeynCalcForm.m loaded"];
End[]
