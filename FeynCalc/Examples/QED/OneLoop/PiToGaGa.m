(* ::Package:: *)

(* :Title: PiToGaGa															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:  Pi -> Ga Ga, QED, axial current, 1-loop						*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Adler-Bell-Jackiw anomaly in QED*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Pi -> Ga Ga, QED, axial current, 1-loop";
If[ $FrontEnd === Null,
	$FeynCalcStartupMessages = False;
	Print[description];
];
If[ $Notebooks === False,
	$FeynCalcStartupMessages = False
];
<<FeynCalc`
$FAVerbose = 0;

If[!MatchQ[ToExpression[StringSplit[$FeynCalcVersion, "."]],{a_/;a>=9,b_/;b>=3,_}],
	If[ ($FrontEnd === Null||$Notebooks===False),
	Print["Your FeynCalc version is too old. \
This example requires at least FeynCalc 9.3!"];
	Quit[],
	CreateDialog[{TextCell["Your FeynCalc version is too old. \
This example requires at least FeynCalc 9.3!"],DefaultButton[]},
	Modal->True];
	]
];


(* ::Section:: *)
(*Obtain the amplitude*)


(* ::Text:: *)
(*Nicer typesetting*)


MakeBoxes[mu,TraditionalForm]:="\[Mu]";
MakeBoxes[nu,TraditionalForm]:="\[Nu]";
MakeBoxes[la,TraditionalForm]:="\[Lambda]";


(* ::Text:: *)
(*According to Peskin and Schroeder (Ch 19.2), the amplitude for the first triangle diagram reads*)


amp1[0] = ((-1)(-I SMP["e"])^2 DiracTrace[GAD[mu].GA[5].
	QuarkPropagator[l-k].GAD[la].QuarkPropagator[l].
	GAD[nu].QuarkPropagator[l+p]])//Explicit


(* ::Text:: *)
(*And the second one follows from the first by interchanging k with p and la with nu*)


amp2[0] = amp1[0]/.{k->p,p->k,la->nu,nu->la}


(* ::Section:: *)
(*Calculate the amplitude*)


(* ::Text:: *)
(*Contracting both amplitudes with I*(k+p)^mu we can check the non-conservation of the axial current.*)


amp[0] = Contract[I*FVD[k+p,mu](amp1[0]+amp2[0])]


(* ::Text:: *)
(*For this calculation it is crucial to use a correct scheme for gamma^5. As in the book, we use the *)
(*Breitenlohner-Maison-t'Hooft-Veltman prescription.*)


FCSetDiracGammaScheme["BMHV"];
amp[1] = TID[amp[0] ,l, ToPaVe->True]


FCClearScalarProducts[];
Momentum[k,D|D-4]=Momentum[k];
Momentum[p,D|D-4]=Momentum[p];


(* ::Text:: *)
(*The explicit values for the PaVe functions B0 and C0 can be obtained e.g. from H. Patel's Package-X. *)
(*Here we just insert the known results. The C0 function is finite here, so because of the prefactor (D-4) it *)
(*gives no contribution in the D->4 limit.*)


amp[2]=Collect2[amp[1],{B0,C0}]//. {
	B0[FCI@SP[p_,p_],0,0]:>
		1/(16 Epsilon \[Pi]^4)-(-2+EulerGamma)/(16 \[Pi]^4)+
		Log[-((4 \[Pi] ScaleMu^2)/Pair[Momentum[p],Momentum[p]])]/(16 \[Pi]^4),
	B0[FCI[SP[p,p]+2SP[p,k]+SP[k,k]],0,0]:>
		B0[FCI[SP[k+p,k+p]],0,0],
	(D-4)ExpandScalarProduct[C0[SP[k],SP[p],SP[k+p],0,0,0]]->0
}


(* ::Text:: *)
(*Now we insert the explicit values, convert the external momenta to 4 dimensions and expand in Epsilon*)


amp[3] = amp[2]//FCReplaceD[#,D->4-2Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal


(* ::Text:: *)
(*The result should be twice Eq. 19.59 in Peskin and Schroeder*)


(* ::Section:: *)
(*Check the final results*)


knownResult = 2(SMP["e"]^2/(4 Pi^2)LC[al,la,be,nu]FV[k,al]FV[p,be])//Contract;
FCCompareResults[amp[3],knownResult,
Text->{"\tCompare to Peskin and Schroeder, An Introduction to QFT, \
Eq 19.59:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];
