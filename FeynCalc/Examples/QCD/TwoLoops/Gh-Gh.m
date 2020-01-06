(* ::Package:: *)

(* :Title: Gh-Gh															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:  Gh -> Gh, massless QCD, 2-loops								*)

(* ------------------------------------------------------------------------ *)


(* ::Title:: *)
(*QCD ghost self-energy*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Gh -> Gh, massless QCD, 2-loops";
If[ $FrontEnd === Null,
	$FeynCalcStartupMessages = False;
	Print[description];
];
If[ $Notebooks === False,
	$FeynCalcStartupMessages = False
];
$LoadAddOns={"TARCER", "FeynArtsLoader"};
<<FeynCalc`
$FAVerbose = 0;

FCCheckVersion[9,3,0];


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


diags = InsertFields[CreateTopologies[2, 1 -> 1,ExcludeTopologies -> {Tadpoles}],
		{U[5]} -> {U[5]}, InsertionLevel -> {Classes}, Model -> "SMQCD"];

Paint[diags, ColumnsXRows -> {4,1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{768,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


(* ::Text:: *)
(*The prefactor 1/(2Pi)^(2D) for the loop integrals is understood. Notice that we ignore the first diagram (zero in DR) and the fifth diagram, since its contribution is identical to that of the fourth diagram.*)


amp[0] = FCFAConvert[CreateFeynAmp[DiagramExtract[diags,{2,3,4,6,7,8,9}], Truncated -> True, GaugeRules->{},
	PreFactor->-I], IncomingMomenta->{p}, OutgoingMomenta->{p},LoopMomenta->{q1,q2},
	UndoChiralSplittings->True, ChangeDimension->D, List->True, SMP->True,
	DropSumOver->True,FinalSubstitutions->{MQU[Index[Generation, 3]]->0,GaugeXi[_]->1-GaugeXi}];


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
ScalarProduct[p,p]=pp;


(* ::Section:: *)
(*Calculate the amplitude*)


amp[1]=FCTraceFactor/@amp[0];


(* ::Text:: *)
(*We simplify the color and Dirac algebra, do some partial fractioning and convert the integrals to the TRACER notation. To do  this we define the following helper function*)


RepRuleCancelQP={
x_. Power[Pair[Momentum[q_,dim_:4],Momentum[q_,dim_:4]],n_] *
FeynAmpDenominator[a___,PD[Momentum[q_,dim_:4],0],b___]:>x Power[Pair[Momentum[q,dim],Momentum[q,dim]],n-1] FeynAmpDenominator[a,b],

x_. Pair[Momentum[q_,dim_:4],Momentum[q_,dim_:4]] *
FeynAmpDenominator[a___,PD[Momentum[q_,dim_:4],0],b___]:>x FeynAmpDenominator[a,b],

x_. Pair[Momentum[p_,dim_:4],Momentum[q_,dim_:4]] *
FeynAmpDenominator[a___,PD[Momentum[q_,dim_:4]-Momentum[p_,dim_:4],0],b___]:>
-(1/2)x FeynAmpDenominator[a,b]
+(1/2)x Pair[Momentum[p,dim],Momentum[p,dim]]FeynAmpDenominator[a,PD[Momentum[q,dim]-Momentum[p,dim],0],b]
+(1/2)x Pair[Momentum[q,dim],Momentum[q,dim]]FeynAmpDenominator[a,PD[Momentum[q,dim]-Momentum[p,dim],0],b],

x_. Power[Pair[Momentum[p_,dim_:4],Momentum[q_,dim_:4]],n_] *
FeynAmpDenominator[a___,PD[Momentum[q_,dim_:4]-Momentum[p_,dim_:4],0],b___]:>
-(1/2)x Power[Pair[Momentum[p,dim],Momentum[q,dim]],n-1]  FeynAmpDenominator[a,b]
+(1/2)x Power[Pair[Momentum[p,dim],Momentum[q,dim]],n-1] Pair[Momentum[p,dim],Momentum[p,dim]]FeynAmpDenominator[a,PD[Momentum[q,dim]-Momentum[p,dim],0],b]
+(1/2)x Power[Pair[Momentum[p,dim],Momentum[q,dim]],n-1] Pair[Momentum[q,dim],Momentum[q,dim]]FeynAmpDenominator[a,PD[Momentum[q,dim]-Momentum[p,dim],0],b]
};
ClearAll[diagCompute];
diagCompute[ex_]:=
ex//SUNSimplify[#,Explicit->True,SUNTrace->True]&//
ReplaceAll[#,DiracTrace[x__]:>DiracTrace[x,DiracTraceEvaluate->True]]&//
Contract//FCLoopIsolate[#,{q1,q2},Head->loopHead]&//ReplaceRepeated[#,RepRuleCancelQP]&//
ReplaceAll[#,loopHead->Identity]&//ToTFI[#,q1,q2,p]&;


(* ::Text:: *)
(*and apply it to every single amplitude.*)


AbsoluteTiming[amp[2]=diagCompute/@amp[1];]


allints = Cases2[amp[2], TFI];
allints // Length


(* ::Text:: *)
(*There are 271 integrals to be done for the ghost self energy. There are several possibilities how to proceed. One possibility is to calculate the integrals one by one and save them to a file in the Database directory. This can be conveniently done using the CheckDB function. If the file "IntegralsQCDTwoLoopGhostSelfEnergy.db" does not exist the first argument of CheckDB is evaluated, otherwise the list is loaded and assigned to inttable.*)


Timing[inttable =
	CheckDB[Dispatch[
		Thread[allints ->
			Table[WriteString["stdout", "."];
				TarcerRecurse[allints[[i]]], {i, Length[allints]}]]],
		"IntegralsQCDTwoLoopGhostSelfEnergy.db"];]


(* ::Text:: *)
(*Now we need to insert the calculated integrals and rewrite the whole expression into a nicer form. Note that the Tarcer two loop integrals are defined to have only 1/(Pi)^D in the measure. Therefore, we will need to multiply the full result by 1/(4Pi)^D, since we did not include the prefactor (1/(2Pi)^D)^2 in the very beginning.*)


Timing[amp[3] =
	FeynAmpDenominatorExplicit[FCI[(Collect2[#, {TAI, TBI, TJI}, Factoring -> Factor2] & /@
	(amp[2] /. inttable))]];]


(* ::Text:: *)
(*The final result*)


resFinal=amp[3]//.SMP["g_s"]:>gs


(* ::Section:: *)
(*Check the final results*)


(* ::Text:: *)
(*Now let us compare our result with the literature. This computation can be found in A.I. Davydychev, P .Osland, O.V. Tarasov, Phys. Rev. D 58, 036007 (1998). The preprint is available at arXiv:hep-ph/9801380.*)
(**)
(*The general expression for the ghost self-energy (two-point function) is given by Eq. 2.15. What we computed is  -delta^{a1 a2} p^2 G^{(2)}(p^2) (c.f. Eq. 6.5).  The authors write G^{(2)}(p^2) as G^{(2,q)}(p^2) + G^{(2,\[Xi])(red)}(p^2) + G^{(2,\[Xi])(irred)}(p^2) (c.f. Eq 2.6), where  G^{(2,q)}(p^2) is the contribution of the quark loops (both one-particle irreducible and one-particle reducible), G^{(2,\[Xi])(irred)}(p^2) is the one particle irreducible contribution of the gluon and ghost loops and G^{(2,\[Xi])(red)}(p^2) is the one particle reducible one.*)
(**)
(*The quark loop contribution is given by the third diagram*)


G2q=resFinal[[3]]


(* ::Text:: *)
(*This should give us the same as Eq 6.13, with T = Nf Tf (c.f. Eq. 4.6) and eta  = ( Gamma[D/2-1]^2 Gamma[3-D/2] ) / Gamma[D-3]. Remember that we must remove - delta^{ab} p^2 from our G2q and multiply it by (1/(4Pi)^D).*)


G2qEval= (-1/(4Pi)^D TarcerExpand[G2q, D -> 4 - 2 Epsilon, 0])/.
	pp SUNDelta[a_,b_]->1/.CA->2T*CA


(* ::Text:: *)
(*Our result contains SEpsilon[4 - 2*Epsilon] which is an abbreviation for Exp[-Epsilon*EulerGamma]. Since eta is given by Exp[- Epsilon*EulerGamma] (1- 1/12 Pi^2 Epsilon^2 + ...) (c.f. Eq 4.7), it is clear that SEpsilon[4 - 2*Epsilon]^2 comes from there. To bring our result into the suitable form, we therefore must divide the term in the brackets by (1- 1/12 Pi^2 Epsilon^2)^2 or (1- Zeta2/2 Epsilon^2)^2  and again expand it in Epsilon. After that we can replace  SEpsilon[4 - 2*Epsilon]^2 by eta^2.*)


G2qFinal=G2qEval//ReplaceAll[#,Dot[a_,b_]:>Dot[a, Normal[Series[b/(1 - Zeta2/2 Epsilon^2)^2,{Epsilon,0,0}]]]]&//
ReplaceAll[#,{SEpsilon[4 - 2*Epsilon]^2->eta^2,gs->SMP["g_s"]}]&


G2qFinalPaper=(((CA*eta^2*SMP["g_s"]^4*T)/(-pp)^(2*Epsilon))*
	(-53/8 - 1/(2*Epsilon^2) - 7/(4*Epsilon))/(4*Pi)^D);


(* ::Text:: *)
(*Repeat the same for G^{(2,\[Xi])(red)}(p^2) which is given by Eq. 6.14. The reducible part comes from the diagram 7, hence*)


G2xiRed=resFinal[[7]];
G2xiRedEval= -1/(4Pi)^D TarcerExpand[G2xiRed, D -> 4 - 2 Epsilon, 0]//
ReplaceRepeated[#,{pp SUNDelta[a_,b_]->1}]&;
G2xiRedFinal=G2xiRedEval//ReplaceAll[#,Dot[a_,b_]:>Dot[a, Normal[Series[b/(1 - Zeta2/2 Epsilon^2)^2,{Epsilon,0,0}]]]]&//
ReplaceAll[#,{SEpsilon[4 - 2*Epsilon]^2->eta^2,gs->SMP["g_s"]}]&


G2xiRedPaper=(((CA^2*eta^2*SMP["g_s"]^4)/(-pp)^(2*Epsilon)) *( 3 + (1 + GaugeXi/2)/Epsilon + GaugeXi +
		(4 + 4*GaugeXi + GaugeXi^2)/(16*Epsilon^2))/(4*Pi)^D);


(* ::Text:: *)
(*Finally, we still need to verify G^{(2,\[Xi])(irred)}(p^2), the irreducible contribution of the gluon and ghost loops given by the remaining diagrams and shown in Eq 6.12*)


G2xiIrred=Collect2[Plus@@Join[resFinal[[1;;2]],resFinal[[4;;6]]], {TBI, TJI}];
G2xiIrredEval= -1/(4Pi)^D TarcerExpand[G2xiIrred, D -> 4 - 2 Epsilon, 0]//
ReplaceRepeated[#,{pp SUNDelta[a_,b_]->1,Nf*Tf->T}]&;
G2xiIrredFinal=G2xiIrredEval//ReplaceAll[#,Dot[a_,b_]:>Dot[a,Collect[b,{SEpsilon[_],(-pp)^(-2Epsilon)}]]]&//
ReplaceAll[#,Dot[a_, SEpsilon[x_]^2 (-pp)^(-2Epsilon) b_]:>Dot[SEpsilon[x]^2 (-pp)^(-2Epsilon) a,b]]&//
ReplaceAll[#,Dot[a_,b_]:>Dot[a, Normal[Series[b/(1 - Zeta2/2 Epsilon^2)^2,{Epsilon,0,0}]]]]&//
ReplaceAll[#,{SEpsilon[4 - 2*Epsilon]^2->eta^2,gs->SMP["g_s"]}]&


G2xiIrredPaper=(((CA^2*eta^2*SMP["g_s"]^4)/(-pp)^(2*Epsilon)) (  1/Epsilon(67/16 - (9*GaugeXi)/32) +
		(1 + (3*GaugeXi)/16 - (3*GaugeXi^2)/32)/Epsilon^2 + 503/32 +(-73*GaugeXi)/64 +
(3*GaugeXi^2)/8 - (3*Zeta[3])/4 - (3*GaugeXi^2*Zeta[3])/16)/(4*Pi)^D);


(* ::Text:: *)
(*Last but not least, let us verify the full contribution of the gluon and ghost loops which is given by Eq. 6.15*)


G2xFinal=(G2xiIrredFinal+G2xiRedFinal)//ReplaceAll[#,f_ Dot[a_,b_]+ f_ Dot[a_,c_]:>f Dot[a,Collect[Simplify[b+c],{1/Epsilon}]]]&


G2xPaper=(((CA^2*eta^2*SMP["g_s"]^4)/(-pp)^(2*Epsilon)) * ((83/16 + 7/32*GaugeXi)/Epsilon +
		(5/4 + 7/16*GaugeXi - 1/32 GaugeXi^2)/Epsilon^2 + 599/32 - 3/4 Zeta[3] - 9/64 GaugeXi + 3/8 GaugeXi^2 - 3/16 GaugeXi^2 Zeta[3] )/
	(4*Pi)^D);


knownResult = {G2qFinalPaper,G2xiRedPaper,G2xiIrredPaper,G2xPaper};
FCCompareResults[({G2qFinal,G2xiRedFinal,G2xiIrredFinal,G2xFinal}/.{Dot->Times}),knownResult,
Text->{"\tCompare to Davydychev, Osland and Tarasov, hep-ph/9801380, \
Eqs. 6.12-6.15:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic},Factoring->Simplify];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];



