(* ::Package:: *)

(* :Title: QEDElectronGMinusTwoOneLoop                                       *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the electron's g-2 in QED at 1-loop *)

(* ------------------------------------------------------------------------ *)


(* ::Section:: *)
(*Electron's g-2 in QED*)


(* ::Subsection:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the electron's g-2 in QED at 1-loop"];
];
$LoadFeynArts=True;
<< FeynCalc`
$FAVerbose=0;


(* ::Subsection:: *)
(*Generate Feynman diagrams*)


topVertex = CreateTopologies[1, 1 -> 2,ExcludeTopologies -> {Tadpoles,WFCorrections}];
diagsVertex = InsertFields[topVertex, {F[2, {1}]} ->
		{V[1],F[2, {1}]},InsertionLevel -> {Classes},ExcludeParticles -> {S[1], S[2],S[3],V[3], V[2]}];
Paint[diagsVertex, ColumnsXRows -> {1, 1}, Numbering -> None,SheetHeader->None,ImageSize->{256,256}];


ampVertex=FCFAConvert[CreateFeynAmp[diagsVertex,Truncated -> False,PreFactor->-1],IncomingMomenta->{p1},
OutgoingMomenta->{k,p2},LoopMomenta->{q},UndoChiralSplittings->True,SMP->True,ChangeDimension->D,
List->False,FinalSubstitutions->{SMP["m_e"]->ME}]/.k->p1-p2/.q->q+p1


(* ::Subsection:: *)
(*Set up the kinematics*)


FCClearScalarProducts[];
ScalarProduct[p1,p1]=ME^2;
ScalarProduct[p2,p2]=ME^2;
ScalarProduct[k,k]=0;
ScalarProduct[p1,p2]=ME^2;


(* ::Subsection:: *)
(*Compute the diagrams*)


ampVertex1=ampVertex//ReplaceAll[#,Pair[Momentum[Polarization[___],___],___]:>1]&//Contract


(* ::Text:: *)
(*Now we simplify the Dirac algebra and reduce our tensor loop integrals into scalar ones.*)


ampVertex2=OneLoopSimplify[ampVertex1,q]//Collect2[#,Spinor]&


(* ::Text:: *)
(*Remember that to extract F2 (0) we need to look only at the piece proportional to (p1+p2)^mu. So let us drop the g^mu -piece*)


ampVertex3=ampVertex2//ReplaceAll[#,FCI[GAD[Lor1]]:>0]&//DotSimplify


(* ::Text:: *)
(*The explicit values for the PaVe functions C1, C11 and C12 can be obtained e.g. from H. Patel's Package-X. Here we just insert the known results.*)


ampVertex4=ampVertex3/.{PaVe[1,{ME^2,0,ME^2},{0,ME^2,ME^2},OptionsPattern[]]->1/(32Pi^4 ME^2),
PaVe[1,1,{ME^2,0,ME^2},{0,ME^2,ME^2},OptionsPattern[]]->-(1/(96Pi^4 ME^2)),
PaVe[1,2,{ME^2,0,ME^2},{0,ME^2,ME^2},OptionsPattern[]]->-(1/(192Pi^4 ME^2))}


(* ::Text:: *)
(*As expected, F2 (0) is free of any divergences. So we can safely do the limit D ->4*)


ampVertex5=ampVertex4//ChangeDimension[#,4]&//ReplaceAll[#,D->4]&


(* ::Text:: *)
(*What we obtained so far is nothing else than $\frac{i e}{2 m_e} (p_1+p_2)^\mu F_2 (0) \bar{u}(p_2) u(p_1)$*)
(*Dividing by the numerical prefactor and substituting $e^2 = 4\pi^2 \alpha$ yields*)


res=(ampVertex5/((I SMP["e"])/(2 ME)))//ReplaceAll [#,{SMP["e"]^2->AlphaFS 4\[Pi],Spinor[__].Spinor[__]:>1,FCI[FV[p1,_]+FV[p2,_]]:>1}]&


(* ::Text:: *)
(*The result should agree with the known value for F2(0)*)


resLit = AlphaFS/(2Pi);
Print["Check with the known result: ", If[(res-resLit)===0,
		"CORRECT.", "!!! WRONG !!!"]];
