(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracSimplify.mt *)

(* :Author: Vladyslav Shtaboveno *)

(* :Summary:  Unit tests for the DiracTrace function via MUnit      *)

(* ------------------------------------------------------------------------ *)

Needs["HighEnergyPhysics`FeynCalc`"];


testsStandard = FileNames[
  "*.test", $FeynCalcDirectory <> "/fctests/DiracTrace/Standard"]

testsSlashes = FileNames[
  "*.test", $FeynCalcDirectory <> "/fctests/DiracTrace/Slashes"]

SetOptions[DiracTrace,DiracTraceEvaluate->True];

TestStandard = True

    TestAllIndicesContracted = False
    TestOneIndexFree = False
    TestTwoIndicesFree = True
    TestThreeIndicesFree = False
    TestFourIndicesFree = False

TestSlashes = False

TestOneSlashAllIndicesContracted = False
TestTwoSlashesAllIndicesContracted = False
TestThreeSlashesAllIndicesContracted = False

fcCheckIdentity[list_List,length_Integer:0]:= Block[{testName=""},
    testName=(ToString(list[[1]][[1]])<>"-NumberOfTests");
    Test[Length[list],length,TestID->testName,TestFailureAction->"Abort",TestErrorAction->"Abort"];
    Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,list];
]

fcCheckEquivalenceViaSimplify[list_List,length_Integer:0]:=Block[{testName=""},
    testName=(ToString(list[[1]][[1]])<>"-NumberOfTests");
    Test[Length[list],length,TestID->testName,TestFailureAction->"Abort",TestErrorAction->"Abort"];
    Map[Test[Simplify[ToExpression[(#[[2]])]-ToExpression[(#[[3]])]],0 , TestID->#[[1]]]&,list]
]
If[TestStandard,

    Map[Get,testsStandard];
    Test[Length[testsStandard],100,TestID->"DiracTrace-Standard-NumberOfTestTypes",
        TestFailureAction->"Abort",TestErrorAction->"Abort"];

    (* Testing simplification of Dirac matrix chains with no free Lorentz indices*)

    If[TestAllIndicesContracted,

        (*7567 items*)
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsAllIndicesContracted,124];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG5AllIndicesContractedKreimer,1068];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG6AllIndicesContractedKreimer,1068];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG7AllIndicesContractedKreimer,1068];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG5OneG6AllIndicesContractedKreimer,942];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG5OneG7AllIndicesContractedKreimer,942];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG6OneG7AllIndicesContractedKreimer,942];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsTwoG5AllIndicesContractedKreimer, 471];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsTwoG6AllIndicesContractedKreimer, 471];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsTwoG7AllIndicesContractedKreimer, 471];


        (*7567 items*)
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsAllIndicesContracted,124];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG5AllIndicesContractedKreimer,1068];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG6AllIndicesContractedKreimer,1068];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG7AllIndicesContractedKreimer,1068];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG5OneG6AllIndicesContractedKreimer,942];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG5OneG7AllIndicesContractedKreimer,942];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG6OneG7AllIndicesContractedKreimer,942];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsTwoG5AllIndicesContractedKreimer,471];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsTwoG6AllIndicesContractedKreimer,471];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsTwoG7AllIndicesContractedKreimer,471];
    ];

    (* Testing simplification of Dirac matrix chains with one free Lorentz index*)


    If[TestOneIndexFree,

        (*6999 items*)
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneIndexFree,1068];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG5OneIndexFreeKreimer,942];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG6OneIndexFreeKreimer,942];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG7OneIndexFreeKreimer,942];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG5OneG6OneIndexFreeKreimer,690];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG5OneG7OneIndexFreeKreimer,690];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG6OneG7OneIndexFreeKreimer,690];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsTwoG5OneIndexFreeKreimer,345];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsTwoG6OneIndexFreeKreimer,345];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsTwoG7OneIndexFreeKreimer,345];

        (*6999 items*)
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneIndexFree,1068];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG5OneIndexFreeKreimer,942];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG6OneIndexFreeKreimer,942];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG7OneIndexFreeKreimer,942];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG5OneG6OneIndexFreeKreimer,690];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG5OneG7OneIndexFreeKreimer,690];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG6OneG7OneIndexFreeKreimer,690];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsTwoG5OneIndexFreeKreimer,345];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsTwoG6OneIndexFreeKreimer,345];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsTwoG7OneIndexFreeKreimer,345];
    ];



    (* Testing simplification of Dirac matrix chains with two free Lorentz indices*)


    If[TestTwoIndicesFree,

        (*5676 items*)
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsTwoIndicesFree,51];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG5TwoIndicesFreeKreimer,345];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG6TwoIndicesFreeKreimer,345];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG7TwoIndicesFreeKreimer,345];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG5OneG6TwoIndicesFreeKreimer,180];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG5OneG7TwoIndicesFreeKreimer,180];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG6OneG7TwoIndicesFreeKreimer,180];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsTwoG5TwoIndicesFreeKreimer,1350];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsTwoG6TwoIndicesFreeKreimer,1350];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsTwoG7TwoIndicesFreeKreimer,1350];

        (*5676 items*)
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsTwoIndicesFreeKreimer,51];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG5TwoIndicesFreeKreimer,345];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG6TwoIndicesFreeKreimer,345];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG7TwoIndicesFreeKreimer,345];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG5OneG6TwoIndicesFreeKreimer,180];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG5OneG7TwoIndicesFreeKreimer,180];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG6OneG7TwoIndicesFreeKreimer,180];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsTwoG5TwoIndicesFreeKreimer,1350];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsTwoG6TwoIndicesFreeKreimer,1350];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsTwoG7TwoIndicesFreeKreimer,1350];
    ];



    (* Testing simplification of Dirac matrix chains with three free Lorentz indices*)


    If[TestThreeIndicesFree,

        (*4705 items*)
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsThreeIndicesFree,115];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG5ThreeIndicesFreeKreimer,900];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG6ThreeIndicesFreeKreimer,900];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG7ThreeIndicesFreeKreimer,900];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG5OneG6ThreeIndicesFreeKreimer,420];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG5OneG7ThreeIndicesFreeKreimer,420];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG6OneG7ThreeIndicesFreeKreimer,420];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsTwoG5ThreeIndicesFreeKreimer,210];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsTwoG6ThreeIndicesFreeKreimer,210];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsTwoG7ThreeIndicesFreeKreimer,210];

        (*4705 items*)
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsThreeIndicesFree,115];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG5ThreeIndicesFreeKreimer,900];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG6ThreeIndicesFreeKreimer,900];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG7ThreeIndicesFreeKreimer,900];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG5OneG6ThreeIndicesFreeKreimer,420];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG5OneG7ThreeIndicesFreeKreimer,420];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG6OneG7ThreeIndicesFreeKreimer,420];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsTwoG5ThreeIndicesFreeKreimer,210];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsTwoG6ThreeIndicesFreeKreimer,210];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsTwoG7ThreeIndicesFreeKreimer,210];


    ];


    (* Testing simplification of Dirac matrix chains with four free Lorentz indices*)


    If[TestFourIndicesFree,

        (*4110 items*)
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsFourIndicesFree,15];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG5FourIndicesFreeKreimer,105];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG6FourIndicesFreeKreimer,105];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG7FourIndicesFreeKreimer,105];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG5OneG6FourIndicesFreeKreimer,840];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG5OneG7FourIndicesFreeKreimer,840];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG6OneG7FourIndicesFreeKreimer,840];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsTwoG5FourIndicesFreeKreimer,420];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsTwoG6FourIndicesFreeKreimer,420];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsTwoG7FourIndicesFreeKreimer,420];

        (*4110 items*)
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsFourIndicesFree,15];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG5FourIndicesFreeKreimer,105];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG6FourIndicesFreeKreimer,105];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG7FourIndicesFreeKreimer,105];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG5OneG6FourIndicesFreeKreimer,840];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG5OneG7FourIndicesFreeKreimer,840];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG6OneG7FourIndicesFreeKreimer,840];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsTwoG5FourIndicesFreeKreimer,420];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsTwoG6FourIndicesFreeKreimer,420];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsTwoG7FourIndicesFreeKreimer,420];

    ];

]





If[TestSlashes,

    Map[Get,testsSlashes];
    Test[Length[testsSlashes],60,TestID->"DiracTrace-Slashes-NumberOfTestTypes"];

    (* Testing simplification of Dirac matrix chains with one Dirac slash and no free Lorentz indices*)

    If[TestOneSlashAllIndicesContracted,

        (*6999 items*)
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneSlashAllIndicesContracted,1068];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG5OneSlashAllIndicesContractedKreimer,942];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG6OneSlashAllIndicesContractedKreimer,942];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG7OneSlashAllIndicesContractedKreimer,942];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG5OneG6OneSlashAllIndicesContractedKreimer,690];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG5OneG7OneSlashAllIndicesContractedKreimer,690];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG6OneG7OneSlashAllIndicesContractedKreimer,690];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsTwoG5OneSlashAllIndicesContractedKreimer,345];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsTwoG6OneSlashAllIndicesContractedKreimer,345];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsTwoG7OneSlashAllIndicesContractedKreimer,345];

        (*6999 items*)
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneSlashAllIndicesContracted,1068];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG5OneSlashAllIndicesContractedKreimer,942];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG6OneSlashAllIndicesContractedKreimer,942];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG7OneSlashAllIndicesContractedKreimer,942];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG5OneG6OneSlashAllIndicesContractedKreimer,690];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG5OneG7OneSlashAllIndicesContractedKreimer,690];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG6OneG7OneSlashAllIndicesContractedKreimer,690];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsTwoG5OneSlashAllIndicesContractedKreimer,345];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsTwoG6OneSlashAllIndicesContractedKreimer,345];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsTwoG7OneSlashAllIndicesContractedKreimer,345];
    ];

    (* Testing simplification of Dirac matrix chains with two Dirac slashes and no free Lorentz indices*)


    If[TestTwoSlashesAllIndicesContracted,

        (*5688 items*)
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsTwoSlashesAllIndicesContracted,153];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG5TwoSlashesAllIndicesContractedKreimer,1035];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG6TwoSlashesAllIndicesContractedKreimer,1035];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG7TwoSlashesAllIndicesContractedKreimer,1035];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG5OneG6TwoSlashesAllIndicesContractedKreimer,540];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG5OneG7TwoSlashesAllIndicesContractedKreimer,540];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG6OneG7TwoSlashesAllIndicesContractedKreimer,540];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsTwoG5TwoSlashesAllIndicesContractedKreimer,270];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsTwoG6TwoSlashesAllIndicesContractedKreimer,270];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsTwoG7TwoSlashesAllIndicesContractedKreimer,270];

        (*5688 items*)
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsTwoSlashesAllIndicesContracted,153];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG5TwoSlashesAllIndicesContractedKreimer,1035];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG6TwoSlashesAllIndicesContractedKreimer,1035];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG7TwoSlashesAllIndicesContractedKreimer,1035];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG5OneG6TwoSlashesAllIndicesContractedKreimer,540];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG5OneG7TwoSlashesAllIndicesContractedKreimer,540];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG6OneG7TwoSlashesAllIndicesContractedKreimer,540];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsTwoG5TwoSlashesAllIndicesContractedKreimer,270];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsTwoG6TwoSlashesAllIndicesContractedKreimer,270];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsTwoG7TwoSlashesAllIndicesContractedKreimer,270];
    ];



    (* Testing trace of Dirac matrix chains with three Dirac slashes and no free Lorentz indices*)


    If[TestThreeSlashesAllIndicesContracted,

        (*6700 items*)
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsThreeSlashesAllIndicesContracted,100];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG5ThreeSlashesAllIndicesContractedKreimer,600];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG6ThreeSlashesAllIndicesContractedKreimer,600];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG7ThreeSlashesAllIndicesContractedKreimer,600];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG5OneG6ThreeSlashesAllIndicesContractedKreimer,1000];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG5OneG7ThreeSlashesAllIndicesContractedKreimer,1000];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsOneG6OneG7ThreeSlashesAllIndicesContractedKreimer,1000];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsTwoG5ThreeSlashesAllIndicesContractedKreimer,600];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsTwoG6ThreeSlashesAllIndicesContractedKreimer,600];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceIn4dimsTwoG7ThreeSlashesAllIndicesContractedKreimer,600];

        (*6700 items*)
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsThreeSlashesAllIndicesContracted,100];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG5ThreeSlashesAllIndicesContractedKreimer,600];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG6ThreeSlashesAllIndicesContractedKreimer,600];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG7ThreeSlashesAllIndicesContractedKreimer,600];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG5OneG6ThreeSlashesAllIndicesContractedKreimer,1000];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG5OneG7ThreeSlashesAllIndicesContractedKreimer,1000];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsOneG6OneG7ThreeSlashesAllIndicesContractedKreimer,1000];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsTwoG5ThreeSlashesAllIndicesContractedKreimer,600];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsTwoG6ThreeSlashesAllIndicesContractedKreimer,600];
        fcCheckEquivalenceViaSimplify[fcstDiracTraceInDdimsTwoG7ThreeSlashesAllIndicesContractedKreimer,600];
    ];


]
