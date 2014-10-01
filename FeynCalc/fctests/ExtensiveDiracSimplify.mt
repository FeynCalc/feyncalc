(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ExtensiveDiracSimplify.mt										*)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2014 Rolf Mertig
   Copyright (C) 1997-2014 Frederik Orellana
   Copyright (C) 2014 Vladyslav Shtabovenko
*)

(* :Summary:  Extensive unit tests for DiracSimplify					    *)

(* ------------------------------------------------------------------------ *)

Needs["HighEnergyPhysics`FeynCalc`"];


testsStandard = FileNames[
  "*.test", $FeynCalcDirectory <> "/fctests/DiracSimplify/Standard"]

testsSlashes = FileNames[
  "*.test", $FeynCalcDirectory <> "/fctests/DiracSimplify/Slashes"]


TestStandard = False

  TestAllIndicesContracted = False
  TestOneIndexFree = False
  TestTwoIndicesFree = False
  TestThreeIndicesFree = False
  TestFourIndicesFree = False

TestSlashes = True

TestOneSlashAllIndicesContracted = False
TestTwoSlashesAllIndicesContracted = True
TestThreeSlashesAllIndicesContracted = False

fcCheckIdentity[list_List,length_Integer:0]:= Block[{testName=""},
  testName=(ToString(list[[1]][[1]])<>"-NumberOfTests");
    Test[Length[list],length,TestID->testName,TestFailureAction->"Abort",TestErrorAction->"Abort"];
    Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,list];
]

fcCheckEquivalenceViaDiracSimplify[list_List,length_Integer:0]:=Block[{testName=""},
  testName=(ToString(list[[1]][[1]])<>"-NumberOfTests");
    Test[Length[list],length,TestID->testName,TestFailureAction->"Abort",TestErrorAction->"Abort"];
    Map[Test[DiracSimplify[ToExpression[(#[[2]])]-ToExpression[(#[[3]])]],0 , TestID->#[[1]]]&,list]
]
If[TestStandard,

  Map[Get,testsStandard];
  Test[Length[testsStandard],100,TestID->"DiracSimplify-Standard-NumberOfTestTypes",
      TestFailureAction->"Abort",TestErrorAction->"Abort"];

  (* Testing simplification of Dirac matrix chains with no free Lorentz indices*)

  If[TestAllIndicesContracted,

    (*7567 items*)
    fcCheckIdentity[fcstDiracSimplifyIn4dimsAllIndicesContracted,124];
        fcCheckIdentity[fcstDiracSimplifyIn4dimsOneG5AllIndicesContractedNaive,1068];
        fcCheckIdentity[fcstDiracSimplifyIn4dimsOneG6AllIndicesContractedNaive,1068];
        fcCheckIdentity[fcstDiracSimplifyIn4dimsOneG7AllIndicesContractedNaive,1068];
        fcCheckIdentity[fcstDiracSimplifyIn4dimsOneG5OneG6AllIndicesContractedNaive,942];
        fcCheckIdentity[fcstDiracSimplifyIn4dimsOneG5OneG7AllIndicesContractedNaive,942];
        fcCheckIdentity[fcstDiracSimplifyIn4dimsOneG6OneG7AllIndicesContractedNaive,942];
        fcCheckIdentity[fcstDiracSimplifyIn4dimsTwoG5AllIndicesContractedNaive, 471];
        fcCheckIdentity[fcstDiracSimplifyIn4dimsTwoG6AllIndicesContractedNaive, 471];
        fcCheckIdentity[fcstDiracSimplifyIn4dimsTwoG7AllIndicesContractedNaive, 471];


    (*7567 items*)
    fcCheckIdentity[fcstDiracSimplifyInDdimsAllIndicesContracted,124];
    fcCheckIdentity[fcstDiracSimplifyInDdimsOneG5AllIndicesContractedNaive,1068];
    fcCheckIdentity[fcstDiracSimplifyInDdimsOneG6AllIndicesContractedNaive,1068];
      fcCheckIdentity[fcstDiracSimplifyInDdimsOneG7AllIndicesContractedNaive,1068];
    fcCheckIdentity[fcstDiracSimplifyInDdimsOneG5OneG6AllIndicesContractedNaive,942];
    fcCheckIdentity[fcstDiracSimplifyInDdimsOneG5OneG7AllIndicesContractedNaive,942];
    fcCheckIdentity[fcstDiracSimplifyInDdimsOneG6OneG7AllIndicesContractedNaive,942];
    fcCheckIdentity[fcstDiracSimplifyInDdimsTwoG5AllIndicesContractedNaive,471];
    fcCheckIdentity[fcstDiracSimplifyInDdimsTwoG6AllIndicesContractedNaive,471];
    fcCheckIdentity[fcstDiracSimplifyInDdimsTwoG7AllIndicesContractedNaive,471];
  ];

  (* Testing simplification of Dirac matrix chains with one free Lorentz index*)


  If[TestOneIndexFree,

      (*6999 items*)
      fcCheckIdentity[fcstDiracSimplifyIn4dimsOneIndexFree,1068];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG5OneIndexFreeNaive,942];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG6OneIndexFreeNaive,942];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG7OneIndexFreeNaive,942];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG5OneG6OneIndexFreeNaive,690];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG5OneG7OneIndexFreeNaive,690];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG6OneG7OneIndexFreeNaive,690];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsTwoG5OneIndexFreeNaive,345];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsTwoG6OneIndexFreeNaive,345];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsTwoG7OneIndexFreeNaive,345];

      (*6999 items*)
      fcCheckIdentity[fcstDiracSimplifyInDdimsOneIndexFree,1068];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG5OneIndexFreeNaive,942];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG6OneIndexFreeNaive,942];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG7OneIndexFreeNaive,942];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG5OneG6OneIndexFreeNaive,690];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG5OneG7OneIndexFreeNaive,690];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG6OneG7OneIndexFreeNaive,690];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsTwoG5OneIndexFreeNaive,345];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsTwoG6OneIndexFreeNaive,345];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsTwoG7OneIndexFreeNaive,345];
  ];



  (* Testing simplification of Dirac matrix chains with two free Lorentz indices*)


  If[TestTwoIndicesFree,

      (*5676 items*)
      fcCheckIdentity[fcstDiracSimplifyIn4dimsTwoIndicesFree,51];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG5TwoIndicesFreeNaive,345];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG6TwoIndicesFreeNaive,345];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG7TwoIndicesFreeNaive,345];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG5OneG6TwoIndicesFreeNaive,180];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG5OneG7TwoIndicesFreeNaive,180];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG6OneG7TwoIndicesFreeNaive,180];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsTwoG5TwoIndicesFreeNaive,1350];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsTwoG6TwoIndicesFreeNaive,1350];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsTwoG7TwoIndicesFreeNaive,1350];

      (*5676 items*)
      fcCheckIdentity[fcstDiracSimplifyInDdimsTwoIndicesFreeNaive,51];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG5TwoIndicesFreeNaive,345];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG6TwoIndicesFreeNaive,345];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG7TwoIndicesFreeNaive,345];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG5OneG6TwoIndicesFreeNaive,180];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG5OneG7TwoIndicesFreeNaive,180];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG6OneG7TwoIndicesFreeNaive,180];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsTwoG5TwoIndicesFreeNaive,1350];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsTwoG6TwoIndicesFreeNaive,1350];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsTwoG7TwoIndicesFreeNaive,1350];
  ];



  (* Testing simplification of Dirac matrix chains with three free Lorentz indices*)


  If[TestThreeIndicesFree,

      (*4705 items*)
      fcCheckIdentity[fcstDiracSimplifyIn4dimsThreeIndicesFree,115];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG5ThreeIndicesFreeNaive,900];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG6ThreeIndicesFreeNaive,900];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG7ThreeIndicesFreeNaive,900];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG5OneG6ThreeIndicesFreeNaive,420];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG5OneG7ThreeIndicesFreeNaive,420];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG6OneG7ThreeIndicesFreeNaive,420];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsTwoG5ThreeIndicesFreeNaive,210];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsTwoG6ThreeIndicesFreeNaive,210];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsTwoG7ThreeIndicesFreeNaive,210];

      (*4705 items*)
      fcCheckIdentity[fcstDiracSimplifyInDdimsThreeIndicesFree,115];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG5ThreeIndicesFreeNaive,900];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG6ThreeIndicesFreeNaive,900];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG7ThreeIndicesFreeNaive,900];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG5OneG6ThreeIndicesFreeNaive,420];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG5OneG7ThreeIndicesFreeNaive,420];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG6OneG7ThreeIndicesFreeNaive,420];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsTwoG5ThreeIndicesFreeNaive,210];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsTwoG6ThreeIndicesFreeNaive,210];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsTwoG7ThreeIndicesFreeNaive,210];


  ];


  (* Testing simplification of Dirac matrix chains with four free Lorentz indices*)


  If[TestFourIndicesFree,

      (*4110 items*)
      fcCheckIdentity[fcstDiracSimplifyIn4dimsFourIndicesFree,15];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG5FourIndicesFreeNaive,105];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG6FourIndicesFreeNaive,105];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG7FourIndicesFreeNaive,105];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG5OneG6FourIndicesFreeNaive,840];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG5OneG7FourIndicesFreeNaive,840];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG6OneG7FourIndicesFreeNaive,840];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsTwoG5FourIndicesFreeNaive,420];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsTwoG6FourIndicesFreeNaive,420];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsTwoG7FourIndicesFreeNaive,420];

      (*4110 items*)
      fcCheckIdentity[fcstDiracSimplifyInDdimsFourIndicesFree,15];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG5FourIndicesFreeNaive,105];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG6FourIndicesFreeNaive,105];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG7FourIndicesFreeNaive,105];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG5OneG6FourIndicesFreeNaive,840];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG5OneG7FourIndicesFreeNaive,840];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG6OneG7FourIndicesFreeNaive,840];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsTwoG5FourIndicesFreeNaive,420];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsTwoG6FourIndicesFreeNaive,420];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsTwoG7FourIndicesFreeNaive,420];

  ];

]





If[TestSlashes,

  Map[Get,testsSlashes];
  Test[Length[testsSlashes],60,TestID->"DiracSimplify-Slashes-NumberOfTestTypes"];

  (* Testing simplification of Dirac matrix chains with one Dirac slash and no free Lorentz indices*)

  If[TestOneSlashAllIndicesContracted,

    (*6999 items*)
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneSlashAllIndicesContracted,1068];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG5OneSlashAllIndicesContractedNaive,942];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG6OneSlashAllIndicesContractedNaive,942];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG7OneSlashAllIndicesContractedNaive,942];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG5OneG6OneSlashAllIndicesContractedNaive,690];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG5OneG7OneSlashAllIndicesContractedNaive,690];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG6OneG7OneSlashAllIndicesContractedNaive,690];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsTwoG5OneSlashAllIndicesContractedNaive,345];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsTwoG6OneSlashAllIndicesContractedNaive,345];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsTwoG7OneSlashAllIndicesContractedNaive,345];

    (*6999 items*)
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneSlashAllIndicesContracted,1068];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG5OneSlashAllIndicesContractedNaive,942];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG6OneSlashAllIndicesContractedNaive,942];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG7OneSlashAllIndicesContractedNaive,942];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG5OneG6OneSlashAllIndicesContractedNaive,690];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG5OneG7OneSlashAllIndicesContractedNaive,690];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG6OneG7OneSlashAllIndicesContractedNaive,690];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsTwoG5OneSlashAllIndicesContractedNaive,345];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsTwoG6OneSlashAllIndicesContractedNaive,345];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsTwoG7OneSlashAllIndicesContractedNaive,345];
  ];

  (* Testing simplification of Dirac matrix chains with two Dirac slashes and no free Lorentz indices*)


  If[TestTwoSlashesAllIndicesContracted,

      (*5688 items*)
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsTwoSlashesAllIndicesContracted,153];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG5TwoSlashesAllIndicesContractedNaive,1035];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG6TwoSlashesAllIndicesContractedNaive,1035];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG7TwoSlashesAllIndicesContractedNaive,1035];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG5OneG6TwoSlashesAllIndicesContractedNaive,540];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG5OneG7TwoSlashesAllIndicesContractedNaive,540];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG6OneG7TwoSlashesAllIndicesContractedNaive,540];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsTwoG5TwoSlashesAllIndicesContractedNaive,270];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsTwoG6TwoSlashesAllIndicesContractedNaive,270];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsTwoG7TwoSlashesAllIndicesContractedNaive,270];

    (*5688 items*)
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsTwoSlashesAllIndicesContracted,153];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG5TwoSlashesAllIndicesContractedNaive,1035];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG6TwoSlashesAllIndicesContractedNaive,1035];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG7TwoSlashesAllIndicesContractedNaive,1035];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG5OneG6TwoSlashesAllIndicesContractedNaive,540];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG5OneG7TwoSlashesAllIndicesContractedNaive,540];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG6OneG7TwoSlashesAllIndicesContractedNaive,540];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsTwoG5TwoSlashesAllIndicesContractedNaive,270];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsTwoG6TwoSlashesAllIndicesContractedNaive,270];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsTwoG7TwoSlashesAllIndicesContractedNaive,270];
  ];



  (* Testing simplification of Dirac matrix chains with three Dirac slashes and no free Lorentz indices*)


  If[TestThreeSlashesAllIndicesContracted,

    (*6700 items*)
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsThreeSlashesAllIndicesContracted,100];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG5ThreeSlashesAllIndicesContractedNaive,600];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG6ThreeSlashesAllIndicesContractedNaive,600];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG7ThreeSlashesAllIndicesContractedNaive,600];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG5OneG6ThreeSlashesAllIndicesContractedNaive,1000];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG5OneG7ThreeSlashesAllIndicesContractedNaive,1000];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsOneG6OneG7ThreeSlashesAllIndicesContractedNaive,1000];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsTwoG5ThreeSlashesAllIndicesContractedNaive,600];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsTwoG6ThreeSlashesAllIndicesContractedNaive,600];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyIn4dimsTwoG7ThreeSlashesAllIndicesContractedNaive,600];

    (*6700 items*)
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsThreeSlashesAllIndicesContracted,100];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG5ThreeSlashesAllIndicesContractedNaive,600];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG6ThreeSlashesAllIndicesContractedNaive,600];
      fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG7ThreeSlashesAllIndicesContractedNaive,600];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG5OneG6ThreeSlashesAllIndicesContractedNaive,1000];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG5OneG7ThreeSlashesAllIndicesContractedNaive,1000];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsOneG6OneG7ThreeSlashesAllIndicesContractedNaive,1000];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsTwoG5ThreeSlashesAllIndicesContractedNaive,600];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsTwoG6ThreeSlashesAllIndicesContractedNaive,600];
    fcCheckEquivalenceViaDiracSimplify[fcstDiracSimplifyInDdimsTwoG7ThreeSlashesAllIndicesContractedNaive,600];
  ];


]
