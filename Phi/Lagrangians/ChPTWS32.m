(* *************************************************************** *)
(*                                                                 *)
(*                      ChPTWS32                                   *)
(*                                                                 *)
(* *************************************************************** *)

(* 
   Author:              F.Orellana 1999

   Mathematica Version: 3.0 

   Requirements:        FeynCalc > 3, Phi 

   Summary:             Lagrangian for Phi

   Description:         The lowest order CP conserving weak ChPT
                        lagrangian with coupling to a scalar source.
    
                        Taken from Ecker, Kambor and Wyler
                        Resonances in the weak chiral Lagrangian,
                        CERN-TH.6610/92    
*)


Begin["HighEnergyPhysics`Phi`Objects`"];

(* -------------------------------------------------------------- *)

(*This is just to get things in the right context*)
HighEnergyPhysics`Phi`Objects`ChPTW3;
HighEnergyPhysics`Phi`Objects`ChPT3;

ChPTWS32::"usage"=
"ChPTWS32 is the name of the file containing the definitions for 
Lagrangian[ChPTWS3[2]] the simplest weak ChPT lagrangian.  
To evaluate use ArgumentsSupply";

C2::"usage"=
"C2 := CouplingConstant[ChPTW3[2],1] is one of the constants of the 
lowest order weak ChPT lagrangian";

C5::"usage"=
"C5 := CouplingConstant[ChPTW3[2],2] is one of the constants of the 
lowest order weak ChPT lagrangian";

(* -------------------------------------------------------------- *)

End[];

(* -------------------------------------------------------------- *)

(* Abbreviations *)

C2 = CouplingConstant[ChPTW3[2],1];
C5 = CouplingConstant[ChPTW3[2],2];

(* ---------------------------------------------------------------- *)

(* Box definitions *)

CouplingConstant/:
  MakeBoxes[
    CouplingConstant[ChPTW3[2],
   1,st___RenormalizationState,
      sc___RenormalizationScheme,qs___ExpansionState],
    TraditionalForm]:=
  SubsuperscriptBox[MakeBoxes[StyleForm["c",FontSlant->"Italic"]][[1]],
    MakeBoxes[TraditionalForm[2]],
    RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
          MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
          MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

CouplingConstant/:
  MakeBoxes[
    CouplingConstant[ChPTW3[2],
   2,st___RenormalizationState,
      sc___RenormalizationScheme,qs___ExpansionState],
    TraditionalForm]:=
  SubsuperscriptBox[MakeBoxes[StyleForm["c",FontSlant->"Italic"]][[1]],
    MakeBoxes[TraditionalForm[5]],
    RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
          MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
          MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

(* --------------------------------------------------------------- *)

Lagrangian[ChPTWS3[2]]:=

C2*UTrace[ NM[QuantumField[Particle[Scalar[1]]],
              UGeneratorMatrix[6],
              Adjoint[CDr[MM,{\[Mu]}]], CDr[MM,{\[Mu]}]] ] +

C5*UTrace[ NM[QuantumField[Particle[Scalar[1]]],
              UGeneratorMatrix[6], Adjoint[MM], UChiMatrix]+
           NM[QuantumField[Particle[Scalar[1]]],
          UGeneratorMatrix[6], Adjoint[UChiMatrix], MM] ];
    
(* --------------------------------------------------------------- *)

FieldsSet[ChPTWS3[2]]:=
{IsoVector[
QuantumField[Particle[PhiMeson,RenormalizationState[0]]]
]};

$Lagrangians=Union[$Lagrangians,{ChPTWS3[2]}];
