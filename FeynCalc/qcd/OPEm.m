(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: OPEm*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  the m *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`OPEm`",{"HighEnergyPhysics`FeynCalc`"}];

OPEm::"usage"= "OPEm is m of the operator product expansion.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

PositiveInteger = MakeContext["CoreObjects","PositiveInteger"];
MakeContext[DataType, OPEi];
DataType[OPEm, PositiveInteger] = True;

OPEm /:
   MakeBoxes[OPEm ,TraditionalForm] :=
If[$Color =!= True,
   "m",
    TagBox[StyleBox["m",
       FontColor -> RGBColor[0,1,0.2]], Tag -> OPEm]
  ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "OPEm | \n "]];
Null
