(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: OPEi*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  the m *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`OPEi`",
             "HighEnergyPhysics`FeynCalc`"];

OPEi::usage= "OPEi is an dummy index in OPESum.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   


Unprotect[Re];
Re /: Re[OPEi] > -3 := True;
Re /: Re[OPEi] > -2 := True;
Re /: Re[OPEi] > -1 := True;
Re /: Re[OPEi] >  0 := True;
Re /: Re[OPEi] >  1 := True;

MakeContext[PositiveInteger, DataType];

DataType[OPEi, PositiveInteger] = True;

OPEi /: MakeBoxes[OPEi ,TraditionalForm] := "i";

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "OPEi | \n "]];
Null
