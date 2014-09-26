(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CovariantFieldDerivative *)

(* :Author: Frederik Orellana *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 20 February 2003 at 22:01 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`CovariantFieldDerivative`",{"HighEnergyPhysics`FeynCalc`"}];

CovariantFieldDerivative::"usage" =
    "CovariantFieldDerivative[f[x],x,{li1,li2,...},opts] is a covariant \
derivative of f[x] with respect to space-time variables x and with Lorentz \
indices li1, li2,... CovariantFieldDerivative has only typesetting \
definitions by default. The user is must supply his/her own \
definition of the actual function.";

CDr::"usage"=
"CDr is the shorthand notation for
CovariantFieldDerivative";


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

LorentzIndex = MakeContext["CoreObjects","LorentzIndex"];
PartialD = MakeContext["CoreObjects","PartialD"];
QuantumField = MakeContext["CoreObjects","QuantumField"];

MakeContext[ $DistributiveFunctions, $Multiplications];

CDr=CovariantFieldDerivative;

Options[CovariantFieldDerivative] = {};

CovariantFieldDerivative /:
    MakeBoxes[CovariantFieldDerivative[a_, lis___HighEnergyPhysics`FeynCalc`CoreObjects`PartialD,
___Rule], TraditionalForm] :=
RowBox[{SubscriptBox[
MakeBoxes[ StyleForm["\[ScriptCapitalD]", FontSlant -> "Italic"]],
     RowBox[MakeBoxes[TraditionalForm[#]] & /@ {lis}]], "(",
   MakeBoxes[TraditionalForm[a]], ")"}];

CovariantFieldDerivative /:
MakeBoxes[CovariantFieldDerivative[a_, _, {lis___}, ___Rule],
TraditionalForm] :=
RowBox[{SubscriptBox[
MakeBoxes[ StyleForm["\[ScriptCapitalD]", FontSlant -> "Italic"]],
     RowBox[MakeBoxes[TraditionalForm[#]] & /@ {lis}]], "(",
   MakeBoxes[TraditionalForm[a]], ")"}];

CovariantFieldDerivative /:
MakeBoxes[CovariantFieldDerivative[a_, {lis___}, ___Rule],
TraditionalForm] :=
RowBox[{SubscriptBox[
MakeBoxes[ StyleForm["\[ScriptCapitalD]", FontSlant -> "Italic"]],
     RowBox[MakeBoxes[TraditionalForm[#]] & /@ {lis}]], "(",
   MakeBoxes[TraditionalForm[a]], ")"}];

CovariantFieldDerivative /:
MakeBoxes[CovariantFieldDerivative[a_, _,
lis___HighEnergyPhysics`FeynCalc`CoreObjects`LorentzIndex,
___Rule],
TraditionalForm] :=
RowBox[{SubscriptBox[
MakeBoxes[ StyleForm["\[ScriptCapitalD]", FontSlant -> "Italic"]],
     RowBox[MakeBoxes[TraditionalForm[#]] & /@ {lis}]], "(",
   MakeBoxes[TraditionalForm[a]], ")"}];

CovariantFieldDerivative /:
    MakeBoxes[CovariantFieldDerivative[a_, lis___HighEnergyPhysics`FeynCalc`CoreObjects`PartialD],
TraditionalForm] :=
RowBox[{SubscriptBox[
MakeBoxes[StyleForm["\[ScriptCapitalD]", FontSlant -> "Italic"]],
RowBox[MakeBoxes[TraditionalForm[#]] & /@ {lis}]], "(",
MakeBoxes[TraditionalForm[a]], ")"}];

CovariantFieldDerivative /:
    MakeBoxes[CovariantFieldDerivative[a_, lis___List],
TraditionalForm] :=
RowBox[{SubscriptBox[
MakeBoxes[StyleForm["\[ScriptCapitalD]", FontSlant -> "Italic"]],
RowBox[MakeBoxes[TraditionalForm[#]] & /@ {lis}]], "(",
MakeBoxes[TraditionalForm[a]], ")"}];


End[]; EndPackage[];


(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "CovariantFieldDerivative | \n "]];
Null
