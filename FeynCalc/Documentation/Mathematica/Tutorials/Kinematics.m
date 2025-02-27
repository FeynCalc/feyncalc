(* ::Package:: *)

 


(* ::Section:: *)
(*Kinematics*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md).*)


(* ::Subsection:: *)
(*Manipulations of scalar products*)


(* ::Text:: *)
(*FeynCalc allows you to specify the values of scalar products before doing the calculation.*)


SP[p,q]=s;


SP[p,q]


FV[q,\[Mu]]FV[q,\[Nu]](FV[p,\[Mu]]FV[p,\[Nu]]-MT[\[Mu],\[Nu]]/SP[p,p])
%//Contract


(* ::Text:: *)
(*To clear the previously set values, use*)


FCClearScalarProducts[]


SP[p,q]


FV[q,\[Mu]]FV[q,\[Nu]](FV[p,\[Mu]]FV[p,\[Nu]]-MT[\[Mu],\[Nu]]/SP[p,p])
%//Contract


(* ::Text:: *)
(*A good habit is to always apply `FCClearScalarProducts[]` before setting the values, like in*)


FCClearScalarProducts[];
SP[p1,p1]=m1^2;
SP[p2,p2]=m2^2;


(* ::Text:: *)
(*Setting up the kinematics in advance improves performance of FeynCalc and leads to more compact results. The results with the fully arbitrary kinematics are the most complicated and the longest ones.*)
