(* ::Package:: *)

 


(* ::Section:: *)
(*FCVerbose*)


(* ::Text:: *)
(*`FCVerbose` is an option for numerous functions that allows to specify a local value of `$VeryVerbose` inside those functions. When set to a positive integer, all the debugging information inside the function will be given according to the value of `FCVerbose`, while the debugging output of other functions will be still governed by the value of `$VeryVerbose`. Following values are common*)


(* ::Text:: *)
(*- `1` - a brief description of the calculational steps including timings*)


(* ::Text:: *)
(*- `2` - somewhat more debugging information*)


(* ::Text:: *)
(*- `3` - lots of debugging output, probably useful only for developers*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [$VeryVerbose]($VeryVerbose.md).*)


(* ::Subsection:: *)
(*Examples*)


DiracSimplify[GA[\[Mu],\[Nu],\[Rho],\[Mu],\[Nu]],FCVerbose->1]


DiracSimplify[GA[\[Mu],\[Nu],\[Rho],\[Mu],\[Nu]],FCVerbose->2]


DiracSimplify[GA[\[Mu],\[Nu],\[Rho],\[Mu],\[Nu]],FCVerbose->3]
