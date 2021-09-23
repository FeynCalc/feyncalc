(* ::Package:: *)

 


(* ::Section:: *)
(*$FCShowIEta*)


(* ::Text:: *)
(*The Boolean setting of `$FCShowIEta` determines whether $i \eta$ should be displayed in the typesetting of propagator objects (except for `FAD`s) or not. This setting affects only the TraditionalForm typesetting and has absolutely no influence on the internal handling of propagator denominators in FeynCalc.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SFAD](SFAD.md), [CFAD](CFAD.md), [GFAD](GFAD.md).*)


(* ::Subsection:: *)
(*Examples*)


$FCShowIEta
SFAD[{p,m^2}]


$FCShowIEta=False
SFAD[{p,m^2}]


$FCShowIEta=True
