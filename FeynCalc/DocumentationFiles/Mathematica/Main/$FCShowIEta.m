 
(* ::Section:: *)
(* $FCShowIEta *)
(* ::Text:: *)
(*$FCShowIEta The boolean setting of $FCShowIEta detrmines whether I \[Eta] should be displayed in the typesetting of GFAD and GenericPropagatorDenominator objects or not. This setting affects only the TraditionalForm typesetting and has absolutely no influence on the internal handling of propagator denominators in FeynCalc..*)


(* ::Subsection:: *)
(* Examples *)
$FCShowIEta

SFAD[{p,m^2}]

$FCShowIEta=False

SFAD[{p,m^2}]

$FCShowIEta=True
