 
(* ::Section:: *)
(* FieldStrength *)
(* ::Text:: *)
(*FieldStrength[\[Mu], \[Nu], a] is the field strength tensor $\partial _{\mu }A_{\nu }^a - \partial _{\nu }A_{\mu }^a + g_sA_{\mu }^bA_{\nu }^c f^{\text{abc}}$.   FieldStrength[\[Mu], \[Nu]] is the field strength tensor $\text{cell}\left(\text{TextData}\left[\text{cell}\left(\partial _{\mu }A_{\nu }-\partial _{\nu }A_{\mu },\text{InlineFormula}\right)\right],\text{InlineFormula}\right).$The name of the field ($text{A}$) and the coupling constant ($text{g}$) can be set through the options or by additional arguments. The first two indices are interpreted as type LorentzIndex, except OPEDelta, which is converted to $text{Momentum}[text{OPEDelta}]$..*)


(* ::Subsection:: *)
(* Examples *)
QuantumField
FieldStrength[\[Mu],\[Nu]]

FieldStrength[\[Mu],\[Nu],a]

FieldStrength[\[Mu],\[Nu],Explicit->True]

FieldStrength[\[Mu],\[Nu],a,Explicit->True]

FieldStrength[\[Mu],\[Nu],a,CouplingConstant->-SMP["g_s"],Explicit->True]
