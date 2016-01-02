

(* :Title: ToLarin *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Dirac trace calculation  (see also DiracToLarinace) *)

(* ------------------------------------------------------------------------ *)

ToLarin::usage =
"ToLarin[exp] translates gamma[mu].gamma[5] into \
-I/6 Eps[mu,nu,la,si] gamma[nu,la,si].";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ToLarin`Private`"]

Options[ToLarin] = {Dimension -> D};

ToLarin[x_, ru___Rule] :=
	Block[ {tt,fi1,fi2,fi3,drsi,temp2, doot, dim},
		dim = Dimension /. {ru} /. Options[ToLarin];
		drsi = LeviCivitaSign /. Options[TR];
		(*drsi is usually -1 *)
		tt = FeynCalcInternal[x] /. DOT -> doot;
		temp2 = tt //. doot[a___, DiracGamma[mUU_, _Symbol], DiracGamma[5], b___] :>
		({fi1, fi2, fi3} = LorentzIndex[#,dim]& /@ Unique[{"du","du","du"}];
		(drsi I/6 Eps[mUU, fi1, fi2, fi3] doot @@ Join[{a},
		Map[DiracGamma[#,dim]&, {fi1,fi2,fi3}], {b}]));
		temp2 = temp2 /. doot -> DOT
	];

FCPrint[1,"ToLarin.m loaded."];
End[]
