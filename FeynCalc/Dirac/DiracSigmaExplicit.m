(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: substitute DiracSigma in terms of DiracGamma's *)

(* ------------------------------------------------------------------------ *)

DiracSigmaExplicit::usage =
"DiracSigmaExplicit[exp] inserts in exp the definition of \
DiracSigma. DiracSigmaExplict is also an option of \
DiracSimplify.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DiracSigmaExplicit`Private`"];

dirsigex[a_DiracGamma, b_DiracGamma] :=
	dirsigex[a,b] = I/2 (DOT[a, b] - DOT[b, a]);

dirsigex[DiracMatrix[a_, b_]] :=
	dirsigex[DiracMatrix[a,b]] =
		I/2 (DiracMatrix[a, b] - DiracMatrix[b, a]);

dirsigex[DiracSlash[a_, b_]] :=
	dirsigex[DiracSlash[a,b]] =
		I/2 (DiracSlash[a, b] - DiracSlash[b, a]);

DiracSigmaExplicit[x_] :=
	FCI[x]/. DiracSigma -> dirsigex;

FCPrint[1,"DiracSigmaExplicit.m loaded"];
End[]
