(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ToDistribution*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 10 July '98 at 12:01 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

ToDistribution::usage =
"ToDistribution[exp, x] replaces (1-x)^(a Epsilon - 1) in exp by 1/(a Epsilon)
DeltaFunction[1-x] + 1/(1-x) + a Epsilon Log[1-x]/(1-x) + 1/2 a^2 Epsilon^2
Log[1-x]^2/(1-x)] and (1-x)^(a Epsilon - 2) in exp by -1/(a Epsilon)
DeltaFunctionPrime[1-x] + 1/(1-x)^2 + (a Epsilon) Log[1-x]/(1-x)^2 + a^2
Epsilon^2/2 Log[1-x]^2/(1-x)^2 + a^3 Epsilon^3/6 Log[1-x]^3/(1-x)^2.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ToDistribution`Private`"]

Options[ToDistribution] = {
	PlusDistribution->Identity
};

ToDistribution[exp_, x_, opt___Rule] :=
	Apart3[exp,x] /. { (1-x)^(a_. Epsilon -1) :>
			(
				1/(a Epsilon)  DeltaFunction[1-x] +
				PlusDistribution[1/(1-x)] +
				a Epsilon PlusDistribution[Log[1-x]/(1-x)] +
				1/2 a^2 Epsilon^2 PlusDistribution[Log[1-x]^2/(1-x)] +
				1/6 a^3 Epsilon^3 PlusDistribution[Log[1-x]^3/(1-x)]
			),
			(1-x)^(a_. Epsilon -2) :>
			-DeltaFunctionPrime[1-x]/Epsilon +
			(
				PlusDistribution[1/(1-x)^2] +
				(a Epsilon ) PlusDistribution[Log[1-x]/(1-x)^2] +
				a^2 Epsilon^2/2 PlusDistribution[Log[1-x]^2/(1-x)^2] +
				a^3 Epsilon^3/6 PlusDistribution[Log[1-x]^3/(1-x)^2]
			),
			(1-x)^(a_. Epsilon -3) :>
				DeltaFunctionDoublePrime[1-x]/2/Epsilon +
				PlusDistribution[(1 - x)^(-3)] +
				(a Epsilon*PlusDistribution[Log[1 - x]/(1 - x)^3]) +
				(a^2 Epsilon^2*PlusDistribution[Log[1 - x]^2/(1 - x)^3]/2) +
				(a^3 Epsilon^3*PlusDistribution[Log[1 - x]^3/(1 - x)^3]/6)
			} /. PlusDistribution :>
					(PlusDistribution /. {opt} /.Options[ToDistribution]);

FCPrint[1,"ToDistribution.m loaded."];
End[]
