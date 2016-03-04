(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* :Title: FeynmanParametrize*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

FeynmanParametrize::usage =
"FeynmanParametrize[exp,k] introduces Feynman parameters for \
all one-loop integrals in exp (k = integration momentum).";


(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FeynmanParametrize`Private`"]

Options[FeynmanParametrize] = {
	FeynmanParameterNames -> {FCGV["x"], FCGV["y"], FCGV["z"]}
};

FeynmanParametrize[exp_, Momentum[q_,_:4], opt:OptionsPattern[]] :=
	FeynmanParametrize[exp,q,opt];

FeynmanParametrize[exp_,q_, OptionsPattern[]] :=
	Block[ {},
		FeynAmpDenominatorCombine[exp] /. FeynAmpDenominator[aa__] :>
			Apply[fpar[OptionValue[FeynmanParameterNames]][q//nomom],{FCI[FeynAmpDenominator[aa]]}]
	];

nomom[y_] :=
	y/.Momentum[aa_,_:4]:>aa;

pc[h_,m_] :=
	Expand[ExpandScalarProduct[Pair[h,h]]-m^2];

fpar[{par__}][k_][FeynAmpDenominator[
					PD[Momentum[k_,di_:4] + min1_. Momentum[p1_,di1_:4], 0],
					PD[Momentum[k_,di_:4] + min2_. Momentum[p2_,di1_:4],0]
				]] :=
	DOT[Integratedx[{par}[[1]],0,1],
		( 1/(Expand[ExpandScalarProduct[
						Pair[Momentum[k,di] + min1 Momentum[p1,di1],Momentum[k,di] +
						min1 Momentum[p1,di1]] ({par}[[1]]) +
						(1-({par}[[1]])) Pair[Momentum[k,di] +
						min2 Momentum[p2,di1], Momentum[k,di] + min2 Momentum[p2,di1]]]])^2)];

(* 2-point; alpha = 2*)
fpar[{par__}][k_][FeynAmpDenominator[
					PD[Momentum[k_,di_:4], 0],
					PD[Momentum[k_,di_:4] +	min_. Momentum[p_,_:4],0]
				]] :=
	DOT[Integratedx[{par}[[1]],0,1],
		( 1/(Pair[Momentum[k,di], Momentum[k,di]] -	2 (-min) ({par}[[1]]) Pair[Momentum[k,di], Momentum[p,di]] +
		({par}[[1]]) min^2 Pair[Momentum[p,di],Momentum[p,di]])^2)];

fpar[{par__}][_][FeynAmpDenominator[
					n1:PD[a_, m1_]..,
					n2:PD[b_ ,m2_]..]] :=
	DOT[Integratedx[{par}[[1]],0,1],
		(Gamma[Length[{n1}]+Length[{n2}]] / Gamma[Length[{n1}]] Gamma[Length[{n2}]] *
		({par}[[1]])^(Length[{n1}]-1) (1-({par}[[1]]))^(Length[{n2}]-1)*
		1/(pc[a,m1] ({par}[[1]]) + (1-({par}[[1]])) pc[b,m2])^(Length[{n1}]+Length[{n2}]))];

(* 2-point; alpha = 3	*)
fpar[{par__}][k_][FeynAmpDenominator[
					PD[Momentum[k_,di_:4], 0],
					PD[Momentum[k_,di_:4], 0],
					PD[Momentum[k_,di_:4] + min_. Momentum[p_,_:4],0]]] :=
		Gamma[3] DOT[Integratedx[{par}[[1]],0,1],((1-({par}[[1]]))/(Pair[Momentum[k,di], Momentum[k,di]] -
		2 (-min) ({par}[[1]]) Pair[Momentum[k,di], Momentum[p,di]] +
		({par}[[1]]) min^2 Pair[Momentum[p,di],Momentum[p,di]])^3)];

(* 3-point; alpha = 3	*)
fpar[{par__}][k_][FeynAmpDenominator[
					PD[Momentum[k_,di_:4], 0],
					PD[Momentum[k_,di_:4] + p1_,0],
					PD[Momentum[k_,di_:4] + p3_,0]]] :=
		Block[{x,y,k2,kp1,p12,kp3,p32},
			x = {par}[[2]];
			y = {par}[[1]];
			k2 = Pair[Momentum[k,di], Momentum[k,di]];
			kp1 = - Pair[Momentum[k,di], p1]//ExpandScalarProduct;
			kp3 = - Pair[Momentum[k,di], p3]//ExpandScalarProduct;
			p12 = Pair[p1, p1] // ExpandScalarProduct;
			p32 = Pair[p3, p3] // ExpandScalarProduct;
			Gamma[3] DOT[Integratedx[y,0,1] , Integratedx[x,0,1],
			(y /( k2-2y x kp1 - 2 y (1-x) kp3 + y x p12 + y (1-x) p32)^3)]
		];

(* 3-point; alpha = 4	*)
fpar[{par__}][k_][FeynAmpDenominator[
					PD[Momentum[k_,di_:4], 0],
					PD[Momentum[k_,di_:4], 0],
					PD[Momentum[k_,di_:4] + p1_,0],
					PD[Momentum[k_,di_:4] + p3_,0]]] :=
		Block[ {x,y,k2,kp1,kp3,p12,p32},
			x = {par}[[1]];
			y = {par}[[2]];
			k2 = Pair[Momentum[k,di], Momentum[k,di]];
			kp1 = - Pair[Momentum[k,di], p1]//ExpandScalarProduct;
			kp3 = - Pair[Momentum[k,di], p3]//ExpandScalarProduct;
			p12 = Pair[p1, p1] // ExpandScalarProduct;
			p32 = Pair[p3, p3] // ExpandScalarProduct;
			Gamma[4] DOT[Integratedx[y,0,1] , Integratedx[x,0,1],
			(y (1-y) /( k2-2y x kp1 - 2 y (1-x) kp3 + y x p12 + y (1-x) p32)^4)]
		];

(*
(* general a^i b^j c^k *)
fpar[{par__}][_][FeynAmpDenominator[
	ni:PD[na_, m1_]..,
	nj:PD[nb_ ,m2_]..,
	nk:PD[nc_ ,m3_]..  ]
				] :=
	Block[{i=Length[{ni}],j=Length[{nj}],k=Length[{nk}],
		x = {par}[[1]], y = {par}[[2]],
		a = pc[na, m1], b = pc[nb, m2], c = pc[nc, m3]
		},
FCPrint[1,"GENERAL fpar "];
		DOT[Integratedx[x,0,1] , Integratedx[y,0,1] ,
	(Gamma[i+j+k]/Gamma[i]/Gamma[j]/Gamma[k] *
	x^(i-1) (1-x)^(j-1) y^(i+j-1) (1-y)^(k-1) /
	((a x + (1-x) b) y + (1-y) c)^(i+j+k)
	)]   ];
*)

(* If the above failed. F.Orellana, 11/9-2002 *)
fpar[{__}][_][p_] :=
	(Print["Unknown denominator ", p, ". Try FeynmanParametrize1."];
	p);



FCPrint[1,"FeynmanParametrize.m loaded."];
End[]
