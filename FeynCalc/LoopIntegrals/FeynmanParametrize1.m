(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* :Title: FeynmanParametrize1 *)

(* :Author: Frederik Orellana *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 16 March 2001 at 15:36 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

FeynmanParametrize1::usage =
"***EXPERIMENTAL***\n
FeynmanParametrize1[exp,k,Method->Denominator] introduces Feynman \
parameters for all one-loop integrals in exp (k = integration momentum) using \
formula (11.A.1) from \"The Quantum Theory of Fields\" vol. 1 by \
Steven Weinberg.  FeynmanParametrize1[exp,k,Method->Exp] introduces Feynman \
parameters for all one-loop integrals in exp (k = integration momentum) using \
1/(A-I eps) = I Integrate[Exp[-I x (A-I eps)],{x,0,Infinity}, \
Assumptions->{Arg[A]==0,Arg[eps]==0}]. \
In this case, when the option Integrate is set to True, odd factors of \
k-tensors are dropped and even factors are replaced according to \
Itzykson&Zuber (8-117).";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FeynmanParametrize1`Private`"]

Options[FeynmanParametrize1] = {
	FeynmanParameterNames -> {FCGV["a"], FCGV["b"], FCGV["c"], FCGV["d"], FCGV["e"]},
	Method -> Denominator,
	Integrate -> True,
	Flatten -> True,
	CompleteSquare -> True
};

FeynmanParametrize1::"noint" = "Could not integrate out `1` in `2`. Got `3`.";

fpar[{par__}][k_][f:FeynAmpDenominator[PropagatorDenominator[_,___]..], opts___Rule] :=
	Block[ {i, n = Length[f], dum, pars = dum@@{par}},
		pars = ReplacePart[pars,0,n];
		(n-1)! * Dot[(DOT@@Table[Integratedx[pars[[i]],0,pars[[i-1]]],{i,n-1}]),
		Sum[(Pair[f[[i,1]],f[[i,1]]]-f[[i,2]]^2)*
			(pars[[n-i]]-pars[[n-i+1]]),{i,n}]^(-n)] /. dum->1
	];

epar[{par__}][k_][(f:FeynAmpDenominator[PropagatorDenominator[_,___]..]), opts___Rule] :=
	epar[{par}][k][dum*f,opts]/.dum->1;

epar[{par__}][k_][
	(rest___)(f:FeynAmpDenominator[PropagatorDenominator[_,___]..]), opts___Rule] :=
	Block[ {i, n = Length[f], pars = {par}, res, y, ee, rr, cc, exp,
			mom, res1, rest1, dum, k2coeff, p},

	(* Exponential factors already there *)
		k2coeff = 0;
		Which[
			MatchQ[Times[rest],_*E^(_?(!FreeQ[#,k]&))](*===True*),
			rest1 = dum*rest /. dum*rr_*E^(ee_?(!FreeQ[#,k]&)) :> {rr, ee};
			k2coeff = Coefficient[rest1[[2]] /. Momentum[k,_] :> Momentum[k], (*Momentum[k]*)(*Bug?*)
									Pair[Momentum[k], Momentum[k]]],
			MatchQ[Times[rest], E^(_?(!FreeQ[#,k]&))](*===True*),
			rest1 = dum*rest /. dum*E^(ee_?(!FreeQ[#,k]&)) :> {1, ee};
			k2coeff = Coefficient[rest1[[2]] /. Momentum[k,_] :> Momentum[k],
									Pair[Momentum[k], Momentum[k]]],
			True,
			rest1 = {rest, 0};
			k2coeff = 0];
		FCPrint[2,"Coefficient of ", k^2, "in exponent: ", k2coeff];

		(* Hmmm... *)
		(*If[k2coeff===0,k2coeff=1];*)

		(* Formula (3) of Murayama with the extra exponent; (-I)'s dropped to suit Mathematica*)
		res1 = (*(-I)^n*) rest1[[1]] * (* (1/k2coeff) *) (* k2coeff * *)
				DOT[(DOT@@Table[Integratedx[pars[[i]], 0, Infinity], {i, n}]),
				(Exp[rest1[[2]]] *
				Product[ Exp[ (* k2coeff * *) (Pair[f[[i,1]],f[[i,1]]]-f[[i,2]]^2) *
								pars[[i]] ], {i,n} ])];
		If[ CompleteSquare/.Flatten[{opts}]/.Options[FeynmanParametrize1],
			res = res1 /. Exp[ee_] :>
			(FCPrint[2,"Completing the square of ", k, " in the exponent ", ee];
			cc = CompleteSquare[ee,k,Unique["y"]];
			(* The rules substituting the old with the
				new integration momentum and back *)
			squarerule = Solve[Equal@@cc[[2]],
			mom = Union[Cases[cc[[2]],Momentum[k,___],Infinity]][[1]]][[1,1]];
			endrule = cc[[2,1]]->mom;
			Exp[cc[[1]]]),
			squarerule = {};
			endrule = Momentum[k,d___] :> Momentum[k,d];
			res = res1
		]
	];

(* ********************************************************** *)

FeynmanParametrize1[exp_,q_,opt___Rule] :=
	Block[ {(*dim,dims,rul,aa,aaa,ee,qfacs,noqfacs,qq,res,par,dum,
	efpar,rr,t,cc,ef,co,y,lis,sil,liss,re,wrap,b,qfac,noqfac,ints,res1,dd*)},
		endrule = {};
		qq = q/.Momentum[aaa_,___]:>aaa;
		par = FeynmanParameterNames/.{opt}/.Options[FeynmanParametrize1];

		(* Make sure all momenta have the same dimension *)
		dims = Union[Cases[exp, (Dimension->_)|(Momentum[_,_]),Infinity]];
		If[ dims =!= {},
			dims = Union[(#[[2]])& /@ dims]
		];
		Which[
		Length[dims] == 0,
		dim = Sequence[],
		Length[dims] == 1,
		dim = dims[[1]],
		True,
		dim = dims[[1]];
		rul = ((Rule@@#)& /@ Transpose[
		{dims, Table[dim,{Length[dims]}]}]);
		exp = exp //. rul;
		];

		(* Choose method *)
		Which[
		(Method/.{opt}/.Options[FeynmanParametrize1])===Denominator,
			efpar = fpar,
		(Method/.{opt}/.Options[FeynmanParametrize1])===Exp,
			efpar = epar,
		True,
			efpar = fpar
		];
		FCPrint[1,"Using Method ", (Method/.{opt}/.Options[FeynmanParametrize1])];

		(* Add extra parameter names if necessary *)
		If[ (len = Max[Length[Cases[#,PropagatorDenominator[_?(!FreeQ[#,qq]&)],Infinity]]& /@
			Cases[exp,_FeynAmpDenominator,Infinity]])>
			(len1 = Length[par]),
			par = Join[par,Table[Unique["x"],{len-len1}]];
			FCPrint[1,"Added extra parameter names ", par]
		];
		FCPrint[2,"Simplifying expression"];
		res1 = ExpandProductExpand[FeynAmpDenominatorCombine[exp//.
				(*First flatten out DOT products with Integratedx's*)
				If[ (Flatten/.Flatten[{opt}]/.Options[FeynmanParametrize1])===True,
					(b_?(((!FreeQ[#,q])&&FreeQ[#,Integratedx])&))*
					DOT[ints:(Integratedx[_,_,_]..),r_?((!FreeQ[#,q]&&FreeQ[#,Integratedx])&)] :>
					(If[ Head[b]===Times,
						qfac = Select[List@@{b},(!FreeQ[#,q]&)];
						noqfac = Complement[b,qfac],
						qfac = b;
						noqfac = 1
					];
					noqfac*DOT[ints,qfac*r]),
					{}
				]
				]];
		FCPrint[2,"Finished simplifying expression. ",
					Length[Cases[{res1},
					FeynAmpDenominator,
					Infinity,Heads->True]], " FeynAmpDenominator(s) present"];
		res = dum*res1 /.

		(rr___)*FeynAmpDenominator[aa__] :>

		(
				(* Operate only on denominator fators containing q *)
				qfacs = Select[{aa},((!FreeQ[#,qq])&)];
				noqfacs = Complement[{aa},qfacs];
				qfacs = FeynAmpDenominator@@qfacs/.FeynAmpDenominator[]->1;
				noqfacs = FeynAmpDenominator@@noqfacs/.FeynAmpDenominator[]->1;
				If[ (Integrate/.{opt}/.Options[FeynmanParametrize1])=!=True ||
					(CompleteSquare/.{opt}/.Options[FeynmanParametrize1])=!=True ||
								efpar=!=epar,
					(* No integration *)
					ef = efpar[par][qq][FeynCalcInternal[qfacs],opt],
					(* Integration *)
					ef = efpar[par][qq][FeynCalcInternal[qfacs*Times@@Cases[{rr},E^(ee_?(!FreeQ[#,qq]&))]],opt]
				]*

					(* wrap is just so we can later absorb into the DOT[Integratedx...] stuff *)
					wrap[If[ (Integrate/.{opt}/.Options[FeynmanParametrize1])=!=True ||
							(CompleteSquare/.{opt}/.Options[FeynmanParametrize1])=!=True ||
								efpar=!=epar,

							(* No integration *)
							ExpandProductExpand[Times[rr]/.If[ efpar===fpar,
																{},
																squarerule
															]],

							(* Integration *)
							(*The integration momentum y*)
							y = endrule[[1]];
							(* Get the coefficient on y^2 in the exponential *)
							co = -Coefficient[(ef/._*DOT[dd_,dd1__]:>DOT[dd,dd1])[[-1]]/.E^(ee_)->ee,Pair[y,y]];
							(* Do the y-integrals *)
							FCPrint[1,"Expanding and replacing tensor terms with integrated terms, using integration momentum ",
									endrule[[2]], " and coefficient ", co];
							(rebug = Replace[
							(undebug = Uncontract[Expand[ExpandProductExpand[
									Times@@Replace[{rr}, E^(ee_?(!FreeQ[#,qq]&)) -> 1 , 1] /.
									squarerule]], y[[1]], Pair -> All]),

							dum | dum * ( tt : ( _?(FreeQ[#, y]&))) |
							dum * HoldPattern[
								Times[t : (Pair[
								LorentzIndex[_, ___],
								_Momentum] ..),
								re__?(FreeQ[#, y]&)]] |
							dum *  (tt : Pair[
								LorentzIndex[_, ___],
								_Momentum]) |
							dum * HoldPattern[
								Times[t : (Pair[
								LorentzIndex[_, ___],
								_Momentum] ..)]] :>
							dum*Contract[
								lis1 = Cases[{t,tt},Pair[
											LorentzIndex[_,___], y]];
								sil = Complement[{t,tt},lis1];
								lis = (#[[1]])&/@lis1;
								cc = Length[lis];
								(*Itzykson & Zuber (8-117)*)
								Which[
								OddQ[cc],0,
								cc==0,(2 Pi)^4/((4 Pi co)^(dim/2)),
								cc==2,(2 Pi)^4/(2 co (4 Pi co)^(dim/2)) Pair@@lis,
								cc==4,(2 Pi)^4/(4 co^2 (4 Pi co)^(dim/2))*
								Plus@@(Times[Pair@@#[[1]],Pair@@#[[2]]]&/@
										Union[Sort/@((Sort/@Partition[#,2])&/@Permutations[lis])]),
								True, Print["Cannot do higher ranks than 4"];
									Return[]]*
										(Times@@sil)*Times[re]
								] , {0, 1}])
						] *
							(*Remove the integrated out momentum*)
							If[ (Integrate/.{opt}/.Options[FeynmanParametrize1])===True &&
								(CompleteSquare/.{opt}/.Options[FeynmanParametrize1])===True &&
								efpar===epar,
								E^(co Pair[y,y]),
								1
							]
						] * noqfacs

		) /. dum -> 1;
		If[ !FreeQ[rebug, y] || FreeQ[rebug, dim],
			Message[FeynmanParametrize1::"noint",
			y, undebug, rebug]
		];
		FeynAmpDenominatorCombine[res /. DOT[rr___]*wrap[aa_] :> ReplacePart[DOT[rr],DOT[rr][[-1]]*aa,-1] /.
		endrule] /. wrap[rr_] :> rr /. efpar[{__}][_][1] :> 1
	];
FCPrint[1,"FeynmanParametrize1.m loaded."];
End[]
