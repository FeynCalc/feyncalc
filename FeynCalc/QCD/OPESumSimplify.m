(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: OPESumSimplify*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 5 January '99 at 21:33 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  simplify OPEsums occuring in the operator product expansions*)

(* ------------------------------------------------------------------------ *)

OPESumSimplify::usage =
"OPESumSimplify[exp] simplifies OPESums in exp.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`OPESumSimplify`Private`"]

powexp[x_] :=
	x /. (-y_)^po_ -> ((-1)^po y^po) /.
			{(-1)^(e1_. + 2 _ ) :> ((-1)^e1),
				(-1)^(e1_. - 2 _ ) :> ((-1)^e1)
			};

Options[OPESumSimplify]= {
	Assumptions -> True
};

OPESumSimplify[exp_, OptionsPattern[]] :=
	Block[ {nx, proex, sumex, suma, csum, psum, lsum,
	lsum1,lsum4,lsum4s,lsum6, power7, nonu},
		psum[a_, b__] :=
			csum[Factor2[powexp[a] /. Power2 -> Power], b];
		lsum4s[aa_,cc__] :=
			-OPESum[-aa,cc] /; NumericalFactor[aa] === (-1);
		lsum4[b_, moreind__List, {i_, c_, d_}] :=
			lsum4s[ powsu7[b , moreind, {i,c,d}],
								moreind, {i,c,d}] /. lsum4s -> OPESum;
		powsu7[a_] :=
			a;
		powsu7[b_, mor___List, {i_, _, _}] :=
			powsu7[b /. Power2[(-1),any_] :> Expand[(-1)^any] /.
								{ (-1)^i pow_[a_,i] :>
									pow[-a,i] /; pow === Power || pow===Power2,
									(-1)^i pow_[a_,(i + n_Integer?OddQ)] :>
									-power7[(-a),(i + n)] /; pow === Power || pow===Power2,
									(-1)^i pow_[a_,(i + n_Integer?EvenQ)] :>
									power7[(-a),(i + n)] /; pow === Power || pow===Power2
								},
						mor];
		lsum5[a_^v_ b_^w_,{i_,0,m_}] :=
			(a^v b^w/(a^i b^(m-i)) OPESum[a^i b^(m-i),{i,0,m}]
			) /; !FreeQ[a, OPEDelta] && !FreeQ[b, OPEDelta] &&
					(Variables[v] === {i}) && (Variables[w] === Variables[{i,m}]);

		(* special trick for sums *)
		lsum6[a_, in1___List, {j_, 0, m4_Plus}] :=
			Block[ {te},
				te = SelectNotFree[a, b_^(m4-j+1)];
				If[ Head[te] === Power,
					te[[1]] OPESum[a /. te :> (te[[1]]^(te[[2]]-1)),
														in1, {j,0,m4}],
					OPESum[a, in1, {j,0,m4}]
				]
			];

		(* special trick for sums *)
		lsum7[a_,more___List, {i_, 0, j_}] :=
			Block[ {te},
				te = SelectNotFree[a, b_^(i+1)];
				If[ Head[te] === Power,
					te[[1]] OPESum[a /. te :> (te[[1]]^(te[[2]]-1)), more,{i,0,j}],
					OPESum[a, more, {i, 0, j}]
				]
			];
		lsum2[a_, b__] :=
			lsum3[
			powexp[a] /. {(z_^(pe_/;Head[pe]=!=Integer)) :>
												Power2[z, pe]
							},   b];
		csum /: x_^n_. csum[ x_^j_ fa_., b__] := csum[ fa x^(j+n), b] /; n =!= OPEm;
		csum /: x_^(n_ /;Head[n]=!=Integer)*
			csum[a_, b__] := csum[PowerSimplify[a x^n, Assumptions->OptionValue[Assumptions]], b] /; n=!=OPEm;
		csum /: x_^n_. csum[ Power2[x_,j_] fa_., b__] :=
											csum[ fa Power2[x,(j+n)], b ];
		lsum8[a_,b__] :=
			If[ IntegerQ[NumericalFactor[a]],
				NumericalFactor[a] OPESum[a/NumericalFactor[a],b],
				OPESum[a,b]
			];

		(* get all summation indices *)
		nonu[a_List] :=
			Select[Variables[Drop[a,-1]], Head[#] =!= Integer&];
		nonu[a_List, b__List] :=
			Select[
			Variables[Map[Drop[#,-1]&,{a,b}]],
								Head[#] =!= Integer&
							];
		power5[-1, aa_ + bb_] :=
			power5[-1, aa] power5[-1, bb];
		power5[-1, nn_Integer] :=
			(-1)^nn;
		minone[y_] :=
			y /. {(-1)^(a_Plus) :> power5[-1,a],
									Power2[(-1),a_Plus] :> power5[-1,a]
								};
		lsum[x_Times, j__List] :=
			(Select[minone[x],
												FreeQ2[#, nonu[j]]&] OPESum[
									Select[minone[x],!FreeQ2[#, nonu[j]]&], j]
									) /. power5 -> Power;
		proex /: f_ proex[b__List] := proex[f, b];
		proex /: f_ proex[c_/;Head[c]=!=List, b__List] := proex[f c, b];
		sumex[a_,b__] :=
			suma[Expand[a /. {(z_^(pe_/;Head[pe]=!=Integer)) :>
												Power2[z, pe]}], b];
		suma[c_, d__] :=
			If[ Head[c] === Plus,
				Map[sumb[#, d]&, c],
				sumb[c, d]
			];
		nx = Expand[powexp[FeynCalcInternal[exp]/.
								OPESum -> proex /. proex -> sumex], sumb
							] /. sumb -> psum;
		nx = ((*PowerExpand[*)nx (*]*))/. OPESum -> sumex/.
						csum -> lsum /. lsum -> lsum2 /. lsum2 -> lsum3;
		nx = nx /.OPESum->lsum4 /. lsum4 -> lsum4s /. lsum4s -> OPESum;
		nx = nx /. lsum3 -> lsum4 /. lsum4 -> OPESum /.
								psum -> OPESum /. csum -> OPESum /. sumex -> OPESum /.
						power7 -> Power;
		nx = PowerSimplify[nx, Assumptions->OptionValue[Assumptions]] /. OPESum -> lsum5 /. lsum5->OPESum /.
					OPESum -> lsum6 /.  lsum6 -> OPESum /. OPESum -> lsum7 /.
					lsum7 -> OPESum /. OPESum -> lsum8;
		nx
	];
FCPrint[1,"OPESumSimplify.m loaded"];
End[]
