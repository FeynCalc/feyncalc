(* ------------------------------------------------------------------------ *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

DiracOrder::usage =
"DiracOrder[expr] orders the Dirac matrices in expr alphabetically. \
DiracOrder[expr, orderlist] orders the Dirac matrices in expr according \
to orderlist.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DiracOrder`Private`"];

dotLin[z_] :=
	DotSimplify[z, Expanding -> False];

dicomm[a___,DiracGamma[vl_[y__],di___],
			DiracGamma[lv_[z__],dim___],b___] :=
	MemSet[dicomm[a, DiracGamma[vl[y], di], DiracGamma[lv[z], dim], b],
			((-DiracTrick[a,DiracGamma[lv[z],dim], DiracGamma[vl[y],di],b] +
			((2 PairContract[vl[y],lv[z]] DiracTrick[a,b])/.PairContract->Pair)
			)/.PairContract->Pair)]/;!OrderedQ[{lv[y],vl[z]}];

dessav[x__] :=
	MemSet[dessav[x], DiracTrick[x]];

diraccanonical[ x_,y__ ] :=
	diraccanonical[x.y];

diraccanonical[x_] :=
	MemSet[diraccanonical[x],
		Block[ {diraccanres},    (*diraccanonicaldef*)
			diraccanres = x //. DOT -> dicomm /. dicomm -> DOT /.
			DOT-> dessav /. DiracTrick -> DOT;
			diraccanres = Expand[dotLin[ diraccanres ], DiracGamma] /.
			Pair -> PairContract /. PairContract->Pair;
			diraccanres
		]
	];

DiracOrder[x__] :=
	diracord@@FCI[{x}];

diracord[x_] :=
	FixedPoint[diraccanonical, x, 42];
diracord[x_,y___,z_] :=
	FixedPoint[diraccanonical, DOT[x,y,z], 42]/;Head[z]=!=List;

diracord[x_,y__,ord_List] :=
	diracord[DOT[x,y],ord];

diracord[x_,ord_List] :=
	MemSet[diracord[x,ord],
		Block[ {diracordrev = Reverse[ord], diracordz,    diracordres = x,diracordi},
			Do[ diracordz = diracordrev[[diracordi]];
				diracordres = diracordres//.
				{_[a___,DiracGamma[vl_[y__],di___],
				DiracGamma[lv_[diracordz0_,dime___],dim___],b___] :>
				((-DiracTrick[a,DiracGamma[lv[diracordz0,dime],dim],
				DiracGamma[vl[y],di],b]+
				(2 PairContract[vl[y],lv[diracordz0,dime]] DiracTrick[a,b] )/.PairContract->Pair))/;
				!FreeQ[lv[diracordz0, dime], diracordz]} /. DOT -> DiracTrick /. DiracTrick -> DOT,
				{diracordi,1,Length[ord]}
			];
			(Expand[dotLin[diracordres], DiracGamma])/.Pair->PairContract/.PairContract->Pair
		]
	];
FCPrint[1,"DiracOrder.m loaded."];
End[]
