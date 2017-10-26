<<FeynCalc` ;
DeclareNonCommutative[s];
FI;
ssimp = (
	  ( Expand[DotSimplify[#, DotSimplifyRelations ->
			{s[i_].s[j_] :> (k=Unique["k"]; KroneckerDelta[i, j] + I Eps[i,j,k] s[k])}]
			  ]
	  ) //. (KroneckerDelta[x_, y_] f_ :> (f/.x->y) /; !FreeQ[f, x]) /.
	  ( x_ s[ij_] :> ( (x /. ij :> k0) s[k0] ) /; !FreeQ[x,ij])
		)&;

ssimp[ s[i1].s[i2].s[i3].s[i4].s[i5] ]

