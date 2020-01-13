timingstart = AbsoluteTime[];
<<FeynCalc`;

(* See also: http://forums.wolfram.com/mathgroup/archive/2003/Oct/msg00516.html *)
eqs={M0 == (1 - f1 - f2 - f3 - f4 - f5 - f6 - f7 - f8 - f9 -
				f10 - f11 - f12 - f13 - f14 - f15 - f16)*m0,
		M1 == f1*m0 + (1 - f1 - f2 - f3 - f4 - f5 - f6 - f7 - f8 - f9 - f10 -
				f11 - f12 - f13 - f14 - f15 - f16)*m1,
		M2 == f1*m1 + f2*m0 + (1 - f1 - f2 - f3 - f4 - f5 - f6 - f7 - f8 - f9 -
				f10 - f11 - f12 - f13 - f14 - f15 - f16)*m2,
		M4 == f1*m3 + f2*m2 + f3*m1 + f4*m0,
		M5 == f2*m3 + f3*m2 + f4*m1 + f5*m0,
		M6 == f3*m3 + f4*m2 + f5*m1 + f6*m0,
		M7 == f4*m3 + f5*m2 + f6*m1 + f7*m0,
		M8 == f5*m3 + f6*m2 + f7*m1 + f8*m0,
		M9 == f6*m3 + f7*m2 + f8*m1 + f9*m0,
		M10 == f7*m3 + f8*m2 + f9*m1 + f10*m0,
		M11 == f8*m3 + f9*m2 + f10*m1 + f11*m0,
		M12 == f9*m3 + f10*m2 + f11*m1 + f12*m0,
		M13 == f10*m3 + f11*m2 + f12*m1 + f13*m0,
		M14 == f11*m3 + f12*m2 + f13*m1 + f14*m0,
		M15 == f12*m3 + f13*m2 + f14*m1 + f15*m0,
		M16 == f13*m3 + f14*m2 + f15*m1 + f16*m0} /. a_ ==b_ :> (a-b);
tim = Timing[ sol=Solve3[ eqs,
{f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16}, ParallelMap -> True ]; ];
Print["time used for the calculation = ",tim];
timcheck = Timing[ Together/@(eqs /. sol)];
Print["time used for checking the result = ",timcheck];
Print["leaf count of sol = ",LeafCount[sol]];
simptime = Timing[solsim=sol /. (a_->b_) :>
									(WriteString["stdout","."];a -> Map[Simplify,b]);]
Print["time used for simplification = ",simptime];
Print["leaf count of solsim = ",LeafCount[solsim]];
Print["Memory used:",MaxMemoryUsed[]/1024.^2," MB"];
Format[Continuation[_]] := ""; Format[LineBreak[_]]:= "";
Print["the result is:"];
Print[InputForm[solsim]];
Print["total time used = ",Round[AbsoluteTime[] - timingstart]/60., "minutes"];
