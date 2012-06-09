(* this is a .m file for calculating the 1-loop gluon selfenergy in 
   a general covariant gauge in D dimensions.
*)
(* Author: rolf@mertig.com *)

(* History: written October 2003 *)

(* Literature: see, e.g., the Appendix A of 
   "Foundations of Quantum Chromodynamics"  by T. Muta, World Scientific, 1998
*)

(* Version: FeynCalc 5.0 with a patched version of FeynArts 3.2 is needed to run
   this file, e.g., in batch mode, as:
   
   math < qcdgluonse1.m > qcdgluonse1.out &

   Alternatively you may use
   <<qcdgluonse1.m  
   in a notebook or terminal interface of Mathematica
 
   This calculation should finish in less than 10 seconds on a 1 GHz computer
running Mathematica 5.0
*)

$LoadFeynArts=True;
$LoadTARCER=True;
<<HighEnergyPhysics`FeynCalc` 

starttime = AbsoluteTime[];

(* generating the 4 amplitudes for the 1-loop gluon selfenergy with FeynArts*)
inserts = InsertFields[CreateTopologies[1, 1 -> 1, 
      ExcludeTopologies -> Tadpoles], {V[5]} -> {V[5]}, 
     InsertionLevel -> {Classes}, 
    GenericModel -> "FCQCDLorentz", Model -> "FCQCD"];
QuarkMass = 0;

(* we omit the usual factor 1/(2 Pi)^D and use p for the external momentum *)
(* the convention in the FCQCDLorentz.gen model file is that p1 is the incoming
and k1 the outgoing momentum, with p1+k1=0 
*)
amps = CreateFeynAmp[inserts, Truncated -> True, PreFactor -> 1] /. {p1 :> p, 
            k1 :> -p, q1 :> q, li1 :> mu, li2 :> nu} /. 
        FeynAmpList[__] :> List /. FeynAmp[_, _, x_] :> x;

(* we use a general covariant gauge, D for the dimension and sum all amplitudes *)
t1 = SUNSimplify[Explicit[Plus@@amps, Gauge -> 1-xi,Dimension->D]];
(* after the SU(N) algebra now the Lorentz and Dirac algebra: *)
t2 = Contract[t1/.DiracTrace -> TR];
(* TID does the tensor integral decomposition and ToTFI together with TarcerRecurse 
   reduces all integrals to a B0 type integral (TBI in Tarcer's notation)
*)
t3 = (Collect2[
      ToTFI[Collect2[TID[t2,q],q], q, p]//TarcerRecurse, {CA,Tf}])/.a_Plus:>Collect2[a,xi]/;FreeQ[a, CA];

WriteString["stdout", "The result is ",t3//InputForm];

(* that's it. Expansion in Epsilon is simple *)

(*
t3 should be:
(2*(2 - D)*Gstrong^2*Nf*Tf*SD[ci1, ci2]*(FVD[p, mu]*FVD[p, nu] -
    MTD[mu, nu]*SPD[p, p])*TBI[D, SPD[p, p], {{1, 0}, {1, 0}}])/(1 - D) -
 (CA*Gstrong^2*(4*(2 - 3*D) - 4*(7 - 2*D)*(1 - D)*xi + (1 - D)*(4 - D)*xi^2)*
   SD[ci1, ci2]*(FVD[p, mu]*FVD[p, nu] - MTD[mu, nu]*SPD[p, p])*
   TBI[D, SPD[p, p], {{1, 0}, {1, 0}}])/(8*(1 - D))
*)

AbsoluteTime[]-starttime

