(* ::Package:: *)

SnellsAngle[\[Theta]1_, n1_, n2_] := ArcSin[(n1/n2) Sin[\[Theta]1]]


(* divergence or acceptance angle (radians) of SM fiber *)
\[Theta]sm[\[Lambda]_, mfd_] := \[Lambda]/(\[Pi] (mfd/2))


(*Rayleigh Range*)
zr = \[Pi]/\[Lambda] (mfd/2)^2;

(*Rayleigh Range given waist radius*)
zR\[Omega][\[Lambda]_, \[Omega]0_, 
  n_] := (\[Pi] \[Omega]0^2 n)/\[Lambda] 

(*Rayleigh Range given angle of divergency*)
zR\[Theta][\[Lambda]_, \[Theta]_, n_] := \[Lambda]/(
 n \[Pi] \[Theta]^2)


(* Beam radius at distance z from center*)
w[z_, \[Lambda]_, mfd_] := 
 mfd/2 Sqrt[1 + ((z 4 \[Lambda])/(\[Pi] mfd^2))^2]

wz[z_, w0_, zR_] := w0 Sqrt[1 + (z/zR)^2]

wz[z_, z0_, \[Lambda]_, zR_, n_] := 
 Abs[Sqrt[(zR \[Lambda])/(\[Pi] n)] Sqrt[1 + ((z - z0)/zR)^2]]

wz[\[Lambda]_, z_, \[Theta]_, n_] := 
 Abs[\[Lambda]/(n \[Pi] \[Theta]) Sqrt[
   1 + ((n \[Pi] z \[Theta]^2)/\[Lambda])^2] ]

(*Matricies*)
(* x: radius from optical axis, \[Theta]: angle with optical axis *)
initVector[x_, \[Theta]_] := ( {
   {x},
   {\[Theta]}
  } )  
initVectorTan[x_, \[Theta]_] := ( {
   {x},
   {Tan[\[Theta]]}
  } )

freespace[d_] := ( {
   {1, d},
   {0, 1}
  } )

(* R: radius of curvature, Concave R > 0, Convex R < 0 *)
curvedMirror[R_] := ( {
   {1, 0},
   {-(2/R), 1}
  } )   

(* f: focal length, Converging f > 0, Diverging f < 0 *)
thinLens[f_] := ( {
   {1, 0},
   {-(1/f), 1}
  } )   



(*Gaussian Beams*)
(*Complex Beam Parameter: q*)
qFreeSpace[qz_, d_] := qz + d

qLens[qz_, f_] := (1/qz - 1/f)^-1

qMirror[qz_, f_] := -qLens[qz, f]\[Conjugate]

(*
Rcurv: radius of curvature (>0 for diverging, <0 for converging);
w: beam spot size (radius);
1/qz = 1/R-(\[ImaginaryI] \[Lambda]0)/(\[Pi] n w^2)
*)
qRw\[Lambda]Initialize[Rcurv_, \[Lambda]0_, w_, 
  n_] := (1/Rcurv - (I \[Lambda]0)/(\[Pi] n w^2) )^-1

(* Alternative definition of q *)
qzInitialize[z_, z0_, zR_] := (z - z0) + I zR


qzToNextLens[q_, z_, nextLensPos_, focalLength_] :=
 Module[{qnew, z0new},
  qnew = qLens[qFreeSpace[q, nextLensPos - z], focalLength];
  z0new = nextLensPos - Re[qnew]; (* z0new = znew - Re[qnew] *)
  {qnew, z0new}
  ]

Clear[qz0ToNextLens];
qz0ToNextLens[{q_,z0_},nextLensPos_,focalLength_]:=
 qz0ToNextLens[q,z0,nextLensPos,focalLength]

qz0ToNextLens[q_,z0_,nextLensPos_,focalLength_]:=
 Module[{qnew,z0new,d=nextLensPos-(z0+Re[q])},

  qnew=If[QuantityMagnitude[nextLensPos]>0,
    qLens[qFreeSpace[q,d],focalLength],
    qMirror[qFreeSpace[q,d],focalLength]];

  z0new=nextLensPos-Re[qnew];
  {qnew,z0new}
 ]

qz0values[q0_, z0_, focus_, mirrorOffset_, nBounces_] :=
 Module[{qzlist = {{q0, z0}}, qz0, dmirror},
  dmirror = 2 focus + mirrorOffset;
  
  (* Subtract total length so we always start on the left *)
  qz0 = qz0ToNextLens[q0, z0, dmirror, focus];
  qz0[[2]] -= 2 dmirror;
  AppendTo[qzlist, qz0];
  
  For[i = 1, i < nBounces, i++,
   qz0 = qz0ToNextLens[qz0, dmirror, focus];
   qz0[[2]] -= 2 dmirror;
   AppendTo[qzlist, qz0];
   ];
  
  qzlist
  ]


(*
Gives the elements of a list from indices "start" to "end"
e.g. list = {a,b,c,d,e,f,g}
a2b[list,-1,4] == {g,f,e,d}
*)
a2b[list_, start_?IntegerQ, end_?IntegerQ] := 
 Module[{strt = start, nd = end},
  If[strt < 0, strt += Length[list] + 1];
  If[nd < 0, nd += Length[list] + 1];
  list[[#]] & /@ Range[strt, nd, Sign[nd - strt]]
  ]

(* a2b but returns {} if the directions doesn't match *)
a2b[list_, start_?IntegerQ, end_?IntegerQ, dir_?(#^2 == 1 &)] := 
 Module[{strt = start, nd = end},
  If[strt < 0, strt += Length[list] + 1];
  If[nd < 0, nd += Length[list] + 1];
  list[[#]] & /@ Range[strt, nd, dir]
  ]

(*
Returns complex beam parameters (q) and beam waist locations (z0)
at each component in an optical cavity for n cycles (nCycles). 
Components' location and focal length (comps) with the first and last 
elements treated as mirrors while other components are treated as \
lenses
"Start" is the first components the light interacts with
direction (1 or -1) is the initial direction of the light
*)
qz0dCycleValues[q0_, z0_, direction_?(#^2 == 1 &),
  comps_?(Length[#] >= 2 && Length[#[[1]]] == 2 &), 
  nCycles_?NonNegative,
  opts : OptionsPattern[{"Start" -> 2, "End" -> 2}]] :=
 Module[{qz0dlist = {}, q = q0, z0i = z0, d = direction, fullCycle, 
   start, end, i, j},
  start = OptionValue["Start"];
  end = OptionValue["End"];
  
  fullCycle = {
    (* Starting component -> last (or first if d==-1) *)
    a2b[comps, start, -d, d],
    (* Penultimate -> 1st (or 2nd -> last if d==-1) *)
    a2b[comps, -2 d, d, -d],
    (* 2nd -> start-1 (or penultimate -> start+1 if d==-1) *)
    a2b[comps, 2 d, start - d, d]
    };
  
  For[i = 1, i <= nCycles, i++,
   For[j = 1, j <= Length[fullCycle], j++,
     Do[
      AppendTo[qz0dlist, {q, z0i, d}];
      {q, z0i} = qz0ToNextLens[q, z0i, comp[[1]], d comp[[2]]];
      ,
      {comp, fullCycle[[j]]}
      ];
     
     If[j != 3,
      d *= -1;
      (* Reflect across most recent component (mirror) *)
      {q, z0i} = {-Conjugate[q], reflect[z0i, comps[[d]][[1]]]};
      ];
     
     ];
   ];
  
  (* Add section here to incorperate ending at a different place *)
  
  AppendTo[qz0dlist, {q, z0i, d}];
  qz0dlist
  ]
