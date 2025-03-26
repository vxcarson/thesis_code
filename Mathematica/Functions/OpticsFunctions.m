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

qMirror[qz_, f_] := -qLens[qz, f]

(* Definition of q *)
qzInitialize[z_, z0_, zR_, direction_:1] := (z - z0) + I direction zR


(*
Alternative definition of q
Rcurv: radius of curvature (>0 for diverging, <0 for converging);
w: beam spot size (radius);
1/qz = 1/R-(\[ImaginaryI] \[Lambda]0)/(\[Pi] n w^2)
*)
qRw\[Lambda]Initialize[Rcurv_, \[Lambda]0_, w_, 
  n_] := (1/Rcurv - (I \[Lambda]0)/(\[Pi] n w^2) )^-1







Clear[qz0ToNextComp];

(*qz0ToNextComp[{q_, z0_}, nextLensPos_, focalLength_, isLens_ : True] :=
 qz0ToNextComp[q, z0, nextLensPos, focalLength, isLens]*)

qz0ToNextComp[q_, z0_, nextLensPos_, focalLength_, isLens_ : True] :=
 
 Module[{qnew, z0new, d = nextLensPos - (z0 + Re[q])},
  
  qnew = If[isLens,
    qLens[qFreeSpace[q, d], Sign[Im[q]] focalLength],
    qMirror[qFreeSpace[q, d], Sign[Im[q]] focalLength]];
  
  z0new = nextLensPos - Re[qnew]; {qnew, z0new}]


(* Return the percentage of power through an aperture of radius r *)
Clear[powerThrough];
powerThrough[r_, q_, z0_, \[Lambda]_, n_ : 1] := 
 1 - E^(-2 r^2/wz[Re[q], z0, \[Lambda]0, Im[q], n]^2)
powerThrough[r_, wz_] := 1 - E^(-2 r^2/wz^2)

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


Clear[qz0dCyclePower]
(*
Returns a list of {
complex beam parameters (q), 
beam waist locations (z0),
percent of power left (power), 
and the fraction (frac) of the beam width left for plotting purposes
}
at each component in an optical cavity for n cycles (nCycles). 
Components' {location, focal length, aperture} (comps) with the first and last 
elements treated as mirrors while other components are treated as \
lenses
"Start" is the first components the light interacts with
direction (1 or -1) is the initial direction of the light
"power" is the initial percentage of power
"wavelength" is the wavelength of the light
*)
qz0dCyclePower[q0_, z0_,
  comps_?(Length[#] >= 2 && Length[#[[1]]] == 3 &), 
  nCycles_?NonNegative,
  opts : OptionsPattern[{"InitialPower" -> 1, "Start" -> 2, 
     "wavelength" -> nmQ[780]}]] :=
 Module[{qz0dlist = {}, power, frac = 1, q = q0, z0i = z0, w, 
   d = Sign[Im[q0]], fullCycle, start, \[Lambda], i, j},
  start = OptionValue["Start"];
  power = OptionValue["InitialPower"];
  \[Lambda] = OptionValue["wavelength"];
  
  fullCycle = {
    (* Starting component -> last (or first if d==-1) *)
    a2b[comps, start, -d, d],
    (* Penultimate -> 1st (or 2nd -> last if d==-1) *)
    a2b[comps, -2 d, d, -d],
    (* 2nd -> start-1 (or penultimate -> start+1 if d==-1) *)
    a2b[comps, 2 d, start - d, d]
    };
  
  w = wz[comps[[1]][[1]], z0i, \[Lambda], Im[q], 1];
  power = 
   Quiet[Min[power, 
     powerThrough[comps[[1]][[3]], w]], {General::munfl, 
     Infinity::indet}];
  frac = Min[frac, comps[[1]][[3]]/w];
  
  For[i = 1, i <= nCycles, i++,
   If[Chop[power] == 0, Break[]];
   
   For[j = 1, j <= Length[fullCycle], j++,
    Do[
     AppendTo[qz0dlist, {q, z0i, power, frac}];
     {q, z0i} = qz0ToNextComp[q, z0i, comp[[1]], comp[[2]]];
     
     (* Check if beam is too large *)
     w = wz[comp[[1]], z0i, \[Lambda], Im[q], 1];
     power = 
      Quiet[Min[power, powerThrough[comp[[3]], w]], {General::munfl, 
        Infinity::indet}];
     frac = Min[frac, comp[[3]]/w];
     ,
     {comp, fullCycle[[j]]}
     ];
    
    If[j != 3,
     d *= -1;
     (* Reflect across most recent component (mirror) *)
     {q, z0i} = {-q, reflect[z0i, comps[[d]][[1]]]};
     ];
    
    ];
   
   ];
  
  AppendTo[qz0dlist, {q, z0i, power, frac}];
  qz0dlist
  ]


ClearAll[getCycleData];

getCycleData[focusColimnatingLens_,
opts:OptionsPattern[{"cycles" -> 20,"MFD" ->\[Mu]mQ[5.8],"PosColimnatingLens"->-1270Quantity[1,"Millimeters"],"initialOffset"->0Quantity[1,"Millimeters"],"wavelength"->nmQ[780],"iof" -> 1,"focalLength" -> 1Quantity[1,"Meters"] ,"aperturePC" -> 2Quantity[1,"Millimeters"]/2,"maxL" ->5000Quantity[1,"Millimeters"] ,"\[CapitalDelta]max" ->200 Quantity[1,"Millimeters"],"\[CapitalDelta]min" ->25 Quantity[1,"Millimeters"],"InitialPower"->1,"StartComp"->2}]]:=
Module[{qz0dPowerDict=<||>,q00,d00,f0=mmMag[focusColimnatingLens],q,z0,qmm,z0mm,cycles,MFDmm,zi,offset,\[Lambda]mm,iof,f,aPC,maxL,\[CapitalDelta]max,\[CapitalDelta]min,start,power,sol1,sol2,components,mirror1,mirror2},

cycles=OptionValue["cycles"];
MFDmm=OptionValue["MFD"]//mmMag;
zi=OptionValue["PosColimnatingLens"]//mmMag;
offset=OptionValue["initialOffset"]//mmMag;
\[Lambda]mm=OptionValue["wavelength"]//mmMag;
iof=OptionValue["iof"];
f=OptionValue["focalLength"]//mmMag;
aPC=OptionValue["aperturePC"]//mmMag;
maxL=OptionValue["maxL"]//mmMag;
\[CapitalDelta]max=OptionValue["\[CapitalDelta]max"]//mmMag;
\[CapitalDelta]min=OptionValue["\[CapitalDelta]min"]//mmMag;
power=OptionValue["InitialPower"];
start=OptionValue["StartComp"];

q00=0+I zr/.{mfd->MFDmm,\[Lambda]->\[Lambda]mm};
d00=Last[Values@FindRoot[Re[qLens[qFreeSpace[q00,x],f0]]==zi+offset,{x,1.1f0}]];

{q,z0}=qz0ToNextComp[q00,zi-d00,zi,f0];
qmm=q//mmMag;
z0mm=z0//mmMag;

sol1=Re[genSolution1/.{qq->q-zi,fL->f}];
sol2[md_]:=Re[genSolution2/.{qq->q-zi,fL->f,md1->md}];

Monitor[
Do[
AppendTo[qz0dPowerDict,md1-><||>];
Do[
mirror1 = md1; 
mirror2 = md2;

components={{0,\[Infinity],aPC},
{md1,f,12.7},
{(md2+md1),\[Infinity],12.7/2}};

If[Chop[md2]==0,
components=Most[components];
];

AppendTo[qz0dPowerDict[md1],md2->qz0dCyclePower[qmm,z0mm,components,cycles,"wavelength"->\[Lambda]mm,"InitialPower"->power,"Start"->start]];

,{md2,NormDistList[0,maxL,sol2[md1],\[CapitalDelta]max,\[CapitalDelta]min]}]
,{md1,NormDistList[0,maxL,1000,\[CapitalDelta]max,\[CapitalDelta]min]}];

Do[
If[MissingQ[qz0dPowerDict[md1]],
AppendTo[qz0dPowerDict,md1-><||>];
Do[
If[md2<0,Continue[]];
mirror1 = md1; 
mirror2 = md2;

components={{0,\[Infinity],aPC},
{md1,f,12.7},
{(md2+md1),\[Infinity],12.7/2}};

If[Chop[md2]==0,
components=Most[components];
];

AppendTo[qz0dPowerDict[md1],md2->qz0dCyclePower[qmm,z0mm,components,cycles,"wavelength"->\[Lambda]mm,"InitialPower"->power,"Start"->start]];

,{md2,Max[sol2[md1],0]+{0,-\[CapitalDelta]min,+\[CapitalDelta]min}}]
];
,{md1,NormDistList[0,maxL,1000,\[CapitalDelta]max/2,\[CapitalDelta]min/2]}];

Do[
If[MissingQ[qz0dPowerDict[md1]],
AppendTo[qz0dPowerDict,md1-><||>];
Do[
mirror1 = md1; 
mirror2 = md2;

components={{0,\[Infinity],aPC},
{md1,f,12.7},
{(md2+md1),\[Infinity],12.7/2}};

If[Chop[md2]==0,
components=Most[components];
];

AppendTo[qz0dPowerDict[md1],md2->qz0dCyclePower[qmm,z0mm,components,cycles,"wavelength"->\[Lambda]mm,"InitialPower"->power,"Start"->start]];

,{md2,{0}}]
];
,{md1,sol1}];
,Row[{"Current: \!\(\*SubscriptBox[\(d\), \(1\)]\) = ",mirror1," mm, \!\(\*SubscriptBox[\(d\), \(2\)]\) = ",mirror2," mm"}]];

qz0dPowerDict
]


ClearAll[getBeamPlots];

getBeamPlots[focusColimnatingLens_,d1_,d2_,method_:"Exact",
opts:OptionsPattern[{"fillType"->"power","database"->Null,"cycles" -> 20,"clipBeam"->True,"MFD" ->\[Mu]mQ[5.8],"PosColimnatingLens"->-1270Quantity[1,"Millimeters"],"initialOffset"->0Quantity[1,"Millimeters"],"wavelength"->nmQ[780],"iof" -> 1,"focalLength" -> 1Quantity[1,"Meters"] ,"aperturePC" -> 2Quantity[1,"Millimeters"]/2,"InitialPower"->1,"StartComp"->2}]]:=
Module[{qz0Values,fillType,showPower,constantPower,showLog,database,dm1,dm2,q00,d00,f0=mmMag[focusColimnatingLens],q,z0,qmm,z0mm,cycles,clipBeam,MFDmm,zi,offset,\[Lambda]mm,iof,f,aPC,start,power,sol1,sol2,components,plts,aspectR,dmax,curveFix,curveFixLens,lensThickness,vals1,vals2,i,maxIndex,pltRange,isExiting,frac1,frac2,fracOut,wz1,wz2,rnge1,rnge2,rngeOut,power1,power2,powerOut,thickness=40,gaussianOpacity1,gaussianOpacity2,logPerceivedOpacity1,logPerceivedOpacity2},

fillType=OptionValue["fillType"];
database=OptionValue["database"];
cycles=OptionValue["cycles"];
clipBeam=OptionValue["clipBeam"];
MFDmm=OptionValue["MFD"]//mmMag;
zi=OptionValue["PosColimnatingLens"]//mmMag;
offset=OptionValue["initialOffset"]//mmMag;
\[Lambda]mm=OptionValue["wavelength"]//mmMag;
iof=OptionValue["iof"];
f=OptionValue["focalLength"]//mmMag;
aPC=OptionValue["aperturePC"]//mmMag;
power=OptionValue["InitialPower"];
start=OptionValue["StartComp"];

Switch[fillType,
"power",
showPower=True;
constantPower=False,

"constant",
showPower=True;
constantPower=True,

"gaussian",
showPower=False;
showLog=False,

"log",
showPower=False;
showLog=True,

_,(*default*)
Print["Invalid \"fillType\".\n
Options include: \"power\", \"constant\", \"gaussian\", and \"log\".\n
\"fillType\"->\"power\" used by default"];
showPower=True
];


q00=0+I zr/.{mfd->MFDmm,\[Lambda]->\[Lambda]mm};
d00=Last[Values@FindRoot[Re[qLens[qFreeSpace[q00,x],f0]]==zi+offset,{x,1.1f0}]];

{q,z0}=qz0ToNextComp[q00,zi-d00,zi,f0];
qmm=q//mmMag;
z0mm=z0//mmMag;

sol1=Re[genSolution1/.{qq->q-zi,fL->f}];
sol2[md_]:=Re[genSolution2/.{qq->q-zi,fL->f,md1->md}];

Switch[method,
"Nearest",
{dm1,dm2}=Nearest[getKeyPaths[database],{d1,d2}]//First;
qz0Values=database[dm1,dm2],

"Solution1",
{dm1,dm2}={Last[sol1,sol1],sol2[Last[sol1,sol1]]},

"Solution2",
{dm1,dm2}={d1,sol2[d1]},

"Exact",
{dm1,dm2}={d1,d2},

_,(*default*)
Print["Invalid method.\n
Options include: \"Nearest\", \"Solution1\", \"Solution2\", and \"Exact\".\n
method = \"Exact\" used by default"];
{dm1,dm2}={d1,d2}
];

If[!ValueQ[qz0Values],
components={{0,\[Infinity],aPC},
{dm1,f,12.7},
{(dm2+dm1),\[Infinity],12.7/2}};
If[Chop[dm2]==0,
components=Most[components];
];

qz0Values=qz0dCyclePower[qmm,z0mm,components,cycles,"wavelength"->\[Lambda]mm];
];


plts={};
aspectR=0.3;
dmax=Max[dm1+dm2+40,2f];
pltRange={{-1500,1.1dmax},{-12.7,12.7}};
curveFix=aspectR (pltRange[[1,2]] -pltRange[[1,1]])/(pltRange[[2,2]] -pltRange[[2,1]]);

curveFixLens=aspectR (1.1 2f+1500)/(2 12.7);

lensThickness=setSigFigs[
Quiet@SolveValues[\[CapitalDelta]x==2f(1-Cos[\[Theta]])&&
Sin[\[Theta]]==12.7*curveFixLens/(2f)&&
0<\[Theta]<=\[Pi]/4,{\[CapitalDelta]x,\[Theta]}][[1,1]],
3,True];

If[!ValueQ[lensGraphic],
lensGraphic=RegionPlot[-12.7<=y<=12.7&&
2f>=Norm[{x+2f-lensThickness,y*curveFixLens}]&&
2f>=Norm[{x-2f+lensThickness,y*curveFixLens}],
{x,0-lensThickness,0+lensThickness},
{y,pltRange[[2,1]],pltRange[[2,2]]},
BoundaryStyle->Thin,
PlotRange->{1.1{-lensThickness,lensThickness},1.01{pltRange[[2,1]],pltRange[[2,2]]}},
AspectRatio->Full,MaxRecursion->2,Mesh->None,Frame->False];
];

If[!ValueQ[pcGraphics],
pcGraphics=Plot[{mmMag[aPC],-mmMag[aPC]},{z,-80/2,80/2},PlotStyle->Directive[Purple](*,
PlotLabels\[Rule]Placed[{"PC aperture",""},{{Scaled[1.5],Before}}]*)];
];

If[!ValueQ[flatMirrorGraphic],
flatMirrorGraphic=RegionPlot[-12.7/2<=y<=12.7/2&&0<=x<=thickness,
{x,0,thickness},
{y,pltRange[[2,1]],pltRange[[2,2]]},
PlotRange->{{-0.1thickness,1.1thickness},1.01{pltRange[[2,1]],pltRange[[2,2]]}},
AspectRatio->Full,MaxRecursion->2,Mesh->None,Frame->False];
];

maxIndex=Length[qz0Values];
(*maxIndex=4;*)

rnge2={dm1,dm1+dm2};
rngeOut={mmMag[zi],0};

Monitor[
For[i=1,i<maxIndex,i+=2,
rnge1={0,dm1};

If[Sign[Im[qz0Values[[i]][[1]]]]==1,
vals1=qz0Values[[i]];
vals2=qz0Values[[i+1]];
isExiting=False;
,
vals1=qz0Values[[i+1]];
vals2=qz0Values[[i]];
isExiting=True;
];

wz1[z_]:=wz[z,vals1[[2]],\[Lambda]mm,Im[vals1[[1]]],iof];
power1=vals1[[3]];
frac1=vals1[[4]];
wz2[z_]:=wz[z,vals2[[2]],\[Lambda]mm,Im[vals2[[1]]],iof];
power2=vals2[[3]];
frac2=vals2[[4]];

If[i==1,
rnge1={mmMag[zi],dm1};
];


If[isExiting,
powerOut = 
Min[power1,powerThrough[aPC,wz[0,vals1[[2]],\[Lambda]mm,Im[vals1[[1]]],iof]]];
fracOut=
Min[frac1,aPC/wz[0,vals1[[2]],\[Lambda]mm,Im[vals1[[1]]],iof]];
];

If[!clipBeam,
frac1=1;
frac2=1;
fracOut=1;
];

If[constantPower,
power1=0.05;
power2=0.05;
powerOut=0.05;
];

If[!showPower,
gaussianOpacity1[z_,y_]=0.7227897522452308`/wz1[z] (E^(-2 (y/wz1[z])^2))//N;
gaussianOpacity2[z_,y_]=0.7227897522452308`/wz2[z] (E^(-2 (y/wz2[z])^2))//N;
If[showLog,
logPerceivedOpacity1[z_,y_,k_:5]=
Log[1+k gaussianOpacity1[z,y]]/Log[1+k]//N;
logPerceivedOpacity2[z_,y_,k_:5]=
Log[1+k gaussianOpacity2[z,y]]/Log[1+k]//N
,
logPerceivedOpacity1[z_,y_]=gaussianOpacity1[z,y];
logPerceivedOpacity2[z_,y_]= gaussianOpacity2[z,y]
];

AppendTo[plts,
Show[
RegionPlot[Abs[y]<=frac1 wz1[z]&&rnge1[[1]]<=z<=rnge1[[2]],
{z,rnge1[[1]],rnge1[[2]]},
{y,pltRange[[2,1]],pltRange[[2,2]]},
PlotRange->pltRange,
BoundaryStyle->None,
ColorFunction->Function[{z,y},
Directive[Red,Opacity[logPerceivedOpacity1[z,y]]]
],ColorFunctionScaling->False,
ImageSize->Full,AspectRatio->aspectR,
PlotPoints->25,MaxRecursion->4,Mesh->None,
Epilog->{
Inset[lensGraphic,{dm1,0},{0,0},{2lensThickness,pltRange[[2,2]]-pltRange[[2,1]]}],
Inset[lensGraphic,{mmMag[zi],0},{0,0},{50,4}],Inset[flatMirrorGraphic,{dm1+dm2,0},{0,0},{thickness,pltRange[[2,2]]-pltRange[[2,1]]}]
}
],
RegionPlot[Abs[y]<=frac2 wz2[z]&&rnge2[[1]]<=z<=rnge2[[2]],
{z,rnge2[[1]],rnge2[[2]]},
{y,pltRange[[2,1]],pltRange[[2,2]]},
PlotRange->pltRange,
BoundaryStyle->None,
ColorFunction->Function[{z,y},
Directive[Red,Opacity[logPerceivedOpacity2[z,y]]]
],ColorFunctionScaling->False,
ImageSize->Full,AspectRatio->aspectR,
PlotPoints->25,MaxRecursion->4,Mesh->None]
,
If[isExiting,
RegionPlot[Abs[y]<=fracOut wz1[z]&&rngeOut[[1]]<=z<=rngeOut[[2]],
{z,rngeOut[[1]],rngeOut[[2]]},
{y,pltRange[[2,1]],pltRange[[2,2]]},
PlotRange->pltRange,
BoundaryStyle->None,
ColorFunction->Function[{z,y},
Directive[Red,Opacity[logPerceivedOpacity1[z,y]]]
],ColorFunctionScaling->False,
ImageSize->Full,AspectRatio->aspectR,
PlotPoints->25,MaxRecursion->4,Mesh->None],
{}]
,
pcGraphics]
];
,
AppendTo[plts,
Show[
Plot[{frac1 wz1[z],-frac1 wz1[z]},{z,rnge1[[1]],rnge1[[2]]},
PlotStyle->Directive[Red,Thickness[0.002],Opacity[power1]],
PlotRange->pltRange,Filling->{1->{2}},FillingStyle->Directive[Red,Opacity[power1]],
ImageSize->Full,AspectRatio->.3,
Epilog->{
Inset[lensGraphic,{dm1,0},{0,0},{2lensThickness,pltRange[[2,2]]-pltRange[[2,1]]}],
Inset[lensGraphic,{mmMag[zi],0},{0,0},{50,4}],Inset[flatMirrorGraphic,{dm1+dm2,0},{0,0},{thickness,pltRange[[2,2]]-pltRange[[2,1]]}]
}
],
Plot[{frac2 wz2[z],-frac2 wz2[z]},{z,rnge2[[1]],rnge2[[2]]},
PlotStyle->Directive[Red,Thickness[0.002],Opacity[power2]],
PlotRange->pltRange,Filling->{1->{2}},FillingStyle->Directive[Red,Opacity[power2]],
ImageSize->Full,AspectRatio->.3]
,
If[isExiting,
Plot[{fracOut wz1[z],-fracOut wz1[z]},{z,rngeOut[[1]],rngeOut[[2]]},
PlotStyle->Directive[Red,Thickness[0.002],Opacity[powerOut]],
PlotRange->pltRange,Filling->{1->{2}},FillingStyle->Directive[Red,Opacity[powerOut]],
ImageSize->Full,AspectRatio->.3],
{}]
,
pcGraphics]
];
]
]
,Row[{"\!\(\*SubscriptBox[\(d\), \(1\)]\) = ",dm1," mm, \!\(\*SubscriptBox[\(d\), \(2\)]\) = ",dm2," mm   ",Round[i/maxIndex 100,0.1],"%"}]];
plts
]


getCouplingData//ClearAll;

getCouplingData[focusColimnatingLens_,database_,opts:OptionsPattern[{"cycles" -> 20,"MFD" ->\[Mu]mQ[5.8],"PosColimnatingLens"->-1270Quantity[1,"Millimeters"],"initialOffset"->0Quantity[1,"Millimeters"],"wavelength"->nmQ[780],"iof" -> 1,"focalLength" -> 1Quantity[1,"Meters"] ,"aperturePC" -> 2Quantity[1,"Millimeters"]/2,"lensRadius"->2.75Quantity[1,"Millimeters"],"StartComp"->2}]]:=
Module[{minCouplingData={},exitingBeamsData={},exitingBeams,d00,f0=mmMag[focusColimnatingLens],q,z0,cycles,MFDmm,zi,\[Lambda]mm,iof,f,aPC,start,correctPower=1./powerThrough[1,1]},

cycles=OptionValue["cycles"];
MFDmm=OptionValue["MFD"]//mmMag;
zi=OptionValue["PosColimnatingLens"]//mmMag;
offset=OptionValue["initialOffset"]//mmMag;
\[Lambda]mm=OptionValue["wavelength"]//mmMag;
iof=OptionValue["iof"];
f=OptionValue["focalLength"]//mmMag;
aPC=OptionValue["aperturePC"]//mmMag;
lensRadius=OptionValue["lensRadius"]//mmMag;
start=OptionValue["StartComp"];

q00=0+I zr/.{mfd->MFDmm,\[Lambda]->\[Lambda]mm};
d00=Last[Values@FindRoot[Re[qLens[qFreeSpace[q00,x],f0]]==zi+offset,{x,1.1f0}]];

smfRmm=MFDmm/2;

Monitor[
Do[
dm1=md1;
Do[
dm2=md2;
exitingBeams=Select[database[md1,md2],Chop[Re[#[[1]]]+#[[2]]-md1]==0&&Im[#[[1]]]<0&];

If[Length[exitingBeams]==0,Continue[]];

exitingBeams=qz0ToNextComp[#[[1]],#[[2]],zi,f0]~Join~{Quiet[Min[#[[3]],
powerThrough[aPC,wz[0,#[[2]],\[Lambda]mm,Im[#[[1]]],iof]],powerThrough[lensRadius,wz[zi,#[[2]],\[Lambda]mm,Im[#[[1]]],iof]]],
General::munfl]}&/@exitingBeams;

exitingBeams={md1/1000,md2/1000,#[[3]],
Quiet[Min[1,
correctPower*powerThrough[smfRmm,wz[zi-d00,#[[2]],\[Lambda]mm,Im[#[[1]]],iof]]],
General::munfl]}&/@exitingBeams;

minPowerCoupled=Min[Min[#[[3]],#[[4]]]&/@exitingBeams];

AppendTo[minCouplingData,{md1/1000,md2/1000,minPowerCoupled}];

If[Length[exitingBeams]<cycles,
For[j=Length[exitingBeams]+1,j<=cycles,j++,
AppendTo[exitingBeams,{md1/1000,md2/1000,0,0}]
]
];

AppendTo[exitingBeamsData,exitingBeams];

,{md2,database[md1]//Keys}]
,{md1,database//Keys}]
,Row[{"\!\(\*SubscriptBox[\(d\), \(1\)]\) = ",dm1," mm, \!\(\*SubscriptBox[\(d\), \(2\)]\) = ",dm2," mm"}]];

{minCouplingData,Transpose[exitingBeamsData,1<->2]}
]


ClearAll[plotCouplingData]
Options[plotCouplingData]=Join[{"showDataPoints"->True,"showRow0"->True},Options[ListDensityPlot]];

plotCouplingData[data_,opts:OptionsPattern[]]:=Module[{showDataPoints,showRow0,densityOpts,densityPlot,overlay,densityPlot0,overlay0,extraRow0={},getOptionOrDefault,colorFunc,blendList,plotLegends},

(*Extract custom options*)
showDataPoints=OptionValue["showDataPoints"];
showRow0=OptionValue["showRow0"];

(*Extract ListDensityPlot options*)
densityOpts=FilterRules[FilterRules[
{opts},Options[ListDensityPlot]],
Except[{ColorFunction,PlotLegends,ImageSize,AxesLabel,InterpolationOrder,PlotRange}]];


blendList=
{{0,Blend[{Darker[Blue,.4],Darker[Purple,.4]},.8]},
{.9,Blend[{Darker[Cyan,.2],Blue},.3]},
{.99,Darker[Cyan,.1]},
{1,Lighter[Cyan,.3]}};
colorFunc=
Blend[blendList,#]&;

plotLegends=Column[{
BarLegend[{colorFunc,{0,1}},LegendMargins->{{0,0},{10,5}},LegendLabel->"Coupling Eff.",LabelStyle->{FontFamily->"Helvetica"}](*,
LineLegend[{Lighter[Blue,.9]},{"Solution"}]*)}];

getOptionOrDefault[sym_,fallback_]:=If[KeyExistsQ[Association[opts],sym],OptionValue[sym],fallback];

densityOpts=densityOpts~Join~
{ColorFunction->getOptionOrDefault[ColorFunction,colorFunc],
PlotLegends->getOptionOrDefault[PlotLegends,plotLegends],
ImageSize->getOptionOrDefault[ImageSize,350],
AxesLabel->getOptionOrDefault[AxesLabel,{"\!\(\*SubscriptBox[\(d\), \(1\)]\) (m)","\!\(\*SubscriptBox[\(d\), \(2\)]\) (m)"}],
InterpolationOrder->getOptionOrDefault[InterpolationOrder,2],
PlotRange->getOptionOrDefault[PlotRange,{0,1}]
};

Do[
If[Chop[1000p[[2]]]!=0,Continue[],
AppendTo[extraRow0,{p[[1]],0,p[[3]]}];
AppendTo[extraRow0,{p[[1]],-35/1000,p[[3]]}];
AppendTo[extraRow0,{p[[1]],-60/1000,p[[3]]}];
];
,{p,data}
];

densityPlot=
ListDensityPlot[{data,If[showRow0,extraRow0,Nothing]},Evaluate[densityOpts]];

overlay=If[showDataPoints,
ListPlot[
Tooltip[{#[[1]],#[[2]]},
Row[{
Quiet@setSigFigs[#[[3]],3],
" (",1000#[[1]]," mm, ",Max[1000#[[2]],0]," mm)"
}],
TooltipStyle->{Background->White,CellFrameColor->Automatic}
]&/@data~Join~If[showRow0,Select[extraRow0,#[[2]]==-35/1000&],{}]],
Nothing
];

Show[{densityPlot,overlay,If[showRow0,Graphics[{White,Rectangle[{-0.5,-0.01},{10,0.02}]}],Nothing]}]
]
