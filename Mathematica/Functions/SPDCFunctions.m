(* ::Package:: *)

(* Sellmeier Equations (\[Lambda] in \[Mu]m) values from https://www.castech-us.com/casbbo.htm *)

Subscript[n, o][\[Lambda]_] := Sqrt[
 2.7359 + 0.01878/(\[Lambda]^2 - 0.01822) - 0.01354 \[Lambda]^2]

Subscript[n, e][\[Lambda]_] := Sqrt[
 2.3753 + 0.01224/(\[Lambda]^2 - 0.01667) - 0.01516 \[Lambda]^2]

Subscript[n, 
  eff][\[Lambda]_, \[Theta]_] := (Cos[\[Theta]]^2/
   Subscript[n, o][\[Lambda]]^2 + Sin[\[Theta]]^2/
   Subscript[n, e][\[Lambda]]^2)^(-1/2)


(*
Momentum Equations
Subscripts: s -> signal, i -> idler, p -> pump, o -> ordinary axis, e -> effective (mix of ordinary and extraordinary) axes at angle \[Theta]
*)
Subscript[k, so][\[Lambda]_] := (
 2 \[Pi] Subscript[n, o][\[Lambda]])/\[Lambda]
Subscript[k, io][\[Lambda]_] := (
 2 \[Pi] Subscript[n, o][\[Lambda]])/\[Lambda]
Subscript[k, po][\[Lambda]_] := (
 2 \[Pi] Subscript[n, o][\[Lambda]])/\[Lambda]
Subscript[k, se][\[Lambda]_, \[Theta]_] := (
 2 \[Pi] Subscript[n, eff][\[Lambda], \[Theta]])/\[Lambda]
Subscript[k, ie][\[Lambda]_, \[Theta]_] := (
 2 \[Pi] Subscript[n, eff][\[Lambda], \[Theta]])/\[Lambda]
Subscript[k, pe][\[Lambda]_, \[Theta]_] := (
 2 \[Pi] Subscript[n, eff][\[Lambda], \[Theta]])/\[Lambda]


SnellsAngle[\[Theta]1_, n1_, n2_] := ArcSin[(n1/n2) Sin[\[Theta]1]]


SPDCtypeI[\[Theta]OA_,\[Lambda]p_]:=Module[{xval,yval,txval,tyval,val1,val2,f\[Theta]sI,frame1,xmin=0.5,xmax=1.2,ymin=-1.1,ymax=1.1,radius=0.02,plot1,aspectRatio=1/GoldenRatio,colors,maxy=10.7},

{xval,yval}=typeI[\[Theta]OA,\[Lambda]p];

For[i=Length[yval],i>0,i--,
If[!MatchQ[yval[[i]],{_?NumberQ,_?NumberQ}],
xval=Delete[xval,i];
yval=Delete[yval,i];
]
];

txval=Transpose[xval];
tyval=Transpose[yval];
val1=Transpose[{txval[[1]],tyval[[1]]}];
val2=Transpose[{txval[[2]],tyval[[2]]}];
val1={#[[1]],SnellsAngle[#[[2]],Subscript[n, o][#[[1]]],1]}&/@val1;
val2={#[[1]],SnellsAngle[#[[2]],Subscript[n, o][#[[1]]],1]}&/@val2;

(*Change y values to Degrees*)
val1={1,Degree^-1//N}*#&/@val1;
val2={1,Degree^-1//N}*#&/@val2;

f\[Theta]sI=Interpolation[val1];

frame1=Plot[,{x,0,1},Axes->True,AspectRatio->1,
 PlotRange->{{-10,10},{-10,10}},Ticks->{tickDivDegreesBothSides[-10,10,5,4,0],tickDivDegreesBothSides[-10,10,5,4,0]},
ImageSize->200];

xmin=0.5; xmax=1.2;
ymin=-1.1; ymax=1.1;
radius=0.02; (*For the circle around selected points*)

plot1=Plot[{f\[Theta]sI[x],-f\[Theta]sI[x]},
{x,xmin,xmax},
LabelStyle->Directive[Black],
PlotRange->{{xmin,xmax},{Automatic,Automatic}},
PlotLabel->"\!\(\*SubscriptBox[\(\[Theta]\), \(OA\)]\) = 30.5\[Degree]",
AxesLabel->{"\[Lambda] (\[Mu]m)","\!\(\*SubscriptBox[\(\[Theta]\), \(out\)]\)"},
Ticks->{Automatic,tick5Degrees},
PlotLegends->{"\!\(\*SubsuperscriptBox[\(\[Theta]\), \(s\), \(\(\\\ \)\(ext\)\)]\)","\!\(\*SubsuperscriptBox[\(\[Theta]\), \(i\), \(\(\\\ \)\(ext\)\)]\)"},
AspectRatio->1/GoldenRatio,
ImageSize->250
];

colors=Cases[plot1,RGBColor[__],Infinity];

DynamicModule[{\[Lambda]s=0.78,\[Lambda]i=(\[Lambda]s \[Lambda]p)/(\[Lambda]s-\[Lambda]p),\[Theta]s=Quiet@f\[Theta]sI[\[Lambda]s],\[Theta]i=-Quiet@f\[Theta]sI[\[Lambda]i],m\[Lambda]s=\[Lambda]s,m\[Lambda]i=\[Lambda]i,m\[Theta]s=\[Theta]s,m\[Theta]i=\[Theta]i,clicked=True,clickPos=\[Lambda]s,showLine=False},
Dynamic[Row[{Column[{(*Column 1*)
EventHandler[
Dynamic@Show[
plot1,
Graphics[
{colors[[1]],Line[{{\[Lambda]s,0},{\[Lambda]s,\[Theta]s}}],Circle[{\[Lambda]s,\[Theta]s},Scaled[radius{aspectRatio,1}]],
colors[[2]],Line[{{\[Lambda]i,0},{\[Lambda]i,\[Theta]i}}],Circle[{\[Lambda]i,\[Theta]i},Scaled[radius{aspectRatio,1}]]}
],
If[showLine,
Graphics[{Opacity[0.3],
colors[[1]],Line[{{m\[Lambda]s,0},{m\[Lambda]s,m\[Theta]s}}],Circle[{m\[Lambda]s,m\[Theta]s},Scaled[radius{aspectRatio,1}]],
colors[[2]],Line[{{m\[Lambda]i,0},{m\[Lambda]i,m\[Theta]i}}],Circle[{m\[Lambda]i,m\[Theta]i},Scaled[radius{aspectRatio,1}]]}
],
{} (*Else*)
](*end If*)
],{"MouseClicked":>
(clickPos=Round[MousePosition["Graphics",{-0.3xmax,1}],0.001];
If[clickPos[[2]]>=0,
\[Lambda]s=Round[clickPos[[1]],0.001];\[Lambda]i=Round[(\[Lambda]s \[Lambda]p)/(\[Lambda]s-\[Lambda]p),0.001];,
\[Lambda]i=Round[clickPos[[1]],0.001];\[Lambda]s=Round[(\[Lambda]i \[Lambda]p)/(\[Lambda]i-\[Lambda]p),0.001];
];
\[Theta]s=Quiet@f\[Theta]sI[\[Lambda]s];\[Theta]i=-Quiet@f\[Theta]sI[\[Lambda]i];
clicked=True;),
(*"MouseEntered":>(showLine=True),*)
"MouseExited":>(showLine=False),
"MouseMoved":>
(mousePos=MousePosition["Graphics",{-0.3xmax,1}];
If[xmin<=mousePos[[1]]<=xmax,
If[mousePos[[2]]>=0,
m\[Lambda]s=Round[mousePos[[1]],0.001];m\[Lambda]i=Round[(m\[Lambda]s \[Lambda]p)/(m\[Lambda]s-\[Lambda]p),0.001];,
m\[Lambda]i=Round[mousePos[[1]],0.001];m\[Lambda]s=Round[(m\[Lambda]i \[Lambda]p)/(m\[Lambda]i-\[Lambda]p),0.001];
];showLine=True;,
showLine=False];
m\[Theta]s=Quiet@f\[Theta]sI[m\[Lambda]s];m\[Theta]i=-Quiet@f\[Theta]sI[m\[Lambda]i];)
}],(*end of 1^st element in Column1*)
Dynamic@Row[{"\!\(\*SubscriptBox[\(\[Lambda]\), \(s\)]\) = ",Style[Round[1000\[Lambda]s]"nm",colors[[1]]],
If[showLine,Style[" \[RightArrow] "<>ToString[Round[1000m\[Lambda]s]]<>"nm",Opacity[0.5],colors[[1]]]
,""]
}],(*end of 2^nd element in Column1*)
Dynamic@Row[{"\!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) = ",Style[Round[1000\[Lambda]i]"nm",colors[[2]]],
If[showLine,Style[" \[RightArrow] "<>ToString[Round[1000m\[Lambda]i]]<>"nm",Opacity[0.5],colors[[2]]]
,""]
}](*end of 3^rd element in Column1*)
}],(*end Column 1*)
Column[{(*Column 2*)
Dynamic@Show[frame1,
Graphics[{Thick,
colors[[1]],Circle[{0,0},Abs[\[Theta]s]],
colors[[2]],Circle[{0,0},Abs[\[Theta]i]]},
Axes->True,AspectRatio->1, PlotRange->{{-maxy,maxy},{-maxy,maxy}},Ticks->tickIntegerDegrees,ImageSize->250],
If[showLine,
Graphics[{Opacity[0.6],
colors[[1]],Circle[{0,0},Abs[m\[Theta]s]],
colors[[2]],Circle[{0,0},Abs[m\[Theta]i]]}],
{} (*Else*)
]
](*end Dynamic Show*)
},
{}]
}]],(*end Dynamic Row*)
Initialization:>{mousePos={0,0}(*,m\[Lambda]s=1,m\[Lambda]i=1,m\[Theta]s=1,m\[Theta]i=1*)}]
]


typeI[\[Theta]OA_?NumericQ,\[Lambda]p_?NumericQ]:= 
Module[{xvalI,yvalI,intGuess,rules,resultt},
xvalI=Table[{\[Lambda],(\[Lambda] \[Lambda]p)/(\[Lambda]-\[Lambda]p)},{\[Lambda],.50,1.2,.01}];
xvalI=xvalI;
intGuess={4Degree,-4Degree};(*Initial guess for the first point*)

yvalI=Map[
(
rules=Quiet[FindRoot[{
Subscript[k, so][#[[1]]]Cos[Subscript[\[Theta], se]]+Subscript[k, io][#[[2]]]Cos[Subscript[\[Theta], io]]==Subscript[k, pe][\[Lambda]p,\[Theta]OA] , 
Subscript[k, so][#[[1]]]Sin[Subscript[\[Theta], se]]+Subscript[k, io][#[[2]]]Sin[Subscript[\[Theta], io]]==0},
{{Subscript[\[Theta], se],intGuess[[1]]},{Subscript[\[Theta], io],intGuess[[2]]}}]];

If[0==Chop[Subscript[k, so][#[[1]]]Cos[Subscript[\[Theta], se]]+Subscript[k, io][#[[2]]]Cos[Subscript[\[Theta], io]]-Subscript[k, pe][\[Lambda]p,\[Theta]OA]/.rules]
&& 0==Chop[Subscript[k, so][#[[1]]]Sin[Subscript[\[Theta], se]]+Subscript[k, io][#[[2]]]Sin[Subscript[\[Theta], io]]/.rules]
,(*Then*)
resultt=Values[rules];
,(*Else*)
resultt={"nope",intGuess};
];

resultt
)
&,xvalI
];

(*Return both lists*)
{xvalI,yvalI}
]


ClearAll[SPDCtypeII];

SPDCtypeII[\[Lambda]p_]:=SPDCtypeII[
Join[Range[42,42.5,.05],Range[42.75,43,.01],Range[43.005,43.6,.005],Range[43.61,44,.01],Range[44,45,.005]]Degree,
\[Lambda]p]

SPDCtypeII[\[Theta]OA_,\[Lambda]p_]:=Module[{angles,results,resultsInterp,plots,f\[Theta]sIIs,f\[Theta]iIIs,colors={RGBColor[0.368417, 0.506779, 0.709798],RGBColor[0.880722, 0.611041, 0.142051],RGBColor[0.368417, 0.506779, 0.709798],RGBColor[0.880722, 0.611041, 0.142051]},xmax=10,xmin,ymax,ymin,frame1,length},

angles=If[ListQ[\[Theta]OA],\[Theta]OA,{\[Theta]OA}];
results=typeII[#,0.39]&/@angles;

resultsInterp=plotAndInterpolateTypeII[results[[#]],angles[[#]]]&/@Range[Length[angles]];

{plots,f\[Theta]sIIs,f\[Theta]iIIs}=Transpose[resultsInterp];
(*f\[Theta]sIIMins=Quiet[FindMinimum[f\[Theta]sIIs[[#]][\[Theta]s],{\[Theta]s,3.7}]]&/@Range[Length[f\[Theta]sIIs]];*)

length=Length[f\[Theta]sIIs];

xmin=-xmax;
ymax=xmax;
ymin=-ymax;

frame1=Plot[,{x,0,1},Axes->True,AspectRatio->1,
 PlotRange->{{xmin,xmax},{ymin,ymax}},Ticks->{tickDivDegreesBothSides[xmin,xmax,5,4,0],tickDivDegreesBothSides[ymin,ymax,5,4,0]},
ImageSize->200];


firstPlots={};
secondPlots={};
For[j=1,j<=length,j++,
s1=Quiet@FindRoot[f\[Theta]sIIs[[j]][\[Theta]1]==0.780,{\[Theta]1,5}];
s2=Quiet@FindRoot[f\[Theta]sIIs[[j]][\[Theta]2]==0.780,{\[Theta]2,1}];
i1=Quiet@FindRoot[f\[Theta]iIIs[[j]][\[Theta]1]==0.780,{\[Theta]1,-5}];
i2=Quiet@FindRoot[f\[Theta]iIIs[[j]][\[Theta]2]==0.780,{\[Theta]2,-1}];
points={};
If[Chop[f\[Theta]sIIs[[j]][\[Theta]1]-0.780/.s1]==0,
AppendTo[points,{.78,\[Theta]1}/.s1];
If[Chop[\[Theta]2-\[Theta]1/.{s1[[1]],s2[[1]]}]!=0,

AppendTo[points,{.78,\[Theta]2}/.s2];
centerS=Mean[{\[Theta]1,\[Theta]2}/.{s1[[1]],s2[[1]]}];
radiusS=Abs[\[Theta]2-\[Theta]1]/2/.{s1[[1]],s2[[1]]};
,(*Else*)
centerS=\[Theta]1/.s1;
radiusS=0.1;
];
,(*Else*)
centerS=0;
radiusS=0;
];
If[Chop[f\[Theta]iIIs[[j]][\[Theta]1]-0.780/.i1]==0,
AppendTo[points,{.78,\[Theta]1}/.i1];
If[Chop[\[Theta]2-\[Theta]1/.{i1[[1]],i2[[1]]}]!=0,
AppendTo[points,{.78,\[Theta]2}/.i2];
centerI=Mean[{\[Theta]1,\[Theta]2}/.{i1[[1]],i2[[1]]}];
radiusI=Abs[\[Theta]2-\[Theta]1]/2/.{i1[[1]],i2[[1]]};
,(*Else*)
centerI=\[Theta]1/.i1;
radiusI=0.1;
];
,(*Else*)
centerI=0;
radiusI=0;
];

secondPlot=Show[frame1,If[centerS!=0&&centerI!=0,
Graphics[{Thick,
colors[[1]],Circle[{0,centerS},radiusS],
colors[[2]],Circle[{0,centerI},radiusI]}],
{}]];

AppendTo[secondPlots,secondPlot];

firstPlot=Show[ParametricPlot[{{f\[Theta]sIIs[[j]][\[Theta]],\[Theta]},{-2,50}},
{\[Theta],f\[Theta]sIIs[[j]]["Domain"][[1]][[1]],f\[Theta]sIIs[[j]]["Domain"][[1]][[2]]},
PlotStyle->Automatic,
AspectRatio->1/GoldenRatio,
PlotRange->{{0.61,0.96},{-15,15}},
LabelStyle->Directive[Black],
PlotLabel->"Type II SPDC, \!\(\*SubscriptBox[\(\[Theta]\), \(OA\)]\) = "<>ToString[Round[angles[[j]]/Degree,0.01]]<>"\[Degree]",
PlotLegends->{"\!\(\*SubsuperscriptBox[\(\[Theta]\), \(s\), \(\(\\\ \)\(ext\)\)]\)","\!\(\*SubsuperscriptBox[\(\[Theta]\), \(i\), \(\(\\\ \)\(ext\)\)]\)"},
AxesLabel->{"\[Lambda] (\[Mu]m)","\!\(\*SubscriptBox[\(\[Theta]\), \(out\)]\)"},
Ticks->{Automatic,tick5Degrees},
ImageSize->300
],
ParametricPlot[{f\[Theta]iIIs[[j]][\[Theta]],\[Theta]},
{\[Theta],f\[Theta]iIIs[[j]]["Domain"][[1]][[1]],f\[Theta]iIIs[[j]]["Domain"][[1]][[2]]},
PlotStyle->colors[[2]],
AspectRatio->1/GoldenRatio,
PlotRange->{{0.61,1},{-15,20}}
],
ListPlot[points,PlotStyle->Directive[Purple,Thick,Circle]],
Graphics[{Purple,Opacity[0.5],Line[{{0.78,20},{0.78,-15}}]}]
];

AppendTo[firstPlots,firstPlot]
];

Manipulate[
Row[{
firstPlots[[i]],
(*Second Column*)
secondPlots[[i]]
}],
{{i,Round[Length[firstPlots]/2],"\!\(\*SubscriptBox[\(\[Theta]\), \(OA\)]\)"},1,Length[firstPlots],1,Appearance->"Open"},ControlPlacement->Top,LabelStyle->Medium,Paneled->False]
]



ClearAll[typeII];

typeII[\[Theta]OA_?NumericQ,\[Lambda]p_?NumericQ,numPoints_:25]:=
Module[{\[Theta]seVal,\[Lambda]\[Theta]ioVal,xvalII,yvalII,intGuess,rules,resultt},
\[Theta]seVal=Table[Subscript[\[Theta], se],{Subscript[\[Theta], se],-15Degree,13Degree,(13+15)Degree/(numPoints-1)//N}];

\[Lambda]\[Theta]ioVal=Map[
(rules=Quiet[FindRoot[{
Subscript[k, se][\[Lambda],\[Theta]OA-#]Cos[#]+Subscript[k, io][(\[Lambda] \[Lambda]p)/(\[Lambda]-\[Lambda]p)]Cos[Subscript[\[Theta], io]]==Subscript[k, pe][\[Lambda]p,\[Theta]OA] , 
Subscript[k, se][\[Lambda],\[Theta]OA-#]Sin[#]+Subscript[k, io][(\[Lambda] \[Lambda]p)/(\[Lambda]-\[Lambda]p)]Sin[Subscript[\[Theta], io]]==0},
{{\[Lambda],1.},{Subscript[\[Theta], io],0}}]];

If[0==Chop[Subscript[k, se][\[Lambda],\[Theta]OA-#]Cos[#]+Subscript[k, io][(\[Lambda] \[Lambda]p)/(\[Lambda]-\[Lambda]p)]Cos[Subscript[\[Theta], io]]-Subscript[k, pe][\[Lambda]p,\[Theta]OA]/.rules]
&& 0==Chop[Subscript[k, se][\[Lambda],\[Theta]OA-#]Sin[#]+Subscript[k, io][(\[Lambda] \[Lambda]p)/(\[Lambda]-\[Lambda]p)]Sin[Subscript[\[Theta], io]]/.rules]
,(*Then*)
resultt=Values[rules];
,(*Else*)
resultt="nope";
];

resultt
)&,\[Theta]seVal
];
xvalII={First[#,#],(First[#,#] \[Lambda]p)/(First[#,#]-\[Lambda]p)}&/@\[Lambda]\[Theta]ioVal;
yvalII={};
For[i=1,i<=Length[\[Theta]seVal],i++,
AppendTo[yvalII,{\[Theta]seVal[[i]],Last[\[Lambda]\[Theta]ioVal[[i]],\[Lambda]\[Theta]ioVal[[i]]]}]
];

(*Return both lists*)
{xvalII,yvalII}
]


ClearAll[plotAndInterpolateTypeII];
plotAndInterpolateTypeII[{xvalII_,yvalII_},\[Theta]oa_]:= 
Module[{xval,yval,txval,tyval,val1,val2,lpII,f\[Theta]sII,pp\[Theta]sII,f\[Theta]iII,pp\[Theta]iII},
(*xval=xvalII;
yval=yvalII;*)

{xval,yval}=Transpose[
Cases[Transpose[{xvalII,yvalII}],
{{_?RealNumberQ,_?RealNumberQ},{_?RealNumberQ,_?RealNumberQ}}]
];

txval=Transpose[xval];
tyval=Transpose[yval];
val1=Transpose[{txval[[1]],tyval[[1]]}];
val2=Transpose[{txval[[2]],tyval[[2]]}];
val1={#[[1]],SnellsAngle[#[[2]],Subscript[n, eff][#[[1]],#[[2]]],1]}&/@val1; (*not right?*)
val2={#[[1]],SnellsAngle[#[[2]],Subscript[n, o][#[[1]]],1]}&/@val2;

{val1,val2}=Transpose[
Cases[Transpose[{val1,val2}],
{{a_?RealNumberQ,b_?RealNumberQ},{c_?RealNumberQ,d_?RealNumberQ}}->Re[{{a,b},{c,d}}]]
];

(*Change y values to Degrees*)
val1={1,Degree^-1//N}*#&/@val1;
val2={1,Degree^-1//N}*#&/@val2;

lpII=ListPlot[{val1,val2},
LabelStyle->Directive[Black],
PlotLabel->"Type II SPDC, \!\(\*SubscriptBox[\(\[Theta]\), \(OA\)]\) = "<>ToString[Round[\[Theta]oa/Degree,0.1]]<>"\[Degree]",
PlotLegends->{"\!\(\*SubsuperscriptBox[\(\[Theta]\), \(s\), \(ext\)]\)","\!\(\*SubsuperscriptBox[\(\[Theta]\), \(i\), \(ext\)]\)"},
Frame->{{True,True},{True,True}},
FrameTicks->{{tick5Degrees,None},{Automatic,None}},
FrameLabel->{{"\!\(\*SubscriptBox[\(\[Theta]\), \(out\)]\)",None},{"\[Lambda] (\[Mu]m)",None}}];

(*Interpolate curves*)
f\[Theta]sII=Interpolation[Reverse[val1,2]];
pp\[Theta]sII=ParametricPlot[{f\[Theta]sII[y],y},
{y,Min[Map[Last,val1]],Max[Map[Last,val1]]},
PlotStyle->Thin];

f\[Theta]iII=Interpolation[Reverse[val2,2]];
pp\[Theta]iII=ParametricPlot[{f\[Theta]iII[y],y},
{y,Min[Map[Last,val2]],Max[Map[Last,val2]]},
PlotStyle->Directive[ColorData[2][1],Thin]];

{Show[lpII,pp\[Theta]sII,pp\[Theta]iII],f\[Theta]sII,f\[Theta]iII}
]
