(* ::Package:: *)

SIPrefixes = <|
   30 -> "Q", 27 -> "R", 24 -> "Y",
   21 -> "Z", 18 -> "E", 15 -> "P",
   12 -> "T", 9 -> "G", 6 -> "M",
   3 -> "k", 2 -> "h", 1 -> "da",
   0 -> "",
   -1 -> "d", -2 -> "c", -3 -> "m",
   -6 -> "\[Mu]", -9 -> "n", -12 -> "p",
   -15 -> "f", -18 -> "a", -21 -> "z",
   -24 -> "y", -27 -> "r", -30 -> "q"
   |>;

(* Shortcut for StringForm *)
sf[input__] := ToString[StringForm[input]]

(* Returns true if 'num1' is divisible by 'num2' 
works with a mix of floats and integers *)
IsDivisible[num1_, num2_, tol_ : 10^-10] :=
 Module[{quotient}, quotient = num1/num2;
  Abs[Round[quotient] - quotient] < tol]


(*
Generates tick marks from 'min' to 'max' with major tickmarks every 'div'
'minorDivs' is the number of minor tickmarks between each major tickmark
'skip' is the number of major tickmarks without labels between those with labels
'len' is the size of major tickmarks (minor tickmarks are 'len'/2)
('skip' = 1/n - 1 (or -(n-1)/n) adds labels to (n-1) minor tickmarks if 'minorDivs'+1 is divisible by 'n')
*)
ticks[min_, max_, div_, minorDivs_ : 0, skip_ : 0, len_ : 0.024] :=
 Table[{x,
   If[IsDivisible[x - min, div (skip + 1)],
    If[IsDivisible[x, 1], IntegerPart[x + 0.1], Round[x, 0.01]],
    Null],
   If[IsDivisible[x - min, div], len, len/2]},
  {x, min, max, div/(minorDivs + 1)}]

(* Generates tick marks with '\[Degree]' on the labels *)
degreeTick[min_, max_, div_, minorDivs_ : 0, skip_ : 0, 
  len_ : 0.024] :=
 Table[{x,
   If[IsDivisible[x - min, div (skip + 1)],
    If[IsDivisible[x, 1], 
     ToString[IntegerPart[x + Sign[x] 0.1]] "\[Degree]", 
     Round[x, 0.01] "\[Degree]"],
    Null],
   If[Divisible[x - min, div], len, len/2]},
  {x, min - div, max + div, div/(minorDivs + 1)}]



(* The following functions are used for semiAutoTicks *)
getSciFormPower[x_] := RealDigits[x][[2]] - 1;
getEngFormPower[x_] := Floor[getSciFormPower[x], 3];
getEngFormVals[x_] := {x*10^-getEngFormPower[x], getEngFormPower[x]}

Clear[getTickStr]
Clear[getTickVal]

getTickVal::roundTypeError = 
  "Invalid roundType. Accepted inputs: \"Normal\", \"Round\", \
\"Ceiling\", \"Floor\"";

getTickVal[x_, round_List, roundType_ : "Normal"] :=
 Switch[roundType,
  "Normal", 
  First[Nearest[round, getEngFormVals[x][[1]]]]*10^
   getEngFormVals[x][[2]],
  "Round", 
  First[Nearest[round, getEngFormVals[x][[1]]]]*10^
   getEngFormVals[x][[2]],
  "Ceiling", 
  Max[Nearest[round, getEngFormVals[x][[1]], 2]]*10^
   getEngFormVals[x][[2]],
  "Floor", 
  Min[Nearest[round, getEngFormVals[x][[1]], 2]]*10^
   getEngFormVals[x][[2]],
  _, Message[getTickVal::roundTypeError]
  ]

getTickVal[x_, round_ : 1] := 
 Round[getEngFormVals[x][[1]], round]*10^getEngFormVals[x][[2]]

getTickStr[x_, round_List, power_ : Null, roundType_ : "Normal"] := 
 Module[{vals, diff = 0, num},
  vals = getEngFormVals[getTickVal[x, round, roundType]];
  
  If[! MatchQ[power, Null],
   diff = vals[[2]] - power;
   vals[[1]] = vals[[1]]*10^diff;
   If[Chop[vals[[1]]] == 0,
    vals[[2]] = 0,
    vals[[2]] = power];
   ];
  
  num = 1. vals[[1]];
  
  
  If[! MatchQ[round, _Integer],
   If[IsDivisible[num, 1],
     num = Round[num]
     ];
   ];
  
  ToString[num, FormatType -> TraditionalForm] <> SIPrefixes[vals[[2]]]
  ]

getTickStr[x_, round_ : 1, power_ : Null] := 
 Module[{vals, diff = 0, num},
  vals = getEngFormVals[x];
  
  If[! MatchQ[power, Null],
   diff = vals[[2]] - power;
   vals[[1]] = vals[[1]]*10^diff;
   If[Chop[vals[[1]]] == 0,
    vals[[2]] = 0,
    vals[[2]] = power];
   ];
  
  num = Round[vals[[1]], round];
  
  If[! MatchQ[round, _Integer],
   If[IsDivisible[num, 1],
     num = Round[num]
     ];
   ];
  
  ToString[num] <> SIPrefixes[vals[[2]]]
  ]


(* semiAutoTicks produces a simple three-tick ticklist and uses SI prefixes when appropriate *)
Clear[semiAutoTicks]
semiAutoTicks[min_, max_, divs_ : 1, minorDivs_ : 0, skip_ : 0, 
  len_ : 0.024] := 
 Module[{ticks, power, newMax, newMin = 0, newMaxStr, newMinStr, div},
  newMax = getTickVal[N[Max[min, max], 3], roundToTickList, "Ceiling"];
  
  If[Min[min, max] != 0,
   power = getSciFormPower[newMax];
   newMin = Round[Min[min, max], 10^power];
   ];
  
  power = getEngFormPower[newMax];
  
  div = (newMax - newMin)/(divs + 1);
  
  ticks = Table[{x,
     If[IsDivisible[x - newMin, div (skip + 1)],
      getTickStr[x, 0.5, power],
      Null],
     If[IsDivisible[x - newMin, div], len, len/2]},
    {x, newMin - div, newMax + div, div/(minorDivs + 1)}];
  
  {ticks, {newMin, newMax}}
  ]

Clear[getMinMaxTickVals]
getMinMaxTickVals[ticks_, scaling_ : 0] := 
 MinMax[First[Transpose[ticks]], Scaled[scaling]]


(*Clear[title, titleStyle, width, height, aspectR, fieldSize, colors,
labels, zoomFactor, sumOffsets]*)

Options[plotIDQ]={
title->Null,titleStyle->"Subtitle",width->600,height->30,aspectR->0.4,fieldSize->4,colors-> {Blue,Red,Gray,Green},labels -> {"\!\(\*SubscriptBox[\(B\), \(1\)]\)-\!\(\*SubscriptBox[\(B\), \(2\)]\)","\!\(\*SubscriptBox[\(A\), \(1\)]\)-\!\(\*SubscriptBox[\(A\), \(2\)]\)","\!\(\*SubscriptBox[\(A\), \(1\)]\)-\!\(\*SubscriptBox[\(B\), \(2\)]\)","\!\(\*SubscriptBox[\(B\), \(1\)]\)-\!\(\*SubscriptBox[\(A\), \(2\)]\)"},zoomFactor->4/5,sumOffsets->{0,0,0,0}
};

plotIDQ[plotList_, OptionsPattern[]]:=Module[{titleOpt,titleStyleOpt,widthOpt,heightOpt,aspectROpt,fieldSizeOpt,colorsOpt,labelsOpt,zoomOpt,xint,xmin,xmax, ymax,legends,frame,plots,sumOffsetsOpt},

{titleOpt,titleStyleOpt,widthOpt,heightOpt,aspectROpt,fieldSizeOpt,colorsOpt,labelsOpt,zoomOpt,sumOffsetsOpt}=OptionValue[{title,titleStyle,width,height,aspectR,fieldSize,colors,labels,zoomFactor,sumOffsets}];

xint=Round[plotList[[1]][[2]][[1]]-plotList[[1]][[1]][[1]],0.1];
xmin=Floor[First[plotList[[1]]][[1]]-xint/2];
xmax=Floor[Last[plotList[[1]]][[1]]+xint/2];

ymax=Max[plotList[[All,All,2]]];

legends=SwatchLegend[{colorsOpt[[#]]},{labelsOpt[[#]]},LegendMarkers->"SphereBubble"]&/@lenRange[labelsOpt];

frame=Plot[Null,{x,0,1},PlotRange->{{-20,20},{0,50}},Axes->False,Frame->True];

(*If[maxDepth[plotList]];
If[maxDepth[data]<3,
lists={data},
lists=data];*)

plots=ListStepPlot[plotList[[#]],Center,
PlotStyle->{Thickness[0.002],colorsOpt[[#]],Opacity[0.8]},
Filling->Axis,FillingStyle->Opacity[0.4],ImageSize->Large
]&/@lenRange[colorsOpt];

If[titleOpt===Null,
titleOpt=Row[legends],
titleOpt=Style[titleOpt,titleStyleOpt]
];

DynamicModule[{show={True,True,True,True},highlight={True,False,False,False},sumRanges = {{-3,3},{-3,3},{-3,3},{-3,3}}+sumOffsetsOpt,sums={0,0,0,0},xRange={xmin,xmax},dragging=False,activeEdge=None,diff=None,round=1,divisors={"-",1,2,3},showPercent=True},

(*Compute the initial sum for the default range*)
sums=sumPointsWithinRange[plotList[[#]],sumRanges[[#]]]&/@lenRange[sumRanges];

Column[{titleOpt,
EventHandler[Dynamic[
Show[{frame}~Join~Evaluate[If[show[[#]],plots[[#]],{}]&/@lenRange[show]],PlotRange->{xRange,Automatic},Axis->False,FrameLabel->{"Time (ns)","Counts"},ImageSize->widthOpt,AspectRatio->aspectROpt,PlotRangePadding->{{0,0},{0.2,Scaled[0.1]}},ImagePadding->{{40,10},{40,10}},
Epilog->{If[highlight[[#]],
{Opacity[0.1],EdgeForm[Directive[Thickness[0.001],colorsOpt[[#]]]],colorsOpt[[#]],
Rectangle[{Dynamic[sumRanges[[#]][[1]]],-2},{Dynamic[sumRanges[[#]][[2]]],ymax*1.2}]},
{}]&/@lenRange[sumRanges]}]
],
{"MouseDown":>(Module[{mousePos=MousePosition["Graphics"],plts,index},
If[mousePos=!=None,
plts=Select[{1,2,3,4},highlight[[#]]&];
index=minDistIndex[mousePos[[1]],sumRanges[[plts]],(xRange[[2]]-xRange[[1]])/100];
If[index=!=0,
dragging=True;
If[Length[index]==2,
activeEdge={plts[[index[[1]]]],index[[2]]};,
activeEdge={plts[[index[[1]]]]};diff = mousePos[[1]]-sumRanges[[activeEdge[[1]]]];
]
]
]
]),
"MouseDragged":>(Module[{mousePos=MousePosition["Graphics"]},
If[dragging&&mousePos=!=None,
If[Length[activeEdge]==2,
sumRanges[[activeEdge[[1]],activeEdge[[2]]]]=Round[mousePos[[1]],round];,
sumRanges[[activeEdge[[1]]]] = Round[mousePos[[1]]-diff,round];
]
]
]),
"MouseUp":>(dragging=False;activeEdge=None;diff=None),
(*"MouseWheel":>(Module[{mousePos=MousePosition["Graphics"]},If[mousePos=!=None,
Module[{xCenter=mousePos[[1]],xRangeOld,xMin,xMax,zoomAmount},xRangeOld=xRange[[2]]-xRange[[1]];
zoomAmount=xRange*(1-1/zoomFactor);
If[CurrentValue["EventKey"]==="MouseWheelUp",
(*Zoom In*)xMin=xCenter-(xCenter-xRange[[1]])/zoomFactor;
xMax=xCenter+(xRange[[2]]-xCenter)/xRange;,
(*Zoom Out*)xMin=xCenter-(xCenter-xRange[[1]])*zoomFactor;
xMax=xCenter+(xRange[[2]]-xCenter)*zoomFactor;];
xRange={xMin,xMax};]
]
]),*)
"KeyDown":>(test=CurrentValue["EventKey"];round=1),
{"KeyUp","ControlKey"}:>(test=None;round=1)
}
],


(* Range Control *)
Row[{"    ",EventHandler[MouseAppearance[Dynamic@NumberLinePlot[{xRange[[1]]<=x<=xRange[[2]]},{x,xmin,xmax},PlotRange->{xmin,xmax},PlotStyle->Thick,ImageSize -> widthOpt-35,AspectRatio->1/50],"FrameLRResize"],
{
"MouseDown":>(Module[{mousePos=MousePosition["Graphics"],index},
If[mousePos=!=None,
index=minDistIndex[mousePos[[1]],{xRange},(xRange[[2]]-xRange[[1]])/100];
If[index=!=0,
dragging=True;
If[Length[index]==2,
activeEdge={index[[1]],index[[2]]};,
activeEdge={index[[1]]};diff = mousePos[[1]]-xRange;
]
]
]
]),
"MouseDragged":>(Module[{mousePos=MousePosition["Graphics"]},
If[dragging&&mousePos=!=None,
If[Length[activeEdge]==2,
xRange[[activeEdge[[2]]]]=Min[xmax,Max[xmin,Round[mousePos[[1]],round]]];,
xRange = Min[xmax,Max[xmin,#]]&/@Round[mousePos[[1]]-diff,round];
]
]
]),
"MouseUp":>(dragging=False;activeEdge=None;diff=None)
}]
}],
ButtonBar[{
"Zoom In":>(xRange=zoomOpt xRange +Mean[xRange](1-zoomOpt)),
"Zoom Out":>(xRange=Min[xmax,Max[xmin,#]]&/@(xRange/zoomOpt+Mean[xRange](1-1/zoomOpt))),
"Reset":>(xRange={xmin,xmax})
},ImageSize->Automatic,ImageMargins->2],

(* Summation Grid *)
Grid[{{
"Display:","Plot","\[Sum]","\!\(\*SubscriptBox[\(t\), \(1\)]\)                 \!\(\*SubscriptBox[\(t\), \(2\)]\)","\!\(\*SubscriptBox[\(t\), \(1\)]\)  -  \!\(\*SubscriptBox[\(t\), \(2\)]\)",(*Sum*)Null," Comparisons "
}}
~Join~
(Function[plt,
{legends[[plt]],
Checkbox[Dynamic[show[[plt]]]],
Checkbox[Dynamic[highlight[[plt]]]],
ButtonBar[{
"\[LeftTeeArrow]":>(sumRanges[[plt]][[1]]-=round),
"\[RightTeeArrow]":>(sumRanges[[plt]][[1]]+=round),
"\[LeftArrow]\[LeftDoubleBracketingBar]":>(sumRanges[[plt]][[1]]-=round;sumRanges[[plt]][[2]]-=round),
"\[RightArrow]\[LeftDoubleBracketingBar]\[LeftArrow]":>(sumRanges[[plt]][[1]]+=round;sumRanges[[plt]][[2]]-=round),
"\[LeftArrow]\[LeftDoubleBracketingBar]\[RightArrow]":>(sumRanges[[plt]][[1]]-=round;sumRanges[[plt]][[2]]+=round),
"\[RightDoubleBracketingBar]\[RightArrow]":>(sumRanges[[plt]][[1]]+=round;sumRanges[[plt]][[2]]+=round),
"\[LeftTeeArrow]":>(sumRanges[[plt]][[2]]-=round),
"\[RightTeeArrow]":>(sumRanges[[plt]][[2]]+=round)
},ImageSize->{22,12},ImageMargins->{{1,0},{1,0}},FrameMargins->{{0,0},{0,-3}},Alignment->Center],
(*IntervalSlider[
Dynamic[sumRanges[[plt]],(sumRanges[[plt]][[1]]=Round[#[[1]],round];sumRanges[[plt]][[2]]=Round[#[[2]],round])&],{xmin,xmax,xint},ContinuousAction->True,
Appearance \[Rule] {"ThumbAppearance" \[Rule] {Style["I",18],None,Style["I",18]}}
],*)
Row[{"\[Sum](",
InputField[Dynamic[sumRanges[[plt]][[1]],(sumRanges[[plt]][[1]]=Round[#,round])&],Number,FieldSize->fieldSize,ImageSize->{42,20}],
"-",
InputField[Dynamic[sumRanges[[plt]][[2]],(sumRanges[[plt]][[2]]=Round[#,round])&],Number,FieldSize->fieldSize,ImageSize->{42,20}],
")="}],
Dynamic[sums[[plt]]=sumPointsWithinRange[plotList[[plt]],sumRanges[[plt]]]],
Row[{
"  /",
PopupMenu[Dynamic[divisors[[plt]]],{"-"}~Join~lenRange[sums]],
" = ",
Dynamic@If[NumberQ[divisors[[plt]]],
PercentForm[N[sums[[plt]]/sums[[divisors[[plt]]]]],3],
" "]
},ImageSize->{120,25},Alignment->Left]
}]/@lenRange[legends])
,Frame->True,FrameStyle->Gray,Spacings->{{1,1},{1,0.5,0,0,0}}],
Row[{"Minimum Increment: ",RadioButtonBar[Dynamic[round],{0.1,1},ImageMargins->5]}](*,Dynamic[CurrentValue["ModifierKeys"]]Dynamic[dragging],Dynamic[activeEdge],Dynamic[diff],
Dynamic[sums=sumPointsWithinRange[plotList[[#]],sumRanges[[#]]]&/@lenRange[sumRanges];
]*)
,Null,Null
},Alignment->Center]
]
]

(*Function to sum data within a range*)
sumPointsWithinRange[data_,range_]:=
	Total[Select[data,Min[range]<=#[[1]]<=Max[range]&][[All,2]]];



(*Clear[factors, titles, titleStyle, width, height, aspectR, fieldSize, \
colors, labels]*)

Clear[plotAllData]

Options[plotAllData] = 
  Join[{factors -> Null, titles -> Null}, 
   FilterRules[Options[plotIDQ], Except[{title}]]];

plotAllData[data_,keys_,opts: OptionsPattern[]]:=Module[{factorsOpt,titlesOpt,otherOptions,title1,plotList=Table[Null,{j,Length[keys]}]},

factorsOpt=OptionValue[factors];
titlesOpt=OptionValue[titles];
otherOptions=FilterRules[{opts},Except[{factors,titles}]];

title1=plotList;

For[i=1,i<=Length[keys],i++,
plotList[[i]] = cxy[data[keys[[i]]],"x",ch[#],safeAccessNum[factorsOpt,{i,#},1]]&/@lenRange[Rest[data[keys[[i]]]]];
title1[[i]] = safeAccessStr[titlesOpt,i,keys[[i]]<>" Cycles"];
];

Column[
plotIDQ[
plotList[[#]],
Sequence@@Join[{title->title1[[#]]},otherOptions]
]&/@lenRange[keys]
]
]

plotIDQTitled[plotList_,title_String]:=Module[{width=600,height=30,aspectR=0.4,fieldSize=3,xint,xmin,xmax,ymax,colors= {Blue,Red,Gray,Green},zoomF=4/5,labels = {"\!\(\*SubscriptBox[\(B\), \(1\)]\)-\!\(\*SubscriptBox[\(B\), \(2\)]\)","\!\(\*SubscriptBox[\(A\), \(1\)]\)-\!\(\*SubscriptBox[\(A\), \(2\)]\)","\!\(\*SubscriptBox[\(A\), \(1\)]\)-\!\(\*SubscriptBox[\(B\), \(2\)]\)","\!\(\*SubscriptBox[\(B\), \(1\)]\)-\!\(\*SubscriptBox[\(A\), \(2\)]\)"},legends,frame,plots,title1=Style[title,"Subtitle"]},

xint=Round[plotList[[1]][[2]][[1]]-plotList[[1]][[1]][[1]],0.1];
xmin=Floor[First[plotList[[1]]][[1]]];
xmax=Ceiling[Last[plotList[[1]]][[1]]];

ymax=Max[plotList[[All,All,2]]];

legends=SwatchLegend[{colors[[#]]},{labels[[#]]},LegendMarkers->"SphereBubble"]&/@lenRange[labels];

frame=Plot[Null,{x,0,1},PlotRange->{{-20,20},{0,50}},Axes->False,Frame->True];

plots=ListPlot[plotList[[#]],
PlotStyle->{Thickness[0.002],colors[[#]],Opacity[0.8]},
Filling->Axis,Joined->True,ImageSize->Large,PlotRange->All
]&/@lenRange[colors];


DynamicModule[{show={True,True,True,True},highlight={True,False,False,False},sumRanges = {{-3,3},{-3,3},{-3,3},{-3,3}},sums={0,0,0,0},divisors={"-",1,2,3},xRange={xmin,xmax},dragging=False,activeEdge=None,diff=None,round=1,test=None},

(*Function to sum data within a range*)sumPointsWithinRange[data_,range_]:=Total[Select[data,Min[range]<=#[[1]]<=Max[range]&][[All,2]]];

(*Compute the initial sum for the default range*)
sums=sumPointsWithinRange[plotList[[#]],sumRanges[[#]]]&/@lenRange[sumRanges];

Column[{title1,
EventHandler[Dynamic[
Show[{frame}~Join~Evaluate[If[show[[#]],plots[[#]],{}]&/@lenRange[show]],PlotRange->{xRange,All},Axis->False,FrameLabel->{"Time (ns)","Counts"},ImageSize->width,AspectRatio->aspectR,PlotRangePadding->{{0,0},{0.2,Scaled[0.1]}},ImagePadding->{{40,10},{40,10}},
Epilog->{If[highlight[[#]],
{Opacity[0.1],EdgeForm[Directive[Thickness[0.001],colors[[#]]]],colors[[#]],
Rectangle[{Dynamic[sumRanges[[#]][[1]]],-2},{Dynamic[sumRanges[[#]][[2]]],ymax*1.2}]},
{}]&/@lenRange[sumRanges]}]
],
{"MouseDown":>(Module[{mousePos=MousePosition["Graphics"],plts,index},
If[mousePos=!=None,
plts=Select[{1,2,3,4},highlight[[#]]&];
index=minDistIndex[mousePos[[1]],sumRanges[[plts]],(xRange[[2]]-xRange[[1]])/100];
If[index=!=0,
dragging=True;
If[Length[index]==2,
activeEdge={plts[[index[[1]]]],index[[2]]};,
activeEdge={plts[[index[[1]]]]};diff = mousePos[[1]]-sumRanges[[activeEdge[[1]]]];
]
]
]
]),
"MouseDragged":>(Module[{mousePos=MousePosition["Graphics"]},
If[dragging&&mousePos=!=None,
If[Length[activeEdge]==2,
sumRanges[[activeEdge[[1]],activeEdge[[2]]]]=Round[mousePos[[1]],round];,
sumRanges[[activeEdge[[1]]]] = Round[mousePos[[1]]-diff,round];
]
]
]),
"MouseUp":>(dragging=False;activeEdge=None;diff=None),
"KeyDown":>(test=CurrentValue["EventKey"];round=1),
{"KeyUp","ControlKey"}:>(test=None;round=1)
}
],

(* Range Controls *)
Row[{"    ",EventHandler[MouseAppearance[Dynamic@NumberLinePlot[{xRange[[1]]<=x<=xRange[[2]]},{x,xmin,xmax},PlotRange->{xmin,xmax},PlotStyle->Thick,ImageSize -> width-35,AspectRatio->1/50],"FrameLRResize"],
{
"MouseDown":>(Module[{mousePos=MousePosition["Graphics"],index},
If[mousePos=!=None,
index=minDistIndex[mousePos[[1]],{xRange},(xRange[[2]]-xRange[[1]])/100];
If[index=!=0,
dragging=True;
If[Length[index]==2,
activeEdge={index[[1]],index[[2]]};,
activeEdge={index[[1]]};diff = mousePos[[1]]-xRange;
]
]
]
]),
"MouseDragged":>(Module[{mousePos=MousePosition["Graphics"]},
If[dragging&&mousePos=!=None,
If[Length[activeEdge]==2,
xRange[[activeEdge[[2]]]]=Min[xmax,Max[xmin,Round[mousePos[[1]],round]]];,
xRange = Min[xmax,Max[xmin,#]]&/@Round[mousePos[[1]]-diff,round];
]
]
]),
"MouseUp":>(dragging=False;activeEdge=None;diff=None)
}]
}],
ButtonBar[{
"Zoom In":>(xRange=zoomF xRange +Mean[xRange](1-zoomF)),
"Zoom Out":>(xRange=Min[xmax,Max[xmin,#]]&/@(xRange/zoomF+Mean[xRange](1-1/zoomF))),
"Reset":>(xRange={xmin,xmax})
},ImageSize->Automatic,ImageMargins->2],

(* Summation Grid *)
Grid[{{
"Display:","Plot","\[Sum]","\!\(\*SubscriptBox[\(t\), \(1\)]\)                 \!\(\*SubscriptBox[\(t\), \(2\)]\)","\!\(\*SubscriptBox[\(t\), \(1\)]\)  -  \!\(\*SubscriptBox[\(t\), \(2\)]\)",(*Sum*)Null," Comparisons "
}}
~Join~
(Function[plt,
{legends[[plt]],
Checkbox[Dynamic[show[[plt]]]],
Checkbox[Dynamic[highlight[[plt]]]],
ButtonBar[{
"\[LeftTeeArrow]":>sumRanges[[plt]][[1]]--,
"\[RightTeeArrow]":>sumRanges[[plt]][[1]]++,
"\[LeftArrow]\[LeftDoubleBracketingBar]":>(sumRanges[[plt]][[1]]--;sumRanges[[plt]][[2]]--),
"\[RightArrow]\[LeftDoubleBracketingBar]\[LeftArrow]":>(sumRanges[[plt]][[1]]++;sumRanges[[plt]][[2]]--),
"\[LeftArrow]\[LeftDoubleBracketingBar]\[RightArrow]":>(sumRanges[[plt]][[1]]--;sumRanges[[plt]][[2]]++),
"\[RightDoubleBracketingBar]\[RightArrow]":>(sumRanges[[plt]][[1]]++;sumRanges[[plt]][[2]]++),
"\[LeftTeeArrow]":>sumRanges[[plt]][[2]]--,
"\[RightTeeArrow]":>sumRanges[[plt]][[2]]++
},ImageSize->{22,12},ImageMargins->{{1,0},{1,0}},FrameMargins->{{0,0},{0,-3}},Alignment->Center],
(*IntervalSlider[
Dynamic[sumRanges[[plt]],(sumRanges[[plt]][[1]]=Round[#[[1]],round];sumRanges[[plt]][[2]]=Round[#[[2]],round])&],{xmin,xmax,xint},ContinuousAction->True,
Appearance \[Rule] {"ThumbAppearance" \[Rule] {Style["I",18],None,Style["I",18]}}
],*)
Row[{"\[Sum](",
InputField[Dynamic[sumRanges[[plt]][[1]],(sumRanges[[plt]][[1]]=Round[#,round])&],Number,FieldSize->fieldSize,ImageSize->{42,20}],
"-",
InputField[Dynamic[sumRanges[[plt]][[2]],(sumRanges[[plt]][[2]]=Round[#,round])&],Number,FieldSize->fieldSize,ImageSize->{42,20}],
")="}],
Dynamic[sums[[plt]]=sumPointsWithinRange[plotList[[plt]],sumRanges[[plt]]]],
Row[{
"  /",
PopupMenu[Dynamic[divisors[[plt]]],{"-"}~Join~lenRange[sums]],
" = ",
Dynamic@If[NumberQ[divisors[[plt]]],
PercentForm[N[sums[[plt]]/sums[[divisors[[plt]]]]],3],
" "]
},ImageSize->{120,25},Alignment->Left]
}]/@lenRange[legends])
,Frame->True,FrameStyle->Gray,Spacings->{{1,1},{1,0.5,0,0,0}}],
Row[{"Minimum Increment: ",RadioButtonBar[Dynamic[round],{0.1,1},ImageMargins->5]}](*,Dynamic[CurrentValue["ModifierKeys"]]Dynamic[dragging],Dynamic[activeEdge],Dynamic[diff],
Dynamic[sums=sumPointsWithinRange[plotList[[#]],sumRanges[[#]]]&/@lenRange[sumRanges];
]*)
},Alignment->Center]
]
]

(*Function to sum data within a range*)
sumPointsWithinRange[data_, range_] := 
  Total[Select[data, Min[range] <= #[[1]] <= Max[range] &][[All, 2]]];


Unprotect[#] & /@ Keys[Options[visListPlot]];
Clear[visListPlot]

Options[visListPlot] = {
   title -> Null, titleStyle -> "Subtitle", factor -> 1, yMax -> Null,
    yMin -> 0, scaling -> {0.04, 0.2}, size -> 220, 
   aspectR -> GoldenRatio^-1, plotStyle -> {Blue, Red, Green, Orange},
    axesLabels -> {"Measurement Angle", "Coinc. per Minute"}, 
   labels -> {"Horizontal", "Diagonal", "Vertical", "Anti-Diagonal"}, 
   xdiv -> 45, xminorDivs -> 2, 
   plotMarkers -> {\[FilledSmallCircle], \[FilledSmallCircle], \[EmptySmallCircle], \[EmptySmallCircle]}, yskip -> 0
   };
Protect[#] & /@ Keys[Options[visListPlot]];

visListPlot[data_Association, opts : OptionsPattern[]] := 
 visListPlot[Values[data], opts]

visListPlot[data_List, opts : OptionsPattern[]] := 
 Module[{lists, xmax, ymax, xmin, ymin, plot, bottomTicks, topTicks, 
   leftTicks, rightTicks, xskip = 0, numMajTicks, titleO, titleStyleO,
    factorO, yMaxO, yMinO, scalingO, sizeO, aspectRO, pStyleO, 
   axesLabelsO, labelsO, xdivO, xminorDivsO, markersO, yskipO},
  
  {titleO, titleStyleO, factorO, yMaxO, yMinO, scalingO, sizeO, 
    aspectRO, pStyleO, axesLabelsO, labelsO, xdivO, xminorDivsO, 
    markersO, yskipO} = OptionValue[
    {title, titleStyle, factor, yMax, yMin, scaling, size, aspectR, 
     plotStyle, axesLabels, labels, xdiv, xminorDivs, plotMarkers, 
     yskip}
    ];
  
  If[maxDepth[data] < 3,
   lists = {data},
   lists = data];
  
  {xmin, xmax} = MinMax[MinMax[xValues[#]] & /@ lists];
  {ymin, ymax} = MinMax[MinMax[yValues[#]] & /@ lists];
  
  If[! MatchQ[yMinO, Null],
   ymin = yMinO];
  If[! MatchQ[yMaxO, Null],
   ymax = yMaxO];
  
  numMajTicks = Floor[(xmax - xmin)/xdivO];
  xskip = numMajTicks - 1;
  For[i = xskip, i >= 1, i--,
   If[Divisible[numMajTicks, i] && (numMajTicks/i) <= (sizeO/45),
     xskip = i - 1];
   ];
  
  For[i = 2, i <= xminorDivsO + 1, i++,
   If[Divisible[xminorDivsO + 1, i] && (numMajTicks*i) <= (sizeO/45),
     xskip = 1/i - 1];
   ];
  
  bottomTicks = 
   degreeTick[Floor[xmin, xdivO], Ceiling[xmax, xdivO], xdivO, 
    xminorDivsO, xskip];
  topTicks = {#[[1]], Null, #[[3]]} & /@ bottomTicks;
  
  
  {leftTicks, {ymin, ymax}} = semiAutoTicks[ymin, ymax, 1, 0, yskipO];
  rightTicks = {#[[1]], Null, #[[3]]} & /@ leftTicks;
  
  {xmin, xmax} = MinMax[{xmin, xmax}, Scaled[scalingO[[1]]]];
  {ymin, ymax} = MinMax[{ymin, ymax}, Scaled[scalingO[[2]]]];
  
  plot = ListPlot[lists, PlotRange -> {{xmin, xmax}, {ymin, ymax}}, 
    PlotLabel -> titleO, PlotLegends -> labelsO, PlotStyle -> pStyleO,
     Axes -> False, PlotMarkers -> markersO, Frame -> True, 
    FrameStyle -> Directive[Black, Thickness[0.006], FontSize -> 12], 
    FrameLabel -> axesLabelsO, 
    FrameTicks -> {{leftTicks, rightTicks}, {bottomTicks, topTicks}}, 
    ImageSize -> sizeO, AspectRatio -> aspectRO];
  
  
  plot
  ]



Clear[nlmFitSin, x]

Options[nlmFitSin] = Options[visListPlot];
(*
Options[visListPlot]={
title->Null,titleStyle->"Subtitle",factor->1,yMax->Null,yMin->0,\
scaling->{0.04,0.2},size->220,aspectR->GoldenRatio^-1,plotStyle->{\
Blue,Red},axesLabels -> {"Measurement Angle","Coinc. per \
Minute"},labels -> \
{"Horizontal","Diagonal","Vertical","Anti-Diagonal"},xdiv->45,\
xminorDivs->2,plotMarkers\[Rule]{\[FilledSmallCircle],\
\[FilledSmallCircle],\[EmptySmallCircle],\[EmptySmallCircle]},yskip->0
};
*)

nlmFitSin[data_Association, maxAngle_, weightPower_ : 2, 
  includeAdjAngle_ : False, opts : OptionsPattern[]] := 
 nlmFitSin[Values[data], maxAngle, weightPower, includeAdjAngle, opts]

nlmFitSin[data_List, maxAngle_, weightPower_ : 2, 
  includeAdjAngle_ : False, opts : OptionsPattern[]] := 
 Quiet[Module[{dataLists, aRanges, visApprox, vRanges, 
    angleRange = {-55, 125}, maxAngles, weights, fits, fitFuncs, 
    fitsAngle = {}, fitAngleFuncs = {}, plotAngleStyleO, outputGrid, 
    labelsO, plotStyleO},
   
   {labelsO, plotStyleO} = OptionValue[{labels, plotStyle}];
   plotAngleStyleO = {#, Thickness[0.005], Opacity[0.6], Dashed} & /@ 
     plotStyleO;
   plotStyleO = {#, Thickness[0.005]} & /@ plotStyleO;
   
   If[maxDepth[data] < 3,
    dataLists = {data},
    dataLists = data];
   
   aRanges = {};
   vRanges = {};
   
   weights = {};
   Do[
    AppendTo[aRanges, MinMax[yValues[dl], Scaled[-0.2]]];
    visApprox = visMaxApprox[dl];
    AppendTo[
     vRanges, {Max[First[visApprox] - 0.2, 0], 
      Min[First[visApprox] + 0.2, 1]}];
    AppendTo[weights, Evaluate[1/yUncerts[dl]^weightPower]];
    , {dl, dataLists}];
   
   maxAngles = 
    safeAccessNum[maxAngle, #, 
       If[Length[maxAngle] == 0, maxAngle]] & /@ lenRange[dataLists];
   
   fits = If[! MatchQ[maxAngles[[#]], Null],
       
       NonlinearModelFit[onlyValues[dataLists[[#]]],
        {a (1 + v Cos[2 (x - maxAngles[[#]]) Degree]),
         {Between[a, aRanges[[#]]], Between[v, vRanges[[#]]]}},
        {a, v}, x, Weights -> weights[[#]]]
       ,
       NonlinearModelFit[onlyValues[dataLists[[#]]],
        {a (1 + v Cos[2 (x - \[Theta]) Degree]),
         {Between[a, aRanges[[#]]], Between[v, vRanges[[#]]], 
          Between[\[Theta], angleRange]}},
        {a, v, \[Theta]}, x, Weights -> weights[[#]]]
       
       ] & /@ lenRange[dataLists];
   
   fitFuncs = Normal[#] & /@ fits;
   
   
   If[includeAdjAngle,
    maxAngles = If[! MatchQ[#, Null], #, 0] & /@ maxAngles;
    
    fitsAngle = NonlinearModelFit[onlyValues[dataLists[[#]]],
        {a (1 + v Cos[2 (x - \[Theta]) Degree]),
         {Between[a, aRanges[[#]]], Between[v, vRanges[[#]]], 
          Between[\[Theta], maxAngles[[#]] + angleRange]}},
        {a, v, \[Theta]}, x, Weights -> weights[[#]]] & /@ 
      lenRange[dataLists];
    
    fitAngleFuncs = Normal[#] & /@ fitsAngle;
    ];
   
   
   outputGrid = Grid[{
      labelsO[[#]] & /@ lenRange[fits],
      ShortParameterTable[#] & /@ fits,
      If[Length[fitsAngle] > 0, 
       ShortParameterTable[#] & /@ fitsAngle, {}]
      }];
   
   Row[{outputGrid,
     Show[visListPlot[dataLists, opts],
      Plot[fitFuncs, {x, -100, 400}, PlotStyle -> plotStyleO],
      Plot[fitAngleFuncs, {x, -100, 400}, 
       PlotStyle -> plotAngleStyleO]]},
    "   \t   "]
   
   ], {FittedModel::constr}]

Options[plotAllData] = 
  Join[{factors -> Null, titles -> Null}, 
   FilterRules[Options[plotIDQ], Except[{title}]]];

plotAllData[data_, keys_, opts : OptionsPattern[]] := 
 Module[{factorsOpt, titlesOpt, otherOptions, title1, 
   plotList = Table[Null, {j, Length[keys]}]},
  
  factorsOpt = OptionValue[factors];
  titlesOpt = OptionValue[titles];
  otherOptions = FilterRules[{opts}, Except[{factors, titles}]];
  
  title1 = plotList;
  
  For[i = 1, i <= Length[keys], i++,
   plotList[[i]] = 
    cxy[data[keys[[i]]], "x", ch[#], 
       safeAccessNum[factorsOpt, {i, #}, 1]] & /@ 
     lenRange[Rest[data[keys[[i]]]]];
   title1[[i]] = safeAccessStr[titlesOpt, i, keys[[i]] <> " Cycles"];
   ];
  
  Column[
   plotIDQ[
      plotList[[#]],
      Sequence @@ Join[{title -> title1[[#]]}, otherOptions]
      ] & /@ lenRange[keys]
   ]
  ]

Unprotect[#] & /@ Keys[Options[nlmFitSin2]];
Clear[nlmFitSin2]

Options[nlmFitSin2] = 
  Join[{peakAngle -> Null, weightPower -> 2, includeAdjAngle -> False,
     gridLabels -> {"Horizontal", "Diagonal", "Vertical", 
      "Anti-Diagonal"}, labels -> ""}, 
   FilterRules[Options[visListPlot], Except[{labels}]]];
Protect[#] & /@ Keys[Options[nlmFitSin2]];

nlmFitSin2[data_Association, opts : OptionsPattern[]] := 
 nlmFitSin2[Values[data], opts]

nlmFitSin2[data_List, opts : OptionsPattern[]] := 
 Quiet[Module[{dataLists, amps, vGuesses, peakAngles, weights, fits, 
    fitFuncs, fitsAngle = {}, fitAngleFuncs = {}, outputGrid, 
    visPlotOptions, peakAngleO, weightPowerO, includeAdjAngleO, 
    plotAngleStyleO, gridLabelsO, labelsO, plotStyleO},
   
   {peakAngleO, weightPowerO, includeAdjAngleO, gridLabelsO, labelsO, 
     plotStyleO} = 
    OptionValue[{peakAngle, weightPower, includeAdjAngle, gridLabels, 
      labels, plotStyle}];
   visPlotOptions = 
    FilterRules[{opts}, 
     Except[{peakAngle, weightPower, includeAdjAngle, gridLabels}]];
   
   If[maxDepth[data] < 3,
    dataLists = {data},
    dataLists = data];
   
   gridLabelsO = 
    SwatchLegend[{plotStyleO[[#]]}, {gridLabelsO[[#]]}, 
       LegendMarkers -> "SphereBubble"] & /@ lenRange[dataLists];
   plotAngleStyleO = {#, Thickness[0.006], Opacity[0.6], Dashed} & /@ 
     plotStyleO;
   plotStyleO = {#, Thickness[0.005], Opacity[0.8]} & /@ plotStyleO;
   
   amps = Mean[ys[#]] & /@ dataLists;
   vGuesses = visMaxApprox[#] & /@ dataLists;
   (*weights=Map[Evaluate[1/yUncerts[#]^weightPowerO]&,
   dataLists,{2}];*)
   weights = 1/Evaluate[yUncerts[#]^weightPowerO] & /@ dataLists;
   
   peakAngles = 
    safeAccessNum[peakAngleO, #, 
       If[Length[peakAngleO] == 0, peakAngleO]] & /@ 
     lenRange[dataLists];
   
   fits = If[! MatchQ[peakAngles[[#]], Null],
       
       NonlinearModelFit[onlyValues[dataLists[[#]]],
        a (1 + v Cos[2 (x - peakAngles[[#]]) Degree]),
        {{a, amps[[#]][[1]]}, {v, vGuesses[[#]][[1]]}}, x,
        Weights -> weights[[#]], Method -> "LevenbergMarquardt"]
       ,
       NonlinearModelFit[onlyValues[dataLists[[#]]],
        a (1 + v Cos[2 (x - \[Theta]) Degree]),
        {{a, amps[[#]][[1]]}, {v, vGuesses[[#]][[1]]}, {\[Theta], 
          15}}, x,
        Weights -> weights[[#]], Method -> "LevenbergMarquardt"]
       
       ] & /@ lenRange[dataLists];
   
   fitFuncs = Normal[#] & /@ fits;
   
   
   If[includeAdjAngleO,
    peakAngles = If[! MatchQ[#, Null], #, 15] & /@ peakAngles;
    
    fitsAngle = NonlinearModelFit[onlyValues[dataLists[[#]]],
        a (1 + v Cos[2 (x - \[Theta]) Degree]),
        {{a, amps[[#]][[1]]}, {v, vGuesses[[#]][[1]]}, {\[Theta], 
          peakAngles[[#]]}}, x,
        Weights -> weights[[#]], Method -> "LevenbergMarquardt"] & /@ 
      lenRange[dataLists];
    
    fitAngleFuncs = Normal[#] & /@ fitsAngle;
    ];
   
   
   outputGrid = Grid[{
      gridLabelsO[[#]] & /@ lenRange[fits],
      ShortParameterTable3[#] & /@ fits,
      If[Length[fitsAngle] > 0, 
       ShortParameterTable3[#] & /@ fitsAngle, {}]
      }];
   
   Row[{
     Show[
      visListPlot[dataLists, 
       Sequence @@ Join[{labels -> labelsO}, visPlotOptions]],
      Plot[fitFuncs, {x, -100, 400}, PlotStyle -> plotStyleO],
      Plot[fitAngleFuncs, {x, -100, 400}, 
       PlotStyle -> plotAngleStyleO]],
     outputGrid
     }, "   "]
   
   ], {FittedModel::constr, NonlinearModelFit::lmnl}]

Unprotect[#] & /@ Keys[Options[nlmFitSin3]];
Clear[nlmFitSin3]

Options[nlmFitSin3] = 
  Join[{peakAngle -> Null, weightPower -> 2, includeAdjAngle -> False,
     gridLabels -> {"Horizontal", "Diagonal", "Vertical", 
      "Anti-Diagonal"}, labels -> ""}, 
   FilterRules[Options[visListPlot], Except[{labels}]]];
Protect[#] & /@ Keys[Options[nlmFitSin3]];

nlmFitSin3[data_Association, opts : OptionsPattern[]] := 
 nlmFitSin3[Values[data], opts]

nlmFitSin3[data_List, opts : OptionsPattern[]] := 
 Quiet[Module[{dataLists, amps, vGuesses, peakAngles, weights, fits, 
    fitFuncs, fitsAngle = {}, fitAngleFuncs = {}, outputGrid, 
    visPlotOptions, peakAngleO, weightPowerO, includeAdjAngleO, 
    plotAngleStyleO, gridLabelsO, labelsO, plotStyleO},
   
   {peakAngleO, weightPowerO, includeAdjAngleO, gridLabelsO, labelsO, 
     plotStyleO} = 
    OptionValue[{peakAngle, weightPower, includeAdjAngle, gridLabels, 
      labels, plotStyle}];
   visPlotOptions = 
    FilterRules[{opts}, 
     Except[{peakAngle, weightPower, includeAdjAngle, gridLabels}]];
   
   If[maxDepth[data] < 3,
    dataLists = {data},
    dataLists = data];
   
   gridLabelsO = 
    SwatchLegend[{plotStyleO[[#]]}, {gridLabelsO[[#]]}, 
       LegendMarkers -> "SphereBubble"] & /@ lenRange[dataLists];
   plotAngleStyleO = {#, Thickness[0.006], Opacity[0.6], Dashed} & /@ 
     plotStyleO;
   plotStyleO = {#, Thickness[0.005], Opacity[0.8]} & /@ plotStyleO;
   
   amps = Mean[ys[#]] & /@ dataLists;
   vGuesses = ArcSin[Sqrt[visMaxApprox[#][[1]]]] & /@ dataLists;
   (*weights=Map[Evaluate[1/yUncerts[#]^weightPowerO]&,
   dataLists,{2}];*)
   weights = 1/Evaluate[yUncerts[#]^weightPowerO] & /@ dataLists;
   
   peakAngles = 
    safeAccessNum[peakAngleO, #, 
       If[Length[peakAngleO] == 0, peakAngleO]] & /@ 
     lenRange[dataLists];
   
   fits = If[! MatchQ[peakAngles[[#]], Null],
       
       NonlinearModelFit[onlyValues[dataLists[[#]]],
        a (1 + Sin[v]^2 Cos[2 (x - peakAngles[[#]]) Degree]),
        {{a, amps[[#]][[1]]}, {v, vGuesses[[#]]}}, x,
        Weights -> weights[[#]],
        Method -> "LevenbergMarquardt", AccuracyGoal -> 5]
       ,
       NonlinearModelFit[onlyValues[dataLists[[#]]],
        a (1 + Sin[v]^2 Cos[2 (x - \[Theta]) Degree]),
        {{a, amps[[#]][[1]]}, {v, vGuesses[[#]]}, {\[Theta], 15}}, x,
        Weights -> weights[[#]],
        Method -> "LevenbergMarquardt", AccuracyGoal -> 5]
       
       ] & /@ lenRange[dataLists];
   
   fitFuncs = Normal[#] & /@ fits;
   
   
   If[includeAdjAngleO,
    peakAngles = If[! MatchQ[#, Null], #, 15] & /@ peakAngles;
    
    fitsAngle = NonlinearModelFit[onlyValues[dataLists[[#]]],
        a (1 + Sin[v]^2 Cos[2 (x - \[Theta]) Degree]),
        {{a, amps[[#]][[1]]}, {v, vGuesses[[#]]}, {\[Theta], 
          peakAngles[[#]]}}, x,
        Weights -> weights[[#]],
        Method -> "LevenbergMarquardt", AccuracyGoal -> 5] & /@ 
      lenRange[dataLists];
    
    fitAngleFuncs = Normal[#] & /@ fitsAngle;
    ];
   
   
   outputGrid = Grid[{
      gridLabelsO[[#]] & /@ lenRange[fits],
      ParamTableVisSine[#] & /@ fits,
      If[Length[fitsAngle] > 0, 
       ParamTableVisSine[#] & /@ fitsAngle, {}]
      }];
   
   Row[{
     Show[
      visListPlot[dataLists, 
       Sequence @@ Join[{labels -> labelsO}, visPlotOptions]],
      Plot[fitFuncs, {x, -100, 400}, PlotStyle -> plotStyleO],
      Plot[fitAngleFuncs, {x, -100, 400}, 
       PlotStyle -> plotAngleStyleO]],
     outputGrid
     }, "   "]
   
   ], {FittedModel::constr, NonlinearModelFit::lmnl}]

ShortParameterTable2[fitmodel_] :=
 Module[{datalist, keys, header, vals},
  header = {"", "Value", "\[Sigma]"};
  keys = Keys[fitmodel["BestFitParameters"]];
  
  vals = sigFigs[#[[1]], #[[2]]] & /@ 
    fitmodel["ParameterTableEntries"];
  
  datalist = 
   Join[{header}, 
    Transpose[{keys, Transpose[vals][[1]], Transpose[vals][[2]]}]];
  
  \!\(\*
TagBox[
StyleBox[
RowBox[{"Style", "[", 
RowBox[{
RowBox[{"Grid", "[", 
RowBox[{"datalist", ",", 
RowBox[{"Alignment", "->", 
RowBox[{"{", 
RowBox[{"Center", ",", "Automatic"}], "}"}]}], ",", 
RowBox[{"Dividers", "->", 
RowBox[{"{", 
RowBox[{
RowBox[{"{", 
RowBox[{"2", "->", 
RowBox[{"GrayLevel", "[", "0.7`", "]"}]}], "}"}], ",", 
RowBox[{"{", 
RowBox[{"2", "->", 
RowBox[{"GrayLevel", "[", "0.7`", "]"}]}], "}"}]}], "}"}]}], ",", 
RowBox[{"Rule", "[", 
RowBox[{"Spacings", ",", 
RowBox[{"{", 
RowBox[{
RowBox[{"{", 
RowBox[{"2", "->", "1"}], "}"}], ",", 
RowBox[{"{", 
RowBox[{"2", "->", "0.75`"}], "}"}]}], "}"}]}], "]"}]}], "]"}], ",", "\"\<DialogStyle\>\""}], "]"}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\)
  ]

ParamTableVisSine[fitmodel_] :=
 Module[{datalist, keys, header, vals},
  header = {"", "Value", "\[Sigma]"};
  keys = Keys[fitmodel["BestFitParameters"]];
  
  vals = MapIndexed[
    If[First[#2] == 2, sigFigs[Sin[Around[#1[[1]], #1[[2]]]]^2], 
      sigFigs[#1[[1]], #1[[2]]]] &, fitmodel["ParameterTableEntries"]];
  
  datalist = 
   Join[{header}, 
    Transpose[{keys, Transpose[vals][[1]], Transpose[vals][[2]]}]];
  
  \!\(\*
TagBox[
StyleBox[
RowBox[{"Style", "[", 
RowBox[{
RowBox[{"Grid", "[", 
RowBox[{"datalist", ",", 
RowBox[{"Alignment", "->", 
RowBox[{"{", 
RowBox[{"Center", ",", "Automatic"}], "}"}]}], ",", 
RowBox[{"Dividers", "->", 
RowBox[{"{", 
RowBox[{
RowBox[{"{", 
RowBox[{"2", "->", 
RowBox[{"GrayLevel", "[", "0.7`", "]"}]}], "}"}], ",", 
RowBox[{"{", 
RowBox[{"2", "->", 
RowBox[{"GrayLevel", "[", "0.7`", "]"}]}], "}"}]}], "}"}]}], ",", 
RowBox[{"Rule", "[", 
RowBox[{"Spacings", ",", 
RowBox[{"{", 
RowBox[{
RowBox[{"{", 
RowBox[{"2", "->", "1"}], "}"}], ",", 
RowBox[{"{", 
RowBox[{"2", "->", "0.75`"}], "}"}]}], "}"}]}], "]"}]}], "]"}], ",", "\"\<DialogStyle\>\""}], "]"}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\)
  ]

ShortParameterTable3[fitmodel_] :=
 Module[{datalist, keys, header, vals},
  header = {"", "Value", "\[Sigma]"};
  keys = Keys[fitmodel["BestFitParameters"]];
  
  vals = sigFigs[#[[1]], #[[2]]] & /@ 
    fitmodel["ParameterTableEntries"];
  vals = {#[[1]], sf["\[PlusMinus]``", #[[2]]]} & /@ vals;
  
  datalist = 
   Join[{header}, 
    Transpose[{keys, Transpose[vals][[1]], Transpose[vals][[2]]}]];
  
  \!\(\*
TagBox[
StyleBox[
RowBox[{"Style", "[", 
RowBox[{
RowBox[{"Grid", "[", 
RowBox[{"datalist", ",", 
RowBox[{"Alignment", "->", 
RowBox[{"{", 
RowBox[{"Center", ",", "Automatic"}], "}"}]}], ",", 
RowBox[{"Dividers", "->", 
RowBox[{"{", 
RowBox[{
RowBox[{"{", 
RowBox[{"2", "->", 
RowBox[{"GrayLevel", "[", "0.7`", "]"}]}], "}"}], ",", 
RowBox[{"{", 
RowBox[{"2", "->", 
RowBox[{"GrayLevel", "[", "0.7`", "]"}]}], "}"}]}], "}"}]}], ",", 
RowBox[{"Rule", "[", 
RowBox[{"Spacings", ",", 
RowBox[{"{", 
RowBox[{
RowBox[{"{", 
RowBox[{"2", "->", "1"}], "}"}], ",", 
RowBox[{"{", 
RowBox[{"2", "->", "0.75`"}], "}"}]}], "}"}]}], "]"}]}], "]"}], ",", "\"\<DialogStyle\>\""}], "]"}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\)
  ]

