(* print for variables when debugging *)
SetAttributes[printVariable, HoldFirst]
printVariable[variable_] := 
 Print[ToString[HoldForm[variable]] <> " = ", variable]


(* Reflects a number "x" across "a" (e.g. reflect[8,3] == -2) *)
reflect[x_, a_ : 0] := 2 a - x


(* Tests to see if x is a real number (works when Im[x] is essentially 0) *)
RealNumberQ[x_] := MatchQ[x, _?(NumberQ[#] && Chop[Im[#]] == 0 &)]

(* returns a list of all valid indicies from 1 - Length[lst] *)
lenRange[lst_] := Range[Length[lst]]


(* Shortcut for StringForm *)
sf[input__] := ToString[StringForm[input]]


(* Turns an association into a list of key value tuples
e.g. keyVals[<|1 -> a, 2 -> b, 3 -> c|>] == {{1, a}, {2, b}, {3, c}} *)
keyVals[assoc_] := {Keys[assoc], Values[assoc]} // Transpose


(* Remove leading and trailing zeros from a list *)
trimList[list_List] := 
 Drop[Drop[list, 
   First@FirstPosition[list, Except[0], Heads -> False] - 
    1], -(First@
      FirstPosition[Reverse[list], Except[0], Heads -> False] - 1)]

(* 
Return a rounded number with a given number of significant figures.
Resulting integers are returned as such.
Set 'ceiling' to True for uncertainties (which should always be rounded up)
*)
setSigFigs[number_, sigfigs_, ceiling_ : False] := 
 Module[{num = number, sigFigs = sigfigs, numlist, givenfigs, 
   rounded = 0, oldSigFigs, decPointPos, return},
  numlist = RealDigits[num];
  oldSigFigs = If[Length[numlist[[1]]] < 16,
    Length[trimList[numlist[[1]]]],
    Max[{Length[trimList[numlist[[1]]]], numlist[[2]]}]
    ];
  givenfigs = Min[{Length[numlist[[1]]], sigFigs}];
  decPointPos = numlist[[2]];
  
 (*If there are more digits than desired sig figs round off the last sig fig*)
  If[oldSigFigs > sigFigs,
   
   (*Round up if needed*)
   If[numlist[[1]][[sigFigs + 1]] >= 5 || ceiling,
     numlist[[1]][[sigFigs]]++;
     If[numlist[[1]][[sigFigs]] > 9 && sigFigs == 1,
      sigFigs++;
      decPointPos++;
      rounded++
      ];
     ];
   ];
  
  (*If there are enough digits for the desired sig figs round off the \
last sig fig*)
  If[oldSigFigs >= sigFigs || True,
   
   (*If the number is now an integer*)
   If[decPointPos >= sigFigs,
     (*Then*)
     numlist[[1]] = 
      Join[numlist[[1]][[;; givenfigs]], 
       Table[0, decPointPos - givenfigs]];
     return = FromDigits[numlist[[1]]]
     ,
     (*Else*)
     numlist[[1]] = numlist[[1]][[;; givenfigs]];
     return = 
      N[FromDigits[numlist[[1]]]*10^(
        decPointPos - givenfigs - rounded), sigFigs];
     ];
   ,
   (*Else*)
   Print["To many sig figs"];
   ];
  
  return
  ]

(* sigFigs rounds a number and its uncertainty to the appropriate number of significant figures *)
sigFigs[num_Around] := sigFigs[num["Value"], num["Uncertainty"]]

sigFigs[num_?NumericQ, unc_?NumericQ] :=
 Module[{uncertlist, uncertSigFigs, vallist, valSigFigs},
  vallist = RealDigits[num];
  uncertlist = RealDigits[unc];
  uncertSigFigs = If[uncertlist[[1, 1]] == 1, 2, 1];
  valSigFigs = vallist[[2]] - uncertlist[[2]] + uncertSigFigs;
  {Sign[num]*setSigFigs[num, valSigFigs], 
   setSigFigs[unc, uncertSigFigs, True]}
  ]



(* Returns true if 'num1' is divisible by 'num2' 
works with a mix of floats and integers *)
IsDivisible[num1_, num2_, tol_ : 10^-10] :=
 Module[{quotient}, quotient = num1/num2;
  Abs[Round[quotient] - quotient] < tol]



(* Returns a range of numbers from xMin to xMax where the points are 
distributed according to a Normal Distribution centered on xFocus *)
NormDistList[xMin_, xMax_, xFocus_, \[CapitalDelta]x_, \[CapitalDelta]xmin_, width_ : "half"] :=
  Module[{xi, dist, factor, w, x, left = {}, right = {}},
  (*Define a Normal Distribution centered on xFocus*)
  w = If[width == "half", (xMax - xMin)/4,(*else*)width];
  dist = PDF[NormalDistribution[xFocus, w], x] // N;
  factor = (1 - \[CapitalDelta]xmin/\[CapitalDelta]x)/dist /. 
    x -> xFocus;
  dist = (1 - factor dist);
  
  (* Generate left side *)
  xi = Min[Max[xFocus, xMin], xMax];
  While[xMin <= xi <= xMax,
   PrependTo[left, xi];
   xi -= \[CapitalDelta]x dist /. x -> xi;
   ];
  
  (* Generate right side *)
  xi = Min[Max[xFocus, xMin], xMax];
  While[xMin <= xi <= xMax,
   AppendTo[right, xi];
   xi += \[CapitalDelta]x dist /. x -> xi;
   ];
  
  (* Rescale to include xMin and xMax *)
  If[Length[left] > 1,
   left = Rescale[left, {left[[1]], left[[-1]]}, {xMin, left[[-1]]}];
   ];
  If[Length[right] > 1,
   right = 
     Rescale[right, {right[[1]], right[[-1]]}, {right[[1]], xMax}];
   ];
  
  Union[left, right]
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

(*Function to find intersection points of line and ellipse*)
findEllipseIntersection[point_, a_, b_, center_, r_ : 1] :=
 Module[{x, y, xc, yc, t, X, Y, tSolutions, intersectionPoints},
  (*Extract coordinates*)
  {x, y} = point;
  {xc, yc} = center;
  
  (*Parameterize the line*)
  X[t_] := xc + t (x - xc);
  Y[t_] := yc + t (y - yc);
  
  (*Solve for t using the ellipse equation*)
  tSolutions = Solve[((X[t] - xc)/a)^2 + ((Y[t] - yc)/b)^2 == r^2, t];
  
  (*Compute intersection points*)
  intersectionPoints = {X[#], Y[#]} & /@ (t /. tSolutions);
  intersectionPoints
  ]

Clear[distanceToEllipseIntersection]

distanceToEllipseIntersection[point_, ratio_, center_, side_,
  opts : OptionsPattern[{radius -> 1, threshold -> 1/100}]] :=
 distanceToEllipseIntersection[point, 1, ratio^-1, center, side, opts]

(*Function to compute distance to left or right intersection*)
distanceToEllipseIntersection[point_, a_, b_, center_, side_,
  opts : OptionsPattern[{radius -> 1, threshold -> 1/100}]] :=
 Module[{p, r, xc, yc, thrsh, intersectionPoints, selectedPoint},
  (*Extract ellipse center*)
  {xc, yc} = center;
  p = point;
  r = OptionValue[radius];
  
  (*If point is too close to center move it further away for finding \
intersection points*)
  thrsh = OptionValue[threshold];
  If[Abs[p[[1]] - xc] < a r thrsh,
   p[[1]] += Sign[p[[1]] - xc] a r thrsh];
  
  (*Find all intersection points*)
  intersectionPoints = findEllipseIntersection[p, a, b, center, r];
  
  (*Select left or right intersection*)
  selectedPoint = Switch[side,
    1, Select[intersectionPoints, #[[1]] >= xc &][[1]],
    -1, Select[intersectionPoints, #[[1]] <= xc &][[1]],
    "Right", Select[intersectionPoints, #[[1]] > xc &][[1]],(*x>xc*)
    "Left", Select[intersectionPoints, #[[1]] < xc &][[1]],(*x<xc*)
    "Top", Select[intersectionPoints, #[[2]] > yc &][[1]],(*y>yc*)
    "Bottom", Select[intersectionPoints, #[[2]] < yc &][[1]],(*y<yc*)_,
    Missing["InvalidSide"](*Invalid side*)
    ];
  
  (*Compute distance if a valid point was selected*)
  If[selectedPoint === Missing["InvalidSide"],
   selectedPoint(*Return error if side is invalid*),
   EuclideanDistance[point, selectedPoint] (*Compute distance*)]
  ]


(* Recursive function that returns the maximum depth of an object *)
maxDepth[obj_, curDepth_ : 0] := 
 If[Length[obj] > 0 && ! MatchQ[obj, _Around],
  Return[Max[maxDepth[#, curDepth + 1] & /@ obj]],
  Return[curDepth]]


(*
Attempts structure[[pos]] and returns the value if it exists and is a number
If it is not a number, or 'structure' doesn't have an element at 'pos', it returns 'else'
*)
safeAccessNum[structure_, pos_, else_ : Null] := Module[{element},
  element = Quiet@Check[structure[[Sequence @@ pos]], else];
  If[NumberQ[element],
   element,(*Return the element if it is a number*)
   else  
   ]
  ]

(*
Attempts structure[[pos]] and returns the value if it exists and is a string
If it is not a string, or 'structure' doesn't have an element at 'pos', it returns 'else'
*)

safeAccessStr[structure_, pos_, else_ : Null] := Module[{element},
  element = Quiet@Check[structure[[Sequence @@ pos]], else];
  If[StringQ[element],
   element,(*Return the element if it is a string*)
   else  
   ]
  ]

(* Combine the x and y data into a list of tuples data *)
cxy[data_List, xIndex_Integer, yIndex_Integer] := 
 Transpose[{data[[xIndex]], data[[yIndex]]}]

cxy[data_Association, xkey_, ykey_] := Module[{d1, d2},
  d1 = data[xkey];
  If[MissingQ[d1], d1 = data[[xkey]]];
  d2 = data[ykey];
  If[MissingQ[d2], d2 = data[[ykey]]];
  
  Check[Transpose[{d1, d2}], {Dimensions[d1], Dimensions[d2]}]
  ]

cxy[data_Association, xkey_, ykey_, yfactor_ : 1] := Module[{d1, d2},
  d1 = data[xkey];
  If[MissingQ[d1], d1 = data[[xkey]]];
  d2 = data[ykey];
  If[MissingQ[d2], d2 = data[[ykey]]];
  
  Check[Transpose[{d1, yfactor*d2}], {Dimensions[d1], Dimensions[d2]}]
  ]

cxy[data_Association, ykey_] := Module[{d1, d2},
  d1 = data[[1]];
  d2 = data[ykey];
  If[MissingQ[d2], d2 = data[[ykey]]];
  
  Check[Transpose[{d1, d2}], {Dimensions[d1], Dimensions[d2]}]
  ]



cxy[data_List, yIndex_Integer] := 
 Transpose[{data[[1]], data[[yIndex]]}]

cxy[data1_List, data2_List] := Transpose[{data1, data2}]

cxy[data1_List, index1_Integer, data2_, index2_Integer] := 
 Transpose[{data1[[index1]], data2[[index2]]}]
