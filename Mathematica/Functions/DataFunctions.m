(* 
Given a list of tuples (i.e. 'ranges'), this returns the index of the closest value (within 'ranges') to 'pos'
within a tolerance. 
If not within the tolerance for any value in 'ranges', the first listed tuple the 'pos' is between is returned, otherwise 0 is returned.
This was origonally used with handlers so one could click and drag the edge of an element (that represented a range on a plot) even if that edge was within the range of another element, with the other option of moving the entire range if you didn't click on an edge.
*)
minDistIndex[pos_, ranges_, tolerance_] := 
 Module[{index = 0, tol = tolerance, i = 1},
  MapIndexed[
   If[Abs[pos - #1] < tol,
     index = #2; tol = Abs[pos - #1];] &,
   ranges, {2}];
  
  While[index == 0 && i <= Length[ranges],
   If[Min[ranges[[i]]] < pos < Max[ranges[[i]]], index = {i}];
   i++;
   ];
  index
  ]

(*Function to sum all y-values whose x-value was within a range*)
sumPointsWithinRange[data_, range_] := 
  Total[Select[data, Min[range] <= #[[1]] <= Max[range] &][[All, 2]]];


(* Functions to access data *)
yValues[data_List] :=
 If[MatchQ[#, _Around], #["Value"], #] & /@ data[[All, 2]]
yUncerts[data_List] :=
 If[MatchQ[#, _Around], #["Uncertainty"], 0] & /@ data[[All, 2]]
xValues[data_List] :=
 If[MatchQ[#, _Around], #["Value"], #] & /@ data[[All, 1]]
xUncerts[data_List] :=
 If[MatchQ[#, _Around], #["Uncertainty"], 0] & /@ data[[All, 1]]
yMaxValues[data_List] := yValues[data] + yUncerts[data]
yMinValues[data_List] := yValues[data] - yUncerts[data]
xMaxValues[data_List] := xValues[data] + xUncerts[data]
xMinValues[data_List] := xValues[data] - xUncerts[data]
onlyValues[data_List] := Transpose[{xValues[data], yValues[data]}]
ys[data_List] := data[[All, 2]]
xs[data_List] := data[[All, 1]]


(* Functions to compute the maximum (and minimum) "Around" value \
based on its upper (lower) bound*)
maxAround[values_List] := MaximalBy[values, First[#] + Last[#] &][[1]]
minAround[values_List] := MinimalBy[values, First[#] - Last[#] &][[1]]
minMaxAround[values_List] := {minAround[values], maxAround[values]}


(* approximates the visibility of data *)
visMaxApprox[data_List] := 
 Quiet[Check[(maxAround[ys[data]] - minAround[ys[data]])/(
   maxAround[ys[data]] + minAround[ys[data]]), 
   maxAround[ys[data]] - minAround[ys[data]], Power::infy], 
  Power::infy]