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