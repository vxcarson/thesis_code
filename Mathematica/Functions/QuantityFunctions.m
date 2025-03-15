abbrev[unit_] := ToString[QuantityForm[unit, "Abbreviation"]]

(* Turns an association into a list of key value tuples
e.g. keyVals[<|1 -> a, 2 -> b, 3 -> c|>] == {{1, a}, {2, b}, {3, c}} *)
keyVals[assoc_] := {Keys[assoc], Values[assoc]} // Transpose


commonSIPrefixes = {"Atto", "Femto", "Pico", "Nano", "Micro", "Milli",
    "Centi", "", "Kilo", "Mega", "Giga"};

lessCommonSIPrefixes = {"Yocto", "Zepto", "Deci", "Deka", "Hecto", 
   "Tera", "Peta", "Exa", "Zetta", "Yotta"};

siPrefixes = {"Yocto", "Zepto", "Atto", "Femto", "Pico", "Nano", 
   "Micro", "Milli", "Centi", "Deci", "", "Deka", "Hecto", "Kilo", 
   "Mega", "Giga", "Tera", "Peta", "Exa", "Zetta", "Yotta"};

metricUnits =
  {"Meters", "Liters", "Grams", "Seconds", "Joules", "Watts", 
   "Newtons", "Pascals", "Kelvins", "Coulombs", "Volts", "Amperes", 
   "Ohms", "Farads", "Henries", "Candelas", "Moles", "Hertz"};

metricUnitsAbbrev = Table[
     prefix <> If[prefix != "", ToLowerCase[metric], metric] ->
      abbrev[prefix] <> abbrev[metric],
     {metric, metricUnits}, {prefix, commonSIPrefixes}
     ] // Flatten // Association;

AppendTo[metricUnitsAbbrev, #] & /@ {("Meters")^2 -> "m2", (
    "Meters")^3 -> "m3", "Meters"/"Second" -> "mps", 
   "Meters"/("Second")^2 -> "mps2"};

siUnits =
  {(*Length*)"Meters",
   (*Area*)("Meters")^2,
   (*Volume*)("Meters")^3, "Liters",
   (*Mass*)"Kilograms",
   (*Time*)"Seconds",
   (*Speed*)"Meters"/"Second",
   (*Acceleration*)"Meters"/("Second")^2,
   (*Energy*)"Joules",
   (*Power*)"Watts",
   (*Force*)"Newtons",
   (*Pressure*)"Pascals",
   (*Temperature*)"Kelvin",
   (*Charge*)"Coulombs",
   (*Voltage*)"Volts",
   (*Current*)"Amperes",
   (*Resistance*)"Ohms",
   (*Capacitance*)"Farads",
   (*Inductance*)"Henries",
   (*Luminous Intensity*)"Candelas",
   (*Amount of Substance*)"Moles",
   (*Frequency*)"Hertz"};

lessCommonUnits = {"Hectares", "Grains", "FootPounds", "PoundForces", 
   "Dynes", "Rankine"};

commonUnits =
  {(*Length*)"Inches", "Feet", "Yards", "Miles", "NauticalMiles", 
   "AstronomicalUnit", "LightYears", "Parsecs",
   (*Area*)"Inches"^2, "Feet"^2, "Yards"^2, "Acres",
   (*Volume*)"Inches"^3, "Feet"^3, "Yards"^3, "Teaspoons", 
   "Tablespoons", "FluidOunces", "Cups", "Pints", "Quarts", "Gallons",
    "Liters",
   (*Mass*)"Ounces", "Pounds", "MetricTons", "Stones",
   (*Time*)"Seconds", "Minutes", "Hours", "Days", "Weeks", "Months", 
   "Years",
   (*Speed*)"Miles"/"Hour", "Kilometers"/"Hour", "Knots",
   (*Energy*)"Joules", "Calories", "Kilocalories", "Electronvolts", 
   "BritishThermalUnits",
   (*Pressure*)"Atmospheres", "Bars", "Pascals", "PSI", "Torr", 
   "MillimetersOfMercury",
   (*Temperature*)"Celsius", "Fahrenheit",
   (*Force*)"Newtons",
   (*Power*)"Watts",
   (*Misc*)"Kilowatts"/"Hours"};

commonUnitsAbbrev = <|"Inches" -> "in", "Feet" -> "ft", 
   "Yards" -> "yd", "Miles" -> "mi", "NauticalMiles" -> "nmi", 
   "AstronomicalUnit" -> "au", "LightYears" -> "ly", 
   "Parsecs" -> "pc", ("Inches")^2 -> "in2", ("Feet")^2 -> "ft2", (
    "Yards")^2 -> "yd2", 
   "Acres" -> "acres", ("Inches")^3 -> "in3", ("Feet")^3 -> "ft3", (
    "Yards")^3 -> "yd3", ("Centimeters")^3 -> "cc", 
   "Teaspoons" -> "tsp", "Tablespoons" -> "tbsp", 
   "FluidOunces" -> "floz", "Cups" -> "c", "Pints" -> "pt", 
   "Quarts" -> "qt", "Gallons" -> "gal", "Liters" -> "L", 
   "Ounces" -> "oz", "Pounds" -> "lb", "MetricTons" -> "t", 
   "Stones" -> "stone", "Seconds" -> "s", "Minutes" -> "min", 
   "Hours" -> "h", "Days" -> "days", "Weeks" -> "wk", 
   "Months" -> "mo", "Years" -> "yr", "Miles"/"Hour" -> "mph", 
   "Kilometers"/"Hour" -> "kmph", "Knots" -> "kn", "Joules" -> "J", 
   "Calories" -> "cal", "Kilocalories" -> "kcal", 
   "Electronvolts" -> "eV", "BritishThermalUnits" -> "btu", 
   "Atmospheres" -> "atm", "Bars" -> "bar", "Pascals" -> "Pa", 
   "PSI" -> "psi", "Torr" -> "torr", "MillimetersOfMercury" -> "mmHg",
    "Celsius" -> "degC", "Fahrenheit" -> "degF", "Newtons" -> "N", 
   "Watts" -> "W", "Kilowatts"/"Hours" -> "kWh"|>;


(*Function to dynamically define shortcut conversion functions for \
each unit*)
Clear[createQuantityShortCutFunctions]
createQuantityShortCutFunctions[unit_, abbrev_?StringQ] :=
  Module[{symMag, symQ},
   (*Define function names dynamically*)
   symMag = Symbol[abbrev <> "Mag"];
   symQ = Symbol[abbrev <> "Q"];
   (*Define the functions*)
   symMag[quantity_?QuantityQ] := QuantityMagnitude[quantity, unit];
   symMag[quantity_?(! QuantityQ[#] &)] := quantity;
   symQ[quantity_?QuantityQ] := UnitConvert[quantity, unit];
   symQ[quantity_?(! QuantityQ[#] &)] := Quantity[quantity, unit];
   symQ[] := Quantity[1, unit];
   ];

(*Apply the function generator to all units in the associations*)
Scan[createQuantityShortCutFunctions[#[[1]], #[[2]]] &, 
  metricUnitsAbbrev // keyVals];
Scan[createQuantityShortCutFunctions[#[[1]], #[[2]]] &, 
  commonUnitsAbbrev // keyVals];