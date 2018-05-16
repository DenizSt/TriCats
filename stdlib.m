<|"stdlib" -> <|"description" -> "\!\(\*StyleBox[\"stdlib\",FontSlant->\"Ital\
ic\"]\) is the standard library for computations with trivalent diagrams. It \
comes with the TriCats package and contains lists of often-used diagrams and \
relations.", "Cap" -> <|"description" -> "\!\(\*StyleBox[\"Cap\",FontSlant->\
\"Italic\"]\) gives the cup diagram, which corresponds to the evaluation \
map.", "value" -> Diagram[{{0, 1}, {1, 0}}, {1, 2}, {}]|>, 
   "Cup" -> <|"description" -> "\!\(\*StyleBox[\"Cup\",FontSlant->\"Italic\"]\
\) gives the cup diagram, which corresponds to the coevaluation map.", 
     "value" -> Diagram[{{0, 1}, {1, 0}}, {}, {1, 2}]|>, 
   "Line" -> <|"description" -> "\!\(\*StyleBox[\"Line\",FontSlant->\"Italic\
\"]\) gives a line with one ingoing and one outgoing leg.", 
     "value" -> Diagram[{{0, 1}, {1, 0}}, {1}, {2}]|>, 
   "Lollipop" -> <|"description" -> "\!\(\*StyleBox[\"Lollipop\",FontSlant->\
\"Italic\"]\) gives the lollipop diagram with one leg up, which is an element \
of \!\(\*Cell[TextData[Cell[BoxData[FormBox[\nSubscriptBox[\"C\", \"0\"], \
TraditionalForm]]]]]\) and evaluates to zero.", 
     "value" -> Diagram[{{1, 1}, {1, 0}}, {}, {2}]|>, 
   "Trivalent" -> <|"description" -> "\!\(\*StyleBox[\"Trivalent\",FontSlant-\
>\"Italic\"]\) gives the trivalent vertex with all legs up.", 
     "value" -> Diagram[{{0, 0, 0, 1}, {0, 0, 0, 1}, {0, 0, 0, 1}, 
        {1, 1, 1, 0}}, {}, {1, 2, 3}]|>, "TrivalentBranch" -> 
    <|"description" -> "\!\(\*StyleBox[\"TrivalentBranch\",FontSlant->\"Itali\
c\"]\) gives the trivalent vertex with one ingoing and two outgoing legs.", 
     "value" -> Diagram[{{0, 0, 0, 1}, {0, 0, 0, 1}, {0, 0, 0, 1}, 
        {1, 1, 1, 0}}, {1}, {2, 3}]|>, "C4Atoms" -> 
    <|"description" -> "\!\(\*StyleBox[\"C4Atoms\",FontSlant->\"Italic\"]\) \
contains the diagrams in C4 with zero or one face, in the order specified in \
Section 4 of the paper on trivalent categories.", 
     "value" -> {Diagram[{{0, 0, 0, 1}, {0, 0, 1, 0}, {0, 1, 0, 0}, 
         {1, 0, 0, 0}}, {}, {1, 2, 3, 4}], 
       Diagram[{{0, 1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, 1, 0}}, {}, 
        {1, 2, 3, 4}], Diagram[{{0, 0, 0, 0, 1, 0}, {0, 0, 0, 0, 1, 0}, 
         {0, 0, 0, 0, 0, 1}, {0, 0, 0, 0, 0, 1}, {1, 1, 0, 0, 0, 1}, 
         {0, 0, 1, 1, 1, 0}}, {}, {1, 2, 3, 4}], 
       Diagram[{{0, 0, 0, 0, 0, 1}, {0, 0, 0, 0, 1, 0}, {0, 0, 0, 0, 1, 0}, 
         {0, 0, 0, 0, 0, 1}, {0, 1, 1, 0, 0, 1}, {1, 0, 0, 1, 1, 0}}, {}, 
        {1, 2, 3, 4}], Diagram[{{0, 0, 0, 0, 1, 0, 0, 0}, 
         {0, 0, 0, 0, 0, 1, 0, 0}, {0, 0, 0, 0, 0, 0, 1, 0}, 
         {0, 0, 0, 0, 0, 0, 0, 1}, {1, 0, 0, 0, 0, 1, 0, 1}, 
         {0, 1, 0, 0, 1, 0, 1, 0}, {0, 0, 1, 0, 0, 1, 0, 1}, 
         {0, 0, 0, 1, 1, 0, 1, 0}}, {}, {1, 2, 3, 4}]}|>, 
   "C4Basis" -> <|"description" -> "\!\(\*StyleBox[\"C4Basis\",FontSlant->\"I\
talic\"]\) gives the appropriate list of diagrams that form a basis of \
\!\(\*Cell[TextData[Cell[BoxData[FormBox[\nSubscriptBox[\"C\", \"4\"], \
TraditionalForm]]]]]\) when \
\!\(\*Cell[TextData[Cell[BoxData[FormBox[\nSubscriptBox[\"C\", \"4\"], \
TraditionalForm]]]]]\) has the dimension specified by the option dimC4.", 
     "attributes" -> {"releasehold"}, "value" -> Hold[#1[[1 ;; dimC4]] & ][
       {Diagram[{{0, 0, 0, 1}, {0, 0, 1, 0}, {0, 1, 0, 0}, {1, 0, 0, 0}}, {}, 
         {1, 2, 3, 4}], Diagram[{{0, 1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, 
          {0, 0, 1, 0}}, {}, {1, 2, 3, 4}], Diagram[{{0, 0, 0, 0, 1, 0}, 
          {0, 0, 0, 0, 1, 0}, {0, 0, 0, 0, 0, 1}, {0, 0, 0, 0, 0, 1}, 
          {1, 1, 0, 0, 0, 1}, {0, 0, 1, 1, 1, 0}}, {}, {1, 2, 3, 4}], 
        Diagram[{{0, 0, 0, 0, 0, 1}, {0, 0, 0, 0, 1, 0}, {0, 0, 0, 0, 1, 0}, 
          {0, 0, 0, 0, 0, 1}, {0, 1, 1, 0, 0, 1}, {1, 0, 0, 1, 1, 0}}, {}, 
         {1, 2, 3, 4}], Diagram[{{0, 0, 0, 0, 1, 0, 0, 0}, 
          {0, 0, 0, 0, 0, 1, 0, 0}, {0, 0, 0, 0, 0, 0, 1, 0}, 
          {0, 0, 0, 0, 0, 0, 0, 1}, {1, 0, 0, 0, 0, 1, 0, 1}, 
          {0, 1, 0, 0, 1, 0, 1, 0}, {0, 0, 1, 0, 0, 1, 0, 1}, 
          {0, 0, 0, 1, 1, 0, 1, 0}}, {}, {1, 2, 3, 4}]}]|>, 
   "PSO3Cond" -> <|"description" -> "\!\(\*StyleBox[\"PSO3Cond\",FontSlant->\
\"Italic\"]\) gives the equation for d, b, and t that singles out \
\!\(\*Cell[TextData[Cell[BoxData[FormBox[\nSubscriptBox[RowBox[{\"SO\", \
\"(\", \"3\", \")\"}], \"q\"], \n  TraditionalForm]]]]]\).", 
     "value" -> -2*b + b*d + t - d*t == 0|>, 
   "RealCond" -> <|"description" -> "\!\(\*StyleBox[\"RealCond\",FontSlant->\
\"Italic\"]\) gives the condition that d, b, and t are real.", 
     "value" -> Element[d, Reals] && Element[b, Reals] && 
       Element[t, Reals]|>, "SquareCoefficients" -> 
    <|"description" -> "\!\(\*StyleBox[\"SquareCoefficients\",FontSlant->\"It\
alic\"]\) gives the coefficients of the square in the standard basis \
depending on the dimension of \
\!\(\*Cell[TextData[Cell[BoxData[FormBox[\nSubscriptBox[\"C\", \"4\"], \
TraditionalForm]]]]]\) specified by the option dimC4.", 
     "attributes" -> {"releasehold"}, "value" -> Hold[#1[dimC4] & ][
       <|2 -> {b^2/(1 + d), b^2/(1 + d)}, 
        3 -> {(b^2 - b^2*d + d*t^2)/(1 + d - d^2), (b^2*(-2 + d) + t^2)/
           (-1 - d + d^2), ((-1 + d)*(-b^2 + (1 + d)*t^2))/
           (b*(-1 - d + d^2))}, 4 -> {(b*(b^2 + b*t - t^2))/(b*d + t + d*t), 
          (b*(b^2 + b*t - t^2))/(b*d + t + d*t), (-b^2 + (1 + d)*t^2)/
           (b*d + t + d*t), (-b^2 + (1 + d)*t^2)/(b*d + t + d*t)}|>]|>, 
   "C4Gramian" -> <|"description" -> "\!\(\*StyleBox[\"C4Gramian\",FontSlant-\
>\"Italic\"]\) gives the Gram matrix for the four diagrams in \
\!\(\*Cell[TextData[Cell[BoxData[FormBox[\nSubscriptBox[\"C\", \"4\"], \
TraditionalForm]]]]]\) with no faces.", "value" -> {{d^2, d, b*d, 0}, 
       {d, d^2, 0, b*d}, {b*d, 0, b^2*d, b*d*t}, {0, b*d, b*d*t, b^2*d}}|>, 
   "C4ScalarProducts" -> <|"description" -> "C4ScalarProducts gives the \
scalar products of all simple diagrams with zero or one face, where the \
appropriate relations for the square are used depending on the dimension of \
\!\(\*Cell[TextData[Cell[BoxData[FormBox[\nSubscriptBox[\"C\", \"4\"], \
TraditionalForm]]]]]\) specified by the option dimC4.", 
     "attributes" -> {"releasehold"}, "value" -> Hold[#1[dimC4] & ][
       <|2 -> {{d^2, d, b*d, 0, b^2*d}, {d, d^2, 0, b*d, b^2*d}, 
          {b*d, 0, b^2*d, b*d*t, b*d*t^2}, {0, b*d, b*d*t, b^2*d, b*d*t^2}, 
          {b^2*d, b^2*d, b*d*t^2, b*d*t^2, (2*b^4*d)/(1 + d)}}, 
        3 -> {{d^2, d, b*d, 0, b^2*d}, {d, d^2, 0, b*d, b^2*d}, 
          {b*d, 0, b^2*d, b*d*t, b*d*t^2}, {0, b*d, b*d*t, b^2*d, b*d*t^2}, 
          {b^2*d, b^2*d, b*d*t^2, b*d*t^2, 
           (d*(b^4*(-3 + 2*d) - 2*b^2*(-1 + d)*t^2 + (-1 + d^2)*t^4))/
            (-1 - d + d^2)}}, 4 -> {{d^2, d, b*d, 0, b^2*d}, 
          {d, d^2, 0, b*d, b^2*d}, {b*d, 0, b^2*d, b*d*t, b*d*t^2}, 
          {0, b*d, b*d*t, b^2*d, b*d*t^2}, {b^2*d, b^2*d, b*d*t^2, b*d*t^2, 
           (2*b*d*(b^4 + b^3*t - 2*b^2*t^2 + (1 + d)*t^4))/
            (b*d + t + d*t)}}|>]|>, "C4ONBCoefficients" -> 
    <|"description" -> "\!\(\*StyleBox[\"C4ONBCoefficients\",FontSlant->\"Ita\
lic\"]\) gives the coefficients of the orthonormal basis of \
\!\(\*Cell[TextData[Cell[BoxData[FormBox[\nSubscriptBox[\"C\", \"4\"], \
TraditionalForm]]]]]\) obtained by performing the Gram\[Dash]Schmidt \
algorithm on the standard basis. The dimension of \
\!\(\*Cell[TextData[Cell[BoxData[FormBox[\nSubscriptBox[\"C\", \"4\"], \
TraditionalForm]]]]]\) can be specified by the option dimC4.", 
     "attributes" -> {"releasehold"}, "value" -> 
      Hold[#1[[1 ;; dimC4,1 ;; dimC4]] & ][{{Abs[d]^(-1), 0, 0, 0}, 
        {-(1/(d*Sqrt[-1 + d^2])), 1/Sqrt[-1 + d^2], 0, 0}, 
        {(b*Sqrt[(d*(-1 - d + d^2))/(-1 + d^2)])/((1 + d - d^2)*Abs[b]), 
         b/((-1 + d^2)*Sqrt[(d*(-1 - d + d^2))/(-1 + d^2)]*Abs[b]), 
         1/Sqrt[(b^2*d*(-1 - d + d^2))/(-1 + d^2)], 0}, 
        {(b + d*t)/((-1 - d + d^2)*Sqrt[(d*(b^2*(-2 + d)*d - 2*b*t - 
               (-1 + d^2)*t^2))/(-1 - d + d^2)]), (b - b*d - t)/
          ((-1 - d + d^2)*Sqrt[(d*(b^2*(-2 + d)*d - 2*b*t - (-1 + d^2)*t^2))/
             (-1 - d + d^2)]), (-b + t - d^2*t)/(b*(-1 - d + d^2)*
           Sqrt[(d*(b^2*(-2 + d)*d - 2*b*t - (-1 + d^2)*t^2))/
             (-1 - d + d^2)]), 1/Sqrt[(d*(b^2*(-2 + d)*d - 2*b*t - 
              (-1 + d^2)*t^2))/(-1 - d + d^2)]}}]|>, 
   "C4ONB" -> <|"description" -> "\!\(\*StyleBox[\"C4ONB\",FontSlant->\"Itali\
c\"]\) gives the diagrams of the orthonormal basis of \
\!\(\*Cell[TextData[Cell[BoxData[FormBox[\nSubscriptBox[\"C\", \"4\"], \
TraditionalForm]]]]]\) obtained by performing the Gram\[Dash]Schmidt \
algorithm on the standard basis. The dimension of \
\!\(\*Cell[TextData[Cell[BoxData[FormBox[\nSubscriptBox[\"C\", \"4\"], \
TraditionalForm]]]]]\) can be specified by the option dimC4.", 
     "attributes" -> {"releasehold"}, "value" -> 
      Hold[#1[[1 ;; dimC4,1 ;; dimC4]] . #2[[1 ;; dimC4]] & ][
       {{Abs[d]^(-1), 0, 0, 0}, {-(1/(d*Sqrt[-1 + d^2])), 1/Sqrt[-1 + d^2], 
         0, 0}, {(b*Sqrt[(d*(-1 - d + d^2))/(-1 + d^2)])/
          ((1 + d - d^2)*Abs[b]), b/((-1 + d^2)*Sqrt[(d*(-1 - d + d^2))/
             (-1 + d^2)]*Abs[b]), 1/Sqrt[(b^2*d*(-1 - d + d^2))/(-1 + d^2)], 
         0}, {(b + d*t)/((-1 - d + d^2)*Sqrt[(d*(b^2*(-2 + d)*d - 2*b*t - 
               (-1 + d^2)*t^2))/(-1 - d + d^2)]), (b - b*d - t)/
          ((-1 - d + d^2)*Sqrt[(d*(b^2*(-2 + d)*d - 2*b*t - (-1 + d^2)*t^2))/
             (-1 - d + d^2)]), (-b + t - d^2*t)/(b*(-1 - d + d^2)*
           Sqrt[(d*(b^2*(-2 + d)*d - 2*b*t - (-1 + d^2)*t^2))/
             (-1 - d + d^2)]), 1/Sqrt[(d*(b^2*(-2 + d)*d - 2*b*t - 
              (-1 + d^2)*t^2))/(-1 - d + d^2)]}}, 
       {Diagram[{{0, 0, 0, 1}, {0, 0, 1, 0}, {0, 1, 0, 0}, {1, 0, 0, 0}}, {}, 
         {1, 2, 3, 4}], Diagram[{{0, 1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, 
          {0, 0, 1, 0}}, {}, {1, 2, 3, 4}], Diagram[{{0, 0, 0, 0, 1, 0}, 
          {0, 0, 0, 0, 1, 0}, {0, 0, 0, 0, 0, 1}, {0, 0, 0, 0, 0, 1}, 
          {1, 1, 0, 0, 0, 1}, {0, 0, 1, 1, 1, 0}}, {}, {1, 2, 3, 4}], 
        Diagram[{{0, 0, 0, 0, 0, 1}, {0, 0, 0, 0, 1, 0}, {0, 0, 0, 0, 1, 0}, 
          {0, 0, 0, 0, 0, 1}, {0, 1, 1, 0, 0, 1}, {1, 0, 0, 1, 1, 0}}, {}, 
         {1, 2, 3, 4}], Diagram[{{0, 0, 0, 0, 1, 0, 0, 0}, 
          {0, 0, 0, 0, 0, 1, 0, 0}, {0, 0, 0, 0, 0, 0, 1, 0}, 
          {0, 0, 0, 0, 0, 0, 0, 1}, {1, 0, 0, 0, 0, 1, 0, 1}, 
          {0, 1, 0, 0, 1, 0, 1, 0}, {0, 0, 1, 0, 0, 1, 0, 1}, 
          {0, 0, 0, 1, 1, 0, 1, 0}}, {}, {1, 2, 3, 4}]}]|>, 
   "SO3qStandardBraiding" -> <|"description" -> "\!\(\*StyleBox[\"SO3qStandar\
dBraiding\",FontSlant->\"Italic\"]\) gives the standard braiding for \
\!\(\*Cell[TextData[Cell[BoxData[FormBox[\nSubscriptBox[RowBox[{\"SO\", \
\"(\", \"3\", \")\"}], \"q\"], \n  TraditionalForm]]]]]\) as defined in the \
paper on trivalent categories.", "value" -> 
      (-1 + q^2)*Diagram[{{0, 0, 0, 1}, {0, 0, 1, 0}, {0, 1, 0, 0}, 
          {1, 0, 0, 0}}, {}, {1, 2, 3, 4}] + 
       Diagram[{{0, 1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, 1, 0}}, {}, 
         {1, 2, 3, 4}]/q^2 + (-q^(-2) - q^2)*Diagram[{{0, 0, 0, 0, 0, 1}, 
          {0, 0, 0, 0, 1, 0}, {0, 0, 0, 0, 1, 0}, {0, 0, 0, 0, 0, 1}, 
          {0, 1, 1, 0, 0, 1}, {1, 0, 0, 1, 1, 0}}, {}, {1, 2, 3, 4}]|>, 
   "dqRel" -> <|"description" -> "\!\(\*StyleBox[\"dqRel\",FontSlant->\"Itali\
c\"]\) gives the relation between \!\(\*Cell[\"d\"]\) and \!\(\*Cell[\"q\"]\) \
for \!\(\*Cell[TextData[Cell[BoxData[FormBox[\nSubscriptBox[RowBox[{\"SO\", \
\"(\", \"3\", \")\"}], \"q\"], \n  TraditionalForm]]]]]\).", 
     "attributes" -> {"releasehold"}, "value" -> d -> 1 + q^(-2) + q^2|>, 
   "C5Atoms" -> <|"description" -> "\!\(\*StyleBox[\"C5Atoms\",FontSlant->\"I\
talic\"]\) contains the diagrams in C5 with zero or one face, in non-standard \
order to simplify the results of the Gram\[Dash]Schmidt algorithm.", 
     "value" -> {Diagram[{{0, 1, 0, 0, 0, 0}, {1, 0, 0, 0, 0, 0}, 
         {0, 0, 0, 0, 0, 1}, {0, 0, 0, 0, 0, 1}, {0, 0, 0, 0, 0, 1}, 
         {0, 0, 1, 1, 1, 0}}, {}, {1, 2, 3, 4, 5}], 
       Diagram[{{0, 0, 0, 0, 0, 1}, {0, 0, 0, 0, 0, 1}, {0, 0, 0, 0, 0, 1}, 
         {0, 0, 0, 0, 1, 0}, {0, 0, 0, 1, 0, 0}, {1, 1, 1, 0, 0, 0}}, {}, 
        {1, 2, 3, 4, 5}], Diagram[{{0, 0, 0, 0, 0, 1, 0, 0}, 
         {0, 0, 0, 0, 0, 1, 0, 0}, {0, 0, 0, 0, 0, 0, 1, 0}, 
         {0, 0, 0, 0, 0, 0, 0, 1}, {0, 0, 0, 0, 0, 0, 0, 1}, 
         {1, 1, 0, 0, 0, 0, 1, 0}, {0, 0, 1, 0, 0, 1, 0, 1}, 
         {0, 0, 0, 1, 1, 0, 1, 0}}, {}, {1, 2, 3, 4, 5}], 
       Diagram[{{0, 0, 0, 0, 0, 1}, {0, 0, 0, 0, 0, 1}, {0, 0, 0, 1, 0, 0}, 
         {0, 0, 1, 0, 0, 0}, {0, 0, 0, 0, 0, 1}, {1, 1, 0, 0, 1, 0}}, {}, 
        {1, 2, 3, 4, 5}], Diagram[{{0, 0, 0, 0, 0, 1, 0, 0}, 
         {0, 0, 0, 0, 0, 1, 0, 0}, {0, 0, 0, 0, 0, 0, 1, 0}, 
         {0, 0, 0, 0, 0, 0, 1, 0}, {0, 0, 0, 0, 0, 0, 0, 1}, 
         {1, 1, 0, 0, 0, 0, 0, 1}, {0, 0, 1, 1, 0, 0, 0, 1}, 
         {0, 0, 0, 0, 1, 1, 1, 0}}, {}, {1, 2, 3, 4, 5}], 
       Diagram[{{0, 0, 0, 0, 0, 1}, {0, 0, 1, 0, 0, 0}, {0, 1, 0, 0, 0, 0}, 
         {0, 0, 0, 0, 0, 1}, {0, 0, 0, 0, 0, 1}, {1, 0, 0, 1, 1, 0}}, {}, 
        {1, 2, 3, 4, 5}], Diagram[{{0, 0, 0, 0, 0, 0, 1, 0}, 
         {0, 0, 0, 0, 0, 1, 0, 0}, {0, 0, 0, 0, 0, 1, 0, 0}, 
         {0, 0, 0, 0, 0, 0, 0, 1}, {0, 0, 0, 0, 0, 0, 0, 1}, 
         {0, 1, 1, 0, 0, 0, 1, 0}, {1, 0, 0, 0, 0, 1, 0, 1}, 
         {0, 0, 0, 1, 1, 0, 1, 0}}, {}, {1, 2, 3, 4, 5}], 
       Diagram[{{0, 0, 0, 0, 0, 0, 0, 1}, {0, 0, 0, 0, 0, 1, 0, 0}, 
         {0, 0, 0, 0, 0, 1, 0, 0}, {0, 0, 0, 0, 0, 0, 1, 0}, 
         {0, 0, 0, 0, 0, 0, 0, 1}, {0, 1, 1, 0, 0, 0, 1, 0}, 
         {0, 0, 0, 1, 0, 1, 0, 1}, {1, 0, 0, 0, 1, 0, 1, 0}}, {}, 
        {1, 2, 3, 4, 5}], Diagram[{{0, 0, 0, 0, 1, 0}, {0, 0, 0, 0, 0, 1}, 
         {0, 0, 0, 0, 0, 1}, {0, 0, 0, 0, 0, 1}, {1, 0, 0, 0, 0, 0}, 
         {0, 1, 1, 1, 0, 0}}, {}, {1, 2, 3, 4, 5}], 
       Diagram[{{0, 0, 0, 0, 0, 0, 0, 1}, {0, 0, 0, 0, 0, 0, 1, 0}, 
         {0, 0, 0, 0, 0, 1, 0, 0}, {0, 0, 0, 0, 0, 1, 0, 0}, 
         {0, 0, 0, 0, 0, 0, 0, 1}, {0, 0, 1, 1, 0, 0, 1, 0}, 
         {0, 1, 0, 0, 0, 1, 0, 1}, {1, 0, 0, 0, 1, 0, 1, 0}}, {}, 
        {1, 2, 3, 4, 5}], Diagram[{{0, 0, 0, 0, 0, 1, 0, 0, 0, 0}, 
         {0, 0, 0, 0, 0, 0, 1, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 1, 0, 0}, 
         {0, 0, 0, 0, 0, 0, 0, 0, 1, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 1}, 
         {1, 0, 0, 0, 0, 0, 1, 0, 0, 1}, {0, 1, 0, 0, 0, 1, 0, 1, 0, 0}, 
         {0, 0, 1, 0, 0, 0, 1, 0, 1, 0}, {0, 0, 0, 1, 0, 0, 0, 1, 0, 1}, 
         {0, 0, 0, 0, 1, 1, 0, 0, 1, 0}}, {}, {1, 2, 3, 4, 5}]}|>|>|>
