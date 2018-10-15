(* The TriCats package by Deniz Stiegemann. *)
BeginPackage["TriCats`"]

Unprotect[Diagram,EnsureGraph,EnsureMatrix,b,d,dimC4,ReduceDiagram,ReduceSquares,t,ConnectAt,DiagramCompose,DiagramConjugate,DiagramFlipH,DiagramMoveDown,DiagramMoveUp,DiagramNorm,DiagramRotate,DiagramScalar,DiagramTensor,DiagramTensorPower,DiagramTrace,Bilinearize,ConjugateLinearize,Linearize,MakeGraphs,Sesquilinearize,ClearLibrary,Description,LoadLibrary,Retrieve,FindDiagramIsomorphisms,IsomorphicDiagramQ,Components,DistinctDiagrams];

Retrieve::usage=
"Retrieve[item,opts] gives the value of item in the current library.";
Description::usage=
"Description[item] gives the description of item in the library.";
LoadLibrary::usage=
"LoadLibrary[file] adds the contents of file to the library.";
ClearLibrary::usage=
"ClearLibrary[] deletes all entries from the library.";

EnsureGraph::usage=
"EnsureGraph[expr] replaces adjacency matrices with graphs, if necessary, in all diagrams occuring in expr.";
EnsureMatrix::usage=
"EnsureMatrix[expr] replaces graphs with adjacency matrices, if necessary, in all diagrams occuring in expr.";

Linearize::usage="Linearize[f] makes the function f linear, in its first argument, with respect to expressions with head Diagram. f can allow more than one argument.";
ConjugateLinearize::usage=
"ConjugateLinearize[f] makes the function f linear, in its first argument, with respect to expressions with head Diagram. f can allow more than one argument.";
Bilinearize::usage="Bilinearize[f] makes the function f linear, in its first argument, with respect to expressions with head Diagram. f can allow more than one argument.";
Sesquilinearize::usage="Sesquilinearize[f] makes the function f linear, in its first argument, with respect to expressions with head Diagram. f can allow more than one argument.";


d::usage="d represents the formal loop parameter of a trivalent category. It is also an option of functions such as ReduceDiagram, having the symbol d as its default value.";
b::usage="b represents the formal bigon parameter of a trivalent category. It is also an option of functions such as ReduceDiagram, having the symbol b as its default value.";
t::usage="t represents the formal triangle parameter of a trivalent category. It is also an option of functions such as ReduceDiagram, having the symbol t as its default value.";
ReduceSquares::usage="ReduceSquare is an option for ReduceDiagram that specifies whether squares should be reduced.";
dimC4::usage="dimC4 represents the dimension of Subscript[C, 4] of a trivalent category, the linear space of diagrams with fours external legs. It is an option of functions such as ReduceDiagram, where it is relevant for substituting squares. In this case, the default value is 4.";
Diagram::usage="Diagram[a,in,out] represents a diagram with adjacency matrix a, ingoing legs in, and outgoing legs out.";
ReduceDiagram::usage="ReduceDiagram[diagram, opts] reduces the diagram diagram. Possbile options are d, b, t, dimC4, ReduceSquares. ReduceDiagram is linear in diagram.";


DiagramTensor::usage="DiagramTensor[diagram1, diagram2] gives the tensor product of the diagrams diagram1 and diagram2. DiagramTensor is bilinear.";
DiagramTensorPower::usage=
"DiagramTensorPower[diagram, n] gives the n-th tensor power of diagram, and the empty diagram if n is zero."
ConnectAt::usage="ConnectAt[a1, a2, legs1, legs2] is a low-level function and gives the adjacency matrix obtained by connecting the legs legs1 of a1 to the legs legs2 of a2.";
DiagramCompose::usage="DiagramCompose[diagram1, diagram2] gives the diagram obtained from composing diagram1 and diagram2. DiagramCompose is bilinear.";
DiagramTrace::usage="DiagramTrace[diagram] gives the trace of diagram. DiagramTrace is linear.";
DiagramFlipH::usage=
"DiagramFlipH[diagram] gives the diagram diagram flipped horizontally, by exchanging the lists of in and out vertices.";
DiagramConjugate::usage=
"DiagramConjugate[diagram] gives the diagram diagram flipped horizontally, by exchanging the lists of in and out vertices. DiagramConjugate is conjugate-linear.";
DiagramRotate::usage="DiagramRotate[diagram] gives the diagram diagram rotated by 180 degrees, i.e. the lists for in and out legs are swapped and both reversed. DiagramRotate is linear.";
DiagramScalar::usage="DiagramScalar[diagram1, diagram2] gives the scalar product of diagram1 and diagram2. DiagramScalar is bilinear.";
DiagramNorm::usage=
"DiagramNorm[diagram] gives the norm of the diagram diagram.";
DiagramMoveUp::usage="DiagramMoveUp[diagram, n] takes the n rightmost in legs of diagram and makes them out legs in reverse order. DiagramMoveUp is linear.";
DiagramMoveDown::usage="DiagramMoveDown[diagram, n] takes the n rightmost out legs of diagram and makes them in legs in reverse order. DiagramMoveDown is linear.";
MakeGraphs::usage=
"MakeGraphs[expr] gives a list of all graphs corresponding to diagramcacency matrices occuring in expr.";

FindDiagramIsomorphisms::usage=
"FindDiagramIsomorphisms[diagram1, diagram2] finds all graph isomorphisms from diagram1 to diagram2 that correctly map open legs.";
IsomorphicDiagramQ::usage=
"IsomorphicDiagramQ[diagram1, diagram2] yields True if diagram1 and diagram2 are isomorphic, and False ortherwise.";
Components::usage=
"Components[expr,d iagrams] gives the coefficients that the diagrams specified in the list diagrams have in the linear combination expr of diagrams.";
DistinctDiagrams::usage=
"DistinctDiagrams[expr] gives a list of all the distinct diagrams appearing at any level in expr.";

Begin["`Private`"]

ParameterOptions={d->d,b->b,t->t,dimC4->4};
ReduceOptions=Append[ParameterOptions,ReduceSquares->True];
Set[Options@#,ReduceOptions]&/@
{SquareCoefficients,ReduceDiagram,DiagramNorm};

library=<||>;

LoadLibrary[libname_]:=(library=Join[library,Get@(libname<>".m")];);
LoadLibrary[libnames__]:=Scan[LoadLibrary,{libnames}];
ClearLibrary[]:=(library=<||>;);

GetEntry[key_]:=Module[
{k=key,parentlib,subkey},
{parentlib,subkey}=If[StringContainsQ[k,":"],
StringSplit[k,":"],
{SelectFirst[Keys@library,KeyExistsQ[library[#],k]&],k}
];
Return[library[parentlib,subkey]];
];

GetAttributes[key_]:=If[KeyExistsQ[#,"attributes"],#["attributes"],{}]&@GetEntry[key];

Retrieve[key_,opts___]:=Module[
{k=key,entry,value},
value=GetEntry[k]["value"];

If[MemberQ[GetAttributes[key],"releasehold"],
Return[ReleaseHold[value/.{opts}/.dimC4->4]];
];
Return[value/.{opts}/.dimC4->4];
];

Description[key_]:=If[KeyExistsQ[library,key],library[key,"description"],GetEntry[key]["description"]];

SetAttributes[EnsureGraph,Listable];
EnsureGraph[expr_]:=expr/.x_Diagram:>EnsureGraph[x];
EnsureGraph[diagram_Diagram]:=If[MatchQ[First@diagram,_List],Prepend[Rest@diagram,AdjacencyGraph[First@diagram,VertexShapeFunction->"Name"]],diagram];

SetAttributes[EnsureMatrix,Listable];
EnsureMatrix[expr_]:=expr/.x_Diagram:>EnsureGraph[x];
EnsureMatrix[diagram_Diagram]:=If[MatchQ[First@diagram,_Graph],Prepend[Rest@diagram,Normal@AdjacencyMatrix@First@diagram],diagram];

Linearize[f_]:=(
f[x_+y_,args___]:=f[x,args]+f[y,args];
f[c_*diagram_Diagram,args___]:=c*f[diagram,args];
f[c_*x_/;FreeQ[c,_Diagram],args___]:=c*f[x,args];
);

ConjugateLinearize[f_]:=(
f[x_+y_,args___]:=f[x,args]+f[y,args];
f[c_*diagram_Diagram,args___]:=Conjugate@c*f[diagram,args];
f[c_*x_/;FreeQ[c,_Diagram],args___]:=Conjugate@c*f[x,args];
);

Bilinearize[f_]:=(
f[c_*diagram_Diagram,z_]:=c*f[diagram,z];
f[x_,c_*diagram_Diagram]:=c*f[x,diagram];
f[c_*x_/;FreeQ[c,_Diagram],z_]:=c*f[x,z];
f[x_,c_*z_/;FreeQ[c,_Diagram]]:=c*f[x,z];
f[x_+y_,z_]:=f[x,z]+f[y,z];
f[x_,y_+z_]:=f[x,y]+f[x,z];
);

Sesquilinearize[f_]:=(
f[c_*diagram_Diagram,z_]:=Conjugate@c*f[diagram,z];
f[x_,c_*diagram_Diagram]:=c*f[x,diagram];
f[c_*x_/;FreeQ[c,_Diagram],z_]:=Conjugate@c*f[x,z];
f[x_,c_*z_/;FreeQ[c,_Diagram]]:=c*f[x,z];
f[x_+y_,z_]:=f[x,z]+f[y,z];
f[x_,y_+z_]:=f[x,y]+f[x,z];
);

Linearize/@{DiagramTrace,DiagramMoveUp,DiagramMoveDown};ConjugateLinearize/@{DiagramConjugate};
Bilinearize/@{DiagramTensor,DiagramCompose};

(**********)

SetAttributes[DeleteRowCol,HoldFirst];
DeleteRowCol[list_,indices_]:=
Do[
list=Delete[list,i];
list=Drop[list,None,{i}];
,{i,ReverseSort@indices}];

internalC4Atoms=(Diagram[#1,{},{2,3,4,1}]&)/@{{{0,1,0,0},{1,0,0,0},{0,0,0,1},{0,0,1,0}},{{0,0,0,1},{0,0,1,0},{0,1,0,0},{1,0,0,0}},{{0,0,0,0,0,1},{0,0,0,0,1,0},{0,0,0,0,1,0},{0,0,0,0,0,1},{0,1,1,0,0,1},{1,0,0,1,1,0}},{{0,0,0,0,1,0},{0,0,0,0,1,0},{0,0,0,0,0,1},{0,0,0,0,0,1},{1,1,0,0,0,1},{0,0,1,1,1,0}}};

internalEmptyDiagram=Diagram[{{0}}];

SquareCoefficients[opts:OptionsPattern[]]:=
Switch[OptionValue@dimC4,
2,{b^2/(1+d),b^2/(1+d)},
3,{(b^2-b^2 d+d t^2)/(1+d-d^2),(b^2 (-2+d)+t^2)/(-1-d+d^2),((-1+d) (-b^2+(1+d) t^2))/(b (-1-d+d^2))},
4,{(b (b^2+b t-t^2))/(b d+t+d t),(b (b^2+b t-t^2))/(b d+t+d t),(-b^2+(1+d) t^2)/(b d+t+d t),(-b^2+(1+d) t^2)/(b d+t+d t)}
]/.{d->OptionValue@d,b->OptionValue@b,t->OptionValue@t};

GetNewLegIndices[adj_List,inlst_,outlst_]:=Module[{
noflegs,a=adj,i,in=inlst,out=outlst,oldlegs,oldlegslist,newlegs={},counter=1,result
},
oldlegs=Join[in,out];
noflegs=Length@oldlegs;
oldlegslist=Transpose@{oldlegs,Range@noflegs};
oldlegslist=Sort[oldlegslist,First@#1<First@#2&];
For[i=1,i<=Length[a],i++,
If[Plus@@a[[i]]==1&&a[[i,i]]==0,
AppendTo[newlegs,i];
counter++;
]; (*if*)
];(*for*)
newlegs=Sort@newlegs;
result=Table[newlegs[[oldlegslist[[i,2]]]],{i,1,noflegs}];
Return[TakeDrop[result,Length@in]];
];(*module*)

ReduceDiagram[diagram1_Diagram,opts:OptionsPattern[]]:=Module[{
diagram,
a,(* adjacency matrix*)
numberoflegs,
in,out,newin,newout,
result,secondround=False,
i,j,noe,
dp=0,bp=0,tp=0, (* powers of d, b, t *)
current,(* current vertex in the loop *)
degree,(* degree of current vertex *)
neighbours,(* other neighbours of current vertex, excluding current vertex, to which it is connected by a *single* edge *)
selfconnected, (* whether the current vertex is connected to itself *)
bigon,bigonneighbour,bigonloop,(* a close neighbour is one to which it is connected by 2 edges *)
bigonneighboursotherneighbour,
triangle,triangleneighbours,othertriangleneighbour,
square,squareneighbours,othersquarecorner,squarevertices,neighboursofsquare,sqcoeff,newa,newdiagram,
idxlst={{1,2},{1,3},{2,3}}
},
diagram=EnsureMatrix@diagram1;
a=First@diagram;

(* In the first round, remove all 2-valent vertices. *)
current=Length[a]; 
While[current>0, (**** 2-valent loop ****)
degree=0;
selfconnected=False;
neighbours={};
For[i=1,i<=Length[a],i++,
noe=a[[current,i]];
degree+=a[[current,i]];
Switch[noe,
1,
	AppendTo[neighbours,i];
	If[i==current,selfconnected=True];,
2,
	AppendTo[neighbours,i];
];(*switch*)
];(*for*)
If[degree==0,
DeleteRowCol[a,{current}];
current--;
Continue[];
];
If[selfconnected,degree++];
If[degree==2,
If[selfconnected,
DeleteRowCol[a,{current}];
dp++;
current--;
Continue[];
];
Switch[Length[neighbours],
1,
	a[[First@neighbours,First@neighbours]]+=1;
	DeleteRowCol[a,{current}];
	current--;
	Continue[];,
2,
	a[[First@neighbours,Last@neighbours]]+=1;
	a[[Last@neighbours,First@neighbours]]+=1;
	DeleteRowCol[a,{current}];
	current--;
	Continue[];
];

];(*if*)
current--;
];(*while*)

(* Begin with the last vertex, work towards the first, always considering the *last* interesting vertex. Interesting means that it is not 1-valent. 2-valent vertices do not occur since we change the graph carefully. *)
current=Length[a];
numberoflegs=If[Length[diagram]>1,Length[diagram[[2]]]+Length[diagram[[3]]],0];

While[True, (**** main loop ****)
If[current<1,
If[Length[a]>numberoflegs&&!secondround,
current=Length[a];
secondround=True;
,(*else*)
Break[];
];
];
degree=0;neighbours={};selfconnected=False;bigon=False;
bigonloop=False;
For[i=1,i<=Length[a],i++,
noe=a[[current,i]];
degree+=a[[current,i]];

(* At this point we can already detect several interesting cases, depending on the number of edges between the current vertex and vertex i: *)
Switch[noe,
1,
If[i==current,
	selfconnected=True,
	AppendTo[neighbours,i]];,
2,(* In this case, i must be unequal to current. The two must be part of a bigon or a loop or a lollipop. *)
	bigon=True;bigonneighbour=i;,
3,(* For simplicity, we treat this case separately. *)
	bigonloop=True;bigonneighbour=i;
]; (*switch*)

]; (*for*)
(* important: *)
If[selfconnected,degree++]; 

(* main distinction according to vertex degree *)
Switch[degree,
1, (* external leg, do nothing except make sure that this vertex is not visited again *)
current-=1;
Continue[];,

3, (* trivalent vertex, it can be a lollipop, part of a bigon(loop), triangle or square *)
If[selfconnected, (* lollipop *)
Return[0];
];

If[bigon,
bigonneighboursotherneighbour=First@FirstPosition[a[[bigonneighbour]],1];
a[[First@neighbours,bigonneighboursotherneighbour]]+=1;
If[First@neighbours!=bigonneighboursotherneighbour,
a[[bigonneighboursotherneighbour,First@neighbours]]+=1;
];
DeleteRowCol[a,{current,bigonneighbour}];
bp+=1;
current-=2;
Continue[];
];

If[bigonloop,
DeleteRowCol[a,{current,bigonneighbour}];
dp+=1;bp+=1;
current-=2;
Continue[];
];

triangle=False;
square=False;
Do[
If[a[[neighbours[[First@i]],neighbours[[Last@i]]]]==1,
triangle=True;
triangleneighbours=neighbours[[i]];
Break[];,
If[OptionValue@ReduceSquares&&secondround,
(* look for a square: *)
For[j=1,j<=Length[a],j++,
If[j!=current&&a[[neighbours[[First@i]],j]]==1&&a[[neighbours[[Last@i]],j]]==1&&a[[current,j]]==0&&a[[neighbours[[First@i]],neighbours[[Last@i]]]]==0,
square=True;
squareneighbours=neighbours[[i]];
othersquarecorner=j;
Break[];
];(*if*)
];(*for*)
];(*if*)
];(*if*)
If[square,Break[]];
,{i,idxlst}];(*do*)

If[triangle,
(* find the neighbours of the triangle and connect them to current *)
Do[
(* iterate over the two vertices with which "current" forms a triangle *)
othertriangleneighbour=triangleneighbours[[If[i==1,2,1]]];
For[j=1,j<=Length[a],j++,
If[j!=current&&j!=othertriangleneighbour&&a[[triangleneighbours[[i]],j]]!=0,
a[[current,j]]+=1;
a[[j,current]]+=1;
Break[];
];(*if*)
];(*for*)
,{i,{1,2}}];(*do*)

(* then delete the vertices *)

DeleteRowCol[a,triangleneighbours];
tp+=1;
current-=2;
Continue[];
];(*if*)

If[square,
squarevertices={current,squareneighbours[[1]],othersquarecorner,squareneighbours[[2]]};
neighboursofsquare={};
(* find the neighbours of the square *)
Do[
For[j=1,j<=Length[a],j++,
If[a[[i,j]]!=0&&!MemberQ[squarevertices,j],
AppendTo[neighboursofsquare,j];
Break[];
];
];
,{i,squarevertices}];
sqcoeff=SquareCoefficients[opts];
newdiagram=0;
For[i=1,i<=OptionValue@dimC4,i++,
newa=ConnectAt[a,internalC4Atoms[[i,1]],
neighboursofsquare,
internalC4Atoms[[i,3]]];
DeleteRowCol[newa,squarevertices];
If[Length@diagram>1,
newdiagram+=sqcoeff[[i]]*Join[Diagram[newa],Diagram@@GetNewLegIndices[newa,diagram[[2]],diagram[[3]]]];
,
newdiagram+=sqcoeff[[i]]*Diagram[newa];
];(*if*)
];
Return@ReduceDiagram[OptionValue@d^dp*OptionValue@b^bp*OptionValue@t^tp*newdiagram,opts];
];(*if*)

(* If we reach this point, then the diagram is invalid or the vertex is not part of something reducable because there are open legs. *)
current--;
];(*switch*)
];(*while*) (*********)

result=Diagram[a];

(* It might happen that one calls ReduceDiagram on a diagram with distinguished in/out legs. In this case, we can easily recover the new indices. Since this won't occur in time-critical cases, we don't optimize. *)
If[Length[diagram]>1,
result=Join[result,Diagram@@GetNewLegIndices[a,diagram[[2]],diagram[[3]]]];
];

If[Length[a]>0,Return[OptionValue@d^dp*OptionValue@b^bp*OptionValue@t^tp*result],Return[OptionValue@d^dp*OptionValue@b^bp*OptionValue@t^tp]];
];(*module*)
ReduceDiagram[x_+y_,opts:OptionsPattern[]]:=ReduceDiagram[x,opts]+ReduceDiagram[y,opts];
ReduceDiagram[c_*diagram_Diagram,opts:OptionsPattern[]]:=c*ReduceDiagram[diagram,opts];
ReduceDiagram[c_*x_/;FreeQ[c,_Diagram],opts:OptionsPattern[]]:=c*ReduceDiagram[x,opts];
ReduceDiagram[c_/;FreeQ[c,_Diagram],args___]:=c;

DiagramTensor[diagram1_Diagram,diagram2_Diagram]:=Module[
{d1=EnsureMatrix@diagram1,d2=EnsureMatrix@diagram2,legs1,legs2,newa},
legs1=If[Length@d1>1,List@@d1[[2;;3]],{{},{}}];
legs2=If[Length@d2>1,List@@d2[[2;;3]],{{},{}}];
newa=ArrayFlatten[{{First@d1,0},{0,First@d2}}];
If[legs1==legs2=={{},{}},
Return[Diagram[newa]];
,
legs2+=Length@First@d1;
Return[Diagram[newa,Join[legs1[[1]],legs2[[1]]],Join[legs1[[2]],legs2[[2]]]]];
];
];
DiagramTensor[x_,y_,z__]:=DiagramTensor[x,DiagramTensor[y,z]];
DiagramTensor[x_]:=x;

DiagramTensorPower[diagram_Diagram,n_Integer?NonNegative]:=If[n>0,DiagramTensor@@ConstantArray[diagram,n],internalEmptyDiagram];

ConnectAt[matrix1_List,matrix2_List,legs1_List,legs2_List]:=Module[{
a1=matrix1,a2=matrix2,l1=legs1,l2=legs2,
len1,result,i
},
(* ConnectAt checks *nothing*. The arguments need to have the appropriate format. *)
(* Put both matrices into one matrix *)
result=ArrayFlatten[{{a1,0},{0,a2}}];
len1=Length[a1];
For[i=1,i<=Length[l1],i++,
result[[l1[[i]],len1+l2[[i]]]]=1;
result[[len1+l2[[i]],l1[[i]]]]=1;
];(*for*)
Return[result];
](*module*)

DiagramCompose[diagram1_Diagram,diagram2_Diagram]:=Module[
{d1=EnsureMatrix@diagram1,d2=EnsureMatrix@diagram2,legs1,legs2,newa},
legs1=If[Length@d1>1,List@@d1[[2;;3]],{{},{}}];
legs2=If[Length@d2>1,List@@d2[[2;;3]],{{},{}}];
newa=ConnectAt[First@d1,First@d2,legs1[[2]],legs2[[1]]];
If[Length@legs1[[1]]==Length@legs2[[2]]==0,
Return[Diagram[newa]];
,
Return[Diagram[newa,legs1[[1]],Length@First@d1+legs2[[2]]]];
];
];
DiagramCompose[x_,y_,z__]:=DiagramCompose[x,DiagramCompose[y,z]];
DiagramCompose[x_]:=x;

DiagramTrace[diagram1_Diagram]:=Module[{
diagram,a,legs,len,i,v1,v2
},
diagram=EnsureMatrix@diagram1;
a=First@diagram;
(* To make life easier and speed up the program, I implement the trace manually. Again, DiagramTrace checks *nothing*. *)
legs=Join[diagram[[2]],Reverse@diagram[[3]]];
len=Length@legs;
For[i=1,i<=len/2,i++,
v1=legs[[i]];
v2=legs[[len-i+1]];
a[[v1,v2]]+=1;
a[[v2,v1]]+=1;
];(*for*)
Return[Diagram[a]];
];

DiagramRotate[diagram_Diagram]:=Diagram[First@diagram,Reverse@diagram[[3]],Reverse@diagram[[2]]];

DiagramFlipH[diagram_Diagram]:=Diagram[First@diagram,diagram[[3]],diagram[[2]]];

DiagramConjugate[diagram_Diagram]:=DiagramFlipH[diagram];

DiagramScalar[diagram1_,diagram2_]:=DiagramTrace@DiagramCompose[DiagramConjugate@diagram1,diagram2];

DiagramNorm[diagram_,opts:OptionsPattern[]]:=Sqrt@ReduceDiagram[DiagramScalar[diagram,diagram],opts];

DiagramMoveDown[diagram_Diagram,nlegs_]:=Diagram[First@diagram,Join[diagram[[2]],Reverse@Take[diagram[[3]],-nlegs]],Drop[diagram[[3]],-nlegs]];

DiagramMoveUp[diagram_Diagram,nlegs_]:=Diagram[First@diagram,Drop[diagram[[2]],-nlegs],Join[diagram[[3]],Reverse@Take[diagram[[2]],-nlegs]]];

MakeGraphs[diagrams_]:=(AdjacencyGraph[#,VertexLabels->"Name"]&)/@Cases[{diagrams},Diagram[x_,___]->x,Infinity];

FindDiagramIsomorphisms[diagram1_Diagram,diagram2_Diagram]:=Module[
{d1,d2,g1,g2,in1,in2,out1,out2,legs1,legs2,isos},

{d1,d2}=EnsureGraph[{diagram1,diagram2}];
{g1,in1,out1}=List@@d1;
{g2,in2,out2}=List@@d2;
legs1=Join[in1,out1];
legs2=Join[in2,out2];

isos=FindGraphIsomorphism[g1,g2,All];
Return@Cases[isos,_Association?((legs1/.#)==legs2&)];
];

IsomorphicDiagramQ[diagram1_Diagram,diagram2_Diagram]:=FindDiagramIsomorphisms[diagram1,diagram2]!={};

Components::unidentified="Diagram `1` could not be identified.";
Components[expr_,diagrams_List]:=Module[
{lendiagrams,e=expr,ds=diagrams,lincom,coeffs,i,j,identified},

lendiagrams=Length@ds;
If[e==0,Return@ConstantArray[0,lendiagrams]];

{e,ds}=EnsureGraph[{e,ds}];
e=Expand[e,_Diagram];

lincom=#/.c_.*x_Diagram/;FreeQ[c,_Diagram]->{c,x}&/@If[MatchQ[e,_Plus],List@@e,List@e];

coeffs=ConstantArray[0,lendiagrams];

Do[
identified=False;
For[j=1,j<=lendiagrams,j++,
If[IsomorphicDiagramQ[i[[2]],diagrams[[j]]],
identified=True;
coeffs[[j]]+=i[[1]];
Break[];
];(*if*)
];(*for*)
If[!identified,Message[Components::unidentified,i[[2]]]];
,{i,lincom}];

Return@coeffs;
];

DistinctDiagrams[expr_,levelspec_:Infinity]:=DeleteDuplicates[Cases[expr,_Diagram,levelspec],IsomorphicDiagramQ];

End[]

SetAttributes[{Diagram,EnsureGraph,EnsureMatrix,b,d,dimC4,ReduceDiagram,ReduceSquares,t,ConnectAt,DiagramCompose,DiagramConjugate,DiagramFlipH,DiagramMoveDown,DiagramMoveUp,DiagramNorm,DiagramRotate,DiagramScalar,DiagramTensor,DiagramTensorPower,DiagramTrace,Bilinearize,ConjugateLinearize,Linearize,MakeGraphs,Sesquilinearize,ClearLibrary,Description,LoadLibrary,Retrieve,FindDiagramIsomorphisms,IsomorphicDiagramQ,Components,DistinctDiagrams},{ReadProtected,Protected}];

EndPackage[]
