(* ::Package:: *)

BeginPackage["MathematicaToFEniCS`"]

(* Naming conventions *)
functionSpacePrefix = "funSpace";
totalSpaceName = "totalSpace";
totalFunction = "w";
weakFormName = "F";
pointName = ptx; (* This should not be a string *)
boundaryFunctionName = "boundary_parts";


SymbolSeparatedStringFromList[list_,symbol_] := StringJoin@ (ToString/@ Riffle[list,symbol]);
(* turns {a,b,c} to "a,b,c" *)
CommaSeparatedStringFromList[list_] := SymbolSeparatedStringFromList[list,","];
(* turns {a,b,c} to "a+b+c" *)
PlusSeparatedStringFromList[list_] := SymbolSeparatedStringFromList[list,"+"];

PointExpression[vars_]:= 
	Module[{n,pycPointExpression},
	       n=Length[vars];
	       pycPointExpression = {StringForm["`1`expr = Expression((\"x[0]\",\"x[1]\",\"x[2]\"))",pointName]};
	       AppendTo[ pycPointExpression, StringForm["`1` = lambda i: `1`expr[i]",pointName] ];
	       pycPointExpression
	]

InitializeFunctionSpaces[ funs_ ,testFuns_, femSpaces_,mesh_ ] := 
	Module[ {distinctFunctionSpaces,femSpacesNames,pycSpaceInitialization,pycFunctionInitialization,periodicBoundaryString,CreateFemSpaceName,meshFile},
		
		(* Make sure that we initialize each space only once *)
		distinctFunctionSpaces = DeleteDuplicates @ femSpaces;
		
		(* Function to convert {"femType",degree} to the function space name *)
		CreateFemSpaceName[femSpace_] := StringForm["`1``2``3`",functionSpacePrefix,femSpace[[1]],femSpace[[2]]];
		femSpacesNames = CreateFemSpaceName /@ femSpaces;

		(* Use periodic bounary if mesh file contains symbol "PeriodicBoundary" *)
		meshFile = FileNameJoin[{NotebookDirectory[],mesh<>".py"}];
		periodicBoundaryString = If[ FindList[ meshFile, "PeriodicBoundary"] != {}, 
					    ", constrained_domain=PeriodicBoundary()",
					    ""];
		
		(* Create python command to initialize all function spaces *)
		pycSpaceInitialization =StringForm[ "`1` = FunctionSpace(`4`, '`2`', `3``5`)",CreateFemSpaceName[#],#[[1]],#[[2]],mesh,periodicBoundaryString]& /@ distinctFunctionSpaces;
		
		(* Create python command to initialize space of all spaces *)
		AppendTo[ pycSpaceInitialization, StringForm[ "`1` = MixedFunctionSpace([`2`])",totalSpaceName, CommaSeparatedStringFromList[femSpacesNames] ] ] ;
		
		(* Initialize total function *)
		pycFunctionInitialization =  {StringForm["`1` = Function( `2` )",totalFunction,totalSpaceName]};
		
		(* Split total function to its parts *)
		AppendTo[pycFunctionInitialization, StringForm["`1` = split(`2`)",CommaSeparatedStringFromList[funs],totalFunction]];
		AppendTo[pycFunctionInitialization, StringForm["`1` = TestFunctions(`2`)",CommaSeparatedStringFromList[testFuns],totalSpaceName]];
		
		(* return all python code lines *)
		Flatten@{pycSpaceInitialization,pycFunctionInitialization}
	]

DefineWeakForm[weakFormName_,weakForm_,funs_] :=
	Module[{pycWeakFun},
	       pycWeakFun = {StringForm["def `1`(`2`):",weakFormName, CommaSeparatedStringFromList[funs] ] };
	       AppendTo[ pycWeakFun, StringForm["\treturn (`1`)*dx", weakForm] ];
	       AppendTo[ pycWeakFun, "" ];
	       pycWeakFun
	]					 
	

CreateWeakForm[weak_,funs_,testFuns_,vars_] :=
	Module[{n,pointRules,weakFix,argSequence,removeFunArgRules,weakFormNames,weakCForms,pycWeakForm,haha},
	       n = Length[vars];
	       m = Length[weak];
	       (* First rename variables to the name given by `pointName` e.g. changes x,y,z to ptx[0],ptx[1],ptx[2] if `pointName`=ptx *)
	       pointRules = (  #[[1]] ->  pointName[#[[2]]] ) & /@ Transpose[{vars,Range[0,n-1]}];

	       (* Remove function aguments rules *)
	       argSequence = CommaSeparatedStringFromList[ (ToString @ pointName <> ToString @ StringForm["(`1`)",#]) & /@ Range[0,n-1] ];
	       removeFunArgRules =Flatten[ {ToString @ StringForm["`1`(`2`)"  ,#,argSequence] -> ToString @ StringForm["`1`"  ,#,argSequence],
	       				    ToString @ StringForm["(`1`)(`2`)",#,argSequence] -> ToString @ StringForm["(`1`)"  ,#,argSequence] } & /@ Flatten[{funs,testFuns}]];

	       (* Generate names of weak forms *)
	       weakFormNames = StringForm["`1``2`",weakFormName,#]& /@ Range[1,m];

	       (* Generate CForm of weak forms and remove function arguments *)
	       weakCForms = ToString /@ (CForm /@ (weak /. pointRules));
	       (* {removeFunArgRules,weakCForms} *)
	       weakCForms = StringReplace[#,removeFunArgRules] & /@ weakCForms;

	       (* Generate python code *)
	       argSequence = StringForm["(`1`)", CommaSeparatedStringFromList[funs] ];
	       pycWeakForm = Flatten[DefineWeakForm[#[[1]],#[[2]],funs] &  /@ Transpose[{weakFormNames,weakCForms}]];
	       AppendTo[ pycWeakForm, StringForm["`1` = `2`", weakFormName, PlusSeparatedStringFromList[ (StringForm["`1``2`",#,argSequence])& /@ weakFormNames ] ]];
	       pycWeakForm
	]


BCName[fun_,bcId_]:= StringForm["bc_`1`_`2`",fun,bcId];
BCNames[fun_,bc_]:= BCName[fun,#[[2]] ] & /@ bc;
ConstantBoundaryCond[fun_,funId_,bc_]:=
	Module[{n,pycBC,bcNames},
	       n= Length[bc];
	       pycBC = StringForm[ "`1` = DirichletBC(`2`.sub(`3`), `4`, `5`, `6`)",BCName[fun,#[[2]] ],totalSpaceName,funId,#[[1]],boundaryFunctionName,#[[2]]]& /@ bc;
	       pycBC
	]
ConstantBoundaryConditions[funs_,bcs_]:=
	Module[{n,funsIdsBcsList,bcNames,pycBC},
	       n=Length[funs];
	       funsIdsBcsList = Transpose[{funs,Range[0,n-1],bcs}];
	       pycBC = ConstantBoundaryCond[ #[[1]],#[[2]],#[[3]]] & /@ funsIdsBcsList;
	       bcNames =  Flatten[BCNames[#[[1]],#[[3]] ]& /@ funsIdsBcsList];
	       AppendTo[ pycBC, StringForm["bc = [`1`]", SymbolSeparatedStringFromList[bcNames,","]]];
	       Flatten @ pycBC
	]

(* Generate solver part of code
  params - parameters for solver, for now it is underines and can be anything
 *)
InitializeSolver[params_]:= 
	Module[{pycSolver},
	       pycSolver = {StringForm["J = derivative(`1`,`2`)",weakFormName,totalFunction]};
	       AppendTo[pycSolver, StringForm["problem = NonlinearVariationalProblem(`1`,`2`,bc,J)",weakFormName,totalFunction] ];
	       AppendTo[pycSolver, StringForm["solver = NonlinearVariationalSolver(problem)"] ];
	       AppendTo[pycSolver, "" ];
	       AppendTo[pycSolver, "prm = solver.parameters" ];
	       AppendTo[pycSolver, "prm['newton_solver']['absolute_tolerance'] = 1E-8" ];
	       AppendTo[pycSolver, "prm['newton_solver']['relative_tolerance'] = 1E-7" ];
	       AppendTo[pycSolver, "prm['newton_solver']['maximum_iterations'] = 10" ];
	       AppendTo[pycSolver, "prm['newton_solver']['relaxation_parameter'] = 1.0" ];
	       pycSolver
	];

SolveAndPlot[funs_]:=
	Module[{pycSolveAndPlot},
	       pycSolveAndPlot = {};
	       AppendTo[pycSolveAndPlot, "solver.solve()"];
	       (* AppendTo[pycSolveAndPlot, "" ]; *)
	       (* AppendTo[pycSolveAndPlot, StringForm["plot(`1`, title=\"`1`\")",#] ] & /@ funs; *)
	       (* AppendTo[pycSolveAndPlot, "" ]; *)
	       (* AppendTo[pycSolveAndPlot, "interactive()" ]; *)
	       pycSolveAndPlot
	];


GenerateCode[fileName_,mesh_,vars_,funs_, bcs_,testFuns_,femSpaces_,weakForm_,customCode_:{}]:=
	Module[{code ,WriteListOfString,outFile},
	       
	       WriteListOfString[file_,stringList_]:=Module[
		       {text},
		       text = SymbolSeparatedStringFromList[stringList,"\n"];
		       WriteString[file,text]
						  ];

	       (* Prepare output file *)
	       outFile = FileNameJoin[{ToString@NotebookDirectory[],fileName}];
	       If[FileExistsQ[outFile],DeleteFile[outFile],{}];

	       (* Make header of python code *)
	       WriteString[outFile,"# This file was generated by MathematicaToFEniCS\n"];
	       WriteString[outFile,"from MathematicaToFEniCS import *\n"];
	       WriteString[outFile,StringForm["from `1` import *",mesh]];
	       WriteString[outFile,"\n\n\n"];

	       (* Insert custom code *)
	       WriteString[outFile, "# Custom code\n"];
	       If[Length[customCode]>0,
		  WriteListOfString[outFile,customCode[[1]] ]]
	       WriteString[outFile,"\n\n\n"];

	       (* Make some definitions *)
	       WriteString[outFile,"# Define few useful expressions\n"];
	       code = PointExpression[vars];
	       WriteListOfString[outFile,code];
	       WriteString[outFile,"\n\n\n"];

	       (* Function space initialization *)
	       WriteString[outFile,"# Initialize function spaces and functions\n"];
	       code  = InitializeFunctionSpaces[funs,testFuns,femSpaces,mesh];
	       WriteListOfString[outFile,code];
	       WriteString[outFile,"\n\n\n"];

	       (* Define weak form *)
	       WriteString[outFile,"# Define weak form\n"];
	       code = CreateWeakForm[weakForm,funs,testFuns,vars];
	       WriteListOfString[outFile,code];
	       WriteString[outFile,"\n\n\n"];

	       (* Define boundary conditions *)
	       WriteString[outFile,"# Define boundary conditions\n"];
	       code = ConstantBoundaryConditions[funs,bcs];
	       WriteListOfString[outFile,code];
	       WriteString[outFile,"\n\n\n"];

	       (* Initialize solver *)
	       WriteString[outFile,"# Initialize solver\n"];
	       code = InitializeSolver[{}];
	       WriteListOfString[outFile,code];
	       WriteString[outFile,"\n\n\n"];

	       (* Solve and plot *)
	       WriteString[outFile,"# Solve and plot\n"];
	       code = SolveAndPlot[funs];
	       WriteListOfString[outFile,code];
	       WriteString[outFile,"\n\n\n"];

	       (* Insert custom code *)
	       WriteString[outFile, "# Custom code\n"];
	       If[Length[customCode]>1,
		  WriteListOfString[outFile,customCode[[2]] ]]
	       WriteString[outFile,"\n\n\n"];

	]


EndPackage[]
