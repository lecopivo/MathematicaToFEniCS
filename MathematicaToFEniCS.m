(* ::Package:: *)

BeginPackage["MathematicaToFEniCS`"]

(* Naming conventions *)
functionSpacePrefix = "funSpace";
totalSpaceName = "totalSpace";
totalFunction = "w";
weakFormName = "F";
pointName = "ptx";
boundaryFunctionName = "boundary_parts";

SymbolSeparatedStringFromList[list_,symbol_] := StringJoin@ (ToString/@ Riffle[list,symbol]);
(* turns {a,b,c} to "a,b,c" *)
CommaSeparatedStringFromList[list_] := SymbolSeparatedStringFromList[list,","];
(* turns {a,b,c} to "a+b+c" *)
PlusSeparatedStringFromList[list_] := SymbolSeparatedStringFromList[list,"+"];

PointExpression[vars_]:= 
	Module[{n,pycPointExpression},
	       n=Length[vars];
	       pycPointExpression = StringForm["`1` = Expression((\"x[0]\",\"x[1]\",\"x[2]\"))",pointName];
	       pycPointExpression
	]

InitializeFunctionSpaces[ funs_ ,testFuns_, femSpaces_,mesh_ ] := 
	Module[ {distinctFunctionSpaces,femSpacesNames,pycSpaceInitialization,pycFunctionInitialization,CreateFemSpaceName},
		(* Make sure that we initialize each space only once *)
		distinctFunctionSpaces = DeleteDuplicates @ femSpaces;
		(* Function to convert {"femType",degree} to the function space name *)
		CreateFemSpaceName[femSpace_] := StringForm["`1``2``3`",functionSpacePrefix,femSpace[[1]],femSpace[[2]]];
		femSpacesNames = CreateFemSpaceName /@ femSpaces;
		(* Create python command to initialize all function spaces *)
		pycSpaceInitialization =StringForm[ "`1` = FunctionSpace(`4`, '`2`', `3`)",CreateFemSpaceName[#],#[[1]],#[[2]],mesh ]& /@ distinctFunctionSpaces;
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

(* Rules how to remove function arguments from `fun` - Wierd stuff: StringForm does not produce string, so ToString is neccessary *)
RemoveFunctionArgumentRulesFun[ fun_, vars_] :=
	{ 
		ToString@StringForm["`1`(`2`)",fun,CommaSeparatedStringFromList[vars]] -> ToString@StringForm["`1`",fun],
		ToString@StringForm["(`1`)(`2`)",fun,CommaSeparatedStringFromList[vars] ]->  ToString@StringForm["(`1`)",fun]
	}
CreateWeakForm[weak_,funs_,testFuns_,vars_] :=
	Module[{n,removeFunctionArgumentRules,pointRules,rules,weakFormNames,weakCForms,pycWeakForm},
	       n = Length[vars];
	       m = Length[weak];
	       removeFunctionArgumentRules =  Flatten @ (RemoveFunctionArgumentRulesFun[#,vars]& /@ Flatten[{funs,testFuns}]);
	       pointRules = (ToString@#[[1]] -> ToString@StringForm["`1`[`2`]",pointName,#[[2]]] )  &/@ Transpose[{vars,Range[0,n-1]}];
	       weakFormNames = StringForm["`1``2`",weakFormName,#]& /@ Range[1,m];
	       rules = Flatten @{removeFunctionArgumentRules,pointRules};
	       weakCForms =StringReplace[ToString@#,rules]&/@(CForm /@ weak);
	       pycWeakForm =StringForm[  "`1` = (`2`)*dx",#[[1]],#[[2]]]&/@ Transpose[{weakFormNames,weakCForms}];
	       AppendTo[ pycWeakForm, StringForm["`1` = `2`", weakFormName, PlusSeparatedStringFromList[weakFormNames ] ]];
	       pycWeakForm
	]


BCName[fun_,bcId_]:= StringForm["bc_`1`_`2`",fun,bcId];
BCNames[fun_,bc_]:= BCName[fun,#[[2]] ] & /@ bc;
ConstantBoundaryCond[fun_,funId_,bc_]:=
	Module[{n,pycBC,bcNames},
	       n= Length[bc];
	       pycBC = StringForm[ "`1` = DirichletBC(`2`.sub(`3`), Constant(`4`), `5`, `6`)",BCName[fun,#[[2]] ],totalSpaceName,funId,#[[1]],boundaryFunctionName,#[[2]]]& /@ bc;
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
	       AppendTo[pycSolveAndPlot, "" ];
	       AppendTo[pycSolveAndPlot, StringForm["plot(`1`)",#] ] & /@ funs;
	       AppendTo[pycSolveAndPlot, "" ];
	       AppendTo[pycSolveAndPlot, "interactive()" ];
	       pycSolveAndPlot
	];


GenerateCode[fileName_,mesh_,vars_,funs_, bcs_,testFuns_,femSpaces_,weakForm_]:=
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

	      (* Make some definitions *)
	       WriteString[outFile,"# Define few useful expressions\n"];
	       code = PointExpression[vars];
	       WriteString[outFile,code];
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
	]


EndPackage[]
