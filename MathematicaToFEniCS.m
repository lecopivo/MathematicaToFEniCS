(* ::Package:: *)

BeginPackage["MathematicaToFEniCS`"]

(* Naming conventions *)
functionSpacePrefix = "funSpace";
totalSpaceName = "totalSpace";
totalFunction = "w";
weakFormName = "F";
pointName = ptx; (* This should not be a string *)
boundaryFunctionName = "boundary_parts";

(* Internal variables *)
timeProblemDegree = 0;

TimeSuffix[0] := ""
TimeSuffix[n_] := TimeSuffix[n-1]<>"0"

SymbolSeparatedStringFromList[list_,symbol_] := StringJoin@ (ToString/@ Riffle[list,symbol]);
(* turns {a,b,c} to "a,b,c" *)
CommaSeparatedStringFromList[list_] := SymbolSeparatedStringFromList[list,","];
(* turns {a,b,c} to "a+b+c" *)
PlusSeparatedStringFromList[list_] := SymbolSeparatedStringFromList[list,"+"];

(* Extracts code lines from file `fileName`<>`Extra.py` which is between comment lines `# START: `tag`` and `# END: `tag`` *)
GetExtraCode[fileName_,tag_] :=
	Module[ {codeFile,extraCode},
		extraCode = {};
		codeFile = FileNameJoin[{ToString@NotebookDirectory[],fileName<>"Extra.py"}];
		If[FileExistsQ[codeFile],
		   Module[
			   {code,GetNumber},
			   code = ReadList[codeFile, String];
			   GetNumber[x_] := SelectFirst[Flatten @ x,IntegerQ,{}];
			   start = GetNumber @ Position[code, x_String /; StringMatchQ[x, ToString@StringForm["# START: `1`",tag]], 1];
			   end   = GetNumber @ Position[code, x_String /; StringMatchQ[x, ToString@StringForm["# END: `1`",tag]], 1];
			   If[IntegerQ[start] && IntegerQ[end] && (start<end),
			      extraCode = code[[start+1 ;; end-1]];
			   ];
		   ]
		];
		extraCode
	];

PointExpression[vars_]:= 
	Module[{n,pycPointExpression},
	       n=Length[vars];
	       pycPointExpression = {StringForm["`1`Expr = Expression((\"x[0]\",\"x[1]\",\"x[2]\"))",pointName]};
	       AppendTo[ pycPointExpression, StringForm["`1` = lambda i: `1`Expr[i]",pointName] ];
	       pycPointExpression
	]

InitializeFunctionSpaces[ funs_ ,testFuns_, femSpaces_,mesh_ ] := 
	Module[ {funSymbols,distinctFunctionSpaces,femSpacesNames,pycSpaceInitialization,pycFunctionInitialization,periodicBoundaryString,CreateFemSpaceName,meshFile},
		(* Get only symbols of functions, i.e. remove the time derivatives *)
		funSymbols = Replace[ funs, x_List :> First[x],1];
		
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
		
		(* Initialize total functions *)
		pycFunctionInitialization = {};
		For[ i=0, i <= timeProblemDegree, i++,
		     AppendTo[ pycFunctionInitialization, StringForm["`1``3` = Function( `2` )",totalFunction,totalSpaceName,TimeSuffix[i]]];
		];
		
		(* Split total function to its parts *)
		For[ i=0, i <= timeProblemDegree, i++,
		     Module[{funNames},
			    funNames =  (#<>TimeSuffix[i]) & /@ (ToString /@ funSymbols);
			    AppendTo[pycFunctionInitialization, StringForm["`1` = split(`2``3`)",CommaSeparatedStringFromList[funNames],totalFunction,TimeSuffix[i]]];
		     ]
		]
		AppendTo[pycFunctionInitialization, StringForm["`1` = TestFunctions(`2`)",CommaSeparatedStringFromList[testFuns],totalSpaceName]];
		
		(* return all python code lines *)
		Flatten@{pycSpaceInitialization,pycFunctionInitialization}
	]

DefineWeakForm[weakFormName_,weakForm_,funs_] :=
	Module[{pycWeakFun},
	       funSequence = CommaSeparatedStringFromList[ funs ];
	       pycWeakFun = {StringForm["def `1`(`2`):",weakFormName, totalFunction] };
	       AppendTo[ pycWeakFun, StringForm["\t`1` = split(`2`)", funSequence, totalFunction] ];
	       AppendTo[ pycWeakFun, StringForm["\treturn (`1`)*dx", weakForm] ];
	       AppendTo[ pycWeakFun, "" ];
	       pycWeakFun
	]					 


CreateWeakForm[weakForm_,funs_,testFuns_,vars_] :=
	Module[{n,m,tm,weak,funSymbols,pointRules,weakFix,argSequence,removeFunArgRules,weakFormNames,weakCForms,pycWeakForm,haha},
	       
	       n = Length[vars];
	       
	       If[ IntegerQ[Last[weakForm]],
			    m  = Length[weakForm]-1;
			    tm = Last[weakForm];
			    weak = Drop[weakForm,-1];
			  ,
			    m = Length[weakForm];
			    tm = m;
			    weak = weakForm;
	       ];

	       (* Get only symbols of functions, i.e. remove the time derivatives *)
	       funSymbols = Replace[ funs, x_List :> First[x],1];
	       
	       (* First rename variables to the name given by `pointName` e.g. changes x,y,z to ptx[0],ptx[1],ptx[2] if `pointName`=ptx *)
	       pointRules = (  #[[1]] ->  pointName[#[[2]]] ) & /@ Transpose[{vars,Range[0,n-1]}];

	       (* Remove function aguments rules *)
	       argSequence = CommaSeparatedStringFromList[ (ToString @ pointName <> ToString @ StringForm["(`1`)",#]) & /@ Range[0,n-1] ];
	       removeFunArgRules =Flatten[ {ToString @ StringForm["`1`(`2`)"  ,#,argSequence] -> ToString @ StringForm["`1`"  ,#,argSequence],
	       				    ToString @ StringForm["(`1`)(`2`)",#,argSequence] -> ToString @ StringForm["(`1`)"  ,#,argSequence] } & /@ Flatten[{funSymbols,testFuns}]];

	       (* Generate names of weak forms *)
	       weakFormNames = StringForm["`1``2`",weakFormName,#]& /@ Range[1,m];

	       (* Generate CForm of weak forms and remove function arguments *)
	       weakCForms = ToString /@ (CForm /@ (weak /. pointRules));
	       (* {removeFunArgRules,weakCForms} *)
	       weakCForms = StringReplace[#,removeFunArgRules] & /@ weakCForms;

	       (* Generate python code *)
	       pycWeakForm = Flatten[DefineWeakForm[#[[1]],#[[2]],funSymbols] &  /@ Transpose[{weakFormNames,weakCForms}]];
	       AppendTo[ pycWeakForm, StringForm["`1`Static = `2`", weakFormName, PlusSeparatedStringFromList[ (StringForm["`1`(`2`)",#,totalFunction])& /@ weakFormNames ] ]];

	       (* If time problem, generate Dynamic weak form *)
	       If[ timeProblemDegree>0,
		   Module[{ids},
			  ids = Flatten @ Position[funs, {_Symbol,_Integer}];
			  dtFuns = funs[[#,1]] & /@ ids;
			  tFuns  = testFuns[[#]] & /@ ids; 
			  timeDerivatives = StringForm["(`1`-`1``2`)*`3`",#[[1]],TimeSuffix[1],#[[2]] ] & /@ Transpose[{dtFuns,tFuns}];
			  AppendTo[ pycWeakForm, StringForm["`1`TimeDer = 1/Constant(dt)*(`2`)*dx",weakFormName,PlusSeparatedStringFromList[timeDerivatives]] ];
			  AppendTo[ pycWeakForm, StringForm["`1`CrankNicolson = 0.5*( `2` + `3` ) +`4`", weakFormName,
							    PlusSeparatedStringFromList[ (StringForm["`1`(`2`)",#,totalFunction])& /@ weakFormNames[[1;;tm]] ] /. "" -> "0",
							    PlusSeparatedStringFromList[ (StringForm["`1`(`2``3`)",#,totalFunction,TimeSuffix[1]])& /@ weakFormNames[[1;;tm]] ] /. "" -> "0",
							    PlusSeparatedStringFromList[ (StringForm["`1`(`2`)",#,totalFunction])& /@ weakFormNames[[tm+1;;m]] ] /. "" -> " 0"
						 ]]; (* the rules '""->"0"` are ther for the case when the string is empty but we still need valid mathematical expression *)
			  AppendTo[ pycWeakForm, StringForm["`1`Dynamic = `1`TimeDer + `1`CrankNicolson", weakFormName ]];
		   ];
	       ];
			       
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
	Module[{n,funSymbols,funsIdsBcsList,bcNames,pycBC},
	       
	       n=Length[funs];

	       (* Get only symbols of functions, i.e. remove the time derivatives *)
	       funSymbols = Replace[ funs, x_List :> First[x],1];
	       
	       funsIdsBcsList = Transpose[{funSymbols,Range[0,n-1],bcs}];
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
	       pycSolver = {StringForm["JStatic = derivative(`1`Static,`2`)",weakFormName,totalFunction]};
	       AppendTo[pycSolver, StringForm["problemStatic = NonlinearVariationalProblem(`1`Static,`2`,bc,JStatic)",weakFormName,totalFunction] ];
	       AppendTo[pycSolver, StringForm["solverStatic = NonlinearVariationalSolver(problemStatic)"] ];
	       AppendTo[pycSolver, "" ];
	       AppendTo[pycSolver, "prm = solverStatic.parameters" ];
	       AppendTo[pycSolver, "prm['newton_solver']['absolute_tolerance'] = 1E-8" ];
	       AppendTo[pycSolver, "prm['newton_solver']['relative_tolerance'] = 1E-7" ];
	       AppendTo[pycSolver, "prm['newton_solver']['maximum_iterations'] = 10" ];
	       AppendTo[pycSolver, "prm['newton_solver']['relaxation_parameter'] = 1.0" ];
	       If[timeProblemDegree>0,
		  AppendTo[pycSolver, "" ];
		  AppendTo[pycSolver, "# Initialize dynamic solver" ];
		  AppendTo[pycSolver, StringForm["JDynamic = derivative(`1`Dynamic,`2`)",weakFormName,totalFunction] ];
		  AppendTo[pycSolver, StringForm["problemDynamic = NonlinearVariationalProblem(`1`Dynamic,`2`,bc,JDynamic)",weakFormName,totalFunction] ];
		  AppendTo[pycSolver, StringForm["solverDynamic = NonlinearVariationalSolver(problemDynamic)"] ];
		  AppendTo[pycSolver, "" ];
		  AppendTo[pycSolver, "prm = solverDynamic.parameters" ];
		  AppendTo[pycSolver, "prm['newton_solver']['absolute_tolerance'] = 1E-8" ];
		  AppendTo[pycSolver, "prm['newton_solver']['relative_tolerance'] = 1E-7" ];
		  AppendTo[pycSolver, "prm['newton_solver']['maximum_iterations'] = 10" ];
		  AppendTo[pycSolver, "prm['newton_solver']['relaxation_parameter'] = 1.0" ];
	       ];
	       pycSolver
	];

SolveAndPlot[fileName_,funs_]:=
	Module[{pycSolveAndPlot},
	       pycSolveAndPlot = {};
	       If[timeProblemDegree==0,
		  
		  (* Static solve *)
		  AppendTo[pycSolveAndPlot, "solverStatic.solve()"];
		  AppendTo[pycSolveAndPlot, ""];
		  AppendTo[pycSolveAndPlot, "# Extra code" ];
		  pycSolveAndPlot = Join[pycSolveAndPlot, GetExtraCode[fileName,"POST_SOLVE"] ];
		,

		  (*else*)
		  
		  (* Dynamic solve *)
		  AppendTo[pycSolveAndPlot, "t = 0.0" ];
		  AppendTo[pycSolveAndPlot, "while t<T:" ];
		  AppendTo[pycSolveAndPlot, "\tsolverDynamic.solve()" ];
		  AppendTo[pycSolveAndPlot, "\t" ];
		  (* Add custom code *)
		  AppendTo[pycSolveAndPlot, "\t# Extra code" ];
		  pycSolveAndPlot = Join[pycSolveAndPlot, ("\t"<>#) & /@ GetExtraCode[fileName,"POST_SOLVE"] ];
		  (* Advance in time *)
		  AppendTo[pycSolveAndPlot, "\t" ];
		  AppendTo[pycSolveAndPlot, "\tt += dt" ];
		  AppendTo[pycSolveAndPlot, StringForm["\t`1``2`.assign(`1`)",totalFunction,TimeSuffix[1]] ];
	       ];

	       pycSolveAndPlot
	];

GenerateConstants[] :=
	Module[{pycConstants},
	       pycConstants = {};
	       If[ timeProblemDegree>0,
		   AppendTo[pycConstants, "dt = 0.01"];
		   AppendTo[pycConstants, "T = 1.0" ];
	       ];
	       pycConstants
	];


(* Process the input arguments and decide what to do with them.
  For example: It looks for time derivative, so it can decide wheather it is a time problem or not

 *)
ProcessArguments[fileName_,mesh_,vars_,funs_, bcs_,testFuns_,femSpaces_,weakForm_]:=
	Module[{},
	       timeProblemDegree = Max[ Cases[ funs, {_Symbol,n_Integer} -> n, 1], 0];
	       If[timeProblemDegree>1,
		  Print["Error: Problems with second or higher time derivative are not supported"];
		  Exit[];
	       ];
	];


GenerateCode[fileName_String,
	     mesh_String,
	     vars          : {_Symbol...},
	     funs          : {({_Symbol,_Integer}|_Symbol)...},
	     bcs           : {({{(_Symbol | _Integer | _Real ),_}...}|{})...},
	     testFuns      : {_Symbol...},
	     femSpaces     : {{_String,_Integer}...},
	     weakForm      : {___}]:=
	Module[{code ,WriteListOfString,outFile},
	       
	       WriteListOfString[file_,stringList_]:=Module[
		       {text},
		       text = SymbolSeparatedStringFromList[stringList,"\n"];
		       WriteString[file,text]
						     ];
	       
	       (* Prepare output file *)
	       outFile = FileNameJoin[{ToString@NotebookDirectory[],fileName<>".py"}];
	       If[FileExistsQ[outFile],DeleteFile[outFile],{}];

	       (* Process input arguments *)
	       ProcessArguments[fileName,mesh,vars,funs, bcs,testFuns,femSpaces,weakForm];

	       (* Make header of python code *)
	       WriteString[outFile,"# This file was generated by MathematicaToFEniCS\n"];
	       WriteString[outFile,"from MathematicaToFEniCS import *\n"];
	       WriteString[outFile,StringForm["from `1` import *",mesh]];
	       WriteString[outFile,"\n\n\n"];

	       (* Constants in code *)
	       WriteString[outFile,"# Problem constants\n"];
	       code = GenerateConstants[];
	       WriteListOfString[outFile,code];
	       WriteString[outFile,"\n\n\n"];

	       (* Custom initialization code *)
	       code = GetExtraCode[fileName,"INIT"];
	       WriteListOfString[outFile,code];
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

	       (* Custom post-solve code *)
	       code = GetExtraCode[fileName,"POST_INIT"];
	       WriteListOfString[outFile,code];
	       WriteString[outFile,"\n\n\n"];

	       (* Solve *)
	       WriteString[outFile,"# Solve\n"];
	       code = SolveAndPlot[fileName,funs];
	       WriteListOfString[outFile,code];
	       WriteString[outFile,"\n\n\n"];

	]


EndPackage[]

