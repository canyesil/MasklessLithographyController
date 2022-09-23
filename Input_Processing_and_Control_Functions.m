(* ::Package:: *)

(* ::Input::Initialization:: *)
process=Function[{imgpath,ratio},
b2=Import[imgpath];
b=Binarize[b2];
ad=ImageData[b];
dim2=Dimensions[ad];
cmre=Ceiling[ArrayResample[ad,Round[dim2[[1]]/ratio]]];
trim=Position[cmre,0];
tr1=MinMax[Sort[trim[[;;,1]]]];
tr2=MinMax[Sort[trim[[;;,2]]]];
cmret=cmre[[tr1[[1]];;tr1[[2]],tr2[[1]];;tr2[[2]]]];
res={Image[cmret],cmret,Dimensions[cmret]}
];

process2=Function[{dim,cmretto},
pos=-1*{Round[dim[[1]]/2],Round[dim[[2]]/2]};
poses=Position[cmretto,0];
commands=Partition[Flatten[Table[Join[{{1,0}},Join[Join[Table[{0,1},dim[[2]]],{{1,0}}],Join[Table[{0,-1},dim[[2]]]]]],Round[dim[[1]]/2]]],2];
pose=Accumulate[commands];
LaserOn=Table[If[MemberQ[poses,pose[[X]]],1,0],{X,1,Length[pose]}];
lines={};
pos={0,0};
For[i=1,i<Length[commands],i++,
pos+=commands[[i]];
If[LaserOn[[i]]==1,
AppendTo[lines,pos];
pos={0,0};
];
];
lines
];

process3=Function[{lines,LaserH,step},
LaserOnR=Table[If[lines[[XX,1]]==0&&Abs[lines[[XX,2]]]<=1,1,0],{XX,1,Length[lines]}];
dir=<|-1->"l",1->"r",0->"r"|>;
comm=StringPadLeft[ToString[LaserH*LaserOnR[[#]]],3,"0"]<>dir[Sign[lines[[#,1]]]]<>ToString[step*Abs[lines[[#,1]]]]<>"m2"<>dir[Sign[lines[[#,2]]]]<>ToString[step*Abs[lines[[#,2]]]]<>"\n"&/@Range[1,Length[lines]];
gap=step*Abs[lines[[#,1]]]+step*Abs[lines[[#,2]]]&/@Range[1,Length[lines]];
{comm,gap}
];


process4=Function[{},

{comm, gap} = Import[NotebookDirectory[] <> "commngap.mx"];

temp = PrintTemporary["-Device Response-"];
For[ii = 1, ii <= Length[comm], ii++,
 DeviceWriteBuffer[dev, comm[[ii]]];
 Pause[gap[[ii]] * 10^-3 + 0.1];
 NotebookDelete[temp];
 temp = PrintTemporary[FromCharacterCode[DeviceReadBuffer[dev]]];
];

];

processGetDevices=Function[{},
pro = StartProcess[$SystemShell];
WriteLine[pro, "ls /dev/cu.*"];
Pause[0.2]; 
r = ReadString[pro, EndOfBuffer];
Pause[0.2];
rm = StringSplit[r, "\n"];
WriteLine[pro, "exit"];
rm
];
