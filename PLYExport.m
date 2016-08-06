PLYWriteVertices::usage =
    "Produces ply file storing a list of vertex elements, each with \
attributes as given in attrs of type double";
PLYWriteVertices[
  fn_String
  , attrs : {__String}
  , data_ /; MatrixQ[data, NumericQ]
  , BinaryFormat -> bf_?BooleanQ] /;
    Dimensions@data~MatchQ~{_, Length@attrs} :=
    Module[{f, h},

      f = OpenWrite[fn, BinaryFormat -> bf];
      h = StringTemplate["ply
      format `` 1.0
      comment Created with the Wolfram Language : www.wolfram.com
      element vertex ``
      ``
      end_header
      "][
        If[bf, "binary_little_endian", "ascii"]
        , Length@data
        , StringRiffle["property double " <> # & /@ attrs, "\n"]
      ];

      If[bf, BinaryWrite[f, h], WriteString[f, h]];
      If[bf, BinaryWrite[f, data, "Real64"], Export[f, data, "Table"]];

      Close[f];
      fn
    ];

PLYWriteVertices[fn_String,
  data_List /; Dimensions@data~MatchQ~{_, 3},
  BinaryFormat -> bf_?BooleanQ] :=
    PLYWriteVertices[fn, {"x", "y", "z"}, data, BinaryFormat -> bf];
PLYWriteVertices[fn_String,
  data_List /; Dimensions@data~MatchQ~{_, 6},
  BinaryFormat -> bf_?BooleanQ] :=
    PLYWriteVertices[fn, {"x", "y", "z", "red", "green", "blue"}, data,
      BinaryFormat -> bf];
PLYWriteVertices[fn_String,
  data_List /; Dimensions@data~MatchQ~{_, 7},
  BinaryFormat -> bf_?BooleanQ] :=
    PLYWriteVertices[
      fn, {"x", "y", "z", "red", "green", "blue", "alpha"}, data,
      BinaryFormat -> bf];

ClearAll@UnpackPointData
UnpackPointData::usage =
    "Takes a Point, specifying a single point or multiple, including or \
without UnpackPointData and returns a list of 3 or 6 (7) vectors \
describing xyzrgb(a)";

UnpackPointData[
  Point[c : {_, _, _},
    VertexColors -> {col : (List | RGBColor)[__]}]] := {c~
    Join~(List @@ col)};
UnpackPointData[
  Point[c : {_, _, _},
    VertexColors -> col : (List | RGBColor)[__]]] := {c~
    Join~(List @@ col)};
UnpackPointData[
  Point[cs : {{_, _, _} ..},
    VertexColors -> cols : {(List | RGBColor)[__] ..}]] /;
    Length@cs == Length@cols := ArrayFlatten[{{cs, List @@@ cols}}];

UnpackPointData[Point[cs : {{_, _, _} ..}]] := cs;
UnpackPointData[Point[c : {_, _, _}]] := {c};

Unprotect@Export;
DownValues@Export = DeleteDuplicates[{bf}~Module~Flatten@Apply[
  {
    HoldPattern@Export[fn_String, pts : {__Point}, #1] :>
        PLYWriteVertices[fn, Join @@ (UnpackPointData /@ pts),
          BinaryFormat -> #2]
    ,
    HoldPattern@
        Export[fn_String, Graphics3D[pts : {__Point}], #1] :>
        PLYWriteVertices[fn, Join @@ (UnpackPointData /@ pts),
          BinaryFormat -> #2]
    ,
    HoldPattern@Export[fn_String, pt_Point, #1] :>
        PLYWriteVertices[fn, UnpackPointData@pt, BinaryFormat -> #2]
    ,
    HoldPattern@Export[fn_String, Graphics3D[pt_Point], #1] :>
        PLYWriteVertices[fn, UnpackPointData@pt, BinaryFormat -> #2]
  } &
  , {{{"PLY", "BinaryFormat" -> bf_?BooleanQ}, bf}, {"PLY", False}}
  , {1}
]~Join~{
    HoldPattern@Export[fn_String, pts : {__Point}] /; ToLowerCase@FileExtension[fn] == "ply" :>
        PLYWriteVertices[fn, Join @@ (UnpackPointData /@ pts),
          BinaryFormat -> False]
    ,
    HoldPattern@
        Export[fn_String, Graphics3D[pts : {__Point}]] /; ToLowerCase@FileExtension[fn] == "ply" :>
        PLYWriteVertices[fn, Join @@ (UnpackPointData /@ pts),
          BinaryFormat -> False]
    ,
    HoldPattern@Export[fn_String, pt_Point] /; ToLowerCase@FileExtension[fn] == "ply" :>
        PLYWriteVertices[fn, UnpackPointData@pt, BinaryFormat -> False]
    ,
    HoldPattern@Export[fn_String, Graphics3D[pt_Point]] /; ToLowerCase@FileExtension[fn] == "ply" :>
        PLYWriteVertices[fn, UnpackPointData@pt, BinaryFormat -> False]
  }~Join~DownValues@Export];
Protect@Export;