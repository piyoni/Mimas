module TypeTree
    type TypeTree =
        | Var   of string
        | Base  of string
        | Cases of TypeTree list
        | Composite of string * TypeTree list
    let bint   = TypeTree.Base("int")
    let bbool  = TypeTree.Base("bool")
    let bfloat = TypeTree.Base("float")
    let bintfloat = TypeTree.Cases([bint;bfloat])
    type TypeRel =
        TypeRel of TypeTree * TypeTree list
    type FnNodeType =
        FnNode of TypeRel list
    let radd = FnNode([TypeRel(bint,[bint;bint]);TypeRel(bfloat,[bfloat;bfloat]);TypeRel(bbool,[bbool;bbool])])

