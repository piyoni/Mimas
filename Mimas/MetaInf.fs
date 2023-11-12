module MetaInf

let intMaxValue =  2147483647
let intMinValue = -2147483648
type CodeSpan =
    {
        startcol:int;
        endcol:int;
        startln:int;
        endln:int;
    }
let codeSpanNull =
    {
        startcol = intMaxValue;
        endcol   = intMinValue;
        startln  = intMaxValue;
        endln    = intMinValue;
    }

let codeSpanMerge csa csb =
    {
        startcol  = min csa.startcol csb.startcol;
        endcol    = max csa.endcol   csb.endcol;
        startln   = min csa.startln  csb.startln;
        endln     = max csa.endln    csb.endln;
    }