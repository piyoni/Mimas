module MetaInf

type CodeSpan =
    {
        startcol:int;
        endcol:int;
        startln:int;
        endln:int;
    }


let codeSpanMerge csa csb =
    {
        startcol  = min csa.startcol csb.startcol;
        endcol    = max csa.endcol   csb.endcol;
        startln   = min csa.startln  csb.startln;
        endln     = max csa.endln    csb.endln;
    }