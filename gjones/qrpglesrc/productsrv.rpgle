      // @MBRTEXT: RPG Service pgm
     h Option(*NoDebugIO:*SrcStmt) NoMain
     FProducts  if   e           k Disk    Rename(PRODUCTS:PRODUCTSR)




          DCL-PR GETSTOCK PACKED(5:0);
          Prod_In PACKED(7:0) VALUE;
          END-PR ;


          // --------------------------------------------------
          // Procedure name: GETSTOCK
          // Purpose:
          // Returns:
          // Parameter: Prod_In
          // --------------------------------------------------
          DCL-PROC GETSTOCK EXPORT;
          DCL-PI *N PACKED(5:0);
          Prod_In PACKED(7:0) VALUE;
          END-PI ;
          DCL-S retField PACKED(5:0);
          Chain (Prod_In) ProductsR;
          If %Found;
          retField = PRQTY;
          Else;
          retField = 0;
          EndIf;
          return retField ;
          END-PROC ;
