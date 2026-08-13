      // @MBRTEXT: PAPI Training PGM
     h Option(*NoDebugIO:*SrcStmt) DftActGrp(*No) ActGrp('PJSTRAIN')
     FProducts  if   e           k Disk    Rename(PRODUCTS:PRODUCTSR)
     C     *Entry        PList
     C                   Parm                    Prod_In           7 0
     C                   Parm                    Name_Out         50
     C                   Parm                    Desc_Out        100
     C                   Parm                    Price_Out         9 2
     C     Prod_In       Chain     Productsr                          95
     C                   If        *in95 = *Off
     C                   Eval      Name_Out = PRNAME
     C                   Eval      Desc_Out = PRDESC
     C                   Eval      Price_Out = PRPRICE
     C                   Else
     C                   Eval      Name_Out = *blanks
     C                   Eval      Desc_Out = *blanks
     C                   Eval      Price_Out = 0
     C                   Endif

     C                   RETURN