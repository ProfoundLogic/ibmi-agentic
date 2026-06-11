     H copyright('(c) Copyright Fiserv, Inc 2006')
     H nomain
    H*********************************************************************
    H*  REVSIONS:                                                        *
    H*                                                                   *
    H*********************************************************************
     Fstgtl     if   e           k disk    rename(pgt01:pgt00)
     D*
     D ##co#           s                   like(GTCO#) inz(205)
     D kkKey           s                   like(GTKEY)
     D kkTbID          s                   like(GTTBID) inz('CDDT')
     D*
     D/copy mPrMenu
     C*
     C     keySTGTL      klist
     C                   kfld                    ##co#
     C                   kfld                    kkTbID
     C                   kfld                    kkKey

    C*====================================================================
    C* CrtDtaAraNm - Create the DTAARA name from the prompts
    C*====================================================================
     PCrtDtaAraNm      b                   export
     D CrtDtaAraNm     pi            10
     D  inClt#                        4    const
     D  inApp                         3    const
     D  inEnv                         1    const
     D  inRls                         4    const

     D dtaAraNm        s             10

     C                   clear                   GTCF02
     C                   eval      kkKey = inRls
     C     keySTGTL      chain     pgt00
     C                   if        GTCF02 = *blank
     C                   eval      GTCF02 =inRls
     C                   endif
     C*
     C                   eval      dtaAraNm = inEnv +
     C                                        %subst(inApp:1:1) +
     C                                        %subst(inClt#:1:3) +
     C                                        GTCF02

     C                   return    dtaAraNm

     PCrtDtaAraNm      e
