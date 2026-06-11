     H**********************************************************************
     H*(C) Copyright FISERV Insurance Solutions, Inc., 2000 - 2006
    H**********************************************************************
    H*PROGRAM: FIXSTSTP1
    H*DESCRIPTION: API - update SWCPF and STSTP records for endorsement
    H*                   processing clients that do not have "All States"
    H*                   endorsements.
     H**********************************************************************
     H*CREATED: 07/17/07 C: 184 PROJECT: WC1529
     H**********************************************************************
     H**********************************************************************
     F*
     Fswcpf     Uf   e             disk
     Fststl4    Uf   e           k disk
     F*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D*
     D#CCCSS           s              5    dim(15) ctdata perrcd(1)
     Dkkcd             s                   like(stnst)
     D##form           s             15    inz(' ')
     D##rtn            s              1    inz('Y')
     Dxx               s              2  0 inz
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     C*
     C     stky          klist
     C                   kfld                    kkcd
     C*
     C*=====================================================================
     C*MAIN PROGRAM
     C*=====================================================================
     C*Retrieve client ID
     C                   call      'RTVCLTID'
     C                   parm                    CLTID             3
     C*
     C                   Eval      xx = 1
     C                   Dow       XX <= 15
     C*
     C                   If        cltid = %subst(#CCCSS(xx):1:3)
     C*
     C                   Read      swcpf
     C                   if        not %eof
     C                   Eval      wcfl34 = 'Y'
     C                   update    swc01
     C                   endif
     C*
     C                   Eval      kkcd = %subst(#CCCSS(xx):4:2)
     C     stky          chain     ststl4
     C                   if        %found
     C*
     C                   Eval      stenau = 'Y'
     C                   Update    pst01
     C*
     C                   endif
     C                   endif
     C*
     C                   Eval      xx = xx + 1
     C                   enddo
     C
     C*
     C                   Eval      *inlr=*on
    C*====================================================================*
**  #CCCSS - "CCC" is client number + "SS" is NCCI state code
03309
04617
06599
08216
08899
09042
09799
10232
10245
10809
13204
13317
13604
14010
14109
