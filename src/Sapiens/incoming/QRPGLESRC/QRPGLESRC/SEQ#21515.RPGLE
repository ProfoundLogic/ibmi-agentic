    H*  (C) COPYRIGHT PROGRESSIVE DATA SOLUTIONS, INC., 2000
    H*********************************************************************
    H/TITLE SEQ#21515 - Load net amount based on commissions
    H*********************************************************************
    H* DESCRIPTION:                                                      *
    H*   Load the net amount as follows:  NET = AMT - COMM               *
    H*                                                                   *
    H*********************************************************************
    H* REVISIONS:                                                        *
    H*                                                                   *
    H*********************************************************************
     Fwdabl     uf   e           k disk
     Fwsasl     uf   e           k disk
     Fwtabl     IF   E           K DISK    ignore(PRS01)
    F*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D xxbill          s                   like(ASBILL)
     D xxpaid          s                   like(ASPAID)
     D xxkey           s                   like(TTKEY)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     C*
     C     abky          klist
     C                   kfld                    ASCO#
     C                   kfld                    ASSTDT
     C                   kfld                    ASAGT#
     C*
     C     tbky          klist
     C                   kfld                    ABCO#
     C                   kfld                    xxkey
     C*____________________________________________________________________
    C*
     C     *loval        setll     pab01
     C                   read      pab01                                  99
    C*
     C                   dow       *in99 = *off
     C*
     C                   if        ABARTY <> 'P'
     C                              and ABARTY <> 'V'
     C                              and ABARTY <> 'R'
     C*
     C                   movel(P)  ABARCD        XXKEY
     C*
     C     TBKY          chain     PTT01                              60
     C*
     C                   if        *in60 = *off
     C                              and TTECFL <> 'Y'
     C                   eval      ABNET = ABAMT - ABCOMM
     C                   else
     C                   eval      ABNET = ABAMT
     C                   endif                                                  *in60=*off
     C*
     C                   endif
    C*
     C                   update    pab01
    C*
     C                   read      pab01                                  99
    C*
     C                   enddo
     C*
     C                   exsr      UPDT_WSASP
     C*
     C                   eval      *inlr = *on
     C*____________________________________________________________________
     C*
     C*====================================================================
     C* UPDT_WSASP - Update records in WSASP
     C*====================================================================
     C*
     C     UPDT_WSASP    BEGSR
     C*
     C                   read      pas01                                  60
     C*
     C                   dow       *in60 = *off
     C*
     C                   clear                   xxbill
     C                   clear                   xxpaid
     C*
     C     abky          chain(N)  pab01                              99
     C*
     C                   dow       *in60 = *off
     C*
     C                   select
     C                   when      ABARTY = 'A'
     C                              or ABARTY = 'B'
     C                              or ABARTY = 'D'
     C                              or ABARTY = 'F'
     C                              or ABARTY = 'G'
     C                              or ABARTY = 'N'
     C                   add       ABNET         xxbill
     C                   when      ABARTY = 'P'
     C                   add       ABAMT         xxpaid
     C                   when      ABARTY = 'R'
     C                              or ABARTY = 'V'
     C                   sub       ABAMT         xxpaid
     C                   endsl
     C*
     C     abky          reade(N)  pab01                                  60
     C*
     C                   enddo                                                  *in60=*off
     C*
     C                   eval      ASBILL = xxbill
     C                   eval      ASPAID = xxpaid
     C                   eval      ASBAL  = ASBILL - ASPAID
     C*
     C                   update    pas01
     C*
     C                   read      pas01                                  60
     C*
     C                   enddo                                                  *in60=*off
     C*
     C                   ENDSR                                                  UPDT_WSASP
