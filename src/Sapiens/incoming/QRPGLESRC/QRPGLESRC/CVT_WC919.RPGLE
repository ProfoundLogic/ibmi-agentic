    H*  (C) Copyright Progressive Data Solutions, Inc., 2000
    H*********************************************************************
    H/TITLE CVT_WC919 - Load last rate set code from WDF1P to WDRRP
    H*********************************************************************
    H*  DESCRIPTION:  This program will transfer the rate set code value *
    H*                of the last WDF1P record to all records in WDRRP.  *
    H*                                                                   *
    H*  CREATED: 02/10/00  PROGRAMMER: C:33  PROJECT: WC919              *
    H*********************************************************************
    H*  REVISIONS:                                                       *
    H*                                                                   *
    H*********************************************************************
     Fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     Fwdf1l     if   e           k disk    rename(pf101:pf100)
     Fwdrrl     uf   e           k disk    rename(prr01:prr00)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@co#           s              3
     D @@fnd           s              3
     D xxco#           s                   like(FNCO#)
     D xxfnd           s                   like(FNFND)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C     co#_key       klist
     C                   kfld                    xxco#
    C*
     C     fnd_key       klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
    C*
     C     cvt_key       klist
     C                   kfld                    FNCO#
     C                   kfld                    FNFND
     C*____________________________________________________________________
    C*
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@fnd
     C*____________________________________________________________________
     C*
     C                   clear                   xxco#
     C                   clear                   xxfnd
     C*
     C                   if        %parms >= 1
     C                   move      @@co#         xxco#
     C                   endif
     C*
     C                   if        %parms >= 2
     C                   move      @@fnd         xxfnd
     C                   endif
     C*____________________________________________________________________
     C*
     C     *start        setll     wmfnl
     C*
     C                   select
     C                   when      xxco# = *zero
     C                   read      pfn00                                  99
     C                   when      xxco# <> *zero and xxfnd = *zero
     C     co#_key       chain     pfn00                              99
     C                   other
     C     fnd_key       chain     pfn00                              99
     C                   endsl
     C*
     C                   dow       *in99 = *off
     C*
     C                   exsr      process_F1
     C*
     C                   select
     C                   when      xxco# = *zero
     C                   read      pfn00                                  99
     C                   when      xxco# <> *zero and xxfnd = *zero
     C     co#_key       reade     pfn00                                  99
     C                   other
     C     fnd_key       reade     pfn00                                  99
     C                   endsl
     C*
     C                   enddo
     C*
     C                   eval      *inlr = *on
     C*____________________________________________________________________
     C*
     C*====================================================================
     C* process_F1 - Get the last WDF1P record for the group
     C*====================================================================
     C*
     C     process_F1    begsr
     C*
     C     cvt_key       setgt     pf100
     C     cvt_key       readpe    pf100                                  99
     C*
     C                   if        *in99 = *off
     C                   exsr      process_RR
     C                   endif
     C*
     C                   endsr
     C*
     C*====================================================================
     C* process_RR - Load the rate set code for all WDRRP records
     C*====================================================================
     C*
     C     process_RR    begsr
     C*
     C     cvt_key       chain     prr00                              99
     C*
     C                   dow       *in99 = *off
     C*
     C                   eval      RRDRAT = F1RTCD
     C*
     C                   update    prr00
     C*
     C     cvt_key       reade     prr00                                  99
     C*
     C                   enddo
     C*
     C                   endsr
