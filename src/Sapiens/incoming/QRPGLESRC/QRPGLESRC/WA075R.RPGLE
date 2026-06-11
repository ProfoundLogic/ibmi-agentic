     H*(C) Copyright StoneRiver Inc., 2009
     H*
     H/TITLE WA075R - API to check EDI format ID in groups
     H*
     H*****************************************************************
     H*                                                               *
     H*  CREATED: 09/03/09
     H*           C:53   Project: WC1839
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 00/00/09 C:00   Project:
     H*****************************************************************
     Fwmfnl     if   e           k disk

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@co#           s              3  0
     D @@fnd           s              3  0
     D @@rtcd          s              1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     C*
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@fnd
     C                   parm                    @@rtcd
     C*____________________________________________________________________
     C*
     C     fnky          klist
     C                   kfld                    @@co#
     C*____________________________________________________________________
     C*
     C*
     C     fnky          setll     wmfnl
     C*
     C                   dou       *in99
     C*
     C     fnky          reade     wmfnl                                  99
     C*
     C* SKIP test fund, non edi, current fund being processed.
     C*
     C                   if        not *in99 and FNTEST <> 'Y'
     C                               and FNEDI <> 'Y'
     C                               and FNFND <> @@fnd
     C*
     C                   if        FNEDIF <> *BLANKS
     C                   eval      @@rtcd = 'F'
     C                   leave
     C                   endif
     C*
     C                   endif
     C*
     C                   enddo
     C*____________________________________________________________________
     C*
     C                   move      *on           *inlr
