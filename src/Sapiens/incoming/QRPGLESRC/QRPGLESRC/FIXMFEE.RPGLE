    H*  (C) Copyright Fiserv, Inc., 2000 - 2005
    H*****************************************************************
    H/TITLE FIXMFEE - UPDATE EMPLOYER DETAIL WITH F1MFEE
    H*****************************************************************
    H*  REVISIONS:                                                   *
    H*                                                               *
    H*  06/24/05  C:33  Project: 109WDZ                              *
    H*                  Cvt to RPGIV from CLT109 environment         *
    H*****************************************************************
     Fwdelp     uf   e           k disk
     Fwdf1l     if   e           k disk
    C*
    C*  FUND DETAIL FILE
    C*
     C     f1ky          klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elfyr
     *
     C                   read      pel01                                  60
     *
     C                   dow       *in60 = *off
     *
     C     f1ky          chain     pf101                              60
     *
     C                   if        *in60 = *off
     C                             and elmfee = *zero
     C                   eval      elmfee = f1mfee
     C                   update    pel01
     C                   endif
     *
     C                   read      pel01                                  60
     *
     C                   enddo
     *
     C                   move      *on           *inlr
