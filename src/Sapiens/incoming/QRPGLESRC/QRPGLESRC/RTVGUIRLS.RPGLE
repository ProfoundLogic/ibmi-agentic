     Hcopyright ('(c) Copyright Fiserv, Inc., 2008')
     hoption(*nodebugio)
     H*********************************************************************
     H/title RTVGUIRLS - Retrieve the GUI/Go! Release Level
     H*********************************************************************
     H*                                                                   *
     H*  Created: 01/29/08   C:33    Project: DR260                       *
     H*********************************************************************
     H*  Revisions:                                                       *
     H*                                                                   *
     H*********************************************************************
     Fssylf     if   e           k disk    ignore(spc01:src01:sdr01:sbc01:
     F                                            scs01:sgl01:sp201)
     D*entry
     D RtvGUIRls       pr
     D  outRlsLvl                     1
     D RtvGUIRls       pi
     D  outRlsLvl                     1
     D
     C                   eval      SYFL11 = *blanks
     C     'SY'          chain     SSY01
     C                   eval      outRlsLvl = %subst(SYFL11:4)
     C                   eval      *inlr = *on
     C                   return
