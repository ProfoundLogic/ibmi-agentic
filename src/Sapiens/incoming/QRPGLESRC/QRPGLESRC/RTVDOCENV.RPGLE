     Hcopyright ('(c) Copyright StoneRiver, Inc., 2010')
     Hoption(*nodebugio)
     H*********************************************************************
     H/title RTVDOCENV - Retrieve Document Usage Environment
     H*********************************************************************
     H*                                                                   *
     H*  Created: 11/02/10   C:183   Project: SY195                       *
     H*********************************************************************
     H*  Revisions:                                                       *
     H*                                                                   *
     H*********************************************************************
     Fssylf     if   e           k disk    ignore(spc01:src01:sdr01:sbc01:
     F                                            scs01:sgl01:sp201)
     D*entry
     D RtvDocEnv       pr
     D  outEnv                        1
     D RtvDocEnv       pi
     D  outEnv                        1
     D
     C                   eval      SYFL11 = *blanks
     C     'SY'          chain     SSY01
     C                   eval      outEnv = %subst(SYFL11:5)
     C                   select
     C* If Position 5 of SYFL11 field is blank, then default it to 'Q' for QDLS
     C                   when      outEnv = *Blank
     C                   eval      OutEnv = 'Q'
     C                   endsl
     C                   eval      *inlr = *on
     C                   return
