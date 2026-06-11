     Hcopyright ('(c) Copyright StoneRiver, Inc., 2010')
     Hoption(*nodebugio)
     H*********************************************************************
     H/title RTVDOCROOT - Retrieve Document Root Directory
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
     D RtvDocRoot      pr
     D  outRoot                      10
     D RtvDocRoot      pi
     D  outRoot                      10
     D
     C                   eval      SYFL13 = *blanks
     C     'SY'          chain     SSY01
     C                   eval      outRoot = SYFL13
     C                   select
     C                   when      outRoot = *Blanks
     C                   eval      outRoot = 'QDLS'
     C                   endsl
     C                   eval      *inlr = *on
     C                   return
