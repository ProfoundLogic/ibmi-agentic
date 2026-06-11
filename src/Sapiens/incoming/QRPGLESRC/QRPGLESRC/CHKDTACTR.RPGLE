     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H*********************************************************************
     H/title CHKDTACTR - Check DataCenter Client
     H*********************************************************************
     H*                                                                   *
     H*  Created: 09/21/09   C:33    Seq#: WA099/69233                    *
     H*********************************************************************
     H*  Revisions:                                                       *
     H*                                                                   *
     H*********************************************************************
     Fssylf     if   e           k disk    ignore(spc01:src01:sdr01:sbc01:
     F                                            scs01:sgl01:sp201)
     D*entry
     D ChkDtaCtr       pr
     D  outDtaCtr                     1n
     D ChkDtaCtr       pi
     D  outDtaCtr                     1n
     D
     C                   eval      SYFL11 = *blanks
     C     'SY'          chain     SSY01
     C                   select
     C                   when      %subst(SYFL11:2:1) = 'Y'
     C                   eval      outDtaCtr = *on
     C                   other
     C                   eval      outDtaCtr = *off
     C                   endsl
     C                   eval      *inlr = *on
     C                   return
