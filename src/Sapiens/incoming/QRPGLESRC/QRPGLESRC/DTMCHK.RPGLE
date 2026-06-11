     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H/TITLE DTMCHK - DTM CHECK
     H*****************************************************************
     H*  Description: Looks at SSYPF/SYFL07 to see if we are to use   *
     H*               DTM processing, if so, the flag will be 'D'     *
     H*                                                               *
     H*  Called from: various pgms (e.g. SYPRTDOC, SYINCDOC)          *
     H*                                                               *
     H*  Calls to   :                                                 *
     H*                                                               *
     H*  Created: 03/24/03  Programmer: 087                           *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*  10/23/09 C:33  Project: SY059                                *
     H*                 Only check the subsystems at a defined        *
     H*                  interval.                                    *
     H*                                                               *
     H*  10/05/05 C:33  Project: SY059  WA045/49240                   *
     H*                 Start the PDSDTM subsystem in case it isn't   *
     H*                  already started.                             *
     H*****************************************************************
    H*
     Fssypf     if   e             disk    usropn
     F***  Begin Add  ***  10/23/09  **************************************
     FDTMChk    uf a e             disk    usropn

     DCheckDTMSbs      pr             1n
     D***  End   Add  ***  10/23/09  **************************************

     D***  Begin Add  ***  10/05/05  **************************************
     d/copy sPrRunCmd

     D cmdString       s           3000
     D***  End   Add  ***  10/05/05  **************************************
     D@usedtm          s              1
     D
     C     *entry        plist
     C                   parm                    @usedtm
     C
     C                   movel     *blanks       @usedtm
     C                   open      ssypf
     C     1             chain     ssypf                              99
     C                   close     ssypf
     C                   if        *in99 = *off
     C                   if        syfl07 = 'D'
     C                   eval      @usedtm = 'Y'
     C***  Begin Add  ***  10/23/09  **************************************
     C                   if        CheckDTMSbs
     C***  End   Add  ***  10/23/09  **************************************
     C***  Begin Add  ***  10/05/05  **************************************
     C                   eval      cmdString = 'STRSBS SBSD(PDSDTM)'
     C                   callp(e)  RunCmd(cmdString:%len(cmdString))
     C                   eval      cmdString = 'STRSBS SBSD(PDSFAX)'
     C                   callp(e)  RunCmd(cmdString:%len(cmdString))
     C***  End   Add  ***  10/05/05  **************************************
     C***  Begin Add  ***  10/23/09  **************************************
     C                   endif
     C***  End   Add  ***  10/23/09  **************************************
     C                   endif
     C                   endif

     C                   return





     P***  Begin Add  ***  10/23/09  **************************************
     P*====================================================================
     P* Check last DTM subsystm check
     P*====================================================================
     PCheckDTMSbs      b
     DCheckDTMSbs      pi             1n

     Dcheck            s              1n
     Dhours            s              5  0
     DsysTimeStamp     s               z
     Dinterval         c                   1

     C* DataCenter
     C                   if        %subst(SYFL11:2:1) = 'Y'
     C                   return    *off
     C                   endif
     C*
     C                   eval      sysTimeStamp = %timestamp
     C*
     C                   open      DTMChk
     C*
     C     1             chain     DTMChk                             99
     C*
     C                   if        not %found
     C                   eval      DTMLstTime = %timestamp
     C                   write     pDT01
     C     1             chain     DTMChk                             99
     C                   endif
     C*
     C     sysTimeStamp  subdur    DTMLstTime    hours:*hours           99
     C                   eval      check = hours >= interval
     C*
     C                   select
     C                   when      check
     C                   eval      DTMLstTime = %timestamp
     C                   endsl
     C*
     C                   update    pDT01
     C                   close     DTMChk
     C*
     C                   return    check
     C*
     PCheckDTMSbs      e
     P***  End   Add  ***  10/23/09  **************************************
