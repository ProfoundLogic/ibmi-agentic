     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
    H*********************************************************************
    H/TITLE PBR/HBA/Network Hold/Release - Scrn Fmt
    H*********************************************************************
    H*  REVSIONS:                                                        *
    H*                                                                   *
    H*********************************************************************
     Fn0215f1   cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
     D/COPY SCOPYPR01D
     D*entry
     DN0215R1          pr                  ExtPgm('N0215R1')
     DN0215R1          pi
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D optnm           c                   const('Hold/Release Records -
     D                                     for Billing')
     D comapi          c                   const('FA007R')
     D ##co#           s              3  0
     D ##fnam          s             40
     D ##fnd           s              3  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYDTPL
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C/COPY SCOPYPR01C
    C*
    C*====================================================================
    C* DSCAPI - Call API for getting descriptions (use 91 on error)
    C*====================================================================
    C*
     C     DSCAPI        begsr
    C*
     C                   call      comapi        compl                  91
    C*
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
    C*
     C     ERRCK1        begsr
     C                   endsr
    C*
    C*====================================================================
    C* KJDO   - Processing for F10
    C*====================================================================
    C*
     C     KJDO          begsr
     C                   endsr
    C*
    C*====================================================================
    C* OTHFKS - Processing for other functions keys
    C*====================================================================
    C*
     C     OTHFKS        begsr
    C*
     C     *inkj         caseq     *on           kjdo                     90
     C                   endcs
    C*
     C                   endsr
    C*
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
    C*
     C     PGMEND        begsr
     C                   endsr
    C*
    C*====================================================================
    C* PGMSTR - Used for any start of program processing (one-time)
    C*====================================================================
    C*
     C     PGMSTR        begsr
    C*
     C                   movel(p)  optnm         ##data
    C*
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
    C*
     C                   movel     ##data        scprds
    C*
     C                   endsr
    C*
    C*====================================================================
    C* PREDSP - Pre-processing before displaying (every time)
    C*====================================================================
    C*
     C     PREDSP        begsr
     C                   endsr
    C*
    C*====================================================================
    C* PRSCTL - Process control after no errors found (Call Submitter)
    C*====================================================================
    C*
     C     PRSCTL        begsr
     C                   endsr
    C*
