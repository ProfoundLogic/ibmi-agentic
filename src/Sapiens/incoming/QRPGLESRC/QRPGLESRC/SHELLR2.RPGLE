     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H/TITLE
     H****************************************************************
     H*  DESCRIPTION:                                                *
     H*                                                              *
     H*  CALLED FROM:                                                *
     H*                                                              *
     H*  CALLS TO:                                                   *
     H*                                                              *
     H*  CREATED: XX/XX/XX      C:XX      PROJECT: XXXXX             *
     H****************************************************************
     H*  REVSIONS:                                                   *
     H*                                                              *
     H****************************************************************
     FSHELLF2   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
      /COPY SCOPYPR01D
     D*
     D***PY SCOPYPR01I
     D*
     D*================================================================
     D*  Constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Title')
     D COMAPI          C                   CONST('WA001R')

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@PRDS           40
     C                   PARM                    @@CALL            1
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    @@CALL        XXCALL
     C*
     C/COPY SCOPYPR01C
     C*
     C*================================================================
     C* DSCAPI - Call API for getting descriptions (use 91 on error)
     C*================================================================
     C*
     C     DSCAPI        BEGSR
     C*
     C                   CALL      COMAPI        COMPL                  91
     C*
     C                   ENDSR                                                  DSCAPI
     C*
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C                   EXSR      OTHFKS
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*================================================================
     C* KJDO   - Processing for F10
     C*================================================================
     C*
     C     KJDO          BEGSR
     C                   ENDSR                                                  KJDO
     C*
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
     C*
     C*================================================================
     C* PGMSTR - Used for any start of program processing (one-time)
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C     Q1PRMS        IFGE      1
     C     @@PRDS        ANDNE     *BLANKS
     C                   MOVEL(P)  @@PRDS        ##DATA
     C                   ELSE
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   ENDIF                                                  Q1PRMS>=1
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA
     C                   PARM      40            ##LENG
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C*
     C     Q1PRMS        IFGE      2
     C                   MOVEL     @@CALL        XXCALL
     C                   ELSE
     C                   CLEAR                   XXCALL
     C                   ENDIF                                                  Q1PRMS>=2
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*================================================================
     C* PREDSP - Pre-processing before displaying (every time)
     C*================================================================
     C*
     C     PREDSP        BEGSR
     C                   ENDSR                                                  PREDSP
     C*
     C*================================================================
     C* PRSCTL - Process control after no errors found (Call Submitter)
     C*================================================================
     C*
     C     PRSCTL        BEGSR
     C                   ENDSR                                                  PRSCTL
     C*
