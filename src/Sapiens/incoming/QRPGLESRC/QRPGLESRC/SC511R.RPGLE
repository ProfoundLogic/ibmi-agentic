     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE SC511R - DISPLAY FOLDER SEARCH                           *
     H*                                                               *
     H* 03/29/16 C:306 Support:30409                                  *
     H*                Recreate SC511R program (folder search)        *
     H*                This program is mimic/recreated by taking ref. *
     H*                SC510R (Document Search program).              *
     H*****************************************************************
     H*Revisions:                                                     *
     H*                                                               *
     H*****************************************************************
     FSC511F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     F*
     FSAFSL     IF   E           K DISK

     D*
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D
     D StrLen          s              5  0 Inz(0)

      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQBASFLR         63
     C                   PARM                    QQFLDNM          12
     C                   PARM                    QQRTNCD           1
     C
     C     KEY1          KLIST
     C                   KFLD                    QQBASFLR
     C*
     C                   MOVEL     QQBASFLR      SCFOLD
     C*
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C*
     C*   INITIALIZE ROUTINE * * *
     C                   EXSR      INIT
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C
     C                   DOU       *INKC = *On
     C*
     C     Q2RECF        IFEQ      'SCR01S'
     C     Q2RECF        OREQ      'SCR01C'
     C
     C                   MOVE      '1'           *IN01
     C                   EXFMT     SCR01C
     C                   MOVE      '0'           *IN01
     C                   MOVE      '0'           *IN20
     C                   ENDIF
     C*
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*******************************************************************
     C*
     C     *IN11         IFEQ      '1'
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD            1
     C                   Leave
     C                   ENDIF                                                  END *INKC
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01S'
     C     Q2RECF        OREQ      'SCR01C'
     C                   MOVE      'B'           QQRTCD
     C                   Leave
     C                   ENDIF
     C                   ENDIF                                                  END *INKL
     C                   EndIF                                                  *IN11
     C
     C
     C                   READC     SCR01S                                 50
     C     *IN50         IFEQ      '0'
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*** SAVE THAT DATA TO BE PASSED BACK TO THE CALLING PROGRAM
     C
     C                   If        SCSEL = '1'
     C                   MOVE      '*'           QQRTCD
     C
     C                   If        SCDNAM  <> ' '
     C                   Eval      StrLen = %len(%trim(QQBASFLR))
     C***** Checking that entered path is ended with '/' backslash or not
     C
     C                   If        %subst(%trim(QQBASFLR):Strlen:1) = '/'
     C                   Eval      QQBASFLR = %xlate('/':' ':QQBASFLR:Strlen)
     C                   EndIf
     C
     C                   Eval      QQBASFLR = %trim(QQBASFLR) + '/' +
     C                             %trim(SCDNAM)
     C                   Leave
     C                   Else
     C                   Eval      QQBASFLR = %trim(QQBASFLR)
     C                   Leave
     C                   EndIf                                                  SCDNAM <> ' '
     C
     C                   Elseif    SCSEL <> ' '
     C                   MOVE      '1'           *IN20
     C                   UPDATE    SCR01S
     C                   EndIf
     C
     C                   Endif
     C
     C                   EndDo
     C                   Eval      *Inlr = *On
      *====================================================================*
      * * *   INIT SUBROUTINE                                    I N I T ***
      *====================================================================*
     C     INIT          BEGSR
     C                   Z-ADD     0             RRN2              5 0
     C*
     C                   Eval      SCFILE = 'SC511F'
     C                   Eval      SCRECF = 'SCR01S'
     C*
     C                   ENDSR
      *====================================================================*
      * * *   LOAD SUBROUTINE                                    L O A D ***
      *====================================================================*
     C     LOAD          BEGSR
     C*
     C                   EVAL      RRN2 = 0
     C                   MOVEL     *BLANK        SCDNAM
     C                   MOVEL     *BLANK        SCDESC
     C                   MOVEL     *BLANK        SCRVDAT
     C
     C     *loval        SETLL     SAFSL
     C                   READ      SAFSL
     C                   DOW       NOT %EOF(SAFSL)
     C                   If        FLRNAM  <> ' ' And FLRNAM <> '.'
     C                             And FLRNAM <> '..' And FLRNAM <> '...'
     C                             And FLRNAM <> '....'
     C                             And FLRNAM <> '.....'
     C                             And FLRNAM <> '......'
     C                             And FLRNAM <> '.......'
     C                             And FLRNAM <> '........'
     C                             And FLRNAM <> '.........'
     C                             And FLRNAM <> '..........'
     C                             And FLRNAM <> '...........'
     C                             And FLRNAM <> '............'
     C
     C                   EVAL      RRN2 = RRN2 + 1
     C
     C                   EVAL      SCDNAM  = FLRNAM
     C                   EVAL      SCDESC  = DESC
     C                   EVAL      SCRVDAT = LASTRV
     C
     C                   IF        RRN2 > 9999
     C                   LEAVESR
     C                   ENDIF
     C
     C                   WRITE     SCR01S
     C                   EndIF
     C
     C                   READ      SAFSL
     C*
     C                   ENDDO
     C
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2          IFEQ      0
     C                   MOVEL     *BLANK        SCDNAM
     C                   MOVEL     *BLANK        SCDESC
     C                   MOVEL     *BLANK        SCRVDAT
     C                   Z-ADD     1             RRN2
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01S
     C                   END
     C*
     C                   ENDSR
