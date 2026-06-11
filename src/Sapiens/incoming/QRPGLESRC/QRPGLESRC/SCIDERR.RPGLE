     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H* SCIDERR  iDarts Error display                                 *
     H*****************************************************************
     H* Revisions:
     H*
     H* 08/03/13  C:33  Implement GS (no comments)
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  23    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*****************************************************************
     FSCIDERRF  CF   E             WORKSTN usrOpn
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSCIDERR   CF   E             WORKSTN usrOpn
     F                                     INFDS(FIDS01G)

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D                 DS
     D  XXTIME                 1      6  0
     D  XXMIN                  3      4  0
     D  XXSEC                  5      6  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D FIDS01G       E DS                  EXTNAME(SZQ2P)
     D                                     prefix(QG:2)
     D  CURSOR               370    371B 0
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D DFTMSG          C                   CONST('No response from the-
     D                                      FIC module. Undefin-
     D                                     ed error  condition.-
     D                                                         -
     D                                                         ')
     DisUseGS          s              1n   inz

     D/copy sPrAtrium

     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    RTNMSG          100
     C                   PARM                    RCD               1
     C*
     C                   MOVEL     *BLANKS       RCD
     C                   MOVEL     'N'           SCRCD
     C*
     C     RTNMSG        IFEQ      *BLANKS
     C                   MOVEL     DFTMSG        SCMSG1
     C                   MOVE      DFTMSG        SCMSG2
     C                   ELSE
     C                   MOVEL     RTNMSG        SCMSG1
     C                   MOVE      RTNMSG        SCMSG2
     C                   ENDIF
     C*
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        XXTIME
     C*
     C*
      *===============================================================
      * INITIALIZATION    AREA
      *
     C     TOP           TAG
      /Free
       isUseGS = not AtriumJob;

       Select;
       When isUseGS;
          Open SCIDERR;
          q2RecF = 'WDW01G';
       Other;
          Open SCIDERRF;
          q2RecF = 'WDW01';
       Endsl;
      /End-free
      *
      *===============================================================
      *
     C     MAINLN        TAG
      *
      *===============================================================
      * SCREEN EXECUTION AREA
      *
     C                   EXSR      SCREXC
      *
      *===============================================================
      * HANDLE F KEYS IF PRESSED
      *
     C     *IN11         CASEQ     '1'           FKEYS
     C                   END                                                      END *IN11
      *
      *===============================================================
      *
     C     Q2RECF        CASEQ     'WDW01 '      WDW01T
     C     Q2RECF        CASEQ     'WDW01G'      WDW01T
     C                   ENDCS
      *
     C*
      *===============================================================
      * TERMINATION OF PROGRAM
      *
     C     TERM          TAG
     C*
     C*
     C                   MOVE      '1'           *INLR
     C*
     C/EJECT
      *===============================================================
     C     SCREXC        BEGSR
      *===============================================================
      *
     C                   MOVEL     Q2RECF        SCFILE                         SET FORMAT/FILE
     C*
      /Free
       Select;
       When q2RecF = 'WDW01';
          Exfmt WDW01;
       When q2RecF = 'WDW01G';
          Exfmt WDW01G;
       Endsl;
       fiDs01 = fiDs01G;
      /End-free
     C*
     C*
     C                   ENDSR
      *
     C/EJECT
      *===============================================================
     C     FKEYS         BEGSR
      *===============================================================
      *
     C     *INKA         IFEQ      '1'
     C                   END
      *
     C     *INKB         IFEQ      '1'
     C                   END
      *
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD            1
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
      *
     C     *INKD         IFEQ      '1'
     C                   END
      *
      *
      *
     C     *INKF         IFEQ      '1'
     C                   END
      *
      *
     C     *INKG         IFEQ      '1'
     C                   END
      *
      *
     C     *INKH         IFEQ      '1'
     C                   END
      *
      *
     C     *INKI         IFEQ      '1'
     C                   END
      *
      *
     C     *INKJ         IFEQ      '1'
     C                   END
      *
     C     *INKK         IFEQ      '1'
     C                   END
      *
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'WDW01'
     C     Q2RECF        OREQ      'WDW01G'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C                   END                                                    END *INKL
     C*
     C     *INKM         IFEQ      '1'
     C                   END
      *
     C     *INKN         IFEQ      '1'
     C                   END
      *
     C     *INKP         IFEQ      '1'
     C                   END
      *
     C     *INKQ         IFEQ      '1'
     C                   END
      *
     C     *INKR         IFEQ      '1'
     C                   END
      *
     C     *INKS         IFEQ      '1'
     C                   END
      *
     C     *INKT         IFEQ      '1'
     C                   END
      *
     C     *INKU         IFEQ      '1'
     C                   END
      *
     C     *INKV         IFEQ      '1'
     C                   END
      *
     C     *INKW         IFEQ      '1'
     C                   END
      *
     C     *INKX         IFEQ      '1'
     C                   END
      *
     C*
     C                   GOTO      MAINLN
      *
     C                   ENDSR
     C/EJECT
      *===================WDW01T======================================*
     C     WDW01T        BEGSR
     C**** EDIT FIELDS ENTERED HERE **************
     C*
     C***    VALID CHAR IN SELECT FIELDS
     C     SCRCD         IFNE      'Y'
     C     SCRCD         ANDNE     'N'
     C                   MOVE      *ON           *IN23
     C                   GOTO      MAINLN
     C                   ENDIF
     C*
     C*
     C*
     C*
     C**** LOAD VALUES TO RETURN WITH ***
     C                   MOVEL     SCRCD         RCD
     C*
     C*** END OF THE PGM
     C*
     C                   GOTO      TERM
     C*
     C*
     C                   ENDSR

