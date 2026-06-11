     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE IS159R - PROCESS REJECTED DOCUMENT                       *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR01:                                                        *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H*  10/17/11 C:279 Project: SY195
     H*                 Modify program to pass/return UNC path field
     H*                 and IFS flag to called/calling programs.
     H*
     H*****************************************************************
     FIS159F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FIMDOL1    IF   E           K DISK
     FIMDTL     IF   E           K DISK
     F*

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  XCURDT                 1      8  0
     D  XCURYR                 1      4  0
     D  XCURMD                 5      8  0
     D*
     D                 DS
     D  D2DTTM                 1     14  0
     D  D2TIME                 1      6  0
     D  D2DATE                 7     14  0
     D*
     D                 DS
     D  XXA75                  1     75
     D  XXBSTS                24     25
     D  XXBM                  24     25
     D  XXSLH1                26     26
     D  XXBD                  27     28
     D  XXHYPH                28     28
     D  XXSLH2                29     29
     D  XXBYR                 30     33
     D  XXBTMH                39     40
     D  XXCOL1                41     41
     D  XXBTMM                42     43
     D  XXCOL2                44     44
     D  XXBTMS                45     46
     D  XXLDSC                31     70
     D  XXBUSR                57     66
     D  XXBERR                24     75
     D*
     D*
     D*
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D  CURSOR               370    371B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
     C*
     C*** BATCH STATUS TABLE FILE
     C*
     C     DSKY1         KLIST
     C                   KFLD                    QQBSTS
     C*
     C*** DOC TYPE FILE
     C*
     C     DTKY1         KLIST
     C                   KFLD                    QQDTYP
     C*
     C*
     C*** DOCUMENT FILE
     C*
     C     DOKY1         KLIST
     C                   KFLD                    QQDCID
     C*
     C*
     C*
     C*
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDCID           12              BTCH ID
     C                   PARM                    QQBSTS            2              BTCH STS
     C                   PARM                    QQCPGM           10              CALLING PGM
     C                   PARM                    QQERRM           60              ERROR MESSAGE
     C                   PARM                    QQRTCD            1              RTN CODE
     C*
     C     QQRTCD        IFEQ      'L'
     C                   MOVE      *ON           *IN77
     C                   ENDIF
     C*
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C     DSDATE        MULT      10000.01      XXDT              6 0          YY/MM/DD
     C                   MOVE      DSTIME        XXTM              6 0
     C*
     C                   MOVE      XXDT          ##DATE            6
     C*
     C                   MOVEL     *BLANKS       QQRTCD
     C                   Z-ADD     *ZEROS        XXSDT             6 0
     C                   Z-ADD     *ZEROS        XXSTM             6 0
     C*
      *===============================================================
      * INITIALIZATION    AREA
      *
     C     TOP           TAG
     C                   EXSR      INZSR
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
     C*
     C*
      *================================================================
      *
     C     SCRLFL        IFEQ      *BLANKS
     C     SCRJFL        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN22
     C                   GOTO      MAINLN
     C                   ENDIF
      *
     C     SCRLFL        IFNE      '1'
     C     SCRJFL        ANDNE     '1'
     C                   MOVE      *ON           *IN23
     C                   GOTO      MAINLN
     C                   ENDIF
      *
     C*
      *
     C     SCRJFL        IFNE      *BLANKS
     C                   MOVEL     'DL'          QQBSTS
     C                   ELSE
     C                   MOVEL     'QC'          QQBSTS
     C                   ENDIF
      *
      *** Process all Docs for the Batch, Chg status of any NON-Indexed
      *** documents.
      *
     C**** CALL THE 722 PGM HERE???
     C*
     C*
     C                   CALL      'IS722R'
     C                   PARM                    QQDCID
     C                   PARM      QQBSTS        S#DSTS            2
     C                   PARM      'RJ'          @@BSTS            2
     C                   PARM      'IS240R  '    QQCPGM           10
     C                   PARM      *BLANK        QQRTNC            1
     C*
      *
     C*
      *===============================================================
      * TERMINATION OF PROGRAM
      *
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
     C/EJECT
      *====================================================================*
      * * *   INZSR SUBROUTINE                                    I N I T ***
      *====================================================================*
     C     INZSR         BEGSR
     C*
     C*
     C                   ENDSR
     C/EJECT
      *===============================================================
     C     SCREXC        BEGSR
      *===============================================================
      *
      *
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C*
     C                   EXFMT     WDW01
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
     C                   MOVE      '1'           *INLR
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END *INKC
      *
     C     *INKD         IFEQ      '1'
     C                   END
      *
      *
     C     *INKE         IFEQ      '1'
     C*
     C* * * PROCESS THE RE-SCAN OF A DOCUMENT HERE!!!!
     C*
     C*
     C*
     C* * *  CALL TO IS620 FOR THE RESCAN
     C*
     C     DOKY1         CHAIN     PDO01                              60
     C*
     C                   MOVEL     DOFLR         QQFLR             8
     C                   MOVEL     DOSFLR        QQSFLR           12
     C                   MOVEL     DODTYP        QQDTYP            8
     C     6             SUBST     DODCID:3      QQDSQ#            6
     C     3             SUBST     DODCID:10     QQDSFX            3
     C*
     C     DTKY1         CHAIN     PDT01                              60
     C                   MOVEL     DTRMCD        QQRMCD            5
     C*
     C*
     C                   CALL      'IS620C'
     C                   PARM                    QQDCID           12
     C                   PARM                    QQDTYP            8
     C                   PARM                    QQFLR
     C                   PARM                    QQSFLR
     C                   PARM                    QQDSQ#
     C                   PARM                    QQDSFX
     C                   PARM                    QQRMCD
     C                   PARM      *BLANKS       QQERRM           60
     C                   PARM      *BLANKS       Q$RTCD            6
     C*** BEG ADD *** 10/17/11 ****************************************
     C                   PARM      DOUNCN        QQUNCN          255
     C                   PARM      DOIFSP        QQIFSP            1
     C*** END ADD *** 10/17/11 ****************************************
     C*
     C     Q$RTCD        IFNE      *BLANKS
     C                   MOVEL     'E'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*
      *
     C**** CALL THE 722 PGM HERE??? UPDATES THE DOC STATUS', AND WRITES
     C****** DOC NOTES!!!
     C*
     C                   MOVEL     'QC'          QQBSTS
     C*
     C                   CALL      'IS722R'
     C                   PARM                    QQDCID
     C                   PARM      QQBSTS        S#DSTS            2
     C                   PARM      'SC'          @@BSTS            2
     C                   PARM      'IS240R  '    QQCPGM           10
     C                   PARM      *BLANK        QQRTNC            1
     C*
     C*
     C                   GOTO      TERM
     C*
     C*
     C                   END
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
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C*
     C                   END                                                    END *INKL
     C****************************************
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
     C*
      *
