     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE SC525R - CUSTOM GROUP ID/FIELD HEADING SEARCH            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*== 07/29/91 === LJD === PROJ: 90007/D0007 =====================*
     H*   Added processing to allow display of new field 35A          *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  20    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*****************************************************************
     FSC525F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     F*
     FSMCGL     IF   E           K DISK

     D*
     D*** FOR RECORD FORMAT A
     D FL              S              4    DIM(20) CTDATA PERRCD(20)            FIELD NAMES
     D AT              S              6    DIM(20) CTDATA PERRCD(10)            ATTRIBUTES
     D*** FOR RECORD FORMAT B
     D FB              S              4    DIM(6) CTDATA PERRCD(6)              FIELD NAMES
     D AB              S              6    DIM(6) CTDATA PERRCD(6)              ATTRIBUTES
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
     D  DS                     1    200
     D                                     DIM(20)                              USR ASGN FLD DSC
     D  CGDA01                 1     10
     D  CGDA02                11     20
     D  CGDA03                21     30
     D  CGDA04                31     40
     D  CGDA05                41     50
     D  CGDA06                51     60
     D  CGDA07                61     70
     D  CGDA08                71     80
     D  CGDA09                81     90
     D  CGDA10                91    100
     D  CGDD01               101    110
     D  CGDD02               111    120
     D  CGDD03               121    130
     D  CGDD04               131    140
     D  CGDD05               141    150
     D  CGDN01               151    160
     D  CGDN02               161    170
     D  CGDN03               171    180
     D  CGDN04               181    190
     D  CGDN05               191    200
     D*
     D*
     D                 DS
     D  TL                     1     20
     D                                     DIM(20)                              TABLE BAKUP FLDS
     D  CGTA01                 1      1
     D  CGTA02                 2      2
     D  CGTA03                 3      3
     D  CGTA04                 4      4
     D  CGTA05                 5      5
     D  CGTA06                 6      6
     D  CGTA07                 7      7
     D  CGTA08                 8      8
     D  CGTA09                 9      9
     D  CGTA10                10     10
     D  CGTD01                11     11
     D  CGTD02                12     12
     D  CGTD03                13     13
     D  CGTD04                14     14
     D  CGTD05                15     15
     D  CGTN01                16     16
     D  CGTN02                17     17
     D  CGTN03                18     18
     D  CGTN04                19     19
     D  CGTN05                20     20
     D                 DS
     D  DB                     1     60
     D                                     DIM(6)                               USR ASGN FLD DSC
     D  CGDB01                 1     10
     D  CGDB02                11     20
     D  CGDB03                21     30
     D  CGDB04                31     40
     D  CGDB05                41     50
     D  CGDB06                51     60
     D*
     D                 DS
     D  TB                     1      6
     D                                     DIM(6)                               TABLE BAKUP FLDS
     D  CGTB01                 1      1
     D  CGTB02                 2      2
     D  CGTB03                 3      3
     D  CGTB04                 4      4
     D  CGTB05                 5      5
     D  CGTB06                 6      6
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C*
     C*   KEY LIST FOR THE CUSTOM GROUP ID FILE
     C*
     C     CGKY          KLIST
     C                   KFLD                    SCCSID
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCSID            2
     C                   PARM                    QQFLD             4
     C                   PARM                    QQRTCD            1
     C*
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C*
     C     TOP           TAG
     C*
     C*   INITIALIZE ROUTINE * * *
     C                   EXSR      INIT
     C*
     C     CGKY          CHAIN     PCG01                              60
     C                   MOVE      CGRFMT        SCRFMT
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C*
     C     *IN60         IFEQ      '0'
     C*
     C     SCRFMT        IFEQ      'B'
     C                   EXSR      LOADB                                        LOAD RCDS FROM FILE
     C                   ELSE                                                   ELSE EQ 'B'
     C                   EXSR      LOADA
     C                   END                                                    END EQ 'B'
     C*
     C                   ELSE                                                   ELSE 0 *IN60
     C*
     C     SCRFMT        IFEQ      'B'
     C                   EXSR      BLANKB                                       LOAD BLANKS
     C                   ELSE                                                   ELSE EQ 'B'
     C                   EXSR      BLANKA
     C                   END                                                    END EQ 'B'
     C*
     C                   END                                                    END 0 *IN60
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF                          FOR DISPLAY
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01C
     C                   MOVE      '0'           *IN01
     C                   END
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
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01S'
     C     Q2RECF        OREQ      'SCR01C'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C                   END                                                    END *INKL
     C****************************************
     C*   *IN10 -  PROCESS HELP REQUESTS     *
     C****************************************
     C  N10
     CAN 10*IN10         IFEQ      '1'
     C  N10
     CAN 10              MOVE      '1'           *IN10
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *IN10
     C*
     C*
     C                   GOTO      STRT
     C                   END                                                    *IN11
     C****************************************
     C*  CHECK FOR SEARCH SELECTION
     C*
     C*
     C                   READC     SCR01S                                 50
     C     *IN50         IFEQ      '0'
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*** SAVE THAT DATA TO BE PASSED BACK TO THE CALLING PROGRAM
     C     SCSEL         IFEQ      '1'
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *BLANK        QQFLD
     C                   MOVEL     SCFLD         QQFLD
     C                   ELSE                                                   ELSE EQ '1'
     C*** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO INDICATE
     C*** A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C     SCSEL         IFNE      ' '
     C                   MOVE      '1'           *IN20
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END                                                    END NE ' '
     C                   END                                                    END EQ '1'
     C*** IF THE SUBFILE HAS NOT BEEN CHANGED, RETURN TO THE CALLING PROGRAM
     C*** WITHOUT A SELECTED RECORD
     C                   ELSE                                                   ELSE 0 *IN50
     C                   GOTO      TERM
     C                   END                                                    END 0 *IN50
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
      *====================================================================*
      * * *   INIT SUBROUTINE                                    I N I T ***
      *====================================================================*
     C     INIT          BEGSR
     C                   TIME                    DSDTTM
     C                   MOVE      '0'           XXOPN             1
     C                   MOVE      *HIVAL        @@TIME            6 0
     C                   Z-ADD     0             RRN2              5 0
     C                   Z-ADD     0             HLDRRN            5 0
     C                   Z-ADD     0             HLDRR2            5 0
     C                   Z-ADD     0             XXTIME            6 0
     C                   Z-ADD     0             XXDATE            6 0
     C                   Z-ADD     0             XXSEQ#            5 0
     C                   MOVE      *BLANK        SCDESC
     C                   MOVE      *BLANK        DS
     C                   MOVE      *BLANK        TL
     C                   MOVE      *BLANK        DB
     C                   MOVE      *BLANK        TB
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      QQCSID        SCCSID
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   ENDSR
     C*====================================================================*
     C* * *   LOADA SUBROUTINE                                 L O A D A ***
     C*====================================================================*
     C     LOADA         BEGSR
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 20 ITEMS
     C                   MOVEL     CGDESC        SCDESC
     C                   DO        20            RRN2              5 0
     C                   MOVEL     FL(RRN2)      SCFLD
     C                   MOVEL     AT(RRN2)      SCATTR
     C                   MOVEL     DS(RRN2)      SCHEAD
     C                   MOVEL     TL(RRN2)      SCTBUP
     C                   WRITE     SCR01S
     C                   END
     C                   Z-ADD     1             #@$2
     C                   ENDSR
     C*====================================================================*
     C* * *  BLANKA SUBROUTINE                              B L A N K A ***
     C*====================================================================*
     C     BLANKA        BEGSR
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 20 ITEMS
     C                   MOVEL     CGDESC        SCDESC
     C                   DO        20            RRN2              5 0
     C                   MOVEL     *BLANKS       SCFLD
     C                   MOVEL     *BLANKS       SCATTR
     C                   MOVEL     *BLANKS       SCHEAD
     C                   MOVEL     *BLANKS       SCTBUP
     C                   WRITE     SCR01S
     C                   END
     C                   Z-ADD     1             #@$2
     C                   ENDSR
     C*====================================================================*
     C* * *   LOADB SUBROUTINE                                  L O A D B***
     C*====================================================================*
     C     LOADB         BEGSR
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 6 ITEMS
     C                   MOVEL     CGDESC        SCDESC
     C                   DO        6             RRN2              5 0
     C                   MOVEL     FB(RRN2)      SCFLD
     C                   MOVEL     AB(RRN2)      SCATTR
     C                   MOVEL     DB(RRN2)      SCHEAD
     C                   MOVEL     TB(RRN2)      SCTBUP
     C                   WRITE     SCR01S
     C                   END
     C                   Z-ADD     1             #@$2
     C                   ENDSR
     C*====================================================================*
     C* * *  BLANKB SUBROUTINE                              B L A N K A ***
     C*====================================================================*
     C     BLANKB        BEGSR
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 6 ITEMS
     C                   MOVEL     CGDESC        SCDESC
     C                   DO        6             RRN2              5 0
     C                   MOVEL     *BLANKS       SCFLD
     C                   MOVEL     *BLANKS       SCATTR
     C                   MOVEL     *BLANKS       SCHEAD
     C                   MOVEL     *BLANKS       SCTBUP
     C                   WRITE     SCR01S
     C                   END
     C                   Z-ADD     1             #@$2
     C                   ENDSR
     C*
** FL = FIELD NAMES  (FORMAT A)
UA01UA02UA03UA04UA05UA06UA07UA08UA09UA10UD01UD02UD03UD04UD05UN01UN02UN03UN04UN05
** AT = FIELD ATTRIBUTES (FORMAT A)
10   A10   A10   A10   A10   A10   A10   A10   A10   A10   A
 6/0 N 6/0 N 6/0 N 6/0 N 6/0 N 9/2 N 9/2 N 9/2 N 9/2 N 9/2 N
** FB = FIELD NAMES (FORMAT B)
UB01UB02UB03UB04UB05UB06
** AB = FIELD ATTRIBUTES (FORMAT B)
35   A35   A35   A35   A35   A35   A
