     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE IS220R - BATCH DISPLAY/SELECT                            *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON OPT FLAG FIELDS                                     *
     H*  15    AFTER 1ST DISPLAY
     H*  20    INVALID OPTION ENTERED                                 *
     H*  21    INVALID INDEX QUEUE                                    *
     H*  22    INVALID OPTION ENTERED                                 *
     H*  32    ROLL UP INDICATOR                                      *
     H*  40    GENERAL CHAIN                                          *
     H*  50    READ ON SCR01                                          *
     H*  60    READ ON PDT01                                          *
     H*  91    ACTUAL # DOCS DOESN'T MATCH EXPECTED # DOCS            *
     H*  92    ACTUAL # PAGES DOESN'T MATCH EXPECTED # PAGES          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/30/04 C:062 Project SY100                                  *
     H*                Add Data Center Processing                     *
     H*                                                               *
     H*****************************************************************
     FIS220F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     F                                     SFILE(SCR02S:RRN3)
     F*
     FIMBML     IF   E           K DISK
     FIMBML1    IF   E           K DISK
     F                                     RENAME(PBM01:PBM02)
     FIMDOL3    IF   E           K DISK
     F                                     RENAME(PDO01:PDO02)
     FIMDOL1    IF   E           K DISK
     F                                     RENAME(PDO01:PDO03)
     FITBSL     IF   E           K DISK
     FITDSL     IF   E           K DISK
     F*

     D STS             S              2    DIM(20)                              POSSIBLE BCH STS'
     D CS              S              2    DIM(5) CTDATA PERRCD(1)              COMPILE ARRAYS STS
     D AS              S             40    DIM(5) ALT(CS)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  DOSDT                  1      8  0
     D  DOSDYY                 1      4
     D  DOSDMD                 5      8
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE BATCH ADMIN FILE
     C*
     C     DOKY          KLIST
     C                   KFLD                    SCID
     C*
     C*   KEY LIST FOR THE DOCUMENT TRNS  FILE
     C*
     C     DOKY1         KLIST
     C                   KFLD                    XXBCHI           12
     C                   KFLD                    XXDCID           12
     C*
     C*   KEY LIST FOR THE DOCUMENT TRNS  FILE
     C*
     C     DOKY2         KLIST
     C                   KFLD                    XXBCHI           12
     C*
     C*
     C*   KEY LIST FOR THE BATCH STATUS TABLE FILE
     C*
     C     BSKY1         KLIST
     C                   KFLD                    BMBSTS
     C*
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQWSID            2
     C                   PARM                    QQDOCT            8
     C                   PARM                    QQDCID           12
     C                   PARM                    QQDDSC           40
     C                   PARM                    QQBCHI           12
     C                   PARM                    QQRTCD            1
     C*
     C                   TIME                    DSDTTM
     C*
     C     *LIKE         DEFINE    RRN2          SRRN2
     C     *LIKE         DEFINE    RRN3          SRRN3
     C*
     C                   MOVEL     QQBCHI        SCID
     C*
      /Free
       scPrDs = 'Batch Administration';
      /End-Free
     C*
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C     TOP           TAG
     C*
     C*   INITIALIZE ROUTINE * * *
     C                   EXSR      INIT
     C*
     C     DOKY          SETLL     PBM01                              60
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C*
     C                   MOVEL     QQBCHI        SVBCHI           12
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     *BLANK        SCPDCT
     C                   MOVEL     *BLANK        SCPDCI           12
     C*
     C                   MOVEL     Q2FILE        SCFILE           10            SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF            8             FOR DISPLAY
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
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR02S'
     C                   MOVE      *ON           *IN01
     C                   WRITE     SCR02
     C                   EXFMT     SCR02C
     C                   MOVE      *OFF          *IN01
     C                   END
     C*
     C                   MOVE      *OFF          *IN15
     C                   MOVE      *OFF          *IN16
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
     C*
     C     Q2RECF        IFEQ      'SCR01   '
     C     Q2RECF        OREQ      'SCR01C  '
     C     Q2RECF        OREQ      'SCR01S  '
     C*
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C                   END                                                    END *INKC
     C****************************************
     C*   F12 = PREVIOUS                     *
     C****************************************
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01   '
     C     Q2RECF        OREQ      'SCR01C  '
     C     Q2RECF        OREQ      'SCR01S  '
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C     Q2RECF        IFEQ      'SCR02   '
     C     Q2RECF        OREQ      'SCR02C  '
     C     Q2RECF        OREQ      'SCR02S  '
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01   '    Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C*
     C                   END                                                    END *INKL
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C*
     C     Q2RECF        IFEQ      'SCR01   '
     C     Q2RECF        OREQ      'SCR01C  '
     C     Q2RECF        OREQ      'SCR01S  '
     C*
     C                   EXSR      LOAD
     C                   ENDIF
     C*
     C*
     C     Q2RECF        IFEQ      'SCR02   '
     C     Q2RECF        OREQ      'SCR02C  '
     C     Q2RECF        OREQ      'SCR02S  '
     C*
     C                   EXSR      LOAD2
     C                   ENDIF
     C*
     C                   GOTO      STRT
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C*
     C                   GOTO      STRT
     C                   END                                                    END CMDKEYS
     C**************************************************************************
     C     Q2RECF        CABEQ     'SCR01   '    SCR01T
     C     Q2RECF        CABEQ     'SCR01S  '    SCR01T
     C     Q2RECF        CABEQ     'SCR01C  '    SCR01T
     C     Q2RECF        CABEQ     'SCR02   '    SCR02T
     C     Q2RECF        CABEQ     'SCR02S  '    SCR02T
     C     Q2RECF        CABEQ     'SCR02C  '    SCR02T
     C***********************************
     C     SCR01T        TAG
     C***********************************
     C*
     C     SLCTIT        TAG
     C*
     C*** IF THE SCPSEL IS SELECTED, THE SCPDCT MUST NOT BE BLANKS
     C*** , MUST BE VALID, THEN AUTO SELECT THIS ONE!
     C*
     C     SCPSEL        IFNE      *BLANKS
     C     SCPSEL        CABNE     '1'           STRT                 2222
     C                   ENDIF
     C*
     C*** EDIT THE SCPDCT FIELD TO SEE IF IT'S VALID, THEN SELECT IT
     C     SCPSEL        IFNE      *BLANKS
     C     SCPDCT        CHAIN     PBM02                              67
     C     *IN67         CABEQ     *ON           STRT                     21
     C*
     C* * * ELSE VALID SELECTION - DISPLAY SCR02 STUFF HERE ***
     C                   MOVEL     BMBCHI        XXBCHI
     C                   MOVEL     BMBDSC        SCBDSC
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR02   '    Q2RECF
     C*
     C                   MOVEL     *BLANK        SCPDCI           12
     C*
     C     DOKY2         SETLL     PDO02
     C*
     C                   MOVEL     Q2RECF        SCRECF            8             FOR DISPLAY
     C                   MOVEL     XXBCHI        SCDBCH           12             FOR DISPLAY
     C*
     C*
     C*** CLEAR SUBFILE BEFORE LOAD ***
     C*
     C                   Z-ADD     0             RRN3              5 0
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR02C
     C                   MOVE      '0'           *IN04
     C*
     C*
     C                   EXSR      LOAD2
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C*
     C***** IF POSITION FIELDS ENTERED, PERFORM RE POSITION
     C     SCPDCT        IFNE      *BLANKS
     C*
     C* * * CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
     C*
     C                   MOVEL     SCPDCT        SVBCHI
     C*
     C     SCPDCT        SETLL     PBM01
     C                   EXSR      LOAD
     C*
     C                   GOTO      STRT
     C                   ENDIF
     C**
     C* * * EDIT FIELDS
     C                   MOVE      '0'           ERR
     C                   MOVEA     '0000'        *IN(20)
     C*
     C*** SAVE RRN SO THAT ROLLING CAN PROPERLY FUNCTION IF COMPLETION IS MARKED
     C                   MOVE      RRN2          SRRN2
     C*
     C                   READC     SCR01S                                 50
     C*
     C                   setoff                                       15
     C     *IN50         DOWEQ     '0'
     C*
     C*** PROCESS 1=BATCH DETAIL DISPLAY
     C     SCSEL         IFEQ      '1'
     C*
     C*
     C*
     C*
     C                   CALL      'IS222R'
     C                   PARM                    SCID
     C                   PARM      ' '           QQRTCD            1
     C*
     C                   MOVE      *BLANKS       SCSEL             1
     C     SC91          COMP      '1'                                    91
     C     SC92          COMP      '1'                                    92
     C                   UPDATE    SCR01S
     C*
     C                   MOVE      SRRN2         RRN2
     C*
     C                   GOTO      STRT
     C*
     C*
     C                   END                                                    END SEL EQ '1'
     C*
     C*
     C*** PROCESS 2=CHANGE BATCH STATUS
     C     SCSEL         IFEQ      '2'
     C*
     C*** FIND THE STATUS' TO SELECT!!!!!
     C*
     C                   MOVEL     SCBSTS        @@BSTS            2
     C                   MOVEL     SCBSTS        @1BSTS            2
     C                   EXSR      FNDSTS
     C*
     C********* PASS THE STATUS'THAT WOULD BE ALLOWED!
     C*
     C                   CALL      'IS155R'
     C                   PARM                    @1BSTS                         CURRENT STATUS
     C                   PARM                    STS                            POSSIBLE STS'
     C                   PARM      'IS220R  '    QQCPGM           10            CALLING PROGRAM
     C                   PARM      ' '           QQRTCD            1            RTN CODE
     C*
     C****** NOW THE STATUS IS SELECTED..... DECIDE WHAT TO DO!!!
     C*
     C*
     C     QQRTCD        IFEQ      *BLANKS
     C**** VALIDATE THE SELECTED STATUS**** IF NOT AVAILABLE
     C*** IE. INDEX IS ALREADY THERE, CAN'T DELETE....ETC,
     C*** DISPLAY ERRMSG (MAKE IT A VARIABLE ERROR MESSAGE)
     C*
     C*** GET THE BATCH RCD HERE AND UPDATE THE STATUS THEN SET IT UP
     C*** TO RELOAD, BY USING THE SEARCH FLDS.
     C*****      SCID      CHAINPBM03                60
     C**         *IN60     IFEQ *OFF
     C*                    MOVELSCBSTS    BMBSTS
     C*                    UPDATPBM03
     C******               ENDIF
     C*
     C                   CALL      'IS720R'
     C                   PARM      SCID          QQBCHI           12              BTCH ID
     C                   PARM      @1BSTS        QQBSTS            2              BTCH STS
     C                   PARM      *BLANKS       QQDOCT            8              DOC TYPE
     C                   PARM      *BLANKS       QQSREP           10              SCAN REP
     C                   PARM      *ZEROS        QQSDT             8              SCAN DT (A)
     C                   PARM      *ZEROS        QQSTM             6              SCAN TM (A)
     C                   PARM      *ZEROS        QQE#DC            5              EXP # DOCS (A)
     C                   PARM      *ZEROS        QQA#DC            5              ACT # DOCS(A)
     C                   PARM      *ZEROS        QQE#PG            5              EXP # PGS (A)
     C                   PARM      *ZEROS        QQA#PG            5              ACT # DOCS(A)
     C                   PARM      *ZEROS        QQ#PGP            3              # PGS PER (A)
     C                   PARM      *BLANKS       QQDSEP            1              USE DOC SEP?
     C                   PARM      'IS220R'      QQCPGM           10              CALLING PGM
     C                   PARM      *BLANKS       QQBERR           52              Batch Error
     C                   PARM      *BLANKS       QQBDSC           40              Batch DESC
     C                   PARM      *BLANKS       QQRTNC            1              RTN CODE
     C*
     C     QQRTNC        IFEQ      '1'
     C*** ERROR !!!
     C                   MOVE      '1'           *IN23
     C                   MOVE      '1'           ERR               1
     C     SC91          COMP      '1'                                    91
     C     SC92          COMP      '1'                                    92
     C                   UPDATE    SCR01S
     C                   GOTO      NXT1
     C                   ENDIF
     C*
     C                   MOVEL     QQBSTS        SCBSTS
     C*
     C* * * CALL THE PROGRAM (721) TO UPDATE THE IDIVIDUAL DOCUMENT RCDS
      * * *   DOCS ARE UPDATED ONLY WHEN THEY ARE NOT ALREADY INDEXED.
      * * *    ETC.
     C*
     C                   CALL      'IS721R'
     C                   PARM                    SCID
     C                   PARM                    SCBSTS
     C                   PARM                    @@BSTS
     C                   PARM      'IS220R  '    QQCPGM           10
     C                   PARM      *BLANK        QQRTCN            1
     C*
     C*** CHECK TO SEE IF STS WAS CHANGED PROPERLY?!!!!!!
     C*
     C*
     C                   MOVE      *BLANKS       SCSEL             1
     C     SC91          COMP      '1'                                    91
     C     SC92          COMP      '1'                                    92
     C                   UPDATE    SCR01S
     C*
     C                   MOVE      SRRN2         RRN2
     C*
     C                   MOVEL     SCID          SCPDCT
     C                   MOVEL     ' '           SCPSEL
     C                   GOTO      SLCTIT
     C*
     C                   ELSE
     C*
     C                   ENDIF
     C*
     C*
     C*
     C*
     C                   MOVE      *BLANKS       SCSEL             1
     C     SC91          COMP      '1'                                    91
     C     SC92          COMP      '1'                                    92
     C                   UPDATE    SCR01S
     C*
     C                   MOVE      SRRN2         RRN2
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                   END IF SEL=7
     C*
     C*** PROCESS 5=SELECT DOCUMENTS
     C     SCSEL         IFEQ      '5'
     C                   MOVEA     '000'         *IN(20)
     C* * * SET CURSOR POS. ON SUBFILE ***
     C                   Z-ADD     RRN2          #@$2
     C*
     C*
     C* * * VALID SELECTION - DISPLAY SCR02 STUFF HERE ***
     C                   MOVEL     SCID          XXBCHI
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR02   '    Q2RECF
     C*
     C     SCID          CHAIN     PBM02                              66
     C     *IN66         IFEQ      *OFF
     C                   MOVEL     BMBDSC        SCBDSC
     C                   ELSE
     C                   MOVEL     *BLANKS       SCBDSC
     C                   ENDIF
     C*
     C*
     C                   MOVEL     *BLANK        SCPDCI           12
     C*
     C     DOKY2         SETLL     PDO02
     C*
     C                   MOVEL     Q2RECF        SCRECF            8             FOR DISPLAY
     C                   MOVEL     XXBCHI        SCDBCH           12             FOR DISPLAY
     C*
     C*** CLEAR SELECT FIELD ON VALID CHOICE ***
     C*
     C                   MOVE      *BLANKS       SCSEL             1
     C     SC91          COMP      '1'                                    91
     C     SC92          COMP      '1'                                    92
     C                   UPDATE    SCR01S
     C*
     C                   MOVE      SRRN2         RRN2
     C*
     C*** CLEAR SUBFILE BEFORE LOAD ***
     C*
     C                   Z-ADD     0             RRN3              5 0
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR02C
     C                   MOVE      '0'           *IN04
     C*
     C                   EXSR      LOAD2
     C*
     C                   GOTO      STRT
     C*
     C                   END                                                    END SEL EQ '5'
     C*
     C*** PROCESS 7=BATCH NOTES
     C     SCSEL         IFEQ      '7'
     C*
     C                   CALL      'IS014R'
     C                   PARM                    SCID
     C                   PARM      'IS220R  '    XXPNAM           10
     C*
     C                   MOVE      *BLANKS       SCSEL             1
     C     SC91          COMP      '1'                                    91
     C     SC92          COMP      '1'                                    92
     C                   UPDATE    SCR01S
     C*
     C                   MOVE      SRRN2         RRN2
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                   END IF SEL=7
     C*
     C*
     C                   MOVEA     '000'         *IN(20)
     C     SCSEL         IFNE      ' '
     C     SCSEL         IFNE      '1'
     C     SCSEL         ANDNE     '2'
     C     SCSEL         ANDNE     '5'
     C     SCSEL         ANDNE     '7'
     C                   MOVE      '1'           *IN20
     C                   MOVE      '1'           ERR               1
     C     SC91          COMP      '1'                                    91
     C     SC92          COMP      '1'                                    92
     C                   UPDATE    SCR01S
     C                   END
     C                   END
     C*
     C     NXT1          TAG
     C*
     C                   READC     SCR01S                                 50
     C                   END
     C*
     C*** RESTORE RRN SO THAT ROLLING CAN PROPERLY FUNCTION
     C                   MOVE      SRRN2         RRN2
     C*
     C     ERR           CABEQ     '1'           STRT
     C*
     C*
     C                   GOTO      STRT
     C*
     C***********************************
     C     SCR02T        TAG
     C***********************************
     C*
     C* * * EDIT FIELDS
     C*
     C     SLCT2         TAG
     C*
     C*
     C*** IF THE SCPSEL IS SELECTED, THE SCPDCI MUST NOT BE BLANKS
     C*** , MUST BE VALID, THEN AUTO SELECT THIS ONE!
     C*
     C     SCPSEL        IFNE      *BLANKS
     C     SCPSEL        IFNE      '5'
     C     SCPSEL        ANDNE     '7'
     C     SCPSEL        ANDNE     '1'
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C*** EDIT THE SCPDCI FIELD TO SEE IF IT'S VALID, THEN SELECT IT
     C     SCPSEL        IFNE      *BLANKS
     C     SCPDCI        CHAIN     PDO03                              67
     C     *IN67         CABEQ     *ON           STRT                     21
     C*
     C     SCPSEL        IFEQ      '5'
     C* * * ELSE VALID SELECTION - DISPLAY DOC!!!!!!!!!!!   ***
     C                   MOVEL     DOBCHI        XXBCHI
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR02   '    Q2RECF
     C*
     C                   MOVEL     *BLANK        SCPDCI           12
     C                   MOVEL     *BLANK        SCPSEL
     C*
     C*** VIEW DOCUMENT *********
     C*
     C                   MOVEL     DODCID        QVDCTB          120
     C                   MOVEL     Q1USER        QVUSER           10
     C                   MOVEL     Q1JOB         QVTRID           10
     C                   MOVEL     *BLANK        QVRTCD            6
     C*
     C                   MOVEL     DODCID        XXDCID
     C*
     C     DOFLR         CAT       '\'           XX9A              9
     C*** BEGIN ADD *** 06/18/04 **************************************
     C                   CALL      'SA030R'
     C                   PARM      *BLANKS       ##PRFX            3
     C                   PARM      *BLANKS       ##PFLR           80
     C*
     C     ##PFLR        IFNE      *BLANKS
     C     ##PRFX        CAT(P)    '\':0         XXFOLD
     C                   CAT       XX9A:0        XXFOLD
     C                   CAT       DOSFLR:0      XXFOLD
     C                   ELSE
     C*** END   ADD *** 06/18/04 **************************************
     C     XX9A          CAT       DOSFLR        XXFOLD           80
     C*** BEGIN ADD *** 06/18/04 **************************************
     C                   ENDIF
     C*** END   ADD *** 06/18/04 **************************************
     C                   MOVE      DODSQ#        XXSQ#             5
     C                   MOVEL     DODKEY        XXKY             40
     C                   MOVEL     *BLANK        QVRTCD
     C*
     C                   CALL      'SCIMGV'                             60
     C                   PARM                    XXDCID           12
     C                   PARM                    XXFOLD
     C                   PARM                    XXKY
     C                   PARM                    XXSQ#
     C                   PARM                    DOOPTF
     C*
     C                   PARM                    QVRTCD
     C*
     C     QVRTCD        IFNE      '     0'
     C     QVRTCD        ANDNE     '000000'
     C     QVRTCD        ANDNE     '      '
     C*
     C                   MOVE      *ON           *IN68                              UNABLE TO PRC
     C*
     C                   ENDIF
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C***** IF POSITION FIELDS ENTERED, PERFORM RE POSITION
     C     SCPDCI        IFNE      *BLANKS
     C*
     C*
     C* * * VALID SELECTION - DISPLAY SCR02 STUFF HERE ***
     C                   MOVEL     SCDBCH        XXBCHI
     C                   MOVEL     SCPDCI        XXDCID
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR02   '    Q2RECF
     C*
     C                   MOVEL     *BLANK        SCPDCI           12
     C*
     C     DOKY1         SETLL     PDO02
     C*
     C                   MOVEL     Q2RECF        SCRECF            8             FOR DISPLAY
     C*
     C                   MOVE      *BLANKS       SCPSEL            1
     C*
     C*** CLEAR SUBFILE BEFORE LOAD ***
     C*
     C                   Z-ADD     0             RRN3              5 0
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR02C
     C                   MOVE      '0'           *IN04
     C*
     C                   EXSR      LOAD2
     C*
     C*
     C                   GOTO      STRT
     C                   ENDIF
     C**
     C* * * EDIT FIELDS
     C                   MOVE      '0'           ERR
     C*
     C*** SAVE RRN SO THAT ROLLING CAN PROPERLY FUNCTION IF COMPLETION IS MARKED
     C                   MOVE      RRN3          SRRN3
     C*
     C                   READC     SCR02S                                 50
     C*
     C     *IN50         DOWEQ     '0'
     C*** PROCESS 1=DOC   DETAIL DISPLAY
     C     SCSEL         IFEQ      '1'
     C*
     C                   MOVEA     '0000'        *IN(20)
     C* * * SET CURSOR POS. ON SUBFILE ***
     C                   Z-ADD     RRN3          #@$3
     C*
     C                   CALL      'IS223R'
     C                   PARM      SCDCID        QQDID            12
     C                   PARM      ' '           QQRTCD            1
     C*
     C                   MOVE      *BLANKS       SCSEL             1
     C*
     C                   MOVEA     '0000'        *IN(20)
     C                   UPDATE    SCR02S
     C*
     C*
     C                   END                                                    END SEL EQ '1'
     C*
     C*
     C*** PROCESS 2=CHANGE DOCUMENT STS
     C     SCSEL         IFEQ      '2'
     C*
     C                   MOVEA     '0000'        *IN(20)
     C* * * SET CURSOR POS. ON SUBFILE ***
     C                   Z-ADD     RRN3          #@$3
     C*
     C*
     C*** FIND THE STATUS' TO SELECT!!!!!
     C*
     C                   MOVEL     S#DSTS        @@BSTS            2
     C                   MOVEL     S#DSTS        @1BSTS            2
     C                   EXSR      FNDSTS
     C*
     C********* PASS THE STATUS'THAT WOULD BE ALLOWED!
     C*
     C                   CALL      'IS156R'
     C                   PARM                    @1BSTS                         CURRENT STATUS
     C                   PARM                    STS                            POSSIBLE STS'
     C                   PARM      'IS220R  '    QQCPGM           10            CALLING PROGRAM
     C                   PARM      ' '           QQRTCD            1            RTN CODE
     C*
     C****** NOW THE STATUS IS SELECTED..... DECIDE WHAT TO DO!!!
     C*
     C*
     C     QQRTCD        IFEQ      *BLANKS
     C**** VALIDATE THE SELECTED STATUS**** IF NOT AVAILABLE
     C*** IE. INDEX IS ALREADY THERE, CAN'T DELETE....ETC,
     C*** DISPLAY ERRMSG (MAKE IT A VARIABLE ERROR MESSAGE)
     C*
     C* * * CALL THE PROGRAM (721) TO UPDATE THE IDIVIDUAL DOCUMENT RCDS
      * * *   DOCS ARE UPDATED ONLY WHEN THEY ARE NOT ALREADY INDEXED.
      * * *    ETC.
     C*
     C                   CALL      'IS722R'
     C                   PARM                    SCDCID
     C                   PARM      @1BSTS        S#DSTS
     C                   PARM                    @@BSTS
     C                   PARM      'IS220R  '    QQCPGM           10
     C                   PARM      *BLANK        QQRTNC            1
     C*
     C*
     C     QQRTNC        IFNE      *BLANK
     C*** ERROR !!!
     C                   MOVEL     @@BSTS        S#DSTS
     C                   MOVE      '1'           *IN23
     C                   MOVE      '1'           ERR               1
     C                   UPDATE    SCR02S
     C                   GOTO      NXT2
     C                   ENDIF
     C*
     C*** CHECK TO SEE IF STS WAS CHANGED PROPERLY?!!!!!!
     C*
     C     S#DSTS        CHAIN     PDS01                              66
     C     *IN66         IFEQ      *OFF
     C                   MOVEL     DSSDSC        SCDSTS
     C                   ELSE
     C                   MOVEL     *BLANKS       SCDSTS
     C                   ENDIF
     C*
     C*
     C                   MOVE      *BLANKS       SCSEL             1
     C                   UPDATE    SCR02S
     C                   MOVE      SRRN3         RRN3
     C*
     C                   MOVEL     SCDCID        SCPDCI
     C                   MOVEL     ' '           SCPSEL
     C                   GOTO      SLCT2
     C*
     C                   ELSE
     C*
     C                   ENDIF
     C*
     C*
     C*
     C*
     C                   MOVE      *BLANKS       SCSEL             1
     C                   UPDATE    SCR02S
     C                   MOVE      SRRN3         RRN3
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                   END IF SEL=7
     C*
     C*
     C*** PROCESS 5=VIEW   DOCUMENTS
     C     SCSEL         IFEQ      '5'
     C                   MOVEA     '0000'        *IN(20)
     C* * * SET CURSOR POS. ON SUBFILE ***
     C                   Z-ADD     RRN3          #@$3
     C*
     C*
     C* * * VALID SELECTION - DISPLAY THE DODCUMENT HERE *
     C     SCDCID        CHAIN     PDO03                              67
     C     *IN67         CABEQ     *ON           STRT                     21
     C*
     C* * * ELSE VALID SELECTION - DISPLAY DOC!!!!!!!!!!!   ***
     C                   MOVEL     DOBCHI        XXBCHI
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR02   '    Q2RECF
     C*
     C                   MOVEL     *BLANK        SCPDCI           12
     C                   MOVEL     *BLANK        SCSEL
     C*
     C*** VIEW DOCUMENT *********
     C*
     C                   MOVEL     DODCID        QVDCTB          120
     C                   MOVEL     Q1USER        QVUSER           10
     C                   MOVEL     Q1JOB         QVTRID           10
     C                   MOVEL     *BLANK        QVRTCD            6
     C*
     C                   MOVEL     DODCID        XXDCID
     C*
     C     DOFLR         CAT       '\'           XX9A              9
     C*** BEGIN ADD *** 06/18/04 **************************************
     C                   CALL      'SA030R'
     C                   PARM      *BLANKS       ##PRFX
     C                   PARM      *BLANKS       ##PFLR
     C*
     C     ##PFLR        IFNE      *BLANKS
     C     ##PRFX        CAT(P)    '\':0         XXFOLD
     C                   CAT       XX9A:0        XXFOLD
     C                   CAT       DOSFLR:0      XXFOLD
     C                   ELSE
     C*** END   ADD *** 06/18/04 **************************************
     C     XX9A          CAT       DOSFLR        XXFOLD           80
     C*** BEGIN ADD *** 06/18/04 **************************************
     C                   ENDIF
     C*** END   ADD *** 06/18/04 **************************************
     C                   MOVE      DODSQ#        XXSQ#             5
     C                   MOVEL     DODKEY        XXKY             40
     C                   MOVEL     *BLANK        QVRTCD
     C*
     C                   CALL      'SCIMGV'                             60
     C                   PARM                    XXDCID           12
     C                   PARM                    XXFOLD
     C                   PARM                    XXKY
     C                   PARM                    XXSQ#
     C                   PARM                    DOOPTF
     C*
     C                   PARM                    QVRTCD
     C*
     C     QVRTCD        IFNE      '     0'
     C     QVRTCD        ANDNE     '000000'
     C     QVRTCD        ANDNE     '      '
     C*
     C                   MOVE      *ON           *IN68                              UNABLE TO PRC
     C*
     C                   ENDIF
     C                   UPDATE    SCR02S
     C*
     C                   END                                                    END SEL EQ '5'
     C*
     C*
     C*** PROCESS 7=DISPALY DOCUMENT NOTES
     C     SCSEL         IFEQ      '7'
     C*
     C                   MOVEA     '0000'        *IN(20)
     C* * * SET CURSOR POS. ON SUBFILE ***
     C                   Z-ADD     RRN3          #@$3
     C*
     C* * * VALID SELECTION - DISPLAY THE DODCUMENT NOTES HERE *
     C     SCDCID        CHAIN     PDO03                              67
     C     *IN67         CABEQ     *ON           STRT                     21
     C*
     C* * * ELSE VALID SELECTION - DISPLAY BATCH NOTES !!!!! **
     C*
     C*
     C                   CALL      'IS016R'
     C                   PARM                    SCDCID
     C                   PARM      'IS220R  '    XXPNAM           10
     C*
     C                   MOVE      *BLANKS       SCSEL             1
     C*
     C                   MOVEA     '0000'        *IN(20)
     C                   UPDATE    SCR02S
     C                   MOVE      SRRN3         RRN3
     C*
     C                   END                                                    END SEL EQ '7'
     C*
     C     SCSEL         IFNE      ' '
     C     SCSEL         IFNE      '5'
     C     SCSEL         ANDNE     '7'
     C     SCSEL         ANDNE     '1'
     C                   MOVE      '1'           *IN20
     C                   MOVE      '1'           ERR               1
     C                   UPDATE    SCR02S
     C                   END
     C                   END
     C*
     C     NXT2          TAG
     C*
     C                   READC     SCR02S                                 50
     C                   END
     C*
     C*** RESTORE RRN SO THAT ROLLING CAN PROPERLY FUNCTION
     C                   MOVE      SRRN3         RRN3
     C*
     C     ERR           CABEQ     '1'           STRT
     C*
     C*
     C                   GOTO      STRT
     C*
     C*
     C*
     C*** END OF PROGRAM
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
      *====================================================================*
      * * *   INIT SUBROUTINE                                    I N I T ***
      *====================================================================*
     C     INIT          BEGSR
     C                   Z-ADD     0             RRN2              5 0
     C                   Z-ADD     0             RRN3              5 0
     C*
     C*
     C                   MOVE      *BLANK        Q2RECF           10
     C                   MOVEL     'SCR01'       Q2RECF
     C*                                                            TOGGLE
     C                   ENDSR
      *====================================================================*
      * * *   LOAD SUBROUTINE                                    L O A D ***
      *====================================================================*
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C                   MOVE      '0'           *IN20
     C                   MOVEL     SVBCHI        $$BCHI           12
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C                   DO        13            X                 5 0
     C*
     C                   READ      PBM01                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C*
     C                   ADD       1             RRN2
     C*** TO POSITION SCREEN AND CURSOR
     C     X             IFEQ      1
     C                   Z-ADD     RRN2          #@$2
     C                   ENDIF
     C                   MOVEL     BMDOCT        SCDCTP
     C*** GET BTCH STS TO DISPLAY
     C     BSKY1         CHAIN     PBS01                              63
     C     *IN63         IFEQ      *OFF
     C                   MOVEL     BSSDSC        SCSTAT
     C                   ELSE
     C                   MOVEL     *BLANKS       SCSTAT
     C                   MOVEL     BMBSTS        SCSTAT
     C                   ENDIF
     C                   MOVEL     BMBCHI        SCID
     C                   MOVEL     BMSREP        SCSUSR
     C                   MOVEL     *BLANKS       SCSEL
     C  N60
     CAN 60              EXSR      COUNT
     C                   MOVE      BMA#DC        SCA#DC
     C                   MOVE      BMA#PG        SCA#PG
     C                   MOVEL     BMBSTS        SCBSTS
     C* * CHECK EXPECTED VS. ACTUAL VALUES ***
     C*
     C     BMA#DC        IFNE      BME#DC
     C                   MOVE      *ON           *IN91
     C                   MOVE      '1'           SC91
     C                   ELSE
     C                   MOVE      *OFF          *IN91
     C                   MOVE      ' '           SC91
     C                   ENDIF
     C*
     C     BMA#PG        IFNE      BME#PG
     C                   MOVE      *ON           *IN92
     C                   MOVE      '1'           SC92
     C                   ELSE
     C                   MOVE      *OFF          *IN92
     C                   MOVE      ' '           SC92
     C                   ENDIF
     C*
     C     RRN2          IFEQ      1
     C     *IN91         IFEQ      *ON
     C                   MOVE      *ON           *IN15
     C                   ENDIF
     C     *IN92         IFEQ      *ON
     C                   MOVE      *ON           *IN16
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL     BMBCHI        $$BCHI           12
     C*
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01S
     C*
     C                   MOVE      *OFF          *IN91
     C                   MOVE      *OFF          *IN92
     C*
     C*
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C*
     C                   MOVE      *OFF          *IN91
     C                   MOVE      *OFF          *IN92
     C*
     C*
     C                   END
     C                   END
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANKS       SCID
     C                   MOVEL     *BLANKS       SCDCTP
     C                   MOVEL     *BLANKS       SCSTAT
     C                   MOVEL     *BLANKS       SCID
     C                   MOVEL     *BLANKS       SCSUSR
     C                   MOVEL     *BLANKS       SCA#DC
     C                   MOVEL     *BLANKS       SCA#PG
     C                   MOVEL     *BLANKS       SCSEL
     C                   MOVEL     *BLANKS       SCBSTS
     C*
     C                   MOVE      *OFF          *IN91
     C                   MOVE      *OFF          *IN92
     C*
     C*
     C                   Z-ADD     1             #@$2
     C                   Z-ADD     1             RRN2
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01S
     C                   END
     C*
     C*
     C                   ENDSR
     C*
      *====================================================================*
      * * *   COUNT  SUBROUTINE                                C O U N T ***
      *====================================================================*
     C     COUNT         BEGSR
     C                   Z-ADD     0             DOCCNT            6 0
     C*
     C*
     C                   MOVEL     DOBCHI        XXBCHI
     C                   MOVEL     DODCID        XXDCID
     C*
     C     DOKY1         CHAIN     PDO02                              61
     C     *IN61         DOWEQ     *OFF
     C                   ADD       1             DOCCNT
     C     DOKY2         READE     PDO02                                  61
     C                   ENDDO
     C*
     C                   ENDSR
     C*
      *====================================================================*
      * * *   FNDSTS SUBROUTINE - FIND VALID STATUS'         F N D S T S ***
      *====================================================================*
     C     FNDSTS        BEGSR
     C*
     C*** CLEAR STS ARRAY
     C                   MOVE      *BLANKS       STS
     C*
     C* * * FIND THE VALID STATUS'
     C*
     C*** IF CURRENT BATCH STATUS IS X THEN ALLOW .....
     C                   Z-ADD     1             X
     C     @@BSTS        LOOKUP    CS(X)                                  60
     C   60              MOVEA     AS(X)         STS
     C  N60              MOVE      *BLANKS       STS
     C*
     C*
     C                   ENDSR
     C*
      *====================================================================*
      * * *  LOAD2 SUBROUTINE                                  L O A D 2 ***
      *====================================================================*
     C     LOAD2         BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C                   MOVE      '0'           *IN20
     C                   MOVE      *OFF          *IN92
     C*
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C                   DO        13            X                 5 0
     C*
     C     DOKY2         READE     PDO02                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C*
     C                   ADD       1             RRN3
     C*** TO POSITION SCREEN AND CURSOR
     C     X             IFEQ      1
     C                   Z-ADD     RRN3          #@$3
     C                   ENDIF
     C                   MOVEL     DODCID        SCDCID
     C                   MOVE      DOBYTD        SCBYTD
     C*
     C*** FORMAT THE DATE FIELD FOR OUTPUT TO SCREEN ***
     C*
     C     DOSDMD        CAT       DOSDYY        XXMDY             8
     C*
     C*
     C                   MOVE      XXMDY         SCSDT
     C****                 MOVE DOSTMD    SCSTM
     C***                  MOVELDOSREP    SCSUSR
     C                   MOVE      DOPGS         SCPGS
     C                   MOVEL     DOOPTF        SCOPTF
     C                   MOVEL     DOIDFX        SCIDXF
     C                   MOVEL     DODTYP        SCDTYP
     C                   MOVEL     DODSTS        S#DSTS
     C*
     C     DODSTS        CHAIN     PDS01                              66
     C     *IN66         IFEQ      *OFF
     C                   MOVEL     DSSDSC        SCDSTS
     C                   ELSE
     C                   MOVEL     *BLANKS       SCDSTS
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       SCSEL
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR02S
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C*
     C                   END
     C                   END
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN3          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVEL     *BLANKS       SCDCID
     C                   MOVE      *BLANKS       SCBYTD
     C                   MOVE      *BLANKS       SCSDT
     C*****                MOVE *BLANKS   SCSTM
     C                   MOVEL     *BLANKS       SCSUSR
     C                   MOVE      *BLANKS       SCPGS
     C                   MOVEL     *BLANKS       SCOPTF
     C                   MOVEL     *BLANKS       SCIDXF
     C                   MOVEL     *BLANKS       SCDTYP
     C*
     C                   MOVEL     *BLANKS       SCDSTS
     C*
     C*
     C                   Z-ADD     1             #@$3
     C                   Z-ADD     1             RRN3
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR02S
     C                   END
     C*
     C*
     C                   ENDSR
     C*
**
SCRLQC
RLRJCM
QCRLRJ
RJRLQCDL
DLQC
