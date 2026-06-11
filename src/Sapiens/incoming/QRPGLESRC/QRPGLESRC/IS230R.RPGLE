     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE IS230R - QUALITY CONTROL-VERIFY IMAGES                   *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON OPT FLAG FIELDS                                     *
     H*  20    INVALID OPTION ENTERED                                 *
     H*  21    INVALID                                                *
     H*  22    INVALID OPTION ENTERED                                 *
     H*  32    ROLL UP INDICATOR                                      *
     H*  40    GENERAL CHAIN                                          *
     H*  50    READ ON SCR01                                          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/20/12 C:62 PROJECT SY266                                   *
     H*          CORRECTED F8                                         *
     H*                                                               *
     H*****************************************************************
     FIS230F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     FIMDOL1    IF   E           K DISK
     FIMIEL7    IF   E           K DISK
     F                                     RENAME(PIE01:PIE07)
     FIMIEL8    UF   E           K DISK
     F                                     RENAME(PIE01:PIE08)
     FIMIEL5    UF   E           K DISK
     F                                     RENAME(PIE01:PIE05)
     FIMDNL2    IF   E           K DISK
     F*

     D STS             S              2    DIM(20)                              POSSIBLE BCH STS'
     D*
     D                 DS
     D*
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  IESDT                  1      8  0
     D  IESDYY                 1      4
     D  IESDMD                 5      8
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D NOTES           C                   CONST('**Notes**')
     D RELSD           C                   CONST('*RELEASED')
     D REJCT           C                   CONST('*REJECTED')

     C*
     C*   KEY LIST FOR THE DOCUMENT TYPE  FILE
     C*
     C     DTKY          KLIST
     C                   KFLD                    IEDOCT
     C*
     C*
     C*
     C     IEKY7         KLIST
     C                   KFLD                    QQDCID           12
     C*
     C*
     C*   KEY LIST FOR THE DOCUMENT TRANSACTION FILE
     C*
     C*
     C*
     C*
     C     BMKY1         KLIST
     C                   KFLD                    DOBCHI
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQRTCD            1
     C*
     C                   TIME                    DSDTTM
     C*
     C     *LIKE         DEFINE    RRN2          SRRN2
     C*
     C*
      /Free
       scPrDs = 'Quality Control - Verify Scanned Images';
      /End-Free
     C*
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C     TOP           TAG
     C*
     C                   MOVE      *BLANKS       NOUP
     C                   MOVE      *OFF          *IN88
     C*   INITIALIZE ROUTINE * * *
     C                   EXSR      INIT
     C*
     C*
     C     *LOVAL        SETLL     PIE07                              60
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C*
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C     STRT          TAG
     C                   MOVE      *BLANKS       NOUP
     C                   MOVE      *OFF          *IN88
     C*
     C                   MOVEL     *BLANK        SCPSEL
     C                   MOVEL     *BLANK        SCPDCT
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
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*******************************************************************
     C*
     C                   EXSR      CMDKYS
     C*
     C***********************************
     C     SCR01T        TAG
     C***********************************
     C*
     C*** IF THE SCPSEL IS SELECTED, THE SCPDCT MUST NOT BE BLANKS
     C*** , MUST BE VALID, THEN AUTO SELECT THIS ONE!
     C*
     C     SCPSEL        IFNE      *BLANKS
     C     SCPSEL        IFNE      '1'
     C     SCPSEL        ANDNE     '2'
     C     SCPSEL        ANDNE     '5'
     C     SCPSEL        ANDNE     '7'
     C     SCPSEL        ANDNE     '8'
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C*** EDIT THE SCPDCT FIELD TO SEE IF IT'S VALID, THEN SELECT IT
     C*
     C     SCPSEL        IFNE      *BLANKS
     C     SCPDCT        CHAIN     PIE08                              67
     C     *IN67         CABEQ     *ON           STRT                     21
     C*** ERROR IF RCD ALREADY LOCKED, CAN'T PROCESS IT!
     C     SCPSEL        IFEQ      '2'
     C     SCPSEL        OREQ      '5'
     C     IESTAT        IFEQ      'L'
     C                   UNLOCK    IMIEL8
     C                   MOVE      *ON           *IN28
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      'L'           IESTAT
     C                   UPDATE    PIE08
     C                   ENDIF
     C                   ELSE
     C                   UNLOCK    IMIEL8
     C                   ENDIF
     C*
     C*
     C                   MOVEL     SCPDCT        SLDCID           12
     C*
     C     SLDCID        CHAIN     PDO01                              64
     C     *IN64         IFEQ      *OFF
     C                   MOVEL     DOBCHI        ##BCHI
     C                   ENDIF
     C*
     C                   MOVE      *OFF          *IN88
     C                   MOVEL     *BLANKS       ERR
     C                   MOVEL     *BLANKS       SRERR             1
     C*
     C                   MOVEL     '1'           NOUP
     C*
     C*** PROCESS SELECTIONS 1,2,5,7,8....
     C     SCPSEL        CASEQ     '1'           SEL1
     C     SCPSEL        CASEQ     '2'           SEL2
     C     SCPSEL        CASEQ     '5'           SEL5
     C     SCPSEL        CASEQ     '7'           SEL7
     C     SCPSEL        CASEQ     '8'           SEL8
     C                   ENDCS
     C                   MOVE      *BLANKS       NOUP
     C*
     C                   GOTO      TOP
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
     C*
     C     SCPDCT        SETLL     PIE07
     C                   EXSR      LOAD
     C*
     C                   Z-ADD     1             #@$2
     C*
     C                   GOTO      STRT
     C                   ENDIF
     C**
     C* * * EDIT FIELDS
     C                   MOVE      '0'           ERR
     C                   MOVEL     *BLANKS       SRERR
     C*
     C*** SAVE RRN SO THAT ROLLING CAN PROPERLY FUNCTION IF COMPLETION IS MARKED
     C                   MOVE      RRN2          SRRN2
     C*
     C                   MOVEL     *BLANKS       ERR
     C                   READC     SCR01S                                 50
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C*** PROCESS SELECTIONS ***
     C*
     C                   MOVEL     IEDCID        SLDCID
     C                   MOVEL     SCBCHI        ##BCHI
     C                   MOVE      *OFF          *IN88
     C*
     C     SLDCID        CHAIN     PIE08                              67
     C*
     C                   MOVEA     '00000000'    *IN(20)
     C*** ERROR IF RCD ALREADY LOCKED, CAN'T PROCESS IT!
     C     SCSEL         IFEQ      '2'
     C     SCSEL         OREQ      '5'
     C     IESTAT        IFEQ      'L'
     C                   UNLOCK    IMIEL8
     C                   MOVE      *ON           *IN27
     C                   MOVE      '1'           ERR               1
     C                   UPDATE    SCR01S
     C*********************MOVEA'00000000'*IN,20
     C                   GOTO      NXT1
     C                   ELSE
     C                   MOVE      'L'           IESTAT
     C                   UPDATE    PIE08
     C                   ENDIF
     C                   ELSE
     C                   UNLOCK    IMIEL8
     C                   ENDIF
     C*
     C*
     C     SCSEL         CASEQ     '1'           SEL1
     C     SCSEL         CASEQ     '2'           SEL2
     C     SCSEL         CASEQ     '5'           SEL5
     C     SCSEL         CASEQ     '7'           SEL7
     C     SCSEL         CASEQ     '8'           SEL8
     C                   ENDCS
     C*
     C*
     C                   MOVEA     '00000000'    *IN(20)
     C     SCSEL         IFNE      ' '
     C     SCSEL         IFNE      '1'
     C     SCSEL         ANDNE     '2'
     C     SCSEL         ANDNE     '5'
     C     SCSEL         ANDNE     '7'
     C     SCSEL         ANDNE     '8'
     C                   MOVE      '1'           *IN20
     C                   MOVE      '1'           ERR               1
     C                   UPDATE    SCR01S
     C******************** MOVEA'00000000'*IN,20
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
     C*
     C*
     C                   MOVE      *BLANK        Q2RECF           10
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   MOVEA     '00000000'    *IN(20)
     C*
     C*                                                            TOGGLE
     C                   ENDSR
     C*
      *====================================================================*
      * * *   LOAD SUBROUTINE                                    L O A D ***
      *====================================================================*
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN89
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C                   DO        13            X                 5 0
     C*
     C                   READ      PIE07                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C                   ADD       1             RRN2
     C     IEDCID        CHAIN     PDO01                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     DOBCHI        SCBCHI
     C                   ELSE
     C                   MOVEL     *BLANKS       SCBCHI
     C                   ENDIF
     C                   MOVEL     *BLANKS       SCSEL
     C*
     C*** DETERMINE IF THERE ARE NOTES (NON-SYSTME) FOR THIS DOC  ***
     C*
     C     IEDCID        CHAIN     PDN01                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     NOTES         SCNOTE
     C                   ELSE
     C                   MOVE      *BLANKS       SCNOTE
     C                   ENDIF
     C*** CHANGE FORMAT OF DATE FOR DISPLAY
     C*
     C     IESDMD        CAT       IESDYY        XXMDY             8
     C*
     C                   MOVE      XXMDY         SCSDT
     C*
     C                   MOVE      IESTM         SCSTM
     C*
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01S
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANKS       IEDCID
     C                   MOVE      *BLANKS       SCBCHI
     C                   MOVE      *ZEROS        SCSDT
     C                   MOVE      *ZEROS        SCSTM
     C                   MOVE      *BLANKS       IEDOCT
     C                   MOVE      *BLANKS       SCNOTE
     C*
     C                   Z-ADD     1             RRN2
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01S
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     RRN2          #@$2
     C*
     C                   ENDSR
     C*
     C*
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*******************************************************************
     C*
     C     CMDKYS        BEGSR
     C*
     C     *IN11         IFEQ      '1'
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C****************************************
     C*   F5 = REFRESH                       *
     C****************************************
     C     *INKE         IFEQ      '1'
     C                   GOTO      TOP
     C                   END                                                    END *INKC
     C****************************************
     C*   F8 = PROCESS ALL DOCUMENTS         *
     C****************************************
     C     *INKH         IFEQ      '1'
     C*
     C                   MOVEL     '1'           NOUP              1
     C*** BEGIN ADD *** 11/20/12 *******************************************************************
     C                   z-add     1             scr01sRrn         9 0
     C*** END   ADD *** 11/20/12 *******************************************************************
     C*** BEGIN DEL *** 11/20/12 *******************************************************************
     C**   *LOVAL        SETLL     PIE08
     C**                 READ      PIE08                                  65
     C*** END   DEL *** 11/20/12 *******************************************************************
     C*
     C*** BEGIN ADD *** 11/20/12 *******************************************************************
     C     scr01sRrn     chain     scr01s                             65
     C*** END   ADD *** 11/20/12 *******************************************************************
     C     *IN65         DOWEQ     *OFF
     C*** BEGIN ADD *** 11/20/12 *******************************************************************
     C                   eval      scr01sRrn=scr01sRrn+1
     C*** END   ADD *** 11/20/12 *******************************************************************
     C     iedcid        chain     pie08
     C                   MOVEL     IEDCID        SLDCID
     C*
     C     SLDCID        CHAIN     PDO01                              64
     C     *IN64         IFEQ      *OFF
     C                   MOVEL     DOBCHI        ##BCHI
     C                   ENDIF
     C*
     C*** ERROR IF RCD ALREADY LOCKED, CAN'T PROCESS IT!
     C     IESTAT        IFEQ      'L'
     C                   UNLOCK    IMIEL8
     C                   GOTO      NXTF8
     C                   ELSE
     C                   MOVE      'L'           IESTAT
     C                   UPDATE    PIE08
     C                   ENDIF
     C*
     C                   EXSR      SEL5
     C*
     C     QQRTCD        IFEQ      'A'
     C     QQRTCD        OREQ      'B'
     C                   MOVEL     *BLANKS       QQRTCD
     C                   GOTO      EXTDO
     C                   ENDIF
     C*
     C     NXTF8         TAG
     C*
     C*** BEGIN DEL *** 11/20/12 *******************************************************************
     C**********         READ      PIE08                                  65
     C*** END   DEL *** 11/20/12 *******************************************************************
     C*** BEGIN ADD *** 11/20/12 *******************************************************************
     C     scr01sRrn     chain     scr01s                             65
     C*** END   ADD *** 11/20/12 *******************************************************************
     C                   ENDDO
     C*
     C     EXTDO         TAG
     C*
     C                   MOVEL     *BLANKS       NOUP
     C                   GOTO      TOP
     C*
     C                   END                                                    END *INKH
     C****************************************
     C*   F12 = PREVIOUS                     *
     C****************************************
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END *INKL
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C*
     C                   GOTO      STRT
     C                   END                                                    END CMDKEYS
     C*
     C                   ENDSR
     C*
     C*                                                                     ****
      *===================================================================*
      * * * SEL1 *  PROCESS SELECTION 1                               * * *
      *===================================================================*
     C*
     C     SEL1          BEGSR
     C*
     C                   MOVEA     '00000000'    *IN(20)
     C*
     C* * * SET CURSOR POS. ON SUBFILE ***
     C                   Z-ADD     RRN2          #@$2
     C*
     C                   CALL      'IS223R'
     C                   PARM      SLDCID        QQDID            12
     C                   PARM      ' '           QQRTCD            1
     C*
     C     NOUP          IFEQ      *BLANKS
     C                   MOVE      *BLANKS       SCSEL             1
     C*
     C                   MOVEA     '000000'      *IN(20)
     C                   UPDATE    SCR01S
     C                   ENDIF
     C*
     C     ESEL1         ENDSR
     C*                                                                     ****
      *===================================================================*
      * * * SEL2 *  PROCESS SELECTION 2                               * * *
      *===================================================================*
     C*
     C     SEL2          BEGSR
     C*
     C                   MOVEA     '00000000'    *IN(20)
     C* * * SET CURSOR POS. ON SUBFILE ***
     C                   Z-ADD     RRN2          #@$2
     C*
     C*_________________________________________________________________
     C*** FIND THE STATUS' TO SELECT!!!!!
     C*
     C                   MOVEL     'QC'          @@BSTS            2
     C                   MOVEL     'QC'          @1BSTS            2
     C                   MOVEL     *BLANKS       STS
     C                   MOVEA     'RLRJ    '    STS
     C***
     C                   CALL      'IS158R'
     C                   PARM                    SLDCID
     C                   PARM      '  '          S#DSTS            2
     C                   PARM      Q1PGM         XXPNAM           10
     C                   PARM      ' '           QQRTCD            1
     C     QQRTCD        IFEQ      *BLANKS
     C*
     C                   MOVE      *OFF          *IN88
     C**** CHANGE THE SCREEN FIELDS HERE TO SHOW THE ITEM PROCESSED.
     C     S#DSTS        IFEQ      'RL'
     C                   MOVEL     RELSD         SCNOTE
     C                   MOVE      *ON           *IN88
     C                   ENDIF
     C     S#DSTS        IFEQ      'RJ'
     C                   MOVE      *ON           *IN88
     C                   MOVEL     REJCT         SCNOTE
     C                   ENDIF
     C*
     C                   ELSE
     C     QQRTCD        IFEQ      'A'
     C*** END THE PROCESS NOW!!!
     C                   ENDIF
     C*___________________________________________________________________
     C                   ENDIF
     C*
     C     S2ERR         TAG
     C     NOUP          IFEQ      *BLANKS
     C*
     C                   MOVE      *BLANKS       SCSEL             1
     C*
     C                   UPDATE    SCR01S
     C                   ENDIF
     C*
     C*** UNLOCK RECORD *****
     C*
     C     SLDCID        CHAIN     PIE05                              58
     C     *IN58         IFEQ      *OFF
     C                   MOVEL     *BLANKS       IESTAT
     C                   UPDATE    PIE05
     C                   ENDIF
     C*
     C***  UPDATE THE BCH ID FROM DOC STATUS'
     C*
     C                   CALL      'IS723R'
     C                   PARM                    ##BCHI           12
     C                   PARM      'IS230R  '    ##CPGM           10
     C                   PARM      ' '           ##RTCD            1
     C*
     C     ESEL2         ENDSR
     C*                                                                     ****
      *===================================================================*
      * * * SEL5 *  PROCESS SELECTION 5                               * * *
      *===================================================================*
     C*
     C     SEL5          BEGSR
     C*
     C                   MOVEA     '00000000'    *IN(20)
     C*
     C*** PROCESS THE VIEW/VERIFY REQUEST
     C*
     C                   MOVE      *OFF          *IN88
     C                   MOVEA     '00000000'    *IN(20)
     C* * * SET CURSOR POS. ON SUBFILE ***
     C                   Z-ADD     RRN2          #@$2
     C*
     C     SLDCID        CHAIN     PDO01                              64
     C     *IN64         IFEQ      *OFF
     C*
     C                   MOVEL     DOBCHI        XXBCHI
     C*
     C                   MOVEL     *BLANK        SCPDCI           12
     C                   MOVEL     *BLANK        SCSEL
     C*
     C*** VIEW DOCUMENT *********
     C*
     C                   MOVEL     SLDCID        QVDCTB          120
     C                   MOVEL     Q1USER        QVUSER           10
     C                   MOVEL     Q1JOB         QVTRID           10
     C                   MOVEL     *BLANK        QVRTCD            6
     C*
     C                   MOVEL     SLDCID        XXDCID           12
     C*
     C     DOFLR         CAT       '\'           XX9A              9
     C     XX9A          CAT       DOSFLR        XXFOLD           80
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
     C                   MOVE      *ON           *IN25                              UNABLE TO PRC
     C                   MOVE      '1'           ERR               1
     C                   UPDATE    SCR01S
     C                   GOTO      ERR5
     C*
     C                   ENDIF
     C*_________________________________________________________________
     C*** FIND THE STATUS' TO SELECT!!!!!
     C*
     C                   MOVEL     'QC'          @@BSTS            2
     C                   MOVEL     'QC'          @1BSTS            2
     C                   MOVEL     *BLANKS       STS
     C                   MOVEA     'RLRJ    '    STS
     C*
     C***
     C***
     C                   CALL      'IS158R'
     C                   PARM                    SLDCID
     C                   PARM      '  '          S#DSTS            2
     C                   PARM      Q1PGM         XXPNAM           10
     C                   PARM      ' '           QQRTCD            1
     C     QQRTCD        IFEQ      *BLANKS
     C*
     C*
     C***
     C*
     C                   MOVE      *OFF          *IN88
     C**** CHANGE THE SCREEN FIELDS HERE TO SHOW THE ITEM PROCESSED.
     C     S#DSTS        IFEQ      'RL'
     C                   MOVEL     RELSD         SCNOTE
     C                   MOVE      *ON           *IN88
     C                   ENDIF
     C     S#DSTS        IFEQ      'RJ'
     C                   MOVE      *ON           *IN88
     C                   MOVEL     REJCT         SCNOTE
     C                   ENDIF
     C*
     C                   ELSE
     C     QQRTCD        IFEQ      'A'
     C*** END THE PROCESS NOW!!!
     C                   ENDIF
     C*___________________________________________________________________
     C                   ENDIF
     C                   ENDIF
     C*_________________________________________________________________
     C     NOUP          IFEQ      *BLANK
     C                   MOVE      *BLANKS       SCSEL             1
     C*
     C                   MOVEA     '00000000'    *IN(20)
     C                   UPDATE    SCR01S
     C                   MOVE      *OFF          *IN88
     C                   ENDIF
     C*
     C     ERR5          TAG
     C*** UNLOCK RECORD *****
     C*
     C     SLDCID        CHAIN     PIE05                              58
     C     *IN58         IFEQ      *OFF
     C                   MOVEL     *BLANKS       IESTAT
     C                   UPDATE    PIE05
     C                   ENDIF
     C*
     C*
     C***  UPDATE THE BCH ID FROM DOC STATUS'
     C*
     C                   CALL      'IS723R'
     C                   PARM                    ##BCHI           12
     C                   PARM      'IS230R  '    ##CPGM           10
     C                   PARM      ' '           ##RTCD            1
     C*
     C*
     C     ESEL5         ENDSR
     C*                                                                     ****
      *===================================================================*
      * * * SEL7 *  PROCESS SELECTION 7                               * * *
      *===================================================================*
     C*
     C     SEL7          BEGSR
     C*
     C                   MOVEA     '00000000'    *IN(20)
     C* * * SET CURSOR POS. ON SUBFILE ***
     C                   Z-ADD     RRN2          #@$2
     C*
     C*
     C                   CALL      'IS016R'
     C                   PARM                    SLDCID
     C                   PARM      'IS230R  '    XXPNAM           10
     C*
     C     NOUP          IFEQ      *BLANKS
     C                   MOVE      *BLANKS       SCSEL             1
     C*
     C                   MOVEA     '00000000'    *IN(20)
     C                   UPDATE    SCR01S
     C                   ENDIF
     C*
     C     ESEL7         ENDSR
     C*                                                                     ****
      *===================================================================*
      * * * SEL8 *  PROCESS SELECTION 8                               * * *
      *===================================================================*
     C*
     C     SEL8          BEGSR
     C*
     C                   MOVEA     '00000000'    *IN(20)
     C*
     C* * * SET CURSOR POS. ON SUBFILE ***
     C                   Z-ADD     RRN2          #@$2
     C*
     C     SLDCID        CHAIN     PDO01                              64
     C     *IN64         IFEQ      *OFF
     C*
     C                   MOVEL     DOBCHI        XXBCHI
     C*
     C                   CALL      'IS014R'
     C                   PARM                    XXBCHI           12
     C                   PARM      'IS230R  '    XXPNAM           10
     C*
     C                   ENDIF
     C*
     C     NOUP          IFEQ      *BLANKS
     C                   MOVE      *BLANKS       SCSEL             1
     C*
     C                   MOVEA     '00000000'    *IN(20)
     C                   UPDATE    SCR01S
     C                   ENDIF
     C*
     C     ESEL8         ENDSR
