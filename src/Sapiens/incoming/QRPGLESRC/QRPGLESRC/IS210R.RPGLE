     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE IS210R - IMAGE INDEXING                                  *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:       1                                                *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON OPT FLAG FIELDS                                     *
     H*  20    INVALID OPTION ENTERED                                 *
     H*  21    INVALID DOC TYPE                                       *
     H*  22    INVALID OPTION ENTERED                                 *
     F*  23    TO CHECK FOR DRFL15 BEING "Y"                          *
     H*  32    ROLL UP INDICATOR                                      *
     H*  40    GENERAL CHAIN                                          *
     H*  50    READ ON SCR01                                          *
     H*  60    READ ON PDT01                                          *
     H*****************************************************************
     H* REVISIONS
     *                                                               *
     H*  03/11/17 C:62  Support 46110(114)
     H*                 Don't display queues that are inactive.
     *                                                               *
     H*  10/10/12 C:33  Project: SY266
     H*                 Do not update subfile if not read.
     F*                                                               *
     H*  10/17/11 C:279 Project: SY195
     H*                 Modify program to pass/return UNC path field
     H*                 and IFS flag to called/calling programs.
     H*
     F* 07/09/07 C:191 PROJECT: 10IJI                                 *
     F*               -CHANGED THE SEQUENCE OF PROCESSING IN THE ORDER*
     F*                OF RECEIEVD DATE WHEN SYSTEM CONTROL FLAG IS   *
     F*                "Y".                                           *
     H*                                                               *
     H*** 1/3/07  C: 02 AOOPTICAL
     F**                REMOVE DATACENTER PROCESSING (PREFIX) AS IT
     F**                SHOULD NOT BE USED IN THIS PROGRAM.
     H* 08/30/05 C:062 Project IM129 48767                            *
     H*                Fix paging issue.                              *
     H*                                                               *
     H* 06/18/04 C:062 Project SY100                                  *
     H*                Add Data Center Processing                     *
     H*                                                               *
     H* 04/12/04 C:062 Project 114MEX                                 *
     H*                Added assign pgm process.                      *
     H*                                                               *
     H* 04/02/04 C:128 Project IM129 Support # 41754                  *
     H*                With the advent of several scan stations       *
     H*                scanning the same document type just the       *
     H*                Document ID was not enough to insure that      *
     H*                the queue pas being processed FIFO. Added      *
     H*                Scan Date and Scan Time to the new logical.    *
     H*                                                               *
      * 06/04/02 C:87  Project: DR170
      *                Added a function key, F8, to display/hide the
      *                number of documents in an indexing queue.  The
      *                # of docs is initially not calculated but if the
      *                user presses F8, the number of docs (for those
      *                queues that have docs) is displayed.  You can
      *                hide this value by pressing F8 again.
      *
      * 07/10/00 C:113 Project: Support
      *                Corrected more date conversions.
      *
      *
      * 01/27/00 C:113 PROJECT: DR095                                 *
      *                Changed to use the Work With Key Types program *
      *                (SC092R) for key type searches.                *
      *                                                               *
      * 01/04/00 C:113 PROJECT: SUPPORT
      *                Corrected date conversion.
      *
      *****************************************************************
     FIS210F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     F*
     FIMDTL     IF   E           K DISK
     FIMDTL1    IF   E           K DISK
     F                                     RENAME(PDT01:PDT02)
     FIMIEL9    UF   E           K DISK
     F                                     INFDS(FILDS1)
     FIMIELI    IF   E           K DISK
     F                                     RENAME(PIE01:PIE02)
     FIMIQL     IF   E           K DISK
     FIMDOL1    UF   E           K DISK
     FIMBML     UF   E           K DISK
     FIMDOL4    IF   E           K DISK
     F                                     RENAME(PDO01:PDO04)
     FIMIEL6    UF   E           K DISK
     F                                     RENAME(PIE01:PIE06)
     FIMDOP1    O    E             DISK
     FIMIELC    IF   E           K DISK
     F                                     RENAME(PIE01:PIE0C)
     F* LOGICAL OF IMIEP TO FEED IMAGES IN THE ORDER OF RCVD DATE
     FIMIELQ    UF   E           K DISK
     F                                     RENAME(PIE01:PIE0Q)
     F*** LOGICAL OF SDRPF
     FSSYLF     IF   E           K DISK
     F*
     F** Key Type File
     FSMKTP     IF   E             DISK

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D FILDS1          DS
     D  FIL1             *FILE
     D  REC1             *RECORD
     D  OP1              *OPCODE
     D  STS1             *STATUS
     D  RTN1             *ROUTINE

     C*
     C*   KEY LIST FOR THE DOCUMENT TYPE  FILE
     C*
     C     DTKY          KLIST
     C                   KFLD                    SCDOCT
     C*
     C*
     C*   KEY LIST FOR THE IMAGE QUEUE  FILE
     C*
     C     IQKY          KLIST
     C                   KFLD                    QQQID
     C*
     C*
     C*   KEY LIST FOR THE IMAGE INDEXING FILE
     C*
     C     IEKY          KLIST
     C                   KFLD                    QQQID
     C*
     C*
     C*
     C     IEKY1         KLIST
     C                   KFLD                    QQQID
     C                   KFLD                    SCDCID
     C*
     C*
     C*   KEY LIST FOR THE DOCUMENT TRANSACTION FILE
     C*
     C     DOKY          KLIST
     C                   KFLD                    SCDCID
     C*
     C*
     C*
     C     BMKY1         KLIST
     C                   KFLD                    DOBCHI
     C*
     C*   KEY LIST FOR THE DOCUMENT TRANSACTION FILE
     C*
     C     DOKY4         KLIST
     C                   KFLD                    BMBCHI
     C*
     * KEY LIST FOR THE SYSTEM CONTROL FILE
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQRTCD            1
     C*
     C     ASSPL         PLIST
     C     QQQID         PARM      QQQID         $$QID            10            10A Q ID
     C                   PARM      *BLANKS       $$USER           10            10A USER
     C                   PARM      IESDT         $$SDT             8 0           80 SCNDTMDY
     C                   PARM      IESTM         $$STM             6 0           60 SCTIM
     C                   PARM      IEDCID        $$DCID           12            12A DCID
     C                   PARM                    $$DESC           30            30A DC DSC
     C                   PARM      IEDOCT        $$DOCT            8             8A DOCTYP
     C                   PARM      IEFL01        $$FL01            1             1A IMG/FAX
     C                   PARM                    $$PTH            63            63A FULL PTH
     C                   PARM                    $$FLR             8             8A FOLD
     C                   PARM                    $$SFLR           12            12A SUBFLR
     C     QQRTCD        PARM      QQRTCD        $$RTCD            1             1A RTNCODE
      *
      ** Date check/conversion parameters
     C     DTYMD         PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*YMD'        SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     ACDPL1        PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      '2'           ##VIEW            1
      /Free
       scPrDs = 'Select Document Type for Indexing';
      /End-Free
     C*
     C                   TIME                    DSDTTM
     C*
     C     *LIKE         DEFINE    RRN2          SRRN2
     C*
     C*
     C*
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C     TOP           TAG
     C*
     C*   INITIALIZE ROUTINE * * *
     C                   EXSR      INIT
     C*
     C     DTKY          SETLL     PDT01                              60
     C*
     C*   CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C*
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C     STRT          TAG
     C*
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
     C                   Z-ADD     Q2LSFR        RRNTRK            5 0
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
     C*   IF THE SCPSEL IS SELECTED, THE SCPDCT MUST NOT BE BLANKS
     C*   , MUST BE VALID, THEN AUTO SELECT THIS ONE!
     C*
     C     SCPSEL        IFNE      *BLANKS
     C     SCPSEL        CABNE     '1'           STRT                 2222
     C                   ENDIF
     C*
     C*   EDIT THE SCPDCT FIELD TO SEE IF IT'S VALID, THEN SELECT IT
     C*
     C     SCPSEL        IFNE      *BLANKS
     C     SCPDCT        CHAIN     PDT02                              67
     C     *IN67         CABEQ     *ON           STRT                     21
     C                   MOVEL     DTDOCT        SCDOCT
     C                   MOVE      *BLANKS       QQRTCD
     C                   GOTO      VWRQS
     C                   ENDIF
     C*
     C*
     C* IF POSITION FIELDS ENTERED, PERFORM RE POSITION
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
     C     SCPDCT        SETLL     PDT01
     C                   EXSR      LOAD
     C*
     C                   Z-ADD     1             #@$2
     C*
     C                   GOTO      STRT
     C                   ENDIF
     C**
     C* * * EDIT FIELDS
     C                   MOVE      '0'           ERR
     C*
     C* SAVE RRN SO THAT ROLLING CAN PROPERLY FUNCTION IF COMPLETION IS MA  RKED
     C                   MOVE      RRN2          SRRN2
     C*
     C                   READC     SCR01S                                 50
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C* PROCESS 1=SELECT DOCUMENTS
     C     SCSEL         IFEQ      '1'
     C                   MOVEA     '000'         *IN(20)
     C*
     C*
     C                   MOVEL     SCDOCT        QQDOCT            8
     C                   MOVEL     SCDESC        QQDESC           40
     C                   MOVEL     ' '           QQRTCD
     C                   GOTO      VWRQS
     C*
     C                   MOVE      *BLANK        SCSEL
     C                   UPDATE    SCR01S
     C*
     C                   END                                                    END SEL EQ '5'
     C*
     C                   MOVEA     '000'         *IN(20)
     C     SCSEL         IFNE      ' '
     C     SCSEL         IFNE      '1'
     C                   MOVE      '1'           *IN20
     C                   MOVE      '1'           ERR               1
     C                   UPDATE    SCR01S
     C                   END
     C                   END
     C*
     C     NXT1          TAG
     C*
     C                   READC     SCR01S                                 50
     C                   END
     C*
     C* RESTORE RRN SO THAT ROLLING CAN PROPERLY FUNCTION
     C                   MOVE      SRRN2         RRN2
     C*
     C     ERR           CABEQ     '1'           STRT
     C*
     C*
     C                   GOTO      STRT
     C*
     C     VWRQS         TAG
     C*
     C     DTKY          CHAIN     PDT01                              60
     C*
     C                   MOVEL     DTDOCT        SCDOCT
     C                   MOVEL     DTDESC        SCDDSC
     C                   MOVEL     DTDQUE        QQQID
     C                   MOVEL     *BLANKS       IDXPGM
     C                   MOVEL     DTIDXP        IDXPGM           10            INDEX PGM
     C                   MOVEL     DT10A1        ASSPGM           10            ASSIGN PGM
     C*
     C     IQKY          CHAIN     PIQ01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     IQDESC        SCQDSC           40
     C                   ELSE
     C                   MOVEL     *BLANKS       SCQDSC
     C                   ENDIF
     C*
     C*
     C     BMKY1         CHAIN     PBM01                              60
     C     *IN60         IFEQ      *OFF
     C                   UNLOCK    IMBML
     C                   MOVEL     BMBDSC        SCBDSC           40
     C                   ELSE
     C                   MOVEL     *BLANKS       SCBDSC
     C                   ENDIF
     C*
     C  N23IEKY          SETLL     PIE01                              60
     C   23IEKY          SETLL     PIE0Q                              60
     C     NXTIE         TAG
     C                   MOVEL     *BLANKS       IDXPGM
     C                   MOVEL     DTIDXP        IDXPGM           10            INDEX PGM
     C  N23IEKY          READE     PIE01                                  60
     C   23IEKY          READE     PIE0Q                                  60
     C     *IN60         IFEQ      *OFF
     C     IESTAT        CABEQ     'L'           NXTIE
     C                   MOVE      'L'           IESTAT
     C  N23              UPDATE    PIE01
     C   23              UPDATE    PIE0Q
     C*
     C                   MOVEL     IEDCID        SCDCID           12
     C                   Z-ADD     IESDT         SRDTI
     C                   CALL      'ISCVTDT'     DTMDYY
     C                   MOVEL     SRDTO         SCSDAT
     C                   MOVE      IESTM         SCSTIM
     C                   MOVEL     IESREP        SCSREP            8
     C*
     C                   ELSE
     C*
     C                   MOVE      *ZEROS        SCSDAT
     C                   MOVE      *ZEROS        SCSDAT
     C                   MOVE      *ZEROS        SCSTIM
     C                   MOVEL     *BLANKS       SCSREP
     C* NO MORE RECORDS TO PROCESS FOR THIS DOCUMENT TYPE
     C                   MOVE      *ON           *IN90
      /Free
       If srrn2 <> *zero;
      /End-free
     C                   MOVE      SRRN2         RRN2
     C                   MOVE      *BLANK        SCSEL
     C                   UPDATE    SCR01S
     C                   GOTO      TOP
      /Free
       Endif;
      /End-free
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     DOKY          CHAIN     PDO01                              60
     C     *IN60         IFEQ      *OFF
     C                   UNLOCK    IMDOL1
     C*
     C                   MOVEL     DOFLR         XXFLR             8
     C                   MOVEL     DOSFLR        XXSFLR           12
     C     XXFLR         CAT       '/'           XX9A              9
     C     XX9A          CAT       XXSFLR        SCPTH
     C*
     C                   MOVE      DOPGS         SC#PGS
     C                   MOVEL     DOBCHI        SCBCHI
     C                   MOVE      DOUNCN        QQUNCN          255
     C                   MOVE      DOIFSP        QQIFSP            1
     C                   ELSE
     C                   MOVEL     *BLANKS       SCPTH
     C*
     C                   MOVE      *ZEROS        SC#PGS
     C                   MOVEL     *BLANKS       SCBCHI
     C                   MOVE      *BLANKS       QQUNCN          255
     C                   MOVE      *BLANKS       QQIFSP            1
     C                   ENDIF
      *
      *** CALL PROGRAM TO PROCESS THE VIEW REQUEST
      *
     C                   CALL      'IS211C'
     C                   PARM                    SCDCID           12
     C                   PARM                    XXFLR             8
     C                   PARM                    XXSFLR           12
     C                   PARM                    SCDOCT
     C                   PARM                    SCDDSC
     C                   PARM                    QQRTN             6
      *
      *** CALL INDEX PROGRAM TO PERFORM INDEX OF THE DOCUMENT
      *
     C     IDXDSP        TAG
      *
     C                   CALL      IDXPGM
     C                   PARM                    SCDCID           12
     C                   PARM                    SCPTH            40
     C                   PARM                    SCDOCT            8
     C                   PARM                    SCDDSC           40
     C                   PARM                    QQQID            10
     C                   PARM                    SCQDSC           40
     C                   PARM                    SCBCHI           12
     C                   PARM                    SC#PGS            6 0
     C                   PARM                    SCSDAT            8 0
     C                   PARM                    SCSTIM            6 0
     C                   PARM                    SCSREP            8
     C                   PARM                    XXKEY            40
     C                   PARM                    XXSEQ#            5 0
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQUNCN
     C                   PARM                    QQIFSP
      *
      ** PROCESS THE COMMAND KEYS 1ST!!!!
      ** WE MUST UNLOCK THE IE RECORD IF WE DONT USE IT!!!!!
      *
      ** UPDATE THE DO RCD AND THE IE RCD THEN TRY FOR ANOTHER!!!!
      *
      * * * GO AGAIN.... NEXT DOC TO BE INDEXED FOR THE PRIOR SELECTED
      * * * DOC TYPE.
      *
      **  UNLOCK  RCDS ETC.
      *
     C     QQRTCD        IFEQ      *BLANK
     C     XXKEY         ANDNE     *BLANK
     C                   MOVEL     QQQID         HDQID            10
     C                   CALL      ASSPGM        ASSPL                  99
     C*
     C     IEKY1         CHAIN     PIE06                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     HDQID         QQQID
     C*
&&&  C     QQRTCD        IFNE      'R'
     C                   MOVE      'D'           IESTAT
     C                   MOVE      'CM'          IEDSTS
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'ISCVTDT'     DTYMD
     C                   MOVEL     SRDTO         XXDT              6 0
     C                   Z-ADD     XXDT          IELDT
     C                   Z-ADD     DSTIME        IELTIM
     C                   MOVEL     Q1USER        IELUSR
     C                   ENDIF
     C                   UPDATE    PIE06
      * * * UPDATE THE DO RECORD WITH THE DARTS KEY AND FLAG ETC.
     C     DOKY          CHAIN     PDO01                              60
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVEL     XXKEY         DODKEY
     C                   MOVE      XXSEQ#        DODSQ#
     C                   MOVEL     'Y'           DOIDFX
     C* STATUS IS ONLY UPDATED TO "CM" IF ALL INDEXES ARE COMPLETE
     C     DOKY          SETLL     PIE0C                                  61    61=RCD FOUND
     C     *IN61         IFEQ      *OFF                                         IF ALL ARE IDXD
     C*
     C                   MOVEL     'CM'          DODSTS
     C*
     C                   ELSE
     C*
     C*
     C                   ENDIF
     C                   UPDATE    PDO01
     C* CREATE AN INDEX RECORD HERE****
     C     XXKEY         IFNE      *BLANKS
     C                   WRITE     PDO011
     C                   ENDIF
     C*
     C* * *  PERFORM DOC NOTES   HERE!!!
     C*
     C                   EXSR      BCHUPD
     C*
     C                   ENDIF
      *
     C                   ENDIF
      *
      *
     C                   ELSE
      *
      * ALLOW A F KEY TO RETURN AND ALLOW SELECT OF OTHER DOC TYPE.
      *
     C     QQRTCD        IFEQ      'R'
      *
      * * * UPDATE THE DO RECORD WITH THE DARTS KEY AND FLAG ETC.
     C     DOKY          CHAIN     PDO01                              60
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVEL     XXKEY         DODKEY
     C                   MOVE      XXSEQ#        DODSQ#
     C                   MOVEL     'Y'           DOIDFX
     C*   STATUS IS ONLY UPDATED TO "CM" IF ALL INDEXES ARE COMPLETE
     C     DOKY          SETLL     PIE0C                                  61    61=RCD FOUND
     C     *IN61         IFEQ      *OFF                                         IF ALL ARE IDXD
     C*
     C                   MOVEL     'CM'          DODSTS
     C*
     C                   ELSE
     C*
     C*
     C                   ENDIF
     C                   UPDATE    PDO01
     C*   CREATE AN INDEX RECORD HERE****
     C     XXKEY         IFNE      *BLANKS
     C                   WRITE     PDO011
     C                   ENDIF
     C*
     C* * *  PERFORM DOC NOTES   HERE!!!
     C*
     C                   EXSR      BCHUPD
     C*
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      '3'
     C     QQRTCD        OREQ      'C'
      *
     C     IEKY1         CHAIN     PIE06                              60
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      ' '           IESTAT
     C                   UPDATE    PIE06
      *
     C     QQRTCD        CABEQ     'C'           TOP
     C     QQRTCD        CABEQ     '3'           TERM
      *
     C                   ENDIF
     C*
     C                   ELSE
     C*
      *   IF THE PGM RTNS A RQS FOR A DIFFERENT KEY PROGRAM (INDEXPGM)
     C     QQRTCD        IFEQ      'K'
      **
     C                   EXSR      NEWIDX
      **
     C                   ENDIF
     C*
     C     QQRTCD        IFEQ      '9'                                           REASSIGNED
     C     IEKY1         CHAIN     PIE06                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ' '           IESTAT
     C                   MOVE      'RA'          IEDSTS                          RE-ASS STS
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'ISCVTDT'     DTYMD
     C                   MOVEL     SRDTO         XXDT              6 0
     C                   Z-ADD     XXDT          IELDT
     C                   Z-ADD     DSTIME        IELTIM
     C                   MOVEL     Q1USER        IELUSR
     C                   UPDATE    PIE06
     C                   GOTO      NXTIE
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
      *
     C     IEKY1         CHAIN     PIE06                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ' '           IESTAT
     C                   UPDATE    PIE06
     C                   ENDIF
     C*
     C                   GOTO      NXTIE
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
     C                   MOVEL     'SC'          XXSYK
     C     SYKY          CHAIN     SDR01                              60
     C     *IN60         IFEQ      *OFF
     C     DRFL15        IFEQ      'Y'
     C                   SETON                                        23
     C                   ENDIF                                                  END DRFL15
     C                   ENDIF                                                  END *IN60
     C*                                                            TOGGLE
     C                   ENDSR
     C*
      *====================================================================*
      * * *   NEWIDX SUBROUTINE                              N E W I D X ***
      *====================================================================*
     C     NEWIDX        BEGSR
     C*
      * * * DISPLAY LIST OF INDEX TYPES TO CHOOSE.
      *
     C*
     C                   CALL      'SC092R'      ACDPL1
     C*
     C     *INKC         IFEQ      '1'
     C     *INKL         OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
      *
     C     IEKY1         CHAIN     PIE06                              60
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      ' '           IESTAT
     C                   UPDATE    PIE06
      *
     C                   ENDIF
     C*
     C*** ON RTRN WITH F3(3) OR F12(C) , NEED TO RETURN TO CALLING PGM WITH
     C* RETRN CODE INFO...
     C     *INKC         IFEQ      '1'
     C                   MOVEL     '3'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'C'           QQRTCD
     C                   GOTO      TOP
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PKT01                              60
     C                   MOVEL     KTINPG        IDXPGM
     C                   ENDIF
     C*
     C                   GOTO      IDXDSP
     C*
     C                   ENDSR
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
     C                   Z-ADD     0             SC#REC
     C*
     C                   READ      PDT01                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C*** Begin Add *** 03/11/17 *************************************
     C     DTstat        AndNE     'I'
     C*** End   Add *** 03/11/17 *************************************
     C                   ADD       1             RRN2
     C                   MOVEL     DTDOCT        SCDOCT
     C                   MOVEL     DTDESC        SCDESC
     C                   MOVEL     *BLANKS       SCSEL
     C*
     C*** DETERMINE IF THERE ARE DOCUMENTS TO INDEX FOR THIS TYPE ***
     C*
     C                   MOVEL     SCDOCT        $#QID            10
     C     $#QID         CHAIN     PIE02                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVE      *ON           *IN89
     C*  If *in80 is *on, loop through and count number of documents
     C   80*IN61         DOUEQ     *ON
     C                   ADD       1             SC#REC
     C     $#QID         READE     PIE02                                  61
     C                   ENDDO                                                  *IN61=*OFF
     C*
     C                   ELSE
     C                   MOVE      *OFF          *IN89
     C                   ENDIF
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
     C                   MOVE      *BLANKS       SCDOCT
     C                   MOVE      *BLANKS       SCDESC
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
     C*   F8 = SHOW #OF DOCS                 *
     C****************************************
     C     *INKH         IFEQ      '1'
     C     '0'           COMP      *IN80                                  80
     C*
     C                   EXSR      INIT
     C     *LOVAL        SETLL     PDT01                              60
     C*
     C*              CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     #@$2          #SFRCS            3 0
     C     #SFRCS        DOULE     0
     C                   EXSR      LOAD
     C                   SUB       13            #SFRCS
     C                   ENDDO                                                  #SFRCS<=0
     C*
     C*    Set the cursor to the first record of the page to be displayed
     C                   Z-ADD     RRNTRK        #@$2
     C*
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKH='1'
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
     C                   GOTO      STRT
     C                   END                                                    END CMDKEYS
     C*
     C                   ENDSR
     C*
      *====================================================================*
      * * *   BCHUPD SUBROUTINE                              B C H U P D ***
      *====================================================================*
     C     BCHUPD        BEGSR
     C*
     C     BMKY1         CHAIN     PBM01                              60
     C     *IN60         IFEQ      *OFF
     C     BMBSTS        ANDEQ     'RL'
     C*
     C     DOKY4         CHAIN     PDO04                              61
     C     *IN61         IFEQ      *OFF
     C*
     C                   MOVEL     'RL'          BMBSTS
     C                   ELSE
     C                   MOVEL     'CM'          BMBSTS
     C*
     C* * *  PERFORM BATCH NOTES HERE!!!
     C*
     C                   ENDIF
     C                   UPDATE    PBM01
     C                   ELSE
     C     *IN60         IFEQ      *OFF
     C                   UNLOCK    IMBML
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*                                                                     ****
