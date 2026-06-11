     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H/TITLE F0761R - PENDED CHARGEES SEARCH FOR A CASE               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR01:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  20    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  06/09/20 C:212 Project : SY420                               *
     H*                 Mask SSN                                      *
     H*                                                               *
     H*  04/08/99 C:56 Support: MC095 #17568                          *
     H*               1. Add CO#/SUB#/FND TO CALL OF F0574R           *
     H*                                                               *
     H*  12/01/93 C:20   PROJ: FSMNT                                  *
     H*                  1. If CACPT4 = 'FSMRA', don't display        *
     H*                     Total Charge field                        *
     H*                                                               *
     H*  10/16/96 C:32   PROJ: OK005                                  *
     H*                  1. If CACPT4 = 'FSMRA'                       *
     H*                     NO SPECIAL PROCESSING                     *
     H*****************************************************************
     FF0761F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(WDW01S:RRN2)
     F*
     FSMUPL     IF   E           K DISK
     FFDCALG    IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFDHCL     IF   E           K DISK
     FFMSVL1    IF   E           K DISK    USROPN
     FFMPRL     IF   E           K DISK    USROPN
     FFMRXL     IF   E           K DISK

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

     D*
     D QCA             S             55    DIM(4) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  XXCMSV                 1     11
     D  XXRVCD                 1      3  0
     D  XXHCPC                 4      8
     D  XXUNTS                 9     11  0
     D*** QCMDEXC CMD STRING MAPPED
     D                 DS
     D  @@CMD                  1     55
     D  @@ST                  38     39
     D*** QCMDEXC CMD STRING MAPPED
     D                 DS
     D  @XCMD                  1     55
     D  @XST                  40     41
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D  CURSOR               370    371B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C     *LIKE         DEFINE    CACASE        Q1CASE
     C     *LIKE         DEFINE    CADSEQ        Q1SEQ#
     C     *LIKE         DEFINE    CAFYR         Q1FYR
     C     *LIKE         DEFINE    CAFDAT        XXFDAT
     C     *LIKE         DEFINE    CACPT4        XXCPT4
     C     *LIKE         DEFINE    RXNDC         XXNDC
     C*
     C*   KEY LIST FOR THE CASE DETAIL FILE
     C*
     C     CAKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    XXFDAT
     C*
     C     CAKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    SCSS#
     C*
     C*  HOSPITAL DETAIL CHARGE FILE
     C     HCKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CADSEQ
     C*
     C*  SERVICE CODE FILE
     C*
     C     SVKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXCMSV
     C                   KFLD                    XXFDAT
     C*
     C*  PROCEDURE CODE FILE
     C*
     C     PRKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXFDAT
     C*
     C*  PHARMACY CODE FILE
     C*
     C     RXKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXNDC
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQRTCD            1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      QQFND         SCFND
     C                   MOVE      QQFYR         SCFYR
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQCASE        SCCASE
     C                   MOVE      QQFEIN        SCFEIN
     C                   MOVE      QQMOD         SCMOD
     C                   Z-ADD     *ZEROS        XXFDAT
     C     QQCALL        COMP      '1'                                    09    INQUIRY
     C     QQCALL        COMP      '2'                                    08    ANOTHER PGM
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   MOVE      *ZERO         UPFND
     C                   ELSE
     C                   Z-ADD     UPSUB#        SCSUB#
     C                   END
     C*
     C*   GET CLAIMANT NAME
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C                   MOVE      CMSS#         SCSS#
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SCNAME
     C                   ELSE
     C                   MOVEL     *BLANKS       SCNAME
     C                   END
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       scSS# = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
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
     C*   CHECK FOR USER SELECTION TO REPOSITION
     C     SCFRDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT
     C*
     C                   Z-ADD     *ZEROS        SCFRDT
     C                   GOTO      TOP
     C                   END                                                    END SCSUBS
     C*
     C                   Z-ADD     RRN2          SVRRN2            5 0
     C*
     C                   READC     WDW01S                                 50
     C     *IN50         IFEQ      '0'
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*** SAVE THAT DATA TO BE PASSED BACK TO THE CALLING PROGRAM
     C     SCSEL         IFEQ      '5'
     C                   Z-ADD     SCFYR         Q1FYR
     C                   Z-ADD     SCCASE        Q1CASE
     C                   Z-ADD     SCSEQ#        Q1SEQ#
     C*** BEG ADD *** 04/08/99 ****************************************
     C                   Z-ADD     UPCO#         Q1CO#
     C                   MOVE(P)   SCSUB#        Q1SUB#
     C                   MOVE(P)   SCFND         Q1FND
     C*** END ADD *** 04/08/99 ****************************************
     C                   CALL      'F0574R'
     C                   PARM                    QQPRDS
     C                   PARM      '2'           QQCALL
     C                   PARM                    Q1FYR
     C                   PARM                    Q1CASE
     C                   PARM                    Q1SEQ#
     C                   PARM                    Q1RTCD            1
     C*** BEG ADD *** 04/08/99 ****************************************
     C                   PARM                    Q1CO#             3 0
     C                   PARM                    Q1SUB#            5 0
     C                   PARM                    Q1FND             3 0
     C*** END ADD *** 04/08/99 ****************************************
     C*
     C                   Z-ADD     RRN2          #@$2
     C                   Z-ADD     RRN2          SVRRN2
     C                   MOVE      *BLANKS       SCSEL
     C                   UPDATE    WDW01S
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'WDW01'       Q2RECF
     C                   Z-ADD     *ZEROS        XXFDAT
     C                   GOTO      TOP
     C*
     C                   ELSE                                                   ELSE EQ '1'
     C*** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '5' TO INDICATE
     C*** A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C     SCSEL         IFNE      ' '
     C                   MOVE      '1'           *IN20
     C                   Z-ADD     RRN2          #@$2
     C                   UPDATE    WDW01S
     C                   MOVE      *BLANK        SCSEL
     C                   GOTO      MAINLN
     C                   END                                                    END NE ' '
     C                   END                                                    END EQ '1'
     C*** IF THE SUBFILE HAS NOT BEEN CHANGED, RETURN TO THE CALLING PROGRAM
     C*** WITHOUT A SELECTED RECORD
     C                   ELSE
     C                   GOTO      MAINLN
     C                   END                                                    END 0 *IN50
     C*
     C                   GOTO      TOP                                          RE-SET SUBFILE
     C*
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
     C     CAKEY         SETLL     PCA01                              60
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     WDW01C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
     C*
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C*
     C                   ENDSR
      *====================================================================*
      * * *   LOAD SUBROUTINE                                    L O A D ***
      *====================================================================*
     C     LOAD          BEGSR
     C*
     C*** CLOSE FILE IF OPENED
     C     STOPN         IFEQ      '1'
     C                   CLOSE     FMPRL
     C                   CLOSE     FMSVL1
     C                   MOVE      ' '           STOPN             1
     C                   MOVE      ' '           STOVR
     C                   END
     C*
     C*** DELETE OVERRIDE
     C     STOVR         IFEQ      '1'
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           STOVR             1
     C                   END
     C*
     C*** OVERRIDE FMPRL FILE TO SELECTED STATE CODE
     C     STOVR         IFEQ      ' '
     C                   Z-ADD     55            LENGTH
     C                   MOVEL     QCA(2)        @@CMD
     C                   MOVEL     XXAST         @@ST                           EOMB ADJUD STATE
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C*
     C                   Z-ADD     55            LENGTH
     C                   MOVEL     QCA(4)        @XCMD
     C                   MOVEL     XXAST         @@ST                           EOMB ADJUD STATE
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           STOVR             1
     C                   END
     C*
     C*** OPEN FILE FOR PROCESSING
     C     STOPN         IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FMPRL
     C                   OPEN      FMSVL1
     C                   MOVE      '1'           STOPN
     C                   END
     C*
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C                   DO        7             X                 5 0
     C*
     C     CAKY1         READE     PCA01                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C     CAFEIN        ANDEQ     SCFEIN
     C     CAMOD         ANDEQ     SCMOD
     C*
     C                   ADD       1             RRN2              5 0
     C                   MOVE      *BLANKS       XXAST             2
     C                   MOVEL     CAAST         XXAST
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFDAT
     C*
     C                   Z-ADD     CADSEQ        SCSEQ#
     C                   Z-ADD     CALTR#        SCLTR#
     C*
     C                   Z-ADD     CASVCD        XXRVCD            3 0
     C                   MOVE      CACPT4        XXHCPC            5
     C                   Z-ADD     *ZEROS        XXUNTS            3 0          ADD 05/31/92
     C     CAPRTY        IFEQ      '4'
     C                   MOVE      *BLANK        SCCODE
     C                   MOVEL     XXCMSV        SCCODE
     C*
     C                   Z-ADD     SCFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT
     C*
     C     SVKEY         SETGT     PSV01
     C     NXTSV2        TAG
     C                   READP     PSV01                                  60
     C     *IN60         IFEQ      '0'
     C     SVCO#         ANDEQ     UPCO#
     C     SVCMSV        ANDEQ     XXCMSV                                       CHG 04/28/92
     C     SVSTAT        IFNE      'I'
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVEL     SVDESC        SCDESC
     C                   ELSE
     C                   GOTO      NXTSV2
     C                   END
     C                   ELSE
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVEL     'UNLISTED'    SCDESC
     C     HCKY          CHAIN     PHC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVEL     HCDESC        SCDESC
     C                   END
     C                   END
     C                   ELSE
     C     CARCFL        IFNE      'X'
     C                   MOVE      *BLANKS       SCCODE
     C                   MOVEL     CACPT4        SCCODE
     C*
     C*** CLOSE FILE IF OPENED
     C     STOPN         IFEQ      '1'
     C                   CLOSE     FMPRL
     C                   MOVE      ' '           STOPN             1
     C                   MOVE      ' '           STOVR
     C                   END
     C*
     C*** DELETE OVERRIDE
     C     STOVR         IFEQ      '1'
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           STOVR             1
     C                   END
     C*
     C*** OVERRIDE FMPRL FILE TO SELECTED STATE CODE
     C     STOVR         IFEQ      ' '
     C                   Z-ADD     55            LENGTH
     C                   MOVEL     QCA(2)        @@CMD
     C                   MOVEL     XXAST         @@ST                           EOMB ADJUD STATE
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           STOVR             1
     C                   END
     C*
     C*** OPEN FILE FOR PROCESSING
     C     STOPN         IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FMPRL
     C                   MOVE      '1'           STOPN
     C                   END
     C*
     C                   Z-ADD     CAFDAT        XXFDAT
     C                   MOVE      *BLANKS       XXCPT4
     C                   MOVEL     CACPT4        XXCPT4
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  60
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVEL     PRDSC1        SCDESC
     C                   ELSE
     C     HCKY          CHAIN     PHC01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVE      *BLANKS       SCCODE
     C                   MOVEL     HCNDC         SCCODE
     C                   MOVEL     HCNDC         XXNDC
     C     RXKEY         CHAIN     PRX01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVEL     RXNAME        SCDESC
     C                   ELSE
     C                   MOVE      *BLANKS       SCDESC
     C                   END
     C                   ELSE
     C                   MOVE      *BLANKS       SCCODE
     C                   MOVE      *BLANKS       SCDESC
     C                   END
     C                   END
     C                   END
     C                   MOVEL     CAPMOD        SCPMOD
     C                   Z-ADD     CATOTC        SCTOTC
     C                   MOVE      CAFLAG        SCSTAT
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     WDW01S
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C* * * INZSRIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   Z-ADD     *ZEROS        SCFDAT
     C                   Z-ADD     *ZEROS        SCSEQ#
     C                   MOVE      *BLANKS       SCCODE
     C                   Z-ADD     *ZEROS        SCTOTC
     C                   Z-ADD     *ZEROS        SCLTR#
     C                   Z-ADD     *ZEROS        SCPMOD
     C                   MOVE      *BLANKS       SCSTAT
     C                   MOVE      *BLANKS       SCDESC
     C                   Z-ADD     1             RRN2
     C                   MOVE      '1'           *IN06
     C                   WRITE     WDW01S
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     RRN2          #@$2
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
     C     Q2RECF        IFEQ      'WDW01'
     C     Q2RECF        OREQ      'WDW01C'
     C     Q2RECF        OREQ      'WDW01S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     WDW01C
     C                   WRITE     WDW01
     C                   EXFMT     WDW01C
     C                   MOVE      '0'           *IN01
     C                   END
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
     C     Q2RECF        IFEQ      'WDW01'
     C     Q2RECF        OREQ      'WDW01S'
     C     Q2RECF        OREQ      'WDW01C'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
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
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           MAINLN                         NO ROLL IF EOF
     C                   EXSR      LOAD
     C                   GOTO      MAINLN
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C                   GOTO      MAINLN
      *
     C                   ENDSR
     C/EJECT
**  QCMDEXC TABLE
DLTOVR FILE(FMPRL)
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
DLTOVR FILE(FMSVL1)
OVRDBF FILE(FMSVL1) TOFILE(*LIBL/FMSVL1  )
