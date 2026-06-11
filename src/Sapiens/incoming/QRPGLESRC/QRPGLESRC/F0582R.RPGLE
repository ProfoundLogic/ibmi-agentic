     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE F0582R - CASE/PROVIDER CURRENT CHARGES SEARCH            *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  20    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                                 *
     H*  06/09/20  C:212 Project : SY420                              *
     H*                  Mask SSN                                     *
     H*                  Recompile only (SCSS# is hidden output field)*
     H*                                                               *
     H*  04/08/99 C:56 Support: MC095 #17568                          *
     H*               1. Add CO#/SUB#/FND TO CALL OF F0574R           *
     H*                                                               *
     H* 09/24/93  C:20    REQ/PROJ: FSMNT                             *
     H*                   1. FIX TO LOAD WDW02 CORRECTLY.  IF YOU HAD *
     H*                      EIGHT CHARGES AND FIRST SEVEN WERE       *
     H*                      APPLIED TO EOMB, WDW02 WOULD CRASH WHEN  *
     H*                      IT HAD NOTHING TO DISPLAY AND THOUGHT IT *
     H*                      SHOULD.                                  *
     H*****************************************************************
     FF0582F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(WDW01S:RRN2)
     F                                     SFILE(WDW02S:RRN3)
     FFDCAL1    IF   E           K DISK
     FFDCALN    IF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     FSMUPL     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     FFDHCL     IF   E           K DISK
     FFADCL     IF   E           K DISK

     D*
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
     D  XXUNIT                 9     11  0
     D                 DS
     D  XXPROC                 1     11
     D  XXCPT4                 1      5
     D  XXUNTS                 9     11
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D  CURSOR               370    371B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C     *LIKE         DEFINE    CADT          XXSDAT
     C     *LIKE         DEFINE    CADT          XHSDAT
     C*
     C*   KEY LIST FOR THE DETAIL CHARGE FILE
     C*
     C     CAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     CAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C     CAKY1A        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    XXSDAT
     C*
     C*   KEY LIST FOR THE DETAIL CHARGE AUXILIARY FILE
     C*
     C     HCKEY         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CADSEQ
     C*
     C*   KEY LIST FOR THE DIAGNOSIS CODE WORK FILE
     C*
     C     DCKEY         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    CADCS#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  FS BASE CONTROL FILE
     C*
     C     FBKY          KLIST
     C                   KFLD                    XSKEY             2
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
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
     C                   MOVEL     QQPRDS        SCPRDS
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQCASE        XXCASE            7 0
     C                   Z-ADD     *ZEROS        XHFEIN            9 0
     C                   Z-ADD     *ZEROS        XHMOD             3 0
     C                   Z-ADD     *ZEROS        X1FEIN            9 0
     C                   Z-ADD     *ZEROS        X1MOD             3 0
     C                   Z-ADD     *ZEROS        XXFEIN            9 0
     C                   Z-ADD     *ZEROS        XXMOD             3 0
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'WDW01'       Q2RECF
     C                   MOVE      QQRTCD        XXRTCD            1
     C     XXRTCD        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN26
     C                   ELSE
     C                   MOVE      '0'           *IN26
     C                   END
     C*
     C* CHECK TO SEE IF NETWORK PPO IS INSTALLED FOR NETWORK LISTING
     C*
     C                   MOVE      *BLANKS       XSKEY
     C                   MOVEL     'FB'          XSKEY
     C     FBKY          CHAIN     SFB01                              60
     C     *IN60         IFEQ      '0'
     C     FBFL06        IFEQ      'Y'
     C                   MOVE      '1'           *IN74
     C                   ELSE
     C                   MOVE      '0'           *IN74
     C                   END                                                    END FBFL06
     C                   END                                                    END *IN60
      *===============================================================
      * INITIALIZATION    AREA
      *
     C     UPKEY         CHAIN     PUP01                              60
     C*
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
     C     Q2RECF        IFEQ      'WDW01'
     C     Q2RECF        OREQ      'WDW01C'
     C     Q2RECF        OREQ      'WDW01S'
     C                   Z-ADD     RRN2          SVRRN2            5 0
     C*
     C                   READC     WDW01S                                 50
     C     *IN50         IFEQ      '0'
     C*
     C*** IF THE USER HAS USED A '1' TO SELECT A RECORD RETURN THIS PROVIDERS
     C*** FIEN/MOD TO BILL ENTRY
     C     SCSEL         IFEQ      '1'
     C     XXRTCD        IFEQ      *BLANKS
     C                   MOVE      SCFEIN        QQFEIN
     C                   MOVE      SCMOD         QQMOD
     C                   MOVE      '*'           QQRTCD
     C                   GOTO      TERM
     C                   ELSE
     C                   MOVE      '1'           *IN20
     C                   MOVE      '1'           *IN25
     C                   Z-ADD     RRN2          #@$2
     C                   Z-ADD     RRN2          SVRRN2
     C                   UPDATE    WDW01S
     C                   MOVE      *BLANKS       SCSEL
     C                   GOTO      MAINLN
     C                   END
     C                   ELSE
     C*
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*** GOTO SCREEN TWO AND DISPLAY DETAIL CHARGES
     C     SCSEL         IFEQ      '5'
     C                   Z-ADD     SCFEIN        XXFEIN
     C                   Z-ADD     SCMOD         XXMOD
     C*
     C                   Z-ADD     RRN2          #@$2
     C                   Z-ADD     RRN2          SVRRN2
     C                   MOVE      *BLANKS       SCSEL
     C                   UPDATE    WDW01S
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'WDW02'       Q2RECF
     C                   GOTO      TOP
     C                   ELSE                                                   ELSE EQ '1'
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*** CALL F9900C TO GIVE DETAIL CHARGES LISTING
     C     SCSEL         IFEQ      '6'
     C                   MOVE      SCCO#         QQCO#1            3
     C                   MOVE      SCSUB#        QQSUB1            5
     C                   MOVE      SCFND         QQFND1            3
     C                   MOVE      SCFYR         QQFYR1            3
     C                   MOVE      SCCASE        QQCAS1            7
     C                   MOVE      SCSS#         QQSS#1            9
     C                   MOVE      SCACDT        QQACD1            8
     C                   MOVE      SCFEIN        QQFEN1            9
     C                   MOVE      SCMOD         QQMOD1            3
     C                   CALL      'F9900C'
     C                   PARM                    QQCO#1
     C                   PARM                    QQSUB1
     C                   PARM                    QQFND1
     C                   PARM                    QQFYR1
     C                   PARM                    QQCAS1
     C                   PARM                    QQSS#1
     C                   PARM                    QQACD1
     C                   PARM                    QQFEN1
     C                   PARM                    QQMOD1
     C                   PARM      'C'           QQTYP1            1
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C*
     C     *IN74         IFEQ      '1'
     C                   CALL      'N9900C'
     C                   PARM                    QQCO#1
     C                   PARM                    QQSUB1
     C                   PARM                    QQFND1
     C                   PARM                    QQFYR1
     C                   PARM                    QQCAS1
     C                   PARM                    QQSS#1
     C                   PARM                    QQACD1
     C                   PARM                    QQFEN1
     C                   PARM                    QQMOD1
     C                   PARM      'C'           QQTYP1            1
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C                   END
     C*
     C                   Z-ADD     RRN2          #@$2
     C                   Z-ADD     RRN2          SVRRN2
     C                   MOVE      *BLANKS       SCSEL
     C                   UPDATE    WDW01S
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'WDW01'       Q2RECF
     C                   GOTO      TOP
     C                   ELSE                                                   ELSE EQ '6'
     C*** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '5' OR '6'
     C*** TO SLECT A RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C     SCSEL         IFNE      ' '
     C                   MOVE      '1'           *IN20
     C                   MOVE      '1'           *IN25
     C                   Z-ADD     RRN2          #@$2
     C                   UPDATE    WDW01S
     C                   MOVE      *BLANK        SCSEL
     C                   GOTO      MAINLN
     C                   END                                                    END NE ' '
     C                   END                                                    END EQ '6'
     C                   END                                                    END EQ '5'
     C                   END                                                    END EQ '1'
     C*
     C*** IF THE SUBFILE HAS NOT BEEN CHANGED, RETURN TO THE CALLING PROGRAM
     C*** WITHOUT A SELECTED RECORD
     C                   ELSE
     C                   GOTO      MAINLN
     C                   END                                                    END 0 *IN50
     C*
     C                   END                                                    WDW01
     C*
     C*
     C     Q2RECF        IFEQ      'WDW02'
     C     Q2RECF        OREQ      'WDW02C'
     C     Q2RECF        OREQ      'WDW02S'
     C*   CHECK FOR USER SELECTION TO REPOSITION
     C     SCSDAT        IFNE      *ZEROS
     C*
     C*
     C                   Z-ADD     SCSDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSDAT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN25
     C                   GOTO      MAINLN
     C                   END
     C*
     C                   Z-ADD     SCSDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSDAT
     C*
     C     XXSDAT        IFNE      XHSDAT
     C*
     C*   Clear subfile
     C*
     C                   MOVE      '1'           *IN04
     C                   WRITE     WDW02C
     C                   MOVE      '0'           *IN04
     C*
     C*   Reset relative record number since date changed
     C*
     C                   Z-ADD     0             RRN3
     C*
     C     CAKY1A        SETLL     PCA02                              60
     C*
     C                   EXSR      LOAD2
     C*
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'WDW02'       Q2RECF
     C*
     C                   Z-ADD     XXSDAT        XHSDAT
     C*
     C                   END
     C*
     C                   GOTO      MAINLN
     C                   END                                                    END SCSDAT
     C*
     C                   Z-ADD     RRN3          SVRRN3            5 0
     C*
     C                   READC     WDW02S                                 50
     C     *IN50         IFEQ      '0'
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*** CALL F0574R DISPLAY DETAIL CHARGES
     C     SCSEL         IFEQ      '5'
     C                   Z-ADD     SCFYR         Q1FYR             3 0
     C                   Z-ADD     SCCASE        Q1CASE            7 0
     C                   MOVE      SCSEQ#        Q1SEQ#
     C*** BEG ADD *** 04/08/99 ****************************************
     C                   MOVE(P)   SCCO#         Q1CO#
     C                   MOVE(P)   SCSUB#        Q1SUB#
     C                   MOVE(P)   SCFND         Q1FND
     C*** END ADD *** 04/08/99 ****************************************
     C*
     C                   CALL      'F0574R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL            1
     C                   PARM                    Q1FYR
     C                   PARM                    Q1CASE
     C                   PARM                    Q1SEQ#            5 0
     C                   PARM                    Q1RTCD            1
     C*** BEG ADD *** 04/08/99 ****************************************
     C                   PARM                    Q1CO#             3 0
     C                   PARM                    Q1SUB#            5 0
     C                   PARM                    Q1FND             3 0
     C*** END ADD *** 04/08/99 ****************************************
     C*
     C     Q1RTCD        IFEQ      'A'
???  C     Q1RTCD        OREQ      'B'
???  C     Q1RTCD        OREQ      ' '
     C                   Z-ADD     RRN3          #@$3
     C                   Z-ADD     RRN3          SVRRN3
     C                   MOVE      *BLANKS       SCSEL
     C                   UPDATE    WDW02S
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'WDW02'       Q2RECF
     C                   GOTO      MAINLN
     C                   END
     C                   ELSE
     C*** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '5'
     C*** TO SELECT A RECORD,SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C     SCSEL         IFNE      ' '
     C                   MOVE      '1'           *IN20
     C                   Z-ADD     RRN3          #@$3
     C                   UPDATE    WDW02S
     C                   MOVE      *BLANK        SCSEL
     C                   GOTO      MAINLN
     C                   END                                                    END NE ' '
     C                   END                                                    END EQ '5'
     C*** IF THE SUBFILE HAS NOT BEEN CHANGED, RETURN TO THE CALLING PROGRAM
     C*** WITHOUT A SELECTED RECORD
     C                   ELSE
     C                   GOTO      MAINLN
     C                   END                                                    END 0 *IN50
     C*
     C                   END                                                    WDW02
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
     C     Q2RECF        IFEQ      'WDW01'
     C     Q2RECF        OREQ      'WDW01C'
     C     Q2RECF        OREQ      'WDW01S'
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
     C                   END
     C*
     C     Q2RECF        IFEQ      'WDW02'
     C     Q2RECF        OREQ      'WDW02C'
     C     Q2RECF        OREQ      'WDW02S'
     C*
     C                   Z-ADD     *ZEROS        SCSDAT
     C                   Z-ADD     *ZEROS        XXSDAT
     C                   Z-ADD     *ZEROS        XHSDAT
     C*
     C     CAKY1         SETLL     PCA02                              60
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     WDW02C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN3
     C*
     C                   EXSR      LOAD2                                        LOAD RCDS FROM FILE
     C*
     C                   END
     C*
     C                   ENDSR
      *====================================================================*
      * * *   LOAD SUBROUTINE                                    L O A D ***
      *====================================================================*
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN25
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C                   Z-ADD     *ZEROS        X1FEIN                         ADD 05/14/93
     C                   Z-ADD     *ZEROS        X1MOD                          ADD 05/14/93
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C*
     C                   DO        999           X                 5 0
     C     *IN05         CABEQ     '1'           LDOUT
     C     CAKEY         READE     PCA01                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C*
     C* PROVIDER CANNOT BE SAME AS ONE WE ARE IN BILL ENTRY WITH
     C*
     C     CAFEIN        IFEQ      XHFEIN
     C     CAMOD         IFEQ      XHMOD
     C                   GOTO      NXTRCD
     C                   END
     C                   END
     C*
     C* PROVIDER CANNOT BE SAME AS LAST ONE PROCESSED
     C*
     C     CAFEIN        IFEQ      X1FEIN
     C     CAMOD         IFEQ      X1MOD
     C                   GOTO      NXTRCD
     C                   END
     C                   END
     C*
     C* LOAD SCREEN FIELDS
     C*
     C                   ADD       1             RRN2              5 0
     C                   Z-ADD     CAFEIN        SCFEIN
     C                   Z-ADD     CAFEIN        XXFEIN
     C                   Z-ADD     CAMOD         SCMOD
     C                   Z-ADD     CAMOD         XXMOD
     C                   MOVE      CAPRTY        SCPRTY
     C                   Z-ADD     CACASE        SCCASE
     C                   Z-ADD     CAFYR         SCFYR
     C                   Z-ADD     CACO#         SCCO#
     C                   Z-ADD     CASUB#        SCSUB#
     C                   Z-ADD     CAFND         SCFND
     C                   MOVE      CASS#         SCSS#
     C*
     C                   Z-ADD     CAACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C     DCKEY         CHAIN     PDC01                              55
     C     *IN55         IFEQ      '0'
     C                   MOVEL     DCBLTY        SCBLTY
     C                   ELSE
     C                   MOVE      *BLANKS       SCBLTY
     C                   END
      *
     C                   MOVE      '0'           *IN06
     C*
     C*** BEGIN DEL *** 12/02/97 **************************************
     C**********           MOVE *HIVAL    XHFRDT  60
     C**********           MOVE *ZERO     XHTODT  60
     C*** END   DEL *** 12/02/97 **************************************
     C*
     C*** BEGIN CHG *** 12/02/97 **************************************
     C                   MOVE      *HIVAL        XHFRDT            7 0
     C                   MOVE      *ZERO         XHTODT            7 0
     C*** END   CHG *** 12/02/97 **************************************
     C     CAKY1         CHAIN     PCA02                              65
     C     *IN65         DOWEQ     '0'
     C     CAEOB#        IFEQ      *ZEROS
     C*
     C* FIND FIRST AND LAST SERVICE DATE FOR THIS PROVIDER
     C*
     C     CAFDAT        IFLT      XHFRDT
     C                   Z-ADD     CAFDAT        XHFRDT
     C                   END
     C     CATDAT        IFGT      XHTODT
     C                   Z-ADD     CATDAT        XHTODT
     C                   END
     C                   END
     C     CAKY1         READE     PCA02                                  65
     C                   END
     C*
     C* UPDATE SUBFILE WITH THE DATES OF SERVICE
     C*
     C*
     C                   Z-ADD     XHFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFDAT
     C*
     C                   Z-ADD     XHTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTDAT
     C*
     C                   WRITE     WDW01S
     C*
     C                   Z-ADD     XXFEIN        X1FEIN
     C                   Z-ADD     XXMOD         X1MOD
     C*
     C* READ THE NEXT DETAIL CHARGE RECORD
     C*
     C     NXTRCD        TAG
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C* * * INZSRIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C*
     C     LDOUT         TAG
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *ZEROS        SCFEIN
     C                   MOVE      *ZEROS        SCMOD
     C                   MOVE      *ZEROS        SCFDAT
     C                   MOVE      *ZEROS        SCTDAT
     C                   MOVE      *BLANKS       SCPRTY
     C                   Z-ADD     1             RRN2
     C                   MOVE      '1'           *IN06
     C                   WRITE     WDW01S
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     1             #@$2
     C*
     C                   ENDSR
     C/EJECT
      *====================================================================*
      * * *   LOAD2 SUBROUTINE                                   L O A D ***
      *====================================================================*
     C     LOAD2         BEGSR
     C*
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C                   Z-ADD     XXFEIN        SCFEI2
     C                   Z-ADD     XXMOD         SCMOD2
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C*
     C                   DO        999           X                 5 0
     C     CAKY1         READE     PCA02                                  60
     C     *IN05         CABEQ     '1'           LDOUT1
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C*
     C     CAEOB#        IFEQ      *ZEROS
     C*
     C* LOAD SCREEN FIELDS
     C*
     C                   ADD       1             RRN3              5 0
     C                   Z-ADD     CADSEQ        SCSEQ#
     C                   Z-ADD     CACASE        SCCASE
     C                   Z-ADD     CAFYR         SCFYR
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFDAT
     C*
     C                   MOVE      CACSCD        SCCSCD
     C                   MOVE      *BLANKS       SCCODE
     C*
     C*   IF TYPE = 'N'
     C*
     C     CAPRTY        IFEQ      '1'
     C     CARCFL        IFNE      'X'
     C                   MOVEL     CACPT4        XXCPT4
     C                   MOVE      *BLANKS       XXUNTS
     C                   MOVEL     XXPROC        SCCODE
     C                   MOVE      CAPMOD        SCPMOD
     C                   ELSE                                                   ELSE EQ 'X'
     C     HCKEY         CHAIN     PHC01                              45
     C     *IN45         IFEQ      '0'
     C                   MOVEL     HCNDC         SCCODE
     C                   ELSE                                                   ELSE *IN45
     C                   MOVE      *BLANKS       SCCODE
     C                   END                                                    END *IN45
     C                   END                                                    END EQ 'X'
     C                   ELSE
     C*
     C*   ELSE TYPE = 'H'
     C*
     C                   Z-ADD     CASVCD        XXRVCD
     C                   MOVE      CACPT4        XXHCPC
     C                   Z-ADD     *ZEROS        XXUNIT
     C                   MOVE      XXCMSV        SCCODE
     C                   MOVE      *BLANKS       SCPMOD
     C                   END
     C*
     C                   Z-ADD     CATOTC        SCTOTC
     C                   Z-ADD     CATOTP        SCTOTP
     C                   MOVE      CAEOMB        SCEOMB
     C                   MOVE      CAPART        SCPART
     C                   MOVE      CAIPOP        SCIPOP
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     WDW02S
     C                   END
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C* * * INZSRIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C*
     C     LDOUT1        TAG
     C     RRN3          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   Z-ADD     *ZEROS        SCFDAT
     C                   Z-ADD     *ZEROS        SCSEQ#
     C                   MOVE      *BLANKS       SCCSCD
     C                   MOVE      *BLANKS       SCCODE
     C                   MOVE      *BLANKS       SCPMOD
     C                   Z-ADD     *ZEROS        SCTOTC
     C                   Z-ADD     *ZEROS        SCTOTP
     C                   MOVE      *BLANKS       SCEOMB
     C                   MOVE      *BLANKS       SCPART
     C                   MOVE      *BLANKS       SCIPOP
     C                   Z-ADD     1             RRN3
     C                   MOVE      '1'           *IN06
     C                   WRITE     WDW02S
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     RRN3          #@$3
     C*
     C                   Z-ADD     *ZEROS        SCFEIN
     C*
     C                   ENDSR
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
     C     SVRRN2        IFNE      0
     C                   Z-ADD     SVRRN2        RRN2
     C                   Z-ADD     SVRRN2        #@$2
     C                   Z-ADD     0             SVRRN2
     C                   END
     C                   MOVE      '1'           *IN01
     C                   WRITE     WDW01C
     C                   WRITE     WDW01
     C                   EXFMT     WDW01C
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C*
     C     Q2RECF        IFEQ      'WDW02'
     C     Q2RECF        OREQ      'WDW02C'
     C     Q2RECF        OREQ      'WDW02S'
     C     SVRRN3        IFNE      0
     C                   Z-ADD     SVRRN3        RRN3
     C                   Z-ADD     SVRRN3        #@$3
     C                   Z-ADD     0             SVRRN3
     C                   END
     C                   MOVE      '1'           *IN01
     C                   WRITE     WDW02C
     C                   WRITE     WDW02
     C                   EXFMT     WDW02C
     C                   MOVE      '0'           *IN01
     C                   END
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
     C                   MOVE      'B'           QQRTCD
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
     C     Q2RECF        IFEQ      'WDW02'
     C     Q2RECF        OREQ      'WDW02S'
     C     Q2RECF        OREQ      'WDW02C'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'WDW01'       Q2RECF
     C                   GOTO      MAINLN
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
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           MAINLN                         NO ROLL IF EOF
     C     Q2RECF        IFEQ      'WDW02'
     C     Q2RECF        OREQ      'WDW02C'
     C     Q2RECF        OREQ      'WDW02S'
     C                   EXSR      LOAD2
     C                   END
     C                   GOTO      MAINLN
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C                   GOTO      MAINLN
      *
     C                   ENDSR
     C/EJECT
