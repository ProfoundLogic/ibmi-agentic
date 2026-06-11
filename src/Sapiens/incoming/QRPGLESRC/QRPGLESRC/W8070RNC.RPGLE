     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE  W8070RNC -  PRINT PROGRAM FOR FORM 19
     H*****************************************************************
     H*
     H* CALLED FROM: W8070CNC - OVERRIDE PGM FOR FORM 19
     H* CALLS TO:  NONE
     H*
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H* 03/24/10  C:232 Project: 139WNN                               *
     H*                 Update programs to use the new EDI Control API*
     H*                 WA076R instead of using WMIXP.                *
     H*                                                               *
     H* 12/19/08  C:61  PROJECT 81WCN
     H*                 Created.  Copied and modified from CLT081
     H*                 version W8070RA1.
     H*
     H*****************************************************************
      *
     FSMUPL1    IF   E           K DISK
     FWMRPL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMLGL1    IF   E           K DISK
     FWMLGL     UF   E           K DISK
     F                                     RENAME(PLG01:PLG02)
     FWMFNL     IF   E           K DISK
     F*** BEGIN DLT *** 03/24/10 **************************************
     F***WMIXL   IF  E           K        DISK
     F*** END DLT ***** 03/24/10 **************************************
     FSTCTL1    IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMNIL     IF   E           K DISK
     F                                     RENAME(PNI01:PNI04)
      *
     FWMCML3    IF   E           K DISK
     F                                     RENAME(PCM01:PCML3)
     FQSYSPRT   O    F  127        PRINTER OFLIND(*INOF)
     F                                     USROPN
     FQSYSPRT1  O    F  132        PRINTER OFLIND(*INOA)
     FQSYSPRT2  O    F  132        PRINTER OFLIND(*INOB)
      *
      *

     D SZ              S              1    DIM(13)                              STATE & ZIP
      *
      *
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXACTM                 1      6  0
     D  XXTIME                 1      4  0
     D                 DS
     D  XXPOL#                 1     15
     D  CMFND                  1      3  0
     D  CMEMP#                 4     12  0
     D  CMFYR                 13     15  0
     D                 DS
     D  XXFNMD                 1     12
     D  XXFEIN                 1      9  0
     D  XXMOD                 10     12  0
     D XCEMPR          C                   CONST('EMPLOYER COPY')
     D XCEMPE          C                   CONST('EMPLOYEE COPY')
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *===============================================================
      * KLIST
      *===============================================================
      *
      *  CARRIER POLICY
      *

     C     RPKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
      *
      *  GROUP AUXILIARY
      *
     C     F2KY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
      *
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
      *
     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
     C     NIKY4         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    CMJRST
      *
      *  KEY LIST FOR THE LOG HISTORY FILE
      *
     C     LGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXHCPY
     C                   KFLD                    XXJRST
     C                   KFLD                    XXDATE
      *
     C     LGKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    LGIDT
     C                   KFLD                    LGITM
      *
      *  KEY LIST FOR THE FUND MASTER FILE
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *  KEY LIST FOR THE EDI CLAIM CONTROL FILE
      *
     C     IXKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMJRST
     C                   KFLD                    KKTSET
      *
      *  KEY LIST FOR THE DARTS REMARK CODE FILE
      *
     C     CTKY          KLIST
     C                   KFLD                    IXHCPR
      *
      *  KEY LIST FOR THE CLAIM MASTER FILE
      *
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
      *  KEY LIST FOR THE CLAIM MASTER FILE - FORMAT PCML3
      *
     C     L3KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDATE
      *
      *  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
      *
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
      *
      *  KEY LIST FOR THE EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
      *
      *  KEY LIST FOR THE EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    XXFYR
      *
      *  KEY LIST FOR THE STATE NOI INFORMATION FILE
      *
     C     NIKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    X2ST
      *
      *************************************************************
      *  *LIKE
      *************************************************************
      *
     C*** BEG DEL ****** 03/24/10 *************************************
     C***        *LIKE     DEFN IXTRID    KKTSET
     C*** END DEL ****** 03/24/10 *************************************
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    LGHCPY        XXHCPY
     C*** BEG CHG ****** 03/24/10 *************************************
     C***        *LIKE     DEFN IXJRST    XXJRST
     C     *LIKE         DEFINE    CMJRST        XXJRST
     C*** END CHG ****** 03/24/10 *************************************
     C     *LIKE         DEFINE    CMDT          XXDATE
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFND         SVFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CDCTY         XXCITY
     C     *LIKE         DEFINE    CDST          XXST
     C     *LIKE         DEFINE    EMMAD1        XXAD1
     C     *LIKE         DEFINE    EMMAD2        XXAD2
     C     *LIKE         DEFINE    NIST          X2ST
      *
      *
      *************************************************************
      *  PLIST
      *************************************************************
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFROI            2
     C                   PARM                    QQJRST            2
     C                   PARM                    QQDATE            8
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C*** BEG ADD ****** 03/24/10 *************************************
     C     IXAPI         PLIST
     C                   PARM                    CMCO#
     C                   PARM                    CMJRST
     C                   PARM                    KKTSET            3
     C                   PARM                    @@FND             3 0
     C                   PARM      *BLANKS       @RTNCD            1
     C                   PARM      *BLANKS       @EDIF             3
     C                   PARM      *BLANKS       IXELT             1
     C                   PARM      *ZEROS        IXELTD            3 0
     C                   PARM      *BLANKS       IXFROI            2
     C                   PARM      *ZEROS        IXSDUE            3 0
     C                   PARM      *BLANKS       IXRLS             1
     C                   PARM      *ZEROS        IXCSDT            7 0
     C                   PARM      *BLANKS       IXEADC            5
     C                   PARM      *BLANKS       IXHCPR            5
     C                   PARM      *BLANKS       IXFNRC            5
     C                   PARM      *BLANKS       IXHCED            1
     C*** END ADD ****** 03/24/10 *************************************
      *
      *************************************************************
      *  BEGIN
      *************************************************************
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              99
     C     OROUTQ        IFEQ      *BLANK
     C                   MOVEL     'QPRINT'      XXOUTQ           10
     C                   ENDIF
      *
     C                   MOVE      *BLANKS       XXAREA            3
     C                   MOVE      *BLANKS       XEAREA            5
     C                   MOVE      *BLANKS       XCAREA            5
      *
     C                   MOVE      QQCO#         XXCO#
     C                   MOVEL     QQFROI        XXHCPY
     C                   MOVE      QQDATE        XXDTE             8 0
     C                   MOVE      'NC'          XXJRST
      *
     C                   Z-ADD     XXDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
      *
     C                   TIME                    DSDTTM
      *
      *  DETERMINE IF CALLED FROM DAILY BATCH.
      *
     C                   MOVE      *BLANK        XXBTCH            1
      *
     C     QQDATE        IFNE      *ZERO
     C     QQFND         ANDEQ     *BLANKS
     C     QQFYR         ANDEQ     *BLANKS
     C     QQCASE        ANDEQ     *BLANKS
     C     QQFROI        ANDEQ     *BLANKS
     C                   MOVE      'Y'           XXBTCH
     C                   END
      *
      * Only one pass through the DO loop if
      * the program has been called from the 'Work With Claims' menu
      * 'Option 31=Reprint NOI' and will print only the NOI for the
      * associated Claim
      *
     C     QQFND         IFEQ      *BLANK
     C     QQFYR         ANDEQ     *BLANK
     C     QQCASE        ANDEQ     *BLANK
      *
      *  READ ALL RECORDS IN THE LOG HISTORY FILE IF CALLED FROM MENU,
      *   OR
      *  READ RECORDS IN THE CLAIM MASTER IF CALLED FROM DAILY BATCH
      *   FOR THE GIVEN DAILY BATCH RUN DATE.
      *
     C     XXBTCH        IFEQ      *BLANK
     C     LGKY          CHAIN     PLG01                              60
     C                   ELSE
     C     L3KY          CHAIN     PCML3                              60
     C                   END
      *
     C                   ELSE
      *
     C                   SETOFF                                       60
     C                   ENDIF
      *
     C     *IN60         DOWEQ     '0'
      *
      * This code allows for only one pass through the DO loop because
      * the program has been called from the 'Work With Claims' menu
      * 'Option 31=Reprint NOI' and will print only the NOI for the
      * associated Claim
      *
     C     QQFND         IFEQ      *BLANK
     C     QQFYR         ANDEQ     *BLANK
     C     QQCASE        ANDEQ     *BLANK
      *
     C     XXBTCH        IFEQ      *BLANK
      *
     C     LGPRSQ        IFNE      0
     C     LGTSET        ORNE      '148'
     C                   GOTO      SKIP
     C                   END
      *
     C                   END
      *
     C                   ENDIF
      *
     C                   EXSR      SRINIT
      *
      *  RETRIEVE THE CLAIM MASTER INFORMATION.
      *
     C     CMKY          CHAIN     PCM01                              60
     C   60              GOTO      SKIP
      *
      *  RETRIEVE THE CLAIMANT NAME AND ADDRESS.
      *
     C     CDKY          CHAIN     PCD01                              60
     C   60              GOTO      SKIP
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         XXCNAM           35
     C     XXCNAM        CAT       ',':0         XXCNAM
     C     XXCNAM        CAT       CDFSN:1       XXCNAM
     C     XXCNAM        CAT       CDMID:1       XXCNAM
     C                   MOVEL     CDADR1        XXCAD1           35
     C                   MOVEL     CDADR2        XXCAD2           35
     C*
     C                   MOVE(P)   CDCTY         XXCITY
     C                   MOVE      CDST          XXST
     C                   Z-ADD     CDZIP         XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         XXCCSZ           35
     C     XXCAD2        IFEQ      *BLANKS
     C                   MOVEL(P)  XXCCSZ        XXCAD2
     C                   MOVEL(P)  *BLANKS       XXCCSZ
     C                   ENDIF
     C                   END
      *
      *  GET THE EMPLOYER MASTER AND DETAIL INFORMATION
      *
     C     ELKY          CHAIN     PEL01                              60
     C   60              GOTO      SKIP
      *
     C     EMKY          CHAIN     PEM01                              60
     C   60              GOTO      SKIP
      *
     C     NIKY4         CHAIN     PNI04                              60
     C   60              GOTO      SKIP
      *
     C     COKY          CHAIN     PCO01                              60
     C   60              GOTO      SKIP
      *
      *
      *================================================================
      * Top of Page Info
      *================================================================
      *
      *  IC File #
      *
     C                   MOVEL(P)  CMACL#        X1SCL#           17
      *
      *  Carrier Code #
      *
     C                   MOVE      CMFND         XXTMP3            3
     C                   MOVEL(P)  XXTMP3        X1CAR#           15
     C                   CAT(P)    '-':0         X1CAR#
     C                   MOVE      CMFYR         XXTMP3            3
     C                   CAT(P)    XXTMP3:0      X1CAR#
     C                   CAT(P)    '-':0         X1CAR#
     C                   MOVE      CMCASE        XXTMP7            7
     C                   CAT(P)    XXTMP7:0      X1CAR#
      *
      *  Carrier File #
      *
     C     F2KY          CHAIN     PF201                              60
     C  N60              MOVE      F2CRCD        X1CRCD            9
      *
      * Employee Name
      *
     C                   MOVEL(P)  CMFSN         X1EENM           48
     C                   CAT(P)    CMMID:1       X1EENM
     C                   CAT(P)    CMLSN:1       X1EENM
      * Employee address
     C                   MOVEL(P)  CDADR1        X1EEA1           40
     C                   MOVEL(P)  CDADR2        X1EEA2           40
      *
     C     X1EEA2        IFEQ      *BLANKS
     C                   MOVEL(P)  X1EEA1        X1EEA2
     C                   MOVEL(P)  *BLANKS       X1EEA1
     C                   ENDIF
      *
      * Employee city state zip
      *
     C     CDCTRY        IFEQ      *BLANKS
     C     CDCTRY        OREQ      'USA'
     C                   MOVE(P)   CDCTY         XXCITY
     C                   MOVE      CDST          XXSTAT            2
     C                   Z-ADD     CDZIP         XXZIP             9 0
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         X1EEA3           40
     C                   ELSE
     C                   MOVEL(P)  CDADR3        X1EEA3
     C                   ENDIF
      * Employee home phone
     C                   MOVE      *BLANKS       X1EEPN
     C     CDAREA        IFNE      *ZERO
     C                   MOVE      CDAREA        XXTMP3            3
     C     '('           CAT(P)    XXTMP3:0      X1EEPN           14
     C                   CAT(P)    ')':0         X1EEPN
     C                   MOVEL     CDPHN#        XXTMP3
     C                   CAT(P)    XXTMP3:1      X1EEPN
     C                   CAT(P)    '-':0         X1EEPN
     C                   MOVE      CDPHN#        XXTMP4            4
     C                   CAT(P)    XXTMP4:0      X1EEPN
     C                   ENDIF
      * Employee work phone
     C                   MOVE      *BLANKS       X1EEWP
     C     CDWARA        IFNE      *ZERO
     C                   MOVE      CDWARA        XXTMP3            3
     C     '('           CAT(P)    XXTMP3:0      X1EEWP           14
     C                   CAT(P)    ')':0         X1EEWP
     C                   MOVEL     CDWPH#        XXTMP3
     C                   CAT(P)    XXTMP3:1      X1EEWP
     C                   CAT(P)    '-':0         X1EEWP
     C                   MOVE      CDWPH#        XXTMP4            4
     C                   CAT(P)    XXTMP4:0      X1EEWP
     C                   ENDIF
      *
      * Employee Social Security Number
      *
     C                   MOVE      *BLANKS       X1ESSN
     C     CMSS#         IFNE      *BLANKS
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C                   MOVEL(P)  CMSS#         XXTMP3
     C                   MOVEL(P)  XXTMP3        X1ESSN           11
     C                   CAT(P)    '-':0         X1ESSN
     C                   MOVEL(P)  CMSS#         XXTMP5            5
     C                   MOVE(P)   XXTMP5        XXTMP2            2
     C                   CAT(P)    XXTMP2:0      X1ESSN
     C                   CAT(P)    '-':0         X1ESSN
     C                   MOVE      CMSS#         XXTMP4
     C                   CAT(P)    XXTMP4:0      X1ESSN
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C                   ENDIF
      *
      * Employee Gender
      *
     C     CDSEX         IFEQ      'M'
     C                   MOVE      'XX'          X1SEXM            2
     C                   ELSE
     C                   MOVE      *BLANK        X1SEXM
     C                   ENDIF
      *
     C     CDSEX         IFEQ      'F'
     C                   MOVE      'XX'          X1SEXF            2
     C                   ELSE
     C                   MOVE      *BLANK        X1SEXF
     C                   ENDIF
      *
      * Employee Date of Birth
      *
     C                   Z-ADD     *ZERO         X1DTOB
     C     CDDTOB        IFNE      *ZERO
      *
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X1DTOB            6 0
      *
     C                   ENDIF
      *
      * Employer Name
      *
     C                   MOVEL     EMNAME        X1EMNM           40
      *
      * Employer phone number
      *
     C                   MOVE      *BLANKS       X1EMPH
     C     EMAREA        IFNE      *ZERO
     C                   MOVE      EMAREA        XXTMP3            3
     C     '('           CAT(P)    XXTMP3:0      X1EMPH           14
     C                   CAT(P)    ')':0         X1EMPH
     C                   MOVEL     EMPHN#        XXTMP3
     C                   CAT(P)    XXTMP3:1      X1EMPH
     C                   CAT(P)    '-':0         X1EMPH
     C                   MOVE      EMPHN#        XXTMP4            4
     C                   CAT(P)    XXTMP4:0      X1EMPH
     C                   ENDIF
      *
      * Employer address
      *
     C     EMPAD1        IFNE      *BLANKS
     C                   MOVEL(P)  EMPAD1        X1EMA1           30
     C                   MOVEL(P)  EMPAD2        X1EMA2           30
     C                   MOVE(P)   EMPCTY        XXCITY
     C                   MOVE      EMPST         XXSTAT            2
     C                   Z-ADD     EMPZIP        XXZIP             9 0
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         X1EMA3           30
      *
     C                   ELSE
      *
     C                   MOVEL(P)  EMMAD1        X1EMA1           30
     C                   MOVEL(P)  EMMAD2        X1EMA2           30
     C                   MOVE(P)   EMMCTY        XXCITY
     C                   MOVE      EMMST         XXSTAT            2
     C                   Z-ADD     EMMZIP        XXZIP             9 0
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         X1EMA3           30
     C                   ENDIF
      *
     C     X1EMA2        IFEQ      *BLANKS
     C                   MOVEL(P)  X1EMA1        X1EMA2
     C                   MOVEL(P)  *BLANKS       X1EMA1
     C                   ENDIF
      *
      * Carrier (Company) name
      *
     C                   MOVEL(P)  CONAME        X1CONM           35
     C                   MOVEL(P)  CONAME        X1CONM           35
      *
      * Carrier #
     C     RPKY          CHAIN     PRP01                              60
     C  N60              MOVEL(P)  RPCAP#        X1CAP#           19
      *
      * Carrier address
     C                   MOVEL(P)  COADR1        X1CRA1           40
     C                   MOVEL(P)  COADR2        X1CRA2           30
     C                   MOVE(P)   COCTY         XXCITY
     C                   MOVE      COST          XXSTAT            2
     C                   Z-ADD     COZIP         XXZIP             9 0
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         X1CRA3           30
      *
      * Carrier phone #
     C                   MOVE      *BLANKS       X1CRPH
     C     COAREA        IFNE      *ZERO
     C                   MOVE      COAREA        XXTMP3            3
     C     '('           CAT(P)    XXTMP3:0      X1CRPH           14
     C                   CAT(P)    ')':0         X1CRPH
     C                   MOVEL     COPHN#        XXTMP3
     C                   CAT(P)    XXTMP3:1      X1CRPH
     C                   CAT(P)    '-':0         X1CRPH
     C                   MOVE      COPHN#        XXTMP4            4
     C                   CAT(P)    XXTMP4:0      X1CRPH
     C                   ENDIF
      *
      * Carrier fax #
     C                   MOVE      *BLANKS       X1CRFX
     C     COFARA        IFNE      *ZERO
     C                   MOVE      COFARA        XXTMP3            3
     C     '('           CAT(P)    XXTMP3:0      X1CRFX           14
     C                   CAT(P)    ')':0         X1CRFX
     C                   MOVEL     COFAX#        XXTMP3
     C                   CAT(P)    XXTMP3:1      X1CRFX
     C                   CAT(P)    '-':0         X1CRFX
     C                   MOVE      COFAX#        XXTMP4            4
     C                   CAT(P)    XXTMP4:0      X1CRFX
     C                   ENDIF
      *
      * Nature of Employer's Business
     C                   MOVEL(P)  NI60A1        X160A1           60
     C                   MOVEL(P)  NI60A2        X160A2           60
      *
      * Location of plant where injury occurred
     C                   MOVEL(P)  NI40A1        X140A1           40
     C                   MOVEL(P)  NI30A1        X130A1           15
     C                   MOVEL(P)  NI30A2        X130A2           25
     C                   MOVEL(P)  NI20A1        X120A1           12
      *
      * Date of Injury
     C                   CLEAR                   X1DOI
     C                   CLEAR                   X1DAY
     C     CMACDT        IFNE      *ZERO
      *
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X1DOI             6 0
      *
     C                   MOVE      X1DOI         #1DATE
      *
     C                   CALL      'SRDAY'
     C                   PARM                    #1DATE            6            M/D/Y
     C                   PARM                    #1SDAY            4            Abbreviation
     C                   PARM                    #1LDAY            9            Full Day Name
     C                   PARM                    #1ERR             1            Error Flag
      *
     C                   MOVEL(P)  #1LDAY        X1DAY             9
      *
     C                   ENDIF
      *
     C                   MOVE(P)   *BLANKS       X1IAM             2
     C                   MOVE(P)   *BLANKS       X1IPM             2
     C                   MOVEL     CMTIME        X1ITM             4 0
      *
     C     X1ITM         IFGE      1300
     C                   SUB       1200          X1ITM
     C                   ENDIF
      *
     C     CMTIME        IFLT      120000
     C                   MOVE      'XX'          X1IAM
     C                   ELSE
     C                   MOVE      'XX'          X1IPM
     C                   ENDIF
      *
      * Employee paid for entire day
     C     CMPOI         IFEQ      'Y'
     C                   MOVE      'YES'         X1POI             3
     C                   ELSE
     C                   MOVE      'NO '         X1POI
     C                   ENDIF
      *
      * Date Disability began
     C                   Z-ADD     CMDBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X1DBDT            6 0
      *
     C                   MOVE      *BLANKS       X1DBAM            2
     C                   MOVE      *BLANKS       X1DBPM            2
      *
     C     NI1A3         IFEQ      'A'
     C                   MOVE      'XX'          X1DBAM
     C                   ELSE
     C                   MOVE      'XX'          X1DBPM
     C                   ENDIF
      *
      *  First knew of Injury
      *
     C                   CLEAR                   X1DTRP
     C     CMDTRP        IFNE      *ZERO
     C                   Z-ADD     CMDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X1DTRP            6 0
     C                   ENDIF
      *
      *  Name of Supervisor
     C     CMCLEX        CHAIN     SMUPL1                             60
     C  N60              MOVEL     UPUSNM        X1USNM
     C   60              MOVEL     *BLANKS       X1USNM           25
      *
      *  Occupation when injured
     C                   MOVEL     CMOCDS        X1OCDS           40
      *
      *  Time employed by you
     C                   MOVEL     NI20A3        X1YRS             2 0
     C                   MOVEL     NI20A3        XXTMP4
     C                   MOVE      XXTMP4        X1MTHS            2 0
     C                   MOVEL     NI20A3        XXTMP6            6
     C                   MOVE      XXTMP6        X1DAYS            2 0
      *
     C     X1YRS         COMP      0                                      81
     C     X1MTHS        COMP      0                                      82
     C     X1DAYS        COMP      0                                      83
      *  Wages per hour.
     C                   MOVE      NI10A1        X110A1            9 2
      *  Hours per day
     C                   Z-ADD     CMHRSD        X1HRSD            3 0
      *  Wages per day
     C                   MOVE      NI10A2        X110A2            9 2
      *  Number of days per week
     C                   Z-ADD     CMDAYS        X1NDAY            1 0
      *  Average Weekly Wage
     C                   Z-ADD     CMAWW         X1AWW             7 2
      *  Board, lodging, fuel
     C                   MOVE      NI10A3        X110A3            9 2
     C                   MOVE      NI10A4        X110A4           10
      *  How injury occurred
     C                   MOVEL(P)  CMACDS        X1ACDS           60
     C                   MOVEL(P)  CMACD2        X1ACD2           60
      *  List all injuries
     C                   MOVEL(P)  NI60A3        X160A3           60
     C                   MOVEL(P)  NI60A4        X160A4           60
      *  Date returned to work
     C                   CLEAR                   X1RTW
     C     CMRTW         IFNE      *ZERO
     C                   Z-ADD     CMRTW         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X1RTW             6 0
     C                   ENDIF
      *  Hour returned to work
      *
     C                   MOVE      *OFF          *IN84
     C                   MOVE      NI10A5        X110A5            4 0
      *
     C     X110A5        IFNE      *ZERO
     C                   MOVE      *ON           *IN84
      *
     C     NI1A5         IFNE      ' '
     C                   MOVE      'A.M.'        X1AMPM            4
     C                   ENDIF
      *
     C     NI1A6         IFNE      ' '
     C                   MOVE      'P.M.'        X1AMPM            4
     C                   ENDIF
      *
     C                   ENDIF
      *  Return to work wages
     C                   MOVEL     NI20A2        X120A2            9 2
     C                   MOVEL(P)  NI20A4        X120A4           10
      *  At what occupation
     C                   MOVEL(P)  NI60A5        X160A5           26
      *  Employee salary continued in full?
     C     CMFSAL        IFEQ      'Y'
     C                   MOVEL     'YES'         X1FSAL            3
     C                   ENDIF
     C     CMFSAL        IFEQ      'N'
     C                   MOVEL     'NO '         X1FSAL            3
     C                   ENDIF
      *
      *  Treated by Physician ?
      *
     C                   MOVE      *BLANKS       X1PHY             3
     C     NI1A8         IFEQ      'Y'
     C                   MOVE      'YES'         X1PHY             3
     C                   ENDIF
     C     NI1A8         IFEQ      'N'
     C                   MOVE      'NO '         X1PHY             3
     C                   ENDIF
      *  Date of Death
     C     CMDOD         IFNE      *ZERO
     C                   MOVEL     'YES'         X1DTH             3
     C                   Z-ADD     CMDOD         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X1DOD             6 0
     C                   ELSE
     C                   MOVEL     'NO '         X1DTH             3
     C                   CLEAR                   X1DOD
     C                   ENDIF
      *
      *================================================================
      *  BOTTOM OF PAGE INFO
      *================================================================
      *
      *  OPEN THE SPOOLFILE AND PRINT THE REPORT.  THEN CLOSE THE
      *  SPOOLFILE, CREATE THE DARTS REMARK AND RESOLVE THE OUTPUT.
      *
     C                   MOVE(P)   *BLANK        XXBOTM           20
      *
     C     XXBTCH        IFEQ      *BLANK
     C                   OPEN      QSYSPRT
     C                   EXCEPT    DET
     C                   EXCEPT    DET2
     C                   CLOSE     QSYSPRT
      *
      * code added here bypasses call for DARTS remark creation
      * when program is invoked via 'Work With Claims' option
      *
     C     QQFND         IFEQ      *BLANK
     C     QQFYR         ANDEQ     *BLANK
     C     QQCASE        ANDEQ     *BLANK
     C                   EXSR      SRDART
     C                   EXSR      SRLABL
     C                   ENDIF                                                  /opt 31 called
      *
      * CALLED FROM DAILY BATCH
      *
     C                   ELSE
     C                   OPEN      QSYSPRT
     C                   MOVEL(P)  *BLANK        XXBOTM
     C                   EXCEPT    DET
     C                   EXCEPT    DET2
     C                   MOVEL(P)  XCEMPR        XXBOTM                         EMPLOYER COP
     C                   EXCEPT    DET
     C                   EXCEPT    DET2
     C                   MOVEL(P)  XCEMPE        XXBOTM                         EMPLOYEE COP
     C                   EXCEPT    DET
     C                   EXCEPT    DET2
     C                   CLOSE     QSYSPRT
     C                   EXSR      SRLABL
     C                   END
      *                                                                **
      *  RETRIEVE THE NEXT RECORD
      *
     C     SKIP          TAG
      *
      * This code allows for only one pass through the DO loop because
      * the program has been called from the 'Work With Claims' menu
      * 'Option 31=Reprint NOI' and will print only the NOI for the
      * associated Claim
      *
     C     QQFND         IFEQ      *BLANK
     C     QQFYR         ANDEQ     *BLANK
     C     QQCASE        ANDEQ     *BLANK
      *
     C     XXBTCH        IFEQ      *BLANK
     C     LGKY          READE     PLG01                                  60
      *
     C                   ELSE
      *
     C     L3KY          READE     PCML3                                  60
     C                   END
      *
     C                   ELSE
     C                   MOVE      '1'           *IN60                          End the DO loop
     C                   ENDIF
      *
     C                   ENDDO
      *  END OF PROGRAM
      *
     C                   MOVE      '1'           *INLR
      *
      *
      *****************************************************************
      * SRINIT SUBROUTINE              ** Subroutine **   S R I N I T *
      * THIS SUBROUTINE INITIALIZES ALL WORK FIELDS                   *
      *****************************************************************
     C     SRINIT        BEGSR
      *
     C                   CLEAR                   X1SCL#
     C                   CLEAR                   X1CAR#
     C                   CLEAR                   X1CRCD
     C                   CLEAR                   X1EENM
     C                   CLEAR                   X1EMNM
     C                   CLEAR                   X1EMPH
     C                   CLEAR                   X1EEA1
     C                   CLEAR                   X1EMA1
     C                   CLEAR                   X1EEA2
     C                   CLEAR                   X1EMA2
     C                   CLEAR                   X1EMA3
     C                   CLEAR                   X1EEA3
     C                   CLEAR                   X1CONM
     C                   CLEAR                   X1CAP#
     C                   CLEAR                   X1CRA1
     C                   CLEAR                   X1EEPN
     C                   CLEAR                   X1EEWP
     C                   CLEAR                   X1CRA2
     C                   CLEAR                   X1CRA3
     C                   CLEAR                   X1ESSN
     C                   CLEAR                   X1SEXM
     C                   CLEAR                   X1SEXF
     C                   CLEAR                   X1DTOB
     C                   CLEAR                   X1CRPH
     C                   CLEAR                   X1CRFX
     C                   CLEAR                   X160A1
     C                   CLEAR                   X160A2
     C                   CLEAR                   X140A1
     C                   CLEAR                   X130A1
     C                   CLEAR                   X130A2
     C                   CLEAR                   X120A1
     C                   CLEAR                   X1DOI
     C                   CLEAR                   X1DAY
     C                   CLEAR                   X1ITM
     C                   CLEAR                   X1IAM
     C                   CLEAR                   X1IPM
     C                   CLEAR                   X1POI
     C                   CLEAR                   X1DBDT
     C                   CLEAR                   X1DBAM
     C                   CLEAR                   X1DBPM
     C                   CLEAR                   X1DTRP
     C                   CLEAR                   X1USNM
     C                   CLEAR                   X1OCDS
     C                   CLEAR                   X1YRS
     C                   CLEAR                   X1MTHS
     C                   CLEAR                   X1DAYS
     C                   CLEAR                   X110A1
     C                   CLEAR                   X1HRSD
     C                   CLEAR                   X110A2
     C                   CLEAR                   X1NDAY
     C                   CLEAR                   X1AWW
     C                   CLEAR                   X110A3
     C                   CLEAR                   X110A4
     C                   CLEAR                   X1ACDS
     C                   CLEAR                   X1ACD2
     C                   CLEAR                   X160A3
     C                   CLEAR                   X160A4
     C                   CLEAR                   X1RTW
     C                   CLEAR                   X110A5
     C                   CLEAR                   X1AMPM
     C                   CLEAR                   X120A2
     C                   CLEAR                   X120A4
     C                   CLEAR                   X160A5
     C                   CLEAR                   X1FSAL
     C                   CLEAR                   X1PHY
     C                   CLEAR                   X1DTH
     C                   CLEAR                   X1DOD
     C                   CLEAR                   XXBOTM
     C                   CLEAR                   XXAD1
     C                   CLEAR                   XXAD2
     C                   CLEAR                   XX1CSZ
     C                   CLEAR                   XXCNAM
     C                   CLEAR                   XXCAD1
     C                   CLEAR                   XXCAD2
     C                   CLEAR                   XXCCSZ
      *
      * This code is used to initialize the Keys for whichever
      * files need to be opened based on the calling programs.  If
      * called from the 'Work With Claims  Option '31=Reprint NOI'
      * the Claim Master File will be opened
      *
     C     QQFND         IFEQ      *BLANK
     C     QQFYR         ANDEQ     *BLANK
     C     QQCASE        ANDEQ     *BLANK
      *
     C     XXBTCH        IFEQ      *BLANK
     C                   Z-ADD     LGFND         XXFND
     C                   Z-ADD     LGFYR         XXFYR
     C                   Z-ADD     LGCASE        XXCASE
     C                   MOVE      LGJST         X2ST
      *
     C                   ELSE
     C                   Z-ADD     CMFND         XXFND
     C                   Z-ADD     CMFYR         XXFYR
     C                   Z-ADD     CMCASE        XXCASE
     C                   MOVE      CMJRST        X2ST
     C                   END
      *
     C                   ELSE
      *
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   ENDIF
      *
      *  RETRIEVE THE NEW FUND INFORMATION, IF THE FUND CHANGED.
      *
     C     XXFND         IFNE      SVFND
     C     FNKY          CHAIN     PFN01                              60
     C                   Z-ADD     XXFND         SVFND
     C                   END
      *
     C                   ENDSR
      *
      *
      *****************************************************************
      * SRADDR SUBROUTINE              ** Subroutine **   S R A D D R *
      * THIS SUBROUTINE PUTS TOGETHER AN ADDRESS INTO ONE STRING      *
      * CONTAINING THE CITY, STATE AND ZIP                            *
      *****************************************************************
     C     SRADDR        BEGSR
      *
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVE      *BLANKS       XXSZ             13
      *
     C                   MOVEA     XXST          SZ
     C                   MOVEL     XXZIP         XXZP1
     C                   MOVE      XXZIP         XXZP2
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZP2         IFNE      '0000'
     C                   MOVE      '-'           SZ(9)
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
      *
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVEL     XXCITY        XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST           56
     C                   PARM                    XXFRST           15
     C                   PARM                    XXTERM            1
     C                   MOVEL     XXLAST        XXCSZ            40
      *
     C                   ENDSR
      *
      *
      *****************************************************************
      * SRDART SUBROUTINE              ** Subroutine **   S R D A R T *
      * THIS SUBROUTINE WRITES A DARTS REMARK FOR EACH REPORT WHICH   *
      * IS PROCESSED.                                                 *
      *****************************************************************
     C     SRDART        BEGSR
      *
      *  DON'T CREATE THE DARTS REMARK IF THE HARDCOPY DARTS CODE IS
      *  BLANK OR INVALID.
      *
      *
     C                   MOVE      '148'         KKTSET
      *
     C*** BEG CHG ****** 03/24/10 *************************************
     C***        IXKY      CHAINPIX01                99
     C                   MOVEL     CMFND         @@FND
     C                   CALL      'WA076R'      IXAPI                  60
     C     @RTNCD        IFEQ      'Y'
     C                   MOVE      '0'           *IN99
     C                   ELSE
     C                   MOVE      '1'           *IN99
     C                   ENDIF
     C*** END CHG ****** 03/24/10 *************************************
      *
     C     *IN99         IFEQ      *OFF
      *
      *  ESTABLISH KEY
      *
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      XXFND         QCFND
     C                   MOVE      XXFYR         QCFYR
     C                   MOVE      XXCASE        QCCASE
      *
      *          Call API to write a DARTS Remark
      *          ------------------------------------------------------
     C                   MOVEL(P)  QCKEY         #2KEY            40
     C                   MOVEL     DSDATE        #2CDAT
     C                   MOVEL     IXHCPR        #2RMCD
     C     #2RMCD        CHAIN     PCT01                              60
     C  N60              MOVEL(P)  CTDESC        #2COMM
      *
      *  GET FOLDER NAME
      *
     C                   MOVEL     CMFND         ##FND                          for Fund
     C                   MOVEL(P)  'C'           ##FOLD                         means "Member"
     C                   MOVE      CMCASE        ##FOLD
      *
     C                   CALL      'SRFBLD'
     C                   PARM                    ##FND             8
     C                   PARM      'CS'          ##SER             8
     C                   PARM      CTTYPE        ##SUB             8
     C                   PARM                    ##FOLD            8
     C                   PARM      *BLANKS       ##FPTH           40            Built, returned
     C                   PARM      *BLANKS       ##TERM            1
      *
     C                   MOVEL(P)  ##FPTH        #2FOLD
      *
      *  GET DOCUMENT ID
      *
     C                   CALL      'SRDBLD'
     C                   PARM                    ##FPTH                         From SRFBLD routine
     C                   PARM      CTTYPE        ##RT              2            Rmrk Type for Doc ID
     C                   PARM      *BLANKS       ##DOC            12            Built, returned
     C                   PARM      *BLANKS       ##TERM
      *
     C                   MOVEL(P)  ##DOC         XXTMP7
     C     XXTMP7        CAT(P)    '.PDF'        #2DCID
     C                   MOVEL(P)  'W0070R'      #2PNAM
      *
     C                   CALL      'DA700R'
     C                   PARM                    #2KEY            40
     C                   PARM                    #2COMM          256
     C                   PARM                    #2CDAT            8
     C                   PARM                    #2DCID           12
     C                   PARM                    #2PNAM           10
     C                   PARM      *ZEROS        #2SECL            3
     C                   PARM      LGIUSR        #2USER           10
     C                   PARM      Q1PGM         #2PGM            10
     C                   PARM                    #2FOLD           80
     C                   PARM      'N'           #2DIFL            1
     C                   PARM      Q1PGM         #2IPGM           10
     C                   PARM      *BLANK        #2FL01            1
     C                   PARM      *BLANK        #2FL02            1
     C                   PARM                    #2RMCD            5
     C                   PARM      '00000'       #2SEQ#            5
     C                   PARM      'N'           #2SBYL            1
     C                   PARM      *BLANKS       #2COM2           19
     C                   PARM      'WC'          #2APID            2
     C                   PARM      *BLANKS       #2IDOC           12
     C                   PARM      *BLANKS       #2IQUE           10
     C                   PARM      'O'           #2IMGF            1
     C                   PARM      *BLANKS       #2IPTH           10
     C                   PARM      *ZEROS        #2PIDX            5
     C                   PARM      *BLANK        #2QPTY            1
     C                   PARM      *BLANKS       #2DCTP            5
     C                   PARM      *BLANKS       #2CSID            2
     C                   PARM      *BLANKS       #2CODE            2
      *
      *  LOAD THE DARTS SEQ# INTO THE EDI LOG FILE
      *
     C     LGKY1         CHAIN     PLG02                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     *ZERO         LGPRSQ
     C                   MOVE      #2SEQ#        LGPRSQ
     C                   UPDATE    PLG02
     C                   END
      *
      *  CALL W8070C1 TO RESOLVE THE OUTPUT INTO AN OFFICE DOCUMENT
      *
     C                   MOVEL(P)  'QSYSPRT'     #3SPLF
     C                   MOVEL(P)  #2FOLD        #3FOLD
     C                   MOVEL(P)  #2DCID        #3DCID
     C                   MOVEL(P)  XXOUTQ        #3OUTQ
      *
     C                   CALL      'SRESOLVE'
     C                   PARM                    #3SPLF           10
     C                   PARM                    #3FOLD           80
     C                   PARM                    #3DCID           12
     C                   PARM                    #3OUTQ           10
      *
     C                   ENDIF
      *
     C                   ENDSR
      *
      *
      * ***************************************************************
      * SRLABL SUBROUTINE              ** Subroutine **   S R L A B L *
      * THIS SUBROUTINE PRINTS AN EMPLOYER  AND A CLAIMANT LABEL.     *
      * ***************************************************************
     C     SRLABL        BEGSR
     C     EMMAD1        IFNE      *BLANKS
     C                   MOVEL     EMMAD1        XXAD1
     C                   MOVEL     EMMAD2        XXAD2
     C                   MOVEL(P)  EMMCTY        XXCITY
     C                   MOVE(P)   EMMST         XXST
     C                   Z-ADD     EMMZIP        XXZIP
     C                   ELSE
     C                   MOVEL     EMPAD1        XXAD1
     C                   MOVEL     EMPAD2        XXAD2
     C                   MOVEL(P)  EMPCTY        XXCITY
     C                   MOVE(P)   EMPST         XXST
     C                   Z-ADD     EMPZIP        XXZIP
     C                   END
      *
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         XX1CSZ           40
     C     XXAD2         IFEQ      *BLANKS
     C                   MOVEL(P)  XX1CSZ        XXAD2
     C                   MOVEL(P)  *BLANKS       XX1CSZ
     C                   ENDIF
     C                   EXCEPT    LABEL1
      *
     C                   EXCEPT    LABEL2
      *
      *
     C                   ENDSR
      *
      *
     OQSYSPRT   E            DET              05
      *
     O          E            DET            3  5
     O                       X1SCL#            +110
      *
     O          E            DET         1  3
     O                       EMSEMP            +110
      *
     O          E            DET            3
     O                       X1CAR#            +110
      *
     O          E            DET            3
     O                       EMFEIN            +110
      *
     O          E            DET            3
     O                       X1CRCD            +110
      * NAME
     O          E            DET              37
     O                       X1EENM            +  9
     O                       X1EMNM            + 05
     O                       X1EMPH            + 04
      * Employee and Employer ADDRESS
     O          E            DET              40
     O                       X1EEA1            +  9
     O                       X1EMA1            + 13
     O          E            DET              41
     O                       X1EEA2            +  9
     O                       X1EMA2            + 13
     O                       X1EMA3            +  2
      *
      * Employee city,st,zip / Carrier name
     O          E            DET              44
     O                       X1EEA3            +  9
     O                       X1CONM            + 13
     O                       X1CAP#            +  2
      *
      * HOME AND WORK PHONE / CARRIER ADDRESS
     O          E            DET              47
     O                       X1CRA1            + 62
      *
     O          E            DET              48
     O                       X1EEPN            +  8
     O                       X1EEWP            + 20
     O                       X1CRA2            +  6
     O                       X1CRA3            +  2
      *
      * SOCIAL SECURITY NUMBER, GENDER, DOB / CARRIER PHONE + FAX
      *
     O          E            DET              51
     O                       X1ESSN            +  8
     O                       X1SEXM            + 11
     O                       X1SEXF            +  3
     O                       X1DTOB            +  9 '  /  /  '
     O                       X1CRPH            +  9
     O                       X1CRFX            + 20
      *
      * NATURE OF EMPLOYERS BUSINESS
      *
     O          E            DET              56
     O                       X160A1            + 58
      *
     O          E            DET              57
     O                       X160A2            + 58
      *
      * LOCATION OF PLANT WHERE INJURY OCCURRED
      *
     O          E            DET              58
     O                       X140A1            + 65
      *
     O          E            DET              60
     O                       X130A1            + 34
     O                       X130A2            + 12
     O                       X120A1            + 24
      * DATE OF INJURY
     O          E            DET              62
     O                       X1DOI             + 40 '  /  /  '
     O                       X1DAY             + 20
     O                       X1ITM             + 18 '  :  '
     O                       X1IAM             +  3
     O                       X1IPM             +  7
      * PAID FOR ENTIRE DAY
     O          E            DET              64
     O                       X1POI             + 57
     O                       X1DBDT            + 30 '  /  /  '
     O                       X1DBAM            +  5
     O                       X1DBPM            +  7
      * FIRST KNEW OF INJURY
     O          E            DET              67
     O                       X1DTRP            + 67 '  /  /  '
     O                       X1USNM            + 25
      * OCCUPATION WHEN INJURED
     O          E            DET              69
     O                       X1OCDS            + 51
      * TIME EMPLOYED / WAGES PER HOUR
     O          E            DET              71
     O              N81      X1YRS         4   + 51
     O              N81                        +  0 'y'
     O              N82      X1MTHS        4   +  2
     O              N82                        +  0 'm'
     O              N83      X1DAYS        4   +  2
     O              N83                        +  0 'd'
     O                       X110A1        2   + 24
      * HOURS PER DAY / WAGES PER DAY
     O          E            DET              73
     O                       X1HRSD        4   + 52
     O                       X110A2        2   + 21
     O                       X1NDAY            + 29
      * AVERAGE WEEKLY WAGE
     O          E            DET              75
     O                       X1AWW         2   + 59
      * BOARD, LODGING, FUEL
     O          E            DET              77
     O                       X110A3        2   + 93
     O                       X110A4            +  5
      * HOW INJURY OCCURRED
     O          E            DET              81
     O                       X1ACDS            + 28
     O          E            DET              83
     O                       X1ACD2            + 28
      * LIST ALL INJURIES
     O          E            DET              88
     O                       X160A3            + 28
     O          E            DET              89
     O                       X160A4            + 28
      * DATE RETURNED TO WORK
     O          E            DET              91
     O                       X1RTW             + 52 '  /  /  '
     O               84                        +  1 'at'
     O                       X110A5            +  1 '  :  '
     O                       X1AMPM            +  1
     O                       X120A2        2   + 25
     O                       X120A4            +  4
      *  AT WHAT OCCUPATION
     O          E            DET              92
     O                       X160A5            + 44
     O                       X1FSAL            + 37
      *  EMPLOYEE TREATED BY PHYSICIAN ?
     O          E            DET              94
     O                       X1PHY             + 61
      *  EMPLOYEE DIED ?
     O          E            DET              96
     O                       X1DTH             + 51
     O                       X1DOD             + 45 '  /  /  '
      *
     O          E            DET            3 99
     O          E            DET         3  3
     O          E            DET         3  3
     O          E            DET         3  3
     O          E            DET         3  3
     O          E            DET         3  2
     O                                         + 57 '.'
     O          E            DET
     O                       XXBOTM            + 57
      *
     OQSYSPRT   E            DET2           3 01
      *
     O          E            DET2           3 99
     O          E            DET2        3  3
     O          E            DET2        3  3
     O          E            DET2        3  3
     O          E            DET2
     O                                         + 57 '.'
     O          E            DET2        2  3
     O          E            DET2        3  2
     O          E            DET2
     O                       XXBOTM            + 57
      *
      *
     O* ==============================================================
     O*
     O* LABELS FOR EMPLOYER
     O*
     OQSYSPRT1  E            LABEL1         1 01
     O                       EMFND                8
     O                                            9 '-'
     O                       EMEMP#              18
     O                       EMDIV               23
     O                                           45 'FROI'
     O          E            LABEL1         1
     O                       EMNAME              45
     O          E            LABEL1         1
     O                       XXAD1               45
     O          E            LABEL1         1
     O                       XXAD2               45
     O          E            LABEL1         1
     O                       XX1CSZ              45
     O* ==============================================================
     O*
     O* LABELS FOR CLAIMANT
     O*
     OQSYSPRT2  E            LABEL2         1 01
     O                       CMFND                8
     O                                            9 '-'
     O                       CMFYR               12
     O                                           13 '-'
     O                       CMCASE              20
     O                                           45 'FROI'
     O          E            LABEL2         1
     O                       XXCNAM              40
     O          E            LABEL2         1
     O                       XXCAD1              40
     O          E            LABEL2         1
     O                       XXCAD2              40
     O          E            LABEL2         1
     O                       XXCCSZ              40
