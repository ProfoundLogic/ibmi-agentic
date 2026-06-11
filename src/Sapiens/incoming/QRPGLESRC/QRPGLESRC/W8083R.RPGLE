     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE W8083R - HES Eligibility Extract File - Extract Program   am
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*     CARRIES OUT THE CLAIMS ELIGIBILITY FILE EXTRACT           *
     H*     PROCESS                                                   *
     H*                                                               *
     H*  CALLED FROM: W8083C1 - HES Eligibility File Extract          *
     H*                        - Override Program                     *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*****************************************************************
     H*  CREATED: 11/03/09       PROGRAMMER: 227     PROJECT: 135MMX  *
     H*                          Copied from C136PNSRC/W8152R         *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  11/06/08  C:191  SUPPORT 65278                               *
     H*                   CLAIM EXPORT FILE                           *
     H*                                                               *
     H*  03/11/08  C:220  SUPPORT 65257                               *
     H*                   CLAIM EXPORT FILE IS COMING THROUGH AS A    *
     H*                   COMPLETE FILE                               *
     H*                                                               *
     H*  05/16/07  C:188  PROJECT 136MBR                              *
     H*                   Do not exclude denied claims                *
     H*                                                               *
     H*  10/25/05  C:163  PROJECT 136MBR                              *
     H*                   Managed Care/Prov Bill Interface Process    *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  FILES:                                                       *
     H*  SMCOL    PCO01: CO                                   Company *
     H*  SMORL    POR01: CO/PGM                              Override *
     H*  SMUPL    PUP01: USER                               User Parm *
     H*                                                               *
     H*  WMCMLG   PCM01: CMCO#  CMACL# CMFND               Claim File *
     H*                  CMFYR  CMCASE                                *
     H*                                                               *
     H*  WMCHL    PCH01: CHCO#  CHFND  CHFYR            Claim History *
     H*                  CHCASE CHEFDT CHTIME             Master File *
     H*                                                               *
     H*  WMFNL    PFN01: FNCO#  FNFND                Fund Master File *
     H*  WMCDL    PCD01: CDSS#             Claimant Demographics File *
     H*  WAHEP    PHE01:                         HES Eligibility File *
     H*                                                               *
     H*****************************************************************
     F*SMCOL   IF  E           K        DISK
     F*SMORL   IF  E           K        DISK
     F***SMUPL   IF  E           K        DISK
     FWMCMLG    IF   E           K DISK
     FWMCHL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F*** BEGIN ADD *** 10/25/05 **************************************
     FWMCDL     IF   E           K DISK
     F*** END   ADD *** 10/25/05 **************************************
     F*** BEG ADD *** 11/03/09 ****************************************
     FWDPAL5    IF   E           K DISK
     FWDF3L     IF   E           K DISK
     FWMREL     IF   E           K DISK
     FFMF1L     IF   E           K DISK
     FWMRPL     IF   E           K DISK
     FWMGCL     IF   E           K DISK
     FWMC1L     IF   E           K DISK
     FSADAL5    IF   E           K DISK
     FSMUPL1    IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWACML     IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     F*** END ADD *** 11/03/09 ****************************************
     F*** BEGIN CHG *** 10/25/05 **************************************
     F*** WACEP2  O   E                    DISK
     FWAHEP     O    E             DISK
     F*** END   CHG *** 10/25/05 **************************************

     D*** BEG ADD *** 11/03/09 ****************************************
     D*** END ADD *** 11/03/09 ****************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D                 DS
     D  CLM#                   1     15
     D  FND                    1      3  0
     D  FYR                    4      6  0
     D  CASE                   7     13  0
     D*** BEGIN ADD *** 10/25/05 **************************************
     D HEDATA         UDS
     D  HEDADS                 1     10
     D  HEDAMM                 1      2
     D  HEDAS1                 3      3
     D  HEDADD                 4      5
     D  HEDAS2                 6      6
     D  HEDACY                 7     10
     D*
     D                 DS
     D  RUNDAT                 1      8  0
     D  RUNDMM                 1      2
     D  RUNDDD                 3      4
     D  RUNDCY                 5      8
     D*** BEG ADD *** 11/03/09 ****************************************
     D                 DS
     D  ACTDAT                 1      8  0
     D  ACTDCY                 1      4  0
     D  ACTDMM                 5      6
     D  ACTDDD                 7      8
     D                 DS
     D  CLMPHN                 1     13
     D  CLMSP1                 1      1
     D  KKAREA                 2      4
     D  CLMSP2                 5      5
     D  KKPH#1                 6      8
     D  CLMSP3                 9      9
     D  KKPH#2                10     13
     D                 DS
     D  CPH#                   1      7
     D  CPH#1                  1      3
     D  CPH#2                  4      7
     D                 DS
     D  NWP                    1     50
     D                                     DIM(10)
     D  F1NW01                 1      5
     D  F1NW02                 6     10
     D  F1NW03                11     15
     D  F1NW04                16     20
     D  F1NW05                21     25
     D  F1NW06                26     30
     D  F1NW07                31     35
     D  F1NW08                36     40
     D  F1NW09                41     45
     D  F1NW10                46     50
     D*** END ADD *** 11/03/09 ****************************************
     D*** END   ADD *** 10/25/05 **************************************
     D*
     D*****************************************************************
     D* CONSTANTS
     D*****************************************************************
     D*
     D*** BEGIN CHG *** 10/25/05 **************************************
     D XTITL           C                   CONST('Medata Claims El-
     D                                     igibility Summar-
     D                                     y Report')
     D*** END   CHG *** 10/25/05 **************************************
     D DIGITS          C                   CONST('0123456789')

     C*****************************************************************
     C*   KEY LISTS
     C*****************************************************************
     C*
     C*   Company
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  Case History
     C*
     C     CHKY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C     CHKY2         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    KKEFDT
     C                   KFLD                    KKTIME
     C*** BEG ADD *** 11/03/09 ****************************************
     C*
     C*   PAYMENT ACTIVITY
     C*
     C     PAKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    KKEFDT
     C*
     C     PAKY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C     EMKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C     UPKY1         KLIST
     C                   KFLD                    CMCLEX
     C*
     C     UPKY2         KLIST
     C                   KFLD                    CMSUPR
     C*
     C     SAKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    KKPRIM
     C*
     C     F3KY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    KKJRST
     C*
     C     RPKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C*
     C     REKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    KKRGN#
     C*
     C     FMKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    KKSUB
     C                   KFLD                    KKFND
     C*** END ADD *** 11/03/09 ****************************************
     C*
     C*   Fund
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C*
     C*  Output Routing
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    KKPRT             1
     C*
     C*   User Parameter
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*** BEGIN CHG *** 10/25/05 **************************************
     C*
     C*   Claimant demographics
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*** END   CHG *** 10/25/05 **************************************
     C*
     C*****************************************************************
     C*   *LIKE Define Variables
     C*****************************************************************
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CHEFDT        KKEFDT
     C     *LIKE         DEFINE    CHTIME        KKTIME
     C     *LIKE         DEFINE    CHEFDT        X1FRDT
     C     *LIKE         DEFINE    CHEFDT        X1TODT
     C     *LIKE         DEFINE    CHEFDT        XXCODT
     C     *LIKE         DEFINE    CHEFDT        XXEFDT
     C*** BEG ADD *** 11/03/09 ****************************************
     C     *LIKE         DEFINE    DAPRIM        KKPRIM
     C     *LIKE         DEFINE    EMRGN#        KKRGN#
     C     *LIKE         DEFINE    F1SUB#        KKSUB
     C     *LIKE         DEFINE    F1FND         KKFND
     C     *LIKE         DEFINE    F3KST         KKJRST
     C*** END ADD *** 11/03/09 ****************************************
     C*****************************************************************
     C*   PARAMETER LISTS INCLUDING ENTRY PARMS
     C*****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@DCID           12
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
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*** BEGIN CHG *** 10/25/05 **************************************
     C*
     C     DTYMD         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YMD'        SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*** END   CHG *** 10/25/05 **************************************
     C*****************************************************************
     C*   Initialize
     C*****************************************************************
     C*
     C*  PGMSTR executes first
     C*
     C                   EXSR      PGMSTR
     C*
     C*  Print Routing Page
     C*
     C*
     C*  Read 1st record
     C*
     C                   READ      PCM01                                  99
     C     *IN99         DOWEQ     '0'
     C*
     C*  If subset criteria met
     C*
     C                   EXSR      SUBSET
     C*
     C     MEETSS        IFEQ      *ON
     C                   EXSR      ERRCK
     C*
     C     *IN90         IFEQ      *ON
     C     COUNTE        IFEQ      *ZEROS
     C                   ENDIF
     C                   ADD       1             COUNTE
     C                   ELSE
     C                   ADD       1             COUNTW
     C                   EXSR      WRITER
     C                   ENDIF
     C                   ENDIF                                                  MEETSS
     C*
     C*  Read next record
     C*
     C                   READ      PCM01                                  99
     C                   ENDDO                                                  WRK RECORD
     C*
     C*  Last Record processing
     C*
     C                   EXSR      CRTDOC
     C*
     C*  End of Processing
     C*
     C*** BEGIN ADD *** 10/25/05 **************************************
     C                   MOVEL     DSDATE        RUNDAT
     C                   MOVEL     RUNDMM        HEDAMM
     C                   MOVEL     '/'           HEDAS1
     C                   MOVEL     RUNDDD        HEDADD
     C                   MOVEL     '/'           HEDAS2
     C                   MOVEL     RUNDCY        HEDACY
     C*** END   ADD *** 10/25/05 **************************************
     C                   MOVE      *ON           *INLR
     C*****************************************************************
     C* CRTDOC - CREATE THE DOC NAME
     C*****************************************************************
     CSR   CRTDOC        BEGSR
     C*
     C     HIT           IFEQ      'Y'
     C*
     C                   TIME                    DSDTTM
     C*
     C     DSTIME        IFLE      120000
     C                   CALL      'SRPAST8'
     C     DSDATE        PARM      DSDATE        ##DATE            8 0
     C                   PARM      1             ##DAYS            5 0
     C                   PARM      *BLANKS       ##ERR             1
     C                   ENDIF
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT7            7
     C                   MOVE      XXDAT7        XXDAT6            6
     C*
     C     'HE'          CAT(P)    XXDAT6:0      @@DCID
     C                   CAT       '.TXT':0      @@DCID
     C*
     C                   ELSE
     C                   CLEAR                   @@DCID
     C                   ENDIF
     C*
     C                   ENDSR
     C*****************************************************************
     C* ERRCK - Check for errors
     C*****************************************************************
     CSR   ERRCK         BEGSR
     C*
     C                   SETOFF                                       90
     C*
     C* *IN90 = *ON, Write error detail line and not write WAHEP record
     C*
     C* Does SS# have any non-numeric characters
     C*
     C     DIGITS        CHECK     CMSS#         N                 2 0    90
     C*
     C                   ENDSR                                                  ERRCK
     C*****************************************************************
     C* PGMSTR - Set-up and Initialize
     C*****************************************************************
     CSR   PGMSTR        BEGSR
     C*
     C                   Z-ADD     *ZEROS        COUNTE            9 0
     C                   Z-ADD     *ZEROS        COUNTW            9 0
     C                   TIME                    DSDTTM
     C                   MOVE      'N'           HIT               1
     C                   MOVE      @@CO#         XXCO#
     C*
     C* GET THE GROUP NAME
     C*
     C     FNKY          CHAIN     PFN01                              95
     C     *IN95         IFEQ      *OFF
     C                   MOVEL(P)  FNNAME        XXFNNM           40
     C                   ELSE
     C                   MOVEL     *BLANKS       XXFNNM
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 10/25/05 **************************************
     C     HEDADS        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        RUNDT7            7 0
     C                   ELSE
     C                   MOVEL     HEDAMM        RUNDMM
     C                   MOVEL     HEDADD        RUNDDD
     C                   MOVEL     HEDACY        RUNDCY
     C                   Z-ADD     RUNDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RUNDT7
     C                   ENDIF
     C*** END   ADD *** 10/25/05 **************************************
     C                   ENDSR                                                  PGMSTR
     C*****************************************************************
     C* SUBSET - Check if member of records processed subset
     C*****************************************************************
     CSR   SUBSET        BEGSR
     C*
     C                   MOVE      *OFF          MEETSS            1
     C*
     C*  MEETSS = *ON, if to be processed
     C*
     C*** BEGIN DEL *** 05/16/07 **************************************
     C*** BEGIN ADD *** 10/25/05 **************************************
     C*****      CMDNY     IFNE 'Y'
     C*****      CMLTFL    ANDNE'R'
     C*****      CMDT      ANDGERUNDT7
     C*** BEGIN ADD *** 03/11/08 **************************************
     C**** BEG ADD **** 11/03/09 **************************************
     C     CMLTFL        IFEQ      'I'
     C     CMLTFL        OREQ      'M'
     C**** END ADD **** 11/03/09 **************************************
     C     CMDT          IFGE      RUNDT7
     C*** END   ADD *** 03/11/08 **************************************
     C*** END   ADD *** 10/25/05 **************************************
     C*** END   DEL *** 05/16/07 **************************************
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C                   Z-ADD     *HIVAL        KKTIME
     C*
     C**** BEG ADD **** 11/03/09 **************************************
     C     CMSTAT        IFEQ      'O'
     C                   MOVEL     *ON           MEETSS
     C                   ELSE
     C     CMSTAT        IFEQ      'C'
     C     PAKY          SETGT     PPA01
     C     PAKY1         READPE    PPA01                                  99
     C                   Z-ADD     KKEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         ACTDAT
     C     ACTDCY        SUB       2             ACTDCY
     C*
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*
     C     XXTRDT        IFGE      ACTDAT
     C                   MOVEL     *ON           MEETSS
     C                   ENDIF                                                  PATRDT>=ACTDAT
     C                   ENDIF                                                  CMSTAT='C'
     C                   ENDIF                                                  CMSTAT='O'
     C**** END ADD **** 11/03/09 **************************************
     C**** BEG ADD **** 11/06/08 **************************************
     C                   MOVEL     *ZEROS        XXEFDT
     C**** END ADD **** 11/06/08 **************************************
     C**** BEG DEL **** 11/03/09 **************************************
     C********   CHKY2     SETGTPCH01
     C********   CHKY1     REDPEPCH01                    99
     C********   *IN99     IFEQ *OFF
     C********   CHOCST    IFEQ 'O'
     C********             MOVEL*ON       MEETSS
     C********             ELSE
     C********   CHOCST    IFEQ 'C'
     C********             MOVELCHEFDT    XXEFDT
     C********             MOVEL*ON       MEETSS
     C********             ENDIF                           CHOCST = 'C'
     C********             ENDIF
     C********             ENDIF
     C**** END DEL **** 11/03/09 **************************************
     C*** BEGIN DEL *** 05/16/07 **************************************
     C*** BEGIN ADD *** 10/25/05 **************************************
     C                   ENDIF
     C*** BEGIN ADD *** 03/11/08 **************************************
     C                   ENDIF
     C*** END   ADD *** 03/11/08 **************************************
     C*** END   ADD *** 10/25/05 **************************************
     C*** END   DEL *** 05/16/07 **************************************
     C*
     C                   ENDSR                                                  SUBSET
     C*****************************************************************
     C* WRITER - WRITE WAHEP
     C*****************************************************************
     CSR   WRITER        BEGSR
     C*
     C                   CLEAR                   PHE01
     C*
     C                   MOVE      'Y'           HIT
     C*Claim Number
     C                   Z-ADD     CMFND         FND
     C                   Z-ADD     CMFYR         FYR
     C                   Z-ADD     CMCASE        CASE
     C                   MOVEL(P)  CLM#          HECLM#                         Claim Number
     C*Account ID
     C                   MOVEL(P)  '135'         HEACCT
     C*Accident Date
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         HEDTLS                         Accident Date
     C*
     C                   MOVE      CMSS#         HESS#                          Claimant SS#
     C*********            MOVEL'CAREWEST'HECONO           Company number
     C*
     C*** BEGIN ADD *** 10/25/05 **************************************
     C     CDKY          CHAIN     PCD01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*Date of Birth
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         HECDOB                         Birth Date
     C*
     C                   MOVEL(P)  CDSEX         HECSEX                         Sex
     C                   MOVEL(P)  CDLSN         HECLSN                         Claimant Last Name
     C                   MOVEL(P)  CDFSN         HECFSN                         Claimant First Name
     C                   MOVEL(P)  CDMID         HECMID                         Claimant Mid Initial
     C                   MOVEL(P)  CDADR1        HECAD1                         Claimant Address 1
     C                   MOVEL(P)  CDADR2        HECAD2                         Claimant Address 2
     C                   MOVEL     CDAREA        KKAREA
     C                   MOVE      CDPHN#        CPH#
     C                   MOVE      CPH#1         KKPH#1
     C                   MOVE      CPH#2         KKPH#2
     C                   MOVE      '('           CLMSP1
     C                   MOVE      ')'           CLMSP2
     C                   MOVE      '-'           CLMSP3
     C                   MOVE(P)   CLMPHN        HECPHN                         Claimant City
     C                   MOVEL(P)  CDCTY         HECCTY                         Claimant City
     C                   MOVEL(P)  CDST          HECST                          Claimant State
     C                   MOVE      CDZIP         HECZIP                         Claimant Zip
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C     CMLTFL        IFEQ      'I'
     C                   MOVEL(P)  'LT'          HELOSS
     C                   ELSE
     C     CMLTFL        IFEQ      'M'
     C                   MOVEL(P)  'MO'          HELOSS
     C                   ENDIF
     C                   ENDIF
     C*Jurisdiction State
     C                   MOVEL(P)  CMJRST        HEJRST                         Jurisdiction State
     C*Accident Description
     C     CMACDS        CAT(P)    CMACD2:1      ACCDSC          120
     C                   MOVEL(P)  ACCDSC        HEADSC                         Accident Desc
     C*NCCI Codes
     C                   MOVEL(P)  CMPART        HENCC1
     C                   MOVEL(P)  CMNATR        HENCC2
     C                   MOVEL(P)  CMCAUS        HENCC3
     C*Claim close date/reopen date and claim status
     C     CMSTAT        IFEQ      'O'
     C                   MOVEL     '0'           HECSTS
     C                   ELSE
     C     CMSTAT        IFEQ      'C'
     C                   MOVEL     '1'           HECSTS
     C                   ENDIF
     C                   ENDIF
     C     CHKY2         SETGT     PCH01
     C     CHKY1         READPE    PCH01                                  99
     C     *IN99         IFEQ      *OFF
     C     CHOCST        IFEQ      'O'
     C                   MOVEL     '0'           HECSTS
     C                   MOVEL     CHEFDT        XXEFDT
     C                   Z-ADD     XXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C     CHKY1         READPE    PCH01                                  98
     C     *IN98         IFEQ      *OFF
     C     CHOCST        ANDEQ     'C'
     C                   MOVEL     SRDTO         HERODT                                     f Claim
     C                   MOVEL     '3'           HECSTS
     C                   ENDIF
     C                   ELSE
     C     CHOCST        IFEQ      'C'
     C                   MOVEL     CHEFDT        XXEFDT
     C                   Z-ADD     XXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         HECLDT                                     f Claim
     C                   MOVEL     '1'           HECSTS
     C                   ENDIF                                                  CHOCST = 'C'
     C                   ENDIF
     C                   ENDIF
     C* Claim Status (Deleted)
     C     CHKY1         CHAIN     PCM02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     '6'           HECSTS
     C                   ENDIF
     C*
     C*Locked Claim Indicator
     C                   MOVEL(P)  CMDNY         HELOCK
     C*
     C*Controverted Indicator
     C                   MOVEL(P)  CMCVFL        HECVFL
     C*
     C*Coverage Indicator
     C                   MOVEL(P)  CMCOVT        HECOVI
     C*
     C*Supportive Care Effective Date
     C     CMDNY         IFEQ      'Y'
     C                   Z-ADD     CMDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         HESCDT
     C                   ENDIF
     C*
     C*Recission Date
     C                   Z-ADD     CMDRES        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         HEREDT
     C*
     C*Employer Name
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  EMNAME        HEENAM
     C                   MOVEL(P)  EMMAD1        HEEAD1
     C                   MOVEL(P)  EMMAD2        HEEAD2
     C                   MOVEL(P)  EMMCTY        HEECTY
     C                   MOVEL(P)  EMMST         HEEST
     C                   MOVE      EMMZIP        HEEZIP
     C                   MOVEL(P)  EMRGN#        HERGN
     C                   MOVE(P)   'N'           PEFFFL            1
     C     EMEFD5        IFNE      0
     C     PEFFFL        ANDEQ     'N'
     C                   Z-ADD     EMEFD5        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         HEPEDT
     C                   Z-ADD     EMCAD5        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         HEPTDT
     C                   MOVE      'Y'           PEFFFL
     C                   ENDIF
     C     EMEFD4        IFNE      0
     C     PEFFFL        ANDEQ     'N'
     C                   Z-ADD     EMEFD4        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         HEPEDT
     C                   Z-ADD     EMCAD4        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         HEPTDT
     C                   MOVE      'Y'           PEFFFL
     C                   ENDIF
     C     EMEFD3        IFNE      0
     C     PEFFFL        ANDEQ     'N'
     C                   Z-ADD     EMEFD3        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         HEPEDT
     C                   Z-ADD     EMCAD3        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         HEPTDT
     C                   MOVE      'Y'           PEFFFL
     C                   ENDIF
     C     EMEFD2        IFNE      0
     C     PEFFFL        ANDEQ     'N'
     C                   Z-ADD     EMEFD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         HEPEDT
     C                   Z-ADD     EMCAD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         HEPTDT
     C                   MOVE      'Y'           PEFFFL
     C                   ENDIF
     C     EMEFD1        IFNE      0
     C     PEFFFL        ANDEQ     'N'
     C                   Z-ADD     EMEFD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         HEPEDT
     C                   Z-ADD     EMCAD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         HEPTDT
     C                   MOVE      'Y'           PEFFFL
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*Region Description
     C     EMRGN#        IFNE      0
     C                   Z-ADD     EMRGN#        KKRGN#
     C     REKY          CHAIN     PRE01                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL(P)  REDESC        HERGDS
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*Adjuster First Name
     C     UPKY1         CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  UPUSNM        HEAFSN
     C*
     C*Adjuster Phone Number
     C                   MOVEL     UPPHN#        CPH#
     C                   MOVEL     UPPHNA        KKAREA
     C                   MOVEL     CPH#1         KKPH#1
     C                   MOVEL     CPH#2         KKPH#2
     C     CLMPHN        CAT(P)    'ext':1       HEAWPH
     C                   MOVE(P)   UPEXT#        KKEXT#            4
     C     HEAWPH        CAT(P)    KKEXT#:1      HEAWPH
     C*
     C*Adjuster Email
     C                   MOVEL(P)  UPMAIL        HEAEML
     C*
     C*Adjuster ID
     C                   MOVEL(P)  UPSTAF        HEAID
     C*
     C*Adjuster Office
     C                   MOVEL(P)  UPAL5         HEAFOF
     C*
     C                   ENDIF
     C*
     C*Alternate Contact ID
     C*
     C     UPKY2         CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  UPSTAF        HEACID
     C                   ENDIF
     C*
     C*ICD9 Code1
     C                   MOVEL     'Y'           KKPRIM
     C     SAKY          CHAIN     PDA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  DAICD9        HEICD1
     C                   ENDIF
     C*
     C*Apportionment Indicator
     C     CHKY1         CHAIN     PC101                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  C1APIN        HEAPPI
     C                   MOVE      C1APCT        HEAPPC
     C                   ENDIF
     C*
     C*Insurer Information
     C                   MOVE      CMJRST        KKJRST
     C     F3KY          CHAIN     PF301                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       KKJRST
     C     F3KY          CHAIN     PF301                              60
     C                   ENDIF
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ALL'0'       HEIFEI
     C                   MOVE      F3IFEI        HEIFEI
     C                   MOVE      F3ZIP         HEIZIP
     C                   MOVEL(P)  F3INAM        HEINAM
     C                   MOVEL(P)  F3ADR1        HEIADR
     C                   MOVEL(P)  F3CTY         HEICTY
     C                   MOVEL(P)  F3ST          HEIST
     C                   MOVE      F3FEIN        HETFEI
     C                   MOVE      F3ZIP         HETZIP
     C                   MOVEL(P)  F3NAME        HETNAM
     C                   MOVEL(P)  F3ADR1        HETADR
     C                   MOVEL(P)  F3CTY         HETCTY
     C                   MOVEL(P)  F3ST          HETST
     C                   MOVEL(P)  F3ICOD        HEICOD
     C                   MOVEL(P)  F3TCOD        HETCOD
     C                   ENDIF
     C*
     C*Policy Identifier Number
     C     RPKY          CHAIN     PRP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     RPCAP#        HEPOL#
     C                   ENDIF
     C*
     C*NCCI Carrier Code
     C     FNKY          CHAIN     PGC01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GCDCAR        HENCAR
     C                   ENDIF
     C*
     C*Vendor Network ID
     C                   MOVE      CMFND         @@FND             3
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM      @@CO#         #1CO#             3
     C                   PARM      @@FND         #1WCF#            3
     C                   PARM      *BLANKS       #1SUB#            5
     C                   PARM      *BLANKS       #1FND             3
     C     ##RTCD        IFEQ      ' '
     C                   MOVEL     #1SUB#        KKSUB
     C                   MOVEL     #1FND         KKFND
     C                   ENDIF
     C     FMKY          CHAIN     PF101                              99
     C     *IN99         IFEQ      *OFF
     C     1             DO        10            X                 2 0
     C     NWP(X)        IFEQ      'PPC'
     C     NWP(X)        OREQ      'EIA'
     C                   MOVE      NWP(X)        HEVEND
     C     NWP(X)        IFEQ      'EIA'
     C                   LEAVE
     C                   ENDIF
     C                   ADD       1             X
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
     C*** END   ADD *** 10/25/05 **************************************
     C                   WRITE     PHE01
     C*
     C                   ENDSR                                                  WRITER
     C*****************************************************************
