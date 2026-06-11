     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8051R - CLAIMS ELIGIBILITY FILE EXTRACT - EXTRACT PGM
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*     CARRIES OUT THE CLAIMS ELIGIBILITY FILE EXTRACT           *
     H*     PROCESS                                                   *
     H*                                                               *
     H*  CALLED FROM: W8151C - CLAIMS ELIGIBILITY FILE EXTRACT        *
     H*                        OVERRIDE PROGRAM                       *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*****************************************************************
     H*  CREATED: 04/21/04       PROGRAMMER: 62      PROJECT: 51MFN   *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 08/08/07 C:193 PROJECT: MC300                                 *
     H*                CLAIMS UPDATE/ELIGIBILITY FILE                 *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  FILES:                                                       *
     H*  SMCOL    PCO01: CO                                   Company *
     H*  SMORL    POR01: CO/PGM                              Override *
     H*  SMUPL    PUP01: USER                               User Parm *
     H*                                                               *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     F*** BEGIN CHG *** 08/08/07 **************************************
     F*******WMCMLG  IF  E           K        DISK
     F*******WMCHL   IF  E           K        DISK
     F*******WMFNL   IF  E           K        DISK
     FWMCMLG    IF   E           K DISK    USROPN
     FWMCHL     IF   E           K DISK    USROPN
     FWMFNL     IF   E           K DISK    USROPN
     F*** END CHG ***** 08/08/07 **************************************
     F*** BEGIN ADD *** 08/08/07 **************************************
     FWMCDL     IF   E           K DISK    USROPN
     FWMEML     IF   E           K DISK    USROPN
     FFADAL2    IF   E           K DISK
     FFMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FFMCHL2    IF   E           K DISK
     F                                     RENAME(PCH01:PCH02)
     FFMCDL     IF   E           K DISK
     F                                     RENAME(PCD01:PCD02)
     FFMEML     IF   E           K DISK
     F                                     RENAME(PEM01:PEM02)
     FFAPPL1    IF   E           K DISK
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     F*** END ADD ***** 08/08/07 **************************************
     FWACEP2    O    E             DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
     F*** END ADD ***** 08/08/07 **************************************

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
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
     D*
     D*****************************************************************
     D* CONSTANTS
     D*****************************************************************
     D*
     D XTITL           C                   CONST('Claims Eligibili-
     D                                     ty Summary Report')
     D DIGITS          C                   CONST('0123456789')
     F*** BEGIN ADD *** 08/08/07 **************************************

     IPCM02
     I              CMOSCL                      CMOXCL
     I*
     I*
      *****************************************************************

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
     C*
     C*** BEGIN ADD *** 08/08/07 **************************************
     C     CHKY3         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    KKEFDT
     C*
     C     CHKY4         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*** END ADD ***** 08/08/07 **************************************
     C*
     C*   Fund
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C*** BEGIN ADD *** 08/08/07 **************************************
     C*
     C*   Diagnosis Code
     C*
     C     DAKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    XXSUB#            5 0
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*   Claimant demographics
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*   Claim Physician/Provider Specialist
     C*
     C     PPKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    XXSUB#            5 0
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*  Employer
     C*
     C     EMKY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C     EMKY2         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*** END ADD ***** 08/08/07 **************************************
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
     C*
     C*** BEGIN ADD *** 08/08/07 **************************************
     C     UPKY1         KLIST
     C                   KFLD                    XXSTAF
     C*** END ADD ***** 08/08/07 **************************************
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
     C*** BEGIN ADD *** 08/08/07 **************************************
     C     *LIKE         DEFINE    UPSTAF        XXSTAF
     C*** END ADD ***** 08/08/07 **************************************
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
     C                   EXSR      SRRTE
     C*
     C*** BEGIN ADD *** 08/08/07 *************************************
     C                   OPEN      WMFNL
     C*** END ADD ***** 08/08/07 *************************************
     C*** BEGIN ADD *** 08/08/07 *************************************
     C*  CALL TO SA001R TO FIND OUT WHETHER CLIENT HAS WCMS
     C                   CALL      'SA001R'
     C                   PARM                    @@WCIF            1            WC INST
     C                   PARM                    @@MCIF            1            MC INST
     C                   PARM                    @@WCIN            1            WC INTERFACE
     C                   PARM                    @@AR              1            MC AR INST
     C                   PARM                    @@NTWK            1            NETWORK INST
     C                   PARM                    @@UR              1            PRECERT INST
     C                   PARM                    @@MED             1            MEDMGMT INST
     C                   PARM                    @@EDI             1            EDI INST
     C                   PARM                    @@WCDK            1            WC DARTS KEY
     C                   PARM                    @@RHAB            1            REHAB INST
     C                   PARM                    @@MSS             1            MS
     C                   PARM                    @@FSCH            1            FEE SCHEDULE
     C                   PARM                    @@PRVB            1            PROVIDER BILL
     C*
     C* IF WCMS THEN READ FILE WMCMP
     C     @@WCIF        IFEQ      'Y'
     C*** END ADD ***** 08/08/07 *************************************
     C*  Read 1st record
     C*
     C*** BEGIN ADD *** 08/08/07 *************************************
     C                   OPEN      WMCMLG
     C                   OPEN      WMCHL
     C*** END ADD ***** 08/08/07 *************************************
     C                   READ      PCM01                                  99
     C                   EXCEPT    HEAD
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
     C                   EXCEPT    HEAD1
     C                   ENDIF
     C                   ADD       1             COUNTE
     C                   EXSR      BREAK2
     C                   EXSR      SRDET
     C                   ELSE
     C                   ADD       1             COUNTW
     C*** BEGIN ADD *** 08/08/07 *************************************
     C                   OPEN      WMCDL
     C                   OPEN      WMEML
     C*** END ADD ***** 08/08/07 *************************************
     C                   EXSR      WRITER
     C*** BEGIN ADD *** 08/08/07 *************************************
     C                   CLOSE     WMCDL
     C                   CLOSE     WMEML
     C*** END ADD ***** 08/08/07 *************************************
     C                   ENDIF
     C                   ENDIF                                                  MEETSS
     C*
     C*  Read next record
     C*
     C                   READ      PCM01                                  99
     C                   ENDDO                                                  WRK RECORD
     C*** BEGIN ADD *** 08/08/07 *************************************
     C                   CLOSE     WMCMLG
     C                   CLOSE     WMCHL
     C*** END ADD ***** 08/08/07 *************************************
     C*
     C*** BEGIN ADD *** 08/08/07 *************************************
     C* IF NON WCMS THEN READ FILE FMCMP
     C                   ELSE
     C*
     C*  Read 1st record
     C*
     C                   READ      PCM02                                  99
     C                   EXCEPT    HEAD
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
     C                   EXCEPT    HEAD1
     C                   ENDIF
     C                   ADD       1             COUNTE
     C                   EXSR      BREAK2
     C                   EXSR      SRDET
     C                   ELSE
     C                   ADD       1             COUNTW
     C                   EXSR      WRINWC
     C                   ENDIF
     C                   ENDIF                                                  MEETSS
     C*
     C*  Read next record
     C*
     C                   READ      PCM02                                  99
     C                   ENDDO                                                  WRK RECORD
     C*
     C                   ENDIF
     C                   CLOSE     WMFNL
     C*** END ADD ***** 08/08/07 *************************************
     C*  Last Record processing
     C*
     C                   EXSR      BREAK1
     C                   EXCEPT    TOTAL
     C*
     C                   EXSR      CRTDOC
     C*
     C*  End of Processing
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       kkPrt = '0';
       Chain ORKY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      *ON           *INLR
     C*****************************************************************
     C* BREAK1 - CHECK IF AT END OF PAGE AND BREAK IF SO
     C*****************************************************************
     CSR   BREAK1        BEGSR
     C*
     C     *INOA         IFEQ      *ON
     C*
     C                   EXCEPT    HEAD
     C                   SETOFF                                       OA
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  BREAK
     C*****************************************************************
     C* BREAK2 - CHECK IF AT END OF PAGE AND BREAK IF SO
     C*****************************************************************
     CSR   BREAK2        BEGSR
     C*
     C     *INOA         IFEQ      *ON
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   SETOFF                                       OA
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  BREAK
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
     C     'CE'          CAT(P)    XXDAT6:0      @@DCID
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
     C* *IN90 = *ON, Write error detail line and not write WACEP record
     C*
     C* Does SS# have any non-numeric characters
     C*
     C     DIGITS        CHECK     CMSS#         N                        90
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
     C*
     C                   MOVE      @@CO#         XXCO#
     C*
     C                   ENDSR                                                  PGMSTR
     C*****************************************************************
     C* SRDET - Write Detail Record
     C*****************************************************************
     CSR   SRDET         BEGSR
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C*
     C*  Detail record print
     C*
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C                   EXCEPT    DETAIL
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C*
     C                   ENDSR                                                  SRDET
     C*****************************************************************
     C* SRRTE - Print routing page
     C*****************************************************************
     CSR   SRRTE         BEGSR
     C*
     C*   GET COMPANY, USER, INFO.
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C     UPKY          CHAIN     PUP01                              60
     C*
     C*   Output Routing - detail report 0
     C*
     C                   MOVE      '0'           KKPRT
     C     ORKY          CHAIN     POR01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C* Left Justify Title in Field
     C*
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF
     C                   ENDIF
     C*
     C*  User Name if no Distribution specified in routing
     C*
     C     ORDIS1        IFEQ      *BLANKS
     C     ORDIS2        ANDEQ     *BLANKS
     C     ORDIS3        ANDEQ     *BLANKS
     C     ORDIS4        ANDEQ     *BLANKS
     C                   MOVEL(P)  UPUSNM        ORDIS1
     C                   ENDIF
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  XTITL         ORTITL
     C*
     C                   ENDIF
     C*
     C*  Center Title in Field
     C*
     C                   MOVEL(P)  ORTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        ORTITL
     C*
     C* Center Company name in display field
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        CONAME
     C*
     C                   ENDSR                                                  SRRTE
     C*****************************************************************
     C* SUBSET - Check if member of records processed subset
     C*****************************************************************
     CSR   SUBSET        BEGSR
     C*
     C                   MOVE      *OFF          MEETSS            1
     C*
     C*  MEETSS = *ON, if to be processed
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C                   Z-ADD     *HIVAL        KKTIME
     C*
     C*** BEGIN ADD *** 08/08/07 **************************************
     C     @@WCIF        IFEQ      'Y'
     C*** END ADD ***** 08/08/07 **************************************
     C     CHKY2         SETGT     PCH01
     C     CHKY1         READPE    PCH01                                  99
     C*** BEGIN ADD *** 08/08/07 **************************************
     C                   ELSE
     C     CHKY3         SETGT     PCH02
     C     CHKY4         READPE    PCH02                                  99
     C                   ENDIF
     C*** END ADD ***** 08/08/07 **************************************
     C     *IN99         IFEQ      *OFF
     C     CHOCST        IFEQ      'O'
     C                   MOVEL     *ON           MEETSS
     C                   ENDIF
     C                   ENDIF
     C*
     C* IS THIS A TEST GROUP
     C*
     C     FNKY          CHAIN     PFN01                              95
     C     *IN95         IFEQ      *OFF
     C     FNTEST        ANDNE     'Y'
     C                   MOVEL     *ON           MEETSS
     C                   ENDIF
     C*
     C                   ENDSR                                                  SUBSET
     C*****************************************************************
     C* WRITER - WRITE WACEP
     C*****************************************************************
     CSR   WRITER        BEGSR
     C*
     C                   CLEAR                   PCE01
     C*
     C                   Z-ADD     CMFND         FND
     C                   Z-ADD     CMFYR         FYR
     C                   Z-ADD     CMCASE        CASE
     C                   MOVEL(P)  CLM#          CECLM#
     C*
     C*** BEGIN ADD *** 08/08/07 **************************************
     C*** CALL FA700R TO RETRIEVE SUB#
     C                   MOVEL     CMCO#         XCO#              3
     C                   MOVEL     CMFND         XWCF#             3
     C                   CALL      'FA700R'
     C                   PARM      ' '           @@RTCD            1
     C                   PARM                    XCO#
     C                   PARM                    XWCF#
     C                   PARM      ' '           @@SUB#            5
     C                   PARM      ' '           @@FND             3
     C*
     C                   MOVE      @@SUB#        XXSUB#
     C     DAKY          CHAIN     PDA01                              70
     C     *IN70         IFEQ      '0'
     C                   MOVEL     DAICD9        CEDIAG
     C                   ENDIF
     C*
     C     CHKY2         SETGT     PCH01
     C     CHKY1         READPE    PCH01                                  99
     C     *IN99         IFEQ      *OFF
     C     CHOCST        IFEQ      'C'
     C                   Z-ADD     CHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         CECLDT
     C                   ENDIF
     C                   ENDIF
     C*** END ADD ***** 08/08/07 **************************************
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         CEACDT
     C*
     C*** BEGIN CHG *** 08/08/07 **************************************
     C**********           MOVELCMLSN     CELSN     P
     C**********           MOVELCMFSN     CEFSN     P
     C     CDKY          CHAIN     PCD01                              62
     C     *IN62         IFEQ      '0'
     C                   MOVE(P)   CDLSN         CELSN
     C                   MOVE(P)   CDFSN         CEFSN
     C                   MOVEL     CDADR1        CEADD
     C                   MOVEL     CDCTY         CECTY
     C                   MOVE      CDST          CEST
     C                   MOVE      CDZIP         CEZIP
     C                   MOVE      CDPHN#        CEPHN
     C*
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         CEDOB
     C*
     C                   MOVE      CDSEX         CESEX
     C                   ENDIF
     C*** END CHG ***** 08/08/07 **************************************
     C                   MOVE      CMSS#         CESS#
     C                   MOVE      CMLTFL        CELTFL
     C*** BEGIN ADD *** 08/08/07 **************************************
     C                   MOVEL     CMACDS        CEINJ
     C                   MOVE      CMSTAT        CESTS
     C*
     C                   Z-ADD     CMIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         CEETDT
     C*
     C     PPKY          CHAIN     PPP01                              71
     C     *IN71         IFEQ      '0'
     C                   MOVE      PPFEIN        CEPDEA
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       CERDE1
     C                   MOVE      *BLANKS       CERDE2
     C                   MOVE      *BLANKS       CERDE3
     C     EMKY1         CHAIN     PEM01                              67
     C     *IN67         IFEQ      '0'
     C                   MOVEL     EMNAME        CEEMPN
     C                   MOVEL     EMMAD1        CEADDR
     C                   MOVEL     EMMCTY        CEECTY
     C                   MOVE      EMMST         CEEST
     C                   MOVE      EMMZIP        CEEZIP
     C                   ENDIF
     C*
     C                   MOVE      CMCLEX        XXSTAF
     C     UPKY1         CHAIN     PUP02                              68
     C     *IN68         IFEQ      '0'
     C                   MOVEL     UPUSNM        CEADJ
     C                   MOVEL     UPPHNA        CEAPHN
     C                   MOVE      UPPHN#        CEAPHN
     C                   MOVEL     UPFAXA        CEAFAX
     C                   MOVE      UPFAX#        CEAFAX
     C                   MOVEL     UPMAIL        CEAEML
     C                   MOVE      UPEXT#        CEAEXT
     C                   ENDIF
     C*
     C                   MOVE      CMSUPR        XXSTAF
     C     UPKY1         CHAIN     PUP02                              68
     C     *IN68         IFEQ      '0'
     C                   MOVEL     UPUSNM        CECMGR
     C                   MOVEL     UPPHNA        CECPHN
     C                   MOVE      UPPHN#        CECPHN
     C                   MOVEL     UPFAXA        CECFAX
     C                   MOVE      UPFAX#        CECFAX
     C                   MOVEL     UPMAIL        CECEML
     C                   MOVE      UPEXT#        CECEXT
     C                   ENDIF
     C*** END ADD ***** 08/08/07 **************************************
     C                   MOVE      'Y'           HIT
     C*
     C                   WRITE     PCE01
     C*
     C                   ENDSR                                                  WRITER
     C*** BEGIN ADD *** 08/08/07 **************************************
     C*****************************************************************
     C* WRINWC - WRITE WACEP FOR NON WCMS CLIENT
     C*****************************************************************
     CSR   WRINWC        BEGSR
     C*
     C                   CLEAR                   PCE01
     C*
     C                   Z-ADD     CMFND         FND
     C                   Z-ADD     CMFYR         FYR
     C                   Z-ADD     CMCASE        CASE
     C                   MOVEL(P)  CLM#          CECLM#
     C                   MOVEL(P)  CMSUB#        CESUB#
     C*
     C                   MOVEL     CMSUB#        XXSUB#
     C*** MOVE DIAGNOSIS FIELD FROM FADAP
     C*
     C     DAKY          CHAIN     PDA01                              70
     C     *IN70         IFEQ      '0'
     C                   MOVEL     DAICD9        CEDIAG
     C                   ENDIF
     C*
     C     CHKY3         SETGT     PCH02
     C     CHKY4         READPE    PCH02                                  99
     C     *IN99         IFEQ      *OFF
     C     CHOCST        IFEQ      'C'
     C                   Z-ADD     CHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         CECLDT
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         CEACDT
     C*
     C     CDKY          CHAIN     PCD02                              62
     C     *IN62         IFEQ      '0'
     C                   MOVE(P)   CDLSN         CELSN
     C                   MOVE(P)   CDFSN         CEFSN
     C                   MOVEL     CDADR1        CEADD
     C                   MOVEL     CDCTY         CECTY
     C                   MOVE      CDST          CEST
     C                   MOVE      CDZIP         CEZIP
     C                   MOVE      CDPHN#        CEPHN
     C                   MOVE      CDDTOB        CEDOB
     C                   MOVE      CDSEX         CESEX
     C                   ENDIF
     C*
     C                   MOVE      CMSS#         CESS#
     C                   MOVE      CMLTFL        CELTFL
     C                   MOVEL     CMACDS        CEINJ
     C                   MOVE      CMSTAT        CESTS
     C*
     C                   Z-ADD     CMIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         CEETDT
     C*
     C     PPKY          CHAIN     PPP01                              71
     C     *IN71         IFEQ      '0'
     C                   MOVE      PPFEIN        CEPDEA
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       CERDE1
     C                   MOVE      *BLANKS       CERDE2
     C                   MOVE      *BLANKS       CERDE3
     C*
     C     EMKY2         CHAIN     PEM02                              67
     C     *IN67         IFEQ      '0'
     C                   MOVEL     EMNAME        CEEMPN
     C                   MOVEL     EMMAD1        CEADDR
     C                   MOVEL     EMMCTY        CEECTY
     C                   MOVE      EMMST         CEEST
     C                   MOVE      EMMZIP        CEEZIP
     C                   ENDIF
     C*
     C                   MOVE      CMCLAD        XXSTAF
     C     UPKY1         CHAIN     PUP02                              68
     C     *IN68         IFEQ      '0'
     C                   MOVEL     UPUSNM        CEADJ
     C                   MOVEL     UPPHNA        CEAPHN
     C                   MOVE      UPPHN#        CEAPHN
     C                   MOVEL     UPFAXA        CEAFAX
     C                   MOVE      UPFAX#        CEAFAX
     C                   MOVEL     UPMAIL        CEAEML
     C                   MOVE      UPEXT#        CEAEXT
     C                   ENDIF
     C*
     C                   MOVE      CMCLSU        XXSTAF
     C     UPKY1         CHAIN     PUP02                              68
     C     *IN68         IFEQ      '0'
     C                   MOVEL     UPUSNM        CECMGR
     C                   MOVEL     UPPHNA        CECPHN
     C                   MOVE      UPPHN#        CECPHN
     C                   MOVEL     UPFAXA        CECFAX
     C                   MOVE      UPFAX#        CECFAX
     C                   MOVEL     UPMAIL        CECEML
     C                   MOVE      UPEXT#        CECEXT
     C                   ENDIF
     C*
     C                   MOVE      'Y'           HIT
     C*
     C                   WRITE     PCE01
     C*
     C                   ENDSR                                                  WRINWS
     C*** END ADD ***** 08/08/07 **************************************
     C*****************************************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             ORDIS1              56
     O**********
     O**********E            ROUTE          1
     O**********             ORDIS2              56
     O**********
     O**********E            ROUTE          1
     O**********             ORDIS3              56
     O**********
     O**********E            ROUTE          1
     O**********             ORDIS4              56
     O**********
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              56
     O**********
     O          E            ROUTE          1
     O                       ORDIS2              56
     O**********
     O          E            ROUTE          1
     O                       ORDIS3              56
     O**********
     O          E            ROUTE          1
     O                       ORDIS4              56
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O*
     O          E            ROUTE       2  2
     O                                           24 'REPORT SELECTIONS:'
     O*
     O          E            ROUTE          2
     O                                           29 'COMPANY  . . . . . :'
     O                       XXCO#         Z   +001
     O*
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE
     O***                                25 '(C) COPYRIGHT PROGRESSI'
     O***                              +000 'VE DATA SOLUTIONS. INC.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
     O          E            HEAD           1 03
     O                       DSTIME               8 '  :  :  '
     O                       CONAME              80
     O                       DSDATE        Y    132
     O*
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              90
     O                                          125 'PAGE:'
     O                       PAGE          Z    132
     O*
     O          E            HEAD1       1  1
     O                                           41 'Accident'
     O*
     O          E            HEAD1          2
     O                                            3 'Grp'
     O                                           17 'Claim number'
     O                                           39 'Date'
     O                                           50 'SS#'
     O*
     O          E            DETAIL         1
     O                       CMFND         4      3
     O                       CMACL#            +002
     O                       XXACDT            +002 '  /  /    '
     O                       CMSS#             +002
     O*
     O          E            TOTAL       1  1
     O                                           20 'Number of records no'
     O                                           32 't extracted:'
     O                       COUNTE            +002 '       0 '
     O*
     O          E            TOTAL          1
     O                                           20 'Number of records ex'
     O                                           32 'tracted    :'
     O                       COUNTW            +002 '       0 '
     O*
