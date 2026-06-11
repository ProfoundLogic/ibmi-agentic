     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W8155R - ANCILLARY MEDICAL ELIGIBILITY FILE EXPORT PGM
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*     CARRIES OUT THE ANCILLARY MEDICAL ELIGIBILITY FILE EXPORT *
     H*     PROCESS                                                   *
     H*                                                               *
     H*  CALLED FROM: W8155C - ANCILLARY MEDICAL ELIGIBILITY EXTRACT  *
     H*                        OVERRIDE PROGRAM                       *
     H*                                                               *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*****************************************************************
     H*  CREATED: 12/09/10     PROGRAMMER: C:251     PROJECT: 45MIA   *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
      * 08/26/14 C:307 Project: SY354                                 *
      *                -Implement Routing Page Changes.               *
      *                                                               *
     H* 04/10/12 C:277 Support: 79006 Project 45MIQ                   *
     H*                - Add Logic to use CMDOD if CMDOD <> 0         *
     H*                                                               *
     H* 02/16/12 C:277 Support: 79006 Project 45MIQ                   *
     H*                -Add Logic to avoid to write closed claim      *
     H*                -into WATRP5 file                              *
     H*                                                               *
     H* 11/22/11 C:277 PROJECT: 45MIQ PRISS #30                       *
     H*                -Correct the Termination Date population       *
     H*                                                               *
     H* 10/31/11 C:277 PROJECT: 45MIQ                                 *
     H*                -Claim Eligibility: Select records based on    *
     H*                -Claim field changes                           *
     H*                                                               *
     H* 06/17/11 C:079 Support: 77076 Project 45MIA                   *
     H*                -Modify logic not to load the previous date    *
     H*                -into DSDATE                                   *
     H*                                                               *
     H* 06/10/11 C:251 Support: 77076 Project 45MIA                   *
     H*                -Modify Subset to be on CMDT LE AMDATA date    *
     H*                                                               *
     H* 05/09/11 C:079 Project: 45MIA Priss 32                        *
     H*                -Default the Plan ID                           *
     H*                                                               *
     H* 04/12/11 C:251 Project: 45MIA Priss 29                        *
     H*                -Modify to include Denied claim in extract     *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  FILES:                                                       *
     H*  SMCOL    PCO01: CO                                   Company *
     H*  SMORL    POR01: CO/PGM                              Override *
     H*                                                               *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FWMCMLG    IF   E           K DISK    USROPN
     FWMCHL     IF   E           K DISK    USROPN
     FWMCDL     IF   E           K DISK    USROPN
     FWMCXL     IF   E           K DISK
     FWACEP5    O    E             DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
     F*** Beg Add *** 10/31/11 ***********************************
     FWATRL5    UF A E           K DISK
     F*** END ADD *** 10/31/11 ***********************************

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
     D AMDATA         UDS
     D  AMDATE                 1     10
     D  AMDAMM                 1      2
     D  AMDAS1                 3      3
     D  AMDADD                 4      5
     D  AMDAS2                 6      6
     D  AMDACY                 7     10
     D*
     D                 DS
     D  RUNDAT                 1      8  0
     D  RUNDMM                 1      2
     D  RUNDDD                 3      4
     D  RUNDCY                 5      8
     D*
     D*****************************************************************
     D* CONSTANTS
     D*****************************************************************
     D*
     D XTITL           C                   CONST('Ancillary Medica-
     D                                     l Eligibility Su-
     D                                     mmary Report')
     D DIGITS          C                   CONST('0123456789')
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
     C*
     C*  Claim Control File
     C*
     C     CXKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C*
     C*   Fund
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C*
     C*   Claimant demographics
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  Output Routing
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    KKPRT             1
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*** Beg Add *** 10/31/11 ***********************************
     C     TRKY          KLIST
     C                   KFLD                    KKMEMB
     C*** End Add *** 10/31/11 ***********************************
     C*
     C*****************************************************************
     C*   *LIKE Define Variables
     C*****************************************************************
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CHEFDT        KKEFDT
     C     *LIKE         DEFINE    CHTIME        KKTIME
     C*** Beg Add *** 10/31/11 ***********************************
     C     *LIKE         DEFINE    TRMEMB        KKMEMB
     C     *LIKE         DEFINE    CDZIP         XXZIPF
     C     *LIKE         DEFINE    TRDTOB        XXDTOB
     C     *LIKE         DEFINE    TRCVDT        XXCDAT
     C*** End Add *** 10/31/11 ***********************************
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
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
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C*                    OPEN WMFNL
     C*
     C*  Read 1st record
     C*
     C                   OPEN      WMCMLG
     C                   OPEN      WMCHL
     C*                    OPEN WDF2L
     C                   READ      PCM01                                  99
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C     *IN99         DOWEQ     '0'
     C*
     C*  If subset criteria met
     C*
     C*** Beg Add *** 10/31/11 ***********************************
     C                   OPEN      WMCDL
     C*** End Add *** 10/31/11 ***********************************
     C                   EXSR      SUBSET
     C*
     C     MEETSS        IFEQ      *ON
     C*                    EXCPTHEAD1
     C                   ADD       1             COUNTW
     C*** Beg Dlt *** 10/31/11 ***********************************
     C********             OPEN WMCDL
     C*** End Dlt *** 10/31/11 ***********************************
     C                   EXSR      WRITER
     C                   EXSR      BREAK2
     C                   EXSR      SRDET
     C*** Beg Dlt *** 10/31/11 ***********************************
     C********             CLOSEWMCDL
     C*** End Dlt *** 10/31/11 ***********************************
     C                   ENDIF                                                  MEETSS
     C*** Beg Add *** 10/31/11 ***********************************
     C                   CLOSE     WMCDL
     C*** End Add *** 10/31/11 ***********************************
     C*
     C*  Read next record
     C*
     C                   READ      PCM01                                  99
     C                   ENDDO                                                  WRK RECORD
     C                   CLOSE     WMCMLG
     C                   CLOSE     WMCHL
     C*
     C*                    CLOSEWMFNL
     C*  Last Record processing
     C*
     C                   EXSR      BREAK1
     C                   EXCEPT    TOTAL
     C*
     C                   EXSR      CRTDOC
     C*
     C* Update AMDATA with Today's date
     C                   MOVEL     DSDATE        RUNDAT
     C                   MOVEL     RUNDMM        AMDAMM
     C                   MOVEL     '/'           AMDAS1
     C                   MOVEL     RUNDDD        AMDADD
     C                   MOVEL     '/'           AMDAS2
     C                   MOVEL     RUNDCY        AMDACY
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
     C*** BEG DEL *** 06/17/11 ****************************************
     C********
     C********   DSTIME    IFLE 120000
     C********             CALL 'SRPAST8'
     C********   DSDATE    PARM DSDATE    ##DATE  80
     C********             PARM 1         ##DAYS  50
     C********             PARM *BLANKS   ##ERR   1
     C********             ENDIF
     C*** END DEL *** 06/17/11 ****************************************
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT7            7
     C                   MOVE      XXDAT7        XXDAT6            6
     C*
     C     'AE'          CAT(P)    XXDAT6:0      @@DCID
     C                   CAT       '.TXT':0      @@DCID
     C*
     C                   ELSE
     C                   CLEAR                   @@DCID
     C                   ENDIF
     C*
     C                   ENDSR
     C*****************************************************************
     C* PGMSTR - Set-up and Initialize
     C*****************************************************************
     CSR   PGMSTR        BEGSR
     C*
     C                   Z-ADD     *ZEROS        COUNTW            9 0
     C                   TIME                    DSDTTM
     C                   MOVE      'N'           HIT               1
     C*
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      *ALL' '       LINE            122
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       XXCLT#            3
     C*
     C* Get Last run date
     C*
     C     AMDATE        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        RUNDT7            7 0
     C*
     C                   ELSE
     C                   MOVEL     AMDAMM        RUNDMM
     C                   MOVEL     AMDADD        RUNDDD
     C                   MOVEL     AMDACY        RUNDCY
     C*
     C                   Z-ADD     RUNDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RUNDT7
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  PGMSTR
     C*****************************************************************
     C* SRDET - Write Detail Record
     C*****************************************************************
     CSR   SRDET         BEGSR
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCVDT            8 0
     C*
     C     CETMDT        IFNE      *ZEROS
     C     CHOCST        ANDEQ     'C'
     C                   Z-ADD     CHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTMDT            8 0
     C                   ELSE
     C*** Beg Add *** 04/12/11 ***********************************
     C     XXDNY         IFEQ      *ON
     C                   Z-ADD     CMDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTMDT
     C                   ELSE
     C*** End Add *** 04/12/11 ***********************************
     C                   MOVE      *ZEROS        XXTMDT
     C*** Beg Add *** 04/12/11 ***********************************
     C                   ENDIF
     C*** End Add *** 04/12/11 ***********************************
     C                   ENDIF
     C*
     C*  Detail record print
     C*
     C                   EXCEPT    DETAIL
     C*
     C                   ENDSR                                                  SRDET
     C*****************************************************************
     C* SRRTE - Print routing page
     C*****************************************************************
     CSR   SRRTE         BEGSR
     C*
     C*** GET COPYRIGHT INFORMATION
     C*
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*
     C*   GET COMPANY, USER, INFO.
     C*
     C     COKY          CHAIN     PCO01                              60
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
     C                   MOVE      *ON           MEETSS            1
     C*** Beg Add *** 04/12/11 ***********************************
     C                   MOVE      *OFF          XXDNY             1
     C*** End Add *** 04/12/11 ***********************************
     C*** Beg Add *** 10/31/11 ***********************************
     C                   MOVE      *OFF          XXDTH             1
     C*** End Add *** 10/31/11 ***********************************
     C*
     C*  MEETSS = *ON, if to be processed
     C*
     C* Exclude Denied Claims
     C*
     C     CMDNY         IFEQ      'Y'
     C*** Beg Add *** 04/12/11 ***********************************
     C     RUNDT7        ANDEQ     *ZEROS
     C*** End Add *** 04/12/11 ***********************************
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C* Exclude Claims with Lost time status <> 'I' or 'M'
     C*
     C     CMLTFL        IFNE      'I'
     C     CMLTFL        ANDNE     'M'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C* Exclude Death Claims
     C*
     C*** Beg Dlt *** 10/31/11 ***********************************
     C*****      CMDTH     IFEQ 'Y'
     C*****                MOVE *OFF      MEETSS
     C*****                ENDIF
     C*** End Dlt *** 10/31/11 ***********************************
     C*** Beg Add *** 10/31/11 ***********************************
     C     CMDTH         IFEQ      'Y'
     C     RUNDT7        ANDEQ     *ZEROS
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C* Exclude Claims if nothing changed on the claim.
     C* i.e Exclude Claims with maintainance date < AMDATA date
     C     CMDT          IFLE      RUNDT7
     C                   MOVE      *OFF          MEETSS
     C                   ELSE
     C     CMDTH         IFEQ      'Y'
     C     MEETSS        ANDEQ     *ON
     C                   MOVE      *ON           XXDTH
     C                   ENDIF
     C                   ENDIF
     C*** End Add *** 10/31/11 ***********************************
     C*
     C* Exclude Claims if nothing changed on the claim.
     C* i.e Exclude Claims with maintainance date < AMDATA date
     C*
     C*** Beg Chg *** 04/12/11 ***********************************
     C******     CMDT      IFLT RUNDT7
     C     CMDT          IFLE      RUNDT7
     C*** End Chg *** 04/12/11 ***********************************
     C                   MOVE      *OFF          MEETSS
     C*** Beg Add *** 04/12/11 ***********************************
     C                   ELSE
     C     CMDNY         IFEQ      'Y'
     C     MEETSS        ANDEQ     *ON
     C                   MOVE      *ON           XXDNY
     C                   ENDIF
     C*** End Add *** 04/12/11 ***********************************
     C                   ENDIF
     C*
     C* Include all Claims with status = O on first run
     C*
     C     CMSTAT        IFEQ      'C'
     C     RUNDT7        ANDEQ     *ZEROS
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*** Beg Add *** 10/31/11 ***********************************
     C     MEETSS        IFEQ      *ON
     C                   EXSR      SRCOMP
     C                   ENDIF
     C*** End Add *** 10/31/11 ***********************************
     C*
     C                   ENDSR                                                  SUBSET
     C*****************************************************************
     C* WRITER - WRITE WACEP5
     C*****************************************************************
     CSR   WRITER        BEGSR
     C*
     C     CXKY          CHAIN     PCX01                              99
     C     *IN99         IFEQ      *ON
     C     *IN99         OREQ      *OFF
     C     CXFL01        ANDNE     'Y'
     C*
     C                   CLEAR                   PCE01
     C*
     C                   Z-ADD     CMFND         FND
     C                   Z-ADD     CMFYR         FYR
     C                   Z-ADD     CMCASE        CASE
     C                   MOVEL(P)  CLM#          CEMEMB
     C*
     C     CHKY2         SETGT     PCH01
     C     CHKY1         READPE    PCH01                                  99
     C     *IN99         IFEQ      *OFF
     C     CHOCST        IFEQ      'C'
     C                   Z-ADD     CHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL(P)  SRDTO         CETMDT
     C                   ELSE
     C*** Beg Chg *** 04/12/11 *********************************
     C********             MOVEL*BLANKS   CETMDT    P
     C                   MOVEL     *ZEROS        CETMDT
     C*** End Chg *** 04/12/11 *********************************
     C                   ENDIF
     C                   ENDIF
     C*** Beg Add *** 04/12/11 *********************************
     C* If denied claim (and not closed) then Termination date = Maint. Date
     C*
     C     CETMDT        IFEQ      *ZEROS
     C     XXDNY         ANDEQ     *ON
     C                   Z-ADD     CMDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL(P)  SRDTO         CETMDT
     C                   ENDIF
     C*** End Add *** 04/12/11 *********************************
     C*** Beg Add *** 10/31/11 ***********************************
     C     CETMDT        IFEQ      *ZEROS
     C     XXDTH         ANDEQ     *ON
     C*** Beg Add *** 04/10/12 ***********************************
     C     CMDOD         IFNE      *ZEROS
     C                   Z-ADD     CMDOD         SRDTI
     C                   ELSE
     C*
     C*** End Add *** 04/10/12 ***********************************
     C                   Z-ADD     CMDT          SRDTI
     C*** Beg Add *** 04/10/12 ***********************************
     C                   ENDIF
     C*** End Add *** 04/10/12 ***********************************
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL(P)  SRDTO         CETMDT
     C                   ENDIF
     C*** End Add *** 10/31/11 ***********************************
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         CECVDT
     C*
     C     CDKY          CHAIN     PCD01                              62
     C     *IN62         IFEQ      '0'
     C                   MOVEL(P)  CDLSN         CELSN
     C                   MOVEL(P)  CDFSN         CEFSN
     C                   MOVEL(P)  CDMID         CEMID
     C                   MOVEL(P)  CDADR1        CEADR1
     C                   MOVEL(P)  CDADR2        CEADR2
     C                   MOVEL     CDCTY         CECTY
     C                   MOVE      CDST          CEST
     C                   MOVEL     CDZIP         CEZIP5
     C                   MOVE      CDZIP         CEZIP4
     C*
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         CEDTOB
     C*
     C                   MOVE      CDSEX         CESEX
     C                   ENDIF
     C*
     C                   MOVE      'Y'           HIT
     C*
     C* Defaulting of data fields
     C*
     C                   Z-ADD     0             CEFIL1
     C                   MOVE      *BLANKS       CEFIL2
     C                   MOVE      *BLANKS       CEFIL3
     C                   MOVE      '01'          CEPERS
     C                   MOVE      '1'           CEREL
     C                   MOVE      '1'           CEEOVR
     C                   MOVE      *BLANKS       CELOC
     C                   MOVE      *BLANKS       CEFLX2
     C                   MOVE      *BLANKS       CEFLX3
     C                   MOVE      *BLANKS       CEHCTR
     C                   MOVE      '01'          CEESTS
     C                   MOVE      '03'          CECT
     C                   MOVE      *BLANKS       CEALTM
     C                   MOVE      'X'           CERCTM
     C*** BEG CHG *** 05/09/11 ****************************************
     C******************** MOVE *BLANKS   CEPLID
     C                   MOVE      '40014010'    CEPLID
     C*** END CHG *** 05/09/11 ****************************************
     C*
     C                   WRITE     PCE01
     C                   ENDIF
     C*
     C                   ENDSR                                                  WRITER
     F*** Beg Add *** 10/31/11 ***********************************
     C*****************************************************************
     C* SRCOMP - Compare the key fields
     C*****************************************************************
     C     SRCOMP        BEGSR
     C*
     C*** BEGIN ADD *** 02/16/12 ********************************
     C                   MOVE      'N'           XXSTAT
     C*** END   ADD *** 02/16/12 ********************************
     C                   Z-ADD     CMFND         FND
     C                   Z-ADD     CMFYR         FYR
     C                   Z-ADD     CMCASE        CASE
     C                   MOVEL(P)  CLM#          KKMEMB
     C*
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      *OFF
     C     TRKY          CHAIN     PTR01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     TRZIP5        XXZIP             9
     C                   MOVE      TRZIP4        XXZIP
     C                   MOVEL     XXZIP         XXZIPF
     C*
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         XXDTOB
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         XXCDAT
     C*
     C     CDLSN         IFEQ      TRLSN
     C     CDFSN         ANDEQ     TRFSN
     C     CDMID         ANDEQ     TRMID
     C     XXDTOB        ANDEQ     TRDTOB
     C     CDSEX         ANDEQ     TRSEX
     C     CDADR1        ANDEQ     TRADR1
     C     CDADR2        ANDEQ     TRADR2
     C     CDCTY         ANDEQ     TRCTY
     C     CDST          ANDEQ     TRST
     C     CDZIP         ANDEQ     XXZIPF
     C     XXCDAT        ANDEQ     TRCVDT
     C     CMLTFL        ANDEQ     TRLTFL
     C     CMDNY         ANDEQ     TRDNY
     C     CMDTH         ANDEQ     TRDTH
     C     CMSTAT        ANDEQ     TRSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ELSE
     C*** BEGIN ADD *** 02/16/12 ********************************
     C     TRSTAT        IFEQ      'O'
     C*** END   ADD *** 02/16/12 ********************************
     C* Update WATRL5
     C                   EXSR      SRMOVE
     C                   UPDATE    PTR01
     C                   MOVE      'Y'           XXSTAT            1
     C*** BEGIN ADD *** 02/16/12 ********************************
     C                   ENDIF
     C*** END   ADD *** 02/16/12 ********************************
     C                   ENDIF
     C                   ELSE
     C* Write a new claim into WATRL5
     C                   EXSR      SRMOVE
     C*** BEGIN ADD *** 11/22/11 ********************************
     C* IF Denied claim exist in Log file then update
     C     CMDNY         IFEQ      'Y'
     C     TRKY          CHAIN     PTR01                              60
     C     *IN60         IFEQ      *OFF
     C                   EXSR      SRMOVE
     C                   MOVE      'Y'           XXSTAT
     C                   UPDATE    PTR01
     C                   ENDIF
     C                   ENDIF
     C* IF Death claim exist in Log file then update
     C     CMDTH         IFEQ      'Y'
     C     TRKY          CHAIN     PTR01                              60
     C     *IN60         IFEQ      *OFF
     C                   EXSR      SRMOVE
     C                   MOVE      'Y'           XXSTAT
     C                   UPDATE    PTR01
     C                   ENDIF
     C                   ENDIF
     C*
     C     CMDTH         IFNE      'Y'
     C     CMDNY         ANDNE     'Y'
     C*** END   ADD *** 11/22/11 ********************************
     C*** BEGIN ADD *** 02/16/12 ********************************
     C     CMSTAT        ANDNE     'C'
     C                   MOVE      'Y'           XXSTAT
     C*** END   ADD *** 02/16/12 ********************************
     C                   WRITE     PTR01
     C*** BEGIN ADD *** 11/22/11 ********************************
     C                   ENDIF
     C*** END   ADD *** 11/22/11 ********************************
     C                   ENDIF
     C                   ENDIF
     C*** BEGIN ADD *** 02/16/12 ********************************
     C     XXSTAT        IFEQ      'Y'
     C                   MOVE      *ON           MEETSS
     C                   ELSE
     C*
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*** END   ADD *** 02/16/12 ********************************
     C*
     C                   ENDSR
     C*****************************************************************
     C* SRMOVE - Update the transaction history file fields
     C*****************************************************************
     C     SRMOVE        BEGSR
     C*
     C                   MOVEL(P)  CDLSN         TRLSN
     C                   MOVEL(P)  CDFSN         TRFSN
     C                   MOVEL(P)  CDMID         TRMID
     C*
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         TRDTOB
     C*
     C                   MOVE      CDSEX         TRSEX
     C                   MOVEL(P)  CDADR1        TRADR1
     C                   MOVEL(P)  CDADR2        TRADR2
     C                   MOVEL     CDCTY         TRCTY
     C                   MOVE      CDST          TRST
     C                   MOVEL     CDZIP         TRZIP5
     C                   MOVE      CDZIP         TRZIP4
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         TRCVDT
     C*
     C                   MOVEL     CMLTFL        TRLTFL
     C                   MOVEL     CMDTH         TRDTH
     C                   MOVEL     CMDNY         TRDNY
     C                   MOVEL     CMSTAT        TRSTAT
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVEL(P)  CLM#          TRMEMB
     C                   Z-ADD     0             TRFIL1
     C                   MOVE      *BLANKS       TRFIL2
     C                   MOVE      *BLANKS       TRFIL3
     C                   MOVE      '01'          TRPERS
     C                   MOVE      '1'           TRREL
     C                   MOVE      '1'           TREOVR
     C                   MOVE      *BLANKS       TRLOC
     C                   MOVE      *BLANKS       TRFLX2
     C                   MOVE      *BLANKS       TRFLX3
     C                   MOVE      *BLANKS       TRHCTR
     C                   MOVE      '01'          TRESTS
     C                   MOVE      '03'          TRCT
     C                   MOVE      *BLANKS       TRALTM
     C                   MOVE      'X'           TRRCTM
     C                   MOVE      '40014010'    TRPLID
     C*** BEG ADD *** 11/22/11 ***********************************
     C                   ENDIF
     C*** END ADD *** 11/22/11 ***********************************
     C*
     C     CHKY2         SETGT     PCH01
     C     CHKY1         READPE    PCH01                                  99
     C     *IN99         IFEQ      *OFF
     C     CHOCST        IFEQ      'C'
     C                   Z-ADD     CHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL(P)  SRDTO         TRTMDT
     C                   ELSE
     C                   MOVEL     *ZEROS        TRTMDT
     C                   ENDIF
     C                   ENDIF
     C* If death claim (and not closed) then Termination date = Maint. Date
     C*
     C     TRTMDT        IFEQ      *ZEROS
     C*** BEG CHG *** 11/22/11 *********************************
     C********** XXDNY     ANDEQ*ON
     C     CMDTH         ANDEQ     'Y'
     C*** END CHG *** 11/22/11 *********************************
     C                   Z-ADD     CMDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL(P)  SRDTO         TRTMDT
     C                   ENDIF
     C*
     C*** BEG ADD *** 11/22/11 ***********************************
     C* If denied claim (and not closed) then Termination date = Maint. Date
     C*
     C     TRTMDT        IFEQ      *ZEROS
     C     CMDNY         ANDEQ     'Y'
     C                   Z-ADD     CMDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL(P)  SRDTO         TRTMDT
     C                   ENDIF
     C*** END ADD *** 11/22/11 ***********************************
     C*** BEG DLT *** 11/22/11 ***********************************
     C***                  ENDIF
     C*** END DLT *** 11/22/11 ***********************************
     C*
     C                   ENDSR
     C*** End Add *** 10/31/11 ***********************************
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
     O          E            ROUTE          1 60
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
     O*
     O          E            HEAD           1 03
     O                       DSTIME               8 '  :  :  '
     O                       CONAME              76
     O                       DSDATE        Y    127
     O*
     O          E            HEAD           0
     O                       LINE              +001
     O*
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              90
     O                                          120 'PAGE:'
     O                       PAGE          Z    127
     O*
     O          E            HEAD1          2
     O                                            3 'Grp'
     O                                            9 'Year'
     O                                           18 'Case'
     O                                           29 'Last Name'
     O                                           49 'First Name'
     O                                           68 'Middle Initial'
     O                                           85 'Eff. Date'
     O                                          106 'Termination Date'
     O*
     O          E            HEAD1          0
     O                       LINE              +001
     O*
     O          E            DETAIL         1
     O                       CMFND         4      3
     O                       CMFYR         4   +003
     O                       CMCASE            +002
     O                       CELSN             +002
     O                       CEFSN             +003
     O                       CEMID             +009
     O                       XXCVDT            +014 '  /  /    '
     O                       XXTMDT            +011 '  /  /    '
     O*
     O          E            TOTAL          1
     O                                           20 'Number of records ex'
     O                                           32 'tracted    :'
     O                       COUNTW            +002 '       0 '
     O*
