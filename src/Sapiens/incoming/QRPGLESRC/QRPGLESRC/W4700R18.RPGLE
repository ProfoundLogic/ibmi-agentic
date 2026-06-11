     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W4700R18 - AUDIT STAUS REPORT
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*       THIS PROGRAM WILL PRINT A AUDIT TRACKING CONTROL DETAIL *
     H*    REPORT. THE REPORT WILL SHOW THE NUMBER OF MEMBERS, TOTALS *
     H*    FROM THE AUDIT TRACKING MASTER FILE, AND PREMIUM AMOUNTS.  *
     H*                                                               *
     H*  CALLED FROM: W4700C18                                        *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*                                                               *
     H*                                                               *
     H*  CREATED: 03/04/98  PROGRAMMER: C:87  PROJECT: WC630          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/05/14  C:283  Support: WA045 86194                         *
     H*                  Account with Audit Received should not be    *
     H*                  counted as Non-Productive Reporting.         *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 05/22/06  C:65  PROJECT: WC1412                               *
     H*                 POPULATE NEW FYR, EFF, AND EXP DATE FIELDS.   *
     H*                                                               *
     H*  04/10/00  C:118 PROJECT: WC987                               *
     H*                  LOAD ALTERNATE COMPANY NAME FROM FUND MASTER *
     H*                  IF AVAILABLE.                                *
     H*                                                               *
     H*  04/16/99  C:118 PROJECT: WC630                               *
     H*                  Modified reports to change 'Fund' to 'Group' *
     H*                  and 'Members' to 'Policies'. Modified routing*
     H*                  page and report headings to center.          *
     H*
     H*  09/24/98  C:87  PROJ: Support WA091 #13782                   *
     H*                  Added code to load key fields for chain to   *
     H*                  employer detail (WDELP).                     *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H*     OA -  OVERFLOW FOR PRINTER                                *
     H*     08 -  DEMAND MODE                                         *
     H*     09 -  BATCH  MODE                                         *
     H*     50 -  MAIN LOOP - DO WHILE                                *
     H*     60 -  GENERAL CHAIN INDICATOR                             *
     H*     98 -  LOKUP OPERATIONS                                    *
     H*                                                               *
     H*****************************************************************
     F*
     FWMA2L     UF A E           K DISK
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMAUL4    IF   E           K DISK
     FWMAVL     UF A E           K DISK
     FQSYSPRT   O    F   80        PRINTER OFLIND(*INOA)

     D*
     D*
     D TITLE           C                   CONST('AUDIT STATUS DETA-
     D                                     IL REPORT')
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  EF                     1     35  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C****************************************************************
     C*  KEY LISTINGS
     C****************************************************************
     C*
     C*  Company Master key list
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  Fund Master key list
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  Employer Master key list
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  Employer Detail key list
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  Audit Tracking Control key list
     C     AUKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFRDT
     C*
     C     AUKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  Audit detail file key list
     C*
     C     A2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  Audit status detail listing
     C*
     C     AVKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  User parameter key list
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  Key list for the Overide file
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C****************************************************************
     C*  *LIKE DEFINES
     C****************************************************************
     C*
     C     *LIKE         DEFINE    AUCO#         XXCO#
     C     *LIKE         DEFINE    AUFND         XXFND
     C     *LIKE         DEFINE    AUFYR         XXFYR
     C     *LIKE         DEFINE    AUEMP#        XXEMP#
     C     *LIKE         DEFINE    AUDIV         XXDIV
     C     *LIKE         DEFINE    AUPTDT        XXFRDT
     C     *LIKE         DEFINE    AUPTDT        XXTODT
     C     *LIKE         DEFINE    AUPTDT        XXRNDT
     C*
     C*  Percentages
     C     *LIKE         DEFINE    ELASP1        XXRPCT
     C     *LIKE         DEFINE    ELASP1        XXCPCT
     C     *LIKE         DEFINE    ELASP1        XXOPCT
     C*
     C*  Counter fields
     C     *LIKE         DEFINE    AUATTM        XXACNT
     C     *LIKE         DEFINE    AUATTM        XXCCNT
     C     *LIKE         DEFINE    AUATTM        XXTCNT
     C     *LIKE         DEFINE    AUATTM        XXRQST
     C     *LIKE         DEFINE    AUATTM        XXRCVD
     C     *LIKE         DEFINE    AUATTM        XXOUTS
     C     *LIKE         DEFINE    AUATTM        XXCOMP
     C     *LIKE         DEFINE    AUATTM        XXATTM
     C     *LIKE         DEFINE    AUATTM        XXRVSD
     C     *LIKE         DEFINE    AUATTM        XXCNTD
     C     *LIKE         DEFINE    AUATTM        XXINHS
     C     *LIKE         DEFINE    AUATTM        XX#AUD
     C*
     C*  Premiums work fields
     C     *LIKE         DEFINE    ELANPR        XXPREM
     C     *LIKE         DEFINE    ELANPR        XXEXPR
     C     *LIKE         DEFINE    ELANPR        XXPRM1
     C     *LIKE         DEFINE    ELANPR        XXPRM2
     C     *LIKE         DEFINE    ELANPR        XXAPRM
     C     *LIKE         DEFINE    ELANPR        X1PRM1
     C     *LIKE         DEFINE    ELANPR        X1PRM2
     C     *LIKE         DEFINE    ELANPR        X1APRM
     C*
     C****************************************************************
     C*  *ENTRY PARAMETERS
     C****************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQRNDT            7
     C                   PARM                    QQFRDT            7
     C                   PARM                    QQTODT            7
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
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
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFRDT        XXFRDT
     C                   MOVE      QQTODT        XXTODT
     C                   MOVE      QQRNDT        XXRNDT
     C*
     C                   Z-ADD     XXRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDATE            8 0
     C*
     C                   Z-ADD     XXFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XSFRDT            8 0
     C*
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XSTODT            8 0
     C*
     C****************************************************************
     C*   SET UP AND INITIALIZE                                 "
     C****************************************************************
     C*
     C                   TIME                    DSDTTM
     C*
     C*  Counters
     C                   Z-ADD     0             XXACNT                         ACT. MEMBER
     C                   Z-ADD     0             XXCCNT                         CANX  MEMBER
     C                   Z-ADD     0             XXTCNT                         TOTL  MEMBER
     C                   Z-ADD     0             XXRQST                         REQUESTED
     C                   Z-ADD     0             XXRCVD                         RECEIVED
     C                   Z-ADD     0             XXOUTS                         OUTSTANDINGS
     C                   Z-ADD     0             XXCOMP                         COMPLETED
     C                   Z-ADD     0             XXATTM                         ATTEMPTED
     C                   Z-ADD     0             XXRVSD                         REVISED
     C                   Z-ADD     0             XXCNTD                         CONTESTED
     C                   Z-ADD     0             XXINHS                         IN-HOUSE
     C                   Z-ADD     0             XX#AUD                         # AUDITED ACCTS
     C*
     C*  Percentages
     C                   Z-ADD     0             XXRPCT                         RECEIVED
     C                   Z-ADD     0             XXCPCT                         COMPLETED
     C                   Z-ADD     0             XXOPCT                         OUTSTANDING
     C*
     C*  Premiums
     C                   Z-ADD     0             XXPREM                         WITH EXP CON
     C                   Z-ADD     0             XXEXPR                         W/O EXP CON
     C                   Z-ADD     0             XXPRM1                         TTL  W/O E.C.
     C                   Z-ADD     0             XXPRM2                         TTL  WITH E.C.
     C                   Z-ADD     0             XXAPRM                         AUDITED PREM.
     C*
     C                   Z-ADD     0             X1PRM1                         TTL W/O E.C.
     C                   Z-ADD     0             X1PRM2                         TTL WITH E.C.
     C                   Z-ADD     0             X1APRM                         AUDITED PREM.
     C*
     C     QQMODE        IFNE      ' '                                               "
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE                                                        "
     C                   SETON                                        08        BATCH MODE
     C                   END                                                         "
     C*
     C*  Delete records from WMAVP keyed on co#/fnd so that we can
     C*  write new records from this report
     C*
     C     AVKY          CHAIN     PAV01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PAV01
     C     AVKY          READE     PAV01                                  60
     C                   ENDDO
     C*
     C*  Chain to company file
     C*
     C     COKY          CHAIN     PCO01                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*  Chain to fund master file
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C*
     C*  LOAD ALTERNATE COMPANY NAME FROM FUND MASTER IF AVAILABLE
     C*
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C*
     C***** END   ADD **** 04/10/00 ***********************************
     C*
     C*  Chain to the user parameter file
     C*
     C     UPKY          CHAIN     PUP01                              60
     C*
     C*  MAIN LOOP - Read Audit Tracking file and process records
     C*              within the expiration date range passed
     C*
     C     AUKY          SETLL     PAU01
     C     AUKY1         READE     PAU01                                  50
     C     *IN50         DOWEQ     *OFF
     C     AUPTDT        ANDLE     XXTODT
     C*
     C*   Chain to Employer Detail
     C*
     C***  BEGIN ADD  ***  09/24/98  **********************************
     C                   Z-ADD     AUEMP#        XXEMP#
     C                   Z-ADD     AUDIV         XXDIV
     C                   Z-ADD     AUFYR         XXFYR
     C***  END   ADD  ***  09/24/98  **********************************
     C     ELKY          CHAIN     PEL01                              51
     C*
     C*  Chain to Employer Master to determine Active vs Cancelled
     C*
     C                   Z-ADD     AUEMP#        XXEMP#
     C                   Z-ADD     AUDIV         XXDIV
     C     EMKY          CHAIN     PEM01                              60
     C*
     C*  Find policy effective date
     C                   Z-ADD     1             XI                1 0
     C     *ZERO         LOOKUP    EF(XI)                                 70
     C*
     C     EF(XI)        IFNE      *ZERO
     C                   Z-ADD     5             XI
     C                   ELSE
     C                   SUB       1             XI
     C                   ENDIF
     C*
     C     XI            DOWGT     *ZERO
     C     EF(XI)        IFGT      ELPFDT
     C                   SUB       1             XI
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C*  Determine if cancellation date exists
     C     XI            IFEQ      *ZERO
     C                   ADD       1             XXACNT
     C                   ELSE
     C     CX(XI)        IFEQ      *ZEROS
     C     CX(XI)        ORGT      XXRNDT
     C                   ADD       1             XXACNT
     C                   ELSE
     C                   ADD       1             XXCCNT
     C                   ENDIF                                                  CX,XI=*ZEROS
     C                   ENDIF                                                  XI IFEQ *ZERO
     C*
     C*  Accumulate Audited premiums if Final Audit Flag = 'A',
     C*  else accumulate Estimated Annual premiums
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELAAPR        XXPREM
     C                   ADD       ELAAPR        XXAPRM
     C                   ADD       1             XX#AUD
     C                   ELSE
     C                   Z-ADD     ELANPR        XXPREM
     C                   END
     C*
     C*  Subtract expense constant
     C     XXPREM        SUB       ELPRTD        XXEXPR
     C*
     C*  Accumulate premium without expense constant = XXPRM1
     C*  Accumulate premium with expense constant = XXPRM2
     C                   ADD       XXEXPR        XXPRM1
     C                   ADD       XXPREM        XXPRM2
     C*
     C* # of Audit Requests
     C     AURQDT        IFNE      *ZERO
     C                   ADD       1             XXRQST
     C                   ENDIF
     C*
     C* # of Audits Received and # Outstanding
     C     AURCVD        IFGE      AURQDT
     C                   ADD       1             XXRCVD
     C                   MOVE      '1'           XXOPT             1 0
     C                   EXSR      OPTION
     C                   ELSE
     C                   ADD       1             XXOUTS
     C                   MOVE      '3'           XXOPT
     C                   EXSR      OPTION
     C                   ENDIF
     C*
     C* # of Completed Audits
     C     AUPRDT        IFGE      AURQDT
     C                   ADD       1             XXCOMP
     C                   MOVE      '2'           XXOPT
     C                   EXSR      OPTION
     C                   ENDIF
     C*
     C*  Calculate audits in house to process
     C     AURCVD        IFGE      AURQDT
     C     AUPRDT        ANDLT     AURQDT
     C                   ADD       1             XXINHS
     C                   MOVE      '4'           XXOPT
     C                   EXSR      OPTION
     C                   ENDIF
     C*
     C* # of Non-Productive Accounts
     C     AUATTM        IFNE      *ZERO
     C*** BEG ADD ***** 01/05/14 **************************************
     C     AURCVD        ANDEQ     *ZERO
     C*** END ADD ***** 01/05/14 **************************************
     C                   ADD       1             XXATTM
     C                   MOVE      '5'           XXOPT
     C                   EXSR      OPTION
     C                   ENDIF
     C*
     C* # of Re-Audits
     C     AURVFL        IFEQ      'Y'
     C                   ADD       1             XXRVSD
     C                   MOVE      '6'           XXOPT
     C                   EXSR      OPTION
     C                   ENDIF
     C*
     C* # of Contested Audits
     C     AUCNFL        IFEQ      'Y'
     C                   ADD       1             XXCNTD
     C                   MOVE      '7'           XXOPT
     C                   EXSR      OPTION
     C                   ENDIF
     C*
     C     AUKY1         READE     PAU01                                  50
     C                   ENDDO                                                  *IN50 = *OFF
     C*
     C*  Calculate percentages for report.
     C     XXRQST        IFNE      *ZERO
     C     XXRCVD        DIV       XXRQST        XXRPCT
     C                   MULT      100           XXRPCT
     C                   ENDIF
     C*
     C     XXRQST        IFNE      *ZERO
     C     XXCOMP        DIV       XXRQST        XXCPCT
     C                   MULT      100           XXCPCT
     C                   ENDIF
     C*
     C     XXRQST        IFNE      *ZERO
     C     XXOUTS        DIV       XXRQST        XXOPCT
     C                   MULT      100           XXOPCT
     C                   ENDIF
     C*
     C                   Z-ADD     XXPRM1        X1PRM1
     C                   Z-ADD     XXPRM2        X1PRM2
     C                   Z-ADD     XXAPRM        X1APRM
     C     XXACNT        ADD       XXCCNT        XXTCNT
     C*
     C                   EXSR      LODFIL
     C*
     C                   EXSR      PRTDTL
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain ORKY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*
     C*================================================================
     C* LODFIL - Load variables for output to screen
     C*================================================================
     C     LODFIL        BEGSR
     C*
     C     A2KY          CHAIN     PA201                              60
     C*
     C                   Z-ADD     AUCO#         A2CO#
     C                   Z-ADD     AUFND         A2FND
     C                   MOVEL     FNNAME        A2FNNM
     C*
     C                   Z-ADD     XXRNDT        A2RNDT
     C                   Z-ADD     XXFRDT        A2FRDT
     C                   Z-ADD     XXTODT        A2TODT
     C*
     C                   Z-ADD     XXTCNT        A2TOTM
     C                   Z-ADD     XXACNT        A2TOTA
     C                   Z-ADD     XXCCNT        A2TOTC
     C*
     C                   Z-ADD     XXRQST        A2RQS#
     C                   Z-ADD     X1PRM1        A2PRM1
     C                   Z-ADD     X1PRM2        A2PRM2
     C                   Z-ADD     X1APRM        A2APRM
     C                   Z-ADD     XX#AUD        A2#AUD
     C*
     C                   Z-ADD     XXRCVD        A2RCVD
     C                   Z-ADD     XXRPCT        A2RPCT
     C                   Z-ADD     XXCOMP        A2COMP
     C                   Z-ADD     XXCPCT        A2CPCT
     C                   Z-ADD     XXOUTS        A2OUTS
     C                   Z-ADD     XXOPCT        A2OPCT
     C*
     C                   Z-ADD     XXINHS        A2INHS
     C                   Z-ADD     XXATTM        A2ATTM
     C                   Z-ADD     XXRVSD        A2RVSD
     C                   Z-ADD     XXCNTD        A2CNTD
     C*
     C                   MOVEL     Q1USER        A2USER
     C                   MOVEL     Q1PGM         A2PGM
     C                   Z-ADD     DSTIME        A2TIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         A2DT
     C*
     C     *IN60         IFEQ      *OFF
     C                   UPDATE    PA201
     C                   ELSE
     C                   WRITE     PA201
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C                   ENDSR                                                  LODFIL
     C*================================================================
     C* OPTION - Write records to Audit Status Detail Listing file
     C*================================================================
     C     OPTION        BEGSR
     C*
     C                   CLEAR                   PAV01
     C*
     C                   Z-ADD     XXOPT         AVOPT
     C*
     C                   Z-ADD     AUCO#         AVCO#
     C                   Z-ADD     AUFND         AVFND
     C*
     C                   Z-ADD     AUEMP#        AVEMP#
     C                   MOVEL     EMNAME        AVNAME
     C                   Z-ADD     AUDIV         AVDIV
     C*** BEGIN ADD ***  05/22/06  ***************************************
     C                   Z-ADD     ELFYR         AVFYR
     C                   Z-ADD     ELPFDT        AVPFDT
     C                   Z-ADD     ELPTDT        AVPTDT
     C*** END ADD *****  05/22/06  ***************************************
     C*
     C                   MOVEL     AUACO         AVACO
     C                   Z-ADD     AURQDT        AVRQDT
     C                   Z-ADD     AURCVD        AVRCVD
     C                   Z-ADD     AUPRDT        AVPRDT
     C                   Z-ADD     AUMAIL        AVMAIL
     C                   Z-ADD     AUCNTD        AVCNTD
     C                   Z-ADD     AUATTM        AVATTM
     C*
     C                   MOVEL     AUSTAF        AVSTAF
     C                   MOVE      AURSGN        AVRSGN
     C                   MOVEL     AUCOM1        AVCOM1
     C                   MOVEL     AUCOM2        AVCOM2
     C                   MOVEL     AUCOM3        AVCOM3
     C                   MOVEL     AUCOM4        AVCOM4
     C*
     C                   MOVEL     Q1USER        AVUSER
     C                   MOVEL     Q1PGM         AVPGM
     C                   Z-ADD     DSTIME        AVTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AVDT
     C*
     C                   WRITE     PAV01
     C*
     C                   ENDSR                                                  OPTION
     C*================================================================
     C* PRTDTL - Print audit status detail report
     C*================================================================
     C     PRTDTL        BEGSR
     C*
     C*  Get the distribution Information
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C*
     C***** BEGIN ADD **** 04/16/99 ***********************************
     C*  Left justify title
     C*
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXTITL           60
     C     ORTITL        IFNE      *BLANKS
     C                   MOVE(P)   ORTITL        XXDATA           60
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60
     C                   SUBST(P)  XXDATA:N      XXTITL
     C                   ENDIF
     C                   ENDIF
     C***** END ADD ****** 04/16/99 ***********************************
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   ENDIF
     C*
     C***** BEGIN ADD **** 04/16/99 ***********************************
     C*   CENTER TEXT IN FIELD
     C*
     C                   MOVEL     ORTITL        ##DATA
     C                   CALL      'SRCENTER'                             99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL     ##DATA        XXTITL
     C*
     C***** END ADD ****** 04/16/99 ***********************************
     C*   Print detail report
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET
     C*
     C                   ENDSR                                                  PRTDTL
     C*================================================================
     C* Begin Output Specs
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
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O***** BEGIN CHG ******** 04/16/99 *******************************
     O                       XXTITL              80
     O**********               ORTITL    80
     O***** END CHG ********** 04/16/99 *******************************
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O          E            ROUTE          2 34
     O***** BEGIN ADD ******** 04/16/99 *******************************
     O          E            ROUTE          2
     O                                           24 'REPORT SELECTIONS:'
     O          E            ROUTE          2
     O***** END ADD ********** 04/16/99 *******************************
     O***** BEGIN CHG ******** 04/16/99 *******************************
     O**********                          8 'COMPANY:'
     O                                           18 'COMPANY:'
     O**********               XXCO# Z   26
     O                       XXCO#         Z     24
     O***** END CHG ********** 04/16/99 *******************************
     O          E            ROUTE          2
     O***** BEGIN CHG ******** 04/16/99 *******************************
     O**********                          5 'FUND'
     O                                           16 'GROUP:'
     O**********               XXFND Z   26
     O                       XXFND         Z     24
     O***** END CHG ********** 04/16/99 *******************************
     O***** BEGIN CHG ******** 04/16/99 *******************************
     O********E  170           ROUTE
     O**********                         24 '(C) PROGRESSIVE DATA'
     O**********                         45 'SOLUTIONS, INC. 1991'
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE
     O****                               23 '(C) COPYRIGHT PROGRESSI'
     O****                             +000 'VE DATA SOLUTIONS, INC.,'
     O                                         +000 '(C)'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
     O***** END CHG ********** 04/16/99 *******************************
     O*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              60
     O                       DSDATE        Y     80
     O          E            HEAD           1
     O                       Q1PGM               10
     O***** BEGIN CHG ******** 04/16/99 *******************************
     O**********               TITLE     56
     O                       XXTITL              70
     O***** END CHG ********** 04/16/99 *******************************
     O                                           75 'PAGE:'
     O                       PAGE          Z     80
     O          E            HEAD           1
     O***** BEGIN CHG ******** 04/16/99 *******************************
     O**********                         37 'AS OF:'
     O**********               XXDATE    46 '  /  /    '
     O                                           36 'AS OF:'
     O                       XXDATE              48 '  /  /    '
     O***** END CHG ********** 04/16/99 *******************************
     O          E            HEAD           2
     O***** BEGIN CHG ******** 04/16/99 *******************************
     O**********                         23 'POLICY EXPIRATION DATE'
     O**********                         30 'RANGE:'
     O                                           22 'POLICY EXPIRATION DATE'
     O                                           29 'RANGE:'
     O***** END CHG ********** 04/16/99 *******************************
     O                       XSFRDT              41 '  /  /    '
     O***** BEGIN CHG ******** 04/16/99 *******************************
     O**********               XSTODT    53 '  /  /    '
     O                       XSTODT              54 '  /  /    '
     O***** END CHG ********** 04/16/99 *******************************
     O          E            HEAD           1
     O***** BEGIN CHG ******** 04/16/99 *******************************
     O**********                          5 'FUND:'
     O                                            6 'GROUP:'
     O**********               XXFND Z   09
     O                       XXFND         Z     10
     O**********               FNNAME    70
     O                       FNNAME              71
     O***** END CHG ********** 04/16/99 *******************************
     O*
     O          E            DET         1  1
     O                                           25 'TOTAL NUMBER OF'
     O***** BEGIN CHG ******** 04/16/99 *******************************
     O**********                         35 'MEMBERS:'
     O                                           35 'POLICIES:'
     O***** END CHG ********** 04/16/99 *******************************
     O                       XXTCNT        J     70
     O          E            DET            2
     O                                           21 'ACTIVE:'
     O                       XXACNT        J     32
     O                                           44 'CANCELLED:'
     O                       XXCCNT        J     55
     O          E            DET         1  2
     O                                           27 'AUDITS REQUESTED:'
     O                       XXRQST        J     70
     O          E            DET         1  2
     O                                           25 'AUDITS RECEIVED'
     O                                           41 'AND PERCENTAGE:'
     O                       XXRPCT        J     56
     O                                           58 '%'
     O                       XXRCVD        J     70
     O          E            DET         1  2
     O                                           26 'AUDITS COMPLETED'
     O                                           42 'AND PERCENTAGE:'
     O                       XXCPCT        J     56
     O                                           58 '%'
     O                       XXCOMP        J     70
     O          E            DET         1  2
     O                                           28 'AUDITS OUTSTANDING'
     O                                           44 'AND PERCENTAGE:'
     O                       XXOPCT        J     56
     O                                           58 '%'
     O                       XXOUTS        J     70
     O          E            DET         1  2
     O                                           28 'AUDITS IN HOUSE TO'
     O                                           37 'PROCESS:'
     O                       XXINHS        J     70
     O          E            DET         1  0
     O                                           27 'PROJECTED PREMIUM'
     O                                           44 'FROM LOSS REPORT'
     O          E            DET         1  2
     O                                           45 'AS OF __/__/__'
     O                                           70 '______________'
     O          E            DET         1  0
     O                                           31 'NON-PRODUCTIVE STATUS'
     O                                           43 'REPORT FROM'
     O          E            DET         1  2
     O                                           44 'AUDIT COMPANIES:'
     O                       XXATTM        J     70
     O          E            DET         1  2
     O                                           20 'RE-AUDITS:'
     O                       XXRVSD        J     70
     O          E            DET         1  2
     O                                           27 'CONTESTED AUDITS:'
     O                       XXCNTD        J     70
     O          E            DET         1  0
     O                                           26 'AUDITED PREMIUM:'
     O                       X1APRM        J     70
     O          E            DET         1  2
     O                                           31 'NUMBER OF AUDITED'
     O                                           41 'ACCOUNTS:'
     O                       XX#AUD        J     70
     O          E            DET         1  0
     O                                           27 'AUDITED/ESTIMATED'
     O                                           35 'PREMIUM'
     O          E            DET         1  1
     O                                           33 'WITHOUT EXPENSE'
     O                                           43 'CONSTANT:'
     O                       X1PRM1        J     70
     O          E            DET         1  1
     O                                           33 'WITH    EXPENSE'
     O                                           43 'CONSTANT:'
     O                       X1PRM2        J     70
