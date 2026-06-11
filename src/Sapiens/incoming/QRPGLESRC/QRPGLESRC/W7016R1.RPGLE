     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7016R1 - GROUP CHECK REGISTER BY PROVIDER
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H* DESCRIPTION:  THIS PROGRAM PRODUCES A TWO REPORTS, A DETAIL   *
     H*               REPORT AND A SUMMARY REPORT.  THE DETAIL REPORT *
     H*               LISTS ALL PAYMENTS FOR EACH GROUP CHECK AND     *
     H*               PAGE BREAKS AT EACH PROVIDER.  THE SUMMARY      *
     H*               REPORT PRINTS ONE LINE FOR EACH GROUP CHECK AND *
     H*               PRINTS TOTALS FOR EACH FUND.                    *
     H*                                                               *
     H* CALLED FROM:  W7016C1                                         *
     H* CALLS TO:     NONE                                            *
     H*                                                               *
     H*  CREATED: 01/27/95     PROGRAMMER: C:15       PROJECT: WC361  *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  07/19/21 C:079 Project: WC2749                               *
     H*                 Add EPay processing                           *
     H*                                                               *
     H*  06/09/20 C:318 Project: SY420                                *
     H*                 Mask SSN                                      *
     H*                                                               *
     H* 10/09/19 C:291 Project: 102WHX                                *
     H*                Add free form Remark code on the report in     *
     H*                place of EOB# and ID# for client 102.          *
     H*                                                               *
     H* 12/27/17 C:062 Support: 63294(146)                            *
     H*                Clear XXSINV                                   *
     H*                                                               *
     H* 08/07/15 C:136 Support: 36211 (114MRH)                        *
     H*                Add EFT processing                             *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H*  06/27/14 C:280  SUPPORT: 85502                               *
     H*                  Changing ending pos in print for             *
     H*                  xxsinv which was increase but print was      *
     H*                  not modified                                 *
     H*                                                               *
     H*  07/19/12 C:282  SUPPORT: WA133 80333                         *
     H*                  Changing the size of variable XXSINV form    *
     H*                  10 to 16.                                    *
     H*                                                               *
     H*  02/10/10 C:246  PROJECT: C133WIJ                             *
     H*                  ADD JURISDICTION STATE IN QSYSPRT            *
     H*                                                               *
     H*  01/28/10 C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 12/30/08 C:220 SUPPORT: 66430                                 *
     H*                Condition call for WA068R                      *
     H*                                                               *
     H* 07/01/08 C:190 Project: WC1638 Priss#6                        *
     H*                Change the group check register report to
     H*                sort based on control flag.                    *
     H*                                                               *
     H* 03/18/08 C:190 Project: WC1638                                *
     H*                Change the group check register report to
     H*                sort based on control flag.                    *
     H*                                                               *
     H* 07/24/07 C:191 Project: 132WIZ                                *
     H*                Include EOB# and Provider Bill# on report      *
     H*                                                               *
     H* 12/15/06 C:15  Support: WC091/56282                           *
     H*                Changed XXSS# to alphanumeric to prevent       *
     H*                decimal-data errors.                           *
     H*                                                               *
     H* 03/23/05 C:15  Project: WC1269                                *
     H*                Added code to load the Payee out of country    *
     H*                address.                                       *
     H*                                                               *
     H* 05/10/01 C:15 Project: WC1020                                 *
     H*               Added code to read the check schedule records   *
     H*               by CO#/PRDT/ACCT/CKTP/FND.  And read the rqst   *
     H*               pmt records by CO#/CKTP/FND.                    *
     H*                                                               *
     H*  04/30/01 C:15  Support: WC082/27202                          *
     H*                 Added code to reset the company name printed  *
     H*                 on the report if the Alternate company name   *
     H*                 is used for a specific group.                 *
     H*                                                               *
     H*  04/10/00 C:118 PROJECT: WC987
     H*                 LOAD ALTERNATE COMPANY NAME FROM FUND MASTER
     H*                 IF AVAILABLE.
     H*
     H*  04/01/99 C:90 PROJECT: WC889                                 *
     H*                ADD USAGE OF PDADR2 WITH PDADR01.              *
     H*                                                               *
     H*  07/23/98 C:33 SEQ#: WC051/12353                              *
     H*                USE FED ID # EDITING FOR FEIN'S NOT SS# EDITING*
     H*                                                               *
     H*  06/11/98 C:33 SEQ#: WC065/12184                              *
     H*               -MAKE SURE DATE PRINTED ON REPORT IS NOT ZERO.  *
     H*               -PRINT THE CORRECT GROUP #.                     *
     H*                                                               *
     H*  03/02/98 C:85 PROJECT: WC577                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                                                               *
     H*  03/06/95 C:15 PROJECT:                                       *
     H*                ADDED FINAL TOTALS TO THE SUMMARY REPORT.      *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  *INOA  -  OVERFLOW INDICATOR QSYSPRT1                        *
     H*  *INOF  -  OVERFLOW INDICATOR QSYSPRT                         *
     H*  *IN20  -  DO NOT PRINT PROVIDER INFORMATION IN HEADING       *
     H*  *IN60  -  GENERAL USAGE                                      *
     H*                                                               *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FWAC3L4    IF   E           K DISK
     FWARPLU    IF   E           K DISK    USROPN
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMPDL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FSMBUP     IF   E             DISK
     FWARPLX    IF   E           K DISK    USROPN
     F                                     RENAME(PRP01:PRP02)
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     FQSYSPRT1  O    F  132        PRINTER OFLIND(*INOA)

     d procStep        s              1  0
     d limitStep       s              1  0
     d clientID        s              3    inz('000')
     D SZ              S              1    DIM(13)
     D*
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXSS#                  1      9
     D  XXSS1                  1      3
     D  XXSS2                  4      5
     D  XXSS3                  6      9
     D*
     D*                                                               *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE CHECK PROCESSING FILE
     C*
     C     CPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT
     C*
     C*   KEY LIST FOR THE FUND FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CPFND
     C*
     C*   KEY LIST FOR THE PROVIDER FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    RPFEIN
     C                   KFLD                    RPMOD
     C*
     C*   KEY LIST FOR THE REQUEST PAYMENT FILE
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CPCKTP
     C                   KFLD                    CPFND
     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C*
     C*   KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  ENTRY PARM LIST
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  INITIALIZE FIELDS
     C*
     C     *LIKE         DEFINE    RPCO#         XXCO#
     C     *LIKE         DEFINE    RPTRDT        XXDT
     C     *LIKE         DEFINE    ORFIL#        XXPRT
     C     *LIKE         DEFINE    RPFEIN        SVFEIN
     C     *LIKE         DEFINE    RPMOD         SVMOD
     C     *LIKE         DEFINE    RPCHK#        SVCHK#
     C     *LIKE         DEFINE    PDNAME        XXCNAM
     C     *LIKE         DEFINE    PDADR1        XXADR1
     C     *LIKE         DEFINE    PDADR2        XXADR2
     C     *LIKE         DEFINE    PDADR1        XXCSZ
     C     *LIKE         DEFINE    ORTITL        X1TITL
     C*
     C     *LIKE         DEFINE    ORTITL        XXTITL
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQDATE        X1DATE            8 0
     C*
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C                   Z-ADD     XXDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDATE            6 0
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      *BLANKS       XXFNAM           20
     C                   MOVE      *BLANKS       XXPNAM           25
     C                   Z-ADD     0             XXCNT             5 0
     C                   Z-ADD     0             XXAMT            11 2
     C                   Z-ADD     0             X1CNT             5 0
     C                   Z-ADD     0             X1AMT            11 2
     C                   Z-ADD     0             X2CNT             5 0
     C                   Z-ADD     0             X2AMT            11 2
     C                   MOVE      'Y'           XX1ST             1
     C                   MOVE      'Y'           XXNORC            1
     C                   MOVE      '0'           *IN20
     C                   MOVE      '1'           *INOF
      *
     C     1             CHAIN     SMBUP                              60
     C     BUGCKS        IFEQ      'Y'
     C                   OPEN      WARPLX
     C                   ELSE
     C                   OPEN      WARPLU
     C                   ENDIF

     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILLS
     C                   PARM      *BLANK        ##NPWC            1            NON-PDS WC  LS
     C                   PARM      *BLANK        ##OS              1            OUTOME/SAVINGS
     C                   PARM      *BLANK        ##ARLS            1            Auto Release Flag
     C                   PARM      *BLANK        ##CCOD            1            Correct Coding Flag
     C                   PARM      *BLANK        ##CRVW            1            Code Review Flag
     C                   PARM      *BLANK        ##PROM            1            ProMatch Flag
     C                   PARM      *BLANK        ##MCRV            1            Medicare CR Flag
     C                   PARM      *BLANK        ##FREQ            1            Frequency Flag
     C                   PARM      *BLANK        ##CRAC            1            CR Auto Complete
     C                   PARM      *BLANK        ##PREO            1            CR Auto Complete
     C                   PARM      *BLANK        ##PARN            1            Par/Non-Par/Both
     C                   PARM      *BLANK        ##MGRI            1            Multi Group Restr
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SVCONM           40
     C*
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*
     C*   GET THE ROUTING INFO FOR THE SUMMARY REPORT.
     C*
     C                   MOVE      '1'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*
     C*  CENTER PROGRAM TITLE
     C*
     C                   MOVEL(P)  ORTITL        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        XXTITL
     C*                                                               *
     C*
     C                   MOVE      '1'           *INOA
     C                   END
      /free
        GetClientId(ClientID);
        If ClientID = '102';
           *In25 = *On;
        Else;
           *In25 = *Off;
        Endif;
      /end-free
     C*
     C*   START READING THE CHECK PROCESSING FILE
     C*
     C     CPKY          CHAIN     PCP01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*   CONVERT THE GROUP CHECK DATE RANGE TO MMDDYY.
     C*
     C                   Z-ADD     CPFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFDT             6 0
     C*
     C                   Z-ADD     CPTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTDT             6 0
     C*
     C*   GET THE FUND NAME
     C*
     C     FNKY          CHAIN     PFN01                              60
     C   60              MOVE      *BLANK        FNNAME
     C                   MOVEL     FNNAME        XXFNAM
     C*
     C     *IN60         IFEQ      *OFF
     C     FNANAM        ANDNE     *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ELSE
     C                   MOVEL(P)  SVCONM        CONAME
     C                   ENDIF                                                  *IN60=OFF
     C*
     C*   CENTER COMPANY NAME
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        CONAME
     C*
     C*   GET THE ROUTING INFO FOR THE DETAIL REPORT.
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  CENTER PROGRAM TITLE
     C*
     C                   MOVEL(P)  ORTITL        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        X1TITL
     C                   MOVE      '1'           *INOF
     C                   END

      /free
       Select;
       When CPeftc  = *blank;
          procStep  = 1;
          limitStep = 1;
       When CPeftc = 'E';
          procStep  = 2;
          limitStep = 2;
       When CPeftc = 'B';
          procStep  = 1;
          limitStep = 2;
       //*** Beg Add *** 07/19/21 *********************************************
       When CPeftc = 'V';
          procStep  = 3;
          limitStep = 3;
       When CPeftc = 'A';
          procStep  = 1;
          limitStep = 3;
       //*** End Add *** 07/19/21 *********************************************
       Endsl;

       For procStep to limitStep;
          If procStep = 2;
             *inof = *on;
          //*** Beg Add *** 07/19/21 ******************************************
             *inoa = *on;
          //*** End Add *** 07/19/21 ******************************************
             *in99 = *on;
             xx1st = 'Y';
             xxnorc = 'Y';
             svChk# = 0;
          Else;
          //*** Beg Add *** 07/19/21 ******************************************
             If procStep = 3;
                *inof = *on;
                *inoa = *on;
                *in98 = *on;
                xx1st = 'Y';
                xxnorc = 'Y';
                svChk# = 0;
             Else;
          //*** End Add *** 07/19/21 ******************************************
                *in98 = *off;
                *in99 = *off;
          //*** Beg Add *** 07/19/21 ******************************************
             Endif;
          //*** End Add *** 07/19/21 ******************************************
          Endif;
      /end-free

     C*   SELECT ONLY REQUEST PAYMENTS BEING PROCESSED
      /free
       If BUgcks = 'Y';
          Setll rpKy prp02;
          Reade rpKy prp02;
       Else;
          Setll rpKy prp01;
          Reade rpKy prp01;
       Endif;
       Dow Not(%Eof);
      /end-free

     C     RPSTAT        IFEQ      'Y'

      /free
       Select;
       When procStep = 1 and RPeftc <> ' ';
          Exsr GetNextRcd;
          Iter;
       When procStep = 2 and RPeftc <> 'E';
          Exsr GetNextRcd;
          Iter;
       //*** Beg Add *** 07/19/21 **********************************************
       When procStep = 3 and RPeftc <> 'V';
          Exsr GetNextRcd;
          Iter;
       //*** End Add *** 07/19/21 **********************************************
       Endsl;
      /end-free

     C*   IF FIRST TIME THRU, GET PROVIDER INFO
     C*
     C     XX1ST         IFEQ      'Y'
     C                   EXSR      SRCHK
     C                   Z-ADD     RPCHK#        SVCHK#
     C                   MOVE      'N'           XX1ST
     C                   MOVE      'N'           XXNORC
     C                   END
     C*
     C*   PRINT THE CHECK TOTALS IF THE CHECK# CHANGED.
     C*
     C     RPCHK#        IFNE      SVCHK#
     C                   EXSR      SRCHK
     C                   Z-ADD     RPCHK#        SVCHK#
     C                   END
     C*
     C*   GET THE CLAIMANTS SOCIAL SECURITY #.
     C*
     C                   MOVE      *BLANK        XXCNAM
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CMSS#         XXSS#
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'XXSS#'       QQNAME           25
     C     XXSS#         PARM      XXSS#         QQVAL           100
     C                   MOVE      CMJRST        XXST              2
     C*
     C*   GET THE CLAIMANTS NAME.
     C*
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       QQFSN            15
     C                   MOVE      *BLANKS       QQMID            56
     C                   MOVE      '1'           QQTERM            1
     C                   MOVEL     CDFSN         QQFSN
     C                   MOVEL     CDMID         QQMID
     C                   CALL      'SRFRLS'
     C                   PARM                    QQMID
     C                   PARM                    QQFSN
     C                   PARM                    QQTERM
     C*
     C                   MOVE      *BLANKS       QQFNAM           15
     C                   MOVE      *BLANK        QQLNAM           56
     C                   MOVEL     QQMID         QQFNAM
     C                   MOVEL     CDLSN         QQLNAM
     C                   MOVE      '1'           QQTERM
     C                   CALL      'SRNAME'
     C                   PARM                    QQLNAM
     C                   PARM                    QQFNAM
     C                   PARM                    QQTERM
     C                   MOVEL     QQLNAM        XXCNAM
     C                   END
     C*
     C                   END
     C*
     C*   CONVERT THE DATES OF SERVICE TO MMDDYY.
     C*
     C                   Z-ADD     RPFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFRDT            6 0
     C*
     C                   Z-ADD     RPTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTODT            6 0
     C* Retrieve Bill ID using API
     C                   MOVE      XXCO#         ##CO#             3
     C                   MOVE      RPFND         ##FND             3
     C                   MOVE      RPFYR         ##FYR             3
     C                   MOVE      RPCASE        ##CASE            7
     C                   MOVE      RPEOB#        ##EOB#            9
     C*
     C*
     C     ##MCIF        IFEQ      'Y'
     C*
     C                   CALL      'WA068R'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##CASE
     C                   PARM                    ##EOB#
     C                   PARM      *BLANKS       ##SINV           35
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       XXSINV           16
     C*
     C                   ENDIF                                                   ##MCIF = 'Y'
     C*
     C*
     C                   MOVE      *BLANKS       XXSINV
     C     ##RTCD        IFEQ      ' '
     C                   MOVEL(P)  ##SINV        XXSINV
     C                   ENDIF
     C*
     C*   PRINT THE CHECK DETAIL INFORMATION.
     C*
     C   OF              EXCEPT    HEAD
     c                   If        *inof = *on
     c                   Eval      *inof = *off
     c                   Endif
     C                   EXCEPT    DET
     C*
     C*   ACCUMULATE THE CHECK TOTALS
     C*
     C                   ADD       1             XXCNT
     C                   ADD       RPAMT         XXAMT
     C                   ADD       RPAMT         X1AMT
     C*
     C*  GET THE NEXT REQUEST PAYMENT RECORD.
     C*
     C                   END
     C     BUGCKS        IFEQ      'Y'
     C     RPKY          READE     PRP02                                  60
     C                   ELSE
     C     RPKY          READE     PRP01                                  60
     C                   ENDIF                                                  BUGCKS = 'Y'
     C                   END
     C*
     C*  PRINT THE CHECK TOTALS AND THE FUND TOTALS.
     C*
     C     XX1ST         IFEQ      'N'
     C   OA              EXCEPT    HEAD1
     C                   EXCEPT    DET1
     C                   EXCEPT    TOT1
     C                   EXCEPT    TOT
     C                   ELSE
     C                   MOVE      '1'           *IN20
     C                   EXCEPT    HEAD
     C                   EXCEPT    NORCD
     C                   END
     C*
     C*  ACCUMULATE FINAL TOTALS.
     C*
     C                   ADD       X1CNT         X2CNT
     C                   ADD       X1AMT         X2AMT
     C*
     C*  CLEAR ALL TOTAL FIELDS.
     C*
     C                   Z-ADD     0             XXCNT
     C                   Z-ADD     0             XXAMT
     C                   Z-ADD     0             X1CNT
     C                   Z-ADD     0             X1AMT
     C                   MOVE      'Y'           XX1ST
     C                   MOVE      '0'           *IN20
     C                   Z-ADD     0             PAGE
     C*
     C*  GET THE NEXT CHECK PROCESSING RECORD.
     C*
      /free
       EndFor;
      /end-free
     C*
     C     CPKY          READE     PCP01                                  60
     C                   END
     C*
     C*  IF NO RECORDS WERE PROCESSED, PRINT MESSAGE.
     C*
     C     XXNORC        IFEQ      'Y'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    NORC1
     C                   ELSE
     C*
     C*  IF RECORDS WERE PROCESSED PRINT FINAL TOTALS.
     C*
     C                   EXCEPT    TOT2
     C                   END
     C     BUGCKS        IFEQ      'Y'
     C                   CLOSE     WARPLX
     C                   ELSE
     C                   CLOSE     WARPLU
     C                   ENDIF
     C*
     C*  END OF PROGRAM
     C*
      /Free
       xxPrt = '0';
       Chain ORKY POR01;
       If %found;
          Except Route;
       Endif;
       xxPrt = '1';
       Chain ORKY POR01;
       If %found;
          Except Rout1;
       Endif;
      /End-free

     C                   MOVE      '1'           *INLR

      *=========================================================
      *  GetNextRcd - Retrieves next check request record
      *=========================================================
      /free
       Begsr GetNextRcd;
       If BUgcks = 'Y';
          Reade rpKy prp02;
       Else;
          Reade rpKy prp01;
       Endif;
       Endsr;
      /end-free
     C**************************************************************************
     C*  SRCHK  - THIS SUBROUTINE PRINTS THE GROUP CHECK TOTAL FOR THE
     C*           DETAIL AND SUMMARY REPORTS.  NEXT, THE FUND TOTALS ARE
     C*           ACCUMULATED AND THEN THE PROVIDER INFO IS RETRIEVED
     C*           FOR THE DETAIL REPORT HEADING.
     C**************************************************************************
     C     SRCHK         BEGSR
     C*
     C     XX1ST         IFEQ      'N'
     C   OA              EXCEPT    HEAD1
     C                   EXCEPT    DET1
     C                   EXCEPT    TOT
     C                   END
     C*
     C                   ADD       1             X1CNT
     C                   Z-ADD     0             XXCNT
     C                   Z-ADD     0             XXAMT
     C                   Z-ADD     RPFEIN        SVFEIN
     C                   Z-ADD     RPMOD         SVMOD
     C*
     C*   CHAIN TO THE PROVIDER FILE
     C*
     C                   MOVE      *BLANKS       XXPNAM
     C                   MOVE      *BLANKS       XXADR1
     C                   MOVE      *BLANKS       XXADR2
     C                   MOVE      *BLANKS       XXCSZ
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVEL     PDNAME        XXPNAM
     C                   MOVEL     PDADR1        XXADR1
     C                   MOVEL     PDADR2        XXADR2
     C*
     C*  LOAD CITY, STATE AND ZIP
     C*
     C     PDADR3        IFNE      *BLANK
     C                   MOVEL     PDADR3        XXCSZ
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEA     PDST          SZ
     C                   MOVEL     PDZIP         XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C                   MOVE      PDZIP         XXZP2             4
     C     XXZP2         IFNE      '0000'
     C                   MOVE      '-'           SZ(9)
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     PDCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ
     C*
     C                   END
     C*
     C                   END
     C*
     C*   SKIP TO NEW PAGE FOR NEXT PROVIDER.
     C*
     C                   MOVE      '1'           *INOF
     C*
     C                   ENDSR
     C**************************************************************************
     OQSYSPRT   E            ROUTE          1 06
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       X1TITL              80
     O          E            ROUTE          2
     O                                           19 'DATE:'
     O                       XXDATE              28 '  /  /  '
     O*
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O*
     O                       CONAME              87
     O*
     O                       XXDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O*
     O                       X1TITL             106
     O*
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O*
     O                                            6 'GROUP:'
     O                       CPFND         4     10
     O                       FNNAME              71
     O          E   N20      HEAD           1
     O                                           10 'PROVIDER:'
     O                       RPFEIN        4     20
     O                       RPMOD               24
     O*** Beg Chg *** 07/19/21 *************************************************
     O*****         n99                          72 'CHECK #:'
     O*****          99                          75 'EFT SEQ#:'
     O              n99n98                       72 'CHECK #:'
     O               99n98                       75 'EFT SEQ#:'
     O               98                          74 'EPAY SEQ#:'
     O*** End Chg *** 07/19/21 *************************************************
     O                       RPCHK#        4     84
     O          E   N20      HEAD           1
     O                       PDNAME              51
     O*** Beg Chg *** 07/19/21 *************************************************
     O******        n99                          75 'CHECK DATE:'
     O******         99                          75 'EFT DATE:'
     O              n99n98                       75 'CHECK DATE:'
     O               99n98                       75 'EFT DATE:'
     O               98                          74 'EPAY DATE:'
     O*** End Chg *** 07/19/21 *************************************************
     O                       XXDATE              84 '  /  /  '
     O          E   N20      HEAD           1
     O                       XXADR1              51
     O          E   N20      HEAD           1
     O                       XXADR2              51
     O          E   N20      HEAD           2
     O                       XXCSZ               51
     O          E            HEAD           1
     O                                           27 'JURISDICTION'
     O                                          122 'RESERVE'
     O          E            HEAD           2
     O                                            5 'YEAR'
     O                                           13 'CASE'
     O                                           23 'STATE'
     O                                           43 'CLAIMANT NAME'
     O                                           83 'SOC SEC #'
     O                                          103 'DATES OF SERVICE'
     O                                          110 'CODE'
     O                                          123 'AMOUNT PAID'
     O              n25                         147 'BILL ID'
     O              n25                         134 'EOMB#'
     O               25                         136 'REMARKS'
     O*
     O          E            DET            1
     O                       RPFYR         4      5
     O                       RPCASE        4     13
     O                       XXST                22
     O                       XXCNAM              70
     O                       XXSS1               76
     O                                           77 '-'
     O                       XXSS2               79
     O                                           80 '-'
     O                       XXSS3               84
     O                       XXFRDT              95 '  /  /  '
     O                                           97 '-'
     O                       XXTODT             105 '  /  /  '
     O                       RPRECD             110
     O                       RPAMT         J    124
     O              n25      RPEOB#        4    134
     O              n25      XXSINV             152
     O               25      RPRMDS             159
     O          E            TOT         1  1
     O                                           92 '*** PROVIDER TOTALS:'
     O                       XXCNT         4    102
     O                       XXAMT         J    124
     O*
     O          E            NORCD       2  2
     O*** Beg Chg *** 07/19/21 *************************************************
     O*****         n99                          33 '*** NO CHECKS WERE PROCE'
     O*****         n99                          56 'SSED FOR THIS GROUP ***'
     O*****          99                          33 '*** NO EFT''S WERE PROCE'
     O*****          99                          56 'SSED FOR THIS GROUP ***'
     O              n99n98                       33 '*** NO CHECKS WERE PROCE'
     O              n99n98                       56 'SSED FOR THIS GROUP ***'
     O               99n98                       33 '*** NO EFT''S WERE PROCE'
     O               99n98                       56 'SSED FOR THIS GROUP ***'
     O               98                          33 '*** NO EPAY''S WERE PROCE'
     O               98                          56 'SSED FOR THIS GROUP ***'
     O*** End Chg *** 07/19/21 *************************************************
     OQSYSPRT1  E            ROUT1          1 06
     O          E            ROUT1          2
     O                                           19 'REPORT TITLE:'
     O                       XXTITL              80
     O          E            ROUT1          2
     O                                           19 'DATE:'
     O                       XXDATE              28 '  /  /  '
     O          E            ROUT1          1 59
     O                       #BRND1
     O          E            ROUT1          1
     O                       #BRND2
     O          E            HEAD1          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              87
     O                       XXDATE        Y    132
     O          E            HEAD1          2
     O                       Q1PGM               10
     O                       XXTITL             106
     O                                          126 'PAGE:'
     O                       PAGE1         Z    132
     O          E            HEAD1          1
     O*** Beg Chg *** 07/19/21 *************************************************
     O******        n99                          43 'GROUP CHECK'
     O******         99                          41 'GROUP EFT'
     O              n99n98                       43 'GROUP CHECK'
     O               99n98                       41 'GROUP EFT'
     O               98                          42 'GROUP EPAY'
     O*** End Chg *** 07/19/21 *************************************************
     O                                           65 '- - - - - - - -'
     O                                           74 'PROVIDER'
     O                                           90 '- - - - - - - -'
     O                                          101 'GROUP'
     O                                          112 'GROUP'
     O*** Beg Chg *** 07/19/21 *************************************************
     O********      n99                         131 'GROUP CHECK'
     O********       99                         130 'GROUP EFT'
     O              n99n98                      131 'GROUP CHECK'
     O               99n98                      130 'GROUP EFT'
     O               98                         129 'GROUP EPAY'
     O*** End Chg *** 07/19/21 *************************************************
     O          E            HEAD1          2
     O                                            4 'GRP'
     O                                           16 'GROUP NAME'
     O                                           42 'DATE RANGE'
     O                                           59 'FED ID#'
     O                                           65 'MOD'
     O                                           71 'NAME'
     O*** Beg Chg *** 07/19/21 *************************************************
     O*********     n99                         102 'CHECK#'
     O*********     n99                         115 'CHECK DATE'
     O*********      99                         101 'EFT #'
     O*********      99                         113 'EFT DATE'
     O              n99n98                      102 'CHECK#'
     O              n99n98                      115 'CHECK DATE'
     O               99n98                      101 'EFT #'
     O               99n98                      113 'EFT DATE'
     O               98                         102 'EPAY #'
     O               98                         114 'EPAY DATE'
     O*** End Chg *** 07/19/21 *************************************************
     O                                          129 'AMOUNT'
     O          E            DET1           1
     O                       RPFND         4      4
     O                       XXFNAM              26
     O                       XXFDT               36 '  /  /  '
     O                                           38 '-'
     O                       XXTDT               47 '  /  /  '
     O                       SVFEIN              61 '  -       '
     O                       SVMOD         4     65
     O                       XXPNAM              92
     O                       SVCHK#        4    102
     O                       XXDATE             114 '  /  /  '
     O                       XXAMT         J    132
     O          E            TOT1        1  3
     O                                          102 '*** GROUP TOTALS:'
     O                       X1CNT         4    114
     O                       X1AMT         J    132
     O          E            TOT2        1  3
     O                                          102 '***** FINAL TOTALS:'
     O                       X2CNT         4    114
     O                       X2AMT         J    132
     O          E            NORC1       2  2
     O                                           33 '*** NO RECORDS WERE PROC'
     O                                           42 'ESSED ***'
