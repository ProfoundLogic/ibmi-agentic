     H*Copyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_OVR  dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7272R - LOSS RATIO BY JOB CLASS
     H*****************************************************************
     H* REVISIONS:                                                    *
      *                                                               *
      * 06/17/24 C:344 PROJECT: WC2952                                *
      *                Expand rate field to zone 8.3                  *
      *                                                               *
      * 10/20/15 C:302 PROJECT: WC2364                                *
      *                Exclude Non-Ratable class codes                *
      *                 from this report.                             *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/22/13  C:287 Seq#: SY266_OVR                               *
     H*                 Added DFTACTGRP and ACTGRP compile options    *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 04/20/09 C:190 PROJECT WC1473                                 *
     H*                Add column to display the "Spec" & "Extension" *
     H*                filed on report.                               *
     H*                                                               *
     H* 04/01/08 C:183 PROJECT: WC1619                                *
     H*                Call SREFFD to set the effective date to be    *
     H*                used on SETGT.                                 *
     H*                                                               *
     H* 07/30/04 C:136 Support: WA090 43374                           *
     H*                Previous project change caused wrong class     *
     H*                description to be written on report.           *
     H*                                                               *
     H* 04/26/04 C:136 Project: WC1169                                *
     H*                Exclude Occupational Disease class codes.      *
     H*                                                               *
     H* 08/30/01  C:90   PROJECT: WC1035                              *
     H*                  Expand premium fields from 9.2 to 11.2       *
     H*                                                               *
     H* 08/15/01  C:90   PROJECT: WC1035                              *
     H*                  Expand rate field from 5.2 to 7.2            *
     H*                                                               *
     H* 04/10/00  C:118  PROJECT: WC987                               *
     H*                  Load alternate company name from fund master.*
     H*                                                               *
     H* 12/15/99  C:15   Project: WC577                               *
     H*                  Y2K - Chgd FYR parm to 3A.                   *
     H*                                                               *
     H* 05/27/99  C:35   PROJECT: SUPPORT WA102 #18254                *
     H*                  IF POLICY EFFECTIVE DATE FALLS AFTER THE RUN *
     H*                  DATE, THE PRORATA FACTOR SHOULD BE ZERO.     *
     H*                                                               *
     H* 02/10/99  C:35   PROJECT: SUPPORT WA096 16506                 *
     H*                  REMOVE VERBIAGE REFERRING TO RATES.          *
     H*                                                               *
     H* 02/08/99  C:35   PROJECT: SUPPORT WA100 16468                 *
     H*                  USE AUDITED MANUAL PREMIUM IF ELFAF = A,     *
     H*                  OTHERWISE, USE EXTIMATED ANNUAL PREMIUM.     *
     H*                  USE CURRENT DATE INSTEAD OF PAYROLL DATE     *
     H*                  PARAMETER TO ACCESS THE PAYROLL CLASS TABLE. *
     H*                                                               *
     H* 01/14/99 C:90 PROJECT WC860                                   *
     H*               REMOVE F1RTCD IN CHAIN TO GET PRDESC.           *
     H*               TAKE MANUAL PREMIUM FROM EMAMPR/WMEPP.          *
     H*                                                               *
     H* 04/29/98 C:78 PROJECT WC745                                   *
     H*               CORRECT PROJ DONE ON 09/08/97. F2CT01 REPLACED  *
     H*               WITH WMFNP/FNCGFL.                              *
     H*                                                               *
     H* 09/08/97  C:78  PROJ:WC745                                    *
     H*                 F2CT01 REPLACED WITH WMFNP/FNCGFL             *
     H*                                                               *
     H* 01/30/97 C:65 PROJECT WC635                                   *
     H*               DO NOT APPLY AIRCRAFT SURCHARGE TO MANUAL PREM. *
     H*                                                               *
     H* 11/28/95 C:33 PROJECT CM007                                   *
     H*               ON NON-COMMON EXPIRATION DATE POLICIES, WHEN    *
     H*               CALCULATING PRORATA FACTOR (XXPROR) WE SHOULD   *
     H*               CONSIDER - ELPFDT -> XXYMD / ELPFDT -> ELPTDT.  *
     H*                               (EDAYS)    /     (FDAYS)        *
     H*                                                               *
     H*****************************************************************
     F*
      ***** BEG CHG  **** 04/20/09 ************************************
     F******WMEPL4  IF  E           K        DISK
     FWMEPL4E   IF   E           K DISK
     F**** END CHG **** 04/20/09 **************************************
      ***** BEG ADD  **** 04/20/09 ************************************
     FWMPXL1    IF   E           K DISK
      ***** END ADD  **** 04/20/09 ************************************
     F* * * Employer Payroll File
     F*
     FWMCMLF    IF   E           K DISK
     F* * * Claims File
     F*
     FWMFNL     IF   E           K DISK
     F* * * Fund File
     F*
     FWDF1L     IF   E           K DISK
     F* * * Fund DETAIL FILE
     F*
     F**** BEGIN CHG **** 01/14/99 ************************************
     F*WMPRL1  IF  E           K        DISK
     FWMPRL6    IF   E           K DISK
     F                                     RENAME(PPR01:PPR02)
     F**** END   CHG **** 01/14/99 ************************************
     F* * * Fund File
     F*
     FSMORL     IF   E           K DISK
     F* * * Batch Routing File
     F*
     FSMCOL     IF   E           K DISK
     F* * * Company File
     F*
     FWMUPL     IF   E           K DISK
     F* * * User Parameter File
     F*
     F**** BEGIN DEL ***** 04/29/98 ***********************************
     F**** WDF2L   IF  E           K        DISK
     F**** END   DEL ***** 04/29/98 ***********************************
     F* * * Fund Auxiliary File
     F*
     FWDELL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWDRRL     IF   E           K DISK
     FWDHEL     IF   E           K DISK
     F*** BEG ADD *****  04/26/04  ************************************
     FWMPRL1    IF   E           K DISK
     F*** END ADD *****  04/26/04  ************************************
     F**** BEG   CHG **** 04/20/09 ************************************
     F*******QSYSPRT O   F     132     OF     PRINTER
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     F**** END CHG **** 04/20/09 **************************************
     F* * * Output File
     F*

     D                 DS
     D  CMA                    1     90  2
     D                                     DIM(10)                              PMT AMOUNT
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS
     D  CMR                    1     90  2
     D                                     DIM(10)                              RESERVE AMOUNT
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  EMPLOYER PAYROLL FILE
     C*
     C     EPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C     EPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCLAS
     C*
     C*  HARBOR EXPOSURE FILE
     C*
     C     HEKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
     C*
     C     HEKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRATE
     C*
     C*   KEY LIST FOR THE CLAIMS FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCLAS            5 0
     C*
     C*   KEY LIST FOR THE CLAIMS FILE
     C*
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    ELPFDT
     C*
     C     RRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C*
     C*  FUND MASTER
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  FUND DETAIL
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*  PAYROLL CLASSIFICATION FILE
     C*
     C     PRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCLAS
     C*
      **** BEGIN DEL **** 02/08/99 *********************************************
     C********** PRKY1     KLIST
     C******************** KFLD           XXCO#
     C******************** KFLD           XXRTCD
     C******************** KFLD           XXCLAS
     C******************** KFLD           XXPDAT
      **** END   DEL **** 02/08/99 *********************************************
     C*
     C**** BEGIN ADD **** 01/14/99 ************************************
     C     PRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXPDAT
     C**** END   ADD **** 01/14/99 ************************************
     C*** BEG ADD *****  04/26/04  ************************************
     C     PRKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    EPCLAS
     C*** BEG CHG *****  04/01/08  ************************************
     C******************** KFLD           EPEFDT
     C                   KFLD                    X1EFDT            7 0
     C*** END CHG *****  04/01/08  ************************************
     C*
     C     PRKY4         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    EPCLAS
     C*** END ADD *****  04/26/04  ************************************
      ***** BEG ADD  **** 04/20/09 ************************************
     C*  PAYROLL DESCRIPTION EXT FILE
     C     PXKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XXST
     C                   KFLD                    EPEFDT
      *
     C     PXKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XXST
      ***** END ADD  **** 04/20/09 ************************************
      *
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C**** BEGIN DEL ***** 04/29/98 ***********************************
     C*
     C*   KEY LIST FOR THE FUND AUXILIARY FILE
     C*
     C*******    F2KY      KLIST
     C*******              KFLD           XXCO#
     C*******              KFLD           XXFND
     C**** END   DEL ***** 04/29/98 ***********************************
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     ELKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EPEMP#
     C                   KFLD                    EPDIV
     C                   KFLD                    EPFYR
     C*
     C     *LIKE         DEFINE    PRRTCD        XXRTCD
     C     *LIKE         DEFINE    PRDT          XXPDAT
     C     *LIKE         DEFINE    PRDT          XXYMD
     C     *LIKE         DEFINE    PRDT          XXWK60
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELRTCD        XXRATE
     C     *LIKE         DEFINE    ELPFDT        XXEFDT
     C     *LIKE         DEFINE    RSJRST        XXST
      *** BEG ADD ***** 07/30/04 *************************************
     C     *LIKE         DEFINE    PRDESC        XXDESC
      *** END ADD ***** 07/30/04 *************************************
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C*** BEGIN CHG *** 12/15/99 **************************************
     C******************** PARM           QQFYR   2
     C                   PARM                    QQFYR             3
     C*** END CHG ***** 12/15/99 **************************************
     C                   PARM                    QQDATE            8
     C                   PARM                    QQQUE            10            MLS 1/18/88
     C                   PARM                    QQMODE            1
     C                   PARM                    QQPDAT            8
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
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQDATE        XXDATE            8 0
     C                   MOVE      QQMODE        XXMODE            1
     C                   MOVE      QQPDAT        XXPDT             8 0
     C*
      **** BEGIN DEL **** 02/08/99 *********************************************
     C******************** Z-ADDXXPDT     SRDTI
     C******************** CALL 'SRCVTDAT'DTCYMD
     C******************** MOVELSRDTO     XXPDAT
     C********************
     C******************** Z-ADDXXPDT     SRDTI
     C******************** CALL 'SRCVTDAT'DTMDY1
     C******************** MOVELSRDTO     X1PDT   60
      **** END   DEL **** 02/08/99 *********************************************
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         X1DATE            6 0
     C*
     C                   EXSR      SR1ST
      **** BEGIN ADD **** 02/08/99 *********************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPDAT
      **** END   DEL **** 02/08/99 *********************************************
     C*
     C*  GET THE FUND MASTER
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANK        XFNAME           60
     C**** BEGIN ADD ****** 04/29/98 **********************************
     C                   MOVE      *BLANK        FNCGFL
     C**** END   ADD ****** 04/29/98 **********************************
     C                   ELSE
     C                   MOVEL     FNNAME        XFNAME           60
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     FNANAM        IFNE      *BLANK
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C***** END   ADD **** 04/10/00 ***********************************
     C                   END
     C*
     C                   EXCEPT    HEAD
     C*
     C*  CHAIN TO THE FUND DETAIL FILE
     C*
     C     F1KEY         CHAIN     PF101                              60
     C*
     C*  GET DEFAULT RATE SET CODE FROM THE FUND DETAIL FILE.
     C*  THIS RATE SET CODE REPRESENTS THE MAJORITY OF EMPLOYERS.
     C*
     C                   MOVE      *BLANKS       XXRTCD
     C**** BEGIN DEL **** 01/14/99 ************************************
     C**N60                MOVELF1RTCD    XXRTCD
     C**** END   DEL **** 01/14/99 ************************************
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
     C***** BEGIN DEL ******* 04/29/98 ********************************
     C*
     C*  LOAD FLAG THAT DETERMINES WHETHER OR NOT THIS FUND WAS SET-UP
     C*  FOR NON-COMMON EXPIRATION DATE POLICIES
     C*
     C**** FLAG (FNCGL) ALREADY LOADED WHEN CHAINED TO WMFNL *********
     C********             MOVE *BLANK    XXFCT1  1
     C********   F2KY      CHAINPF201                60
      ***** BEGIN CHG ******* 09/08/97 ********************************
     C**N60****************MOVELF2CT01    XXFCT1
     C**N60***             MOVELFNCGFL    XXFCT1
      ***** END   CHG ******* 09/08/97 ********************************
     C***** END   DEL ******* 04/29/98 ********************************
     C*
     C*  DO THE FOLLOWING CALCULATIONS ONLY IF THIS FUND IS NOT SET
     C*  UP FOR NON-COMMON EXPIRATION DATE POLICIES
     C*
     C******* BEGIN CHG ***** 04/29/98 ********************************
     C******     XXFCT1    IFNE 'Y'
     C     FNCGFL        IFNE      'Y'
     C******* DEL   CHG ***** 04/29/98 ********************************
     C*
     C*  GET NUMBER OF FUND POLICY DAYS
     C*
     C                   Z-ADD     F1STPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C*
     C                   Z-ADD     F1ENPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO              8 0
     C*
     C                   MOVE      XQFROM        QQFROM            8 0
     C                   MOVE      XQTO          QQTO              8 0
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C     QQDAYS        IFEQ      366
     C                   Z-ADD     365           FDAYS
     C                   ELSE
     C                   Z-ADD     QQDAYS        FDAYS             5 0
     C                   END
     C*
     C*
     C*  GET NUMBER OF POLICY DAYS
     C*
     C                   Z-ADD     F1STPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     XXDATE        XQTO
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXWK60
     C*
     C     F1ENPO        IFLT      XXWK60
     C*
     C                   Z-ADD     F1ENPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO
     C*
     C                   END
     C                   MOVE      XQFROM        QQFROM
     C                   MOVE      XQTO          QQTO
     C                   Z-ADD     *ZERO         QQDAYS
     C                   MOVE      *BLANK        QQERR
     C                   MOVE      '1'           QQTERM
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C*
     C                   Z-ADD     QQDAYS        EDAYS             5 0
     C*
     C     XXYMD         IFGT      F1ENPO
     C                   Z-ADD     FDAYS         EDAYS
     C                   END
     C     EDAYS         DIV       FDAYS         XXPROR            5 4
     C                   END                                                    FNCGFL <> Y
     C*
     C*  INITIALIZE FIELDS
     C*
     C                   Z-ADD     0             COUNT1            5 0
     C                   Z-ADD     0             CLEMPR           11 2
     C                   Z-ADD     0             C2EMPR           11 2
     C                   Z-ADD     0             XXPRM2           11 2
     C                   Z-ADD     0             XXPRM3           11 2
     C                   Z-ADD     0             XXPREM
     C                   Z-ADD     0             CLMS              5 0
     C                   Z-ADD     0             TOTINC           11 2
     C                   Z-ADD     0             XXPYRL           11 2
     C                   Z-ADD     0             XXEMP#
     C*
     C*  CHAIN OUT TO THE EMPLOYER PAYROLL FILE AND GET NUMBER OF
     C*  EMPLOYERS AND MANUAL PREMIUM
     C*
     C     EPKEY         CHAIN     PEP01                              50
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C     ELKY3         CHAIN     PEL01                              60
     C                   MOVE      ELRTCD        XXRATE
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C*** BEG ADD *****  04/01/08  *******************************************
     C                   Z-ADD     0             X1EFDT
     C                   MOVE(P)   ELCO#         ##CO#             3
     C                   MOVE(P)   ELFND         ##FND             3
     C                   MOVE(P)   ELFYR         ##FYR             3
     C                   MOVE(P)   ELEMP#        ##EMP#            9
     C                   MOVE(P)   ELDIV         ##DIV             5
     C                   MOVE(P)   EPEFDT        ##DATE            7
     C                   MOVE(P)   'P'           ##TYPE            1
     C                   MOVE(P)   EPCLAS        ##CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##DATE
     C                   PARM                    ##TYPE
     C                   PARM                    ##CLAS
      *
     C                   MOVE      ##DATE        X1EFDT
     C*** END ADD *****  04/01/08  *******************************************
      *** BEG ADD ***** 04/26/04 ***********************************
     C     PRKY3         SETGT     PPR01
     C     PRKY4         READPE    PPR01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      PROCCD        XXOCCD            1
      *** Beg Add ***** 10/20/15
     C                   MOVE      PRNRAT        XXNRAT            1
      *** End Add ***** 10/20/15
     C                   ELSE
     C                   MOVE      *BLANK        XXOCCD
      *** Beg Add ***** 10/20/15
     C                   MOVE      *BLANK        XXNRAT
      *** End Add ***** 10/20/15
     C                   ENDIF
      *** END ADD ***** 04/26/04 ***********************************
     C*
     C     EPSTAT        IFNE      'I'
     C     EPCLAS        ANDNE     RRDCCD
      *** BEG ADD ***** 04/26/04 ***********************************
     C     XXOCCD        ANDNE     'Y'
      *** END ADD ***** 04/26/04 ***********************************
      *** Beg Add ***** 10/20/15
     C     XXNRAT        ANDNE     'Y'
      *** End Add ***** 10/20/15
     C  N85              Z-ADD     EPCLAS        XXCLAS
      *** BEG ADD ***** 04/20/09 ***********************************
     C  N85              MOVE      EPSPEC        XXSPEC            1
     C  N85              MOVEL     EPEXT         XXEXT             5
      *** END ADD ***** 04/20/09 ***********************************
     C                   MOVE      '1'           *IN85
     C*
     C*  FOR ACCOUNTS WITH NON-COMMON EXPIRATION DATES, WE SHOULD
     C*  CALCULATE THE PRO-RATA BASED ON ELPFDT-XXYMD/365
     C*
     C     EPEMP#        IFNE      XXEMP#
     C     XXEMP#        ANDNE     *ZEROS
     C***** BEGIN CHG **** 04/29/98 ***********************************
     C********   XXFCT1    ANDEQ'Y'
     C     FNCGFL        ANDEQ     'Y'
     C***** END   CHG **** 04/29/98 ***********************************
     C     EPCLAS        ORNE      XXCLAS
     C***** BEGIN CHG **** 04/29/98 ***********************************
     C********   XXFCT1    ANDEQ'Y'
     C     FNCGFL        ANDEQ     'Y'
     C***** END   CHG **** 04/29/98 ***********************************
     C                   EXSR      SUBNC
     C                   END                                                    EPEMP#<>XXEMP#
     C*
     C     EPCLAS        IFEQ      XXCLAS
      *** BEG ADD ***** 04/20/09 *************************************
     C     EPSPEC        ANDEQ     XXSPEC
     C     EPEXT         ANDEQ     XXEXT
      *** END ADD ***** 04/20/09 *************************************
     C*
     C*  DETERMINE WHETHER ESTIMATED OR AUDITED PAYROLL SHOULD BE
     C*  LISTED BASED ON PAYROLL TYPE FLAG (EPSAFL)
     C*
     C     EPSAFL        IFNE      'A'
     C                   ADD       EPEAPR        XXPYRL                         |
     C                   ELSE                                                   |
     C                   ADD       EPAAPR        XXPYRL                         |
     C                   END                                                    |
     C*                                                    |
     C*  CALCULATE PREMIUM ACCORDING TO ENTERED RATE DATE. |
     C*                                                    |
     C                   EXSR      CALCPR                                       |
     C*
     C     XXAMPR        IFEQ      *ZEROS
     C                   ADD       XXEMPR        XXPRM3
     C                   ELSE
     C                   ADD       XXAMPR        XXPRM3
     C                   END
     C*
     C*  ACCUMULATE MANUAL PREMIUM AMOUNTS
     C*
     C     XXAMPR        IFEQ      *ZERO
     C                   ADD       XXEMPR        CLEMPR           11 2
     C                   ELSE
     C                   ADD       XXAMPR        CLEMPR           11 2
     C                   END
     C*
     C*
     C*   IF CLASS CODE HAS CHANGED GET TOTAL NUMBER OF CLAIMS AND
     C*   TOTAL INCURRED FOR PAYROLL CLASS
     C*
     C                   ELSE
     C*
     C                   EXSR      SUBTI
     C                   Z-ADD     0             XXPREM
     C     EPKY1         SETGT     PEP01
     C*
     C                   END
     C*
     C     EPEMP#        IFNE      XXEMP#
     C     EPCLAS        ORNE      XXCLAS
      *** BEG ADD ***** 04/20/09 *************************************
     C     EPSPEC        ORNE      XXSPEC
     C     EPEXT         ORNE      XXEXT
      *** END ADD ***** 04/20/09 *************************************
     C                   ADD       1             COUNT1
     C                   MOVE      EPEMP#        XXEMP#
     C                   MOVE      EPCLAS        XXCLAS
      *** BEG ADD ***** 04/20/09 *************************************
     C                   MOVEL     EPSPEC        XXSPEC
     C                   MOVEL     EPEXT         XXEXT
      *** END ADD ***** 04/20/09 *************************************
     C                   END
     C*
     C                   END
     C     EPKEY         READE     PEP01                                  50
     C*
     C                   END
     C**
     C                   MOVE      '1'           *INLR
     CLR                 EXSR      SUBNC
     C**
     CLR                 EXSR      SUBTI
     CLR                 EXSR      SUBL1
     CLR                 EXSR      SUBLR
      ***** Beg Add ***** 08/26/14 ***********************************
      /Free
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
      /End-free
      ***** End Add ***** 08/26/14 ***********************************
     C*
      *==================================================================*
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      *==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C     UPKEY         CHAIN     WMUPL                              60
     C*
     C* * * INITIALIZE FIELDS
     C                   TIME                    DSDTTM
     C*
     C     COKEY         CHAIN     SMCOL                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C*********          MOVE      '0'           XXPRT
     C*********     ORKEY         CHAIN     POR01                              60
     C*********    *IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*********          EXCEPT    ROUTE
     C*********          END
      ***** End Del ***** 08/26/14 ************************************
     C*
     C                   MOVE      '1'           *IN11                          1ST TIME INDIC.
     C*
     C                   ENDSR
     C*
      *==================================================================*
      *   GET TOTAL INCURRED, ACCUM TOTALS ** Subroutine **   S U B T I  *
      *==================================================================*
     C*
     C     SUBTI         BEGSR
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   ADD       1             CLMS              5 0          COUNT CLAIMS
     C**
     C                   Z-ADD     *ZERO         TOTPMT            9 2
     C                   Z-ADD     *ZERO         TOTRES            9 2
     C**
     C                   Z-ADD     1             R
     C                   DO        10            R                 2 0
     C**
     C**  ACCUMULATE PAYMENT AMOUNTS
     C                   ADD       CMA(R)        TOTPMT
     C**
     C**  ACCUMULATE RESERVE AMOUNTS
     C                   ADD       CMR(R)        TOTRES
     C**
     C                   END
     C                   ADD       TOTPMT        TOTINC
     C                   ADD       TOTRES        TOTINC
     C**
     C     CMKEY         READE     PCM01                                  60
     C                   END
     C*
     C*                 ROUND PREM TO NEAREST WHOLE DOLLAR
     C*
     C****** BEGIN CHG ****** 04/29/98 ********************************
     C********** XXFCT1    IFNE 'Y'
     C     FNCGFL        IFNE      'Y'
     C****** END   CHG ****** 04/29/98 ********************************
     C     CLEMPR        MULT(H)   XXPROR        XXPREM           11 0          |
     C                   END
     C                   Z-ADD     XXPREM        CLEMPR
     C*
     C*  UPDATE FUND AND FINAL TOTALS FOR CLASS
     C*
     C                   ADD       TOTINC        L1ITOT           11 2
     C                   ADD       COUNT1        L1EMPL            5 0
     C                   ADD       CLEMPR        L1EMPR           13 2
     C                   ADD       CLMS          L1TOTC            5 0
     C                   ADD       XXPYRL        L1PYRL           13 2
     C*
     C*  CATCULATE LOSS AND FREQUENCY RATIO
     C*
     C                   EXSR      SUBLF
     C*
     C*  OUTPUT DETAIL LINE ITEM
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    NOTE
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C                   EXCEPT    DETAIL
     C*
     C                   Z-ADD     0             COUNT1
     C                   Z-ADD     0             CLEMPR
     C                   Z-ADD     0             CLMS
     C                   Z-ADD     0             TOTINC
     C                   Z-ADD     0             XXPYRL
     C**
     C                   ENDSR
      *==================================================================*
      *   Loss and Frequency Ratio Calcs   ** Subroutine **   S U B L F  *
      *==================================================================*
     C*
     CSR   SUBLF         BEGSR
     C**
     C**   CALCULATE LOSS RATIO
     C*  ROUND LOSS TO NEAREST TENTH
     C     CLEMPR        IFNE      *ZERO
     C     TOTINC        DIV(H)    CLEMPR        XLOSS             7 3
     C                   ELSE
     C                   Z-ADD     *ZERO         XLOSS
     C                   END
     C     XLOSS         MULT      100           LOSS              7 2
     C*
     C**   CALCULATE FREQUENCY RATIO
     C     CLEMPR        DIV(H)    1000          THPREM            9 2
     C*
     C     THPREM        IFNE      *ZERO
     C     CLMS          DIV(H)    THPREM        FREQ              5 2
     C                   ELSE
     C                   Z-ADD     *ZERO         FREQ
     C                   END
     C*
     C                   ENDSR
     C**
      *==================================================================*
      *   L1 Calculations (Fund)           ** Subroutine **   S U B L 1  *
      *==================================================================*
     C*
     CSR   SUBL1         BEGSR
     C**
     C**   CALCULATE LOSS RATIO
     C*  ROUND LOSS TO NEAREST TENTH
     C     L1EMPR        IFNE      *ZERO
     C     L1ITOT        DIV(H)    L1EMPR        X1LOSS            7 3
     C                   ELSE
     C                   Z-ADD     *ZERO         X1LOSS
     C                   END
     C     X1LOSS        MULT      100           L1LOSS            7 2
     C*
     C**   CALCULATE FREQUENCY RATIO
     C     L1EMPR        DIV(H)    1000          THPREM            9 2
     C*
     C     THPREM        IFNE      *ZERO
     C     L1TOTC        DIV(H)    THPREM        L1FREQ            5 2
     C                   ELSE
     C                   Z-ADD     *ZERO         L1FREQ
     C                   END
     C                   ADD       L1EMPR        LREMPR           13 2
     C                   ADD       L1EMPL        LREMPL            5 0
     C                   ADD       L1TOTC        LRTOTC            5 0
     C                   ADD       L1ITOT        LRITOT           11 2
     C                   ADD       L1PYRL        LRPYRL           13 2
     C*
     C*  OUTPUT TOTALS
     C*
     C   OF              EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   EXCEPT    TOTL1
     C**
     C                   ENDSR
     C*
      *==================================================================*
      *   LR Calculations                  ** Subroutine **   S U B L R  *
      *==================================================================*
     C*
     CSR   SUBLR         BEGSR
     C*
     C**   CALCULATE LOSS RATIO
     C*  ROUND LOSS TO NEAREST TENTH
     C     LREMPR        IFNE      *ZERO
     C     LRITOT        DIV(H)    LREMPR        XRLOSS            7 3
     C                   ELSE
     C                   Z-ADD     *ZERO         XRLOSS
     C                   END
     C     XRLOSS        MULT      100           LRLOSS            7 2
     C*
     C**   CALCULATE FREQUENCY RATIO
     C     LREMPR        DIV(H)    1000          THPREM            9 2
     C*
     C     THPREM        IFNE      *ZERO
     C     LRTOTC        DIV(H)    THPREM        LRFREQ            5 2
     C                   ELSE
     C                   Z-ADD     *ZERO         LRFREQ
     C                   END
     C*
     C*  OUTPUT TOTALS
     C*
     C   OF              EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   EXCEPT    TOTLR
     C**
     C                   ENDSR
     C*
     C*==================================================================*
     C*   CALCULATE PREMIUM SUBROUTINE     ** Subroutine ** C A L C P R  *
     C*==================================================================*
     C*
     C     CALCPR        BEGSR
     C*
     C                   Z-ADD     0             XXEMPR           11 2
     C                   Z-ADD     0             XXAMPR           11 2
     C                   MOVE      *BLANKS       PRDESC
      **** BEGIN CHG **** 06/17/24 ************************************
      **** BEGIN CHG **** 08/15/01 ************************************
     C******************** Z-ADD0         XXARAT  52
     C****************** Z-ADD     0             XXARAT            7 2
     C                   Z-ADD     0             XXARAT            8 3
      **** END   CHG **** 08/15/01 ************************************
      **** END   CHG **** 06/17/24 ************************************
     C*
     C*  GET PAYROLL CLASS FILE
     C*
     C**** BEGIN CHG **** 01/14/99 ************************************
     C**         PRKY1     SETGTPPR01
     C**                   READPPPR01                    60
     C     PRKY2         SETGT     PPR02
     C                   READP     PPR02                                  60
     C**** END   CHG **** 01/14/99 ************************************
     C     *IN60         IFEQ      '0'
     C     PRCO#         COMP      XXCO#                              6060
     C  N60PRCLAS        COMP      XXCLAS                             6060
     C**** BEGIN DEL **** 01/14/99 ************************************
     C**N60      PRRTCD    COMP XXRTCD               6060
     C**** END   DEL **** 01/14/99 ************************************
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C*
     C*  ADJUST RATE IF NECESSARY
     C*
     C                   Z-ADD     PRRT          XXARAT
     C     EPSPEC        IFEQ      'F'
     C     PRLHCD        IFEQ      'N'
     C                   MOVE      ELRTCD        XXRATE
     C                   MOVE      ELPFDT        XXEFDT
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     HEKY          SETGT     PHE01
     C     HEKY2         READPE    PHE01                                  60
     C     *IN60         IFEQ      *OFF
     C     XXARAT        MULT      HELHNF        XXARAT
     C                   ENDIF                                                  *IN60 IFEQ
     C                   END
     C                   END
     C*
     C     EPSAFL        COMP      'A'                                    60
     C*
     C*  CALCULATE MANUAL PREMIUM
     C*
     C**** BEGIN CHG **** 01/14/99 ************************************
     C*****      PRPCF     IFEQ 'P'
     C*****N60   EPEAPR    DIV  100       XXWK92  92
     C***** 60   EPAAPR    DIV  100       XXWK92  92
     C*****                ELSE
     C*****N60             Z-ADDEPEAPR    XXWK92  92
     C***** 60             Z-ADDEPAAPR    XXWK92  92
     C*****                END
     C*****      XXWK92    MULT XXARAT    XXMAN   92
      **** BEGIN ADD **** 02/08/99 *********************************************
     C     ELFAF         IFEQ      'A'
      **** END   ADD **** 02/08/99 *********************************************
      **** BEGIN CHG **** 08/30/01 ************************************
     C******************** Z-ADDEPAMPR    XXMAN   92
     C                   Z-ADD     EPAMPR        XXMAN            11 2
      **** END   CHG **** 08/30/01 ************************************
      **** BEGIN ADD **** 02/08/99 *********************************************
     C                   ELSE
     C                   Z-ADD     EPEMPR        XXMAN
     C                   ENDIF                                                  ELFAF IFEQ 'A'
      **** END   ADD **** 02/08/99 *********************************************
     C**** END   CHG **** 01/14/99 ************************************
     C*
     C*  ROUND TO NEAREST DOLLAR
     C*
     C     XXMAN         IFLT      *ZERO
     C                   MULT      -1            XXMAN
     C                   END
     C     XXMAN         MULT(H)   1             XXSAVE            9 0
     C                   Z-ADD     XXSAVE        XXMAN
     C  N60              Z-ADD     XXMAN         XXEMPR
     C   60              Z-ADD     XXMAN         XXAMPR
      ***** BEG ADD  **** 04/20/09 ************************************
     C     EPEXT         IFNE      ' '
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  PXDESC        XXDESC
     C                   ENDIF
     C                   ELSE
      ***** END ADD  **** 04/20/09 ************************************
      *** BEG ADD ***** 07/30/04 **************************************
     C                   MOVEL(P)  PRDESC        XXDESC
      *** END ADD ***** 07/30/04 **************************************
      ***** BEG ADD  **** 04/20/09 ************************************
     C                   ENDIF                                                  EPEXT = ' '
      ***** END ADD  **** 04/20/09 ************************************
     C*
     C                   END
     C*
     C                   ENDSR
     C*
     C*==================================================================*
     C*   NON-COMMON EXP DATE SUBROUTINE   ** Subroutine ** S U B N C    *
     C*==================================================================*
     C*
     CSR   SUBNC         BEGSR
     C*
     C*  GET NUMBER OF DAYS ACTIVE
     C*
     C                   Z-ADD     *ZEROS        XXDIV
     C     ELKY2         CHAIN     PEL01                              60
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     XXDATE        XQTO
     C     ELPTDT        IFLT      XXYMD
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO
     C*
     C                   END
     C                   MOVE      XQFROM        QQFROM
     C                   MOVE      XQTO          QQTO
     C                   Z-ADD     *ZERO         QQDAYS
     C                   MOVE      *BLANK        QQERR
     C                   MOVE      '1'           QQTERM
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C                   Z-ADD     QQDAYS        EDAYS             5 0
     C*
     C     EDAYS         DIV       365           XXPROR            5 4
     C*
     C     XXYMD         IFGT      ELPTDT
     C                   Z-ADD     365           EDAYS
     C                   END
     C     EDAYS         DIV       365           XXPROR
     C*
     C     XXPRM3        MULT      XXPROR        XXPRM2
      **** BEGIN ADD **** 05/27/99 *********************************************
     C     XXPROR        IFLT      0
     C                   Z-ADD     0             XXPRM2
     C                   ENDIF                                                  XXPROR IFLT 0
      **** END   ADD **** 05/27/99 *********************************************
     C                   ADD       XXPRM2        XXPREM
     C                   Z-ADD     *ZEROS        XXPRM3
     C*
     C                   ENDSR
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14  **********************************
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
      ***** End Del ***** 08/26/14  **********************************
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
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14  **********************************
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
     O**********     08      OROUTQ              30
     O*  MLS 1/18/88  ADD OUT QUEUE AND INDICATORS
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14  **********************************
     O**   SELECTION CRITERIA
     O          E            ROUTE          1
     O                                           20 'POLICY YEAR  :'
     O                       XXFYR         Z   +  1
     O          E            ROUTE          1
     O                                           20 'RUN DATE     :'
     O                       X1DATE        Y   +  1
      **** BEGIN DEL **** 02/08/99 *********************************************
     O******* E  1             ROUTE
     O********************************** 20 'RATE DATE    :'
     O************************ X1PDT Y +  1
      **** END   DEL **** 02/08/99 *********************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E  160           ROUTE
     O***                                23 '(c) Copyright Progressi'
     O***                                   've Data Solutions, Inc.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              87
     O                       DSDATE        Y    132
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                           58 'POLICY YEAR'
     O                       XXFYR         Z   +  1
     O                                           79 'AS OF'
     O                       X1DATE        Y     88
      **** BEGIN DEL **** 02/08/99 *********************************************
     O********************************* 114 'PAYROLL RATE DATE:'
     O************************ X1PDT Y  123
      **** END   DEL **** 02/08/99 *********************************************
     O          E            HEAD           2
     O                                              'GROUP:'
     O                       XXFND         Z   +  1
     O                       XFNAME            +  1
     O          E            HEAD           1
      *** BEG CHG ***** 04/20/09 *************************************
     O***********                        45 '# OF'
     O                                           57 '# OF'
      *** END CHG ***** 04/20/09 *************************************
      **** BEGIN ADD **** 05/27/99 *********************************************
      *** BEG CHG ***** 04/20/09 *************************************
     O***********                        79 'EARNED'
     O                                           91 'EARNED'
      *** END CHG ***** 04/20/09 *************************************
      **** END   ADD **** 05/27/99 *********************************************
     O**** BEGIN CHG **** 01/14/99 ************************************
     O********************************  123 'LOSS'
     O********************************  131 'FREQ'
      *** BEG CHG ***** 04/20/09 *************************************
     O********************************  118 'LOSS'
     O********************************  126 'FREQ'
     O                                          129 'LOSS'
     O                                          137 'FREQ'
      *** END CHG ***** 04/20/09 *************************************
     O**** END   CHG **** 01/14/99 ************************************
     O          E            HEAD           2
     O                                           05 'CLASS'
      *** BEG CHG ***** 04/20/09 *************************************
     O********                           18 'DESCRIPTION'
     O********                           47 'EMPLOYERS'
     O                                           10 'SPEC'
     O                                           15 'EXT'
     O                                           29 'DESCRIPTION'
     O                                           59 'EMPLOYERS'
      *** END CHG ***** 04/20/09 *************************************
     O**** BEGIN CHG **** 01/14/99 ************************************
     O********************************   69 'TOTAL PAYROLL   RATE'
     O********************************   88 'MANUAL PREMIUM'
     O********************************   96 'CLAIMS'
     O********************************  112 'TOTAL INCURRED'
     O********************************  131 'RATIO   RATIO'
      *** BEG CHG ***** 04/20/09 *************************************
     O********************************   62 'TOTAL PAYROLL'
     O********************************   83 'MANUAL PREMIUM'
     O********************************   91 'CLAIMS'
     O********************************  107 'TOTAL INCURRED'
     O********************************  126 'RATIO   RATIO'
     O                                           73 'TOTAL PAYROLL'
     O                                           94 'MANUAL PREMIUM'
     O                                          102 'CLAIMS'
     O                                          118 'TOTAL INCURRED'
     O                                          137 'RATIO   RATIO'
      *** END CHG ***** 04/20/09 *************************************
     O**** END   CHG **** 01/14/99 ************************************
     O          E            DETAIL         1
     O                       XXCLAS        Z     05
      *** BEG ADD ***** 04/20/09 *************************************
     O                       XXSPEC              07
     O                       XXEXT               17
      *** END ADD ***** 04/20/09 *************************************
      *** BEG CHG ***** 07/30/04 *************************************
     O************************ PRDESC    37
      *** BEG CHG ***** 04/20/09 *************************************
     O********                 XXDESC    37
     O********                 COUNT13   45
     O********                 XXPYRLJ   63
     O                       XXDESC              48
     O                       COUNT1        3     57
     O                       XXPYRL        J     74
      *** END CHG ***** 04/20/09 *************************************
     O**** BEGIN CHG **** 01/14/99 ************************************
     O************************ XXARATJ   71
     O************************ CLEMPRJ   89
     O************************ CLMS  3   96
     O************************ TOTINCJ  113
     O************************ LOSS  J  124
     O************************ FREQ  J  132
     O**** END   CHG **** 01/14/99 ************************************
     O**** BEG   CHG **** 04/20/09 ************************************
     O************************ CLEMPRJ   84
     O************************ CLMS  3   91
     O************************ TOTINCJ  108
     O************************ LOSS  J  119
     O************************ FREQ  J  127
     O                       CLEMPR        J     95
     O                       CLMS          3    102
     O                       TOTINC        J    119
     O                       LOSS          J    130
     O                       FREQ          J    138
     O**** END   CHG **** 04/20/09 ************************************
     O          E            NOTE        1  1
     O                                           32 'RATIOS ARE DERIVED FROM '
     O                                           55 'MANUAL PREMIUM AMOUNTS.'
      **** BEGIN DEL **** 02/10/99 ************************************
     O********************************   92 'RATES ARE BASED ON'
     O********************************  114 'DEFAULT RATE SET CODE'
     O******************************** +  1 'IN GROUP DETAIL.'
      **** END   DEL **** 02/10/99 ************************************
     O          E            TOTL1       1  1
      *** BEG CHG ***** 04/20/09 *************************************
     O**********************             23 'GROUP TOTALS:'
     O***********************  L1PYRLJ   63
     O                                           35 'GROUP TOTALS:'
     O                       L1PYRL        J     74
      *** END CHG ***** 04/20/09 *************************************
     O**** BEGIN CHG **** 01/14/99 ************************************
     O***********************  L1EMPRJ   89
     O***********************  L1TOTC3   96
     O***********************  L1ITOTJ  113
     O***********************  L1LOSSJ  124
     O***********************  L1FREQJ  132
      *** BEG CHG ***** 04/20/09 *************************************
     O**********************   L1EMPRJ   84
     O**********************   L1TOTC3   91
     O**********************   L1ITOTJ  108
     O**********************   L1LOSSJ  119
     O**********************   L1FREQJ  127
     O                       L1EMPR        J     95
     O                       L1TOTC        3    102
     O                       L1ITOT        J    119
     O                       L1LOSS        J    130
     O                       L1FREQ        J    138
      *** END CHG ***** 04/20/09 *************************************
     O**** END   CHG **** 01/14/99 ************************************
     O          E            TOTLR       2  1
      *** BEG CHG ***** 04/20/09 *************************************
     O*****************                  23 'FINAL TOTALS:'
     O*****************        LRPYRLJ   63
     O                                           35 'FINAL TOTALS:'
     O                       LRPYRL        J     74
      *** END CHG ***** 04/20/09 *************************************
     O**** BEGIN CHG **** 01/14/99 ************************************
     O************************ LREMPRJ   89
     O************************ LRTOTC3   96
     O************************ LRITOTJ  113
     O************************ LRLOSSJ  124
     O************************ LRFREQJ  132
      *** BEG CHG ***** 04/20/09 *************************************
     O*******************      LREMPRJ   84
     O*******************      LRTOTC3   91
     O*******************      LRITOTJ  108
     O*******************      LRLOSSJ  119
     O*******************      LRFREQJ  127
     O                       LREMPR        J     95
     O                       LRTOTC        3    102
     O                       LRITOT        J    119
     O                       LRLOSS        J    130
     O                       LRFREQ        J    138
      *** END CHG ***** 04/20/09 *************************************
     O**** END   CHG **** 01/14/99 ************************************
     O          E            TOTLR          1 60
     O                                           32 'RATIOS ARE DERIVED FROM '
     O                                           55 'MANUAL PREMIUM AMOUNTS.'
      **** BEGIN DEL **** 02/10/99 ************************************
     O********************************   92 'RATES ARE BASED ON'
     O********************************  114 'DEFAULT RATE SET CODE'
     O******************************** +  1 'IN GROUP DETAIL.'
      **** END   DEL **** 02/10/99 ************************************
