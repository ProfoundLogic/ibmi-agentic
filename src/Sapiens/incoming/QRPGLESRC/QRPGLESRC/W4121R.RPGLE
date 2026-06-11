     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W4121R - EXPERIENCE BY PAYROLL CLASS REPORT
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/11/24 C:333 Project: WC2985                                *
     H*                Apply Deductible discount to Discounted Premium*
     H*                                                               *
     H* 06/17/24 C:344 Project: WC2952                                *
     H*                Increase rate fields to zone 8.3               *
     H*                                                               *
     H* 01/10/18 C:33  Support 63341                                  *
     H*                Clear agt# before WA002R call                  *
      *                                                               *
      * 04/07/17 C:062 Project 136WHF                                 *
      *                Add Territory Rating for CareWest.             *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/11/13 C:287 Project: SY266_QRY                             *
     H* 01/26/11 C:136 SUPPORT: WA093 75454                           *
     H*                ACCUMULATE '# OF EMPLOYEES' ACROSS ARD         *
     H*                POILCIES                                       *
     H*                                                               *
     H* 11/29/10  C:180  SUPPORT 74882
     H*                  Move correct desc to outfile
     F*
     H* 06/21/10 C:255 PROJECT: 114WVY                                *
     H*                ADD CHANGES IN PROGRAM FOR 2ND PERIOD DEVIATION*
     H*                WHEN DEVIATIONS ARE SPLIT. 2ND PERIOD DEVIATIO-*
     H*                NS CHANGES WILL APPLY FOR (M,J,E,S,D) PREMIUM  *
     H*                LEVELS AS IT IS FOR 1ST PERIOD DEVIATIONS.     *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 01/19/10 C:232 SUPPORT: WA071 71208                           *
     H*                NO DATA DISPLAYED IN THE REPORT IF RUN FOR THE *
     H*                ONE PAYROLL CALSS CODE.                        *
     H*                                                               *
     H* 05/07/09 C:189 SUPPORT: WA093 68317                           *
     H*                NO REPORT DISPLAYED EVEN IF MANY RECORDS       *
     H*                SHOULD HAVE BEEN POPULATED.                    *
     H*                                                               *
     H* 04/20/09 C:190 PROJECT: WC1473                                *
     H*                LOAD THE NEW CLASS CODE EXT. FIELD EXEXT       *
     H*                                                               *
     H* 05/23/08 C:136 PROJECT: 93WHJ                                 *
     H*                ACCUMULATE S/R PAYROLLS INTO WFEXP.EXSRPY      *
     H*                                                               *
     H* 10/09/07 C:136 SUPPORT: WA093 60489                           *
     H*                CORRECT LCM PULLED IN BY 93WGV                 *
     H*                                                               *
     H* 05/01/07 C:191 Project: 93WGV  SEQ# 56719                     *
     H*              - Load the # of employee flds from estimated or  *
     H*                audited fugures.                               *
     H*              - Load the LCM from payroll class/rate set level *
     H*                /Group level.                                  *
     H*                                                               *
     H* 03/17/06 C:132 Project: WC741 - SEQ# 49658                    *
     H*              - Load new Payroll Effective Date field EXPYED   *
     H*                in work file WFEXP.                            *
     H*              - Load Policy From Date in existing field EXPFD  *
     H*                in work file WFEXP.                            *
     H*                                                               *
     H* 04/07/05       PROJECT: 126WDK                                *
     H*          C:33  PROJECT: 126WCD                                *
     H*                MULTIPLE DEDUCTIBLE DISCOUNTS PER YEAR.        *
     H*                                                               *
     H* 01/14/05 C:33  Project: WC1252                                *
     H*                Load 'Anniversary Rating' fields.              *
     H*                                                               *
     H* 01/13/03 C:65  PROJECT: WC1104                                *
     H*                ALLOW THE DEVIATIONS TO BE APPLIED TO MANUAL.  *
     H*                                                               *
     H* 03/19/02 C:61  SUPPORT WC093 31663                            *
     H*                Ignore WMEPP records that are not active       *
     H*                status.                                        *
     H*                                                               *
     H* 02/13/02 C:90  SUPPORT: SEQ#31255                             *
     H*                Load proper premium amounts.                   *
     H*                                                               *
     H* 02/11/02 C:61  SUPPORT WA003 31101                            *
     H*                Do not include flat CX employers.              *
     H*                                                               *
     H* 01/08/02 C:90  SUPPORT: SEQ#29401                             *
     H*                The amount for earned premium was the same a   *
     H*                the amount for manual premium. Changed earned  *
     H*                premium to load the proper field.              *
     H*                                                               *
     H* 08/11/00 C:61  SUPPORT 23838                                  *
     H*                Produce output for all payroll class codes     *
     H*                with effective date less than report run date. *
     H*                (pgm was terminating after finding first       *
     H*                 class code with eff dt > run dt.              *
     H*                                                               *
     H* 06/28/00  C:61   SUPPORT: WA093 #23305                        *
     H*                  Use RUN DATE instead of current system date  *
     H*                  to calculate earned premium.                 *
     H*                                                               *
     H* 02/23/00  C:35   SUPPORT: WA093 #21935                        *
     H*                  PRINT SVCLAS ON HEADER INSTEAD OF XXCLAS.    *
     H*                                                               *
     H* 02/18/00  C:90   SUPPORT: WA096/21919                         *
     H*                  LOAD VALUE FOR KEY ON CHAIN TO LOAD RATING   *
     H*                  STATE WHEN WRITING TO FILE WFEXP.            *
     H*                                                               *
     H* 01/19/00  C:118  SUPPORT: WC102/21332                         *
     H*                  REMOVE 'TOTAL NORMAL PREMIUM' VALUES FROM    *
     H*                  FINAL TOTALS ON REPORT. HEADINGS SHOULD      *
     H*                  PRINT ON TOTAL PAGE. ALSO DO NOT PRINT LOSS  *
     H*                  AND FREQ RATIOS IN FINAL TOTALS.             *
     H*
     H* 06/28/99  C:35   PROJECT: SUPPORT WA093 #18524                *
     H*                  INDICATOR USAGE IN SUBROUTINE WRITE WAS      *
     H*                  CAUSING DISCREPANCIES BETWEEN PRINT-ONLY AND *
     H*                  PRINT/FILE.  ALSO ADDED CODE TO LOAD NEW     *
     H*                  FIELD EXSPEC (SPECIAL DESIGNATION).          *
     H*                                                               *
     H* 06/18/99  C:90  SUPPORT SEQ#: WA093/18524                     *
     H*                 PREMIUM AND PAYROLL FIELDS WITHIN THE WRITE   *
     H*                 SUBROUTINE WERE NOT BEING LOADED WITH THE     *
     H*                 TOTALED FIELDS THAT PRINT IN THE REPORT.      *
     H*                 REMOVED USAGE OF SUBROUTINE SUBCP.            *
     H*                                                               *
     H* 05/26/99  C:35   PROJECT: SUPPORT WA102 #18239                *
     H*                  EXPERIENCE MODIFIER TOTAL FIELDS WERE NOT    *
     H*                  BIG ENOUGH TO HOLD RESULTS.  ALSO, CORRECTED *
     H*                  EARNED PREMIUM SUBROUTINE TO USE ACCUMULATED *
     H*                  PREMIUM FIELD INSTEAD OF THE VALUE FROM THE  *
     H*                  LAST PAYROLL RECORD READ.                    *
     H*                                                               *
     H* 04/28/99 C:90   PROJECT: 96WBK                                *
     H*                 PRINT REPORT WHEN REPORT OUTPUT (QQRPT) = 'P' *
     H*                 OR 'B'. WRITE OUTPUT TO FILE WFEXP WHEN QQRPT *
     H*                 = 'F' OR 'B'.                                 *
     H*                                                               *
     H* 12/30/98 C:113  PROJECT: WC876                                *
     H*                 Changed to not use F1RTCD when reading WMPRP. *
     H*                                                               *
     H* 08/19/98  C:65  PROJECT: SUPPORT                              *
     H*                 ADDED SECTION WHICH MOVES EXP MOD FACTORS.    *
     H*                 ALSO CHANGED DATE FORMAT FOR SRPRT1.          *
     H*                                                               *
     H* 06/16/98  C:90  PROJECT: WC705                                *
     H*                 CHECK FOR INACTIVE INCR LIMIT CODES.          *
     H*                                                               *
     H*  04/29/98  C:78  PROJ:WC745                                   *
     H*                 F2CT01 REPLACED WITH WMFNP/FNCGFL. WDF2L      *
     H*                 DELETED CAUSE ITS NOT BEING USED ANYMORE.     *
     H*                                                               *
     H*  10/27/97  C:86  VERSION 3 PROJECT: WC577                     *
     H*            REVISED TO PRINT THE ALTERNATE FUND NAME IN PLACE  *
     H*            OF COMPANY NAME, IF ONE EXISTS.                    *
     H*****************************************************************
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWDRRL     IF   E           K DISK
     FWMCML5    IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMEPLE    IF   E           K DISK
     FWMEPL4E   IF   E           K DISK
     F                                     RENAME(PEP01:PEP02)
     FWMPXL1    IF   E           K DISK
     FWMPRL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWTLML1    IF   E           K DISK
     FWMREL     IF   E           K DISK
     FWFEXP     O    E             DISK
     F*
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)

     D*
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
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
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  PAYROLL CLASS FILE
     C*
     C     PRKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C     PRKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCLAS
     C     PRKEY3        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXDAT1
     C*  PAYROLL DESCRIPTION EXT FILE
     C     PXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXEXT
     C                   KFLD                    XXST
     C                   KFLD                    XXDAT1
      *
     C     PXKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXEXT
     C                   KFLD                    XXST
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR             3 0
     C*
     C*  STATISTICAL REPORTING/RATING FILE
     C*
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
     C*
     C     RRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C*
     C*  EMPLOYER PAYROLL FILE
     C*
     C     EPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C     EPKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCLAS            5 0
     C                   KFLD                    XXSPEC            1
     C                   KFLD                    XXEXT             5
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C     EPKEY3        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCLAS
     C*
     C     EPKEY4        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    ZZEFDT
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXSPEC
     C                   KFLD                    XXEXT
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
     C*   KEY LIST FOR THE CLAIMS FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
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
     C*  LIMITS FILE KEY
     C*
     C     LMKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY            10
     C                   KFLD                    XXEFDT
      *
      *  KEY LIST FOR REGION MASTER FILE
      *
     C     REKY          KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPFND
     C                   KFLD                    EMRGN#
     C*
     C                   MOVE      *BLANK        XXSVSP            1
     C                   MOVE      *BLANK        XXADD             1
     C                   Z-ADD     *ZEROS        XXSVCL            5 0
     C                   Z-ADD     *ZEROS        LRANPR           11 2
     C                   Z-ADD     *ZEROS        XXPRTD           11 2
     C                   Z-ADD     *ZEROS        XXRPC             6 4
     C                   Z-ADD     *ZEROS        XXANP            11 2
     C*
     C     *LIKE         DEFINE    EMDT          ZZEFDT
     C     *LIKE         DEFINE    EMDT          XXEFDT
     C     *LIKE         DEFINE    RSJRST        XXST
     C     *LIKE         DEFINE    ELRTCD        XXRTCD
     C     *LIKE         DEFINE    EMDT          XXYMD
     C     *LIKE         DEFINE    EMDT          XXWK60
     C     *LIKE         DEFINE    EMDT          XXDAT1
     C     *LIKE         DEFINE    EMDT          XHEFDT
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCLAS            5
     C                   PARM                    QQDATE            8
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    QQRPT             1
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
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR2            2 0
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCLAS        XXCLAS            5 0
     C                   MOVE      QQDATE        XXDATE            8 0
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         X1DATE            6 0
     C*
     C                   MOVE      QQMODE        XXMODE            1
     C*
     C     XXCLAS        IFNE      *ZERO
     C                   MOVE      '1'           *IN12
     C                   END
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
     C*
     C                   Z-ADD     XXYMD         XXEFDT
     C*
     C                   EXSR      SR1ST
     C*
     C*  GET FUND MASTER FOR FUND NAME
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL(P)  FNNAME        XFNAME           60
     C     FNANAM        IFNE      *BLANK
     C                   MOVEL(P)  FNANAM        XCNAME           60
     C                   MOVE      *ON           *IN39
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *BLANK        FNCGFL
     C                   ENDIF
     C*
     C*  CHAIN TO THE FUND DETAIL FILE
     C*
     C     F1KEY         CHAIN     PF101                              60
     C*
     C*  LOAD FLAG THAT DETERMINES WHETHER OR NOT THIS FUND WAS SET-UP
     C*  FOR NON-COMMON EXPIRATION DATE POLICIES
     C*
     C*  DO THE FOLLOWING CALCULATIONS ONLY IF THIS FUND IS NOT SET
     C*  UP FOR NON-COMMON EXPIRATION DATE POLICIES
     C*
     C     FNCGFL        IFNE      'Y'
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
     C                   Z-ADD     *ZERO         QQDAYS
     C                   MOVE      *BLANK        QQERR
     C                   MOVE      '1'           QQTERM
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
     C                   MOVEL     SRDTO         XQFROM            8 0
     C*
     C                   Z-ADD     XXDATE        XQTO              8 0
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
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C                   Z-ADD     QQDAYS        EDAYS             5 0
     C*
     C     FDAYS         IFNE      *ZEROS
     C     EDAYS         DIV       FDAYS         XXPROR            5 4
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXPROR
     C                   END
     C*
     C     XXYMD         IFGT      F1ENPO
     C                   Z-ADD     FDAYS         EDAYS
     C                   END
     C                   END                                                    XXFCT <> 'Y'
     C*
     C*  START READING THE PAYROLL CLASS FILE - FOR ALL OR ONE
     C*          PRKEY  --------- CO#, RTCD
     C*          PRKEY2 --------- CO#, RTCD, CLAS
     C*          PRKEY3 --------- CO#, RTCD, CLAS, EFDT
     C*
     C  N12PRKEY         CHAIN     PPR01                              60
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT1
      *
     C     PRKEY2        SETGT     PPR01
      *
     C     *IN60         DOUEQ     *ON
     C     PREFDT        ORLT      XXDAT1
     C     PRKEY         READE     PPR01                                  60
     C                   ENDDO
      *
     C  N12              MOVE      PRCLAS        XXCLAS
     C     PRKEY3        SETGT     PPR01
     C     PRKEY2        READPE    PPR01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C                   Z-ADD     PRCLAS        SVCLAS            5 0
     C                   Z-ADD     PRCLAS        XXCLAS
     C*
     C*  READ EMPLOYER PAYROLL RECORDS FOR CLASS FOUND IN PREVIOUS STEP
     C*          EPKEY3 ----------- CO#, FND, FYR, CLAS
     C*
     C     EPKEY3        CHAIN     PEP02                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      EPSPEC        XXSPEC
     C                   MOVEL     EPEXT         XXEXT
     C                   Z-ADD     EPEMP#        XXEMP#
     C                   Z-ADD     EPDIV         XXDIV
     C                   Z-ADD     EPEFDT        ZZEFDT
     C*
     C     *LIKE         DEFINE    ELANDT        X7ANDT
     C     *LIKE         DEFINE    ELANDT        X7AND1
     C     *LIKE         DEFINE    EXARAT        XXARAT
     C     *LIKE         DEFINE    EXANRT        XXANRT
     C*
     C                   CLEAR                   X7ANDT
     C                   CLEAR                   X7AND1
     C                   CLEAR                   XXANRT
     C                   CLEAR                   XXARAT
     C*
     C*  GET ALL CURRENTLY EFFECTIVE PAYROLL CLASS RECORDS FOR EMPLOYER
     C*  RUN DATE
     C*          EPKEY2 -----CO#, FND, FYR, CLAS, SPEC, EMP#, DIV
     C*
     C     EPKEY2        CHAIN     PEP02                              57
     C*
     C     *IN57         IFEQ      '0'
     C*
     C*   GET PREMIUM FOR PAYROLL CLASS
     C*
     C                   Z-ADD     0             XXEAPR           11 2
     C                   Z-ADD     0             XXEMPR           11 2
     C     *LIKE         DEFINE    EXEMPY        XXEMPY
     C     *LIKE         DEFINE    EXEMPY        XXAMPY
     C                   Z-ADD     0             XXEMPY
     C                   Z-ADD     0             XXAMPY
     C     *IN57         DOWEQ     '0'
      *
     C     EPSTAT        IFEQ      'A'
     C*
     C*  GET EMPLOYER DETAIL RECORD TO DETERMINE IF AUDITED
     C*
     C     ELKEY         CHAIN     PEL01                              51
     C     *IN51         IFEQ      '0'
     C*
     C*
     C*  FOR ACCOUNTS WITH NON-COMMON EXPIRATION DATES, WE SHOULD
     C*  CALCULATE THE PRO-RATA BASED ON ELPFDT-XXYMD/365
     C*
     C     FNCGFL        IFEQ      'Y'
     C*
     C*  GET NUMBER OF DAYS ACTIVE
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     XXDATE        XQTO
     C*
     C     ELPTDT        IFLT      XXYMD
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO
     C*
     C                   END
     C                   MOVE      XQFROM        QQFROM
     C                   MOVE      XQTO          QQTO
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C                   MOVE      '1'           QQTERM            1
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
     C                   END                                                    XXFCT = 'Y'
     C*
     C*
     C     ELFAF         IFNE      'A'
     C                   ADD       EPEMPR        XXEMPR
     C                   ADD       EPEAPR        XXEAPR
     C                   ADD       EPECNT        XXEMPY
     C                   ELSE
     C                   ADD       EPAMPR        XXEMPR
     C                   ADD       EPAAPR        XXEAPR
     C                   ADD       EPACNT        XXAMPY
     C                   END
     C*
     C                   END
     C*
     C*  SAVE MOST RECENT EFFECTIVE DATE
     C*
     C     EPEFDT        IFGT      XHEFDT
     C                   Z-ADD     EPEFDT        XHEFDT
     C                   END
     C*
     C     ELANDT        IFNE      *ZERO
     C                   Z-ADD     ELANDT        X7ANDT
     C     X7ANDT        ADD       10000         X7AND1                         Ann Rt Dt + 1yr
     C                   ENDIF
     C*
     C     ELANDT        IFNE      *ZERO
     C*
     C                   SELECT
     C     EPEFDT        WHENLT    X7AND1
     C                   Z-ADD     EPARAT        XXANRT
     C     EPEFDT        WHENGE    X7AND1
     C                   Z-ADD     EPARAT        XXARAT
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C                   SELECT
     C     EPEFDT        WHENLE    ELPFDT
     C     XXARAT        OREQ      *ZERO
     C                   Z-ADD     EPARAT        XXARAT
     C                   ENDSL
     C*
     C                   ENDIF
      *
     C                   ENDIF                                                  EPSTAT = 'A'
      *
     C     EPKEY2        READE     PEP02                                  57
     C                   END
     C*
     C*  GET EMPLOYER RECORD
     C*
     C                   MOVE      *BLANKS       ENAME
     C     EMKEY         CHAIN     PEM01                              51
     C     *IN51         IFEQ      '0'
     C*
     C*  GET EMPLOYER DETAIL RECORD & CALULATE MINIMUMS AND MAXIMUMS
     C*
     C     ELKEY         CHAIN     PEL01                              51
     C     *IN51         IFEQ      '0'
     C     ELPFDT        ANDNE     ELPTDT
      *
     C                   Z-ADD     XXEMPR        XEXMAN           11 2
     C     *IN51         IFEQ      '0'
     C                   EXSR      SBCPA
     C                   EXSR      SRMNMX
     C                   END
     C*
     C*  GET CLAIM FILE - CALC TOTAL INCURRED, TOTAL CLAIMS, LOSS RATIO
     C*                   AND FREQ RATIO. ONLY FOR THE CURRENT CLASS
     C*
     C                   Z-ADD     0             XXCLMS
     C                   Z-ADD     0             XXTOTI
     C     CMKEY         CHAIN     PCM01                              51
     C     *IN51         DOWEQ     '0'
     C*
     C     CMCLAS        IFEQ      XXCLAS
     C     CMSPEC        ANDEQ     XXSPEC
     C     CMEXT         ANDEQ     XXEXT
     C                   ADD       1             XXCLMS            5 0          COUNT CLAIMS
     C                   Z-ADD     *ZERO         TOTPMT            9 2
     C                   Z-ADD     *ZERO         TOTRES            9 2
     C                   XFOOT     CMA           TOTPMT
     C                   XFOOT     CMR           TOTRES
     C                   ADD       TOTPMT        XXTOTI           11 2
     C                   ADD       TOTRES        XXTOTI
     C                   END
     C*
     C     CMKEY         READE     PCM01                                  51
     C                   END
     C*
     C                   ADD       XXCLMS        L1CLMS            5 0
     C                   ADD       XXTOTI        L1TOTI           11 2
     C*
     C*  CATCULATE LOSS AND FREQUENCY RATIO
     C*
     C                   Z-ADD     XXCLMS        CLMS              5 0
     C                   Z-ADD     XXEMPR        PREM             11 2
     C                   Z-ADD     XXTOTI        TOTINC           11 2
     C                   EXSR      SUBLF
     C                   Z-ADD     LOSS          XXLOSS            7 2
     C                   Z-ADD     FREQ          XXFREQ            5 2
     C     XXEXT         IFNE      ' '
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  PXDESC        XXDESC
     C                   ENDIF
     C                   ELSE
     C                   MOVE      PRDESC        XXDESC           30
     C                   ENDIF                                                  EPEXT <> ' '
     C*
     C*  OUTPUT DETAIL LINE ITEM
     C*
     C     QQRPT         IFNE      'F'
     C   OF              EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   ENDIF                                                  QQRPT NE 'F'
      *
     C     QQRPT         IFEQ      'F'
     C     QQRPT         OREQ      'B'
     C                   EXSR      FILE
     C                   ENDIF
      *
     C     QQRPT         IFNE      'F'
     C                   EXCEPT    DETAIL
     C                   ENDIF                                                  QQRPT NE 'F'
      *
     C                   ENDIF                                                  PFDT <> PTDT
     C*
     C                   END
     C*
     C                   Z-ADD     0             XXEMD1
     C                   Z-ADD     0             XXEMD2
     C                   Z-ADD     0             XXEMD3
     C                   Z-ADD     0             XXEMPR
     C                   Z-ADD     0             XXCLMS
     C                   Z-ADD     0             XXTOTI
     C                   Z-ADD     0             XXLOSS
     C                   Z-ADD     0             XXFREQ
     C*
     C*  DETERMINE IF ANY MORE CLASSES FOR THIS EMPLOYER
     C*          EPKEY  ----------- CO#, FND, EMP#, DIV, FYR
     C*
     C                   MOVE      '0'           *IN52
     C     EPKEY         CHAIN     PEP01                              53
     C     EPKEY         READE     PEP01                                  53
     C     *IN53         IFEQ      '1'
     C                   MOVE      '1'           *IN52
     C                   END
     C     EPKEY4        CHAIN     PEP01                              80
     C     *IN80         IFEQ      *OFF
     C     *IN12         ANDEQ     *ON
     C                   ADD       XXANPR        LRANPR
     C                   Z-ADD     0             XXANPR
     C                   END
     C     EPKEY         READPE    PEP01                                  80
     C     *IN80         IFEQ      '1'
     C     *IN12         ANDEQ     *OFF
     C                   ADD       XXANPR        LRANPR
     C                   Z-ADD     0             XXANPR
     C                   END
     C*
     C*  OUTPUT SPECIAL LINE FOR EMPLOYER IF ADD'L CLASSES NOT FOUND
     C*
     C     *IN52         IFEQ      '1'
     C     QQRPT         IFNE      'F'
     C                   EXCEPT    SPEC
     C                   ENDIF                                                  QQRPT NE 'F'
     C                   END
     C                   END
     C*
     C*  GET NEXT EMPLOYER IN EMPLOYER PAYROLL FILE FOR THE CLASS
     C*          EPKEY3 ---------- CO#, FND, FYR, CLAS
     C*
     C                   MOVEA     '00'          *IN(54)                        SETOF 54-55
     C     EPKEY2        SETGT     PEP02
     C     EPKEY3        READE     PEP02                                  54
     C     *IN54         DOWEQ     '0'
     C*
     C     EPSPEC        IFNE      XXSPEC
     C     EPEXT         ORNE      XXEXT
     C                   EXSR      SUBL1
     C                   MOVEA     '11'          *IN(54)
     C                   ELSE
     C     EPEMP#        IFNE      XXEMP#
     C     EPDIV         ORNE      XXDIV
     C                   MOVEA     '11'          *IN(54)                        NEXT EMP#
     C                   ELSE
     C                   MOVE      '0'           *IN55                          SAME EMP#
     C                   END
     C                   END
     C*
     C     *IN54         IFEQ      '0'
     C     EPKEY3        READE     PEP02                                  54
     C                   END
     C                   END
     C*
     C     *IN55         IFEQ      '0'
     C                   MOVE      '1'           *IN60
     C                   END
     C*
     C                   END
     C*
     C*  SETGT LAST PRCLAS PROCESSED AND READ TO GET THE NEXT CLASS
     C*  IF ALL CLASSES SELECTED
     C*          PRKEY2 --------- CO#, RTCD, CLAS
     C*          PRKEY3 --------- CO#, RTCD, CLAS, EFDT
      *
     C     PRKEY2        SETGT     PPR01
      *
     C     *IN60         DOUEQ     *ON
     C     PREFDT        ORLT      XXDAT1
     C     PRKEY         READE     PPR01                                  60
     C                   ENDDO
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      PRCLAS        XXCLAS
     C     PRKEY3        SETGT     PPR01
     C     PRKEY2        READPE    PPR01                                  60
     C                   ENDIF
      *
     C     PRCO#         IFNE      XXCO#
     C     *IN12         OREQ      '1'
     C                   MOVE      '1'           *IN60
     C                   END
     C*
     C*  DETERMINE IF LEVEL BREAK PROCESSING SHOULD BE EXECUTED
     C*  * *  JOB CLASS TOTALS ARE PRINTED HERE IF CRITERIA MET  * *
     C*
     C     SVCLAS        IFNE      *ZERO
     C     PRCLAS        ANDNE     SVCLAS
     C     COUNT1        ANDNE     *ZERO
     C                   EXSR      SUBL1
     C                   END
     C*
     C                   END
     C**
     C                   MOVE      '1'           *INLR
     C                   MOVE      '1'           *IN78
     C**
     CLR                 EXSR      SUBL1
     CLR                 EXSR      SUBLR
     C*
      /Free
       xxprt = '0';
       Chain Orkey Por01;
       If %found and (QQRPT <> 'F');
          Except Route;
       Endif;
      /End-free
     C*==================================================================*
     C*   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
     C*==================================================================*
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
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
     C     QQRPT         IFNE      'F'
     C                   ENDIF                                                  QQRPT NE 'F'
     C                   END
     C*
     C                   Z-ADD     *HIVAL        MNEAPR           11 2
     C                   Z-ADD     *HIVAL        MNANPR           11 2
     C                   Z-ADD     *HIVAL        MNEMPR           11 2
     C                   Z-ADD     *HIVAL        MNEXM
     C*
     C                   Z-ADD     *HIVAL        MNEALR           11 2
     C                   Z-ADD     *HIVAL        MNANLR           11 2
     C                   Z-ADD     *HIVAL        MNEMLR           11 2
     C                   Z-ADD     *HIVAL        MNEXLR            5 2
     C*
     C                   Z-ADD     *ZERO         MXEALR           11 2
     C                   Z-ADD     *ZERO         MXANLR           11 2
     C                   Z-ADD     *ZERO         MXEMLR           11 2
     C                   Z-ADD     *ZERO         MXEXLR            5 2
     C*
     C                   Z-ADD     0             LRANPR           11 2
     C                   Z-ADD     0             LREAPR           13 2
     C                   Z-ADD     0             LREMPR           11 2
     C                   Z-ADD     0             LRCLMS            6 0
     C                   Z-ADD     0             LRTOTI           11 2
     C                   Z-ADD     0             LRLOSS            7 2
     C                   Z-ADD     0             LRFREQ            5 2
     C*
     C                   MOVE      '1'           *INOF
     C*
     C                   ENDSR
     C*
     C*==================================================================*
     C*   Minimum/Maximum Calculations   ** Subroutine **   S R M N M X  *
     C*==================================================================*
     C*
     CSR   SRMNMX        BEGSR
     C*
     C                   ADD       1             COUNT1            5 0
     C*
     C     XXEAPR        IFLT      MNEAPR
     C                   Z-ADD     XXEAPR        MNEAPR
     C                   END
     C*
     C     XXEAPR        IFGT      MXEAPR
     C                   Z-ADD     XXEAPR        MXEAPR
     C                   END
     C*
     C     XXEMPR        IFLT      MNEMPR
     C                   Z-ADD     XXEMPR        MNEMPR
     C                   END
     C*
     C     XXEMPR        IFGT      MXEMPR
     C                   Z-ADD     XXEMPR        MXEMPR
     C                   END
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFROM            8 0
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTO              8 0
     C*
     C                   Z-ADD     ELEMD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEMD1            6 0
     C*
     C                   Z-ADD     ELEMD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEMD2            6 0
     C*
     C                   Z-ADD     ELEMD3        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEMD3            6 0
     C*
     C*  EXPERIENCE MODS
     C*
     C                   Z-ADD     ELEXM1        X1EXM1            5 4
     C                   Z-ADD     ELEXM2        X1EXM2            5 4
     C                   Z-ADD     ELEXM3        X1EXM3            5 4
     C*
     C                   MOVEL     EMNAME        ENAME            30
     C*
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELANMP        XXANPR           11 2
     C                   ELSE
     C                   Z-ADD     ELENMP        XXANPR
     C                   END
     C*
     C     XXANPR        IFLT      MNANPR
     C                   Z-ADD     XXANPR        MNANPR
     C                   END
     C*
     C     XXANPR        IFGT      MXANPR
     C                   Z-ADD     XXANPR        MXANPR
     C                   END
     C*
     C                   ADD       XXANPR        L1ANPR           11 2
     C                   ADD       XXEAPR        L1EAPR           13 2
     C                   ADD       XXEMPR        L1EMPR           11 2
     C*
     C                   ADD       ELEXM1        L1EXM            11 4
     C                   ADD       ELEXM2        L1EXM
     C                   ADD       ELEXM3        L1EXM
     C*
     C     ELEXM1        IFNE      *ZERO
     C                   ADD       1             COUNTE            5 0
     C                   END
     C*
     C     ELEXM2        IFNE      *ZERO
     C                   ADD       1             COUNTE            5 0
     C                   END
     C*
     C     ELEXM3        IFNE      *ZERO
     C                   ADD       1             COUNTE            5 0
     C                   END
     C*
     C     ELEXM1        IFNE      *ZERO
     C     ELEXM1        ANDLT     MNEXM
     C                   Z-ADD     ELEXM1        MNEXM             5 4
     C                   END
     C     ELEXM2        IFNE      *ZERO
     C     ELEXM2        ANDLT     MNEXM
     C                   Z-ADD     ELEXM2        MNEXM
     C                   END
     C     ELEXM3        IFNE      *ZERO
     C     ELEXM3        ANDLT     MNEXM
     C                   Z-ADD     ELEXM3        MNEXM
     C                   END
     C*
     C     ELEXM1        IFGT      MXEXM
     C                   Z-ADD     ELEXM1        MXEXM             5 4
     C                   END
     C     ELEXM2        IFGT      MXEXM
     C                   Z-ADD     ELEXM2        MXEXM
     C                   END
     C     ELEXM3        IFGT      MXEXM
     C                   Z-ADD     ELEXM3        MXEXM
     C                   END
     C*
     C                   ENDSR
     C*
     C*==================================================================*
     C*   L1 Calculations (PR Class)       ** Subroutine **   S U B L 1  *
     C*==================================================================*
     C*
     CSR   SUBL1         BEGSR
     C*
     C                   Z-ADD     *ZERO         AVEAPR           11 2
     C                   Z-ADD     *ZERO         AVANPR           11 2
     C                   Z-ADD     *ZERO         AVEMPR           11 2
     C*
     C*  CALCULATE AVERAGES, MINIMUMS, MAXIMUMS
     C*
     C     COUNT1        IFNE      *ZERO
     C     L1ANPR        DIV(H)    COUNT1        AVANPR           11 2
     C     L1EAPR        DIV(H)    COUNT1        AVEAPR           11 2
     C     L1EMPR        DIV(H)    COUNT1        AVEMPR           11 2
     C                   ELSE
     C                   Z-ADD     *ZERO         AVANPR
     C                   Z-ADD     *ZERO         AVEAPR
     C                   Z-ADD     *ZERO         AVEMPR
     C                   END
     C**
     C     COUNTE        IFNE      *ZERO
     C     L1EXM         DIV(H)    COUNTE        AVEXM             5 4
     C                   ELSE
     C                   Z-ADD     *ZERO         AVEXM
     C                   END
     C**
     C     MNEAPR        IFEQ      *HIVAL
     C                   Z-ADD     *ZERO         MNEAPR
     C                   END
     C     MNANPR        IFEQ      *HIVAL
     C                   Z-ADD     *ZERO         MNANPR
     C                   END
     C     MNEMPR        IFEQ      *HIVAL
     C                   Z-ADD     *ZERO         MNEMPR
     C                   END
     C     MNEXM         IFEQ      *HIVAL
     C                   Z-ADD     *ZERO         MNEXM
     C                   END
     C*
     C                   ADD       L1EXM         LREXM            11 4
     C                   ADD       COUNT1        LRCT              5 0
     C                   ADD       COUNTE        LRCTE             5 0
     C*
     C     MNANPR        IFNE      *ZERO
     C     MNANPR        ANDLT     MNANLR
     C                   Z-ADD     MNANPR        MNANLR
     C                   END
     C*
     C     MXANPR        IFGT      MXANLR
     C                   Z-ADD     MXANPR        MXANLR
     C                   END
     C*
     C     MNEAPR        IFNE      *ZERO
     C     MNEAPR        ANDLT     MNEALR
     C                   Z-ADD     MNEAPR        MNEALR
     C                   END
     C*
     C     MXEAPR        IFGT      MXEALR
     C                   Z-ADD     MXEAPR        MXEALR
     C                   END
     C*
     C     MNEMPR        IFNE      *ZERO
     C     MNEMPR        ANDLT     MNEMLR
     C                   Z-ADD     MNEMPR        MNEMLR
     C                   END
     C*
     C     MXEMPR        IFGT      MXEMLR
     C                   Z-ADD     MXEMPR        MXEMLR
     C                   END
     C*
     C     MNEXM         IFNE      *ZERO
     C     MNEXM         ANDLT     MNEXLR
     C                   Z-ADD     MNEXM         MNEXLR
     C                   END
     C*
     C     MXEXM         IFGT      MXEXLR
     C                   Z-ADD     MXEXM         MXEXLR
     C                   END
     C*
     C*  GET LOSS & FREQUENCY RATIOS
     C*
     C                   Z-ADD     L1CLMS        CLMS
     C                   Z-ADD     L1EMPR        PREM
     C                   Z-ADD     L1TOTI        TOTINC           11 2
     C                   EXSR      SUBLF
     C                   Z-ADD     LOSS          L1LOSS            7 2
     C                   Z-ADD     FREQ          L1FREQ            5 2
     C*
     C                   ADD       L1EAPR        LREAPR
     C                   ADD       L1EMPR        LREMPR           11 2
     C                   ADD       L1CLMS        LRCLMS            6 0
     C                   ADD       L1TOTI        LRTOTI           11 2
     C*
     C*  OUTPUT TOTALS
     C*
     C     COUNT1        IFNE      *ZERO
     C     QQRPT         IFNE      'F'
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    TOT
     C                   ENDIF                                                  QQRPT NE 'F'
     C     *INLR         IFNE      '1'
     C                   MOVE      '1'           *INOF
     C                   END
     C                   END
     C*
     C                   Z-ADD     *ZERO         L1EXM
     C                   Z-ADD     *ZERO         COUNTE
     C                   Z-ADD     *ZERO         COUNT1
     C                   Z-ADD     0             L1ANPR
     C                   Z-ADD     0             L1EAPR
     C                   Z-ADD     0             L1EMPR
     C                   Z-ADD     0             L1CLMS
     C                   Z-ADD     0             L1TOTI
     C                   Z-ADD     0             L1LOSS
     C                   Z-ADD     0             L1FREQ
     C*
     C                   Z-ADD     *HIVAL        MNEAPR
     C                   Z-ADD     *HIVAL        MNANPR
     C                   Z-ADD     *HIVAL        MNEMPR
     C                   Z-ADD     *HIVAL        MNEXM
     C**
     C                   Z-ADD     *ZERO         MXEAPR           11 2
     C                   Z-ADD     *ZERO         MXANPR           11 2
     C                   Z-ADD     *ZERO         MXEMPR           11 2
     C                   Z-ADD     *ZERO         MXEXM
     C**
     C                   ENDSR
     C*
     C*==================================================================*
     C*   LR Calculations                  ** Subroutine **   S U B L R  *
     C*==================================================================*
     C*
     CSR   SUBLR         BEGSR
     C*
     C                   Z-ADD     *ZERO         AVEAPR           11 2
     C                   Z-ADD     *ZERO         AVANPR           11 2
     C                   Z-ADD     *ZERO         AVEMPR           11 2
     C*
     C*  CALCULATE AVERAGES, MINIMUMS, MAXIMUMS
     C*
     C     LRCT          IFNE      *ZERO
     C     LRANPR        DIV(H)    LRCT          AVANPR           11 2
     C     LREAPR        DIV(H)    LRCT          AVEAPR           11 2
     C     LREMPR        DIV(H)    LRCT          AVEMPR           11 2
     C                   ELSE
     C                   Z-ADD     *ZERO         AVANPR
     C                   Z-ADD     *ZERO         AVEAPR
     C                   Z-ADD     *ZERO         AVEMPR
     C                   END
     C**
     C     LRCTE         IFNE      *ZERO
     C     LREXM         DIV(H)    LRCTE         AVEXM             5 4
     C                   ELSE
     C                   Z-ADD     *ZERO         AVEXM
     C                   END
     C**
     C     MNEALR        IFEQ      *HIVAL
     C                   Z-ADD     *ZERO         MNEALR
     C                   END
     C     MNANLR        IFEQ      *HIVAL
     C                   Z-ADD     *ZERO         MNANLR
     C                   END
     C     MNEMLR        IFEQ      *HIVAL
     C                   Z-ADD     *ZERO         MNEMLR
     C                   END
     C     MNEXLR        IFEQ      *HIVAL
     C                   Z-ADD     *ZERO         MNEXLR
     C                   END
     C*
     C*  GET LOSS & FREQUENCY RATIOS
     C*
     C                   Z-ADD     LRCLMS        CLMS
     C                   Z-ADD     LREMPR        PREM
     C                   Z-ADD     LRTOTI        TOTINC
     C                   EXSR      SUBLF
     C                   Z-ADD     LOSS          LRLOSS            7 2
     C                   Z-ADD     FREQ          LRFREQ            5 2
     C*
     C*  OUTPUT TOTALS
     C*
     C     QQRPT         IFNE      'F'
     C   OF              EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   EXCEPT    TOTLR
     C                   ENDIF                                                  QQRPT NE 'F'
     C**
     C                   ENDSR
     C*
     C**
     C*==================================================================*
     C*   Loss and Frequency Ratio Calcs   ** Subroutine **   S U B L F  *
     C*==================================================================*
     C*
     CSR   SUBLF         BEGSR
     C*
     C*    CALCULATE LOSS RATIO
     C*
     C     PREM          IFNE      *ZERO
     C     TOTINC        DIV(H)    PREM          LOSS              7 2
     C                   ELSE
     C                   Z-ADD     *ZERO         LOSS
     C                   END
     C     LOSS          MULT      100           LOSS
     C*
     C*    CALCULATE FREQUENCY RATIO
     C*
     C     PREM          DIV(H)    1000          THPREM            9 2
     C*
     C     THPREM        IFNE      *ZERO
     C     CLMS          DIV(H)    THPREM        FREQ              5 2
     C                   ELSE
     C                   Z-ADD     *ZERO         FREQ
     C                   END
     C*
     C                   ENDSR
     C*
     C*==================================================================*
     C*   Calculate Earned Premium Amount  ** Subroutine **   S U B C P  *
     C*==================================================================*
     C*
     CSR   SUBCP         BEGSR
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C*
     C                   Z-ADD     ELPFDT        XXEFDT
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
     C*
     C*  GET NUMBER OF EMPLOYER POLICY DAYS
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO
     C*
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
     C                   Z-ADD     QQDAYS        PDAYS             5 0
     C*
     C*  GET NUMBER OF FUND DAYS
     C*
     C                   Z-ADD     F1STPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     F1ENPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO
     C*
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
     C                   Z-ADD     QQDAYS        FDAYS             5 0
     C*
     C                   Z-ADD     XXEMPR        XXMAN            11 2
     C                   Z-ADD     XXEMPR        XXSTAN           11 2
     C*
     C*  COMPUTE DEVIATION TAKEN FROM MANUAL
     C*
     C                   Z-ADD     *ZERO         XXDIS             9 0
     C     RRDV1L        IFEQ      'M'
     C     ELASP2        ANDNE     *ZERO
     C     XXMAN         MULT(H)   ELASP2        XXDIS
     C                   DIV(H)    100           XXDIS
     C                   SUB       XXDIS         XXMAN
     C                   END
     C*  COMPUTE DEVIATION 1 2ND PERIOD TAKEN FROM MANUAL
     C     RRDV1L        IFEQ      'M'
     C     ELD1S2        ANDNE     *ZERO
     C     XXMAN         MULT(H)   ELD1S2        XXDIS
     C                   DIV(H)    100           XXDIS
     C                   SUB       XXDIS         XXMAN
     C                   END
     C*  COMPUTE 2ND DEVIATION TAKEN FROM MANUAL
     C*
     C                   Z-ADD     *ZERO         XXDIS             9 0
     C     RRDV2L        IFEQ      'M'
     C                   Select
     C                   When      ELdev2F <> 0
     C     xxMan         Mult(H)   ELdev2F       xxMan
     C                   When      ELdev2  <> 0
     C     xxMan         Mult(H)   ELdev2        xxDis
     C                   Div(H)    100           xxDis
     C                   Sub       xxDis         xxMan
     C                   EndSl
     C                   END
     C*
     C*  COMPUTE 2ND DEVIATION 2ND PERIOD TAKEN FROM MANUAL
     C                   Z-ADD     *ZERO         XXDIS
     C     RRDV2L        IFEQ      'M'
     C     ELD2S2        ANDNE     *ZERO
     C     XXMAN         MULT(H)   ELD2S2        XXDIS
     C                   DIV(H)    100           XXDIS
     C                   SUB       XXDIS         XXMAN
     C                   END
     C                   Z-ADD     XXMAN         XXSTAN
     C*
     C*  CALCULATE INCREASED LIMIT AMOUNT
     C*
     C     ELILCD        IFNE      *BLANK
     C*
     C     ELILEF        IFEQ      *ZERO
     C                   Z-ADD     ELPFDT        ELILEF
     C                   END
     C*
     C                   Z-ADD     0             LMPCT
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     ELILCD        XXKEY
     C                   Z-ADD     ELPFDT        XXEFDT
     C     LMKEY1        SETGT     PLM01
     C                   READP     PLM01                                  60
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C                   READP     PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C     *IN60         IFNE      '0'
     C     XXCO#         ORNE      LMCO#
     C     XXFND         ORNE      LMFND
     C     XXST          ORNE      LMST
     C     XXKEY         ORNE      LMKEY
     C     XXEFDT        ORLT      LMEFDT
     C                   Z-ADD     0             LMPCT
     C                   END
     C*
     C     LMPCT         IFNE      *ZERO
     C*
     C                   Z-ADD     ELILEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C*
     C     ELILEX        IFNE      *ZERO
     C*
     C                   Z-ADD     ELILEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO
     C*
     C                   ELSE
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO
     C*
     C                   ENDIF
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
     C     FDAYS         IFNE      *ZERO
     C     QQDAYS        DIV       FDAYS         FACT              5 4
     C                   END
     C*
     C     XXMAN         MULT(H)   FACT          XXWRK            11 2
     C     XXWRK         MULT(H)   LMPCT         XXLIM             9 0
     C                   DIV(H)    100           XXLIM
     C*
     C                   END                                                    LMPCT <> 0
     C*
     C                   ADD       XXLIM         XXSTAN
     C*
     C                   END                                                    ELILCD *NE
     C*
     C*  COMPUTE DEDUCTIBLE CREDIT TAKEN FROM MANUAL
     C*
     C                   Z-ADD     *ZERO         XXDIS             9 0
     C     RRRDPR        IFEQ      'M'
     C     ELDDCT        ANDNE     *ZERO
     C     XXMAN         MULT(H)   ELDDCT        XXDIS
     C                   DIV(H)    100           XXDIS
     C                   SUB       XXDIS         XXSTAN
     C                   END
     C*
     C     RRRDPR        IFEQ      'M'
     C     ELDD2P        ANDNE     *ZERO
     C     XXMAN         MULT(H)   ELDD2P        XXDIS
     C                   DIV(H)    100           XXDIS
     C                   SUB       XXDIS         XXSTAN
     C                   END
     C*
     C*  COMPUTE MANAGED CREDIT
     C*
     C                   Z-ADD     *ZERO         XXDIS             9 0
     C     ELASPM        IFNE      *ZERO
     C     XXSTAN        MULT(H)   ELASPM        XXDIS
     C                   DIV(H)    100           XXDIS
     C                   SUB       XXDIS         XXSTAN
     C                   END
     C*
     C*  COMPUTE SAFETY CREDIT
     C*
     C                   Z-ADD     *ZERO         XXDIS             9 0
     C     ELASPS        IFNE      *ZERO
     C     XXSTAN        MULT(H)   ELASPS        XXDIS
     C                   DIV(H)    100           XXDIS
     C                   SUB       XXDIS         XXSTAN
     C                   END
     C*
     C*  COMPUTE DEVIATION TAKEN FROM SUBJECT
     C*
     C                   Z-ADD     *ZERO         XXDIS             9 0
     C     RRDV1L        IFEQ      'J'
     C     ELASP2        ANDNE     *ZERO
     C     XXSTAN        MULT(H)   ELASP2        XXDIS
     C                   DIV(H)    100           XXDIS
     C                   SUB       XXDIS         XXSTAN
     C                   END
     C*
     C*  COMPUTE DEVIATION 1 2ND PERIOD TAKEN FROM SUBJECT
     C     RRDV1L        IFEQ      'J'
     C     ELD1S2        ANDNE     *ZERO
     C     XXSTAN        MULT(H)   ELD1S2        XXDIS
     C                   DIV(H)    100           XXDIS
     C                   SUB       XXDIS         XXSTAN
     C                   END
     C*  COMPUTE 2ND DEVIATION TAKEN FROM MODIFIED
     C*
     C                   Z-ADD     *ZERO         XXDIS             9 0
     C     RRDV2L        IFEQ      'J'
     C                   Select
     C                   When      ELdev2F <> 0
     C     xxStan        Mult(H)   ELdev2F       xxStan
     C                   When      ELdev2  <> 0
     C     xxStan        Mult(H)   ELdev2        xxDis
     C                   Div(H)    100           xxDis
     C                   Sub       xxDis         xxStan
     C                   EndSl
     C                   END
     C*
     C*  COMPUTE 2ND DEVIATION 2ND PERIOD TAKEN FROM MODIFIED
     C                   Z-ADD     *ZERO         XXDIS
     C     RRDV2L        IFEQ      'J'
     C     ELD2S2        ANDNE     *ZERO
     C     XXSTAN        MULT(H)   ELD2S2        XXDIS
     C                   DIV(H)    100           XXDIS
     C                   SUB       XXDIS         XXSTAN
     C                   END
     C*  CALCULATIONS FOR EXPERIENCE MODIFIERS
     C*
     C                   Z-ADD     *ZERO         XXEXM1            5 4
     C                   Z-ADD     *ZERO         XXEXM2            5 4
     C                   Z-ADD     *ZERO         XXEXM3            5 4
     C                   Z-ADD     *ZERO         XXEXP1           11 2
     C                   Z-ADD     *ZERO         XXEXP2           11 2
     C                   Z-ADD     *ZERO         XXEXP3           11 2
     C*
     C                   Z-ADD     F1ENPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXTO              8 0
     C*
     C                   MOVE      *BLANKS       XHEFDT
     C*
     C*  EXPERIENCE MODS
     C*
     C                   Z-ADD     ELEXM1        X1EXM1            5 4
     C                   Z-ADD     ELEXM2        X1EXM2            5 4
     C                   Z-ADD     ELEXM3        X1EXM3            5 4
     C*
     C*  USE EXPERIENCE MODS BUCKET 1 IF:
     C*    1. NO EXP.MODS AMOUNTS FOUND
     C*    2. ONLY ONE EXPERIENCE MOD AMOUNT FOUND
     C*    3. LATEST PAYROLL EFFECTIVE DATE AND ENDING POLICY DATE
     C*       ARE BOTH LESS THAN MOD DATE 2
     C*
     C*
     C     ELEXM1        IFEQ      *ZERO
     C     ELEXM2        OREQ      *ZERO
     C                   ADD       ELEXM1        XXEXM1
     C                   ADD       XXSTAN        XXEXP1
     C                   GOTO      NEXTEM
     C                   END
     C*
     C     XHEFDT        IFLT      ELEMD2
     C     F1ENPO        ANDLT     ELEMD2
     C                   ADD       ELEXM1        XXEXM1
     C                   ADD       XXSTAN        XXEXP1
     C                   GOTO      NEXTEM
     C                   END
     C*
     C*  GET NUMBER OF DAYS IN FIRST MODIFIER PERIOD
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     ELEMD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO
     C*
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
     C     PDAYS         IFNE      *ZEROS
     C     QQDAYS        DIV       PDAYS         XXPCT             5 4
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXPCT
     C                   END
     C     ELEXM1        MULT(H)   XXPCT         XXWK11            5 2
     C                   ADD       XXWK11        XXEXM1
     C     XXSTAN        MULT(H)   XXPCT         XXEX11           11 2
     C                   ADD       XXEX11        XXEXP1
     C*
     C*  GET NUMBER OF DAYS IN SECOND MODIFIER PERIOD
     C*
     C                   Z-ADD     ELEMD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     ELEMD3        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO
     C*
     C     ELEMD3        IFEQ      *ZERO
     C     F1ENPO        ORLT      ELEMD3
     C                   Z-ADD     EXTO          XQTO
     C                   END
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
     C     PDAYS         IFNE      *ZEROS
     C     QQDAYS        DIV       PDAYS         XXPCT             5 4
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXPCT
     C                   END
     C     ELEXM2        MULT(H)   XXPCT         XXWK11            5 2
     C                   ADD       XXWK11        XXEXM2
     C     XXSTAN        MULT(H)   XXPCT         XXEX11           11 2
     C                   ADD       XXEX11        XXEXP2
     C*
     C*  GET NUMBER OF DAYS IN THIRD MODIFIER PERIOD
     C*
     C     ELEMD3        IFNE      *ZERO
     C*
     C                   Z-ADD     ELEMD3        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     EXTO          XQTO
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
     C     PDAYS         IFNE      *ZEROS
     C     QQDAYS        DIV       PDAYS         XXPCT             5 4
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXPCT
     C                   END
     C     ELEXM3        MULT(H)   XXPCT         XXWK11            5 2
     C                   ADD       XXWK11        XXEXM3
     C     XXSTAN        MULT(H)   XXPCT         XXEX11
     C                   ADD       XXEX11        XXEXP3
     C                   END
     C*
     C     NEXTEM        TAG
     C*
     C*  MAKE SURE WE DON'T EXCEED ORIGINAL AMOUNT
     C*
     C     XXEXM1        ADD       XXEXM2        XXWK52            5 2
     C                   ADD       XXEXM3        XXWK52            5 2
     C*
     C     XXEXP1        ADD       XXEXP2        XXWX11           11 2
     C                   ADD       XXEXP3        XXWX11           11 2
     C     XXWX11        IFNE      XXSTAN
     C     XXSTAN        SUB       XXWX11        XXDIF            11 2
     C                   ADD       XXDIF         XXEXP1
     C                   END
     C*
     C                   MULT(H)   XXWK52        XXSTAN
     C*
     C*  COMPUTE SCHEDULE DB/CR
     C*
     C     ELDBCR        IFNE      *ZERO
     C     XXSTAN        MULT(H)   ELDBCR        XXSTAN
     C                   ENDIF                                                  ELDBCR IFNE
     C*
     C*  COMPUTE DEVIATION TAKEN FROM MODIFIED
     C*
     C                   Z-ADD     *ZERO         XXDIS             9 0
     C     RRDV1L        IFEQ      'E'
     C     ELASP2        ANDNE     *ZERO
     C     XXSTAN        MULT(H)   ELASP2        XXDIS
     C                   DIV(H)    100           XXDIS
     C                   SUB       XXDIS         XXSTAN
     C                   END
     C*
     C*  COMPUTE DEVIATION TAKEN FROM MODIFIED
     C     RRDV1L        IFEQ      'E'
     C     ELD1S2        ANDNE     *ZERO
     C     XXSTAN        MULT(H)   ELD1S2        XXDIS
     C                   DIV(H)    100           XXDIS
     C                   SUB       XXDIS         XXSTAN
     C                   END
     C*  COMPUTE 2ND DEVIATION TAKEN FROM MODIFIED
     C*
     C                   Z-ADD     *ZERO         XXDIS             9 0
     C     RRDV2L        IFEQ      'E'
     C                   Select
     C                   When      ELdev2F <> 0
     C     xxStan        Mult(H)   ELdev2F       xxStan
     C                   When      ELdev2  <> 0
     C     xxStan        Mult(H)   ELdev2        xxDis
     C                   Div(H)    100           xxDis
     C                   Sub       xxDis         xxStan
     C                   EndSl
     C                   END
     C*  COMPUTE 2ND DEVIATION 2ND PERIOD TAKEN FROM MODIFIED
     C                   Z-ADD     *ZERO         XXDIS
     C     RRDV2L        IFEQ      'E'
     C     ELD2S2        ANDNE     *ZERO
     C     XXSTAN        MULT(H)   ELD2S2        XXDIS
     C                   DIV(H)    100           XXDIS
     C                   SUB       XXDIS         XXSTAN
     C                   END
     C*
     C                   Z-ADD     XXSTAN        SVSTAN           11 2
     C*
     C*  CALCULATE CONTRACTORS ADJUSTMENT CREDIT
     C*
     C                   Z-ADD     0             XXDIS             9 0
     C     ELASP1        IFNE      *ZERO
     C     XXSTAN        MULT(H)   ELASP1        XXDIS
     C                   DIV       100           XXDIS
     C                   END
     C                   SUB       XXDIS         XXSTAN
     C*
     C*  COMPUTE DEDUCTIBLE CREDIT TAKEN FROM STANDARD
     C*
     C                   Z-ADD     *ZERO         XXDIS             9 0
     C     RRRDPR        IFEQ      'S'
     C     ELDDCT        ANDNE     *ZERO
     C     SVSTAN        MULT(H)   ELDDCT        XXDIS
     C                   DIV(H)    100           XXDIS
     C                   SUB       XXDIS         XXSTAN
     C                   END
     C*
     C     RRRDPR        IFEQ      'S'
     C     ELDD2P        ANDNE     *ZERO
     C     SVSTAN        MULT(H)   ELDD2P        XXDIS
     C                   DIV(H)    100           XXDIS
     C                   SUB       XXDIS         XXSTAN
     C                   END
     C*
     C*  COMPUTE DEVIATION TAKEN FROM STANDARD
     C*
     C                   Z-ADD     *ZERO         XXDIS             9 0
     C     RRDV1L        IFEQ      'S'
     C     ELASP2        ANDNE     *ZERO
     C     XXSTAN        MULT(H)   ELASP2        XXDIS
     C                   DIV(H)    100           XXDIS
     C                   SUB       XXDIS         XXSTAN
     C                   END
     C*
     C*  COMPUTE DEVIATION 1 2ND PERIOD TAKEN FROM STANDARD
     C     RRDV1L        IFEQ      'S'
     C     ELD1S2        ANDNE     *ZERO
     C     XXSTAN        MULT(H)   ELD1S2        XXDIS
     C                   DIV(H)    100           XXDIS
     C                   SUB       XXDIS         XXSTAN
     C                   END
     C*  COMPUTE 2ND DEVIATION TAKEN FROM STANDARD
     C*
     C                   Z-ADD     *ZERO         XXDIS             9 0
     C     RRDV2L        IFEQ      'S'
     C                   Select
     C                   When      ELdev2F <> 0
     C     xxStan        Mult(H)   ELdev2F       xxStan
     C                   When      ELdev2  <> 0
     C     xxStan        Mult(H)   ELdev2        xxDis
     C                   Div(H)    100           xxDis
     C                   Sub       xxDis         xxStan
     C                   EndSl
     C                   END
     C*
     C*  COMPUTE 2ND DEVIATION 2ND PERIOD TAKEN FROM STANDARD
     C                   Z-ADD     *ZERO         XXDIS
     C     RRDV2L        IFEQ      'S'
     C     ELD2S2        ANDNE     *ZERO
     C     XXSTAN        MULT(H)   ELD2S2        XXDIS
     C                   DIV(H)    100           XXDIS
     C                   SUB       XXDIS         XXSTAN
     C                   END
     C*  COMPUTE DISCOUNT
     C*
     C                   Z-ADD     *ZERO         XXDIS             9 0
     C     ELASP3        IFNE      *ZERO
     C     XXSTAN        MULT(H)   ELASP3        XXDIS
     C                   DIV(H)    100           XXDIS
     C     XXSTAN        SUB       XXDIS         XXNORM           11 2
     C                   ENDIF                                                  ELASP3 IFNE
     C*
     C*  COMPUTE DEVIATION TO BE TAKEN FROM DISCOUNTED
     C*
     C                   Z-ADD     *ZERO         XXDIS             9 0
     C     RRDV1L        IFEQ      'D'
     C     ELASP2        ANDNE     *ZERO
     C     XXNORM        MULT(H)   ELASP2        XXDIS
     C                   DIV(H)    100           XXDIS
     C     XXNORM        SUB       XXDIS         XXNORM
     C                   END
     C*
     C*  COMPUTE DEVIATION 1 2ND PERIOD TO BE TAKEN FROM DISCOUNTED
     C     RRDV1L        IFEQ      'D'
     C     ELD1S2        ANDNE     *ZERO
     C     XXNORM        MULT(H)   ELD1S2        XXDIS
     C                   DIV(H)    100           XXDIS
     C     XXNORM        SUB       XXDIS         XXNORM
     C                   END
     C*  COMPUTE 2ND DEVIATION TO BE TAKEN FROM DISCOUNTED
     C*
     C                   Z-ADD     *ZERO         XXDIS             9 0
     C     RRDV2L        IFEQ      'D'
     C                   Select
     C                   When      ELdev2F <> 0
     C     xxNorm        Mult(H)   ELdev2F       xxNorm
     C                   When      ELdev2  <> 0
     C     xxNorm        Mult(H)   ELdev2        xxDis
     C                   Div(H)    100           xxDis
     C                   Sub       xxDis         xxNorm
     C                   EndSl
     C                   END
     C*
     C*  COMPUTE 2ND DEVIATION 2ND PERIOD TO BE TAKEN FROM DISCOUNTED
     C                   Z-ADD     *ZERO         XXDIS
     C     RRDV2L        IFEQ      'D'
     C     ELD2S2        ANDNE     *ZERO
     C     XXNORM        MULT(H)   ELD2S2        XXDIS
     C                   DIV(H)    100           XXDIS
     C     XXNORM        SUB       XXDIS         XXNORM
     C                   END
      *** Beg Add ***** 11/11/24 *******************************
     C                   Z-ADD     XXNORM        SVNORM           11 2
     C     RRRDPR        IFEQ      'D'
     C     ELDDCT        ANDNE     *ZERO
     C     SVNORM        MULT(H)   ELDDCT        XXDIS
     C                   DIV(H)    100           XXDIS
     C                   SUB       XXDIS         XXNORM
     C                   END
     C*
     C     RRRDPR        IFEQ      'D'
     C     ELDD2P        ANDNE     *ZERO
     C     SVNORM        MULT(H)   ELDD2P        XXDIS
     C                   DIV(H)    100           XXDIS
     C                   SUB       XXDIS         XXNORM
     C                   END
      *** End Add ***** 11/11/24 *******************************
     C*  PRORATE IF NOT AUDITED AMOUNT
     C*
     C                   Z-ADD     *ZERO         XXWK12           11 2
     C     ELFAF         IFNE      'A'
     C     XXNORM        MULT(H)   XXPROR        XXWK12
     C                   ELSE
     C                   Z-ADD     XXNORM        XXWK12
     C                   END
     C*
     C                   Z-ADD     XXWK12        XXEMPR
     C*
     C*
     C                   ENDSR
     C**
     C*==================================================================*
     C*   Calculate Earned Premium Amount  ** Subroutine **   S B C P A  *
     C*==================================================================*
     CSR   SBCPA         BEGSR
     C*
     C     ELFAF         IFEQ      'A'
     C     ELAMPR        IFNE      *ZEROS
     C     XXEMPR        DIV       ELAMPR        XXRPC
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXRPC
     C                   ENDIF                                                  ELAMPR=0
     C                   ELSE
     C     ELMAPR        IFNE      *ZEROS
     C     XXEMPR        DIV       ELMAPR        XXRPC
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXRPC
     C                   ENDIF                                                  ELMAPR=0
     C                   ENDIF                                                  ELFAF=A
     C*
     C*
     C*  CALCULATE EARNED PREMIUM TO DATE
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     XXCO#         SRCO#             3
     C                   MOVEL     XXFND         SRFND             3
     C                   MOVEL     ELFYR         SRFYR             3
     C                   MOVEL     ELEMP#        SREMP#            9
     C                   MOVEL     ELDIV         SRDIV             5
     C                   MOVE      QQDATE        QQDTE             8
     C                   MOVE      *BLANK        SRAMT            11
     C                   MOVE      *BLANK        SRAMT2           11
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRPRT1'
     C                   PARM                    SRCO#
     C                   PARM                    SRFND
     C                   PARM                    SRFYR
     C                   PARM                    SREMP#
     C                   PARM                    SRDIV
     C                   PARM                    QQDTE
     C                   PARM                    SRAMT
     C                   PARM                    SRAMT2
     C                   PARM                    QQTERM
     C                   MOVE      SRAMT         XXPRTD
     C*
     C     XXPRTD        MULT      XXRPC         XXEMPR
     C                   ENDSR                                                  END SBCPA
     C*================================================================
     C* FILE - SUBROUTINE TO WRITE TO FILE WFEXP
     C*================================================================
     CSR   FILE          BEGSR
      *
     C                   CLEAR                   PEX01
      *
     C                   Z-ADD     EPCO#         EXCO#
     C                   MOVEL     CONAME        EXCNAM
     C                   Z-ADD     EPFND         EXGRP
     C                   MOVEL     FNNAME        EXGNAM
     C                   Z-ADD     EPFYR         EXFYR
     C                   Z-ADD     XXDATE        EXASOF
     C                   Z-ADD     EPCLAS        EXCLAS
     C                   MOVE      EPSPEC        EXSPEC
     C                   MOVEL     EPEXT         EXEXT
     C                   MOVEL(P)  XXDESC        EXCDES
     C                   Z-ADD     XXARAT        EXARAT
     C*
     C                   Z-ADD     XXANRT        EXANRT
     C                   Z-ADD     X7ANDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXANDT
     C*
     C                   Z-ADD     EPEMP#        EXEMP#
     C                   Z-ADD     EPDIV         EXDIV
     C                   MOVEL     EMNAME        EXENAM
      *
     C                   Z-ADD     XXEAPR        EXPYRL
     C                   Z-ADD     XEXMAN        EXMAN
     C                   Z-ADD     XXANPR        EXNORM
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELAAPR        EXWRIT
     C                   ELSE
     C                   Z-ADD     ELANPR        EXWRIT
     C                   ENDIF
      *
     C                   Z-ADD     ELEXM1        EXEXM1
     C                   Z-ADD     ELEMD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXEXD1
      *
     C                   Z-ADD     ELEXM2        EXEXM2
     C                   Z-ADD     ELEMD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXEXD2
      *
     C                   Z-ADD     ELEXM3        EXEXM3
     C                   Z-ADD     ELEMD3        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXEXD3
      *
      * Load Policy From Date and Payroll Effective Date
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXPFD
      *
     C                   Z-ADD     EPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXPYED
      *
     C                   Z-ADD     XXEMPR        EXEARN
     C                   Z-ADD     XXCLMS        EX#CLM
     C                   Z-ADD     XXTOTI        EXTOTI
     C                   Z-ADD     XXLOSS        EXLOSS
     C                   Z-ADD     XXFREQ        EXFREQ
     C                   Z-ADD     EMRGN#        EXRGN#
      *
     C     REKY          CHAIN     PRE01                              42
     C     *IN42         IFEQ      *OFF
     C                   MOVEL     REDESC        EXRDES
     C                   ENDIF
      *
     C                   CALL      'WA002R'
     C                   PARM      EPCO#         @@CO#             3 0
     C                   PARM      EPFND         @@FND             3 0
     C                   PARM      EPEMP#        @@EMP#            9 0
     C                   PARM      EPDIV         @@DIV             5 0
     C                   PARM      EPFYR         @@FYR             3 0
     C***   Begin Chg  ***  01/10/18  *************************************
     C***********        PARM                    @@AGT#            5 0
     C***********        PARM                    @@ANAM           60
     C                   PARM      *zeros        @@AGT#            5 0
     C                   PARM      *blanks       @@ANAM           60
     C***   End   Chg  ***  01/10/18  *************************************
     C                   PARM                    @@PROD            5 0
     C                   PARM                    @@PNAM           60
     C                   PARM                    @@AREA            3 0
     C                   PARM                    @@PHON            7 0
     C                   Z-ADD     @@AGT#        EXAGT#
     C                   MOVEL     @@ANAM        EXANAM
      *
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              42
     C     *IN42         IFEQ      *OFF
     C                   MOVEL     RSJRST        EXRST
     C                   ELSE
     C                   MOVE      *BLANKS       EXRST
     C                   ENDIF                                                  *IN42 IFEQ *OFF
      * Populate EXEMPY with EPACNT if ELFAF is equal to 'A',
      * else, populate it with EPECNT.
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     XXAMPY        EXEMPY
     C                   ELSE
     C                   Z-ADD     XXEMPY        EXEMPY
     C                   ENDIF                                                  ELFAF = 'A'
      *
      * Call SRLCM to to retrieve LCM value
      *
      *
     C                   MOVE      EPCO#         @ACO#
     C                   MOVE(P)   EPFND         @AFND
     C                   MOVE(P)   ELRTCD        @ARTCD
     C                   MOVE(P)   EPCLAS        @ACLAS
     C                   MOVE(P)   EPFYR         @AFYR             3
     C                   MOVE(P)   EPEMP#        @AEMP#            9
     C                   MOVE(P)   EPDIV         @ADIV             5
     C                   MOVE(P)   EPEFDT        @AEFDT            7
     C                   CALL      'SREFFD'
     C                   PARM                    @ACO#
     C                   PARM                    @AFND
     C                   PARM                    @AFYR
     C                   PARM                    @AEMP#
     C                   PARM                    @ADIV
     C                   PARM                    @AEFDT
     C                   PARM      'P'           @ATYPE            1
      *
     C                   CALL      'SRLCM'
     C                   PARM                    @ACO#             3
     C                   PARM                    @AFND             3
     C                   PARM                    @ARTCD            3
     C                   PARM                    @ACLAS            5
     C                   PARM                    @AEFDT            7
     C                   PARM      *BLANKS       @ALCM             5
      *
     C     @ALCM         IFNE      *BLANKS
     C                   MOVE      @ALCM         EXLCM
     C                   ENDIF
      *
     C                   EXSR      SRPY
      *
     C                   WRITE     PEX01
      *
     C                   ENDSR                                                  FILE
      *================================================================
      * COLLECT SELF REPORTED PAYROLLS FOR EACH CLASS BEING WRITTEN
      *================================================================
     C     SRPY          BEGSR
      *
     C/EXEC SQL
     C+ SELECT SUM(CASE
     C+              WHEN SRRPMY <> 0 AND
     C+                   INTEGER(CHAR(SRRPMY CONCAT '31')) <= :XXYMD OR
     C+                   SRTODT <> 0 AND SRTODT <=  :XXYMD
     C+              THEN SRAMTP
     C+            END)
     C+   INTO :EXSRPY
     C+   FROM WASRP
     C+   WHERE SRCO#  = :EPCO#  AND
     C+         SRFND  = :EPFND  AND
     C+         SREMP# = :EPEMP# AND
     C+         SRDIV  = :EPDIV  AND
     C+         SRFYR  = :EPFYR  AND
     C+         SRCLAS = :EPCLAS AND
     C+         SRSPEC = :EPSPEC
     C/END-EXEC
      *
     C                   ENDSR                                                  SRPY
     C*
     OQSYSPRT   E            ROUTE          1 06
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
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
     O                       *YEAR             +  1
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O              N39      CONAME             146
     O               39      XCNAME             146
     O                       DSDATE        Y    198
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL             127
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
     O          E            HEAD           2
     O                                           86 'YEAR'
     O                       XXFYR         Z     90
     O                                           96 'AS OF'
     O                       X1DATE        Y    105
     O          E            HEAD           2
     O                                            6 'GROUP:'
     O                       XXFND         Z     10
     O                       XFNAME              71
     O          E   N78      HEAD           2
     O                                           14 'PAYROLL CLASS:'
     O                       SVCLAS        Z     20
     O                       XXSPEC              21
     O                       XXEXT               28
     O                       XXDESC              63
     O          E            HEAD           1
     O              N78                          52 'TOTAL'
     O                                           89 'EXP'
     O              N78                         109 'EXP'
     O              N78                         129 'EXP'
     O                                          163 'EARNED PREM   TOTAL'
     O              N78                         197 'LOSS     FREQ'
     O          E            HEAD           2
     O              N78                             'EMPLOYER    DIV  NAME'
     O              N78                          61 'NORMAL PREMIUM'
     O                                           89 'PAYROLL AMT       MOD'
     O              N78                         109 'EFF DATE   MOD'
     O              N78                         121 'EFF DATE'
     O              N78                         141 'MOD    EFF DATE'
     O                                          164 'FOR CLASS   CLAIMS'
     O                                          180 'INCURRED LOSS'
     O              N78                         198 'RATIO    RATIO'
     O          E            DETAIL         1
     O                       EPEMP#        Z     09
     O                       EPDIV         Z     15
     O                       ENAME               45
     O                       XXANPR        J     61
     O                       XXEAPR        J     79
     O                       X1EXM1        4     89
     O                       XXEMD1             103 '  /  /  '
     O                       X1EXM2        4    111
     O                       XXEMD2             121 '  /  /  '
     O                       X1EXM3        4    129
     O                       XXEMD3             141 '  /  /  '
     O                       XXEMPR        J    156
     O                       XXCLMS        Z    164
     O                       XXTOTI        J    180
     O                       XXLOSS        J    189
     O                       XXFREQ        J    198
     O          E    52      SPEC           1
     O                                           32 'NO OTHER CLASSES EXIST'
     O                                           45 'FOR EMPLOYER'
     O          E            TOT         1  1
     O                                           45 'JOB CLASS TOTAL:'
     O                       L1ANPR        JB    61
     O                       L1EAPR        JB    79
     O                       L1EMPR        JB   156
     O                       L1CLMS        Z    164
     O                       L1TOTI        JB   180
     O                       L1LOSS        J    189
     O                       L1FREQ        J    198
     O          E            TOT            1
     O                                           42 'AVG  '
     O                       AVANPR        JB    61
     O                       AVEAPR        JB    79
     O                       AVEXM         3B    89
     O                       AVEMPR        JB   156
     O          E            TOT            1
     O                                           42 'MIN  '
     O                       MNANPR        JB    61
     O                       MNEAPR        JB    79
     O                       MNEXM         3B    89
     O                       MNEMPR        JB   156
     O          E            TOT            1
     O                                           42 'MAX  '
     O                       MXANPR        JB    61
     O                       MXEAPR        JB    79
     O                       MXEXM         3B    89
     O                       MXEMPR        JB   156
     O          E            TOT            2
     O                                           44 'COUNT'
     O                       COUNT1        ZB    60
     O          E            TOTLR          1
     O                                           45 '    FINAL TOTALS:'
     O                       LREAPR        JB    79
     O                       LREMPR        JB   156
     O                       LRCLMS        Z    164
     O                       LRTOTI        JB   180
     O          E            TOTLR          1
     O                                           42 'AVG  '
     O                       AVEAPR        JB    79
     O                       AVEXM         3B    89
     O                       AVEMPR        JB   156
     O          E            TOTLR          1
     O                                           42 'MIN  '
     O                       MNEALR        JB    79
     O                       MNEXLR        3B    89
     O                       MNEMLR        JB   156
     O          E            TOTLR          1
     O                                           42 'MAX  '
     O                       MXEALR        JB    79
     O                       MXEXLR        3B    89
     O                       MXEMLR        JB   156
     O**
