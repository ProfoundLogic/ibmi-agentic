     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W8400R1 - BUILD ERM-6 WORKFILE RECORDS
     H***************************************************************
     H** PROGRAM DESCRIPTION:                                      **
     H**                                                           **
     H**   This program generates workfiles for submitting the     **
     H**   ERM-6 State Reprots.                                    **
     H**                                                           **
     H**   CALLED FROM:                                            **
     H**         W8400C1                                           **
     H**                                                           **
     H**   CALLS TO:                                               **
     H**         NONE                                              **
     H**                                                           **
     H**   CREATED:  10/26/95   C:30   PROJECT: WC460              **
     H**                                                           **
     H***************************************************************
     H* MODIFICATIONS:                                              *
     H*                                                               *
     H* 06/21/10 C:255 PROJECT: 114WVY                                *
     H*                SUBSTRACT AUDITED AND ESTIMATED AMOUNT OF 2ND  *
     H*                PERIOD DEVIATIONS FROM XXPREM TO CALCULATE,PRI-*
     H*                NT OR DISPLAY PREMIUM WHEN DEVIATIONS ARE SPLIT*
     H*                                                               *
     H* 01/13/03 C:65  PROJECT: WC1104                                *
     H*                ALLOW THE DEVIATIONS TO BE APPLIED TO MANUAL.  *
     H*                                                             *
     H* 10/09/00  C:35   PROJECT: SUPPORT WA088 #24387              *
     H*                  USE POLICY DATES FROM STATE REPORTING      *
     H*                  DIVISION.                                  *
     H*                                                             *
     H* 07/16/99  C:35   PROJECT: SUPPORT WA090 #18838              *
     H*                  RATING EFFECTIVE DATE WAS BEING            *
     H*                  OVERWRITTEN BY POLICY EFFECTIVE DATE.       *
     H*                                                             *
     H* 02/15/99  C:87  PROJECT: SUPPORT WA071 #15766 & #16270      *
     H*                 For NON EXCESS claims, the pgm was accessing*
     H*                 EXCESS records in workfile 4 by not looking *
     H*                 for accident date = 0.  Added code to make  *
     H*                 sure record being updated has acc dt = 0.   *
     H*                                                             *
     H* 09/17/98  C:35  PROJECT: SUPPORT WA090 #13015               *
     H*                 ADDED FIELDS FOR MANUAL RATE, PREMIUM       *
     H*                 AMOUNT AND EXPOSURE COVERAGE CODE.          *
     H*                                                             *
     H* 05/15/98  C:35  PROJECT: SUPPORT WA077 #11388               *
     H*                 PROGRAM WAS CUTTING OFF CENTS INSTEAD OF    *
     H*                 ROUNDING TO GET WHOLE DOLLAR AMOUNT.        *
     H*                                                             *
     H* 05/04/98  C:35  PROJECT: SUPPORT WA077 #11388               *
     H*                 ACCUMULATE PREMIUM ACROSS DIVISIONS.        *
     H*                                                             *
     H***************************************************************
     H** Indicators:                                               **
     H**                                                           **
     H**  44 *ON  - Employer # PARM greater than zero              **
     H**  50      - General File processing                        **
     H**  60      - General File processing                        **
     H**  61      - General File processing                        **
     H**  65      - WXW4L File processing                          **
     H**  78 *OFF - Anniversary Rating Date / Rate Split           **
     H**                                                           **
     H***************************************************************
     H** Files:                                                    **
     H**                                                           **
     H**  WTABL  - CO#/XXKEY                                       **
     H**  WDF1L  - CO#/FND/FYR                                     **
     H**  WDELL1 - CO#/FND/FYR/EMP#/DIV                            **
     H**  WMEML  - CO#/FND/EMP#/DIV                                **
     H**  WMEPL1 - CO#/FND/FYR/EMP#/DIV/CLASS                      **
     H**  WMCML5 - CO#/FND/FYR/EMP#/DIV                            **
     H**  WXW1P  - none                                            **
     H**  WXW2P  - none                                            **
     H**  WXW3L  - CO#/FND/EMP#/FYR/Period/CLASS                   **
     H**  WXW4L  - CO#/FND/EMP#/FYR/Period/CLASS/NCCI/STATUS       **
     H**                                                           **
     H***************************************************************
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     FWDF1L     IF   E           K DISK
     FWDELL1    IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWDRRL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMEPL1    IF   E           K DISK
     FWMCML5    IF   E           K DISK
      **** BEGIN CHG **** 05/04/98 *********************************************
     F********** WXW1P   O   E           K        DISK
     FWXW1L     UF A E           K DISK
      **** END   CHG **** 05/04/98 *********************************************
     FWXW2P     O    E           K DISK
     FWXW3L     UF A E           K DISK
     FWXW4L     UF A E           K DISK

     D*
     D*  THE FOLLOWING ARRAYS ARE FOR TO SPECIFY RATE CHANGES AND
     D*  PAYROLL EFFECTIVE DATES
     D*  PDAT - DATES IN WHICH PAYROLLS TAKE ON DIFFERENT RATING
     D*         THERE CAN BE UP TO 3 DIFF RATING PERIODS
     D*  RDAT - CORRESPONDING RATES IN EFFECT FOR PAYROLLS AS OF PDAT
     D*
     D PDAT            S              7  0 DIM(10) ASCEND                       PAYROLL DATES
     D RDAT            S              7  0 DIM(10) ASCEND                       RATING DATES
     D SMD             S              3  2 DIM(10)                              Split Modifier
     D SMDT            S              7  0 DIM(10)                              Split Mod Date
     D*
     D                 DS
     D  XXANDT                 1      7  0
     D  XXANYR                 2      3  0
     D*  System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*   Claim Reserve Type Array
     D                 DS
     D  CMT                    1     20
     D                                     DIM(10)                              OUTSTAND RSV TYPE
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D*   Claim Payments Array
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
     D*   Claim Remaining Reserve
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
     D*  Fund Year Parms assigned to array
     D                 DS
     D  YR                     1      9
     D                                     DIM(3)                               PARM Fund Years
     D  XXFYR1                 1      3  0
     D  XXFYR2                 4      6  0
     D  XXFYR3                 7      9  0
     D*  Experience Modifier fields assigned to array
     D                 DS
     D  XMD                    1     15  4
     D                                     DIM(3)                               Exp Modifier
     D  ELEXM1                 1      5  4
     D  ELEXM2                 6     10  4
     D  ELEXM3                11     15  4
     D*  Exp Mod Effective Date fields assigned to array
     D                 DS
     D  XDT                    1    021  0
     D                                     DIM(3)                               Exp Mod Eff. DT
     D  ELEMD1               001    007  0
     D  ELEMD2               008    014  0
     D  ELEMD3               015    021  0
      *
      **** BEGIN ADD **** 05/15/98 ***********************************
     D                 DS
     D  XROUND                 1      9  2
     D  XWHOLE                 1      9  0
     D  XCENTS                 8      9  0
      **** END   ADD **** 05/15/98 ***********************************

     C****************************************************************
     C*                    K E Y     L I S T S
     C****************************************************************
     C*
     C*  Table file key
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*   Key List for the Fund Detail file
     C*
     C     F1KY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*   Key List for the Employer Detail file
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
      **** BEGIN ADD **** 05/04/98 *********************************************
     C                   KFLD                    XXEMP#
      **** END   ADD **** 05/04/98 *********************************************
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*   Key List for the Employer Master  file
     C*
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     EMKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*   Key List for the Employer Payroll file
     C*
     C     EPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C*
     C*   Key List for the Claim Master file
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
      **** BEGIN ADD **** 05/04/98 ********************************
      *
      *   Key List for Work File # 1
      *
     C     W1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
      **** END   ADD **** 05/04/98 ********************************
     C*
     C*   Key List for Work File # 3
     C*
     C     W3KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXPER#
     C                   KFLD                    XXCLAS
     C*
     C*   Key List for Work File # 4
     C*
     C     W4KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXPER#
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXNCCI
     C                   KFLD                    XXSTAT
     C*
     C*   RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C*
     C*   STAT RPT/RATING FILE
     C*
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXDT
     C*
     C     RRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C*
     C     *LIKE         DEFINE    RTKEY         XXKEY
     C     *LIKE         DEFINE    ELPTDT        XXPDAT
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    W4PER#        XXPER#
     C     *LIKE         DEFINE    W4CLAS        XXCLAS
     C     *LIKE         DEFINE    W4NCCI        XXNCCI
     C     *LIKE         DEFINE    W4STAT        XXSTAT
     C     *LIKE         DEFINE    ELPTDT        XXEFDT
     C     *LIKE         DEFINE    ELPTDT        XXDATE
     C     *LIKE         DEFINE    ELPFDT        XXDT
     C     *LIKE         DEFINE    ELRTCD        XXRTCD
     C     *LIKE         DEFINE    RSJRST        XXST
     C*
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR1            3
     C                   PARM                    QQFYR2            3
     C                   PARM                    QQFYR3            3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQEFDT            8
     C                   PARM                    QQAMT1            9
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
     C*================================================================
     C*
     C                   MOVE      QQFYR1        XXFYR1
     C                   MOVE      QQFYR2        XXFYR2
     C                   MOVE      QQFYR3        XXFYR3
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQAMT1        XXEXC             9 0
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   MOVE      QQEFDT        X1EFDT            8 0
     C*
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   MOVE      *OFF          *IN44
     C     QQEMP#        IFGT      *ZERO
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      *ON           *IN44
     C                   ENDIF
     C*
     C*   Read Employer Master file based on QQEMP# PARM
     C*
     C   44EMKY2         SETLL     PEM01
     C  N44EMKY1         SETLL     PEM01
     C*
     C                   EXSR      SRSTRP
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C*   "Z" variables used to determine Period Number while processing
     C*   multiple fund years.
     C*
     C                   Z-ADD     1             Z                 3 0
     C                   Z-ADD     *ZERO         ZMIN              3 0
     C                   MOVE      EMEMP#        XXEMP#
     C                   MOVEA     *HIVAL        PDAT
     C                   Z-ADD     0             RDAT
     C                   Z-ADD     *ZERO         SMD
     C                   Z-ADD     *ZERO         SMDT
     C*
     C*   Process Fund Year array
     C*
     C                   Z-ADD     1             I                 3 0
     C                   DO        3             I
     C     YR(I)         IFNE      *BLANK
     C     YR(I)         ANDNE     *ZERO
     C                   MOVE      YR(I)         XXFYR
     C*
     C*  Get the Fund Detail file
     C*
     C     F1KY1         CHAIN     PF101                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*  Initialize Arrays based on Employer Detail Fields
     C*
     C                   MOVEA     *ZERO         XMD
     C                   MOVEA     *ZERO         XDT
     C*
     C*  Read Employer Detail file
     C*
     C     ELKY2         CHAIN     PEL01                              60
     C*
     C     *IN60         IFEQ      *OFF
      **** BEGIN ADD **** 10/09/00 *********************************************
     C                   Z-ADD     ELPFDT        XXPFDT            7 0
     C                   Z-ADD     ELPTDT        XXPTDT            7 0
      **** END   ADD **** 10/09/00 *********************************************
      **** BEGIN ADD **** 05/04/98 *******************************
     C     ELKY1         CHAIN     PEL01                              60
     C     *IN60         DOWEQ     *OFF
      **** END   ADD **** 05/04/98 *******************************
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
      **** BEGIN CHG **** 07/16/99 *********************************************
     C******************** Z-ADDELPFDT    XXEFDT
     C                   Z-ADD     ELPFDT        XXDT
      **** END   CHG **** 07/16/99 *********************************************
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C*  Write Workfile #1:  Header
     C*
     C                   EXSR      SRWRK1
      **** BEGIN ADD **** 05/04/98 ***********************************
     C     ELKY1         READE     PEL01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C     ELKY2         CHAIN     PEL01                              60
      **** END   ADD **** 05/04/98 ***********************************
     C*
     C*  Write Workfile #2:  Split Rate / Mod
     C*
     C                   EXSR      SRWRK2
     C*
     C*  Write Workfile #3:  Exposure (Payroll)
     C*
     C                   EXSR      SRWRK3
     C*
     C*  Write Workfile #4:  Loss
     C*
     C                   EXSR      SRWRK4
     C*
     C*  Initialize Exp. Mod. arrays
     C*
     C                   MOVEA     *ZERO         XMD
     C                   MOVEA     *ZERO         XDT
     C*
     C                   ENDIF                                                  PEL01
     C*
     C                   ENDIF                                                  *IN60 IFEQ 0
     C                   ENDIF                                                  YR > Zero
     C                   ENDDO                                                  I
     C*
     C*  Read Next Employer Master record based on QQEMP# PARM
     C*
     C   44              MOVE      *ON           *IN60
     C*
     C  N44EMKY2         SETGT     PEM01
     C  N44              EXSR      SRSTRP
     C*
     C                   ENDDO                                                  *IN60 DOWEQ 0
     C*
     C*  End Program
     C*
     C                   MOVE      *ON           *INLR
     C*
     C**********************************************************************
     C*  SRWRK1 - Subroutine to write Header record
     C**********************************************************************
     C     SRWRK1        BEGSR
      **** BEGIN ADD **** 05/04/98 ********************************
     C     W1KY          CHAIN     PW101                              60
      *
     C     *IN60         IFEQ      *ON
      **** END   ADD **** 05/04/98 ********************************
     C*
     C                   Z-ADD     ELCO#         W1CO#
     C                   Z-ADD     ELFND         W1FND
     C                   Z-ADD     ELEMP#        W1EMP#
     C                   Z-ADD     ELFYR         W1FYR
     C*
      **** BEGIN CHG **** 10/09/00 *********************************************
     C******************** Z-ADDELPFDT    W1PFDT
     C******************** Z-ADDELPTDT    W1PTDT
     C                   Z-ADD     XXPFDT        W1PFDT
     C                   Z-ADD     XXPTDT        W1PTDT
      **** END   CHG **** 10/09/00 *********************************************
     C                   Z-ADD     XXEFDT        W1RTDT
     C*
     C                   Z-ADD     *ZERO         XXPREM           11 2
      **** BEGIN ADD **** 05/04/98 ***************************************
     C                   ENDIF                                                  *IN60 IFEQ *ON
      **** END   ADD **** 05/04/98 ***************************************
     C*
     C*  Calculate Premium (Step 12) before Experience Modifier
     C*
     C     ELFAF         IFEQ      'A'
     C*
     C     ELAMPR        ADD       ELAIN1        XXPREM
     C     XXPREM        SUB       ELADCM        XXPREM
     C     XXPREM        SUB       ELADCS        XXPREM
     C*
     C     RRDV1L        IFEQ      'J'
     C*** BEGIN ADD ***  01/13/03  ************************************
     C     RRDV1L        OREQ      'M'
     C*** END ADD *****  01/13/03  ************************************
     C     XXPREM        SUB       ELADC2        XXPREM
     C*** BEG ADD *****  06/21/10  ************************************
     C     XXPREM        SUB       ELD1A2        XXPREM
     C*** END ADD *****  06/21/10  ************************************
     C                   ENDIF
     C*
     C     RRDV2L        IFEQ      'J'
     C*** BEGIN ADD ***  01/13/03  ************************************
     C     RRDV2L        OREQ      'M'
     C*** END ADD *****  01/13/03  ************************************
     C     XXPREM        SUB       ELADV2        XXPREM
     C*** BEG ADD *****  06/21/10  ************************************
     C     XXPREM        SUB       ELD2A2        XXPREM
     C*** END ADD *****  06/21/10  ************************************
     C                   ENDIF
     C*
     C     RRRDPR        IFEQ      'M'
     C     XXPREM        SUB       ELADCT        XXPREM
     C                   ENDIF
     C*
     C                   ELSE                                                   ELFAF = 'E'
     C*
     C     ELMAPR        ADD       ELEIN1        XXPREM
     C     XXPREM        SUB       ELEDCM        XXPREM
     C     XXPREM        SUB       ELEDCS        XXPREM
     C*
     C     RRDV1L        IFEQ      'J'
     C*** BEGIN ADD ***  01/13/03  ************************************
     C     RRDV1L        OREQ      'M'
     C*** END ADD *****  01/13/03  ************************************
     C     XXPREM        SUB       ELEDC2        XXPREM
     C*** BEG ADD *****  06/21/10  ************************************
     C     XXPREM        SUB       ELD1E2        XXPREM
     C*** END ADD *****  06/21/10  ************************************
     C                   ENDIF
     C*
     C     RRDV2L        IFEQ      'J'
     C*** BEGIN ADD ***  01/13/03  ************************************
     C     RRDV2L        OREQ      'M'
     C*** END ADD *****  01/13/03  ************************************
     C     XXPREM        SUB       ELEDV2        XXPREM
     C*** BEG ADD *****  06/21/10  ************************************
     C     XXPREM        SUB       ELD2E2        XXPREM
     C*** END ADD *****  06/21/10  ************************************
     C                   ENDIF
     C*
     C     RRRDPR        IFEQ      'M'
     C     XXPREM        SUB       ELEDCT        XXPREM
     C                   ENDIF
     C                   ENDIF                                                  ELFAF
     C*
      **** BEGIN ADD **** 05/04/98 *********************************
     C     *IN60         IFEQ      *ON
      **** END   ADD **** 05/04/98 *********************************
     C                   Z-ADD     XXPREM        W1PREM
     C*
     C                   WRITE     PW101
      **** BEGIN ADD **** 05/04/98 ***************************************
     C                   ELSE
     C                   ADD       XXPREM        W1PREM
     C                   UPDATE    PW101
     C                   ENDIF                                                  *IN60 IFEQ *ON
      **** END   ADD **** 05/04/98 ***************************************
     C*
     C                   ENDSR
     C*
     C**********************************************************************
     C*  SRWRK2 - Subroutine to write Split Rate / Mod  records
     C**********************************************************************
     C     SRWRK2        BEGSR
     C*
     C*  CLEAR PAYROLL ARRAYS
     C*
     C*
     C*   LOAD POLICY FROM DATE INTO ARRAY AS 1ST PAYROLL DATE
     C*
     C                   Z-ADD     ELPFDT        PDAT(Z)
     C                   Z-ADD     ELPFDT        RDAT(Z)
     C*
     C*   USE ANNIVERSARY DATE TO RETRIEVE RATE IF NOT = 0
     C*
     C                   Z-ADD     0             XXANDT
     C     ELANDT        COMP      0                                      78
     C  N78ELANDT        COMP      ELPFDT                                 78
     C  N78              Z-ADD     ELANDT        XXANDT
     C  N78              ADD       1             XXANYR
     C  N78XXANDT        COMP      ELPFDT                               7878
     C*
     C*  IF ARD IS NOT WITHIN POLICY PERIOD, DO NOT SPLIT PAYROLLS
     C*  BUT GET RATE EFFECTIVE AS OF ARD
     C*
     C  N78              Z-ADD     ELANDT        RDAT(Z)
     C     XXANDT        IFGE      F1ENPO
     C     XXANDT        ORLE      ELPFDT
     C                   MOVE      *ON           *IN78
     C                   END
     C*
     C*  LOAD PAYROLL DATE & RATE ARRAYS
     C*
     C                   ADD       1             Z
     C     *IN78         IFEQ      *OFF
     C     XXANDT        IFLT      ELPTDT
     C                   Z-ADD     XXANDT        PDAT(Z)
     C                   Z-ADD     XXANDT        RDAT(Z)
     C                   ADD       1             Z
     C                   END
     C                   END                                                      *IN78='0'
     C*
     C     Z             SUB       1             ZMAX              3 0
     C*
     C*  Assign Experience Modifier values to the Period array.
     C*
     C     XDT(1)        IFGT      *ZERO
     C*
     C*  Find 'Highest Index' of Modifier Changes
     C*
     C                   SELECT
     C     XDT(3)        WHENGT    *ZERO
     C                   Z-ADD     3             K                 3 0
     C     XDT(2)        WHENGT    *ZERO
     C                   Z-ADD     2             K                 3 0
     C     XDT(1)        WHENGT    *ZERO
     C                   Z-ADD     1             K                 3 0
     C                   ENDSL
     C*
     C                   Z-ADD     ZMAX          J                 3 0
     C*
     C*  Process the Period array from the last index backwards
     C*  inserting Modifiers.
     C*
     C     LOOP1         TAG
     C*
     C                   MOVE      *OFF          *IN60
     C     *IN60         DOWEQ     *OFF
     C*
     C     XDT(K)        IFGT      PDAT(J)
     C                   SUB       1             K
     C*
     C     K             IFEQ      *ZERO
     C                   Z-ADD     1             K
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C                   ENDDO
     C*
     C                   Z-ADD     PDAT(J)       SMDT(J)
     C                   Z-ADD     XMD(K)        SMD(J)
     C*
     C                   SUB       1             J
     C     J             IFGT      ZMIN
     C                   GOTO      LOOP1
     C                   ENDIF
     C*
     C                   ENDIF                                                  XDT > 0
     C*
     C*  Assign Workfile #2 Fields and write record.
     C*
     C     ZMIN          ADD       1             X
     C*
     C     PDAT(X)       DOWNE     *HIVAL
     C                   Z-ADD     ELCO#         W2CO#
     C                   Z-ADD     ELFND         W2FND
     C                   Z-ADD     ELEMP#        W2EMP#
     C                   Z-ADD     ELFYR         W2FYR
     C                   Z-ADD     X             W2PER#
     C                   Z-ADD     PDAT(X)       W2RTDT
     C                   Z-ADD     SMDT(X)       W2MDDT
     C                   Z-ADD     SMD(X)        W2EXMD
     C*
     C                   WRITE     PW201
     C*
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   Z-ADD     ZMAX          ZMIN
     C*
     C                   ENDSR
     C*
     C**********************************************************************
     C*  SRWRK3 - Subroutine to write Exposure (Payroll) records
     C**********************************************************************
     C     SRWRK3        BEGSR
     C*
     C*   Read Employer Payroll file
     C*
     C     EPKEY         CHAIN     PEP01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C*   Process ACTIVE records Only
     C*
     C     EPSTAT        IFEQ      'A'
     C*
     C                   Z-ADD     1             X                 3 0
     C     EPEFDT        LOOKUP    PDAT(X)                              6060
     C*
     C*   Process Recs With Effective DT LE to a Period Effective DT
     C*
     C     *IN60         IFEQ      *ON
     C*
     C                   Z-ADD     X             XXPER#
     C                   Z-ADD     EPCLAS        XXCLAS
     C*
     C*  Determine if Work File #3 record exists
     C*
     C     W3KEY         CHAIN     PW301                              61
     C*
     C     *IN61         IFEQ      *ON
     C                   Z-ADD     ELCO#         W3CO#
     C                   Z-ADD     ELFND         W3FND
     C                   Z-ADD     ELEMP#        W3EMP#
     C                   Z-ADD     ELFYR         W3FYR
     C                   Z-ADD     X             W3PER#
     C                   Z-ADD     PDAT(X)       W3RTDT
     C                   Z-ADD     SMDT(X)       W3MDDT
     C                   Z-ADD     EPCLAS        W3CLAS
     C                   Z-ADD     SMD(X)        W3EXMD
     C*
     C     EPSAFL        IFEQ      'A'
     C                   Z-ADD     EPAAPR        W3EAMT
     C                   ELSE
     C                   Z-ADD     EPEAPR        W3EAMT
     C                   ENDIF
     C*
      **** BEGIN ADD **** 09/17/98 *********************************************
     C                   Z-ADD     EPARAT        W3ARAT
     C     EPSAFL        IFEQ      'A'
     C                   Z-ADD     EPAMPR        W3AMPR
     C                   ELSE
     C                   Z-ADD     EPEMPR        W3AMPR
     C                   ENDIF
     C     EPSPEC        IFEQ      'F'
     C                   MOVE      '21'          W3EXCC
     C                   ELSE
     C                   MOVE      '11'          W3EXCC
     C                   ENDIF                                                  EPSPEC IFEQ 'F'
      **** END   ADD **** 09/17/98 *********************************************
     C                   WRITE     PW301
     C*
     C                   ELSE
     C*
     C     EPSAFL        IFEQ      'A'
     C     W3EAMT        ADD       EPAAPR        W3EAMT
     C                   ELSE
     C     W3EAMT        ADD       EPEAPR        W3EAMT
     C                   ENDIF
     C*
     C                   UPDATE    PW301
     C*
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     EPKEY         READE     PEP01                                  60
     C                   ENDDO
     C*
     C                   ENDSR
     C*
     C**********************************************************************
     C*  SRWRK4 - Subroutine to write Loss records
     C**********************************************************************
     C     SRWRK4        BEGSR
     C*
     C*  Read the Claim Master file
     C*
     C     CMKEY         CHAIN     PCM01                              50
     C     *IN50         DOWEQ     *OFF
     C*
     C*  Check the Total Incurred of the Claim for Medical and
     C*  Compensation.  If Greater Than the field XXEXC Then
     C*  this is an Excess Claim.
     C*
      **** BEGIN CHG **** 05/15/98 *********************************
     C******************** Z-ADD0         XXCOMP  90
     C******************** Z-ADD0         XXMED   90
     C******************** Z-ADD0         XXLEG   90
     C******************** Z-ADD0         XXOTH   90
     C                   Z-ADD     0             XXCOMP            9 2
     C                   Z-ADD     0             XXMED             9 2
     C                   Z-ADD     0             XXLEG             9 2
     C                   Z-ADD     0             XXOTH             9 2
      **** END   CHG **** 05/15/98 *********************************
      *
     C     1             DO        10            X
     C*
     C                   MOVEL     CMT(X)        XXKEY
     C     TBKEY         CHAIN     PRT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   SELECT
     C     RTTYPE        WHENEQ    'C'
     C                   ADD       CMA(X)        XXCOMP
     C                   ADD       CMR(X)        XXCOMP
     C     RTTYPE        WHENEQ    'M'
     C                   ADD       CMA(X)        XXMED
     C                   ADD       CMR(X)        XXMED
     C     RTTYPE        WHENEQ    'L'
     C                   ADD       CMA(X)        XXLEG
     C                   ADD       CMR(X)        XXLEG
     C                   OTHER
     C                   ADD       CMA(X)        XXOTH
     C                   ADD       CMR(X)        XXOTH
     C                   ENDSL
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C     XXMED         ADD       XXCOMP        XXA90             9 0
     C*
     C*  If Claim is Closed and Total Incurred is zero, skip claim processing
     C*
     C     CMSTAT        IFEQ      'C'
     C     XXA90         ANDEQ     *ZERO
     C                   GOTO      NXTCLM
     C                   ENDIF
     C*
     C     XXA90         IFGT      XXEXC
     C*
     C*  Write Excess Record
     C*
     C                   EXSR      W4ASGN
     C*
     C                   WRITE     PW401
     C*
     C                   ELSE
     C*
     C*  This is a NON EXCESS Claim.  A record must be written for
     C*  this NCCI, Class Code, and Period # and Claim Status
     C*
     C*
     C*  Find Period # for ACCIDENT Date
     C*
     C                   Z-ADD     1             X
     C     CMACDT        LOOKUP    PDAT(X)                              6060
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     X             XXPER#
     C                   ENDIF
     C*
     C*  Access record for NON EXCESS Claims.
     C*
     C                   MOVE      CMEMP#        XXEMP#
     C                   MOVE      CMNCCI        XXNCCI
     C                   MOVE      CMCLAS        XXCLAS
     C     CMSTAT        IFEQ      'O'
     C                   MOVE      '0'           XXSTAT
     C                   ELSE
     C                   MOVE      '1'           XXSTAT
     C                   END
     C*
     C     W4KEY         CHAIN     PW401                              65
     C*
     C***  BEGIN ADD  ***  02/15/99  **********************************
     C*    NON EXCESS record should have accident date of zero
     C*
     C     *IN65         DOWEQ     *OFF
     C     W4ACDT        ANDNE     *ZERO
     C     W4KEY         READE     PW401                                  65
     C                   ENDDO
     C***  END   ADD  ***  02/15/99  **********************************
     C*
     C     *IN65         IFEQ      *ON
     C*
     C*  NON EXCESS record NOT FOUND, Initialize fields for write.
     C*
     C                   EXSR      W4ASGN
     C*
     C                   Z-ADD     *ZERO         W4ACDT
     C                   MOVE      *BLANK        W4CLM
     C*
     C                   WRITE     PW401
     C*
     C                   ELSE
     C*
     C*  NON EXCESS record FOUND, Increment fields for update.
     C*  Increment Incurred Amounts
     C*
      **** BEGIN ADD **** 05/15/98 ***************************************
     C                   Z-ADD     XXMED         XROUND
     C     XCENTS        IFGE      50
     C                   ADD       1             XWHOLE
     C                   ENDIF                                                  XCENTS IFGE 50
     C                   Z-ADD     0             XCENTS
     C                   Z-ADD     XROUND        XXMED
      *
     C                   Z-ADD     XXCOMP        XROUND
     C     XCENTS        IFGE      50
     C                   ADD       1             XWHOLE
     C                   ENDIF                                                  XCENTS IFGE 50
     C                   Z-ADD     0             XCENTS
     C                   Z-ADD     XROUND        XXCOMP
      **** END   ADD **** 05/15/98 ***************************************
      *
     C                   ADD       XXMED         W4MED
     C                   ADD       XXCOMP        W4COMP
     C*
     C*  Add to the number of NON-EXCESS Claims
     C*
     C                   ADD       1             W4NCLM
     C*
     C                   UPDATE    PW401
     C*
     C                   ENDIF                                                  NON-EXCESS END
     C                   ENDIF                                                  EXCESS END
     C*
     C*  Get the Next Claim Master record
     C*
     C     NXTCLM        TAG
     C*
     C     CMKEY         READE     PCM01                                  50
     C                   ENDDO
     C*
     C                   ENDSR
     C*
     C**********************************************************************
     C*  W4ASGN - Subroutine to Assign W4 Workfile Fields
     C**********************************************************************
     C     W4ASGN        BEGSR
     C*
     C                   MOVE      CMCO#         W4CO#
     C                   MOVE      CMFND         W4FND
     C                   MOVE      CMFYR         W4FYR
     C                   MOVE      CMCASE        W4CLM
     C                   MOVE      CMACDT        W4ACDT
     C                   MOVE      CMEMP#        W4EMP#
     C                   MOVE      CMNCCI        W4NCCI
     C                   Z-ADD     1             W4NCLM
     C                   MOVE      CMCLAS        W4CLAS
     C                   Z-ADD     11            W4LCC
      *
      **** BEGIN ADD **** 05/15/98 ***************************************
     C                   Z-ADD     XXMED         XROUND
     C     XCENTS        IFGE      50
     C                   ADD       1             XWHOLE
     C                   ENDIF                                                  XCENTS IFGE 50
     C                   Z-ADD     0             XCENTS
     C                   Z-ADD     XROUND        XXMED
      *
     C                   Z-ADD     XXCOMP        XROUND
     C     XCENTS        IFGE      50
     C                   ADD       1             XWHOLE
     C                   ENDIF                                                  XCENTS IFGE 50
     C                   Z-ADD     0             XCENTS
     C                   Z-ADD     XROUND        XXCOMP
      **** END   ADD **** 05/15/98 ***************************************
      *
     C                   Z-ADD     XXMED         W4MED
     C                   Z-ADD     XXCOMP        W4COMP
     C*
     C     CMSTAT        IFEQ      'C'
     C                   MOVE      '1'           W4STAT
     C                   ELSE
     C                   MOVE      '0'           W4STAT
     C                   ENDIF
     C*
     C*  Determine the Period #
     C*
     C                   Z-ADD     1             X
     C     CMACDT        LOOKUP    PDAT(X)                              6060
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     X             W4PER#
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C**********************************************************************
     C*  SRSTRP - Subroutine to Determine Employer record with EMSTRP = 'Y'
     C**********************************************************************
     C     SRSTRP        BEGSR
     C*
     C   44EMKY2         READE     PEM01                                  60
     C  N44EMKY1         READE     PEM01                                  60
     C*
     C*  NOTE:  Indicator 60 *ON means End of File has occurred
     C*
     C*  Default the first Employer record read (Employer # and Division)
     C*
     C                   Z-ADD     EMEMP#        XXEMP#
     C                   Z-ADD     EMDIV         XXDIV
     C                   MOVE      *OFF          *IN61
     C*
     C     *IN61         DOWEQ     *OFF
     C*
     C                   SELECT
     C     EMSTRP        WHENEQ    'Y'
     C                   Z-ADD     EMEMP#        XXEMP#
     C                   Z-ADD     EMDIV         XXDIV
     C                   MOVE      *ON           *IN61
     C     *IN60         WHENEQ    *ON
     C                   MOVE      *ON           *IN61
     C                   OTHER
     C     EMKY2         READE     PEM01                                  61
     C*
     C*  NOTE:  Indicator 61 *ON has been logically set up to mean
     C*         End of Employer has occurred.
     C*
     C                   ENDSL
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C**********************************************************************
