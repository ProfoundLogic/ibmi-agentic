     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W8200R - BUILD BSI-17 WORK RECORD
     H*************************************************************
     H** PROGRAM DESCRIPTION:
     H**   THIS PROGRAM GENERATES A WORK FILE FOR SUBMITTING BSI-17
     H**   STATE REPORTS.
     H**
     H**   A RECORD IS WRITTEN FOR EACH EXCESS CLAIM SHOWING AMOUNT
     H**   INCURRED.
     H**
     H**   ANOTHER RECORD IS WRITTEN FOR ALL NON-EXCESS CLAIMS WITH THE
     H**   SAME NCCI, CLASS CODE, AND PAYROLL EFFECTIVE DATE WITH THE
     H**   TOTAL INCURRED.
     H**
     H**   EACH NON-EXCESS RECORD IS USED TO ESTABLISH THE EMPLOYERS
     H**   PAYROLL DATE RANGES (ANNIVERSARY RATING MAY CAUSE MORE THAN
     H**   ONE PAYROLL DATE RANGE).  EACH DATE RANGE MUST BE SPECIFIED
     H**   EVEN THOUGH NO NON-EXCESS CLAIMS EXISTED.
     H**
     H**   IF QQRPT = '1', A RECORD WILL BE WRITTEN FOR EACH PAYROLL
     H**   DATE RANGE FOR EMPLOYERS WITH NO CLAIMS.
     H**
     H**   CALLED FROM:
     H**         W8200C - OVERRIDE, LOAD TAPE CL
     H**   CALLS TO:
     H**         NONE
     H*****************************************************************
     H* REVISIONS:                                                    *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/25/11  C:246 SUPPORT 75005 CLT - 71                        *
     H*                 Move the claim number to file as NCCI as '*'  *
     H*                 is not valid on column 7 on ERM-6 form when   *
     H*                 claim identification number is 1.             *
     H*                                                               *
     H* 04/27/10  C:180 SUPPORT CHG EXCESS AMMT FROM 5000 TO 2000 FOR
     H*                 CLT 71 - GOING BACK TO DEFAULT.
     H*
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 04/29/07  C:15  Project: 71WHI                                *
     H*                 Chgd excess amount from 2000 to 5000 for clt  *
     H*                 71 only. (Original change was on 7/9/98).     *
     H*                                                               *
     H* 12/20/06  C:15  Support: WC129/56041                          *
     H*                 Chgd pgm to use the policy inception date of  *
     H*                 div 0 instead of the state reporting div.     *
     H*                                                               *
     H* 01/08/02  C:15  Support: WC094/30604                          *
     H*                 Fixed date conversion routine for month begin *
     H*                 and end dates.                                *
     H*                                                               *
     H* 04/30/98  C:35  PROJECT: WC577                                *
     H*                 REMOVED UNNECESSARY CALL TO DATE ROUTINE.     *
     H*                                                               *
     H* 04/10/98  C:35  PROJECT: SUPPORT                              *
     H*                 FIXED DATE MANIPULATION FOR MONTH BEGINNING   *
     H*                 AND END DATE.                                 *
     H*                                                               *
     H* 04/02/98  C:35  PROJECT: SUPPORT                              *
     H*                 PROGRAM WAS CHECKING FOR SRERR = *BLANK AFTER *
     H*                 DATE CONVERSION, BUT NEW DATE CONVERSION      *
     H*                 RETURNS '0' OR '1'.  ALSO, PROGRAM WAS        *
     H*                 PASSING PARAMETER TO SRPAST8 AS CHARACTER     *
     H*                 INSTEAD OF NUMERIC.                           *
     H*                                                               *
     H*****************************************************************
     H** INDICATOR USAGE:
     H**
     H**    60     GLOBAL FILE CHAIN
     H**    63     B7KY2 CHAIN - DETERMINE IF DATE HAS BEEN ESTABLISHED
     H**    65     B7KEY CHAIN
     H**    66     RUN FOR SINGLE INCEPTION MONTH/YEAR
     H**    78     ANNIVERSARY RATING IN EFFECT (*IN78 = '0')
     H**    85     FIRST TIME INDICATOR
     H**
     H*************************************************************
     H** FILE USAGE:
     H**
     H** FILES:
     H**       WXBL1 (PBL02): CO/FND/FYR/EMP/ACCD
     H**                      OMIT SS# NE '000000000'
     H*************************************************************
     FWDELL     IF   E           K DISK
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWMUPL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PB701)
     FWMCML5    IF   E           K DISK
     FWXB7L     UF A E           K DISK
     FWXB7L1    IF   E           K DISK
     F                                     RENAME(PB701:PB702)
     FWMCDL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FQSYSPRT2  O    F  198        PRINTER OFLIND(*INOF)

     D*
     D*  THE FOLLOWING ARRAY IS TO SPECIFY DATE IN WHICH PAYROLLS
     D*  TAKE ON DIFFERENT RATING (THERE CAN BE UP TO 3 DUE TO THE
     D*                            THE 9/1/90 CHANGES)
     D PDAT            S              7  0 DIM(4) ASCEND                        PAYROLL DATES
     D CLM             S              5  0 DIM(4)                               CLAIMS/PERIOD
     D MSG             S             60    DIM(6) CTDATA PERRCD(1)
     D                 DS
     D  XXANDT                 1      7  0
     D  XXANYR                 1      3  0
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
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5)                               CANCEL  DATE
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D                 DS
     D  XXQQMY                 1      6
     D  XXQQMM                 1      2
     D*** Begin add *** 01/08/02 **************************************
     D  XXQQCY                 3      6  0
     D*** End add ***** 01/08/02 **************************************
      **** BEGIN CHG **** 04/10/98 *********************************************
     D*************************************** 3   6 XXQQYY
     D*** Begin dlt *** 01/08/02 **************************************
     D***********************************     3   4 XXQQCC
     D***********************************     5   60XXQQYY
     D*** End dlt ***** 01/08/02 **************************************
      **** END   CHG **** 04/10/98 *********************************************
     D                 DS
     D  XXRFDT                 1      7  0
     D  XXRFYY                 1      3  0
     D  XXRFMM                 4      5  0
     D  XXRFDD                 6      7  0
     D                 DS
     D  XXRTDT                 1      7  0
     D  XXRTYY                 1      3  0
     D  XXRTMM                 4      5  0
     D  XXRTDD                 6      7  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     EMKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     ELKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C     ELKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
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
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  BSI-17 WORKFILE
     C*
     C     B7KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXNCCI
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXSS#
     C                   KFLD                    XXACCD
     C*
     C     B7KY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C     B7KY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXPFDT
     C*
     C*
     C*   KEY LIST FOR THE CLAIMANT DEMOGRAPHIC FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQRPT             1
     C                   PARM                    QQRPMY            6
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
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMEMP#        XXSEMP
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELPFDT        XXPFDT
     C     *LIKE         DEFINE    ELPTDT        XXPTDT
     C     *LIKE         DEFINE    B7ACCD        XXACCD
     C     *LIKE         DEFINE    B7EOPD        XXEOPD
     C     *LIKE         DEFINE    CMCLAS        XXCLAS
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    CMNCCI        XXNCCI
     C     *LIKE         DEFINE    RTKEY         XXKEY
     C*
     C                   Z-ADD     0             X                 5 0
     C                   Z-ADD     0             Y                 2 0
     C                   Z-ADD     0             Z                 2 0
     C                   Z-ADD     0             XXW112           11 2
     C                   Z-ADD     0             XXCMP            11 2
     C                   Z-ADD     0             XXMED            11 2
     C                   Z-ADD     0             XXLEG            11 2
     C                   Z-ADD     0             XXOTH            11 2
     C                   Z-ADD     0             XXCNT             5 0
     C                   Z-ADD     0             XXWK30            3 0
     C                   Z-ADD     0             XXWK32            3 2
     C                   MOVEL     '0000000'     XXSS#
     C                   MOVE      '00'          XXSS#
     C*
     C**** Begin add *** 04/29/07 *************************************
     C*  Retrieve the client ID.
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C**** End add ***** 04/29/07 *************************************
     C*
     C* CLM ARRAY WILL KEEP TRACK OF THE NUMBER OF CLAIMS THAT EXIST
     C* FOR EACH PERIOD. AFTER ALL THE PERIODS HAVE BEEN PROCESS A
     C* CLACULATION WILL DETERMINE THE NUMBER OF PAGES NEEDED TO PRINT
     C* ALL OF THE EMPLOYERS BSI-17 RECORDS.  20 DETAIL LINES CAN
     C* BE PRINTED PER PAGE.
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C     QQRPMY        IFNE      '000000'
     C*
     C                   MOVE      *ON           *IN66
     C                   MOVE      QQRPMY        XXQQMY
     C                   MOVE      XXQQMM        XXRFMM
     C*** Begin add *** 01/08/02 **************************************
     C                   SUB       1900          XXQQCY
     C                   Z-ADD     XXQQCY        XXRFYY
     C*** End add ***** 01/08/02 **************************************
      **** BEGIN CHG **** 04/10/98 *********************************************
     C******************** MOVE XXQQYY    XXRFYY
     C*** Begin dlt *** 01/08/02 **************************************
     C***************      Z-ADDXXQQYY    XXRFYY
     C*** End dlt ***** 01/08/02 **************************************
      **** END   CHG **** 04/10/98 *********************************************
     C                   Z-ADD     1             XXRFDD
     C                   MOVE      XXQQMM        XXRTMM
     C*** Begin add *** 01/08/02 **************************************
     C                   Z-ADD     XXQQCY        XXRTYY
     C*** End add ***** 01/08/02 **************************************
      **** BEGIN CHG **** 04/10/98 *********************************************
     C******************** MOVE XXQQYY    XXRTYY
     C*** Begin dlt *** 01/08/02 **************************************
     C***************      Z-ADDXXQQYY    XXRTYY
     C*** End dlt ***** 01/08/02 **************************************
      **** END   CHG **** 04/10/98 *********************************************
     C                   Z-ADD     31            XXRTDD
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             XXRFDT
     C                   Z-ADD     9999999       XXRTDT
     C                   ENDIF                                                  QQRPMY IFNE
     C*
     C*  EXCESS AMOUNT 2,000.00
     C*
     C                   Z-ADD     2000          XXEXC             5 0
     C**** Begin add *** 04/29/07 *************************************
     C     CLTID         IFEQ      '071'
     C**** BEGIN DLT 04/27/10 *****************************************
     C******               Z-ADD5000      XXEXC
     C**** END DLT   04/27/10 *****************************************
     C                   ENDIF
     C**** End add ***** 04/29/07 *************************************
     C*
     C*  GET THE USER PARMS
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*  GET THE FUND MASTER
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         CABEQ     '1'           ENDPG
     C*
     C*  GET THE FUND DETAIL FILE
     C*
     C     F1KEY         CHAIN     PF101                              60
     C     *IN60         CABEQ     '1'           ENDPG
     C*
     C                   TIME                    DSDTTM
     C*
     C  N11              Z-ADD     *ZEROS        XXCNT4            7 0
     C  N11              MOVE      '1'           *IN11
     C*
     C*  GET THE COMPANY MASTER FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
      *
     C                   ADD       1             XXCNT4
     C*
     C*  SINCE THIS PROGRAM WILL BE CALLED UP TO 3 TIMES BY THE
     C*  ERM-6 REPORT, BE SURE TO PRINT ROUTING ONLY ONCE
     C*
     C     XXCNT4        IFEQ      1
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '2'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C*
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD3
     C                   EXCEPT    HEAD2
     C*
     C                   END                                                    XXCNT4 = 0
     C*
     C     XXCNT4        IFGT      1
     C                   EXCEPT    HEAD2
     C                   END
     C*
     C                   Z-ADD     *ZEROS        XXCNT2            7 0
     C                   Z-ADD     *ZEROS        XXCNT3            7 0
     C                   MOVE      *OFF          *IN42
     C*
     C*  START READING THE CLAIM MASTER FILE
     C*
     C     CMKEY         CHAIN     PCM01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
     C                   MOVE      *BLANKS       XXCLNM           30
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXCLNM
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDFSN         XXFRST
     C                   MOVEL     CDLSN         XXLAST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCLNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXCLNM
     C                   END
     C*
     C* IF INCEPTION MONTH/YEAR WAS CHOSEN, CHECK TO SEE IF EMPLOYER
     C* FALLS WITHIN THE RANGE
     C*
     C     *IN66         IFEQ      *ON
     C*
     C                   MOVE      CMEMP#        XXEMP#
     C                   MOVE      CMDIV         XXDIV
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C*
     C     ELPFDT        IFLT      XXRFDT
     C     ELPFDT        ORGT      XXRTDT
     C*
     C                   GOTO      NEXTC
     C*
     C                   ENDIF                                                  EMEFD1 IFLT
     C                   ENDIF                                                  *IN66 IFEQ
     C*
     C*          FIRST TIME ROUTINE
     C*
     C  N85              Z-ADD     CMEMP#        XXSEMP
     C  N85              EXSR      DATES
     C                   MOVE      '1'           *IN85
     C*
     C* CHECK THE CHANGE IN EMPLOYER NUMBER
     C*
     C     XXSEMP        IFNE      CMEMP#
     C*    MAKE SURE ALL DATE RANGES HAVE BEEN WRITTEN FOR AN EMPLOYER
     C                   EXSR      CHECK
     C                   Z-ADD     CMEMP#        XXSEMP
     C*    LOAD PAYROLL DATES
     C                   EXSR      DATES
     C                   END
     C*
     C*  CHECK THE TOTAL INCURRED OF THE FILE FOR MEDICAL AND
     C*  COMPENSATION, IF GREATER THAN THE FIELD XXEXC THEN
     C*  THIS IS AN EXCESS CLAIM
     C*
     C                   Z-ADD     0             XXCMP
     C                   Z-ADD     0             XXMED
     C                   Z-ADD     0             XXLEG
     C                   Z-ADD     0             XXOTH
     C     1             DO        10            X
     C                   MOVEL     CMT(X)        XXKEY
     C     TBKEY         CHAIN     PRT01                              60
     C     *IN60         IFEQ      '0'
     C     RTTYPE        IFEQ      'C'
     C                   ADD       CMA(X)        XXCMP
     C                   ADD       CMR(X)        XXCMP
     C                   GOTO      NXT
     C                   END
     C     RTTYPE        IFEQ      'M'
     C                   ADD       CMA(X)        XXMED
     C                   ADD       CMR(X)        XXMED
     C                   GOTO      NXT
     C                   END
     C     RTTYPE        IFEQ      'L'
     C                   ADD       CMA(X)        XXLEG
     C                   ADD       CMR(X)        XXLEG
     C                   GOTO      NXT
     C                   END
     C                   ADD       CMA(X)        XXOTH
     C                   ADD       CMR(X)        XXOTH
     C                   END
     C     NXT           TAG
     C                   END
     C*
     C     XXMED         ADD       XXCMP         XXW112
     C     XXW112        IFGT      XXEXC
     C*
     C*  WRITE EXCESS RECORD
     C*
     C                   MOVE      CMCO#         B7CO#
     C                   MOVE      CMFND         B7FND
     C                   MOVE      CMFYR         B7FYR
     C                   MOVE      CMCASE        B7CASE
     C                   MOVE      CMACDT        B7ACCD
     C                   Z-ADD     ELPFDT        B7PFDT                         Policy Eff Date
     C                   MOVE      'Y'           B7EXCS                         Excess Claim
     C                   MOVE      *ZERO         B7EOPD
     C                   MOVE      CMEMP#        B7EMP#
     C                   MOVE      EMDIV         B7DIV
     C                   MOVE      CMSS#         B7SS#
     C                   MOVE      CMNCCI        B7NCCI
     C                   MOVE      *ZERO         B7#CLM
     C                   MOVEL     CMCLAS        B7CLAS
     C                   MOVE      QQRPT         B7REP#
     C                   Z-ADD     XXMED         B7MEDA                               "
     C                   Z-ADD     XXCMP         B7INDA                               "
     C     XXOTH         ADD       XXLEG         B7ALAE
     C                   Z-ADD     0             B7#PAG
     C                   MOVE      *BLANK        B7CATA
     C     CMSTAT        IFEQ      'C'
     C                   MOVE      '1'           B7STAT
     C                   ELSE
     C                   MOVE      '0'           B7STAT
     C                   END
     C*
     C*  ADD TO CLAIM ARRAY THAT CORRESPONDS TO THE TIME PERIOD
     C*
     C                   Z-ADD     1             Z
     C     CMACDT        LOOKUP    PDAT(Z)                            60
     C     *IN60         IFEQ      '1'
     C     Z             ANDGE     2
     C                   SUB       1             Z
     C                   ADD       1             CLM(Z)
     C                   ELSE
     C                   MOVE      MSG(4)        XXDESC           60
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD3
     C                   EXCEPT    HEAD2
     C                   MOVE      '0'           *INOF
     C                   END
     C                   ADD       1             XXCNT2
     C                   EXCEPT    DTL1
     C  N50              GOTO      NEXTC
     C   50              GOTO      NEXTE
     C                   END
     C*
     C                   WRITE     PB701
     C                   ADD       1             XXCNT3
     C*
     C                   ELSE
     C*
     C*  THIS IS A NON EXCESS CLAIM, A RECORD MUST BE WRITTEN FOR
     C*  THIS NCCI, CLASS CODE, AND CORRESPONDING PAYROLL DATE RANGE
     C*
     C*  INITIALIZE DATES IN THE CASE WE DO NOT FIND AN EFF DATE
     C*
     C                   Z-ADD     PDAT(1)       XXACCD
     C                   Z-ADD     1             X
     C     *ZERO         LOOKUP    PDAT(X)                                60
     C   60              SUB       1             X
     C     X             IFGE      1
     C     X             ANDLE     4
     C                   Z-ADD     PDAT(X)       XXEOPD
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXEOPD
     C                   END
     C*
     C                   Z-ADD     XXEOPD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDATE            8 0
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXDATE            8 0
     C*
      **** BEGIN CHG **** 04/02/98 *********************************************
     C********** SRERR     IFNE *BLANK
     C     SRERR         IFNE      *OFF
      **** END   CHG **** 04/02/98 *********************************************
     C                   MOVE      MSG(5)        XXDESC
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD3
     C                   EXCEPT    HEAD2
     C                   MOVE      '0'           *INOF
     C                   END
     C                   ADD       1             XXCNT2
     C                   EXCEPT    DTL1
     C  N50              GOTO      NEXTC
     C   50              GOTO      NEXTE
     C                   END
     C*
     C*  FIND EFFECTIVE PAYROLL DATE FOR ACCIDENT DATE
     C*
     C                   Z-ADD     1             Y
     C     CMACDT        LOOKUP    PDAT(Y)                            60
     C     *IN60         IFEQ      '1'
     C     Y             ANDGE     2
     C     Y             SUB       1             X
     C                   Z-ADD     PDAT(X)       XXACCD
     C                   Z-ADD     PDAT(Y)       XXEOPD
     C                   ELSE
     C                   MOVE      MSG(6)        XXDESC
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD3
     C                   EXCEPT    HEAD2
     C                   MOVE      '0'           *INOF
     C                   END
     C                   ADD       1             XXCNT2
     C                   EXCEPT    DTL1
     C  N50              GOTO      NEXTC
     C   50              GOTO      NEXTE
     C                   END
     C*
     C*  A RECORD MUST EXIST FOR THE EMPLR, NCCI, CLASS CODE, AND
     C*  CORRESPONDING PAYROLL DATE RANGE
     C*
     C                   MOVE      CMEMP#        XXEMP#
     C                   MOVE      CMNCCI        XXNCCI
     C                   MOVE      CMCLAS        XXCLAS
     C*
     C*  FIRST, CHECK TO SEE IF DATE IS ALREADY ESTABLISHED
     C*
     C                   Z-ADD     XXACCD        XXPFDT
     C     B7KY2         CHAIN     PB702                              63
     C*
     C*  CHECK FOR NCCI, CLASS, PAYROLL DATE
     C*
     C     B7KEY         CHAIN     PB701                              65
     C     *IN65         IFEQ      '1'
     C*
     C*  NON EXCESS RECORD NOT FOUND, INITIALIZE ALL FIELDS FOR WRITE
     C*
     C                   MOVE      CMCO#         B7CO#
     C                   MOVE      CMCO#         B7CO#
     C                   MOVE      CMFND         B7FND
     C                   MOVE      CMFYR         B7FYR
     C                   MOVE      CMEMP#        B7EMP#
     C                   MOVE      EMDIV         B7DIV
     C                   MOVE      CMNCCI        B7NCCI
     C                   MOVEL     CMCLAS        B7CLAS
     C                   MOVE      QQRPT         B7REP#
      ****BEG CHG 01/25/11 *******************************
     C*****                Z-ADD0         B7CASE
     C                   MOVE      CMCASE        B7CASE
      ****END CHG 01/25/11 *******************************
     C                   Z-ADD     0             B7MEDA
     C                   Z-ADD     0             B7INDA
     C                   Z-ADD     0             B7ALAE
     C                   MOVE      *BLANK        B7CATA
     C                   MOVE      XXSS#         B7SS#
     C                   MOVE      '1'           B7STAT
     C                   Z-ADD     0             B7#CLM
     C                   Z-ADD     0             B7#PAG
     C*  LOAD DATES
     C                   MOVE      XXACCD        B7ACCD
     C*  MUST SUBTRACT 1 FROM END DATE
     C*
     C                   Z-ADD     XXEOPD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   MOVE      XXDATE        QQDTE
     C                   Z-ADD     1             QQPST
     C                   MOVE      *BLANK        QQERR
     C                   MOVE      *BLANK        QQTERM
     C                   CALL      'SRPAST8'
     C                   PARM                    QQDTE
     C                   PARM                    QQPST
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C*
     C                   MOVE      QQDTE         XXDATE
     C     QQERR         IFEQ      '0'
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         B7EOPD
     C*
     C                   ELSE
     C                   Z-ADD     XXEOPD        B7EOPD
     C                   END
     C*
     C     B7EOPD        IFLT      B7ACCD
     C                   Z-ADD     B7ACCD        B7EOPD
     C                   END
     C*
     C                   Z-ADD     ELPFDT        B7PFDT                         Policy Eff Date
     C                   MOVE      'N'           B7EXCS                         Not Excess Clm
     C*
     C                   END                                                    B7 CHAIN
     C*
     C*  CHECK THE STATUS OF THE RECORD.
     C*
     C     CMSTAT        IFEQ      'O'
     C                   MOVE      '0'           B7STAT
     C                   END
     C*
     C*  INCREASE INCURRED AMOUNTS
     C*
     C                   ADD       XXMED         B7MEDA                               "
     C                   ADD       XXCMP         B7INDA                               "
     C                   ADD       XXLEG         B7ALAE
     C                   ADD       XXOTH         B7ALAE
     C*
     C*  ADD TO THE NUMBER OF NON-EXCESS CLAIMS (B7#CLM)
     C*
     C                   ADD       1             B7#CLM
     C*
     C   65              WRITE     PB701
     C  N65              UPDATE    PB701
     C                   ADD       1             XXCNT3
     C*
     C*  IF WE ARE WRITING MORE THAN ONE DATE RANGE, THEN WE NEED TO
     C*  ADD ENOUGH LINES TO CAUSE A PAGE BREAK POSITIONING AT LINE
     C*  ONE OF NEW PAGE
     C*
     C     *IN65         IFEQ      '1'
     C                   ADD       1             CLM(X)
     C                   END
     C*
     C                   END                                                    EXCESS END
     C*
     C*  GET THE NEXT CLAIM MASTER RECORD
     C*
     C     NEXTC         TAG
     C     CMKEY         READE     PCM01                                  50
     C                   END
     C*
     C*    MAKE SURE ALL DATE RANGES HAVE BEEN WRITTEN FOR AN EMPLOYER
     C*
     C                   EXSR      CHECK
     C*
     C*  WRITE A RECORD FOR ALL EMPLOYERS WITHOUT CLAIMS
     C*
     C     QQRPT         IFEQ      '1'
     C*
     C                   MOVE      *ON           *IN42
     C                   Z-ADD     0             CMCASE
     C                   Z-ADD     0             XXACDT
     C                   MOVE      *BLANKS       XXCLNM
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFYR         XXFYR
     C*
     C     ELKEY1        CHAIN     PEL02                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  SEE IF A WORK RECORD ALREADY EXISTS
     C*
     C                   Z-ADD     ELEMP#        XXEMP#
     C     B7KY1         CHAIN     PB701                              60
     C     *IN60         IFEQ      '1'
     C*
     C* IF INCEPTION MONTH/YEAR WAS CHOSEN, CHECK TO SEE IF EMPLOYER
     C* FALLS WITHIN THE RANGE
     C*
     C     ELPFDT        IFLT      XXRFDT
     C     ELPFDT        ORGT      XXRTDT
     C                   GOTO      NEXTE
     C                   ENDIF                                                  ELPFDT IFGT
     C*
     C*  LOAD ALL PAYROLL DATES FOR THE STATE REPORTING EMPLOYER
     C*
     C                   Z-ADD     ELEMP#        XXSEMP
     C                   EXSR      DATES
     C*
     C*  WRITE A RECORD FOR ALL PAYROLL DATES
     C*
     C                   EXSR      CHECK
     C*
     C                   END                                                    B7KY1 CH
     C*
     C     NEXTE         TAG
     C     ELKEY2        SETGT     PEL02
     C     ELKEY1        READE     PEL02                                  60
     C                   END                                                    ELKEY1 CH
     C*
     C                   END                                                    QQRPT = '1'
     C*  END PROGRAM
     C*
     C     ENDPG         TAG
     C     XXCNT3        IFEQ      *ZEROS
     C                   EXCEPT    NORECS
     C                   ELSE
     C                   EXCEPT    TOTAL
     C                   END
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '2';
       Chain ORKEY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   RETURN
     C*
     C*****************************************************************
     C*  CHECK FOR ALL DATE RANGES WRITTEN FOR AN EMPLOYER
     C*****************************************************************
     C*
     C     CHECK         BEGSR
     C*
     C                   Z-ADD     2             Y
     C     Y             DOWLE     4
     C     PDAT(Y)       ANDNE     0
     C*
     C     Y             SUB       1             X
     C                   MOVE      PDAT(X)       XXPFDT
     C     B7KY2         CHAIN     PB702                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      PDAT(Y)       XXPTDT
     C                   EXSR      WORK
     C                   Z-ADD     0             B7#PAG
     C                   ADD       1             CLM(X)
     C                   WRITE     PB701
     C                   ADD       1             XXCNT3
     C                   END
     C                   ADD       1             Y
     C*
     C                   END
     C*
     C* DETERMINE HOW MANY PAGES ARE TO BE WRITTEN PER PERIOD
     C*  AND ACCUMULATE.
     C*
     C                   Z-ADD     0             XXCNT
     C                   Z-ADD     1             X
     C     X             DOWLE     4
     C     PDAT(X)       ANDNE     0
     C     CLM(X)        DIV       20            XXWK30
     C                   MVR                     XXWK32
     C     XXWK32        IFGT      *ZERO
     C                   ADD       1             XXWK30
     C                   END
     C                   ADD       XXWK30        XXCNT
     C                   ADD       1             X
     C                   END
     C*
     C*  THE FIRST EMPLOYER RECORD WILL INDICATE NUMBER OF PAGES
     C*  FOR THAT EMPLOYER
     C*
     C     B7KY1         CHAIN     PB701                              60
     C  N60XXCNT         MULT      20            B7#PAG
     C  N60              UPDATE    PB701
     C*
     C                   ENDSR
     C*****************************************************************
     C*  LOAD PAYROLL DATES
     C*****************************************************************
     C*
     C     DATES         BEGSR
     C*
     C                   Z-ADD     0             PDAT
     C                   Z-ADD     0             CLM
     C                   Z-ADD     XXSEMP        XXEMP#
     C                   Z-ADD     0             XXDIV
     C                   MOVE      *BLANKS       XXEMNM           30
     C                   Z-ADD     *ZEROS        X2PFDT            6 0
     C                   Z-ADD     *ZEROS        X2PTDT            6 0
     C*** Begin add *** 12/20/06 ***********************************
     C                   MOVE      '0'           XXMAIN            1
     C*** End add ***** 12/20/06 ***********************************
     C*
     C*   GET EMPLOYER MASTER RECORD & FIND STATE REPORTING EMPLOYER
     C*
     C     EMKEY1        CHAIN     PEM01                              60
     C     *IN60         IFEQ      '1'
     C*
     C                   MOVE      '1'           *IN95
     C     ELKEY         CHAIN     PEL01                              96
     C     *IN96         IFEQ      '0'
     C*
     C     1             DO        5             X
     C     CD(X)         IFGT      ELPFDT
     C     CD(X)         ANDLT     ELPTDT
     C                   Z-ADD     CD(X)         ELPTDT
     C                   END
     C                   END
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X2PFDT
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X2PTDT
     C*
     C                   END
     C*
     C                   MOVEL     MSG(2)        XXDESC
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD3
     C                   EXCEPT    HEAD2
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   ADD       1             XXCNT2
     C  N50              EXCEPT    DTL1
     C   50              EXCEPT    DTL2
     C                   MOVE      '0'           *IN95
     C                   MOVE      '0'           *IN96
     C                   MOVE      *ZEROS        XXSEMP
     C  N50              GOTO      NEXTC
     C   50              GOTO      NEXTE
     C                   END
     C*
     C     *IN60         DOWEQ     '0'
     C                   MOVE      '1'           *IN60
     C     EMSTRP        IFNE      'Y'
     C     EMKEY1        READE     PEM01                                  60
     C                   END
     C                   END
     C*
     C*  IF NO STATE REPORTS FLAG, AND DIVISION NOT *ZERO THEN
     C*  CHAIN TO DIVISION *ZERO
     C*
     C*** Begin add *** 12/20/06 ***********************************
     C     MAINDV        TAG
     C*** End add ***** 12/20/06 ***********************************
     C     EMSTRP        IFNE      'Y'
     C     EMDIV         ANDNE     *ZERO
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C                   MOVEL     EMNAME        XXEMNM
     C                   MOVEL     MSG(1)        XXDESC
     C     ELKEY         CHAIN     PEL01                              96
     C     *IN96         IFEQ      '0'
     C*
     C     1             DO        5             X
     C     CD(X)         IFGT      ELPFDT
     C     CD(X)         ANDLT     ELPTDT
     C                   Z-ADD     CD(X)         ELPTDT
     C                   END
     C                   END
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X2PFDT
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X2PTDT
     C                   END
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD3
     C                   EXCEPT    HEAD2
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   ADD       1             XXCNT2
     C*
     C     *IN42         IFEQ      *ON
     C                   MOVE      EMEMP#        CMEMP#
     C                   MOVE      EMDIV         CMDIV
     C                   ENDIF                                                  *IN42 IFEQ
     C*
     C                   EXCEPT    DTL1
     C                   END
     C*
     C                   MOVEL     EMNAME        XXEMNM
     C                   Z-ADD     EMDIV         XXDIV
     C*
     C*
     C*   GET EMP DTL1   RECORD FOR STATE REPORTING EMPLOYER
     C*
     C     ELKEY         CHAIN     PEL01                              96
     C     *IN96         IFEQ      '0'
     C*
     C*   IF CANCELLED, THEN LOAD THIS CX DATE TO ELPTDT IN THE CASE
     C*   THAT THE ACCOUNT HAS NOT YET BEEN AUDITED
     C*
     C     1             DO        5             X
     C     CD(X)         IFGT      ELPFDT
     C     CD(X)         ANDLT     ELPTDT
     C                   Z-ADD     CD(X)         ELPTDT
     C                   END
     C                   END
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X2PFDT
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X2PTDT
     C*
     C*   LOAD POLICY FROM DATE INTO ARRAY AS 1ST PAYROLL DATE
     C*
     C                   Z-ADD     ELPFDT        PDAT(1)
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
     C     XXANDT        IFGE      F1ENPO
     C     XXANDT        ORLE      ELPFDT
     C                   MOVE      '1'           *IN78
     C                   END
     C*
     C*  LOAD PAYROLL DATE & RATE ARRAYS
     C*
     C                   Z-ADD     2             X
     C     *IN78         IFEQ      '0'
     C*
     C     XXANDT        IFLT      ELPTDT
     C                   Z-ADD     XXANDT        PDAT(X)
     C                   ADD       1             X
     C                   END
     C*
     C                   END                                                      *IN78='0'
     C*
     C                   Z-ADD     ELPTDT        PDAT(X)
     C*
     C                   ELSE
     C*
     C*** Begin add *** 12/20/06 ***********************************
     C     XXMAIN        IFEQ      '0'
     C                   MOVE      '1'           XXMAIN
     C                   MOVE      'N'           EMSTRP
     C                   Z-ADD     0             XXDIV
     C                   GOTO      MAINDV
     C                   END
     C*** End add ***** 12/20/06 ***********************************
     C*
     C                   MOVEL     EMNAME        XXEMNM
     C                   MOVEL     MSG(3)        XXDESC
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD3
     C                   EXCEPT    HEAD2
     C                   MOVE      '0'           *INOF
     C                   END
     C                   ADD       1             XXCNT2
     C*
     C     *IN42         IFEQ      *ON
     C                   MOVE      EMEMP#        CMEMP#
     C                   MOVE      EMDIV         CMDIV
     C                   ENDIF                                                  *IN42 IFEQ
     C*
     C                   EXCEPT    DTL1
     C                   MOVE      '0'           *IN96
     C                   MOVE      *ZEROS        XXSEMP
     C  N50              GOTO      NEXTC
     C   50              GOTO      NEXTE
     C*
     C                   END                                                      ELKEY
     C*
     C                   ENDSR
     C*****************************************************************
     C*  "WORK"-SUBROUTINE TO ADD BLANK WORK RECORD FOR EMPLOYER      *
     C*****************************************************************
     C*
     C     WORK          BEGSR
     C*
     C                   Z-ADD     ELCO#         B7CO#
     C                   Z-ADD     ELFND         B7FND
     C                   Z-ADD     ELFYR         B7FYR
     C                   Z-ADD     ELEMP#        B7EMP#
     C                   Z-ADD     ELDIV         B7DIV
     C                   MOVEL     '06'          B7NCCI
     C                   Z-ADD     0             B7CLAS
     C                   MOVE      QQRPT         B7REP#
     C                   Z-ADD     0             B7MEDA
     C                   Z-ADD     0             B7INDA
     C                   Z-ADD     0             B7ALAE
     C                   MOVE      *BLANK        B7CATA
     C                   MOVE      XXSS#         B7SS#
     C                   MOVE      '1'           B7STAT
     C                   Z-ADD     0             B7#CLM
     C*
     C*  DECIDE ON THE POLICY FROM, TO DATES
     C*
     C                   Z-ADD     XXPFDT        B7ACCD
     C*
     C*   SUBTRACT ONE FROM THE POLICY TO DATE
     C*
     C                   Z-ADD     XXPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDATE
     C*
      **** BEGIN DEL **** 04/30/98 *********************************************
     C******************** Z-ADDXXDATE    SRDTI
     C******************** CALL 'SRCVTDAT'DATCHK
     C******************** MOVELSRDTO     XXDATE
      **** END   DEL **** 04/30/98 *********************************************
     C*
      **** BEGIN CHG **** 04/02/98 *********************************************
     C********** SRERR     IFEQ *BLANK
     C     SRERR         IFEQ      *OFF
      **** END   CHG **** 04/02/98 *********************************************
     C*
      **** BEGIN CHG **** 04/02/98 *********************************************
     C******************** MOVE XXDATE    QQDTE   8
     C                   MOVE      XXDATE        QQDTE             8 0
      **** END   CHG **** 04/02/98 *********************************************
     C                   Z-ADD     1             QQPST             5 0
     C                   MOVE      *BLANK        QQERR             1
     C                   MOVE      *BLANK        QQTERM            1
     C                   CALL      'SRPAST8'
     C                   PARM                    QQDTE
     C                   PARM                    QQPST
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C*
     C                   MOVE      QQDTE         XXDATE
     C     QQERR         IFEQ      '0'
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         B7EOPD
     C*
     C                   ELSE
     C                   Z-ADD     XXPTDT        B7EOPD
     C                   END
     C*
      **** BEGIN DEL **** 04/30/98 *********************************************
     C******************** ELSE
      **** END   DEL **** 04/30/98 *********************************************
     C                   END                                                    SRERR2 = ' '
     C     B7EOPD        IFLT      B7ACCD
     C                   Z-ADD     B7ACCD        B7EOPD
     C                   END
     C*
     C                   Z-ADD     ELPFDT        B7PFDT                         Policy Eff Date
     C                   MOVE      'N'           B7EXCS                         Not Excess Clm
     C*
     C                   ENDSR
     OQSYSPRT2  E            ROUTE          1 06
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
     O**********E            ROUTE          2
     O**********                                 15 'ROUTE TO:'
     O**********             UPUSNM              67
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           15 'ROUTE TO:'
     O                       UPUSNM              67
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
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
     O**********             UPOUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
      ****** BEG DLT ***** 01/28/10 **********************************
     O***     E  160           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
      ****** END DLT ***** 01/28/10 **********************************
      **** BEGIN CHG **** 09/19/97 ************************************
     O***********************************46 'VE DATA SOLUTIONS. INC.'
     O***********************************52 ', 1996'
      ****** BEG DLT ***** 01/28/10 **********************************
     O****                               47 'VE DATA SOLUTIONS. INC.,'
      ****** END DLT ***** 01/28/10 **********************************
      ****** BEG ADD ***** 01/28/10 **********************************
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
      **** END   CHG **** 09/19/97 ************************************
     O          E            HEAD1          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             119
     O                       DSDATE        Y    198
     O          E            HEAD1          2
     O                       Q1PGM               10
     O                       ORTITL             129
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
     O          E            HEAD1          2
      **** BEGIN CHG **** 09/19/97 ************************************
     O*********************************** 5 'FUND:'
     O************************ XXFND Z    9
     O                                            6 'GROUP:'
     O                       XXFND         Z     10
      **** END   CHG **** 09/19/97 ************************************
     O                       FNNAME              70
     O          E            HEAD2          2
      **** BEGIN CHG **** 09/19/97 ************************************
     O********************************** 10 'FUND YEAR:'
     O*************************XXFYR Z   14
     O                                           12 'POLICY YEAR:'
     O                       XXFYR         Z     16
      **** END   CHG **** 09/19/97 ************************************
     O          E            HEAD3          2
     O                                            8 'EMPLR'
     O                                           30 'EMPLOYER NAME'
     O                                           58 'POLICY DATES'
     O                                           70 'CLAIM'
     O                                           90 'CLAIMANT NAME'
     O                                          110 'ACC DT'
     O                                          126 'DESCRIPTION'
     O          E            DTL1           1
     O                       CMEMP#        Z      9
     O                       CMDIV         Z     15
     O              N95      XXEMNM              47
     O               95                          36 '*** NOT FOUND ***'
     O              N96      X2PFDT              57 '  /  /  '
     O              N96                          59 '-'
     O              N96      X2PTDT              68 '  /  /  '
     O               96                          67 '*** NOT FOUND ***'
     O                       CMCASE        Z     77
     O                       XXCLNM             109
     O                       XXACDT             116 '  /  /  '
     O                       XXDESC             181
     O          E            DTL2           1
     O                       ELEMP#        Z      9
     O                       ELDIV         Z     15
     O              N95      XXEMNM              47
     O               95                          36 '*** NOT FOUND ***'
     O              N96      X2PFDT              57 '  /  /  '
     O              N96                          59 '-'
     O              N96      X2PTDT              68 '  /  /  '
     O               96                          67 '*** NOT FOUND ***'
     O                       XXDESC             181
     O          E            TOTAL       2  2
     O                                           30 'TOTAL RECORDS LISTED'
      **** BEGIN CHG **** 09/19/97 ************************************
     O***********************************45 ' FOR FUND YEAR:'
     O************************ XXCNT24   53
     O                                           47 ' FOR POLICY YEAR:'
     O                       XXCNT2        4     55
      **** END   CHG **** 09/19/97 ************************************
     O          E            TOTAL          2
     O                                           33 'TOTAL RECORDS PROCESSED'
      **** BEGIN CHG **** 09/19/97 ************************************
     O********************************** 48 ' FOR FUND YEAR:'
     O************************ XXCNT34   57
     O                                           50 ' FOR POLICY YEAR:'
     O                       XXCNT3        4     59
      **** END   CHG **** 09/19/97 ************************************
     O          E            NORECS      2  1
     O                                           23 '*** NO RECORDS MEET THE'
     O                                           46 'SELECTION CRITERIA ***'
**
STATE RPT FLG='N' ON ALL DIVISIONS, DEFAULT TO MAIN DIV
EMPLOYER MASTER RECORD DOES NOT EXIST
EMPLOYER DETAIL RECORD DOES NOT EXIST
EXCESS CLM - ACCIDENT DATE FALLS OUTSIDE POLICY PERIOD
INVALID EMPLOYER POLICY TO DATE
NON EXCESS CLM - ACCIDENT DT FALLS OUTSIDE POLICY PERIOD
