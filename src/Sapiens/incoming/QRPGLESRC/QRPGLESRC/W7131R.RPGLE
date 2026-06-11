     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*******************************************************************
     H/TITLE W7131R - UNDERWRITERS REPORT
     H*******************************************************************
     H* REVISIONS:                                                      *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 04/21/09  C: 58 Support: WC102  67090                           *
     H*           Program fatal error when size of arrays CSM, CDS, and *
     H*           COMB is exceeded, due to increased history in file.   *
     H*           40 or more years times number of cause or part codes. *
     H*           Increase size from 999 to 5000.                       *
     H*                                                                 *
     H* 08/07/02  C:136 Support: WC114  32095                           *
     H*                 The division field is passing in zeros into     *
     H*                 the report, even when left blank. This is       *
     H*                 causing the report to err.                      *
     H*                                                                 *
     H* 12/15/99 C:33 Y2K#: 042/067/068                                 *
     H*              -LEFT JUSTIFY TITLE ON ROUTING PAGE AND CENTER IT  *
     H*               ON THE REPORT.                                    *
     H*              -PRINT '*ALL' IF ALL YEARS ARE SELECTED.           *
     H*              -YEAR SHOULD BE PRINTED AS 3.                      *
     H*                                                                 *
     H* 08/20/99 C:69 Support: 19283                                    *
     H*               Added logic to Seton LR when no records found, so *
     H*               won't cause a problem in Report Order/Sorting     *
     H*                                                                 *
     H* 07/09/98 C:35 SUPPORT WA033 12610                               *
     H*               PARM LIST WAS INCORRECT FOR DATE CONVERT,         *
     H*               CAUSING INJURY DATE AND RECEIVED DATE NOT TO      *
     H*               PRINT.                                            *
     H*                                                                 *
     H* 05/14/98 C:65 SUPPORT 11641                                     *
     H*              -DO NOT EXECUTE EXCPT'S ON CLOSED PRINT FILES.     *
     H*               THIS IS A FIX TO ITEM 11071 BELOW.                *
     H*                                                                 *
     H* 04/13/98 C:61 SUPPORT 11071                                     *
     H*              -Correct printing of FYR information               *
     H*              -Add 'No records meet selected criteria' message.  *
     H*                                                                 *
     H* 12/23/97 C:35 PROJECT WC577                                     *
     H*               SORT TOTALS FOR LOST TIME CLAIMS AND NO LOST TIME *
     H*               CLAIMS WERE NOT BEING SET BACK TO ZERO AFTER THE  *
     H*               BREAK.                                            *
     H*                                                                 *
     H* 12/19/97 C:61 PROJECT WC577                                     *
     H*               OUTPUT CHANGED TO MEET NEW STANDARDS              *
     H*                                                                 *
     H* 08/27/97 C:78 PROJECT WC577                                     *
     H*               OUTPUT CHANGED TO MEET NEW STANDRADS              *
     H*                                                                 *
     H* 08/12/97 C:78 PROJECT WC577                                     *
     H*               FIELDS SVEMP# AND SVDIV FORCED INTO SMALLER       *
     H*               FIELDS FOR OUTPUT PURPOSES.                       *
     H*                                                                 *
     H* 06/26/97 C:62 PROJECT WC710                                     *
     H*               COUNTY CODE NOW IS 4A, STATE + COUNTY.            *
     H*               ALTERNATE COMPANY NAME IF NOT BLANK.              *
     H*                                                                 *
     H* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  *
     H* !!! ADD NEW REVISIONS COMMENTS AT THE TOP OF THE REVISIONS !!!  *
     H* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  *
     H*                                                                 *
     H*******************************************************************
     F*
     FWXEFL     IF   E           K DISK
     FWXEFL1    IF   E           K DISK
     F                                     RENAME(PEF01:PEF02)
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWMCPL     IF   E           K DISK
     FWMREL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FQSYSPRT   O    F  196        PRINTER OFLIND(*INOF)
     F                                     USROPN
     FQSYSPRT1  O    F  196        PRINTER OFLIND(*INOA)
     F                                     USROPN
     F*

     D SEQ             S              1    DIM(3) CTDATA PERRCD(1) ASCEND       SORT TITLES
     D SEQN            S              8    DIM(3) ALT(SEQ)
     D* * * NATURE, PART, CAUSE ARRAYS
     D*
     D NA              S              2    DIM(9) ASCEND                        NATURE CODES
     D NAD             S             30    DIM(9)                               NATURE DESC.
     D PA              S              2    DIM(9) ASCEND                        PART CODES
     D PAD             S             30    DIM(9)                               PART DESC.
     D CA              S              2    DIM(9) ASCEND                        CAUSE CODES
     D CAD             S             30    DIM(9)                               CAUSE DESC.
     D*
     D*   CAUSE SUMMARY ARRAYS
     D*
     D***  BEGIN CHG  ***  04/21/09  *******************************************
     D CSM             S              5    DIM(5000)                            Yr/CAUSE CODE
     D CDS             S             81    DIM(5000)                            CAUSE TTL FIELDS
     D COMB            S             86    DIM(5000)                            COMBINATION ARRAY
     D*-------------------------------------------------------------------------
     D*
     D***  BEGIN CHG  ***  12/15/99  *********************************
     D**********          CSM       999  4               CAUSE CODE
     D**********          CSM       999  5               CAUSE CODE
     D***  END   CHG  ***  12/15/99  *********************************
     D**********          CDS       999 81               CAUSE TTL FIELDS
     D***  BEGIN CHG  ***  12/15/99  *********************************
     D**********          COMB      999 85               COMBINATION ARRAY
     D**********          COMB      999 86               COMBINATION ARRAY
     D***  END   CHG  ***  12/15/99  *********************************
     D***  END CHG  *****  04/21/09  *******************************************
     D*
     D*
     D***  BEGIN ADD  ***  12/15/99  *********************************
     D TITLE0          C                   CONST('Underwriter''s Repor-
     D                                     t')
     D TITLE1          C                   CONST('Underwriter''s Repor-
     D                                     t (Nature, Part and -
     D                                     Cause) Summary')
     D***  END   ADD  ***  12/15/99  *********************************
     D*
     D*
     D                 DS
     D  CMT                    1     20
     D                                     DIM(10)                              RESERVE TYPES
     D  EFRT01                 1      2
     D  EFRT02                 3      4
     D  EFRT03                 5      6
     D  EFRT04                 7      8
     D  EFRT05                 9     10
     D  EFRT06                11     12
     D  EFRT07                13     14
     D  EFRT08                15     16
     D  EFRT09                17     18
     D  EFRT10                19     20
     D                 DS
     D  CMA                    1     90  2
     D                                     DIM(10)                              PMT AMOUNT
     D  EFAR01                 1      9  2
     D  EFAR02                10     18  2
     D  EFAR03                19     27  2
     D  EFAR04                28     36  2
     D  EFAR05                37     45  2
     D  EFAR06                46     54  2
     D  EFAR07                55     63  2
     D  EFAR08                64     72  2
     D  EFAR09                73     81  2
     D  EFAR10                82     90  2
     D                 DS
     D  CMR                    1     90  2
     D                                     DIM(10)                              RESERVE AMOUNT
     D  EFRR01                 1      9  2
     D  EFRR02                10     18  2
     D  EFRR03                19     27  2
     D  EFRR04                28     36  2
     D  EFRR05                37     45  2
     D  EFRR06                46     54  2
     D  EFRR07                55     63  2
     D  EFRR08                64     72  2
     D  EFRR09                73     81  2
     D  EFRR10                82     90  2
     D*
     D*  DATA STRUCTURE FOR CDS ARRAY
     D*
     D                 DS
     D  XCDS                   1     81
     D  XXCNT                  1      5  0
     D  XXNLTP                 6     16  2
     D  XXNLTO                17     27  2
     D  XXNLTI                28     38  2
     D  XXLTP                 39     49  2
     D  XXLTO                 50     60  2
     D  XXLTI                 61     71  2
     D  XXLTC                 72     76  0
     D  XXNLC                 77     81  0
     D*
     D*  DATA STRUCTURE FOR COMBINATION (CSM & CDS) ARRAY
     D*
     D***  BEGIN CHG  ***  12/15/99  *********************************
     D**********  DS
     D**********                              1  85 XCOMB
     D**********                              1   2 XYR
     D**********                              3   4 XCODE
     D**********                              5   90XCNT
     D**********                             10  202XNLTP
     D**********                             21  312XNLTO
     D**********                             32  422XNLTI
     D**********                             43  532XLTP
     D**********                             54  642XLTO
     D**********                             65  752XLTI
     D**********                             76  800XLTC
     D**********                             81  850XNLC
     D                 DS
     D  XCOMB                  1     86
     D  XYR                    1      3
     D  XCODE                  4      5
     D  XCNT                   6     10  0
     D  XNLTP                 11     21  2
     D  XNLTO                 22     32  2
     D  XNLTI                 33     43  2
     D  XLTP                  44     54  2
     D  XLTO                  55     65  2
     D  XLTI                  66     76  2
     D  XLTC                  77     81  0
     D  XNLC                  82     86  0
     D***  END   CHG  ***  12/15/99  *********************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND             3 0
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRES            10
     C*
     C*   KEY LIST FOR CHAPTER FILE
     C*
     C     CPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXCHAP            3 0
     C*
     C*   KEY LIST FOR REGION FILE
     C*
     C     REKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXRGN#            3 0
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*   ENTRY PARMS
     C*
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMDT          XXDATE
     C     *LIKE         DEFINE    FNANAM        XXCONM
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQDTE             8
     C                   PARM                    QQQUE            10            MLS 1/15/88
     C                   PARM                    QQMODE            1
     C                   PARM                    QQPRNT            1            JJJ 1-13-89
     C                   PARM                    QQSEQ             1            JJJ 3-30-89
     C                   PARM                    QQSSRT            1            JJJ 3-30-89
     C                   PARM                    QQFYR             3
     C                   PARM                    QQSELC            1
     C                   PARM                    QQSELO            1
     C                   PARM                    QQMTH             3
     C*
     C***  BEGIN ADD  ***  12/15/99  *********************************
     C     CNTRPL        PLIST
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C***  END   ADD  ***  12/15/99  *********************************
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
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
      **** BEGIN CHG **** 07/09/98 *********************************************
     C******************** PARM '*JOB'    SRTFMT
     C                   PARM      '*MDY'        SRTFMT
      **** END   CHG **** 07/09/98 *********************************************
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C**
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQDTE         X1DATE            8 0
     C                   MOVE      QQMODE        XXMODE            1
     C                   TIME                    DSDTTM
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         DSDATE
     C     XXEMP#        COMP      0                                      87
     C  N87EMKEY         CHAIN     PEM01                              60
      *** BEG ADD **** 08/07/02 ************************************************
     C     QQDIV         COMP      *BLANKS                                89
      *** END ADD **** 08/07/02 ************************************************
     C*
     C     QQSSRT        COMP      '1'                                    33
     C     QQSSRT        COMP      '2'                                    34
     C                   MOVE      '1'           *IN35
     C     QQSEQ         IFEQ      'C'
     C     QQSEQ         OREQ      'N'
     C                   MOVE      '0'           *IN35
     C                   END
     C*                                                    JJJ 1-13-89
     C*  OPEN PRINT FILE(S) ACCORDING TO SELECTION         |
     C*                                                    |
     C     QQPRNT        IFEQ      'S'                                          |SUMMARY
     C     QQPRNT        OREQ      'B'                                          |
     C                   OPEN      QSYSPRT1                                     |
     C                   MOVE      '1'           *IN95                          |
     C                   END                                                    |
     C*                                                    |
     C     QQPRNT        IFEQ      'D'                                          |DETAIL
     C     QQPRNT        OREQ      'B'                                          |
     C                   OPEN      QSYSPRT                                      |
     C                   MOVE      '1'           *IN96                          |
     C                   END                                                    |
      ***** BEG ADD ******* 04/13/98 **********************************
     C                   EXSR      SR1ST
      ***** END ADD ******* 04/13/98 **********************************
     C*
     C*  START READING CLAIM WORK FILE
     C*
     C   35              READ      PEF01                                  60
     C  N35              READ      PEF02                                  60
      ***** BEG ADD ******* 04/13/98 **********************************
     C   60              SETON                                        OFOA
     C*** BEGIN CHG ***  05/14/98  ************************************
     C*****    60          EXCPTNOREC
     C   96
     CAN 60              EXCEPT    NOREC
     C   95
     CAN 60              EXCEPT    NOREC1
     C*** END CHG *****  05/14/98  ************************************
     C*** Begin add ******* 08/20/98 **********************************
     C   60              SETON                                        LR
     C*** End   add ******* 08/20/98 **********************************
     C   60              RETURN
      ***** END ADD ******* 04/13/98 **********************************
     C     *IN60         DOWEQ     '0'
     C*
     C* FIRST CYCLE PROCESSING
     C*
     C  N11              EXSR      SR1ST
     C*
     C*   CHECK FOR FUND YEAR BREAK
     C*
     C     EFFYR         IFNE      XXFYR
     C*
     C                   EXSR      PRTBRK
     C                   EXSR      NATBRK
     C   35              EXSR      SRTBRK
     C                   EXSR      YRBRK
     C                   Z-ADD     EFFYR         XXFYR
     C                   MOVE      EFSORT        XXSORT
     C                   MOVE      EFNATR        HNATR
     C   33              MOVE      EFPART        HPART
     C   34              MOVE      EFCAUS        HPART
     C                   END
     C*
     C*   CHECK FOR SORT SELECTION BREAK - IF NECESSARY
     C*
     C     *IN35         IFEQ      '1'
     C     EFSORT        ANDNE     XXSORT
     C*
     C                   EXSR      PRTBRK
     C                   EXSR      NATBRK
     C                   EXSR      SRTBRK
     C                   MOVE      EFSORT        XXSORT
     C                   MOVE      EFNATR        HNATR
     C   33              MOVE      EFPART        HPART
     C   34              MOVE      EFCAUS        HPART
     C                   END
     C*
     C*   CHECK FOR NATURE BREAK
     C*
     C     EFNATR        IFNE      HNATR
     C     HNATR         ANDNE     *BLANKS
     C*
     C                   EXSR      PRTBRK
     C                   EXSR      NATBRK
     C                   MOVE      EFNATR        HNATR
     C   33              MOVE      EFPART        HPART
     C   34              MOVE      EFCAUS        HPART
     C                   END
     C*
     C*   CHECK FOR PART(CAUSE) BREAK
     C*
     C   33              MOVE      EFPART        XXPTCS            2
     C   34              MOVE      EFCAUS        XXPTCS
     C     XXPTCS        IFNE      HPART
     C     HPART         ANDNE     *BLANKS
     C*
     C                   EXSR      PRTBRK
     C   33              MOVE      EFPART        HPART
     C   34              MOVE      EFCAUS        HPART
     C                   END
     C*
     C*
     C*  ACCUMULATE PAYMENTS & INCURRED AMOUNTS
     C*
     C                   Z-ADD     0             COMP             11 2
     C                   Z-ADD     0             MED              11 2
     C                   Z-ADD     0             OTHR             11 2
     C                   Z-ADD     0             TOTALP           11 2
     C                   Z-ADD     0             TOTINC           11 2
     C                   Z-ADD     0             OUTRES           11 2
     C*
     C                   Z-ADD     1             R                 2 0
     C                   DO        10            R                 2 0
     C*
     C*  GET RESERVE TYPE CODE FILE
     C                   MOVEA     '00'          *IN(20)
     C                   MOVEL     CMT(R)        XXRES
     C     TBKEY         CHAIN     PRT01                              60
     C     *IN60         IFEQ      '0'
     C     RTTYPE        COMP      'C'                                    20
     C     RTTYPE        COMP      'M'                                    21
     C                   END
     C   20              ADD       CMA(R)        COMP             11 2
     C   21              ADD       CMA(R)        MED              11 2
     C  N20
     CANN21              ADD       CMA(R)        OTHR             11 2
     C                   ADD       CMR(R)        OUTRES           11 2
     C*
     C                   END
     C*
     C     COMP          ADD       MED           TOTALP           11 2
     C                   ADD       OTHR          TOTALP
     C     TOTALP        ADD       OUTRES        TOTINC           11 2
     C*
     C*   MOVE INFO TO BE PRINTED & ACCUMULATE ALL TOTALS
     C*
     C     EFPRNT        IFEQ      'Y'
     C*
     C                   MOVEL     EFSS#         SS4               4
     C                   MOVE      '-'           SS4
     C                   MOVE      EFSS#         SS6               6
     C                   MOVEL     SS6           SS2               2
     C                   MOVE      SS6           SS5               5
     C                   MOVEL     '-'           SS5
     C                   Z-ADD     EFACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         ACCDTE            6 0
     C                   Z-ADD     EFRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         RECDTE            6 0
     C                   MOVE      *BLANKS       CNAME            15
     C                   MOVEL     EFCLNM        CNAME
     C     EFNATR        IFEQ      *BLANKS
     C                   MOVE      '**'          XXNATR            2
     C                   ELSE
     C                   MOVE      EFNATR        XXNATR
     C                   END
     C     XXNATR        LOOKUP    NA                                     31
     C     *IN31         IFEQ      '0'
     C     X             IFLT      9
     C                   ADD       1             X
     C                   MOVEL     XXNATR        NA(X)
     C                   ELSE
     C                   SETON                                        81  88
     C                   END
     C                   END
     C**
     C     EFPART        IFEQ      *BLANKS
     C                   MOVE      '**'          XXPART            2
     C                   ELSE
     C                   MOVE      EFPART        XXPART
     C                   END
     C     XXPART        LOOKUP    PA                                     31
     C     *IN31         IFEQ      '0'
     C     Y             IFLT      9
     C                   ADD       1             Y
     C                   MOVEL     XXPART        PA(Y)
     C                   ELSE
     C                   SETON                                        82  88
     C                   END
     C                   END
     C**
     C     EFCAUS        IFEQ      *BLANKS
     C                   MOVE      '**'          XXCAUS            2
     C                   ELSE
     C                   MOVE      EFCAUS        XXCAUS
     C                   END
     C     XXCAUS        LOOKUP    CA                                     31
     C     *IN31         IFEQ      '0'
     C     Z             IFLT      9
     C                   ADD       1             Z
     C                   MOVEL     XXCAUS        CA(Z)
     C                   ELSE
     C                   SETON                                        83  88
     C                   END
     C                   END
     C*
     C*  ACCUMULATE OPEN & CLOSED TOTALS
     C*
     C     EFOPCL        IFEQ      'C'
     C                   ADD       1             L1CLS             5 0
     C                   ELSE
     C                   ADD       1             L1OPN             5 0
     C                   END
     C                   ADD       1             PARTL1            5 0
     C                   ADD       COMP          L1COMP           11 2
     C                   ADD       MED           L1MED            11 2
     C                   ADD       OTHR          L1OTHR           11 2
     C                   ADD       TOTALP        L1TOTL           11 2
     C                   ADD       TOTINC        L1TINC           11 2
     C                   ADD       OUTRES        L1ORES           11 2
     C                   ADD       TOTINC        L1TTLI           11 2
     C*** BEGIN DEL *** 05/13/97 **************************************
     C********** EFLTFL    IFEQ 'Y'
     C*** END   DEL *** 05/13/97 **************************************
     C*** BEGIN ADD *** 05/13/97 **************************************
     C     EFLTFL        IFEQ      'I'
     C*** END   ADD *** 05/13/97 **************************************
     C                   ADD       1             L1LTC             5 0
     C                   ADD       TOTALP        L1LTP            11 2          *LOSS PAID
     C                   ADD       OUTRES        L1LTO            11 2          *LOSS OUT RES
     C                   ADD       TOTINC        L1LTI            11 2          *LOSS INCURRED
     C                   ELSE
     C                   ADD       1             L1NLC             5 0
     C                   ADD       TOTALP        L1NLTP           11 2          *NOLOSS PAID
     C                   ADD       OUTRES        L1NLTO           11 2          *NOLOSS OUT RES
     C                   ADD       TOTINC        L1NLTI           11 2          *NOLOSS INCURRD
     C                   END
     C*
     C* EFEMP# AND EFDIV FORCED TO SMALLER FIELDS FOR OUTPUT PURPOSES
     C*
     ****** BEGIN ADD ******** 08/12/97 *******************************
     C                   Z-ADD     EFEMP#        EFEMP2            5 0
     C                   Z-ADD     EFDIV         EFDIV2            3 0
      **** END   ADD ***** 08/12/97 ***********************************
     C*
     C*
     C*  EXECUTE SUBROUTINE TO LOAD ARRAYS FOR CAUSE(PART) CODE SUMMARY
     C*
     C                   EXSR      CSUM
     C   22
     CAN 96              EXCEPT    HEAD
     C                   SETOFF                                       22
     C   96              EXCEPT    DETAIL
     C     *INOF         IFEQ      '1'
     C                   SETON                                        22
     C                   EXSR      SUB01
     C                   END
     C*
     C                   ELSE
     C*
     C     EFOPCL        IFEQ      'C'
     C                   ADD       1             LRCLS
     C                   ELSE
     C                   ADD       1             LROPN
     C                   END
     C                   ADD       COMP          LRCOMP           11 2
     C                   ADD       MED           LRMED            11 2
     C                   ADD       OTHR          LROTHR           11 2
     C                   ADD       TOTALP        LRTOTL           11 2
     C                   ADD       TOTINC        LRTINC           11 2
     C                   ADD       OUTRES        LRORES           11 2
     C                   ADD       TOTINC        LRTTLI           11 2
     C*** BEGIN DEL *** 05/13/97 **************************************
     C********** EFLTFL    IFEQ 'Y'
     C*** END   DEL *** 05/13/97 **************************************
     C*** BEGIN ADD *** 05/13/97 **************************************
     C     EFLTFL        IFEQ      'I'
     C*** END   ADD *** 05/13/97 **************************************
     C                   ADD       1             LRLTC             5 0
     C                   ADD       TOTALP        LRLTP            11 2
     C                   ADD       OUTRES        LRLTO            11 2                      S
     C                   ADD       TOTINC        LRLTI            11 2
     C                   ELSE
     C                   ADD       1             LRNLC             5 0
     C                   ADD       TOTALP        LRNLTP           11 2
     C                   ADD       OUTRES        LRNLTO           11 2
     C                   ADD       TOTINC        LRNLTI           11 2
     C                   END
     C*
     C                   END
     C*
     C   35              READ      PEF01                                  60
     C  N35              READ      PEF02                                  60
     C                   END
     C*
     C*   EXECUTE ALL BREAKS
     C*
     C                   EXSR      PRTBRK
     C                   EXSR      NATBRK
     C   35              EXSR      SRTBRK
     C                   EXSR      YRBRK
      ***** BEG ADD ******* 04/13/98 **********************************
     C     TERM          TAG
      ***** END ADD ******* 04/13/98 **********************************
     C*
     C                   MOVE      '1'           *INLR
     C*
     C   22
     CAN 96              EXCEPT    HEAD
     C   96              EXCEPT    TOTALS
     C   23
     CAN 95              EXCEPT    SUMHD
     C   95              EXCEPT    FNTTL
     C                   EXSR      CDONE
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found and (qqPrnt = 'D' or qqprnt = 'B');
          Except Route;
       Endif;
       xxPrt = '1';
       Chain orKey POR01;
       If %found and (qqPrnt = 'S' or qqprnt = 'B');
          Except Rout1;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     C*==================================================================*
     C*   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
     C*==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
     C     XXMODE        IFEQ      'D'                                          DEMAND MODE
     C                   SETON                                        09
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     COKEY         CHAIN     SMCOL                              60
     C     FNKEY         CHAIN     WMFNL                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XFNAME           60
     C                   ELSE
     C                   MOVE      *BLANKS       XFNAME           60
     C                   END
     C*** BEGIN ADD *** 06/26/97 **************************************
     C     FNANAM        IFNE      *BLANKS
     C*
     C                   MOVEL     FNANAM        XXCONM
     C*
     C                   ELSE
     C*
     C                   MOVEL     CONAME        XXCONM
     C*
     C                   ENDIF                                                  FNANAM
     C*
     C                   MOVEL(P)  XXCONM        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      50            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XXCONM
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*** END   ADD *** 06/26/97 **************************************
     C***  BEGIN ADD  ***  12/15/99  *********************************
     C                   MOVE      QQFYR         XXFYRR            3 0
     C     XXFYRR        COMP      *ZEROS                                 28
     C*
     C                   CLEAR                   ORTITL
     C***  END   ADD  ***  12/15/99  *********************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION FOR PRINTER FILE 0
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C*
     C***  BEGIN CHG  ***  12/15/99  *********************************
     C********** *IN60     IFEQ '0'
     C**********           MOVE *BLANKS   XXTITL 60
     C**********           MOVELORTITL    XXTITL
     C******96**           EXCPTROUTE
     C**********           END
     C     ORTITL        IFNE      *BLANK
     C                   MOVEL(P)  ORTITL        ##DATA
     C                   EXSR      SRLEFT
     C                   ELSE
     C                   MOVEL(P)  TITLE0        ##DATA
     C                   ENDIF                                                  ORTITL<>*BLANK
     C*
     C                   MOVEL     ##DATA        XXTITL           60
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**N60
     C**********AN 96              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C*
     C                   EXSR      SRCNTR
     C*
     C                   MOVEL     ##DATA        XXTITL
     C*
     C                   CLEAR                   ORTITL
     C***  END   CHG  ***  12/15/99  *********************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION FOR PRINTER FILE 1
     C*
     C                   MOVE      '1'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C*
     C***  BEGIN CHG  ***  12/15/99  *********************************
     C********** *IN60     IFEQ '0'
     C**********           MOVE *BLANKS   X1TITL 60
     C**********           MOVELORTITL    X1TITL
     C******95**           EXCPTROUT1
     C**********           END
     C     ORTITL        IFNE      *BLANK
     C                   MOVEL(P)  ORTITL        ##DATA
     C                   EXSR      SRLEFT
     C                   ELSE
     C                   MOVEL(P)  TITLE1        ##DATA
     C                   ENDIF                                                  ORTITL<>*BLANK
     C*
     C                   MOVEL     ##DATA        X1TITL           60
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**N60
     C**********AN 95              EXCEPT    ROUT1
      ***** End Del ***** 08/26/14 ************************************
     C*
     C                   EXSR      SRCNTR
     C*
     C                   MOVEL     ##DATA        X1TITL
     C***  END   CHG  ***  12/15/99  *********************************
     C*
     C* * * INITIALIZE FIELDS
     C***  BEGIN DEL  ***  12/15/99  *********************************
     C**********           MOVE QQFYR     XXFYRR  30
     C********** XXFYRR    COMP *ZEROS                   28
     C***  END   DEL  ***  12/15/99  *********************************
      ***** BEG DEL ******* 04/13/98 **********************************
     C********** EFFYR     ADD  1900      XXYEAR  40
      ***** END DEL ******* 04/13/98 **********************************
      ***** BEG ADD ******* 04/13/98 **********************************
     C***  BEGIN CHG  ***  12/15/99  *********************************
     C**********           Z-ADDEFFYR     XXYEAR  20
     C                   Z-ADD     EFFYR         XXYEAR            3 0
     C***  END   CHG  ***  12/15/99  *********************************
      ***** END ADD ******* 04/13/98 **********************************
     C                   Z-ADD     *ZERO         X                 1 0
     C                   Z-ADD     *ZERO         Y                 1 0
     C                   Z-ADD     *ZERO         Z                 1 0
     C                   MOVE      *BLANKS       CSM
     C                   MOVE      *BLANKS       CDS
     C                   MOVE      *BLANKS       COMB
     C                   MOVE      *ZEROS        XCDS
     C                   MOVE      *ZEROS        XCOMB
     C                   MOVE      *BLANKS       XXSORT           50
     C                   MOVE      *BLANKS       HPART             2
     C                   MOVE      *BLANKS       HNATR             2
     C                   Z-ADD     EFFYR         XXFYR             3 0
     C                   MOVEL     EFSORT        XXSORT
     C                   MOVE      EFNATR        HNATR
     C   33              MOVE      EFPART        HPART
     C   34              MOVE      EFCAUS        HPART
     C                   MOVE      '1'           *IN11
     C                   MOVE      '1'           *IN22
     C                   MOVE      '1'           *IN23
     C                   MOVE      '1'           *IN24
     C*
     C*  CHAIN TO PART(CAUSE) & NATURE CODE FILE TO RETRIEVE DESC
     C*
     C                   MOVE      *BLANKS       XXRES
     C                   MOVEL     EFPART        XXRES
     C     *IN33         IFEQ      '1'
     C     TBKEY         CHAIN     PPA01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     PADESC        HPRTD            30
     C                   ELSE
     C                   MOVE      *BLANKS       HPRTD
     C                   END
     C                   END
     C*
     C                   MOVE      *BLANKS       XXRES
     C                   MOVEL     EFCAUS        XXRES
     C     *IN34         IFEQ      '1'
     C     TBKEY         CHAIN     PCA01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CADESC        HPRTD            30
     C                   ELSE
     C                   MOVE      *BLANKS       HPRTD
     C                   END
     C                   END
     C*
     C                   MOVE      *BLANKS       XXRES
     C                   MOVEL     EFNATR        XXRES
     C     TBKEY         CHAIN     PNA01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     NADESC        HNTRD            30
     C                   ELSE
     C                   MOVE      *BLANKS       HNTRD
     C                   END
     C*
     C*   GET SORT CODE & DESCRIPTION FOR HEADING
     C*
     C     *IN35         IFEQ      '1'
     C                   EXSR      SRTHD
     C                   END
     C*
     C                   ENDSR
     C*
     C***  BEGIN ADD  ***  12/15/99  *********************************
     C*===============================================================
     C* SRCNTR - CENTER FIELD
     C*===============================================================
     C*
     C     SRCNTR        BEGSR
     C*
     C                   CALL      'SRCENTER'    CNTRPL                 99
     C*
     C                   ENDSR                                                  SRCNTR
     C*
     C*===============================================================
     C* SRLEFT - LEFT JUSTIFY FIELD
     C*===============================================================
     C*
     C     SRLEFT        BEGSR
     C*
     C     ' '           CHECK     ##DATA        N                 3 0
     C*
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  ##DATA:N      ##DATA                         FIELD
     C                   ENDIF                                                  N<=60
     C*
     C                   ENDSR                                                  SRLEFT
     C***  END   ADD  ***  12/15/99  *********************************
     C*
     C*==================================================================*
     C*   GET SORT HEADING                                 ** S R T H D  *
     C*==================================================================*
     C*
     C     SRTHD         BEGSR
     C*
     C*** BEGIN CHG *** 06/26/97 **************************************
     C**********           MOVE *BLANKS   XXSHD  43
     C                   MOVE      *BLANKS       XXSHD            45
     C*** END   CHG *** 06/26/97 **************************************
     C                   Z-ADD     1             V
     C     QQSEQ         LOOKUP    SEQ(V)                                 60
     C     *IN60         IFEQ      '1'
     C*** BEGIN CHG *** 06/26/97 **************************************
     C**********           MOVELSEQN,V    XXSNAM 12
     C                   MOVEL     SEQN(V)       XXSNAM           13
     C*** END   CHG *** 06/26/97 **************************************
     C*
     C     QQSEQ         IFEQ      'H'
     C                   MOVEL     EFSORT        XXCHAP            3 0
     C                   MOVE      XXCHAP        XXSNAM
     C     CPKEY         CHAIN     PCP01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CPDESC        XXSHD
     C                   END
     C                   MOVEL     XXSNAM        XXSHD
     C                   END
     C*
     C     QQSEQ         IFEQ      'O'
     C*** BEGIN CHG *** 06/26/97 **************************************
     C**********           MOVELEFSORT    XXCNTY  2
     C**********           MOVE XXCNTY    XXSNAM
     C                   MOVE      EFSTCO        XXSNAM
     C                   MOVE      *BLANKS       XXRES
     C**********           MOVELEFSORT    XXRES
     C                   MOVEL     EFSTCO        XXRES
     C*** END   CHG *** 06/26/97 **************************************
     C     TBKEY         CHAIN     PCN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CNDESC        XXSHD
     C                   END
     C                   MOVEL     XXSNAM        XXSHD
     C                   END
     C*
     C     QQSEQ         IFEQ      'R'
     C                   MOVEL     EFSORT        XXRGN#            3 0
     C                   MOVE      XXRGN#        XXSNAM
     C     REKEY         CHAIN     PRE01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      REDESC        XXSHD
     C                   END
     C                   MOVEL     XXSNAM        XXSHD
     C                   END
     C*
     C                   END
     C*
     C                   ENDSR
     C*
     C*==================================================================*
     C*   Print arrays / bottom of each page Subroutine    ** S U B 0 1  *
     C*==================================================================*
     C*
     C     SUB01         BEGSR
     C*
     C     NA(1)         IFNE      *BLANK
     C     PA(1)         ORNE      *BLANK
     C     CA(1)         ORNE      *BLANK
     C*
     C                   SORTA     NA
     C                   SORTA     PA
     C                   SORTA     CA
     C                   MOVE      '0'           *IN41
     C                   MOVE      '0'           *IN42
     C                   MOVE      '0'           *IN43
     C   96              EXCEPT    TOTALH
     C                   Z-ADD     1             K                 2 0
     C                   Z-ADD     1             L                 2 0
     C                   Z-ADD     1             M                 2 0
     C*
     C     *BLANKS       LOOKUP    NA(K)                              55
     C  N55              Z-ADD     10            K
     C     *BLANKS       LOOKUP    PA(L)                              55
     C  N55              Z-ADD     10            L
     C     *BLANKS       LOOKUP    CA(M)                              55
     C  N55              Z-ADD     10            M
     C*
     C     *IN41         DOWEQ     '0'
     C     *IN42         OREQ      '0'
     C     *IN43         OREQ      '0'
     C*
     C     K             IFLE      9
     C                   MOVEL     NA(K)         XXRES
     C     TBKEY         CHAIN     PNA01                              60
     C                   MOVEL     NADESC        NAD(K)
     C                   ELSE
     C                   MOVE      '1'           *IN41
     C                   END
     C*
     C     L             IFLE      9
     C                   MOVEL     PA(L)         XXRES
     C     TBKEY         CHAIN     PPA01                              60
     C                   MOVE      PADESC        PAD(L)
     C                   ELSE
     C                   MOVE      '1'           *IN42
     C                   END
     C*
     C     M             IFLE      9
     C                   MOVEL     CA(M)         XXRES
     C     TBKEY         CHAIN     PCA01                              60
     C                   MOVE      CADESC        CAD(M)
     C                   ELSE
     C                   MOVE      '1'           *IN43
     C                   END
     C*
     C   96              EXCEPT    TOTAL
     C                   ADD       1             K
     C                   ADD       1             L
     C                   ADD       1             M
     C*
     C                   END
     C                   END
     C*
     C   96              EXCEPT    TOTALE
     C                   Z-ADD     *ZERO         R
     C                   Z-ADD     *ZERO         X
     C                   Z-ADD     *ZERO         Y
     C                   Z-ADD     *ZERO         Z
     C                   MOVE      '0'           *IN88
     C                   MOVE      '0'           *INOF
     C                   MOVE      *BLANK        NA
     C                   MOVE      *BLANK        PA
     C                   MOVE      *BLANK        CA
     C                   MOVE      *BLANK        NAD
     C                   MOVE      *BLANK        PAD
     C                   MOVE      *BLANK        CAD
     C                   ENDSR
     C*==================================================================*
     C*   Load Arrays for Cause Code Summary   Subroutine  ** CSUM    =  *
     C*==================================================================*
     C*
     C     CSUM          BEGSR
     C*
     C     *IN33         IFEQ      '1'
     C     EFCAUS        ANDEQ     *BLANKS
     C                   MOVE      '**'          EFCAUS
     C                   END
     C*
     C     *IN34         IFEQ      '1'
     C     EFPART        ANDEQ     *BLANKS
     C                   MOVE      '**'          EFPART
     C                   END
     C*
     C***  BEGIN CHG  ***  12/15/99  *********************************
     C**********           Z-ADDEFFYR     XXYR    20
     C**********           MOVE *BLANKS   XXYRCA  4
     C                   Z-ADD     EFFYR         XXYR              3 0
     C                   MOVE      *BLANKS       XXYRCA            5
     C***  END   CHG  ***  12/15/99  *********************************
     C                   MOVEL     XXYR          XXYRCA
     C   33              MOVE      EFCAUS        XXYRCA
     C   34              MOVE      EFPART        XXYRCA
     C*
     C*  CHECK TO SEE IF CODE IS ALREADY IN ARRAY
     C*
     C                   Z-ADD     1             V                 5 0
     C     XXYRCA        LOOKUP    CSM(V)                                 38
     C     *IN38         IFEQ      '0'                                          NEW CODE-ADD
     C*
     C*  FIND NEXT AVAILABLE POSITION & ADD FIELDS TO THE ARRAY
     C*
     C                   Z-ADD     1             V
     C     CSM(V)        DOWNE     *BLANKS
     C                   ADD       1             V
     C                   END
     C                   MOVE      XXYRCA        CSM(V)
     C                   MOVE      *ZEROS        XCDS
     C                   Z-ADD     1             XXCNT                          COUNTER
     C*
     C                   ELSE
     C*
     C                   MOVE      *ZEROS        XCDS
     C                   MOVEA     CDS(V)        XCDS
     C                   ADD       1             XXCNT
     C*
     C                   END
     C*
     C*** BEGIN DEL *** 05/13/97 **************************************
     C********** EFLTFL    IFEQ 'Y'
     C*** END   DEL *** 05/13/97 **************************************
     C*** BEGIN ADD *** 05/13/97 **************************************
     C     EFLTFL        IFEQ      'I'
     C*** END   ADD *** 05/13/97 **************************************
     C                   ADD       1             XXLTC                          *LOST TIME
     C                   ADD       TOTALP        XXLTP            11 2          *LOSS PAID
     C                   ADD       OUTRES        XXLTO            11 2          *LOSS OUT RES
     C                   ADD       TOTINC        XXLTI            11 2          *LOSS INCURRED
     C                   ELSE                                                   *
     C                   ADD       1             XXNLC                          *NO LOST TIM
     C                   ADD       TOTALP        XXNLTP           11 2          *NOLOSS PAID
     C                   ADD       OUTRES        XXNLTO           11 2          *NOLOSS OUT RES
     C                   ADD       TOTINC        XXNLTI           11 2          *NOLOSS INCURRD
     C                   END                                                    *
     C*                                                    *
     C                   MOVEA     XCDS          CDS(V)
     C*
     C                   ENDSR
     C*
     C*==================================================================*
     C*   PRINT CAUSE(PART) CODE SUMMARY       Subroutine  ** CDONE   =  *
     C*==================================================================*
     C*
     C     CDONE         BEGSR
     C*
     C*  SET INDICATOR ON TO PRINT CAUSE CODE SUMMARY HEADING
     C*
     C                   MOVE      '1'           *IN39
     C*
     C* SET INDICATOR ON TO PRINT FUNDYEAR HEADING FOR CAUSE-SUMMARY
     C*                                                    LJQ 03-22-88
     C                   MOVE      '1'           *IN24                          *
     C*
     C* MOVE CODE AND TOTAL ARRAYS TO COMBINATION ARRAY
     C*
     C                   Z-ADD     1             V
     C     CSM(V)        DOWNE     *BLANKS
     C                   MOVEL     CSM(V)        COMB(V)
     C                   MOVE      CDS(V)        COMB(V)
     C                   ADD       1             V
     C                   END
     C*
     C* MOVE COMBINATION ARRAY TO OUTPUT FIELDS AFTER RETRIEVING CAUSE DES
     C*
     C   95              EXCEPT    SUMHD
     C*
     C*  SORT THE ARRAY
     C*
     C                   SORTA     COMB
     C*
     C*  INITIALIZE FUND YEAR HOLDING FIELD-USED TO TRIGGER PRINTING
     C*  OF YEARLY TOTALS                                  LJQ 03-22-88
     C*                                                    *
     C***  BEGIN CHG  ***  12/15/99  *********************************
     C**********           MOVE *BLANKS   HYR     2        *
     C                   MOVE      *BLANKS       HYR               3
     C***  END   CHG  ***  12/15/99  *********************************
     C*
     C*  FIND THE FIRST RECORD
     C*
     C                   Z-ADD     1             X1                5 0
     C     COMB(X1)      DOWEQ     *BLANKS
     C                   ADD       1             X1
     C     X1            IFGT      999
     C                   GOTO      BYIT
     C                   END
     C                   END
     C*
     C     BYIT          TAG
     C     X1            DOWLE     999
     C                   MOVE      *BLANKS       XCOMB
     C                   MOVEA     COMB(X1)      XCOMB
     C*
     C*  IF WE HAVE A NEW FUND YEAR, PRINT PRIOR FUND YEAR'S TOTALS
     C*                                                    LJQ 03-22-88
     C     XYR           IFNE      HYR                                          *
     C     HYR           ANDNE     *BLANKS                                      *
     C*                                                    *
     C     *INOA         IFEQ      '1'                                          *
     C   95              EXCEPT    SUMHD                                        *
     C                   END                                                    *
     C   95              EXCEPT    YRTTL                                        *
     C                   MOVE      '1'           *IN24                          *
     C*                                                    *
     C* INITIALIZE YEAR TOTALS                             *
     C*                                                    *
     C                   Z-ADD     0             L3TCAS                         *
     C                   Z-ADD     0             L3LTC
     C                   Z-ADD     0             L3NLC
     C                   Z-ADD     0             L3NLTP           11 2
     C                   Z-ADD     0             L3NLTO           11 2
     C                   Z-ADD     0             L3NLTI           11 2
     C                   Z-ADD     0             L3LTP            11 2
     C                   Z-ADD     0             L3LTO            11 2
     C                   Z-ADD     0             L3LTI            11 2
     C                   Z-ADD     0             L3TTLI           11 2
     C*                                                    *
     C                   END                                                    *
     C*
     C*  RETRIEVE CAUSE CODE DESCRIPTION
     C*
     C                   MOVE      *BLANKS       XXRES
     C                   MOVE      *BLANKS       XXDESC           30
     C*
     C                   MOVE      XCODE         XXCD              2
     C                   MOVEL     XXCD          XXRES
     C   33TBKEY         CHAIN     PCA01                              60
     C   33              MOVE      CADESC        XXDESC
     C   34TBKEY         CHAIN     PPA01                              60
     C   34              MOVE      PADESC        XXDESC
     C*                                                    LJQ 03-22-88
     C* CHECK FOR PAGE OVERFLOW                            *
     C*                                                    *
     C     *INOA         IFEQ      '1'                                          *
     C   95              EXCEPT    SUMHD                                        *
     C                   MOVE      '1'           *IN24                          *
     C                   END                                                    *
     C*                                                    *
     C* ADD TO TOTAL INCURRED BUCKET AND FUND YEAR TOTALS  *
     C*                                                    LJQ 03-17-88
     C                   ADD       XLTI          XXTTLI           11 2          *
     C                   ADD       XNLTI         XXTTLI                         *
     C                   ADD       XCNT          L3TCAS                         *TOTAL CASES
     C                   ADD       XNLTP         L3NLTP                         *NOLOSS PAID
     C                   ADD       XNLTO         L3NLTO                         *NOLOSS OUT RES
     C                   ADD       XNLTI         L3NLTI                         *NOLOSS INCURRD
     C                   ADD       XLTP          L3LTP                          *LOSS PAID
     C                   ADD       XLTO          L3LTO                          *LOSS OUT RES
     C                   ADD       XLTI          L3LTI                          *LOSS INCURRED
     C                   ADD       XXTTLI        L3TTLI                         *TOTAL INCURRED
     C                   ADD       XLTC          L3LTC
     C                   ADD       XNLC          L3NLC
     C                   MOVE      XYR           HYR                            *
     C*                                                    *
     C* CHECK TO SEE IF FUND YEAR HEADING HAS BEEN PRINTED *
     C*                                                    *
     C     *IN24         IFEQ      '1'                                          *
     C   95              EXCEPT    YRHEAD                                       *
     C                   MOVE      '0'           *IN24                          *
     C                   END                                                    *
     C*
     C*  PRINT DETAIL LINE
     C*
     C   95              EXCEPT    CASDT
     C*
     C                   Z-ADD     0             XXTTLI
     C                   ADD       1             X1
     C                   END
     C*
     C   95              EXCEPT    YRTTL                                                    8
     C   95              EXCEPT    FNTTL
     C*
     C                   ENDSR
     C*
     C*
     C*==================================================================*
     C*   PART OR CAUSE BREAK                  Subroutine  ** PRTBRK     *
     C*==================================================================*
     C     PRTBRK        BEGSR
     C*
     C*  ACCUMULATE NATURE TOTALS
     C*
     C                   ADD       L1COMP        L2COMP           11 2
     C                   ADD       L1MED         L2MED            11 2
     C                   ADD       L1OTHR        L2OTHR           11 2
     C                   ADD       L1TOTL        L2TOTL           11 2
     C                   ADD       L1TINC        L2TINC           11 2
     C                   ADD       L1ORES        L2ORES           11 2
     C     L1OPN         ADD       L1CLS         L1TCAS            5 0
     C                   ADD       L1TCAS        L2TCAS            5 0
     C                   ADD       L1OPN         L2OPN             5 0
     C                   ADD       L1CLS         L2CLS             5 0
     C*
     C                   ADD       L1NLTP        L2NLTP           11 2          *NOLOSS PAID
     C                   ADD       L1NLTO        L2NLTO           11 2          *NOLOSS OUT RES
     C                   ADD       L1NLTI        L2NLTI           11 2          *NOLOSS INCURRD
     C                   ADD       L1LTP         L2LTP            11 2          *LOSS PAID
     C                   ADD       L1LTO         L2LTO            11 2          *LOSS OUT RES
     C                   ADD       L1LTI         L2LTI            11 2          *LOSS INCURRED
     C                   ADD       L1TTLI        L2TTLI           11 2          *TOTAL INCURRED
     C                   ADD       L1LTC         L2LTC             5 0
     C                   ADD       L1NLC         L2NLC             5 0
     C*
     C                   ADD       PARTL1        NATRL1            5 0
     C*
     C* IF THERE ARE ANY TOTALS FOR PART(OR CAUSE) CODE PRINT TOTALS
     C*
     C     L1TCAS        IFGT      0
     C*
     C*  CHECK FOR PAGE OVERFLOW
     C*
     C*
     C     *IN22         IFEQ      '1'
     C     *INOF         OREQ      '1'
     C   96              EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   MOVE      '0'           *IN22
     C                   END
     C     *IN23         IFEQ      '1'
     C     *INOA         OREQ      '1'
     C   95              EXCEPT    SUMHD
     C                   MOVE      '0'           *INOA
     C                   MOVE      '0'           *IN23
     C                   END
     C*
     C   96              EXCEPT    TOTL1
     C*
     C*  CHECK TO SEE IF NATURE CODE HEADING HAS BEEN PRINTED BEFORE
     C*  PRINT PART CODE DETAIL
     C*
     C     *IN24         IFEQ      '1'
     C   95              EXCEPT    NATHD
     C                   MOVE      '0'           *IN24
     C                   END
     C   95              EXCEPT    PDETL
     C*
     C                   END
     C*
     C                   Z-ADD     0             PARTL1                         # OF CASES
     C                   Z-ADD     0             L1COMP                         COMPENSATION
     C                   Z-ADD     0             L1MED                          MEDICAL
     C                   Z-ADD     0             L1OTHR                         OTHER
     C                   Z-ADD     0             L1TOTL                         TOTAL PAID
     C                   Z-ADD     0             L1TINC                         TOTAL INCURRED
     C                   Z-ADD     0             L1ORES                         OUT RES
     C                   Z-ADD     0             L1NLTP                         *NOLOSS PAID
     C                   Z-ADD     0             L1NLTO                         *NOLOSS OUT RES
     C                   Z-ADD     0             L1NLTI                         *NOLOSS INCURRD
     C                   Z-ADD     0             L1LTP                          *LOSS PAID
     C                   Z-ADD     0             L1LTO                          *LOSS OUT RES
     C                   Z-ADD     0             L1LTI                          *LOSS INCURRED
     C                   Z-ADD     0             L1TTLI                         *TOTAL INCURRED
     C                   Z-ADD     0             L1OPN                          OPEN CASES
     C                   Z-ADD     0             L1CLS                          CLOSED CASES
     C                   Z-ADD     0             L1TCAS                         TOTAL CASES
     C                   Z-ADD     0             L1LTC                          LOST TIME
     C                   Z-ADD     0             L1NLC                          NO LOST TIME
     C*
     C*  CHAIN TO PART(CAUSE) FILE TO RETRIEVE DESC
     C*
     C                   MOVE      *BLANKS       XXRES
     C                   MOVEL     EFPART        XXRES
     C     *IN33         IFEQ      '1'
     C     TBKEY         CHAIN     PPA01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     PADESC        HPRTD            30
     C                   ELSE
     C                   MOVE      *BLANKS       HPRTD
     C                   END
     C                   END
     C*
     C                   MOVE      *BLANKS       XXRES
     C                   MOVEL     EFCAUS        XXRES
     C     *IN34         IFEQ      '1'
     C     TBKEY         CHAIN     PCA01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CADESC        HPRTD            30
     C                   ELSE
     C                   MOVE      *BLANKS       HPRTD
     C                   END
     C                   END
     C*
     C*
     C                   ENDSR
     C*==================================================================*
     C*   NATURE CODE BREAK                    Subroutine  ** NATBRK  =  *
     C*==================================================================*
     C     NATBRK        BEGSR
     C*
     C*  ACCUMULATE FYR TOTALS & SORT TOTALS
     C*
     C                   ADD       L2COMP        L3COMP           11 2
     C                   ADD       L2MED         L3MED            11 2
     C                   ADD       L2OTHR        L3OTHR           11 2
     C                   ADD       L2TOTL        L3TOTL           11 2
     C                   ADD       L2TINC        L3TINC           11 2
     C                   ADD       L2ORES        L3ORES           11 2
     C     L2OPN         ADD       L2CLS         L2TCAS            5 0
     C                   ADD       L2TCAS        L3TCAS            5 0
     C                   ADD       L2OPN         L3OPN             5 0
     C                   ADD       L2CLS         L3CLS             5 0
     C*
     C                   ADD       L2NLTP        L3NLTP                         *NOLOSS PAID
     C                   ADD       L2NLTO        L3NLTO                         *NOLOSS OUT RES
     C                   ADD       L2NLTI        L3NLTI                         *NOLOSS INCURRD
     C                   ADD       L2LTP         L3LTP                          *LOSS PAID
     C                   ADD       L2LTO         L3LTO                          *LOSS OUT RES
     C                   ADD       L2LTI         L3LTI                          *LOSS INCURRED
     C                   ADD       L2TTLI        L3TTLI                         *TOTAL INCURRED
     C                   ADD       L2LTC         L3LTC             5 0
     C                   ADD       L2NLC         L3NLC             5 0
     C*
     C                   ADD       L2COMP        STCOMP           11 2
     C                   ADD       L2MED         STMED            11 2
     C                   ADD       L2OTHR        STOTHR           11 2
     C                   ADD       L2TOTL        STTOTL           11 2
     C                   ADD       L2TINC        STTINC           11 2
     C                   ADD       L2ORES        STORES           11 2
     C                   ADD       L2TCAS        STTCAS            5 0
     C                   ADD       L2OPN         STOPN             5 0
     C                   ADD       L2CLS         STCLS             5 0
     C*
     C                   ADD       L2NLTP        STNLTP           11 2          *NOLOSS PAID
     C                   ADD       L2NLTO        STNLTO           11 2          *NOLOSS OUT RES
     C                   ADD       L2NLTI        STNLTI           11 2          *NOLOSS INCURRD
     C                   ADD       L2LTP         STLTP            11 2          *LOSS PAID
     C                   ADD       L2LTO         STLTO            11 2          *LOSS OUT RES
     C                   ADD       L2LTI         STLTI            11 2          *LOSS INCURRED
     C                   ADD       L2TTLI        STTTLI           11 2          *TOTAL INCURRED
     C                   ADD       L2LTC         STLTC             5 0
     C                   ADD       L2NLC         STNLC             5 0
     C*
     C*
     C* EFEMP# AND EFDIV FORCED TO SMALLER FIELDS FOR OUTPUT PURPOSES
     C*
     C*
     C*
     C* PRINT NATURE TOTALS IF THERE ARE ANY & SET INDICATOR TO PRINT
     C* THE NEXT NEW NATURE CODE
     C*
     C     L2TCAS        IFGT      0
     C*
     C*  CHECK FOR PAGE OVERFLOW
     C*
     C     *INOF         IFEQ      '1'
     C                   EXSR      SUB01
     C   96              EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C     *IN23         IFEQ      '1'
     C     *INOA         OREQ      '1'
     C   95              EXCEPT    SUMHD
     C                   MOVE      '0'           *INOA
     C                   MOVE      '0'           *IN23
     C                   END
     C*
     C   96              EXCEPT    TOTL2
     C   95              EXCEPT    NDETL
     C                   MOVE      '1'           *IN24
     C*
     C                   END
     C*
     C                   Z-ADD     0             NATRL1                         # OF CASES
     C                   Z-ADD     0             L2COMP                         COMPENSATION
     C                   Z-ADD     0             L2MED                          MEDICAL
     C                   Z-ADD     0             L2OTHR                         OTHER
     C                   Z-ADD     0             L2TOTL                         TOTAL PAID
     C                   Z-ADD     0             L2TINC                         TOTAL INCURRED
     C                   Z-ADD     0             L2ORES                         OUT RES
     C                   Z-ADD     0             L2NLTP                         *NOLOSS PAID
     C                   Z-ADD     0             L2NLTO                         *NOLOSS OUT RES
     C                   Z-ADD     0             L2NLTI                         *NOLOSS INCURRD
     C                   Z-ADD     0             L2LTP                          *LOSS PAID
     C                   Z-ADD     0             L2LTO                          *LOSS OUT RES
     C                   Z-ADD     0             L2LTI                          *LOSS INCURRED
     C                   Z-ADD     0             L2TTLI                         *TOTAL INCURRED
     C                   Z-ADD     0             L2OPN                          CASES OPEN
     C                   Z-ADD     0             L2CLS                          CASES CLOSED
     C                   Z-ADD     0             L2TCAS                         TOTAL CASES
     C                   Z-ADD     0             L2LTC                          LOST TIME
     C                   Z-ADD     0             L2NLC                          NO LOST TIME
     C*
     C*  CHAIN TO PART(CAUSE) FILE TO RETRIEVE DESC
     C*
     C                   MOVE      *BLANKS       XXRES
     C                   MOVEL     EFNATR        XXRES
     C     TBKEY         CHAIN     PNA01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     NADESC        HNTRD            30
     C                   ELSE
     C                   MOVE      *BLANKS       HNTRD
     C                   END
     C*
     C                   ENDSR
     C*==================================================================*
     C*   BREAK ON SELECTED SORT               Subroutine  ** SRTBRK  =  *
     C*==================================================================*
     C     SRTBRK        BEGSR
     C*
     C*   PRINT SORT TOTALS IF ANY
     C*
     C     STTCAS        IFGT      0
     C   96              EXCEPT    TOTST
     C   95              EXCEPT    STTTL
     C                   EXSR      SUB01
     C                   END
     C*
     C                   Z-ADD     0             STOPN                          OPEN CASES
     C                   Z-ADD     0             STCLS                          CLOSED CASES
     C                   Z-ADD     0             STTCAS                         TOTAL CASES
     C                   Z-ADD     0             STCOMP                         COMPENSATION
     C                   Z-ADD     0             STMED                          MEDICAL
     C                   Z-ADD     0             STOTHR                         OTHER
     C                   Z-ADD     0             STTOTL                         TOTAL PAID
     C                   Z-ADD     0             STTINC                         TOTAL INCURRED
     C                   Z-ADD     0             STORES                         OUT RES
     C                   Z-ADD     0             STNLTP                         *NOLOSS PAID
     C                   Z-ADD     0             STNLTO                         *NOLOSS OUT RES
     C                   Z-ADD     0             STNLTI                         *NOLOSS INCURRD
     C                   Z-ADD     0             STLTP                          *LOSS PAID
     C                   Z-ADD     0             STLTO                          *LOSS OUT RES
     C                   Z-ADD     0             STLTI                          *LOSS INCURRED
     C                   Z-ADD     0             STTTLI                         *TOTAL INCURRED
      **** BEGIN ADD **** 12/23/97 *********************************************
     C                   Z-ADD     0             STNLC                          *NO LOST TIME CLAIMS
     C                   Z-ADD     0             STLTC                          *LOST TIMR CLAIMS
      **** END   ADD **** 12/23/97 *********************************************
     C*
     C                   MOVE      '1'           *IN22
     C                   MOVE      '1'           *IN23
     C                   MOVE      '1'           *IN24
     C                   EXSR      SRTHD
     C*
     C                   ENDSR
     C*==================================================================*
     C*   FUND YEAR BREAK                      Subroutine  ** YRBRK   =  *
     C*==================================================================*
     C     YRBRK         BEGSR
     C*
     C                   ADD       L3COMP        LRCOMP           11 2
     C                   ADD       L3MED         LRMED            11 2
     C                   ADD       L3OTHR        LROTHR           11 2
     C                   ADD       L3TOTL        LRTOTL           11 2
     C                   ADD       L3TINC        LRTINC           11 2
     C                   ADD       L3ORES        LRORES           11 2
     C     L3OPN         ADD       L3CLS         L3TCAS            5 0
     C                   ADD       L3OPN         LROPN             5 0
     C                   ADD       L3CLS         LRCLS             5 0
     C     LROPN         ADD       LRCLS         LRTCAS            5 0
     C*
     C                   ADD       L3NLTP        LRNLTP           11 2          *NOLOSS PAID
     C                   ADD       L3NLTO        LRNLTO           11 2          *NOLOSS OUT RES
     C                   ADD       L3NLTI        LRNLTI           11 2          *NOLOSS INCURRD
     C                   ADD       L3LTP         LRLTP            11 2          *LOSS PAID
     C                   ADD       L3LTO         LRLTO            11 2          *LOSS OUT RES
     C                   ADD       L3LTI         LRLTI            11 2          *LOSS INCURRED
     C                   ADD       L3TTLI        LRTTLI           11 2          *TOTAL INCURRED
     C                   ADD       L3LTC         LRLTC             5 0          LOST TIME
     C                   ADD       L3NLC         LRNLC             5 0          NO LOST TIME
     C*
     C* PRINT YEAR TOTALS IF ANY
     C*
     C     L3TCAS        IFGT      0
     C                   MOVE      '1'           *IN36
     C   96
     CAN 35              EXCEPT    HEAD
     C                   MOVE      '0'           *IN36
     C   96              EXCEPT    TOTL3
     C   95              EXCEPT    YRTTL
     C  N35              EXSR      SUB01
     C                   END                                                    LJQ 5-4-88
     C*
      ***** BEG DEL ******* 04/13/98 **********************************
     C*********  EFFYR     ADD  1900      XXYEAR
      ***** END DEL ******* 04/13/98 **********************************
      ***** BEG ADD ******* 04/13/98 **********************************
     C                   Z-ADD     EFFYR         XXYEAR
      ***** END ADD ******* 04/13/98 **********************************
     C                   MOVE      '1'           *IN22
     C                   MOVE      '1'           *IN23
     C                   MOVE      '1'           *IN24
     C                   Z-ADD     0             L3OPN                          OPEN CASES
     C                   Z-ADD     0             L3CLS                          CLOSED CASES
     C                   Z-ADD     0             L3TCAS                         TOTAL CASES
     C                   Z-ADD     0             L3COMP                         COMPENSATION
     C                   Z-ADD     0             L3MED                          MEDICAL
     C                   Z-ADD     0             L3OTHR                         OTHER
     C                   Z-ADD     0             L3TOTL                         TOTAL PAID
     C                   Z-ADD     0             L3TINC                         TOTAL INCURRED
     C                   Z-ADD     0             L3ORES                         OUT RES
     C                   Z-ADD     0             L3NLTP                         *NOLOSS PAID
     C                   Z-ADD     0             L3NLTO                         *NOLOSS OUT RES
     C                   Z-ADD     0             L3NLTI                         *NOLOSS INCURRD
     C                   Z-ADD     0             L3LTP                          *LOSS PAID
     C                   Z-ADD     0             L3LTO                          *LOSS OUT RES
     C                   Z-ADD     0             L3LTI                          *LOSS INCURRED
     C                   Z-ADD     0             L3TTLI                         *TOTAL INCURRED
     C                   Z-ADD     0             L3LTC                          LOST TIME
     C                   Z-ADD     0             L3NLC                          NO LOST TIME
     C*
     C                   ENDSR
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
     O                       XXTITL              80
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
     O**********     08      OROUTQ              30
     O*  MLS 1/15/88 ADD OUT QUEUE AND INDICATORS
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O          E            ROUTE          2
      *** BEG CHG ***** 12/19/97 **************************************
     O********                           16 'FUND YEAR:'
     O                                           17 'GROUP YEAR:'
      *** END CHG ***** 12/19/97 **************************************
     O              N28      QQFYR               36
     O               28                          36 '*ALL'
     O*
     O          E            ROUTE          2
     O                                           15 'RUN DATE:'
     O                       DSDATE        Y     36
     O*
     O          E            ROUTE          2
     O                                           19 'EMPLOYER/DIV:'
     O              N87      XXEMP#              30
     O              N87                          31 '/'
      *** BEG CHG **** 08/07/02 ************************************************
     O*************** N87      XXDIV     36
     O***************  87                36 '*ALL'
     O              N87N89   XXDIV               36
     O               89                          36 '*ALL'
      *** END CHG **** 08/07/02 ************************************************
     O*
     O          E            ROUTE          2
     O                                           25 'SELECT OPEN CLAIMS:'
     O                       QQSELO              36
     O*
     O          E            ROUTE          2
     O                                           27 'SELECT CLOSED CLAIMS:'
     O                       QQSELC              36
     O*
     O          E            ROUTE          2
     O                                           18 '# OF MONTHS:'
     O                       QQMTH               36
     O*
     O          E            ROUTE          2
     O                                           20 'PRINT SUMMARY:'
     O                       QQPRNT              36
     O*
     O          E            ROUTE          2
     O                                           20 'SORT SEQUENCE:'
     O                       QQSEQ               36
     O*
     O          E            ROUTE          2
     O*
     O          E            ROUTE          2
     O                                           23 'SUMMARY SORT SEQ:'
     O                       QQSSRT              36
     O*
     O***  BEGIN CHG  ***  01/28/10  *********************************
     O****    E  160           ROUTE
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O***  BEGIN DLT  ***  01/28/10  *********************************
     O***                                23 '(C) COPYRIGHT PROGRESSI'
      ***** END DLT   ******* 08/27/97 ********************************
     O***********************************46 'VE DATA SOLUTIONS. INC,'
     O***********************************51 '.1997'
     O***  BEGIN DLT  ***  01/28/10  *********************************
     O***                                47 'VE DATA SOLUTIONS. INC.,'
     O***                      *YEAR   +  1
     O***  END DLT  *****  01/28/10  *********************************
      ***** END   CHG ******* 08/27/97 ********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O*** BEGIN CHG *** 06/26/97 **************************************
     O**********               CONAME   115
     O***  BEGIN CHG  ***  12/15/99  *********************************
     O**********               XXCONM   125
     O                       XXCONM             121
     O***  END   CHG  ***  12/15/99  *********************************
     O*** END   CHG *** 06/26/97 **************************************
     O                       DSDATE        Y    196
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       XXTITL             126
     O                                          190 'PAGE:'
     O                       PAGE          Z    196
     O          E            HEAD           1
      ***** BEGIN CHG ***** 08/27/97 **********************************
     O***********************************09 'FUND:'
     O                                           09 'GROUP:'
      ***** END   CHG ***** 08/27/97 **********************************
     O                       EFFND         Z     13
     O                       XFNAME              75
     O          E            HEAD           2
      ***** BEGIN CHG ***** 08/27/97 **********************************
     O***********************************14 'FUND YEAR:'
     O*************** NLR      XXYEAR    19
     O***************  LRN28   XXYEAR    19
     O***************  LR 28             19 '*ALL'
     O*************** N87                42 'EMPLOYER/DIV:'
     O*************** N87      XXEMP#Z   48
     O*************** N87      XXDIV Z   52
     O*************** N87      EMNAME    94
     O                                           15 'POLICY YEAR:'
      ***** BEG DEL ******* 04/13/98 **********************************
     O*********       NLR      XXYEAR    20
     O*********        LRN28   XXYEAR    20
      ***** END DEL ******* 04/13/98 **********************************
      ***** BEG ADD ******* 04/13/98 **********************************
     O***  BEGIN CHG  ***  12/15/99  *********************************
     O**********      NLR      XXYEAR    18
     O**********       LRN28   XXYEAR    18
     O              NLR      XXYEAR        Z     19
     O               LRN28   XXYEAR        Z     19
     O***  END   CHG  ***  12/15/99  *********************************
      ***** END ADD ******* 04/13/98 **********************************
     O               LR 28                       20 '*ALL'
     O              N87                          43 'EMPLOYER/DIV:'
     O*** BEGIN CHG ***  05/14/98  ************************************
     O*****           N87      XXEMP#Z   49
     O*****           N87      XXDIV Z   53
     O*****           N87      EMNAME    95
     O              N87      XXEMP#        Z     53
      *** BEG CHG **** 08/07/02 ************************************************
     O*************** N87      XXDIV Z   59
     O              N87N89   XXDIV         Z     59
     O              N87 89                       59 '/ *ALL'
      *** END CHG **** 08/07/02 ************************************************
     O              N87      EMNAME             101
     O*** END CHG *****  05/14/98  ************************************
      ***** END   CHG ***** 08/27/97 **********************************
     O          E   NLR 35N36HEAD           2
     O                       XXSHD               47
     O          E            HEAD           1
     O                                           61 'INJURY'
     O                                           79 'INJURY CODES'
     O                                           91 'LOST   DATE'
     O          E            HEAD           2
      ****** BEGIN CHG **** 08/12/97 **********************************
     O***********************************15 'EMPLR DIV  CASE'
     O***********************************26 'CLAIMANT'
     O                                            9 'EMPLR DIV'
     O                                           34 'CASE  CLAIMANT'
      ****** END   CHG **** 08/12/97 **********************************
     O                                           51 'SOC SEC'
     O                                           66 'DATE   O/C'
     O                                           79 'NAT PRT CAU'
     O                                           93 'TIME RECEIVED'
     O                                          106 'COMPENSATION'
     O                                          124 'M E D I C A L'
     O                                          137 'O T H E R'
     O                                          155 'TOTAL PAID'
     O                                          176 'TOTAL INCURRED'
     O                                          194 'OUTSTANDING RES'
     O          E            DETAIL         1
      *******  BEGIN CHG ******** 08/12/97 ****************************
     O*************************EFEMP#Z****9
     O*************************EFDIV Z***16
     O                       EFEMP2        Z      5
     O                       EFDIV2        Z      9
      *******  END   CHG ******** 08/12/97 ****************************
     O                       EFCASE        Z     24
     O                       CNAME               41
     O                       SS4                 46
     O                       SS2                 48
     O                       SS5                 53
     O                       ACCDTE              62 '  /  /  '
     O                       EFOPCL              65
     O                       EFNATR              70
     O                       EFPART              74
     O                       EFCAUS              78
     O                       EFLTFL              82
     O                       RECDTE              92 '  /  /  '
     O                       COMP          AB   108
     O                       MED           AB   125
     O                       OTHR          AB   142
     O                       TOTALP        AB   159
     O                       TOTINC        AB   176
     O                       OUTRES        AB   193
     O          E            TOTL1       1  2
     O                       PARTL1        1     66
     O                  33                       86 'CASES FOR PART CODE'
     O                  34                       87 'CASES FOR CAUSE CODE'
     O                  33   HPART               89
     O                  34   HPART               90
     O                       L1COMP        A    108
     O                                          109 '*'
     O                       L1MED         A    125
     O                                          126 '*'
     O                       L1OTHR        A    142
     O                                          143 '*'
     O                       L1TOTL        A    159
     O                                          160 '*'
     O                       L1TINC        A    176
     O                                          177 '*'
     O                       L1ORES        A    193
     O                                          194 '*'
     O          E            TOTL2          3
     O                       NATRL1        1     66
     O                                           88 'CASES FOR NATURE CODE'
     O                       HNATR               91
     O                       L2COMP        A    108
     O                                          110 '**'
     O                       L2MED         A    125
     O                                          127 '**'
     O                       L2OTHR        A    142
     O                                          144 '**'
     O                       L2TOTL        A    159
     O                                          161 '**'
     O                       L2TINC        A    176
     O                                          178 '**'
     O                       L2ORES        A    193
     O                                          195 '**'
     O          E       35   TOTST          3
     O                                           12 'TOTALS FOR'
     O*** BEGIN CHG *** 06/26/97 **************************************
     O**********               XXSNAM    25
     O                       XXSNAM              26
     O*** BEGIN CHG *** 06/26/97 **************************************
     O                       STOPN         1     32
     O                                           43 'CASES OPEN'
     O                       STCLS         1     50
     O                                           63 'CASES CLOSED'
     O                       STTCAS        1     73
     O                                           85 'TOTAL CASES'
     O                       STCOMP        A    108
     O                       STMED         A    125
     O                       STOTHR        A    142
     O                       STTOTL        A    159
     O                       STTINC        A    176
     O                       STORES        A    193
     O          E            TOTL3          3
     O                                           17 'TOTALS FOR YEAR'
     O                       L3OPN         1     25
     O                                           36 'CASES OPEN'
     O                       L3CLS         1     43
     O                                           56 'CASES CLOSED'
     O                       L3TCAS        1     68
     O                                           80 'TOTAL CASES'
     O                       L3COMP        A    108
     O                       L3MED         A    125
     O                       L3OTHR        A    142
     O                       L3TOTL        A    159
     O                       L3TINC        A    176
     O                       L3ORES        A    193
     O          E            TOTALS      2  2
     O                                           14 'FINAL TOTALS'
     O                       LROPN         1     22
     O                                           33 'CASES OPEN'
     O                       LRCLS         1     40
     O                                           53 'CASES CLOSED'
     O                       LRTCAS        1     68
     O                                           80 'TOTAL CASES'
     O                       LRCOMP        A    108
     O                       LRMED         A    125
     O                       LROTHR        A    142
     O                       LRTOTL        A    159
     O                       LRTINC        A    176
     O                       LRORES        A    193
     O          E            TOTALH         2 78
     O                                           16 'NATURE OF INJURY'
     O                                           54 'PART OF BODY INJURED'
     O                                           83 'CAUSE OF INJURY'
     O          E            TOTAL          1
     O                 N41   NA(K)          B    02
     O                 N41   NAD(K)         B    33
     O                 N42   PA(L)          B    36
     O                 N42   PAD(L)         B    67
     O                 N43   CA(M)          B    70
     O                 N43   CAD(M)         B   101
     O          E       81 88TOTALE         1
     O                                           15 'TOO MANY CODES,'
     O                                           37 'REFERENCE NATURE LIST'
     O          E       82 88TOTALE         1
     O                                           48 'TOO MANY CODES,'
     O                                           70 'REFERENCE PART LIST'
     O          E       83 88TOTALE         1
     O                                           81 'TOO MANY CODES,'
     O                                          103 'REFERENCE CAUSE LIST'
      ***** BEG ADD ******* 04/13/98 **********************************
     O*
     O          E            NOREC          3 10
     O                                         +010 '*** NO RECORDS MEET '
     O                                         +000 'SELECTED CRITERIA ***'
     O*
      ***** END ADD ******* 04/13/98 **********************************
     O*
     O* PRINTER FILE FOR SUMMARY FOR NATURE, PART AND CAUSE CODES
     O*
     OQSYSPRT1  E            ROUT1          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUT1          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUT1          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUT1          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUT1          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUT1          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUT1          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUT1          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUT1          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUT1          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUT1          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUT1          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUT1          1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUT1          1
     O**********     08      ORDIS2              60
     O**********E            ROUT1          1
     O**********     08      ORDIS3              60
     O**********E            ROUT1          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUT1          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUT1          1
     O               08      ORDIS2              60
     O          E            ROUT1          1
     O               08      ORDIS3              60
     O          E            ROUT1          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUT1          2
     O                                           19 'REPORT TITLE:'
     O                       X1TITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUT1          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUT1          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O*  MLS 1/15/88  ADD OUT QUEUE AND INDICATORS
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O          E            ROUT1          2
      *** BEG DEL ***** 12/19/97 **************************************
     O********                           16 'FUND YEAR:'
      *** END DEL ***** 12/19/97 **************************************
      *** BEG ADD ***** 12/19/97 **************************************
     O                                           17 'GROUP YEAR:'
      *** END ADD ***** 12/19/97 **************************************
     O              N28      QQFYR               36
     O               28                          36 '*ALL'
     O*
     O          E            ROUT1          2
     O                                           15 'RUN DATE:'
     O                       DSDATE        Y     36
     O*
     O          E            ROUT1          2
     O                                           19 'EMPLOYER/DIV:'
     O              N87      XXEMP#              30
     O              N87                          31 '/'
      *** BEG CHG **** 08/07/02 ************************************************
     O*************** N87      XXDIV     36
     O***************  87                36 '*ALL'
     O              N87N89   XXDIV               36
     O               89                          36 '*ALL'
      *** END CHG **** 08/07/02 ************************************************
     O*
     O          E            ROUT1          2
     O                                           25 'SELECT OPEN CLAIMS:'
     O                       QQSELO              36
     O*
     O          E            ROUT1          2
     O                                           27 'SELECT CLOSED CLAIMS:'
     O                       QQSELC              36
     O*
     O          E            ROUT1          2
     O                                           18 '# OF MONTHS:'
     O                       QQMTH               36
     O*
     O          E            ROUT1          2
     O                                           20 'PRINT SUMMARY:'
     O                       QQPRNT              36
     O*
     O          E            ROUT1          2
     O                                           20 'SORT SEQUENCE:'
     O                       QQSEQ               36
     O*
     O          E            ROUT1          2
     O*
     O          E            ROUT1          2
     O                                           23 'SUMMARY SORT SEQ:'
     O                       QQSSRT              36
     O*
     O***  BEGIN CHG  ***  01/28/10  *********************************
     O****    E  160           ROUT1
     O          E            ROUT1          1 59
     O                       #BRND1
     O          E            ROUT1          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      ****** BEG DLT ***** 01/28/10 **********************************
     O***                                23 '(C) COPYRIGHT PROGRESSI'
      ****** END DLT ***** 01/28/10 **********************************
      ***** BEGIN CHG ***** 08/27/97 **********************************
     O***********************************46 'VE DATA SOLUTIONS. INC,'
     O***********************************51 '.1990'
      ****** BEG DLT ***** 01/28/10 **********************************
     O***                                47 'VE DATA SOLUTIONS. INC.,'
     O***                      *YEAR   +  1
      ****** END DLT ***** 01/28/10 **********************************
      ***** END   CHG ***** 08/27/97 **********************************
     O*
     O*  HEADING FOR NATURE, PART CAUSE SUMMARY
     O*
     O          E            SUMHD          1 06
     O*
     O                       DSTIME              08 '  :  :  '
     O*** BEGIN CHG *** 06/26/97 **************************************
     O**********               CONAME   115
     O***  BEGIN CHG  ***  12/15/99  *********************************
     O**********               XXCONM   125
     O                       XXCONM             121
     O***  END   CHG  ***  12/15/99  *********************************
     O*** END   CHG *** 06/26/97 **************************************
     O                       DSDATE        Y    196
     O          E            SUMHD          2
     O                       Q1PGM               10
     O                       X1TITL             126
     O                                          190 'PAGE:'
     O                       PAGE1         Z    196
     O          E            SUMHD          1
      ******* BEGIN CHG ***** 08/27/97 ********************************
     O***********************************09 'FUND:'
     O                                           09 'GROUP:'
      ******* END   CHG ***** 08/27/97 ********************************
     O                       EFFND         Z     13
     O                       XFNAME              75
     O          E            SUMHD          2
      ******* BEGIN CHG ***** 08/27/97 ********************************
     O****************NLRN39             14 'FUND YEAR:'
     O****************NLRN39   XXYEAR    19
     O*******************N39             14 'FUND YEAR:'
     O****************NLRN39   XXYEAR    19
     O*****************LRN28N39XXYEAR    19
     O*****************LR 28N39          19 '*ALL'
     O****************N87                42 'EMPLOYER/DIV:'
     O****************N87      XXEMP#Z   48
     O****************N87      XXDIV Z   52
     O****************N87      EMNAME    94
     O              NLRN39                       15 'POLICY YEAR:'
      ***** BEG DEL ******* 04/13/98 **********************************
     O*********       NLRN39   XXYEAR    20
     O*********          N39             15 'POLICY YEAR:'
     O*********       NLRN39   XXYEAR    20
     O*********        LRN28N39XXYEAR    20
      ***** END DEL ******* 04/13/98 **********************************
      ***** BEG ADD ******* 04/13/98 **********************************
     O***  BEGIN CHG  ***  12/15/99  *********************************
     O**********      NLRN39   XXYEAR    18
     O**********         N39             15 'POLICY YEAR:'
     O**********      NLRN39   XXYEAR    18
     O**********       LRN28N39XXYEAR    18
     O              NLRN39   XXYEAR        Z     19
     O                 N39                       15 'POLICY YEAR:'
     O              NLRN39   XXYEAR        Z     19
     O               LRN28N39XXYEAR        Z     19
     O***  END   CHG  ***  12/15/99  *********************************
      ***** END ADD ******* 04/13/98 **********************************
     O               LR 28N39                    20 '*ALL'
     O              N87                          43 'EMPLOYER/DIV:'
     O*** BEGIN CHG ***  05/14/98  ************************************
     O*****           N87      XXEMP#Z   49
     O*****           N87      XXDIV Z   53
     O*****           N87      EMNAME    95
     O              N87      XXEMP#        Z     53
      *** BEG CHG **** 08/07/02 ************************************************
     O*************** N87      XXDIV Z   59
     O              N87N89   XXDIV         Z     59
     O              N87 89                       59 '/ *ALL'
      *** END CHG **** 08/07/02 ************************************************
     O              N87      EMNAME             101
     O*** END CHG *****  05/14/98  ************************************
      ******* END   CHG ***** 08/27/97 ********************************
     O*
     O          E   NLR 35   SUMHD          2
     O                       XXSHD               47
     O          E            SUMHD          1
     O               39 33                       18 'CAUSE CODE SUMMARY'
     O               39 34                       18 'PART  CODE SUMMARY'
     O                                           54 '*'
     O                                           94 'N O   L O S T   T I M E'
     O                                          116 '*  *'
     O                                          153 'L O S T   T I M E'
     O                                          174 '*'
     O                                          189 'T O T A L'
     O          E            SUMHD          2
     O                                           76 'CASES       TOTAL PAID'
     O                                           95 'OUTSTANDING RES'
     O                                          112 'TOTAL INCURRED'
     O                                          137 'CASES      TOTAL PAID'
     O                                          156 'OUTSTANDING RES'
     O                                          173 'TOTAL INCURRED'
     O                                          192 'I N C U R R E D'
     O*
     O* NATURE CODE HEADING
     O*
     O          E            NATHD          2
     O                                           15 'NATURE CODE'
     O                       HNATR               19
     O                       HNTRD               51
     O*
     O* PART CODE DETAIL
     O*
     O          E            PDETL          1
     O                  33                       15 '* PART CODE'
     O                  34                       16 '* CAUSE CODE'
     O                       HPART               19
     O                       HPRTD               51
     O                       L1NLC         1     59
     O                       L1NLTP        A     77
     O                       L1NLTO        A     95
     O                       L1NLTI        A    113
     O                       L1LTC         1    121
     O                       L1LTP         A    138
     O                       L1LTO         A    156
     O                       L1LTI         A    174
     O                       L1TTLI        A    192
     O                                          195 '*'
     O*
     O* NATURE CODE DETAIL
     O*
     O          E            NDETL       2  3
     O                                            3 '**'
     O                                           15 'NATURE CODE'
     O                       HNATR               19
     O                                           29 'TOTALS'
     O                       NATRL1        1     40
     O                                           46 'CASES'
     O                       L2NLC         1     59
     O                       L2NLTP        A     77
     O                       L2NLTO        A     95
     O                       L2NLTI        A    113
     O                       L2LTC         1    121
     O                       L2LTP         A    138
     O                       L2LTO         A    156
     O                       L2LTI         A    174
     O                       L2TTLI        A    192
     O                                          196 '**'
     O*
     O* SORT TOTALS
     O*
     O          E       35   STTTL       2  2
     O                                           12 'TOTALS FOR'
     O*** BEGIN CHG *** 06/26/97 **************************************
     O                       XXSNAM              26
     O**********               XXSNAM    25
     O*** END   CHG *** 06/26/97 **************************************
     O                       STTCAS        1     40
     O                                           46 'CASES'
     O                       STNLC         1     59
     O                       STNLTP        A     77
     O                       STNLTO        A     95
     O                       STNLTI        A    113
     O                       STLTC         1    121
     O                       STLTP         A    138
     O                       STLTO         A    156
     O                       STLTI         A    174
     O                       STTTLI        A    192
     O*
     O* FUND YEAR TOTALS
     O*
     O          E            YRTTL       2  2
     O                                           17 'TOTALS FOR YEAR'
     O                       L3TCAS        1     40
     O                                           46 'CASES'
     O                       L3NLC         1     59
     O                       L3NLTP        A     77
     O                       L3NLTO        A     95
     O                       L3NLTI        A    113
     O                       L3LTC         1    121
     O                       L3LTP         A    138
     O                       L3LTO         A    156
     O                       L3LTI         A    174
     O                       L3TTLI        A    192
     O*
     O*  FINAL TOTALS
     O*
     O          E            FNTTL       2  2
     O                                           14 'FINAL TOTALS'
     O                       LRTCAS        1     25
     O                                           31 'CASES'
     O          E            FNTTL          2
     O                                           12 'CASES OPEN'
     O                       LROPN         1     19
     O                                           38 'CASES CLOSED'
     O                       LRCLS         1     45
     O                       LRNLC         1     59
     O                       LRNLTP        A     77
     O                       LRNLTO        A     95
     O                       LRNLTI        A    113
     O                       LRLTC         1    121
     O                       LRLTP         A    138
     O                       LRLTO         A    156
     O                       LRLTI         A    174
     O                       LRTTLI        A    192
     O*
     O*  CAUSE CODE SUMMARY FUND YEAR HEADING
     O*
     O          E            YRHEAD      2  2
      *** BEG DEL ***** 12/19/97 **************************************
     O*******                            13 '** FUND YEAR'
     O*******                  HYR       16
     O*******                            19 '**'
      *** END DEL ***** 12/19/97 **************************************
      *** BEG ADD ***** 12/19/97 **************************************
     O                                           13 '** GROUP YEAR'
     O                       HYR               +001
     O                                         +001 '**'
      *** END ADD ***** 12/19/97 **************************************
     O*
     O*  CAUSE CODE SUMMARY
     O*
     O          E            CASDT          1
     O                  33                       14 'CAUSE CODE'
     O                  34                       14 'PART  CODE'
     O                       XCODE               17
     O                       XXDESC         B    50
     O                       XNLC          1     59
     O                       XNLTP         A     77
     O                       XNLTO         A     95
     O                       XNLTI         A    113
     O                       XLTC          1    121
     O                       XLTP          A    138
     O                       XLTO          A    156
     O                       XLTI          A    174
     O                       XXTTLI        A    192
      ***** BEG ADD ******* 04/13/98 **********************************
     O*
     O*** BEGIN CHG ***  05/14/98  ************************************
     O*****   E  310           NOREC
     O          E            NOREC1         3 10
     O*** END CHG *****  05/14/98  ************************************
     O                                         +010 '*** NO RECORDS MEET '
     O                                         +000 'SELECTED CRITERIA ***'
      ***** END ADD ******* 04/13/98 **********************************
     O*
**
HCHAPTER:
OCOUNTY:
RREGION:
