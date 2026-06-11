     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7426R - Agent Commission Amount Breakdown Report
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 08/18/14  C:33  Seq#: SY354                                   *
     H*                 Remove extraneous 'routing' info.             *
     H*                                                               *
     H* 01/22/13  C:287 Seq#: SY266_OVR                               *
     H*                 Added DFTACTGRP and ACTGRP compile options    *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  02/08/02 C:90  PROJECT: SEQ#31210                            *
     H*                 Increase array size from 10 to 50.            *
     H*                                                               *
     H*  04/10/00 C:118 PROJECT: WC987                                *
     H*                 LOAD ALTERNATE COMPANY NAME FROM FUND MASTER  *
     H*                                                               *
     H*  11/05/99 C:33 SUPPORT: WA048/20542                           *
     H*                WHEN LOADING THE COMBINATION (COMB) ARRAY,     *
     H*                USE THE YEAR AS THE COMPARISON VALUE INSTEAD OF*
     H*                THE AMOUNT                                     *
     H*                                                               *
     H*  01/06/99 C:33 SUPPORT: WA048/15692                           *
     H*                DO NOT TOTAL COLLECTIONS FOR OTHER EARNED REC'D*
     H*****************************************************************
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMACL3    IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWMARL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D*
      **** BEGIN CHG **** 02/08/02 ************************************
     D******************* YEAR       10  3 0             Year array
     D******************* AMT        10 66               Amount ttl array
     D******************* COMB       10 69               Combination array
     D YEAR            S              3  0 DIM(50)                              Year array
     D AMT             S             66    DIM(50)                              Amount ttl array
     D COMB            S             69    DIM(50)                              Combination array
      **** END   CHG **** 02/08/02 ************************************
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXDTTO                 1      7  0
     D  XXTDYR                 2      3  0
     D  XXTDMO                 4      5  0
     D                 DS
     D  XXEFDT                 1      7  0
     D  XXEDYR                 2      3  0
     D  XXEDMO                 4      5  0
     D                 DS
     D  XAMT                   1     66
     D  PREM                   1     11  2
     D  COMM                  12     22  2
     D  AMT5                  23     33  2
     D  AMT2                  34     44  2
     D  AMT3                  45     55  2
     D  AMT1                  56     66  2
     D                 DS
     D  XCOMB                  1     69
     D  CYEAR                  1      3  0
     D  CPREM                  4     14  2
     D  CCOMM                 15     25  2
     D  CAMT5                 26     36  2
     D  CAMT2                 37     47  2
     D  CAMT3                 48     58  2
     D  CAMT1                 59     69  2
     D*
     D TITLE           C                   CONST('Agent Commission -
     D                                     Amount Breakdown -
     D                                     Report')
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  Override File
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT
     C*
     C*  Group Master File
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  Master File
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ACEMP#
     C                   KFLD                    ACDIV
     C*
     C*  Agent Commission Master File
     C*
     C     ACKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  Agent History Master File
     C*
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ACEMP#
     C                   KFLD                    ACDIV
     C                   KFLD                    XXSFYR
     C*
     C*  Agent Rate Master File
     C*
     C     ARKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    AHRT#
     C*_______________________________________________________________
     C*
     C     *LIKE         DEFINE    ORFIL#        XXPRT
     C     *LIKE         DEFINE    ACCO#         XXCO#
     C     *LIKE         DEFINE    ACFND         XXFND
     C     *LIKE         DEFINE    ARDT          X7FRDT
     C     *LIKE         DEFINE    ARDT          X7TODT
     C*_______________________________________________________________
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
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFDT             8
     C                   PARM                    QQTDT             8
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFDT         X8FRDT            8 0
     C                   MOVE      QQTDT         X8TODT            8 0
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      *ZEROS        COMB
     C                   MOVE      *ZEROS        AMT
     C                   Z-ADD     0             YEAR
     C*
     C                   Z-ADD     X8FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7FRDT
     C*
     C                   Z-ADD     X8FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         X6FDT             6 0
     C*
     C                   Z-ADD     X8TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7TODT
     C*
     C                   Z-ADD     X8TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         X6TDT             6 0
     C*_______________________________________________________________
     C*
     C*   Set Up and Initialize
     C*
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*   Chain to the User Parameter File
     C*
     C     Q1USER        CHAIN     PUP01                              60
     C*
     C*   Chain to the Comapny Master File
     C*
     C     XXCO#         CHAIN     PCO01                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   Chain to the Group Master File
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     *IN60         IFEQ      *OFF
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C                   ENDIF                                                  *IN60=OFF
     C***** END   ADD **** 04/10/00 ***********************************
     C*
     C*   Get the Distribution Information & Print
     C*
     C                   MOVE      '0'           XXPRT
     C*
     C     ORKY          CHAIN     POR01                              60
     C*
     C     ORTITL        IFNE      *BLANKS
     C*
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C*
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF                                                  N<=60
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  TITLE         ORTITL
     C*
     C                   ENDIF                                                  ORTITL<>*BLANKS
     C*
     C     *IN60         IFEQ      *OFF
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*  Center Text in Field
     C*
     C                   MOVEL(P)  ORTITL        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        ORTITL
     C*
     C                   MOVE      '1'           *INOA
     C*_______________________________________________________________
     C*
     C*  Get Agent Commission Records
     C*
     C     ACKY          CHAIN     PAC01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C*  Use only 'A' or 'C' Commission type records
     C*
     C     ACCOTP        IFEQ      'A'
     C     ACCOTP        OREQ      'C'
     C*
     C*  Check to be sure record is within the date range selected
     C*
     C     ACPODT        IFGE      X7FRDT
     C     ACPODT        ANDLE     X7TODT
     C*
     C  N85              Z-ADD     ACAGT#        XXSAGT            5 0          Save agent #
     C  N85              Z-ADD     ACFYR         XXSFYR            3 0          Save year
     C                   MOVE      '1'           *IN85
     C*
     C                   SELECT
     C*
     C* Check for new AGENT
     C*
     C     XXSAGT        WHENNE    ACAGT#
     C*
     C* Check page overflow before printing detail line
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   END
     C*
     C* Print Agent Year Totals & Zero-out Year Fields
     C*
     C     XXSFYR        IFNE      0
     C                   EXCEPT    YRTTL
     C                   END
     C*
     C                   EXSR      YRCLR
     C*
     C* Print Agent Total & Zero-out Total Fields
     C*
     C                   EXCEPT    TOTL
     C                   EXSR      TOTCLR
     C*
     C                   MOVE      '1'           *INOA
     C                   Z-ADD     ACAGT#        XXSAGT
     C                   Z-ADD     ACFYR         XXSFYR
     C                   Z-ADD     0             PAGE
     C*
     C* Check for new year
     C*
     C     ACFYR         WHENNE    XXSFYR
     C*
     C* Check page overflow before printing detail line
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   END
     C*
     C* Print Agent Year Totals & Zero-out Year Fields
     C*
     C     XXSFYR        IFNE      0
     C                   EXCEPT    YRTTL
     C                   END
     C*
     C                   EXSR      YRCLR
     C*
     C                   Z-ADD     ACFYR         XXSFYR
     C*
     C                   ENDSL
     C*
     C* If commission record is an "A" type; retrieve the most current
     C* agent history record for employer.
     C*
     C     ACCOTP        IFEQ      'A'
     C     AHKY          SETGT     PAH01
     C                   READP     PAH01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  60
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C     AHCO#         ANDEQ     XXCO#
     C     AHFND         ANDEQ     XXFND
     C     AHEMP#        ANDEQ     ACEMP#
     C     AHDIV         ANDEQ     ACDIV
     C     AHFYR         ANDEQ     XXSFYR
     C*
     C*  Retrieve the Employer Record to Determine Length of Enrollment
     C*
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  Retrieve the Rate.
     C*  If Employer enrolled 1 year or more, use Renew Rate.
     C*  Otherwise, use New Rate.  Use Earliest EFFECTIVE DATE.
     C*  (IGNORE LAPSES IN COVERAGE.)
     C*
     C                   Z-ADD     X7TODT        XXDTTO                         LJQ 8-25-88
     C     ARKY          CHAIN     PAR01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     EMEFD1        XXEFDT
     C*
     C     XXTDYR        SUB       XXEDYR        XXREM1            3 0
     C     XXREM1        IFGT      1
     C                   Z-ADD     ARRNRT        ACRATE
     C                   ELSE
     C     XXREM1        IFEQ      1
     C                   ADD       12            XXTDMO
     C     XXTDMO        SUB       XXEDMO        XXREM2            3 0
     C     XXREM2        IFGE      12
     C                   Z-ADD     ARRNRT        ACRATE            5 2
     C                   ELSE
     C                   Z-ADD     ARNWRT        ACRATE
     C                   END
     C                   ELSE
     C                   Z-ADD     ARNWRT        ACRATE
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C* Calculate percentages
     C*
     C                   EXSR      CALC
     C*
     C*
     C* Check page overflow before printing detail line
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C***  BEGIN ADD  ***  01/06/99  *********************************
     C     ACAGTO        COMP      'Y'                                    97
     C***  END   ADD  ***  01/06/99  *********************************
     C*
     C* Print Detail Line & Zero-out Detail Fields
     C*
     C                   EXCEPT    DETL
     C                   EXSR      DETCLR
     C*
     C                   END
     C*
     C                   ENDIF                                                  ACCOTP=A/C
     C*
     C     ACKY          READE     PAC01                                  60
     C*
     C                   END
     C*
     C* Print last agent's totals,
     C* Check page overflow before printing detail line
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   END
     C*
     C* Print Agent Year Totals & Zero-out Year Fields
     C*
     C     XXSFYR        IFNE      0
     C                   EXCEPT    YRTTL
     C                   END
     C*
     C                   EXSR      YRCLR
     C*
     C* Print Agent Total & Zero-out Total Fields
     C*
     C                   EXCEPT    TOTL
     C                   EXSR      TOTCLR
     C*
     C* Print Total Page
     C*
     C                   EXCEPT    FNTOT
     C                   EXSR      SRT
     C                   EXCEPT    FNTOT2
     C*
     C*   END OF PROGRAM
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orky POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*
     C*****************************************************************
     C*  CALC - Subroutine to Breakdown Agent Commission Amounts      *
     C*****************************************************************
     C*
     C     CALC          BEGSR
     C                   Z-ADD     0             PER5              9 6          5% rate
     C                   Z-ADD     0             PER2              9 6          2% rate
     C                   Z-ADD     0             PER3              9 6          3% rate
     C                   Z-ADD     0             PER1              9 6          1% rate
     C                   Z-ADD     0             XXTTL             9 2          total % fields
     C                   Z-ADD     0             XXRMDR            9 2          remainder
     C*
     C* Determine which calculation
     C*
     C     ACRATE        IFEQ      1.00
     C                   Z-ADD     ACAMT         X1AMT1
     C                   END
     C*
     C     ACRATE        IFEQ      5.00
     C                   Z-ADD     ACAMT         X1AMT5
     C                   END
     C*
     C* If agent commission rate equals 7.00 then use percentage fields
     C* PER5(5%) and PER2(2%).  Take the commission amount (acamt) and
     C* calculate the $amount for these percentages.  Add the amount
     C* fields together and subtract from the actual commission amount.
     C* If there is a remainder; add this to the 5% amount field.
     C*
     C     ACRATE        IFEQ      7.00
     C*
     C     5             DIV(H)    7             PER5
     C     2             DIV(H)    7             PER2
     C     ACAMT         MULT(H)   PER5          X1AMT5
     C     ACAMT         MULT(H)   PER2          X1AMT2
     C     X1AMT5        ADD       X1AMT2        XXTTL
     C     ACAMT         SUB       XXTTL         XXRMDR
     C*
     C     XXRMDR        IFNE      0
     C     X1AMT5        ADD       XXRMDR        X1AMT5
     C                   END
     C*
     C                   END
     C*
     C* If agent commission rate equals 8.00 then use percentage fields
     C* PER5(5%),PER2(2%) and PER1(1%). Take the commission (acamt) and
     C* calculate the $amount for these percentages.  Add the amount
     C* fields together and subtract from the actual commission amount.
     C* If there is a remainder; add this to the 5% amount field.
     C*
     C     ACRATE        IFEQ      8.00
     C*
     C     5             DIV(H)    8             PER5
     C     2             DIV(H)    8             PER2
     C     1             DIV(H)    8             PER1
     C     ACAMT         MULT(H)   PER5          X1AMT5
     C     ACAMT         MULT(H)   PER2          X1AMT2
     C     ACAMT         MULT(H)   PER1          X1AMT1
     C     X1AMT5        ADD       X1AMT2        XXTTL
     C     X1AMT1        ADD       XXTTL         XXTTL
     C     ACAMT         SUB       XXTTL         XXRMDR
     C*
     C     XXRMDR        IFNE      0
     C     X1AMT5        ADD       XXRMDR        X1AMT5
     C                   END
     C*
     C                   END
     C*
     C* If agent commission rate equals 10.0 then use percentage fields
     C* PER5(5%),PER2(2%) and PER3(3%). Take the commission (acamt) and
     C* calculate the $amount for these percentages.  Add the amount
     C* fields together and subtract from the actual commission amount.
     C* If there is a remainder; add this to the 5% amount field.
     C*
     C     ACRATE        IFEQ      10.00
     C*
     C     5             DIV(H)    10            PER5
     C     2             DIV(H)    10            PER2
     C     3             DIV(H)    10            PER3
     C     ACAMT         MULT(H)   PER5          X1AMT5
     C     ACAMT         MULT(H)   PER2          X1AMT2
     C     ACAMT         MULT(H)   PER3          X1AMT3
     C     X1AMT5        ADD       X1AMT2        XXTTL
     C     X1AMT3        ADD       XXTTL         XXTTL
     C     ACAMT         SUB       XXTTL         XXRMDR
     C*
     C     XXRMDR        IFNE      0
     C     X1AMT5        ADD       XXRMDR        X1AMT5
     C                   END
     C*
     C                   END
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*  DETCLR - Subroutine to Add Detail Fields to Agent Totals and *
     C*           Zero-out Detail Fields                              *
     C*****************************************************************
     C*
     C     DETCLR        BEGSR
     C*
     C* Add Detail Fields to Agent Year Total Fields
     C*
     C***  BEGIN ADD  ***  01/06/99  *********************************
     C     ACAGTO        IFNE      'Y'
     C***  END   ADD  ***  01/06/99  *********************************
     C                   ADD       ACTOCL        X4PREM
     C***  BEGIN ADD  ***  01/06/99  *********************************
     C                   ENDIF                                                  ACAGTO<>Y
     C***  END   ADD  ***  01/06/99  *********************************
     C*
     C                   ADD       ACAMT         X4AMT
     C                   ADD       X1AMT5        X4AMT5
     C                   ADD       X1AMT2        X4AMT2
     C                   ADD       X1AMT3        X4AMT3
     C                   ADD       X1AMT1        X4AMT1
     C*
     C* Add Detail Fields to Agent Total Fields
     C*
     C***  BEGIN ADD  ***  01/06/99  *********************************
     C     ACAGTO        IFNE      'Y'
     C***  END   ADD  ***  01/06/99  *********************************
     C                   ADD       ACTOCL        X2PREM
     C***  BEGIN ADD  ***  01/06/99  *********************************
     C                   ENDIF                                                  ACAGTO<>Y
     C***  END   ADD  ***  01/06/99  *********************************
     C*
     C                   ADD       ACAMT         X2AMT
     C                   ADD       X1AMT5        X2AMT5
     C                   ADD       X1AMT2        X2AMT2
     C                   ADD       X1AMT3        X2AMT3
     C                   ADD       X1AMT1        X2AMT1
     C*
     C* Zero-out Detail Fields
     C*
     C                   Z-ADD     0             X1AMT5            9 2
     C                   Z-ADD     0             X1AMT2            9 2
     C                   Z-ADD     0             X1AMT3            9 2
     C                   Z-ADD     0             X1AMT1            9 2
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*  YRCLR - Subroutine to Add Detail Fields to Yearly Total      *
     C*          Arrays & Zero-out Agent Year Totals                  *
     C*****************************************************************
     C*
     C     YRCLR         BEGSR
     C*
     C* Add Detail Fields to Year Total Array Fields
     C*
     C*
     C*  CHECK TO SEE IF CODE IS ALREADY IN ARRAY
     C*
     C                   Z-ADD     1             V                 5 0
     C                   MOVE      '1'           *IN33
     C*
     C*
     C     XXSFYR        LOOKUP    YEAR(V)                                33
     C*
     C     *IN33         IFEQ      '0'                                          NEW CODE-ADD
     C                   Z-ADD     1             V
     C*
     C*  FIND NEXT AVAILABLE POSITION
     C*
     C     YEAR(V)       DOWNE     *ZERO
     C                   ADD       1             V
     C                   END
     C*
     C*  ADD FIELDS TO THE ARRAY
     C*
     C                   Z-ADD     XXSFYR        YEAR(V)
     C                   MOVE      *ZEROS        XAMT
     C                   Z-ADD     X4PREM        PREM
     C                   Z-ADD     X4AMT         COMM
     C                   Z-ADD     X4AMT5        AMT5
     C                   Z-ADD     X4AMT2        AMT2
     C                   Z-ADD     X4AMT3        AMT3
     C                   Z-ADD     X4AMT1        AMT1
     C                   MOVEA     XAMT          AMT(V)
     C*
     C*   IF CODE IS IN ARRAY ADD TO EXISTING TOTALS
     C*
     C                   ELSE
     C*
     C                   MOVE      *ZEROS        XAMT
     C                   MOVEA     AMT(V)        XAMT
     C                   ADD       X4PREM        PREM
     C                   ADD       X4AMT         COMM
     C                   ADD       X4AMT5        AMT5
     C                   ADD       X4AMT2        AMT2
     C                   ADD       X4AMT3        AMT3
     C                   ADD       X4AMT1        AMT1
     C                   MOVEA     XAMT          AMT(V)
     C*
     C                   END
     C*
     C* Zero-out Detail Fields
     C*
     C                   Z-ADD     0             X4PREM           11 2
     C                   Z-ADD     0             X4AMT            11 2
     C                   Z-ADD     0             X4AMT5            9 2
     C                   Z-ADD     0             X4AMT2            9 2
     C                   Z-ADD     0             X4AMT3            9 2
     C                   Z-ADD     0             X4AMT1            9 2
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*  TOTCLR - Subroutine to Add Agent Total Fields to Total       *
     C*           Fields and Zero-out Agent Total Fields              *
     C*****************************************************************
     C*
     C     TOTCLR        BEGSR
     C*
     C* Add Agent Total Fields to Total Fields
     C*
     C                   ADD       X2PREM        X3PREM           11 2
     C                   ADD       X2AMT         X3AMT            11 2
     C                   ADD       X2AMT5        X3AMT5           11 2
     C                   ADD       X2AMT2        X3AMT2           11 2
     C                   ADD       X2AMT3        X3AMT3           11 2
     C                   ADD       X2AMT1        X3AMT1           11 2
     C*
     C* Zero-out Detail Fields
     C*
     C                   Z-ADD     0             X2PREM           11 2
     C                   Z-ADD     0             X2AMT            11 2
     C                   Z-ADD     0             X2AMT5            9 2
     C                   Z-ADD     0             X2AMT2            9 2
     C                   Z-ADD     0             X2AMT3            9 2
     C                   Z-ADD     0             X2AMT1            9 2
     C*
     C*
     C                   ENDSR
     C*
     C*
     C*****************************************************************
     C*  SRT - Subroutine to sort arrays for Group totals by year     *
     C*****************************************************************
     C*
     C     SRT           BEGSR
     C*
     C* MOVE CODE AND TOTAL ARRAYS TO COMBINATION ARRAY
     C*
     C                   Z-ADD     1             V
     C*
     C***  BEGIN ADD  ***  11/05/99  *********************************
     C********** AMT,V     DOWNE*ZEROS
     C     YEAR(V)       DOWNE     *ZEROS
     C***  END   ADD  ***  11/05/99  *********************************
     C                   MOVEL     YEAR(V)       COMB(V)
     C                   MOVE      AMT(V)        COMB(V)
     C                   ADD       1             V
     C                   END
     C*
     C*  SORT THE ARRAY
     C*
     C                   SORTA     COMB
     C*
     C*  FIND THE FIRST RECORD
     C*
     C                   Z-ADD     1             X1                5 0
     C*
      **** BEGIN CHG **** 02/08/02 ************************************
     C********** X1        DOWLE10
     C     X1            DOWLE     50
      **** END   CHG **** 02/08/02 ************************************
     C     COMB(X1)      ANDEQ     *ZEROS
     C                   ADD       1             X1
     C                   ENDDO
     C*
      **** BEGIN CHG **** 02/08/02 ************************************
     C********** X1        DOWLE10
     C     X1            DOWLE     50
      **** END   CHG **** 02/08/02 ************************************
     C                   MOVE      *ZEROS        XCOMB
     C                   MOVEA     COMB(X1)      XCOMB
     C*
     C*  PRINT DETAIL LINE
     C*
     C                   EXCEPT    FNTOT1
     C*
     C                   ADD       1             X1
     C                   END
     C*
     C                   ENDSR
     C*
     C*
     O*****************************************************************
     O*                Output Specifications                          *
     O*****************************************************************
     O*
     O*   Routing Heading
     O*
     OQSYSPRT   E            ROUTE          1 06
     O***  Begin Del  ***  08/18/14  **************************************
     O***********                                 8 'XXXXXXXX'
     O***********                                20 'XXXXXX'
     O***********                                34 'XX      XX'
     O***********                                45 'XXXXXXXXXX'
     O***********                                56 'XXXXXXXXXX'
     O***********                                67 'XXX     XX'
     O***********                                77 'XXXXXX'
     O**********E            ROUTE          1
     O***********                                 9 'XXXXXXXXX'
     O***********                                21 'XXXXXXXX'
     O***********                                34 'XX      XX'
     O***********                                45 'XXXXXXXXXX'
     O***********                                56 'XXXXXXXXXX'
     O***********                                67 'XXXX    XX'
     O***********                                78 'XXXXXXXX'
     O**********E            ROUTE          1
     O***********                                10 'XX      XX'
     O***********                                22 'XX      XX'
     O***********                                34 'XX      XX'
     O***********                                41 'XX'
     O***********                                52 'XX'
     O***********                                67 'XXXX    XX'
     O***********                                79 'XX      XX'
     O**********E            ROUTE          1
     O***********                                10 'XX      XX'
     O***********                                22 'XX      XX'
     O***********                                34 'XX      XX'
     O***********                                41 'XX'
     O***********                                52 'XX'
     O***********                                67 'XX XX   XX'
     O***********                                71 'XX'
     O**********E            ROUTE          1
     O***********                                 9 'XXXXXXXXX'
     O***********                                22 'XX      XX'
     O***********                                34 'XX      XX'
     O***********                                41 'XX'
     O***********                                52 'XX'
     O***********                                67 'XX XX   XX'
     O***********                                71 'XX'
     O**********E            ROUTE          1
     O***********                                 8 'XXXXXXXX'
     O***********                                22 'XX      XX'
     O***********                                34 'XX      XX'
     O***********                                41 'XX'
     O***********                                52 'XX'
     O***********                                67 'XX  XX  XX'
     O***********                                71 'XX'
     O**********E            ROUTE          1
     O***********                                 4 'XXXX'
     O***********                                22 'XX      XX'
     O***********                                34 'XX      XX'
     O***********                                41 'XX'
     O***********                                52 'XX'
     O***********                                67 'XX  XX  XX'
     O***********                                79 'XX    XXXX'
     O**********E            ROUTE          1
     O***********                                 5 'XXXXX'
     O***********                                22 'XX      XX'
     O***********                                34 'XX      XX'
     O***********                                41 'XX'
     O***********                                52 'XX'
     O***********                                67 'XX   XX XX'
     O***********                                79 'XX    XXXX'
     O**********E            ROUTE          1
     O***********                                 6 'XX XXX'
     O***********                                22 'XX      XX'
     O***********                                34 'XX      XX'
     O***********                                41 'XX'
     O***********                                52 'XX'
     O***********                                67 'XX   XX XX'
     O***********                                79 'XX      XX'
     O**********E            ROUTE          1
     O***********                                 7 'XX  XXX'
     O***********                                22 'XX      XX'
     O***********                                34 'XX      XX'
     O***********                                41 'XX'
     O***********                                52 'XX'
     O***********                                67 'XX    XXXX'
     O***********                                79 'XX      XX'
     O**********E            ROUTE          1
     O***********                                 8 'XX   XXX'
     O***********                                21 'XXXXXXXX'
     O***********                                33 'XXXXXXXX'
     O***********                                41 'XX'
     O***********                                56 'XXXXXXXXXX'
     O***********                                67 'XX    XXXX'
     O***********                                78 'XXXXXXXX'
     O**********E            ROUTE          3
     O***********                                 9 'XX    XXX'
     O***********                                20 'XXXXXX'
     O***********                                32 'XXXXXX'
     O***********                                41 'XX'
     O***********                                56 'XXXXXXXXXX'
     O***********                                67 'XX     XXX'
     O***********                                77 'XXXXXX'
     O**********E            ROUTE          1
     O***********                                15 'ROUTE TO:'
     O***********    08      ORDIS1              60
     O***********       09   UPUSNM              67
     O**********E            ROUTE          1
     O***********    08      ORDIS2              60
     O**********E            ROUTE          1
     O***********    08      ORDIS3              60
     O**********E            ROUTE          2
     O***********    08      ORDIS4              60
     O**********E            ROUTE          2
     O***  End   Del  ***  08/18/14  **************************************
     O***  Beg   Add  ***  08/05/15  **************************************
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
     O***  End   Add  ***  08/05/15  **************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
     O***  Begin Del  ***  08/18/14  **************************************
      ***********
     O**********E            ROUTE          2
     O***********                                19 'PROGRAM NAME:'
     O***********            ORPNAM              30
     O***********                                36 'FILE:'
     O***********            ORFIL#              38
     O***********                                46 'COPIES:'
     O***********            ORCOPY        Z     50
     O***********                                56 'HOLD:'
     O***********            ORHOLD              61
     O**********E            ROUTE          2
     O***********                                19 'OUTPUT QUEUE:'
     O***********    08      OROUTQ              30
     O***********       09   QQQUE               30
     O***********                                37 'FORMS:'
     O***********            ORFORM              48
     O***********                                56 'SAVE:'
     O***********            ORSAVE              61
      *******BEG*CHG ***** 01/28/10 **********************************
     O********E** 60           ROUTE
     O***********                        23 '(C) COPYRIGHT PROGRESSI'
     O***********                        46 'VE DATA SOLUTIONS. INC,'
     O***********                        51 '.1996'
     O*****End***Del  ***  08/18/14  **************************************
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
     O* Standard Heading
     O*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              84
     O                       DSDATE        Y    131
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              93
     O                                          123 'PAGE:'
     O                       PAGE          Z    131
     O*
     O* Report Heading
     O*
     O          E            HEAD           2
     O                                           11 'DATE RANGE:'
     O                       X6FDT         Y     20
     O                                           22 '-'
     O                       X6TDT         Y     31
     O          E            HEAD           2
     O                                              'GROUP:'
     O                       FNFND         Z   +002
     O                       FNNAME            +002
     O          E            HEAD           2
     O                                            7 'AGENT'
     O                       XXSAGT        Z     14
     O          E            HEAD           2
     O                                           13 'YEAR'
     O                                           30 'COLLECTED PREM'
     O                                           47 'COMM EARNED'
     O                                           60 'RATE'
     O                                           78 '5% AMT'
     O                                           94 '2% AMT'
     O                                          110 '3% AMT'
     O                                          126 '1% AMT'
     O*
     O* Detail Line
     O*
     O          E            DETL           1
     O                       ACFYR         Z     12
     O                       ACTOCL        A     32
     O***  BEGIN ADD  ***  01/06/99  *********************************
     O               97                        +000 '*'
     O***  END   ADD  ***  01/06/99  *********************************
     O                       ACAMT         A     49
     O                       ACRATE        B     62
     O                       X1AMT5        B     80
     O                       X1AMT2        B     96
     O                       X1AMT3        B    112
     O                       X1AMT1        B    128
     O*
     O* Agent Year Total
     O*
     O          E            YRTTL       2  2
     O                                            6 'YEAR'
     O                       XXSFYR        Z      9
     O                                           16 'TOTAL:'
     O                       X4PREM        A     32
     O                       X4AMT         A     49
     O                       X4AMT5        B     80
     O                       X4AMT2        B     96
     O                       X4AMT3        B    112
     O                       X4AMT1        B    128
     O*
     O* Agent Total
     O*
     O          E            TOTL        2  2
     O                                           14 'AGENT TOTAL:'
     O                       X2PREM        A     32
     O                       X2AMT         A     49
     O                       X2AMT5        B     80
     O                       X2AMT2        B     96
     O                       X2AMT3        B    112
     O                       X2AMT1        B    128
     O***  BEGIN ADD  ***  01/06/99  *********************************
     O          E            TOTL           2
     O                                              '''*'' - NOT INCLUDED'
     O                                         +001 'IN COLLECTED PREMIUM'
     O                                         +001 'TOTALS'
     O***  END   ADD  ***  01/06/99  *********************************
     O*
     O* Total Page Heading
     O*
     O          E            FNTOT          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              84
     O                       DSDATE        Y    127
     O          E            FNTOT          2
     O                       Q1PGM               10
     O                       ORTITL              93
     O                                          120 'PAGE:'
     O                       PAGE          Z    127
     O          E            FNTOT          2
     O                                           11 'DATE RANGE:'
     O                       X6FDT         Y     20
     O                                           22 '-'
     O                       X6TDT         Y     31
     O          E            FNTOT          2
     O                                              'GROUP:'
     O                       FNFND         Z   +002
     O                       FNNAME            +002
     O          E            FNTOT          2
     O                                           30 'COLLECTED PREM'
     O                                           47 'COMM EARNED'
     O                                           78 '5% AMT'
     O                                           94 '2% AMT'
     O                                          110 '3% AMT'
     O                                          126 '1% AMT'
     O*
     O* Total by Year
     O*
     O          E            FNTOT1         2
     O                                           10 'YEAR:'
     O                       CYEAR         Z     14
     O                       CPREM         A     32
     O                       CCOMM         A     49
     O                       CAMT5         B     80
     O                       CAMT2         B     96
     O                       CAMT3         B    112
     O                       CAMT1         B    128
     O*
     O* Final Group Total
     O*
     O          E            FNTOT2         2
     O                                           14 'GROUP TOTAL:'
     O                       X3PREM        A     32
     O                       X3AMT         A     49
     O                       X3AMT5        B     80
     O                       X3AMT2        B     96
     O                       X3AMT3        B    112
     O                       X3AMT1        B    128
