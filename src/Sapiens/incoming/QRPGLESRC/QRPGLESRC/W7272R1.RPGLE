     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_OVR  dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7272R1 - LOSS RATIO BY JOB CLASS
     H****************************************************************
     H* Description:  Workfile for the Loss Ratio by Job Class       *
     H*               Report.                                        *
     H*                                                              *
     H* Created: 06/24/04    Programmer: C:15    Project: WC1063     *
     H*****************************************************************
     H* REVISIONS:                                                    *
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
     H*                                                               *
     H*****************************************************************
     FWMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWMEPL4    IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWDRRL     IF   E           K DISK
     FWMPRL1    IF   E           K DISK
     FWMCMLF    IF   E           K DISK
     FWMRHL     IF   E           K DISK
     FWFLJP     O    E             DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     F* * * Output File
     F*

     D                 DS
     D  RHSU                   1     90  2
     D                                     DIM(10)                              PMT AMOUNT
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQASOF            8
     C                   PARM                    QQTYPO            1
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C*
     C                   EXSR      SR1ST
     C*
     C*  Process groups.
     C*
     C  N21FNKY          CHAIN     PFN01                              99
     C   21FNKY1         CHAIN     PFN01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C*  Process a group break.
     C*
     C     FNFND         IFNE      SVFND
     C                   EXSR      GRPBRK
     C                   ENDIF
     C*
     C*  Process policy years.
     C*
     C  N22F1KY          CHAIN     PF101                              98
     C   22F1KY1         CHAIN     PF101                              98
     C     *IN98         DOWEQ     *OFF
     C*
     C*  Process a group break.
     C*
     C     F1FYR         IFNE      SVFYR
     C                   EXSR      YRBRK
     C                   ENDIF
     C*
     C*  CHAIN OUT TO THE EMPLOYER PAYROLL FILE AND GET NUMBER OF
     C*  EMPLOYERS AND MANUAL PREMIUM
     C*
     C     EPKY          CHAIN     PEP01                              97
     C     *IN97         DOWEQ     '0'
     C*
     C     EPSTAT        IFNE      'I'
     C*
     C*   IF CLASS CODE HAS CHANGED GET TOTAL NUMBER OF CLAIMS AND
     C*   TOTAL INCURRED FOR PAYROLL CLASS
     C*
     C     EPCLAS        IFNE      XXCLAS
     C     XXCLAS        ANDNE     0
     C                   EXSR      SUBTI
     C                   ENDIF
     C*
     C     EPCLAS        IFNE      XXCLAS
     C     EPEMP#        ORNE      XXEMP#
     C                   ADD       1             COUNT1
     C                   ENDIF
     C                   MOVE      EPCLAS        XXCLAS
     C*
     C     EPEMP#        IFNE      XXEMP#
     C                   MOVE      EPEMP#        XXEMP#
     C                   EXSR      SUBNC
     C                   ENDIF
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      PROCCD        XXOCCD            1
      *** Beg Add ***** 10/20/15
     C                   MOVE      PRNRAT        XXNRAT            1
      *** End Add ***** 10/20/15
     C                   ELSE
     C                   MOVE      *BLANK        XXOCCD
      *** Beg Add ***** 10/20/15
     C                   MOVE      *BLANKS       XXNRAT            1
      *** End Add ***** 10/20/15
     C                   ENDIF
     C*
     C     EPCLAS        IFNE      RRDCCD
     C     XXOCCD        ANDNE     'Y'
      *** Beg Add ***** 10/20/15
     C     XXNRAT        ANDNE     'Y'
      *** End Add ***** 10/20/15
     C*
     C*  DETERMINE WHETHER ESTIMATED OR AUDITED PAYROLL SHOULD BE
     C*  LISTED BASED ON PAYROLL TYPE FLAG (EPSAFL)
     C*
     C     EPSAFL        IFNE      'A'
     C                   ADD       EPEAPR        XXPYRL
     C                   ELSE
     C                   ADD       EPAAPR        XXPYRL
     C                   ENDIF
     C*
     C*  CALCULATE PREMIUM ACCORDING TO ENTERED RATE DATE.
     C*
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     EPAMPR        XXMAN            11 2
     C                   ELSE
     C                   Z-ADD     EPEMPR        XXMAN
     C                   ENDIF                                                  ELFAF IFEQ 'A'
     C*
     C*  ACCUMULATE MANUAL PREMIUM AMOUNTS
     C*
     C                   ADD       XXMAN         CLEMPR
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  Process next payroll record.
     C*
     C     EPKY          READE     PEP01                                  97
     C                   ENDDO
     C*
     C****       CLEMPR    IFNE 0
     C                   EXSR      SUBTI
     C                   Z-ADD     0             XXCLAS
     C****                 ENDIF
     C   11              EXSR      SUBYR
     C*
     C*  Get the next Group.
     C*
     C  N22              SETON                                            98
     C   22F1KY1         READE     PF101                                  98
     C                   ENDDO
     C*
     C   11              EXSR      SUBL1
     C*
     C*  Get the next Group.
     C*
     C  N21              SETON                                            99
     C   21FNKY1         READE     PFN01                                  99
     C                   ENDDO
     C**
     C                   MOVE      '1'           *INLR
     C*
     C   11              EXSR      SUBLR
      /Free
       //***** Beg Add ***** 08/26/14 ******************************************
       If *in11 = '1';
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ******************************************
      /End-free
     C*
      *==================================================================*
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      *==================================================================*
     C     SR1ST         BEGSR
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  FUND MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     FNKY1         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  FUND DETAIL
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C     F1KY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  EMPLOYER PAYROLL FILE
     C*
     C     EPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRATE
     C*
     C*   KEY LIST FOR THE CLAIMS FILE
     C*
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    ELPFDT
     C*
     C     RRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C*
     C*  PAYROLL CLASSIFICATION FILE
     C*
     C     PRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEFDT
     C*
     C     PRKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    EPCLAS
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EPEMP#
     C                   KFLD                    EPDIV
     C                   KFLD                    EPFYR
     C*
     C*   KEY LIST FOR THE CLAIMS FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCLAS            5 0
     C*
     C*   KEY LIST FOR THE RESERVE HISTORY FILE
     C*
     C     RHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    CMCASE
     C*
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
     C     *LIKE         DEFINE    FNFND         SVFND
     C     *LIKE         DEFINE    F1FYR         SVFYR
     C     *LIKE         DEFINE    ELPFDT        XXASOF
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELRTCD        XXRATE
     C     *LIKE         DEFINE    ELPFDT        XXEFDT
     C     *LIKE         DEFINE    RSJRST        XXST
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQASOF        X8ASOF            8 0
     C                   MOVE      QQMODE        XXMODE            1
     C*
     C                   Z-ADD     X8ASOF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXASOF
     C*
     C     XXFND         COMP      *ZEROS                                 21
     C     XXFYR         COMP      *ZEROS                                 22
     C     QQTYPO        IFEQ      'P'
     C     QQTYPO        OREQ      'B'
     C                   SETON                                        11
     C                   END
     C     QQTYPO        IFEQ      'F'
     C     QQTYPO        OREQ      'B'
     C                   SETON                                        12
     C                   END
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C* * * INITIALIZE FIELDS
     C                   TIME                    DSDTTM
     C*
     C     UPKY          CHAIN     WMUPL                              60
     C     COKY          CHAIN     SMCOL                              60
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
     C     *IN11         IFEQ      *ON
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
      ***** Beg Del ***** 08/26/14 ***************************************
     C************       EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ***************************************
     C                   END
     C*
     C                   MOVE      '1'           *IN10                          1ST TIME INDIC.
     C*
     C                   ENDSR
     C*
      *==================================================================*
      *   GRPBRK - Group break
      *==================================================================*
     C     GRPBRK        BEGSR
     C*
     C     *IN21         DOWEQ     *ON
     C     *IN99         ANDEQ     *OFF
     C     FNTEST        ANDEQ     'Y'
     C     FNKY1         READE     PFN01                                  99
     C                   ENDDO
     C*
     C                   MOVEL     FNNAME        XFNAME           60
     C     FNANAM        IFNE      *BLANK
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF
     C*
     C                   Z-ADD     0             L1PYRL           13 2
     C                   Z-ADD     0             L1EMPR           13 2
     C                   Z-ADD     0             L1TOTC            7 0
     C                   Z-ADD     0             L1ITOT           13 2
     C*
     C                   Z-ADD     FNFND         SVFND
     C                   Z-ADD     FNFND         XXFND
     C*
     C                   ENDSR
     C*
      *==================================================================*
      *   YRBRK - Policy year break
      *==================================================================*
     C     YRBRK         BEGSR
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
     C     QQDAYS        OREQ      0
     C                   Z-ADD     365           FDAYS
     C                   ELSE
     C                   Z-ADD     QQDAYS        FDAYS             5 0
     C                   END
     C*
     C*  GET NUMBER OF POLICY DAYS
     C*
     C                   Z-ADD     F1STPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     X8ASOF        XQTO
     C*
     C     F1ENPO        IFLT      XXASOF
     C                   Z-ADD     F1ENPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO
     C                   END
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
     C*
     C                   Z-ADD     QQDAYS        EDAYS             5 0
     C*
     C     XXASOF        IFGT      F1ENPO
     C                   Z-ADD     FDAYS         EDAYS
     C                   END
     C     EDAYS         DIV       FDAYS         XXPROR            5 4
     C                   END                                                    FNCGFL <> Y
     C*
     C*  INITIALIZE FIELDS
     C*
     C                   Z-ADD     0             XXEMP#
     C*
     C                   Z-ADD     0             YRPYRL           13 2
     C                   Z-ADD     0             YREMPR           13 2
     C                   Z-ADD     0             YRTOTC            7 0
     C                   Z-ADD     0             YRITOT           13 2
     C*
     C                   Z-ADD     F1FYR         SVFYR
     C                   Z-ADD     F1FYR         XXFYR
     C*
     C   11              EXCEPT    HEAD
     C*
     C                   ENDSR
      *==================================================================*
      *   GET TOTAL INCURRED, ACCUM TOTALS ** Subroutine **   S U B T I  *
      *==================================================================*
     C*
     C     SUBTI         BEGSR
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     CMIDT         IFLE      XXASOF
     C*
     C                   ADD       1             CLMS              5 0          COUNT CLAIMS
     C**
     C                   Z-ADD     *ZERO         TOTRES            9 2
     C*
     C*   ACCUMULATE RESERVE AMOUNTS
     C*
     C     RHKY          CHAIN     PRH01                              60
     C     *IN60         DOWEQ     '0'
     C     RHEFDT        ANDLE     XXASOF
     C                   XFOOT     RHSU          TOTRES
     C                   ADD       TOTRES        TOTINC
     C     RHKY          READE     PRH01                                  60
     C                   END
     C*
     C                   ENDIF
     C*
     C     CMKY          READE     PCM01                                  60
     C                   END
     C*
     C*  CALCULATE PRORATA PREM
     C*
     C     CLEMPR        MULT(H)   XXPROR        XXPREM           11 0
     C                   Z-ADD     XXPREM        CLEMPR
     C*
     C*  CALCULATE LOSS AND FREQUENCY RATIO
     C*
     C                   EXSR      SUBLF
     C*
     C*  OUTPUT DETAIL LINE ITEM
     C*
     C     *IN11         IFEQ      '1'
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    NOTE
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   ENDIF
     C                   EXCEPT    DETAIL
     C*
     C*  UPDATE FUND AND FINAL TOTALS FOR CLASS
     C*
     C                   ADD       XXPYRL        YRPYRL
     C                   ADD       CLEMPR        YREMPR
     C                   ADD       CLMS          YRTOTC
     C                   ADD       TOTINC        YRITOT
     C*
     C                   ENDIF
     C*
     C     *IN12         IFEQ      '1'
     C                   EXSR      LOADF
     C                   ENDIF
     C*
     C                   Z-ADD     0             COUNT1            5 0
     C                   Z-ADD     0             XXPYRL           11 2
     C                   Z-ADD     0             CLEMPR           11 2
     C                   Z-ADD     0             CLMS              5 0
     C                   Z-ADD     0             TOTINC           11 2
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
      *   YR Calculations (YEAR)           ** Subroutine **   S U B Y R  *
      *==================================================================*
     C*
     C     SUBYR         BEGSR
     C*
     C*    CALCULATE LOSS RATIO
     C*  ROUND LOSS TO NEAREST TENTH
     C     YREMPR        IFNE      *ZERO
     C     YRITOT        DIV(H)    YREMPR        X1LOSS            7 3
     C                   ELSE
     C                   Z-ADD     *ZERO         X1LOSS
     C                   END
     C     X1LOSS        MULT      100           YRLOSS            7 2
     C*
     C**   CALCULATE FREQUENCY RATIO
     C     YREMPR        DIV(H)    1000          THPREM            9 2
     C*
     C     THPREM        IFNE      *ZERO
     C     YRTOTC        DIV(H)    THPREM        YRFREQ            5 2
     C                   ELSE
     C                   Z-ADD     *ZERO         YRFREQ
     C                   END
     C*
     C                   ADD       YRPYRL        L1PYRL
     C                   ADD       YREMPR        L1EMPR
     C                   ADD       YRTOTC        L1TOTC
     C                   ADD       YRITOT        L1ITOT
     C*
     C*  OUTPUT TOTALS
     C*
     C   OF              EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   EXCEPT    TOTYR
     C**
     C                   ENDSR
     C*
      *==================================================================*
      *   L1 Calculations (Fund)           ** Subroutine **   S U B L 1  *
      *==================================================================*
     C*
     C     SUBL1         BEGSR
     C*
     C*    CALCULATE LOSS RATIO
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
     C*
     C                   ADD       L1PYRL        LRPYRL           13 2
     C                   ADD       L1EMPR        LREMPR           13 2
     C                   ADD       L1TOTC        LRTOTC            7 0
     C                   ADD       L1ITOT        LRITOT           13 2
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
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   EXCEPT    TOTLR
     C**
     C                   ENDSR
     C*
     C*==================================================================*
     C*   NON-COMMON EXP DATE SUBROUTINE   ** Subroutine ** S U B N C    *
     C*==================================================================*
     C*
     C     SUBNC         BEGSR
     C*
     C     ELKY1         CHAIN     PEL01                              60
     C                   MOVE      ELRTCD        XXRATE
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
     C*
     C*  GET NUMBER OF DAYS ACTIVE
     C*
     C     FNCGFL        IFEQ      'Y'
     C*
     C                   Z-ADD     *ZEROS        XXDIV
     C     ELKY          CHAIN     PEL01                              60
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     X8ASOF        XQTO
     C*
     C     ELPTDT        IFLT      XXASOF
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO
     C                   END
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
     C                   Z-ADD     QQDAYS        EDAYS             5 0
     C*
     C     EDAYS         DIV       365           XXPROR
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*==================================================================*
     C*  Load the Outfile                  ** Subroutine **   LOADF      *
     C*==================================================================*
     C     LOADF         BEGSR
     C*
     C                   CLEAR                   PLJ01
     C*
     C                   MOVE      XXCO#         LJCO#
     C                   MOVEL     CONAME        LJCNAM
     C                   MOVE      XXFND         LJFND
     C                   MOVEL     FNNAME        LJFNAM
     C                   MOVE      DSDATE        LJRNDT
     C                   MOVE      X8ASOF        LJASOF
     C                   MOVE      XXFYR         LJFYR
     C                   MOVE      XXCLAS        LJCLAS
     C                   MOVEL     PRDESC        LJCLDS
     C                   MOVE      COUNT1        LJ#EMP
     C                   MOVE      XXPYRL        LJPYRL
     C                   MOVE      CLEMPR        LJMAN
     C                   MOVE      CLMS          LJ#CLM
     C                   MOVE      TOTINC        LJTOTI
     C                   MOVE      LOSS          LJLOSS
     C                   MOVE      FREQ          LJFREQ
     C*
     C                   WRITE     PLJ01
     C*
     C                   ENDSR
     C*==================================================================*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ***************************************
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
      ***** End Del ***** 08/26/14 ***************************************
      ***** Beg Add ***** 08/05/15 ***************************************
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
      ***** Beg Add ***** 08/05/15 ***************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ***************************************
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
      ***** End Del ***** 08/26/14 ***************************************
     O**   SELECTION CRITERIA
     O          E            ROUTE          1
     O                                           20 'GROUP        :'
     O              N21      XXFND         Z     24
     O               21                          31 'ALL GROUPS'
     O          E            ROUTE          1
     O                                           20 'POLICY YEAR  :'
     O              N22      XXFYR         Z     24
     O               22                          30 'ALL YEARS'
     O          E            ROUTE          1
     O                                           20 'AS OF DATE   :'
     O                       X8ASOF        Y   +  1
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE
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
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                              'GROUP:'
     O                       XXFND         Z   +  1
     O                       XFNAME            +  1
     O                                          121 'AS OF DATE:'
     O                       X8ASOF        Y    132
     O          E            HEAD           2
     O                                           12 'POLICY YEAR:'
     O                       XXFYR         Z   +  1
     O          E            HEAD           1
     O                                           45 '# OF'
     O                                           79 'EARNED'
     O                                           92 '# OF'
     O                                          120 'LOSS'
     O                                          128 'FREQ'
     O          E            HEAD           2
     O                                           05 'CLASS'
     O                                           18 'DESCRIPTION'
     O                                           47 'EMPLOYERS'
     O                                           64 'TOTAL PAYROLL'
     O                                           83 'MANUAL PREMIUM'
     O                                           93 'CLAIMS'
     O                                          111 'TOTAL INCURRED'
     O                                          128 'RATIO   RATIO'
     O          E            DETAIL         1
     O                       XXCLAS        Z     05
     O                       PRDESC              37
     O                       COUNT1        3     45
     O                       XXPYRL        J     65
     O                       CLEMPR        J     84
     O                       CLMS          3     93
     O                       TOTINC        J    112
     O                       LOSS          J    121
     O                       FREQ          J    129
     O          E            NOTE        1  1
     O                                           32 'RATIOS ARE DERIVED FROM '
     O                                           55 'MANUAL PREMIUM AMOUNTS.'
     O          E            TOTYR       1  1
     O                                           23 'YEAR TOTALS:'
     O                       YRPYRL        J     65
     O                       YREMPR        J     84
     O                       YRTOTC        3     93
     O                       YRITOT        J    112
     O                       YRLOSS        J    121
     O                       YRFREQ        J    129
     O          E            TOTL1       1  1
     O                                           23 'GROUP TOTALS:'
     O                       L1PYRL        J     65
     O                       L1EMPR        J     84
     O                       L1TOTC        3     93
     O                       L1ITOT        J    112
     O                       L1LOSS        J    121
     O                       L1FREQ        J    129
     O          E            TOTLR       2  1
     O                                           23 'FINAL TOTALS:'
     O                       LRPYRL        J     65
     O                       LREMPR        J     84
     O                       LRTOTC        3     93
     O                       LRITOT        J    112
     O                       LRLOSS        J    121
     O                       LRFREQ        J    129
     O          E            TOTLR          1 60
     O                                           32 'RATIOS ARE DERIVED FROM '
     O                                           55 'MANUAL PREMIUM AMOUNTS.'
