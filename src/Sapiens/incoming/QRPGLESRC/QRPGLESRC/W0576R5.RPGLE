     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE W0576R5 - Display Summary Payments
     H Bnddir('WTBNDDIR')
      *****************************************************************
      * PROGRAM DESCRIPTION:                                          *
      *                                                               *
      * This Pgm will accumulate Payment Detail amounts by Transaction*
      * Code and Payment Value 1 Code and then display a summary      *
      * display screen of each Codes' Total.                          *
      *                                                               *
      * CALLED FROM:                                                  *
      *   W0576R                                                      *
      *                                                               *
      * CALLS TO:                                                     *
      *   SRFRLS                                                      *
      *                                                               *
      * CREATED: 05/07/97  c:30  Project:  60AMQ                      *
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 06/09/20 C:298 Project: SY420
      *                Mask SSN
      *                                                               *
      * 04/07/11 C:062 PROJECT: WC133 76470                           *
      *                Expand the arrays from 100 to 300.             *
      *                                                               *
      * 05/25/07 C:191 PROJECT: WC1515                                *
      *                 Check claims authority                        *
      *                                                               *
      * 03/16/04 C:53 Support: WC072/41644                            *
      *               Added code to check for weeks/days in addition  *
      *               to the amount for summary code to display.      *
      *                                                               *
      * 09/17/03 C:15 Support: WC065/39083                            *
      *               Added code to display the weeks and days in a   *
      *               more logical way when one figure is positive    *
      *               and the other figure is negative.               *
      *                                                               *
      * 07/17/03 C:53 Support: WC065/38013                            *
      *               Correct weeks/days calc.  When a neg recovery   *
      *               is processed, the weeks/days should be added.   *
      *                                                               *
      * 04/01/99  C:33  SEQ#: WA095/15404                             *
      *                 DO NOT CALCULATE TOTAL WEEKS/DAYS FOR VOIDS   *
      *****************************************************************
      * INDICATORS:                                                   *
      *                                                               *
      *   11  COMMAND KEY WAS PRESSED                                 *
      *****************************************************************
      *
      *  Sub File Display Screen
     FW0576F5   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:RRN1)
     F                                     INFDS(FIDS01)
      *
      *  Company Master
     FSMCOL     IF   E           K DISK
      *
      *  Fund Master
     FWMFNL     IF   E           K DISK
      *
      *  User Parameters
     FWMUPL     IF   E           K DISK
      *
      *  Claim Master (Key= CO#/FND/FYR/CASE)
     FWMCML1    IF   E           K DISK
      *
      *  Claimant Demographics (Key= SS#)
     FWMCDL     IF   E           K DISK
      *
      *  Payment Detail (Key= CO#/FND/FYR/CASE)
     FWDPAL     IF   E           K DISK
      *
      *  Table file
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PPA01)

      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
      ***  End   Add  ***  06/09/20  ****************************
     D*** BEGIN CHG *** 04/07/11 **************************************
      *
      *  Payment Transaction Code Arrays
     D**********          CD        100  2               PMT CODES
     D**********          DS        100 40               CODE DESC
     D**********          AM        100 11 2             CODE AMTS
     D**********          WK        100  5 0             CODE WKS
     D**********          DA        100  5 0             CODE DYS
      *  Payment Value 1 Arrays
     D**********          CD2       100  2               BR2 CODES
     D**********          DS2       100 40               BR2 CODE DESC
     D**********          AM2       100 11 2             BR2 CODE AMT
     D**********          WK2       100  5 0             BR2 # OF WKS
     D**********          DA2       100  5 0             BR2 # OF DYS
      *
      *  Payment Transaction Code Arrays
     D CD              S              2    DIM(300)                             PMT CODES
     D DS              S             40    DIM(300)                             CODE DESC
     D AM              S             11  2 DIM(300)                             CODE AMTS
     D WK              S              5  0 DIM(300)                             CODE WKS
     D DA              S              5  0 DIM(300)                             CODE DYS
      *  Payment Value 1 Arrays
     D CD2             S              2    DIM(300)                             BR2 CODES
     D DS2             S             40    DIM(300)                             BR2 CODE DESC
     D AM2             S             11  2 DIM(300)                             BR2 CODE AMT
     D WK2             S              5  0 DIM(300)                             BR2 # OF WKS
     D DA2             S              5  0 DIM(300)                             BR2 # OF DYS
     D*** END   CHG *** 04/07/11 **************************************
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      *
     D NOTFND          C                   CONST('Code Not Found')
     D SUMTRN          C                   CONST('Payment Transaction -
     D                                     Code Summary')
     D SUMPV1          C                   CONST('Payment Value 1 Code-
     D                                      Summary')
     D PRDS            C                   CONST('Summary Payment Disp-
     D                                     lay')
     D*** BEGIN ADD *** 04/07/11 **************************************
     D SIZE            C                   CONST(300)
     D*** END   ADD *** 04/07/11 **************************************
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *   KEY LIST FOR THE COMPANY FILE
      *

     C     COKEY         KLIST
     C                   KFLD                    UPCO#
      *
      *   KEY LIST FOR THE FUND
      *
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
      *
      *  USER PARAMETER FILE
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
      *  TABLE FILE KEY
      *
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
      *
      *  CHECK PAYMENT DETAIL FILE KEY LIST
      *
     C     PAKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
      *  CLAIMANT DEMOGRAPHICS FILE
      *
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
      *
      *  CLAIM MASTER FILE
      *
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
      *
      * ---------------------------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQTRCD            2
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQRECD            2
     C                   PARM                    QQRETP            1
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
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C*** END ADD ****** 05/25/07 ********************************************
      * ---------------------------------------------------------------
      *
      *  INITIALIZE
      *
     C     TOP           TAG
      *
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
      *
     C                   MOVEL(P)  PRDS          ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
      *
      *  Build Subfile of Summary Payment records
      *
     C                   EXSR      SUMMRY
      *
      *   DISPLAY ALL FORMATS
      *
     C     STRT          TAG
      *
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C                   SETON                                        01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01C
     C                   SETOFF                                       01
     C                   ENDIF
      *
      ********************************
      *   PROCESS ALL COMMAND KEYS   *
      ********************************
      *
     C     *IN11         IFEQ      '1'
      *
      *   COMMAND 01 = END OF JOB
      *
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD            1
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF
      *
      *   COMMAND 02 = PRIOR SCREEN
      *
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF
      *
     C                   ENDIF
      *
      *   GOTO THE APPROPRIATE SCREEN
      *
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
      *
      *   PROCESS REQUEST SCREEN
      *
     C     SCR01T        TAG
      *
     C                   MOVE      '*'           QQRTCD
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
      *
     C                   MOVE      '1'           *INLR
      *
      *===============================================================
      * SUMMRY - Sub to build subfile of Summary Payments
      *===============================================================
     C     SUMMRY        BEGSR
      *
     C                   Z-ADD     *ZERO         RRN1              3 0
      *
      *  Get the User Parameter file
      *
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   ENDIF
     C                   MOVE      UPCO#         XXCO#
      *
      *  Get the Company file
      *
     C                   MOVE      *BLANK        SCCONM
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   ENDIF
      *
      *  Access Fund Master
      *
     C                   MOVE      *BLANK        SCFNNM
     C                   MOVE      *ZERO         SCFND
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      FNNAME        SCFNNM
     C                   MOVE      FNFND         SCFND
     C                   ENDIF
      *
      *  Get the Claim Master file
      *
     C                   MOVE      *BLANK        SCSS#
     C                   Z-ADD     *ZERO         SCCASE
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
      *****  Begin Chg  ***  06/09/20  **********************
     C*****              MOVE      CMSS#         SCSS#
      /Free
       scss# = MaskData('SSN':'cmss#':cmss#);
      /End-free
      *****  End Chg  ***  06/09/20  **********************
     C                   Z-ADD     CMCASE        SCCASE
     C                   Z-ADD     CMFYR         SCFYR
      *
      *  Get the Claimant Name
      *
     C     CDKEY         CHAIN     PCD01                              61
     C     *IN61         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SCCNAM
     C                   ELSE
     C                   MOVE      *BLANK        SCCNAM
     C                   ENDIF
      *
     C                   CALL      'WA006R'
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM                    XXFYR
     C                   PARM                    XXCASE
     C                   PARM                    SCCS01
     C                   PARM                    SCCS02
      *
      *   Load Payment Codes into array
      *
     C                   MOVE      *BLANKS       CD
     C                   MOVE      *BLANKS       DS
     C                   Z-ADD     *ZERO         AM
     C                   Z-ADD     *ZERO         WK
     C                   Z-ADD     *ZERO         DA
     C                   Z-ADD     1             X                 3 0
     C     TBKEY         CHAIN     PPC01                              50
      *
     C     *IN50         DOWEQ     '0'
     C*** BEGIN CHG *** 04/07/11 **************************************
     C********** X         ANDLT100
     C     X             ANDLT     SIZE
     C*** END   CHG *** 04/07/11 **************************************
     C                   MOVEL     PCKEY         CD(X)
     C                   MOVEL     PCDESC        DS(X)
     C                   ADD       1             X
     C     TBKEY         READE     PPC01                                  50
     C                   ENDDO
      *
     C*** BEGIN CHG *** 04/07/11 **************************************
     C********** X         IFLE 100
     C     X             IFLE      SIZE
     C*** END   CHG *** 04/07/11 **************************************
     C                   MOVEL     NOTFND        DS(X)
     C                   Z-ADD     X             NOTFD             3 0
     C                   ENDIF
      *
      *   Load Payment Value 1 Codes into array
      *
     C                   MOVE      *BLANKS       CD2
     C                   MOVE      *BLANKS       DS2
     C                   Z-ADD     *ZERO         AM2
     C                   Z-ADD     *ZERO         WK2
     C                   Z-ADD     *ZERO         DA2
     C                   Z-ADD     1             X                 3 0
     C     TBKEY         CHAIN     PB201                              50
      *
     C     *IN50         DOWEQ     '0'
     C*** BEGIN CHG *** 04/07/11 **************************************
     C********** X         ANDLT100
     C     X             ANDLT     SIZE
     C*** END   CHG *** 04/07/11 **************************************
     C                   MOVEL     B2KEY         CD2(X)
     C                   MOVEL     B2DESC        DS2(X)
     C                   ADD       1             X
     C     TBKEY         READE     PB201                                  50
     C                   ENDDO
      *
     C*** BEGIN CHG *** 04/07/11 **************************************
     C********** X         IFLE 100
     C     X             IFLE      SIZE
     C*** END   CHG *** 04/07/11 **************************************
     C                   MOVEL     NOTFND        DS2(X)
     C                   Z-ADD     X             NOTFD2            3 0
     C                   ENDIF
      *
      *   Access ALL Claim Payment Detail records
      *
     C                   Z-ADD     *ZERO         SCTAMT                         Total Amount
      *
     C     PAKEY         CHAIN     PPA01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
      *   Payment Code must be equal if not blank.
      *
     C     QQTRCD        IFNE      *BLANK
     C     PATRCD        CABNE     QQTRCD        NEXT
     C                   END
      *
      *   Payment Type must be equal if not blank.
      *
     C     QQTYPE        IFNE      *BLANK
     C     PATYPE        CABNE     QQTYPE        NEXT
     C                   END
      *
      *   Reserve Code must be equal if not blank.
      *
     C     QQRECD        IFNE      *BLANK
     C     PARECD        CABNE     QQRECD        NEXT
     C                   END
      *
      *   Reserve Type must be equal if not blank.
      *
     C     QQRETP        IFNE      *BLANK
     C     PARETP        CABNE     QQRETP        NEXT
     C                   END
      *
      *   Find Payment Code index
      *
     C                   Z-ADD     1             X                 3 0
     C     PATRCD        LOOKUP    CD(X)                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     NOTFD         X
     C                   ENDIF
      *
      *   Find Payment Value 1 code index
      *
     C                   Z-ADD     1             Y                 3 0
     C     PABR2         LOOKUP    CD2(Y)                                 60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     NOTFD2        Y
     C                   ENDIF
      *
      *   Accumulate Amounts for Payment Code and Payment Value 1
      *
      *
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'A'
     C     PATYPE        OREQ      'Z'
     C                   ADD       PAAMT         AM(X)
     C                   ADD       PAAMT         AM2(Y)
     C                   ADD       PAAMT         SCTAMT
     C                   ELSE                                                   PATYPE= V, S, R
     C                   SUB       PAAMT         AM(X)
     C                   SUB       PAAMT         AM2(Y)
     C                   SUB       PAAMT         SCTAMT
     C                   ENDIF
      *
      *  Accumulate Days and Weeks
      *
     C                   EXSR      SRWKDY
      *
     C     NEXT          TAG
      *
     C     PAKEY         READE     PPA01                                  60
     C                   ENDDO
      *
      *   Clear Subfile
      *
     C                   SETON                                        04
     C                   WRITE     SCR01C
     C                   SETOFF                                       04
      *
     C                   SETOFF                                       05        SFLEND IND
      *
      *  Build subfile of Payment Codes using arrays
      *
     C                   EXSR      SRBLNK
     C                   MOVE      *BLANK        SCTYPE
     C                   MOVE      *BLANK        SCCODE
     C                   MOVE      *ZERO         SCAMT
     C                   MOVE      *ZERO         SCWKS
     C                   MOVE      *ZERO         SCDYS
     C                   MOVEL(P)  SUMTRN        SCDESC
     C                   MOVE      *ON           *IN71
     C                   ADD       1             RRN1
     C                   WRITE     SCR01S
      *
     C                   MOVE      *OFF          *IN71
     C                   Z-ADD     1             X
     C*** BEGIN CHG *** 04/07/11 **************************************
     C**********           DO   100       X
     C                   DO        SIZE          X
     C*** END   CHG *** 04/07/11 **************************************
     C*
     C*** Begin add *** 09/17/03 **************************************
     C     WK(X)         IFLT      *ZERO
     C     DA(X)         ANDGT     *ZERO
     C                   ADD       1             WK(X)
     C                   SUB       7             DA(X)
     C                   ENDIF
     C*
     C     WK(X)         IFGT      *ZERO
     C     DA(X)         ANDLT     *ZERO
     C                   SUB       1             WK(X)
     C                   ADD       7             DA(X)
     C                   ENDIF
     C*** End add ***** 09/17/03 **************************************
     C*
     C     AM(X)         IFNE      *ZERO
     C*** BEG ADD *** 03/16/04 ****************************
     C     WK(X)         ORNE      *ZERO
     C     DA(X)         ORNE      *ZERO
     C*** END ADD *** 03/16/04 ****************************
     C                   MOVEL     'Trans'       SCTYPE
     C                   MOVEL     CD(X)         SCCODE
     C                   Z-ADD     AM(X)         SCAMT
     C                   Z-ADD     WK(X)         SCWKS
     C                   Z-ADD     DA(X)         SCDYS
     C                   MOVEL(P)  DS(X)         SCDESC
     C                   ADD       1             RRN1
     C                   WRITE     SCR01S
     C                   ENDIF
      *
     C                   ENDDO
      *
      *  Build subfile of Payment Value 1 codes using arrays
      *
     C                   EXSR      SRBLNK
     C                   MOVE      *BLANK        SCTYPE
     C                   MOVE      *BLANK        SCCODE
     C                   MOVE      *ZERO         SCAMT
     C                   MOVE      *ZERO         SCWKS
     C                   MOVE      *ZERO         SCDYS
     C                   MOVEL(P)  SUMPV1        SCDESC
     C                   MOVE      *ON           *IN71
     C                   ADD       1             RRN1
     C                   WRITE     SCR01S
      *
     C                   MOVE      *OFF          *IN71
     C                   Z-ADD     1             X
     C*** BEGIN CHG *** 04/07/11 **************************************
     C**********           DO   100       X
     C                   DO        SIZE          X
     C*** END   CHG *** 04/07/11 **************************************
     C*
     C*** Begin add *** 09/17/03 **************************************
     C     WK2(X)        IFLT      *ZERO
     C     DA2(X)        ANDGT     *ZERO
     C                   ADD       1             WK2(X)
     C                   SUB       7             DA2(X)
     C                   ENDIF
     C*
     C     WK2(X)        IFGT      *ZERO
     C     DA2(X)        ANDLT     *ZERO
     C                   SUB       1             WK2(X)
     C                   ADD       7             DA2(X)
     C                   ENDIF
     C*** End add ***** 09/17/03 **************************************
      *
     C     AM2(X)        IFNE      *ZERO
     C                   MOVEL     'PV1'         SCTYPE
     C                   MOVEL     CD2(X)        SCCODE
     C                   Z-ADD     AM2(X)        SCAMT
     C                   Z-ADD     WK2(X)        SCWKS
     C                   Z-ADD     DA2(X)        SCDYS
     C                   MOVEL(P)  DS2(X)        SCDESC
     C                   ADD       1             RRN1
     C                   WRITE     SCR01S
     C                   ENDIF
      *
     C                   ENDDO
      *
     C                   Z-ADD     1             #@$3
     C                   MOVE      *ON           *IN05
     C                   EXSR      SRBLNK
      *
     C                   ENDSR
      *
      *===============================================================
      *  SRBLNK - Subroutine to write Blank Subfile rec
      *===============================================================
     C     SRBLNK        BEGSR
      *
     C                   MOVE      *BLANK        SCTYPE
     C                   MOVE      *BLANK        SCCODE
     C                   Z-ADD     *ZERO         SCAMT
     C                   Z-ADD     *ZERO         SCWKS
     C                   Z-ADD     *ZERO         SCDYS
     C                   MOVEL(P)  *BLANK        SCDESC
     C                   ADD       1             RRN1
     C                   WRITE     SCR01S
      *
     C                   ENDSR
      *
      *===============================================================
      *   SUBROUTINE CALCULATE WEEKS AND DAYS            *
      *===============================================================
     C     SRWKDY        BEGSR
      *
      *  Increment Weeks and Days Arrays
      *
     C     PATYPE        IFEQ      'P'
     C     PAAMT         ANDGT     0
     C     PATYPE        OREQ      'M'
     C     PAAMT         ANDGT     0
     C     PATYPE        OREQ      'A'
     C     PAAMT         ANDGT     0
     C     PATYPE        OREQ      'Z'
     C     PAAMT         ANDGT     0
     C*** BEG ADD *** 07/17/03 **************************************
     C     PATYPE        OREQ      'R'
     C     PAAMT         ANDLT     0
     C     PATYPE        OREQ      'S'
     C     PAAMT         ANDLT     0
     C     PATYPE        OREQ      'V'
     C     PAAMT         ANDLT     0
     C*** END ADD *** 07/17/03 **************************************
      *
     C                   Z-ADD     DA(X)         ##DAYS            5 0
     C                   Z-ADD     WK(X)         ##WKS             5 0
     C                   EXSR      SRADD
     C                   Z-ADD     ##DAYS        DA(X)
     C                   Z-ADD     ##WKS         WK(X)
      *
     C                   Z-ADD     DA2(Y)        ##DAYS
     C                   Z-ADD     WK2(Y)        ##WKS
     C                   EXSR      SRADD
     C                   Z-ADD     ##DAYS        DA2(Y)
     C                   Z-ADD     ##WKS         WK2(Y)
      *
     C                   ELSE
      *
     C                   Z-ADD     DA(X)         ##DAYS            5 0
     C                   Z-ADD     WK(X)         ##WKS             5 0
     C                   EXSR      SRSUB
     C                   Z-ADD     ##DAYS        DA(X)
     C                   Z-ADD     ##WKS         WK(X)
      *
     C                   Z-ADD     DA2(Y)        ##DAYS
     C                   Z-ADD     WK2(Y)        ##WKS
     C***  BEGIN CHG  ***  04/01/99  *********************************
     C**********           EXSR SRADD
     C                   EXSR      SRSUB
     C***  END   CHG  ***  04/01/99  *********************************
     C                   Z-ADD     ##DAYS        DA2(Y)
     C                   Z-ADD     ##WKS         WK2(Y)
      *
     C                   END
      *
     C                   ENDSR
      *
      *===============================================================
      * SRADD - subroutine to Add Weeks and Days
      *===============================================================
     C     SRADD         BEGSR
      *
     C                   ADD       PAWKS         ##WKS
     C     ##DAYS        ADD       PADAYS        XXDAYS            5 0
     C     XXDAYS        IFGE      7
     C     XXDAYS        DIV       7             XXWEEK            1 0
     C                   ADD       XXWEEK        ##WKS
     C                   MULT      7             XXWEEK
     C     XXDAYS        SUB       XXWEEK        ##DAYS
     C                   ELSE
     C                   Z-ADD     XXDAYS        ##DAYS
     C                   END
      *
     C                   ENDSR
      *
      *===============================================================
      * SRSUB - subroutine to Subtract Weeks and Days
      *===============================================================
     C     SRSUB         BEGSR
      *
     C                   SUB       PAWKS         ##WKS
     C     ##DAYS        SUB       PADAYS        XXDAYS
     C     XXDAYS        IFLT      0
     C                   SUB       1             ##WKS
     C     7             ADD       XXDAYS        ##DAYS
     C                   ELSE
     C                   Z-ADD     XXDAYS        ##DAYS
     C                   END
      *
     C                   ENDSR
