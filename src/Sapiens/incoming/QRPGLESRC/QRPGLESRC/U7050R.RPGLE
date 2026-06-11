     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_Qry dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE U7080R - INTERVIEWER LIABILITY REPORT PRT PGM
     F*
     H*****************************************************************
     H*  INDICATORS:                                                  *
     H*  08    Batch Mode                                             *
     H*  09    Demand Mode                                            *
     H*  60    General Chain/Usage                                    *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/11/13 C:287 Project: SY266_QRY                             *
     H* 01/28/10 C:249 Project: SY215                                 *
     H*          C:189 Retrieve brand Name                            *
     H*                                                               *
     H*  03/14/00 C:62 PROJECT: MCMNT                                 *
     H*                CHANGE DATE FORMATS TO 8.0                     *
     H*                                                               *
     H*  12/03/97 C:85 PROJECT: MC145                                 *
     H*                CHANGED PLIST DTMDYY TO DTMDY                  *
     H*                IN SRCVTDAT FOR X1FRDT & X1TODT.               *
     H*                                                               *
     H*  10/20/94  C:20   REQ/PROJ: URMNT                             *
     H*                   1. MODIFIED CODE TO ONLY ACCUMULATE TOTAL   *
     H*                      FOR THE LIABILITY IF HISTORY DATE WAS    *
     H*                      WITHIN DATE RANGE SELECTED               *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FUMPCL     IF   E           K DISK
     F                                     RENAME(PPC01:PPC02)
     FUMPCL1    IF   E           K DISK
     FUMCHL1    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
     FQSYSPRT1  O    F  132        PRINTER OFLIND(*INOF)
     F*

     D                 DS
     D*** BEGIN CHG *** 03/14/00 **************************************
     D**********                              1  120DSDTTM
     D**********                              1   60DSTIME
     D**********                              7  120DSDATE
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D*
     D*** END   CHG *** 03/14/00 **************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* COMPNAY MASTER FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C* OUTPUT ROUTING FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT
     C*
     C* USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     UPKY1         KLIST
     C                   KFLD                    XXINTR
     C*
     C*   KEY LIST FOR THE SUBSCRIBER
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE PRE-CERT FILE
     C*
     C     PCKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXCERT
     C                   KFLD                    XXEXT                          ADD 10/20/94
     C*
     C*   KEY LIST FOR THE PRE-CERT HISTORY FILE
     C*
     C     CHKEY         KLIST
     C                   KFLD                    PCCO#
     C                   KFLD                    PCSUB#
     C                   KFLD                    PCFND
     C                   KFLD                    PCFYR
     C                   KFLD                    PCCASE
     C                   KFLD                    PCCERT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQINTR            3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQTYPE            1
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
     C*** BEGIN DEL *** 12/03/97 **************************************
     C********** DTMDYY    PLIST
     C**********           PARM           SRDTI
     C**********           PARM '*CYMD'   SRFFMT
     C**********           PARM *BLANKS   SRTFMT
     C**********           PARM           SRDTO
     C**********           PARM           SRERR
     C*** END   DEL *** 12/03/97 **************************************
     C*
     C*** BEGIN ADD *** 12/03/97 **************************************
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*** END   ADD *** 12/03/97 **************************************
     C*
     C* INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    PCCO#         XXCO#                          3.0N
     C     *LIKE         DEFINE    PCSUB#        XXSUB#                         5.0N
     C     *LIKE         DEFINE    PCFND         XXFND                          3.0N
     C     *LIKE         DEFINE    SMNAME        XXSBNM                         40A
     C     *LIKE         DEFINE    PCINTR        XXINTR                         3A
     C     *LIKE         DEFINE    PCCERT        XXCERT                         15A
     C     *LIKE         DEFINE    PCEXT         XXEXT                          3.0N
     C     *LIKE         DEFINE    ORTITL        XXTITL                         40A
     C     *LIKE         DEFINE    ORDT          XXFRDT                         7.0N
     C     *LIKE         DEFINE    ORDT          XXTODT                         7.0N
     C*
     C                   Z-ADD     *ZEROS        XXFRDT
     C                   Z-ADD     *ZEROS        XXTODT
     C                   Z-ADD     *ZEROS        X1FRDT            6 0          6.0N
     C                   Z-ADD     *ZEROS        X1TODT            6 0          6.0N
     C                   Z-ADD     *ZEROS        XXOCNT            5 0          5.0N
     C                   Z-ADD     *ZEROS        XXCCNT            5 0          5.0N
     C                   Z-ADD     *ZEROS        XXRCNT            5 0          5.0N
     C                   Z-ADD     *ZEROS        XXTCNT            5 0          5.0N
     C                   Z-ADD     *ZEROS        XXACNT            5 0          5.0N
     C                   Z-ADD     *ZEROS        XXPCNT            5 0          5.0N
     C                   Z-ADD     *ZEROS        XXDCNT            5 0          5.0N
     C                   Z-ADD     *ZEROS        XXOPCT            5 4          5.4N
     C                   Z-ADD     *ZEROS        XXCPCT            5 4          5.4N
     C                   Z-ADD     *ZEROS        XXRPCT            5 4          5.4N
     C                   Z-ADD     *ZEROS        XXAPCT            5 4          5.4N
     C                   Z-ADD     *ZEROS        XXPPCT            5 4          5.4N
     C                   Z-ADD     *ZEROS        XXDPCT            5 4          5.4N
     C                   Z-ADD     *ZEROS        X1OPCT            5 2          5.2N
     C                   Z-ADD     *ZEROS        X1CPCT            5 2          5.2N
     C                   Z-ADD     *ZEROS        X1RPCT            5 2          5.2N
     C                   Z-ADD     *ZEROS        X1APCT            5 2          5.2N
     C                   Z-ADD     *ZEROS        X1PPCT            5 2          5.2N
     C                   Z-ADD     *ZEROS        X1DPCT            5 2          5.2N
     C                   Z-ADD     *ZEROS        TOTDCT            5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTACT            5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTCCT            5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTOCT            5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTPCT            5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTRCT            5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTTCT            5 0          5.0N
     C                   MOVE      *BLANKS       XXTYPE            1            1A
     C                   MOVE      *BLANKS       XXPRT             1            1A
     C                   MOVE      *BLANKS       XXFNNM           60            60A
     C                   MOVE      *BLANKS       XXUSNM           30            30A
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQINTR        XXINTR
     C                   MOVE      QQFRDT        X2FRDT            8 0
     C*
     C                   Z-ADD     X2FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   MOVE      QQTODT        X2TODT            8 0
     C*
     C                   Z-ADD     X2TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   MOVE      QQTYPE        XXTYPE
     C*
     C                   TIME                    DSDTTM
     C*
     C* SET-UP AND INITIALIZE
     C*
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*   SET INDICATORS TO DETERMINE WHICH REPORTS TO PRINT
     C*   SUMMARY/DETAIL
     C*
     C     XXTYPE        IFEQ      'B'
     C                   MOVE      '1'           *IN15
     C                   MOVE      '1'           *IN16
     C                   ELSE
     C     XXTYPE        IFEQ      'D'
     C                   MOVE      '1'           *IN15
     C                   ELSE
     C     XXTYPE        IFEQ      'S'
     C                   MOVE      '1'           *IN16
     C                   END
     C                   END
     C                   END
     C*
     C* CHECK THE SORT SELECTION
     C*
     C                   MOVE      '0'           *IN18
     C                   MOVE      '0'           *IN19
     C*
     C     XXINTR        IFNE      *BLANKS
     C                   MOVE      '1'           *IN18
     C                   END
     C*
     C                   Z-ADD     XXFRDT        SRDTI
     C*** BEGIN DEL *** 12/03/97 **************************************
     C**********           CALL 'SRCVTDAT'DTMDYY
     C*** END   DEL *** 12/03/97 **************************************
     C*
     C*** BEGIN CHG *** 12/03/97 **************************************
     C                   CALL      'SRCVTDAT'    DTMDY
     C*** END   CHG *** 12/03/97 **************************************
     C                   MOVEL     SRDTO         X1FRDT            6 0
     C*
     C                   Z-ADD     XXTODT        SRDTI
     C*** BEGIN DEL *** 12/03/97 **************************************
     C**********           CALL 'SRCVTDAT'DTMDYY
     C*** END   DEL *** 12/03/97 **************************************
     C*
     C*** BEGIN CHG *** 12/03/97 **************************************
     C                   CALL      'SRCVTDAT'    DTMDY
     C*** END   CHG *** 12/03/97 **************************************
     C                   MOVEL     SRDTO         X1TODT            6 0
     C*
     C* GET THE USER PARAMETERS FILE.
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C   60              MOVE      *BLANK        UPUSNM
     C*
     C* CHAIN TO THE COMPANY FILE
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C* GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C*
     C* PRINT THE DISTRIBUTION INFORMATION
     C*
     C     *IN60         IFEQ      '0'
     C                   MOVEL     ORTITL        XXTITL
      ***** Beg Del ***** 08/26/14 ************************************
     C*********15              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C*
     C                   END
     C*
     C*   GET THE DISTRIBUTION INFORMATION FOR QSYSPRT1
     C*
     C                   MOVE      '1'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C*********16              EXCEPT    ROUTE1
      ***** End Del ***** 08/26/14 ************************************
     C*
     C                   END
     C*
     C* GET THE SUBSCRIBER INFORMATION FOR THE HEADING
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSBNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXSBNM
     C                   END
     C*
     C* GET THE FUND INFORMATION FOR THE HEADING
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   END
     C*
     C* PRINT HEADINGS
     C*
     C   15              EXCEPT    HEAD
     C   16              EXCEPT    HEAD1
     C*
     C* PROCESS THE CORRECT LOGICAL
     C*
     C                   READ      PPC01                                  50
     C*
     C* GET THE INTERVIEWER INFORMATION
     C*
     C                   MOVE      *BLANKS       XXINTR
     C                   MOVEL     PCINTR        XXINTR
     C     UPKY1         CHAIN     PUP02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     UPUSNM        XXUSNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXUSNM
     C                   END
     C*
     C* IF NO RECORDS PRINT ERROR MESSAGE
     C*
     C     *IN50         IFEQ      '1'
     C   15              EXCEPT    ERRMSG
     C   16              EXCEPT    ERRMS1
     C                   GOTO      TERM
     C                   END                                                    END *IN60
     C*
     C* START LOOP
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C* CHECK FOR CHANGE IN INTERVIEWER - IF CHANGE PRINT DETAIL LINE
     C*
     C     PCINTR        IFNE      XXINTR
     C                   EXSR      SRINTR
     C                   END
     C*
     C* READ PRE-CERT HISTORY FILE
     C*
     C     CHKEY         CHAIN     PCH01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C* IF THE HISTORY DATE IS WITHIN THE SELECTED DATE RANGE PROCESS
     C*
     C     CHEFDT        IFGE      XXFRDT
     C     CHEFDT        ANDLE     XXTODT
     C*
     C* CHECK OPEN/CLOSE STATUS AND ADD TO COUNT FOR EACH
     C*
     C     CHOCST        IFEQ      'O'
     C                   ADD       1             XXOCNT
     C                   END
     C*
     C     CHOCST        IFEQ      'C'
     C                   ADD       1             XXCCNT
     C                   END
     C*
     C     CHOCST        IFEQ      'R'
     C                   ADD       1             XXRCNT
     C                   END
     C*
     C* CHECK TO STATUS AND ADD TO TOTALS
     C*
     C                   MOVE      CHCERT        XXCERT
     C                   Z-ADD     CHEXT         XXEXT
     C     PCKEY         CHAIN     PPC02                              60
     C     *IN60         IFEQ      '0'
     C     PCLIAB        IFEQ      'A'
     C                   ADD       1             XXACNT
     C                   END                                                    END = 'A'
     C     PCLIAB        IFEQ      'P'
     C                   ADD       1             XXPCNT
     C                   END                                                    END = 'P'
     C     PCLIAB        IFEQ      'D'
     C                   ADD       1             XXDCNT
     C                   END                                                    END = 'D'
     C                   END                                                    END *IN60
     C*
     C* ADD RECORS TO ACCUMULATED TOTAL ACTIVITY COUNT
     C*
     C                   ADD       1             XXTCNT
     C*
     C* READ NEXT HISTORY RECORD FOR CURRENT PRE-CERT
     C*
     C                   END                                                    END XXFRDT/TODT
     C     CHKEY         READE     PCH01                                  40
     C                   END                                                    END DOWEQ 40
     C                   READ      PPC01                                  50
     C*
     C                   END                                                    END DOW 50
     C*
     C* PRINT TOTALS FOR ALL PROCESSED INTERVIEWERS
     C*
     C                   EXSR      SRINTR
     C*
     C* IF NO RECORDS WERE SELECTED BECAUSE OUTSIDE OF DATE RANGE
     C* PRINT ERRO MSG
     C*
     C     TOTOCT        IFEQ      0
     C   15              EXCEPT    ERRMSG
     C   16              EXCEPT    ERRMS1
     C                   END
     C*
     C   15              EXCEPT    TOT
     C*
     C* CALCULATE THE OPEN/CLOSED AND LIABILITY PERCENTAGES
     C*
     C     TOTOCT        IFGT      *ZEROS
     C     TOTOCT        DIV       TOTTCT        XXOPCT
     C     XXOPCT        MULT      100           X1OPCT
     C                   ELSE
     C                   Z-ADD     *ZEROS        X1OPCT
     C                   END
     C*
     C     TOTCCT        IFGT      *ZEROS
     C     TOTCCT        DIV       TOTTCT        XXCPCT
     C     XXCPCT        MULT      100           X1CPCT
     C                   ELSE
     C                   Z-ADD     *ZEROS        X1CPCT
     C                   END
     C*
     C     TOTRCT        IFGT      *ZEROS
     C     TOTRCT        DIV       TOTTCT        XXRPCT
     C     XXRPCT        MULT      100           X1RPCT
     C                   ELSE
     C                   Z-ADD     *ZEROS        X1RPCT
     C                   END
     C*
     C     TOTACT        IFGT      *ZEROS
     C     TOTACT        DIV       TOTTCT        XXAPCT
     C     XXAPCT        MULT      100           X1APCT
     C                   ELSE
     C                   Z-ADD     *ZEROS        X1APCT
     C                   END
     C*
     C     TOTPCT        IFGT      *ZEROS
     C     TOTPCT        DIV       TOTTCT        XXPPCT
     C     XXPPCT        MULT      100           X1PPCT
     C                   ELSE
     C                   Z-ADD     *ZEROS        X1PPCT
     C                   END
     C*
     C     TOTDCT        IFGT      *ZEROS
     C     TOTDCT        DIV       TOTTCT        XXDPCT
     C     XXDPCT        MULT      100           X1DPCT
     C                   ELSE
     C                   Z-ADD     *ZEROS        X1DPCT
     C                   END
     C*
     C   16              EXCEPT    DET1
     C*
     C* END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain Orkey Por01;
       If %found and (xxtype = 'B' or xxtype = 'D');
          Except Route;
       Endif;
       xxprt = '1';
       Chain Orkey Por01;
       If %found and (xxtype = 'B' or xxtype = 'S');
          Except Route1;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*****************************************************************
     C* SRINTR - SUBROUTINE TO PRINT DETAIL LINE AND GET NEXT INTR
     C*****************************************************************
     C     SRINTR        BEGSR
     C*
     C     XXTCNT        IFGT      0
     C*
     C* GET THE INTERVIEWER INFORMATION FOR THE DETAIL LINE
     C*
     C     UPKY1         CHAIN     PUP02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     UPUSNM        XXUSNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXUSNM
     C                   END
     C*
     C* ACCUMULATE ALL INTERVIEWER TOTALS
     C*
     C                   ADD       XXOCNT        TOTOCT
     C                   ADD       XXCCNT        TOTCCT
     C                   ADD       XXRCNT        TOTRCT
     C                   ADD       XXTCNT        TOTTCT
     C                   ADD       XXACNT        TOTACT
     C                   ADD       XXPCNT        TOTPCT
     C                   ADD       XXDCNT        TOTDCT
     C*
     C* PRINT THE DETAIL LINE
     C*
     C     *INOA         IFEQ      '1'
     C   15              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C   15              EXCEPT    DET
     C*
     C                   Z-ADD     *ZEROS        XXOCNT
     C                   Z-ADD     *ZEROS        XXCCNT
     C                   Z-ADD     *ZEROS        XXRCNT
     C                   Z-ADD     *ZEROS        XXTCNT
     C                   Z-ADD     *ZEROS        XXACNT
     C                   Z-ADD     *ZEROS        XXPCNT
     C                   Z-ADD     *ZEROS        XXDCNT
     C                   MOVE      *BLANKS       XXINTR
     C                   MOVEL     PCINTR        XXINTR
     C*
     C                   END
     C*
     C                   ENDSR
     C*
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
      ***** End Del ***** 08/26/14 ************************************
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
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O          E            ROUTE          2 33
     O                                           18 'REPORT SELECTIONS:'
     O          E            ROUTE          2
     O                                           26 'COMPANY        :'
     O                       XXCO#               31
     O          E            ROUTE          2
     O                                           26 'SUBSCRIBER     :'
     O                       XXSUB#              33
     O          E            ROUTE          2
     O                                           26 'GROUP          :'
     O                       XXFND               31
     O          E            ROUTE          2
     O                                           26 'INTERVIEWER    :'
     O               18      XXINTR              31
     O              N18                          32 '*ALL'
     O          E            ROUTE          2
     O                                           26 'ACTIVITY RANGE :'
     O*** BEGIN CHG *** 03/14/00 **************************************
     O**********               X1FRDT    36 '  /  /  '
     O**********                         39 'TO'
     O**********               X1TODT    48 '  /  /  '
     O                       X2FRDT              38 '  /  /    '
     O                                           41 'TO'
     O                       X2TODT              52 '  /  /    '
     O*** END   CHG *** 03/14/00 **************************************
     O          E            ROUTE          2
     O                                           26 'REPORT TYPE    :'
     O               15N16                       34 'DETAIL'
     O               16N15                       35 'SUMMARY'
     O               15 16                       32 'BOTH'
     O          E            ROUTE          2
     O                                           26 'RUN DATE       :'
     O*** BEGIN CHG *** 03/14/00 **************************************
     O**********               DSDATE    36 '  /  /  '
     O                       DSDATE              38 '  /  /    '
     O*** END   CHG *** 03/14/00 **************************************
     O*** END   CHG *** 07/31/15 **************************************
     O          E            ROUTE          1
     O                                           26 'ROUTE TO       :'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Chg ***** 07/31/15 ************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E  160           ROUTE
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                                57 'INC. 2000      '
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              97
     O                       DSDATE        Y    127
     O          E            HEAD           1
     O                       Q1PGM               10
     O*** BEGIN CHG *** 03/14/00 **************************************
     O**********               XXTITL    96
     O                       XXTITL             110
     O*** END   CHG *** 03/14/00 **************************************
     O                                          121 'PAGE:'
     O                       PAGE          Z    127
     O          E            HEAD           2
     O                                           62 'FOR ACTIVITY:'
     O*** BEGIN CHG *** 03/14/00 **************************************
     O**********               X1FRDT    71 '  /  /  '
     O**********                         75 'TO'
     O**********               X1TODT    84 '  /  /  '
     O                       X2FRDT              73 '  /  /    '
     O                                           77 'TO'
     O                       X2TODT              88 '  /  /    '
     O*** END   CHG *** 03/14/00 **************************************
     O          E            HEAD           1
     O                                           12 'SUBSCRIBER.:'
     O                       XXSUB#              19
     O                                           21 '-'
     O                       XXSBNM              62
     O          E            HEAD           2
     O                                           12 'GROUP......:'
     O                       XXFND               19
     O                                           21 '-'
     O                       XXFNNM              82
     O          E            HEAD           1
     O                                           38 'TOTAL'
     O                                           48 'OPEN'
     O                                           58 'CLOSED'
     O                                           68 'RE-CERT'
     O                                           89 'LIABILITY FLAG'
     O          E            HEAD           2
     O                                           11 'INTERVIEWER'
     O                                           41 'ACTIVITY'
     O                                           46 '#'
     O                                           55 '#'
     O                                           64 '#'
     O                                           75 'APPR'
     O                                           83 'PEND'
     O                                           93 'DENIED'
     O          E            DET            1
     O                       XXUSNM              31
     O                       XXTCNT        Z     38
     O                       XXOCNT        Z     49
     O                       XXCCNT        Z     57
     O                       XXRCNT        Z     66
     O                       XXACNT        Z     76
     O                       XXPCNT        Z     84
     O                       XXDCNT        Z     92
     O          E            TOT         2  2
     O                                           27 'TOTALS:'
     O                       TOTTCT        Z     38
     O                       TOTOCT        Z     49
     O                       TOTCCT        Z     57
     O                       TOTRCT        Z     66
     O                       TOTACT        Z     76
     O                       TOTPCT        Z     84
     O                       TOTDCT        Z     92
     O          E            ERRMSG      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           41 'SELECTED CRITERIA ***'
     OQSYSPRT1  E            ROUTE1         1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE1         1
     O**********     08      ORDIS2              60
     O**********E            ROUTE1         1
     O**********     08      ORDIS3              60
     O**********E            ROUTE1         2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE1         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE1         1
     O               08      ORDIS2              60
     O          E            ROUTE1         1
     O               08      ORDIS3              60
     O          E            ROUTE1         2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE1         2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE1         2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE1         2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O          E            ROUTE1         2 33
     O                                           18 'REPORT SELECTIONS:'
     O          E            ROUTE1         2
     O                                           26 'COMPANY        :'
     O                       XXCO#               31
     O          E            ROUTE1         2
     O                                           26 'SUBSCRIBER     :'
     O                       XXSUB#              33
     O          E            ROUTE1         2
     O                                           26 'GROUP          :'
     O                       XXFND               31
     O          E            ROUTE1         2
     O                                           26 'INTERVIEWER    :'
     O               18      XXINTR              31
     O              N18                          32 '*ALL'
     O          E            ROUTE1         2
     O                                           26 'ACTIVITY RANGE :'
     O*** BEGIN CHG *** 03/14/00 **************************************
     O**********               X1FRDT    36 '  /  /  '
     O**********                         39 'TO'
     O**********               X1TODT    48 '  /  /  '
     O                       X2FRDT              38 '  /  /    '
     O                                           41 'TO'
     O                       X2TODT              52 '  /  /    '
     O*** END   CHG *** 03/14/00 **************************************
     O          E            ROUTE1         2
     O                                           26 'REPORT TYPE    :'
     O               15N16                       34 'DETAIL'
     O               16N15                       35 'SUMMARY'
     O               15 16                       32 'BOTH'
     O          E            ROUTE1         2
     O                                           26 'RUN DATE       :'
     O*** BEGIN CHG *** 03/14/00 **************************************
     O**********               DSDATE    36 '  /  /  '
     O                       DSDATE              38 '  /  /    '
     O*** END   CHG *** 03/14/00 **************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE1
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O****                               57 'INC. 2000      '
     O          E            ROUTE1         1 59
     O                       #BRND1
     O          E            ROUTE1         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD1          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              97
     O                       DSDATE        Y    127
     O          E            HEAD1          1
     O                       Q1PGM               10
     O*** BEGIN CHG *** 03/14/00 **************************************
     O**********               ORTITL    96
     O                       ORTITL             110
     O*** END   CHG *** 03/14/00 **************************************
     O                                          121 'PAGE:'
     O                       PAGE          Z    127
     O          E            HEAD1          1
     O                                           62 'FOR ACTIVITY:'
     O*** BEGIN CHG *** 03/14/00 **************************************
     O**********               X1FRDT    71 '  /  /  '
     O**********                         75 'TO'
     O**********               X1TODT    84 '  /  /  '
     O                       X2FRDT              73 '  /  /    '
     O                                           77 'TO'
     O                       X2TODT              88 '  /  /    '
     O*** END   CHG *** 03/14/00 **************************************
     O          E            HEAD1          2
     O                                           69 'STATISTICS FOR ALL'
     O                                           82 'INTERVIEWERS'
     O          E            HEAD1          1
     O                                           12 'SUBSCRIBER.:'
     O                       XXSUB#              19
     O                                           21 '-'
     O                       XXSBNM              62
     O          E            HEAD1          2
     O                                           12 'GROUP......:'
     O                       XXFND               19
     O                                           21 '-'
     O                       XXFNNM              82
     O          E            HEAD1          1
     O                                           76 '----------- LIABILITY F'
     O                                           92 'LAG ------------'
     O          E            HEAD1          1
     O                                           05 'TOTAL'
     O                                           17 'OPEN'
     O                                           33 'CLOSED'
     O                                           47 'RE-CERT'
     O                                           61 'APPROVED'
     O                                           74 'PENDED'
     O                                           88 'DENIED'
     O          E            HEAD1          2
     O                                           08 'ACTIVITY'
     O                                           13 '#'
     O                                           19 '%'
     O                                           27 '#'
     O                                           33 '%'
     O                                           41 '#'
     O                                           47 '%'
     O                                           55 '#'
     O                                           61 '%'
     O                                           69 '#'
     O                                           75 '%'
     O                                           83 '#'
     O                                           89 '%'
     O          E            DET1           1
     O                       TOTTCT        Z     05
     O                       TOTOCT        Z     14
     O                       X1OPCT        M     22
     O                       TOTCCT        Z     28
     O                       X1CPCT        M     36
     O                       TOTRCT        Z     42
     O                       X1RPCT        M     50
     O                       TOTACT        Z     56
     O                       X1APCT        M     64
     O                       TOTPCT        Z     70
     O                       X1PPCT        M     78
     O                       TOTDCT        Z     84
     O                       X1DPCT        M     92
     O          E            ERRMS1      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           41 'SELECTED CRITERIA ***'
     O*
