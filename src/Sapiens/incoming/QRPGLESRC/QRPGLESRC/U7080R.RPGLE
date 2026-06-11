     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_Qry  dftactgrp(*no) actgrp('QILE')
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
     H*                DELETED XXFRDT & XXTODT BEING PASSED TO        *
     H*                X1FRDT & X1TODT, WHICH WAS CAUSING HEADINGS TO *
     H*                PRINT WITH ZERO VALUES.                        *
     H*                ADDED X2FRDT & X2TODT PASSED TO XXFRDT & XXTODT*
     H*                FOR USE IN CALCULATIONS.                       *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FUMPCL     IF   E           K DISK
     F                                     RENAME(PPC01:PPC02)
     FUMPCL1    IF   E           K DISK
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
     D*** END   CHG *** 03/14/00 **************************************
     D*
     D*
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
     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    PCCO#
     C                   KFLD                    PCSUB#
     C                   KFLD                    PCFND
     C                   KFLD                    PCFYR
     C                   KFLD                    PCCASE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQINTR            3
     C                   PARM                    QQLIAB            1
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
     C* INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    PCCO#         XXCO#                          3.0N
     C     *LIKE         DEFINE    PCSUB#        XXSUB#                         5.0N
     C     *LIKE         DEFINE    PCFND         XXFND                          3.0N
     C     *LIKE         DEFINE    SMNAME        XXSBNM                         40A
     C     *LIKE         DEFINE    UPUSNM        XXUSNM                         40A
     C     *LIKE         DEFINE    PCCERT        XXCERT                         15A
     C     *LIKE         DEFINE    PCINTR        XXINTR                         3A
     C     *LIKE         DEFINE    PCLIAB        XXLIAB                         1A
     C     *LIKE         DEFINE    ORTITL        XXTITL                         40A
     C     *LIKE         DEFINE    UPDT          XXFRDT                         7.0N
     C     *LIKE         DEFINE    UPDT          XXTODT                         7.0N
     C*
     C                   Z-ADD     *ZEROS        XXFRDT
     C                   Z-ADD     *ZEROS        XXTODT
     C                   Z-ADD     *ZEROS        X1FRDT            6 0          6.0N
     C                   Z-ADD     *ZEROS        X1TODT            6 0          6.0N
     C                   Z-ADD     *ZEROS        XXACNT            5 0          5.0N
     C                   Z-ADD     *ZEROS        XXPCNT            5 0          5.0N
     C                   Z-ADD     *ZEROS        XXDCNT            5 0          5.0N
     C                   Z-ADD     *ZEROS        XXTCNT            5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTACT            5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTPCT            5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTDCT            5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTTCT            5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTAT             5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTPT             5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTDT             5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTAC             5 0          5.0N
     C                   Z-ADD     *ZEROS        XXAPCT            5 4          5.4N
     C                   Z-ADD     *ZEROS        XXPPCT            5 4          5.4N
     C                   Z-ADD     *ZEROS        XXDPCT            5 4          5.4N
     C                   Z-ADD     *ZEROS        X1APCT            5 2          5.2N
     C                   Z-ADD     *ZEROS        X1PPCT            5 2          5.2N
     C                   Z-ADD     *ZEROS        X1DPCT            5 2          5.2N
     C                   MOVE      *BLANKS       XXTYPE            1            1A
     C                   MOVE      *BLANKS       XXPRT             1            1A
     C                   MOVE      *BLANKS       XXFNNM           60            60A
     C                   MOVE      *BLANKS       XXNAME           40            40A
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQINTR        XXINTR
     C                   MOVE      QQLIAB        XXLIAB
     C                   MOVE      QQFRDT        X2FRDT            8 0
     C*
     C                   Z-ADD     X2FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         X1FRDT
     C*
     C                   MOVE      QQTODT        X2TODT            8 0
     C*
     C                   Z-ADD     X2TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         X1TODT
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
     C*
     C* GET THE INTERVIEWER INFORMATION FOR THE HEADING
     C*
     C     UPKY1         CHAIN     PUP02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     UPUSNM        XXUSNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXUSNM
     C                   END
     C                   END
     C*
     C     XXLIAB        IFNE      *BLANKS
     C                   MOVE      '1'           *IN19
     C                   END
     C*
     C*** BEGIN DEL *** 12/03/97 **************************************
     C**********           Z-ADDXXFRDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     X1FRDT
     C*
     C**********           Z-ADDXXTODT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     X1TODT
     C*** END   DEL *** 12/03/97 **************************************
     C*
     C*** BEGIN ADD *** 12/03/97 **************************************
     C                   Z-ADD     X2FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   Z-ADD     X2TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*** END   ADD *** 12/03/97 **************************************
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
      ***** Beg Del ***** 08/26/14 *************************************************
     C*********15              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *************************************************
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
      ***** Beg Del ***** 08/26/14 *************************************************
     C**********16              EXCEPT    ROUTE1
      ***** End Del ***** 08/26/14 *************************************************
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
     C   15              EXCEPT    HEAD3
     C   16              EXCEPT    HEAD1
     C   18
     CAN 15              EXCEPT    HEAD2
     C*
     C* PROCESS THE CORRECT LOGICAL
     C*
     C                   READ      PPC01                                  50
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
     C* PRINT ONLY PRE-CERT RECORDS THAT FALL WITHIN DATE RANGE SELECTED
     C*
     C     PCADDT        IFGE      XXFRDT
     C     PCADDT        ANDLE     XXTODT
     C*
     C* IF THIS IS THE FIRST TIME THROUGH AND REPORT IS FOR ALL INTR
     C* GET THE FIRST ONE NOW
     C*
     C     XXINTR        IFEQ      *BLANKS
     C                   MOVEL     PCINTR        XXINTR
     C     UPKY1         CHAIN     PUP02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     UPUSNM        XXUSNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXUSNM
     C                   END
     C   15              EXCEPT    HEAD2
     C                   END
     C*
     C* IF THE INTR CHANGES PRINT TOTALS AND GET NEXT INTR NAME FOR
     C* HEADING
     C*
     C     PCINTR        IFNE      XXINTR
     C                   EXSR      SRINTR
     C   15              EXCEPT    HEAD2
     C                   END                                                    END PCINTR
     C*
     C                   MOVE      PCCERT        XXCERT
     C*
     C* GET THE CLAIMANT NAME FOR PRINTING
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C     CMSS#         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDLSN         XXLAST           56
     C     CDFSN         IFNE      *BLANK
     C                   MOVEL     CDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END                                                    END CDFSN
     C                   MOVEL     XXLAST        XXNAME
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END *IN60
     C*
     C* CONVERT PRE-CERT DATE FIELDS TO CORRECT FORMAT MM/DD/YY
     C*
     C                   Z-ADD     PCADDT        SRDTI
     C*** BEGIN CHG *** 03/14/00 **************************************
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     XXADDT  60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXADDT            8 0
     C*** END   CHG *** 03/14/00 **************************************
     C*
     C                   Z-ADD     PCRVDT        SRDTI
     C*** BEGIN CHG *** 03/14/00 **************************************
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     XXRVDT  60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXRVDT            8 0
     C*** END   CHG *** 03/14/00 **************************************
     C*
     C                   Z-ADD     PCIDT         SRDTI
     C*** BEGIN CHG *** 03/14/00 **************************************
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     XXIDT   60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXIDT             8 0
     C*** END   CHG *** 03/14/00 **************************************
     C*
     C* TOTAL UP ALL APPR/DENIED/PEND RECERTS FOR CERT
     C*
     C                   Z-ADD     *ZEROS        XXACNT
     C                   Z-ADD     *ZEROS        XXPCNT
     C                   Z-ADD     *ZEROS        XXDCNT
     C                   Z-ADD     *ZEROS        XXTCNT
     C     PCKEY         CHAIN     PPC02                              60
     C     *IN60         DOWEQ     '0'
     C     PCLIAB        IFEQ      'A'
     C                   ADD       1             XXACNT
     C                   END                                                    END = 'A'
     C     PCLIAB        IFEQ      'P'
     C                   ADD       1             XXPCNT
     C                   END                                                    END = 'P'
     C     PCLIAB        IFEQ      'D'
     C                   ADD       1             XXDCNT
     C                   END                                                    END = 'D'
     C                   ADD       1             XXTCNT
     C     PCKEY         READE     PPC02                                  60
     C                   END                                                    END *IN60
     C*
     C* ACCUMULATE THE INTERVIEWER TOTALS
     C*
     C                   ADD       XXACNT        TOTACT
     C                   ADD       XXPCNT        TOTPCT
     C                   ADD       XXDCNT        TOTDCT
     C                   ADD       XXTCNT        TOTTCT
     C*
     C* PRINT THE DETAIL LINE
     C*
     C     *INOA         IFEQ      '1'
     C   15              EXCEPT    HEAD
     C   15              EXCEPT    HEAD3
     C   15              EXCEPT    HEAD2
     C                   MOVE      '0'           *INOA
     C                   END
     C   15              EXCEPT    DET
     C*
     C                   END                                                    END XXFRDT/TODT
     C*
     C* GET THE NEXT PRE-CERT RECORD
     C*
     C                   READ      PPC01                                  50
     C*
     C                   END                                                    END DOW 50
     C*
     C* PRINT TOTAL FOR THE LAST RECORD
     C*
     C     *INOA         IFEQ      '1'
     C   15              EXCEPT    HEAD
     C   15              EXCEPT    HEAD3
     C   15              EXCEPT    HEAD2
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXSR      SRINTR
     C*
     C* PRINT THE REPORT TOTALS
     C*
     C     *INOA         IFEQ      '1'
     C   15              EXCEPT    HEAD
     C   15              EXCEPT    HEAD3
     C   15              EXCEPT    HEAD2
     C                   MOVE      '0'           *INOA
     C                   END
     C   15              EXCEPT    TOTRP
     C*
     C* CALCULATE THE LIABILITY PERCENTAGES
     C*
     C     TOTAT         IFGT      *ZEROS
     C     TOTAT         DIV       TOTAC         XXAPCT
     C     XXAPCT        MULT      100           X1APCT
     C                   ELSE
     C                   Z-ADD     *ZEROS        X1APCT
     C                   END
     C*
     C     TOTPT         IFGT      *ZEROS
     C     TOTPT         DIV       TOTAC         XXPPCT
     C     XXPPCT        MULT      100           X1PPCT
     C                   ELSE
     C                   Z-ADD     *ZEROS        X1PPCT
     C                   END
     C*
     C     TOTDT         IFGT      *ZEROS
     C     TOTDT         DIV       TOTAC         XXDPCT
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
       If %found and (xxtype= 'B' or xxtype = 'D');
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
     C* SRINTR - ACCUMULATE TOTALS, PRINT TOTALS, GET NEXT INTR
     C*****************************************************************
     C     SRINTR        BEGSR
     C*
     C* PRINT THE INTERVIEWER TOTALS
     C*
     C   15              EXCEPT    TOT
     C*
     C     *IN16         IFEQ      '1'
     C                   ADD       TOTACT        TOTAT
     C                   ADD       TOTPCT        TOTPT
     C                   ADD       TOTDCT        TOTDT
     C     TOTAT         ADD       TOTPT         TOTAC
     C     TOTAC         ADD       TOTDT         TOTAC
     C                   END
     C                   Z-ADD     *ZEROS        TOTACT
     C                   Z-ADD     *ZEROS        TOTPCT
     C                   Z-ADD     *ZEROS        TOTDCT
     C                   Z-ADD     *ZEROS        TOTTCT
     C*
     C* GET THE NEXT INTERVIEWER NAME
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
     C                   ENDSR
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *************************************************
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
      ***** End Del ***** 08/26/14 *************************************************
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
      ***** Beg Del ***** 08/26/14 *************************************************
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
      ***** End Del ***** 08/26/14 *************************************************
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
     O                                           26 'LIABILITY      :'
     O               19      XXLIAB              29
     O              N19                          32 '*ALL'
     O          E            ROUTE          2
     O                                           26 'ADMIT DT RANGE :'
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
     O                                           61 'FOR DATE RANGE:'
     O*** BEGIN CHG *** 03/14/00 **************************************
     O**********               X1FRDT    70 '  /  /  '
     O**********                         74 'TO'
     O**********               X1TODT    83 '  /  /  '
     O                       X2FRDT              72 '  /  /    '
     O                                           76 'TO'
     O                       X2TODT              87 '  /  /    '
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
     O          E            HEAD2          2
     O                                           12 'INTERVIEWER:'
     O                       XXINTR              19
     O                                           21 '-'
     O                       XXUSNM              64
     O          E            HEAD3          1
     O                                           68 'ENTRY'
     O                                           79 'ADMIT'
     O                                           90 'REVIEW'
     O                                           99 'TOTAL'
     O                                          126 '---- LIABILITY ----'
     O          E            HEAD3          2
     O                                           06 'CERT #'
     O                                           31 'CLAIMANT NAME'
     O                                           67 'DATE'
     O                                           78 'DATE'
     O                                           89 'DATE'
     O                                          101 'ACTIVITY'
     O                                          110 'APPR'
     O*** BEGIN CHG *** 03/14/00 **************************************
     O**********                        116 'PEND'
     O                                          118 'PEND'
     O*** END   CHG *** 03/14/00 **************************************
     O                                          126 'DENIED'
     O          E            DET            1
     O                       XXCERT              15
     O                       XXNAME              58
     O*** BEGIN CHG *** 03/14/00 **************************************
     O**********               XXIDT     69 '  /  /  '
     O**********               XXADDT    80 '  /  /  '
     O**********               XXRVDT    91 '  /  /  '
     O                       XXIDT               69 '  /  /    '
     O                       XXADDT              80 '  /  /    '
     O                       XXRVDT              91 '  /  /    '
     O*** END   CHG *** 03/14/00 **************************************
     O                       XXTCNT        Z     99
     O                       XXACNT        Z    109
     O                       XXPCNT        Z    117
     O                       XXDCNT        Z    125
     O          E            TOT         2  2
     O                                           90 'INTERVIEWER TOTALS:'
     O                       TOTTCT        Z     99
     O                       TOTACT        Z    109
     O                       TOTPCT        Z    117
     O                       TOTDCT        Z    125
     O          E            TOTRP          2
     O                                           90 'REPORT TOTALS:'
     O                       TOTAC         Z     99
     O                       TOTAT         Z    109
     O                       TOTPT         Z    117
     O                       TOTDT         Z    125
     O          E            ERRMSG      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           41 'SELECTED CRITERIA ***'
     OQSYSPRT1  E            ROUTE1         1 06
      ***** Beg Del ***** 08/26/14 *************************************************
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
      ***** End Del ***** 08/26/14 *************************************************
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
      ***** Beg Del ***** 08/26/14 *************************************************
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
      ***** End Del ***** 08/26/14 *************************************************
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
     O                                           26 'LIABILITY      :'
     O               19      XXLIAB              29
     O              N19                          32 '*ALL'
     O          E            ROUTE1         2
     O                                           26 'ADMIT DT RANGE :'
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
     O                                           61 'FOR DATE RANGE:'
     O*** BEGIN CHG *** 03/14/00 **************************************
     O**********               X1FRDT    70 '  /  /  '
     O**********                         74 'TO'
     O**********               X1TODT    83 '  /  /  '
     O                       X2FRDT              72 '  /  /    '
     O                                           76 'TO'
     O                       X2TODT              87 '  /  /    '
     O*** END   CHG *** 03/14/00 **************************************
     O          E            HEAD1          2
     O                                           66 'STATISTICS FOR ALL'
     O                                           79 'INTERVIEWERS'
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
     O                                           05 'TOTAL'
     O                                           36 '------------ LIABILITY F'
     O                                           53 'LAG  ------------'
     O          E            HEAD1          1
     O                                           08 'ACTIVITY'
     O                                           20 'APPROVED'
     O                                           33 'PENDED'
     O                                           48 'DENIED'
     O          E            HEAD1          2
     O                                           04 '#'
     O                                           13 '#'
     O                                           19 '%'
     O                                           28 '#'
     O                                           34 '%'
     O                                           43 '#'
     O                                           49 '%'
     O          E            DET1           1
     O                       TOTAC         Z     05
     O                       TOTAT         Z     14
     O                       X1APCT        M     23
     O                       TOTPT         Z     29
     O                       X1PPCT        M     38
     O                       TOTDT         Z     44
     O                       X1DPCT        M     53
     O          E            ERRMS1      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           41 'SELECTED CRITERIA ***'
     O*
