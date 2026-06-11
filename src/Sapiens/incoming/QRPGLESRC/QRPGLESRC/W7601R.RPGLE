     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7601R - AUDIT CHANGE LIST REPORT
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  10/27/97  C:86  VERSION 3 PROJECT: WC577                     *
     H*            REVISED TO PRINT THE ALTERNATE FUND NAME IN PLACE  *
     H*            OF COMPANY NAME, IF ONE EXISTS, AND CENTER NAME.   *
     H*****************************************************************
     FWMEPL1    IF   E           K DISK
     FWMCML5    IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDELL1    IF   E           K DISK
     FWMEML     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FQSYSPRT   O    F  190        PRINTER OFLIND(*INOF)

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
     C*  FUND MASTER
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
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
     C*  EMPLOYER PAYROLL FILE
     C*
     C     EPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*   KEY LIST FOR THE CLAIMS FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C     ELKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*  TABLE FILE KEY - BILLING SCHEDULE CODE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
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
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C     XXEMP#        COMP      0                                      25
     C     QQTYPE        COMP      'E'                                    23
     C*
     C                   EXSR      SR1ST
     C*
     C*  READ ALL AUDITED EMPLOYERS FROM THE EMP DETAIL FILE
     C*
     C   25ELKEY         CHAIN     PEL01                              60
     C  N25ELKEY1        CHAIN     PEL01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     ELFAF         IFEQ      'A'
     C*
     C*  GET BILLING SCHEDULE CODE
     C*
     C                   MOVEL     ELBSCD        XXKEY
     C     TBKEY         CHAIN     PBS01                              60
     C                   MOVE      '1'           *IN43
     C     QQTYPE        IFEQ      'S'
     C     BSSEFL        ANDEQ     'S'
     C                   MOVE      '0'           *IN43
     C                   END
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXPFDT            6 0
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXPTDT            6 0
     C                   Z-ADD     ELANDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXANDT            6 0
     C*
     C*  GET THE EMPLOYER NAME
     C*
     C                   MOVE      *BLANKS       XXNAME           19
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     EMNAME        XXNAME
     C                   END
     C*
     C*  CHAIN OUT TO THE EMPLOYER PAYROLL FILE & PRINT ALL AUDITED
     C*  ACTIVE PAYROLLS.  INITIALIZE LINE COUNT TO ONE THE FIRST
     C*  TIME THROUGH TO ONLY PRINT EMPLOYER INFORMATION ONCE.
     C*
     C                   Z-ADD     0             XXLCNT            3 0
     C     EPKEY         CHAIN     PEP01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     EPSTAT        IFNE      'I'
     C     EPSAFL        ANDEQ     'A'
     C*
     C*   GET TOTAL INCURRED FOR PAYROLL CLASS
     C*
     C                   Z-ADD     0             XXTOTI           11 2
     C     CMKEY         CHAIN     PCM01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C     CMCLAS        IFEQ      EPCLAS
     C                   Z-ADD     *ZERO         TOTPMT            9 2
     C                   Z-ADD     *ZERO         TOTRES            9 2
     C                   XFOOT     CMA           TOTPMT
     C                   XFOOT     CMR           TOTRES
     C                   ADD       TOTPMT        XXTOTI
     C                   ADD       TOTRES        XXTOTI
     C                   END
     C*
     C     CMKEY         READE     PCM01                                  50
     C                   END
     C*
     C                   Z-ADD     EPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEFDT            6 0
     C   43EPEAPR        SUB       EPAAPR        XXDIFF           11 2
     C  N43EPSAPR        SUB       EPAAPR        XXDIFF           11 2
     C*
     C*  UPDATE FUND TOTALS FOR CLASS
     C*
     C   43              ADD       EPEAPR        L1PREM
     C  N43              ADD       EPSAPR        L1PREM
     C                   ADD       EPAAPR        L1AAPR
     C                   ADD       XXTOTI        L1TOTI
     C*
     C*  OUTPUT DETAIL LINE ITEM
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   END
     C                   ADD       1             XXLCNT
     C                   EXCEPT    DETAIL
     C                   END
     C*
     C     EPKEY         READE     PEP01                                  60
     C                   END
     C                   END
     C   25ELKEY         READE     PEL01                                  60
     C  N25              MOVE      '1'           *IN60
     C     *IN60         IFEQ      '1'
     C     XXLCNT        ANDEQ     0
     C                   EXCEPT    DETL2
     C                   END
     C                   END
     C*
     C*  PRINT FUND TOTALS
     C*
     C     L1PREM        SUB       L1AAPR        L1DIFF           13 2
     C                   MOVE      '1'           *IN12
     C   OF              EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C     XXLCNT        IFNE      0
     C                   EXCEPT    TOTL1
     C                   ENDIF
      /Free
       // ***** Beg Add ***** 08/26/14 ***********************************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       // ***** End Add ***** 08/26/14 ***********************************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*
     C*==================================================================*
     C*   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
     C*==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
     C     QQMODE        IFEQ      'D'                                          DEMAND MODE
     C                   SETON                                        09
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C     UPKEY         CHAIN     WMUPL                              60
     C     COKEY         CHAIN     SMCOL                              60
     C                   TIME                    DSDTTM
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*
     C*   GET THE DISTRIBUTION INFORMATION & PRINT
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C*********          MOVE      '0'           XXPRT
     C*********     ORKEY         CHAIN     POR01                              60
     C*********     *IN60         IFEQ      '0'
     C*********          EXCEPT    ROUTE
     C*********          END
      ***** End Del ***** 08/26/14 ************************************
     C*
     C*  GET THE FUND MASTER
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C*** BEGIN ADD *** 10/27/97 *******************************
     C     *IN60         IFEQ      '0'
     C                   MOVEL(P)  FNNAME        XFNAME           60
     C     FNANAM        IFNE      *BLANK
     C                   MOVEL(P)  FNANAM        ##DATA
     C                   ELSE
     C                   MOVEL(P)  CONAME        ##DATA
     C                   ENDIF
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XCNAME           60
     C                   ENDIF
     C*
     C*** END ADD ***** 10/27/97 ********************************
     C*
     C                   EXCEPT    HEAD
     C*
     C*  INITIALIZE FIELDS
     C*
     C                   Z-ADD     0             L1PREM           13 2
     C                   Z-ADD     0             L1AAPR           13 2
     C                   Z-ADD     0             L1TOTI           13 2
     C*
     C                   ENDSR
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O*********                                   8 'XXXXXXXX'
     O*********                                  20 'XXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXX     XX'
     O*********                                  77 'XXXXXX'
     O********* E            ROUTE          1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXXX    XX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE          1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XXXX    XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   8 'XXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   4 'XXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE          1
     O*********                                   5 'XXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE          1
     O*********                                   6 'XX XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                   7 'XX  XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX    XXXX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                   8 'XX   XXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  33 'XXXXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX    XXXX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE          3
     O*********                                   9 'XX    XXX'
     O*********                                  20 'XXXXXX'
     O*********                                  32 'XXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX     XXX'
     O*********                                  77 'XXXXXX'
     O********* E            ROUTE          1
     O*********                                  15 'ROUTE TO:'
     O*********      08      ORDIS1              60
     O*********         09   UPUSNM              67
     O********* E            ROUTE          1
     O*********      08      ORDIS2              60
     O********* E            ROUTE          1
     O*********      08      ORDIS3              60
     O********* E            ROUTE          2
     O*********      08      ORDIS4              60
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
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              77
      ***** Beg Del ***** 08/26/14 ************************************
     O********* E            ROUTE          2
     O*********                                  19 'PROGRAM NAME:'
     O*********              ORPNAM              49
     O*********                                  58 'FILE:'
     O*********              ORFIL#              60
     O*********                                  68 'COPIES:'
     O*********              ORCOPY        Z     72
     O*********                                  78 'HOLD:'
     O*********              ORHOLD              83
     O********* E            ROUTE          2
     O*********                                  19 'OUTPUT QUEUE:'
     O*********         08   OROUTQ              49
     O*********         09   QQQUE               49
     O*********                                  59 'FORMS:'
     O*********              ORFORM              70
     O*********                                  78 'SAVE:'
     O*********              ORSAVE              83
      ***** End Del ***** 08/26/14 ************************************
     O          E            ROUTE          2
     O                                         +  6 'POLICY YEAR:'
     O                       XXFYR         Z   + 21
     O          E            ROUTE          2
     O                                         +  6 'EMPLOYER:'
     O                       XXEMP#        Z   + 24
     O          E            ROUTE          2
     O                                         +  6 'DIVISION:'
     O                       XXDIV         Z   + 19
     O          E            ROUTE          2
     O                                         +  6 'ESTIMATED/SELF-REPORTING'
     O                                         +  1 '(E/S):'
     O                       QQTYPE            +  2
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE
     O****                               23 '(C) COPYRIGHT PROGRESSI'
     O****                               46 'VE DATA SOLUTIONS. INC,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XCNAME             125
     O                       DSDATE        Y    190
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL             125
     O                                          184 'PAGE:'
     O                       PAGE          Z    190
     O          E            HEAD           2
     O                                              'GROUP:'
     O                       XXFND         Z   +  1
     O                       XFNAME            +  2
     O                                           83 'POLICY YEAR:'
     O                       XXFYR         Z   +  1
     O                                          122 'PAYROLL SELECTION:'
     O                  23                     +  1 'ESTIMATED'
     O                 N23                      137 'SELF-REPORTING'
     O          E      N12   HEAD           1
     O                                              ' EMPLOYER   DIV NAME'
     O                                           61 'POLICY PERIOD'
     O                                           77 'ANNIV RT DT'
     O                                           83 'TYP'
     O                                           92 'CLASS'
     O                                          108 'EFFECTIVE'
     O                                          130 'EST / SR '
     O                                          149 'AUDITED'
     O          E      N12   HEAD           1
     O                                          108 'DATE'
     O                                          129 'PAYROLL'
     O                                          149 'PAYROLL'
     O                                          169 'DIFFERENCE'
     O                                          189 'TOTAL INCURRED'
     O*
     O* Detail Line
     O*
     O          E            DETL2          1
     O                                           94 '***   NO RECORDS MEET'
     O                                         +  1 'SELECTION CRITERIA   ***'
     O          E            DETAIL         1
     O                       ELEMP#        Z     09
     O                       ELDIV         Z     15
     O                       XXNAME              35
     O                       XXPFDT              53 '  /  /  '
     O                                           55 '-'
     O                       XXPTDT              64 '  /  /  '
     O                       XXANDT              75 '  /  /  '
     O                       BSSEFL              82
     O                       EPCLAS        Z     92
     O                       EPSPEC              93
     O                       XXEFDT             108 '  /  /  '
     O                  43   EPEAPR        J    130
     O                 N43   EPSAPR        J    130
     O                       EPAAPR        J    150
     O                       XXDIFF        J    170
     O                       XXTOTI        J    190
     O          E            TOTL1       1
     O                                          110 'GROUP TOTALS:'
     O                       L1PREM        J    130
     O                       L1AAPR        J    150
     O                       L1DIFF        J    170
     O                       L1TOTI        J    190
