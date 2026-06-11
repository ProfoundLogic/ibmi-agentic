     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W8400R3 - CREATE ERM-6 TAPE
     H***************************************************************
     H** PROGRAM DESCRIPTION:                                      **
     H**                                                           **
     H**   This program generates the ERM-6 Tape.                  **
     H**                                                           **
     H**   CALLED FROM:                                            **
     H**         W8400C3                                           **
     H**                                                           **
     H**   CALLS TO:                                               **
     H**         NONE                                              **
     H**                                                           **
     H**   CREATED:  11/13/95   C:30   PROJECT: WC460              **
     H**                                                           **
     H***************************************************************
     H* REVISIONS:                                                  *
     H*                                                             *
     H* 06/04/99  C:35   PROJECT: SUPPORT WA046 #17400              *
     H*                  USE CARRIER POLICY NUMBER IN PLACE OF FND/ *
     H*                  EMP#/FYR, IF AVAILABLE.                    *
     H*                                                             *
     H* 12/18/98 C:61  PROJECT 81WNO                                *
     H*                Recompiled due to changes WXW2P and WXW3P.   *
     H*                                                             *
     H* 12/14/98 C:113 PROJECT: SUPPORT WA077 15413                 *
     H*                Added code to initialize 'I' after line 314. *
     H*                                                             *
     H* 09/17/98  C:35   PROJECT: SUPPORT WA090 13015               *
     H*                  ADDED FIELDS TO THE EXPOSURE RECORD FOR    *
     H*                  MANUAL RATE, PREMIUM AMOUNT AND EXPOSURE   *
     H*                  COVERAGE CODE.                             *
     H*                  ALSO, LOSS COVERAGE CODE WAS BEING         *
     H*                  REPORTED IN PLACE OF LOSS CLASS CODE.      *
     H*                  THIS HAS BEEN CORRECTED.                   *
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
     H**  WDF2L  - CO#/FND                                         **
     H**  WMEML  - CO#/FND/EMP#/DIV                                **
     H**  WXW1L  - CO#/FND/EMP#/FYR                                **
     H**  WXW3L  - CO#/FND/EMP#/FYR/Period/CLASS                   **
     H**  WXW4L  - CO#/FND/EMP#/FYR/Period/CLASS/NCCI/STATUS       **
     H**  QTAPE  - none                                            **
     H**                                                           **
     H***************************************************************
     FWDF2L     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWXW1L     IF   E           K DISK
     FWXW3L     IF   E           K DISK
     FWXW4L     IF   E           K DISK
      **** BEGIN ADD **** 06/04/99 *********************************************
     FWMRPL     IF   E           K DISK
      **** END   ADD **** 06/04/99 *********************************************
     FQTAPE     O    F  150        SEQ

     D*
     D ST              S              2    DIM(54) CTDATA PERRCD(27)            State Codes
     D*
     D*  Tape Record Link Data
     D*
     D                 DS
     D  LINK                   1     42
     D  LNPROV                 1      5  0
     D  LNPOL                  6     23
     D  LNEFDT                24     29  0
     D  LNSTCD                30     31  0
     D  LNDPI                 32     32
     D  LNRCN                 33     37  0
     D  LNFIL                 38     42
     D*
     D*  Tape Header Record
     D*
     D TYPE1           DS
     D  T1LINK                 1     42
     D  T1TYPE                43     43  0
     D  T1ENAM                44    122
     D  T1RTDT               123    128  0
     D  T1TODT               129    134  0
     D  T1FIL                135    150
     D*
     D*  Tape Exposure Record
     D*
     D TYPE2           DS
     D  T2LINK                 1     42
     D  T2TYPE                43     43  0
     D  T2MDDT                44     49  0
     D  T2RTDT                50     55  0
     D  T2CLAS                56     59  0
     D  T2AMT                 60     69  0
     D  T2MOD                 70     73  0
      **** BEGIN CHG **** 09/17/98 *********************************************
     D************************************* 135 150 T2FIL
     D  T2ARAT                74     80  3
     D  T2AMPR                81     88  0
     D  T2EXCC                89     90
     D  T2FIL                 91    150
      **** END   CHG **** 09/17/98 *********************************************
     D*
     D*  Tape Loss Record
     D*
     D TYPE3           DS
     D  T3LINK                 1     42
     D  T3TYPE                43     43  0
     D  T3CLM                 44     55
     D  T3ACDT                56     61  0
     D  T3#CLM                62     64  0
      **** BEGIN CHG **** 09/17/98 *********************************************
     D************************************** 65  680T3LCC
     D  T3CLAS                65     68  0
      **** END   CHG **** 09/17/98 *********************************************
     D  T3NCCI                69     69
     D  T3COMP                70     78  0
     D  T3MED                 79     87  0
     D  T3STAT                88     88
     D  T3CAT                 89     90
      **** BEGIN CHG **** 09/17/98 *********************************************
     D************************************** 91 150 T3FIL
     D  T3LCC                 91     92  0
     D  T3FIL                 93    150
      **** END   CHG **** 09/17/98 *********************************************
     D*
     D*  Tape Qualifier Record
     D*
     D TYPE4           DS
     D  T4LINK                 1     42
     D  T4TYPE                43     43  0
     D  T4PINS                44     58
     D  T4PPOL                59     76
     D  T4PFDT                77     82  0
     D  T4PREM                83     92  0
     D  T4FIL                 93    150
     D*
     D*  Tape Submission Control Record
     D*
     D TYPE9           DS
     D  T9FIL1                 1     42
     D  T9TYPE                43     43  0
     D  T9PROV                44     48
     D  T9#HDR                49     52  0
     D  T9#TOT                53     62  0
     D  T9FIL2                93    150
     D*  System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*  Policy Number
      **** BEGIN DEL **** 06/04/99 *********************************************
     D*********** DS
     D**************************************  1  18 XXPOL
     D**************************************  1   3 XFND
     D**************************************  4  12 XEMP#
     D************************************** 13  15 XFYR
      **** END   DEL **** 06/04/99 *********************************************
      **** BEGIN ADD **** 06/04/99 *********************************************
     D                 DS
     D  XPOL                   1     15
     D  W1FND                  1      3  0
     D  W1EMP#                 4     12  0
     D  W1FYR                 13     15  0
      **** END   ADD **** 06/04/99 *********************************************

     C****************************************************************
     C*                    K E Y     L I S T S
     C****************************************************************
     C*
     C*   Key List for the Fund Aux file
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
     C     F2KY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   Key List for the Employer Master  file
     C*
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*   Key List for Work File # 1
     C*
     C     W1KY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C*
     C*   Key List for Work File # 3
     C*
     C     W3KY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C*
     C*   Key List for Work File # 4
     C*
     C     W4KY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
      **** BEGIN ADD **** 06/04/99 ************************************
      *
      * KEY LIST FOR THE CARRIER/STATE REPORTING FILE
      *
     C     RPKY          KLIST
     C                   KFLD                    W1CO#
     C                   KFLD                    W1FND
     C                   KFLD                    W1EMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    W1FYR
      **** END   ADD **** 06/04/99 ************************************
     C*
     C*================================================================
     C*
     C*================================================================
     C*
     C     *LIKE         DEFINE    W4CO#         XXCO#
     C     *LIKE         DEFINE    W4FND         XXFND
     C     *LIKE         DEFINE    W4FYR         XXFYR
     C     *LIKE         DEFINE    W4EMP#        XXEMP#
     C     *LIKE         DEFINE    W4PER#        XXPER#
     C     *LIKE         DEFINE    W4CLAS        XXCLAS
     C     *LIKE         DEFINE    W4NCCI        XXNCCI
     C     *LIKE         DEFINE    W4STAT        XXSTAT
     C     *LIKE         DEFINE    EMDT          XXDATE
      **** BEGIN ADD **** 06/04/99 *********************************************
     C                   MOVE      *BLANKS       XXPOL            18
      **** END   ADD **** 06/04/99 *********************************************
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   Z-ADD     *ZERO         CNTHDR            4 0
     C                   Z-ADD     *ZERO         CNTTOT           10 0
     C                   Z-ADD     *ZERO         CUREMP            5 0
     C*
     C*********************************************************
     C*                    M A I N     L O G I C
     C*********************************************************
     C*
     C*   Read WXW1P Workfile
     C*
     C                   READ      PW101                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C*  If "New" Employer Number, Write 'Qualifier Record'
     C*
     C     CUREMP        IFNE      W1EMP#
     C     CUREMP        IFGT      *ZERO
     C                   EXSR      SRREC4
     C                   Z-ADD     W1EMP#        CUREMP
     C                   Z-ADD     *ZERO         XXPREM           10 0
     C                   ELSE
     C                   Z-ADD     W1EMP#        CUREMP
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Assign 'LINK DATA' and Write 'Header Record'
     C*
     C                   EXSR      SRREC1
     C*
     C*  Write 'Exposure Records'
     C*
     C                   EXSR      SRREC2
     C*
     C*  Write 'Loss Records'
     C*
     C                   EXSR      SRREC3
     C*
     C*  Read Next WXW1L record
     C*
     C                   READ      PW101                                  60
     C*
     C                   ENDDO                                                  *IN60 DOWEQ 0
     C*
     C*  Write 'Submission Control Record'
     C*
     C                   MOVE      *BLANK        T9FIL1
     C                   Z-ADD     9             T9TYPE
     C                   MOVE      F2CAR#        T9PROV
     C                   Z-ADD     CNTHDR        T9#HDR
     C                   Z-ADD     CNTTOT        T9#TOT
     C                   MOVE      *BLANK        T9FIL2
     C*
     C                   WRITE     QTAPE         TYPE9
     C*
     C*  End Program
     C*
     C                   MOVE      *ON           *INLR
     C*
     C**********************************************************************
     C*  SRREC1 - Subroutine to write Header record
     C**********************************************************************
     C     SRREC1        BEGSR
     C*
     C                   MOVE      W1CO#         XXCO#
     C                   MOVE      W1FND         XXFND
     C                   MOVE      W1EMP#        XXEMP#
     C                   MOVE      W1FYR         XXFYR
     C*
     C*  Get the Employer Master
     C*
     C                   MOVE      *BLANK        T1ENAM
     C     EMKY1         CHAIN     PEM01                              60
      **** BEGIN ADD **** 06/04/99 *********************************************
      *
      * GET THE STATE REPORTING DIVISION.  IF NO STATE REPORTING DIVISION,
      * DEFAULT TO THE FIRST DIVISION.
      *
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             EMDIV
     C                   ELSE
      *
     C     *IN60         DOWEQ     *OFF
     C     EMSTRP        ANDNE     'Y'
     C     EMKY1         READE     PEM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
      *
     C     *IN60         IFEQ      *ON
     C     EMKY1         CHAIN     PEM01                              60
     C                   ENDIF                                                  *IN60 IFEQ *ON
      *
     C                   ENDIF                                                  *IN60 IFEQ *ON
      **** END   ADD **** 06/04/99 *********************************************
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     EMNAME        T1ENAM
     C                   ENDIF
     C*
     C*  Assign State code
     C*
     C     EMPST         IFNE      *BLANK
     C                   Z-ADD     1             I                 3 0
     C     EMPST         LOOKUP    ST(I)                                  60
     C                   ELSE
     C*** BEGIN ADD *** 12/14/98 **************************************
     C                   Z-ADD     1             I                 3 0
     C*** END   ADD *** 12/14/98 **************************************
     C     EMMST         LOOKUP    ST(I)                                  60
     C                   ENDIF
     C*
     C                   Z-ADD     I             LNSTCD
     C*
     C*  Get the Fund Aux file
     C*
     C                   MOVE      *ZERO         LNPROV
     C     F2KY1         CHAIN     PF201                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      F2CAR#        LNPROV
     C                   ENDIF
     C*
     C*  Assign Policy Number
     C*
      **** BEGIN CHG **** 06/04/99 *********************************************
     C******************** MOVE W1FND     XFND
     C******************** MOVE W1EMP#    XEMP#
     C******************** MOVE W1FYR     XFYR
     C******************** MOVE XXPOL     LNPOL
     C                   MOVEL(P)  XPOL          XXPOL
      *
      * CHECK FOR A CARRIER/STATE REPORTING RECORD
      *
     C     RPKY          CHAIN     PRP01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     RPCAP#        IFNE      *BLANKS
      *
     C                   MOVEL(P)  RPCAP#        XXPOL
      *
     C                   ENDIF                                                  RPCAP# IFNE *BLANKS
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C                   MOVEL(P)  XXPOL         LNPOL
      **** END   CHG **** 06/04/99 *********************************************
     C*
     C                   MOVE      W1PFDT        LNEFDT
     C                   MOVE      'S'           LNDPI
     C                   MOVE      W1EMP#        LNRCN
     C                   MOVE      *BLANK        LNFIL
     C*
     C*  Accumulate Total Subject Premium for Current Employer
     C*
     C                   ADD       W1PREM        XXPREM
     C*
     C*  Write Header Record
     C*
     C                   MOVE      LINK          T1LINK
     C                   Z-ADD     1             T1TYPE
     C                   MOVE      W1RTDT        T1RTDT
     C                   MOVE      W1PTDT        T1TODT
     C                   MOVE      *BLANK        T1FIL
     C*
     C*
     C                   WRITE     QTAPE         TYPE1
     C*
     C                   ADD       1             CNTHDR
     C                   ADD       1             CNTTOT
     C*
     C                   ENDSR
     C*
     C**********************************************************************
     C*  SRREC2 - Subroutine to write Exposure (Payroll) records
     C**********************************************************************
     C     SRREC2        BEGSR
     C*
     C*   Read Workfile # 3
     C*
     C     W3KY1         CHAIN     PW301                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVE      LINK          T2LINK
     C                   Z-ADD     2             T2TYPE
     C                   Z-ADD     W3RTDT        T2RTDT
     C                   Z-ADD     W3MDDT        T2MDDT
     C                   Z-ADD     W3CLAS        T2CLAS
     C                   MOVE      *ZERO         T2MOD
     C                   MOVEL     W3EXMD        T2MOD
     C                   Z-ADD     W3EAMT        T2AMT
     C                   MOVE      *BLANK        T2FIL
      **** BEGIN ADD **** 09/17/98 *********************************************
     C                   Z-ADD     W3ARAT        T2ARAT
     C                   Z-ADD     W3AMPR        T2AMPR
     C                   MOVE      W3EXCC        T2EXCC
      **** END   ADD **** 09/17/98 *********************************************
     C*
     C                   WRITE     QTAPE         TYPE2
     C*
     C                   ADD       1             CNTTOT
     C*
     C     W3KY1         READE     PW301                                  60
     C                   ENDDO
     C*
     C                   ENDSR
     C*
     C**********************************************************************
     C*  SRREC3 - Subroutine to write Loss records
     C**********************************************************************
     C     SRREC3        BEGSR
     C*
     C*  Read Workfile # 4
     C*
     C     W4KY1         CHAIN     PW401                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVE      LINK          T3LINK
     C                   Z-ADD     3             T3TYPE
     C                   MOVE      W4CLM         T3CLM
     C                   Z-ADD     W4ACDT        T3ACDT
     C                   MOVE      W4NCCI        T3NCCI
     C                   Z-ADD     W4NCLM        T3#CLM
     C                   Z-ADD     W4LCC         T3LCC
     C                   Z-ADD     W4MED         T3MED
     C                   Z-ADD     W4COMP        T3COMP
     C                   MOVE      W4STAT        T3STAT
      **** BEGIN ADD **** 09/17/98 *********************************************
     C                   Z-ADD     W4CLAS        T3CLAS
      **** END   ADD **** 09/17/98 *********************************************
     C*
     C                   WRITE     QTAPE         TYPE3
     C*
     C                   ADD       1             CNTTOT
     C*
     C*  Get the Next WXW4L record
     C*
     C     W4KY1         READE     PW401                                  60
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*
     C**********************************************************************
     C*  SRREC4 - Subroutine to write Exposure records
     C**********************************************************************
     C     SRREC4        BEGSR
     C*
     C                   MOVE      LINK          T4LINK
     C                   Z-ADD     4             T4TYPE
     C                   MOVE      *BLANK        T4PINS
     C                   MOVE      *BLANK        T4PPOL
     C                   Z-ADD     *ZERO         T4PFDT
     C                   Z-ADD     XXPREM        T4PREM
     C                   MOVE      *BLANK        T4FIL
     C*
     C                   WRITE     QTAPE         TYPE4
     C*
     C                   ADD       1             CNTTOT
     C*
     C                   ENDSR
     C**********************************************************************
**  TABLE ST - STATES IN ENDORSEMENT NUMERICAL ORDER
ALAZARCACOCTDEDCFLGAIDILINIAKSKYLAMEMDMAMIMNMSMOMTNENV
NHNJNMNYNCNDOHOKORPARISCSDTNTXUTVTVAWAWVWIWYPRZZHIZZAK
