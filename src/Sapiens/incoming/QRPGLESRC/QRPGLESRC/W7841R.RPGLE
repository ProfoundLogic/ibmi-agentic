     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7841R - AGENT ACTIVITY REPORT
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 11/01/00 C:33  PROJECT: WC1008                                *
     H*                IF A TRANSFERED ACCOUNT SHOW CX'D DO NOT       *
     H*                INCLUDE PREMIUM ON REPORT.                     *
     H*                                                               *
     H*  04/10/00 C:118 PROJECT: WC987                                *
     H*                 LOAD ALTERNATE COMPANY NAME FROM FUND MASTER  *
     H*                                                               *
     H*  12/01/99 C:33 SEQ#: WA107/20865                              *
     H*                CORRECT SPELLING OF SUBMISSIONS                *
     H*                                                               *
     H*  01/22/99 C:33 SEQ#: WA090/15459                              *
     H*                MAKE SUB-HEADINGS MORE ACCURATE                *
     H*                                                               *
     H*  10/21/98 C:33 SUPPORT: PER JJ                                *
     H*                GET FIRST SUB-HEADING TITLE                    *
     H*                                                               *
     H*  08/11/98 C:33 SUPPORT: WA065/13101                           *
     H*                CHANGE ORDER OF SUB-HEADINGS                   *
     H*                                                               *
     H*  04/10/98 C:33 PROJECT: WC395                                 *
     H*                ADD BOUND & BOUND W/MONEY AFTER QUOTED.        *
     H*                ADD TRANSFERS AFTER WRITTEN.                   *
     H*                                                               *
     H*  12/02/97 C:80 PROJECT: WC395                                 *
     H*                ADD REINSTATEMENT DETAIL AFTER CANCELLATION.   *
     H*                INCLUDE REINSTATEMENT TYPES IN CALCULATION OF  *
     H*                HIT RATIO AND BALANCES.                        *
     H*                                                               *
     H*  10/23/97 C:85 PROJECT: WC577                                 *
     H*                VERSION 3 REPORT STANDARDS.                    *
     H*                                                               *
     H*  04/03/97 C:62 PROJECT WC567                                  *
     H*           USES AGMKRP INSTEAD OF AGSTF1 TO RETRIEVE REP NAME. *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FWMFNL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWAQTL1    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D**** BEGIN CHG *** 12/02/97 *************************************
     D******************* TP      6   6  1
     D***  BEGIN CHG  ***  04/10/98  *********************************
     D**********          TP      7   7  1
     D***  BEGIN CHG  ***  08/11/98  *********************************
     D**********          TP     11  11  1
     D***  BEGIN CHG  ***  01/22/99  *********************************
     D**********          TP      1  11  1   TPD    25   TYPE CHG, DESCRIPTIONS
     D TP              S              1    DIM(11) CTDATA PERRCD(1)             TYPE CHG, DESCRIPTIO
     D TPD             S             35    DIM(11) ALT(TP)
     D***  END   CHG  ***  01/22/99  *********************************
     D***  END   CHG  ***  08/11/98  *********************************
     D***  END   CHG  ***  04/10/98  *********************************
     D**** END   CHG *** 12/02/97 *************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    COCO#         XXCO#
     C     *LIKE         DEFINE    FNFND         XXFND
     C     *LIKE         DEFINE    ORFIL#        XXPRT
     C     *LIKE         DEFINE    QTTYPE        XXTYPE
     C     *LIKE         DEFINE    QTTYPE        SVTYPE
     C     *LIKE         DEFINE    FNDT          XXFRDT
     C     *LIKE         DEFINE    FNDT          XXTODT
     C***  BEGIN ADD  ***  08/11/98  *********************************
     C     *LIKE         DEFINE    TPD           SUBHDG
     C***  END   ADD  ***  08/11/98  *********************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQAGT#            5
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQQUE            10
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
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFRDT        XXFROM            8 0
     C                   MOVE      QQTODT        XXTO              8 0
     C*
     C                   Z-ADD     XXFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   Z-ADD     XXFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         X1FROM            6 0
     C*
     C                   Z-ADD     XXTO          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         X1TO              6 0
     C*
     C                   Z-ADD     XXTO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   MOVE      QQAGT#        XXAGT#            5 0
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     QQQUE         IFNE      ' '
     C                   SETON                                        09
     C                   ELSE
     C                   SETON                                        08
     C                   END
     C*
     C*   KEY LIST FOR THE QUOTE FILE
     C*
     C     QTKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     QTKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#
     C                   KFLD                    XXTYPE
     C*
     C     QTKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#
     C*
     C*   KEY LIST FOR THE AGENT ASSIGNMENT MASTER FILE
     C*
     C     AAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    QTAGT#
     C*
     C*   KEY LIST FOR THE AGENT MASTER FILE
     C*
     C     AGKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE REPORT OVERRIDE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE GROUP
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C                   MOVE      *BLANKS       X1USNM           50
     C     UPKEY         CHAIN     PUP01                              60
     C  N60              MOVEL     UPUSNM        X1USNM
     C                   TIME                    DSDTTM
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C***** BEGIN DEL **** 04/10/00 ***********************************
     C***  BEGIN ADD  ***  04/10/98  *********************************
     C**********
     C*** CENTER COMPANY NAME
     C**********
     C**********           MOVELCONAME    ##DATA    P
     C**********
     C**********           CALL 'SRCENTER'             99
     C**********           PARM           ##DATA
     C**********           PARM 40        ##LENG
     C**********
     C**********           MOVEL##DATA    CONAME    P
     C***  END   ADD  ***  04/10/98  *********************************
     C***** END   DEL **** 04/10/00 ***********************************
     C*
     C*   CHAIN TO THE GROUP FILE
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     *IN60         IFEQ      *OFF
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C                   ENDIF                                                  *IN60=OFF
     C*
     C*   CENTER COMPANY NAME
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA
     C                   PARM      40            ##LENG
     C*
     C                   MOVEL(P)  ##DATA        CONAME
     C***** END   ADD **** 04/10/00 ***********************************
     C*
     C*   DETERMINE IF AGENT WAS SELECTED
     C*
     C                   MOVE      *BLANKS       SELAGT            5            SELECTION CRIT.
     C     XXAGT#        COMP      0                                      78
     C     *IN78         IFEQ      *ON                                          ALL AGENTS
     C                   MOVE      'ALL  '       SELAGT                         SELECTION CRIT.
     C                   END
     C*
     C*
     C*   GET THE DISTRIBUTION INFORMATION & PRINT
     C*
     C                   MOVE      '0'           XXPRT
     C*
     C     ORKEY         CHAIN     POR01                              60
     C*
     C     *IN60         IFEQ      '0'
     C***  BEGIN ADD  ***  04/10/98  *********************************
     C*
     C*  Left Justify Title in Field
     C*
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C*
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF                                                  N<=60
     C*
     C                   ENDIF                                                  ORTITL<>*BLANKS
     C***  END   ADD  ***  04/10/98  *********************************
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C*
     C***  BEGIN ADD  ***  04/10/98  *********************************
     C                   MOVEL(P)  ORTITL        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        ORTITL
     C***  END   ADD  ***  04/10/98  *********************************
     C*
     C                   END
     C*
     C*   DETERMINE IF AGENT WAS SELECTED
     C*
     C********** XXAGT#    COMP 0                        78
     C*
     C*   INITIALIZE FIELDS
     C*
     C                   Z-ADD     1             X                 2 0
     C                   MOVE      TP(X)         XXTYPE
     C***  BEGIN ADD  ***  10/21/98  *********************************
     C                   MOVEL     TPD(X)        SUBHDG
     C***  END   ADD  ***  10/21/98  *********************************
     C                   MOVE      '0'           *IN57
     C                   MOVE      '1'           *INOA
     C                   MOVE      'Y'           XXFRST            1
     C                   Z-ADD     0             XXCNT             6 0
     C                   Z-ADD     0             XXPREM            9 0
     C                   Z-ADD     0             XXNWRT            6 0
     C*
     C*   START READING QUOTE FILE
     C*
     C  N78QTKEY1        CHAIN     PQT01                              60
     C*
     C     *IN78         IFEQ      '1'                                          ALL AGENTS
     C     QTKEY         CHAIN     PQT01                              60
     C                   Z-ADD     QTAGT#        XXAGT#
     C  N60QTKEY1        CHAIN     PQT01                              60
     C                   END
     C*
     C     *IN57         DOWEQ     '0'
     C*
     C* 1- IF NO HIT ON TYPE, CONTINUE CHAINING WITH OTHER TYPES UNTIL
     C*    EITHER NO RECORDS FOUND, OR HIT ON TYPE.
     C*
     C* 2- IF ALL AGENTS SELECTED, GET NEXT AGENT
     C*
     C     *IN60         DOWEQ     '1'
     C     *IN57         ANDEQ     '0'
     C*
     C*   PRINT TOTALS FOR PREVIOUS TYPE IF COUNT > 0
     C*
     C     XXCNT         IFGT      0
     C     XXTYPE        ANDNE     TP(1)
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    SUBHD
     C                   MOVE      '0'           *INOA
     C                   END
     C     XXTYPE        IFNE      'C'
     C     XXTYPE        ANDNE     'W'
     C**** BEGIN ADD *** 12/02/97 *************************************
     C     XXTYPE        ANDNE     'R'
     C**** END   ADD *** 12/02/97 *************************************
     C***  BEGIN ADD  ***  04/10/98  *********************************
     C     XXTYPE        ANDNE     'T'
     C     XXTYPE        ANDNE     'X'
     C***  END   ADD  ***  04/10/98  *********************************
     C                   ADD       XXCNT         XXNWRT
     C                   END
     C                   EXCEPT    TOTAL
     C                   Z-ADD     0             XXCNT
     C                   Z-ADD     0             XXPREM
     C*
     C                   END
     C*
     C*  CHECK FOR END OF DATA
     C*
     C**** BEGIN CHG *** 12/02/97 *************************************
     C********** X         IFEQ 6
     C***  BEGIN CHG  ***  04/10/98  *********************************
     C********** X         IFEQ 7
     C     X             IFEQ      11
     C***  END   CHG  ***  04/10/98  *********************************
     C**** END   CHG *** 12/02/97 *************************************
     C*
     C*  PRINT HIT RATIO
     C*
     C     XXWRIT        ADD       XXNWRT        XXTOTW            6 0
     C     XXTOTW        IFNE      0
     C     XXWRIT        DIV(H)    XXTOTW        XXHIT             6 2
     C                   EXCEPT    HIT
     C                   ELSE
     C                   Z-ADD     0             XXHIT
     C                   END
     C*
     C*  GET NEXT AGENT
     C*
     C     *IN78         IFEQ      '1'
     C*
     C                   Z-ADD     0             XXNWRT            6 0
     C                   Z-ADD     0             XXWRIT            6 0
     C                   Z-ADD     0             X
     C     QTKEY2        SETGT     PQT01
     C     QTKEY         READE     PQT01                                  60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN57
     C                   ELSE
     C                   Z-ADD     QTAGT#        XXAGT#
     C                   END
     C                   MOVE      'Y'           XXFRST
     C                   MOVE      '1'           *INOA
     C*
     C                   ELSE
     C*  END
     C                   MOVE      '1'           *IN57
     C*
     C                   END
     C*
     C                   END
     C*
     C*  GET NEXT TYPE, IF NOT HIT END PGM
     C*
     C     *IN57         IFEQ      '0'
     C                   ADD       1             X
     C                   MOVE      TP(X)         XXTYPE
     C     QTKEY1        CHAIN     PQT01                              60
     C***  BEGIN ADD  ***  08/11/98  *********************************
     C                   MOVEL     TPD(X)        SUBHDG
     C***  END   ADD  ***  08/11/98  *********************************
     C                   END
     C*
     C                   END
     C***  BEGIN CHG  ***  08/11/98  *********************************
     C**********
     C***SET*SUB HEADING INDICATORS
     C**********
     C*****BEGIN CHG *** 12/02/97 *************************************
     C******************** MOVEA'00000'   *IN,20
     C**********           MOVEA'000000'  *IN,20
     C*****END** CHG *** 12/02/97 *************************************
     C*****BEGIN ADD  ***  04/10/98  *********************************
     C**********           MOVEA'0000'    *IN,26
     C*****END** ADD  ***  04/10/98  *********************************
     C********** XXTYPE    IFEQ 'D'
     C********** XXTYPE    OREQ 'L'
     C**********           MOVE '1'       *IN20
     C**********           ELSE
     C********** X         ADD  18        X1      20
     C**********           MOVEA'1'       *IN,X1
     C**********           MOVE '1'       *IN35
     C**********           END
     C*
     C     XXTYPE        IFNE      'D'
     C     XXTYPE        ORNE      'L'
     C                   MOVE      *ON           *IN35
     C                   ENDIF                                                  XXTYPE<>D/L
     C****  END   CHG  ***  08/11/98  ********************************
     C*
     C*  PRINT & CONTINUE READING EQUAL TO AGENT & TYPE
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C*  CHECK DATE RANGE
     C*
     C     QTEFDT        IFGE      XXFRDT
     C     QTEFDT        ANDLE     XXTODT
     C*
     C*  GET AGENT NAME FIRST TIME THRU & CALC WRITTEN AMT TO BET BAL.
     C*  ALSO GET REPRESENTATIVE NAME FROM THE DIARY INFO - ALL STAFF
     C*  CODES WILL BE THE SAME JUST USE THE FIRST ONE (PER CRIMS RQST)
     C*
     C     XXFRST        IFEQ      'Y'
     C                   MOVE      *BLANKS       XXAGNM           30
     C                   MOVE      *BLANKS       XXRPNM           40
     C                   Z-ADD     0             AACMMT
     C                   Z-ADD     0             XXBAL             9 0
     C                   Z-ADD     0             XXCMDT
     C     AAKEY         CHAIN     PAA01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     AACMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXCMDT            6 0
     C*
     C     AGKEY         CHAIN     PAG01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     AGNAME        XXAGNM
     C*** BEGIN DEL *** 04/03/97 **************************************
     C********** AGSTF1    IFNE *BLANKS
     C********** AGSTF1    CHAINPUP02                60
     C*** END   DEL *** 04/03/97 **************************************
     C*** BEGIN ADD *** 04/03/97 **************************************
     C     AGMKRP        IFNE      *BLANKS
     C     AGMKRP        CHAIN     PUP02                              60
     C*** END   ADD *** 04/03/97 **************************************
     C     *IN60         IFEQ      '0'
     C                   MOVEL     UPUSNM        XXRPNM
     C                   END
     C                   END
     C                   END
     C                   END
     C                   EXSR      WRIT
     C     AACMMT        SUB       XXWRPR        XXBAL
     C                   MOVE      'N'           XXFRST
     C                   END
     C*
     C*  PRINT SUB HEADING THE FIRST TIME THRU
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    SUBHD
     C                   MOVE      '0'           *INOA
     C                   MOVE      '0'           *IN35
     C                   END
     C*
     C     *IN35         IFEQ      '1'
     C                   EXCEPT    SUBHD
     C                   MOVE      '0'           *IN35
     C                   END
     C*
     C*
     C                   Z-ADD     QTEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEFDT            6 0
     C*
     C                   ADD       1             XXCNT
     C*
     C***  BEGIN ADD  ***  11/01/00  *********************************
     C     QTXFER        IFEQ      'C'
     C                   CLEAR                   QTPREM
     C                   ENDIF                                                  QTXFER=C
     C***  END   ADD  ***  11/01/00  *********************************
     C*
     C                   ADD       QTPREM        XXPREM
     C*
     C                   EXCEPT    DETAIL
     C*
     C*  SELECTION CRITERIA FOUND
     C*
     C                   SETON                                        61
     C*
     C                   END
     C*
     C     QTKEY1        READE     PQT01                                  60
     C                   END
     C*
     C                   END
     C*
     C*  SELECTION CRITERIA NOT FOUND
     C*
     C     *IN61         IFEQ      '0'
     C                   EXCEPT    HEAD
     C                   EXCEPT    NOREC
     C                   ENDIF
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain ORKEY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C******************************************************
     C*    -SUBROUTINE TO CALCULATE WRITTEN AMT            *
     C******************************************************
     C     WRIT          BEGSR
     C*
     C                   Z-ADD     0             XXWRIT            6 0
     C                   Z-ADD     0             XXWRPR            9 0
     C                   MOVE      XXTYPE        SVTYPE
     C                   MOVE      'W'           XXTYPE
     C     QTKEY1        CHAIN     PQT01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C     QTEFDT        IFGE      XXFRDT
     C     QTEFDT        ANDLE     XXTODT
     C*
     C***  BEGIN ADD  ***  11/01/00  *********************************
     C     QTTYPE        IFEQ      'T'
     C     QTXFER        ANDEQ     'C'
     C                   CLEAR                   QTPREM
     C                   ENDIF                                                  QTTYPE=T/QTXFER=C
     C***  END   ADD  ***  11/01/00  *********************************
     C*
     C                   ADD       QTPREM        XXWRPR
     C                   ADD       1             XXWRIT
     C                   END
     C*
     C     QTKEY1        READE     PQT01                                  50
     C                   END
     C*
     C                   MOVE      SVTYPE        XXTYPE
     C     QTKEY1        CHAIN     PQT01                              60
     C     *IN60         DOWEQ     '0'
     C     QTEFDT        ANDLT     XXFRDT
     C     QTKEY1        READE     PQT01                                  60
     C                   END
     C*
     C                   ENDSR
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
     O**********        09   X1USNM              67
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
     O                  09   X1USNM              67
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
     O**********E            ROUTE          3
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O*
     O*  SELECTION CRITERIA ADDED TO ROUTING PAGE
     O*
     O          E            ROUTE          2
     O                                           25 'SELECTION CRITERIA:'
     O          E            ROUTE          1
     O                                           19 'GROUP . . . :'
     O                       FNFND         Z     23
     O                       FNNAME            +  2
     O          E            ROUTE          1
     O                                           19 'AGENT NUMBER:'
     O               78      SELAGT              25
     O              N78      XXAGT#        Z     25
     O          E            ROUTE          1
     O                                           19 'DATE RANGE  :'
     O                       X1FROM        Y     28
     O                                           30 '-'
     O                       X1TO          Y     39
     O*
     O*
     O*
      ****** BEG CHG ***** 01/28/10 **********************************
     O*****   E   60           ROUTE
     O****                               23 '(c) Copyright Progressi'
     O****                                  've Data Solutions, Inc.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              86
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                           06 'GROUP:'
     O                       XXFND         Z     12
     O                       FNNAME            +  1
     O                                          112 'DATE RANGE:'
     O                       X1FROM        Y    121
     O                                          123 '-'
     O                       X1TO          Y    132
     O          E            HEAD           1
     O                                           06 'AGENT:'
     O                       XXAGT#        Z     12
     O                       XXAGNM              43
     O                                           58 'COMMITMENT:'
     O                       AACMMT        1     70
     O                                           89 'REPRESENTATIVE:'
     O                       XXRPNM             130
     O          E            HEAD           1
     O                                           52 'DATE:'
     O                       XXCMDT        Y     70
     O          E            HEAD           2
     O                                           55 'BALANCE:'
     O                       XXBAL         J     71
     O          E            HEAD           2
     O                                           12 'ACCOUNT NAME'
     O                                           44 'EFF DATE  CARRIER'
     O                                           75 'PREMIUM   COMMENTS'
     O          E            SUBHD       1  2
     O***  BEGIN CHG  ***  08/11/98  *********************************
     O**********
     O**********          20             23 'SUBMISSIONS NOT WRITTEN'
     O**********          21             23 'SUBMISSIONS WRITTEN    '
     O**********          22             23 'ACCOUNTS PENDING       '
     O**********          23             23 'ACCOUNTS QUOTED        '
     O**********          24             23 'ACCOUNTS CANCELLED     '
     O*****BEGIN ADD *** 12/02/97 *************************************
     O**********          25             23 'ACCOUNTS REINSTATED    '
     O*****END** ADD *** 12/02/97 *************************************
     O*****BEGIN ADD  ***  04/10/98  *********************************
     O**********          26             23 'ACCOUNTS BOUND         '
     O**********          27             23 'ACCOUNTS BOUND W/MONEY '
     O**********          28             23 'ACCOUNTS TRANSFERED (OU'
     O**********          28           +000 'T)'
     O**********          29             24 'ACCOUNTS TRANSFERED (IN)'
     O*****END** ADD  ***  04/10/98  *********************************
     O***  BEGIN CHG  ***  01/22/99  *********************************
     O**********               SUBHDG    25
     O                       SUBHDG
     O***  END   CHG  ***  01/22/99  *********************************
     O***  END   CHG  ***  08/11/98  *********************************
     O          E            DETAIL         1
     O                       QTNAME              25
     O                       XXEFDT        Y     35
     O                       QTCARR              52
     O                       QTPREM        1     64
     O                       QTCOMM             107
     O          E            TOTAL       1  1
     O                       XXCNT         1     28
     O                       XXPREM        1     64
     O          E            HIT         1  1
     O                                           12 'HIT RATIO: ('
     O                       XXWRIT        1     19
     O                                           20 '/'
     O                       XXTOTW        1     27
     O                                           30 ') ='
     O                       XXHIT         1     38
     O*
     O*  SELECTION CRITERIA NOT FOUND
     O*
     O          E            NOREC
     O                                           19 '*** NO RECORDS MEET'
     O                                           42 'SELECTION CRITERIA ***'
     O*
**  TYPE TABLE
LSUBMISSIONS NOT WRITTEN (LOST)
DSUBMISSIONS NOT WRITTEN (DECLINED)
WSUBMISSIONS WRITTEN
XACCOUNTS TRANSFERED (OUT)
TACCOUNTS TRANSFERED (IN)
PACCOUNTS PENDING
QACCOUNTS QUOTED
BACCOUNTS BOUND
MACCOUNTS BOUND W/MONEY
CACCOUNTS CANCELLED
RACCOUNTS REINSTATED
