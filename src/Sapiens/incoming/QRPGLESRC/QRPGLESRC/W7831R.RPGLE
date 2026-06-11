     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7831R - ACTIVITY REPORT
     H*****************************************************************
     H* REVISIONS:
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
     H*  9/26/06 C:58  PROJECT:  71WHI
     H*                Get premium from Employer Detail History(WDEHP)
     H*
     H* 11/01/00 C:33  PROJECT: WC1008                                *
     H*                IF A TRANSFERED ACCOUNT SHOW CX'D DO NOT       *
     H*                INCLUDE PREMIUM ON REPORT.                     *
     H*                                                               *
     H* 10/12/00 C:61  PROJECT WC577                                  *
     H*                Add page break on type.                        *
     H*                                                               *
     H* 04/10/00  C:118 PROJECT: WC987                                *
     H*                 LOAD ALTERNATE COMPANY NAME FROM FUND MASTER  *
     H*                                                               *
     H* 10/22/99  C:33  SEQ#: WA048/20138                             *
     H*                 CHANGE XXDAT FROM 6.0 TO 7.0                  *
     H*                                                               *
     H*  07/28/98 C:33 SUPPORT: WA091/12838                           *
     H*                CHANGE ORDER OF SUB-HEADINGS                   *
     H*                                                               *
     H*  04/10/98 C:33 PROJECT: WC395                                 *
     H*                ADD BOUND & BOUND W/MONEY AFTER QUOTED.        *
     H*                ADD TRANSFERS AFTER WRITTEN.                   *
     H*                                                               *
     H*  12/02/97 C:80 PROJECT: WC395                                 *
     H*                ADDED REINSTATEMENT DETAIL AFTER CANCELLATION. *
     H*                                                               *
     H*  10/13/97 C:85 PROJECT: WC577                                 *
     H*                VERSION 3 REPORT STANDARDS                     *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWAQTL     IF   E           K DISK
     FWAQTL2    IF   E           K DISK
     F                                     RENAME(PQT01:PQT02)
     FWAQTL4    IF   E           K DISK
     F                                     RENAME(PQT01:PQT03)
     FWAQTL5    IF   E           K DISK
     F                                     RENAME(PQT01:PQT04)
     F**** BEGIN ADD ***  9/26/06 *************************************
     FWDELL2    IF   E           K DISK
     FWDEHL1    IF   E           K DISK
     F**** END ADD *****  9/26/06 *************************************
     FWMCPL     IF   E           K DISK
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)

     D**** BEGIN CHG *** 12/02/97 *************************************
     D******************* TP      6   6  1
     D***  BEGIN CHG  ***  04/10/98  *********************************
     D**********          TP      7   7  1
     D TP              S              1    DIM(11) CTDATA PERRCD(11)
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
     C     *LIKE         DEFINE    CPCHAP        XXCHAP
     C     *LIKE         DEFINE    ORFIL#        XXPRT
     C     *LIKE         DEFINE    QTTYPE        XXTYPE
     C     *LIKE         DEFINE    QTDT          XXTODT
     C     *LIKE         DEFINE    QTDT          XXFRDT
     C***  BEGIN ADD  ***  10/22/99  *********************************
     C     *LIKE         DEFINE    QTEFDT        XXDAT
     C***  END   ADD  ***  10/22/99  *********************************
     C**** BEGIN ADD ***  9/26/06 *************************************
     C     *LIKE         DEFINE    ELPFDT        KKPFDT
     C**** END ADD *****  9/26/06 *************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQRNGE            1
     C                   PARM                    QQCHAP            1
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
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   Z-ADD     XXTO          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         X1TO              6 0
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
     C                   KFLD                    XXTYPE
     C*
     C     QTKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     QTKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXCHAP
     C                   KFLD                    XXTYPE
     C*
     C     QTKEY3        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXCHAP
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
     C**** BEGIN ADD ***  9/26/06 *************************************
     C*   KEY LIST FOR EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    QTCO#
     C                   KFLD                    QTFND
     C                   KFLD                    QTEMP#
     C                   KFLD                    QTDIV
     C                   KFLD                    KKPFDT
     C*
     C*   KEY LIST FOR EMPLOYER DETAIL HISTORY FILE
     C*
     C     EHKEY         KLIST
     C                   KFLD                    QTCO#
     C                   KFLD                    QTFND
     C                   KFLD                    QTEMP#
     C                   KFLD                    QTDIV
     C                   KFLD                    ELFYR
     C**** END ADD *****  9/26/06 *************************************
     C*
     C*   KEY LIST FOR CHAPTER
     C*
     C     CPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXCHAP
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
     C     UPKEY         CHAIN     PUP01                              60
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
     C*   DETERMINE IF SORT BY CHAPTER
     C*
     C***  BEGIN CHG  ***  04/10/98  *********************************
     C********** QQRNGE    COMP 'W'                      79
     C     QQRNGE        COMP      'T'                                    79
     C***  END   CHG  ***  04/10/98  *********************************
     C     QQCHAP        COMP      'Y'                                    78
     C*
     C*   INITIALIZE FIELDS
     C*
     C                   Z-ADD     0             XXCNT             6 0
     C                   Z-ADD     1             X                 2 0
     C                   MOVE      TP(X)         XXTYPE
     C                   MOVE      '0'           *IN57
     C                   Z-ADD     0             XXCHAP
     C                   MOVE      '1'           *INOA
     C*
     C*   START READING QUOTE FILE
     C*
     C   79
     CANN78QTKEY         CHAIN     PQT01                              60
     C  N79
     CANN78QTKEY         CHAIN     PQT03                              60
     C*
     C*   READ QUOTE FILE BY CHAPTER, IF HIT ON TYPE GET CHAPTER DESC
     C*
     C     *IN78         IFEQ      '1'
     C*
     C   79QTKEY1        CHAIN     PQT02                              60
     C  N79QTKEY1        CHAIN     PQT04                              60
     C  N60              Z-ADD     QTCHAP        XXCHAP
     C   79
     CANN60QTKEY2        CHAIN     PQT02                              60
     C  N79
     CANN60QTKEY2        CHAIN     PQT04                              60
     C  N60CPKEY         CHAIN     PCP01                              50
     C                   MOVE      *BLANKS       XXCPNM
     C  N60*IN50         IFEQ      '0'
     C                   MOVEL     CPDESC        XXCPNM           30
     C                   END
     C*
     C                   END
     C*
     C     *IN57         DOWEQ     '0'
     C*
     C* 1- IF NO HIT ON TYPE, CONTINUE CHAINING WITH OTHER TYPES UNTIL
     C*    EITHER NO RECORDS FOUND, OR HIT ON TYPE.
     C*
     C* 2- CHECK FOR CHAPTER BREAK, BEGIN WITH FIRST TYPE & CHAIN
     C*
     C     *IN60         DOWEQ     '1'
     C     *IN57         ANDEQ     '0'
     C*
     C*   PRINT TOTALS FOR PREVIOUS TYPE IF COUNT > 0
     C*
     C     XXCNT         IFGT      0
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C   78              EXCEPT    SRTHD
     C                   EXCEPT    SUBHD
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    TOTAL
      ***** BEG ADD ******* 10/12/00 **********************************
     C                   SETON                                        OA
      ***** END ADD ******* 10/12/00 **********************************
     C                   Z-ADD     0             XXCNT
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
     C*  CHAPTER BREAK
     C*
     C     QQCHAP        IFEQ      'Y'
     C*
     C                   Z-ADD     0             X
     C   79QTKEY3        SETGT     PQT02
     C   79QTKEY1        READE     PQT02                                  60
     C  N79QTKEY3        SETGT     PQT04
     C  N79QTKEY1        READE     PQT04                                  60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN57
     C                   ELSE
     C                   Z-ADD     QTCHAP        XXCHAP
     C                   END
     C                   MOVE      *BLANKS       XXCPNM
     C     CPKEY         CHAIN     PCP01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     CPDESC        XXCPNM
     C                   END
     C                   MOVE      '1'           *INOA
     C*
     C                   ELSE
     C*
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
     C   79
     CANN78QTKEY         CHAIN     PQT01                              60
     C   79
     CAN 78QTKEY2        CHAIN     PQT02                              60
     C  N79
     CANN78QTKEY         CHAIN     PQT03                              60
     C  N79
     CAN 78QTKEY2        CHAIN     PQT04                              60
     C                   END
     C*
     C                   END
     C*
     C*  INITIALIZE COUNT
     C*
     C                   Z-ADD     0             XXCNT             6 0
     C                   Z-ADD     0             XXPREM            9 0
     C     X             ADD       19            X1                2 0
     C**** BEGIN CHG *** 12/02/97 *************************************
     C******************** MOVEA'000000'  *IN,20
     C                   MOVEA     '0000000'     *IN(20)
     C**** END   CHG *** 12/02/97 *************************************
     C***  BEGIN ADD  ***  04/10/98  *********************************
     C                   MOVEA     '0000'        *IN(27)
     C***  END   ADD  ***  04/10/98  *********************************
     C                   MOVEA     '1'           *IN(X1)
     C                   MOVE      '1'           *IN35
     C*
     C*  PRINT & CONTINUE READING EQUAL TO TYPE (& CHAPTER IF SELECTED)
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C*  CHECK DATE RANGE FOR ALL TYPES, EXCEPT P & Q.
     C*
     C                   MOVE      'Y'           XXPRNT            1
     C*
     C     QTTYPE        IFNE      'P'
     C     QTTYPE        ANDNE     'Q'
     C***  BEGIN CHG  ***  10/22/99  *********************************
     C******79**           Z-ADDQTWKDT    XXDAT   60
     C   79              Z-ADD     QTWKDT        XXDAT
     C***  END   CHG  ***  10/22/99  *********************************
     C  N79              Z-ADD     QTEFDT        XXDAT
     C     XXDAT         IFLT      XXFRDT
     C     XXDAT         ORGT      XXTODT
     C                   MOVE      'N'           XXPRNT
     C                   END
     C                   END
     C*
     C     XXPRNT        IFEQ      'Y'
     C*
     C*  PRINT SUB HEADING THE FIRST TIME THRU
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C   78              EXCEPT    SRTHD
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
     C                   Z-ADD     QTWKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXWKDT            6 0
     C*
     C                   Z-ADD     QTEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEFDT            6 0
     C*
     C                   ADD       1             XXCNT
     C*
     C**** BEGIN ADD ***  9/26/06 *************************************
     C                   Z-ADD     QTEFDT        KKPFDT
     C     ELKEY         CHAIN     PEL01                              99
     C     *IN99         IFEQ      *OFF
     C     EHKEY         SETGT     PEH01                              99
     C     EHKEY         READPE    PEH01                                  99
     C     *IN99         DOWEQ     '0'
     C     EHFAF         ANDEQ     'A'
     C     EHKEY         READPE    PEH01                                  99
     C                   ENDDO
     C     *IN99         IFEQ      '0'
     C     EHFAF         ANDNE     'A'
     C     EHWNMP        ADD       EHPRTD        QTPREM
     C                   ENDIF
     C                   ENDIF
     C**** END ADD *****  9/26/06 *************************************
     C*
     C***  BEGIN ADD  ***  11/01/00  *********************************
     C     QTXFER        IFEQ      'C'
     C                   CLEAR                   QTPREM
     C                   ENDIF                                                  QTXFER=C
     C***  END   ADD  ***  11/01/00  *********************************
     C*
     C                   ADD       QTPREM        XXPREM
     C*
     C*   GET AGENT NAME
     C*
     C                   MOVE      *BLANKS       XXAGNM           25
     C     AAKEY         CHAIN     PAA01                              60
     C     *IN60         IFEQ      '0'
     C     AGKEY         CHAIN     PAG01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     AGNAME        XXAGNM
     C                   END
     C                   END
     C*
     C                   EXCEPT    DETAIL
     C*
     C*  SELECTION CRITERIA FOUND
     C*
     C                   SETON                                        61
     C*
     C*
     C                   END
     C*
     C   79
     CANN78QTKEY         READE     PQT01                                  60
     C   79
     CAN 78QTKEY2        READE     PQT02                                  60
     C  N79
     CANN78QTKEY         READE     PQT03                                  60
     C  N79
     CAN 78QTKEY2        READE     PQT04                                  60
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
       Chain orkey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
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
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O*  MLS 1/14/88 ADD DISTRIBUTION INFROMATION WITH INDICATORS
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
     O*  MLS 1/14/88 ADD DISTRIBUTION INFROMATION WITH INDICATORS
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
     O                                           30 'GROUP  . . . . . . . . .'
     O                                           42 ' . . . . . :'
     O                       FNFND         Z     46
     O                       FNNAME            +  2
     O          E            ROUTE          1
     O                                           30 'DATE RANGE . . . . . . .'
     O                                           42 ' . . . . . :'
     O                       X1FROM        Y     51
     O                                           53 '-'
     O                       X1TO          Y     62
     O          E            ROUTE          1
     O***  BEGIN CHG  ***  04/10/98  *********************************
     O**********                         30 'WEEK ENDING OR EFFECTIVE'
     O                                           30 'TYPE CHANGE OR EFFECTIVE'
     O***  END   CHG  ***  04/10/98  *********************************
     O                                           42 ' DATE RANGE:'
     O                       QQRNGE              44
     O          E            ROUTE          1
     O                                           30 'SORT BY CHAPTER  . . . .'
     O                                           42 ' . . . . . :'
     O                       QQCHAP              44
     O*
     O*
     O***  BEGIN CHG  ***  07/31/15  *********************************
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O*  MLS 1/14/88 ADD DISTRIBUTION INFROMATION WITH INDICATORS
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
     O***  BEGIN CHG  ***  07/31/15  *********************************
     O*
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O****                               23 '(c) Copyright Progressi'
     O****                                  've Data Solutions, Inc.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              91
     O                       DSDATE        Y    142
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL             101
     O                                          136 'PAGE:'
     O                       PAGE          Z    142
     O          E            HEAD           2
     O                                           06 'GROUP:'
     O                       XXFND         Z     10
     O                       FNNAME            +  2
     O                                           83 'DATE RANGE:'
     O                       X1FROM        Y     92
     O*****                    XXFRDTY   92
     O                                           94 '-'
     O                       X1TO          Y    103
     O*****                    XXTODTY  103
     O                                          121 'DATE SELECTION:'
     O                       QQRNGE             123
     O          E            HEAD           1
     O***  BEGIN CHG  ***  04/10/98  *********************************
     O**********                         05 'WEEK'
     O                                           05 'TYPE'
     O***  END   CHG  ***  04/10/98  *********************************
     O                                           73 'EFFECTIVE'
     O          E            HEAD           2
     O***  BEGIN CHG  ***  04/10/98  *********************************
     O**********                         07 'ENDING'
     O                                           07 'CHANGE'
     O***  END   CHG  ***  04/10/98  *********************************
     O                                           26 'ACCOUNT NAME'
     O                                           53 'AGENT NAME'
     O                                           85 'DATE        CARRIER'
     O                                          110 'PREMIUM  COMMENTS'
     O          E            SRTHD       1  2
     O                                           09 'CHAPTER:'
     O                       XXCHAP        Z     13
     O                       XXCPNM              44
     O          E            SUBHD       1  2
     O***  BEGIN CHG  ***  07/28/98  *********************************
     O**********          20             10 'WRITTEN  '
     O**********          21             10 'PENDING  '
     O**********          22             10 'QUOTED   '
     O**********          23             10 'CANCELLED'
     O**********          24             10 'LOST     '
     O**********          25             10 'DECLINED '
     O*****BEGIN ADD *** 12/02/97 *************************************
     O**********          26             11 'REINSTATED'
     O*****END** ADD *** 12/02/97 *************************************
     O*****BEGIN ADD  ***  04/10/98  *********************************
     O**********          27             10 'BOUND    '
     O**********          28             13 'BOUND W/MONEY'
     O**********          29             14 'TRANSFER (OUT)'
     O**********          30             13 'TRANSFER (IN)'
     O*****END** ADD  ***  04/10/98  *********************************
     O                  20                       10 'WRITTEN  '
     O                  21                       10 'PENDING  '
     O                  22                       10 'QUOTED   '
     O                  23                       10 'BOUND    '
     O                  24                       13 'BOUND W/MONEY'
     O                  25                       10 'CANCELLED'
     O                  26                       11 'REINSTATED'
     O                  27                       10 'LOST     '
     O                  28                       10 'DECLINED '
     O                  29                       14 'TRANSFER (OUT)'
     O                  30                       13 'TRANSFER (IN)'
     O***  END   CHG  ***  07/28/98  *********************************
     O          E            DETAIL         1
     O                       XXWKDT        Y      8
     O                       QTNAME              35
     O                       XXAGNM              62
     O                       XXEFDT        Y     72
     O                       QTCARR              89
     O                       QTPREM        1    100
     O                       QTCOMM             142
     O          E            TOTAL       1  1
     O                       XXCNT         1     30
     O                       XXPREM        1    100
     O*
     O*  SELECTION CRITERIA NOT FOUND
     O*
     O          E            NOREC
     O                                           19 '*** NO RECORDS MEET'
     O                                           42 'SELECTION CRITERIA ***'
     O*
**  TYPE TABLE
WPQBMCRLDXT
