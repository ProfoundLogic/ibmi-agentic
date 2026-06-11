     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CMPSTS: COMPARE CMSTAT TO LAST CHOCST IN WMCHP
     H****************************************************************
     H*  REVISIONS:                                                  *
     H*                                                              *
     H*  03/25/99  C:33  SEQ#: WC081/17027                           *
     H*                  ADD FIX CAPABILITY                          *
     H****************************************************************
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMCHL     IF   E           K DISK
     F***  BEGIN CHG  ***  03/25/99  *********************************
     FWMRHL     IF   E           K DISK
     FWMCHP     O    E             DISK
     F                                     RENAME(PCH01:PCH0P)
     F***  END   ADD  ***  03/25/99  *********************************
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D*
     D***  BEGIN ADD  ***  03/25/99  *********************************
     D CLS             S             60    DIM(1) CTDATA PERRCD(1)
     D OPN             S             60    DIM(1) CTDATA PERRCD(1)
     D*
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I***  END   ADD  ***  03/25/99  *********************************

     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*  CLAIM HISTORY MASTER FILE
     C*
     C     CHKEY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C***  BEGIN ADD  ***  03/25/99  *********************************
     C*
     C*  RESERVE HISTORY FILE
     C*
     C     RHKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C     *LIKE         DEFINE    @@FIX         XXFIX
     C     *LIKE         DEFINE    RHEXPL        U1EXPL
     C     *LIKE         DEFINE    RHEXPL        U2EXPL
     C     *LIKE         DEFINE    *IN60         FOUND
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@FIX             1            Y/N
     C*
     C     Q1PRMS        IFGE      1
     C                   MOVE      @@FIX         XXFIX
     C                   ELSE
     C                   MOVE      'N'           XXFIX
     C                   ENDIF                                                  Q1PRMS>=1
     C***  END   ADD  ***  03/25/99  *********************************
     C*
     C*  START READING CLAIM MASTER FILE
     C*
     C                   Z-ADD     0             XXCNT             9 0
     C                   Z-ADD     0             XXLINE            2 0
     C*
     C                   EXCEPT    HEAD
     C*
     C                   READ      PCM01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     XXLINE        IFGT      55
     C                   EXCEPT    HEAD
     C                   Z-ADD     0             XXLINE
     C                   END
     C*
     C*  GET CLAIM HISTORY RECORD
     C*
     C     CHKEY         SETGT     WMCHL
     C***  BEGIN CHG  ***  03/25/99  *********************************
     C**********           READPWMCHL                    60
     C********** *IN60     IFEQ '0'
     C********** CMCO#     ANDEQCHCO#
     C********** CMFND     ANDEQCHFND
     C********** CMFYR     ANDEQCHFYR
     C********** CMCASE    ANDEQCHCASE
     C********** CHOCST    IFNE CMSTAT
     C     CHKEY         READPE    WMCHL                                  60
     C*
     C     *IN60         IFEQ      *ON
     C     CHOCST        ORNE      CMSTAT
     C*
     C                   MOVE      *IN60         *IN25
     C*
     C                   EXSR      RSVHST
     C***  END   CHG  ***  03/25/99  *********************************
     C*
     C                   ADD       1             XXCNT
     C                   ADD       1             XXLINE
     C                   MOVE      *BLANKS       CDFSN
     C                   MOVE      *BLANKS       CDLSN
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C*
     C***  BEGIN DEL  ***  03/25/99  *********************************
     C**********           MOVE '0'       *IN25
     C***  END   DEL  ***  03/25/99  *********************************
     C*
     C                   EXCEPT    DET
     C*
     C***  BEGIN ADD  ***  03/25/99  *********************************
     C     FOUND         IFEQ      *ON
     C     XXFIX         ANDEQ     'Y'
     C                   EXSR      WRITER
     C                   ENDIF                                                  FOUND=*ON
     C***  END   ADD  ***  03/25/99  *********************************
     C*
     C***  BEGIN DEL  ***  03/25/99  *********************************
     C**********           END
     C**********
     C**********           ELSE
     C**********
     C**********           ADD  1         XXCNT
     C**********           ADD  1         XXLINE
     C**********           MOVE *BLANKS   CDFSN
     C**********           MOVE *BLANKS   CDLSN
     C********** CDKEY     CHAINPCD01                60
     C**********           MOVE '1'       *IN25
     C**********           EXCPTDET
     C**********
     C***  END   DEL  ***  03/25/99  *********************************
     C                   END
     C*
     C                   READ      PCM01                                  60
     C*
     C                   END
     C*
     C                   EXCEPT    TOT
     C*
     C                   MOVE      '1'           *INLR
     C*_______________________________________________________________
     C*
     C*===============================================================
     C* CLOSE  - Compare reserve history explantions to close strings
     C*===============================================================
     C*
     C     CLOSE         BEGSR
     C*
     C                   MOVEL(P)  CLS(1)        U2EXPL
     C*
     C                   EXSR      STRING
     C*
     C     FOUND         IFEQ      *ON
     C     RHPGM         ANDEQ     'SRPRCC'
     C                   MOVE      *OFF          FOUND
     C                   ENDIF                                                  FOUND=*ON
     C*
     C                   ENDSR                                                  CLOSE
     C*
     C*===============================================================
     C* OPEN   - Compare reserve history explantions to open strings
     C*===============================================================
     C*
     C     OPEN          BEGSR
     C*
     C                   DO        1             X                 3 0
     C*
     C                   MOVEL(P)  OPN(X)        U2EXPL
     C*
     C                   EXSR      STRING
     C*
     C     FOUND         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF                                                  FOUND=*ON
     C*
     C                   ENDDO                                                  X->2
     C*
     C                   ENDSR                                                  OPEN
     C*
     C*===============================================================
     C* RSVHST - Get reserve history record to get status history date
     C*===============================================================
     C*
     C     RSVHST        BEGSR
     C*
     C     RHKY          SETGT     PRH01
     C     RHKY          READPE    PRH01                                  60
     C*
     C                   MOVE      *OFF          FOUND
     C*
     C     *IN60         DOWEQ     *OFF
     C     FOUND         ANDEQ     *OFF
     C*
     C     LO:UP         XLATE(P)  RHEXPL        U1EXPL
     C*
     C     CMSTAT        CASEQ     'O'           OPEN
     C     CMSTAT        CASEQ     'C'           CLOSE
     C                   ENDCS
     C*
     C     FOUND         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF                                                  FOUND=*ON
     C*
     C     RHKY          READPE    PRH01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   MOVEL(P)  RHEXPL        XXEXPL           27
     C*
     C                   MOVE      FOUND         *IN26
     C*
     C                   ENDSR                                                  RSVHST
     C*
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C*
     C     STRING        BEGSR
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1EXPL
     C                   PARM      60            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2EXPL
     C                   PARM      60            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C*
B01  C     SCNRES        IFNE      0
     C                   MOVE      *ON           FOUND
E01  C                   ENDIF                                                  SCNRES<>0
     C*
     C                   ENDSR                                                  STRING
     C*
     C*===============================================================
     C* WRITER - Write claim history record
     C*===============================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   PCH0P
     C*
     C                   Z-ADD     CMCO#         CHCO#
     C                   Z-ADD     CMFND         CHFND
     C                   Z-ADD     CMFYR         CHFYR
     C                   Z-ADD     CMCASE        CHCASE
     C                   Z-ADD     RHEFDT        CHEFDT
     C                   MOVEL(P)  RHEXPL        CHEXPL
     C                   MOVEL(P)  CMSTAT        CHOCST
     C                   MOVEL(P)  RHUSER        CHUSER
     C                   MOVEL(P)  RHPGM         CHPGM
     C                   Z-ADD     RHTIME        CHTIME
     C                   Z-ADD     RHDT          CHDT
     C*
     C                   WRITE     PCH0P
     C*
     C                   ENDSR                                                  WRITER
     C*_______________________________________________________________
     C*
     OQSYSPRT   E            HEAD           2 03
     O                                           19 'CO# FND FYR   CASE#'
     O                                           29 'LAST NAME'
     O                                           61 'FIRST NAME'
     O***  BEGIN CHG  ***  03/25/99  *********************************
     O**********                         95 'CHOCST  CMSTAT'
     O                                           95 'CMSTAT  CHOCST'
     O                                         +003 'RHEFDT RHEXPL'
     O***  END   CHG  ***  03/25/99  *********************************
     O          E            DET            1
     O                       CMCO#                3
     O                       CMFND                7
     O                       CMFYR         Z     11
     O                       CMCASE        Z     19
     O                       CDLSN               50
     O                       CDFSN               66
     O***  BEGIN CHG  ***  03/25/99  *********************************
     O**********      N25      CHOCST    84
     O**********      N25      CMSTAT    92
     O**********       25                92 'NO WMCHP RECORD'
     O              N25      CMSTAT              84
     O              N25      CHOCST              92
     O               25                          95 'NO WMCHP RECORD'
     O               26      RHEFDT        Z    104
     O               26      XXEXPL             132
     O              N26                         122 'NO WMRHP RECORDS FIT FOR'
     O              N26                        +001 'CHANGE'
     O***  END   CHG  ***  03/25/99  *********************************
     O          E            TOT         2  1
     O                                           13 'TOTAL COUNT:'
     O                       XXCNT         J     30
** Close strings
CLOSE
** Open strings
OPEN
