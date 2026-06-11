      *  (C) 2012 StoneRiver Inc. All rights reserved.
      *   StoneRiver is a trademark of StoneRiver, Inc.
      *
      *****************************************************************
     H/TITLE FIXD005 - Retrieve PV1 and other payment attributes
      *****************************************************************
      *
      * DESCRIPTION:  This pgm retrieves the pv1 and other payment
      *               attributes for records in file BADPV2. It also
      *               identifies records that will not be updated by
      *               the initial data fix.
      *
      * CREATED:      06/14/12  C:180    SUPPORT 79581
      ********************************************************************
      * REVISIONS:
      *
     FBADPV2    UF   E             DISK    PREFIX(x) RENAME(BADPV2:BADPVR)
     FWDPAL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D XXCO#           S              3  0 INZ(777)
     D OBR2            S                   LIKE(PABR2)
     D OTRCD           S                   LIKE(PATRCD)
     D OTYPE           S                   LIKE(PATYPE)
     D ORECD           S                   LIKE(PARECD)
     D ORETP           S                   LIKE(PARETP)
     D ODESC           S                   LIKE(PADESC)
     D TOTCNT          S              5  0 INZ
     D CNT             S              5  0 INZ
     D J               S              3  0 INZ
     D TOTCNT2         S              5  0 INZ
     D @@BR2           S                   DIM(4) LIKE(PABR2) INZ
     D @@TRCD          S                   DIM(4) LIKE(PATRCD)INZ
     D @@TYPE          S                   DIM(4) LIKE(PATYPE)INZ
     D @@RECD          S                   DIM(4) LIKE(PARECD)INZ
     D @@RETP          S                   DIM(4) LIKE(PARETP)INZ
     D @@AMT           S                   DIM(4) LIKE(PAAMT) INZ
     D @@DESC          S                   DIM(4) LIKE(PADESC) INZ

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D
     C     *ENTRY        PLIST
     C                   PARM                    RPT               1

     C     PAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    xPAFND
     C                   KFLD                    xPAFYR
     C                   KFLD                    xPACASE
     C                   KFLD                    xPASEQ#

     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     P5A           PLIST
     C                   PARM      *BLANK        @@CALL            1
     C                   PARM      '777'         @@CO#             3
     C                   PARM      *BLANKS       @@SUB#            5
     C                   PARM                    @@GRP#            3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@EOB#            9
     C                   PARM      *BLANKS       @@RTCD            1
     C                   PARM                    @@NAMT            9 2
     C                   PARM      *BLANKS       @@BR2
     C                   PARM      *BLANKS       @@TRCD
     C                   PARM      *BLANKS       @@TYPE
     C                   PARM      *BLANKS       @@RECD
     C                   PARM      *BLANKS       @@RETP
     C                   PARM      *ZEROS        @@AMT
     C                   PARM      *BLANKS       @@DESC

     C     RPT           IFEQ      'Y'
     C                   SETON                                        80
     C                   ENDIF

     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C   80              EXCEPT    HEAD


     C                   READ      BADPV2
     C                   DOW       NOT %EOF(BADPV2)

     C     PAKY1         CHAIN     WDPAL
     C                   IF        %FOUND(WDPAL)
     C                   MOVE      xPAFND        @@GRP#
     C                   MOVE      xPAFYR        @@FYR
     C                   MOVE      xPACASE       @@CASE
     C                   MOVE      xPAEOB#       @@EOB#
     C                   MOVE      PAAMT         @@NAMT

     C                   CALL      'FIXD005A'    P5A

     C                   MOVE      *OFF          *IN75
     C                   MOVE      *ZEROS        CNT
     C                   DO        4             J
     C                   SELECT
     C                   WHEN       PABR2 <> *BLANKS and  PATRCD = *BLANKS and
     C                              PATYPE = *BLANKS and  PARECD = *BLANKS and
     C                              PARETP = *BLANKS and
     C                              @@BR2(J)  =  PABR2   and
     C                              @@AMT(J)  =  PAAMT   and
     C                              @@TRCD(J) <> *BLANKS and                    3
     C                              @@TYPE(J) <> *BLANKS and                    3
     C                              @@RECD(J) <> *BLANKS and                    3
     C                              @@RETP(J) <> *BLANKS                        3
     C                   MOVE      @@BR2(J)      OBR2
     C                   MOVE      @@TRCD(J)     OTRCD
     C                   MOVE      @@TYPE(J)     OTYPE
     C                   MOVE      @@RECD(J)     ORECD
     C                   MOVE      @@RETP(J)     ORETP
     C                   MOVE      @@DESC(J)     ODESC
     C                   MOVE      @@BR2(J)      xPABR2
     C                   MOVE      @@TRCD(J)     xPATRCD
     C                   MOVE      @@TYPE(J)     xPATYPE
     C                   MOVE      @@RECD(J)     xPARECD
     C                   MOVE      @@RETP(J)     xPARETP
     C                   MOVE      @@DESC(J)     xPADESC
     C                   EVAL      CNT = CNT + 1

     C                   WHEN       PABR2 =  *BLANKS and  PATRCD = *BLANKS and
     C                              PATYPE = *BLANKS and  PARECD = *BLANKS and
     C                              PARETP = *BLANKS and
     C                              @@BR2(J)  > *BLANKS  and
     C                              @@AMT(J)  =  PAAMT   and
     C                              @@TRCD(J) <> *BLANKS and                    3
     C                              @@TYPE(J) <> *BLANKS and                    3
     C                              @@RECD(J) <> *BLANKS and                    3
     C                              @@RETP(J) <> *BLANKS                        3
     C                   MOVE      @@BR2(J)      OBR2
     C                   MOVE      @@TRCD(J)     OTRCD
     C                   MOVE      @@TYPE(J)     OTYPE
     C                   MOVE      @@RECD(J)     ORECD
     C                   MOVE      @@RETP(J)     ORETP
     C                   MOVE      @@DESC(J)     ODESC
     C                   MOVE      @@BR2(J)      xPABR2
     C                   MOVE      @@TRCD(J)     xPATRCD
     C                   MOVE      @@TYPE(J)     xPATYPE
     C                   MOVE      @@RECD(J)     xPARECD
     C                   MOVE      @@RETP(J)     xPARETP
     C                   MOVE      @@DESC(J)     xPADESC
     C                   EVAL      CNT = CNT + 1
     C                   ENDSL
     C
     C                   ENDDO

     C                   IF        CNT <> 1
     C                   MOVEL(P)  'DONTFIX'     xPARQST
     C                   SETON                                        75
     C                   ENDIF

     C                   UPDATE    BADPVR


     C                   IF        *IN80
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   EVAL      TOTCNT = TOTCNT + 1
     C   75              EVAL      TOTCNT2 = TOTCNT2 + 1
     C                   ENDIF

     C                   ELSE
     C                   IF        *IN80
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    ERR
     C                   EVAL      TOTCNT2 = TOTCNT2 + 1
     C                   ENDIF
     C                   ENDIF


     C                   READ      BADPV2
     C                   ENDDO

     C                   IF        *IN80
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    TOT1
     C                   ENDIF

     C                   EVAL      *INLR = *ON

     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                                           75 'Retrieve PV1 and other'
     O                                         +001 'payment attributes'
     O                                         +001 '- PAYMENT FILE'
     O                       DSDATE        Y    131
     O          E            HEAD           2
     O                                           10 'WTFIXD005'
     O                                          125 'PAGE:'
     O                       PAGE          Z    131
     O          E            HEAD           2
     O                                            3 'FND'
     O                                           10 'YEAR'
     O                                           20 'CLAIM #'
     O                                           28 'SEQ #'
     O                                           37 'PV1'
     O                                           40 'TCDE'
     O                                           55 'TTYP'
     O                                           60 'RCDE'
     O                                           65 'RTYP'
     O                                           80 'AMT'
     O                                          100 'DESCRIPTION'
     O          E            DET         1
     O                       xPAFND        4      3
     O                       xPAFYR        4      9
     O                       xPACASE       4     20
     O                       xPASEQ#       4     28
     O                       OBR2           B    37
     O                       OTRCD          B    40
     O                       OTYPE          B    54
     O                       ORECD          B    60
     O                       ORETP          B    64
     O                       PAAMT         K     80
     O                       ODESC          B   119
     O               75                         125 'PV1-ERR'
     O               75      OBR2           B  +001
     O               75      PABR2             +001
     O          E            ERR         2
     O                       xPAFND        4      3
     O                       xPAFYR        4      9
     O                       xPACASE       4     20
     O                       xPASEQ#       4     28
     O                                         +001 'NOT FOUND IN PAYMENT FILE'
     O          E            TOT1        2
     O                                              'COUNT:'
     O                       TOTCNT        Z   +001
     O                                          +10 'ERROR COUNT:'
     O                       TOTCNT2       Z   +001
