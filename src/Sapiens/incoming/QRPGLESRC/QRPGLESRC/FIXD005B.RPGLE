      *  (C) 2012 StoneRiver Inc. All rights reserved.
      *   StoneRiver is a trademark of StoneRiver, Inc.
      *
      *****************************************************************
     H/TITLE FIXD005B - Retrieve PV1 and other payment request attributes
      *****************************************************************
      *
      * DESCRIPTION:  This pgm retrieves the pv1 and other payment request
      *               attributes for records in file BADPV2R. It also
      *               identifies records that will not be updated by
      *               the initial data fix.
      *
      * CREATED:      06/14/12  C:180    SUPPORT 79581
      ********************************************************************
      * REVISIONS:
      *
     FBADPV2R   UF   E             DISK    PREFIX(x) RENAME(BADPV2R:BADPVR)
     FWARPL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D XXCO#           S              3  0 INZ(777)
     D OBR2            S                   LIKE(RPBR2)
     D OTRCD           S                   LIKE(RPTRCD)
     D OTYPE           S                   LIKE(RPTYPE)
     D ORECD           S                   LIKE(RPRECD)
     D ORETP           S                   LIKE(RPRETP)
     D ODESC           S                   LIKE(RPDESC)
     D TOTCNT          S              5  0 INZ
     D CNT             S              5  0 INZ
     D J               S              3  0 INZ
     D TOTCNT2         S              5  0 INZ
     D @@BR2           S                   DIM(4) LIKE(RPBR2) INZ
     D @@TRCD          S                   DIM(4) LIKE(RPTRCD)INZ
     D @@TYPE          S                   DIM(4) LIKE(RPTYPE)INZ
     D @@RECD          S                   DIM(4) LIKE(RPRECD)INZ
     D @@RETP          S                   DIM(4) LIKE(RPRETP)INZ
     D @@AMT           S                   DIM(4) LIKE(RPAMT) INZ
     D @@DESC          S                   DIM(4) LIKE(RPDESC) INZ

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D
     C     *ENTRY        PLIST
     C                   PARM                    RPT               1

     C     RPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    xRPFND
     C                   KFLD                    xRPFYR
     C                   KFLD                    xRPCASE
     C                   KFLD                    xRPSEQ#

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
     C                   PARM      *ZEROS        @@DESC

     C     RPT           IFEQ      'Y'
     C                   SETON                                        80
     C                   ENDIF

     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C   80              EXCEPT    HEAD


     C                   READ      BADPV2R
     C                   DOW       NOT %EOF(BADPV2R)

     C     RPKY1         CHAIN     WARPL
     C                   IF        %FOUND(WARPL)
     C                   MOVE      xRPFND        @@GRP#
     C                   MOVE      xRPFYR        @@FYR
     C                   MOVE      xRPCASE       @@CASE
     C                   MOVE      xRPEOB#       @@EOB#
     C                   MOVE      RPAMT         @@NAMT

     C                   CALL      'FIXD005A'    P5A

     C                   MOVE      *OFF          *IN75
     C                   MOVE      *ZEROS        CNT
     C                   DO        4             J
     C                   SELECT
     C                   WHEN       RPBR2 <> *BLANKS and  RPTRCD = *BLANKS and
     C                              RPTYPE = *BLANKS and  RPRECD = *BLANKS and
     C                              RPRETP = *BLANKS and
     C                              @@BR2(J)  =  RPBR2   and
     C                              @@AMT(J)  =  RPAMT   and
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
     C                   MOVE      @@BR2(J)      xRPBR2
     C                   MOVE      @@TRCD(J)     xRPTRCD
     C                   MOVE      @@TYPE(J)     xRPTYPE
     C                   MOVE      @@RECD(J)     xRPRECD
     C                   MOVE      @@RETP(J)     xRPRETP
     C                   MOVE      @@DESC(J)     xRPDESC
     C                   EVAL      CNT = CNT + 1

     C                   WHEN       RPBR2 =  *BLANKS and  RPTRCD = *BLANKS and
     C                              RPTYPE = *BLANKS and  RPRECD = *BLANKS and
     C                              RPRETP = *BLANKS and
     C                              @@BR2(J)  > *BLANKS  and
     C                              @@AMT(J)  =  RPAMT   and
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
     C                   MOVE      @@BR2(J)      xRPBR2
     C                   MOVE      @@TRCD(J)     xRPTRCD
     C                   MOVE      @@TYPE(J)     xRPTYPE
     C                   MOVE      @@RECD(J)     xRPRECD
     C                   MOVE      @@RETP(J)     xRPRETP
     C                   MOVE      @@DESC(J)     xRPDESC
     C                   EVAL      CNT = CNT + 1
     C                   ENDSL
     C
     C                   ENDDO

     C                   IF        CNT <> 1
     C                   MOVEL(P)  'DONTFIX'     xRPRQST
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


     C                   READ      BADPV2R
     C                   ENDDO

     C                   IF        *IN80
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    TOT1
     C                   ENDIF

     C                   EVAL      *INLR = *ON

     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                                           65 'Retrieve PV1 and other'
     O                                         +001 'payment request attributes'
     O                                         +001 '- PAYMENT REQUEST FILE'
     O                       DSDATE        Y    131
     O          E            HEAD           2
     O                                           10 'WTFIXD005B'
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
     O                       xRPFND        4      3
     O                       xRPFYR        4      9
     O                       xRPCASE       4     20
     O                       xRPSEQ#       4     28
     O                       OBR2           B    37
     O                       OTRCD          B    40
     O                       OTYPE          B    54
     O                       ORECD          B    60
     O                       ORETP          B    64
     O                       RPAMT         K     80
     O                       ODESC          B   119
     O               75                         125 'PV1-ERR'
     O               75      OBR2           B  +001
     O               75      RPBR2             +001
     O          E            ERR         2
     O                       xRPFND        4      3
     O                       xRPFYR        4      9
     O                       xRPCASE       4     20
     O                       xRPSEQ#       4     28
     O                                         +001 'NOT FOUND IN PAYMENT'
     O                                         +001 'REQUEST FILE.'
     O          E            TOT1        2
     O                                              'COUNT:'
     O                       TOTCNT        Z   +001
     O                                          +10 'ERROR COUNT:'
     O                       TOTCNT2       Z   +001
