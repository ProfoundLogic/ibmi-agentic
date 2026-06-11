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
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D @@BR2           S                   CTDATA DIM(15) PERRCD(1) LIKE(xPABR2)
     D @@CHK           S              7  0 CTDATA DIM(15) PERRCD(1)
     D IX              S              3  0 INZ
     D OBR2            S                   LIKE(xPABR2)
     D OTRCD           S                   LIKE(xPATRCD)
     D OTYPE           S                   LIKE(xPATYPE)
     D ORECD           S                   LIKE(xPARECD)
     D ORETP           S                   LIKE(xPARETP)
     D ODESC           S                   LIKE(xPADESC)
     D TOTCNT          S              5  0 INZ
     D TOTCNT2         S              5  0 INZ

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C     *ENTRY        PLIST
     C                   PARM                    RPT               1


     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
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

     C                   EVAL      IX = 1
     C     xPACHK#       LOOKUP    @@CHK(IX)                              50
     C                   IF        *IN50
     C                   MOVE      @@BR2(IX)     xPABR2
     C                   UPDATE    BADPVR
     C                   MOVE      xPABR2        OBR2
     C                   MOVE      xPATRCD       OTRCD
     C                   MOVE      xPATYPE       OTYPE
     C                   MOVE      xPARECD       ORECD
     C                   MOVE      xPARETP       ORETP
     C                   MOVE      xPADESC       ODESC

     C                   IF        *IN80
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   EVAL      TOTCNT = TOTCNT + 1
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
     O                                           10 'WTFIXD005L'
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
     O                       xPAAMT        K     80
     O                       ODESC          B   119
     O          E            ERR         2
     O                       xPAFND        4      3
     O                       xPAFYR        4      9
     O                       xPACASE       4     20
     O                       xPASEQ#       4     28
     O                                         +001 'NOT FOUND IN PV1 ARRAY   '
     O          E            TOT1        2
     O                                              'COUNT:'
     O                       TOTCNT        Z   +001
     O                                          +10 'ERROR COUNT:'
     O                       TOTCNT2       Z   +001
**
UR
RP
TT
TT
TT
TT
TT
TP
TT
TT
TT
TT
TT
TT
TT
**
0017345
0022018
0008688
0009511
0017781
0020165
0024487
0019191
0023757
0003115
0010150
0012668
0015519
0007586
0010357
