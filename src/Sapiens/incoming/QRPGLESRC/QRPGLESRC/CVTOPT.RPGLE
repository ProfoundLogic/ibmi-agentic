     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE CVTOPT - Convert Document/optical files
     H*               SMDLP & SMPRP
     H*****************************************************************
     H*
     H*        If PNAM = W0050R, rearrange DARTS Key a la V3
     H*
     H*      10.20.98   C:58 81WLE  For V3 Conversion
     H*
     H* REVISIONS:
     H*
     F*
     F*================================================================
     F*     FILES
     F*================================================================
     F*
     F*     Document file
     FSMDLP     UPE  E             DISK
     F*
     F*     Optical file
     FSMPRP     USE  E             DISK
     F*
     F*     DARTS Diary File by  PNAM/KEY
     FSDCMLC    IF   E           K DISK
     F*
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D*
     D*
     D*   System date and time.
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*   DATA STRUCTURE FOR OLD DARTS Key
     D                 DS
     D  XOKEY                  1     40
     D  XOCO#                  1      3  0
     D  XOFND                  4      6  0
     D  XOEMP#                 7     11  0
     D  XODIV                 12     14  0
     D  XZEMP#                 7     11
     D  XZDIV                 12     14
     D*
     D*   DATA STRUCTURE FOR NEW DARTS Key
     D                 DS
     D  XNKEY                  1     40
     D  XNCO#                  1      3  0
     D  XNFND                  4      6  0
     D  XNEMP#                 7     15  0
     D  XNDIV                 16     20  0
     D  NUKEY                  1     20
     D*
     D ERR01           C                   CONST('01 No xref  ')
     D ERR02           C                   CONST('02          ')
     F*

     I*================================================================
     I*     ARRAYS
     I*================================================================
     I*
     IPDL01         55
     IPPR01         56

     C*================================================================
     C*     MAIN LOGIC
     C*================================================================
     C*
     C*   Process document location record
     C     *IN55         IFEQ      *ON                                          Got record
     C                   ADD       1             DLCNT             7 0
     C*
     C*     Rearrange key
     C                   MOVEL     DLKEY         XOKEY
     C                   Z-ADD     XOCO#         XNCO#
     C                   Z-ADD     XOFND         XNFND
     C*
     C                   MOVE      *OFF          *IN66
     C                   TESTN                   XZEMP#               66
     C     *IN66         IFEQ      *ON
     C                   Z-ADD     XOEMP#        XNEMP#
     C                   ENDIF                                                  .IN61 OFF
     C                   MOVE      *OFF          *IN66
     C                   TESTN                   XZDIV                66
     C     *IN66         IFEQ      *ON
     C                   Z-ADD     XODIV         XNDIV
     C                   ENDIF                                                  .IN61 OFF
     C*
     C*     Get DARTS record with PNAM = W0050R
     C     DARTKY        CHAIN     PCM01                              61
     C*
     C*     If there, update location record
     C     *IN61         IFEQ      *OFF
     C                   ADD       1             XXUP1             7 0
     C                   MOVEL     NUKEY         DLKEY
     C                   UPDATE    PDL01
     C*    Print Log
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOA
     C                   ENDIF                                                  .INOA
     C                   EXCEPT    DETL
     C                   ENDIF                                                  .IN61 OFF
     C*
     C                   ENDIF                                                  .IN55
     C*
     C*   Process optical purge request
     C     *IN56         IFEQ      *ON                                          Got record
     C                   ADD       1             PRCNT             7 0
     C*
     C*     Rearrange key
     C                   MOVEL     PRKEY         XOKEY
     C                   Z-ADD     XOCO#         XNCO#
     C                   Z-ADD     XOFND         XNFND
     C*
     C                   MOVE      *OFF          *IN66
     C                   TESTN                   XZEMP#               66
     C     *IN66         IFEQ      *ON
     C                   Z-ADD     XOEMP#        XNEMP#
     C                   ENDIF                                                  .IN61 OFF
     C                   MOVE      *OFF          *IN66
     C                   TESTN                   XZDIV                66
     C     *IN66         IFEQ      *ON
     C                   Z-ADD     XODIV         XNDIV
     C                   ENDIF                                                  .IN61 OFF
     C*
     C*     Get DARTS record with PNAM = W0050R
     C     DARTKY        CHAIN     PCM01                              61
     C*
     C*     If there, update purge record
     C     *IN61         IFEQ      *OFF
     C                   ADD       1             XXUP2             7 0
     C                   MOVEL     NUKEY         PRKEY
     C                   UPDATE    PPR01
     C*    Print Log
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOA
     C                   ENDIF                                                  .INOA
     C                   EXCEPT    DETL
     C                   ENDIF                                                  .IN61 OFF
     C*
     C                   ENDIF                                                  .IN56
     C*
     CLR                 EXCEPT    HEAD
     CLR                 EXCEPT    TOTL
     C*****************************************************************
     C*    *INZSR   Initialization
     C*****************************************************************
     C*
     C     *INZSR        BEGSR
     C*
     C*
     C*  Keylist for DARTS SDCMLI File
     C*
     C     DARTKY        KLIST
     C                   KFLD                    XXPNAM
     C                   KFLD                    XNKEY
     C*
     C                   MOVEL     'W0050R'      XXPNAM
     C*
     C****************************************************************
     C     *LIKE         DEFINE    CMPNAM        XXPNAM
     C****************************************************************
     C*
     C                   Z-ADD     *ZERO         XXCNT             7 0
     C                   Z-ADD     *ZERO         XXADD             7 0
     C                   Z-ADD     *ZERO         XXCNT2            7 0
     C                   Z-ADD     *ZERO         XXADD2            7 0
     C*
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*
     C                   ENDSR
     C*
     O*****************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME               8 '  :  :  '
     O                                           45 'SMDLP and SMPRP Conv'
     O                                           61 'esion Register'
     O                       UDATE         Y     78
     O          E            HEAD           3
     O                       Q1PGM               10
     O                       PAGE          Z     70
     O                                           78 'PAGE'
     O*
     O          E            HEAD           1
     O                                            6 'File  '
     O                                           13 'Key'
     O                                           38 'Old Key'
     O          E            DETL           1
     O               55                           6 'SMDLP '
     O               56                           6 'SMPRP '
     O                       XNKEY               50
     O                       XOKEY               71
     O          E            TOTL        1  1
     O                                           30 'SMDLP Records Read:'
     O                       DLCNT               41
     O          E            TOTL        1  1
     O                                           30 'Total SMDLP Updated:'
     O                       XXUP1               41
     O          E            TOTL        1  1
     O                                           30 'SMPRP Records Read:'
     O                       PRCNT               41
     O          E            TOTL        1  1
     O                                           30 'Total SMPRP Updated:'
     O                       XXUP2               41
     O*
