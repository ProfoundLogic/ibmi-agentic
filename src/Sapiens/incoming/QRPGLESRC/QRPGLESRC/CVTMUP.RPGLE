     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE CVTMUP - Convert Assessable Mutual Forms   WMMUP
     H*****************************************************************
     H*
     H*        Read WMMUP, all records:
     H*                      o initialize certain fields
     H*                      o get description from WTENL3
     H*                      o update
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
     F*     Conversion fund selections
     F*     Assessable Mutual Forms File
     FWMMUL     UPE  E           K DISK
     F*
     F*     Optical file
     FWTENL3    IF   E           K DISK
     F*
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     F*
     FQSYSPRT2  O    F  132        PRINTER OFLIND(*INOA)

     D*
     D*
     D*   System date and time.
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*   Unpack key fields
     D                 DS
     D  MUCO#                  1      3  0
     D  MUFND                  4      6  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*
     D FM313           C                   CONST('WC000313       ')
     D FM01A           C                   CONST('WC000001A      ')
     D*
     D ERR01           C                   CONST('01 No xref  ')
     D ERR02           C                   CONST('02          ')
     F*

     I*================================================================
     I*     ARRAYS
     I*================================================================
     I*
     IPMU01         55

     C*================================================================
     C*     MAIN LOGIC
     C*================================================================
     C*
     C*   Process Fund Conversion Control File
     C     *IN55         IFEQ      *ON                                          Got record
     C                   ADD       1             MUCNT             7 0
     C*
     C                   MOVE      MUEXFL        SVEXFL            1
     C                   MOVE      MUMNAU        SVMNAU            1
     C                   MOVE      MUPRNT        SVPRNT            1
     C                   MOVEL     MUDESC        OLDESC           30
     C*
     C*     Get Endorsement Form Table record
     C     ENKY          CHAIN     PEN01                              61
     C*
     C     *IN61         IFEQ      *ON
     C                   ADD       1             ERCNT             7 0
     C                   MOVEL     *BLANKS       ENDESC
     C*    Print Error
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD2
     C                   MOVE      *OFF          *INOA
     C                   ENDIF                                                  .INOA
     C                   EXCEPT    DETL2
     C*
     C                   ELSE                                                   .IN61 OFF
     C*
     C                   MOVEL     ENDESC        MUDESC
     C                   ENDIF                                                  .IN61 OFF
     C*
     C*
     C*     Update WMMUP
     C                   MOVEL     'I'           MUEXFL
     C*
     C     MUFORM        IFEQ      FM313
     C                   MOVEL     'M'           MUMNAU
     C                   ELSE                                                   .FORM = 313
     C                   MOVEL     'A'           MUMNAU
     C                   ENDIF                                                  .FORM = 313
     C*
     C     MUFORM        IFEQ      FM01A
     C                   MOVEL     'N'           MUPRNT
     C                   ELSE                                                   .FORM = 01A
     C                   MOVEL     'Y'           MUPRNT
     C                   ENDIF                                                  .FORM = 01A
     C*
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      MUDT
     C                   MOVEL     Q1PGM         MUPGM
     C                   MOVEL     Q1USER        MUUSER
     C                   Z-ADD     DSTIME        MUTIME
     C*
     C                   UPDATE    PMU01
     C                   ADD       1             UPCNT             7 0
     C*    Print Listing
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF                                                  .INOF
     C                   EXCEPT    DETL
     C*
     C                   ENDIF                                                  .IN55
     C*
     C*
     CLR                 EXCEPT    HEAD
     CLR                 EXCEPT    TOTL
     CLR                 EXCEPT    HEAD2
     CLR                 EXCEPT    TOTL2
     C*****************************************************************
     C*    *INZSR   Initialization
     C*****************************************************************
     C*
     C     *INZSR        BEGSR
     C*
     C*  Keylist for Endorsement Table WTENL3
     C*
     C     ENKY          KLIST
     C                   KFLD                    MUCO#
     C                   KFLD                    MUFORM
     C                   KFLD                    MUFND
     C*
     C****************************************************************
     C*
     C*
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD2
     C*
     C                   ENDSR
     C*
     O*****************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME               8 '  :  :  '
     O                                           44 'Convert WMMUP Assessab'
     O                                           66 'le Mutual Forms       '
     O                       UDATE         Y     78
     O          E            HEAD           3
     O                       Q1PGM               10
     O                       PAGE          Z     70
     O                                           78 'PAGE'
     O*
     O          E            HEAD           1
     O                                            3 'Grp'
     O                                            7 'YR'
     O                                           16 'Employer'
     O                                           21 'Div'
     O                                           28 'Form'
     O                                           48 ' NEW   OLD'
     O                                           61 'Description'
     O          E            DETL           1
     O                       MUFND                3
     O                       MUFYR                7
     O                       MUEMP#              17
     O                       MUDIV               23
     O                       MUFORM              39
     O                       MUEXFL              41
     O                       MUMNAU              42
     O                       MUPRNT              43
     O                       SVEXFL              46
     O                       SVMNAU              47
     O                       SVPRNT              48
     O                       MUDESC              80
     O                       OLDESC             111
     O          E            TOTL        1  1
     O                                           44 '*** TOTALS ***        '
     O          E            TOTL        1  1
     O                                           44 'WMMUP Records Read . .'
     O                                           55 ' . . . . . '
     O                       MUCNT               66
     O          E            TOTL        1  1
     O                                           44 'WMMUP Records Updated '
     O                                           55 ' . . . . . '
     O                       UPCNT               66
     O          E            TOTL        1  1
     O                                           44 'Endorsement Table not '
     O                                           55 'found  . . '
     O                       ERCNT               66
     O*
     OQSYSPRT2  E            HEAD2          1 06
     O                       DSTIME               8 '  :  :  '
     O                                           44 'Convert WMMUP Assessab'
     O                                           66 'le Mutual Forms       '
     O                       UDATE         Y     78
     O          E            HEAD2          3
     O                       Q1PGM               10
     O                                           44 'Errors -- WTENP Endors'
     O                                           66 ' Table Not Found      '
     O                       PAGE          Z     70
     O                                           78 'PAGE'
     O*
     O          E            HEAD2          1
     O                                           48 'FLAGS XMP '
     O          E            HEAD2          1
     O                                            3 'Grp'
     O                                            7 'YR'
     O                                           16 'Employer'
     O                                           21 'Div'
     O                                           28 'Form'
     O                                           48 ' NEW   OLD'
     O                                           61 'Description'
     O          E            DETL2          1
     O                       MUFND                3
     O                       MUFYR                7
     O                       MUEMP#              17
     O                       MUDIV               23
     O                       MUFORM              39
     O                       MUEXFL              41
     O                       MUMNAU              42
     O                       MUPRNT              43
     O                       SVEXFL              46
     O                       SVMNAU              47
     O                       SVPRNT              48
     O                       MUDESC              80
     O                       OLDESC             111
     O          E            TOTL2       1  1
     O                                           44 '*** TOTALS ***        '
     O          E            TOTL2       1  1
     O                                           44 'Endorsement Table not '
     O                                           55 'found  . . '
     O                       ERCNT               66
     O*
