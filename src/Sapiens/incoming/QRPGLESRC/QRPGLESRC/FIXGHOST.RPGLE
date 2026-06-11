     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H**************************************************************************
     H*       StoneRiver is a trademark of StoneRiver, Inc.
     H*
     H*=========================================================================
     H/TITLE FIXGHOST  WC Clear WMRHP TI Amount if no associated Rsv Code  C: 58
     H*  Support Correction   WC099   Sequence #71191
     H*=========================================================================
     H*               = Workers Compensation =
     H*
     H**************************************************************************
     H*  CREATED: 12.31.09                C:58                  SUPPORT: 71191
     H*-------------------------------------------------------------------------
     H*
     H* DESCRIPTION:  Read WMRHL file; Test array RT 1-10; if RHRTxx is blank,
     H*               zero the corresponding amount in array SU RHSUxx (1-10).
     H**************************************************************************
     H* INDICATORS:
     H*     51  RI - read input WMRHL record
     H*     66  UPDATE THIS RECORD
     H*
     F*=========================================================================
     F*        F i l e s
     F*=========================================================================
     F*     INPUT CLAIM Reserve History WMRHL
     FWMRHL     UPE  E           K DISK
     F*
     F*    AUDIT REPORT
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     F*

     D*=========================================================================
     D*        A r r a y s
     D*=========================================================================
     D*     Conversion file arrays
     D FI              S              7  0 DIM(20)                              Nbr input
     D FU              S              7  0 DIM(20)                              Nbr updated
     D FW              S              7  0 DIM(20)                              Nbr written
     D FE              S              7  0 DIM(20)                              Error counts
     D FN              S             15    DIM(20) CTDATA PERRCD(1)             File names
     D*
     D*       Reserve History Total Arrays.
     D*
     D*  Copy books.
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*     Run Date and Time.
     D                 DS                  INZ
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D*     Reserve Array fields
     D                 DS                  INZ
     D  RT                     1     20
     D                                     DIM(10)
     D  RHRT01                 1      2
     D  RHRT02                 3      4
     D  RHRT03                 5      6
     D  RHRT04                 7      8
     D  RHRT05                 9     10
     D  RHRT06                11     12
     D  RHRT07                13     14
     D  RHRT08                15     16
     D  RHRT09                17     18
     D  RHRT10                19     20
     D                 DS                  INZ
     D  SU                     1     90  2
     D                                     DIM(10)
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2
     D*
     D* ------------------------------------------------------------------------
     D* Constants
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D*
     D*    Report Headings   length 33
     D TITL1           C                   CONST('StoneRiver Worker -
     D                                     s Compensation ')
     D TITL2           C                   CONST('Clear Extraneous  -
     D                                     Reserve Amounts')
     D TITL3           C                   CONST('SUPP Seq # 71191  -
     D                                                    ')

     I*
     I*
     I* PRIMARY   WMRHL Claim reserve history input record.
     I*
     IPRH01         51
     I                                          RHFYR         L5
     I                                          RHCASE        L5

     C*=========================================================================
     C*      M  A I  N    L  O  G  I  C
     C*=========================================================================
     C*    Got Input Record WMRHL
     C     *IN51         IFEQ      *ON
     C                   ADD       1             FI(1)
     C*
     C*    First of new case
     C     *INL5         IFEQ      *ON
     C                   ADD       1             FI(3)
     C                   ENDIF                                                  e L5 *ON
     C*
     C*    Bump through reserves 1-10
     C                   MOVE      *OFF          *IN66
     C                   DO        10            U                 3 0
     C     SU(U)         IFNE      *ZERO
     C     RT(U)         ANDEQ     *BLANK
     C                   MOVE      *ON           *IN66                          UPDATING
     C*
     C*    ACCUMULATE & Print
     C                   ADD       SU(U)         TOTTIZ           11 2
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF                                                  e OF *ON
     C                   EXCEPT    DETD
     C*
     C*    Clear TI Amount
     C                   Z-ADD     0             SU(U)
     C     U             ADD       10            B                 3 0
     C                   ADD       1             FU(B)
     C                   ENDIF                                                  e RT,U=*BLANK
     C                   ENDDO                                                  e DO 10
     C*
     C*    UPDATE IF ANY ZEROED
     C     *IN66         IFEQ      *ON
     C*
     C*
     C*    DELETE IF NO DOLLARS LEFT
     C                   XFOOT     SU            TOTTI            11 2
     C     TOTTI         IFEQ      *ZERO
     C                   DELETE    PRH01
     C                   ADD       1             FU(5)
     C                   ELSE                                                   x TOTTI = 0
     C*
     C*    Audit fields
     C*  NOTE Using the new (mostly unused) Initial fields for this FIX
     C                   Z-ADD     XXDT          RHIDT
     C                   MOVEL(P)  Q1USER        RHIUSR
     C                   MOVEL(P)  Q1PGM         RHIPGM
     C*    Update
     C                   UPDATE    PRH01
     C                   ADD       1             FU(4)
     C                   ENDIF                                                  e TOTTI <> 0
     C                   ENDIF                                                  e 66 *ON
     C*
     C*    Print Register
     C*****      *INOF     IFEQ *ON
     C*****                EXCPTHEAD
     C*****                EXCPTHED1
     C*****                MOVE *OFF      *INOF
     C*****                ENDIF                           e OF *ON
     C*****                EXCPTDET1
     C*
     C                   ENDIF                                                  e 51 *ON
     C*
     C*=========================================================================
     C*     Lx - Level Breaks
     C*=========================================================================
     CLR                 DO
     CL0                 EXSR      ENDER
     CLR                 ENDDO                                                  e LR DO
     C*=
     C*=========================================================================
     C*      S   U   B   R   O   U   T   I   N   E   S
     C*=========================================================================
     C*
     C*=========================================================================
     C*      ENDING  SUBROUTINE
     C*=========================================================================
     C     ENDER         BEGSR
     C*
     C*     Print File Totals
     C                   EXCEPT    HEAD
     C                   EXCEPT    TOTF
     C                   DO        20            F                 3 0
     C     FI(F)         IFNE      *ZERO
     C     FU(F)         ORNE      *ZERO
     C     FW(F)         ORNE      *ZERO
     C     FE(F)         ORNE      *ZERO
     C                   EXCEPT    DETF
     C                   ENDIF                                                  Fx, <> 0
     C                   ENDDO
     C*
     C                   ENDSR
     C*=========================================================================
     C*      INITIALIZATION  SUBROUTINE
     C*=========================================================================
     C     *INZSR        BEGSR
     C*                                                a
     C* ---  K  E  Y  ---  L  I  S  T  S  ----------------------------------
     C*   KEY LIST for
     C*
     C*
     C* ---- P A R A M E T E R --- L I S T S --------------------------
     C     *ENTRY        PLIST
     C                   PARM                    EFFDT             7 0
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
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
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
     C*-------------------------------------------------------------------------
     C* Like Defines
     C*-------------------------------------------------------------------------
     C*
     C     *LIKE         DEFINE    RHCO#         XXCO#                          Company
     C     *LIKE         DEFINE    RHFND         XXFND                          Group
     C     *LIKE         DEFINE    RHDT          XXDT                           Date 7.0 CYMD
     C*
     C*-------------------------------------------------------------------------
     C*     Initialize constants and workfields.
     C                   Z-ADD     777           XXCO#
     C                   Z-ADD     25            XXFND
     C                   TIME                    DSDTTM
     C*
     C*     Calculate today as CYMD
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C                   EXCEPT    HEAD
     C*
     C                   ENDSR
     O*=========================================================================
     O*      R   E   P   O   R   T        O  U  T  P  U  T
     O*=========================================================================
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME               8 '  :  :  '
     O                       TITL1               44
     O                       TITL2               77
     O                       TITL3              110
     O                       DSDATE              98 '  /  /    '
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       Q1USER             116
     O                       PAGE          Z    128
     O                                          123 'PAGE'
     O*
     O*=========================================================================
     O          E            HEAD        2  1
     O                                           22 'Grp FYR    Case  Eff d'
     O                                           44 'ate   Nbr TYP     Amou'
     O                                           66 'nt                    '
     O                                           88 '                      '
     O          E            DETD           1
     O                       RHFND         Z      3
     O                       RHFYR         Z      7
     O                       RHCASE        Z     15
     O                       RHEFDT        Y     27
     O                       U             Z     31
     O                       RT(U)               35
     O                       SU(U)         J     48
     O*
     O*=========================================================================
     O          E            TOTF        2  1
     O                                           22 '*** RECORD  COUNT SUMM'
     O                                           44 'MARY ***              '
     O          E            TOTF        1  2
     O                                           22 '  Description       In'
     O                                           44 'put read        Update'
     O                                           66 'd    Written out      '
     O                                           88 '   Errors             '
     O          E            DETF           1
     O                       FN(F)               15
     O                       FI(F)         Z     30
     O                       FU(F)         Z     45
     O                       FW(F)         Z     60
     O                       FE(F)         Z     75
     O*
     O          E            DETF           1
     O                                           42 'TOT INCURRED ZEROED . .'
     O                       TOTTIZ             116 '   ,   ,   .  -'
     O*
** TABLE      v       FN - File names   30 @ 15
WMRHL  IN         1
Select grp 25     2
Nbr of claims     3
Records Updated   4
Records Deleted   5
xxxxxxxxx6        6
xxxxxxxxx7        7
xxxxxxxxx8        8
xxxxxxxxx9        9
xxxxxxxx10       10
Cleared # 01     11
Cleared # 02     12
Cleared # 03     13
Cleared # 04     14
Cleared # 05     15
Cleared # 06     16
Cleared # 07     17
Cleared # 08     18
Cleared # 09     19
Cleared # 10     20
