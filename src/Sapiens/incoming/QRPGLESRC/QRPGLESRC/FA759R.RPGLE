     H**********************************************************************
     H* (C)COPYRIGHT FISERV, INC., 2008
     H**********************************************************************
     H* PROGRAM: FA759R
     H* DESCRIPTION: API - UPDATE OTHER AMOUNTS FIELDS
     H**********************************************************************
     H* CREATED: 05/27/08 C:191 PROJECT: 25MOS WO2
     H**********************************************************************
     H* REVISIONS:
     H**********************************************************************
     F*
     FFAEAL1    UF   E           K DISK
     F*
     D XXCO#           S              3  0 INZ                                  Company #
     D XXSUB#          S              3  0 INZ                                  Subscriber #
     D XXFND           S              3  0 INZ                                  Fund #
     D XXFYR           S              3  0 INZ                                  Year
     D XXCASE          S              7  0 INZ                                  Case
     D XXEOB#          S              9  0 INZ                                  EOB#
     D XXOCA1          S              9  2 INZ                                  AMOUNT
     D XXOCA2          S              9  2 INZ                                  AMOUNT
     D XXOCA3          S              9  2 INZ                                  AMOUNT
     D*
     D SRDTI           S              8  0
     D SRDTO           S             10
     D SRERR           S              1
     D SRFFMT          S              8
     D SRTFMT          S              8
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D DSDTTM                        14  0
     D  DSTIME                        6  0 OVERLAY(DSDTTM)
     D  DSDATE                        8  0 OVERLAY(DSDTTM:7)
     D*
     C*================================================================
     C* KEYLIST - DEFINITION
     C*================================================================
     C     EAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C*================================================================
     C* PARAMETER LIST
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C                   PARM                    @@EOB#            9 0
     C                   PARM                    @@OCA1            9 2
     C                   PARM                    @@OCA2            9 2
     C                   PARM                    @@OCA3            9 2
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    @@CO#         KKCO#
     C     *LIKE         DEFINE    @@SUB#        KKSUB#
     C     *LIKE         DEFINE    @@FND         KKFND
     C     *LIKE         DEFINE    @@FYR         KKFYR
     C     *LIKE         DEFINE    @@CASE        KKCASE
     C     *LIKE         DEFINE    @@EOB#        KKEOB#
     C*
     C*================================================================
     C*  MAINLINE CODE
     C*================================================================
     C* Set work variables
     C                   EVAL      XXCO# = @@CO#
     C                   EVAL      XXSUB#= @@SUB#
     C                   EVAL      XXFND = @@FND
     C                   EVAL      XXFYR = @@FYR
     C                   EVAL      XXCASE= @@CASE
     C                   EVAL      XXEOB#= @@EOB#
     C                   EVAL      XXOCA1= @@OCA1
     C                   EVAL      XXOCA2= @@OCA2
     C                   EVAL      XXOCA3= @@OCA3
     C*
     C* Set key field variables
     C                   EVAL      KKCO# = XXCO#
     C                   EVAL      KKSUB#= XXSUB#
     C                   EVAL      KKFND = XXFND
     C                   EVAL      KKFYR = XXFYR
     C                   EVAL      KKCASE= XXCASE
     C                   EVAL      KKEOB#= XXEOB#
     C*
     C* Get EOB Transaction file record
     C     EAKY          CHAIN     PEA01
     C                   IF        %FOUND
     C*
     C                   EVAL      EAOCA1= XXOCA1
     C                   EVAL      EAOCA2= XXOCA2
     C                   EVAL      EAOCA3= XXOCA3
     C*
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   MOVEL     DSTIME        EATIME
     C*
     C                   UPDATE    PEA01
     C                   ENDIF                                                  %FOUND (PEA01)
     C*
     C                   SETON                                        LR
     C                   RETURN
     C*
