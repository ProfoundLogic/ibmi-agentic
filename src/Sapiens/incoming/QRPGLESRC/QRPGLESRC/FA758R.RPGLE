     H**********************************************************************
     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H dftactgrp(*no) actgrp(*caller)
     H**********************************************************************
     H* PROGRAM: FA758R
     H* DESCRIPTION: API - NOTES WRITE PROGRAM
     H**********************************************************************
     H* CREATED: 04/30/08 C:191 PROJECT: TX045
     H**********************************************************************
     H* REVISIONS:                                                         *
     H*  07/18/23 C:342 Project: NY060                                     *
     H*                 NY Compliance: CARC/RARC Update                    *
     H*                                                                    *
     H**********************************************************************
     F*
     FFAEOL4    IF   E           K DISK
     FFAEEL     IF   E           K DISK
     FFDDEL     IF   E           K DISK
     FFAEEP     O    E             DISK
     F                                     RENAME(PEE01:PEE02)
     FFDDEP     O    E             DISK
     F                                     RENAME(PDE01:PDE02)
     F*
     D XXCO#           S              3  0 INZ                                  Company #
     D XXSUB#          S              3  0 INZ                                  Subscriber #
     D XXFND           S              3  0 INZ                                  Fund #
     D XXFYR           S              3  0 INZ                                  Year
     D XXCASE          S              7  0 INZ                                  Case
     D XXEOB#          S              9  0 INZ                                  EOB#
     D XXRCN           S              3  0 INZ                                  Re-con #
     D XXDSEQ          S              5  0 INZ
     D XXCALL          S              1    INZ                                  'H' or 'D'
     D XXOVAL          S             70    INZ                                  Original Value
     D XXNVAL          S             70    INZ                                  New Value
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
     C     EOKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     EEKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     EEKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKSEQ#
     C*
     C     DEKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     DEKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C                   KFLD                    KKSEQ#
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
     C                   PARM                    @@RCN             3 0
     C                   PARM                    @@DSEQ            5 0
     C                   PARM                    @@CALL            1
     C                   PARM                    @@OVAL           70
     C                   PARM                    @@NVAL           70
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
     C     *LIKE         DEFINE    @@RCN         KKRCN
     C     *LIKE         DEFINE    @@DSEQ        KKDSEQ
     C     *LIKE         DEFINE    DESEQ#        KKSEQ#
     C     *LIKE         DEFINE    DESEQ#        XXSEQ#
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
     C                   EVAL      XXRCN = @@RCN
     C                   EVAL      XXDSEQ= @@DSEQ
     C                   EVAL      XXCALL= @@CALL
     C                   EVAL      XXOVAL= @@OVAL
     C                   EVAL      XXNVAL= @@NVAL
     C*
     C* Set key field variables
     C                   EVAL      KKCO# = XXCO#
     C                   EVAL      KKSUB#= XXSUB#
     C                   EVAL      KKFND = XXFND
     C                   EVAL      KKFYR = XXFYR
     C                   EVAL      KKCASE= XXCASE
     C                   EVAL      KKEOB#= XXEOB#
     C                   EVAL      KKRCN = XXRCN
     C                   EVAL      KKDSEQ= XXDSEQ
     C*
     C* Get EOB Transaction file record
     C     EOKY          CHAIN     PEO01
     C                   IF        %FOUND
     C*
     C* Call relevant subroutine depending on @@CALL parm
     C                   SELECT
     C                   WHEN      XXCALL = 'H'
     C* Write header record to FAEEP
     C                   EXSR      SRWRTEEP
     C                   WHEN      XXCALL = 'D'
     C* Write Detail record to FDDEP
     C                   EXSR      SRWRTDEP
     C                   ENDSL
     C                   ENDIF                                                  %FOUND (PEO01)
     C*
     C                   SETON                                        LR
     C                   RETURN
     C*
     C*================================================================
     C* SRWRTEEP - Write header note
     C*================================================================
     C     SRWRTEEP      BEGSR
     C*
     C                   MOVE      *HIVAL        KKSEQ#
     C*
     C     EEKY2         SETGT     PEE01
     C     EEKY1         READPE    PEE01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C     EESEQ#        ADD       1             XXSEQ#
     C                   ELSE
     C                   Z-ADD     1             XXSEQ#
     C                   ENDIF
     C*
     C                   CLEAR                   PEE01
     C*
     C                   Z-ADD     EOCO#         EECO#
     C                   Z-ADD     EOSUB#        EESUB#
     C                   Z-ADD     EOFND         EEFND
     C                   Z-ADD     EOFYR         EEFYR
     C                   Z-ADD     EOCASE        EECASE
     C                   Z-ADD     EOEOB#        EEEOB#
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVEL(P)  *BLANKS       EEEOB
     C                   MOVEL(P)  *BLANKS       EEEOBCD
     C**** End Chg *** 07/18/23 **************************************
     C                   Z-ADD     EORCN         EERCN
     C                   Z-ADD     XXSEQ#        EESEQ#
     C*
     C                   MOVEL(P)  XXOVAL        EENOT1
     C                   MOVEL(P)  XXNVAL        EENOT2
     C                   MOVEL     'A'           EESTAT
     C                   MOVEL(P)  'N'           EEPRNT
     C*
     C                   MOVEL(P)  EOSS#         EESS#
     C                   Z-ADD     EOACDT        EEACDT
     C*
     C                   MOVEL     Q1USER        EEUSER
     C                   MOVEL     Q1USER        EEIUSR
     C                   MOVEL     Q1PGM         EEPGM
     C                   MOVEL     Q1PGM         EEIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EEDT
     C*
     C                   MOVEL     SRDTO         EEIDT
     C*
     C                   MOVEL     DSTIME        EETIME
     C                   MOVEL     DSTIME        EEITIM
     C*
     C                   WRITE     PEE02
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* SRWRTDEP - Write detail note
     C*================================================================
     C     SRWRTDEP      BEGSR
     C*
     C                   MOVE      *HIVAL        KKSEQ#
     C*
     C     DEKY2         SETGT     PDE01
     C     DEKY1         READPE    PDE01                                  99
     C*
     C                   IF        *IN99 = *OFF
     C*
     C     DESEQ#        ADD       1             XXSEQ#
     C*
     C                   ELSE
     C*
     C                   Z-ADD     1             XXSEQ#
     C*
     C                   ENDIF
     C*
     C                   CLEAR                   PDE01
     C*
     C                   Z-ADD     EOCO#         DECO#
     C                   Z-ADD     EOSUB#        DESUB#
     C                   Z-ADD     EOFND         DEFND
     C                   Z-ADD     EOFYR         DEFYR
     C                   Z-ADD     EOCASE        DECASE
     C                   Z-ADD     EOEOB#        DEEOB#
     C                   Z-ADD     XXDSEQ        DEDSEQ
     C                   Z-ADD     XXSEQ#        DESEQ#
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVEL     *BLANKS       DEEOB
     C                   MOVEL     *BLANKS       DEEOBCD
     C**** End Chg *** 07/18/23 **************************************
     C                   MOVEL(P)  XXOVAL        DENOT1
     C                   MOVEL(P)  XXNVAL        DENOT2
     C                   MOVEL     'A'           DESTAT
     C                   MOVEL(P)  'N'           DEPRNT
     C                   Z-ADD     XXRCN         DERCN
     C                   MOVEL(P)  EOSS#         DESS#
     C                   Z-ADD     EOACDT        DEACDT
     C                   MOVE      'U'           DEESRC                         user entered
     C                   MOVEL     Q1USER        DEUSER
     C                   MOVEL     Q1USER        DEIUSR
     C                   MOVEL     Q1PGM         DEPGM
     C                   MOVEL     Q1PGM         DEIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DEDT
     C*
     C                   MOVEL     SRDTO         DEIDT
     C*
     C                   MOVEL     DSTIME        DETIME
     C                   MOVEL     DSTIME        DEITIM
     C*
     C                   WRITE     PDE02
     C*
     C                   ENDSR
