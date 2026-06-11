     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H/TITLE F9999R - WCMS/FSMS PBILLS ANALYSIS
     H*****************************************************************
     H*                                                               *
     H* CALLED BY: F9999C                                             *
     H* CALLS TO:  NONE                                               *
     H*                                                               *
     H*****************************************************************
     H* CREATED ON 10/01/2002 BY C:62 FOR PROJECT FS243               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     FFAEOL67   IF   E           K DISK
     FFDCALR    IF   E           K DISK
     FFXANP     O    E             DISK

     D*
     D O1C             S              3    DIM(999)
     D O2C             S              3    DIM(999)
     D NTC             S              5    DIM(999)
     D O1A             S             11  2 DIM(999)
     D O2A             S             11  2 DIM(999)
     D NTA             S             11  2 DIM(999)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*========================================================================
     C* KLISTS
     C*========================================================================
     C     CAKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C*================================================================
     C* LIKE DEFINES
     C*================================================================
     C     *LIKE         DEFINE    EOTRDT        XXFRDT
     C     *LIKE         DEFINE    EOTRDT        XXTODT
     C     *LIKE         DEFINE    EOCHRG        XXCHRG
     C     *LIKE         DEFINE    EOMRA         XXMRA
     C     *LIKE         DEFINE    EOPAID        XXPAID
     C     *LIKE         DEFINE    EOAST         HDAST
     C     *LIKE         DEFINE    EOBTYP        HDBTYP
     C     *LIKE         DEFINE    ANBCNT        COUNTA
     C*========================================================================
     C* PLISTS
     C*========================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FRDT            8
     C                   PARM                    @@TODT            8
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*========================================================================
     C* BEGIN
     C*========================================================================
     C                   Z-ADD     *ZEROS        COUNTT            7 0
     C*
     C                   MOVE      @@FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   MOVE      @@TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   READ      PEO01                                  99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      CLRAMT
     C                   ENDIF
     C     *IN99         DOWEQ     *OFF
     C*
     C     EOAST         IFNE      HDAST
     C     EOBTYP        ORNE      HDBTYP
     C                   EXSR      WRTAMT
     C                   EXSR      CLRAMT
     C                   EXSR      ACCAMT
     C                   ELSE
     C                   EXSR      ACCAMT
     C                   ENDIF
     C*
     C                   READ      PEO01                                  99
     C     *IN99         IFEQ      *ON
     C                   EXSR      WRTAMT
     C                   ENDIF
     C                   ENDDO
     C*
     C                   EXSR      WRTCNT
     C*
     C*   END PROGRAM
     C*
     C                   SETON                                        LR
     C*================================================================
     C* ACCAMT - Accumulate Amount Records
     C*================================================================
     CSR   ACCAMT        BEGSR
     C*
     C                   ADD       1             COUNTA
     C                   ADD       1             COUNTT
     C                   ADD       EOCHRG        XXCHRG
     C                   ADD       EOMRA         XXMRA
     C                   ADD       EOPAID        XXPAID
     C*
     C* LET'S COLLECT OTHER 1 DISCOUNT INFO
     C*
     C     EOOTHR        IFNE      *ZEROS
     C     CAKY          CHAIN     PCA01                              99
     C     *IN99         DOWEQ     *OFF
     C     CAOTHR        IFNE      *ZEROS
     C                   Z-ADD     1             X
     C     CAOT1C        LOOKUP    O1C(X)                                 99
     C     *IN99         IFEQ      *ON
     C                   ADD       CAOTHR        O1A(X)
     C                   ELSE
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    O1C(X)                                 99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  CAOT1C        O1C(X)
     C                   Z-ADD     CAOTHR        O1A(X)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C     CAKY          READE     PCA01                                  99
     C                   ENDDO
     C                   ENDIF
     C*
     C* LET'S COLLECT OTHER 2 DISCOUNT INFO
     C*
     C     EOOTH2        IFNE      *ZEROS
     C     CAKY          CHAIN     PCA01                              99
     C     *IN99         DOWEQ     *OFF
     C     CAOTH2        IFNE      *ZEROS
     C                   Z-ADD     1             X
     C     CAOT2C        LOOKUP    O2C(X)                                 99
     C     *IN99         IFEQ      *ON
     C                   ADD       CAOTH2        O2A(X)
     C                   ELSE
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    O2C(X)                                 99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  CAOT2C        O2C(X)
     C                   Z-ADD     CAOTH2        O2A(X)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C     CAKY          READE     PCA01                                  99
     C                   ENDDO
     C                   ENDIF
     C*
     C* LET'S COLLECT NETWORK DISCOUNT INFO
     C*
     C     EONDIS        IFNE      *ZEROS
     C                   Z-ADD     1             X
     C     EONTWK        LOOKUP    NTC(X)                                 99
     C     *IN99         IFEQ      *ON
     C                   ADD       EONDIS        NTA(X)
     C                   ELSE
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    NTC(X)                                 99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  EONTWK        NTC(X)
     C                   Z-ADD     EONDIS        NTA(X)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR                                                  ACCAMT
     C*================================================================
     C* CLRAMT - Clears Amount Records
     C*================================================================
     CSR   CLRAMT        BEGSR
     C*
     C                   MOVEL(P)  EOAST         HDAST
     C                   MOVEL(P)  EOBTYP        HDBTYP
     C                   CLEAR                   COUNTA
     C                   CLEAR                   O1C
     C                   CLEAR                   O2C
     C                   CLEAR                   NTC
     C                   CLEAR                   O1A
     C                   CLEAR                   O2A
     C                   CLEAR                   NTA
     C                   CLEAR                   XXCHRG
     C                   CLEAR                   XXMRA
     C                   CLEAR                   XXPAID
     C*
     C                   ENDSR                                                  CLRAMT
     C*================================================================
     C* WRTAMT - Write Amount Records
     C*================================================================
     CSR   WRTAMT        BEGSR
     C*
     C* WRITE CHARGE RECORD
     C*
     C                   CLEAR                   PAN01
     C*
     C                   MOVE      @@CO#         ANCLT#
     C                   Z-ADD     XXFRDT        ANFRDT
     C                   Z-ADD     XXTODT        ANTODT
     C                   MOVEL(P)  HDAST         ANAST
     C                   MOVEL(P)  HDBTYP        ANBTYP
     C                   MOVEL(P)  'C'           ANTYPE
     C                   MOVEL     *BLANKS       ANTYCD
     C                   Z-ADD     COUNTA        ANBCNT
     C     XXCHRG        DIV(H)    COUNTA        ANAAMT
     C                   Z-ADD     XXCHRG        ANTAMT
     C*
     C                   WRITE     PAN01
     C*
     C* WRITE MRA RECORD
     C*
     C                   CLEAR                   PAN01
     C*
     C                   MOVE      @@CO#         ANCLT#
     C                   Z-ADD     XXFRDT        ANFRDT
     C                   Z-ADD     XXTODT        ANTODT
     C                   MOVEL(P)  HDAST         ANAST
     C                   MOVEL(P)  HDBTYP        ANBTYP
     C                   MOVEL(P)  'M'           ANTYPE
     C                   MOVEL     *BLANKS       ANTYCD
     C                   Z-ADD     COUNTA        ANBCNT
     C     XXMRA         DIV(H)    COUNTA        ANAAMT
     C                   Z-ADD     XXMRA         ANTAMT
     C*
     C                   WRITE     PAN01
     C*
     C* WRITE PAID RECORD
     C*
     C                   CLEAR                   PAN01
     C*
     C                   MOVE      @@CO#         ANCLT#
     C                   Z-ADD     XXFRDT        ANFRDT
     C                   Z-ADD     XXTODT        ANTODT
     C                   MOVEL(P)  HDAST         ANAST
     C                   MOVEL(P)  HDBTYP        ANBTYP
     C                   MOVEL(P)  'P'           ANTYPE
     C                   MOVEL     *BLANKS       ANTYCD
     C                   Z-ADD     COUNTA        ANBCNT
     C     XXPAID        DIV(H)    COUNTA        ANAAMT
     C                   Z-ADD     XXPAID        ANTAMT
     C*
     C                   WRITE     PAN01
     C*
     C* WRITE OTHER 1  RECORDS
     C*
     C     1             DO        999           X                 3 0
     C     O1C(X)        IFNE      *BLANKS
     C                   CLEAR                   PAN01
     C*
     C                   MOVE      @@CO#         ANCLT#
     C                   Z-ADD     XXFRDT        ANFRDT
     C                   Z-ADD     XXTODT        ANTODT
     C                   MOVEL(P)  HDAST         ANAST
     C                   MOVEL(P)  HDBTYP        ANBTYP
     C                   MOVEL(P)  'O1'          ANTYPE
     C                   MOVEL(P)  O1C(X)        ANTYCD
     C                   Z-ADD     COUNTA        ANBCNT
     C     O1A(X)        DIV(H)    COUNTA        ANAAMT
     C                   Z-ADD     O1A(X)        ANTAMT
     C*
     C                   WRITE     PAN01
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C* WRITE OTHER 2  RECORDS
     C*
     C     1             DO        999           X
     C     O2C(X)        IFNE      *BLANKS
     C                   CLEAR                   PAN01
     C*
     C                   MOVE      @@CO#         ANCLT#
     C                   Z-ADD     XXFRDT        ANFRDT
     C                   Z-ADD     XXTODT        ANTODT
     C                   MOVEL(P)  HDAST         ANAST
     C                   MOVEL(P)  HDBTYP        ANBTYP
     C                   MOVEL(P)  'O2'          ANTYPE
     C                   MOVEL(P)  O2C(X)        ANTYCD
     C                   Z-ADD     COUNTA        ANBCNT
     C     O2A(X)        DIV(H)    COUNTA        ANAAMT
     C                   Z-ADD     O2A(X)        ANTAMT
     C*
     C                   WRITE     PAN01
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C* WRITE OTHER 1  RECORDS
     C*
     C     1             DO        999           X
     C     NTC(X)        IFNE      *BLANKS
     C                   CLEAR                   PAN01
     C*
     C                   MOVE      @@CO#         ANCLT#
     C                   Z-ADD     XXFRDT        ANFRDT
     C                   Z-ADD     XXTODT        ANTODT
     C                   MOVEL(P)  HDAST         ANAST
     C                   MOVEL(P)  HDBTYP        ANBTYP
     C                   MOVEL(P)  'N'           ANTYPE
     C                   MOVEL(P)  NTC(X)        ANTYCD
     C                   Z-ADD     COUNTA        ANBCNT
     C     NTA(X)        DIV(H)    COUNTA        ANAAMT
     C                   Z-ADD     NTA(X)        ANTAMT
     C*
     C                   WRITE     PAN01
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C                   ENDSR                                                  WRTAMT
     C*================================================================
     C* WRTCNT - Write Count  Records
     C*================================================================
     CSR   WRTCNT        BEGSR
     C*
     C                   CLEAR                   PAN01
     C*
     C                   MOVE      @@CO#         ANCLT#
     C                   Z-ADD     XXFRDT        ANFRDT
     C                   Z-ADD     XXTODT        ANTODT
     C                   MOVEL(P)  *BLANKS       ANAST
     C                   MOVEL(P)  *BLANKS       ANBTYP
     C                   MOVEL(P)  'B'           ANTYPE
     C                   MOVEL     *BLANKS       ANTYCD
     C                   Z-ADD     COUNTT        ANBCNT
     C                   Z-ADD(H)  *ZEROS        ANAAMT
     C                   Z-ADD     *ZEROS        ANTAMT
     C*
     C                   WRITE     PAN01
     C*
     C                   ENDSR                                                  WRTCNT
