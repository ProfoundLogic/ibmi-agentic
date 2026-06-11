     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  CVTBPRD - POPULATE DHBPRD                     C:62 04/27/04  *
     H*                                                FS279          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FFAEOP     IF   E             DISK
     FFDDHL     UF   E           K DISK
     FFDDDL     UF   E           K DISK
     FFMPRL     IF   E           K DISK    USROPN

     D*
     D QCA             S             56    DIM(2) CTDATA PERRCD(1)
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  QQCMD                  1     56
     D  QQST                  38     39

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DHKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     DDKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     PRKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    DHBPRC
     C                   KFLD                    KKEFDT
     C*
     C     PRKY1         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    DHBPRC
     C*
     C     *LIKE         DEFINE    PREFDT        KKEFDT
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C                   MOVEL     *BLANKS       HDST              2
     C*
     C                   READ      PEO01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     EOBTYP        IFEQ      'N'
     C*
     C     DHKY          CHAIN     PDH01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     DHBPRC        IFNE      DHPRCD
     C                   EXSR      OVERID
     C                   EXSR      UPDAT1
     C                   EXSR      DLTOVR
     C*
     C                   ELSE
     C                   EXSR      UPDAT2
     C*
     C                   ENDIF
     C*
     C     DHKY          READE     PDH01                                  99
     C*
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C     EOBTYP        IFEQ      'D'
     C*
     C     DDKY          CHAIN     PDD01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     DDBPRC        IFNE      DDPRCD
     C*
     C                   EXSR      OVERID
     C                   EXSR      UPDAT1
     C                   EXSR      DLTOVR
     C*
     C                   ELSE
     C                   EXSR      UPDAT2
     C*
     C                   ENDIF
     C*
     C     DDKY          READE     PDD01                                  99
     C*
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C                   READ      PEO01                                  99
     C*
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C*
     C*================================================================
     C* DLTOVR - Deletes file override
     C*================================================================
     C     DLTOVR        BEGSR
     C*
     C                   CLOSE     FMPRL
     C*
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM      56            LENGTH           15 5
     C*
     C                   ENDSR
     C*================================================================
     C* OVERID - Overrides file
     C*================================================================
     C     OVERID        BEGSR
     C*
     C                   MOVEL(P)  QCA(1)        QQCMD
     C                   MOVEL(P)  EOAST         QQST
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM      56            LENGTH
     C*
     C                   OPEN      FMPRL
     C*
     C                   ENDSR
     C*================================================================
     C* UPDAT1 - Update DHBPRD
     C*================================================================
     C     UPDAT1        BEGSR
     C*
     C                   SELECT
     C*
     C     EOBTYP        WHENEQ    'N'
     C*
     C                   CLEAR                   DHBPRD
     C     DHBPRC        IFEQ      *BLANKS
     C                   MOVE      DHPRCD        DHBPRC
     C                   ENDIF
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     PRDSC1        DHBPRD
     C                   ENDIF
     C                   UPDATE    PDH01
     C*
     C     EOBTYP        WHENEQ    'D'
     C*
     C                   CLEAR                   DDBPRD
     C     DDBPRC        IFEQ      *BLANKS
     C                   MOVE      DDPRCD        DDBPRC
     C                   ENDIF
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     PRDSC1        DDBPRD
     C                   ENDIF
     C                   UPDATE    PDD01
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* UPDAT2 - Update DHBPRD
     C*================================================================
     C     UPDAT2        BEGSR
     C*
     C                   SELECT
     C*
     C     EOBTYP        WHENEQ    'N'
     C                   MOVEL     DHPRDS        DHBPRD
     C                   UPDATE    PDH01
     C*
     C     EOBTYP        WHENEQ    'D'
     C                   MOVEL     DDPRDS        DDBPRD
     C                   UPDATE    PDD01
     C*
     C                   ENDSL
     C*
     C                   ENDSR
**  (QCMDEX)
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
DLTOVR FILE(FMPRL)
