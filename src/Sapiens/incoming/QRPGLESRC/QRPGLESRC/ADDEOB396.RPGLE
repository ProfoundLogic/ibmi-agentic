     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE ADDEOB396 - ADD EOB CODE 396
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM ADDS EOB CODE 396                 *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    MCPOSTCV                                                   *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 07/22/02  PROGRAMMER: C:62  PROJECT: NW074          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* INDICATORS:                                                   *
     H*   SCR01:                                                      *
     H*   99  READ  - STSTL1                                          *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FSTSTL     IF   E             DISK
     FFTABL     IF A E           K DISK    USROPN

     D*
     D QCA             S             56    DIM(2) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS                  INZ
     D  XXCMD                  1     56
     D  XXST                  38     39
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D*****************************************************************
     D* Constants
     D*****************************************************************
     D DESC            C                   CONST('DISCOUNT OFF BILLED -
     D                                     CHARGES HAS BEEN -
     D                                     APPLIED')

     C*****************************************************************
     C* Parameter lists
     C*****************************************************************
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
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*****************************************************************
     C* Key lists.
     C*****************************************************************
     C     TBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C*  LIKE DEFINES
     C*
     C     *LIKE         DEFINE    COCO#         KKCO#
     C     *LIKE         DEFINE    STKEY         KKKEY
     C*
     C                   MOVEL(P)  '396'         KKKEY
     C*
     C                   READ      PST01                                  99
     C     *IN99         DOWEQ     '0'
     C*
     C     STKEY         IFNE      'CC'
     C     STKEY         ANDNE     'CR'
     C*
     C                   EXSR      OVRABL
     C                   EXSR      OPNABL
     C*
     C     *LOVAL        SETLL     PCO01
     C                   READ      PCO01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     COCO#         KKCO#
     C*
     C     TBKY          CHAIN     PEB01                              99
     C     *IN99         IFEQ      *ON
     C                   CLEAR                   PEB01
     C                   Z-ADD     COCO#         EBCO#
     C                   MOVEL(P)  KKKEY         EBKEY
     C                   MOVEL(P)  DESC          EBDESC
     C     STKEY         IFEQ      'KY'
     C                   MOVEL(P)  'CO'          EBXREF
     C                   MOVEL(P)  'A2'          EBDSC2
     C                   ENDIF
     C                   MOVEL(P)  'A'           EBSTAT
     C                   MOVEL(P)  Q1PGM         EBPGM
     C                   MOVEL(P)  Q1USER        EBUSER
     C                   MOVEL(P)  Q1USER        EBIUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EBDT
     C                   MOVEL     SRDTO         EBIDT
     C                   Z-ADD     DSTIME        EBTIME
     C                   Z-ADD     DSTIME        EBTIME
     C                   WRITE     PEB01
     C                   ENDIF
     C*
     C                   READ      PCO01                                  99
     C                   ENDDO
     C*
     C                   EXSR      CLSABL
     C                   EXSR      DOVABL
     C*
     C                   ENDIF
     C*
     C                   READ      PST01                                  99
     C                   ENDDO
     C*
     C*
     C                   SETON                                        LR
     C*****************************************************************
     C* CLSABL - CLOSE FTABL
     C*****************************************************************
     C     CLSABL        BEGSR
     C*
     C                   CLOSE     FTABL
     C*
     C                   ENDSR
     C*****************************************************************
     C* DOVABL - DELETES OVERRIDE OF FTABL
     C*****************************************************************
     C     DOVABL        BEGSR
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*
     C                   ENDSR
     C*****************************************************************
     C* OPNABL - OPEN FTABL
     C*****************************************************************
     C     OPNABL        BEGSR
     C*
     C                   OPEN      FTABL
     C*
     C                   ENDSR
     C*****************************************************************
     C* OVRABL - OVERRIDES FTABL TO STATE FILE
     C*****************************************************************
     C     OVRABL        BEGSR
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(2)        XXCMD
     C                   MOVEL     STKEY         XXST
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    XXCMD
     C                   PARM                    LENGTH
     C*
     C                   ENDSR
**  QCMDEXC TABLE
DLTOVR FILE(FTABL)
OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABL  )
