     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *    PROGRAM: CVTUDPOP
      * ***************************************************************
      *  PROGRAMMER: 47                 PROJECT: 87MCB                *
      *  CREATED: 03/03/98                                            *
      *  THIS PROGRAM LOADS UDPOP FROM UMPCP.                         *
      * ***************************************************************
      *
      * Pre-Cert Master file
     FUMPCL11   IF   E           K DISK
      * Reasonable & Customary file
     FFMRCL1    IF   E           K DISK
      * Case Master file
     FFMCML1    IF   E           K DISK
      * State Master file
     FSTSTL     IF   E           K DISK
      * Provider Auxiliary file
     FFMP1L     IF   E           K DISK
      * Procedure Code file
     FFMPRL     IF   E           K DISK    USROPN
      * New Procedure Code file
     FUDPOP     O    E             DISK
      *

     D QCA             S             56    DIM(2) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  @@CMD                  1     56
     D  @@ST                  38     39
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     CMKY          KLIST
     C                   KFLD                    PCCO#
     C                   KFLD                    PCSUB#
     C                   KFLD                    PCFND
     C                   KFLD                    PCFYR
     C                   KFLD                    PCCASE
     C*
     C*  PROCEDURE CODE FILE
     C*
     C     PRKY          KLIST
     C                   KFLD                    PCCO#
     C                   KFLD                    XXCPCD
      *
     C     P1KY          KLIST
     C                   KFLD                    PCFEIN
     C                   KFLD                    PCMOD
      *
     C     STKY          KLIST
     C                   KFLD                    XXAST            10
     C*
     C     *LIKE         DEFINE    PCDT          XXDT
     C     *LIKE         DEFINE    CMAST         XXPRST
     C     *LIKE         DEFINE    PCCPT1        XXCPCD
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
      *
      * -----------------------------------------------------------------
      *
     C     *INLR         DOUEQ     *ON
     C                   READ      PPC01                                  LR
     C     *INLR         IFEQ      *OFF
     C                   Z-ADD     PCCO#         POCO#
     C                   Z-ADD     PCSUB#        POSUB#
     C                   Z-ADD     PCFND         POFND
     C                   Z-ADD     PCFYR         POFYR
     C                   Z-ADD     PCCASE        POCASE
     C                   MOVEL(P)  PCCERT        POCERT
     C                   Z-ADD     PCEXT         POEXT
     C                   Z-ADD     *ZERO         POSEQ#
     C                   MOVE      *BLANK        POALLW
     C                   MOVE      *BLANK        POAUTH
     C                   MOVE      *BLANK        POEOB
      *
     C     PCCPT1        IFNE      *BLANK
     C                   MOVEL(P)  PCCPT1        XXCPCD
     C                   EXSR      SRPROC
      *
     C                   MOVEL(P)  PCTOS1        POTOS
     C                   Z-ADD     PCA#V1        POVST
     C                   Z-ADD     PCSPD1        POFRDT
     C                   Z-ADD     PCEPD1        POTODT
     C*
     C                   EXSR      SRMNT
     C                   WRITE     PPO01
     C                   ENDIF                                                  PCCPT  NE BLANK
      *
     C     PCCPT2        IFNE      *BLANK
     C                   MOVEL(P)  PCCPT2        XXCPCD
     C                   EXSR      SRPROC
      *
     C                   MOVEL(P)  PCTOS2        POTOS
     C                   Z-ADD     PCA#V2        POVST
     C                   Z-ADD     PCSPD2        POFRDT
     C                   Z-ADD     PCEPD2        POTODT
     C*
     C                   EXSR      SRMNT
     C                   WRITE     PPO01
     C                   ENDIF                                                  PCCPT  NE BLANK
      *
     C     PCCPT3        IFNE      *BLANK
     C                   MOVEL(P)  PCCPT3        XXCPCD
     C                   EXSR      SRPROC
      *
     C                   MOVEL(P)  PCTOS3        POTOS
     C                   Z-ADD     PCA#V3        POVST
     C                   Z-ADD     PCSPD3        POFRDT
     C                   Z-ADD     PCEPD3        POTODT
     C*
     C                   EXSR      SRMNT
     C                   WRITE     PPO01
     C                   ENDIF                                                  PCCPT  NE BLANK
      *
     C     PCCPT4        IFNE      *BLANK
     C                   MOVEL(P)  PCCPT4        XXCPCD
     C                   EXSR      SRPROC
      *
     C                   MOVEL(P)  PCTOS4        POTOS
     C                   Z-ADD     PCA#V4        POVST
     C                   Z-ADD     PCSPD4        POFRDT
     C                   Z-ADD     PCEPD4        POTODT
     C*
     C                   EXSR      SRMNT
     C                   WRITE     PPO01
     C                   ENDIF                                                  PCCPT  NE BLANK
      *
     C     PCCPT5        IFNE      *BLANK
     C                   MOVEL(P)  PCCPT5        XXCPCD
     C                   EXSR      SRPROC
      *
     C                   MOVEL(P)  PCTOS5        POTOS
     C                   Z-ADD     PCA#V5        POVST
     C                   Z-ADD     PCSPD5        POFRDT
     C                   Z-ADD     PCEPD5        POTODT
     C*
     C                   EXSR      SRMNT
     C                   WRITE     PPO01
     C                   ENDIF                                                  PCCPT  NE BLANK
      *
     C     PCCPT6        IFNE      *BLANK
     C                   MOVEL(P)  PCCPT6        XXCPCD
     C                   EXSR      SRPROC
      *
     C                   MOVEL(P)  PCTOS6        POTOS
     C                   Z-ADD     PCA#V6        POVST
     C                   Z-ADD     PCSPD6        POFRDT
     C                   Z-ADD     PCEPD6        POTODT
     C*
     C                   EXSR      SRMNT
     C                   WRITE     PPO01
     C                   ENDIF                                                  PCCPT  NE BLANK
      *
     C     PCCPT7        IFNE      *BLANK
     C                   MOVEL(P)  PCCPT7        XXCPCD
     C                   EXSR      SRPROC
      *
     C                   MOVEL(P)  PCTOS7        POTOS
     C                   Z-ADD     PCA#V7        POVST
     C                   Z-ADD     PCSPD7        POFRDT
     C                   Z-ADD     PCEPD7        POTODT
     C*
     C                   EXSR      SRMNT
     C                   WRITE     PPO01
     C                   ENDIF                                                  PCCPT  NE BLANK
      *
     C     PCCPT8        IFNE      *BLANK
     C                   MOVEL(P)  PCCPT8        XXCPCD
     C                   EXSR      SRPROC
      *
     C                   MOVEL(P)  PCTOS8        POTOS
     C                   Z-ADD     PCA#V8        POVST
     C                   Z-ADD     PCSPD8        POFRDT
     C                   Z-ADD     PCEPD8        POTODT
     C*
     C                   EXSR      SRMNT
     C                   WRITE     PPO01
     C                   ENDIF                                                  PCCPT  NE BLANK
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
      * =================================================================
     C     SRMNT         BEGSR
      *
     C     POSEQ#        IFEQ      *ZERO
     C                   Z-ADD     1             POSEQ#
     C                   ELSE
     C                   ADD       1             POSEQ#
     C                   ENDIF
      *
     C                   MOVEL(P)  'A'           POSTAT
     C                   MOVEL(P)  Q1USER        POIUSR
     C                   MOVEL(P)  Q1PGM         POIPGM
     C                   Z-ADD     XXDT          POIDT
     C                   Z-ADD     DSTIME        POITIM
     C*
     C                   MOVEL     Q1USER        POUSER
     C                   MOVEL     Q1PGM         POPGM
     C                   Z-ADD     XXDT          PODT
     C                   Z-ADD     DSTIME        POTIME
     C                   ENDSR
      *
      * =============================================================
     C     SRPROC        BEGSR
      *
      *  GET ADJUDICATION STATE
      *
     C     CMKY          CHAIN     FMCML1                             60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CMAST         XXAST
      *
     C     STKY          CHAIN     STSTL                              61
     C     *IN61         IFEQ      *OFF
     C     STINST        IFEQ      'Y'
     C                   MOVEL(P)  CMAST         XXPRST
     C                   END
      *
     C     STINST        IFEQ      *BLANK
     C     P1KY          CHAIN     FMP1L                              62
     C     *IN62         IFEQ      *OFF
     C                   MOVEL(P)  P1ST          XXPRST
     C                   END
     C                   END
      *
     C                   END
     C                   END
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*
     C*   CLOSE FILE IF OPENED
     C*
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FMPRL
     C                   MOVE      ' '           OPNFLG            1
     C                   END                                                    END OPNFLG=1
     C*
     C*   DELETE OVERRIDE
     C*
     C     OVRFLG        IFEQ      '1'
     C*
     C*   DELETE OVERRIDE TO FMPRL
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG            1
     C                   END                                                    END OVRFLG=1
     C*
     C     OVRFLG        IFEQ      ' '
     C*
     C*   OVERRIDE TO FMPRL
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(2)        @@CMD
     C                   MOVEL     XXPRST        @@ST
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG            1
     C                   END                                                    END OVRFLG=0
     C*
     C*   OPEN FILE FOR PROCESSING
     C*
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FMPRL
     C                   MOVE      '1'           OPNFLG
     C                   END                                                    END OPNFLG= ' '
      *
     C                   MOVEL(P)  XXCPCD        POPROC
      *
     C     PRKY          CHAIN     PPR01                              60
     C     *IN60         IFEQ      *OFF                                         FOUND
     C                   MOVEL(P)  PRDSC1        PODESC
     C                   END
      *
     C     *IN60         IFEQ      *ON                                          NOT FOUND
     C     PRKY          CHAIN     PRC01                              61
     C     *IN61         IFEQ      *OFF
     C     RCSTAT        ANDEQ     'A'
     C                   MOVEL(P)  RCDSC1        PODESC
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDSR
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
**  QCMDEXC TABLE
DLTOVR FILE(FMPRL)
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
