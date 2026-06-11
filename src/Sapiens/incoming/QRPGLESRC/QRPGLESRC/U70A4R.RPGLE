     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE U70A4R - INDIVIDUAL EDIT PROGRAM FOR EDIT TYPE A4
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    INDIVIDUAL EDIT PROGRAM FOR EDIT TYPE A4                   *
     H*    NO MORE THAN X NUMBER OF OCCUR IN A 7-DAY PERIOD PRIOR TO  *
     H*    X NUMBER OF WEEKS                                          *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*   U7023R                                                      *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*   NONE                                                        *
     H*                                                               *
     H*  PROGRAMMER: C:20               PROJECT: UR007                *
     H*  CREATED: 01/26/94                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*  50        READ/DOWEQ PCA01                                   *
     H*  60        GENERAL CHAIN                                      *
     H*****************************************************************
     FUMCRL     IF   E           K DISK
     FFDCAL3    IF   E           K DISK
     FFDCALL    IF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     FFMPDL     IF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     IPCA02
     I              CACPT4                      X2CPT4
     I              CADSEQ                      X2DSEQ
     I              CAFDAT                      X2FDAT
     I              CAEOB#                      X2EOB#

     C*
     C*   KEY LIST FOR THE C/R REVIEW FILE
     C*
     C     CRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEDIT
     C*
     C*   KEY LIST FOR THE DETAIL CHARGE FILE
     C*
     C     CAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     CAKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE PROVIDER FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQEDIT            5
     C                   PARM                    QQEXT             3
     C                   PARM                    QQDSEQ            5
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
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
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C* INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    CREDIT        XXEDIT
     C     *LIKE         DEFINE    CREXT         XXEXT
     C     *LIKE         DEFINE    CROCCR        XXOCCR
     C     *LIKE         DEFINE    CRWKST        XXWKST
     C     *LIKE         DEFINE    CACASE        XXCASE
     C     *LIKE         DEFINE    CASUB#        XXSUB#
     C     *LIKE         DEFINE    CAFND         XXFND
     C     *LIKE         DEFINE    CAFYR         XXFYR
     C     *LIKE         DEFINE    CACO#         XXCO#
     C     *LIKE         DEFINE    PDFEIN        XXFEIN
     C     *LIKE         DEFINE    PDMOD         XXMOD
     C     *LIKE         DEFINE    PDDT          XXDATE
     C     *LIKE         DEFINE    PDDT          X1DATE
     C*
     C                   MOVE      *BLANKS       XXRTCD            1
     C                   MOVE      *BLANKS       XXFFLG            1
     C                   Z-ADD     *ZEROS        XXOCCR
     C                   Z-ADD     *ZEROS        XXWKST
     C*
     C* MOVE ENTRY PARMS TO WORK FIELDS
     C*
     C                   MOVE      QQEDIT        XXEDIT
     C                   MOVE      QQEXT         XXEXT
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C*
     C* READ DETAIL CHARGE FILE/READ C/R REVIEW FILE
     C*
     C     CAKEY         CHAIN     PCA01                              50
     C*
     C* FIND THE NUMBER OF WEEKS THE TREATMENT IS ALLOWED
     C*
     C     *IN50         IFEQ      '0'
     C*
     C     CRKEY         CHAIN     PCR01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     CRWKST        XXWKST
     C                   END
     C     XXWKST        MULT      7             XXDAYT            5 0
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C*
     C                   Z-ADD     XXDAYT        SRFUTR            5 0
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD            1
     C*
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   END                                                    END IF 50
     C*
     C     *IN50         DOWEQ     '0'
     C     XXRTCD        ANDNE     'N'
     C*
     C* FIND THE SEVEN DAY PERIOD THAT X NO OF OCCURENCES HAVE TO TAKE PLACE IN
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE
     C*
     C                   Z-ADD     7             SRFUTR            5 0
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD            1
     C*
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1DATE
     C*
     C                   Z-ADD     *ZEROS        XXOCCR
     C     CAPRTY        IFEQ      '1'
     C     CRKEY         CHAIN     PCR01                              60
     C     *IN60         DOWEQ     '0'
     C     XXRTCD        ANDNE     'N'
     C                   Z-ADD     CREXT         XXEXT
     C*
     C     CRCPT1        IFEQ      *BLANKS
     C     CRCPT2        ANDEQ     *BLANKS
     C     CRCPT3        ANDEQ     *BLANKS
     C     CRCPT4        ANDEQ     *BLANKS
     C     CRCPT5        ANDEQ     *BLANKS
     C     CRCPT6        ANDEQ     *BLANKS
     C     CRCPT7        ANDEQ     *BLANKS
     C     CRCPT8        ANDEQ     *BLANKS
     C*
     C     CRSPCD        IFNE      *BLANKS
     C                   Z-ADD     CAFEIN        XXFEIN
     C                   Z-ADD     CAMOD         XXMOD
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C     PDSPCD        IFNE      CRSPCD
     C                   GOTO      NXTCA
     C                   END
     C                   END
     C                   END
     C*
     C     CAKEY1        CHAIN     PCA02                              40
     C     *IN40         DOWEQ     '0'
     C     X2FDAT        IFLE      XXDATE
     C     X2EOB#        IFNE      *ZEROS
     C     X2CPT4        IFEQ      CACPT4
     C     X2FDAT        IFGE      CAFDAT
     C     X2FDAT        ANDLE     X1DATE
     C*
     C                   ADD       1             XXOCCR
     C     XXOCCR        IFGT      CROCCR
     C                   MOVE      'N'           XXRTCD
     C                   GOTO      TERM
     C                   ELSE
     C                   MOVE      'Y'           XXRTCD
     C                   END                                                    END XXOCCR
     C*
     C                   END                                                    END X2FDAT
     C                   END                                                    END X2CPT4
     C                   END                                                    END X2EOB#
     C                   ELSE                                                   ELSE XXDATE
     C                   MOVE      'N'           XXRTCD
     C                   GOTO      TERM
     C                   END                                                    END XXDATE
     C     CAKEY1        READE     PCA02                                  40
     C                   END                                                    END DO *IN40
     C*
     C                   ELSE
     C*
     C     CACPT4        IFEQ      CRCPT1
     C     CACPT4        OREQ      CRCPT2
     C     CACPT4        OREQ      CRCPT3
     C     CACPT4        OREQ      CRCPT4
     C     CACPT4        OREQ      CRCPT5
     C     CACPT4        OREQ      CRCPT6
     C     CACPT4        OREQ      CRCPT7
     C     CACPT4        OREQ      CRCPT8
     C*
     C     CRSPCD        IFNE      *BLANKS
     C                   Z-ADD     CAFEIN        XXFEIN
     C                   Z-ADD     CAMOD         XXMOD
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C     PDSPCD        IFNE      CRSPCD
     C                   GOTO      NXTCA
     C                   END
     C                   END
     C                   END
     C*
     C     CAKEY1        CHAIN     PCA02                              40
     C     *IN40         DOWEQ     '0'
     C     X2FDAT        IFLE      XXDATE
     C     X2EOB#        IFNE      *ZEROS
     C     X2CPT4        IFEQ      CACPT4
     C     X2FDAT        IFGE      CAFDAT
     C     X2FDAT        ANDLE     X1DATE
     C*
     C                   ADD       1             XXOCCR
     C     XXOCCR        IFGT      CROCCR
     C                   MOVE      'N'           XXRTCD
     C                   GOTO      TERM
     C                   ELSE
     C                   MOVE      'Y'           XXRTCD
     C                   END                                                    END XXOCCR
     C*
     C                   END                                                    END X2FDAT
     C                   END                                                    END X2CPT4
     C                   END                                                    END X2EOB#
     C                   ELSE                                                   ELSE XXDATE
     C                   MOVE      'N'           XXRTCD
     C                   GOTO      TERM
     C                   END                                                    END XXDATE
     C     CAKEY1        READE     PCA02                                  40
     C                   END                                                    END DO *IN40
     C*
     C                   END                                                    END CPT4
     C                   END                                                    END CPT4
     C     CRKEY         READE     PCR01                                  60
     C                   END                                                    END *IN60
     C                   END                                                    END CAPRTY
     C     XXRTCD        CABEQ     'N'           TERM
     C     NXTCA         TAG
     C     CAKEY         READE     PCA01                                  50
     C                   END                                                    END *IN50
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      CADSEQ        QQDSEQ
     C                   MOVE      XXEXT         QQEXT
     C                   MOVE      XXRTCD        QQRTCD
     C                   RETURN
     C*
