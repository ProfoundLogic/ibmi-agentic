     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE U70A5R - INDIVIDUAL EDIT PROGRAM FOR EDIT TYPE A5
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    INDIVIDUAL EDIT PROGRAM FOR EDIT TYPE A5                   *
     H*    NO MORE THAN X NUMBER OF VISITS PER LIFE OF CLAIM          *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*   U7023R                                                      *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*   NONE                                                        *
     H*                                                               *
     H*  PROGRAMMER: KEN RUTHVEN        PROJECT: 99AAF                *
     H*  CREATED: 12/16/92                                            *
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
     C* INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    CREDIT        XXEDIT                         5A
     C     *LIKE         DEFINE    CREXT         XXEXT                          3.0N
     C     *LIKE         DEFINE    CRVIST        XXVSTC                         3.0N
     C     *LIKE         DEFINE    CACASE        XXCASE                         7.0N
     C     *LIKE         DEFINE    CASUB#        XXSUB#                         5.0N
     C     *LIKE         DEFINE    CAFND         XXFND                          3.0N
     C     *LIKE         DEFINE    CAFYR         XXFYR                          3.0N
     C     *LIKE         DEFINE    CACO#         XXCO#                          3.0N
     C     *LIKE         DEFINE    PDFEIN        XXFEIN                         9.0N
     C     *LIKE         DEFINE    PDMOD         XXMOD                          3.0N
     C*
     C                   MOVE      *BLANKS       XXRTCD            1            1A
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
     C                   Z-ADD     *ZEROS        XXVSTC
     C*
     C* READ DETAIL CHARGE FILE/READ C/R REVIEW FILE
     C*
     C     CAKEY         CHAIN     PCA01                              50
     C     *IN50         DOWEQ     '0'
     C     XXRTCD        ANDNE     'N'
     C                   Z-ADD     *ZEROS        XXVSTC
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
     C     X2EOB#        IFNE      *ZEROS
     C     X2CPT4        IFEQ      CACPT4
     C     X2FDAT        IFNE      CAFDAT
     C                   ADD       1             XXVSTC
     C     XXVSTC        IFGT      CRVIST
     C                   MOVE      'N'           XXRTCD
     C                   ELSE
     C                   MOVE      'Y'           XXRTCD
     C                   END                                                    END XXVSTC
     C                   END
     C                   END
     C                   END
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
     C     X2EOB#        IFNE      *ZEROS
     C     X2CPT4        IFEQ      CACPT4
     C     X2FDAT        IFNE      CAFDAT
     C                   ADD       1             XXVSTC
     C     XXVSTC        IFGT      CRVIST
     C                   MOVE      'N'           XXRTCD
     C                   ELSE
     C                   MOVE      'Y'           XXRTCD
     C                   END                                                    END XXVSTC
     C                   END
     C                   END
     C                   END
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
