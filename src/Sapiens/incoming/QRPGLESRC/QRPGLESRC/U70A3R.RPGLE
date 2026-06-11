     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE U70A3R - INDIVIDUAL EDIT PROGRAM FOR EDIT TYPE A3
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    INDIVIDUAL EDIT PROGRAM FOR EDIT TYPE A3                   *
     H*    NO MORE THAN X NUMBER OF OCCURENCES                        *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*   U7023R                                                      *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*   NONE                                                        *
     H*                                                               *
     H*  PROGRAMMER: KEN RUTHVEN        PROJECT: 99AAF                *
     H*  CREATED: 12/15/92                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*  50        READ/DOWEQ PCA01                                   *
     H*  60        GENERAL CHAIN                                      *
     H*****************************************************************
     FUMCRL     IF   E           K DISK
     FFDCAL3    IF   E           K DISK
     FFMPDL     IF   E           K DISK

     D*
     D CPT             S              5    DIM(999)                             CPT4 HLD ARRAY
     D OCC             S              3  0 DIM(999)                             OCC HLD ARRAY
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE C/R REVIEW FILE
     C*
     C     CRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEDIT
     C                   KFLD                    XXEXT
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
     C     *LIKE         DEFINE    CROCCR        XXOCCR                         3.0N
     C     *LIKE         DEFINE    CACASE        XXCASE                         7.0N
     C     *LIKE         DEFINE    CASUB#        XXSUB#                         5.0N
     C     *LIKE         DEFINE    CAFND         XXFND                          3.0N
     C     *LIKE         DEFINE    CAFYR         XXFYR                          3.0N
     C     *LIKE         DEFINE    CACO#         XXCO#                          3.0N
     C     *LIKE         DEFINE    PDFEIN        XXFEIN                         9.0N
     C     *LIKE         DEFINE    PDMOD         XXMOD                          3.0N
     C*
     C                   MOVE      *BLANKS       XXRTCD            1            1A
     C                   Z-ADD     *ZEROS        TOTOCC            3 0          3.0N
     C                   MOVE      *BLANKS       CPT
     C                   Z-ADD     *ZEROS        OCC
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
     C* CHAIN TO C/R REVIEW FILE TO GET # OF OCCURENCES
     C*
     C     CRKEY         CHAIN     PCR01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     CROCCR        XXOCCR
     C                   END
     C*
     C* READ DETAIL CHARGE FILE/READ C/R REVIEW FILE
     C*
     C     CAKEY         CHAIN     PCA01                              50
     C     *IN50         DOWEQ     '0'
     C     XXRTCD        CABEQ     'N'           TERM
     C*
     C     CRSPCD        IFNE      *BLANKS
     C                   Z-ADD     CAFEIN        XXFEIN
     C                   Z-ADD     CAMOD         XXMOD
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C     PDSPCD        IFNE      CRSPCD
     C                   GOTO      NXTCA
     C                   END                                                    END *IN60
     C                   END                                                    END PDSPCD
     C                   END                                                    END CRSPCD
     C                   Z-ADD     1             Q                 3 0
     C     CACPT4        LOOKUP    CPT(Q)                                 55
     C     *IN55         IFEQ      '1'
     C                   ADD       1             OCC(Q)
     C     OCC(Q)        IFGT      XXOCCR
     C                   MOVE      'N'           XXRTCD
     C                   ELSE                                                   ELSE OCC,Q
     C                   MOVE      'Y'           XXRTCD
     C                   END                                                    END OCC,Q
     C                   ELSE                                                   ELSE *IN55
     C                   Z-ADD     1             Q                 3 0
     C     *BLANKS       LOOKUP    CPT(Q)                                 55
     C     *IN55         IFEQ      '1'
     C                   MOVEL     CACPT4        CPT(Q)
     C                   ADD       1             OCC(Q)
     C     OCC(Q)        IFGT      XXOCCR
     C                   MOVE      'N'           XXRTCD
     C                   ELSE                                                   ELSE OCC,Q
     C                   MOVE      'Y'           XXRTCD
     C                   END                                                    END OCC,Q
     C                   END                                                    END *IN55
     C                   END                                                    END *IN55
     C*
     C     NXTCA         TAG
     C     CAKEY         READE     PCA01                                  50
     C                   END                                                    END DOW 50
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      CADSEQ        QQDSEQ
     C                   MOVE      XXEXT         QQEXT
     C                   MOVE      XXRTCD        QQRTCD
     C                   RETURN
