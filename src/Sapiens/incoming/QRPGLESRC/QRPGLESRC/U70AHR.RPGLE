     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE U70AHR - INDIVIDUAL EDIT PROGRAM FOR EDIT TYPE AH
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    INDIVIDUAL EDIT PROGRAM FOR EDIT TYPE AH                   *
     H*    NO MORE THAN $X PAID IN MEDICAL PER DAY                    *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*   U7023R                                                      *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*   NONE                                                        *
     H*                                                               *
     H*  PROGRAMMER: KEN RUTHVEN        PROJECT: 99AAF                *
     H*  CREATED: 12/23/92                                            *
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
     D DAY             S              7  0 DIM(999)                             SRV DATE ARRAY
     D CHRG            S              9  2 DIM(999)                             AMOUNT CHARGED ARRAY
     D AMT             S              9  2 DIM(999) ASCEND                      AMOUNT CHARGED ARRAY
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
     C     *LIKE         DEFINE    CACASE        XXCASE                         7.0N
     C     *LIKE         DEFINE    CASUB#        XXSUB#                         5.0N
     C     *LIKE         DEFINE    CAFND         XXFND                          3.0N
     C     *LIKE         DEFINE    CAFYR         XXFYR                          3.0N
     C     *LIKE         DEFINE    CACO#         XXCO#                          3.0N
     C     *LIKE         DEFINE    PDFEIN        XXFEIN                         9.0N
     C     *LIKE         DEFINE    PDMOD         XXMOD                          3.0N
     C                   MOVE      *BLANKS       XXRTCD            1            1A
     C                   Z-ADD     *ZEROS        AMT
     C                   Z-ADD     *ZEROS        CHRG
     C                   Z-ADD     *ZEROS        DAY
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
     C     *IN50         DOWEQ     '0'
     C     XXRTCD        ANDNE     'N'
     C     CAPRTY        IFEQ      '1'
     C     CRKEY         CHAIN     PCR01                              60
     C     *IN60         IFEQ      '0'
     C     XXRTCD        ANDNE     'N'
     C*
     C* IF THE SPEC CODE IS NOT BLANKS THEN LOOK AT ONLY PROVIDERS
     C* WITH SAME SPECIALTY CODE
     C*
     C     CRSPCD        IFNE      *BLANKS
     C                   Z-ADD     CAFEIN        XXFEIN
     C                   Z-ADD     CAMOD         XXMOD
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C     PDSPCD        IFNE      CRSPCD
     C                   GOTO      NXTRCD
     C                   END                                                    END PCSPCD
     C                   END                                                    END *IN60
     C                   END                                                    END CRSPCD
     C*
     C* ADD UP THE TOTAL CHARGE FOR THE PROVIDER ON A PARTICULAR DAY
     C*
     C                   Z-ADD     1             Q                 3 0
     C     CAFDAT        LOOKUP    DAY(Q)                                 45
     C     *IN45         IFEQ      '1'
     C                   ADD       CATOTP        CHRG(Q)
     C                   ELSE
     C                   Z-ADD     1             Q                 3 0
     C     *ZEROS        LOOKUP    DAY(Q)                                 45
     C                   Z-ADD     CAFDAT        DAY(Q)
     C                   Z-ADD     CATOTP        CHRG(Q)
     C                   END
     C*
     C* MOVE TOTAL CHARGED ARRAY INTO AN ASCENDING ARRAY FOR COMPARISIO
     C*
     C                   MOVEA     CHRG          AMT
     C                   Z-ADD     1             Q                 3 0
     C     CRAMT1        LOOKUP    AMT(Q)                             70
     C     *IN70         IFEQ      '1'
     C                   MOVE      'N'           XXRTCD
     C                   ELSE
     C                   MOVE      'Y'           XXRTCD
     C                   END
     C*
     C                   END                                                    END *IN60
     C                   END                                                    END CAPRTY
     C     XXRTCD        CABEQ     'N'           TERM
     C     NXTRCD        TAG
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
