     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE U70ABR - INDIVIDUAL EDIT PROGRAM FOR EDIT TYPE AB
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    INDIVIDUAL EDIT PROGRAM FOR EDIT TYPE AB                   *
     H*    NO MORE THAN X NUMBER OF PROVIDER SPECIALTIES              *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*   U7023R                                                      *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*   NONE                                                        *
     H*                                                               *
     H*  PROGRAMMER: KEN RUTHVEN        PROJECT: 99AAF                *
     H*  CREATED: 12/22/92                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*  07/23/93  C:20      PROJ: URMNT                              *
     H*                      1. ADDED CODE TO EXIT OUT IF CNT GT      *
     H*                         CROCCR RIGHT AWAY RATHER THAN WHEN    *
     H*                         DONE PROCESSING DETAIL CHARGE FILE    *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*  50        READ/DOWEQ PCA01                                   *
     H*  60        GENERAL CHAIN                                      *
     H*****************************************************************
     FUMCRL     IF   E           K DISK
     FFDCAL3    IF   E           K DISK
     FFMPDL     IF   E           K DISK

     D*
     D CNT             S              3    DIM(50)                              SPEC CODE HLD ARRAY
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
     C* KEY LIST FOR THE PROVIDER FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
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
     C                   MOVE      *BLANKS       XXRTCD            1            1A
     C                   Z-ADD     *ZEROS        XXCNT             5 0          5.0N
     C                   MOVE      *BLANKS       CNT
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
     C     *IN60         DOWEQ     '0'
     C     XXRTCD        ANDNE     'N'
     C                   Z-ADD     CREXT         XXEXT
     C*
     C     PDKEY         CHAIN     PPD01                              45
     C     *IN45         IFEQ      '0'
     C     PDSPCD        ANDNE     *BLANKS
     C                   Z-ADD     1             Q                 3 0
     C     PDSPCD        LOOKUP    CNT(Q)                                 55
     C     *IN55         IFEQ      '0'
     C                   ADD       1             XXCNT
     C     XXCNT         IFGT      CROCCR
     C                   MOVE      'N'           XXRTCD
     C                   ELSE
     C                   MOVE      'Y'           XXRTCD
     C                   END                                                    END XXVSTC
     C                   Z-ADD     1             Q                 3 0
     C     *BLANKS       LOOKUP    CNT(Q)                                 55
     C                   MOVEL     PDSPCD        CNT(Q)
     C                   END                                                    END *IN55
     C                   END                                                    END *IN45
     C*
     C     CRKEY         READE     PCR01                                  60
     C                   END                                                    END *IN60
     C                   END                                                    END CAPRTY
     C     XXRTCD        CABEQ     'N'           TERM
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
