     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE DELCASE - DELETES CASES
     H*****************************************************************
     H* PROGRAM: DELCASE
     H* DESCRIPTION: DELETES CASES
     H*****************************************************************
     H** REVISIONS:
     H*
     H*****************************************************************
     FFMCML1    IF   E           K DISK
     F                                     INFDS(DBFDS)

     D DBFDS           DS
     D  DBRRN                397    400B 0

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#
     C                   PARM                    @@SUB#
     C                   PARM                    @@FND
     C                   PARM                    @@FYR
     C                   PARM                    @@CASE
     C                   PARM                    @@TYPE            1
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    CMSUB#        KKSUB#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    CMCO#         @@CO#
     C     *LIKE         DEFINE    CMSUB#        @@SUB#
     C     *LIKE         DEFINE    CMFND         @@FND
     C     *LIKE         DEFINE    CMFYR         @@FYR
     C     *LIKE         DEFINE    CMCASE        @@CASE
     C*
     C                   Z-ADD     @@CO#         KKCO#
     C                   Z-ADD     @@SUB#        KKSUB#
     C                   Z-ADD     @@FND         KKFND
     C                   Z-ADD     @@FYR         KKFYR
     C                   Z-ADD     @@CASE        KKCASE
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  'FAT3PQ'      #FNAME
     C                   MOVEL(P)  'FAT3PQ'      #TNAME
     C*
     C                   CALL      'FA710C'
     C                   PARM      ' '           ##RTN             1
     C                   PARM                    #FNAME           10
     C                   PARM                    #TNAME           10
     C*
     C                   MOVEL(P)  'FAT3LQ'      #FNAME
     C                   MOVEL(P)  'FAT3LQ'      #TNAME
     C*
     C                   CALL      'FA710C'
     C                   PARM      ' '           ##RTN             1
     C                   PARM                    #FNAME           10
     C                   PARM                    #TNAME           10
     C*
     C     @@TYPE        IFEQ      'D'
     C*
     C                   CALL      'F7090R'
     C                   PARM      ' '           ##CALL            1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      DBRRN         ##RRN4            9 0
     C                   PARM      *ZEROS        ##RRN5            9 0
     C                   PARM      *ZEROS        ##RRN6            9 0
     C                   PARM      *ZEROS        ##RRN7            9 0
     C                   PARM      'D'           ##TD              1
     C                   PARM      'X'           ##FLAG            1
     C                   PARM      'N'           ##WCMS            1
     C*
     C                   ELSE
     C*
     C                   CALL      'F7091R'
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      'C'           ##MODE            1
     C                   PARM      DBRRN         ##RRN             9 0
     C                   PARM      'T'           ##TD              1
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   SETON                                        LR
