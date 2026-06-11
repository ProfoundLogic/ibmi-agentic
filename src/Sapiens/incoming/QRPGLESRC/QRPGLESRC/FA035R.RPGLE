     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* API - FA035R - Get the number of detail records for an EOMB.
     H*
     H*****************************************************************
     H* Created on 05/27/2005 by C:53 for Project 129WKU
     H*****************************************************************
     H*                                                               *
     H*    CALLED FROM: MULTIPLE PGMS                                 *
     H*    CALLS TO: NONE                                             *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  00/00/00 C:00 PROJECT XXXXXX                                 *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*****************************************************************
     FFDCALL    IF   E           K DISK
     FFDDEL     IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#
     C                   PARM                    @@SUB#
     C                   PARM                    @@FND
     C                   PARM                    @@FYR
     C                   PARM                    @@CASE
     C                   PARM                    @@FEIN
     C                   PARM                    @@MOD
     C                   PARM                    @@EOB#
     C                   PARM                    @@#DTL
     C*
     C     CAKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@SUB#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
     C                   KFLD                    @@FEIN
     C                   KFLD                    @@MOD
     C                   KFLD                    @@EOB#
     C*
     C     DEKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@SUB#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
     C                   KFLD                    @@EOB#
     C*
     C     *LIKE         DEFINE    CACO#         @@CO#
     C     *LIKE         DEFINE    CASUB#        @@SUB#
     C     *LIKE         DEFINE    CAFND         @@FND
     C     *LIKE         DEFINE    CAFYR         @@FYR
     C     *LIKE         DEFINE    CACASE        @@CASE
     C     *LIKE         DEFINE    CAEOB#        @@EOB#
     C     *LIKE         DEFINE    CADSEQ        @@#DTL
     C     *LIKE         DEFINE    CAFEIN        @@FEIN
     C     *LIKE         DEFINE    CAMOD         @@MOD
     C*
     C     @@SUB#        IFEQ      *ZEROS
     C                   MOVE      @@FND         ##WCF#
     C                   MOVE      @@CO#         ##CO#
     C*
     C                   CALL      'FA700R'
     C                   PARM                    ##RTCD            1
     C                   PARM                    ##CO#             3
     C                   PARM                    ##WCF#            3
     C                   PARM                    ##SUB#            5
     C                   PARM                    ##FND             3
     C*
     C                   MOVE      ##FND         @@FND
     C                   MOVE      ##SUB#        @@SUB#
     C                   ENDIF
     C*
     C                   Z-ADD     *ZEROS        @@#DTL
     C*
     C     CAKY          CHAIN     PCA01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   ADD       1             @@#DTL
     C*
     C     CAKY          READE     PCA01                                  99
     C                   ENDDO
     C*
     C*
     C     DEKY          CHAIN     PDE01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   ADD       1             @@#DTL
     C     DENOT2        IFNE      *BLANKS
     C                   ADD       1             @@#DTL
     C                   ENDIF
     C*
     C     DEKY          READE     PDE01                                  99
     C                   ENDDO
     C*
     C                   RETURN
