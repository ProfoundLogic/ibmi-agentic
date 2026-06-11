     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CVTHCPOS-CONVERT HCPOS AND HC10A1
     F*
     FFDCALF    IF   E           K DISK
     FFTABL     IF   E           K DISK
     FFDHCP     UF   E             DISK
     FFMPDL     IF   E           K DISK

     D*
     D* HC10A1 DATA STRUCTURE
     D                 DS
     D  XX10A1                 1     10
     D  X3SPLT                 1      3
     D  X510A1                 5      6
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C*  DETAIL CHARGE FILE KEY LIST
     C*
     C     CAKY          KLIST
     C                   KFLD                    HCCO#
     C                   KFLD                    HCSUB#
     C                   KFLD                    HCFND
     C                   KFLD                    HCFYR
     C                   KFLD                    HCCASE
     C                   KFLD                    HCDSEQ
     C*
     C*  PROVIDER MASTER FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C*
     C*  PLACE OF SERVICE TABLE FILE KEY LIST
     C*
     C     PSKY          KLIST
     C                   KFLD                    HCCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  READ THE DETAIL CHARGE FILE
     C*
     C                   READ      PHC01                                  50
     C     *IN50         DOWEQ     '0'
     C*
     C     HCPGM         IFNE      'CVTHCPOS'
     C*
     C*  GET THE DETAIL CHARGE RECORD
     C*
     C     CAKY          CHAIN     PCA01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  GET THE PLACE OF SERVICE TABLE FILE RECORD
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     CAIPOP        XXKEY
     C     PSKY          CHAIN     PPS01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  GET THE PROVDER SPECIALTY CODE
     C*
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      PSPOS         HCPOS
     C                   MOVEL     PDSPCD        X3SPLT
     C                   MOVE      'FS'          X510A1
     C                   MOVE      XX10A1        HC10A1
     C                   MOVE      *BLANKS       HCPGM
     C                   MOVEL     'CVTHCPOS'    HCPGM
     C                   UPDATE    PHC01
     C                   END
     C                   END
     C*
     C                   END
     C                   END
     C*
     C                   READ      PHC01                                  50
     C                   END
     C*
     C                   MOVE      '1'           *INLR
