     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CV1HCPOS-CONVERT HCPOS AND HC10A1
     F*
     FFDCALTMP  IF   E           K DISK
     FFDHCL     UF   E           K DISK

     D*
     D* HC10A1 DATA STRUCTURE
     D                 DS
     D  XX10A1                 1     10
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
     C                   KFLD                    XXIPOP
     C*
     C*  HOSPITAL DETAIL CHARGE FILE KEY LIST
     C*
     C     HCKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CADSEQ
     C*
     C     *LIKE         DEFINE    CAIPOP        XXIPOP
     C                   MOVE      '0'           XXIPOP
     C*  READ THE DETAIL CHARGE FILE
     C*  PROCESS ONLY RECORDS WITH 0 IN CAIPOP
     C*
     C     CAKY          CHAIN     PCA01                              60
     C     *IN60         DOWEQ     '0'
     C     *IN60         IFEQ      '0'
     C*
     C*  GET THE DETAIL CHARGE AUXILIARY RECORD
     C*
     C     HCKY          CHAIN     PHC01                              60
     C     *IN60         DOWEQ     '0'
     C     *IN60         IFEQ      '0'
     C                   MOVE      '011'         HCPOS
     C                   MOVE      'FS'          X510A1
     C                   MOVE      XX10A1        HC10A1
     C                   MOVE      *BLANKS       HCPGM
     C                   MOVEL     'CV1HCPOS'    HCPGM
     C                   UPDATE    PHC01
     C                   END
     C     HCKY          READE     PHC01                                  60
     C                   END
     C*
     C                   END
     C*
     C     CAKY          READE     PCA01                                  60
     C                   END
     C*
     C                   MOVE      '1'           *INLR
