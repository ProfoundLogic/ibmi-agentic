     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CVTFSMRA-CONVERT CATOTC AND CATOTD FOR FSMRA RECORDS
     H*****************************************************************
     H*  DESCRIPTION:
     H*  1)  READ ALL FDCAP FILE
     H*  2)  PROCESS ONLY 'FSMRA' RECORDS
     H*  2)  UPDATE CATOTC TO 0, AND CATOTD (NEG CATOTP)
     H*****************************************************************
     F*
     FFDCAP     UF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C*  DETAIL CHARGE FILE KEY LIST
     C*
     C     CAKY          KLIST
     C                   KFLD                    XXCPT4
     C*
     C     *LIKE         DEFINE    CACPT4        XXCPT4
     C*
     C*  GET THE DETAIL CHARGE RECORD
     C*
     C                   MOVE      'FSMRA'       XXCPT4
     C                   READ      PCA01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     CACPT4        IFEQ      'FSMRA'
     C                   Z-ADD     *ZEROS        CATOTC
     C     CATOTP        MULT      -1            CATOTD
     C                   MOVEL(P)  'CVTFSMRA'    CAPGM
     C                   UPDATE    PCA01
     C*
     C                   ENDIF
     C*
     C                   READ      PCA01                                  60
     C                   ENDDO
     C*
     C                   MOVE      '1'           *INLR
