     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE FIXCM75 - FIXES ARCM75
     H*****************************************************************
      ** REVISIONS:
      *                                                               *
     FSMARL     UF   E           K DISK

     D CM              S              1    DIM(75)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

     C*
     C*   KEY LIST FOR THE REMARK TYPE TABLE FILE
     C*
     C     ARKY          KLIST
     C                   KFLD                    @@KEY
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@KEY            40
     C*
     C     Q1PRMS        IFGT      0
     C     ARKY          CHAIN     PAR01                              99
     C     *IN99         DOWEQ     *OFF
     C                   EXSR      RMVBCH
     C                   UPDATE    PAR01
     C     ARKY          READE     PAR01                                  99
     C                   ENDDO
     C                   ELSE
     C                   READ      PAR01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   EXSR      RMVBCH
     C                   UPDATE    PAR01
     C                   READ      PAR01                                  99
     C                   ENDDO
     C                   ENDIF
     C*
     C                   SETON                                        LR
     C*================================================================
     C* RMVBCH - REMOVES INVALID CHARACTERS
     C*================================================================
     C     RMVBCH        BEGSR
     C*
     C                   CLEAR                   CM
     C                   MOVEA     ARCM75        CM
      *
     C                   DO        75            X                 3 0
     C     CM(X)         IFLT      X'40'
      *
     C     CM(X)         IFNE      X'08'
     C     CM(X)         ANDNE     X'09'
     C                   MOVE      X'40'         CM(X)
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     CM(X)         IFGT      X'F9'
     C                   MOVE      X'40'         CM(X)
     C                   ENDIF
     C                   ENDDO
      *
     C                   MOVEA     CM            ARCM75
     C*
     C                   ENDSR
