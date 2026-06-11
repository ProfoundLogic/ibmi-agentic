     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE FIXBADCHR - FIXES BAD CHAR IN ACC DESC
     H*****************************************************************
   H*===============================================================*
   H*  Created   : 07/26/13                                         *
   H*  Programmer: C:180 copied from FIXCOMM                        *
   H*  Support   : CI114 82954                                      *
   H*===============================================================*
     H** REVISIONS:
     H*                                                               *
     FWMCML1    UF   E           K DISK

     D XXCO#           S                   LIKE(CMCO#)
     D XXFND           S                   LIKE(CMFND)
     D XXFYR           S                   LIKE(CMFYR)
     D XXCASE          S                   LIKE(CMCASE)
     D @@CO#           S              3
     D @@FND           S              3
     D @@FYR           S              3
     D @@CASE          S              7
     D CM1             S              1    DIM(60)
     D CM2             S              1    DIM(60)
     D CM3             S              1    DIM(144)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

     C     *ENTRY        PLIST
     C                   PARM                    @@CO#
     C                   PARM                    @@FND
     C                   PARM                    @@FYR
     C                   PARM                    @@CASE
     C*
     C*
     C*   KEY LIST FOR THE CLAIM FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE


     C     Q1PRMS        IFGT      0
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      @@FND         XXFND
     C                   MOVE      @@FYR         XXFYR
     C                   MOVE      @@CASE        XXCASE
     C     CMKY          CHAIN     PCM01                              99
     C                   IF        *IN99 = *OFF
     C                   EXSR      RMVBCH1
     C                   EXSR      RMVBCH2
     C                   EXSR      RMVBCH3
     C                   UPDATE    PCM01
     C                   ENDIF

     C                   ELSE
     C                   READ      PCM01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   EXSR      RMVBCH1
     C                   EXSR      RMVBCH2
     C                   EXSR      RMVBCH3
     C                   UPDATE    PCM01
     C                   READ      PCM01                                  99
     C                   ENDDO
     C                   ENDIF
     C*
     C                   SETON                                        LR
     C*================================================================
     C* RMVBCH - REMOVES INVALID CHARACTERS FOR ACC DESC 1
     C*================================================================
     C     RMVBCH1       BEGSR
     C*
     C** FIX ACC DESC 1
     C*
     C                   CLEAR                   CM1
     C                   MOVEA     CMACDS        CM1
      *
     C                   DO        60            X                 3 0
     C     CM1(X)        IFLT      X'40'
      *
     C     CM1(X)        IFNE      X'08'
     C     CM1(X)        ANDNE     X'09'
     C                   MOVE      X'40'         CM1(X)
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     CM1(X)        IFGT      X'F9'
     C                   MOVE      X'40'         CM1(X)
     C                   ENDIF
     C                   ENDDO
      *
     C                   MOVEA     CM1           CMACDS
     C*
     C                   ENDSR
     C*================================================================
     C* RMVBCH2 - REMOVES INVALID CHARACTERS FOR ACC DESC 2
     C*================================================================
     C     RMVBCH2       BEGSR
     C*
     C** FIX ACC DESC 2
     C*
     C                   CLEAR                   CM2
     C                   MOVEA     CMACD2        CM2
      *
     C                   DO        60            X                 3 0
     C     CM2(X)        IFLT      X'40'
      *
     C     CM2(X)        IFNE      X'08'
     C     CM2(X)        ANDNE     X'09'
     C                   MOVE      X'40'         CM2(X)
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     CM2(X)        IFGT      X'F9'
     C                   MOVE      X'40'         CM2(X)
     C                   ENDIF
     C                   ENDDO
      *
     C                   MOVEA     CM2           CMACD2
     C*
     C                   ENDSR
     C*================================================================
     C* RMVBCH3 - REMOVES INVALID CHARACTERS FOR ACC DESC 3
     C*================================================================
     C     RMVBCH3       BEGSR
     C*
     C** FIX ACC DESC 3
     C*
     C                   CLEAR                   CM3
     C                   MOVEA     CMACD3        CM3
      *
     C                   DO        144           X                 3 0
     C     CM3(X)        IFLT      X'40'
      *
     C     CM3(X)        IFNE      X'08'
     C     CM3(X)        ANDNE     X'09'
     C                   MOVE      X'40'         CM3(X)
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     CM3(X)        IFGT      X'F9'
     C                   MOVE      X'40'         CM3(X)
     C                   ENDIF
     C                   ENDDO
      *
     C                   MOVEA     CM3           CMACD3
     C*
     C                   ENDSR
