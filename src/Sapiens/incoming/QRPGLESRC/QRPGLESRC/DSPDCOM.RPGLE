     H*****************************************************************
     H** (C) COPYRIGHT PROGRESSIVE DATA SOLUTIONS, INC., 2004
     H*****************************************************************
     H/TITLE FIXCM75 - FIXES ARCM75
     H*****************************************************************
      ** REVISIONS:
      *                                                               *
     FSDCMLA    IF   E           K DISK
     FSMARL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
     FQSYSPRT2  O    F  132        PRINTER OFLIND(*INOB)
     D CM              S              1    DIM(275)
     D CM75            S              1    DIM(75)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*
     C                   EXCEPT    HEAD
     C                   READ      PCM01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   EXSR      RMVBCH
     C                   READ      PCM01                                  99
     C                   ENDDO
     C*
     C                   EXCEPT    HEAD75
     C                   READ      PAR01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   EXSR      RMVBC75
     C                   READ      PAR01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C*================================================================
     C* RMVBCH - REMOVES INVALID CHARACTERS
     C*================================================================
     C     RMVBCH        BEGSR
     C*
     C                   CLEAR                   CM
     C                   MOVEA     CMCOMM        CM
      *
     C                   DO        275           X                 3 0
     C     CM(X)         IFLT      X'40'
      *
     C     CM(X)         IFNE      X'08'
     C     CM(X)         ANDNE     X'09'
     C                   IF        *INOA = *ON
     C                   EXCEPT    HEAD
     C                   ENDIF
     C                   EXCEPT    DET
     C                   LEAVE
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     CM(X)         IFGT      X'F9'
     C                   IF        *INOA = *ON
     C                   EXCEPT    HEAD
     C                   ENDIF
     C                   EXCEPT    DET
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
      *
     C                   MOVEA     CM            CMCOMM
     C*
     C                   ENDSR
     C*================================================================
     C* RMVBC75 - REMOVES INVALID CHARACTERS
     C*================================================================
     C     RMVBC75       BEGSR
     C*
     C                   CLEAR                   CM75
     C                   MOVEA     ARCM75        CM75
      *
     C                   DO        75            X                 3 0
     C     CM75(X)       IFLT      X'40'
      *
     C     CM75(X)       IFNE      X'08'
     C     CM75(X)       ANDNE     X'09'
     C                   IF        *INOB = *ON
     C                   EXCEPT    HEAD75
     C                   ENDIF
     C                   EXCEPT    DET75
     C                   LEAVE
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     CM75(X)       IFGT      X'F9'
     C                   IF        *INOB = *ON
     C                   EXCEPT    HEAD75
     C                   ENDIF
     C                   EXCEPT    DET75
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C                   ENDSR
     OQSYSPRT   E            HEAD           1 06
     O                                            8 'Key'
     O                                         +039 'Seq # '
     O*
     O          E            DET            1
     O                       CMKEY               45
     O                       CMSEQ#            +002
     OQSYSPRT2  E            HEAD75         1 06
     O                                            8 'Key'
     O                                         +039 'Seq # '
     O                                         +002 'A Seq # '
     O*
     O          E            DET75          1
     O                       ARKEY               45
     O                       ARSEQ#            +002
     O                       ARASQ#            +003
