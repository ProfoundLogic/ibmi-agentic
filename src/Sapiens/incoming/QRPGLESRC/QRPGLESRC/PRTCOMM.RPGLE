     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE FIXCOMM - FIXES CMCOMM
     H*****************************************************************
     FSDCMLA    IF   E           K DISK
     FQSYSPRT   O    F  127        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOA)

     D CM              S              1    DIM(275)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*
     C*   KEY LIST FOR THE REMARK TYPE TABLE FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    @@KEY
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@KEY            40
     C*
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*
     C     Q1PRMS        IFGT      0
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         DOWEQ     *OFF
     C                   EXSR      RMVBCH
     C     CMKY          READE     PCM01                                  99
     C                   ENDDO
     C                   ELSE
     C                   READ      PCM01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   EXSR      RMVBCH
     C                   READ      PCM01                                  99
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
     C                   MOVEA     CMCOMM        CM
      *
     C                   DO        275           X                 3 0
     C     CM(X)         IFLT      X'40'
      *
     C     CM(X)         IFNE      X'08'
     C     CM(X)         ANDNE     X'09'
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    DETAIL
     C                   LEAVE
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     CM(X)         IFGT      X'F9'
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    DETAIL
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C                   ENDSR
     OQSYSPRT   E            HEAD           1 02
     O                                           90 'INVALID CHARS'
     O                                          111 'PAGE:'
     O                       PAGE               122
     O          E            HEAD           1
     O                                          111 'DATE:'
     O                       DSDATE             122 '  /  /    '
     O*
     O          E            HEAD           2
     O*
     O                                            4 'KEY'
     O                                           48 'SEQ #'
     O          E            DETAIL         1
     O                       CMKEY               41
     O                       CMSEQ#        4     48
