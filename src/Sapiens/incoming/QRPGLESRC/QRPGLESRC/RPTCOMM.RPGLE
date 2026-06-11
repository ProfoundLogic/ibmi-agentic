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
     FSDCMLA    IF   E           K DISK
     FQSYSPRT   O    F  198        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOF)

     D CM              S              1    DIM(275)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

     C*
     C*   KEY LIST FOR THE REMARK TYPE TABLE FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    @@KEY
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@KEY            40
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     *LIKE         DEFINE    CMDT          XXDT
     C     *LIKE         DEFINE    CMIDT         XXIDT
     C*
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
     C* PRINT  - PRINT THE ERROR
     C*================================================================
     C     PRINT         BEGSR
     C*
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   SETOFF                                       OA
     C                   ENDIF
     C*
     C                   Z-ADD     CMDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT
     C*
     C                   Z-ADD     CMIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXIDT
     C*
     C                   EXCEPT    DETAIL
     C*
     C                   ENDSR
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
     C                   EXSR      PRINT
     C                   LEAVE
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     CM(X)         IFGT      X'F9'
     C                   EXSR      PRINT
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C                   ENDSR
     OQSYSPRT   E            HEAD           1 02
     O                                            3 'KEY'
     O                                         +039 'SEQ # '
     O                                         +002 'MAINT PGM '
     O                                         +002 'MAINT USER'
     O                                         +002 'MAINT DATE'
     O                                         +002 'MAINT TIME'
     O                                         +002 'INIT USER '
     O                                         +002 'INIT DATE '
     O          E            DETAIL         1
     O                       CMKEY               40
     O                       CMSEQ#        4   +002
     O                       CMPGM             +002
     O                       CMUSER            +002
     O                       XXDT              +002 '  /  /    '
     O                       CMTIME            +002 '  :  :  '
     O                       CMIUSR            +004
     O                       XXIDT             +002 '  /  /    '
