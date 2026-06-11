     H*
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     H/TITLE SR837FIX -  EDI 837 LOG API
     H*
     H*****************************************************************
     H* PROGRAM DESCRIPTION:  THIS PROGRAM IS CLONED FROM SUBROUTINE  *
     H*         SR837 OF F6250R PROGRAM.                              *
     H*                                                               *
     H*   Called from:  COMMAND LINE                                  *
     H*                                                               *
     H* Created: 09/27/13    C:189    SUPPORT: 83506                  *
     H*****************************************************************
     F*
     FFAEOL31   UF   E           K DISK
     FFELGL1    UF   E           K DISK
     FFAEAL1    UF   E           K DISK
     FFXEXP     IF   E           K DISK
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     C*
     C     EOKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKEOB#
     C*
     C*  FELGL - EDI Log File
     C*
     C     LGKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEOB#
     C*
     C* KEY LIST FOR WCMC CASE LOCKOUT FILE
     C*
     C*
     C     *LIKE         DEFINE    EOCO#         @@CO#
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOCNTL        KKCNTL
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EOAST         EDITST
     C     *LIKE         DEFINE    EAESDT        XXESDT
     C*
     C                   READ      FXEXP                                  90
     C                   DOW       *IN90 = *OFF
     C*
     C                   EVAL      KKCO# =  25
     C                   EVAL      KKEOB# = EXEOB#
     C*
     C     EOKY          CHAIN     PEO01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     EOSUB#        KKSUB#
     C                   MOVEL     EOFND         KKFND
     C                   MOVE      'R'           EOTAPE
     C                   UPDATE    PEO01
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C     LGKY          SETGT     PLG01
     C     LGKY          READPE    PLG01                                  99
     C*
     C                   MOVEL     KKCO#         XXCO#
     C                   MOVEL     KKSUB#        XXSUB#
     C                   MOVEL     KKFND         XXFND
     C                   MOVEL     EOFYR         XXFYR
     C                   MOVEL     EOCASE        XXCASE
     C                   MOVEL     KKEOB#        XXEOB#
     C*
     C*
     C                   MOVE      LGRCN         XXRCN
     C*
     C     LGMTC         IFEQ      '00'
     C     LGSTS         ANDNE     'TA'
     C     LGSTS         ANDNE     'TE'
     C                   CALL      'F6250R'
     C                   PARM                    XXCO#             3            COMPANY
     C                   PARM                    XXSUB#            5            SUBSCRIBER #
     C                   PARM                    XXFND             3            GROUP #
     C                   PARM                    XXFYR             3            POLICY YR
     C                   PARM                    XXCASE            7            CASE/CLAIM
     C                   PARM                    XXEOB#            9            EOMB#
     C                   PARM                    XXRCN             3            RECON #
     C                   PARM      ' '           XXACTF            1            ACTION FLAG
     C                   PARM      ' '           XXRTCD            1
     C                   ELSE
     C                   CALL      'F6250R'
     C                   PARM                    XXCO#             3            COMPANY
     C                   PARM                    XXSUB#            5            SUBSCRIBER #
     C                   PARM                    XXFND             3            GROUP #
     C                   PARM                    XXFYR             3            POLICY YR
     C                   PARM                    XXCASE            7            CASE/CLAIM
     C                   PARM                    XXEOB#            9            EOMB#
     C                   PARM                    XXRCN             3            RECON #
     C                   PARM      'C'           XXACTF            1            ACTION FLAG
     C                   PARM      ' '           XXRTCD            1
     C                   ENDIF
     C*
     C                   READ      FXEXP                                  90
     C                   ENDDO
     C*
     C                   Seton                                        lr
