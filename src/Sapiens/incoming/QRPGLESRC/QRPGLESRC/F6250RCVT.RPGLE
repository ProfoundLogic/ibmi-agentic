     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE F6250RCVT - EDI 837 LOG CONVERSION
     H*
     H*****************************************************************
     H* Description:  This program creates Log records for EOMBs that *
     H*   entered within a selected date range and for a specific     *
     H*   group.                                                      *
     H*                                                               *
     H* Created: 07/09/02    C:15     Project: KY010                  *
     H*****************************************************************
     H*
     FFAEOL     IF   E           K DISK
     FFELGL1    IF   E           K DISK
     F*

     C*================================================================
     C*
     C*  FAEOL - EOMB Master File
     C*
     C     EOKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C*  FELGL1 - 837 Log History File
     C*
     C     LGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    EOEOB#
     C*
     C*  Entry parm list
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3            Company
     C                   PARM                    QQSUB#            5            Subscriber #
     C                   PARM                    QQFND             3            Group #
     C                   PARM                    QQFRDT            8            From date
     C                   PARM                    QQTODT            8            To date
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     *LIKE         DEFINE    EOCO#         XXCO#
     C     *LIKE         DEFINE    EOSUB#        XXSUB#
     C     *LIKE         DEFINE    EOFND         XXFND
     C     *LIKE         DEFINE    EOIDT         XXFRDT
     C     *LIKE         DEFINE    EOIDT         XXTODT
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C*
     C                   MOVE      QQFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   MOVE      QQTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C*  Retrieve group and group detail records
     C*
     C     EOKY          CHAIN     PEO01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     EOIDT         IFGE      XXFRDT
     C     EOIDT         ANDLE     XXTODT
     C*
     C     LGKY          CHAIN     PLG01                              99
     C     *IN99         IFEQ      *OFF
     C     EOBLST        ORNE      'C'
     C                   GOTO      NEXT
     C                   ENDIF
     C*
     C                   MOVE      EOCO#         QQCO#
     C                   MOVE      EOSUB#        QQSUB#
     C                   MOVE      EOFND         QQFND
     C                   MOVE      EOFYR         QQFYR
     C                   MOVE      EOCASE        QQCASE
     C                   MOVE      EOEOB#        QQEOB#
     C                   MOVE      '000'         QQRCN
     C*
     C                   CALL      'F6250R'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQEOB#            9
     C                   PARM                    QQRCN             3
     C                   PARM      *BLANK        QQACTF            1
     C                   PARM      *BLANK        QQRTCD            1
     C*
     C                   ENDIF
     C*
     C     NEXT          TAG
     C     EOKY          READE     PEO01                                  99
     C                   ENDDO
     C*
     C*  End the program.
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*****************************************************************
