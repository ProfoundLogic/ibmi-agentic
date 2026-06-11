     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FXEOUSER - UPDATE EOUSER WITH FAESP/EAUSER
     H*****************************************************************
     H*
     H* DESCRIPTION:  UPDATE EOUSER WITH FAESP/EAUSER
     H*****************************************************************
     H*
     H* 01/20/00 C:56 Support: MC221
     H*
     H*****************************************************************
     FFAEOL36   UF   E           K DISK
     FFAESL2    IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     I*

     C     *ENTRY        PLIST
     C                   PARM                    SCDATE            8            MMDDYYYY
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     EOKY          KLIST
     C                   KFLD                    KKDATE
     C*
     C     ESKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C                   KFLD                    KKIDT
     C                   KFLD                    KKITIM
     C*
     C     ESKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EORCN         KKRCN
     C     *LIKE         DEFINE    EOIDT         KKIDT
     C     *LIKE         DEFINE    EOITIM        KKITIM
     C     *LIKE         DEFINE    EODT          KKDATE
     C*
     C                   MOVEL     SCDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKDATE                         CYYMMDD
     C                   MOVE      *HIVAL        KKIDT
     C                   MOVE      *HIVAL        KKITIM
     C*
     C     SRERR         IFEQ      '0'
     C     EOKY          SETLL     PEO01
     C                   READ      PEO01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C                   Z-ADD     EORCN         KKRCN
     C     ESKY1         SETGT     PES01
     C     ESKY2         READPE    PES01                                  98
     C     *IN98         IFEQ      *OFF
     C                   MOVEL     ESUSER        EOUSER
     C                   UPDATE    PEO01
     C                   ELSE
     C                   EXCEPT    PEO0A
     C                   ENDIF                                                  *IN98 = *OFF
     C                   READ      PEO01                                  99
     C                   ENDDO
     C                   ENDIF                                                  SRERR = BLANKS
     C*
     C                   MOVE      '1'           *INLR
     C*
     OPEO01     E            PEO0A
