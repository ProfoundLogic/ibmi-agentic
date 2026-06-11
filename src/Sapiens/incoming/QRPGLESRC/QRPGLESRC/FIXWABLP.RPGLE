     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWABLP - CORRECT AGENT NUMBER ON BILLING REQUEST RECORDS
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FWABLP     UF   E           K DISK
     FWMAHL     IF   E           K DISK

     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    BLCO#
     C                   KFLD                    BLFND
     C                   KFLD                    BLEMP#
     C                   KFLD                    BLDIV
     C                   KFLD                    BLFYR
      *
     C                   READ      PBL01                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     BLAGT#        IFEQ      *ZERO
     C                   EXSR      UPDTE
     C                   UPDATE    PBL01
     C                   ENDIF
      *
     C                   READ      PBL01                                  60
      *
     C                   ENDDO
      *
     C                   MOVE      *ON           *INLR
      *
      * UPDATE THE AGENT NUMBER FOR BILLING REQUEST.
      *
     C     UPDTE         BEGSR
      *
     C     AHKY          SETGT     PAH01
     C                   READP     PAH01                                  50
     C     *IN50         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  50
     C                   ENDDO                                                  *IN50 EQ 0
     C*
     C     *IN50         IFEQ      '0'
     C     BLCO#         ANDEQ     AHCO#
     C     BLFND         ANDEQ     AHFND
     C     BLEMP#        ANDEQ     AHEMP#
     C     BLDIV         ANDEQ     AHDIV
     C     BLFYR         ANDEQ     AHFYR
     C                   MOVEL     AHAGT#        BLAGT#
     C                   ENDIF                                                  *IN50 EQ 0
      *
     C                   ENDSR
