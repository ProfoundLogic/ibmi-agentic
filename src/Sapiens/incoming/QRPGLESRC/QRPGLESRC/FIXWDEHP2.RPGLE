     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWDEHP2 - INITIALIZE THE NEW/RENEWAL FLAG IN WDEHP.
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/12/02 C:65 PROJECT: SUPP#31223                             *
     H*               ONLY SET THE NEW/RENEWAL FLAG TO NEW IF THE     *
     H*               POLICY YEAR IS THE EARLIEST ON THE POLICY.      *
     H*                                                               *
     H* 10/05/01  C:65  PROJECT WC1030                                *
     H*                 CHECK FOR PREVIOUS EMPLR# FOR NEW/RENEWAL.    *
     H*                                                               *
     H*****************************************************************
     FWDEHP     UF   E             DISK
     FWDELL     IF   E           K DISK
     F*** BEGIN ADD ***  10/05/01  ************************************
     FWZEXL     IF   E           K DISK
     F*** END ADD *****  10/05/01  ************************************
      *

     C     ELKY          KLIST
     C                   KFLD                    EHCO#
     C                   KFLD                    EHFND
     C                   KFLD                    EHEMP#
     C                   KFLD                    EHDIV
     C                   KFLD                    EHFYR
      *
     C     ELKY1         KLIST
     C                   KFLD                    EHCO#
     C                   KFLD                    EHFND
     C                   KFLD                    EHEMP#
     C                   KFLD                    EHDIV
     C*** BEGIN ADD ***  08/12/02  ************************************
     C     ELKY7         KLIST
     C                   KFLD                    EHCO#
     C                   KFLD                    EHFND
     C                   KFLD                    EHEMP#
     C*** END ADD *****  08/12/02  ************************************
     C*** BEGIN ADD ***  10/05/01  ************************************
     C     ZEXKY         KLIST
     C                   KFLD                    EHCO#
     C                   KFLD                    EHFND
     C                   KFLD                    EHEMP#
     C                   KFLD                    EHDIV
     C*** END ADD *****  10/05/01  ************************************
      *
     C                   READ      PEH01                                  60
     C     *IN60         DOWEQ     *OFF
      *
     C     ELKY          CHAIN     PEL01                              60
     C     ELKY1         READPE    PEL01                                  60
     C*** BEGIN ADD ***  10/05/01  ************************************
     C   60ZEXKY         CHAIN     PEX01                              60
     C*** END ADD *****  10/05/01  ************************************
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'R'           EHNWRW
     C                   ELSE
     C                   MOVE      'N'           EHNWRW
     C*** BEGIN ADD ***  08/12/02  ************************************
     C     ELKY7         CHAIN     PEL01                              60
     C     *IN60         DOWEQ     *OFF
     C     EHNWRW        ANDEQ     'N'
     C     ELFYR         IFLT      EHFYR
     C                   MOVE      'R'           EHNWRW
     C                   ENDIF
     C     ELKY7         READE     PEL01                                  60
     C                   ENDDO
     C*** END ADD *****  08/12/02  ************************************
     C                   ENDIF
      *
     C                   UPDATE    PEH01
      *
     C                   READ      PEH01                                  60
     C                   ENDDO
      *
     C                   MOVE      *ON           *INLR
