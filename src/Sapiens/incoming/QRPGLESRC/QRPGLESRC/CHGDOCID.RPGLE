     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H*===============================================================*
     H** REVISIONS:
     H*
     H*  10/17/11 C:279 Project: SY195
     H*                 Modify program to use UNC path field.
     H*
     H*===============================================================*
     FIMDOL1    IF   E           K DISK

     D FRMDOC          C                   CONST('S4000001.TIF')
     D TODOC           C                   CONST('S4000999.TIF')
     D CHGPGM          C                   CONST('CHGDOCIDC')
      ***** BEG ADD ***** 10/17/11 *********************************************
     D RPGM            C                   CONST('RTVDOCROOT')
      ***** END ADD ***** 10/17/11 *********************************************

     C*
     C     *LIKE         DEFINE    DODCID        KKDCID
     C     *LIKE         DEFINE    DODCID        HDDCID
     C*
     C* IF CALLED SEPERATED FROM PROGRAM CHAIN.
     C*
     C                   MOVEL     FRMDOC        KKDCID
     C     KKDCID        CHAIN     PDO01                              99
     C     *IN99         DOWEQ     *OFF
     C     DODCID        IFLT      TODOC
     C     DOIDT         ANDGT     080101
     C*
     C***** BEG  ADD ***  10/17/11  **********************************
     C     DOUNCN        IFNE      *BLANKS
     C                   MOVE(P)   DOUNCN        FRMPTH          100
     C                   ELSE
     C     DOIFSP        IFEQ      'I'
     C                   CALL      RPGM
     C                   PARM                    ROOT             10
     C*
     C                   CAT(P)    '/':0         FRMPTH          100
     C                   CAT(P)    ROOT:0        FRMPTH
     C                   CAT(P)    '/':0         FRMPTH
     C                   CAT(P)    DOFLR:0       FRMPTH
     C                   ELSE
     C***** END  ADD ***  10/17/11  **********************************
     C     '/QDLS/'      CAT(P)    DOFLR:0       FRMPTH          100
     C***** BEG  ADD ***  10/17/11  **********************************
     C                   ENDIF
     C***** END  ADD ***  10/17/11  **********************************
     C                   CAT       '/':0         FRMPTH
     C                   CAT       DOSFLR:0      FRMPTH
     C                   CAT       '/':0         FRMPTH
     C                   CAT       DODCID:0      FRMPTH
     C***** BEG  ADD ***  10/17/11  **********************************
     C                   ENDIF
     C***** END  ADD ***  10/17/11  **********************************
     C                   MOVEL     DODCID        HDDCID
     C                   MOVEL     'S7'          DODCID
     C                   MOVEL(P)  DODCID        TOPTH           100
     C                   MOVEL     HDDCID        DODCID
     C*
     C                   CALL      CHGPGM
     C                   PARM                    FRMPTH
     C                   PARM                    TOPTH
     C*
     C                   ENDIF
     C*
     C     DODCID        IFGT      TODOC
     C                   LEAVE
     C                   ENDIF
     C*
     C                   READ      PDO01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
