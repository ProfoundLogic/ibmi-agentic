     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR**dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT

     D CHR             S              1    DIM(256)
     D HEX             S              1    DIM(128)
     D TABCHR          S              1    DIM(17) CTDATA PERRCD(17)
     D TABDEC          S              2  0 DIM(17) ALT(TABCHR)
     D                 DS
     D  BIN                    1      2B 0
     D  LBIN                   3      4B 0
     D  RBIN                   5      6B 0
     D  BITS                   2      2

     C     *ENTRY        PLIST
     C                   PARM                    CHR
     C                   PARM                    HEX
     C                   PARM                    RTCODE            1
     C                   Z-ADD     0             LX                3 0
     C                   Z-ADD     0             RX                3 0
     C                   Z-ADD     0             HX                3 0
     C                   MOVE      ' '           HEX
     C                   DO        255           LX
     C                   ADD       1             HX                       22
     C                   MOVE      CHR(LX)       TESTA             1
     C     TESTA         COMP      ' '                                    41
     C   41              GOTO      DONE
     C                   EXSR      LOKCHR
     C     RTCODE        CABEQ     'E'           DONE
     C     BIN           MULT      16            LBIN
     C     LX            ADD       1             RX
     C                   MOVE      CHR(RX)       TESTA
     C                   EXSR      LOKCHR
     C     RTCODE        CABEQ     'E'           DONE
     C*
     C                   Z-ADD     BIN           RBIN
     C     LBIN          ADD       RBIN          BIN
     C                   MOVE      BITS          HEX(HX)
     C                   END       2
     C     DONE          TAG
     C                   MOVE      '1'           *INLR
     C*
     CSR   LOKCHR        BEGSR
     CSR   TESTA         LOOKUP    TABCHR        TABDEC                   40
     CSR 40              Z-ADD     TABDEC        BIN
     CSRN40              MOVE      'E'           RTCODE
     CSR                 ENDSR
**
000000101202303404505606707808909A10B11C12D13E14F15
