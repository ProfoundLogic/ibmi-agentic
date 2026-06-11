     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CVTEOMBCDS - CONVERT PDS EOMB CODES IN FDCAP TO TX STATE CODES
     H**************************************************************************
     FFDCAP     UF   E           K DISK

     D PDSEOB          S              3    DIM(11) CTDATA PERRCD(1)             PDS EOMB CDS/TX EOMB
     D TXEOB           S              1    DIM(11) ALT(PDSEOB)

     C*
     C* READ THE DETAIL CHARGE FILE
     C*
     C                   READ      PCA01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C* TAKE CAEOMB LOKUP AND RELOAD WITH TX STATE CODES
     C*
     C                   Z-ADD     1             X                 5 0
     C     CAEOMB        LOOKUP    PDSEOB(X)                              70
     C     *IN70         IFEQ      '1'
     C*
     C                   MOVE      *BLANKS       CAEOMB
     C                   MOVEL     TXEOB(X)      CAEOMB
     C*
     C                   ENDIF
     C*
     C                   UPDATE    PCA01
     C*
     C* READ THE NEXT DETAIL CHARGE FILE RECORD
     C*
     C                   READ      PCA01                                  60
     C                   END                                                    ENDDOW 60
     C*
     C* END THE PROGRAM
     C*
     C                   MOVE      '1'           *INLR
**  EOMB CODES
201T
202F
203D
205G
207T
209F
210F
215T
216F
217G
229M
