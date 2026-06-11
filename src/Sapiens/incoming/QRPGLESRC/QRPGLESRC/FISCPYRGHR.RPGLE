     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H DATEDIT(*YMD)
     FSOURCE    UF A F   92        DISK

     D CR              S             80    DIM(3) CTDATA PERRCD(1)

     ISOURCE    NS
     I                                 13   92  SRCDTA
     I                                 13   14  SD0102
     I                                 18   19  SD0607

     C*
     C     *ENTRY        PLIST
     C                   PARM                    SRCFIL           10
     C*
     C                   READ      SOURCE                                 99
     C     *IN99         DOWEQ     *OFF
     C                   EXSR      CHKSRC
     C                   READ      SOURCE                                 99
     C                   ENDDO
     C                   SELECT
     C     SRCFIL        WHENEQ    'QDDSSRC'
     C                   MOVEL     CR(1)         SRCDTA
     C     SRCFIL        WHENEQ    'QRPGSRC'
     C                   MOVEL     CR(2)         SRCDTA
     C     SRCFIL        WHENEQ    'QCLSRC'
     C                   MOVEL     CR(3)         SRCDTA
     C                   ENDSL
     C                   EXCEPT    WRITE
     C                   MOVE      *ON           *INLR
     C*
     C     CHKSRC        BEGSR
     C     SRCFIL        IFEQ      'QRPGSRC'
     C     SD0607        ANDEQ     'H*'
     C     SRCFIL        OREQ      'QDDSSRC'
     C     SD0607        ANDEQ     'A*'
     C     SRCFIL        OREQ      'QCLSRC'
     C     SD0102        ANDEQ     '/*'
     C     '(C)'         SCAN      SRCDTA                                 90
     C     *IN90         IFEQ      *ON
     C                   DELETE    SOURCE
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
     C*
     OSOURCE    EADD         WRITE
     O                                            6 '000001'
     O                       UDATE               12
     O                       SRCDTA              92
     O                       *YEAR               73
**
     A*  (C) Copyright Progressive Data Solutions, Inc., xxxx
     H*  (C) Copyright Progressive Data Solutions, Inc., xxxx
/*       (C) Copyright Progressive Data Solutions, Inc., xxxx        */
