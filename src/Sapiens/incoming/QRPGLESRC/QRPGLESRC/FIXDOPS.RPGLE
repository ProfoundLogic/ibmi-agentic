     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      * Work station ID file
     FIASRP     UF   E           K DISK
      * Document Object (transaction) File
     F****IMDOL   UF  E           K        DISK
     FIMDOL1    UF   E           K DISK
     FIMDOLA    UF   E           K DISK
     F*
     FSDCMLH    IF   E           K DISK
     F*
     FQSYSPRT   O    F  132        PRINTER

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*** DOCUMENT TRANSACTION FILE
     C*
     C     DOKY1         KLIST
     C                   KFLD                    SRTDCI
     C*
     C     DOKY2         KLIST
     C                   KFLD                    SRKEY
     C*
     C*
     C*
     C*
     C                   READ      PSR01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C*** IF THIS SR IS INDEXED,
     C     SRINDX        IFEQ      'Y'
     C                   MOVE      *ZERO         SRSEQ#
     C                   EXCEPT    Q1
     C*
     C** GET THE DART RCD BASED ON THE DOC ID.
     C     DOKY1         CHAIN     PCM01                              61
     C     *IN61         DOWEQ     *OFF
     C     CMKEY         IFEQ      SRKEY
     C                   MOVE      CMSEQ#        SRSEQ#
     C                   EXCEPT    Q2
     C                   GOTO      DN1
     C                   ENDIF                                                       61 DOWEQ OFF
     C     DOKY1         READE     PCM01                                  61
     C                   ENDDO                                                       61 DOWEQ OFF
     C     DN1           TAG
     C     SRSEQ#        IFNE      *ZERO
     C                   UPDATE    PSR01
     C                   EXCEPT    Q3
     C     DOKY1         CHAIN     PDO01                              62
     C     *IN62         IFEQ      *OFF
     C                   MOVE      SRSEQ#        DODSQ#
     C                   UPDATE    PDO01
     C                   EXCEPT    Q4
     C                   ENDIF                                                         62 = 0
     C     DOKY2         CHAIN     PDO011                             63
     C     *IN63         DOWEQ     *OFF
     C     DODCID        IFEQ      SRTDCI
     C                   MOVE      SRSEQ#        DODSQ#
     C                   UPDATE    PDO011
     C                   EXCEPT    Q5
     C                   GOTO      DN2
     C                   ENDIF
     C     DOKY2         READE     PDO011                                 63
     C                   ENDDO                                                         63 = 0
     C                   ENDIF                                                         SRSEQ ¬= 0
     C     DN2           TAG
     C*
     C                   ENDIF                                                       INDX
     C                   READ      PSR01                                  60
     C                   ENDDO
     C*
     C     TERM          TAG
     C     EXIT          TAG
     C                   MOVE      '1'           *INLR
     C*
     OQSYSPRT   E            Q1               05
     O                                            3 'Q1:'
     O                       SRTDCI
     O                                              ' '
     O                       SRKEY
     O                                              ' '
     O                       SRSEQ#
     O                                              ' '
     O          E            Q2                1
     O                                            3 'Q2:'
     O                       SRTDCI
     O                                              ' '
     O                       SRKEY
     O                                              ' '
     O                       SRSEQ#
     O                                              ' '
     O          E            Q3                1
     O                                            3 'Q3:'
     O                       SRTDCI
     O                                              ' '
     O                       SRKEY
     O                                              ' '
     O                       SRSEQ#
     O                                              ' '
     O          E            Q4                1
     O                                            3 'Q4:'
     O                       SRTDCI
     O                                              ' '
     O                       SRKEY
     O                                              ' '
     O                       SRSEQ#
     O                                              ' '
     O          E            Q5                1
     O                                            3 'Q5:'
     O                       SRTDCI
     O                                              ' '
     O                       SRKEY
     O                                              ' '
     O                       SRSEQ#
     O                                              ' '
