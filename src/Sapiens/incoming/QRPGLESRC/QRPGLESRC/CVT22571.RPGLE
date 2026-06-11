     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE UPDATE DQAMTR FOR RECORDS PRIOR TO DEDUC CHANGE INSTALL
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     FWDCRP     UF   E             DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C**************************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    QQUPD             1
     C*
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*----------------------------------------------------------------
     C*  Start of main loop
     C*
     C                   READ      PCR01                                  99
     C     *IN99         DOWEQ     '0'
     C*
     C     CRACCT        IFEQ      '71142930'
     C     CRFND         ANDEQ     1
     C     CRCKDT        ANDLE     1000310
     C     CRRDT1        ANDEQ     1000317
     C*
     C                   Z-ADD     1000310       CRRDT1
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C*
     C     QQUPD         IFEQ      'Y'
     C                   UPDATE    PCR01
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*
     C                   READ      PCR01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                                           10 'CVT22571 '
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                            3 'FND'
     O                                            8 'KEY'
     O                                           35 'CRCKDT'
     O                                           45 'CRRDT1'
     O                                           55 'CRRDT2'
     O                                           65 'CRACTP'
     O          E            DET
     O                       CRFND         4      3
     O                       CRKY                20
     O                       CRCKDT              35
     O                       CRRDT1              45
     O                       CRRDT2              55
     O                       CRACTP              65
