     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE SUP42238R
     H*****************************************************************
     H*
     H*  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     H*  *** VERY IMPORTANT THIS PROGRAM BE CALLED ONLY FROM SUP42238C ***
     H*  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     H*
     H*  SUP42238C - Overrides physical files to archive library prior
     H*              to calling this program.
     H*
     H*
     H* DELETE AUDIT TRACKING RECORDS WMAUP WITH NO CORRESPONDING
     H* EMPLOYER DETAIL WDELP RECORD.
     H*
     H*****************************************************************
      *
      * Audit Tracking
     FWMAUL6    UF   E           K DISK
     FWMAUP     O    E             DISK
     F                                     RENAME(PAU01:PAU02)
      *
      * Employer Detail
     FWDELL     IF   E           K DISK
      *
      * Output
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
      *
      *
      *

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
      *
      *
      *==============================================================
      * KLIST
      *==============================================================
      *
      * Employer Policy Year
      *

     C     ELKY          KLIST
     C                   KFLD                    AUCO#
     C                   KFLD                    AUFND
     C                   KFLD                    AUEMP#
     C                   KFLD                    AUDIV
     C                   KFLD                    AUFYR
      *
      *
      *
      *=================================================================
      *  PLIST
      *=================================================================
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CL              1
      *
      *==============================================================
      *  Start of main loop
      *==============================================================
      *
      *  !!! ONLY RUN / CALL THIS PROGRAM FROM: SUP42238C !!!
      *
      *  Otherwise duplicate records will be created in LIVE data.
      *
     C     @@CL          IFNE      'C'
     C                   SETON                                        LR
     C                   RETURN
     C                   ENDIF
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     0             XXCNT             7 0
     C                   SETON                                        OF
      *
     C     *LOVAL        SETLL     PAU01
     C                   READ      PAU01                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     ELKY          CHAIN     PEL01                              60
      *
     C     *IN60         IFEQ      *ON
     C   OF              EXCEPT    HEAD
     C   OF              SETOFF                                       OF
     C                   EXCEPT    DET
     C                   ADD       1             XXCNT
     C                   WRITE     PAU02
     C                   DELETE    PAU01
     C                   ENDIF
      *
     C                   READ      PAU01                                  60
     C                   ENDDO
      *
     C                   EXCEPT    TOT
     C                   SETON                                        LR
      *
      *
      **************************************************************************
      *
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       DSDATE        Y    132
      *
     O          E            HEAD           2
     O                                           10 'SUP42238'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
      *
     O          E            HEAD           2
     O                                         +000 'CO#'
     O                                         +002 'FND'
     O                                         +002 'EMPLOYER '
     O                                         +002 'DIV  '
     O                                         +002 'FYR'
      *
     O          E            DET
     O                       AUCO#         4   +000
     O                       AUFND         4   +002
     O                       AUEMP#        4   +002
     O                       AUDIV         4   +002
     O                       AUFYR         4   +002
      *
     O          E            TOT
     O                                           13 'TOTAL COUNT:'
     O                       XXCNT         4     21
      *
