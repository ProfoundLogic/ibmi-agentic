     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE PROGRAM TO LOAD CMIDT IF ZERO
     H*****************************************************************
     H* Description:  This pgm will load CMIDT, if zero, with CHEFDT  *
     H*   of the first history record.                                *
     H*****************************************************************
     FWMCML1    UF   E           K DISK
     FWMCHL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     FQSYSPRT1  O    F  132        PRINTER OFLIND(*INOA)

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D TITLE           C                   CONST('Updated Records')
     D NON             C                   CONST('Non-')
     D NORECP          C                   CONST('* * * No Records * *-
     D                                      *')
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*
     C*  CLAIM HISTORY FILE
     C*
     C     CHKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C     *LIKE         DEFINE    CMCO#         SVCO#
     C     *LIKE         DEFINE    CMFND         SVFND
     C     *LIKE         DEFINE    CMFYR         SVFYR
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*_______________________________________________________________
     C*
     C                   MOVE      *ON           NOREC             1
     C                   MOVE      *ON           NOREC1            1
     C                   TIME                    DSDTTM
     C*
     C                   EXSR      TITLES
     C*_______________________________________________________________
     C*
     C*  MAIN LOOP
     C*
     C                   READ      PCM01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     CMCO#         IFNE      SVCO#
     C     CMCO#         CHAIN     PCO01                              61
     C                   EXSR      CNTRCO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   Z-ADD     CMCO#         SVCO#
     C                   Z-ADD     CMFND         SVFND
     C                   Z-ADD     CMFYR         SVFYR
     C                   ENDIF                                                  CMCO#<>SVCO#
     C*
     C     CMIDT         IFEQ      *ZERO
     C*
     C     CHKY          SETLL     PCH01
     C     CHKY          READE     PCH01                                  61
     C*
     C     *IN61         IFEQ      *OFF
     C     CHOCST        ANDEQ     'O'
     C     CHEFDT        ANDGE     CMACDT
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     CHEFDT        CMIDT
     C                   MOVEL(P)  Q1PGM         CMPGM
     C                   MOVEL(P)  Q1USER        CMUSER
     C                   Z-ADD     DSTIME        CMTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C*
     C                   UPDATE    PCM01
     C*
     C                   EXSR      DATES
     C*
     C                   MOVE      *OFF          NOREC
     C*
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C*
     C                   ELSE
     C*
     C                   EXSR      DATES
     C*
     C                   MOVE      *OFF          NOREC1
     C*
     C   OA              EXCEPT    HEAD1
     C                   EXCEPT    DET1
     C*
     C                   ENDIF                                                  *IN61=*OFF
     C*
     C                   ENDIF                                                  CMIDT=*ZEROS
     C*
     C                   READ      PCM01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C     NOREC         IFEQ      *ON
     C                   EXCEPT    MSG
     C                   ENDIF                                                  NOREC=*ON
     C*
     C     NOREC1        IFEQ      *ON
     C                   EXCEPT    MSG1
     C                   ENDIF                                                  NOREC1=*ON
     C*
     C                   SETON                                        LR
     C*_______________________________________________________________
     C*
     C*===============================================================
     C* CNTRCO - SUBROUTINE TO CENTER COMPANY NAME
     C*===============================================================
     C*
     C     CNTRCO        BEGSR
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        CONAME
     C*
     C                   ENDSR                                                  CNTRCO
     C*
     C*===============================================================
     C* DATES - SUBROUTINE TO CONVERT THE DATES FOR PRINTING
     C*===============================================================
     C*
     C     DATES         BEGSR
     C*
     C     CMFND         IFNE      SVFND
     C     CMFYR         ORNE      SVFYR
     C                   EXCEPT    LINE
     C                   Z-ADD     CMFND         SVFND
     C                   Z-ADD     CMFYR         SVFYR
     C                   ENDIF                                                  CMFND<>SVFND
     C*
     C                   Z-ADD     CHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X8EFDT            8 0
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X8ACDT            8 0
     C*
     C                   Z-ADD     CMIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X8IDT             8 0
     C*
     C                   CLEAR                   CHEFDT
     C                   CLEAR                   CMACDT
     C                   CLEAR                   CMIDT
     C*
     C                   ENDSR                                                  DATES
     C*
     C*===============================================================
     C* TITLES - SUBROUTINE TO CENTER TITLES
     C*===============================================================
     C*
     C     TITLES        BEGSR
     C*
     C                   MOVEL(P)  TITLE         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA
     C                   PARM      60            ##LENG
     C*
     C                   MOVEL     ##DATA        XXTITL           60
     C*
     C     NON           CAT(P)    TITLE:0       ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA
     C                   PARM      60            ##LENG
     C*
     C                   MOVEL     ##DATA        X1TITL           60
     C*
     C                   ENDSR                                                  CNTRCO
     C*_______________________________________________________________
     C*
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              81
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       XXTITL              91
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                            3 'CO#'
     O                                           10 'FND'
     O                                           15 'FYR'
     O                                           24 'CASE'
     O                                           37 'CMIDT'
     O                                           52 'CMACDT'
     O                                           67 'CHEFDT'
     O                                           77 'CHOCST'
     O          E            DET            1
     O                       CMCO#         4      3
     O                       CMFND         4     10
     O                       CMFYR         4     15
     O                       CMCASE        4     24
     O                       X8IDT               40 '  /  /    '
     O                       X8ACDT              55 '  /  /    '
     O                       X8EFDT              70 '  /  /    '
     O                       CHOCST              73
     O          E            LINE           1
     O          E            MSG            1
     O                       NORECP              50
     O*_______________________________________________________________
     O*
     OQSYSPRT1  E            HEAD1          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              81
     O                       DSDATE        Y    132
     O          E            HEAD1          2
     O                       Q1PGM               10
     O                       X1TITL              91
     O                                          126 'PAGE:'
     O                       PAGE1         Z    132
     O          E            HEAD1          2
     O                                            3 'CO#'
     O                                           10 'FND'
     O                                           15 'FYR'
     O                                           24 'CASE'
     O                                           37 'CMIDT'
     O                                           52 'CMACDT'
     O                                           67 'CHEFDT'
     O                                           77 'CHOCST'
     O          E            DET1           1
     O                       CMCO#         4      3
     O                       CMFND         4     10
     O                       CMFYR         4     15
     O                       CMCASE        4     24
     O                       X8IDT               40 '  /  /    '
     O                       X8ACDT              55 '  /  /    '
     O                       X8EFDT              70 '  /  /    '
     O                       CHOCST              73
     O          E            LINE           1
     O          E            MSG1           1
     O                       NORECP              50
