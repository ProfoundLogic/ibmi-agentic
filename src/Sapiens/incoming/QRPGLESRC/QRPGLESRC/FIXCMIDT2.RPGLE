     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE PROGRAM TO LOAD CMIDT IF ZERO
     H*****************************************************************
     H* Description:  This pgm will load CMIDT, if zero and CHEFDT    *
     H*   can not be used, w/CMACDT + 10 days                         *
     H*****************************************************************
     FWMCML1    UF   E           K DISK
     FWMCHL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     FQSYSPRT1  O    F  132        PRINTER OFLIND(*INOA)
     FQSYSPRT2  O    F  132        PRINTER OFLIND(*INOB)

     D*
     D DAYS            S              2  0 DIM(12) CTDATA PERRCD(1)
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D TITLE           C                   CONST('Updated Records w/')
     D EFDT            C                   CONST('CHEFDT')
     D ACDT            C                   CONST('CMACDT + 10 days')
     D NON             C                   CONST('Non-Updated Records')
     D NORECP          C                   CONST('* * * No Records * *-
     D                                      *')
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  X7ACDT                 1      7  0
     D  X7CYY                  1      3  0
     D  X7MM                   4      5  0
     D  X7DD                   6      7  0

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
     C     *LIKE         DEFINE    *IN60         NOREC
     C     *LIKE         DEFINE    *IN60         NOREC1
     C     *LIKE         DEFINE    *IN60         NOREC2
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*_______________________________________________________________
     C*
     C                   MOVE      *ON           NOREC
     C                   MOVE      *ON           NOREC1
     C                   MOVE      *ON           NOREC2
     C                   TIME                    DSDTTM
     C*
     C                   EXSR      TITLES
     C*_______________________________________________________________
     C*
     C*  MAIN LOOP
     C*
     C                   READ      PCM01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     CMCO#         IFNE      SVCO#
     C     CMCO#         CHAIN     PCO01                              61
     C                   EXSR      CNTRCO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD2
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
     C                   Z-ADD     CHEFDT        CMIDT
     C*
     C                   EXSR      UPDATR
     C*
     C                   MOVE      *OFF          NOREC
     C*
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C*
     C                   ELSE
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*
     C     SRERR         IFEQ      *OFF
     C*
     C                   Z-ADD     CMACDT        X7ACDT
     C*
     C                   ADD       10            X7DD
     C*
     C                   Z-ADD     X7MM          X                 3 0
     C*
     C     X7DD          IFGT      DAYS(X)
     C                   SUB       DAYS(X)       X7DD
     C                   ADD       1             X7MM
     C                   ENDIF                                                  X7DD>DAYS,X
     C*
     C     X7MM          IFGT      12
     C                   SUB       12            X7MM
     C                   ADD       1             X7CYY
     C                   ENDIF                                                  X7MM>12
     C*
     C                   Z-ADD     X7ACDT        CMIDT
     C*
     C                   EXSR      UPDATR
     C*
     C                   MOVE      *OFF          NOREC1
     C*
     C   OA              EXCEPT    HEAD1
     C                   EXCEPT    DET1
     C*
     C                   ELSE
     C*
     C                   EXSR      DATES
     C*
     C                   MOVE      *OFF          NOREC2
     C*
     C   OB              EXCEPT    HEAD2
     C                   EXCEPT    DET2
     C*
     C                   ENDIF                                                  SRERR=*OFF
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
     C     NOREC2        IFEQ      *ON
     C                   EXCEPT    MSG2
     C                   ENDIF                                                  NOREC2=*ON
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
     C                   MOVE      SRERR         *IN73
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X8ACDT            8 0
     C                   MOVE      SRERR         *IN72
     C*
     C                   Z-ADD     CMIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X8IDT             8 0
     C*
     C     CMIDT         IFNE      *ZEROS
     C                   MOVE      SRERR         *IN71
     C                   ENDIF                                                  CMIDT<>*ZEROS
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
     C     TITLE         CAT(P)    EFDT:0        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA
     C                   PARM      60            ##LENG
     C*
     C                   MOVEL     ##DATA        XXTITL           60
     C*_______________________________________________________________
     C*
     C     TITLE         CAT(P)    ACDT:0        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA
     C                   PARM      60            ##LENG
     C*
     C                   MOVEL     ##DATA        X1TITL           60
     C*_______________________________________________________________
     C*
     C                   MOVEL(P)  NON           ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA
     C                   PARM      60            ##LENG
     C*
     C                   MOVEL     ##DATA        X2TITL           60
     C*
     C                   ENDSR                                                  CNTRCO
     C*
     C*===============================================================
     C* UPDATR - UPDATE RECORDS
     C*===============================================================
     C*
     C     UPDATR        BEGSR
     C*
     C                   TIME                    DSDTTM
     C*
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
     O              N71      X8IDT               40 '  /  /    '
     O               71                          40 ' BAD DATE '
     O              N72      X8ACDT              55 '  /  /    '
     O               72                          55 ' BAD DATE '
     O              N73      X8EFDT              70 '  /  /    '
     O               73                          70 ' BAD DATE '
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
     O              N71      X8IDT               40 '  /  /    '
     O               71                          40 ' BAD DATE '
     O              N72      X8ACDT              55 '  /  /    '
     O               72                          55 ' BAD DATE '
     O              N73      X8EFDT              70 '  /  /    '
     O               73                          70 ' BAD DATE '
     O                       CHOCST              73
     O          E            LINE           1
     O          E            MSG1           1
     O                       NORECP              50
     O*_______________________________________________________________
     O*
     OQSYSPRT2  E            HEAD2          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              81
     O                       DSDATE        Y    132
     O          E            HEAD2          2
     O                       Q1PGM               10
     O                       X2TITL              91
     O                                          126 'PAGE:'
     O                       PAGE3         Z    132
     O          E            HEAD2          2
     O                                            3 'CO#'
     O                                           10 'FND'
     O                                           15 'FYR'
     O                                           24 'CASE'
     O                                           37 'CMIDT'
     O                                           52 'CMACDT'
     O                                           67 'CHEFDT'
     O                                           77 'CHOCST'
     O          E            DET2           1
     O                       CMCO#         4      3
     O                       CMFND         4     10
     O                       CMFYR         4     15
     O                       CMCASE        4     24
     O              N71      X8IDT               40 '  /  /    '
     O               71                          40 ' BAD DATE '
     O              N72      X8ACDT              55 '  /  /    '
     O               72                          55 ' BAD DATE '
     O              N73      X8EFDT              70 '  /  /    '
     O               73                          70 ' BAD DATE '
     O                       CHOCST              73
     O          E            LINE           1
     O          E            MSG2           1
     O                       NORECP              50
** DAYS
31
28
31
30
31
30
31
31
30
31
30
31
