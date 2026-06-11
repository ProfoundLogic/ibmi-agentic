     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE FIXWASRL8 - CONSOLIDATE DUPLICATE S/R PAYROLL RECORDS
     H*****************************************************************
     FWASRL8E   UF   E           K DISK
     F                                     INFDS(SRFDS)
     FWASRP     UF   E             DISK
     F                                     RENAME(PSR01:PSR0P)
     FWDELL     IF   E           K DISK
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)

     D*_______________________________________________________________
     D*
     D SRFDS           DS
     D  SRRRN                397    400B 0
     D*
     D TITLE           C                   CONST('Duplicate S/R -
     D                                     Payroll Records')
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*_______________________________________________________________
     C*
     C     ELKY          KLIST
     C                   KFLD                    SRCO#
     C                   KFLD                    SRFND
     C                   KFLD                    SREMP#
     C                   KFLD                    SRDIV
     C                   KFLD                    SRFYR
     C*_______________________________________________________________
     C*
     C*  INITIALIZE FIELDS
     C*
     C     *LIKE         DEFINE    @@UPDT        XXUPDT
     C     *LIKE         DEFINE    SRRRN         SVRRN
     C     *LIKE         DEFINE    SRCO#         SVCO#
     C     *LIKE         DEFINE    SRFND         SVFND
     C     *LIKE         DEFINE    SREMP#        SVEMP#
     C     *LIKE         DEFINE    SRDIV         SVDIV
     C     *LIKE         DEFINE    SRFYR         SVFYR
     C     *LIKE         DEFINE    SREFDT        SVEFDT
     C     *LIKE         DEFINE    SRCLAS        SVCLAS
     C     *LIKE         DEFINE    SRSPEC        SVSPEC
     C     *LIKE         DEFINE    SREXT         SVEXT
     C     *LIKE         DEFINE    SRTODT        SVTODT
     C     *LIKE         DEFINE    SRAMTP        SVAMTP
     C     *LIKE         DEFINE    SRAMTP        SVAMT
     C     *LIKE         DEFINE    *IN60         DUPFLG
     C*_______________________________________________________________
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
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@UPDT            1            Y/N
     C*
     C                   READ      PSR01                                  60
     C                   Z-ADD     SRRRN         SVRRN
     C                   MOVE      *ON           *INOF
     C     *IN60         DOWEQ     *OFF
     C*
     C     SRCO#         IFEQ      SVCO#
     C     SRFND         ANDEQ     SVFND
     C     SRFYR         ANDEQ     SVFYR
     C     SREMP#        ANDEQ     SVEMP#
     C     SRDIV         ANDEQ     SVDIV
     C     SRTODT        ANDEQ     SVTODT
     C     SRTODT        ANDNE     *ZERO
     C     SRRPMY        ANDEQ     *ZERO
     C     SRCLAS        ANDEQ     SVCLAS
     C     SRSPEC        ANDEQ     SVSPEC
     C     SREXT         ANDEQ     SVEXT
     C     SREFDT        ANDEQ     SVEFDT
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF                                                  *INOF=*ON
     C                   EXCEPT    DET
     C                   MOVE      *ON           DUPFLG
     C                   ADD       SRAMTP        SVAMT
     C                   ADD       1             XXTOTL
     C     XXUPDT        IFEQ      'Y'
     C                   DELETE    PSR01
     C                   ENDIF                                                  XXUPDT=Y
     C                   ENDIF                                                  *IN60=*OFF
     C                   ELSE
     C                   Z-ADD     SRCO#         SVCO#
     C                   Z-ADD     SRFND         SVFND
     C                   Z-ADD     SRFYR         SVFYR
     C                   Z-ADD     SREMP#        SVEMP#
     C                   Z-ADD     SRDIV         SVDIV
     C                   Z-ADD     SRTODT        SVTODT
     C                   Z-ADD     SRCLAS        SVCLAS
     C                   MOVE      SRSPEC        SVSPEC
     C                   MOVE      SREXT         SVEXT
     C                   Z-ADD     SREFDT        SVEFDT
     C                   Z-ADD     SRAMTP        SVAMTP
     C     XXUPDT        IFEQ      'Y'
     C     DUPFLG        ANDEQ     *ON
     C                   EXSR      UPDTSR
     C                   ENDIF                                                  XXUPDT=Y
     C                   MOVE      *OFF          DUPFLG
     C                   Z-ADD     SRRRN         SVRRN
     C                   Z-ADD     SVAMTP        SVAMT
     C                   ENDIF                                                  EPCO#=SVCO#
      *
     C                   READ      PSR01                                  60
     C                   ENDDO                                                  *IN60=*OFF
      *
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF                                                  *INOF=*ON
     C     XXTOTL        COMP      *ZEROS                                 70
     C                   EXCEPT    TOTAL
     C                   MOVE      *ON           *INLR
     C*===============================================================
     C* *INZSR - INITIALIZATION SUBROUTINE
     C*===============================================================
     C*
     C     *INZSR        BEGSR
     C*
     C                   Z-ADD     *ZEROS        XXTOTL           11 0
     C*
     C                   CLEAR                   XXUPDT
     C*
     C     Q1PRMS        IFGE      1
     C                   MOVE      @@UPDT        XXUPDT
     C                   ENDIF                                                  Q1PRMS>=1
     C*
     C                   CLEAR                   SVCO#
     C                   CLEAR                   SVFND
     C                   CLEAR                   SVFYR
     C                   CLEAR                   SVEMP#
     C                   CLEAR                   SVDIV
     C                   CLEAR                   SVTODT
     C                   CLEAR                   SVCLAS
     C                   CLEAR                   SVSPEC
     C                   CLEAR                   SVEFDT
     C*
     C     XXUPDT        IFEQ      'Y'
     C     'Fix'         CAT(P)    TITLE:1       ##DATA
     C                   ELSE
     C     'List'        CAT(P)    TITLE:1       ##DATA
     C                   ENDIF                                                  XXUPDT=Y
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XXTITL           60
     C*
     C                   ENDSR                                                  *INZSR
     C*
     C*===============================================================
     C* UPDTSR - UPDATE WASRP WITH THE TOTAL OF ALL DUPLICATE RECORDS
     C*===============================================================
     C*
     C     UPDTSR        BEGSR
     C*
     C     SVRRN         CHAIN     PSR0P                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     SVAMT         SRAMTP
     C*
     C                   MOVEL(P)  Q1PGM         SRPGM
     C                   MOVEL(P)  Q1USER        SRUSER
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SRDT
     C*
     C                   Z-ADD     DSTIME        SRTIME
     C*
     C                   UPDATE    PSR0P
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  UPDTSR
     C*_______________________________________________________________
     C*
     OQSYSPRT   E            HEAD           2 06
     O                       Q1PGM
     O                       XXTITL              91
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                              'CO#'
     O                                         +001 'GRP'
     O                                         +001 'EMPLOYER#'
     O                                         +001 '  DIV'
     O                                         +001 ' YR'
     O                                         +001 'EFFECT DT'
     O                                         +001 'CLASS'
     O                                         +001 'SPC'
     O                                         +001 'EXT'
     O                                         +001 'CYY/MM'
     O                                         +001 'STS'
     O                                         +001 '      AMOUNT '
     O                                         +001 'DUP REC AMNT '
     O                                         +001 'USER      '
     O                                         +001 'PROGRAM   '
     O                                         +001 ' MAINT DT'
     O                                         +001 'MAINT TM'
     O          E            DET            1
     O                       SRCO#         4
     O                       SRFND         4   +001
     O                       SREMP#        4   +001
     O                       SRDIV         4   +001
     O                       SRFYR         4   +001
     O                       SREFDT            +001 '   /  /  '
     O                       SRCLAS            +001
     O                       SRSPEC            +002
     O                       SREXT             +001
     O                       SRTODT            +002
     O                       SRSTAT            +002
     O                       SVAMTP        L   +002
     O                       SRAMTP        L   +001
     O                       SRUSER            +001
     O                       SRPGM             +001
     O                       SRDT              +001 '   /  /  '
     O                       SRTIME            +001 '  :  :  '
     O          E            TOTAL       2  1
     O              N70                             'Total # of duplicates:'
     O              N70      XXTOTL        J   +002
     O               70                             '* * * No Dupliacte'
     O               70                        +001 'Records Found * * *'
