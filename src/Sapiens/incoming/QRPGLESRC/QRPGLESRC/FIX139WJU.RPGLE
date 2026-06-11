     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIX139WJU - REBILL A/R ON S/R ACCOUNTS WITHOUT INSTALLMENTS
     H*****************************************************************
     H*                                                               *
     H*  ASSESSMENTS WERE BILLED INTO A/R ON S/R ACCOUNTS DUE TO A    *
     H*  UNREQUESTED LIVE INSTALL OF 139WJU.  THIS PROGRAM WILL       *
     H*  REMOVE THOSE FUTURE INSTALLMENTS AND REBILL THE A/R TO THE   *
     H*  CURRENT SYSTEM DATE.                                         *
     H*                                                               *
     H*  PROJECT: 139WJU                   PROGRAMMER: C:65           *
     H*  Created: 10/14/09                                            *
     H*****************************************************************
     FWDELL1    IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWDRRL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     FWMEAL14   UF A E           K DISK
     F*
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  TABLE FILE KEY
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C* RATE SET KEY LIST
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C*
     C* STATISTICAL KEY LIST
     C*
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
     C*
     C     RRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C*
     C* EMPLOYER A/R KEY LIST
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXARCD
     C*
     C*  Like defines
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELPFDT        XXEFDT
     C     *LIKE         DEFINE    ELPFDT        XXDATE
     C     *LIKE         DEFINE    RSJRST        XXST
     C     *LIKE         DEFINE    EAARCD        XXARCD
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C* ENTRY LIST
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQUPD             1
     C*
     C                   MOVE      *OFF          *IN65
     C     QQUPD         IFEQ      'Y'
     C                   MOVE      *ON           *IN65
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C*
     C* STORE CURRENT SYSTEM DATE.
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDAT8            8 0
     C*
     C                   EXCEPT    HEADER
     C*
     C*  MAIN LOOP
     C*
     C                   READ      PEL01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   Z-ADD     ELCO#         XXCO#
     C                   Z-ADD     ELFND         XXFND
     C                   Z-ADD     ELFYR         XXFYR
     C                   Z-ADD     ELEMP#        XXEMP#
     C                   Z-ADD     ELDIV         XXDIV
     C                   Z-ADD     ELPFDT        XXEFDT
     C*
     C                   MOVEL     ELBSCD        XXKEY
     C     TBKEY         CHAIN     PBS01                              60
     C*
     C* PROCESS S/R EMPLOYERS
     C*
     C     *IN60         IFEQ      *OFF
     C     BSSEFL        ANDEQ     'S'
     C*
     C* GET JURISDICTION STATE
     C*
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      RSJRST        XXST
     C*
     C* ACCESS STATISTICAL REPORTING/RATING FOR ASSESSMENT FLAGS
     C*
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   EXSR      PROCE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   READ      PEL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   MOVE      *ON           *INLR
     C*================================================================
     C*  PROCE - PROCESS EMPLOYER
     C*================================================================
     C     PROCE         BEGSR
     C*
     C* 1ST ASSESSMENT
     C*
     C     RR1STB        IFEQ      'Y'
     C*
     C                   MOVE      '1'           XXCTYP            1
     C                   Z-ADD     EL1AAM        XXAAM            11 2
     C                   EXSR      CHKAR
     C*
     C                   ENDIF                                                  RR1STB IFEQ
     C*
     C* 2ND ASSESSMENT
     C*
     C     RR2NDB        IFEQ      'Y'
     C*
     C                   MOVE      '2'           XXCTYP
     C                   Z-ADD     EL2AAM        XXAAM
     C                   EXSR      CHKAR
     C*
     C                   ENDIF                                                  RR2NDB IFEQ
     C*
     C* 3RD ASSESSMENT
     C*
     C     RR3RDB        IFEQ      'Y'
     C*
     C                   MOVE      '3'           XXCTYP
     C                   Z-ADD     EL3AAM        XXAAM
     C                   EXSR      CHKAR
     C*
     C                   ENDIF                                                  RR3RDB IFEQ
     C*
     C* 4TH ASSESSMENT
     C*
     C     RR4THB        IFEQ      'Y'
     C*
     C                   MOVE      '4'           XXCTYP
     C                   Z-ADD     EL4AAM        XXAAM
     C                   EXSR      CHKAR
     C*
     C                   ENDIF                                                  RR4THB IFEQ
     C*
     C* 5TH ASSESSMENT
     C*
     C     RR5THB        IFEQ      'Y'
     C*
     C                   MOVE      '5'           XXCTYP
     C                   Z-ADD     EL5AAM        XXAAM
     C                   EXSR      CHKAR
     C*
     C                   ENDIF                                                  RR5THB IFEQ
     C*
     C* 6TH ASSESSMENT
     C*
     C     RR6THB        IFEQ      'Y'
     C*
     C                   MOVE      '6'           XXCTYP
     C                   Z-ADD     EL6AAM        XXAAM
     C                   EXSR      CHKAR
     C*
     C                   ENDIF                                                  RR6THB IFEQ
     C*
     C* 7TH ASSESSMENT
     C*
     C     RR7THB        IFEQ      'Y'
     C*
     C                   MOVE      '7'           XXCTYP
     C                   Z-ADD     EL7AAM        XXAAM
     C                   EXSR      CHKAR
     C*
     C                   ENDIF                                                  RR7THB IFEQ
     C*
     C                   ENDSR                                                  PROCE
     C*================================================================
     C*  CHKAR - REVIEW A/R FOR ASSESSMENTS
     C*================================================================
     C     CHKAR         BEGSR
     C*
     C* FIND BILLED A/R CODE.
     C*
     C                   MOVE      *BLANKS       XXARCD
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C     TTCTYP        IFEQ      XXCTYP
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XXARCD
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C* PROCESS IF HAVING FOUND ASSESSMENT BILLED A/R CODE
     C*
     C     XXARCD        IFNE      *BLANKS
     C*
     C* READ A/R RECORDS FOR THE ASSESSMENT IN POSTING DATE ORDER.
     C*
     C                   Z-ADD     0             XXAMT            11 2
     C*
     C     EAKY          CHAIN     PEA01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EAPODT        IFGT      XXDATE
     C     EAPODT        ANDNE     ELPFDT
     C     EAAMT         ANDNE     XXAAM
     C*
     C                   Z-ADD     EAPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPODT            8 0
     C*
     C   OF              EXCEPT    HEADER
     C                   EXCEPT    DET
     C                   ADD       EAAMT         XXAMT
     C   65              DELETE    PEA01
     C*
     C                   ENDIF                                                  EAPODT IFGT
     C*
     C     EAKY          READE     PEA01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     XXAMT         IFNE      *ZERO
     C   OF              EXCEPT    HEADER
     C                   EXCEPT    DET2
     C   65              EXSR      ARWRIT
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXARCD IFEQ
     C*
     C                   ENDSR                                                  CHKAR
     C*================================================================
     C*  ARWRIT - WRITE RECORD FOR DELETED A/R AMOUNT
     C*================================================================
     C     ARWRIT        BEGSR
     C*
     C                   CLEAR                   PEA01
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      ELEMP#        EAEMP#
     C                   MOVE      ELDIV         EADIV
     C                   MOVE      XXARCD        EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVEL     TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   Z-ADD     XXAMT         EAAMT
     C                   MOVE      XXDATE        EAPODT
     C                   MOVE      XXDATE        EATRDT
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   MOVE      XXDATE        EADT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EATIME
     C                   MOVE      ' '           EAEDIT
     C                   Z-ADD     0             EABUL1
     C                   Z-ADD     0             EACHK#
     C                   MOVE      *BLANK        EALATE
     C*
     C                   WRITE     PEA01
     C*
     C                   ENDSR                                                  ARWRIT
     C*================================================================
     OQSYSPRT   E            HEADER         2  4
     O                                           24 'S/R EMPLOYER ASSESSMENT'
     O                                           37 'CHANGES FOR:'
     O               65                          60 'UPDATE MODE'
     O                                           80 'PAGE:'
     O                       PAGE          Z     85
     O          E            HEADER         2
     O                                            8 'CO#'
     O                                           12 'FND'
     O                                           23 'EMPLOYER'
     O                                           30 'DIV'
     O                                           34 'FYR'
     O                                           40 'ARCD'
     O                                           52 'POST DATE'
     O                                           68 'AMOUNT'
     O                                           80 'ACTION'
     O          E            DET            1
     O                       XXCO#         Z      8
     O                       XXFND         Z     12
     O                       XXEMP#        Z     23
     O                       XXDIV         Z     30
     O                       XXFYR         Z     34
     O                       XXARCD              40
     O                       XXPODT              52 '  /  /    '
     O                       EAAMT         2     68
     O                                           80 'DELETED'
     O          E            DET2           2
     O                       XXCO#         Z      8
     O                       XXFND         Z     12
     O                       XXEMP#        Z     23
     O                       XXDIV         Z     30
     O                       XXFYR         Z     34
     O                       XXARCD              40
     O                       XXDAT8              52 '  /  /    '
     O                       XXAMT         2     68
     O                                           80 'ADDED'
