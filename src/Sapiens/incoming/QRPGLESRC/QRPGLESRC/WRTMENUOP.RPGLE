     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE WRTMENUOP - WRITES MENU OPTIONS
     H*
     H*****************************************************************
     H* Created on 08/05/2003 by c:62 for project PRMGT
     H*****************************************************************
     H* Revisions:
     H*
     H*****************************************************************
     FMMOPL1    IF   E           K DISK
     FMMOPL     IF A E           K DISK
     F                                     RENAME(POP01:POP02)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D CMD1            C                   CONST('WRKSPLF')
     D CMD2            C                   CONST('WRKUSRJOB')
     D CMD3            C                   CONST('WRKWTR WTR(*ALL)')
     D*
     D NAM1            C                   CONST('DISPLAY USER SPOOLED-
     D                                      FILES')
     D NAM2            C                   CONST('WORK WITH USER JOBS')
     D NAM3            C                   CONST('WORK WITH ALL -
     D                                     WRITERS')

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     OPKY          KLIST
     C                   KFLD                    KKMNAM
     C                   KFLD                    KKOPTN
     C*
     C     *LIKE         DEFINE    OPMNAM        KKMNAM
     C     *LIKE         DEFINE    OPOPTN        KKOPTN
     C*
     C                   READ      POP01                                  99
     C     *IN99         DOWEQ     *OFF
     C     OPMNAM        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  OPMNAM        KKMNAM
     C*
     C* Write the spool file record
     C*
     C                   EXSR      WRTSPL
     C*
     C* Write the user job record
     C*
     C                   EXSR      WRTUSR
     C*
     C* Write the writers record
     C*
     C                   EXSR      WRTWRT
     C*
     C                   ENDIF
     C                   READ      POP01                                  99                R
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C*================================================================
     C* WRTSPL - Writes the spool file record
     C*================================================================
     C     WRTSPL        BEGSR
     C*
     C                   MOVEL(P)  'A002'        KKOPTN
     C     OPKY          CHAIN     POP02                              99
     C     *IN99         IFEQ      *ON
     C                   CLEAR                   POP02
     C*
     C                   MOVEL(P)  KKMNAM        OPMNAM
     C                   MOVEL(P)  KKOPTN        OPOPTN
     C*
     C                   MOVEL(P)  'N'           OPMYN
     C                   MOVEL(P)  'LIV'         OPAPP
     C                   MOVEL(P)  CMD1          OPEXCL
     C                   MOVEL(P)  'Y'           OPYNO
     C                   Z-ADD     10            OPMINS
     C                   MOVEL(P)  '*'           OPTY01
     C                   MOVEL(P)  NAM1          OPMNTT
     C*
     C                   MOVEL(P)  'A'           OPSTAT
     C                   MOVEL(P)  Q1USER        OPUSER
     C                   MOVEL(P)  Q1PGM         OPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        OPTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OPDT
     C*
     C                   WRITE     POP02
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTUSR - Writes the user job record
     C*================================================================
     C     WRTUSR        BEGSR
     C*
     C                   MOVEL(P)  'A003'        KKOPTN
     C     OPKY          CHAIN     POP02                              99
     C     *IN99         IFEQ      *ON
     C                   CLEAR                   POP02
     C*
     C                   MOVEL(P)  KKMNAM        OPMNAM
     C                   MOVEL(P)  KKOPTN        OPOPTN
     C*
     C                   MOVEL(P)  'N'           OPMYN
     C                   MOVEL(P)  'LIV'         OPAPP
     C                   MOVEL(P)  CMD2          OPEXCL
     C                   MOVEL(P)  'Y'           OPYNO
     C                   Z-ADD     999           OPMINS
     C                   MOVEL(P)  '*'           OPTY01
     C                   MOVEL(P)  NAM2          OPMNTT
     C*
     C                   MOVEL(P)  'A'           OPSTAT
     C                   MOVEL(P)  Q1USER        OPUSER
     C                   MOVEL(P)  Q1PGM         OPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        OPTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OPDT
     C*
     C                   WRITE     POP02
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTWRT - Writes the all writers record
     C*================================================================
     C     WRTWRT        BEGSR
     C*
     C                   MOVEL(P)  'A004'        KKOPTN
     C     OPKY          CHAIN     POP02                              99
     C     *IN99         IFEQ      *ON
     C                   CLEAR                   POP02
     C*
     C                   MOVEL(P)  KKMNAM        OPMNAM
     C                   MOVEL(P)  KKOPTN        OPOPTN
     C*
     C                   MOVEL(P)  'N'           OPMYN
     C                   MOVEL(P)  'LIV'         OPAPP
     C                   MOVEL(P)  CMD3          OPEXCL
     C                   MOVEL(P)  'Y'           OPYNO
     C                   Z-ADD     999           OPMINS
     C                   MOVEL(P)  '*'           OPTY01
     C                   MOVEL(P)  NAM3          OPMNTT
     C*
     C                   MOVEL(P)  'A'           OPSTAT
     C                   MOVEL(P)  Q1USER        OPUSER
     C                   MOVEL(P)  Q1PGM         OPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        OPTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OPDT
     C*
     C                   WRITE     POP02
     C                   ENDIF
     C*
     C                   ENDSR
