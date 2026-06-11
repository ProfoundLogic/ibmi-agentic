     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWMOOP - SEQUENCES THE OWNERS/OFFICERS IN WMOOP
     H*****************************************************************
     H*                                                               *
     H*  CREATED: 06/10/98  C:77  PROJECT SUPPORT(WA091 #12193)       *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  06/22/98  C:33  SEQ#: WA091/12339                            *
     H*                  LOAD THE DATE CORRECTLY                      *
     H*****************************************************************
     FWMOOL7    UF   E           K DISK

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C* AGENT MASTER FILE SEARCH KEY
     C*
     C     OOKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXZERO                         (SEQ# = 0)
     C*
     C     OOKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C***  BEGIN ADD  ***  06/22/98  *********************************
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C***  END   ADD  ***  06/22/98  *********************************
     C*
     C     *LIKE         DEFINE    OOCO#         XXCO#
     C     *LIKE         DEFINE    OOFND         XXFND
     C     *LIKE         DEFINE    OOEMP#        XXEMP#
     C     *LIKE         DEFINE    OODIV         XXDIV
     C     *LIKE         DEFINE    OOSEQ#        XXSEQ#
     C     *LIKE         DEFINE    OOSEQ#        XXZERO
     C***  BEGIN ADD  ***  06/22/98  *********************************
     C     *LIKE         DEFINE    OODT          X7DATE
     C***  END   ADD  ***  06/22/98  *********************************
     C*
     C                   Z-ADD     0             XXZERO
     C*
     C                   TIME                    DSDTTM
     C*
     C***  BEGIN ADD  ***  06/22/98  *********************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7DATE
     C***  END   ADD  ***  06/22/98  *********************************
     C*
     C                   Z-ADD     0             XXCO#
     C                   Z-ADD     0             XXFND
     C                   Z-ADD     0             XXEMP#
     C                   Z-ADD     0             XXDIV
     C*
     C*  ZERO OUT ALL SEQUENCE NUMBERS
     C*
     C     OOKY1         SETLL     POO01
     C                   READ      POO01                                  60
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     0             OOSEQ#
     C                   UPDATE    POO01
     C                   READ      POO01                                  60
     C                   ENDDO
     C*
     C*  SEQUENCE THE OWNER/OFFICER RECS
     C*
     C     OOKY          SETLL     POO01
     C                   READ      POO01                                  60
     C*
     C     *IN60         DOWEQ     *OFF                                         WH NOT EOF
     C*
     C                   Z-ADD     OOCO#         XXCO#
     C                   Z-ADD     OOFND         XXFND
     C                   Z-ADD     OOEMP#        XXEMP#
     C                   Z-ADD     OODIV         XXDIV
     C*
     C                   Z-ADD     0             XXSEQ#
     C*
     C     OOKY          CHAIN     POO01                              61
     C*
     C     *IN61         DOWEQ     '0'
     C*
     C     1             ADD       XXSEQ#        OOSEQ#
     C                   ADD       1             XXSEQ#
     C*
     C***  BEGIN CHG  ***  06/22/98  *********************************
     C********** DSDATE    MULT 10000.01  OODT
     C                   Z-ADD     X7DATE        OODT
     C***  END   CHG  ***  06/22/98  *********************************
     C*
     C                   Z-ADD     DSTIME        OOTIME
     C                   MOVEL(P)  Q1PGM         OOPGM
     C                   MOVEL(P)  Q1USER        OOUSER
     C*
     C                   UPDATE    POO01
     C*
     C     OOKY          CHAIN     POO01                              61
     C                   ENDDO                                                  *IN61 = '0'
     C*
     C     OOKY1         SETGT     POO01
     C                   READ      POO01                                  60
     C*
     C                   ENDDO                                                  *IN60 = '0'
     C*
     C                   MOVE      *ON           *INLR
