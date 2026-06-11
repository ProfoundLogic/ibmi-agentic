     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F9711R1FL                                         *
     H*    DESCRIPTION: CREATE EOMB WORKFILE                          *
     H*****************************************************************
     H*  CREATED ON 6/21/2004 BY C:62 FOR PROJECT 51MFP               *
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*******************************************************************
     H*  FILES:                                                         *
     H*  FAEOL KEY LIST: CO/SUB#/FND/FYR/CASE/TRDT/FEIN/MOD/EOB#        *
     H*        OPEN QUERY SELECTION:    QQFRDT GE EOTRDT LE QQTODT      *
     H*                                 EOTAPE EQ 'Y', EASUB# EQ QQSUB# *
     H*                                 IF QQFND NE 000, QQFND EQ EOFND *
     H*******************************************************************
     FFTEOP     O    E           K DISK
     F                                     RENAME(PEO01:PEO02)
     FFAEOL     IF   E           K DISK
     FWMEML     IF A E           K DISK
     FWMCML1    IF A E           K DISK
     FFMFNL     IF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* External Data Structure
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* KEY LIST FOR THE WCMS CLAIM MASTER FILE
     C*
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
     C     CMKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    XXRFND            3 0
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C*
     C* KEY LIST FOR THE WCMS EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    XXRFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C*
     C*  FUND MASTER
     C*
     C     FNKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C*
     C                   TIME                    DSDTTM
     C*
     C                   READ      PEO01                                  50
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C* GET THE FUND MASTER RECORD
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     FNWCF#        XXRFND
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXRFND
     C                   END
     C*
     C* GET THE WCMS CLAIM MASTER RECORD
     C*
     C                   CLEAR                   PCM01
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     CMEMP#        XXEMP#
     C                   Z-ADD     *ZEROS        XXDIV
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXEMP#
     C                   Z-ADD     *ZEROS        XXDIV
     C                   END
     C*
     C* GET THE WCMS EMPLOYER FILE TO GET THE STATE EMPLOYER NUMBER
     C*
     C                   CLEAR                   PEM01
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     '00000'       XXWK20           20
     C                   MOVE      EMSEMP        XXWK20
     C     ' '           SCAN      XXWK20        I                 3 0
     C     I             IFEQ      0
     C                   MOVEL     EMSEMP        XXSEMP
     C                   ELSE                                                   I = 0
     C                   SUB       5             I
     C     5             SUBST     XXWK20:I      XXSEMP            5
     C                   ENDIF                                                  I = 0
     C                   MOVE      XXSEMP        EOSEMP
     C                   MOVEL     EMDBA         EODBA
     C                   END
     C*
     C* MOVE FIELDS TO WORKFILE FIELDS FOR WRITE OF WORKFILE RECORD
     C*
     C                   Z-ADD     EOCO#         EOCO#
     C                   Z-ADD     EOSUB#        EOSUB#
     C                   Z-ADD     EOFND         EOFND
     C                   Z-ADD     XXEMP#        EOEMP#
     C                   Z-ADD     EOFYR         EOFYR
     C                   Z-ADD     EOCASE        EOCASE
     C                   Z-ADD     EOEOB#        EOEOB#
     C                   Z-ADD     EOFEIN        EOFEIN
     C                   Z-ADD     EOMOD         EOMOD
     C                   Z-ADD     EOAMT         EOAMT
     C                   MOVEL     EODIA1        EODIA1
     C                   MOVEL     EODIA2        EODIA2
     C                   MOVEL     EODIA3        EODIA3
     C                   MOVEL     EODIA4        EODIA4
     C                   MOVEL     EOEMPL        EOEMPL
     C                   MOVEL     EODFC         EODFC
     C                   MOVEL     EOFLIC        EOFLIC
     C                   MOVEL     EOTAPE        EOTAPE
     C                   MOVE      Q1USER        EOUSER
     C                   MOVE      Q1PGM         EOPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   Z-ADD     DSTIME        EOTIME
     C                   MOVEL     Q1USER        EOIUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOIDT
     C                   MOVE      EMFEIN        EOEFID
     C                   CLEAR                   EOEZIP
     C     EMMPFL        IFEQ      'M'
     C                   MOVE      EMMZIP        EOEZIP
     C                   END
     C     EMMPFL        IFEQ      'P'
     C                   MOVE      EMPZIP        EOEZIP
     C                   END
     C*
     C                   WRITE     PEO02
     C*
     C* READ THE NEXT EOMB RECORD
     C*
     C                   READ      PEO01                                  50
     C*
     C                   END                                                    END DOW 50 EQ '0'
     C*
     C* END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     C* DUMMY SUBROUTINE
     C*****************************************************************
     C     DUMMY         BEGSR
     C                   WRITE     PCM01
     C                   WRITE     PEM01
     C                   ENDSR
