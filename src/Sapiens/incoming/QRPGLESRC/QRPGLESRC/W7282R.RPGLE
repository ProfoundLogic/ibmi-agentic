     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7282R - Build Work File for PREMIUM DUE SELF REPORTING
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  10/10/95 C:11 PROJECT WC453                                  *
     H*                GET THE LAST ACTIVE AGENT AS OF THE FYR, NOT   *
     H*                AS OF THE RUN DATE.                            *
     H*****************************************************************
     FWXPDP     O    E             DISK
     FWDELL1    IF   E           K DISK
     FWMAHL     IF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Key List for the Employer DETAIL File
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C* Key List for the Agent History Master File
     C*
     C     AHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    XXFYR
     C*
     C     *LIKE         DEFINE    ELDT          XXYMD
     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQRPMY            6
     C                   PARM                    QQFAGT            5
     C                   PARM                    QQTAGT            5
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
     C* Set Up and Initialize
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVEL     QQRPMY        XXMMYY            6 0
     C*
     C                   Z-ADD     XXMMYY        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         XXYYMM            5 0
     C*
     C                   MOVEL     XXYYMM        XXYMD
     C                   MOVE      '01'          XXYMD
     C*
     C                   MOVE      QQFAGT        XXFAGT            5 0
     C                   MOVE      QQTAGT        XXTAGT            5 0
     C                   TIME                    DSDTTM
     C*
     C* Get the Employer DETAIL Record
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C* Get the Agent # & COMPARE ON RANGE
     C*
     C     AHKEY         SETGT     PAH01
     C                   READP     PAH01                                  50
     C     *IN50         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  50
     C                   END
     C*
     C     *IN50         IFEQ      '1'
     C     XXCO#         ORNE      AHCO#
     C     XXFND         ORNE      AHFND
     C     ELEMP#        ORNE      AHEMP#
     C     ELDIV         ORNE      AHDIV
     C     XXFYR         ORNE      AHFYR
     C                   Z-ADD     0             AHAGT#
     C                   END
     C*
     C     AHAGT#        IFGE      XXFAGT
     C     AHAGT#        ANDLE     XXTAGT
     C*
     C* Move Information to Work File Fields
     C*
     C                   Z-ADD     ELCO#         PDCO#
     C                   Z-ADD     ELFND         PDFND
     C                   Z-ADD     ELEMP#        PDEMP#
     C                   Z-ADD     ELDIV         PDDIV
     C                   Z-ADD     AHAGT#        PDAGT#
     C                   MOVE      *BLANKS       PDSTAT
     C                   MOVE      Q1USER        PDUSER
     C                   MOVE      Q1PGM         PDPGM
     C                   MOVE      DSTIME        PDTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PDDT
     C                   WRITE     PPD01
     C*
     C                   END
     C*
     C* Get the Next Employer
     C*
     C     ELKEY         READE     PEL01                                  60
     C                   END
     C*
     C                   MOVE      '1'           *INLR
