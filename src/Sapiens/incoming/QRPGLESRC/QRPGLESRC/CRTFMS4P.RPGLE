     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  CRTFMS4P- READ THROUGH FMPDP AND CREATE       C:62 06/01/05  *
     H*            NETWORK CONTRACTS.                                 *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 11/02/09 C:232 PROJECT: 139WMR                                *
     H*                OFAC Reporting-Add Pending Status Indicator.   *
     H*                                                               *
     H*****************************************************************
     FFMS4P     O    E             DISK
     FFMPDP     IF   E             DISK
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   READ      PPD01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     PDTYPE        IFEQ      '1'
     C     PDTYPE        OREQ      '4'
     C*** BEG CHG *** 11/02/09 **************************************
     C****       PDSTAT    IFNE 'I'
     C     PDSTAT        IFEQ      'A'
     C     PDSTAT        OREQ      *BLANKS
     C*** END CHG *** 11/02/09 **************************************
     C*
     C                   Z-ADD     PDFEIN        S4FEIN
     C                   Z-ADD     PDMOD         S4MOD
     C                   MOVEL     'PPC'         S4NWCD
     C                   MOVEL     'P'           S4SYS
     C                   Z-ADD     0900101       S4BGDT
     C                   MOVEL     'N00'         S4DISC
     C                   MOVEL     'N'           S4FSFL
     C*
     C                   MOVEL     'A'           S4STAT
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         S4DT
     C                   MOVEL     SRDTO         S4IDT
     C                   Z-ADD     DSTIME        S4TIME
     C                   Z-ADD     DSTIME        S4ITIM
     C*
     C                   MOVEL     Q1USER        S4USER
     C                   MOVEL     Q1USER        S4IUSR
     C                   MOVEL     Q1PGM         S4PGM
     C                   MOVEL     Q1PGM         S4IPGM
     C*
     C                   WRITE     PS401
     C*
     C                   MOVEL     'H'           S4SYS
     C*
     C                   WRITE     PS401
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   READ      PPD01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
