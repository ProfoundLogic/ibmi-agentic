     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE C0010R - REINSURANCE LAYOFF MASTER MAINTENANCE
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program is the Reinsurance Layoff Master Maint    **
     H**   CALLED FROM:                                             **
     H**     ______ - ___________________________                   **
     H**   CALLS TO:                                                **
     H**     ______ - ___________________________                   **
     H****************************************************************
     H*
     H****************************************************************
     H** REVISIONS:                                                 **
     H**  11/18/96 *** C:107 *** V2000 ***                          **
     H**           1. Version 2 format changes.                     **
     H****************************************************************
     H*
     H****************************************************************
     H** INDICATOR USAGE:                                           **
     H**   INDICATOR            FUNCTION DESCRIPTION                **
     H**      __       ___________________________                  **
     H****************************************************************
     H*
     H****************************************************************
     H** FILE USAGE:                                                **
     H**   FILES:                                                   **
     H**   _____  (_____): __________________________ (___)         **
     H****************************************************************
     H*
     H*
     F*****************************************************************
     F*                        File Specs                             *
     F*****************************************************************
     F*
     F*  Prompt Screen File
     F*
     FC0010F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F*
     F*  Company Master File
     F*
     FSMCOL     IF   E           K DISK
     F*
     F*  User Parameters File
     F*
     FCMUPL     IF   E           K DISK
     F*
     F*  Reinsurance Layoff File
     F*
     FCDRLL     UF A E           K DISK
     F*
     F*  Policy Master File
     F*
     FCMPML     IF   E           K DISK
     FCTABL     IF   E           K DISK
     F*

     D*****************************************************************
     D*                 Extension Specs - Define Table                *
     D*****************************************************************
     D*
     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D*
     D*****************************************************************
     D*                Input Specs - Define Data Structures           *
     D*****************************************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C*****************************************************************
     C*                       Calculation Specs                       *
     C*****************************************************************
     C*
     C*  Key List for the Comapny File
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  Key List for the User Parameter File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  Key List for the Reinsurance Layoff File
     C*
     C     RLKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXCDE             2
     C                   KFLD                    XXPOL#           13
     C                   KFLD                    XXEFDT            7 0
     C*
     C*  Key List for the Policy Master File
     C*
     C     PMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXPOL#
     C*
     C*  COVERAGE CODE TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
     C*  Get the User Parameter Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C                   Z-ADD     UPCO#         XXCO#
     C*
     C     TOP           TAG
     C*
     C*  Get the Company Record
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  Initialize and Set-up
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C* If program was called as inquiry (qqcall = 1) do not allow
     C*  update of the record.  If program is being called from another
     C*  program (qqcall = 2) do not allow change of user parms.
     C*
     C     QQCALL        COMP      '1'                                    09
     C     QQCALL        IFEQ      '2'
     C                   MOVE      '1'           *IN24
     C                   END
     C*
     C*  Load the Record Format Info for First Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCCODE
     C*
     C*  Display all Formats
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C*  Process All FUNCTION Keys
     C*
     C     *IN11         IFEQ      '1'
     C*
     C* FUNCTION 01 = End of Job
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C* FUNCTION 02 = Prior Screen
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C* FUNCTION 05 = Search List Call
     C*
     C     *INKE         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      *BLANKS       QQCDE             2            Coverage Code
     C                   MOVE      *BLANKS       QQPOL#           13            Policy #
     C                   MOVE      *BLANKS       QQEFDT            6            Effective Date
     C                   MOVEL     SCCODE        QQCDE
     C                   MOVEL     SCPOL#        QQPOL#
     C                   MOVE      SCEFDT        QQEFDT
     C                   CALL      'C0510R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCDE
     C                   PARM                    QQPOL#
     C                   PARM                    QQEFDT                         MM/DD/YY
     C                   PARM                    QQRTC             1
     C                   MOVEL     QQCDE         SCCODE
     C                   MOVEL     QQPOL#        SCPOL#
     C                   MOVE      QQEFDT        SCEFDT                         MM/DD/YY
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQCDE         OREQ      *BLANK
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 07 = SEARCH POLICY MASTER - POLICY ORDER - SCREEN 1
     C*
     C     *INKG         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   MOVE      *BLANK        QQPOL#
     C                   MOVE      SCPOL#        QQPOL#
     C                   CALL      'C0530R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC             1
     C                   MOVE      QQPOL#        SCPOL#
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQPOL#        OREQ      *BLANK
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C                   END
     C*
     C*  FUNCTION 09 = SEARCH POLICY MASTER BY NAME, SCR01
     C*
     C     *INKI         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      *BLANKS       QQPOL#
     C                   CALL      'C0531R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC             1
     C                   MOVE      QQPOL#        SCPOL#
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQPOL#        OREQ      *BLANK
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C                   END
     C*
     C*  TABLE FILE SEARCH  - COVERAGE CODE SCR01
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   MOVEL     SCCODE        XXQ               2
     C     XXQ           IFEQ      '? '
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   CALL      'C2001R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXKEY            10
     C                   PARM                    QQRTC             1
     C                   MOVEL     XXKEY         SCCODE
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C                   END
     C                   END
     C*
     C* FUNCTION 20- Change Status
     C*
     C     *INKU         IFEQ      '1'
     C     *IN40         ANDEQ     '0'
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*  Process HELP Requests
     C*
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*  FUNCTION 13 - Process Change of User Parameters
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'C0006R'
     C                   WRITE     SCR99
     C*
     C*  Get the User Parameter Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   Z-ADD     UPCO#         XXCO#
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C*
     C                   END
     C*
     C*  Goto the Appropriate Screen
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*  Process Request Screen
     C*
     C     SCR01T        TAG
     C*
     C*  The Reinsurance Layoff Section Code Cannot be Equal to *Blanks
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCCODE        XXKEY
     C     TBKEY         CHAIN     PCV01                              20
     C     *IN20         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C     CVSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*  The Policy number can be blank: if not blank; it must be valid
     C*
     C     SCPOL#        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXPOL#
     C                   Z-ADD     UPCO#         XXCO#
     C                   MOVEL     SCPOL#        XXPOL#
     C     PMKEY         CHAIN     PPM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  Validate the Effective Date
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  Get the Requested Record, Load Information
     C*
     C                   MOVE      *BLANKS       XXCDE
     C                   MOVE      *BLANKS       XXPOL#
     C                   MOVE      *BLANKS       XXEFDT
     C                   Z-ADD     UPCO#         XXCO#
     C                   MOVEL     SCCODE        XXCDE
     C                   MOVEL     SCPOL#        XXPOL#
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C     RLKEY         CHAIN     PRL01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PRL01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVEL     RLNAM1        SCNAM1
     C                   MOVEL     RLCON1        SCCON1
     C                   Z-ADD     RLIMI1        SCIMI1
     C                   Z-ADD     RLPCT1        SCPCT1
     C                   MOVEL     RLNAM2        SCNAM2
     C                   MOVEL     RLCON2        SCCON2
     C                   Z-ADD     RLIMI2        SCIMI2
     C                   Z-ADD     RLPCT2        SCPCT2
     C                   MOVEL     RLNAM3        SCNAM3
     C                   MOVEL     RLCON3        SCCON3
     C                   Z-ADD     RLIMI3        SCIMI3
     C                   Z-ADD     RLPCT3        SCPCT3
     C                   MOVEL     RLNAM4        SCNAM4
     C                   MOVEL     RLCON4        SCCON4
     C                   Z-ADD     RLIMI4        SCIMI4
     C                   Z-ADD     RLPCT4        SCPCT4
     C                   MOVE      RLSTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANKS       SCNAM1
     C                   MOVE      *BLANKS       SCCON1
     C                   Z-ADD     0             SCIMI1
     C                   Z-ADD     0             SCPCT1
     C                   MOVE      *BLANKS       SCNAM2
     C                   MOVE      *BLANKS       SCCON2
     C                   Z-ADD     0             SCIMI2
     C                   Z-ADD     0             SCPCT2
     C                   MOVE      *BLANKS       SCNAM3
     C                   MOVE      *BLANKS       SCCON3
     C                   Z-ADD     0             SCIMI3
     C                   Z-ADD     0             SCPCT3
     C                   MOVE      *BLANKS       SCNAM4
     C                   MOVE      *BLANKS       SCCON4
     C                   Z-ADD     0             SCIMI4
     C                   Z-ADD     0             SCPCT4
     C                   MOVE      'A'           SCSTAT
     C*
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C*  All Edits have been Passed
     C*
     C     *IN09         IFEQ      '0'
     C                   MOVE      *BLANKS       XXCDE
     C                   MOVE      *BLANKS       XXPOL#
     C                   MOVE      *BLANKS       XXEFDT
     C                   Z-ADD     UPCO#         XXCO#
     C                   MOVEL     SCCODE        XXCDE
     C                   MOVEL     SCPOL#        XXPOL#
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C     RLKEY         CHAIN     PRL01                              40
     C                   Z-ADD     UPCO#         RLCO#
     C                   MOVEL     SCCODE        RLCDE
     C                   MOVEL     SCPOL#        RLPOL#
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RLEFDT
     C                   MOVEL     SCNAM1        RLNAM1
     C                   MOVEL     SCCON1        RLCON1
     C                   Z-ADD     SCIMI1        RLIMI1
     C                   Z-ADD     SCPCT1        RLPCT1
     C                   MOVEL     SCNAM2        RLNAM2
     C                   MOVEL     SCCON2        RLCON2
     C                   Z-ADD     SCIMI2        RLIMI2
     C                   Z-ADD     SCPCT2        RLPCT2
     C                   MOVEL     SCNAM3        RLNAM3
     C                   MOVEL     SCCON3        RLCON3
     C                   Z-ADD     SCIMI3        RLIMI3
     C                   Z-ADD     SCPCT3        RLPCT3
     C                   MOVEL     SCNAM4        RLNAM4
     C                   MOVEL     SCCON4        RLCON4
     C                   Z-ADD     SCIMI4        RLIMI4
     C                   Z-ADD     SCPCT4        RLPCT4
     C                   MOVE      SCSTAT        RLSTAT
     C                   MOVE      Q1USER        RLUSER
     C                   MOVE      Q1PGM         RLPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RLDT
     C                   Z-ADD     DSTIME        RLTIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PRL01
     C                   ELSE
     C                   WRITE     PRL01
     C                   END
     C*
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCCODE
     C                   MOVE      *BLANKS       SCPOL#
     C                   Z-ADD     0             SCEFDT
     C                   GOTO      STRT
     C*
     C*
     C*
     C*  End the Program
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     O*
     O*  Release record
     O*
     OPRL01     E            PRL01E
     O*
     O***
**  TABLE UA
-- ADD --
- UPDATE -
