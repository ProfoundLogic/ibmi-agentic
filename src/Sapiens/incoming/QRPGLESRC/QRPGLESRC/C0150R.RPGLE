     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE C0150R - POLICY FORMS MAINTENANCE
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program is the Policy Forms Maintenance Pgm       **
     H**   CALLED FROM:                                             **
     H**     ______ - ___________________________                   **
     H**   CALLS TO:                                                **
     H**     ______ - ___________________________                   **
     H****************************************************************
     H*
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 08/25/01 C:15  Project: 94CAX                                 *
     H*               -Removed various edits to allow easier entry of *
     H*                policy.                                        *
     H*                                                               *
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
     FC0150F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FCMUPL     IF   E           K DISK
     FCMPML     IF   E           K DISK
     FCMP1L     UF A E           K DISK
     FCTABL     IF   E           K DISK
     F                                     IGNORE(PPC01)

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  POLICY MASTER FILE
     C*
     C     PMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C*
     C*  POLICY COVERAGE FILE
     C*
     C     PCKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    XXCC              2
     C*
     C*  TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQPOL#           13
     C                   PARM                    QQPNM            40
     C                   PARM                    QQCC              2
     C                   PARM                    QQRTCD            1
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
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      QQPOL#        SCPOL#
     C                   MOVE      QQCC          SCCC
     C                   MOVE      QQCC          XXCC
     C                   MOVEL     QQPNM         SCPNM
     C*
     C*   CHAIN OUT TO POLICY MASTER FOR COMMISSION RATE
     C*
     C     PMKEY         CHAIN     PPM01                              60
     C     *IN60         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C*   SET INDICATOR TO DISPLAY POLICY CANCELLED IF CANCELATION
     C*   FLAG(PMCANC) IS EQUAL TO "C"
     C*
     C     PMCANC        IFEQ      'C'
     C                   MOVE      '1'           *IN03
     C                   ELSE
     C                   MOVE      '0'           *IN03
     C                   END
     C*
     C*
     C*  GET DESC FOR COVERAGE CODE
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCCC          XXKEY
     C     TBKEY         CHAIN     PCV01                              60
     C                   MOVEL     CVDESC        SCCODS
     C*
     C     PCKEY         CHAIN     PPC01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PPC01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      PCFR01        SCFR01
     C                   MOVE      PCFR02        SCFR02
     C                   MOVE      PCFR03        SCFR03
     C                   MOVE      PCFR04        SCFR04
     C                   MOVE      PCFR05        SCFR05
     C                   MOVE      PCFR06        SCFR06
     C                   MOVE      PCFR07        SCFR07
     C                   MOVE      PCFR08        SCFR08
     C                   MOVE      PCFR09        SCFR09
     C                   MOVE      PCFR10        SCFR10
     C                   MOVE      PCCMNR        SCAGPC
     C                   MOVE      PCREIN        SCREIN
     C                   MOVE      PCRTID        SCRTID
     C                   MOVE      PCRTMF        SCRTMD
     C                   MOVE      PCRTDP        SCRTDP
     C                   MOVE      PCMINP        SCMNPR
     C                   MOVE      PCASLB        SCASLB
     C*
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANK        SCFR01
     C                   MOVE      *BLANK        SCFR02
     C                   MOVE      *BLANK        SCFR03
     C                   MOVE      *BLANK        SCFR04
     C                   MOVE      *BLANK        SCFR05
     C                   MOVE      *BLANK        SCFR06
     C                   MOVE      *BLANK        SCFR07
     C                   MOVE      *BLANK        SCFR08
     C                   MOVE      *BLANK        SCFR09
     C                   MOVE      *BLANK        SCFR10
     C                   Z-ADD     PMCMNR        SCAGPC
     C                   MOVE      *BLANK        SCREIN
     C*** Begin chg *** 08/25/01 **************************************
     C******************** MOVE *BLANK    SCRTID
     C                   MOVE      '1'           SCRTID
     C*** End chg ***** 08/25/01 **************************************
     C                   MOVE      *BLANK        SCRTMD
     C                   MOVE      *BLANK        SCRTDP
     C                   MOVE      'N'           SCMNPR
     C                   MOVE      *BLANK        SCASLB
     C*
     C                   END
     C*
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*   PROCESS ALL FUNCTION KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*  FUNCTION 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  FUNCTION 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  FUNCTION 03 = RESTART
     C*
     C     *INKA         IFEQ      '1'
     C                   MOVE      'C'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*   COMMISSION RATE MUST BE FROM 0.01 THRU 999.99
     C*
     C*** Begin chg *** 08/25/01 **************************************
     C********** SCAGPC    IFLT 0.01
     C     SCAGPC        IFLT      0
     C*** End chg ***** 08/25/01 **************************************
     C     SCAGPC        ORGT      999.99
     C                   MOVE      '1'           *IN37
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE REINSURANCE MUST BE F, T, OR BLANK
     C*
     C     SCREIN        IFNE      'F'
     C     SCREIN        ANDNE     'T'
     C     SCREIN        ANDNE     *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*   RATE ID MUST BE VALID FOR SECTION
     C*
     C*   FIRE SECTION
     C*
     C     CVVAL         IFEQ      'C'
     C     SCRTID        IFNE      '1'
     C     SCRTID        ANDNE     '2'
     C     SCRTID        ANDNE     '3'
     C     SCRTID        ANDNE     '4'
     C     SCRTID        ANDNE     '5'
     C     SCRTID        ANDNE     '6'
     C     SCRTID        ANDNE     '7'
     C     SCRTID        ANDNE     '8'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   GENERAL LIABILITY AND AUTO SECTION
     C*
     C     CVVAL         IFEQ      'L'
     C     CVVAL         OREQ      'A'
     C     SCRTID        IFNE      '1'
     C     SCRTID        ANDNE     '9'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   BOILER AND MACHINERY SECTION
     C*
     C     CVVAL         IFEQ      'B'
     C     SCRTID        IFNE      '1'
     C     SCRTID        ANDNE     '9'
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   BUSINESSOWNERS SECTION
     C*
     C     CVVAL         IFEQ      'O'
     C     SCRTID        IFNE      '4'
     C     SCRTID        ANDNE     '5'
     C                   MOVE      '1'           *IN34
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   RATE DEPARTURE MUST BE FROM 1 THRU 999
     C*
     C*** Begin chg *** 08/25/01 **************************************
     C********** SCRTDP    IFLT 1
     C     SCRTDP        IFLT      0
     C*** End chg ***** 08/25/01 **************************************
     C     SCRTDP        ORGT      999
     C                   MOVE      '1'           *IN35
     C                   GOTO      STRT
     C                   END
     C*
     C*   RATE MOD MUST BE FROM 1 THRU 999
     C*
     C*** Begin chg *** 08/25/01 **************************************
     C********** SCRTMD    IFLT 1
     C     SCRTMD        IFLT      0
     C*** End chg ***** 08/25/01 **************************************
     C     SCRTMD        ORGT      999
     C                   MOVE      '1'           *IN36
     C                   GOTO      STRT
     C                   END
     C*
     C*   MINIMUM PREMIUM MUST BE Y OR N.
     C*
     C     SCMNPR        IFNE      'Y'
     C     SCMNPR        ANDNE     'N'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE ANNUAL STATEMENT LINE OF BUSINESS DEPENDING
     C*   UPON VALUE OF SECTION CODE
     C*
     C*** Begin add *** 08/25/01 **************************************
     C     SCASLB        IFNE      *BLANK
     C*** End add ***** 08/25/01 **************************************
     C*
     C*   FIRE SECTION
     C*
     C     CVVAL         IFEQ      'C'
     C     SCASLB        IFNE      '010'
     C     SCASLB        ANDNE     '020'
     C     SCASLB        ANDNE     '050'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   GENERAL LIABILITY SECTION
     C*
     C     CVVAL         IFEQ      'L'
     C     SCASLB        IFNE      '170'
     C     SCASLB        ANDNE     '050'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   BURGLARY AND THEFT SECTION
     C*
     C     CVVAL         IFEQ      'T'
     C     SCASLB        IFNE      '260'
     C     SCASLB        ANDNE     '050'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   FIDELITY AND FORGERY SECTION
     C*
     C     CVVAL         IFEQ      'F'
     C     SCASLB        IFNE      '230'
     C     SCASLB        ANDNE     '050'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   GLASS SECTION
     C*
     C     CVVAL         IFEQ      'G'
     C     SCASLB        IFNE      '250'
     C     SCASLB        ANDNE     '050'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   INLAND MARINE SECTION
     C*
     C     CVVAL         IFEQ      'I'
     C     SCASLB        IFNE      '090'
     C     SCASLB        ANDNE     '050'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   BOILER MACHINERY SECTION
     C*
     C     CVVAL         IFEQ      'B'
     C     SCASLB        IFNE      '270'
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   BUSINESS OWNERS SECTION
     C*
     C     CVVAL         IFEQ      'O'
     C     SCASLB        IFNE      '050'
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*** Begin add *** 08/25/01 **************************************
     C                   END
     C*** End add ***** 08/25/01 **************************************
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C     *IN09         IFEQ      '0'
     C     PCKEY         CHAIN     PPC01                              40
     C                   MOVE      UPCO#         PCCO#
     C                   MOVE      SCPOL#        PCPOL#
     C                   MOVE      XXCC          PCCC
     C                   MOVE      SCFR01        PCFR01
     C                   MOVE      SCFR02        PCFR02
     C                   MOVE      SCFR03        PCFR03
     C                   MOVE      SCFR04        PCFR04
     C                   MOVE      SCFR05        PCFR05
     C                   MOVE      SCFR06        PCFR06
     C                   MOVE      SCFR07        PCFR07
     C                   MOVE      SCFR08        PCFR08
     C                   MOVE      SCFR09        PCFR09
     C                   MOVE      SCFR10        PCFR10
     C                   MOVE      SCAGPC        PCCMNR
     C                   MOVE      SCREIN        PCREIN
     C                   MOVE      SCRTID        PCRTID
     C                   MOVE      SCRTMD        PCRTMF
     C                   MOVE      SCRTDP        PCRTDP
     C                   MOVE      SCMNPR        PCMINP
     C                   MOVE      SCASLB        PCASLB
     C                   MOVE      *BLANKS       PCSTAT
     C                   MOVE      Q1USER        PCUSER
     C                   MOVE      Q1PGM         PCPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PCDT
     C                   Z-ADD     DSTIME        PCTIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PPC01
     C                   ELSE
     C                   WRITE     PPC01
     C                   END
     C*
     C                   END
     C*
     C                   MOVE      '*'           QQRTCD
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPPC01     E            PPC01E
**  TABLE UA
-- ADD --
- UPDATE -
