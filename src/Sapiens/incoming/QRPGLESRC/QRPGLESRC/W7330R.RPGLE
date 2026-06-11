     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7330R - EMPLOYER LABELS-PROMPT FOR FUND, SORT, SELECTION
     F*
     F* Prompt Screen File
     FW7330F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F*
     F* Fund Master File
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F*
     F* Company Master File
     FSMCOL     IF   E           K DISK
     F*
     F* User parameter Master File
     FWMUPL     IF   E           K DISK
     FWMEMP     IF   E             DISK
     FWTCNP     IF   E             DISK
     FWMAAP     IF   E             DISK
     D***PY SCOPYWW01I
      /COPY SCOPYWW01D
     D*
     D* System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* Screen Refernece
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5

     I*
     I*

     C*
     C* Key List for the Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C* Key List for the User Parameter Master File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* Key List for the Fund Master File
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
     C*
     C* Key List for the Fund Detail File                  LJQ 9-21-88
     C*                                                          ¦
     C     F1KEY         KLIST                                                        ¦
     C                   KFLD                    UPCO#                                ¦
     C                   KFLD                    XXFND                                ¦
     C                   KFLD                    XXFYR             3 0                ¦
     C*
     C     *LIKE         DEFINE    SCFEMP        XXEMP#
     C     *LIKE         DEFINE    SCFRGN        XXRGN#
      *
     C                   Z-ADD     0             XXFRDT            7 0
     C                   Z-ADD     0             XXTODT            7 0
     C                   Z-ADD     0             XXRANG            1 0
     C*
     C* Entry Parameters
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
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C* Get the User Parameter Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C* Get the Company Record
     C*
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   MOVE      UPFND         SCFND
     C*
     C* Get the Requested Record, Load Information
     C*
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C*
     C* Set-up and Initialize
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      *BLANK        SCSRT1
     C                   MOVE      *BLANK        SCSRT2
     C                   Z-ADD     0             SCFEMP
     C                   Z-ADD     0             SCTEMP
     C                   Z-ADD     0             SCFRGN
     C                   Z-ADD     0             SCTRGN
     C                   MOVE      *BLANKS       SCSLCT
     C                   MOVE      *BLANKS       SCTYPE
     C                   MOVE      *BLANKS       SCFCNT
     C                   MOVE      *BLANKS       SCTCNT
     C                   Z-ADD     0             SCFCHP
     C                   Z-ADD     0             SCTCHP
     C                   Z-ADD     1             SCNUM
      *
      * Center Screen Prompt in display field
      *
     C                   MOVEL(P)  SCPRDS        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCPRDS
     C*
     C* Load the Record Format Information for the First Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C* Display all Formats
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C* Process all Command Keys
     C*
     C     *IN11         IFEQ      '1'
     C*
     C* F3 = EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
      *
      * CONVERT THE FROM DATE
      *
     C                   MOVE      SCFRDT        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTMDY8
      *
     C                   MOVEL     SRDTO         SCFRDT
      *
      * CONVERT THE TO DATE
      *
     C                   MOVE      SCTODT        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTMDY8
      *
     C                   MOVEL     SRDTO         SCTODT
     C*
     C* PROCESS HELP REQUESTS
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
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END
      *
      * F10 = SEARCH
      *
     C     *INKJ         IFEQ      *ON
      *
      * EMPLOYER # SEARCH
      *
     C     HFLD          IFEQ      'SCFEMP'
     C     HFLD          OREQ      'SCTEMP'
      *
     C                   CALL      'W0050R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##MODE            1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      DBRRN         ##RRN2            9 0
     C                   PARM      'W'           ##QTE             1
     C                   PARM      2             ##VIEW            1 0
     C                   PARM      0             ##RRN3            9 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         IFEQ      *OFF
      *
     C     ##RRN         CHAIN     PEM01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     HFLD          IFEQ      'SCFEMP'
     C                   MOVE      EMEMP#        SCFEMP
     C                   MOVE      *ON           *IN30
     C                   ELSE
     C                   MOVE      EMEMP#        SCTEMP
     C                   MOVE      *ON           *IN39
     C                   ENDIF                                                  HFLD IFEQ
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ELSE
      *
     C     HFLD          IFEQ      'SCFEMP'
     C                   MOVE      *ON           *IN29
     C                   ELSE
     C                   MOVE      *ON           *IN30
     C                   ENDIF                                                  HFLD IFEQ
      *
     C                   ENDIF                                                  *INKL IFEQ
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  HFLD IFEQ
      *
      * AGENT SEARCH
      *
     C     HFLD          IFEQ      'SCFAGT'
     C     HFLD          OREQ      'SCTAGT'
      *
     C                   CALL      'W0040R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      DBRRN         ##RRN             9 0
     C                   PARM      2             ##VIEW            1 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         IFNE      *ON
     C     ##RRN         ANDNE     *ZEROS
      *
     C     ##RRN         CHAIN     PAA01                              99
      *
     C     HFLD          IFEQ      'SCFAGT'
     C                   MOVEL     AAAGT#        SCFAGT
     C                   MOVE      *ON           *IN40
     C                   ELSE
     C                   MOVEL     AAAGT#        SCTAGT
     C                   MOVE      *ON           *IN31
     C                   ENDIF                                                  HFLD IFEQ
      *
     C                   ELSE
      *
     C     HFLD          IFEQ      'SCFAGT'
     C                   MOVE      *ON           *IN39
     C                   ELSE
     C                   MOVE      *ON           *IN40
     C                   ENDIF                                                  HFLD IFEQ
      *
     C                   ENDIF                                                  QQRTCD IFNE
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  HFLD IFEQ
      *
      * REGION SEARCH
      *
     C     HFLD          IFEQ      'SCFRGN'
     C     HFLD          OREQ      'SCTRGN'
     C*
     C                   CALL      'W0520R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##MODE            1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      DBRRN         ##RRN2            9 0
     C     XXRGN#        PARM                    ##RGN#            3 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         IFEQ      *OFF
      *
     C     HFLD          IFEQ      'SCFRGN'
     C                   MOVE      XXRGN#        SCFRGN
     C                   MOVE      *ON           *IN36
     C                   ELSE
     C                   MOVE      XXRGN#        SCTRGN
     C                   MOVE      *ON           *IN48
     C                   ENDIF                                                  HFLD IFEQ
      *
     C                   ELSE
      *
     C     HFLD          IFEQ      'SCFRGN'
     C                   MOVE      *ON           *IN35
     C                   ELSE
     C                   MOVE      *ON           *IN36
     C                   ENDIF                                                  HFLD IFEQ
      *
     C                   ENDIF                                                  *INKL IFEQ
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  HFLD IFEQ
      *
      * COUNTY CODE SEARCH
      *
     C     HFLD          IFEQ      'SCFCNT'
     C     HFLD          OREQ      'SCTCNT'
      *
     C                   CALL      'W2215R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##MODE            1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      *BLANKS       ##ST              2
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         IFEQ      *OFF
      *
     C     ##RRN         CHAIN     PCN01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     HFLD          IFEQ      'SCFCNT'
     C                   MOVEL     CNKEY         SCFCNT
     C                   MOVE      *ON           *IN32
     C                   ELSE
     C                   MOVEL     CNKEY         SCTCNT
     C                   MOVE      *ON           *IN33
     C                   ENDIF                                                  HFLD IFEQ
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ELSE
      *
     C     HFLD          IFEQ      'SCFCNT'
     C                   MOVE      *ON           *IN31
     C                   ELSE
     C                   MOVE      *ON           *IN32
     C                   ENDIF                                                  HFLD IFEQ
      *
     C                   ENDIF                                                  *INKL IFEQ
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  HFLD IFEQ
      *
      * CHAPTER SEARCH
      *
     C     HFLD          IFEQ      'SCFCHP'
     C     HFLD          OREQ      'SCTCHP'
      *
     C                   CALL      'W0720R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##MODE            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      DBRRN         ##RRN2            9 0
     C                   PARM                    ##CHP             3 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         IFEQ      *OFF
      *
     C     HFLD          IFEQ      'SCFCHP'
     C                   MOVE      ##CHP         SCFCHP
     C                   MOVE      *ON           *IN34
     C                   ELSE
     C                   MOVE      ##CHP         SCTCHP
     C                   MOVE      *ON           *IN35
     C                   ENDIF                                                  HFLD IFEQ
      *
     C                   ELSE
      *
     C     HFLD          IFEQ      'SCFCHP'
     C                   MOVE      *ON           *IN33
     C                   ELSE
     C                   MOVE      *ON           *IN34
     C                   ENDIF                                                  HFLD IFEQ
      *
     C                   ENDIF                                                  *INKL IFEQ
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  HFLD IFEQ
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  *INKJ IFEQ
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   GOTO      STRT01
     C                   END
      *
     C                   ENDIF                                                  *IN11 IFEQ
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
      *
      * VALIDATE THE FROM DATE
      *
     C                   MOVE      SCFRDT        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTMDY8
      *
     C                   MOVEL     SRDTO         SCFRDT
      *
     C     SRERR         CABEQ     *ON           STRT                     27
      *
      * VALIDATE THE TO DATE
      *
     C                   MOVE      SCTODT        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTMDY8
      *
     C                   MOVEL     SRDTO         SCTODT
      *
     C     SRERR         CABEQ     *ON           STRT                     37
      *
      * FROM DATE MUST BE LESS THAN TO DATE
      *
     C                   MOVE      SCFRDT        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTCYMD
      *
     C                   MOVEL     SRDTO         XXFRDT
     C                   MOVE      SCTODT        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTCYMD
      *
     C                   MOVEL     SRDTO         XXTODT
      *
     C     XXFRDT        CABGT     XXTODT        STRT                 38
      *
      *  NUMBER OF COPIES MUST BE BETWEEN 1 & 9.
      *
     C     SCNUM         IFLT      1
     C     SCNUM         ORGT      9
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
      *
      * VALIDATE LABEL SELECTION.  'R'=REGULAR; 'L'=LOGO
      *
     C     SCTYPE        IFNE      'R'
     C     SCTYPE        ANDNE     'L'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
      *
      * VALIDATE FORM SELECTION.  'L'=LASER; 'P'=PINFEED
      *
     C     SCFORM        IFNE      'L'
     C     SCFORM        ANDNE     'P'
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFORM IFNE
      *
      * VALIDATE FORM SELECTION.  LASER NOT ALLOWED WITH LOGO FORMS
      *
     C     SCFORM        IFEQ      'L'
     C     SCTYPE        ANDEQ     'L'
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFORM IFEQ
      *
      * PRIMARY SORT MUST BE EQUAL TO 'E', 'A', 'C', 'H', 'R' OR 'Z'
      *
     C     SCSRT1        IFNE      'E'
     C     SCSRT1        ANDNE     'A'
     C     SCSRT1        ANDNE     'R'
     C     SCSRT1        ANDNE     'C'
     C     SCSRT1        ANDNE     'H'
     C     SCSRT1        ANDNE     'Z'
      *
     C                   MOVE      '1'           *IN20
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SCSRT1 IFNE 'E'
      *
      * SECONDARY SORT MUST BE EQUAL TO 'E', 'A', 'C', 'H', 'R', 'Z' OR BLANK
      *
     C     SCSRT2        IFNE      'E'
     C     SCSRT2        ANDNE     'A'
     C     SCSRT2        ANDNE     'R'
     C     SCSRT2        ANDNE     'C'
     C     SCSRT2        ANDNE     'H'
     C     SCSRT2        ANDNE     'Z'
     C     SCSRT2        ANDNE     *BLANK
      *
     C                   MOVE      *ON           *IN24
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SCSRT1 IFNE 'E'
      *
      * VALIDATE EMPLOYER SELECTION.  'A' = ACTIVE EMPLOYERS; 'C' =
      * CANCELLED EMPLOYERS AND 'B = ALL EMPLOYERS
      *
     C     SCSLCT        IFNE      'A'
     C     SCSLCT        ANDNE     'C'
     C     SCSLCT        ANDNE     'B'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
      *
      * VALIDATE ACCOUNT TYPE
      *
     C     SCATYP        IFNE      'E'
     C     SCATYP        ANDNE     'S'
     C     SCATYP        ANDNE     'B'
      *
     C                   MOVE      '1'           *IN46
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SCATYP IFNE
     C*** BEG ADD *** 07/26/04 ****************************
      *
      * VALIDATE POLICY/FEDERAL NAME
      *
     C     SCPOL#        IFNE      'P'
     C     SCPOL#        ANDNE     'F'
     C                   MOVE      '1'           *IN49
     C                   GOTO      STRT
     C                   ENDIF
     C*** END ADD *** 07/26/04 ****************************
      *
      * ONLY TWO RANGES MAY BE SELECTED AT ONE TIME
      *
     C                   Z-ADD     0             XXRANG
      *
     C     SCFEMP        IFNE      0
     C     SCTEMP        ORNE      0
     C                   ADD       1             XXRANG
     C                   ENDIF                                                  SCFEMP IFNE 0
      *
     C     SCFAGT        IFNE      0
     C     SCTAGT        ORNE      0
     C                   ADD       1             XXRANG
     C                   ENDIF                                                  SCFAGT IFNE 0
      *
     C     SCFRGN        IFNE      0
     C     SCTRGN        ORNE      0
     C                   ADD       1             XXRANG
     C                   ENDIF                                                  SCFRGN IFNE 0
      *
     C     SCFCHP        IFNE      0
     C     SCTCHP        ORNE      0
     C                   ADD       1             XXRANG
     C                   ENDIF                                                  SCFCHP IFNE 0
      *
     C     SCFCNT        IFNE      *BLANKS
     C     SCTCNT        ORNE      *BLANKS
     C                   ADD       1             XXRANG
     C                   ENDIF                                                  SCFCNT IFNE
      *
     C     SCFZIP        IFNE      0
     C     SCTZIP        ORNE      0
     C                   ADD       1             XXRANG
     C                   ENDIF                                                  SCFZIP IFNE 0
      *
     C     XXRANG        IFGT      2
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF                                                  XXRANG IFGT 2
      *
      *
      * FROM VALUE CANNOT BE GREATER THAN TO VALUE
      *
     C     SCFEMP        IFGT      SCTEMP
     C                   MOVE      *ON           *IN41
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFEMP IFGT
      *
     C     SCFAGT        IFGT      SCTAGT
     C                   MOVE      *ON           *IN42
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFAGT IFGT
      *
     C     SCFCNT        IFGT      SCTCNT
     C                   MOVE      *ON           *IN43
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFCNT IFGT
      *
     C     SCFCHP        IFGT      SCTCHP
     C                   MOVE      *ON           *IN44
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFCHP IFGT
      *
     C     SCFRGN        IFGT      SCTRGN
     C                   MOVE      *ON           *IN45
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFRGN IFGT
      *
     C     SCFZIP        IFGT      SCTZIP
     C                   MOVE      *ON           *IN47
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFZIP IFGT
      *
      *   CALL THE CLP TO SUBMIT THE LIST.
      *
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFRDT        QQFRDT            8
     C                   MOVE      SCTODT        QQTODT            8
     C                   MOVE      SCNUM         QQNUM             1
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   MOVE      SCSRT1        QQSRT1            1
     C                   MOVE      SCSRT2        QQSRT2            1
     C                   MOVE      SCSLCT        QQSLCT            1
     C                   MOVE      SCATYP        QQATYP            1
     C                   MOVE      SCFEMP        QQFEMP            9
     C                   MOVE      SCTEMP        QQTEMP            9
     C                   MOVE      SCFAGT        QQFAGT            5
     C                   MOVE      SCTAGT        QQTAGT            5
     C                   MOVE      SCFCNT        QQFCNT            4
     C                   MOVE      SCTCNT        QQTCNT            4
     C                   MOVE      SCFCHP        QQFCHP            3
     C                   MOVE      SCTCHP        QQTCHP            3
     C                   MOVE      SCFRGN        QQFRGN            3
     C                   MOVE      SCTRGN        QQTRGN            3
     C                   MOVE      SCFZIP        QQFZIP            5
     C                   MOVE      SCTZIP        QQTZIP            5
     C                   MOVE      SCTYPE        QQTYPE            1
     C                   MOVE      SCFORM        QQFORM            1
     C                   CALL      'W7330C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFRDT
     C                   PARM                    QQTODT
     C                   PARM                    QQNUM
     C                   PARM                    QQOUTQ
     C                   PARM                    QQSRT1
     C                   PARM                    QQSRT2
     C                   PARM                    QQSLCT
     C                   PARM                    QQATYP
     C                   PARM                    QQFEMP
     C                   PARM                    QQTEMP
     C                   PARM                    QQFAGT
     C                   PARM                    QQTAGT
     C                   PARM                    QQFCNT
     C                   PARM                    QQTCNT
     C                   PARM                    QQFCHP
     C                   PARM                    QQTCHP
     C                   PARM                    QQFRGN
     C                   PARM                    QQTRGN
     C                   PARM                    QQFZIP
     C                   PARM                    QQTZIP
     C                   PARM                    QQTYPE
     C                   PARM                    QQFORM
     C*** BEG ADD *** 07/26/04 ****************************
     C                   PARM      SCPOL#        QQPOL#            1
     C*** END ADD *** 07/26/04 ****************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
