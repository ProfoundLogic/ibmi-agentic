     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CS040R - CLIENT MASTER FILE MAINTENANCE
     H******************************************************************
     H* INDICATOR USAGE                                                *
     H*  09    INQUIRY ONLY                                            *
     H*  40    CHAIN ON CMCTL/PCT01                                    *
     H*  60    GENERAL USAGE/CHAIN                                     *
     H* SCR01:                                                         *
     H*  20    SCM0504- CLIENT NUMBER REQUIRED                         *
     H*  21    CANNOT INQUIRE ON AN ADD                  *
     H* SCR02:                                                         *
     H*  20    SCM0510- MUST BE M FOR MEMBER OR N FOR NON-MEMBER       *
     H*  21    SCM0503- NAME REQUIRED                                  *
     H*  22    SCM0503- ADDRESS 1 REQUIRED                             *
     H*  23    SCM0503- CITY REQUIRED                                  *
     H*  24    SCM0501- STATE CODE MUST BE VALID                       *
     H*  25    SCM0504- ZIP CODE 1 REQUIRED                            *
     H*  26    SCM0503- CONTACT NAME REQUIRED                          *
     H*  27    SCM0504- TELEPHONE NUMBER REQUIRED                      *
     H*  28    POS CURSOR AT ZIP1 AFTER TABLE SEARCH ON STATE          *
     H*  64    DELETE NOT ALLOWED                                      *
     H*****************************************************************
     H* REVISIONS:
     H* 08/09/94  C:26  Project: CS004
     H*        1. System to automatically assign next availabl         e
     H*           Client#, which is stored in SCSPF/CSDT01
     H*        2. Added new search to SCR01: F6=Client Alpha S         earch
     H******************************************************************
     FCS040F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FCMCTL1    UF A E           K DISK
     FSTABL     IF   E           K DISK
     FSSYLF     UF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK

     D UA              S             10    DIM(3) CTDATA PERRCD(1)              ADD/UPD/INQ MSG
     D CD              S             20    DIM(1) CTDATA PERRCD(1)              F23 MSG
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
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
     C*   KEY LIST FOR CARRIER MASTER FILE
     C     CTKEY         KLIST
     C                   KFLD                    SCCLT#
     C*
     C     *LIKE         DEFINE    STKEY         STKEY1
     C     *LIKE         DEFINE    CSKEY         CSKEY1
     C*
     C                   TIME                    DSDTTM
     C*
     C*   ENTRY PARM LIST
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
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   ELSE
     C                   Z-ADD     UPCO#         QQCO#             3 0
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
     C*
     C     QQCALL        IFEQ      '7'
     C                   MOVE      *BLANK        QQCALL
     C*  SYSTEM WILL ASSIGN NEXT AVAILABLE CLIENT NUMBER
     C                   MOVEL     'CS'          CSKEY1
     C     CSKEY1        CHAIN     SCS01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     CSDT01        SCCLT#
     C                   ADD       1             CSDT01
     C                   UPDATE    SCS01
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   END
     C                   END
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVE      Q2RECF        SCRECF                          FOR DISPLAY
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   END
     C*                                                                *****
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C********************************************
     C*   F3 = EXIT                              *
     C********************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C********************************************
     C*   F5 = CLIENT SEARCH (SCR01)             *
     C********************************************
     C     *INKE         IFEQ      '1'
     C                   MOVE      SCCLT#        QQCLT#            5
     C                   CALL      'CS540R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCLT#
     C                   PARM                    QQRTCD            1
     C                   MOVE      QQCLT#        SCCLT#
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCCLT#        OREQ      *ZEROS
     C                   GOTO      STRT
     C                   END                                                    ENDEQ 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C                   GOTO      SCR01T
     C                   END                                                    ENDEQ '*'
     C*
     C                   END                                                    END *INKE
     C****************************************
     C*   F6 = XLIENT ALPHA SEARCH (SCR01    *
     C****************************************
     C     *INKF         IFEQ      '1'
     C**********************************
     C*   SCR01 = CLIENT ALPHA SEARCH  *
     C**********************************
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      SCCLT#        QQCLT#
     C                   MOVE      *BLANKS       QQNAME
     C                   CALL      'CS541R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCLT#
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQNAME           40
     C                   MOVE      QQNAME        SCNAME
     C                   MOVE      QQCLT#        SCCLT#
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCCLT#        OREQ      *ZEROS
     C                   GOTO      STRT
     C                   END                                                    ENDEQ 'B'
     C*
     C     QQRTCD        IFEQ      'C'
     C                   GOTO      STRT
     C                   END                                                    ENDEQ 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C                   GOTO      SCR01T
     C                   END                                                    ENDEQ '*'
     C*
     C                   END                                                    END SCR01
     C                   END                                                    END SCR01
     C********************************************
     C*   F10 = TABLE SEARCH (SCR02)             *
     C********************************************
     C     *INKJ         IFEQ      '1'
     C                   MOVEL     SCST          XX2               2
     C     XX2           IFEQ      '? '
     C*
     C                   MOVE      *BLANK        QQST              2
     C                   CALL      'SY600R'
     C                   PARM                    QQST
     C                   PARM                    QQRTCD            1
     C                   MOVEL     QQST          SCST
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   MOVE      '1'           *IN28                          POS CURSOR AT ZIP1
     C                   GOTO      STRT
     C                   END                                                    END EQ '? '
     C                   END                                                    END *INKJ
     C********************************************
     C*   F12 = PREVIOUS SCREEN (CANCEL)         *
     C********************************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END                                                    END SCR02
     C     Q2RECF        IFEQ      'SCR03'
     C                   MOVEL     'SCR02'       Q2RECF
     C                   END                                                    END SCR03
     C                   GOTO      STRT
     C                   END                                                    END *INKL
     C********************************************
     C*   *IN10 = PROCESS HELP REQUESTS          *
     C********************************************
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
     C                   END                                                    END *IN10
     C********************************************
     C*   F20 = CHANGE STATUS                    *
     C********************************************
     C     *INKU         IFEQ      '1'
     C     *IN40         ANDEQ     '0'
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   MOVE      '1'           *IN64                           ALLOW DELETE
     C                   MOVEL     CD(1)         SCCMDD                          |
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   MOVE      '0'           *IN64                           NO DELETING
     C                   MOVEL     *BLANKS       SCCMDD                          |
     C                   END
     C                   GOTO      STRT
     C                   END                                                     END *INKU
     C********************************************
     C*   F23 = DELETE CLIENT (IF STATUS = I)    *
     C********************************************
     C     *INKX         IFEQ      '1'
     C     *IN64         ANDEQ     '1'
     C     Q2RECF        ANDEQ     'SCR02'
     C     CTKEY         DELETE    PCT01                              60
     C                   MOVE      *BLANKS       SCCLT#
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                     END *INKX
     C*
     C                   END                                                    END CMDKEYS
     C*************************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C*
     C*************************************************************************
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  CLIENT NUMBER CANNOT BE EQUAL TO ZEROS
     C     SCCLT#        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     SCCLT#        CHAIN     PCT01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PCT01E
     C*
     C*** IF INQUIRY, DISPLAY "INQUIRY" MODE, ELSE DISPLAY "UPDATE" MODE
     C     *IN09         IFEQ      '1'
     C                   MOVE      UA(3)         SCMODE                         INQUIRY
     C                   ELSE
     C                   MOVE      UA(2)         SCMODE                         UPDATE
     C                   END
     C*
     C*** DEMOGRAPHICS FOR SCR02
     C                   MOVE      CTMBR         SCMBR
     C                   Z-ADD     CTFND         SCFND
     C                   Z-ADD     CTMBR#        SCMBR#
     C                   MOVE      CTNAME        SCNAME
     C                   MOVE      CTADR1        SCADR1
     C                   MOVE      CTADR2        SCADR2
     C                   MOVE      CTCTY         SCCTY
     C                   MOVE      CTST          SCST
     C                   Z-ADD     CTZIP1        SCZIP1
     C                   Z-ADD     CTZIP2        SCZIP2
     C                   MOVE      CTCONT        SCCONT
     C                   Z-ADD     CTTARA        SCTARA
     C                   Z-ADD     CTPHN#        SCPHN#
     C                   Z-ADD     CTFARA        SCFARA
     C                   Z-ADD     CTFAX#        SCFAX#
     C*
     C*** ACCOUNTING INFORMATION FOR SCR03
     C*** # OF SEARCHS (MTD/QTD/YTD)
     C                   Z-ADD     CT#MM         SC#MM
     C                   Z-ADD     CT#MT         SC#MT
     C                   Z-ADD     CT#MF         SC#MF
     C                   Z-ADD     CT#QM         SC#QM
     C                   Z-ADD     CT#QT         SC#QT
     C                   Z-ADD     CT#QF         SC#QF
     C                   Z-ADD     CT#YM         SC#YM
     C                   Z-ADD     CT#YT         SC#YT
     C                   Z-ADD     CT#YF         SC#YF
     C*
     C                   Z-ADD     CTLSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCLSDT
     C*
     C*** AMOUNT DUE (MTD/QTD/YTD)
     C                   Z-ADD     CT$DMM        SC$DMM
     C                   Z-ADD     CT$DMT        SC$DMT
     C                   Z-ADD     CT$DMF        SC$DMF
     C                   Z-ADD     CT$DQM        SC$DQM
     C                   Z-ADD     CT$DQT        SC$DQT
     C                   Z-ADD     CT$DQF        SC$DQF
     C                   Z-ADD     CT$DYM        SC$DYM
     C                   Z-ADD     CT$DYT        SC$DYT
     C                   Z-ADD     CT$DYF        SC$DYF
     C*
     C                   Z-ADD     CTLBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCLBDT
     C*
     C                   MOVE      CT2NOT        SC2NOT
     C*
     C*** ADD UP YTD TO BE USED IN BALANCE DUE FIELD
     C                   Z-ADD     CT$DYM        XXTOTD           11 2
     C                   ADD       CT$DYT        XXTOTD
     C                   ADD       CT$DYF        XXTOTD
     C*
     C*** AMOUNT PAID (MTD/QTD/YTD)
     C                   Z-ADD     CT$PMM        SC$PMM
     C                   Z-ADD     CT$PMT        SC$PMT
     C                   Z-ADD     CT$PMF        SC$PMF
     C                   Z-ADD     CT$PQM        SC$PQM
     C                   Z-ADD     CT$PQT        SC$PQT
     C                   Z-ADD     CT$PQF        SC$PQF
     C                   Z-ADD     CT$PYM        SC$PYM
     C                   Z-ADD     CT$PYT        SC$PYT
     C                   Z-ADD     CT$PYF        SC$PYF
     C*
     C                   Z-ADD     CTLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCLPDT
     C*
     C*** ADD UP YTD TO BE USED IN BALANCE DUE FIELD
     C                   Z-ADD     CT$PYM        XXTOTP           11 2
     C                   ADD       CT$PYT        XXTOTP
     C                   ADD       CT$PYF        XXTOTP
     C*
     C*** CALCULATE BALANCE DUE
     C     XXTOTP        SUB       XXTOTD        SCBALD
     C*
     C                   MOVE      CTSTAT        SCSTAT
     C     SCSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN64                          ALLOW DELETING
     C                   MOVEL     CD(1)         SCCMDD                         |
     C                   ELSE                                                   |
     C                   MOVE      '0'           *IN64                          NO DELETING
     C                   MOVEL     *BLANKS       SCCMDD                         |
     C                   END                                                    |
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C*
     C                   MOVE      *BLANKS       SCMBR
     C                   Z-ADD     *ZEROS        SCFND
     C                   Z-ADD     *ZEROS        SCMBR#
     C                   MOVE      *BLANKS       SCNAME
     C                   MOVE      *BLANKS       SCADR1
     C                   MOVE      *BLANKS       SCADR2
     C                   MOVE      *BLANKS       SCCTY
     C                   MOVE      *BLANKS       SCST
     C                   Z-ADD     *ZEROS        SCZIP1
     C                   Z-ADD     *ZEROS        SCZIP2
     C                   MOVE      *BLANKS       SCCONT
     C                   Z-ADD     *ZEROS        SCTARA
     C                   Z-ADD     *ZEROS        SCPHN#
     C                   Z-ADD     *ZEROS        SCFARA
     C                   Z-ADD     *ZEROS        SCFAX#
     C*
     C*** ACCOUNTING INFORMATION FOR SCR03
     C*** # OF SEARCHS (MTD/QTD/YTD)
     C                   Z-ADD     *ZEROS        SC#MM
     C                   Z-ADD     *ZEROS        SC#MT
     C                   Z-ADD     *ZEROS        SC#MF
     C                   Z-ADD     *ZEROS        SC#QM
     C                   Z-ADD     *ZEROS        SC#QT
     C                   Z-ADD     *ZEROS        SC#QF
     C                   Z-ADD     *ZEROS        SC#YM
     C                   Z-ADD     *ZEROS        SC#YT
     C                   Z-ADD     *ZEROS        SC#YF
     C                   Z-ADD     *ZEROS        SCLSDT
     C*** AMOUNT DUE (MTD/QTD/YTD)
     C                   Z-ADD     *ZEROS        SC$DMM
     C                   Z-ADD     *ZEROS        SC$DMT
     C                   Z-ADD     *ZEROS        SC$DMF
     C                   Z-ADD     *ZEROS        SC$DQM
     C                   Z-ADD     *ZEROS        SC$DQT
     C                   Z-ADD     *ZEROS        SC$DQF
     C                   Z-ADD     *ZEROS        SC$DYM
     C                   Z-ADD     *ZEROS        SC$DYT
     C                   Z-ADD     *ZEROS        SC$DYF
     C                   Z-ADD     *ZEROS        SCLBDT
     C                   MOVE      *BLANKS       SC2NOT
     C*
     C*** AMOUNT PAID (MTD/QTD/YTD)
     C                   Z-ADD     *ZEROS        SC$PMM
     C                   Z-ADD     *ZEROS        SC$PMT
     C                   Z-ADD     *ZEROS        SC$PMF
     C                   Z-ADD     *ZEROS        SC$PQM
     C                   Z-ADD     *ZEROS        SC$PQT
     C                   Z-ADD     *ZEROS        SC$PQF
     C                   Z-ADD     *ZEROS        SC$PYM
     C                   Z-ADD     *ZEROS        SC$PYT
     C                   Z-ADD     *ZEROS        SC$PYF
     C                   Z-ADD     *ZEROS        SCLPDT
     C*
     C                   MOVE      '0'           *IN64                           NO DELETING
     C                   MOVEL     *BLANKS       SCCMDD                          |
     C                   MOVE      'A'           SCSTAT
     C*
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C*  MEMBER FLAG MUST BE "M" OR "N"
     C     SCMBR         IFNE      'M'
     C     SCMBR         ANDNE     'N'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  NAME CANNOT BE EQUAL TO BLANKS
     C     SCNAME        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  ADDRESS 1 CANNOT BE EQUAL TO BLANKS
     C     SCADR1        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  CITY CANNOT BE EQUAL TO BLANKS
     C     SCCTY         IFEQ      *BLANKS
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  STATE MUST BE VALID
     C                   MOVE      *BLANKS       STKEY1
     C                   MOVEL     SCST          STKEY1
     C     STKEY1        CHAIN     PST01                              24
     C     *IN24         CABEQ     '1'           STRT
     C*
     C*  ZIP CODE 1 CANNOT BE EQUAL TO ZEROS
     C     SCZIP1        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*  CONTACT NAME CANNOT BE EQUAL TO BLANKS
     C     SCCONT        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C*  TELEPHONE# CANNOT BE EQUAL TO ZEROS
     C     SCPHN#        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*  IF THIS SYSTEM HAS BILLING INSTALLED, THEN DETERMINE:
     C*  IF THIS IS AN UPDATE OR INQUIRY, DISPLAY SCR03 FIRST
     C*  IF THIS IS AN ADD, THEN UPDATE THE RECORD, GOTO SCR01
     C*
     C                   MOVEL     'CS'          CSKEY1
     C     CSKEY1        CHAIN     SCS01                              60
     C     *IN60         IFEQ      '0'
     C     CSBLIN        ANDEQ     'Y'
     C*
     C     *IN40         IFEQ      '0'                                          UPD/INQ
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   EXCEPT    SCS01E
     C     CSBLIN        IFEQ      'Y'
     C*
     C     *IN40         IFEQ      '0'                                          UPD/INQ
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END CSBLIN=Y
     C                   END                                                    END *IN60=0
     C*
     C                   END                                                    CSBLIN = Y
     C*
     C*
     C     SCR03T        TAG
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO SCR01
     C*
     C     *IN09         IFEQ      '0'
     C     CTKEY         CHAIN     PCT01                              40
     C*
     C                   MOVE      SCMBR         CTMBR
     C                   MOVE      SCFND         CTFND
     C                   MOVE      SCMBR#        CTMBR#
     C                   MOVE      SCNAME        CTNAME
     C                   MOVE      SCADR1        CTADR1
     C                   MOVE      SCADR2        CTADR2
     C                   MOVE      SCCTY         CTCTY
     C                   MOVE      SCST          CTST
     C                   Z-ADD     SCZIP1        CTZIP1
     C                   Z-ADD     SCZIP2        CTZIP2
     C                   MOVE      SCCONT        CTCONT
     C                   Z-ADD     SCTARA        CTTARA
     C                   Z-ADD     SCPHN#        CTPHN#
     C                   Z-ADD     SCFARA        CTFARA
     C                   Z-ADD     SCFAX#        CTFAX#
     C*
     C                   Z-ADD     SC#MM         CT#MM
     C                   Z-ADD     SC#MT         CT#MT
     C                   Z-ADD     SC#MF         CT#MF
     C                   Z-ADD     SC#QM         CT#QM
     C                   Z-ADD     SC#QT         CT#QT
     C                   Z-ADD     SC#QF         CT#QF
     C                   Z-ADD     SC#YM         CT#YM
     C                   Z-ADD     SC#YT         CT#YT
     C                   Z-ADD     SC#YF         CT#YF
     C*
     C                   Z-ADD     SCLSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CTLSDT
     C*
     C                   Z-ADD     SC$DMM        CT$DMM
     C                   Z-ADD     SC$DMT        CT$DMT
     C                   Z-ADD     SC$DMF        CT$DMF
     C                   Z-ADD     SC$DQM        CT$DQM
     C                   Z-ADD     SC$DQT        CT$DQT
     C                   Z-ADD     SC$DQF        CT$DQF
     C                   Z-ADD     SC$DYM        CT$DYM
     C                   Z-ADD     SC$DYT        CT$DYT
     C                   Z-ADD     SC$DYF        CT$DYF
     C*
     C                   Z-ADD     SCLBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CTLBDT
     C*
     C                   MOVE      SC2NOT        CT2NOT
     C*
     C                   Z-ADD     SC$PMM        CT$PMM
     C                   Z-ADD     SC$PMT        CT$PMT
     C                   Z-ADD     SC$PMF        CT$PMF
     C                   Z-ADD     SC$PQM        CT$PQM
     C                   Z-ADD     SC$PQT        CT$PQT
     C                   Z-ADD     SC$PQF        CT$PQF
     C                   Z-ADD     SC$PYM        CT$PYM
     C                   Z-ADD     SC$PYT        CT$PYT
     C                   Z-ADD     SC$PYF        CT$PYF
     C*
     C                   Z-ADD     SCLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CTLPDT
     C*
     C                   MOVE      SCSTAT        CTSTAT
     C                   MOVE      Q1USER        CTUSER
     C                   MOVE      Q1PGM         CTPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CTDT
     C*
     C                   Z-ADD     DSTIME        CTTIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PCT01
     C                   ELSE
     C                   Z-ADD     SCCLT#        CTCLT#
     C                   MOVEL     Q1USER        CTIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CTIDT
     C*
     C                   WRITE     PCT01
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     *ZEROS        SCCLT#
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPCT01     E            PCT01E
     OSCS01     E            SCS01E
**  TABLE UA
-- ADD --
- UPDATE -
- INQUIRY-
**  TABLE CD
F23=Delete
