     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*
     H/TITLE SC810R - PROMPT FOR QUERY INTERFACE WORKFILE CREATION
     H*
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*    05/25/07   C:191 PROJECT: WC1515
     H*                     Check claims authority
     H*
     H*    03/03/99   C:03  CHANGE TITLE ON SCR02E IN DSPF ONLY
     H*               TITLE ORIG: "CLAIM SELECTION"
     H*               TITLE CORRECTION:  "EMPLOYER SELECTION"
     H*
     H*    02/16/98   C:58  New for V3.
     H*
     H*****************************************************************
     H* Indicator Usage:
     H*  60    General Chain
     H* SCR01
     H*  20    SCM0509 - At least one category must be selected
     H* SCR02C
     H*  20    WCR0508 - Invalid Fund
     H*  21    WCR0681 - Invalid Case
     H* SCR02E
     H*  20    WCR0508 - Invalid Fund
     H*  21    WCR0538 - Invalid Employer
     H*****************************************************************
     H* File Usage:
     H*  WMCML1 (PCM01) - CO/FND/FYR/CASE
     H*  WMEML  (PEM01) - CO/FND/EMP#/DIV
     H*  WMFNL  (PFN01) - CO/FND
     H*****************************************************************
     FSC810F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMUPL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMCMP     IF   E             DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCM01:DBRCDC)
     FWMEMP     IF   E             DISK
     F                                     INFDS(DBFDE)
     F                                     RENAME(PEM01:DBRCDE)

     D*
     D DBFDS           DS
     D  DBRRNC               397    400B 0
     D DBFDE           DS
     D  DBRRNE               397    400B 0
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
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C*** END ADD ****** 05/25/07 ********************************************
     C*
     C*  User Parameter File
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  Claim Master File
     C     CMKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCCAS
     C*
     C*  Employer Master File
     C     EMKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCEFND
     C                   KFLD                    SCEEMP
     C                   KFLD                    SCEDIV
     C*
     C*  Fund Master File
     C     FNKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C*
     C*  Entry List
     C     *ENTRY        PLIST
     C                   PARM                    QQCALL            1
     C*
      /Free
       scPrDs = 'DARTS/WCMS Query Interface - Workfile';
      /End-Free
     C*  Initialization of Workfields
     C     *LIKE         DEFINE    Q2RECF        HLRECF
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C                   MOVE      *BLANKS       SCCMFL            1
     C                   MOVE      *BLANKS       QCCMFL            1
     C                   MOVE      *ZEROS        QCCO#             3
     C                   MOVE      *ZEROS        QCFND             3
     C                   MOVE      *ZEROS        QCFYR             3
     C                   MOVE      *ZEROS        QCCASE            7
     C                   MOVE      *BLANKS       SCEMFL            1
     C                   MOVE      *BLANKS       QEEMFL            1
     C                   MOVE      *ZEROS        QECO#             3
     C                   MOVE      *ZEROS        QEFND             3
     C                   MOVE      *ZEROS        QEEMP#            9
     C                   MOVE      *ZEROS        QEDIV             5
     C*
     C     TOP           TAG
     C*
     C*  Get the User Parameter File
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         SCCFND
     C                   MOVE      *ZERO         SCEFND
     C                   ELSE
     C                   MOVE      UPFND         SCCFND
     C                   MOVE      UPFND         SCEFND
     C                   END
     C*
     C                   TIME                    DSDTTM
     C*
     C*   Load the Record Format Info for First Time
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   Display all formats
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02C'
     C                   EXFMT     SCR02C
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02E'
     C                   EXFMT     SCR02E
     C                   END
     C**************************************************************************
     C*   Process All Command Keys
     C*
     C     *IN11         IFEQ      '1'
     C*
     C***********************************
     C*   F3 = EXIT                     *
     C***********************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C*
     C*   F10 = SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C     HREC          IFEQ      'SCR02C'
     C*________________________________________________________________
     C*   Claim #
     C     HFLD          IFEQ      'SCCCAS'
     C     HFLD          OREQ      'SCCYR'
     C*
     C*  Call the 'Work with claims' search.
     C                   CALL      'W0070R'
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM      *ZERO         @@RRN             9 0
     C                   PARM      'CN'          @@SORT            2
     C                   PARM      0             @@RRN2            9 0
     C                   PARM      SCCYR         @@YEAR            3 0
     C*
     C     @@KC          CABEQ     *ON           TERM
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02C'      Q2RECF
     C*
     C     @@RRN         CABEQ     0             STRT
     C*
     C     @@RRN         CHAIN     WMCMP                              60
     C     *IN60         CABEQ     *ON           STRT
     C*
     C                   Z-ADD     CMFYR         SCCYR
     C                   Z-ADD     CMCASE        SCCCAS
     C*
     C                   GOTO      STRT
     C*
     C                   END                                                    e = 'SCCASE'C'
     C                   END                                                    e = 'SCR02C'C'
     C*
     C     HREC          IFEQ      'SCR02E'
     C*________________________________________________________________
     C*   Claim #
     C     HFLD          IFEQ      'SCEEMP'
     C     HFLD          OREQ      'SCEDIV'
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##MODE            1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      0             ##RRN2            9 0
     C                   PARM      'W'           ##QTE             1
     C                   PARM      2             ##VIEW            1 0
     C                   PARM      0             ##RRN3            9 0
     C*
     C     ##KC          CABEQ     *ON           TERM
     C     ##RRN3        CABEQ     0             STRT
     C*
     C     ##RRN3        CHAIN     WMEMP                              60
     C*
     C     *IN60         CABEQ     *ON           STRT
     C*
     C                   MOVE      EMEMP#        SCEEMP
     C                   MOVE      EMDIV         SCEDIV
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02E'      Q2RECF
     C*
     C                   GOTO      STRT
     C*
     C                   END                                                    e = 'SCR02E'
     C                   END                                                    e = 'SCEMP#'
     C***
     C                   END                                                    ENDIF *INKJ
     C*
     C***********************************
     C*   F12 = PREVIOUS                *
     C***********************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR02E'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C***********************************
     C*   F13 = CHG USER PARMS          *
     C***********************************
     C     *INKM         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         SCCFND
     C                   MOVE      *ZERO         SCEFND
     C                   ELSE
     C                   MOVE      UPFND         SCCFND
     C                   MOVE      UPFND         SCEFND
     C                   END
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *INKM
     C*
     C***********************************
     C                   END                                                    END CMDKEYS
     C**************************************************************************
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02C'      SCR2CT
     C     Q2RECF        CABEQ     'SCR02E'      SCR2ET
     C**************************************************************************
     C*   Process Request Screen - Prompt Screen
     C     SCR01T        TAG
     C*
     C     SCCMFL        IFEQ      *BLANKS
     C     SCEMFL        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C* If Case Creation was selected, go to SCR02C
     C     SCCMFL        IFNE      *BLANKS
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02C'      Q2RECF
     C                   ELSE
     C* Else go to SCR02E for Employer Creation
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02E'      Q2RECF
     C                   END
     C*
     C                   GOTO      STRT
     C*
     C**************************************************************************
     C*   Process Request Screen - Claim Selection
     C     SCR2CT        TAG
     C*
     C* If entered, validate Fund
     C     SCCFND        IFNE      *ZEROS
     C                   MOVE      SCCFND        XXFND
     C     FNKY          CHAIN     PFN01                              20
     C     *IN20         CABEQ     '1'           STRT
     C                   END                                                    END SCCFND NE *ZEROS
     C*
     C* If entered, validate Claim
     C     SCCCAS        IFNE      *ZEROS
     C                   MOVE      SCCYR         XXFYR
     C     CMKY          CHAIN     PCM01                              21
     C     *IN21         CABEQ     '1'           STRT
     C*
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Check claim auth, Call claims security API
     C                   MOVE      UPCO#         ##CO#
     C                   MOVE      SCCFND        ##FND
     C                   MOVE      XXFYR         ##FYR
     C                   MOVE      SCCCAS        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   SETON                                        21
     C                   GOTO      STRT
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C                   END                                                    END SCCCAS NE *ZEROS
     C*
     C                   MOVE      SCCMFL        QCCMFL
     C                   MOVE      UPCO#         QCCO#
     C                   MOVE      SCCFND        QCFND
     C                   MOVE      SCCYR         QCFYR
     C                   MOVE      SCCCAS        QCCASE
     C*
     C* If Employer Creation was also selected, go to SCR02E
     C*    Else, submit the creation program
     C     SCEMFL        IFNE      *BLANKS
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02E'      Q2RECF
     C                   GOTO      STRT
     C                   ELSE
     C                   GOTO      SUBMIT
     C                   END
     C*
     C**************************************************************************
     C*   Process Request Screen - Employer Selection
     C     SCR2ET        TAG
     C*
     C* If entered, validate Fund
     C     SCEFND        IFNE      *ZEROS
     C                   MOVE      SCEFND        XXFND
     C     FNKY          CHAIN     PFN01                              20
     C     *IN20         CABEQ     '1'           STRT
     C                   END                                                    END SCEFND NE *ZEROS
     C*
     C* Validate Employer Number
     C     SCEEMP        IFNE      *ZEROS
     C     EMKY          CHAIN     PEM01                              21
     C     *IN21         CABEQ     '1'           STRT
     C                   END                                                    END SCEEMP NE *ZEROS
     C*
     C                   MOVE      SCEMFL        QEEMFL
     C                   MOVE      UPCO#         QECO#
     C                   MOVE      SCEFND        QEFND
     C                   MOVE      SCEEMP        QEEMP#
     C                   MOVE      SCEDIV        QEDIV
     C*
     C**************************************************************************
     C*   Call the CLP to create the workfile
     C     SUBMIT        TAG
     C                   CALL      'SC810C'
     C                   PARM                    QCCMFL
     C                   PARM                    QCCO#
     C                   PARM                    QCFND
     C                   PARM                    QCFYR
     C                   PARM                    QCCASE
     C                   PARM                    QEEMFL
     C                   PARM                    QECO#
     C                   PARM                    QEFND
     C                   PARM                    QEEMP#
     C                   PARM                    QEDIV
     C*
     C**************************************************************************
     C*  End the Program
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *====================================================================*
