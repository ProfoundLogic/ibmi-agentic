     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*
     H/TITLE W4060R - Prompt for Deposit Recap Report
     H****************************************************************
     H**  MODIFICATIONS:
     H**
     H**   05/14/01  C:136 PROJECT WC577
     H**             - SCR01 WOULD NOT DISPLAY UPDATED GROUP NUMBER FROM SCR02
     H**             - ERR MSG WAS WCR1058 OVERWRITING EXFMT SCR01.
     H**             - MOVED *INKJ IF STATEMENT TO END OF *IN11 IF STATEMENT TO
     H**               GROUP *IN FUNCTIONS TOGETHER. REVERSED LOGIC FOR WCR1058
     H**               TO DISPLAY WHEN *IN75 IS ON.
     H**
     H**   11/06/98  C:78  PROJECT WC577
     H**             - DISPLAY PGM NUMBER INSTEAD OF W4080F.
     H**             - IF GROUP IS CLEARED IN USER PARMS, SCREEN DOES
     H**               NOT CLEAR PREVIOUS GROUP NAME.
     H**             - IF F10=SEARCH IS INVOKED ON A FIELD THAT THERE
     H**               IS NO SEARCH FOR, THEN DISPLAY ERROR MESSAGE.
     H**
     H**   01/16/98  C:80  PROJECT WC775
     H**             ADDED EDIT CHECKS FOR ALL FIELDS.  CHANGED TO
     H**             PASS PARAMETERS ACCEPTED BY SRRPTC.
     H**
     H*****************************************************************
     F*
     F* Prompt Screen
     FW4080F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F*
     F* Company Master File
     FSMCOL     IF   E           K DISK
     F*
     F* User Parameter Master File
     FWMUPL     IF   E           K DISK
     F*
     F* Fund Master File
     FWMFNL     IF   E           K DISK
     F*** BEGIN ADD *** 01/16/98 **************************************
     F                                     INFDS(DBFDSF)
     FWMEMP     IF   E             DISK
     FWMAAP     IF   E             DISK
     F*** END   ADD *** 01/16/98 **************************************
     F*

     D*
     D*** BEGIN ADD *** 01/16/98 **************************************
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D*** END   ADD *** 01/16/98 **************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*** BEGIN ADD *** 01/16/98 **************************************
     D                 DS
     D  XXRNG1                 1     14
     D  XXEMP1                 1      9
     D  XXDIV1                10     14
     D                 DS
     D  XXRNG2                 1     14
     D  XXEMP2                 1      9
     D  XXDIV2                10     14
     D*** END   ADD *** 01/16/98 **************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** BEGIN ADD *** 01/16/98 **************************************
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')

     I*** END   ADD *** 01/16/98 **************************************

     C*
     C* Key List for the Comapnay Master File
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
     C* Key List for the Fund Detail File
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR             3 0
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
     C* Get the User Parameter Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      UPCO#         XXCO#
     **
     C*** BEGIN ADD *** 01/16/98 **************************************
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPCO#         @@CO#
     C     *LIKE         DEFINE    EMFND         @@FND
     C     *LIKE         DEFINE    EMEMP#        @@EMP#
     C     *LIKE         DEFINE    EMDIV         @@DIV
     C*** END   ADD *** 01/16/98 **************************************
     **
     C*
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
     C*
     C* Get the Requested Record, Load Information
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      UPFND         SCFND
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
      ******* BEGIN CHG ****** 11/06/98 *******************************
     C***************      MOVELFNNAME    SCNAME
     C  N60              MOVEL     FNNAME        SCNAME
     C   60              MOVE      *BLANKS       SCNAME
      ******* END   CHG ****** 11/06/98 *******************************
     C*
     C* Load the Record Format Info for First Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C* Display All Formats
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C* Process All Command Keys
     C*
     C     *IN11         IFEQ      '1'
     C*
     C* F3 = End of Job
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*
     C* Process Change of User Parameters
     C*
     C     *INKM         IFEQ      '1'
     C*
     C*
     C                   CALL      'W0006R'
     C*
     C* Get the User Parameter Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      UPCO#         XXCO#
     C                   GOTO      STRT01
     C                   END
     C*
     C*** BEG DEL ******** 05/14/01 ********************************************
     C******************** END
     C*** END DEL ******** 05/14/01 ********************************************
     C*
     C* Process Request Screen
     C*
     C     SCR01T        TAG
     C*
     C*** BEGIN CHG *** 02/18/98 **************************************
     C*
     C* Searches and Edit Checks Follow for Agent and Employer Ranges
     C*
     **
     **  FUNCTION 10= TABLE FILE SEARCH
     **
     C     *INKJ         IFEQ      *ON
      *
      * EMPLOYER NAME SEARCH
      *
     C     HFLD          IFEQ      'SCEFNR'
     C     HFLD          OREQ      'SCETNR'
     C     HFLD          OREQ      'SCEFBR'
     C     HFLD          OREQ      'SCETBR'
      ******* BEGIN DEL ***** 05/14/01 ********************************
      ******* BEGIN ADD ***** 11/06/98 ********************************
     C******************** MOVE *ON       *IN75
      ******* END   ADD ***** 11/06/98 ********************************
      ******* END   DEL ***** 05/14/01 ********************************
      *
     C     HFLD          IFEQ      'SCEFNR'
     C     HFLD          OREQ      'SCETNR'
     C                   Z-ADD     1             QQVIEW
     C                   ELSE
     C                   Z-ADD     2             QQVIEW
     C                   ENDIF                                                  HFLD IFEQ 'SCEFNR'
     **
     C                   CALL      'W0050R'
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      '2'           QQCALL            1
     C                   PARM      0             QQRRN             9 0
     C                   PARM      DBRRNF        QQRRN2            9 0
     C                   PARM                    QQQTE             1
     C                   PARM                    QQVIEW            1 0
     **
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     **
     C     *INKL         IFEQ      *ON
     C     HFLD          CABEQ     'SCEFNR'      STRT                     61
     C     HFLD          CABEQ     'SCEFNR'      STRT                     46
     C     HFLD          CABEQ     'SCEFBR'      STRT                     47
     C     HFLD          CABEQ     'SCETBR'      STRT                     48
     C                   ENDIF
     **
     ** LOAD EMPLOYER NUMBER/NAME
     **
     C     QQRRN         IFNE      *ZERO
     **
     C     QQRRN         CHAIN     WMEMP                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     HFLD          IFEQ      'SCEFNR'
     C                   MOVEL     EMNAME        SCEFNR
     C     LO:UP         XLATE(P)  SCEFNR        SCEFNR
     C                   MOVE      *ON           *IN46
      *
     C                   ELSE
      *
     C     HFLD          IFEQ      'SCETNR'
     C                   MOVEL     EMNAME        SCETNR
     C     LO:UP         XLATE(P)  SCETNR        SCETNR
     C                   MOVE      *ON           *IN47
      *
     C                   ELSE
      *
     C     HFLD          IFEQ      'SCEFBR'
      *
     C                   MOVE      *ON           *IN48
     C                   MOVE      EMEMP#        SCEFBR
      *
     C                   ELSE
      *
     C                   MOVE      *ON           *IN49
     C                   MOVE      EMEMP#        SCETBR
      *
     C                   ENDIF                                                  HFLD IFEQ 'SCEFBR'
      *
     C                   ENDIF                                                  HFLD IFEQ 'SCETNR'
      *
     C                   ENDIF                                                  HFLD IFEQ 'SCEFNR'
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C                   ELSE
      *
     C     HFLD          CABEQ     'SCEFNR'      STRT                     61
     C     HFLD          CABEQ     'SCETNR'      STRT                     46
     C     HFLD          CABEQ     'SCEFBR'      STRT                     47
     C     HFLD          CABEQ     'SCETBR'      STRT                     48
      *
     C                   ENDIF                                                  QQRRN IFNE *ZERO
     **
     C                   GOTO      STRT
     **
     C                   ENDIF                                                  HFLD=SCEFNR
     **
     **  TABLE SEARCH - FROM AGENT NAME
     **
     C     HFLD          IFEQ      'SCAFNR'
     C     HFLD          OREQ      'SCATNR'
     C     HFLD          OREQ      'SCAFBR'
     C     HFLD          OREQ      'SCATBR'
      ******* BEGIN DEL ***** 05/14/01 ********************************
      ******* BEGIN ADD ***** 11/06/98 ********************************
     C******************** MOVE *ON       *IN75
      ******* END   ADD ***** 11/06/98 ********************************
      ******* END   DEL ***** 05/14/01 ********************************
      *
     C     HFLD          IFEQ      'SCAFNR'
     C     HFLD          OREQ      'SCATNR'
     C                   Z-ADD     1             ##VIEW
     C                   ELSE
     C                   Z-ADD     2             ##VIEW
     C                   ENDIF                                                  HFLD IFEQ 'SCAFNR'
      *
     C                   CALL      'W0040R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      DBRRNF        ##RRN             9 0
     C                   PARM                    ##VIEW            1 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         IFEQ      *ON
     C     HFLD          CABEQ     'SCAFNR'      STRT                     49
     C     HFLD          CABEQ     'SCATNR'      STRT                     50
     C     HFLD          CABEQ     'SCAFBR'      STRT                     51
     C     HFLD          CABEQ     'SCATBR'      STRT                     52
     C                   ENDIF                                                  *INKL IFEQ *ON
      *
     C     ##RRN         CHAIN     PAA01                              60
      *
     C     HFLD          IFEQ      'SCAFNR'
     C     HFLD          OREQ      'SCATNR'
      *
     C                   MOVE      *BLANKS       QQANAM
      *
     C     *IN60         IFEQ      *OFF
     C                   CALL      'WA002R'
     C                   PARM      XXCO#         @@CO#
     C                   PARM      XXFND         @@FND
     C                   PARM      *ZERO         @@EMP#
     C                   PARM      *ZERO         @@DIV
     C                   PARM      *ZERO         @@FYR             3 0
     C                   PARM      AAAGT#        QQAGT#            5 0
     C                   PARM      *BLANKS       QQANAM           40
     C                   ENDIF                                                  *IN60 IFEQ *OFF
     **
     **
     **  IF AGNAME IS FOUND POSITION CURSOR TO NEXT FIELD ELSE POSITION
     **  TO FROM AGENT ALPHA
     **
     C     QQANAM        IFNE      *BLANKS
      *
     C     HFLD          IFEQ      'SCAFNR'
      *
     C                   MOVEL     QQANAM        SCAFNR
     C     LO:UP         XLATE(P)  SCAFNR        SCAFNR
     C                   MOVE      *ON           *IN50
      *
     C                   ELSE
      *
     C                   MOVEL     QQANAM        SCATNR
     C     LO:UP         XLATE(P)  SCATNR        SCATNR
     C                   MOVE      *ON           *IN51
      *
     C                   ENDIF                                                  HFLD IFEQ 'SCAFNR'
      *
     C                   ELSE
      *
     C     HFLD          CABEQ     'SCAFNR'      STRT                     49
     C     HFLD          CABEQ     'SCATNR'      STRT                     50
      *
     C                   ENDIF                                                  QQANAM IFNE *BLANKS
      *
     C                   ELSE
      *
     C     *IN60         IFEQ      *OFF
      *
     C     HFLD          IFEQ      'SCAFBR'
      *
     C                   MOVE      AAAGT#        SCAFBR
     C                   MOVE      *ON           *IN52
      *
     C                   ELSE
      *
     C                   MOVE      AAAGT#        SCATBR
      *
     C                   ENDIF                                                  HFLD IFEQ 'SCAFBR'
      *
     C                   ELSE
      *
     C     HFLD          CABEQ     'SCAFBR'      STRT                     51
     C     HFLD          CABEQ     'SCATBR'      STRT                     52
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C                   ENDIF                                                  HFLD IFEQ SCAFNR
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  HFLD IFEQ SCAFNR
     **
     **
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPFDT
     **
     C                   Z-ADD     SCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPTDT
     **
     C                   Z-ADD     SCRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRNDT
     **
     C*** BEG ADD ******** 05/14/01 ***********************************
     C                   MOVE      *ON           *IN75
     C*** END ADD ******** 05/14/01 ***********************************
     C                   GOTO      STRT
     C*** BEG DEL ******** 05/14/01 ***********************************
      ****** BEGIN ADD ********* 11/06/98 *****************************
     C********** *IN75     CABEQ*ON       STRT
      ****** END   ADD ********* 11/06/98 *****************************
     C*** END DEL ******** 05/14/01 ***********************************
     C                   ENDIF                                                  *INKJ IFEQ *ON
     C*** END   ADD *** 01/16/98 **************************************
     C*
     C*** BEG ADD ******** 05/14/01 ***********************************
     C                   ENDIF                                                  *IN11 IFEQ *ON
     C*** END ADD ******** 05/14/01 ***********************************
     C*** BEGIN ADD *** 01/16/98 **************************************
     C*
     C* From date must be less than to date
     C*
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPFDT            8 0
     C*
     C                   Z-ADD     SCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPTDT            8 0
     C*
     C     XXPFDT        IFGT      XXPTDT
     C*
     C                   MOVE      *ON           *IN24
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCPFDT IFGT  SCPTDT
     C*
     C* Validate the Policy From Date Entered
     C*
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPFDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C* Validate the Policy To Date Entered
     C*
     C                   Z-ADD     SCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPTDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C* Validate the Run Date Entered
     C*
     C                   Z-ADD     SCRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRNDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C* Validate Account Type, Account Status, Primary Sort and
     C* Secondary Sort
     C*
     C     SCEMTP        IFNE      'E'
     C     SCEMTP        ANDNE     'S'
     C     SCEMTP        ANDNE     'B'
     C*
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCEMTP IFNE 'E'
     C*
     C     SCRDTS        IFNE      'A'
     C     SCRDTS        ANDNE     'C'
     C     SCRDTS        ANDNE     'B'
     C*
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCRDTS IFNE 'A'
     C*
     C     SCPSRT        IFNE      'EA'
     C     SCPSRT        ANDNE     'EN'
     C     SCPSRT        ANDNE     'AA'
     C     SCPSRT        ANDNE     'AN'
     C*
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCPSRT IFNE 'EA'
     C*
     C     SCPSRT        IFEQ      'AA'
     C     SCPSRT        OREQ      'AN'
     C*
     C     SCSSRT        IFEQ      *BLANKS
     C*
     C                   MOVE      *ON           *IN62
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCSSRT IFEQ *BLANKS
     C*
     C                   ENDIF                                                  SCPSRT IFEQ 'AA'
     C*
     C     SCSSRT        IFNE      *BLANKS
     C     SCSSRT        ANDNE     'EA'
     C     SCSSRT        ANDNE     'EN'
     C*
     C                   MOVE      *ON           *IN28
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCSSRT IFNE *BL
     C*
     C* Validate the data entered for the ranges
     C*
     C     SCEFNR        IFNE      *BLANK
     C     SCETNR        ANDEQ     *BLANK
     C*
     C                   MOVE      *ON           *IN41
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCEFNR IFNE *BLANK
     C*
     C     SCETNR        IFNE      *BLANK
     C     SCEFNR        ANDEQ     *BLANK
     C*
     C                   MOVE      *ON           *IN42
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCEFNR IFNE *BLANK
     C*
     C     SCAFNR        IFNE      *BLANK
     C     SCATNR        ANDEQ     *BLANK
     C*
     C                   MOVE      *ON           *IN45
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCAFNR IFNE *BLANK
     C*
     C     SCATNR        IFNE      *BLANK
     C     SCAFNR        ANDEQ     *BLANK
     C*
     C                   MOVE      *ON           *IN53
     C*
     C                   ENDIF                                                  SCATNR IFNE *BLANK
     C*
     C* Ensure ranges entered are valid.
     C*
     C     ' '           SCAN      SCETNR        X                 1 0
     C*
     C     X             IFEQ      1
     C*
     C                   MOVE      SCETNR        XXETNR            1
     C                   MOVE      *BLANKS       SCETNR
     C                   MOVEL     XXETNR        SCETNR
     C*
     C                   ENDIF                                                  X IFEQ 1
     C*
     C     SCEFNR        IFGT      SCETNR
     C*
     C                   MOVE      *ON           *IN29
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCEFNR IFNE *BLANK
     C*
     C     SCEFBR        IFGT      SCETBR
     C*
     C                   MOVE      *ON           *IN30
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCEFBR IFNE *BLANK
     C*
     C     ' '           SCAN      SCATNR        X                 1 0
     C*
     C     X             IFEQ      1
     C*
     C                   MOVE      SCATNR        XXATNR            1
     C                   MOVE      *BLANKS       SCATNR
     C                   MOVEL     XXATNR        SCATNR
     C*
     C                   ENDIF                                                  X IFEQ 1
     C*
     C     SCAFNR        IFGT      SCATNR
     C*
     C                   MOVE      *ON           *IN31
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCAFNR IFNE *BLANK
     C*
     C     SCAFBR        IFGT      SCATBR
     C*
     C                   MOVE      *ON           *IN32
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCAFNR IFNE *BLANK
     C*
     C* Check all mutually exclusive fields.
     C*
     C     SCEFNR        IFNE      *BLANKS
     C     SCETNR        ORNE      *BLANKS
     C*
     C                   SELECT
     C*
     C     SCEFBR        WHENNE    *ZEROS
     C                   MOVE      *ON           *IN33
     C                   GOTO      STRT
     C*
     C     SCETBR        WHENNE    *ZEROS
     C                   MOVE      *ON           *IN33
     C                   GOTO      STRT
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  SCEFNR IFNE *BLANKS
     C*
     C     SCEFBR        IFNE      *ZEROS
     C     SCETBR        ORNE      *ZEROS
     C*
     C                   SELECT
     C*
     C     SCEFNR        WHENNE    *BLANKS
     C                   MOVE      *ON           *IN33
     C                   GOTO      STRT
     C*
     C     SCETNR        WHENNE    *BLANKS
     C                   MOVE      *ON           *IN33
     C                   GOTO      STRT
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  SCEFBR IFNE *BLANKS
     C*
     C     SCAFNR        IFNE      *BLANKS
     C     SCATNR        ORNE      *BLANKS
     C*
     C                   SELECT
     C*
     C     SCAFBR        WHENNE    *ZEROS
     C                   MOVE      *ON           *IN34
     C                   GOTO      STRT
     C*
     C     SCATBR        WHENNE    *ZEROS
     C                   MOVE      *ON           *IN34
     C                   GOTO      STRT
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  SCAFNR IFNE *BLANKS
     C*
     C     SCAFBR        IFNE      *ZEROS
     C     SCATBR        ORNE      *ZEROS
     C*
     C                   SELECT
     C*
     C     SCAFNR        WHENNE    *BLANKS
     C                   MOVE      *ON           *IN34
     C                   GOTO      STRT
     C*
     C     SCATNR        WHENNE    *BLANKS
     C                   MOVE      *ON           *IN34
     C                   GOTO      STRT
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  SCAFBR IFNE *BLANKS
     C*
     C* Load Selection Data
     C*
     C     SCEFNR        IFNE      *BLANKS
     C*
     C                   MOVEL     'EA'          XXRGTP            2
     C                   MOVE      *ON           *IN56
     C*
     C                   ENDIF                                                  SCEFNR IFNE *BLANKS
     C*
     C     SCEFBR        IFNE      *ZEROS
     C*
     C                   MOVEL     'EN'          XXRGTP
     C                   MOVE      SCEFBR        XXEMP1
     C                   MOVE      *ZEROS        XXDIV1
     C*
     C                   MOVE      SCETBR        XXEMP2
     C                   MOVE      '99999'       XXDIV2
     C*
     C                   ENDIF                                                  SCEFBR IFNE *BLANKS
     C*
     C     SCAFNR        IFNE      *BLANKS
     C*
     C                   MOVEL     'AA'          XXRGT2            2
     C                   MOVE      *ON           *IN57
     C*
     C                   ENDIF                                                  SCAFNR IFNE *BLANKS
     C*
     C     SCAFBR        IFNE      *ZEROS
     C*
     C                   MOVEL     'AN'          XXRGT2
     C*
     C                   ENDIF                                                  SAAFBR IFNE *BLANKS
     C*
     C     SCAFBR        IFEQ      *ZEROS
     C*
     C                   MOVE      *BLANKS       XXAFBR            5
     C*
     C                   ELSE
     C*
     C                   MOVE      SCAFBR        XXAFBR
     C*
     C                   ENDIF                                                  SCAFBR IFEQ *ZEROS
     C*
     C     SCATBR        IFEQ      *ZEROS
     C*
     C                   MOVE      *BLANKS       XXATBR            5
     C*
     C                   ELSE
     C*
     C                   MOVE      SCATBR        XXATBR
     C*
     C                   ENDIF                                                  SCATBR IFEQ *ZEROS
     C*** END   ADD *** 01/16/98 **************************************
     C* Call the CLP to Submit the List
     C*
     C*** BEGIN CHG *** 01/16/98 **************************************
     C******************** MOVE UPCO#     QQCO#   3
     C******************** MOVE SCFND     QQFND   3
     C******************** MOVE SCFYR     QQFYR   3
     C******************** MOVE SCDTE     QQRNDT  8
     C******************** MOVE SCEMOR    QQEMOR  1
     C******************** MOVE SCAGOR    QQAGOR  1
     C******************** MOVE UPOUTQ    QQOUTQ 10
     C******************** MOVE 'D'O#     QQMODE  1
     C******************** CALL 'W4080C'  QQCO#   3
     C*********************************** QQCO#   3
     C*********************************** QQFND   3
     C*********************************** QQFYR   3
     C*********************************** QQRNDT  3
     C*********************************** QQEMOR  3
     C*********************************** QQAGOR  3
     C*********************************** QQOUTQ  3
     C*********************************** QQMODE  3
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      UPFND         QQFUND            3
     C                   MOVE      SCPFDT        QQPFDT            8
     C                   MOVE      SCPTDT        QQPTDT            8
     C                   MOVE      SCRNDT        QQRNDT            8
     C                   MOVE      SCRDTS        QQRDTS            1
     C                   MOVE      SCEMTP        QQEMTP            1
     C                   MOVE      'E'           QQACST            1
     C                   MOVE      SCPSRT        QQPSRT            2
     C                   MOVE      SCSSRT        QQSSRT            2
     C                   MOVE      'M'           QQEMFG            1
     C                   MOVE      'M'           QQAGFG            1
     C                   MOVE      ' '           QQPREM            1
     C                   MOVE      *BLANKS       QQEMP#            9
     C                   MOVE      *BLANKS       QQDIV             5
     C                   MOVE      *ZEROS        QQFYR             3
     C                   MOVE      XXRGTP        QQRGTP            2
     C   56              MOVEL     SCEFNR        QQ1LRG           15
     C   56              MOVEL     SCETNR        QQ1HRG           15
     C  N56              MOVE      XXRNG1        QQ1LRG           15
     C  N56              MOVE      XXRNG2        QQ1HRG           15
     C                   MOVE      XXRGT2        QQRGT2            2
     C   57              MOVEL     SCAFNR        QQ2LRG           15
     C   57              MOVEL     SCATNR        QQ2HRG           15
     C  N57              MOVE      XXAFBR        QQ2LRG           15
     C  N57              MOVE      XXATBR        QQ2HRG           15
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   MOVE      'D'           QQMODE            1
     C*
     C                   CALL      'W4080C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFUND
     C                   PARM                    QQPFDT
     C                   PARM                    QQPTDT
     C                   PARM                    QQRNDT
     C                   PARM                    QQRDTS
     C                   PARM                    QQEMTP
     C                   PARM                    QQACST
     C                   PARM                    QQPSRT
     C                   PARM                    QQSSRT
     C                   PARM                    QQEMFG
     C                   PARM                    QQAGFG
     C                   PARM                    QQPREM
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQFYR
     C                   PARM                    QQRGTP
     C                   PARM                    QQ1LRG
     C                   PARM                    QQ1HRG
     C                   PARM                    QQRGT2
     C                   PARM                    QQ2LRG
     C                   PARM                    QQ2HRG
     C                   PARM                    QQOUTQ
     C                   PARM                    QQMODE
     C*** END   CHG *** 01/16/98 **************************************
     C*  End the Program
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
