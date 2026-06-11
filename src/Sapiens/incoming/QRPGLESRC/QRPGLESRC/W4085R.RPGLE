     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W4085R - Prompt - Large Loss Report
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  05/14/01  C:136 PROJECT: WC577                               *
     H*                  Display location of output file.             *
     H*                                                               *
     H*  10/09/00  C:61  PROJECT WC1005                               *
     H*                  Allow SCFYR = BLANKS, for all policy years.  *
     H*                  Add Claim Status select (open, closed, both).*
     H*                  Add Output Type select (Print, File, Both).  *
     H*                                                               *
     H*  09/24/98 C:78 PROJECT: WC577                                 *
     H*                DISPLAY Q1PGM INSTEAD OF W4085F IN SCREEN.     *
     H*                                                               *
     H*  03/12/98 C:85 PROJECT: WC770                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                                                               *
     H*  06/01/93 C:11 PROJECT WC240                                  *
     H*                ADD "AS OF DATE" TO PROMPT TO SELECT ONLY      *
     H*                TRANSACTIONS <= THIS DATE.                     *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     F*
     F* Prompt Screen File
     FW4085F    CF   E             WORKSTN
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
      ***** BEG ADD ******* 10/09/00 **********************************
     F                                     INFDS(FNFDS)
      ***** END ADD ******* 10/09/00 **********************************
     F*
     F* Fund Detail File
     FWDF1L     IF   E           K DISK

     D*
     D***** BEG ADD ******* 10/09/00 **********************************
     D*
     D FNFDS           DS
     D  FNRRN                397    400B 0
     D*
     D***** END ADD ******* 10/09/00 **********************************
     D*                                                               *
     D*** BEGIN ADD *** 03/12/98 ***********************************************
     D OPTNM           C                   CONST('Large Loss Report')
     D*** END   ADD *** 03/12/98 ***********************************************
     D*
     D*** BEGIN ADD  ***  05/14/01  ********************************************
     D OPTSEL          C                   CONST(', if selected.')
     D*** END   ADD  ***  05/14/01  ********************************************
     D*
     D* System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* Screen Number Reference
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D* External Program File Information
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

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
     C*
     C                   Z-ADD     0             SCLAMT
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
     C                   MOVE      UPFYR         SCFYR
     C*
     C* Get the Requested Record & Load Information
     C*
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C*
     C*** BEGIN CHG *** 03/12/98 **************************************
     C*
     C**********           MOVE QQPRDS    SCPRDS
     C*
     C*  CENTER PROGRAM NAME
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        SCPRDS
     C                   MOVEL(P)  ##DATA        QQPRDS
     C*
     C*** END   CHG *** 03/12/98 **************************************
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
     C*
     C* Load the Record Format Infomation for the First Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*** BEG ADD ******** 05/14/01 ********************************************
     C*
     C*   DISPLAY LOCATION OF OUTPUT FILE
     C*
     C                   CALL      'SA030R'
     C                   PARM                    QQSUFX            3
     C     QQSUFX        IFEQ      *BLANKS
     C     'PDSRPT'      CAT(P)    OPTSEL        XXRPTL           23
     C                   ELSE
     C     'PDSRPT'      CAT(P)    QQSUFX        XXRPTL
     C                   CAT(P)    OPTSEL:0      XXRPTL
     C                   ENDIF
     C                   MOVE      XXRPTL        SCRPTL
     C*
     C*** END ADD ******** 05/14/01 ********************************************
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
     C* COMMAND 01 = End of Job
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C* Process HELP Requests
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
      ***** BEG ADD ******* 10/09/00 **********************************
      *
      * Search Fund Detail (Group Detail) - Search Policy Year
      *     given fund
      *
     C     *INKJ         IFEQ      *ON
      *
     C     HFLD          IFEQ      'SCFYR'
     C                   CALL      'W0011R'
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      FNRRN         ##RRN             9 0
     C                   PARM      *ZERO         ##YR              3 0
      *
     C     ##YR          IFEQ      *ZERO
     C                   SETON                                        25
     C                   ELSE
     C                   Z-ADD     ##YR          SCFYR
     C                   SETON                                        26
     C                   ENDIF                                                  ##YR
      *
     C                   ENDIF                                                  SCFYR
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKJ
      *
      *
      ***** END ADD ******* 10/09/00 **********************************
     C*
     C* Process Change of User Parameters
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C* Get the User Parameter Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C* Process Request Screen
     C*
     C     SCR01T        TAG
     C*
     C* Validate the Fund Year Entered
     C*
      ***** BEG ADD ******* 10/09/00 **********************************
     C     SCFYR         IFNE      *ZERO
      *
      ***** END ADD ******* 10/09/00 **********************************
     C                   Z-ADD     SCFYR         XXFYR
     C     F1KEY         CHAIN     PF101                              60
      *
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
      ***** BEG ADD ******* 10/09/00 **********************************
      *
     C                   ENDIF                                                  SCFYR
      ***** END ADD ******* 10/09/00 **********************************
     C*
     C* Validate the Loss Amount Entered - Cannot be Less than (<) Zero
     C*
     C     SCLAMT        IFLT      0
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C* "AS OF" DATE MUST BE A VALID DATE.
     C*
     C     SCDATE        CABEQ     *ZERO         STRT                     22
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
      ***** BEG ADD ******* 10/09/00 **********************************
      *
      * Claim status must be Open / Closed / Both
      *
     C     SCOCB         IFNE      'O'
     C     SCOCB         ANDNE     'C'
     C     SCOCB         ANDNE     'B'
     C                   SETON                                        23
     C                   GOTO      STRT
     C                   END
      *
      * Output type must be Print / File / Both
      *
     C     SCPFB         IFNE      'P'
     C     SCPFB         ANDNE     'F'
     C     SCPFB         ANDNE     'B'
     C                   SETON                                        24
     C                   GOTO      STRT
     C                   END
      *
      ***** END ADD ******* 10/09/00 **********************************
     C*
     C* Call the CLP to Submit the Report
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCLAMT        QQLAMT           11
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   MOVE      SCDATE        QQDATE            8
      *
     C                   CALL      'W4085C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQLAMT
     C                   PARM                    QQOUTQ
     C                   PARM                    QQDATE
      ***** BEG ADD ******* 10/09/00 **********************************
     C                   PARM                    SCOCB
     C                   PARM                    SCPFB
      ***** END ADD ******* 10/09/00 **********************************
     C*
     C*
     C* End the Program
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
