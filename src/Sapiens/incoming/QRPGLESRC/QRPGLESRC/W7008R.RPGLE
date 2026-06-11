     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7008R01 - PROMPT FOR RELEASE PENDED PAYMENT BY DATE PGM
     H****************************************************************
     H*  DESCRIPTION:                                                *
     H*         THIS PROGRAM PROMPTS THE USER FOR THE DATE IN WHICH  *
     H*         ALL PAYMENTS IN THE REQUEST FOR PAYMENT FILE (WARPP) *
     H*         WILL BE RELEASED.  THIS PROGRAM ELIMINATE THE USER   *
     H*         FROM HAVING TO RELEASE PAYMENTS INDIVIDUALLY.        *
     H*                                                              *
     H*  01/22/93  C:06  MD034                                       *
     H*  CALLED BY:       MENU                                       *
     H*  CALLS TO:        W7008C - SUBMITTER                         *
     H*  CREATED IN BASE: 09/13/93       PGMR: 11      PROJ: WC264   *
     H****************************************************************
     H*  REVISIONS:                                                  *
     H*                                                              *
     H*  07/10/01  C:136 PROJECT: WC577                               *
     H*                  Display location of output file.             *
     H*                                                               *
     H* 11/20/98  C:94 PROJECT: 48MBV                                *
     H*                MAKE YR2000 COMPLIANT, COMPARE FIELDS TO      *
     H*                WARPP                                         *
     H*                                                              *
     H* 03/10/95  C:28 PROJECT:68AEZ                                 *
     H*                ADDED TO PROMPT:                              *
     H*                 1-PENDED PMT STATUS CODE                     *
     H*                 2-RELEASED PMT STATUS CODE                   *
     H*                 3-SUPERVISORS EOMB PEND REPORT (Y/N)         *
     H*                 4-EOMB PAYMENTS OLDER THAN  XXX  DAYS.       *
     H****************************************************************
     H*  INDICATOR USAGE:                                            *
     H*  20   (WCR0500) - INVALID DATE                               *
     H*  21   (WCR0513) - INVALID TABLE FOUND IN FIELD               *
     H*  22   (WCR0501) - VALUE MUST BE Y OR N                       *
     H*  23   (WCR0590) - FIELDS ARE MUTUALLY EXCLUSIVE              *
     H****************************************************************
     FW7008F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     F*
     F*** BEG ADD ****** 03/10/95 ***********************************
     F*WTABL   IF  E           K        DISK
     F*** END ADD ****** 03/10/95 ***********************************
     F*** BEG ADD *** 11/23/98 ***************************************
     F*
     FWTPSL     IF   E           K DISK
     F*
     FWMFNL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     F*** BEG ADD *** 11/23/98 ***************************************

     D                 DS
     D  DSDTTM                 1     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D***  BEGIN ADD  ***  07/10/01  *********************************
     D OPTSEL          C                   CONST(', if selected.')
     D***  END   ADD  ***  07/10/01  *********************************
     D*** BEG ADD *** 11/23/98 ****************************************
     D*****************************************************************
     D* Program constants.
     D*****************************************************************
     D*
     D QQPRDS          C                   CONST('Mass Release of WCM-
     D                                     C Requests')

     I*** END ADD *** 11/23/98 ****************************************

     C*
     C****************************************************************
     C*  KEY LISTING FOR PROGRAM FILES
     C****************************************************************
     C*
     C* Key List For The Compnay Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* Key List For The User Parameter Master File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C**** BEG ADD ****** 03/10/95 ***********************************
     C*
     C* Key List For The Table File
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C**** END ADD ****** 03/10/95 ***********************************
     C*** BEG ADD *** 11/23/98 ***********************************
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXGRP
     C*
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXGRP
     C*
     C*** END ADD *** 11/23/98 ***********************************
     C****************************************************************
     C*  *LIKE DEFINES FOR PROGRAM VARIABLES
     C****************************************************************
     C*
     C     *LIKE         DEFINE    COCO#         XXCO#
     C**** BEG ADD ****** 03/10/95 ***********************************
     C     *LIKE         DEFINE    PSKEY         XXKEY
     C**** END ADD ****** 03/10/95 ***********************************
     C*
     C****************************************************************
     C*  PROGRAM ENTRY PARAMETERS
     C****************************************************************
     C*
     C     *ENTRY        PLIST
     C*** BEG DEL *** 11/23/98 ***************************************
     C***********          PARM           QQPRDS 40
     C*** END DEL *** 11/23/98 ***************************************
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
     C****************************************************************
     C*  START PROGRAM
     C****************************************************************
     C     TOP           TAG
     C*
     C*  Get The User Parameter File
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C                   MOVE      UPCO#         XXCO#
     C*** BEG DEL *** 11/23/98 ***************************************
     C**********           Z-ADDUPPRDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDYY
     C**********           MOVELSRDTO     SCDTE
     C*** END DEL *** 11/23/98 ***************************************
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
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C*** BEG CHG *** 11/23/98 ***************************************
     C                   MOVEL(P)  QQPRDS        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
     C*
     C*** END CHG *** 11/23/98 ***************************************
     C*
     C*   Load the Record Format Info For First Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   Display All Formats
     C*
     C     STRT          TAG
     C*
     C*** BEG ADD ******** 07/10/01 ********************************************
     C*
     C*   DISPLAY LOCATION OF OUTPUT FILE
     C*
     C                   CALL      'SA030R'
     C                   PARM                    QQSUFX            3
     C     QQSUFX        IFEQ      *BLANKS
     C     'PDSFIL'      CAT(P)    OPTSEL        XXRPTL           23
     C                   ELSE
     C     'PDSFIL'      CAT(P)    QQSUFX        XXRPTL
     C                   CAT(P)    OPTSEL:0      XXRPTL
     C                   ENDIF
     C                   MOVE      XXRPTL        SCRPTL
     C*
     C*** END ADD ******** 07/10/01 ********************************************
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*** BEG ADD *** 11/23/98 ***************************************
     C     SCDTE         IFNE      *ZEROS
     C*
     C                   Z-ADD     SCDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTE
     C*
     C                   ENDIF
     C*** END ADD *** 11/23/98 ***************************************
     C                   EXFMT     SCR01
     C                   END
     C*
     C****************************************************************
     C*  COMMAND KEY SECTION - BEGIN
     C****************************************************************
     C     *IN11         IFEQ      '1'
     C*
     C****************
     C*   COMMAND 01 = End of Job
     C****************
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*** BEGIN ADD *** 03/10/95 **************************************
     C*  CMD 10 - TABLE FILE SEARCH -  STATUS CODE
     C*
     C     *INKJ         IFEQ      '1'
     C*** BEG CHG *** 11/23/98 ****************************************
     C                   SELECT
     C     HFLD          WHENEQ    'SCPCDE'
     C**********           MOVELSCPCDE    XX2     2
     C********** XX2       IFEQ '? '
     C*** END CHG *** 11/23/98 ****************************************
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C*** BEG CHG *** 11/23/98 ***************************************
     C                   CALL      'W2142R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      *BLANK        @@KEY            10
     C*
     C                   MOVEL(P)  @@KEY         SCPCDE
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     @@KEY         OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C     *INKL         IFEQ      *OFF
     C     *INKC         ANDEQ     *OFF
     C                   MOVE      '1'           *IN32
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C**********           CALL 'W2143R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           SCKEY
     C**********           PARM           QQRTCD  1
     C**********           MOVELSCKEY     SCPCDE
     C********** QQRTCD    CABEQ'A'       TERM
     C**********
     C********** QQRTCD    IFEQ 'B'
     C********** SCPCDE    OREQ *BLANKS
     C**********           WRITESCR99
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           MOVE '1'       *IN31
     C**********           GOTO STRT
     C**********           ENDIF
     C**********
     C********** QQRTCD    IFEQ '*'
     C**********           MOVE '1'       *IN32
     C**********           GOTO STRT
     C**********           ENDIF
     C*** END CHG *** 11/23/98 ***************************************
     C*
     C*** BEG CHG *** 11/23/98 ***************************************
     C     HFLD          WHENEQ    'SCRCDE'
     C**********           MOVELSCRCDE    XX2     2
     C********** XX2       IFEQ '? '
     C*** END CHG *** 11/23/98 ***************************************
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C*** BEG CHG *** 11/23/98 ***************************************
     C                   CALL      'W2142R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      *BLANK        @@KEY            10
     C*
     C                   MOVEL(P)  @@KEY         SCRCDE
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     @@KEY         OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C     *INKL         IFEQ      *OFF
     C     *INKC         ANDEQ     *OFF
     C                   MOVE      '1'           *IN33
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C**********
     C**********           CALL 'W2143R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           SCKEY
     C**********           PARM           QQRTCD  1
     C**********           MOVELSCKEY     SCRCDE
     C********** QQRTCD    CABEQ'A'       TERM
     C**********
     C********** QQRTCD    IFEQ 'B'
     C********** SCRCDE    OREQ *BLANKS
     C**********           WRITESCR99
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           MOVE '1'       *IN32
     C**********           GOTO STRT
     C**********           ENDIF
     C**********
     C********** QQRTCD    IFEQ '*'
     C**********           MOVE '1'       *IN33
     C**********           GOTO STRT
     C**********           ENDIF
     C*** END CHG *** 11/23/98 ****************************************
     C                   ENDSL
     C                   ENDIF
     C                   ENDIF
     C*
     C*** END ADD ***** 03/10/95 **************************************
     C****************
     C*   Process Help Requests
     C****************
     C*
     C     *IN10         IFEQ      '1'
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
     C                   GOTO      STRT
     C                   END
     C*
     C****************
     C*   Process Change of User Parameters
     C****************
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
     C                   GOTO      TOP
     C                   END
     C*
     C*                    END
     C****************************************************************
     C*  COMMAND KEY SECTION - END
     C****************************************************************
     C*
     C*   Process Request Screen
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C****************
     C*   SCR01 EDITS
     C****************
     C*
     C     SCR01T        TAG
     C*
     C* Validate the Run Date Entered
     C*
     C                   Z-ADD     SCDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTE
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C**** BEG ADD ****** 03/10/95 ***********************************
     C* Validate Pending status code
     C                   MOVEL     SCPCDE        XXKEY
     C     TBKY          CHAIN     PPS01                              21
     C     *IN21         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C     PSTYPE        IFNE      'P'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C* Validate Released Status Code
     C     SCRCDE        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C                   MOVEL     SCRCDE        XXKEY
     C     TBKY          CHAIN     PPS01                              25
     C     *IN25         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C     PSTYPE        IFNE      'R'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C* Validate Supervisors Report Y/N
     C     SCSRP         IFNE      'Y'
     C     SCSRP         ANDNE     'N'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C* Validate Pending days
     C     SCSRP         IFEQ      'Y'
     C     SCOLD         ANDEQ     *ZERO
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C**** END ADD ****** 03/10/95 ***********************************
     C*** BEG ADD *** 11/23/98 ***************************************
     C*
     C     SCGRP         IFNE      *ZEROS
     C                   MOVEL     SCGRP         XXGRP             3 0
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           *IN34                          GRP NOT FOUND
     C                   GOTO      STRT
     C*
     C                   ELSE
     C                   Z-ADD     SCDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C                   ENDIF
     C*
     C     FNEXDT        IFGT      *ZERO                                        Group Expiration DT
     C     FNEXDT        ANDLE     XXDT
     C                   MOVE      '1'           *IN35
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     F2KY          CHAIN     PF201                              61
     C     *IN61         IFEQ      *OFF
     C     F2CLCK        IFEQ      'N'
     C                   MOVE      '1'           *IN36
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD *** 11/23/98 ***************************************
     C*
     C* Call the CLP to Submit the List
     C*
     C                   MOVE      XXCO#         QQCO#             3
     C*** BEG CHG *** 11/23/98 ***************************************
     C                   MOVE      XXGRP         QQGRP             3
     C                   Z-ADD     SCDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QQDATE            7
     C**********           MOVE SCDTE     QQDATE  6
     C*** END CHG *** 11/23/98 ***************************************
     C**** BEG ADD ****** 03/10/95 ***********************************
     C                   MOVE      SCPCDE        QQPCDE            2
     C                   MOVE      SCRCDE        QQRCDE            2
     C                   MOVE      SCSRP         QQSRP             1
     C                   MOVE      SCOLD         QQOLD             5
     C**** END ADD ****** 03/10/95 ***********************************
     C                   CALL      'W7008C'
     C                   PARM                    QQCO#
     C                   PARM                    QQDATE
     C**** BEG ADD ****** 03/10/95 ***********************************
     C                   PARM                    QQPCDE
     C                   PARM                    QQRCDE
     C                   PARM                    QQSRP
     C                   PARM                    QQOLD
     C**** END ADD ****** 03/10/95 ***********************************
     C*** BEG ADD *** 11/23/98 ***************************************
     C                   PARM                    QQGRP
     C*** END ADD *** 11/23/98 ***************************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
