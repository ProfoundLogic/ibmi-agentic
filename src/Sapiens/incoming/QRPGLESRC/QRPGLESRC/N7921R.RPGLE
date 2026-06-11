     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*                                                               *
     H* PROGRAM:      N7921R                                          *
     H* DESCRIPTION:  HOSPITAL UTILIZATION REPORT - SCREEN PROGRAM    *
     H* CREATED BY: KEN KENNEDY     DATE: 12/08/89                    *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  11/02/09 C:232 Project: 139WMR                               *
     H*                 OFAC Reporting-Add Pending Status Indicator.  *
     H*                                                               *
     H*  07/14/98 C:61  Project: 98MAY                                *
     H*                 Restrict access to provider types 1 and 4     *
     H*                 only.                                         *
     H*                                                               *
     H*  03/25/98 C:53  Project: MC145
     H*                 SWAP F0533R/31R TO F0032R (NEW VERSION)
     H*                                                        *
     H* 02/19/98 C:62  Project: MC159                                 *
     H*                ** UPDATE PARM LIST FOR F0534R.                *
     H*                                                               *
     H* 01/15/90  LJD - IF NON-PAR PROVIDER - USE F7921/F7920 SERIES  *
     H*                 IF PAR PROVIDER - USE F7923/F7922 SERIES      *
     H*                                                               *
     H* 01/08/90  LJD - MADE FROM/TO DATES REQUIRED ON PROMPT SCREEN  *
     H*                                                               *
     H* 12/26/89 LJD   PROJ #: I0046                                  *
     H*                - ADD SUBSCRIBER # AS A PROMPT                 *
     H*                - ADD PROVIDER ALPHA SEARCH                    *
     H*                - ADD SUBSCIRBER SEARCH                        *
     H*                - DON'T FILL PROMPT WITH USER PARM VALUES      *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*      20     INVALID PROVIDER                                  *
     H*      21     INVALID GROUP                                     *
     H*      22     INVALID FROM DATE                                 *
     H*      23     INVALID TO DATE                                   *
     H*      24     MUST BE IN DESCENDING ORDER                       *
     H*      25     MUST BE D, S, OR B                                *
     H*      26     INVALID SUBSCRIBER                                *
     H*      31     USED TO FIND PROVIDERS & MULTIPLE MODIFIERS       *
     H*****************************************************************
     FN7921F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
      ***** BEG DEL ******* 07/14/98 **********************************
     F*FMPDL   IF  E           K        DISK
      ***** END DEL ******* 07/14/98 **********************************
      ***** BEG ADD ******* 07/14/98 **********************************
     FFMPDL3    IF   E           K DISK
      ***** END ADD ******* 07/14/98 **********************************
     FFMSML     IF   E           K DISK

     D*
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
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
     C*   KEY LIST FOR SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#            5 0
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND             3 0
     C*
     C*  PAYEE DEMOGRAPHIC RECORD
     C*
     C     PDKEY         KLIST
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C*
     C     PDKEY1        KLIST
     C                   KFLD                    SCFEIN
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
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
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C*
     C*  GET COMPANY NAME
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       SCCONM
     C                   ELSE
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   END
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     0             SCTDT
     C                   Z-ADD     0             SCFDT
     C                   MOVE      *BLANKS       SCRTYP
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C     *IN11         IFEQ      '1'
     C*************************************
     C*   COMMAND 03 = EXIT               *
     C*************************************
     C     *INKC         IFEQ      '1'
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C*************************************
     C*   COMMAND 06 = SUBSCRIBER SEARCH  *
     C*************************************
     C     *INKF         IFEQ      '1'
     C                   CALL      'F0545R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCSUB#
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     QQRTC         IFEQ      'B'
     C     SCSUB#        OREQ      *ZERO
     C     QQRTC         OREQ      '*'
     C                   GOTO      STRT
     C                   END                                                    END EQ B/0/*
     C                   END                                                    END *INKF
     C*************************************
     C*   COMMAND 07 = GROUP SEARCH       *
     C*************************************
     C     *INKG         IFEQ      '1'
     C                   CALL      'F0508R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCSUB#
     C                   PARM                    SCFND
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     QQRTC         IFEQ      'B'
     C     SCFND         OREQ      *ZERO
     C     QQRTC         OREQ      '*'
     C                   GOTO      STRT
     C                   END                                                    END EQ B/0/*
     C                   END                                                    END *INKG
     C*************************************
     C*   COMMAND 08 = PROVIDER SRCH ALP  *
     C*************************************
     C     *INKH         IFEQ      '1'
     C*** END DEL *** 03/25/98 ***************************
     C********             CALL 'F0531R'
     C********             PARM           QQPRDS
     C********             PARM           QQCALL
     C********             PARM           SCFEIN
     C********             PARM           SCMOD
     C********             PARM           QQRTC   1
     C*** END DEL *** 03/25/98 ***************************
     C*** BEG ADD *** 03/25/98 ***************************
     C                   MOVE      SCFEIN        XXFEIN            9
     C                   MOVE      SCMOD         XXMOD             3
     C                   CALL      'F0032R'
     C                   PARM      '2'           QQCALL
     C                   PARM                    XXFEIN
     C                   PARM                    XXMOD
     C                   PARM                    QQRTCD            1
     C                   PARM      ' '           QQTYPE            1
      ***** BEG ADD ******* 07/14/98 **********************************
     C                   PARM      'Y'           ##1N4             1            type 1 & 4 only
      ***** END ADD ******* 07/14/98 **********************************
      *
     C                   MOVE      XXFEIN        SCFEIN
     C                   MOVE      XXMOD         SCMOD
     C*** END ADD *** 03/25/98 ***************************
     C     QQRTC         CABEQ     'A'           TERM
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     QQRTC         IFEQ      'B'
     C     SCFEIN        OREQ      *ZERO
     C                   GOTO      STRT
     C                   END                                                    END EQ B/0
     C     QQRTCD        IFEQ      '*'
     C     SCFEIN        IFNE      *ZERO
     C                   MOVE      '1'           *IN31
     C                   END                                                    END NE 0
     C                   GOTO      STRT
     C                   END                                                    END EQ *
     C                   END                                                    END *INKH
     C*************************************
     C*   COMMAND 09 = PROVIDER SRCH NUM  *
     C*************************************
     C     *INKI         IFEQ      '1'
     C                   MOVE      '0'           *IN31
     C*** BEG DEL *** 03/25/98 ***************************
     C*******              MOVE SCFEIN    QQFEIN  90
     C*******              MOVE SCMOD     QQMOD   30
     C*******              MOVE *BLANK    QQTYPE  1
     C*******              CALL 'F0532R'
     C*******              PARM           QQPRDS
     C*******              PARM           QQCALL
     C*******              PARM           QQFEIN
     C*******              PARM           QQMOD
     C*******              PARM           QQTYPE  1
     C*******              PARM           QQRTCD  1
     C*******              MOVELQQFEIN    SCFEIN
     C*******              MOVELQQMOD     SCMOD
     C*** END DEL *** 03/25/98 ***************************
     C*** BEG ADD *** 03/25/98 ***************************
     C                   MOVE      SCFEIN        XXFEIN
     C                   MOVE      SCMOD         XXMOD
     C                   CALL      'F0032R'
     C                   PARM      '2'           QQCALL
     C                   PARM                    XXFEIN
     C                   PARM                    XXMOD
     C                   PARM                    QQRTCD            1
     C                   PARM      ' '           QQTYPE            1
      ***** BEG ADD ******* 07/14/98 **********************************
     C                   PARM      'Y'           ##1N4             1            type 1 & 4 only
      ***** END ADD ******* 07/14/98 **********************************
      *
     C                   MOVE      XXFEIN        SCFEIN
     C                   MOVE      XXMOD         SCMOD
     C*** END ADD *** 03/25/98 ***************************
     C     QQRTCD        CABEQ     'A'           TERM
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     QQRTCD        IFEQ      'B'
     C     QQRTCD        OREQ      ' '
     C                   GOTO      STRT
     C                   END                                                    END EQ B/BLK
     C     QQRTCD        IFEQ      '*'
     C     QQFEIN        IFNE      *ZERO
     C                   MOVE      '1'           *IN31
     C                   END                                                    END NE 0
     C                   GOTO      STRT
     C                   END                                                    END EQ *
     C                   END                                                    END *INKI
     C*************************************
     C*   *IN10 - PROCESS HELP REQUESTS   *
     C*************************************
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
     C*************************************
     C*   COMMAND 13 - CHG USER PARAMETERS*
     C*************************************
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'F0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   GOTO      STRT01
     C                   END                                                    END *INKM
     C*
     C                   END                                                    END CMDKEYS
     C***************************************************************
     C*
     C*  PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C     SCFEIN        IFNE      *ZEROS
     C*
     C*  VALIDATE PROVIDER AND ALLOW MODIFIER SELECTION
     C*
     C     *IN31         IFEQ      '0'
     C*
     C*  THIS BLOCK OF CODE IS EXECUTED IF A CHAIN USING
     C*  PROVIDER AND MODIFIER WAS UNSUCCESSFUL. A CHAIN
     C*  USING A PARTIAL KEY IS THEN USED TO TRY TO FIND
     C*  AT LEAST ONE RECORD WITH A MATCHING PROVIDER.
     C*
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '1'
     C     PDKEY1        CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C     PDKEY1        READE     PPD01                                  60
     C     *IN60         IFEQ      '0'
     C     *IN60         OREQ      '1'
     C     SCMOD         ANDNE     *ZERO
     C                   MOVE      SCFEIN        QQFEIN            9 0
     C                   MOVE      SCMOD         QQMOD             3 0
     C                   MOVE      *BLANK        QQTYPE            1
     C                   CALL      'F0534R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQFEIN
     C                   PARM                    QQMOD
     C                   PARM                    QQTYPE
     C*** BEGIN ADD *** 02/19/98 **************************************
     C                   PARM                    QQNWCD            5
     C                   PARM                    QQEFDT            8
     C*** END   ADD *** 02/19/98 **************************************
     C                   PARM                    QQRTCD
     C*
     C                   MOVEL     QQFEIN        SCFEIN
     C                   MOVEL     QQMOD         SCMOD
     C                   WRITE     SCR99
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     QQRTCD        CABEQ     'A'           TERM
     C     QQRTCD        IFEQ      'B'
     C     QQFEIN        OREQ      *ZERO
     C     QQMOD         ANDEQ     *ZERO
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '*'
     C     QQFEIN        IFNE      *ZERO
     C                   MOVE      '1'           *IN31
     C                   END
     C                   GOTO      SCR01T
     C                   END
     C                   ELSE
     C                   MOVE      PDMOD         SCMOD
     C                   END
     C                   ELSE
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  THIS BLOCK OF CODE IS EXECUTED IF A CHAIN USING
     C*  PROVIDER AND MODIFIER WAS SUCCESSFUL. A CHAIN IS
     C*  THEN PERFORMED WITH A PARTIAL KEY TO SEE IF ANY
     C*  MORE RECORDS WITH THE SAME PROVIDER NUMBER EXIST
     C*  WITH POSSIBLY DIFFERENT MODIFIERS
     C*
     C                   ELSE
     C     PDKEY1        READE     PPD01                                  60
     C     *IN60         IFEQ      '0'
     C     SCMOD         ANDEQ     *ZERO
     C                   MOVE      SCFEIN        QQFEIN            9 0
     C                   MOVE      SCMOD         QQMOD             3 0
     C                   MOVE      *BLANK        QQTYPE            1
     C                   CALL      'F0534R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQFEIN
     C                   PARM                    QQMOD
     C                   PARM                    QQTYPE
     C*** BEGIN ADD *** 02/19/98 **************************************
     C                   PARM                    QQNWCD            5
     C                   PARM                    QQEFDT            8
     C*** END   ADD *** 02/19/98 **************************************
     C                   PARM                    QQRTCD
     C*
     C                   MOVEL     QQFEIN        SCFEIN
     C                   MOVEL     QQMOD         SCMOD
     C                   WRITE     SCR99
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     QQRTCD        CABEQ     'A'           TERM
     C     QQRTCD        IFEQ      'B'
     C     QQFEIN        OREQ      *ZERO
     C     QQMOD         ANDEQ     *ZERO
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '*'
     C     QQFEIN        IFNE      *ZERO
     C                   MOVE      '1'           *IN31
     C                   END
     C                   GOTO      SCR01T
     C                   END
     C                   END
     C                   END
     C                   END                                                    *IN31 = 0
     C*
     C*  RETRIEVE SELECTED PAYEE RECORD,
     C*  PAYEE(PROVIDER) MUST BE ACTIVE
     C*
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C*** BEG CHG *** 11/02/09 **************************************
     C****       PDSTAT    CABEQ'I'       STRT           20
     C     PDSTAT        IFNE      'A'
     C     PDSTAT        ANDNE     *BLANKS
     C                   SETON                                            20
     C                   GOTO      STRT
     C                   ENDIF
     C*** END CHG *** 11/02/09 **************************************
     C                   END
     C*
     C*  VALIDATE SELECTED PROVIDER AND MODIFIER
     C*
     C     *IN31         IFEQ      '1'
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ELSE
     C*** BEG CHG *** 11/02/09 **************************************
     C****       PDSTAT    CABEQ'I'       STRT           20
     C     PDSTAT        IFNE      'A'
     C     PDSTAT        ANDNE     *BLANKS
     C                   SETON                                            20
     C                   GOTO      STRT
     C                   ENDIF
     C*** END CHG *** 11/02/09 **************************************
     C                   MOVE      PDPARN        XXPARN            1            PAR/NONPAR FLAG
     C                   END                                                    *IN60 = 1
     C                   END                                                    *IN31 = 1
     C*
     C                   ELSE                                                   ELSE FEIN <>0
     C                   MOVE      'P'           XXPARN            1            PAR/NONPAR FLAG
     C                   END                                                    END FEIN <> 0
     C*
     C*  SUBSCRIBER MUST BE VALID, IF NOT BLANK
     C*
     C     SCSUB#        IFNE      *ZEROS
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN26                          INVLD SUB#
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  GROUP MUST BE VALID, IF NOT BLANK
     C*
     C     SCFND         IFNE      *ZEROS
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN21                          INVLD GRP#
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  VALIDATE THE DATE ENTERED
     C*
     C*********  SCFDT     IFNE *ZEROS
     C*********  SCTDT     ORNE *ZEROS
     C*
     C*
     C                   Z-ADD     SCFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFDT
     C*
     C     SRERR         CABEQ     '1'           STRT                     22
     C*
     C*
     C                   Z-ADD     SCTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTDT
     C*
     C     SRERR         CABEQ     '1'           STRT                     23
     C*
     C                   MOVE      SCFDT         SRFRDC            8 0
     C                   MOVE      SCTDT         SRTODC            8 0
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    SRFRDC
     C                   PARM                    SRTODC
     C                   PARM                    SRDAYS            5 0
     C                   PARM      ' '           SRERR             1
     C                   PARM      ' '           SRTERM            1
     C*
     C     SRERR         CABEQ     '1'           STRT                     24
     C*                                                    RANGE?
     C*
     C*   THE REPORT TYPE MUST BE 'D', 'S', OR 'B'
     C*
     C     SCRTYP        IFNE      'D'
     C     SCRTYP        ANDNE     'S'
     C     SCRTYP        ANDNE     'B'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCSUB#        QQSUB#            5
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFEIN        QQPROV            9
     C                   MOVE      SCMOD         XXMOD             3
     C                   MOVE      SCFDT         QQFDT             8
     C                   MOVE      SCTDT         QQTDT             8
     C                   MOVE      SCRTYP        QQRTYP            1
     C*
     C     XXPARN        IFNE      'P'                                          NON-PAR PROVIDER
     C                   CALL      'N7921C'
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQPROV
     C                   PARM                    XXMOD
     C                   PARM                    QQFDT
     C                   PARM                    QQTDT
     C                   PARM                    QQRTYP
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
     C*
     C                   ELSE                                                   ELSE EQ 'N'
     C*
     C                   CALL      'N7923C'                                     PAR PROVIDER(S)
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQPROV
     C                   PARM                    XXMOD
     C                   PARM                    QQFDT
     C                   PARM                    QQTDT
     C                   PARM                    QQRTYP
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
     C                   END                                                    END EQ 'N'
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
