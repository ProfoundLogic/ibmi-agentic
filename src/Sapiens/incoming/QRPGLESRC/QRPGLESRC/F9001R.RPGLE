     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F9001R                                            *
     H*    DESCRIPTION: PROMPT FOR UR REPORT FOR CASES > $20,000      *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  04/22/98  C:91  PROJECT:MC181                                *
     H*                  CHANGED CALLED FILES FOR SEARCHES ON SCSUB#  *
     H*                  AND SCFND FIELDS                             *
     H*                  DELETED F13 KEY (CHG USER PARMS)             *
     H*                                                               *
     H*  04/13/98  C:89  PROJECT:MC181                                *
     H*                  CHANGED F6 AND F7 TO F10 FOR SEARCH          *
     H*                  VERSION 4 STANDARDS                          *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*  20     MCR0508 - INVALID FUND (IF ENTERED)                   *
     H*  21     MCR0502 - AMOUNT MUST BE GREATER THAN ZERO            *
     H*  26     PC AT SCSUB# FIELD                                    *
     H*  60     GENERAL CHAIN                                         *
     H*****************************************************************
     FF9001F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMSML     IF   E           K DISK
      **** BEGIN ADD **** 04/22/98 ************************************
     FFMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
      **** END   ADD **** 04/22/98 ************************************

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
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
     C* Key List for the FUND Master File
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C* Key List for the SUB# Master File
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#
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
     C*  INITIALIZE WORKFIELDS
     C     *LIKE         DEFINE    FNFND         XXFND
     C     *LIKE         DEFINE    SMSUB#        XXSUB#
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C*
     C*  GET COMPANY NAME
     C*
     C     STRT01        TAG
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
     C* Get the Requested Record, Load Information
     C*
     C                   MOVE      SCFND         XXFND
     C                   MOVE      SCSUB#        XXSUB#
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C* Load the Record Format Information for the First Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C* Display Screen Formats
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C******************************************************************
     C*
     C****************************
     C* Process All Command Keys *
     C****************************
     C*
     C     *IN11         IFEQ      '1'
     C*
      ************************
      * F3 = EXIT            *
      ************************
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C*
     C************************
     C* F10 = SEARCH         *
     C************************
     C*
      **** BEGIN ADD **** 04/22/98 *********************************************
     C*
     C     *INKJ         IFEQ      *ON
     C*
      *********************
      * SUBSCRIBER SEARCH *
      *********************
     C     HFLD          IFEQ      'SCSUB#'
     C*
     C                   CALL      'F0055R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRN             9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           STRT                     26     PC AT SUB#
     C*
     C     ##RRN         CHAIN     PSM02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      SMSUB#        SCSUB#
     C                   MOVE      *ON           *IN23                          PC AT SCFND
     C                   ELSE
     C                   MOVE      *ON           *IN26                          PC AT SCSUB#
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCSUB#
     C*
      ****************
      * GROUP SEARCH *
      ****************
     C     HFLD          IFEQ      'SCFND'
     C*
     C                   MOVE      ##RRN         ##RRN2
     C*
     C                   CALL      'F0060R'
     C                   PARM      '2'           ##CALL
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##RRN2            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           STRT                     23     PC AT SCFND
     C*
     C     ##RRN2        CHAIN     PFN02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      FNFND         SCFND
     C                   MOVE      *ON           *IN24                          PC AT SCAMT
     C                   ELSE
     C                   MOVE      *ON           *IN23
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCFND
     C*
     C                   MOVE      *ON           *IN25
     C*
     C                   ENDIF                                                  *INKJ IFEQ *ON
     C*
      **** END   ADD **** 04/22/98 ************************************
     C*
      **** BEGIN DEL **** 04/22/98 ************************************
     C********** *INKF     IFEQ '1'
     C********** SCSUB#    IFEQ *ZEROS
     C**********           MOVE UPSUB#    SCSUB#
     C**********           END
     C**********           CALL 'F0508R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           SCSUB#
     C**********           PARM           SCFND
     C**********           PARM           QQRTC   1
     C********** QQRTC     CABEQ'A'       TERM
     C**********
     C********** QQRTC     IFEQ 'B'
     C********** SCFND     OREQ *ZERO
     C********** QQRTC     OREQ '*'
     C**********           WRITESCR99
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           END                             END EQ B/ZERO
     C**********
     C**********           END                             END *INKF
      **********
     C********** SCSUB#    IFEQ *ZEROS
     C**********           MOVE UPSUB#    SCSUB#
     C**********           ENDIF                           SCSUB# IFEQ *ZEROS
      **********
     C**********           Z-ADDSCSUB#    QXSUB#
     C**********           Z-ADD0         QXFND
      **********
     C********** HFLD      IFEQ 'SCFND'
      **********
     C**********           CALL 'F0508R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QXSUB#  50
     C**********           PARM           QXFND   30
     C**********           PARM           QQRTC   1
      **********
     C********** QQRTC     CABEQ'A'       TERM
      **********
     C********** QQRTC     IFEQ 'B'
     C********** QXFND     OREQ *ZERO
     C**********           MOVE *ON       *IN23
     C**********           GOTO STRT
     C**********           ENDIF                           QQRTC IFEQ 'B'
      **********
     C********** QQRTC     IFEQ '*'
     C**********           MOVE QXFND     SCFND
     C**********           MOVE *ON       *IN24
     C**********           GOTO STRT
     C**********           ENDIF                           QQRTC IFEQ '*'
     C**********
     C**********           ENDIF                           HFLD IFEQ 'SCFND'
     C**********************************
     C*   F10 = SEARCH                 *
     C**********************************
     C********** *INKG     IFEQ '1'
     C**********           CALL 'F0545R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           SCSUB#
     C**********           PARM           QQRTC   1
     C********** QQRTC     CABEQ'A'       TERM
     C**********
     C********** QQRTC     IFEQ 'B'
     C********** SCSUB#    OREQ *ZERO
     C********** QQRTC     OREQ '*'
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           END                             END EQUAL B
     C**********
     C**********
     C**********           END                             END *INKF
     C********** HFLD      IFEQ 'SCSUB#'
      **********
     C**********           CALL 'F0545R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QXSUB#
     C**********           PARM           QQRTC   1
     C********** QQRTC     CABEQ'A'       TERM
     C**********
     C********** QQRTC     IFEQ 'B'
     C********** QXSUB#    OREQ *ZERO
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           ENDIF                           END EQUAL B
     C**********
     C********** QQRTC     IFEQ '*'
     C**********           MOVE QXSUB#    SCSUB#
     C**********           MOVE *ON       *IN23
     C**********           GOTO STRT
     C**********           ENDIF                           END IFEQ *
     C**********
     C**********           ENDIF                           END HFLD
     C**********           MOVE *ON       *IN25
     C**********           GOTO STRT
     C**********
      **** END   DEL **** 04/22/98 *********************************************
     C*
     C************************************
     C* *IN10 - Process HELP Requests    *
     C************************************
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
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *IN10
      **** BEGIN DEL **** 04/22/98 ************************************
     C************************************
     C* F13 = CHG USER PARMS             *
     C************************************
     C********** *INKM     IFEQ '1'
     C**********           CALL 'F0006R'
     C********** UPKEY     CHAINPUP01                60
     C********** *IN60     IFEQ '1'
     C**********           MOVE *ZERO     UPCO#
     C**********           END
     C**********           GOTO STRT01
     C**********           END                             END *INKM
     C**********
      **** END   DEL **** 04/22/98 ************************************
     C*
     C                   END                                                    END CMDKEYS
     C*
     C*********************************************************************
     C* Process Request Screen
     C*
     C     SCR01T        TAG
     C*
     C* Validate the SUB#, if entered.
     C*
     C                   Z-ADD     SCSUB#        XXSUB#
     C     XXSUB#        IFNE      0
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C* Validate the FUND, if entered.
     C*
     C                   Z-ADD     SCFND         XXFND
     C     XXFND         IFNE      0
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C* Validate the AMOUNT
     C*
     C     SCAMT         IFLE      0
     C                   MOVE      '1'           *IN21
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*** ALL EDITS HAVE BEEN PASSED, SUBMIT REPORT
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCSUB#        QQSUB#            5
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCAMT         QQAMT             9
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   MOVE      'D'           QQMODE            1
     C                   CALL      'F9001C'
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQAMT
     C                   PARM                    QQOUTQ
     C                   PARM                    QQMODE
     C*
     C* End the Program
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
