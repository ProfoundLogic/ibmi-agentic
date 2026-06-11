     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE U7071R - PROVIDER ANALYSIS REPORT
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    PROMPT PROGRAM FOR PROVIDER ANALYSIS REPORT                *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    UTILIZATION REVIEW MENU                                    *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    F0545R         SUBSCRIBER SEARCH                           *
     H*    F0508R         GROUP SEARCH                                *
     H*    F0690R         EMPLOYER SEARCH                             *
     H*                                                               *
     H*  PROGRAMMER: KEN RUTHVEN        PROJECT: 99AAA                *
     H*  CREATED: 12/01/92                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  12/18/24 C:342 Project : MC447 Phase 2                       *
     H*                 1.F10 Search disabled on the Group Field      *
     H*                 DSPF Changes:                                 *
     H*                 1.Changed Existing Group field into Typeahead *
     H*                                                               *
     H*  03/14/00 C:62  Project: URMNT                                *
     H*                 CHANGE EMP# TO 9.0 FROM 5.0.                  *
     H*                                                               *
     H*  05/06/98 C:91  Project: MC181                                *
     H*                 COMBINED F6 & F7 KEYS INTO ONE F10 SEARCH KEY *
     H*                 SWAP F0545R TO F0055R ON SUBSCRIBER SEARCH    *
     H*                 SWAP F0508R TO F0060R ON GROUP SEARCH         *
     H*                 SWAP F0690R TO F0140R ON EMP AND DIV SEARCH   *
     H*                 DELETED CHG USER PARMS                        *
     H*                 CHANGED SCREEN DESIGN TO CURRENT STANDARDS    *
     H*                                                               *
     H*  12/20/96   C:59     REQ/PRJ: MCMNT                           *
     H*             ADDED NEW PARM TO CALL FOR EMPLOYER SEARCH.  NEW  *
     H*             FIELD WAS CREATED IN MCMOD3 ENVIRONMENT.          *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H* SCR01:                                                        *
     H*  20        POSIT CURSOR @ SUB# IF NO SELECTION FROM SEARCH    *
     H*  21        POSIT CURSOR @ GROUP IF NO SELECCTION FROM SEARCH  *
     H*  22        POSIT CURSOR @ EMP# IF NO SELECCTION FROM SEARCH   *
     H*  23        POSIT CURSOR @ TYPE AFTER EMPLOYER SEARCH          *
     H*  27        EMP# WAS LEFT BLANK DO NOT VALIDATE                *
     H*  31        INVALID SUBSCRIBER ENTERED                         *
     H*  32        INVALID GROUP NUMBER ENTERED                       *
     H*  33        INVALID EMPLOYER NUMBER                            *
     H*  37        INVALID REPORT TYPE SELECTED                       *
     H*  38        SUBSCRIBER CANNOT BE EQUAL TO BLANKS               *
     H*  39        GROUP CANNOT BE LEFT BLANK                         *
     H*  40        POSITION CURSOR @ DIVISION                         *
     H*  41        MC40004-CURSOR NOT ON A PROPER FIELD FOR SEARCH    *
     H*  60        GENERAL CHAIN                                      *
     H*****************************************************************
     FU7071F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
      **** BEGIN ADD **** 05/06/98 ************************************
     F                                     INFDS(DBFDB)
      **** END   ADD **** 05/06/98 ************************************
     FFMEML     IF   E           K DISK
     FFMSML     IF   E           K DISK
      **** BEGIN ADD **** 05/06/98 ************************************
     FFMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
     FFMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
      **** END   ADD **** 05/06/98 ************************************

     D*
      **** BEGIN DEL **** 05/06/98 ************************************
     D**********          CK1A    1   3 78               CMD KEYS FOR SCR01
      **** END   DEL **** 05/06/98 ************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      **** BEGIN ADD **** 05/06/98 ************************************
     D DBFDB           DS
     D  DBRRB                397    400B 0
      **** END   ADD **** 05/06/98 ************************************

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
     C                   KFLD                    SCSUB#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C*
     C*   KEY LIST FOR THE EMPLOYER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
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
     C                   MOVE      *ZERO         UPSUB#
     C                   ELSE
     C                   MOVE      UPSUB#        SCSUB#
     C                   MOVE      UPFND         SCFND
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
      **** BEGIN DEL **** 05/06/98 ************************************
     C**********           MOVE Q1USER    SCUSER
      **** END   DEL **** 05/06/98 ************************************
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN22
      **** BEGIN DEL **** 05/06/98 ************************************
     C**********           Z-ADD1         CK
     C**********           MOVELCK1A,CK   SCCMD1
      **** END   DEL **** 05/06/98 ************************************
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C***********************************
     C*   F3 = EXIT                     *
     C***********************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
      **** BEGIN CHG **** 05/06/98 ************************************
     C************************************
     C*   F6                             *
     C************************************
     C********** *INKF     IFEQ '1'
     C************************************
     C*   SCR01 = SUSCRIBER SEARCH       *
     C************************************
     C**********           CALL 'F0545R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           SCSUB#
     C**********           PARM           QQRTCD  1
     C********** QQRTCD    CABEQ'A'       TERM
     C**********
     C********** QQRTCD    IFEQ 'B'
     C********** SCSUB#    OREQ *ZERO
     C**********           MOVE '1'       *IN20
     C**********           GOTO STRT
     C**********           END                             END EQUAL B
     C**********
     C********** QQRTCD    IFEQ '*'
     C**********           MOVE '1'       *IN21
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           END                             END *
     C**********           END                             END *INKF
     C***********************************
     C*   F7                            *
     C***********************************
     C********** *INKG     IFEQ '1'
     C************************************
     C*   SCR01 = GROUP SEARCH           *
     C************************************
     C**********
     C********** SCSUB#    IFEQ *ZEROS
     C**********           MOVE UPSUB#    QQSUB   50
     C**********           ELSE
     C**********           MOVE SCSUB#    QQSUB
     C**********           END
     C**********           CALL 'F0508R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQSUB
     C**********           PARM           SCFND
     C**********           PARM           QQRTC   1
     C********** QQRTC     CABEQ'A'       TERM
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** QQRTC     IFEQ 'B'
     C********** SCFND     OREQ *ZERO
     C**********           MOVE '1'       *IN21
     C**********           GOTO STRT
     C**********           END                             END EQUAL B
     C**********
     C********** QQRTC     IFEQ '*'
     C**********           MOVE QQSUB     SCSUB#
     C**********           MOVE '1'       *IN22
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           END                             END *
     C**********
     C**********           END                             END *INKG
     C**********************************
     C*   F8                           *
     C**********************************
     C********** *INKH     IFEQ '1'
     C**********************************
     C*   SCR01 = EMPLOYER # SEARCH    *
     C**********************************
     C*** BEGIN ADD *** 12/20/96 *************************************
     C*   ADDED QQDIV TO CALL, UPDATE TO MCMOD3 VERSION ADDED DIV TO
     C*   EMPLOYER SEARCH.
     C**********
     C**********           MOVEL'000'     QQDIV   30
     C*** END ADD ***** 12/20/96 *************************************
     C**********           CALL 'F0690R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           SCEMP#
     C**********           PARM           QQDIV            ADD 12/20/96
     C**********           PARM           QQRTCD  1
     C**********
     C********** QQRTCD    CABEQ'A'       TERM
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** QQRTCD    IFEQ 'B'
     C********** SCEMP#    OREQ *ZERO
     C**********           MOVE '1'       *IN22
     C**********           GOTO STRT
     C**********           END
     C**********
     C********** QQRTCD    IFEQ '*'
     C**********           MOVE '1'       *IN23
     C**********           GOTO STRT
     C**********           END
     C**********
     C**********           END                             END *INKH
     C****************************************
     C*      F24 = MORE COMMAND KEYS         *
     C****************************************
     C********** *INKY     IFEQ '1'
     C**********           MOVELQ2RECF    HLRECF 10
     C********** CK        ADD  1         CK      50
     C**********
     C********** HLRECF    IFEQ 'SCR01'
     C********** CK        IFGT 2
     C**********           Z-ADD1         CK
     C**********           END                             END CK GT 2
     C**********           MOVELCK1A,CK   SCCMD1
     C**********           END                             END SCR01
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVELHLRECF    Q2RECF
     C**********           GOTO STRT
     C**********           END                             END *INKY
     C**************************************
     C*   PROCESS CHANGE OF USER PARAMETERS*
     C**************************************
     C********** *INKM     IFEQ '1'
     C**********
     C**********           CALL 'F0006R'
     C**********
     C*  GET THE USER PARAMETER FILE
     C**********
     C********** UPKEY     CHAINPUP01                60
     C********** *IN60     IFEQ '1'
     C**********           MOVE *ZERO     UPCO#
     C**********           MOVE *ZERO     UPSUB#
     C**********           ELSE
     C**********           MOVE UPSUB#    SCSUB#
     C**********           MOVE UPFND     SCFND
     C**********           END
     C**********           GOTO STRT01
     C**********           END
     C*
      ***************************
      * F10 = SEARCH            *
      ***************************
     C     *INKJ         IFEQ      *ON
     C     Q2RECF        IFEQ      'SCR01'
     C*
      *********************
      * SUBSCRIBER SEARCH *
      *********************
     C     HFLD          IFEQ      'SCSUB#'
     C*
     C                   CALL      'F0055R'
     C*
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRN             9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           STRT                     20     PC AT SUB#
     C*
     C     ##RRN         CHAIN     PSM02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      SMSUB#        SCSUB#
     C                   MOVE      *ON           *IN21                          PC AT SCFND
     C                   ELSE
     C                   MOVE      *ON           *IN20                          PC AT SCSUB#
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
     C******** Beg Del ***** 12/18/24 ***************************
     C*****
     C*****              MOVE      ##RRN         ##RRN2
     C*****
     C*****              CALL      'F0060R'
     C*****
     C*****              PARM      '2'           ##CALL            1
     C******INKC         PARM                    ##KC              1
     C******INKL         PARM                    ##KL              1
     C*****              PARM                    ##RRN2            9 0
     C*****
     C******INKC         CABEQ     *ON           TERM
     C******INKL         CABEQ     *ON           STRT                     21     PC AT SCFND
     C*****
     C*****##RRN2        CHAIN     PFN02                              60
     C*****
     C******IN60         IFEQ      *OFF
     C*****              MOVE      FNFND         SCFND
     C*****              MOVE      *ON           *IN22                          PC AT SCEMP#
     C*****              ELSE
     C*****              MOVE      *ON           *IN21                          PC AT SCFND
     C*****              ENDIF                                                  *IN60 IFEQ
     C******** End Del ***** 12/18/24 ***************************
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCFND
     C*
      *******************************
      * EMPLOYER & DIVISION SEARCH  *
      *******************************
     C     HFLD          IFEQ      'SCEMP#'
     C     HFLD          OREQ      'FLD001'
     C*
     C     SCFND         IFNE      0
     C     FNKEY         CHAIN     PFN01                              60
     C                   Z-ADD     DBRRB         ##RRN3
     C                   ENDIF                                                  SCFND EQ 0
     C*
     C                   CALL      'F0140R'
     C*
     C                   PARM      '2'           ##CALL
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##RRN                          FMEMP
     C                   PARM                    ##RRN3            9 0          FMFNP
     C                   PARM                    ##VIEW            1 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      0
     C     HFLD          CABEQ     'SCEMP#'      STRT                     22     PC @ EMP#
     C     HFLD          CABEQ     'FLD001'      STRT                     40     PC @ DIV
     C                   ENDIF                                                   *INKL
     C*
     C     ##RRN         CHAIN     PEM02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         FLD001
     C                   MOVE      *ON           *IN23                          PC AT SCTYPE
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCFND
     C*
     C                   MOVE      *ON           *IN41                          ERROR MESSAGE
     C*
     C                   ENDIF                                                  SCR01
     C                   ENDIF                                                  *INKJ
     C*
      **** END   CHG **** 05/06/98 ************************************
     C*
     C                   END
     C***************************
     C*   PROCESS REQUEST SCREEN*
     C***************************
     C     SCR01T        TAG
     C                   MOVE      '0'           *IN27
     C*
     C*  DETERMINE WHAT WAS ENTERED
     C*
     C     SCEMP#        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN27
     C                   END
     C*
     C*  VALIDATE THE SUBSCRIBER
     C*
     C     SCSUB#        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN38
     C                   GOTO      STRT
     C                   END
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN31                          INVALID SUB#
     C                   GOTO      STRT
     C                   END                                                    END IF *IN60
     C*
     C*  VALIDATE THE GROUP
     C*
     C     SCFND         IFEQ      *ZEROS
     C                   MOVE      '1'           *IN39
     C                   GOTO      STRT
     C                   END
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN32                          INVALID FUND
     C                   GOTO      STRT
     C                   END                                                    END IF *IN60
     C*
     C*   VALIDATE THE EMPLOYER NUMBER
     C*
     C     *IN27         IFEQ      '0'
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END                                                    END IF *IN60
     C                   END                                                    END IF *IN27
     C*
     C* VALIDATE THE REPORT TYPE
     C*
     C     SCTYPE        IFNE      'D'
     C     SCTYPE        ANDNE     'B'
     C     SCTYPE        ANDNE     'S'
     C                   MOVE      '1'           *IN37
     C                   GOTO      STRT
     C                   END
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCSUB#        QQSUB#            5
     C                   MOVE      SCFND         QQFND             3
     C*** BEGIN CHG *** 03/14/00 **************************************
     C**********           MOVE SCEMP#    QQEMP#  5
     C                   MOVE      SCEMP#        QQEMP#            9
     C*** END   CHG *** 03/14/00 **************************************
     C                   MOVE      SCTYPE        QQTYPE            1
     C                   CALL      'U7071C'
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQEMP#
     C                   PARM                    QQTYPE
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      **** BEGIN DEL **** 05/06/98 ************************************
     C****** CK1A - COMMAND KEYS for SCR01
      ****** F3=Exit F6=Subscriber Search F7=Group Search F24=More keys
      ****** F8=Employer Search F13=Chg User Parms F24=More keys
      **** END   DEL **** 05/06/98 ************************************
