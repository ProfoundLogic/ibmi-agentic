     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE U7051R - INTERVIEWER ACTIVITY REPORT
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    PROMPT PROGRAM FOR INTERVIEWER ACTIVITY REPORT             *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    UTILIZATION REVIEW MENU                                    *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    F0545R         SUBSCRIBER SEARCH                           *
     H*    F0508R         GROUP SEARCH                                *
     H*    F2165R         INTERVIEWER SEARCH                          *
     H*                                                               *
     H*  PROGRAMMER: KEN RUTHVEN        PROJECT: 99AAA                *
     H*  CREATED: 11/30/92                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  12/18/24 C:342 Project : MC447 Phase 2                       *
     H*                 1.F10 Search disabled on the Group Field      *
     H*                 DSPF Changes:                                 *
     H*                 1.Changed Existing Group field into Typeahead *
     H*                                                               *
     H*  04/30/98 C:90  PROJ:MC181                                    *
     H*                 SCR01 - CHANGED TO MEET VERSION 4 STANDARDS   *
     H*                 COMBINED ALL SEARCHES INTO F10 SEARCH         *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H* SCR01:                                                        *
     H*  20        POSIT CURSOR @ SUB# IF NO SELECTION FROM SEARCH    *
     H*  21        INVALID SUBSCRIBER NUMBER ENTERED                  *
     H*  22        SUBSCRIBER CANNOT BE EQUAL TO ZEROS                *
     H*  23        POSIT CURSOR @ GROUP IF NO SELECTION FRON SEARCH   *
     H*  25        INVALID GROUP NUMBER ENTERED                       *
     H*  26        GROUP NUMBER CANNOT BE EQUAL TO ZEROS              *
     H*  27        INVALID REPORT TYPE SELECTED                       *
     H*  28        INVALID FROM DATE                                  *
     H*  29        FROM DATE CANNOT BE LEFT BLANK                     *
     H*  30        INVALID TO DATE                                    *
     H*  31        TO DATE CANNOT BE LEFT BLANKS                      *
     H*  32        INVALID INTERVIEWER ENTERED                        *
     H*  34        POSITION CURSOR AT REPORT TYPE                     *
     H*  35        POSITION CURSOR AT INTERVIEWER                     *
     H*****************************************************************
     FU7051F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FFMSML     IF   E           K DISK
      **** BEGIN ADD **** 04/29/98 ************************************
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
     FFMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP03)
      **** END   ADD **** 04/29/98 ************************************

     D*
      **** BEGIN DEL **** 04/29/98 ************************************
     D******************** K1A    1   3 78               CMD KEYS FOR SCR01
      **** END   DEL **** 04/29/98 ************************************
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
     C     UPKY1         KLIST
     C                   KFLD                    SCINTR
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C*
     C     *LIKE         DEFINE    FNDT          XXFRDT
     C     *LIKE         DEFINE    FNDT          XXTODT
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
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      **** BEGIN CHG **** 04/29/98 ************************************
     C                   MOVE      '1'           *IN20
     C******************** MOVE '1'       *IN35
     C******************** Z-ADD1         CK
     C******************** MOVELCK1A,CK   SCCMD1
      **** END   CHG **** 04/29/98 ************************************
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
      *
      **** BEGIN CHG **** 04/29/98 ************************************
     C************************************
     C*          F10=SEARCH              *
     C************************************
      *
     C     *INKJ         IFEQ      '1'
      *
     C************************************
     C*     SUBCRIBER SEARCH             *
     C************************************
     C     HFLD          IFEQ      'SCSUB#'
     C                   CALL      'F0055R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    QQRRNS            9 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         CABEQ     *ON           STRT                     20
      *
     C     QQRRNS        CHAIN     PSM02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      SMSUB#        SCSUB#
     C                   MOVE      *ON           *IN23
     C                   ENDIF                                                  IFEQ *IN60
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD SCSUB#
     C************************************
     C******************   F6                             *
     C************************************
     C********** *INKF     IFEQ '1'
     C************************************
     C**********   SCR01 = SUSCRIBER SEARCH       *
     C************************************
     C******************** CALL 'F0545R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           SCSUB#
     C******************** PARM           QQRTCD  1
     C********** QQRTCD    CABEQ'A'       TERM
     C******************** WRITESCR99
     C*
     C********** QQRTCD    IFEQ 'B'
     C********** SCSUB#    OREQ *ZERO
     C******************** MOVE '1'       *IN20
     C******************** GOTO STRT
     C******************** END                             END EQUAL B
     C*
     C********** QQRTCD    IFEQ '*'
     C******************** MOVE '1'       *IN23
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** GOTO STRT
     C******************** END                             END *
     C******************** END                             END *INKF
     C************************************
     C*         GROUP SEARCH             *
     C************************************
      *
     C     HFLD          IFEQ      'SCFND'
     C******** Beg Del ***** 12/18/24 ***************************
     C*****              MOVE      QQRRNS        QQRRNF
     C*****              CALL      'F0060R'
     C*****              PARM      '2'           QQCALL            1
     C******INKC         PARM                    QQKC              1
     C******INKL         PARM                    QQKL              1
     C*****              PARM                    QQRRNF            9 0
      *****
     C******INKC         CABEQ     *ON           TERM
      *****
     C******INKL         CABEQ     *ON           STRT                     23
      *****
     C*****QQRRNF        CHAIN     PFN02                              60
     C******IN60         IFEQ      *OFF
     C*****              MOVE      FNFND         SCFND
     C*****              MOVE      *ON           *IN35
     C*****              ELSE
     C*****              MOVE      *ON           *IN23
     C*****              ENDIF                                                  IFEQ *IN60
     C******** End Del ***** 12/18/24 ***************************
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD SCFND
     C***********************************
     C***********   F7                            *
     C***********************************
     C********** *INKG     IFEQ '1'
     C************************************
     C**********   SCR01 = GROUP SEARCH           *
     C************************************
     C*
     C********** SCSUB#    IFEQ *ZEROS
     C******************** MOVE UPSUB#    QQSUB   50
     C******************** ELSE
     C******************** MOVE SCSUB#    QQSUB
     C******************** END
     C******************** CALL 'F0508R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           QQSUB
     C******************** PARM           SCFND
     C******************** PARM           QQRTC   1
     C********** QQRTC     CABEQ'A'       TERM
     C*
     C******************** WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C*
     C********** QQRTC     IFEQ 'B'
     C********** SCFND     OREQ *ZERO
     C******************** MOVE '1'       *IN23
     C******************** GOTO STRT
     C******************** END                             END EQUAL B
     C*
     C********** QQRTC     IFEQ '*'
     C******************** MOVE QQSUB     SCSUB#
     C******************** MOVE '1'       *IN35
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** GOTO STRT
     C******************** END                             END *
     C*
     C******************** END                             END *INKG
      *
     C**********************************
     C*     INTERVIEWER SEARCH         *
     C**********************************
      *
     C     HFLD          IFEQ      'SCINTR'
     C                   CALL      'S0005R'
     C                   PARM      '2'           QQCALL            1
     C                   PARM      0             QQRRN             9 0
     C                   PARM      3             QQVIEW            1 0
      *
     C     QQRRN         CHAIN     PUP03                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      UPSTAF        SCINTR
     C                   MOVE      *ON           *IN36
     C                   ENDIF                                                  IFEQ *IN60
      *
     C     QQRRN         IFEQ      *ZEROS
     C                   MOVE      *ON           *IN35
     C                   ENDIF                                                  IFEQ *BLANK
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD SCINTR
      *
     C                   MOVE      *ON           *IN90
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  IFEQ INKJ
     C**********************************
     C**********   F8                           *
     C**********************************
     C********** *INKH     IFEQ '1'
     C**********************************
     C**********   SCR01 = INTERVIEWER SEARCH   *
     C**********************************
     C******************** MOVE *BLANKS   QQSTAF
     C******************** CALL 'F2165R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQSTAF  3
     C******************** PARM           QQRTCD  1
     C******************** MOVELQQSTAF    SCINTR
     C********** QQRTCD    CABEQ'A'       TERM
     C******************** WRITESCR99
     C*
     C********** QQRTCD    IFEQ 'B'
     C********** QQSTAF    OREQ *BLANKS
     C******************** MOVE *BLANKS   SCINTR
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** MOVE '1'       *IN35
     C******************** GOTO STRT
     C******************** END                             ENDEQ 'B'
     C*
     C********** QQRTCD    IFEQ '*'
     C******************** MOVE '1'       *IN36
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** GOTO STRT
     C******************** END                             ENDEQ '*'
     C*
     C******************** END                             END *INKH
     C****************************************
     C***********      F24 = MORE COMMAND KEYS         *
     C****************************************
     C********** *INKY     IFEQ '1'
     C******************** MOVELQ2RECF    HLRECF 10
     C********** CK        ADD  1         CK      50
     C*
     C********** HLRECF    IFEQ 'SCR01'
     C********** CK        IFGT 2
     C******************** Z-ADD1         CK
     C******************** END                             END CK GT 2
     C******************** MOVELCK1A,CK   SCCMD1
     C******************** END                             END SCR01
     C******************** WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVELHLRECF    Q2RECF
     C******************** GOTO STRT
     C******************** END                             END *INKY
     C**************************************
     C***********   PROCESS CHANGE OF USER PARAMETERS*
     C**************************************
     C********** *INKM     IFEQ '1'
     C*
     C******************** CALL 'F0006R'
     C******************** WRITESCR99
     C*
     C**********  GET THE USER PARAMETER FILE
     C*
     C********** UPKEY     CHAINPUP01                60
     C********** *IN60     IFEQ '1'
     C******************** MOVE *ZERO     UPCO#
     C******************** MOVE *ZERO     UPSUB#
     C******************** ELSE
     C******************** MOVE UPSUB#    SCSUB#
     C******************** MOVE UPFND     SCFND
     C******************** END
     C******************** GOTO STRT01
     C******************** END
     C                   GOTO      STRT
      **** END   CHG **** 04/29/98 ************************************
     C*
     C                   END
     C***************************
     C*   PROCESS REQUEST SCREEN*
     C***************************
     C     SCR01T        TAG
     C*
     C*   VALIDATE ENTRYS KEYED IN CORRECT ORDER
     C*
     C*
     C*  VALIDATE THE SUBSCRIBER
     C*
     C     SCSUB#        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN21                          INVALID SUB#
     C                   GOTO      STRT
     C                   END                                                    END IF *IN60
     C*
     C*  VALIDATE THE GROUP
     C*
     C     SCFND         IFEQ      *ZEROS
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C     HFLD          IFNE      *ZEROS
     C                   MOVE      '1'           *IN25                          INVALID FUND
     C                   GOTO      STRT
     C                   ENDIF                                                  IFNE *ZEROS
     C                   END                                                    END IF *IN60
     C*
     C*   VALIDATE THE INTERVIEWER
     C*
     C     SCINTR        IFNE      *BLANKS
     C     UPKY1         CHAIN     PUP02                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END                                                    END IF *IN60
     C                   END                                                    END IF *IN27
     C*
     C* VALIDATE THE ADMIT DATE RANGE IF NOT EQUAL TO BLANKS
     C*
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C     SRERR         CABEQ     '1'           STRT                     29
     C*
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         CABEQ     '1'           STRT                     31
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C     XXTODT        IFLT      XXFRDT
     C                   MOVE      '1'           *IN28
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C*
     C* VALIDATE THE REPORT TYPE
     C*
     C     SCTYPE        IFNE      'D'
     C     SCTYPE        ANDNE     'S'
     C     SCTYPE        ANDNE     'B'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCSUB#        QQSUB#            5
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCINTR        QQINTR            3
     C                   MOVE      SCFRDT        QQFRDT            8
     C                   MOVE      SCTODT        QQTODT            8
     C                   MOVE      SCTYPE        QQTYPE            1
     C                   CALL      'U7051C'
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQINTR
     C                   PARM                    QQFRDT
     C                   PARM                    QQTODT
     C                   PARM                    QQTYPE
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      **** BEGIN DEL **** 04/29/98 ************************************
**********  CK1A - COMMAND KEYS for SCR01
**********     F6=Subscriber Search      F7=Group Search       F24=More keys
**********ewer Search         F13=Chg User Parms               F24=More keys
********* END    DEL **** 04/29/98 ************************************
