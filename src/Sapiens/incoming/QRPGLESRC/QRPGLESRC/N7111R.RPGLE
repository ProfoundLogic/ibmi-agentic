     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE N7111R - PROMPT FOR HBA ACTIVITY REPORT
     H*
     H****************************************************************
     H*  INDICATORS:                                                 *
     H* SCR01                                                        *
     H*                                                              *
     H*  25 - POSITION CURSOR AT SUBCRIBER                           *
     H*  26 - POSITION CURSOR AT FROM DATE                           *
     H*  27 - ERROR MESSAGE FOR INVALID FUNCTION KEY FOR SEARCH      *
     H****************************************************************
     H*
     H****************************************************************
     H*                 R E V I S I O N S                            *
     H*                                                              *
     H*  04/29/98 C:91  PROJ: MC181                                  *
     H*                 CHANGED F6 TO F10 FOR SEARCH                 *
     H*                 VERSION 4 STANDARDS                          *
     H*                 CHANGED CALLED FILE WITHIN SUBSCRIBER SEARCH *
     H*                 TO F0055R                                    *
     H*                                                              *
     H****************************************************************
     H*
     FN7111F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMSML     IF   E           K DISK
      **** BEGIN ADD **** 04/29/98 ************************************
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
      **** END   ADD **** 04/29/98 ************************************

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
     C                   KFLD                    XXSUB#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     *LIKE         DEFINE    UPDT          XXFRDT
     C     *LIKE         DEFINE    UPDT          XXTODT
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
     C                   MOVEL     UPSUB#        SCSUB
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
     C                   TIME                    DSDTTM
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
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*********************************
     C*   COMMAND 03 = EXIT           *
     C*********************************
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
      **** BEGIN CHG **** 04/29/98 ************************************
     C************************************
     C*   COMMAND 06 = SUBSCRIBER SEARCH *
     C************************************
     C**********
     C********** *INKF     IFEQ '1'
     C********** Q2RECF    IFEQ 'SCR01'
     C**********           CALL 'F0545R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           SCSUB   50
     C**********           PARM           QQRTC   1
     C********** QQRTC     CABEQ'A'       TERM
     C**********
     C********** QQRTC     IFEQ 'B'
     C********** SCSUB     OREQ *ZERO
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           END
     C**********
     C********** QQRTC     IFEQ '*'
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           END
     C**********
     C**********           END
     C**********           GOTO STRT
     C**********           END
     C*
      ****************
      * F10 = SEARCH *
      ****************
     C     *INKJ         IFEQ      *ON
     C*
      *********************
      * SUBSCRIBER SEARCH *
      *********************
     C*
     C     HFLD          IFEQ      'SCSUB'
     C*
     C                   CALL      'F0055R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRN             9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           STRT                     25     PC AT SUB
     C*
     C     ##RRN         CHAIN     PSM02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      SMSUB#        SCSUB
     C                   MOVE      *ON           *IN26                          PC AT SCFRDT
     C                   ELSE
     C                   MOVE      *ON           *IN25                          PC AT SCSUB
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCSUB
     C*
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  INKJ IFEQ 1
      **** END   CHG **** 04/29/98 ************************************
     C*
     C*   PROCESS HELP REQUESTS
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
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   END
     C                   GOTO      STRT01
     C                   END                                                    *IN11
     C*****************************************************************
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*** SUBSCRIBER CANNOT BE BLANKS
     C     SCSUB         CABEQ     *ZERO         STRT                     20     NEED A SUB
     C                   MOVE      SCSUB         XXSUB#            5 0
     C*
     C***
     C     SMKEY         CHAIN     PSM01                              60
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN21                           INVLD SUB
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*  VALIDATE THE ENTERED FROM DATE
     C*
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE ENTERED TO DATE
     C*
     C     SCTODT        IFEQ      *ZERO
     C*
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTODT
     C*
     C                   END
     C*
     C*  VALIDATE THE ENTERED TO DATE
     C*
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE ENTERED DATES FOR ASCENDING ORDER
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C     XXFRDT        CABGT     XXTODT        STRT                 24
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCSUB         QQSUB             5
     C                   MOVE      SCFRDT        QQFRDT            8
     C                   MOVE      SCTODT        QQTODT            8
     C                   CALL      'N7111C'
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB
     C                   PARM                    QQFRDT
     C                   PARM                    QQTODT
     C                   PARM                    UPOUTQ
     C                   PARM                    MODE              1
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
