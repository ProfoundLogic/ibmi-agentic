     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8114R - PROMPT FOR POLICY ADMIN QUOTE FILE PURGE
     H****************************************************************
     H*                                                              *
     H*  PROGRAM DESCRIPTION:                                        *
     H*       THIS PROGRAM ALLOWS THE USER TO ENTER THE FUND TO BE   *
     H*       PURGED IN THE POLICY ADMIN QUOTE FILE PURGE JOB.       *
     H*                                                              *
     H*  CALLED BY: MENU                                             *
     H*                                                              *
     H*  CALLS TO:  W8114C - PURGE SUBMITTER                         *
     H*             W0010R - FUND SEARCH                             *
     H*             W0060R - CHG USER PARMS                          *
     H*                                                              *
     H*  PROGRAMMER: C:77                                            *
     H*  CREATED:    05/21/98    PROJECT: WC494                      *
     H*  COPIED FROM WCMS/BASE/W8105R                                *
     H****************************************************************
     H* REVISIONS:                                                   *
     H*                                                               *
     H* 08/04/02 C:61  PROJECT WC577                                  *
     H*                V3 SCREEN STANDARDS.                           *
     H*                                                              *
     H*  07/22/98 C:61 PROJECT SUPPORT SEQ:12498                     *
     H*              - Use correct display file.                     *
     H*                                                              *
     H* PLEASE ADD TO TOP                                            *
     H****************************************************************
     H*                                                              *
     H* INDICATOR USAGE:                                             *
     H*  SCR01                                                       *
     H*  21 - INVALID FUND NUMBER                                    *
     H*                                                              *
     H****************************************************************
      ***** BEG CHG ******* 07/22/98 **********************************
     F*W8105F  CF  E                    WORKSTN
     FW8114F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      ***** END CHG ******* 07/22/98 **********************************
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK

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
      *
      *  KEY LIST FOR THE COMPANY FILE
      *

     C     COKEY         KLIST
     C                   KFLD                    UPCO#
      *
      *  USER PARAMETER FILE
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
      *  FUND MASTER FILE
      *
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND             3 0
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
      *
      *  GET THE COMPANY RECORD
      *
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      *
     C                   TIME                    DSDTTM
      ***** BEG DEL ******* 08/04/02 **********************************
     C*********            MOVE Q1USER    SCUSER
      ***** END DEL ******* 08/04/02 **********************************
     C                   MOVE      QQPRDS        SCPRDS
      *
      * Center Company name in display field
      *
     C                   MOVEL(P)  SCCONM        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCCONM
      *
      * Center Screen Prompt in display field
      *
     C                   MOVEL(P)  SCPRDS        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCPRDS
      *
      *   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
      *   DISPLAY ALL FORMATS
      *
     C     STRT          TAG
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
      *
      *   PROCESS ALL COMMAND KEYS
      *
     C     *IN11         IFEQ      '1'
      *
      *   F3 = END OF JOB
      *
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF
      *
      *   PROCESS HELP REQUESTS
      *
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
     C                   ENDIF
      ***** BEG DEL ******* 08/04/02 **********************************
      *********
      ****F5*=*FUND # SEARCH
      *********
     C*********  *INKE     IFEQ '1'
     C*********            CALL 'W0010R'
     C*********  *INKC     PARM           @@KC    1
     C*********  *INKL     PARM           @@KL    1
     C*********            PARM '2'       @@CALL  1
     C*********            PARM           @@CO#   3
     C*********            PARM           @@FND   3
     C*********            PARM           @@FYR   3
     C*********            PARM 1         @@VIEW  10
      *********
     C*********            MOVE @@FND     SCFND
      *********
     C*********  *INKC     CABEQ*ON       TERM
      *********
     C*********  *INKL     IFEQ *ON
     C*********  SCFND     OREQ *ZERO
     C*********            WRITESCR99
     C*********            MOVE *BLANK    Q2RECF
     C*********            MOVEL'SCR01'   Q2RECF
     C*********            MOVE '1'       *IN20
     C*********            GOTO STRT
     C*********            ELSE
      *********
     C*********            WRITESCR99
     C*********            MOVE *BLANK    Q2RECF
     C*********            MOVEL'SCR01'   Q2RECF
     C*********            GOTO STRT
     C*********            END
     C*********            END
      *********
      ****F8*=*FUND ALPHA SEARCH
      *********
     C*********  *INKH     IFEQ '1'
     C*********            CALL 'W0010R'
     C*********  *INKC     PARM           @@KC    1
     C*********  *INKL     PARM           @@KL    1
     C*********            PARM '2'       @@CALL  1
     C*********            PARM           @@CO#   3
     C*********            PARM           @@FND   3
     C*********            PARM           @@FYR   3
     C*********            PARM 2         @@VIEW  10
      *********
     C*********            MOVE @@FND     SCFND
      *********
     C*********  *INKC     CABEQ*ON       TERM
      *********
     C*********  *INKL     IFEQ *ON
     C*********  SCFND     OREQ *ZERO
     C*********            WRITESCR99
     C*********            MOVE *BLANK    Q2RECF
     C*********            MOVEL'SCR01'   Q2RECF
     C*********            MOVE '1'       *IN20
     C*********            GOTO STRT
     C*********            ELSE
      *********
     C*********            WRITESCR99
     C*********            MOVE *BLANK    Q2RECF
     C*********            MOVEL'SCR01'   Q2RECF
     C*********            GOTO STRT
     C*********            END
     C*********            END
      ***** END DEL ******* 08/04/02 **********************************
      ***** BEG ADD ******* 08/04/02 **********************************
      *
      *   F5 = FUND # SEARCH
      *
     C     HFLD          IFEQ      'SCFND'
     C                   MOVE      UPCO#         ##CO#
     C                   CALL      'W0010R'
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##CO#             3
     C                   PARM      *BLANKS       ##FND             3
     C                   PARM      *BLANKS       ##FYR             3
     C                   PARM      1             ##VIEW            1 0
      *
     C     ##FND         IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
      *
     C                   ELSE
      *
     C                   MOVE      ##FND         SCFND
     C                   END                                                    ##FND
      *
     C                   WRITE     SCR99
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    HFLD = SCFND
      *
      ***** END ADD ******* 08/04/02 **********************************
      *
      *   F13 = CHG USR PARM
      *
      *   PROCESS CHANGE OF USER PARAMETERS
      *
     C     *INKM         IFEQ      '1'
      *
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
      *
     C     SCFND         CABEQ     *ZERO         STRT01                   20
      *
     C                   GOTO      STRT01
     C                   ENDIF
      *
     C                   ENDIF
      *
      *   PROCESS REQUEST SCREEN
      *
     C     SCR01T        TAG
      *
      *   CALL THE CLP TO SUBMIT THE POLICY ADMIN PURGE.
      *
     C                   MOVE      UPCO#         X@CO#
     C                   MOVE      SCFND         X@FND
      *
     C                   CALL      'W8114C'
     C                   PARM                    X@CO#             3
     C                   PARM                    X@FND             3
     C                   PARM                    UPOUTQ
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
