     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W9600R - Prompt for EXCESSIVE LOSS REPORT
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM ALLOWS THE USER TO RUN A REPORT WHICH      **
     H**     WILL LIST MEMBERS WITH EXCESSIVE LOSSES BASED UPON      **
     H**     THE DATA ENTERED ON THE PROMPT.                         **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     MENU                                                    **
     H**   CALLS TO:                                                 **
     H**     W9600C                                                  **
     H*                                                               *
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                               *
     H*  03/04/20  C:079  Project: 102WIH                             *
     H*                   Add type of output                          *
     H*                                                               *
     H*  02/01/17  C:062  SUPPORT:44995(102)
     H*                   Expand the size of Loss Ratio
     H*                                                               *
     H*  05/18/09  C:220  SUPPORT:68503
     H*                   Occurance should be Occurrence
     H*                                                               *
     H************************************************************************
     FW9600F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F*
     FWMFNL     IF   E           K DISK
     F*
     FSMCOL     IF   E           K DISK
     F*
     FWMUPL     IF   E           K DISK
     F*

     D*
     D OPTNM           C                   CONST('Members with Excessi-
     D                                     ve Losses')
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
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND             3 0
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
     C* FIRST CYCLE PROCESSING
     C*
     C                   EXSR      SR1ST
     C*
     C     STRT01        TAG
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*** Beg Add ******** 03/04/20 ********************************************
     C*
     C*   DISPLAY LOCATION OF OUTPUT FILE
     C*
     C                   CALL      'SA030R'
     C                   PARM                    QQSUFX            3
     C     QQSUFX        IFEQ      *BLANKS
     C                   MOVEL     'PDSRPT'      XXRPTL           10
     C                   ELSE
     C     'PDSRPT'      CAT(P)    QQSUFX        XXRPTL
     C                   ENDIF
     C                   MOVE      XXRPTL        SCRPTL1
     C                   MOVE      XXRPTL        SCRPTL2
     C*
     C*** End Add ******** 03/04/20 ********************************************
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*** Beg Add *** 03/04/20 *********************************************
     C                   MOVE      '0'           errTypeOut
     C*** End Add *** 03/04/20 *********************************************
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F3=Exit
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
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
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C                   MOVE      UPFND         SCFND
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  GET THE FUND RECORD
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   END
     C                   GOTO      STRT01
     C*
     C                   END                                                    11=valid key
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  VALIDATE FROM DATE
     C*
     C                   Z-ADD     SCFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE TO DATE
     C*
     C                   Z-ADD     SCTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE DATE RANGE
     C*
     C                   Z-ADD     SCFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDT             7 0
     C*
     C                   Z-ADD     SCTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDT             7 0
     C*
     C     XXFDT         IFGT      XXTDT
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE LOSS DATE
     C*
     C                   Z-ADD     SCLSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCLSDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  LOSS RATIO CANNOT BE LESS THAN ZERO
     C*
     C     SCLSRA        IFLT      *ZERO
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*  YEAR OCCURRENCE HAS TO BE FROM 0 TO 3
     C*
     C     SCYEAR        IFLT      *ZERO
     C     SCYEAR        ORGT      3
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*  MONTH OCCURRENCE HAS TO BE FROM 0 TO 12.
     C*
     C     SCMNTH        IFLT      *ZERO
     C     SCMNTH        ORGT      12
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*** Beg Add *** 03/04/20 *********************************************
     C*
     C*  Type of Report must be P,F or B.
     C*
     C                   MOVE      '0'           errTypeOut
     C     SCTOO         IFNE      'P'
     C     SCTOO         ANDNE     'F'
     C     SCTOO         ANDNE     'B'
     C                   MOVE      '1'           errTypeOut
     C                   GOTO      STRT
     C                   END
     C*** End Add *** 03/04/20 *********************************************
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFDT         QQFDT             8
     C                   MOVE      SCTDT         QQTDT             8
     C                   MOVE      SCLSDT        QQLSDT            8
     C*** Begin Chg *** 02/01/17 ***************************************
     C**********         MOVE      SCLSRA        QQLSRA            5
     C                   MOVE      SCLSRA        QQLSRA            6
     C*** End   Chg *** 02/01/17 ***************************************
     C                   MOVE      SCYEAR        QQYEAR            1
     C                   MOVE      SCMNTH        QQMNTH            2
     C*** Beg Add *** 03/04/20 *********************************************
     C                   MOVE      SCTOO         QQTOO             1
     C*** End Add *** 03/04/20 *********************************************
     C*
     C                   CALL      'W9600C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFDT
     C                   PARM                    QQTDT
     C                   PARM                    QQLSDT
     C                   PARM                    QQLSRA
     C                   PARM                    QQYEAR
     C                   PARM                    QQMNTH
     C                   PARM                    UPOUTQ
     C*** Beg Add *** 03/04/20 *********************************************
     C                   PARM                    QQTOO
     C*** End Add *** 03/04/20 *********************************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C**
     C*==================================================================*
     C*   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
     C*==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
     C                   TIME                    DSDTTM
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C                   MOVE      UPFND         SCFND
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCLSDT
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  GET THE FUND RECORD
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C*
     C* * * INITIALIZE FIELDS FOR OUTPUT TO SCREEN
     C*
     C                   MOVE      UPCO#         XXCO#             3 0
     C*
     C                   ENDSR
     C*
