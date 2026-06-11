     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE SC015R1 - CLAIMS DIARY SUMMARY                           *
     H*****************************************************************
     H*  PROGRAM DESCRIPTION: CLAIMS DIARY SUMMARY DISPLAYS A LIST    *
     H*         OF ALL THE USER'S DIARY ITEMS UP TO THE ENTERED DATE  *
     H*         IN REMARK CODE ORDER.  A COUNT IS DISPLAYED OF THE    *
     H*         TOTAL NUMBER OF REMARKS THE USER HAS FOR EACH REMARK  *
     H*         CODE.  IF THE USER PLACES A '1' BESIDE A PARTICULAR   *
     H*         REMARK CODE, "WORK WITH DIARY ITEMS" (SC015R) IS      *
     H*         CALLED AND DISPLAYS ONLY THE REMARKS THAT HAVE THE    *
     H*         CHOSEN REMARK CODE.                                   *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*                                                               *
     H*  CALLS TO: SC015R (WORK WITH DIARY ITEMS)                     *
     H*                                                               *
     H*  PROGRAMMER: W. DILLARD                                       *
     H*  CREATED:    08/26/92             PROJECT: AMN                *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  11/08/04 C:136 SUPPORT: DR065 43386                          *
     H*                 Restrict user access if SMSLP/SLRACC = 'Y'    *
     H*                                                               *
     H*  06/05/03 C:128 PROJECT:DRMNT                                 *
     H*                 Added the access to Visual Darts back into   *
     H*                 program. - Client requests                    *
     H*                                                               *
     H*  11/07/02 C:128 PROJECT:WC099 #32243                          *
     H*                 After a User Search the subfile would not     *
     H*                 refresh automatically.                        *
     H*                                                               *
     H*  03/18/02 C:128 PROJECT:DR099 #31651                          *
     H*                 Changed program to remove option selected on  *
     H*                 a F12 return from a called program.           *
     H*                                                               *
     H*  10/05/01 C:02  REMOVED THE ACCESS TO VDARTS UNTIL MORE WORK  *
     H*                 IS DONE ON THE DIARY SUMMARY GROUPING OPTS.   *
     H*                                                               *
     H*  09/25/01 C:128 Bypass this screen if user security file is   *
     H*                 set for the user to default to Visual Darts.  *
     H*                 Set QQRTCD to 'G' to indicate to Visual Darts *
     H*                 to display in Group Summary mode.             *
     H*                                                               *
     H*                                                               *
     H*  02/15/00 C:33  SEQ#: DR048/20811                             *
     H*                 TURN OF *IN30 WHEN CHANGES HAVE BEEN MADE TO  *
     H*                 THE HEADER INFO HAS BEEN CHANGED.             *
     H*                                                               *
     H*  12/08/99 C:113 PROJECT: SUPPORT                              *
     H*                 Correct display of date.                      *
     H*                                                               *
     H*  11/19/99 C:113 PROJECT: DR079                                *
     H*                 Corrected security flag usage.                *
     H*                                                               *
     H*  08/16/99 C:33 PROJECT: WC922                                 *
     H*                ALLOW FOR A DYNAMIC HEADING.                   *
     H*                                                               *
     H*  04/16/99 C:02 SUPPORT: DR098 #17398                          *
     H*                CORRECTED DISPLAY WHEN USING 0 FOR DATE        *
     H*                                                               *
     H*  09/22/98 C:53 SUPPORT: 13677                                 *
     H*               CORRECT MISSING SIGNOFF PGM WITH QCMDEXC        *
     H*                                                               *
     H*  08/13/98 C:98 PROJECT: DR065                                 *
     H*               APPLY DIARY USER SECURITY IN WORK WITH DIARY    *
     H*               ITEMS                                           *
     H*                                                               *
     H* 10/28/92 C:10 - Proj: AMN                                     *
     H* 11/02/92 C:10 - Proj: AMN                                     *
     H*  01/11/93 C:10  - Proj: ASQ                                   *
     H*  02/24/93 C:15  PROJ: ATT                                     *
     H*                                                               *
     H*  ADD NEW REVISION REMARKS AT TOP OF REVISON SECTION           *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR01:                                                        *
     H*  11    Used to detect if a command key was pressed            *
     H*  18    Roll up                                                *
     H*  19    SCM0510 - Invalid value entered                        *
     H*  20    SCM0500 - Invalid date entered                         *
     H*  30    Used to allow End of file on roll message to display   *
     H*        at the correct time.                                   *
     H*  32    End of file on roll up or roll down.                   *
     H*  83    Set on if User has security level to change to another *
     H*                user's diary.                                  *
     H*****************************************************************
     FSC015F1   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN1)
     F*
     FSTCTL1    IF   E           K DISK
     FSDCMLG    IF   E           K DISK
     FSMSLL     IF   E           K DISK
     F*** BEGIN ADD *** 08/13/98 ***********************************************
     FSSYLF     IF   E           K DISK
     F*** END   ADD *** 08/13/98 ***********************************************

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D***  BEGIN ADD  ***  08/16/99  *********************************
     D OPTNM           C                   CONST('Diary Summary')
     D***  END   ADD  ***  08/16/99  *********************************
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  Key list for the Remark Code Maintenance file
     C*
     C     CTKY          KLIST
     C                   KFLD                    SCRMCD
     C*
     C*  Key list for the Comment Detail file
     C*
     C     CMKY          KLIST
     C                   KFLD                    SCUSER                         ADD 01/11/93
     C*
     C     CMKY1         KLIST
     C                   KFLD                    SCUSER                         ADD 01/11/93
     C                   KFLD                    SCRMCD
     C*
     C     CMKYG         KLIST
     C                   KFLD                    SCUSER                         ADD 01/11/93
     C                   KFLD                    SCDRCD
     C*
     C*   KEY LIST FOR THE DARTS SECURITY FILE
     C*
     C     SLKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     SLKY          KLIST
     C                   KFLD                    KKUSER
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQRTCD            1
     C***  BEGIN ADD  ***  08/16/99  *********************************
     C                   PARM                    QQPRDS           40
     C***  END   ADD  ***  08/16/99  *********************************
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
     C                   TIME                    DSDTTM
     C*
     C***  BEGIN ADD  ***  08/16/99  *********************************
     C     Q1PRMS        IFGE      2
     C     QQPRDS        ANDNE     *BLANKS
     C                   MOVEL(P)  QQPRDS        ##DATA
     C                   ELSE
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   ENDIF                                                  Q1PRMS>=2
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C***  END   ADD  ***  08/16/99  *********************************
     C*
     C     *LIKE         DEFINE    RRN1          SRRN1                          SAVE RRN1
     C     *LIKE         DEFINE    CMCDAT        XXCDAT                         CDAT VARIABL
     C     *LIKE         DEFINE    CMRMCD        XHDRCD                         HOLD RMCD
     C     *LIKE         DEFINE    SCCDAT        XHCDAT                         CDAT VARIABL
     C     *LIKE         DEFINE    CMRMCD        QQRMCD                         PARM RMCD
     C     *LIKE         DEFINE    CMRMCD        XXRMCD                         TEMP RMCD
     C     *LIKE         DEFINE    Q1USER        XHUSER                         CDAT VARIABL
     C     *LIKE         DEFINE    SLUSRP        KKUSER                         CDAT VARIABL
     C     *LIKE         DEFINE    SLUSRP        ##USER                         CDAT VARIABL
     C     *LIKE         DEFINE    SLSECL        XXSECL                         CDAT VARIABL
     C**** BEGIN ADD **** 08/13/98 ************************************
     C     *LIKE         DEFINE    SLSECL        YYSECL
     C*
     C* GET MINIMUM SECURITY LEVEL TO BE ABLE TO CHANGE TO ANOTHER
     C*     USER'S DIARY.
     C*
     C     'SC'          CHAIN     SDR01                              99
     C                   MOVE      DRDT02        YYSECL
     C*
     C     SLKEY         CHAIN     PSL01                              99
     C                   MOVE      SLSECL        XXSECL
     C***  Begin Add  ***  09/25/01  *********************************
      *
     C     SLVDAD        IFEQ      'D'
      *** REMOVE NEXT LINE TEMPORARILY UNTIL FIXES IN VDARTS FOR GRPNG
     C*** 10/05/01 ***     EXSR VDARTS
      *** 10/05/01 *** WHEN WE FIX VDARTS, ALSO MAKE IT SO AFTER THE
      *** VDARTS SUBROUTINE ENDS, WE GO BACK TO MENU, NOT THIS PGM!!!
      *** Added the access to Visual Darts back in this program.
     C                   EXSR      VDARTS
     C                   GOTO      TERM
     C                   ENDIF
      *
     C***  End   Add  ***  09/25/01  *********************************
     C*
     C     XXSECL        IFGE      YYSECL
     C                   MOVE      *ON           *IN83
     C                   ENDIF                                                  ENDIF GE YYSECL
     C**** END   ADD **** 08/13/98 ************************************
     C*
     C*** BEG ADD ***** 11/08/04 **************************************
     C     SLRACC        IFEQ      'Y'
     C                   GOTO      TERM
     C                   ENDIF
     C*** END ADD ***** 11/08/04 **************************************
     C*
     C*  Initialize routine * * *
     C*
     C                   EXSR      INIT
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01C
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C*   THIS WAS ADDED TO KEEP SCREEN FROM "FLIPPING OVER" AT THE END
     C*      OF A "ROLL-UP" AFER RETURN FROM A SELECTION.
     C*
     C                   MOVE      XXRRN1        #@$1
     C*
     C*******************************************************************
     C*   Process all COMMAND KEYS
     C*******************************************************************
     C*
     C     *IN11         IFEQ      '1'
     C*
     C****************************************
     C*   F2 = SIGNOFF                       *
     C****************************************
     C     *INKB         IFEQ      '1'
     C*** BEG CHG *** 09/22/98 **********************************
     C**********           CALL 'SGNOFF'
     C                   Z-ADD     7             LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM      'SIGNOFF'     XXSGNF            7
     C                   PARM                    LENGTH           15 5
     C*** END CHG *** 09/22/98 **********************************
     C                   END                                                    END *INKB
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C****************************************
     C*   F5 = REFRESH                       *
     C****************************************
     C     *INKE         IFEQ      '1'
     C*
     C     SLKEY         CHAIN     PSL01                              99
     C*
     C                   MOVEL     SLSECL        XXSECL
     C*
     C                   MOVEL     SCUSER        KKUSER
     C*
     C     SLKY          CHAIN     PSL01                              99
     C*
     C**** BEGIN ADD **** 08/13/98 ************************************
     C*
     C     XXSECL        IFGE      YYSECL
     C                   MOVE      *ON           *IN83
     C                   ENDIF                                                  ENDIF GE YYSECL
     C*
     C**** END   ADD **** 08/13/98 ************************************
     C*
     C     XXSECL        IFGE      SLSECL
     C     *IN99         ANDEQ     *OFF
     C*
     C*   CLEAR SUBFILE
     C*
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN1
     C*
     C     CMKYG         SETLL     PCM01                              60
     C                   EXSR      LOAD
     C*
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   ELSE
     C*
     C                   GOTO      SCR01T
     C*
     C                   ENDIF
     C*
     C                   END                                                    END *INKE
     C*
     C****************************************
     C*  F7 = USER PROFILE SEARCH           *
     C****************************************
     C     *INKG         IFEQ      '1'
     C*
     C     SLKEY         CHAIN     PSL01                              60
     C     *IN60         IFEQ      '0'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C********** SLF07     IFEQ 'Y'
     C     SLF09         IFEQ      'Y'
      *** END   CHG *** 11/19/99 ***********************************************
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C*
     C                   CALL      'SC530R'
     C                   PARM      *BLANKS       ##USER
     C                   PARM                    QQRTCD
     C*
     C                   MOVE      SLSECL        XXSECL
     C*
     C                   MOVEL     ##USER        KKUSER
     C*
     C     SLKY          CHAIN     PSL01                              99
     C*
     C     XXSECL        IFGE      SLSECL
     C     *IN99         ANDEQ     *OFF
     C*
     C                   MOVEL     ##USER        SCUSER
     C*
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*** Begin Chg 11/07/02 ******************************************
     C****************     GOTO STRT
     C                   GOTO      SCR01T
     C*** End   Chg 11/07/02 ******************************************
     C                   END                                                    END SCR01
     C*
     C                   END                                                    END SLF09=Y
     C                   END                                                    END *IN60=0
     C*
     C                   GOTO      STRT
     C                   END                                                    END *INKG
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C*
     C     *IN30         IFEQ      '1'
     C                   MOVE      '1'           *IN32
     C                   MOVE      '0'           *IN30
     C                   END
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C     CMKY1         SETLL     PCM01                              60
     C                   EXSR      LOAD
     C*
     C                   END                                                    END 1 *IN18 ROLLUP
     C**************************************************************************
     C*
     C                   GOTO      STRT
     C                   END                                                    END CMDKEYS
     C**************************************************************************
     C*  GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR01C'      SCR01T
     C     Q2RECF        CABEQ     'SCR01S'      SCR01T
     C*
     C***********************************
     C     SCR01T        TAG
     C***********************************
     C*
     C*** MUST ENTER A VALID USER NAME
     C*
     C     SCUSER        IFEQ      *BLANK
     C                   MOVE      '1'           *IN57
     C                   GOTO      STRT
     C                   ELSE
     C*
     C                   MOVEL     SCUSER        KKUSER
     C*
     C     SLKY          CHAIN     PSL01                              58
     C     *IN58         CABEQ     '1'           STRT
     C                   END
     C*
     C                   MOVE      SLSECL        XXSECL
     C*
     C     SLKEY         CHAIN     PSL01                              99
     C*
     C**** BEGIN ADD **** 08/13/98 ************************************
     C*
     C     XXSECL        IFGE      YYSECL
     C                   MOVE      *ON           *IN83
     C                   ENDIF                                                  ENDIF GE YYSECL
     C*
     C**** END   ADD **** 08/13/98 ************************************
     C*
     C     XXSECL        IFGT      SLSECL
     C*
     C                   SETON                                        73
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C*  Validate date on screen
     C*
     C     SCCDAT        IFNE      0
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
      *** BEGIN ADD *** 12/08/99 ***********************************************
     C                   MOVEL     SRDTO         SCCDAT
      *** END   ADD *** 12/08/99 ***********************************************
     C     SRERR         CABEQ     '1'           STRT                     20
     C                   END
     C*
     C* Check to see if user changes the date or the remark type
     C* on the screen to reposition or redisplay the remarks
     C*
     C*
     C     SCCDAT        IFNE      XHCDAT
     C     SCDRCD        ORNE      XHDRCD
     C     SCUSER        ORNE      XHUSER
     C*
     C***  BEGIN ADD  ***  02/15/00  *********************************
     C                   MOVE      *OFF          *IN30
     C***  END   ADD  ***  02/15/00  *********************************
     C*
     C*  Clear subfile
     C*
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C*
     C*  Reset relative record # since remark code or date changed
     C*
     C                   Z-ADD     0             RRN1
     C*
     C     CMKYG         SETLL     PCM01                              60
     C                   EXSR      LOAD
     C*
     C                   Z-ADD     SCCDAT        XHCDAT
     C                   MOVEL     SCDRCD        XHDRCD
     C                   MOVEL     SCUSER        XHUSER
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      '0'           ERR               1
     C*
     C*  Save RRN1 so that rolling can properly function
     C*
     C                   MOVE      RRN1          SRRN1
     C                   MOVE      *BLANKS       XXRMCD
     C                   MOVEL     SCRMCD        XXRMCD
     C*
     C                   READC     SCR01S                                 50
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C*   Process for invalid option selected
     C*
     C     SCSEL         IFNE      ' '
     C     SCSEL         IFNE      '1'
     C                   MOVE      '1'           *IN19
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN1          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      NXT1
     C                   END
     C                   END
     C*****
     C*  Process for '1=Choose remark type'
     C*****
     C     SCSEL         IFEQ      '1'
     C*
      *** Begin Change 09/25/01 ***************************************
     C******               MOVE *BLANKS   QQRTCD  1
     C                   MOVE      'G'           QQRTCD            1
      *** End   Change 09/25/01 ***************************************
     C                   MOVE      *BLANKS       QQKEY            40
     C                   MOVEL     SCRMCD        QQRMCD
     C                   MOVE      *BLANKS       QQDATE
     C                   MOVE      SCCDAT        QQDATE            8
     C                   MOVE      *BLANKS       QQUSER           10
     C                   MOVEL     SCUSER        QQUSER
     C*
     C                   CALL      'SC015R'
     C                   PARM                    QQKEY
     C                   PARM                    QQRTCD
     C                   PARM                    QQRMCD
     C                   PARM                    QQDATE
     C                   PARM                    QQUSER                         ADD 01/11/93
     C*
     C                   MOVE      *BLANKS       SCSEL
     C*** Begin Add *** 03/18/02 ***********************************************
     C     QQRTCD        IFEQ      'B'
     C                   UPDATE    SCR01S
     C                   ENDIF
     C*** End   Add *** 03/18/02 ***********************************************
     C     QQRTCD        CABEQ     'A'           TERM
     C     QQRTCD        CABEQ     'B'           STRT
     C*
     C     QQUSER        IFNE      SCUSER
     C*
     C*   CLEAR SUBFILE
     C*
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN1
     C*
     C                   MOVEL     QQUSER        SCUSER
     C*
     C     CMKYG         SETLL     PCM01                              60
     C                   EXSR      LOAD
     C*
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      RRN1          XRRN2
     C                   UPDATE    SCR01S
     C                   END
     C*
     C                   END                                                    END SEL EQ '1'
     C*
     C*  Read next subfile record
     C*
     C     NXT1          TAG
     C                   READC     SCR01S                                 50
     C                   END
     C*
     C*  Restore RRN1 so that rolling can properly function
     C*
     C                   MOVE      SRRN1         RRN1
     C                   MOVE      *BLANKS       SCRMCD
     C                   MOVEL     XXRMCD        SCRMCD
     C*
     C     ERR           IFEQ      '1'
     C                   MOVE      XRRN2         #@$1
     C                   GOTO      STRT
     C                   END
     C*
     C*  Set at last record selected
     C*
     C                   Z-ADD     XRRN2         #@$1
     C*
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   GOTO      STRT
     C*
     C*  End of program
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
      *====================================================================*
      * * *   VDARTS SUBROUTINE                              V D A R T S ***
      *====================================================================*
      *** Begin Add 09/25/01 ******************************************
     C     VDARTS        BEGSR
      *
      *  Set initial parm date to system date
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCDAT
      *
     C                   MOVE      'G'           QQRTCD            1
     C                   MOVE      *BLANKS       QQKEY            40
     C                   MOVE      *BLANKS       QQRMCD
     C                   MOVE      *BLANKS       QQDATE
     C                   MOVE      SCCDAT        QQDATE            8
     C                   MOVE      Q1USER        QQUSER           10
      *
     C                   CALL      'SC015R'
     C                   PARM                    QQKEY
     C                   PARM                    QQRTCD
     C                   PARM                    QQRMCD
     C                   PARM                    QQDATE
     C                   PARM                    QQUSER
      *
     C                   MOVE      *ON           *INLR
     C                   RETURN
      *
     C                   ENDSR
      *** End   Add 09/25/01 ******************************************
      *====================================================================*
      * * *   INIT SUBROUTINE                                    I N I T ***
      *====================================================================*
     C     INIT          BEGSR
     C*
     C                   Z-ADD     0             RRN1              5 0
     C                   Z-ADD     0             XRRN2             5 0
     C                   Z-ADD     0             XXRRN1            5 0
     C                   MOVE      *BLANKS       SCSEL
     C*
     C                   MOVE      '0'           *IN30
     C*
     C*  Clear Subfile
     C*
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C*
     C                   MOVE      *BLANKS       SCUSER
     C                   MOVEL     Q1USER        SCUSER
     C                   MOVEL     SCUSER        XHUSER
     C*
     C*  Set initial display date to system date
     C*  If screen date = 0, then display everything
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCDAT
     C                   Z-ADD     SCCDAT        XHCDAT
     C*
     C*  Set initial Remark Code to blanks
     C*
     C                   MOVEL     *BLANKS       SCDRCD
     C                   MOVEL     *BLANKS       XHDRCD
     C*
     C     CMKYG         SETLL     PCM01                              60
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   ENDSR
      *====================================================================*
      * * *   LOAD SUBROUTINE                                    L O A D ***
      *====================================================================*
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C                   MOVE      '0'           *IN69
     C*
     C*  Initialize screen and work variables
     C*
     C                   Z-ADD     *ZERO         SCCNT
     C                   MOVE      *BLANKS       SCRMCD
     C                   MOVE      *BLANKS       SCDESC
     C                   Z-ADD     *ZEROS        XXCDAT
     C*
     C*  Convert entered screen date
     C*
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCDAT
     C     XXCDAT        IFEQ      0
     C**** CHANGE NXT LIN FROM 999999 TO 9999999 **** 4/16/99
     C                   Z-ADD     9999999       XXCDAT
     C                   END
     C*
     C*  Read remark detail file
     C*
     C     CMKY          READE     PCM01                                  60
     C*
     C*  Initialize RMCD variable
     C*
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CMRMCD        SCRMCD
     C                   END
     C*
     C*  Do until entire screen is loaded
     C*
     C                   Z-ADD     1             X                 5 0
     C*
     C     X             DOWLE     13
     C     *IN60         IFEQ      '0'
     C*
     C     CMCDAT        IFLE      XXCDAT
     C*
     C     CMRMCD        IFEQ      SCRMCD
     C                   ADD       1             SCCNT
     C                   ELSE
     C*
     C     CTKY          CHAIN     PCT01                              50
     C*
     C     SCCNT         IFNE      0
     C*
     C                   MOVEL     *BLANKS       SCDESC
     C     *IN50         IFEQ      '0'
     C                   MOVEL     CTDESC        SCDESC
     C                   END
     C*
     C                   ADD       1             RRN1
     C*
     C*  Save 1st subfile RRN of the current page
     C*
     C     *IN69         IFEQ      '0'
     C                   Z-ADD     RRN1          XXRRN1
     C                   MOVE      '1'           *IN69
     C                   END
     C*
     C                   WRITE     SCR01S
     C                   ADD       1             X
     C*
     C                   END
     C*
     C                   MOVE      *BLANKS       SCRMCD
     C                   MOVEL     CMRMCD        SCRMCD
     C                   Z-ADD     1             SCCNT
     C*
     C                   END                                                    IFEQ SCRMCD
     C*
     C                   END                                                    IFLE XXCDAT
     C                   MOVE      '0'           *IN06
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *IN05
     C     X             IFEQ      1
     C                   Z-ADD     888           X
     C                   ELSE
     C                   Z-ADD     999           X
     C                   END
     C                   END                                                    *IN60 = 1
     C*
     C     CMKY          READE     PCM01                                  60
     C                   END                                                    DOWLE 13
     C*
     C     X             IFEQ      999
     C     X             OREQ      888
     C*
     C     *IN60         IFEQ      '1'
     C*
     C                   MOVEL     *BLANKS       SCDESC
     C     CTKY          CHAIN     PCT01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     CTDESC        SCDESC
     C                   END
     C*
     C     SCCNT         IFNE      0
     C                   ADD       1             RRN1
     C     RRN1          IFEQ      1
     C     X             OREQ      888
     C                   Z-ADD     RRN1          XXRRN1
     C                   END
     C                   WRITE     SCR01S
     C                   MOVE      '1'           *IN30
     C                   MOVE      '1'           *IN05
     C                   END
     C*
     C                   END
     C                   END
     C*
     C* Initialize subfile if no records to display
     C*
     C     RRN1          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANKS       SCRMCD
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCCNT
     C*
     C                   MOVE      ' '           SCSEL
     C                   Z-ADD     1             RRN1
     C                   Z-ADD     1             XXRRN1
     C                   MOVE      '1'           *IN06
     C                   MOVE      '1'           *IN32
     C                   WRITE     SCR01S
     C                   END
     C*
     C*  To position screen and cursor
     C*
     C                   Z-ADD     XXRRN1        #@$1
     C                   Z-ADD     XXRRN1        XRRN2
     C*
     C                   ENDSR
