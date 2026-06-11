     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    THIS PROGRAM IS THE EDIT PROGRAM FOR THE DARTS SINGLE    *
     H*    REMARK COPY/TRANSFER OPTION FROM VDARTS                    *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    VISUAL DARTS                                               *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    SC916C - SUBMITTER FOR DARTS SINGLE REMARK COPY/TRANSFER   *
     H*                                                               *
     H*  PROGRAMMER: 02                                               *
     H*                 11/29/99
     H*****************************************************************
     H* REVISIONS                                                     *
      *                                                               *
      * MODIFIED   : 10/25/06                                         *
      * Programmer : C:002                                            *
      * Project    : DR214                                            *
      *
      * ****   IDARTS ADDED AS POSSIBLE CALL.
      *                                                               *
     H*****************************************************************
     H*****************************************************************
     H* FILE SPECIFICATIONS                                           *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR01:                                                        *
     H*  07  INDICATE A COMMAND KEY WAS PRESSED                       *
     H*  20  SCTYPE  INVALID TYPE SELECTED(COPY/TRANSFER)             *
     H*  21  SCFRKY  USER SELECTED TRANSFER TO SAME KEY               *
     H*  21  SCTOKY  USER SELECTED TRANSFER TO SAME KEY               *
     H*  22  SCTORC  INVALID REMARK CODE ENTERED                      *
     H*  23  SCTORC  INACTIVE REMARK CODE ENTERED                     *
     H*  24  SCTODI  INVALID DIARY TYPE FLAG ENTERED(Y OR N)          *
     H*  25  SCTODU  IF 'TO' DIARY FLAG 'N' 'TO' DRY USR AND DT MUST  *
     H*              BE BLANKS AND ZEROS RESPECTIVELY                 *
     H*  25  QQTODD  IF 'TO' DIARY FLAG 'N' 'TO' DRY USR AND DT MUST  *
     H*              BE BLANKS AND ZEROS RESPECTIVELY                 *
     H*  26  SCTODU  IF 'TO' DIARY FLAG 'Y' 'TO' DRY USR AND DT MUST  *
     H*              NOT BE BLANKS AND ZEROS RESPECTIVELY             *
     H*  26  QQTODD  IF 'TO' DIARY FLAG 'Y' 'TO' DRY USR AND DT MUST  *
     H*              NOT BE BLANKS AND ZEROS RESPECTIVELY             *
     H*  27  SCTODU  INVALID DIARY USER ENTERED                       *
     H*  28  QQTODD  INVALID DIARY DATE ENTERED                       *
     H*  29  SCTOKY  CANNOT BE BLANKS                                 *
     H*  30  SCKTYP  INVALID KEY TYPE ENTERED                         *
     H*  60  GENERAL CHAINING                                         *
     H*****************************************************************
     F*
     F* FILE SPECIFICATIONS
     F*

     D*
     D*
     D* INPUT SPECIFICATIONS
     D*
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D PTITL           C                   CONST('  DARTS SINGLE REMAR-
     D                                     K COPY/TRANSFER     ')
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* *LIKE DEFINE SECTION
     C*
     C                   Z-ADD     0             CK1               5 0
     C                   MOVE      *ZEROS        QQFRS#            5
     C                   MOVE      *ZEROS        QQFRDD            8
     C                   MOVE      *ZEROS        QQTODD            8
     C                   MOVE      *BLANKS       QQPRDS           40
     C                   MOVE      *BLANKS       QQRTCD            1
     C                   MOVE      *BLANKS       QQMODE            1
     C                   MOVE      *BLANKS       XX2               2
     C                   MOVE      *ZEROS        SRDTE             6
     C                   MOVE      *BLANKS       SRERR             1
     C                   MOVE      *BLANKS       SRTERM            1
     C*
     C* PARM LIST FOR THE CALL TO SC916C - DARTS SINGLE REMARK COPY/
     C* TRANSFER SUBMITTER
     C*
     C     PL916C        PLIST
     C                   PARM                    SCTYPE
     C                   PARM                    SCFRKY
     C                   PARM                    QQFRS#
     C                   PARM                    SCFRRC
     C                   PARM                    SCFRDI
     C                   PARM                    SCFRDU
     C                   PARM                    QQFRDD
     C                   PARM                    SCTOKY
     C                   PARM                    SCTORC
     C                   PARM                    SCTODI
     C                   PARM                    SCTODU
     C                   PARM                    QQTODD
     C                   PARM                    SCKTYP
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C*
     C*
     **** ADD 10/25/06 ****************************************
     C*
     C     PL918C        PLIST
     C                   PARM                    SCTYPE
     C                   PARM                    SCFRKY
     C                   PARM                    QQFRS#
     C                   PARM                    SCFRRC
     C                   PARM                    SCFRDI
     C                   PARM                    SCFRDU
     C                   PARM                    QQFRDD
     C                   PARM                    SCTOKY
     C                   PARM                    SCTORC
     C                   PARM                    SCTODI
     C                   PARM                    SCTODU
     C                   PARM                    QQTODD
     C                   PARM                    SCKTYP
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C                   PARM                    QQINUS
     *** END ADD 10/25/06 *************************************
     C*
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C* ENTRY PARMS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    SCTYPE            1
     C                   PARM                    SCFRKY           40
     C                   PARM                    XXFRS#            5
     C                   PARM                    SCFRRC            5
     C                   PARM                    SCFRDI            1
     C                   PARM                    SCFRDU           10
     C                   PARM                    XXFRDD            8
     C                   PARM                    SCTOKY           40
     C                   PARM                    SCTORC            5
     C                   PARM                    SCTODI            1
     C                   PARM                    SCTODU           10
     C                   PARM                    XXTODD            8
     C                   PARM                    SCKTYP            2
     C                   PARM                    UPOUTQ           10
     C                   PARM                    QQRTCD            1
     **** ADD 10/25/06 ****************************************
     C                   PARM                    QQINUS           10                      16
     *** END ADD 10/25/06 *************************************
     C*
     C* EXECUTE THE INITALIZATION SUBROUTINE
     C*
     C                   EXSR      INIT
     C*
     C* DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C*
     C* EXECUTE THE SUBROUTINE TO EDIT THE SCREEN 1 FIELDS
     C*
     C                   EXSR      EDTS01
     C*
     C* WE HAVE PASSED ALL THE SCREEN EDITS, CALL THE DARTS SINGLE
     C* REMARK COPY/TRANSFER SUBMITTER PROGRAM
     C*
     **** ADD 10/25/06 ****************************************
     C     Q1PRMS        IFGT      15
     C                   CALL      'SC918C'      PL918C
     C                   ELSE
     *** END ADD 10/25/06 *************************************
     C                   CALL      'SC916C'      PL916C
     **** ADD 10/25/06 ****************************************
     C                   ENDIF
     *** END ADD 10/25/06 *************************************
     C*
     C* END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C*
     C/EJECT
     C*******************************************************************
     C* EDIT SCREEN 1 FIELDS SUBROUTINE                      E D T S 0 1*
     C*******************************************************************
     C     EDTS01        BEGSR
     C*
     C*
     C* THE "TO" KEY CANNOT BE BLANKS
     C*
     C     SCTOKY        IFEQ      *BLANKS
     C*
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C*
     C                   END                                                    END SCTOKY EQ *BLANK
     C*
     C*
     C*
     C* IF THE REMARK DATE IS ENTERED, IT MUST BE A VALID DATE
     C*
     C*
     C                   MOVE      QQTODD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         QQTODD
     C*
     C* IF WE RETURN FROM THE DATE VERIFICATION PROGRAM WITH ERROR
     C* DISPLAY ERROR MESSAGE
     C*
     C     SRERR         IFEQ      '1'
     C*
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C*
     C                   ENDIF                                                  END SRERR EQ '1'
     C*
     C                   MOVEL     *BLANKS       QQRTCD
     C*
     C                   ENDSR
     C/EJECT
     C*******************************************************************
     C* INITALIZATION SUBROUTINE                                 I N I T*
     C*******************************************************************
     C     INIT          BEGSR
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     1             CK1
     C                   MOVE      PTITL         QQPRDS
     C*
     C*
     C                   MOVE      XXFRDD        QQFRDD            8
     C                   MOVE      XXTODD        QQTODD            8
     C                   MOVE      XXFRS#        QQFRS#
     C*
     C                   ENDSR
