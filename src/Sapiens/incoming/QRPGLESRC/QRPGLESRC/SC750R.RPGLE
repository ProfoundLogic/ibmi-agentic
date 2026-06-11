     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H/TITLE SC750R - DARTS Document Printout by File
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**       Prompt for DARTS Document Printout by File.           **
     H**       This program allows the user to enter a specific      **
     H**       key in order to print out all of the documents        **
     H**       associated with that key.  It calls SC750C which      **
     H**       submits the program that actually controls the        **
     H**       printing of the documents.  A report of all of the    **
     H**       document names is also generated during this          **
     H**       process.  The user also enters the OUTQ where         **
     H**       the report will be printed.                           **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**       MENU                                                  **
     H**   CALLS TO:                                                 **
     H**       SC750C                                               **
     H**   PROGRAMMER: W. Denehy                                     **
     H**   CREATED:    03/05/93     PROJECT: ANA                     **
     H****************************************************************
     H** REVISIONS:                                                  **
     H****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H**      20      SCM0623-You can only enter one key at a time.  **
     H**      21      SCM0623-You can only enter one key at a time.  **
     H**      22      SCM0624-You can only enter one key at a time.  **
     H**      23      SCM0503-Value cannot be equal to blanks.       **
     H*****************************************************************
     H** FILE USAGE:
     H** FILES:                                                      **
     H**    SMCOL (PCO01): CO                                        **
     H**    SMUPL (PUP01): USER                                      **
     H*****************************************************************
     FSC750F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  X1KEY                  1     40
     D  X1CO#                  1      3  0
     D  X1FND1                 4      6  0
     D  X1FYR                  7      9  0
     D  X1CASE                10     16  0
     D                 DS
     D  X2KEY                  1     40
     D  X2CO#                  1      3  0
     D  X2FND2                 4      6  0
     D  X2EMP#                 7     15  0
     D  X2DIV                 16     20  0
     D                 DS
     D  X3KEY                  1     40
     D  X3FEIN                 1      9  0
     D  X3MOD                 10     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   Company File
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*  User Parameter File
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
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
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      *BLANKS       X1KEY
     C                   MOVE      *BLANKS       X2KEY
     C                   MOVE      *BLANKS       X3KEY
     C*
     C*  Get the User Parameter File
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C                   Z-ADD     UPCO#         X1CO#
     C                   Z-ADD     UPCO#         X2CO#
     C*
     C*  Get the Company Record
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*   Load the Record Format Info for first time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   Display all Formats
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C**********************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C********************************************
     C*    F3 = EXIT                             *
     C********************************************
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C                   END
     C*
     C*****************************************************************
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  User can only enter values for 1 of the 3 keys:
     C*
     C*
     C*  If All of them are blank, error msg
     C*
     C     SCFND1        IFEQ      *ZERO
     C     SCFYR         ANDEQ     *ZERO
     C     SCCASE        ANDEQ     *ZERO
     C     SCFND2        ANDEQ     *ZERO
     C     SCEMP#        ANDEQ     *ZERO
     C     SCDIV         ANDEQ     *ZERO
     C     SCFEIN        ANDEQ     *ZERO
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  Validate the 1st key if it is entered
     C*
     C     SCFND1        IFNE      *ZERO
     C     SCFYR         ORNE      *ZERO
     C     SCCASE        ORNE      *ZERO
     C*
     C     SCFND2        IFNE      *ZERO
     C     SCEMP#        ORNE      *ZERO
     C     SCDIV         ORNE      *ZERO
     C     SCFEIN        ORNE      *ZERO
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*     Move FYR/CASE into User Data Field
     C*
     C                   MOVE      *BLANKS       QQUDTA
     C                   MOVEL     SCFYR         QQUDTA
     C                   MOVE      SCCASE        QQUDTA
     C*
     C*     Move Key Value into QQKEY
     C*
     C                   MOVE      *BLANKS       QQKEY
     C                   Z-ADD     SCFND1        X1FND1
     C                   Z-ADD     SCFYR         X1FYR
     C                   Z-ADD     SCCASE        X1CASE
     C                   MOVEL     X1KEY         QQKEY
     C*
     C                   END
     C*
     C*  Validate the 2nd key
     C*
     C     SCFND2        IFNE      *ZERO
     C     SCEMP#        ORNE      *ZERO
     C     SCDIV         ORNE      *ZERO
     C*
     C     SCFEIN        IFNE      *ZERO
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  Move EMP#/DIV into User Data
     C*
     C                   MOVE      *BLANKS       QQUDTA
     C                   MOVE      *BLANKS       X2UDTA           14
     C                   MOVEL     SCEMP#        X2UDTA
     C                   MOVE      SCDIV         X2UDTA
     C                   MOVEL     X2UDTA        QQUDTA
     C*
     C*  Move key value into QQKEY
     C*
     C                   MOVE      *BLANKS       QQKEY
     C                   Z-ADD     SCFND2        X2FND2
     C                   Z-ADD     SCEMP#        X2EMP#
     C                   Z-ADD     SCDIV         X2DIV
     C                   MOVEL     X2KEY         QQKEY
     C*
     C                   END
     C*****************************************************************
     C*
     C*  Validate the 3rd key
     C*
     C     SCFEIN        IFNE      *ZERO
     C*
     C*   Move FEIN into User Data field
     C*
     C                   MOVE      *BLANKS       QQUDTA
     C                   MOVEL     SCFEIN        QQUDTA
     C*
     C*   Move the Key Value into QQKEY
     C*
     C                   MOVE      *BLANKS       QQKEY
     C                   Z-ADD     SCFEIN        X3FEIN
     C                   Z-ADD     SCMOD         X3MOD
     C                   MOVEL     X3KEY         QQKEY
     C*
     C                   END
     C*
     C*  Validate the OUTQ
     C*
     C     SCOUTQ        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*   Call the CLP to submit the job for printing the documents.
     C*
     C                   CALL      'SC750C'
     C                   PARM                    QQKEY            40
     C                   PARM                    QQUDTA           16
     C                   PARM                    SCOUTQ           10
     C                   PARM      'D'           QQMODE            1
     C*
     C*  End the Program
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
