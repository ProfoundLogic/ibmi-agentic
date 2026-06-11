     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: SC932R                                            *
     H*    DESCRIPTION: PROMPT FOR PURGE ROUTINE:                     *
     H*                     TO INITIALIZE TAPE                        *
     H*                       NOTE: INITIALIZE VALUES GOTTEN FROM     *
     H*                             SYSTEM CONTROL FILE: SDRPF IN SMNU*
     H*                             (VOL ID HARD-CODED W/ 'PDS')      *
     H*    CREATED BY:  WENDY DILLARD       DATE:  03/18/92           *
     H*               BASED ON FPRG1R IN FSSRC                        *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*  07/28/93 C:10  - Proj: ATD                                   *
     H*                   Changed this to be called from the menu     *
     H*                   rather than from SC930C in the daily        *
     H*                   backup.                                     *
     H*                   Add call to SC931C to submit SC930C to      *
     H*                   batch.                                      *
     H*  07/17/00 C:128 - Proj:                                       *
     H*                   Changed to only validate Volume and Density *
     H*                   if the user requests to INZ the tape.       *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*  20     INVALID INITIALIZATION VALUE                          *
     H*  21     SYSTEM CONTROL FILE SDRPF IS IN ERROR                 *
     H*  22     INVALID DEVICE NAME ENTERED                           *
     H*  23     VALUE CANNOT BE EQUAL TO BLANKS.  (DEVICE)            *
     H*  24     VALUE CANNOT BE EQUAL TO BLANKS.  (VOLUME)            *
     H*  25     VALUE CANNOT BE EQUAL TO BLANKS.  (DENSITY)           *
     H*  60     GENERAL CHAIN                                         *
     H*****************************************************************
     FSC932F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FSSYLF     IF   E           K DISK

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
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQRTCD            1
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
     C                   MOVE      *ZEROS        QQCO#             3
     C                   MOVE      *BLANKS       QQDEV            10
     C                   MOVE      *BLANKS       QQVOL            10
     C                   MOVE      *BLANKS       QQTAPE           10
     C                   MOVE      *BLANKS       QQINZ             3
     C*
      /Free
       scPrDs = 'PURGE OF DARTS REMARKS';
      /End-Free
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C* LOAD INITIAL SCREEN VALUES
     C* Get the tape initialization values from SMNU/SDRPF
     C*
     C     'SC'          CHAIN     SDR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     DRTDNM        SCDEV
     C                   MOVEL     'PDS'         SCVOL
     C                   MOVEL     DRTDEN        SCDEN
     C*
     C* If there is no System Control File, send message
     C*
     C                   ELSE                                                   ELSE 0 *IN60
     C                   MOVEL     *BLANKS       SCDEV
     C                   MOVEL     'PDS'         SCVOL
     C                   MOVEL     *BLANKS       SCDEN
     C                   MOVE      '1'           *IN21
     C                   END                                                    END 0 *IN60
     C*
     C                   TIME                    DSDTTM
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
     C*  LOAD USER NAME
     C*
     C                   MOVE      Q1USER        SCUSER
     C*
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
     C* Process All Command Keys
     C     *IN11         IFEQ      '1'
     C************************************
     C* F3 = EXIT                        *
     C************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END *INKC
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
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *IN10
     C************************************
     C* CMD 13 - CHG USER PARMS          *
     C************************************
     C     *INKM         IFEQ      '1'
     C                   CALL      'F0006R'
     C                   WRITE     SCR99
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   GOTO      STRT01
     C                   END                                                    END *INKM
     C*
     C                   END                                                    END CMDKEYS
     C*********************************************************************
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C****************************
     C* LOAD TAPE & INITIALIZE   *
     C****************************
     C     SCR01T        TAG
     C*
     C* Validate initialization value.
     C*
     C     SCINZ         IFNE      'INZ'
     C     SCINZ         ANDNE     *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C                   MOVEL     SCINZ         QQINZ
     C                   MOVEL     SCDEV         QQDEV
     C                   MOVEL     SCVOL         QQVOL
     C                   MOVEL     SCDEN         QQTAPE
     C*
     C* If Device Name or Device Density is blank, send message
     C*
     C     QQDEV         IFEQ      *BLANKS
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END                                                    END EQ *BLKS
     C*
     C* If Device Name is incorrect, send message
     C*
     C                   MOVE      ' '           QQRTC             1
     C                   CALL      'F0001C'
     C                   PARM                    QQDEV
     C                   PARM                    QQRTC
     C     QQRTC         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END                                                    END 1 QQRTC
     C*
     C* IF VOLUME IS BLANK, SEND MESSAGE
     C*
     C     QQVOL         IFEQ      *BLANKS
      *** BEGIN CHG ***07/17/00 ************************************************
     C     SCINZ         ANDNE     *BLANKS
      *** END   CHG ***07/17/00 ************************************************
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END                                                    END EQ *BLKS
     C*
     C* IF DEVICE DENSITY IS BLANK, SEND MESSAGE
     C*
     C     QQTAPE        IFEQ      *BLANKS
      *** BEGIN CHG ***07/17/00 ************************************************
     C     SCINZ         ANDNE     *BLANKS
      *** END   CHG ***07/17/00 ************************************************
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END                                                    END EQ *BLKS
     C*
     C                   MOVE      UPCO#         QQCO#
     C*
     C*  Call CL to SUBMIT program to batch
     C*
     C                   CALL      'SC931C'
     C                   PARM                    QQCO#
     C                   PARM                    QQDEV
     C                   PARM                    QQVOL
     C                   PARM                    QQTAPE
     C                   PARM                    QQINZ
     C                   PARM                    QQRTCD
     C*
     C* End the Program
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
