     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H**
     H/TITLE E7521R - EMPLOYER MASTER LISTING
     H**
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PROVIDES PRINT CABABILITIES FOR THE        **
     H**     EMPLOYER MASTER FILE                                    **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**                                                             **
     H**   CALLS TO:                                                 **
     H**     E7521C                                                  **
     H**   PROGRAMMER: ALAN M. MOSS                                  **
     H**   CREATED:    03/07/91     PROJECT: EXP MOD                 **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** MODIFICATIONS:                                              **
     H**                                                             **
     H** 09/05/01  C:136 PROJ: WC577                                 **
     H**           Emp_Exp_Mod Rewrite - Added a search routine for  **
     H**           group.                                            **
     H**                                                             **
     H** 08/12/98  C:90  PROJ: WC853                                 **
     H**           MODIFIED TO VERSION 3 STANDARDS                   **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H**      KC      Command 03 - EXIT                              **
     H**      KL      Command 12 - CANCEL                            **
     H**      10      HELP                                           **
     H**  SCR01:                                                     **
     H**                                                             **
     H**      20      NO FUND ENTERED  ECR0503                       **
     H**      21      FUND NOT FOUND   ECR0508                       **
     H**      60      CHAIN EB01,UP01,CO01                           **
     H**                                                             **
     H**                                                             **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    E7521F:  Screen Display                                  **
     H**    SMCOL (PCO01): CO. FILE                                  **
     H**    EMUPL (PUP01): USER FILE                                 **
     H**    EMEBL (PEM01): EMPLOYER MASTER FILE (FND,EMP#)           **
     H**    EMFNL (PFN01): FUND FILE (CO, FND)                       **
     H*****************************************************************
     H*
     F*
     F* Display Screen File
     FE7521F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F*
     F* Company Master File
     FSMCOL     IF   E           K DISK
     F*
     F* User Parameter Master File
      *** BEG CHG ***** 09/05/01 ***********************************************
     F**********************EMUPL   IF  E           K        DISK
     FSMUPL     IF   E           K DISK
      *** END CHG ***** 09/05/01 ***********************************************
      *
     F* Employer BATCH File
     FEMEML     IF   E           K DISK
      *
      *** BEG ADD ***** 09/05/01 ***********************************************
     FEMFNL     IF   E           K DISK
      *** END CHG ***** 09/05/01 ***********************************************

     D*
     D*
     D* Data Structure for System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* Data Structure for Screen Reference
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D* External Program Data Structures
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Define Work Fields
     C*
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
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
     C* Key List for EMPLOYER MASTER FILE BY FUND
     C*
     C     EMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C*
     C*
     C* Entry Parameters
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
     C* Initialize and Set-Up
     C*
     C                   TIME                    DSDTTM
     C******************   MOVE Q1USER    SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C* Retrieve the User Parameter Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C* Retrieve the Company Record
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C* Load Screen Record Format for the First Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C* Display All Screen Formats
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*
     C* PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C* COMMAND 03 = EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
      *** BEG ADD 09/05/01 *****************************************************
     C     *INKJ         IFEQ      *ON
     C     HFLD          ANDEQ     'SCFND'
     C                   CALL      'E0015R'
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@FND             3
      *
     C                   MOVEL(P)  @@FND         SCFND
     C                   Z-ADD     SCFND         XXFND
     C     EMKEY         CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNNAME        SCFNNM
     C   60              MOVEL(P)  *BLANKS       SCFNNM
     C                   GOTO      STRT
     C                   ENDIF
      *** END ADD 09/05/01 *****************************************************
     C*
     C* PROCESS HELP REQUESTS
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
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C* Goto the Appropriate Screen
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C* Process Requested Screen
     C*
     C     SCR01T        TAG
     C*
     C***  VALIDATE THE FUND
     C                   Z-ADD     SCFND         XXFND
     C     XXFND         CABLE     0             STRT                   2020    NO FUND
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         CABEQ     '1'           STRT                     21     BAD FND
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   CALL      'E7521C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
