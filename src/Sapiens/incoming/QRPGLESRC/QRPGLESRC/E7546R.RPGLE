     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H**
     H/TITLE E7546R - EMPLOYER PAYROLL LISTING REPORT
     H**
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PROVIDES PRINT CABABILITIES FOR THE        **
     H**     EMPLOYER PAYROLL FILE                                   **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**                                                             **
     H**   CALLS TO:                                                 **
     H**     E7546C                                                  **
     H**   PROGRAMMER: ALAN M. MOSS                                  **
     H**   CREATED:    03/11/91     PROJECT: EXP MOD                 **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** MODIFICATIONS:                                              **
     H**                                                             **
     H** 09/07/01  C:136 PROJ: WC577                                 **
     H**           Emp_Exp_Mod Rewrite - Added search routines       **
     H**           Deleted "Change User Parms"                       **
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
     H**      KJ      Command 10 - SEARCH                            **
     H**      KL      Command 12 - CANCEL                            **
     H**      10      HELP                                           **
     H**  SCR01:                                                     **
     H**                                                             **
     H**      20      NO FUND ENTERED  ECR0503                       **
     H**      21      FUND NOT FOUND   ECR0508                       **
     H**      22      EMP#  NOT FOUND ECR0505                        **
     H**      23      BATCH NO. NOT FOUND ECR0505                    **
     H**      60      CHAIN                                          **
     H**                                                             **
     H**                                                             **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    E7546F:  Screen Display                                  **
     H**    SMCOL (PCO01): CO. FILE                                  **
     H**    SMUPL (PUP01): USER FILE                                 **
     H**    EMFNL (PFN01)  FUND MASTER FILE
     H**    EMEPL (PEP01): EMPLOYER PAYROLL FILE (FND,EMP#,YEAR)     **
     H**    EMEPL1 (PEP02): EMPLOYER PAYROLL FILE (FND,YEAR)         **
     H*****************************************************************
     H*
     F*
     F* Display Screen File
     FE7546F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F*
     F* Company Master File
     FSMCOL     IF   E           K DISK
     F*
     F* User Parameter Master File
      *** BEG CHG *** 09/07/01 ***********************************************
     F********************EMUPL   IF  E           K        DISK
     FSMUPL     IF   E           K DISK
      *** END CHG *** 09/07/01 ***********************************************
     F*
     F* FUND MASTER  Master File
     FEMFNL     IF   E           K DISK
     F*
     F* Employer PAYROLL FILE
     FEMEPL     IF   E           K DISK
     F*
     FEMEPL1    IF   E           K DISK
     F                                     RENAME(PEP01:PEP02)
     F*

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
     D* external Program Data Structures
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Define Work Fields
     C*
     C     *LIKE         DEFINE    EPFND         XXFND
     C     *LIKE         DEFINE    EPEMP#        XXEMP#
     C     *LIKE         DEFINE    EPYEAR        XXYEAR
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
     C* Key List for FUND MASTER FILE BY FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C*
     C* Key List for EMPLOYER PAYROLL FILE BY FUND,EMP#
     C*
     C     EPKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C* Key List for EMPLOYER PAYROLL FILE BY FUND,YEAR
     C*
     C     EPKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXYEAR
     C*
     C* Key List for EMPLOYER PAYROLL FILE BY FUND,EMP#, YEAR
     C*
     C     EPKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXYEAR
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
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
     C     TOP           TAG
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
     C*
      *** BEG ADD *** 09/07/01 *************************************************
     C     *INKJ         IFEQ      '1'
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SCFND'
     C                   CALL      'E0015R'
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@FND             3
     C                   MOVE(P)   @@FND         SCFND
     C                   Z-ADD     SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL(P)  FNNAME        SCFNAM
     C                   SETON                                        30
     C                   GOTO      STRT
      *
     C     HFLD          WHENEQ    'SCEMP#'
     C                   CALL      'E0020R'
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM      SCFND         @1FND             3 0
     C                   PARM                    @@EMP#            9 0
     C                   Z-ADD     @1FND         SCFND
     C                   Z-ADD     SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL(P)  FNNAME        SCFNAM
     C                   Z-ADD     @@EMP#        SCEMP#
     C                   SETON                                        31
     C                   GOTO      STRT
     C                   ENDSL
     C                   ENDIF                                                  *INKJ IS ON
      *** END ADD *** 09/07/01 *************************************************
      *** BEG DEL *** 09/07/01 *************************************************
     C* COMMAND 13 - Change User Parameters
     C*
     C********** *INKM     IFEQ '1'
     C*
     C**********           CALL 'E0006R'
     C**********           WRITESCR99
     C*
     C* Retrieve User Parameter Record
     C*
     C********** UPKEY     CHAINPUP01                60
     C********** *IN60     IFEQ '1'
     C**********           MOVE *ZERO     UPCO#
     C**********           END
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO TOP
     C**********           END
     C*** END DEL *** 09/07/01 *************************************************
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
     C                   MOVE      *ZERO         XXFND
     C                   MOVE      *ZERO         XXEMP#
     C                   MOVE      *ZERO         XXYEAR
     C*
     C***  VALIDATE THE FUND
     C                   Z-ADD     SCFND         XXFND
     C     XXFND         CABLE     0             STRT                   2020     NO FND
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         CABEQ     '1'           STRT                     21     NO REC
     C*
     C*
     C***  VALIDATE THE EMP#
     C     SCEMP#        IFNE      0
     C                   Z-ADD     SCEMP#        XXEMP#
     C     EPKEY         CHAIN     PEP01                              60
     C     *IN60         CABEQ     '1'           STRT                     22     NO REC
     C*
     C                   END
     C*
     C*
     C***  VALIDATE THE BATCH YEAR
     C     SCEMP#        IFEQ      0
     C     SCYEAR        ANDNE     0
     C                   Z-ADD     SCYEAR        XXYEAR
     C     EPKEY1        CHAIN     PEP02                              60
     C     *IN60         CABEQ     '1'           STRT                     23     NO REC
     C*
     C                   END
     C*
     C***  VALIDATE THE EMP# AND YEAR
     C     SCEMP#        IFNE      0
     C     SCYEAR        ANDNE     0
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCYEAR        XXYEAR
     C     EPKEY2        CHAIN     PEP01                              60
     C     *IN60         CABEQ     '1'           STRT                     25     NO REC
     C*
     C                   END
     C*
      *** BEG DEL *** 09/07/01 **********************************************
     C*    EMP# AND  BATCH ARE BLANK
     C*
     C********** SCEMP#    IFEQ 0
     C********** SCYEAR    ANDEQ0
     C**********           MOVE 1         *IN24
     C**********           GOTO STRT
     C**********           END
      *** END DEL *** 09/07/01 **********************************************
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCEMP#        QQEMP#            9
     C                   MOVE      SCYEAR        QQYEAR            3
     C                   CALL      'E7546C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQEMP#
     C                   PARM                    QQYEAR
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
