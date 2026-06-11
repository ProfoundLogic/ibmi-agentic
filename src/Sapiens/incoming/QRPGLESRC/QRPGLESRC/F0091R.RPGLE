     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H**
     H* F0091R - PHYSICIAN ENTRY SCREEN
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: F0091R                                            *
     H*    CREATED: 03/06/96  C:43,45 87MAW                           *
     H*    DESCRIPTION: LOAD THE PHYSICIAN DATA FOR F0070R            *
     H*                                                               *
     H*****************************************************************
     H**                                                             **
     H**    IMPORTANT NOTE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  **
     H**                                                             **
     H**    IF A FIELD IS ADDED OR DELETED FROM F0091F OR F0092F     **
     H**    THE FOLLOWING PROGRAMS MUST BE RECOMPILED:               **
     H**                                                             **
     H**    F0090R,  F0091R,   F0092R,  F0070R                       **
     H**                                                             **
     H*****************************************************************
     H*    REVISIONS
     H*
     H* 11/08/00 C:79 PROJECT:FS146
     H*                CHANGE 'BLTY' TO 'ZZBT' AND 'FLBT'.
     H*
     H* 10/12/98 C:53 MC145
     H*                     QUALIFY CALL TO F4200ROH
     H*                                                               *
     H*  07/02/98 C:94  PROJECT: MC145
     H*                 CORRECTED CURSOR POSITIONING
     H*
     H*  03/06/98 C:53  Project: MC145
     H*                 SWAP F0031R FOR F0032R (NEW PROVIDER)
     H*
     H*  11/07/97 C:53  Project: MC145 YEAR 2000/BASE FIT
     H*                 PRE-CODE FOR SELF INSURED OR STATE FUND
     H*                 HARD CODING FOR ACCORDIA
     H*
     H*  11/06/97 C:53  Project: MC145 YEAR 2000/BASE FIT
     H*
     H*  10/14/97 C:56  Project: MC087 #9058 support
     H*              1. Add condition that if charges already exist,
     H*                 protect Type of Bill and Diagnosis Code fields.
     H*                 Based on added PARM QQPRTC
     H*
     H*              2. Do not allow search or validation of Type of
     H*                 Bill and Diagnosis Codes when current charges
     H*                 exists.
     H*****************************************************************
     H* OLD INFO ON PROJECTS STARTS HERE
     H*****************************************************************
     H*
     H*  04/24/97 C:53  Project: 87MAW - MC087 #7351
     H*              1. RMV AUTO DEFAULT OF SERVICE PROV IF BLANK
     H*              2. CORRECT F3 NOT EXITING PROPERLY
     H*
     H*  04/24/97 C:56  Project: 87MAW
     H*              1. Add QQSYS to ENTRY PARM LIST
     H*              2. Add 'Type of Bill' field to SCR01.
     H*
     H*  05/06/97 C:56  Project: 87MAW
     H*              1. Replace ICD9 Searchs F0510R/F ---> F2010R/F
     H*
     H*  05/07/97 C:56  Project: 87MAW
     H*              1. Add edits to all fields that allow the user to
     H*                 key in a FEIN/Mod - must have BWC ID
     H*                 established in Provider Master File.
     H*
     H* 06/12/97 C:32  Project: 87MBY
     H*                REMOVE EDITS FOR ICD9 CODE AND ADD CALL
     H*                FOR PGM F4200ROH
     H*                                                               *
     H* 06/26/97 C:32 Req/Proj: FTMNT  MC051 #7823                    *
     H*               Add edit for carrier receive date.              *
     H*               Date must be less than the current date.        *
     H*
     H*  07/03/97 C:31  Project: 87MCD   **** RMVD ****
     H*                 Loading BWC Provider Types
     H*                 1. Add edit for servicing provider
     H*                 2. Change test for BWC status
     H*
     H*  07/22/97 C:56 Project: 87MAW - CO# 12 & 14
     H*                Add edit to 'Date Bill went to Carrier' field
     H*                Add edit to 'Date First Consulted': vs acc. date
     H*                Add edit to 'Date Bill went to Carrier': vs acc. date
     H*                Add edit to 'Date Bill recv'by Carrier': vs acc. date
     H*
     H*  07/31/97 C:56 Project: 87MAW - CO# 16
     H*                Pass new PARM for Servicing Provider Search
     H*
     H*  08/01/97 C:56 Project: 87MAW - CO# 4
     H*                Add Active/Inactive code edits.
     H*
     H*  08/07/97 C:32 Project: 87MBY - CO# 2
     H*                Remove edit for invalid ICD9 code.
     H*
     H*  08/22/97 C:32 Project: 87MCJ
     H*                Added previous EOMB# to screen.
     H*                Added 1 to SCLI91 variable (line position)
     H*                throughout program.
     H*****************************************************************
     H* UPDATE PROJECT INFO AT TOP
     H*****************************************************************
     FF0091F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      * DIAGNOSIS CODE FILE
     FFMEML     IF   E           K DISK
     FFMDIL     IF   E           K DISK
     FFTABL     IF   E           K DISK
      * PAYEE DEMOGRAPHIC MASTER FILE
     FFMPDL     IF   E           K DISK
      * GENERIC TABLE FILE
     FFTGTL     IF   E           K DISK
      *
      /COPY SCOPYPR01D
     D DC              S              5    DIM(8)                               DIAG CODES - SCRN
     D LT              C                   CONST('0123456789V')
     D*** BEGIN ADD *** 02/24/97 **************************************
     D LE              C                   CONST('0123456789')
     D*** END ADD *** 02/24/97 ****************************************
     D SCR91         E DS                  EXTNAME(F0091F)
     D  @IN10        E                     EXTFLD(*IN10)
     D  @IN12        E                     EXTFLD(*IN12)
     D  @IN20        E                     EXTFLD(*IN20)
      /IF DEFINED (WEB)
     D*PUIMOD*** BEGIN ADD
     D  PUIDUMMY01   E                     EXTFLD(SCAST)
     D  PUIDUMMY02   E                     EXTFLD(SCGZIP)
     D  PUIDUMMY03   E                     EXTFLD(SCSUB#)
     D  PUIDUMMY04   E                     EXTFLD(SCNAME)
     D  PUIDUMMY05   E                     EXTFLD(SCFND)
     D  PUIDUMMY06   E                     EXTFLD(SCFY)
     D  PUIDUMMY07   E                     EXTFLD(SCCASE)
     D  PUIDUMMY08   E                     EXTFLD(SCEMP#)
     D  PUIDUMMY09   E                     EXTFLD(SCDIV)
     D  PUIDUMMY10   E                     EXTFLD(SCSS#)
     D  PUIDUMMY11   E                     EXTFLD(SCMMI)
     D  PUIDUMMY12   E                     EXTFLD(SCCNME)
     D  PUIDUMMY13   E                     EXTFLD(SCSCL#)
     D  PUIDUMMY14   E                     EXTFLD(SCFEIN)
     D  PUIDUMMY15   E                     EXTFLD(SCMOD)
     D  PUIDUMMY16   E                     EXTFLD(SCPNME)
     D  PUIDUMMY17   E                     EXTFLD(SCZIP)
     D  PUIDUMMY18   E                     EXTFLD(SCPRDS)
     D*PUIMOD*** END ADD
      /ENDIF

     D*** BEGIN ADD *** 06/26/97 **************************************
     D                 DS
     D  DSDT                   1     12  0
     D  DSTM                   1      6  0
     D  DSMDY                  7     12  0

     I*** END   ADD *** 06/26/97 **************************************
      *
      ***PY SCOPYPR01I
      /COPY SCOPYPR01C
      *
      *   KEY LIST FOR THE DIAGNOSIS CODE FILE -- FMDIL
      *

     C     DIKY          KLIST
     C                   KFLD                    DIKEY             5
     C                   KFLD                    XXLOST            1
      *
     C     PDKEY         KLIST
     C                   KFLD                    KYFEIN
     C                   KFLD                    KYMOD
      *
      *   KEY LIST FOR GENERIC TABLE FILE -- FTGTL
      *
     C     GTKY          KLIST
     C                   KFLD                    KYCO#
     C                   KFLD                    KYTBID
     C                   KFLD                    KYKEY
     C*** BEGIN ADD *** 11/08/00 **************************************
     C*
     C     GTKY2         KLIST
     C                   KFLD                    KYCO#
     C                   KFLD                    KYTBID
     C*** END   ADD *** 11/08/00 **************************************
      *
     C     *ENTRY        PLIST
     C                   PARM                    SCAST
     C                   PARM                    SCGZIP
     C                   PARM                    XXCO#
     C                   PARM                    SCSUB#
     C                   PARM                    SCNAME
     C                   PARM                    SCFND
     C                   PARM                    SCFY
     C                   PARM                    SCCASE
     C                   PARM                    QQPREX            1
     C                   PARM                    QQPREC            1
     C                   PARM                    SCEMP#
     C                   PARM                    SCDIV
     C                   PARM                    SCSS#
     C     SCACDT        PARM                    XXACDT            8 0
     C                   PARM                    SCMMI
     C                   PARM                    SCCNME
     C                   PARM                    SCSCL#
     C                   PARM                    SCFEIN
     C                   PARM                    SCMOD
     C                   PARM                    SCPNME
     C                   PARM                    SCZIP
     C                   PARM                    QQWCCE            1
     C******************   PARM           Q1PST   2
     C                   PARM                    Q1CNTY           15
     C*** BEGIN ADD *** 04/24/97 **************************************
     C                   PARM                    QQSYS             1
     C*** END ADD *** 04/24/97 ****************************************
     C     SCR91         PARM                    SCR91
     C                   PARM                    SCPRDS
     C                   PARM                    QQRTCD            1
     C*** BEGIN ADD *** 10/14/97 **************************************
     C                   PARM                    QQPRTC            1
     C*
     C*** END ADD *** 10/14/97 ****************************************
      *
      * INITIALIZE WORK FIELDS
      *
     C     *LIKE         DEFINE    PDFEIN        KYFEIN
     C     *LIKE         DEFINE    PDMOD         KYMOD
     C     *LIKE         DEFINE    GTCO#         KYCO#
     C     *LIKE         DEFINE    GTTBID        KYTBID
     C     *LIKE         DEFINE    GTKEY         KYKEY
     C     *LIKE         DEFINE    GTTBID        QQID
     C     *LIKE         DEFINE    GTKEY         QQKEY
     C     *LIKE         DEFINE    GTCO#         XXCO#
     C*** BEGIN ADD *** 02/24/97 **************************************
     C     *LIKE         DEFINE    GTFG01        XTFG01
     C*** END ADD *** 02/24/97 ****************************************
      *
      /EJECT
      **************************************************************
      ** CALL THE API TO LOAD CO, SUB, GRP  DESCRIPTIONS         ***
      ** THIS IS A TO DO.                                        ***
      **************************************************************
     C     DSCAPI        BEGSR
     C                   ENDSR
      /EJECT
      **************************************************************
      **************************************************************
     C     OTHFKS        BEGSR
     C                   ENDSR
      /EJECT
      **************************************************************
      ** ALL SCREEN FILE FIELD EDITS WILL BE CONTROLLED HERE     ***
      ** IF ANY FAILED EDITS, THEN *IN90 MUST BE SET ON          ***
      **************************************************************
     C     ERRCK1        BEGSR
     C*** BEGIN ADD *** 04/24/97 **************************************
     C*** BEGIN DEL *** 10/14/97 **************************************
     C**********           MOVE *OFF      *IN30
     C*** END DEL *** 10/14/97 ****************************************
     C                   MOVE      *OFF          *IN32
     C                   MOVE      *OFF          *IN33
     C                   MOVE      *OFF          *IN34
     C                   MOVE      *OFF          *IN35
     C                   MOVE      *OFF          *IN36
     C                   MOVE      *OFF          *IN37
     C                   MOVE      *OFF          *IN38
     C                   MOVE      *OFF          *IN39
     C                   MOVE      *OFF          *IN40
     C                   MOVE      *OFF          *IN41
     C                   MOVE      *OFF          *IN42
     C                   MOVE      *OFF          *IN50
     C*** END ADD *** 04/24/97 ****************************************
     C*** BEGIN ADD *** 05/07/97 **************************************
     C                   MOVE      *OFF          *IN43
     C                   MOVE      *OFF          *IN44
     C*** END ADD *** 05/07/97 ****************************************
     C*** BEGIN ADD ** 07/03/97 ************************************************
     C                   MOVE      *OFF          *IN46
     C*** END ADD ** 07/03/97 **************************************************
     C*** BEGIN ADD ** 07/22/97 ************************************************
     C                   MOVE      *OFF          *IN47
     C                   MOVE      *OFF          *IN48
     C                   MOVE      *OFF          *IN49
     C                   MOVE      *OFF          *IN50
     C*** END ADD ** 07/22/97 **************************************************
     C                   MOVE      *OFF          *IN31
     C                   MOVE      *OFF          *IN42
     C*** END ADD ** 08/01/97 **************************************************
     C*** BEGIN ADD ** 08/01/97 ************************************************
     C                   MOVE      *OFF          *IN45                          ADD 06/26/97
     C     *IN12         IFEQ      *ON
     C                   EXSR      FUNKEY
     C                   ELSE
     C                   EXSR      EDITS
     C                   ENDIF
     C                   ENDSR
      /EJECT
      **************************************************************
      ** EDITS : VERIFY ALL SCREEN FIELDS                         **
      **************************************************************
     C     EDITS         BEGSR
      *
     C                   MOVE      *OFF          *IN70
     C                   MOVE      *OFF          *IN50
     C*** BEGIN ADD *** 10/14/97 **************************************
     C**********           MOVE *OFF      *IN30
     C*** END ADD *** 10/14/97 ****************************************
     C                   MOVE      *OFF          *IN32
     C                   MOVE      *OFF          *IN33
     C                   MOVE      *OFF          *IN34
     C                   MOVE      *OFF          *IN35
     C                   MOVE      *OFF          *IN36
     C                   MOVE      *OFF          *IN37
     C                   MOVE      *OFF          *IN38
     C                   MOVE      *OFF          *IN39
     C                   MOVE      *OFF          *IN40
     C                   MOVE      *OFF          *IN41
     C                   MOVE      *OFF          *IN42
      *
      ** VALIDATE PHYSICIAN TYPE
      *
     C                   MOVE      *OFF          *IN41
     C     SCPTYP        IFEQ      *BLANKS
     C****                 MOVE *ON       *IN41            DEL 11/06/97
     C****                 MOVE *ON       *IN90            DEL 11/06/97
     C****                 GOTO ENDEDT                     DEL 11/06/97
      *
     C                   ELSE
      *
     C                   MOVEL(P)  'PHTY'        KYTBID
     C                   MOVEL(P)  SCPTYP        KYKEY
     C     GTKY          CHAIN     PGT01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN41                          MCR0513
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 08/01/97 **************************************
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'I'
     C                   MOVE      *ON           *IN31
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** END ADD *** 08/01/97 ****************************************
     C*
     C*** BEGIN ADD *** 07/22/97 **************************************
     C*
     C     GTFG01        IFNE      QQSYS
     C                   SETON                                        9041
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** END ADD *** 07/22/97 ****************************************
      *
     C                   ENDIF
     C*** BEGIN ADD *** 04/24/97 **************************************
     C*
     C** VALIDATE TYPE OF BILL
     C*                    MOVE *BLANK    XTFG01
     C     QQSYS         IFEQ      'H'
     C*** BEGIN ADD *** 10/14/97 **************************************
     C     *IN69         ANDEQ     *OFF
     C*** END ADD *** 10/14/97 ****************************************
     C*
     C     SCBTYP        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN86
     C                   MOVE      *ON           *IN90
     C     *IN90         CABEQ     '1'           ENDEDT
     C                   ENDIF
     C*
     C*** BEGIN CHG *** 11/08/00 **************************************
     C**********           MOVE 'BLTY'    KYTBID
     C                   MOVEL(P)  BTTBID        KYTBID
     C*** END   CHG *** 11/08/00 **************************************
     C                   MOVEL(P)  SCBTYP        KYKEY
     C     GTKY          CHAIN     PGT01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN87                          MCR0513
     C                   MOVE      *ON           *IN90
     C     *IN90         CABEQ     '1'           ENDEDT
     C                   ELSE
     C*
     C*** BEGIN ADD *** 08/01/97 **************************************
     C     GTSTAT        IFEQ      'I'
     C                   MOVE      *ON           *IN42
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF                                                  GTSTAT IFEQ
     C*** END ADD *** 08/01/97 ****************************************
     C                   MOVEL     GTFG01        XTFG01
     C                   ENDIF
     C                   ENDIF                                                  QQSYS IFEQ H
     C*
     C*** END ADD *** 04/24/97 ****************************************
      *
      ** VALIDATE PATIENT CONTROL NUMBER (REQUIRED)
      *
     C*** BEG ADD *** 11/06/97 ****************************************
     C****       SCACCT    IFEQ *BLANKS
     C****                 MOVE *ON       *IN71            MCR0503
     C****                 MOVE *ON       *IN90
     C****                 GOTO ENDEDT
     C****                 ENDIF
     C*** END ADD *** 11/06/97 ****************************************
      *
      ** VALIDATE PRIME DIAGNOSTIC CODE (REQUIRED)
      *
     C*** BEGIN ADD *** 10/14/97 **************************************
     C*
     C** DO NOT VALIDATE DIAGNOSIS CODES IF CURRENT CHARGES EXIST
     C*
     C     *IN69         IFEQ      *OFF
     C*
     C*** END ADD *** 10/14/97 ****************************************
     C*
     C                   MOVE      *OFF          *IN72
     C                   MOVE      *OFF          *IN32
     C                   MOVE      *OFF          *IN73
      *
     C     SCDIA1        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN72
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ELSE
     C*** BEG ADD *** 11/06/97 ***********************
     C     SCDIA1        IFEQ      '0    '
     C                   GOTO      ENDPRI
     C                   ENDIF
     C*** END ADD *** 11/06/97 ***********************
     C                   MOVE      *BLANK        DIKEY             5
     C                   MOVE      SCDIA1        DIKEY
     C                   MOVE      'D'           XXLOST
     C     DIKY          CHAIN     PDI01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN72
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
      * ELSE FOUND, NOW TEST FOR STARTING CHARACTER
     C                   ELSE
      *
     C     XXFTYP        IFEQ      '1'                                          ADD 11/07/97
     C*** BEGIN DEL *** 04/24/97 **************************************
     C*** BEGIN ADD *** 11/07/97 **** REINSTATE ***********************
     C     1             SUBST     SCDIA1:1      XBASE             1            BASE STRING
     C     LT            CHECK     XBASE         X                 3 0    50
     C     *IN50         IFEQ      *ON
     C                   MOVE      *ON           *IN32
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF                                                  *IN50
     C*** END   ADD *** 11/07/97 **** REINSTATE ***********************
     C*** END DEL *** 04/24/97 ****************************************
     C*** BEGIN DEL *** 06/09/97 **************************************
     C*** BEGIN ADD *** 04/24/97 **************************************
     C********   QQSYS     IFEQ 'P'
     C********   QQSYS     OREQ 'H'
     C********   XTFG01    ANDEQ'O'
     C********   1         SUBSTSCDIA1:1  LETTER  1
     C********   LE        CHECKLETTER    X       30     50
     C********   *IN50     IFEQ *ON
     C********             SETON                     9032
     C********   *IN90     CABEQ'1'       ENDEDT
     C********             ENDIF
     C********             ENDIF
     C********
     C********   QQSYS     IFEQ 'H'
     C********   XTFG01    ANDEQ'I'
     C********   1         SUBSTSCDIA1:1  LETTER
     C********   LT        CHECKLETTER    X              50
     C********   *IN50     IFEQ *ON
     C********             SETON                     9032
     C********   *IN90     CABEQ'1'       ENDEDT
     C********             ENDIF
     C********             ENDIF
     C*** END ADD *** 04/24/97 ****************************************
     C*** END DEL *** 06/12/97 ****************************************
     C*** BEGIN ADD *** 06/12/97 **************************************
     C                   ELSE                                                   ADD 11/07/97
     C*** REPLACE DIAG EDITS WITH CALL TO PGM F4200ROH
     C                   MOVEL     SCDIA1        QQICD9
     C                   MOVE      'P'           QQPRSC
     C                   MOVE      'B'           Q2CALL
     C                   MOVE      XTFG01        QQSTYP
     C                   MOVE      *BLANKS       QQVLD
     C                   MOVE      *BLANKS       QQGRP
     C                   MOVE      *BLANKS       QQERR
     C                   MOVE      *BLANKS       QQSTS
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      SCSUB#        QQSUB
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCFY          QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   CALL      'F4200ROH'                           99
     C                   PARM                    QQICD9            5            IDC9 CODE
     C                   PARM                    QQPRSC            1            PRIMARY/SECOND
     C                   PARM                    Q2CALL            1            CALLED PGM CODE
     C                   PARM                    QQSYS             1            SYSTEM (P/H)
     C                   PARM                    QQSTYP            1            INPAT/OUTPAT
     C                   PARM                    QQVLD             1            VALID/INVALID
     C                   PARM                    QQGRP             1            GROUP COVERAGE
     C                   PARM                    QQERR             2            Ennn/Vnnn ICD9
     C                   PARM                    QQSTS             2            CLAIM STATUS
     C                   PARM                    QQCO#             3            CO #
     C                   PARM                    QQSUB             5            SUB #
     C                   PARM                    QQFND             3            FUND
     C                   PARM                    QQFYR             2            FUND YEAR
     C                   PARM                    QQCASE            7            CASE#
     C*
     C                   MOVE      *OFF          *IN50
     C                   MOVE      *OFF          *IN32
     C* CHECK ERROR FLAG FOR 'E' OR 'V' CODE
     C     QQERR         IFEQ      'EV'
     C     QQERR         OREQ      'E '
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN50
     C                   MOVE      *ON           *IN32
     C                   GOTO      ENDEDT
     C                   ENDIF
     C                   ENDIF                                                  ADD 11/07/97
     C*
     C*** BEGIN DEL *** 08/07/97 **************************************
     C* CHECK WHETHER ICD9 CODE HAS BEEN FLAGGED AS BWC INVALID
     C********
     C********   QQVLD     IFEQ 'I'
     C********             MOVE *ON       *IN90
     C********             MOVE *OFF      *IN73
     C********             MOVE *ON       *IN32
     C********             GOTO ENDEDT
     C********             ENDIF
     C********
     C*** END   DEL *** 08/07/97 **************************************
     C*** END   ADD *** 06/12/97 **************************************
     C                   ENDIF
      *
     C                   ENDIF
      *
     C** END PRIMARY EDIT TAG ON 0 DIAG CODE               ADD 11/06/97
     C     ENDPRI        TAG                                                    ADD 11/06/97
      *
      ** VALIDATE OTHER DIAGNOSTIC CODES (OPTIONAL)
      *
     C                   MOVE      SCDIA2        DC(1)
     C                   MOVE      SCDIA3        DC(2)
     C                   MOVE      SCDIA4        DC(3)
     C                   MOVE      SCDIA5        DC(4)
     C                   MOVE      SCDIA6        DC(5)
     C                   MOVE      SCDIA7        DC(6)
     C                   MOVE      SCDIA8        DC(7)
     C                   MOVE      SCDIA9        DC(8)
     C                   MOVEA     DC            XXDC             40
     C     XXDC          IFNE      *BLANKS
      *
     C                   MOVE      *OFF          *IN73
     C                   MOVEA     '00000000'    *IN(33)
     C                   Z-ADD     32            D1
      *
     C                   DO        8             D                 5 0
     C                   ADD       1             D1                2 0
     C     DC(D)         IFNE      *BLANK
     C                   MOVEL     *BLANK        DIKEY             5
     C                   MOVEL     DC(D)         DIKEY
     C                   MOVE      'D'           XXLOST                         ADD 05/18/95
     C     DIKY          CHAIN     PDI01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN73
     C                   MOVE      *ON           *IN(D1)
     C                   MOVE      *ON           *IN90
     C                   ELSE
     C*** BEGIN DEL *** 04/24/97 **************************************
     C********** 1         SUBSTDC,D:1    XBASE   1        BASE STRING
     C********** LT        CHECKXBASE     X       30     50
     C********** *IN50     IFEQ *ON
     C**********           MOVE *ON       *IN,D1
     C**********           MOVE *ON       *IN90
     C**********           GOTO ENDEDT
     C**********           ENDIF
     C*** END DEL *** 04/24/97 ****************************************
     C*
     C*** BEGIN DEL *** 06/12/97 **************************************
     C*** BEGIN ADD *** 04/24/97 **************************************
     C*********  QQSYS     IFEQ 'P'
     C*********  QQSYS     OREQ 'H'
     C*********  XTFG01    ANDEQ'O'
     C*********
     C*********  1         SUBSTDC,D:1    LETTER
     C*********  LE        CHECKLETTER    X              50
     C*********  *IN50     IFEQ *ON
     C*********            SETON                     90
     C*********            MOVE *ON       *IN,D1
     C*********  *IN90     CABEQ'1'       ENDEDT
     C*********            ENDIF
     C*********            ENDIF
     C*** END ADD *** 04/24/97 ****************************************
     C*** END DEL ***   06/12/97 **************************************
      *
     C*** BEGIN ADD *** 06/12/97 **************************************
     C*** REPLACE DIAG EDITS WITH CALL TO PGM F4200ROH
     C     XXFTYP        IFNE      '1'                                          ADD 10/12/98
     C                   MOVEL     DC(D)         QQICD9
     C                   MOVE      'S'           QQPRSC
     C                   MOVE      'B'           Q2CALL
     C                   MOVE      XTFG01        QQSTYP
     C                   MOVE      *BLANKS       QQVLD
     C                   MOVE      *BLANKS       QQGRP
     C                   MOVE      *BLANKS       QQERR
     C                   MOVE      *BLANKS       QQSTS
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      SCSUB#        QQSUB
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCFY          QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   CALL      'F4200ROH'                           99
     C                   PARM                    QQICD9            5            IDC9 CODE
     C                   PARM                    QQPRSC            1            PRIMARY/SECOND
     C                   PARM                    Q2CALL            1            CALLED PGM CODE
     C                   PARM                    QQSYS             1            SYSTEM (P/H)
     C                   PARM                    QQSTYP            1            INPAT/OUTPAT
     C                   PARM                    QQVLD             1            VALID/INVALID
     C                   PARM                    QQGRP             1            GROUP COVERAGE
     C                   PARM                    QQERR             2            Ennn/Vnnn ICD9
     C                   PARM                    QQSTS             2            CLAIM STATUS
     C                   PARM                    QQCO#             3            CO #
     C                   PARM                    QQSUB             5            SUB #
     C                   PARM                    QQFND             3            FUND
     C                   PARM                    QQFYR             2            FUND YEAR
     C                   PARM                    QQCASE            7            CASE#
     C*
     C                   MOVE      *OFF          *IN50
     C                   MOVE      *OFF          *IN(D1)
     C* CHECK ERROR FLAG FOR 'E' OR 'V' CODE
     C     QQERR         IFEQ      'EV'
     C     QQERR         OREQ      'E '
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN50
     C                   MOVE      *ON           *IN(D1)
     C                   GOTO      ENDEDT
     C                   ENDIF
     C                   ENDIF                                                  ADD 10/12/98
     C*
     C*** BEGIN DEL *** 08/07/97 **************************************
     C* CHECK WHETHER ICD9 CODE HAS BEEN FLAGGED AS BWC INVALID
     C********
     C********   QQVLD     IFEQ 'I'
     C********             MOVE *ON       *IN90
     C********             MOVE *OFF      *IN73
     C********             MOVE *ON       *IN,D1
     C********             GOTO ENDEDT
     C********             ENDIF
     C********
     C*** END   DEL *** 08/07/97 **************************************
     C*** END   ADD *** 06/12/97 **************************************
     C     DISTAT        IFNE      'A'
     C                   MOVE      *ON           *IN73                          MCR0742
     C                   MOVE      *ON           *IN(D1)
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDDO
      *
     C                   ENDIF
     C*** BEGIN ADD *** 10/14/97 **************************************
     C                   ENDIF                                                  *IN69 IFEQ *OFF
     C*** END ADD *** 10/14/97 ****************************************
      *
      ** VALIDATE ATTENDING PHYSICIAN ID FEIN AND MOD (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN74
      *
     C     SCTFEI        IFNE      *ZEROS
      *
     C                   MOVE      SCTFEI        KYFEIN
     C                   MOVE      SCTMOD        KYMOD
      *
     C     PDKEY         CHAIN     PPD01                              74
      *
     C     *IN74         IFEQ      *ON
     C                   MOVE      *ON           *IN74                          MCR0546
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
      *
     C                   ELSE
      *
     C     PDSTAT        IFEQ      'I'
     C                   MOVE      *ON           *IN74
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** BEGIN ADD *** 05/07/97 **************************************
     C     PDBWC#        IFEQ      *BLANKS
     C                   SETON                                        9088
     C     *IN90         CABEQ     '1'           ENDEDT
     C                   ENDIF
     C*** END ADD *** 05/07/97 ****************************************
     C                   ENDIF
      *
     C                   ENDIF
      *
      ** VALIDATE OTHER PHYSICIAN ID FEIN AND MOD (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN75
      *
     C     SCOFEI        IFNE      *ZEROS
      *
     C                   MOVE      SCOFEI        KYFEIN
     C                   MOVE      SCOMOD        KYMOD
      *
     C     PDKEY         CHAIN     PPD01                              75
      *
     C     *IN75         IFEQ      *ON
     C                   MOVE      *ON           *IN75                          MCR0546
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
      *
     C                   ELSE
      *
     C     PDSTAT        IFEQ      'I'
     C                   MOVE      *ON           *IN75
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** BEGIN ADD *** 05/07/97 **************************************
     C     PDBWC#        IFEQ      *BLANKS
     C                   SETON                                        9089
     C     *IN90         CABEQ     '1'           ENDEDT
     C                   ENDIF
     C*** END ADD *** 05/07/97 ****************************************
     C                   ENDIF
      *
     C                   ENDIF
      *
      ** VALIDATE SERVICE PROVIDER ID (REQUIRED)
      *
     C                   MOVE      *OFF          *IN76
     C                   MOVE      *OFF          *IN84
      *
     C*** BEG DEL *** 11/06/97 ***
     C****       SCSFEI    IFEQ *ZEROS
     C****                 MOVE SCFEIN    SCSFEI
     C****                 MOVE SCMOD     SCSMOD
     C****                 ENDIF
      ****
     C*** END DEL *** 11/06/97 ***
     C     SCSFEI        IFEQ      *ZEROS
      *
     C*** BEG DEL *** 11/06/97 ***
     C****                 MOVE *ON       *IN76            MCR0503
     C****                 MOVE *ON       *IN84
     C****                 MOVE *ON       *IN90
     C****                 GOTO ENDEDT
     C*** END DEL *** 11/06/97 ***
     C                   ELSE
      *
     C                   MOVE      SCSFEI        KYFEIN
     C                   MOVE      SCSMOD        KYMOD
      *
     C     PDKEY         CHAIN     PPD01                              76
      *
     C     *IN76         IFEQ      *ON
     C                   MOVE      *ON           *IN76                          MCR0546
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
      *
     C                   ELSE
      ****
     C     PDSTAT        IFEQ      'I'
     C                   MOVE      *ON           *IN76                          MCR0546
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** BEGIN ADD *** 05/07/97 **************************************
     C*** BEG DEL *** 11/06/97 ***
     C***        PDBWC#    IFEQ *BLANKS
     C***                  SETON                     9043  MCR2385
     C***        *IN90     CABEQ'1'       ENDEDT
     C***                  ENDIF
     C*** END DEL *** 11/06/97 ***
     C*** END ADD *** 05/07/97 ****************************************
     C*** BEGIN ADD ** 07/03/97 ************************************************
     C*** BEG DEL *** 11/06/97 ***
     C****
     C****       Test for servicing provider: must have record in
     C****       PRTY table and service flag must be set to 'Y'
     C****       ------------------------------------------------------
     C****                 MOVEL'PRTY'    KYTBID    P
     C****                 MOVELPDBWCT    KYKEY     P
     C****       GTKY      CHAINPGT01                60
     C****       *IN60     IFEQ *ON
     C****                 SETON                     4690  MCR2396
     C****                 GOTO ENDEDT
     C****                 ELSE                            *IN60 = On
     C****       GTFG01    IFNE 'Y'
     C****                 SETON                     4690  MCR2396
     C****                 GOTO ENDEDT
     C****                 ENDIF                           FG01 <> Y
     C****                 ENDIF                           *IN60 = On
     C*** END ADD ** 07/03/97 **************************************************
     C*** END DEL *** 11/06/97 ***********************************
     C                   ENDIF
      *
     C                   ENDIF
      *
      ** VALIDATE BILLING PROVIDER ID (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN77
      *
     C     SCBFEI        IFNE      *ZEROS
      *
     C                   MOVE      SCBFEI        KYFEIN
     C                   MOVE      SCBMOD        KYMOD
      *
     C     PDKEY         CHAIN     PPD01                              77
      *
     C     *IN77         IFEQ      *ON
     C                   MOVE      *ON           *IN77                          MCR0546
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
      *
     C                   ELSE
      *
     C     PDSTAT        IFEQ      'I'
     C                   MOVE      *ON           *IN77
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** BEGIN ADD *** 05/07/97 **************************************
     C     PDBWC#        IFEQ      *BLANKS
     C                   SETON                                        9044
     C     *IN90         CABEQ     '1'           ENDEDT
     C                   ENDIF
     C*** END ADD *** 05/07/97 ****************************************
     C                   ENDIF
      *
     C                   ENDIF
      *
      ** VALIDATE INJURED WORKER REIMBURSEMENT (REQUIRED)
      *
     C                   MOVE      *OFF          *IN78
      *
     C     SCIWRI        IFNE      *BLANK                                       ADD 11/06/97
      *
     C     SCIWRI        IFNE      'Y'                                          MCR0501
     C     SCIWRI        ANDNE     'N'
     C                   MOVE      *ON           *IN78
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C                   ENDIF                                                  ADD 11/06/97
      *
      ** VALIDATE PATIENT MEDICAL RELEASE (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN79
      *
     C     SCPMRI        IFGT      *BLANKS
      *
     C     SCPMRI        IFNE      'Y'
     C     SCPMRI        ANDNE     'N'
     C     SCPMRI        ANDNE     'M'
     C                   MOVE      *ON           *IN79                          MCR0301
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
      *
     C                   ENDIF
      *
      ** VALIDATE INJURY DUE TO EMPLOYMENT (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN80
      *
     C     SCEMPL        IFGT      *BLANKS
      *
     C     SCEMPL        IFNE      'Y'
     C     SCEMPL        ANDNE     'N'
     C                   MOVE      *ON           *IN80                          MCR0501
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
      *
     C                   ENDIF
      *
      ** VALIDATE DATE FIRST CONSULTED (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN81
      *
     C     SCDFC         IFGT      *ZEROS
      *
      *
     C                   Z-ADD     SCDFC         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDFC
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN81                          MCR0500
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 07/22/97 **************************************
     C*
     C*  VALIDATE AGAINST ACCIDENT DATE
     C*
     C                   Z-ADD     SCDFC         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZDFC             7 0
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZACDT            7 0
     C     ZZDFC         IFLT      ZZACDT
     C                   MOVE      *ON           *IN48
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF                                                  ZZDFC IFLT
     C*** END ADD *** 07/22/97 ****************************************
      *
     C                   ENDIF
      *
      ** VALIDATE BILL TO CARRIER DATE (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN82
      *
     C     SCDT09        IFGT      *ZEROS
      *
      *
     C                   Z-ADD     SCDT09        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDT09
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN82                          MCR0500
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 07/22/97 **************************************
     C*
     C*  VALIDATE AGAINST ACCIDENT DATE
     C*
     C                   Z-ADD     SCDT09        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZDT09            7 0
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZACDT
     C     ZZDT09        IFLT      ZZACDT
     C                   MOVE      *ON           *IN49
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF                                                  ZZDFC IFLT
     C*** END ADD *** 07/22/97 ****************************************
      *
     C                   ENDIF
      *
      ** VALIDATE BILL RECEIVED BY CARRIER (REQUIRED)
      *
     C                   MOVE      *OFF          *IN83
      *
     C                   MOVE      *OFF          *IN85
     C                   MOVE      *OFF          *IN45                          ADD 06/26/97
     C     SCCRDT        IFLE      *ZEROS
     C                   MOVE      *ON           *IN83                          MCR0502
     C                   MOVE      *ON           *IN85
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
      *
     C                   ELSE
      *
      *
     C                   Z-ADD     SCCRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCRDT
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN83                          MCR0500
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 07/22/97 **************************************
     C*
     C*  VALIDATE AGAINST ACCIDENT DATE
     C*
     C                   Z-ADD     SCCRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZCRDT            7 0
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZACDT
     C     ZZCRDT        IFLT      ZZACDT
     C                   MOVE      *ON           *IN50
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF                                                  ZZDFC IFLT
     C*** END ADD *** 07/22/97 ****************************************
      *
     C                   ENDIF
      *
     C*** BEGIN ADD *** 06/26/97 **********************************
     C*** EDIT IF CARRIER RECEIVED DATE IS GREATER THAN SYSTEM DATE
     C                   Z-ADD     SCCRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCRDT            7 0
     C     XXCRDT        IFGT      SYDATE
     C                   MOVE      *ON           *IN45
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   END
     C*** END   ADD *** 06/26/97 **********************************
     C*
     C*** BEGIN ADD *** 07/22/97 **************************************
     C*
     C** EDIT IF CARRIER RECEIVED DATE IS LESS THAN DATE WENT TO CARRIER
     C*
     C     SCDT09        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCCRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCRDT
     C                   Z-ADD     SCDT09        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT09            7 0
     C*
     C     XXCRDT        IFLT      XXDT09
     C                   SETON                                        9047
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD *** 07/22/97 ****************************************
      *
     C     ENDEDT        ENDSR
      *
      /EJECT
      **************************************************************
      ** ALL FUNCTION KEY PROCESSING WILL BE CODED HERE          ***
      ** INVOKED WITH *IN33, VLDCMDKEY                           ***
      **************************************************************
     C     FUNKEY        BEGSR
      *
     C     *INKJ         IFEQ      *ON
      *
     C******************   MOVE *ON       *IN70 ****** DEL 07/02/98
     C*** BEG ADD *** 07/02/98 ************************************
     C                   SETOFF                                       010203
     C                   SETOFF                                       04
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCPEOB'
     C                   SETON                                        0190
     C                   GOTO      TERM
     C     HFLD          WHENEQ    'SCMLI#'
     C                   SETON                                        0290
     C                   GOTO      TERM
     C     HFLD          WHENEQ    'SCACCT'
     C                   SETON                                        0390
     C                   GOTO      TERM
     C     HFLD          WHENEQ    'SCMRC#'
     C                   SETON                                        0490
     C                   GOTO      TERM
     C                   ENDSL
     C*** END ADD *** 07/02/98 ************************************
      *
      ** SEARCH FOR PHYSICIAN TYPE
      *
     C     HFLD          IFEQ      'SCPTYP'
      *
     C                   MOVE      'PHTY'        QQID
     C                   MOVE      *BLANKS       QQKEY
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQID
     C                   PARM                    QQKEY
     C                   PARM                    QQRTCD            1
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   GOTO      TERM
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      'B'
     C*** BEGIN CHG *** 04/24/97 **************************************
     C**********           Z-ADD11        SCLI91
     C**********           Z-ADD20        SCPS91
     C**********           Z-ADD11        SCLI91           CHG 08/22/97
     C**********           Z-ADD54        SCPS91
     C                   Z-ADD     14            SCLI91                         CHG 08/22/97
     C                   Z-ADD     55            SCPS91
     C*** END CHG *** 04/24/97 ****************************************
     C                   ELSE
     C**********           Z-ADD12        SCLI91
     C**********           Z-ADD21        SCPS91
     C     *IN30         IFEQ      '0'
     C     *IN69         ANDEQ     '0'
     C**********           Z-ADD11        SCLI91           CHG 08/22/97
     C**********           Z-ADD54        SCPS91
     C                   Z-ADD     15            SCLI91                         CHG 08/22/97
     C                   Z-ADD     22            SCPS91
     C*** BEGIN ADD *** 10/14/97 **************************************
     C                   ENDIF
     C     *IN30         IFEQ      '1'
     C     *IN69         OREQ      '1'
     C**********           Z-ADD13        SCLI91           CHG 08/22/97
     C**********           Z-ADD21        SCPS91
     C                   Z-ADD     15            SCLI91                         CHG 08/22/97
     C                   Z-ADD     22            SCPS91
     C                   ENDIF
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      '*'
     C     QQKEY         ANDEQ     *BLANKS
     C     QQRTCD        OREQ      ' '
     C*** BEGIN CHG *** 04/24/97 **************************************
     C**********           Z-ADD11        SCLI91
     C**********           Z-ADD20        SCPS91
     C**********           Z-ADD11        SCLI91           CHG 08/22/97
     C**********           Z-ADD54        SCPS91
     C                   Z-ADD     14            SCLI91                         CHG 08/22/97
     C                   Z-ADD     55            SCPS91
     C*** END ADD *** 10/14/97 ****************************************
     C*** END CHG *** 04/24/97 ****************************************
     C                   ENDIF
      *
     C                   MOVEL     QQKEY         SCPTYP
     C                   MOVE      *ON           *IN90
     C**********           MOVE *OFF      *IN70
     C*** BEG ADD *** 07/02/98 *************************************
     C*          SCPTYP    IFEQ *BLANKS
     C*                    SETON                     22
     C*                    ELSE
     C*                    SETON                     21
     C*                    ENDIF
     C*** END ADD *** 07/02/98 *************************************
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END PHY TYPE
      *
      ** SEARCH FOR DIAGNOSTIC CODE 1
      *
     C     HFLD          IFEQ      'SCDIA1'
     C*** BEGIN ADD *** 10/14/97 **************************************
     C*          *IN69     ANDEQ*OFF
     C*** END ADD *** 10/14/97 ****************************************
      *
     C*** BEGIN DEL *** 05/06/97 **************************************
     C**********           MOVE *BLANK    QQDIAG  5
     C**********           CALL 'F0510R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQDIAG
     C**********           PARM           QQRTCD  1
     C*** END DEL *** 05/06/97 ****************************************
     C*** BEGIN ADD *** 05/06/97 **************************************
     C                   MOVE      '2'           QQCALL
     C                   MOVE      *BLANKS       QQKC
     C                   MOVE      *BLANKS       QQKL
     C                   MOVE      *BLANKS       QQDIAG
     C                   MOVE      *BLANKS       QQLOST
     C*
     C                   CALL      'F2010R'
     C                   PARM                    QQCALL            1
     C                   PARM                    QQKC              1
     C                   PARM                    QQKL              1
     C                   PARM                    QQDIAG            5
     C                   PARM                    QQLOST            1
     C*** END ADD *** 05/06/97 ****************************************
      *
     C*
     C*** BEGIN CHG *** 05/06/97 **************************************
     C********** QQRTCD    IFEQ 'A'
     C     QQKC          IFEQ      '1'                                          ADD 05/06/97
     C                   MOVE      *ON           *INKC
     C                   GOTO      TERM
     C                   ENDIF
     C*** BEG DEL *** 07/02/98 ***************************************
      *
     C********** QQRTCD    IFEQ 'B'
     C     QQKL          IFEQ      '1'                                          ADD 05/06/97
     C**********           Z-ADD14        SCLI91           CHG 08/22/97
     C**********           Z-ADD20        SCPS91
     C                   Z-ADD     11            SCLI91                         CHG 07/02/98
     C                   Z-ADD     27            SCPS91
     C                   ENDIF
      *
     C********** QQRTCD    IFEQ '*'
     C********** QQDIAG    ANDEQ*BLANKS
     C     QQDIAG        IFEQ      *BLANKS                                      ADD 05/06/97
     C**********           Z-ADD14        SCLI91           CHG 08/22/97
     C**********           Z-ADD20        SCPS91
     C                   Z-ADD     11            SCLI91                         CHG 07/02/98
     C                   Z-ADD     27            SCPS91
     C                   ELSE
     C**********           Z-ADD15        SCLI91           CHG 08/22/97
     C**********           Z-ADD20        SCPS91
     C                   Z-ADD     12            SCLI91                         CHG 07/02/98
     C                   Z-ADD     27            SCPS91
     C                   ENDIF
     C*** END CHG *** 05/06/97 ****************************************
      *
     C*** END DEL *** 07/02/98 ****************************************
     C                   MOVEL     QQDIAG        SCDIA1
     C                   MOVE      *ON           *IN90
     C**********           MOVE *OFF      *IN70            DEL 07/02/98
     C*** BEG ADD *** 07/02/98 *************************************
     C***        SCDIA1    IFEQ *BLANKS
     C***                  SETON                     202122
     C***                  ELSE
     C***                  SETON                     2021
     C***                  ENDIF
     C*** END ADD *** 07/02/98 *************************************
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END DIAG 1
      *
      ** SEARCH FOR DIAGNOSTIC CODE 2
      *
     C     HFLD          IFEQ      'SCDIA2'
     C*** BEGIN ADD *** 10/14/97 **************************************
     C     *IN69         ANDEQ     *OFF
     C*** END ADD *** 10/14/97 ****************************************
      *
     C*** BEGIN DEL *** 05/06/97 **************************************
     C**********           MOVE *BLANK    QQDIAG
     C**********           CALL 'F0510R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQDIAG
     C**********           PARM           QQRTCD
     C*** END DEL *** 05/06/97 ****************************************
      *
     C*** BEGIN ADD *** 05/06/97 **************************************
     C                   MOVE      '2'           QQCALL
     C                   MOVE      *BLANKS       QQKC
     C                   MOVE      *BLANKS       QQKL
     C                   MOVE      *BLANKS       QQDIAG
     C                   MOVE      *BLANKS       QQLOST
     C*
     C                   CALL      'F2010R'
     C                   PARM                    QQCALL
     C                   PARM                    QQKC
     C                   PARM                    QQKL
     C                   PARM                    QQDIAG
     C                   PARM                    QQLOST
     C*** END ADD *** 05/06/97 ****************************************
     C*
     C*** BEGIN CHG *** 05/06/97 **************************************
     C********** QQRTCD    IFEQ 'A'
     C     QQKC          IFEQ      '1'                                          ADD 05/06/97
     C                   MOVE      *ON           *INKC
     C                   GOTO      TERM
     C                   ENDIF
      *
     C********** QQRTCD    IFEQ 'B'
     C     QQKL          IFEQ      '1'                                          ADD 05/06/97
     C                   Z-ADD     16            SCLI91                         CHG 08/22/97
     C                   Z-ADD     20            SCPS91
     C                   END
      *
     C********** QQRTCD    IFEQ '*'
     C********** QQDIAG    ANDEQ*BLANKS
     C     QQDIAG        IFEQ      *BLANKS                                      ADD 05/06/97
     C                   Z-ADD     16            SCLI91                         CHG 08/22/97
     C                   Z-ADD     20            SCPS91
     C                   ELSE
     C                   Z-ADD     16            SCLI91                         CHG 08/22/97
     C                   Z-ADD     26            SCPS91
     C                   ENDIF
     C*** END CHG *** 05/06/97 ****************************************
      *
     C                   MOVEL     QQDIAG        SCDIA2
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C*** BEG ADD *** 07/02/98 *************************************
     C***        SCDIA2    IFEQ *BLANKS
     C***                  SETON                     232425
     C***                  ELSE
     C***                  SETON                     2324
     C***                  ENDIF
     C*** END ADD *** 07/02/98 *************************************
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END DIAG 2
      *
      ** SEARCH FOR DIAGNOSTIC CODE 3
      *
     C     HFLD          IFEQ      'SCDIA3'
     C*** BEGIN ADD *** 10/14/97 **************************************
     C     *IN69         ANDEQ     *OFF
     C*** END ADD *** 10/14/97 ****************************************
      *
     C*** BEGIN DEL *** 05/06/97 **************************************
     C**********           MOVE *BLANK    QQDIAG
     C**********           CALL 'F0510R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQDIAG
     C**********           PARM           QQRTCD
     C*** END DEL *** 05/06/97 ****************************************
      *
     C*** BEGIN ADD *** 05/06/97 **************************************
     C                   MOVE      '2'           QQCALL
     C                   MOVE      *BLANKS       QQKC
     C                   MOVE      *BLANKS       QQKL
     C                   MOVE      *BLANKS       QQDIAG
     C                   MOVE      *BLANKS       QQLOST
     C*
     C                   CALL      'F2010R'
     C                   PARM                    QQCALL
     C                   PARM                    QQKC
     C                   PARM                    QQKL
     C                   PARM                    QQDIAG
     C                   PARM                    QQLOST
     C*** END ADD *** 05/06/97 ****************************************
     C*
     C*** BEGIN CHG *** 05/06/97 **************************************
     C********** QQRTCD    IFEQ 'A'
     C     QQKC          IFEQ      '1'                                          ADD 05/06/97
     C                   MOVE      *ON           *INKC
     C                   GOTO      TERM
     C                   ENDIF
      *
     C********** QQRTCD    IFEQ 'B'
     C     QQKL          IFEQ      '1'                                          ADD 05/06/97
     C                   Z-ADD     16            SCLI91                         CHG 08/22/97
     C                   Z-ADD     26            SCPS91
     C                   END
      *
     C********** QQRTCD    IFEQ '*'
     C********** QQDIAG    ANDEQ*BLANKS
     C     QQDIAG        IFEQ      *BLANKS                                      ADD 05/06/97
     C                   Z-ADD     16            SCLI91                         CHG 08/22/97
     C                   Z-ADD     26            SCPS91
     C                   ELSE
     C                   Z-ADD     16            SCLI91                         CHG 08/22/97
     C                   Z-ADD     32            SCPS91
     C                   ENDIF
     C*** END CHG *** 05/06/97 ****************************************
      *
     C                   MOVEL     QQDIAG        SCDIA3
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C*** BEG ADD *** 07/02/98 *************************************
     C***        SCDIA3    IFEQ *BLANKS
     C***                  SETON                     2324
     C***                  ELSE
     C***                  SETON                     2325
     C***                  ENDIF
     C*** END ADD *** 07/02/98 *************************************
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END DIAG 3
      *
      ** SEARCH FOR DIAGNOSTIC CODE 4
      *
     C     HFLD          IFEQ      'SCDIA4'
     C*** BEGIN ADD *** 10/14/97 **************************************
     C     *IN69         ANDEQ     *OFF
     C*** END ADD *** 10/14/97 ****************************************
      *
     C*** BEGIN DEL *** 05/06/97 **************************************
     C**********           MOVE *BLANK    QQDIAG
     C**********           CALL 'F0510R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQDIAG
     C**********           PARM           QQRTCD
     C*** END DEL *** 05/06/97 ****************************************
      *
     C*** BEGIN ADD *** 05/06/97 **************************************
     C                   MOVE      '2'           QQCALL
     C                   MOVE      *BLANKS       QQKC
     C                   MOVE      *BLANKS       QQKL
     C                   MOVE      *BLANKS       QQDIAG
     C                   MOVE      *BLANKS       QQLOST
     C*
     C                   CALL      'F2010R'
     C                   PARM                    QQCALL
     C                   PARM                    QQKC
     C                   PARM                    QQKL
     C                   PARM                    QQDIAG
     C                   PARM                    QQLOST
     C*** END ADD *** 05/06/97 ****************************************
     C*
     C*** BEGIN CHG *** 05/06/97 **************************************
     C********** QQRTCD    IFEQ 'A'
     C     QQKC          IFEQ      '1'                                          ADD 05/06/97
     C                   MOVE      *ON           *INKC
     C                   GOTO      TERM
     C                   ENDIF
      *
     C********** QQRTCD    IFEQ 'B'
     C     QQKL          IFEQ      '1'                                          ADD 05/06/97
     C                   Z-ADD     16            SCLI91                         CHG 08/22/97
     C                   Z-ADD     32            SCPS91
     C                   END
      *
     C********** QQRTCD    IFEQ '*'
     C********** QQDIAG    ANDEQ*BLANKS
     C     QQDIAG        IFEQ      *BLANKS
     C                   Z-ADD     16            SCLI91                         CHG 08/22/97
     C                   Z-ADD     32            SCPS91
     C                   ELSE
     C                   Z-ADD     16            SCLI91                         CHG 08/22/97
     C                   Z-ADD     38            SCPS91
     C                   ENDIF
     C*** END CHG *** 05/06/97 ****************************************
      *
     C                   MOVEL     QQDIAG        SCDIA4
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C*** BEG ADD *** 07/02/98 *************************************
     C***        SCDIA4    IFEQ *BLANKS
     C***                  SETON                     2325
     C***                  ELSE
     C***                  SETON                     23
     C***                  ENDIF
     C*** END ADD *** 07/02/98 *************************************
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END DIAG 4
      *
      ** SEARCH FOR DIAGNOSTIC CODE 5
      *
     C     HFLD          IFEQ      'SCDIA5'
     C*** BEGIN ADD *** 10/14/97 **************************************
     C     *IN69         ANDEQ     *OFF
     C*** END ADD *** 10/14/97 ****************************************
      *
     C*** BEGIN DEL *** 05/06/97 **************************************
     C**********           MOVE *BLANK    QQDIAG
     C**********           CALL 'F0510R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQDIAG
     C**********           PARM           QQRTCD
     C*** END DEL *** 05/06/97 ****************************************
      *
     C*** BEGIN ADD *** 05/06/97 **************************************
     C                   MOVE      '2'           QQCALL
     C                   MOVE      *BLANKS       QQKC
     C                   MOVE      *BLANKS       QQKL
     C                   MOVE      *BLANKS       QQDIAG
     C                   MOVE      *BLANKS       QQLOST
     C*
     C                   CALL      'F2010R'
     C                   PARM                    QQCALL
     C                   PARM                    QQKC
     C                   PARM                    QQKL
     C                   PARM                    QQDIAG
     C                   PARM                    QQLOST
     C*** END ADD *** 05/06/97 ****************************************
     C*
     C*** BEGIN CHG *** 05/06/97 **************************************
     C********** QQRTCD    IFEQ 'A'
     C     QQKC          IFEQ      '1'                                          ADD 05/06/97
     C                   MOVE      *ON           *INKC
     C                   GOTO      TERM
     C                   ENDIF
      *
     C********** QQRTCD    IFEQ 'B'
     C     QQKL          IFEQ      '1'                                          ADD 05/06/97
     C                   Z-ADD     16            SCLI91                         CHG 08/22/97
     C                   Z-ADD     38            SCPS91
     C                   END
      *
     C********** QQRTCD    IFEQ '*'
     C********** QQDIAG    ANDEQ*BLANKS
     C     QQDIAG        IFEQ      *BLANKS                                      ADD 05/06/97
     C                   Z-ADD     16            SCLI91                         CHG 08/22/97
     C                   Z-ADD     38            SCPS91
     C                   ELSE
     C                   Z-ADD     16            SCLI91                         CHG 08/22/97
     C                   Z-ADD     44            SCPS91
     C                   ENDIF
     C*** END CHG *** 05/06/97 ****************************************
      *
     C                   MOVEL     QQDIAG        SCDIA5
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C*** BEG ADD *** 07/02/98 *************************************
     C*          SCDIA5    IFEQ *BLANKS
     C*                    SETON                     23
     C*                    ELSE
     C*                    SETON                     2425
     C**                   ENDIF
     C*** END ADD *** 07/02/98 *************************************
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END DIAG 5
      *
      ** SEARCH FOR DIAGNOSTIC CODE 6
      *
     C     HFLD          IFEQ      'SCDIA6'
     C*** BEGIN ADD *** 10/14/97 **************************************
     C     *IN69         ANDEQ     *OFF
     C*** END ADD *** 10/14/97 ****************************************
      *
     C*** BEGIN DEL *** 05/06/97 **************************************
     C**********           MOVE *BLANK    QQDIAG
     C**********           CALL 'F0510R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQDIAG
     C**********           PARM           QQRTCD
     C*** END DEL *** 05/06/97 ****************************************
      *
     C*** BEGIN ADD *** 05/06/97 **************************************
     C                   MOVE      '2'           QQCALL
     C                   MOVE      *BLANKS       QQKC
     C                   MOVE      *BLANKS       QQKL
     C                   MOVE      *BLANKS       QQDIAG
     C                   MOVE      *BLANKS       QQLOST
     C*
     C                   CALL      'F2010R'
     C                   PARM                    QQCALL
     C                   PARM                    QQKC
     C                   PARM                    QQKL
     C                   PARM                    QQDIAG
     C                   PARM                    QQLOST
     C*** END ADD *** 05/06/97 ****************************************
     C*
     C*** BEGIN CHG *** 05/06/97 **************************************
     C********** QQRTCD    IFEQ 'A'
     C     QQKC          IFEQ      '1'                                          ADD 05/06/97
     C                   MOVE      *ON           *INKC
     C                   GOTO      TERM
     C                   ENDIF
      *
     C********** QQRTCD    IFEQ 'B'
     C     QQKL          IFEQ      '1'                                          ADD 05/06/97
     C                   Z-ADD     16            SCLI91                         CHG 08/22/97
     C                   Z-ADD     44            SCPS91
     C                   END
      *
     C********** QQRTCD    IFEQ '*'
     C********** QQDIAG    ANDEQ*BLANKS
     C     QQDIAG        IFEQ      *BLANKS                                      ADD 05/06/97
     C                   Z-ADD     16            SCLI91                         CHG 08/22/97
     C                   Z-ADD     44            SCPS91
     C                   ELSE
     C                   Z-ADD     16            SCLI91                         CHG 08/22/97
     C                   Z-ADD     50            SCPS91
     C                   ENDIF
     C*** END CHG *** 05/06/97 ****************************************
      *
     C                   MOVEL     QQDIAG        SCDIA6
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C*** BEG ADD *** 07/02/98 *************************************
     C*          SCDIA6    IFEQ *BLANKS
     C*                    SETON                     2425
     C*                    ELSE
     C*                    SETON                     25
     C*                    ENDIF
     C*** END ADD *** 07/02/98 *************************************
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END DIAG 6
      *
      ** SEARCH FOR DIAGNOSTIC CODE 7
      *
     C     HFLD          IFEQ      'SCDIA7'
     C*** BEGIN ADD *** 10/14/97 **************************************
     C     *IN69         ANDEQ     *OFF
     C*** END ADD *** 10/14/97 ****************************************
      *
     C*** BEGIN DEL *** 05/06/97 **************************************
     C**********           MOVE *BLANK    QQDIAG
     C**********           CALL 'F0510R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQDIAG
     C**********           PARM           QQRTCD
     C*** END DEL *** 05/06/97 ****************************************
      *
     C*** BEGIN ADD *** 05/06/97 **************************************
     C                   MOVE      '2'           QQCALL
     C                   MOVE      *BLANKS       QQKC
     C                   MOVE      *BLANKS       QQKL
     C                   MOVE      *BLANKS       QQDIAG
     C                   MOVE      *BLANKS       QQLOST
     C*
     C                   CALL      'F2010R'
     C                   PARM                    QQCALL
     C                   PARM                    QQKC
     C                   PARM                    QQKL
     C                   PARM                    QQDIAG
     C                   PARM                    QQLOST
     C*** END ADD *** 05/06/97 ****************************************
     C*
     C*** BEGIN CHG *** 05/06/97 **************************************
     C********** QQRTCD    IFEQ 'A'
     C     QQKC          IFEQ      '1'                                          ADD 05/06/97
     C                   MOVE      *ON           *INKC
     C                   GOTO      TERM
     C                   ENDIF
      *
     C********** QQRTCD    IFEQ 'B'
     C     QQKL          IFEQ      '1'                                          ADD 05/06/97
     C                   Z-ADD     16            SCLI91                         CHG 08/22/97
     C                   Z-ADD     50            SCPS91
     C                   END
      *
     C********** QQRTCD    IFEQ '*'
     C********** QQDIAG    ANDEQ*BLANKS
     C     QQDIAG        IFEQ      *BLANKS                                      ADD 05/06/97
     C                   Z-ADD     16            SCLI91                         CHG 08/22/97
     C                   Z-ADD     50            SCPS91
     C                   ELSE
     C                   Z-ADD     16            SCLI91                         CHG 08/22/97
     C                   Z-ADD     56            SCPS91
     C                   ENDIF
     C*** END CHG *** 05/06/97 ****************************************
      *
     C                   MOVEL     QQDIAG        SCDIA7
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C*** BEG ADD *** 07/02/98 *************************************
     C*          SCDIA7    IFEQ *BLANKS
     C*                    SETON                     25
     C*                    ELSE
     C*                    SETON                     24
     C*                    ENDIF
     C*** END ADD *** 07/02/98 *************************************
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END DIAG 7
      *
      ** SEARCH FOR DIAGNOSTIC CODE 8
      *
     C     HFLD          IFEQ      'SCDIA8'
     C*** BEGIN ADD *** 10/14/97 **************************************
     C     *IN69         ANDEQ     *OFF
     C*** END ADD *** 10/14/97 ****************************************
      *
     C*** BEGIN DEL *** 05/06/97 **************************************
     C**********           MOVE *BLANK    QQDIAG
     C**********           CALL 'F0510R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQDIAG
     C**********           PARM           QQRTCD
     C*** END DEL *** 05/06/97 ****************************************
      *
     C*** BEGIN ADD *** 05/06/97 **************************************
     C                   MOVE      '2'           QQCALL
     C                   MOVE      *BLANKS       QQKC
     C                   MOVE      *BLANKS       QQKL
     C                   MOVE      *BLANKS       QQDIAG
     C                   MOVE      *BLANKS       QQLOST
     C*
     C                   CALL      'F2010R'
     C                   PARM                    QQCALL
     C                   PARM                    QQKC
     C                   PARM                    QQKL
     C                   PARM                    QQDIAG
     C                   PARM                    QQLOST
     C*** END ADD *** 05/06/97 ****************************************
     C*
     C*** BEGIN CHG *** 05/06/97 **************************************
     C********** QQRTCD    IFEQ 'A'
     C     QQKC          IFEQ      '1'                                          ADD 05/06/97
     C                   MOVE      *ON           *INKC
     C                   GOTO      TERM
     C                   ENDIF
      *
     C********** QQRTCD    IFEQ 'B'
     C     QQKL          IFEQ      '1'                                          ADD 05/06/97
     C                   Z-ADD     16            SCLI91                         CHG 08/22/97
     C                   Z-ADD     56            SCPS91
     C                   END
      *
     C********** QQRTCD    IFEQ '*'
     C********** QQDIAG    ANDEQ*BLANKS
     C     QQDIAG        IFEQ      *BLANKS                                      ADD 05/06/97
     C                   Z-ADD     16            SCLI91                         CHG 08/22/97
     C                   Z-ADD     56            SCPS91
     C                   ELSE
     C                   Z-ADD     16            SCLI91                         CHG 08/22/97
     C                   Z-ADD     62            SCPS91
     C                   ENDIF
     C*** END CHG *** 05/06/97 ****************************************
      *
     C                   MOVEL     QQDIAG        SCDIA8
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C*** BEG ADD *** 07/02/98 *************************************
     C*          SCDIA8    IFEQ *BLANKS
     C*                    SETON                     24
     C*                    ELSE
     C*                    SETON                     11
     C*                    ENDIF
     C*** END ADD *** 07/02/98 *************************************
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END DIAG 8
      *
      ** SEARCH FOR DIAGNOSTIC CODE 9
      *
     C     HFLD          IFEQ      'SCDIA9'
     C*** BEGIN ADD *** 10/14/97 **************************************
     C     *IN69         ANDEQ     *OFF
     C*** END ADD *** 10/14/97 ****************************************
      *
     C*** BEGIN DEL *** 05/06/97 **************************************
     C**********           MOVE *BLANK    QQDIAG
     C**********           CALL 'F0510R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQDIAG
     C**********           PARM           QQRTCD
     C*** END DEL *** 05/06/97 ****************************************
      *
     C*** BEGIN ADD *** 05/06/97 **************************************
     C                   MOVE      '2'           QQCALL
     C                   MOVE      *BLANKS       QQKC
     C                   MOVE      *BLANKS       QQKL
     C                   MOVE      *BLANKS       QQDIAG
     C                   MOVE      *BLANKS       QQLOST
     C*
     C                   CALL      'F2010R'
     C                   PARM                    QQCALL
     C                   PARM                    QQKC
     C                   PARM                    QQKL
     C                   PARM                    QQDIAG
     C                   PARM                    QQLOST
     C*** END ADD *** 05/06/97 ****************************************
     C*
     C*** BEGIN CHG *** 05/06/97 **************************************
     C********** QQRTCD    IFEQ 'A'
     C     QQKC          IFEQ      '1'                                          ADD 05/06/97
     C                   MOVE      *ON           *INKC
     C                   GOTO      TERM
     C                   ENDIF
      *
     C********** QQRTCD    IFEQ 'B'
     C     QQKL          IFEQ      '1'                                          ADD 05/06/97
     C                   Z-ADD     16            SCLI91                         CHG 08/22/97
     C                   Z-ADD     62            SCPS91
     C                   END
      *
     C********** QQRTCD    IFEQ '*'
     C********** QQDIAG    ANDEQ*BLANKS
     C     QQDIAG        IFEQ      *BLANKS                                      ADD 05/06/97
     C                   Z-ADD     16            SCLI91                         CHG 08/22/97
     C                   Z-ADD     62            SCPS91
     C                   ELSE
     C                   Z-ADD     17            SCLI91                         CHG 08/22/97
     C                   Z-ADD     20            SCPS91
     C                   ENDIF
      *
     C                   MOVEL     QQDIAG        SCDIA9
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C*** BEG ADD *** 07/02/98 *************************************
     C*          SCDIA9    IFEQ *BLANKS
     C*                    SETON                     11
     C*                    ELSE
     C*                    SETON                     262728
     C*                    ENDIF
     C*** END ADD *** 07/02/98 *************************************
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END DIAG 9
     C*** END CHG *** 05/06/97 ****************************************
      *
      ** SEARCH FOR ATTENDING PHYSICIAN FEIN
      *
     C     HFLD          IFEQ      'SCTFEI'
      *
     C                   MOVE      *BLANKS       ZZFEIN            9
     C                   MOVE      *BLANKS       ZZMOD             3
     C                   MOVE      '2'           QQCALL
     C******************   CALL 'F0031R'                   DEL 03/06/98
     C                   CALL      'F0032R'                                     ADD 03/06/98
     C                   PARM                    QQCALL
     C                   PARM                    ZZFEIN
     C                   PARM                    ZZMOD
     C                   PARM                    QQRTCD                         CHG 03/06/98
     C                   PARM                    QQTYPE            1            ADD 03/06/98
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   GOTO      TERM
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      'B'
     C                   Z-ADD     17            SCLI91                         CHG 08/22/97
     C                   Z-ADD     20            SCPS91
     C                   END
      *
     C     QQRTCD        IFEQ      '*'
     C     ZZFEIN        ANDEQ     *BLANKS
     C                   Z-ADD     17            SCLI91                         CHG 08/22/97
     C                   Z-ADD     20            SCPS91
     C                   ELSE
     C                   Z-ADD     17            SCLI91                         CHG 08/22/97
     C                   Z-ADD     55            SCPS91
     C                   ENDIF
      *
     C                   MOVEL     ZZFEIN        SCTFEI
     C                   MOVEL     ZZMOD         SCTMOD
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C*** BEG ADD *** 07/02/98 *************************************
     C*          SCTFEI    IFEQ *ZEROS
     C*                    SETON                     262728
     C*                    ELSE
     C*                    SETON                     2627
     C*                    ENDIF
     C*** END ADD *** 07/02/98 *************************************
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END PHY FN
      *
      ** SEARCH FOR OTHER PHYSICIAN FEIN
      *
     C     HFLD          IFEQ      'SCOFEI'
      *
     C                   MOVE      *BLANKS       ZZFEIN
     C                   MOVE      *BLANKS       ZZMOD
     C                   MOVE      '2'           QQCALL
     C******************   CALL 'F0031R'                   DEL 03/06/98
     C                   CALL      'F0032R'                                     ADD 03/06/98
     C                   PARM                    QQCALL
     C                   PARM                    ZZFEIN
     C                   PARM                    ZZMOD
     C                   PARM                    QQRTCD                         CHG 03/06/98
     C                   PARM                    QQTYPE                         ADD 03/06/98
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   GOTO      TERM
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      'B'
     C                   Z-ADD     17            SCLI91                         CHG 08/22/97
     C                   Z-ADD     55            SCPS91
     C                   END
      *
     C     QQRTCD        IFEQ      '*'
     C     ZZFEIN        ANDEQ     *BLANKS
     C                   Z-ADD     17            SCLI91                         CHG 08/22/97
     C                   Z-ADD     55            SCPS91
     C                   ELSE
     C                   Z-ADD     18            SCLI91                         CHG 08/22/97
     C                   Z-ADD     20            SCPS91
     C                   ENDIF
      *
     C                   MOVEL     ZZFEIN        SCOFEI
     C                   MOVEL     ZZMOD         SCOMOD
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C*** BEG ADD *** 07/02/98 *************************************
     C*          SCOFEI    IFEQ *ZEROS
     C**                   SETON                     2628
     C*                    ELSE
     C*                    SETON                     26
     C*                    ENDIF
     C*** END ADD *** 07/02/98 *************************************
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END OTH PHY FN
      *
      ** SEARCH FOR SERVICE PROVIDER FEIN
      *
     C     HFLD          IFEQ      'SCSFEI'
      *
     C                   MOVE      *BLANKS       ZZFEIN
     C                   MOVE      *BLANKS       ZZMOD
     C                   MOVE      '2'           QQCALL
     C*** BEGIN ADD *** 07/31/97 **************************************
     C*****                MOVE ' '       QQTYPE  1        DEL 11/06/97
     C*** END ADD *** 07/31/97 ****************************************
     C******************   CALL 'F0031R'                   DEL 03/06/98
     C                   CALL      'F0032R'                                     ADD 03/06/98
     C                   PARM                    QQCALL
     C                   PARM                    ZZFEIN
     C                   PARM                    ZZMOD
     C                   PARM                    QQRTCD                         CHG 03/06/98
     C                   PARM                    QQTYPE                         ADD 03/06/98
     C*** BEGIN ADD *** 07/21/97 **************************************
     C*****                PARM           QQTYPE           DEL 11/06/97
     C*
     C*****                MOVE *BLANKS   QQTYPE           DEL 11/06/97
     C*** END ADD *** 07/21/97 ****************************************
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   GOTO      TERM
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      'B'
     C                   Z-ADD     18            SCLI91                         CHG 08/22/97
     C                   Z-ADD     20            SCPS91
     C                   END
      *
     C     QQRTCD        IFEQ      '*'
     C     ZZFEIN        ANDEQ     *BLANKS
     C                   Z-ADD     18            SCLI91                         CHG 08/22/97
     C                   Z-ADD     20            SCPS91
     C                   ELSE
     C                   Z-ADD     18            SCLI91                         CHG 08/22/97
     C                   Z-ADD     55            SCPS91
     C                   ENDIF
      *
     C                   MOVEL     ZZFEIN        SCSFEI
     C                   MOVEL     ZZMOD         SCSMOD
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C*** BEG ADD *** 07/02/98 *************************************
     C*          SCSFEI    IFEQ *ZEROS
     C*                    SETON                     2728
     C*                    ELSE
     C*                    SETON                     28
     C*                    ENDIF
     C*** END ADD *** 07/02/98 *************************************
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END SERV PROV
      *
      ** SEARCH FOR BILLING PROVIDER FEIN
      *
     C     HFLD          IFEQ      'SCBFEI'
      *
     C                   MOVE      *BLANKS       ZZFEIN
     C                   MOVE      *BLANKS       ZZMOD
     C                   MOVE      '2'           QQCALL
     C******************   CALL 'F0031R'                   DEL 03/06/98
     C                   CALL      'F0032R'                                     ADD 03/06/98
     C                   PARM                    QQCALL
     C                   PARM                    ZZFEIN
     C                   PARM                    ZZMOD
     C                   PARM                    QQRTCD                         CHG 03/06/98
     C                   PARM                    QQTYPE                         ADD 03/06/98
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   GOTO      TERM
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      'B'
     C                   Z-ADD     18            SCLI91                         CHG 08/22/97
     C                   Z-ADD     55            SCPS91
     C                   END
      *
     C     QQRTCD        IFEQ      '*'
     C     ZZFEIN        ANDEQ     *BLANKS
     C                   Z-ADD     18            SCLI91                         CHG 08/22/97
     C                   Z-ADD     55            SCPS91
     C                   ELSE
     C                   Z-ADD     20            SCLI91                         CHG 08/22/97
     C                   Z-ADD     33            SCPS91
     C                   ENDIF
      *
     C                   MOVEL     ZZFEIN        SCBFEI
     C                   MOVEL     ZZMOD         SCBMOD
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C*** BEG ADD *** 07/02/98 *************************************
     C     SCBFEI        IFEQ      *ZEROS
     C                   SETON                                        27
     C                   ELSE
     C                   SETON                                        10
     C                   ENDIF
     C*** END ADD *** 07/02/98 *************************************
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END BILL PROV
     C*** BEGIN ADD *** 04/24/97 **************************************
     C*
     C*** BILL TYPE SEARCH
     C     HFLD          IFEQ      'SCBTYP'
     C*** BEGIN ADD *** 10/14/97 **************************************
     C     *IN69         ANDEQ     *OFF
     C*** END ADD *** 10/14/97 ****************************************
      *
     C*** BEG CHG *** 12/03/00 ****************************************
     C******************** MOVE 'BLTY'    QQID
     C                   MOVE      BTTBID        QQID
     C*** END CHG *** 12/03/00 ****************************************
     C                   MOVE      *BLANKS       QQKEY
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQID
     C                   PARM                    QQKEY
     C                   PARM                    QQRTCD
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   GOTO      TERM
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      'B'
     C                   Z-ADD     12            SCLI91                         CHG 08/22/97
     C                   Z-ADD     55            SCPS91
     C                   ELSE
     C**********           MOVELQQKEY     SCBTYP
     C                   Z-ADD     13            SCLI91                         CHG 08/22/97
     C                   Z-ADD     27            SCPS91
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      '*'
     C     QQKEY         ANDEQ     *BLANKS
     C     QQRTCD        OREQ      ' '
     C                   Z-ADD     12            SCLI91                         CHG 08/22/97
     C                   Z-ADD     55            SCPS91
     C                   ENDIF
      *
     C                   MOVEL     QQKEY         SCBTYP
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C*** BEG ADD *** 07/02/98 *************************************
     C*          SCBTYP    IFEQ *BLANKS
     C*                    SETON                     2022
     C*                    ELSE
     C*                    SETON                     20
     C*                    ENDIF
     C*** END ADD *** 07/02/98 *************************************
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END BILL TYPE
     C*
     C*** END ADD *** 04/24/97 ****************************************
      *
      ** A COMMAND KEY WAS STRUCK ON A NON VALID FIELD ISSUE ERROR
      *
     C     *IN70         IFEQ      *ON
     C                   Z-ADD     12            SCLI91                         CHG 08/22/97
     C                   Z-ADD     20            SCPS91
     C                   MOVE      *ON           *IN90
     C                   GOTO      TERM
     C                   ENDIF
      *
     C                   ENDIF                                                  END *INKJ
      *
     C     TERM          ENDSR
      *
      /EJECT
      **************************************************************
      ** PROGRAM END                                             ***
      **************************************************************
     C     PGMEND        BEGSR
     C*** BEG CHG *** 04/24/97 ***********************************
     C****       *INKL     IFEQ *ON
     C****                 MOVE 'B'       QQRTCD
     C****                 ELSE
     C****                 MOVE ' '       QQRTCD
     C****                 ENDIF                           *INKL
     C                   SELECT
     C     *INKC         WHENEQ    *ON
     C                   MOVE      'A'           QQRTCD
     C     *INKL         WHENEQ    *ON
     C                   MOVE      'B'           QQRTCD
     C                   OTHER
     C                   MOVE      ' '           QQRTCD
     C                   ENDSL                                                  *INKL
     C*** BEG CHG *** 04/24/97 ***********************************
     C                   ENDSR
      *
      /EJECT
      **************************************************************
      ** PROGRAM INITIALIZATION                                  ***
      **  ONE TIME                                               ***
      **************************************************************
     C     PGMSTR        BEGSR
     C*** BEG ADD *** 11/07/97 ***********************
     C                   MOVE      '1'           XXFTYP            1            FUND TYPE
     C*** END ADD *** 11/07/97 ***********************
     C                   MOVE      *BLANKS       SCCONM           40
     C                   MOVE      '0'           F3REQD
     C****                 Z-ADDSCACDT    SRDTI            MJF
     C****                 CALL 'SRCVTDAT'DTMDYY            *
     C****                 MOVELSRDTO     SCACDT            *
     C                   MOVEL     XXCO#         KYCO#
     C     QQPREX        IFEQ      'Y'
     C                   MOVE      *ON           *IN99
     C                   ELSE
     C                   MOVE      *OFF          *IN99
     C                   ENDIF
     C     QQPREC        IFEQ      'Y'
     C                   MOVE      *ON           *IN98
     C                   ELSE
     C                   MOVE      *OFF          *IN98
     C                   ENDIF
     C*** BEGIN DEL ** 07/03/97 ************************************************
     C********   QQWCCE    IFEQ 'N'
     C********   QQWCCE    OREQ *BLANK
     C*** END DEL ** 07/03/97 **************************************************
     C*** BEGIN ADD ** 07/03/97 ************************************************
     C*** BEGIN DEL ** 11/06/97 ************************************************
     C****       QQWCCE    IFNE 'C'
     C*** END ADD ** 07/03/97 **************************************************
     C****                 MOVE *ON       *IN97
     C****                 ELSE
     C                   MOVE      *OFF          *IN97                          CHG 11/06/97
     C****                 ENDIF
     C*** BEGIN DEL ** 11/06/97 ************************************************
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV                          ADDED 03/28/95
     C     EMKEY         CHAIN(N)  PEM01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  EMNAME        SCENAM
     C                   ELSE
     C                   CLEAR                   SCENAM
     C                   ENDIF
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
      *
     C                   MOVEL(P)  SCAST         XXSTCN            6
     C     4             SUBST     Q1CNTY:6      XCNTY             4
     C                   MOVE      XCNTY         XXSTCN
     C                   MOVEL(P)  XXSTCN        XXKEY
      *
     C     TBKEY         CHAIN     PCC01                              60
     C     *IN60         IFEQ      '1'
     C                   CLEAR                   SCCNTY
     C                   ELSE
     C                   MOVEL     CCDESC        SCCNTY
     C                   END
     C*
     C*** BEGIN ADD *** 11/08/00 **************************************
     C* Load table id for type of bill based on Adjudication state
     C     SCAST         CAT(P)    'BT':0        BTTBID            4
     C                   MOVEL(P)  BTTBID        KYTBID
     C     GTKY2         CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'ZZBT'        KYTBID
     C     GTKY2         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'BLTY'        BTTBID
     C                   ELSE
     C                   MOVEL(P)  'ZZBT'        BTTBID
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 11/08/00 **************************************
     C*** BEGIN ADD *** 04/24/97 **************************************
     C*
     C*** PROTECT 'TYPE OF BILL' FIELD IF SYSTEM TYPE IS P
     C*
     C     QQSYS         IFEQ      'P'
     C                   MOVE      *ON           *IN30                          PROTECT
     C                   ELSE
     C                   MOVE      *OFF          *IN30
     C                   ENDIF
     C*** END ADD *** 04/24/97 ****************************************
     C                   TIME                    DSDT                           ADD 06/26/96
     C                   Z-ADD     DSMDY         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYDATE            7 0
     C*
     C*** BEGIN ADD *** 08/22/97 **************************************
     C*
     C*** PROTECT PREVIOUS EOMB# IF ENTERED
     C*
     C     SCPEOB        IFNE      *ZEROS
     C                   MOVE      *ON           *IN51                          PROTECT
     C                   ELSE
     C                   MOVE      *OFF          *IN51
     C                   ENDIF
     C*** END ADD *** 02/22/97 ****************************************
     C*** BEGIN ADD *** 10/14/97 **************************************
     C*
     C*** PROTECT TYPE OF BILL AND DIAGNOSIS CODES FIELDS
     C*
     C     QQPRTC        IFEQ      'A'
     C                   SETON                                        69
     C                   ELSE
     C                   SETOFF                                       69
     C                   ENDIF
     C*** END ADD *** 10/14/97 ****************************************
     C*** BEGIN ADD *** 04/06/98 **************************************
     C*
     C* SET INDICATOR ACCORDING TO WHETHER 'P' TYPE OR 'H' TYPE, THIS
     C* WILL HIGHLIGHT REQUIRED FIELDS FOR THE TO TYPES.
     C*
     C                   SELECT
     C*
     C     QQSYS         WHENEQ    'P'
     C                   SETON                                        16
     C                   SETOFF                                       17
     C*
     C     QQSYS         WHENEQ    'H'
     C                   SETON                                        17
     C                   SETOFF                                       16
     C*
     C                   ENDSL
     C*
     C*** END   ADD *** 04/06/98 **************************************
     C*
     C                   ENDSR
      /EJECT
      **************************************************************
      ** PROCESSING AFTER ALL EDITS PASSED                       ***
      **************************************************************
     C     PRSCTL        BEGSR
     C                   ENDSR
      /EJECT
      **************************************************************
      ** ANY PRE SCREEN PROCESSING DONE HERE                     ***
      **************************************************************
     C     PREDSP        BEGSR
     C                   ENDSR
      /EJECT
     C*
