     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*
     H* F0092R - HOSPITAL ENTRY SCREEN
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: F0092R                                            *
     H*    CREATED: 03/06/96  C:43,45 87MAW                           *
     H*    DESCRIPTION: LOAD THE HOSPITAL DATA FOR F0070R             *
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
     H*                                                               *
     H*  REVISIONS:                                                   *
     H*
     H* 06/09/20  C:212 Project : SY420
     H*                  Mask SSN
     H*
     H* 10/12/98 C:53 MC145
     H*                     QUALIFY CALL TO F4200ROH
     H*                                                               *
     H* 03/10/98 C:56 MC145 YEAR 2000
     H*            1. CHANGED PARM ISSUE
     H*                                                               *
     H* 11/06/97 : MC145 YEAR 2000
     H*            1. REMOVE REQUIRED EDITS
     H*                                                               *
     H* 03/24/97 : GO LIVE ISSUE:
     H*            1. TYPE OF BILL : CHG FROM OPTIONAL TO REQ'D
     H*            2. HOSPITAL ACCOMODATION DAILY RATE:
     H*                  CHG FROM REQ'D TO CONDITIONALLY REQ'D
     H*
     H* 04/24/97 C:56  PROJECT: 87MAW
     H*                1. CHANGE NAME OF 'TYPE OF BILL' FIELD FROM
     H*                   SCBTYP TO SCBTY2.
     H*                2. REMOVE VALIDATION OF 'TYPE OF BILL' FIELD
     H*                   BECAUSE IT IS NOW DISPLAY ONLY.  LEAVE
     H*                   CHAIN TO GENERIC TABLE FILE TO GET
     H*                   'SUB-TYPE' FOR OTHE VALIDATION.
     H*
     H* 05/06/97 C:56  Project: 87MAW
     H*             1. Replace ICD9 Searchs F0510R/F ---> F2010R/F
     H*
     H* 06/06/97 C:32  Project: 87MBY
     H*                REMOVE EDITS FOR ICD9 CODE AND ADD CALL
     H*                CALL FOR PGM F4200ROH
     H*
     H* 07/22/97 C:56  Project: 87MAW
     H*            1.  Add edit: If Bill Type is Inpatient, Statement
     H*                Covers From Date and To Date cannot be equal.
     H*            2.  Add edit of Covers From Date and To Date: vs
     H*                accident date.
     H*
     H* 08/01/97  C:56 Project: 87MAW CO# 4
     H*                Add Active/Inactive code edits.
     H*
     H*  08/07/97 C:32 Project: 87MBY - CO# 2
     H*                Remove edit for invalid ICD9 code.
     H*
     H*  07/03/97 C:31  Project: 87MCD
     H*                 Loading BWC Provider Types
     H*                 1. Change test for BWC status
     H*
     H*****************************************************************
     H*
     FF0092F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FFTABL     IF   E           K DISK
     F                                     IGNORE(PRC01)
     FFMEML     IF   E           K DISK
      * DIAGNOSIS CODE FILE
     FFMDIL     IF   E           K DISK
      * REASONABLE/CUSTOMARY FILE
     FFMRCL1    IF   E           K DISK
      * PROCEDURE CODE FILE
     FFMPRL     IF   E           K DISK
      * GENERIC TABLE FILE
     FFTGTL     IF   E           K DISK

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************
      /COPY SCOPYPR01D
     D SCR92         E DS                  EXTNAME(F0092F)
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
     D  PUIDUMMY18   E                     EXTFLD(SCACDT)
     D  PUIDUMMY11   E                     EXTFLD(SCMMI)
     D  PUIDUMMY12   E                     EXTFLD(SCCNME)
     D  PUIDUMMY13   E                     EXTFLD(SCSCL#)
     D  PUIDUMMY14   E                     EXTFLD(SCFEIN)
     D  PUIDUMMY15   E                     EXTFLD(SCMOD)
     D  PUIDUMMY16   E                     EXTFLD(SCPNME)
     D  PUIDUMMY17   E                     EXTFLD(SCZIP)
     D*PUIMOD*** END ADD
      /ENDIF

      ***PY SCOPYPR01I
      /COPY SCOPYPR01C
      *
      *   KEY LIST FOR THE DIAGNOSIS CODE FILE -- FMDIL
      *

     C     DIKY          KLIST
     C                   KFLD                    DIKEY             5
     C                   KFLD                    XXLOST            1
      *
      *   KEY LIST FOR REASONABLE/CUSTOMARY FILE -- FMRCL1 AND
      *   KEY LIST FOR PROCEDURE CODE FILE -- FMPRL
      *
     C     CPT4KY        KLIST
     C                   KFLD                    KYCO#
     C                   KFLD                    KYCPT4
      *
      *   KEY LIST FOR GENERIC TABLE FILE -- FTGTL
      *
     C     GTKY          KLIST
     C                   KFLD                    KYCO#
     C                   KFLD                    KYTBID
     C                   KFLD                    KYKEY
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
     C                   PARM                    SCACDT
     C                   PARM                    SCMMI
     C                   PARM                    SCCNME
     C                   PARM                    SCSCL#
     C                   PARM                    SCFEIN
     C                   PARM                    SCMOD
     C                   PARM                    SCPNME
     C                   PARM                    SCZIP
     C                   PARM                    QQWCCE            1
     C****************     PARM           Q1PST   2
     C                   PARM                    Q1CNTY           15
     C     SCPRDS        PARM                    QQPRDS           40
     C     SCR92         PARM                    SCR92
     C                   PARM                    QQRTCD            1
     C*
      *
      * INITIALIZE WORK FIELDS
      *
     C     *LIKE         DEFINE    RCCO#         KYCO#
     C     *LIKE         DEFINE    RCCO#         XXCO#
     C     *LIKE         DEFINE    RCCPT4        KYCPT4
     C     *LIKE         DEFINE    GTTBID        KYTBID
     C     *LIKE         DEFINE    GTKEY         KYKEY
     C     *LIKE         DEFINE    GTTBID        QQID
     C     *LIKE         DEFINE    GTKEY         QQKEY
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
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       scSS# = UnMaskData('SSN':'SCSS#':SCSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C                   ENDSR
      /EJECT
      **************************************************************
      ** ALL SCREEN FILE FIELD EDITS WILL BE CONTROLLED HERE     ***
      ** IF ANY FAILED ¢DITS, THEN *IN90 MUST BE SET ON          ***
      **************************************************************
     C     ERRCK1        BEGSR
     C*** BEGIN ADD *** 07/22/97 **************************************
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZACDT            7 0
     C*** END ADD *** 07/22/97 ****************************************
     C     *IN12         IFEQ      *ON
      *
      ** THE FOLLOWING INDICATORS NEED TO BE SETOFF BECAUSE THEY ARE NOT
      ** HANDLED BY THE COPYBOOKS.
      *
     C                   MOVEA     '0000000'     *IN(33)
     C                   MOVEA     '00000000'    *IN(40)
     C                   MOVEA     '0000'        *IN(48)
     C                   MOVEA     '00000000'    *IN(52)                        ADD 06/06/97
      *
     C                   EXSR      FUNKEY
     C                   ELSE
     C                   MOVEA     '00000000'    *IN(52)                        ADD 06/06/97
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
     C*** BEGIN ADD *** 07/22/97 **************************************
     C                   MOVE      *OFF          *IN31
     C                   MOVE      *OFF          *IN32
     C                   MOVE      *OFF          *IN39
     C                   MOVE      *OFF          *IN40
     C                   MOVEA     '0000000'     *IN(60)
     C                   MOVEA     '0000'        *IN(66)
     C                   MOVEA     '000'         *IN(21)
     C                   MOVEA     '00000'       *IN(25)
     C*** END ADD *** 07/22/97 ****************************************
     C*
     C*** BEGIN ADD *** 08/01/97 **************************************
     C                   MOVE      '0'           *IN65
     C                   MOVE      '0'           *IN11
     C                   MOVEA     '00000'       *IN(13)
     C                   MOVEA     '00'          *IN(18)
     C*** END ADD *** 08/01/97 ****************************************
      *
      ** VALIDATE TYPE OF BILL (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN40
      *
     C*** BEG CHG *** 03/24/97 ***
     C********   SCBTYP    IFGT *BLANKS
     C                   MOVE      *BLANK        XTFG01            1            INPAT/OUTPAT
     C*** END CHG *** 03/24/97 ***
      *
     C*** BEGIN DEL *** 04/24/97 **************************************
     C**********           MOVEL'BLTY'    KYTBID    P
     C**********           MOVELSCBTYP    KYKEY     P
     C********** GTKY      CHAINPGT01                60
     C**********
     C********** *IN60     IFEQ *ON
     C**********           MOVE *ON       *IN40            MCR0513
     C**********           MOVE *ON       *IN90
     C**********           GOTO ENDEDT
     C**********           ENDIF
     C*** END DEL *** 04/24/97 ****************************************
     C*
     C*** BEGIN ADD *** 04/24/97 **************************************
     C                   MOVEL(P)  'BLTY'        KYTBID
     C*** BEGIN CHG *** 07/22/97 **************************************
     C**********           MOVELSCBTY2    KYKEY
     C                   MOVEL(P)  SCBTY2        KYKEY
     C*** END CHG *** 07/22/97 ****************************************
     C     GTKY          CHAIN     PGT01                              60
     C*** END ADD *** 04/24/97 ****************************************
     C*** BEG CHG *** 03/24/97 ***
     C******************   ENDIF
     C                   MOVE      GTFG01        XTFG01                         INPAT/OUTPAT
     C*** END CHG *** 03/24/97 ***
      *
     C*** BEGIN ADD *** 07/22/97 **************************************
     C*
     C*** STATEMENT COVERS FROM AND TO DATES MUST BE ENTERED IF
     C*** BILL TYPE IS INPATIENT
     C*
     C*** BEG ADD *** 11/06/97 ****************************************
     C****       XTFG01    IFEQ 'I'
     C****
     C****       SCDT07    IFEQ *ZEROS
     C****                 MOVE *ON       *IN27
     C****                 MOVE *ON       *IN90
     C****                 GOTO ENDEDT
     C****                 ENDIF
     C****
     C****       SCDT08    IFEQ *ZEROS
     C****                 MOVE *ON       *IN61
     C****                 MOVE *ON       *IN90
     C****                 GOTO ENDEDT
     C****                 ENDIF
     C****                 ENDIF                           XTFG01 IFEQ
     C*** BEG ADD *** 11/06/97 ****************************************
     C*** END ADD *** 07/22/97 ****************************************
     C*
      ** VALIDATE STATEMENT COVERS FROM DATE (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN71
      *
     C     SCDT07        IFGT      *ZEROS
      *
     C                   Z-ADD     SCDT07        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDT07
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN71                          MCR0500
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 07/22/97 **************************************
     C                   Z-ADD     SCDT07        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZDT07            7 0
     C     ZZDT07        IFLT      ZZACDT
     C                   MOVE      *ON           *IN31
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF                                                  ZZDT07 IFLT
     C*** END ADD *** 07/22/97 ****************************************
      *
     C                   ENDIF
      *
      ** VALIDATE STATEMENT COVERS TO DATE (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN72
      *
     C     SCDT08        IFGT      *ZEROS
      *
      *
     C                   Z-ADD     SCDT08        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDT08
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN72                          MCR0500
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 07/22/97 **************************************
     C                   Z-ADD     SCDT08        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZDT08            7 0
     C     ZZDT08        IFLT      ZZACDT
     C                   MOVE      *ON           *IN32
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF                                                  ZZDT07 IFLT
     C*** END ADD *** 07/22/97 ****************************************
      *
     C                   ENDIF
     C*** BEGIN ADD *** 07/22/97 **************************************
     C*
     C*** IF BILL TYPE IS INPATIENT, COVERS FROM AND TO CANNOT BE EQUAL
     C*
     C                   MOVE      *OFF          *IN30
     C     XTFG01        IFEQ      'I'
     C*
     C     SCDT07        IFNE      *ZEROS
     C     SCDT08        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCDT07        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT07            7 0
     C                   Z-ADD     SCDT08        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT08            7 0
     C     XXDT07        IFEQ      XXDT08
     C                   MOVE      *ON           *IN30
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF                                                  XXDT07 IFEQ
     C*
     C                   ENDIF                                                  SCDT07 IFNE
     C*
     C                   ENDIF                                                  XTFG01 IFEQ
     C*
     C** BOTH  COVERS FROM DATE AND TO DATE MUST BE ENTER FOR RANGE
     C** TO BE VALID
     C*
     C     SCDT07        IFNE      *ZEROS
     C     SCDT08        ANDEQ     *ZEROS
     C                   MOVE      *ON           *IN40
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*
     C     SCDT08        IFNE      *ZEROS
     C     SCDT07        ANDEQ     *ZEROS
     C                   MOVE      *ON           *IN40
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*
     C** COVERS FROM DATE CANNOT BE GREATER THAN TO DATE
     C*
     C     SCDT07        IFNE      *ZEROS
     C     SCDT08        ORNE      *ZEROS
     C*
     C                   Z-ADD     SCDT07        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT07
     C                   Z-ADD     SCDT08        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT08
     C*
     C     XXDT07        IFGT      XXDT08
     C                   MOVE      *ON           *IN39
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF                                                  XXDT07 IFGT
     C                   ENDIF                                                  SCDT07 IFNE
     C*** END ADD *** 07/22/97 ****************************************
      *
      ** VALIDATE COVERED DAYS (OPTIONAL) NO EDIT
      *
      ** VALIDATE NON-COVERED DAYS (OPTIONAL) NO EDIT
      *
      ** VALIDATE ADMISSION DATE (REQUIRED)
      *
     C                   MOVE      *OFF          *IN73
      *
     C*** BEG DEL *** 11/06/97 *************************
     C****       SCDT06    IFLE *ZEROS
     C****       XTFG01    ANDEQ'I'
     C****                 MOVE *ON       *IN73            MCR0500
     C****                 MOVE *ON       *IN90
     C****                 GOTO ENDEDT
     C****                 ENDIF
     C*** BEG DEL *** 11/06/97 *************************
      *
     C     SCDT06        IFGT      *ZEROS
      *
     C                   Z-ADD     SCDT06        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDT06
      *
     C     SRERR         IFEQ      *ON
     C*** BEGIN CHG *** 07/22/97 **************************************
     C**********           MOVE *ON       *IN73            MCR0500
     C                   MOVE      *ON           *IN63
     C*** END CHG *** 07/22/97 ****************************************
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 07/22/97 **************************************
     C** VALIDATE AGAINST ACCIDENT DATE
     C*
     C                   Z-ADD     SCDT06        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZDT06
     C     ZZDT06        IFLT      ZZACDT
     C                   MOVE      *ON           *IN64
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** END ADD *** 07/22/97 ****************************************
      *
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 07/22/97 **************************************
     C** IF INPATIENT BILL, ADMISSION DATE MUST BE WITH STATEMENT COVERS
     C** DATE RANGE
     C*
     C     XTFG01        IFEQ      'I'
     C*
     C                   Z-ADD     SCDT06        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZDT06            7 0
     C                   Z-ADD     SCDT07        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZDT07
     C                   Z-ADD     SCDT08        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZDT08
     C*
     C     ZZDT06        IFGT      ZZDT08
     C     ZZDT06        ORLT      ZZDT07
     C                   MOVE      *ON           *IN62
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF                                                  ZZDT06 IFGT
     C                   ENDIF                                                  XTFG01 IFEQ
     C*** END ADD *** 07/22/97 ****************************************
      *
      ** VALIDATE ADMISSION HOUR (REQUIRED)
      *
     C                   MOVE      *OFF          *IN74
      *
     C*** BEGIN DEL ** 11/06/97 ************************************************
     C*****      SCADHR    IFLE *ZEROS
     C*****      XTFG01    ANDEQ'I'
     C*****                MOVE *ON       *IN74            MCR0502
     C*****                MOVE *ON       *IN90
     C*****                GOTO ENDEDT
     C*****                ENDIF
     C*** END   DEL ** 11/06/97 ************************************************
      *
     C     SCADHR        IFGT      *ZEROS
      *
     C                   Z-ADD     *ZEROS        XXADHH            2 0
     C                   Z-ADD     *ZEROS        XXADMM            2 0
     C                   MOVEL     SCADHR        XXADHH
     C                   MOVE      SCADHR        XXADMM
      *
     C     XXADHH        IFGT      24
     C     XXADMM        ORGT      59
     C                   MOVE      *ON           *IN74                          MCR0502
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
      *
     C                   ENDIF
      *
      ** VALIDATE ADMISSION TYPE (REQUIRED)
      *
     C                   MOVE      *OFF          *IN75
     C                   MOVE      *OFF          *IN33
      *
     C*** BEGIN DEL ** 11/06/97 ************************************************
     C*****      SCADTY    IFEQ *BLANKS
     C*****      XTFG01    ANDEQ'I'
     C*****                MOVE *ON       *IN75            MCR0503
     C*****                MOVE *ON       *IN33
     C*****                MOVE *ON       *IN90
     C*****                GOTO ENDEDT
     C*****                ENDIF
     C*** END   DEL ** 11/06/97 ************************************************
      *
     C     SCADTY        IFGT      *BLANKS
      *
     C                   MOVEL(P)  'ADTY'        KYTBID
     C                   MOVEL(P)  SCADTY        KYKEY
     C     GTKY          CHAIN     PGT01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN75                          MCR0513
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 08/01/97 **************************************
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'I'
     C                   MOVE      *ON           *IN65
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF                                                  *IN60 IFEQ
     C*** END ADD *** 07/01/97 ****************************************
      *
     C                   ENDIF
      *
      ** VALIDATE ADMISSION SOURCE (REQUIRED)
      *
     C                   MOVE      *OFF          *IN76
     C                   MOVE      *OFF          *IN34
      *
     C*** BEG   DEL ** 11/06/97 ************************************************
     C*****      SCADSC    IFEQ *BLANKS
     C*****      XTFG01    ANDEQ'I'
     C*****                MOVE *ON       *IN76            MCR0503
     C*****                MOVE *ON       *IN34
     C*****                MOVE *ON       *IN90
     C*****                GOTO ENDEDT
     C*****                ENDIF
     C*** END   DEL ** 11/06/97 ************************************************
      *
     C     SCADSC        IFGT      *BLANKS
      *
     C                   MOVEL(P)  'ADSC'        KYTBID
     C                   MOVEL(P)  SCADSC        KYKEY
     C     GTKY          CHAIN     PGT01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN76                          MCR0513
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 08/01/97 **************************************
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'I'
     C                   MOVE      *ON           *IN12
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF                                                  *IN60 IFEQ
     C*** END ADD *** 08/01/97 ****************************************
      *
     C                   ENDIF
      *
      ** VALIDATE DISCHARGE HOUR (REQUIRED)
      *
     C                   MOVE      *OFF          *IN77
      *
     C*** BEG   DEL ** 11/06/97 ************************************************
     C*****      SCDCHR    IFLE *ZEROS
     C*****      XTFG01    ANDEQ'I'
     C*****                MOVE *ON       *IN77            MCR0502
     C*****                MOVE *ON       *IN90
     C*****                GOTO ENDEDT
     C*****
     C*****                ENDIF
     C*** END   DEL ** 11/06/97 ************************************************
     C*** BEG   DEL ** 11/06/97 ************************************************
     C*****
     C*****
     C*****
     C*****
     C*****
     C*****
     C*****
     C*****
     C*** END   DEL ** 11/06/97 ************************************************
      *
     C     SCDCHR        IFGT      *ZEROS
      *
     C                   Z-ADD     *ZEROS        XXDCHH            2 0
     C                   MOVE      SCDCHR        XXDCHH
     C     XXDCHH        IFGT      24
     C                   MOVE      *ON           *IN77                          MCR0678
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
      *
     C                   ENDIF
      *
      ** VALIDATE PATIENT STATUS (REQUIRED)
      *
     C                   MOVE      *OFF          *IN78
     C                   MOVE      *OFF          *IN35
      *
     C*** BEG   DEL ** 11/06/97 ************************************************
     C*****      SCPATS    IFEQ *BLANKS
     C*****      XTFG01    ANDEQ'I'
     C*****                MOVE *ON       *IN78            MCR0503
     C*****                MOVE *ON       *IN35
     C*****                MOVE *ON       *IN90
     C*****                GOTO ENDEDT
     C*****                ENDIF
     C*** BEG   DEL ** 11/06/97 ************************************************
      *
     C     SCPATS        IFGT      *BLANKS
      *
     C                   MOVEL(P)  'PATS'        KYTBID
     C                   MOVEL(P)  SCPATS        KYKEY
     C     GTKY          CHAIN     PGT01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN78                          MCR0513
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** BEGIN ADD *** 08/01/97 **************************************
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'I'
     C                   MOVE      *ON           *IN13
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** END ADD *** 08/01/97 ****************************************
      *
     C                   ENDIF
      *
      ** VALIDATE OCC CODE (REQUIRED)
      *
     C                   MOVE      *OFF          *IN79
     C                   MOVE      *OFF          *IN36
      *
     C*** BEG   DEL ** 11/06/97 ************************************************
     C*****      SCOCD1    IFEQ *BLANKS
     C*****      XTFG01    ANDEQ'I'
     C*****                MOVE *ON       *IN79            MCR0503
     C*****                MOVE *ON       *IN36
     C*****                MOVE *ON       *IN90
     C*****                GOTO ENDEDT
     C*****                ENDIF
     C*** END   DEL ** 11/06/97 ************************************************
      *
     C     SCOCD1        IFGT      *BLANKS
      *
     C                   MOVEL(P)  'OCCD'        KYTBID
     C                   MOVEL(P)  SCOCD1        KYKEY
     C     GTKY          CHAIN     PGT01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN79                          MCR0513
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** BEGIN ADD *** 08/01/97 **************************************
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'I'
     C                   MOVE      *ON           *IN14
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** END ADD *** 08/01/97 ****************************************
      *
     C                   ENDIF
      *
      ** VALIDATE OCC DATE (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN80
      *
     C     SCODT1        IFGT      *ZEROS
      *
      *
     C                   Z-ADD     SCODT1        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCODT1
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN80                          MCR0500
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** BEGIN ADD *** 07/22/97 **************************************
     C*
     C** VALIDATE AGAINST ACCIDENT DATE
     C*
     C                   Z-ADD     SCODT1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZODT1            7 0
     C     ZZODT1        IFLT      ZZACDT
     C                   MOVE      *ON           *IN28
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** END ADD *** 07/22/97 ****************************************
      *
     C                   ENDIF
      *
      ** VALIDATE SPAN BEGIN DATE (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN81
      *
     C     SCOSFD        IFGT      *ZEROS
      *
      *
     C                   Z-ADD     SCOSFD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCOSFD
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN81                          MCR0500
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** BEGIN ADD *** 07/22/97 **************************************
     C                   Z-ADD     SCOSFD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZOSFD            7 0
     C     ZZOSFD        IFLT      ZZACDT
     C                   MOVE      *ON           *IN21
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** END ADD *** 07/22/97 ****************************************
      *
     C                   ENDIF
      *
      ** VALIDATE SPAN END DATE (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN82
      *
     C     SCOSTD        IFGT      *ZEROS
      *
      *
     C                   Z-ADD     SCOSTD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCOSTD
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN82                          MCR0500
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** BEGIN ADD *** 07/22/97 **************************************
     C                   Z-ADD     SCOSTD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZOSTD            7 0
     C     ZZOSTD        IFLT      ZZACDT
     C                   MOVE      *ON           *IN22
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** END ADD *** 07/22/97 ****************************************
      *
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 07/22/97 **************************************
     C*
     C     SCOSFD        IFNE      *ZEROS
     C     SCOSTD        ANDEQ     *ZEROS
     C                   MOVE      *ON           *IN29
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*
     C     SCOSTD        IFNE      *ZEROS
     C     SCOSFD        ANDEQ     *ZEROS
     C                   MOVE      *ON           *IN29
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*
     C     SCOSFD        IFNE      *ZEROS
     C     SCOSTD        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCOSFD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZOSFD
     C                   Z-ADD     SCOSTD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZOSTD
     C*
     C     ZZOSFD        IFGT      ZZOSTD
     C                   MOVE      *ON           *IN23
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF                                                  ZZOSFD IFNE
     C*
     C                   ENDIF                                                  SCOSFD IFNE
     C*** END ADD *** 07/22/97 ****************************************
      *
      ** VALIDATE TYPE OF SERVICE (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN38
      *
     C     SCHTOS        IFGT      *BLANKS
      *
     C                   MOVEL(P)  'SVTY'        KYTBID
     C                   MOVEL(P)  SCHTOS        KYKEY
     C     GTKY          CHAIN     PGT01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN38                          MCR0513
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** BEGIN ADD *** 08/01/97 **************************************
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'I'
     C                   MOVE      *ON           *IN15
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** END ADD *** 08/01/97 ****************************************
      *
     C                   ENDIF
      *
      ** VALIDATE BILL STATUS (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN83
      *
     C     SCTOB         IFGT      *BLANKS
      *
     C                   MOVEL(P)  'BLST'        KYTBID
     C                   MOVEL(P)  SCTOB         KYKEY
     C     GTKY          CHAIN     PGT01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN83                          MCR0513
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** BEGIN ADD *** 08/01/97 **************************************
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'I'
     C                   MOVE      *ON           *IN16
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** END ADD *** 08/01/97 ****************************************
      *
     C                   ENDIF
      *
      ** VALIDATE VALUE CODE (REQUIRED)
      *
     C                   MOVE      *OFF          *IN84
     C                   MOVE      *OFF          *IN37
      *
     C*** BEG   DEL ** 11/06/97 ************************************************
     C*****      SCVC01    IFEQ *BLANKS
     C*****      XTFG01    ANDEQ'I'
     C*****                MOVE *ON       *IN84            MCR0503
     C*****                MOVE *ON       *IN37
     C*****                MOVE *ON       *IN90
     C*****                GOTO ENDEDT
     C*****                ENDIF
     C*** END   DEL ** 11/06/97 ************************************************
      *
     C     SCVC01        IFGT      *BLANKS
      *
     C                   MOVEL(P)  'VLCD'        KYTBID
     C                   MOVEL(P)  SCVC01        KYKEY
     C     GTKY          CHAIN     PGT01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN84                          MCR0513
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** BEGIN ADD *** 08/01/97 **************************************
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'I'
     C                   MOVE      *ON           *IN17
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** END ADD *** 08/01/97 ****************************************
      *
     C                   ENDIF
      *
      ** VALIDATE AMOUNT (REQUIRED)
      *
     C*** BEG   DEL ** 11/06/97 ************************************************
     C*****      SCVA01    IFLE *ZEROS
     C*****      XTFG01    ANDEQ'I'
     C*****                MOVE *ON       *IN85
     C*****                MOVE *ON       *IN90
     C*****                GOTO ENDEDT
     C*****                ENDIF
     C*** END   DEL ** 11/06/97 ************************************************
      *
      ** VALIDATE HOSPITAL ACCOM DAILY RATE (REQUIRED)
      *
     C*** BEG ADD *** 03/24/97 ***
     C     XTFG01        IFEQ      'I'
     C*** END ADD *** 03/24/97 ***
     C*** BEG   DEL ** 11/06/97 ************************************************
     C*****      SCACRT    IFLE *ZEROS
     C*****                MOVE *ON       *IN86
     C*****                MOVE *ON       *IN90
     C*****                GOTO ENDEDT
     C*****                ENDIF
     C*** END   DEL ** 11/06/97 ************************************************
     C*** BEG ADD *** 03/24/97 ***
     C                   ENDIF                                                  XTFG01=I
     C*** END ADD *** 03/24/97 ***
      *
      ** VALIDATE ADMIT DIAGNOSTIC CODE (REQUIRED)
      *
     C                   MOVE      *OFF          *IN87
      *
     C*** BEG   DEL ** 11/06/97 ************************************************
     C*****      SCADDI    IFEQ *BLANKS
     C*****      XTFG01    ANDEQ'I'
     C*****                MOVE *ON       *IN87            MCR0742
     C*****                MOVE *ON       *IN90
     C*****                GOTO ENDEDT
     C*****                ENDIF
     C*** END   DEL ** 11/06/97 ************************************************
      *
     C     SCADDI        IFGT      *BLANKS
      *
     C                   MOVEL     *BLANK        DIKEY             5
     C                   MOVEL     SCADDI        DIKEY
     C                   MOVE      'D'           XXLOST
     C     DIKY          CHAIN     PDI01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN87                          MCR0742
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
      *
     C*** BEGIN ADD 06/06/97 ******************************************
     C     XXFTYP        IFNE      '1'                                          ADD 10/12/98
     C*** CALL ICD9 EDIT PROGRAM
     C                   MOVEL     SCADDI        QQICD9
     C                   MOVE      'S'           QQPRSC
     C                   MOVE      'B'           Q2CALL
     C                   MOVE      'H'           QQSYS
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
     C                   MOVE      *OFF          *IN52
     C                   MOVE      *OFF          *IN59
     C* CHECK ERROR FLAG FOR 'E' OR 'V' CODE
     C     QQERR         IFEQ      'EV'
     C     QQERR         OREQ      'E '
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN52
     C                   MOVE      *ON           *IN59
     C                   GOTO      ENDEDT
     C                   ENDIF
     C                   ENDIF                                                  ADD 10/12/98
     C*
     C*** BEGIN DEL *** 08/07/97 **************************************
     C* CHECK WHETHER ICD9 CODE HAS BEEN FLAGGED AS BWC INVALID
     C********   QQVLD     IFEQ 'I'
     C********             MOVE *ON       *IN90
     C********             MOVE *ON       *IN52
     C********             MOVE *ON       *IN59
     C********             GOTO ENDEDT
     C********             ENDIF
     C********
     C*** END   DEL *** 08/07/97 **************************************
     C*** END   ADD 06/06/97 ******************************************
     C                   ENDIF
      *
      ** VALIDATE PROCEDURE CODE METHOD (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN88
      *
     C     SCPCDM        IFGT      *BLANKS
      *
     C                   MOVEL(P)  'PCMT'        KYTBID
     C                   MOVEL(P)  SCPCDM        KYKEY
     C     GTKY          CHAIN     PGT01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN88                          MCR0513
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** BEGIN ADD *** 08/01/97 **************************************
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'I'
     C                   MOVE      *ON           *IN18
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** END ADD *** 08/01/97 ****************************************
      *
     C                   ENDIF
      *
      ** VALIDATE PRINCIPAL PROCEDURE CODE (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN89
      *
     C     SCPCDM        IFEQ      ' '
     C     SC7A01        ANDGT     *BLANKS
     C                   MOVE      *ON           *IN89                          MCR0687
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
      *
     C     SCPCDM        IFEQ      '0'
     C     SC7A01        ANDGT     *BLANKS
     C                   MOVE      *ON           *IN89                          MCR0687
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
      *
     C     SCPCDM        IFEQ      '4'
     C     SC7A01        ANDEQ     *BLANKS
     C     SCPCDM        OREQ      '5'
     C     SC7A01        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN89                          MCR0687
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
      *
     C     SCPCDM        IFEQ      '5'
     C                   MOVEL     SC7A01        XXHCHK            1
     C     XXHCHK        IFGE      '0'
     C                   MOVE      *ON           *IN89                          MCR0687
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C                   ENDIF
      *
     C     SCPCDM        IFEQ      '4'
     C     SC7A01        ANDGT     *BLANKS
     C     SCPCDM        OREQ      '5'
     C     SC7A01        ANDGT     *BLANKS
     C                   MOVEL     *BLANK        KYCPT4
     C                   MOVEL     SC7A01        KYCPT4
      *
     C     CPT4KY        CHAIN     PRC01                              60
      *
     C     *IN60         IFEQ      *ON
     C     CPT4KY        CHAIN     PPR01                              60
     C                   ENDIF
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN89                          MCR0687
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C                   ENDIF
      *
     C     SCPCDM        IFEQ      '9'
     C     SC7A01        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN89                          MCR0687
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
      *
     C     SCPCDM        IFEQ      '9'
     C     SC7A01        ANDGT     *BLANKS
     C                   MOVEL     *BLANK        DIKEY             5
     C                   MOVEL     SC7A01        DIKEY
     C                   MOVE      'O'           XXLOST
     C     DIKY          CHAIN     PDI01                              60
      *
     C                   MOVE      *OFF          *IN52                          ADD 06/06/97
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN89
     C                   MOVE      *ON           *IN52                          ADD 06/06/97
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** BEGIN ADD 06/06/97 ******************************************
     C*** CALL ICD9 EDIT PROGRAM
     C     XXFTYP        IFNE      '1'                                          ADD 10/12/98
     C                   MOVEL     SC7A01        QQICD9
     C                   MOVE      'P'           QQPRSC
     C                   MOVE      'B'           Q2CALL
     C                   MOVE      'H'           QQSYS
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
     C                   MOVE      *OFF          *IN52
     C                   MOVE      *OFF          *IN53
     C* CHECK ERROR FLAG FOR 'E' OR 'V' CODE
     C     QQERR         IFEQ      'EV'
     C     QQERR         OREQ      'E '
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN52
     C                   MOVE      *ON           *IN53
     C                   GOTO      ENDEDT
     C                   ENDIF
     C                   ENDIF                                                  ADD 10/12/98
     C*
     C*** BEGIN DEL *** 08/07/97 **************************************
     C* CHECK WHETHER ICD9 CODE HAS BEEN FLAGGED AS BWC INVALID
     C********   QQVLD     IFEQ 'I'
     C********             MOVE *ON       *IN90
     C********             MOVE *ON       *IN52
     C********             MOVE *ON       *IN53
     C********             GOTO ENDEDT
     C********             ENDIF
     C********
     C*** END   DEL *** 08/07/97 **************************************
     C*** END   ADD 06/06/97 ******************************************
     C                   ENDIF
      *
      ** VALIDATE PRINCIPAL PROCEDURE DATE (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN41
      *
     C     SCDT13        IFGT      *ZEROS
      *
      *
     C                   Z-ADD     SCDT13        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDT13
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN41                          MCR0500
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 07/22/97 **************************************
     C                   Z-ADD     SCDT13        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZDT13            7 0
     C     ZZDT13        IFLT      ZZACDT
     C                   MOVE      *ON           *IN66
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** END ADD *** 07/22/97 ****************************************
      *
     C                   ENDIF
      *
      ** VALIDATE OTHER PROCEDURE CODE 1 (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN42
      *
     C     SCPCDM        IFEQ      ' '
     C     SC7A02        ANDGT     *BLANKS
     C                   MOVE      *ON           *IN42                          MCR0687
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
      *
     C     SCPCDM        IFEQ      '0'
     C     SC7A02        ANDGT     *BLANKS
     C                   MOVE      *ON           *IN42                          MCR0687
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
      *
     C     SCPCDM        IFEQ      '5'
     C     SC7A02        ANDGT     *BLANKS
     C                   MOVEL     SC7A02        XXHCHK            1
     C     XXHCHK        IFGE      '0'
     C                   MOVE      *ON           *IN42                          MCR0687
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C                   ENDIF
      *
     C     SCPCDM        IFEQ      '4'
     C     SC7A02        ANDGT     *BLANKS
     C     SCPCDM        OREQ      '5'
     C     SC7A02        ANDGT     *BLANKS
     C                   MOVEL     *BLANK        KYCPT4
     C                   MOVEL     SC7A02        KYCPT4
      *
     C     CPT4KY        CHAIN     PRC01                              60
      *
     C     *IN60         IFEQ      *ON
     C     CPT4KY        CHAIN     PPR01                              60
     C                   ENDIF
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN42                          MCR0687
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C                   ENDIF
      *
     C     SCPCDM        IFEQ      '9'
     C     SC7A02        ANDGT     *BLANKS
     C                   MOVEL     *BLANK        DIKEY             5
     C                   MOVEL     SC7A02        DIKEY
     C                   MOVE      'O'           XXLOST
     C     DIKY          CHAIN     PDI01                              60
      *
     C                   MOVE      *OFF          *IN52                          ADD 06/06/97
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN42
     C                   MOVE      *ON           *IN52                          ADD 06/06/97
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** BEGIN ADD 06/06/97 ******************************************
     C*** CALL ICD9 EDIT PROGRAM
     C     XXFTYP        IFNE      '1'                                          ADD 10/12/98
     C                   MOVEL     SC7A02        QQICD9
     C                   MOVE      'S'           QQPRSC
     C                   MOVE      'B'           Q2CALL
     C                   MOVE      'H'           QQSYS
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
     C                   MOVE      *OFF          *IN52
     C                   MOVE      *OFF          *IN54
     C* CHECK ERROR FLAG FOR 'E' OR 'V' CODE
     C     QQERR         IFEQ      'EV'
     C     QQERR         OREQ      'E '
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN52
     C                   MOVE      *ON           *IN54
     C                   GOTO      ENDEDT
     C                   ENDIF
     C                   ENDIF                                                  ADD 10/12/98
     C*
     C*** BEGIN DEL *** 08/07/97 **************************************
     C* CHECK WHETHER ICD9 CODE HAS BEEN FLAGGED AS BWC INVALID
     C********   QQVLD     IFEQ 'I'
     C********             MOVE *ON       *IN90
     C********             MOVE *ON       *IN52
     C********             MOVE *ON       *IN54
     C********             GOTO ENDEDT
     C********             ENDIF
     C********
     C*** END   DEL *** 08/07/97 **************************************
     C*** END   ADD 06/06/97 ******************************************
     C                   ENDIF
      *
      ** VALIDATE OTHER PROCEDURE DATE 1 (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN43
      *
     C     SCDT14        IFGT      *ZEROS
      *
      *
     C                   Z-ADD     SCDT14        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDT14
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN43                          MCR0500
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** BEGIN ADD *** 07/22/97 **************************************
     C                   Z-ADD     SCDT14        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZDT14            7 0
     C     ZZDT14        IFLT      ZZACDT
     C                   MOVE      *ON           *IN67
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** END ADD *** 07/22/97 ****************************************
      *
     C                   ENDIF
      *
      ** VALIDATE OTHER PROCEDURE CODE 2 (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN44
     C                   MOVE      *OFF          *IN39
      *
     C     SCPCDM        IFEQ      ' '
     C     SC7A03        ANDGT     *BLANKS
     C                   MOVE      *ON           *IN44                          MCR0687
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
      *
     C     SCPCDM        IFEQ      '0'
     C     SC7A03        ANDGT     *BLANKS
     C                   MOVE      *ON           *IN44                          MCR0687
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
      *
     C     SCPCDM        IFEQ      '5'
     C     SC7A03        ANDGT     *BLANKS
     C                   MOVEL     SC7A03        XXHCHK            1
     C     XXHCHK        IFGE      '0'
     C                   MOVE      *ON           *IN44                          MCR0687
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C                   ENDIF
      *
     C     SCPCDM        IFEQ      '4'
     C     SC7A03        ANDGT     *BLANKS
     C     SCPCDM        OREQ      '5'
     C     SC7A03        ANDGT     *BLANKS
     C                   MOVEL     *BLANK        KYCPT4
     C                   MOVEL     SC7A03        KYCPT4
      *
     C     CPT4KY        CHAIN     PRC01                              60
      *
     C     *IN60         IFEQ      *ON
     C     CPT4KY        CHAIN     PPR01                              60
     C                   ENDIF
      *
     C                   MOVE      *ON           *IN44                          MCR0687
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN44                          MCR0687
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C                   ENDIF
      *
     C     SCPCDM        IFEQ      '9'
     C     SC7A03        ANDGT     *BLANKS
     C                   MOVEL     *BLANK        DIKEY             5
     C                   MOVEL     SC7A03        DIKEY
     C                   MOVE      'O'           XXLOST
     C     DIKY          CHAIN     PDI01                              60
      *
     C                   MOVE      *OFF          *IN52                          ADD 06/06/97
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN44
     C                   MOVE      *ON           *IN52                          ADD 06/06/97
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** BEGIN ADD 06/06/97 ******************************************
     C*** CALL ICD9 EDIT PROGRAM
     C     XXFTYP        IFNE      '1'                                          ADD 10/12/98
     C                   MOVEL     SC7A03        QQICD9
     C                   MOVE      'S'           QQPRSC
     C                   MOVE      'B'           Q2CALL
     C                   MOVE      'H'           QQSYS
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
     C                   MOVE      *OFF          *IN52
     C                   MOVE      *OFF          *IN55
     C* CHECK ERROR FLAG FOR 'E' OR 'V' CODE
     C     QQERR         IFEQ      'EV'
     C     QQERR         OREQ      'E '
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN52
     C                   MOVE      *ON           *IN55
     C                   GOTO      ENDEDT
     C                   ENDIF
     C                   ENDIF                                                  ADD 10/12/98
     C*
     C*** BEGIN DEL *** 08/07/97 **************************************
     C* CHECK WHETHER ICD9 CODE HAS BEEN FLAGGED AS BWC INVALID
     C********   QQVLD     IFEQ 'I'
     C********             MOVE *ON       *IN90
     C********             MOVE *ON       *IN52
     C********             MOVE *ON       *IN55
     C********             GOTO ENDEDT
     C********             ENDIF
     C********
     C*** END   DEL *** 08/07/97 **************************************
     C*** END   ADD 06/06/97 ******************************************
     C                   ENDIF
      *
      ** VALIDATE OTHER PROCEDURE DATE 2 (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN45
      *
     C     SCDT15        IFGT      *ZEROS
      *
      *
     C                   Z-ADD     SCDT15        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDT15
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN45                          MCR0500
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** BEGIN ADD *** 07/22/97 **************************************
     C                   Z-ADD     SCDT15        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZDT15            7 0
     C     ZZDT15        IFLT      ZZACDT
     C                   MOVE      *ON           *IN68
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** END ADD *** 07/22/97 ****************************************
      *
     C                   ENDIF
      *
      ** VALIDATE OTHER PROCEDURE CODE 3 (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN46
      *
     C     SCPCDM        IFEQ      ' '
     C     SC7A04        ANDGT     *BLANKS
     C                   MOVE      *ON           *IN46                          MCR0687
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
      *
     C     SCPCDM        IFEQ      '0'
     C     SC7A04        ANDGT     *BLANKS
     C                   MOVE      *ON           *IN46                          MCR0687
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
      *
     C     SCPCDM        IFEQ      '5'
     C     SC7A04        ANDGT     *BLANKS
     C                   MOVEL     SC7A04        XXHCHK            1
     C     XXHCHK        IFGE      '0'
     C                   MOVE      *ON           *IN46                          MCR0687
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C                   ENDIF
      *
     C     SCPCDM        IFEQ      '4'
     C     SC7A04        ANDGT     *BLANKS
     C     SCPCDM        OREQ      '5'
     C     SC7A04        ANDGT     *BLANKS
     C                   MOVEL     *BLANK        KYCPT4
     C                   MOVEL     SC7A04        KYCPT4
      *
     C     CPT4KY        CHAIN     PRC01                              60
      *
     C     *IN60         IFEQ      *ON
     C     CPT4KY        CHAIN     PPR01                              60
     C                   ENDIF
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN46                          MCR0687
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C                   ENDIF
      *
     C     SCPCDM        IFEQ      '9'
     C     SC7A04        ANDGT     *BLANKS
     C                   MOVEL     *BLANK        DIKEY             5
     C                   MOVEL     SC7A04        DIKEY
     C                   MOVE      'O'           XXLOST
     C     DIKY          CHAIN     PDI01                              60
      *
     C                   MOVE      *OFF          *IN52                          ADD 06/06/97
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN46                          MCR0687
     C                   MOVE      *ON           *IN52                          ADD 06/06/97
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** BEGIN ADD 06/06/97 ******************************************
     C*** CALL ICD9 EDIT PROGRAM
     C     XXFTYP        IFNE      '1'                                          ADD 10/12/98
     C                   MOVEL     SC7A04        QQICD9
     C                   MOVE      'S'           QQPRSC
     C                   MOVE      'B'           Q2CALL
     C                   MOVE      'H'           QQSYS
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
     C                   MOVE      *OFF          *IN52
     C                   MOVE      *OFF          *IN56
     C* CHECK ERROR FLAG FOR 'E' OR 'V' CODE
     C     QQERR         IFEQ      'EV'
     C     QQERR         OREQ      'E '
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN52
     C                   MOVE      *ON           *IN56
     C                   GOTO      ENDEDT
     C                   ENDIF
     C                   ENDIF                                                  ADD 10/12/98
     C*
     C*** BEGIN DEL *** 08/07/97 **************************************
     C* CHECK WHETHER ICD9 CODE HAS BEEN FLAGGED AS BWC INVALID
     C********   QQVLD     IFEQ 'I'
     C********             MOVE *ON       *IN90
     C********             MOVE *ON       *IN52
     C********             MOVE *ON       *IN56
     C********             GOTO ENDEDT
     C********             ENDIF
     C********
     C*** END   DEL *** 08/07/97 **************************************
     C*** END   ADD 06/06/97 ******************************************
     C                   ENDIF
      *
      ** VALIDATE OTHER PROCEDURE DATE 3 (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN47
      *
     C     SCDT16        IFGT      *ZEROS
      *
      *
     C                   Z-ADD     SCDT16        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDT16
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN47                          MCR0500
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** BEGIN ADD *** 07/22/97 **************************************
     C                   Z-ADD     SCDT16        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZDT16            7 0
     C     ZZDT16        IFLT      ZZACDT
     C                   MOVE      *ON           *IN69
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** END ADD *** 07/22/97 ****************************************
      *
     C                   ENDIF
      *
      ** VALIDATE OTHER PROCEDURE CODE 4 (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN48
      *
     C     SCPCDM        IFEQ      ' '
     C     SC7A05        ANDGT     *BLANKS
     C                   MOVE      *ON           *IN48                          MCR0687
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
      *
     C     SCPCDM        IFEQ      '0'
     C     SC7A05        ANDGT     *BLANKS
     C                   MOVE      *ON           *IN48                          MCR0687
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
      *
     C     SCPCDM        IFEQ      '5'
     C     SC7A05        ANDGT     *BLANKS
     C                   MOVEL     SC7A05        XXHCHK            1
     C     XXHCHK        IFGE      '0'
     C                   MOVE      *ON           *IN48                          MCR0687
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C                   ENDIF
      *
     C     SCPCDM        IFEQ      '4'
     C     SC7A05        ANDGT     *BLANKS
     C     SCPCDM        OREQ      '5'
     C     SC7A05        ANDGT     *BLANKS
     C                   MOVEL     *BLANK        KYCPT4
     C                   MOVEL     SC7A05        KYCPT4
      *
     C     CPT4KY        CHAIN     PRC01                              60
      *
     C     *IN60         IFEQ      *ON
     C     CPT4KY        CHAIN     PPR01                              60
     C                   ENDIF
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN48                          MCR0687
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C                   ENDIF
      *
     C     SCPCDM        IFEQ      '9'
     C     SC7A05        ANDGT     *BLANKS
     C                   MOVEL     *BLANK        DIKEY             5
     C                   MOVEL     SC7A05        DIKEY
     C                   MOVE      'O'           XXLOST
     C     DIKY          CHAIN     PDI01                              60
      *
     C                   MOVE      *OFF          *IN52                          ADD 06/06/97
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN48                          MCR0687
     C                   MOVE      *ON           *IN52                          ADD 06/06/97
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** BEGIN ADD 06/06/97 ******************************************
     C*** CALL ICD9 EDIT PROGRAM
     C     XXFTYP        IFNE      '1'                                          ADD 10/12/98
     C                   MOVEL     SC7A05        QQICD9
     C                   MOVE      'S'           QQPRSC
     C                   MOVE      'B'           Q2CALL
     C                   MOVE      'H'           QQSYS
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
     C                   MOVE      *OFF          *IN52
     C                   MOVE      *OFF          *IN57
     C* CHECK ERROR FLAG FOR 'E' OR 'V' CODE
     C     QQERR         IFEQ      'EV'
     C     QQERR         OREQ      'E '
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN52
     C                   MOVE      *ON           *IN57
     C                   GOTO      ENDEDT
     C                   ENDIF
     C                   ENDIF                                                  ADD 10/12/98
     C*
     C*** BEGIN DEL *** 08/07/97 **************************************
     C* CHECK WHETHER ICD9 CODE HAS BEEN FLAGGED AS BWC INVALID
     C********   QQVLD     IFEQ 'I'
     C********             MOVE *ON       *IN90
     C********             MOVE *ON       *IN52
     C********             MOVE *ON       *IN57
     C********             GOTO ENDEDT
     C********             ENDIF
     C********
     C*** END   DEL *** 08/07/97 **************************************
     C*** END   ADD 06/06/97 ******************************************
     C                   ENDIF
      *
      ** VALIDATE OTHER PROCEDURE DATE 4 (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN49
      *
     C     SCDT17        IFGT      *ZEROS
      *
      *
     C                   Z-ADD     SCDT17        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDT17
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN49                          MCR0500
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** BEGIN ADD *** 07/22/97 **************************************
     C                   Z-ADD     SCDT17        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZDT17            7 0
     C     ZZDT17        IFLT      ZZACDT
     C                   MOVE      *ON           *IN26
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** END ADD *** 07/22/97 ****************************************
      *
     C                   ENDIF
      *
      ** VALIDATE OTHER PROCEDURE CODE 5 (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN50
      *
     C     SCPCDM        IFEQ      ' '
     C     SC7A06        ANDGT     *BLANKS
     C                   MOVE      *ON           *IN50                          MCR0687
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
      *
     C     SCPCDM        IFEQ      '0'
     C     SC7A06        ANDGT     *BLANKS
     C                   MOVE      *ON           *IN50                          MCR0687
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
      *
     C     SCPCDM        IFEQ      '5'
     C     SC7A06        ANDGT     *BLANKS
     C                   MOVEL     SC7A06        XXHCHK            1
     C     XXHCHK        IFGE      '0'
     C                   MOVE      *ON           *IN50                          MCR0687
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C                   ENDIF
      *
     C     SCPCDM        IFEQ      '4'
     C     SC7A06        ANDGT     *BLANKS
     C     SCPCDM        OREQ      '5'
     C     SC7A06        ANDGT     *BLANKS
     C                   MOVEL     *BLANK        KYCPT4
     C                   MOVEL     SC7A06        KYCPT4
      *
     C     CPT4KY        CHAIN     PRC01                              60
      *
     C     *IN60         IFEQ      *ON
     C     CPT4KY        CHAIN     PPR01                              60
     C                   ENDIF
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN50                          MCR0687
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C                   ENDIF
      *
     C     SCPCDM        IFEQ      '9'
     C     SC7A06        ANDGT     *BLANKS
     C                   MOVEL     *BLANK        DIKEY             5
     C                   MOVEL     SC7A06        DIKEY
     C                   MOVE      'O'           XXLOST
     C     DIKY          CHAIN     PDI01                              60
      *
     C                   MOVE      *OFF          *IN52                          ADD 06/06/97
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN50                          MCR0687
     C                   MOVE      *ON           *IN52                          ADD 06/06/97
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** BEGIN ADD 06/06/97 ******************************************
     C*** CALL ICD9 EDIT PROGRAM
     C     XXFTYP        IFNE      '1'                                          ADD 10/12/98
     C                   MOVEL     SC7A06        QQICD9
     C                   MOVE      'S'           QQPRSC
     C                   MOVE      'B'           Q2CALL
     C                   MOVE      'H'           QQSYS
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
     C                   MOVE      *OFF          *IN52
     C                   MOVE      *OFF          *IN58
     C* CHECK ERROR FLAG FOR 'E' OR 'V' CODE
     C     QQERR         IFEQ      'EV'
     C     QQERR         OREQ      'E '
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN52
     C                   MOVE      *ON           *IN58
     C                   GOTO      ENDEDT
     C                   ENDIF
     C                   ENDIF                                                  ADD 10/12/98
     C*
     C*** BEGIN DEL *** 08/07/97 **************************************
     C* CHECK WHETHER ICD9 CODE HAS BEEN FLAGGED AS BWC INVALID
     C********   QQVLD     IFEQ 'I'
     C********             MOVE *ON       *IN90
     C********             MOVE *ON       *IN52
     C********             MOVE *ON       *IN58
     C********             GOTO ENDEDT
     C********             ENDIF
     C********
     C*** END   DEL *** 08/07/97 **************************************
     C*** END   ADD 06/06/97 ******************************************
     C                   ENDIF
      *
      ** VALIDATE OTHER PROCEDURE DATE 5 (OPTIONAL)
      *
     C                   MOVE      *OFF          *IN51
      *
     C     SCDT18        IFGT      *ZEROS
      *
      *
     C                   Z-ADD     SCDT18        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDT18
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN51                          MCR0500
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** BEGIN ADD *** 07/22/97 **************************************
     C                   Z-ADD     SCDT18        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZDT18            7 0
     C     ZZDT18        IFLT      ZZACDT
     C                   MOVE      *ON           *IN25
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*** END ADD *** 07/22/97 ****************************************
      *
     C                   ENDIF
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
     C                   MOVE      *ON           *IN70
      *
     C     *INKJ         IFEQ      *ON
      *
     C*** BEGIN DEL *** 04/24/97 **************************************
     C** SEARCH FOR TYPE OF BILL
     C**********
     C********** HFLD      IFEQ 'SCBTYP'
     C**********
     C**********           MOVE 'BLTY'    QQID
     C**********           MOVE *BLANKS   QQKEY
     C**********           MOVE *BLANKS   QQRTCD
     C**********           CALL 'F2493R'
     C**********           PARM           QQPRDS 40
     C**********           PARM           QQCALL  1
     C**********           PARM           QQID
     C**********           PARM           QQKEY
     C**********           PARM           QQRTCD
     C**********
     C********** QQRTCD    IFEQ 'A'
     C**********           MOVE *ON       *INKC
     C**********           GOTO TERM
     C**********           ENDIF
     C**********
     C********** QQRTCD    IFEQ 'B'
     C**********           Z-ADD10        SCLI92
     C**********           Z-ADD21        SCPS92
     C**********           ENDIF
     C**********
     C********** QQRTCD    IFEQ '*'
     C********** QQKEY     ANDEQ*BLANKS
     C********** QQRTCD    OREQ ' '
     C**********           Z-ADD10        SCLI92
     C**********           Z-ADD21        SCPS92
     C**********           ELSE
     C**********           Z-ADD11        SCLI92
     C**********           Z-ADD21        SCPS92
     C**********           ENDIF
     C**********
     C**********           MOVELQQKEY     SCBTYP
     C**********           MOVE *ON       *IN90
     C**********           MOVE *OFF      *IN70
     C**********           GOTO TERM
     C**********
     C**********           ENDIF                           END BILL TYPE
     C*** END DEL *** 04/24/97 ****************************************
      *
      ** SEARCH FOR ADMISSION TYPE
      *
     C     HFLD          IFEQ      'SCADTY'
      *
     C                   MOVE      'ADTY'        QQID
     C                   MOVE      *BLANKS       QQKEY
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS
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
     C                   Z-ADD     13            SCLI92
     C                   Z-ADD     58            SCPS92
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      '*'
     C     QQKEY         ANDEQ     *BLANKS
     C     QQRTCD        OREQ      ' '
     C                   Z-ADD     13            SCLI92
     C                   Z-ADD     58            SCPS92
     C                   ELSE
     C                   Z-ADD     13            SCLI92
     C                   Z-ADD     69            SCPS92
     C                   ENDIF
      *
     C                   MOVEL     QQKEY         SCADTY
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END ADMIS TYPE
      *
      ** SEARCH FOR ADMISSION SOURCE
      *
     C     HFLD          IFEQ      'SCADSC'
      *
     C                   MOVE      'ADSC'        QQID
     C                   MOVE      *BLANKS       QQKEY
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
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
     C                   Z-ADD     13            SCLI92
     C                   Z-ADD     69            SCPS92
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      '*'
     C     QQKEY         ANDEQ     *BLANKS
     C     QQRTCD        OREQ      ' '
     C                   Z-ADD     13            SCLI92
     C                   Z-ADD     69            SCPS92
     C                   ELSE
     C                   Z-ADD     14            SCLI92
     C                   Z-ADD     21            SCPS92
     C                   ENDIF
      *
     C                   MOVEL     QQKEY         SCADSC
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END ADMIS SOURCE
      *
      ** SEARCH FOR PATIENT STATUS
      *
     C     HFLD          IFEQ      'SCPATS'
      *
     C                   MOVE      'PATS'        QQID
     C                   MOVE      *BLANKS       QQKEY
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
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
     C                   Z-ADD     14            SCLI92
     C                   Z-ADD     58            SCPS92
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      '*'
     C     QQKEY         ANDEQ     *BLANKS
     C     QQRTCD        OREQ      ' '
     C                   Z-ADD     14            SCLI92
     C                   Z-ADD     58            SCPS92
     C                   ELSE
     C                   Z-ADD     15            SCLI92
     C                   Z-ADD     21            SCPS92
     C                   ENDIF
      *
     C                   MOVEL     QQKEY         SCPATS
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END PATIENT STAT
      *
      ** SEARCH FOR OCC CODE
      *
     C     HFLD          IFEQ      'SCOCD1'
      *
     C                   MOVE      'OCCD'        QQID
     C                   MOVE      *BLANKS       QQKEY
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
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
     C                   Z-ADD     15            SCLI92
     C                   Z-ADD     21            SCPS92
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      '*'
     C********   QQKEY     ANDEQ*BLANKS
     C     QQRTCD        OREQ      ' '
     C                   Z-ADD     15            SCLI92
     C                   Z-ADD     58            SCPS92
     C                   ELSE
     C                   Z-ADD     15            SCLI92
     C                   Z-ADD     21            SCPS92
     C                   ENDIF
      *
     C                   MOVEL     QQKEY         SCOCD1
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END OCC CODE
      *
      ** SEARCH FOR TYPE OF SERVICE
      *
     C     HFLD          IFEQ      'SCHTOS'
      *
     C                   MOVE      'SVTY'        QQID
     C                   MOVE      *BLANKS       QQKEY
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
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
     C                   Z-ADD     17            SCLI92
     C                   Z-ADD     21            SCPS92
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      '*'
     C     QQKEY         ANDEQ     *BLANKS
     C     QQRTCD        OREQ      ' '
     C                   Z-ADD     17            SCLI92
     C                   Z-ADD     21            SCPS92
     C                   ELSE
     C                   Z-ADD     17            SCLI92
     C                   Z-ADD     58            SCPS92
     C                   ENDIF
      *
     C                   MOVEL     QQKEY         SCHTOS
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END TYPE OF SERV
      *
      ** SEARCH FOR TYPE OF BILL
      *
     C     HFLD          IFEQ      'SCTOB'
      *
     C                   MOVE      'BLST'        QQID
     C                   MOVE      *BLANKS       QQKEY
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
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
     C                   Z-ADD     17            SCLI92
     C                   Z-ADD     58            SCPS92
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      '*'
     C     QQKEY         ANDEQ     *BLANKS
     C     QQRTCD        OREQ      ' '
     C                   Z-ADD     17            SCLI92
     C                   Z-ADD     58            SCPS92
     C                   ELSE
     C                   Z-ADD     18            SCLI92
     C                   Z-ADD     21            SCPS92
     C                   ENDIF
      *
     C                   MOVEL     QQKEY         SCTOB
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END TYPE OF SERV
      *
      ** SEARCH FOR VALUE CODE
      *
     C     HFLD          IFEQ      'SCVC01'
      *
     C                   MOVE      'VLCD'        QQID
     C                   MOVE      *BLANKS       QQKEY
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
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
     C                   Z-ADD     18            SCLI92
     C                   Z-ADD     21            SCPS92
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      '*'
     C     QQKEY         ANDEQ     *BLANKS
     C     QQRTCD        OREQ      ' '
     C                   Z-ADD     18            SCLI92
     C                   Z-ADD     21            SCPS92
     C                   ELSE
     C                   Z-ADD     18            SCLI92
     C                   Z-ADD     58            SCPS92
     C                   ENDIF
      *
     C                   MOVEL     QQKEY         SCVC01
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END VALUE CODE
      *
      ** SEARCH FOR ADMITTING DIAGNOSTIC CODE
      *
     C     HFLD          IFEQ      'SCADDI'
      *
     C*** BEGIN DEL *** 05/06/97 **************************************
     C**********           MOVE *BLANK    QQDIAG  5
     C**********           CALL 'F0510R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQDIAG
     C**********           PARM           QQRTCD  1
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
     C                   PARM                    QQCALL            1
     C                   PARM                    QQKC              1
     C                   PARM                    QQKL              1
     C                   PARM                    QQDIAG            5
     C                   PARM                    QQLOST            1
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
     C                   Z-ADD     20            SCLI92
     C                   Z-ADD     72            SCPS92
     C                   ENDIF
      *
     C********** QQRTCD    IFEQ '*'
     C********** QQKEY     ANDEQ*BLANKS
     C     QQDIAG        IFNE      *BLANKS                                      ADD 05/06/97
     C                   Z-ADD     20            SCLI92
     C                   Z-ADD     72            SCPS92
     C                   ELSE
     C                   Z-ADD     21            SCLI92
     C                   Z-ADD     22            SCPS92
     C                   ENDIF
     C*** END CHG *** 05/06/97 ****************************************
      *
     C                   MOVEL     QQDIAG        SCADDI
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END DIAG CODE
      *
      ** SEARCH FOR PROCEDURE CODE METHOD
      *
     C     HFLD          IFEQ      'SCPCDM'
      *
     C                   MOVE      'PCMT'        QQID
     C                   MOVE      *BLANKS       QQKEY
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
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
     C                   Z-ADD     21            SCLI92
     C                   Z-ADD     22            SCPS92
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      '*'
     C     QQKEY         ANDEQ     *BLANKS
     C     QQRTCD        OREQ      ' '
     C                   Z-ADD     21            SCLI92
     C                   Z-ADD     22            SCPS92
     C                   ELSE
     C                   Z-ADD     21            SCLI92
     C                   Z-ADD     50            SCPS92
     C                   ENDIF
      *
     C                   MOVEL     QQKEY         SCPCDM
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END VALUE CODE
      *
      ** SEARCH FOR PRINCIPLE PROCEDURE
      *
      ** PROCEDURE CODE METHOD WAS 4 SEARCH FOR CPT4 CODES
      *
     C     HFLD          IFEQ      'SC7A01'
      *
     C     SCPCDM        IFEQ      '4'
      *
     C                   MOVE      *BLANKS       QQCPT4            5
     C******************** MOVE *ZEROS    QQFDAT  60       CHG 03/10/98
     C                   MOVE      *ZEROS        QQFDAT            8 0          CHG 03/10/98
     C                   CALL      'F0520R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCPT4
     C                   PARM                    QQFDAT
     C                   PARM                    QQAST             2
     C                   PARM                    QQRTCD
     C                   MOVE      *BLANKS       SC7A01
     C                   MOVEL     QQCPT4        SC7A01
     C                   ENDIF                                                  END VALUE CODE
      *
      ** PROCEDURE CODE METHOD WAS 5 SEARCH FOR HCPCS CODES
      *
     C     SCPCDM        IFEQ      '5'
      *
     C                   MOVE      *BLANKS       QQCPT4
     C                   MOVE      *ZEROS        QQFDAT
     C                   CALL      'F0520R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCPT4
     C                   PARM                    QQFDAT
     C                   PARM                    QQAST
     C                   PARM                    QQRTCD
     C                   MOVE      *BLANKS       SC7A01
     C                   MOVEL     QQCPT4        SC7A01
     C                   ENDIF                                                  END VALUE CODE
      *
      ** PROCEDURE CODE METHOD WAS 9 SEARCH FOR ICD9 CODES
      *
     C     SCPCDM        IFEQ      '9'
      *
     C*** BEGIN DEL *** 05/06/97 **************************************
     C**********           MOVE *BLANKS   QQCPT4
     C**********           CALL 'F0510R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQCPT4
     C**********           PARM           QQRTCD
     C*** END DEL *** 05/06/97 ****************************************
     C*
     C*** BEGIN ADD *** 05/06/97 **************************************
     C                   MOVE      '2'           QQCALL
     C                   MOVE      *BLANKS       QQKC
     C                   MOVE      *BLANKS       QQKL
     C                   MOVE      *BLANKS       QQCPT4
     C                   MOVE      *BLANKS       QQLOST
     C*
     C                   CALL      'F2010R'
     C                   PARM                    QQCALL
     C                   PARM                    QQKC
     C                   PARM                    QQKL
     C                   PARM                    QQCPT4
     C                   PARM                    QQLOST
     C     QQKC          IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   ENDIF
     C     QQKL          IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   ENDIF
     C     QQCPT4        IFEQ      *BLANKS
     C                   MOVE      '*'           QQRTCD
     C                   ENDIF
     C*** END ADD *** 05/06/97 ****************************************
     C                   MOVE      *BLANKS       SC7A01
     C                   MOVEL     QQCPT4        SC7A01
     C                   ENDIF                                                  END VALUE CODE
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   GOTO      TERM
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      'B'
     C                   Z-ADD     21            SCLI92
     C                   Z-ADD     50            SCPS92
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      '*'
     C     QQCPT4        ANDEQ     *BLANKS
     C                   Z-ADD     21            SCLI92
     C                   Z-ADD     50            SCPS92
     C                   ELSE
     C                   Z-ADD     21            SCLI92
     C                   Z-ADD     67            SCPS92
     C                   ENDIF
      *
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END PRIN PROC
      *
      ** SEARCH FOR OTHER PROCEDURE 1
      *
      ** PROCEDURE CODE METHOD WAS 4 SEARCH FOR CPT4 CODES
      *
     C     HFLD          IFEQ      'SC7A02'
      *
     C     SCPCDM        IFEQ      '4'
      *
     C                   MOVE      *BLANKS       QQCPT4
     C                   MOVE      *ZEROS        QQFDAT
     C                   CALL      'F0520R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCPT4
     C                   PARM                    QQFDAT
     C                   PARM                    QQAST
     C                   PARM                    QQRTCD
     C                   MOVE      *BLANKS       SC7A02
     C                   MOVEL     QQCPT4        SC7A02
     C                   ENDIF                                                  END VALUE CODE
      *
      ** PROCEDURE CODE METHOD WAS 5 SEARCH FOR HCPCS CODES
      *
     C     SCPCDM        IFEQ      '5'
      *
     C                   MOVE      *BLANKS       QQCPT4
     C                   MOVE      *ZEROS        QQFDAT
     C                   CALL      'F0520R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCPT4
     C                   PARM                    QQFDAT
     C                   PARM                    QQAST
     C                   PARM                    QQRTCD
     C                   MOVE      *BLANKS       SC7A02
     C                   MOVEL     QQCPT4        SC7A02
     C                   ENDIF                                                  END VALUE CODE
      *
      ** PROCEDURE CODE METHOD WAS 9 SEARCH FOR ICD9 CODES
      *
     C     SCPCDM        IFEQ      '9'
      *
     C*** BEGIN DEL *** 05/06/97 **************************************
     C**********           MOVE *BLANKS   QQCPT4
     C**********           CALL 'F0510R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQCPT4
     C**********           PARM           QQRTCD
     C*** END DEL *** 05/06/97 ****************************************
     C*** BEGIN ADD *** 05/06/97 **************************************
     C                   MOVE      '2'           QQCALL
     C                   MOVE      *BLANKS       QQKC
     C                   MOVE      *BLANKS       QQKL
     C                   MOVE      *BLANKS       QQCPT4
     C                   MOVE      *BLANKS       QQLOST
     C*
     C                   CALL      'F2010R'
     C                   PARM                    QQCALL
     C                   PARM                    QQKC
     C                   PARM                    QQKL
     C                   PARM                    QQCPT4
     C                   PARM                    QQLOST
     C     QQKC          IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   ENDIF
     C     QQKL          IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   ENDIF
     C     QQCPT4        IFEQ      *BLANKS
     C                   MOVE      '*'           QQRTCD
     C                   ENDIF
     C*** END ADD *** 05/06/97 ****************************************
     C                   MOVE      *BLANKS       SC7A02
     C                   MOVEL     QQCPT4        SC7A02
     C                   ENDIF                                                  END VALUE CODE
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   GOTO      TERM
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      'B'
     C                   Z-ADD     22            SCLI92
     C                   Z-ADD     22            SCPS92
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      '*'
     C     QQCPT4        ANDEQ     *BLANKS
     C                   Z-ADD     22            SCLI92
     C                   Z-ADD     22            SCPS92
     C                   ELSE
     C                   Z-ADD     22            SCLI92
     C                   Z-ADD     30            SCPS92
     C                   ENDIF
      *
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END OTHR PROC 1
      *
      ** SEARCH FOR OTHER PROCEDURE 2
      *
      ** PROCEDURE CODE METHOD WAS 4 SEARCH FOR CPT4 CODES
      *
     C     HFLD          IFEQ      'SC7A03'
      *
     C     SCPCDM        IFEQ      '4'
      *
     C                   MOVE      *BLANKS       QQCPT4
     C                   MOVE      *ZEROS        QQFDAT
     C                   CALL      'F0520R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCPT4
     C                   PARM                    QQFDAT
     C                   PARM                    QQAST
     C                   PARM                    QQRTCD
     C                   MOVE      *BLANKS       SC7A03
     C                   MOVEL     QQCPT4        SC7A03
     C                   ENDIF                                                  END VALUE CODE
      *
      ** PROCEDURE CODE METHOD WAS 5 SEARCH FOR HCPCS CODES
      *
     C     SCPCDM        IFEQ      '5'
      *
     C                   MOVE      *BLANKS       QQCPT4
     C                   MOVE      *ZEROS        QQFDAT
     C                   CALL      'F0520R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCPT4
     C                   PARM                    QQFDAT
     C                   PARM                    QQAST
     C                   PARM                    QQRTCD
     C                   MOVE      *BLANKS       SC7A03
     C                   MOVEL     QQCPT4        SC7A03
     C                   ENDIF                                                  END VALUE CODE
      *
      ** PROCEDURE CODE METHOD WAS 9 SEARCH FOR ICD9 CODES
      *
     C     SCPCDM        IFEQ      '9'
      *
     C*** BEGIN DEL *** 05/06/97 **************************************
     C**********           MOVE *BLANKS   QQCPT4
     C**********           CALL 'F0510R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQCPT4
     C**********           PARM           QQRTCD
     C*** END DEL *** 05/06/97 ****************************************
     C*** BEGIN ADD *** 05/06/97 **************************************
     C                   MOVE      '2'           QQCALL
     C                   MOVE      *BLANKS       QQKC
     C                   MOVE      *BLANKS       QQKL
     C                   MOVE      *BLANKS       QQCPT4
     C                   MOVE      *BLANKS       QQLOST
     C*
     C                   CALL      'F2010R'
     C                   PARM                    QQCALL
     C                   PARM                    QQKC
     C                   PARM                    QQKL
     C                   PARM                    QQCPT4
     C                   PARM                    QQLOST
     C     QQKC          IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   ENDIF
     C     QQKL          IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   ENDIF
     C     QQCPT4        IFEQ      *BLANKS
     C                   MOVE      '*'           QQRTCD
     C                   ENDIF
     C*** END ADD *** 05/06/97 ****************************************
     C                   MOVE      *BLANKS       SC7A03
     C                   MOVEL     QQCPT4        SC7A03
     C                   ENDIF                                                  END VALUE CODE
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   GOTO      TERM
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      'B'
     C                   Z-ADD     22            SCLI92
     C                   Z-ADD     40            SCPS92
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      '*'
     C     QQCPT4        ANDEQ     *BLANKS
     C                   Z-ADD     22            SCLI92
     C                   Z-ADD     40            SCPS92
     C                   ELSE
     C                   Z-ADD     22            SCLI92
     C                   Z-ADD     48            SCPS92
     C                   ENDIF
      *
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END OTHR PROC 3
      *
      ** SEARCH FOR OTHER PROCEDURE 3
      *
      ** PROCEDURE CODE METHOD WAS 4 SEARCH FOR CPT4 CODES
      *
     C     HFLD          IFEQ      'SC7A04'
      *
     C     SCPCDM        IFEQ      '4'
      *
     C                   MOVE      *BLANKS       QQCPT4
     C                   MOVE      *ZEROS        QQFDAT
     C                   CALL      'F0520R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCPT4
     C                   PARM                    QQFDAT
     C                   PARM                    QQAST
     C                   PARM                    QQRTCD
     C                   MOVE      *BLANKS       SC7A04
     C                   MOVEL     QQCPT4        SC7A04
     C                   ENDIF                                                  END VALUE CODE
      *
      ** PROCEDURE CODE METHOD WAS 5 SEARCH FOR HCPCS CODES
      *
     C     SCPCDM        IFEQ      '5'
      *
     C                   MOVE      *BLANKS       QQCPT4
     C                   MOVE      *ZEROS        QQFDAT
     C                   CALL      'F0520R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCPT4
     C                   PARM                    QQFDAT
     C                   PARM                    QQAST
     C                   PARM                    QQRTCD
     C                   MOVE      *BLANKS       SC7A04
     C                   MOVEL     QQCPT4        SC7A04
     C                   ENDIF                                                  END VALUE CODE
      *
      ** PROCEDURE CODE METHOD WAS 9 SEARCH FOR ICD9 CODES
      *
     C     SCPCDM        IFEQ      '9'
      *
     C*** BEGIN DEL *** 05/06/97 **************************************
     C**********           MOVE *BLANKS   QQCPT4
     C**********           CALL 'F0510R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQCPT4
     C**********           PARM           QQRTCD
     C*** END DEL *** 05/06/97 ****************************************
     C*
     C*** BEGIN ADD *** 05/06/97 **************************************
     C                   MOVE      '2'           QQCALL
     C                   MOVE      *BLANKS       QQKC
     C                   MOVE      *BLANKS       QQKL
     C                   MOVE      *BLANKS       QQCPT4
     C                   MOVE      *BLANKS       QQLOST
     C*
     C                   CALL      'F2010R'
     C                   PARM                    QQCALL
     C                   PARM                    QQKC
     C                   PARM                    QQKL
     C                   PARM                    QQCPT4
     C                   PARM                    QQLOST
     C     QQKC          IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   ENDIF
     C     QQKL          IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   ENDIF
     C     QQCPT4        IFEQ      *BLANKS
     C                   MOVE      '*'           QQRTCD
     C                   ENDIF
     C*** END ADD *** 05/06/97 ****************************************
     C                   MOVE      *BLANKS       SC7A04
     C                   MOVEL     QQCPT4        SC7A04
     C                   ENDIF                                                  END VALUE CODE
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   GOTO      TERM
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      'B'
     C                   Z-ADD     22            SCLI92
     C                   Z-ADD     59            SCPS92
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      '*'
     C     QQCPT4        ANDEQ     *BLANKS
     C                   Z-ADD     22            SCLI92
     C                   Z-ADD     59            SCPS92
     C                   ELSE
     C                   Z-ADD     22            SCLI92
     C                   Z-ADD     67            SCPS92
     C                   ENDIF
      *
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END OTHR PROC 4
      *
      ** SEARCH FOR OTHER PROCEDURE 4
      *
      ** PROCEDURE CODE METHOD WAS 4 SEARCH FOR CPT4 CODES
      *
     C     HFLD          IFEQ      'SC7A05'
      *
     C     SCPCDM        IFEQ      '4'
      *
     C                   MOVE      *BLANKS       QQCPT4
     C                   MOVE      *ZEROS        QQFDAT
     C                   CALL      'F0520R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCPT4
     C                   PARM                    QQFDAT
     C                   PARM                    QQAST
     C                   PARM                    QQRTCD
     C                   MOVE      *BLANKS       SC7A05
     C                   MOVEL     QQCPT4        SC7A05
     C                   ENDIF                                                  END VALUE CODE
      *
      ** PROCEDURE CODE METHOD WAS 5 SEARCH FOR HCPCS CODES
      *
     C     SCPCDM        IFEQ      '5'
      *
     C                   MOVE      *BLANKS       QQCPT4
     C                   MOVE      *ZEROS        QQFDAT
     C                   CALL      'F0520R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCPT4
     C                   PARM                    QQFDAT
     C                   PARM                    QQAST
     C                   PARM                    QQRTCD
     C                   MOVE      *BLANKS       SC7A05
     C                   MOVEL     QQCPT4        SC7A05
     C                   ENDIF                                                  END VALUE CODE
      *
      ** PROCEDURE CODE METHOD WAS 9 SEARCH FOR ICD9 CODES
      *
     C     SCPCDM        IFEQ      '9'
      *
     C*** BEGIN DEL *** 05/06/97 **************************************
     C**********           MOVE *BLANKS   QQCPT4
     C**********           CALL 'F0510R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQCPT4
     C**********           PARM           QQRTCD
     C*** END DEL *** 05/06/97 ****************************************
     C*
     C*** BEGIN ADD *** 05/06/97 **************************************
     C                   MOVE      '2'           QQCALL
     C                   MOVE      *BLANKS       QQKC
     C                   MOVE      *BLANKS       QQKL
     C                   MOVE      *BLANKS       QQCPT4
     C                   MOVE      *BLANKS       QQLOST
     C*
     C                   CALL      'F2010R'
     C                   PARM                    QQCALL
     C                   PARM                    QQKC
     C                   PARM                    QQKL
     C                   PARM                    QQCPT4
     C                   PARM                    QQLOST
     C     QQKC          IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   ENDIF
     C     QQKL          IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   ENDIF
     C     QQCPT4        IFEQ      *BLANKS
     C                   MOVE      '*'           QQRTCD
     C                   ENDIF
     C*** END ADD *** 05/06/97 ****************************************
     C                   MOVE      *BLANKS       SC7A05
     C                   MOVEL     QQCPT4        SC7A05
     C                   ENDIF                                                  END VALUE CODE
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   GOTO      TERM
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      'B'
     C                   Z-ADD     23            SCLI92
     C                   Z-ADD     22            SCPS92
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      '*'
     C     QQCPT4        ANDEQ     *BLANKS
     C                   Z-ADD     23            SCLI92
     C                   Z-ADD     22            SCPS92
     C                   ELSE
     C                   Z-ADD     23            SCLI92
     C                   Z-ADD     30            SCPS92
     C                   ENDIF
      *
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END OTHR PROC 5
      *
      ** SEARCH FOR OTHER PROCEDURE 5
      *
      ** PROCEDURE CODE METHOD WAS 4 SEARCH FOR CPT4 CODES
      *
     C     HFLD          IFEQ      'SC7A06'
      *
     C     SCPCDM        IFEQ      '4'
      *
     C                   MOVE      *BLANKS       QQCPT4
     C                   MOVE      *ZEROS        QQFDAT
     C                   CALL      'F0520R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCPT4
     C                   PARM                    QQFDAT
     C                   PARM                    QQAST
     C                   PARM                    QQRTCD
     C                   MOVE      *BLANKS       SC7A06
     C                   MOVEL     QQCPT4        SC7A06
     C                   ENDIF                                                  END VALUE CODE
      *
      ** PROCEDURE CODE METHOD WAS 5 SEARCH FOR HCPCS CODES
      *
     C     SCPCDM        IFEQ      '5'
      *
     C                   MOVE      *BLANKS       QQCPT4
     C                   MOVE      *ZEROS        QQFDAT
     C                   CALL      'F0520R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCPT4
     C                   PARM                    QQFDAT
     C                   PARM                    QQAST
     C                   PARM                    QQRTCD
     C                   MOVE      *BLANKS       SC7A06
     C                   MOVEL     QQCPT4        SC7A06
     C                   ENDIF                                                  END VALUE CODE
      *
      ** PROCEDURE CODE METHOD WAS 9 SEARCH FOR ICD9 CODES
      *
     C     SCPCDM        IFEQ      '9'
      *
     C*** BEGIN DEL *** 05/06/97 **************************************
     C**********           MOVE *BLANKS   QQCPT4
     C**********           CALL 'F0510R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQCPT4
     C**********           PARM           QQRTCD
     C*** END DEL *** 05/06/97 ****************************************
     C*** BEGIN ADD *** 05/06/97 **************************************
     C                   MOVE      '2'           QQCALL
     C                   MOVE      *BLANKS       QQKC
     C                   MOVE      *BLANKS       QQKL
     C                   MOVE      *BLANKS       QQCPT4
     C                   MOVE      *BLANKS       QQLOST
     C*
     C                   CALL      'F2010R'
     C                   PARM                    QQCALL
     C                   PARM                    QQKC
     C                   PARM                    QQKL
     C                   PARM                    QQCPT4
     C                   PARM                    QQLOST
     C     QQKC          IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   ENDIF
     C     QQKL          IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   ENDIF
     C     QQCPT4        IFEQ      *BLANKS
     C                   MOVE      '*'           QQRTCD
     C                   ENDIF
     C*** END ADD *** 05/06/97 ****************************************
     C                   MOVE      *BLANKS       SC7A06
     C                   MOVEL     QQCPT4        SC7A06
     C                   ENDIF                                                  END VALUE CODE
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   GOTO      TERM
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      'B'
     C                   Z-ADD     23            SCLI92
     C                   Z-ADD     40            SCPS92
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      '*'
     C     QQKEY         ANDEQ     *BLANKS
     C                   Z-ADD     23            SCLI92
     C                   Z-ADD     40            SCPS92
     C                   ELSE
     C                   Z-ADD     23            SCLI92
     C                   Z-ADD     48            SCPS92
     C                   ENDIF
      *
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *IN70
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  END OTHR PROC 6
      *
      ** A COMMAND KEY WAS STRUCK ON A NON VALID FIELD ISSUE ERROR
      *
     C     *IN70         IFEQ      *ON
     C                   Z-ADD     10            SCLI92
     C                   Z-ADD     21            SCPS92
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
     C     *INKL         IFEQ      *ON
     C                   MOVE      'B'           QQRTCD
     C                   ELSE
     C                   MOVE      ' '           QQRTCD
     C                   ENDIF                                                  *INKL
     C                   ENDSR
      *
      /EJECT
      **************************************************************
      ** PROGRAM INITIALIZATION                                  ***
      **  ONE TIME, AND PERHAPS AFTER F13=CHG USR PARMS          ***
      **************************************************************
     C     PGMSTR        BEGSR
     C*** BEG ADD *** 10/12/98 ***********************
     C                   MOVE      '1'           XXFTYP            1            FUND TYPE
     C*** END ADD *** 10/12/98 ***********************
     C                   MOVE      *BLANKS       SCCONM           40
     C                   MOVE      '0'           F3REQD
     C******     SCACDT    MULT 100.0001  SCACDT
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
     C*** END   DEL ** 11/06/97 ************************************************
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV                          ADDED 03/28/95
     C     EMKEY         CHAIN(N)  PEM01                              40
     C                   MOVEL(P)  EMNAME        SCENAM
      *
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C                   MOVEL(P)  SCAST         XXSTCN            6
     C     4             SUBST     Q1CNTY:6      XCNTY             4
     C                   MOVE      XCNTY         XXSTCN
     C                   MOVEL(P)  XXSTCN        XXKEY
     C*
     C     TBKEY         CHAIN     PCC01                              60
     C     *IN60         IFEQ      '1'
     C                   CLEAR                   SCCNTY
     C                   ELSE
     C                   MOVEL     CCDESC        SCCNTY
     C                   END
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
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       scSS# = MaskData('SSN':'SCSS#':SCSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C                   ENDSR
      /EJECT
      *
