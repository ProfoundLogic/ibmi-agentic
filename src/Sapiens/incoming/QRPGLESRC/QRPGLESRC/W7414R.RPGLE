     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE W7414R - NCCI Policy Information Page Pending File Build
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**                                                             **
     H**     THIS PROGRAM SEARCHES THE EMPLOYER DETAIL FILE FOR A    **
     H**     GIVEN FUND YEAR AND IDENTIFIES EMPLOYERS THAT HAVE BEEN **
     H**     BILLED.  IF A POLICY PAGE HAS NOT BEEN SUBMITTED FOR    **
     H**     A BILLED EMPLOYER, A PENDING POLICY SUBMISSION HISTORY  **
     H**     RECORD WILL BE WRITTEN FOR THE EMPLOYER.                **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     W7414C - PENDING FILE BUILD OVERRIDE CL                 **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**                                                             **
     H**   PROGRAMMER: C:24                                          **
     H**   CREATED:    6/27/94      PROJECT: WC313                   **
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/14/24 C:65  Project: Phw#117111 (Amwins)                   *
     H*                Pull the policy effective date for the policy  *
     H*                from the earliest found on policy year instead *
     H*                of from the state reporting division.          *
     H*                                                               *
     H* 04/04/08 C:65  Project: SUPP#62336                            *
     H*                PREVENT POLICY PAGE OPTION IF CANCELLATION     *
     H*                DATE IS EQUAL TO OR LESS THAN POLICY EFF DATE. *
     H*                                                               *
     H* 04/02/04 C:65  SUPPORT WA072 41873                            *
     H*                CHANGE THE RESTRICTION ON SUBMITTING POLICY    *
     H*                PAGES TO REQUIRE AN EMPLOYER HISTORY RECORD.   *
     H*                SIMPLY CHECKING ELFAF IS NOT SUFFICIENT TO     *
     H*                GURANTEE THEY HAVE RUN A CALC,UPD,& PRT BILL.  *
     H*                                                               *
     H* 06/14/02  C:35   PROJECT: SUPPORT WA107 #33024                *
     H*                  INITIALIZE SHIDT.                            *
     H*                                                               *
     H* 03/30/01  C:133  PROJECT: SUPPORT WA112 #26854                *
     H*                  Add output of standard maintenance fields to *
     H*                  policy submission history record             *
     H*                                                               *
     H* 02/09/99  C:87   PROJECT: SUPPORT #16471                      *
     H*                  Do not generate a policy page record of the  *
     H*                  policy from date is equal to the pol to date.*
     H*                                                               *
     H* 01/18/99  C:35   PROJECT: SUPPORT WA048 15732                 *
     H*                  CHANGE TO USE ELFAF = BLANK TO DETERMINE IF  *
     H*                  EMPLOYER HAS NOT BEEN BILLED.                *
     H*                                                               *
     H*  11/21/96 C:65 PROJECT: CM023                                 *
     H*                ADDED MOST RECENT AGENT FOR EACH EMPLOYER      *
     H*                SELECTED AND ADD TO SUBMISSION HISTORY FILE.   *
     H*                                                               *
     H*  08/22/96 C:35 PROJECT WC525                                  *
     H*                CHANGED TO USE PROPER KEY FOR SETGT.  ALSO,    *
     H*                UNDO MOD FROM 07/20/94 THAT SKIP CANCELLED     *
     H*                ACCOUNTS.                                      *
     H*                                                               *
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      60      General Usage (CHAIN)                          **
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H*     WDELL3 (PEM01): CO/FND/PFDT                              **
     H**    WMEPL  (PEP01): CO/FND/EMP#/DIV/FYR                      **
     H**    WMEML  (PEM01): CO/FND/EMP#/DIV                          **
     H**    WMSHL1 (PSH01): CO/FND/EMP#/FYR/TYPE                     **
     H*****************************************************************
     FWDELL3    IF   E           K DISK
     F*** begin add ***  08/14/24  ***********************************
     Fwdellb    if   e           k disk    prefix(b)
     F                                     rename(pel01:pel0b)
     F*** end add *****  08/14/24  ***********************************
      **** BEGIN DEL **** 01/18/99 *********************************************
     F********** WMEPL   IF  E           K        DISK
      **** END   DEL **** 01/18/99 *********************************************
     FWMAHL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMSHL1    IF A E           K DISK
     F*** BEGIN ADD ***  04/02/04  ************************************
     FWDEHL6    IF   E           K DISK
     F*** END ADD *****  04/02/04  ************************************

     D*** BEGIN ADD ***  04/04/08  ***************************************
     D*** END ADD *****  04/04/08  ***************************************
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATES
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D*** BEGIN ADD ***  04/04/08  ***************************************
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECTIVE DATES
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D*** END ADD *****  04/04/08  ***************************************
     D*
     D***  BEGIN ADD  ***  03/30/01  **********************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I***  END ADD    ***  03/30/01   *********************************

     C*
     C*  KEY LIST FOR EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXPFDT
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXPFDT
     C                   KFLD                    XXEMP#
     C*** begin add ***  08/14/24  *********************************************
     C     ELKYB         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*** end add *****  08/14/24  *********************************************
     C*
     C*  AGENT HISTORY MASTER FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXJDIV
     C                   KFLD                    XXFYR
     C*
     C*  KEY LIST FOR EMPLOYER PAYROLL FILE
     C*
     C     EPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  KEY LIST FOR EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  KEY LIST FOR SUBMISSION HISTORY FILE
     C*
     C     SHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXTYPE
     C*** BEGIN ADD ***  04/02/04  ************************************
     C*
     C* EMPLOYER HISTORY FILE
     C*
     C     EHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*** END ADD *****  04/02/04  ************************************
     C*
     C     *LIKE         DEFINE    AHAGT#        XXAGT#
     C     *LIKE         DEFINE    ELPFDT        XXFPFD
     C     *LIKE         DEFINE    ELPFDT        XXTPFD
     C     *LIKE         DEFINE    ELPFDT        XXPFDT
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    SHDIV         XXJDIV
     C     *LIKE         DEFINE    ELDIV         XXRDIV
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFPFD            8
     C                   PARM                    QQTPFD            8
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
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFPFD        X1FPFD            8 0
     C                   MOVE      QQTPFD        X1TPFD            8 0
     C*
     C                   Z-ADD     X1FPFD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFPFD
     C*
     C                   Z-ADD     X1TPFD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTPFD
     C*
     C                   MOVE      'P'           XXTYPE            1
     C                   MOVE      *OFF          XXWRIT            1
     C*
     C*  GET THE FIRST EMPLOYER DETAIL RECORD
     C*
     C                   MOVE      XXFPFD        XXPFDT
     C     ELKY          SETLL     PEL01
     C     ELKY1         READE     PEL01                                  50
     C*
     C***  BEGIN ADD  ***  02/09/99  **********************************
     C*  Keep reading employer detail for the first record found where
     C*  the policy from date is not equal to the policy to date.
     C     *IN50         DOWEQ     *OFF
     C     ELPFDT        ANDEQ     ELPTDT
     C     ELKY1         READE     PEL01                                  50
     C                   ENDDO
     C***  END   ADD  ***  02/09/99  **********************************
      *
     C                   MOVE      *OFF          *IN61
     C*
     C*  FOR THE FIRST RECORD, SET UP WORK FIELDS
     C*
     C     *IN50         IFEQ      *OFF
     C     ELPFDT        ANDLE     XXTPFD
     C                   MOVE      *ON           XXWRIT
     C                   MOVE      ELEMP#        XXEMP#
     C                   MOVE      ELDIV         XXDIV
     C                   MOVE      ELDIV         XXRDIV
     C                   MOVE      ELFYR         XXFYR
     C                   MOVE      ELPFDT        XXPFDT
     C*
     C     SHKY          CHAIN     PSH01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *OFF          XXWRIT
     C                   MOVE      ELPFDT        XXPFDT
     C     ELKY2         SETGT     PEL01
     C                   GOTO      NEXT
     C                   ENDIF                                                  *IN60 = OFF
     C*
     C                   ENDIF                                                  *IN50 = OFF
     C*
     C     *IN50         DOWEQ     *OFF
     C     ELPFDT        ANDLE     XXTPFD
     C*
     C***  BEGIN ADD  ***  02/09/99  **********************************
     C*    Skip policy where from date equals to date
     C     ELPFDT        IFNE      ELPTDT
     C***  END   ADD  ***  02/09/99  **********************************
     C*
     C*  IF THE EMPLOYER NUMBER CHANGED, WRITE THE RECORD
     C*
     C     ELEMP#        IFNE      XXEMP#
     C*
     C     XXWRIT        IFEQ      *ON
     C                   CLEAR                   PSH01
     C                   MOVE      XXCO#         SHCO#
     C                   MOVE      XXFND         SHFND
     C                   MOVE      XXEMP#        SHEMP#
     C                   MOVE      XXRDIV        SHDIV
     C                   MOVE      XXFYR         SHFYR
     C                   EXSR      WRITE
     C                   MOVE      XXAGT#        SHAGT#
     C                   MOVE      XXTYPE        SHTYPE
     C                   MOVE      '1'           SHSTAT
     C*** begin chg ***  08/14/24  *********************************************
     C******             MOVE      XXPFDT        SHPFDT
     C     elkyb         Setll     pel0b
     C     elkyb         Reade     pel0b
     C                   Move      BELpfdt       SHPFDT
     C*** end chg *****  08/14/24  *********************************************
     C***  BEGIN ADD  ***  03/30/01  **********************************
     C                   MOVE      Q1USER        SHUSER
     C                   MOVE      Q1PGM         SHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     C                   MOVE      DSTIME        SHTIME
     C                   MOVE      EMDT          SHDT
     C***  END ADD  ***  03/30/01  ************************************
      **** BEGIN ADD **** 06/14/02 *********************************************
     C                   Z-ADD     SHDT          SHIDT
      **** END   ADD **** 06/14/02 *********************************************
     C                   WRITE     PSH01
     C                   ENDIF                                                  XXWRIT = ON
     C*
     C*  SET UP WORK FIELDS FOR THE NEXT EMPLOYER
     C*
     C                   MOVE      *ON           XXWRIT
     C                   MOVE      ELEMP#        XXEMP#
     C                   MOVE      ELDIV         XXDIV
     C                   MOVE      ELDIV         XXRDIV
     C                   MOVE      ELFYR         XXFYR
     C                   MOVE      ELPFDT        XXPFDT
     C*
     C*  IF A POLICY SUBMISSION RECORD EXISTS, SKIP AHEAD
     C*  TO THE NEXT EMPLOYER.
     C*
     C     SHKY          CHAIN     PSH01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *OFF          XXWRIT
     C                   MOVE      ELPFDT        XXPFDT
     C     ELKY2         SETGT     PEL01
     C                   GOTO      NEXT
     C                   ENDIF                                                  *IN60 = OFF
     C*
     C                   ENDIF                                                  ELEMP# NE XXEMP#
     C*
     C                   MOVE      ELDIV         XXDIV
      **** BEGIN CHG **** 01/18/99 *********************************************
     C**********
     C** IF THE MANUAL PREMIUM IS ZERO AND AT LEAST ONE PAYROLL
     C** RECORD EXISTS, THE EMPLOYER HAS NOT BEEN BILLED.
     C**********
     C********** ELMAPR    IFEQ *ZERO
     C**********
     C********** EPKY      CHAINPEP01                60
     C**********
     C********** *IN60     DOWEQ*OFF
     C********** EPSAPR    IFNE *ZERO
     C********** EPEAPR    ORNE *ZERO
     C******************** MOVE *OFF      XXWRIT
     C******************** MOVE ELPFDT    XXPFDT
     C********** ELKY2     SETGTPEL01
     C******************** GOTO NEXT
     C******************** ENDIF
     C********************
     C********** EPKY      READEPEP01                    60
     C******************** ENDDO
     C********************
     C******************** ENDIF                           ELMAPR = 0
      *
      * IF THE FINAL AUDIT FLAG (ELFAF) IS BLANK, THE EMPLOYER HAS
      * NOT BEEN BILLED
      *
     C*** BEGIN ADD ***  04/02/04  ************************************
     C*********  ELFAF     IFEQ *BLANK
     C                   MOVE      *BLANK        XXBILL            1
     C     EHKY1         CHAIN     PEH01                              60
     C     *IN60         IFEQ      *OFF
     C     EHTRDT        ANDNE     *ZERO
     C                   MOVE      'Y'           XXBILL
     C                   ENDIF
      *
     C     XXBILL        IFEQ      *BLANK
     C*** END ADD *****  04/02/04  ************************************
     C                   MOVE      *OFF          XXWRIT
     C                   MOVE      ELPFDT        XXPFDT
     C     ELKY2         SETGT     PEL01
     C                   GOTO      NEXT
     C                   ENDIF                                                  ELFAF IFEQ *BLANK
      **** END   CHG **** 01/18/99 *********************************************
     C*
     C*  CHECK THE STATE REPORTING FLAG IN THE EMPLOYER MASTER.
     C*
     C     EMKY          CHAIN     PEM01                              60
     C     EMSTRP        IFEQ      'Y'
     C                   MOVE      EMDIV         XXRDIV
     C                   ENDIF                                                  EMSTRP = Y
     C*** BEGIN ADD ***  04/04/08  ***************************************
     C                   Z-ADD     1             I1                3 0
     C*
     C     *ZERO         LOOKUP    EF(I1)                                 70
     C*
     C     *IN70         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN70 IFEQ *ON
     C*
     C     I1            IFNE      0
     C*
     C     CD(I1)        IFNE      *ZERO
     C     CD(I1)        ANDLE     ELPFDT
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  CD,I1 IFNE *ZERO
     C*
     C                   ENDIF                                                  I1 IFNE 0
     C*** END ADD *****  04/04/08  ***************************************
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *OFF          XXWRIT
     C                   MOVE      ELPFDT        XXPFDT
     C     ELKY2         SETGT     PEL01
     C                   GOTO      NEXT
     C                   END
     C*
     C***  BEGIN ADD  ***  02/09/99  **********************************
     C                   ENDIF                                                  ELPFDT<>ELPTDT
     C***  END   ADD  ***  02/09/99  **********************************
     C*
     C     NEXT          TAG
     C     ELKY1         READE     PEL01                                  50
     C                   ENDDO                                                  *IN50 DOW OFF
     C*
     C*  IF THE LAST EMPLOYER READ NEEDS A RECORD, WRITE
     C*  IT NOW.
     C*
     C     XXWRIT        IFEQ      *ON
     C                   CLEAR                   PSH01
     C                   MOVE      XXCO#         SHCO#
     C                   MOVE      XXFND         SHFND
     C                   MOVE      XXEMP#        SHEMP#
     C                   MOVE      XXRDIV        SHDIV
     C                   MOVE      XXFYR         SHFYR
     C                   EXSR      WRITE
     C                   MOVE      XXAGT#        SHAGT#
     C                   MOVE      XXTYPE        SHTYPE
     C                   MOVE      '1'           SHSTAT
     C*** begin chg ***  08/14/24  *********************************************
     C******             MOVE      XXPFDT        SHPFDT
     C     elkyb         Setll     pel0b
     C     elkyb         Reade     pel0b
     C                   Move      BELpfdt       SHPFDT
     C*** end chg *****  08/14/24  *********************************************
     C***  BEGIN ADD  ***  03/30/01  **********************************
     C                   MOVE      Q1USER        SHUSER
     C                   MOVE      Q1PGM         SHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     C                   MOVE      DSTIME        SHTIME
     C                   MOVE      EMDT          SHDT
     C***  END ADD    ***  03/30/01  **********************************
     C                   WRITE     PSH01
     C                   ENDIF                                                  XXWRIT = ON
     C*
     C                   MOVE      *ON           *INLR
     C**************************************************************
     C*
     C*  SUBROUTINE TO FIND MOST RECENT ACTIVE AGENT FOR AN EMPLOYER
     C*
     C**************************************************************
     C     WRITE         BEGSR
     C*
     C                   Z-ADD     SHDIV         XXJDIV
     C     AHKY          SETGT     PAH01
     C                   READP     PAH01                                  60
     C     *IN60         DOWEQ     *OFF
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  60
     C                   ENDDO                                                  *IN60 == 0
     C*
     C***  TEST THE SELECTION CRITERIA.
     C*
     C     *IN60         IFEQ      *OFF
     C     XXCO#         ANDEQ     AHCO#
     C     XXFND         ANDEQ     AHFND
     C     XXEMP#        ANDEQ     AHEMP#
     C     XXJDIV        ANDEQ     AHDIV
     C     XXFYR         ANDEQ     AHFYR
     C*
     C                   MOVE      AHAGT#        XXAGT#
     C                   ELSE
     C                   MOVE      *ZERO         XXAGT#
     C                   ENDIF                                                  *IN60 == 0
     C*
     C                   ENDSR
     C**************************************************************
