     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*****************************************************************
     H/TITLE W0051R38 - LARGE DEDUCTIBLE MAINTENANCE
     H*****************************************************************
     H*  DESCRIPTION:  This program will allow a user to change, view *
     H*                or add records in the Large Deductible file.   *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0051R38 - LARGE DEDUCTIBLE MAINTENANCE                    *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 12/30/09  PROGRAMMER: C:232 PROJECT: WC1838         *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 04/26/23 C:300 Support: 110193                                *
     H*                Work with Groups - Stat Rpt/Rating - Opt 38    *
     H*                Large Deductibe is not working in PDSQA due to *
     H*                Level check error. Just Recompling DSPF & PGM  *
     H*                                                               *
     H* 07/06/22 C:136 Project: WC2809                                *
     H*                Remove popup when LD group defaults change.    *
     H*                                                               *
     H* 01/24/18 C:33  Support: 50626                                 *
     H*                Include LD with RR copy                        *
      *                                                               *
      * 10/08/17 C:33  Project: 150WAV/150WBL - CLT150 - 49390        *
      *                Add deductible aggregate %.                    *
      *                Add variable aggregate Flag.                   *
     H*                                                               *
     H* 01/28/11 C:190 Project: WC1954                                *
     H*                Make the changes to use the screen for both    *
     H*                the group setup and for the individual policy  *
     H*                setup.                                         *
     H*                                                               *
     H* 06/25/10 C:189 Project: WC1878                                *
     H*                 Prevent Large Deductible Information from     *
     H*                 being cleared if there is a  balance          *
     H*                 in deductible billing.                        *
     H*                                                               *
     H*  04/15/10 C:062 Project: WC1838  25WQW  Priss #30             *
     H*                 Set Exp dt 1 to Eff dt 2 if 0                 *
     H*                                                               *
     H*  03/19/10 C:136 Project: WC1838  25WQW  Priss #17             *
     H*                -Add edit so that the  2nd eff date range can  *
     H*                 not overlap the first.                        *
     H*                -Remove the second renewal date                *
     H*                                                               *
     H*  03/17/10 C:33  Project: WC1838  25WQW  Priss #16             *
     H*                 Aggregate amount is not required.             *
     H*                                                               *
     H*  03/12/10 C:62  Project: WC1838  25WQW  Priss #9 and 12       *
     H*                 -Fix add mode                                 *
     H*                 -Require Plan                                 *
     H*                                                               *
     H*  03/11/10 C:33  Project: WC1838  25WQW  Priss #10             *
     H*                 Compare aggregate amount to other divisions.  *
     H*                                                               *
     H*  03/08/10 C:33  Project: WC1838  25WQW  Priss #11             *
     H*                -Edit to not allow for SCR01 to be cleared if  *
     H*                  SCR02 has values.                            *
     H*                -Allow for data to be cleared.                 *
     H*                                                               *
     H*  03/01/10 C:33  Project: 25WRB  Priss #7                      *
     H*                 Load fields for maintainability.              *
     H*                                                               *
     H*  02/05/10 C:65  Project:  25WRB                               *
     H*                 Add claims liability and allowance fields to  *
     H*                 screen 3.  Only display this screen if client *
     H*                 ID is for TASB.                               *
     H*                                                               *
     H* ------------------------------------------------------------- *
     H*  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
     H* ------------------------------------------------------------- *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*  SCR02:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*  SCR03:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW0051F38  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMLDP     UF A E             DISK
     F                                     RENAME(PLD01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWMLDL1    IF   E           K DISK
     F                                     INFDS(DBLDS)
     FWMLDL2    if   e           k disk    rename(pld01:pld02)
     FWDELP     IF   E             DISK
     FWMEML     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWTGTL     IF   E           K DISK
     FWTRTL1    IF   E           K DISK
     FWDDDL1    IF   E           K DISK
     FWMFNL     IF   E           K DISK
     fwmrsl     if   e           k disk
     fwdrrp     if   e             disk
     fwmldl4    if   e           k disk    rename(pld01:pld04) usropn
     fwmemlq    if   e           k disk    rename(pem01:pem0q) usropn
      *
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     FWmeal10   if   e           k disk
     Fwtabl     if   e           k disk
     F                                     IGNORE(PRT01)
     f                                     ignore(prs01)
      *
     D*================================================================
     D*  Stand Alone Field definitions
     D*================================================================
     D addMode         S               n
     D chgMode         S               n
     D dspMode         S               n
     dprEmpexist       pr             1n
     D*================================================================
     D*  Array & Table definitions
     D*================================================================
     D XXBL            S             50    DIM(5)
     D XXAL            S             50    DIM(5)
     D*================================================================
     D*  Data Structures & constants definitions
     D*================================================================
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
      *
     D DBLDS           DS
     D  @@RRN1               397    400B 0
      *
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)                               EFF.DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
      *
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
      *
      *
     D OPTNM           C                   CONST('Large Deductible In-
     D                                     formation')
     D COMAPI          C                   CONST('WA001R')
     D**            2                     C         #OFSCR
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)

     DAcceptAgg        pr             1n
     D inEffDt                             like(scEfD1)
     D inAgg                               like(LDAMG1)
     DxtotalDedB       s             14  2
     d submit          s               n
     d clear           s               n
     d lib             s             10
     d modlib          s             10
     d cmd             s            255
     drtvenvlib        PR                  extpgm('RTVENVLIB')
     d requesttype                   10    const
     d libname                       10
     d modlibname                    10
    D/copy sPrRunCmd
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C*  PLIST Definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @1MODE            1            Decides Emp or Grp
     C                   PARM                    @@QTE             1
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*================================================================
     C*  KLIST Definitions
     C*================================================================
     C*
     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*  GROUP MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     LDKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C     ldky4         klist
     C                   kfld                    xxco#             3 0
     C                   kfld                    xxfnd             3 0
     C                   kfld                    xxst              2
     C                   kfld                    x@emp#            9 0
     C                   kfld                    xxefdt            7 0

     c     ldky5         klist
     c                   kfld                    xxco#             3 0
     c                   kfld                    xxfnd             3 0
     c                   kfld                    xxst              2
     C                   kfld                    x@emp#            9 0

     c     rsky          klist
     c                   kfld                    xxco#
     c                   kfld                    elrtcd

     C     fnky1         klist
     C                   kfld                    rrco#
     C                   kfld                    rrfnd

     C     EMKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C*
     C     RTKY          KLIST
     C                   kfld                    xxco#
     C                   KFLD                    XXKEY1
     C*
     C* KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   kfld                    xxco#
     C                   KFLD                    XZTBID
     C                   KFLD                    XXKEY
     C*
     C*  GROUP DETAIL FILE
     C*
     C     F1KY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXFYR
     C*
     C     EAKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    XXARTY            1
     C
     C     tbkey         klist
     C                   kfld                    xxco#
     C                   kfld                    xxkey            10
     C
     C*================================================================
     C*  *LIKE DEFN definitions
     C*================================================================
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    RTKEY         XXKEY1
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    GTTBID        XZTBID
     C     *LIKE         DEFINE    ELPFDT        XXPFDT
     C     *LIKE         DEFINE    ELPTDT        XXDAT2
     C     *LIKE         DEFINE    ELPFDT        XXDAT1
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    EMCAD1        XXCXDT
     C*===============================================================
     C* DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
     C*===============================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*
     C*===============================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*===============================================================
     C*
     C     CHGSTS        BEGSR
     C                   ENDSR                                                  CHGSTS
     C*
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
      *
     C                   MOVEA     '00000000'    *IN(30)
     C                   MOVEA     '000'         *IN(38)
     C                   MOVEA     '00000000'    *IN(42)
     C                   MOVEA     '00000'       *IN(50)
     C                   MOVEA     '0'           *IN(88)
      /Free
       erADP1   = *off;
       msADP1   = *blanks;
      /End-free
      *
     C                   MOVEL     'WC'          XSKEY             2
      *
     C     XSKEY         CHAIN     SWC01                              60
     C*
     C* IF BILLING RECORDS RESIDE FOR THIS POLICY YEAR, DEDUCTIBLE DATA
     C* CANNOT BE CLEARED"
     C*
     C                   if        *in91 = *off
     C     SCEFD1        IFEQ      *ZERO
     C     SCEFD1        IFNE      LDEFD1
     C     LDKY          CHAIN     PDD01                              60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        86  90
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C* If Deductible A/R billings exist, Large Deductible Info can not be cleared
     C*
     C     SCEFD1        IFEQ      *ZERO
     C     XTOTALDEDB    ANDNE     0
     C                   SETON                                        28  90
     C                   ENDIF
     C* SCR01 data must be entered, if SCR02 data exists.
     C  n90              if        scEfD1 = *zero
     C                              and scEfD2 <> *zero
     C                   seton                                        30  90
     C                   endif
     C*
     C                   Z-ADD     SCFYR         XXFYR
     C*
     C     F1KY1         CHAIN     PF101                              60
     C*
     C* EDIT POLICY FROM DATE
     C*
     C  N90SCEFD1        IFNE      *ZEROS
     C                   Z-ADD     SCEFD1        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFD1
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        55  90
     C                   ENDIF
     C                   ENDIF
     C*
     C  N90SCEFD1        IFNE      *ZEROS
     C                   Z-ADD     SCEFD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT1
     C*
     C     XXDAT1        IFLT      F1STPO
     C     XXDAT1        ORGE      F1ENPO
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* EDIT POLICY EXPRIRATION DATE
     C*
     C     SCEFD1        IFNE      *ZEROS
     C     SCEXP1        ANDNE     *ZEROS
     C                   Z-ADD     SCEXP1        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEXP1
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        315990
     C                   ENDIF
     C                   ENDIF
     C*
     C*  DATES MUST BE ASCENDING ORDER
     C*
     C     SCEFD1        IFNE      *ZEROS
     C     SCEXP1        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCEFD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT1
     C*
     C                   Z-ADD     SCEXP1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT2
     C*
     C  N90XXDAT2        IFLT      XXDAT1
     C                   SETON                                        57  90
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  BOTH DATES MUST BE BETWEEN FUND POLICY START & END DATES
     C*
     C     SCEFD1        IFNE      *ZEROS
     C     *IN90         ANDEQ     *OFF
     C*
     C     XXDAT1        IFLT      ELPFDT
     C     XXDAT1        ORGE      ELPTDT
     C                   SETON                                        5890
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     SCEXP1        IFNE      *ZEROS
     C     *IN90         ANDEQ     *OFF
     C*
     C     XXDAT2        IFLT      ELPFDT
     C     XXDAT2        ORGT      ELPTDT
     C                   SETON                                        1690
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* EFFETIVE DATE CAN NOT BE ZERO
     C*
     C  N90SCEFD1        IFEQ      *ZEROS
     C     SCDES1        IFNE      *BLANKS
     C     SCPCT1        ORNE      *ZEROS
     C     SCTYP1        ORNE      *BLANKS
     C     SCAMP1        ORNE      *ZEROS
     C     SCAMG1        ORNE      *ZEROS
     C     scVarA1       ORNE      *BLANKS
     C     scADP1        ORNE      *ZEROS
     C     SCPLN1        ORNE      *BLANKS
     C     SCVRB1        ORNE      *BLANKS
     C     SCOCV1        ORNE      *BLANKS
     C     SCTOT1        ORNE      *BLANKS
     C     SCCLS1        ORNE      *ZEROS
     C     SCPLV1        ORNE      *BLANKS
     C     SCALA1        ORNE      *BLANKS
     C     SCRNW1        ORNE      *BLANKS
     C     SCR101        ORNE      *BLANKS
     C     SCR102        ORNE      *BLANKS
     C     SCR103        ORNE      *BLANKS
     C     SCR104        ORNE      *BLANKS
     C     SCR105        ORNE      *BLANKS
     C     SCR106        ORNE      *BLANKS
     C     SCR107        ORNE      *BLANKS
     C     SCR108        ORNE      *BLANKS
     C     SCR109        ORNE      *BLANKS
     C     SCR110        ORNE      *BLANKS
     C                   SETON                                        87  90
     C                   ENDIF
     C                   ENDIF
     c                   endif
     C*
     C*   DESCRIPTION IS REQUIRED (IF EFFECTIVE DATE IS ENTERED)
     C*
     C                   if        scefd1 <> *zeros or
     c                             (*in91 = *on and sctyp1 <> ' ')
     C  N90SCDES1        IFEQ      *BLANKS
     C                   SETON                                        326290
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
      *
     C*   GROSS/NET FLAG SHOULD BE 'G' OR 'N'
      *
     C  N90SCTYP1        IFNE      'G'
     C     SCTYP1        ANDNE     'N'
     C     SCTYP1        ANDNE     *BLANKS
     C                   SETON                                        346390
     C                   ENDIF                                                              O
      *
     C                   if        scefd1 <> *zeros or
     c                             (*in91 = *on and scdes1 <> ' ')
     C  N90SCTYP1        IFEQ      *BLANKS
     C                   SETON                                        346490
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
      *
     C*   Deductible amount can not be less than zero
     C  n90              if        scdes1 <> ' '
     C                              and scAmP1 < *zero
     C                   seton                                        356590
     C                   endif                                                              O
     c  n91              eval      xxco# = elco#
     c   91              eval      xxco# = rrco#
      *
     C*   VALIDATE DEDUCTIBLE PLAN TYPE.....
      *
     C     WCFL03        IFEQ      'Y'
      *
     C  N90SCPLN1        IFNE      *BLANKS
     C                   MOVEL(P)  '  DT'        XZTBID
     C                   MOVEL(P)  SCPLN1        XXKEY
     C                   if        scdes1 <> ' '
     C     gtky          chain     pgt01                              60
     C     *in60         ifeq      *on
     C     *in60         oreq      *off
     C     GTSTAT        andne     'A'
     C                   SETON                                        377290
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
      *
     C                   ENDIF                                                  WCFL03 = 'Y'
      *
     C* PER CLAIM/PER OCCURRENCE FLAG SHOULD BE 'C' OR 'O'
      *
     C  N90SCVRB1        IFNE      'C'
     C     SCVRB1        ANDNE     'O'
     C     SCVRB1        ANDNE     *BLANKS
     C                   SETON                                        386690
     C                   ENDIF                                                              O
     C  n90              if        scdes1 <> ' 'and scvrb1 = *blanks
     C                   SETON                                        386790
     C                   ENDIF                                                              O
      *
     C*  OCCUPATIONAL DISEASE ONLY: PER CLAIM/PER OCCURRENCE FLAG
      * SHOULD BE 'C' OR 'O' OR BLANKS
      *
     C  N90SCOCV1        IFNE      'C'
     C     SCOCV1        ANDNE     'O'
     C     SCOCV1        ANDNE     *BLANKS
     C                   SETON                                        396890
     C                   ENDIF                                                              O
      *
     C*  STATISTICAL CLASS CODE IS REQUIRED....
      *
     C  N90WCFL03        IFEQ      'Y'
     C                   if        scdes1 <> ' '
     C     SCCLS1        IFEQ      *ZERO
     C                   SETON                                        888590
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C*USE TOTAL INCURRED FOR DEDUCTIBLE BILLING SHOULD BE 'Y' OR BLANKS
      *
     C  N90SCTOT1        IFNE      'Y'
     C     SCTOT1        ANDNE     *BLANKS
     C                   SETON                                        406990
     C                   ENDIF
      *
     C*   VALIDATE BILLING CALULATION LEVEL.....
      *
     C  N90SCPLV1        IFNE      *BLANKS
     C                   MOVEL(P)  'BCLV'        XZTBID
     C                   MOVEL(P)  SCPLV1        XXKEY
     C                   if        scdes1 <> ' '
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        427390
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR101
     C*
     C  N90SCR101        IFNE      *BLANKS
     C                   MOVEL(P)  SCR101        XXKEY1
     C                   if        scdes1 <> ' '
     C     RTKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        437490
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR102
     C*
     C  N90SCR102        IFNE      *BLANKS
     C                   MOVEL(P)  SCR102        XXKEY1
     C                   if        scdes1 <> ' '
     C     RTKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        447590
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR103
     C*
     C  N90SCR103        IFNE      *BLANKS
     C                   MOVEL(P)  SCR103        XXKEY1
     C                   if        scdes1 <> ' '
     C     RTKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        457690
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR104
     C*
     C  N90SCR104        IFNE      *BLANKS
     C                   MOVEL(P)  SCR104        XXKEY1
     C                   if        scdes1 <> ' '
     C     RTKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        467790
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR105
     C*
     C  N90SCR105        IFNE      *BLANKS
     C                   MOVEL(P)  SCR105        XXKEY1
     C                   if        scdes1 <> ' '
     C     RTKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        477890
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR106
     C*
     C  N90SCR106        IFNE      *BLANKS
     C                   MOVEL(P)  SCR106        XXKEY1
     C                   if        scdes1 <> ' '
     C     RTKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        487990
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR107
     C*
     C  N90SCR107        IFNE      *BLANKS
     C                   MOVEL(P)  SCR107        XXKEY1
     C                   if        scdes1 <> ' '
     C     RTKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        498090
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR108
     C*
     C  N90SCR108        IFNE      *BLANKS
     C                   MOVEL(P)  SCR108        XXKEY1
     C                   if        scdes1 <> ' '
     C     RTKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        508190
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR109
     C*
     C  N90SCR109        IFNE      *BLANKS
     C                   MOVEL(P)  SCR109        XXKEY1
     C                   if        scdes1 <> ' '
     C     RTKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        518290
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR110
     C*
     C  N90SCR110        IFNE      *BLANKS
     C                   MOVEL(P)  SCR110        XXKEY1
     C                   if        scdes1 <> ' '
     C     RTKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        528390
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* RESERVE HIEARCHY .. AT LEAST ONE RESERVE CODE IS REQUIRED..
     C*
     C  n90              if        scdes1 <> ' '
      *
     C     SCR101        IFEQ      *BLANKS
     C     SCR102        ANDEQ     *BLANKS
     C     SCR103        ANDEQ     *BLANKS
     C     SCR104        ANDEQ     *BLANKS
     C     SCR105        ANDEQ     *BLANKS
     C     SCR106        ANDEQ     *BLANKS
     C     SCR107        ANDEQ     *BLANKS
     C     SCR108        ANDEQ     *BLANKS
     C     SCR109        ANDEQ     *BLANKS
     C     SCR110        ANDEQ     *BLANKS
      *
     C                   SETON                                        438490
      *
     C                   ENDIF
     C                   ENDIF
     C*
     C* ALAE INCLUDED - SHOULD BE 'Y' OR BLANKS                        S
     C*
     C  N90SCALA1        IFNE      'Y'
     C     SCALA1        ANDNE     *BLANKS
     C                   SETON                                        537090
     C                   ENDIF
     C*
     C*  AUTO RENEW - SHOULD BE 'Y' OR BLANKS                          S
     C*
     C  N90SCRNW1        IFNE      'Y'
     C     SCRNW1        ANDNE     *BLANKS
     C                   SETON                                        547190
     C                   ENDIF
     C*
     C* Notify user if aggregate entered differs from other divisions
     c  n90              if        not AcceptAgg(scEfD1:scAmg1)
     c                   seton                                        29  90
     c                   endif                                                  (scCFee <> *blanks)
     C*
     C  n91scEfd1        ifeq      *zeros
     C                   MOVEL     1             #OFSCR            1 0
     C                   ELSE
     C                   MOVEL     2             #OFSCR

     c                   if        cltid = '025'
     c                             and (xxbtyp = 'A' or xxbtyp = 'M')
     C                   MOVEL     3             #OFSCR
     c                   endif
     C                   ENDIF
     C  n90              IF        SCDES1 = ' '  and *in91 = *on
     C     SCPCT1        IFNE      *ZEROS
     C     SCTYP1        ORNE      *BLANKS
     C     SCAMP1        ORNE      *ZEROS
     C     SCAMG1        ORNE      *ZEROS
     C     scVarA1       ORNE      *BLANKS
     C     scADP1        ORNE      *ZEROS
     C     SCPLN1        ORNE      *BLANKS
     C     SCVRB1        ORNE      *BLANKS
     C     SCOCV1        ORNE      *BLANKS
     C     SCTOT1        ORNE      *BLANKS
     C     SCCLS1        ORNE      *ZEROS
     C     SCPLV1        ORNE      *BLANKS
     C     SCALA1        ORNE      *BLANKS
     C     SCRNW1        ORNE      *BLANKS
     C     SCR101        ORNE      *BLANKS
     C     SCR102        ORNE      *BLANKS
     C     SCR103        ORNE      *BLANKS
     C     SCR104        ORNE      *BLANKS
     C     SCR105        ORNE      *BLANKS
     C     SCR106        ORNE      *BLANKS
     C     SCR107        ORNE      *BLANKS
     C     SCR108        ORNE      *BLANKS
     C     SCR109        ORNE      *BLANKS
     C     SCR110        ORNE      *BLANKS
     C                   SETON                                        62  90
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C                   MOVEA     '00000000'    *IN(30)
     C                   MOVEA     '000'         *IN(38)
     C                   MOVEA     '00000000'    *IN(42)
     C                   MOVEA     '00000'       *IN(50)
     C                   MOVEA     '0'           *IN(88)
     C*
     C                   Z-ADD     SCFYR         XXFYR
     C*
     C     F1KY1         CHAIN     PF101                              60
     C*
     C* EDIT POLICY FROM DATE
     C*
     C                   if        *in91 = *off
     C     SCEFD2        IFNE      *ZEROS
     C                   Z-ADD     SCEFD2        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFD2
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        305590
     C                   ENDIF
     C                   ENDIF
     C*
     C  N90SCEFD2        IFNE      *ZEROS
     C                   Z-ADD     SCEFD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT1
     C*
     C     XXDAT1        IFLT      F1STPO
     C     XXDAT1        ORGE      F1ENPO
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   if        scefd2<>0 and scexp1=0
     C                   eval      scexp1=scefd2
     C                   endif
      *
      * SECOND DATE RANGE CANNOT OVERLAP FIRST DATE RANGE
      *
     C  N90              SELECT
     C                   WHEN      SCEFD2 <> 0 AND SCEXP1 = 0
     C                   SETON                                        89  90
     C                   LEAVESR
     C                   WHEN      SCEFD2 <> 0 AND SCEXP1 <> 0
     C                   Z-ADD     SCEFD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT1
     C                   Z-ADD     SCEXP1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT2
     C     XXDAT1        IFLT      XXDAT2
     C                   SETON                                        89  90
     C                   LEAVESR
     C                   ENDIF
     C                   ENDSL
     C*
     C* EDIT POLICY EXPRIRATION DATE
     C     SCEFD2        IFNE      *ZEROS
     C     SCEXP2        ANDNE     *ZEROS
     C                   Z-ADD     SCEXP2        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEXP2
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        315990
     C                   ENDIF
     C                   ENDIF
     C*
     C*  DATES MUST BE ASCENDING ORDER
     C*
     C     SCEFD2        IFNE      *ZEROS
     C     SCEXP2        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCEFD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT1
     C*
     C                   Z-ADD     SCEXP2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT2
     C*
     C  N90XXDAT2        IFLT      XXDAT1
     C                   SETON                                        305790
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  BOTH DATES MUST BE BETWEEN FUND POLICY START & END DATES
     C*
     C     SCEFD2        IFNE      *ZEROS
     C     *IN90         ANDEQ     *OFF
     C*
     C     XXDAT1        IFLT      ELPFDT
     C     XXDAT1        ORGE      ELPTDT
     C                   SETON                                        5890
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     SCEXP2        IFNE      *ZEROS
     C     *IN90         ANDEQ     *OFF
     C*
     C     XXDAT2        IFLT      ELPFDT
     C     XXDAT2        ORGT      ELPTDT
     C                   SETON                                        1690
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* EFFETIVE DATE CAN NOT BE ZERO
     C*
     C  N90SCEFD2        IFEQ      *ZEROS
     C     SCDES2        IFNE      *BLANKS
     C     SCPCT2        ORNE      *ZEROS
     C     SCTYP2        ORNE      *BLANKS
     C     SCAMP2        ORNE      *ZEROS
     C     SCAMG2        ORNE      *ZEROS
     C     scVarA2       ORNE      *BLANKS
     C     scADP2        ORNE      *ZEROS
     C     SCPLN2        ORNE      *BLANKS
     C     SCVRB2        ORNE      *BLANKS
     C     SCOCV2        ORNE      *BLANKS
     C     SCTOT2        ORNE      *BLANKS
     C     SCCLS2        ORNE      *ZEROS
     C     SCPLV2        ORNE      *BLANKS
     C     SCALA2        ORNE      *BLANKS
     C     SCR201        ORNE      *BLANKS
     C     SCR202        ORNE      *BLANKS
     C     SCR203        ORNE      *BLANKS
     C     SCR204        ORNE      *BLANKS
     C     SCR205        ORNE      *BLANKS
     C     SCR206        ORNE      *BLANKS
     C     SCR207        ORNE      *BLANKS
     C     SCR208        ORNE      *BLANKS
     C     SCR209        ORNE      *BLANKS
     C     SCR210        ORNE      *BLANKS
     C                   SETON                                        308790
     C                   ENDIF
     C                   ENDIF
     c                   endif
     C*
     C*   DESCRIPTION IS REQUIRED (IF EFFECTIVE DATE IS ENTERED)
     C*
     C                   if        scefd2 <> *zeros or
     c                             (*in91 = *on and sctyp2 <> ' ')
     C  N90SCDES2        IFEQ      *BLANKS
     C                   SETON                                        326290
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
      *
     C*   GROSS/NET FLAG SHOULD BE 'G' OR 'N'
      *
     C  N90SCTYP2        IFNE      'G'
     C     SCTYP2        ANDNE     'N'
     C     SCTYP2        ANDNE     *BLANKS
     C                   SETON                                        346390
     C                   ENDIF                                                              O
      *
     C  N90SCTYP2        IFEQ      *BLANKS
     C                   if        scefd2 <> *zeros or
     c                             (*in91 = *on and scdes2 <> ' ')
     C                   SETON                                        346490
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
      *
     C  n90              if        scdes2 <> ' '
     C                              and scAmP2 < *zero
     C                   seton                                        356590
     C                   endif                                                              O
      *
     c  n91              eval      xxco# = elco#
     c   91              eval      xxco# = rrco#
     C*   VALIDATE DEDUCTIBLE PLAN TYPE.....
      *
     C     WCFL03        IFEQ      'Y'
      *
     C  N90SCPLN2        IFNE      *BLANKS
     C                   MOVEL(P)  '  DT'        XZTBID
     C                   MOVEL(P)  SCPLN2        XXKEY
     C                   if        scdes2 <> ' '
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        377290
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
      *
     C                   ENDIF                                                  WCFL03 = 'Y'
      *
     C* PER CLAIM/PER OCCURRENCE FLAG SHOULD BE 'C' OR 'O'
      *
     C  N90SCVRB2        IFNE      'C'
     C     SCVRB2        ANDNE     'O'
     C     SCVRB2        ANDNE     *BLANKS
     C                   SETON                                        386690
     C                   ENDIF                                                              O
     C  n90              if        scdes2 <> ' 'and scvrb2 = *blanks
     C                   SETON                                        386790
     C                   ENDIF                                                              O
      *
     C*  OCCUPATIONAL DISEASE ONLY: PER CLAIM/PER OCCURRENCE FLAG
      * SHOULD BE 'C' OR 'O' OR BLANKS
      *
     C  N90SCOCV2        IFNE      'C'
     C     SCOCV2        ANDNE     'O'
     C     SCOCV2        ANDNE     *BLANKS
     C                   SETON                                        396890
     C                   ENDIF                                                              O
      *
     C*  STATISTICAL CLASS CODE IS REQUIRED....
     C*
     C  N90WCFL03        IFEQ      'Y'
     C                   if        scdes2 <> ' '
     C     SCCLS2        IFEQ      *ZEROS
     C                   SETON                                        888590
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C*USE TOTAL INCURRED FOR DEDUCTIBLE BILLING SHOULD BE 'Y' OR BLANKS
     C*
     C  N90SCTOT2        IFNE      'Y'
     C     SCTOT2        ANDNE     *BLANKS
     C                   SETON                                        406990
     C                   ENDIF
      *
     C*   VALIDATE BILLING CALULATION LEVEL.....
      *
     C  N90SCPLV2        IFNE      *BLANKS
     C                   MOVEL(P)  'BCLV'        XZTBID
     C                   MOVEL(P)  SCPLV2        XXKEY
     C                   if        scdes2 <> ' '
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        427390
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR201
     C*
     C  N90SCR201        IFNE      *BLANKS
     C                   MOVEL(P)  SCR201        XXKEY1
     C                   if        scdes2 <> ' '
     C     RTKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        437490
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR202
     C*
     C  N90SCR202        IFNE      *BLANKS
     C                   MOVEL(P)  SCR202        XXKEY1
     C                   if        scdes2 <> ' '
     C     RTKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        447590
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR203
     C*
     C  N90SCR203        IFNE      *BLANKS
     C                   MOVEL(P)  SCR203        XXKEY1
     C                   if        scdes2 <> ' '
     C     RTKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        457690
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR204
     C*
     C  N90SCR204        IFNE      *BLANKS
     C                   MOVEL(P)  SCR204        XXKEY1
     C                   if        scdes2 <> ' '
     C     RTKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        467790
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR205
     C*
     C  N90SCR205        IFNE      *BLANKS
     C                   MOVEL(P)  SCR205        XXKEY1
     C                   if        scdes2 <> ' '
     C     RTKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        477890
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR206
     C*
     C  N90SCR206        IFNE      *BLANKS
     C                   MOVEL(P)  SCR206        XXKEY1
     C                   if        scdes2 <> ' '
     C     RTKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        487990
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR207
     C*
     C  N90SCR207        IFNE      *BLANKS
     C                   MOVEL(P)  SCR207        XXKEY1
     C                   if        scdes2 <> ' '
     C     RTKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        498090
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR208
     C*
     C  N90SCR208        IFNE      *BLANKS
     C                   MOVEL(P)  SCR208        XXKEY1
     C                   if        scdes2 <> ' '
     C     RTKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        508190
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR209
     C*
     C  N90SCR209        IFNE      *BLANKS
     C                   MOVEL(P)  SCR209        XXKEY1
     C                   if        scdes2 <> ' '
     C     RTKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        518290
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR210
     C*
     C  N90SCR210        IFNE      *BLANKS
     C                   MOVEL(P)  SCR210        XXKEY1
     C                   if        scdes2 <> ' '
     C     RTKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        528390
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* RESERVE HIEARCHY .. AT LEAST ONE RESERVE CODE IS REQUIRED..
     C*
     C  n90              if        scdes2 <> ' '
      *
     C     SCR201        IFEQ      *BLANKS
     C     SCR202        ANDEQ     *BLANKS
     C     SCR203        ANDEQ     *BLANKS
     C     SCR204        ANDEQ     *BLANKS
     C     SCR205        ANDEQ     *BLANKS
     C     SCR206        ANDEQ     *BLANKS
     C     SCR207        ANDEQ     *BLANKS
     C     SCR208        ANDEQ     *BLANKS
     C     SCR209        ANDEQ     *BLANKS
     C     SCR210        ANDEQ     *BLANKS
      *
     C                   SETON                                        438490
      *
     C                   ENDIF
     C                   ENDIF
     C*
     C* ALAE INCLUDED - SHOULD BE 'Y' OR BLANKS                        S
     C*
     C  N90SCALA2        IFNE      'Y'
     C     SCALA2        ANDNE     *BLANKS
     C                   SETON                                        537090
     C                   ENDIF
     C*
     C*  AUTO RENEW - SHOULD BE 'Y' OR BLANKS                          S
     C*
     C* Notify user if aggregate entered differs from other divisions
     c  n90              if        not AcceptAgg(scEfD2:scAmg2)
     c                   seton                                        29  90
     c                   endif                                                  (scCFee <> *blanks)
     C* Description can not be blanks
     C*
     C  n90              if        scdes2 = ' ' and *in91 = *on
     C     SCPCT2        IFNE      *zeros
     C     SCTYP2        ORNE      *BLANKS
     C     SCAMP2        ORNE      *ZEROS
     C     SCAMG2        ORNE      *ZEROS
     C     scVarA2       ORNE      *BLANKS
     C     scADP2        ORNE      *ZEROS
     C     SCPLN2        ORNE      *BLANKS
     C     SCVRB2        ORNE      *BLANKS
     C     SCOCV2        ORNE      *BLANKS
     C     SCTOT2        ORNE      *BLANKS
     C     SCCLS2        ORNE      *ZEROS
     C     SCPLV2        ORNE      *BLANKS
     C     SCALA2        ORNE      *BLANKS
     C     SCR201        ORNE      *BLANKS
     C     SCR202        ORNE      *BLANKS
     C     SCR203        ORNE      *BLANKS
     C     SCR204        ORNE      *BLANKS
     C     SCR205        ORNE      *BLANKS
     C     SCR206        ORNE      *BLANKS
     C     SCR207        ORNE      *BLANKS
     C     SCR208        ORNE      *BLANKS
     C     SCR209        ORNE      *BLANKS
     C     SCR210        ORNE      *BLANKS
     C                   SETON                                        306290
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*===============================================================
     C*
     C     ERRCK3        BEGSR
     C*
     C*  Claims liability rate must be greater than zero for TASB 'A' and 'M' plans.
     C*
     C  N90SCLIRT        IFLT      *ZERO
     C     *IN94         ANDEQ     *ON
     C                   SETON                                        96  90
     C                   ENDIF
     C*
     C*
     C*  Claims allowance amount must be greater than zero for TASB 'M' plans.
     C*
     C  N90SCCLAL        IFLT      *ZERO
     C     *IN95         ANDEQ     *ON
     C                   SETON                                        97  90
     C                   ENDIF
     C*
     C                   ENDSR                                                  ERRCK3
     C*
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=TABLE SEARCH
     C*
     C     KJDO          BEGSR
     C*
     C                   MOVEA     '00000000'    *IN(43)
     C                   MOVEA     '00'          *IN(51)
     C                   SETOFF                                       3742
     C*
     C                   Z-ADD     SCEFD1        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCEFD1
     C                   ENDIF
     C*
     C                   Z-ADD     SCEXP1        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCEXP1
     C                   ENDIF
     C*
     C                   Z-ADD     SCEFD2        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCEFD2
     C                   ENDIF
     C*
     C                   Z-ADD     SCEXP2        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCEXP2
     C                   ENDIF
     C*
     C                   SELECT
     C*
     C*  DEDUCTIBLE PLAN TYPE
     C     HFLD          WHENEQ    'SCPLN1'
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM      '2'           QQCALL            1
     C                   PARM      '  DT'        QQID              4
     C                   PARM      *BLANKS       QQKEY            10
     C                   PARM                    QQRTCD            1
     C*
     C     QQKEY         IFNE      *BLANKS
     C                   SETON                                        38
     C                   MOVEL     QQKEY         SCPLN1
     C                   ELSE
     C                   SETON                                        37
     C                   ENDIF
     C*
     C*  BILLING CALCULATION LEVEL
     C*
     C     HFLD          WHENEQ    'SCPLV1'
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM      '2'           QQCALL            1
     C                   PARM      'BCLV'        QQID              4
     C                   PARM      *BLANKS       QQKEY            10
     C                   PARM                    QQRTCD            1
     C*
     C     QQKEY         IFNE      *BLANKS
     C                   MOVEL     QQKEY         SCPLV1
     C                   SETON                                        43
     C                   ELSE
     C                   SETON                                        42
     C                   ENDIF
      *
     C* SEARCH RESERVE CODES SCR101
     C     HFLD          WHENEQ    'SCR101'
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCR101
     C                   SETON                                        44
     C                   ELSE
     C                   SETON                                        43
     C                   ENDIF
      *
     C* SEARCH RESERVE CODES SCR102
      *
     C     HFLD          WHENEQ    'SCR102'
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCR102
     C                   SETON                                        45
     C                   ELSE
     C                   SETON                                        44
     C                   ENDIF
      *
     C* SEARCH RESERVE CODES SCR103
      *
     C     HFLD          WHENEQ    'SCR103'
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCR103
     C                   SETON                                        46
     C                   ELSE
     C                   SETON                                        45
     C                   ENDIF
      *
     C* SEARCH RESERVE CODES SCR104
      *
     C     HFLD          WHENEQ    'SCR104'
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCR104
     C                   SETON                                        47
     C                   ELSE
     C                   SETON                                        46
     C                   ENDIF
     C*
      *
     C* SEARCH RESERVE CODES SCR105
      *
     C     HFLD          WHENEQ    'SCR105'
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCR105
     C                   SETON                                        48
     C                   ELSE
     C                   SETON                                        47
     C                   ENDIF
     C*
      *
     C* SEARCH RESERVE CODES SCR106
      *
     C     HFLD          WHENEQ    'SCR106'
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCR106
     C                   SETON                                        49
     C                   ELSE
     C                   SETON                                        48
     C                   ENDIF
      *
     C* SEARCH RESERVE CODES SCR107
      *
     C     HFLD          WHENEQ    'SCR107'
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCR107
     C                   SETON                                        50
     C                   ELSE
     C                   SETON                                        49
     C                   ENDIF
      *
     C* SEARCH RESERVE CODES SCR108
      *
     C     HFLD          WHENEQ    'SCR108'
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCR108
     C                   SETON                                        51
     C                   ELSE
     C                   SETON                                        50
     C                   ENDIF
      *
     C* SEARCH RESERVE CODES SCR109
      *
     C     HFLD          WHENEQ    'SCR109'
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCR109
     C                   SETON                                        52
     C                   ELSE
     C                   SETON                                        51
     C                   ENDIF
      *
     C* SEARCH RESERVE CODES SCR110
      *
     C     HFLD          WHENEQ    'SCR110'
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCR110
     C                   SETON                                        53
     C                   ELSE
     C                   SETON                                        52
     C                   ENDIF
     C*
     C*  DEDUCTIBLE PLAN TYPE SCPLN2
     C*
     C     HFLD          WHENEQ    'SCPLN2'
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM      '2'           QQCALL            1
     C                   PARM      '  DT'        QQID              4
     C                   PARM      *BLANKS       QQKEY            10
     C                   PARM                    QQRTCD            1
     C*
     C     QQKEY         IFNE      *BLANKS
     C                   MOVEL     QQKEY         SCPLN2
     C                   SETON                                        38
     C                   ELSE
     C                   SETON                                        37
     C                   ENDIF
     C*
     C*  BILLING CALCULATION LEVEL
     C*
     C     HFLD          WHENEQ    'SCPLV2'
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM      '2'           QQCALL            1
     C                   PARM      'BCLV'        QQID              4
     C                   PARM      *BLANKS       QQKEY            10
     C                   PARM                    QQRTCD            1
     C*
     C     QQKEY         IFNE      *BLANKS
     C                   MOVEL     QQKEY         SCPLV2
     C                   SETON                                        43
     C                   ELSE
     C                   SETON                                        42
     C                   ENDIF
      *
     C* SEARCH RESERVE CODES SCR201
     C     HFLD          WHENEQ    'SCR201'
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCR201
     C                   SETON                                        44
     C                   ELSE
     C                   SETON                                        43
     C                   ENDIF
     C*
     C* SEARCH RESERVE CODES SCR102
     C*
     C     HFLD          WHENEQ    'SCR202'
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCR202
     C                   SETON                                        45
     C                   ELSE
     C                   SETON                                        44
     C                   ENDIF
      *
     C* SEARCH RESERVE CODES SCR103
      *
     C     HFLD          WHENEQ    'SCR203'
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCR203
     C                   SETON                                        46
     C                   ELSE
     C                   SETON                                        45
     C                   ENDIF
      *
     C* SEARCH RESERVE CODES SCR204
      *
     C     HFLD          WHENEQ    'SCR204'
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCR204
     C                   SETON                                        47
     C                   ELSE
     C                   SETON                                        46
     C                   ENDIF
     C*
     C* SEARCH RESERVE CODES SCR205
     C*
     C     HFLD          WHENEQ    'SCR205'
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCR205
     C                   SETON                                        48
     C                   ELSE
     C                   SETON                                        47
     C                   ENDIF
     C*
     C* SEARCH RESERVE CODES SCR206
     C*
     C     HFLD          WHENEQ    'SCR206'
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCR206
     C                   SETON                                        49
     C                   ELSE
     C                   SETON                                        48
     C                   ENDIF
      *
     C* SEARCH RESERVE CODES SCR207
      *
     C     HFLD          WHENEQ    'SCR207'
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCR207
     C                   SETON                                        50
     C                   ELSE
     C                   SETON                                        49
     C                   ENDIF
      *
     C* SEARCH RESERVE CODES SCR208
      *
     C     HFLD          WHENEQ    'SCR208'
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCR208
     C                   SETON                                        51
     C                   ELSE
     C                   SETON                                        50
     C                   ENDIF
      *
     C* SEARCH RESERVE CODES SCR209
      *
     C     HFLD          WHENEQ    'SCR209'
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCR209
     C                   SETON                                        52
     C                   ELSE
     C                   SETON                                        51
     C                   ENDIF
      *
     C* SEARCH RESERVE CODES SCR210
      *
     C     HFLD          WHENEQ    'SCR210'
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCR210
     C                   SETON                                        53
     C                   ELSE
     C                   SETON                                        52
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  KJDO
     C*===============================================================
     C* Subroutine for F20
     C*===============================================================
     C     kudo          begsr
     C*
     C*
     c                   eval      clear  = *on
     C                   exfmt     kuwdw
     C*
     C                   if        not *inkl
     C                   clear                   dbrcdf
     C                   exsr      MvDbSc
     C                   endif
     C*
     C                   endsr                                                  KJDO
     C*===============================================================
     C* Subroutine to Update All Policies/Divisions.
     C*===============================================================
     c     kvsbm         begsr
     c                   setoff                                       98
     c                   exfmt     kvwdw

     c     *in90         doueq     *off
     c                   setoff                                       98
     c     *inkl         ifne      *on
     c     *inkg         andne     *on
     c                   seton                                        98  90
     c                   exfmt     kvwdw
     c   98              iter
     c                   endif
     c  n98              leave
     c                   enddo
      *
     c                   if        *inkg
     c                   eval      submit = *on
     c                   endif
      *
     c* If running an individual policy.
     c                   if        (not *inkl) and
     c                             (*in91 = *off and submit = *on)
     c                   move      @@rrn2        qqrrn                          wdelp
     c                   eval      qqmode = 'E'
     c                   eval      qqupdate = 'Y'
     C                   exsr      Updateall
     c                   endif

     c* If running for a group setup.
     c                   if        not *inkl and
     c                             (*in91 = *on and submit = *on)
     c                   move      @@rrn2        qqrrn                          wdrrp
     c                   eval      qqmode = 'G'
     c                   eval      qqupdate = 'Y'
     C                   exsr      Updateall
     c                   endif
     c                   endsr                                                  kvsbm
     C*===============================================================
      *
     C*===============================================================
     C     Updateall     begsr
     c                   call      'W7138C'
     c                   parm                    qqrrn             9
     c                   parm                    qqmode            1
     c                   parm                    qqupdate          1
     C                   endsr
     C*===============================================================
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C* Check the balance in Dedcutible billing
     C
     C                   Movel     'F'           XXARTY
     C                   Z-Add     0             xtotalDedB
     C
     C     EAKY          Chain     PEA01                              60
     C                   Dow       *in60 = *off
     C                   movel(p)  eaarcd        xxkey
     C
     C     tbkey         chain     ptt01
     C                   If        *in60 = *off
     C                   if        ttctyp = 'R'
     C
     C                   Eval      xtotalDedB = xtotalDedB + EAAMT
     C                   endif
     C                   endif
     C
     C     EAKY          Reade     PEA01                                  60
     C                   enddo
     C*
     c                   If         xtotalDedB <> 0
     c                   Eval       *in26 = *on
     c                   Endif
     C     LDEFD1        IFEQ      *ZEROS
     C     @@MODE        ANDEQ     'D'
     C                   MOVEL     1             #OFSCR            1 0
     C                   ELSE
     C                   MOVEL     2             #OFSCR
     c                   if        cltid = '025'
     c                             and (xxbtyp = 'A' or xxbtyp = 'M')
     C                   MOVEL     3             #OFSCR
     c                   endif
     C                   ENDIF
      *
     c                   if        *in91  = *off  and ldemp# = 0 and addMode
     c                             and clear = *off
     c                   eval      xxco#  = elco#
     c                   eval      xxfnd  = elfnd
     c                   eval      xxefdt = elpfdt
     c     rsky          chain     prs01                              60
     c  n60              movel     rsjrst        xxst

     c                   eval      x@emp# = 0
     C     ldky4         setgt     pld04
     C     ldky5         readpe    pld04                                  60
     c                   if        *in60 = *off and lddes1 <> ' '
     C                   z-add     elpfdt        ldefd1
     c                   if        LDDES2  <>  ' '
     C                   z-add     elpfdt        ldefd2
     c                   endif
     C                   z-add     *zeros        ldexp1
     C                   z-add     *zeros        ldexp2
     c                   endif
     c                   endif
     C* SCREEN #1 FIELDS
     C                   Z-ADD     LDEFD1        SRDTI
     C                   Z-ADD     SRDTI         X1PFDT            7 0
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFD1
      *
     C                   Z-ADD     LDEXP1        SRDTI
     C                   Z-ADD     SRDTI         X1PFDT            7 0
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXP1
      *
     C                   MOVEL     LDDES1        SCDES1
     C                   MOVEL     LDPCT1        SCPCT1
     C                   MOVEL     LDTYP1        SCTYP1
     C                   MOVE      LDAMP1        SCAMP1
     C                   MOVE      LDAMG1        SCAMG1
     C                   MOVE      LDaDP1        scADP1
     C                   MOVE      LDvarA1       scVarA1
     C                   MOVEL     LDPLN1        SCPLN1
     C                   MOVE      LDVRB1        SCVRB1
     C                   MOVE      LDOCV1        SCOCV1
     C                   MOVE      LDTOT1        SCTOT1
     C                   MOVE      LDCLS1        SCCLS1
     C                   MOVE      LDPLV1        SCPLV1
     C                   MOVE      LDTOT1        SCTOT1
     C                   MOVEL     LDALA1        SCALA1
     C                   MOVEL     LDRNW1        SCRNW1
     C*
     C                   MOVE      LDR101        SCR101
     C                   MOVE      LDR102        SCR102
     C                   MOVE      LDR103        SCR103
     C                   MOVE      LDR104        SCR104
     C                   MOVE      LDR105        SCR105
     C                   MOVE      LDR106        SCR106
     C                   MOVE      LDR107        SCR107
     C                   MOVE      LDR108        SCR108
     C                   MOVE      LDR109        SCR109
     C                   MOVE      LDR110        SCR110
     C*
     C* SCREEN #2 FIELDS
     C*
     C                   Z-ADD     LDEFD2        SRDTI
     C                   Z-ADD     SRDTI         X1PFDT            7 0
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFD2
      *
     C                   Z-ADD     LDEXP2        SRDTI
     C                   Z-ADD     SRDTI         X1PFDT            7 0
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXP2
      *
     C                   MOVEL     LDDES2        SCDES2
     C                   MOVE      LDPCT2        SCPCT2
     C                   MOVEL     LDTYP2        SCTYP2
     C                   MOVE      LDAMP2        SCAMP2
     C                   MOVE      LDAMG2        SCAMG2
     C                   MOVE      LDaDP2        scADP2
     C                   MOVE      LDvarA2       scVarA2
     C                   MOVEL     LDPLN2        SCPLN2
     C                   MOVE      LDVRB2        SCVRB2
     C                   MOVE      LDOCV2        SCOCV2
     C                   MOVE      LDTOT2        SCTOT2
     C                   MOVE      LDCLS2        SCCLS2
     C                   MOVE      LDPLV2        SCPLV2
     C                   MOVE      LDTOT2        SCTOT2
     C                   MOVEL     LDALA2        SCALA2
     C*
     C                   MOVE      LDR201        SCR201
     C                   MOVE      LDR202        SCR202
     C                   MOVE      LDR203        SCR203
     C                   MOVE      LDR204        SCR204
     C                   MOVE      LDR205        SCR205
     C                   MOVE      LDR206        SCR206
     C                   MOVE      LDR207        SCR207
     C                   MOVE      LDR208        SCR208
     C                   MOVE      LDR209        SCR209
     C                   MOVE      LDR210        SCR210
     C*
     c                   if        addMode and cltid = '025' and ELQTYP = 'RD'
     c                   eval      scrnw1 = 'Y'
     c                   endif
     C*
     C* Screen #3 fields
     C                   eval      scLiRt = LDLIRT
     C                   eval      scClAl = LDCLAL
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C* SCREEN #1 FIELDS
     c     @@rrn2        chain     wdrrp                              60
     c                   if        *in60 = *off and *in91 = *on
     c                   eval      ldemp# = 0
     c                   eval      lddiv  = 0
     c                   eval      ldfnd  = 0
     c                   eval      ldco#  = rrco#
     c                   eval      ldfnd  = rrfnd
     c                   eval      ldst   = rrst
     C                   z-add     scEfdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         ldefdt
     c                   endif
     C  n91              MOVEL     ELCO#         LDCO#
     C  n91              MOVEL     ELFND         LDFND
     C  n91              MOVEL     ELEMP#        LDEMP#
     C  n91              MOVEL     ELDIV         LDDIV
     C  n91              MOVEL     ELFYR         LDFYR
      *
     C                   Z-ADD     SCEFD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LDEFD1
      *
     C                   Z-ADD     SCEXP1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LDEXP1
      *
     C                   MOVEL     SCDES1        LDDES1
     C                   MOVEL     SCPCT1        LDPCT1
     C                   MOVEL     SCTYP1        LDTYP1
     C                   MOVE      SCAMP1        LDAMP1
     C                   MOVE      SCAMG1        LDAMG1
     C                   MOVE      scADP1        LDaDP1
     C                   MOVE      scVarA1       LDvarA1
     C                   MOVEL     SCPLN1        LDPLN1
     C                   MOVE      SCVRB1        LDVRB1
     C                   MOVE      SCOCV1        LDOCV1
     C                   MOVE      SCTOT1        LDTOT1
     C                   MOVE      SCCLS1        LDCLS1
     C                   MOVE      SCPLV1        LDPLV1
     C                   MOVE      SCTOT1        LDTOT1
     C                   MOVEL     SCALA1        LDALA1
     C                   MOVEL     SCRNW1        LDRNW1
     C*
     C                   MOVE      SCR101        LDR101
     C                   MOVE      SCR102        LDR102
     C                   MOVE      SCR103        LDR103
     C                   MOVE      SCR104        LDR104
     C                   MOVE      SCR105        LDR105
     C                   MOVE      SCR106        LDR106
     C                   MOVE      SCR107        LDR107
     C                   MOVE      SCR108        LDR108
     C                   MOVE      SCR109        LDR109
     C                   MOVE      SCR110        LDR110
     C*
     C* SCREEN #2 FIELDS
     C*
     c                   if        *in60 = *off and *in91 = *on
     c                   eval      ldemp# = 0
     c                   eval      lddiv  = 0
     c                   eval      ldfnd  = 0
     c                   eval      ldco#  = rrco#
     c                   eval      ldfnd  = rrfnd
     c                   eval      ldst   = rrst
     c                   endif
     C  n91              movel     elco#         ldco#
     C  n91              movel     elfnd         ldfnd
     C  n91              movel     elemp#        ldemp#
     C  n91              movel     eldiv         lddiv
     C  n91              movel     elfyr         ldfyr
      *
     C*                    MOVE SCEFD2    LDEFD2
      *
     C                   Z-ADD     SCEFD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LDEFD2
      *
     C                   Z-ADD     SCEXP2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LDEXP2
      *
     C                   MOVEL     SCDES2        LDDES2
     C                   MOVE      SCPCT2        LDPCT2
     C                   MOVEL     SCTYP2        LDTYP2
     C                   MOVE      SCAMP2        LDAMP2
     C                   MOVE      SCAMG2        LDAMG2
     C                   MOVE      scADP2        LDaDP2
     C                   MOVE      scVarA2       LDvarA2
     C                   MOVEL     SCPLN2        LDPLN2
     C                   MOVE      SCVRB2        LDVRB2
     C                   MOVE      SCOCV2        LDOCV2
     C                   MOVE      SCTOT2        LDTOT2
     C                   MOVE      SCCLS2        LDCLS2
     C                   MOVE      SCPLV2        LDPLV2
     C                   MOVE      SCTOT2        LDTOT2
     C                   MOVE      SCALA2        LDALA2
     C*
     C                   MOVE      SCR201        LDR201
     C                   MOVE      SCR202        LDR202
     C                   MOVE      SCR203        LDR203
     C                   MOVE      SCR204        LDR204
     C                   MOVE      SCR205        LDR205
     C                   MOVE      SCR206        LDR206
     C                   MOVE      SCR207        LDR207
     C                   MOVE      SCR208        LDR208
     C                   MOVE      SCR209        LDR209
     C                   MOVE      SCR210        LDR210
     C*
     C* Screen #3 fields
     C                   eval      LDLIRT = scLiRt
     C                   eval      LDCLAL = scClAl
     C*
     C                   ENDSR                                                  MVSCDB
     C*
     C*===============================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*===============================================================
     C*
     C     OTHERR        BEGSR
     C                   ENDSR                                                   OTHERR
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C     *inku         caseq     *on           kudo                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     c*
       //** beg del ***** 07/06/22 *********************
     c***                   dow       scupd <> 'Y' and scupd <> 'N' and *inkl=*off
     c***                             and *inkc =*off and *in92=*off and
     c***                             (##qte<>'Q' and (*in91=*on or sc#div>1) or
     c***                             ##qte='Q' and not prEmpExist)
      ***
     c***                   exfmt     updall
      ****
     C***                   if        scupd <> 'Y' and scupd <> 'N'
     C***                   eval      *in98 = *on
     C***                   endif
      ****
     C***                   enddo
      ****
     C***                   if        scupd = 'Y'
     C***                   exsr      kvsbm
     C***                   endif                                                  SCOVER = Y
      ****
      ****
     c***                   setoff                                       98
       //** end del ***** 07/06/22 *********************

     c                   if        %open(wmldl4)
     c                   close     wmldl4
     c                   endif
      *
     c                   if        ##qte = 'Q'
      *
     c                   if        %open(wmemlq)
     c                   close     wmemlq
     c                   endif
      *

     c*   Delete Override
     c                   eval      cmd = 'dltovr file(wmemlq)'
     c                   callp     runcmd(cmd:%len(cmd))
     c                   eval      cmd = 'dltovr file(wmldl4)'
     c                   callp     runcmd(cmd:%len(cmd))
      *
     c                   endif                                                  ##qte = 'Q'
      *
     C                   MOVE      *ON           *INLR
     C                   ENDSR
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      /Free
       VisibAggrP  = *off;
       VisibVarA   = *off;
       If cltId = '150';
          VisibAggrP = *on;
          VisibVarA = *on;
       Endif;
      /End-free
     c                   select
     c                   when       @@mode = 'A'
     c                   eval       addMode = *on
     c                   when       @@mode = 'C'
     c                   eval       chgMode = *on
     c                   when       @@mode = 'D'
     c                   eval       dspMode = *on
     C***  Begin Add  ***  01/24/18  **************************************
     c                   when       @@mode = 'P'
     c                   eval       addMode = *on
     C***  End   Add  ***  01/24/18  **************************************
     c                   other
     c                   seton                                        lr
     c                   return
     c                   endsl
     c                   if        %parms >= 7
     c                   move      @@qte         ##qte             1
     c                   endif

     c                   if        ##qte = 'Q'
     c                   callp     rtvenvlib('*OTHER':lib:modlib)
     c                   eval      cmd = 'OVRDBF FILE(WMEMLQ) TOFILE('
     c                                 + %trim(lib) + '/WMEMLQ) SHARE(*YES)'
     c                   callp     runcmd(cmd:%len(cmd))
     c                   eval      cmd = 'ovrdbf file(wmldl4) tofile('
     c                                 + %trim(lib) + '/wmldl4) SHARE(*YES)'
     c                   callp     runcmd(cmd:%len(cmd))
      *
     c                   if        not %open(wmemlq)
     c                   open      wmemlq
     c                   endif
      *
     c                   endif                                                  ##qte = 'Q'
      *
     c                   if        not %open(wmldl4)
     c                   open      wmldl4
     c                   endif
     C                   Z-ADD     0             I1                3 0
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     c     @@rrn         chain     wmldp                              60
     c     @@rrn2        chain     wdrrp                              60
     c                   if        @1mode = 'G'
     c                   eval      *in91 = *on
     c  n60              eval      scst   = rrst
     c  n60              eval      scfnd  = rrfnd
     C  n60              z-add     rrefdt        srdti
     C  n60              call      'SRCVTDAT'    DTMDYY
     C  n60              movel     srdto         scefdt
     c                   else
     c                   eval      *in91 = *off
     c                   endif

     c                   if        *in91 = *off
     C*
     C*  GET EMPLOYER DETAIL RECORD
     C*
     C     @@RRN2        CHAIN     WDELP                              60
     C*
     C                   Z-ADD     ELFYR         SCFYR
     C*
     C* Check the balance in Dedcutible billing
     C
     C                   Movel     'F'           XXARTY
     C                   Z-Add     0             xtotalDedB
     C
     C     EAKY          Chain     PEA01                              60
     C                   Dow       *in60 = *off
     C                   movel(p)  eaarcd        xxkey
     C
     C     tbkey         chain     ptt01
     C                   If        *in60 = *off
     C                   if        ttctyp = 'R'
     C
     C                   Eval      xtotalDedB = xtotalDedB + EAAMT
     C                   endif
     C                   endif
     C
     C     EAKY          Reade     PEA01                                  60
     C                   enddo
     C*
     c                   If         xtotalDedB <> 0
     c                   Eval       *in26 = *on
     c                   Endif
     C*  GET EMPLOYER MASTER RECORD
     C*
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
     C*
     C*  COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL     XXEDN         SCEDN
     C*
     C                   MOVE      *OFF          *IN41
     C                   Z-ADD     0             SCCXDT
     C                   Z-ADD     1             I1
     C*
     C*  GET MOST RECENT CANCELLATION DATE, IF ANY
     C*
     C                   Z-ADD     *ZERO         XXCXDT
     C*
     C     *ZERO         LOOKUP    EF(I1)                                 61
     C*
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
     C*
     C     I1            IFNE      0
     C*
     C     CX(I1)        IFNE      *ZERO
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   MOVE      *ON           *IN41
     C                   Z-ADD     CX(I1)        XXCXDT
     C                   ENDIF                                                  CX,I1 IFNE *ZERO
     C*
     C                   ENDIF                                                  I1 IFNE 0
     C*
     C*  GET FUND MASTER INFORMATION
     C*
     C                   MOVEL     *BLANKS       SCNAME
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL     FNNAME        SCNAME
     c                   if        emstrp <> 'Y'
     c                   eval      *in92 = *on
     c                   endif
     C*
     C     LDKY          SETGT     PLD01
     C     LDKY          READPE    PLD01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      @@RRN1        @@RRN
     C                   MOVE      '1'           CPYFLG
     C                   ELSE
     C                   MOVE      *ZERO         @@RRN1
     C                   MOVE      *ZERO         @@RRN
     C                   MOVE      '0'           CPYFLG
     C                   ENDIF
     C*
     c                   move      *blank        xxbtyp            1
     c                   movea     '00'          *in(94)

     c                   if        cltid = '025'
     C*
     C                   MOVEL     'PTYP'        XZTBID
     C                   MOVEL(P)  ELQTYP        XXKEY
     C     GTKY          CHAIN     PGT01                              60

     c                   if        *in60 = *off
     c                   movel     gtfg01        xxbtyp
     c
     c                   if        xxbtyp = 'A' or xxbtyp = 'M'
     c                   move      *on           *in94
     c                   endif
     c
     c                   if        xxbtyp = 'M'
     c                   move      *on           *in95
     c                   endif
     c
     c                   endif

     c                   endif                                                  if cltid = '025'
     c                   endif                                                  if *in91 = *off

     c     fnky1         chain     pfn01                              60
     c  n60              if        fnpeof = ' ' and *in91 = *on
     c                   eval      *in92 = *on
     c                   endif

     c                   call      'SA030R'
     c                   parm                    suffix            3
     c                   eval      sclib  = 'PDSRPT' + suffix

     C     emky2         chain(n)  pem01                              60
     C                   z-add     0             sc#div            5 0
     C     *in60         doweq     *off
     C                   ADD       1             sc#div
     C     EMKY2         reade     pem01                                  60
     C                   enddo
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*===============================================================
     C*
     C     PREOTH        BEGSR
     C                   ENDSR
     C*
     C*===============================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*===============================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        LDUSER
     C                   MOVEL     Q1PGM         LDPGM
     C                   TIME                    DSDTTM
     C                   MOVEL     DSTIME        LDTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LDDT
     C                   update    dbRcdF
     C                   MOVEL     DBRRN         @@RRN
     C                   ELSE                                                   *IN99=*ON
     C     scdes1        ifne      *zeros
     C                   EXSR      WRITER
     C                   ENDIF
     C                   MOVEL     DBRRN         @@RRN
     C                   ENDIF
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*===============================================================
     C* WRITER - SUBROUTINE FOR WRITEING DATA BASE RECORDS
     C*===============================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        LDUSER
     C                   MOVEL     Q1USER        LDIUSR
     C                   MOVEL     Q1PGM         LDPGM
     C                   MOVEL     Q1PGM         LDIPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVEL     DSTIME        LDTIME
     C                   MOVEL     DSTIME        LDITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LDDT
     C                   MOVEL     SRDTO         LDIDT
     C*
     C     scdes1        IFNE      *blanks
     C     addmode       oreq      *on
     c     clear         andeq     *on
     C                   WRITE     DBRCDF
     C                   ENDIF
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  WRITER

    P*================================================================
    P* Clear associated info
    P*================================================================
     PAcceptAgg        b
     DAcceptAgg        pi             1n
     D inEffDt                             like(scEfD1)
     D inAgg                               like(LDAMG1)
     D*
     D effDt7          s                   like(LDEFD1)
     D compareAgg      s                   like(LDAMG1)
     D accepted        s              1n   inz('0')

     C     keyWMLDL2     klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    ELFYR
     C                   kfld                    ELEMP#

     C                   z-add     inEffDt       srDtI
     C                   call      'SRCVTDAT'    DtCYMD
     C                   movel     srDtO         effDt7

     c     keyWMLDL2     setll     wmldl2
     c                   dou       %eof
     c     keyWMLDL2     reade     wmldl2
     c                   if        not %eof
     c                   if        LDDIV = ELDIV
     c                   iter
     c                   endif
     c                   exsr      GetAggregate
     c                   if        inAgg <> compareAgg
     c                   eval      scAgg1   = inAgg
     c                   eval      scAgg2   = compareAgg
     c                   eval      scAggDiv = LDDIV
     C                   exfmt     AggWdw
     C                   if        *inkg
     C                   eval      accepted = *on
     C                   endif
     c                   leave
     C                   endif
     C                   endif
     C                   enddo

     c                   if        %eof
     C                   eval      accepted = *on
     c                   endif

     C                   return    accepted

    C*================================================================
    C* Determine comparison aggregate
    C*================================================================
     C     getAggregate  begsr

     c                   select
     c                   when      effDt7 >= LDEFD1
     c                              and (effDt7 <= LDEXP1 or LDEXP1 = *zero)
     c                   eval      compareAgg = LDAMG1
     c                   when      effDt7 >= LDEFD2
     c                              and (effDt7 <= LDEXP2 or LDEXP2 = *zero)
     c                   eval      compareAgg = LDAMG2
     c                   other
     c                   clear                   compareAgg
     c                   endsl

     C                   endsr

     PAcceptAgg        e
    P*================================================================
    P* Check Status
    P*================================================================
     PprEmpexist       b
     DprEmpexist       pi             1n

     C                   z-add     1             scemp#
     C     keyWMEPL      klist
     C                   kfld                    xxCo#
     C                   kfld                    scFnd
     C                   kfld                    scEmp#
     C                   kfld                    scDiv

     c                   if        fnpeof <> ' ' and emstrp = 'Y'
     c     keyWMEPL      chain     wmemlq
     c                   if        %found
     c                   return    *on
     c                   endif
     c                   endif
     c                   return    *off

     PprEmpexist       e
