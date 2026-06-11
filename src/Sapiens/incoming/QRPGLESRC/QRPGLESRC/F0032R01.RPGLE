     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/16/15 **************************************
     H BNDDIR('STBNDDIR')
     H***** End Add *** 06/16/15 **************************************
     H*****************************************************************
     H/TITLE F0032R01 - Provider Master Maintenance
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**   WCMC - Writes/Updates Provider Master records.            **
     H**                                                             **
     H** Called From: F0032R                                         **
     H**                                                             **
     H** Calls To:  F0601C                                           **
     H**            F0601R                                           **
     H**            F2380R                                           **
     H**            F2335R                                           **
     H**            F2355R                                           **
     H**            F2360R                                           **
     H**            F2380R                                           **
     H**            SA001R                                           **
     H**            SRCVTDAT                                         **
     H**            SY600R                                           **
     H**                                                             **
     H**   PROGRAMMER: C:61                                          **
     H**   CREATED:   09/18/97      PROJECT: 87MAU                   **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                               *
     H*  02/09/18 C:180 PROJECT: 114MJC                               *
     H*                 Add 835 Payment advice check box.             *
     H*                                                               *
     H*  02/11/16 C:307 SUPPORT:37159                                 *
     H*                 To subset non adjudication state list and     *
     H*                 not to show in provider bill and work with    *
     H*                 medical cases.                                *
     H*                                                               *
     H*  06/16/15  C:309 PHW34136                                     *
     H*                  Add check on NPI# so that it can contain only*
     H*                  numeric values                               *
     H*                                                               *
     H* 07/25/13 C:277 SUPPORT: 82949   MC133                         *
     H*                Change the edit for Payee Type when MC is Activ*
     H*                                                               *
     H* 05/25/12 C:282 Project: WC2109                                *
     H*                Added Claim Contacts email field               *
     H*                                                               *
     H* 03/14/11 C:257 SUPPORT:76168                                  *
     H*                Taxonomy Filed should be present always and    *
     H*                should not depend on NCCI data call flag being *
     H*                Y.                                             *
     H*                                                               *
     H* 10/15/10 C:246 SUPPORT:74382                                  *
     H*                Allow Address line 2 for editable.             *
     H*                                                               *
     H* 02/16/09 C:198 Project: MC330                                 *
     H*                ADDED SCTAXO                                   *
     H*                                                               *
     H* 01/30/08 C:47  Project: CA002                                 *
     H*                REQUIRE SPECIALTY CODE 3
     H*                WHEN THE ADJUDICATION STATE = CA.
     H*                                                               *
     H* 12/10/07 C:198 Project: FL064                                 *
     H*                Expanded license# field to 13 characters.      *
     H*                                                               *
     H* 05/11/07 C:062 Project: MC293                                 *
     H*                ADD NPI #.                                     *
     H*                                                               *
     H* 09/20/05 C:062 Project: KY014                                 *
     H*                Remove the Kentucky only edit of SCAJST/SCSPC3 *
     H*                                                               *
     H* 06/22/05 C:132 Project: WC129-47611 & MC135-47733             *
     H*                Edit on out-of-Contry code field in wrong spot *
     H*                This edit is done when WC is not installed,    *
     H*                since the edit in WC will pass correct code.   *
     H*                                                               *
     H* 05/26/05 C:132 Project: WC082  47306                          *
     H*                If ajudication state is "KY", specialty code 3 *
     H*                must exist/be entered on screen one.           *
     H*                                                               *
     H* 03/09/05 C:15  Project: WC1269                                *
     H*                Added an additional address line for an out of *
     H*                country address.                               *
     F*
     F*  07/24/04 C:79  Project: MA012
     F*                 ADD MA SPECIAL RATES FIELD
     H*                                                               *
     H*  04/12/01 C:062 PROJECT: MC245                                *
     H*                 ASSIGN VALUE TO PHYSICAL ADDRESS TWO.         *
     H*                                                               *
     H*  12/22/00 C:062 PROJECT: MCMNT                                *
     H*                 ADD PARM TO PARM LIST OF F0601C.              *
     H*                                                               *
     H*  09/06/00 C:62  Support: MC101/24239                          *
     H*                 ADD PARM TO CALL OF F0032R01.                 *
     H*                                                               *
     H*  01/25/00 C:53  PROJECT: KY010                                *
     H*                 SPECIALTY CODE FIELD CHANGES FOR KY EDI/BASE  *
     H*                                                               *
     H*  07/15/99 C:062 PROJECT: MC145                                *
     H*                 CORRECT SEARCHABLE FIELDS AND POSTIONING.     *
     H*                                                               *
     H*  06/23/99 C:118 SUPPORT: MC033/18538                          *
     H*                 Modified SCFEIN so a value less than or equal *
     H*                 to zero can not be entered. Changed cursor    *
     H*                 positioning indicator *IN40 to *IN30.         *
     H*
     H*  05/24/99 C:62  PROJECT: MC145                                *
     H*                 IMPLEMENT ERROR MESSAGE FOR NON-SEARCHABLE    *
     H*                 FIELDS.                                       *
     H*                                                               *
     H*  04/08/99 C:53  PROJECT: MCMNT # 17406                        *
     H*                 UPDATE MAINT FIELDS FOR FMPDP                 *
     H*                                                               *
     H*  03/24/99 C:94  PROJECT: MC194                                *
     H*                 CONDITION LOAD OF ADDRESS                     *
     H*                                                               *
     H*  11/09/98 C:31  Support: 14873                                *
     H*                 County search issue                           *
     H*                 1. Change county code from 3A to 4A           *
     H*                                                               *
     H*  07/29/98 C:94  PROJECT: MC145                                *
     H*                 MAKE ADDRESS 2 OPTIONAL.                      *
     H*                                                               *
     H*  03/04/98 C:31  Project: MC145                                *
     H*                 Yr 2000, RISC Development                     *
     H*                 1. Correct update with no record found        *
     H*                                                               *
     H*  01/15/98 C:31  Project: 60MBS                                *
     H*                 Anthem Network                                *
     H*                 1. Corrected error processing in screen 3     *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      09 - Inquiry                                           **
     H**                                                             **
     H**   ALL SCREENS                                               **
     H**      20       WC INSTALLED                                  **
     H**      23       Display Participating in Network              **
     H**      40       ADD MODE for Standalones                      **
     H**      60       GENERAL READ/CHAIN                            **
     H**      61       General READ/CHAIN                            **
     H**                                                             **
     H**   SCR01                                                     **
     H**      30 - 35  POSITION CURSOR                               **
     H**      71 - 89  ERROR CHECK                                   **
     H**                                                             **
     H**   SCR02                                                     **
     H**      30 - 38  POSITION CURSOR                               **
     H**      71 - 89  ERROR CHECK                                   **
     H**                                                             **
     H**   SCR03                                                     **
     H**                                                             **
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      ______ (_____): ___________________ (___/___)          **
     H*****************************************************************
     H*
     F*
     F*
     FF0032F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMPDP     UF A E             DISK
     F                                     RENAME(PPD01:DBRCDF)
     F                                     INFDS(DBFDS)
     F*
     F*  Logical file definition (for provider/fein edit)
     FFMPDL     IF   E           K DISK
     F*
     F*  FS Provider Auxiliary
     FFMP1P     O    E             DISK
     FFMP1L     UF   E           K DISK
     F                                     RENAME(PP101:PP102)
     F*
     F*  State code table file
     FSTSTL1    IF   E           K DISK
     F*
     F*  State Code Table File
     FSTSTP     IF   E             DISK
     F                                     RENAME(PST01:PST02)
     F*
     F*  Generic table file
     FFTGTL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     FFTGIL     IF   E           K DISK
     F*
     F*  Specialty code table file
     FFTSPL1    IF   E           K DISK
     F*
     F*  County code table file
     FFTCCL1    IF   E           K DISK
     FFTCCP     IF   E             DISK
     F                                     RENAME(PCC01:PCC02)
     FFMSIL     IF   E           K DISK
     F*
     F*  Company Auxiliary file
     F*
     FFTTCL1    IF   E           K DISK
     FFTTCP     IF   E             DISK
     F                                     RENAME(PTC01:PTC02)
      /COPY SCOPYMT01D
     D***** Beg Add *** 06/16/15 **************************************
     D Error           S              1
     D Mode            S              1
     D XXN#02          S             30A   Inz(*Blanks)
     D***** Beg Add *** 06/16/15 **************************************
     D*
     D*** Beg Add *** 02/11/16 ****************************************
     D ##adstate       s              1
     D*** End Add *** 02/11/16 ****************************************
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D                 DS
     D  XXCNTY                 1      6
     D  STATE                  1      2
     D  COUNTY                 3      6
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Provider Master Info-
     D                                     rmation')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(3)
      *
     C***** Beg Add *** 06/16/15 **************************************
     C/COPY sprstring
     C***** End Add *** 06/16/15 **************************************
     C/COPY SCOPYMT01C
      *
     c*================================================================
     c*  PLIST definitions
     c*================================================================
     C*
     C* SEARCH PARM LIST
     C*
     C     PGMLS         PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##TBID
      *
      *  Parm list for Facility Code search
      *
     C     PGMLS1        PLIST
     C                   PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM                    ##PGM            10
     C                   PARM                    ##FIL1           10
     C                   PARM                    ##FIL2           10
     C                   PARM                    ##FIL3           10
     C                   PARM                    ##ST
     C                   PARM                    ##CODE           15
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM                    ##FIL7           10
      *
      *  Parm list for County Codes
      *
     C     PGMLS2        PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZERO         ##RRN             9 0
     C                   PARM                    #@ST              2
      *
      *  Parm list for County Codes
      *
     C     PGMLS3        PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    #@ST              2
     C                   PARM                    ##KEY            15
      *
      *  STATE PARM LIST
      *
     C     STST          PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      *ZEROS        ##RRN1            9 0
      *
      *
      *
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CALL            1
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
      *================================================================
      *  KLIST definitions
      *================================================================
      *
      * Provider Master Key
      *
     C     PDKY          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
      *
      * FS Provider Aux Key
      *
     C     P1KY          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
      *
      * Generic Table Key
      *
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C     GIKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXTBID
      *
      * Specialy Code Table Key
      *
     C     SPKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXKEY
      *
      * State/Provider file Key
      *
     C     SIKY          KLIST
     C                   KFLD                    KKAJST
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKSPCD
      *
      * County Code Table Key
      *
     C     CCKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXKEY
      *
      * Company Auxiliary Key
      *
     C     C1KY          KLIST
     C                   KFLD                    ##CO#
      *
     C     TCKY          KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKEFDT
      *
     C     TCKY1         KLIST
     C                   KFLD                    KKCODE
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
      *
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    PDFEIN        XXFEIN
     C     *LIKE         DEFINE    PDMOD         XXMOD
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    PDST          KKAJST
     C     *LIKE         DEFINE    PDTYPE        KKTYPE
     C     *LIKE         DEFINE    PDSPCD        KKSPCD
     C     *LIKE         DEFINE    TCCODE        KKCODE
     C     *LIKE         DEFINE    TCEFDT        KKEFDT
      *
      *================================================================
      * CHGSTS - Change Status
      *================================================================
     C     CHGSTS        BEGSR
      *
      *  CHANGE STATUS
      *
     C     @@RRN         CHAIN     DBRCDF                             99
     C                   Z-ADD     PDFEIN        XXFEIN
     C                   Z-ADD     PDMOD         XXMOD
     C     P1KY          CHAIN     PP102                              60
      *
     C     *IN99         IFEQ      *OFF
     C     *IN60         ANDEQ     *OFF
      *
     C     PDSTAT        IFEQ      'A'
     C                   MOVEL(P)  'I'           PDSTAT
     C                   MOVEL(P)  'I'           P1STAT
     C                   ELSE
     C                   MOVEL(P)  'A'           PDSTAT
     C                   MOVEL(P)  'A'           P1STAT
     C                   ENDIF
      *
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        PDUSER
     C                   MOVE      Q1PGM         PDPGM
     C                   Z-ADD     DSTIME        PDTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PDDT
     C                   MOVE      Q1USER        P1USER
     C                   MOVE      Q1PGM         P1PGM
     C                   Z-ADD     DSTIME        P1TIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P1DT
      *
     C                   UPDATE    DBRCDF
     C                   UPDATE    PP102
      *
     C                   ENDIF                                                  *IN99 *IN60
      *
     C                   RETURN
      *
     C                   ENDSR
      *
      *================================================================
      * DLTPRE - Pre-processing before delete occurs
      *================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
      *================================================================
      * DSPLYR - Processing after displaying information
      *================================================================
     C     DSPLYR        BEGSR
     C*
     C* EXECUTE SUBROUTINE NXTPGM TO DETERMINE IF F0042R OR N0037R
     C* SHOULD BE CALLED. IT IS POSSIBLE THAT NEITHER PGM WILL BE
     C* CALLED.
     C*
     C     SCRN#         IFEQ      3
     C*
     C                   MOVE      @@RRN         ##RRN2
     C*
     C                   EXSR      NXTPGM
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C* RESET PLACE CURSOR INDICATORS.
     C*
     C*
     C*  ADD MODE
     C*
     C     *IN40         IFEQ      *ON
     C*
     C*  PROVIDER MUST BE ENTERED
     C*
     C  N90SCFEIN        IFLE      *ZERO
     C*
     C                   SETON                                        9071
     C*
     C                   ENDIF
     C*
     C*  PROVIDER & MODIFIER MUST BE UNIQUE
     C*
     C                   Z-ADD     SCFEIN        XXFEIN
     C                   Z-ADD     SCMOD         XXMOD
     C*
     C     PDKY          CHAIN(N)  PPD01                              99
     C*
     C  N99
     CANN90              SETON                                        9072
     C*
     C                   ENDIF                                                  ADD MODE
     C*
     C*  Name or Last name Required
     C*
     C  N90SCMNAM        IFEQ      *BLANK
     C     *IN20         ANDEQ     '0'                                          NO WC INSTALLED
     C*
     C                   SETON                                        9073
     C*
     C                   ENDIF
      *
      *  EITHER THE C,S,Z IS ENTERED OR THE OUT OF COUNTRY ADDRESS
      *
     C     *IN20         IFEQ      '0'                                          NO WC INSTALLED
      *
     C     SCMCTY        IFEQ      *BLANKS
     C     SCMST         ANDEQ     *BLANKS
     C     SCMZPA        ANDEQ     *ZERO
     C     SCMAD3        ANDEQ     *BLANK
     C                   SETON                                        9081
     C                   ENDIF
      *
      * BOTH THE C,S,Z AND THE OUT OF COUNTRY ADDRESS CANNOT BE ENTERED
      *
     C     SCMAD3        IFNE      *BLANK
     C     SCMCTY        IFNE      *BLANKS
     C     SCMST         ORNE      *BLANKS
     C     SCMZPA        ORNE      *ZERO
     C                   SETON                                        9082
     C                   ENDIF
     C                   ENDIF
      *
     C*  Edit the Out-of-Country Code before edit on out-of-country addr
     C*
     C  N90SCMCTR        IFNE      *BLANK
     C                   MOVEL(P)  'CTRY'        XXTBID
     C                   MOVEL(P)  SCMCTR        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9080
     C                   ENDIF
     C*
      *
     C     SCMAD3        IFEQ      *BLANKS
     C*
     C*  Mailing Address must be entered
     C*
     C  N90SCMAD1        IFEQ      *BLANKS
     C     *IN20         ANDEQ     '0'                                          NO WC INSTALLED
     C*
     C                   SETON                                        9074
     C*
     C                   ENDIF
     C*
     C*  Mailing Address must be entered
     C*
     C*
     C*  DBA must be 'Y' or 'N'
     C*
     C  N90SCDBA         IFNE      'Y'
     C     SCDBA         ANDNE     'N'
     C     *IN20         ANDEQ     '0'                                          NO WC INSTALLED
     C*
     C                   SETON                                        9076
     C*
     C                   ENDIF
     C*
     C*  Mail City must be entered
     C*
     C  N90SCMCTY        IFEQ      *BLANKS
     C     *IN20         ANDEQ     '0'                                          NO WC INSTALLED
     C*
     C                   SETON                                        9077
     C*
     C                   ENDIF
     C*
     C*  Mail State must be entered
     C*
     C  N90SCMST         IFEQ      *BLANKS
     C     *IN20         ANDEQ     '0'                                          NO WC INSTALLED
     C*
     C                   SETON                                        9078
     C*
     C                   ENDIF
     C*
     C*  Mail Zip code required
     C*
     C  N90SCMZPA        IFEQ      *ZERO
     C     *IN20         ANDEQ     '0'                                          NO WC INSTALLED
     C*
     C                   SETON                                        9079
     C*
     C                   ENDIF
     C*
     C*  Mail Country must be valid
     C*
     C  N90SCMCTR        IFNE      *BLANK
     C*
     C                   MOVEL(P)  'CTRY'        XXTBID
     C                   MOVEL(P)  SCMCTR        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C   99              SETON                                        9080
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C*  If the country code is not 'USA', then the province/postal
     C*  code has to be entered.
     C*
     C     SCMCTR        IFNE      *BLANK
     C     SCMCTR        ANDNE     'USA'
     C     SCMAD3        ANDEQ     *BLANK
     C                   SETON                                        9083
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF                                                  *IN20
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C*  Physical Name default
     C*
     C     SCPNAM        IFEQ      *BLANKS
     C     SCMNAM        ANDNE     *BLANKS
     C*
     C                   MOVEL(P)  SCMNAM        SCPNAM
     C                   SETON                                        90
     C*
     C                   ENDIF
     C*
     C*  Physical First Name default
     C*
     C     SCPFSN        IFEQ      *BLANKS
     C     SCMFSN        ANDNE     *BLANKS
     C*
     C                   MOVEL(P)  SCMFSN        SCPFSN
     C                   SETON                                        90
     C*
     C                   ENDIF
     C*
     C*  Physical address 1 default
     C*
     C     PDDBA1        IFEQ      'N'
     C                   SETOFF                                       85
     C     SCPAD1        IFEQ      *BLANKS
     C     SCMAD1        ANDNE     *BLANKS
     C*
     C                   MOVEL(P)  SCMAD1        SCPAD1
     C                   SETON                                        90
     C                   SETON                                        85
     C*
     C                   ENDIF
     C     *IN85         IFEQ      '1'
     C     SCPAD2        IFEQ      *BLANKS
     C     SCMAD2        ANDNE     *BLANKS
     C                   MOVEL(P)  SCMAD2        SCPAD2
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C     SCPAD1        IFEQ      *BLANKS
     C     SCMAD2        ANDNE     *BLANKS
     C*
     C                   MOVEL(P)  SCMAD2        SCPAD1
     C                   SETON                                        90
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Physical city default
     C*
     C     SCPCTY        IFEQ      *BLANKS
     C     SCMCTY        ANDNE     *BLANKS
     C*
     C                   MOVEL(P)  SCMCTY        SCPCTY
     C                   SETON                                        90
     C*
     C                   ENDIF
     C*
     C*  Physical State default
     C*
     C     SCPST         IFEQ      *BLANKS
     C     SCMST         ANDNE     *BLANKS
     C*
     C                   MOVEL(P)  SCMST         SCPST
     C                   SETON                                        90
     C*
     C                   ENDIF
     C*
     C*  Physical Zip code default
     C*
     C     SCPZPA        IFEQ      *ZEROS
     C     SCMZPA        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCMZPA        SCPZPA
     C                   Z-ADD     SCMZPB        SCPZPB
     C                   SETON                                        90
     C*
     C                   ENDIF
      *
     C*
     C*  Physical Country must be valid
     C*
     C  N90SCPCTR        IFNE      *BLANK
     C*
     C                   MOVEL(P)  'CTRY'        XXTBID
     C                   MOVEL(P)  SCPCTR        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        908084
     C*
     C                   ENDIF
     C*
     C     SCPCTR        IFEQ      *BLANKS
     C     SCMCTR        ANDNE     *BLANKS
     C                   MOVE      SCMCTR        SCPCTR
     C                   SETON                                        90
     C                   ENDIF
     C*
     C     SCPAD3        IFEQ      *BLANKS
     C     SCMAD3        ANDNE     *BLANKS
     C                   MOVE      SCMAD3        SCPAD3
     C                   SETON                                        90
     C                   ENDIF
     C*
     C*  Physical Area code default
     C*
     C     SCPACD        IFEQ      *ZEROS
     C     SCMACD        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCMACD        SCPACD
     C                   SETON                                        90
     C*
     C                   ENDIF
     C*
     C*  Physical Area code default
     C*
     C     SCPPH#        IFEQ      *ZEROS
     C     SCMPH#        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCMPH#        SCPPH#
     C                   SETON                                        90
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* BUILD PROVIDER NAME HEADER FOR SCREEN 2 AND 3.
     C*
     C                   MOVEL(P)  SCMNAM        SCPRNM
     C                   CAT       ', ':0        SCPRNM
     C                   CAT       SCMFSN:1      SCPRNM
     C*
     C***** Beg Add *** 06/16/15 **************************************
      /Free
       // Validate NPI#
        ErN#02 =*Off;
        Mode    = 'N';
        Error   = ' ';
        XXN#02  =%Trim(SCN#02);
        ErN#02 = validchar(XXN#02:Mode:Error);
        If ErN#02 = *On;
          *IN90 = *On;
        EndIf;

      /End-Free
     C***** End Add *** 06/16/15 **************************************
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C                   MOVEA     '00000'       *IN(30)
     C                   MOVEA     '00000'       *IN(35)
     C*
     C*  Ajudication State required
     C*
     C  N90SCAJST        IFEQ      *BLANKS
     C                   SETON                                        9071
     C                   ELSE
     C*
     C*  Ajudication State must be valid
     C*
     C                   MOVEL(P)  SCAJST        XTMP2            10
     C*
     C     XTMP2         CHAIN     PST01                              60
     C     *IN60         IFEQ      '1'
     C     ST1A3         ORNE      'Y'
     C*** Beg Add *** 02/11/16 ************************************************
b05  C     ST1A2         OREQ      'Y'
     C*** End Add *** 02/11/16 ************************************************
     C                   SETON                                        9072
     C                   ENDIF
     C                   ENDIF                                                  SCAJST
     C*
     C*  Ajudication State must be valid
     C*
     C                   MOVEL(P)  SCAJST        XTMP2            10
     C*
     C     XTMP2         CHAIN     PST01                              99
     C   99
     CANN90              SETON                                        9072
     C  N90              EXSR      SETTBL                                       ADD 01/25/00
     C*
     C*  Participating in Network required
     C*
     C  N90
     CAN 23SCPARN        IFEQ      *BLANKS
     C                   SETON                                        9073
     C                   ENDIF
     C*
     C*  Particpating in Network Must be valid
     C*
     C   23              MOVEL(P)  'PARN'        XXTBID
     C   23              MOVEL(P)  SCPARN        XXKEY
     C*
     C   23GTKY          CHAIN     PGT01                              60
     C*
     C   60
     CANN90
     CAN 23              SETON                                        9074
     C*
     C*  Payee Type required
     C*
     C  N90SCTYPE        IFEQ      *BLANKS
     C     *IN20         ANDEQ     '0'                                          NO WC INSTALLED
     C*
     C                   SETON                                        9076
     C*
     C                   ENDIF
     C*
     C*  Payee Type must be valid
     C*
     C*** Beg Add *** 07/25/13 ****************************************
     C                   IF        *in20 = *off and SCTYPE <> *Blanks
     C*** End Add *** 07/25/13 ****************************************
     C                   MOVEL(P)  'PRTY'        XXTBID
     C                   MOVEL(P)  SCTYPE        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C   99
     CANN90              SETON                                        9077
     C*** Beg Add *** 07/25/13 ****************************************
     C                   ENDIF
     C*** End Add *** 07/25/13 ****************************************
     C*
     C*  Specialty code 1 must be valid (if entered)
     C*
     C  N90SCSPC1        IFNE      *BLANKS
     C     *IN20         ANDEQ     *OFF
     C*
     C                   MOVEL(P)  SCSPC1        XXKEY
     C     SPKY          CHAIN     PSP01                              99
     C   99              SETON                                        9078
     C                   ENDIF
     C*
     C*
     C  N90SCTAXO        IFEQ      *BLANKS
     C     *IN24         ANDNE     *OFF
     C*
     C                   SETON                                        9025
     C*
     C                   ELSE
     C  N90*IN24         IFEQ      *OFF
     C     SCTAXO        ANDNE     *BLANKS
     C     *IN24         OREQ      *ON
     C*
     C                   MOVEL(P)  SCTAXO        KKCODE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C     TCKY          SETGT     PTC01                              99
     C     TCKY1         READPE    PTC01                                  99
     C     *IN99         IFEQ      *OFF
     C     TCSTAT        ANDEQ     'I'
     C     *IN99         OREQ      *ON
     C                   SETON                                        9026
     C                   ENDIF                                                  TCSTAT = 'I'/*IN99
     C                   ENDIF                                                  *IN24
     C*
     C                   ENDIF                                                  SCTAXO = ' '
     C*
     C*
     C*  Specialty code 2 must be valid (if entered)
     C*
     C  N90SCSPC2        IFNE      *BLANKS
     C                   MOVEL(P)  'SPCD'        XXTBID
     C                   MOVEL(P)  SCSPC2        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9079
     C                   ENDIF
     C*
     C*  Specialty code 3 must be entered if adj state = KY
     C*
     C*
     C*  Specialty code 3 must be entered if adj state is CA
     C*
     C  N90SCAJST        IFEQ      'CA'
     C     SCSPC3        IFEQ      *BLANK
     C                   SETON                                        9086
     C                   END
     C                   END
     C*
     C*  Specialty code 3 must be valid (if entered)
     C*
     C  N90SCSPC3        IFNE      *BLANKS
     C                   MOVEL(P)  SPC3          XXTBID
     C                   MOVEL(P)  SCSPC3        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C     GTSTAT        OREQ      'I'
     C                   SETON                                        9080
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Specialty Rating must be valid (if entered)
     C*
     C  N90SCSR          IFNE      *BLANKS
     C*
     C                   MOVEL(P)  'SPRT'        XXTBID
     C                   MOVEL(P)  SCSR          XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C   99              SETON                                        9081
     C*
     C                   ENDIF
     C  N90SCTYPE        IFEQ      '4'
     C     SCAJST        ANDEQ     'MA'
     C     SCCNTY        ANDEQ     *BLANKS
     C                   SETON                                        9087
     C                   ENDIF
     C*
     C*  County code Must be valid (if entered)
     C*
     C  N90SCCNTY        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCCNTY        COUNTY
     C                   MOVEL(P)  SCAJST        STATE
     C                   MOVEL(P)  XXCNTY        XXKEY
     C*
     C     CCKY          CHAIN     PCC01                              99
     C   99              SETON                                        9082
     C                   ENDIF
     C*
     C*  SMSA Must be valid (if entered)
     C*
     C  N90SCSMSA        IFNE      *BLANKS
     C     SCAJST        CAT(P)    'SM':0        XXTBID
     C                   MOVEL(P)  SCSMSA        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9083
     C                   ENDIF
     C*
     C*  Facility code must be valid (if entered)
     C*
     C  N90SCFCD         IFNE      *BLANKS
     C* ZZZ
     C                   MOVEL(P)  'E'           ##CALL
     C                   MOVEL(P)  'F2355R'      ##PGM
     C                   MOVEL(P)  'FTFTP'       ##FIL1
     C                   MOVEL(P)  'FTABL'       ##FIL2
     C                   MOVEL(P)  SCAJST        ##ST              2
     C                   MOVEL(P)  SCFCD         ##CODE
     C*
     C                   CALL      'F0601C'      PGMLS1
     C*
     C  N90##CODE        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  ##CODE        SCFCD
     C                   MOVEL(P)  ##DESC        SCFDS
     C*
     C                   ELSE
     C*
     C                   SETON                                        9084
     C*
     C                   ENDIF                                                  ##CODE
     C*
     C                   ENDIF                                                  SCFCD
     C  N90SCAJST        IFEQ      'MA'
     C     SCTYPE        ANDEQ     '4'
     C     SCSPRT        ANDEQ     ' '
     C                   SETON                                        9088
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C                   MOVEA     '00000'       *IN(30)
     C                   MOVEA     '00000'       *IN(35)
     C*
     C*  1099 required
     C*
     C  N90SC1099        IFNE      'Y'
     C     SC1099        ANDNE     'N'
     C     *IN20         ANDEQ     '0'                                          NO WC INSTALLED
     C*
     C                   SETON                                        9071
     C*
     C                   ENDIF
      /Free
       //*** Begin Add *** 02/09/18 **************************************
       //835 Valid values are Y or N
        If SC835pa = *blanks;
           SC835pa = 'N';
        Endif;
       //*** End   Add *** 02/09/18 **************************************
      /End-Free
     C*
     C*  Certification code must be valid (if entered)
     C*
     C  N90*IN20         IFEQ      '0'                                          NO WC INSTALLED
     C*
     C     SCAJST        CAT(P)    'WC':0        XXTBID
     C                   MOVEL(P)  SCWCCE        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9073
     C                   ENDIF
     C*
     C*  Certification type must be valid (if entered)
     C*
     C  N90SCBWCT        IFNE      *ZERO
     C     *IN20         ANDEQ     '0'                                          NO WC INSTALLED
     C     SCAJST        CAT(P)    'WT':0        XXTBID
     C                   MOVE      SCBWCT        X1BWCT
     C                   MOVEL(P)  X1BWCT        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9074
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* KJDO - F10 processing
     C*================================================================
     C     KJDO          BEGSR
      *
     C                   MOVE      '1'           *IN90
      *
     C                   MOVEA     '00000'       *IN(30)
     C                   MOVEA     '00000'       *IN(35)
      *
     C     *IN09         IFEQ      *OFF                                         NOT DISP MODE
     C                   SELECT
      *
      *  Search Mail Country
      *
     C     HFLD          WHENEQ    'SCMCTR'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'CTRY'        ##TBID
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   MOVE      '1'           *IN33
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C                   MOVEL     GTKEY         SCMCTR
     C     *IN20         IFEQ      *OFF
     C                   MOVE      '1'           *IN34
     C                   ELSE
     C*
     C                   SETON                                        37
     C*
     C                   ENDIF
     C*
     C                   ENDIF
      *
      *  Search Mail State
      *
     C     HFLD          WHENEQ    'SCMST'
     C     *IN20         ANDEQ     *OFF
     C                   CALL      'SY601R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   MOVE      '1'           *IN31
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PST02                              99
     C*
     C                   MOVEL     STKEY         SCMST
     C                   MOVE      '1'           *IN32
     C*
     C                   ENDIF
      *
      *  Search Physical State
      *
     C     HFLD          WHENEQ    'SCPST'
     C                   CALL      'SY601R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   MOVE      '1'           *IN35
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PST02                              99
     C*
     C                   MOVEL     STKEY         SCPST
     C                   MOVE      '1'           *IN36
     C*
     C                   ENDIF
      *
      *  Search Mail Country
      *
     C     HFLD          WHENEQ    'SCPCTR'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'CTRY'        ##TBID
     C*
     C                   SELECT
     C*
     C     *INKL         WHENEQ    *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        3384
     C*
     C                   OTHER
     C     ##RRN         CHAIN     PGT02                              99
     C                   MOVEL     GTKEY         SCPCTR
     C                   SETON                                        3484
     C*
     C                   ENDSL
      *
      *  Search Adjudication State
      *
      *
     C     HFLD          WHENEQ    'SCAJST'
      *
     C*** Beg Chg *** 02/11/16 ********************************************
     C**********         CALL      'F0601R'      STST
     C                   CALL      'F0601R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      *ZEROS        ##RRN1            9 0
     C                   PARM      'A'           ##adstate         1
     C*** End Chg *** 02/11/16 ********************************************
      *
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        31
     C                   ELSE
     C     ##RRN         CHAIN     PST02                              99
     C                   MOVEL(P)  STKEY         SCAJST
     C                   MOVEL(P)  STNAME        SCAJDS
     C  N90              EXSR      SETTBL                                       ADD 01/25/00
     C     *IN23         IFEQ      *ON
     C                   SETON                                        32
     C                   ELSE
     C*
     C     *IN20         IFEQ      *OFF
     C                   SETON                                        33
     C                   ELSE
     C*
     C                   SETON                                        35
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
      *  Search Participating in Network
      *
      *
      *   Get Network participation description
      *
      *
     C*
     C* SEARCH PARTICIPATING/NON-PARTICIPATING
     C*
     C     HFLD          WHENEQ    'SCPARN'
     C     *IN23         ANDEQ     *ON
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'PARN'        ##TBID
     C*
     C     *INKL         IFEQ      *OFF
     C     ##RRN         ANDNE     *ZEROS
     C*
     C     *IN20         IFEQ      *OFF
      *
     C                   SETON                                        33
     C*
     C                   ELSE
     C*
     C     *IN24         IFEQ      *OFF
     C                   SETON                                        35
     C                   ELSE
     C                   SETON                                        27
     C                   ENDIF                                                  *IN24 = *OFF
     C*
     C                   ENDIF
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C*   Get Payee Type description
     C*
     C                   MOVEL     GTKEY         SCTYPE
     C                   MOVEL     GTDESC        SCTPDS
      *
     C                   ELSE
     C*
     C                   SETON                                        32
      *
     C                   ENDIF
      *
      *  Search Payee Type
      *
     C     HFLD          WHENEQ    'SCTYPE'
     C     *IN20         ANDEQ     *OFF
     C*
     C                   MOVEL(P)  'PRTY'        ##TBID
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##TBID
     C*
     C     *INKL         IFEQ      *OFF
     C     ##RRN         ANDNE     *ZEROS
     C*
     C                   MOVEL(P)  XXKEY         SCTYPE
     C                   MOVE      '1'           *IN34
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C*   Get Payee Type description
     C*
     C                   MOVEL     GTKEY         SCTYPE
     C                   MOVEL     GTDESC        SCTPDS
      *
     C                   ELSE
     C                   MOVE      '1'           *IN33
     C                   ENDIF
      *
      *  Search Specialty Code 1
      *
     C     HFLD          WHENEQ    'SCSPC1'
     C     *IN20         ANDEQ     *OFF
     C                   MOVEL(P)  SCSPC1        SPKEY            10
     C                   CALL      'F2335R'
     C                   PARM      '2'           QQCALL            1
     C                   PARM                    SPKEY
     C                   PARM                    QQRTC             1
      *
     C     QQRTC         IFEQ      '*'
     C     SPKEY         ANDNE     *BLANK
     C                   MOVEL(P)  SPKEY         SCSPC1
     C                   MOVE      '1'           *IN35
      *
      *   Get Specialty code 1 description
      *
     C                   MOVEL(P)  SCSPC1        XXKEY
     C     SPKY          CHAIN     PSP01                              60
     C  N60              MOVEL(P)  SPDESC        SCSPD1
     C   60              MOVEL(P)  *BLANKS       SCSPD1
      *
     C                   ELSE
     C                   MOVE      '1'           *IN34
     C                   END
      *
     C     HFLD          WHENEQ    'SCTAXO'
     C                   CALL      'F2440R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PTC02                              99
     C                   MOVEL(P)  TCCODE        SCTAXO
     C                   MOVE      '1'           *IN35
     C                   ELSE
     C                   MOVE      '1'           *IN27
     C                   ENDIF
      *  Search Specialty code 2
      *
     C     HFLD          WHENEQ    'SCSPC2'
      *
     C                   MOVEL(P)  'SPCD'        ##TBID
     C                   CALL      'F2380R'      PGMLS
      *
     C     ##RRN         IFNE      *ZEROS
     C                   MOVE      '1'           *IN36
     C     ##RRN         CHAIN     PGT02                              99
     C                   MOVEL(P)  GTKEY         SCSPC2
     C                   MOVEL(P)  GTDESC        SCSPD2
     C                   ELSE
     C                   MOVE      '1'           *IN35
     C                   ENDIF                                                  ##RRN
      *
      *  Search Specialty code 3
      *
     C     HFLD          WHENEQ    'SCSPC3'
      *
     C                   EXSR      SETTBL
     C                   MOVEL(P)  SPC3          ##TBID
     C                   CALL      'F2380R'      PGMLS
      *
     C     ##RRN         IFNE      *ZEROS
     C                   MOVE      '1'           *IN37
     C     ##RRN         CHAIN     PGT02                              99
     C                   MOVEL(P)  GTKEY         SCSPC3
     C                   MOVEL(P)  GTDESC        SCSPD3
     C                   ELSE
     C                   MOVE      '1'           *IN36
     C                   ENDIF                                                  ##RRN
      *
      *  Search Specialty rating
      *
     C     HFLD          WHENEQ    'SCSR'
     C*
     C                   MOVEL(P)  'SPRT'        ##TBID
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##TBID
     C*
     C     *INKL         IFEQ      *OFF
     C     ##RRN         ANDNE     *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C                   MOVEL(P)  GTKEY         SCSR
     C                   MOVEL(P)  GTDESC        SCSRDS
     C                   MOVE      '1'           *IN38
      *
     C                   ELSE
     C                   MOVE      '1'           *IN37
     C                   ENDIF
      *
      *  Search County
      *
     C     HFLD          WHENEQ    'SCCNTY'
     C                   MOVEL     SCAJST        #@ST
     C                   CALL      'F2360R'      PGMLS2
      *
     C     ##RRN         IFNE      *ZEROS
     C                   MOVE      '1'           *IN39
     C     ##RRN         CHAIN     PCC02                              99
     C*
     C                   MOVEL(P)  CCKEY         XXCNTY
     C                   MOVEL(P)  COUNTY        SCCNTY
     C                   MOVEL(P)  CCDESC        SCCNDS
     C                   ELSE
     C                   MOVE      '1'           *IN38
     C                   ENDIF                                                  ##RRN
      *
      *  Search SMSA
      *
     C     HFLD          WHENEQ    'SCSMSA'
      *
     C     SCPST         IFNE      *BLANKS
     C*
     C     SCPST         CAT(P)    'SM':0        ##TBID
     C                   ELSE
     C*
     C     SCMST         CAT(P)    'SM':0        ##TBID
     C                   ENDIF
     C                   CALL      'F2380R'      PGMLS
      *
     C     ##RRN         IFNE      *ZEROS
     C                   MOVE      '1'           *IN30
     C     ##RRN         CHAIN     PGT02                              99
     C                   MOVEL(P)  GTKEY         SCSMSA
     C                   MOVEL(P)  GTDESC        SCSMDS
     C                   ELSE
     C                   MOVE      '1'           *IN39
     C                   ENDIF                                                  ##RRN
      *
      *  Search Facility code
      *
     C     HFLD          WHENEQ    'SCFCD'
     C                   EXSR      SRCHFC
      *
      *  Search Certification Code
      *
     C     HFLD          WHENEQ    'SCWCCE'
     C     *IN20         ANDEQ     *OFF
     C*
     C     SCAJST        CAT(P)    'WC':0        ##TBID
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##TBID
     C*
     C     *INKL         IFEQ      *OFF
     C     ##RRN         ANDNE     *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C                   MOVEL(P)  GTKEY         SCWCCE
     C                   MOVEL(P)  GTDESC        SCWCDS
     C                   MOVE      '1'           *IN32
      *
     C                   ELSE
     C                   MOVE      '1'           *IN31
     C                   ENDIF
      *
      *  Search Certification Type
      *
     C     HFLD          WHENEQ    'SCBWCT'
     C     *IN20         ANDEQ     *OFF
     C*
     C     SCAJST        CAT(P)    'WT':0        ##TBID
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##TBID
     C*
     C                   MOVE      '1'           *IN33
     C*
     C     *INKL         IFEQ      *OFF
     C     ##RRN         ANDNE     *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C                   MOVEL(P)  GTKEY         X1BWCT            3
     C                   MOVE      X1BWCT        SCBWCT
     C                   MOVEL(P)  GTDESC        SCBWDS
      *
     C                   ENDIF
     C                   OTHER
     C                   SETON                                        8990
      *
     C                   ENDSL
     C                   ENDIF
      *
     C                   ENDSR
      *
      *
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C  NKE              EXSR      SCRNUM
     C*
     C* IF NOT IN ADD MODE LOAD DATA BASE FIELDS INTO SCREEN FIELDS.
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C*  PD FIELDS
     C*
     C                   MOVEL(P)  PDSTAT        SCSTAT
     C                   Z-ADD     PDFEIN        SCFEIN
     C                   Z-ADD     PDMOD         SCMOD
     C                   MOVEL(P)  PDNAME        SCMNAM
     C                   MOVEL(P)  PDFSN         SCMFSN
     C*
     C                   MOVEL(P)  SCMNAM        SCPRNM
     C                   CAT       ', ':0        SCPRNM
     C                   CAT       SCMFSN:1      SCPRNM
     C*
     C                   MOVEL(P)  PDTITL        SCTITL
     C                   MOVEL(P)  PDADR1        SCMAD1
     C                   MOVEL(P)  PDDBA1        SCDBA
     C                   MOVEL(P)  PDADR2        SCMAD2
     C                   MOVEL(P)  PDCTY         SCMCTY
     C                   MOVEL(P)  PDST          SCMST
     C                   MOVEL     PDZIP         SCMZPA
     C                   MOVE      PDZIP         SCMZPB
     C                   MOVEL     PDADR3        SCMAD3
     C                   Z-ADD     PDAREA        SCMACD
     C                   Z-ADD     PDPHN#        SCMPH#
     C                   Z-ADD     PDFXAR        SCFXAR
     C                   Z-ADD     PDFAX#        SCFAX#
     C                   MOVEL(P)  PDPARN        SCPARN
     C*
     C*   Get Network participation description
     C*
     C                   MOVEL(P)  'PARN'        XXTBID
     C                   MOVEL(P)  SCPARN        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SCPADS
     C   99              MOVEL(P)  *BLANKS       SCPADS
     C*
     C                   MOVEL(P)  PDTYPE        SCTYPE
     C*
     C*   Get Payee Type description
     C*
     C                   MOVEL(P)  'PRTY'        XXTBID
     C                   MOVEL(P)  SCTYPE        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SCTPDS
     C   99              MOVEL(P)  *BLANKS       SCTPDS
     C*
     C                   MOVEL(P)  PDSPCD        SCSPC1
     C                   MOVEL(P)  PDTAXO        SCTAXO
     C*
     C*   Get Specialty code 1 description
     C*
     C                   MOVEL(P)  SCSPC1        XXKEY
     C*
     C     SPKY          CHAIN     PSP01                              99
     C*
     C  N99              MOVEL(P)  SPDESC        SCSPD1
     C   99              MOVEL(P)  *BLANKS       SCSPD1
     C*
     C                   MOVEL(P)  PDLIC#        SCLIC#
     C                   MOVEL(P)  PDSHNM        SCSHNM
     C                   MOVEL(P)  PD1099        SC1099
      /Free
       //*** Begin Add *** 02/09/18 **************************************
        SC835pa = PD835P;
       //*** End   Add *** 02/09/18 **************************************
      /End-Free
     C                   MOVEL(P)  PDWCCE        SCWCCE
     C*
     C*   Get Certification Code Description
     C*
     C     SCAJST        CAT(P)    'WC':0        XXTBID
     C                   MOVEL(P)  SCWCCE        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SCWCDS
     C   99              MOVEL(P)  *BLANKS       SCWCDS
     C*
     C                   Z-ADD     PDWCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCWCDT
     C*
     C                   MOVEL(P)  PDBWC#        SCBWC#
     C                   MOVE      PDBWCT        SCBWCT
     C*
     C*   Get Certification Type Description
     C*
     C     SCAJST        CAT(P)    'WT':0        XXTBID
     C                   MOVE(P)   SCBWCT        X1BWCT
     C                   MOVEL     X1BWCT        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SCBWDS
     C   99              MOVEL(P)  *BLANKS       SCBWDS
     C*
     C*  P1 FIELDS
     C*
     C                   Z-ADD     PDFEIN        XXFEIN
     C                   Z-ADD     PDMOD         XXMOD
     C*
     C     P1KY          CHAIN(N)  PP102                              99
     C*
     C     *IN99         IFEQ      '0'
     C*
     C                   MOVEL(P)  P1N#02        SCN#02
     C                   MOVEL(P)  P1MCTR        SCMCTR
     C     SCMCTR        IFEQ      *BLANKS
     C                   MOVEL(P)  PDCTRY        SCMCTR
     C                   ENDIF
     C                   MOVEL(P)  P1NAME        SCPNAM
     C                   MOVEL(P)  P1FSN         SCPFSN
     C                   MOVEL(P)  P1ADR1        SCPAD1
     C                   MOVEL(P)  P1ADR2        SCPAD2
     C                   MOVEL(P)  P1CTY         SCPCTY
     C                   MOVEL(P)  P1PST         SCPST
     C                   MOVEL     P1ZIP         SCPZPA
     C                   MOVE      P1ZIP         SCPZPB
     C                   MOVEL(P)  P1CTRY        SCPCTR
     C                   MOVEL(P)  P1ADR3        SCPAD3
     C                   Z-ADD     P1AREA        SCPACD
     C                   Z-ADD     P1PHN#        SCPPH#
     C                   MOVEL(P)  P1ST          SCAJST
     C  N90              EXSR      SETTBL                                       ADD 01/25/00
     C*
     C*   GET AJUDICATION STATE DESCRIPTION
     C*
     C                   MOVEL(P)  SCAJST        XTMP2            10
     C*
     C     XTMP2         CHAIN     PST01                              99
     C*
     C  N99              MOVEL(P)  STNAME        SCAJDS
     C   99              MOVEL(P)  *BLANKS       SCAJDS
     C*
     C                   MOVEL(P)  P1SPC2        SCSPC2
     C*
     C*   Get Specialty code 2 description
     C*
     C                   MOVEL(P)  'AMCS'        XXTBID
     C                   MOVEL(P)  SCSPC2        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SCSPD2
     C   99              MOVEL(P)  *BLANKS       SCSPD2
     C*
     C                   MOVEL(P)  P1SPC3        SCSPC3
     C*
     C*   Get Specialty code 3 description
     C*
     C                   MOVEL(P)  SPC3          XXTBID
     C                   MOVEL(P)  SCSPC3        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL(P)  GTDESC        SCSPD3
     C   99              MOVEL(P)  *BLANKS       SCSPD3
     C*
     C                   MOVEL(P)  P1FL01        SCSR
     C*
     C*   Get Specialty rating description
     C*
     C                   MOVEL(P)  'SPRT'        XXTBID
     C                   MOVEL(P)  SCSR          XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SCSRDS
     C   99              MOVEL(P)  *BLANKS       SCSRDS
     C*
     C*   COUNTY
     C     4             SUBST(P)  P1CNTY:6      SCCNTY
     C                   MOVEL(P)  SCAJST        STATE
     C                   MOVEL(P)  SCCNTY        COUNTY
     C*
     C*   Get County code description
     C*
     C                   MOVEL(P)  XXCNTY        XXKEY
     C*
     C     CCKY          CHAIN     PCC01                              99
     C*
     C  N99              MOVEL(P)  CCDESC        SCCNDS
     C   99              MOVEL(P)  *BLANKS       SCCNDS
     C*
     C*   SMSA
     C     5             SUBST(P)  P1CNTY:1      SCSMSA
     C*
     C*   Get SMSA description
     C*
     C     SCAJST        CAT(P)    'SM':0        XXTBID
     C                   MOVEL(P)  SCSMSA        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SCSMDS
     C   99              MOVEL(P)  *BLANKS       SCSMDS
     C*
     C*   FACILITY CODE
     C*
     C     6             SUBST(P)  P1CNTY:10     SCFCD
     C*
     C*   Get Facility code description
     C*
     C     SCAJST        CAT(P)    'FC':0        XXTBID
     C                   MOVEL(P)  SCFCD         XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SCFDS
     C   99              MOVEL(P)  *BLANKS       SCFDS
     C*
     C                   MOVEL(P)  P1MCR#        SCMCR#
     C                   MOVE      P1FL03        SCSPRT
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  *BLANKS       SCN#02
     C                   MOVEL(P)  *BLANKS       SCMCTR
     C                   MOVEL(P)  *BLANKS       SCPNAM
     C                   MOVEL(P)  *BLANKS       SCPFSN
     C                   MOVEL(P)  *BLANKS       SCPAD1
     C                   MOVEL(P)  *BLANKS       SCPAD2
     C                   MOVEL(P)  *BLANKS       SCPCTY
     C                   MOVEL(P)  *BLANKS       SCPST
     C                   MOVE      *ZEROS        SCPZPA
     C                   MOVE      *ZEROS        SCPZPB
     C                   MOVE      *BLANKS       SCPCTR
     C                   MOVE      *BLANKS       SCPAD3
     C                   Z-ADD     *ZEROS        SCPACD
     C                   Z-ADD     *ZEROS        SCPPH#
     C                   MOVEL(P)  *BLANKS       SCAJST
     C                   MOVEL(P)  *BLANKS       SCAJDS
     C                   MOVEL(P)  *BLANKS       SCSPC2
     C                   MOVEL(P)  *BLANKS       SCSPD2
     C                   MOVEL(P)  *BLANKS       SCSPC3
     C                   MOVEL(P)  *BLANKS       SCSPD3
     C                   MOVEL(P)  *BLANKS       SCSR
     C                   MOVEL(P)  *BLANKS       SCSRDS
     C                   MOVEL(P)  *BLANKS       SCCNTY
     C                   MOVEL(P)  *BLANKS       SCCNDS
     C                   MOVEL(P)  *BLANKS       SCSMSA
     C                   MOVEL(P)  *BLANKS       SCSMDS
     C                   MOVEL(P)  *BLANKS       SCFCD
     C                   MOVEL(P)  *BLANKS       SCFDS
     C                   MOVEL(P)  *BLANKS       SCMCR#
     C                   MOVE      *BLANKS       SCSPRT
     C*
     C                   ENDIF
      ***** Beg Add ***** 05/25/12 ***************************************
      /free
       scEmad = PDemad;
      /end-free
      ***** End Add ***** 05/25/12 ***************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  'A'           SCSTAT
     C                   Z-ADD     *ZEROS        SCFEIN
     C                   Z-ADD     *ZEROS        SCMOD
     C                   MOVEL(P)  *BLANKS       SCMNAM
     C                   MOVEL(P)  *BLANKS       SCMFSN
     C                   MOVEL(P)  *BLANKS       SCTITL
     C                   MOVEL(P)  *BLANKS       SCMAD1
     C                   MOVEL(P)  *BLANKS       SCDBA
     C                   MOVEL(P)  *BLANKS       SCMAD2
     C                   MOVEL(P)  *BLANKS       SCMCTY
     C                   MOVEL(P)  *BLANKS       SCMST
     C                   Z-ADD     *ZEROS        SCMZPA
     C                   Z-ADD     *ZEROS        SCMZPB
     C                   MOVEL     *BLANKS       SCMAD3
     C                   Z-ADD     *ZEROS        SCMACD
     C                   Z-ADD     *ZEROS        SCMPH#
     C                   Z-ADD     *ZEROS        SCFXAR
     C                   Z-ADD     *ZEROS        SCFAX#
     C                   MOVEL(P)  *BLANKS       SCLIC#
     C                   MOVEL(P)  *BLANKS       SCSHNM
     C                   MOVEL(P)  *BLANKS       SCAJDS
     C                   MOVEL(P)  *BLANKS       SCTPDS
     C                   MOVEL(P)  *BLANKS       SCSPD1
     C                   MOVEL(P)  *BLANKS       SCSPD2
     C                   MOVEL(P)  *BLANKS       SCSPD3
     C                   MOVEL(P)  *BLANKS       SCSRDS
     C                   MOVEL(P)  *BLANKS       SCCNDS
     C                   MOVEL(P)  *BLANKS       SCSMDS
     C                   MOVEL(P)  *BLANKS       SCWCDS
     C                   MOVEL(P)  *BLANKS       SCBWDS
     C                   MOVEL(P)  *BLANKS       SCFDS
     C                   MOVEL(P)  'N'           SCPARN
     C*
     C*   Get Network participation description
     C*
     C                   MOVEL(P)  'PARN'        XXTBID
     C                   MOVEL(P)  SCPARN        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SCPADS
     C   99              MOVEL(P)  *BLANKS       SCPADS
     C*
     C                   MOVEL(P)  'N'           SCWCCE
     C*
     C*   Get Certification Code Description
     C*
     C     SCAJST        CAT(P)    'WC':0        XXTBID
     C                   MOVEL(P)  SCWCCE        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SCWCDS
     C   99              MOVEL(P)  *BLANKS       SCWCDS
     C                   MOVEL(P)  'Y'           SC1099
      /Free
       //*** Begin Add *** 02/09/18 **************************************
        SC835pa = 'N';
       //*** End   Add *** 02/09/18 **************************************
      /End-Free
     C*
     C* FMP1P FIELDS
     C*
     C                   MOVEL(P)  *BLANKS       SCMCTR
     C                   MOVEL(P)  *BLANKS       SCPNAM
     C                   MOVEL(P)  *BLANKS       SCPFSN
     C                   MOVEL(P)  *BLANKS       SCPAD1
     C                   MOVEL(P)  *BLANKS       SCPAD2
     C                   MOVEL(P)  *BLANKS       SCPCTY
     C                   MOVEL(P)  *BLANKS       SCPST
     C                   Z-ADD     *ZEROS        SCPZPA
     C                   Z-ADD     *ZEROS        SCPZPB
     C                   MOVE      *BLANKS       SCPCTR
     C                   MOVE      *BLANKS       SCPAD3
     C                   Z-ADD     *ZEROS        SCPACD
     C                   Z-ADD     *ZEROS        SCPPH#
     C                   MOVEL(P)  *BLANKS       SCAJST
      ***** Beg Add ***** 05/25/12 ***************************************
      /free
       scEmad = *blanks;
      /end-free
      ***** End Add ***** 05/25/12 ***************************************
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C*  PD FIELDS
     C*
     C                   MOVEL     SCSTAT        PDSTAT
     C                   Z-ADD     SCFEIN        PDFEIN
     C                   Z-ADD     SCMOD         PDMOD
     C                   MOVEL(P)  SCMNAM        PDNAME
     C                   MOVEL(P)  SCMFSN        PDFSN
     C                   MOVEL(P)  SCTITL        PDTITL
     C                   MOVEL(P)  SCMAD1        PDADR1
     C                   MOVEL(P)  SCDBA         PDDBA1
     C                   MOVEL(P)  SCMAD2        PDADR2
     C                   MOVEL(P)  SCMCTY        PDCTY
     C                   MOVEL(P)  SCMST         PDST
     C                   MOVEL     SCMZPA        PDZIP
     C                   MOVE      SCMZPB        PDZIP
     C                   MOVEL(P)  SCMCTR        PDCTRY
     C                   MOVEL(P)  SCMAD3        PDADR3
     C                   Z-ADD     SCMACD        PDAREA
     C                   Z-ADD     SCMPH#        PDPHN#
     C                   Z-ADD     SCFXAR        PDFXAR
     C                   Z-ADD     SCFAX#        PDFAX#
     C                   MOVEL(P)  SCPARN        PDPARN
     C                   MOVEL(P)  SCTYPE        PDTYPE
     C                   MOVEL(P)  SCSPC1        PDSPCD
     C                   MOVEL(P)  SCTAXO        PDTAXO
     C                   MOVEL(P)  SCLIC#        PDLIC#
     C                   MOVEL(P)  SCSHNM        PDSHNM
     C                   MOVEL(P)  SC1099        PD1099
      /Free
       //*** Begin Add *** 02/09/18 **************************************
        PD835P = SC835pa;
       //*** End   Add *** 02/09/18 **************************************
      /End-Free
     C                   MOVEL(P)  SCWCCE        PDWCCE
     C*
     C                   Z-ADD     SCWCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PDWCDT
     C*
     C                   MOVEL(P)  SCBWC#        PDBWC#
     C                   MOVE      SCBWCT        PDBWCT
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        PDUSER
     C                   MOVE      Q1PGM         PDPGM
     C                   Z-ADD     DSTIME        PDTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PDDT
     C*
     C*  P1 FIELDS
     C*
     C                   Z-ADD     PDFEIN        XXFEIN
     C                   Z-ADD     PDMOD         XXMOD
     C*
     C     P1KY          CHAIN     PP102                              99
     C*
     C*   WRITE/UPDAT P1
     C*
     C     *IN99         IFEQ      '0'
     C*
     C                   EXSR      MVSCP1
     C*
     C                   UPDATE    PP102
     C*
     C                   ELSE
     C*
     C                   CLEAR                   PP101
     C                   EXSR      MVSCP1
     C*
     C                   MOVE      Q1USER        P1IUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P1IDT
     C*
     C                   WRITE     PP101
     C                   FEOD      FMP1P
     C*
     C                   ENDIF
      ***** Beg Add ***** 05/25/12 ***************************************
      /free
       PDemad = scEmad;
      /end-free
      ***** End Add ***** 05/25/12 ***************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCP1 - Move screen fields to FMP1P data base fields
     C*================================================================
     C     MVSCP1        BEGSR
     C*
     C                   Z-ADD     SCFEIN        P1FEIN
     C                   Z-ADD     SCMOD         P1MOD
     C                   MOVEL(P)  SCN#02        P1N#02
     C                   MOVEL(P)  SCSTAT        P1STAT
     C                   MOVEL(P)  SCMCTR        P1MCTR
     C                   MOVEL(P)  SCPNAM        P1NAME
     C                   MOVEL(P)  SCPFSN        P1FSN
     C                   MOVEL(P)  SCPAD1        P1ADR1
     C                   MOVEL(P)  SCPAD2        P1ADR2
     C                   MOVEL(P)  SCPCTY        P1CTY
     C                   MOVEL(P)  SCPST         P1PST
     C                   MOVEL     SCPZPA        P1ZIP
     C                   MOVE      SCPZPB        P1ZIP
     C                   MOVEL     SCPCTR        P1CTRY
     C                   MOVEL     SCPAD3        P1ADR3
     C                   Z-ADD     SCPACD        P1AREA
     C                   Z-ADD     SCPPH#        P1PHN#
     C                   MOVEL(P)  SCAJST        P1ST
     C                   MOVEL(P)  SCSPC2        P1SPC2
     C                   MOVEL(P)  SCSPC3        P1SPC3
     C                   MOVEL(P)  SCSR          P1FL01
     C     SCSMSA        CAT(P)    SCCNTY        XTMP1             9
     C     XTMP1         CAT(P)    SCFCD         P1CNTY
     C*
     C                   MOVEL(P)  SCMCR#        P1MCR#
     C                   MOVE      SCSPRT        P1FL03
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        P1USER
     C                   MOVE      Q1PGM         P1PGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P1DT
     C*
     C                   Z-ADD     DSTIME        P1TIME
     C*
     C                   ENDSR
     C*================================================================
     C* NXTPGM - Subroutine that determines the next pgm to be called.
     C*================================================================
     C     NXTPGM        BEGSR
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C* DETERMINE THE MODE THAT THE NEXT PROGRAMS ARE TO BE CALLED.
     C*
     C     @@MODE        IFEQ      'D'
     C*
     C                   MOVEL     '1'           ##CALL
     C*
     C                   ELSE
     C*
     C                   MOVEL     ' '           ##CALL
     C*
     C                   ENDIF
     C*
     C* MOVE VALUES INTO KEY FIELDS TO CHAIN OUT TO FMSIP
     C*
     C                   MOVEL     SCAJST        KKAJST
     C                   MOVEL     SCTYPE        KKTYPE
     C*
     C                   SELECT
     C*
     C     SCTYPE        WHENEQ    '1'
     C                   MOVEL     SCSPC1        KKSPCD
     C*
     C     SCTYPE        WHENEQ    '4'
     C                   MOVEL     *BLANKS       KKSPCD
     C*
     C                   ENDSL
     C*
     C     SIKY          CHAIN     PSI01                              99
     C*
     C* IF CHAIN IS SUCCESSFUL, THEN F0042C SHOULD BE CALLED.
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   CALL      'F0042R'
     C                   PARM                    ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0          FMP2P
     C                   PARM                    ##RRN2            9 0          FMPDP
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   SELECT
     C*
     C     ##CALL        WHENEQ    '1'
     C                   MOVEL     'D'           @@MODE
     C*
     C     ##CALL        WHENEQ    ' '
     C                   MOVEL     'C'           @@MODE
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C                   SETOFF                                       KL
     C                   Z-ADD     3             SCRN#
     C                   EXSR      GETDTM
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C* IF F0042R IS NOT CALLED, THEN CHECK TO SEE IF PROVIDER
     C* IS PARTICIPATING IN NETWORKS. WHEN SCPARN = 'P' THEN THE
     C* PROVIDER IS PARTICIPATING IN NETWORKS AND N0037R SHOULD
     C* BE CALLED.
     C*
     C     SCPARN        IFEQ      'P'
     C*
     C                   MOVE      SCFEIN        ##FEIN
     C                   MOVE      SCMOD         ##MOD
     C*
     C                   CALL      'N0037R'
     C                   PARM                    ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##FEIN            9
     C                   PARM                    ##MOD             3
     C                   PARM      *BLANKS       ##NWCD            5
     C                   PARM      *BLANKS       ##CNT#           15
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   SETON                                        90
     C                   SETOFF                                       KL
     C                   MOVEL     'D'           @@MODE
     C                   Z-ADD     3             SCRN#
     C                   EXSR      GETDTM
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
      *
      *================================================================
      * OTHFKS - Processing for other functions keys
      *================================================================
     C     OTHFKS        BEGSR
     C*
     C* RESET PLACE CURSOR INDICATORS.
     C*
     C                   MOVEA     '00000'       *IN(30)
     C                   MOVEA     '00000'       *IN(35)
      *
      *  Process F10 search
      *
     C     *INKJ         CASEQ     '1'           KJDO
     C                   ENDCS
      *
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C* KEEP USER FROM F3, IS CALLED FROM WCMS IN ADD MODE.
     C*
     C     Q1PRMS        IFGT      4
     C*
     C     @@CALL        COMP      'A'                                    16
     C*
     C                   ENDIF
     C*
     C* INITIALIZE SCREEN CHARACTERISTIC INDICATORS.
     C* 20-WCMS INSTALLED; 23-DISPLAY NETWORK FIELDS.
     C*
     C                   SETOFF                                       2023
     C                   Z-ADD     *ZEROS        SCRN#
     C*
     C* ADD MODE INDICATOR FOR STANDALONE PROVIDERS(NOT OF WCMS).
     C*
     C     @@MODE        COMP      'A'                                    40    ADD MODE
     C*
     C* DISPLAY MODE INDICATOR.
     C*
     C     @@MODE        COMP      'D'                                    09    DISPLAY MODE
     C*
     C*  Workman's Comp installed
     C*
     C                   CALL      'SA001R'
     C                   PARM                    QQWCIF            1
     C                   PARM                    QQFBIF            1
     C                   PARM                    QQFL01            1
     C                   PARM      *BLANK        QQAR              1            MC AR INST
     C                   PARM      *BLANK        QQNTWK            1            NETWORK INST
     C                   PARM      *BLANK        QQUR              1            PRECERT INST
     C                   PARM      *BLANK        QQMED             1            MEDMGMT INST
     C                   PARM      *BLANK        QQEDI             1            EDI INST
     C                   PARM      *BLANK        QQWCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        QQRHAB            1            NCCI DATA CALL
     C                   PARM      *BLANK        QQMSS             1            NCCI DATA CALL
     C                   PARM      *BLANK        QQFSCH            1            NCCI DATA CALL
     C                   PARM      *BLANK        QQPRVB            1            NCCI DATA CALL
     C                   PARM      *BLANK        QQNPWC            1            NCCI DATA CALL
     C                   PARM      *BLANK        QQOS              1            NCCI DATA CALL
     C                   PARM      *BLANK        QQARLS            1            NCCI DATA CALL
     C                   PARM      *BLANK        QQCCOD            1            NCCI DATA CALL
     C                   PARM      *BLANK        QQCRVW            1            NCCI DATA CALL
     C                   PARM      *BLANK        QQPROM            1            NCCI DATA CALL
     C                   PARM      *BLANK        QQMCRV            1            NCCI DATA CALL
     C                   PARM      *BLANK        QQFREQ            1            NCCI DATA CALL
     C                   PARM      *BLANK        QQCRAC            1            NCCI DATA CALL
     C                   PARM      *BLANK        QQPREO            1            NCCI DATA CALL
     C                   PARM      *BLANK        QQPARN            1            NCCI DATA CALL
     C                   PARM      *BLANK        QQMGRI            1            NCCI DATA CALL
     C                   PARM      *BLANK        QQKCIF            1            NCCI DATA CALL
     C                   PARM      *BLANK        QQCURS            1            NCCI DATA CALL
     C                   PARM      *BLANK        QQPCIB            1            NCCI DATA CALL
     C                   PARM      *BLANK        QQCLT#            3            NCCI DATA CALL
     C                   PARM      *BLANK        QQUCPF            1            NCCI DATA CALL
     C                   PARM      *BLANK        QQDCF             1            NCCI DATA CALL
     C                   PARM      *BLANK        QQAUTB            1            NCCI DATA CALL
     C                   PARM      *BLANK        QQDEIF            1            NCCI DATA CALL
     C                   PARM      *BLANK        QQAURE            1            NCCI DATA CALL
     C                   PARM      *BLANK        QQCRED            1            NCCI DATA CALL
     C                   PARM      *BLANK        QQIDRT            1            NCCI DATA CALL
     C                   PARM      *BLANK        QQCPRV            1            NCCI DATA CALL
     C                   PARM      *BLANK        QQEFT             1            NCCI DATA CALL
     C                   PARM      *BLANK        QQGOVR            1            NCCI DATA CALL
     C                   PARM      *BLANK        QQNCDC            1            NCCI DATA CALL
     C*
     C     QQFL01        IFEQ      'Y'
     C                   MOVE      *ON           *IN20
     C                   ENDIF
     C*
     C*  Check flag for display/not of Participating in Network Field
     C*
     C     QQNTWK        IFEQ      'Y'
     C                   MOVE      *ON           *IN23
     C                   ENDIF
     C*
     C*
     C     QQNCDC        IFEQ      'Y'
     C                   MOVE      *ON           *IN24
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C*
     C     @@MODE        WHENEQ    'S'                                          CHG STATUS
     C                   EXSR      CHGSTS
     C                   RETURN
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* SCRNUM - Tells program what screen is being processed.
     C*================================================================
     C     SCRNUM        BEGSR
     C*
     C*
     C                   SELECT
     C*
     C     SCRN#         WHENLT    3
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C                   ADD       1             SCRN#             1 0
     C*
     C                   ELSE
     C*
     C                   SUB       1             SCRN#
     C*
     C                   ENDIF
     C*
     C     SCRN#         WHENEQ    3
     C     *INKL         ANDEQ     *ON
     C*
     C                   SUB       1             SCRN#
     C*
     C                   ENDSL
     C*
     C                   ENDSR
      *================================================================
      * SRCHFC - Search Facility Code
      *================================================================
     C     SRCHFC        BEGSR
      *
      *  Ajudication State must be valid
      *
      *
      * ZZZ
     C                   MOVEL(P)  '2'           ##CALL
     C                   MOVEL(P)  'F2355R'      ##PGM
     C                   MOVEL(P)  'FTFTP'       ##FIL1
     C                   MOVEL(P)  'FTABL'       ##FIL2
     C     SCPST         IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCPST         ##ST              2
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  SCMST         ##ST
     C*
     C                   ENDIF
     C                   MOVEL(P)  *BLANKS       ##CODE
      *
     C                   CALL      'F0601C'      PGMLS1
      *
     C     ##CODE        IFNE      *BLANKS
     C                   MOVEL(P)  ##CODE        SCFCD
     C                   MOVEL(P)  ##DESC        SCFDS
     C                   MOVE      '1'           *IN30
     C                   ELSE
     C                   MOVE      '1'           *IN30
     C                   ENDIF                                                  ##CODE
      *
      *
     C                   ENDSR
      *
      *
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF
     C*
     C* EXECUTE SUBROUTINE NXTPGM TO DETERMINE IF F0042R OR N0037R
     C* SHOULD BE CALLED. IT IS POSSIBLE THAT NEITHER PGM WILL BE
     C* CALLED.
     C*
     C     SCRN#         IFEQ      3
     C*
     C                   MOVE      @@RRN         ##RRN2
     C*
     C                   EXSR      NXTPGM
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FMPDP
     C*
     C* EXECUTE SUBROUTINE NXTPGM TO DETERMINE IF F0042R OR N0037R
     C* SHOULD BE CALLED. IT IS POSSIBLE THAT NEITHER PGM WILL BE
     C* CALLED.
     C*
     C                   MOVE      DBRRN         ##RRN2
     C*
     C                   EXSR      NXTPGM
     C*
     C                   MOVEL     *ON           OUTFLG
     C*
     C                   ENDSR
     C*================================================================
     C* SETTBL - SET THE TABLE ID TO THE SCAJST FIELD.
     C*================================================================
     C     SETTBL        BEGSR
     C*
     C* DETERMINE THE TBID'S FOR SPECIALTY CODE 3
     C*
     C     SCAJST        CAT(P)    'SC':0        SPC3              4
     C                   MOVEL(P)  SPC3          XXTBID
     C     GIKY          CHAIN     PGI01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'ZZSC'        SPC3
     C                   ENDIF
     C                   ENDSR
