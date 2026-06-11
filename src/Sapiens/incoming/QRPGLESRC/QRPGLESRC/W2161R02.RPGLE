     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE') bnddir('WTBNDDIR')
     H/define CONVERT
     H/Title W2161R02 - A/R Transaction Code Master Information
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/29/24 C:344 Support: PHW115806 (CoveRisk)                  *
     H*                Add revisions from project 129WAK:             *
     H*                - Add A/R Security Type fields to the W2161F02 *
     H*                  display file.                                *
     H*                - Update Security Type fields entered from the *
     H*                  display file.                                *
     H* 03/16/21 C:062 Project: WC2663                                *
     H*                Add exclude from invoice cloud.                *
     H*                                                               *
     H* 04/30/12 C:255 Support: WA099 79570                           *
     H*                Add an edit to ensure only 1 billing type code *
     H*                is entered for 4th to 7th assessments.         *
     H*                                                               *
     H* 03/24/10 C:136 PROJECT: 25WQI - JDEDWARDS DOWNLOAD B2B        *
     H*                ADD G/L FIELDS FOR JDEDWARDS DOWNLOAD (CLIENT  *
     H*                SPECIFIC)                                      *
     H*                                                               *
     H* 12/11/08 C:33  Project; WC1769                                *
     H*                Add 6th and 7th assessments.                   *
     H*                                                               *
     H* 05/16/08 C:136 SUPPORT: WA146 63372                           *
     H*                ENABLE SEARCH FUNCTION WHEN G/L IS NOT         *
     H*                INSTALLED PER CG                               *
     H*                                                               *
     H* 07/18/07 C:136 PROJECT: WC1491 (IND DEP CASH DIST)            *
     H*               -ADD REFUND AND VOID/STOP PAY TO DEPOSIT CAT    *
     H*               -DISABLE G/L EDIT FOR REFUNDS                   *
     H*                                                               *
     H* 06/27/06 C:65  PROJECT: 109WDQ                                *
     H*                ADD A NEW 'G' CATEGORY FOR STOP GAP FEES.      *
     H*                                                               *
     H* 08/23/05 C:65  PROJECT: 114WHO                                *
     H*                ALLOW TYPES B AND P FOR RETRO CATEGORY AND     *
     H*                CHANGE CLAIMS REPLENISHMENT CATEGORY TO        *
     H*                TYPES B AND P.                                 *
     H*                                                               *
     H* 07/02/04 C:136 PROJECT: WC1227                                *
     H*                ADD CODE FOR 4TH AND 5TH ASSESSMENTS.          *
     H*                                                               *
     H* 02/18/04 C:136 PROJECT: WC1033                                *
     H*                ADD 'T' TRANSACTION TYPE CODE.                 *
     H*                                                               *
     H* 06/29/02 C:136 PROJECT: WC1080                                *
     H*                REMOVE EDIT THAT LIMITS MORE THAN ONE ACTIVE   *
     H*                CODE WITH A TRANSACTION TYPE OF 'F' AND        *
     H*                CATEGORY OF 'R'.                               *
     H*                                                               *
     H* 01/02/01 C:65  PROJECT: WC1024                                *
     H*                 ADD 3RD ASSESSMENT AS A CATEGORY.             *
     H*                                                               *
     H* 10/17/00  C:33  PROJECT: WC577                                *
     H*                 ONLY REQUIRE G/L ACCOUNTS FOR 'R' TYPES.      *
     H*                                                               *
     H* 04/12/99  C:65  PROJECT WC875                                 *
     H*                 ALLOW F AND Q TYPES FOR CATEGORY D TO ENABLE  *
     H*                 STATE TAX ON DEPOSIT A/R CODES.               *
     H*                                                               *
     H* 12/31/98  C:78   SUPPORT #15558                               *
     H*                  EDIT FOR CATEGORY TYPE = 'E' SHOULD ALSO     *
     H*                  ALLOW AN A/R TRANSACTION CODE TYPE 'N' AND   *
     H*                  'R'.                                         *
     H*                                                               *
     H* 10/16/98  C:35   PROJECT: SUPPORT                             *
     H*                  STANDARD INDICATOR 40 (ADD) WAS ALSO BEING   *
     H*                  USED AS PART OF THE ERROR CHECKING, CAUSING  *
     H*                  THE PROGRAM TO DISPLAY THE ERROR INDICATING  *
     H*                  THAT AN A/R CODE ALREADY EXISTS FOR THE      *
     H*                  CATEGORY TYPE WHEN ACTUALLY IT DID NOT.      *
     H*                                                               *
     H* 06/02/98 C:90 PROJECT WC837                                   *
     H*               VERIFY CATEGORY TYPES WITH CTCTYP IN FILE W2162R*
     H*                                                               *
     H* 04/07/98 C:85 PROJECT WC577                                   *
     H*               ADD THREE NEW CATEGORY TYPES: X, F, AND O.      *
     H*                                                               *
     H* 03/10/98 C:85 PROJECT WC577                                   *
     H*               VERSION 3 STANDARDS, CORRECTED PROGRAM TITLE.   *
     H*                                                               *
     H* 12/23/97  C:65  PROJECT WC776                                 *
     H*                 ADDED 1ST AND 2ND ASSESSMENT CATEGORIES.      *
     H*                                                               *
     H* 11/24/97  C:62  PROJECT WC768                                 *
     H*                 ADDED API INTERFACE TO BKCAL1.                *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     FW2161F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWTTTP     UF A E             DISK
     F                                     RENAME(PTT01:DBRCDF)
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SWC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SPC01)
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PSY01)
      **** BEGIN ADD **** 06/02/98 ************************************
     FWTCTL     IF   E           K DISK
     FWTCTP     IF   E             DISK
     F                                     RENAME(PCT01:PCT02)
      **** END   ADD **** 06/02/98 ************************************
      /COPY SCOPYMT01D
     D*** Begin Add *** 03/16/21 **************************************
      /Copy sprModule
     D*** End   Add *** 03/16/21 **************************************
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('A/R Transaction-
     D*** BEGIN CHG *** 03/10/98 **************************************
     D**********    'Code Master Inf-
     D**********    'ormation'
     D                                      Code Master In-
     D                                     formation')
     D*** END   CHG *** 03/10/98 **************************************
     D COMAPI          C                   CONST('WA001R')
      //**** Beg Chg **** 08/29/24 ***** 115806 ************************
     D****#OFSCR          C                   CONST(1)
     D #ofSCR          s              1  0 inz
      //**** End Chg **** 08/29/24 ***** 115806 ************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*** BEGIN ADD *** 11/24/97 **************************************
     C*
     C*  AP001R PARM LIST
     C*
     C     AP01PL        PLIST
     C                   PARM                    ##CO##
     C                   PARM                    ##FAC#
     C                   PARM                    ##DPT#
     C                   PARM                    ##ACC#
     C                   PARM                    ##SUB#
     C                   PARM                    ##RTC             1
     C*** END   ADD *** 11/24/97 **************************************
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     TTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##KEY
     C*
     C     TTKY1         KLIST
     C                   KFLD                    ##CO#
     C*
     C     SYKY          KLIST
     C                   KFLD                    ##SKY
     C*
     C     GLKY          KLIST
     C                   KFLD                    ##GKY
     C*
      **** BEGIN ADD **** 06/02/98 ************************************
     C     CTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKCTYP
      **** END   ADD **** 06/02/98 ************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    RTKEY         ##KEY
     C     *LIKE         DEFINE    GLKEY         ##GKY
     C     *LIKE         DEFINE    SYKEY         ##SKY
     C     *LIKE         DEFINE    RTDCO         ##CO#
     C     *LIKE         DEFINE    RTDCO         ##CO##
     C     *LIKE         DEFINE    RTDFAC        ##FAC#
     C     *LIKE         DEFINE    RTDDPT        ##DPT#
     C     *LIKE         DEFINE    RTDACC        ##ACC#
     C     *LIKE         DEFINE    RTDSUB        ##SUB#
     C     *LIKE         DEFINE    RTDCO         @@CO#
     C     *LIKE         DEFINE    RTDFAC        @@FAC#
     C     *LIKE         DEFINE    RTDDPT        @@DPT#
     C     *LIKE         DEFINE    RTDACC        @@ACC#
     C     *LIKE         DEFINE    RTDSUB        @@SUB#
      **** BEGIN ADD **** 06/02/98 ************************************
     C     *LIKE         DEFINE    CTCTYP        KKCTYP
      **** END   ADD **** 06/02/98 ************************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     TTSTAT        IFEQ      'I'
     C                   MOVE      'A'           TTSTAT
     C                   ELSE
     C                   MOVE      'I'           TTSTAT
     C                   ENDIF                                                  TTSTAT
     C*
     C                   MOVEL     Q1USER        TTUSER
     C                   MOVEL     Q1PGM         TTPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TTDT
     C*
     C                   MOVEL     DSTIME        TTTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99
     C*
     C     *IN16         IFEQ      *ON
     C*
     C                   ENDIF                                                  *IN16
     C*
     C                   ENDSR                                                  CHGSTS
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C* RESERVE TYPE CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCKEY         IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCKEY
     C*
     C* CHECK TO BE SURE TRANSACTION CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCKEY         ##KEY
     C*
     C   40
     CANN90TTKY          CHAIN     PTT01                              99
     C   40
     CANN90
     CANN99              SETON                                        72  90
     C*
     C* A/R TRANSACTION TYPE CODE DESCRIPTION CANNOT BE BLANK
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCDESC
     C*
     C*** BEGIN DEL *** 11/14/97 **************************************
     C*
     C*  THE A/R TRANSACTION TYPE RECORD TYPE MUST BE A,B,D,H,I,N,V,
     C*  P,F, OR Q.
     C*
     C**** N90   SCTYPE    IFNE 'A'
     C********** SCTYPE    ANDNE'B'
     C********** SCTYPE    ANDNE'D'
     C********** SCTYPE    ANDNE'H'
     C********** SCTYPE    ANDNE'I'
     C********** SCTYPE    ANDNE'P'
     C********** SCTYPE    ANDNE'N'
     C********** SCTYPE    ANDNE'V'
     C********** SCTYPE    ANDNE'R'
     C********** SCTYPE    ANDNE'F'
     C********** SCTYPE    ANDNE'Q'
     C*
     C******************** SETON                     74  90
     C*
     C******************** ENDIF                           SCTYPE
     C*** END   DEL *** 11/14/97 **************************************
     C*
      **** BEGIN CHG **** 06/02/98 ************************************
     C***********  THE A/R CATEGORY TYPE MUST BE P,E,M,S,D,R,L,C, OR T.
     C*
     C**** N90   SCCTYP    IFNE 'P'
     C********** SCCTYP    ANDNE'E'
     C********** SCCTYP    ANDNE'M'
     C********** SCCTYP    ANDNE'S'
     C********** SCCTYP    ANDNE'D'
     C********** SCCTYP    ANDNE'R'
     C********** SCCTYP    ANDNE'L'
     C********** SCCTYP    ANDNE'C'
     C********** SCCTYP    ANDNE'T'
     C*** BEGIN DEL *** 11/14/97 **************************************
     C********** SCCTYP    ANDNE'1'
     C********** SCCTYP    ANDNE'2'
     C********** SCCTYP    ANDNE'3'
     C*** END   DEL *** 11/14/97 **************************************
     C*** BEGIN ADD ***  12/23/97  ************************************
     C********** SCCTYP    ANDNE'1'
     C********** SCCTYP    ANDNE'2'
     C*** END ADD *****  12/23/97  ************************************
     C*** BEGIN ADD ***  04/07/98  ************************************
     C********** SCCTYP    ANDNE'X'
     C********** SCCTYP    ANDNE'F'
     C********** SCCTYP    ANDNE'O'
     C*** END ADD *****  04/07/98  ************************************
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVE      SCCTYP        KKCTYP
     C     CTKY          CHAIN     PCT01                              60
     C     *IN60         IFEQ      *ON
     C*
      **** END   CHG **** 06/02/98 ************************************
     C                   SETON                                        77  90
     C*
      **** BEGIN CHG **** 06/02/98 ************************************
     C                   ENDIF                                                  IFEQ *IN60
     C******************** ENDIF                           SCCTYP
     C                   ENDIF                                                  IFEQ *IN90
      **** END   CHG **** 06/02/98 ************************************
     C*
     C*** BEGIN ADD *** 11/14/97 **************************************
     C*
     C*  CHECK FOR PROPER ASSIGNMENT OF CATEGORY CODES
     C*
     C*  IF CATEGORY TYPE EQ 'P'
     C*
     C     SCCTYP        IFEQ      'P'
     C     SCTYPE        ANDNE     'B'
     C     SCTYPE        ANDNE     'P'
     C     SCTYPE        ANDNE     'A'
     C     SCTYPE        ANDNE     'D'
     C     SCTYPE        ANDNE     'V'
     C     SCTYPE        ANDNE     'N'
     C     SCTYPE        ANDNE     'R'
      *** BEG ADD ***** 02/18/04  *********************************************
     C     SCTYPE        ANDNE     'T'
      *** END ADD ***** 02/18/04  *********************************************
     C*
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN39
     C*
     C                   ENDIF                                                  SCCTYP IFEQ 'P'
     C*
     C*  IF CATEGORY TYPE EQ 'D'
     C*
     C     SCCTYP        IFEQ      'D'
     C     SCTYPE        ANDNE     'H'
     C     SCTYPE        ANDNE     'I'
     C*** BEGIN ADD ***  04/12/99  ************************************
     C     SCTYPE        ANDNE     'F'
     C     SCTYPE        ANDNE     'Q'
     C*** END ADD *****  04/12/99  ************************************
     C*** BEG ADD *****  07/18/07  ************************************
     C     SCTYPE        ANDNE     'R'
     C     SCTYPE        ANDNE     'T'
     C*** END ADD *****  07/18/07  ************************************
     C*
     C                   MOVE      *ON           *IN36
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF                                                  SCCTYP IFEQ 'D'
     C*** BEGIN ADD ***  04/07/98  ************************************
     C*
     C*  IF CATEGORY TYPE EQ 'X'
     C*
     C     SCCTYP        IFEQ      'X'
     C     SCTYPE        ANDNE     'F'
     C     SCTYPE        ANDNE     'Q'
     C*
     C                   MOVE      *ON           *IN38
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF                                                  SCCTYP IFEQ 'D'
     C*
     C*  IF CATEGORY TYPE EQ 'F'
     C*
     C     SCCTYP        IFEQ      'F'
     C     SCTYPE        ANDNE     'F'
     C     SCTYPE        ANDNE     'Q'
     C*
     C                   MOVE      *ON           *IN38
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF                                                  SCCTYP IFEQ 'D'
     C*** END ADD *****  04/07/98  ************************************
     C*
     C*  IF CATEGORY TYPE EQ 'E'
     C*
     C     SCCTYP        IFEQ      'E'
     C     SCTYPE        IFNE      'B'
     C     SCTYPE        ANDNE     'P'
      ****** BEGIN ADD ***** 12/31/98 ******************************************
     C     SCTYPE        ANDNE     'N'
     C     SCTYPE        ANDNE     'R'
      ****** END   ADD ***** 12/31/98 ******************************************
     C*
     C                   MOVE      *ON           *IN37
     C                   MOVE      *ON           *IN90
     C*
     C                   ELSE
     C*
      **** BEGIN CHG **** 10/16/98 *********************************************
     C******************** MOVE *ON       *IN40
     C                   MOVE      *ON           *IN49
      **** END   CHG **** 10/16/98 *********************************************
     C*
     C                   ENDIF                                                  SCTYPE IFNE 'B'
     C*
     C     SCTYPE        IFEQ      'B'
     C*
     C                   EXSR      READAR
     C*
     C                   ENDIF                                                  SCTYPE IFEQ 'B'
     C*
     C                   ENDIF                                                  SCCTYP IFEQ 'E'
     C*
     C*  IF CATEGORY TYPE EQ 'M'
     C*
     C     SCCTYP        IFEQ      'M'
     C     SCTYPE        IFNE      'F'
     C     SCTYPE        ANDNE     'Q'
     C*
     C                   MOVE      *ON           *IN38
     C                   MOVE      *ON           *IN90
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN42
     C*
     C                   ENDIF                                                  SCTYPE IFNE 'F'
     C*
     C     SCTYPE        IFEQ      'F'
     C*
     C                   EXSR      READAR
     C*
     C                   ENDIF                                                  SCTYPE IFEQ 'F'
     C*
     C*
     C                   ENDIF                                                  SCCTYP IFNE 'F'
     C*
     C*  IF CATEGORY TYPE EQ 'S'
     C*
     C     SCCTYP        IFEQ      'S'
     C     SCTYPE        IFNE      'F'
     C     SCTYPE        ANDNE     'Q'
     C*
     C                   MOVE      *ON           *IN38
     C                   MOVE      *ON           *IN90
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN44
     C*
     C                   ENDIF                                                  SCTYPE IFNE 'F'
     C*
     C     SCTYPE        IFEQ      'F'
     C*
     C                   EXSR      READAR
     C*
     C                   ENDIF                                                  SCTYPE IFEQ 'F'
     C*
     C                   ENDIF                                                  SCCTYP IFEQ 'S'
     C*
     C*  IF CATEGORY TYPE EQ 'R'
     C*
     C     SCCTYP        IFEQ      'R'
     C     SCTYPE        IFNE      'F'
     C     SCTYPE        ANDNE     'Q'
     C*** BEGIN CHG ***  08/23/05  ************************************
     C     SCTYPE        ANDNE     'B'
     C     SCTYPE        ANDNE     'P'
     C*
     C**********           MOVE *ON       *IN38
     C                   MOVE      *ON           *IN29
     C*** END CHG *****  08/23/05  ************************************
     C                   MOVE      *ON           *IN90
     C*
     C                   ELSE
     C*** BEG DEL **** 06/29/02 ********************************************
     C******************** MOVE *ON       *IN46
     C*** END DEL **** 06/29/02 ********************************************
     C                   ENDIF                                                  SCTYPE IFNE 'F'
     C*
     C     SCTYPE        IFEQ      'F'
     C*
     C                   EXSR      READAR
     C*
     C                   ENDIF                                                  SCTYPE IFEQ 'F'
     C*
     C                   ENDIF                                                  SCCTYP IFEQ 'R'
     C*
     C*  IF CATEGORY TYPE EQ 'L'
     C*
     C     SCCTYP        IFEQ      'L'
     C     SCTYPE        ANDNE     'F'
     C     SCTYPE        ANDNE     'Q'
     C*
     C                   MOVE      *ON           *IN38
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF                                                  SCCTYP IFEQ 'L'
     C*** BEGIN ADD ***  06/27/06  ************************************
     C*
     C*  IF CATEGORY TYPE EQ 'G'
     C*
     C     SCCTYP        IFEQ      'G'
     C     SCTYPE        ANDNE     'F'
     C     SCTYPE        ANDNE     'Q'
     C*
     C                   MOVE      *ON           *IN38
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF                                                  SCCTYP IFEQ 'L'
     C*** END ADD *****  06/27/06  ************************************
     C*
     C*  IF CATEGORY TYPE EQ 'C'
     C*
     C     SCCTYP        IFEQ      'C'
     C*** BEGIN CHG ***  08/23/05  ************************************
     C********** SCTYPE    IFNE 'F'
     C********** SCTYPE    ANDNE'Q'
     C     SCTYPE        IFNE      'B'
     C     SCTYPE        ANDNE     'P'
     C*
     C**********           MOVE *ON       *IN38
     C                   MOVE      *ON           *IN28
     C*** END CHG *****  08/23/05  ************************************
     C*
     C                   MOVE      *ON           *IN90
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN48
     C*
     C                   ENDIF                                                  SCTYPE IFNE 'F'
     C*
     C     SCTYPE        IFEQ      'F'
     C*
     C                   EXSR      READAR
     C*
     C                   ENDIF                                                  SCTYPE IFEQ 'F'
     C*
     C                   ENDIF                                                  SCCTYP IFEQ 'C'
     C*
     C*  IF CATEGORY TYPE EQ 'T'
     C*
     C     SCCTYP        IFEQ      'T'
     C     SCTYPE        IFNE      'F'
     C     SCTYPE        ANDNE     'Q'
     C*
     C                   MOVE      *ON           *IN38
     C                   MOVE      *ON           *IN90
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN50
     C*
     C                   ENDIF                                                  SCTYPE IFNE 'F'
     C*
     C     SCTYPE        IFEQ      'F'
     C*
     C                   EXSR      READAR
     C*
     C                   ENDIF                                                  SCTYPE IFEQ 'F'
     C*
     C                   ENDIF                                                  SCCTYP IFEQ 'T'
     C*
     C*** END   ADD *** 11/14/97 **************************************
     C*** BEGIN ADD ***  12/23/97  ************************************
     C*
     C*  IF CATEGORY TYPE EQ '1'
     C*
     C     SCCTYP        IFEQ      '1'
     C     SCTYPE        IFNE      'F'
     C     SCTYPE        ANDNE     'Q'
     C*
     C                   MOVE      *ON           *IN38
     C                   MOVE      *ON           *IN90
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN52
     C*
     C                   ENDIF                                                  SCTYPE IFNE 'F'
     C*
     C     SCTYPE        IFEQ      'F'
     C*
     C                   EXSR      READAR
     C*
     C                   ENDIF                                                  SCTYPE IFEQ 'F'
     C*
     C                   ENDIF                                                  SCCTYP IFEQ 'T'
     C*
     C*  IF CATEGORY TYPE EQ '2'
     C*
     C     SCCTYP        IFEQ      '2'
     C     SCTYPE        IFNE      'F'
     C     SCTYPE        ANDNE     'Q'
     C*
     C                   MOVE      *ON           *IN38
     C                   MOVE      *ON           *IN90
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN54
     C*
     C                   ENDIF                                                  SCTYPE IFNE 'F'
     C*
     C     SCTYPE        IFEQ      'F'
     C*
     C                   EXSR      READAR
     C*
     C                   ENDIF                                                  SCTYPE IFEQ 'F'
     C*
     C                   ENDIF                                                  SCCTYP IFEQ 'T'
     C*
     C*** END ADD *****  12/23/97  ************************************
     C*** BEGIN ADD ***  01/02/01  ************************************
     C*
     C*  IF CATEGORY TYPE EQ '3'
     C*
     C     SCCTYP        IFEQ      '3'
     C     SCTYPE        IFNE      'F'
     C     SCTYPE        ANDNE     'Q'
     C*
     C                   MOVE      *ON           *IN38
     C                   MOVE      *ON           *IN90
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN55
     C*
     C                   ENDIF                                                  SCTYPE IFNE 'F'
     C*
     C     SCTYPE        IFEQ      'F'
     C*
     C                   EXSR      READAR
     C*
     C                   ENDIF                                                  SCTYPE IFEQ 'F'
     C*
     C                   ENDIF                                                  SCCTYP IFEQ 'T'
     C*
     C*** END ADD *****  01/02/01  ************************************
     C*** BEG ADD *****  07/02/04  ************************************
     C*
     C*  IF CATEGORY TYPE EQ '4'
     C*
     C     SCCTYP        IFEQ      '4'
     C     SCTYPE        IFNE      'F'
     C     SCTYPE        ANDNE     'Q'
     C*
     C                   MOVE      *ON           *IN38
     C                   MOVE      *ON           *IN90
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN56
     C*
     C                   ENDIF                                                  SCTYPE IFNE 'F'
     C*
     C     SCTYPE        IFEQ      'F'
     C*
     C                   EXSR      READAR
     C*
     C                   ENDIF                                                  SCTYPE IFEQ 'F'
     C*
     C                   ENDIF                                                  SCCTYP IFEQ 'T'
     C*
     C*  IF CATEGORY TYPE EQ '5'
     C*
     C     SCCTYP        IFEQ      '5'
     C     SCTYPE        IFNE      'F'
     C     SCTYPE        ANDNE     'Q'
     C*
     C                   MOVE      *ON           *IN38
     C                   MOVE      *ON           *IN90
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN57
     C*
     C                   ENDIF                                                  SCTYPE IFNE 'F'
     C*
     C     SCTYPE        IFEQ      'F'
     C*
     C                   EXSR      READAR
     C*
     C                   ENDIF                                                  SCTYPE IFEQ 'F'
     C*
     C                   ENDIF                                                  SCCTYP IFEQ 'T'
     C*** END ADD *****  07/02/04  ************************************
     C***  Begin Add  ***  12/11/08  **************************************
     C*
     C*  IF CATEGORY TYPE EQ '6'
     C*
     C     SCCTYP        IFEQ      '6'
     C     SCTYPE        IFNE      'F'
     C     SCTYPE        ANDNE     'Q'
     C*
     C                   MOVE      *ON           *IN38
     C                   MOVE      *ON           *IN90
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN58
     C*
     C                   ENDIF                                                  SCTYPE IFNE 'F'
     C*
     C     SCTYPE        IFEQ      'F'
     C*
     C                   EXSR      READAR
     C*
     C                   ENDIF                                                  SCTYPE IFEQ 'F'
     C*
     C                   ENDIF                                                  SCCTYP IFEQ '6'
     C*
     C*  IF CATEGORY TYPE EQ '7'
     C*
     C     SCCTYP        IFEQ      '7'
     C     SCTYPE        IFNE      'F'
     C     SCTYPE        ANDNE     'Q'
     C*
     C                   MOVE      *ON           *IN38
     C                   MOVE      *ON           *IN90
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN59
     C*
     C                   ENDIF                                                  SCTYPE IFNE 'F'
     C*
     C     SCTYPE        IFEQ      'F'
     C*
     C                   EXSR      READAR
     C*
     C                   ENDIF                                                  SCTYPE IFEQ 'F'
     C*
     C                   ENDIF                                                  SCCTYP IFEQ '7'
     C***  End   Add  ***  12/11/08  **************************************
     C*
     C*  THE EXCLUDE FROM CALCULATION FLAG MUST BE 'Y' OR ' '
     C*
     C  N90SCEXDS        IFNE      'Y'
     C     SCEXDS        ANDNE     *BLANK
     C*
     C                   SETON                                        82  90
     C*
     C                   ENDIF                                                  SCEXDS
     C*
     C*  THE EXCLUDE FROM AGT COMMISSIONS FLAG MUST BE 'Y' OR ' '
     C*
     C  N90SCECFL        IFNE      'Y'
     C     SCECFL        ANDNE     *BLANK
     C*
     C                   SETON                                        83  90
     C*
     C                   ENDIF                                                  SCEXDS
      *
      ***** BEG ADD ******* 03/24/10 **********************************
     C  N90CLTID         IFEQ      '025'
     C  N90SC1@TY        IFEQ      *BLANKS
     C                   SETON                                        9084
     C                   ENDIF                                                  SC1@TY
     C  N90SC1DCT        IFEQ      *BLANKS
     C                   SETON                                        9085
     C                   ENDIF                                                  SC1DCT
     C  N90SC1OBJ        IFEQ      *BLANKS
     C                   SETON                                        9086
     C                   ENDIF                                                  SC1OBJ
     C  N90SC1GLC        IFEQ      *BLANKS
     C                   SETON                                        9087
     C                   ENDIF                                                  SC1GLC
     C                   ENDIF                                                  CLTID = 025
      ***** END ADD ******* 03/24/10 **********************************
     C*
     C*  THE G/L DEBIT ACCOUNT MUST BE VALID
     C*
      *** BEG CHG ***** 07/18/07 *************************************
     C*****N90****IN16     IFEQ '1'
     C   90
     CANN90*IN16         IFEQ      '1'
      *** END CHG ***** 07/18/07 *************************************
     C*
     C***  BEGIN ADD  ***  10/17/00  *********************************
     C     SCTYPE        IFEQ      'R'
     C     SCDCO         ORNE      *ZERO
     C     SCDFAC        ORNE      *ZERO
     C     SCDDPT        ORNE      *ZERO
     C     SCDACC        ORNE      *ZERO
     C     SCDSUB        ORNE      *ZERO
     C***  END   ADD  ***  10/17/00  *********************************
     C*
     C                   MOVE      SCDCO         ##CO##
     C                   MOVE      SCDFAC        ##FAC#
     C                   MOVE      SCDDPT        ##DPT#
     C                   MOVE      SCDACC        ##ACC#
     C                   MOVE      SCDSUB        ##SUB#
     C*
     C*** BEGIN CHG *** 11/24/97 **************************************
     C*
     C  N90              CALL      'AP001R'      AP01PL
     C*
     C  N90##RTC         IFEQ      '1'
     C*
     C                   SETON                                        7590
     C*
     C                   ENDIF                                                  ##RTC
     C*** END   CHG *** 11/24/97 **************************************
     C*
     C***  BEGIN ADD  ***  10/17/00  *********************************
     C                   ENDIF                                                  SCTYPE=R
     C***  END   ADD  ***  10/17/00  *********************************
     C*
     C                   ENDIF                                                  DEBIT ACCT
     C*
     C*  THE G/L CREDIT ACCOUNT MUST BE VALID
     C*
      *** BEG CHG ***** 07/18/07 *************************************
     C*****N90****IN16     IFEQ '1'
     C   90
     CANN90*IN16         IFEQ      '1'
      *** END CHG ***** 07/18/07 *************************************
     C*
     C***  BEGIN ADD  ***  10/17/00  *********************************
     C     SCTYPE        IFEQ      'R'
     C     SCCCO         ORNE      *ZERO
     C     SCCFAC        ORNE      *ZERO
     C     SCCDPT        ORNE      *ZERO
     C     SCCACC        ORNE      *ZERO
     C     SCCSUB        ORNE      *ZERO
     C***  END   ADD  ***  10/17/00  *********************************
     C*
     C                   MOVE      SCCCO         ##CO##
     C                   MOVE      SCCFAC        ##FAC#
     C                   MOVE      SCCDPT        ##DPT#
     C                   MOVE      SCCACC        ##ACC#
     C                   MOVE      SCCSUB        ##SUB#
     C*
     C*** BEGIN CHG *** 11/24/97 **************************************
     C  N90              CALL      'AP001R'      AP01PL
     C*
     C  N90##RTC         IFEQ      '1'
     C*
     C                   SETON                                        7690
     C*
     C                   ENDIF                                                  ##RTC
     C*** END   CHG *** 11/24/97 **************************************
     C*
     C***  BEGIN ADD  ***  10/17/00  *********************************
     C                   ENDIF                                                  SCTYPE=R
     C***  END   ADD  ***  10/17/00  *********************************
     C*
     C                   ENDIF                                                  CREDIT ACCT
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90TTDT          IFGT      SAVEDT
     C     TTDT          OREQ      SAVEDT
     C     TTTIME        ANDGT     SAVETM
     C                   SETON                                        9078
     C                   ENDIF
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     TTKEY         SCKEY
     C                   MOVEL     TTDESC        SCDESC
     C                   MOVEL     TTTYPE        SCTYPE
     C                   MOVEL     TTCTYP        SCCTYP
     C                   MOVEL     TTEXDS        SCEXDS
     C                   MOVEL     TTECFL        SCECFL
     C                   MOVEL     TTSTAT        SCSTAT
     C                   MOVEL     TTDCO         SCDCO
     C                   MOVEL     TTDFAC        SCDFAC
     C                   MOVEL     TTDDPT        SCDDPT
     C                   MOVEL     TTDACC        SCDACC
     C                   MOVEL     TTDSUB        SCDSUB
     C                   MOVEL     TTCCO         SCCCO
     C                   MOVEL     TTCFAC        SCCFAC
     C                   MOVEL     TTCDPT        SCCDPT
     C                   MOVEL     TTCACC        SCCACC
     C                   MOVEL     TTCSUB        SCCSUB
       //***** BEG ADD ******* 08/29/24 **** 115806 *******************
       If #ofSCR = 2;
          scMINS = TTmins;
          scTY01 = TTty01;
          scTY02 = TTty02;
          scTY03 = TTty03;
          scTY04 = TTty04;
          scTY05 = TTty05;
          scTY06 = TTty06;
       Endif;
       //***** END ADD ******* 08/29/24 **** 115806 *******************
      ***** BEG ADD ******* 03/24/10 **********************************
     C                   MOVEL(P)  TT1@TY        SC1@TY
     C                   MOVEL(P)  TT1DCT        SC1DCT
     C                   MOVEL(P)  TT1OBJ        SC1OBJ
     C                   MOVEL(P)  TT1GLC        SC1GLC
      ***** END ADD ******* 03/24/10 **********************************
      *** Begin Add *** 03/16/21 **************************************
     C                   MOVEL(P)  TTEXIC        SCEXIC
      *** End   Add *** 03/16/21 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCKEY
     C                   MOVEL     *BLANKS       SCDESC
     C                   MOVEL     *BLANKS       SCTYPE
     C                   MOVEL     *BLANKS       SCCTYP
     C                   MOVEL     *BLANKS       SCEXDS
     C                   MOVEL     *BLANKS       SCECFL
     C                   MOVEL     'A'           SCSTAT
     C                   MOVE      *ZERO         SCDCO
     C                   MOVE      *ZERO         SCDFAC
     C                   MOVE      *ZERO         SCDDPT
     C                   MOVE      *ZERO         SCDACC
     C                   MOVE      *ZERO         SCDSUB
     C                   MOVE      *ZERO         SCCCO
     C                   MOVE      *ZERO         SCCFAC
     C                   MOVE      *ZERO         SCCDPT
     C                   MOVE      *ZERO         SCCACC
     C                   MOVE      *ZERO         SCCSUB
      ***** BEG ADD ******* 03/24/10 **********************************
     C                   MOVEL(P)  *BLANKS       SC1@TY
     C                   MOVEL(P)  *BLANKS       SC1DCT
     C                   MOVEL(P)  *BLANKS       SC1OBJ
     C                   MOVEL(P)  *BLANKS       SC1GLC
      ***** END ADD ******* 03/24/10 **********************************
      *** Begin Add *** 03/16/21 **************************************
     C                   MOVEL(P)  *BLANKS       SCEXIC
      *** End   Add *** 03/16/21 **************************************
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         TTCO#
     C                   MOVEL     SCKEY         TTKEY
     C                   MOVEL     SCDESC        TTDESC
     C                   MOVEL     SCTYPE        TTTYPE
     C                   MOVEL     SCCTYP        TTCTYP
     C                   MOVEL     SCEXDS        TTEXDS
     C                   MOVEL     SCECFL        TTECFL
     C                   MOVEL     SCSTAT        TTSTAT
     C                   MOVEL     SCDCO         TTDCO
     C                   MOVEL     SCDFAC        TTDFAC
     C                   MOVEL     SCDDPT        TTDDPT
     C                   MOVEL     SCDACC        TTDACC
     C                   MOVEL     SCDSUB        TTDSUB
     C                   MOVEL     SCCCO         TTCCO
     C                   MOVEL     SCCFAC        TTCFAC
     C                   MOVEL     SCCDPT        TTCDPT
     C                   MOVEL     SCCACC        TTCACC
     C                   MOVEL     SCCSUB        TTCSUB
       //**** BEG ADD ******* 08/29/24 **** 115806 ********************
       If #ofSCR = 2;
          TTmins = scMINS;
          TTty01 = scTY01;
          TTty02 = scTY02;
          TTty03 = scTY03;
          TTty04 = scTY04;
          TTty05 = scTY05;
          TTty06 = scTY06;
       Endif;
       //**** End ADD ******* 08/29/24 **** 115806 ********************
      ***** BEG ADD ******* 03/24/10 **********************************
     C                   MOVEL(P)  SC1@TY        TT1@TY
     C                   MOVEL(P)  SC1DCT        TT1DCT
     C                   MOVEL(P)  SC1OBJ        TT1OBJ
     C                   MOVEL(P)  SC1GLC        TT1GLC
      ***** END ADD ******* 03/24/10 **********************************
     C*** Begin Add *** 03/16/21 **************************************
     C                   MOVEL(P)  SCEXIC        TTEXIC
     C*** End   Add *** 03/16/21 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C*** BEGIN ADD *** 11/14/97 **************************************
     C*
     C     HFLD          IFEQ      'SCDCO'
     C     HFLD          OREQ      'SCDFAC'
     C     HFLD          OREQ      'SCDDPT'
     C     HFLD          OREQ      'SCDACC'
     C     HFLD          OREQ      'SCDSUB'
     C     HFLD          OREQ      'SCCCO'
     C     HFLD          OREQ      'SCCFAC'
     C     HFLD          OREQ      'SCCDPT'
     C     HFLD          OREQ      'SCCACC'
     C     HFLD          OREQ      'SCCSUB'
     C*
     C*** END   ADD *** 11/14/97 **************************************
     C*
     C                   MOVE      *ZEROS        @@CO#
     C                   MOVE      *ZEROS        @@FAC#
     C                   MOVE      *ZEROS        @@DPT#
     C                   MOVE      *ZEROS        @@ACC#
     C                   MOVE      *ZEROS        @@SUB#
     C                   MOVE      *BLANKS       @@TYPE            1
     C                   MOVE      *BLANKS       @1TYPE            1
     C                   MOVE      *BLANKS       @@RTC             1
     C*
     C                   CALL      'BKD010R'
     C                   PARM                    @@TYPE
     C                   PARM                    @@CO#
     C                   PARM                    @@FAC#
     C                   PARM                    @@DPT#
     C                   PARM                    @@ACC#
     C                   PARM                    @@SUB#
     C                   PARM                    @1TYPE
     C                   PARM                    @@RTC
     C*
     C     HFLD          IFEQ      'SCDCO'
     C     HFLD          OREQ      'SCDFAC'
     C     HFLD          OREQ      'SCDDPT'
     C     HFLD          OREQ      'SCDACC'
     C     HFLD          OREQ      'SCDSUB'
     C*
     C     @@CO#         IFNE      *ZEROS
     C*
     C                   Z-ADD     @@CO#         SCDCO
     C                   Z-ADD     @@FAC#        SCDFAC
     C                   Z-ADD     @@DPT#        SCDDPT
     C                   Z-ADD     @@ACC#        SCDACC
     C                   Z-ADD     @@SUB#        SCDSUB
     C*
     C                   MOVE      *ON           *IN81
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN80
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCCCO'
     C     HFLD          OREQ      'SCCFAC'
     C     HFLD          OREQ      'SCCDPT'
     C     HFLD          OREQ      'SCCACC'
     C     HFLD          OREQ      'SCCSUB'
     C*
     C     @@CO#         IFNE      *ZEROS
     C*
     C                   MOVE      @@CO#         SCCCO
     C                   MOVE      @@FAC#        SCCFAC
     C                   MOVE      @@DPT#        SCCDPT
     C                   MOVE      @@ACC#        SCCACC
     C                   MOVE      @@SUB#        SCCSUB
     C*
     C                   ENDIF
     C*
     C                   MOVE      *ON           *IN81
     C*
     C                   ENDIF
     C*
     C                   MOVE      *ON           *IN90
     C*
      *
     C*** BEGIN ADD *** 11/14/97 **************************************
     C                   ELSE
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCDESC'
     C                   MOVE      *ON           *IN31
     C                   MOVE      *ON           *IN90
     C*
     C     HFLD          WHENEQ    'SCTYPE'
     C                   MOVE      *ON           *IN32
     C                   MOVE      *ON           *IN90
     C*
     C     HFLD          WHENEQ    'SCCTYP'
      **** BEGIN CHG **** 06/02/98 ************************************
     C******************** MOVE *ON       *IN33
     C*
     C*
     C                   CALL      'W2162R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    ##RRN             9 0
     C*
     C     ##RRN         IFNE      0
     C     ##RRN         CHAIN     PCT02                              60
     C                   ENDIF                                                  ##RRN IFNE 0
     C*
     C                   MOVE      *ON           *IN51
     C     *INKL         IFEQ      *OFF
     C     *IN60         ANDEQ     *OFF
     C     ##RRN         ANDNE     0
      *
     C                   MOVE      CTCTYP        SCCTYP
     C                   MOVE      *ON           *IN53
     C                   MOVE      *OFF          *IN51
      *
     C                   ENDIF                                                  *INKL IFEQ *OFF
      *
      **** END   CHG **** 06/02/98 ************************************
     C                   MOVE      *ON           *IN90
     C*
     C     HFLD          WHENEQ    'SCEXDS'
     C                   MOVE      *ON           *IN34
     C                   MOVE      *ON           *IN90
     C*
     C     HFLD          WHENEQ    'SCECFL'
     C                   MOVE      *ON           *IN35
     C                   MOVE      *ON           *IN90
     C*
     C                   OTHER
     C*
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDSL
     C*
     C                   ENDIF
     C*** END   ADD *** 11/14/97 **************************************
     C*
     C                   ENDIF
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
      *
      *** BEG ADD ***** 03/24/10 **********************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *
      * DISPLAY JDEDWARDS DOWNLOAD FIELDS (G/L RECORD TYPE, G/L
      * DOCUMENT TYPE, G/L OBJECT TYPE, G/L OFFSET) FOR TASB
      *
     C     CLTID         IFEQ      '025'
     C                   MOVE      *ON           *IN43
     C                   ENDIF
      *** END ADD ***** 03/24/10 **********************************
      //**** BEG ADD ***** 08/29/24 ******************************
      // Set number of screens = 2 if Coverisk (129)
        If CLTID = '129';
           #ofSCR = 2;
        Endif;
      *** END ADD ***** 08/29/24 **********************************
      *
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C* CHECK THAT G/L IS INSTALLED, OR IGNORE G/L INFO
     C*
     C                   MOVEL     'SY'          ##SKY
     C*
     C     SYKY          CHAIN     SSY01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     SYFL01        IFNE      *BLANKS
     C*
     C                   MOVE      *ON           *IN16
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDIF                                                  SYFL01
     C*
     C* CHECK G/L FLAG TO SEE IF FUND WILL BE USED AS CO#
     C*
     C                   MOVEL     'GL'          ##GKY
     C*
     C     GLKY          CHAIN     SGL01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     GLFL03        IFNE      *BLANKS
     C*
     C                   SETON                                            17    90
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDIF                                                  GLFL03
      *** Begin Add *** 03/16/21 **************************************
       // Let's see if Invoice Cloud is Installed
       isInvCloud = isModEnabled('Invoice Cloud');
       // Let's get the Invoice Cloud Header
       scICHeader = 'Exclude from ' +
          %Trim(GetModuleProperty('Invoice Cloud':'Name'));
      *** End   Add *** 03/16/21 **************************************
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C*
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
     C*
     C                   ENDSR
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
     C*
     C                   MOVEL     Q1USER        TTUSER
     C                   MOVEL     Q1PGM         TTPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TTDT
     C*
     C                   MOVEL     DSTIME        TTTIME
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        TTUSER
     C                   MOVEL     Q1PGM         TTPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TTDT
     C*
     C                   MOVEL     DSTIME        TTTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WTTTP
     C*
     C                   ENDSR
     C*
     C*** BEGIN ADD *** 11/14/97 **************************************
     C*
     C*================================================================
     C* READAR - Ensures only one A/R trans. code exists
     C*================================================================
     C     READAR        BEGSR
     C*
     C     TTKY1         CHAIN     PTT01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C*  ENSURE STATUS IS ACTIVE AND RECORD CURRENTLY BEING DISPLAYED
     C*  ISN'T CHECKED
     C*
     C     TTSTAT        IFEQ      'A'
     C     TTKEY         ANDNE     SCKEY
     C*
      **** BEGIN CHG **** 10/16/98 *********************************************
     C******** 40TTTYPE    IFEQ 'B'
     C   49TTTYPE        IFEQ      'B'
      **** END   CHG **** 10/16/98 *********************************************
     C     TTCTYP        ANDEQ     'E'
     C*
     C                   MOVE      *ON           *IN41
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF                                                  TTTYPE IFEQ 'B'
     C*
     C   42TTTYPE        IFEQ      'F'
     C     TTCTYP        ANDEQ     'M'
     C*
     C                   MOVE      *ON           *IN41
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF                                                  TTTYPE IFEQ 'F'
     C*
     C   44TTTYPE        IFEQ      'F'
     C     TTCTYP        ANDEQ     'S'
     C*
     C                   MOVE      *ON           *IN41
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF                                                  TTTYPE IFEQ 'F'
     C*
     C   46TTTYPE        IFEQ      'F'
     C     TTCTYP        ANDEQ     'R'
     C*
     C                   MOVE      *ON           *IN41
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF                                                  TTTYPE IFEQ 'F'
     C*
     C*** BEGIN CHG ***  08/23/05  ************************************
     C***48****  TTTYPE    IFEQ 'F'
     C   48TTTYPE        IFEQ      'B'
     C*** END CHG *****  08/23/05  ************************************
     C     TTCTYP        ANDEQ     'C'
     C*
     C                   MOVE      *ON           *IN41
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF                                                  TTTYPE IFEQ 'F'
     C*
     C   50TTTYPE        IFEQ      'F'
     C     TTCTYP        ANDEQ     'T'
     C*
     C                   MOVE      *ON           *IN41
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF                                                  TTTYPE IFEQ 'F'
     C*
     C*** BEGIN ADD ***  12/23/97  ************************************
     C*
     C   52TTTYPE        IFEQ      'F'
     C     TTCTYP        ANDEQ     '1'
     C*
     C                   MOVE      *ON           *IN41
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF                                                  TTTYPE IFEQ 'F'
     C*
     C   54TTTYPE        IFEQ      'F'
     C     TTCTYP        ANDEQ     '2'
     C*
     C                   MOVE      *ON           *IN41
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF                                                  TTTYPE IFEQ 'F'
     C*
     C*** END ADD *****  12/23/97  ************************************
     C*** BEGIN ADD ***  01/02/01  ************************************
     C*
     C   55TTTYPE        IFEQ      'F'
     C     TTCTYP        ANDEQ     '3'
     C*
     C                   MOVE      *ON           *IN41
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF                                                  TTTYPE IFEQ 'F'
     C*
     C*** END ADD *****  01/02/01  ************************************
     C*** Beg Add *****  04/30/12  ************************************
     C   56TTTYPE        IFEQ      'F'
     C     TTCTYP        ANDEQ     '4'
     C*
     C                   MOVE      *ON           *IN41
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF                                                  TTTYPE IFEQ 'F'
     C*
     C   57TTTYPE        IFEQ      'F'
     C     TTCTYP        ANDEQ     '5'
     C*
     C                   MOVE      *ON           *IN41
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF                                                  TTTYPE IFEQ 'F'
     C*
     C   58TTTYPE        IFEQ      'F'
     C     TTCTYP        ANDEQ     '6'
     C*
     C                   MOVE      *ON           *IN41
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF                                                  TTTYPE IFEQ 'F'
     C*
     C   59TTTYPE        IFEQ      'F'
     C     TTCTYP        ANDEQ     '7'
     C*
     C                   MOVE      *ON           *IN41
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF                                                  TTTYPE IFEQ 'F'
     C*** End Add *****  04/30/12  ************************************
     C*
     C                   ENDIF                                                  TTSTAT IFEQ 'A'
     C*
     C     TTKY1         READE     PTT01                                  60
     C*
     C                   ENDDO                                                  *IN60 DOWEQ*OFF
     C*
      **** BEGIN CHG **** 10/16/98 *********************************************
     C******************** MOVE *OFF      *IN40
     C                   MOVE      *OFF          *IN49
      **** END   CHG **** 10/16/98 *********************************************
     C                   MOVE      *OFF          *IN42
     C                   MOVE      *OFF          *IN44
     C                   MOVE      *OFF          *IN46
     C                   MOVE      *OFF          *IN48
     C                   MOVE      *OFF          *IN50
     C*** BEGIN ADD ***  12/23/97  ************************************
     C                   MOVE      *OFF          *IN52
     C                   MOVE      *OFF          *IN54
     C*** END ADD *****  12/23/97  ************************************
     C*** BEGIN ADD ***  01/02/01  ************************************
     C                   MOVE      *OFF          *IN55
     C*** END ADD *****  01/02/01  ************************************
     C*** BEG ADD *****  12/09/03  ************************************
     C                   MOVE      *OFF          *IN56
     C                   MOVE      *OFF          *IN57
     C*** END ADD *****  12/09/03  ************************************
     C***  Begin Add  ***  12/11/08  **************************************
     C                   MOVE      *OFF          *IN58
     C                   MOVE      *OFF          *IN59
     C***  End   Add  ***  12/11/08  **************************************
     C*
     C                   ENDSR
     C*** END   ADD *** 11/14/97 **************************************
