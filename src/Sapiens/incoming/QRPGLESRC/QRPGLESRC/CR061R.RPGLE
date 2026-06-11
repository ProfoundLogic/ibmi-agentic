     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT

*****D***** DO FOR SISCLM -
*****D**(** GET CMNAME FILE FOR CLAIMS USE CMNAME01
*****D** GETINSURED CR061R1 THEN UPTO 5 CLAIMS, AND AN INDICATOR TO SHOW
*****D***   IF THERE ARE OT BEING DISPLAYED.
*****D**************************************************************************
     H*  (C) Copyright Fiserv, Inc., 1991-2007
     H*****************************************************************
     H*  PROGRAM SUBROUTINE CR061R - BUILD CLAIMANT INFO PROGRAM
     H*****************************************************************
     H*  THIS SUBPROGRAM WILL TAKE THE INCOMING PARMS OF PROGRAM AND
     H*  DARTS KEY; RETRIEVE CLAIMANT INFO FOR SPEICLATY SYSTEM .  SPECIFICALLY
     H*  INSURED NAME, AND CLAIMANT NAMES  (PLURAL)
     H*  RETURN CLAIMANT NAME TO SRKCVT PGM.
     H*****************************************************************
     H*  PROGRAMMER:  MG
     H*  DATE:        10/28/2009
     H*  CLIENT:      002
     H*  PROJECT:
     H*****************************************************************
     H*  REVISIONS:                                                   *
     D**************************************************************************
     H*  12/10/07  C:33  Seq#: DR144/61418
     H*                  Change WTABL & WMEML to be User Controlled.
     H*                   Only open the file if WCMS is installed.
     H*
      * ** C:002                                                              **
      * ** DR214                                                              **
      * ** 10/24/06                                                           **
      *    Changes for idarts compatibility                                   **
      *** RETURN ADDITIONAL INFORMATION TO THE CALLER (2 79 CHR FLDS)     ******
     D*
     H*                                                               *
     H*   2/19/06  C:002  VDARTS                                      *
     H*                   CHANGE PROGRAM TO USE RETRN INSTEAD OF *INLR*
     H*                    ALSO, CHANGE SRNAME TO USE RETRN SRTERM='1'*
     H*                      ALSO, ONLY OPEN FILES ONCE )
     H*                                                               *
     H*                                                               *
     H*  11/08/04  C:062  Support: DR129/44505                        *
     H*                   Validate key                                *
     H*                                                               *
     H*  11/21/01  C:128  Support: OP109/29973                        *
     H*                   Inz all data structures that contain        *
     H*                   numeric data to prevent data decimal error. *
     H*                                                               *
     H*                                                               *
     H*  09/20/00  C:128  Support: MC091/24414                        *
     H*                   Added code to prevent opening files for     *
     H*                   applications that are no longer installed.  *
     H*                                                               *
     H*  03/20/00  C:15   Support: DR048/22271                        *
     H*                   Added code to prevent data-decimal error.   *
     H*                                                               *
     H*  10/22/99  C:53   PROJ: MC145
     H*                   QUALIFY WCMS FILE W/NON-PDS WCMS FLAG.        *
     H*
     H*  10/30/92  C:20   PROJ:                                       *
     H*                   1. CHANGES FILES TO BE USER CONTROLLED      *
     H*                   2. ADDED FS FILES                           *
     H*                   3. MOVE WC AND FS PROCESSING INTO SEPERATE  *
     H*                      SUBROUTINES.                             *
     H*                   4. CONDITIONED CALL OF SUBROUTINE ON FIRST  *
     H*                      LETTER OF ENTRY CALLING PROGRAM          *
     H*  05/11/93  C:12   PROJ:                                       *
     H*                   1.  INITIALIZE XZKEY,X1KEY WITH ZEROS NOT   *
     H*                       BLANKS, TO ALLEVIATE DATA DECIMAL ERRORS*
     H*****************************************************************
     H*
     D* DATA STRUCTURE FOR KEY FOR WORK COMP
     D                 DS                  INZ
     D  XZKEY                  1     79
     D*** BEGIN ADD *** 11/08/04 **************************************
     D  XZCKEY                 1     27
     D*** END   ADD *** 11/08/04 **************************************
     D  XZCO#                  1      2
     D  XZPRE                  3      6
     D  XZPOL                  7     15
     D  XZBRCH                16     17
     D  XZCLM                 18     27
     D*** BEGIN ADD *** 10/30/92 **************************************
     D*
     D*** END ADD *** 10/30/92 ****************************************
     D                 DS                  INZ
     D  X3KEY                  1     79
     D*** BEGIN ADD *** 11/08/04 **************************************
     D  X3CKEY                 1     27
     D*** END   ADD *** 11/08/04 **************************************
     D  X3CO#                  1      2
     D  X3PRE                  3      6
     D  X3POL                  7     15
     D  X3BRCH                16     17
     D  X3CLM                 18     27
     D*** BEGIN ADD *** 02/10/94 **************************************
     D*
     D* DATA STRUCTURE FOR KEY FOR FEE SCHEDULE
     D*** END ADD *** 02/10/94 ****************************************
     D***  ADD 10/24/06 ****************************************************
      *
     D DATEDS          DS
     D  CYMD                   1      8  0
     D  CC1                    1      2  0
     D  CY1                    1      4
     D  YY1                    3      4  0
     D  YMD                    3      8  0
     D  MMDD                   5      8  0
     D  MM                     5      6
     D  DD                     7      8
     D  CC2                    9     10  0
     D  CY2                    9     12
     D  YY2                   11     12  0
     D  MDCY                   5     12  0
     D*
    *D**** TEMP HERE ***************************
     D STUFF           C                   CONST('TESTING THE ABILTY O-
     D                                     F IDARTS TO DEAL WIT-
     D                                     H TEXT ENTRIES 2 AND-
     D                                     THREE. TEST TEST TES')
     D**
     D CINS            C                   CONST('Insured: ')
     D CCLMNT          C                   CONST('Claimant:')
     D*
     D*
    *D**** END TEMP HERE ***************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     ****  END ADD 10/24/06 *******************************************
      *****************************************************************
      * CALCULATION SPECIFICATIONS  * * *
      *****************************************************************
      *

     C*
     C*
     C*
     C* ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPGM            10            PROGRAM NUMBER   AFT
     C                   PARM                    Q1KEY            79            DARTS' KEY
     C                   PARM                    Q1NAME           79            NAME
     C                   PARM                    Q1DTA1           79            DTA 1
     C                   PARM                    Q1DTA2           79            DTA 2
     C     Q1PRMS        IFLT      4                                                ONLY FOR IDARTS
     C                   GOTO      ENDIT
     C                   ENDIF
     C*
     C* SET-UP AND INITIALIZE
     C*
     C                   MOVE      *BLANKS       XXPGM             1
     C                   MOVE      *BLANKS       Q1NAME
     C                   MOVE      *ZEROS        XZKEY
     C                   MOVE      *ZEROS        X3KEY
      *
     C                   MOVEL     QQPGM         XXPGM                          AFTER
     C*
     C     Q1KEY         IFNE      *BLANKS
     C                   MOVEL     Q1KEY         XZKEY
     C                   MOVE      *BLANKS       XINS              1
     C                   MOVE      *BLANKS       XCLM1             1
     C                   MOVE      *BLANKS       XCLM2             1
     C                   MOVE      *BLANKS       XCLM3             1
     C                   MOVE      *BLANKS       XCLM4             1
     ***
     **
     **
     ****  TRY TO GET INSURED NAME :
     C                   CALL      'CR061R1 '                           99        BYPAS IFNTHERE
     C                   PARM                    XZCO#
     C                   PARM                    XZPRE
     C                   PARM                    XZPOL
     C                   PARM      *BLANKS       ZZNAME           79
     ***
     C     *IN99         IFEQ      *OFF
     *** USE THE NANE RETRIEVED AS THE "INSURED NAME"
     C**  INSURED NAME IS UPTO 50 LONG + "INSURED:"
     C     ZZNAME        IFNE      *BLANKS
     C                   MOVE      '1'           XINS
     ***
     C     CINS          CAT(P)    ZZNAME:0      Q1NAME
     C                   ENDIF                                                   Name not blanks
     C                   ENDIF                                                   99 is off
     ***
     **
     **
     **
     ****  TRY TO GET claimant names: (upto 4)
     C                   CALL      'CR061R2 '                           99        BYPAS IFNTHERE
     C                   PARM                    XZBRCH
     C                   PARM                    XZCLM
     C                   PARM      *BLANKS       ZZMULT            1              more than one clmn
     C                   PARM      *BLANKS       ZZMORE            1              more thn 4 clmnts
     C                   PARM      *BLANKS       ZZNAM1           79              data1
     C                   PARM      *BLANKS       ZZNAM2           79              data2
     ***
     C     *IN99         IFEQ      *OFF
     *** USE THE NANE RETRIEVED AS THE "INSURED NAME"
     C**  INSURED NAME IS UPTO 50 LONG + "INSURED:"
     C     ZZNAM1        IFNE      *BLANKS
     C                   MOVE      '1'           XCLM1
     ***
     C     XINS          IFNE      '1'                                             No Insured
     C     CCLMNT        CAT(P)    ZZNAM1:0      Q1NAME
     C     ZZNAM2        IFNE      *BLANKS
     C     CCLMNT        CAT(P)    ZZNAM2:0      Q1DTA1
     C                   ENDIF                                                   ZZNAM2 not blanks
     C                   ELSE
     C     CCLMNT        CAT(P)    ZZNAM1:0      Q1DTA1
     C     ZZNAM2        IFNE      *BLANKS
     C     CCLMNT        CAT(P)    ZZNAM2:0      Q1DTA2
     C                   ENDIF                                                   ZZNAM2 not blanks
     **
     **
     C                   ENDIF                                                   INS Not retrnd
     C                   ENDIF                                                   ZZNAM1 not blanks
     C                   ENDIF                                                   99 is off
     ***
     **
     **
     **
     **
     C*
     C                   ENDIF
     C*
     C*
     C*
     C     ENDIT         TAG
     C                   MOVE      '1'           *INLR
      *
      * ===============================================================
      *   CONVERT 7,0 DATE TO 8,0 ;   0961213 --> 19961213
      *   THEN, CYMD TO MDCY      ;  19961213 --> 12/13/1996
      * ===============================================================
     C     SRCDAT        BEGSR
      *
     C     YMD           IFNE      *ZERO
     C     CC1           IFEQ      0
     C                   MOVE      '19'          CC1
     C                   ELSE
     C                   MOVE      '20'          CC1
     C                   END
      *
     C                   MOVE(P)   CY1           CY2
     C     MM            CAT(P)    '/':0         XXDT10           10
     C     XXDT10        CAT       DD:0          XXDT10
     C     XXDT10        CAT       '/':0         XXDT10
     C     XXDT10        CAT       CY1:0         XXDT10
     C                   END
     C                   ENDSR
     C***   END ADD *** 10/24/06 **************************************
