     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller) bnddir('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0002R02 - OFFICE INFORMATION
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE OFFICE MASTER FILE.      *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0002R - WORK WITH OFFICES                                 *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 03/03/97  PROGRAMMER: C:33  PROJECT: 60AND          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 01/16/24 C:340 Project: 153WAD                                *
     H*                Add Office Email Address.                      *
     H*                                                               *
     H* 03/16/21 C:062 Project: WC2663                                *
     H*                Add 3rd Party Verbiage.                        *
     H*                                                               *
     H* 08/07/19 C:136 Project: SY406                                 *
     H*                US889 - Remove S/R Notification Email Address  *
     H*                                                               *
     H*  03/29/19 C:136 Project: 150WDW                               *
     H*                 Remove 150WBV permanently.                    *
     H*                                                               *
     H* 02/01/19  C:65  Project: SY407                                *
     H*                 Add email field for s/r notifications.        *
     H*                                                               *
     H*  01/31/18 C:313 Project: 150WBV                               *
     H*                 -Add 21 new fields to the screen for          *
     H*                  Remit To Info                                *
     H*                                                               *
     H*  06/28/11 C: 53 Project: SY255
     H*                 -Correct CMS flag
     H*
     H* 11/23/10  C:190 Project: WC1740                               *
     H*                 Added an entry for "EFT verbiage".            *
     H*                                                               *
     H* 08/09/10  C:198 Project: WC1879 Priss 47                      *
     H*                 Fixed the edit of office# to be all numeric   *
     H*                 only in case of Add operation.                *
     H*                                                               *
     H* 05/18/10  C:198 Project: WC1879                               *
     H*                 Added edit to make office# numeric and right  *
     H*                 justified if CMS is installed.                *
     H*                                                               *
     H* 03/17/09  C:33  Project 139WJE   25WRE Priss#22               *
     H*                 Allow for SCR02 to not require mandatory info.*
     H*                                                               *
     H* 05/05/09  C:190 Project WC1699(Priss#2)                       *
     H*                 Changed processing of 139WJE so that it is    *
     H*                 not client flagged.                           *
     H*                                                               *
     H* 07/24/08  C:190 Project 139WJE(Priss#13)                      *
     H*                 Add Claims office phone #.  Print Claims      *
     H*                  office phone # on the DWC forms if not zero. *
     H*                                                               *
     H*  05/30/08 C:190 PROJECT: 139WJE                               *
     H*           Allow entry of claim office information through     *
     H*           the Work with Offices application and print the     *
     H*           Claim office information on the FL DWC forms        *
     H*           when the information is available. If the Claim     *
     H*           office information is not available, continue to    *
     H*           print the company level information.                *
     H*                                                               *
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H*  10/14/98 C:65 PROJECT: SUPPORT                               *
     H*                REMOVE EDIT CODE ON ZIP CODE.                  *
     H*                                                               *
     H*  07/28/98 C:65 PROJECT: CM039                                 *
     H*                ADD ACCTING PHONE NUMBERS AND DIRECT INQUIRY   *
     H*                TO FIELDS.                                     *
     H*                                                               *
     H*  06/01/98 C:65 PROJECT: CM039                                 *
     H*                REMOVE STAFF AND CHECK UP INFO AND ADD         *
     H*                ACCOUNTING INFORMATION.                        *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD MODE                                                *
     H* N40 - UPDATE MODE                                             *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*  91 - GENERAL POSITION CURSOR                                 *
     H*  92 - GENERAL F KEY PRESSED                                   *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  27-36 Staff Codes                                            *
     H*  ERROR:                                                       *
     H*  20 - (WCR0503) Office# must be entered {ADD mode only}       *
     H*  21 - (WC30005) Office# previously used                       *
     H*  22 - (WCR0503) Office Name cannot be blank                   *
     H*  23 - (WCR0503) Address 1 cannot be blank                     *
     H*  24 - (WCR0503) Mailing City cannot be blank                  *
     H*  25 - (WCR0504) Mailing State invalid                         *
     H*  26 - (WCR0503) Mailing Zip 1 cannot be zero                  *
     H*  27 - (WCR0513) Invalid value                                 *
     H*  28 - CMS module is installed or not                          *
     H*  38 - (WCR0505) Staff Codes not found                         *
     H*  70 - (WCR0827) Remark Code not valid                         *
     H*  71 - (SCM0614) Remark Code Inactive                          *
     H*  POSITION CURSOR:                                             *
     H*  37 - Remark Code (SCCKRM)                                    *
     H*  39                                                           *
     H*                                                               *
     H*  SCR02:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*  SCR03:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*****************************************************************
     FW0002F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMOFP     UF A E             DISK
     F                                     RENAME(POF01:DBRCDF)
     FWMOFL     IF   E           K DISK
     F*** BEGIN DEL ***  06/01/98  ************************************
     F***WMUPL1  IF  E           K        DISK
     F***SMUPP   IF  E                    DISK
     F***            PUP01                             KRENAMEPUP02
     F*** END DEL *****  06/01/98  ************************************
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
     F*** Beg Add *** 05/18/10 **********************************
     FSSYLF     IF   E           K DISK
     F*** BEGIN DEL *** 06/28/11 **************************************
     F********    SSY01                             KIGNORE
     F*** END   DEL *** 06/28/11 **************************************
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     F*** End Add *** 05/18/10 **********************************
     F*** BEGIN DEL ***  06/01/98  ************************************
     F***STCTL1  IF  E           K        DISK
      /COPY SCOPYMT01D
     D*** Begin Add *** 03/16/21 *******************************************
      /copy sprModule
NP01  /copy wprEmplr
NP01  /copy sprGenErr
     D*** End   Add *** 03/16/21 *******************************************
     D*
     D*****               F10     1  11 10
     D*****               SCK        10  3
     D*****               OCK        10  3
     D*** END DEL *****  06/01/98  ************************************
     D*
     D***PY SCOPYMT01I
     D*
     D OPTNM           C                   CONST('Office Information')
     D*** BEG DLT *** 05/30/08 **********************************
     D**********    1                     C         #OFSCR
     D*** BEG ADD *** 05/05/09 **********************************
     D #OFSCR          C                   CONST(2)
     D*** END ADD *** 05/05/09 **********************************
     D*** END DLT *** 05/30/08 **********************************
     D COMAPI          C                   CONST('WA001R')
     D*** Beg Add *** 05/18/10 **********************************
     D VALNUM          C                   CONST('0123456789')
     D*** End Add *** 05/18/10 **********************************
     D*
     D*** BEGIN DEL ***  06/01/98  ************************************
     D*****       DS
     D*****                                   1  30 SCK
     D*****                                   1   3 SCCK1
     D*****                                   4   6 SCCK2
     D*****                                   7   9 SCCK3
     D*****                                  10  12 SCCK4
     D*****                                  13  15 SCCK5
     D*****                                  16  18 SCCK6
     D*****                                  19  21 SCCK7
     D*****                                  22  24 SCCK8
     D*****                                  25  27 SCCK9
     D*****                                  28  30 SCCK10
     D*****       DS
     D*****                                   1  30 OCK
     D*****                                   1   3 OFCK1
     D*****                                   4   6 OFCK2
     D*****                                   7   9 OFCK3
     D*****                                  10  12 OFCK4
     D*****                                  13  15 OFCK5
     D*****                                  16  18 OFCK6
     D*****                                  19  21 OFCK7
     D*****                                  22  24 OFCK8
     D*****                                  25  27 OFCK9
     D*****                                  28  30 OFCK10
     D*** END DEL *****  06/01/98  ************************************
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D*** BEGIN ADD *** 06/28/11 **************************************
     D                 DS
     D  SYFL12                 1     10
     D  XXCMS                  2      2

     I*** END   ADD *** 06/28/11 **************************************

     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  OFFICE MASTER FILE
     C*
     C     OFKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCOFF#
     C*
     C*  SYSTEM TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*** BEG ADD *** 05/30/08 ****************************************
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       XXCLT#            3
     C                   READ      POF01                                  60
     C*** END ADD *** 05/30/08 ****************************************
     C*
     C     UPRMPL        PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      3             ##VIEW            1 0
     C*
     C     *LIKE         DEFINE    STKEY         XXKEY
     C*** BEGIN DEL ***  06/01/98  ************************************
     C*****      *LIKE     DEFN SCCK1     ##STAF
     C*** END DEL *****  06/01/98  ************************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*===============================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*===============================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     OFSTAT        IFEQ      'I'
     C                   MOVE      'A'           OFSTAT
     C                   ELSE
     C                   MOVE      'I'           OFSTAT
     C                   ENDIF                                                  DPSTAT=I
     C*
     C                   MOVEL     Q1USER        OFUSER
     C                   MOVEL     Q1PGM         OFPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OFDT
     C*
     C                   MOVEL     DSTIME        OFTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  CHGSTS
     C*
     C*===============================================================
     C* DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
     C*===============================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
     C*
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C*  OFFICE # MUST BE > ZERO (IF IN ADD MODE)
     C*
     C   40
     CANN90SCOFF#        IFEQ      *BLANK
     C                   SETON                                        9020
     C                   ENDIF                                                  SCOFF#=*BLANK
     C*
     C*** Beg Add *** 05/18/10 ******************************************
     C*** Beg Add *** 08/09/10 ******************************************
     C     *IN40         IFEQ      *ON
     C     *IN90         ANDEQ     *OFF
     C*** End Add *** 08/09/10 ******************************************
     C                   SETOFF                                       28
     C*** if CMS module is installed then office number can not have characters
     C*** and it is going to be right justified.
     C*** BEG CHG *** 06/28/11 **************************************
     C*****                MOVE 'WC'      KKWC    2
     C*****      KKWC      CHAINSWC01                60
     C*****      WCFL46    IFEQ 'Y'
     C                   MOVEL     'SY'          KKSY              2
     C     KKSY          CHAIN     SSY01                              60
     C     XXCMS         IFEQ      'Y'
     C*** END CHG *** 06/28/11 **************************************
     C                   SETON                                        28
     C     VALNUM        CHECK     SCOFF#                                 26
     C     *IN26         IFEQ      '1'
     C                   SETON                                        9027
     C                   ENDIF                                                  *IN27 = '1'
     C                   ENDIF                                                  XXCMS = 'Y'
     C*** Beg Add *** 08/09/10 ******************************************
     C                   ENDIF                                                  40N90
     C*** End Add *** 08/09/10 ******************************************
     C*** End Add *** 05/18/10 ******************************************
     C*  CHECK TO BE SURE OFFICE # HAS NOT BEEN USED PREVIOUSLY
     C*
     C   40
B02  CANN90OFKY          CHAIN     POF01                              60
     C   40
     CANN90
     CANN60              SETON                                        9021
     C*
     C*  OFFICE NAME CANNOT BE BLANK
     C*
B02  C  N90SCNAME        IFEQ      *BLANK
     C                   SETON                                        9022
E02  C                   ENDIF                                                  SCNAME=*BLANK
     C*
     C*  ADDRESS 1 CANNOT BE BLANK
     C*
     C     SCADR1        IFEQ      *BLANKS
     C                   SETON                                        9023
     C                   ENDIF                                                  SCADR1=*BLANKS
     C*
     C*  CITY CANNOT BE BLANK
     C*
     C     SCCTY         IFEQ      *BLANKS
     C                   SETON                                        9024
     C                   ENDIF                                                  SCCTY=*BLANKS
     C*
     C*  STATE MUST BE VALID
     C*
     C                   MOVEL     SCST          XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        9025
     C*
     C*  ZIP 1 CANNOT BE ZERO
     C*
B03  C  N90SCZIP1        IFEQ      *ZERO
     C                   SETON                                        9025
E03  C                   ENDIF                                                  SCZIP1=*ZERO
     C*
     C*** BEGIN ADD ***  06/01/98  ************************************
     C*
     C*  ACCOUNTING NAME CANNOT BE BLANK
     C*
B02  C  N90SCACNM        IFEQ      *BLANK
     C                   SETON                                        9041
E02  C                   ENDIF                                                  SCACNM=*BLANK
     C*
     C*  ADDRESS 1 CANNOT BE BLANK
     C*
     C     SCAAD1        IFEQ      *BLANKS
     C                   SETON                                        9042
     C                   ENDIF                                                  SCAAD1=*BLANKS
     C*
     C*  CITY CANNOT BE BLANK
     C*
     C     SCACTY        IFEQ      *BLANKS
     C                   SETON                                        9043
     C                   ENDIF                                                  SCACTY IFEQ S
     C*
     C*  STATE MUST BE VALID
     C*
     C                   MOVEL     SCAST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        9044
     C*
     C*  ZIP 1 CANNOT BE ZERO
     C*
     C  N90SCAZP1        IFEQ      *ZERO
     C                   SETON                                        9045
     C                   ENDIF                                                  SCAZP1=*ZERO
     C*
     C*
     C*  ACCOUNTING NAME CANNOT BE BLANK
     C*
     C  N90SCCHKN        IFEQ      *BLANK
     C                   SETON                                        9046
     C                   ENDIF                                                  SCCHKN=*BLANK
     C*** END ADD *****  06/01/98  ************************************
     C*** Beg Add *****  01/16/24  *** 153WAD *************************
          erEmail = *Off;
          msEmail = ' ';
          If *In90 = *Off and scEmail <> ' ';
             erEmail = Check_Email(scEmail:msEMail);
             If erEmail = *on;
                *In90 = *On;
                msEmail = 'Please Enter Valid Email.';
             Endif;
          EndIf;

     C*** End Add *****  01/16/24  *** 153WAD *************************
     C*** BEGIN DEL ***  06/01/98  ************************************
     C*  VALIDATE STAFF CODES
     C*
     C*****                DO   10        X       30
     C*
     C*****      SCK,X     IFNE *BLANKS
     C*
     C*****      SCK,X     CHAINPUP01                60
     C*
     C*****      *IN60     IFEQ *ON
     C*****      X         ADD  26        X1      30
     C*****                SETON                     37  90
     C*****                MOVE *ON       *IN,X1
     C*****                LEAVE
     C*****                ENDIF                           *IN60=*ON
     C*
     C*****                ENDIF                           SCK,X<>*BLANKS
     C*
     C**N90                ENDDO                           X->10
     C*
     C*  VALIDATE REMARK CODE
     C*
     C**N90      SCCKRM    IFNE *BLANKS
     C*
     C*****      SCCKRM    CHAINPCT01                60
     C*
     C*****      *IN60     IFEQ *ON
     C*
     C*****                SETON                     70  90
     C*
     C*****                ELSE
     C*
     C*****      CTSTAT    IFEQ 'I'
     C*****                SETON                     71  90
     C*****                ENDIF                           CTSTAT=I
     C*
     C*****                ENDIF                           *IN60=*ON
     C*
     C*****                ENDIF                           SCCKRM<>*BLANKS
     C*** END DEL *****  06/01/98  ************************************
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C***  Begin Add  ***  03/17/10  **************************************
     C* Field validation will only occur if any field is entered.
     C  N90SCCNAM        IFNE      *BLANKS
     C     SCCAD1        ORNE      *BLANKS
     C     SCCAD2        ORNE      *BLANKS
     C     SCCCTY        ORNE      *BLANKS
     C     SCCST         ORNE      *BLANKS
     C     SCCZP1        ORNE      *ZEROS
     C     SCCZP2        ORNE      *ZEROS
     C     SCCARA        ORNE      *ZEROS
     C     SCCPH1        ORNE      *ZEROS
     C     SCCPH2        ORNE      *ZEROS
     C***  End   Add  ***  03/17/10  **************************************
     C*** BEG ADD *** 05/30/08 **********************************
     C*
     C*  CLAIM OFFICE NAME CANNOT BE BLANK
     C*
     C  N90SCCNAM        IFEQ      *BLANK
     C                   SETON                                        9030
     C                   ENDIF                                                  SCCNAM=*BLANK
     C*
     C*
     C*  CLAIM ADDRESS
     C*
     C     SCCCTY        IFNE      *BLANKS
     C     SCCAD1        ANDEQ     *BLANKS
     C                   SETON                                        9032
     C                   ENDIF                                                  SCCCTY<>BLANKS
     C*
     C     SCCST         IFNE      *BLANKS
     C     SCCAD1        ANDEQ     *BLANKS
     C                   SETON                                        9032
     C                   ENDIF                                                  SCCST<>BLANKS
     C*
     C     SCCZP1        IFNE      *ZEROS
     C     SCCAD1        ANDEQ     *BLANKS
     C                   SETON                                        9032
     C                   ENDIF                                                  SCCST<>BLANKS
     C*
     C*  CITY CANNOT BE BLANK IF ADDRESS IS ENTERED.
     C*
     C     SCCAD1        IFNE      *BLANKS
     C     SCCCTY        ANDEQ     *BLANKS
     C                   SETON                                        9033
     C                   ENDIF                                                  SCCCTY=*BLANKS
     C*
     C*  STATE MUST BE VALID
     C*
     C     SCCAD1        IFNE      *BLANKS
     C                   MOVEL     SCCST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        9034
     C                   ENDIF                                                  SCCAD1<>*BLANKS
     C*
     C*  ZIP 1 CANNOT BE ZERO IF ADDRESS IS ENTERED.
     C*
     C     SCCAD1        IFNE      *BLANKS
     C     SCCZP1        ANDEQ     *ZERO
     C                   SETON                                        9035
     C                   ENDIF                                                  SCCZP1= *ZERO
     C*
     C*** END ADD *** 05/30/08 **********************************
     C***  Begin Add  ***  03/17/10  **************************************
     C                   ENDIF
     C***  End   Add  ***  03/17/10  **************************************
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*===============================================================
     C*
     C     ERRCK3        BEGSR
     C*
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK3
     C*
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=SEARCH
     C*
     C     KJDO          BEGSR
     C*
     C*** BEGIN DEL ***  06/01/98  ************************************
     C*****                Z-ADD1         I1      30
     C*****      HFLD      LOKUPF10,I1                   60
     C*****
     C*****      *IN60     IFEQ *ON
     C*****
     C*****                SELEC
     C*****
     C*****      I1        WHGE 1
     C*****      I1        ANDLE10
     C*****
     C*****                MOVE *BLANKS   ##STAF
     C*****
     C*****                CALL 'S0005R'  UPRMPL
     C*****
     C*****      ##RRN     IFNE *ZEROS
     C*****
     C*****      ##RRN     CHAINPUP02                99
     C*****
     C*****                MOVELUPSTAF    SCK,I1
     C*****      I1        ADD  27        X1
     C*****                ELSE
     C*****      I1        ADD  26        X1
     C*****                ENDIF                           ##STAF<>*BLANKS
     C*****
     C*****                MOVE *ON       *IN39
     C*****                MOVE *ON       *IN,X1
     C*****
     C*****      I1        WHEQ 11                         1<=I1<=10
     C*
     C*****      HFLD      IFEQ 'SCCKRM'
     C*****                CALL 'SC580R'
     C*****                PARM           SCCKRM
     C*****                PARM           QQRTC   1
     C*
     C*****      SCCKRM    COMP *BLANKS                  37
     C*
     C*****                ENDSL                           I1
     C*
     C*****                ENDIF                           *IN60=*OFF
     C*** END DEL *****  06/01/98  ************************************
     C*
     C                   ENDSR                                                  KJDO
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     OFOFC#        SCOFF#
     C                   MOVEL     OFNAME        SCNAME
     C                   MOVEL     OFADR1        SCADR1
     C                   MOVEL     OFADR2        SCADR2
     C                   MOVEL     OFCTY         SCCTY
     C                   MOVEL     OFST          SCST
     C                   MOVEL     OFZIP         SCZIP1
     C                   MOVE      OFZIP         SCZIP2
     C                   Z-ADD     OFAREA        SCAREA
     C                   MOVEL     OFPHN#        SCPHN1
     C                   MOVE      OFPHN#        SCPHN2
     C*** BEGIN DEL ***  06/01/98  ************************************
     C*****                MOVEAOCK       SCK
     C*****                MOVELOFCKRM    SCCKRM
     C*** END DEL *****  06/01/98  ************************************
     C                   Z-ADD     OFFARA        SCFARA
     C                   MOVEL     OFFAX#        SCFAX1
     C                   MOVE      OFFAX#        SCFAX2
     C*** BEGIN ADD ***  06/01/98  ************************************
     C                   MOVEL     OFACNM        SCACNM
     C                   MOVEL     OFAAD1        SCAAD1
     C                   MOVEL     OFAAD2        SCAAD2
     C                   MOVEL     OFACTY        SCACTY
     C                   MOVEL     OFAST         SCAST
     C                   MOVEL     OFAZIP        SCAZP1
     C                   MOVE      OFAZIP        SCAZP2
     C                   MOVEL     OFCKNM        SCCHKN
     C*** END ADD *****  06/01/98  ************************************
     C*** BEGIN ADD ***  07/28/98  ************************************
     C                   MOVEL     OFLNNM        SCLNNM
     C                   MOVEL     OFPRNM        SCPRNM
     C                   MOVEL     OFMINM        SCMINM
     C                   Z-ADD     OFAARA        SCAARA
     C                   MOVEL     OFAPHN        SCAPH1
     C                   MOVE      OFAPHN        SCAPH2
     C                   Z-ADD     OFAFXA        SCAFXA
     C                   MOVEL     OFAFX#        SCAFP1
     C                   MOVE      OFAFX#        SCAFP2
     C*** END ADD *****  07/28/98  ************************************
     C*** BEG ADD *** 05/30/08 **********************************
     C*** BEG DLT *** 05/05/09 **********************************
     C********   XXCLT#    IFEQ '139'
     C********             Z-ADD2         #OFSCR  10
     C********             ELSE
     C********             Z-ADD1         #OFSCR
     C********             ENDIF                           XXCLT#='139'
     C*** END DLT *** 05/05/09 **********************************
     C*
     C                   MOVEL     OFCNAM        SCCNAM
     C                   MOVEL     OFCAD1        SCCAD1
     C                   MOVEL     OFCAD2        SCCAD2
     C                   MOVEL     OFCCTY        SCCCTY
     C                   MOVEL     OFCST         SCCST
     C                   MOVEL     OFCZP         SCCZP1
     C                   MOVE      OFCZP         SCCZP2
     C*** END ADD *** 05/30/08 **********************************
     C*** BEG ADD *** 07/24/08 **********************************
     C                   MOVE      OFCARA        SCCARA
     C                   MOVEL     OFCPH1        SCCPH1
     C                   MOVE      OFCPH2        SCCPH2
     C*** END ADD *** 07/24/08 **********************************
     C*** Beg Add *** 11/23/10 ******************************************
     C                   MOVE      OFEFTV        SCEFTV                         EFT
     C*** End Add *** 11/23/10 ******************************************
     C*** Begin Add *** 03/16/21 **************************************
     C                   MOVEL     OF3RDVRB      SC3RDVRB                       EFT
     C*** Begin Add *** 03/16/21 **************************************
     C*** Beg Add *** 01/16/24 *** 153WAD *****************************
     C                   MOVEL     OFEMAIL       scEMAIL                        Office Email
     C*** End Add *** 01/16/24 *** 153WAD *****************************
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL     SCOFF#        OFOFC#
     C                   MOVEL     SCNAME        OFNAME
     C                   MOVEL     SCADR1        OFADR1
     C                   MOVEL     SCADR2        OFADR2
     C                   MOVEL     SCCTY         OFCTY
     C                   MOVEL     SCST          OFST
     C                   MOVEL     SCZIP1        OFZIP
     C                   MOVE      SCZIP2        OFZIP
     C                   Z-ADD     SCAREA        OFAREA
     C                   MOVEL     SCPHN1        OFPHN#
     C                   MOVE      SCPHN2        OFPHN#
     C                   Z-ADD     SCFARA        OFFARA
     C                   MOVEL     SCFAX1        OFFAX#
     C                   MOVE      SCFAX2        OFFAX#
     C*** BEGIN DEL ***  06/01/98  ************************************
     C*****                MOVEASCK       OCK
     C*****                MOVELSCCKRM    OFCKRM
     C*** END DEL *****  06/01/98  ************************************
     C*** BEGIN ADD ***  06/01/98  ************************************
     C                   MOVEL     SCACNM        OFACNM
     C                   MOVEL     SCAAD1        OFAAD1
     C                   MOVEL     SCAAD2        OFAAD2
     C                   MOVEL     SCACTY        OFACTY
     C                   MOVEL     SCAST         OFAST
     C                   MOVEL     SCAZP1        OFAZIP
     C                   MOVE      SCAZP2        OFAZIP
     C                   MOVEL     SCCHKN        OFCKNM
     C*** END ADD *****  06/01/98  ************************************
     C*** BEGIN ADD ***  07/28/98  ************************************
     C                   MOVEL(P)  SCLNNM        OFLNNM
     C                   MOVEL(P)  SCPRNM        OFPRNM
     C                   MOVEL(P)  SCMINM        OFMINM
     C                   Z-ADD     SCAARA        OFAARA
     C                   MOVEL     SCAPH1        OFAPHN
     C                   MOVE      SCAPH2        OFAPHN
     C                   Z-ADD     SCAFXA        OFAFXA
     C                   MOVEL     SCAFP1        OFAFX#
     C                   MOVE      SCAFP2        OFAFX#
     C*** END ADD *****  07/28/98  ************************************
     C*** BEG ADD *** 05/30/08 **********************************
     C                   MOVEL     SCCNAM        OFCNAM
     C                   MOVEL     SCCAD1        OFCAD1
     C                   MOVEL     SCCAD2        OFCAD2
     C                   MOVEL     SCCCTY        OFCCTY
     C                   MOVEL     SCCST         OFCST
     C                   MOVEL     SCCZP1        OFCZP
     C                   MOVE      SCCZP2        OFCZP
     C*** END ADD *** 05/30/08 **********************************
     C*** BEG ADD *** 07/24/08 **********************************
     C                   MOVE      SCCARA        OFCARA
     C                   MOVEL     SCCPH1        OFCPH1
     C                   MOVE      SCCPH2        OFCPH2
     C*** END ADD *** 07/24/08 **********************************
     C*** Beg Add *** 11/23/10 ******************************************
     C                   MOVE      SCEFTV        OFEFTV                         EFT
     C*** End Add *** 11/23/10 ******************************************
     C*** Begin Add *** 03/16/21 **************************************
     C                   MOVEL     SC3RDVRB      OF3RDVRB                       EFT
     C*** Begin Add *** 03/16/21 **************************************
     C*** Beg Add *** 01/16/24 *** 153WAD *****************************
     C                   MOVEL     scEMAIL       OFEMAIL                        Office Email
     C*** End Add *** 01/16/24 *** 153WAD *****************************
     C*
     C                   ENDSR                                                  MVSCDB
     C*
     C*===============================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*===============================================================
     C*
     C     OTHERR        BEGSR
     C*
     C     *INKC         CASEQ     *ON           FINISH
     C                   ENDCS
     C*
     C                   ENDSR                                                   OTHERR
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*** Beg Add *** 11/23/10 ******************************************
     C*** Check EFT flag
     C*
     C                   MOVE      'WC'          KKWC              2
     C     KKWC          CHAIN     SWC01                              60
     C     WCFL24        IFEQ      'Y'
     C                   SETON                                        47
     C                   ENDIF                                                  WCFL46 = 'Y'
     C*** End Add *** 11/23/10 ******************************************
      *** Begin Add *** 03/16/21 **************************************
       // Let's see if Invoice Cloud is Installed
       isInvCloud = isModEnabled('Invoice Cloud');
      *** End   Add *** 03/16/21 **************************************
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*===============================================================
     C*
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C*
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*===============================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B02  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        OFUSER
     C                   MOVEL     Q1PGM         OFPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OFDT
     C                   MOVEL     DSTIME        OFTIME
     C*
     C                   UPDATE    DBRCDF
     C*
E02  C                   ENDIF                                                  *IN99=*OFF
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
     C                   CLEAR                   DBRCDF
     C*
     C                   Z-ADD     ##CO#         OFCO#
     C                   MOVEL     'A'           OFSTAT
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        OFIUSR
     C                   MOVEL     Q1USER        OFUSER
     C                   MOVEL     Q1PGM         OFIPGM
     C                   MOVEL     Q1PGM         OFPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OFIDT
     C                   MOVEL     SRDTO         OFDT
     C                   MOVEL     DSTIME        OFITM
     C                   MOVEL     DSTIME        OFTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WMOFP
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  WRITER
