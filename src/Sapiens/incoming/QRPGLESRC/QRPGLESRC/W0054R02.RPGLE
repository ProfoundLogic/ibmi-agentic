     H Copyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0054R02 - ENDORSEMENT FORM INFO MAINTENANCE
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE ENDORSEMENT FORM INFO    *
     H*                FILE                                           *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0054R - ENDORSEMENT FORM INFO MAINTENANCE                 *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 12/03/97  PROGRAMMER: C:57  PROJECT: CM027          *
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 02/19/24 C:333  Project: WC2942
     H*                 HI Endorsement WC520602 Schedule Data
     H*
     H* 11/13/23 C:340  Project: WC2929
     H*                 Assigned Stat Code for ME(WC180607*)
     H*
     H* 11/17/22  C:340 Project WC2886
     H*                 Add AZ Endorsement.
     H*
     H* 04/09/20  C:291 Project WC2608
     H*                 Add Owner/Officer Title Required flag.
     H*
     H* 08/20/18  C:180 Support PHW72461
     H*                 Add decimals to salary limitation range.
     H*                 DSPF changes only  - No changes to pgm.
     H*
     H* 12/01/17  C:291 Project PHW49878
     H*                 Fix edit for "Consider loss adjustment
     H*                 expenses as advancements" field.
     H*
     H* 09/01/17  C:291 Project 150WBM
     H*                 Add Suppress Policy Notices flag.
     H*
     H* 01/05/10  C:61  Project WC1865
     H*                 Add processing for two new fields:
     H*                   NXCPCT - State Coverage % Change
     H*                   NXLPCT - Longshore & Harbor Workers Act
     H*                            Coverage %
     H*
     H* 02/15/08  C:190 Project WC1425
     H*                 Change the title of this option from
     H*                 "Endorsement Forms Information" to
     H*                 "Endors/Policy Form Info".
     H*
     H* 01/08/07 C:184 PROJECT WC1525
     H*                Add prompt fields for "Loss adjustment considered
     H*                as advancements".
     H*
     H* 07/03/06 C:61  PROJECT WC1439
     H*                Add prompt fields for "Salary Limitation Range".
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H*****************************************************************
     H* INDICATORS USED:                                              *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD                                                     *
     H* N40 - UPDATE/INQUIRY                                          *
     H*  92 - GENERAL F KEY PRESSED                                   *
     H*  93 - SCREEN WAS UPDATE WHILE USER WAS IN IT                  *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  30 - (WCR0504) The state code was not found in list.         *
     H*  31 - (WC30001) Record already exists.                        *
     H*  32 - (WCR0500) Invalid date.                                 *
     H*  33 - (WCR0501) VALUE MUST BE Y OR N.                         *
     H*  34 - (WC30646) PEND RATE CHG DATE INVALID.                   *
     H*  35 - (WC30646) PEND RATE CHG FLG/DATE CROSS EDIT.            *
     H*  36 - (WCR0501) VALUE MUST BE Y OR N.                         *
     H*  37 - (WC30645) RATE CHG DATE INVALID.                        *
     H*  38 - (WC30645) RATE CHG FLG/DATE CROSS EDIT.                 *
     H*                                                               *
     H* SCR02:                                                        *
     H*  61 - (WCR0501) VALUE MUST BE Y OR N.                         *
     H*                                                               *
     H* SCR03:                                                        *
     H*                                                               *
     H* SCR04:                                                        *
     H*                                                               *
     H*****************************************************************
     FW0054F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMNXP     UF A E             DISK
     F                                     RENAME(PNX01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWMNXL     IF   E           K DISK
     F                                     INFDS(DBFDS2)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
     D*
     D*** BEG CHG *** 02/15/08 **********************************
     D OPTNM           C                   CONST('Endors/Policy Form -
     D                                     Info')
     D*******       'Endorsement Forms I- C         OPTNM
     D*******       'nformation'
     D*** END CHG *** 02/15/08 **********************************
     F*** BEG CHG *** 01/08/07 **********************************
     D***           1                     C         #OFSCR
     D #OFSCR          C                   CONST(2)
     F*** END CHG *** 01/08/07 **********************************
     D COMAPI          C                   CONST('WA001R')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      *
      *

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  KEY LIST FOR THE SYSTEM-WIDE STATE CODE TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY            10
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  ENDORSEMENT FORM STAT INFO FILE
     C*
     C     NXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    NXFND         XXFND
     C     *LIKE         DEFINE    NXCO#         XXCO#
     C     *LIKE         DEFINE    NXST          XXST
     C     *LIKE         DEFINE    NXEFDT        XXEFDT
     C     *LIKE         DEFINE    NXEFDT        XXPRCD
     C     *LIKE         DEFINE    NXEFDT        XXRCED
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
     C     NXSTAT        IFEQ      'I'
     C                   MOVE      'A'           NXSTAT
     C                   ELSE
     C                   MOVE      'I'           NXSTAT
     C                   ENDIF                                                  NXSTAT
     C*
     C                   MOVEL     Q1USER        NXUSER
     C                   MOVEL     Q1PGM         NXPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NXDT
     C*
     C                   MOVEL     DSTIME        NXTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR                                                  CHGSTS
      *
      *
     C*===============================================================
     C* DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
     C*===============================================================
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
      *
      *
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
      *
      *
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C     ERRCK1        BEGSR
     C*
     C*  VERIFY STATE AND EFFECTIVE DATE IF IN ADD MODE.
     C*
     C     *IN40         IFEQ      *ON
     C*
     C*  STATE MUST BE VALID.
     C*
     C                   MOVEL     SCST          XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        9074
     C*
     C*  EFFECTIVE DATE MUST BE VALID
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C  N90SRERR         IFEQ      '1'
     C                   SETON                                        9076
     C                   ENDIF                                                  SRERR=1
     C*
     C*  RECORD MUST NOT ALREADY EXISTS FOR ST/EFDT
     C*
     C                   MOVE      SCST          XXST
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C     NXKY          CHAIN     PNX01                              60
     C  N90
     CANN60              SETON                                        9075
     C*
     C                   ENDIF                                                  *IN40 EQ 1
     C*
     C* FLAG MUST BE Y/N
     C*
     C  N90SCPRCF        IFNE      'Y'
     C     SCPRCF        ANDNE     'N'
     C                   SETON                                        9077
     C                   ENDIF                                                  SCPRCF IFNE
     C*
     C* IF FLAG IS Y, DATE IS REQUIRED. IF FLAG IS N, DATE MUST BE ZERO
     C*
     C  N90SCPRCF        IFEQ      'Y'
     C     SCPRCD        ANDEQ     *ZERO
     C     SCPRCF        ORNE      'Y'
     C     SCPRCD        ANDNE     *ZERO
     C                   SETON                                        9079
     C                   ENDIF                                                  SCPRCF IFEQ
     C*
     C* IF DATE ENTERED, IT MUST BE VALID. IT MUST ALSO BE >= SCEFDT
     C*
     C  N90SCPRCD        IFNE      *ZERO
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   Z-ADD     SCPRCD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPRCD
     C*
     C     SRERR         IFEQ      *ON
     C     XXPRCD        ORLT      XXEFDT
     C                   SETON                                        9078
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SCPRCD IFNE
     C*
     C* FLAG MUST BE Y/N
     C*
     C  N90SCRCEF        IFNE      'Y'
     C     SCRCEF        ANDNE     'N'
     C                   SETON                                        9080
     C                   ENDIF                                                  SCRCEF IFNE
     C*
     C* IF FLAG IS Y, DATE IS REQUIRED. IF FLAG IS N, DATE MUST BE ZERO
     C*
     C  N90SCRCEF        IFEQ      'Y'
     C     SCRCED        ANDEQ     *ZERO
     C     SCRCEF        ORNE      'Y'
     C     SCRCED        ANDNE     *ZERO
     C                   SETON                                        9082
     C                   ENDIF                                                  SCRCEF IFEQ
     C*
     C* IF DATE ENTERED, IT MUST BE VALID. IT MUST ALSO BE >= SCEFDT
     C*
     C  N90SCRCED        IFNE      *ZERO
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   Z-ADD     SCRCED        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRCED
     C*
     C     SRERR         IFEQ      *ON
     C     XXRCED        ORLT      XXEFDT
     C                   SETON                                        9081
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SCRCED IFNE
      ******* beg add ****** 07/03/06 *********************************
      *
      * Salary Limitation Range must be valid if entered
      *
     C  N90SCLLMT        IFNE      *ZERO
     C     SCHLMT        ORNE      *ZERO
      *
      *   Lower Limit must be greater than zero
      *
     C  N90SCLLMT        IFLE      *ZERO
     C                   SETON                                        90  83
     C                   ENDIF
      *
      *   High Limit must be greater than zero
      *
     C  N90SCHLMT        IFLE      *ZERO
     C                   SETON                                        90  84
     C                   ENDIF
      *
      *   Range Entered must be in ascending order.
      *
     C  N90SCLLMT        IFGT      SCHLMT
     C                   SETON                                        90  85
     C                   ENDIF
      *
     C                   ENDIF
      *
      ******* end add ****** 07/03/06 *********************************
     C******* BEG ADD ****** 02/15/08 *********************************
     C*
     C* IF STATE ENTERED IS 'AR' THEN MAKE DEPARTMENT NAME AND
     C* PHONE NUMBER REQUIRED ENTRTY.
     C*
     C  N90SCST          IFEQ      'AR'
     C     SCDEPT        ANDEQ     *BLANKS
     C                   SETON                                        9065
     C                   ENDIF                                                  SCCTCT=*BLANKS
     C*
     C  N90SCST          IFEQ      'AR'
     C     SCDPAR        ANDEQ     *ZEROS
     C                   SETON                                        9066
     C                   ENDIF
     C*
     C  N90SCST          IFEQ      'AR'
     C     SCPHN#        ANDEQ     *ZEROS
     C                   SETON                                        9067
     C                   ENDIF
     C******* END ADD ****** 02/15/08 *********************************
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90NXDT          IFGT      SAVEDT
     C     NXDT          OREQ      SAVEDT
     C     NXTIME        ANDGT     SAVETM
     C                   SETON                                        9073
     C                   ENDIF
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
      ******* Beg Add *** 11/17/22 **********************************
                         If scSt = 'AZ';
                            visCxNtDy = *on;
                         Else;
                         visCxNtDy = *off;
                         Endif;
      ******* End Add *** 11/17/22 **********************************
      ******* Beg Add *** 11/13/23 *** WC2929 ***********************
                         If scSt = 'ME';
                            visNXStCME = *On;
                            CntNXStCME = 240;
                         Else;
                            visNXStCME = *Off;
                         Endif;
      ******* End Add *** 11/13/23 *** WC2929 ***********************
      ******* Beg Add *** 02/19/24 **********************************
                         If scSt = 'HI';
                            visHIinfo = *On;
                            topHIinfo = 290;
                         Else;
                            visHIinfo = *Off;
                         Endif;
      ******* End Add *** 02/19/24 **********************************
     C                   ENDSR                                                  ERRCK1
      *
      *
     C*===============================================================
     C* ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
     C*===============================================================
     C     ERRCK2        BEGSR
      ******* BEG ADD *** 01/08/07 **********************************
       //*** Beg Add ***** 02/19/24 *********************************
       If visCxNtDy = *On;
       //*** End Add ***** 02/19/24 *********************************
     C*
     C* FLAG MUST BE Y/N
     C*
     C  N90SCDEDF        IFNE      'Y'
     C     SCDEDF        ANDNE     'N'
     C                   SETON                                        9061
     C                   ENDIF                                                  SCDEDF IFNE
       //*** Beg Add ***** 02/19/24 *********************************

       Endif;

       If visHIinfo = *On and *in90 = *Off;
          errEnd602A = *off;
          msEnd602A = *blanks;
          errEnd602B = *off;
          msEnd602B = *blanks;
                                                                // Column A edits
          If scMCareA <> *zeros or scWgLossA <> *zeros
           or scIBenDisA <> *zeros or scDBenA <> *zeros
           or scAdmLglA <> *zeros or scMiscA <> *zeros;
             If (scMCareA + scWgLossA + scIBenDisA + scDBenA
                 + scAdmLglA + scMiscA) <> 100;
                errEnd602A = *on;
                msEnd602A = 'Column A categories 1-6 must '+
                            'total 100%.';
                *in90 = *On;
             Endif;
          Endif;
                                                              // Column B edits
          If scMCareB <> *zeros or scWgLossB <> *zeros
           or scIBenDisB <> *zeros or scDBenB <> *zeros
           or scAdmLglB <> *zeros or scMiscB <> *zeros;
             If (scMCareB + scWgLossB + scIBenDisB + scDBenB
                 + scAdmLglB + scMiscB) > 100;
                errEnd602B = *on;
                msEnd602B = 'Column B categories 1-6 total ' +
                         'is more than 100%.';
                *in90 = *On;
             Endif;
          Endif;
       Endif;
       //*** End Add ***** 02/19/24 *********************************
      ******* END ADD *** 01/08/07 **********************************
     C                   ENDSR                                                  ERRCK2
      *
      *
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*===============================================================
     C     ERRCK3        BEGSR
     C                   ENDSR                                                  ERRCK3
      *
      *
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVE      NXST          XXST
     C                   Z-ADD     NXEFDT        XXEFDT
     C*
     C                   MOVEL     NXST          SCST
     C*
     C                   Z-ADD     NXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVEL     NXPRCF        SCPRCF
     C                   MOVEL     NXRCEF        SCRCEF
     C*
     C                   Z-ADD     NXPRCD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPRCD
     C*
     C                   Z-ADD     NXRCED        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRCED
     C*
      ***** BEG ADD ******* 01/05/10 **********************************
     C                   Z-ADD     NXCPCT        SCCPCT
     C                   Z-ADD     NXLPCT        SCLPCT
      ***** END ADD ******* 01/05/10 **********************************
     C                   MOVEL     NXSTAT        SCSTAT
      ******* beg add ****** 07/03/06 *********************************
     C                   Z-ADD     NXLLMT        SCLLMT
     C                   Z-ADD     NXHLMT        SCHLMT
      ******* end add ****** 07/03/06 *********************************
      ******* BEG ADD *** 01/08/07 **********************************
     C                   MOVEL     NXDEDF        SCDEDF
      ******* END ADD *** 01/08/07 **********************************
     C******* BEG ADD ****** 02/15/08 *********************************
     C                   MOVEL     NXDEPT        SCDEPT
     C                   MOVEL     NXDPAR        SCDPAR
     C                   MOVEL     NXDPPN        SCPHN#
     C******* END ADD ****** 02/15/08 *********************************
      ******* beg add *** 09/01/17 **********************************
     C                   Eval      scSPolNtc = NXSPLN
      ******* end add *** 09/01/17 **********************************
      ******* beg add *** 04/09/20 **********************************
     C                   Eval      scOffTitle = NXOFFT
      ******* end add *** 04/09/20 **********************************
      ******* Beg Add *** 11/17/22 **********************************
                         If scSt = 'AZ';
                            visCxNtDy = *on;
                         Else;
                         visCxNtDy = *off;
                         Endif;
                         scInsrCxDy = NXinsrCxDy;
                         scInsdCxDy = NXinsdCxDy;
      ******* End Add *** 11/17/22 **********************************
      ******* Beg Add *** 11/13/23 *** WC2929 ***********************
                         If scSt = 'ME';
                            visNXStCME = *on;
                            CntNXStCME = 240;
                         Else;
                            visNXStCME = *off;
                         Endif;
                         scNXStatCd = NXStatCdMe;
      ******* End Add *** 11/13/23 *** WC2929 ***********************
      ******* Beg Add *** 02/19/24 **********************************
                         If scSt = 'HI';
                            visHIinfo = *On;
                            topHIinfo = 240;
                         Else;
                            visHIinfo = *Off;
                         Endif;
                         scMCareA = NXMCareA;
                         scMCareB = NXMCareB;
                         scWgLossA = NXWgLossA;
                         scWgLossB = NXWgLossB;
                         scIBenDisA = NXIBenDisA;
                         scIBenDisB = NXIBenDisB;
                         scDBenA = NXDBenA;
                         scDBenB = NXDBenB;
                         scAdmLglA = NXAdmLglA;
                         scAdmLglB = NXAdmLglB;
                         scMiscA = NXMiscA;
                         scMiscB = NXMiscB;
                         If *in09 = *On;
                            indFocus = *Off;
                         Else;
                            indFocus = *On;
                         Endif;
      ******* End Add *** 02/19/24 **********************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANK        SCST
     C                   MOVEL     *ZERO         SCEFDT
     C                   MOVEL     *BLANK        SCPRCF
     C                   MOVEL     *BLANK        SCRCEF
     C                   MOVE      *ZERO         SCPRCD
     C                   MOVE      *ZERO         SCRCED
      ***** BEG ADD ******* 01/05/10 **********************************
     C                   Z-ADD     *ZERO         SCCPCT
     C                   Z-ADD     *ZERO         SCLPCT
      ***** END ADD ******* 01/05/10 **********************************
     C                   MOVEL     'A'           SCSTAT
      ******* beg add ****** 07/03/06 *********************************
     C                   Z-ADD     *ZERO         SCLLMT
     C                   Z-ADD     *ZERO         SCHLMT
      ******* end add ****** 07/03/06 *********************************
      ******* BEG ADD *** 01/08/07 **********************************
      ******* Beg Chg *** 12/01/17 **********************************
     C***                MOVEL     *BLANKS       SCDEDF
     C                   MOVE      'N'           SCDEDF
      ******* End Chg *** 12/01/17 **********************************
      ******* END ADD *** 01/08/07 **********************************
     C******* BEG ADD *** 02/15/08 **********************************
     C                   MOVEL     *BLANK        SCDEPT
     C                   Z-ADD     *ZERO         SCDPAR
     C                   Z-ADD     *ZERO         SCPHN#
     C******* END ADD *** 02/15/08 **********************************
      ******* beg add *** 09/01/17 **********************************
     C                   Eval      scSPolNtc = *Blank
      ******* end add *** 09/01/17 **********************************
      ******* beg add *** 04/09/20 **********************************
     C                   Eval      scOffTitle = *Blank
      ******* end add *** 04/09/20 **********************************
      ******* Beg Add *** 11/17/22 **********************************
                         scInsrCxDy = *Zeros;
                         scInsdCxDy = *Zeros;
      ******* End Add *** 11/17/22 **********************************
      ******* Beg Add *** 11/13/23 *** WC2929 ***********************
                         scNXStatCd = *Zeros;
      ******* End Add *** 11/13/23 *** WC2929 ***********************
      ******* Beg Add *** 02/19/24 **********************************
                         scMCareA = *Zeros;
                         scMCareB = *Zeros;
                         scWgLossA = *Zeros;
                         scWgLossB = *Zeros;
                         scIBenDisA = *Zeros;
                         scIBenDisB = *Zeros;
                         scDBenA = *Zeros;
                         scDBenB = *Zeros;
                         scAdmLglA = *Zeros;
                         scAdmLglB = *Zeros;
                         scMiscA = *Zeros;
                         scMiscB = *Zeros;
                         If *in09 = *On;
                            indFocus = *Off;
                         Else;
                            indFocus = *On;
                         Endif;
      ******* End Add *** 02/19/24 **********************************
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  MVDBSC
      *
      *
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     @@CO#         NXCO#
     C                   Z-ADD     @@FND         NXFND
     C                   MOVE      SCST          NXST
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NXEFDT
     C*
     C                   MOVEL     SCPRCF        NXPRCF
     C                   MOVEL     SCRCEF        NXRCEF
     C*
     C                   Z-ADD     SCPRCD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NXPRCD
     C*
     C                   Z-ADD     SCRCED        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NXRCED
     C*
      ***** BEG ADD ******* 01/05/10 **********************************
     C                   Z-ADD     SCCPCT        NXCPCT
     C                   Z-ADD     SCLPCT        NXLPCT
      ***** END ADD ******* 01/05/10 **********************************
     C                   MOVEL     SCSTAT        NXSTAT
      ******* beg add ****** 07/03/06 *********************************
     C                   Z-ADD     SCLLMT        NXLLMT
     C                   Z-ADD     SCHLMT        NXHLMT
      ******* end add ****** 07/03/06 *********************************
      ******* BEG ADD *** 01/08/07 **********************************
     C                   MOVEL     SCDEDF        NXDEDF
      ******* END ADD *** 01/08/07 **********************************
     C******* BEG ADD ****** 02/15/08 *********************************
     C                   MOVEL     SCDEPT        NXDEPT
     C                   MOVEL     SCDPAR        NXDPAR
     C                   MOVEL     SCPHN#        NXDPPN
     C******* END ADD ****** 02/15/08 *********************************
      ******* beg add *** 09/01/17 **********************************
     C                   Eval      NXSPLN = scSPolNtc                           Suppress Pol Notices
      ******* end add *** 09/01/17 **********************************
      ******* beg add *** 04/09/20 **********************************
     C                   Eval      NXOFFT = scOffTitle                          Owner/Officer Title
      ******* end add *** 04/09/20 **********************************
      ******* Beg Add *** 11/17/22 **********************************
                         NXinsrCxDy = scInsrCxDy;
                         NXinsdCxDy = scInsdCxDy;
      ******* End Add *** 11/17/22 **********************************
      ******* Beg Add *** 11/13/23 *** WC2929 ***********************
                         NXStatCdMe = scNXStatCd;
      ******* Beg Add *** 11/13/23 *** WC2929 ***********************
      //***** Beg Add *** 02/19/24 **********************************
        If scSt = 'HI';
           NXMCareA = scMCareA;
           NXMCareB = scMCareB;
           NXWgLossA = scWgLossA;
           NXWgLossB = scWgLossB;
           NXIBenDisA = scIBenDisA;
           NXIBenDisB = scIBenDisB;
           NXDBenA = scDBenA;
           NXDBenB = scDBenB;
           NXAdmLglA = scAdmLglA;
           NXAdmLglB = scAdmLglB;
           NXMiscA = scMiscA;
           NXMiscB = scMiscB;
        Endif;
      //***** End Add *** 02/19/24 **********************************
     C*
     C                   ENDSR                                                  MVSCDB
      *
      *
     C*===============================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*===============================================================
     C     OTHERR        BEGSR
     C                   ENDSR                                                   OTHERR
      *
      *
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR PROCESSING OTHER FUNCTION KEYS
     C*===============================================================
     C     OTHFKS        BEGSR
     C                   ENDSR                                                   OTHFKS
      *
      *
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C     PGMEND        BEGSR
     C                   ENDSR
      *
      *
     C*===============================================================
     C* pgmSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C     PGMSTR        BEGSR
     C*
     C                   MOVE      *OFF          *IN09
     C                   MOVE      *OFF          *IN40
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    '1'                                          INQUIRY
     C     @@MODE        OREQ      '2'
     C     @@MODE        OREQ      '3'
     C     @@MODE        OREQ      'D'
     C                   MOVE      *ON           *IN09
     C     @@MODE        WHENEQ    'A'                                          ADD
     C     @@MODE        OREQ      'P'
     C                   MOVE      *ON           *IN40
     C                   ENDSL
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C                   Z-ADD     @@CO#         XXCO#
     C                   Z-ADD     @@FND         XXFND
     C                   MOVE      @@FND         SCFND
     C*
     C     FNKY          CHAIN     PFN01                              99
     C  N99              MOVEL     FNNAME        SCFNNM
     C*
      ******* Beg Add *** 11/17/22 **********************************
                         If scSt = 'AZ';
                            visCxNtDy = *on;
                         Else;
                         visCxNtDy = *off;
                         Endif;
      ******* End Add *** 11/17/22 **********************************
      ******* Beg Add *** 11/13/23 *** WC2929 ***********************
                         If scSt = 'ME';
                            visNXStCME = *On;
                            CntNXStCME = 240;
                         Else;
                            visNXStCME = *Off;
                         Endif;
      ******* End Add *** 11/13/23 *** WC2929 ***********************
      ******* Beg Add *** 02/19/24 **********************************
                         If scSt = 'HI';
                            visHIinfo = *On;
                            topHIinfo = 240;
                         Else;
                            visHIinfo = *Off;
                         Endif;
      ******* End Add *** 02/19/24 **********************************
     C
     C                   ENDSR                                                  PGMSTR
      *
      *
     C*===============================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*===============================================================
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
      *
      *
     C*===============================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*===============================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B02  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        NXUSER
     C                   MOVEL     Q1PGM         NXPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NXDT
     C*
     C                   MOVEL     DSTIME        NXTIME
     C*
     C                   UPDATE    DBRCDF
E02  C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   ENDSR                                                  UPDATR
      *
      *
     C*===============================================================
     C* WRITER - SUBROUTINE FOR WRITEING DATA BASE RECORDS
     C*===============================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1PGM         NXPGM
     C                   MOVEL     Q1USER        NXUSER
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NXDT
     C*
     C                   MOVEL     DSTIME        NXTIME
     C*
     C                   MOVEL     Q1PGM         NXIPGM
     C                   MOVEL     Q1USER        NXIUSR
     C                   MOVEL     SRDTO         NXIDT
     C                   MOVEL     DSTIME        NXITIM
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      WMNXP
     C*
     C                   ENDSR                                                  WRITER
     C*===============================================================
