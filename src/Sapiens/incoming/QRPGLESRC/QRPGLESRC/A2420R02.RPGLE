     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H/TITLE A2420R01 - Bank Information Maintenance Program
     H*
     H*  Description:
     H*
     H*    This program maintains the AP/GL Bank Information file.
     H*
     H*    F10 is used for all search pgm execution.  Cursor is placed
     H*    in field to search.  If a valid search program exists, a
     H*    search field list will display.  If no valid search program
     H*    exists, an error message will display.
     H*
     H*    Bank Account Master file is accessed to assign Bank Name/Address
     H*    fields.  The Bank Info record is updated with this information.
     H*    During Daily Batch Check Processing the Bank Info file Bank Name/
     H*    Address fields will be used for check assignments.
     H*
     H*  Called From:
     H*    A2420R
     H*
     H*  Calls To:
     H*    SRCENTER
     H*    SRCVTDAT
     H*    SY600R
     H*
     H*  Created:  12/08/97  C:30  Project:  WC500
     H*
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/07/03 C:61 SUPPORT WA099 39888                             *
     H*               Correct issues with Edits and Screen display.   *
     H*                                                               *
     H* 10/22/03 C:61 SUPPORT AP121 39313                             *
     H*               Only display F10=Search in ADD mode.            *
     H*                                                               *
     H* 10/11/00 C:15 PROJECT:                                        *
     H*               Added code to control the display of fields on  *
     H*               SCR02 in add mode.                              *
     H*                                                               *
     H* 02/02/98  47  PROJECT: 91WBN                                  *
     H*           ADDED A THIRD SCREEN TO ENTER FOUR 40A FREE FORM    *
     H*           FIELDS TO PRINT IN THE UPPER LEFT OF THE CHECK AND  *
     H*           FILECOPY.                                           *
     H*           RENAMED SCR02 TO SCR03.                             *
     H*           MADE THIS NEW SCREEN SCR02.                         *
     H*                                                               *
     H*****************************************************************
     H*  Indicators:
     H*
     H*   09 - Display Mode.
     H*   49 - No Search pgm for F10 field search error message.
     H*   60 - General File Access
     H*   71 - 76  Used to select error messages
     H*            71 highest order bit, values used in ascending
     H*            order.  Last value used = 24 decimal.
     H*
     H*   80 -     Voucher
     H*
     H*   81 - 86  Used to position cursor
     H*            81 highest order bit, values used in ascending
     H*            order.  Last value used =  7 decimal.
     H*
     H*   90 - Edit Error occurred.
     H*   99 - General File access.
     H*
     H* ***************************************************************
      *  Bank Info Screen
     FA2420F02  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
      *
      *  WCMS Bank Information
     FAPBIP     UF A E             DISK
     F                                     RENAME(PBI01:DBRCDF)
      *
      *  WCMS Bank Information
     FAPBIL1    IF   E           K DISK
     F                                     RENAME(PBI01:DBRCDF2)
      *
      *  AP/GL Company/Facility file
     FBKCFL1    IF   E           K DISK
      *
      *  AP/GL Bank file
     FAPBKL1    IF   E           K DISK
      *
      *  WCMS Bank Account Master
     FWSCKL1    IF   E           K DISK
      *
      *  WCMS Check Signature file
     FWMSGL2    IF   E           K DISK
      *
      *  User Parameters
     FSMUPL     IF   E           K DISK
      *
      *  System table file
     FSTABL     IF   E           K DISK
      *
      *** BEG ADD *** 02/03/98 **************************************
      *  WCMS GROUP MASTER
     FWMFNL     IF   E           K DISK
      *** END ADD *** 02/03/98 **************************************
      *
     D***PY SCOPYMT01I
      *
      /COPY SCOPYMT01D
     D OPTNM           C                   CONST('Bank Information Mai-
     D                                     ntenance')
     D COMAPI          C                   CONST('WA001R')
      *
      *  Bank Information FILE
      *

     C     KEY1          KLIST
     C                   KFLD                    XXFND
     C                   KFLD                    XXAPBK
     C                   KFLD                    XXEFDT
     C                   KFLD                    XXACCT
      *
     C     BIKY          KLIST
     C                   KFLD                    XXFND
     C                   KFLD                    XXAPBK
     C                   KFLD                    XXEFDT
      *
      *  Bank Account Master file
      *
     C     CKKEY         KLIST
     C                   KFLD                    XXACCT
      *
      *  AP/GL Company/Facility file
      *
     C     CFKEY         KLIST
     C                   KFLD                    XXFND
      *
      *  AP/GL Bank file
      *
     C     BKKEY         KLIST
     C                   KFLD                    XXAPBK
      *
      *  Signature file
      *
     C     SGKEY         KLIST
     C                   KFLD                    KKNAME
      *
      *  USER PARAMETER FILE
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
      *  SYSTEM TABLE FILE
      *
     C     STKY          KLIST
     C                   KFLD                    XXKEY
      *** BEG ADD *** 02/03/98 **************************************
      *
      *  WCMS GROUP MASTER FILE
      *
     C     FNKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
      *** END ADD *** 02/03/98 **************************************
      *
      *  Company Name
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
     C     *LIKE         DEFINE    BICO#         XXCO#
     C     *LIKE         DEFINE    BICO#         XXFAC#
     C     *LIKE         DEFINE    BIFND         XXFND
     C     *LIKE         DEFINE    BIACCT        XXACCT
     C     *LIKE         DEFINE    BIACCT        @@ACCT
     C     *LIKE         DEFINE    BIACCT        ##ACCT
     C***        *LIKE     DEFN BICKTP    XXCKTP
     C     *LIKE         DEFINE    BIEFDT        SVEFDT
     C     *LIKE         DEFINE    BIEFDT        XXEFDT
     C     *LIKE         DEFINE    BINAM1        ##NAM1
     C     *LIKE         DEFINE    BINAM2        ##NAM2
     C     *LIKE         DEFINE    BI10A1        ##10A1
     C     *LIKE         DEFINE    BI10A2        ##10A2
     C     *LIKE         DEFINE    BICMD1        ##CMD1
     C     *LIKE         DEFINE    BICMD2        ##CMD2
     C     *LIKE         DEFINE    BINAM1        KKNAME
     C     *LIKE         DEFINE    BILOGO        SVLOGO
     C     *LIKE         DEFINE    STKEY         XXKEY
     C     *LIKE         DEFINE    BIAPBK        XXAPBK
      *
      *  ---------------------------
      *  P r o g r a m     S t a r t
      *  ---------------------------
      *
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@FND             3 0
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   END
      *
      /COPY SCOPYMT01C
      *
      ****************************************************************
      *  DLTPRE                                                      *
      ****************************************************************
     C     DLTPRE        BEGSR
      *
     C                   ENDSR
      *
      ****************************************************************
      *  DSPLYR - Indicate Screen has been displayed.                *
      ****************************************************************
     C     DSPLYR        BEGSR
      *
     C                   MOVE      *ON           OUTFLG
      *
     C                   ENDSR
      *
      ****************************************************************
      *  ERRCK1 - Edit A2420F02 SCR01 fields                         *
      ****************************************************************
     C     ERRCK1        BEGSR
      ***** BEG ADD ******* 11/07/03 **********************************
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
      ***** END ADD ******* 11/07/03 **********************************
      *
      *  Voucher Flag
      *
     C***                  MOVE UPCO#     XXCO#
     C                   MOVEL     SCACCT        XXACCT
     C     CKKEY         CHAIN     PCK01                              60
     C     *IN60         IFEQ      '0'
     C     CKVCHR        ANDEQ     'Y'
     C                   MOVE      '1'           *IN80
     C                   ELSE
     C                   MOVE      '0'           *IN80
      *** BEG CHG *** 02/02/98 ************************************
     C***                  Z-ADD2         #OFSCR
     C                   Z-ADD     3             #OFSCR
      *** END CHG *** 02/02/98 ************************************
     C                   ENDIF                                                  VCHR
      *
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CKAI01        SCBKNM                         Bank Name
     C                   MOVEL     CKAI02        SCBKAD                         Bank Address
     C                   MOVEL     CKAI03        SCBKCS                         Bank City State Zip
     C                   MOVE      CKVCHR        SCVCHR                         Voucher flag Y/N
     C                   ENDIF
      *
     C                   WRITE     SCR01
      *
      *
      *  If DISPLAY mode, ignore Edit processing.
      *
      *
      *  If ADD mode
      *
     C     @@MODE        IFEQ      'A'
      *
      *  The AP/GL Company # cannot be zero    (--Add-- Mode only)
      *
     C     *IN90         IFEQ      *OFF
     C     SCFND         ANDEQ     *ZERO
     C                   SETON                                        9020
     C                   ENDIF
      *
      *  The AP/GL Company # must exist in AP/GL Company/Facility Master
      *                                        (--Add-- Mode only)
      *
     C     *IN90         IFEQ      *OFF
     C***                  MOVE UPCO#     XXCO#
     C                   Z-ADD     SCFND         XXFND
     C     CFKEY         CHAIN     PCF01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        9021
     C                   ENDIF
     C                   ENDIF
      *
      *  The AP/GL Bank # cannot be zero       (--Add-- Mode only)
      *
     C     *IN90         IFEQ      *OFF
     C     SCAPBK        ANDEQ     *ZERO
     C                   SETON                                        9024
     C                   ENDIF
      *
      *  The AP/GL Bank # must exist in AP/GL Bank file
      *                                        (--Add-- Mode only)
      *
     C     *IN90         IFEQ      *OFF
     C***                  MOVE UPCO#     XXCO#
     C                   Z-ADD     SCAPBK        XXAPBK
     C     BKKEY         CHAIN     PBK01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        9025
     C                   ENDIF
     C                   ENDIF
      *
      *  The Account Number cannot be BLANKS  (--Add-- Mode only)
      *
     C     *IN90         IFEQ      *OFF
     C     SCACCT        ANDEQ     *BLANKS
     C                   SETON                                        90
     C                   MOVEA     '010011'      *IN(71)                        ERR MSG
     C                   ENDIF
      *
      *  The Bank Account Number must exist in Account Master
      *                                        (--Add-- Mode only)
      *
     C     *IN90         IFEQ      *OFF
     C***                  MOVE UPCO#     XXCO#
     C                   MOVEL     SCACCT        XXACCT
     C     CKKEY         CHAIN     PCK01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        90
     C                   MOVEA     '010100'      *IN(71)                        ERR MSG
     C                   ENDIF
     C                   ENDIF
      *
      *  The Effective Date cannot be zero       (--Add-- Mode only)
      *
     C     *IN90         IFEQ      *OFF
     C     SCEFDT        ANDEQ     *ZERO
     C                   SETON                                        9087
     C                   ENDIF
      *
      *  Effective Date must be valid Date
      *
      ***** BEG CHG ******* 11/07/03 **********************************
     C*********  SCEFDT    IFNE *ZERO
     C  N90SCEFDT        IFNE      *ZERO
      ***** END CHG ******* 11/07/03 **********************************
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
      ***** BEG DEL ******* 11/07/03 **********************************
     C*********            WRITESCR01
      ***** END DEL ******* 11/07/03 **********************************
     C                   ENDIF
      *
     C     *IN90         IFEQ      *OFF
      *
     C     SRERR         IFEQ      *ON
     C                   SETON                                        9088
      *
     C                   ELSE
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SVEFDT
     C                   ENDIF
      *
     C                   ENDIF
      *
      *  The CO#/APBK/EFDT/ACCT can NOT already exist in Bank Info file
      *                                        (--Add-- Mode only)
      *
     C     *IN90         IFEQ      *OFF
     C***                  MOVE UPCO#     XXCO#
     C                   MOVEL     SCACCT        XXACCT
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C                   Z-ADD     SCFND         XXFND
     C***                  MOVE SCAPPL    XXAPPL
     C     KEY1          CHAIN     DBRCDF2                            60
      *
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9023
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     *IN90         IFEQ      *OFF
      *
      *  If screen Effective Date LE Today, then
      *  Call API WA023R to determine the "Current" Bank Acct.
      *  using Today as Search Date.
      *  If a "Current" Bank Acct. exists, then display Error msg.
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDAY            7 0
      *
     C     XXEFDT        IFLE      XXTDAY
      *
     C                   MOVE      SCFND         #FND                           AP/GL CO#
     C                   MOVE      SCAPBK        ##APBK                         AP/GL CO#
     C                   MOVE      DSDATE        ##SRDT                         TODAY
     C                   MOVE      'C'           ##CALL                         "CHECK" Account
     C****                 MOVE SCAPPL    ##APPL           Applic Code
     C****                 MOVE FNCO#     #CO#             Company
      *
      *
      *  Call API to access "Current" Bank Account based on ##CALL value
      *
     C                   CALL      'AA023R'
     C                   PARM                    #FND              3            AP/GL CO#
     C                   PARM                    ##APBK            3            AP/GL Bank #
     C                   PARM                    ##SRDT            8            MMDDCCYY
     C                   PARM                    ##ACCT                         Bank Acct #
     C                   PARM                    ##CALL            1            (V)oucher/(C)heck
     C                   PARM      *BLANK        ##HDCY            1            Hardcopy Y/N
     C                   PARM      *BLANK        ##CHKQ           10            Check/Voucher OUTQ
     C                   PARM      *BLANK        ##FTYP           10            Form Type
     C                   PARM      *BLANK        ##RTCD            1            Return Code
     C                   PARM      *ZERO         ##DT              7            APBIP Eff. DT CYYMMD
      *
     C     ##RTCD        IFEQ      *BLANK
      *  Bank Info found for "Check" acct, with effective date prior to today.
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '111111'      *IN(71)
     C                   ENDIF                                                  ##RTCD
     C                   ENDIF                                                  XXEFDT LE XXTDAY
      *
     C                   ENDIF
      *
     C                   ENDIF                                                  ADD mode
      *
      *  If CHANGE mode or ADD mode
      *
     C     @@MODE        IFEQ      'C'
     C     @@MODE        OREQ      'A'
      *
      *  Trust Y/N
      *
     C     *IN90         IFEQ      *OFF
     C     SCTRST        ANDNE     'Y'
     C     SCTRST        ANDNE     'N'
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '000100'      *IN(71)
     C                   ENDIF
      *
      *  Hardcopy Y/N
      *
     C     *IN90         IFEQ      *OFF
     C     SCHDCY        ANDNE     'Y'
     C     SCHDCY        ANDNE     'N'
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '000101'      *IN(71)
     C                   ENDIF
      *
      *  Formtype cannot be BLANK, if hardcpy = 'Y'
      *
     C     *IN90         IFEQ      *OFF
     C     SCFTYP        ANDEQ     *BLANKS
     C     SCHDCY        ANDEQ     'Y'
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '000111'      *IN(71)
     C                   ENDIF
      *
      *  Logo Y/N
      *
     C     *IN90         IFEQ      *OFF
     C     SCLOGO        ANDNE     'Y'
     C     SCLOGO        ANDNE     'N'
      *
     C     SCHDCY        IFEQ      'Y'
     C     SCHDCY        OREQ      'N'
     C     SCLOGO        ANDNE     *BLANK
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '001000'      *IN(71)
     C                   ENDIF
     C                   ENDIF
      *
      *  Macro ID cannot be ZERO
      *
     C     *IN90         IFEQ      *OFF
     C     SCMACR        ANDEQ     *ZERO
     C     SCHDCY        ANDEQ     'Y'
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '001001'      *IN(71)
     C                   ENDIF
      *
      *  Check OUTQ cannot be BLANK
      *
     C     *IN90         IFEQ      *OFF
     C     SCOUTQ        ANDEQ     *BLANKS
     C     SCHDCY        ANDEQ     'Y'
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '001010'      *IN(71)
     C                   ENDIF
      *
      *  File Copy OUTQ cannot be BLANK
      *
     C     *IN90         IFEQ      *OFF
     C     SCCPYQ        ANDEQ     *BLANKS
     C     SCHDCY        ANDEQ     'Y'
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '001011'      *IN(71)
     C                   ENDIF
      *
      *  Edit Bank State (required)
      *
     C     *IN90         IFEQ      *OFF
      *
     C                   MOVE      *OFF          *IN60
     C     SCST          IFNE      *BLANKS
     C                   MOVEL     SCST          XXKEY
     C     STKY          CHAIN     PST01                              60
     C                   ENDIF
      *
     C     *IN60         IFEQ      *ON                                          State Not Found
     C     SCST          OREQ      *BLANK                                       State Not Entered
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '101001'      *IN(71)
     C                   ENDIF
     C                   ENDIF                                                  *IN90
      *
     C                   ENDIF                                                  Add or Chg mode
      *
     C*** BEGIN ADD *** 10/11/00 ***************************************
      *
      *  THIS CONTROLS THE MESSAGE DISPLAYED ON SCR02.
      *
      ***** BEG ADD ******* 11/07/03 **********************************
     C     *IN90         IFEQ      *OFF
      ***** END ADD ******* 11/07/03 **********************************
     C                   MOVEA     '000'         *IN(41)
     C     SCLOGO        IFEQ      'N'
     C                   MOVE      *ON           *IN41
     C                   ENDIF
     C     SCLOGO        IFEQ      'Y'
     C                   MOVE      *ON           *IN42
     C                   MOVE      *ON           *IN43
     C                   ENDIF
     C     SCLOGO        IFEQ      *BLANK
     C                   MOVE      *ON           *IN43
     C                   ENDIF
     C*** END ADD ***** 10/11/00 ***************************************
      ***** BEG ADD ******* 11/07/03 **********************************
     C                   ENDIF                                                  *IN90 = *OFF
      ***** END ADD ******* 11/07/03 **********************************
      *
     C                   ENDSR                                                  ERRCK1
      *
      ****************************************************************
      *  ERRCK2 - NO EDITS ON SCR02                                  *
      ****************************************************************
     C     ERRCK2        BEGSR
      *
     C                   ENDSR                                                  ERRCK2
      *
      ****************************************************************
      *  ERRCK3 - Edit A2420F02 SCR03 fields                         *
      ****************************************************************
     C     ERRCK3        BEGSR
      *
      *  Process search requests; status change requests
      *
      *  Routing for MICR cannot be BLANK
      *
     C     *IN90         IFEQ      *OFF
     C     *IN80         ANDEQ     *OFF
     C     SCRTE1        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '001100'      *IN(71)
     C                   ENDIF
      *
      *  Transit Number cannot be BLANK
      *
     C     *IN90         IFEQ      *OFF
     C     *IN80         ANDEQ     *OFF
     C     SCTRNS        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '001101'      *IN(71)
     C                   ENDIF
      *
      *  Check Digit cannot be BLANK
      *
     C     *IN90         IFEQ      *OFF
     C     *IN80         ANDEQ     *OFF
     C     SCCKDG        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '001110'      *IN(71)
     C                   ENDIF
      *
      *  ON-US cannot be BLANK
      *
     C     *IN90         IFEQ      *OFF
     C     *IN80         ANDEQ     *OFF
     C     SCONUS        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '001111'      *IN(71)
     C                   ENDIF
      *
      *  Routing Entry Upper cannot be BLANK
      *
     C     *IN90         IFEQ      *OFF
     C     *IN80         ANDEQ     *OFF
     C     SCRTE2        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '010000'      *IN(71)
     C                   ENDIF
      *
      *  Routing Entry Lower cannot be BLANK
      *
     C     *IN90         IFEQ      *OFF
     C     *IN80         ANDEQ     *OFF
     C     SCRTE3        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '010001'      *IN(71)
     C                   ENDIF
      *
      *  Signature 1 must exist in Check Signature file
      *
     C     *IN90         IFEQ      *OFF
     C     SCNAM1        ANDNE     *BLANKS
     C                   MOVE      UPCO#         XXCO#
     C                   MOVEL     SCNAM1        KKNAME
     C     SGKEY         CHAIN     PSG01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        90
     C                   MOVEA     '010101'      *IN(71)                        ERR MSG
     C                   ENDIF
     C                   ENDIF
      *
      *  Signature 2 must exist in Check Signature file
      *
     C     *IN90         IFEQ      *OFF
     C     SCNAM2        ANDNE     *BLANKS
     C                   MOVE      UPCO#         XXCO#
     C                   MOVEL     SCNAM2        KKNAME
     C     SGKEY         CHAIN     PSG01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        90
     C                   MOVEA     '010110'      *IN(71)                        ERR MSG
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDSR                                                  ERRCK3
      *
      ****************************************************************
      *  GETNXK                                                      *
      ****************************************************************
     C     GETNXK        BEGSR
      *
     C                   ENDSR
      *
      ****************************************************************
      *  KJDO - Process F10 "Table Search" requests.                 *
      ****************************************************************
     C     KJDO          BEGSR
      *
     C                   SELECT
      *
      * ****************************************************************
      *
     C     HFLD          WHENEQ    'SCFND'                                      AP/GL CO# Search
     C     @@MODE        ANDEQ     'A'                                          Add mode only
     C     *IN55         ANDEQ     *OFF                                         NOT Work With Grp
      *
     C                   CALL      'BKD001R'
     C                   PARM      *ZERO         XXCO#
     C                   PARM      *ZERO         XXFAC#
     C                   PARM      *BLANK        QQRTC             1
      *
     C                   SELECT
     C     QQRTC         WHENEQ    'A'
     C                   MOVE      *ON           *INKC
     C                   MOVE      *OFF          *INKL
     C     QQRTC         WHENEQ    'B'
     C                   MOVE      *OFF          *INKC
     C                   MOVE      *ON           *INKL
     C                   OTHER
     C                   MOVE      *OFF          *INKC
     C                   MOVE      *OFF          *INKL
     C                   ENDSL
      *
     C     *INKL         IFEQ      *ON
     C     XXCO#         OREQ      *ZEROS
     C*
     C                   MOVE      '1'           *INKL
     C                   MOVEA     '000011'      *IN(81)
     C*
     C                   ELSE
     C*
     C*                    *** Position cursor to Bank Number
     C                   MOVEA     '000111'      *IN(81)
     C                   MOVE      XXCO#         SCFND
     C*
     C                   ENDIF
      *
      ******************************************************************
      *
     C     HFLD          WHENEQ    'SCAPBK'                                     AP/GL Bank# Search
     C     @@MODE        ANDEQ     'A'                                          Add mode only
      *
     C                   CALL      'BKD020R'
     C                   PARM                    XXAPBK
     C                   PARM                    QQRTC
      *
     C                   SELECT
     C     QQRTC         WHENEQ    'A'
     C                   MOVE      *ON           *INKC
     C                   MOVE      *OFF          *INKL
     C     QQRTC         WHENEQ    'B'
     C                   MOVE      *OFF          *INKC
     C                   MOVE      *ON           *INKL
     C                   OTHER
     C                   MOVE      *OFF          *INKC
     C                   MOVE      *OFF          *INKL
     C                   ENDSL
      *
     C     *INKL         IFEQ      *ON
     C     XXAPBK        OREQ      *ZEROS
     C*
     C                   MOVE      '1'           *INKL
     C                   MOVEA     '000111'      *IN(81)
     C*
     C                   ELSE
     C*
     C*                    *** Position cursor to Bank Account
     C                   MOVEA     '001000'      *IN(81)
     C                   Z-ADD     XXAPBK        SCAPBK
     C*
     C                   ENDIF
      *
      ******************************************************************
      *
     C     HFLD          WHENEQ    'SCACCT'                                     Bank Acct Search
     C     @@MODE        ANDEQ     'A'                                          Add mode only
      *
     C                   CALL      'W2422R'
     C     *INKC         PARM      *INKC         ##KC              1
     C     *INKL         PARM      *INKL         ##KL              1
     C                   PARM      '2'           ##MODE            1
     C                   PARM                    QQCO#             3
     C                   PARM      *BLANKS       ##ACCT
      *
      *                    *** If account # returned
     C     ##ACCT        IFNE      *BLANKS
      *
      *                    *** Position cursor to effective date
     C                   MOVEA     '000001'      *IN(81)
     C                   MOVEL     ##ACCT        SCACCT
      *
     C                   MOVEL     ##ACCT        XXACCT
     C     CKKEY         CHAIN     PCK01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CKAI01        SCBKNM                         Bank Name
     C                   MOVEL(P)  CKAI02        SCBKAD                         Bank Address
     C                   MOVEL(P)  CKAI03        SCBKCS                         Bank City State Zip
     C                   MOVEL(P)  CKVCHR        SCVCHR                         Voucher flag Y/N
     C                   ENDIF
      *
     C                   ELSE
      *                    *** Position cursor to Bank Acct
     C                   MOVEA     '001000'      *IN(81)
     C                   ENDIF
      *
      *
      ******************************************************************
      *
     C     HFLD          WHENEQ    'SCNAM1'                                     Sig 1 Search
      *
     C                   CALL      'W2424R'
     C                   PARM      '2'           ##MODE            1
     C     *INKC         PARM      *INKC         ##KC              1
     C     *INKL         PARM      *INKL         ##KL              1
     C     XXCO#         PARM      XXCO#         ##CO#
     C                   PARM      *BLANKS       ##NAM1
     C                   PARM      *BLANKS       ##10A1
     C                   PARM      *BLANKS       ##CMD1
      *
     C     ##NAM1        IFNE      *BLANKS
     C                   MOVEL     ##NAM1        SCNAM1
     C                   MOVEL     ##10A1        SC10A1
     C                   MOVEL     ##CMD1        SCCMD1
     C                   ENDIF
      *
      *                    *** If Signature 1 name returned
     C     SCNAM1        IFNE      *BLANKS
      *
      *                    *** Position cursor to Signature 2 name
     C                   MOVEA     '000111'      *IN(81)
     C                   ELSE
      *
      *                    *** Position cursor to Signature 1 name
     C                   MOVEA     '000101'      *IN(81)
     C                   ENDIF
      *
      ******************************************************************
      *
     C     HFLD          WHENEQ    'SCNAM2'                                     Sig 2 Search
      *
     C                   CALL      'W2424R'
     C                   PARM      '2'           ##MODE            1
     C     *INKC         PARM      *INKC         ##KC              1
     C     *INKL         PARM      *INKL         ##KL              1
     C                   PARM                    XXCO#
     C                   PARM      *BLANKS       ##NAM2
     C                   PARM      *BLANKS       ##10A2
     C                   PARM      *BLANKS       ##CMD2
      *
     C     ##NAM2        IFNE      *BLANKS
     C                   MOVEL     ##NAM2        SCNAM2
     C                   MOVEL     ##10A2        SC10A2
     C                   MOVEL     ##CMD2        SCCMD2
     C                   ENDIF
      *
      *                    *** If Signature 2 name returned
     C     SCNAM2        IFNE      *BLANKS
      *
      *                    *** Position cursor to File Copy OUTQ
     C                   MOVEA     '000110'      *IN(81)
     C                   ELSE
      *
      *                    *** Position cursor to Signature 2 name
     C                   MOVEA     '000111'      *IN(81)
     C                   ENDIF
      *
      *
      ******************************************************************
      *
     C                   OTHER                                                  FIELD NO SEARCH
     C                   MOVE      *ON           *IN49
      *
     C                   ENDSL
      *
     C                   MOVE      *ON           *IN90
      *
     C                   ENDSR
      *
      ****************************************************************
      *  KUDO - Process F20 ACTIVE/INACTIVE                          *
      ****************************************************************
     C     KUDO          BEGSR
      *
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   ENDIF
      *
     C                   MOVE      *ON           *IN90
      *
     C                   ENDSR
      *
      ****************************************************************
      *  MVDBSC - Assign Bank Information fields to Screen fields   *
      ****************************************************************
     C     MVDBSC        BEGSR
      *
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      UPCO#         QQCO#
     C                   MOVEL     BIACCT        XXACCT
     C     CKKEY         CHAIN     PCK01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CKAI01        SCBKNM                         Bank Name
     C                   MOVEL     CKAI02        SCBKAD                         Bank Address
     C                   MOVEL     CKAI03        SCBKCS                         Bank City State Zip
     C                   MOVE      CKVCHR        SCVCHR                         Voucher flag Y/N
     C     CKVCHR        IFEQ      'N'
      *** BEG CHG *** 02/02/98 ************************************
     C***                  Z-ADD2         #OFSCR
     C                   Z-ADD     3             #OFSCR
      *** END CHG *** 02/02/98 ************************************
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVE      BIFND         SCFND                          FUND NUMBER
     C                   MOVEL     BIONUS        SCONUS                         ON-US FLD UP TO 19A
     C                   MOVEL     BICKDG        SCCKDG                         TRANSIT CHECK DIGIT
     C                   MOVEL     BIBNK#        SCTRNS                         TRANSIT BANK #
     C                   MOVEL     BITRNS        SCRTE1                         TRANSIT ROUTING #
     C                   MOVEL     BIAXOU        SCAUON                         AUX ON-US FLD UPTO
     C                   MOVEL     BISTAT        SCSTAT                         STATUS
     C                   MOVEL     BIRTUP        SCRTE2                         ROUTING ENTRY UPPER
     C                   MOVEL     BIRTLW        SCRTE3                         ROUTING ENRTY LOWER
     C                   MOVEL     BITRST        SCTRST                         TRUST? Y/N
      *
      *** BEG ADD *** 02/02/98 *****************************************
     C     FIRST         IFEQ      *BLANK
     C                   MOVEL     BILOGO        SCLOGO                         LOGO? Y/N
     C                   MOVEL     BILOGO        SVLOGO
     C                   MOVE      'Y'           FIRST             1
     C                   ENDIF
      *
      *  THIS CONTROLS THE MESSAGE DISPLAYED ON SCR02.
      *
     C     SCLOGO        IFNE      SVLOGO
     C     FIRST         OREQ      'Y'
     C                   MOVEA     '000'         *IN(41)
     C     SCLOGO        IFEQ      'N'
     C                   MOVE      *ON           *IN41
     C                   ENDIF
     C     SCLOGO        IFEQ      'Y'
     C                   MOVE      *ON           *IN42
     C                   MOVE      *ON           *IN43
     C                   ENDIF
     C     SCLOGO        IFEQ      *BLANK
     C                   MOVE      *ON           *IN43
     C                   ENDIF
     C                   MOVEL     SCLOGO        SVLOGO
     C                   MOVE      'N'           FIRST
     C                   ENDIF
      *** END ADD *** 02/02/98 *****************************************
     C                   MOVEL     BI$LMT        SC$LMT                         $LIMIT W/O SIGN
     C                   MOVEL     BI10A1        SC10A1                         SIG 1 STRING
     C                   MOVEL     BI10A2        SC10A2                         SIG 2 STRING
     C***                  MOVELBIAPPL    SCAPPL           Application Code
     C                   MOVEL     BIFTYP        SCFTYP                         Form Type
     C                   MOVEL     BINAM1        SCNAM1                         Signature 1 Full N
     C                   MOVEL     BICMD1        SCCMD1                         Sig 1 PCL5 CMD
     C                   MOVEL     BINAM2        SCNAM2                         Signature 2 Full N
     C                   MOVEL     BICMD2        SCCMD2                         Sig 2 PCL5 CMD
     C                   MOVEL     BIMACR        SCMACR                         Macro ID
     C                   MOVEL     BIHDCY        SCHDCY                         Hardcopy Produced?
     C                   MOVEL     BIOUTQ        SCOUTQ                         Check OUTQ
     C                   MOVEL     BICPYQ        SCCPYQ                         File Copy OUTQ
     C***                  MOVELBIANTH    SCANTH           Anthem Y/N
     C                   MOVEL     BIACCT        SCACCT                         Bank Account #
     C*****                MOVELBICKTP    SCCKTP           Check Type
     C                   MOVEL     BIST          SCST                           Bank State Location
     C                   MOVEL     BIAPBK        SCAPBK
      *
     C                   Z-ADD     BIEFDT        SRDTI                          Effective Date
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
      *
     C     *IN40         IFEQ      *ON
     C                   MOVE      'A'           SCSTAT
     C                   ENDIF
      *
      *  "Add" Group occurring from Work With Groups
      *  Avoid pgm recursion thru F10 by assign/protec Group field.
      *
     C     *IN55         IFEQ      *ON
     C                   Z-ADD     @@FND         SCFND
     C                   ENDIF
      *
      *  Voucher Flag
      *
     C                   MOVE      UPCO#         XXCO#
     C                   MOVEL     SCACCT        XXACCT
     C     CKKEY         CHAIN     PCK01                              60
     C     *IN60         IFEQ      '0'
     C     CKVCHR        ANDEQ     'Y'
     C                   MOVE      '1'           *IN80
     C                   ELSE
     C                   MOVE      '0'           *IN80
     C                   ENDIF                                                  VCHR
      *
      *** BEG ADD *** 02/02/98 ************************************
      *
      *    UPDATE MODE
     C                   MOVEL(P)  BI40A1        SC40A1
     C                   MOVEL(P)  BI40A2        SC40A2
     C                   MOVEL(P)  BI40A3        SC40A3
     C                   MOVEL(P)  BI40A4        SC40A4
      *
      *  LOAD FOUR FREE FORM FIELDS WITH GROUP INFO FIRST TIME
      *    ADD MODE
     C     *IN40         IFEQ      *ON                                          ADD MODE
      *
     C                   Z-ADD     @@FND         XXFND
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  FNNAME        SC40A1
     C                   MOVEL(P)  FNMAD1        SC40A2
      *
     C     FNMAD2        IFNE      *BLANK
     C                   MOVEL     FNMAD2        SC40A3
      *
     C     FNMCTY        CAT(P)    ',':0         SC40A4
     C                   CAT       FNMST:1       SC40A4
     C                   MOVEL(P)  FNMZIP        XA5               5
     C                   MOVE(P)   FNMZIP        XA4               4
     C                   CAT       XA5:2         SC40A4
     C                   CAT       '-':0         SC40A4
     C                   CAT       XA4:0         SC40A4
     C                   ENDIF
      *
     C     FNMAD2        IFEQ      *BLANK
     C     FNMCTY        CAT(P)    ',':0         SC40A3
     C                   CAT       FNMST:1       SC40A3
     C                   MOVEL(P)  FNMZIP        XA5               5
     C                   MOVE(P)   FNMZIP        XA4               4
     C                   CAT       XA5:2         SC40A3
     C                   CAT       '-':0         SC40A3
     C                   CAT       XA4:0         SC40A3
      *
     C                   MOVE      *BLANK        SC40A4
     C                   ENDIF
      *
     C                   ENDIF                                                  *IN60
     C                   ENDIF                                                  *IN40
      *** BEG ADD *** 02/02/98 ************************************
      *
     C                   ENDSR
      *
      ****************************************************************
      *  MVSCDB - Assign Screen fields to Bank Information Fields    *
      ****************************************************************
     C     MVSCDB        BEGSR
      *
     C                   MOVE      UPCO#         BICO#                          COMPANY NUMBER
     C                   MOVE      SCFND         BIFND                          FUND NUMBER
     C                   MOVEL     SCAPBK        BIAPBK                         Bank Number
     C                   MOVEL     SCBKNM        BIAI01                         Bank Name
     C                   MOVEL     SCBKAD        BIAI02                         Bank Address
     C                   MOVEL     SCBKCS        BIAI03                         Bank City State Zip
     C                   MOVEL     SCONUS        BIONUS                         ON-US FLD UP TO 19A
     C                   MOVEL     SCCKDG        BICKDG                         TRANSIT CHECK DIGIT
     C                   MOVEL     SCTRNS        BIBNK#                         TRANSIT BANK #
     C                   MOVEL     SCRTE1        BITRNS                         TRANSIT ROUTING #
     C                   MOVEL     SCAUON        BIAXOU                         AUX ON-US FLD UPTO
     C                   MOVEL     SCSTAT        BISTAT                         STATUS
     C                   MOVEL     SCRTE2        BIRTUP                         ROUTING ENTRY UPPER
     C                   MOVEL     SCRTE3        BIRTLW                         ROUTING ENRTY LOWER
     C                   MOVEL     SCTRST        BITRST                         TRUST? Y/N
     C                   MOVEL     SCLOGO        BILOGO                         LOGO? Y/N
     C                   MOVEL     SC$LMT        BI$LMT                         $LIMIT W/O SIGN
     C                   MOVEL     'GL'          BIAPPL                         Application Code
     C                   MOVEL     SCFTYP        BIFTYP                         Form Type
      *
     C     SCNAM1        IFEQ      *BLANK
     C                   MOVE      *BLANK        BI10A1                         SIG 1 STRING
     C                   MOVE      *BLANK        BINAM1                         Signature 1 Full N
     C                   MOVE      *BLANK        BICMD1                         Sig 1 PCL5 CMD
      *
     C                   ELSE
      *  Signature 1 must exist in Check Signature file
      *
     C                   MOVE      UPCO#         XXCO#
     C                   MOVEL(P)  SCNAM1        KKNAME
     C                   MOVEL(P)  SCNAM1        BINAM1                         Signature 1 Full N
     C     SGKEY         CHAIN     PSG01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  SGFONT        BI10A1                         SIG 1 STRING
     C                   MOVEL(P)  SGCMD         BICMD1                         Sig 1 PCL5 CMD
     C                   ENDIF
     C                   ENDIF
      *
     C     SCNAM2        IFEQ      *BLANK
     C                   MOVE      *BLANK        BI10A2                         SIG 2 STRING
     C                   MOVE      *BLANK        BINAM2                         Signature 2 Full N
     C                   MOVE      *BLANK        BICMD2                         Sig 2 PCL5 CMD
      *
     C                   ELSE
      *  Signature 2 must exist in Check Signature file
      *
     C                   MOVE      UPCO#         XXCO#
     C                   MOVEL(P)  SCNAM2        KKNAME
     C                   MOVEL(P)  SCNAM2        BINAM2                         Signature 2 Full N
     C     SGKEY         CHAIN     PSG01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  SGFONT        BI10A2                         SIG 2 STRING
     C                   MOVEL(P)  SGCMD         BICMD2                         Sig 2 PCL5 CMD
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVEL     SCMACR        BIMACR                         Macro ID
     C                   MOVEL     SCHDCY        BIHDCY                         Hardcopy Produced?
     C                   MOVEL     SCOUTQ        BIOUTQ                         Check OUTQ
     C                   MOVEL     SCCPYQ        BICPYQ                         File Copy OUTQ
     C***                  MOVELSCANTH    BIANTH           Anthem Y/N
     C                   MOVEL     SCACCT        BIACCT                         Bank Account #
     C*****                MOVELSCCKTP    BICKTP           Check Type
     C                   MOVEL     SCST          BIST                           Bank State Location
     C                   MOVEL     Q1USER        BIUSER
     C                   MOVEL     Q1PGM         BIPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BIDT
      *
     C                   Z-ADD     DSTIME        BITIME
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BIEFDT                         Effective Date
      *
      *** BEG ADD *** 02/03/98 ***************************************
     C                   MOVEL(P)  SC40A1        BI40A1
     C                   MOVEL(P)  SC40A2        BI40A2
     C                   MOVEL(P)  SC40A3        BI40A3
     C                   MOVEL(P)  SC40A4        BI40A4
      *** END ADD *** 02/03/98 ***************************************
      *
     C                   ENDSR
      *
      ****************************************************************
      *  OTHERR - Used for processing other Modes                    *
      ****************************************************************
     C     OTHERR        BEGSR
      *
     C                   ENDSR
      *
      ****************************************************************
      *  OTHFKS - Used for processing other Function Keys            *
      ****************************************************************
     C     OTHFKS        BEGSR
      *
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C     *INKU         CASEQ     *ON           KUDO                     90
     C                   ENDCS
      *
     C                   ENDSR
      *
      ****************************************************************
      *  PGMEND                                                      *
      ****************************************************************
     C     PGMEND        BEGSR
      *
     C                   MOVE      *ON           *INLR
      *
     C                   ENDSR
      *
      ****************************************************************
      *  PGMSTR                                                      *
      ****************************************************************
     C     PGMSTR        BEGSR
      *
      *** BEG CHG *** 02/02/98 ************************************
     C***                  Z-ADD1         #OFSCR
     C                   Z-ADD     2             #OFSCR            1 0
      *** END CHG *** 02/02/98 ************************************
      *
     C                   MOVE      *OFF          *IN55
     C     @@MODE        IFEQ      'A'
     C                   MOVE      *ON           *IN40
     C     @@FND         IFGT      *ZERO
     C                   MOVE      *ON           *IN55
     C                   Z-ADD     @@FND         SCFND
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *OFF          *IN40
     C                   ENDIF
      *
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*****                MOVEL##DATA    SCPRDS
     C                   ENDSR
      *
      ****************************************************************
      *  PREOTH                                                      *
      ****************************************************************
     C     PREOTH        BEGSR
      *
     C     @@MODE        IFEQ      'S'
      *
     C     @@RRN         CHAIN     DBRCDF                             99
      *
     C     *IN99         IFEQ      *OFF
     C     BISTAT        IFEQ      'I'
     C                   MOVE      'A'           BISTAT
     C                   ELSE
     C                   MOVE      'I'           BISTAT
     C                   ENDIF
      *
     C                   MOVE      Q1USER        BIUSER
     C                   MOVE      Q1PGM         BIPGM
     C                   TIME                    DSDTTM
      *
      ***  BIDT defined 6,0 Zoned - Use Old command
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BIDT
      *
     C                   Z-ADD     DSTIME        BITIME
      *
     C                   UPDATE    DBRCDF
      *
     C                   MOVE      *ON           *INLR
     C                   RETURN
      *
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDSR
      *
      ****************************************************************
      *  UPDATR - Update Payee Demographics file                     *
      ****************************************************************
     C     UPDATR        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             99
      *
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        BIUSER
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BIDT
      *
     C                   MOVEL     DSTIME        BITIME
      *
     C                   UPDATE    DBRCDF
      *
     C                   ENDIF
      *
     C                   ENDSR
      *
      ****************************************************************
      *  WRITER - Write Payee Demographic file                       *
      ****************************************************************
     C     WRITER        BEGSR
      *
     C                   CLEAR                   DBRCDF
     C                   EXSR      GETNXK
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        BIUSER
     C                   TIME                    DSDTTM
      *
      ***  BIDT defined 6,0 Zoned - Use Old command
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BIDT
      *
     C                   MOVEL     DSTIME        BITIME
     C                   MOVE      'A'           BISTAT
      *
     C                   WRITE     DBRCDF
      *
     C                   FEOD      APBIP
     C                   MOVE      *ON           OUTFLG
      *
     C                   ENDSR
