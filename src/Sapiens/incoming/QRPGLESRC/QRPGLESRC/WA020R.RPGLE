     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      * ***************************************************************
      /TITLE W0020R - DISPLAY MISCELLANEOUS MESSAGES
      * ***************************************************************
      *  DESCRIPTION:  THIS PROGRAM WILL DISPLAY AN WINDOW MESSAGE    *
      *                BASED ON THE VALUE OF QQCALL.                  *
      *                                                               *
      *  CALLED FROM:                                                 *
      *                                                               *
      *  CALLS TO:                                                    *
      *    NONE                                                       *
      *                                                               *
      *  CREATED: 08/06/97  PROGRAMMER: C:47  PROJECT: 60ANC          *
      * ***************************************************************
      *  REVISIONS:                                                   *
     F*                                                               *
     F* 10/04/13  C:267  SUPPORT: Red 3116     (COMPILED ONLY)        *
     F*                  Removed HELP botton and 'press ENTER'        *
     F*                  instructions from UI.                        *
     F*                                                               *
     F* 04/29/11  C:189  SUPPORT: 76718 WC025  (COMPILED ONLY)        *
     F*                  THERE IS A GRAMMER MISTAKE IN THE MESSAGE    *
     F*                                                               *
     H*
     H*  03/25/11 C:235 PROJECT: WC1983
     H*                 ADD FROI INSTALL FLAG PROCESSING.
     H*
     H*  04/27/00 C:53  Support: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
      * ***************************************************************
      *  INDICATOR USAGE:                                             *
      *                                                               *
      *  30     Heading: '**********  ERROR  *********'               *
      * N30     Heading: 'This is an information window.              *
      *  31     MSG01: There is no state specific screen for CMJRST.
      *  32     MSG02:
      * ***************************************************************
     FWA020F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D XC01            C                   CONST('someone else')

     C*
     C*================================================================
     C*  Key List Definitions
     C*================================================================
     C*
     C     STKY          KLIST
     C                   KFLD                    XXST             10
     C*
     C*================================================================
     C*  Parameter List Definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
     C                   PARM                    QQINF1           40
     C                   PARM                    QQINF2           40
     C                   PARM                    QQNUM1           13 2
     C                   PARM                    QQNUM2           13 2
     C                   PARM                    QQDAT1            8
     C                   PARM                    QQDAT2            8
     C                   PARM                    QQCALL            1
     C                   PARM                    QQRTCD            1
      *
      *================================================================
      *  Normal Programming Area
      *================================================================
      *
     C                   SELECT
     C     QQCALL        WHENEQ    '1'
     C                   EXSR      SR01
     C     QQCALL        WHENEQ    '2'
     C                   EXSR      SR02
     C*** BEG ADD *** 03/25/11 **********************************
     C     QQCALL        WHENEQ    '3'
     C                   EXSR      SR03
     C*** END ADD *** 03/25/11 **********************************
     C                   ENDSL
      *
     C                   EXFMT     WDW01
      *
     C                   MOVE      *ON           *INLR
     C                   RETURN
      *
      *================================================================
      *  Subroutine SR01 for displaying msg:
      *  There is no state specific screen for XXST.
      *================================================================
     C     SR01          BEGSR
     C                   MOVE      *ON           *IN31
     C                   MOVEL(P)  'MSG01'       SCMSG#
     C                   MOVEL(P)  QQINF1        XXST
      *
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C     STNAME        CAT(P)    '.':0         SCST
     C                   END
      *
     C                   ENDSR
      *
      *================================================================
      *  Subroutine SR02 for displaying msg:
      *
      * 'The jurisdiction state is blank.'                             lank.'
      * 'Use the Claim Master option 6 to add.'                        n 6 to -
      *================================================================
     C     SR02          BEGSR
     C                   MOVE      *ON           *IN32
     C                   MOVEL(P)  'MSG02'       SCMSG#
      *
     C                   ENDSR
     C*** BEG ADD *** 03/25/11 **********************************
     C*================================================================
     C*  Subroutine SR03 for displaying msg:
     C*
     C* 'FROI module is not available for <STATE NAME>'                lank.'
     C*================================================================
     C     SR03          BEGSR
     C                   MOVE      *ON           *IN33
     C                   MOVEL(P)  'MSG03'       SCMSG#
     C                   MOVEL(P)  QQINF1        XXST
     C*
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C     STNAME        CAT(P)    '.':0         SCST
     C                   END
     C*
     C                   ENDSR
     C*** END ADD *** 03/25/11 **********************************
