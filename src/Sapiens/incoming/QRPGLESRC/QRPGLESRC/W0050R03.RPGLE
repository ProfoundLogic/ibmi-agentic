     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H bnddir('WTBNDDIR')
     H/TITLE W0050R03 - Copy Employer / Division
     H*****************************************************************
     H*                                                               *
     H* CALLED BY: W0050R02                                           *
     H*                                                               *
     H* CALLS TO:  WXXXXR - XX NAME XXXXXXXXXXXXXXXXXXX               *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
      *                                                               *
      * 05/23/24 C:300 Support: 115634 (SUNZ)                         *
      *                Update WMFNP Maintenance fields while updating *
      *                Next Available Employer Number                 *
      *
      * 09/17/15 C:302 Project: WC2254
      *                Procedurwe to check if it is a renewal
      *                This program becomes just a shell to
      *                call the new service program WPIPOLCPY
      *                procedure CopyPolicy.
      *
     H* 06/17/15 C:136 PHW35402                                       *
     H*                Allow pgm to process policies with more than   *
     H*                9 years                                        *
     H*                                                               *
     H* 02/27/15 C:280 PHW32340 (CSI)                                 *
     H*                add logic to determine correct effective date  *
     H*                if current date is less then effective date    *
     H*                use current instead of ahefdt                  *
     H*                                                               *
     H*                Removed WRT114 as it is not used anymore       *
     H*                per matt                                       *
     H*                                                               *
     H* 01/28/15 C:136 PHW32790 (CSI)                                 *
     H*                Remove logic that 132WMV implemented. We       *
     H*                don't want to pull in the grandparent employer *
     H*                number when writing the cross reference        *
     H*                                                               *
     H* 02/06/14 C:136  SY336 Priss #48                               *
     H*                 Write a cross reference if the fnd or         *
     H*                 employer number changes                       *
     H*                                                               *
     H* 11/18/13 C:279 Support: WA146 83785                           *
     H*                Modified to not execute changes under redmine  *
     H*                2247 for clientID='146'.                       *
     H*                                                               *
     H* 16/10/13 C:290 Support: WA114 83314                           *
     H*                WRT114 subroutine is same as WRTREC            *
     H*                and therefore use WRTREC for CLT114 as well.   *
     H*                                                               *
     H* 05/31/13 C:33  Redmine: 2644                                  *
     H*                If the new policy is NOT within the same       *
     H*                 carrier or has a lapse in coverage, mark as   *
     H*                 new:                                          *
     H*                  - WDELP/ELNWRW = 'N'                         *
     H*                  - WAQTP/QTTYPE = uppercase of prior policy   *
     H*                                                               *
     H* 05/09/13 C:65  Project: Redmine 2247                          *
     H*                Update the employer master effective date to   *
     H*                the new employer detail policy from date since *
     H*                we are creating a new employer.                *
     H*                                                               *
     H* 10/12/11 C:255 Support: WA139 75199                           *
     H*               -Removed  edit for Agent history file that was  *
     H*                added earlier for this support itself.         *
     H*                                                               *
     H* 12/21/10 C:257 PROJECT: WC1958                                *
     H*                Enhance Employer Copy Process for Agent        *
     H*                History                                        *
     H*                                                               *
     H* 12/17/10 C:136 Support: WA139 75199                           *
     H*               -Determine which year should be written to if   *
     H*                the new policy if changing groups.             *
     H*               -Add edit to ensure that a group year exists    *
     H*                for the new policy                             *
     H*               -Enhance agent check to ensure that an ACTIVE   *
     H*                Agent Assignment record exists for the policy  *
     H*                created by the copy process                    *
     H*                                                               *
     H* 02/16/10 C:189 PROJECT: 132WMV(WC1872)                        *
     H*                Employer Copy in Quote                         *
     H*                                                               *
     H* 01/08/08 C:136 SUPPORT: WA114 61755                           *
     H*                TRANSFER TO LIVE IS CARRYING OVER QUOTE EMP#   *
     H*                WHEN POLICY HAS BEEN COPIED IN THE QUOTE       *
     H*                ENVIRONMENT BECAUSE THE RENEW TO QUOTE FLAG    *
     H*                WASN'T CLEARED BY THE COPY OPTION              *
     H*                                                               *
     H* 08/18/06 C:180 PROJECT 114WMF                                 *
     H*                New pgm - Replaced base version.               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  57       TO Employer is pre-existing, a new division will    *
     H*             be added.                                         *
     H*  58       FROM Policy year entered.                           *
     H*  59       TO Fund is Auto Assign Employer                     *
     H*  60       GENERAL READ/CHAIN                                  *
     H*                                                               *
     H* SCR01                                                         *
     H*  XX - XX  XXX............................XXX                  *
     H*                                                               *
     H* SCR02                                                         *
     H*  XX - XX  XXX............................XXX                  *
     H*                                                               *
     H*****************************************************************
      *
      *
     FW0050F03  CF   E             WORKSTN SFILE(WDW01S:W@Rrn1)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FWMEMP     UF A E             DISK
     F                                     RENAME(PEM01:DBRCDF)
     FWMEML     IF A E           K DISK
     FWMFNL     UF   E           K DISK
     FWDELL     IF A E           K DISK
     FWDELP     IF   E             DISK
     F                                     RENAME(PEL01:PEL02)
     FWMAHL     IF A E           K DISK
     FWMARL     IF   E           K DISK
     FWAQTL7    IF A E           K DISK    USROPN
     FWMEPL     IF A E           K DISK
     FWMOOL     IF A E           K DISK
     FWZEXP     O    E             DISK
     FWMA3L     IF A E           K DISK
     FWMALL     IF A E           K DISK
     FWMANL     IF A E           K DISK
     FWMECL     IF A E           K DISK
      *** BEG ADD ***** 02/16/10 ******************************************
      *** beg del ***** 01/28/15 *******************************
     F***WMEMLQ    IF   E           K DISK    USROPN
     F*********                            RENAME(PEM01:PEM0Q)
      *** end del ***** 01/28/15 *******************************
     FWMAAL1    IF   E           K DISK
      *** beg del ***** 01/28/15 *******************************
     F***WZEXL     IF   E           K DISK
     F********                             RENAME(PEX01:PEX0X)
     F********                             PREFIX(A)
      *** end del ***** 01/28/15 *******************************
     fwdf1l1    if   e           k disk

      *================================================================
      *  /COPY required constants
      *================================================================
     d x               s              2  0                                      Array index
     d y               s              2  0                                      Array index
     d prerr           c                   const('Primary Agent')
     d scerr           c                   const('Secondary Agent')
     d trerr           c                   const('Tertiary Agent')
     d ofrcd           c                   const(' of record')
     d agtnbr          s                   like(ahagt#)                         Agent Number
     d rtnbr           s                   like(ARRT#)                          Agent Number
     d togrp1          s                   like(emco#)                          To Group
     d togrp           s                   like(emco#)                          To Group
     d agttyp          s              1  0                                      Agent type
     d agttyp1         s              1  0                                      Agent type
     d agtrcd          s              1                                         Agent of record
     d agtrcd1         s              1a                                        Agent of record
     d xerror1         s             60a
     d xerror          s             60a
     d errmsg          s             60a   dim(12)
     d CheckAgent      s               n
     d CheckRate       s               n
     dW@Rrn1           s              4  0 inz
      ***  Begin Add  ***  05/31/13  ************************************
     d isRenewal       s              1n

     dCheckRenewal     pr             1n
      ***  End   Add  ***  05/31/13  ************************************
     d*************************************
     d* Procedure for Validating Agent
     d*************************************
     d prValidAgent    pr            60
     d togrp                               like(emco#)                          To Group
     d agtnbr                         5p 0                                      Agent Number
     d agttyp                         1  0                                      Agent type
     d agtrcd                         1a                                        Agent of record
     d*************************************
     d* Procedure for Validating Rate
     d*************************************
     d prValidRate     pr            60
     d togrp                               like(emco#)                          To Group
     d rtnbr                               like(ARRT#)                          Agent Number
     d agttyp                         1  0                                      Agent type
     d agtrcd                         1a                                        Agent of record
     D OPTNM           C                   CONST('Copy Employer / Divi-
     D                                     sion')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(2)

     D XXCLT#          S              3    INZ
      *** beg chg ***** 06/17/15 ******************************************
     d*oyr*******      s              3  0 dim(9)
     d*nyr*******      s              3  0 dim(9)
     d oyr             s              3  0 dim(99)
     d nyr             s              3  0 dim(99)
      *** end chg ***** 06/17/15 ******************************************
     D LIB             s             10
     D type            s             10
     D MODLIB          s             10
     D CMD             s            255
     DRTVENVLIB        PR                  EXTPGM('RTVENVLIB')
     D REQUESTTYPE                   10    const
     D LIBNAME                       10
     D MODLIBNAME                    10
     D/Copy wPrPolCpy
    D/copy sPrRunCmd
     D***  Begin Add  ***  05/31/13  **************************************
    D/copy sPrString
    D/copy wPrEmplr
    D/copy wPrGroup
     D***  End   Add  ***  05/31/13  **************************************

     C*COPY SCOPYMT01C
      /if not Defined(CONVERT)
     D psds          esds                  extname(szq1p)
    D*
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    D*
     D modea           c                   const('-- Add --')
     D modec           c                   const('- Change -')
     D moded           c                   const('-Display-')
     D*** BEGIN CHG *********** 01/28/10 ************************************
     D*** pdsnam       c                   const('Progressive Data So-
     D***                                  lutions, Inc.')
     D pdsnam          c                   const('Stoneriver, Inc.')
     D*** END CHG   ********* 01/28/10 **************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##data          s             80
     D ##leng          s              3  0
     D cpyflg          s              1
     D dsptyp          s              1
     D outflg          s              1
     D savedt          s              7  0
     D savetm          s              6  0
     D scuser          s             10
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D***  Begin Add  ***  01/28/10  **************************************
     DbrandName        s             80    inz
     D*
     D*** BEGIN CHG ****** 08/09/10 ************************************
     D*******/copy SPrRtvBrnd
     DRtvBrnd          pr                  extPgm('RTVBRND')
     D outType                        1    const
     D inLine1                       80
     D inLine2                       80    options(*nopass)
     D outCo#                         3    options(*nopass)
     D*** END   CHG ****** 08/09/10 ************************************
     D***  End   Add  ***  01/28/10  **************************************
     D AV_KEYS_DS      ds                  qualified
     D ARRAY                         81    dim(14)
     D KEYS                        1134    OVERLAY(AV_KEYS_DS)
     D FL1             S             78    DIM(14)                              Function lines
     D FL2             S             78    DIM(14)                              Function lines
     D FL3             S             78    DIM(14)                              Function lines
     D FL4             S             78    DIM(14)                              Function lines
     D FL5             S             78    DIM(14)                              Function lines
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
      /Endif
    C*
    C*  MAINLINE (INTERNAL) INDICATOR REFERENCE SUMMARY
    C*
    C*  09 - Reserved for maintenance/inquiry conditions
    C*  71 - These indicators are to used as error indicators for
    C* thru  the various screens. They will set off after
    C*  89 - displaying each input capible screen.
    C*  90 - Used to general purpose error indicator
    C*  99 - Used when chaining to file being updated
    C*
    C****************************************************************
    C*
    C*  MAINLINE (INTERNAL) SUBROUTINE REFERENCE SUMMARY
    C*
    C*  FINISH - Final processing for file update/display
    C*  GETCOF - Gets company and fund names if possible; otherwise
    C*           get PDS name for company name
    C*  PREADD - Pre-processing before add process
    C*  PRECHG - Pre-processing before change process
    C*  PREDLT - Pre-processing before delete process
    C*  PREDSP - Pre-processing before display process
    C*  XSCR02 - Display second screen and edit check
    C*  XSCR03 - Display third screen and edit check
    C*
    C****************************************************************
    C*  MAINLINE (EXTERNAL) SUBROUTINE REFERENCE SUMMARY
    C*
    C*  DLTPRE - Used for any processing before delete occurs
    C*  DSPLYR - After displaying information
    C*  ERRCK1 - Used to determine errors on the first screen
    C*  ERRCK2 - Used to determine errors on the second screen
    C*  ERRCK3 - Used to determine errors on the third screen
    C*  MVDBSC - Used to move data base fields to screen fields
    C*  MVSCDB - Used to move screen fields to data base fields
    C*  OTHERR - Used for processing other modes
    C*  PGMEND - Used for any end of program processing
    C*  PGMSTR - Used for any start of program processing
    C*  PREOTH - Pre-processing for other modes
    C*  UPDATR - Used to update data base record
    C*  WRITER - Used to write data base record
    C*
    C****************************************************************
    C*
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     ****** beg add ****** 09/06/06 *******************************
     C                   call      'RTVCLTID'
     C                   parm                    CLTID             3
     ****** end add ****** 09/06/06 *******************************
     C                   movel     q1user        scuser
     C                   move      *off          *inkl
     C                   move      *off          outflg
     C                   move      *off          dsptyp
B01  C                   if        @@mode = 'D'
     C                   move      *on           *in09
X01  C                   else
     C                   move      *off          *in09
E01  C                   endif
     C                   move      *off          cpyflg
     C                   if        @@mode = 'P'
     C                   move      *on           cpyflg
     C                   endif
     C                   exsr      getcof
     C                   exsr      getpgt
     C                   exsr      pgmstr
    C*
B01  C                   dou       *inkc = *on
     C                             or *inkl = *on
     C                             or outflg = *on
     C                   exsr      refrsh
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl1
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
      /Endif
B02  C                   dou       *in90 = *off
     C                   exfmt     scr01
     C                   movea     '00000'       *in(71)
     C                   movea     '00000'       *in(76)
     C                   movea     '00000'       *in(81)
     C                   movea     '00000'       *in(86)
B03  C                   if        *inkc = *off
     C                             and *inkl = *off
     C     *inke         caseq     *on           kedo
     C                   cas                     othfks
     C                   endcs
     C                   if        *inkl = *on
     C                   eval      *in90 = *on
     C                   endif
B04  C                   if        *inkc = *off
     C                             and *inkl = *off
B04  C                   if        *in90 = *off
     C                             and @@mode <> 'D'
     C                   exsr      errck1
E04  C                   endif
B04  C                   if        *in90 = *off
B05  C                   if        #ofscr = 1
     C                   exsr      finish
X05  C                   else
     C     @@mode        caseq     'C'           updatr
     C     @@mode        caseq     'D'           dsplyr
     C                   cas                     otherr
     C                   endcs
     C                   exsr      xscr02
E05  C                   endif
E04  C                   endif
E03  C                   endif
E03  C                   endif
E02  C                   enddo
E01  C                   enddo
     C                   exsr      pgmend
    C*
     C                   return
    C*================================================================
    C*  FINISH - Final processing for file update/display
    C*================================================================
     C     finish        begsr
     C     @@mode        caseq     'A'           writer
     C     @@mode        caseq     'C'           updatr
     C     @@mode        caseq     'D'           dsplyr
     C     @@mode        caseq     'P'           writer
     C                   cas                     otherr
     C                   endcs
B01  C                   if        @@mode <> 'A'
     C                   move      *on           outflg
E01  C                   endif
     C                   endsr
    C*================================================================
    C*  GETCOF - Gets company and fund names if possible; otherwise
    C*           get PDS name for company name
    C*================================================================
     C     getcof        begsr
     C                   call      comapi        compl                  99
B01  C                   if        *in99 = *on
     C***  Begin Chg  ***  01/28/10  **************************************
     C**********         movel(p)  pdsnam        ##data
     C                   callp     RtvBrnd('I':brandName)
     C*
     C                   eval      ##data = brandname
     C***  End   Chg  ***  01/28/10  **************************************
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
     C                   movel     ##data        ##conm
E01  C                   endif
     C                   movel     ##conm        scconm
     C                   endsr
    C*================================================================
    C*  GETDTM - Gets date/time before display screen and saves
    C*================================================================
     C     getdtm        begsr
     C                   time                    dsdttm
     C                   eval      savetm = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         savedt
     C                   endsr
    C*================================================================
    C*  GETPGT - Get program title for display heading
    C*================================================================
     C     getpgt        begsr
     C                   movel(p)  optnm         ##data
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
     C                   movel     ##data        scprds
     C                   endsr
    C*================================================================
    C*  KEDO   - Process F5; refresh
    C*================================================================
     C     kedo          begsr
     C                   exsr      refrsh
     C                   move      *on           *in90
     C                   endsr
    C*================================================================
    C*  PREADD - Pre-processing before add process
    C*================================================================
     C     preadd        begsr
     C                   if        cpyflg = *off
     C                   clear                   dbrcdf
     C                   else
     C     @@rrn         chain(n)  dbrcdf                             99
     C                   endif
     C                   movel(p)  modea         scmode
     C                   exsr      mvdbsc
     C                   endsr
    C*================================================================
    C*  PRECHG - Pre-processing before change process
    C*================================================================
     C     prechg        begsr
     C     @@rrn         chain(n)  dbrcdf                             99
     C                   movel(p)  modec         scmode
     C                   exsr      mvdbsc
     C                   endsr
    C*================================================================
    C*  PREDLT - Pre-processing before delete process
    C*================================================================
     C     predlt        begsr
     C     @@rrn         chain(n)  dbrcdf                             99
     C                   exsr      dltpre
     C     @@rrn         delete    dbrcdf                             99
     C                   exsr      pgmend
     C                   return
     C                   endsr
    C*================================================================
    C*  PREDSP - Pre-processing before display process
    C*================================================================
     C     predsp        begsr
     C     @@rrn         chain(n)  dbrcdf                             99
     C                   movel(p)  moded         scmode
     C                   exsr      mvdbsc
     C                   endsr
    C*================================================================
    C*  REFRSH - Refresh screen values and date/time (1st time in/F5)
    C*================================================================
     C     refrsh        begsr
     C     @@mode        caseq     'A'           preadd
     C     @@mode        caseq     'C'           prechg
     C     @@mode        caseq     'D'           predsp
     C     @@mode        caseq     'E'           predlt
     C     @@mode        caseq     'P'           preadd
     C                   cas                     preoth
     C                   endcs
     C                   exsr      getdtm
     C                   endsr
    C*================================================================
    C*  REFRS2 - Refresh screen values and date/time (on screen chg)
    C*================================================================
     C     refrs2        begsr
     C     @@mode        caseq     'C'           prechg
     C     @@mode        caseq     'D'           predsp
     C                   cas                     preoth
     C                   endcs
     C                   exsr      getdtm
     C                   endsr
    C*================================================================
    C*  XSCR02 - Display second screen and edit check
    C*================================================================
     C     xscr02        begsr
     C                   exsr      refrs2
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl2
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
      /Endif
B01  C                   dou       *in90 = *off
     C                   exfmt     scr02
     C                   movea     '00000'       *in(71)
     C                   movea     '00000'       *in(76)
     C                   movea     '00000'       *in(81)
     C                   movea     '00000'       *in(86)
B02  C                   if        *inkc = *off
     C                             and *inkl = *off
     C     *inke         caseq     *on           kedo
     C                   cas                     othfks
     C                   endcs
     C                   if        *inkl = *on
     C                   eval      *in90 = *on
     C                   endif
B03  C                   if        *inkc = *off
     C                             and *inkl = *off
B03  C                   if        *in90 = *off
     C                             and @@mode <> 'D'
     C                   exsr      errck2
E03  C                   endif
B03  C                   if        *in90 = *off
B04  C                   if        #ofscr = 2
     C                   exsr      finish
X04  C                   else
     C     @@mode        caseq     'C'           updatr
     C     @@mode        caseq     'D'           dsplyr
     C                   cas                     otherr
     C                   endcs
     C                   exsr      xscr03
E04  C                   endif
E03  C                   endif
E02  C                   endif
E02  C                   endif
E01  C                   enddo
B01  C                   if        *inkl = *on
     C                   move      *on           *in90
     C                   exsr      refrs2
E01  C                   endif
     C                   endsr
    C*================================================================
    C*  XSCR03 - Display third screen and edit check
    C*================================================================
     C     xscr03        begsr
     C                   exsr      refrs2
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl3
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
      /Endif
B01  C                   dou       *in90 = *off
     C                   exfmt     scr03
     C                   movea     '00000'       *in(71)
     C                   movea     '00000'       *in(76)
     C                   movea     '00000'       *in(81)
     C                   movea     '00000'       *in(86)
B02  C                   if        *inkc = *off
     C                             and *inkl = *off
     C     *inke         caseq     *on           kedo
     C                   cas                     othfks
     C                   endcs
     C                   if        *inkl = *on
     C                   eval      *in90 = *on
     C                   endif
B03  C                   if        *inkc = *off
     C                             and *inkl = *off
B03  C                   if        *in90 = *off
     C                             and @@mode <> 'D'
     C                   exsr      errck3
E03  C                   endif
B03  C                   if        *in90 = *off
     C                   exsr      finish
E03  C                   endif
E02  C                   endif
E02  C                   endif
E01  C                   enddo
B01  C                   if        *inkl = *on
     C                   move      *on           *in90
     C                   exsr      refrs2
E01  C                   endif
     C                   endsr
     C*COPY SCOPYMT01C
      *================================================================
      *  PLIST definitions
      *================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@QTE             1
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      *
      *
      *================================================================
      *  KLIST definitions
      *================================================================
      *
      * Copy of employer related files
      *
     C     CPYKY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFFND
     C                   KFLD                    SCFEMP
     C                   KFLD                    SCFDIV
      *
     C     CPYKY2        KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFFND
     C                   KFLD                    SCFEMP
     C                   KFLD                    SCFDIV
     C                   KFLD                    ELFYR
      *
     C     CPYKY3        KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFFND
     C                   KFLD                    SCFEMP
     C                   KFLD                    SCFDIV
     C                   KFLD                    SCFFYR
      *
      *   Agent History
      *
     C     AHKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFFND
     C                   KFLD                    SCFEMP
     C                   KFLD                    SCFDIV
      *
      * Fund Master
      *
     C     FNKY1         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFFND
      *
     C     FNKY2         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCTFND
      *
      * Employer Master
      *
     C     EMKY1         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCTFND
     C                   KFLD                    SCTEMP
      *
     C     EMKY2         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCTFND
     C                   KFLD                    SCTEMP
     C                   KFLD                    SCTDIV
      *
      * Employer Detail
      *
     C     ELKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFFND
     C                   KFLD                    SCFEMP
     C                   KFLD                    SCFDIV
     C                   KFLD                    SCFFYR
      *
     C     AAKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCTFND
     C                   KFLD                    AHAGT#
      *
     C     EXKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFFND
     C                   KFLD                    SCFEMP
     C                   KFLD                    SCFDIV
      *
      *** beg del ***** 01/28/15 ***************************
     C*****EMKY**        KLIST
     C***********        KFLD                    AEXCO#
     C***********        KFLD                    AEXOFND
     C***********        KFLD                    AEXOEMP
     C***********        KFLD                    AEXODIV
      *** end del ***** 01/28/15 ***************************
      *
     c     f1ky1         klist
     c                   kfld                    ELCO#
     c                   kfld                    sctfnd
     c                   kfld                    ELPFDT

     c     f1ky          klist
     c                   kfld                    ELCO#
     c                   kfld                    sctfnd
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    EMDT          XXDT
      *
      *================================================================
      * DLTPRE - Pre-processing before delete occurs
      *================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
      *
      *================================================================
      * DSPLYR - Processing after displaying information
      *================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
      *
      *================================================================
      * ERRCK1 - Error checking for srceen 1 (SCR01)
      *================================================================
     C     ERRCK1        BEGSR
      *
      *  'FROM' Policy Year must be valid for employer, if entered
      *
     C     *IN90         IFEQ      *OFF
     C     SCFFYR        ANDNE     *ZERO
     C     ELKY          CHAIN     PEL01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   SETON                                        90  73
     C                   ENDIF
      *
     C                   ENDIF
      *
      *  'TO' group can not be blank
      *
     C  N90SCTFND        IFEQ      *ZERO
     C                   SETON                                        90  74
     C                   ENDIF
      *
      *  'TO' group must be valid
      *
     C     *IN90         IFEQ      *OFF
     C     FNKY2         CHAIN(N)  PFN01                              60
     C  N60              MOVEL(P)  FNSHNM        SCTFNM
     C   60              MOVEL(P)  *BLANKS       SCTFNM
     C   60              SETON                                        90  75
     C     FNAUTE        COMP      'Y'                                    59
     C                   ENDIF
      *
      *  Get LAST active agent history record for FROM employer
      *
     C     AHKY          SETGT     PAH01
     C     AHKY          READPE(N) PAH01                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     AHSTAT        ANDNE     'A'
     C     AHKY          READPE(N) PAH01                                  60
     C                   ENDDO
      *
      *  Ensure fund year exists for new group
      *
     c  n90              if        scffnd <> sctfnd
     c     cpyky         setll     pel01
     c     cpyky         reade     pel01
     c                   dow       not(%eof)
     c                   if        scffyr <> 0 and
     c                             ELFYR  <  scffyr
     c     cpyky         reade     pel01
     c                   iter
     c                   endif
     c     f1ky1         setgt     pf101
     c     f1ky          readpe    pf101
     c                   if        %eof or
     c                             ELPFDT >= F1ENPO
     c                   eval      *in76 = *on
     c                   eval      *in90 = *on
     c                   leave
     c                   endif
     c     cpyky         reade     pel01
     c                   enddo
      *
      * Ensure that an active agent assignment record exists in the new group
      *
     c                   endif                                                  scffnd <> sctfnd
     C                   if        *in90 = *off
     C*
     C* Call procedure prValidAgent and prValidRate to validate Primary,Secondary,Tertiary agent
     C* and agent of records.
     C*
     C* 1) Agents and rates for Primary , Secondary and Tertiary
     C*
     c                   movel     sctfnd        togrp                          To group
     c                   z-add     1             agttyp                         Agent type
     c                   move      'N'           agtrcd                         Agent of rec flag
     C*
     c                   z-add     0             x
     c                   exsr      srBldErr
     c*
     c* 2) Agents and rates for Primary , Secondary and Tertiary for Agent of Records
     c*
     c                   eval      agttyp =  1
     c                   eval      agtrcd =  'Y'
     c                   exsr      srBldErr
     C*
     C* If errors are loaded into array then show them on screen
     C*
     C                   if        x <> 0
     c                   exsr      srLodErr
     C* Set on the last record indicator as the file object gets locked.
     C                   seton                                        lr
     C                   endif
     C                   endif                                                  *in90
     C
      *
     C                   ENDSR
      *
      *================================================================
      * ERRCK2 - Error checking for srceen 2 (SCR02)
      *================================================================
     C     ERRCK2        BEGSR
      *
      * If Auto Assign employer AND 'TO' employer entered,
      * 'TO' employer must exist.
      *
     C  N90*IN59         IFEQ      *ON
     C     SCTEMP        ANDNE     *ZERO
     C     EMKY1         CHAIN(N)  PEM01                              60
     C   60              SETON                                        71  90
     C                   ENDIF
      *
      * If Auto Assign employer AND 'TO' employer entered,
      * 'TO' employer division must not exist.
      *
     C  N90*IN59         IFEQ      *ON
     C     SCTEMP        ANDNE     *ZERO
     C     EMKY2         CHAIN(N)  PEM01                              60
     C  N60              SETON                                        72  90
     C                   ENDIF
      *
      *  If Group is auto assign and value for employer has been entered
      *  and Group does not allow *zero division
      *
     C  N90*IN59         IFEQ      *ON
     C     SCTEMP        ANDNE     *ZEROS
     C     SCTDIV        ANDEQ     *ZEROS
     C     FNDIV0        ANDEQ     'N'
     C                   SETON                                        75  90
     C                   ENDIF
      *
      * If NOT Auto Assign employer then TO employer / div must be entered.
      *
     C  N90*IN59         IFEQ      *OFF
     C     SCTEMP        ANDEQ     *ZERO
     C  N60              SETON                                        73  90
     C                   ENDIF
      *
      * If NOT Auto Assign employer then
      *  then TO employer / div must not already exist.
      *
     C  N90*IN59         IFEQ      *OFF
     C     EMKY2         CHAIN(N)  PEM01                              60
     C  N60              SETON                                        74  90
     C                   ENDIF
      *
      *  If Group is NOT auto assign
      *  and Group does not allow *zero division
      *
     C  N90*IN59         IFEQ      *OFF
     C     FNDIV0        ANDEQ     'N'
     C     SCTDIV        ANDEQ     *ZEROS
     C                   SETON                                        75  90
     C                   ENDIF
      *
      *  Set indicator *off if employer is pre-existing
      *
     C  N90EMKY1         CHAIN(N)  PEM01                              57
      *
     C                   ENDSR
      *
      *================================================================
      * ERRCK3 - Error checking for srceen 3 (SCR03)
      *================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
      *
      *================================================================
      * KJDO - SEARCHES
      *================================================================
     C     KJDO          BEGSR
      *
     C                   SELECT
      *
      * FROM Policy Year
      *
     C     HFLD          WHENEQ    'SCFFYR'
     C                   CALL      'W0051R'                             99
     C                   PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZERO         ##RRN             9 0
     C                   PARM      @@RRN         ##RRN2            9 0
     C                   PARM      @@QTE         ##QTE             1
      *
     C     ##RRN         IFEQ      *ZERO
     C                   SETON                                        71
      *
     C                   ELSE
      *
     C     ##RRN         CHAIN     PEL02                              60
     C                   Z-ADD     ELFYR         SCFFYR
     C                   SETON                                        72
     C                   ENDIF
      *
      * TO fund
      *
     C     HFLD          WHENEQ    'SCTFND'
      *
     C                   MOVE      SCCO#         #1CO#
      *
     C                   CALL      'W0010R'                             99
     C                   PARM      *OFF          #1KC              1
     C                   PARM      *OFF          #1KL              1
     C                   PARM      '2'           #1CALL            1
     C                   PARM                    #1CO#             3
     C                   PARM      *BLANKS       #1FND             3
      *
     C     #1FND         IFEQ      *BLANKS
     C                   SETON                                        72
      *
     C                   ELSE
      *
     C                   MOVE      #1FND         SCTFND
     C     FNKY2         CHAIN(N)  PFN01                              60
     C  N60              MOVEL(P)  FNSHNM        SCTFNM
     C   60              MOVEL(P)  *BLANKS       SCTFNM
     C                   SETON                                        71
     C                   ENDIF
      *
     C                   ENDSL
      *
     C                   ENDSR
      *
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
      *
     C                   ENDSR
      *
      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
     C     MVSCDB        BEGSR
     C                   ENDSR
      *
      *================================================================
      * OTHERR - Processing for other modes
      *================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
      *
      *================================================================
      * OTHFKS - Processing for other functions keys
      *================================================================
     C     OTHFKS        BEGSR
      *
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
      *
     C                   ENDSR
      *
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
      *
      *================================================================
      * srBldErr - Load errors into array
      *================================================================
     C     srBldErr      begsr
     c*
     c                   dow       agttyp <= 3
     c                   eval      xerror1 =  *blanks
     c* Change Agent number as per the agent type
     c                   select
     c                   when      agttyp = 1 and agtrcd  = 'N'
     c                   movel     ahagt#        agtnbr                         AGENT # ASSIGNED BY
     c                   move      ahrt#         rtnbr                          RATE NUMBER
     c                   when      agttyp = 2 and agtrcd  = 'N'                 FUND
     c                   movel     ahsagt        agtnbr                         SECONDARY AGENT #
     c                   move      ahsrt#        rtnbr                          SECONDARY RATE #
     c                   when      agttyp = 3 and agtrcd  = 'N'
     c                   movel     ah3agt        agtnbr                         TERTIARY AGENT #
     c                   move      ah3rt#        rtnbr                          TERTIARY RATE #
     c                   when      agttyp = 1 and agtrcd  = 'Y'
     c                   movel     ahagrc        agtnbr                         AGT OF RECORD
     c                   move      ahrtrc        rtnbr                          RATE# AGT OF RECORD
     c                   when      agttyp = 2 and agtrcd  = 'Y'
     c                   movel     ah2arc        agtnbr                         SECONDARY AGT OF REC
     c                   movel     ah2rrc        rtnbr                          SECONDARY AGT OF REC
     c                   when      agttyp = 3 and agtrcd  = 'Y'
     c                   movel     ah3arc        agtnbr                         TERTIARY AGT OF  REC
     c                   move      ah3rrc        rtnbr                          TER. RT # AGT OF REC
     c                   endsl
     c*
     c                   if        agtnbr <> 0
     c                   eval      xerror1 = prValidAgent(togrp:agtnbr:agttyp
     c                                      :agtrcd)
     c                   endif
     c*
     c* Load errors to runtime array
     c*
     c                   if        xerror1 <> ' '
     c                   eval      x = x+1
     c                   eval      errmsg(x) = xerror1                          arry to store error
     c                   eval      xerror1 =  *blanks
     c                   endif
     c*
     C                   if        *in90 = *off and SCFFND <> SCTFND
     c                             and agtnbr <> 0                              Chck rt if agt exist
     c                   eval      xerror1= prValidRate(togrp:rtnbr:agttyp
     c                                      :agtrcd)
     c                   endif
     c*
     c* Load errors to runtime array
     c*
     c                   if        xerror1 <> ' '
     c                   eval      x = x+1
     c                   eval      errmsg(x) = xerror1                          arry to store error
     c                   eval      xerror1 =  *blanks
     c                   endif
     c*
     c                   eval      agttyp = agttyp + 1
     c                   enddo
     c*
     C                   ENDSR
      *================================================================
      * srLodErr - Load errors from Array to Screen
      *================================================================
     C     srLodErr      begsr
     c*
     c                   z-add     1             y
     c                   eval      *In(45) = *On                                clear subfile
     c                   write     WDW01C                                       control rec fmt
     c                   eval      *In(45) = *Off
     c                   eval      W@Rrn1  = 1                                  Subfile RRN
     c                   dow       W@Rrn1 <= x
     c                   eval      scerror = errmsg(y)
     c                   write     WDW01S                                       Sfl rec fmt
     c                   eval      W@Rrn1 = W@Rrn1 + 1
     c                   eval      y = y + 1
     c                   enddo
     c                   exfmt     WDW01C                                       control rec fmt
     c                   DoW       *INKl = *Off
     c                   exfmt     WDW01C                                       control rec fmt
     c                   enddo
     C   KL              seton                                        90
     c                   endsr
      *
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
      *
     C                   CLEAR                   DBRCDF
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C                   Z-ADD     EMCO#         SCCO#
     C                   Z-ADD     EMFND         SCFFND
      *
     C     FNKY1         CHAIN(N)  PFN01                              60
     C  N60              MOVEL(P)  FNSHNM        SCFFNM
     C   60              MOVEL(P)  *BLANKS       SCFFNM
      *
     C                   Z-ADD     EMEMP#        SCFEMP
     C                   Z-ADD     EMDIV         SCFDIV
     C                   MOVEL(P)  EMNAME        SCFENM
      *
     C                   Z-ADD     0             SCFFYR
     C                   Z-ADD     0             SCTFND
     C                   Z-ADD     0             SCTEMP
     C                   Z-ADD     0             SCTDIV
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#
     C                   ENDSR
      *
      *================================================================
      * PREOTH - Pre-processing for other modes
      *================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
      *
      *================================================================
      * UPDATR - Update data base record and after processing
      *================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C   99
     CANN99              UPDATE    DBRCDF
     C                   ENDIF
     C                   ENDSR
      *
      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
     C     WRITER        BEGSR
      *
      * If Auto Assign employer AND 'TO' employer NOT entered,
      * assign next available employer and division.
      *
     C  N90*IN59         IFEQ      *ON
     C     SCTEMP        ANDEQ     *ZERO
      *
     C     FNKY2         CHAIN     PFN01                              60
      *
     C     @@QTE         IFEQ      'Q'
     C                   Z-ADD     FNNAQN        SCTEMP
     C                   ADD       1             FNNAQN
      *
     C                   ELSE
      *
     C                   Z-ADD     FNNAEN        SCTEMP
     C                   ADD       1             FNNAEN
     C                   ENDIF
      *
      *** Beg Add ***** 05/23/24 ***********************
     C                   MOVE      Q1USER        FNUSER
     C                   MOVE      Q1PGM         FNPGM
     C                   exsr      getdtm
     C                   Z-ADD     SAVEDT        FNDT
     C                   MOVE      DSTIME        FNTIME
      *** End Add ***** 05/23/24 ***********************
      *
     C                   UPDATE    PFN01
      *
     C     FNDIV0        IFEQ      'Y'
     C                   Z-ADD     0             SCTDIV
      *
     C                   ELSE
      *
     C                   Z-ADD     1             SCTDIV
     C                   ENDIF
      *
     C                   ENDIF
      *
      *  Write new Employer / Division records
      *
     C
     C                   SELECT
     C                   WHEN      XXCLT# = '114'
     C*** BEGIN CHG *********** 16/10/13 ************************************
     C*********          EXSR      WRT114
     C                   EXSR      WRTREC
     C*** END CHG *********** 16/10/13 **************************************
     C                   OTHER
     C                   EXSR      WRTREC
     C                   ENDSL
      *
      *  Display Confirmation window if Auto-Assigned group
      *
     C     *IN59         IFEQ      *ON
     C                   EXFMT     WDW02
     C                   ENDIF
      *
      *
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C   99
     CANN99              WRITE     DBRCDF
     C                   FEOD      WMEMP
     C                   MOVE      *ON           OUTFLG
      *
     C                   ENDSR
      *
      *================================================================
      * WRTREC - Write new Employer / Division records
      *================================================================
     C     WRTREC        BEGSR
      *
     C     SCFFYR        COMP      *ZERO                                  58
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     c*** begin add ***  05/09/13  *********************************************************
     C                   z-add     0             xxefd1            7 0
     C   58CPYKY         CHAIN     PEL01                              60
     C  N58CPYKY3        CHAIN     PEL01                              60
     C                   If        *in60 = *off
     c                   z-add     elpfdt        xxefd1
     c                   endif
     c*** end add *****  05/09/13  *********************************************************
      *                                                                    *****
      * Copy records from the Employer Master File
      *                                                                    *****
     C     CPYKY         CHAIN     PEM01                              60
      *
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     SCCO#         EMCO#
     C                   Z-ADD     SCTFND        EMFND
     C                   Z-ADD     SCTEMP        EMEMP#
     C                   Z-ADD     SCTDIV        EMDIV
      *
     C  N57              MOVE      'N'           EMSTRP
      *
     c*** begin add ***  05/09/13  *********************************************************
     c***** beg chg ***** 11/18/13 *********************************************************
     C*****              If        xxefd1 <> 0
      /free
       If xxefd1 <> 0 and xxClt# <> '146';
      /end-free
     c***** end chg ***** 11/18/13 *********************************************************
     c                   z-add     xxefd1        emefd1
     c                   z-add     0             emefd2
     c                   z-add     0             emefd3
     c                   z-add     0             emefd4
     c                   z-add     0             emefd5
     c                   z-add     0             emcad1
     c                   z-add     0             emcad2
     c                   z-add     0             emcad3
     c                   z-add     0             emcad4
     c                   z-add     0             emcad5
     c                   endif
     c*** end add *****  05/09/13  *********************************************************
     C                   MOVEL     Q1USER        EMUSER
     C                   MOVEL     Q1USER        EMIUSR
     C                   MOVEL     Q1PGM         EMPGM
     C                   MOVEL     Q1PGM         EMIPGM
     C                   Z-ADD     XXDT          EMDT
     C                   Z-ADD     XXDT          EMIDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EMTIME
     C                   Z-ADD     DSTIME        EMITIM
     C                   CLEAR                   EMRQFL
      *
     C                   WRITE     PEM01
      *
     C     CPYKY         READE     PEM01                                  60
     C                   ENDDO
      *
      *
      *
      *  Copy records from the Employer Detail File
      *
     c                   z-add     1             xi                3 0
     C   58CPYKY         CHAIN     PEL01                              60
     C  N58CPYKY3        CHAIN     PEL01                              60
      *
     C     *IN60         DOWEQ     *OFF
      //***  Begin Add  ***  05/31/13  ************************************
      /Free
       isRenewal = CheckRenewal;
      /End-free
      //***  End   Add  ***  05/31/13  ************************************
     C                   Z-ADD     SCCO#         ELCO#
     C                   Z-ADD     SCTFND        ELFND
     C                   Z-ADD     SCTEMP        ELEMP#
     C                   Z-ADD     SCTDIV        ELDIV
      *
      * Check to see which fund year is going to be assigned
      * to the new fund. Create arrays to cross reference when
      * writing payrolls
      *
     c                   if        scffnd <> sctfnd
     c     f1ky1         setgt     pf101
     c     f1ky          readpe    pf101
     c                   eval      oyr(xi) = ELFYR
     c                   eval      nyr(xi) = F1FYR
     c                   add       1             xi
     c                   eval      ELFYR  = F1FYR
     c                   endif
      *
     C                   MOVEL     *BLANK        ELFAF
     C                   CLEAR                   ELMAPR
     C                   CLEAR                   ELAMPR
     C                   CLEAR                   ELESBJ
     C                   CLEAR                   ELASBJ
     C                   CLEAR                   ELSTPR
     C                   CLEAR                   ELASPR
     C                   CLEAR                   ELEMPM
     C                   CLEAR                   ELAMPM
     C                   CLEAR                   ELENMP
     C                   CLEAR                   ELANMP
     C                   CLEAR                   ELANPR
     C                   CLEAR                   ELAAPR
     C                   CLEAR                   ELINMP
     C                   CLEAR                   ELTXPR
     C                   CLEAR                   ELPRTD
     C                   CLEAR                   ELTCLP
     C                   CLEAR                   ELTXDU
     C                   CLEAR                   ELEDC1
     C                   CLEAR                   ELADC1
     C                   CLEAR                   ELEDC2
     C                   CLEAR                   ELADC2
     C                   CLEAR                   ELEDC3
     C                   CLEAR                   ELADC3
     C                   CLEAR                   ELEXP1
     C                   CLEAR                   ELEXP2
     C                   CLEAR                   ELEXP3
     C                   CLEAR                   ELAXP1
     C                   CLEAR                   ELAXP2
     C                   CLEAR                   ELAXP3
     C                   CLEAR                   ELEIN1
     C                   CLEAR                   ELAIN1
     C                   CLEAR                   ELEIN2
     C                   CLEAR                   ELAIN2
     C                   CLEAR                   ELEDCT
     C                   CLEAR                   ELADCT
     C                   CLEAR                   ELEDCM
     C                   CLEAR                   ELADCM
     C                   CLEAR                   ELEDCS
     C                   CLEAR                   ELADCS
     C                   CLEAR                   ELEDCR
     C                   CLEAR                   ELADCR
     C                   CLEAR                   ELEDC
     C                   CLEAR                   ELADC
     C                   CLEAR                   ELEMIN
     C                   CLEAR                   ELAMIN
     C                   CLEAR                   ELEMAX
     C                   CLEAR                   ELAMAX
     C                   CLEAR                   ELSTAM
     C                   CLEAR                   ELRETN
     C                   CLEAR                   ELEFRS
     C                   CLEAR                   ELAFRS
     C                   CLEAR                   EL1APR
     C                   CLEAR                   EL2APR
     C                   CLEAR                   ELSBFE
      //***  Begin Add  ***  05/31/13  ************************************
      /Free
       If not isRenewal;
          ELnwRw = 'N';
       Endif;
      /End-free
      //***  End   Add  ***  05/31/13  ************************************
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1USER        ELIUSR
     C                   MOVEL     Q1PGM         ELPGM
     C                   MOVEL     Q1PGM         ELIPGM
     C                   Z-ADD     XXDT          ELDT
     C                   Z-ADD     XXDT          ELIDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        ELTIME
     C                   Z-ADD     DSTIME        ELITM
      *
     C                   WRITE     PEL01
      *                                                                    *****
      * Copy last active records from the Agent History File, each year
      *                                                                    *****
     C     CPYKY2        SETGT     PAH01
     C     CPYKY2        READPE    PAH01                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     AHSTAT        ANDEQ     'I'
     C     CPYKY2        READPE    PAH01                                  60
     C                   ENDDO
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     SCCO#         AHCO#
     C                   Z-ADD     SCTFND        AHFND
     C                   Z-ADD     SCTEMP        AHEMP#
     C                   Z-ADD     SCTDIV        AHDIV
     c                   if        scffnd <> sctfnd
     c                   eval      AHFYR  = F1FYR
     c                   endif
     c*** begin add ***  05/09/13  *********************************************************
     c***** beg chg ***** 11/18/13 *********************************************************
     C*****              If        xxefd1 <> 0
      /free
       If xxefd1 <> 0 and xxClt# <> '146';
      /end-free
     c***** end chg ***** 11/18/13 *********************************************************
     c                   z-add     xxefd1        ahefdt
     c*** begin add ***  02/27/15  *********************************************************
      /free

         if savedt < ahefdt;
          ahefdt = savedt;
         endif;

      /end-free
     c*** end   add ***  02/27/15  *********************************************************
     c                   endif
     c*** end add *****  05/09/13  *********************************************************
     C                   MOVEL     Q1USER        AHUSER
     C                   MOVEL     Q1USER        AHIUSR
     C                   MOVEL     Q1PGM         AHPGM
     C                   MOVEL     Q1PGM         AHIPGM
     C                   Z-ADD     XXDT          AHDT
     C                   Z-ADD     XXDT          AHIDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        AHTIME
     C                   Z-ADD     DSTIME        AHITM
      *
     C                   WRITE     PAH01
     C                   ENDIF
      *
     C   58CPYKY         READE     PEL01                                  60
     C  N58CPYKY3        READE     PEL01                                  60
     C                   ENDDO
      *
      * Copy Employer Payroll
      *
     C   58CPYKY         CHAIN     PEP01                              60
     C  N58CPYKY3        CHAIN     PEP01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     EPSTAT        IFEQ      'A'
     C                   Z-ADD     SCCO#         EPCO#
     C                   Z-ADD     SCTFND        EPFND
     C                   Z-ADD     SCTEMP        EPEMP#
     C                   Z-ADD     SCTDIV        EPDIV
     c                   if        scffnd <> sctfnd
     c                   eval      xi = %lookup(EPFYR : oyr)
     c                   eval      EPFYR = nyr(xi)
     c                   endif
     C                   MOVEL     Q1USER        EPUSER
     C                   MOVEL     Q1USER        EPIUSR
     C                   MOVEL     Q1PGM         EPPGM
     C                   MOVEL     Q1PGM         EPIPGM
     C                   Z-ADD     XXDT          EPDT
     C                   Z-ADD     XXDT          EPIDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EPTIME
     C                   Z-ADD     DSTIME        EPITM
      *
     C                   WRITE     PEP01
     C                   ENDIF
      *
     C   58CPYKY         READE     PEP01                                  60
     C  N58CPYKY3        READE     PEP01                                  60
     C                   ENDDO
      *
      *  Copy records from the Quote Tracking File
      *                                                                    *****
     C                   OPEN      WAQTL7
     C     CPYKY         CHAIN     PQT01                              60
      *
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     SCCO#         QTCO#
     C                   Z-ADD     SCTFND        QTFND
     C                   Z-ADD     SCTEMP        QTEMP#
     C                   Z-ADD     SCTDIV        QTDIV
      //***  Begin Add  ***  05/31/13  ************************************
      /Free
       If not isRenewal;
          Select;
          When QTtype = 'F';
             QTtype = 'p';
          When QTtype = 'G';
             QTtype = 'q';
          Endsl;
          QTtype = Upper(QTtype);
       Endif;
      /End-free
      //***  End   Add  ***  05/31/13  ************************************
     C                   MOVEL     Q1USER        QTUSER
     C                   MOVEL     Q1USER        QTIUSR
     C                   MOVEL     Q1PGM         QTPGM
     C                   MOVEL     Q1PGM         QTIPGM
     C                   Z-ADD     XXDT          QTDT
     C                   Z-ADD     XXDT          QTIDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        QTTIME
     C                   Z-ADD     DSTIME        QTITM
      *
     C                   WRITE     PQT01
      *
     C     CPYKY         READE     PQT01                                  60
     C                   ENDDO
      *
     C                   CLOSE     WAQTL7
      *
      *  Copy records from the Owner/Officer File
      *
     C     CPYKY         CHAIN     POO01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     OOSTAT        IFEQ      'A'
     C                   Z-ADD     SCCO#         OOCO#
     C                   Z-ADD     SCTFND        OOFND
     C                   Z-ADD     SCTEMP        OOEMP#
     C                   Z-ADD     SCTDIV        OODIV
      *
     C                   MOVEL     Q1USER        OOUSER
     C                   MOVEL     Q1USER        OOIUSR
     C                   MOVEL     Q1PGM         OOPGM
     C                   MOVEL     Q1PGM         OOIPGM
     C                   Z-ADD     XXDT          OODT
     C                   Z-ADD     XXDT          OOIDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        OOTIME
     C                   Z-ADD     DSTIME        OOITIM
      *
     C                   WRITE     POO01
     C                   ENDIF
      *
     C     CPYKY         READE     POO01                                  60
     C                   ENDDO
      *
      *
      *  Copy records from the Employer Contact file WMECP
      *
     C     CPYKY         CHAIN     PEC01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   Z-ADD     SCCO#         ECCO#
     C                   Z-ADD     SCTFND        ECFND
     C                   Z-ADD     SCTEMP        ECEMP#
     C                   Z-ADD     SCTDIV        ECDIV
      *
     C                   MOVEL     Q1USER        ECUSER
     C                   MOVEL     Q1USER        ECIUSR
     C                   MOVEL     Q1PGM         ECPGM
     C                   MOVEL     Q1PGM         ECIPGM
     C                   Z-ADD     XXDT          ECDT
     C                   Z-ADD     XXDT          ECIDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        ECTIME
     C                   Z-ADD     DSTIME        ECITIM
      *
     C                   WRITE     PEC01
      *
     C     CPYKY         READE     PEC01                                  60
     C                   ENDDO
      *
      *
      *  Copy record from the Employer Third Address File
      *
     C     CPYKY         CHAIN     PA301                              60
      *
     C     *IN60         IFEQ      *OFF
     C     A3STAT        ANDNE     'I'
     C                   Z-ADD     SCCO#         A3CO#
     C                   Z-ADD     SCTFND        A3FND
     C                   Z-ADD     SCTEMP        A3EMP#
     C                   Z-ADD     SCTDIV        A3DIV
      *
     C                   MOVEL     Q1USER        A3USER
     C                   MOVEL     Q1USER        A3IUSR
     C                   MOVEL     Q1PGM         A3PGM
     C                   MOVEL     Q1PGM         A3IPGM
     C                   Z-ADD     XXDT          A3DT
     C                   Z-ADD     XXDT          A3IDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        A3TIME
      *
     C                   WRITE     PA301
     C                   ENDIF
      *
      *
      *  Copy records from the Entity/Location File
      *
     C     CPYKY         CHAIN     PAL01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   Z-ADD     SCCO#         ALCO#
     C                   Z-ADD     SCTFND        ALFND
     C                   Z-ADD     SCTEMP        ALEMP#
     C                   Z-ADD     SCTDIV        ALDIV
      *
     C                   MOVEL     Q1USER        ALUSER
     C                   MOVEL     Q1USER        ALIUSR
     C                   MOVEL     Q1PGM         ALPGM
     C                   MOVEL     Q1PGM         ALIPGM
     C                   Z-ADD     XXDT          ALDT
     C                   Z-ADD     XXDT          ALIDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        ALTIME
      *
     C                   WRITE     PAL01
      *
     C     CPYKY         READE     PAL01                                  60
     C                   ENDDO
      *
      *  Copy records from the Additional Named Insured
      *
     C     CPYKY         CHAIN     PAN01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     ANSTAT        IFNE      'I'
     C                   Z-ADD     SCCO#         ANCO#
     C                   Z-ADD     SCTFND        ANFND
     C                   Z-ADD     SCTEMP        ANEMP#
     C                   Z-ADD     SCTDIV        ANDIV
      *
     C                   MOVEL     Q1USER        ANUSER
     C                   MOVEL     Q1USER        ANIUSR
     C                   MOVEL     Q1PGM         ANPGM
     C                   MOVEL     Q1PGM         ANIPGM
     C                   Z-ADD     XXDT          ANDT
     C                   Z-ADD     XXDT          ANIDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        ANTIME
      *
     C                   WRITE     PAN01
     C                   ENDIF
      *
     C     CPYKY         READE     PAN01                                  60
     C                   ENDDO
      *
      *  Write Cross Reference Record if LIVE environment
      *
     C     SCFFND        IFNE      SCTFND
      *** BEG ADD ***** 02/06/14 ************************************
     C     SCFEMP        ORNE      SCTEMP
      *** END ADD ***** 02/06/14 ************************************
     C                   Z-ADD     SCCO#         EXCO#
     C                   Z-ADD     SCTFND        EXFND
     C                   Z-ADD     SCTEMP        EXEMP#
     C                   Z-ADD     SCTDIV        EXDIV
     C                   Z-ADD     SCFFND        EXOFND
     C                   Z-ADD     SCFEMP        EXOEMP
     C                   Z-ADD     SCFDIV        EXODIV
     C                   MOVEL     Q1USER        EXIUSR
     C                   MOVEL     Q1USER        EXUSER
     C                   MOVEL     Q1PGM         EXPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EXIDT
     C                   MOVEL     SRDTO         EXDT
     C                   MOVEL     DSTIME        EXTIME
      *
      *** beg del ***** 01/28/15 ***************************************
     C***********        IF        @@QTE  = 'Q'
     C***********        CALLP     RTVENVLIB('*OTHER':LIB:MODLIB)
     C***********        eval      CMD = 'OVRDBF FILE(WMEMLQ) TOFILE('
     C***********                      + %trim(lib) + '/WMEMLQ)'
     C***********        CALLP     RUNCMD(CMD:%LEN(CMD))
      ***********
     C***********        if        NOT %OPEN(WMEMLQ)
     C***********        OPEN      WMEMLQ
     C***********        ENDIF
      ***********
     C*****EXKY**        CHAIN     PEX0X                              60
     C*****EMKY**        CHAIN     PEM0Q                              70
     C***********
     C***********        IF        *IN60 = *OFF  and *IN70 = *off
     C***********        EVAL      EXOFND  =  AEXOFND
     C***********        EVAL      EXOEMP  =  AEXOEMP
     C***********        EVAL      EXODIV  =  AEXODIV
     C***********        MOVE      *ON           @@UPD             1
     C***********        ENDIF
     C***********
     C***********        IF        %OPEN(WMEMLQ)
     C***********        CLOSE     WMEMLQ
     C***********        ENDIF
     C***********
     C***********        ENDIF
      *** end del ***** 01/28/15 ***************************************
     C                   WRITE     PEX01
     C                   FEOD      WZEXP
     C                   ENDIF
      *
     C                   ENDSR

      *** Beg Add ***** 09/17/15 ***************************************
     C     *InzSr        begsr
      /Free
          type = 'Master';
          CopyPolicy(@@rrn:@@Qte:type);
          *InLr =*on;
          Return;
      /End-Free
     C                   endsr

      ***  Begin Add  ***  05/31/13  ************************************
     PCheckRenewal     b
     dCheckRenewal     pi             1n
      /Free
       Select;
       When not SameCarrier(ELco#:scFFnd:scTFnd);
          Return *off;
       When CoverageLapse(ELco#:ELfnd:ELemp#:ELdiv:ELfyr:@@qte);
          Return *off;
       Other;
          Return *on;
       Endsl;
      /End-free
     PCheckRenewal     e
      ***  End   Add  ***  05/31/13  ************************************

     *====================================================================
      * prValidAgent
     *====================================================================
     pprValidAgent     b
     dprValidAgent     pi            60
     dxtogrp                               like(emco#)                          To Group
     dxagtnbr                         5p 0                                      Agent Number
     dxagttyp                         1  0                                      Agent type
     dxagtrcd                         1a                                        Agent of record
     dxtogrpnm         s              3a                                        To Group Char
     d*
     d* Error messages
     d*
     d ntvld           c                   const(' is not valid for group ')
     d*
     C***********************************
     C* klist
     C***********************************
     C* for file WMAAL1
     C*
     c     aaky          klist
     c                   kfld                    scCO#
     c                   kfld                    Xtogrp
     c                   kfld                    Xagtnbr
     C*
     C                   move      Xtogrp        Xtogrpnm
     C                   move      *blanks       xerror
     C     aaky          chain(N)  paa01                              60
     C                   if        *in60 = *off
     C                   move      *blanks       xerror
     C                   else
     C                   select
     C                   when      Xagttyp = 1                                  Primary agent
     C                   if        Xagtrcd = 'N'
     C                   eval      xerror  = prerr+ntvld+Xtogrpnm
     C                   else
     C                   eval      xerror  = prerr+ofrcd+ntvld+Xtogrpnm
     C                   endif
     C                   when      Xagttyp = 2
     C                   if        Xagtrcd = 'N'                                Secondary Agent
     C                   eval      xerror  = scerr + ntvld +Xtogrpnm
     C                   else
     C                   eval      xerror  = scerr+ofrcd+ntvld+Xtogrpnm
     C                   endif
     C                   when      Xagttyp = 3
     C                   if        Xagtrcd = 'N'                                Tertiary Agent
     C                   eval      xerror  = trerr+ntvld+Xtogrpnm
     C                   else
     C                   eval      xerror  = trerr+ofrcd+ntvld+Xtogrpnm
     C                   endif
     C                   endsl
     C                   endif
     C                   return    xerror
     pprValidAgent     e
     *====================================================================
      * prValidRate
     *====================================================================
     pprValidRate      b
     dprValidRate      pi            60
     dxtogrp                               like(emco#)                          To Group
     dxrtnbr                               like(ARRT#)                          Rate
     dxagttyp                         1  0                                      Agent type
     dxagtrcd                         1a                                        Agent of record
     dxtogrpnm         s              3a                                        To Group Char
     d*
     d* Error messages
     d*
     d ntvld           c                   const(' rate is not valid for group +
     d                                           ')
     d*
     C***********************************
     C* klist
     C***********************************
     C* for file WMARL
     C*
     c     arky          klist
     c                   kfld                    scCO#
     c                   kfld                    Xtogrp
     c                   kfld                    Xrtnbr
     C*
     C                   move      Xtogrp        Xtogrpnm
     C                   move      *blanks       xerror
     C     arky          chain(N)  par01                              60
     C                   if        *in60 = *off
     C                   move      *blanks       xerror
     C                   else
     C                   select
     C                   when      Xagttyp = 1                                  Primary agent
     C                   if        Xagtrcd = 'N'
     C*                  eval      xerror  = prerr+ntvld+Xtogrpnm+grper
     C                   eval      xerror  = prerr+ntvld+Xtogrpnm
     C                   else
     C                   eval      xerror  = prerr+ofrcd+ntvld+Xtogrpnm
     C                   endif
     C                   when      Xagttyp = 2
     C                   if        Xagtrcd = 'N'                                Secondary Agent
     C                   eval      xerror  = scerr+ntvld+Xtogrpnm
     C                   else
     C                   eval      xerror  = scerr+ofrcd+ntvld+Xtogrpnm
     C                   endif
     C                   when      Xagttyp = 3
     C                   if        Xagtrcd = 'N'                                Tertiary Agent
     C                   eval      xerror  = trerr+ntvld+Xtogrpnm
     C                   else
     C                   eval      xerror  = trerr+ofrcd+ntvld+Xtogrpnm
     C                   endif
     C                   endsl
     C                   endif
     C                   return    xerror
     pprValidRate      e
