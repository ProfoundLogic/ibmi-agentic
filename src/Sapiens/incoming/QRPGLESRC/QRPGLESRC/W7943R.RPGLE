      * ============================================================= *
     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
      * ============================================================= *
     h/title W7943R - Calculate & Print Renewal Tracking Reports
      * ============================================================= *
      * PROGRAM DESCRIPTION:                                          *
      *                                                               *
      * This program calculates and prints three Renewal Tracking     *
      * Reports.                                                      *
      *                                                               *
      * CALLED FROM:                                                  *
      *   W7943C                                                      *
      * CALLS TO:                                                     *
      *                                                               *
      *                                                               *
      * CREATED: 08/21/03     C: 132    PROJECT: 129WAM               *
      *                                                               *
      * ============================================================= *
      * REVISIONS:                                                    *
     H* 04/07/21 C:180 Project: WC1986                                *
     H*                Add GetEftStatus to replace old methods        *
     H*                                                               *
     H* 01/10/18 C:33  Support 63341                                  *
     H*                Clear agt# before WA002R call                  *
      *                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
      * 10/22/10 C:255 Support: SEQ73572                              *
      *                Checked object WA073R in library list instead  *
      *                Call Stack.                                    *
      *                                                               *
      * 07/27/10 C:257 Support: SEQ73572                              *
      *                If API WA073R exist in call stack then use     *
      *                file WMEFL6 instead of WMDBL3                  *
      *                                                               *
      * 01/28/10  C:249  Project: SY215                               *
      *           C:189  Retrieve brand Name                          *
      *                                                               *
      * 12/12/06  C:33  Seq#: WA129/56214                             *
      *                 Make sure employers are listed with the       *
      *                  correct group.                               *
      *                                                               *
      * 03/03/06  C:132 PROJECT: WA129 - 51168                        *
      *                 Printing cancel Employer in next group.       *
      *                                                               *
      * 03/16/05  C:53 PROJECT: 129WJW/WC1277
      *                MOVE TO BASE.
      *                                                               *
      * 02/03/05  C:132 Project: WA129  45630 (bill to 129WAM)        *
      *               - Bypass the processing for Group 001 requested *
      *                 by Cathy Geoffory.                            *
      *                                                               *
      * 01/14/05  C:132 Project: WA129  45272                         *
      *               - Save name of state reporting division for     *
      *                 printing on report, it may not be the zero    *
      *                 division. Check for other divisions being     *
      *               - Check for other divisions being cancelled &   *
      *                 if have premium, ignore it.                   *
      *                                                               *
      *                                                               *
      * 04/15/04  C:132 Project: WA129  42164                         *
      *                 Add additional codes to determine if had a    *
      *                 response on ACH (Easy Pay) employers.         *
      *                                                               *
      * 01/13/04  C:136 Project: 129WEJ                               *
      *                 Replace ELVDR# with information from new      *
      *                 vendor history file (WDVHP)                   *
      *                 Update check for ACH to look at effective     *
      *                 and expiration dates in WMDBP.                *
      *                                                               *
      * 01/09/04  C:132   Project WA129  SEQ: 40747                   *
      *               - Chg cancelled accounts to list if effect date *
      *                 is prior to begin date, and to use estimated  *
      *                 premium on these cancelled accounts           *
      *               - Change WDELL4 logical to WDELL3               *
      *                                                               *
      * 12/29/03  C:132   Project WA129  SEQ: 40559                   *
      *                 Chg total used in calculation for Grand Total *
      *                 page total response premium percentage.       *
      *                                                               *
      * 12/22/03  C:132   Project WA129  SEQ: 40471                   *
      *                 Chg totals for all groups on the Renewal      *
      *                 Status Report (QSYSPRT2) Also change Cancel   *
      *                 compare to use Run Date instead of end period *
      *                                                               *
      * 12/10/03  C:132   Project WA129  SEQ: 40302                   *
      *                 ADD totals for all groups to the Renewal      *
      *                 Status Report (QSYSPRT2)                      *
      *                 - Change to accumulate response/non-response  *
      *                   to reflect correct payment accounts-Response*
      *                                                               *
      * 12/09/03  C:132   Project WA129  SEQ: 40218                   *
      *                 Chg to start with group number one, and add   *
      *                 company number to output file                 *
      *                                                               *
      *****************************************************************
      *
      *  Fund master file
     FWMFNL     IF   E           K DISK
      *
      *  Employer Detail by descending Policy expiration date
      ***** BEG CHG **** 01/09/04 *********************************
     F**********WDELL4    IF   E           K DISK
     FWDELL3    IF   E           K DISK
      ***** END CHG **** 01/09/04 *********************************
      *
      *  Cancllation Detail File - co#,fnd,emp#,div,fyr, seq# (Desc) cancel date
     FWDCXL1    IF   E           K DISK
      *
      *  Employer A/R file
     FWMEAL     IF   E           K DISK
      *
      *  Employer Master file
     FWMEML     IF   E           K DISK
      *
      *  Direct Debit ACH file
      *** beg chg ***** 01/13/04 **********************************
     F**********WMDBL     IF   E           K DISK
     FWMDBL3    IF   E           K DISK
     FWDVHL1    IF   E           K DISK
      *** end chg ***** 01/13/04 **********************************
     F***** BEG ADD ******* 07/27/10 **********************************
     Fwmefl6    if   e           k disk
     F***** END ADD ******* 07/27/10 **********************************
      *
      *  User profile file
     FWMUPL     IF   E           K Disk
      *
      *  Cancellation Reason File - co#,state,Reason code
     FWTCRL     IF   E           K DISK
      *
      *  System Company File
     FSMCOL     IF   E           K DISK
      *
      *  System Routing File
     FSMORL     IF   E           K DISK
      *
      *  System File
     FSSYLF     IF   E           K DISK
      *
      *  Output Workfile
     FWXTRP     if a E             DISK    USROPN
      *** BEG ADD *** 03/16/05 ****************************************
     Fwxgsp     if   e             disk
      *** end ADD *** 03/16/05 ****************************************
      *
      * Renewal Detail Status Report
     FQSYSPRT   O    F  132        Printer OFLIND(*INOB)
     F                                     USROPN
     F                                     PRTCTL(Prtfile)
      *
      * Cancelled/Non-Renewed Report w/Routing Page
     FQSYSPRT1  O    F  196        Printer OFLIND(*INOC)
     F                                     USROPN
      *
      * Renewal Status Report - (As of Cash Receipts)
     FQSYSPRT2  O    F  132        Printer OFLIND(*INOD)
     F                                     USROPN
      *
     D Prtfile         DS
     D  Q4spb                         3
     D  Q4spa                         3
     D  Q4skb                         3
     D  Q4ska                         3
     D  Q4Lin#                        3  0
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D                 DS
     D Effdates                1     35  0 Dim(5)
     D Emefd1                  1      7  0
     D Emefd2                  8     14  0
     D Emefd3                 15     21  0
     D Emefd4                 22     28  0
     D Emefd5                 29     35  0
      *
     D                 DS
     D Cxdates                 1     35  0 Dim(5)
     D Emcad1                  1      7  0
     D Emcad2                  8     14  0
     D Emcad3                 15     21  0
     D Emcad4                 22     28  0
     D Emcad5                 29     35  0
      *
     D Fids01        E DS                  extname(szq2p)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     D Fund            S              3  0
     D #1outq          S             10
     D #1prds          S             40    INZ
     D #1co#           S              3    inz
     D #1ptdt          S              8    inz
     D #1pfdt          S              8    inz
     D Srdfld          S             80
     D Srdlen          S              3  0
     D Srdti           S              8  0
     D Srdto           S             10
     D Srdate          S             10
     D Srerr           S              1
     D Srffmt          S              8
     D Srtfmt          S              8
      *
     d opuser          S                   like(Q1user)
     d svemp#          s                   like(ememp#)
     d svfnd           s                   like(trfnd)
     d totlprm         s                   like(trprem)
     d xxco#           S                   like(COCO#)
     d xxconm          s                   like(coname)
     d xxfnd           s                   like(trfnd)
      ***** BEG ADD ***** 03/03/06 ****************************************
     d xxemp#          s                   like(ememp#)
      ***** END ADD ***** 03/03/06 ****************************************
     d xxanam          s             36                                         Agent Name
     d xename          s             36                                         Emp Name
     d xefdnm          s             37                                         Emp Fed Name
     d xxgnam          s             40                                         Group Name
     d svgnam          s             40                                         Group Name
     d xxfnnm          s             60
     d xxfdnm          s             60
      ***** BEG CHG ***** 12/09/03 ****************************************
     d xxTITL          s             60
      ***** END CHG ***** 12/09/03 ****************************************
     d xxkey           s             10
     d CanDesc         s             45
     d dtname          s             35
     d dtfdnm          s             33
     d xxresp          s              8
     d xxmdcy          s              8  0
     d xxsyk           s              2
     d achcnt          s              7  0
     d actcnt          s              7  0
     d bilcnt          s              7  0
     d cancnt          s              7  0
     d dircnt          s              7  0
     d fndcnt          s              7  0
     d vndcnt          s              7  0
     d nrscnt          s              7  0
     d rspcnt          s              7  0
     d I               s              3  0 inz
      *
     d achpct          s              7  4
     d bilpct          s              7  4
     d vndpct          s              7  4
     d fndpct          s              7  4
     d nrspct          s              7  4
     d rsppct          s              7  4
     d achpctr         s              5  2
     d bilpctr         s              5  2
     d vndpctr         s              5  2
     d fndpctr         s              5  2
     d nrspctr         s              5  2
     d rsppctr         s              5  2
      *
     d achprpct        s              7  4
     d bilprpct        s              7  4
     d vndprpct        s              7  4
     d nrsprpct        s              7  4
     d rspprpct        s              7  4
     d achprpcr        s              5  2
     d bilprpcr        s              5  2
     d vndprpcr        s              5  2
     d nrsprpcr        s              5  2
     d rspprpcr        s              5  2
      *
     d ResponB         s              7  0
     d NonResp         s              7  0
     d ResponBT        s             13  2
     d NonRespT        s             13  2
      *
      ***** BEG ADD ***** 12/10/03 ****************************************
      * VARIBLES FOR GRAND TOTALS
     d actcntG         s              7  0
     d actcntY         s              7  0
     d achcntL         s              7  0
     d actcntL         s              7  0
     d bilcntL         s              7  0
     d cancntL         s              7  0
     d vndcntL         s              7  0
     d nrscntL         s              7  0
     d rspcntL         s              7  0
     d achpctL         s              7  4
     d bilpctL         s              7  4
     d vndpctL         s              7  4
     d nrspctL         s              7  4
     d rsppctL         s              7  4
     d achpctrL        s              5  2
     d bilpctrL        s              5  2
     d vndpctrL        s              5  2
     d nrspctrL        s              5  2
     d rsppctrL        s              5  2
      *
     d achprpctL       s              7  4
     d bilprpctL       s              7  4
     d vndprpctL       s              7  4
     d nrsprpctL       s              7  4
     d rspprpctL       s              7  4
     d achprpcrL       s              5  2
     d bilprpcrL       s              5  2
     d vndprpcrL       s              5  2
     d nrsprpcrL       s              5  2
     d rspprpcrL       s              5  2
     d actprmL         s             13  2
     d achpremL        s             13  2
     d bilpremL        s             13  2
     d vndpremL        s             13  2
     d canprmL         s             13  2
     d RspPrmL         s             13  2
     d NrsPrmL         s             13  2
     d actprmG         s             13  2
     d actprmY         s             13  2
     d actprmT         s             13  2
     d achpremT        s             13  2
     d bilpremT        s             13  2
     d vndpremT        s             13  2
      ***** END ADD ***** 12/10/03 ****************************************
      *
     d achflg          s              1N
     d vndrfl          s              1N
     d achemp          s              1N
     d canemp          s              1N
     d vdremp          s              1N
     d active          s              1N
     d cancel          s              1N
     d flatcan         s              1N
      *
     d achprem         s             13  2
     d bilprem         s             13  2
     d canprem         s             13  2
     d empprem         s             13  2
     d fndprem         s             13  2
     d vndprem         s             13  2
     d actprm          s             13  2
     d canprm          s             13  2
     d RspPrm          s             13  2
     d NrsPrm          s             13  2
      *
     D DATUSA          S               D   DATFMT(*USA)
     D Frdate          S               D   DATFMT(*USA)
     D Today           S               D   DATFMT(*USA)
     D Todate          S               D   DATFMT(*USA)
     D Todate1         S               D   DATFMT(*USA)
     D Fromdt          S              7  0
     D Todt            S              7  0
     D Todt1           S              7  0
     D DateChk         S              1N
     D*
     D DATYMD          S               D   DATFMT(*USA)
     D CancDt          S              7  0
     D HldCndt         S             10
      *
      ***** BEG ADD ***** 12/22/03 ****************************************
     D CurrDt          S              7  0
     d cerprm          s             11  2
     d Dirprm          s             11  2
     d Easprm          s             11  2
     d Payprm          s             11  2
     d cerprmT         s             13  2
     d DirprmT         s             13  2
     d EasprmT         s             13  2
     d PayprmT         s             13  2
      ***** END ADD ***** 12/22/03 ****************************************
      *
      ***** BEG ADD ***** 12/10/03 ****************************************
     d****** xxccyy          s              4  0
     d xxccyy          s              4
     d  ##date         S             10    INZ
      ***** END ADD ***** 12/10/03 ****************************************
     D***  Begin Add  ***  01/28/10  **************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz

     d*** BEG ADD *** 04/07/21 ****************************************
     d strCo#          S              3a
     d strEftType      S              1a
     d strFnd          S              3a
     d strEftKey       S             30a
     d strDate         S              7a
     d strPre          S              1a   inz
     d strRrn          S              9a   inz
     d strEobTyp       S              3    inz
     d strCkTp         S             10    inz
     d strFyr          S              3    inz
     d strEnv          S              1    inz
     d*** END ADD *** 04/07/21 ****************************************

     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************

     d*** BEG ADD *** 04/07/21 ****************************************
     d GetEFTStatus    Pr             1n
     d inCo#                          3
     d inType                         1
     d inFnd                          3
     d inKey                         30
     d inDate                         7
     d inPre                          1
     d inRrn                          9    options(*nopass)
     d inEobType                      3    options(*nopass)
     d inCkTp                        10    options(*nopass)
     d inFyr                          3    options(*nopass)
     d inEnv                          1    options(*nopass)
     d*** END ADD *** 04/07/21 ****************************************
      *
     d/eject
      * ================================================================== *
      * Mainline of program - MAINLINE                                     *
      * ================================================================== *
      *
      *
      *** BEG DEL *** 03/16/05 ****************************************
      ***** BEG CHG ***** 12/09/03 ****************************************
     c*************      eval      xxfnd  = 777
      ***** BEG CHG ***** 02/03/05 ****************************************
     C*********          eval      xxfnd  = 001
     C****               eval      xxfnd  = 002                                 Bypass grp 001
      ***** BEG CHG ***** 02/03/05 ****************************************
      ***** END CHG ***** 12/09/03 ****************************************
     C**** fnky1         setll     PFN01
      ****
     C****               read      PFN01                                  50
     C****               dow       *in50 = *OFF
      *** END DEL *** 03/16/05 ****************************************
      *** BEG ADD *** 03/16/05 ****************************************
     C     1             chain     pgs01                              99
     C                   dow       *in99 = *OFF
      *
     C                   if        gsiefl <> 'I'                                'I'=include
     C                   goto      nextgs
     C                   endif
     C*
     C                   eval      xxfnd  = gsfnd                               Bypass grp 001
     C     fnky1         chain     PFN01                              50
     C     *in50         cabeq     *on           nextgs
      *** END ADD *** 03/16/05 ****************************************
      *
     C                   if        fntest = 'Y'
      *** BEG CHG *** 03/16/05 ****************************************
     C*******            goto      nxtgrp
     C                   goto      nextgs
      *** END CHG *** 03/16/05 ****************************************
     C                   endif
      *
     C                   Movel(p)  fnname        xxgnam
      *
     C                   Movel(p)  fnname        Srdfld
     C                   Call      'SRCENTER'
     C                   Parm                    Srdfld
     C                   Parm      60            Srdlen
     C                   Movel     Srdfld        XXfnnm
      *
     C                   Movel(p)  fnanam        Srdfld
     C                   Call      'SRCENTER'
     C                   Parm                    Srdfld
     C                   Parm      60            Srdlen
     C                   Movel     Srdfld        XXfdnm
      *
     c                   eval      xxfnd  = fnfnd
     c                   eval      trfnd  = fnfnd
      *
      *  Process the employer detail records for this group
      *
     c                   exsr      detlsr
      *
      *** BEG DEL *** 03/16/05 ****************************************
     c**** nxtgrp        tag
      ****
     c**** fnky          reade     PFN01                                  50
      ****
     c****               enddo
      *** END DEL *** 03/16/05 ****************************************
      *** BEG ADD *** 03/16/05 ****************************************
     c     nextgs        tag
     C                   read      pgs01                                  99
     c                   enddo
      *** END ADD *** 03/16/05 ****************************************
      *
      * Now close file then, reprocess it for the other reports
      *
     c                   close     WXTRP
      *
     c                   exsr      WRITERPT
      *
      * ================================================================== *
      *  TERM - TERMINATE THE PROGRAM
      * ================================================================== *
      *
     c     TERM          TAG
      *
     c                   Move      *On           *Inlr
      *
      * ================================================================== *
      *   DETLSR - Accumulate the premiums from employers in Group         *
      * ================================================================== *
      *
     C     Detlsr        Begsr
      *
     C                   eval      svemp# = *zeros
     C     elky          setll     pel01
     C                   read      pel01                                  51
      *
     C     elfnd         cabgt     fnfnd         detlend
      *
     C                   dow       NOT *IN51
      *
      ***** BEG CHG **** 01/09/04 *********************************
      * If not eof, check for same group #, and if not get out
     C                   if        elfnd  <> fnfnd
     C                   eval      *in51 = *on
     C                   leave
     C                   endif
      ***** END CHG **** 01/09/04 *********************************
     C                   if        *in51
     C                   leave
     C                   endif
      *
     C***  Begin Chg  ***  12/12/06  **************************************
      ***If*employer break, and have prem amount, write workfile record, the reset for next emp#
     C**********         if        elemp# <> svemp#
     C**********         if        totlprm <> *zeros
      **********
     C**********         z-add     totlprm       trprem
      **********
      ******BEG*ADD ***** 12/09/03 ****************************************
     C**********         eval      trco#  = xxco#
      ******END*ADD ***** 12/09/03 ****************************************
      **********
      **If*active write record, else print cancel report
     C**********         if        active
     C**********         eval      actcnt = actcnt + 1
      **********
     C**********         select
     C**********         when      trtype = 'A'
     C**********         eval      achcnt = achcnt + 1
     C**********         eval      achprem = achprem + totlprm
     C**********         when      trtype = 'P'
     C**********         eval      vndcnt = vndcnt + 1
     C**********         eval      vndprem = vndprem + totlprm
     C**********         when      trtype = 'B'
     C**********         eval      bilcnt = bilcnt + 1
     C**********         eval      bilprem = bilprem + totlprm
     C**********         endsl
      **********
     C**********         exsr      writesr
      **********
     C**********         else
     C**********         add       totlprm       canprem
     C**********         eval      cancnt = cancnt + 1
     C**********         exsr      writeca
     C**********         endif                                                  active/cancel prt
      **********
     C**********         clear                   totlprm
      **********
      **********
     C**********         endif                                                  Totlprm > zero
      **********
     C**********         exsr      emprsr
     C**********         eval      svemp# = elemp#
     C**********         endif                                                  elemp# <> svemp#
     C                   exsr      EmplBrk
     C***  End   Chg  ***  12/12/06  **************************************
      *
     C                   if        elpfdt >= fromdt
     C                             AND  elpfdt <= todt
      *
      *  Accumulate the Premium for this Empployer
      *
      ****** BEG CHG ***** 12/10/03 *************************************
     C*************      if        elfaf  = 'F'
      *
      ***** BEG CHG **** 01/09/04 *********************************
     C*************      if        elfaf  = 'A'
      *
     C                   if        elfaf  = 'A' AND Cancel = *Off
      ***** END CHG **** 01/09/04 *********************************
      *
      ****** END CHG ***** 12/10/03 *************************************
     C                   eval      totlprm = totlprm + elaapr
     C                   eval      empprem  = elaapr
     C                   else
     C                   eval      totlprm = totlprm + elanpr
     C                   eval      empprem  = elanpr
     C                   endif
      *
     C                   endif
      *
     C                   read      pel01                                  51
      *
     C                   endDO                                                  END *IN51
     C*
     C***  Begin Add  ***  12/12/06  **************************************
     C                   exsr      EmplBrk
     C***  End   Add  ***  12/12/06  **************************************
      *
     C                   if        fndprem <> *zeros
     C                   eval      fndcnt  = fndcnt + 1
     C                   eval      *inod  = *on
     C                   endif
      *
     C                   if        canprem <> *zeros
     C***  Begin Del  ***  12/12/06  **************************************
      ******BEG*ADD ***** 03/03/06 ****************************************
     C**********         if        svemp# <> xxemp# AND Cancel
     C**********         add       totlprm       canprem
     C**********         eval      cancnt = cancnt + 1
     C**********         exsr      writeca
     C**********         clear                   totlprm
     C**********         endif
      ******END*ADD ***** 03/03/06 ****************************************
     C***  End   Del  ***  12/12/06  **************************************
     C                   except    totl1
     C                   eval      *inoc  = *on
     C                   endif
     C
     C     DETLEND       Endsr                                                  DETLSR
     C/EJECT
     C***  Begin Add  ***  12/12/06  **************************************
     C*====================================================================
     C* Process employer break
     C*====================================================================
     C     EmplBrk       begsr
     C*
     C*  If employer break, and have prem amount, write workfile record, the reset for next emp#
     C                   if        elemp# <> svemp#
     C                   if        totlprm <> *zeros
     C*
     C                   z-add     totlprm       trprem
     C*
     C                   eval      trco#  = xxco#
     C*
     C* If active write record, else print cancel report
     C                   if        active
     C                   eval      actcnt = actcnt + 1
     C*
     C                   select
     C                   when      trtype = 'A'
     C                   eval      achcnt = achcnt + 1
     C                   eval      achprem = achprem + totlprm
     C                   when      trtype = 'P'
     C                   eval      vndcnt = vndcnt + 1
     C                   eval      vndprem = vndprem + totlprm
     C                   when      trtype = 'B'
     C                   eval      bilcnt = bilcnt + 1
     C                   eval      bilprem = bilprem + totlprm
     C                   endsl
     C*
     C                   exsr      writesr
     C*
     C                   else
     C                   add       totlprm       canprem
     C                   eval      cancnt = cancnt + 1
     C                   exsr      writeca
     C                   endif                                                  active/cancel prt
     C*
     C                   clear                   totlprm
     C*
     C                   endif                                                  Totlprm > zero
     C*
     C                   exsr      emprsr
     C                   eval      svemp# = elemp#
     C*
     C                   endif                                                  elemp# <> svemp#
     C*
     C                   endsr                                                  DETLSR
     C***  End   Add  ***  12/12/06  **************************************
     C*
      * ================================================================ *
      *   EMPRSR - Get Employer info - cancel, ACH from State reporting  *
      * ================================================================ *
      *
     C     EMPRSR        Begsr
      *
      *  Get the state reporting division and check for ACH
      *
     C                   move      *off          achemp
     C                   move      *off          canemp
     C                   move      *off          vdremp
      *
     C     emky          chain     pem01                              52
      *
     C                   dow       not *in52
     C                   if        emstrp = 'Y'
      *
     C                   if        achflg = *on
      *** Beg Add *** 04/07/21 ***************************************
       strCo# = %Editc(EMco#:'X');
       strEftType = 'E';
       strFnd = %Editc(EMfnd:'X');
       strEftKey = %Editc(EMemp#:'X') + %Editc(EMdiv:'X');
       strDate =%Editc(CurrDt:'X');
       strFyr = %Editc(ELfyr:'X');                                                 //Implied from EM
       achemp = GetEftStatus(strCo#:strEftType:strfnd:strEftKey:
                           strDate:strPre:strRrn:strEobTyp:strCkTp:strFyr);
      *** End Add *** 04/07/21 ***************************************
      *** beg chg ***** 01/13/04 *************************************
     C*****dbky******    chain     pdb01                              53
     C***************    if        NOT *IN53
     C*** BEG CHG ***** 07/27/10 ************************************
     C*
     C* check if new ACH module is installed ('WC1806')
     C*
     C**** Beg   Chg **** 10/22/10 *********************************************
     C*******            if        xxnach = *on
      *** Begin Delete *** 04/07/21 ********************************************
     C**                 if        xxnach = *off
     C**** End   Chg **** 10/22/10 *********************************************
     C**   dbky          chain     pef01                              62
     C**   *in62         doweq     *off
     C**   currDt        iflt      efexdt
     C**   currDt        andge     efefdt
     C**   efexdt        oreq      *zero
     C**   currDt        andge     efefdt
     C**
     C***                   move      *on           achemp
     C**                    leave
     C**                    endif
     C**      dbky          reade     pef01                                       62
     C**                    enddo
     C**                    else
     C** *** END CHG ***** 07/27/10 ************************************
     C**      dbky1         setgt     pdb01
     C**      dbky          readpe    pdb01
     C**                    if        not %eof and dbexdt = 0 or
     C**                              not %eof and currDt < dbexdt
      ** *** end chg ***** 01/13/04 *************************************
     C**                    move      *on           achemp
     C**                    endif                                                  end NOT 53
     C** *** BEG CHG ***** 07/27/10 ************************************
     C**                 endif
     C*** END CHG ***** 07/27/10 ************************************
      *** End Delete *** 04/07/21 ********************************************
     C                   endif                                                  end achflg = on
      *
      * now check for cancelled
      *
     C                   Eval      HldCndt = *Blanks
     C                   Eval      Active  = *Off
     C                   eval      Flatcan = *Off
      ***** BEG ADD **** 01/09/04 *********************************
     C                   Eval      Cancel  = *Off
      ***** END ADD **** 01/09/04 *********************************
      *
     C                   Eval      i = 1
      *
     C                   If        Effdates(I) = *zero
     C                   Eval      Active = *On
     C                   Eval      HldCndt = *Blanks
     C                   Else
      *
      * Get the most recent cancel date.
      *
     C     *Zero         Lookup    Effdates(I)                            58
      *
     C                   If        *in58 = *on
     C                   Eval      I = I - 1
     C                   Else
     C                   Eval      I = 5
     C                   Endif
      *
      * At this point, we have the most current Cancel Date
      *
     C                   If        Cxdates(I) > *Zero
     C     *Ymd          Move      Cxdates(I)    Datusa
     C     *Ymd          Move      Cxdates(I)    Datymd
      *
     C                   If        Cxdates(I) >= fromdt and
     C                             Cxdates(I) <= Todt
      *
      ***** BEG ADD **** 01/09/04 *********************************
     C                   If        Effdates(1) <  fromdt
     C                   Eval      Cancel = *On
     C                   Endif                                                  effdate(1) < fromdt
      ***** END ADD **** 01/09/04 *********************************
      *
     C                   eval      cancdt = Cxdates(I)
     C                   Move      Datusa        HldCndt
     C                   If        Cxdates(I) > *Zero  AND
     C                             Cxdates(I) = Effdates(I) AND I = 1
     C                   eval      Flatcan = *on
     C                   Endif
      *
     C                   Else
     C                   Eval      Active = *On
      ***** BEG ADD ***** 01/14/05 **********************************
     C                   Movel(p)  emname        trname
     C                   Movel(p)  emfdnm        trfdnm
      ***** END ADD ***** 01/14/05 **********************************
     C                   Endif
      *
     C                   Else
     C                   Eval      Active = *On
      ***** BEG ADD ***** 01/14/05 **********************************
     C                   Movel(p)  emname        trname
     C                   Movel(p)  emfdnm        trfdnm
      ***** END ADD ***** 01/14/05 **********************************
     C                   Endif
      *
     C                   Endif
      *
     C                   leave
      *
     C                   endif                                                  end emstrp = Y
      ***** BEG CHG ***** 01/14/05 **********************************
     C*****emky*****     reade     pem01                                  52
     C     EMKY2         reade     pem01                                  52
      ***** END CHG ***** 01/14/05 **********************************
      *
     C                   enddo                                                  Enddo Not 52
      *
      *** beg chg ***** 01/13/04 ************************************
     C****************   if        achemp = *off  AND elvdr# > *zeros
     C                   if        achemp = *off
     C     vhky1         setgt     pvh01
     C     vhky          readpe    pvh01
     C                   if        not %eof and vhexdt = 0 or
     C                             not %eof and currDt < vhexdt
      *** end chg ***** 01/13/04 ************************************
     C                   eval      vdremp = *on
     C                   endif
      *
      *** beg add ***** 01/13/04 ************************************
     C                   endif
      *** end add ***** 01/13/04 ************************************
      *
     C                   select
     C                   when      achemp = *on
     C                   eval      trtype = 'A'
     C                   when      vdremp = *on
     C                   eval      trtype = 'P'
     C                   other
     C                   eval      trtype = 'B'
     C                   endsl
      *
     C                   if        active
     C                   eval      tractv = 'Y'
     C                   else
     C                   eval      tractv = 'C'
      ***** BEG ADD ***** 01/14/05 **********************************
     C                   Movel(p)  emname        trname
     C                   Movel(p)  emfdnm        trfdnm
      ***** END ADD ***** 01/14/05 **********************************
     C                   endif
      *
      ***** BEG DLT ***** 01/14/05 **********************************
     C*********          Movel(p)  emname        trname
     C*********          Movel(p)  emfdnm        trfdnm
      ***** BEG DLT ***** 01/14/05 **********************************
      *
      ****** BEG ADD ***** 12/10/03 *************************************
      ********** Include the Policy NUMBER  part of Fed Name Field
     C************       Movel     ELEMP#        XZEMPN            9
     C************       EVAL      TRFDNM = XZEMPN + ' ' + TRFDNM
      ****** END ADD ***** 12/10/03 *************************************
     C                   eval      tremp# = ememp#
      *
     C                   Endsr                                                  EMPRSR
      *
     C/Eject
      * ================================================================== *
      *   WRITECA - Write out the premium amounts                          *
      * ================================================================== *
      *
     C     WRITECA       Begsr
      *
     C                   clear                   CanDesc
      *
     C                   eval      trresp = *blanks
     C*
     C                   write     ptr01
      *
     c                   eval      *in91 = *off
      *
     C                   if        Flatcan
     c                   eval      *in91 = *on
     c                   endif
      *
     C     cxky          setgt     wdcxl1
     C     cxky          readpe    wdcxl1                                 54
     C                   IF        not *in54
     C                   if        cxrst = *blanks
     C                   Select
     C                   when      empst  > *blanks
     C                   movel(p)  empst         cxrst
     C                   other
     C                   movel(p)  emmst         cxrst
     C                   endsl
     C                   endif                                                  End cxrst = *blanks
     C                   movel(P)  cxrsn         xxkey
     C     crky          chain     wtcrl                              60
     C  n60              eval      CanDesc = %TRIMR(CRDESC) + ' ' + hldcndt
     C   60              eval      CanDesc = 'NO CANCEL REASON ' + hldcndt
      *
      *  If Flat Cancel, check prior History record, get reason and append Term verbage
     C                   IF        FLATCAN
     C     cxky          readpe    wdcxl1                                 54
     C                   IF        not *in54
     C                   movel(P)  cxrsn         xxkey
     C     crky          chain     wtcrl                              60
     C                   eval      CanDesc = *blanks
     C  n60              eval      CanDesc = %TRIMR(CRDESC) + ' ' + hldcndt +
     C                             '-Prior Term Cancel'
     C   60              eval      CanDesc = 'FLAT CANCELLED ' + hldcndt  +
     C                             '-Prior Term Cancel'
     C                   ELSE
     C                   eval      CanDesc = %TRIMR(CanDesc) + '-Flat Cancelled'
     C                   endif
      *
     C                   ENDIF                                                  FLATCAN = *ON
      *
     C                   endif
      *
      * Get the agent name for cancel report
      *
     C                   CALL      'WA002R'
     C                   PARM      XXCO#         @@CO#             3 0
     C                   PARM      EMFND         @@FND             3 0
     C                   PARM      EMEMP#        @@EMP#            9 0
     C                   PARM      EMDIV         @@DIV             5 0
     C***   Begin Chg  ***  01/10/18  *************************************
     C***********        PARM                    @@AGT#            5 0
     C***********        PARM                    @@ANAM           60
     C                   PARM      *zeros        @@AGT#            5 0
     C                   PARM      *blanks       @@ANAM           60
     C***   End   Chg  ***  01/10/18  *************************************
     C                   PARM                    @@ANAM           60
     C                   PARM                    @@PROD            5 0
     C                   PARM                    @@PNAM           60
     C                   PARM                    @@AREA            3 0
     C                   PARM                    @@PHON            7 0
     C                   PARM      '*'           @@PLAN            1
     C                   PARM      *ZERO         @@DATE            8 0
      *
     C                   MOVEL(P)  @@ANAM        XXANAM
     C                   movel(p)  trname        xename
     C                   movel(P)  trfdnm        xefdnm
      *
     C   oc              except    head1
     C   oc              eval      *inoc = *off
     C                   except    candetl
      ***** BEG ADD ***** 03/03/06 ****************************************
      * Save the last Cancel Employer number to check at Group Break
     C                   z-add     ememp#        xxemp#
      ***** END ADD ***** 03/03/06 ****************************************
      *
     C                   Endsr                                                  WRITECA
      *
     C/Eject
      * ================================================================== *
      *   WRITESR - Write out the premium amounts                          *
      * ================================================================== *
      *
     C     WRITESR       Begsr
      *
      * Get Response and non-response amounts:
      *
      *     If Direct Billing code = "B", check for Payment Code "P", if found add to totals
      *     If Payroll Vendor code = "P", check for Billing Code "B" or "H", add to totals
      *     If ACH Account code    = "A", check for Billing Code "B", if found add to totals
      *
     C     eaky          setll     pea01
     C     eaky          reade     pea01                                  54
      *
     C                   IF        *IN54
     C                   eval      trresp   = 'N'
     C                   endif
      *
     C                   DOW       not *in54
      *
      ***** BEG ADD ***** 12/10/03 ****************************************
     C**************     if        trtype = 'B' AND eaarty = 'B'
     C                   if        trtype = 'B' AND eaarty = 'P'
      ***** END ADD ***** 12/10/03 ****************************************
     C                   eval      trresp   = 'Y'
     C                   leave
     C                   endif
      *
      ***** BEG ADD ***** 04/15/04 ****************************************
     C                   if        (trtype = 'A' AND eaarty = 'B') OR
     C                             (trtype = 'A' AND eaarty = 'A') OR
     C                             (trtype = 'A' AND eaarty = 'D') OR
     C                             (trtype = 'A' AND eaarty = 'H')
     C                   eval      trresp   = 'Y'
     C                   leave
     C                   endif
      ***** END ADD ***** 04/15/04 ****************************************
      ***** BEG ADD ***** 12/10/03 ****************************************
     C**************     if        trtype = 'A' AND eaarty = 'P' OR
     C**************               trtype = 'P' AND eaarty = 'P'
      ***** BEG CHG ***** 04/15/04 ****************************************
     C**********         if        trtype = 'A' AND eaarty = 'B' OR
     C**********                   trtype = 'P' AND eaarty = 'B' OR
     C                   IF        trtype = 'P' AND eaarty = 'B' OR
      ***** END CHG ***** 04/15/04 ****************************************
     C                             trtype = 'P' AND eaarty = 'D' OR
     C                             trtype = 'P' AND eaarty = 'A' OR
      ***** END CHG ***** 04/15/04 ****************************************
     C                             trtype = 'P' AND eaarty = 'H'
      ***** END ADD ***** 12/10/03 ****************************************
     C                   eval      trresp   = 'Y'
     C                   leave
     C                   endif
     C     eaky          reade     pea01                                  54
      *
     C                   IF        *IN54
     C                   eval      trresp   = 'N'
     C                   eval      NonResp  = NonResp + 1
     C                   eval      NonRespT = NonRespT + Totlprm
     C                   endif
      *
     C                   endDO                                                  Enddo-not *in54
     C*
     C                   write     ptr01
      *
     C                   Endsr                                                  WRITESR
      *
     C/Eject
      * ================================================================== *
      *   WRITERPT - Write out the premium amounts                          *
      * ================================================================== *
      *
     c     WRITERPT      Begsr
      *
      * Get Response and non-response amounts:
      *
     c                   open      WXTRP
     c                   open      Qsysprt
     c                   open      Qsysprt2
      *
     c                   eval      actcnt  = *zeros
     c                   eval      cancnt  = *zeros
     c                   eval      bilcnt  = *zeros
     c                   eval      achcnt  = *zeros
     c                   eval      vndcnt  = *zeros
     c                   eval      rspcnt  = *zeros
     c                   eval      nrscnt  = *zeros
     c                   eval      achprem = *zeros
     c                   eval      vndprem = *zeros
     c                   eval      bilprem = *zeros
     c                   eval      canprm  = *zeros
     c                   eval      svfnd   = *zeros
     c                   eval      xxfnd   = *zeros
     C                   eval      fndcnt  = *zeros
      ***** BEG CHG ***** 12/09/03 ****************************************
     c                   eval      rspprm  = *zeros
     c                   eval      nrsprm  = *zeros
      ***** END CHG ***** 12/09/03 ****************************************
      *
     C     1             chain     wxtrp                              51
      *
     C                   dow       NOT *in51
      *
      *  Get Group name for Heading, print head if 1st time thru
     C                   if        trfnd  <> svfnd
     C                   eval      xxfnd  = trfnd
     C                   eval      fndcnt = fndcnt + 1
     C     fnky1         chain     pfn01                              60
     C                   if        svfnd = *zeros
     C                   movel(p)  fnname        xxgnam
     C                   movel(p)  fnname        svgnam
     C                   eval      svfnd = trfnd
      *
     C                   Movel(p)  fnname        Srdfld
     C                   Call      'SRCENTER'
     C                   Parm                    Srdfld
     C                   Parm      60            Srdlen
     C                   Movel     Srdfld        XXfnnm
     C                   except    head
     C                   endif
     c                   endif                                                  trfnd <> svfnd
      *
      *  When Group changes, Calculate percentages, print total line, Then new heading
     C                   if        trfnd  <> svfnd  AND svfnd <> *zeros
     C                   exsr      percnt
     C                   except    totl
     C                   eval      svfnd = trfnd
     c                   eval      actcnt  = *zeros
     c                   eval      cancnt  = *zeros
     c                   eval      bilcnt  = *zeros
     c                   eval      achcnt  = *zeros
     c                   eval      vndcnt  = *zeros
     c                   eval      rspcnt  = *zeros
     c                   eval      nrscnt  = *zeros
     c                   eval      achprem = *zeros
     c                   eval      vndprem = *zeros
     c                   eval      bilprem = *zeros
     c                   eval      actprm  = *zeros
     c                   eval      canprm  = *zeros
      ***** BEG CHG ***** 12/09/03 ****************************************
     c                   eval      rspprm  = *zeros
     c                   eval      nrsprm  = *zeros
      ***** END CHG ***** 12/09/03 ****************************************
     C                   movel(p)  fnname        xxgnam
     C                   movel(p)  fnname        svgnam
      *
     C                   Movel(p)  fnname        Srdfld
     C                   Call      'SRCENTER'
     C                   Parm                    Srdfld
     C                   Parm      60            Srdlen
     C                   Movel     Srdfld        XXfnnm
      *
     C                   except    head
     C                   endif                                                  trfnd <> svfnd
      *
     c                   eval      *in71  = *OFF
     c                   eval      *in72  = *OFF
     c                   eval      *in73  = *OFF
      *
     C                   if        tractv = 'Y'
      ***** BEG DEL ***** 12/10/03 ****************************************
     C*********          eval      actcnt = actcnt + 1
     C********           eval      actprm = actprm + trprem
      ***** END DEL ***** 12/10/03 ****************************************
      *
     C                   if        trresp = 'Y'
     C                   eval      rspcnt = rspcnt + 1
     C                   eval      xxresp = '    Y   '
     C                   eval      rspprm = rspprm + trprem
      ***** BEG CHG ***** 12/10/03 ****************************************
     C                   eval      actcnt = actcnt + 1
     C                   eval      actprm = actprm + trprem
     c                   select
     c                   when      trtype = 'B'
     c                   eval      bilcnt = bilcnt + 1
     c                   eval      bilprem = bilprem + trprem
     c                   when      trtype = 'A'
     c                   eval      achcnt = achcnt + 1
     c                   eval      achprem = achprem + trprem
     c                   when      trtype = 'P'
     c                   eval      vndcnt = vndcnt + 1
     c                   eval      vndprem = vndprem + trprem
     c                   endsl
      ***** END CHG ***** 12/10/03 ****************************************
      *
     C                   else
     C                   eval      nrscnt = nrscnt + 1
     C                   eval      xxresp = *blanks
     C                   eval      nrsprm = nrsprm + trprem
     C                   endif
      *
     C                   movel(P)  trname        dtname
     C                   movel(P)  trfdnm        dtfdnm
     C
      *
      ***** BEG CHG ***** 12/10/03 ****************************************
      * Only set indicators for print, don't get premium amts here
     c                   select
     c                   when      trtype = 'B'
     c                   eval      *in71  = *ON
     c*********          eval      bilcnt = bilcnt + 1
     c*********          eval      bilprem = bilprem + trprem
     c                   when      trtype = 'A'
     c                   eval      *in72  = *ON
     c*********          eval      achcnt = achcnt + 1
     c*********          eval      achprem = achprem + trprem
     c                   when      trtype = 'P'
     c                   eval      *in73  = *ON
     c*********          eval      vndcnt = vndcnt + 1
     c*********          eval      vndprem = vndprem + trprem
     c                   endsl
      ***** END CHG ***** 12/10/03 ****************************************
      *
      ***** BEG CHG ***** 12/22/03 ****************************************
      * Save Totals for Detail Report
     C                   eval      cerprm  = cerprm  + trprem
     C                   eval      cerprmT = cerprmT + trprem
     C                   select
     C                   when      *in71
     C                   eval      dirprm  = dirprm  + trprem
     C                   eval      dirprmT = dirprmT + trprem
     C                   when      *in72
     C                   eval      easprm  = easprm  + trprem
     C                   eval      easprmT = easprmT + trprem
     C                   when      *in73
     C                   eval      payprm  = payprm  + trprem
     C                   eval      payprmT = payprmT + trprem
     C                   endsl
      ***** END CHG ***** 12/22/03 ****************************************
      *
      * Only print active accounts
     C   ob              except    head
     C                   except    trdetl
      *
     C                   else
      *
     C                   eval      cancnt = cancnt + 1
     C                   eval      canprm = canprm + trprem
     C                   endif
      *
     C                   read      wxtrp                                  51
      *
     C                   enddo
      *
     c                   close     WXTRP
      *
     C                   exsr      percnt
     c   ob              except    head
     c                   except    totl
     c                   except    FndTotl
      *
      ***** BEG CHG ***** 12/10/03 ****************************************
     c*********          except    FndTotl2
      *  Exit to Subroutine to Calculate and print final totals
     C                   exsr      percntLR
      ***** END CHG ***** 12/10/03 ****************************************
      *
     c                   close     Qsysprt
     c                   close     Qsysprt2
      *
     c                   Endsr                                                  WRITERPT
      *
     C/Eject
      * ================================================================ *
      *   PERCNT - Calculate and print Percents for each group
      * ================================================================ *
      *
     c     PERCNT        Begsr
      *
      ***** BEG ADD ***** 12/10/03 ****************************************
      * Add to LR total fields
     C                   eval      actprmT  = actprmT  + actprm
     C                   eval      achpremT = achpremT + achprem
     C                   eval      bilpremT = bilpremT + bilprem
     C                   eval      vndpremT = vndpremT + vndprem
      *
     C                   eval      actcntL  = actcntL  + actcnt
     C                   eval      actprmL  = actprmL  + actprm
     C                   eval      achcntL  = achcntL  + achcnt
     C                   eval      achpremL = achpremL + achprem
     C                   eval      bilcntL  = bilcntL  + bilcnt
     C                   eval      bilpremL = bilpremL + bilprem
     C                   eval      cancntL  = cancntL  + cancnt
     C                   eval      canprmL  = canprmL  + canprm
     C                   eval      vndcntL  = vndcntL  + vndcnt
     C                   eval      vndpremL = vndpremL + vndprem
     C                   eval      rspcntL  = rspcntL  + rspcnt
     C                   eval      rspprmL  = rspprmL  + rspprm
     C                   eval      nrscntL  = nrscntL  + nrscnt
     C                   eval      nrsprmL  = nrsprmL  + nrsprm
     C                   eval      actcntG = rspcnt  + nrscnt
     C                   eval      actprmG = rspprm  + nrsprm
     C                   eval      actcntY = actcntY + actcntG
     C                   eval      actprmY = actprmY + actprmG
      *
      ***** END ADD ***** 12/10/03 ****************************************
      *
      ***** BEG CHG ***** 12/22/03 ****************************************
     c                   if        nrscnt > *zeros OR  actcnt > *zeros
     c                   except    head2
     c                   except    detlg
     c                   endif                                                  End actcnt = 0
      ***** END CHG ***** 12/22/03 ****************************************
      *
     c                   if        actcnt > *zeros
      *
     c                   eval(H)   bilpct   = bilcnt / actcnt
     c                   eval      bilpctr  = bilpct * 100
     c                   eval(H)   bilprpct = bilprem /actprm
     c                   eval      bilprpcr = bilprpct * 100
     c                   except    detlbil
      *
     c                   eval(H)   achpct   = achcnt / actcnt
     c                   eval      achpctr  = achpct * 100
     c                   eval(H)   achprpct = achprem /actprm
     c                   eval      achprpcr = achprpct * 100
     c                   except    detlach
      *
     c                   eval(H)   vndpct   = vndcnt / actcnt
     c                   eval      vndpctr  = vndpct * 100
     c                   eval(H)   vndprpct = vndprem /actprm
     c                   eval      vndprpcr = vndprpct * 100
     c                   except    detlvnd
      *
      ***** BEG CHG ***** 12/22/03 ****************************************
     c                   endif                                                  End actcnt > 0
      *
     c                   if        actcntG > *zeros
      ***** END CHG ***** 12/22/03 ****************************************
     c                   eval(H)   rsppct   = rspcnt / actcntG
     c                   eval      rsppctr  = rsppct * 100
     c                   eval(H)   rspprpct = rspprm /actprmG
     c                   eval      rspprpcr = rspprpct * 100
      *
     c                   eval(H)   nrspct   = nrscnt / actcntG
     c                   eval      nrspctr  = nrspct * 100
     c                   eval(H)   nrsprpct = nrsprm /actprmG
     c                   eval      nrsprpcr = nrsprpct * 100
     c                   except    detlrsp
      *
     c                   endif                                                  End actcntG > 0
      *
     c                   except    detlcan
      *
     c                   Endsr                                                  PERCNT
      *
      ***** BEG ADD ***** 12/10/03 ****************************************
      *
      /Eject
      * ================================================================ *
      *   PERCNTLR - Calculate and print Percents for ALL groups
      * ================================================================ *
      *
     c     PERCNTLR      Begsr
      *
      *  Print total lines and close report
      *
     C                   except    head2L
     C                   except    FndTotl2
     C                   except    detlgL
      *
     C                   if        fndcnt > *zeros
      *
      ***** BEG ADD ***** 12/22/03 ****************************************
     C                   if        actcntL > *zeros
      ***** EMD ADD ***** 12/22/03 ****************************************
      *
     c                   eval(H)   bilpctL   = bilcntL / actcntL
     c                   eval      bilpctrL  = bilpctL * 100
     c                   eval(H)   bilprpctL = bilpremL /actprmL
     c                   eval      bilprpcrL = bilprpctL * 100
     c                   except    detlbilL
      *
     c                   eval(H)   achpctL   = achcntL / actcntL
     c                   eval      achpctrL  = achpctL * 100
     c                   eval(H)   achprpctL = achpremL /actprmL
     c                   eval      achprpcrL = achprpctL * 100
     c                   except    detlachL
      *
     c                   eval(H)   vndpctL   = vndcntL / actcntL
     c                   eval      vndpctrL  = vndpctL * 100
     c                   eval(H)   vndprpctL = vndpremL /actprmL
     c                   eval      vndprpcrL = vndprpctL * 100
     c                   except    detlvndL
      *
      ***** BEG ADD ***** 12/22/03 ****************************************
     C                   endif
      *
     C                   if        actcntY > *zeros
      ***** EMD ADD ***** 12/22/03 ****************************************
      *
     c                   eval(H)   rsppctL   = rspcntL / actcntY
     c                   eval      rsppctrL  = rsppctL * 100
      ***** BEG CHG ***** 12/29/03 ****************************************
     c***************    eval(H)   rspprpctL = rspprmL /actprmL
     c                   eval(H)   rspprpctL = rspprmL /actprmY
      ***** END CHG ***** 12/29/03 ****************************************
     c                   eval      rspprpcrL = rspprpctL * 100
      *
     c                   eval(H)   nrspctL   = nrscntL / actcntY
     c                   eval      nrspctrL  = nrspctL * 100
     c                   eval(H)   nrsprpctL = nrsprmL /actprmY
     c                   eval      nrsprpcrL = nrsprpctL * 100
     c                   except    detlrspL
      *
      ***** BEG ADD ***** 12/22/03 ****************************************
     C                   endif
      ***** EMD ADD ***** 12/22/03 ****************************************
      *
     c                   endif                                                  End actcnt > 0
      *
     c                   except    detlcanL
      *
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       If %Open(qsysprt1);
          Except Route;
       EndIf;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     c                   close     qsysprt1
      *
     c                   Endsr                                                  PERCNT
      *
      ***** END ADD ***** 12/10/03 ****************************************
      *
      /Eject
      * ================================================================ *
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      * ================================================================ *
      *
     C     *Inzsr        Begsr
      *
     C                   Time                    Dsdttm
      *
      *   Key List for COMPANY fiLe
      *
     C     coky          Klist
     C                   Kfld                    Xxco#
      *
      *   Key List for CANCEL REASON file
      *
     C     crky          Klist
     C                   Kfld                    xxco#
     C                   Kfld                    cxrst
     C                   Kfld                    xxkey
      *
      *   Key List for CANCEL Detail file
      *
     C     cxky          Klist
     C                   Kfld                    Emco#
     C                   Kfld                    Emfnd
     C                   Kfld                    Ememp#
     C                   Kfld                    Emdiv
      *
      *   Key List for Employer A/R file
      *
     C     eaky          Klist
     C                   Kfld                    Emco#
     C                   Kfld                    Emfnd
     C                   Kfld                    Ememp#
     C                   Kfld                    Emdiv
     C                   Kfld                    Elfyr
      *
      *   Key List for ACH Direct Debit File
      *
     C     dbky          Klist
     C                   Kfld                    Emco#
     C                   Kfld                    Emfnd
     C                   Kfld                    Ememp#
     C                   Kfld                    Emdiv
      *
      *** beg add ***** 01/13/04 **********************************
     C     dbky1         Klist
     C                   Kfld                    Emco#
     C                   Kfld                    Emfnd
     C                   Kfld                    Ememp#
     C                   Kfld                    Emdiv
     C                   Kfld                    currDt
      *
     C     vhky          Klist
     C                   Kfld                    Emco#
     C                   Kfld                    Emfnd
     C                   Kfld                    Ememp#
     C                   Kfld                    Emdiv
     C                   Kfld                    Elfyr
      *
     C     vhky1         Klist
     C                   Kfld                    Emco#
     C                   Kfld                    Emfnd
     C                   Kfld                    Ememp#
     C                   Kfld                    Emdiv
     C                   Kfld                    Elfyr
     C                   Kfld                    currDt
      *** end add ***** 01/13/04 **********************************
      *
      *   Key List for EMPLOYER DETAIL File
      *
      ***** BEG CHG **** 01/09/04 *********************************
     C*****elky          Klist
     C**********         Kfld                    fnco#
     C**********         Kfld                    fnfnd
     C**********         Kfld                    todt1
     C     elky          Klist
     C                   Kfld                    fnco#
     C                   Kfld                    fnfnd
     C                   Kfld                    fromdt
      ***** END CHG **** 01/09/04 *********************************
      *
      *   Key List for EMPLOYER File
      *
     C     emky          Klist
     C                   Kfld                    Elco#
     C                   Kfld                    Elfnd
     C                   Kfld                    Elemp#
     C                   Kfld                    Eldiv
      *
      ****** BEG ADD ***** 01/14/05 ************************************
     C     emky2         Klist
     C                   Kfld                    Elco#
     C                   Kfld                    Elfnd
     C                   Kfld                    Elemp#
      ****** END ADD ***** 01/14/05 ************************************
      *
      *   Key List for GROUP file
      *
     C     fnky          Klist
     C                   Kfld                    Xxco#
      *
     C     fnky1         Klist
     C                   Kfld                    Xxco#
     C                   Kfld                    Xxfnd
      *
      *
      *   Key List for OVERRIDE File
      *
     C     orky          Klist
     C                   Kfld                    Q1pgm
     C                   Kfld                    Xxprt             1
      *
      *   Key List for the System File
      *
     C     syky          Klist
     C                   Kfld                    xxsyk
      *
      *   Key List for USER PARAMETER fike
      *
     C     upky          Klist
     C                   Kfld                    Q1user
      *
     C     DATCHK        Plist
     C                   Parm                    SRDTI
     C                   Parm      *Blanks       SRFFMT
     C                   Parm      *Blanks       SRTFMT
     C                   Parm                    SRDTO
     C                   Parm                    SRERR
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   Parm      *Blanks       Srtfmt
     C                   Parm                    Srdto
     C                   Parm                    Srerr
      *
      * Check for ACH and Payroll Vendor flags
      *
     c                   movel     'WC'          xxsyk
     c***  syky          chain     SWC01                              60
     c     XXSYK         chain     SWC01                              60
     c                   if        NOT *in60
      *
     c                   eval      vndrfl = *off
     c                   if        wcfl23 = 'Y'
     c                   eval      vndrfl = *on
     c                   endif                                                  end wcfl23
      *
     c                   eval      achflg = *off
     c                   if        wcfl24 = 'Y'
     c                   eval      achflg = *on
     c                   endif
      *
     c                   endif                                                  end Not 60
      *
     C     *Entry        Plist
     C                   Parm                    QQCO#             3
     C                   Parm                    QQfrdt            8
     C                   Parm                    QQtodt            8
     C                   Parm                    QQque            10
      *
     C                   Move      QQCO#         XXCO#
      *
     C                   Time                    DSDTTM
     C     *mdy          Move      Dsdate        Today
      ***** BEG ADD ***** 12/22/03 ****************************************
     C     *Cymd         Move      Today         CurrDt
      ***** END ADD ***** 12/22/03 ****************************************
      *
      ***** BEG DEL ***** 12/10/03 ****************************************
     C**********         move      *year         xxccyy
      ***** END DEL ***** 12/10/03 ****************************************
      *
     C                   Eval      Datechk = *Off
      *
     C                   If        QQfrdt <> *Blanks and
     C                             QQtodt <> *Blanks
     C     *Usa0         Move      QQfrdt        Frdate
     C     *Cymd         Move      Frdate        Fromdt
      ***** BEG ADD ***** 12/10/03 ****************************************
     C                   move      *ZEROS        xxccyy
     C     *USA          Move      Frdate        ##date
     C                   Move      ##date        xxccyy
      ***** END ADD ***** 12/10/03 ****************************************
     C     *Usa0         Move      QQtodt        Todate
     C     *Cymd         Move      Todate        Todt
     c     Frdate        adddur    1:*D          Todate1
     C     *Cymd         Move      Todate1       Todt1
     C
     C                   Eval      Datechk = *On
     C                   Endif
      *
      *  Get the Company Record
      *
     C     Coky          Chain     Pco01                              60
     C                   Movel(p)  Coname        Srdfld
     C                   Call      'SRCENTER'
     C                   Parm                    Srdfld
     C                   Parm      50            Srdlen
     C                   Movel     Srdfld        XXCONM
     c*** Beg Add ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     c*** End add ***** 01/28/10 **************************************
      *
     C     upky          Chain     Wmupl                              60
      *
      *  Get the Fund Record
      *
     C                   Move      Q1user        opuser
      *
      ***** BEG ADD ***** 12/09/03 ****************************************
     C                   EVAL      XXPRT = '1'
      ***** END ADD ***** 12/09/03 ****************************************
      *
     C                   Open      Qsysprt1
     C                   Open      WXTRP
      *
     C     orky          Chain     Smorl                              60
      *
     C                   IF        *IN60 = *Off
     C                   eval      *in08 = *ON                                  Have routing rcd
     C                   eval      xxtitl = %TRIML(ortitl)
     C                   ELSE
     C                   eval      *in08 = *off
     C                   eval      *in09 = *ON                                  No routing record
     C                   Endif
      *
     C                   Move      *On           *INOC
     C                   Move      *On           *INOD
      *
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         Except    Route
      ***** End Del ***** 08/26/14 ************************************
     C*** BEG ADD ***** 07/27/10 ************************************
     C                   movel     'WA073R'      xxpgm            10
     C*** Beg Chg ***** 10/22/10 ************************************
     C*******            call      'SRCHKPGM'
     C*******            parm                    xxpgm
     C*******            parm                    xxnach            1
     C*
     C                   MOVEL     '*PGM'        XXTYP             7
     C                   CALL      'SRCHKOBJ'
     C                   PARM                    XXPGM
     C                   PARM                    XXTYP
     C                   PARM                    XXNACH            1
     C*** End Chg ***** 10/22/10 ************************************
    C*
     C*** END ADD ***** 07/27/10 ************************************
      *
     C                   Endsr
      *
      * ================================================================ *
      /eject
      * ================================================================ *
     * QYSPRT - Renewal Detail Status Report
      * ================================================================ *
     *
     OQsysprt   E            Head           1 06
     O                       Dstime                 '  :  :  '
     O                       xxconm              82
     O                       Dsdate        Y    132
     *
     O          E            Head           2
     O                       Q1pgm
     O                       xxccyy              58
     O                                        +   1 'Renewal Detail Status'
     O                                          128 'PAGE:'
     O                       Page          Z    132
     *
     * Group Name
     O          E            Head           1
     O                       xxfnd         3      4
     O                       xxgnam              45
     *
     O          E            Head           2
     O                       xxccyy               4
     O                                        +   1 'Policy Year'
     *
     O          E            Head           2
     O                                            4 'Name'
     O                                           81 'CertFee'
     O                                           95 'Direct'
     O                                          109 'Easy Pay'
     O                                          122 'Payroll'
     O                                          132 'Response'
     *
     O          E            TrDetl         1
     O                       dtname              35
     O                       dtfdnm              69
      **** 12/10/03 ****** chg edit code 3 to edit code 1
     O                       trprem        1     81
     O                  71   trprem        1     95
     O                  72   trprem        1    109
     O                  73   trprem        1    123
     O                       xxresp             132
     *
     * Group Totals
     O          E            Totl        1  1
     O                       svfnd                4
     O                                        +   1 'Group Total Premiums'
      ***** BEG CHG ***** 12/22/03 *******************************************
     O**********             actprm        4b    81
     O**********             bilprem       4b    95
     O**********             achprem       4b   109
     O**********             vndprem       4b   123
     O                       cerprm        1b    81
     O                       dirprm        1b    95
     O                       easprm        1b   109
     O                       payprm        1b   123
      ***** END CHG ***** 12/22/03 *******************************************
     *
     * Group Totals
     O          E            Totl           1
     O                       svfnd                4
     O                                           20 'Group Counts - '
     O                                           33 'Directs  . :'
     O                       bilcnt        3  +   1
     O          E            Totl           1
     O                                           33 'Easy Pay . :'
     O                       achcnt        3  +   1
     O          E            Totl           1
     O                                           33 'Payroll  . :'
     O                       vndcnt        3  +   1
     O          E            Totl           1
     O                                           33 'Cancelled  :'
     O                       cancnt        3  +   1
     O          E            Totl           1
     O                                           33 'Response . :'
     O                       rspcnt        3  +   1
     O          E            Totl           1
     O                                           33 'No Response:'
     O                       nrscnt        3  +   1
     *
     * Grand Totals
     *
     O          E            FndTotl        1 06
     O                       Dstime                 '  :  :  '
     O                       xxconm              82
     O                       Dsdate        Y    132
     *
     O          E            FndTotl        4
     O                       Q1pgm
     O                       xxccyy              58
     O                                        +   1 'Renewal Detail Status'
     O                                          128 'PAGE:'
     O                       Page          Z    132
     O          E            FndTotl        2
     O                                           72 'CertFee'
     O                                           89 'Direct'
     O                                          106 'Easy Pay'
     O                                          122 'Payroll'
     O          E            FndTotl        1
     O                                        +   1 'Grand Totals'
     O                       fndcnt        3  +   1
     O                                        +   1 'Groups Processed'
      ***** BEG CHG ***** 12/22/03 *******************************************
     O**********             actprmT       1B    81
     O**********             bilpremT      1b    95
     O**********             achpremT      1b   109
     O**********             vndpremT      1b   123
     O                       cerprmT       1b    72
     O                       dirprmT       1b    89
     O                       easprmT       1b   106
     O                       payprmT       1b   123
      ***** END CHG ***** 12/22/03 *******************************************
     /EJECT
     * ===================================================================== *
     *  QSYSPRT1 - 2ND REPORT - Cancellation Report Heading
     * ===================================================================== *
     OQSYSPRT1  E            Route          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            Route          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            Route          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            Route          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            Route          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            Route          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            Route          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            Route          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            Route          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            Route          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            Route          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            Route          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            Route          1
     O**********                                 15 'Route TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
      **********
     O**********E    08      Route          1
     O**********             ORDIS2              60
      **********
     O**********E    08      Route          1
     O**********             ORDIS3              60
      **********
     O**********E    08      Route          2
     O**********             ORDIS4              60
      **********
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            Route          1
     O                                           15 'Route TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
      **********
     O          E    08      Route          1
     O                       ORDIS2              60
      **********
     O          E    08      Route          1
     O                       ORDIS3              60
      **********
     O          E    08      Route          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            Route          2
     O                                           19 'REPORT TITLE:'
     O                 n08                       44 'Renewal Tracking Reports'
     O                  08   xxtitl              80
      *
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            Route          2
     O**********                                 19 'PROGRAM NAME:'
      ***** BEG ADD ***** 12/09/03 ****************************************
     O**********       n08   q1pgm               30
      ***** END ADD ***** 12/09/03 ****************************************
     O**********        08   ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 60 'HOLD:'
     O**********             ORHOLD              65
      **********
     O**********E            Route          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 60 'SAVE:'
     O**********             ORSAVE              65
      ***** End Del ***** 08/26/14 ************************************
      *
     O          E            Route       2  2
     O                                           46 'REPORT SELECTIONS'
      * 1
     O          E            Route          2
     O                                         +  6 'DATE RANGE . . . . . . . :'
     O                       Frdate            +  1
     O                                         +  2 '-'
     O                       Todate            +  2
      * 10
     O          E            Route          2
     O                                         +  6 'RUN TIME & DATE  . . . . :'
     O                       DSDTTM            +  1 '0 :  :  &  /  /  '
      *
     c*** Beg Add ***** 01/28/10 **************************************
     O***       E            Route          1 60
     O***                                        23 '(C) COPYRIGHT PROGRESSI'
     O***                                        46 'VE DATA SOLUTIONS. INC,'
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     O*** End Chg *****  01/28/10  ************************************
     *
     * ===================================================================== *
     OQSYSPRT1  E            Head1          1 06
     O                       Dstime                 '  :  :  '
     O                       xxconm             114
     O                       Dsdate        Y    196
     *
     O          E            Head1          1
     O                       Q1pgm
     O                                          102 'Cancelled/Non-Renewed'
     O                       xxccyy           +   1
     O                                        +   1 'Policies'
     O                                          191 'PAGE:'
     O                       Page1         Z    196
     *
     O          E            Head1          2
     O                       Today              102
     *
     O          E            Head1          1
     O                                            6 'Trust:'
     O                       xxgnam           +   1
     *
     O          E            Head1          2
     O                                           20 'Cert #'
     O                                           28 'Member'
     O                                           71 'Federal Name'
     O                                          103 'Broker'
     O                                          147 'Cancel Reason'
     O                                          196 'Amount'
     *
     O          E            canDetl        1
     O                       Ememp#        3     20
     O                       xename              58
     O                       xefdnm              96
     O                       xxanam             133
     O                       candesc            179
     O                       totlprm       1    196
     *
     * Group Totals
     O          E            Totl1       1  1
     O
     O                       xxfnd            +   1
     O                                        +   1 '-'
     O                       xxgnam           +   1
     O                       cancnt        3b +   8
     O                                        +   1 'Cancelled Count'
     O                                          178 'Cancelled Amount:'
     O                       canprem       1b   196
     /EJECT
     * ===================================================================== *
     *  QSYSPRT2 - 3RD REPORT - Renewal Status Report - As of Cash Receipts
     * ===================================================================== *
     OQsysprt2  E            Head2          1 06
     O                       Dstime                 '  :  :  '
     O                       xxconm              82
     O                       Dsdate        Y    132
     *
     O          E            Head2          1
     O                       Q1pgm
     O                       xxccyy              61
     O                                        +   1 'Renewal Status'
     O                                          128 'PAGE:'
     O                       Page2         Z    132
     *
     O          E            Head2          2
     O                                           66 'As of Cash Receipts'
     O                                        +   1 'Through:'
     O                       Today            +   1
     *
     * Group Name
     O          E            Head2          3
     O                       xxfnnm              97
     *
     O          E            DetlG          3
     O                       xxccyy               4
     O                                        +   1 'Policy Year'
     O                                           52 '# of Accounts'
     O                                           59 '%'
     O                                           82 '$'
     O                                           93 '%'
     *
     O          E            DetlG          3
     O                                           11 'Grand Total'
      ***** BEG CHG ***** 12/22/03 ****************************************
     O**********                                 24 'of Responses'
     O**********             actcnt        1     50
     O                       actcntG       1     50
     O                                           62 '100.00%'
     O**********             actprm        1     84
     O                       actprmG       1     84
      ***** END CHG ***** 12/22/03 ****************************************
     O                                           96 '100.00%'
     *
     O          E            Detlbil        2
     O                                           13 'Direct Billed'
     O                       bilcnt        1     50
     O                       bilpctr        B    62 '  0.  %'
     O                       bilprem       1     84
     O                       bilprpcr       B    96 '  0.  %'
     *
     O          E            Detlach        2
     O                                            8 'Easy Pay'
     O                       achcnt        1     50
     O                       achpctr        B    62 '  0.  %'
     O                       achprem       1     84
     O                       achprpcr       B    96 '  0.  %'
     *
     O          E            Detlvnd        3
     O                                            8 'Payroll '
     O                       vndcnt        3     50
     O                       vndpctr        B    62 '  0.  %'
     O                       vndprem       1     84
     O                       vndprpcr       B    96 '  0.  %'
     *
     O          E            Detlrsp        2
     O                                           23 'Total Responses'
     O                       rspcnt        1     50
     O                       rsppctr        B    62 '  0.  %'
     O                       rspprm        1     84
     O                       rspprpcr       B    96 '  0.  %'
     *
     O          E            Detlrsp        3
     O                                           23 'Non-Responses  '
     O                       nrscnt        1     50
     O                       nrspctr        B    62 '  0.  %'
     O                       nrsprm        1     84
     O                       nrsprpcr       B    96 '  0.  %'
     *
      ***** BEG ADD ***** 12/10/03 ****************************************
      ***** BEG DEL ***** 12/22/03 ****************************************
     O********* E            Detlrsp        3
     O*********                                  23 'Totals Both    '
     O*********              actcntG       1B    50
     O*********                                  62 '100.00%'
     O*********              actprmG       1B    84
     O*********                                  96 '100.00%'
      ***** DND DEL ***** 12/22/03 ****************************************
      ***** END ADD ***** 12/10/03 ****************************************
     *
     O          E            Detlcan        2
     O                                           21 'Cancelled/Non-Renewed'
     O                       cancnt        3     50
     O                       canprm        1     84
     *
      *
      ***** BEG ADD ***** 12/10/03 ****************************************
      *
     * Grand Totals
     O          E            Head2L         1 06
     O                       Dstime                 '  :  :  '
     O                       xxconm              82
     O                       Dsdate        Y    132
     *
     O          E            Head2L         1
     O                       Q1pgm
     O                       xxccyy              61
     O                                        +   1 'Renewal Status'
     O                                          128 'PAGE:'
     O                       Page2         Z    132
     *
     O          E            Head2L         2
     O                                           66 'As of Cash Receipts'
     O                                        +   1 'Through:'
     O                       Today            +   1
     *
     O          E            Head2L         2
     O                                           68 'Grand Totals for AL'
     O                                              'L Groups Listed'
     O          E            FndTotl2    2  3
     O                                           12 'Grand Totals'
     O                       fndcnt        1  +   1
     o                                        +   1 'Groups Processed'
     *
     O          E            DetlGL         3
     O                                           24 'Grand Total - ALL GROUPS'
      ***** BEG CHG ***** 12/22/03 ****************************************
     O************           actcntL       1     50
     O                       actcntY       1     50
     O                                           62 '100.00%'
     O************           actprmL       1     84
     O                       actprmY       1     84
      ***** END CHG ***** 12/22/03 ****************************************
     O                                           96 '100.00%'
     *
     O          E            DetlbilL       2
     O                                           26 'Direct Billed - ALL GROUPS'
     O                       bilcntL       1     50
     O                       bilpctrL       B    62 '  0.  %'
     O                       bilpremL      1     84
     O                       bilprpcrL      B    96 '  0.  %'
     *
     O          E            DetlachL       2
     O                                           21 'Easy Pay - ALL GROUPS'
     O                       achcntL       1     50
     O                       achpctrL       B    62 '  0.  %'
     O                       achpremL      1     84
     O                       achprpcrL      B    96 '  0.  %'
     *
     O          E            DetlvndL       3
     O                                           20 'Payroll - ALL GROUPS'
     O                       vndcntL       1     50
     O                       vndpctrL       B    62 '  0.  %'
     O                       vndpremL      1     84
     O                       vndprpcrL      B    96 '  0.  %'
     *
     O          E            DetlrspL       2
     O                                           36 'Total Responses - ALL GROU'
     O                                           38 'PS'
     O                       rspcntL       1     50
     O                       rsppctrL       B    62 '  0.  %'
     O                       rspprmL       1     84
     O                       rspprpcrL      B    96 '  0.  %'
     *
     O          E            DetlrspL       3
     O                                           34 'Non-Responses - ALL GROUPS'
     O                       nrscntL       1     50
     O                       nrspctrL       B    62 '  0.  %'
     O                       nrsprmL       1     84
     O                       nrsprpcrL      B    96 '  0.  %'
     *
      ***** BEG DEL ***** 12/22/03 ****************************************
     O********* E            DetlrspL       3
     O*********                                  23 'Totals Both    '
     O*********              actcntY       1     50
     O*********                                  62 '100.00%'
     O*********              actprmY       1     84
     O*********                                  96 '100.00%'
      ***** BEG DEL ***** 12/22/03 ****************************************
     *
     O          E            DetlcanL       2
     O                                           24 'Cancelled/Non-Renewed - '
     O                                           34 'ALL GROUPS'
     O                       cancntL       1     50
     O                       canprmL       1     84
      ***** END ADD ***** 12/10/03 ****************************************
     ** =============================================================== **
