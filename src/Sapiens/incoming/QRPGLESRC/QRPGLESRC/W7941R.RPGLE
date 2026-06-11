     hcopyright('(c) 2016 SfoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
      *****************************************************************
      *      W7941R - NEW/RENEWAL BUSINESS REPORT                     *
      *      CREATED 01/17/03 C:142   PROJECT: WC1029                 *
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 07/20/16 C:310 Project: WC2443                                *
      *                - Modified code to print report as per the     *
      *                  groups selected on group select screen.      *
      * 08/05/15 C:307 Project: SY354                                 *
      *                -Adding 'Route To:'                            *
      *                                                               *
      * 05/22/15 C:306 Project: SY246                                 *
      *                - Intializing the PRTFILE datastructure to     *
      *                  avoid the data decimal error.                *
      *                                                               *
      * 08/26/14 C:298 Project: SY354                                 *
      *                -Implement Routing Page Changes.               *
      *                                                               *
      *  07/14/14  C:283 Support: WA045 86021 (SY246)                 *
      *                  Print the Detail Info for all Clients and    *
      *                  when running for StoneRiver, do not print    *
      *                  the Detail Info.                             *
      *                                                               *
      *  04/24/14  C:300 Project :SY246 and Priss 39                  *
      *                  Do not show Employer Details                 *
      *                                                               *
      *  11/20/13  C:295 Support WA046 84105                          *
      *                  Scheduled credit/debit is not printing       *
      *                  Correct value.It is populating the prior     *
      *                  year DB/CR value to current year.            *
      *                                                               *
      *  06/22/12  C:255 Support WA114 79911                          *
      *                  Add and edit to check renewal flag instead   *
      *                  EMRQFL.                                      *
      *                                                               *
      *  02/08/11  C:180 SUPPORT 73933                                *
      *                  Make 08/19/10 change client-specific.        *
      *                                                               *
      *  01/11/11  C:189  Support WA133 74918                         *
      *                  Some renewal employers were not being        *
      *                  reported as the program was checking Renewal/*
      *                  Non-Renwal on prior year instead of the year *
      *                  being processed                              *
      *                                                               *
      *
      *  08/19/10  C:180 SUPPORT 73933                                *
      *                  SKIP LOST/DECLINED RENEWAL QUOTES WHEN
      *                  REQUESTING PENDING ACCT STATUS.
      *
      *  01/28/10  C:249 Project: SY215
      *            C:189 Retrieve brand Name
      *                                                               *
      * 09/04/08  C:062  Project: WC1611                              *
      *                  Use ELNWRW                                   *
      *                                                               *
      * 01/15/07  C:185  Seq#: WA139/55946                            *
      *                  Issue with incorrect policy number.          *
      *                                                               *
      * 05/05/06  C:33   Seq#: WA139/52623                            *
      *                  Expand field sizes for average fields.       *
      *                                                               *
      * 02/15/06  C:33   Project: WC1017  WA129/50150                 *
      *                  Account for situations where the same emp #  *
      *                   is in both Quote and Live.                  *
      *                   EBACUR is currently used in this process to *
      *                    indicate the employer was selected from    *
      *                    Quote environment.                         *
      *                                                               *
      * 06/14/05  C:132  Project: WA114  Seq: 46719                   *
      *                - New accounts printing when only want renewal *
      *                                                               *
      * 06/09/05  C:132  Project: WA114  Seq: 46719                   *
      *                - Not printing prior year premium if was not   *
      *                  same group, employer number.                 *
      *                                                               *
      * 02/03/05  C:132  Project: WA099  Seq: 45671                   *
      *                - Change to print final total page, if there   *
      *                  were any group total pages printed.          *
      *                                                               *
      * 12/14/04  C:132  Project: WA126  Seq: 44981                   *
      *                - Change to print group total page, if there   *
      *                  were any detail lines printed for the group. *
      *                - Add Group number to Headings.                *
      *                                                               *
      * 11/01/04  C:132  Project: WA129  Seq: 44290                   *
      *                  Change to print correct group name when the  *
      *                  group number changes and the prior group     *
      *                  total page has been printed.                 *
      *                                                               *
      * 07/06/04  C:61  Support WA114  43087                          *
      *               - Expand Average Experience Mod Result Field    *
      *                                                               *
      * 05/20/04  C:132 Project: 114WHQ   PRISS - 3                   *
      *               - Print Headings on First Detail line, don't    *
      *                 print total lines for agent if amts are zero. *
      *                                                               *
      * 05/06/04  C:132 Project: 114WHQ                               *
      *               - Add secondary selection:                      *
      *                  "Sort by Agent#/Name when Primary sort is    *
      *                   Underwriter Code.                           *
      *               - Totals by Agent within Underwriter Code if    *
      *                 have 2nd sort by Agent3/Name. Always give     *
      *                 totals for underwriter even if no 2nd sort.   *
      *                                                               *
      *  03/31/04 C:132 Project: WA121 Seq: 38407                     *
      *               - Correct Totals for Policy counts by moving    *
      *                 code to where print the detail lines, change  *
      *                 total headings.                               *
      *                                                               *
      *  02/13/04 C:132 Project: WA121 Seq: 38407                     *
      *               - Correct Totals on prior averages by moving   *
      *                 code from main division into a separate subr  *
      *                 The Code was not deleted, just moved.         *
      *               - Remove totals printed that were used in total *
      *                 group averages.                               *
      *                                                               *
      *                                                               *
      *  02/05/04 C:132 Project: WA121 Seq: 38407                     *
      *                 Correct Average of Exp Mods and DB/CR when   *
      *                 have multiple divisions. Only use state       *
      *                 reporting division for averages.              *
      *
      * 10/13/03  C:61  Project: WC1017
      *                -Use standard date conversion routines.
      *                -Add selection (LIVE / RENEWAL QUOTE / BOTH)
      *
      *  08/20/03 C:132 Project: WA121 Seq: 38407                     *
      *                 Correct print totals for Prior Premium and    *
      *                 Prior Avg DB/CR                               *
      *                                                               *
      *  08/13/03 C:132 Project: WA121 Seq: 38407                     *
      *                 Chg to print totals used for averages on      *
      *                 lines                                         *
      *                                                               *
      *  08/06/03 C:132 Project: WA121  Seq: 38407                    *
      *                 Chg to print original Exp Mod & DB/CD on      *
      *                 year line                                     *
      *                                                               *
      *  07/16/03 C:142 Supp 38033                                    *
      *  Chgd exp con to subtract from ann prem instead of adding to  *
      *****************************************************************
     FSmcol     IF   E           K Disk
     FWmupl     IF   E           K Disk
     FWmupl1    IF   E           K Disk
     F                                     Rename(Pup01:Pup99)
     FSmorl     IF   E           K Disk
     FWmfnl     IF   E           K Disk
     FWxebl     IF   E           K Disk
     FWmeml     IF   E           K Disk
     FWdell     IF   E           K Disk
      ******* beg add ****** 10/13/03 *********************************
     FWmemlQ    IF   E           K Disk
     F                                     Rename(Pem01:Pem02)
     FWdellQ    IF   E           K Disk
     F                                     Rename(Pel01:Pel02)
      ******* end add ****** 10/13/03 *********************************
     FWdehl12   IF   E           K Disk
     FWzexl     IF   E           K Disk
     FWmaal1    IF   E           K Disk
     *
     * Carrier/State Reporting Master File
     FWmrpl4    IF   E           K Disk
      ******* beg add ****** 10/13/03 *********************************
     FWmrplQ    IF   E           K Disk
     F                                     Rename(Prp01:Prp02)
      ******* end add ****** 10/13/03 *********************************
     *
     FWxnbp     UF A E           K Disk
     FWxnbp1    UF A E           K Disk
     FWxnbp2    UF A E           K Disk
     *
     FWfnbp     O    E             Disk
     F***** BEG-ADD ***** 07/20/16 *****************************************
     FWXGSL     IF   E           K DISK
     F***** END-ADD ***** 07/20/16 *****************************************
      ******* Beg Chg ****** 01/15/07 *********************************
     F************  Qsysprt   O    F  198        Printer OFLIND(*INOF)
     FQsysprt   O    F  208        Printer OFLIND(*INOF)
      ******* End Chg ****** 01/15/07 *********************************
     F                                     USROPN
     F                                     PRTCTL(Prtfile)
      ******* Beg add ****** 08/19/10 *********************************
     Fwaqtl7    if   e           k disk
      ******* Eeg add ****** 08/19/10 *********************************
      **
     D Active          S              1n
     D Pending         S              1n
     D New             S              1n
     D Renewal         S              1n
     D First           S              1n
      **
     D QQfrdt          S              8
     D QQtodt          S              8
     D QQpsrt          S              2
     D QQpsrtd         S             25
     D QQssrtd         S             25
      ******* beg del ****** 10/13/03 *********************************
     D******** QQtypo          S              1
      ******* end del ****** 10/13/03 *********************************
     D QQstat          S              1
     D QQagnt          S              5
     D QQchap          S              3
     D QQundw          S              3
     D QQprst          S              2
     D QQssrt          S              2
     D Srt1            C                   'Agent Number'
     D Srt2            C                   'Agent Name'
     D Srt3            C                   'Employer #'
     D Srt4            C                   'Employer Name'
     D Srt5            C                   'Underwriter Code'
     D Srt6            C                   'Issue/Eff Date'
      *
     D***  Begin Add  ***  05/05/06  **************************************
     D sizeDbCr        s             13  4
     D sizeExpMod      s             13  4
     D sizeAvgDbCr     s             -2    like(sizeDbCr)
     D sizeAvgExpMod   s             -2    like(sizeExpMod)
     D***  End   Add  ***  05/05/06  **************************************
     D Datusa          S               D   DATFMT(*USA)
     D Today           S               D   DATFMT(*USA)
     D Trnusa          S               D   DATFMT(*USA)
     D Issdate         S               D   DATFMT(*USA)
     D Pfdate          S               D   DATFMT(*USA)
     D Ptdate          S               D   DATFMT(*USA)
      ******* beg chg ****** 10/13/03 *********************************
     D**** Frdate          S               D   DATFMT(*USA)
     D**** Todate          S               D   DATFMT(*USA)
     D Frdate          S              8  0
     D Todate          S              8  0
      ******* end chg ****** 10/13/03 *********************************
     D Fromdt          S              7  0
     D Todt            S              7  0
     D DateChk         S              1N
     D Tab10           S             10
     D Pbsdesc         S             30
     D Totdbcr         S             13  4
     D Prdbcr          S             13  4
     D***  Begin Chg  ***  05/05/06  **************************************
     D*Dbcr*****       S              6  4
     D*Dbcrpr***       S              6  4
     D*Dbcrprt**       S              6  4
     D DbCr            s                   like(sizeAvgDbCr)
     D DbCrPr          s                   like(sizeAvgDbCr)
     D DbCrPrT         s                   like(sizeAvgDbCr)
     D***  End   Chg  ***  05/05/06  **************************************
     D Totemp          S              7  0
     D Premp           S              7  0
     D Totexpmod       S             13  4
     D Prexpmod        S             13  4
     D***  Begin Chg  ***  05/05/06  **************************************
     D*Expmod***       S              6  4
     D*Expmodpr*       S              6  4
     D ExpMod          s                   like(sizeAvgExpMod)
     D ExpModPr        s                   like(sizeAvgExpMod)
     D***  End   Chg  ***  05/05/06  **************************************
     D Totpol          S              7  0
     D Totpprem        S             13  2
     D Totprem         S             13  2
      *
     D Totdbcra        S             13  4
     D Prdbcra         S             13  4
     D***  Begin Chg  ***  05/05/06  **************************************
     D*Dbcra****       S              6  4
     D*Dbcrpra**       S              6  4
     D DbCrA           s                   like(sizeAvgDbCr)
     D DbCrPrA         s                   like(sizeAvgDbCr)
     D***  End   Chg  ***  05/05/06  **************************************
     D Totempa         S              7  0
     D Prempa          S              7  0
     D Totexpmoda      S             13  4
     D Prexpmoda       S             13  4
     D***  Begin Chg  ***  05/05/06  **************************************
     D*Expmoda**       S              6  4
     D*Expmodpra       S              6  4
     D ExpModA         s                   like(sizeAvgExpMod)
     D ExpModPrA       s                   like(sizeAvgExpMod)
     D***  End   Chg  ***  05/05/06  **************************************
     D Totpola         S              7  0
     D Totpprema       S             13  2
     D Totprema        S             13  2
      *
      ***** BEG ADD ***** 05/06/04 ************************************
     D Totdbcru        S             13  4
     D Prdbcru         S             13  4
     D***  Begin Chg  ***  05/05/06  **************************************
     D*Dbcru****       S              6  4
     D*Dbcrpru**       S              6  4
     D DbCrU           s                   like(sizeAvgDbCr)
     D DbCrPrU         s                   like(sizeAvgDbCr)
     D***  End   Chg  ***  05/05/06  **************************************
     D Totempu         S              7  0
     D Prempu          S              7  0
     D Totexpmodu      S             13  4
     D Prexpmodu       S             13  4
     D***  Begin Chg  ***  05/05/06  **************************************
     D*Expmodu**       S              6  4
     D*Expmodpru       S              6  4
     D ExpModU         s                   like(sizeAvgExpMod)
     D ExpModPrU       s                   like(sizeAvgExpMod)
     D***  End   Chg  ***  05/05/06  **************************************
     D Totpolu         S              7  0
     D Totppremu       S             13  2
     D Totpremu        S             13  2
      ***** END ADD ***** 05/06/04 ************************************
      *
      ***** BEG ADD ***** 05/20/04 ************************************
     D PrtNwHd         S              1N
      ***** END ADD ***** 05/20/04 ************************************
      *
     D Totdbcrf        S             13  4
     D Prdbcrf         S             13  4
     D Prdbcrt         S             13  4
     D***  Begin Chg  ***  05/05/06  **************************************
     D*Dbcrf****       S              6  4
     D*Dbcrprf**       S              6  4
     D DbCrF           s                   like(sizeAvgDbCr)
     D DbCrPrF         s                   like(sizeAvgDbCr)
     D***  End   Chg  ***  05/05/06  **************************************
     D Totempf         S              7  0
     D Prempf          S              7  0
     D Totexpmodf      S             13  4
     D Prexpmodf       S             13  4
     D***  Begin Chg  ***  05/05/06  **************************************
     D*Expmodf**       S              6  4
      ******beg*chg ****** 07/06/04 ***********************************
     D**********Expmodprf       S              6  4
     D*Expmodprf       S                   like(prexpmodf)
      ******end*chg ****** 07/06/04 ***********************************
     D ExpModF         s                   like(sizeAvgExpMod)
     D ExpModPrF       s                   like(sizeAvgExpMod)
     D***  End   Chg  ***  05/05/06  **************************************
     D Totpolf         S              7  0
     D Totppremf       S             13  2
     D Totpremf        S             13  2
      *
      ***** BEG ADD ***** 08/06/03 ***********************************
     D Xpemex          S                   like(ELexm1)
     D Xpdbcr          S                   like(ELdbcr)
      ***** END ADD ***** 08/06/03 ***********************************
      *
      ****** BEG ADD ***** 02/05/04 ****************************************
     D Strpdiv         S                   like(Emdiv)
      ****** END ADD ***** 02/05/04 ****************************************
      *
      ***** BEG ADD ***** 05/06/04 ************************************
     D SvUwtr          S                   like(Emuwtr)
     D XXUwtr          S                   like(Emuwtr)
     D SvUwnm          S                   like(UpUsnm)
      ***** END ADD ***** 05/06/04 ************************************
      *
      ***** BEG ADD ***** 12/14/04 *********************************************
     D ZZdiv           S                   like(Emdiv)
     D DetlCnt         S              7  0 inz
      ***** END ADD ***** 12/14/04 *********************************************
      *
      ***** BEG ADD ***** 02/03/04 ************************************
     D GroupCnt        S              3  0 inz
      ***** END ADD ***** 02/03/04 ************************************
      *
     D***  Begin Add  ***  01/28/10  **************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
     d***** Beg Add ****** 11/20/13 ***************************************
     d xcDbCr          s                   like(ELdbcr)
     d xcNwRw          s                   like(ELnwrw)
     d***** End Add ****** 11/20/13 ***************************************
      ***** BEG ADD ***** 07/14/14 ************************************
     D xxPGM           S             10
     D xxERR           S              1
     D xxDTL           S              1

     D CheckPGMStack   PR                  EXTPGM('SRCHKPGM')
     D xxPGM                         10
     D xxErr                          1
      ***** END ADD ***** 07/14/14 ************************************
      *
     D                 DS
     D  Dsdttm                 1     12  0
     D  Dstime                 1      6  0
     D  Dsdate                 7     12  0
      *
     D PSDS          ESDS                  Extname(Szq1p)
      ***** Beg Chg ***** 05/22/15 ************************************
     D*Prtfile         DS
     D**Q4spb                         3
     D**Q4spa                         3
     D**Q4skb                         3
     D**Q4ska                         3
     D**Q4Lin#                        3  0
      *
     D Prtfile         DS                  Inz
     D  Q4spb                         3    Inz(' ')
     D  Q4spa                         3    Inz(' ')
     D  Q4skb                         3    Inz(' ')
     D  Q4ska                         3    Inz(' ')
     D  Q4Lin#                        3  0 Inz(0)
      ***** End Chg ***** 05/22/15 ************************************
      ***** BEG-ADD ***** 07/20/16 ************************************
     D charGrpSlct     S              1A
     D SlctdGrp        S            100A   inz(*Blanks)
     D SlctPri         S             15A
     D SlctSec         S             15A
     D SlctPrnt        S             25A
      ***** END-ADD ***** 07/20/16 ************************************
      *
     C                   Eval      *Inlr = *Off
      /eject
      **********************************************************************
      * M A I N L I N E   R O U T I N E                                    *
      **********************************************************************
      *
      ***** BEG ADD ***** 07/14/14 ************************************
      /Free
       xxPGM = 'RPTQTRC';
       xxERR = *Blanks;
       CheckPGMStack(xxPGM : xxERR);
       If xxERR = '1';
          xxDTL = 'N';
       Else;
          xxDTL = 'Y';
       Endif;
      /End-Free
      ***** END ADD ***** 07/14/14 ************************************
      *
      *  Read work file input
      *
     C                   Dou       %Eof(Wxebl)
     C                   Read      Wxebl
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif

     C***  Begin Add  ***  02/15/06  **************************************
     C                   if        EBACUR = 'Q'
     C***  End   Add  ***  02/15/06  **************************************
     C*
      ******* Beg Chg ****** 06/22/12 *********************************
      ******* beg add ****** 10/13/03 *********************************
     C*****Wmeml_Key     Chain     Wmemlq
      *****
     C*****              If        ( (%Found(Wmemlq)) and (emrqfl <> 'Y') )
     c*****              Iter
     C*****              Endif
      ******* end add ****** 10/13/03 *********************************

     C     Wdell_Key     Chain     Wdellq
     C                   If        ( (%Found(Wdellq)) and (elnwrw <> 'R') )
     c                   Iter
     C                   Endif
      ******* End Chg ****** 06/22/12 *********************************
     C*
     C***  Begin Add  ***  02/08/11  **************************************
     C                   IF        CLTID = '132'
     C***  Begin Add  ***  08/19/10  **************************************
     C                   IF        (SCAPB = 'P' or SCAPB = 'B')
     C     QTKY          SETGT     WAQTL7
     C     QTKY1         READPE    WAQTL7
     C                   IF        (NOT %EOF(WAQTL7)) and
     c                             (QTTYPE ='D' or QTTYPE = 'L')
     c                   iter
     c                   ENDIF
     C                   ENDIF
     C***  END   Add  ***  08/19/10  **************************************
     C                   ENDIF
     C***  END   Add  ***  02/08/11  **************************************
     C***  Begin Add  ***  02/15/06  **************************************
     C                   Endif
     C***  End   Add  ***  02/15/06  **************************************

     C                   Clear                   Pex01
     C                   Clear                   Pnb01
     C                   Clear                   Uwname
     C                   Movel     Ebanam        Pranam           30
     C                   Eval      Prfyr = Ebfyr - 1
      *
      ***** BEG ADD ***** 02/13/04 ******************************************
     C                   IF        EBFND <> SVFND AND SVFND > *ZEROS
     C                   EXSR      PRINT_TOTS
     C                   ENDIF
      ***** END ADD ***** 02/13/04 ******************************************
      *
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Movel     ebsrt1        XxUwtr
     C                   IF        Not First
     C                   select
      * Break on PRIMARY SORT if Agent Number or Agent Name
     C                   When      (EBAGT# <> SVAGT# AND QQPSRT = 'AN') OR
     C                             (EBANAM <> SVANAM AND QQPSRT = 'AA')
     C                   EXSR      PRINT_TOTS
      *
      * Break on PRIMARY SORT if Underwriter
     C                   When      (XxUwtr <> SVUWTR AND QQPSRT = 'UW')
     C                   EXSR      PRINT_TOTS
      *
      * Break on SECONDARY SORT if Agent Number or Agent Name
     C                   When      (EBAGT# <> SVAGT# AND QQSSRT = 'AN') OR
     C                             (EBANAM <> SVANAM AND QQSSRT = 'AA')
     C                   EXSR      PRINT_TOTS
     C                   EndSL
      *
     C                   ENDIF                                                  Not First
      ***** END ADD ***** 05/06/04 ************************************
      *
     C     Wmfnl_Key     Chain     Wmfnl
      *
     C                   Movel(p)  Coname        Nbconm
     C                   Movel(p)  Fnname        Nbfnnm
      *
     C                   Exsr      Agent_Fein
     C                   Exsr      Proc_Curr
     C
     C*** begin add *** 01/11/11 *******************************************
     C                   eval      nbnwrw=elnwrw
     C*
     C                   if        elnwrw='N'
     C                   eval      New=*on
     C                   eval      Renewal=*off
     C                   endif
     C*
     C                   if        elnwrw='R'
     C                   eval      New=*off
     C                   eval      Renewal=*on
     C                   endif
     C*** end   add *** 01/11/11 *******************************************
      *  Get prior year detail record
     C     Wdell_Key3    Chain     Wdell
      *
     C                   Eval      *In70 = *Off
      *
     C*** begin del *** 01/11/11 *******************************************
     C*** begin chg *** 09/04/08 *******************************************
     C**********         Exsr      NewRenew
     C**********         eval      nbnwrw=elnwrw
     C**********
     C**********         if        elnwrw='N'
     C**********         eval      New=*on
     C**********         eval      Renewal=*off
     C**********         endif
     C***********
     C**********         if        elnwrw='R'
     C**********         eval      New=*off
     C**********         eval      Renewal=*on
     C**********         endif
     C*** end   chg *** 09/04/08 *******************************************
     C*** end   del *** 01/11/11 *******************************************
      *
      ***** BEG ADD ***** 06/14/05 ****************************************
      * If Renewal, check X-Ref file first, if get hit and premium don't
      * Check for Prior (*IN70 is *ON)
     C                   IF        Renewal and Not *IN51                        Renew or both
     C                   Exsr      Proc_Wzexl
     C                   IF        NOT *IN70
     C                   Exsr      Proc_Prior
     C                   Endif
     C                   Endif
      *
     C                   If        New
     C                   Exsr      Proc_Wzexl
     C                   Endif
      ***** END ADD ***** 06/14/05 ****************************************
      *
      ***** BEG DEL ***** 06/14/05 ****************************************
     C**********         If        %Found(Wdell)
     C**********         Exsr      Proc_Prior
     C**********         Else
     C**********         Exsr      Proc_Wzexl
     C**********         Endif
      ***** BEG DEL ***** 06/14/05 ****************************************
      *
     C                   Exsr      Get_Pol#
      *
     C                   Eval      Nbagt#  = Ebagt#
     C                   Eval      Nbagnm  = Ebanam
      *
     C                   Eval      Nbpgm   = Q1pgm
     C                   Eval      Nbuser  = Q1User
     C                   Eval      Nbdt    = Q1Dt
     C     *Mdy          Move      Q1Dt          Datusa
     C                   Move      Datusa        Nbdt
     C                   Eval      Nbtime  = Q1Time
      *
     C                   Exsr      ActivePend
     C                   If        Active and
     C                             Scapb = 'P'
     C                   Iter
     C                   Endif
      *
     C                   If        Pending and
     C                             Scapb = 'A'
     C                   Iter
     C                   Endif
      *
     C                   If        New and
     C                             Scnrb = 'R'
     C                   Iter
     C                   Endif
     C                   If        Renewal and
     C                             Scnrb = 'N'
     C                   Iter
     C                   Endif
      *
     C                   Exsr      Issue
      *
      ******* beg del ****** 10/13/03 *********************************
     C**** *Ymd          Move      Ebpfdt        Pfdate
     C**** *Usa          Move      Pfdate        Effdte            8 0
     C**** *Ymd          Move      Ebptdt        Ptdate
     C**** *Usa          Move      Ptdate        Expdte            8 0
      ******* end del ****** 10/13/03 *********************************
      ******* beg add ****** 10/13/03 *********************************
      *
     C                   z-add     Ebpfdt        SRDTI
     C                   Call      'SRCVTDAT'    DTMDYY
     C                   Movel     SRDTO         Effdte            8 0
      *
     C                   z-add     Ebptdt        SRDTI
     C                   Call      'SRCVTDAT'    DTMDYY
     C                   Movel     SRDTO         Expdte            8 0
      *
      ******* end add ****** 10/13/03 *********************************
      *
     C                   Eval      Nbco#   = Ebco#
     C                   Eval      Nbfnd   = Ebfnd
     C                   Eval      Nbemp#  = Ebemp#
     C                   Eval      Nbfyr   = Ebfyr
     C                   Eval      Nbdiv   = Ebdiv
     C                   Eval      Nbenam  = Ebenam
     C                   Movel     Ebenam        Prenam           26
      *
      ******* beg del ****** 10/13/03 *********************************
     C**** *Ymd          Move      Ebindt        Datusa
     C**** *Usa          Move      Datusa        Nbindt
     C**** *Ymd          Move      Ebpfdt        Datusa
     C**** *Usa          Move      Datusa        Nbpfdt
     C**** *Ymd          Move      Ebptdt        Datusa
     C**** *Usa          Move      Datusa        Nbptdt
      ******* end del ****** 10/13/03 *********************************
      ******* beg add ****** 10/13/03 *********************************
      *
     C                   z-add     Ebindt        SRDTI
     C                   Call      'SRCVTDAT'    DTMDYY
     C                   Movel     SRDTO         Nbindt            8 0
      *
     C                   z-add     Ebpfdt        SRDTI
     C                   Call      'SRCVTDAT'    DTMDYY
     C                   Movel     SRDTO         Nbpfdt            8 0
      *
     C                   z-add     Ebptdt        SRDTI
     C                   Call      'SRCVTDAT'    DTMDYY
     C                   Movel     SRDTO         Nbptdt            8 0
      *
      ******* end add ****** 10/13/03 *********************************
     C                   Clear                   Nbcxdt
      *
     C                   Exsr      Get_Wmemp
     C                   Enddo
      *
      * Process last record
      *
      ***** BEG ADD ***** 12/14/04 *********************************************
      * If don't have any totals but printed a detail line, print fnd total Page
     C                   If        Scpfb <> 'F' and
     C                             Totempa  = *Zero AND
     C                             DetlCnt  > *Zero
     C                   Clear                   Totempf
     C                   Clear                   Totpolf
     C                   Clear                   Totpremf
     C                   Clear                   Totppremf
     C                   Clear                   Dbcrf
     C                   Clear                   Dbcrprf
     C                   Clear                   Expmodf
     C                   Clear                   Expmodprf
     C                   Except    Head
     C                   Except    HeadGrp
     C                   Except    Totalfnd
     C                   Except    Head
      *
      ***** BEG ADD ***** 02/03/05 ************************************
     C                   Eval      Groupcnt = Groupcnt + 1
      ***** END ADD ***** 02/03/05 ************************************
      *
     C                   If        Scpfb <> 'F' and
     C                             Totemp <> *Zero
     C                   Eval(h)   Dbcr = Totdbcr / Totemp
     C                   Eval(h)   Expmod = Totexpmod / Totemp
     C                   If        Premp <> *Zero
     C                   Eval(h)   Dbcrprt = Prdbcrt / Premp
     C                   Eval(h)   Expmodpr = Prexpmod / Premp
     C                   Endif
     C                   Endif
      *
      ***** BEG DEL ***** 02/03/05 ************************************
      * Chg to Print Total page at end if printed any group totals
     C*************      Except    Total
      ***** END DEL ***** 02/03/05 ************************************
     C                   Endif
      ***** END ADD ***** 12/14/04 *********************************************
      *
     C                   If        Scpfb <> 'F' and
     C                             Totempa <> *Zero
     C                   Eval(h)   Dbcra = Totdbcra / Totempa
     C                   Eval(h)   Expmoda = Totexpmoda / Totempa
      *
     C                   If        Prempa <> *Zero
     C                   Eval(h)   Dbcrpra = Prdbcra / Prempa
     C                   Eval(h)   Expmodpra = Prexpmoda / Prempa
     C                   Endif
      *
      ***** BEG CHG ***** 05/06/04 ************************************
     C************       If        QQpsrt = 'AA' or
     C************                 QQpsrt = 'AN'
     C                   If        QQpsrt = 'AA' or QQpsrt = 'AN' or
     C                             QQssrt = 'AA' or QQssrt = 'AN'
      ***** END CHG ***** 05/06/04 ************************************
      *
     C                   If        Q4Lin# > 46
      ***** BEG DEL ***** 04/24/14 ************************************
     C*********          Except    Head
     C*********          Except    HeadGrp
     C*********          Except    Total
      ***** BEG ADD ***** 05/06/04 ************************************
     C*********          Except    HeadUND
      ***** END ADD ***** 05/06/04 ************************************
     C*********          Except    Headagt
     C*********          Except    Headcol
      ***** END DEL ***** 04/24/14 ************************************
      ***** BEG ADD ***** 07/14/14 ************************************
      /Free
       If xxDTL = 'Y';
          Except Head;
          Except HeadGrp;
          Except Total;
          Except HeadUND;
          Except Headagt;
          Except Headcol;
       Endif;
       /End-Free
      ***** END ADD ***** 07/14/14 ************************************
     C                   Endif
      *
     C                   If        totempa <> *zeros
     C                   Eval(h)   Dbcra = Totdbcra / Totempa
     C                   Eval(h)   Expmoda = Totexpmoda / Totempa
     C                   Endif
      *
     C                   If        Prempa <> *Zero
     C                   Eval(h)   Dbcrpra = Prdbcra / Prempa
     C                   Eval(h)   Expmodpra = Prexpmoda / Prempa
     C                   Endif
      *
     C                   Except    Totalag
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Clear                   DBCRA
     C                   Clear                   EXPMODA
     C                   Clear                   DBCRPRA
     C                   Clear                   EXPMODPRA
     C                   Clear                   PRDBCRA
     C                   Clear                   PREXPMODA
     C                   Clear                   PREMPA
     C                   Clear                   Totdbcra
     C                   Clear                   Totempa
     C                   Clear                   Totexpmoda
     C                   Clear                   Totpola
     C                   Clear                   Totpprema
     C                   Clear                   Totprema
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Endif
      *
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   IF        TOTEMPU <> *ZEROS AND QQPSRT = 'UW'
     C                   Eval(h)   Dbcru = Totdbcru / Totempu
     C                   Eval(h)   Expmodu = Totexpmodu / Totempu
     C                   If        Prempu <> *Zero
     C                   Eval(h)   Dbcrpru = Prdbcru / Prempu
     C                   Eval(h)   Expmodpru = Prexpmodu / Prempu
     C                   Endif
     C                   Except    Head
     C                   Except    Totaluw
     C                   Clear                   DBCRU
     C                   Clear                   EXPMODU
     C                   Clear                   DBCRPRU
     C                   Clear                   EXPMODPRU
     C                   Clear                   PRDBCRU
     C                   Clear                   PREXPMODU
     C                   Clear                   PREMPU
     C                   Clear                   Totdbcru
     C                   Clear                   Totempu
     C                   Clear                   Totexpmodu
     C                   Clear                   Totpolu
     C                   Clear                   Totppremu
     C                   Clear                   Totpremu
     C                   Endif
      ***** END ADD ***** 05/06/04 ************************************
      *
     C                   If        Scpfb <> 'F' and
     C                             Totempf <> *Zero
      *
     C                   Eval(h)   Dbcrf = Totdbcrf / Totempf
     C                   Eval(h)   Expmodf = Totexpmodf / Totempf
      *
     C                   If        Prempf <> *Zero
     C                   Eval(h)   Dbcrprf = Prdbcrf / Prempf
     C                   Eval(h)   Expmodprf = Prexpmodf / Prempf
     C                   Endif
      *
     C                   Endif
      *
     C                   Except    Head
     C                   Except    Totalfnd
      *
      ***** BEG ADD ***** 02/03/05 ************************************
     C                   Eval      Groupcnt = Groupcnt + 1
      ***** END ADD ***** 02/03/05 ************************************
      *
      ***** BEG ADD ***** 12/14/04 *********************************************
     C                   Z-ADD     *zeros        DetlCnt
      ***** END ADD ***** 12/14/04 *********************************************
      *
     C                   Except    Head
      *
     C                   If        Scpfb <> 'F' and
     C                             Totemp <> *Zero
     C                   Eval(h)   Dbcr = Totdbcr / Totemp
     C                   Eval(h)   Expmod = Totexpmod / Totemp
      *
     C                   If        Premp <> *Zero
      ***** BEG CHG ***** 08/20/03 *****************************************
     C*************      Eval(h)   Dbcrprt = Prdbcr / Premp
     C                   Eval(h)   Dbcrprt = Prdbcrt / Premp
      ***** END CHG ***** 08/20/03 *****************************************
     C                   Eval(h)   Expmodpr = Prexpmod / Premp
     C                   Endif
      *
     C                   Endif
      *
      ***** BEG DEL ***** 02/03/05 ************************************
      * Chg to Print Total page at end if printed any group totals
     C*************      Except    Total
      ***** END DEL ***** 02/03/05 ************************************
     C                   Endif
      *
      ***** BEG ADD ***** 02/03/05 ************************************
     C                   IF        Groupcnt <> *zeros
     C                   Except    Total
     C                   EndIF
      ***** END ADD ***** 02/03/05 ************************************
      *
      /Free
       //***** Beg Add ***** 08/26/14 ********************************
       xxPrt = '0';
       Chain Smorl_Key POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ********************************
      /End-free
     C                   Eval      *Inlr = *On
      *
      *
      *
      **********************************************************************
      * Main processing routine
      **********************************************************************
      *
      * ==================== *
     C     Main          Begsr
      * ==================== *
      *
      ******* beg chg ****** 10/13/03 *********************************
     C*********          If        QQtypo <> 'F'
     C                   If        Scpfb  <> 'F'
      ******* end chg ****** 10/13/03 *********************************
      *
      *
      ***** BEG DEL ***** 02/13/04 ******************************************
      *
      * ALL the code in Subroutine PRINT_TOTS was move from this subroutine
      * to inside the "IF SCPBF <> 'F'" loop in PRINT_TOTS subroutine.
      * The code was above these lines
      *
      ***** END DEL ***** 02/13/04 ******************************************
      *
      ***** BEG ADD ***** 03/31/04 ********************************************
     C                   Eval      Totpola = Totpola + 1
     C                   Eval      Totpol = Totpol + 1
     C                   Eval      Totpolf = Totpolf + 1
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Eval      Totpolu = Totpolu + 1
      ***** END ADD ***** 05/06/04 ************************************
      *
      ***** END ADD ***** 03/31/04 ********************************************
      *
      ***** BEG ADD ***** 05/20/04 ************************************
     C                   If        *Inof = *On OR PrtNwHd
      ***** BEG DEL ***** 04/24/14 ************************************
     C*********          Except    Head
     C*********          Except    HeadGrp
     C*********          Except    HeadUND
     C*********          Except    HeadAgt
      *
     C*********          Except    Headcol
      ***** END DEL ***** 04/24/14 ************************************
      ***** BEG ADD ***** 07/14/14 ************************************
      /Free
       If xxDTL = 'Y';
          Except Head;
          Except HeadGrp;
          Except HeadUND;
          Except HeadAgt;
          Except Headcol;
       Endif;
      /End-Free
      ***** END ADD ***** 07/14/14 ************************************
     C                   EVAL      *INOF = *OFF
     C                   EVAL      PrtNwHd = *OFF
     C                   Endif
      ***** END ADD ***** 05/20/04 ************************************
      *
      ***** BEG DEL ***** 04/24/14 ************************************
     C*********          Except    Detail
      ***** END DEL ***** 04/24/14 ************************************
      ***** BEG ADD ***** 07/14/14 ************************************
      /Free
       If xxDTL = 'Y';
          Except Detail;
       Endif;
      /End-Free
      ***** END ADD ***** 07/14/14 ************************************
     C                   Eval      *In70 = *Off
     C                   Eval      *In72 = *Off
     C                   Clear                   Pex01
      *
      ***** BEG ADD ***** 12/14/04 *********************************************
     C                   Eval      DetlCnt = DetlCnt  + 1
      ***** END ADD ***** 12/14/04 *********************************************
      *
      ***** BEG DEL ***** 05/20/04 ************************************
     C***********        If        *Inof = *On
     C***********        Except    Head
     C***********        Except    HeadGrp
      ***** BEG ADD ***** 05/06/04 ************************************
     C**********         Except    HeadUND
     C***********        If        QQSsrt = 'AA' or
     C***********                  QQSsrt = 'AN'
     C***********        Except    HeadAgt
     C***********        Endif
      ***** END ADD ***** 05/06/04 ************************************
     C**********         If        QQpsrt = 'AA' or
     C**********                   QQpsrt = 'AN'
     C**********         Except    HeadAgt
     C**********         Else
     C**********         Endif
     C**********         Except    Headcol
     C**********         Endif
      ***** BEG DEL ***** 05/20/04 ************************************
      *
     C                   Eval      Totprem   = Nbprmr + Totprem
     C                   Eval      Totpprem  = Nbprmp + Totpprem
     C                   Eval      Totprema  = Nbprmr + Totprema
     C                   Eval      Totpprema = Nbprmp + Totpprema
     C                   Eval      Totpremf  = Nbprmr + Totpremf
     C                   Eval      Totppremf = Nbprmp + Totppremf
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Eval      Totpremu  = Nbprmr + Totpremu
     C                   Eval      Totppremu = Nbprmp + Totppremu
      ***** END ADD ***** 05/06/04 ************************************
      *
      ****** BEG ADD ***** 02/05/04 ****************************************
      * If state reporting division, do the accumulations, else bypass
     C                   if        ebdiv  = strpdiv
      ****** END ADD ***** 02/05/04 ****************************************
      *
     C                   If        Xxemex > *Zero
     C                   Eval      Totexpmod  = Xxemex + Totexpmod
     C                   Eval      Totexpmoda = Xxemex + Totexpmoda
     C                   Eval      Totexpmodf = Xxemex + Totexpmodf
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Eval      Totexpmodu = Xxemex + Totexpmodu
      ***** END ADD ***** 05/06/04 ************************************
      *
     C                   Else
      *
     C                   Eval      Totexpmod  = 1 + Totexpmod
     C                   Eval      Totexpmoda = 1 + Totexpmoda
     C                   Eval      Totexpmodf = 1 + Totexpmodf
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Eval      Totexpmodu = 1 + Totexpmodu
      ***** END ADD ***** 05/06/04 ************************************
     C                   Endif
      *
     C                   If        Eldbcr   > *Zero
     C                   Eval      Totdbcr  = Eldbcr + Totdbcr
     C                   Eval      Totdbcra = Eldbcr + Totdbcra
     C                   Eval      Totdbcrf = Eldbcr + Totdbcrf
      *
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Eval      Totdbcru = Eldbcr + Totdbcru
      ***** END ADD ***** 05/06/04 ************************************
      *
     C                   Else
      *
     C                   Eval      Totdbcr = 1 + Totdbcr
     C                   Eval      Totdbcra = 1 + Totdbcra
     C                   Eval      Totdbcrf = 1 + Totdbcrf
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Eval      Totdbcru = 1 + Totdbcru
      ***** END ADD ***** 05/06/04 ************************************
      *
     C                   Endif
      *
     C                   Eval      Totemp  = Totemp  + 1
     C                   Eval      Totempa = Totempa + 1
     C                   Eval      Totempf = Totempf + 1
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Eval      Totempu = Totempu + 1
      ***** END ADD ***** 05/06/04 ************************************
      *
      ****** BEG ADD ***** 02/05/04 ****************************************
     C                   endif                                                  ebdiv = strpdiv
      ****** END ADD ***** 02/05/04 ****************************************
      *
      ***** BEG CHG ***** 05/06/04 ************************************
     C**************     If        QQpsrt = 'AN'
     C                   If        QQpsrt = 'AN' OR QQssrt = 'AN'
      ***** END CHG ***** 05/06/04 ************************************
     C     Wxnbp1_Key    Chain     Wxnbp1
      *
     C                   If        Not %Found(Wxnbp1)
      ***** BEG DEL ***** 03/31/04 ********************************************
     C***********        Eval      Totpola = Totpola + 1
      ***** END DEL ***** 03/31/04 ********************************************
     C                   Eval      Xbco#  = Ebco#
     C                   Eval      Xbfnd  = Ebfnd
     C                   Eval      Xbemp# = Ebemp#
     C                   Eval      Xbfyr  = Ebfyr
     C                   Eval      Xbagt# = Ebagt#
     C                   Eval      Xbagnm = Ebanam
     C                   Write     Pxb02
     C                   Endif
      *
     C                   Endif
      *
      ***** BEG CHG ***** 05/06/04 ************************************
     C**************     If        QQpsrt = 'AA'
     C                   If        QQpsrt = 'AA' OR QQssrt = 'AA'
      ***** END CHG ***** 05/06/04 ************************************
     C     Wxnbp2_Key    Chain     Wxnbp2
      *
     C                   If        Not %Found(Wxnbp2)
      ***** BEG DEL ***** 03/31/04 ********************************************
     C************       Eval      Totpola = Totpola + 1
      ***** END DEL ***** 03/31/04 ********************************************
     C                   Eval      Xbco#  = Ebco#
     C                   Eval      Xbfnd  = Ebfnd
     C                   Eval      Xbemp# = Ebemp#
     C                   Eval      Xbfyr  = Ebfyr
     C                   Eval      Xbagt# = Ebagt#
     C                   Eval      Xbagnm = Ebanam
     C                   Write     Pxb03
     C                   Endif
      *
     C                   Endif
      *
     C     Wxnbp_Key     Chain(N)  Wxnbp
      *
     C                   If        Not %Found(Wxnbp)
      ***** BEG DEL ***** 03/31/04 ********************************************
     C***********        Eval      Totpol = Totpol + 1
     C***********        Eval      Totpolf = Totpolf + 1
      ***** END DEL ***** 03/31/04 ********************************************
     C                   Eval      Xbco#  = Ebco#
     C                   Eval      Xbfnd  = Ebfnd
     C                   Eval      Xbemp# = Ebemp#
     C                   Eval      Xbfyr  = Ebfyr
     C                   Eval      Xbagt# = Ebagt#
     C                   Eval      Xbagnm = Ebanam
     C                   Write     Pxb01
     C                   Endif
      *
     C                   Endif
      *
      ******* beg chg ****** 10/13/03 *********************************
     C***********        If        QQtypo <> 'R'
     C***********        Exsr      Write_Wfnbp
     C                   If        Scpfb  <> 'P'
     C                   Write     Pnb01
      ******* end chg ****** 10/13/03 *********************************
     C                   Endif
      *
     C                   Endsr
      *
      ***** BEG ADD ***** 02/13/04 ******************************************
      *
      **********************************************************************
      * Print Totals when group number changes
      **********************************************************************
      *
      * ==================== *
     C     Print_Tots    Begsr
      * ==================== *
      *
     C                   If        Scpfb  <> 'F'
      *
      *  ALL THE CODE INSIDE THIS IF LOOP WAS MOVED FROM MAIN SUBROUTINE
      *
      * Check for agent name break
      *
      ***** BEG CHG ***** 05/06/04 ************************************
     C************       If        QQpsrt = 'AA'
     C************                 (Ebanam <> Svanam or
     C                   If        (QQpsrt = 'AA' AND Ebanam <> SvAnam) OR
     C                             (QQssrt = 'AA' AND Ebanam <> SvAnam) OR
     C                             (QQssrt = 'AA' AND XxUwtr <> SvUwtr AND
     C                             QQpsrt = 'UW') OR
     C                             Ebfnd <> Svfnd
      ***** END CHG ***** 05/06/04 ************************************
      *
     C                   If        Not First
      *
      ***** BEG ADD ***** 05/20/04 ************************************
      ***** END ADD ***** 05/20/04 ************************************
      *
      ***** BEG CHG ***** 05/20/04 ************************************
     C************       If        Q4Lin# > 46
     C                   If        Q4Lin# > 46 AND totempa <> *zeros
      ***** END CHG ***** 05/20/04 ************************************
      ***** BEG DEL ***** 04/24/14 ************************************
     C*********          Except    Head
     C*********          Except    HeadGrp
      ***** BEG ADD ***** 05/06/04 ************************************
     C*********          Except    HeadUND
      ***** END ADD ***** 05/06/04 ************************************
     C*********          Except    Headagt
     C*********          Except    Headcol
      ***** END DEL ***** 04/24/14 ************************************
      ***** BEG ADD ***** 07/14/14 ************************************
      /Free
       If xxDTL = 'Y';
          Except Head;
          Except HeadGrp;
          Except HeadUND;
          Except Headagt;
          Except Headcol;
       Endif;
      /End-Free
      ***** END ADD ***** 07/14/14 ************************************
     C                   Endif
      *
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   If        totempa <> *zeros
      ***** END ADD ***** 05/06/04 ************************************
     C                   Eval(h)   Dbcra = Totdbcra / Totempa
     C                   Eval(h)   Expmoda = Totexpmoda / Totempa
      ***** BEG DEL ***** 05/20/04 ************************************
      ***** BEG ADD ***** 05/06/04 ************************************
     C*********          Endif
      ***** END ADD ***** 05/06/04 ************************************
      ***** END DEL ***** 05/20/04 ************************************
     C                   If        Prempa <> *Zero
     C                   Eval(h)   Dbcrpra = Prdbcra / Prempa
     C                   Eval(h)   Expmodpra = Prexpmoda / Prempa
     C                   Endif
     C                   Except    Totalag
      ***** BEG ADD ***** 05/20/04 ************************************
     C                   Eval      PrtNwHd = *ON
      ***** END ADD ***** 05/20/04 ************************************
     C                   Endif
      ***** BEG ADD ***** 05/20/04 ************************************
     C                   Endif
      ***** END ADD ***** 05/20/04 ************************************
      *
     C                   Eval      Svanam = Ebanam
     C                   Eval      Svagt# = Ebagt#
     C                   Eval      *Inof = *Off
      *
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Clear                   DBCRA
     C                   Clear                   EXPMODA
     C                   Clear                   DBCRPRA
     C                   Clear                   EXPMODPRA
     C                   Clear                   PRDBCRA
     C                   Clear                   PREXPMODA
     C                   Clear                   PREMPA
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Clear                   Totdbcra
     C                   Clear                   Totempa
     C                   Clear                   Totexpmoda
     C                   Clear                   Totpola
     C                   Clear                   Totpprema
     C                   Clear                   Totprema
      *
      ***** BEG DEL ***** 05/20/04 ************************************
     C**********         If        Ebfnd = Svfnd and
     C**********                   Not %Eof(Wxebl)
     C**********         Except    Head
     C**********         Except    HeadGrp
      ***** BEG ADD ***** 05/06/04 ************************************
     C**********         Except    HeadUND
      ***** END ADD ***** 05/06/04 ************************************
     C***********        Except    HeadAgt
     C***********        Except    Headcol
     C***********        Endif
      ***** END DEL ***** 05/20/04 ************************************
      *
     C                   Endif
      *
      * Check for agent number break
      *
      ***** BEG CHG ***** 05/06/04 ************************************
     C***********        If        QQpsrt = 'AN' and
     C***********                  (Ebagt# <> Svagt# or
     C                   If        (QQpsrt = 'AN' AND Ebagt# <> SvAgt#) OR
     C                             (QQssrt = 'AN' AND Ebagt# <> SvAgt#) OR
     C                             (QQssrt = 'AN' AND XxUwtr <> SvUwtr  AND
     C                             QQpsrt = 'UW') OR
      ***** END CHG ***** 05/06/04 ************************************
     C                             Ebfnd <> Svfnd
      *
     C                   If        Not First
      *
      ***** BEG CHG ***** 05/20/04 ************************************
     C********           If        Q4Lin# > 46
     C                   If        Q4Lin# > 46 AND totempa <> *zeros
      ***** END CHG ***** 05/20/04 ************************************
      ***** BEG DEL ***** 04/24/14 ************************************
     C*********          Except    Head
     C*********          Except    HeadGrp
      ***** BEG ADD ***** 05/06/04 ************************************
     C*********          Except    HeadUND
      ***** END ADD ***** 05/06/04 ************************************
     C*********          Except    Headagt
     C*********          Except    Headcol
      ***** END DEL ***** 04/24/14 ************************************
      ***** BEG ADD ***** 07/14/14 ************************************
      /Free
       If xxDTL = 'Y';
          Except Head;
          Except HeadGrp;
          Except HeadUND;
          Except Headagt;
          Except Headcol;
       Endif;
      /End-Free
      ***** END ADD ***** 07/14/14 ************************************
     C                   Endif
      *
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   If        totempa <> *zeros
      ***** END ADD ***** 05/06/04 ************************************
     C                   Eval(h)   Dbcra = Totdbcra / Totempa
     C                   Eval(h)   Expmoda = Totexpmoda / Totempa
      ***** BEG DEL ***** 05/20/04 ************************************
      ***** BEG ADD ***** 05/06/04 ************************************
     C*********          Endif
      ***** END ADD ***** 05/06/04 ************************************
      ***** END DEL ***** 05/20/04 ************************************
     C                   If        Prempa <> *Zero
     C                   Eval(h)   Dbcrpra = Prdbcra / Prempa
     C                   Eval(h)   Expmodpra = Prexpmoda / Prempa
     C                   Endif
     C                   Except    Totalag
      ***** BEG ADD ***** 05/20/04 ************************************
     C                   Eval      PrtNwHd = *ON
     C                   Endif
      ***** END ADD ***** 05/20/04 ************************************
      *
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Clear                   DBCRA
     C                   Clear                   EXPMODA
     C                   Clear                   DBCRPRA
     C                   Clear                   EXPMODPRA
     C                   Clear                   PRDBCRA
     C                   Clear                   PREXPMODA
     C                   Clear                   PREMPA
     C                   Clear                   Totdbcra
     C                   Clear                   Totempa
     C                   Clear                   Totexpmoda
     C                   Clear                   Totpola
     C                   Clear                   Totpprema
     C                   Clear                   Totprema
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Endif
      *
     C                   Eval      Svagt# = Ebagt#
     C                   Eval      Svanam = Ebanam
     C                   Eval      *Inof = *Off
      *
     C                   Clear                   Totdbcra
     C                   Clear                   Totempa
     C                   Clear                   Totexpmoda
     C                   Clear                   Totpola
     C                   Clear                   Totpprema
     C                   Clear                   Totprema
      *
      ***** BEG DEL ***** 05/20/04 ************************************
     C**************     If        Ebfnd = Svfnd and
     C**************               Not %Eof(Wxebl)
     C************       If        (QQpsrt = 'UW' AND Xxuwtr = SvUwtr) or
     C************                 Ebfnd <> Svfnd
     C************       Except    Head
     C************       Except    HeadGrp
      ***** BEG ADD ***** 05/06/04 ************************************
     C************       Except    HeadUND
      ***** END ADD ***** 05/06/04 ************************************
     C***********        Except    HeadAgt
     C***********        Except    Headcol
     C***********        Endif
     C***********        Endif
      ***** END DEL ***** 05/20/04 ************************************
      *
     C                   Endif
      *
      ***** BEG ADD ***** 05/06/04 ************************************
      *
      * Check for Underwriter ID break
      *
     C                   If        (QQpsrt = 'UW' AND Xxuwtr <> SvUwtr) or
     C                             (Ebfnd <> Svfnd AND qqpsrt = 'UW')
      *
     C                   If        Not First
      *
      *
      ***** BEG CHG ***** 05/20/04 ************************************
     C************       If        Q4Lin# > 46
     C                   If        Q4Lin# > 46 AND Totempu <> *zeros
      ***** END CHG ***** 05/20/04 ************************************
      ***** BEG DEL ***** 04/24/14 ************************************
     C*********          Except    Head
     C*********          Except    HeadGrp
     C*********          Except    HeadUND
     C*********          Except    Headagt
     C*********          Except    Headcol
      ***** END DEL ***** 04/24/14 ************************************
      ***** BEG ADD ***** 07/14/14 ************************************
      /Free
       If xxDTL = 'Y';
          Except Head;
          Except HeadGrp;
          Except HeadUND;
          Except Headagt;
          Except Headcol;
       Endif;
      /End-Free
      ***** END ADD ***** 07/14/14 ************************************
     C                   Endif
      *
      *
      ***** BEG ADD ***** 05/20/04 ************************************
      * FIX DIVIDE BY ZERO -
     C                   If        Totempu <> *zeros
      ***** END ADD ***** 05/20/04 ************************************
     C                   Eval(h)   Dbcru = Totdbcru / Totempu
     C                   Eval(h)   Expmodu = Totexpmodu / Totempu
     C                   If        Prempu <> *Zero
     C                   Eval(h)   Dbcrpru = Prdbcru / Prempu
     C                   Eval(h)   Expmodpru = Prexpmodu / Prempu
     C                   Endif
     C                   Except    Totaluw
      ***** BEG ADD ***** 05/20/04 ************************************
     C                   Eval      PrtNwHd = *ON
     C                   endIF
      ***** END ADD ***** 05/20/04 ************************************
     C                   Clear                   Totdbcru
     C                   Clear                   Totempu
     C                   Clear                   Totexpmodu
     C                   Clear                   Totpolu
     C                   Clear                   Totppremu
     C                   Clear                   Totpremu
     C                   Clear                   DBCRU
     C                   Clear                   EXPMODU
     C                   Clear                   DBCRPRU
     C                   Clear                   EXPMODPRU
     C                   Clear                   PRDBCRU
     C                   Clear                   PREXPMODU
     C                   Clear                   PREMPU
      ***** END ADD ***** 05/06/04 ************************************
      *
     C                   Endif
      *
     C                   Eval      Svagt# = Ebagt#
     C                   Eval      Svanam = Ebanam
     C                   Eval      SvUwtr = XxUwtr
     C                   Eval      EmUwtr = XxUwtr
      *
     C     Wmupl1_key    Chain     Wmupl1
     C                   If        %Found
     C                   Eval      svuwnm = Upusnm
     C                   ELSE
     C                   Eval      svuwnm = *blanks
     C                   Endif
      *
     C                   Eval      *Inof = *Off
      *
      ***** BEG DEL ***** 05/20/04 ************************************
     C*************      If        Ebfnd = Svfnd and
     C*************                Not %Eof(Wxebl)
     C*************      Except    Head
     C*************      Except    HeadGrp
     C*************      Except    HeadUND
     C*************      Except    HeadAgt
     C*************      Except    Headcol
     C*************      Endif
      ***** END DEL ***** 05/20/04 ************************************
      *
     C                   Endif
      *
      ***** END ADD ***** 05/06/04 ************************************
      *
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   If        Not First
      ***** END ADD ***** 05/06/04 ************************************

     C                   If        Ebfnd  <> Svfnd
      *
      ***** BEG ADD ***** 12/14/04 *********************************************
      * If don't have any totals but printed a detail line, print fnd total Page
     C                   If        Scpfb <> 'F' and
     C                             Totempa  = *Zero AND
     C                             DetlCnt  > *Zero
     C                   Except    Head
     C                   Except    HeadGrp
     C                   Except    Totalfnd
      *
      ***** BEG ADD ***** 02/03/05 ************************************
     C                   Eval      Groupcnt = Groupcnt + 1
      ***** END ADD ***** 02/03/05 ************************************
     C                   Clear                   Totempf
     C                   Clear                   Totpolf
     C                   Clear                   Totpremf
     C                   Clear                   Totppremf
     C                   Clear                   Dbcrf
     C                   Clear                   Dbcrprf
     C                   Clear                   Expmodf
     C                   Clear                   Expmodprf
     C                   Eval      PrtNwHd = *ON
     C                   Eval      *Inof = *Off
     C                   Z-ADD     *zeros        DetlCnt
     C                   Endif
      ***** END ADD ***** 12/14/04 *********************************************
      *
     C                   If        Scpfb <> 'F' and
     C                             Totempf <> *Zero
     C                   Eval(h)   Dbcrf = Totdbcrf / Totempf
     C                   If        Prempf <> *Zero
     C                   Eval(h)   Dbcrprf = Prdbcrf / Prempf
     C                   Eval(h)   Expmodprf = Prexpmodf / Prempf
     C                   Endif
      ***** BEG DEL ***** 05/20/04 ************************************
     C***********        Endif
      ***** END DEL ***** 05/20/04 ************************************
      *
     C                   Except    Head
     C                   Except    HeadGrp
     C                   Eval(h)   Dbcrf = Totdbcrf / Totempf
     C                   Eval(h)   Expmodf = Totexpmodf / Totempf
     C                   If        Prempf <> *Zero
     C                   Eval(h)   Dbcrprf = Prdbcrf / Prempf
     C                   Eval(h)   Expmodprf = Prexpmodf / Prempf
     C                   Endif
     C                   Except    Totalfnd
      *
      ***** BEG ADD ***** 02/03/05 ************************************
     C                   Eval      Groupcnt = Groupcnt + 1
      ***** END ADD ***** 02/03/05 ************************************
      *
      ***** BEG ADD ***** 12/14/04 *********************************************
     C                   Z-ADD     *zeros        DetlCnt
      ***** END ADD ***** 12/14/04 *********************************************
      *
      ***** BEG ADD ***** 05/20/04 ************************************
     C                   Eval      PrtNwHd = *ON
     C                   Eval      *Inof = *Off
     C                   Endif
      ***** END ADD ***** 05/20/04 ************************************
     C                   Eval      Svfnd  = Ebfnd
      *
      ****** BEG ADD ***** 11/01/04 ***************************************
      * Chain out to get new fund name after printing total page
     C     Wmfnl_Key     Chain     Wmfnl
     C                   Movel(p)  Fnname        Nbfnnm
      ****** END ADD ***** 11/01/04 ***************************************
      *
     C                   Eval      Svfnnm = Fnname
      *
     C                   Clear                   Totdbcrf
     C                   Clear                   Totempf
     C                   Clear                   Totexpmodf
     C                   Clear                   Totpolf
     C                   Clear                   Totppremf
     C                   Clear                   Totpremf
      ****** BEG ADD ***** 02/05/04 ****************************************
     C                   Clear                   prexpmod
     C                   Clear                   prdbcrf
     C                   Clear                   premp
     C                   Clear                   prempf
      ****** END ADD ***** 02/05/04 ****************************************
      *
      ***** BEG DEL ***** 05/20/04 ************************************
     C**********         Except    Head
     C*********          Except    HeadGrp
      ***** BEG ADD ***** 05/06/04 ************************************
     C**********         Except    HeadUND
      ***** END ADD ***** 05/06/04 ************************************
     C**********         Except    HeadAgt
     C*********          Except    Headcol
      ***** END DEL ***** 05/20/04 ************************************
     C                   Endif
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Endif
      ***** END ADD ***** 05/06/04 ************************************
      *
     C                   Endif
      *
     C                   Endsr                                                  End Print_Tots
      *
      ***** END ADD ***** 02/13/04 ******************************************
      *
      **********************************************************************
      * Get Master File
      **********************************************************************
      *
      * ==================== *
     C     Get_Wmemp     Begsr
      * ==================== *
      *
      ******* beg add ****** 10/13/03 *********************************
     C     Wmeml_Key     Chain     Wmemlq

     C                   If        not (%Found)
      ******* end add ****** 10/13/03 *********************************
     C     Wmeml_Key     Chain     Wmeml
      ******* beg add ****** 10/13/03 *********************************
     C                   Endif
      ******* end add ****** 10/13/03 *********************************
      *
      ******* beg chg ****** 10/13/03 *********************************
     C***********        If        %Found(Wmeml)
     C                   If        %Found
      ******* end chg ****** 10/13/03 *********************************
     C                   Eval      Nbuwtr = Emuwtr
      *
     C                   Select
      *
     C                   When      Emcad5 > *Zero
      ******* beg del ****** 10/13/03 *********************************
     C***  *Cymd         Move      Emcad5        Datusa
     C***                Move      Datusa        Nbcxdt
      ******* end del ****** 10/13/03 *********************************
      ******* beg add ****** 10/13/03 *********************************
     C                   z-add     Emcad5        SRDTI
     C                   Call      'SRCVTDAT'    DTMDYY
     C                   Movel     SRDTO         Nbcxdt            8 0
      ******* end add ****** 10/13/03 *********************************
      *
     C                   When      Emcad4 > *Zero
      ******* beg del ****** 10/13/03 *********************************
     C***  *Cymd         Move      Emcad4        Datusa
     C***                Move      Datusa        Nbcxdt
      ******* end del ****** 10/13/03 *********************************
      ******* beg add ****** 10/13/03 *********************************
     C                   z-add     Emcad4        SRDTI
     C                   Call      'SRCVTDAT'    DTMDYY
     C                   Movel     SRDTO         Nbcxdt            8 0
      ******* end add ****** 10/13/03 *********************************
      *
     C                   When      Emcad3 > *Zero
      ******* beg del ****** 10/13/03 *********************************
     C***  *Cymd         Move      Emcad3        Datusa
     C***                Move      Datusa        Nbcxdt
      ******* end del ****** 10/13/03 *********************************
      ******* beg add ****** 10/13/03 *********************************
     C                   z-add     Emcad3        SRDTI
     C                   Call      'SRCVTDAT'    DTMDYY
     C                   Movel     SRDTO         Nbcxdt            8 0
      ******* end add ****** 10/13/03 *********************************
      *
     C                   When      Emcad2 > *Zero
      ******* beg del ****** 10/13/03 *********************************
     C***  *Cymd         Move      Emcad2        Datusa
     C***                Move      Datusa        Nbcxdt
      ******* end del ****** 10/13/03 *********************************
      ******* beg add ****** 10/13/03 *********************************
     C                   z-add     Emcad2        SRDTI
     C                   Call      'SRCVTDAT'    DTMDYY
     C                   Movel     SRDTO         Nbcxdt            8 0
      ******* end add ****** 10/13/03 *********************************
      *
     C                   When      Emcad1 > *Zero
      ******* beg del ****** 10/13/03 *********************************
     C***  *Cymd         Move      Emcad1        Datusa
     C***                Move      Datusa        Nbcxdt            8 0
      ******* end del ****** 10/13/03 *********************************
      ******* beg add ****** 10/13/03 *********************************
     C                   z-add     Emcad1        SRDTI
     C                   Call      'SRCVTDAT'    DTMDYY
     C                   Movel     SRDTO         Nbcxdt            8 0
      ******* end add ****** 10/13/03 *********************************
      *
     C                   Other
     C                   Eval      Nbcxdt = *Zero
     C                   Endsl
      *
      ****** BEG ADD ***** 02/05/04 ****************************************
      * If state reporting division, save division, else loop until get it
      *
     C                   If        emstrp = 'Y'
     C                   eval      strpdiv = emdiv
     C                   else
     C                   clear                   strpdiv
      *
      ***** BEG CHG ***** 12/14/04 *********************************************
      * Change to position at first division and read from there
     C                   z-add     *zeros        ZZdiv
     C*****Wmeml_Key3    chain     wmeml                              60
     C     Wmeml_Key3    setll     wmeml
     C     Wmeml_Key3    reade     wmeml                                  60
      ***** END CHG ***** 12/14/04 *********************************************
      *
     C                   dou       *in60
     C                   If        emstrp  = 'Y'
     C                   eval      strpdiv = emdiv
      ***** BEG CHG ***** 12/14/04 *********************************************
      * Check for State Reporting Division in the input date range. If not, load
      * the information for calculations here.
     C
     C                   If        emefd1 < Fromdt
     C************       Eval      EMempCnt = EMempCnt  + 1
     C                   EndIf
      ***** END CHG ***** 12/14/04 *********************************************
     C                   leave
     C                   endif
      ***** BEG CHG ***** 12/14/04 *********************************************
      *
     C*****Wmeml_Key3    reade     wmeml                                  60
     C     Wmeml_Key4    reade     wmeml                                  60
      ***** END CHG ***** 12/14/04 *********************************************
     C                   enddo
      *
     C                   endif
      *
      ****** END ADD ***** 02/05/04 ****************************************
      *
     C     Wmupl1_key    Chain     Wmupl1
      *
     C                   If        %Found
     C                   Eval      Nbuwex = Upext#
     C                   Eval      Uwname = Upusnm
     C                   Movel(p)  Upusnm        Nbuwnm
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Eval      svuwnm = Upusnm
     C                   ELSE
     C                   Eval      svuwnm = *blanks
     C                   Eval      Nbuwnm = *blanks
      ***** END ADD ***** 05/06/04 ************************************
     C                   Endif
      *
     C                   If        First
     C                   Eval      Svagt# = Ebagt#
     C                   Eval      Svanam = Ebanam
     C                   Eval      Svfnd  = Ebfnd
     C                   Eval      Svfnnm = Fnname
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Eval      Svuwtr = XxUwtr
      ***** END ADD ***** 05/06/04 ************************************
      ***** BEG DEL ***** 04/24/14 ************************************
     C*********          Except    Head
     C*********          Except    HeadGrp
      ***** BEG ADD ***** 05/06/04 ************************************
     C*********          Except    HeadUND
      ***** END ADD ***** 05/06/04 ************************************
     C*********          Except    Headagt
     C*********          Except    Headcol
      ***** END DEL ***** 04/24/14 ************************************
      ***** BEG ADD ***** 07/14/14 ************************************
      /Free
       If xxDTL = 'Y';
          Except Head;
          Except HeadGrp;
          Except HeadUND;
          Except Headagt;
          Except Headcol;
       Endif;
      /End-Free
      ***** END ADD ***** 07/14/14 ************************************
     C                   Endif
      *
     C                   Exsr      Main
     C                   Eval      First = *Off
     C                   Endif
      *
     C                   Endsr
      *
      /eject
      **********************************************************************
      * Get Agent Fein
      **********************************************************************
      *
      * ==================== *
     C     Agent_Fein    Begsr
      * ==================== *
      *
     C     Wmaal1_Key    CHAIN     Wmaal1
      *
     C                   If        Not %Found
     C                   Eval      AaFein = *Zero
     C                   Else
     C                   Eval      NbFein  = AaFein
     C                   Endif
      *
     C                   Endsr
      *
      /eject
      **********************************************************************
      * Active or Pending
      **********************************************************************
      *
      * ==================== *
     C     ActivePend    Begsr
      * ==================== *
      *
     C                   Eval      Active = *Off
     C                   Eval      Pending = *Off
      *
     C     Wdehl12_Key   Setgt     Wdehl12
     C     Wdehl12_Key   Readpe    Wdehl12
      *
     C                   If        Not %Eof
     C                   Eval      Active = *On
     C                   Eval      Nbapfl = 'A'
      *
     C                   Else
     C                   Eval      Pending = *On
     C                   Eval      Nbapfl = 'P'
     C                   Endif
      *
      ******* beg add ****** 10/13/03 *********************************
     C                   If        (emrqfl = 'Y')
     C                   Eval      Nbrqlp = 'R'
     c                   else
     C                   Eval      Nbrqlp = 'L'
     C                   Endif
      ******* end add ****** 10/13/03 *********************************
     C                   Endsr
      *
      /eject
      **********************************************************************
      * New or Renewal
      **********************************************************************
      *
      * ==================== *
     C     NewRenew      Begsr
      * ==================== *
      *
     C                   Move      *Off          New
     C                   Move      *Off          Renewal
      ***** BEG ADD ***** 06/14/05 ****************************************
      * Clear before determine New or Renewal Code
     C                   Eval      Nbnwrw = ' '
      ***** END ADD ***** 06/14/05 ****************************************
      *
     C     Wdehl12_Key   Setgt     Wdehl12
      *
     C                   Dou       %Eof
     C     Wdehl12_Key   Readpe    Wdehl12
      *
     C                   If        Not %Eof
      *
     C                   If        Ehnwrw = 'N'
     C                   Move      *Off          Renewal
     C                   Move      *On           New
     C                   Leave
     C                   Endif
      *
     C                   If        Ehnwrw = 'R'
     C                   Move      *On           Renewal
     C                   Move      *Off          New
     C                   Leave
     C                   Endif
     C                   Else
      *
     C     Wdell_Key3    Chain     Wdell
      *
     C                   If        %Found
     C                   Move      *On           Renewal
     C                   Move      *Off          New
     C                   Else
     C                   Move      *Off          Renewal
     C                   Move      *On           New
     C                   Endif
      *
     C                   Endif
      *
     C                   Enddo
      *
      ***** BEG DEL ***** 06/14/05 ****************************************
      * Deleted this code since have not yet chained to Wzexl File
     C************       If        New and
     C************                 %Found(Wzexl)
     C************       Move      *On           Renewal
     C************       Move      *Off          New
     C************       Endif
      ***** END DEL ***** 06/14/05 ****************************************
      *
     C                   If        New
     C                   Eval      Nbnwrw = 'N'
     C                   Endif
      *
     C                   If        Renewal
     C                   Eval      Nbnwrw = 'R'
     C                   Endif
      *
     C                   Endsr
      *
      /EJECT
      *****************************************************************
      *  Get Issue Date
      *****************************************************************
      *
      * ==================== *
     C     Issue         Begsr
      * ==================== *
      *
     C     Wdehl12_Key   Setll     Wdehl12
      *
     C                   Dou       %Eof
     C     Wdehl12_Key   Reade     Wdehl12                              60
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   If        Ehfaf = 'E' or
     C                             Ehfaf = 'R'
      ******* beg del ****** 10/13/03 *********************************
     C**** *Ymd          Move      Ehtrdt        Datusa
     C**** *Usa          Move      Datusa        Nbisdt
      ******* end del ****** 10/13/03 *********************************
      ******* beg add ****** 10/13/03 *********************************
      *
     C                   z-add     Ehtrdt        SRDTI
     C                   Call      'SRCVTDAT'    DTMDYY
     C                   Movel     SRDTO         Nbisdt            8 0
      *
      ******* end add ****** 10/13/03 *********************************
     C                   Leave
     C                   Endif
      *
     C                   Enddo
      *
     C                   Endsr
      /eject
     **********************************************************************
     *  Get File WMRPL4 for Policy Number
     **********************************************************************
      *
      * ==================== *
     C     Get_Pol#      Begsr
      * ==================== *
      *
      ******* beg add ****** 10/13/03 *********************************
     C     Wmrpl4_KeyQ   Chain     WmrplQ

     C                   If        not (%Found)
      ******* end add ****** 10/13/03 *********************************
     C     Wmrpl4_Key    Chain     Wmrpl4
      ******* beg add ****** 10/13/03 *********************************
     C                   Endif
      ******* end add ****** 10/13/03 *********************************
      *
     C                   If        %Found and
     C                             Rpcap# <> *blank
     C                   Eval      Nbcap# = Rpcap#
     C                   Eval      Nbcarr = Rpcarr
     C                   Else
     C                   Clear                   Nbcap#
     C                   Clear                   Nbcarr
     C                   Endif
      *
     C                   Endsr
      /eject
     **********************************************************************
     *  Get Prior Policy information
     **********************************************************************
      *
      * ==================== *
     C     Proc_Prior    Begsr
      * ==================== *
      *
      *  Get prior policy number
      *
     C     Wmrpl4_KeyPr  Chain     Wmrpl4
      *
     C                   If        %Found and
     C                             Rpcap# <> *blank
     C                   Eval      Prcap# = Rpcap#
     C                   Else
     C                   Clear                   Prcap#
     C                   Endif
      *
      *  GET PRIOR ANNUAL PREM & EXP MOD FROM EMP DETAIL
      *
     C                   Z-Add     0             Premex            5 4
     C                   Z-Add     0             Prdbcr
      ***** BEG ADD ***** 08/06/03 ***********************************
     C                   Z-Add     0             Xpemex
     C                   Z-Add     0             Xpdbcr
      ***** end ADD ***** 08/06/03 ***********************************
      *
      **********************************************************************
      *   Process prior year detail
      *
     C                   Eval      *In70 = *On
      ******* beg del ****** 10/13/03 *********************************
     C**** *Ymd          Move      Elpfdt        Pfdate
     C**** *Usa          Move      Pfdate        Prpfdt            8 0
     C**** *Ymd          Move      Elptdt        Ptdate
     C**** *Usa          Move      Ptdate        Prptdt            8 0
      ******* end del ****** 10/13/03 *********************************
      ******* beg add ****** 10/13/03 *********************************
      *
     C                   z-add     Elpfdt        SRDTI
     C                   Call      'SRCVTDAT'    DTMDYY
     C                   Movel     SRDTO         Prpfdt            8 0
      *
     C                   z-add     Elptdt        SRDTI
     C                   Call      'SRCVTDAT'    DTMDYY
     C                   Movel     SRDTO         Prptdt            8 0
      *
      ******* end add ****** 10/13/03 *********************************
      *
     C                   If        Elemd1 <> *Zero
     C                   Z-add     Elexm1        Premex            5 4
     C                   Endif
      *
     C                   If        Elemd2 <> *Zero
     C                   Z-Add     Elexm2        Premex
     C                   Endif
      *
     C                   If        Elemd3 <> *Zero
     C                   Z-Add     Elexm3        Premex
     C                   Endif
      *
      ****** BEG ADD ***** 02/05/04 ****************************************
      * If state reporting division, do the accumulations, else bypass
     C                   if        ebdiv  = strpdiv
      ****** END ADD ***** 02/05/04 ****************************************
      *
     C                   If        Premex > *Zero
     C                   Eval      Prexpmod  = Premex + Prexpmod
     C                   Eval      Prexpmoda = Premex + Prexpmoda
     C                   Eval      Prexpmodf = Premex + Prexpmodf
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Eval      Prexpmodu = Premex + Prexpmodu
      ***** END ADD ***** 05/06/04 ************************************
      *
      ***** BEG ADD ***** 08/06/03 ***********************************
     C                   Z-add     PREMEX        Xpemex
      ***** END ADD ***** 08/06/03 ***********************************
     C                   Else
     C                   Eval      Prexpmod  = 1 + Prexpmod
     C                   Eval      Prexpmoda = 1 + Prexpmoda
     C                   Eval      Prexpmodf = 1 + Prexpmodf
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Eval      Prexpmodu = 1 + Prexpmodu
      ***** END ADD ***** 05/06/04 ************************************
     C                   Endif
      *
     C                   If        Eldbcr   > *Zero
      *
      ***** END ADD ***** 08/06/03 ***********************************
     C                   Z-add     Eldbcr        Xpdbcr
      ***** END ADD ***** 08/06/03 ***********************************
      *
      ***** BEG CHG ***** 08/20/03 *****************************************
     C*************      Eval      Prdbcr  = Eldbcr + Prdbcr
     C                   Eval      Prdbcrt = Eldbcr + Prdbcrt
      ***** END CHG ***** 08/20/03 *****************************************
     C                   Eval      Prdbcra = Eldbcr + Prdbcra
     C                   Eval      Prdbcrf = Eldbcr + Prdbcrf
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Eval      Prdbcru = Eldbcr + Prdbcru
      ***** END ADD ***** 05/06/04 ************************************
      *
     C                   Else
      ***** BEG CHG ***** 08/20/03 *****************************************
     C***************    Eval      Prdbcr  = 1 + Prdbcr
     C                   Eval      Prdbcrt = 1 + Prdbcrt
      ***** END CHG ***** 08/20/03 *****************************************
     C                   Eval      Prdbcra = 1 + Prdbcra
     C                   Eval      Prdbcrf = 1 + Prdbcrf
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Eval      Prdbcru = 1 + Prdbcru
      ***** END ADD ***** 05/06/04 ************************************
     C                   Endif
      *
     C                   Eval      Premp  = Premp  + 1
     C                   Eval      Prempa = Prempa + 1
     C                   Eval      Prempf = Prempf + 1
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Eval      Prempu = Prempu + 1
      ***** END ADD ***** 05/06/04 ************************************
      *
      ****** BEG ADD ***** 02/05/04 ****************************************
     C                   endif                                                  ebdiv = strpdiv
      ****** END ADD ***** 02/05/04 ****************************************
      *
     C                   If        Elfaf = 'A'
     C                   Eval      Nbprmp = Elaapr
     C                   Else
     C                   Eval      Nbprmp = Elanpr
     C                   Endif
      *
      **** Begin Change *** 07/16/03 ***************************************
     C**********         If        Scexpc = 'Y' and
     C                   If        Scexpc <> 'Y' and
     C                             Elanpr <> *Zero
     C**********         Eval      Nbprmp = Nbprmp + Elprtf
     C                   Eval      Nbprmp = Nbprmp - Elprtf
      **** End   Change *** 07/16/03 ***************************************
     C                   Endif
      *
     C                   Endsr
      *
      *
      *
     **********************************************************************
     *  Process current year detail file
     **********************************************************************
      *
      * ==================== *
     C     Proc_Curr     Begsr
      * ==================== *
      *
      **********************************************************************
      *
     C                   Z-Add     0             Xxemex            5 4
      *
      ******* beg add ****** 10/13/03 *********************************
     C     Wdell_Key     Chain     Wdellq

     C                   If        not %found
      ******* end add ****** 10/13/03 *********************************
     C     Wdell_Key     Chain     Wdell
      ******* beg add ****** 10/13/03 *********************************
     C                   Endif
      ******* end add ****** 10/13/03 *********************************
      *
     C                   If        %Found
      *
     C                   If        Elemd1 <> *Zero
     C                   Z-Add     ELEXM1        NBEXM1
     C                   Z-Add     ELEMD1        NBEMD1
     C                   Z-Add     ELEXM1        XXEMEX
     C                   Endif
      *
     C                   If        Elemd2 <> *Zero
     C                   Z-Add     ELEXM2        NBEXM2
     C                   Z-Add     ELEMD2        NBEMD2
     C                   Z-Add     ELEXM2        XXEMEX
     C                   Endif
      *
     C                   If        Elemd3 <> *Zero
     C                   Z-Add     ELEXM3        NBEXM3
     C                   Z-Add     ELEMD3        NBEMD3
     C                   Z-Add     ELEXM3        XXEMEX
     C                   Endif
      *
      ********* Beg Add ****** 11/20/13 **************************************
      /Free
         xcDbCr = ELdbcr;
         xcNwRw = ELnwrw;
      /End-Free
      ******** End Add ****** 11/20/13 ***************************************
     C                   Eval      Nbdbcr = Eldbcr
     C                   If        Elfaf = 'A'
     C                   Eval      Nbprmr = Elaapr
     C                   Else
     C                   Eval      Nbprmr = Elanpr
     C                   Endif
      *
      **** Begin Change *** 07/16/03 ***************************************
     C**********         If        Scexpc = 'Y' and
     C                   If        Scexpc <> 'Y' and
     C                             Elanpr <> *Zero
     C**********         Eval      Nbprmr = Nbprmr + Elprtf
     C                   Eval      Nbprmr = Nbprmr - Elprtf
      **** End   Change *** 07/16/03 ***************************************
     C                   Endif
      *
     C                   Endif
      *
     C                   Endsr
      /eject
     ** ================================================================ **
     *  Check Emp cross reference file
     *  This routine isa called because a previous year detail record
     *  was not found for the employer/fund.
     ** =================== *
     C     Proc_Wzexl    Begsr
      ** =================== *
      *
      * Get employer master file for old number
      *
     C     Wzexl_Key     Chain     Wzexl
      *
     C                   If        %Found
     C                   Move      *On           Renewal
     C                   Move      *Off          New
      *
      ***** BEG DEL ***** 06/09/05 **********************************
      * Chg to always use code regardless of match on Group
     C****************   If        Exofnd = Exfnd
      ***** END DEL ***** 06/09/05 **********************************
      *
      ***** BEG ADD ***** 06/14/05 ****************************************
      * If is a renewal and selection is New (*IN51 = *ON) bypass
     C                   If        Renewal and NOT *IN51
      ***** END ADD ***** 06/14/05 ****************************************
      *
     C                   Eval      *In72 = *On
     C     Wmeml_Key2    Chain     Wmeml
      *
     C                   If        %Found(Wmeml)
     C                   Movel     Emname        Prenamx          26
     C                   Endif
      *
     C                   Eval      Nbpfnd = Exofnd
     C                   Eval      Nbpemp = Exoemp
     C                   Eval      Nbpdiv = Exodiv
     C     Wmrpl4_Key2   Chain     Wmrpl4
      *
     C                   If        %Found and
     C                             Rpcap# <> *blank
     C                   Eval      Nbpcap# = Rpcap#
     C                   Endif
      *
      **********************************************************************
      * Get prior year detail record
      *  GET PRIOR ANNUAL PREM & EXP MOD FROM EMP DETAIL
      *
     C                   Z-Add     0             Premex            5 4
      ***** BEG ADD ***** 08/06/03 ***********************************
     C                   Z-Add     0             Xpemex
     C                   Z-Add     0             Xpdbcr
      ***** end ADD ***** 08/06/03 ***********************************
      *
      ***** BEG CHG ***** 06/09/05 **********************************
      * Use the Retrieved key value to get the premium amounts
     C*****Wdell_Key4    Chain     Wdell
     C     Wdell_Key5    Chain     Wdell
      ***** END CHG ***** 06/09/05 **********************************
      *
     C                   If        %Found
     C                   Eval      *In70 = *On
      ******* beg del ****** 10/13/03 *********************************
     C**** *Ymd          Move      Elpfdt        Pfdate
     C**** *Usa          Move      Pfdate        Prpfdt            8 0
     C**** *Ymd          Move      Elptdt        Ptdate
     C**** *Usa          Move      Ptdate        Prptdt            8 0
      ******* end del ****** 10/13/03 *********************************
      ******* beg add ****** 10/13/03 *********************************
      *
     C                   z-add     Elpfdt        SRDTI
     C                   Call      'SRCVTDAT'    DTMDYY
     C                   Movel     SRDTO         Prpfdt            8 0
      *
     C                   z-add     Elptdt        SRDTI
     C                   Call      'SRCVTDAT'    DTMDYY
     C                   Movel     SRDTO         Prptdt            8 0
      *
      ******* end add ****** 10/13/03 *********************************
     C                   Z-add     Eldbcr        Prdbcr
      *
     C                   If        Elemd1 <> *Zero
     C                   Z-add     Elexm1        Premex            5 4
     C                   Endif
      *
     C                   If        Elemd2 <> *Zero
     C                   Z-Add     Elexm2        Premex
     C                   Endif
      *
     C                   If        Elemd3 <> *Zero
     C                   Z-Add     Elexm3        Premex
     C                   Endif
      *
     C                   If        Premex > *Zero
      ***** BEG ADD ***** 08/06/03 ***********************************
     C                   Z-add     Premex        Xpemex
      ***** END ADD ***** 08/06/03 ***********************************
     C                   Eval      Prexpmod  = Premex + Prexpmod
     C                   Eval      Prexpmoda = Premex + Prexpmoda
     C                   Eval      Prexpmodf = Premex + Prexpmodf
     C                   Else
     C                   Eval      Prexpmod  = 1 + Prexpmod
     C                   Eval      Prexpmoda = 1 + Prexpmoda
     C                   Eval      Prexpmodf = 1 + Prexpmodf
     C                   Endif
      *
     C                   If        Eldbcr   > *Zero
      ***** BEG ADD ***** 08/06/03 ***********************************
     C                   Z-add     Eldbcr        Xpdbcr
      ***** END ADD ***** 08/06/03 ***********************************
      *
      ***** BEG CHG ***** 08/20/03 *****************************************
     C************       Eval      Prdbcr  = Eldbcr + Prdbcr
     C                   Eval      Prdbcrt = Eldbcr + Prdbcrt
      ***** END CHG ***** 08/20/03 *****************************************
     C                   Eval      Prdbcra = Eldbcr + Prdbcra
     C                   Eval      Prdbcrf = Eldbcr + Prdbcrf
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Eval      Prdbcru = Eldbcr + Prdbcru
      ***** END ADD ***** 05/06/04 ************************************
      *
     C                   Else
      ***** BEG CHG ***** 08/20/03 *****************************************
     C************       Eval      Prdbcr  = 1 + Prdbcr
     C                   Eval      Prdbcrt = 1 + Prdbcrt
      ***** END CHG ***** 08/20/03 *****************************************
     C                   Eval      Prdbcra = 1 + Prdbcra
     C                   Eval      Prdbcrf = 1 + Prdbcrf
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Eval      Prdbcru = 1 + Prdbcru
      ***** END ADD ***** 05/06/04 ************************************
     C                   Endif
      *
     C                   Eval      Premp  = Premp  + 1
     C                   Eval      Prempa = Prempa + 1
     C                   Eval      Prempf = Prempf + 1
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Eval      Prempu = Prempu + 1
      ***** END ADD ***** 05/06/04 ************************************
      *
     C                   If        Elfaf = 'A'
     C                   Eval      Nbprmp = Elaapr
     C                   Else
     C                   Eval      Nbprmp = Elanpr
     C                   Endif
      *
      **** Begin Change *** 07/16/03 ***************************************
     C**********         If        Scexpc = 'Y' and
     C                   If        Scexpc <> 'Y' and
     C                             Elanpr <> *Zero
     C**********         Eval      Nbprmp = Nbprmp + Elprtf
     C                   Eval      Nbprmp = Nbprmp - Elprtf
      **** End   Change *** 07/16/03 ***************************************
     C                   Endif                                                  If Scexpc = Y
      *
     C                   Endif                                                  If found(Wdell_Key5)
      *
     C                   Endif                                                  If Not *IN51
      *
     C                   Endif                                                  If found(Wzexl)
      *
     C                   Endsr
      *
      ******* beg del ****** 10/13/03 *********************************
      *****************************************************************
      ***Build*Wfnbp                   ** SUBROUTINE **   S R 1 S T   *
      *****************************************************************
      ***********
     C*****Write_Wfnbp   Begsr
      ***********
     C***********        Write     Pnb01
      ***********
     C***********        Endsr
      ******* end del ****** 10/13/03 *********************************
      *
      *
      *
      *****************************************************************
      *  INITIALIZATION                ** SUBROUTINE **   S R 1 S T   *
      *****************************************************************
      *
     C     *Inzsr        Begsr
      *
      *   KEY LIST FOR THE COMPANY FILE
      *
     C     Smcol_Key     Klist
     C                   Kfld                    Xxco#
      *
      *   KEY LIST FOR THE USER FILE (FOR UNDERWRITER NAME)
      *
     C     Wmupl1_Key    Klist
     C                   Kfld                    Emuwtr
      *
      *   KEY LIST FOR THE USER PARAMETER FILE
      *
     C     Wmupl_Key     Klist
     C                   Kfld                    Q1user
      *
      *   KEY LIST FOR THE OVERRIDE FILE
      *
     C     Smorl_Key     Klist
     C                   Kfld                    Q1pgm
     C                   Kfld                    Xxprt             1
      *
      *  FUND FILE
      *
     C     Wmfnl_Key     Klist
     C                   Kfld                    Xxco#
     C                   Kfld                    Ebfnd
      *
      *   KEY LIST FOR THE WORK FILE
      *
     C     Wxnbp_Key     Klist
     C                   Kfld                    Ebco#
     C                   Kfld                    Ebfnd
     C                   Kfld                    Ebemp#
     C                   Kfld                    Ebfyr
      *
     C     Wxnbp1_Key    Klist
     C                   Kfld                    Ebco#
     C                   Kfld                    Ebfnd
     C                   Kfld                    Ebemp#
     C                   Kfld                    Ebfyr
     C                   Kfld                    Ebagt#
      *
     C     Wxnbp2_Key    Klist
     C                   Kfld                    Ebco#
     C                   Kfld                    Ebfnd
     C                   Kfld                    Ebemp#
     C                   Kfld                    Ebfyr
     C                   Kfld                    Ebanam
      *
      *   KEY LIST FOR THE EMPLOYER FILE
      *
     C     Wmeml_Key     Klist
     C                   Kfld                    Ebco#
     C                   Kfld                    Ebfnd
     C                   Kfld                    Ebemp#
     C                   Kfld                    Ebdiv
      *
     C     Wmeml_Key2    Klist
     C                   Kfld                    Exco#
     C                   Kfld                    Exofnd
     C                   Kfld                    Exoemp
     C                   Kfld                    Exodiv
      *
      ****** BEG ADD ***** 02/05/04 ****************************************
      *
     C     Wmeml_Key3    Klist
     C                   Kfld                    Ebco#
     C                   Kfld                    Ebfnd
     C                   Kfld                    Ebemp#
      ***** BEG CHG ***** 12/14/04 *********************************************
     C                   Kfld                    ZZdiv
     C*************      Kfld                    Ebdiv
      ***** END CHG ***** 12/14/04 *********************************************
      ****** END ADD ***** 02/05/04 ****************************************
      *
      ***** BEG ADD ***** 12/14/04 *********************************************
      *
     C     Wmeml_Key4    Klist
     C                   Kfld                    Ebco#
     C                   Kfld                    Ebfnd
     C                   Kfld                    Ebemp#
      ***** END ADD ***** 12/14/04 *********************************************
      *
      *  EMPLOYER DETAIL FILE
      *
     C     Wdell_Key     Klist
     C                   Kfld                    Xxco#
     C                   Kfld                    Ebfnd
     C                   Kfld                    Ebemp#
     C                   Kfld                    Ebdiv
     C                   Kfld                    Ebfyr             3 0
      *
     C     Wdell_Key2    Klist
     C                   Kfld                    Xxco#
     C                   Kfld                    Ebfnd
     C                   Kfld                    Ebemp#
     C                   Kfld                    Ebdiv
      *
     C     Wdell_Key3    Klist
     C                   Kfld                    Xxco#
     C                   Kfld                    Ebfnd
     C                   Kfld                    Ebemp#
     C                   Kfld                    Ebdiv
     C                   Kfld                    Prfyr             3 0
      *
     C     Wdell_Key4    Klist
     C                   Kfld                    Xxco#
     C                   Kfld                    Exfnd
     C                   Kfld                    Exemp#
     C                   Kfld                    Exdiv
     C                   Kfld                    Prfyr             3 0
      *
      ***** BEG CHG ***** 06/09/05 **********************************
      *
     C     Wdell_Key5    Klist
     C                   Kfld                    Xxco#
     C                   Kfld                    Exofnd
     C                   Kfld                    Exoemp
     C                   Kfld                    Exodiv
     C                   Kfld                    Prfyr             3 0
      ***** END CHG ***** 06/09/05 **********************************
      *
      *   KEY LIST FOR THE CLAIMS FILE
      *
      * KEY LIST FOR THE AGENT HISTORY MASTER FILE
      *
     C     Wmahl_Key     Klist
     C                   Kfld                    XXCO#
     C                   Kfld                    XXFND
     C                   Kfld                    EBEMP#
     C                   Kfld                    EBDIV
      *
      *  KEY LIST FOR THE AGENT ASSIGNMENT FILE
      *
     C     Wmaal1_Key    Klist
     C                   Kfld                    EBCO#
     C                   Kfld                    EBFND
     C                   Kfld                    EBAGT#
      *
     C     Wmrpl4_Key    Klist
     C                   Kfld                    Ebco#
     C                   Kfld                    Ebfnd
     C                   Kfld                    Ebfyr
     C                   Kfld                    Ebemp#
      ******* beg add ****** 10/13/03 *********************************
      *
     C     Wmrpl4_KeyQ   Klist
     C                   Kfld                    Ebco#
     C                   Kfld                    Ebfnd
     C                   Kfld                    Ebemp#
     C                   Kfld                    Ebdiv
     C                   Kfld                    Ebfyr
      ******* end add ****** 10/13/03 *********************************
      *
     C     Wmrpl4_KeyPr  Klist
     C                   Kfld                    Ebco#
     C                   Kfld                    Ebfnd
     C                   Kfld                    Prfyr
     C                   Kfld                    Ebemp#
      *
     C     Wmrpl4_Key2   Klist
     C                   Kfld                    Exco#
     C                   Kfld                    Exofnd
     C                   Kfld                    Ebfyr
     C                   Kfld                    Exoemp
      *
     C     Wmeal9_Key    Klist
     C                   Kfld                    Nbco#
     C                   Kfld                    Nbfnd
     C                   Kfld                    Nbemp#
      *
      *   KEY LIST FOR WDEHL12
      *
     C     Wdehl12_Key   Klist
     C                   Kfld                    Ebco#
     C                   Kfld                    Ebfnd
     C                   Kfld                    Ebemp#
     C                   Kfld                    Ebdiv
     C                   Kfld                    Ebfyr
     *
     C     Wdehl12_Key2  Klist
     C                   Kfld                    Ebco#
     C                   Kfld                    Ebfnd
     C                   Kfld                    Ebemp#
     C                   Kfld                    Ebdiv
     *
     C     Wzexl_Key     Klist
     C                   Kfld                    Ebco#
     C                   Kfld                    Ebfnd
     C                   Kfld                    Ebemp#
     C                   Kfld                    Ebdiv
      ******* beg add ****** 08/19/10 *********************************
     C     QTKY          Klist
     C                   Kfld                    Ebco#
     C                   Kfld                    Ebfnd
     C                   Kfld                    Ebemp#
     C                   Kfld                    Ebdiv
     C                   Kfld                    Ebacur
     C                   Kfld                    Ebagt#
      *
     C     QTKY1         Klist
     C                   Kfld                    Ebco#
     C                   Kfld                    Ebfnd
     C                   Kfld                    Ebemp#
     C                   Kfld                    Ebdiv
     C                   Kfld                    Ebacur
      ******* END add ****** 08/19/10 *********************************
      ******  Beg- Add ***** 07/20/16 *********************************
     C     FNKY1         KLIST
     C                   KFLD                    XXCO#
      *****************************************************************
      *  KEY LIST FOR FUND WORK FILE
      *****************************************************************
     C     XGKY          KLIST
     C                   KFLD                    GsCO#
     C                   KFLD                    GsFnd
      ******  End- Add ***** 07/20/16 *********************************
     *
     * Key For Agent file Wmaal1
     *
     C     *Like         Define    Ememp#        Xxemp#
     C     *Like         Define    Emdiv         Xxdiv
     C     *Like         Define    Emdt          Xxymd
     C     *Like         Define    Emdt          Currdt
     C     *Like         Define    Upuser        Uwname
     C     *Like         Define    Nbcap#        Prcap#
     C     *Like         Define    Ebagt#        Svagt#
     C     *Like         Define    Ebanam        Svanam
     C     *Like         Define    Ebfnd         Svfnd
     C     *Like         Define    Fnname        Svfnnm
      *
     C     *Entry        Plist
     C                   Parm                    QQCO#             3
     C                   Parm                    QQFND1            3
     C                   Parm                    QQFND2            3
     C                   Parm                    QQFYR             3
     C                   Parm                    QQDTE             8
     C                   Parm                    QQEMP#            9
     C                   Parm                    QQDIV             5
     C                   Parm                    QQfrdt
     C                   Parm                    QQtodt
      ******* beg del ****** 10/13/03 *********************************
     C*********          Parm                    QQtypo
      ******* end del ****** 10/13/03 *********************************
     C                   Parm                    QQagnt
     C                   Parm                    QQprst
     C                   Parm                    QQpsrt
     C                   Parm                    QQssrt
     C                   Parm                    QQstat
     C                   Parm                    QQque            10
     C                   Parm                    QQmode            1
     C                   Parm                    Scnrb             1
     C                   Parm                    Scapb             1
     C                   Parm                    Scdtsl            1
     C                   Parm                    Scexpc            1
     C                   Parm                    Scpsa             1
     C                   Parm                    Scpfb             1
      ******* beg add ****** 10/13/03 *********************************
     C                   Parm                    @@RLB             1
      ******* end add ****** 10/13/03 *********************************
      *
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Parm                    Scssa             1
      ***** END ADD ***** 05/06/04 ************************************
      ***** BEG-ADD ***** 07/20/16 ************************************
     C                   Parm                    QQGRPSLCT         1
      ***** END-ADD ***** 07/20/16 ************************************
      *
     C                   If        Scdtsl = 'I'
     C                   Eval      *In45 = *On
     C                   Else
     C                   Eval      *In45 = *Off
     C                   Endif
      *
     C                   If        QQemp# = *Zero
     C                   Eval      *In40 = *On
     C                   Endif
      *
     C                   If        QQagnt = *Zero
     C                   Eval      *In41 = *On
     C                   Endif
      *
     C                   If        QQchap = *Zero
     C                   Eval      *In42 = *On
     C                   Endif
      *
     C                   If        QQundw = *Blank
     C                   Eval      *In43 = *On
     C                   Endif
      *
     C                   If        QQprst = *Blank
     C                   Eval      *In44 = *On
     C                   Endif
      *
     C                   Eval      *In71 = *Off
      *
     C                   Select
      *
     C                   When      Qqpsrt = 'AN'
     C                   Movel(P)  Srt1          QQpsrtd
     C                   Eval      *In71 = *On
      *
     C                   When      Qqpsrt = 'AA'
     C                   Movel(P)  Srt2          QQpsrtd
     C                   Eval      *In71 = *On
      *
     C                   When      Qqpsrt = 'EN'
     C                   Movel(P)  Srt3          QQpsrtd
      *
     C                   When      Qqpsrt = 'EA'
     C                   Movel(P)  Srt4          QQpsrtd
      *
     C                   When      Qqpsrt = 'UW'
     C                   Movel(P)  Srt5          QQpsrtd
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Eval      *In73 = *On
      ***** END ADD ***** 05/06/04 ************************************
      *
     C                   When      Qqpsrt = 'DT'
     C                   Movel(P)  Srt6          QQpsrtd
     C                   Endsl
      *
     C                   Select
      *
     C                   When      Qqssrt = 'AN'
     C                   Movel(P)  Srt1          QQssrtd
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Eval      *In74 = *On
     C                   Eval      *In71 = *On
      ***** END ADD ***** 05/06/04 ************************************
      *
     C                   When      Qqssrt = 'AA'
     C                   Movel(P)  Srt2          QQssrtd
      ***** BEG ADD ***** 05/06/04 ************************************
     C                   Eval      *In74 = *On
     C                   Eval      *In71 = *On
      ***** END ADD ***** 05/06/04 ************************************
      *
     C                   When      Qqssrt = 'EN'
     C                   Movel(P)  Srt3          QQssrtd
      *
     C                   When      Qqssrt = 'EA'
     C                   Movel(P)  Srt4          QQssrtd
      *
     C                   When      Qqssrt = 'UW'
     C                   Movel(P)  Srt5          QQssrtd
      *
     C                   When      Qqssrt = 'DT'
     C                   Movel(P)  Srt6          QQssrtd
     C                   Endsl
      *
     C                   If        QQagnt <> *Blanks
     C                   Move      QQagnt        QQagnt2           5 0
     C                   Else
     C                   Eval      QQagnt2 = *Zero
     C                   Endif
      *
     C                   Select
      *
     C                   When      Scnrb = 'N'
     C                   Eval      *In51 = *On
      *
     C                   When      Scnrb = 'R'
     C                   Eval      *In52 = *On
      *
     C                   When      Scnrb = 'B'
     C                   Eval      *In53 = *On
      *
     C                   Other
     C                   Endsl
      *
     C                   Select
      *
     C                   When      Scapb = 'A'
     C                   Eval      *In54 = *On
      *
     C                   When      Scapb = 'P'
     C                   Eval      *In55 = *On
      *
     C                   When      Scapb = 'B'
     C                   Eval      *In56 = *On
      *
     C                   Other
     C                   Endsl
      *
     C                   Eval      Datechk = *Off
      *
     C                   If        QQfrdt <> *Blanks and
     C                             QQtodt <> *Blanks
      ******* beg del ****** 10/13/03 *********************************
     C**** *Usa0         Move      QQfrdt        Frdate
     C**** *Cymd         Move      Frdate        Fromdt
     C**** *Usa0         Move      QQtodt        Todate
     C**** *Cymd         Move      Todate        Todt
      ******* end del ****** 10/13/03 *********************************
      ******* beg add ****** 10/13/03 *********************************
     C                   Move      QQfrdt        Frdate
     C                   Move      QQfrdt        SRDTI
     C                   Call      'SRCVTDAT'    DTCYMD
     C                   Movel     SRDTO         Fromdt
      *
     C                   Move      QQtodt        Todate
     C                   Move      QQtodt        SRDTI
     C                   Call      'SRCVTDAT'    DTCYMD
     C                   Movel     SRDTO         Todt
      *
      ******* end add ****** 10/13/03 *********************************
     C                   Eval      Datechk = *On
     C                   Endif
      *
     C                   Move      QQdte         SRDTI
     C                   Call      'SRCVTDAT'    DTCYMD
     C                   Movel     SRDTO         Currdt
      *
     C     DATCHK        Plist
     C                   Parm                    SRDTI             8 0
     C                   Parm      *Blanks       SRFFMT            8
     C                   Parm      *Blanks       SRTFMT            8
     C                   Parm                    SRDTO            10
     C                   Parm                    SRERR             1
      *
     C     DTCYMD        Plist
     C                   Parm                    SRDTI
     C                   Parm      *Blanks       SRFFMT
     C                   Parm      '*CYMD'       SRTFMT
     C                   Parm                    SRDTO
     C                   Parm                    SRERR
      *
     C     DTMDYY        Plist
     C                   Parm                    SRDTI
     C                   Parm      '*CYMD'       SRFFMT
     C                   Parm      *Blanks       SRTFMT
     C                   Parm                    SRDTO
     C                   Parm                    SRERR
      *
     C     DTMDY         Plist
     C                   Parm                    SRDTI
     C                   Parm      '*CYMD'       SRFFMT
     C                   Parm      '*MDY'        SRTFMT
     C                   Parm                    SRDTO
     C                   Parm                    SRERR
      *
     C     DTMDY1        Plist
     C                   Parm                    SRDTI
     C                   Parm      *Blanks       SRFFMT
     C                   Parm      '*MDY'        SRTFMT
     C                   Parm                    SRDTO
     C                   Parm                    SRERR
      *
     C                   Move      QQCO#         XXCO#             3 0
     C                   Move      QQFND1        XXFND             3 0
     C                   Move      QQDTE         XXDATE            8 0
     C                   Move      QQEMP#        XXEMP#
     C                   Move      QQDIV         XXDIV
     C                   Move      QQMODE        XXMODE            1
      ***** BEG-ADD ***** 07/20/16 ************************************
     C                   If        %Parms > 24
     C                   MOVE      QQGRPSLCT     charGrpSlct
     C                   ENDIF
     C                   IF        charGrpSlct = 'Y'
     C                   EVAL      *In13 = *OFF
     C                   EVAL      *In14 = *ON
     C                   If        Scpsa =' '
     C                   Eval      slctPri = 'N/A'
     C                   Endif
     C                   If        Scpsa ='P'
     C                   Eval      slctpri = 'Primary'
     C                   Endif
     C                   If        Scpsa ='S'
     C                   Eval      slctpri = 'Secondary'
     C                   Endif
     C                   If        Scssa =' '
     C                   Eval      slctSec = 'N/A'
     C                   Endif
     C                   If        Scssa ='P'
     C                   Eval      slctSec = 'Primary'
     C                   Endif
     C                   If        Scssa ='S'
     C                   Eval      slctSec = 'Secondary'
     C                   Endif
     C                   If        ScPfb ='P'
     C                   Eval      slctPrnt = 'Print'
     C                   Endif
     C                   If        ScPfb ='F'
     C                   Eval      slctPrnt = 'File'
     C                   Endif
     C                   If        ScPfb ='B'
     C                   Eval      slctPrnt = 'Both (Print & File)'
     C                   Endif
     C     FNKY1         SETLL     PFN01
     C                   READ      PFN01
     C                   DOW       NOT %EOF
     C                   Eval      GsCo# = FnCo#
     C                   Eval      GsFnd = Fnfnd
     C     XGKY          chain     PGS01
     C                   If        %Found and GsIEFl ='I'
     C                   MOVE      GsFnd         XXFND
     C                   If        slctdGrp = ' '
     C                   eval      slctdGrp = %editc(XXFND:'X')
     C                   Else
     C                   eval      slctdGrp =%trim(slctdGrp) + ',' +
     C                                        %editc(XXFND:'X')
     C                   Endif
     C                   Endif
     C                   READ      PFN01
     C                   Enddo
     C                   ELSE
     C                   EVAL      *In13 = *ON
     C                   EVAL      *In14 = *OFF
     C                   Eval      charGrpSlct = 'N'
     C                   ENDIF
      ***** END ADD ***** 07/20/16 ************************************
      *
      /eject
      *
      * FIRST CYCLE PROCESSING
      *
     C                   If        Xxmode = 'D'
     C                   Eval      *In09 = *On
     C                   Else
     C                   Eval      *In08 = *On
     C                   Endif
      *
     C                   Time                    DSDTTM
     C     *Mdy          Move      Dsdate        Today
      *
     C     Wmupl_Key     Chain     Wmupl                              60
     C     Smcol_Key     Chain     Smcol                              60
      *
     c*** Beg Add ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     C*** End Add ***** 01/28/10 **************************************
      *
      * OPEN PRINT FILE
      *
     C                   Open      Qsysprt
     C                   Eval      First = *On
      *
      * GET THE DISTRIBUTION INFORMATION
      *
     C                   Move      '0'           XXPRT
      *
     C     Smorl_Key     Chain     Smorl                              60
      *
     C                   If        *IN60 = *Off
     C                   Endif
      *
     C                   Move      *On           *INOF
      *
      ***** Beg Del ***** 08/26/14 ************************************
     C*********          Except    Route
      ***** End Del ***** 08/26/14 ************************************
      *
      *** BEG CHANGE ***** 02/08/11 ***********************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *** END CHANGE ***** 02/08/11 ***********************************
     C                   Endsr
      /eject
     OQSYSPRT   E            Route          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O*********                                   8 'XXXXXXXX'
     O*********                                  20 'XXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXX     XX'
     O*********                                  77 'XXXXXX'
     O********* E            Route          1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXXX    XX'
     O*********                                  78 'XXXXXXXX'
     O********* E            Route          1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XXXX    XX'
     O*********                                  79 'XX      XX'
     O********* E            Route          1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            Route          1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            Route          1
     O*********                                   8 'XXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  71 'XX'
     O********* E            Route          1
     O*********                                   4 'XXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  79 'XX    XXXX'
     O********* E            Route          1
     O*********                                   5 'XXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX    XXXX'
     O********* E            Route          1
     O*********                                   6 'XX XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX      XX'
     O********* E            Route          1
     O*********                                   7 'XX  XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX    XXXX'
     O*********                                  79 'XX      XX'
     O********* E            Route          1
     O*********                                   8 'XX   XXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  33 'XXXXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX    XXXX'
     O*********                                  78 'XXXXXXXX'
     O********* E            Route          3
     O*********                                   9 'XX    XXX'
     O*********                                  20 'XXXXXX'
     O*********                                  32 'XXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX     XXX'
     O*********                                  77 'XXXXXX'
     O********* E            Route          1
     O*********                                  15 'Route TO:'
     O*********      08      ORDIS1              60
     O*********         09   UPUSNM              67
      *********
     O********* E            Route          1
     O*********      08      ORDIS2              60
      *********
     O********* E            Route          1
     O*********      08      ORDIS3              60
      *********
     O********* E            Route          2
     O*********      08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************************
     O          E            Route          1
     O                                           15 'Route TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67

     O          E            Route          1
     O               08      ORDIS2              60

     O          E            Route          1
     O               08      ORDIS3              60

     O          E            Route          2
     O               08      ORDIS4              60
      ***** Beg Add ***** 08/05/15 ************************************************
      *
     O          E            Route          2
     O                                           19 'REPORT TITLE:'
     O                                        +   1 'NEW/RENEWAL BUSINESS'
      *
      ***** Beg Del ***** 08/26/14 ************************************
     O********* E            Route          2
     O*********                                  19 'PROGRAM NAME:'
     O*********              ORPNAM              30
     O*********                                  36 'FILE:'
     O*********              ORFIL#              38
     O*********                                  46 'COPIES:'
     O*********              ORCOPY        Z     50
     O*********                                  56 'HOLD:'
     O*********              ORHOLD              61
      *********
     O********* E            Route          2
     O*********                                  19 'OUTPUT QUEUE:'
     O*********              OROUTQ              30
     O*********                                  37 'FORMS:'
     O*********              ORFORM              48
     O*********                                  56 'SAVE:'
     O*********              ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
      *
      * CHANGED ROUTING PAGE TO COMPLY WITH VERSION 3 STANDARDS.
      *
     O          E            Route       2  2
     O                                           46 'REPORT SELECTIONS'
      * 1
     O          E            Route          2
     C***** BEG-CHG ***** 07/20/16 *****************************************
     O*****                                    +  6 'GROUP RANGE  . . . . . . :'
     O*****                  QQfnd1            +  1
     O*****                                    +  2 '-'
     O*****                  QQfnd2            +  2
     O               13                        +  6 'GROUP RANGE  . . . . . . :'
     O               13      QQfnd1            +  1
     O               13                        +  2 '-'
     O               13      QQfnd2            +  2
     O               14                        - 38 'SELECTED GROUP . . . . . :'
     O               14      SlctdGrp          +  1
     C***** END-CHG ***** 07/20/16 *****************************************
      * 2
     O          E            Route          2
     O                                         +  6 'ACCOUNT TYPE . . . . . . :'
     O               51                          45 'New Business'
     O               52                          40 'Renewal'
     O               53                          51 'Both (New/Renewal)'
      * 3
     O          E            Route          2
     O                                         +  6 'ACCOUNT STATUS . . . . . :'
     O               54                          39 'Active'
     O               55                          40 'Pending'
     O               56                          54 'Both (Active/Pending)'
      * 4
     O          E            Route          1
     O                                         +  6 'DATE RANGE . . . . . . . :'
      ******* beg chg ****** 10/13/03 *********************************
     O***********            Frdate            +  1
     O***********                              +  2 '-'
     O***********            Todate            +  2
     O                       Frdate            +  1 '  /  /    '
     O                                         +  2 '-'
     O                       Todate            +  2 '  /  /    '
      ******* end chg ****** 10/13/03 *********************************
     O          E            Route          2
     O                                         +  6 '  Selected by. . . . . . :'
     O                  45                       43 'Issue Date'
     O                 N45                       47 'Effective Date'
      * 5
     O          E            Route          2
     O                                         +  6 'PRIMARY SORT . . . . . . :'
     O                       QQpsrtd           +  1
      * 6
     O          E            Route          2
     O                                         +  6 'PRIMARY/SECONDARY AGENT  :'
      ******* Beg-chg ****** 07/20/16 *********************************
     O***                    scpsa             +  1
     O               13      scpsa             +  1
     O               14      slctpri           -  1
      ******* End-chg ****** 07/20/16 *********************************
      * 7
     O          E            Route          2
     O                                         +  6 'SECONDARY SORT . . . . . :'
     O                       Qqssrtd           +  1
      ***** BEG ADD ***** 05/06/04 ************************************
      * 7
     O          E    74      Route          2
     O                                         +  6 'SECONDARY PRIM/SECN AGENT:'
      ******* Beg-chg ****** 07/20/16 *********************************
     O**                     Scssa             +  1
     O               13      Scssa             +  1
     O               14      SlctSec           -  1
      ******* Beg-chg ****** 07/20/16 *********************************
      ***** END ADD ***** 05/06/04 ************************************
      * 8
     O          E            Route          2
     O                                         +  6 'TYPE OF OUTPUT . . . . . :'
      ******* beg chg ****** 10/13/03 *********************************
     O*********              Qqtypo            +  1
      ******* beg-chg ****** 07/20/16 *********************************
     O*****                  Scpfb             +  1
     O               13      Scpfb             +  1
     O               14      SlctPrnt          -  1
      ******* end-chg ****** 07/20/16 *********************************
      ******* end chg ****** 10/13/03 *********************************
      * 9
     O          E            Route          2
     O                                         +  6 'INCLUDE EXPENSE CONSTANT :'
     O                       Scexpc            +  1
      ***** BEG CHG ***** 08/06/03 ***********************************
      * 10
     O          E            Route          2
     O                                         +  6 'RUN TIME & DATE  . . . . :'
     O                       DSDTTM            +  1 '0 :  :  &  /  /  '
      ***** END CHG ***** 08/06/03 ***********************************
      *
     o*** Beg Chg *****  01/28/10  ************************************
     O***       E            Route          1 60
     O***                                        23 '(C) COPYRIGHT FISERV IN'
     O***                                        46 'SURANCE SOLUTIONS. INC,'
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     o*** END CHG *****  01/28/10  ************************************
     *
     * Report Heading
     *
     O          E            Head           1 06
     O                       Dstime                 '  :  :  '
     O                       Coname             120
     O                       Dsdate        Y    198
     O          E            Head           1
     O                       Q1pgm
     O                       Ortitl             125
     O                                          192 'PAGE:'
     O                       Page          Z    198
     *
      ***** BEG ADD ***** 05/06/04 ************************************
     *
     * Underwriter Heading
     *
     O          E    73      HeadUnd     1  1
     O                                              'UNDERWRITER:'
     O                       svuwtr            +  1
     O                       svuwnm            +  2
      ***** END ADD ***** 05/06/04 ************************************
     *
     * Agent Heading
     *
     O          E    71      HeadAgt     1  1
     O                                              'AGENT:'
     O                       Ebagt#        Z   +  1
     O                       Ebanam            +  2
     *
     * Group Heading
     *
     O          E            HeadGrp     1  1
     O                                              'GROUP:'
     O
      ***** BEG ADD ***** 12/14/04 *********************************************
     O                       Svfnd             +  2
      ***** END ADD ***** 12/14/04 *********************************************
     O                                         +  2 '-'
     O                       Svfnnm            +  2
     *
     * Col Head
     *  Line 1
     O          E            Headcol     1
     O                                           10 'Employer'
     O                                           16 'Div'
     O                                           32 'Employer Name'
     O                                           59 'Incept Date'
      ******* beg del ****** 10/13/03 *********************************
     O***********                                66 'Typ'
     O***********                                73 'Sts'
      ******* end del ****** 10/13/03 *********************************
      ******* beg add ****** 10/13/03 *********************************
     O                                           64 'Typ'
     O                                           69 'Sts'
     O                                           74 'R/L'
      ******* end add ****** 10/13/03 *********************************
     O                                           87 'Underwriter'
     O                                          100 'Issue Date'
     O                                          107 'Yrs'
     O                                          129 'Policy Date Range'
     O                                          154 'Carrier Policy #'
      ******* Beg Chg ****** 01/15/07 *********************************
     O***********                               177 'Current Premium'
     O***********                               187 'Exp'
     O***********                               197 'Sched'
     O                                          187 'Current Premium'
     O                                          197 'Exp'
     O                                          207 'Sched'
      ******* End Chg ****** 01/15/07 *********************************
      *  Line 2
     O          E            Headcol     1  2
     O                                           59 'Cancel Date'
      ******* Beg Chg ****** 01/15/07 *********************************
     O***********                               177 'Prior Yr Premium'
     O***********                               187 'Mod'
     O***********                               197 'DB/CR'
     O                                          187 'Prior Yr Premium'
     O                                          197 'Mod'
     O                                          207 'DB/CR'
      ******* End Chg ****** 01/15/07 *********************************
      *  Line 3
     O          E         N71Headcol     1  2
     O                                           22 'Current Agent# / Name'
     *
     * Detail 1
     *
     O          E            Detail      1  1
     O                       Ebemp#        Z     10
     O                       Ebdiv         Z     16
     O                       Prenam              45
     O                       Nbindt              59 '  /  /    '
      *
      ******* beg del ****** 10/13/03 *********************************
     O*******                Nbnwrw              65
     O*******                Nbapfl              72
      ******* end del ****** 10/13/03 *********************************
      ******* beg add ****** 10/13/03 *********************************
     O*** begin chg *** 09/04/08 *******************************************
     O**********             Nbnwrw              63
      ***** Beg Chg ****** 11/20/13 ***************************************
     O***************        elnwrw              63
     O                       xcNwRw              63
      ***** End Chg ****** 11/20/13 ***************************************
     O*** end   chg *** 09/04/08 *******************************************
     O                       Nbapfl              68
     O                       Nbrqlp              73
      ******* end add ****** 10/13/03 *********************************
     O                       Uwname              86
     O                       Nbisdt             100 '  /  /    '
     O                       Ebfyr              107
     O                       Effdte             121 '  /  /    '
     O                                              ' -'
     O                       Expdte            +  1 '  /  /    '
      ******* Beg Chg ****** 01/15/07 *********************************
     O***************        Nbcap#             153
     O***************        Nbprmr        J    178
     O***************        Xxemex        J    188
     O***************        Eldbcr        J    198
     O                       Nbcap#             163
     O                       Nbprmr        J    188
     O                       Xxemex        J    198
      ***** Beg Chg ****** 11/20/13 ***************************************
     O                       xcDbCr        J    208
     O***************        Eldbcr        J    208
      ***** Beg Chg ****** 11/20/13 ***************************************
      ******* End chg ****** 01/15/07 *********************************
     *
     * Detail 2
     *                                                                    a
     O          E            Detail      0  1
      ***** BEG ADD ***** 02/11/04 ***********************************
      ** Used for testing & sebug only
     O**********             totempf       Z     10
     O**********             totpolf       Z   +  1
     O**********             Premp         Z   +  1
     O**********             Prempf        Z   +  1
     O**********             PREXPMOD      J   +  1
     O**********             PRDBCRF       J   +  1
      ***** END ADD ***** 02/11/04 ***********************************
      ***** BEG DEL ***** 06/09/05 **********************************
     O***********       72   Exoemp        Z     10
     O***********       72   Exodiv        Z     16
     O***********       72   Prenamx             45
     O***********            Nbcxdt              59 '  /  /    '
      ***** END DEL ***** 06/09/05 **********************************
      *
      ***** BEG ADD ***** 06/09/05 ***********************************
      ***** PRINT LINE OF CHARACTERS IF PRIOR EMPLOYER NUMBER
     O                  72   Exofnd        Z      4
     O                  72   Exoemp        Z     14
     O                  72   Exodiv        Z     19
     O                  72   Prenamx             46
     O                       Nbcxdt              59 '  /  /    '
     O                  72                      100 '** PRIOR EMP# **'
      ***** END ADD ***** 06/09/05 ***********************************
      *
      ***** BEG DEL ***** 06/09/05 **********************************
     O************      70N72Prfyr              107
     O************      70N72Prpfdt             121 '  /  /    '
     O************      70N72                       ' -'
     O************      70N72Prptdt            +  1 '  /  /    '
     O************      70N72Prcap#             153
     O************      70N72Nbprmp        J    178
      ***** END DEL ***** 06/09/05 **********************************
      *
      ***** BEG ADD ***** 06/09/05 **********************************
     O                       Prfyr              107
     O                       Prpfdt             121 '  /  /    '
     O                                              ' -'
     O                       Prptdt            +  1 '  /  /    '
      ******* Beg Chg ****** 01/15/07 *********************************
     O*********              Prcap#             153
     O*********              Nbprmp        J    178
     O                       Prcap#             163
     O                       Nbprmp        J    188
      ******* End Chg ****** 01/15/07 *********************************
      ***** END ADD ***** 06/09/05 **********************************
      *
      ***** BEG CHG ***** 08/06/03 ***********************************
     O*********         70N72Premex        J    188
     O*********         70N72Prdbcr        J    198
      ******* Beg Chg ****** 01/15/07 *********************************
     O*********         70N72Xpemex        J    188
     O*********         70N72Xpdbcr        J    198
     O                  70N72Xpemex        J    198
     O                  70N72Xpdbcr        J    208
      ******* Beg Chg ****** 01/15/07 *********************************
      ***** END CHG ***** 08/06/03 ***********************************
     *
     * Detail 3
     *                                                                    a
     O          E      N71   Detail      0  1
     O                       Ebagt#        Z      5
     O                       Pranam              36
     *
     * Agent Total Page
     *
     O          E       71   Totalag     3
     O                                         +  1 'SUMMARY FOR AGENT #'
     O                       Svagt#        Z   +  1
     O                       Svanam            +  1
     O          E            Totalag     2
     O                                           11 'TOTALS:'
     O          E            Totalag     1
     O                                           19 'Current Year'
     O          E            Totalag     1
     O                                           33 'Premium . . . . . . . .:'
     O                       Totprema      J   +  1
     O          E            Totalag     1
     O                                           33 'Average Exp Mod . . . .:'
     O                       Expmoda       J   +  1
      ***** BEG DEL ***** 02/13/04 **************************************************
      ***** BEG ADD ***** 08/13/03 **************************************************
    O*******                                  +  3 '='
    O*******                Totexpmoda    J   +  1
    O*******                                  +  3 '/'
    O*******                Totempa       J   +  1
      ***** END ADD ***** 08/13/03 **************************************************
      ***** END DEL ***** 02/13/04 **************************************************
     *
     O          E            Totalag     1
     O                                           33 'Average Scheduled DB/CR:'
     O                       Dbcra         J   +  1
      ***** BEG DEL ***** 02/13/04 **************************************************
      ***** BEG ADD ***** 08/13/03 **************************************************
    O*******                                  +  3 '='
    O*******                Totdbcra      J   +  1
    O*******                                  +  3 '/'
    O*******                Totempa       J   +  1
      ***** END ADD ***** 08/13/03 **************************************************
      ***** END DEL ***** 02/13/04 **************************************************
     O          E            Totalag     1
     O                                           17 'Prior Year'
     O          E            Totalag     1
     O                                           33 'Premium . . . . . . . .:'
     O                       Totpprema     J   +  1
     O          E            Totalag     1
     O                                           33 'Average Exp Mod . . . .:'
    O                       Expmodpra     J   +  1
      ***** BEG DEL ***** 02/13/04 **************************************************
      ***** BEG ADD ***** 08/13/03 **************************************************
    O*******                                  +  3 '='
    O*******                Prexpmoda     J   +  1
    O*******                                  +  3 '/'
    O*******                Prempa        J   +  1
      ***** END ADD ***** 08/13/03 **************************************************
      ***** END DEL ***** 02/13/04 **************************************************
     *
     O          E            Totalag     1
     O                                           33 'Average Scheduled DB/CR:'
    O                       Dbcrpra       J   +  1
      ***** BEG DEL ***** 02/13/04 **************************************************
      ***** BEG ADD ***** 08/13/03 **************************************************
    O*******                                  +  3 '='
    O*******                Prdbcra       J   +  1
    O*******                                  +  3 '/'
    O*******                Prempa        J   +  1
      ***** END ADD ***** 08/13/03 **************************************************
      ***** END DEL ***** 02/13/04 **************************************************
     *
     O          E            Totalag     2
     O                                           11 'COUNTS:'
     O          E            Totalag     1
      ***** BEG CHG ***** 03/31/04 ********************************************
     O***********                                25 'Number of employers:'
     O                                           25 'Number of policies :'
      ***** END CHG ***** 03/31/04 ********************************************
     O                       Totempa       J   +  1
     *
     O          E            Totalag     1
      ***** BEG CHG ***** 03/31/04 ********************************************
     O                                           25 'Number of employers:'
     O**********                                 25 'Number of policies :'
      ***** END CHG ***** 03/31/04 ********************************************
     O                       Totpola       J   +  1
      *
      ***** BEG ADD ***** 05/06/04 ************************************
     *
     * Underwriter Total Page
     *
     O          E            Totaluw     3
     O                                         +  1 'SUMMARY FOR UNDERWRITER'
     O                       SvUwtr            +  1
     O                       SvUwnm            +  1
     O          E            Totaluw     2
     O                                           11 'TOTALS:'
     O          E            Totaluw     1
     O                                           19 'Current Year'
     O          E            Totaluw     1
     O                                           33 'Premium . . . . . . . .:'
     O                       Totpremu      J   +  1
     O          E            Totaluw     1
     O                                           33 'Average Exp Mod . . . .:'
     O                       Expmodu       J   +  1
     *
     O          E            Totaluw     1
     O                                           33 'Average Scheduled DB/CR:'
     O                       Dbcru         J   +  1
     O          E            Totaluw     1
     O                                           17 'Prior Year'
     O          E            Totaluw     1
     O                                           33 'Premium . . . . . . . .:'
     O                       Totppremu     J   +  1
     O          E            Totaluw     1
     O                                           33 'Average Exp Mod . . . .:'
    O                       Expmodpru     J   +  1
     *
     O          E            Totaluw     1
     O                                           33 'Average Scheduled DB/CR:'
    O                       Dbcrpru       J   +  1
     *
     O          E            Totaluw     2
     O                                           11 'COUNTS:'
     O          E            Totaluw     1
     O                                           25 'Number of policies :'
     O                       Totempu       J   +  1
     *
     O          E            Totaluw     1
     O                                           25 'Number of employers:'
     O                       Totpolu       J   +  1
      *
      ***** END ADD ***** 05/06/04 ************************************
     *
     * Group Total Page
     *
     O          E            Totalfnd    6
     O                                         +  1 'GROUP SUMMARY:'
     O                       Svfnd             +  1
     O                       Svfnnm            +  1
     O          E            Totalfnd    2
     O                                           11 'TOTALS:'
     O          E            Totalfnd    1
     O                                           19 'Current Year'
     O          E            Totalfnd    1
     O                                           33 'Premium . . . . . . . .:'
      ***** BEG ADD ***** 08/20/03 **************************************************
     O*************          Totppremf     J   +  1
     O                       Totpremf      J   +  1
      ***** END ADD ***** 08/20/03 **************************************************
     O          E            Totalfnd    1
     O                                           33 'Average Exp Mod . . . .:'
     O                       Expmodf       J   +  1
      ***** BEG DEL ***** 02/13/04 **************************************************
      ***** BEG ADD ***** 08/13/03 **************************************************
    O********                                 +  3 '='
    O********               Totexpmodf    J   +  1
    O********                                 +  3 '/'
    O********               Totempf       J   +  1
      ***** END ADD ***** 08/13/03 **************************************************
      ***** END ADD ***** 02/13/04 **************************************************
     *
     O          E            Totalfnd    1
     O                                           33 'Average Scheduled DB/CR:'
     O                       Dbcrf         J   +  1
      ***** BEG DEL ***** 02/13/04 **************************************************
      ***** BEG ADD ***** 08/13/03 **************************************************
    O********                                 +  3 '='
    O********               Totdbcrf      J   +  1
    O********                                 +  3 '/'
    O********               Totempf       J   +  1
      ***** END ADD ***** 08/13/03 **************************************************
      ***** END DEL ***** 02/13/04 **************************************************
     O          E            Totalfnd    1
     O                                           17 'Prior Year'
     O          E            Totalfnd    1
     O                                           33 'Premium . . . . . . . .:'
     O                       Totppremf     J   +  1
     O          E            Totalfnd    1
     O                                           33 'Average Exp Mod . . . .:'
    O                       Expmodprf     J   +  1
      ***** BEG DEL ***** 02/13/04 **************************************************
      ***** BEG ADD ***** 08/13/03 **************************************************
    O********                                 +  3 '='
    O********               Prexpmod      J   +  1
    O********                                 +  3 '/'
    O********               Premp         J   +  1
      ***** END ADD ***** 08/13/03 **************************************************
      ***** END DEL ***** 02/13/04 **************************************************
     *
     O          E            Totalfnd    1
     O                                           33 'Average Scheduled DB/CR:'
    O                       Dbcrprf       J   +  1
      ***** BEG DEL ***** 02/13/04 **************************************************
      ***** BEG ADD ***** 08/13/03 **************************************************
    O*******                                  +  3 '='
    O*******                Prdbcrf       J   +  1
    O*******                                  +  3 '/'
    O*******                Prempf        J   +  1
      ***** END ADD ***** 08/13/03 **************************************************
      ***** END DEL ***** 02/13/04 **************************************************
     *
     O          E            Totalfnd    2
     O                                           11 'COUNTS:'
     O          E            Totalfnd    1
      ***** BEG CHG ***** 03/31/04 ********************************************
     O***************                            25 'Number of employers:'
     O                                           25 'Number of policies :'
      ***** END CHG ***** 03/31/04 ********************************************
     O                       Totempf       J   +  1
     *
     O          E            Totalfnd    1
      ***** BEG CHG ***** 03/31/04 ********************************************
     O**************                             25 'Number of policies :'
     O                                           25 'Number of employers:'
      ***** END CHG ***** 03/31/04 ********************************************
     O                       Totpolf       J   +  1
     *
     * Total Page
     *
     O          E            Total       6
     O                                         +  1 'FINAL REPORT SUMMARY:'
     *
     O          E            Total       2
     O                                           11 'TOTALS'
     O          E            Total       1
     O                                           19 'Current Year'
     O          E            Total       1
     O                                           33 'Premium . . . . . . . .:'
     O                       Totprem       J   +  1
     O          E            Total       1
     O                                           33 'Average Exp Mod . . . .:'
     O                       Expmod        J   +  1
      ***** BEG DEL ***** 02/13/04 **************************************************
      ***** BEG ADD ***** 08/13/03 **************************************************
    O********                                 +  3 '='
    O********               Totexpmod     J   +  1
    O********                                 +  3 '/'
    O********               Totemp        J   +  1
      ***** END ADD ***** 08/13/03  *************************************************
      ***** END DEL ***** 02/13/04 **************************************************
     *
     O          E            Total       1
     O                                           33 'Average Scheduled DB/CR:'
     O                       Dbcr          J   +  1
      ***** BEG DEL ***** 02/13/04 **************************************************
      ***** BEG ADD ***** 08/13/03 **************************************************
    O**********                               +  3 '='
    O**********             Totdbcr       J   +  1
    O**********                               +  3 '/'
    O**********             Totemp        J   +  1
      ***** END DEL ***** 02/13/04 **************************************************
      ***** END ADD ***** 08/13/03 **************************************************
     O          E            Total       1
     O                                           17 'Prior Year'
     O          E            Total       1
     O                                           33 'Premium . . . . . . . .:'
     O                       Totpprem      J   +  1
     O          E            Total       1
     O                                           33 'Average Exp Mod . . . .:'
    O                       Expmodpr      J   +  1
      ***** BEG DEL ***** 02/13/04 **************************************************
      ***** BEG ADD ***** 08/13/03 **************************************************
    O*********                                +  3 '='
    O*********              Prexpmod      J   +  1
    O*********                                +  3 '/'
    O*********              Premp         J   +  1
      ***** END ADD ***** 08/13/03 **************************************************
      ***** END DEL ***** 02/13/04 **************************************************
     *
     O          E            Total       1
     O                                           33 'Average Scheduled DB/CR:'
    O                       Dbcrprt       J   +  1
      ***** BEG DEL ***** 02/13/04 **************************************************
      ***** BEG ADD ***** 08/13/03 **************************************************
    O*********                                +  3 '='
    O*********              Prdbcrt       J   +  1
    O*********                                +  3 '/'
    O*********              Premp         J   +  1
      ***** END ADD ***** 08/13/03 **************************************************
      ***** END DEL ***** 02/13/04 **************************************************
     *
     O          E            Total       2
     O                                           11 'COUNTS:'
     O          E            Total       1
      ***** BEG CHG ***** 03/31/04 ********************************************
     O************                               25 'Number of employers:'
     O                                           25 'Number of policies :'
      ***** END CHG ***** 03/31/04 ********************************************
     O                       Totemp        J   +  1
     *
     O          E            Total       1
      ***** BEG CHG ***** 03/31/04 ********************************************
     O                                           25 'Number of employers:'
     O*************                              25 'Number of policies :'
      ***** END CHG ***** 03/31/04 ********************************************
     O                       Totpol        J   +  1
