     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
      *****************************************************************
      *TITLE W7731R - MEMBER PROFILE REPORT
      *****************************************************************
      *                                                               *
      *      ****** ======= ---> NOTICE <--- ========== ******        *
      *      ALL CHANGES TO THIS PROGRAM NEEDS TO BE ALSO PUT         *
      *              in the Source for Program W7732R                 *
      *      ****** ============ NOTICE =============== ******        *
      *                                                               *
      * REVISIONS:                                                    *
      *                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
      * 08/26/14 C:298 Project: SY354                                 *
      *                -Implement Routing Page Changes.               *
      *                                                               *
      * 01/28/10  C:249  Project: SY215                               *
      *           C:189  Retrieve brand Name                          *
      *                                                               *
      * 10/11/07 C:190 SUPPORT WA099 54504                            *
      *                MAKE SURE TO DO NOT ACCESS THE DEDUCTIBLE      *
      *                DISCOUNT CODE HAVING INACTIVE RECORDS.         *
      *                                                               *
     H*  04/10/06 C:65 PROJECT SUPP#51293                             *
     H*                CHANGE DEDUCTIBLE DISCOUNT TO PULL BASED ON    *
     H*                ANNIV RATING DATE INSTEAD OF POLICY EFF DATE.  *
      *                                                               *
      * 09/24/04  C:53  Project: 132WBD                               *
      *                 Add agent header variable field               *
      *                                                               *
      * 07/16/04  C:61  support wa065 43091                           *
      *               - Check inception date when determining if      *
      *                 employer is cancelled.                        *
      *                                                               *
     * 04/16/04 C:132 Project: WA122 SEQ: 42070                      *
     *                Desc of Operations from prior employer fixed   *
      *                                                               *
     * 03/18/04 C:132 Project: WA093 SEQ: 41807                      *
     *                Missing Desc of Operations corrected           *
      *                                                               *
     * 03/08/04 C:09  Project: 93WDR                                 *
     *                Print Policy Svcs Rep information and include  *
     *                in report outfile.                             *
      *                                                               *
     * 02/19/04 C:136 Project: WC1033 (Return Premium)               *
     *                Add processing for EAARTY = 'T' (true void)    *
     *                transaction.                                   *
     *                                                               *
      * 12/05/03  C: 132    PROJECT: WA071 SEQ: 40150                 *
      *               - Change to process all divisions if single     *
      *                 employer# entered without entering a division *
      *                                                               *
      * 10/13/03  C:132 Project WC1083  SEQ: 37882                    *
      *                 Change to print shorter Nature of Business    *
      *                 description on report.                        *
      *                                                               *
      * 10/06/03  C:09  Project WC1083  SEQ: 39361                    *
      *                 Modify this report to generate reports for    *
      *                 cancelled employers since we are printing     *
      *                 the cancel date on this report.               *
      *                                                               *
      * 09/09/03  C: 132    PROJECT: WA129 SEQ: 37882                 *
      *               - Change to output correct address in WFRMP     *
      *                 and print correct address on report           *
      *                                                               *
      * 07/23/03  C: 132    PROJECT: WA033 SEQ: 37882                 *
      *               - Change to output Est Annual Prem in WFRMP     *
      *                                                               *
      * 04/24/03 C:142 SUPPORT WA122 36963                            *
      *             - ADDED FILE PROCESSING FOR WXEBPL1 FOR PROCESSING*
      *               FOR EMPLOYER NUMBER ONLY                        *
      *                                                               *
      * 02/11/03 C:142 SUPPORT WA122 35950                            *
      *             - Changed check from todays date to run date      *
      *                                                               *
      * 12/17/02 C:142 SUPPORT WA122 35178                            *
      *             - Show cancellation date, if > run date. Bypass   *
      *             if cnx date is past.                              *
      *                                                               *
      * 12/05/02 C:142 SUPPORT WA093 35062                            *
      *             - Was not using experience mod if the eff date    *
      *               was greater than run date. Fixed.               *
      *                                                               *
      * 10/25/02 C:142 SUPPORT WA122 34601                            *
      *             - Print for all divisions                         *
      *                                                               *
      * 10/11/02 C:61 SUPPORT wa045 34444                             *
      *             - Correct paging issue when four policy years.    *
      *             - Align header output.                            *
      *                                                               *
      *****************************************************************
      *                                                               *
     FSmcol     IF   E           K Disk
     FWmupl     IF   E           K Disk
     FSmorl     IF   E           K Disk
     FWmfnl     IF   E           K Disk
     FWmeal9    IF   E           K Disk
     FWxebl     IF   E           K Disk
      *****  BEGIN ADD *** 04/24/03 ****************************************
     FWxebl1    IF   E           K Disk
     F                                     Rename(Peb01:Peb02)
      *****  END   ADD *** 04/24/03 ****************************************
     FWmeml     IF   E           K Disk
     FWdell     IF   E           K Disk
     FWmcml     IF   E           K Disk
     FWmahl     IF   E           K Disk
     FWmaal1    IF   E           K Disk
     FWmagl     IF   E           K Disk
     FWmcpl     IF   E           K Disk
     FWtabl     IF   E           K Disk
     FWtlml     IF   E           K Disk
     FWdcxl     IF   E           K Disk
     *
     * Carrier/State Reporting Master File
     FWmrpl4    IF   E           K Disk
     *
     * Rate Set Code Master File
     FWmrsl     IF   E           K Disk
     F                                     Rename(PRS01:WMRSLR)
     * Deductible Discount Code File
     FWtddl2    IF   E           K Disk
     F                                     Rename(PDD01:WTDDLR)
     F*** BEG ADD *** 09/24/04 ************************************
     FWmexl     IF   E           K Disk
     F*** END ADD *** 09/24/04 ************************************
     *
     FWfmrp     O    E             Disk
     FQsysprt   O    F   92        Printer OFLIND(*INOF)
     F                                     USROPN
      ***** Beg Add ****** 03/08/04 *******************************************************
     FWmupl1    IF   E           K Disk
     F                                     Rename(Pup01:Pup02)
      ***** End Add ****** 03/08/04 *******************************************************
     D Year            S              3  0 DIM(4)                               Fund Year array
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D First           S              1n
     D Cnxdate         S             10
     D QQfrdt          S              8
     D QQtodt          S              8
     D QQpsrt          S              2
     D QQpsrtd         S             25
     D QQssrtd         S             25
     D QQtypo          S              1
     D QQstat          S              1
     D QQagnt          S              5
     D QQchap          S              3
     D QQundw          S              3
     D QQprst          S              2
     D QQssrt          S              2
      ***** BEG ADD ****** 09/09/03 *******************************************************
     D XXADR1          S                   LIKE(EMPAD1)
     D XXADR2          S                   LIKE(EMPAD2)
     D XXCSZ           S             40
     D XPCSZ           S             40
     D XMCSZ           S             40
      ***** END ADD ****** 09/09/03 *******************************************************
     D*** BEG ADD *** 09/24/04 ******************************          ******
     D CHGNUM          C                   'SA020R'
     D*** END ADD *** 09/24/04 ***********************************     ******
     D Srt1            C                   'Expiration date'
     D Srt2            C                   'Agent'
     D Srt3            C                   'Chapter'
     D Srt4            C                   'Underwriter'
     D Srt5            C                   'Premium rating state'
     D Srt6            C                   'Employer'
     D Datusa          S               D   DATFMT(*USA)
      **** Begin Add **** 12/17/2002 ***************************************
     D Today           S               D   DATFMT(*USA)
      **** End   Add **** 12/17/2002 ***************************************
     D Inceptdt        S               D   DATFMT(*USA)
     D Frdate          S               D   DATFMT(*USA)
     D Todate          S               D   DATFMT(*USA)
     D Fromdt          S              7  0
     D Todt            S              7  0
     D DateChk         S              1N
     D Tab10           S             10
     D Cfund           S              3
     D Cemp#           S              9
     D Cfyr            S              3
     D Pbsdesc         S             30
     *
      *****  BEG ADD ***** 12/05/03 ****************************************
     D AllDiv          S              1N
      *****  END ADD ***** 12/05/03 ****************************************
      *
     D                 DS
     D  Cmt                    1     20
     D                                     DIM(10)                              RESERVE TYPES
     D  Cmrt01                 1      2
     D  Cmrt02                 3      4
     D  Cmrt03                 5      6
     D  Cmrt04                 7      8
     D  Cmrt05                 9     10
     D  Cmrt06                11     12
     D  Cmrt07                13     14
     D  Cmrt08                15     16
     D  Cmrt09                17     18
     D  Cmrt10                19     20
     D                 DS
     D  CMA                    1     90  2
     D                                     DIM(10)                              PMT AMOUNT
     D  Cmar01                 1      9  2
     D  Cmar02                10     18  2
     D  Cmar03                19     27  2
     D  Cmar04                28     36  2
     D  Cmar05                37     45  2
     D  Cmar06                46     54  2
     D  Cmar07                55     63  2
     D  Cmar08                64     72  2
     D  Cmar09                73     81  2
     D  Cmar10                82     90  2
     D                 DS
     D  Cmr                    1     90  2
     D                                     DIM(10)                              RESERVE AMOUNT
     D  Cmrr01                 1      9  2
     D  Cmrr02                10     18  2
     D  Cmrr03                19     27  2
     D  Cmrr04                28     36  2
     D  Cmrr05                37     45  2
     D  Cmrr06                46     54  2
     D  Cmrr07                55     63  2
     D  Cmrr08                64     72  2
     D  Cmrr09                73     81  2
     D  Cmrr10                82     90  2
     D                 DS
     D  Empzip                 1      9  0
     D  Xxzpp1                 1      5  0
     D  Xxzpp2                 6      9  0
      ***** BEG ADD ****** 09/09/03 *******************************************************
     D                 DS
     D  Emmzip                 1      9  0
     D  Xmzpp1                 1      5  0
     D  Xmzpp2                 6      9  0
      ***** END ADD ****** 09/09/03 *******************************************************
     D                 DS
     D  Ef                     1    035  0
     D                                     DIM(5)                               EFFECTIVE DATE
     D  Emefd1               001    007  0
     D  Emefd2               008    014  0
     D  Emefd3               015    021  0
     D  Emefd4               022    028  0
     D  Emefd5               029    035  0
     D                 DS
     D  Dsdttm                 1     12  0
     D  Dstime                 1      6  0
     D  Dsdate                 7     12  0
      *
      ***** BEG ADD ****** 10/13/03 *******************************************************
     D xxnaob          S             30
      ***** END ADD ****** 10/13/03 *******************************************************
      *
     D***  Begin Add  ***  01/28/10  **************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
      *
     D PSDS          ESDS                  Extname(Szq1p)
      *
     C                   Eval      *Inlr = *Off
      /eject
     C                   Move      *Blanks       XXCARR            3
      *
      *  Read work file input
      *
      *****  BEGIN ADD *** 04/24/03 ****************************************
     C                   If        Xxemp# = *Zero
     C                   Exsr      Proc_Wxebl
     C                   Else
     C                   Exsr      Proc_Wxebl1
     C                   Endif
      *****  END   ADD *** 04/24/03 ****************************************
      *
      /Free
       // ***** Beg Add ***** 08/26/14 *************************************
       xxPrt = '0';
       Chain Smorl_Key SMORL;
       If %found;
          Except Route;
       Endif;
       // ***** End Add ***** 08/26/14 *************************************
      /End-free
     C                   Eval      *Inlr = *On
      *
      /eject
      *****  BEGIN ADD *** 04/24/03 ****************************************
      **********************************************************************
      * Process Wxebl record
      **********************************************************************
      *
     C     Proc_Wxebl    Begsr
      *****  END   ADD *** 04/24/03 ****************************************
      *
     C                   Dou       %Eof(Wxebl)
     C                   Read      Wxebl
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   If        QQagnt2 <> *Zero and
     C                             QQagnt2 <> Ebagt#
     C                   Iter
     C                   Endif
      *
     C                   If        QQprst <> *Blanks and
     C                             QQprst <> Ebjrst
     C                   Iter
     C                   Endif
      *
     C                   If        Ebco#  <> Svco# or
     C                             Ebfnd  <> Svfnd or
     C                             Ebemp# <> Svemp# or
      *** Begin Add  10/25/02 **********************************************
     C                             Ebdiv  <> Svdiv
      *** End   Add  10/25/02 **********************************************
     C                   Eval      Svco#  = Ebco#
     C                   Eval      Svfnd  = Ebfnd
     C                   Eval      Svemp# = Ebemp#
      *** Begin Add  10/25/02 **********************************************
     C                   Eval      Svdiv  = Ebdiv
      *** End   Add  10/25/02 **********************************************
     C                   Else
     C*                  Iter
     C                   Endif
      *
     C                   Z-add     Ebptdt        SRDTI
      *
     C                   Call      'SRCVTDAT'    DTMDY
     C                   Movel     SRDTO         XXptdt            6 0
     C     *Mdy          Move      XXptdt        Datusa
     C                   Move      Datusa        Ptdt             10
     C                   Z-add     EbpFdt        SRDTI
      *
     C                   Call      'SRCVTDAT'    DTMDY
     C                   Movel     SRDTO         XXpFdt            6 0
     C     *Mdy          Move      XXpFdt        Datusa
     C                   Move      Datusa        Pfdt             10
      *
     C     Wmeml_Key1    Chain     Wmeml
      *
     C                   If        %Found(Wmeml)
      *
     C                   If        QQchap2 <> *Zero and
     C                             QQchap2 <> Emchap
     C                   Iter
     C                   Endif
      *
     C                   If        QQundw <> *Blanks and
     C                             QQundw <> Emuwtr
     C                   Iter
     C                   Endif
      *
      **** Begin Add **** 12/17/2002 ***************************************
     C                   Eval      *In45 = *Off
      *****  BEGIN ADD *** 04/24/03 ****************************************
     C                   Eval      *In46 = *Off
      ***** Beg del ****** 07/16/04 *******************************************************
      *************                                                         ***************
     C*************      Clear                   Cnxdate                    ***************
      *******END***ADD *** 04/24/03 *******************************************************
     C*************      Select                                             ***************
      *************                                                         ***************
     C*************      When      Emcad5 > *Zero                           ***************
     C******Cymd***      Move      Emcad5        Datusa                     ***************
      *******BEGIN*ADD *** 04/24/03 *******************************************************
     C******Usa****      Move      Datusa        Cnxdate                    ***************
     C*************      Eval      *In46 = *On                              ***************
      *******END***ADD *** 04/24/03 *******************************************************
      *****Begin*Chg **** 02/11/2003 ******************************************************
     C*************      If        Datusa < Today                           ***************
     C*************      If        Emcad5 < Xxymd                           ***************
      *****End***Chg **** 02/11/2003 ******************************************************
      ******Beg*Del ****** 10/06/03 *******************************************************
     C*******************Iter                                               ***************
      ******End*Del ****** 10/06/03 *******************************************************
     C*************      Else                                               ***************
     C*************      Move      Datusa        Mrcxdt                     ***************
     C*************      Eval      *In45 = *On                              ***************
     C*************      Endif                                              ***************
      *************                                                         ***************
     C*************      When      Emcad4 > *Zero                           ***************
     C******Cymd***      Move      Emcad4        Datusa                     ***************
      *******BEGIN*ADD *** 04/24/03 *******************************************************
     C******Usa****      Move      Datusa        Cnxdate                    ***************
     C*************      Eval      *In46 = *On                              ***************
      *******END***ADD *** 04/24/03 *******************************************************
      *****Begin*Chg **** 02/11/2003 ******************************************************
     C*************      If        Datusa < Today                           ***************
     C*************      If        Emcad4 < Xxymd                           ***************
      *****End***Chg **** 02/11/2003 ******************************************************
      ******Beg*Del ****** 10/06/03 *******************************************************
     C*******************Iter                                               ***************
      ******End*Del ****** 10/06/03 *******************************************************
     C*************      Else                                               ***************
     C*************      Move      Datusa        Mrcxdt                     ***************
     C*************      Eval      *In45 = *On                              ***************
     C*************      Endif                                              ***************
      *************                                                         ***************
     C*************      When      Emcad3 > *Zero                           ***************
     C******Cymd***      Move      Emcad3        Datusa                     ***************
      *******BEGIN*ADD *** 04/24/03 *******************************************************
     C******Usa****      Move      Datusa        Cnxdate                    ***************
     C*************      Eval      *In46 = *On                              ***************
      *******END***ADD *** 04/24/03 *******************************************************
      *****Begin*Chg **** 02/11/2003 ******************************************************
     C*************      If        Datusa < Today                           ***************
     C*************      If        Emcad3 < Xxymd                           ***************
      *****End***Chg **** 02/11/2003 ******************************************************
      ******Beg*Del ****** 10/06/03 *******************************************************
     C*******************Iter                                               ***************
      ******End*Del ****** 10/06/03 *******************************************************
     C*************      Else                                               ***************
     C*************      Move      Datusa        Mrcxdt                     ***************
     C*************      Eval      *In45 = *On                              ***************
     C*************      Endif                                              ***************
      *************                                                         ***************
     C*************      When      Emcad2 > *Zero                           ***************
     C******Cymd***      Move      Emcad2        Datusa                     ***************
      *******BEGIN*ADD *** 04/24/03 *******************************************************
     C******Usa****      Move      Datusa        Cnxdate                    ***************
     C*************      Eval      *In46 = *On                              ***************
      *******END***ADD *** 04/24/03 *******************************************************
      *****Begin*Chg **** 02/11/2003 ******************************************************
     C*************      If        Datusa < Today                           ***************
     C*************      If        Emcad2 < Xxymd                           ***************
      *****End***Chg **** 02/11/2003 ******************************************************
      ******Beg*Del ****** 10/06/03 *******************************************************
     C*******************Iter                                               ***************
      ******End*Del ****** 10/06/03 *******************************************************
     C*************      Else                                               ***************
     C*************      Move      Datusa        Mrcxdt                     ***************
     C*************      Eval      *In45 = *On                              ***************
     C*************      Endif                                              ***************
      *************                                                         ***************
     C*************      When      Emcad1 > *Zero                           ***************
     C******Cymd***      Move      Emcad1        Datusa                     ***************
      *******BEGIN*ADD *** 04/24/03 *******************************************************
     C******Usa****      Move      Datusa        Cnxdate                    ***************
     C*************      Eval      *In46 = *On                              ***************
      *******END***ADD *** 04/24/03 *******************************************************
      *****Begin*Chg **** 02/11/2003 ******************************************************
     C*************      If        Datusa < Today                           ***************
     C*************      If        Emcad1 < Xxymd                           ***************
      *****End***Chg **** 02/11/2003 ******************************************************
      ******Beg*Del ****** 10/06/03 *******************************************************
     C*******************Iter                                               ***************
      ******End*Del ****** 10/06/03 *******************************************************
     C*************      Else                                               ***************
     C*************      Move      Datusa        Mrcxdt                     ***************
     C*************      Eval      *In45 = *On                              ***************
     C*************      Endif                                              ***************
      *************                                                         ***************
     C*************      Other                                              ***************
     C*************      Endsl                                              ***************
      **** End   Add **** 12/17/2002 ***************************************
      ***** End del ****** 07/16/04 *******************************************************
      ***** Beg add ****** 07/16/04 *******************************************************
      *
      * get cancel date
     c                   exsr      getcxdt
      *
      ***** End add ****** 07/16/04 *******************************************************
      *
     C                   Eval      Mrco#   = Emco#
     C                   Eval      Mrfnd   = Emfnd
     C                   Eval      Mremp#  = Ememp#
     C                   Move      Emfnd         Cfund
     C                   Move      Ememp#        Cemp#
     C                   Move      Ebfyr         Cfyr
     C                   Exsr      Main
     C                   Endif
      *
     C                   Enddo
      *****  BEGIN ADD *** 04/24/03 ****************************************
      *
     C                   Endsr
      /eject
      **********************************************************************
      * Process Wxebl1 record
      **********************************************************************
      *
     C     Proc_Wxebl1   Begsr
      *
     C     Wxebl1_Key    Setll     Wxebl1
      *
     C                   Dou       %Eof(Wxebl1)
     *
      *****  BEG CHG ***** 12/05/03 ****************************************
     C**** Wxebl1_Key    Reade     Wxebl1
     C                   If        AllDiv = *ON
     C     Wxebl1_Ky2    Reade     Wxebl1
     C                   else
     C     Wxebl1_Key    Reade     Wxebl1
     C                   EndIF
      *****  END CHG ***** 12/05/03 ****************************************
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   Eval      Svco#  = Ebco#
     C                   Eval      Svfnd  = Ebfnd
     C                   Eval      Svemp# = Ebemp#
     C                   Eval      Svdiv  = Ebdiv
     C                   Z-add     Ebptdt        SRDTI
      *
     C                   Call      'SRCVTDAT'    DTMDY
     C                   Movel     SRDTO         XXptdt            6 0
     C     *Mdy          Move      XXptdt        Datusa
     C                   Move      Datusa        Ptdt             10
     C                   Z-add     EbpFdt        SRDTI
      *
     C                   Call      'SRCVTDAT'    DTMDY
     C                   Movel     SRDTO         XXpFdt            6 0
     C     *Mdy          Move      XXpFdt        Datusa
     C                   Move      Datusa        Pfdt             10
      *
      ***** BEG DEL ****** 03/08/04 *******************************************************
      *****  BEGIN ADD *** 03/18/04 ****************************************
     C************       Eval      xxnaob = *blanks
      *****  END ADD ***** 03/18/04 ****************************************
      ***** END DEL ****** 03/08/04 *******************************************************
      *
     C     Wmeml_Key1    Chain     Wmeml
      *
     C                   If        %Found(Wmeml)
      *
      *****  BEGIN ADD *** 10/13/03 ****************************************
     C                   movel(p)  emnaob        xxnaob
      *****  END ADD ***** 10/13/03 ****************************************
      *
      ***** Beg add ****** 07/16/04 *******************************************************
      *************                                                         ***************
     C*************      Eval      *In45 = *Off                             ***************
     C*************      Eval      *In46 = *Off                             ***************
     C*************      Clear                   Cnxdate                    ***************
     C*************      Select                                             ***************
      *************                                                         ***************
     C*************      When      Emcad5 > *Zero                           ***************
     C******Cymd***      Move      Emcad5        Datusa                     ***************
     C******Usa****      Move      Datusa        Cnxdate                    ***************
     C*************      Eval      *In46 = *On                              ***************
     C*************      If        Emcad5 < Xxymd                           ***************
      ******Beg*Del ****** 10/06/03 *******************************************************
     C*******************Iter                                               ***************
      ******End*Del ****** 10/06/03 *******************************************************
     C*************      Else                                               ***************
     C*************      Move      Datusa        Mrcxdt                     ***************
     C*************      Eval      *In45 = *On                              ***************
     C*************      Endif                                              ***************
      *************                                                         ***************
     C*************      When      Emcad4 > *Zero                           ***************
     C******Cymd***      Move      Emcad4        Datusa                     ***************
     C******Usa****      Move      Datusa        Cnxdate                    ***************
     C*************      Eval      *In46 = *On                              ***************
     C*************      If        Emcad4 < Xxymd                           ***************
      ******Beg*Del ****** 10/06/03 *******************************************************
     C*******************Iter                                               ***************
      ******End*Del ****** 10/06/03 *******************************************************
     C*************      Else                                               ***************
     C*************      Move      Datusa        Mrcxdt                     ***************
     C*************      Eval      *In45 = *On                              ***************
     C*************      Endif                                              ***************
      *************                                                         ***************
     C*************      When      Emcad3 > *Zero                           ***************
     C******Cymd***      Move      Emcad3        Datusa                     ***************
     C******Usa****      Move      Datusa        Cnxdate                    ***************
     C*************      Eval      *In46 = *On                              ***************
     C*************      If        Emcad3 < Xxymd                           ***************
      ******Beg*Del ****** 10/06/03 *******************************************************
     C*******************Iter                                               ***************
      ******End*Del ****** 10/06/03 *******************************************************
     C*************      Else                                               ***************
     C*************      Move      Datusa        Mrcxdt                     ***************
     C*************      Eval      *In45 = *On                              ***************
     C*************      Endif                                              ***************
      *************                                                         ***************
     C*************      When      Emcad2 > *Zero                           ***************
     C******Cymd***      Move      Emcad2        Datusa                     ***************
     C******Usa****      Move      Datusa        Cnxdate                    ***************
     C*************      Eval      *In46 = *On                              ***************
     C*************      If        Emcad2 < Xxymd                           ***************
      ******Beg*Del ****** 10/06/03 *******************************************************
     C*******************Iter                                               ***************
      ******End*Del ****** 10/06/03 *******************************************************
     C*************      Else                                               ***************
     C*************      Move      Datusa        Mrcxdt                     ***************
     C*************      Eval      *In45 = *On                              ***************
     C*************      Endif                                              ***************
      *************                                                         ***************
     C*************      When      Emcad1 > *Zero                           ***************
     C******Cymd***      Move      Emcad1        Datusa                     ***************
     C******Usa****      Move      Datusa        Cnxdate                    ***************
     C*************      Eval      *In46 = *On                              ***************
     C*************      If        Emcad1 < Xxymd                           ***************
      ******Beg*Del ****** 10/06/03 *******************************************************
     C*******************Iter                                               ***************
      ******End*Del ****** 10/06/03 *******************************************************
     C*************      Else                                               ***************
     C*************      Move      Datusa        Mrcxdt                     ***************
     C*************      Eval      *In45 = *On                              ***************
     C*************      Endif                                              ***************
      *************                                                         ***************
     C*************      Other                                              ***************
     C*************      Endsl                                              ***************
      ***** end del ****** 07/16/04 *******************************************************
      ***** Beg add ****** 07/16/04 *******************************************************
      *
      * get cancel date
     c                   exsr      getcxdt
      *
      ***** End add ****** 07/16/04 *******************************************************
      *
     C                   Eval      Mrco#   = Emco#
     C                   Eval      Mrfnd   = Emfnd
     C                   Eval      Mremp#  = Ememp#
     C                   Move      Emfnd         Cfund
     C                   Move      Ememp#        Cemp#
     C                   Move      Ebfyr         Cfyr
     C                   Exsr      Main
     C                   Endif
      *
     C                   Enddo
      *
     C                   Endsr
      *****  END   ADD *** 04/24/03 ****************************************
      /eject
      **********************************************************************
      * Main processing routine
      **********************************************************************
      *
     C     Main          Begsr
      *
      *  INITIALIZE LOSS HISTORY FIELDS
      *
     C                   Clear                   L1PREM           11 2
     C                   Clear                   L1TOTI           11 2
     C                   Clear                   L1TOTC            6 0
     C                   Move      'N'           XXPRNT            1
      *
     C                   Exsr      Get_Cnx_Cnt
     C*** BEG ADD *** 09/24/04 *****************************
     C                   Exsr      Getagt
     C*** END ADD *** 09/24/04 *****************************
     C                   Exsr      Curr_Year
      *
      *   IF HEAD HAS BEEN PRINTED FOR EMPLOYER, PRINT TOTALS &
      *   COMMENT LINES
      *
     C     XXPRNT        IFEQ      'Y'
      *
     C                   Exsr      Proc_Year
      *
      * CALCULATE LOSS RATIO
      *
     C                   If        L1PREM <> *ZERO
     C     L1TOTI        Mult      100           XXITOT           11 2
     C     XXITOT        Div(h)    L1PREM        L1LOSS            7 2
     C                   Else
     C                   Z-add     *ZERO         L1LOSS
     C                   Endif
      *
      * CALCULATE FREQUENCY RATIO
      *
     C     L1PREM        DIV(H)    1000          THPREM           11 2
      *
     C                   If        THPREM <> *ZERO
     C     L1TOTC        Div(h)    THPREM        L1FREQ            6 2
     C                   Else
     C                   Z-Add     *ZERO         L1FREQ
     C                   Endif
      *
     C                   Except    Tot
      *
     C                   Do        6
     C                   Except    Tot1
     C                   Enddo
      *
     C                   Except    Tot2
     C                   Endif
      *
     C                   If        QQtypo <> 'R'
     C                   Exsr      Write_Wfmrp
     C                   Endif
      *
     C                   Endsr
      *
      /eject
      *****************************************************************
      * Get current year Wdell and move to output fields
      *****************************************************************
      *
     C     Curr_Year     Begsr
      *
     C                   Move      'N'           XXPRNT
     C                   Move      Ebfyr         Xxfyr
     C     Wdell_Key     Chain     Wdell
      *
     C                   If        %Found
      *
     C                   Move      'Y'           XXPRNT            1
     C                   If        Elpfdt > *Zero
     C     *Cymd         Move      Elpfdt        Datusa
     C                   Move      Datusa        Mrcef1
     C                   Endif
     C                   If        Elptdt > *Zero
     C     *Cymd         Move      Elptdt        Datusa
     C                   Move      Datusa        Mrcex1
     C                   Endif
     C                   Eval      Mrcrtc  = Elrtcd
     C                   Exsr      Get_Risk_St
     C                   Eval      Mrcrst  = Risk_St
      ***** BEG CHG **** 07/23/03 ******************************************
     C**********         Eval      Mrceap  = Elampr
     C                   Eval      Mrceap  = Elanpr
     C                   MOVE      *zeros        xxcemd            8 0
      ***** END CHG **** 07/23/03 ******************************************
     C                   Eval      Mrcemo  = Elexm1
     C                   If        Elemd1 > *Zero
     C     *Cymd         Move      Elemd1        Datusa
     C                   Move      Datusa        Mrcemd
      ***** BEG ADD **** 07/23/03 ******************************************
     C                   Move      Datusa        xxcemd
      ***** END ADD **** 07/23/03 ******************************************
     C                   Endif
      ***** BEG DEL **** 07/23/03 ******************************************
     C**N78******        Eval      Mrcemo  = Elexm2
      ***** END DEL **** 07/23/03 ******************************************
     C                   If        Elemd2 > *Zero
     C  N78*Cymd         Move      Elemd2        Datusa
     C  N78              Move      Datusa        Mrcemd
      ***** BEG ADD **** 07/23/03 ******************************************
     C                   Move      Datusa        xxcemd
     C                   Eval      Mrcemo  = Elexm2
      ***** END ADD **** 07/23/03 ******************************************
     C                   Endif
      ***** BEG DEL **** 07/23/03 ******************************************
     C**N79******        Eval      Mrcemo  = Elexm3
      ***** END DEL **** 07/23/03 ******************************************
     C                   If        Elemd3 > *Zero
     C  N79*Cymd         Move      Elemd3        Datusa
     C  N79              Move      Datusa        Mrcemd
      ***** BEG ADD **** 07/23/03 ******************************************
     C                   Move      Datusa        xxcemd
     C                   Eval      Mrcemo  = Elexm3
      ***** END ADD **** 07/23/03 ******************************************
     C                   Endif
     C                   Exsr      Get_Pol#
     C                   Eval      Mrcbsc  = Elbscd
      *
     C                   Clear                   Pbsdesc
     C                   Move      *Blank        Xxblsc
     C                   Movel     Elbscd        Xxblsc
     C     BS_Key1       Chain     Pbs01
      *
     C                   If        %Found
     C                   Movel     Bsdesc        Mrcbsd
     C                   Movel     Bsdesc        Pbsdesc
     C                   Endif
      *
     C                   Move      Qqdte         Mrrndt
     C                   Exsr      Get_AR_Bal
     C                   Eval      Mrcilc  = Elilcd
      *
     C                   Move      *Blank        Tab10
     C                   Movel(p)  Elilcd        Tab10
     C     IL_Key        Chain     Wtlml
     C                   If        %Found
     C                   Movel(p)  Lmdesc        Mrcild
     C                   Endif
      *
     C                   Eval      Mrcdcd  = Eldcde
     C*** BEGIN CHG ***  04/10/06  ************************************
     C                   MOVE      ELDCEF        Q2DATE
     C                   MOVE      ELCO#         Q1CO#
     C                   MOVE      ELFND         Q1FND
     C                   MOVE      ELFYR         Q1FYR
     C                   MOVE      ELEMP#        Q1EMP#
     C                   MOVE      ELDIV         Q1DIV
     C*
     C                   CALL      'SREFFD'
     C                   PARM                    Q1CO#             3
     C                   PARM                    Q1FND             3
     C                   PARM                    Q1FYR             3
     C                   PARM                    Q1EMP#            9
     C                   PARM                    Q1DIV             5
     C                   PARM                    Q2DATE            7
     C*
     C                   MOVE      Q2DATE        XXDDT
     C*** END CHG *****  04/10/06  ************************************
      *
      * Get deductible description
      *
     C     Wtddl2_Key    Setgt     Wtddl2
     C     Wtddl2_Key2   Readpe    Wtddl2                                 60
      *
     C                   If        Not *In60
     C*** BEG ADD *****  10/11/07  ************************************
     c                             and (DDSTAT = 'A')
     C*** END ADD *****  10/11/07  ************************************
     C                   Movel(p)  Dddesc        Mrcdds
     C                   Endif
      *
     C                   Eval      Mrcsch  = Eldbcr
      *
     C                   Exsr      Get_NA
     C                   Except    Head
      *
     C                   Endif
      *
     C                   Endsr
      *
      /eject
     **********************************************************************
     *  Get Risk State from Rate Set Code Master File
     **********************************************************************
      *
     C     Get_Risk_St   Begsr
      *
     C     Wmrsl_Key     Chain     Wmrsl
      *
     C                   If        %Found
     C                   Eval      Risk_St = Rsjrst
     C                   Else
     C                   Eval      Risk_St = *blank
     C                   Endif
      *
     C                   Endsr
      /eject
     **********************************************************************
     *  Get Cancellation Count
     **********************************************************************
      *
     C     Get_Cnx_Cnt   Begsr
      *
  181C                   Move      Ebfyr         Xxfyr
      *
     C     Wdcxl_Key     Setll     Wdcxl
      *
     C                   Dou       %Eof
     C     Wdcxl_Key     Reade     Wdcxl
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   If        Cxerr <> 'Y'
     C                   Eval      Mrc#cx = Mrc#cx + 1
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
     C     Get_Pol#      Begsr
      *
     C     Wmrpl4_Key    Chain     Wmrpl4
      *
     C                   If        %Found and
     C                             Rpcap# <> *blank
     C                   Eval      Mrccar# = Rpcap#
     C                   Endif
      *
     C                   Endsr
      /eject
      *****************************************************************
      * Get A/R balance
      *****************************************************************
      *
     C     Get_AR_Bal    Begsr
      *
      *
     C     Wmeal9_Key    Setll     Wmeal9
      *
     C                   Dou       %Eof
     C     Wmeal9_Key    Reade     Wmeal9
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   If        Eapodt > Currdt
     C                   Iter
     C                   Endif
      *
     C                   If        Eaarty = 'P' or
     C                             Eaarty = 'Q' or
      *** BEG CHG ***** 02/19/04 ***********************************************
     C**************************   Eaarty = 'I'
     C                             Eaarty = 'I' or
     C                             Eaarty = 'T'
      *** END CHG ***** 02/19/04 ***********************************************
     C                   Eval      Eaamt = Eaamt * -1
     C                   Endif
      *
     C                   Eval      Mrcarb = Eaamt + Mrcarb
      *
     C                   Enddo
      *
     C                   Endsr
      /eject
      *****************************************************************==*
      * Move detail fields to file WFMRP
      *****************************************************************
      *
     C     Move_Out      Begsr
      *
     C                   Select
      *
     C                   When      Z = 1 and
     C                             Mr1yr = *Zero
     C                   Eval      Mr1yr   = Xxfyr
     C                   Eval      Mr1eap  = Xxprem
     C                   Eval      Mr1los  = Xxtoti
     C                   Eval      Mr1clm  = Xxtotc
     C                   Eval      Mr1lr   = Xxlrat
     C                   Eval      Mr1fr   = Xxfreq
     C                   Eval      Mr1emo  = Elexm1
     C                   If        Elemd1 > *Zero
     C     *Cymd         Move      Elemd1        Datusa
     C                   Move      Datusa        Mr1edt
     C                   Endif
     C  N78              Eval      Mr1emo  = Elexm2
     C                   If        Elemd2 > *Zero
     C  N78*Cymd         Move      Elemd2        Datusa
     C  N78              Move      Datusa        Mr1edt
     C                   Endif
     C  N79              Eval      Mr1emo  = Elexm3
     C                   If        Elemd3 > *Zero
     C  N79*Cymd         Move      Elemd3        Datusa
     C  N79              Move      Datusa        Mr1edt
     C                   Endif
      *
     C                   When      Z = 2
     C                   If        Mr1yr = *Zero
     C                   Eval      Mr1yr   = Xxfyr
     C                   Eval      Mr1eap  = Xxprem
     C                   Eval      Mr1los  = Xxtoti
     C                   Eval      Mr1clm  = Xxtotc
     C                   Eval      Mr1lr   = Xxlrat
     C                   Eval      Mr1fr   = Xxfreq
     C                   Eval      Mr1emo  = Elexm1
     C                   If        Elemd1 > *Zero
     C     *Cymd         Move      Elemd1        Datusa
     C                   Move      Datusa        Mr1edt
     C                   Endif
     C  N78              Eval      Mr1emo  = Elexm2
     C                   If        Elemd2 > *Zero
     C  N78*Cymd         Move      Elemd2        Datusa
     C  N78              Move      Datusa        Mr1edt
     C                   Endif
     C  N79              Eval      Mr1emo  = Elexm3
     C                   If        Elemd3 > *Zero
     C  N79*Cymd         Move      Elemd3        Datusa
     C  N79              Move      Datusa        Mr1edt
     C                   Endif
     C                   Else
     C                   Eval      Mr2yr   = Xxfyr
     C                   Eval      Mr2eap  = Xxprem
     C                   Eval      Mr2los  = Xxtoti
     C                   Eval      Mr2clm  = Xxtotc
     C                   Eval      Mr2lr   = Xxlrat
     C                   Eval      Mr2fr   = Xxfreq
     C                   Eval      Mr2emo  = Elexm1
     C                   If        Elemd1 > *Zero
     C     *Cymd         Move      Elemd1        Datusa
     C                   Move      Datusa        Mr2edt
     C                   Endif
     C  N78              Eval      Mr1emo  = Elexm2
     C                   If        Elemd2 > *Zero
     C  N78*Cymd         Move      Elemd2        Datusa
     C  N78              Move      Datusa        Mr2edt
     C                   Endif
     C  N79              Eval      Mr1emo  = Elexm3
     C                   If        Elemd3 > *Zero
     C  N79*Cymd         Move      Elemd3        Datusa
     C  N79              Move      Datusa        Mr2edt
     C                   Endif
     C                   Endif
      *
     C                   When      Z = 3
     C                   If        Mr1yr = *Zero
     C                   Eval      Mr1yr   = Xxfyr
     C                   Eval      Mr1eap  = Xxprem
     C                   Eval      Mr1los  = Xxtoti
     C                   Eval      Mr1clm  = Xxtotc
     C                   Eval      Mr1lr   = Xxlrat
     C                   Eval      Mr1fr   = Xxfreq
     C                   Eval      Mr1emo  = Elexm1
     C                   If        Elemd1 > *Zero
     C     *Cymd         Move      Elemd1        Datusa
     C                   Move      Datusa        Mr1edt
     C                   Endif
     C  N78              Eval      Mr1emo  = Elexm2
     C                   If        Elemd2 > *Zero
     C  N78*Cymd         Move      Elemd2        Datusa
     C  N78              Move      Datusa        Mr1edt
     C                   Endif
     C  N79              Eval      Mr1emo  = Elexm3
     C                   If        Elemd3 > *Zero
     C  N79*Cymd         Move      Elemd3        Datusa
     C  N79              Move      Datusa        Mr1edt
     C                   Endif
     C                   Else
     C                   If        Mr2yr = *Zero
     C                   Eval      Mr2yr   = Xxfyr
     C                   Eval      Mr2eap  = Xxprem
     C                   Eval      Mr2los  = Xxtoti
     C                   Eval      Mr2clm  = Xxtotc
     C                   Eval      Mr2lr   = Xxlrat
     C                   Eval      Mr2fr   = Xxfreq
     C                   Eval      Mr2emo  = Elexm1
     C                   If        Elemd1 > *Zero
     C     *Cymd         Move      Elemd1        Datusa
     C                   Move      Datusa        Mr2edt
     C                   Endif
     C  N78              Eval      Mr1emo  = Elexm2
     C                   If        Elemd2 > *Zero
     C  N78*Cymd         Move      Elemd2        Datusa
     C  N78              Move      Datusa        Mr2edt
     C                   Endif
     C  N79              Eval      Mr1emo  = Elexm3
     C                   If        Elemd3 > *Zero
     C  N79*Cymd         Move      Elemd3        Datusa
     C  N79              Move      Datusa        Mr2edt
     C                   Endif
     C                   Else
     C                   Eval      Mr3yr   = Xxfyr
     C                   Eval      Mr3eap  = Xxprem
     C                   Eval      Mr3los  = Xxtoti
     C                   Eval      Mr3clm  = Xxtotc
     C                   Eval      Mr3lr   = Xxlrat
     C                   Eval      Mr3fr   = Xxfreq
     C                   Eval      Mr3emo  = Elexm1
     C                   If        Elemd1 > *Zero
     C     *Cymd         Move      Elemd1        Datusa
     C                   Move      Datusa        Mr3edt
     C                   Endif
     C  N78              Eval      Mr1emo  = Elexm2
     C                   If        Elemd2 > *Zero
     C  N78*Cymd         Move      Elemd2        Datusa
     C  N78              Move      Datusa        Mr3edt
     C                   Endif
     C  N79              Eval      Mr1emo  = Elexm3
     C                   If        Elemd3 > *Zero
     C  N79*Cymd         Move      Elemd3        Datusa
     C  N79              Move      Datusa        Mr3edt
     C                   Endif
     C                   Endif
     C                   Endif
      *
     C                   When      Z = 4
     C                   If        Mr1yr = *Zero
     C                   Eval      Mr1yr   = Xxfyr
     C                   Eval      Mr1eap  = Xxprem
     C                   Eval      Mr1los  = Xxtoti
     C                   Eval      Mr1clm  = Xxtotc
     C                   Eval      Mr1lr   = Xxlrat
     C                   Eval      Mr1fr   = Xxfreq
     C                   Eval      Mr1emo  = Elexm1
     C                   If        Elemd1 > *Zero
     C     *Cymd         Move      Elemd1        Datusa
     C                   Move      Datusa        Mr1edt
     C                   Endif
     C  N78              Eval      Mr1emo  = Elexm2
     C                   If        Elemd2 > *Zero
     C  N78*Cymd         Move      Elemd2        Datusa
     C  N78              Move      Datusa        Mr1edt
     C                   Endif
     C  N79              Eval      Mr1emo  = Elexm3
     C                   If        Elemd3 > *Zero
     C  N79*Cymd         Move      Elemd3        Datusa
     C  N79              Move      Datusa        Mr1edt
     C                   Endif
     C                   Else
     C                   If        Mr2yr = *Zero
     C                   Eval      Mr2yr   = Xxfyr
     C                   Eval      Mr2eap  = Xxprem
     C                   Eval      Mr2los  = Xxtoti
     C                   Eval      Mr2clm  = Xxtotc
     C                   Eval      Mr2lr   = Xxlrat
     C                   Eval      Mr2fr   = Xxfreq
     C                   Else
     C                   If        Mr3yr = *Zero
     C                   Eval      Mr3yr   = Xxfyr
     C                   Eval      Mr3eap  = Xxprem
     C                   Eval      Mr3los  = Xxtoti
     C                   Eval      Mr3clm  = Xxtotc
     C                   Eval      Mr3lr   = Xxlrat
     C                   Eval      Mr3fr   = Xxfreq
     C                   Eval      Mr3emo  = Elexm1
     C                   If        Elemd1 > *Zero
     C     *Cymd         Move      Elemd1        Datusa
     C                   Move      Datusa        Mr3edt
     C                   Endif
     C  N78              Eval      Mr1emo  = Elexm2
     C                   If        Elemd2 > *Zero
     C  N78*Cymd         Move      Elemd2        Datusa
     C  N78              Move      Datusa        Mr3edt
     C                   Endif
     C  N79              Eval      Mr1emo  = Elexm3
     C                   If        Elemd3 > *Zero
     C  N79*Cymd         Move      Elemd3        Datusa
     C  N79              Move      Datusa        Mr3edt
     C                   Endif
     C                   Else
     C                   Eval      Mr4yr   = Xxfyr
     C                   Eval      Mr4eap  = Xxprem
     C                   Eval      Mr4los  = Xxtoti
     C                   Eval      Mr4clm  = Xxtotc
     C                   Eval      Mr4lr   = Xxlrat
     C                   Eval      Mr4fr   = Xxfreq
     C                   Eval      Mr4emo  = Elexm1
     C                   If        Elemd1 > *Zero
     C     *Cymd         Move      Elemd1        Datusa
     C                   Move      Datusa        Mr4edt
     C                   Endif
     C  N78              Eval      Mr1emo  = Elexm2
     C                   If        Elemd2 > *Zero
     C  N78*Cymd         Move      Elemd2        Datusa
     C  N78              Move      Datusa        Mr4edt
     C                   Endif
     C  N79              Eval      Mr1emo  = Elexm3
     C                   If        Elemd3 > *Zero
     C  N79*Cymd         Move      Elemd3        Datusa
     C  N79              Move      Datusa        Mr4edt
     C                   Endif
     C                   Endif
     C                   Endif
     C                   Endif
      *
     C                   Other
     C                   Endsl
      *
     C                   Endsr
      *
      /eject
      *****************************************************************==*
      *   PREMIUM CALCULATIONS         ** SUBROUTINE **   S U B P R   *
      *****************************************************************
      *
     C     Subpr         Begsr
      *
      *     Call PGM TO CALCULATE EARNED PREMIUM YEAR-TO-DATE
      *                       AND EARNED PREMIUM THIS PERIOD
      *
     C                   Movel     XXCO#         SRCO#             3
     C                   Movel     XXFND         SRFND             3
     C                   Movel     XXFYR         SRFYR             3
     C                   Movel     EMEMP#        SREMP#            9
     C                   Movel     EMDIV         SRDIV             5
     C                   Movel     XXDATE        QQDTE             8
     C                   Move      *BLANK        SRAMT            11
     C                   Move      *BLANK        SRAMT2           11
     C                   Move      '1'           QQTERM            1
      *
     C                   Call      'SRPRT1'
     C                   Parm                    SRCO#
     C                   Parm                    SRFND
     C                   Parm                    SRFYR
     C                   Parm                    SREMP#
     C                   Parm                    SRDIV
     C                   Parm                    QQDTE
     C                   Parm                    SRAMT
     C                   Parm                    SRAMT2
     C                   Parm                    QQTERM
      *
     C                   Move      SRAMT         PRMYTD           11 2
     C                   Move      SRAMT2        PRMCUR           11 2
      *
      * CALCULATE LOSS RATIO
      *
     C                   If        Prmytd <> *Zero
     C     XXTOTI        Mult      100           XXITOT           11 2
     C     XXITOT        Div(h)    PRMYTD        Xxlrat            7 2
     C                   Else
     C                   Z-Add     *ZERO         Xxlrat
     C                   Endif
      *
      * CALCULATE FREQUENCY RATIO
      *
     C     PRMYTD        Div(h)    1000          THPREM           11 2
      *
     C                   If        THPREM <> *Zero
     C     XXTOTC        Div(h)    THPREM        XXFREQ            4 2
     C                   Else
     C                   Z-Add     *Zero         XXFREQ
     C                   Endif
      *
     C                   Endsr
      *
      /eject
      *****************************************************************==*
      *
      *****************************************************************
      *
     C     Proc_Year     Begsr
      *
      *
      * READ & PRINT ALL DETAIL LOSS INFOR FOR THE EMPLOYER FOR EACH
      * FUND YEAR.  PRINT HEADING INFO THE FIRST TIME THRU LOOP.
      *
     C     1             Do        4             Z                 2 0
      *
     C                   If        Year(Z) <> *Zero
      *
     C                   Z-Add     Year(Z)       Xxfyr             3 0
      *
     C     Wdell_Key     Chain     Wdell
      *
     C                   If        %Found
      *
      * FLIP EXPERIENCE MOD EFFECTIVE DATES
      *
     C                   Z-Add     ELEMD1        SRDTI
      *
     C                   Call      'SRCVTDAT'    DTMDY
     C                   Movel     SRDTO         XXEMD1            6 0
      *
     C                   Z-Add     ELEMD2        SRDTI
      *
     C                   Call      'SRCVTDAT'    DTMDY
     C                   Movel     SRDTO         XXEMD2            6 0
      *
     C                   Z-Add     ELEMD3        SRDTI
      *
     C                   Call      'SRCVTDAT'    DTMDY
     C                   Movel     SRDTO         XXEMD3            6 0
     C     ELEMD2        COMP      0                                      78
     C     ELEMD3        COMP      0                                      79
      *
      *  READ CLAIM FILE - GET TOTAL CLAIMS, TOTAL INCURRED, LOSS &
      *  FREQ
      *
     C                   Z-Add     0             XXTOTI
     C                   Z-Add     0             XXTOTC
      *
     C     Wmcml_Key     Setll     Wmcml
      *
     C                   Dou       %Eof
      *
     C     Wmcml_Key     Reade     Wmcml
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   ADD       1             XXTOTC            6 0
     C                   Z-Add     *ZERO         TOTPMT           11 2
     C                   Z-Add     *ZERO         TOTRES           11 2
     C                   Xfoot     CMA           TOTPMT
     C                   Xfoot     CMR           TOTRES
     C                   ADD       TOTPMT        XXTOTI           11 2
     C                   ADD       TOTRES        XXTOTI
      *
     C                   Enddo
      *
      *  GET PREMIUM, LOSS RATIO, & FREQUENCY RATIO FOR THE FUND YEAR
      *
     C                   Exsr      SUBPR
      *
      *  ACCUMULATE EMPLOYER TOTALS & PRINT FYR DETAIL
      *
     C                   Z-Add     PRMYTD        XXPREM           11 2
     C                   Add       XXPREM        L1PREM
     C                   Add       XXTOTI        L1TOTI
     C                   Add       XXTOTC        L1TOTC
      *
     C                   Except    Det
     C                   Exsr      Move_Out
      *
     C                   Endif
      *
     C                   Endif
     C                   Enddo
      *
     C                   Endsr
      *
      /eject
      *****************************************************************==*
      *   LOAD DETAIL HEADING INFOR    ** SUBROUTINE **   G E T D E T *
      *****************************************************************
      *
     C     Get_NA        Begsr
      *
      *  LOAD EMPLOYER ADDRESS
      *
      ***** BEG ADD ****** 09/09/03 *******************************************************
      *
     C                   EVAL      XMCSZ  = *BLANKS
     C                   EVAL      XPCSZ  = *BLANKS
      *
     C                   Move      XMZPP1        XXZIP1
     C                   Move      XMZPP2        XXZIP2
      *
     C                   Move      *Blanks       SZ
     C                   Move      *Blanks       ST
     C                   Move      *Blanks       XXZP1
     C                   Move      *Blanks       XXZP2
     C                   Movel     EMMST         XXST
     C                   Movea     XXST          SZ
     C                   Move      XXZIP1        XXZP1
     C                   Movea     XXZP1         SZ(4)
      *
     C                   If        XXZIP2 <> *Zero
     C                   Move      '-'           SZ(9)
     C                   Move      XXZIP2        XXZP2
     C                   Movea     XXZP2         SZ(10)
     C                   Endif
      *
     C                   Move      *Blanks       XXSZ             13
     C                   Movea     SZ            XXSZ
     C
     C                   Move      *Blanks       XXlast           56
     C                   Move      *Blanks       XXFRST           15
     C                   Movel     EmMcty        XXLAST
     C                   Movel     XXSZ          XXFRST
     C                   Move      '1'           XXTERM            1
      *
     C                   Call      'SRNAME'
     C                   Parm                    XXLAST
     C                   Parm                    XXFRST
     C                   Parm                    XXTERM
      *
     C                   Movel     XXLAST        XMCSZ            40
      *
      * If Physical address is not blank, load it else load mailing address
     C                   If        empad1 <> *blanks
      ***** END ADD ****** 09/09/03 *******************************************************
      *
     C                   Move      XXZPP1        XXZIP1            5
     C                   Move      XXZPP2        XXZIP2            4 0
      *
     C                   Move      *Blanks       SZ
     C                   Move      *Blanks       ST
     C                   Move      *Blanks       XXZP1
     C                   Move      *Blanks       XXZP2
     C                   Movel     EMPST         XXST             13
     C                   Movea     XXST          SZ
     C                   Move      XXZIP1        XXZP1             5
     C                   Movea     XXZP1         SZ(4)
      *
     C                   If        XXZIP2 <> *Zero
     C                   Move      '-'           SZ(9)
     C                   Move      XXZIP2        XXZP2             4
     C                   Movea     XXZP2         SZ(10)
     C                   Endif
      *
     C                   Move      *Blanks       XXSZ             13
     C                   Movea     SZ            XXSZ
     C
     C                   Move      *Blanks       XXlast           56
     C                   Move      *Blanks       XXFRST           15
     C                   Movel     Empcty        XXLAST
     C                   Movel     XXSZ          XXFRST
     C                   Move      '1'           XXTERM            1
      *
     C                   Call      'SRNAME'
     C                   Parm                    XXLAST
     C                   Parm                    XXFRST
     C                   Parm                    XXTERM
      *
      ***** BEG CHG ****** 09/09/03 *******************************************************
     C*************      Movel     XXLAST        XXCSZ            40
     C                   Movel     XXLAST        XPCSZ
     C                   Movel     XXLAST        XXCSZ
      ***** END CHG ****** 09/09/03 *******************************************************
      *
     C     EMPAD2        Comp      *Blanks                                38
      *
      ***** BEG ADD ****** 09/09/03 *******************************************************
     C                   movel(P)  empad1        xxadr1
     C                   movel(P)  empad2        xxadr2
      *
     C                   ELSE
      *
     C                   movel(P)  emMad1        xxadr1
     C                   movel(P)  emMad2        xxadr2
     C                   Movel(P)  XMCSZ         XXCSZ
      *
     C     EMMAD2        Comp      *Blanks                                38
      *
     C                   EndIF                                                  empad1 <> Blanks
      ***** END ADD ****** 09/09/03 *******************************************************
      *
      *  GET AGENT# AND NAME FOR THE CURRENT FYR
      *
     C                   CLEAR                   AHAGT#
      *
     C     Wmahl_Key     SETGT     Wmahl
     C     Wmahl_Key     READPE    Wmahl
      *
     C                   DOW       NOT %EOF(Wmahl) and
     C                             AHSTAT = 'I'
     C     Wmahl_Key     READPE    Wmahl
     C                   ENDDO
      *
     C                   CLEAR                   AHAGT#
      *
     C                   MOVE      *BLANKS       AGNAME
     C     Wmaal1_Key    CHAIN     Wmaal1
     C     Wmagl_Key     CHAIN     Wmagl
      *
      *  GET OLDEST EFFECTIVE DATE
      *
     C                   Z-Add     Emefd1        SRDTI
      *
     C                   Call      'SRCVTDAT'    DTMDY
     C                   Movel     SRDTO         XXEFDT            6 0
     C     *Mdy          Move      XXEFDT        INCEPTDT
      *
      *  GET CHAPTER DESCRIPTION
      *
     C                   Move      *Blanks       XXCHAP
      *
     C     Wmcpl_Key     Chain     Wmcpl
      *
     C                   If        %Found
     C                   Movel     CPDESC        XXCHAP           21
     C                   Endif
      *
      *  GET CURRENT ANNUAL PREM & EXP MOD FROM EMP DETAIL
      *
     C                   Z-Add     0             XXANPR           11 2
     C                   Z-Add     0             XXEMEX            5 4
     C                   Z-Add     XXFYR         SVFYR             3 0
     C                   Z-Add     Ebfyr         XXFYR             3 0
     C                   Z-Add     0             XXDEP             9 2
      *
     C     Wdell_Key     Chain     Wdell
      *
     C                   If        %Found
      *
     C     ELEMD1        COMP      0                                      20
     C     ELEMD2        COMP      0                                      21
     C     ELEMD3        COMP      0                                      22
      *
      *** Begin Chg  12/05/02 **********************************************
     C* N20ELEMD1        IFLT      XXYMD
     C     ELEMD1        IFGT      *Zero
      *** End   Chg  12/05/02 **********************************************
     C                   Z-Add     ELEXM1        XXEMEX
     C                   Endif
      *
      *** Begin Chg  12/05/02 **********************************************
     C* N21ELEMD2        IFLT      XXYMD
     C     ELEMD2        IFGT      *Zero
      *** End   Chg  12/05/02 **********************************************
     C                   Z-Add     ELEXM2        XXEMEX
     C                   Endif
      *
      *** Begin Chg  12/05/02 **********************************************
     C* N22ELEMD3        IFLT      XXYMD
     C     ELEMD3        IFGT      *Zero
      *** End   Chg  12/05/02 **********************************************
     C                   Z-Add     ELEXM3        XXEMEX
     C                   Endif
      *
     C                   Z-Add     ELANPR        XXANPR
      *
     C                   Move      *BLANK        XXBLSC
     C                   Movel     ELBSCD        XXBLSC
     C                   Move      '0'           *IN25
      *
     C     BS_Key1       Chain     PBS01                              60
      *
     C                   If        %Found and
     C                             BSSEFL = 'S'
     C                   Move      '1'           *IN25
     C     BSAB01        MULT(H)   ELENMP        XXWK90            9 0
     C                   Z-Add     XXWK90        XXDEP
     C                   Endif
      *
     C                   Endif
      *
      *   RELOAD ORIGINAL EMP DETAIL RECORD
      *
     C                   Z-Add     SVFYR         XXFYR
      *
     C     Wdell_Key     Chain     Wdell                              60
      ***** Beg Add ****** 03/08/04 *******************************************************
     C                   Eval      Xxstaf  = Emuwtr
     C     Wmupl1_Key    Chain     Wmupl1
     C                   Movel     Upusnm        Xxundn           21
     C                   Eval      Xxstaf  = Empsrv
     C     Wmupl1_Key    Chain     Wmupl1
     C                   Movel     Upusnm        Xxplsn           21
      ***** End Add ****** 03/08/04 *******************************************************
      ***** BEG ADD ***** 04/16/04 ****************************************
     C                   movel(p)  emnaob        xxnaob
      ***** END ADD ****** 04/16/04 *******************************************************
      *
     C                   Endsr
      /eject
      *****************************************************************
      *  Build Wfmrp                   ** SUBROUTINE **   S R 1 S T   *
      *****************************************************************
      *
     C     Write_Wfmrp   Begsr
      *
     C                   Movel(p)  Coname        Mrconm
     C                   Movel(p)  Fnname        Mrgrnm
     C                   Eval      Mrdiv   = Emdiv
     C                   Eval      Mrenam  = Emname
     C                   Eval      Mremad1 = Emmad1
     C                   Eval      Mremad2 = Emmad2
      ***** BEG CHG ****** 09/09/03 *******************************************************
     C**************     Eval      Mremcsz = Xxcsz
     C                   Eval      Mremcsz = XMcsz
      ***** END CHG ****** 09/09/03 *******************************************************
     C                   Eval      Mrepad1 = Empad1
     C                   Eval      Mrepad2 = Empad2
      ***** BEG CHG ****** 09/09/03 *******************************************************
     C***********        Eval      Mrepcsz = Empcty
     C                   Eval      Mrepcsz = XPCSZ
      ***** END CHG ****** 09/09/03 *******************************************************
     C                   If        Emefd1 > *Zero
     C     *Cymd         Move      Emefd1        Datusa
     C                   Move      Datusa        Mrindt
     C                   Endif
      *
      ***** beg del ****** 07/16/04 *******************************************************
      *************                                                         ***************
     C*************      Select                                             ***************
      *************                                                         ***************
     C*************      When      Emcad5 > *Zero                           ***************
     C*************      If        Emcad5 > *Zero                           ***************
     C******Cymd***      Move      Emcad5        Datusa                     ***************
     C*************      Move      Datusa        Mrcxdt                     ***************
     C*************      Endif                                              ***************
      *************                                                         ***************
     C*************      When      Emcad4 > *Zero                           ***************
     C*************      If        Emcad4 > *Zero                           ***************
     C******Cymd***      Move      Emcad4        Datusa                     ***************
     C*************      Move      Datusa        Mrcxdt                     ***************
     C*************      Endif                                              ***************
      *************                                                         ***************
     C*************      When      Emcad3 > *Zero                           ***************
     C*************      If        Emcad3 > *Zero                           ***************
     C******Cymd***      Move      Emcad3        Datusa                     ***************
     C*************      Move      Datusa        Mrcxdt                     ***************
     C*************      Endif                                              ***************
      *************                                                         ***************
     C*************      When      Emcad2 > *Zero                           ***************
     C*************      If        Emcad2 > *Zero                           ***************
     C******Cymd***      Move      Emcad2        Datusa                     ***************
     C*************      Move      Datusa        Mrcxdt                     ***************
     C*************      Endif                                              ***************
      *************                                                         ***************
     C*************      When      Emcad1 > *Zero                           ***************
     C*************      If        Emcad1 > *Zero                           ***************
     C******Cymd***      Move      Emcad1        Datusa                     ***************
     C*************      Move      Datusa        Mrcxdt                     ***************
     C*************      Endif                                              ***************
      *************                                                         ***************
     C*************      Other                                              ***************
     C*************      Eval      Mrcxdt = *Zero                           ***************
     C*************      Endsl                                              ***************
      ***** end del ****** 07/16/04 *******************************************************
      ***** Beg add ****** 07/16/04 *******************************************************
      *
      * get cancel date
     c                   exsr      getcxdt
      *
      ***** End add ****** 07/16/04 *******************************************************
      *
     C                   Eval      Mrechp  = Emchap
     C                   Eval      Mrechd  = Cpdesc
     C                   Eval      Mragt#  = Ebagt#
     C                   Eval      Mragnm  = Ebanam
     C                   Eval      MrFein  = EmFein
     C                   Eval      Mrnaob  = Emnaob
      ***** BEG DEL ****** 03/08/04 *******************************************************
      *****  BEGIN ADD *** 03/18/04 ****************************************
     C**********         movel(p)  emnaob        xxnaob
      *****  END ADD ***** 03/18/04 ****************************************
      ***** END DEL ****** 03/08/04 *******************************************************
      *
     C                   Eval      Mrundw  = Emuwtr
      ***** Beg Add ****** 03/08/04 *******************************************************
     C                   Eval      Xxstaf  = Emuwtr
     C     Wmupl1_Key    Chain     Wmupl1
      ***** End Add ****** 03/08/04 *******************************************************
     C                   Eval      Mrundn  = Upusnm
      *
      ***** Beg Add ****** 03/08/04 *******************************************************
     C                   Eval      Mrplsv  = Empsrv
     C                   Eval      Xxstaf  = Empsrv
     C     Wmupl1_Key    Chain     Wmupl1
     C                   Eval      Mrplsn  = Upusnm
      ***** End Add ****** 03/08/04 *******************************************************
      *
     C                   Move      Ebfyr         Mrcpyr
     C                   Clear                   Mrpol#
     C     Cfund         Cat       Cemp#:0       Mrpol#
     C     Mrpol#        Cat       Cfyr:0        Mrpol#
      *
     C                   Eval      Mrpgm   = Q1pgm
     C                   Eval      Mruser  = Q1User
     C                   Eval      Mrdt    = Q1Dt
     C     *Mdy          Move      Q1Dt          Datusa
     C                   Move      Datusa        Mrdt
     C                   Eval      Mrtime  = Q1Time
      *
     C                   Write     Mrp01
     C                   Clear                   Mrp01
      *
      *
     C                   Endsr
      /eject
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
     C                   Kfld                    Xxfnd
      *
      *   KEY LIST FOR THE EMPLOYER FILE
      *
     C     Wmeml_Key1    Klist
     C                   Kfld                    Ebco#
     C                   Kfld                    Ebfnd
     C                   Kfld                    Ebemp#
     C                   Kfld                    Ebdiv
      *
      *  EMPLOYER DETAIL FILE
      *
     C     Wdell_Key     Klist
     C                   Kfld                    Xxco#
     C                   Kfld                    Xxfnd
     C                   Kfld                    Ememp#
     C                   Kfld                    Emdiv
     C                   Kfld                    Xxfyr             3 0
      *
      *   KEY LIST FOR THE CLAIMS FILE
      *
     C     Wmcml_Key     Klist
     C                   Kfld                    XXCO#
     C                   Kfld                    EMFND
     C                   Kfld                    EMEMP#
     C                   Kfld                    EMDIV
     C                   Kfld                    XXFYR
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
     C                   Kfld                    XXCO#
     C                   Kfld                    XXFND
     C                   Kfld                    EBAGT#
      *
      *  KEY LIST FOR THE AGENT MASTER FILE
      *
     C     Wmagl_Key     Klist
     C                   Kfld                    XXCO#
     C                   Kfld                    AAFEIN
     C                   Kfld                    AAMOD
      *
      *  KEY LIST FOR THE TABLE FILE - CHAPTER FILE
      *
     C     Wmcpl_Key     Klist
     C                   Kfld                    XXCO#
     C                   Kfld                    XXFND
     C                   Kfld                    EMCHAP
      *
      * KEY LIST FOR THE BILLING SCHEDULE CODE FILE
      *
     C     BS_Key1       Klist
     C                   Kfld                    XXCO#
     C                   Kfld                    XXBLSC           10
      *
     C     Wmrsl_Key     Klist
     C                   Kfld                    Xxco#
     C                   Kfld                    Elrtcd
      *
     C     Wtddl2_Key    Klist
     C                   Kfld                    Mrco#
     C                   Kfld                    Mrfnd
     C                   Kfld                    Mrcrst
     C                   Kfld                    Eldcde
     C*** BEGIN CHG ***  04/10/06  ************************************
     C*************      KFLD                    Elpfdt
     C                   KFLD                    XXDDT             7 0
     C*** END CHG *****  04/10/06  ************************************
      *
     C     Wtddl2_Key2   Klist
     C                   Kfld                    Mrco#
     C                   Kfld                    Mrfnd
     C                   Kfld                    Mrcrst
     C                   Kfld                    Eldcde
      *
     C     IL_Key        Klist
     C                   Kfld                    Mrco#
     C                   Kfld                    Mrfnd
     C                   Kfld                    Tab10
      *
     C     Wmrpl4_Key    Klist
     C                   Kfld                    Mrco#
     C                   Kfld                    Mrfnd
     C                   Kfld                    Mrcpyr
     C                   Kfld                    Mremp#
      *
     C     Wmeal9_Key    Klist
     C                   Kfld                    Mrco#
     C                   Kfld                    Mrfnd
     C                   Kfld                    Mremp#
      *
      *   KEY LIST FOR WDCXL FILE
      *
     C     Wdcxl_Key     Klist
     C                   Kfld                    Ebco#
     C                   Kfld                    Ebfnd
     C                   Kfld                    Xxfyr
     C                   Kfld                    Ebemp#
     C                   Kfld                    Ebdiv
      *****  BEGIN ADD *** 04/24/03 ****************************************
      *
     C     Wxebl1_Key    Klist
     C                   Kfld                    Xxco#
     C                   Kfld                    XXfnd
     C                   Kfld                    Xxcfyr
     C                   Kfld                    Xxemp#
     C                   Kfld                    Xxdiv
      *****  END   ADD *** 04/24/03 ****************************************
     *
      *****  BEG ADD ***** 12/05/03 ****************************************
      *
     C     Wxebl1_Ky2    Klist
     C                   Kfld                    Xxco#
     C                   Kfld                    XXfnd
     C                   Kfld                    Xxcfyr
     C                   Kfld                    Xxemp#
      *****  END ADD ***** 12/05/03 ****************************************
      ***** Beg Add ****** 03/08/04 *******************************************************
      *
      *   KEY LIST FOR THE USER PARAMETER FILE
      *
     C     Wmupl1_Key    Klist
     C                   Kfld                    Xxstaf
      ***** End Add ****** 03/08/04 *******************************************************
     *
     C     *Like         Define    EMEMP#        XXEMP#
     C     *Like         Define    EMDIV         XXDIV
     C     *Like         Define    EMDT          XXYMD
     C     *Like         Define    EMDT          Currdt
     C     *Like         Define    Ebco#         Svco#
     C     *Like         Define    Ebfnd         Svfnd
     C     *Like         Define    Ebemp#        Svemp#
      *** Begin Add  10/25/02 **********************************************
     C     *Like         Define    Ebdiv         Svdiv
      *** End   Add  10/25/02 **********************************************
     C     *Like         Define    RsJrst        Risk_St
      ***** Beg Add ****** 03/08/04 *******************************************************
     C     *Like         Define    Upstaf        Xxstaf
      ***** End Add ****** 03/08/04 *******************************************************
      *
     C     *Entry        Plist
     C                   Parm                    QQCO#             3
     C                   Parm                    QQFND             3
     C                   Parm                    QQCFYR            3
     C                   Parm                    QQFYR1            3
     C                   Parm                    QQFYR2            3
     C                   Parm                    QQFYR3            3
     C                   Parm                    QQFYR4            3
     C                   Parm                    QQDTE             8
     C                   Parm                    QQEMP#            9
     C                   Parm                    QQDIV             5
     C                   Parm                    QQfrdt
     C                   Parm                    QQtodt
     C                   Parm                    QQtypo
     C                   Parm                    QQagnt
     C                   Parm                    QQchap
     C                   Parm                    QQundw
     C                   Parm                    QQprst
     C                   Parm                    QQpsrt
     C                   Parm                    QQssrt
     C                   Parm                    QQstat
     C                   Parm                    QQQUE            10
     C                   Parm                    QQMODE            1
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
     C                   Select
     C                   When      Qqpsrt = 'EX'
     C                   Movel(P)  Srt1          QQpsrtd
     C                   When      Qqpsrt = 'AN'
     C                   Movel(P)  Srt2          QQpsrtd
     C                   When      Qqpsrt = 'EC'
     C                   Movel(P)  Srt3          QQpsrtd
     C                   When      Qqpsrt = 'UW'
     C                   Movel(P)  Srt4          QQpsrtd
     C                   When      Qqpsrt = 'ST'
     C                   Movel(P)  Srt5          QQpsrtd
     C                   When      Qqpsrt = 'EN'
     C                   Movel(P)  Srt6          QQpsrtd
     C                   Endsl
      *
     C                   Select
     C                   When      Qqssrt = 'EX'
     C                   Movel(P)  Srt1          QQssrtd
     C                   When      Qqssrt = 'AN'
     C                   Movel(P)  Srt2          QQssrtd
     C                   When      Qqssrt = 'EC'
     C                   Movel(P)  Srt3          QQssrtd
     C                   When      Qqssrt = 'UW'
     C                   Movel(P)  Srt4          QQssrtd
     C                   When      Qqssrt = 'ST'
     C                   Movel(P)  Srt5          QQssrtd
     C                   Endsl
      *
     C                   If        QQagnt <> *Blanks
     C                   Move      QQagnt        QQagnt2           5 0
     C                   Else
     C                   Eval      QQagnt2 = *Zero
     C                   Endif
      *
     C                   If        QQchap <> *Blanks
     C                   Move      QQchap        QQchap2           3 0
     C                   Else
     C                   Eval      QQchap2 = *Zero
     C                   Endif
      *
     C                   Eval      Datechk = *Off
     C                   If        QQfrdt <> *Blanks and
     C                             QQtodt <> *Blanks
     C     *Usa0         Move      QQfrdt        Frdate
     C     *Cymd         Move      Frdate        Fromdt
     C     *Usa0         Move      QQtodt        Todate
     C     *Cymd         Move      Todate        Todt
     C                   Eval      Datechk = *On
     C                   Endif
      *
     C                   Move      QQdte         SRDTI
      *
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
     C*** BEG ADD *** 09/24/04 ******************************          ******
     C*
     C     chgpl         Plist
     C                   Parm                    XXNUM            80
     C                   Parm                    EDTMSK           10
     *
     C*** END ADD *** 09/24/04 ***********************************     ******
     C                   Move      QQCO#         XXCO#             3 0
     C                   Move      QQFND         XXFND             3 0
     C                   Move      QQCFYR        XXCFYR            3 0
     C                   Move      QQFYR1        XXFYR1            3 0
     C                   Move      QQFYR2        XXFYR2            3 0
     C                   Move      QQFYR3        XXFYR3            3 0
     C                   Move      QQFYR4        XXFYR4            3 0
     C                   Move      QQDTE         XXDATE            8 0
     C                   Move      QQEMP#        XXEMP#
     C                   Move      QQDIV         XXDIV
     C                   Move      QQMODE        XXMODE            1
     *
      *****  BEG ADD ***** 12/05/03 ****************************************
      * If want single employer and div is blank, do all divisions
     C                   IF        QQDIV = *BLANKS and XXEMP# <> *ZEROS OR
     C                             QQDIV = *ZEROS  AND XXEMP# <> *ZEROS
     C                   EVAL      ALLDIV = *ON
     C                   ELSE
     C                   EVAL      ALLDIV = *OFF
     C                   ENDIF                                                  QQDIV = BLANKS
      *****  END ADD ***** 12/05/03 ****************************************
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
      **** Begin Add **** 12/17/2002 ***************************************
     C     *Mdy          Move      Dsdate        Today
      **** End   Add **** 12/17/2002 ***************************************
      *
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
      *
     C     Wmupl_Key     Chain     Wmupl                              60
     C     Smcol_Key     Chain     Smcol                              60
     C     Wmfnl_Key     Chain     Wmfnl                              60
      *
      * OPEN PRINT FILE
      *
     C                   Open      Qsysprt
      *
      * GET THE DISTRIBUTION INFORMATION
      *
     C                   Move      '0'           XXPRT
      *
     C     Smorl_Key     Chain     Smorl                              60
      *
     C                   If        *IN60 = *Off
     c*** Beg Add ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     c*** End add ***** 01/28/10 **************************************
      ***** Beg Del ***** 08/26/14 ************************************
     C************       Except    Route
      ***** End Del ***** 08/26/14 ************************************
     C                   Endif
      *
     C                   Move      '1'           *INOF
      *
      *  LOAD YEAR ARRAY & SORT
      *
     C                   Z-Add     0             Year
     C                   Move      XXFYR1        Year(1)
     C                   Move      XXFYR2        Year(2)
     C                   Move      XXFYR3        Year(3)
     C                   Move      XXFYR4        Year(4)
     C                   Sorta     Year
      *
     C                   Endsr
      ***** Beg add ****** 07/16/04 *******************************************************
      *
     C*
     C*** BEG ADD *** 09/24/04 *****************************
     C*
      *================================================================
      * Getagt - Get Agent info
      *================================================================
      *
     C     Getagt        Begsr
     C*
     C     Wmfnl_Key     Chain     Wmexl                              60
     C                   If        *IN60 = *Off
     C                   If        EXAHDR = *blanks
     C                   Movel(P)  'AGENT'       EXAHDR
     C                   Endif
     C*
     C                   Else
     C                   Movel(P)  'AGENT'       EXAHDR
     C                   Endif
     C*
     C                   Movel(P)  EBAGT#        XXNUM
     C                   Move      *BLANKS       EDTMSK
     C                   Call      CHGNUM        CHGPL
     C                   Movel(P)  XXNUM         XPAGT#            5
     C*
     C     EXAHDR        Cat(P)    ':':0         XPAHDR           75
     C                   Cat       XPAGT#:1      XPAHDR
     C                   Cat       EBANAM:2      XPAHDR
     C*
     C                   Endsr
     C*** END ADD *** 09/24/04 *****************************
      *
      *
      *================================================================
      * Getcxdt - Get Cancellation date
      *================================================================
      *
     C     Getcxdt       Begsr
      *
     C                   Eval      *In45 = *Off
     C                   Eval      *In46 = *Off
     C                   Clear                   Cnxdate
     C                   Clear                   Mrcxdt
      *
     C                   Select
      *
     C                   When      Emefd5 > *Zero
     C                   if        Emcad5 > *Zero
     C     *Cymd         Move      Emcad5        Datusa
     C     *Usa          Move      Datusa        Cnxdate
     C                   Eval      *In46 = *On
     C                   Move      Datusa        Mrcxdt
     C                   Eval      *In45 = *On
     c                   endif
      *
     C                   When      Emefd4 > *Zero
     C                   if        Emcad4 > *Zero
     C     *Cymd         Move      Emcad4        Datusa
     C     *Usa          Move      Datusa        Cnxdate
     C                   Eval      *In46 = *On
     C                   Move      Datusa        Mrcxdt
     C                   Eval      *In45 = *On
     C                   Endif
      *
     C                   When      Emefd3 > *Zero
     C                   if        Emcad3 > *Zero
     C     *Cymd         Move      Emcad3        Datusa
     C     *Usa          Move      Datusa        Cnxdate
     C                   Eval      *In46 = *On
     C                   Move      Datusa        Mrcxdt
     C                   Eval      *In45 = *On
     C                   Endif
      *
     C                   When      Emefd2 > *Zero
     C                   if        Emcad2 > *Zero
     C     *Cymd         Move      Emcad2        Datusa
     C     *Usa          Move      Datusa        Cnxdate
     C                   Eval      *In46 = *On
     C                   Move      Datusa        Mrcxdt
     C                   Eval      *In45 = *On
     C                   Endif
      *
     C                   When      Emefd1 > *Zero
     C                   if        Emcad1 > *Zero
     C     *Cymd         Move      Emcad1        Datusa
     C     *Usa          Move      Datusa        Cnxdate
     C                   Eval      *In46 = *On
     C                   Move      Datusa        Mrcxdt
     C                   Eval      *In45 = *On
     C                   Endif
      *
     C                   Endsl
      *
     c                   Endsr
      *
      ***** End add ****** 07/16/04 *******************************************************
      /eject
     OQSYSPRT   E            Route          1 06
      ***** Beg Del ***** 08/26/14 *****************************************************
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
     O********* E            Route          1
     O*********      08      ORDIS2              60
     O********* E            Route          1
     O*********      08      ORDIS3              60
     O********* E            Route          2
     O*********      08      ORDIS4              60
      ***** End Del ***** 08/26/14 *****************************************************
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
     O          E            Route          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 *****************************************************
     O********* E            Route          2
     O*********                                  19 'PROGRAM NAME:'
     O*********              ORPNAM              30
     O*********                                  36 'FILE:'
     O*********              ORFIL#              38
     O*********                                  46 'COPIES:'
     O*********              ORCOPY        Z     50
     O*********                                  56 'HOLD:'
     O*********              ORHOLD              61
     O********* E            Route          2
     O*********                                  19 'OUTPUT QUEUE:'
     O*********              OROUTQ              30
     O*********                                  37 'FORMS:'
     O*********              ORFORM              48
     O*********                                  56 'SAVE:'
     O*********              ORSAVE              61
      ***** End Del ***** 08/26/14 *****************************************************
      *
      * CHANGED ROUTING PAGE TO COMPLY WITH VERSION 3 STANDARDS.
      *
     O          E            Route       2  2
     O                                           46 'REPORT SELECTIONS'
     O          E            Route          2
     O                                         +  6 'POLICY YEAR  . . . . . . :'
     O                       XXCFYR        Z   +  1
     O          E            Route          2
     O                                         +  6 'EMPLOYER/DIVISION  . . . :'
     O              N40      XXEMP#            +  1
     O              N40      XXDIV             +  1
     O               40                          59 'Selected by exp date range'
     O          E            Route          2
     O                                         +  6 'POLICY EXP DATE RANGE    :'
     O                       Frdate            +  1
     O                                         +  2 '-'
     O                       Todate            +  2
     O          E            Route          2
     O                                         +  6 'POLICY YEARS . . . . . . :'
     O                       XXFYR1        Z   +  1
     O                       XXFYR2        Z   +  2
     O                       XXFYR3        Z   +  2
     O                       XXFYR4        Z   +  2
     O          E            Route          2
     O                                         +  6 'RUN DATE . . . . . . . . :'
     O                       XXDATE        Y   +  1
     O          E            Route          2
     O                                         +  6 'GROUP  . . . . . . . . . :'
     O                       XXFND         Z   +  1
      *
     O          E            Route          2
     O                                         +  6 'AGENT  . . . . . . . . . :'
     O              N41      Qqagnt            +  1
     O               41                          36 'All'
      *
     O          E            Route          2
     O                                         +  6 'CHAPTER  . . . . . . . . :'
     O              N42      Qqchap            +  1
     O               42                          36 'All'
      *
     O          E            Route          2
     O                                         +  6 'UNDERWRITER  . . . . . . :'
     O              N43      Qqundw            +  1
     O               43                          36 'All'
      *
     O          E            Route          2
     O                                         +  6 'PREMIUM RATING STATE . . :'
     O              N44      Qqprst            +  1
     O               44                          36 'All'
      *
      ******* beg chg ****** 10/11/02 *********************************
     O*******   E            Route          2
     O          E    40      Route          2
      ******* end chg ****** 10/11/02 *********************************
     O                                         +  6 'PRIMARY SORT . . . . . . :'
     O                       Qqpsrtd           +  1
      *
      ******* beg chg ****** 10/11/02 *********************************
     O*******   E            Route          2
     O          E    40      Route          2
      ******* end chg ****** 10/11/02 *********************************
     O                                         +  6 'SECONDARY SORT . . . . . :'
     O                       Qqssrtd           +  1
      *
     O*** Beg Chg *****  01/28/10  ************************************
     O***       E            Route          1 60
     O***                                        23 '(C) COPYRIGHT PROGRESSI'
     O***                                        46 'VE DATA SOLUTIONS. INC,'
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     O*** End Chg *****  01/28/10  ************************************
      *
      * MAIN PAGE HEADING
      *
     O          E            HEAD           1 04
     O                                           45 'MEMBER EVALUATION'
     O                                           55 'WORKSHEET'
      *
     O          E            HEAD        1  1
     O                                           08 'GROUP:'
     O                       FNFND             +  4
     O                       FNNAME            +  2
      *
      ******* beg del ****** 10/11/02 *********************************
     O********* E            HEAD           1 08
     O*********                                  10 'POLICY :'
     O*********              EMNAME            +  1
     O*********                                  67 'POLICY# . . . :'
     O*********              EMEMP#        Z     76
     O*********              EMDIV               82
     O********* E            HEAD           1
     O*********                                  67 'INCEPTION DATE:'
     O*********              INCEPTDT          +  5
     O********* E            HEAD           1
     O*********                                  10 'ADDRESS:'
     O*********              EMPAD1            +  1
     O*********                                +  1 'CHAPTER . . . :'
     O*********              XXCHAP            +  1
     O********* E            HEAD           1
     O*********     N38      EMPAD2              51
     O*********      38      XXCSZ               51
     O*********                                +  1 'UNDERWRITER   :'
     O*********              EMUWTR            +  1
     O********* E            HEAD           1
     O*********     N38      XXCSZ               51
     O********* E            HEAD           1
      ******* end del ****** 10/11/02 *********************************
      ******* beg add ****** 10/11/02 *********************************
      *
     O          E            HEAD           1 08
     O                                           10 'POLICY :'
     O                       EMNAME            +  1
     O                                           65 'POLICY# . . :'
     O                       EMEMP#        4   +001
     O                       EMDIV         4   +001
      *
     O          E            HEAD           1
     O                                           65 'INCEPTION DT:'
     O                       INCEPTDT          +  1
      *
      *****  BEGIN ADD *** 04/24/03 ****************************************
     O          E    46      HEAD           1
     O                                           65 'CANCEL DT   :'
     O                       Cnxdate           +  1
      *****  END   ADD *** 04/24/03 ****************************************
      *
     O          E            HEAD           1
     O                                           10 'ADDRESS:'
      ***** BEG CHG ****** 09/09/03 *******************************************************
     O***********            EMPAD1            +  1
     O                       XXADR1            +  1
      ***** END CHG ****** 09/09/03 *******************************************************
     O                                         +  1 'CHAPTER . . :'
     O                       XXCHAP            +  1
      *
     O          E            HEAD           1
      ***** BEG CHG ****** 09/09/03 *******************************************************
     O***********   N38      EMPAD2              51
     O              N38      XXADR2              51
      ***** END CHG ****** 09/09/03 *******************************************************
     O               38      XXCSZ               51
     O                                         +  1 'UNDERWRITER :'
      ***** Beg Chg ****** 03/08/04 *******************************************************
     O***********************EMUWTR            +  1
     O                       XXUNDN            +  1
      ***** End Chg ****** 03/08/04 *******************************************************
      *
     O          E            HEAD           2
     O              N38      XXCSZ               51
      ***** Beg Add ****** 03/08/04 *******************************************************
     O                                         +  1 'POLICY SVCS :'
     O                       XXPLSN            +  1
      ***** End Add ****** 03/08/04 *******************************************************
      *
      ******* end add ****** 10/11/02 *********************************
     O          E            HEAD           1
     O*** BEG DEL *** 09/24/04 *****************************
     O*****                                       8 'AGENT:'
     O*****                  AHAGT#        Z   +  1
     O*****                  EBAGT#        Z   +  1
     O*****                  AGNAME            +  1
     O*****                  EBANAM            +  1
     O*** END DEL *** 09/24/04 *****************************
     O*** BEG ADD *** 09/24/04 *****************************
     O                       XPAHDR              77
     O*** END ADD *** 09/24/04 *****************************
     O          E            HEAD           1
     O          E            HEAD           2
     O                                           20 'DESC OF OPERATION:'
      *
      *****  BEGIN CHG *** 10/13/03 ****************************************
     O************           Emnaob            +  1
     O                       xxnaob            +  1
      *****  END CHG ***** 10/13/03 ****************************************
     O                                           67 'EMPLOYER FEIN :'
     O                       EmFein        Z   +  2
     O          E            HEAD           1
     O                                           22 '____________________'
     O                                         +  0 '____________________'
     O                                         +  0 '____________________'
     O                                         +  0 '____________________'
     O          E            HEAD           2
     O                                           26 'CURRENT YEAR INFORMATION'
     O                                              ' AS OF'
     O                       Xxdate        Y   +  1
     O          E            HEAD           1
     O                                           18 'EST ANNUAL PREM:'
     O                       Xxanpr        J   +  1
     O                                           50 'EXPERIENCE MOD:'
     O                       Xxemex        3   +  1
      ***** BEG ADD **** 07/23/03 ******************************************
     O                       XXCEMD            +  2 '  /  /    '
      ***** END ADD **** 07/23/03 ******************************************
     O          E            HEAD           1
     O                                           18 'SCHED DR/CR    :'
     O                       Eldbcr        J   +  9
     O                                           50 'INCR LIMITS   :'
     O                       Mrcild            +  1
     O          E            HEAD           1
     O                                           18 'A/R BALANCE    :'
     O                       Mrcarb        J   +  1
     O                                         +  1 'PAY PLAN:'
     O                       Pbsdesc           +  1
      **** Begin Chg **** 12/17/2002 ***************************************
     O*         E            HEAD           2
     O          E            HEAD           1
      **** End   Chg **** 12/17/2002 ***************************************
     O                                           18 '# OF CNX . . . :'
     O                       Mrc#cx        J   +  9
     O                                           44 'DED PLAN:'
     O                       Mrcdds            +  1
      **** Begin Add **** 12/17/2002 ***************************************
     O          E            HEAD           1
     O                  45                       18 'CANCELLATION DT:'
     O                  45   Mrcxdt            +  1 '  /  /    '
      **** End   Add **** 12/17/2002 ***************************************
     O          E            HEAD           2
     O                                           18 'PREM RTG STATE :'
     O                       Ebjrst            + 13
     O                                           49 'POLICY PERIOD:'
     O                       Pfdt              +  1
     O                                         +  1 ' - '
     O                       Ptdt              +  1
     O          E    25      HEAD           1
     O               25                        +  2 'PREMIUM DEPOSIT:'
     O               25      Xxdep               33 '  ,   , $0.  '
      *
     O          E            HEAD           1
     O                                           22 '____________________'
     O                                         +  0 '____________________'
     O                                         +  0 '____________________'
     O                                         +  0 '____________________'
      *
     O          E            HEAD           2
     O                                         +  2 'LOSS HISTORY POLICY YEARS'
      *
     O          E            HEAD           2
     O                                            6 'YEAR'
     O                                           22 'PREMIUM'
     O                                           37 'LOSSES'
     O                                           47 '#CLAIMS'
     O                                           54 'L/R'
     O                                           63 'FREQ'
     O                                           78 'EXP MOD   DATE'
      *
     O          E            DET            1
     O                       XXFYR         Z     05
     O                       XXPREM        J     23
     O                       XXTOTI        J     38
     O                       XXTOTC        1     45
     O                       Xxlrat        3     54
     O                       XXFREQ        3     63
     O                       ELEXM1        3     70
     O                       XXEMD1        Y     80
      *
     O          E      N78   DET            1
     O                       ELEXM2        3     70
     O                       XXEMD2        Y     80
      *
     O          E      N79   DET            1
     O                       ELEXM3        3     70
     O                       XXEMD3        Y     80
      *
      ******* beg chg ****** 10/11/02 *********************************
     O********  E            TOT         2       39
     O          E            TOT         2  2
      ******* end chg ****** 10/11/02 *********************************
     O                                           07 'TOTAL'
     O                       L1PREM        J     23
     O                       L1TOTI        J     38
     O                       L1TOTC        1     45
     O                       L1LOSS        3     54
     O                       L1FREQ        3     63
      *
     O          E            TOT            1
     O                                           22 '____________________'
     O                                         +  0 '____________________'
     O                                         +  0 '____________________'
     O                                         +  0 '____________________'
      *
     O          E            TOT            2
     O                                           23 'UNDERWRITING RECOMMEN'
     O                                           43 'DATIONS, ACTIONS AND'
     O                                           52 'COMMENTS'
      *
     O          E            TOT1           2
     O                                           22 '____________________'
     O                                         +  0 '____________________'
     O                                         +  0 '____________________'
     O                                         +  0 '____________________'
      *
     O          E            TOT2           2 62
     O                                           13 'UNDERWRITER'
     O                                           38 '________________________'
     O                                           62 '________________________'
     O                                           64 '__'
     O                                           69 'DATE'
     O                                           82 '____________'
      *
