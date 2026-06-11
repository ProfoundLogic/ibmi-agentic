     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
    H*****************************************************************
    H/TITLE W8050R - REPORT - Generate A/R adjustment batch entry
    H*****************************************************************
    H*                                                               *
    H* CALLED FROM: W6000C                                           *
    H*                                                               *
    H* CALLS TO:    none                                             *
    H*                                                               *
    H* CREATED: 10/18/00  C:61  PROJECT  109BG                       *
    H*****************************************************************
    H* REVISIONS:                                                    *
    H*                                                               *
     H* 01/20/15 C:295 PROJECT: 129WTI                                *
     H*                Add Assessments to Auto Write-Off              *
     H*                                                               *
    H* 11/26/14 C:136 Support: WA129 87464                           *
    H*               -Change Leave statement to Iter so that         *
    H*                pgm doesn't bail out of group when the         *
    H*                first policy gets filtered out                 *
    H*                                                               *
    H* 08/22/14 C:136 Support: WA129 86384                           *
    H*               -Flip sign on write-off adjustment              *
    H*               -Adjust write-off logic                         *
    H*                                                               *
    H* 02/08/14 C:136 Project: 129WSB (No comments)                  *
     H*               - Archived prv version of 129WSB to
     H*                 V4R0PNSRCA/QRPGLESRC/W8050R0001
    H*               - Modified to accumulate balances through the   *
    H*                 entire policy year.                           *
    H*               - Modified to accumulate balances for each cat- *
    H*                 egory.                                        *
    H*                                                               *
    H* 06/24/05 C:33  Project: 109WDZ                                *
    H*                {109BG, 109CW}                                 *
    H*               -Move from client environment to base.          *
    H*               -Convert to RPGIV.                              *
    H*               -Removed comment lines.                         *
    H*                                                               *
    H* 04/13/04 C:132 PROJECT:  WC1190  WA109 42171                  *
    H*              - On ADD, populate Inital Date, User & Pgm       *
    H*                                                               *
    H* 12/16/01 C:09  PROJECT 109CW/Seq # 30204                      *
    H*                Do not address accounts with ZERO balance.     *
    H*                Otherwise, determine overall account balance   *
    H*                (including deposit, fees and premium) if the   *
    H*                balance is within threshold and not zero,      *
    H*                then post an adjustment to premium for the     *
    H*                offset. Do not address deposit, fee and        *
    H*                premium balances separately as before per      *
    H*                Laura C.                                       *
    H*                                                               *
    H* 12/13/01 C:09  PROJECT 109CW/Seq # 30204                      *
    H*                Per request from Laura C/Eunice D, do not      *
    H*                generate ANY write-offs until further notice.  *
    H*                                                               *
    H* 10/29/01 C:09  PROJECT 109CW/Seq # 29515                      *
    H*                Modified processing to ensure that ALL policy  *
    H*                years are checked for "small" A/R balances,    *
    H*                not just the "current" year (based on the      *
    H*                Daily Batch run date).                         *
    H*                                                               *
    H* 06/27/01 C:90  PROJECT 109CW                                  *
    H*                Changed the processing of the program to       *
    H*                account for all 3 types of balances (premium,  *
    H*                deposit, fee) in determining if a write-off    *
    H*                needs to be generated.                         *
    H*****************************************************************
     * Group Auxiliary
     fwdf2l     if   e           k disk    rename(pf201:pf200)
     *
     * Employer Detail
     Fwdell     if   e           k disk    rename(pel01:pel00)
     *
     * Accounts Receivable
     Fwmeal6    if   e           k disk    rename(pea01:pea06)
     *
     * A/R Batch
     Fwdeal4    uf a e           k disk    rename(pea01:pea04)
     F                                     prefix(e4:2)
     *
     * TABLE - PTT01 - A/R TRANSACTION TYPE
     Fwtabl     if   e           k disk
     F                                     ignore(ppc01)
     F                                     ignore(prc01)
     F                                     ignore(pbc01)
     F                                     ignore(pcs01)
     *
     D psds          esds                  extname(szq1p)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@bat#          s              9
     D @@co#           s              3
     D @@date          s              8
     D kkkey           s                   like(ttkey)
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D sysdat          s              7  0
     D xxbal           s             15  5
     D xxbata          s             15  5
     D xxco#           s                   like(elco#)
     D xxcode          s                   like(F2dbcd)
     D xxdate          s              7  0
     D xxdbal          s             15  5
     D xxdbil          s             15  5
     D xxdesc          s                   like(ttdesc)
     D xxdpmt          s             15  5
     D xxfbal          s             15  5
     D xxfbil          s             15  5
     D xxfpmt          s             15  5
     D xxnxt           s                   like(e4bat#)
     D xxpbal          s             15  5
     D xxpbil          s             15  5
     D xxppmt          s             15  5
     D xxsycd          s                   like(e4sycd)
     D xxtbal          s             15  5
     D x1date          s              8  0
     D******* Beg add ****** 01/20/15 *************************************
     D xx1Bal          s             15  5
     D xx2Bal          s             15  5
     D xx3Bal          s             15  5
     D xx4Bal          s             15  5
     D xx5Bal          s             15  5
     D xx6Bal          s             15  5
     D xx7Bal          s             15  5
     D******* End add ****** 01/20/15 *************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*
     D/copy sPrDate
     *
     *================================================================
     *   KEY LISTS
     *================================================================
     *
     *   Company
     *
     C     keySMCOL      klist
     C                   kfld                    xxco#
     *
     *  Employer
     *
     C     keyWDELL      klist
     C                   kfld                    xxco#
     C                   kfld                    F2fnd
     *
     *  Accounts Recievable
     *
     C     keyWMEAL6     klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
     C                   kfld                    eldiv
     C                   kfld                    elfyr
     *
     *  A/R Transaction Type
     *
     C     keyTable      klist
     C                   kfld                    xxco#
     C                   kfld                    kkkey
     *
     *  A/R Batch
     *
     C     keyWDEAL4     klist
     C                   kfld                    xxco#
     *
     C     key1WDEAL4    klist
     C                   kfld                    xxco#
     C                   kfld                    xxnxt
     *
     *  System A/R Batch Code
     *
     C     syky          klist
     C                   kfld                    xxco#
     *================================================================
     *   PARAMETER LISTS INCLUDING ENTRY PARMS
     *================================================================
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@date
     C                   parm                    @@bat#
     *   Initialize
     C                   exsr      pgmstr
     * Check for Threshold Adjustment
      /free
       Setll xxCo# pf200;
       Reade xxCo# pf200;
       Dow not %eof;
          // Get description associated with A/R code specified for
          // threshold adjustment transfers
          If F2thrl <> *zero or F2thrh <> *zero;
             // Read Employer Policy Years
             Setll (xxCo#:F2fnd) pel00;
             Reade (xxCo#:F2fnd) pel00;
             Dow not %eof;
                Exsr srArB;

                If xxTBal  <  F2thrl or xxTBal >  F2thrh or xxTbal = *zero
                 or xxPBal <  F2thrl or xxPBal >  F2thrh
                 or xxFBal <  F2thrl or xxFBal >  F2thrh
          //*** beg chg ***** 01/20/15 ********************
          //***  or xxDBal <  F2thrl or xxDBal >  F2thrh;
                 or xxDBal <  F2thrl or xxDBal >  F2thrh
                 or xx1Bal <  F2thrl or xx1Bal >  F2thrh
                 or xx2Bal <  F2thrl or xx2Bal >  F2thrh
                 or xx3Bal <  F2thrl or xx3Bal >  F2thrh
                 or xx4Bal <  F2thrl or xx4Bal >  F2thrh
                 or xx5Bal <  F2thrl or xx5Bal >  F2thrh
                 or xx6Bal <  F2thrl or xx6Bal >  F2thrh
                 or xx7Bal <  F2thrl or xx7Bal >  F2thrh;
          //*** end add ***** 01/20/15 ********************
                   Reade (xxCo#:F2fnd) pel00;
                   Iter;
                Endif;

                If xxPBal >= F2thrl and xxPBal <= F2thrh;
                   kkKey  = F2pbcd;
                   xxCode = F2pbcd;
                   xxbal  = xxPBal;
                   Exsr wrBat;
                Endif;

                If xxDBal >= F2thrl and xxDBal <= F2thrh;
                   kkKey  = F2dbcd;
                   xxCode = F2dbcd;
                   xxbal  = xxDBal;
                   Exsr wrBat;
                Endif;

                If xxFBal >= F2thrl and xxFBal <= F2thrh;
                   kkKey  = F2fbcd;
                   xxCode = F2fbcd;
                   xxbal  = xxFBal;
                   Exsr wrBat;
                Endif;

       //***** Beg Add ***** 01/20/15 ********************************
                If xx1Bal >= F2thrl and xx1Bal <= F2thrh;
                   kkKey  = F21acd;
                   xxCode = F21acd;
                   xxbal  = xx1Bal;
                   Exsr wrBat;
                Endif;

                If xx2Bal >= F2thrl and xx2Bal <= F2thrh;
                   kkKey  = F22acd;
                   xxCode = F22acd;
                   xxBal  = xx2Bal;
                   Exsr wrBat;
                Endif;

                If xx3Bal >= F2thrl and xx3Bal <= F2thrh;
                   kkKey  = F23acd;
                   xxCode = F23acd;
                   xxBal  = xx3Bal;
                   Exsr wrBat;
                Endif;

                If xx4Bal >= F2thrl and xx4Bal <= F2thrh;
                   kkKey  = F24acd;
                   xxCode = F24acd;
                   xxBal  = xx4Bal;
                   Exsr wrBat;
                Endif;

                If xx5Bal >= F2thrl and xx5Bal <= F2thrh;
                   kkKey  = F25acd;
                   xxCode = F25acd;
                   xxBal  = xx5Bal;
                   Exsr wrBat;
                Endif;

                If xx6Bal >= F2thrl and xx6Bal <= F2thrh;
                   kkKey  = F26acd;
                   xxCode = F26acd;
                   xxBal  = xx6Bal;
                   Exsr wrBat;
                Endif;

                If xx7Bal >= F2thrl and xx7Bal <= F2thrh;
                   kkKey  = F27acd;
                   xxCode = F27acd;
                   xxBal  = xx7Bal;
                   Exsr wrBat;
                Endif;

       //***** End Add ***** 01/20/15 ********************************
             Reade (xxCo#:F2fnd) pel00;
             Enddo;
          Endif;
       Reade xxCo# pf200;
       Enddo;
      /end-free
     c                   if        xxBata = 0
     c                   clear                   @@bat#
     c                   else
     c                   eval      @@Bat# = %editc(xxNxt:'X')
     c                   endif
     c     key1WDEAL4    setll     pea04
     c     key1WDEAL4    reade     pea04
     c                   dow       not(%eof)
     c                   if        E4fnd <> 0
     c                   eval      e4bata = xxbata
     c                   update    pea04
     c                   else
     c                   delete    pea04                                        Delete placeholder
     c                   endif
     c     key1WDEAL4    reade     pea04
     c                   enddo
     c                   move      *on           *inlr
     *================================================================
     * WRBAT - Write A/R Batch entry to offset Premium Balance
     *================================================================
     C     wrbat         begsr
     *
     C                   if        xxbal = 0
     C                   leavesr
     C                   endif
     C                   if        kkkey = *blank
     C                   leavesr
     C                   endif
     C                   eval      xxbata = xxbata + xxbal
     C     keyTable      chain     ptt01                              61
     C                   clear                   pea04
     C                   eval      E4co#  = ELco#
     C                   eval      E4bat# = xxNxt
     C                   eval      E4fnd  = ELfnd
     C                   eval      E4fyr  = ELfyr
     C                   eval      E4emp# = ELemp#
     C                   eval      E4div  = ELdiv
     C                   eval      E4rdiv = ELdiv
     C                   eval      E4arcd = xxCode
     C                   eval      E4desc = TTdesc
     C                   eval      E4ctyp = TTctyp
     C                   eval      E4podt = sysdat
     C                   eval      E4amt  = xxbal
     C                   eval      E4sycd = xxsycd
     C                   eval      E4tfap = 'N'
     C                   eval      E4tffl = 'N'
     C                   eval      E4stat = *blank
     C                   eval      E4user = Q1user
     C                   eval      E4pgm  = Q1pgm
     C                   eval      E4time = TimeTo6(SystemTime)
     C                   eval      E4dt   = sysdat
     C                   eval      E4idt  = sysdat
     C                   eval      E4iusr = Q1user
     C                   eval      E4ipgm = Q1pgm
     C                   write     pea04
     C                   endsr
     *================================================================
     * SRARB - Calculate A/R Balance for policy
     *================================================================
     C     srarb         begsr
     c                   eval      xxpbal = *zero
     c                   eval      xxdbal = *zero
     c                   eval      xxfbal = *zero
      /free
       //********** Beg Add ********** 01/20/15 **********************
       xx1Bal = *zero;
       xx2Bal = *zero;
       xx3Bal = *zero;
       xx4Bal = *zero;
       xx5Bal = *zero;
       xx6Bal = *zero;
       xx7Bal = *zero;
       //********** End Add ********** 01/20/15 **********************
      /End-Free
     c     keyWMEAL6     setll     pea06
     c     keyWMEAL6     reade     pea06
     c                   dow       not(%eof)
     c                   eval      kkkey = EAarcd
     c     keyTable      chain     ptt01
     c                   if        not(%found)
     c     keyWMEAL6     reade     pea06
     c                   iter
     c                   endif
     c                   select
     c                   when      ttctyp = 'P'
     c                   if        eaarty = 'B' or
     c                             eaarty = 'A' or
     c                             eaarty = 'D' or
     c                             eaarty = 'V' or
     c                             eaarty = 'R' or
     c                             eaarty = 'N'
     c                   eval      xxpbal = xxpbal + eaamt
     c                   else
     c                   eval      xxpbal = xxpbal - eaamt
     c                   endif
     c                   when      eaarty = 'F'
       //****** Beg Del ******* 01/20/15 *****************************
     c******             eval      xxfbal = xxfbal + eaamt
       //****** End Del ******* 01/20/15 *****************************
      /Free
       //****** Beg Add ******* 01/20/15 *****************************
        If TTctyp <> '1' and TTctyp <> '2'and TTctyp <> '3' and TTctyp <> '4'
         and TTctyp <> '5' and TTctyp <> '6'and TTctyp <> '7';
           xxfBal = xxfBal + EAamt;
        Else;
           Exsr AddBal;
        Endif;
       //****** End Add ******* 01/20/15 *****************************
      /End-Free
     c                   when      eaarty = 'Q'
       //****** Beg Del ******* 01/20/15 *****************************
     c******             eval      xxfbal = xxfbal - eaamt
       //****** End Del ******* 01/20/15 *****************************
      /Free
       //****** Beg Add ******* 01/20/15 *****************************
        If ttctyp <> '1' and ttctyp <> '2'and ttctyp <> '3' and ttctyp <> '4'
         and ttctyp <> '5' and ttctyp <> '6'and ttctyp <> '7';
           xxfbal = xxfbal - eaamt;
        Else;
           Exsr SubBal;
        Endif;
       //****** End Add ******* 01/20/15 *****************************
      /End-Free
     c                   when      ttctyp = 'D'
      *** beg chg ***** 01/20/15 **********************************
     c***********        if        eaarty = 'H'
     c***********        eval      xxdbal = xxdbal + eaamt
     c***********        else
     c***********        eval      xxdbal = xxdbal - eaamt
     c***********        endif
      /free
       If eaarty = 'H' or eaarty = 'R';
          xxdbal = xxdbal + eaamt;
       Endif;
       If eaarty = 'I' or eaarty = 'T';
          xxdbal = xxdbal - eaamt;
       Endif;
      /End-free
      *** end chg ***** 01/20/15 **********************************
     c                   endsl
     c     keyWMEAL6     reade     pea06                                  60
     c                   enddo
      *** beg del ***** 01/20/15 **********************************
      *** beg add ***** 08/22/14 **********************************
      /free
       //****** xxtbal = xxpbal + xxfbal + xxdbal;
      /end-free
      *** end add ***** 08/22/14 **********************************
      *** end del ***** 01/20/15 **********************************
      *** beg add ***** 01/20/15 **********************************
      /free
       xxtBal = xxpBal + xxfBal + xx1Bal + xx2Bal + xx3Bal + xx4Bal + xx5Bal
        + xx6Bal + xx7Bal + xxdBal;
      /end-free
      *** end add ***** 01/20/15 **********************************
     c                   endsr
     *================================================================
     * PGMSTR - Set-up and Initialize
     *================================================================
     C     pgmstr        begsr
     C                   eval      sysdat = DateTo7(SystemDate)
     C                   move      @@co#         xxco#
     C                   move      @@date        x1date
     C                   eval      xxdate = dtCYMD(x1date)
     C                   eval      xxbata = *zero
     *
     *  Get next A/R Batch number
     *
     C     keyWDEAL4     setgt     pea04
     C     keyWDEAL4     readpe(n) pea04
     C                   if        %eof
     C                   eval      xxnxt = 1
     C                   else
     C                   eval      xxnxt = e4bat# + 1
     C                   endif
     c                   clear                   pea04
     c                   eval      E4co#  = xxCo#
     c                   eval      E4bat# = xxNxt
     c                   write     pea04                                        Create placeholder
     *
     *  Get System A/R Batch Code to be used for tranfers
     *
     C                   clear                   xxsycd
     C     syky          chain     psy01                              60
     C                   dow       *in60 = *off
     C                             and xxsycd = *blanks
     C                   if        systat = 'A'
     C                             and sytype = 'Y'
     C                   movel(p)  sykey         xxsycd
     C                   endif
     C     syky          reade     psy01                                  60
     C                   enddo
     C                   endsr
      /Free
       // ****** Beg Add ****** 01/20/15 ***************************************
       //=======================================================================
       // Processing balance When 'F' fee payment.
       //=======================================================================
       Begsr AddBal;
       Select;
         When TTctyp = '1';
            xx1Bal = xx1Bal + EAamt;

         When TTctyp = '2';
            xx2Bal = xx2Bal + EAamt;

         When TTctyp = '3';
            xx3Bal = xx3Bal + EAamt;

         When TTctyp = '4';
            xx4Bal = xx4Bal + EAamt;

         When TTctyp = '5';
            xx5bal = xx5bal + EAamt;

         When TTctyp = '6';
            xx6bal = xx6bal + EAamt;

         When TTctyp = '7';
            xx7Bal = xx7Bal + EAamt;
       Endsl;
       Endsr;

       //=======================================================================
       // Processing balance When 'Q' fee payment.
       //=======================================================================
       Begsr SubBal;
       Select;
         When TTctyp = '1';
            xx1Bal = xx1Bal - EAamt;

         When TTctyp = '2';
            xx2Bal = xx2Bal - EAamt;

         When TTctyp = '3';
            xx3Bal = xx3Bal - EAamt;

         When TTctyp = '4';
            xx4Bal = xx4Bal - EAamt;

         When TTctyp = '5';
            xx5Bal = xx5Bal - EAamt;

         When ttctyp = '6';
            xx6Bal = xx6Bal - EAamt;

         When ttctyp = '7';
            xx7Bal = xx7Bal - EAamt;
       Endsl;
       Endsr;
        // ****** End Add ****** 01/20/15 ******************************************
