     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
      ******************************************************************
      *   CREATED BY                                                   *
      *                                                                *
      *   04/04/16  C:212 079  Project: OK022                          *
      *                        OK 2012 FS Update                       *
      *             This program will create a DRG line item. It is    *
      *             modelled on FA756R                                 *
      ******************************************************************
      *  REVISIONS:                                                    *
      *                                                                *
      *  07/01/16 C: 079 Project: OK022                                *
      *                  Pay lessor of DRG allow or billed charges     *
      *                                                                *
      ******************************************************************
     Ffmsvl1OK  if   e           k disk
     Ffaeop     if   e             disk
     Ffdcal     if   e           k disk
     Ffddul3    Uf A e           k disk
     Ffaeul     if   e           k disk
     FFTRTL1    if   e           k disk
     Fftgtl     if   e           k disk
     Ffdcalf    IF A E           K DISK
     F                                     RENAME(PCA01:PCA0F)
     Ffdcal19   IF   E           K DISK
     F                                     RENAME(PCA01:PCA19)
     Ffdcal24   UF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     D psds          esds                  extname(szq1p)

     D                 ds
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 ds
     D kkcmsv                        11
     D  xxsvcd                        3  0 overlay(kkcmsv)
     D  xxcpt4                        5    overlay(kkcmsv:4)
     D  xxunit                        3  0 overlay(kkcmsv:9)
     D*
     Dxxcmsbr          s              9  2 inz
     Dxxout            s              9  2 inz
     Dcaddon           s              9  2 inz(0.15)
     Dxxmtsh           s              9  2 inz
     Dxxtotout         s              9  2 inz
     Dxxdrout          s              9  2 inz
     Dxxalow           s              9  2 inz
     Dsmchrg           s             11  2 inz
     Dxxaudchrg        s             11  2 inz
     D@@car            s             10    inz
     Dxxcode           s             11    inz
     D@@chrg           s                   like(cachrg) inz
     DxxASCmul         s                   like(RTmul1) inz
     DxxInpmul1        s                   like(RTmul1) inz
     DxxInpmul2        s                   like(RTmul1) inz
     DxxInpmul3        s                   like(RTmul1) inz
     Dxxpdiem          s                   like(RTpdiem) inz
     Dkktbid           s                   like(gttbid) inz
     Dkkkey            s                   like(gtkey) inz
     DindCatDrg        s               n   inz(*off)
     DindWrtLine       s               n   inz(*on)

     C     *LIKE         DEFINE    CALIN#        X1LIN#
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
     C     *LIKE         DEFINE    CALIN#        KKLIN#
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    SVrelW        xxRelW
     C     *like         define    durvcd        kkrvcd
     C     *like         define    durate        kkrate
     C     *like         define    eoco#         kkco#
     C     *like         define    eosub#        kksub#
     C     *like         define    eofnd         kkfnd
     C     *like         define    eofyr         kkfyr
     C     *like         define    eocase        kkcase
     C     *like         define    eoeob#        kkeob#
     C     *like         define    EOAST         KKST
     C     *like         define    EUDCDT        KKEFDT

     C     *entry        plist
     C                   parm                    @@rrn             9 0
     C                   parm                    @@mode            1
     C                   parm                    @@rtcd            1
     C                   parm                    @@allow           9 2

     C     dtcymd1       plist
     C                   parm                    srdti1            8 0
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr

     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1

     C     svky          klist
     C                   kfld                    kkCo#
     C                   kfld                    kkcmSv
     C                   kfld                    kkEfdt

     C     svky1         klist
     C                   kfld                    kkCo#
     C                   kfld                    kkcmSv

     C     duky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrvcd
     C                   kfld                    kkrate

     C     duky#         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrvcd

     C     CAKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#

     C     CAKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKDSEQ

     C     CAKY3         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE

     C     CAKY4         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKLIN#


     C     CAKY#         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C     RTKY          KLIST
     C                   KFLD                    KKST
     C                   KFLD                    KKEFDT
     C     RTKY1         KLIST
     C                   KFLD                    KKST

      *** Set the pointer for the FAEOP from the RRN

      /FREE
       @@rtcd = *blanks;
       @@allow = *zeros;
       indwrtLine = *on;

       Chain @@rrn FAEOP;
       *IN99 = NOT %FOUND;
       if *in99 = *on;
         @@rtcd = 'F';
         *INLR = *ON;
         return;
       endif;                                                                 //*in99=*on

      /END-FREE
     C                   Z-add     eoco#         kkco#
     C                   Z-add     eosub#        kksub#
     C                   Z-add     eofnd         kkfnd
     C                   Z-add     eocase        kkcase
     C                   Z-add     eoeob#        kkeob#
     C                   Z-add     eofyr         kkfyr
     C                   MOVE      EOAST         KKST
      /FREE

       // Remove the rev code 100 line item if it exists

       exsr remove_rev_100;

       // Calculate total charged

       exsr calc_tot_chrg;

       // Set the pointer for the FAEUL from the values of FAEOP

       Chain caky1 faeul;
       *IN99 = NOT %FOUND;
       KKEFDT = EUDCDT;

       // Set CMS base rate and Outlier threshold according to discharge date
       SETGT RTKY PRT01;
       READPE(N) RTKY1 PRT01;
       DOW NOT %EOF;
         IF RTTYPE = 'I';
           xxcmsbr = RTOPBR;
           xxout = RTOUTL;
           xxpdiem = RTPDIEM;
           xxInpmul1 = RTmul1;
           xxInpmul2 = RTmul2;
           xxInpmul3 = RTmul3;
           LEAVE;
         ELSE;
           READPE(N) RTKY1 PRT01;
         ENDIF;
       ENDDO;

       // Set ASC percentage mutliplier
       SETGT RTKY PRT01;
       READPE(N) RTKY1 PRT01;
       DOW NOT %EOF;
         IF RTTYPE = 'A';
           xxASCmul = RTmul1;
           LEAVE;
         ELSE;
           READPE(N) RTKY1 PRT01;
         ENDIF;
       ENDDO;

       // Retrieve the Relative value from the OK DRG Service Code file

       xxSvcd = EUdrg#;
       xxCpt4 = *blanks;
       xxUnit = 0;
       setgt svky psv01;
       readpe(n) svky1 psv01;
       *in99 = %eof;

       if *in99 = *off;
         xxrelw = svrelw;
       Endif;

         @@chrg = 0;
         EXSR cal_implant;

       // audited charges = smchrg - @@chrg;
       xxaudchrg = smchrg - @@chrg;
       // ASC payment
       If @@Mode = 'A';

         // Pay lesser of 60% of audited charges or the DRG payment
         // 60% = coming from FTRTP in xxASCmul

        If EUdrg# <> 0;
         exsr inpatient;
        Endif;

         if xxdrout < (xxASCmul * xxaudchrg) and EUdrg# <> 0;
            xxalow = xxdrout;
            indwrtLine = *on;
            @@rtcd = ' ';
         else;
            @@rtcd = '2';
            indwrtLine = *off;
         endif;

         xxdrout = xxalow;

       EndIf;

       // Inpatient payment

       If @@Mode = 'I';
         exsr inpatient;
       EndIf;

       if @@rtcd = ' ';
         if indwrtLine = *on;
           Exsr lodrcd;
         endif;                                                                 //@@rtcd = ' '
         @@allow = xxdrout;
       endif;                                                                 //@@rtcd = ' '

       *INLR = *ON;
       return;

      //*******************************************************************
      //
      //  Calculate total charged for this bill
      //
      //****************************************************************
       begsr calc_tot_chrg;

         smchrg = 0;

         chain(N) caky1 PCA19;
         *IN99 = NOT %FOUND;

         dow *in99 = *off;

           smchrg = cachrg + smchrg;

           reade(N) caky1 pca19;
           *IN99 = %EOF;

         enddo;

       endsr;

      //*******************************************************************
      //
      //  Calculate Inpatient Allowance for this Bill
      //
      //****************************************************************
       begsr Inpatient;

         // If audited charges < 70,000 (coming from FTRTP/RToutl in xxout)
         If xxaudchrg < xxout;
            xxalow = xxrelw * xxpdiem;

         Else;
           // If audited charges > 70,000

           // Verify if Catastrophic Drug
           kktbid = 'OKCD';
           kkkey = %char(EUdrg#);
           Setll (kkco#:kktbid:kkkey) pgt01;
           If %equal(ftgtl);
              indCatDrg = *on;
           Else;
              indCatDrg = *off;
           Endif;

           If indCatDrg = *on;
             // 70% of audited charges
             xxalow = xxInpmul1 * xxaudchrg;
           Else;
             // 65% of ((Tot. chrg. - Implant chrg) - 50% of (Rel.Wt * PDiem))
             // compute the paranthesis
             xxalow = ((smchrg - @@chrg)-(xxInpmul2 * xxrelw * xxpdiem));
             // get 65% of the value
             xxalow = xxInpmul3 * xxalow;
           Endif;

         Endif;

         xxdrout = xxalow;

       endsr;

      //*******************************************************************
       // Calculate implant charges

       // 8)  XXCHRG = SMCHRG - any implants that were billed
       BEGSR cal_implant;

         kkrvcd = 278;
         setll duky# pdu01;
         reade duky# pdu01;
         *IN60 = %EOF;
         Dow *in60 = *off;
           kkdseq = dudseq;
           chain caky2 PCA0F;
           *IN99 = NOT %FOUND;
           if *in99 = *off;
             @@chrg = @@chrg +Cachrg;
           endif;
           reade duky# pdu01;
           *IN60 = %EOF;
         enddo;
       Endsr;

      //*******************************************************************
      //  Start loading the records in FDDUP & FDCAP
      //
      //****************************************************************
       begsr lodrcd;

         kkrate = '   ';
         kkrvcd = 100;
         chain duky1 pdu01;
         *IN60 = NOT %FOUND;
         if *in60 = *off;

           kkdseq = dudseq;
           chain caky# pca02;
           *IN60 = NOT %FOUND;
           if *in60 = *off;
             if cacscd  <> 'DRG';
               exsr updpd;
             endif;                                                           //cacscd  <> 'DRG'
           endif;

         else;

           exsr wrtpd;

         endif;
      /END-FREE
     C     lodrcde       endsr

     //*******************************************************************
     //  Rrmove the revenue code 100 line item
     //
     //****************************************************************
      /Free
       Begsr remove_rev_100;

         kkrate = '   ';
         kkrvcd = 100;
         chain duky1 pdu01;
         *in99 = Not %Found;
         if *in99 = *off;
           kkdseq = dudseq;
           chain caky# pca02;
           *in99 = Not %Found;
           if *in99 = *off;
             if cacscd  <> 'DRG';
               Delete pca02;
               Delete pdu01;
             endif;                                                           //cacscd  <> 'DRG'
           endif;
         endif;
       Endsr;

      /End-Free
     //*******************************************************************
     //  Write the records in FDCAP and FDDUP
     //
     //****************************************************************
      /FREE
       begsr wrtpd;

         // Get the next available seq# & line#

      /END-FREE
     C                   TIME                    DSDTTM
     C                   MOVE      *HIVAL        KKDSEQ

      /FREE
         SETGT caky2 PCA0F;
         READPE(N) caky3 PCA0F;
         *IN99 = %EOF;

         IF *IN99 = *OFF;
           xxdseq = CADSEQ + 1;
         ELSE;
           xxdseq = 1;
         ENDIF;

         kklin# = *HIVAL;
         SETGT CAKY4 PCA19;
         READPE(N) CAKY1 PCA19;
         *IN99 = %EOF;

         IF *IN99 = *OFF;
           x1lin# = CALIN# + 1;
         ELSE;
           x1lin# = 1;
         ENDIF;

      /END-FREE
     C                   Z-ADD     EOCO#         CACO#
     C                   Z-ADD     EOFND         CAFND
     C                   Z-ADD     EOFYR         CAFYR
     C                   Z-ADD     EOCASE        CACASE
     C                   MOVEL     '2  '         CACSCD
     C                   Z-ADD     EOFRDT        CAFDAT
     C                   Z-ADD     EOTODT        CATDAT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CARDAT
     C                   Z-ADD     EOFEIN        CAFEIN
     C                   Z-ADD     EOMOD         CAMOD
     C                   MOVEL     'DRG  '       CACPT4
     C                   MOVEL     *BLANKS       CAFLAG
     C                   Z-ADD     EOEOB#        CAEOB#
     C                   MOVEL     EOSS#         CASS#
     C                   Z-ADD     EOACDT        CAACDT
     C                   MOVEL     'A'           CASTAT
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   Z-ADD     DSTIME        CATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   MOVEL     *BLANKS       CADXR
     C                   MOVEL     'D'           CARCFL
     C                   MOVEL     EOSUB#        CASUB#
     C                   Z-ADD     *ZERO         CAOTHR
     C                   Z-ADD     *ZERO         CAOTH2
     C                   Z-ADD     *ZERO         CANDIS
     C*** Beg Chg *** 07/01/16 *********************************************
     C****************   Z-ADD     xxdrout       CAPAID
     C                   if        xxdrout < smChrg
     C                   eval      CApaid = xxDrOut
     C                   else
     C                   eval      CApaid = smChrg
     C                   endif
     C*** End Chg *** 07/01/16 *********************************************
     C                   Z-ADD     *ZEROS        CACHRG
     C     CACHRG        SUB       CAPAID        CAMRA
     C                   MOVEL     EORCN         CARCN
     C                   MOVEL     *BLANKS       CARULE
     C                   MOVEL     *BLANKS       CAOT1C
     C                   MOVEL     *BLANKS       CAOT2C
     C                   Z-ADD     *ZEROS        CAMNAA
     C                   MOVEL     *BLANKS       CAADJ
     C                   MOVEL     *BLANKS       CABPRC
     C                   Z-ADD     X1LIN#        CALIN#
     C                   Z-ADD     XXDSEQ        CADSEQ
     C                   MOVEL     Q1USER        CAIUSR
     C                   MOVEL     Q1PGM         CAIPGM
     C                   Z-ADD     DSTIME        CAITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CAIDT
     C                   Z-ADD     *ZERO         CADIFF


     C                   MOVE      'X'           CADELF
      /FREE
         xxcode = '100' + '   ';
      /END-FREE
     C                   Movel     xxcode        cacode
     C                   Z-ADD     *ZERO         CABOPR
     C                   Z-ADD     *ZERO         CABCAP
     C                   Z-ADD     *ZERO         CAOIME
     C                   Z-ADD     *ZERO         CACIME
     C                   Z-ADD     *ZERO         CAODSH
     C                   Z-ADD     *ZERO         CACDSH
     C                   Z-ADD     xxtotout      CAOUTL
     C                   Z-ADD     *ZERO         CATRAN
      //
      /FREE
         WRITE PCA0F;
         //
         // WRITE RECORD IN FDDUP
         //
      /END-FREE
     C                   Z-ADD     EOCO#         DUCO#
     C                   Z-ADD     EOSUB#        DUSUB#
     C                   Z-ADD     EOFND         DUFND
     C                   Z-ADD     EOFYR         DUFYR
     C                   Z-ADD     EOCASE        DUCASE
     C                   MOVEL     CASS#         DUSS#
     C                   Z-ADD     EOACDT        DUACDT
     C                   Z-ADD     CADSEQ        DUDSEQ
     C                   Z-ADD     EOEOB#        DUEOB#
     C                   MOVE      '21'          DUPOS
     C                   Z-ADD     100           DURVCD
     C                   MOVEL     '     '       DURATE
     C                   MOVEL     EUDGDS        DUDESC
     C                   Z-ADD     1             DUUNTS
     C                   MOVEL     'A'           DUSTAT
     C                   MOVEL     Q1USER        DUUSER
     C                   MOVEL     Q1USER        DUIUSR
     C                   MOVEL     Q1PGM         DUPGM
     C                   MOVEL     Q1PGM         DUIPGM
     C                   Z-ADD     DSTIME        DUTIME
     C                   Z-ADD     DSTIME        DUITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DUDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DUIDT
     C                   MOVEL     *BLANKS       DUBRAT
     C                   Movel     *ZEROS        DUBPRC
     C                   MOVEL     *BLANKS       DUBPRD
      //
      /FREE
         WRITE PDU01;

      /END-FREE
     C     wrtpde        endsr

     //*******************************************************************
     //  Update the records in FDCAP and FDDUP
     //*******************************************************************
      /FREE
       begsr updpd;
      /END-FREE
     C                   TIME                    DSDTTM
     //
     //Update the FDCAP
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   MOVEL     *BLANKS       CADXR
     C                   MOVEL     'D'           CARCFL
     C                   MOVEL     EOSUB#        CASUB#
     C                   Z-ADD     *ZERO         CAOTHR
     C                   Z-ADD     *ZERO         CAOTH2
     C                   Z-ADD     *ZERO         CANDIS
     C*** Beg Chg *** 07/01/16 *********************************************
     C****************** Z-ADD     xxdrout       CAPAID
     C                   if        xxdrout < smChrg
     C                   eval      CApaid = xxDrOut
     C                   else
     C                   eval      CApaid = smChrg
     C                   endif
     C*** End Chg *** 07/01/16 *********************************************
     C                   Z-ADD     *ZEROS        CACHRG
     C     CACHRG        SUB       CAPAID        CAMRA
     C                   Z-ADD     xxdrout       CAMRAA
     C                   Z-ADD     *ZEROS        CAMNAA
     C                   Z-ADD     *Zeros        CABOPR
     C                   Z-ADD     *Zeros        CABCAP
     C                   Z-ADD     *Zeros        CAOIME
     C                   Z-ADD     *Zeros        CACIME
     C                   Z-ADD     *Zeros        CAODSH
     C                   Z-ADD     *Zeros        CACDSH
     C                   Z-ADD     xxtotout      CAOUTL
     C                   Z-ADD     *Zeros        CATRAN
      /FREE
         Update PCA02;
        //
      /END-FREE
     C                   MOVEL     'A'           DUSTAT
     C                   MOVEL     Q1USER        DUUSER
     C                   MOVEL     Q1USER        DUIUSR
     C                   MOVEL     Q1PGM         DUPGM
     C                   MOVEL     Q1PGM         DUIPGM
     C                   Z-ADD     DSTIME        DUTIME
     C                   Z-ADD     DSTIME        DUITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DUDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DUIDT
      //
      /FREE
         update PDU01;
      /END-FREE
     C     updpde        endsr
