     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('STBNDDIR')
     H*****************************************************************
     H*  CREATED BY                                                   *
     H*                                                               *
     H*  04/05/18  C:180 Project: PA085 (DRG Update)                  *
     H*                                                               *
     H*            This program will create a DRG line item. It is    *
     H*            modelled on FA756RDE                               *
     H*                                                               *
     H*  ***NOTE*** Any changes to Drg Payment Calc. should be updated*
     H*             to F0366R.                                        *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/09/18 C: 079 Support: 114 70740                            *
     H*                                                               *
     H* 04/12/18 C: 079 Project: PA085                                *
     H*****************************************************************
     Ffaeop     if   e             disk
     Ffdcal     if   e           k disk
     Ffaeul     if   e           k disk
     Ffapolpa   if   e           k DISK
     Ffdcal13   IF   E           K DISK
     F                                     RENAME(PCA01:PCA13)  Prefix(L1)
     Ffdcal19   IF   E           K DISK
     F                                     RENAME(PCA01:PCA19)
     Ffdcalf    IF A E           K DISK
     F                                     RENAME(PCA01:PCA0F)
     Ffddul3    Uf A e           k disk
     Ffdcal24   UF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     Fftfelpa   if   e           k DISK
     f**** Beg Add *** 05/09/18 **********************************
     Ffddul     if   e           k disk    rename(pdu01:pdu02)
     f**** End Add *** 05/09/18 **********************************
     D psds          esds                  extname(szq1p)
     *
     D                 ds
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     Dxxtotout         s              9  2
     Dxxdrout          s              9  2
     D@@car            s             10
     Dxxcode           s             11
     D@@drg#           s              5P 0 inz
     Dxxchrg           s                   like(cachrg)
     D@@chrg           s                   like(cachrg)
     D sum1            s             14  5
     D sum1a           s             14  5
     D sum1b           s             14  5
     D sum2            s             14  5
     D sum3            s             14  5
     D sum3a           s             14  5
     D sum4            s             14  5
     D sum5            s             14  5
     D sum6            s             14  5
     D sum6a           s             14  5
     D sum6b           s             14  5
     D sum7            s             14  5
     D sum8            s             14  5
     D sum9            s             14  5
     D sum10           s             14  5
     D sum11           s             14  5
     D sum12           s             14  5
     D sum12a          s             14  5
     D sum12b          s             14  5
     D sum13           s             14  5
     D sum14           s             14  5
     D sum15           s             14  5
     D sum16           s             14  5
     D sum17           s             14  5
     D sum18           s             14  5
     d*** Beg Add *** 05/09/18 *************************************
     D SumUnits        s                   like(DUunts)
     d*** End Add *** 05/09/18 *************************************
     D/COPY sPrString

     C     *LIKE         DEFINE    CALIN#        X1LIN#
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
     C     *LIKE         DEFINE    CALIN#        KKLIN#
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
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
      *
     C     *entry        plist
     C                   parm                    @@rrn             9 0
     C                   parm                    @@rtcd            1
     C                   parm                    @@allow           9 2
     *
     C     dtcymd1       plist
     C                   parm                    srdti1            8 0
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     *
     C     ifky          klist
     C                   kfld                    @@car
     C                   kfld                    kkefdt
     *
     C     ifky1         klist
     C                   kfld                    @@car
     *
     *
     C     duky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrvcd
     C                   kfld                    kkrate
     *
     C     duky#         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrvcd
     *
     C     CAKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     CAKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKDSEQ
     C*
     C     CAKY3         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     CAKY4         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKLIN#
     C*
     C*
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
     C*
     C*Set the pointer for the FAEOP from the RRN
     C*
     C                   Move      ' '           @@rtcd
     C                   z-add     0             @@allow
     C     @@rrn         Chain     FAEOP                              99
     C                   if        *in99 = *on
     C                   Move      'F'           @@rtcd
     C                   seton                                        LR
     C                   return
     C                   endif                                                  *in99=*on
     C*
     C                   Z-add     eoco#         kkco#
     C                   Z-add     eosub#        kksub#
     C                   Z-add     eofnd         kkfnd
     C                   Z-add     eocase        kkcase
     C                   Z-add     eoeob#        kkeob#
     C                   Z-add     eofyr         kkfyr
     C                   MOVE      EOAST         KKST
     C*
     C*Set the pointer for the FAEUL from the values of FAEOP
     C*
     C     caky1         Chain     faeul                              99
     C                   EVAL      KKEFDT = EUDCDT
     C**************************************************************************
     C* MAIN CODE STARTS HERE
     C**************************************************************************
      /free

       // Calculating CMS base rate and Outlier threshold according to discharge date

       @@car   = EUMCAR;
       @@drg#  = eudrg#;
       xxdrout = *zeros;
       xxChrg =  *zeros;
       //*** Beg Add *** 05/09/18 *****************************************
       SumUnits = 0;
       //*** End Add *** 05/09/18 *****************************************

       //Get Charged Amount
       setll caky1 FDCAL13;
       reade caky1 FDCAL13;

       Dow not %eof(FDCAL13);
          If L1carcn = EOrcn;
             xxChrg = xxChrg + L1cachrg;
          Endif;
       //*** Beg Add *** 05/09/18 *******************************
       // Get number of days for room and board codes
          Chain (L1caco#:L1casub#:L1cafnd:L1cafyr:L1cacase:L1caeob#:
                 L1cadseq) pdu02;
          If %Found(fddul) and DUrvcd >= 110 and DUrvcd <= 169
             and DUrvcd <> 100;
             SumUnits = SumUnits + DUunts;
          Endif;
       //*** End Add *** 05/09/18 *******************************
          reade caky1 FDCAL13;
       Enddo;

       //Get Federal Register Amount
       chain @@drg# FTFELPA;
       If not %found(FTFELPA);
          //CAfema = 'N';
          @@rtcd = 'F';
       Endif;

       //Get Propspective Payment Amount
       setgt ifky ppo01;
       readpe ifky1 ppo01;

       If not %eof(FAPOLPA) and @@rtcd = *blanks;
          sum1 = ferlwt * podrrt;
          sum2 = ferlwt * pocprt;
          sum3 = pohhrt;
          sum3a= (podrrt+pocprt)*ferlwt;
          sum4 = sum3a * poinad;
          sum5 = sum3a * podiad;
          sum6a = sum1 * 1.13;
          sum6 = sum6a * podsad;
          sum7 = sum3a * popmad;
          sum8 = pocprt;
          sum9 = 0;
          sum10 = sum3+sum4+sum5+sum6+sum6a+sum7+sum2;

        // Calculate outlier payment

        // Cost outlier
           sum11 = xxChrg * poarcc;

        // Threshold 1 (T1)
           sum12a = ferlwt * podrrt;
           sum12b = ferlwt * pocprt;
           sum12 = (sum12a + sum12b) * 2;

        // Threshold 2 (T2) is 36,000 set by Medicare. Select Greater of T1,T2
           if sum12 > 36000;
             sum13 = sum12;
           else;
             sum13 = 36000;
           endif;

           sum14 = sum11 - sum13;
           sum15 = sum14 * .80;

           // Sum15 is total Cost outlier
           if sum15 <= 0;
             sum15 = 0;
           endif;

           // Day outlier
       //*** Beg Chg *** 05/09/18 *******************************
       //**if CAunts > feoutl;
           if SumUnits > feoutl;
       //*** End Chg *** 05/09/18 *******************************
             sum16 = sum12a + sum12b;
             if fealos > 0;
               sum17 = sum16 / fealos;
             endif;
       //*** Beg Chg *** 05/09/18 *******************************
       //*** sum18 = (CAunts-feoutl) * sum17;
             sum18 = (SumUnits-feoutl) * sum17;
       //*** End Chg *** 05/09/18 *******************************
           endif;

           // Sum18 is day outlier amount
       //*** Beg Chg *** 05/09/18 *******************************
       //**if CAunts <= feoutl;
           if SumUnits <= feoutl;
       //*** End Chg *** 05/09/18 *******************************
             sum18 = 0;
           endif;

           // Calculate the final payment
           if sum15 = 0 or sum18 = 0;
             xxdrout = sum10 + sum15 + sum18;
           endif;

           if sum15 > 0 and sum18 > 0;
             if sum15 <= sum18;
               xxdrout = sum10 + sum15;
             else;
               xxdrout = sum10 + sum18;
             endif;
           endif;
           @@Allow = xxdrout;
       Else;
          // CApoma ='N';
          @@rtcd = 'F';
       Endif;

        if @@rtcd = ' ';
           Exsr lodrcd;
        endif;

        *inlr = *on;
        return;


       // *****************************************************************
       // Subroutine - lodrcd (Start loading the records in FDDUP & FDCAP)
       // *****************************************************************
        begsr lodrcd;
           kkrate = *blanks;
           kkrvcd = 100;

           chain duky1 pdu01;
              if %found(fddul3);
                 kkdseq = dudseq;
                    chain caky# pca02;
                       if %found(fdcal24);
                          if cacscd  <> 'DRG';
                             exsr updpd;
                          endif;
                       endif;
              else;
                 exsr wrtpd;
              endif;
        endsr;
      /end-free

    C********************************************************************
    C*  Write the records in FDCAP and FDDUP
    C*
    C*****************************************************************
     C     wrtpd         begsr
     C*
     C* Get the next available seq# & line#
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      *HIVAL        KKDSEQ
     C*
     C     caky2         SETGT     PCA0F
     C     caky3         READPE    PCA0F                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C     CADSEQ        ADD       1             XXDSEQ
     C                   ELSE
     C                   Z-ADD     1             XXDSEQ
     C                   ENDIF
     C*
     C                   MOVE      *HIVAL        KKLIN#

     C     CAKY4         SETGT     PCA19
     C     CAKY1         READPE    PCA19                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C     CALIN#        ADD       1             X1LIN#
     C                   ELSE
     C                   Z-ADD     1             X1LIN#
     C                   ENDIF
     C*
     C                   Z-ADD     EOCO#         CACO#
     C                   Z-ADD     EOFND         CAFND
     C                   Z-ADD     EOFYR         CAFYR
     C                   Z-ADD     EOCASE        CACASE
     C*** Beg Chg *** 04/12/18 *************************************
     C***************    MOVEL     '2  '         CACSCD
     C                   MOVEL     '1  '         CACSCD
     C*** End Chg *** 04/12/18 *************************************
     C                   Z-ADD     EOFRDT        CAFDAT
     C                   Z-ADD     EOTODT        CATDAT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CARDAT
     C                   Z-ADD     EOFEIN        CAFEIN
     C                   Z-ADD     EOMOD         CAMOD
     C                   MOVEL     *blanks       CACPT4
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
     C                   Z-ADD     xxdrout       CAPAID
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
     C*
     C*
     C                   MOVE      'X'           CADELF
     C                   eval      xxcode = '100' + '   '
     C                   Movel     xxcode        cacode
     C                   Z-ADD     *ZERO         CABOPR
     C                   Z-ADD     *ZERO         CABCAP
     C                   Z-ADD     *ZERO         CAOIME
     C                   Z-ADD     *ZERO         CACIME
     C                   Z-ADD     *ZERO         CAODSH
     C                   Z-ADD     *ZERO         CACDSH
     C                   Z-ADD     *Zero         CAOUTL
     C                   Z-ADD     *ZERO         CATRAN
       //*** Beg Add *** 05/09/18 *******************************
      /Free
                         CAunts = SumUnits;
      /End-free
       //*** End Add *** 05/09/18 *******************************
     C*
     C                   WRITE     PCA0F
     C*
     C* WRITE RECORD IN FDDUP
     C*
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
     C                   MOVEL     *blanks       DURATE
     C                   MOVEL     EUDGDS        DUDESC
       //*** Beg Chg *** 05/09/18 *******************************
     C*************      Z-ADD     1             DUUNTS
      /Free
                         DUunts = SumUnits;
      /End-free
       //*** End Add *** 05/09/18 *******************************
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
     C*
     C                   WRITE     PDU01
     C*
     C     wrtpde        endsr
    C********************************************************************
    C*  Update the records in FDCAP and FDDUP
    C********************************************************************
     C     updpd         begsr
     C                   TIME                    DSDTTM
    C*
    C*Update the FDCAP
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   MOVEL     *BLANKS       CADXR
     C                   MOVEL     'D'           CARCFL
     C                   MOVEL     EOSUB#        CASUB#
     C                   Z-ADD     *ZERO         CAOTHR
     C                   Z-ADD     *ZERO         CAOTH2
     C                   Z-ADD     *ZERO         CANDIS
     C                   Z-ADD     xxdrout       CAPAID
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
     C                   Z-ADD     *zero         CAOUTL
     C                   Z-ADD     *Zeros        CATRAN
       //*** Beg Add *** 05/09/18 **************************************
      /Free
                         CAunts = SumUnits;
                         CAfdat = EOfrDt;
                         CAtdat = EOtoDt;
      /End-free
       //*** End Add *** 05/09/18 **************************************
     C                   Update    PCA02
    C*
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
       //*** Beg Add *** 05/09/18 **************************************
      /Free
                         DUunts = SumUnits;
      /End-free
       //*** End Add *** 05/09/18 **************************************
     C*
     C                   update    PDU01
     C     updpde        endsr
