     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H*  CREATED BY                                                   *
     H*                                                               *
     H*  11/28/08  C:191 Project: MS017                               *
     H*                           DRG Update                          *
     H*            This program will create a DRG line item. It is    *
     H*            modelled on FA756R                                 *
     H*  ***NOTE*** Any changes to Drg Payment Calc. should be updated*
     H*             to F0366R.                                        *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  10/29/20 C:295 Project: MS039                                *
     H*                 MS 10/1/2020 Inpatient and Outlier Update     *
     H*                                                               *
     H*  12/21/18 C:274 Project: MS035 (Retrofit FS547)               *
     H*                Load service date from admission date          *
     H*                                                               *
     H*  05/11/11  C:079 Support: 76850  MC133                        *
     H*                           Prevent dividing by zero error      *
     H*                           XRef - 76129                        *
     H*                                                               *
     H*  02/23/11  C:227 Support: 75402  MC133                        *
     H*                           Provide functionality of manual over*
     H*                           ride for inaptient hospital bills   *
     H*                                                               *
     H*  09/27/10  C:227 Project: MS019                               *
     H*                           MS 2010 Fee Schedule - Changes to   *
     H*                           fix the way for getting data from   *
     H*                           FTDGP and FTIIP.                    *
     H*                                                               *
     H*  01/13/09  C:227 Project: MS018                               *
     H*                           MS 10/1/2008 Inpat & Outlier Update *
     H*                                                               *
     H*****************************************************************
     Fftdgl     if   e           k disk
     Ffaeop     if   e             disk
     Ffdcal     if   e           k disk
     Ffddul3    Uf A e           k disk
     Ffaeul     if   e           k disk
     Fftiil     if   e           k disk
     FFTRTL1    if   e           k disk
     Ffdcalf    IF A E           K DISK
     F                                     RENAME(PCA01:PCA0F)
     Ffdcal19   IF   E           K DISK
     F                                     RENAME(PCA01:PCA19)
     Ffdcal24   UF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     Ffminptl1msif   e           k disk
     D psds          esds                  extname(szq1p)
     *
     D                 ds
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     Dxxcmsbr          s              9  2
     Dxxout            s              9  2
     Dcaddon           s              9  2 inz(0.15)
     Dxxtsh            s              9  2
     Dxxmtsh           s              9  2
     Dxxmsdrg          s              9  2
     Dxxmsbr           s              9  2
     Dxxtdrg           s              9  2
     Dxxelout          s              9  2
     Dxxoutb           s              9  2
     Dxxoutan          s              9  2
     Dxxtotout         s              9  2
     Dxxdrout          s              9  2
     Dsmchrg           s             11  2
     D@@car            s             10
     Dxxcode           s             11
     D@@drg#           s                   like(dgdrg#)
     Dxxchrg           s                   like(cachrg)
     D@@chrg           s                   like(cachrg)
     D*** Beg Add *** 10/29/20 ****************************************
     DkkProv           s             10
     D*** End Add *** 10/29/20 ****************************************

     C     *LIKE         DEFINE    CALIN#        X1LIN#
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
     C     *LIKE         DEFINE    CALIN#        KKLIN#
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    IIOCCR        XXOCCR
     C     *LIKE         DEFINE    DGRELW        XXRELW
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
     C     dgky          klist
     C                   kfld                    @@drg#
     C                   kfld                    kkefdt
     *
     C     dgky1         klist
     C                   kfld                    @@drg#
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
     C* Calculate total charged
     C*
     C                   exsr      calc_tot_chrg
      *
     C*Set the pointer for the FAEUL from the values of FAEOP
     C*
     C     caky1         Chain     faeul                              99
     C                   EVAL      KKEFDT = EUDCDT
     C**************************************************************************
     C* MAIN CODE STARTS HERE
     C*
     C* Set CMS base rate and Outlier threshold according to discharge date
     C     RTKY          SETGT     PRT01
     C     RTKY1         READPE(N) PRT01
     C                   DOW       NOT %EOF
     C                   IF        RTTYPE = 'I'
     C                   Eval      xxcmsbr = RTOPBR
     C                   Eval      xxout = RTOUTL
     C                   LEAVE
     C                   ELSE
     C     RTKY1         READPE(N) PRT01
     C                   ENDIF
     C                   ENDDO
     C*
     C                   eval      @@car = EUMCAR
     C                   eval      @@drg#  = eudrg#
     C*** Beg Add *** 10/29/20 ****************************************
     C                   Select
     C                   When      EUdcdt >= 1201001
     C                   Exsr      CalcCcrOutl
     C                   Other
     C*** End Add *** 10/29/20 ****************************************
     C     ifky          setgt     pii01
     C     ifky1         readpe(n) pii01                                  90
     C                   If        *in90 = *off
     C                   EVAL      XXOCCR = IIOCCR
     C                   endif
      *
      *Calculate the Medicare Outlier Threshold (xxmtsh)
      *
     C                   if        xxoccr = 0
     C                   eval      @@rtcd = 'F'
     C                   Eval(h)   xxtsh = 0
     C                   else
     C                   Eval(h)   xxtsh = xxout / xxoccr
     C                   endif
      *
     C                   Eval(h)   xxmtsh  = xxtsh / 2
      *
     C*** Beg Add *** 10/29/20 ****************************************
     C                   endsl
     C*** End Add *** 10/29/20 ****************************************

      * Retrieve the Relative value
     C     dgky          setgt     pdg01
     C     dgky1         readpe(n) pdg01                                  99
     C                   if        *in99 = *off
     C                   Eval      xxrelw = dgrelw
     C                   Endif
      *
     C                   Eval(h)   xxmsbr = xxcmsbr *2
      * Calculate MS DRG payment (xxmsdrg)
     C                   Eval(h)   xxmsdrg = xxmsbr * xxrelw
      * Total DRG payment (xxtdrg)
     C                   Eval(h)   xxtdrg = xxmsdrg + xxmtsh
      *
     C                   If        xxtdrg < smchrg
      * Eligible outlier charges (xxelout)
     C                   Eval      @@chrg = 0
     C                   EXSR      cal_implant
     C                   Eval      xxelout = smchrg - xxtdrg - @@chrg
     C                   If        xxelout < 0
     C                   Eval      xxelout = 0
     C                   Endif
      *
      * Outlier base (xxoutb)
     C                   Eval(h)   xxoutb = xxelout * xxoccr
      * Outlier Add On  (xxoutan)
     C                   Eval(h)   xxoutan = xxoutb * caddon
      * Total Outlier payment
     C                   Eval(h)   xxtotout = xxoutb + xxoutan
     C
     C                   Endif                                                  xxtdrg < smchrg
      *
      * Total DRG payment with outlier (xxdrout)
     C                   Eval      xxdrout = xxmsdrg + xxoutb + xxoutan
      *
     C                   if        @@rtcd = ' '
     C                   Exsr      lodrcd
     C                   Z-add     xxdrout       @@allow
     C                   endif                                                  @@rtcd = ' '
      *
     C                   seton                                        LR
     C                   return
    C********************************************************************
    C*
    C*  Calculate total charged for this bill
    C*
    C*****************************************************************
     C     calc_tot_chrg begsr
     C*
     C                   eval      smchrg = 0
     C*
     C     caky1         chain(N)  PCA19                              99
     C*
     C                   dow       *in99 = *off
     C*
     C                   eval      smchrg = cachrg + smchrg
     C*
     C     caky1         reade(N)  pca19                                  99
     C*
     C                   enddo
     C*
     C                   endsr
    C********************************************************************
     C* Calculate implant charges

     * 8)  XXCHRG = SMCHRG - any implants that were billed
     C     cal_implant   BEGSR
     C                   Z-add     278           kkrvcd
     C
     C     duky#         setll     pdu01
     C     duky#         reade     pdu01                                  60
     C                   Dow       *in60 = *off
     C                   Eval      kkdseq = dudseq
     C     caky2         chain     PCA0F                              99
     C                   if        *in99 = *off
     C                   eval      @@chrg    = @@chrg +Cachrg
     C                   endif
     C     duky#         reade     pdu01                                  60
     C                   enddo
     C                   Endsr
    C********************************************************************
    C*  Start loading the records in FDDUP & FDCAP
    C*
    C*****************************************************************
     C     lodrcd        begsr
     C                   Movel     'DRG'         kkrate
     C                   Z-add     0             kkrvcd
     C     duky1         chain     pdu01                              60
     C                   if        *in60 = *off
     *
     C                   Z-add     dudseq        kkdseq
     C     caky#         chain     pca02                              60
     C                   if        *in60 = *off
ks01 C                   if        cacscd  <> 'DRG'
     C                   exsr      updpd
     C                   endif                                                  cacscd  <> 'DRG'
     C                   endif
     C                   else
     C                   exsr      wrtpd
     C                   endif
     C     lodrcde       endsr
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
     C     caky3         READPE(N) PCA0F                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C     CADSEQ        ADD       1             XXDSEQ
     C                   ELSE
     C                   Z-ADD     1             XXDSEQ
     C                   ENDIF
     C*
     C                   MOVE      *HIVAL        KKLIN#

     C     CAKY4         SETGT     PCA19
     C     CAKY1         READPE(N) PCA19                                  99
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
     C                   MOVEL     '2  '         CACSCD
      /Free
                         If EUadDt <> 0;
                            CAfDat = EUadDt;
                         Else;
                            CAfDat = EOfrDt;
                         Endif;

                         If EUdcDt <> 0;
                            CAtDat = EUdcDt;
                         Else;
                            CAtDat = EOtoDt;
                         Endif;
      /End-Free
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
     C                   eval      xxcode = '000' + 'DRG'
     C                   Movel     xxcode        cacode
     C                   Z-ADD     *ZERO         CABOPR
     C                   Z-ADD     *ZERO         CABCAP
     C                   Z-ADD     *ZERO         CAOIME
     C                   Z-ADD     *ZERO         CACIME
     C                   Z-ADD     *ZERO         CAODSH
     C                   Z-ADD     *ZERO         CACDSH
     C                   Z-ADD     xxtotout      CAOUTL
     C                   Z-ADD     *ZERO         CATRAN
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
     C                   Z-ADD     *Zeros        DURVCD
     C                   MOVEL     'DRG  '       DURATE
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
     C                   Z-ADD     xxtotout      CAOUTL
     C                   Z-ADD     *Zeros        CATRAN
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
     C*
     C                   update    PDU01
     C     updpde        endsr
      // free
       //*** Beg Add *** 10/29/20 ***************************************
          Begsr CalcCcrOutl ;
            kkProv = EUmCar;
            xxMtsh = 0;
            xxOccr = 0;

            Setgt (kkProv:kkEfDt) pinpt01;
            Readpe (kkProv) pinpt01;
            If Not %Eof(FMINPTL1MS);
               xxMtsh = INoutThrs;
               xxOccr = INoccr;
               Endif;
               If xxOccr = 0;
                  @@rtcd = 'F';
               Endif;
           Endsr;
       //*** End Add *** 10/29/20 ***************************************
      //end-free
