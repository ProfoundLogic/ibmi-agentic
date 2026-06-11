     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('STBNDDIR')
     H*****************************************************************
     H*  CREATED BY                                                   *
     H*                                                               *
     H*  03/13/15  C:296 Project: DE012 (DRG Update)                  *
     H*                                                               *
     H*            This program will create a DRG line item. It is    *
     H*            modelled on FA756R                                 *
     H*                                                               *
     H*  ***NOTE*** Any changes to Drg Payment Calc. should be updated*
     H*             to F0366R.                                        *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/06/15 C: 079 Project: DE012                                *
     H*                 Right justify DRG code key                    *
     H*                                                               *
     H*****************************************************************
     Ffaeop     if   e             disk
     Ffdcal     if   e           k disk
     Ffddul3    Uf A e           k disk
     Ffaeul     if   e           k disk
     Ffdcalf    IF A E           K DISK
     F                                     RENAME(PCA01:PCA0F)
     Ffdcal19   IF   E           K DISK
     F                                     RENAME(PCA01:PCA19)
     Ffdcal24   UF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     Ffmprlde   if   e           k DISK
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
     D@@drg#           s              5A
     Dxxchrg           s                   like(cachrg)
     D@@chrg           s                   like(cachrg)
     D*
     D*** Beg Add *** 04/06/15 *********************************************
     D/COPY sPrString
     D*** Beg Add *** 04/06/15 *********************************************

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
       @@drg#  = %char(eudrg#);
       //*** Beg Add *** 04/06/15 **************************************
       eval      @@drg# = RightJstfy(@@Drg#);
       //*** End Add *** 04/06/15 **************************************

       setgt (euco#: @@drg#: kkefdt) ppr01;
       readpe (euco#: @@drg#) ppr01;

          if not %eof (fmprlde);

             select;
             when %subst(eoflic:1:3) = '197' or
              %subst(eoflic:1:3) = '198';
                @@allow = pr92nu;
                xxdrout = pr92nu;
             when %subst(eoflic:1:3) = '199';
                @@allow = pr92nv;
                xxdrout = pr92nv;
             other;
             endsl;

          endif;

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
       //*** Beg Chg *** 04/06/15 **************************************
       //***************  if cacscd  <> *blanks;
                          if cacscd  <> 'DRG';
       //*** End Chg *** 04/06/15 **************************************
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
     C*****              Z-ADD     xxtotout      CAOUTL
     C                   Z-ADD     *Zero         CAOUTL
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
     C                   Z-ADD     100           DURVCD
     C                   MOVEL     *blanks       DURATE
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
     C******             Z-ADD     xxtotout      CAOUTL
     C                   Z-ADD     *zero         CAOUTL
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
