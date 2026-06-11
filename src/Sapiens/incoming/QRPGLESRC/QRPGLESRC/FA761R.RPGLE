     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H*     This program will create a provider bill line item.       *
     H*****************************************************************
     H*  CREATED BY                                                   *
     H*                                                               *
     H*  07/03/09  C:227 Project: AR038                               *
     H*                           AR 2008 FS                          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     Ffaeop     if   e             disk
     Ffdcalf    uf a e           k disk
     Ffdcal19   uf a e           k disk    rename(pca01:pca19)
     Ffddhl3    uf a e           k disk
     Ffddul3    uf a e           k disk
     Ffdddl2    uf a e           k disk
     Ffddpl3    uf a e           k disk
     F*
     D psds          esds                  extname(szq1p)
     *
     D                 ds
     D  dsdttm                 1     14  0
     D  dstime                 1      6  0
     D  dsdate                 7     14  0
     Dsmchrg           s             11  2
     Dxxcode           s             11
     Dxxchrg           s                   like(cachrg)
     D@@chrg           s                   like(cachrg)
     Dxxrcfl           s                   like(carcfl)

     C     *like         define    calin#        x1lin#
     C     *like         define    cadseq        xxdseq
     C     *like         define    calin#        kklin#
     C     *like         define    cadseq        kkdseq
     C     *like         define    cacpt4        kkprcd
     C     *like         define    durvcd        kkrvcd
     C     *like         define    durate        kkrate
     C     *like         define    eoco#         kkco#
     C     *like         define    eosub#        kksub#
     C     *like         define    eofnd         kkfnd
     C     *like         define    eofyr         kkfyr
     C     *like         define    eocase        kkcase
     C     *like         define    eoeob#        kkeob#
     C     *like         define    dpndc         kkndc
      *
     C     *entry        plist
     C                   parm                    @@rrn             9 0
     C                   parm                    @@rvcd            3 0
     C                   parm                    @@prcd            5
     C                   parm                    @@ndc            11
     C                   parm                    @@desc           25
     C                   parm                    @@rtcd            1
     *
     C     dtcymd1       plist
     C                   parm                    srdti1            8 0
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
     C     dtcymd        plist
     C                   parm                    srdti             8 0
     C                   parm      *BLANKS       srffmt            8
     C                   parm      '*CYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
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
     C     dhky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkprcd
     *
     C     dpky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkndc
     *
     C     caky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C*
     C     caky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkdseq
     C*
     C     caky3         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C*
     C     caky4         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kklin#
     C*
     C*
     C     caky#         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkdseq
     C*
     C*Set the pointer for the FAEOP from the RRN
     C*
     C                   move      ' '           @@rtcd
     C     @@rrn         chain     peo01                              99
     C                   if        *in99 = *on
     C                   Move      'F'           @@rtcd
     C                   seton                                        LR
     C                   return
     C*
     C                   else                                                   *in99=*on
     C*
     C                   Z-add     eoco#         kkco#
     C                   Z-add     eosub#        kksub#
     C                   Z-add     eofnd         kkfnd
     C                   Z-add     eofyr         kkfyr
     C                   Z-add     eocase        kkcase
     C                   Z-add     eoeob#        kkeob#
     C*
     C                   select
     C                   when      eobtyp = 'N'
     C                   exsr      med
     C                   when      eobtyp = 'H'
     C                   exsr      hosp
     C                   when      eobtyp = 'D'
     C                   exsr      dent
     C                   when      eobtyp = 'X'
     C                   exsr      phar
     C                   endsl
     C                   endif
     C*
     C                   seton                                        LR
     C                   return
    C********************************************************************
    C*  Subroutine MED
    C*****************************************************************
     C     med           begsr
     C*
     C                   eval      xxrcfl = 'C'
     C                   movel     @@prcd        kkprcd
     C     dhky1         chain     pdh01
     C                   if        not %found
     C                   exsr      wrtcap
     C                   exsr      wrtdh
     C                   endif
     C*
     C     endmed        endsr
    C********************************************************************
    C*  Subroutine HOSP
    C*****************************************************************
     C     hosp          begsr
     C*
     C                   eval      xxrcfl = 'H'
     C                   movel     @@rvcd        kkrvcd
     C                   movel     @@prcd        kkprcd
     C     duky1         chain     pdu01
     C                   if        not %found
     C                   exsr      wrtcap
     C                   exsr      wrtdu
     C                   endif
     C*
     C     endhos        endsr
    C********************************************************************
    C*  Subroutine DENT
    C*****************************************************************
     C     dent          begsr
     C*
     C                   eval      xxrcfl = 'D'
     C                   movel     @@prcd        kkprcd
     C     dhky1         chain     pdd01
     C                   if        not %found
     C                   exsr      wrtcap
     C                   exsr      wrtdd
     C                   endif
     C*
     C     endden        endsr
    C********************************************************************
    C*  Subroutine PHAR
    C*****************************************************************
     C     phar          begsr
     C*
     C                   eval      xxrcfl = 'X'
     C                   movel     @@ndc         kkndc
     C     dpky1         chain     pdp01
     C                   if        not %found
     C                   exsr      wrtcap
     C                   exsr      wrtdp
     C                   endif
     C*
     C     endpha        endsr
    C********************************************************************
    C*  Calculate total charged for this bill
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
    C*  Write the records in FDCAP and FDDUP
    C*****************************************************************
     C     wrtcap        begsr
     C*
     C* Get the next available seq# & line#
     C*
     C                   time                    dsdttm
     C                   move      *hival        kkdseq
     C*
     C     caky2         setgt     pca01
     C     caky3         readpe(n) pca01                                  99
     C*
     C     *in99         ifeq      *off
     C     cadseq        add       1             xxdseq
     C                   else
     C                   z-add     1             xxdseq
     C                   endif
     C*
     C                   move      *hival        kklin#

     C     caky4         setgt     pca19
     C     caky1         readpe(n) pca19                                  99
     C*
     C     *in99         ifeq      *off
     C     calin#        add       1             x1lin#
     C                   else
     C                   z-add     1             x1lin#
     C                   endif
     C*
     C                   z-add     eoco#         caco#
     C                   z-add     eosub#        casub#
     C                   z-add     eofnd         cafnd
     C                   z-add     eofyr         cafyr
     C                   z-add     eocase        cacase
     C                   movel     '1  '         cacscd
     C                   z-add     eofrdt        cafdat
     C                   z-add     eotodt        catdat
     C                   z-add     dsdate        srdti
     C                   CALL      'SRCVTDAT'    dtcymd
     C                   movel     srdto         cardat
     C                   z-add     eofein        cafein
     C                   z-add     eomod         camod
     C                   movel     kkprcd        cacpt4
     C                   movel     *blanks       caflag
     C                   z-add     eoeob#        caeob#
     C                   movel     eoss#         cass#
     C                   z-add     eoacdt        caacdt
     C                   movel     *blanks       cadxr
     C                   movel     xxrcfl        carcfl
     C                   movel     eorcn         carcn
     C                   z-add     x1lin#        calin#
     C                   z-add     xxdseq        cadseq
     C                   eval      cachrg = 0
     C                   eval      camra  = 0
     C                   eval      candis = 0
     C                   eval      caothr = 0
     C                   eval      caoth2 = 0
     C                   eval      capaid = 0
     C                   movel     'A'           castat
     C                   movel     q1user        causer
     C                   movel     q1pgm         capgm
     C                   z-add     dstime        catime
     C                   z-add     dsdate        srdti
     C                   CALL      'SRCVTDAT'    dtcymd
     C                   movel     srdto         cadt
     C                   movel     q1user        caiusr
     C                   movel     q1pgm         caipgm
     C                   z-add     dstime        caitim
     C                   z-add     dsdate        srdti
     C                   CALL      'SRCVTDAT'    dtcymd
     C                   movel     srdto         caidt
     C*
     C*
     C                   MOVE      'X'           cadelf
     C                   eval      xxcode = '100'
     C                   movel     xxcode        cacode
     C*
     C                   write     pca01
     C*
     C     endcap        endsr
    C********************************************************************
    C*  Write the records in FDDUP
    C*****************************************************************
     C     wrtdu         begsr
     C*
     C                   z-add     eoco#         duco#
     C                   z-add     eosub#        dusub#
     C                   z-add     eofnd         dufnd
     C                   z-add     eofyr         dufyr
     C                   z-add     eocase        ducase
     C                   movel     cass#         duss#
     C                   z-add     eoacdt        duacdt
     C                   z-add     xxdseq        dudseq
     C                   z-add     eoeob#        dueob#
     C                   move      '21'          dupos
     C                   z-add     @@rvcd        durvcd
     C                   movel     @@prcd        durate
     C                   movel     @@desc        dudesc
     C                   z-add     1             duunts
     C                   movel     *blanks       dubrat
     C                   movel     *zeros        dubprc
     C                   movel     *blanks       dubprd
     C                   movel     'A'           dustat
     C                   movel     q1user        duuser
     C                   movel     q1user        duiusr
     C                   movel     q1pgm         dupgm
     C                   movel     q1pgm         duipgm
     C                   z-add     dstime        dutime
     C                   z-add     dstime        duitim
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         dudt
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         duidt
     C*
     C                   write     pdu01
     C*
     C     enddu         endsr
    C********************************************************************
    C*  Write the records in FDDHP
    C*****************************************************************
     C     wrtdh         begsr
     C*
     C     enddh         endsr
    C********************************************************************
    C*  Write the records in FDDDP
    C*****************************************************************
     C     wrtdd         begsr
     C*
     C     enddd         endsr
    C********************************************************************
    C*  Write the records in FDDPP
    C*****************************************************************
     C     wrtdp         begsr
     C*
     C     enddp         endsr
    C********************************************************************
    C*  Update the records in FDCAP and FDDUP
    C********************************************************************
     C*****updpd         begsr
     C*****              time                    dsdttm
    C*
    C*Update the FDCAP
     C*****              z-add     dsdate        srdti
     C*****              CALL      'SRCVTDAT'    dtcymd
     C*****              movel     srdto         cadt
     C*****              movel     *blanks       cadxr
     C*****              movel     'D'           carcfl
     C*****              movel     eosub#        casub#
     C*****              z-add     *zero         caothr
     C*****              z-add     *zero         caoth2
     C*****              z-add     *zero         candis
     C*****              z-add     xxdrout       capaid
     C*****              z-add     *zeros        cachrg
     C*****cachrg        sub       capaid        camra
     C*****              z-add     xxdrout       camraa
     C*****              z-add     *zeroS        camnaa
     C*****              z-add     *zeros        cabopr
     C*****              z-add     *zeros        cabcap
     C*****              z-add     *zeros        caoime
     C*****              z-add     *zeros        cacime
     C*****              z-add     *zeros        caodsh
     C*****              z-add     *zeros        cacdsh
     C*****              z-add     xxtotout      caoutl
     C*****              z-add     *zeros        catran
     C*****              update    pca02
    C*
     C*****              movel     'A'           dustat
     C*****              movel     q1user        duuser
     C*****              movel     q1user        duiusr
     C*****              movel     q1pgm         dupgm
     C*****              movel     q1pgm         duipgm
     C*****              z-add     dstime        dutime
     C*****              z-add     dstime        duitim
     C*****              z-add     dsdate        srdti
     C*****              call      'SRCVTDAT'    dtcymd
     C*****              movel     srdto         dudt
     C*****              z-add     dsdate        srdti
     C*****              call      'SRCVTDAT'    dtcymd
     C*****              movel     srdto         duidt
     C*
     C*****              update    pdu01
     C*****updpde        endsr
