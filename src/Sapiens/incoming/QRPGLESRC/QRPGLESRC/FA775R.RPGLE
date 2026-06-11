     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H* This program will calculate payments based on annual and      *
     H* lifetime maximums.                                            *
     H*****************************************************************
     H*  Created by:                                                  *
     H*  12/14/11  C:251 Project: 114MBB                              *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 02/01/13 C:280 Project: 114MBB                                *
     H*                bring into v13r2 and change copyright          *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     Ffdcap     if   e             disk
     Ffdcalh    uf   e           k disk    rename(pca01:pca0h)
     Ffmfnl     if   e           k disk
     Ffmcml1    if   e           k disk
     Ffaeolg    uf   e           k disk
     Ffatfl1    if   e           k disk
     Ffddel     if   e           k disk    rename(pde01:pde02)
     Ffddel3    uf   e           k disk    rename(pde01:pde03)
     Ffddep     o    e             disk
     D*
     D psds          esds                  extname(szq1p)
     D                 ds
     D  dsdttm                 1     14  0
     D  dstime                 1      6  0
     D  dsdate                 7     14  0
     D*
     Dkkco#            s                   like(caco# )
     Dkksub#           s                   like(casub#)
     Dkkfnd            s                   like(cafnd )
     Dkkfyr            s                   like(cafyr )
     Dkkcase           s                   like(cacase)
     Dkkdseq           s                   like(cadseq)
     Dkkeob#           s                   like(caeob#)
     Dxxpaid           s                   like(capaid)
     Dxxprv            s                   like(capaid)
     Dxxprvsav         s                   like(capaid)
     Dxxsav            s                   like(capaid)
     Dhdseq#           s                   like(deseq#)
     Dkkseq#           s                   like(deseq#)
     D*
     Dxxnot1           s            150    inz('The $50,000.00 annual or-
     D                                      $1,000,000.00 lifetime-
     D                                      aggregate has now been-
     D                                      exhausted for the claimant. No-
     D                                      further benefits are available.')
     C*
     C     caky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C*
     C     fnky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C*
     C     caky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C*
     C     tfky          klist
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C*
     C     cmky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C*
     C     eoky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C*
     C     deky2         klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    cadseq
     C                   kfld                    kkseq#
     C*
     C     deky1         klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    cadseq
     C*
     C     deky3         klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    eorcn
     C                   kfld                    cadseq
     C*
     C     *entry        plist
     C                   parm                    @@rrn             9 0
     C                   parm                    @@mode            1
     C*
     C*** Chain on FDCAP
     C*
     C     @@rrn         chain     pca01                              99
     C*
     C                   if        *in99 = *off
     C                   eval      kkco#  = caco#
     C                   eval      kksub# = casub#
     C                   eval      kkfnd  = cafnd
     C                   eval      kkfyr  = cafyr
     C                   eval      kkcase = cacase
     C                   eval      kkeob# = caeob#
     C                   eval      kkdseq = cadseq
     C                   endif                                                  *in99 = '0'
     C*
     C*** Chain on FAEOP
     C*
     C                   clear                   peo01
     C     eoky          chain     peo01
     C*
     C*** Chain on FMFNP
     C*
     C                   clear                   pfn01
     C     fnky          chain     pfn01
     C*
     C*** Chain on FMCMP
     C*
     C                   clear                   pcm01
     C     cmky          chain     pcm01
     C*
     C                   eval      xxpaid = 0
     C                   eval      xxprv  = 0
     C                   eval      xxprvsav = 0
     C                   eval      xxsav  = 0
     C*
     C                   select
     C                   when      @@mode = 'D'
     C*
     C                   exsr      ChkBck
     C                   if        cafdat<= tfendt and cafdat >= tfbgdt
     C                   eval      xxsav  = xxpaid
     C                   exsr      AdjPmt
     C                   endif
     C*
     C                   when      @@mode = 'A'
     C*
     C     caky2         setll     pca0h
     C     caky2         reade     pca0h
     C*
     C                   exsr      ChkBck
     C*
     C                   dow       not %eof (fdcalh)
     C                   eval      xxprv  = xxpaid
     C                   eval      xxpaid = xxpaid + capaid
     C                   eval      xxprvsav = xxsav
     C                   eval      xxsav = xxsav + capaid
     C*
     C                   if        cafdat <= tfendt and cafdat >= tfbgdt
     C                   exsr      AdjPmt
     C                   else
     C                   exsr      ChkBck
     C                   eval      xxpaid = capaid
     C                   exsr      AdjPmt
     C                   endif
     C*
     C     caky2         reade     pca0h
     C                   enddo
     C*
     C                   endsl
     C*
     C                   seton                                        lr
     C*================================================================
     C* ChkBck - Check Bucket
     C*================================================================
     C     ChkBck        Begsr
     C*
     C                   eval      xxpaid = 0
     C                   eval      xxprv  = 0
     C*
     C* Check for the particular bucket in FATFP
     C*
     C     tfky          setll     ptf01
     C     tfky          reade     ptf01
     C*
     C                   dow       not %eof (fatfl1)
     C                   if        cafdat<= tfendt and cafdat >= tfbgdt
     C                   leave
     C                   endif
     C*
     C     tfky          reade     ptf01
     C                   enddo
     C*
     C                   endsr
     C*================================================================
     C* AdjPmt - Adjust payment
     C*================================================================
     C     AdjPmt        begsr
     C*
     C* Adjust Payment
     C*
     C                   select
     C                   when      (xxpaid + tfanpy) > fnmpln
     C*
     C                   eval      capaid = fnmpln - (xxprv + tfanpy)
     C*
     C                   if        capaid < 0
     C                   eval      capaid = 0
     C                   endif
     C*
     C                   eval      caoth2 = cachrg - camra - caothr - candis
     C                             - capaid
     C                   eval      caot2c = 'PLM'
     C                   update    pca0h
     C                   exsr      wrtdep
     C*
     C                   when      (xxsav + cmlfpy) > 1000000
     C*
     C                   eval      capaid = 1000000 - (xxprvsav + cmlfpy)
     C*
     C                   if        capaid < 0
     C                   eval      capaid = 0
     C                   endif
     C*
     C                   eval      caoth2 = cachrg - camra - caothr - candis
     C                             - capaid
     C                   eval      caot2c = 'PLM'
     C                   update    pca0h
     C                   exsr      wrtdep
     C*
     C                   endsl
     C*
     C                   endsr
     C*================================================================
     C* WRTDEP - Write FDDEP record
     C*================================================================
     C     wrtdep        begsr
     C*
     C                   z-add     *hival        kkseq#
     C     deky2         setgt     pde02
     C     deky1         readpe    pde02                                  99
     C     *in99         ifeq      *off
     C     deseq#        add       1             hdseq#
     C                   else
     C                   z-add     1             hdseq#
     C                   endif
     C*
     C                   clear                   pde01
     C                   z-add     caco#         deco#
     C                   z-add     casub#        desub#
     C                   z-add     cafnd         defnd
     C                   z-add     cafyr         defyr
     C                   z-add     cacase        decase
     C                   movel     cass#         dess#
     C                   z-add     caacdt        deacdt
     C                   z-add     caeob#        deeob#
     C                   z-add     kkdseq        dedseq
     C                   z-add     hdseq#        deseq#
     C                   add       1             hdseq#
     C*
     C                   movel(p)  '230'         DEEOB
     C
     C                   movel(p)  xxnot1        DENOT1
     C                   eval      denot2 = %subst(xxnot1:71:70)
     C*
     C                   movel     'A'           DESTAT
     C                   movel(p)  CAUSER        DEUSER
     C                   movel(p)  capgm         DEPGM
     C                   z-add     CATIME        DETIME
     C                   z-add     CADT          DEDT
     C                   movel(p)  CAUSER        DEIUSR
     C                   movel(p)  CApgm         DEIpgm
     C                   z-add     CAITIM        DEITIM
     C                   z-add     CAIDT         DEIDT
     C                   movel(p)  'U'           DEESRC
     C                   z-add     0             DERCN
     C                   movel(p)  'Y'           DEPRNT
     C                   write     PDE01
     C*
     C                   endsr
