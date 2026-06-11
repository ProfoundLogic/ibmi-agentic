     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H* This program will be used to verify whether claim has reached *
     H* its limit setup for its group.                                *
     H*****************************************************************
     H*  Created by:                                                  *
     H*  06/22/11  C:198 Project: 114MYS                              *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     Ffdcap     if   e             disk
     Ffdcalf    uf   e           k disk    rename(pca01:pca0f)
     Ffdcalh    uf   e           k disk    rename(pca01:pca0h)
     Ffddel     if   e           k disk    rename(pde01:pde02)
     Ffddel3    uf   e           k disk    rename(pde01:pde03)
     Ffmfnl     if   e           k disk
     Ffddep     o    e             disk
     Ffaeolg    uf   e           k disk
     D psds          esds                  extname(szq1p)
     D                 ds
     D  dsdttm                 1     14  0
     D  dstime                 1      6  0
     D  dsdate                 7     14  0
     D*
     Dkkseq#           s                   like(deseq#)
     Dhdseq#           s                   like(deseq#)
     Dkkco#            s                   like(caco# )
     Dkksub#           s                   like(casub#)
     Dkkfnd            s                   like(cafnd )
     Dkkfyr            s                   like(cafyr )
     Dkkcase           s                   like(cacase)
     Dkkdseq           s                   like(cadseq)
     Dkkeob#           s                   like(caeob#)
     Dxxtot            s                   like(capaid)
     Dxxtot1           s                   like(capaid)
     Dxxnot1           s            150    inz('The $5,000.00 lifetime-
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
     C     caky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkdseq
     C*
     C     caky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
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
     C*
     C     eoky          chain     peo01                              99
     C*
     C*** Chain on FMFNP
     C*
     C                   clear                   pfn01
     C*
     C     fnky          chain     pfn01                              99
     C*
     C                   eval      xxtot = 0
     C                   eval      xxtot1 = 0
     C*
     C                   select
     C                   when      @@mode = 'D'
     C*** add up the CAPAID for this claim except the current eob#
     C     caky          chain(n)  pca0h                              99
     C                   dow       *in99 = *off
     C                   if        caflag <> 'P'
     C                   if        caeob# <> kkeob#
     C                   eval      xxtot = xxtot + capaid
     C                   endif                                                  caeob# <> kkeob#
     C                   endif                                                  caflag = 'P'
     C     caky          reade     pca0h                                  99
     C                   enddo                                                  *in99 = *off
     C***
     C     caky1         chain     pca0f                              99
     C*
     C                   eval      xxtot1 = xxtot + capaid
     C*
     C* Delete existing EOB code 230 unless it was previously reduced
     C*
     C                   if        xxtot1 <> fnmpln
     C                   exsr      dltdep
     C                   endif
     C*
     C                   if        xxtot1 >  fnmpln
     C                   eval      capaid = fnmpln - xxtot
     C*
     C                   if        capaid < 0
     C                   eval      capaid = 0
     C                   endif
     C*
     C                   eval      caoth2 = cachrg - camra - caothr - candis
     C                             - capaid
     C                   eval      caot2c = 'PLM'
     C                   update    pca0f
     C                   exsr      wrtdep
     C                   endif
     C*
     C                   when      @@mode = 'A'
     C*** add up the CAPAID for this claim except the current line item
     C     caky          chain     pca0h                              99
     C                   dow       *in99 = *off
     C                   if        cadseq <> kkdseq
     C                             and caflag <> 'P'
     C                   eval      xxtot = xxtot + capaid
     C                   endif
     C     caky          reade     pca0h                                  99
     C                   enddo
     C***
     C     caky1         chain     pca0f                              99
     C*
     C                   eval      xxtot1 = xxtot + capaid
     C*
     C* Delete existing EOB code 230 unless it was previously reduced
     C*
     C                   if        xxtot1 <> fnmpln
     C                   exsr      dltdep
     C                   endif
     C*
     C                   if        xxtot1 > fnmpln
     C                   eval      capaid = fnmpln - xxtot
     C*
     C                   if        capaid < 0
     C                   eval      capaid = 0
     C                   endif
     C*
     C                   eval      caoth2 = cachrg - camra - caothr - candis
     C                             - capaid
     C                   eval      caot2c = 'PLM'
     C                   update    pca0f
     C                   exsr      wrtdep
     C                   endif
     C*
     C                   endsl
     C*
     C                   seton                                        lr
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
     C*================================================================
     C* DLTDEP - Delete FDDEP record for EOB code 230
     C*================================================================
     C     dltdep        begsr
     C*
     C     deky3         chain     pde03                              99
     C*
     C                   dow       *in99 = *off
     C*
     C                   if        deeob = '230' and dercn = eorcn
     C                   delete    PDE03
     C                   endif
     C*
     C     deky3         reade     pde03                                  99
     C*
     C                   enddo
     C*
     C                   endsr
