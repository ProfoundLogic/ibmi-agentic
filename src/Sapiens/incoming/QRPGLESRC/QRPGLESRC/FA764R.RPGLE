     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H*  CREATED BY                                                   *
     H*                                                               *
     H*  08/11/10  C:227 Project: IL001                               *
     H*            This program will calculate IL DRG payment         *
     H*            including Outlier                                  *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  01/13/14  C:274 Project: IL006                               *
     H*                  Redo outlier logic and calculations.         *
     H*                                                               *
     H*  02/27/13  C:284 Project: IL005                               *
     H*                  Add logic to change description for MS-DRG   *
     H*                                                               *
     H*  02/21/12  C:277 Project: IL004                               *
     H*                  If discharge date < 1/1/2012 the read FMFSPIL*
     H*                  Else if discharge date >= 1/1/2012 FMF1PIL   *
     H*                                                               *
     H*  09/15/11  C:251 Project: IL003                               *
     H*                  Condition outlier payment on discharge date  *
     H*                                                               *
     H*  05/10/11  C:257 Support: 76558                               *
     H*            Load leading blanks in the key field, KKDRG, for   *
     H*            DRG code when reading FMFSPIL                      *
     H*                                                               *
     H*****************************************************************
     Ffaeop     if   e             disk
     Ffaeul     if   e           k disk
     Ffdcal26   if   e           k disk
     Ffddul     if   e           k disk
     Ffmfsl1il  if   e           k disk
     F*** Beg Add *** 02/21/12 *************************************************
     Ffmf1l1il  if   e           k disk
     F*** End Add *** 02/21/12 *************************************************
     D psds          esds                  extname(szq1p)
     *
     D                 ds
     D  dsdttm                 1     14  0
     D  dstime                 1      6  0
     D  dsdate                 7     14  0
     D*** BEG CHG 05/10/11 *****************************************************
     D                 DS
     D##prcpt                  1      5
     D##prcpt1                 1      4
     D##prcpt2                 1      3
     D##prcpt3                 1      2
     D*
     D*** END CHG 05/10/11 *****************************************************

     C     *like         define    eudcdt        kkefdt
     C     *like         define    cachrg        ##psth
     C     *like         define    cachrg        totchg
     C     *like         define    eoco#         kkco#
     C     *like         define    eosub#        kksub#
     C     *like         define    eofnd         kkfnd
     C     *like         define    eofyr         kkfyr
     C     *like         define    eocase        kkcase
     C     *like         define    eoeob#        kkeob#
     C     *like         define    cadseq        kkdseq
     C     *like         define    fszip         kkzip
     C     *like         define    fscode        kkdrg
     C     *like         define    fscat         kkcat
     C*** Beg Add *** 02/21/12 *************************************************
     C     *like         define    f1rgn#        kkrgn#
     C     *like         define    f1code        kkcode
     C     *like         define    f1cat         kkcat
     C     *like         define    f1efdt        kkefdt
     C*** End Add *** 02/21/12 *************************************************
     C*** Beg Add *** 01/13/14 *************************************************
     C     *like         define    cachrg        ##implant
     C*** End Add *** 01/13/14 *************************************************
      *
     C     *entry        plist
     C                   parm                    @@rrn             9 0
     C                   parm                    @@svdt            7 0
     C                   parm                    @@alow            9 2
     C                   parm                    @@perc            5 2
     C                   parm                    @@rtcd            1
     *
     C     caky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
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
     C     fsky          klist
     C                   kfld                    kkzip
     C                   kfld                    kkdrg
     C                   kfld                    kkcat
     C                   kfld                    kkefdt
     C*
     C     fsky1         klist
     C                   kfld                    kkzip
     C                   kfld                    kkdrg
     C                   kfld                    kkcat
     C*** Beg Add *** 02/21/12 *************************************************
     C*
     C     f1ky          klist
     C                   kfld                    kkrgn#
     C                   kfld                    kkdrg
     C                   kfld                    kkcat
     C                   kfld                    kkefdt
     C*
     C     f1ky1         klist
     C                   kfld                    kkrgn#
     C                   kfld                    kkdrg
     C                   kfld                    kkcat
     C*** End Add *** 02/21/12 *************************************************
     C*
     C*Set the pointer for the FAEOP from the rrn
     C*
     C                   move      ' '           @@rtcd
     C                   z-add     0             @@alow
     C     @@rrn         chain     faeop                              99
     C                   if        *in99 = *on
     C                   move      'F'           @@rtcd
     C                   seton                                        LR
     C                   return
     C                   endif                                                  *in99=*on
     C*
     C                   Z-add     eoco#         kkco#
     C                   Z-add     eosub#        kksub#
     C                   Z-add     eofnd         kkfnd
     C                   Z-add     eofyr         kkfyr
     C                   Z-add     eocase        kkcase
     C                   Z-add     eoeob#        kkeob#
     C*
     C* Accumulate charge amount for pass through drugs
     C*
     C                   exsr      calc_tot_chrg
     C*
     C*Check if the drug code is POC
     C*
     C     caky1         chain     peu01
     C                   if        %found
     C*** BEG CHG 05/10/11 *****************************************************
     C************       move      *all'0'       kkdrg
     C                   move      *blanks       kkdrg
     C                   move      eudrg#        ##prcpt
     C*
     C                   if        ##prcpt1 = '0000'
     C                   move      *blanks       ##prcpt1
     C                   else
     C                   if        ##prcpt2 = '000'
     C                   move      *blanks       ##prcpt2
     C                   else
     C                   if        ##prcpt3 = '00'
     C                   move      *blanks       ##prcpt3
     C                   endif                                                  ##prcpt3
     C                   endif                                                  ##prcpt2
     C                   endif                                                  ##prcpt1
     C                   eval      kkdrg  = ##prcpt
     C********           move      eudrg#        kkdrg
     C*** END CHG 05/10/11 *****************************************************
     C                   movel     eoflic        kkzip
     C                   eval      kkefdt = eudcdt
     C                   if        eospcd = 'TC'
     C                   eval      kkcat = 'Trauma Admis - MS-DRG'
     C                   else                                                   eospcd = TC
     C                   eval      kkcat = 'Standard - MS-DRG'
     C                   endif                                                  eospcd = TC
     C*
     C*** Beg Add *** 02/21/12 ************************************************
     C                   if        eudcdt < 1120101
     C*** End Add *** 02/21/12 ************************************************
     C     fsky          setgt     pfs01
     C     fsky1         readpe    pfs01                                  60
     C                   if        *in60 = *off
     C                   if        eospcd = 'TC'
     C                   if        fsdtpo > 0
     C                   eval      @@alow = (totchg - ##psth) * fsdtpo / 100
     C                   else                                                   fsdtpo > 0
     C                   eval      @@alow = fsdrgt
     C                   endif                                                  fsdtpo > 0
     C                   else                                                   eospcd = TC
     C                   if        fsdspo > 0
     C                   eval      @@alow = (totchg - ##psth) * fsdspo / 100
     C                   else                                                   fsdspo > 0
     C                   eval      @@alow = fsdrgs
     C                   endif                                                  fsdspo > 0
     C                   endif                                                  eospcd = TC
     C*
     C*Check if cost outlier payment should be made
     C*
     C*****              if        fsdtpo > 0 or fsdspo > 0
     C*** Beg Del *** 01/13/14 ***************************************************
     C*****              if        (totchg - ##psth) >= (2 * @@alow)
     C*** Beg Add *** 09/15/11 ***************************************************
     C*****              select
     C*****              when      eudcdt < 1110901
     C*** End Add *** 09/15/11 ***************************************************
     C*****              eval      @@alow = @@alow + (0.76 * ((totchg - ##psth)
     C*****                        -@@alow))
     C*** Beg Add *** 09/15/11 ***************************************************
     C*****              other
     C*****              eval      @@alow = @@alow + (0.532 * ((totchg - ##psth)
     C*****                        -@@alow))
     C*****              endsl
     C*** End Add *** 09/15/11 ***************************************************
     C*****              endif
     C*** End Del *** 01/13/14 ***************************************************
     C*****              endif
     C*** Beg Add *** 01/13/14 ***************************************************
     C                   If        eudcdt < 1110901
     C                             and (totchg - ##psth) >= (2 * @@alow)
     C                   eval      @@alow = @@alow + (0.76 * ((totchg - ##psth)
     C                             -@@alow))
     C                   endif
     C
     C                   If        eudcdt >= 1110901
     C                             and (totchg - ##psth) >= (2.857 * @@alow)
     C                   eval      @@alow = @@alow + (0.532 * (totchg - @@alow)
     C                             + .65 * (##psth - ##Implant))
     C                   endif
     C*** End Add *** 01/13/14 ***************************************************
     C                   endif                                                  *in60 = *off
     C*** Beg Add *** 02/21/12 ***************************************************
     C                   else
     C                   if        eudcdt >= 1120101
     C                   if        eospcd = 'TC'
     C*** Beg Add *** 02/27/13 *************************************************
     C                   if        eudcdt < 1130101
     C                   eval      kkcat = 'Trauma Admis - DRG'
     C                   else
     C                   eval      kkcat = 'Trauma Admis - MS-DRG'
     C                   endif                                                  eospcd = TC
     C*** End Add *** 02/27/13 *************************************************
     C                   else                                                   eospcd = TC
     C*** Beg Add *** 02/27/13 *************************************************
     C                   if        eudcdt < 1130101
     C                   eval      kkcat = 'Standard - DRG'
     C                   else
     C                   eval      kkcat = 'Standard - MS-DRG'
     C                   endif                                                  eospcd = TC
     C*** End Add *** 02/27/13 *************************************************
     C                   endif                                                  eospcd = TC
     C                   eval      kkrgn# = eorgn#
     C     f1ky          setgt     pf101
     C     f1ky1         readpe    pf101                                  60
     C                   if        *in60 = *off
     C                   if        eospcd = 'TC'
     C                   if        f1dtpo > 0
     C                   eval      @@alow = (totchg - ##psth) * f1dtpo / 100
     C                   else                                                   fsdtpo > 0
     C                   eval      @@alow = f1drgt
     C                   endif                                                  fsdtpo > 0
     C                   else                                                   eospcd = TC
     C                   if        f1dspo > 0
     C                   eval      @@alow = (totchg - ##psth) * f1dspo / 100
     C                   else                                                   fsdspo > 0
     C                   eval      @@alow = f1drgs
     C                   endif                                                  fsdspo > 0
     C                   endif                                                  eospcd = TC
     C*
     C*Check if cost outlier payment should be made
     C*
     C*** Beg Del *** 01/13/14 ***************************************************
     C*******            if        (totchg - ##psth) >= (2 * @@alow)
     C*******            select
     C*******            when      eudcdt < 1110901
     C*******            eval      @@alow = @@alow + (0.76 * ((totchg - ##psth)
     C*******                      -@@alow))
     C*******            other
     C*******            eval      @@alow = @@alow + (0.532 * ((totchg - ##psth)
     C*******                      -@@alow))
     C*******            endsl
     C*******            endif
     C*** End Del *** 01/13/14 ***************************************************
     C*** Beg Add *** 01/13/14 ***************************************************
     C                   If        eudcdt < 1110901
     C                             and (totchg - ##psth) >= (2 * @@alow)
     C                   eval      @@alow = @@alow + (0.76 * ((totchg - ##psth)
     C                             -@@alow))
     C                   endif
     C
     C                   If        eudcdt >= 1110901
     C                             and (totchg - ##psth) >= (2.857 * @@alow)
     C                   eval      @@alow = @@alow + (0.532 * (totchg - @@alow)
     C                             + .65 * (##psth - ##Implant))
     C                   endif
     C*** End Add *** 01/13/14 ***************************************************
     C                   endif                                                  *in60 = *off
     C                   endif
     C                   endif
     C*** End Add *** 02/21/12 ***************************************************
     C                   endif                                                  %found faeup
     C*
     C                   seton                                        LR
     C                   return
    C**********************************************************************************
    C*  Accumulate total charge for pass through drugs
    C**********************************************************************************
     C     calc_tot_chrg begsr
     C*
     C                   eval      ##psth = 0
     C                   eval      totchg = 0
     C*
     C     caky1         setll     pca01
     C     caky1         reade(N)  pca01                                  99
     C*
     C                   dow       *in99 = *off
     C*
     C                   eval      kkdseq = cadseq
     C*
     C     caky#         chain(n)  pdu01
     C                   if        %found
     C                   if        durvcd = 274 or durvcd = 275 or
     C                             durvcd = 276 or durvcd = 278 or
     C                             durvcd = 540 or durvcd = 545 or
     C                             durvcd = 624 or durvcd = 636
     C                   eval      ##psth = ##psth + cachrg
     C                   endif
     C*** Beg Add *** 01/13/14 *************************************************
     C                   if        durvcd = 278
     C                   eval      ##implant = ##implant + cachrg
     C                   endif
     C*** End Add *** 01/13/14 *************************************************
     C                   eval      totchg = totchg + cachrg
     C                   endif
     C*
     C     caky1         reade(N)  pca01                                  99
     C*
     C                   enddo
     C*
     C                   endsr
