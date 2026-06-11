     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*****************************************************************
     H* This program will update entire transaction file based on key *
     H* dates on the bill for the particular claimant .               *
     H*****************************************************************
     H*  Created by:                                                  *
     H*  12/14/11  C:251 Project: 114MBB                              *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     Ffaeop     if   e             disk
     Ffdcal26   uf   e           k disk
     Ffatfl1    uf   e           k disk
     Ffatfp     if a e             disk    rename(ptf01:ptf02)
     Ffmcml1    uf   e           k disk
     F*
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
     Dkkeob#           s                   like(caeob#)
     D*
     Dkkbgdt           s                   like(tfbgdt)
     DxxBegDt          s                   like(tfbgdt)
     DxxEndDt          s                   like(tfbgdt)
     DxxstDt           s                   like(tfbgdt)
     Dxxflg            s              1    inz('N')
     Dxxfnd            s              1
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D*
     C     tfky          klist
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkbgdt
     C*
     C     tfky1         klist
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C*
     C     caky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C*
     C     cmky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     *entry        plist
     C                   parm                    @@rrn             9 0
     C*
     C*** Chain on FAEOP
     C*
     C     @@rrn         chain     peo01
     C*
     C                   if        %found
     C                   eval      kkco#  = eoco#
     C                   eval      kksub# = eosub#
     C                   eval      kkfnd  = eofnd
     C                   eval      kkfyr  = eofyr
     C                   eval      kkcase = eocase
     C                   eval      kkeob# = eoeob#
     C                   else
     C                   return
     C                   endif
     C*
     C* Read FDCAP records for this eob
     C*
     C     caky          setll     pca01
     C     caky          reade     pca01
     C                   dow       not %eof (fdcal26)
     C*
     C                   clear                   ptf01
     C                   eval      xxfnd = *blanks
     C*
     C     tfky1         setll     ptf01
     C     tfky1         reade     ptf01
     C*
     C                   dow       not %eof (fatfl1)
     C                   if        cafdat<= tfendt and cafdat >= tfbgdt
     C                   eval      xxfnd = 'Y'
     C                   leave
     C                   endif
     C*
     C     tfky1         reade     ptf01
     C                   enddo
     C* If Bucket record exists
     C                   if        xxfnd = 'Y'
     C                   exsr      UpdTrnF
     C*
     C* Else create a new bucket records
     C                   else
     C                   exsr      CrtNewB
     C                   endif
     C*
     C     caky          reade     pca01
     C                   enddo
     C*
     C                   seton                                        lr
     C                   return
      *****************************************************************************
      *   UpdTrnF - Update transaction file
      *****************************************************************************
     C     UpdTrnF       Begsr
     C*
     C                   select
     C*
     C                   when      caflag <> 'P' and capybk = *zeros
     C                   eval      tfanpy = tfanpy + capaid
     C                   eval      capybk = capaid
     C                   update    pca01
     C*
     C     cmky          chain     pcm01
     C                   if        %found
     C                   eval      cmlfpy = cmlfpy + capaid
     C                   update    pcm01
     C                   endif
     C*
     C                   when      caflag <> 'P' and capybk <> *zeros
     C                   eval      tfanpy = capaid - tfanpy
     C                   eval      capybk = capaid
     C                   update    pca01
     C*
     C     cmky          chain     pcm01
     C                   if        %found
     C                   eval      cmlfpy = capaid - cmlfpy
     C                   update    pcm01
     C                   endif
     C*
     C                   when      caflag = 'P' and capybk <> *zeros
     C                   eval      tfanpy = tfanpy - capybk
     C                   eval      capybk = 0
     C                   update    pca01
     C*
     C     cmky          chain     pcm01
     C                   if        %found
     C                   eval      cmlfpy = cmlfpy - capybk
     C                   update    pcm01
     C                   endif
     C*
     C                   endsl
     C*
     C                   Endsr
      *****************************************************************************
      *   CrtNewB - Create new bucket
      *****************************************************************************
     C     CrtNewB       Begsr
     C*
     C                   eval      xxflg = 'N'
     C*
     C     tfky          setll     ptf01
     C     tfky1         reade     ptf01
     C                   eval      xxstdt = tfendt
     C*
     C                   dow       xxflg = 'N'
     C** This date will serve as beginning date for the next bucket
     C                   eval      xxBegDt = xxstdt
     C*
     C** Add one year to above date to get the end date for bucket
     C                   eval      xxEndDt = xxBegDt + 10000
     C*
     C** Do further processing only if the service date lies between this bucket
     C*
     C                   if        (cafdat >= xxBegDt ) and (cafdat <= xxEndDt)
     C                   eval      xxflg = 'Y'
     C                   leave
     C                   else
     C                   eval      tfbgdt = xxbegdt
     C                   eval      tfendt = xxenddt
     C                   eval      tfstat = 'A'
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         tfdt
     C                   movel     dstime        tftime
     C*
     C                   eval      tfpgm = q1pgm
     C                   eval      tfuser = q1user
     C*
     C                   eval      tfidt  = tfdt
     C                   eval      tfiusr = tfuser
     C                   eval      tfipgm = tfpgm
     C                   eval      tfitim = tftime
     C*
     C                   write     ptf02
     C                   endif
     C*
     C                   eval      xxstdt = xxenddt
     C                   enddo
     C*
     C                   if        caflag <> 'P'
     C                   eval      tfanpy = tfanpy + capaid
     C                   eval      tfbgdt = xxbegdt
     C                   eval      tfendt = xxenddt
     C                   eval      tfstat = 'A'
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         tfdt
     C                   movel     dstime        tftime
     C*
     C                   eval      tfpgm = q1pgm
     C                   eval      tfuser = q1user
     C*
     C                   eval      tfidt  = tfdt
     C                   eval      tfiusr = tfuser
     C                   eval      tfipgm = tfpgm
     C                   eval      tfitim = tftime
     C*
     C                   write     ptf02
     C*
     C                   eval      capybk = capaid
     C                   update    pca01
     C*
     C     cmky          chain     pcm01
     C                   if        %found
     C                   eval      cmlfpy = cmlfpy + capaid
     C                   update    pcm01
     C                   endif
     C*
     C                   endif
     C*
     C                   Endsr
