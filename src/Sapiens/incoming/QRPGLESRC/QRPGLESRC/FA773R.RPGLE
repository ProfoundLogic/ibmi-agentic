     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*****************************************************************
     H* This program will be used to verify whether the lifetime or   *
     H* annual payment limit has been reached.                        *
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
     Ffaeop     if   e             disk
     Ffdcalh    if   e           k disk
     Ffatfl1    if   e           k disk
     Ffmfnl     if   e           k disk
     Ffmcml1    if   e           k disk
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
     Dxxmpln           s                   like(fnmpln)
     D*
     C*
     C     tfky          klist
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
     C     *entry        plist
     C                   parm                    @@rrn             9 0
     C                   parm                    @@rtcd            1
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
     C*** Read on fmcml for lifetime payment limit
     C*
     C     cmky          chain     pcm01
     C                   if        %found
     C*
     C* Check Lifetime payment amount
     C*
     C                   if        cmlfpy = 1000000
     C                   eval      @@rtcd = 'Y'
     C                   return
     C                   endif
     C                   endif
     C*
     C*** Read on FATFP for annual payment limit
     C*
     C* Get limit from fund master file
     C*
     C                   eval      xxmpln = *zeros
     C     fnky          chain     pfn01
     C                   if        %found
     C                   eval      xxmpln = fnmpln
     C                   endif
     C*
     C* Read FDCAP records to find which record lies in bucket file history
     C*
     C     caky          setll     pca01
     C     caky          reade     pca01
     C                   dow       not %eof
     C*
     C                   if        caflag <> 'P' and capybk <> *zeros
     C*
     C                   clear                   ptf01
     C     tfky          setll     ptf01
     C     tfky          reade     ptf01
     C*
     C                   dow       not %eof
     C                   if         cafdat >= tfbgdt and cafdat <= tfendt
     C*
     C* Check Lifetime payment amount
     C*
     C                   if        tfanpy > xxmpln
     C                   eval      @@rtcd = 'Y'
     C                   return
     C                   endif
     C*
     C                   endif                                                  CAFDAT in bucket
     C*
     C     tfky          reade     ptf01
     C                   enddo                                                  record in FATFP
     C*
     C                   endif                                                  caflag <> 'P'
     C*
     C     caky          reade     pca01
     C                   enddo
     C*
     C                   seton                                        lr
     C                   return
