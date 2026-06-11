     A* (c) 2014 StoneRiver, Inc. All rights Reserved.                     *
     A* StoneRiver is a trademark of StoneRiver, Inc.                      *
      *============================================================================================*
      /title FIXDEBIT - Fix program to move WMDBP records to WMEFP                                 *
     H option(*nodebugio: *srcstmt)                                                                *
     H debug(*yes)                                                                                 *
     H* Revisions:                                                                                 *
     H*                                                                                            *
     H* 06/23/14 c:65  Project: Redmine#4384                                                       *
     H*                Remove chain on WMEFP to allow all records to copy from old WMDBP to new    *
     H*                file, instead of only just the first direct debit record.                   *
     H*                                                                                            *
      *============================================================================================*
     fwmdbp     if   e             disk    rename(pdb01 : pdb0p)
     fwmeml     if   e           k disk    rename(pem01 : pem0l)
     fwmefl6    if a e           k disk    rename(pef01 : pef06)

     dpsds           esds                  extname(szq1p)
     dwrkdate          s               d
     dcymddate         s              7  0

     c     key           klist
     c                   kfld                    dbco#
     c                   kfld                    dbfnd
     c                   kfld                    dbemp#
     c                   kfld                    dbdiv

     c     *mdy          move      q1dt          wrkdate
     c     *cymd         move      wrkdate       cymddate
     c
     c                   read      pdb0p
     c                   dow       not(%eof)
     C*** begin del ***  06/23/14  *********************************************
     c*****key           chain     pef06
     c********           if        not(%found)
     C*** end del *****  06/23/14  *********************************************
     c     key           chain     pem0l
     c                   if        %found
     c                   clear                   pef06
     c                   eval      efco#  = dbco#
     c                   eval      effnd  = dbfnd
     c                   eval      efemp# = dbemp#
     c                   eval      efdiv  = dbdiv
     c                   eval      efactp = dbcsfl
     c                   movel(p)  dbdfia        efacct
     c                   movel(p)  dbtrcd        efdgit
     c                   movel(p)  dbdfii        efid#
     c                   eval      efpdt  = dbpred
     c                   eval      efadt  = dbpred
     c                   eval      efftdt = db1std
     c                   eval      efltdt = dblstd
     c                   eval      efstat = dbstat
     c                   eval      efcxdt = dbcxdt
     c                   eval      efefdt = dbefdt
     c                   eval      efexdt = dbexdt
     c                   eval      effein = emfein
     c                   eval      efmod  = 0
     c                   eval      eftype = 'E'
     c                   eval      efpnam = emname
     c                   eval      efuser = q1user
     c                   eval      efpgm  = q1pgm
     c                   eval      eftime = q1time
     c                   eval      efdt   = cymddate
     c                   eval      efiusr = q1user
     c                   eval      efipgm = q1pgm
     c                   eval      efitm  = q1time
     c                   eval      efidt  = cymddate
     C                   movel(p)  dbemp#        xxwk14           14
     C                   move      dbdiv         xxwk14
     C                   movel(p)  xxwk14        efkey
     c                   write     pef06
     c                   endif
     C*** begin del ***  06/23/14  *********************************************
     c*********          endif
     C*** end del *****  06/23/14  *********************************************
     c                   read      pdb0p
     c                   enddo
     c                   seton                                        lr
