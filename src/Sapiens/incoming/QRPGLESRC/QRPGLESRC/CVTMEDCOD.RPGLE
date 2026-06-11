     H copyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/TITLE CVTMEDCOD
     H**************************************************************************
     H*  Description:  Diagnosis Code Conversion Program
     H*
     H*  Created: 06/02/09   Programmer C:227   Project:71MKD
     H**************************************************************************
     H*  Revisions:
     H*
     H**************************************************************************
     FFMDIL2    UF A E           K DISK
     FF1DIP     IF   E             DISK

     D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D kkds1           s                   like(dids1)
     D @@icd9          s                   like(diicd9)
     D*

     DPSDS           ESDS                  Extname(SZQ1P)

     C     diky          klist
     C                   kfld                    kkds1
     C*----------------------------------------------------------------------
     C*  Read through NYDME
     C*----------------------------------------------------------------------
     C*
     C                   read      pdip1                                  99
     C                   dow       (*in99 = *off)
     C*
     C                   exsr      initz
     C                   eval      kkds1 = dicode
     C*
     C     diky          chain     pdi01                              98
     C                   if        *in98=*on
     C                   exsr      writer
     C                   else
     C                   exsr      updatr
     C                   endif
     C*
     C                   read      pdip1                                  99
     C                   enddo
     C*
     C                   seton                                        lr
     C                   Return
     C**********************************************************************
     C* INITZ - INITIALIZATION
     C**********************************************************************
     C     initz         begsr
     C*
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C                   endsr
     C**********************************************************************
     C* WRITER - WRITES FMDIP
     C**********************************************************************
     C     writer        begsr
     C*
     C                   clear                   pdi01
     C*
     C     '.'           scan      dicode        pos               2 0
     C                   if        pos > 0
     C                   eval      @@icd9 = %subst(dicode:1:pos-1)
     C                   eval      @@icd9 = %trim(@@icd9) + %subst(dicode:pos+1)
     C                   movel     @@icd9        diicd9
     C                   else
     C                   movel     dicode        diicd9
     C                   endif
     C*
     C                   eval      didsc1 = didesc
     C*
     C                   eval      distat = 'A'
     C*
     C                   eval      dids1 = dicode
     C*
     C     *cymd         move      sys_date      didt
     C                   move      didt          diidt
     C                   eval      dipgm=q1pgm
     C                   eval      diiusr=q1user
     C                   eval      diuser=q1user
     C                   move      sys_time      ditime
     C                   eval      dilost = 'D'
     C
     C                   write     pdi01
     C*
     C     wrte          endsr
     C**********************************************************************
     C* UPDATR - UPDATES FMDIP
     C**********************************************************************
     C     updatr        begsr
     C*
     C     '.'           scan      dicode        pos               2 0
     C                   if        pos > 0
     C                   eval      @@icd9 = %subst(dicode:1:pos-1)
     C                   eval      @@icd9 = %trim(@@icd9) + %subst(dicode:pos+1)
     C                   movel     @@icd9        diicd9
     C                   else
     C                   movel     dicode        diicd9
     C                   endif
     C*
     C                   eval      didsc1 = didesc
     C                   eval      didsc2 = *blanks
     C*
     C                   eval      distat = 'A'
     C                   eval      dilost = 'D'
     C*
     C     *cymd         move      sys_date      didt
     C                   eval      dipgm = q1pgm
     C                   eval      diuser = q1user
     C                   move      sys_time      ditime
     C*
     C                   eval      digrp = 0
     C                   eval      difl1 = *blanks
     C                   eval      difl2 = *blanks
     C                   eval      difl3 = *blanks
     C                   eval      difl4 = *blanks
     C                   eval      difl5 = *blanks
     C                   eval      difl6 = *blanks
     C                   eval      difl7 = *blanks
     C                   eval      difl8 = *blanks
     C                   eval      difl9 = *blanks
     C                   eval      difl10 = *blanks
     C                   eval      dids2 = *blanks
     C                   eval      dids3 = *blanks
     C                   eval      dids4 = *blanks
     C                   eval      dids5 = *blanks
     C                   eval      dilgrp = *blanks
     C                   update    pdi01
     C*
     C     updr          endsr
