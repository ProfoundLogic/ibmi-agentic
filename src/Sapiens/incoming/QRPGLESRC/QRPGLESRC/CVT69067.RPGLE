     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*                                                               *
     H/TITLE CVT69067: Update Audit tracking records                  *
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION: Find out all the audit tracking records  *
     H*                      and inactivate all except latest one.    *
     H*                      Key: CO#,FND,FYR,EMP,DIV.                *
     H*                                                               *
     H* CREATED: 10/13/09  C:220 Support: 69067                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     FWMAUP     if   e             disk
     FWMAULCVT  uf   e           k disk    rename(pau01:pau02)
     F                                     prefix(a_)
     F*****************************************************************
     D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D*
     DPSDS           ESDS                  Extname(SZQ1P)
     C*****************************************************************
     C     auky          klist
     C                   kfld                    auco#
     C                   kfld                    aufnd
     C                   kfld                    aufyr
     C                   kfld                    auemp#
     C                   kfld                    audiv
     C
     C*****************************************************************
     C*
     C                   read      pau01
     C                   dow       not %eof(wmaup)
     C     auky          setgt     pau02
     C     auky          readpe    pau02                                  60
     C                   if        a_austat = 'A'
     C     auky          readpe    pau02                                  60
     C                   if        a_auco# = auco# and a_aufnd = aufnd and
     C                             a_aufyr = aufyr and a_auemp# = auemp#
     C                             and a_audiv = audiv and a_austat = 'A' and
     C                             *in60 = *off
     C*
     C                   movel     'I'           a_austat
     C*
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C**   *cymd         move      sys_date      a_audt
     C                   eval      a_aupgm=q1pgm
     C                   eval      a_auuser=q1user
     C                   move      sys_time      a_autime
     C*
     C                   update    pau02
     C                   endif                                                  deuplicate
     C                   endif                                                  a_austat = 'A'
     C                   read      pau01
     C                   enddo                                                  employer rcords
     C                   seton                                        lr
