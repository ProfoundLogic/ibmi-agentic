     H* (C) Copyright Progressive Data Solutions, 2002
     H/TITLE FIX29552                                                                       *
     H**************************************************************************************
     H*  Description:  Populate Message Description in FDMGP                               *
     H*                                                                                    *
     H*  Created: 04/24/02   Programmer C:79    Project:FSMNT                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     FFdmgp     UF   E           K DISK
     FFtmil     UF   E           K DISK
     D*
     D kkmseq          s                   like(mimseq) inz(0)
     D kkmsg#          s                   like(mimsg#)
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0

     DPSDS           ESDS                  Extname(SZQ1P)
    C*
     C     miky          klist
     C                   kfld                    kkmsg#
     C                   kfld                    kkmseq
     C*
     C*----------------------------------------------------------------------
     C*  Read through FDMGP
     C*----------------------------------------------------------------------
     C                   read      pmg01                                  99
     C*
     C                   dow       (*in99 = *off)
     C*
     C* Get the message description from FTMIP if blank
     C*
     C                   if        mgdesc = *blanks
     C*
     C                   eval      kkmsg# = mgmsg#
     C*
     C     miky          chain     pmi01                              99
     C  n99              eval      mgdesc = midesc
     C                   exsr      updfdmgp
     C*
     C                   endif
     C*
     C                   read      pmg01                                  99
     C*
     C                   enddo
     C*
     C                   eval      *inlr=*on
     C*
     C     updfdmgp      begsr
     C*
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C     *cymd         move      sys_date      mgdt
     C                   eval      mgpgm=q1pgm
     C                   eval      mguser=q1user
     C                   move      sys_time      mgtime
     C*
     C                   update    pmg01
     C*
     C                   endsr
