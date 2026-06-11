     H* (C) Copyright Progressive Data Solutions, 2003
     H/TITLE CVTEAPOST                                                                      *
     H**************************************************************************************
     H*  Description:  Populate Policy State.                                              *
     H**************************************************************************************
     H*  Created: 06/13/07   Programmer C:182   Project:10MKP                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     FFAEAP     UF   E             DISK
     FFMCML1    IF   E           K DISK
     F*
     D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D*
     D kkco#           s                   like(eaco#)
     D kksub#          s                   like(easub#)
     D kkfnd           s                   like(eafnd)
     D kkfyr           s                   like(eafyr)
     D kkcase          s                   like(eacase)

     DPSDS           ESDS                  Extname(SZQ1P)

     C     cmky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C*
     C                   read      pea01                                  99
     C*
     C                   dow       *in99 = *off
     C*
     C                   eval      kkco#  = eaco#
     C                   eval      kksub# = easub#
     C                   eval      kkfnd  = eafnd
     C                   eval      kkfyr  = eafyr
     C                   eval      kkcase = eacase
     C*
     C     cmky          chain     pcm01                              98
     C*
     C                   if        *in98 = *off
     C*
     C                   eval      eapost = cmpost
     C*
     C* Update DB file
     C                   exsr      update
     C*
     C                   endif
     C*
     C                   read      pea01                                  99
     C*
     C                   enddo
     C*
     C                   move      *on           *inlr
     C*
     C                   Return
     C*=====================================================================
     C*    SUBROUTINE UPDATE
     C*=====================================================================
     C     update        begsr
     C*
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C     *cymd         move      sys_date      eadt
     C                   eval      eapgm=q1pgm
     C                   eval      eauser=q1user
     C                   move      sys_time      eatime
     C*
     C                   update    pea01
     C*
     C                   endsr
     C*
