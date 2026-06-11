     H* (C) Copyright Progressive Data Solutions, 2003
     H/TITLE UPDBEAST                                                                       *
     H**************************************************************************************
     H*  Description:  Update FMBEP with new values                                        *
     H*                                                                                    *
     H*  Created: 05/02/03   Programmer C:79    Project:FL043                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     FFMBEL     UF   E           K DISK

     D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D kkast           s                   like(beast) inz('FL')
     D kksys           s                   like(besys) inz('P')
     D kkbtyp          s                   like(bebtyp) inz('D')

     DPSDS           ESDS                  Extname(SZQ1P)

     C     beky          klist
     C                   kfld                    kkast
     C                   kfld                    kksys
     C                   kfld                    kkbtyp
     C*
     C     beky          chain     pbe01                              99
     C*
     C                   if        *in99 = *off
     C*
     C                   exsr      update
     C*
     C                   endif
     C*
     C                   move      *on           *inlr
     C*
     C                   Return
     C*=====================================================================
     C*    SUBROUTINE UPDATE
     C*=====================================================================
     C     update        begsr
     C*
     C* Load values
     C*
     C                   eval      behepg = 'F0381RFL'
     C                   eval      beedhd = 'F0481RFL'
     C                   eval      beedi  = 'Y'
     C*
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C     *cymd         move      sys_date      bedt
     C                   eval      bepgm=q1pgm
     C                   eval      beuser=q1user
     C                   move      sys_time      betime
     C*
     C                   update    pbe01
     C*
     C                   endsr
     C*
