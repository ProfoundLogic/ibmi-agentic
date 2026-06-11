     H* (C) Copyright StoneRiver, 2009
     H/TITLE FIXCNTRLA                                                                      *
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
     H**************************************************************************************
     H*  Description:  Assign Controlling Agent Values                                     *
     H*                                                                                    *
     H**************************************************************************************
     H*  Created on 05/27/09 by c:62 for Project 133WGW                                    *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     FWAQTL7    IF   E             DISK
     FWMAHL     UF   E           K DISK

     D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D cafl            s              1
     D dateIn          s             10
     D date            s             10
     D dateCVT         s               D   datfmt(*USA)

     DPSDS           ESDS                  Extname(SZQ1P)

     C     ahky          klist
     C                   kfld                    qtco#
     C                   kfld                    qtfnd
     C                   kfld                    qtemp#
     C                   kfld                    qtdiv
     C*
     C*----------------------------------------------------------------------
     C*  Read through flat file
     C*----------------------------------------------------------------------
     C*
     C                   read      pqt01                                  99
     C                   dow       not *in99
     C*
     c                   movel     qtcomm        cafl
     c                   if        cafl = 'Y'
     c     ahky          chain     pah01
     c                   if        %found
     c                   dou       %eof
     c*
     c                   if        ahagt#=qtagt#
     c                   eval      ahcaf1='Y'
     c                   endif
     c*
     c                   if        ahsagt=qtagt#
     c                   eval      ahcaf2='Y'
     c                   endif
     c*
     c                   if        ahagrc=qtagt#
     c                   eval      ahcrf1='Y'
     c                   endif
     c*
     c                   if        ah2arc=qtagt#
     c                   eval      ahcrf2='Y'
     c                   endif
     c*
     c                   if        ahcaf1<>' ' or ahcaf2<>' ' or
     c                             ahcrf1<>' ' or ahcrf2<>' '
     c                   eval      ahpgm=q1pgm
     c                   eval      ahuser=q1user
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C     *cymd         move      sys_date      ahdt
     C                   move      sys_time      ahtime
     c                   update    pah01
     c                   endif
     c*
     c     ahky          reade     pah01
     c                   enddo
     c                   endif
     c                   endif
     C*
     C                   read      pqt01                                  99
     C                   enddo                                                  *in60=*off
     C*
     C                   move      *on           *inlr
