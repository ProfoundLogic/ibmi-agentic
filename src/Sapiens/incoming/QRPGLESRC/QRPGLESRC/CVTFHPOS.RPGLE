     H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2006
     H/TITLE CVTFHPOS
     H**************************************************************************
     H*  CVTFHPOS   : First Health EOR Financial Place of Service Conversion Pro
     H*
     H*  Description:
     H*
     H*  Called from:
     H*  Calls to   :
     H*
     H**************************************************************************
     H*  Created on 05/24/2006 by C:163 for Project 139MCV from client 139
     H**************************************************************************
     H*  Revisions:
     H*
     H**************************************************************************
     FFTPXL1    UF A E           K DISK
     FFTP1P     IF   E           K DISK

     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D kkpos           s                   like(p1pos)

     DPSDS           ESDS                  Extname(SZQ1P)
     D*
     C     pxky          klist
     C                   kfld                    kkpos
     C*
     C*----------------------------------------------------------------------
     C*  Read through flat file
     C*----------------------------------------------------------------------
     C*
     C                   read      pp101
     C                   dow       not %eof(ftp1p)
     C*
     C                   eval      kkpos  = p1pos
     C*
     C     pxky          chain     ppx01
     C                   exsr      write_ftpxl1
     C*
     C                   read      pp101
     C                   enddo
     C*
     C                   move      *on           *inlr
     C*=====================================================================
     C* Write_FTPXL1 - Write to FTPXL1
     C*=====================================================================
     C     write_ftpxl1  begsr
     C*
     C                   eval      pxpos  = p1pos
     C                   eval      pxdesc = p1desc
     C                   eval      pxpxrf = p1pxrf
     C*
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C*
     C                   eval      pxuser=q1user
     C                   eval      pxpgm=q1pgm
     C                   move      sys_time      pxtime
     C     *cymd         move      sys_date      pxdt
     C*
     C                   if        not %found(ftpxl1)
     C*
     C                   eval      pxiusr=q1user
     C                   eval      pxipgm=q1pgm
     C                   move      sys_time      pxitim
     C     *cymd         move      sys_date      pxidt
     C*
     C                   write     ppx01
     C*
     C                   else
     C*
     C                   update    ppx01
     C*
     C                   endif
     C*
     C                   endsr
