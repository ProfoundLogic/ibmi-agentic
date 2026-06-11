     H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2006
     H/TITLE CVTFHBRCD
     H**************************************************************************
     H*  CVTFHBRCD  : First Health EOR Financial Provider Specialty List Convers
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
     FFTBRL1    UF A E           K DISK
     FFTB1P     IF   E           K DISK

     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D kkcode          s                   like(b1code)

     DPSDS           ESDS                  Extname(SZQ1P)
     D*
     C     brky          klist
     C                   kfld                    kkcode
     C*
     C*----------------------------------------------------------------------
     C*  Read through flat file
     C*----------------------------------------------------------------------
     C*
     C                   read      pb101
     C                   dow       not %eof(ftb1p)
     C*
     C                   eval      kkcode = b1code
     C*
     C     brky          chain     pbr01
     C                   exsr      write_ftbrl1
     C*
     C                   read      pb101
     C                   enddo
     C*
     C                   move      *on           *inlr
     C*=====================================================================
     C* Write_FTBRL1 - Write to FTBRL1
     C*=====================================================================
     C     write_ftbrl1  begsr
     C*
     C                   eval      brcode = b1code
     C                   eval      brdesc = b1desc
     C*
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C*
     C                   eval      bruser=q1user
     C                   eval      brpgm=q1pgm
     C                   move      sys_time      brtime
     C     *cymd         move      sys_date      brdt
     C*
     C                   if        not %found(ftbrl1)
     C*
     C                   eval      briusr=q1user
     C                   eval      bripgm=q1pgm
     C                   move      sys_time      britim
     C     *cymd         move      sys_date      bridt
     C*
     C                   write     pbr01
     C*
     C                   else
     C*
     C                   update    pbr01
     C*
     C                   endif
     C*
     C                   endsr
