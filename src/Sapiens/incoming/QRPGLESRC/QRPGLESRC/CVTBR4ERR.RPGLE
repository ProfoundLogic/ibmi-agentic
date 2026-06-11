     H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2006
     H/TITLE CVTBR4ERR
     H**************************************************************************
     H*  CVTBR4ERR  : First Health Claims Ack - Cvt Flat BR4 to WZBRP
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
     FWZC4L1    UF A E           K DISK
     FWZBRP     IF   E           K DISK

     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D kkcode          s                   like(brcode)

     DPSDS           ESDS                  Extname(SZQ1P)
     D*
     C     c4ky          klist
     C                   kfld                    kkcode
     C*
     C*----------------------------------------------------------------------
     C*  Read through flat file
     C*----------------------------------------------------------------------
     C*
     C                   read      pbr01
     C                   dow       not %eof(wzbrp)
     C*
     C                   eval      kkcode = brcode
     C*
     C     c4ky          chain     pc401
     C                   exsr      write_wzc4l1
     C*
     C                   read      pbr01
     C                   enddo
     C*
     C                   move      *on           *inlr
     C*=====================================================================
     C* Write_WZC4L1 - Write to WZC4L1
     C*=====================================================================
     C     write_wzc4l1  begsr
     C*
     C                   eval      c4code = brcode
     C                   eval      c4fiat = brfiat
     C                   eval      c4desc = brdesc
     C*
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C*
     C                   eval      c4user=q1user
     C                   eval      c4pgm=q1pgm
     C                   move      sys_time      c4time
     C     *cymd         move      sys_date      c4dt
     C*
     C                   if        not %found(wzc4l1)
     C*
     C                   eval      c4stat = 'A'
     C*
     C                   eval      c4iusr=q1user
     C                   eval      c4ipgm=q1pgm
     C                   move      sys_time      c4itim
     C     *cymd         move      sys_date      c4idt
     C*
     C                   write     pc401
     C*
     C                   else
     C*
     C                   update    pc401
     C*
     C                   endif
     C*
     C                   endsr
