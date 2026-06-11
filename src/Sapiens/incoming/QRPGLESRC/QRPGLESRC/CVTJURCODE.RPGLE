     H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2006
     H/TITLE CVTJURCODE
     H**************************************************************************
     H*  CVTJURCODE : First Health EOR Financial Jurisdiction Area Conversion Pr
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
     FFTJCL1    UF A E           K DISK
     FFTJ1P     IF   E           K DISK

     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D kkcode          s                   like(j1code)

     DPSDS           ESDS                  Extname(SZQ1P)
     D*
     C     jcky          klist
     C                   kfld                    kkcode
     C*
     C*----------------------------------------------------------------------
     C*  Read through flat file
     C*----------------------------------------------------------------------
     C*
     C                   read      pj101
     C                   dow       not %eof(ftj1p)
     C*
     C                   eval      kkcode = j1code
     C*
     C     jcky          chain     pjc01
     C                   exsr      write_ftjcl1
     C*
     C                   read      pj101
     C                   enddo
     C*
     C                   move      *on           *inlr
     C*=====================================================================
     C* Write_FTJCL1 - Write to FTJCL1
     C*=====================================================================
     C     write_ftjcl1  begsr
     C*
     C                   eval      jccode = j1code
     C                   eval      jcst   = j1st
     C                   eval      jcdesc = j1desc
     C*
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C*
     C                   eval      jcuser=q1user
     C                   eval      jcpgm=q1pgm
     C                   move      sys_time      jctime
     C     *cymd         move      sys_date      jcdt
     C*
     C                   if        not %found(ftjcl1)
     C*
     C                   eval      jcstat = 'A'
     C*
     C                   eval      jciusr=q1user
     C                   eval      jcipgm=q1pgm
     C                   move      sys_time      jcitim
     C     *cymd         move      sys_date      jcidt
     C*
     C                   write     pjc01
     C*
     C                   else
     C*
     C                   update    pjc01
     C*
     C                   endif
     C*
     C                   endsr
