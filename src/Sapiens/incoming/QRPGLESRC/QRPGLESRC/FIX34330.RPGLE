     H* (C) Copyright Progressive Data Solutions, 2002
     H/TITLE FIX34330                                                                       *
     H**************************************************************************************
     H*  Description:  Populate professional component for Supervision codes               *
     H*                                                                                    *
     H*  Created: 10/29/02   Programmer C:79    Support:MC044 34330                        *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     FFMPRPFL   UF   E             DISK

     D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D*
     D xxdesc          s            100A
     D zzdesc          s            100A
     D LO              s             26A   inz('abcdefghijklmnopqrstuvwxyz')
     D HI              s             26A   inz('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
     D xxspv           s             11A   inz('SUPERVISION')
     D*
     DPSDS           ESDS                  Extname(SZQ1P)
     C*
     C                   read      ppr01                                  99
     C*
     C                   dow       *in99 = *off
     C*
     C                   exsr      scandsc
     C*
     C                   read      ppr01                                  99
     C*
     C                   enddo
     C*
     C                   move      *on           *inlr
     C*
     C                   Return
     C*=====================================================================
     C*    SUBROUTINE SCANDSC
     C*=====================================================================
     C     scandsc       begsr
     C*
     C                   if        prcpt4 >= '70000' and prcpt4 <= '79999'
     C                             and prprof = 0 and pr92n2 = 0
     C*
     C                   eval      xxdesc = (prdsc1+prdsc2)
     C     LO:HI         XLATE     xxdesc        zzdesc
     C     xxspv         SCAN      zzdesc                                 99
     C*
     C                   if        *in99 = *on
     C*
     C                   eval      prprof = prmra
     C                   exsr      update
     C*
     C                   endif
     C*
     C                   endif
     C*
     C                   endsr
     C*
     C*=====================================================================
     C*    SUBROUTINE UPDATE
     C*=====================================================================
     C     update        begsr
     C*
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C     *cymd         move      sys_date      prdt
     C                   eval      prpgm=q1pgm
     C                   eval      pruser=q1user
     C                   move      sys_time      prtime
     C*
     C                   update    ppr01
     C*
     C                   endsr
     C*
