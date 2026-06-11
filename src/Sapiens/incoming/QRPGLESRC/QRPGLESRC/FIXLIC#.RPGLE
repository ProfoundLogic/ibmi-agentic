     H**********************************************************************
     H* (C)COPYRIGHT PROGRESSIVE DATA SOLUTIONS INC., 2004
    H**********************************************************************
    H* PROGRAM: FIXLIC#
    H* DESCRIPTION: UPDATE LICENSE NUMBER FIELDS
     H**********************************************************************
     H* CREATED: 09/29/04 C: 079 PROJECT: TX024
     H**********************************************************************
     H* REVISIONS:
     H**********************************************************************
     F*
     FFAEHL     UF   E             DISK
     FFAEUL     UF   E             DISK
     F*
     D*
     D psds          esds                  extname(szq1p)
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*
     C*
     C*=====================================================================
     C* MAIN PROGRAM
     C*=====================================================================
     C*
     C*
     C                   read      peh01                                  99
     C*
     C                   dow       *in99 = *off
     C*
     C                   if        ehflic = *blanks
     C                   exsr      updfaehp
     C                   endif
     C*
     C                   read      peh01                                  99
     C*
     C                   enddo
     C*
     C                   read      peu01                                  99
     C*
     C                   dow       *in99 = *off
     C*
     C                   if        euflic = *blanks
     C                   exsr      updfaeup
     C                   endif
     C*
     C                   read      peu01                                  99
     C*
     C                   enddo
     C*
     C                   eval      *inlr = *on
     C                   return
     C*
     C*=====================================================================
     C* SUBROUTINE updfaehp
     C*=====================================================================
     C*
     C     updfaehp      begsr
     C*
     C*
     C                   eval      ehflic = ehlic#
     C                   eval      ehuser = q1user
     C                   eval      ehpgm  = q1pgm
     C*
     C                   time                    sys_time_n
     C                   move      *date         sys_date
     C     *hms          move      sys_time_n    sys_time
     C                   move      sys_time      ehtime
     C     *cymd         move      sys_date      ehdt
     C*
     C                   update    peh01
     C*
     C                   endsr
     C*=====================================================================
     C* SUBROUTINE updfaeup
     C*=====================================================================
     C*
     C     updfaeup      begsr
     C*
     C                   eval      euflic = eulic#
     C                   eval      euuser = q1user
     C                   eval      eupgm  = q1pgm
     C*
     C                   time                    sys_time_n
     C                   move      *date         sys_date
     C     *hms          move      sys_time_n    sys_time
     C                   move      sys_time      eutime
     C     *cymd         move      sys_date      eudt
     C*
     C                   update    peu01
     C*
     C                   endsr
     C
