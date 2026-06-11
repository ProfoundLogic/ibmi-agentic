     H* (C) Copyright Progressive Data Solutions, 2003
     H/TITLE FIXEOBSRC                                                                      *
     H**************************************************************************************
     H*  Description:  Populate EOB source with 'C' for Correct Coding                     *
     H*                                                                                    *
     H*  Created: 11/08/03   Programmer C:79    Project:FSMNT                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     FFDDEP     UF   E           K DISK

     D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0

     DPSDS           ESDS                  Extname(SZQ1P)

     C                   exsr      pgmstr
     C*
     C                   if        ##coco = 'Y'
     C*
     C*----------------------------------------------------------------------
     C*  Read through FDDEP file
     C*----------------------------------------------------------------------
     C                   read      pde01                                  99
     C*
     C                   dow       (*in99 = *off)
     C*
     C                   if        deeob >= 'C00' and deeob <= 'C04'
     C                   exsr      update
     C                   endif
     C*
     C                   read      pde01                                  99
     C*
     C                   enddo
     C*
     C                   endif
     C*
     C                   eval      *inlr=*on
     C*
     C                   Return
     C*
     C**********************************************************************
     C     pgmstr        begsr
     C**********************************************************************
     C*
     C                   call      'SA001R'
 1   C                   parm      *blank        ##wcif            1
 2   C                   parm      *blank        ##mcif            1
 3   C                   parm      *blank        ##wcin            1
 4   C                   parm      *blank        ##ar              1
 5   C                   parm      *blank        ##ntwk            1
 6   C                   parm      *blank        ##ur              1
 7   C                   parm      *blank        ##med             1
 8   C                   parm      *blank        ##edi             1
 9   C                   parm      *blank        ##wcdk            1
10   C                   parm      *blank        ##rhab            1
11   C                   parm      *blank        ##mss             1
12   C                   parm      *blank        ##fsch            1
12   C                   parm      *blank        ##prvb            1
12   C                   parm      *blank        ##npwc            1
12   C                   parm      *blank        ##os              1
12   C                   parm      *blank        ##arls            1
12   C                   parm      *blank        ##coco            1
     C*
     C                   endsr
     C*
     C**********************************************************************
     C     update        begsr
     C**********************************************************************
     C*
     C* Update FDDEP
     C*
     C                   eval      deesrc = 'C'
     C*
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C     *cymd         move      sys_date      dedt
     C                   eval      depgm=q1pgm
     C                   eval      deuser = q1user
     C                   move      sys_time      detime
     C*
     C                   update    pde01
     C*
     C                   endsr
