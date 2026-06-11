     H* (C) Copyright Progressive Data Solutions, 2003
     H/TITLE ADDSPECIAL                                                                     *
     H**************************************************************************************
     H*  Description:  Add new specialty codes to WC specialty code table                  *
     H*                                                                                    *
     H*  Created: 05/21/03   Programmer C:79    Project:129MBO                             *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     FWTABL     UF A E           K DISK    USROPN
     FFTSPL1    UF A E           K DISK    RENAME(PSP01:PSP02)
     FSMCOL     IF   E           K DISK

     D spcd            s              3    dim(11) ctdata perrcd(1)
     D spcdd           s             30    dim(11) alt(spcd)
     D n               s              2  0

     D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0

     DPSDS           ESDS                  Extname(SZQ1P)

     C     *like         define    spco#         kkco#
     C     *like         define    spkey         kkkey
     C*
     C     coky          klist
     C                   kfld                    kkco#
     C*
     C     spky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkkey

     C*
     C*----------------------------------------------------------------------
     C*  Cycle through New table items
     C*----------------------------------------------------------------------
     C                   call      'SA001R'
     C                   parm                    ##wcif            1
     C                   parm                    ##mcif            1
     C                   parm                    ##wcin            1
     C*
     C                   if        ##wcin = 'Y'
     C                   open      WTABL
     C                   endif
     C*
     C                   eval      n=1
     C*
     C                   dow       n <= 11
     C*
     C                   eval      kkkey = spcd(n)
     C*
     C* Cycle through all company numbers
     C*
     C                   eval      kkco# = 0
     C     coky          setgt     pco01
     C                   read      pco01                                  99
     C                   dow       *in99 = *off
     C*
     C                   eval      kkco# = coco#
     C*
     C* Managed Care
     C*
     C     spky          chain     psp02                              99
     C*
     C                   if        *in99 = *on
     C                   exsr      mvscdb
     C                   write     psp02
     C                   endif
     C*
     C* Workers Comp
     C*
     C                   if        ##wcin = 'Y'
     C     spky          chain     psp01                              99
     C*
     C                   if        *in99 = *on
     C                   exsr      mvscdb
     C                   write     psp01
     C                   endif
     C*
     C                   endif
     C*
     C                   read      pco01                                  99
     C*
     C                   enddo                                                  *in60=*off
     C*
     C                   eval      n = n + 1
     C*
     C                   enddo                                                  *in60=*off
     C*
     C                   eval      *inlr=*on
     C*
     C                   Return
     C*
     C     mvscdb        begsr
     C*
     C                   eval      spco#  = coco#
     C                   eval      spkey  = spcd(n)
     C                   eval      spdesc = spcdd(n)
     C                   eval      spstat = 'A'
     C                   eval      spuser = q1user
     C*
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C     *cymd         move      sys_date      spdt
     C*
     C                   move      sys_time      sptime
     C                   eval      sppgm = q1pgm
     C*
     C                   endsr
** spcd /spcdd
CDTCARDIAC DIAGNOSTIC TESTING
EMSEMS
ENTEAR, NOSE & THROAT
HRHHOSPITAL - REHABILITATION
OR2ORT: SURGERY
OR3ORT: JOINTS
OR4ORT: SPINE
OR5ORT: BACK
OR6ORT: FOOT
PO1POD: SURGERY
WELWELLNESS CENTER
