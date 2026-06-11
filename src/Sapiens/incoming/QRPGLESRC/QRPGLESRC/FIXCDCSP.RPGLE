    H*================================================================
     H*  (C) Copyright FISERV Insurance Solutions, Inc., 2005
    H*
    H*  FIXCDSCP - Add missing P&C Claims History Records
     H*
     H*  CREATED: 08/15/05  CONSULTANT: C:132  PROJECT: wa051 - 47818
    H*
    H*================================================================
     H* REVISIONS:                                                    *
     H*                                                               *
    H*================================================================
    F*
    F*  P&C Claims record
     FCMCMP     If   e             disk
    F*
    F*  P&C Claims History file
     FCDCSL     uf a e           K disk
     f*
     f* Audit Report
     fqsysprt   o    f  132        printer oflind(*inob) usropn
      *
     d                 ds
     d  dsdttm                 1     14  0
     d  dstime                 1      6  0
     d  dsdate                 7     14  0
     d bycnt           s              7  0 inz
     d incnt           s              7  0 inz
     d nucnt           s              7  0 inz
     d upcnt           s              7  0 inz
     d*
     d prject          s              6
     d prnt            s              1
     d updte           s              1n
      *
     d fids01        e ds                  extname(szq2p)
     d psds          esds                  extname(szq1p)
     d*
      *
     c                   read      pcm01                                  60
      *
     c                   dow       (not *in60)
      *
     c                   eval      incnt = incnt + 1
      *
     c     csky          chain     pcs01                              61
     c*
     c                   If        *in61
     c                   z-add     cmco#         csco#
     c                   eval      csclm# = cmclm#
     c                   if        cmrpdt <> *zeros
     c                   z-add     cmrpdt        csefdt
     c                   else
     c                   z-add     cmlsdt        csefdt
     c                   endif
     c                   eval      csocst = 'O'
     c                   eval      csstat = ' '
     c                   eval      csexpl = 'ORIGINAL OPEN OF CLAIM'
     c                   movel     csexpl        xxexpl           30
     c                   eval      csuser = cmuser
     c                   eval      cspgm  = 'FIXCDCSP'
     c                   eval      cstime = cmtime
     c                   eval      csdt   = cmdt
     c                   If        updte  = *on
     c                   write     PCS01
     c                   eval      upcnt = upcnt + 1
     c                   endif
     c                   if        prnt = 'Y' and *IN91 = *on
     c   ob              except    head
     c                   except    det
     c                   endif                                                  End PRNT = 'Y'
      *
     c                   else
     c                   eval      bycnt = bycnt + 1
      *
     c                   endif
      *
     c                   read      PCM01                                  60
     c                   enddo                                                  *IN60=*OFF
      *
     c                   if        prnt  = 'Y'
      *
     c                   if        updte = *off
     c                   z-add     nucnt         upcnt
     c                   endif
      *
     c   ob              except    head
     c                   except    totl
     c                   close     qsysprt
     c                   endif                                                  END prt = Y
      *
     c                   move      *on           *INLR
      *
      * =========================================================== *
     c     *INZSR        begsr
      * =========================================================== *
     c*
     c* Open printer file if wanted. Use *IN91 to print detail lines
     c* If *IN91 = *OFF then only totals print
     c                   eval      prnt  = 'Y'                                  Open Printer File
     c***********        eval      prnt  = 'N'                                  No Printer File
     c*
     c                   eval      *in91 = *ON                                  Print Detail Lines
     c***********        eval      *in91 = *OFF                                 No Detail Lines prtd
     c*
     c* If Updte is *OFF, then only counts records to be updated. Need to
     c* change to *ON to actually update the records.
     c*
     c*********          move      *OFF          Updte                          No Records are updtd
     c                   move      *ON           Updte                          Records are Updated
      *
     C     CSKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMCLM#
     c*
     c                   if        updte = *on
     c                   eval      *in90 = *on
     c                   else
     c                   eval      *in90 = *off
     c                   endif
      *
     c                   movel(p)  'WA051 '      prject                         PROJECT CODE
     c                   time                    dsdttm
     c                   move      *blanks       xxfile           10
     c                   eval      xxfile = 'CDCSP'
     c*
     c                   if        prnt  = 'Y'
     c                   open      qsysprt
     c                   except    head
     c                   endif
     c*
      *
     c                   endsr
      ** ========================================================= **
     oqsysprt   e            head           1 06
     o                       dstime               8 '  :  :  '
     o                                           44 'Create Missing CDCSP re'
     o                                           60 'cords from CMCmp'
     o                  90                       73 ' - UPDATE File '
     o                 n90                       73 ' - List-No UPDT'
     o                       xxfile              86
     o                       dsdate             128 '  /  /    '
     o          e            head           2
     o                       q1pgm               10
     o                       prject            +  1
     o                                         +  1 '-  47818'
     o                       q1user             110
     o                                          123 'PAGE'
     o                       page1         z    128
     o          e       91   head           2
     o                                            6 'CO#'
     o                                           15 'Claim #'
     o                                           30 'Eff Date'
     o                                           44 'Explanation'
     o                                           65 'O/C'
     o                                           76 'Mnt Time'
     o                                           86 'Mnt Date'
     o                                           97 'Mnt User'
     o                                          109 'Mnt Pgm '
     o                                          121 'In Cnt'
     o*
     o          e            det            1
     o                       CSCO#         4      6
     o                       CSCLM#              20
     o                       CSEFDT              31 '   /  /  '
     o                       XXEXPL              63
     o                       CSOCST              64
     o                       CSTIME              76 '  :  :  '
     o                       CSDT                87 '   /  /  '
     o                       CSUSER              99
     o                       CSPGM              111
     o                       incnt         3    121
     o*
     o          e            totl        1  2
     o                                           30 '***** TOTALS ***** '
     o*
     o          e            totl           1
     o                                           22 'Total number of record'
     o                                           44 's read  . . . . . . . '
     o                       incnt         3     52
     o*
     o          e            totl           1
     o                                           22 'Total number records N'
     o                                           44 'ot written  . . . . . '
     o                       bycnt         3     52
     o*
     o          e            totl           1
     o                                           22 'Total number records w'
     o                                           44 'ritten to CDCSP . . . '
     o                       upcnt         3     52
     o              n90                       +   4 '** Test Mode-File was not'
     o              n90                       +   1 'Updated at this time **'
     o*
      ** =================================================================== **
