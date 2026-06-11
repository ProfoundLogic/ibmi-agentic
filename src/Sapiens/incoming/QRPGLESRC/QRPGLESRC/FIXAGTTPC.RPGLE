    H*================================================================
     H*  (C) Copyright FISERV Insurance Solutions, Inc., 2004
    H*
    H*  FIXAGTTYP - Expand Agent Type Code from 2 to 3 Positions
     H*                                                               *
     H*  CREATED: 12/02/04  CONSULTANT: C:132  PROJECT: WC1232        *
    H*
    H*================================================================
     H* REVISIONS:                                                    *
     H*                                                               *
    H*================================================================
    F*
    F*  Physical file definition (for maintaining record)
     FWMAGP     uf a e             disk
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
     c                   read      pag01                                  60
      *
     c                   dow       (not *in60)
      *
     c                   eval      incnt = incnt + 1
      *
     c                   if        AGTPCD <> *BLANKS AND
     c                             AGTPC2 =  *BLANKS
     c                   EVAL      AGTPC2 =  AGTPCD
     c                   eval      nucnt = nucnt + 1
     c                   If        updte  = *on
     c                   UPDATE    PAG01
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
     c                   read      PAG01                                  60
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
     c************       eval      prnt  = 'N'                                  No Printer File
     c*
     c************       eval      *in91 = *ON                                  Print Detail Lines
     c                   eval      *in91 = *OFF                                 No Detail Lines prtd
     c*
     c* If Updte is *OFF, then only counts records to be updated. Need to
     c* change to *ON to actually update the records.
     c*
     c**********         move      *OFF          Updte                          No Records are updtd
     c                   move      *ON           Updte                          Records are Updated
      *
     c*
     c                   if        updte = *on
     c                   eval      *in90 = *on
     c                   else
     c                   eval      *in90 = *off
     c                   endif
      *
     c                   movel(p)  'WC1232'      prject                         PROJECT CODE
     c                   time                    dsdttm
     c                   move      *blanks       xxfile           10
     c                   eval      xxfile = 'WMAGP'
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
     o                                           44 'Update new AGT TYPE Cod'
     o                                           58 'e 2 -> 3 WMAGP'
     o                  90                       73 ' - UPDATE File '
     o                 n90                       73 ' - List-No UPDT'
     o                       xxfile              86
     o                       dsdate             128 '  /  /    '
     o          e            head           2
     o                       q1pgm               10
     o                       prject            +  1
     o                       q1user             110
     o                                          123 'PAGE'
     o                       page1         z    128
     o          e       91   head           2
     o                                            7 'CO#'
     o                                           17 'Agt FEIN'
     o                                           27 'AGTPCD'
     o                                           35 'AGTPC2'
     o                                           45 'AGT NAME'
     o                                           85 'Mnt Date'
     o                                           97 'Mnt User'
     o                                          109 'Mnt Prgm'
     o                                          119 'In Cnt'
     o*
     o          e            det            1
     o                       AGCO#         4      6
     o                       AGFEIN        3     17
     o                       AGTPCD              25
     o                       AGTPC2              33
     o                       AGNAME              77
     o                       AGDT                87 '   /  /  '
     o                       AGUSER              99
     o                       AGPGM              111
     o                       incnt         3    119
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
     o                                           22 'Total number records w'
     o                                           44 'ith AGTPC2 Not updated'
     o                       bycnt         3     52
     o*
     o          e            totl           1
     o                                           22 'Total number records u'
     o                                           44 'dated to WMAGP. . . . '
     o                       upcnt         3     52
     o              n90                       +   4 '** Test Mode-File was not'
     o              n90                       +   1 'Updated at this time **'
     o*
      ** =================================================================== **
