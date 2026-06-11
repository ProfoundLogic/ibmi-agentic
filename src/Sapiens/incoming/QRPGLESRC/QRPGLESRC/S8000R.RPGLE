     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H/TITLE S8000R - Darts Notes Import Process                      *
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION: Darts Notes Import Process               *
     H*                                                               *
     H* CALLED BY: S8000C - Darts Notes Import Driver Program        *
     H*                                                               *
     H* CALLS TO: NONE                                                *
     H*                                                               *
     H* CREATED: 01/02/12  C:282 PROJECT 114IDN                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
      * Darts Notes Import file
     fsadil     if   e           k disk
     fsdcmla    if   e           k disk
     fsmarl1    if   e           k disk
     fshdip     o    e             disk    rename(pdi01:pdi02) prefix(h)
     fqsysprt   o    f  132        printer oflind(*inof) usropn
    d*---------------------------------------------------------------------
    d* Stand Alone Fields
    d*---------------------------------------------------------------------
     dpsds           esds                  extname(szq1p)
     d xxflag          s              1
     d openPrt         s              1n
     d sys_date        s               D   datfmt(*USA)
     d
     d title           c                   'DARTS Remarks Not Referencing-
     d                                      Valid Documents'
     d xxremark        c                   'Unable to convert the notes since-
     d                                      it looks like pages are missing.'
    D*---------------------------------------------------------------------
    D* Data Structures
    D*---------------------------------------------------------------------
    D** Date & Time structure
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D                 ds
     D ##remark                1   4000
     D mrmk1                   1    256
     D mrmk2                 257    275
     D armk1                 276   4000
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     ** Additional remarks file
     c     arky1         klist
     c                   kfld                    qqkey
     c                   kfld                    xxseq#            5 0
     c
     c     *entry        plist
     c                   parm                    xxrmcd            5
      *
     c     datchk        plist
     c                   parm                    srdti             8 0
     c                   parm      *blanks       srffmt            8
     c                   parm      *blanks       srtfmt            8
     c                   parm                    srdto            10
     c                   parm                    srerr             1
      *
    C*================================================================
    C*  MAIN PROGRAM
    c*================================================================
     c     *loval        setll     sadil
     c                   read      sadil                                  60
     c                   dow       *in60 = *off
      * Main remarks
     c                   select
     c                   when      (dinp# = 1 and
     c                             dinpco <> *blanks)
      *
     c                   movel     dikey         ##key
     c                   movel     dinpco        ##remark
     c                   movel     mrmk1         ##comm
     c                   movel     mrmk2         ##com2
     c                   move      '00000'       ##seq#
      *
     c                   time                    dsdttm
     c                   z-add     dsdate        srdti
     c                   call      'SRCVTDAT'    datchk
     c                   movel     srdto         ##cdat
      *
     c                   call      'DA700R'
     c                   PARM                    ##key            40
     c                   PARM                    ##comm          256
     c                   PARM                    ##cdat            8
     c                   PARM      *blanks       ##dcid           12
     c                   PARM      'W0070R'      ##pnam           10
     c                   PARM      '000'         ##secl            3
     c                   PARM      q1user        ##user           10
     c                   PARM      q1pgm         ##pgm            10
     c                   PARM      *blanks       ##fold           80
     c                   PARM      *blanks       ##difl            1
     c                   PARM      q1pgm         ##ipgm           10
     c                   PARM      *blanks       ##fl01            1
     c                   PARM      *blanks       ##flo2            1
     c                   PARM      xxrmcd        ##rmcd            5
     c                   PARM                    ##seq#            5
     c                   PARM      'N'           ##sbyl            1
     c                   PARM                    ##com2           19
     c                   PARM      *blanks       ##apid            2
     c                   PARM      *blanks       ##idoc           12
     c                   PARM      *blanks       ##ique           10
     c                   PARM      *blanks       ##imgf            1
     c                   PARM      *blanks       ##ipth           10
     c                   PARM      *blanks       ##pdix            5
     c                   PARM      *blanks       ##qpty            1
     c                   PARM      *blanks       ##dctp            5
     c                   PARM      *blanks       ##csid            2
     c                   PARM      *blanks       ##code            2
      *
      * Call DA701R with the following parameters to get the create
      * additional darts remark.
      *
     c                   z-add     1             strlen            4 0
     c     75            subst     armk1:strlen  qqcomm
     c                   movel     ##key         qqkey
     c                   move      ##seq#        qqseq#
      *
     c                   dow       (qqcomm <> *blanks)
      *
     c                   CALL      'DA701R'
     c                   PARM                    qqkey            40
     c                   PARM                    qqseq#            5
     c                   PARM                    qqcomm           75
     c                   PARM      'W0070R'      qqpnam           10
     c                   PARM      *blanks       qqsecl            3
     c                   PARM      q1user        qquser           10
     c                   PARM      q1pgm         qqpgm            10
     c                   PARM      xxrmcd        qqrmcd            5
     c                   PARM      *blanks       qqasq#            5
      *
     c                   eval      strlen = strlen + 75
     c     75            subst     armk1:strlen  qqcomm
      *
     c                   enddo
      * Set flag to 'W' if written in file SDCMP
     c                   eval      xxflag = 'W'
      *
      * Call DA701R with the following parameters to get the create
      * additional darts remark when Notes Page Number <> 1.
      *
     c                   when      dinp# > 1 and
     c                              dinpco <> *blanks
     c                   movel     dikey         qqkey
     c                   move      ##seq#        xxseq#
      *
     c     arky1         chain     par01                              60
     c   60arky1         chain     pcm01                              60
     c                   if        *in60 = *off
      *
     c                   z-add     1             strlen            4 0
     c     75            subst     dinpco:strlen qqcomm
     c                   move      ##seq#        qqseq#
      *
     c                   dow       (qqcomm <> *blanks)
      *
     c                   CALL      'DA701R'
     c                   PARM                    qqkey
     c                   PARM                    qqseq#
     c                   PARM                    qqcomm
     c                   PARM      'W0070R'      qqpnam
     c                   PARM      *blanks       qqsecl
     c                   PARM      q1user        qquser
     c                   PARM      q1pgm         qqpgm
     c                   PARM      xxrmcd        qqrmcd
     c                   PARM      *blanks       qqasq#
      *
     c                   eval      strlen = strlen + 75
     c     75            subst     dinpco:strlen qqcomm
      *
     c                   enddo
      * Set flag to 'W' if written in file SMARP
     c                   eval      xxflag = 'W'
      *
     c                   else
     c                   exsr      srException
     c                   endif
     c                   when      dinpco = *blanks
     c                   exsr      srException
     c                   endsl
      *
      * Write history record in file SHDIP
     c                   eval      hdikey  = dikey
     c                   eval      hdinedt = dinedt
     c                   eval      hdinetm = dinetm
     c                   eval      hdinp#  = dinp#
     c                   eval      hdinpcn = dinpcn
     c                   eval      hdinpco = dinpco
      *
     c                   time                    dsdttm
     c                   move      *date         sys_date
     c     *cymd         move      sys_date      hdiprdt
     c                   eval      hdiprtm = dstime
      *
     c                   if        xxflag = 'W'
     c                   eval      hdiprfl= 'Y'
     c                   else
     c                   eval      hdiprfl= 'E'
     c                   endif
     c                   write     pdi02
      *
     c                   read      sadil                                  60
     c                   enddo
     c
     c                   eval      *inlr = *on
      *********************************************************************
      * srException - Write exception report
      *********************************************************************
     c     srException   begsr
     c                   eval      xxflag = ' '
     c                   time                    dsdttm
      * Open the printer file if closed.
     c                   if        not openPrt
     c                   open      QSYSPRT
     c                   eval      openPrt = *on
     c                   eval      *inof   = *on
     c                   endif
     c
      * Write the header part
     c                   if        *inof
     c                   except    head
     c                   eval      *inof   = *off
     c                   endif
      * Write the detail part
     c                   except    detail
     c                   endsr
      *********************************************************************
     oqsysprt   e            head           1 03
     o                       dsDate             + 2 '  /  /    '
     o                       dsTime             + 3 '  :  :  '
     o                       title               88
     o          e            head           1
     o                                           05 'CLAIM'
     o                                           35 'NOTES'
     o                                           45 'NOTES'
     o                                           53 'NOTES'
     o                                           62 'REMARK'
     o          e            head           1
     o                                           06 'NUMBER'
     o                                           35 'ENTRY'
     o                                           45 'ENTRY'
     o                                           52 'PAGE'
     o          e            head           2
     o                                           34 'DATE'
     o                                           44 'TIME'
     o                                           54 'NUMBER'
      *
     o          e            detail         1
     o                       DIKEY               25
     o                       DINEDT              35
     o                       DINETM              45
     o                       DINP#               50
     o                       xxremark           122
