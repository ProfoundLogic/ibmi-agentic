     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR29601A - NJ Large Risk - Large Deductible Endorsement
      //===============================================================*
      // REVISIONS:                                                    *
      //                                                               *
      // 10/19/22 C:342 Project: WC2834 (Recompile)                    *
      //                Endorsement Recognition of Cancellation        *
      //                                                               *
      // 07/06/22 C:079 Porject: WC2870                                *
      //                Set policy aggregate = 0                       *
      //                                                               *
      // 12/05/18 C:180 Support: 73238                                 *
      //                Change attachment criteria to attach to LD     *
      //                policy regardless of Aggregate.                *
      //                                                               *
      // 01/15/18 C:291 Support: 51608                                 *
      //                Change attachment criteria to attach to LD     *
      //                policy and zero Aggregate.                     *
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      //                                                               *
      //===============================================================*
      // Notes:                                                        *
      //                                                               *
      //  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      //                                                               *
      //  QQMODE = 1 - Called to determine if form should be attached. *
      //  QQMODE = 2 - Called to print endorsement.                    *
      //  QQMODE = 3 - Called for additional input.                    *
      //  QQMODE = 4 - Called for mid-term processing.                 *
      //===============================================================*

      /COPY SCOPYEN01F

     Fwtddl2    if   e           k disk
     Fwxpel     uf a e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)
     fwtabl     if   e           k disk    include(pbs01)
     fwmldl1    if   e           k disk

      /COPY SCOPYEN01D

      /COPY WWCPOLDAT4

     D qqForm          s             15A   Inz('WC290601A ')
     D addpgm          c                   const('SRXXXXXXXX')

     D xxunid          s                   like(peunid)
     D x1end#          s                   like(peend#)
     d xxampo          s                   like(FMAM01)
     d xxamag          s                   like(FMAM01)
     d xxDedSize       s              1
      *=========================================================================
      *  Prototypes
      *=========================================================================
     d rtvDedSize      pr             1                                         return S/L
     d  co#                                like(ELCO#)
     d  bscd                               like(ELBSCD)
     d rtvSmDedInfo    pr
     d  ampo                               like(FMAM01)
     d  amag                               like(FMAM01)
     d rtvLgDedInfo    pr
     d  ampo                               like(FMAM01)
     d  amag                               like(FMAM01)

     D SR29601A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR29601A        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      *  Deductible Discount Code Table
     C     ddky          KList
     C                   KFld                    ELCO#
     C                   KFld                    ELFND
     C                   KFld                    RSJRST
     C                   KFld                    ELDCDE

     C     ddky1         KList
     C                   KFld                    ELCO#
     C                   KFld                    ELFND
     C                   KFld                    RSJRST
     C                   KFld                    ELDCDE
     C                   KFLD                    XXDDT             7 0

     C     pekey         KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxdt              7 0
     C     pekey1        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C     pekey2        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxunid
     C                   KFld                    x1end#

      /COPY SCOPYEN01C

      *=========================================================================
      *  OTHINF - Subroutine to load form specific information & write record
      *=========================================================================
     C     othinf        begsr
      *    initialize
     c                   movel(p)  xxform        fmform
      *  Deductible Discount information

     c                   eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     c                   select
     c                   when      xxDedSize = 'L'
     c                   callp     rtvLgDedInfo (xxAmpo: xxAmag)
     c                   endsl

     c                   eval      FMAM01 = xxAmag
     c                   eval      FMAM02 = xxAmpo
      *  If printing a policy page.  (Not called from W1054R)

     c                   call      'SRCHKPGM'
     c                   parm      'W1054R   '   #1pgm            10
     c                   parm      *BLANK        #1flag            1

     C                   if        #1flag = '0'
      * Update the endorsement reporting file with the proper data.
     C                   Z-ADD     0             xxunid

     c                   move      *hival        xxdt
     C     pekey         setgt     ppe02
     C     pekey1        readpe    ppe02                                  60
     C                   If        *in60 = *off
     C                   z-add     peunid        xxunid
     C                   endif
     C                   movel     xxform        x1end#
     c     pekey2        chain     ppe01                              60
      * Copy data into the record.
     C                   If        *in60 = *off
     c                   clear                   E29601

     c                   eval      xxabi = FMAM01
     c                   eval      xxbia = FMAM02
     c                   eval      xxbid = FMAM02
     c                   Eval      xxbiap = FMAM02
     c                   Eval      xxbidp = FMAM02
       //*** Beg Chg *** 07/06/22 ********************************************
     c*****************  Eval      xxagpo = FMAM01
       xxAgPo = 0;
       //*** End Chg *** 07/06/22 ********************************************
     c                   Eval      xxagpp = FMAM01
     c                   move      *blanks       pedata
     c                   movel     E29601        pedata
     c                   movel     q1user        peuser
     c                   movel     q1pgm         pepgm
     c                   z-add     dstime        petime
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         pedt

     C                   update    ppe01

     C                   endif                                                  *in60 ifeq off

     C                   endif
     C                   endsr
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     pgmstr        begsr
     C                   endsr
      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     C     pgmend        begsr
     C                   endsr
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     C     valid         begsr

     c                   eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     c                   select
     c                   when      xxDedSize = 'L'
     c                   callp     rtvLgDedInfo (xxAmpo: xxAmag)
     c                   endsl
     c                   if        xxAmpo <> 0
     c                   eval      crtfrm = *on
     c                   endif
     c                   endsr
      *=========================================================================
      * Query whether policy is small or large deductible
      *=========================================================================
     p rtvDedSize      b

     d rtvDedSize      pi             1
     d  co#                                like(ELCO#)
     d  bscd                               like(ELBSCD)

     d kkco#           s                   like(BSCO#)
     d kkkey           s                   like(BSKEY)

     c     bsky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkkey

     c     rsky          chain     prs01
     c                   if        not(%found)
     c                   eval      RSJRST = *blank
     c                   return    ' '
     c                   endif

     c                   eval      kkco# = co#
     c                   movel(p)  bscd          kkkey
     c     bsky          chain     pbs01
     c                   select
     c                   when      BSLPGM = 'L'
     c                   return    'L'
     c                   other
     c                   return    'S'
     c                   endsl

     p rtvDedSize      e
      *=========================================================================
      * rtvSmDedInfo  - retrieves DDAMPO and DDAMAG
      *=========================================================================
     p rtvSmDedInfo    b
     d rtvSmDedInfo    pi
     d  ampo                               like(FMAM01)
     d  amag                               like(FMAM01)

     c                   move      ELCO#         qqco#
     c                   move      ELFND         qqfnd
     c                   move      ELEMP#        q1emp#
     c                   move      ELDIV         q1div
     c                   move      ELFYR         q1fyr
     c                   move      ELDCEF        q2date

     c                   call      'SREFFD'
     c                   parm                    qqco#             3
     c                   parm                    qqfnd             3
     c                   parm                    q1fyr             3
     c                   parm                    q1emp#            9
     c                   parm                    q1div             5
     c                   parm                    q2date            7

     c                   move      q2date        xxddt
     c     ddky1         setgt     pdd01
     c     ddky          readpe    pdd01
     c                   dow       not(%eof) and DDSTAT <> 'A'
     c     ddky          readpe    pdd01
     c                   enddo

     c                   if        not(%eof)
     c                   eval      ampo = DDAMPO
     c                   eval      amag = DDAMAG
     c                   endif

     p rtvSmDedInfo    e
      *=========================================================================
      * rtvLgDedInfo - retrieves LDAMPO and LDAMAG
      *=========================================================================
     p rtvLgDedInfo    b
     d rtvLgDedInfo    pi
     d  ampo                               like(FMAM01)
     d  amag                               like(FMAM01)

     c     elky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c                   kfld                    ELFYR

     c     elky          chain     pld01
     c                   if        %found
     c                   eval      ampo = LDAMP1
     c                   eval      amag = LDAMG1
     c                   endif

     p rtvLgDedInfo    e
