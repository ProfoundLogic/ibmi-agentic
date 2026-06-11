     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')
      /TITLE SR40602 - SD BENEFITS DEDUCTIBLE ENDORSEMENT
      *===============================================================*
      * WC400602 - South Dakota Benefits Deductible Endorsement       *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      *===============================================================*
      * REVISIONS:                                                    *
      *                                                               *
      *  10/19/22 C:342 Project: WC2834                               *
      *                 Endorsement Recognition of Cancellation       *
      *                                                               *
      *  08/04/17 C:136 Support: 24858                                *
      *                 Correct attachment/print logic                *
      *                                                               *
      *  10/12/16 C:267 Project: SY371                                *
      *                 Scriptura phase 2                             *
      *                 Clean up Program/Copybook                     *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*
      * Notes:                                                        *
      *                                                               *
      *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      *                                                               *
      *  QQMODE = 1 - Called to determine if form should be attached. *
      *  QQMODE = 2 - Called to print endorsement.                    *
      *  QQMODE = 3 - Called for additional input.                    *
      *  QQMODE = 4 - Called for mid-term processing.                 *
      *===============================================================*
     fwmldl1    if   e           k disk
     fwtabl     if   e           k disk    include(pbs01)
     Fwtddl2    if   e           k disk

      /COPY SCOPYEN01F

      /COPY SCOPYEN01D

     D qqform          s             15A   Inz('WC400602  ')
     D addpgm          c                   const('SRXXXXXXXX')

     D SR40602         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR40602         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

    d rtvDedSize      pr             1                                         return S/L
     d  co#                                like(ELCO#)
     d  bscd                               like(ELBSCD)
    d rtvSmDedInfo    pr
     d  ampo                               like(FMAM01)
     d  amag                               like(FMAM01)
    d rtvLgDedInfo    pr
     d  ampo                               like(FMAM01)
     d  amag                               like(FMAM01)

     d xxampo          s                   like(FMAM01)
     d xxamag          s                   like(FMAM01)
     d xxDedSize       s              1

     C     ddky1         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    rsjrst
     C                   kfld                    eldcde
     C                   KFLD                    XXDDT             7 0

     C     ddky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    rsjrst
     C                   kfld                    eldcde

      /COPY SCOPYEN01C
      *
      *=========================================================================
      *  OTHINF - Subroutine to load form specific information & write record
      *=========================================================================
     c     othinf        begsr

     c     elky2         setll     pel02
     c     elky2         reade     pel02                                  60
     c                   dow       *in60 = *off
        //***** Beg Add *** 10/19/22 ******************************
        xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
        If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
        //***** End Add *** 10/19/22 ******************************
     c     rsky          chain     prs01                              60
     c                   If        RSjrst = ENst
     c                   Eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     c                   Select
     c                   When      xxDedSize = 'L'
     c                   Callp     rtvLgDedInfo (xxAmpo: xxAmag)
     c                   When      xxDedSize = 'S'
     c                   Callp     rtvSmDedInfo (xxAmpo: xxAmag)
     c                   EndSl

     c                   If        xxAmpo <> 0
     c                   Eval      fma701 = xxAmpo
     c                   Leave
     c                   Eval      writeInCopyBook = *Off
     c                   Exsr      WriteWorkFile
     c                   Endif
     c                   Endif

        //***** Beg Add *** 10/19/22 ******************************
        Endif;
        //***** End Add *** 10/19/22 ******************************
     c     elky2         reade     pel02                                  60
     c                   enddo

     c                   endsr
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
     c     valid         begsr

     c                   eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     c                   select
     c                   when      xxDedSize = 'L'
     c                   callp     rtvLgDedInfo (xxAmpo: xxAmag)
     c                   when      xxDedSize = 'S'
     c                   callp     rtvSmDedInfo (xxAmpo: xxAmag)
     c                   endsl
     c                   if        xxAmpo <> 0
     c                   eval      crtfrm = *on
     c                   endif

     c                   endsr
     *=========================================================================
     * Query whether policy is small or large deductible
     *=========================================================================
    p rtvDedSize      b

    d rtvDedSize      pi             1                                         return S/L
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

    p rtvDedSize      e
     *=========================================================================
     * rtvSmDedInfo  - retrieves DDAMPO and DDAMAG
     *=========================================================================
    p rtvSmDedInfo    b
    d rtvSmDedInfo    pi
     d  ampo                               like(FMAM01)
     d  amag                               like(FMAM01)

     c                   move      ELCO#         qqco#
     c                   move      ELFND         qqfnd
     c                   move      ELEMP#        q1emp#
     c                   move      ELDIV         q1div
     c                   move      ELFYR         q1fyr
     c                   move      ELDCEF        q2date
     c*
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

    p rtvSmDedInfo    e
     *=========================================================================
     * rtvLgDedInfo - retrieves LDAMPO and LDAMAG
     *=========================================================================
    p rtvLgDedInfo    b
    d rtvLgDedInfo    pi
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

    p rtvLgDedInfo    e
