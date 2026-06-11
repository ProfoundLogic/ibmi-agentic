     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H bnddir('WTBNDDIR')
     H dftactgrp(*no) actgrp(*CALLER)
     H option(*nodebugio:*srcstmt)
    H/TITLE SR29604A - NJ - Large Risk - Aggregate Limit Endorsement
     *===============================================================*
     * Created   : 07/05/19 - Copied from SR29604                    *
     * Programmer: C:79                                              *
     * Project   : WC2652                                            *
     *===============================================================*
     * REVISIONS:                                                    *
      *                                                               *
     * 10/19/22 C:342 Project: WC2834 (Recompile)                    *
     *                Endorsement Recognition of Cancellation        *
     *                                                               *
     * 01/04/17 C:136 Project: SY371                                 *
     *                Implement Phase 2 - changes not commented      *
     *                                                               *
     * 10/15/12 C:136 Support: WA114 80939                           *
     *               -Add Large Deductible logic                     *
      *                                                               *
      * 10/11/07 C:190 SUPPORT WA099 54504                            *
      *                Not needed.                                    *
      *                                                               *
      *  04/10/06 C:65 PROJECT SUPP#51293                             *
      *                CHANGE DEDUCTIBLE DISCOUNT TO PULL BASED ON    *
      *                ANNIV RATING DATE INSTEAD OF POLICY EFF DATE.  *
     *===============================================================*
     * Notes:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
     *                                                               *
     *  QQMODE = 1 - Called to determine if form should be attached. *
     *  QQMODE = 2 - Called to print endorsement.                    *
     *  QQMODE = 3 - Called for additional input.                    *
     *  QQMODE = 4 - Called for mid-term processing.                 *
     *===============================================================*
     * Copybook F specs
     F/COPY SCOPYEN01F
    
      ** Deductible Discount
     Fwtddl2    if   e           k disk
     fwtabl     if   e           k disk    include(pbs01)
     fwmldl1    if   e           k disk
     *=========================================================================
     *  Definition Specs
     *=========================================================================
     D SR29604A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR29604A        PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1

     * Copybook D specs
     D/COPY SCOPYEN01D
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
     * Form ID
     D qqform          s             15a   inz('WC290604A ')

     * Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
     d xxampo          s                   like(FMAM01)
     d xxamag          s                   like(FMAM01)
     d xxDedSize       s              1
     *=========================================================================
     *  Prototypes
     *=========================================================================
    d rtvDedSize      pr             1                                         return S/L
     d  co#                                like(ELCO#)
     d  bscd                               like(ELBSCD)
    d rtvSmDedInfo    pr
     d  ampo                               like(FMAM01)
     d  amag                               like(FMAM01)
    d rtvLgDedInfo    pr
     d  ampo                               like(FMAM01)
     d  amag                               like(FMAM01)
     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
    
     *=========================================================================
     *  PLIST definitions
     *=========================================================================
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
      *
      *  Deductible Discount Code Table
      *
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
     *=========================================================================
     *  Main
     *=========================================================================
     * Copybook C specs
     C/COPY SCOPYEN01C
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
    
     C                   endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
    C                   eval      runningSRText = *On
     C                   endsr
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
    
     C                   endsr
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr

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

     c                   endsr                                                  valid
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
      *** end chg ***** 10/15/12 ***********************************************
