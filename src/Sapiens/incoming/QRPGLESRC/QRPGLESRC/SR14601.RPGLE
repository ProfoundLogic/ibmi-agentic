     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
      *===============================================================*
      *  (C) Copyright Fiserv Insurance Solution Inc., 2006          *
     H/TITLE SR14601 - IOWA Deductible Endorsement
      *===============================================================*
      * WC140601  - IOWA Deductible Endorsement                       *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      * Created   : 12/07/06                                          *
      * Programmer: C:184                                             *
      * Project   : WC1505                                            *
      *===============================================================*
      * REVISIONS:                                                    *
      *                                                                        *
      * 11/13/13 C:287 Support: WA099 80909                                    *
      *                - Add Large Deductible logic                            *
      *
      * 01/04/13 C:284 Support SEQ79989                               *
      *                Add if condition to process record if          *
      *                DDSTAT = 'A'.                                  *
      *               -Chg'd Q1DATE field to Q2DATE (not commented)   *
      *
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
      **Copybook F specs
     F/COPY SCOPYEN01F
     Fwtddl2    if   e           k disk
     f*** beg add ***** 11/13/13 *******************************************
     fwtabl     if   e           k disk    include(pbs01)
     fwmldl1    if   e           k disk
     f*** end add ***** 11/13/13 *******************************************
      *=========================================================================
      * Definition Specs
      *=========================================================================

      **Copybook D specs
     D/COPY SCOPYEN01D
      *=========================================================================
      *  /COPY required constants
      *=========================================================================
      **Form ID
     D QQform          c                   const('WC140601 ')
      **Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
      *** Begin Add *** 11/13/13 **** SEQ80909 *****************************
     d xxampo          s                   like(FMAM01)
     d xxamag          s                   like(FMAM01)
     d xxDedSize       s              1
      *=========================================================================
      * Prototypes
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
      *** End Add ***** 11/13/13 **** SEQ80909 *****************************

      *=========================================================================
      *  *LIKE DEFN definitions
      *=========================================================================
      *
      *
      *=========================================================================
      *  PLIST definitions
      *=========================================================================

      **Entry parameters
     C     *entry        plist
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
     C                   parm                    qqemp#            9
     C                   parm                    qqdiv             5
     C                   parm                    qqfyr             3
     C                   parm                    qqmode            1
     C                   parm                    qqrrn             9 0
     C                   parm                    qqmod2            1
      *=========================================================================
      * KLIST definitions
      *=========================================================================
     C*
     C     ddky          KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    rsjrst
     C                   KFld                    eldcde
     C                   KFLD                    XXDDT             7 0
     C*
     C     ddky1         KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    rsjrst
     C                   KFld                    eldcde
      *
      * ========================================================================
      * Main
      * ========================================================================
      **Copybook C specs
     C/COPY SCOPYEN01C
      *
      * ========================================================================
      * OTHINF- Subroutine to load form specific information & write record
      * ========================================================================
     C     othinf        begsr
      *
     C     elky2         chain     pel02                              60
    
     C                   dow       *in60 = *off
      *** Begin Delete *** 11/13/13 *** SEQ80909 ******************************
     C***  rsky          chain     prs01                              60
      *
     C***                if        *in60 = *off
     C***                          and rsjrst = enst
      *
     C***                MOVE      ELDCEF        Q2DATE
     C***                MOVE      ELFYR         Q1FYR
     C***                MOVE      ELEMP#        Q1EMP#
     C***                MOVE      ELDIV         Q1DIV
     C*
     C***                CALL      'SREFFD'
     C***                PARM                    QQCO#             3
     C***                PARM                    QQFND             3
     C***                PARM                    Q1FYR             3
     C***                PARM                    Q1EMP#            9
     C***                PARM                    Q1DIV             5
     C***                PARM                    Q2DATE            7
     C*
     C***                MOVE      Q2DATE        XXDDT
     C***  ddky          setgt     pdd01
     C***  ddky1         readpe    pdd01                                  60
      *
     C*** Beg Add *** 01/04/13 ****************************************
     C*  To read until a record with active status is found.
     C***  *In60         Downe     *On
     C***  Ddstat        Andne     'A'
     C*
     C***  Ddky1         Readpe    Pdd01                                  60
     C*
     C***                Enddo
     C*** End Add *** 01/04/13 ****************************************
      *
     C***                if        *in60 = *off
     C***                          and ddampo <> 0
      *
     C***                eval      fma701 = ddampo
     C***                leave
      *
     C***                endif
      *
     C***                endif
      *** End Delete *** 11/13/13 *** SEQ80909  ***********************
     C     elky2         reade     pel02                                  60
     C                   enddo

      *** Begin Add  *** 11/13/13 *** SEQ80909  ***********************
     c                   eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     c                   select
     c                   when      xxDedSize = 'L'
     c                   callp     rtvLgDedInfo (xxAmpo: xxAmag)
     c                   when      xxDedSize = 'S'
     c                   callp     rtvSmDedInfo (xxAmpo: xxAmag)
     c                   endsl

     c                   eval      FMAM01 = xxAmag
     c                   eval      FMAM02 = xxAmpo
      *** End Add *** 11/13/13 *** SEQ80909 ***********************************
    
     C*
      **Write the output record
     C                   write     pfm01
      *
      *
     C                   endsr
      *=========================================================================
      * PGMSTR- Beginning of program processing
      *=========================================================================
     C     pgmstr        begsr
      *
     C                   endsr
      *=========================================================================
      * PGMEND- End of program processing
      *=========================================================================
     C     pgmend        begsr
      *
      *
     C                   endsr
      *=========================================================================
      * VALID- Subroutine to attach this form to the policy
      *=========================================================================
     C     valid         begsr
      *
      *** Begin Delete *** 11/13/13 **** SEQ80909 *****************************
     C***  rsky          chain     prs01                              60
      *
     C***                if        *in60 = *off
     C***                          and rsjrst = enst
      *
     C***                MOVE      ELDCEF        Q2DATE
     C***                MOVE      ELFYR         Q1FYR
     C***                MOVE      ELEMP#        Q1EMP#
     C***                MOVE      ELDIV         Q1DIV
     C*
     C***                CALL      'SREFFD'
     C***                PARM                    QQCO#             3
     C***                PARM                    QQFND             3
     C***                PARM                    Q1FYR             3
     C***                PARM                    Q1EMP#            9
     C***                PARM                    Q1DIV             5
     C***                PARM                    Q2DATE            7
     C*
     C***                MOVE      Q2DATE        XXDDT
     C***  ddky          setgt     pdd01
     C***  ddky1         readpe    pdd01                                  60
      *
     C*** Beg Add *** 01/04/13 ****************************************
     C*  To read until a record with active status is found.
     C***  *In60         Downe     *On
     C***  Ddstat        Andne     'A'
     C*
     C***  Ddky1         Readpe    Pdd01                                  60
     C*
     C***                Enddo
     C*** End Add *** 01/04/13 ****************************************
      *
     C***                if        *in60 = *off
     C***                          and ddampo <> 0
      *
      ** Found a division with a matching jurisdiction state
     C***                move      *on           crtfrm
      *
     C***                endif
      *
     C***                endif
      *** End Delete *** 11/13/13 **** SEQ80909 *****************************
      *

      *** Begin Add *** 11/13/13 **** SEQ80909 *****************************
     c                   eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     c                   if        ENST = RSJRST
     c                   select
     c                   when      xxDedSize = 'L'
     c                   callp     rtvLgDedInfo (xxAmpo: xxAmag)
     c                   when      xxDedSize = 'S'
     c                   callp     rtvSmDedInfo (xxAmpo: xxAmag)
     c                   endsl
     c                   if        xxAmpo <> 0
     c                   eval      crtfrm = *on
     c                   endif
     c                   endif
      *** End Add *** 11/13/13 **** SEQ80909 *****************************
     C                   endsr

      *** Begin Add *** 11/13/13 **** SEQ80909 *****************************
      *=========================================================================
      *rtvDedSize- Query whether policy is small or large deductible
      *=========================================================================
     p rtvDedSize      b

     d rtvDedSize      pi             1                                         return S/L
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
      *rtvSmDedInfo - retrieves DDAMPO and DDAMAG
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
      *rtvLgDedInfo- retrieves LDAMPO and LDAMAG
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

      *** End Add *** 11/13/13 *** SEQ80909 **********************************
