     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
     H/TITLE SR44603 - VT - Benefits Deductible Endorsement
      *========================================================================*
      * WC440603 - VT Benefits Deductible Endorsement                          *
      *                                                                        *
      * Program Description:                                                   *
      *                                                                        *
      *   Determines if this endorsement form should be attached to            *
      *   the policy and creates data in the Endorsement Form                  *
      *   Workfile (WXFMP) prior to a form merge.                              *
      *                                                                        *
      * Created   : 04/18/11                                                   *
      * Programmer: C:235                                                      *
      * Project   : WC2008                                                     *
      *========================================================================*
      * REVISIONS:                                                             *
      *                                                                        *
      * 10/19/22 C:342 Project: WC2834                                         *
      *                Endorsement Recognition of Cancellation                 *
     H*                                                               *
     * 09/29/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -No comments.
      *                                                                        *
      * 05/28/13 C:287 Support: WA099 80909                                    *
      *                - Add Large Deductible logic                            *
      *                                                                        *
      *                                                                        *
      * ---------------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of revisions.                    *
      * ---------------------------------------------------------------------- *
      *========================================================================*
      * Notes:                                                                 *
      *                                                                        *
      *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).                   *
      *                                                                        *
      *  QQMODE = 1 - Called to determine if form should be attached.          *
      *  QQMODE = 2 - Called to print endorsement.                             *
      *  QQMODE = 3 - Called for additional input.                             *
      *  QQMODE = 4 - Called for mid-term processing.                          *
      *========================================================================*
      ** Copybook F specs
     F/COPY SCOPYEN01F
     Fwtddl2    if   e           k disk
     Fwtabl     if   e           k disk    include(pbs01)
     Fwmldl1    if   e           k disk
      *=========================================================================
      *  Definition Specs
      *=========================================================================

      ** Copybook D specs
     D/COPY SCOPYEN01D
      *=========================================================================
      *  /COPY required constants
      *=========================================================================
      ** Form ID
     D qqForm          s             15a   inz('WC440603  ')
      ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
     D xxampo          s                   like(FMAM01)
     D xxamag          s                   like(FMAM01)
     D xxDedSize       s              1
      *=========================================================================
      *  Prototypes
      *=========================================================================
     D rtvDedSize      pr             1
     D  co#                                like(ELCO#)
     D  bscd                               like(ELBSCD)
     D rtvSmDedInfo    pr
     D  ampo                               like(FMAM01)
     D  amag                               like(FMAM01)
     D rtvLgDedInfo    pr
     D  ampo                               like(FMAM01)
     D  amag                               like(FMAM01)

      *entry parameters
     D SR44603         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR44603         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
      *
      *=========================================================================
      *  KLIST definitions
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
     C*
      *=========================================================================
      *  Main
      *=========================================================================
      ** Copybook C specs
     C/COPY SCOPYEN01C
      *
      *=========================================================================
      *  OTHINF - Subroutine to load form specific information & write record
      *=========================================================================
     C     othinf        begsr
      *
     C     elky2         chain     pel02                              60
    
     C                   dow       *in60 = *off
        //***** Beg Add *** 10/19/22 ******************************
        xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
        If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
        //***** End Add *** 10/19/22 ******************************
     C     rsky          chain     prs01                              60
      *
     C                   if        *in60 = *off
     C                             and rsjrst = enst
      *
     C                   Eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     C                   Select
     C                   When      xxDedSize = 'L'
     C                   Callp     rtvLgDedInfo (xxAmpo: xxAmag)
     C                   When      xxDedSize = 'S'
     C                   Callp     rtvSmDedInfo (xxAmpo: xxAmag)
     C                   EndSl

     C                   If        xxAmpo <> 0
     C                   Eval      fma701 = xxAmpo
     C                   leave
      *
     C                   endif
      *
     C                   endif
        //***** Beg Add *** 10/19/22 ******************************
        Endif;
        //***** End Add *** 10/19/22 ******************************
     C     elky2         reade     pel02                                  60
     C                   enddo
      *
     C                   endsr
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     pgmstr        begsr
      *
      *
     C                   endsr
      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     C     pgmend        begsr
      *
      *
     C                   endsr
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     C     valid         begsr
      *
     C                   Eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)

     C                   Select
     C                   When      xxDedSize = 'L'
     C                   Callp     rtvLgDedInfo (xxAmpo: xxAmag)
     C                   When      xxDedSize = 'S'
     C                   Callp     rtvSmDedInfo (xxAmpo: xxAmag)
     C                   Endsl

     C                   If        xxAmpo <> 0
     C                   move      *on           crtfrm
     C                   endif
    
     C                   endsr

      *=========================================================================
      * Query whether policy is small or large deductible
      *=========================================================================
     P rtvDedSize      b

     D rtvDedSize      pi             1                                         return S/L
     D  co#                                like(ELCO#)
     D  bscd                               like(ELBSCD)

     D kkco#           s                   like(BSCO#)
     D kkkey           s                   like(BSKEY)

     C     bsky          Klist
     C                   Kfld                    kkco#
     C                   Kfld                    kkkey

     C     rsky          Chain     prs01
     C                   If        not(%found)
     C                   Eval      RSJRST = *blank
     C                   Return    ' '
     C                   Endif

     C                   Eval      kkco# = co#
     C                   Movel(p)  bscd          kkkey
     C     bsky          Chain     pbs01
     C                   Select
     C                   When      BSLPGM = 'L'
     C                   Return    'L'
     C                   Other
     C                   Return    'S'
     C                   Endsl

     P rtvDedSize      e

      *=========================================================================
      * rtvSmDedInfo  - retrieves DDAMPO and DDAMAG
      *=========================================================================
     P rtvSmDedInfo    b
     D rtvSmDedInfo    pi
     D  ampo                               like(FMAM01)
     D  amag                               like(FMAM01)
     D  kkefdt         s                   like(DDEFDT)

     C     ddky1         Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    RSJRST
     C                   Kfld                    ELDCDE
     C                   Kfld                    kkefdt

     C     ddky          Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    RSJRST
     C                   Kfld                    ELDCDE

     C                   Move      ELCO#         qqco#
     C                   Move      ELFND         qqfnd
     C                   Move      ELEMP#        q1emp#
     C                   Move      ELDIV         q1div
     C                   Move      ELFYR         q1fyr
     C                   Move      ELDCEF        q2date

     C                   Call      'SREFFD'
     C                   Parm                    qqco#             3
     C                   Parm                    qqfnd             3
     C                   Parm                    q1fyr             3
     C                   Parm                    q1emp#            9
     C                   Parm                    q1div             5
     C                   Parm                    q2date            7

     C                   Move      q2date        kkefdt
     C     ddky1         Setgt     pdd01
     C     ddky          Readpe    pdd01
     C                   Dow       not(%eof) and DDSTAT <> 'A'
     C     ddky          Readpe    pdd01
     C                   Enddo

     C                   If        not(%eof)
     C                   Eval      ampo = DDAMPO
     C                   Eval      amag = DDAMAG
     C                   Endif

     P rtvSmDedInfo    e
      *=========================================================================
      * rtvLgDedInfo - retrieves LDAMPO and LDAMAG
      *=========================================================================
     P rtvLgDedInfo    b
     D rtvLgDedInfo    pi
     D  ampo                               like(FMAM01)
     D  amag                               like(FMAM01)

     C     elky          Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    ELEMP#
     C                   Kfld                    ELDIV
     C                   Kfld                    ELFYR

     C     elky          Chain     pld01
     C                   If        %found
     C                   Eval      ampo = LDAMP1
     C                   Eval      amag = LDAMG1
     C                   Endif

     P rtvLgDedInfo    e
