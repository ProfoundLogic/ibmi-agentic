     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt) BNDDIR('WTBNDDIR')
     H DFTACTGRP(*no) ACTGRP(*caller)
    H/TITLE SR42602 - TX ACCIDENT DEDUCTIBLE ENDORSEMENT
     *========================================================================*
     * WC420602 - Texas Accident Deductible Endorsement                       *
     *                                                                        *
     * Program Description:                                                   *
     *                                                                        *
     *   Determines if this endorsement form should be attached to            *
     *   the policy and creates data in the Endorsement Form                  *
     *   Workfile (WXFMP) prior to a form merge.                              *
     *                                                                        *
     * Created   : 01/03/02                                                   *
     * Programmer: C:90                                                       *
     * Project   : CM027                                                      *
     *========================================================================*
     * REVISIONS:                                                             *
      *                                                                        *
      * 01/04/17 C:180 Project SY371 scriptura.
      *                Add Scriptura interface.
      *                                                                        *
     * 05/24/13 C:287 Support: WA099 80909                                    *
     *                - Add Large Deductible logic                            *
      *                                                                        *
      * 10/11/07 C:190 SUPPORT WA099 54504                                     *
      *                Not needed.                                             *
      *                                                                        *
      * 09/01/06 C:61  SUPPORT WA135 54494                                     *
      *              - Add Schedule to WC420602.                               *
      *              - Ignore inactive Deductible Discount Table               *
      *                records.                                                *
      *                                                                        *
      *  04/10/06 C:65 PROJECT SUPP#51293                                      *
      *                CHANGE DEDUCTIBLE DISCOUNT TO PULL BASED ON             *
      *                ANNIV RATING DATE INSTEAD OF POLICY EFF DATE.           *
     *                                                                        *
     *                                                                        *
     * -----------------------------------------------------------------------*
     *  NOTE: Please add comments to the top of revisions.                    *
     * -----------------------------------------------------------------------*
     *========================================================================*
     * Notes:                                                                 *
     *                                                                        *
     *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).                   *
     *                                                                        *
     *  QQMODE = 1 - Called to determine if form should be attached.          *
     *  QQMODE = 2 - Called to print endorsement.                             *
     *  QQMODE = 3 - Called for additional input.                             *
     *  QQMODE = 4 - Called for mid-term processing.                          *
     *========================================================================*
     ** Copybook F specs
     Fwtddl2    if   e           k disk
     F*** Begin Add ***** 05/24/13 *******************************************
     Fwtabl     if   e           k disk    include(pbs01)
     Fwmldl1    if   e           k disk
     F*** End Add ******* 05/24/13 *******************************************
     F/COPY SCOPYEN01F
     *=========================================================================
     *  Data structures and constants
     *=========================================================================
      * Entry
     D SR42602         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR42602         PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1
     ** Copybook D specs
     D/COPY SCOPYEN01D
     *
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
     ** Form ID
     D QQFORM          S             15A   inz('WC420602  ')
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
     *** Begin Add ***** 05/24/13 *******************************************
     D xxampo          s                   like(FMAM01)
     D xxamag          s                   like(FMAM01)
     D xxDedSize       s              1
     *=========================================================================
     *  Prototypes
     *=========================================================================
    D rtvDedSize      pr             1
     D  co#                                like(ELCO#)
     D  bscd                               like(ELBSCD)
    D rtvSmDedInfo    pr
     D  ampo                               like(FMAM01)
     D  amag                               like(FMAM01)
    D rtvLgDedInfo    pr
     D  ampo                               like(FMAM01)
     D  amag                               like(FMAM01)
      *** End Add ****** 05/24/13 *******************************************
     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
     *
     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     *=========================================================================
     *  KLIST definitions
     *=========================================================================
     C     ddky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    rsjrst
     C                   kfld                    eldcde
     C*** BEGIN CHG ***  04/10/06  ************************************
     C*************      KFLD                    ELPFDT
     C                   KFLD                    XXDDT             7 0
     C*** END CHG *****  04/10/06  ************************************
    C*
     C     ddky1         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    rsjrst
     C                   kfld                    eldcde
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs
     C/COPY SCOPYEN01C
     *
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
    C*
     C     elky2         setll     pel02
     C     elky2         reade     pel02                                  60
    C*
     C                   dow       *in60 = *off
    C*
     C     rsky          chain     prs01                              60
     C*** BEGIN CHG ***  04/10/06  ************************************
     C***Begin Delete ***** 05/24/13 *********************************
     C***                MOVE      ELDCEF        Q1DATE
     C***                MOVE      ELFYR         Q1FYR
     C***                MOVE      ELEMP#        Q1EMP#
     C***                MOVE      ELDIV         Q1DIV

     C***                CALL      'SREFFD'
     C***                PARM                    QQCO#             3
     C***                PARM                    QQFND             3
     C***                PARM                    Q1FYR             3
     C***                PARM                    Q1EMP#            9
     C***                PARM                    Q1DIV             5
     C***                PARM                    Q1DATE            7

     C***                MOVE      Q1DATE        XXDDT
     C*** END CHG *****  04/10/06  ************************************
    C*
     C***  ddky          setgt     pdd01
     C***  ddky1         readpe    pdd01                                  60
      ******* beg add ****** 09/01/06 *********************************

     c***                dow       (not %eof) and (ddstat='I')
     C***  ddky1         readpe    pdd01                                  60
     c***                enddo

      ******* end add ****** 09/01/06 *********************************
    C*
    C*  If a record is found and there is a per occurrence deductible ,
    C*  then print that deductible on the endorsement
    C*
     C***                if        *in60 = *off
     C***                          and ddampo <> *zero
     C***                eval      fma701 = ddampo
     C***End Delete   ***** 05/24/13 *********************************
     *
     C***Begin Add ****** 05/24/13 ***********************************
     C                   Eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     C                   Select
     C                   When      xxDedSize = 'L'
     C                   Callp     rtvLgDedInfo (xxAmpo: xxAmag)
     C                   When      xxDedSize = 'S'
     C                   Callp     rtvSmDedInfo (xxAmpo: xxAmag)
     C                   EndSl

     C                   If        xxAmpo <> 0
     C                   Eval      fma701 = xxAmpo
     C***End Add     ***** 05/24/13 *********************************
     ** Write the output record
      /Free
                         Exsr      WriteWorkFile;
      /End-Free
     C                   leave
     *
     C                   endif
     *
     C     elky2         reade     pel02                                  60
     C                   enddo
     *
     C                   endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
      /Free
       WriteInCopyBook = *Off;
      /End-Free
     *
     C                   endsr
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     *
     *
     C                   endsr
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
     *
     C     rsky          chain     prs01                              60
     *
     C                   if        *in60 = *off
     C                             and rsjrst = enst
     *
     ** Found a division with a matching jurisdiction state
     *
     C*** BEGIN CHG ***  04/10/06  ************************************
     C***Begin Delete ***** 05/24/13 *********************************
     C***                MOVE      ELDCEF        Q1DATE
     C***                MOVE      ELFYR         Q1FYR
     C***                MOVE      ELEMP#        Q1EMP#
     C***                MOVE      ELDIV         Q1DIV

     C***                CALL      'SREFFD'
     C***                PARM                    QQCO#             3
     C***                PARM                    QQFND             3
     C***                PARM                    Q1FYR             3
     C***                PARM                    Q1EMP#            9
     C***                PARM                    Q1DIV             5
     C***                PARM                    Q1DATE            7

     C***                MOVE      Q1DATE        XXDDT
     C*** END CHG *****  04/10/06  ************************************
     C***  ddky          setgt     pdd01
     C***  ddky1         readpe    pdd01                                  60
      ******* beg add ****** 09/01/06 *********************************

     c***                dow       (not %eof) and (ddstat='I')
     C***  ddky1         readpe    pdd01                                  60
     c***                enddo

      ******* end add ****** 09/01/06 *********************************
    C*
    C*  If a record is found and there is a per occurrence deductible ,
    C*  then include the endorsement
    C*
     C***                if        *in60 = *off
     C***                          and ddampo <> *zero
     C***End Delete ***** 05/24/13 *********************************
     C*
     C***Start Add ***** 05/24/13 *************************************
     C                   Eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     C                   If        ENST = RSJRST
     C                   Select
     C                   When      xxDedSize = 'L'
     C                   Callp     rtvLgDedInfo (xxAmpo: xxAmag)
     C                   When      xxDedSize = 'S'
     C                   Callp     rtvSmDedInfo (xxAmpo: xxAmag)
     C                   Endsl
     C                   If        xxAmpo <> 0
     C***End Add ***** 05/24/13 ***************************************
     C                   move      *on           crtfrm
     C                   endif
     C                   endif
    C*
     C                   endif
     *
     C                   endsr

     C***Begin Add ***** 05/24/13 **************************************

     *=========================================================================
     * Query whether policy is small or large deductible
     *=========================================================================
    P rtvDedSize      b

    D rtvDedSize      pi             1                                         return S/L
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

    P rtvDedSize      e

     *=========================================================================
     * rtvSmDedInfo  - retrieves DDAMPO and DDAMAG
     *=========================================================================
    P rtvSmDedInfo    b
    D rtvSmDedInfo    pi
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

    P rtvSmDedInfo    e
     *=========================================================================
     * rtvLgDedInfo - retrieves LDAMPO and LDAMAG
     *=========================================================================
    P rtvLgDedInfo    b
    D rtvLgDedInfo    pi
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

    P rtvLgDedInfo    e

      ***End Add ***** 05/24/13 ***********************************************
