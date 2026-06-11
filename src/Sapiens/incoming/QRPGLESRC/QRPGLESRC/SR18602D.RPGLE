     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')

    H/TITLE WC180602.D - ME - Benefits Deductible Endorsement
     *========================================================================*
     * SR18602D    - Maine Benefits Deductible Print/Attach program           *
     *                                                                        *
     * Program Description:                                                   *
     *                                                                        *
     *   Determines if this endorsement form should be attached to            *
     *   the policy and creates data in the Endorsement Form                  *
     *   Workfile (WXFMP) prior to a form merge.                              *
     *                                                                        *
     * Created   : 08/23/10                                                   *
     * Programmer: C:190                                                      *
     * Project   : WC1935                                                     *
     *========================================================================*
     * REVISIONS:                                                             *
     *                                                                        *
     * 11/03/22 C:333 Project: WC2834                                         *
      *                Endorsement Recognition of Cancellation                 *
     *                                                                        *
     * 09/29/16 C:33  Project: SY371                                          *
      *               -Implement Scriptura.
      *               -No comments.
      *                                                                        *
     * 05/24/13 C:287 Support: WA099 80909                                    *
     *                - Add Large Deductible logic                            *
     *                                                                        *
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
    
     ** Copybook F specs
     F/COPY SCOPYEN01F
    
     * Deductible Discount Code
     Fwtddl2    if   e           k disk
     Fwtabl     if   e           k disk    include(pbs01)
     Fwmldl1    if   e           k disk

     *=========================================================================
     *  Definition Specs
     *=========================================================================

     ** Copybook D specs
     D/COPY SCOPYEN01D
    

      * employer effective dates
     d edds            ds
     d  emefd1
     d  emefd2
     d  emefd3
     d  emefd4
     d  emefd5
    d ed                                  OVERLAY(edds)
     d                                     LIKE(emefd1) DIM(5)

      * employer cancel dates
     d cdds            ds
     d  emcad1
     d  emcad2
     d  emcad3
     d  emcad4
     d  emcad5
    d cd                                  OVERLAY(cdds)
     d                                     LIKE(emcad1) DIM(5)

    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D qqForm          s             15a   inz('WC180602D ')
    
     ** Additional entry program
     d addpgm          c                   const('SRXXXXXXXX')
    
     D state           c                   const('ME')
    
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
    
      *entry parameters
     D SR18602D        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR18602D        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
    
     * Employer Master
    
     c     emky1         klist
     c                   KFld                    elco#
     c                   KFld                    elfnd
     c                   KFld                    elemp#
     c                   KFld                    eldiv
    
     c     ddky          klist
     c                   KFld                    xxco#
     c                   KFld                    xxfnd
     c                   KFld                    rsjrst
     c                   KFld                    eldcde
     c                   KFld                    elpfdt
    
     c     ddky1         Klist
     c                   KFld                    xxco#
     c                   KFld                    xxfnd
     c                   KFld                    rsjrst
     c                   KFld                    eldcde

     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs
     c/COPY SCOPYEN01C
    
    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     c     othinf        begsr
     *
     c     elky2         chain     pel02                              60
    
     c                   dow       *in60 = *off
       //***** Beg Add *** 11/03/22 ******************************
          xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
          If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
       //***** End Add *** 11/03/22 ******************************

     * Check that employer division is not cancelled

     c                   exsr      ck_emp_cx

     c                   if        emp_cx = 'N'

     c     rsky          chain     prs01                              60
     c                   if        *in60 = *off
     C                             and rsjrst = state
    
     c                   move      *blanks       ##am01           20

     C                   Eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     C                   Select
     C                   When      xxDedSize = 'L'
     C                   Callp     rtvLgDedInfo (xxAmpo: xxAmag)
     C                   When      xxDedSize = 'S'
     C                   Callp     rtvSmDedInfo (xxAmpo: xxAmag)
     C                   EndSl

     C                   If        xxAmpo <> 0
     c                   eval      ##am01 =  %editc(xxampo:'K':'$')             05/24/13
     c                   move      ##am01        fmc201
    
     c                   leave

     c                   endif                                                  *in60 = 0 & ddampo
     *
     c                   endif                                                  *in60 = *off
     *
     c                   endif                                                  emp_cx = 'N'
    
          //***** Beg Add *** 11/03/22 ******************************
          Endif;
          //***** End Add *** 11/03/22 ******************************
     c     elky2         reade     pel02                                  60

     c                   enddo                                                  dow(*in60 = *off)
    
     c                   endsr
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     c     pgmstr        begsr

     C                   time                    dsdttm
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         sysdat            7 0

     c                   endsr
    
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr
    
    
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr

      *  Policy must have Deductible Discount Code entered with
      *  non-zero  "Amount per Occurrence"

     C                   Eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     C                   Select
     C                   When      xxDedSize = 'L'
     C                   Callp     rtvLgDedInfo (xxAmpo: xxAmag)
     C                   When      xxDedSize = 'S'
     C                   Callp     rtvSmDedInfo (xxAmpo: xxAmag)
     C                   EndSl

     C                   If        xxAmpo <> 0

     * Check that employer division is not cancelled

     c                   exsr      ck_emp_cx


     c                   if        (emp_cx = 'N')
     c

     c                   move      *on           crtfrm

     C                   endif                                                  emp_cx = 'N'

     C                   endif                                                  *in60 = *off

     C                   endsr                                                  valid
    
     *=========================================================================
     *  CK_EMP_CX - Check if employer division is cancelled
     *=========================================================================
     c     ck_emp_cx     begsr

     c                   move      'Y'           emp_cx            1
     c     emky1         chain     pem01

     c                   if        %found

      *  Get Employer Effective/Cancellation date

     C                   z-add     5             x                 3 0

     c                   dow       (ed(x) = 0) and (x > 1)
     c                   sub       1             x
     c                   enddo

     c                   if        (cd(x) = 0) or (cd(x) > sysdat)
     c                   eval      emp_cx = 'N'
     c                   endif

     c                   endif

     c                   endsr

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

