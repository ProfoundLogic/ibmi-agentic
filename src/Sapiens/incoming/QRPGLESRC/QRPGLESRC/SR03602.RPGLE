     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR03602 - AR ARKANSAS BENEFITS DEDUCTIBLE ENDORSEMENT
      *===============================================================*
      * WC030602A - Arkansas Benefits Deductible Endorsement          *
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
      * 10/27/22 C:333 Project: WC2834                                *
      *                Endorsement Recognition of Cancellation        *
      *                                                               *
      * 01/30/17 C:180 Project SY371                                  *
      *                Add Medical and Comp columns for Scriptura     *
      *                only. Per CG - Do not change DTM form.         *
      *                                                               *
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
      /COPY SCOPYEN01F

     Fwtddl2    if   e           k disk
     Fwtabl     if   e           k disk    include(pbs01)
     Fwmldl1    if   e           k disk

      /COPY SCOPYEN01D

     D qqform          s             15A   Inz('WC030602A ')
     D addpgm          c                   const('SRXXXXXXXX')
     D xxampo          s                   like(FMAM01)
     D xxamag          s                   like(FMAM01)
     D xxDedSize       s              1
     D xxCod1          s              2A
     D xxCod2          s              2A
     D                 DS
     D  DDC                    1     20
     D                                     DIM(10)
     D  DDDC01                 1      2
     D  DDDC02                 3      4
     D  DDDC03                 5      6
     D  DDDC04                 7      8
     D  DDDC05                 9     10
     D  DDDC06                11     12
     D  DDDC07                13     14
     D  DDDC08                15     16
     D  DDDC09                17     18
     D  DDDC10                19     20

     D rtvDedSize      pr             1
     D  co#                                like(ELCO#)
     D  bscd                               like(ELBSCD)
     D rtvSmDedInfo    pr
     D  ampo                               like(FMAM01)
     D  amag                               like(FMAM01)
     D  acod1                         2A
     D  acod2                         2A
     D rtvLgDedInfo    pr
     D  ampo                               like(FMAM01)
     D  amag                               like(FMAM01)
     D  acod1                         2A
     D  acod2                         2A

     D SR03602         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR03602         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

     C     ddky          KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    rsjrst
     C                   KFld                    eldcde
     C                   KFLD                    XXDDT             7 0
     C     ddky1         KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    rsjrst
     C                   KFld                    eldcde
      /COPY SCOPYEN01C
      *=========================================================================
      *  OTHINF - Subroutine to load form specific information & write record
      *=========================================================================
     C     othinf        begsr
     C     elky2         chain     pel02                              60

     C                   dow       *in60 = *off
          //***** Beg Add *** 10/27/22 ******************************
             xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
             If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
          //***** End Add *** 10/27/22 ******************************
     C     rsky          chain     prs01                              60
     C                   if        *in60 = *off
     C                             and rsjrst = enst
     C                   Eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     C                   Select
     C                   When      xxDedSize = 'L'
     C                   Callp     rtvLgDedInfo (xxAmpo: xxAmag: xxCod1: xxCod2)
     C                   When      xxDedSize = 'S'
     C                   Callp     rtvSmDedInfo (xxAmpo: xxAmag: xxCod1: xxCod2)
     C                   EndSl

     C                   If        xxAmpo <> 0
      ** If a record is found and there is a per occurrence deductible
      ** then print that deductible on the endorsement.
      /Free

          fma701 = *zeros;
          fma702 = *zeros;
          fma703 = *zeros;

          Select;
          When xxCod1 > *blanks and xxCod2 > *blanks;
             fma701 = xxAmpo;
          When xxCod1 > *blanks;
             fma703 = xxAmpo;
          When xxCod2 > *blanks;
             fma702 = xxAmpo;
          Endsl;
      /End-Free
     C****** 01/30/17    eval      fma701 = xxAmpo
     C                   leave
     C                   endif
     C                   endif
           //***** Beg Add *** 10/27/22 ******************************
             Endif;
           //***** End Add *** 10/27/22 ******************************
     C     elky2         reade     pel02                                  60
     C                   enddo
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
     C     rsky          chain     prs01                              60
     C                   if        *in60 = *off
     C                             and rsjrst = enst
     C                   Eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     C                   If        ENST = RSJRST
     C                   Select
     C                   When      xxDedSize = 'L'
     C                   Callp     rtvLgDedInfo (xxAmpo: xxAmag: xxCod1: xxCod2)
     C                   When      xxDedSize = 'S'
     C                   Callp     rtvSmDedInfo (xxAmpo: xxAmag: xxCod1: xxCod2)
     C                   Endsl
     C                   If        xxAmpo <> 0
     C                   Eval      crtfrm = *on
     C                   Endif
     C                   Endif
     C                   endif
     C                   endsr

      *=========================================================================
      * Query whether policy is small or large deductible
      *=========================================================================
     P rtvDedSize      b

     D rtvDedSize      pi             1
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
     D  acod1                         2a
     D  acod2                         2a
     D  kkefdt         s                   like(DDEFDT)
     D  ix             s              3  0 inz

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
      /Free
        acod1 = *blanks;
        acod2 = *blanks;
        For ix = 1 to 10;
           Select;
           When DDC(ix) = 'CO' and acod1 = *blanks;
             acod1 = DDC(ix);
           When DDC(ix) = 'ME' and acod2 = *blanks;
             acod2 = DDC(ix);
           Endsl;
        Endfor;
      /End-Free
     C                   Endif

     P rtvSmDedInfo    e
      *=========================================================================
      * rtvLgDedInfo - retrieves LDAMPO and LDAMAG
      *=========================================================================
     P rtvLgDedInfo    b
     D rtvLgDedInfo    pi
     D  ampo                               like(FMAM01)
     D  amag                               like(FMAM01)
     D  acod1                         2a
     D  acod2                         2a
     D  ix             s              3  0 inz
     D                 DS
     D  LDR                    1     20
     D                                     DIM(10)
     D  LDR101                 1      2
     D  LDR102                 3      4
     D  LDR103                 5      6
     D  LDR104                 7      8
     D  LDR105                 9     10
     D  LDR106                11     12
     D  LDR107                13     14
     D  LDR108                15     16
     D  LDR109                17     18
     D  LDR110                19     20

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
      /Free
        acod1 = *blanks;
        acod2 = *blanks;
        For ix = 1 to 10;
           Select;
           When LDR(ix) = 'CO' and acod1 = *blanks;
             acod1 = LDR(ix);
           When LDR(ix) = 'ME' and acod2 = *blanks;
             acod2 = LDR(ix);
           Endsl;
        Endfor;
      /End-Free
     C                   Endif

     P rtvLgDedInfo    e
