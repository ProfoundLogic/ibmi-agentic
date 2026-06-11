     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR14603 - IOWA Deductible Endorsement - Carrier Reimbursed
      //                Deductible Endorsement
      //===============================================================*
      // WC140603  - IA Medical Program - Carrier Reimbursed Deductible*
      //             Endorsement                                       *
      //                                                               *
      // PROGRAM DESCRIPTION:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      //===============================================================*
      // Revisions:                                                    *
      *                                                                *
      * 11/02/22 C:333 Project: WC2834                                 *
      *                Endorsement Recognition of Cancellation         *
      *                                                                *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      // ------------------------------------------------------------- *
      //  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
      // ------------------------------------------------------------- *
      //===============================================================*
      // NOTES:                                                        *
      //                                                               *
      //  Utilizes the endorsement copybook (SCOPYEN01F/D/C).          *
      //                                                               *
      //  qqmode = 1 - Called to determine if form should be attached. *
      //  qqmode = 2 - Called to print endorsement.                    *
      //  qqmode = 3 - Called for additional input.                    *
      //  qqmode = 4 - Called for midterm processing.                  *
      //===============================================================*

      /COPY SCOPYEN01F

     fwtddl2    if   e           k disk
     Fwtabl     if   e           k disk    include(pbs01)
     Fwmldl1    if   e           k disk

      /COPY SCOPYEN01D

     d intDdt          s              7  0
     d strFyr          s              3
     d strEmp#         s              9
     d strDiv          s              5
     d strDate         s              7
     d qqForm          s             15A   Inz('WC140603 ')
     d addpgm          c                   const('SRXXXXXXXX')

     D xxampo          s                   like(FMAM01)
     D xxamag          s                   like(FMAM01)
     D xxDedSize       s              1

     D rtvDedSize      pr             1
     D  co#                                like(ELCO#)
     D  bscd                               like(ELBSCD)
     D rtvSmDedInfo    pr
     D  ampo                               like(FMAM01)
     D  amag                               like(FMAM01)
     D rtvLgDedInfo    pr
     D  ampo                               like(FMAM01)
     D  amag                               like(FMAM01)

     d GetEffDat       pr                  extPgm('SREFFD')
     d qqCO#                          3
     d qqFND                          3
     d strFyr                         3
     d strEmp#                        9
     d strDiv                         5
     d strDate                        7

     D SR14603         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR14603         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

     c     keyWTDDL2     KList
     c                   KFld                    xxco#
     c                   KFld                    xxfnd
     c                   KFld                    rsjrst
     c                   KFld                    eldcde
     c                   KFLD                    intDdt
     c     key1WTDDL2    KList
     c                   KFld                    xxco#
     c                   KFld                    xxfnd
     c                   KFld                    rsjrst
     c                   KFld                    eldcde

      /COPY SCOPYEN01C

      /Free

       //=========================================================================
       //  OTHINF - Subroutine to load form specific information & write record
       //=========================================================================

        Begsr Othinf;

        Setll elky2  pel02;
        ReadE elky2  pel02;
           Dow Not %EOF;
       //***** Beg Add *** 11/02/22 ******************************
              xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
              If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
       //***** End Add *** 11/02/22 ******************************
                 xxDedSize = rtvDedSize (ELCO#:ELBSCD) ;
                 Select ;
                 When      xxDedSize = 'L' ;
                    Callp     rtvLgDedInfo (xxAmpo : xxAmag ) ;
                 When      xxDedSize = 'S' ;
                    Callp     rtvSmDedInfo (xxAmpo : xxAmag ) ;
                 EndSl ;

                 If xxampo <> 0 ;
                    FMa701 = xxampo;
                    Leave;
                 Endif;

          //***** Beg Add *** 11/02/22 ******************************
              Endif;
          //***** End Add *** 11/02/22 ******************************
              ReadE elky2 pel02;
           Enddo;

       Endsr;
       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr PgmStr;
       Endsr;
       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       Begsr PgmEnd;
       Endsr;
       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       Begsr Valid;

          xxDedSize = rtvDedSize (ELCO#:ELBSCD) ;
          Select ;
          When xxDedSize = 'L' ;
           Callp rtvLgDedInfo (xxAmpo : xxAmag ) ;
          When xxDedSize = 'S' ;
           Callp rtvSmDedInfo (xxAmpo : xxAmag ) ;
          EndSl ;

           If xxampo <> 0 ;
              FMa701 = xxampo;
              crtfrm = *on;
           Endif;

        EndSr;
      /End-free
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
