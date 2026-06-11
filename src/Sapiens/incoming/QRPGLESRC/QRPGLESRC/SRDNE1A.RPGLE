     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SRDNE1A - TX form DNE-1A
      //===============================================================*
      // Created on 05/09/2018 by C:62 for Project 150WCM              *
      //===============================================================*
      // Revisions:                                                    *
      //                                                               *
      // 10/15/22 C:333 Project: WC2834 (Recompile)                    *
      //                Endorsement Recognition of Cancellation        *
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
     fwtabl     if   e           k disk    include(pbs01)
     fwmldl1    if   e           k disk

      /COPY SCOPYEN01D

      /COPY WWCPOLDAT4

      *=========================================================================
      *  Prototypes
      *=========================================================================
     D SRDNE1A         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SRDNE1A         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

     D RtvDedType      pr             1                                         Returns S/L
     D  co#                                like(ELco#)
     D  bscd                               like(ELbsCd)
      *=========================================================================
      *  Stand alone
      *=========================================================================
     D qqForm          s             15A   Inz('DNE-1A    ')

     D xxampo          s                   like(FMAM01)
     D xxamag          s                   like(FMAM02)

      *=========================================================================
      *  Constants
      *=========================================================================
     D addpgm          c                   const('SRXXXXXXXX')

      /COPY SCOPYEN01C

      /Free
       //=========================================================================
       // OTHINF - Subroutine to load form specific information & write record
       //=========================================================================
       Begsr OthInf;


       Endsr;

       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       Begsr Valid;

         Chain (xxCo#:ELrtCd) prs01;
         If %Found and (RSjrSt = ENst Or ENst = *Blanks);
            If RtvDedType(ELco#:ELbscd) <> 'L';
               crtFrm = *On;
            Endif;
         Endif;

       Endsr;
       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr PgmStr;
          RunningSRText   = *On;
       Endsr;
       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       Begsr PgmEnd;
       Endsr;
      /End-Free
      *=========================================================================
      * Query whether policy is small or large deductible
      *=========================================================================
     p RtvDedType      b

     d RtvDedType      pi             1
     d  co#                                like(ELco#)
     d  bscd                               like(ELbsCd)

     d kkCo#           s                   like(BSco#)
     d kkKey           s                   like(BSkey)

      /Free
       Chain (ELco#:ELrtcd) prs01;
       If %found;
          kkco# = co#;
          kkkey = %editc(bscd:'X');
          Chain (kkco#:kkkey) pbs01;

          If %found;
             Select;
             When BSlpgm = 'L';
                Return 'L';
             Other;
                Return 'S';
             Endsl;
          Else;
               Return *blank;
          Endif;

       Else;
          Rsjrst = *blanks;
          Return *blank;
       Endif;
      /End-Free

     p RtvDedType      e
