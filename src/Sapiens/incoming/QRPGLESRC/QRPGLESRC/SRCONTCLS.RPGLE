     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
      /TITLE SRCONTCLS - Contractor Class Endorsement print/attach
      //===============================================================*
      //                                                               *
      // Program Description:                                          *
      //                                                               *
      // Any Contractor Class Form                                     *
      //                                                               *
      // Print/Attachment program for Contractor Class Endorsement.    *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      //===============================================================*
      // Created on 07/19/2018 by C:291 for Project WC2627             *
      //===============================================================*
      // REVISIONS:                                                    *
      //                                                               *
      // 08/21/25 C:333 Support: 148/117703  (Recompile)               *
      //                For single print endorsements provide specific *
      //                group and employer so that correct sig is found*
      //                                                               *
      // 10/31/22 C:333 Project: WC2834 (Recompile)                    *
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

      //* Copybook F specs
     f/COPY SCOPYEN01F
     fwmepl     if   e           k disk
     Fwmprl1    if   e           k disk

      //* Copybook D specs
      /COPY SCOPYEN01D

      //* Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
     D  qqRrn          s              9  0
     D  qqMod2         s              1
     D #1pgm           s             10A   INZ('W1054R')
     D #1flag          s              1A   INZ(' ')

     D SRCONTCLS       PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                               15

     D SRCONTCLS       PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqForm                       15

     D ChkProgInSt     pr                       ExtPgm('SRCHKPGM')
     D                               10
     D                                1

      //* Copybook C specs
     C/COPY SCOPYEN01C

      /Free
       //=========================================================================
       //  OTHINF - Subroutine to load form specific information & write record
       //=========================================================================

       begsr othinf;

          //If printing a policy page.  (Not called from W1054R)
          ChkProgInSt(#1Pgm:#1Flag);
          If #1Flag = '0';
             PrintingPolicy = *On;
          Else;
             PrintingPolicy = *Off;
          Endif;

       endsr;

       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       begsr pgmstr;
          RunningSRtext = *On;
       endsr;

       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       begsr pgmend;
       endsr;

       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       begsr valid;
          crtfrm = *Off;
          If ELpfDt <> ELptDt and ELasp1 = 0;
             Setll (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pep01;
             Reade (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pep01;
             Dow Not %Eof(wmepl);
                If EPstat <> 'I';
                   Setgt (ELco#:ELrtCd:EPClas:ELpfDt) ppr01;
                   Readpe (ELco#:ELrtCd:EPClas) ppr01;
                   If Not %Eof(wmprl1) and PRccFl = 'Y';
                      crtFrm = *On;
                      Leave;
                   Endif;
                Endif;
                Reade (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pep01;
             Enddo;
          Endif;
       endsr;
      /End-Free
