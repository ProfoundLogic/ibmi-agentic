     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR00306 - MEDICAL BENEFITS EXCLUSION ENDORSEMENT
      //===============================================================*
      // WC000306 - Medical Benefits Exclusion Endorsement             *
      //                                                               *
      // PROGRAM DESCRIPTION:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      //===============================================================*
      // REVISIONS:                                                    *
      //                                                               *
      // 01/27/23 C:291 Project: WC2863                                *
      //                State Include/Exclude Endorsement Print Control*
      //                                                               *
      // 10/19/22 C:333 Project: WC2834                                *
      //                Endorsement Recognition of Cancellation        *
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      //                                                               *
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

     D yst             s              2    dim(25)
     D i2              s              3  0

      /COPY SCOPYEN01D

      //* Form ID
     D qqForm          s             15A   Inz('WC000306  ')

      //* Additional entry program
     D addpgm          c                   const('SR00000I  ')

     D SR00306         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR00306         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      //=========================================================================
      //  Main
      //=========================================================================

      /COPY SCOPYEN01C

      //=========================================================================
      //  OTHINF - Subroutine to load form specific information
      //=========================================================================
      /Free
       BegSr OthInf;

          i2 = 1;

          Setll (xxCo#:xxFnd:xxEmp#) wdell;
          Reade (xxCo#:xxFnd:xxEmp#) wdell;

          Dow Not %EOF(wdell);

          //***** Beg Add *** 10/19/22 ******************************
             xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
             If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
          //***** End Add *** 10/19/22 ******************************

                Chain (xxCo#:ELrtcd) wmrsl;

                If %Found and RSjrst <> *Blanks;
                   //*** Beg Add ***** 01/27/23 *****************************
                   Exsr ChkApprExclSt;
                   If isApprExclSt = *On;
                   //*** End Add ***** 01/27/23 *****************************

                      i = 1;

                      If ELexmf = 'Y';

                         If %lookup(RSjrst:yst) = *Zeros;
                            yst(i2) = RSjrst;
                            i2 = i2 + 1;
                         Endif;

                      Endif;
                   //*** Beg Add ***** 01/27/23 *****************************
                   Endif;
                   //*** End Add ***** 01/27/23 *****************************

                Endif;

        //***** Beg Add *** 10/19/22 ******************************
             Endif;
        //***** End Add *** 10/19/22 ******************************
             Reade (xxCo#:xxFnd:xxEmp#) wdell;
          Enddo;

          FMc401 = yst(1);
          FMc402 = yst(2);
          FMc403 = yst(3);
          FMc404 = yst(4);
          FMc405 = yst(5);
          FMc406 = yst(6);
          FMc407 = yst(7);
          FMc408 = yst(8);
          FMc409 = yst(9);
          FMc410 = yst(10);
          FMc411 = yst(11);
          FMc412 = yst(12);
          FMc413 = yst(13);
          FMc414 = yst(14);
          FMc415 = yst(15);
          FMc416 = yst(16);
          FMc417 = yst(17);
          FMc418 = yst(18);
          FMc419 = yst(19);
          FMc420 = yst(20);
          FMc421 = yst(21);
          FMc422 = yst(22);
          FMc423 = yst(23);
          FMc424 = yst(24);
          FMc425 = yst(25);

       Endsr;

       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr pgmstr;
       Endsr;
       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       Begsr pgmend;
       Endsr;
       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       Begsr valid;

          If ELexmf = 'Y';
             crtFrm = *On;
          Endif;

       Endsr;
      /End-Free
