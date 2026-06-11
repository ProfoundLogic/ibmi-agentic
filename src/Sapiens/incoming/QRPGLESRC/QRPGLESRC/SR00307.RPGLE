     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR00307 -  Medical Benefits Reimbursement                *
      //===============================================================*
      // WC000307 - Medical Benefits Reimbursement Endorsement         *
      //                                                               *
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      // Created   : 08/19/10                                          *
      // Programmer: C:190                                             *
      // Project   : WC1935                                            *
      //===============================================================*
      // REVISIONS:                                                    *
      //                                                               *
      // 01/27/23 C:291 Project: WC2863                                *
      //                State Include/Exclude Endorsement Print Control*
      //                                                               *
      // 10/19/22 C:342 Project: WC2834 (Recompile)                    *
      //                Endorsement Recognition of Cancellation        *
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      //                                                               *
      // 03/28/2011 C:061 wc1935 seq74830
      //                - Change to list states of coverage where      *
      //                  wdelp/elexmf <> 'Y'.                         *
      //                - Changes not commented.                       *
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
     F/COPY SCOPYEN01F

      //=========================================================================
      //  Definition Specs
      //=========================================================================

      //* Copybook D specs
     D/COPY SCOPYEN01D

      // employer effective dates
     d edds            ds
     d  emefd1
     d  emefd2
     d  emefd3
     d  emefd4
     d  emefd5
     d ed                                  OVERLAY(edds)
     d                                     LIKE(emefd1) DIM(5)

      // employer cancel dates
     d cdds            ds
     d  emcad1
     d  emcad2
     d  emcad3
     d  emcad4
     d  emcad5
     d cd                                  OVERLAY(cdds)
     d                                     LIKE(emcad1) DIM(5)

     d j1              s              3  0
     d stsav           s              2A
     d sttmp           s              2A
     d emp_cx          s              1A
     d x               s              3  0

      //* Form ID
     d qqform          s             15A   Inz('WC000307  ')

      //* Additional entry program
     d addpgm          c                   const('SRXXXXXXXX')

     d  fm2ds          ds
     dfm2001
     dfm2002
     dfm2003
     dfm2004
     dfm2005
     dfm2006
     dfm2007
     dfm2008
     dfm2009
     dfm2010
     dfm2011
     dfm2012
     dfm2013

     d fm2                                 OVERLAY(fm2ds)
     d                                     LIKE(fm2001) DIM(13)

     d                 ds
     d  st                                 like(rsjrst)
     d                                     dim(52) ASCEND

     D SR00307         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR00307         PI
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
      //* Copybook C specs
     c/COPY SCOPYEN01C

      //=========================================================================
      //  OTHINF - Subroutine to load form specific information & write record
      //=========================================================================
      /FREE
       Begsr othinf;

          Setll (xxCo#:xxFnd:xxfyr:xxEmp#) wdell1;
          Reade (xxCo#:xxFnd:xxfyr:xxEmp#) wdell1;

          Dow Not %EOF(wdell1);

             // Check that employer division is not cancelled

             Exsr ck_emp_cx;

             If (emp_cx = 'N') And (elexmf <> 'Y');
                Chain (xxCo#:elrtcd) wmrsl;

                If %Found(wmrsl);
                   //*** Beg Add ***** 01/27/23 *********************
                   Exsr ChkApprExclSt;
                   If isApprExclSt = *On;
                   //*** End Add ***** 01/27/23 *********************
                      stsav = RSjrst;
                      Exsr srins;
                   //*** Beg Add ***** 01/27/23 *********************
                   Endif;
                   //*** End Add ***** 01/27/23 *********************
                Endif;

             Endif;

             Reade (xxCo#:xxFnd:xxfyr:xxEmp#) wdell1;
          Enddo;

          //* Write the output record
          Exsr srout;
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
          // Check that employer division is not cancelled
          // and medical is not excluded.
          Exsr ck_emp_cx;

          If (emp_cx = 'N') And (ELexmf <> 'Y');
             crtfrm = *On;
          Endif;

       Endsr;

       //=========================================================================
       //  CK_EMP_CX - Check if employer division is cancelled
       //=========================================================================
       Begsr ck_emp_cx;

          emp_cx = 'Y';
          chain (ELco#:ELfnd:ELemp#:ELdiv) wmeml;

          If %Found(wmeml);

             //  Get Employer Effective/Cancellation date
             x = 5;
             Dow (ed(x) = 0) and (x > 1);
                x = x - 1;
             Enddo;

             If (cd(x) = 0) Or (cd(x) > sysdat);
                emp_cx = 'N';
             Endif;

          Endif;

       Endsr;

       //=========================================================================
       //  srins - Insertion sort
       //=========================================================================
       Begsr srins;

          x = 1;

          Dow x <= 52;

             Select;

             When st(x) = *blanks;
                st(x) = stsav;
                Leave;

             When stsav > st(x);
                x = x + 1;
                Iter;

             When stsav = st(x);
                Leave;

             Other;
                sttmp = st(x);
                st(x) = stsav;
                stsav = sttmp;
                x = x + 1;
                Iter;

             Endsl;

          EndDo;

       Endsr;

       //=========================================================================
       // SROUT - Move data from arrays to output fields
       //=========================================================================
       begsr srout;

          // Assemble schedule of states in 4 columns.
          x = 1;
          Dow x <= 52;
             j1 = %Rem(x:13);

             If st(x) = *blank;
                Leave;
             Endif;

             fm2(j1) = %trimr(fm2(j1)) + '        ' + st(x);
             X = X + 1;
          Enddo;

       Endsr;

      /End-free
