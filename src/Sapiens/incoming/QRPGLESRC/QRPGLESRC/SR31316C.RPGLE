     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     //===============================================================*
    H/TITLE SR31316C - NY - Endorsement
     //===============================================================*
     // WC310316C - NY Sole Prop and Partners Exclusion Endorsement   *
     //                                                               *
     // PROGRAM DESCRIPTION:                                          *
     //                                                               *
     //   Determines if this endorsement form should be attached to   *
     //   the policy and creates data in the Endorsement Form         *
     //   Workfile (WXFMP) prior to a form merge.                     *
     //                                                               *
     // CREATED   : 03/27/20                                          *
     // PROGRAMMER: C:079                                             *
     // PROJECT   : WC2736                                           *
     //                                                               *
     //===============================================================*
     // REVISIONS:                                                    *
     //                                                               *
     // ------------------------------------------------------------- *
     //  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
     // ------------------------------------------------------------- *
     //===============================================================*
     // NOTES:                                                        *
     //                                                               *
     //  Utilizes the endorsement copybook (SCOPYEN01F/D/C).          *
     //                                                               *
     //  qqmode = 1 - Called to determine if form should be attached. *
     //  qqmode = 2 - Called to print endorsement.                    *
     //  qqmode = 3 - Called for additional input.                    *
     //  qqmode = 4 - Called for mid-term processing.                 *
     //===============================================================*
      //* Owner/Officer Master File
     Fwmool     if   e           k disk
      //* Additional Entity/Location File
     Fwmall     if   e           k disk

     //* Copybook F specs
     F/COPY SCOPYEN01F
     //=========================================================================
     //  Definition Specs
     //=========================================================================
     //* Copybook D specs
     D/COPY SCOPYEN01D
    
     D include         s              1
     D ix              s              2  0
     D iy              s              2  0
     D iz              s              2  0
     D x               s              2  0
     D y               s              2  0
     D z               s              2  0
     D xxNm200         s            200
     D xxTpOb          s              1  0

     D xxCompDate      s                   like(ELpfDt)

     D                 ds
     D  nm                     1   1000
     D                                     dim(5)
     D  FM2001                 1    200
     D  FM2002               201    400
     D  FM2003               401    600
     D  FM2004               601    800
     D  FM2005               801   1000
     D                 ds
     D  nm1                    1   1000
     D                                     dim(5)
     D  FM2006                 1    200
     D  FM2007               201    400
     D  FM2008               401    600
     D  FM2009               601    800
     D  FM2010               801   1000
     D                 ds
     D  nm2                    1   1000
     D                                     dim(5)
     D  FM2011                 1    200
     D  FM2012               201    400
     D  FM2013               401    600
     D  FM2014               601    800
     D  FM2015               801   1000
     //=========================================================================
     //  Copybook required constants
     //=========================================================================
     //* Form ID
     D qqForm          s             15a   inz('WC310316C ')
    
     //* Additional entry program
     D addPgm          c                   const('SRxxxxxxxx')

      //entry parameters
     D SR31316C        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                1
     D SR31316C        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qq316                         1
      // Needed for copybook
     D  qqRrn          s              9  0
     D  qqMod2         s              1
    
     //=========================================================================
     //  KLIST definitions
     //=========================================================================
    
     //* Owner/Officer Master file
     C     ooKey         klist
     C                   kfld                    ELco#
     C                   kfld                    ELfnd
     C                   kfld                    ELemp#
     C                   kfld                    ELdiv
     //* Additional Entity/Location file
     C     alKey         klist
     C                   kfld                    OOco#
     C                   kfld                    OOfnd
     C                   kfld                    OOemp#
     C                   kfld                    OOdiv
     C                   kfld                    OOelID
    
     //* Employer Master file
     C     emKey         klist
     C                   kfld                    ELco#
     C                   kfld                    ELfnd
     C                   kfld                    ELemp#
     C                   kfld                    ELdiv
     //=========================================================================
     //  Main
     //=========================================================================
     //* Copybook C specs
     C/COPY SCOPYEN01C

     //=========================================================================
     //  OTHINF - Subroutine to load form specific information
     //=========================================================================
      /FREE
       Begsr OthInf;
    
          WriteInCopybook = *Off;
      //
          ix = *zero;
          iy = *zero;
          iz = *zero;
          //
          Setll elKy2 pel02;
          Dou %Eof(WDELL1);
             Reade elKy2 pel02;
             If %Eof;
                Leave;
             Endif;
             Chain emKey pem01;
             // If type of business is Sole Prop or Partner, Then process
             //
             If EMtpOb = '1' or EMtpOb = '2' or EMtpOb = 'A' or EMtpOb = 'D';
                //
                Chain rsKy prs01;

                If RSjrSt = ENst;

                   //* Look for a qualifying "EXCLUDED" Owner/Officer record.
                   Setll ooKey poo01;
                   Reade ooKey poo01;
                   Dow not %Eof(WMOOL);

                      If OOexFl = 'E' and OOstat <> 'I';

                         //* If called in midterm mode, use the endorsement date to check against, o
                         //* use the policy effective date.
                         include = *off;
                         If qqMode = '4';
                            xxCompDate = LDeeDt;
                         Else;
                            xxCompDate = ELpfDt;
                         Endif;


                         If OOefDt <= xxCompDate and OOexDt > xxCompDate
                          or OOefDt <= xxCompDate and OOexDt = *zeros;
                            include = *on;
                         Endif;
                         //
                         If OOelID <> *blanks And OOelID <> *zeros;
                            Chain alKey pal01;
                            If ALdlDt <= xxCompDate;
                               include = *off;
                            Endif;
                         Endif;

                         If include = *on;

                            If EMtpOb = '1';

                               xxNm200 = %Trim(oooFsn) + ' ' + %Trim(oooLsn);
                               x = 1;
                               x = %Lookup(xxNm200:nm);
                               If x = 0;
                                  ix = ix + 1;
                                  nm(ix) = xxNm200;

      //* This is so the name will print correctly on the form
                                  If EMtpOb = '1';
                                     FMtoBc = '1';
                                  Endif;

                         //* If the page is full, write and setup for more names
                                  If ix = 5;
          //* Write the output record
                                     Exsr WriteWorkFile;
                                     Clear nm;
                                     ix = *zero;
                                  Endif;
                               Endif;

                            Endif;

                            If EMtpOb = '2';
                               xxNm200 = %Trim(oooFsn) + ' ' + %Trim(oooLsn);
                               y = 1;
                               y = %Lookup(xxNm200:nm1);
                               If y = 0;
                                  iy = iy + 1;
                                  nm1(iy) = xxNm200;

      //* If the page is full, write and setup for more names
                                  If iy = 5;
          //* Write the output record
                                     Exsr WriteWorkFile;
                                     Clear nm1;
                                     iy = *zero;
                                  Endif;
                               Endif;
                            Endif;

                            If EMtpOb = 'A' or EMtpOb = 'D';

                               xxNm200 = %Trim(oooFsn) + ' ' + %Trim(oooLsn);
                               z = 1;
                               z = %Lookup(xxNm200:nm2);
                               If z = 0;
                                  iz = iz + 1;
                                  nm2(iz) = xxNm200;

      //* If the page is full, write and setup for more names
                                  If iz = 5;
          //* Write the output record
                                     Exsr WriteWorkFile;
                                     clear nm2;
                                     iz = *zero;
                                  Endif;
                               Endif;
                            Endif;

                         Endif;

                      Endif;
                      Reade ooKey poo01;
                   Enddo;

                Endif;
             Endif;
          Enddo;

          //* Write the output record.
          If ix <> *zero or iy <> *zero or iz <> *zero;
          //* Write the output record
             Exsr WriteWorkFile;
          Endif;

          //
       Endsr;
       //=========================================================================
      //  PGMSTR - Beginning of program processing
      //=========================================================================
       Begsr PgmStr;
       Endsr;
      //=========================================================================
      //  PGMEND - End of program processing
      //=========================================================================
       Begsr PgmEnd;
       Endsr;
      //=========================================================================
      //  VALID - Subroutine to attach this form to the policy
      //=========================================================================
       Begsr Valid;
    
          // If type of business is Sole Prop or Partner, Then process
          //
          If EMtpOb = '1' or EMtpOb = '2' or EMtpOb = 'A' or EMtpOb = 'D';
             //
             //* Get Rate Set description
             If RSjrSt = ENst;

                //* Look for a qualifying "EXCLUDED" Owner/Officer record.
                Setll ooKey poo01;
                Reade ooKey poo01;

                dow not %Eof(WMOOL);

                   If OOexFl = 'E' and OOstat <> 'I';

                      //* If called in midterm mode, use the endorsement date to check against, othe
                      //* use the policy effective date.
                      If qqmode = '4';
                         qq316 = *off;
                         xxCompDate = LDeeDt;
                      Else;
                         xxCompDate = ELpfDt;
                      Endif;


                      If OOefDt <= xxCompDate and OOexDt > xxCompDate
                       or OOefDt <= xxCompDate and OOexDt = *zeros;


                         If OOelID <> *blanks and OOelID <> *zeros;
                            Chain alKey pal01;
                            If ALDLDT > xxCompDate or ALDLDT = *zeros;
                               crtFrm = *on;
                            Endif;
                         Else;
                            crtFrm = *on;
                         Endif;

                         If qqmode = '4';
                            qq316 = *on;
                         Endif;

                         Leave;
                      Endif;

                   Endif;

                   Reade ooKey poo01;
                Enddo;

             Endif;

          Endif;

       Endsr;
      /END-FREE
