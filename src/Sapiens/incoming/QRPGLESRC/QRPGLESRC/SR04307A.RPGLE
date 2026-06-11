     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H bnddir('WTBNDDIR')
     *
    H/TITLE SR04307A - CA - Employee Leasing Endorsement
     *===============================================================*
     * WC040307A                                                     *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 06/09/04                                          *
     * Programmer: C:61                                              *
     * Project   : CM027                                             *
     *===============================================================*
     * REVISIONS:                                                    *
      *                                                               *
      * 10/28/22 C:333 Project: WC2834                                *
      *                Endorsement Recognition of Cancellation        *
      *                                                               *
      * 10/06/17 C:062 Project: 150WAP                                *
      *                Remove peo stuff from valid                    *
     *                                                               *
      * 07/21/17 C:180 Project: 150WAP                                *
      *                Handle groups with combined PEOs.              *
      *                                                               *
      * 02/27/17 C:180  Support: 45822                                *
      *                 Interface with Scriptura.                     *
     *                 Changes not commented.                        *
     *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: Please add comments to the top of revisions.           *
     * ------------------------------------------------------------- *
     *===============================================================*
     * Notes:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
     *                                                               *
     *  QQMODE = 1 - Called to determine if form should be attached. *
     *  QQMODE = 2 - Called to print endorsement.                    *
     *  QQMODE = 3 - Called for additional input.                    *
     *  QQMODE = 4 - Called for mid-term processing.                 *
     *===============================================================*
    
     ** Copybook F specs
     F/COPY SCOPYEN01F
    
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================
      * Entry
     D SR04307A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR04307A        PI
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
     D/COPY WPRPEO
      //***** Beg Del *** 10/28/22 ***********************************
     D*/Copy WprEmplr
      //***** Beg Del *** 10/28/22 ***********************************
    
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D QQFORM          c                   const('WC040307A ')
     d PeoZip          S             10    inz
     D ##emp#          s              9  0 inz
     D ##div           s              5  0 inz

      * Data structures

     D                 ds
     d Zip90                          9  0
     D  @@zip5                        5    overlay(Zip90)
     D  @@zip4                        4    overlay(Zip90:6)
    
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
    
    
     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
    
    
     *=========================================================================
     *  PLIST definitions
     *=========================================================================

    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
    
    
     *=========================================================================
     *  Main
     *=========================================================================
     c     emky1         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxemp#
    
     ** Copybook C specs
     C/COPY SCOPYEN01C
    
    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
      /Free
       Begsr Othinf;
          WriteInCopybook = *Off;

        //Contractor's name and address

          FM2003 = *blanks;
          FM2004 = *blanks;
          FMC401 = *blanks;
          FMC402 = *blanks;
          FMC403 = *blanks;

          ##emp# = 1;
          ##div = 0;
          Chain (xxco#:xxfnd:##emp#:##div) PEM01;

          If %found;
             FM2003 = emname + emnam2;
             FM2004 = emfdnm + emfdn2;

             If EMmpfl = 'P';
                FMC401 = EMpad1;
                FMC402 = EMpad2;
                Zip90  = EMpzip;
                PeoZip = *blanks;

                If @@Zip4 <> '0000';
                   PeoZip = @@Zip5 + '-' + @@Zip4;
                Else;
                   PeoZip = @@Zip5;
                Endif;

                FMC403 = %Trim(EMpcty) + ', ' + %trim(EMpst) + '  ' +
                         %Trim(PeoZip);
             Else;
                FMC401 = EMmad1;
                FMC402 = EMmad2;
                Zip90  = EMmzip;
                PeoZip = *blanks;

                If @@Zip4 <> '0000';
                   PeoZip = @@Zip5 + '-' + @@Zip4;
                Else;
                   PeoZip = @@Zip5;
                Endif;

                FMC403 = %Trim(EMmcty) + ', ' + %trim(EMmst) + '  ' +
                         %Trim(PeoZip);

             Endif;
          Endif;

          If FMC402 = *blanks;
             FMC402 = FMC403;
             FMC403 = *Blanks;
          Endif;

       //Get Client's information

       setll (xxco#:xxfnd:xxfyr:xxemp#) pel02;
       reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;

       Dow not %eof;
       //***** Beg Add *** 10/28/22 ******************************
          xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
          If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
       //***** End Add *** 10/28/22 ******************************
             Chain (Elco#:ELrtcd) prs01;

             Select;
             When FNpeof = 'M' and ELemp# = 1;
             When FNpeof = 'S' and ELdiv = 0;
             Other;
                If RSjrst = ENst;
                   If ELpfdt <> GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
                      dsCLTinfo = GetCltInfo(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);
                      FM2001 = dsCltInfo.fedName;
                      FM2002 = dsCltInfo.name;
                      FMead1 = dsCltInfo.address1;
                      FMead2 = dsCltInfo.address2;
                      FMead3 = dsCltInfo.address3;
                      Exsr WriteWorkFile;
                   Endif;
                Endif;
             Endsl;

        //***** Beg Add *** 10/28/22 ******************************
          Endif;
        //***** End Add *** 10/28/22 ******************************
          reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;
          Enddo;
       Endsr;
      /End-Free
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
      /Free
      /End-Free
     C                   endsr
    
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
      /Free
       //*** Begin Chg *** 10/06/17 *********************************************
       //Exsr Getfnd;
       //If FNpeof <> *blanks;
       ////*** Begin Add **** 07/21/17 ***************************************
       //   Select;
       //   //Non-Combined PEO Group - PEO client
       //   When FNpeof = 'S' and ELdiv <> *Zero;
       //      Crtfrm = *on;
       //
       //   //Non-Combined PEO Group - MCP client
       //   When FNpeof = 'M' and ELemp# <> 1;
       //      Crtfrm = *on;
       //
       //   // Combined PEO Group - Master PEO client
       //   When FNpeof = 'C' and ELemp# = 1 and ELdiv <> *Zero;
       //      Crtfrm = *on;
       //
       //   // Combined PEO Group - MCP client
       //   When FNpeof = 'C' and ELemp# <> 1;
       //      Crtfrm = *on;
       //   Endsl;
       ////*** End   Add **** 07/21/17 ***************************************
       //Endif;
         crtFrm = *On;
       //*** End   Chg *** 10/06/17 *********************************************
      /End-Free
     C                   endsr
