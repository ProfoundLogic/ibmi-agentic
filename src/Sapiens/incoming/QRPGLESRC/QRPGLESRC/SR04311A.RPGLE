     hCopyright('(c) 2022 Sapiens. All Rights Reserved.')

    H/TITLE SR04311A - CA - Employee Leasing Endorsement - Short Form
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
     * WC040311A                                                     *
     *  (For Leasing Arrangements that do not fall within the scope  *
     *   of the Employee Leasing Rule)                               *
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
      * 07/25/17 C:180 Project: 150WAP                                *
      *                Handle groups with combined PEOs               *
      *                                                               *
     * 03/02/17 C:180 Project: SY371                                 *
      *                Add PEO client/contractor info                 *
      *                No comments                                    *
      *                                                               *
     * 09/15/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.                           *
      *               -No comments.                                   *
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

     ** Copybook D specs
     D/COPY SCOPYEN01D
     D/COPY WPRPEO
      //***** Beg Del *** 10/28/22 **********************************
     D*/Copy WprEmplr
      //***** End Del *** 10/28/22 **********************************
    
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D qqForm          s             15a   inz('WC040311A ')
    
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
    
     *entry parameters
     D SR04311A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR04311A        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
    
     d PeoZip          S             10    inz
     D ##emp#          s              9  0 inz
     D ##div           s              5  0 inz


      * Data structures

     D                 ds
     d Zip90                          9  0
     D  @@zip5                        5    overlay(Zip90)
     D  @@zip4                        4    overlay(Zip90:6)

     *=========================================================================
     *  Main
     *=========================================================================
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

       //Contractor's name and address

       dsPeoInfo = GetPeoInfo(xxco#:xxfnd);
       FM2003 = dsPeoInfo.Name;
       FM2004 = dsPeoInfo.FedName;
       FMC401 = dsPeoInfo.Address1;
       FMC402 = dsPeoInfo.Address2;
       FMC403 = dsPeoInfo.Address3;


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
             //*** Begin add *** 07/25/17 ************************************
             When FNpeof = 'C' and ELemp# = 1 and ELdiv = 0;
             //*** End   add *** 07/25/17 ************************************
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
    
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
      /Free
       WriteInCopybook = *Off;
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
      /Free
       Begsr Valid;
       //*** Begin Chg *** 10/06/17 *********************************************
       //Exsr Getfnd;
       //If FNpeof <> *blanks;
       //   Crtfrm = *on;
       //Endif;
         crtFrm = *On;
       //*** End   Chg *** 10/06/17 *********************************************
       Endsr;
      /End-Free
