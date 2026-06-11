     hCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H bnddir('WTBNDDIR')
    H/TITLE SR04308A - CA - Employee Leasing Endorsement - Short Form
     *===============================================================*
     * WC040308A                                                     *
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
     * 03/03/17 C:180   Project: SY371                               *
     *                  New name for PEO procedure.                  *

     * 02/23/17 C:180   Project: SY371                               *
     *                  Interface with Scriptura.                    *
     *                                                               *
     * 09/30/16 Project: SY371 (Scriptura Phase 2)                   *
     *          Convert for phase 2 - not commented                  *
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
     fwxpel     uf a e           k disk
     fwxpel1    if   e           k disk
     f                                     rename(ppe01:ppe02)
     ** Copybook F specs
     F/COPY SCOPYEN01F
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================
      //Entry
     D SR04308A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR04308A        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      ** Prototypes

     ** Copybook D specs
     D/COPY SCOPYEN01D
     D/COPY WPRPEO
      //***** Beg Del *** 10/28/22 **********************************
     D*/Copy WprEmplr
      //***** End Del *** 10/28/22 **********************************
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')

      * Stand alone

     D qqform          s             15a   inz('WC040308A ')
     d PeoZip          S             10    inz
     D ##emp#          s              9  0 inz
     D ##div           s              5  0 inz


      * Data structures

     D                 ds
     d Zip90                          9  0
     D  @@zip5                        5    overlay(Zip90)
     D  @@zip4                        4    overlay(Zip90:6)


     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
    
     *=========================================================================
     *  PLIST definitions
     *=========================================================================
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
    
     *=========================================================================
     *  Main
     *=========================================================================
     c     emky1         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxemp#

     ** Copybook C specs
     C/COPY SCOPYEN01C
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
      /Free

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
             //*** Begin add *** 07/25/17 ************************************
                If (FNpeof = 'C' and ELemp# <> 1) or (FNpeof <> 'C');
             //*** End   add *** 07/25/17 ************************************
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
             //*** Begin add *** 07/25/17 ************************************
                Endif;
             //*** End   add *** 07/25/17 ************************************
             Endsl;

           //***** Beg Add *** 10/28/22 ******************************
             Endif;
           //***** End Add *** 10/28/22 ******************************
          reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;
          Enddo;
      /End-Free
     C                   endsr

     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
      /Free
       Begsr Pgmstr;
       WriteInCopybook = *Off;
       Endsr;
      /End-Free
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr
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
