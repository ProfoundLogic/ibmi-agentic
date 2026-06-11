     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H bnddir('WTBNDDIR')
     *
    H/TITLE SR02301A - ENDORSEMENT ATTACH/PRINT PROGRAM
     *===============================================================*
     * WC020301A - EMPLOYEE LEASING ENDORSEMENT                      *
     *                                                               *
     * PROGRAM DESCRIPTION:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     *===============================================================*
     * Revisions:                                                    *
      *                                                               *
      * 10/27/22 C:333 Project: WC2834                                *
      *                Endorsement Recognition of Cancellation        *
      *                                                               *
      * 05/09/22 C:333 Ticket: 101416(148)                            *
      *                It should load Federal Name and DBA Name for   *
      *                Client Name in EL Form (WC020301A).            *
      *                                                               *
      * 07/26/18 C:062 Ticket: 70800(150)                             *
      *                Allow form to attach for real.                 *
      *                                                               *
      * 04/23/18 C:062 Ticket: 70395(148)                             *
      *                Allow form to attach.                          *
      *                                                               *
      * 10/10/17 C:136 Project: 150WBL                                *
      *                Use longer field in workfile for name          *
      *                                                               *
      * 10/06/17 C:062 Project: 150WAP                                *
      *                Remove peo stuff from valid                    *
      *                                                               *
      * 07/20/17 C:180 Project: 150WAP                                *
      *                Handle groups with combined PEOs.              *
     *===============================================================*
     * NOTES:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/D/C).          *
     *                                                               *
     *  qqmode = 1 - Called to determine if form should be attached. *
     *  qqmode = 2 - Called to print endorsement.                    *
     *  qqmode = 3 - Called for additional input.                    *
     *  qqmode = 4 - Called for midterm processing.                  *
     *===============================================================*

     ** Copybook F specs
     F/COPY SCOPYEN01F
     *=========================================================================
     *  Definition Specs
     *=========================================================================
      * Entry
     D SR02301A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR02301A        PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1
     D/COPY WWCPOLDAT4
     D                 ds
     D state                          2
     D  fmfl01                        1    overlay(state)
     D  fmfl02                        1    overlay(state:2)

     D xxefdt          s                   like(fmeedt)
     D xxuniq          s              1
     D xxactive        s              1
     D x1              s              3  0
     D x2efdt          s                   like(emefd1)
     D x2cxdt          s                   like(emcad1)
     D x2rndt          s                   like(emcad1)

     ** Copybook D specs
     D/COPY SCOPYEN01D
     d/COPY wPrPeo
     *=========================================================================
     *  Copybook required constants
     *=========================================================================

     ** Form ID
     D QQFORM          S             15A   inz('WC020301A')

     ** Additional entry program
     D addpgm          c                   const('XXXXXXXXXX')
     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     ** Entry parameters
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
     ** Alternate Employer file
     C     aeKey         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     *=========================================================================
     *  Main
     *=========================================================================
    
     ** Copybook C specs (PLISTs, KLISTs and mainline)
     C/COPY SCOPYEN01C
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information
     *=========================================================================
      /Free
       Begsr OthInf;
        WriteInCopybook = *off;
        Setll (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
        Dou %eof;
           Reade (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
           Select;
           When %eof;
              Iter;
           EndSL;
           //***** Beg Add *** 10/27/22 ******************************
           xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
           If xxCxDate <= ELpfdt and xxCxDate <> *zeros;
              Iter;
           Endif;
           //***** End Add *** 10/27/22 ******************************
           Chain (xxCo#:ELrtCd) prs01;
           If RSjrSt = Enst;
              dsCltInfo = GetCltInfo(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);
              FMc401 = dsCltInfo.name;
       //***** Beg Chg *** 05/09/22 ********************************************
       //***  FM2001 = dsCltInfo.name;
              FM2001 = dsCltInfo.fedName + %trim(FMc401);
       //***** End Chg *** 05/09/22 ********************************************
              FMc402 = dsCltInfo.address1;
              FMc403 = dsCltInfo.address2;
              FMc404 = dsCltInfo.address3;
              Exsr WriteWorkFile;
           Endif;
        Enddo;
       Endsr;
      /End-free

     *=========================================================================
     *  PEOGRP - PEO GROUP - Auto-attach endorsment
     *=========================================================================
     c     peogrp        begsr

     c                   endsr
     *=========================================================================
     *  UNIQFEIN - Determine if FEIN for Emloyer/Division is identical
     *           - to that of another division of same employer
     *=========================================================================
     c     uniqfein      begsr
     c                   endsr


     *=========================================================================
     *  PEOWRT - PEO GROUP - Write Endorsement
     *=========================================================================
     c     peowrt        begsr
     c                   endsr

     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
    c                   exsr      getfnd
      /Free
       UseDfltAddParms = *OFF;
      /End-Free
     C                   endsr
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr


     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
      /Free
       Begsr Valid;
       //*** Begin Del *** 10/06/17 *********************************************
       // Exsr getFnd;
       // Select;
       // When FNpeof = *blank;
       //    crtFrm = *Off;
       // When FNpeof = 'S' and ELdiv <> 0;
       //    crtFrm = *On;
       // When FNpeof = 'M' and ELemp# <> 1;
       //    crtFrm = *On;
       ////** Begin Add *** 07/20/17 ********************************************
       // When FNpeof = 'C' and ELemp# = 1 and ELdiv <> 0;
       //    crtFrm = *On;
       // When FNpeof = 'C' and ELemp# <> 1;
       //    crtFrm = *On;
       ////** End   Add *** 07/20/17 ********************************************
       // Other;
       //    crtFrm = *off;
       // Endsl;
       //*** End   Del *** 10/06/17 *********************************************
       //*** Begin Add *** 04/23/18 ****************************************
       //*** Begin Chg *** 07/26/18 ****************************************
       //*** crtFrm = *Off;
       crtFrm = *On;
       //*** End   Chg *** 07/26/18 ****************************************
       //*** End   Add *** 04/23/18 ****************************************
       Endsr;
      /End-free
