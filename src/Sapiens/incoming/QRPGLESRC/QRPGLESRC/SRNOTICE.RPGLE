      //copyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H BNDDIR('WTBNDDIR')
      // StoneRiver is a trademark of StoneRiver, Inc.')
      /TITLE SRNotice - "Policy Notice Endorsement" print/attach
      //===============================================================*
      //                                                               *
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXP6P) prior to a form merge.                     *
      //                                                               *
      // Created   : 09/01/17                                          *
      // Programmer: C:291                                             *
      // Project   : 150WBM                                            *
      //                                                               *
      //===============================================================*
      // REVISIONS:                                                    *
      // 01/29/21 C:180 Support: 93990 (LUBA)                          *
      //                Monitor error when running SendToScriptura     *
      //                Changes were made in Copy Book SCOPYEN01C      *
      //                                                               *
      // 10/31/17 C:291 Project: 150WAC                                *
      //                Pull correct info by endorsement form state    *
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

     Fwxp6p     o    e             disk
     Fwmnxl1    if   E           K DISK

      //* Copybook D specs
     D/COPY SCOPYEN01D

      //* Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
     D  qqRrn          s              9  0
     D  qqMod2         s              1

     D xxdate          s              7  0 inz

     D SRNOTICE        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                               15

     D SRNOTICE        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqForm                       15

      //* Copybook C specs
     C/COPY SCOPYEN01C

      /Free
       //=========================================================================
       //  OTHINF - Subroutine to load form specific information & write record
       //=========================================================================

       begsr othinf;

       endsr;

       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       begsr pgmstr;
          RunningSRtext = *Off;
       endsr;

       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       begsr pgmend;

             //***Beg Chg *** 10/31/17 *********
             //Eval xxdate = EMefd1;
             //Setll (EMco#:EMfnd:EMMST:xxdate) pnx01;
             //Reade (EMco#:EMfnd:EMMST) pnx01;

             Eval xxdate = ELPFDT;
             Setll (EMco#:EMfnd:ENST:xxdate) pnx01;
             Reade (EMco#:EMfnd:ENST) pnx01;
             //***End Chg *** 10/31/17 *********

             P6XML = XPXMLT;
             P6PREVST = NXST;
             P6DEPTNAME = %trim(NXDept);
             P6DEPTPHN# = NXDPPN;
             P6DEPTAREA = NXDPAR;
             P6USER = Q1USER;
             P6PGM  = q1pgm;
             P6DT = sysdat;
             P6TIME = dstime;
             P6FNNAME = FNName;
             P6FNMAD1 = FNMad1;
             P6FNMAD2 = FNMad2;
             P6FNMCTY = FNMCty;
             P6FNMST  = FNMSt;
             P6FNMZIP = FNMZip;

             Write PP601;

             Chain inXML XMLTEML1;
             If %found(XMLTEML1);
                XTtype = 'Policy Notice Endorsement';
                Update XT100;
             Endif;

       endsr;

       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       begsr valid;
          crtfrm = *On;
       endsr;
      /End-Free
