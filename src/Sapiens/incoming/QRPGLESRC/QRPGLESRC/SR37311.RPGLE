     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR10302  - PA - Multiple Coordinated Policy Endorsement
      //===============================================================*
      // WC100302  -  PA - Multiple Coordinated Policy Endorsement     *
      //                                                               *
      // PROGRAM DESCRIPTION:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //===============================================================*
      // Revisions:                                                    *
      //                                                               *
      // 10/19/22 C:342 Project: WC2834 (Recompile)                    *
      //                Endorsement Recognition of Cancellation        *
      //                                                               *
     // 10/06/17 C:136 Project: 150WBL                               *
     //                PEO attachment criteria moved to copy code    *
     //                                                              *
      // 07/25/17 C:180 Project: 150WAP                                *
      //                Handle groups with combined PEOs               *
      //                                                               *
      // 03/01/17 C:180 Project: WC2481                                *
      //                                                               *
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

      *
      * Prototypes
      *
     D SR37311         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR37311         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

     D/COPY wPRPEO
     D/COPY SCOPYEN01D

      *
      * Data structures
      *
     D CLTINfo         DS           500    INZ
     D  CLTName                     100    Overlay(CLTinfo:1)
     D  CLTFedName                  100    Overlay(CLTinfo:101)
     D  CLTAdrln1                    50    Overlay(CLTinfo:201)
     D  CLTAdrln2                    50    Overlay(CLTinfo:251)
     D  CLTAdrln3                    50    Overlay(CLTinfo:301)
     D  CLTCity                      25    Overlay(CLTinfo:351)
     D  CLTState                      2    Overlay(CLTinfo:376)
     D  CLTZip                        9    Overlay(CLTinfo:378)
     D    CLTzip5                     5    Overlay(CLTZip)
     D    CLTZip4                     4    Overlay(CLTZip:6)
     D  CLTFein                       9    Overlay(CLTinfo:387)
     D  CLTPhArea                     3    Overlay(CLTinfo:396)
     D  CLTPhNbr                      7    Overlay(CLTinfo:399)
     D  CLTPhone                     12    Overlay(CLTinfo:406)
     D  CLTFiller                    83    Overlay(CLTinfo:418)

     D QQFORM          S             15A   INZ('WC370311  ')

     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')


      /COPY SCOPYEN01C

      /Free

       //=========================================================================
       //  OTHINF - Subroutine to load form specific information
       //=========================================================================
       Begsr othinf;
       Reset CltInfo;
       FM2001 = *Blanks;
       FM2002 = *Blanks;
       FMead1 = *Blanks;
       FMead2 = *Blanks;
       FMead3 = *Blanks;
       //*** Begin Change *** 07/25/17 ***********************************
       //If fnpeof = 'M'  and  xxemp# <> 1;

       If (fnpeof = 'M'  and  xxemp# <> 1) or
          (fnpeof = 'C'  and  xxemp# <> 1);
       //*** End   Change *** 07/25/17 ***********************************
          CLTinfo = GetCltInfo(xxco#:xxfnd:xxemp#:emdiv:xxfyr);
          FM2001 = CltFedName;
          FM2002 = CltName;
          FMead1 = CltAdrln1;
          FMead2 = CltAdrln2;
          FMead3 = CltAdrln3;
       Endif;
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
       crtFrm = *On;
       Endsr;
      /End-Free
