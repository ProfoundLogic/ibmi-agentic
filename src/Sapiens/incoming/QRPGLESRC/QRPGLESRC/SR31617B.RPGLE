     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
      //===============================================================*
     H/TITLE SR31617A - NY - Foreign Voluntary Compensation & Employers
      //===============================================================*
      // WC310617B - NY Foreign Voluntary Compensation and Employers . *
      //                                                               *
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      // Created   : 03/27/20                                          *
      // Programmer: C: 079                                            *
      // Project   : WC2736                                            *
      //===============================================================*
      // REVISIONS:                                                    *
      //                                                               *
      //   10/19/22 C:342 Project: WC2834 (Recompile)                  *
      //                  Endorsement Recognition of Cancellation      *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of revisions.           *
      // ------------------------------------------------------------- *
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
      //
      //* Additional Information File
     Fwxfmp1    if   e           k disk    usropn
     F                                     rename(pfm01:pfm02)
      //=========================================================================
      //  Definition Specs
      //=========================================================================
      //
      //* Copybook D specs
     D/COPY SCOPYEN01D
      //=========================================================================
      //  /COPY required constants
      //=========================================================================
      //* Form ID
     D qqForm          s             15a   inz('WC310617B ')
      //* Additional entry program
     D AddPgm          c                   const('SR31617IB ')

      //entry parameters
     D SR31617B        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR31617B        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      //=========================================================================
      //  KLIST definitions
      //=========================================================================
      //
      //* Additional Input file              C
     C     fmKy3         klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxFyr
     C                   kfld                    xxEmp#
     C                   kfld                    xxDiv
     C                   kfld                    xxForm
      //=========================================================================
      //  Main
      //=========================================================================
      //* Copybook C specs
     C/COPY SCOPYEN01C
      //
      //=========================================================================
      //  OTHINF - Subroutine to load form specific information & write record
      //=========================================================================
      /FREE
       Begsr OthInf;
          //
          Open WXFMP1;
          //
          Chain fmKy3 pfm02;
          //
          //* Move the custom fields to print
          FMc401 = F3c401;
          FMc402 = F3c402;
          FMc403 = F3c403;
          FMc404 = F3c404;
          FMc405 = F3c405;
          FMc406 = F3c406;
          FMc407 = F3c407;
          FMc408 = F3c408;
          FMc409 = F3c409;
          FMc410 = F3c410;
          FMc411 = F3c411;
          FMc412 = F3c412;

          FMc201 = F3c201;
          FMc202 = F3c202;
          FMc203 = F3c203;
          FMc204 = F3c204;
          FMc205 = F3c205;
          FMc206 = F3c206;
          FMc207 = F3c207;
          FMc208 = F3c208;
          FMc209 = F3c209;
          FMc210 = F3c210;

          FMc101 = F3c101;
          FMc102 = F3c102;
          FMc103 = F3c103;
          FMc104 = F3c104;
          FMc105 = F3c105;
          FMc106 = F3c106;
          FMc107 = F3c107;
          FMc108 = F3c108;
          FMc109 = F3c109;
          FMc110 = F3c110;

          FMam01 = F3am01;
          FMam02 = F3am02;
          FMam03 = F3am03;
          FMam04 = F3am04;
          FMam05 = F3am05;

          Close wxfmp1;
          //
       Endsr;
       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr PgmStr;
          //
          //
       Endsr;
       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       Begsr PgmEnd;
          //
          //
       Endsr;
       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       Begsr Valid;
          //
          crtFrm = *on;
      //
       Endsr;
      /END-FREE
