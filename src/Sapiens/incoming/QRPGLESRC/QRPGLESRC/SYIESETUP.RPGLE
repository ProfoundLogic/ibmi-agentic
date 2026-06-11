     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************

     H DFTNAME(SYIESETUP)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H BNDDIR('STBNDDIR')
     H/TITLE SYIESETUP - Establish Import/Export Settings.
      //===================================================================*
      // Program Description:                                              *
      //                                                                   *
      //   Establish Import/Export Settings.                               *
      //                                                                   *
      // Created   : 05/30/17                                              *
      // Programmer: C:180                                                 *
      // Project   : MC325                                                 *
      //===================================================================*
      //====================================================================*
      //  Created   : 05/30/17                                              *
      //  Programmer: C:180                                                 *
      //  Project   : MC325                                                 *
      //====================================================================*
      //  Revisions:                                                        *
      //  06/30/17 C: 079 Project: MC325                                    *
      //                  Change criteria for client 90                     *
      //                                                                    *
      // -------------------------------------------------------------------*
      //  NOTE: Please add comments to the top of Revisions.                *
      // -------------------------------------------------------------------*
      //====================================================================*
     Fsfbpf     uf   e             DISK
     Fsmiep     uf a e             DISK

      *
      * Prototypes
      *
     D RtvCltId        pr                  extpgm('RTVCLTID')
     D                                3    Const

      *
      * Stand Alone
      *
     D StrCltId        s              3A   INZ

      /Free
       RtvCltId(strCltId);

       Read sfbpf;                         //Import/Export Availability

       If not %eof(sfbpf);

          Select;
          When strCltId = '025';
             %subst(FBAL02:5:1) = 'Y';    //Bill Import

          When strCltId = '045';
             %subst(FBAL02:5:1) = 'Y';

          When strCltId = '089';
             %subst(FBAL02:5:1) = 'Y';

          When strCltId = '090';
             %subst(FBAL02:5:1) = 'Y';
             %subst(FBAL02:1:1) = 'Y';   //Claims Export
             %subst(FBAL02:3:1) = 'Y';   //Provider Export
             %subst(FBAL02:4:1) = 'Y';   //Paid Date Export

          When strCltId = '114';
             %subst(FBAL02:5:1) = 'Y';

          When strCltId = '126';
             %subst(FBAL02:5:1) = 'Y';

          When strCltId = '129';
             %subst(FBAL02:5:1) = 'Y';

          When strCltId = '133';
             %subst(FBAL02:5:1) = 'Y';
             %subst(FBAL02:1:1) = 'Y';
             %subst(FBAL02:3:1) = 'Y';
             %subst(FBAL02:4:1) = 'Y';

          When strCltId = '144';
             %subst(FBAL02:5:1) = 'Y';
          Endsl;

          Update sfb01;
       Endif;



       Read smiep;                             //Import/Export Options
          *in25 = *off;

          Select;
          When strCltId = '025';
             Clear pie01;
             *in25 = *on;

          When strCltId = '045';
             IEudup  = 'C';
             IEcsts  = 'B';
             IEcdny  = 'Y';
             IEccvfl = 'Y';
             IEcrcd  = 'Y';
             IEcdth  = 'Y';

          When strCltId = '089';
             Clear pie01;
             *in25 = *on;

          When strCltId = '090';
             IEudup   = 'C';                  //Bill Import
          //*** Beg Chg *** 06/30/17 *************************************
          //***   IEcsts   = 'B';                  //Claims Export
             IEcsts   = 'O';                  //Claims Export
          //*** End Chg *** 06/30/17 *************************************
             IEcchg   = 'Y';
             IEcdny   = 'Y';
             IEccvfl  = 'Y';
             IEcrcd   = 'Y';
             IEcdth   = 'Y';
             IEcldays = 180;
             IEftyp   = 'A';                  //Provider Export
             IEinczro = 'Y';                  //Paid Date Export

          When strCltId = '114';
             IEudup  = 'C';

          When strCltId = '126';
             Clear pie01;
             *in25 = *on;

          When strCltId = '129';
             Clear pie01;
             *in25 = *on;

          When strCltId = '133';
             IEudup  = 'A';
             IEcsts  = 'B';                  //Claims Export
             IEcrcd  = 'Y';
             IEcdth  = 'Y';
             IEftyp  = 'A';                  //Provider Export

          When strCltId = '144';
             Clear pie01;
             *in25 = *on;
          Endsl;

       Select;
       When not %eof(smiep) and *in25 = *off;
          Update pie01;
       When not %eof(smiep) and *in25 = *on;
          Delete pie01;
       When %eof(smiep) and *in25 = *off;
          Write pie01;
       Endsl;

       *inlr = *on;
      /End-Free
