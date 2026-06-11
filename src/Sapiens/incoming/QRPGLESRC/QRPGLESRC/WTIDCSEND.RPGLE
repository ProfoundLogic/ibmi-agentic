     H Copyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H dftactgrp(*no) actgrp(*caller) bnddir('WTBNDDIR')
     H/TITLE WTIDCSEND  - IDC Send File to IFS folder
     H DFTNAME(WTIDCSEND)
     H/DEFINE   PROFOUNDUI
      *---------------------------------------------------------------*
      *  Created   : 08/06/19                                         *
      *  Programmer: C:180                                            *
      *  Project   : WC2604 - NCCI Indemnity Data Call                *
      *---------------------------------------------------------------*
      *  Description:  IDC Send File to IFS folder.
      *
      *
      *---------------------------------------------------------------*
      * Revisions:
      * 03/26/20  C:180  Support 88141 Fix Submission File Id
      *
     FWMIDCSC   UF   E             DISK    Infds(fdsIDCSC)
     FWXIDCTR   O    E             DISK
      * *Entry
      *
     D WTIDCSEND       PR
     D inRRN                          9  0 Const

     D WTIDCSEND       PI
     D inRRN                          9  0 Const

     D CpyIdcDta       PR                  Extpgm('WTIDCCOPY')
     D qqFlr                         80    Const
     D qqFrmFile                     80    Const
     D qqToFile                      80    Const
     D qqArcFlr                      80    Const
     D qqArcFile                     80    Const

     D GetIdcPath      PR            80

     D RtvDocRoot      PR                  ExtPgm('RTVDOCROOT')
     D  RootDir                      10

     D RtvSuffix       pr                  ExtPgm('SA030R')
     D  Suffix                        3
     D  Pflr                         80

     D fdsIDCSC        DS
     D  dbSCrrn              397    400B 0

     D intRRN          S              9  0 inz
     D qqFlr           s             80    inz
     D qqFrmFile       s             80    inz
     D qqToFile        s             80    inz
     D qqArcFlr        s             80    inz
     D qqArcFile       s             80    inz
     D strPath         s             80    inz
     D strFilTyp       s              5    inz

      /Free
       intRRN = inRRN;
       chain(n) intrrn WMIDCSC;
       If %found(WMIDCSC);
          qqFlr   = GetIdcPath;
          strPath = %Trim(qqFlr) + '/';

          //*** Begin Change *** 03/26/20 ***************************
          qqToFile = %Trim(strPath) + %Subst(CCsubFilId:1:22) + '.' +
                     %Subst(CCsubFilId:23:3);
          //*** End   Change *** 03/26/20 ***************************

          strFilTyp = %Subst(CCsubFilId:4:5);

          Select;
          When strFilTyp = 'TRANS';
             qqFrmFile = 'WXIDCTR';
          When strFilTyp = 'QTRLY';
             qqFrmFile = 'WXIDCQT';
          Other;
             qqFrmFile = *blanks;
          Endsl;

          qqArcFlr = %Trim(strPath) + 'IDCARC';                      //Archive Flr
          qqArcFile = %Trim(strPath) + %Subst(CCsubFilId:1:8) + '*'; //Archive file

          If qqFrmFile <> *blanks and qqToFile <> *blanks;
             CpyIdcDta(qqFlr:qqFrmFile:qqToFile:qqArcFlr:qqArcFile);

             //Update Submission Status
             chain intrrn WMIDCSC;
             If %found(WMIDCSC);
                Select;
                When CCrunt = 'O' or CCrunt = 'S';
                   CCsts = 'S';
                When CCrunt = 'R';
                   CCsts = 'R';
                When CCrunt = 'D';
                   CCsts = 'D';
                Endsl;
                Update PCC01;
             Endif;
          Endif;
       Endif;
       *inlr = *on;
      /End-Free
      *================================================================
      * GetIdcPath  - Gets path for IDC submission files
      *  Parameters - None
      *  Returns    - 80A Idc Path
      *================================================================
     PGetIdcPath       B
     DGetIdcPath       PI            80
     D Path            s             80    inz
     D RootDir         s             10    inz
     D Suffix          s              3    inz
     D Pflr            s             80    inz
     D Flr             s             10    inz('IDC')
      /Free
        //Get the Root Directory
         Path = *blanks;
         RtvDocRoot(RootDir);
         RtvSuffix(Suffix:Pflr);
         If Pflr <> *blanks;
            Pflr = %Trim(Pflr) + '/';
         Endif;
         Path = '/' + %Trim(RootDir) + '/' + %Trim(Pflr) +
                %Trim(Flr);
         Return Path;
      /End-Free
     PGetIdcPath       e
