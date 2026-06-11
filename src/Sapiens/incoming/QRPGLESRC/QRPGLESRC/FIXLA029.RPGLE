     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('FTBNDDIR')
     H*
     H****************************************************************
     H*  FIXLA029 - Update FMPRPLA in relation to project LA029
     H*
     H*  Description: This pgm will update FMPRPLA
     H*
     H*  Created    : 05/04/2017  Programmer: 212  Support: 46894
     H*
     H*  Called By  :
     H*  Calls To   :
     H*
     H****************************************************************
     H*  Revisions:
     H****************************************************************
    F*
     ffmprlla   uf   e           k disk
     flacpt     if   e           k disk
    D*
     d** Procedure definitions
     d*
     d**Entry
     d FIXLA029        pr                  ExtPgm('FIXLA029')

     d FIXLA029        pi

      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3
     D*
     d* data structures
     d*
     d psds          esds                  extname(szq1p)
     d*
     d** Standalone variables
     d*
     d clientID        s              3    inz('000')
     d intCo#          s                   like(PRco#) inz(777)
     d kkefdt          s                   like(PRefdt) inz
     C*
     c** Copybook declarations
     c*
     d/copy sPrUsrPrms

     c/free

       // get client ID
       GetClientID(clientID);

       // get company
       intCo# = GetUserInfo(q1user);

       // first pass

        // delete codes where prefdt = 0

        Setll *start fmprlla;
        Read ppr01;

        DoW not %eof(fmprlla);
           If PRefdt = *zeros;
              delete ppr01;
           EndIf;
           Read ppr01;
        EndDo;

       // second pass

        // clear out PR92N3 for all records for prefdt = 6/20/2016

        kkefdt = 1160620;

        Setll *start lacpt;
        Read pla01;

        DoW not %eof(lacpt);
           Chain (intCo#:LAcpt4:kkefdt) ppr01;
           If %found(fmprlla);
              PR92n3 = *zeros;

              PRuser = q1user;
              PRpgm  = q1pgm;
              PRtime = %dec(%char(%time():*iso0):6:0);
              PRdt   = %dec(%char(%date():*cymd0):7:0);

              Update ppr01;
           EndIf;
           Read pla01;
        EndDo;

       *inlr = *on;

      /end-free
