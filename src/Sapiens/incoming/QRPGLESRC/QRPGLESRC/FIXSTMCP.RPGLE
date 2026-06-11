     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H bnddir('STBNDDIR')
     H dftactgrp(*no) actgrp(*caller)
     H*
     H****************************************************************
     H*  FIXSTMCP - Update STMCP entries
     H*
     H*  Description: This pgm will update STMCP entry
     H*
     H*  Created    : 03/15/2017  Programmer: 212  Support: 46145 / 90MHY
     H*
     H*  Called By  :
     H*  Calls To   :
     H*
     H****************************************************************
     H*  Revisions:
     H****************************************************************
     F*
     fstmcl     uf a e           k disk    rename(pmc01:pmc00)
     D*
     d** Procedure declaration

     d* *Entry
     d FIXSTMCP        pr                  ExtPgm('FIXSTMCP')

     d FIXSTMCP        pi

      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3
     D*
     d** Array declaration
     d*
      * To hold STMCP key value
     d MCIDKey         s             25    DIM(1) CTDATA PERRCD(1)
      * To hold STMCP command 1 value
     d MCIDCmd1        s            100    DIM(1) CTDATA PERRCD(1)
     D*
     d** Data structures
     d*
     d psds          esds                  extname(szq1p)
     d*
     d** Standalone variables
     d*
     d clientID        s              3    inz('000')
     d strMCky         s                   like(MCID) inz
     d strMCcm1        s                   like(MCCMD1) inz
     d indproc         s               n   inz(*off)
     d pass            s              2s 0 inz

     d maxpass         s              2s 0 inz
     C*
     c** Copybook declarations
     c*
     c/free

       // get client ID
       GetClientID(clientID);

       // initialize values
       pass = 1;
       maxpass = 1;

       DoW (pass <= maxpass);

          // set/condition values for each pass
          // keep adding values here to update multiple records.
          // To stop update of any key, turn indproc = *off
          Select;

           When pass = 1;

            strMCky  = MCIDKey(1);
            strMCcm1 = MCIDCmd1(1);
            indproc = *on;

           Other;
            indproc = *off;

          EndSl;

          // process for each pass.
          If indproc = *on;

             Chain (strMCky) pmc00;
             If %found(stmcl);

                // set command 1 to new value
                MCcmd1 = strMCcm1;

                // update with new values
                update pmc00;

             EndIf;

          EndIf;

          pass += 1;
       EndDo;

       *inlr = *on;

      /end-free

** Key Value
CMD(CALL PGM(W8154C00001
** Command 1
SBMJOB CMD(CALL PGM(W8154C) PARM('777' ' ' ' ')) JOB(W8154C)
