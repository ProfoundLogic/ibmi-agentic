    Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
    H StoneRiver is a trademark of StoneRiver, Inc.')
    H*
    H/TITLE CVTCSTACT
    H**************************************************************************************
    H*                                                                                    *
    H*  Description:  Convert in House account no data to Customer ID field               *
    H*                                                                                    *
    H*  Created: 03/19/14   Programmer C:295   Project: 89WAX                             *
    H*                                                                                    *
    H**************************************************************************************
    H*  Revisions:                                                                        *
    H*                                                                                    *
    H**************************************************************************************
      ** Employer Master
     fwmeml     uf   e           k disk    rename(pem01:pem00)

     d*=====================================================================================
     d* Proto-types
     d*=====================================================================================
     d* Get client ID
     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d   outCltID                     3

     d*=====================================================================================
     d* Variables
     d*=====================================================================================
     d outCltID        s              3

      //====================================================================================
      // Main line
      //====================================================================================

      /free
       // To retrieve the client id
       GetClientID(outCltID);

       // Process if the client is '089' only.
       If outCltID= '089';
          Setll *Loval pem00;
          Read pem00;
          Dow not %eof(Wmeml);
             EMcust = EMacct;
             Update pem00;
             Read pem00;
          Enddo;
       EndIf;

       Unlock wmeml;
       *Inlr = *On;
      /end-free

