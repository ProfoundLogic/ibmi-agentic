     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*********************************************************************
     H/title Retrieve System Properties - AJAX
    H*********************************************************************
    H*  Created: 10/31/12  C:33  Project: Transform
    H*********************************************************************
    H*  Revisions:                                                       *
    H*                                                                   *
    H*********************************************************************
      *Entry
     DStProperty       pr                  ExtPgm('STPROPERTY')
     D                               25
     D                            32767
     D                                1
      *
     DStProperty       pi
     D inKey                         25
     D outJSON                    32767
     D RtnCode                        1

      /copy sPrAtrium
      /copy sPrPrprty

      /Free

       outJSON = ' ';
       BuildJSON(outJSON:inKey:SysProperty(inKey));
       RtnCode = ' ';
       Return;

      /End-Free
