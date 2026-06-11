     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H/TITLE - Convert to AO
     H*****************************************************************
     H*  Created: 02/21/13   C:282/33     Project: 140WBA
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/01/13 C:282 Project: 140WBA Seq# 16                        *
     H*                Added code to update the output queue of       *
     H*                User Parameter files based on Outq Transition  *
     H*                Table file.                                    *
     H*                                                               *
     H*****************************************************************
     fsxupp     if   e           k disk    Rename(pup01:pup0p)
     f                                     Prefix(T_)
     fsxorp     if   e           k disk    Rename(por01:por0p)
     f                                     Prefix(T_)
     fsmupl     uf   e           k disk    Rename(pup01:pup00)
     fwmupl     uf   e           k disk    Prefix(P_)
     fsdcmlg    uf   e           k disk    Rename(pcm01:pcm0g)
     fsmorl1    uf   e           k disk

     dRunSQLStmt       pr                  ExtPgm('SYRUNSQL')
     d sqlStmt                      550

     dstrSQLStmt       s            550

      /Free

       // Read the User Parameter Transition Table file SXUPP

       Read pup0p;
       DoW Not %EOF(sxupp);

       // Read the User Parameter File SMUPP
          Chain T_UPoldUsr pup00;
          If %Found(smupl);
             UPusr = T_UPnewUsr;
             Update pup00;
          Endif;

       // Read the User Parameter File WMUPP
          Chain T_UPoldUsr pup01;
          If %Found(wmupl);
             P_UPusr = T_UPnewUsr;
             Update pup01;
          Endif;

       // Read the Remark Detail File SDCMP
          strSQLStmt = 'update SDCMP set CMUSER = ''' + %trim(T_UPnewUsr) +
           ''' where CMUSER =''' + %trim(T_UPoldUsr) +
           ''' and CMSTAT <> ''I''';
          Callp RunSQLStmt(strSQLStmt);
        //Setll T_UPoldUsr pcm0g;
        //Reade T_UPoldUsr pcm0g;
        //Dow Not %EOF(sdcmlg);
        //   CMuser = T_UPnewUsr;
        //   Update pcm0g;
        //Reade T_UPoldUsr pcm0g;
        //EndDo;

       // Read the DARTS Security Master File SMSLP
          strSQLStmt = 'update SMSLP set SLUSRP = ''' + %trim(T_UPnewUsr) +
           ''' where SLUSRP =''' + %trim(T_UPoldUsr) + '''';
          Callp RunSQLStmt(strSQLStmt);

       // Read the next record from User Parameter Transition Table file SXUPP

       Read pup0p;
       Enddo;

       // Read the User Parameter Transition Table file SXORP
       Read por0p;
       DoW Not %EOF(sxorp);

       // *** Beg Add  ***  05/01/13 **************************************/

       // User Parameter File SMUPP to update output queue.
          strSQLStmt = 'update SMUPP set UPOUTQ = ''' + %trim(T_ORnewOutQ) +
           ''' where UPOUTQ =''' + %trim(T_ORoldOutQ) + '''';
          Callp RunSQLStmt(strSQLStmt);

       // User Parameter File WMUPP to update output queue.
          strSQLStmt = 'update WMUPP set UPOUTQ = ''' + %trim(T_ORnewOutQ) +
           ''' where UPOUTQ =''' + %trim(T_ORoldOutQ) + '''';
          Callp RunSQLStmt(strSQLStmt);

       // *** Beg End  ***  05/01/13 **************************************/

       // Read the system output routing maintenace
          Setll T_ORoldOutQ por01;
          Reade T_ORoldOutQ por01;
          Dow Not %EOF(smorl1);
             ORoutQ = T_ORnewOutQ;
             Update por01;
          Reade T_ORoldOutQ por01;
          EndDo;

       Read por0p;
       Enddo;

       *InLr = *On;

      /End-Free
