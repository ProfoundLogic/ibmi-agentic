     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     h/title - Test Relay Messenger Service
     h bnddir('STBNDDIR')
     d test114wyq      pr
     d inTypeOfMsg                    2a   const
     d inRrnOfFile                    9  0 const
     d inKey                          4a   const
     d InProgram                     10a   const options(*nopass)
     d inMsgId                      100a   const options(*nopass)
     d test114wyq      pi
     d inTypeOfMsg                    2a   const
     d inRrnOfFile                    9  0 const
     d inKey                          4a   const
     d InProgram                     10a   const options(*nopass)
     d inMsgId                      100a   const options(*nopass)
     d/copy sprRelay
      /free
       If %parms = 3;
          ProcRelayReq(inTypeOfMsg:inRrnOfFile:inKey);
       Endif;
       If %parms = 5;
          ProcRelayReq(inTypeOfMsg:inRrnOfFile:inKey:inMsgId);
       Endif;
       If %parms = 4;
          IsSmsDisabled(inTypeOfMsg:inRrnOfFile:inProgram:inKey);
       Endif;
       If %parms = 5;
          IsSmsDisabled(inTypeOfMsg:inRrnOfFile:inKey:inProgram:inMsgId);
       Endif;
       *inlr = *on;
       Return;
      /end-free
