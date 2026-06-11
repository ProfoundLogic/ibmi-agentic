     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE FIX83388 - Assign Remarks to Claim Adjuster
     H*****************************************************************
     H** REVISIONS:
     H*
     H*****************************************************************
     Fsdcmlua   uf   E           K DISK    Rename(pcm01:pcm0ua)
     Fwmcml1    if   E           K DISK    Prefix(C2:2)
     Fsmupl1    if   E           K DISK
     D*
     D                 DS
     D  xxKey                  1     40
     D  kkco#                  1      3  0
     D  kkfnd                  4      6  0
     D  kkfyr                  7      9  0
     D  kkcase                10     16  0

     D kkUsr           s                   like(UPusr) Inz('EL')

      /Free
           Chain kkUsr pcm0ua;
           If %Found(SDCMLUA);
              DoU %Eof(SDCMLUA);
                 If CMuser='EL' And CMpNam='W0070R';
                    xxKey=CMkey;
                    Chain (kkCo#:kkFnd:kkFyr:kkCase) pcm01;
                    If %Found(WMCML1) and C2clEx<>'TBD';
                       Chain C2clEx pup01;
                       If %Found(SMUPL1);
                          CMuser=UPusr;
                       EndIf;
                    EndIf;
                 EndIf;
                 Update pcm0ua;
                 ReadE kkUsr pcm0ua;
               EndDo;
            EndIf;
      /End-Free

     C                   Seton                                        LR
