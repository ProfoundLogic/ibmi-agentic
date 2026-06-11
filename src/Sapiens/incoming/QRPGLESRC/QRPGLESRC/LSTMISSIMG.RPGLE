     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     H/define CONVERT
     H/TITLE LSTMISSIMG - List Missing Images
     H*****************************************************************
     H* Description:  This pgm reads SMMDP and determines which       *
     H*   images don't exist.                                         *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*****************************************************************
     FSMMDP     IF   E             DISK
     FSDCMLA    IF   E           k DISK
     FIMISSP    O    E             DISK

     d ChkIfsObj       pr                  ExtPgm('CHKIFSOBJ')
     d  ifsObj                      256
     d  ifsObjFound                    n

     d  ifsObj         s            256
     d  ifsObjFound    s               n
     d  mainRemarks    s               n   Inz(*On)

     D PSDS          ESDS                  EXTNAME(SZQ1P)

      /free
       If Not mainRemarks;
          Read pmd01;
          DoW Not %Eof(smmdp);
             Chain (MDkey:MDseq#) pcm01;
             If %Found(sdcmla);
                // Check to see if object exists.
                ifsObj = MDuncp;
                ifsObjFound = *Off;
                ChkIfsObj(ifsObj:ifsObjFound);
                // If image not found then write to missing image file
                If Not ifsObjFound;
                   MIkey  = MDkey;
                   MIseq# = MDseq#;
                   MIuncP = MDuncP;
                   Write PMI01;
                EndIf;
             EndIf;
             Read pmd01;
          EndDo;
       Else;
          Read pcm01;
          DoW Not %Eof(sdcmla);
                // Check to see if object exists.
                ifsObj = CMuncN;
                ifsObjFound = *Off;
                ChkIfsObj(ifsObj:ifsObjFound);
                // If image not found then write to missing image file
                If Not ifsObjFound;
                   MIkey  = CMkey;
                   MIseq# = CMseq#;
                   MIuncP = CMuncN;
                   Write PMI01;
                EndIf;
             Read pcm01;
          EndDo;
       EndIf;

       *inLR = *On;
      /end-free
