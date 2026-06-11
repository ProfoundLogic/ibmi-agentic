     hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h/title DOCVERIFY
     h**************************************************************************************
     h*                                                                                    *
     h*  Description:  Verify Doc Existance in IFS                                         *
     h*                                                                                    *
     h*  Created: 06/13/14   Programmer C:279   Project: SY195
     h*                                                                                    *
     h**************************************************************************************
     h*  Revisions:                                                                        *
     h*                                                                                    *
     h**************************************************************************************

     fsdcmp     if   e             disk
     f                                     infds(dbfds)
     fimdop     if   e             disk
     f                                     infds(dbfds1)
     fsmmdp     if   e             disk
     f                                     infds(dbfds2)
     fdcoutfl   o    e             disk    usropn
     fqsysprt   o    f  136        printer oflind(*inof)
     f                                     infds(prtfds)

     d qca             s             56    dim(1) ctdata perrcd(1)
     d dbfds           DS
     d  dbrrn                397    400B 0
     d dbfds1          DS
     d  dbrrn1               397    400B 0
     d dbfds2          DS
     d  dbrrn2               397    400B 0

     d prtFDs          ds
     d curPage               367    368I 0
     d curLine               369    372I 0

      * Work Fields
     d dsTime          s              6  0
     d dsDate          s              6  0
     d Folder          s             60
     d fileName        s             10
     d intLen          s             15  5 inz(56)
     d objName         s            256
     d error           s             25
     d intCount        s             10  0 inz(0)
     d rtnFlg          s               n
     d rrn             s              9  0

      * Prototypes
     d chkObj          pr                  extPgm('CHKIFSOBJ')
     d objName                      256
     d rtnFlg                          n

     d CmdExc          pr                  extPgm('QCMDEXC')
     d strCmd                        56
     d intLen                              like(intLen)

     d psds          esds                  extname(szq1p)

      *----------------------------------------------------------------------
      *  Process Request
      *----------------------------------------------------------------------
      *
      /free

       // start processing
       dstime = %Dec(%Time(): *ISO);
       dsdate = %dec(%date():*mdy);

       // clear outfile
       CmdExc(qca(1):intLen);

       // open outfile
       open dcoutfl;

       fileName = 'SDCMP';

       // Read Data from SDCMP
       Setll *start sdcmp;
       Read pcm01;
       Dow Not %eof(sdcmp);
          If CMfl05 <> 'O' and CMuncn <> ' ' and CMifsp = 'I';
             objName = %trim(CMuncn);
             rrn = dbRrn;
             Exsr VerifyDoc;
          Endif;

          // Read Next Record
          Read pcm01;
       Enddo;

       // Read Data from IMDOP
       fileName = 'IMDOP';

       Setll *start imdop;
       Read pdo01;
       Dow Not %eof(imdop);
          If DOuncn <> ' ' and DOifsp = 'I';
             objName = %trim(DOuncn);
             rrn = dbRrn1;
             Exsr VerifyDoc;
          Endif;

          // Read Next Record
          Read pdo01;
       Enddo;

       // Read Data from SMMDP
       fileName = 'SMMDP';

       Setll *start smmdp;
       Read pmd01;
       Dow Not %eof(smmdp);
          If MDuncp <> ' ' and MDuncf = 'I';
             objName = %trim(MDuncp);
             rrn = dbRrn2;
             Exsr VerifyDoc;
          Endif;

          // Read Next Record
          Read pmd01;
       Enddo;

       If intCount > 0;
          If *inof or curLine > 99999;
             Except Header;
             *inof = *off;
          Endif;

          Except Total;
       Endif;

       // close outfile
       close dcoutfl;

       *inlr = *on;

       //---------------------------------------------------------------------
       // Verify Document existance
       //---------------------------------------------------------------------
       BegSr VerifyDoc;

       rtnFlg  = *off;

       // verify document for existence
       Monitor;
          CHKOBJ(objName:rtnFlg);

          // document not found Error
          If rtnflg = *off;
             exsr writeError;
          Endif;
       on-Error;
          exsr writeError;
       EndMon;

       EndSr;

       //---------------------------------------------------------------------
       // Error Subroutine
       //---------------------------------------------------------------------

       BegSr WriteError;

       // Error Description
       folder = %trim(objName);
       error = 'Document does not exist.';
       intCount = intCount + 1;

       // Print Error

       If *inof or curLine > 99999;
          Except Header;
          *inof = *off;
       Endif;

       Except Detail;

       OUTfil = fileName;
       OUTrrn = Rrn;
       OUTpth = folder;
       write pout01;

       EndSr;

      /end-free

     O**********************************************************************

     Oqsysprt   e            header         1  3
     O                                            5 'Time:'
     O                       dstime              14 '  :  :  '
     O                                           51 'Document verification:'
     O                                          122 'Date:'
     O                       dsdate        y    131

     O          e            header         2
     O                                            4 'Pgm:'
     O                       q1pgm               15
     O                                          122 'Page:'
     O                       page          z    131
     O*
     O          e            header         1
     O                                           10 'File'
     O                                           21 'Record'
     O                                           72 'Document Path'
     O                                          117 'Message'

     O          e            header         2
     O                                           10 'Name'
     O                                           21 'Number'
     O                                           72 'Name'

     O          e            detail         1
     O                       fileName            10
     O                       rrn           z     23
     O                       folder              95
     O                       Error              131

     O          e            Total          1
     O                                            8 'Total'
     O                       intCount      z     20
**  QCMDEXC TABLE
CLRPFM DCOUTFL
