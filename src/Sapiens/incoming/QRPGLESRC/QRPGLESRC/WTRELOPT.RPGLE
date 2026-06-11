     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h/title WtRelOpt - Relay SMS Opt Out Report Prompt
     h dftname(WTRELOPT)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h bnddir('WTBNDDIR')
      *=============================================================*
      *  Created   : 07/27/16                                       *
      *  Programmer: C:136                                          *
      *  Project   : 114WYQ                                         *
      *=============================================================*
      * Revisions:                                                  *
      *                                                             *
      *=============================================================*
     fwtrelopt  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
      *===================================================
      * Standalone fields
      *===================================================
     d strCommand      s            255
      *===================================================
      * Data areas
      *===================================================
     d psds          esds                  extname(szq1p)
     d lastRunValues   ds                  dtaara('WTRELOPT')
     d  svFileName                  250    overlay(lastRunValues)
     d  svPathName                  250    overlay(lastRunValues:*next)
     d  svDartsRmk                    5    overlay(lastRunValues:*next)
      *===================================================
      * Prototypes
      *===================================================
     d/copy sPrCompany
     d/copy wPrUsrprms
     dChkDlo           pr                  extPgm('CHKIFSOBJ')
     d docName                      256    const
     d error                           n
     d GetSuffix       pr                  extpgm('SA030R')
     d  suffix                        3
     d Main            pr
     d ProgramStart    pr
     d QCmdExc         pr                  extpgm('QCMDEXC')
     d  command                            const like(strCommand)
     d  length                       15  5 const
     d RunEdits        pr              n
     d RunReport       pr                  extpgm('WTRELOPTC')
     d  outFileName                 250
     d  outPathName                 250
     d  outDartsRmkCd                 5
     d  outUser                      10
     d SaveParms       pr
      *===================================================
      * Main
      *===================================================
      /free
       ProgramStart();
       Main();
       *Inlr = *On;
       Return;
      /end-free
       //=========================================
       // Main - Main processing
       //=========================================
     p Main            b
      /free
       Dou btnCancel = *on;
          Write RcdComPnl;
          Write RcdTabRpt;
          Exfmt RcdSelect;
          Read RcdComPnl;
          Read RcdTabRpt;
          Select;
          When btnSubmit = *on;
             btnSubmit = *off;
             If RunEdits() = *off;
                SaveParms();
                RunReport(scFileName:scPathName:scDartsRmk:Q1user);
                Leave;
             Endif;
          When btnRefresh = *on;
             btnRefresh = *off;
             ProgramStart();
          Other;
             Iter;
          Endsl;
       Enddo;
      /end-free
     p Main            e
       //=========================================
       // ProgramStart - Initial Program tasks
       //=========================================
     p ProgramStart    b
     d intCo#          s              3  0
     d errorMsg        s              5  0
     d strLibrary      s             10
     d suffix          s              3
      /Free
       Clear rcdSelect;
       aPnlHeight = 770;
       intCo# = userCo#(q1user);
       scCompName = GetCompNam(intCo#);
       In(e) lastRunValues;
       If %error;
          errorMsg = %status;
       Else;
          scFileName = svFileName;
          scPathName = svPathName;
          scDartsRmk = svDartsRmk;
       Endif;
       If errorMsg = 401;
          GetSuffix(suffix);
          strLibrary = 'PDSOBJ' + suffix;
          strCommand = 'CRTDTAARA DTAARA(' + %trim(strLibrary) +
           '/WTRELOPT) TYPE(*CHAR) LEN(505)';
          Monitor;
             Callp QCmdExc(strCommand:%len(strCommand));
          On-error;
             // Do nothing.
          Endmon;
       Endif;
      /End-free
     p ProgramStart    e
       //=========================================
       // Run Edits - return *on if edits don't pass
       //=========================================
     p RunEdits        b
     d RunEdits        pi              n
     d docExists       s               n
     d error           s               n
     d strDocPath      s            256
      /Free
       Clear msFileName;
       Clear erFileName;
       Clear msDocPath;
       Clear erDocPath;
       Clear msDocRmcd;
       Clear erDocRmcd;
       If scFileName = *blanks;
          msFileName = 'The file name is required.';
          erFileName = *on;
          error = *on;
       Endif;
       If scPathName = *blanks;
          msDocPath  = 'The path name is required.';
          erDocPath  = *on;
          error = *on;
       Endif;
       If scDartsRmk = *blanks;
          msDocRmcd  = 'The Darts remark code is required.';
          erDocRmcd  = *on;
          error = *on;
       Endif;
       strDocPath = %trim(scPathName) + %trim(scFileName);
       ChkDlo(strDocPath:docExists);
       If not(docExists);
          error = *on;
          msFileNam1 = 'This file doesn''t exist in the IFS. -
           See tool tip for more assistance.';
          erFileNam1 = *on;
          msDocPath1 = 'This file doesn''t exist on the IFS. -
           See tool tip for more assistance.';
          erDocPath1 = *on;
       Endif;
       Return error;
      /end-free
     p RunEdits        e
      *=====================================================
      * SaveParms - Saves parms to data area which are used
      *             in the report program and for the next
      *             submission
      *=====================================================
     p SaveParms       b
      /Free
       In(e) *Lock lastRunValues;
       If %error;
          Return;
       Endif;
       svFileName = scFileName;
       svPathName = scPathName;
       svDartsRmk = scDartsRmk;
       Out lastRunValues;
      /End-free
     p SaveParms       e
