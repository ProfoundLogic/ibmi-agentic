     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*
     H****************************************************************
     H*  LODSTMCP - Program to add records to STCMP
     H*
     H*  Description: This pgm will add records to STCMP
     H*
     H*  Created    : 04/28/2015  Programmer: 212  Project: MC377
     H*
     H*  Called By  :
     H*  Calls To   :
     H*
     H****************************************************************
     H*  Revisions:
     H*
     H*  07/08/15 C: 079 Project: MC377
     H*                  Hard code ID and title
     H****************************************************************
    F*
     fstmcl     uf a e           k disk
     fsxmcp     if   e             disk    rename(pmc01:pmc0x) usropn
     f                                     prefix(X_)
    D*
     d** Procedure definitions
     d*
     d**Entry
     d LodSTMCP        pr                  ExtPgm('LODSTMCP')
     d outCltID                       3
     d*
     d RunCmd          pr                  extPgm('QCMDEXC')
     d inCmd                       2750    const
     d inCmdLen                      15  5 const
     d*
     d*
     d* data structures
     d*
     d psds          esds                  extname(szq1p)
     d*
     D qca             s             40    dim(1)  ctdata perrcd(1)
     D*** Beg Add *** 07/08/15 **************************************
     D strID           s             25    inz('Load ICD-10')
     D strMsg          s            256
     D strMkDir        s            110
     D*
     D GetSuffix       pr                  extpgm('SA030R')
     D outSuffix                      3
     D outPFlr                       80
     D*
     D outSuffix       s              3
     D outPFlr         s             80
     D*** End Add *** 07/08/15 **************************************
     d** Standalone variables
     d kkId            s                   like(MCID) inz
    C*
      /free
        //*** Beg Add *** 07/08/15 ***********************************
       callp getSuffix(outSuffix:outPflr);
       If outPflr = *blanks;
         Monitor;
            RunCmd('MKDIR DIR(''/ICD10'')':21);
         On-error;
         Endmon;
       Else;
           Monitor;
            strMkDir = 'MKDIR DIR(''' + %trim(outPflr) + '/ICD10''' +
                       ')';
            RunCmd(strMkDir:110);
           On-error;
           Endmon;
       Endif;

        strMsg   = 'Please make sure that the most current ICD10 CSV files';
        strMsg   = %trim(strMsg) + ' (In the format of FAICD10PCS.csv an';
        strMsg   = %trim(strMsg) + 'd FAICD10CM.csv) have been loaded';
        strMsg   = %trim(strMsg) + ' to the /ICD10 folder.';
        //*** End Add *** 07/08/15 ***********************************

        // Override and open SXMCP

       Monitor;
          RunCmd('OVRDBF FILE(SXMCP) TOFILE(PDSDATA/SXMCP)':40);
       On-error;
       Endmon;

       Open SXMCP;

        // read SXMCP

         Setll 1 pmc0x;
         Read pmc0x;
         DoW not %eof(sxmcp);

           // look if a record exist in STMCP for same key
           kkId = X_MCid;
           Chain (kkId) pmc01;

           If %found(stmcl);
             exsr MvData;
             update pmc01;
           Else;
             exsr MvData;
        //*** Beg Chg *** 07/08/15 **************************************
        //***MCid = X_MCid;
             MCid    = strID;
        //*** End Chg *** 07/08/15 **************************************
             write pmc01;
           Endif;

           // read next
           Read pmc0x;
         EndDo;

       Monitor;
          RunCmd('CLOF OPNID(SXMCP)':17);
       On-error;
       Endmon;

       Close SXMCP;

         *inlr = *on;

        //================================================================
        // MVDATA - move data
        //================================================================
         begsr MvData;

           MCtitle = X_MCtitle;
        //*** Beg Chg *** 07/08/15 **************************************
        //******MCmsg   = X_MCmsg;
           MCmsg   = strMsg;
        //*** End Chg *** 07/08/15 **************************************
           MCbtn1  = X_MCbtn1;
           MCbtn2  = X_MCbtn2;
           MCbtn3  = X_MCbtn3;
           MCcmd1  = X_MCcmd1;
           MCcmd2  = X_MCcmd2;
           MCcmd3  = X_MCcmd3;
           MCuser  = q1user;
           MCpgm   = q1pgm;
           MCtime  = %dec(%char(%time():*iso0):6:0);
           MCdt    = %dec(%char(%date():*cymd0):7:0);

         endsr;

      /end-free
**  (QCMDEX)
OVRDBF FILE(FXMCP) TOFILE(PDSDATA/FXMCP)
