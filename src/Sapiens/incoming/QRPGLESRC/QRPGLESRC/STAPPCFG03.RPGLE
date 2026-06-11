     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h*
     h*  Description: Response program to serve Output Control Configuration detail box
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: STAPPCFG
     h*  Calls to:
     h*
     h*  Created on 11/05/2015 by C:33  for SY371
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     Fstspp     if   e             disk    rename(psp01:psp0p)
      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable

     d* CheckBox Conversion Procedure
     d ChkBoxCnv       pr             3A
     d                                1    const

      *entry
     DOutCtlCfg        pr                  ExtPgm('STAPPCFG03')
     D                                9  0
     D                            32767
     D                                1
      *
     DOutCtlCfg        pi
     D inRRN                          9  0
     D outJSON                    32767
     D RtnCode                        1
      *
      /Free

       Chain(n) inRRN psp0p;
       outJSON = ' ';

       Exsr BuildData;

       RtnCode = ' ';
       Return;

       //*************************************************************
       //* BuildData for detail box
       //*************************************************************
       Begsr BuildData;

       BuildJSON(outJSON :'OutOutput':%trim(SPtype));

       // Hardcopy
       BuildJSON(outJSON :'OutSpooledFileRetired':ChkBoxCnv(SPsplR));

       // Scriptura
       BuildJSON(outJSON :'OutScripturaSndDataQ'       :%trim(SPsDtqS));
       BuildJSON(outJSON :'OutScripturaRcvDataQ'       :%trim(SPsDtqR));
       BuildJSON(outJSON :'OutScripturaDefaultTemplate':%trim(SPsDefT));
       BuildJSON(outJSON :'OutScripturaClientTemplate' :%trim(SPsCltT));
       BuildJSON(outJSON :'OutScripturaXMLFolder'      :%trim(SPsXmlF));
       BuildJSON(outJSON :'OutScripturaXMLFolderArc'   :%trim(SPsXmlA));

       // 3rd Party XML
       BuildJSON(outJSON :'Out3rdSndDataQ' :%trim(SP3DtqS));
       BuildJSON(outJSON :'Out3rdRcvDataQ' :%trim(SP3DtqR));
       BuildJSON(outJSON :'Out3rdXMLFolder':%trim(SP3XmlF));

       endSr;
      /End-Free
       //===================================================================
       // ProcReqDel  - Process the pending check request deletion
       //===================================================================
     P ChkBoxCnv       B                   export
     D ChkBoxCnv       PI             3A
     D  @Value                        1A   const
      /Free
       Select;
          When @Value = 'Y';
             Return ('Yes');
          When @Value = 'N' or @Value = ' ';
             Return ('No ');
          Other;
             Return ('N/A');
       endsl;
      /End-Free
     P ChkBoxCnv       E
