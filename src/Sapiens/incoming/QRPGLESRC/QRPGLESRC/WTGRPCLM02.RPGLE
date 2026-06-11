     hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for Auto Letters
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTGRPCLMCG
     h*  Calls to:
     h*
     h*  Created on 05/10/2021 by C:318 for Project WC2196
     h**********************************************************************
     h* Revisions:
     h*
     h**********************************************************************
     fwaclp     if   e             disk    rename(pcl01:pcl0s)
     fwmdol     if   e           k disk
     fwmofl     if   e           k disk

     d/copy sPrAtrium

      * *ENTRY
     dwtGrpClm02       pr                  extpgm('WTGRPCLM02')
     d                                9  0
     d                            32767
     d                                1

     dwtGrpClm02       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strDoc          s             55
     d strOfc          s             50
     d KeyDept         s                   like(DODEPT)

     C*
     C*   KEY LIST FOR THE GENERIC TABLE FILE
     C     keyWmdol      klist
     C                   kfld                    CLco#
     C                   kfld                    KeyDept
     C                   kfld                    CLdpcd
     C                   kfld                    CLfldr
     C                   kfld                    CLdcid
     C     keyWmofl      klist
     C                   kfld                    CLco#
     C                   kfld                    CLofc#
     C*

      /Free

       Chain inRRN pcl0s;

       Exsr BuildData;

       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlOffice':%trim(strOfc));
       BuildJSON(outJSON :'OutDtlStateACL':%trim(CLst));
       BuildJSON(outJSON :'OutDtlDepartmentCode':%trim(CLdpcd));
       BuildJSON(outJSON :'OutDtlDocId':%trim(strDoc));
       BuildJSON(outJSON :'OutDtlFolder':%trim(CLfldr));
       BuildJSON(outJSON :'OutDtlNumberofCopies':%editc(CL#cpy:'N'));
       BuildJSON(outJSON :'OutDtlOutQ':%trim(CLoutq));
       If CLltfl = 'I';
          BuildJSON(outJSON :'OutDtlLTBegVal':'Indemnity');
       Endif;
       If CLltfl = 'M';
          BuildJSON(outJSON :'OutDtlLTBegVal':'Medical');
       Endif;
       If CLltfl = 'R';
          BuildJSON(outJSON :'OutDtlLTBegVal':'Record Only');
       Endif;
       If CLltfl = *Blanks;
          BuildJSON(outJSON :'OutDtlLTBegVal':*Blanks);
       Endif;
       If CLltto = 'I';
          BuildJSON(outJSON :'OutDtlLTEndVal':'Indemnity');
       Endif;
       If CLltto = 'M';
          BuildJSON(outJSON :'OutDtlLTEndVal':'Medical');
       Endif;
       If CLltto = 'R';
          BuildJSON(outJSON :'OutDtlLTEndVal':'Record Only');
       Endif;
       If CLltto = *Blanks;
          BuildJSON(outJSON :'OutDtlLTEndVal':*Blanks);
       Endif;
       If CLandr = 'O';
          BuildJSON(outJSON :'OutDtlANDOR':'OR');
       Endif;
       If CLandr = 'A';
          BuildJSON(outJSON :'OutDtlANDOR':'AND');
       Endif;
       If CLandr = *Blanks;
          BuildJSON(outJSON :'OutDtlANDOR':*Blanks);
       Endif;
       If CLcsfr = 'O';
          BuildJSON(outJSON :'OutDtlCSBegVal':'Open');
       Endif;
       If CLcsfr = 'C';
          BuildJSON(outJSON :'OutDtlCSBegVal':'Closed');
       Endif;
       If CLcsfr = 'N';
          BuildJSON(outJSON :'OutDtlCSBegVal':'New');
       Endif;
       If CLcsfr = *Blanks;
          BuildJSON(outJSON :'OutDtlCSBegVal':*Blanks);
       Endif;
       If CLcsto = 'O';
          BuildJSON(outJSON :'OutDtlCSEndVal':'Open');
       Endif;
       If CLcsto = 'C';
          BuildJSON(outJSON :'OutDtlCSEndVal':'Closed');
       Endif;
       If CLcsto = *Blanks;
          BuildJSON(outJSON :'OutDtlCSEndVal':*Blanks);
       Endif;

       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

       // Document Id and description
       strDoc = CLdcid;
       KeyDept= 'CS';
       chain keyWmdol wmdol;
       strDoc = %trim(CLdcid) + ' - ' + DOdesc;

       // office name
       chain keyWmofl wmofl;
       strOfc =  %trim(CLofc#) + ' - ' + Ofname;

       endsr;
      /End-Free
