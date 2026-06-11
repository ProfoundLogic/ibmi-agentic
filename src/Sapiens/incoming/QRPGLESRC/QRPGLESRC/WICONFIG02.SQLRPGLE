     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Portal Configuration parent information
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Created on 09/05/18   C:33  Project: SY390
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable

      *entry
     DGetParentInfo    pr                  ExtPgm('WICONFIG02')
     D                                9  0
     D                            32767
     D                                1
     DGetParentInfo    pi
     D inId#                          9  0
     D outJSON                    32767
     D RtnCode                        1

     dintCo#           s              3  0
     dstrChoicesOperator...
     d                 s            100
     dstrchoicesOperand...
     d                 s            100
     dstrCssClassOperand...
     d                 s            100
     dstrFieldTypeOperand...
     d                 s            100

     DdsWIQUP        e ds                  extname(WIQUP)    inz qualified
     DdsWIQUPexp     e ds                  extname(WIQUPexp) inz qualified

     d psds          esds                  extname(szq1p)

      /Free
       Clear outJSON;
       intCo# = UserCo#(q1User);
       Reset dsWIQUP;
       Exec SQL
        select *
         into :dsWIQUP
         from WIQUP
         where QUco# = :intCo# and QUid# = :inId#
         limit 1;
       If sqlStt <> '00000';
          BuildJSON(outJSON :'Error':'Parent information not found. SQL code: '+
           sqlStt);
          RtnCode = 'X';
          Return;
       Endif;

       BuildJSON(outJSON :'QUquestion':%trim(dsWIQUP.QUquestion));
       BuildJSON(outJSON :'QUanswer'  :%trim(dsWIQUP.QUanswer));

       Exsr GetAnswerConfig;
       Exsr GetExposureConfig;

       RtnCode = ' ';
       *inlr = *on;
       Return;

       //===============================================================
       // Get answer screen configuration
       //===============================================================
       Begsr GetAnswerConfig;
        Select;
        When dsWIQUP.QUanswer = 'Yes/No';
           strChoicesOperator  = 'Exists,Equals';
           strchoicesOperand   = 'Yes,No';
           strCssClassOperand  = 'dropdown';
           strFieldTypeOperand = 'select box';
        When dsWIQUP.QUanswer = 'Count';
           strChoicesOperator  = 'Exists,Equals,Not Equals,Less Than,' +
            'Less Than or Equals,Greater Than, Greater Than or Equals';
           strchoicesOperand   = '';
           strCssClassOperand  = 'textbox';
           strFieldTypeOperand = 'textbox';
        When dsWIQUP.QUanswer = 'Amount';
           strChoicesOperator  = 'Exists,Equals,Not Equals,Less Than,' +
            'Less Than or Equals,Greater Than, Greater Than or Equals';
           strchoicesOperand   = '';
           strCssClassOperand  = 'textbox';
           strFieldTypeOperand = 'textbox';
        Endsl;
        BuildJSON(outJSON :'choicesOperator' :%trim(strChoicesOperator));
        BuildJSON(outJSON :'choicesOperand'  :%trim(strchoicesOperand));
        BuildJSON(outJSON :'cssClassOperand' :%trim(strCssClassOperand));
        BuildJSON(outJSON :'fieldTypeOperand':%trim(strFieldTypeOperand));
       Endsr;

       //===============================================================
       // Get exposure information
       //===============================================================
       Begsr GetExposureConfig;
        Exec SQL
         select *
          into :dsWIQUPexp
          from WIQUPEXP
          where QUco# = :intCo# and QUid# = :inId#
          limit 1;
        BuildJSON(outJSON :'rateSetCode':%trim(dsWIQUPexp.QUrateSet));
        BuildJSON(outJSON :'ratingState':%trim(dsWIQUPexp.QUst));
       Endsr;
      /End-free
