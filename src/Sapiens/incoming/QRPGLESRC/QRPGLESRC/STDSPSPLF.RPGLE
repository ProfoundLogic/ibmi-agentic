     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     h*====================================================================
     h/title Display Spoolfile
     h*====================================================================
     h* Created: 10/26/12   C:33   Project: SY266
     h*====================================================================
     h*====================================================================
     h* Revisions:
     h* 09/05/17 C: 079 Project: SY395
     h*                 Add File(PDF) processing
     h*
     h*====================================================================
     DStDspSplF        pr                  extPgm('STDSPSPLF')
     D inSplF                        10
     D inSpl#                         6  0
     D inJob                               likeDs(dsJob)
     D outRtCd                        1    options(*nopass)
     DStDspSplF        pi                  opDesc
     D inSplF                        10
     D inSpl#                         6  0
     D inJob                               likeDs(dsJob)
     D outRtCd                        1    options(*nopass)

     D/copy sPrAtrium
     D/copy sPrPrprty

     DdsJob            ds
     D dsJobName                     10
     D dsJobUser                     10
     D dsJob#                         6

     DconPrefix        c                   '/iWAPDFDownload/'
     DstrAddress       s            670
     DstrMessage       s            256
     DstrTabName       s             50
     DstrIAccessURL    s            256
     DstrRtCd          s              1    inz
     DoutURL           s            720
     Dtesting          s              1n   inz('0')
     Dlocal            s              1n   inz('1')

      /Free
       strTabName    = inSplF;
       strMessage    = 'Click to open ' + %trim(inSplF);
       strIAccessURL = IAccessURL;

       If strIAccessURL = *blanks;
          strRtCd = '1';
          Exsr ProgramEnd;
       Endif;

       strAddress = %trim(strIAccessURL) + conPrefix
        + %trim(inSplF) + '.pdf?destType=brws&transPDF=Run'
        + '&jobInfo=' + %trim(inJob.dsJob#)
        + '/' + %trim(inJob.dsJobUser)
        + '/' + %trim(inJob.dsJobName)
        + '&fileName=' + %trim(inSplF)
        + '&fileNumber=' + %trim(%char(inSpl#))
        + '&saveStatus=*NO&form-charset=utf-8';

       outURL = strTabName + %trim(strAddress);

       Callp LaunchNewTab(strMessage:strTabName:outURL);

       Exsr ProgramEnd;

       Begsr ProgramEnd;
       If %parms >= 4;
          outRtCd = strRtCd;
       Endif;
       *inlr = *on;
       Return;
       Endsr;

      /End-free
