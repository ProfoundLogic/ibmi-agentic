     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     h*====================================================================
     h/title Display IFS Objects
     h*====================================================================
     h* Created: 10/26/12   C:33   Project: SY266
     h*====================================================================
     h*====================================================================
     h* Revisions:
     h*
     h*====================================================================
     DStDspIFS         pr                  extPgm('STDSPIFS')
     D inPath                      1024    options(*varsize)
     D outRtCd                        1    options(*nopass)
     DStDspIFS         pi                  opDesc
     D inPath                      1024    options(*varsize)
     D outRtCd                        1    options(*nopass)

     DGetNameFromPath  pr          1024
     D inPath                      1024    const

     DGetIFSType       pr          1024    opDesc
     D inPath                      1024    const options(*varsize)

     DChk              pr
     D IfsObjName                  1024a   const
     D IfsObjFnd                       n
     D IfsObjSize                    10i 0
     D IfsCrtStam                      z
     D IfsObjType                    10a
     D IfsCodPage                    10a

     DCEEDOD           PR
     D parmNum                       10I 0 const
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               12A   options(*omit)

     D/copy sPrAtrium
     D/copy sPrPrprty

     DconPrefix        c                   '/iWAFile'
     DstrMessage       s            256
     DstrName          s            720
     DstrPath          s           1024    varying
     DstrType          s             10
     DstrTabName       s             50
     DstrIAccessURL    s            256
     DstrRtCd          s              1    inz
     DoutURL           s            720

     D* parameters passed to CEEDOD
     D descType        s             10I 0
     D dataType        s             10I 0
     D descInfo1       s             10I 0
     D descInfo2       s             10I 0
     D inLen           s             10I 0
     D hexLen          s             10I 0

      /Free
       // Get the length of incoming parm and set size correctly
       //Callp CEEDOD(1:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       //%len(strPath) = inLen;
       inPath = %Xlate('\':'/':inPath);
       inLen = %len(%trim(inPath));
       %len(strPath) = %len(inPath);
       strPath = inPath;

       strName       = GetNameFromPath(strPath);
       strTabName    = strName;
       strType       = GetIFSType(strPath);
       strMessage    = 'Click to open ' + %trim(strName);
       strIAccessURL = IAccessURL;

       If strIAccessURL = *blanks;
          strRtCd = '1';
          Exsr ProgramEnd;
       Endif;

       outURL = strTabName + %trim(strIAccessURL) + conPrefix;

       Select;
       When strType = '*DOC';
          //Print document
          //STDSPSPLF
          //Return
       When strType = '*DIR';
          outURL = %trim(outURL) + 'List?filePath=' + %trim(strPath);
       Other;
          outURL = %trim(outURL) + 'Download/' + %trim(strName) +
           '?filePath=' + %trim(strPath);
       Endsl;

       Callp LaunchNewTab(strMessage:strTabName:outURL);

       Exsr ProgramEnd;

       Begsr ProgramEnd;
       If %parms >= 2;
          outRtCd = strRtCd;
       Endif;
       *inlr = *on;
       Return;
       Endsr;

      /End-free
     P*====================================================================
     P* Get the name from the full path
     P*====================================================================
     PGetNameFromPath  b
     DGetNameFromPath  pi          1024
     D inPath                      1024    const

     Dstart            s              5  0
     Di1               s              5  0 inz(1)
     Di2               s              5  0 inz(1)

      /Free
       Dou i1 = 0;
          i1 = %scan('/':inPath:i1+1);
          If i1 <> 0;
             start = i1 + 1;
          Endif;
       Enddo;
       i2 = %checkr(' ':%trim(inPath));
       return %subst(inPath:start);
      /End-free

     PGetNameFromPath  e



     P*====================================================================
     P* Get the name from the full path
     P*====================================================================
     PGetIFSType       b
     DGetIFSType       pi          1024    opDesc
     D inPath                      1024    const options(*varsize)

     Dstart            s              5  0
     DstrPath          s           1024    varying
     DoutFound         s               n
     DoutSize          s             10i 0
     DoutTimeStamp     s               z
     DoutType          s             10a
     DoutCodePage      s             10a

     D* parameters passed to CEEDOD
     D descType        s             10I 0
     D dataType        s             10I 0
     D descInfo1       s             10I 0
     D descInfo2       s             10I 0
     D inLen           s             10I 0
     D hexLen          s             10I 0

      /Free
       // Get the length of incoming parm and set size correctly
       Callp CEEDOD(1:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       %len(strPath) = inLen;
       strPath = inPath;

       callp Chk(strPath:outFound:outSize:outTimeStamp:outType:outCodePage);

       return outType;
      /End-free

     PGetIFSType       e
