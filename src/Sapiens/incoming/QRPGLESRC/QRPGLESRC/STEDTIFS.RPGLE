     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     h*====================================================================
     h/title Edit IFS Objects
     h*====================================================================
     h* Created: 10/26/12   C:33   Project: SY266
     h*====================================================================
     h*====================================================================
     h* Revisions:
     h*
     h*====================================================================
     Fstedtifs  cf   e             workstn Handler('PROFOUNDUI(HANDLER)')
     F                                     usropn

     DGetNameFromPath  pr          1024
     D inPath                      1024    const

     DGetDirFromPath   pr          1024
     D inPath                      1024    const

     DEntryList        pr                  extPgm('STEDTIFS')
     D inPath                      1024    options(*varsize)
     D outRtCd                        1    options(*nopass)
     DEntryList        pi
     D inPath                      1024    options(*varsize)
     D outRtCd                        1    options(*nopass)

     D psds          esds                  extname(szq1p)

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
       inPath = %Xlate('\':'/':inPath);
       inLen = %len(%trim(inPath));
       %len(strPath) = %len(inPath);
       strPath = inPath;

       scDocId       = GetNameFromPath(strPath);
       scFolder      = GetDirFromPath(strPath);
       scUser        = q1User;
       scMessage     = 'Opening ' + scDocId;
       scPnlTitle    = scDocId;

       open stedtifs;

       ExFmt rcdEdtIfs;

       close stedtifs;

       Exsr ProgramEnd;

       // Program End
       Begsr ProgramEnd;
          If %parms >= 2;
             outRtCd = strRtCd;
          Endif;
          *inlr = *on;
          Return;
       Endsr;

      /End-free
     P*====================================================================
     P* Get the directory from the full path
     P*====================================================================
     PGetDirFromPath   b
     DGetDirFromPath   pi          1024
     D inPath                      1024    const

     Dend              s              5  0
     Di1               s              5  0 inz(1)

      /Free
       Dou i1 = 0;
          i1 = %scan('/':inPath:i1+1);
          If i1 <> 0;
             end = i1 - 1;
          Endif;
       Enddo;
       return %subst(inPath:1:end);
      /End-free

     PGetDirFromPath   e
     P*====================================================================
     P* Get the name from the full path
     P*====================================================================
     PGetNameFromPath  b
     DGetNameFromPath  pi          1024
     D inPath                      1024    const

     Dstart            s              5  0
     Di1               s              5  0 inz(1)

      /Free
       Dou i1 = 0;
          i1 = %scan('/':inPath:i1+1);
          If i1 <> 0;
             start = i1 + 1;
          Endif;
       Enddo;
       return %subst(inPath:start);
      /End-free

     PGetNameFromPath  e
