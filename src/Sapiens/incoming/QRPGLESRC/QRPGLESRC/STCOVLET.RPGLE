     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H BNDDIR('STBNDDIR')
     H*====================================================================
     H/TITLE STCOVLET - Program to attach cover letters to Scriptura requests.
     H*====================================================================
     H*  Description:
     H*
     H*  This program will check if a cover or new/renewal letter is set up
     H*  for a process and attach it to the Scriptura request(s).
     H*
     H*  Created:  08/14/17  C:65  Project: 150WAV
     H*====================================================================
     H*  Revisions:
     H*
     H* 08/03/23 C:340 Project: WC2670                                    *
     H*                Add processing to generate a renewal cover or      *
     H*                business letter in place of a new business letter  *
     H*                when processing a policy services request.         *
     H*====================================================================
     FXMLPREL1  UF   E           K DISK
     FXMLTEML1  UF   E           K DISK
     FSTSP4L1   IF   E           K DISK
     FWDELL1    IF   E           K DISK    usropn
     F                                     infds(dbfds)
     FWMCML     IF   E           K DISK    usropn
     F                                     rename(pcm01:pcm00)
     F                                     infds(dbfds2)
     FFMCML1    IF   E           K DISK    usropn
     F                                     prefix(FM:2)
     F                                     infds(dbfds3)
     FUMPCL11   IF   E           K DISK    usropn
     F                                     rename(ppc01:ppc11)
     F                                     infds(dbfds4)
     D*
     D dbfds           ds
     D  dbrrn                397    400b 0
     D dbfds2          ds
     D  dbrrn2               397    400b 0
     D dbfds3          ds
     D  dbrrn3               397    400b 0
     D dbfds4          ds
     D  dbrrn4               397    400b 0
     D*
     D coverTemplate   s             60    inz(' ')
     D coverPath       s            250    inz(' ')
     D coverLang       s             40    inz(' ')
     D coverType       s            100    inz(' ')
     d coverIndex      s              5  0 inz(0)
     d x@RRN           s              9  0 inz(0)
     D sysDt7          s              7  0
     d xmlFile         s                   like(XPXML)
     D xmlFile2        s             60    inz(' ')
     D xmlLink         s             60    inz(' ')
     D templateFile    s             60    inz(' ')
     D xxMode          s              1
     d massFlag        s              1
     D string          s             10    inz(*blanks)
     D message         s            250    inz(*blanks)
     D xxPre           s              2
     d strLang         s             60
     d strLangCode     s            250

     d StLetter        pr                  ExtPgm('STLETTER')
     d   letMode_                    10    const
     d   xmlFile_                    60
     d   message_                   250    options(*nopass)
     d   mode_                        1    const options(*nopass)
     d   rrn_                         9    const options(*nopass)
     d   prefix_                      2    const options(*nopass)
     d   string_                     10    const options(*nopass)
     d   co#_                         3    const options(*nopass)
     d   fnd_                         3    const options(*nopass)
     d   mass_                        1    const options(*nopass)
     d   link_                       60    const options(*nopass)
     D
     D/copy sprDate
     C*
     C* Key list for XML Prep File
     C*
     C     keyXML        klist
     C                   kfld                    xmlFile
     C*
     C     key1XML       klist
     C                   kfld                    xmlFile2
     C                   kfld                    xmlLink
     C*
     C* Entry list to program.
     C*
     C     *entry        Plist
     C                   Parm                    QQXml            60
     C*
      /free

       sysDt7 = Dateto7(SystemDate());
       xmlFile = %trim(QQXML);

       Chain (xmlFile) xp100;

       Select;
       When XPCat = 'PS';
          Open wdell1;
       When XPCat = 'CS';
          Open wmcml;
       When XPCat = 'MC';
          Open fmcml1;
       When XPCat = 'PC';
          Open umpcl11;
       Endsl;

       //* Single request job
       If XPLink = ' ';
          massFlag = ' ';
          Exsr CoverLetter;
       Else;
       //* Mass request job
          massFlag = 'Y';
          xmlFile2 = ' ';
          xmlLink = XPLink;
          Setll key1XML xp100;
          Reade key1XML xp100;
          Dow not %eof;
             Exsr CoverLetter;
             Reade key1XML xp100;
          Enddo;
       Endif;

       Select;
       When XPCat = 'PS';
          Close wdell1;
       When XPCat = 'CS';
          Close wmcml;
       When XPCat = 'MC';
          Close fmcml1;
       When XPCat = 'PC';
          Close umpcl11;
       Endsl;

       *inlr = *on;
       return;

       // *=====================================================================
       // *  CoverLetter - Check if a cover or new/renewal letter is set up for
       // *                the process and attach it to the current request.
       // *=====================================================================
       Begsr CoverLetter;

       //* Check if there is a cover letter set up for process (group level firs
       Setgt (XPType:'C':XPFnd:sysDt7) psp014;
       Readpe(n) (XPType:'C':XPFnd) psp014;
       Dow not %eof and S4Stat <> 'A';
         Readpe(n) (XPType:'C':XPFnd) psp014;
       Enddo;

       If %eof and XPFnd <> 0;
          Setgt (XPType:'C':0:sysDt7) psp014;
          Readpe(n) (XPType:'C':0) psp014;
          Dow not %eof and S4Stat <> 'A';
            Readpe(n) (XPType:'C':0) psp014;
          Enddo;
       Endif;

       If not %eof;
          coverTemplate = S4Doc;
          coverPath = S4Path;
          coverLang = S4Langd;
          coverIndex = -20;
          Exsr CreateXMLrcd;
       Endif;

       //* Check if there is a new/renewal letter set up for process (group leve
       Setgt (XPType:'N':XPFnd:sysDt7) psp014;
       Readpe(n) (XPType:'N':XPFnd) psp014;
       Dow not %eof and S4Stat <> 'A';
         Readpe(n) (XPType:'N':XPFnd) psp014;
       Enddo;

       If %eof and XPFnd <> 0;
          Setgt (XPType:'N':0:sysDt7) psp014;
          Readpe(n) (XPType:'N':0) psp014;
          Dow not %eof and S4Stat <> 'A';
            Readpe(n) (XPType:'N':0) psp014;
          Enddo;
       Endif;

       If not %eof;
          coverTemplate = S4Doc;
          coverPath = S4Path;
          coverLang = S4Langd;
          coverIndex = -10;
          Exsr CreateXMLrcd;
       Endif;

       Endsr;

       // *=====================================================================
       // *  CreateXMLrcd - Create the XMLPREP record for the notice
       // *=====================================================================
       Begsr CreateXMLrcd;

       message = %trim(XPType);
       templateFile = %trim(XPFile);
       xxPre = ' ';
       string = ' ';

       Select;
       When XPCat = 'PS';
          Chain (XPCo#:XPFnd:XPFyr:XPEmp#:XPDiv) pel01;
        //***  Beg Add  ***  08/03/23  WC2670  ***************
          If ELnwRw = 'R';
             coverTemplate = S4rDoc;
             coverPath = S4rPath;
             coverLang = S4rLangD;
          Endif;
        // Do nothing if no cover letter template available
          If coverTemplate = *blank;
             Leavesr;
          Endif;
        //***  End Add  ***  08/03/23  WC2670  ***************
          x@RRN = dbRRN;
          xxMode = 'E';
          coverType = 'Cover Letter Policy';
          XPData = 'Y';
          Update xp100;
       When XPCat = 'CS';
          Chain (XPCo#:XPFnd:XPEmp#:XPDiv:XPFyr:XPCase) pcm00;
          x@RRN = dbRRN2;
          xxMode = 'C';
          coverType = 'Cover Letter Claim';
          XPData = 'Y';
          Update xp100;
       When XPCat = 'MC';
          Chain (XPCo#:XPSub#:XPFnd:XPFyr:XPCase) pcm01;
          x@RRN = dbRRN3;
          xxMode = 'M';
          coverType = 'Cover Letter Medical Case';
          XPData = 'Y';
          Update xp100;
       When XPCat = 'PC';
          Chain (XPCo#:XPSub#:XPFnd:XPFyr:XPCase:XPCert:XPExt) ppc11;
          x@RRN = dbRRN4;
          xxMode = 'P';
          coverType = 'Cover Letter Pre Cert';
          XPData = 'Y';
          Update xp100;
       Other;
          x@RRN = 0;
          xxMode = ' ';
          coverType = 'Cover Letter';
          Update xp100;
       Endsl;

       //* Request record for letter.
       StLetter('TEMPLRQST ':templateFile:message:xxMode:%editc(x@RRN:'X'):
        xxPre:string:%editc(XPCo#:'X'):%editc(XPFnd:'X'):massFlag:xmlLink);

       Chain (templateFile) xt100;

       XTOrder = coverIndex;
       XTType =  %trim(coverType);
       XTDcid = coverTemplate;
       XTPath = coverPath;

       strLang = %trim(coverLang);
       Clear strLangCode;
       StLetter('LANGUAGE':strLang:strLangCode);
       XTLang = %trim(strLangCode);

       Update xt100;

       Endsr;
       // *=====================================================================
      /end-free
