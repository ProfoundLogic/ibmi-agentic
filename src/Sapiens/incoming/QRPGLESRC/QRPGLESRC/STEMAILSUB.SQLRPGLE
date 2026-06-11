     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h/title StEmailSub - Creates Email Link
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
      *=====================================================================
      *  Description: Response program to serve Owner/Office detail box
      *               - Activation group should be *CALLER
      *               - Do not seton LR, just do a return
      *               - This process should not lock any file
      *               - This process should only read files, no updates/writes should be made
      *
      *  Called from: Email widget via JS thru AJAX
      *  Calls to:
      *
      *  Created on 04/17/2013 by C:191 for Project Transform
      *=====================================================================
      * Revisions:
      *
      *  09/06/16 C:136 Support: 42962
      *                -Add processing for SIS Project Elevate
      *                -Remove copy code and make files usropn
      *                -Changes not commented
      *
      *  10/22/14 C:33  Redmine: 4658
      *                 Do not include '-' when no context included
      *
      *  03/31/14 C:65  Redmine: 3886
      *                 Update the processing to only return email and subject
      *                 unless the dartMail property is true, then return all info.
      *
      *  08/21/13 C:189 Redmine: 1862
      *                 Some format changes are required in the body of the
      *                 mail
      *
      *=====================================================================
     fwdelp     if   e             disk    rename(pel01:pel0p) usropn
     fwmcmp     if   e             disk    rename(pcm01:pcm0p) usropn
     fwmcml1    if   e           k disk    usropn
     fwmeml     if   e           k disk    rename(pem01:pem00) usropn

     dBuildEmailID     pr                  ExtPgm('STEMAILSUB')
     d                                9  0
     d                              512
     d                            32767
     d                                1

     dBuildEmailID     pi
     d inRRN                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d RtnCode                        1

     dBuildJSON        pr
     d inJSON                     32767    Options(*VarSize)
     d inFieldID                     50    const
     d inFieldVal                   512    const

     dCEEDOD           pr
     d parmNum                       10I 0 const
     d                               10I 0
     d                               10I 0
     d                               10I 0
     d                               10I 0
     d                               10I 0
     d                               12A   options(*omit)
     d*
     d* parameters passed to CEEDOD
     d*
     d descType        s             10I 0
     d dataType        s             10I 0
     d descInfo1       s             10I 0
     d descInfo2       s             10I 0
     d inLen           s             10I 0
     d hexLen          s             10I 0

     dCiEmailSub       pr                  extpgm('CIEMAILSUB')
     d  rrnType                       2
     d  inRRn                         9  0
     d  subject                     255

     dCvtHexToChar     pr         32767
     d hexVal                     65534A   options(*varsize)
     d inSize                        10I 0 value options(*nopass)

     dGetClaimName     pr            50
     d co#                            3  0 const
     d group                          3  0 const
     d yr                             3  0 const
     d claim#                         7  0 const

     dGetClientID      pr                  extpgm('RTVCLTID')
     d outCltID                       3

     dGetEmpFullName   pr           100
     d emp#                           9  0                  const
     d div                            5  0                  const
     d emrrn                          9  0 options(*nopass)
     d group                          3  0 options(*nopass) const
     d co#                            3  0 options(*nopass) const
     d nametype                       3    options(*nopass) const

     dHexToChar        pr                  extProc('cvtch')
     d szCharVal                  32767A   options(*varsize)
     d szHexVal                   65534A   options(*varsize)
     d nSrcLen                       10I 0 value

     dRmvExceptJSON    pr         32766
     d ##inField                  32766    options(*varsize) const
     d ##incSize                      5  0 options(*nopass)

     d psDs          esds                  extName(SZQ1P)
     d numEdtCde       c                   const('N')
     d #ofExceptJSON   c                   10
     d excCharJSON     s              2    dim(#ofExceptJSON) ctdata perrcd(1)
     d replaceJSON     s              5    dim(#ofExceptJSON) alt(excCharJSON)

     d                 ds
     d dartPlKey               1     40
     d company#                1      3  0
     d group#                  4      6  0
     d employer#               7     15  0
     d division               16     20  0
     d quote                  40     40

     d                 ds
     d dartClKey               1     40
     d company#c               1      3  0
     d group#c                 4      6  0
     d year                    7      9  0
     d case#                  10     16  0

     d xxRRN           s              9  0 inz
     d rrnType         s              2    inz
     d inQuote         s              1    inz
     d emailString     s           1024    inz
     d emailAddress    s             60    inz
     d dartMail        s            128    inz
     d dartMailInfo    s            400    inz
     d propValue       s            255    inz
     d isDartMail      s              1n   inz
     d keyType         s             10    inz
     d dartKey         s             40    inz
     d contextName     s            100    inz
     d clientID        s              3    inz
     d contextNum      s             60    inz
     d startPos        s              5  0 inz
     d endPos          s              5  0 inz
     d len             s              5  0 inz
     d value           s            128    inz
     d subject         s            255    inz
     d nameType        s              3

      /Free
       //genparm format: ,Ty,eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee,Q
       // Ty is type of RRN - policy or claim, eeee... is 60 char email address
       // Q is quote flag
       outJSON = ' ';
       Exsr ProcessGenParm;
       If RtnCode = 'X';
         RtnCode = ' ';
         BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
         Return;
       Endif;
       Select;
       When rrnType = 'P';
          Open WDELP;
          Chain(n) inRRN pel0p;
          Close WDELP;
          nameType = 'DBA';
          contextName = GetEmpFullName(ELemp#:ELdiv:xxRRN:ELfnd:ELco#:nameType);
          contextNum = 'Policy number: ' + %Editc(ELfnd:'X') + '-' +
            %Editc(ELemp#:'X') + '-' + %Editc(ELdiv:'X');
          keyType = 'EMPLOYER';
          company# = ELco#;
          group# = ELfnd;
          employer# = ELemp#;
          division = ELdiv;
          If inQuote = 'Y';
            quote = 'Q';
          Else;
            quote = ' ';
          EndIf;
          dartKey = dartPlKey;
       When rrnType = 'C';
          Open WMCMP;
          Chain(n) inRRN pcm0p;
          Close WMCMP;
          contextName = GetClaimName(CMco#:CMfnd:CMfyr:CMcase);
          contextNum = 'Claim number: ' + %Editc(CMfnd:'X') + '-' +
            %Editc(CMfyr:'X') + '-' + %Editc(CMcase:'X');
          keyType = 'CLAIM';
          company#c = CMco#;
          group#c = CMfnd;
          year = CMfYr;
          case# = CMcase;
          dartKey = dartClKey;
       When rrnType = 'SP' or rrnType = 'SC';
          Callp(e) CiEmailSub (rrnType:inRRn:subject);
          If %error;
             BuildJSON(outJSON :'RtnCode' :'Call to CIEMAILSUB failed.');
             Return;
          Endif;
       EndSl;

       Select;
       When contextName <> *blanks and contextNum <> *blanks;
          subject =  %Trim(contextName) + ' - ' + contextNum;
       When contextName <> *blanks;
          subject =  %Trim(contextName);
       When contextNum <> *blanks;
          subject = contextNum;
       Endsl;

       EXEC SQL select ATTRIBUTE_VALUE into :propValue from
         GLOBAL_PROPERTIES where ATTRIBUTE_NAME = 'dartmail.flag';

       If %trim(propValue) = 'yes' or %trim(propValue) = 'on';
          isDartMail = *On;
       Else;
          isDartMail = *Off;
       Endif;

       //* Always return email and subject.
       outJSON = ' ';
       BuildJSON(outJSON :'email' :%Trim(emailAddress));
       BuildJSON(outJSON :'subject' :%Trim(subject));

       If isDartMail;
          EXEC SQL select ATTRIBUTE_VALUE into :propValue from
           GLOBAL_PROPERTIES where ATTRIBUTE_NAME = 'dartmail.address';
          dartMail = %Trim(propValue);
          GetClientId(clientID);
          BuildJSON(outJSON :'Bcc' :%Trim(dartMail));
          BuildJSON(outJSON :'userid' :%Trim(q1user));
          BuildJSON(outJSON :'clientid' :%Trim(clientId));
          BuildJSON(outJSON :'keytype' :%Trim(keyType));
          BuildJSON(outJSON :'dartkey' :%Trim(dartKey));
       Endif;

       RtnCode = ' ';
       Return;
       //*=================================================
       // Process genparm
       //*=================================================
       BegSr ProcessGenParm;
       startPos = 0;
       endPos = 0;
       Monitor;
       // extract type
         startPos = %Scan(',':inGenParm)+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;
         rrnType = %trim(value);
       // extract email address
         startPos = endPos+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;
         emailAddress = %trim(value);
       // quote flag
         startPos = endPos+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;
         inQuote = %trim(value);
         Rtncode = ' ';
       On-Error;
         Rtncode = 'X';
       EndMon;
       EndSr;
       //*=================================================
       // extract values from genparm
       //*=================================================
       BegSr ExtractValue;
       value = *blanks;
       If startPos > 0;
         endPos = %Scan(',':inGenParm:startPos);
         If endPos > 0;
           len = endPos-startPos;
           If len > 0;
             value = %Subst(inGenParm:startPos:len);
           Endif;
         Endif;
       Endif;
       EndSr;
      /End-free
      *=====================================================================
      * BuildJSON - Build JSON string for respons
      *          Parm 1 = JSON string (32767 varsize)
      *          Parm 2 = Field ID (50A)
      *          Parm 3 = Field Value (512A)
      *=====================================================================
     pBuildJSON        b
     dBuildJSON        pi
     d inJSON                     32767a   Options(*VarSize)
     d inFieldID                     50    const
     d inFieldVal                   512    const

     D strJSON         s          32767a   varying
     D strFieldVal     s                   like(inFieldVal)
     D lastChar        s              1    inz

      /Free
       If inFieldId = *blanks;
         Return;
       EndIf;
       strJSON = %TrimR(inJSON);
       lastChar = *Blanks;
       If strJSON <> *Blanks;
        //get Last char
         lastChar = %Subst(strJSON:%Len(strJSON):1);
       Endif;
       If strJSON<>*blanks And lastChar<>'{' And lastChar<>'['
        And lastChar<>':';
          strJSON += ', ';
       EndIf;
       Monitor;
          strFieldVal = %trim(RmvExceptJSON(%TrimR(inFieldVal)));
       On-Error;
          strFieldVal = %TrimR(inFieldVal);
       Endmon;
       strJSON += '"' + %TrimR(inFieldId) + '": "' + %TrimR(strFieldVal) + '"';
       inJSON = strJSON;
       Return;
      /End-free
     pBuildJSON        e
      *=====================================================================
      * CvtHexToChar - Converts from hexadecimal text to the character form ng
      *                 of the hex values
      * Parameter 1 - character field (any length, 2x's size of return)
      * Parameter 2 - size to be converted (size of parm 1)
      * Returns - character field
      *=====================================================================
     pCvtHexToChar     b
     dCvtHexToChar     pi         32767
     d inHexVal                   65534A   options(*varsize)
     d inSize                        10I 0 value options(*nopass)

     d charVal         s                   like(CvtHexToChar)
     d hexVal          s                   like(inHexVal)
     d outSize         s                   like(inSize)

      /Free
       Callp CEEDOD(1:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       If %parms >= 2 and inSize <> *zero;
          inLen = inSize;
       Endif;
       outSize = inLen * .5;
       hexVal = %subst(inHexVal:1:inLen);

       HexToChar(charVal:hexVal:inLen);

       Return %subst(charVal:1:outSize);
      /End-free

     pCvtHexToChar     e
     *====================================================================
     * GetClaimName - Return Claim Name
     *   co#       - accepts the company #
     *   group     - accepts the group
     *   yr        - accepts the policy year
     *   Case      - accepts the Case#
     *====================================================================
     pGetClaimName     b
    
     dGetClaimName     PI            50
     d co#                            3  0 const
     d group                          3  0 const
     d yr                             3  0 const
     d claim#                         7  0 const

     d claimName       s             50    inz

     c     cmky1         klist
     c                   kfld                    co#
     c                   kfld                    group
     c                   kfld                    yr
     c                   kfld                    claim#

      /Free
       claimName = *Blanks;
       Open WMCML1;
       Chain cmky1 pcm01;
       Close WMCML1;
       If %found;
          claimName = %Trim(CMlsn) + ', ' + %Trim(CMFsn);
       EndIf;
       Return claimName;
      /End-Free

     pGetClaimName     e
      *====================================================================
      * GetEmpFullName - Get employer name
      *
      *  GetEmpFullName - returns employer name
      *   emp#      - passed in for validation  (required)
      *   div       - passed in for validation  (required)
      *   emrrn     - returns the RRN of WMEMP
      *   group     - passed in for validation
      *   co#       - accepts the company # to search over
      *   nametype  - passed in for name: DBA or federal (default to DBA)
      *====================================================================
     pGetEmpFullName   b

     dGetEmpFullName   PI           100
     d emp#                                like(EMemp#)                  const
     d div                                 like(EMdiv)                   const
     d emrrn                          9  0              options(*nopass)
     d group                               like(EMfnd)  options(*nopass) const
     d co#                                 like(EMco#)  options(*nopass) const
     d nameType                            like(EMpsrv) options(*nopass) const

     d group#          s                   like(EMfnd)
     d company         s                   like(EMco#)
     d nameFull        s            100
     d nameDbaFed      s              3

     c     emky          klist
     c                   kfld                    company
     c                   kfld                    group#
     c                   kfld                    emp#
     c                   kfld                    div

      /Free
       If        %parms >= 4;
          group# = group;
       Endif;
       If        %parms >= 5;
          company = co#;
       Endif;
       Clear     nameFull;
       Open WMEML;
       Chain  emky   pem00;
       Close WMEML;
       If        %parms >= 6;
          nameDbaFed = nametype;
          select;
          when      nameDbafed  = 'DBA';
             nameFull = EMname + EMnam2;
          when      nameDbaFed  = 'FED';
             nameFull = EMfdnm + Emfdn2;
          endsl;
       Endif;
       Return nameFull;
      /End-Free
     pGetEmpFullName   e
      *====================================================================
      * Remove Exception Characters - JSON
      *  Parameter 1 - character field (any length)
      *  Parameter 2 - character field (optional)
      *  Returns - character field (length of parameter 1)
      *====================================================================
     pRmvExceptJSON    b                   export
     dRmvExceptJSON    pi         32766
     d ##inField                  32766    options(*varsize) const
     d ##incSize                      5  0 options(*nopass)

     d xxIncSize       s                   like(##incSize) inz
     d f1              s              3  0
     d pos             s              5  0
     d replaceSize     s              5  0
     d returnFld       s                   like(##inField)
     d excChar         s              1

      /Free
       Callp CEEDOD(1:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       returnFld = %subst(##inField:1:inLen);
       For f1 = 1 to #ofExceptJSON;
          excChar = CvtHexToChar(excCharJSON(f1));
          replaceSize = 0;
          pos = 1;
          Dou pos = 0;
             pos = %scan(excChar:returnFld:pos+replaceSize);
             If pos <> 0;
                returnFld = %replace(%trim(replaceJSON(f1)):returnFld:pos:1);
                replaceSize = %len(%trim(replaceJSON(f1)));
                xxIncSize += %len(%trim(replaceJSON(f1))) - 1;
             Endif;
          Enddo;
       Endfor;
       If %parms >= 2;
          ##IncSize = xxIncSize;
       Endif;
       Return returnFld;
      /End-free

     pRmvExceptJSON    e

** JSON Exception Characters (in Hex) & Replacements         10/07/13
E0\\                      <<<==== MUST BE FIRST
7F\"
0D\'
16\b     Backspace (ascii code 08)
0C\f     Form feed (ascii code 0C)
25\n     New line
0D\r     Carriage return
05\t     Tab
0B\v     Vertical tab
3F       Substitute
