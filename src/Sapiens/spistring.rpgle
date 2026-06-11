     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H bnddir('QC2LE')
     H nomain
    H*********************************************************************
    H*  Revisions:                                                       *
    H*                                                                   *
    H*  02/28/25  C:079 Support: 102/118321                              *
    H*                  Add procedure to return letter for day of month  *
    H*                                                                   *
    H*  05/03/23  C:291 Project: WC2838                                  *
    H*                  Add Apostrophe to SpeChar in ClnFileName         *
    H*                                                                   *
    H*  10/05/22  C:340 Project: WC2838                                  *
    H*                  Add procedures ChkFileName and ClnFileName       *
    H*                                                                   *
    H*  01/24/22  C:33  Support: 100639                                  *
    H*                 -Escape line feed x'OA'                           *
    H*                 -Reduce automatic storage                         *
    H*                                                                   *
    H*  12/09/21  C:33  Support: 99529                                   *
    H*                  Increase throughput                              *
    H*                                                                   *
    H*  11/18/20 C:33   Project: SY409  DE843                            *
    H*                  RmvExceptJSON - Remove invalid characters        *
    H*                                                                   *
    H*  02/12/20 C:300 Support: 78170 (LUBA)                             *
    H*                 Resolve WC000311A form under same folder, if      *
    H*                 submitting from Audit Request process             *
    H*                 Create New procedure 'ScanReverse' in SPISTRING   *
     H*                                                                   *
     H*  11/26/19  C:79  Support: 150-85231                               *
     H*                  Determine input string length in ExtrAlphaNum    *
    H*                                                                   *
    H*  10/01/19 C:136  Project: SY406                                   *
    H*                  Add CvtRPGtoHTML                                 *
    H*                                                                   *
    H*  08/28/19  C:33  Project: SY408                                   *
    H*                  Add FormatName                                   *
    H*                                                                   *
    H*  04/03/19  C:33  Project: WC2610                                  *
    H*                  Use opdesc in TitleCase                          *
    H*                                                                   *
    H*  01/30/19  C:33  Project: SY390                                   *
    H*                  When excepting json                              *
    H*                  -Update HEX values                               *
    H*                                                                   *
    H*  08/27/18  C:33  Project: SY390                                   *
    H*                  Ensure ScanReplace works with extended replace   *
    H*                                                                   *
    H*  09/28/17  C:33  Support: 49209                                   *
    H*                  Use actual length for comparison                 *
    H*                                                                   *
    H*  07/18/17  C:33  Project: WC2405                                  *
    H*                  Allow for blanks to be scan value                *
    H*                                                                   *
    H*  06/08/17  C:33  Project: WC2405                                  *
    H*                  Allow for blanks to be replacement value         *
    H*                                                                   *
    H*  03/28/17  C:180 Project: SY371                                   *
    H*                  Add ExtrAlphaNum                                 *
    H*                                                                   *
    H*  01/31/17  C:33  Project: WC2405                                  *
    H*                  Add ScanReplace                                  *
    H*                                                                   *
    H*  07/06/16  C:33  Project: WC2405                                  *
    H*                  Add ParseString                                  *
    H*                                                                   *
    H*  06/16/15  C:309 PHW34136 025                                     *
    H*                  Changes in procedure ValidChar, added a new      *
    H*                  parameter for validating only numeric values     *
    H*                  in "inString" variable                           *
    H*                                                                   *
    H*  01/23/15  C:280 PHW32491 025                                     *
    H*                  Added new prodcedure ValidChar.                  *
    H*                                                                   *
    H*  12/12/13  C:267 Red 1509                                         *
    H*                  Added new prodcedure TitleCase.                  *
    H*                                                                   *
    H*  11/01/13  C:33  Seq# 84065                                       *
    H*                  Include 'Substitute' character replacement in    *
    H*                   RmvExceptJSON                                   *
    H*                                                                   *
    H*  10/07/13  C:33  Seq# 83731                                       *
    H*                  Add procedures                                   *
    H*                  - RmvExceptJSON                                  *
    H*                  - RmvExceptXML                                   *
    H*                  - CvtCharToHex                                   *
    H*                  - CvtHexToChar                                   *
    H*                                                                   *
    H*  06/11/13  C:282 REDMINE: 2785                                    *
    H*                  Add procedure                                    *
    H*                  - rmvApost                                       *
    H*                                                                   *
    H*  04/24/12  C:191 Project: Transform                               *
    H*                  Add procedure                                    *
    H*                  - FormatAddress                                  *
    H*                                                                   *
    H*  04/12/12  C:33  Project: WC2116                                  *
    H*                  Add procedure                                    *
    H*                  - StringFound                                    *
    H*                                                                   *
    H*  03/01/12  C:191 Project: Transform                               *
    H*                  Add procedure for concat and separate code/desc  *
    H*                                                                   *
    H*  12/21/04  C:33  Project: WC1033                                  *
    H*                  Update LeftJstify                                *
    H*                                                                   *
    H*  11/11/03  C:33  Project: WA071/39899                             *
    H*                  Add RightJstfy                                   *
    H*                                                                   *
    H*  10/10/03  C:33  Project: WC1017                                  *
    H*                  Add size to CenterFld                            *
    H*********************************************************************
     D*
     D/copy sprstring
     D
     DCEEDOD           PR
     D parmNum                       10I 0 const
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               12A   options(*omit)
     D*
     D* parameters passed to CEEDOD
     D*
     D descType        s             10I 0
     D dataType        s             10I 0
     D descInfo1       s             10I 0
     D descInfo2       s             10I 0
     D inLen           s             10I 0
     D hexLen          s             10I 0

     D***  Begin Add  ***  12/09/21 ****************
     DQp0zLprintf      pr            10I 0 extProc('Qp0zLprintf')
     D szOutputStg                     *   value options(*string)
     D                                 *   value options(*string:*nopass)
     D                                 *   value options(*string:*nopass)
     D                                 *   value options(*string:*nopass)
     D                                 *   value options(*string:*nopass)
     D***  End   Add  ***  12/09/21 ****************

     DQCLSCAN          pr                  extPgm('QCLSCAN')
     D outString                    999    const
     D outStringLen                   3  0 const
     D outStartPos                    3  0 const
     D outPattern                   999    const
     D outPatternLen                  3  0 const
     D outIgnoreCase                  1    const
     D outTrimBlanks                  1    const
     D outWildCard                    1    const
     D outResult                      3  0
     D#ofExceptXML     c                   5
     D excCharXML      s              1    dim(#ofExceptXML) ctdata perrcd(1)
     D replaceXML      s             10    dim(#ofExceptXML) alt(excCharXML)
     D***  Begin Chg  ***  01/24/22  ******************
     D**********#ofExceptJSON    c                   10
     D#ofExceptJSON    c                   11
     D***  End   Chg  ***  01/24/22  ******************
     D excCharJSON     s              2    dim(#ofExceptJSON) ctdata perrcd(1)
     D replaceJSON     s              5    dim(#ofExceptJSON) alt(excCharJSON)
     D#ofExceptHTML    c                   2
     D excCharHTML     s              2    dim(#ofExceptHTML) ctdata perrcd(1)
     D replaceHTML     s              5    dim(#ofExceptHTML) alt(excCharHTML)

     D lo              c                   'abcdefghijklmnopqrstuvwxyz'
     D up              c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
       //***  Begin Add  ***  11/18/20  ********************
     DfromInvalidChar  C                   X'0001020304060708090A0D0E0F-
     D                                     1011121314151718191A1B1C1D1E1F-
     D                                     2021222324262728292A2B2C2D2E2F-
     D                                     303132333435363738393A3B3C3D3E3F-
     D                                     8A8B8C8D8E8F-
     D                                     9A9B9C9D9E9F-
     D                                     AAABACADAEAF-
     D                                     B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF-
     D                                     CACBCCCDCECF-
     D                                     DADBDCDDDEDF-
     D                                     EAEBECEDEEEF-
     D                                     FBFCFDFEFF'
     DtoValidChar      c                   '             -
     D                                                    -
     D                                                    -
     D                                                     -
     D                                           -
     D                                           -
     D                                           -
     D                                                     -
     D                                           -
     D                                           -
     D                                           -
     D                                          '
       //***  End   Add  ***  11/18/20  ********************

      /eject
     P*=====================================================================
     P* centerFld - centers a character field within the length of the
     P*             field.
     P*  Parameter 1 - character field (any length)
     P*  Parameter 2 - size to center in (optional)
     P*  Returns - character field (length of parameter 1)
     P*=====================================================================
     PcenterFld        B                   Export
     DcenterFld        PI         32766    opdesc
     D fieldToCtr                 32766    options(*varsize) const
     D size                           5  0 options(*nopass)  const
     D
     D x               s              5  0
     D y               s              5  0
     D z               s              5  0
     D returnFld       s                   like(fieldToCtr)
     C                   callp     CEEDOD(1:descType:dataType:descInfo1:
     C                             descInfo2:inLen:*omit)
     C                   eval      x = inLen
     C                   if        %parms >=2 and size <> 0
     C                   eval      x = size
     C                   endif
 001 C     ' '           checkr    fieldToCtr:x  y
B002 C                   eval      z = ((x - y) / 2) + 1
 002 C                   eval      %subst(returnFld:z:y) =
     C                             %subst(fieldToCtr:1:y)
     C
     C                   return    returnFld
     PcenterFld        E



     P*=====================================================================
     P* CvtCharToHex - Converts from a character value to the hexadecimal   ng
     P*                 text to the character values
     P* Parameter 1 - character field (any length, .5x's size of parm 2)
     P* Parameter 2 - size to be converted (size of parm 1)
     P* Returns - character field
     P*=====================================================================
     PCvtCharToHex     b                   export
     DCvtCharToHex     pi         65534    opdesc
     D inCharVal                  32767A   options(*varsize)
     D inSize                        10I 0 value options(*nopass)

     D hexVal          s                   like(CvtCharToHex)
     D charVal         s                   like(inCharVal)
     D outSize         s                   like(inSize)

      /Free
       Callp CEEDOD(1:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       If %parms >= 2 and inSize <> *zero;
          inLen = inSize;
       Endif;
       outSize = inLen * 2;
       charVal = %subst(inCharVal:1:inLen);

       //***  Begin Chg  ***  11/18/20  ***************
       //CharToHex(hexVal:charVal:inLen);
       CharToHex(hexVal:charVal:outSize);
       //***  End   Chg  ***  11/18/20  ***************

       Return %subst(hexVal:1:outSize);
      /End-free

     PCvtCharToHex     e



     P*=====================================================================
     P* CvtHexToChar - Converts from hexadecimal text to the character form ng
     P*                 of the hex values
     P* Parameter 1 - character field (any length, 2x's size of return)
     P* Parameter 2 - size to be converted (size of parm 1)
     P* Returns - character field
     P*=====================================================================
     PCvtHexToChar     b                   export
     DCvtHexToChar     pi         32767    opdesc
     D inHexVal                   65534A   options(*varsize)
     D inSize                        10I 0 value options(*nopass)

     D charVal         s                   like(CvtHexToChar)
     D hexVal          s                   like(inHexVal)
     D outSize         s                   like(inSize)

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

     PCvtHexToChar     e



      /eject
     P*=====================================================================
     P* leftJstify - left justify a character field
     P*  Parameter 1 - character field (any length)
     P*  Returns - character field (length of parameter 1)
     P*=====================================================================
     PleftJstify       B                   Export
     DleftJstify       PI         32766    opdesc
     D fieldToLft                 32766    options(*varsize) const
     D
     D x               s              5  0
     D y               s              5  0
     D z               s              5  0
     D ##data          s                   like(fieldToLft)
     D returnFld       s          32766                     inz

     C                   callp     CEEDOD(1:descType:dataType:descInfo1:
     C                             descInfo2:inLen:*omit)
    C*
     C                   eval      x      = inLen
     C                   eval      ##data = fieldToLft
    C*
     C     ' '           check     ##data        y
     C     ' '           checkr    ##data:x      z
    C*
     C                   if        y <= x
     C                   eval      ##data = %subst(##data:y)
     C                   eval      %subst(##data:z+1:x) = *blanks
     C                   endif
     C*
     C                   eval      returnFld = ##data
     C*
     C                   return    returnFld

     PleftJstify       E

      /eject
     P*=====================================================================
     P* Right Justify a character field
     P*  Parameter 1 - character field (any length)
     P*  Parameter 2 - size of the field (optional)
     P*  Returns - character field (length of parameter 1)
     P*=====================================================================
     PRightJstfy       B                   Export
     DRightJstfy       PI         32766    opdesc
     D fieldToRght                32766    options(*varsize) const
     D inSize                         5  0 options(*nopass)  const

     D size            s              5  0
     D y               s              5  0
     D x               s              5  0
     D hldFld          s                   like(fieldToRght)
     D returnFld       s                   like(fieldToRght)

     C                   callp     CEEDOD(1:descType:dataType:descInfo1:
     C                             descInfo2:inLen:*omit)

     C                   if        %parms >= 2 and inSize <> 0
     C                   eval      size = inSize
     C                   else
     C                   eval      size = inLen
     C                   endif

     C                   eval      hldFld = %subst(fieldToRght:1:size)

     C     ' '           checkr    hldFld        y

     C                   eval      %subst(returnFld:size-y+1) =
     C                             %subst(hldFld:1:y)

     C                   return    returnFld

     PRightJstfy       E

      /eject
     P*=====================================================================
     P* chkApost - checks for an aspostrope (') and returns a string with
     P*             two in place of one
     P*  Parameter 1 - character field (any length)
     P*  Returns - character field (length of parameter 1)
     P*=====================================================================
     PchkApost         B                   Export
     DchkApost         PI         32766    opdesc
     D fieldToChk                 32766    options(*varsize) const
     D
     D pos             s              5  0
     D start           s              5  0 inz(1)
     D returnFld       s                   like(fieldToChk)
     C                   callp     CEEDOD(1:descType:dataType:descInfo1:
     C                             descInfo2:inLen:*omit)
     C                   eval      returnFld = fieldToChk
     C                   eval      pos = %scan('''':returnFld:start)
     C                   dow       pos <> 0
     C                   if        %subst(returnFld:pos+1:1) <> ''''
     C                   eval      returnFld = %subst(returnFld:1:pos)
     C                             + '''' + %subst(returnFld:pos+1)
     C                   eval      pos = pos + 1
     C                   endif
     C                   eval      pos = %scan('''':returnFld:pos+1)
     C                   enddo
     C
     C                   return    returnFld
     PchkApost         E

      /eject
     P*=====================================================================
     P* lower - translates a character field to lower case characters
     P*  Parameter 1 - character field (any length)
     P*  Returns - character field (length of parameter 1)
     P*=====================================================================
     Plower            B                   Export
     Dlower            PI         32766    opdesc
     D fieldIn                    32766    options(*varsize) const
     D fieldOut        s                   like(fieldIn)
     C
      /Free
       CEEDOD(1:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       fieldOut = %Xlate(up:lo:%Subst(fieldIn:1:inLen));
       Return fieldOut;
      /End-free
     C     up:lo         xlate     fieldIn       fieldOut
     C
     C                   return    fieldOut
     Plower            E

     PStringFound      b                   export
     P*====================================================================
     P* Determine if pattern is found in string
     P*====================================================================
     DStringFound      pi             1n   opdesc
     D inString                     999    const options(*varsize)
     D inPattern                    999    const options(*varsize)
     D inIgnoreCase                   1n   const options(*nopass)
     D inWildCard                     1    const options(*nopass)

     D ignoreCase      s              1n
     D wildCard        s              1
     D string          s            999
     D stringLen       s              3  0
     D pattern         s            999
     D patternLen      s              3  0
     D result          s              3  0

      /free
       If %parms >= 3;
          ignoreCase = inIgnoreCase;
       Endif;
       If %parms >= 4;
          wildCard = inWildCard;
       Endif;

       Callp CEEDOD(1:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       stringLen = inLen;
       Callp CEEDOD(2:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       patternLen = inLen;

       pattern = %trim(inPattern);
       string  = %trim(inString);
       If ignoreCase;
          pattern = Upper(pattern);
          string  = Upper(string);
       Endif;

       CallP QCLSCAN(inString:stringLen:1:pattern:patternLen:
        ignoreCase:'1':wildCard:result);
       //result = %scan(%trim(pattern):%trim(string));

       Return result > *zero;

      /End-free
     PStringFound      e

      /eject
     P*=====================================================================
     P* upper - translates a character field to upper case characters
     P*  Parameter 1 - character field (any length)
     P*  Returns - character field (length of parameter 1)
     P*=====================================================================
     Pupper            B                   Export
     Dupper            PI         32766    opdesc
     D fieldIn                    32766    options(*varsize) const
     D fieldOut        s                   like(fieldIn)

      /Free
       CEEDOD(1:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       fieldOut = %Xlate(lo:up:%Subst(fieldIn:1:inLen));
       Return fieldOut;
      /End-free
     C
     C     lo:up         xlate     fieldIn       fieldOut
     C
     C                   return    fieldOut
     Pupper            E

     P*=====================================================================
     P* ExtractCode - Extract Code from string contating code and its description
     P*  Parameter 1 - String contating code and description
     P*  Parameter 2 - Optional input parameter for Separator
     P*  Parameter 3 - Optional output parameter for Description
     P*  Returns - Code value
     P*=====================================================================
     PExtractCode      B                   Export
     DExtractCode      PI            50    opdesc
     D inString                     256    options(*varsize) const
     D inSeparator                    1    options(*nopass) const
     D outDesc                      256    options(*nopass)
     D
     D intLen          s              5  0
     D intSepPos       s              5  0
     D strString       s                   like(inString)
     D strCode         s             50    inz
     D strSeparator    s              1    inz
      /Free
         strString = inString;
         If %parms > 1;
            strSeparator = inSeparator;
         Else;
            strSeparator = ' ';
         Endif;

         CEEDOD(1:descType:dataType:descInfo1:
          descInfo2:inLen:*omit);

         intLen = inLen;
    
         intSepPos = %Scan(strSeparator:strString);
         If intSepPos >0 And intSepPos < intLen;
            strCode = %Subst(strString:1:intSepPos-1);
            If %parms > 2;
               outDesc = %Subst(strString:intSepPos+1:intLen-intSepPos);
            Endif;
         Else;
            strCode = ' ';
            If %parms > 2;
               outDesc = ' ';
            Endif;
         Endif;
         Return strCode;
      /End-Free
     PExtractCode      E
     P*=====================================================================
     P* FormatAddress - This will format address in consecutive non-blanks lines
     P*  Parameter 1 - Address Line 1
     P*  Parameter 2 - Address Line 2
     P*  Parameter 3 - City
     P*  Parameter 4 - State
     P*  Parameter 5 - Zip code
     P*  Parameter 6 - Output line 1
     P*  Parameter 7 - Output line 2
     P*  Parameter 8 - Output line 3
     P*=====================================================================
     PFormatAddress    B                   Export
     DFormatAddress    PI
     D inAdd1                        40
     D inAdd2                        40
     D inCity                        25
     D inState                        2
     D inZip                          9  0
     D outAddLine1                   40
     D outAddLine2                   40
     D outAddLine3                   40
     D*
     D arrAdd          S             40    dim(3) inz
     D intLine         S              2  0 dim(3) inz
      /Free

       If inAdd1 <> *blanks;
          intLine = intLine + 1;
          arrAdd(intLine) = %Trim(inAdd1);
       Endif;
       If inAdd2 <> *blanks;
          intLine = intLine + 1;
          arrAdd(intLine) = %Trim(inAdd2);
       Endif;
       If inCity <> *blanks Or inState <> *blanks Or inZip <> *zeros;
          intLine = intLine + 1;
          If %Subst(%EditC(inZip:'X'):6:4)='0000';
             arrAdd(intLine)=%Trim(inCity) + ', ' + inState + ' ' +
              %Subst(%EditC(inZip:'X'):1:5);
          Else;
             arrAdd(intLine)=%Trim(inCity) + ', ' + inState + ' ' +
              %Subst(%EditC(inZip:'X'):1:5) + '-' +
              %Subst(%EditC(inZip:'X'):6:4);
          Endif;
       Endif;

       outAddLine1 = arrAdd(1);
       outAddLine2 = arrAdd(2);
       outAddLine3 = arrAdd(3);

      /End-Free
     PFormatAddress    E

     P*=====================================================================
     P* FormatName - This will format and return name, as needed per format
     P*  Parameter 1 - Last name (or only name)
     P*  Parameter 2 - First name
     P*  Parameter 3 - Middle name {optional}
     P*  Parameter 4 - Format {optional}
     P*   *LFM - Last, First Middle {default}
     P*   *FML - First Middle Last
     P*=====================================================================
     PFormatName       b                   export
     DFormatName       pi          1525    varying
     D inLast                       500    varying const
     D inFirst                      500    varying const
     D inMiddle                     500    varying const options(*nopass)
     D inFormat                      10    varying const options(*nopass)
     D*
     DoutName          s           1525    varying inz
     DstrFirst         s            500    inz varying
     DstrFormat        s             10    inz('*LFM')
     DstrLast          s            500    inz varying
     DstrMiddle        s            500    inz varying
      /Free
       strLast = %trim(inLast);
       strFirst = %trim(inFirst);
       If %parms >= 3 and inMiddle <> *blanks;
          strMiddle = %trim(inMiddle);
       Endif;
       If %parms >= 4 and inFormat <> *blanks;
          strFormat = inFormat;
       Endif;
       Select;
       When strFormat = '*LFM';
          outName += strLast;
          If strFirst <> *blanks or strMiddle <> *blanks;
             outName += ',';
          Endif;
          If strFirst <> *blanks;
             outName += ' ' + strFirst;
          Endif;
          If strMiddle <> *blanks;
             outName += ' ' + strMiddle;
          Endif;
       When strFormat = '*FML';
          If strFirst <> *blanks;
             outName += strFirst + ' ';
          Endif;
          If strMiddle <> *blanks;
             outName += strMiddle + ' ';
          Endif;
          outName += strLast;
       Endsl;
       Return outName;
      /End-Free
     PFormatName       e

     P*====================================================================
     P* ParseString - returns the Nth field in a string
     P*  parm 1 - string (returns the new string if the 1st field is used)
     P*  parm 2 - delimiter (this can be a field size as well)
     P*  parm 3 - (optional) field number/nth position, defaults to 1
     P*  parm 4 - (optional) return string without selected field,
     P*                       defaults to removing (only for 1st field)
     P*====================================================================
     PParseString      b                   export
     DParseString      pi         32766    opdesc
     D ioString                   32766    options(*varsize)
     D iDelimiter                     5                      const
     D iField                         5  0 options(*nopass)  const
     D iRemove                        1n   options(*nopass)  const

     DintField         s              5  0 inz(1)
     DintFieldSize     s              5  0
     DintStart         s              5  0
     DintLen           s              5  0
     DintNext          s              5  0
     Di                s              5  0
     DisDelimited      s              1n   inz('1')
     DisRemove         s              1n   inz('0')
     DstrDelimiter     s              5    inz
     DstrString        s                   like(ioString) inz
     DstrReturn        s                   like(ioString) inz
      /Free
       // Get the size of the incoming string
       Callp CEEDOD(1:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       strString = %subst(ioString:1:inLen);
       If strString = *blanks;
          Return *blanks;
       Endif;

       If %parms >= 2;
          strDelimiter = %trim(iDelimiter);
          Monitor;
             intFieldSize = %dec(%trim(iDelimiter):5:0);
             isDelimited = *off;
          On-Error;
          Endmon;
       Endif;

       If %parms >= 3 and iField > 0;
          intField = iField;
       Endif;

       If %parms >= 4;
          isRemove = iRemove;
       Endif;

       If isRemove and intField <> 1;
          isRemove = *off;
       Endif;

       Select;
       When isDelimited;
          Exsr ParseDelimited;
       Other;
          Exsr ParseFixed;
       Endsl;

       If intStart > 0 and intLen > 0;
          strReturn = %subst(strString:intStart:intLen);
          If isRemove;
             intStart = intStart + intLen;
             intLen = inLen - intStart + 1;
             If isDelimited;
                intStart = intStart + %len(%trim(strDelimiter));
             Endif;
             %subst(ioString:1:inLen) = %subst(strString:intStart:intLen);
          Endif;
       Endif;

       Return strReturn;

       //==================================================================
       Begsr ParseDelimited;
        intNext = 0;
        For i = 1 to intField;
           intStart = intNext + 1;
           intNext = %scan(%trim(strDelimiter):strString:intStart);
           If intNext = 0 and %subst(strString:%len(%trim(strString)):1) <> ' ';
              intNext = %len(%trim(strString)) + 1;
           Endif;
           intLen = intNext - intStart;
        Endfor;
       Endsr;

       //==================================================================
       Begsr ParseFixed;
        intStart = (intFieldSize * intField) - (intFieldSize - 1);
        intLen = intFieldSize;
       Endsr;
      /End-free
     PParseString      e

     P*=====================================================================
     P* rmvApost - returns a string with the removed (') from the string
     P*  Parameter 1 - character field (any length)
     P*  Returns - character field (length of parameter 1)
     P*=====================================================================
     PrmvApost         B                   Export
     DrmvApost         PI         32766    opdesc
     D fieldToChk                 32766    options(*varsize) const
     D
     D charApost       c                   ''''
     D Pos             s              5  0
     D Start           s              5  0 inz(1)
     D returnFld       s                   like(fieldToChk)
      /Free

       returnFld = fieldToChk;
       pos = %scan(charApost:returnFld:start);
       Dow pos <> 0;
          returnFld = %subst(returnFld:1:pos-1) + %subst(returnFld:pos+1);
          pos = 0;
       pos = %scan(charApost:returnFld:start);
       EndDo;

       Return returnFld;
      /End-Free
     PrmvApost         E





     P*====================================================================
     P* Remove Exception Characters - JSON
     P*  Parameter 1 - character field (any length)
     P*  Parameter 2 - character field (optional)
     P*  Returns - character field (length of parameter 1)
     P*====================================================================
     PRmvExceptJSON    b                   export
     D***  Begin Chg  ***  12/09/21 ****************
     D*****RmvExceptJSON    pi         32766    opdesc
     DRmvExceptJSON    pi         32766    opdesc varying
     D***  End   Chg  ***  12/09/21 ****************
     D ##inField                  32766    options(*varsize) const
     D ##incSize                      5  0 options(*nopass)

     D xxIncSize       s                   like(##incSize) inz
     D f1              s              3  0
     D pos             s              5  0
     D replaceSize     s              5  0
     D***  Begin Chg  ***  12/09/21 ****************
     D*returnFld       s                   like(##inField)
     D returnFld       s          32766    varying
     D***  Begin Del  ***  01/24/22 ****************
     D*inField**       s          32766    varying
     D***  End   Del  ***  01/24/22 ****************
     D***  End   Chg  ***  12/09/21 ****************
     D excChar         s              1

      /Free
       Callp CEEDOD(1:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       //***  Begin Chg  ***  12/09/21 ****************
       //returnFld = %subst(##inField:1:inLen);
       //***  Begin Add  ***  11/18/20  ********************
       //returnFld = %xlate(fromInvalidChar:toValidChar:returnFld);
       //***  End   Add  ***  11/18/20  ********************
       //***  Begin Chg  ***  01/24/22 ****************
       //inField = %subst(##inField:1:inLen);
       //inField = %xlate(fromInvalidChar:toValidChar:inField);
       returnFld = %subst(##inField:1:inLen);
       returnFld = %xlate(fromInvalidChar:toValidChar:returnFld);
       //***  End   Chg  ***  01/24/22 ****************
       //***  End   Chg  ***  12/09/21 ****************
       For f1 = 1 to #ofExceptJSON;
        //***  Begin Chg  ***  12/09/21 ****************
        //excChar = CvtHexToChar(excCharJSON(f1));
        //replaceSize = 0;
        //pos = 1;
        //Dou pos = 0;
        //   pos = %scan(excChar:returnFld:pos+replaceSize);
        //   If pos <> 0;
        //      returnFld = %replace(%trim(replaceJSON(f1)):returnFld:pos:1);
        //      replaceSize = %len(%trim(replaceJSON(f1)));
        //      xxIncSize += %len(%trim(replaceJSON(f1))) - 1;
        //   Endif;
        //Enddo;
          excChar = CvtHexToChar(excCharJSON(f1));
        //***  Begin Chg  ***  01/24/22 ****************
        //inField = %scanrpl(excChar:%trim(replaceJSON(f1)):inField);
          returnFld = %scanrpl(excChar:%trim(replaceJSON(f1)):returnFld);
        //***  End   Chg  ***  01/24/22 ****************
        //***  End   Chg  ***  12/09/21 ****************
       Endfor;
       //***  Begin Add  ***  12/09/21 ****************
       //***  Begin Del  ***  01/24/22 ****************
       //returnFld = inField;
       //***  End   Del  ***  01/24/22 ****************
       //***  End   Add  ***  12/09/21 ****************
       If %parms >= 2;
          ##IncSize = xxIncSize;
       Endif;
       Return returnFld;
      /End-free

     PRmvExceptJSON    e




     P*====================================================================
     P* Remove Exception Characters
     P*  Parameter 1 - character field (any length)
     P*  Parameter 2 - character field (optional)
     P*  Returns - character field (length of parameter 1)
     P*====================================================================
     PRmvExceptXML     b                   export
     DRmvExceptXML     pi         32766    opdesc
     D ##inField                  32766    options(*varsize) const
     D ##incSize                      5  0 options(*nopass)

     D xxIncSize       s                   like(##incSize) inz
     D f1              s              3  0
     D pos             s              5  0
     D returnFld       s                   like(##inField)

      /Free
       Callp CEEDOD(1:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       returnFld = %subst(##inField:1:inLen);
       For f1 = 1 to #ofExceptXML;
          pos = 0;
          Dou pos = 0;
             pos = %scan(excCharXML(f1):returnFld:pos+1);
             If pos <> 0;
                returnFld = %replace(%trim(replaceXML(f1)):returnFld:pos:1);
                xxIncSize += %len(%trim(replaceXML(f1))) - 1;
             Endif;
          Enddo;
       Endfor;
       If %parms >= 2;
          ##IncSize = xxIncSize;
       Endif;
       Return returnFld;
      /End-free

     PRmvExceptXML     e

     P*====================================================================
     P* Scan string for source and replace (%scanrpl is available in OS7.1)
     P*  Parameter 1 - string to be scanned for (any length)
     P*  Parameter 2 - replacement string (any length)
     P*  Parameter 3 - string to be scanned (any length)
     P*  Parameter 4 - start position (optional)
     P*  Parameter 5 - length to scan (optional)
     P*  Returns - string with replacements
     P*====================================================================
     PScanReplace      b                   export
     DScanReplace      pi         32766    opdesc
     D inScan                     32766    options(*varsize) const
     D inReplace                  32766    options(*varsize) const
     D inString                   32766    options(*varsize) const
     D inStart                        5  0 options(*nopass)  const
     D inLength                       5  0 options(*nopass)  const

     DdecEnd           s              7  0 inz(*hival)
     DdecLength        s              5  0 inz(*hival)
     DdecPosNew        s              5  0
     DdecPosOld        s              5  0
     DdecReplaceLen    s              5  0
     DdecStringLen     s              5  0
     DdecStart         s              5  0 inz(1)
     DstrReplace       s          32766    inz
     DstrReturn        s                   like(inString)
     DstrScan          s          32766    inz varying
     DstrString        s          32766    inz
      /Free
       Callp CEEDOD(1:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       strScan = %subst(inScan:1:inLen);
       Callp CEEDOD(2:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       strReplace = %subst(inReplace:1:inLen);
       decReplaceLen = inLen;
       Callp CEEDOD(3:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       strString = %subst(inString:1:inLen);
       decStringLen = inLen;
       If %parms >= 4;
          decStart = inStart;
       Endif;
       If %parms >= 5;
          decLength = inLength;
       Endif;
       If decLength <> *hival;
          decEnd = decStart + decLength;
       Endif;
       decPosOld = decStart;
       decPosNew = decStart;
       strReturn = strString;
       If decPosOld = 1;
          decPosOld = 0;
       Endif;
       Dou decPosOld <= 0;
          decPosOld = %scan(strScan:strString:decPosOld+1);
          Select;
          When decPosOld <= 0;
             Leave;
          When decPosOld > decEnd;
             Leave;
          When decPosOld + decReplaceLen > decStringLen;
             Leave;
          Endsl;
          decPosNew = %scan(strScan:strReturn:decPosNew);
          strReturn = %replace(%subst(strReplace:1:decReplaceLen):strReturn
           :decPosNew:%len(strScan));
       decPosNew = decPosNew + decReplaceLen;
       Enddo;
       Return strReturn;
      /End-free
     PScanReplace      e

     P*====================================================================
     P* TitleCase - translates a character field to upper/lower case
     P*  Parameter 1 - character field (any length)
     P*  Returns - character field (length of parameter 1)
     P*====================================================================
     PTitleCase        B                   export
     DTitleCase        PI         32766    opdesc
     D  inText                    32766    options(*varsize) const

     D i               S             10I 0
     D j               S             10I 0
     D char            S              1A
     D prevChar        S              1A
     D size            s             10I 0
     D text            s                   like(inText)

     D lower           C                   Const('abcdefghijklmnopqrstuvwxyz')
     D UPPER           C                   Const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')

      /Free
       Callp CEEDOD(1:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       text = %subst(inText:1:inLen);
       size = inLen;

       For i = 1 to size;
          prevChar = Char;
          char = %SubSt(text : i : 1);
          If prevChar >= 'A' and PrevChar <= 'Z'
           and char >= 'A' and char <= 'Z';
              %SubSt(text : i : 1) = %XLate(UPPER : lower : char);
          EndIf;
       EndFor;

       Return text;

      /End-Free

     PTitleCase        E

     P*====================================================================
     P* ValidChar - Validates no special characters in field
     P*  instring - character field (any length)
     P*  error    - character field (length of parameter 1)
     P*====================================================================
     PValidChar        B                   export
     DValidChar        PI            30    opdesc
     D inString                      30    options(*varsize) const
     D mode                           1    options(*varsize) const
     D error                          1    options(*nopass)

      /Free

        error = *off;

        Select;
          When Mode = 'S';
            if %check(' 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ':inString) > 0;
               error = *on;
            endif;
          When Mode = 'N';
            if %check(' 0123456789':inString) > 0;
               error = *on;
            endif;
        Endsl;

       Return error;

      /End-Free

     PValidChar        E

     P*====================================================================
     P* ExtrAlphaNum - Extracts Alphanumeric characters and
     P*                clears non-alphanumeric.
     P*
     P*  instring - character field (any length)
     P*====================================================================
     PExtrAlphaNum     B                   export
     DExtrAlphaNum     PI         32766    opdesc
     D inString                   32766    options(*varsize) const

     D OutString       S                   like(inString)
     DAlphaNumeric     C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ-
     D                                      abcdefghijklmnopqrstuvwxyz-
     D                                      0123456789 '

     D Ix              S              5  0
     D Ij              S              5  0
     C                   callp     CEEDOD(1:descType:dataType:descInfo1:
     C                             descInfo2:inLen:*omit)
      /Free
       OutString = InString;
       Dou ix  = 0;
          If ix > *zero;
             ij = ix;
          Else;
             ij = 1;
          Endif;

          ix = %check(AlphaNumeric:OutString:ij);

          If ix > *zero;
             %subst(OutString:ix:1) = *blanks;
          Endif;
       Enddo;
          If inLen > 0;
             OutString = %Subst(OutString:1:inLen);
          Endif;

       Return OutString;
      /End-Free

     PExtrAlphaNum     E


     P*====================================================================
     P* Remove Exception Characters
     P*  Parameter 1 - character field (any length)
     P*  Parameter 2 - character field (optional)
     P*  Returns - character field (length of parameter 1)
     P*====================================================================
     PCvtRpgToHTML     b                   export
     DCvtRpgToHTML     pi         32766    opdesc
     D ##inField                  32766    options(*varsize) const
     D ##incSize                      5  0 options(*nopass)

     D xxIncSize       s                   like(##incSize) inz
     D f1              s              3  0
     D pos             s              5  0
     D replaceSize     s              5  0
     D returnFld       s                   like(##inField)
     D excChar         s              1

      /Free
       Callp CEEDOD(1:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       returnFld = %subst(##inField:1:inLen);
       For f1 = 1 to #ofExceptHTML;
          excChar = CvtHexToChar(excCharHTML(f1));
          replaceSize = 0;
          pos = 1;
          Dou pos = 0;
             pos = %scan(excChar:returnFld:pos+replaceSize);
             If pos <> 0;
                returnFld = %replace(%trim(replaceHTML(f1)):returnFld:pos:1);
                replaceSize = %len(%trim(replaceHTML(f1)));
                xxIncSize += %len(%trim(replaceHTML(f1))) - 1;
             Endif;
          Enddo;
       Endfor;
       If %parms >= 2;
          ##IncSize = xxIncSize;
       Endif;
       Return returnFld;
      /End-free

     PCvtRpgToHTML     e



     P*** beg add ***** 02/12/20 ****************************************
     P*====================================================================
     P* Scan Reverse - Sacn a string in reverse order (like BIF %scanr)
     P*  Parameter 1 - string to be scanned for (any length)
     P*  Parameter 2 - string to be scanned (any length)
     P*  Parameter 3 - start position (optional)
     P*  Parameter 4 - length to scan (optional)
     P*  Returns - position of string to be scanned
     P*====================================================================
     PScanReverse      b                   export
     DScanReverse      pi             5  0 opdesc
     D inScan                     32766    options(*varsize) const
     D inString                   32766    options(*varsize) const
     D inStart                        5  0 options(*nopass)  const
     D inLength                       5  0 options(*nopass)  const

     D x               s              5  0
     D y               s              5  0
     D returnPos       s              5  0
     DstrScan          s          32766    inz varying
     DstrString        s          32766    inz

      /Free

       // Get the size of the incoming string
       Callp CEEDOD(1:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       strScan = %subst(inScan:1:inLen);
       Callp CEEDOD(2:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       strString = %subst(inString:1:inLen);

       x = 0;
       y = 1;
       x = %Scan(strScan:strString:y);
       returnPos = 0;
       Dow x <> 0;
          y = x+1;
          returnPos = x;
          x = 0;
          x = %Scan(strScan:strString:y);
       Enddo;

       Return returnPos;
      /End-free
     PScanReverse      e
     P*** end add ***** 02/12/20 ****************************************
     P*** Beg Add *** 10/05/22 WC2838 *************************************
     P*=====================================================================
     PChkFileName      B                   Export
     DChkFileName      PI             1N
     D fileName                     200    Const
     D Error                        200    options(*NoPass)
     D
     D SplChrPos       s              3  0
     D TotSplChr       s              3  0
     D i               s             10i 0
     D Valid           s               N
     D SpeChar         C                   Const('<>?|/\*":')
     /Free
          Error = ' ';
          Valid = *On;
          TotSplChr = %Len(%Trim(SpeChar));
         //Check for Special Characters
          For i = 1 to TotSplChr;
            SplChrPos = 0;
            SplChrPos = %Scan(%Subst(SpeChar:i:1):fileName);
            If SplChrPos > 0;
               Error = 'Invalid characters are present'+
                       ' in the file name - <>?|/\*?:';
               Valid = *Off;
               Return Valid;
            EndIf;
          Endfor;
         //Check for File Extension
          SplChrPos = 0;
          SplChrPos = %Scan('.':%Trim(fileName));
          If SplChrPos <= 1;
               Error = 'No file extension is present '+
                       'on the file name';
               Valid = *Off;
               Return Valid;
          Else;
          If  %Scan(' ':fileName:SplChrPos) - SplChrPos <= 1;
               Error = 'No file extension is present '+
                       'on the file name';
               Valid = *Off;
               Return Valid;
          EndIf;
          EndIf;
          Return Valid;
     /End-Free
     PChkFileName      E
     P*=====================================================================
     P* ClnFileName  : Clean File Name (Replace Special Characters with Blank
     P*  Parameter 1 - Character Field 200 A
     P*=====================================================================
     PClnFileName      B                   Export
     DClnFileName      PI           200A
     D inFileName                   200A
     D
     D TotSplChr       S              3  0
     D i               S             10i 0
     D Valid           S               N
      *** Beg Chg ***** 05/03/23 **********************************
     D*** SpeChar         C                   Const('<>?|/\*":')
     D SpeChar         C                   Const('<>?|/\*":''')
      *** End Chg ***** 05/03/23 **********************************
     D fileName        S                   Like(inFileName)
     /Free
          Valid = *On;
          TotSplChr = %Len(%Trim(speChar));
          fileName  = inFileName;
         //Check for Special Characters
          For i = 1 to TotSplChr;
            fileName = %ScanRpl(%Subst(SpeChar:i:1):'':fileName);
          Endfor;
         //Check for File Extension
          Return fileName;
     /End-Free
     PClnFileName      E
     P*=====================================================================
     P*** End Add *** 10/05/22 WC2838 *************************************
     P*** Beg Add *** 02/28/25 ********************************************
     P*====================================================================
     P* Return Letter for Day of Month
     P*====================================================================
     PDayToLetter      B                   Export
     DDayToLetter      pi             1a
     D inDayofMonth                   2s 0
       Select;
       When inDayOfMonth = 1;
          Return 'A';
       When inDayOfMonth = 2;
          Return 'B';
       When inDayOfMonth = 3;
          Return 'C';
       When inDayOfMonth = 4;
          Return 'D';
       When inDayOfMonth = 5;
          Return 'E';
       When inDayOfMonth = 6;
          Return 'F';
       When inDayOfMonth = 7;
          Return 'G';
       When inDayOfMonth = 8;
          Return 'H';
       When inDayOfMonth = 9;
          Return 'I';
       When inDayOfMonth = 10;
          Return 'J';
       When inDayOfMonth = 11;
          Return 'K';
       When inDayOfMonth = 12;
          Return 'L';
       When inDayOfMonth = 13;
          Return 'M';
       When inDayOfMonth = 14;
          Return 'N';
       When inDayOfMonth = 15;
          Return 'O';
       When inDayOfMonth = 16;
          Return 'P';
       When inDayOfMonth = 17;
          Return 'Q';
       When inDayOfMonth = 18;
          Return 'R';
       When inDayOfMonth = 19;
          Return 'S';
       When inDayOfMonth = 20;
          Return 'T';
       When inDayOfMonth = 21;
          Return 'U';
       When inDayOfMonth = 22;
          Return 'Z';
       When inDayOfMonth = 23;
          Return 'W';
       When inDayOfMonth = 24;
          Return 'X';
       When inDayOfMonth = 25;
          Return 'Y';
       When inDayOfMonth = 26;
          Return 'Z';
       Other;
          Return ' ';
       Endsl;
     PDayToLetter      e
     P*=====================================================================
     P*** End Add *** 02/28/25 ********************************************
** XML Exception Characters & Replacements         10/07/13
&&amp;                    <<<==== MUST BE FIRST
<&lt;
>&gt;
'&apos;
"&quot;
** JSON Exception Characters (in Hex) & Replacements         10/07/13
E0\\     reverse solidus  <<<==== MUST BE FIRST
7F\"     quotation mark
61\/     solidus                                             01/30/19
16\b     Backspace (ascii code 08)
0C\f     Form feed (ascii code 0C)
0A\n     Line feed                                           01/24/22
25\n     New line
0D\r     Carriage return
05\t     Tab
0B\v     Vertical tab
3F       Substitute                                          11/01/13
** HTML Exception Characters (in Hex) & Replacements         10/01/19
25<BR/>  New line
7D''     Apostrophe
