     D*=====================================================================
     D* String handling procedure prototypes
     D*=====================================================================
     P*=====================================================================
     P* centerFld - centers a character field within the length of the
     P*             field.
     P*  Parameter 1 - character field (any length)
     P*  Parameter 2 - size to center in (optional)
     P*  Returns - character field (length of parameter 1)
     P*=====================================================================
     DcenterFld        PR         32766    opdesc
     D fieldToCtr                 32766    options(*varsize) const
     D size                           5  0 options(*nopass)  const
     D
     P*=====================================================================
     P* leftJstify - left justify a character field
     P*  Parameter 1 - character field (any length)
     P*  Returns - character field (length of parameter 1)
     P*=====================================================================
     DleftJstify       PR         32766    opdesc
     D fieldToLft                 32766    options(*varsize) const
     D
     P*=====================================================================
     P* Right Justify a character field
     P*  Parameter 1 - character field (any length)
     P*  Parameter 2 - size of the field (optional)
     P*  Returns - character field (length of parameter 1)
     P*=====================================================================
     DRightJstfy       PR         32766    opdesc
     D fieldToRght                32766    options(*varsize) const
     D inSize                         5  0 options(*nopass)  const

     P*=====================================================================
     P* chkApost - checks for an aspostrope (') and returns a string with
     P*             two in place of one
     P*  Parameter 1 - character field (any length)
     P*  Returns - character field (length of parameter 1)
     P*=====================================================================
     DchkApost         PR         32766    opdesc
     D fieldToChk                 32766    options(*varsize) const
     D
     P*=====================================================================
     P* lower - translates a character field to lower case characters
     P*  Parameter 1 - character field (any length)
     P*  Returns - character field (length of parameter 1)
     P*=====================================================================
     Dlower            PR         32766    opdesc
     D fieldIn                    32766    options(*varsize) const

     P*====================================================================
     P* ParseString - returns the Nth field in a string
     P*  parm 1 - string (returns the new string if the 1st field is used)
     P*  parm 2 - delimiter (this can be a field size as well)
     P*  parm 3 - (optional) field number/nth position, defaults to 1
     P*  parm 4 - (optional) return string without selected field,
     P*                       defaults to removing (only for 1st field)
     P*====================================================================
     DParseString      pr         32766    opdesc
     D ioString                   32766    options(*varsize)
     D iDelimiter                     5                      const
     D iField                         5  0 options(*nopass)  const
     D iRemove                        1n   options(*nopass)  const

     P*====================================================================
     P* Determine if pattern is found in string
     P*  Parameter 1 - String to compare against
     P*  Parameter 2 - String pattern to use for compare
     P*  Parameter 3 - Translate characters to uppercase
     P*  Parameter 4 - Wildcard used in 'pattern'
     P*====================================================================
     DStringFound      pr             1n   opdesc
     D inString                     999    const options(*varsize)
     D inPattern                    999    const options(*varsize)
     D inIgnoreCase                   1n   const options(*nopass)
     D inWildCard                     1    const options(*nopass)

     P*=====================================================================
     P* upper - translates a character field to upper case characters
     P*  Parameter 1 - character field (any length)
     P*  Returns - character field (length of parameter 1)
     P*=====================================================================
     Dupper            PR         32766    opdesc
     D fieldIn                    32766    options(*varsize) const

     P*=====================================================================
     P* ExtractCode - Extract Code from string contating code and its description
     P*  Parameter 1 - String contating code and description
     P*  Parameter 2 - Optional input parameter for Separator
     P*  Parameter 3 - Optional output parameter for Description
     P*  Returns - Code value
     P*=====================================================================
     DExtractCode      PR            50    opdesc
     D inString                     256    options(*varsize) const
     D inSeparator                    1    options(*nopass)  const
     D outDesc                      256    options(*nopass)
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
     DFormatAddress    PR
     D inAdd1                        40
     D inAdd2                        40
     D inCity                        25
     D inState                        2
     D inZip                          9  0
     D outAddLine1                   40
     D outAddLine2                   40
     D outAddLine3                   40
     P*=====================================================================
     P* FormatName - This will format and return name, as needed per format
     P*  Parameter 1 - Last name (or only name)
     P*  Parameter 2 - First name
     P*  Parameter 3 - Middle name {optional}
     P*  Parameter 4 - Format {optional}
     P*   *LFM - Last, First Middle {default}
     P*   *FML - First Middle Last
     P*=====================================================================
     DFormatName       pr          1525    varying
     D inLast                       500    varying const
     D inFirst                      500    varying const
     D inMiddle                     500    varying const options(*nopass)
     D inFormat                      10    varying const options(*nopass)
     P*=====================================================================
     P* rmvApost - returns a string with the removed (') from the input string
     P* Parameter 1 - character field (any length)
     P* Returns - character field (length of parameter 1)
     P*=====================================================================
     DrmvApost         PR         32766    opdesc
     D fieldToChk                 32766    options(*varsize) const
     D
     P*=====================================================================
     P* CvtCharToHex - Converts from a character value to the hexadecimal   ng
     P*                 text to the character values
     P* Parameter 1 - character field (any length, .5x's size of parm 2)
     P* Parameter 2 - size to be converted (size of parm 1)
     P* Returns - character field
     P*=====================================================================
     DCvtCharToHex     pr         65534    opdesc
     D charVal                    32767A   options(*varsize)
     D inSize                        10I 0 value options(*nopass)
     P*=====================================================================
     P* CvtHexToChar - Converts from hexadecimal text to the character form ng
     P*                 of the hex values
     P* Parameter 1 - character field (any length, 2x's size of return)
     P* Parameter 2 - size to be converted (size of parm 1)
     P* Returns - character field
     P*=====================================================================
     DCvtHexToChar     pr         32767    opdesc
     D hexVal                     65534A   options(*varsize)
     D inSize                        10I 0 value options(*nopass)
     P*=====================================================================
     P* CharToHex - Converts from a character value to the hexadecimal text ng
     P*              to the character values
     P* Parameter 1 - character field (any length, 2x's size of parm 2)
     P* Parameter 2 - character field (any length)
     P* Parameter 3 - size to be converted (size of parm 1)
     P*=====================================================================
     DCharToHex        pr                  extProc('cvthc')
     D szHexVal                   65534A   options(*varsize)
     D szCharVal                  32767A   options(*varsize)
     D nSrcLen                       10I 0 value
     P*=====================================================================
     P* HexToChar - Converts from hexadecimal text to the character form    ng
     P*              of the hex values
     P* Parameter 1 - character field (any length)
     P* Parameter 2 - character field (any length, 2x's size of parm 1)
     P* Parameter 3 - size to be converted (size of parm 2)
     P*=====================================================================
     DHexToChar        pr                  extProc('cvtch')
     D szCharVal                  32767A   options(*varsize)
     D szHexVal                   65534A   options(*varsize)
     D nSrcLen                       10I 0 value
     P*====================================================================
     P* Remove Exception Characters - JSON
     P*  Parameter 1 - character field (any length)
     P*  Parameter 2 - character field (optional)
     P*  Returns - character field (length of parameter 1)
     P*====================================================================
     DRmvExceptJSON    pr         32766    opdesc varying
     D ##inField                  32766    options(*varsize) const
     D ##incSize                      5  0 options(*nopass)
     P*====================================================================
     P* Scan string for source and replace (%scanrpl is available in OS7.1)
     P*  Parameter 1 - string to be scanned for (any length)
     P*  Parameter 2 - replacement string (any length)
     P*  Parameter 3 - string to be scanned (any length)
     P*  Parameter 4 - start position (optional)
     P*  Parameter 5 - length to scan (optional)
     P*  Returns - string with replacements
     P*====================================================================
     DScanReplace      pr         32766    opdesc
     D inScan                     32766    options(*varsize) const
     D inReplace                  32766    options(*varsize) const
     D inString                   32766    options(*varsize) const
     D inStart                        5  0 options(*nopass)  const
     D inLength                       5  0 options(*nopass)  const
     P*====================================================================
     P* Remove Exception Characters
     P*  Parameter 1 - character field (any length)
     P*  Parameter 2 - character field (optional)
     P*  Returns - character field (length of parameter 1)
     P*====================================================================
     DRmvExceptXML     pr         32766    opdesc
     D ##inField                  32766    options(*varsize) const
     D ##incSize                      5  0 options(*nopass)

     P*=====================================================================
     P* TitleCase - translates a character field to upper/lower case
     P*  Parameter 1 - character field (any length)
     P*  Returns - character field (length of parameter 1)
     P*=====================================================================
     D TitleCase       PR         32766    opdesc
     D  inText                    32766    options(*varsize) const

     P*=====================================================================
     P* ValidChar - Validate No Special Charactere in field
     P*  Parameter 1 - character field (any length)
     P*  Returns - character field (length of parameter 1)
     P*=====================================================================
     D ValidChar       PR            30    opdesc
     D  instring                     30    options(*varsize) const
     D  mode                          1    options(*varsize) const
     D  error                         1    options(*nopass)
     P*=====================================================================
     P* ExtrAlphaNum - Extract AlphaNumeric Characters from a string and
     P*                replace non-alphanumeric characters with blanks.
     P*
     P*  Parameter 1 - character field (any length)
     P*  Returns - character field (Alphanumeric characters only)
     P*=====================================================================
     D ExtrAlphaNum    PR         32766    opdesc
     D  instring                  32766    options(*varsize) const
     P*====================================================================
     P* Replace RPG characters with HTML
     P*  Parameter 1 - character field (any length)
     P*  Parameter 2 - character field (optional)
     P*  Returns - character field (length of parameter 1)
     P*====================================================================
     DCvtRPGtoHTML     pr         32766    opdesc
     D ##inField                  32766    options(*varsize) const
     D ##incSize                      5  0 options(*nopass)
     P*====================================================================
     P* Scan Reverse - Sacn a string in reverse order (like BIF %scanr)
     P*  Parameter 1 - string to be scanned for (any length)
     P*  Parameter 2 - string to be scanned (any length)
     P*  Parameter 3 - start position (optional)
     P*  Parameter 4 - length to scan (optional)
     P*  Returns - position of string to be scanned
     P*====================================================================
     DScanReverse      pr             5  0 opdesc
     D inScan                     32766    options(*varsize) const
     D inString                   32766    options(*varsize) const
     D inStart                        5  0 options(*nopass)  const
     D inLength                       5  0 options(*nopass)  const
     P*** Beg Add *** 10/05/2022 ******************************************
     P*=====================================================================
     DChkFileName      PR             1N
     D fileName                     200    Const
     D Error                        200    options(*NoPass)
     P*=====================================================================
     P* ClnFileName  : Clean File Name (Replace Special Characters with Blank
     P*  Parameter 1 - Character Field 200 A
     P*=====================================================================
     DClnFileName      PR           200A
     D inFileName                   200A
     P*** End Add *** 10/05/2022 ******************************************
     P*** Beg Add *** 02/28/25 ********************************************
     P*=====================================================================
     P* DayToLetter  : Convert day of month to a letter                     k
     P*  Parameter 1 - Numeric Field 2 S
     P*=====================================================================
     DDayToLetter      PR             1a
     D inDayOfMonth                   2s 0
     P*** End Add *** 02/28/25 ********************************************
