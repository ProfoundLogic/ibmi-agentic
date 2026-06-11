      **************************************************************************
      * This material is provided by IBM for illustrative purposes             *
      * only and has not been thoroughly tested under all conditions.          *
      * IBM, therefore, cannot guarantee or imply reliability,                 *
      * serviceability, or function of this material.  IBM provides            *
      * no program services for this material.  All material contained         *
      * herein is provided to you "AS IS" without any warranties of            *
      * any kind.  THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS          *
      * FOR A PARTICULAR PURPOSE AND NON-INFRINGMENT ARE EXPRESSLY             *
      * DISCLAIMED.  SOME JURISDICTIONS DO NOT ALLOW THE EXCLUSION             *
      * OF IMPLIED WARRANTIES, SO THE ABOVE EXCLUSIONS MAY NOT APPLY           *
      * TO YOU.  IN NO EVENT WILL IBM BE LIABLE TO ANY PARTY FOR ANY           *
      * DIRECT, INDIRECT, SPECIAL OR OTHER CONSEQUENTIAL DAMAGES FOR           *
      * ANY USE OF THIS MATERIAL, INCLUDING, WITHOUT LIMITATION, ANY           *
      * LOST PROFITS, BUSINESS INTERRUPTION, LOSS OF PROGRAMS OR OTHER         *
      * DATA ON YOUR INFORMATION HANDLING SYSTEM OR OTHERWISE, EVEN            *
      * IF WE ARE EXPRESSLY ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.        *
      *                                                                        *
      * (C) COPYRIGHT IBM CORP. 1997 - 2003                                    *
      * All rights reserved.                                                   *
      * US Government Users Restricted Rights -                                *
      * Use, duplication, or disclosure restricted                             *
      * by GSA ADP Schedule Contract with IBM Corp.                            *
      *                                                                        *
      * Licensed Materials - Property of IBM                                   *
      **************************************************************************
      * Revisions:
      *
      * 09/12/11  C:33  Seq#: OP099/77724
      *                 Add stat64 prototype
      ******************************************************************
      * Prototype for AddMsg (Add A Message) Subprocedure
      ******************************************************************
      * Adds a message to the message list

      * Inputs:
      *   - Message Text
      *   - Level (value 1, 2, or 3).  Optional.  Default is 1.

      * Return value:
      *    0 No problems
      *   -1 Array full
      *   -2 Invalid level. Message is added with level =  1.

     D AddMsg          pr            10i 0
     D   MsgText                    200    value varying
     D   MsgLevel                    10i 0 value options(*nopass)

      ***********************************************************************
      * Prototype for c's bsearch function
      ***********************************************************************
     D bsearch         pr              *   extproc('bsearch')
     D  SearchArg                      *   value
     D  DataStart                      *   value
     D  Elements                     10u 0 value
     D  Elemsize                     10u 0 value
     D  CompFunc                       *   ProcPtr value

      ****************************************************************************
      * Prototype for CEEDOD, get operational descriptors
      ****************************************************************************
     D CEEDOD          PR
     D  Parmnum                      10i 0 CONST                                Parameter number
     D  DescType                     10i 0
     D  DataType                     10i 0
     D  DescInfo1                    10i 0
     D  DescInfo2                    10i 0
     D  InLength                     10i 0
     D  Feedback                     12a   options(*omit)

      ******************************************************************
      * Prototype for CfgMsgs (Configure Messages) Subprocedure
      ******************************************************************
      * Sets up section names, variable names for use by the
      * WrtMsgs subprocedure.  All parameters are optional.  Defaults
      * are assigned in the globals, above.

      * Inputs:
      *   MsgFieldName:  name of the externally described HTML's field to
      *                  receive the message text.  The default is msgtext.
      *   MsgStrSection: name of the HTML section used to start the error
      *                  message output.  The default is msgstart.
      *   MsgEndSection: name of the HTML section used to end the error
      *                  message output.  The default is msgend.
      *   MsgL1Section:  name of the HTML section used to output level 1
      *                  error messages.  It should include the MsgFieldName
      *                  substitution variable.  The default is msgl1.
      *   MsgL2Section:  name of the HTML section used to output level 2
      *                  error messages.  It should include the MsgFieldName
      *                  substitution variable.  The default is msgl2.
      *   MsgL3Section:  name of the HTML section used to output level 3
      *                  error messages.  It should include the MsgFieldName
      *                  substitution variable.  The default is msgl3.

     D CfgMsgs         pr
     D  MsgFieldName                 30    value varying options(*nopass)
     D  MsgStrSection                50    value varying options(*nopass)
     D  MsgEndSection                50    value varying options(*nopass)
     D  MsgL1Section                 50    value varying options(*nopass)
     D  MsgL2Section                 50    value varying options(*nopass)
     D  MsgL3Section                 50    value varying options(*nopass)

      ***********************************************************************
      * Prototype for cgivarcnt.  Returns number of occurences of a cgi input variable
      ***********************************************************************
      * Inputs:  - cgi variable name
      *          - string containing browser's input string.  Should have had escape sequences
      *            converted from ASCII to EBCDIC by the getinput subprocedure.
      *          - length of data in browser's input string (inactln)
      *
      * Outputs: - number of occurences of cgi variable name in browser's input string
      *            - 0 if none found
      *
      * Example:      eval      namecnt = cgivarcnt('name':indata:inactln)
      *
     D cgivarcnt       pr            10i 0
     D  cgivarnam                    50    const
     D  InData                    32767    const varying options(*varsize)
     D  InActLn                      10i 0 const

      ***********************************************************************
      * Prototype for cgivarval.  Returns value of nth occurence of a cgi input variable
      ***********************************************************************
      * Inputs:  - cgi variable name
      *          - cgi variable occurence number (n)
      *          - string containing browser's input string.  Should have had escape sequences
      *            converted from ASCII to EBCDIC by the getinput subprocedure.
      *          - length of the string
      *          - return code
      *
      * Outputs: - value of nth occurence of cgi variable name in browser's input string with
      *            any escape sequences converted to characters.
      *              - set to blanks if return code  < 0.  The value is returned as a
      *              string.
      *
      *          - return code: 0 = successful
      *                        -1 = variable not found
      *                        -2 = variable found but occurrence does not exist
      *
      * Example:      eval      cust2 = cgivarval('cust':2:indata:inactln:rc)
      *
      *
     D cgivarval       pr         32767    varying
     D  cgivarnam                    50    const
     D  cgivarocc                    10i 0 const
     D  InData                    32767    const varying options(*varsize)
     D  InActLn                      10i 0 const
     D  rc                           10i 0

      ***********************************************************************
      * Prototype for cgivarvalUpper.
      ***********************************************************************
      * Returns upper case value of nth occurence of a cgi input variable
      * Inputs:  - cgi variable name
      *          - cgi variable occurence number (n)
      *          - string containing browser's input string.  Should have had escape sequences
      *            converted from ASCII to EBCDIC by the getinput subprocedure.
      *          - length of the string
      *          - return code
      *
      * Outputs: - value of nth occurence of cgi variable name in browser's input string with
      *            any escape sequences converted to characters.
      *              - set to blanks if return code  < 0.  The value is returned as a
      *              string.
      *
      *          - return code: 0 = successful
      *                        -1 = variable not found
      *                        -2 = variable found but occurrence does not exist
      *
      * Example:      eval      cust2 = cgivarvalupper('cust':2:indata:inactln:rc)
      *
      *
     D cgivarvalupper  pr         32767    varying
     D  cgivarnam                    50    const
     D  cgivarocc                    10i 0 const
     D  InData                    32767    const varying options(*varsize)
     D  InActLn                      10i 0 const
     D  rc                           10i 0

      ***********************************************************************
      * Prototype for char2hex.
      ***********************************************************************
      * Converts characters to their hexadecimal characters
      * Input: variable length character field
      * Output variable length character field, twice as long, containing
      *        the hex digits for each input character.
      *
      * Example:      eval      hex = char2hex('ABC') returns the characters C1C2C3
      *
      * Uses MI instruction cvthc. Bind with binding directory QSYS/QC2LE.
      *
     D char2hex        pr         32000    varying
     D   charsin                  16000    const varying options(*varsize)

      ***********************************************************************
      * Prototype for chknbr
      ***********************************************************************
      * Checks a character string to see if it contains a valid number.
      * The only valid characters are -0123456789 and the character being used
      * as the decimal point.
      *
      * Optionally, when errors are found, uses AddMsg to write records into the
      * error message arrays.

      * Inputs: - variable length character field containing data intended to
      *           be a valid decimal number.
      *         - maximum number of digits to the left of the decimal point
      *           (optional parameter).  0 or not passed means don't check it.
      *           If greater than 21, it is changed to 21.
      *         - whether to add messages to the message arrays using AddMsg
      *           if errors are found.
      *         - field's description for AddMsg purposes
      *         - whether to consider a negative value an error for AddMsg
      *           purposes.  If this parameter is not passed, negative
      *           is not considered an error.

      * Returns: Data structure containing indicators (seven indicators)
      *        1 = *on = one or more errors.  *off = no errors
      *        2 = non-numeric characters (includes minus sign in wrong place)
      *        3 = multiple decimal points
      *        4 = multiple signs (both leading and trailing)
      *        5 = zero length input or no numeric characters
      *        6 = error in number of digits to left or right of decimal
      *            point.  Set on if:
      *            - more than 21 digits to the left of the decimal point
      *            - more than 9 digits to the right of the decimal point
      *            - more digits to the left of the decimal point than
      *              specified in maxdigits parameter.
      *        7 = no errors, but value is less than 0.

     D chknbr          pr             7
     D  chars                        32    const varying options(*varsize)
     D  MaxDigits                    10i 0 const options(*nopass)
     D  AddMsgs                        n   const options(*nopass)
     D  FieldDescription...
     D                               32    const varying options(*nopass)
     D  NegIsError                     n   const options(*nopass)

      ******************************************************************
      * Prototype for ClrHtmlBuffer subprocedure
      ******************************************************************
      * Clears the HTML output buffer.
      *
      * Use this subprocedure to empty the buffer without sending it
      * to either the browser or to a stream file.
      *
      * This is useful when program logic dictates you need to output
      * something other than what has already been buffered.
     D ClrHtmlBuffer   pr

      ******************************************************************
      * Prototype for ClrMsgs (Clear Messages) Subprocedure
      ******************************************************************
      * Logically sets the number of messages to 0
     D ClrMsgs         pr

      ***********************************************************************
      * Prototype for contlen (content_length)
      ***********************************************************************
      * Returns as a numeric variable, the CONTENT_LENGTH environment
      * variable.  Used by getinput.  Should not be called directly.
     D contlen         pr            10i 0                                      content length

      ***********************************************************************
      * Prototype for countp.
      ***********************************************************************
      * Increments count record whose key is
      * keyvalin and returns updated count.  If record does not
      * exist, it is created and set to 1.
     D countp          pr            10i 0                                      Counter value
     D  keyvalin                     20    varying const                        Counter key

      ***********************************************************************
      * Prototype for CrtCookie
      ***********************************************************************
      * Creates a cookie string in the form:
      *   Set-Cookie: NAME=VALUE; expires=DATE; path=PATH; domain=DOMAIN_NAME; secure
      * See http://ww.netscape.com/newsref/std/cookie_spec.html for details about
      * how cookies work.
      *
      * Parameters
      *  - Cookie's name                      (required input)
      *  - Cookie's value                     (required input)
      *  - Return code                        (required output)
      *                                       0 = no errors
      *                                       less than zero is sum of following:
      *                                       -1 blank cookie name
      *                                       -2 blank cookie value
      *                                       -4 invalid timestamp
      *  - Cookie's domain                    (required input, null or blank
      *                                        means none)
      *  - Cookie's path                      (required input, null or blank
      *                                        means none)
      *  - Whether secure                     (optional input: *on=yes *off=no)
      *  - Cookie's expiration timestamp      (optional input) timestamp in
      *                                       format: YYYY-MM-DD-HH.MM.SS.MMMMMM
      *                                       Use local date and time.  CrtCookie
      *                                       converts to GMT.  Note: the UTC
      *                                       offset is as of run time, not as of
      *                                       the expiration date, so the expiration
      *                                       date/time could be off by the time difference
      *                                       between the UTC offset on standard
      *                                       versus daylight savings time.
      * Returns:
      *  - variable length character field, containing the
      *    generated Set-Cookie header.  Blank if any errors occurred.
      *
      * Example:         eval      mycookie = CrtCookie('TEST':'The Data':
      *                                       RC:'ibm.com':'/':*off:
      *                                       '2001-10-11-12.12.12.000000')

     D CrtCookie       pr          5000    varying
     D  Name                       1000    const varying options(*varsize)
     D  Value                      4000    const varying options(*varsize)
     D  ReturnCode                   10i 0
     D  Domain                     1000    const varying options(*varsize)
     D  Path                       1000    const varying options(*varsize)
     D  Secure                         n   const options(*nopass)
     D  Expires                        z   const options(*nopass)

      ******************************************************************
      * Prototype for CrtTagOpt
      ******************************************************************
      * Creates an option tag.

      * Use this subprocedure to build selection boxes when the
      * contents are not static, or the SELECTED option varies.

      * Do not use it for static selection boxes.  Static, externally
      * defined HTML is much more efficient.

      * Inputs:
      *   - String to be used for the value attribute
      *   - String for the tag's associated text.
      *   - Optional input containing the value of the option to have
      *     the SELECTED attribute.  If this parameter matches the
      *     first parameter, the SELECTED attribute is output.

      * Ouput:
      *   - String containing an option tag.  For example:

      * Examples
      *   CrtTagOpt('AZ':Arizona) returns
      *       <option value="AZ">Arizona</option>

      *   CrtTagOpt('AZ':Arizona:'AZ') returns
      *       <option value="AZ" SELECTED>Arizona</option>

      *   CrtTagOpt('AZ':Arizona:'MN') returns
      *       <option value="AZ">Arizona</option>

     D CrtTagOpt       pr           200    varying
     D  value                        50    const varying
     D  text                        100    const varying
     D  selected                     50    const varying options(*nopass)

      **************************************************************************
      * Prototype for CrtUsrSpc
      **************************************************************************
      * Creates a randomly named, automatically extendible user space
      * in a user-specified library.  The user space's contents are initialized
      * to all x'00's.

      * Required Parameters
      *  - User space library (input)
      *    - If the library not found, CrtUsrSpc sets the user space name
      *      to blanks and MsgId to CPF9810
      *    - If the requestor does not have change authority to the library,
      *      CrtUsrSpc sets the user space name to blanks and MsgId to CPF2144
      *  - Pointer to user space (output)
      *    - Set to null if the user space is not created
      *  - Message ID (output)
      *    - blank if no errors
      *    - else, message id of error
      * Optional Parameters
      *  - Public authority (input)
      *    - If not passed, it is set to *EXCLUDE
      *  - Text (input)
      *    - If not passed, it is set to 'Created by CGIDEV2' plus timestamp
      *  - Initial size (input)
      *    - If not passed, it is set to 12288
      *  - Extended attribute
      *    - If not passed, it is set to blanks

      *  Returns
      *   - If successful
      *     - User space name
      *   - Otherwise
      *     - Blanks
      *
      *  Errors in system APIs
      *   - If any of the called system APIs fails, a message is forced into
      *     the CGIDEBUG file.
      **************************************************************************
     D CrtUsrSpc       pr            10
     D  usrSpcLib                    10    value
     D  usrSpcPtr                      *
     D  msgid                         7
     D  pubAut                       10    value options(*nopass)
     D  text                         50    value options(*nopass)
     D  initialSize                  10i 0 value options(*nopass)
     D  extAttr                      10    value options(*nopass)

      ***********************************************************************
      * Prototype for MI instruction cvtch (Convert Eight Bit
      * Characters to Hex Nibbles)
      ***********************************************************************
      * Converts eight bit characters to hex nibbles.  2 bytes become 1 byte.
     D cvtch           pr                  extproc('cvtch')
     D  receiver                       *   value
     D  source                         *   value
     D  size                         10i 0 value
     D
      ***********************************************************************
      * Prototype for CvtDb
      ***********************************************************************
      * Calls QtmhCvtDb API from service program QHTTPSVR/QZHBCGI
      * Uses wrtdebug subprocedure to send information about the call to the debugging file.
     D CvtDb           pr                  opdesc
     D  DbFileName                   20                                         Filename & Library
     D  InData                    32767    options(*varsize)                    Input data
     D  InActLn                      10i 0                                      Input data length
     D  DSBuffer                  32767    options(*varsize)                    Data structure buffr
     D  DBDSLn                       10i 0                                      Data struct length
     D  DBActLn                      10i 0                                      Data struc retrn len
     D  DBRespCd                     10i 0                                      Response Code
     D  qusec                       516    options(*varsize)                    Error structure

      ***********************************************************************
      * Prototype for MI cvtefnd (Convert External Form to Numeric Value)
      ***********************************************************************
     D cvtefnd         pr             8f   extproc('cvtefnd')
     D  string                       32
     D  length                       10i 0 value
     D  mask                          3
     D
      ***********************************************************************
      * Prototype for MI instruction cvthc (Convert Hex to Character)
      ***********************************************************************
      * Converts nibbles to characters (1 byte becomes 2)
     D cvthc           pr                  extproc('cvthc')
     D  Receiver                       *   value
     D  Source                         *   value
     D  size                         10i 0 value

      ***********************************************************************
      * Prototype for c2n, converts up to 32-byte string to floating point number
      ***********************************************************************
      * Converts a character string to a floating point variable.  If
      * non-zero, adds a small fuzz to the result in an attempt to
      * ensure that subsequent rounding works as expected.
      *
      * Input: variable length character field containing a valid
      *        decimal number in display format.
      * Output: floating point number which can then be converted to
      *        some other form either by assignment or via %DEC, %DECH,
      *        %INT, or %INTH built-in functions.
      *
      * The only characters included in the conversion are digits, the
      * minus sign, and the current RPG decimal point character.
      * If multiple decimal points are found, only the first one is used.
      *
      * Example:      eval      float = c2n('-123.34')
      *
      * Uses MI instruction cvtefnd.  Bind with binding directory QSYS/QC2LE.
      *
     D c2n             pr             8f
     D  c                            32    const varying options(*varsize)

      ***********************************************************************
      * Prototype for c2n2 subprocedure
      ***********************************************************************
      * Converts a character string to a 30,9 packed variable.

      * Adapted from getnum subprocedure, written by Barbara Morris,
      * IBM Toronto laboratory.
      *
      * This subprocedure avoids precision problems with large
      * floating point numbers by doing virtually all its work with
      * characters.  Performance is improved too.

      * Input: variable length character field containing a valid
      *        decimal number in display format.
      * Output: 30p 9 number
      *
      * The only characters included in the conversion are digits, the
      * minus sign, and the current RPG decimal point character.
      * If multiple decimal points are found, only the first one is used.
      *
      * Example:      eval      number = c2n2('-123.34')

     D c2n2            pr            30p 9
     D  string                       32    const varying

      ***********************************************************************
      * Prototype for Docmd.
      ***********************************************************************
      * Executes a CL command
      * Returns 0 if executed without error; otherwise 1.
     D DoCmd           pr            10i 0
     D  cmd                        2000    const varying options(*varsize)

      ******************************************************************
      * Prototype for Encode
      ******************************************************************
      * Returns a varying length field built from an input varying
      * length field in which the following characters are converted
      * to their HTML character entities:
      *   Character      Character Entity
      *   ---------      ----------------
      *      "              &quot;
      *      &              &amp;
      *      <              &lt;
      *      >              &gt;

      * The input field can be any size up to 32767.  Be sure
      * that it has a correct length as shown in any of the
      * following examples:
      *   - Passing a literal
      *                  eval      result = encode('<html>')
      *   - Assigning value to a varying field.
      *                  eval      vfield = '<html>'
      *                  eval      result = encode(vfield)
      *   - Assigning a value to a fixed field
      *                  eval      ffield = '<html>'
      *                  eval      result = encode(%trimr(ffield))
      *   - Passing an expression
      *                  eval      result = encode('abc' + %trimr(ffield) +
      *                            vfield + 'xyz')

     D Encode          pr         32767    varying
     D  Input                     32767    const options(*varsize) varying

      ******************************************************************
      * Prototype for EncodeBlanks
      ******************************************************************
      * Returns a varying length field built from an input varying
      * length field in which any blanks are converted to the HTML
      * entity, &nbsp; (non-breaking space).

      * The input field can be any size up to 32767.  Be sure
      * that it has a correct length as shown in any of the
      * following examples:
      *   - Passing a literal
      *                  eval      result = EncodeBlanks('<html>')
      *   - Assigning value to a varying field.
      *                  eval      vfield = '<html>'
      *                  eval      result = EncodeBlanks(vfield)
      *   - Assigning a value to a fixed field
      *                  eval      ffield = '<html>'
      *                  eval      result = EncodeBlanks(%trimr(ffield))
      *   - Passing an expression
      *                  eval      result = EncodeBlanks('abc' +
      *                            %trimr(ffield) + vfield + 'xyz')

     D EncodeBlanks    pr         32767    varying
     D  Input                     32767    const options(*varsize) varying

      ***********************************************************************
      * Prototype for fixmixed (need to use only when CGI_MODE = %%MIXED%%)
      ***********************************************************************
      * Used by getinput subprocedure.  It should not be necessary to call fixmixed
      * directly.

      * Modifies CGI input string's escape sequences: in the data portion of a couplet
      * the ASCII code points are converted to the corresponding EBCDIC code points. For
      * those in the field name portion, escape sequences are converted the correct single
      * EBCDIC character.

      * Inputs:  - string containing input from the browser
      *          - length of the string
      * Outputs: - modified string with escape sequences in the data portion of each field converted
      *            to EBCDIC escape sequences and those in the field name part converted to an
      *            EBCDIC character.
      *
      * Example:      callp     fixmixed(inbuff:inactln)
      *
     D fixmixed        pr                  opdesc
     D  string                    32767    options(*varsize)
     D  stringln                     10i 0

      ****************************************************************************
      * Prototype for subprocedure errno
      ****************************************************************************
      * Returns C environment's errno parameter.
     D errno           pr            10i 0

      ****************************************************************************
      * Prototype for subprocedure errnotxt
      ****************************************************************************
      * Returns C environment's errno text
     D errnotxt        pr           100    varying
     D  errno                        10i 0 value

      **************************************************************************
      * GetCookieByName subprocedure
      **************************************************************************
      * Retrieves a cookie's value given its name and occurrence.
      *
      * Parameters
      *  - Cookie's name                      (required input)
      *  - Cookie's occurrence                (optional input)
      *                                       1 is assumed if not passed.

      * Returns:
      *  - value of the cookie having the name and occurrence.  If not
      *    found, returns a null string.
      *
      * Example:         eval      x = GetCookieByName('TEST':1)

     D GetCookieByName...
     D                 pr          5000    varying
     D  Name                       1000    const varying options(*varsize)
     D  Occurrence                   10i 0 const options(*nopass)

      ***********************************************************************
      * Prototype for getenv.
      ***********************************************************************
      * Gets an environment variable
      * If an error occurs, returns blank and error information in qusec

     D getenv          pr         32767    varying
     D  envvarnam                   100    const varying options(*varsize)
     D  qusec                       516    options(*varsize)                    Error structure

      ***********************************************************************
      * Prototype for gethtml
      ***********************************************************************
      * Overrides to specified source physical file and loads arrays with HTML
      * data from the source physical file.  Arrays subsequently used by
      * wrtsection, etc.
      *
      * The optional sectionDelimStart parameter is used to override
      * the default starting section delimiter of /$.
      *
      * The optional sectionDelimEnd  parameter is used to override the
      * default non-existent ending section delimiter.
      *
      * The optional varDelimStart parameter is used to override the default
      * start variable delimiter of /%

      * The optional varDelimEnd parameter is used to override the default
      * end variable delimiter of %/
      *
     D gethtml         pr
     D  fn                           10    const
     D  lib                          10    const
     D  mbr                          10    const
     D  sectionDelim...
     D  Start                        20    const varying options(*nopass)
     D  sectionDelim...
     D  End                          20    const varying options(*nopass)
     D  varDelimStart                20    const varying options(*nopass)
     D  varDelimEnd                  20    const varying options(*nopass)

      ***********************************************************************
      * Prototype for gethtmlIFS
      ***********************************************************************
      * Loads arrays with HTML data from an IFS file.
      * The arrays are subsequently used by wrtsection, etc.
      *
      * The optional sectionDelimStart parameter is used to override
      * the default starting section delimiter of /$.
      *
      * The optional sectionDelimEnd  parameter is used to override the
      * default non-existent ending section delimiter.
      *
      * The optional varDelimStart parameter is used to override the default
      * start variable delimiter of /%

      * The optional varDelimEnd parameter is used to override the default
      * end variable delimiter of %/
      *
     D gethtmlIFS      pr
     D  IfsFile                    1024    const varying
     D  sectionDelim...
     D  Start                        20    const varying options(*nopass)
     D  sectionDelim...
     D  End                          20    const varying options(*nopass)
     D  varDelimStart                20    const varying options(*nopass)
     D  varDelimEnd                  20    const varying options(*nopass)

      ******************************************************************
      * Prototype for GetHtmlIFSMult
      ******************************************************************
      * Loads arrays with HTML data from multiple IFS files.

      * The arrays are subsequently used by wrtsection, etc.

      * Inputs
      * ------
      *   The IfsFiles parameter contains the names of the files.  A blank
      *   signifies the end of a file's name.  The following limitations
      *   apply:
      *     - Maximum length per file name: 255 bytes not counting the
      *       blank seperator
      *     - Maximum length of all the input file names, including any
      *       blank separators: 32767
      *     - Maximum number of file names: 127

      *   All the files must use the same section name delimiters
      *   and substitution variable delimiters.

      *   The optional sectionDelimStart parameter is used to override
      *   the default starting section delimiter of /$.

      *   The optional sectionDelimEnd  parameter is used to override the
      *   default non-existent ending section delimiter.
      *
      *   The optional varDelimStart parameter is used to override the default
      *   start variable delimiter of /%

      *   The optional varDelimEnd parameter is used to override the default
      *   end variable delimiter of %/

      * Return value
      * ------------
      * GetHtmlIFSMult returns a data structure containing an array of six
      * indicators that can be checked to find out if any errors occurred.
      * The indicators and their meanings are:
      *  - NoErrors:        *on  = no error occurred
      *                     *off = one or more errors.  Check other indicators.
      *  - NameTooLong      *on  = one or file's name exceeds 255 characters.
      *                            File is ignored.
      *  - NotAccessible    *on  = File or directory not found, authorization
      *                            failure, etc.  File is ignored.
      *  - NoFilesUsable    *on  = All the files have been ignored.
      *  - DupSections      *on  = One or more duplicate sections were found.
      *                            Only the first occurrence is used.
      *  - FileIsEmpty      *on  = File is empty and is ignored.

     D GetHtmlIFSMult  pr             6
     D  IfsFiles                  32767    const varying options(*varsize)
     D  sectionDelim...
     D  Start                        20    const varying options(*nopass)
     D  sectionDelim...
     D  End                          20    const varying options(*nopass)
     D  varDelimStart                20    const varying options(*nopass)
     D  varDelimEnd                  20    const varying options(*nopass)

      ***********************************************************************
      * Prototype for getinput
      ***********************************************************************
      * Gets and returns browser's input, input length and REQUEST_METHOD
      * (GET or POST).
      *
      * Logic flow
      * - Determine request_method
      *     - If POST, gets data from standard input
      *     - If GET, gets data from query_string
      * - Uses the fixmixed subprocedure to convert ASCII escape sequences
      *   to their EBCDIC equivalents (if CGI_MODE is %%MIXED%%)
      * - Puts data and length into Indata and InActLn, respectively
      * - Puts request_method into InDataType
      ******************************************************************
      *
     D getinput        pr                  opdesc
     D  InData                    32767    options(*varsize)                    Input data
     D  InActLn                      10i 0                                      Actual length Rcvd
     D  InDataType                    4                                         Returns GET or POST

      ******************************************************************
      * Prototype for GetHtmlBytesBuffered Subprocedure
      ******************************************************************
      * Returns the number of bytes in the output HTML buffer.

      * This number is incremented each time output is written with
      * WrtSection or WrtNoSection.

      * It is reset to 0 when either WrtSection('*fini') or
      * WrtHtmlToStmf is run.

      * If this number is allowed to grow to more than 16 MB, the CGI
      * program will fail.

     D GetHtmlBytesBuffered...
     D                 pr            10i 0

      ******************************************************************
      * Prototype for GetMsgCnt (Get Message Count) Subprocedure
      ******************************************************************
      * Returns number of message currently in the arrays.
     D GetMsgCnt       pr            10i 0

      ***********************************************************************
      * Prototype for getsessionid.
      ***********************************************************************
      * Inputs:  - none
      *
      * Outputs: - 15-character session id: comprising the 6 digits of the
      *            job number followed by 9 random digits
      *
      * Example:      eval      sessionid = getsessionid
      *
     D getsessionid    pr            15

      ***********************************************************************
      * Prototype for hex2char
      ***********************************************************************
      * Converts hex characters to the characters they represent
      * Input: variable length character field containing hex characters
      * Output variable length character field, half as long, containing
      *        the character representation of each input pair of hex
      *        characters.
      *
      * Example:      eval      hex = char2hex('ABC') returns the characters C1C2C3
      *
      * Uses MI instruction cvtch. Bind with binding directory QSYS/QC2LE.
      *
     D hex2char        pr         16000    varying
     D  hexcharsin                32000    const varying options(*varsize)

      ***********************************************************************
      * Prototype for isdebug. Returns '1' if debuggins is on; '0' if not.
      ***********************************************************************
     D isdebug         pr             1n

      ***********************************************************************
      * Prototype for putenv.
      ***********************************************************************
      * Puts an environment variable
      * If an error occurs, error information in qusec
     D putenv          pr
     D  putenvval                 32767    const varying options(*varsize)
     D  qusec                       516    options(*varsize)                    Error structure

      ***********************************************************************
      * Prototype for c's qsort function
      ***********************************************************************
     D qsort           pr                  extproc('qsort')
     D  DataStart                      *   value
     D  ElemCount                    10u 0 value
     D  ElemSize                     10u 0 value
     D  CompFunc                       *   ProcPtr value

      ***********************************************************************
      * Prototype for qtmhgetenv, calls QtmhGetEnv API from service program QTCP/QTMHCGI
      ***********************************************************************
     D qtmhgetenv      pr                  extproc('QtmhGetEnv')
     D  envrcvr                        *   value
     D  envrcvrln                    10i 0
     D  envrspln                     10i 0
     D  envrqsnm                       *   value
     D  envrqsln                     10i 0
     D  qusec                       516    options(*varsize)                    Error structure

      ***********************************************************************
      * Prototype for qtmhputenv (put environment variable)
      ***********************************************************************
     D qtmhputenv      pr                  extproc('QtmhPutEnv')
     D  putenvstr                      *   value
     D  putenvlen                    10i 0
     D  qusec                       516    options(*varsize)                    Error structure

      ***********************************************************************
      * Prototype for QzhbCgiParse (CGIParse) API
      ***********************************************************************
     D qzhbcgiparse    pr                  extproc('QzhbCgiParse')
     D  cgipcmd                     300
     D  cgipfmt                       8
     D  cgipbuf                   32767    options(*varsize)
     D  cgipbufsize                  10i 0
     D  cgiprsplen                   10i 0
     D  qusec                       516    options(*varsize)                    Error structure

      ***********************************************************************
      * Prototype for random subprocedure
      ***********************************************************************
      * Returns a random unsigned integer between low and high.
      *
      * Inputs:  low end of range.  Min=1 Max=maxrange
      *          high end of range  Min=2 Max=maxrange + 1
      *          difference must be at least 2
      *
      * Maxrange is a constant, 2147483646, defined in module xxxrandom
      *
      * Uses CEERAN0.  On first call, sets seed to 0 so that CEERAN0 uses GMT.
      * On subsequent calls, uses seed as modified by previous calls to CEERAN0.
      *
      * Exceptions:
      *          If (abs(high - low) < 2) or (abs(high - low) > maxrange + 1)
      *             then range is changed to 1 to (maxrange + 1)
     D random          pr            10u 0                                      Random number
     D  low                          10u 0 value                                Low end of range
     D  high                         10u 0 value                                High end of range

      ***********************************************************************
      * Prototype for RandomString subprocedure
      ***********************************************************************
      * Returns a random, varying length string up to 1024 characters
      * in length.
      * You can select its format as shown below.  The characters
      * used are 0 - 9, a - z, and A - Z.
      *
      * Parameters

      *  - Number of characters to return (0 - 1024)
      *    If 0, a null string is returned.
      *    If > 1024, 1024 characters are returned.

      *  - First character (if not passed, defaults to *mixedDigit)
      *    - *upperLetter (upper case letter only)
      *    - *lowerLetter (lower case letter only)
      *    - *mixedLetter (upper or lower case letter only)
      *    - *upperDigit (upper case letter or digit)
      *    - *lowerDigit (lower case letter or digit)
      *    - *mixedDigit (upper or lower case letter or digit)
      *    - *digit (digit only)

      *  - Remaining characters (if not passed, defaults to *mixedDigit)
      *    - same choices as first character

      *  - UpperChars - characters that are "upper case")
      *    - If not passed, defaults to 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

      *  - LowerChars - characters that are "lower case")
      *    - If not passed, defaults to 'abcdefghijklmnopqrstuvwzyz'

      *  - DigitChars - characters that are "digits")
      *    - If not passed, defaults to '0123456789'
      *    -
      * Returns
      *  - Varying string containing the random characters

     D randomString    pr          1024    varying
     D  length                       10u 0 value
     D  firstChar                    12    value options(*nopass)
     D  remainChar                   12    value options(*nopass)
     D  UpperChars                  100    value varying options(*nopass)
     D  LowerChars                  100    value varying options(*nopass)
     D  DigitChars                  100    value varying options(*nopass)

      **************************************************************************
      * Prototype for RtvUsrSpcPtr
      **************************************************************************
      * Retrieves a pointer to a user space.

      * Parameters
      *  - User space name (input)
      *  - User space library (input)

      *  Returns
      *   - If successful
      *     - Pointer to the user space
      *   - Otherwise
      *     - Null pointer
      *
      *  Errors in system APIs
      *   - If the call to system API QUSPTRUS fails, a message is forced into
      *     the CGIDEBUG file.

     D RtvUsrSpcPtr    pr              *
     D  UsrSpcName                   10    value
     D  UsrSpcLib                    10    value

      ******************************************************************
      * Prototype for RtvHtmlRcd subprocedure
      ******************************************************************
      * Retrieves a single record from the externally described HTML

      * Parameters
      *  - Section  name (input).  If *NONE, gets record by relative
      *    record number regardless of section.
      *  - Relative record number (absolute or by section) (input)
      *  - Return code (output)
      *    0 = record found and returned
      *   -1 = section not found
      *   -2 = record not found
      *   -3 = record part of a duplicate section

      * Returns
      *  - If not found (less than 1 or greater than number of records
      *    read), or part of a duplicate section: a null field,
      *  - Otherwise, the record's contents.

     D RtvHtmlRcd      pr         32767    varying
     D  Section                      50    const
     D  RelRcd                       10i 0 const
     D  RetCode                      10i 0

      ******************************************************************
      * Prototypte for RtvSubsVarInfo subprocedure
      ******************************************************************
      * Retrieves information about substitution variables

      * Parameters
      *  - Section  name (input).  If *NONE, gets information by
      *    sequence number regardless of section.
      *  - Relative sequence number (absolute or by section) (input)
      *  - Data structure containing the following (output)
      *    - Section name
      *      - Character 50 varying
      *      - Null if return code not 0
      *    - Variable name
      *      - Character 30 varying
      *      - null if return code not 0
      *    - Variable's starting position in the html record
      *      - 10 digit unsigned
      *      - 0 if return code not 0
      *    - Length of variable's name (output)
      *      - 10 digit unsigned
      *      - 0 if return code not 0
      * Returns
      *  - Return code (output)
      *    - 10-digit signed integer
      *        0 = Substitution variable found and returned
      *       -1 = section not found
      *       -2 = Sequence number out of range for section or
      *            for list of all substitution variables

     D RtvSubsVarInfo  pr            10i 0
     D  SectionIn                    50    const
     D  RelSeqNo                     10i 0 const
     D  ReturnDS                     92

      ******************************************************************
      * Prototype for SetNoDebug subprocedure
      ******************************************************************
      * Sets value of global NoDebug indicator.
      * If this indicator is set to on (default is off)
      * calls to debugging routines return without doing
      * any work.
      *
      * NoDebug is also set off if the open of the cgidebug file
      * fails.

     D SetNoDebug      pr
     D  SetNoDebugInd                  n   value

      ******************************************************************
      * Prototype for updHTMLvar subprocedure
      ******************************************************************
      * Updates arrays containing variable names and values
      * Inputs
      *  - variable name
      *  - variable value
      *  - action (optional)
      *      - '1' = replace this variable in the arrays if it is already
      *              there. Otherwise add it to the arrays (default).
      *      - '0' = clear arrays and write variable as the first element.
      *  - trim instructions (optional)
      *    - %trim  - trim left and right (default)
      *    - %triml - trim left only
      *    - %trimr - trim right only
      *    - %trim0 - don't trim

     D updHTMLvar      pr
     D  name                         30    const varying options(*varsize)
     D  value                      1000    const varying options(*varsize)
     D  action                        1    value options(*nopass)
     D  trim                          6    value varying options(*nopass)
      ******************************************************************
      * Prototype for uppify subprocedure
      ******************************************************************
      * Converts lowercase characters to uppercase
      * Example:  charstring =  uppify(charstring)
     D uppify          pr         32767    varying
     D  data                      32767    const varying options(*varsize)

      ******************************************************************
      * Prototype for WrtDebug subprocedure
      ******************************************************************
      * Writes a message into the debugging physical file (cgidebug) if:
      *   - Debugging is on (CGIDEBUG command *ON, sets CGIDEBUG data
      *     area to '1')
      *   - Or, debugging is off, but force parameter is *on
      *   - Otherwise, wrtdebug returns without doing anything.
     D wrtdebug        pr
     D  dbgdatain                 32000    const varying options(*varsize)
     d  force                         1n   value options(*nopass)

      ******************************************************************
      * Subprocedure WrtHtmlToStmF: Writes HTML to stream file
      ******************************************************************
      * The contents of the HTML buffer are written to Stmf.
      * If the CodePage parmeter is passed, it is used in
      * writing the streamfile.  Otherwise, codepage 819 is
      * is used.
      * The return value is the C errno if an error was detected.
      * Otherwise, it is 0
     D WrtHtmlToStmf   pr            10i 0
     D  Stmf                       1024    const varying
     D  CodePage                     10u 0 const options(*nopass)

      ******************************************************************
      * Subprocedure wrtjobdbg: Writes qualified job name to debugging file
      ******************************************************************
     D wrtjobdbg       pr
     D  force                         1n   value options(*nopass)

      ******************************************************************
      * Prototype for WrtMsgs (Write Messages) Subprocedure
      ******************************************************************
      * Writes the messges in the arrays to standard output.  If no
      * messages are there, does nothing.

      * If section names set by CfgMsgs are null (length = 0), those
      * sections are not written.

     D WrtMsgs         pr

      ***********************************************************************
      * Prototype for WrtNoSection
      ***********************************************************************
      * Writes data for the browser without using substitution variables
      * or sections.
      * This subprocedure can be used when a large block of data is to
      * written.  This is more likely to happen when writing non-textual
      * data such as images.
      *
      * DataP is a pointer to the storage containing the data to be written.
      * DataLength is the number of bytes to be written, starting at that
      * location.
      *
      * Examples
      *  callp     WrtNoSection(%addr(MyBuffer):MyBufferLength)
      *  callp     WrtNoSection(MyPointer:MyBufferLength)

     D WrtNoSection    pr
     D  DataP                          *   const
     D  DataLength                   10i 0 const

      ***********************************************************************
      * Prototype for wrtpsds.
      ***********************************************************************
      * Writes program status data structure to cgidebug file.
     D wrtpsds         pr
     D  psdsin                      429

      ***********************************************************************
      * Prototype for wrtsection
      ***********************************************************************
      *  (writes one or more sections)
      * If optional parameter, nonewline, is *on, a newline character (x'15')
      * is not appended to each html output line.  This is useful when binary
      * data are being sent to the browser.
     D wrtsection      pr
     D  sections                   1000    value varying                        section name
     D  nonewline                     1n   options(*nopass) value

      **************************************************************************
      * Prototype for xlatwCCSIDs (translate with CCSIDs) subprocedure
      **************************************************************************
      * Translates input data to output data using CCSIDs.
      * If optional parameters 3 and 4 are specified, their CCSIDs are used.
      * Otherwise, the CCSIDS contained in
      * environment variables CGI_EBCDIC_CCSID and CGI_ASCII_CCSID are used
      * and the direction of translation is as specified by parameter two
      * (*on = to EBCDIC, *off = from EBCDIC).
     D xlatwCCSIDs     pr         32767    varying
     D  toebcdic                      1n   value
     D  input                     32767    value varying
     D  fromCCSID                    10u 0 value options(*nopass)
     D  toCCSID                      10u 0 value options(*nopass)

      ***********************************************************************
      * Prototype for ZhbGetInput subprocedure
      ***********************************************************************
      * Uses QzhbCgiParse to prepare internal arrays to be used
      * by subsequent calls to the ZhbGetVar, ZhbGetVarUpper,
      * and ZhbGetVarCnt subprocedures.

      * QzhbCgiParse requires that the CGI_MODE environment variable
      * be a valid EBCDIC value.
      * If this condition is not met, a message is forced into the
      * debugging file and the program continues until it fails.

      * This procedure should not be called more than once for each set
      * of input coming from the browser.

      * If REQUEST_METHOD is GET, saved query string is set to blanks.

      * If REQUEST_METHOD is POST, the original QUERY_STRING is returned as
      * SavedQueryString and QUERY_STRING is changed to null.

      * This procedure returns a count of the number of variables received.
      * Note: a variable with multiple instances counts as one variable.

      * Examine the QUSEC data structure for any errors.

     D ZhbGetInput     pr            10i 0
     D  SavedQueryString...
     D                            32767    varying options(*varsize)
     D  qusec                       516    options(*varsize)                    Error structure

      ***********************************************************************
      * Prototype for ZhbGetVar subprocedure
      ***********************************************************************
      * Returns the value of a CGI input variable.
      * If occurrence is ommitted, the first occurence is returned.
      * If the occurrence does not exist, a null string is returned.

     D ZhbGetVar       pr         32767    varying
     D  varname                      50    const varying options(*varsize)
     D  occurrence                   10i 0 const options(*nopass)

      ***********************************************************************
      * Prototype for ZhbGetVarUpper subprocedure
      ***********************************************************************
      * Returns the value of a CGI input variable with lower case
      * characters converted to upper case.
      * If occurrence is ommitted, the first occurence is returned.
      * If the occurrence does not exist, a null string is returned.

     D ZhbGetVarUpper  pr         32767    varying
     D  varnamein                    50    const varying options(*varsize)
     D  occurrence                   10i 0 const options(*nopass)

      ***********************************************************************
      * Prototype for ZhbGetVarCnt subprocedure
      ***********************************************************************
      * Returns number of occurences of a variable in the CGI input.

     D ZhbGetVarCnt    pr            10i 0
     D  varnamein                    50    const varying options(*varsize)

      ***********************************************************************
      * Prototype for ZhbCountAllVars subprocedure
      ***********************************************************************
      * Returns number of occurences of all variables in the CGI input.
      * ZhbGetInput must have been run before calling this subprocedure.

     D ZhbCountAll...
     D Vars            pr            10i 0

      ***********************************************************************
      * Prototype for ZhbGetVarDetails subprocedure
      ***********************************************************************
      * Returns detailed information for the nth variable of all variables
      * counted by ZhbCountAllVars

      * ZhbGetInput must have been run before calling this subprocedure.
      * If variables are requested in ordinal sequence, they will be
      * returned in name, name's occurrence sequence.  The names will
      * be in upper case.

      * Parameters
      *  ThisOccur      Input    Ordinal value of variable to return
      *  ThisVarName    Output   The variable's name
      *  ThisVarOccur   Output   Occurrence within this variable
      *  FoundInd       Output   *on = found; *off = not found
      *
      * Return value: the variable's value

     D ZhbGetVar...
     D Details         pr         32767    varying
     D  ThisOccur                    10i 0 const
     D  ThisVarName                  50
     D  ThisVarOccur                 10i 0
     D  FoundInd                       n

      *****************************************************************
      * IFS APIs' prototypes and constants
      *****************************************************************

      *****************************************************************
      * IFS prototypes
      *****************************************************************
      * Get status
     D stat            pr            10i 0 extproc('stat')
     D  filename                       *   value options(*string)               null terminated str
     D  statStruct                     *   value
     D***  Begin Add  ***  09/12/11  **************************************
     D stat64          pr            10i 0 extproc('stat64')
     D  filename                       *   value options(*string)               null terminated str
     D  statStruct                     *   value
     D***  Begin Add  ***  09/12/11  **************************************

      * Open
     D open            pr            10i 0 extproc('open')
     D  filename                       *   value options(*string)               null terminated str
     D  openflags                    10i 0 value
     D  mode                         10u 0 value options(*nopass)
     D  codepage                     10u 0 value options(*nopass)

      * Read
     D read            pr            10i 0 extproc('read')
     D  filehandle                   10i 0 value
     D  datareceived                   *   value
     D  nbytes                       10u 0 value

      * Write
     D write           pr            10i 0 extproc('write')
     D  filehandle                   10i 0 value
     D  datatowrite                    *   value
     D  nbytes                       10u 0 value

      * Close
     D close           pr            10i 0 extproc('close')
     D  filehandle                   10i 0 value

      * Link (creates a hard link)
     D link            pr            10i 0 extproc('link')
     D  filepath                       *   value options(*string)
     D  newlink                        *   value options(*string)

      * Unlink (unlinks an IFS file)
     D unlink          pr            10i 0 extproc('unlink')
     D  filepath                       *   value options(*string)

     D*****************************************************************
     D* IFS constants
     D*****************************************************************
      * File Access Modes for open()
     D O_RDONLY        S             10i 0 inz(1)
     D O_WRONLY        S             10i 0 inz(2)
     D O_RDWR          S             10i 0 inz(4)

      * oflag values for open()
     D O_CREAT         S             10i 0 inz(8)
     D O_EXCL          S             10i 0 inz(16)
     D O_TRUNC         S             10i 0 inz(64)

      * File Status Flags for open() and fcntl()
     D O_NONBLOCK      S             10i 0 inz(128)
     D O_APPEND        S             10i 0 inz(256)

      * oflag Share Mode values for open()
     D O_SHARE_RDONLY  S             10i 0 inz(65536)
     D O_SHARE_WRONLY  S             10i 0 inz(131072)
     D O_SHARE_RDWR    S             10i 0 inz(262144)
     D O_SHARE_NONE    S             10i 0 inz(524288)

      * File permissions
     D S_IRUSR         S             10i 0 inz(256)                             Read for owner
     D S_IWUSR         S             10i 0 inz(128)                             Write for owner
     D S_IXUSR         S             10i 0 inz(64)                              Execute and Search f
     D S_IRWXU         S             10i 0 inz(448)                             Read, Write, Execute
     D S_IRGRP         S             10i 0 inz(32)                              Read for group
     D S_IWGRP         S             10i 0 inz(16)                              Write for group
     D S_IXGRP         S             10i 0 inz(8)                               Execute and Search f
     D S_IRWXG         S             10i 0 inz(56)                              Read, Write, Execute
     D S_IROTH         S             10i 0 inz(4)                               Read for other
     D S_IWOTH         S             10i 0 inz(2)                               Write for other
     D S_IXOTH         S             10i 0 inz(1)                               Execute and Search f
     D S_IRWXO         S             10i 0 inz(7)                               Read, Write, Execute

      * Misc
     D O_TEXTDATA      S             10i 0 inz(16777216)                        text data flag
     D O_CODEPAGE      S             10i 0 inz(8388608)                         code page flag
     D O_INHERITMODE   S             10i 0 inz(134217728)                       inherit mode flag
