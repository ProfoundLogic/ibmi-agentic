     h*copyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h* StoneRiver is a trademark of StoneRiver, Inc.')
      *---------------------------------------------------------------------
      *
      *  Description:  This copybook will be used to declare the procedure
      *  Interface.
      *
      *  Be sure to include BNDDIR(YAJL) when using these copybooks.
      *
      *  Created on 09/04/2014 by C:279 for Project: 114WOE
      *---------------------------------------------------------------------
      * Revisions:
      *
      *---------------------------------------------------------------------
      * Procedure name: JSON_Start
      * Purpose:        Start JSON formating
      * Returns:
      *-------------------------------------------------------------------------
     p JSON_Start      b
     d JSON_Start      pi
      /free

         yajl_genOpen(*Off);  // use *on for easier to read JSON
         yajl_beginObj();

      /end-free
     p JSON_Start      e

      *-------------------------------------------------------------------------
      * Procedure name: JSON_Send
      * Purpose:        Resolve and send the JSON string
      * Returns:
      *-------------------------------------------------------------------------
     p JSON_Send       b
     d JSON_Send       pi
     d JSON            s          32767A   varying
      /free

       charVar = 'Content-Type: text/plain' + CRLF + CRLF;
       length = %len(%trim(charvar));
       QtmhWrStout( charVar : length: err );

       yajl_copyBuf( CCSID
                   : %addr(bigCharVar)
                   : %size(bigCharVar)
                   : length );
       yajl_genClose();

       QtmhWrStout( bigCharVar : length: err );

      /end-free
     p JSON_Send       e
      *-------------------------------------------------------------------------
      * Procedure name: JSON_Finish
      * Purpose:        Finish the JSON string
      * Returns:
      *-------------------------------------------------------------------------
     p JSON_Finish     b
     d JSON_Finish     pi
      /free

       yajl_endArray();

      /end-free
     p JSON_Finish     e

      *-------------------------------------------------------------------------
      * Procedure name: isValidParms
      * Purpose:        Validate minimum values passed
      * Returns:
      *-------------------------------------------------------------------------
     p isValidParms    b
     d isValidParms    pi              n

      /free

       // If the file RRN is not passed
       If intRrn = 0;
          yajl_addChar('RtnCode' : 'X');
          yajl_addChar('errorMessage' :'No file record information provided');
          return *off;
       Endif;

       return *on;

      /end-free
     p isValidParms    e

      *----------------------------------------------------------------
      * Procedure name: SetLibraryList
      * Purpose:        Set the jobs labrary list
      * Returns:
      *----------------------------------------------------------------
     p SetLibraryList  b
     d SetLibraryList  pi
     d inCmd           s           2750    inz
     d strUserLibl     s           2750A   varying

      /free

       // SetLibl - Set library list for AJAX call
       // Get job libl and set
       strUserLibl = RtvUserLibl(strJobName:strJobUser:strJobNumber);

       If strUserLibl = 'X' or strUserLibl = ' ';
          //Error default to LIV libl
          SetInitLibl('LIV':' ':' ':' ':' ':' ':' ':' ');
       Else;
          inCmd = 'CHGLIBL LIBL(' + %Trim(strUserLibl) + ')';
          RunCommand(inCmd:%len(inCmd));
       Endif;

      /end-free
     p SetLibraryList  e
