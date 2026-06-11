     h*copyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h* StoneRiver is a trademark of StoneRiver, Inc.')
      *---------------------------------------------------------------------
      *
      *  Description:  This copybook will be used to declare the procedure
      *  Interface.
      *
      *  Be sure to include BNDDIR(YAJL) when using these copybooks.
      *
      *
      *  Created on 09/04/2014 by C:279 for Project: 114WOE
      *---------------------------------------------------------------------
      * Revisions:
      *
      *---------------------------------------------------------------------
      /free

       // Get input from query string
       GetQueryString();

       // start JSON data
       JSON_Start();

       If not isValidParms();
          yajl_endObj();
          JSON_Send();
          Return;
       Endif;

       // set libraray list
       SetLibraryList();

       // process the selections
       Exsr Process;

       // end of JSON data object
       yajl_endObj();

       // sent JSON response
       JSON_Send();

       *inlr = *on;

       return;

      /end-free
