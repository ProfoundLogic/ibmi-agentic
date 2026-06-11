     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H nomain
     *===============================================================================
     *
     * SPIGENERR - Procedures for generic application error
     *
     * Created   : 12/14/12
     * Programmer: C:279
     * Project   : WC2115
     *
     *===============================================================================
      * REVISIONS:                                                                                 *
     *
      * 03/02/23 C:79  Project: 146WKK
      *                Fix Check_Email issues                               .
     *
      * 07/21/16 C:65  Project: SY371
      *                Add Valid_Date, Valid_Document, Valid_Path procedures.
     *                Valid_Date is a copy of the procedure that exists in
     *                WPIPOLERR.  Valid_Document and Valid_Path procedures
     *                are used to validate scriptura document names and
     *                directory paths.  Add CheckEmailList procedure to
     *                verify a list of emails is valid.  Amend Check_Email to
      *                ensure email addresses do not contain commas or semicolons.
      *
      * 04/07/16 C:33  Project: Elevate
      *                E-Mail addrees can not end in a perios.
      *                                                                                            *
      * 10/11/14 C:279 Support: OP136 86962
      *                Added E-Mail addrees format valadation procedure.
     *===============================================================================

      * File
     f*** begin add ***  07/21/16  ******************************************
     fstgtl     if   e           k disk    rename(pgt01:pgt00)
     f*** end add *****  07/21/16  ******************************************

     d/copy sprgenerr

     d psds          esds                  extname(szq1p)

      *=====================================================================
      * Check_Phone# - Check to verify If Phone # has length 10.
      *  Parameter 1 - Phone #
      *  Parameter 2 - Type (Phone = 'P', Cell = 'C', Fax = 'F')
      *  Parameter 3 - Message
      *=====================================================================
     p Check_Phone#    b                   EXPORT
     d Check_Phone#    PI              n
     d   inPhone#                    10  0 Const
     d   inType                       1
     d   inMessage                  150

      /free

       // Determine if KeyCode is Valid
       If inPhone# > 0 and inPhone# < 1000000000;
          If inType = 'P';
             inMessage = 'Phone # must have 10 digits.';
          Elseif inType = 'C';
             inMessage = 'Cell # must have 10 digits.';
          Elseif inType = 'F';
             inMessage = 'Fax # must have 10 digits.';
          Endif;
          return *on;
       Endif;

       Return *off;

      /end-free

     p Check_Phone#    e

      *=====================================================================
      * Check_Email  - Check to verify If Email address format is valid
      *  Parameter 1 - Email address
      *  Parameter 2 - Message
      *=====================================================================
     p Check_Email     b                   Export
     d Check_Email     pi              n
     d  inEmail                      60
     d  inMessage                   200

     d  intPos         s              3  0
     d  strPos         s              3  0
      /free

       // Determine if Email address is valid
       If %scan(' ':%trim(inEmail)) > 0;
          //*** Beg Chg *** 03/02/23 ***************************************
          //***inMessage = 'Email address should not consist spaces.';
          inMessage = 'Email address should not consist of spaces.';
          //*** End Chg *** 03/02/23 ***************************************
          return *on;
       Else;
          If %scan('@':%trim(inEmail)) = 1;
             inMessage = 'Email address should not start with symbol @.';
             return *on;
          ElseIf %scan('@':%trim(inEmail)) = 0;
             inMessage = 'Email address should consist of symbol @.';
             return *on;
          ElseIf %subst(%triml(inEmail):%scan('@':%trim(inEmail))+1:1) = ' ';
             inMessage = 'Email address should not end with symbol @.';
             return *on;
          ElseIf %scan('.':%trim(inEmail)) = 1;
             inMessage = 'Email address should not start with period.';
             return *on;
          ElseIf %scan('.':%trim(inEmail)) = 0;
             inMessage = 'Email address should consist of period.';
             return *on;
          //***  Begin Add  ***  04/07/16  *******************************
          ElseIf %subst(%triml(inEmail):%scan('.':%trim(inEmail))+1:1) = ' ';
             inMessage = 'Email address should not end with period.';
             return *on;
          //***  End   Add  ***  04/07/16  *******************************
          ElseIf %scan('.':%trim(inEmail):%scan('.':%trim(inEmail)) + 1) =
           %scan('.':%trim(inEmail)) + 1;
             inMessage = 'Email address should not consist two or more +
              consecutive periods.';
             return *on;
          ElseIf (%scan('@':%trim(inEmail):%scan('.':%trim(inEmail)) + 1) =
           %scan('.':%trim(inEmail)) + 1) or
           (%scan('.':%trim(inEmail):%scan('@':%trim(inEmail)) + 1) =
           %scan('@':%trim(inEmail)) + 1);
             inMessage = 'Email address should not consist cosecutive symbol @ +
              and period.';
             return *on;
          //***  begin add  ***  07/29/16  *******************************
          ElseIf %scan(',':%trim(inEmail)) <> 0;
             inMessage = 'Email address should not contain a comma.';
             return *on;
          ElseIf %scan(';':%trim(inEmail)) <> 0;
             inMessage = 'Email address should not contain a semicolon.';
             return *on;
          //***  end add ******  07/29/16  *******************************
          Else;
             intPos = 0;
             strPos = 1;
             Dow %scan('.':%triml(inEmail):strPos) > 0;
                 intPos = %scan('.':%trim(inEmail):strPos);
                 strPos = intPos + 1;
             Enddo;
             If %scan('@':%trim(inEmail)) > intPos;
                inMessage = 'Email address should have @ symbol prior to +
                 last position of period.';
                return *on;
             ElseIf %subst(%triml(inEmail):strPos:1) = ' ';
                inMessage = 'Email address should not end with period.';
                return *on;
             Endif;
          Endif;
       Endif;

       Return *off;

      /end-free

     p Check_Email     e
      *** begin add ***  07/21/16  *********************************************
       //===================================================================
       // CheckEmailList - Validate the list of emails passed in.
       //
       //===================================================================
     P CheckEmailList  B                   export
     D CheckEmailList  PI              n
     D string                     32766    options(*varsize) const
     D message                      200
     D*
     D strList         s                   like(string)
     d strEmail        s             60
     d strMessage      s            200
     d index           s              5  0
     d index1          s              5  0
     d x               s              5  0

      /free

       Clear message;
       strList = string;
       x = 1;

       // Email list cannot be blank.
       If strList = ' ';
          message = 'At least one email address must be entered.';
          Return *on;
       Endif;

       DoU strList = ' ';

          If x > 3000;
             message = 'There was a parsing error on field; please '+
              'correct the list of emails addresses.';
             Return *on;
          Endif;

          Clear strEmail;

          index = %scan(';':strList);
          index1 = %scan(',':strList);

          Select;
          When index = 0;
             index = index1;
          When index1 < index and index1 <> 0;
             index = index1;
          Endsl;

          Select;
          When index = 0;
             strEmail = %trim(strList);
             strList = ' ';
          When index = 1;
             message = 'A comma or a semicolon is not preceded by a '+
              'valid email address.';
             Return *on;
          Other;
             strEmail = %trim(%subst(strList:1:index-1));
             strList = %trim(%subst(strList:index+1));
          Endsl;

          If Check_Email(strEmail:strMessage);
             message = %trim(strMessage) + ' ' + %trim(strEmail);
             Return *on;
          Endif;

          x = x + 1;

       Enddo;

       Return *off;

      /end-free
     P CheckEmailList  E
       //===================================================================
       // Valid_Date - Date passed in is valid and return formatted. If date passed
       //              in is not valid then value passed in is returned.
       //===================================================================
     P Valid_Date      B                   export
     D Valid_Date      PI             1
     D date                           8  0
     D messageID                    100
     C*
     C     Datchk        Plist
     C                   Parm                    Srdti             8 0
     C                   Parm      *BLANKS       Srffmt            8
     C                   Parm      *BLANKS       Srtfmt            8
     C                   Parm                    Srdto            10
     C                   Parm                    Srerr             1
     C*
     C                   Clear                   messageID
     C*
     C*  EDIT DATE
     C*
     C                   Z-ADD     date          SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *ON
     C                   movel(p)  'WCR0500'     messageID
     C                   return    *ON
     C                   ELSE
     C                   MOVEL     SRDTO         date
     C                   return    *OFF
     C                   ENDIF                                                  SRERR=*ON
     C*
     P Valid_Date      E

       //===================================================================
       // Valid_Document - Validate the name of the Scriptura document passed in.
       //
       //     The type parameter should have a value of 'N' for non-interactive
       //     documents or 'I' for interactive documents.
       //
       //===================================================================
     P Valid_Document  B                   export
     D Valid_Document  PI             1
     D co#                            3  0 const
     D type                           1    const
     D document                      60    const
     D message                      100
     D*
     D xxTbid          s                   like(GTTbid)
     D xxKey           s                   like(GTKey)
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ')
     D LO              C                   CONST('abcdefghijklmnopqrst-
     D                                     uvwxyz')
     C*
     C* Generic Table File Key List
     C*
     C     keySTGTL      Klist
     C                   Kfld                    co#
     C                   Kfld                    xxTbid
     C                   Kfld                    xxKey
      /free

       Clear message;

       // Document ID must be entered.
       If document = ' ';
          message = 'Template ID/Live Doc ID must be entered.';
          Return *on;
       Else;

       // Document ID must have a valid file extension.
          If type = 'N';
             xxTBID = 'FILN';
          Else;
             xxTBID = 'FILI';
          Endif;

          If %scan('.':document) = 0;
             message = 'Template ID/Live Doc ID must have a valid file ' +
              'extension.';
             Return *on;
          Else;

             xxKey = %xlate(lo:up:%trim(%subst(document:%scan('.':document):
              10)));
             Chain(n) keySTGTL pgt00;
             If not %found;
                message = 'Template ID/Live Doc ID must have a valid file ' +
                 'extension.';
                Return *on;
             Endif;

          Endif;

       Endif;

       Return *off;

      /end-free
     P Valid_Document  E

       //===================================================================
       // Valid_Path - Validate the document path passed in.
       //
       //===================================================================
     P Valid_Path      B                   export
     D Valid_Path      PI             1
     D path                         250    const
     D message                      100
     D*
     d strSlash        s              1
     d strSlash2       s              1

      /free

       Clear message;

       // Document path must be entered.
       If path = ' ';
          message = 'Document path must be entered.';
          Return *on;
       Else;

       // Document path must begin and end with a '/' character.
          strSlash  = %subst(path:1:1);
          strSlash2 = %subst(path:%len(%trim(path)):1);

          If strSlash <> '/' or strSlash2 <> '/';
             message = 'Document path must begin and end with a ''/''.';
             Return *on;
          Endif;

       Endif;

       Return *off;

      /end-free
     P Valid_Path      E

      *** end add *****  07/21/16  *********************************************
