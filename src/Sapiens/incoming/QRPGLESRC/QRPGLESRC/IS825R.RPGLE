     *=====================================================================
     *  (c) Copyright Progressive Data Solutions, Inc., 2000
     *=====================================================================
     H/TITLE IS825R - GetPathStr
     *=====================================================================
     *
     * Description:
     *  Get the path and document id from a string passed in.
     *
     * Created   : 07/03/00
     * Programmer: C:113
     * Project   : DR113
     *
     *=====================================================================
     * Revisions:
     * C:002
      * Date: 10/05/04
      *                 Change to fix issue with UNC path when path is > QDLS
     * C:002
      * Date: 07/28/04
      *                 Change to account for UNC paths, not just Mapped Drv
     *=====================================================================
    
     *=====================================================================
     * Procedure Prototypes
     *=====================================================================
      /copy IS825RPR

     *=====================================================================
     * Table, Data Structure & Array Definitions
     *=====================================================================


     *=====================================================================
     * Stand Alone Field Definitions
     *=====================================================================
     D @String         s             80a
     D @Path           s             80a
     D @DocId          s             12a


     *=====================================================================
     * PList Definitions
     *=====================================================================
     ** ENTRY PARAMETERS
     C     *entry        plist
     C                   parm                    @String
     C                   parm                    @Path
     C                   parm                    @DocId


     *=====================================================================
     * Main
     *=====================================================================
     c                   callp     GetPathStr(@String : @Path : @DocId )

     ** End main
     c                   eval      *inlr = *ON
     *=====================================================================




     *=====================================================================
     * GetDocIdx - Get the document index information
     *=====================================================================
     p GetPathStr      B                   export

     *** Procedure Interface ***
     d GetPathStr      PI
     d  pString                      80a
     d  pPath                        80a
     d  pDocId                       12a

     *** Locals ***
     d  tempString     s             80a
     d  pos            s              3  0
     d  svpos          s              3  0
      *** add 07/28/04 *** 02 *** *****************************
     d  unc            s              2a
     d  uncflg         s              1a

     ***  If using UNC type path, must perform different operations to extract the proper data ***
     c                   eval      uncflg = ' '
     c                   movel     pstring       unc
     c     unc           ifeq      '\\'
     c                   movel     'Y'           uncflg
     c                   else
     c                   movel     ' '           uncflg
     c                   endif
      *
     c     uncflg        ifeq      'Y'                                           UNC type path

     *** Procedure Definition ***
      *temp sting = string starting after the '\\'
     c                   eval      tempString = %subst(pString:pos+3)

     c                   eval      pos = %scan('\' : tempString)
     *** add next line 10/05/04 **********************************
     c                   eval      pos = %scan('\' : tempString : pos+1)
     ** tempstring is data after the single '\' after the unc system name & file sys (QDLS/or qdls..
     c*** del 10/05/04** eval      tempString = %subst(tempString:pos+6)
     *** add/chg  next line 10/05/04 **********************************
     c                   eval      tempString = %subst(tempString:pos+1)

     c*** add    10/05/04 ******************************
     c                   eval      pos = 1
     c*** end add 10/05/04 ******************************
     c                   dow       pos > 0
     c                   eval      svpos = pos
     c                   eval      pos = %scan('\' : tempString : svpos+1)
     c                   enddo

     c                   eval      pDocId = %subst(tempString:svpos+1)
     c                   eval      pPath = %subst(tempString:1:svpos-1)

     c                   else
     *** end add *** 07/28/04 ****************************************************

     *** Procedure Definition ***
     c                   eval      pos = %scan('\' : pString)
     c                   eval      tempString = %subst(pString:pos+1)

     c                   eval      pos = %scan('\' : tempString)
     c                   dow       pos > 0
     c                   eval      svpos = pos
     c                   eval      pos = %scan('\' : tempString : svpos+1)
     c                   enddo

     c                   eval      pDocId = %subst(tempString:svpos+1)
     c                   eval      pPath = %subst(tempString:1:svpos-1)

     *** Add ******* 07/28/04 ****************************************************
     c                   endif
     **** now make sure pPath contains the '/' character not the '\' character.
     c                   eval      pos = 1
     c                   dow       pos > 0
     c                   eval      pos = %scan('\' : pPath : 1)
     c                   if        pos <> 0
     c                   eval      %subst(pPath: pos: 1) = '/'
     c                   endif
     c                   enddo
      ** end add *** 07/28/04 ****************************************************
     c                   return

     p GetPathStr      E




