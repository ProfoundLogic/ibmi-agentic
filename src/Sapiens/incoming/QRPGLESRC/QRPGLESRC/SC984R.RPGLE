      *========================================================================
     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *========================================================================
    H/TITLE SC984R - Print ALl REmarks img print - get document information
      *========================================================================
      * Description: This program gets the information for printing an image
      *              document.
      *
      * Created    : 04/03/00
      * Programmer : C:113
      * Project    : 87MIW
      *========================================================================
     * Revisions:
     *
      *  10/17/11 C:279 Project: SY195
      *                 Modify program to pass/return UNC path field
      *                 and IFS flag to called/calling programs.
      *
     * 07/01/04 C:062 Proj: sy100
     *                Data Center Process.                                 d
     *
     * 04/07/03 C:128 Proj: 129WAO
     *                Modify to include all documents, OV, attached objectsd
     *                as well as "imported" documents.
     *
     * ----------------------------------------------------------------------
     *  NOTE: Please add comments to the top of revisions.
     * ----------------------------------------------------------------------
      *========================================================================
      *========================================================================
      * File Definitions
      *========================================================================
     Fimpsl4    if   e           k disk    rename(pps01:pps03)
     Fimdol1    uf a e           k disk    rename(pdo01:pdo01#)
     Fimdtl     if   e           k disk
      *** Begin Change 04/07/03 ********************************************
     Fsmmdl     if   e           k disk
     Fsmoll3    if   e           k disk
      *** End   Change 04/07/03 ********************************************

     *=========================================================================
      * Prototypes
     *=========================================================================
      /copy SC984RPR

     *=========================================================================
      * Variable Definitions
     *=========================================================================
      * External program information structure
     D psds          esds                  extname(szq1p)

     D eDocid          s             12a
     D eFolder         s             80a
     D eKey            s             40a
     D eSequence       s              5a
     D eJobName        s             10a
     D eDocid2         s             12a
     D eFolder2        s              8a
     D eSubFolder      s             12a
     D eOptical        s              1a
     D eDrive          s              1a
     D eDocType        s              8a
     D eDocDesc        s             40a
     D eDataq          s             10a
     D eDataqLib       s             10a
     D epsid           s              2a
     D eRtnCode        s              1a
     D sequence        s              5  0
      *** Begin Change 04/07/03 ********************************************
     D eImgFile        s              1a
     D Docid           s             40a
      *** End   Change 04/07/03 ********************************************
      *** BEGIN ADD *** 07/01/04 *******************************************
     D ##prfx          s              3a
     D ##pflr          s             80a
     D pdsfldext       s              7a
     D folder3         s             52a
     D folder4         s             72a
      *** END   ADD *** 07/01/04 *******************************************
    
     *=========================================================================
      * KList Definitions
     *=========================================================================
     C     pskey3        klist
     C                   kfld                    epsid

     C     doky          klist
     C                   kfld                    eDocid

     C     dtky          klist
     C                   kfld                    eDocType
      *** Begin Change 04/07/03 ********************************************
     C     mdky          klist
     C                   kfld                    eKey
     C                   kfld                    Sequence
     C                   kfld                    Docid
      *** End   Change 04/07/03 ********************************************

     *=========================================================================
      * PLIST Definitions
     *=========================================================================
     C     *entry        plist
     C                   parm                    eDocid
     C                   parm                    eFolder
     C                   parm                    eKey
     C                   parm                    eSequence
     C                   parm                    eJobName
     C                   parm                    eDocid2
     C                   parm                    eFolder2
     C                   parm                    eSubFolder
     C                   parm                    eOptical
     C                   parm                    eDrive
     C                   parm                    eDocType
     C                   parm                    eDocDesc
     C                   parm                    eDataq
     C                   parm                    eDataqLib
     C                   parm                    epsid
     C                   parm                    eRtnCode
      *** Begin Change 04/07/03 ********************************************
     C                   parm                    eImgFile
      *** End   Change 04/07/03 ********************************************
     C*** BEG ADD *** 10/17/11 ****************************************
     C                   parm                    euncn           255
     C                   parm                    eifsp             1
     C*** END ADD *** 10/17/11 ****************************************

     *=========================================================================
      * Main
     *=========================================================================
      *** BEGIN ADD *** 07/01/04 *******************************************
     C                   call      'SA030R'
     C                   parm      *blanks       ##prfx
     C                   parm      *blanks       ##pflr
      *** END   ADD *** 07/01/04 *******************************************

     C                   move      eSequence     sequence
     C                   movel     eDocid        Docid

      * Get workstation information
     C     pskey3        chain     pps03                              60

     C                   if        NOT *in60
     C                   eval      eDataq    = pspque
     C                   eval      eDataqlib = pspqlb
     C                   eval      eDrive    = psidrv
     C                   endif

      *** Begin Change 04/07/03 ********************************************
     C                   if        eImgFile = 'N'
     C                             OR
     C                             (eImgFile = 'O')
      * Get multiple document information
     C     mdKy          chain     pmd01                              60
     C                   if        NOT *in60
     C                   eval      eDocid2    = mdoid
     C*** BEG ADD *** 10/17/11 ****************************************
     C                   eval      euncn      = mduncp
     C                   eval      eifsp      = mduncf
     C*** END ADD *** 10/17/11 ****************************************
      *** BEGIN ADD *** 07/01/04 *******************************************
     C                   if        ##pflr <> *blanks
     C                   movel(p)  mdoflr        pdsfldext
     C                   if        ##pflr = pdsfldext
     C                   move      mdoflr        folder3
     C                   eval      eFolder2=folder3
     C                   else
     C                   eval      eFolder2   = mdoflr
     C                   endif
     C                   else
      *** END   ADD *** 07/01/04 *******************************************
     C                   eval      eFolder2   = mdoflr
      *** BEGIN ADD *** 07/01/04 *******************************************
     C                   endif
      *** END   ADD *** 07/01/04 *******************************************
     C                   eval      eDocType   = mdotyp

     C                   else

     C                   if        eImgFile <> 'O'
     C                   eval      eRtnCode = '2'
     C                   else
     C                   eval      eDocid2    = eDocid
      *** BEGIN ADD *** 07/01/04 *******************************************
     C                   if        ##pflr <> *blanks
     C                   movel(p)  eFolder       pdsfldext
     C                   if        ##pflr = pdsfldext
     C                   move      eFolder       folder4
     C                   eval      eFolder2=folder4
     C                   else
     C                   eval      eFolder2   = eFolder
     C                   endif
     C                   else
      *** END   ADD *** 07/01/04 *******************************************
     C                   eval      eFolder2   = eFolder
      *** BEGIN ADD *** 07/01/04 *******************************************
     C                   endif
      *** END   ADD *** 07/01/04 *******************************************
     C                   endif

     C                   endif

      * Get multiple document optical information
     C     mdky          chain     pol01                              60
     C                   if        NOT *in60
     C                   eval      eOptical = oloptf
     C                   else
     C                   eval      eOptical = *blanks
     C                   endif

     C                   else
      *** End   Change 04/07/03 ********************************************
      * Get image document information
     C     doKy          chain     pdo01#                             60
     C                   if        NOT *in60
     C                   eval      eDocid2    = dodcid
     C                   eval      eFolder2   = doflr
     C                   eval      eSubFolder = dosflr
     C                   eval      eOptical   = dooptf
     C                   eval      eDocType   = dodtyp
     C                   eval      doarqs     = doarqs + 1
     C*** BEG ADD *** 10/17/11 ****************************************
     C                   eval      euncn      = douncn
     C                   eval      eifsp      = doifsp
     C*** END ADD *** 10/17/11 ****************************************

     c                   update    pdo01#

     C                   else
     C                   eval      eRtnCode = '2'
     C                   endif

      * Get document type information
     C     dtky          chain     pdt01                              60
     C                   if        NOT *in60
     C                   eval      eDocDesc = dtdesc
     C                   else
     C                   eval      eDocDesc = *blanks
     C                   endif

     C                   endif

      * get the document if on optical
     C                   if        eOptical <> ' '
     C                   callp     GetOptical(eKey:eSequence:eDocid:
     C                                        eFolder:eRtnCode)
     C                   if        eRtnCode = ' '
     C                   clear                   eSubFolder
     C                   eval      eDocid2  = eDocid
      *** BEGIN ADD *** 07/01/04 *******************************************
     C                   if        ##pflr <> *blanks
     C                   movel(p)  eFolder       pdsfldext
     C                   if        ##pflr = pdsfldext
     C                   move      eFolder       folder4
     C                   eval      eFolder2=folder4
     C                   else
     C                   eval      eFolder2   = eFolder
     C                   endif
     C                   else
      *** END   ADD *** 07/01/04 *******************************************
     C                   eval      eFolder2 = eFolder
      *** BEGIN ADD *** 07/01/04 *******************************************
     C                   endif
      *** END   ADD *** 07/01/04 *******************************************
     C                   else
     C                   eval      eRtnCode = '3'
     C                   endif

     C                   endif

     C                   move      *on           *inlr

