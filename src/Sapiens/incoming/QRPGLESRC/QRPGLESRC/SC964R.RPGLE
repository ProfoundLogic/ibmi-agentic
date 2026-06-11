      *========================================================================
     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *========================================================================
    H/TITLE SC964R - Print ALl REmarks img print - get document information
      *========================================================================
      * Description: This program gets the information for printing an image
      *              document.
      *
      * Created    : 04/03/00
      * Programmer : C:113
      *========================================================================
      * Created on 07/10/2003 by c:62 for Project 129WAO
      *========================================================================
     * Revisions:
     *
      *  08/01/14 C:62  Support IM045 86020
      *                 Expand document id to accomodate SMMDP/MDOID
     *
      *  08/13/13 C:33  Redmine: 3556
      *                 Get UNCN & IFSP from SDCMP, if needed.
     *
      *  05/25/12 C:062 Project: DR132 79996
      *                 Add PDF processing.
     *
      *  10/17/11 C:279 Project: SY195
      *                 Modify program to pass/return UNC path field
      *                 and IFS flag to called/calling programs.
      *
      *========================================================================
     Fimpsl4    if   e           k disk    rename(pps01:pps03)
     Fimdol1    uf a e           k disk    rename(pdo01:pdo01#)
     Fimdtl     if   e           k disk
     Fsmmdl     if   e           k disk
     Fsmoll3    if   e           k disk
     F*** Begin Add *** 05/25/12 *******************************************
     Fsmdll2    if   e           k disk
     F*** End   Add *** 05/25/12 *******************************************
     F***  Begin Add  ***  08/13/13  **************************************
     Fsdcmla    if   e           k disk    rename(pcm01:pcm0a)
     F***  End   Add  ***  08/13/13  **************************************

     *=========================================================================
      * Prototypes
     *=========================================================================
      /copy SC964RPR

     *=========================================================================
      * Variable Definitions
     *=========================================================================
      * External program information structure
     D psds          esds                  extname(szq1p)

     D*** Begin Chg *** 08/01/14 **********************************
     D*eDocid***       s             12a
     D eDocid          s            100a
     D*** End   Chg *** 08/01/14 **********************************
     D eFolder         s             80a
     D eKey            s             40a
     D eSequence       s              5a
     D eJobName        s             10a
     D*** Begin Chg *** 08/01/14 ************************************
     D*eDocid2**       s             12a
     D eDocid2         s            100a
     D*** End   Chg *** 08/01/14 ************************************
     D eFolder2        s             80a
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
     D eImgFile        s              1a
     D Docid           s             40a
     D folderfix       s              1a   dim(80)
     D*** Begin Add *** 05/25/12 *******************************************
     D mainDoc         s               n   inz(*off)
     D*** End   Add *** 05/25/12 *******************************************
     D*** Begin Add *** 08/01/14 **********************************
     D documentID      s             12a
     D*** End   Add *** 08/01/14 **********************************
    
     *=========================================================================
      * KList Definitions
     *=========================================================================
     C     pskey3        klist
     C                   kfld                    epsid

     C     doky          klist
     C*** Begin Chg *** 08/01/14 **********************************
     C**********         kfld                    eDocid
     C                   kfld                    documentID
     C*** End   Chg *** 08/01/14 **********************************

     C     dtky          klist
     C                   kfld                    eDocType

     C     mdky          klist
     C                   kfld                    eKey
     C                   kfld                    Sequence
     C                   kfld                    Docid
     C*** Begin Add *** 05/25/12 *******************************************
     C     dlky          klist
     C                   kfld                    eKey
     C                   kfld                    Sequence
     C*** End   Add *** 05/25/12 *******************************************
     C*
     C***  Begin Add  ***  08/13/13  ***************************************
     C     keySDCMLA     klist
     C                   kfld                    eKey
     C                   kfld                    Sequence
     C***  End   Add  ***  08/13/13  ***************************************

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
     C                   parm                    eImgFile
     C*** BEG ADD *** 10/17/11 ****************************************
     C                   parm                    euncn           255
     C                   parm                    eifsp             1
     C*** END ADD *** 10/17/11 ****************************************

     *=========================================================================
      * Main
     *=========================================================================

     C                   move      eSequence     sequence
     C                   movel     eDocid        Docid
     C*** Begin Add *** 08/01/14 **********************************
     C                   movel     eDocid        documentID
     C*** End   Add *** 08/01/14 **********************************
     C*** Begin Add *** 05/25/12 *******************************************
     C                   eval      mainDoc=*Off
     C*** End   Add *** 05/25/12 *******************************************

      * Get workstation information
     C     pskey3        chain     pps03                              60

     C                   if        NOT *in60
     C                   eval      eDataq    = pspque
     C                   eval      eDataqlib = pspqlb
     C                   eval      eDrive    = psidrv
     C                   endif

     C                   if        eImgFile = 'N'
     C                             OR
     C                             (eImgFile = 'O')
      * Get multiple document information
     C     mdKy          chain     pmd01                              60
     C                   if        NOT *in60
     C                   eval      eDocid2    = %trim(mdoid)
     C                   eval      eFolder2   = %trim(mdoflr)
     C                   eval      eDocType   = mdotyp
     C*** BEG ADD *** 10/17/11 ****************************************
     C                   eval      euncn      = mduncp
     C                   eval      eifsp      = mduncf
     C*** END ADD *** 10/17/11 ****************************************

     C                   else

     C                   if        eImgFile <> 'O'
     C                   eval      eRtnCode = '2'
     C                   else
     C                   eval      eDocid2    = eDocid
     C                   eval      eFolder2   = eFolder
      /Free
       //***  Begin Add  ***  08/13/13  ********************************
       Clear CMdcId;
       Clear CMiDoc;
       Clear CMfold;
       Clear CMimgF;
       Clear CMuncN;
       Clear CMifsP;
       Chain keySDCMLa pcm0a;
       If (eDocId = CMdcId or
        (eDocId = CMiDoc and (CMimgF = 'Y' or CMimgF = 'F' or CMimgF = 'I')))
        and CMfold = eFolder;
          eUncN = CMuncN;
          eIfsP = CMifsP;
       Endif;
       //***  Begin Add  ***  08/13/13  ********************************
      /End-free
     C                   endif

     C                   endif

      * Get multiple document optical information
     C     mdky          chain     pol01                              60
     C                   if        NOT *in60
     C                   eval      eOptical = oloptf
     C                   else
     C*** Begin Add *** 05/25/12 *******************************************
     C     dlky          chain     pdl01                              60
     C                   if        NOT *in60
     C                   eval      eOptical = dloptf
     C                   eval      mainDoc=*On
     C                   else
     C*** End   Add *** 05/25/12 *******************************************
     C                   eval      eOptical = *blanks
     C*** Begin Add *** 05/25/12 *******************************************
     C                   endif
     C*** End   Add *** 05/25/12 *******************************************
     C                   endif

     C                   else
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
     C*** Begin Add *** 05/25/12 *******************************************
     C                   if        mainDoc=*On
     C*** Begin Chg *** 08/01/14 ************************************************
     C**********         callp     GetOptical2(eKey:eSequence:eDocid:
     C**********                              eFolder:eRtnCode)
     C                   callp     GetOptical2(eKey:eSequence:documentID:
     C                                        eFolder:eRtnCode)
     C*** End   Chg *** 08/01/14 ************************************************
     C                   if        eRtnCode = ' '
     C                   clear                   eSubFolder
     C                   eval      eDocid2  = eDocid
     C                   eval      eFolder2 = eFolder
     C                   else
     C                   eval      eRtnCode = '3'
     C                   endif
     C                   else
     C*** End   Add *** 05/25/12 *******************************************
     C*** Begin Chg *** 08/01/14 ************************************************
     C**********         callp     GetOptical(eKey:eSequence:eDocid:
     C**********                              eFolder:eRtnCode)
     C                   callp     GetOptical(eKey:eSequence:documentID:
     C                                        eFolder:eRtnCode)
     C*** End   Chg *** 08/01/14 ************************************************
     C                   if        eRtnCode = ' '
     C                   clear                   eSubFolder
     C                   eval      eDocid2  = eDocid
     C                   eval      eFolder2 = eFolder
     C                   else
     C                   eval      eRtnCode = '3'
     C                   endif

     C*** Begin Add *** 05/25/12 *******************************************
     C                   endif
     C*** End   Add *** 05/25/12 *******************************************
     C                   endif

     C                   movea     eFolder2      folderfix
     C     1             do        80            x                 3 0
     C                   if        folderfix(x) = '/'
     C                   eval      folderfix(x) = '\'
     C                   endif
     C                   enddo
     C                   movea     folderfix     eFolder2

     C                   move      *on           *inlr
