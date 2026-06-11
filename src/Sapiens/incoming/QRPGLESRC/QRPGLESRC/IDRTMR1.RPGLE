     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE IDRTMR1 - VDARTS E-MAIL SERVER ATTACHMENTS BALANCE PROGRAM
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *   This program returns an 8.3 name for the object and the path to      *
     *   store the object.  Also updates the multi-object flag in the DARTS   *
     *   remark.                                                              *
     *                                                                        *
     * CALLED BY : VDARTS E-mail Server Program                               *
     *                                                                        *
     * CALLS  TO : None                                                       *
     *                                                                        *
     * CREATED   : 01/17/2000                                                 *
     * PROGRAMMER: C:113                                                      *
     * PROJECT   : DR086                                                      *
     *========================================================================*
     * REVISIONS:                                                             *
     *                                                                        *
     *                                                                        *
     * ---------------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.                    *
     * ---------------------------------------------------------------------- *
     *========================================================================*
     * FILE USAGE                                                             *
     *                                                                        *
      *  File    Description                    Rcdfmt  Key                    *
     *  IMDIL   Image Document Index Control    PIT01  WSID                   *
     *  IMFBL   Image Folder Balance Control    PFB01  LDT/LDTIM              *
     *  SDCMLA  DARTS Remark File               PCM01  KEY/SEQ#               *
     *  SMMDL   DARTS Multiple-Object File      PMD01                         *
     *                                                                        *
     *========================================================================*
     * INDICATOR USAGE                                                        *
     *                                                                        *
     *  60 - General Chain indicator.                                         *
     *  99 - General Error indicator.                                         *
     *                                                                        *
     *========================================================================*
     * NOTES:                                                                 *
     *                                                                        *
     *  ** IMPORTANT **                                                       *
     *  -This program must be compiled to a target release of V3R7M0 or       *
     *   greater due to the use of the BIF %Scan.                             *
     *                                                                        *
     *========================================================================*
     FIMDIL     UF   E           K DISK
     FIMFBL     UF   E           K DISK
     FSDCMLA    UF   E           K DISK
     FSMMDL     O    E           K DISK

     *========================================================================*
     * Procedure Prototypes
     *========================================================================*
      **************************************************************************
      * CvtDate - Date check conversion                                        *
      **************************************************************************
      * Parms: 8.0 - in date                                                   *
      *        8a  - from format                                               *
      *        8a  - to format                                                 *
      * Returns: 8.0 converted date; 0 if error                                *
      **************************************************************************
      /COPY CVTDATINC

     D FoldBal         PR            30a

     D NameGen         PR            12a
     D  suffix                        3a   Const

     D NewSub          PR             1a
     D  fldr                               Const Like(FBCIF)
     D  subfldr                            Const Like(FBCISF)

     D UpdateDarts     PR             1a
     D  key                          40a   Const
     D  sequence                      5a   Const

     D WriteMD         PR
     D  key                                Const Like(@@Key)
     D  sequence                           Const Like(@@Sequence)
     D  object_in                          Const Like(@@ObjIn)
     D  object_name                        Const Like(@@NameOut)
     D  object_path                        Const Like(@@PathOut)
     D  remark_code                        Const Like(@@RmkCode)

     *========================================================================*
     * Array, Table & Data Structure Definitions
     *========================================================================*
     D psds          ESDS                  ExtName(szq1p)

     *========================================================================*
     * Global Variables
     *========================================================================*
      ** Entry variables
     D @@ObjIn         S             65a
     D @@ScanID        S              2a
     D @@Key           S             40a
     D @@RmkCode       S              5a
     D @@Sequence      S              5a
     D @@NameOut       S             12a
     D @@PathOut       S             30a
     D @@RtnCode       S              1a
     D @@SysNetNm      S             20a

      ** Working variables
     D xxPath          S                   Like(@@PathOut)
     D xxName          S                   Like(@@NameOut)
     D xxsdt           S                   Like(FBLDT) Inz(0)
     D xxstm           S                   Like(FBLTIM) Inz(0)
     D xxTime          S                   Like(FBLTIM) Inz(0)
     D xxDate          S              8  0
     D xxSufx          S              3a
     D xxDot           S              3  0
     D xxDateOut       S             10a

     *========================================================================*
     * PLIST Definitions
     *========================================================================*
      ** Program entry parameters
     C     *entry        PList
     C                   Parm                    @@ObjIn
     C                   Parm                    @@ScanID
     C                   Parm                    @@Key
     C                   Parm                    @@Sequence
     C                   Parm                    @@RmkCode
     C                   Parm                    @@NameOut
     C                   Parm                    @@PathOut
     C                   Parm                    @@RtnCode
     C                   Parm                    @@SysNetNm

     *========================================================================*
     * Main
     *========================================================================*
      ** Get the path to save the object to.
     C                   Eval      xxPath = FoldBal

      ** Generate a name for the object if a path was created successfully.
     C                   If        xxPath <> *blanks
     C                   Eval      xxName = NameGen( xxSufx )
     C                   Else
     C                   Eval      *in99 = *on
     C                   EndIf

      ** If no problems, move the name and path to the return fields
      ** and write the multiple object record.
     C                   If        *in99 = *off
     C                   Eval      @@NameOut = xxName
     C                   Eval      @@PathOut = xxPath

     C                   If        %Parms > 8
     C                   Eval      MDSYSN = @@SysNetNm
     C                   EndIf

     C                   CALLP     WriteMD( @@Key : @@Sequence :
     C                                      @@ObjIn : @@NameOut :
     C                                      @@PathOut : @@RmkCode )

     C                   If        UpdateDARTS( @@Key : @@Sequence ) = '1'
     C                   Eval      @@RtnCode = '1'
     C                   EndIf

     C                   Else
     C                   Eval      @@RtnCode = '1'
     C                   EndIf

      *** Debug *******
     C*    @@NameOut     Dsply
     C*    @@PathOut     Dsply
     C*    @@RtnCode     Dsply
      *** End Debug ***

     C                   Eval      *inLR = *on

     *========================================================================*
     * *inzsr - Initialization subroutine
     *========================================================================*
     C     *inzsr        BEGSR

     C                   Eval      xxDate = *date
     C                   Time                    xxTime
     C                   Eval      *in99 = *off

      ** Get the extension of the incoming object.
     C                   Eval      xxDot = %Scan('.':@@ObjIn:1)
     C                   Eval      xxSufx = %Subst(@@ObjIn:xxDot+1:3)

     C                   ENDSR                                                  *inzsr

     *========================================================================*
     * FoldBal                                                                *
     *========================================================================*
      * Description: Performs the folder balancing process.                    *
      *                                                                        *
     * Parameters : None                                                      *
      *                                                                        *
      * Returns    : None                                                      *
     *========================================================================*
     P FoldBal         B

      *** Procedure Interface ***
     D FoldBal         PI            30a

      *** Locals ***
     D RtnValue        S                   Like(@@PathOut) Inz(*blanks)
     D xxError         S              1a   Inz(*off)
     D xxSFldr         S                   Like(FBCISF)
     D xxFolder        S                   Like(FBCIF) Inz(*blanks)
     D xxSubFldr       S                   Like(FBCISF) Inz(*blanks)

      *** KList Definitions ***
     C     FBkey         KList
     C                   KFld                    xxsdt
     C                   KFld                    xxstm

      *** Procedure Definition ***
     C                   Time                    xxTime
     C     FBkey         SetLL     PFB01
     C                   Read      PFB01                                  60
     C                   If        *in60 = *off
     C                   Eval      FBC#DS = FBC#DS + 1
     C                   Eval      xxSFldr = FBCISF

      ** If we've exceded the number of documents in the folder, create another subfolder.
     C                   If        FBC#DS >= FBM#DS
     C                             AND NewSub(xxFolder:xxSFldr) <> *blanks
     C                   Eval      xxError = *on
     C                   EndIf

     C                   If        xxError = *off
     C                   Eval      xxFolder = FBCIF
     C                   Eval      xxSubFldr = FBCISF
     C                   Eval      xxDateOut = CvtDate(xxDate:*blanks:dtYMD)
     C                   MoveL     xxDateOut     FBLDT
     C                   Eval      FBLTIM = xxTime


     C                   Update    PFB01

     C                   Eval      RtnValue = xxFolder + '\' + xxSubFldr

     C                   EndIf

     C                   EndIf                                                  *in60 = *off

      ** Return the path, or blanks for error.
     C                   Return    RtnValue

     P FoldBal         E

     *========================================================================*
     * NameGen                                                                *
     *========================================================================*
      * Description: Generates an 8.3 name for the object.                     *
      *                                                                        *
     * Parameters : 3a - Suffix of the object                                 *
      *                                                                        *
      * Returns    : 12a - Generated name for object, or blanks for error      *
     *========================================================================*
     P NameGen         B

      ***Procedure Interface ***
     D NameGen         PI            12a
     D   xxSuffix                     3a   Const

      *** Locals ***
     D RtnValue        S                   Like(@@NameOut) Inz(*blanks)
     D xxPrefix        S              2a
     D xxDocID         S                   Like(@@NameOut) Inz(*blanks)
     D xxSeq#          S              6a
     D xiSeq#          S              6  0

      *** KList Definitions ***
     C     DIkey         KList
     C                   KFld                    @@ScanID

      *** Procedure Definition ***
     C                   Time                    xxTime
     C     DIkey         Chain     PDI01                              60
     C                   If        *in60 = *off

      ** Get the generation values from the file and increment the sequence.
     C                   Eval      xiSeq# = DISEQ# + 1
     C                   Move      xiSeq#        xxSeq#
     C                   Eval      DISEQ# = xiSeq#

     C                   Eval      xxPrefix = DIWSID

     C                   Eval      xxDocID = xxPrefix + xxSeq# + '.' + xxSuffix
     C                   EndIf

     C                   Eval      RtnValue = xxDocID

     C                   Eval      xxDateOut = CvtDate(xxDate:*blanks:dtYMD)
     C                   MoveL     xxDateOut     DILDT
     C                   Eval      DILTIM = xxTime

     C                   Update    PDI01

      ** Return the generated name, or blanks for error.
     C                   Return      RtnValue

     P NameGen         E

     *========================================================================*
     * NewSub                                                                 *
     *========================================================================*
      * Description: Calls the program to create a new subfolder.              *
      *                                                                        *
     * Parameters : 8a - Current folder                                       *
      *              12a - Current subfolder                                   *
      *                                                                        *
      * Returns    : 1a - Return code; blank=success, '1'=Error                *
     *========================================================================*
     P NewSub          B

      *** Procedure Interface ***
     D NewSub          PI             1a
     D   zzInFolder                        Const Like(FBCIF)
     D   zzInSubFld                        Const Like(FBCISF)

      *** Locals ***
     D RtnValue        S              1a
     D xcSuffix        S              3a
     D xiSuffix        S              3  0
     D xxFolder        S                   Like(FBCIF)
     D xxSubFldr       S                   Like(FBCISF)
     D xxRtnCode       S              1a

     D pgAddFldr       C                   Const('ISADDF')

      *** PList Definitions ***
     C     plADDF        PList
     C                   Parm                    xxFolder
     C                   Parm                    xxSubFldr
     C                   Parm                    xxRtnCode

      *** Procedure Definition ***
     C                   Eval      xxFolder = zzInFolder
     C                   Eval      xxSubFldr = zzInSubFld

     C                   Eval      xcSuffix = %SUBST(xxSubFldr:10:3)
     C                   Move      xcSuffix      xiSuffix
     C                   Eval      xiSuffix = xiSuffix + 1
     C                   Move      xiSuffix      xxSubFldr

     C                   CALL      pgAddFldr     plADDF
     C                   Eval      RtnValue = xxRtnCode
     C                   Eval      FBC#DS = 0

     C                   Return      RtnValue

     P NewSub          E

     *========================================================================*
     * UpdateDARTS                                                            *
     *========================================================================*
      * Description: Update the DARTS remark with the multi-object flag.       *
      *                                                                        *
     * Parameters : 40a - DARTS key                                           *
      *               5a - DARTS sequence                                      *
      *                                                                        *
      * Returns    :  1a - Blank for success; '1' for error.                   *
     *========================================================================*
     P UpdateDARTS     B

      *** Procedure Interface ***
     D UpdateDARTS     PI             1a
     D   zzKey                       40a   Const
     D   zzSequence                   5a   Const

      *** Locals ***
     D xxSequence      S              5  0

      *** KList Definitions ***
     C     CMkey         KList
     C                   KFld                    zzKey
     C                   KFld                    xxSequence

      *** Procedure Definition ***
     C                   Move      zzSequence    xxSequence
     C     CMkey         Chain     PCM01                              60
     C                   If        *in60 = *on
     C                   Return    '1'
     C                   Else

     C                   If        CMQPTY = 'Y'
     C                   Return    *blank
     C                   Else
     C                   Eval      CMQPTY = 'Y'
     C                   Eval      CMUSER = q1user
     C                   Eval      CMPGM = q1pgm
     C                   Time                    CMTIME
     C                   Eval      xxDateOut = CvtDate(xxDate:*blanks:dtCYMD)
     C                   MoveL     xxDateOut     CMDT

     C                   Update    PCM01
     C                   EndIf

     C                   EndIf

     C                   Return    *blank

     P UpdateDARTS     E

     *========================================================================*
     * WriteMD                                                                *
     *========================================================================*
      * Description: Write the multiple-object record for the object.          *
      *                                                                        *
     * Parameters : 40a - DARTS key                                           *
      *               5a - DARTS sequence                                      *
      *              65a - Object in                                           *
      *              12a - Generated name of object                            *
      *              30a - Generated path of object                            *
      *               5a - DARTS remark code                                   *
      *                                                                        *
      * Returns    : None                                                      *
     *========================================================================*
     P WriteMD         B

      *** Procedure Interface ***
     D WriteMD         PI
     D   zzKey                       40a   Const
     D   zzSequence                   5a   Const
     D   zzObjIn                           Const Like(@@ObjIn)
     D   zzName                            Const Like(@@NameOut)
     D   zzPath                            Const Like(@@PathOut)
     D   zzRmkCode                         Const Like(@@RmkCode)

      *** Procedure Definition ***
     C                   Time                    xxTime
     C                   Eval      MDKEY = zzKey
     C                   Move      zzSequence    MDSEQ#
     C                   Eval      MDOID = zzName
     C                   Eval      MDOFLR = zzPath
     C                   Eval      MDOTYP = 'O'
     C                   Eval      MDODSC = zzObjIn
     C                   Eval      xxDateOut = CvtDate(xxDate:*blanks:dtCYMD)
     C                   MoveL     xxDateOut     MDTRDT
     C                   Eval      MDTRTM = xxTime
     C                   Eval      MDSTAT = 'A'
     C                   Eval      MDPNAM = q1pgm
     C                   Eval      MDRMCD = zzRmkCode

      ** Initial fields
     C                   Eval      MDIPGM = q1pgm
     C                   Eval      xxDateOut = CvtDate(xxDate:*blanks:dtCYMD)
     C                   MoveL     xxDateOut     MDIDT
     C                   Eval      MDITIM = xxTime
     C                   Eval      MDIUSR = q1user

     C                   WRITE     PMD01

     C                   Return

     P WriteMD         E
