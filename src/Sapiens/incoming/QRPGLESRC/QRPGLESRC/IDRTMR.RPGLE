     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE IDRTMR - VDARTS E-MAIL SERVER EDIT PROGRAM
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  The VDARTS Email Server will parse the incoming email and call this   *
     *  program with the parsed values for validation.  The first ten errors  *
     *  will be returned to the calling program. If there were no errors,     *
     *  this program will call the DARTS API to write the DARTS remark.       *
     *                                                                        *
     * CALLED BY : VDARTS E-mail server program                               *
     *                                                                        *
     * CALLS  TO : -Various DARTS Key validation program                      *
     *             -DA700R - DARTS API                                        *
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
     * FILE USAGE:                                                            *
     *                                                                        *
      *  File    Description                    Rcdfmt  Key                    *
     *  IMDIL   Image Doc Index Control File    PDI01  (WSID)                 *
     *  SMKTL   DARTS Key Type File             PKT01  (KTYP)                 *
     *  SMSLL1  DARTS Security File             PSL01  (EMAL)                 *
     *  SMSLL   DARTS Security File             PSL02  (USRP)                 *
     *  STCTL1  DARTS Remark Type Table File    PCT01  (RMCD)                 *
     *                                                                        *
     *======================================================================= *
     * INDICATOR USAGE:                                                       *
     *                                                                        *
     *  60 - General Chain indicator.                                         *
     *  99 - General Error indicator.                                         *
     *                                                                        *
     *======================================================================= *
     * NOTES:                                                                 *
     *                                                                        *
     *                          ** IMPORTANT **                               *
     * -The following are the error return codes.  If others are added, the   *
     * field ccNumErr must be adjusted accordingly.  To indicate an error,    *
     * move *on to Err(number of error).  The program will return the first   *
     * ten errors to the calling program.  A blank return error code list     *
     * indicates no errors.                                                   *
     *                                                                        *
     *  01 Multiple records match name.                                       *
     *  02 Invalid Key/Name(not found) (KEY: or NAME:).                       *
     *  03 Invalid Remark Code (RC).                                          *
     *  04 Invalid Remark Date (RD).                                          *
     *  05 Invalid Diary Date (DT).                                           *
     *  06 Invalid Diary User (DU).                                           *
     *  07 Invalid Key Type (TYPE:).                                          *
     *  08 Can't resolve Sender's E-mail Address (SNDR).                      *
     *  09 User not allowed to write DARTS remark.                            *
     *  10 Invalid Scan ID (SCNID).                                           *
     *  11 Invalid Default User (DFTUSR).                                     *
     *                                                                        *
     *========================================================================*
     FIMDIL     IF   E           K DISK
     FSMKTL     IF   E           K DISK
     FSMSLL1    IF   E           K DISK
     FSMSLL     IF   E           K DISK
     F                                     Rename(PSL01:PSL02)
     FSTCTL1    IF   E           K DISK

     *========================================================================*
     * Procedure Prototypes
     *========================================================================*
      **************************************************************************
      * CvtDate - Date check/conversion                                        *
      **************************************************************************
      * Parms: 8.0 - in date                                                   *
      *        8a  - from format                                               *
      *        8a  - to format                                                 *
      * Returns: 10a converted date; blank if error                            *
      **************************************************************************
      /COPY CVTDATINC

     *========================================================================*
     * Constant Definitions
     *========================================================================*
      ** Program to write DARTS remarks.
     D pgDartAPI       C                   Const('DA700R')

      ** Number of possible errors.
     D ccNumErr        C                   Const(11)

      ** Suffix for name search programs.
     D ccSuffix        C                   Const('01R')

     *========================================================================*
     * Array, Table & Data Structure Definitions
     *========================================================================*
      ** Errors
     D Err             S              1a   Dim(ccNumErr)

      ** Error codes to return.
     D  rErr           S              2a   Dim(10)

      ** Program information structure
     D psds          ESDS                  ExtName(szq1p)

     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
      ** Entry fields
     D @@WriteTyp      S              4a
     D @@RmkCode       S              5a
     D @@DiaryUsr      S             10a
     D @@RmkDate       S              8a
     D @@DiaryDat      S              8a
     D @@KeyName       S             40a
     D @@KeyType       S              2a
     D @@Resolve       S              1a
     D @@ScanID        S              2a
     D @@DftUser       S             10a
     D @@SeqRtn        S              5a
     D @@Sender        S             65a
     D @@Comment       S            256a
     D @@ErrorRtn      S             20a

      ** DARTS API parameters
     D qqKey           S                   Like(@@KeyName)
     D qqComment       S                   Like(@@Comment)
     D qqRmkDate       S                   Like(@@RmkDate)
     D qqDcid          S             12a
     D qqPnam          S             10a
     D qqSecl          S              3a
     D qqUser          S                   Like(@@DiaryUsr)
     D qqPgm           S                   Like(qqPnam)
     D qqFolder        S             80a
     D qqDifl          S              1a
     D qqIPgm          S                   Like(qqPnam)
     D qqDatEntry      S              1a
     D qqPurgable      S              1a
     D qqRmkCode       S                   Like(@@RmkCode)
     D qqSequence      S                   Like(@@SeqRtn)

      ** Working fields
     D x               S              2  0
     D i               S                   Like(x)
     D pgValPgm        S             10a
     D qqName          S                   Like(@@KeyName)
     D qqRtnCode       S              1a
     D xxDiaryUsr      S                   Like(@@DiaryUsr)
     D xxRmkDate       S              8  0
     D xxDiaryDat      S              8  0
     D xxDiaryTyp      S              1a
     D xxDftUser       S                   Like(@@DiaryUsr)
     D xxUser          S                   Like(@@DiaryUsr)
     D xxKey           S                   Like(@@KeyName)
     D xxKeyType       S              2a
     D xxRmkCode       S                   Like(@@RmkCode)
     D xxScanID        S                   Like(@@ScanID)
     D xxSeq#          S              5  0
     D xxSender        S                   Like(@@Sender)
     D xxDateOut       S             10a
     D xxDate          S              8  0

     *========================================================================*
     * PLIST Definitions
     *========================================================================*
     C     *entry        PList
     C                   Parm                    @@WriteTyp
     C                   Parm                    @@RmkCode
     C                   Parm                    @@DiaryUsr
     C                   Parm                    @@RmkDate
     C                   Parm                    @@DiaryDat
     C                   Parm                    @@KeyName
     C                   Parm                    @@KeyType
     C                   Parm                    @@Resolve
     C                   Parm                    @@ScanID
     C                   Parm                    @@DftUser
     C                   Parm                    @@SeqRtn
     C                   Parm                    @@Sender
     C                   Parm                    @@Comment
     C                   Parm                    @@ErrorRtn

      ** Key Validation program parameters
     C     plValPgm      PList
     C                   Parm      xxKey         qqKey
     C                   Parm                    qqRtnCode

      ** DARTS API parameters
     C     plDARTS       PList
     C                   Parm      xxKey         qqKey
     C                   Parm      @@Comment     qqComment
     C                   Parm                    qqRmkDate
     C                   Parm      *blanks       qqdcid
     C                   Parm                    qqPnam
     C                   Parm                    qqSecl
     C                   Parm      xxUser        qqUser
     C                   Parm      q1pgm         qqPgm
     C                   Parm      *blanks       qqFolder
     C                   Parm      xxDiaryTyp    qqDifl
     C                   Parm      q1pgm         qqIPgm
     C                   Parm                    qqDatEntry
     C                   Parm                    qqPurgable
     C                   Parm      @@RmkCode     qqRmkCode
     C                   Parm      *blanks       qqSequence

      ** Name search parameters
     C     plSrch        PList
     C                   Parm                    qqName
     C                   Parm                    qqKey
     C                   Parm                    qqRtnCode

     *========================================================================*
     * KLIST definitions
     *========================================================================*
      ** DARTS Remark Type file
     C     CTkey         KList
     C                   KFld                    xxRmkCode

      ** DARTS Security file
     C     SLkey         KList
     C                   KFld                    xxSender

     C     SLkey1        KList
     C                   KFld                    xxUser

      ** Key Type file
     C     KTkey         KList
     C                   KFld                    xxKeyType

      ** Document Index Control file
     C     DIkey         KList
     C                   KFld                    xxScanID

     *========================================================================*
     * Main
     *========================================================================*
      ** Edit check the parms.
     C                   Exsr      ErrChk

     C                   If        *in99 = *off

      ** We have passed all edits, so write remarks.
     C                   Exsr      WrtRmk
     C                   Else
      ** We have some errors, so generate the return codes.
     C                   Exsr      RtnCds

     C                   EndIf

      ** Debug **
     C*    @@ErrorRtn    Dsply
     C*    @@KeyName     Dsply
     C*    @@SeqRtn      Dsply

     C                   Eval      *inLR = *on
     *========================================================================*
     * *inzsr - Initialization subroutine
     *========================================================================*
     C     *inzsr        BEGSR

     C                   Eval      xxDate     = *date
     C                   Eval      xxSender   = @@Sender
     C                   Eval      xxRmkCode  = @@RmkCode
     C                   Eval      xxKey      = @@KeyName
     C                   Eval      xxKeyType  = @@KeyType
     C                   Eval      xxScanID   = @@ScanID
     C                   Eval      xxDiaryTyp = *blank

     C                   If        @@RmkDate = *blanks
     C                   Eval      xxRmkDate = xxDate
     C                   Else
     C                   Move      @@RmkDate     xxRmkDate
     C                   EndIf

     C                   If        @@DiaryDat = *blanks
     C                   Eval      xxDiaryDat = xxDate
     C                   Else
     C                   Move      @@DiaryDat    xxDiaryDat
     C                   EndIf

     C                   ENDSR                                                  *inzsr
     *========================================================================*
     * ErrChk - Check input fields for errors.
     *========================================================================*
     C     ErrChk        BEGSR

      ** Validate Remark Code
     C     CTkey         Chain     PCT01                              60
     C                   If        *in60 = *on
     C                   Eval      *in99 = *on
     C                   Eval      Err(03) = *on
     C                   Else

     C                   Eval      xxDiaryTyp = CTFL01
     C                   Move      CTDSLV        qqSecl

     C                   If        CTDEPG <> *blanks
     C                   Eval      qqDatEntry = 'Y'
     C                   EndIf

     C                   If        CTDPRG <> *zeros
     C                   Eval      qqPurgable = 'Y'
     C                   EndIf

      ** If the comment is blank, use the remark code description.
     C                   If        @@Comment = *blanks
     C                   Eval      @@Comment = CTDESC
     C                   EndIf

     C                   EndIf                                                  *in60 = *on

      ** Determine if it is a diary type and who it is diaried to.
     C                   If        @@DiaryUsr = *blanks
     C                   If        xxDiaryTyp = 'Y'

     C                   If        @@Resolve = 'Y'

      ** Resolve the user from the e-mail address.
     C                   Exsr      Resolve

     C                   Else
     C                   Eval      xxUser = @@DftUser
     C                   EndIf                                                  @@Resolve = 'Y'

     C                   Else
     C                   Eval      xxUser = @@DftUser
     C                   EndIf                                                  xxDiaryTyp = 'Y'

     C                   Else
     C                   Eval      xxUser = @@DiaryUsr
     C                   Eval      xxDiaryTyp = 'Y'

     C                   EndIf                                                  @@DiaryUsr = *blanks

      ** Validate user.
     C     SLkey1        Chain     PSL02                              60
     C                   If        *in60 = *on
     C                   Eval      *in99 = *on

     C                   If        xxDiaryTyp = 'Y'
     C                   Eval      Err(06) = *on
     C                   Else
     C                   Eval      Err(11) = *on
     C                   EndIf                                                  xxDiaryTyp = 'Y'

     C                   EndIf                                                  *in60 = *on

      ** Validate the key type.
     C     KTkey         Chain     PKT01                              60
     C                   If        *in60 = *on
     C                   Eval      *in99 = *on
     C                   Eval      Err(07) = *on
     C                   Else

     C                   Eval      qqPnam = KTPNAM

     C                   If        @@WriteTyp = 'KEY '
      ** Validate the Key.
     C                   Exsr      ValKey
     C                   Else
      ** Validate the Name.
     C                   Exsr      ValName
     C                   EndIf                                                  @@WriteTyp = 'KEY '

     C                   EndIf                                                  *in60 = *on

      ** Validate Remark Date.
     C                   If        CvtDate(xxRmkDate:*blanks:*blanks) = *blanks
     C                   Eval      *in99 = *on
     C                   Eval      Err(04) = *on
     C                   EndIf

      ** Validate Diary Date.
     C                   If        CvtDate(xxDiaryDat:*blanks:*blanks) = *blanks
     C                   Eval      *in99 = *on
     C                   Eval      Err(05) = *on
     C                   EndIf

      ** Validate the Scan ID (This is used in the next program to build folders, etc.)
     C     DIkey         Chain     PDI01                              60
     C                   If        *in60 = *on
     C                   Eval      *in99 = *on
     C                   Eval      Err(10) = *on
     C                   EndIf

     C                   ENDSR                                                  ErrChk
     *========================================================================*
     * Resolve - Resolve user from email address.
     *========================================================================*
     C     Resolve       BEGSR

     C     SLkey         Chain     PSL01                              60
     C                   If        *in60 = *off
     C                   Eval      xxUser = SLUSRP

     C                   If        SLALWR <> 'Y'
     C                   Eval      *in99 = *on
     C                   Eval      Err(09) = *on
     C                   EndIf                                                  SLALWR = 'Y'

     C                   Else
      ** Can't resolve user ID.
     C                   Eval      *in99 = *on
     C                   Eval      Err(08) = *on
     C                   EndIf

     C                   ENDSR                                                  Resolve
     *========================================================================*
     * RtnCds - Compile the return codes.
     *========================================================================*
     C     RtnCds        BEGSR

     C                   Eval      x = 1
     C                   Eval      i = 1

     C                   Do        ccNumErr      x

     C                   If        Err(x) = *on
     C                   Move      x             rErr(i)
     C                   Eval      i = i+1

      ** Only return up to 10 codes.
     C                   If        i > 10
     C                   Leave
     C                   EndIf

     C                   EndIf

     C                   EndDo

     C*                  Eval      @@ErrorRtn = dsErrors
     C                   MoveA     rErr          @@ErrorRtn

     C                   ENDSR                                                  RtnCds
     *========================================================================*
     * ValKey - Validate Key
     *========================================================================*
     C     ValKey        BEGSR

      ** Validate the key.
     C                   Eval      pgValPgm = KTVPGM
     C                   CALL      pgValPgm      plValPgm

     C                   If        qqRtnCode <> *blank
     C                   Eval      *in99 = *on
     C                   Eval      Err(02) = *on
     C                   EndIf

     C                   ENDSR                                                  ValKey
     *========================================================================*
     * ValName - Validate KeyType/Name.
     *========================================================================*
     C     ValName       BEGSR

      ** Validate the name based on key type.
     C                   Eval      qqKey = *blanks
     C                   Eval      qqName = xxKey
     C                   Eval      qqRtnCode = *blank

     C                   If        KTSRC1 <> *blanks

     C                   CALL      KTSRC1        plSrch

     C                   If        qqRtnCode = *blank
     C                   Eval      xxKey = qqKey
     C                   Eval      @@KeyName = xxKey
     C                   Else
     C                   Eval      *in99 = *on
     C                   Eval      i = 0
     C                   Move      qqRtnCode     i
     C                   Eval      Err(i) = *on
     C                   Eval      xxKey = *blanks
     C                   EndIf

     C                   Else
     C                   Eval      *in99 = *on
     C                   Eval      Err(1) = *on
     C                   EndIf

     C                   ENDSR                                                  ValName
     *========================================================================*
     * WrtRmk - Write the DARTS remark.
     *========================================================================*
     C     WrtRmk        BEGSR

      ** Call the DARTS API.
     C                   If        xxDiaryTyp = 'Y'
     C                   Move      xxDiaryDat    qqRmkDate
     C                   Else
     C                   Move      xxRmkDate     qqRmkDate
     C                   EndIf

     C                   CALL      pgDartAPI     plDARTS

      ** Move the return value.
     C                   Move      qqSequence    @@SeqRtn

     C                   ENDSR                                                  WrtRmk
