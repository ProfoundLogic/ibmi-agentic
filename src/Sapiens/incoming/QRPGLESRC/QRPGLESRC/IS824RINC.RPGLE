     *------------------------------------------------------------------------
     * CvtToDate
     *------------------------------------------------------------------------
     * Description: Converts number to date data type.
     *
     * Parameter1:  8.0 - date to convert
     *
     * Returns: date data type (date will be 01/01/0001 if invalid)
     *------------------------------------------------------------------------
     D CvtToDate       PR              d
     D  date                          8  0 Const
     D


     *------------------------------------------------------------------------
     * Cvt2cymd
     *------------------------------------------------------------------------
     * Description: Converts a numeric date in *MDYY format to *CYMD format
     *
     * Parameter 1:  8.0 - date to convert
     *
     * Returns:      7.0 - converted date (zero if invalid)
     *------------------------------------------------------------------------
     D Cvt2cymd        PR             7  0
     D  date8                         8  0 Const


     *------------------------------------------------------------------------
     * WriteRmk - This API writes a DARTS remark.  It performs no error
     *            checking.
     *------------------------------------------------------------------------
     * Returns:      1a  1=success, 0=failure
     *
     * Parameters:  40a  DARTS key
     *               5a  DARTS sequence
     *             256a  DARTS remark
     *               8a  Remark date (MDYY)
     *               5a  Remark code
     *              10a  Program that updated/wrote the remark
     *              10a  User who updated/wrote the remark
     *              10a  DARTS Program Name
     *              12a  Document ID
     *              80a  Folder for document
     *               1a  Diary Item flag (Y/N)
     *               1a  Data Entry interface flag (Y/N)
     *               1a  Purgeable item flag  (Y/N)
     *               1a  Secure remark by level (Y/N)
     *               3a  Security level (if secured by level)
     *               12a Document ID
     *               80a Document folder
     *               19a Remark (end)
     *                1a Image Flag
     *               12a Img Document ID
     *------------------------------------------------------------------------
     D WriteRmk        PR             1a
     D  eKey                         40a   const                                Key
     D  eSequence                     5a                                        Sequence
     D  eComment                    256a   const                                Remark
     D  eRmkDate                      8a   const                                Remark date
     D  eRmkCode                      5a   const                                Remark code
     D  eProgram                     10a   const                                Maint program
     D  eUser                        10a   const                                Maint user
     D  ePname                       10a   const                                Key program
     D  eDiaryFlag                    1a   const                                Diary flag
     D  eDEflag                       1a   const                                Data entry flag
     D  ePurgeFlg                     1a   const                                Purge flag
     D  eSecByLvl                     1a   const                                Secure by lvl flag
     D  eSecLevel                     3a   const options(*nopass)               Security level
     D  eDocID                       12a   const options(*nopass)               Document ID
     D  eFolder                      80a   const options(*nopass)               Document folder
     D  eComm2                       19a   const options(*nopass)               Remark (end)
     D  eImgFlag                      1a   const options(*nopass)               Image app prod
     D  eImgDocId                    12a   const options(*nopass)               Img Document ID
