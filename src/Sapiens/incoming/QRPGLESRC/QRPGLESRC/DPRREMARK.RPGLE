     *========================================================================
     * (C) Copyright Progressive Data Solutions, Inc., 2000
     *========================================================================
      * DPRREMARK - prototypes
      *
      * Created   : 04/19/00
      * Programmer: C:113
      * Project   : DARTS
      *
     *========================================================================
     * REVISIONS:
     *
     * XX/XX/XXXX C:XXX PROJECT: XXXXX
     *            XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
     *
     * ----------------------------------------------------------------------
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.
     * ----------------------------------------------------------------------
     *========================================================================
     * NOTES:
     *
     *
     *========================================================================

      *------------------------------------------------------------------------
      * WriteRmk - Writes a remark to the DARTS comment file
      *------------------------------------------------------------------------
      * Returns:      1a  1=success, 0=failure
      *
      * Parameters:
      *              40a  DARTS key
      *               5a  DARTS sequence
      *             256a  DARTS remark
      *               8a  Remark date (MDYY)
      *               5a  Remark code
      *              10a  Program that updated/wrote the remark
      *              10a  User who updated/wrote the remark
      *              10a  DARTS Program Name
      *              12a  Document ID
      *              80a  Folder for document
      *               1a  Diary Item flag (Y/N)
      *               1a  Data Entry interface flag (Y/N)
      *               1a  Purgeable item flag  (Y/N)
      *               1a  Secure remark by level (Y/N)
      *               3a  Security level (if secured by level)
      *------------------------------------------------------------------------
      ** Procedure Prototype
     D WriteRmk        PR             1a
     D  Key                          40a   const
     D  Sequence                      5a
     D  Comment                     256a   const
     D  RmkDate                       8a   const
     D  RmkCode                       5a   const
     D  Program                      10a   const
     D  User                         10a   const
     D  Pnam                         10a   const
     D  DiaryFlag                     1a   const
     D  DEflag                        1a   const
     D  PurgeFlg                      1a   const
     D  SecByLvl                      1a   const
     D  SecLevel                      3a   options(*nopass)
     D  DocID                        12a   options(*nopass)
     D  Fold                         80a   options(*nopass)

      *------------------------------------------------------------------------
      * WriteAddl - Writes a remark to the DARTS additional remark file
      *------------------------------------------------------------------------
      * Returns:      1a  1=success, 0=failure
      *
      * Parameters:   1a  Update/Write flag (U/W)
      *              40a  DARTS key
      *               5a  DARTS sequence
      *               5a  Addional Sequence#
      *              75a  DARTS remark
      *               5a  Remark code
      *              10a  Program that updated/wrote the remark
      *              10a  User who updated/wrote the remark
      *              10a  DARTS Program Name
      *               1a  Secure remark by level (Y/N)
      *               3a  Security level (if secured by level)
      *------------------------------------------------------------------------
      ** Procedure Prototype
     D WriteAddl       PR             1a
     D  UpdateFlg                     1a
     D  Key                          40a
     D  Sequence                      5a
     D  AddlSeq#                      5a
     D  Comment                      75a
     D  RmkCode                       5a
     D  Program                      10a
     D  User                         10a
     D  Pnam                         10a
     D  SecByLvl                      1a
     D  SecLevel                      3a

