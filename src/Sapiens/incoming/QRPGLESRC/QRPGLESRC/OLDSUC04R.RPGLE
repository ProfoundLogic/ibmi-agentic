     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2002                   *
     *========================================================================*
    H/TITLE SUC04R - Summit Copy/Transfer Claim Search
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Allows the user to select a Claim. If one is selected, it returns     *
     *  the DARTS key for that Claim, otherwise it returns an error code.     *
     *                                                                        *
     * CALLED BY : VDARTS                                                     *
     *                                                                        *
     * CALLS  TO : WCS215 Summit Claim Search program.                        *
     *                                                                        *
     * CREATED   : 02/28/2002                                                 *
     * PROGRAMMER: C:128                                                      *
     * PROJECT   : 10IBS                                                      *
     *========================================================================*
     * REVISIONS:                                                             *
     *                                                                        *
     *                                                                        *
     *========================================================================*
     * NOTES:                                                                 *
     *                                                                        *
     *                                                                        *
     *========================================================================*

     *========================================================================*
     * Prototypes
     *========================================================================*

     D SearchPgm       PR                  ExtPgm('WCS215')
     D  key                          25a

     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D eKey            S             40a
     D eRtnCode        S              1a

     D                 DS
     D key                           25a
     D  @class                        1a   Overlay(key:1) inz
     D  @maint                        1a   Overlay(key:2) inz
     D  @pname                       10a   Overlay(key:3)
     D  @key                         13a   Overlay(key:13)inz

     *========================================================================*
     * PLIST Definitions
     *========================================================================*
     C     *entry        PList
     C                   Parm                    eKey
     C                   Parm                    eRtnCode

     *========================================================================*
     * Main
     *========================================================================*
     ** Call Summit Claim Search program.
     C                   MoveL     'SUC04R'      @pname
     C                   CALLP     SearchPgm(key)

     C                   If        key <> *blanks
     C                   Clear                   eRtnCode
     C                   MoveL     key           eKey
     C                   Else
     C                   Eval      eRtnCode = 'C'
     C                   EndIf
    
     C                   Eval      *inLR = *on
