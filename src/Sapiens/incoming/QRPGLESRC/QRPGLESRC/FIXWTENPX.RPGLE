     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE FIXWTENPX - REMOVE UPDATE EXTENSION IN PRINT PROGRAMS.
     *========================================================================*
     * FIXWTENPX - Fix program for WTENP.                                     *
     *                                                                        *
     * PROGRAM DESCRIPTION:                                                   *
     *   Program will remove the update extension from the print program of   *
     *   the form passed in.                                                  *
     *                                                                        *
     * CREATED   : 04/19/2000                                                 *
     * PROGRAMMER: C:90                                                       *
     * PROJECT   : SUPPORT #22552                                             *
     *========================================================================*
     FWTENL3    UF   E           K Disk

     D                 DS
     D ENPGM1                        10
     D  xxpgm                  1      7                                         Year
     D  xxext                  8     10
     D                 DS
     D ENPGM2                        10
     D  xxpgm2                 1      7                                         Year
     D  xxext2                 8     10
     D                 DS
     D ENPGM3                        10
     D  xxpgm3                 1      7                                         Year
     D  xxext3                 8     10

      ** External data structure
     D psds          ESDS                  ExtName(szq1p)

     D*@@Company                      3  0
     D*@@Form                        15

     *========================================================================*
     * PLIST Definitions
     *========================================================================*
     C     *entry        PList
     C                   Parm                    @@Company         3
     C                   Parm                    @@Form           15

     *========================================================================*
     * KLIST definitions
     *========================================================================*
     ** Endorsement form table file
     C     ENkey         KList
     C                   KFld                    xxCompany
     C                   KFld                    @@Form
     *========================================================================*
     * Main
     *========================================================================*
      ** Check all endorsements within company
     C                   move      @@Company     xxCompany         3 0
     C*
     C     ENkey         Setll     pen01
     C     ENkey         Reade     pen01                                  60

B1   C                   DoW       *in60 = *off

     C     ENPGM1        Ifne      *blanks
     C                   Move      *blanks       xxext
     C                   Endif

     C     ENPGM2        Ifne      *blanks
     C                   Move      *blanks       xxext2
     C                   Endif

     C     ENPGM3        Ifne      *blanks
     C                   Move      *blanks       xxext3
     C                   Endif

     C                   Update    pen01

     C     ENkey         Reade     pen01                                  60
E2   C                   EndDo                                                  EMkey


     C                   Eval      *inlr = *on
E
