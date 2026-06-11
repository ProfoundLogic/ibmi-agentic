     *========================================================================*
    H* (c) 2009 StoneRiver, Inc. All rights Reserved.                *
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     *========================================================================*
     * REVISIONS:                                                             *
     *                                                                        *
      * 01/28/10  C:XXX  Project: SY215                               *
      *                  Retrieve brand Name
     *                                                                        *
     * ---------------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.                    *
     * ---------------------------------------------------------------------- *
     *========================================================================*
     * NOTES:                                                                 *
     *                                                                        *
     *                                                                        *
     *========================================================================*

     D EXIT            C                   CONST(03)
     D CANCEL          C                   CONST(12)
     D ERROR           C                   CONST(99)

     D ADD_MODE        C                   CONST('A')
     D CHG_MODE        C                   CONST('C')
     D DSP_MODE        C                   CONST('D')
     D DLT_MODE        C                   CONST('E')
     D CPY_MODE        C                   CONST('P')
     D SLC_MODE        C                   CONST('S')

     D BLANK           C                   *blanks

     D MODEA           C                   '-- Add --'
     D MODEC           C                   '- Change -'
     D MODED           C                   '-Display-'

     D YES             C                   CONST('Y')
     D NO              C                   CONST('N')
     D*** BEGIN CHG *********** 01/28/10 ************************************
     D*** PDSNAME      C                   'Progressive Data Solutions, Inc.'
     D PDSNAME         C                   'Stoneriver, Inc.'
     D*** END CHG ************* 01/28/10 ************************************
     D DELETED         C                   '** DELETED'

     D PgmStatus       S              2  0 Inz(0)

     D @@RRN           S              9  0
     D @@Mode          S              1a
     D @@Return        S              1a

     D PSDS          ESDS                  ExtName(szq1p)

     D                 DS
     D dsDateTime                    14  0
     D  dsTime                        6  0 Overlay(dsDateTime)
     D  dsDate                        8  0 Overlay(dsDateTime:7)
