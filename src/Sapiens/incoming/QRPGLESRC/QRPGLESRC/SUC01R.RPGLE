      *========================================================================*
      * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
      *========================================================================*
     H/TITLE SUC01R - Summit Claim Name Validation
      *========================================================================*
      * PROGRAM DESCRIPTION:                                                   *
      *  Searches the Summit Claims Master File for a matching name.           *
      *  If only one is found, it returns the DARTS key for that Claim,        *
      *  otherwise it returns an error code.                                   *
      *                                                                        *
      * CALLED BY : DARTMail Server Program                                    *
      * CALLS  TO : None                                                       *
      *                                                                        *
      * CREATED   : 02/05/2002                                                 *
      * PROGRAMMER: C:128                                                      *
      * PROJECT   : 10IBS                                                      *
      *========================================================================*
      * REVISIONS:                                                             *
      *                                                                        *
      * XX/XX/XXXX C:XXX PROJECT: XXXXX                                        *
      *            XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                                                        *
      * ---------------------------------------------------------------------- *
      *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.                    *
      * ---------------------------------------------------------------------- *
      *========================================================================*
      * FILE USAGE:                                                            *
      *                                                                        *
      *  File    Description                   Rcdfmt       Key                *
      *  EMPLEL1 Claims Master by Name         EMPLYEPF     (EENAME)           *
      *                                                                        *
      *========================================================================*
      * INDICATOR USAGE:                                                       *
      *                                                                        *
      *  60 - General Chain indicator.                                         *
      *                                                                        *
      *========================================================================*
      * NOTES:                                                                 *
      *                                                                        *
      *                                                                        *
      *========================================================================*
      ** Summit Claim Master File by Name
     FEMPLYEL1  IF   E           K DISK

      *========================================================================*
      * Prototypes
      *========================================================================*

      *========================================================================*
      * Stand-alone Field Definitions
      *========================================================================*
     D @name           S             40a
     D @key            S             40a
     D @rtnCode        S              1a

     D name            S                   Like(EENAME)
     D rtnCode         S              1a

      *========================================================================*
      * Data Structure & Array Definitions
      *========================================================================*
     D                 DS                  INZ
     D ucKey                         40a
     D  ucCo#                         3a   Overlay(ucKey:1)
     D  ucFnd                         3a   Overlay(ucKey:4)
     D  ucFyr                         3a   Overlay(ucKey:6)
     D  ucClaim                       7a   Overlay(ucKey:9)

      *========================================================================*
      * PLIST Definitions
      *========================================================================*
     C     *entry        PList
     C                   Parm                    @name
     C                   Parm                    @key
     C                   Parm                    @rtnCode

      *========================================================================*
      * KLIST definitions
      *========================================================================*
     C     EEkey         KList
     C                   KFld                    name

      *========================================================================*
      * Main
      *========================================================================*

     C     EEkey         Chain     EMPLYEL1                           60
     C                   If        *in60 = *on
     C                   Eval      @rtnCode = '2'
     C                   Else

      ** Check for multiple matching records.
     C     EEkey         ReadE     EMPLYEL1                               60
     C                   If        *in60 = *off
     C                   Eval      @rtnCode = '1'
     C                   Eval      @Key = *Blank
     C                   Else

      ** No multiple records - Return key for single name found.
     C                   Eval      @rtnCode = ' '
      ** Create key
     C                   Move      '001'         ucCo#
     C                   Move      INSDNO        ucFnd
     C                   Move      CALNYR        ucFyr
     C                   Move      CASENO        ucClaim
     C                   Eval      @Key = ucKey
     C                   EndIf

     C                   EndIf

     C                   Eval      *inLR = *on
      *========================================================================*
      * *inzsr - Initialization subroutine
      *========================================================================*
     C     *inzsr        BEGSR

     C                   Eval      name = @name

     C                   ENDSR                                                  *inzsr
