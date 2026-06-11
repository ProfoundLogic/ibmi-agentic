      *========================================================================*
      * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
      *========================================================================*
     H/TITLE SMG01R - MC Group Name Validation
      *========================================================================*
      * PROGRAM DESCRIPTION:                                                   *
      *  Searches the Managed Care Group Master File for a matching name.      *
      *  If only one is found, it returns the DARTS key for that Group,        *
      *  otherwise it returns an error code.                                   *
      *                                                                        *
      * CALLED BY : DARTMail Server Program                                    *
      * CALLS  TO : None                                                       *
      *                                                                        *
      * CREATED   : 02/04/2000                                                 *
      * PROGRAMMER: C:113                                                      *
      * PROJECT   : DR086                                                      *
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
      *  File    Description                    Rcdfmt  Key                    *
      *  FMFNL4  MC Group Master File           PFN01   (NAME)                 *
      *  FMFNL5  MC Group Master File           PFN01   (SHNM)                 *
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
      ** Workers' Comp Claim Master File
     FFMFNL4    IF   E           K DISK
     FFMFNL5    IF   E           K DISK    Rename(PFN01:PFN02)

      *========================================================================*
      * Prototypes
      *========================================================================*
     D McCvtApi        PR                   ExtPgm('FA700R')
     D  rtncode                       1a
     D  company                       3a
     D  wc_group                      3a
     D  subscriber                    5a
     D  mc_group                      3a

      *========================================================================*
      * Stand-alone Field Definitions
      *========================================================================*
     D @name           S             40a
     D @key            S             40a
     D @rtnCode        S              1a

     D name            S                   Like(FNNAME)
     D rtnCode         S              1a
     D shortName       S                   Like(FNSHNM)
     D tempKey         S             40a   Inz

      *========================================================================*
      * Data Structure & Array Definitions
      *========================================================================*
     D                 DS                  INZ
     D mcKey                         40a
     D  mcCompany                     3a   Overlay(mcKey:1)
     D  mcSubs#                       5a   Overlay(mcKey:4)
     D  mcGroup                       3a   Overlay(mcKey:9)

     D                 DS                  INZ
     D wcKey                         40a
     D  wcCompany                     3a   Overlay(wcKey:1)
     D  wcGroup                       3a   Overlay(wcKey:4)
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
     C     FNkey         KList
     C                   KFld                    name

     C     FNkey2        KList
     C                   KFld                    shortName

      *========================================================================*
      * Main
      *========================================================================*
     C     FNkey         Chain     PFN01                              60
     C                   If        *in60 = *on

     C     FNkey2        Chain     PFN02                              60
     C                   If        *in60 = *on
     C                   Eval      @rtnCode = '2'
     C                   Else
      ** Create key
     C                   Move      FNCO#         wcCompany
     C                   Move      FNCO#         mcCompany
     C                   Move      FNSUB#        mcSubs#
     C                   Move      FNFND         mcGroup
     C                   CALLP     McCvtApi(rtnCode:
     C                                      mcCompany:
     C                                      wcGroup:
     C                                      mcSubs#:
     C                                      mcGroup)

     C                   If        wcGroup = '000'
     C                             OR wcGroup = *blanks
     C                   Eval      tempKey = mcKey
     C                   Else
     C                   Eval      tempKey = wcKey
     C                   EndIf

      ** Check for multiple matching records.
     C     FNkey2        ReadE     PFN02                                  60
     C                   If        *in60 = *off
     C                   Eval      @rtnCode = '1'
     C                   EndIf

     C                   EndIf

     C                   Else
      ** Create key
     C                   Move      FNCO#         wcCompany
     C                   Move      FNCO#         mcCompany
     C                   Move      FNSUB#        mcSubs#
     C                   Move      FNFND         mcGroup
     C                   CALLP     McCvtApi(rtnCode:
     C                                      mcCompany:
     C                                      wcGroup:
     C                                      mcSubs#:
     C                                      mcGroup)

     C                   If        wcGroup = '000'
     C                             OR wcGroup = *blanks
     C                   Eval      tempKey = mcKey
     C                   Else
     C                   Eval      tempKey = wcKey
     C                   EndIf

      ** Check for multiple matching records.
     C     FNkey         ReadE     PFN01                                  60
     C                   If        *in60 = *off
     C                   Eval      @rtnCode = '1'
     C                   EndIf

     C                   EndIf

     C                   If        @rtnCode = *blank
     C                   Eval      @key = tempKey
     C                   Else
     C                   Eval      @key = *blanks
     C                   EndIf

     C                   Eval      *inLR = *on
      *========================================================================*
      * *inzsr - Initialization subroutine
      *========================================================================*
     C     *inzsr        BEGSR

     C                   Eval      name = @name
     C                   Eval      shortName = @name

     C                   ENDSR                                                  *inzsr
