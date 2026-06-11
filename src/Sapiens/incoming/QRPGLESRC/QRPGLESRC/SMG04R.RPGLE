     *========================================================================*
     * (c) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE SMG04R - MC Group Name Search - Copy/Transfer
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Calls a program to allow the user to select a Group.  If one is       *
     *  selected, the DARTS key is returned for that Group.                   *
     *                                                                        *
     * CALLED BY : VDARTS                                                     *
     * CALLS  TO : SC916C1 - Program to create a copy of search program into  *
     *                       QTEMP.                                           *
     *             F0060R - Work With Groups (select mode)                    *
     *                                                                        *
     * CREATED   : 02/04/2000                                                 *
     * PROGRAMMER: C:113                                                      *
     * PROJECT   : DR095                                                      *
     *========================================================================*
     * REVISIONS:                                                             *
     *                                                                        *
     *                                                                        *
     *========================================================================*
     * NOTES:                                                                 *
     *                                                                        *
     *                                                                        *
     *========================================================================*
      ** Managed Care Group Master File
     FFMFNP     IF   E             DISK

     *========================================================================*
     * Prototypes
     *========================================================================*
     D LIBLpgm         PR                  ExtPgm('SC916C1')
     D  program                      10a
     D  call                          1a

     D SearchPgm       PR                  ExtPgm('F0060R')
     D  call                          1a
     D  kc                            1a
     D  kl                            1a
     D  rrn                           9  0

     D McCvtApi        PR                   ExtPgm('FA700R')
     D  rtncode                       1a
     D  company                       3a
     D  wc_group                      3a
     D  subscriber                    5a
     D  mc_group                      3a

     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D @key            S             40a
     D @rtnCode        S              1a

     D call            S              1a
     D KC              S              1a
     D KL              S              1a
     D program         S             10a   Inz('F0060R')
     D RRN             S              9  0
     D rtnCode         S              1a
     D tempKey         S             40a

     *========================================================================*
     * Data Structure & Array Definitions
     *========================================================================*
      ** Key structures
     D                 DS
     D mcKey                         40a
     D  mcCompany                     3a   Overlay(mcKey:1)
     D  mcSub#                        5a   Overlay(mcKey:4)
     D  mcGroup                       3a   Overlay(mcKey:9)

     D                 DS
     D wcKey                         40a
     D  wcCompany                     3a   Overlay(wcKey:1)
     D  wcGroup                       3a   Overlay(wcKey:4)

     *========================================================================*
     * PLIST Definitions
     *========================================================================*
     C     *entry        PList
     C                   Parm                    @key
     C                   Parm                    @rtnCode

     *========================================================================*
     * Main
     *========================================================================*
      ** Create a copy of the search program in QTEMP to avoid recursive calls.
     C                   Eval      call = '1'
     C                   CALLP     LIBLpgm(program:call)

     ** Call Work With Groups in search mode.
     C                   Eval      call = '2'
     C                   CALLP     SearchPgm(call:KC:KL:RRN)

      ** If F3 was pressed in the search, exit.
     C                   If        KC = *on
     C                   Eval      @rtnCode = '3'
     C                   Else
    
     C                   If        RRN > *zero
     C                             AND KL = *off
     C     RRN           Chain     FMFNP                              60
    
     C                   If        *in60 = *on
     C                   Eval      @rtnCode = 'D'
     C                   Else
      ** Create key
     C                   Move      FNCO#         mcCompany
     C                   Move      FNCO#         wcCompany
     C                   Move      FNSUB#        mcSub#
     C                   Move      FNFND         mcGroup
      ** Convert key if linked with Workers' Comp.
     C                   CALLP     McCvtApi(rtnCode:
     C                                      mcCompany:
     C                                      wcGroup:
     C                                      mcSub#:
     C                                      mcGroup)

     C                   If        wcGroup = '000'
     C                             OR wcGroup = *blanks
     C                   Eval      tempKey = mcKey
     C                   Else
     C                   Eval      tempKey = wcKey
     C                   EndIf

     C                   Clear                   @rtnCode
     C                   MoveL     tempKey       @key

     C                   EndIf

     C                   Else
     C                   Eval      @rtnCode = 'C'
     C                   EndIf

     C                   EndIf

     C                   Eval      *inLR = *on
