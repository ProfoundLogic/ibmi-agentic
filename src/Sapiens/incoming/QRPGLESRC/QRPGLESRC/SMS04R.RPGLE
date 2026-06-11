     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE SMS04R - MC Subscriber Name Search - Copy/Transfer
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Calls a program to allow the user to select a Subscriber.  If one is  *
     *  selected, the DARTS key is returned for that Subscriber.              *
     *                                                                        *
     * CALLED BY : VDARTS                                                     *
     * CALLS  TO : SC916C1 - Program to create a copy of search program into  *
     *                       QTEMP.                                           *
     *             F0055R - Work With Subscribers (select mode)               *
     *                                                                        *
     * CREATED   : 02/07/2000                                                 *
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
     FFMSMP     IF   E             DISK

     *========================================================================*
     * Prototypes
     *========================================================================*
     D LIBLpgm         PR                  ExtPgm('SC916C1')
     D  program                      10a
     D  call                          1a

     D SearchPgm       PR                  ExtPgm('F0055R')
     D  call                          1a
     D  kc                            1a
     D  kl                            1a
     D  rrn                           9  0

     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D eKey            S             40a
     D eRtnCode        S              1a

     D pCall           S              1a
     D pKC             S              1a
     D pKL             S              1a
     D pRRN            S              9  0

     D pProgram        S             10a   Inz('F0055R')

     *========================================================================*
     * Data Structure & Array Definitions
     *========================================================================*
      ** Key structure
     D                 DS
     D Key                           40a
     D  Company                       3a   Overlay(Key:1)
     D  Subscriber                    5a   Overlay(Key:4)

     *========================================================================*
     * PLIST Definitions
     *========================================================================*
     C     *entry        PList
     C                   Parm                    eKey
     C                   Parm                    eRtnCode

     *========================================================================*
     * Main
     *========================================================================*
      ** Create a copy of the search program in QTEMP to avoid recursive calls.
     C                   Eval      pCall = '1'
     C                   CALLP     LIBLpgm(pProgram : pCall)

     ** Call Work With Medical Cases for a search.
     C                   Eval      pCall = '2'
     C                   CALLP     SearchPgm(pCall : pKC : pKL : pRRN)

      ** If F3 was pressed in the search, exit.
     C                   If        pKC = *on
     C                   Eval      eRtnCode = '3'
     C                   Else
    
     C                   If        pRRN > *zero
     C                             AND pKL = *off
     C     pRRN          Chain     FMSMP                              60
    
     C                   If        *in60 = *on
     C                   Eval      eRtnCode = 'D'
     C                   Else
      ** Create key
     C                   Move      SMCO#         Company
     C                   Move      SMSUB#        Subscriber
     C                   Clear                   eRtnCode
     C                   MoveL     Key           eKey

     C                   EndIf

     C                   Else
     C                   Eval      eRtnCode = 'C'

     C                   EndIf

     C                   EndIf

     C                   Eval      *inLR = *on
