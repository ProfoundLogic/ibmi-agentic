     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2002                   *
     *========================================================================*
    H/TITLE SUC04R - Summit Copy/Transfer Claim Search
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Allows the user to select a Claim. If one is selected, it returns     *
     *  the DARTS key for that Claim, otherwise it returns an error code.     *
     *                                                                        *
     *                                                                        *
     * CALLED BY : VDARTS                                                     *
     *                                                                        *
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

     D                 DS
     D @key                          13a
     D @Fnd                           4a   Overlay(@key:1)
     D @Fyr                           3a   Overlay(@Key:5)
     D @Claim                         6a   Overlay(@key:8)
     D                 DS
     D RtnKey                        40a
     D RCo#                           3a   Overlay(RtnKey:1)
     D RFnd                           3a   Overlay(RtnKey:4)
     D RFyr                           3a   Overlay(Rtnkey:7)
     D RPad                           1a   Overlay(RtnKey:10)
     D RClaim                         6a   Overlay(RtnKey:11)
     I*
     *========================================================================*
     * Prototypes
     *========================================================================*
     D LIBLpgm         PR                  ExtPgm('SC916C1')
     D  program                      10a
     D  call                          1a

     D SearchPgm       PR                  ExtPgm('WCS215')
     D  gclass                        1a
     D  gmaint                        1a
     D  gpname                       10a
     D  gkey                         13a


     D
     D

     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D  key            S             25a
     D eKey            S             40a
     D eRtnCode        S              1a

     D pCall           S              1a
     D pKC             S              1a   Inz(*off)
     D pKL             S              1a   Inz(*off)
     D pMode           S              1a   Inz(*blank)
     D pRRN            S              9  0 Inz(*zeros)
     D pRRN1           S              9  0 Inz(*zeros)
     D pQuote          S              1    Inz('W')
     D pView           S              1  0 Inz(*zero)
     D pRRN3           S              9  0 Inz(*zeros)
     D pProgram        S             10a   Inz('WCS215')
     D pProgram1       S             10a   Inz('SCI070')
     D pProgram2       S             10a   Inz('SCI060')
     D pProgram3       S             10a   Inz('SCI080')
     D pProgram4       S             10a   Inz('WCS210')
     D*
     D  @class         S              1a
     D  @maint         S              1a
     D  @pname         S             10a
     D**@key           S             13a

      ** Key structure
     D                 DS
     D*key                           25a
     D* @class                        1a   Overlay(key:1) inz
     D* @maint                        1a   Overlay(key:2) inz
     D* @pname                       10a   Overlay(key:3)
     D* @key                         13a   Overlay(key:13)inz

     D                 DS
     D xKey                          40a
     D  xCompany                      3a   Overlay(xKey:1)
     D  xGroup                        3a   Overlay(xKey:4)
     D  xEmployer                     9a   Overlay(xKey:7)
     D  xDivision                     5a   Overlay(xKey:16)
     D  xQuote                        1a   Overlay(xKey:40)

     *========================================================================*
     * PLIST Definitions
     *========================================================================*
     C     *entry        PList
     C                   Parm                    eKey
     C                   Parm                    eRtnCode

     *========================================================================*
     * Main
     *========================================================================*
      ** Copy the program into QTEMP to avoid recursive call.
     C                   Eval      pCall = '1'
     C                   CALLP     LIBLpgm(pProgram : pCall)
     C                   CALLP     LIBLpgm(pProgram1 : pCall)
     C                   CALLP     LIBLpgm(pProgram2 : pCall)
     C                   CALLP     LIBLpgm(pProgram3 : pCall)
     C                   CALLP     LIBLpgm(pProgram4 : pCall)

     ** Call Work With Claims for a search.
     C                   Eval      pMode = '2'
     C                   Eval      @key = *blanks
     C                   Eval      @class = *blanks
     C                   Eval      @maint = *blanks
     C                   Eval      @pname = *blanks
     C                   Move      'ISII80R'     @pname
     C                   CALLP     SearchPgm(@class: @Maint: @pname: @key)
     C*
     C                   Eval      Rco# = '001'
     C                   move      @Fnd          RFnd
     C                   Eval      RFyr = @Fyr
     C                   Eval      Rpad = '0'
     C                   Move      *all'0'       RClaim
     C                   MOVE      @Claim        RClaim
     C                   movel     RtnKey        Key
     C*
     C                   Eval      pCall = '2'
     C                   CALLP     LIBLpgm(pProgram : pCall)
     C                   CALLP     LIBLpgm(pProgram1 : pCall)
     C                   CALLP     LIBLpgm(pProgram2 : pCall)
     C                   CALLP     LIBLpgm(pProgram3 : pCall)
     C                   CALLP     LIBLpgm(pProgram4 : pCall)

      ** If F3 was pressed in the search, exit.
     C                   If        pKC = *on
     C                   Eval      eRtnCode = '3'
     C                   Else
    
     C                   If        key <> *blanks
     C                   Clear                   eRtnCode
     C                   MoveL     key           eKey
     C                   Else
     C                   Eval      eRtnCode = 'C'
     C                   EndIf
    

     C                   EndIf

     c****               Eval      pProgram = *blanks
     C                   Eval      pCall = '3'
     C                   CALLP     LIBLpgm(pProgram : pCall)
     C                   CALLP     LIBLpgm(pProgram1 : pCall)
     C                   CALLP     LIBLpgm(pProgram2 : pCall)
     C                   CALLP     LIBLpgm(pProgram3 : pCall)
     C                   CALLP     LIBLpgm(pProgram4 : pCall)
    
     C                   Eval      *inLR = *on
