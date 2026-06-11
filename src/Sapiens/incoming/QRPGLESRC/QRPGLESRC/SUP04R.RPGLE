     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2002                   *
     *========================================================================*
    H/TITLE SUP04R - Summit Copy/Transfer Clnt  Search
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Allows the user to select a POLICY. If one is selected, it returns    *
     *  the DARTS key for that Clnt , otherwise it returns an error code.     *
     *                                                                        *
     *                                                                        *
     * CALLED BY : VDARTS                                                     *
     *                                                                        *
     *                                                                        *
     * CALLS  TO : MBR215 Summit POLICY Search program.                       *
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
     D @key                          15a
     D @Dft                           3a   Overlay(@key:1)
     D @Carr                          3a   Overlay(@Key:4)
     D @Clnt                          5a   Overlay(@key:7)
     D @Year                          3a   Overlay(@key:12)
     D                 DS
     D RtnKey                        40a
     D RDft                           3a   Overlay(RtnKey:1)
     D RCarr                          3a   Overlay(RtnKey:4)
     D RClnt                          5a   Overlay(RtnKey:7 )
     D RYear                          3a   Overlay(RtnKey:12)
     I*
     *========================================================================*
     * Prototypes
     *========================================================================*
     D LIBLpgm         PR                  ExtPgm('SC916C1')
     D  program                      10a
     D  call                          1a

     D SearchPgm       PR                  ExtPgm('MBR215')
     D  gclass                        1a
     D  gCarr                         4a
     D  gClnt                         5a
     D  gYear                         3a


     D
     D

     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D  key            S             40a
     D eKey            S             40a
     D eRtnCode        S              1a
     D zCarr           S              4a

     D pCall           S              1a
     D pKC             S              1a   Inz(*off)
     D pKL             S              1a   Inz(*off)
     D pMode           S              1a   Inz(*blank)
     D pRRN            S              9  0 Inz(*zeros)
     D pRRN1           S              9  0 Inz(*zeros)
     D pQuote          S              1    Inz('W')
     D pView           S              1  0 Inz(*zero)
     D pRRN3           S              9  0 Inz(*zeros)
     D pProgram        S             10a   Inz('MBR215')
     D************** NOT USED (Pprogram1-4) ****************8
     D pProgram1       S             10a   Inz('SCI070')
     D pProgram2       S             10a   Inz('SCI060')
     D pProgram3       S             10a   Inz('SCI080')
     D pProgram4       S             10a   Inz('WCS210')
     D*
     D  @class         S              1a
     D**@Carr          S              4a
     D**@clnt          S              5a
     D**@key           S             13a

      ** Key structure
     D                 DS
     D*key                           25a
     D* @class                        1a   Overlay(key:1) inz
     D* @Carr                         1a   Overlay(key:2) inz
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

     ** Call Work With Clnt s for a search.
     C                   Eval      pMode = '2'
     C                   Eval      @key = *blanks
     C                   Eval      @class = '8'
     C                   Eval      ZCarr  = *blanks
     C                   Eval      @Clnt  = *blanks
     C                   CALLP     SearchPgm(@class: zCarr: @Clnt: @Year)
     C                   move      ZCarr         @carr
     C*
     C                   Eval      RDft = '001'
     C                   Eval      RCarr = @Carr
     C                   Eval      RClnt = @Clnt
     C     @year         ifne      *blanks
     C                   Eval      RYear = @Year
     C                   else
     C                   Eval      RYear = *blanks
     C                   endif
     C                   Move      *all'0'       RClnt
     C                   MOVE      @Clnt         RClnt
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

     C****               Eval      pProgram = *blanks
     C                   Eval      pCall = '3'
     C                   CALLP     LIBLpgm(pProgram : pCall)
     C                   CALLP     LIBLpgm(pProgram1 : pCall)
     C                   CALLP     LIBLpgm(pProgram2 : pCall)
     C                   CALLP     LIBLpgm(pProgram3 : pCall)
     C                   CALLP     LIBLpgm(pProgram4 : pCall)
    
     C                   Eval      *inLR = *on
