     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2006                   *
     *========================================================================*
    H/TITLE SSP04R - SIS Policy Search
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Allows the user to select a POlicy .                                  *
     *                                                                        *
     * CALLED BY : VDARTS                                                     *
     *                                                                        *
     * CALLS  TO : CI00744R - SIS policy Search                               *
     *             SC916C1 - Set up LIBL for searches                         *
     *                                                                        *
     * CREATED   : 12/14/2006                                                 *
     * PROGRAMMER: C:002                                                      *
     * PROJECT   : sup56273                                                   *
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

     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
      ** Application name for policy search
     D search          c                   const('CI00744R')

      ** DARTS PNAM
     D scpnam          c                   const('SISPOL')

      ** Date & Time structure
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsmoda                        4  0 overlay(dsdttm:7)
     D  dsyear                        2  0 overlay(dsdttm:13)
     D  dsdate                        8  0 overlay(dsdttm:7)

      ** SIS Key structure
     D                 ds
     D siskey                        40
     D  sisco#                        2    overlay(siskey)
     D  sisprefix                     4    overlay(siskey:3)
     D  sispolicy                     9    overlay(siskey:7)

     D sisclid         s             12
     D sisoption       s              2
     D sisasofdt       s              6
     D sisroute        s              7

     ************--------------------------------------------------------

     D eKey            S             40a
     D eRtnCode        S              1a



     *========================================================================*
     * PLIST Definitions
     *========================================================================*
      ** SIS  Search program parameters
     C     sispl         plist
     C                   parm                    sisclid
     C                   parm                    sisco#
     C                   parm                    sisprefix
     C                   parm                    sispolicy
     C                   parm                    sisoption
     C                   parm                    sisasofdt
     C                   parm                    sisroute
     C*

     C     *entry        PList
     C                   Parm                    eKey
     C                   Parm                    eRtnCode

     *========================================================================*
     * Main
     *========================================================================*

     ** Call Work With Claims for a search.
     C                   exsr      polsrc

     C     siskey        ifeq      *blanks
     C                   eval      eRtnCode = '3'
     C                   else
    
     C                   Clear                   eRtnCode
     C                   MoveL     siskey        eKey

     C                   EndIf

    
     C                   Eval      *inLR = *on
      *===============================================================*
      *  POlsrc (SIS policy search)
      *===============================================================*
     C     Polsrc        begsr

     C                   time                    dsdttm
     C                   movel     dsyear        sisasofdt
     C                   move      dsmoda        sisasofdt
     C                   Eval      sisclid = *blanks
     C                   Eval      sisco# = *blanks
     C                   Eval      sisprefix = *blanks
     C                   Eval      sispolicy = *blanks
     C                   Eval      sisoption = *blanks
     C                   Eval      sisroute = *blanks

      ** Call the search program
     C                   call      search        sispl

     C                   endsr

