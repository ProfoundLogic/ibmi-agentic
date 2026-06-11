     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2006                   *
     *========================================================================*
    H/TITLE SSC04R - SIS cliams Search
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Allows the user to select a cliams .                                  *
     *                                                                        *
     * CALLED BY : VDARTS                                                     *
     *                                                                        *
     * CALLS  TO : CM01020R - SIS cliams Search                               *
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
     D search          c                   const('SSC04C')
     ** DARTS PNAM
     D scpnam          c                   const('SISCLM')
    
     ** Date & Time structure
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)

     ** PMSC Key structure
     D                 ds
     D siskey                        27
     D  sisco#                        2    overlay(siskey)
     D  sisprefix                     4    overlay(siskey:3)
     D  sispolicy                     9    overlay(siskey:7)
     D  sisbranch                     2    overlay(siskey:16)
     D  sisclaim                     10    overlay(siskey:18)
    

      *** 02/01/06 ***** add external ds for the SIS claims file ** made this in
      *** returns data, but not matching external claims file record.
     D                 ds
     D clds                         256
     D  occonr                        2A   overlay(clds)
     D  ocbrnc                        2A   overlay(clds:3)
     D  ocbtch                        3A   overlay(clds:5)
     D  occmnr                       10A   overlay(clds:8)
     D  ocprfx                        4A   overlay(clds:18)
     D  ocplnr                        9  0 overlay(clds:22)
     D  xblanks                     226    overlay(clds:31)

     ************--------------------------------------------------------

     D eKey            S             40a
     D eRtnCode        S              1a



     *========================================================================*
     * PLIST Definitions
     *========================================================================*
     * PLIST Definitions
     ** SIS  Search program parameters
     C     sisclm        plist
     C                   parm      'CMOCUR03'    sisflnm           8
     C                   parm      001           sisrow            3 0
     C                   parm      001           sisclmn           3 0
     C                   parm                    sisYYDTA        256
     C*
     *==================================================================*
    
     ** SIS  Search program parameters
     C     sispl         plist
     C                   parm                    siskey

    

     C     *entry        PList
     C                   Parm                    eKey
     C                   Parm                    eRtnCode

     *========================================================================*
     * Main
     *========================================================================*

     ** Call Work With Claims for a search.
     C                   exsr      CLMsrc

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
     c     CLMsrc        begsr
     **
     C                   CALL      SEARCH                               77
     c                   parm      *blanks       x#rtad            7
     c                   parm      *blanks       x#brnc            2
     c                   parm      *blanks       x#cmnr           10
     c                   parm      *blanks       x#mode            1
     c                   parm      *blanks       x#conr            2
     c                   parm      *blanks       x#prfx            4
     c                   parm      *zeros        x#plnr            9 0
     c                   parm      *blanks       x#opt             2
     c                   parm      *blanks       x#ssvals         51
     c*
      ** Break out the key fields
     C                   move      x#conr        sisco#
     C                   move      x#prfx        sisprefix
     C                   move      X#plnr        sispolicy
     C                   move      x#brnc        sisbranch
     C                   move      x#cmnr        sisclaim
    
     c*
     c*
    
     c*
     C                   endsr

