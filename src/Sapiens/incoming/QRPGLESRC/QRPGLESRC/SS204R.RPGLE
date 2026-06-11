    H*========================================================================*
    H* (C) Copyright StoneRiver Inc., 2009                                    *
    H*========================================================================*
    H/TITLE SS204R - SIS claims Search
    H*========================================================================*
    H* Program Description:                                                   *
    H*  Allows the user to select a cliams .                                  *
    H*                                                                        *
    H* Called by : VDARTS                                                     *
    H* Calls  to : CM01020R - SIS cliams Search                               *
    H*                                                                        *
    H*========================================================================*
    H* Created on 04/06/2009 by c:62 for Project 115IAK                       *
    H*========================================================================*
    H* REVISIONS:                                                             *
    H*                                                                        *
    H*                                                                        *
    H*========================================================================*
     *========================================================================*
     * Prototypes
     *========================================================================*

     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D search          c                   const('SS204C')
     ** DARTS PNAM
     D scpnam          c                   const('SISCL2')
    
     ** Date & Time structure
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)

     ** PMSC Key structure
     D                 ds
     D siskey                        27
     D  sisclaim                     10    overlay(siskey)

      *** 02/01/06 ***** add external ds for the SIS claims file ** made this in
      *** returns data, but not matching external claims file record.
     D                 ds
     D clds                         256
     D  occonr                        2A   overlay(clds)
     D  ocbrnc                        2A   overlay(clds:3)
     D  ocbtch                        3A   overlay(clds:5)
     D  ocbat#                        3A   overlay(clds:8)
     D  occmnr                       10A   overlay(clds:11)
     D  ocprfx                        4A   overlay(clds:21)
     D  ocplnr                        9  0 overlay(clds:25)
     D  xblanks                     223    overlay(clds:34)

     ************--------------------------------------------------------

     D eKey            S             40a
     D eRtnCode        S              1a



     *========================================================================*
     * PLIST Definitions
     *========================================================================*
     * PLIST Definitions
     ** SIS  Search program parameters
     C     sisclm        plist
     C                   parm      'CMOCUR01'    sisflnm           8
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
     c** Break out the key fields
     c*
     C                   move      x#cmnr        sisclaim
     c*
     C                   endsr

