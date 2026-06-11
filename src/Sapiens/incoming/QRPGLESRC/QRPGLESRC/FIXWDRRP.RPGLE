      ** (C) Copyright Progressive Data Solutions, Inc., 1999
     h/title FIXWDRRP: Load the Aircraft Surcharge Premium Level Flag
      **============================================================**
      ** Program Description:                                       **
      **   This program loads the Aircraft Surcharge Premium Level  **
      **   Flag with an 'E'.                                        **
      **                                                            **
      ** Called from: Tape post load program                        **
      **                                                            **
      ** Calls to:    None                                          **
      **                                                            **
      ** Programmer: C:113                                          **
      ** Created   : 08/09/99                                       **
      ** Project   : CM045                                          **
      **============================================================**
     hdatedit(*ymd) datfmt(*ymd)

     fWDRRP     uf   e             disk

     fQSYSPRT   o    f   80        printer                                      Output

      *=============================================================================================
      *  Data Structure Definitions
      *=============================================================================================
      ** External data structure
     d psds          esds                  extname(szq1p)                       External prog info

     d xxWrkstor       ds
     d xxDate                         8s 0 inz
     d  xxYear                        4s 0 overlay(xxDATE:1)                         Year
     d  xxMonth                       2s 0 overlay(xxDATE:5)                         Month
     d  xxDay                         2s 0 overlay(xxDATE:7)                         Day

      *=============================================================================================
      *  Field Definitions
      *=============================================================================================
      ** Value to load
     d e               c                   const('E')
     d count           s              5p 0 inz(0)

      *=============================================================================================
      *  Main
      *=============================================================================================
     c                   eval      xxDATE=*date
     c                   eval      count = 0

     c                   read      WDRRP                                  60

     c                   DOW       *in60 = *off

     c                   IF        RRASFL = *blanks
     c                   eval      RRASFL = e
     c                   eval      count = count + 1
     c                   update    PRR01
     c                   ENDIF                                                  RRASFL = *blanks

     c                   read      WDRRP                                  60
     c                   ENDDO                                                  *in60 = *off

     c                   IF        count = 0
     c                   seton                                        90
     c                   ENDIF                                                  count = 0

     c                   except    ##Detail

     c                   seton                                        lr
     c                   return

      *=============================================================================================
      *  Printer Output
      *=============================================================================================
      ** Page header
     oqsysprt   e            ##Detail       1  2
     o                       q1pgm               10
     o                                           46 'Load Aircraft Surcharge Pr'
     o                                           62 'emium Level Flag'
     o                       q1user              80

     o          e            ##Detail       2
     o                       xxMonth              2
     o                                            3 '/'
     o                       xxDay                5
     o                                            6 '/'
     o                       xxYear              10
     o                                           74 'Page'
     o                       PAGE          4     80

     o          e            ##Detail       2
     o                                           21 'Records changed:'
     o              N90      count         z     25
     o               90                          27 '*NONE'



