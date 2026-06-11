      * (C) Copyright Fiserv Insurance Solutions, Inc., 2008                                       *
      *============================================================================================*
      /title FIXDEDUCT1 - Fix program that calls W8023C (Write WADQP)                              *
     h option(*nodebugio: *srcstmt)                                                                *
     h debug(*yes)                                                                                 *
      *============================================================================================*
      *  This program accepts the co# and date range.  It begins on date1 and ends on date2
      *  calling W8023C for each date within the range including the end dates.
      *============================================================================================*
     d*entry
     d FIXDEDUCT1      pr
     d  @@co#                         3
     d  @@begDt                       8
     d  @@endDt                       8
     d FIXDEDUCT1      pi
     d  @@co#                         3
     d  @@begDt                       8
     d  @@endDt                       8

     dW8023C           PR                  extpgm('W8023C')
     d callCO#                        3    const
     d callDate                       8    const

     d  usaDate        s               d   datfmt(*usa)
     d  usaToDt        s               d   datfmt(*usa)
     d  callCo#        s              3
     d  callDate       s              8

     c                   move      @@co#         callCo#
     c     *usa0         move      @@begDt       usaDate
     c     *usa0         move      @@endDt       usaToDt
     c                   dow       usaDate <= usaToDt
     c     *usa0         move      usaDate       callDate
     c                   callp     W8023C (callCo# : callDate)
     c                   adddur    1:*days       usaDate
     c                   enddo
     c                   move      *on           *inlr
