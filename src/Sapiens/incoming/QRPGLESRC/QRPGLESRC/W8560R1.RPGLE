     * (C) Copyright Fiserv Insurance Solutions, Inc., 2008
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     *
    H/TITLE W8560R1 - Retrieve date last run
     *****************************************************************
     *  Program Description:                                         *
     *     Retrieves the date from the data structure XCDATA and     *
     *     passes it back to the calling program                     *
     *                                                               *
     *  Called from: W8560C - Claim Export - Override Program        *
     *  Calls to   : None                                            *
     *                                                               *
     *****************************************************************
     *  Created on 02/06/2008 by c:47 for Project 129MQN             *
     *    Copied from W8460R1.                                       *
     *****************************************************************
     *  Revisions:                                                   *
     *                                                               *
     *****************************************************************
    D*
     D xcdata         uds
     D xxdata                        16a

     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    
     D                 ds
     D data                          16
     D  xxdate                       10    overlay(data)

     D date_mdyy       s               d   inz datfmt(*usa)
     D date_mdyy2      s               d   inz datfmt(*usa)
     D date_yymd       s               d   inz datfmt(*iso)

     D @@dt            s              7a
     D xxdt            s              7s 0

    C*****************************************************************
    C* Parameter Lists
    C*****************************************************************
     C     *entry        plist
     C                   parm                    @@dt
    C*
     C                   time                    dsdttm
     C                   move      dsdate        date_mdyy2
     C                   move      xxdata        data
    C*
     C                   if        dstime < 120000
     C                   subdur    1:*days       date_mdyy2
     C                   endif
    C*
     C                   if        xxdate <> *blanks
     C                   move      xxdate        date_mdyy
     C                   if        date_mdyy = date_mdyy2
     C                   subdur    1:*days       date_mdyy
     C                   endif
     C     *cymd         move      date_mdyy     xxdt
     C                   move      xxdt          @@dt
     C                   endif
    C*
     C                   seton                                        lr
