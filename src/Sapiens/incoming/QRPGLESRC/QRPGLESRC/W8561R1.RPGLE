    H* (C) Copyright Fiserv Insurance Solutions, Inc., 2008
    H*
    H/TITLE W8561R1 - Retrieve date last run
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H*****************************************************************
    H*  Program Description:                                         *
    H*     Retrieves the date from the data structure XPDATA and     *
    H*     passes it back to the calling program                     *
    H*                                                               *
    H*  Called from: W8561C - Provider Export - Override Program     *
    H*  Calls to   : None                                            *
    H*                                                               *
    H*****************************************************************
    H*  Created on 02/08/2007 by c:47 for Project 129MQN             *
    H*    Copied from W8461R1.                                       *
    H*****************************************************************
    H*  Revisions:                                                   *
    H*                                                               *
    H*****************************************************************
    D*
     D xpdata         uds
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
