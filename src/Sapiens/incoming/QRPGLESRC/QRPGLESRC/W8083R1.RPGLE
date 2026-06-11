     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*
    H/TITLE W8083R1 - HES Eligibilty File Last Run Date Program
     H DFTACTGRP(*NO) ACTGRP(*caller)
    H*****************************************************************
    H*  Program Description:                                         *
    H*     Retrieves the date from the data structure HEDATA and     *
    H*     passes it back to the calling program                     *
    H*                                                               *
    H*  Called from: W8083C1 - HES Eligibility Override CL           *
    H*  Calls to   : None                                            *
    H*                                                               *
    H*****************************************************************
    H*  Created on 11/03/2009 by c:227 for Project 135MMX from clt135*
    H*****************************************************************
    H*  REVISIONS:                                                   *
    H*                                                               *
    H*****************************************************************
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    D*
    D*
     D hedata         uds
     D xxdate                        10a
    D*

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
     C                   eval      *inlr = *on
