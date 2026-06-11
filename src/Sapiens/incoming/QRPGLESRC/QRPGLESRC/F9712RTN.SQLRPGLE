     h*  (C) Copyright Fiserv Insurance Solutions, Inc., 2007
     h**********************************************************************************************
     h/TITLE F9712RTN - TN EDI Workfile build
     h option(*nodebugio:*srcstmt)
     h**********************************************************************************************
     h*  DESCRIPTION:  This program will build the FAEOPTN work file        ords, and outputs an   *
     h*                                                                                            *
     h*  Called from:  F9711CTN - TN EDI Override                                                  *
     h*  Calls      :  None                                                                        *
     h*                                                                                            *
     h**********************************************************************************************
     h*  Created on 10/11/2007 by c:62 for Project TN004                                           *
     h**********************************************************************************************
     h* Revisions:                                                                                 *
     h*                                                                                            *
     h**********************************************************************************************
     d eFromDate       s              8
     d eToDate         s              8
     d  fromDate       s              8  0
     d  toDate         s              8  0
      *=============================================================================================
      * PLISTS
      *=============================================================================================
     c     *entry        plist
     c                   parm                    eFromDate
     c                   parm                    eToDate
     c*
     c                   move      eFromDate     fromDate
     c                   move      eToDate       toDate
     c*=============================================================================================
     c* Main Processing
     c*=============================================================================================
     c/EXEC SQL
     c+ INSERT INTO FAEOPTN (TNCO#, TNSUB#, TNFND, TNFYR,
     c+ TNCASE, TNSYS, TNBTYP, TNICN, TNEOB#, TNRCN)
     c+ (SELECT EOCO#, EOSUB#, EOFND, EOFYR,
     c+  EOCASE, EOSYS, EOBTYP,
     c+  CASE WHEN EUICN IS NOT NULL THEN
     c+  EUICN ELSE ' ' END,
     c+  EOEOB#, EARCN
     c+ FROM FAEOP JOIN FAEAP ON EOCO#=EACO# AND
     c+ EOSUB#=EASUB# AND EOFND=EAFND AND
     c+ EOFYR=EAFYR AND EOCASE=EACASE AND
     c+ EOEOB#=EAEOB# LEFT OUTER JOIN FAEUP ON
     c+ EOCO#=EUCO# AND EOSUB#=EUSUB# AND
     c+ EOFND=EUFND AND EOFYR=EUFYR AND
     c+ EOCASE=EUCASE AND EOEOB#=EUEOB#
     c+ WHERE EAWCPD >= :fromDate AND EAWCPD<=:toDate AND
     c+       EAEPFL='P' AND EOAST='TN')
     c/END-EXEC

     c                   seton                                        lr
