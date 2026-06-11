     h*  (C) Copyright Fiserv Insurance Solutions, Inc., 2008
     h**********************************************************************************************
     h/TITLE CLREOBS - Clear EOB files for electronically imported bills
     h option(*nodebugio:*srcstmt)
     h**********************************************************************************************
     h*  DESCRIPTION:  This program will clear files that were populated by ords, and outputs an   *
     h*                the medical bill upload                              ords, and outputs an   *
     h*                                                                                            *
     h*  Called from:  Command line                                                                *
     h*  Calls      :  None                                                                        *
     h*                                                                                            *
     h**********************************************************************************************
     h*  Created on 10/09/2008 by c:79 for Project 129MQN                                          *
     h**********************************************************************************************
     h* Revisions:                                                                                 *
     h*                                                                                            *
     h**********************************************************************************************
     d eFromDate       s              7
     d eToDate         s              7
     d  fromDate       s              7  0
     d  toDate         s              7  0
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
     C+ DELETE FROM PDSTST/FAEOP WHERE EOIPGM = 'F8582R' AND EOIDT >=
     C+ :FROMDATE AND EOIDT <= :TODATE
     c/END-EXEC
     c/EXEC SQL
     C+ DELETE FROM PDSTST/FAEAP WHERE EAIPGM = 'F8582R' AND EAIDT >=
     C+ :FROMDATE AND EAIDT <= :TODATE
     c/END-EXEC
     c/EXEC SQL
     C+ DELETE FROM PDSTST/FAESP WHERE ESIPGM = 'F8582R' AND ESIDT >=
     C+ :FROMDATE AND ESIDT <= :TODATE
     c/END-EXEC
     c/EXEC SQL
     C+ DELETE FROM PDSTST/FAEHP WHERE EHIPGM = 'F8582R' AND EHIDT >=
     C+ :FROMDATE AND EHIDT <= :TODATE
     c/END-EXEC
     c/EXEC SQL
     C+ DELETE FROM PDSTST/FAEUP WHERE EUIPGM = 'F8582R' AND EUIDT >=
     C+ :FROMDATE AND EUIDT <= :TODATE
     c/END-EXEC
     c/EXEC SQL
     C+ DELETE FROM PDSTST/FAEPP WHERE EPIPGM = 'F8582R' AND EPIDT >=
     C+ :FROMDATE AND EPIDT <= :TODATE
     c/END-EXEC
     c/EXEC SQL
     C+ DELETE FROM PDSTST/FAEDP WHERE EDIPGM = 'F8582R' AND EDIDT >=
     C+ :FROMDATE AND EDIDT <= :TODATE
     c/END-EXEC
     c/EXEC SQL
     C+ DELETE FROM PDSTST/FAEEP WHERE EEIPGM = 'F8582R' AND EEIDT >=
     C+ :FROMDATE AND EEIDT <= :TODATE
     c/END-EXEC
     c/EXEC SQL
     C+ DELETE FROM PDSTST/FDCAP WHERE CAIPGM = 'F8582R' AND CAIDT >=
     C+ :FROMDATE AND CAIDT <= :TODATE
     c/END-EXEC
     c/EXEC SQL
     C+ DELETE FROM PDSTST/FDDAP WHERE DAIPGM = 'F8582R' AND DAIDT >=
     C+ :FROMDATE AND DAIDT <= :TODATE
     c/END-EXEC
     c/EXEC SQL
     C+ DELETE FROM PDSTST/FDDEP WHERE DEIPGM = 'F8582R' AND DEIDT >=
     C+ :FROMDATE AND DEIDT <= :TODATE
     c/END-EXEC
     c/EXEC SQL
     C+ DELETE FROM PDSTST/FDDHP WHERE DHIPGM = 'F8582R' AND DHIDT >=
     C+ :FROMDATE AND DHIDT <= :TODATE
     c/END-EXEC
     c/EXEC SQL
     C+ DELETE FROM PDSTST/FDDUP WHERE DUIPGM = 'F8582R' AND DUIDT >=
     C+ :FROMDATE AND DUIDT <= :TODATE
     c/END-EXEC
     c/EXEC SQL
     C+ DELETE FROM PDSTST/FDDPP WHERE DPIPGM = 'F8582R' AND DPIDT >=
     C+ :FROMDATE AND DPIDT <= :TODATE
     c/END-EXEC
     c/EXEC SQL
     C+ DELETE FROM PDSTST/FDDDP WHERE DDIPGM = 'F8582R' AND DDIDT >=
     C+ :FROMDATE AND DDIDT <= :TODATE
     c/END-EXEC
     c/EXEC SQL
     C+ DELETE FROM PDSTST/FDMGP WHERE MGIPGM = 'F8582R' AND MGIDT >=
     C+ :FROMDATE AND MGIDT <= :TODATE
     c/END-EXEC
     c/EXEC SQL
     C+ DELETE FROM PDSTST/WARPP WHERE RPIPGM = 'F8582R' AND RPIDT >=
     C+ :FROMDATE AND RPIDT <= :TODATE
     c/END-EXEC

     c                   seton                                        lr
