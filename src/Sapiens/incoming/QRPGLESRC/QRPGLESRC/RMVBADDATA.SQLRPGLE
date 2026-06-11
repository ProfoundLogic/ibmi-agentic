     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
      *============================================================================================*
      /title RMVBADDATA - Remove bad data from darts text fields                                   *
     H option(*nodebugio: *srcstmt)                                                                *
     h DFTACTGRP(*no) ACTGRP('QILE')
     H debug(*yes)                                                                                 *
      *============================================================================================*
      * Created on 06/06/23 by c:62 for Ticket 110176(133)
      *=============================================================================================
      * Revisions:                                                                                 *
      *                                                                                            *
      *=============================================================================================
          // Remove the following characters
          // '|'   = Pipe
          // x'00' = NULL
          // x'OD' = CR
          // x'OA' = LF
          // "     = Double Quotes

          // Clean SDCMP
          Exec Sql
           Update sdcmp
           Set CMcomm = Replace(CMcomm,'|','');
          Exec Sql
           Update sdcmp
           Set CMcomm = Replace(CMcomm,x'00','');
          Exec Sql
           Update sdcmp
           Set CMcomm = Replace(CMcomm,x'0D','');
          Exec Sql
           Update sdcmp
           Set CMcomm = Replace(CMcomm,x'0C','');
          Exec Sql
           Update sdcmp
           Set CMcomm = Replace(CMcomm,'"','');

          // Clean SMARP
          Exec Sql
           Update smarp
           Set ARcm75 = Replace(ARcm75,'|','');
          Exec Sql
           Update smarp
           Set ARcm75 = Replace(ARcm75,x'00','');
          Exec Sql
           Update smarp
           Set ARcm75 = Replace(ARcm75,x'0D','');
          Exec Sql
           Update smarp
           Set ARcm75 = Replace(ARcm75,x'0C','');
          Exec Sql
           Update smarp
           Set ARcm75 = Replace(ARcm75,'"','');

          // Clean SMCLP
          Exec Sql
           Update smclp
           Set CLclob = Replace(CLclob,'|','');
          Exec Sql
           Update smclp
           Set CLclob = Replace(CLclob,x'00','');
          Exec Sql
           Update smclp
           Set CLclob = Replace(CLclob,x'0D','');
          Exec Sql
           Update smclp
           Set CLclob = Replace(CLclob,x'0C','');
          Exec Sql
           Update smclp
           Set CLclob = Replace(CLclob,'"','');

          *inLR = *On;
          Return;
