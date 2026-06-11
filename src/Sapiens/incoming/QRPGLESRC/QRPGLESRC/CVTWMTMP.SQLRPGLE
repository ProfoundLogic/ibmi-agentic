     H* Copyright('(c) 2022 Sapiens. All Rights Reserved.')
     H**********************************************************************
     H/TITLE CVTWMTMP   - Program to combine 2 transmittal record
     H/TITLE              into one
     H DFTNAME(CVTWMTMP)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP('QILE')
     h**********************************************************************
      *===============================================================*
      *  Created   : 03/09/22                                         *
      *  Programmer: C:340                                            *
      *  Project   : WC2843                                           *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*
     d*
     d* Data Structures
     d*
     d PsDs          ESDS                  ExtName(SZQ1P)

     d* Stanalone Variables
     D dtTodayMDY      S               D   DATFMT(*MDY)
     D intDateCYYMMDD  S              7S 0
     D intMissComp     S              7S 0

      /Free

       // Main program processing

       // Convert Current Date in required format
       dtTodayMDY    = %Date();
       intDateCYYMMDD = %int(%Char(dtTodayMDY : *CYMD0)) ;

       Exec Sql Set Option Commit = *None;

       // For all WCSTAT (TMFLAG='S'), Move Default Contact Details to New WCSTAT Contact Fields

       Exec sql
        Update WMTMP
        Set  TMSMail = TMMail    ,
             TMSName = TMName    ,
             TMSPhn# = TMPhn#    ,
             TMSExt# = TMExt#    ,
             TMSFax# = TMFax#    ,
             TMSAdr1 = TMAdr1    ,
             TMSCty  = TMCty     ,
             TMSSt   = TMSt      ,
             TMSZip  = TMZip     ,
             TMUsr   = :Q1User    ,
             TMPgm   = :Q1Pgm     ,
             TMDat   = :intDateCYYMMDD ,
             TMTim   = :Q1Tcom
        Where TMFlag = 'S' ;

       // For all WCSTAT (TMFlag = 'S'), Clear WCPOLS Contact Details
       Exec sql
        Update WMTMP
        Set  TMMail = '' ,
             TMName = '' ,
             TMPhn# = 0  ,
             TMExt# = 0  ,
             TMFax# = 0  ,
             TMAdr1 = '' ,
             TMCty  = '' ,
             TMSt   = '' ,
             TMZip  = 0
        Where TMFlag = 'S'  ;

       // If there are 2 records(One with TMFLAG='P' & TMFLAG='S')
       // For all WCPOLS (TMFLAG='P'),  Update WCSTAT Contact Details from Contact details
       //                               of TMFLAG='S' records for same CO#
       Exec sql
        Update WMTMP A
        Set    A.TMSMail = (Select B.TMSMail From WMTMP B
                            Where  A.TMCo# = B.TMCo# and B.TMFlag = 'S') ,
               A.TMSName = (Select B.TMSName From WMTMP B
                            Where  A.TMCo# = B.TMCo# and B.TMFlag = 'S') ,
               A.TMSPhn# = (Select B.TMSPhn# From WMTMP B
                            Where  A.TMCo# = B.TMCo# and B.TMFlag = 'S') ,
               A.TMSExt# = (Select B.TMSExt# From WMTMP B
                            Where  A.TMCo# = B.TMCo# and B.TMFlag = 'S') ,
               A.TMSFax# = (Select B.TMSFax# From WMTMP B
                            Where  A.TMCo# = B.TMCo# and B.TMFlag = 'S') ,
               A.TMSAdr1 = (Select B.TMSAdr1 From WMTMP B
                            Where  A.TMCo# = B.TMCo# and B.TMFlag = 'S') ,
               A.TMSCty  = (Select B.TMSCty  From WMTMP B
                            Where  A.TMCo# = B.TMCo# and B.TMFlag = 'S') ,
               A.TMSST   = (Select B.TMSSt   From WMTMP B
                            Where  A.TMCo# = B.TMCo# and B.TMFlag = 'S') ,
               A.TMSZip  = (Select B.TMSZip  From WMTMP B
                            Where  A.TMCo# = B.TMCo# and B.TMFlag = 'S')
        Where A.TMFlag = 'P' and
              Exists (Select 1 from WMTMP B
                      Where A.TMCo# = B.TMCo# and B.TMFlag = 'S')         ;

       // If there are 2 records(One with TMFlag='P' & TMFlag='S')
       // Delete record with TMFLAG='S'
       Exec sql
        Delete From WMTMP A
        Where  A.TMFlag = 'S' and
               Exists (Select 1 from WMTMP B
                       Where  A.TMCo# = B.TMCo# and B.TMFlag = 'P')       ;

       // Add Records for Companies for which there no records on WMTMP

       Exec sql
        Select count(*) into :intMissComp From SMCOL A
        Where Not Exists (Select 1 from WMTMP B Where B.TMCo# = A.COCo#)  ;


       If intMissComp > 0;
          Exec sql
           Insert into WMTMP (TMCo#, TMCar#, TMMail, TMName, TMPhn#, TMExt#,
           TMFax# ,TMAdr1, TMCty, TMSt, TMZip, TMDType, TMFein, TMsMail,
           TMsName, TMsPhn#, TMsExt#, TMsFax# ,TMsAdr1, TMsCty, TMsSt, TMsZip,
           TMsDType, TMsFein, TMFlag, TMUsr, TMPgm ,TMDat ,TMTim, TMIUsr,
           TMIPgm, TMIDat, TMITim, TMApnd)
           Values (
           (Select COCo# From SMCOL A
            Where Not Exists (Select 1 from WMTMP B Where B.TMCo# = A.COCo#)),
           0,'','',0,0,0,'','','',0,'',0,'','',0,0,0,'','','',0,'',0,'',
            :Q1User, :Q1Prpg, :intDateCYYMMDD, :Q1TCOM,
            :Q1User, :Q1Prpg, :intDateCYYMMDD, :Q1TCOM,'');
       EndIf;


       // Update All records to clear TMFLAG
       Exec sql
        Update WMTMP
        Set  TMFlag = '' Where TMFlag <> ' ';

       // Update All records to clear TMFLAG
       Exec sql
        Update WMTMP
        Set  TMDType = 'C', TMsDtype = 'C' Where TMCar# = 0 ;

       *InLr = *On;

      /End-Free
