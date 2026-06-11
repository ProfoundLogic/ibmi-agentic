     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H Bnddir('STBNDDIR')
     H/TITLE f8190r
      //*************************************************************************
      //  Description:  837 Bill Import
      //
      //  Created: 09/27/17   Programmer C:180      Project: 114MJB
      //*************************************************************************
      //  Revisions:
      //
     FFA837FP   IF   E             DISK
      *
      *=========================================================================
      *  Entry
      *=========================================================================
     D F8190R          PR
     D                              100A
     D                                1A
     D F8190R          PI
     D  @@UniqueId                  100A
     D  @@Mode                        1A

      *=========================================================================
      *  Prototypes
      *=========================================================================
     D Validate        PR                  EXTPGM('F8191R')
     D                                1A
      *
     D ParseP          PR                  EXTPGM('F8192R')
     D                              100A
     D                                9A
     D                                1A
      *
     D ParseI          PR                  EXTPGM('F8192R2')
     D                              100A
     D                                9A
     D                                1A

     D ProvBills       PR                  EXTPGM('F8193R')
     D                                1A
     D                                9A
     D                                9P 0
     D                               12A
     D                                1A

     D GetSegment      PR             3
     D                              500A

     D ##RetCod        S              1A   inz('1')
     D rrnOne          S              9P 0 inz(1)
     D ##UniqueId      S            100A   inz
     D ##InCtl#        S              9A   inz
     D ##RetCode       S              1A   inz('1')

     D ##Mode          S              1A
     D ##Rrn           S              9P 0 inz
     D ##DcId          S             12A   inz
     D ##RetCd         S              1A   inz
     D strSeg          S              3A   inz
     D FrmPos          S              5  0 inz(1)
     D ToPos           S              5  0 inz
     D FldLen          S              5  0 inz
     D i               S              5  0 inz
     D idx             S              5  0 inz

      /Free
       //Just to use UniqueId
       ##UniqueId = @@UniqueId;

       Setll rrnOne FA837FP;
       Read FA837FP;
       Dow not %eof(FA837FP);
          //Get Segment Id
          StrSeg = GetSegment(FA837DATA);

          If StrSeg = 'GS';
             For i = 1 to 8;
                Idx = %scan('*':FA837data:FrmPos);
                FrmPos = Idx + 1;
             Endfor;
             ToPos = %checkr(' ':FA837data);
             FldLen = (ToPos - FrmPos) + 1;

             // Professional
             If %subst(FA837data:FrmPos:10) = '005010X222';
                //Validate data
                Validate(##RetCod);

                //Parse data
                If ##RetCod= *Off;
                   ParseP(##UniqueId:##InCtl#:##RetCode);
                Endif;

                //Interface to Provider Bills all except Image mode
                If ##RetCode = *Off and @@Mode <> 'I';
                   Eval ##Mode = @@Mode;
                   ProvBills(##Mode:##InCtl#:##Rrn:##DcId:##RetCd);
                Endif;

             Endif;

             // Institutional
             If %subst(FA837data:FrmPos:10) = '005010X223';
                //Validate data
                Validate(##RetCod);

                //Parse data
                If ##RetCod= *Off;
                   ParseI(##UniqueId:##InCtl#:##RetCode);
                Endif;

                //Interface to Provider Bills all except Image mode
                If ##RetCode = *Off and @@Mode <> 'I';
                   Eval ##Mode = @@Mode;
                   ProvBills(##Mode:##InCtl#:##Rrn:##DcId:##RetCd);
                Endif;
             Endif;

          Endif;

          Read FA837FP;
       Enddo;

       *inlr = *on;
      /End-Free
      *=====================================================================
      * Get Segment
      *=====================================================================
     P GetSegment      B
     D GetSegment      PI             3A
     D  segData                     500A

     D  strSeg         S              3A
      /Free
       strSeg = %Subst(segData:1:3);

       If %Subst(strSeg:3:1) = '*';
          strSeg = %Subst(strSeg:1:2) ;
       Endif;

       Return strSeg;
      /End-Free
     P GetSegment      E
