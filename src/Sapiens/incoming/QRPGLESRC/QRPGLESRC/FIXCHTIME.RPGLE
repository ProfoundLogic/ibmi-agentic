     A*************************************************************************
     A*Copyright('(c) 2024 Sapiens. All Rights Reserved.')
     A*************************************************************************
     A*  FIXCHTIME - Fix Claim Histroy time to be unique                      *
     A*                                                                       *
     A*************************************************************************
     A* Created on 04/29/2024 by c:62 for Ticket 115523(133)                  *
     A*************************************************************************
     A* Revisions:                                                           *
     A*
     A*************************************************************************
     fwmchl     uf   e           k Disk    ReName(pch01:wch00)
     ffmchl     uf   e           k Disk    ReName(pch01:fch00)

     D datDate         s               d   DatFmt(*usa)
     D timTime         s               t
     D timeStamp       s               z

     d saveDate        s              7  0 Inz(0)
     d saveTime        s              6  0 Inz(0)
     d seconds         s              5  0 Inz(0)

       // Claim History (WCMS)
       seconds  = 0;
       saveDate = 0;
       saveTime = 0;
       Read wch00;
       DoW Not %Eof(wmchl);
          If saveDate = CHefDt And saveTime = CHtime;
             seconds  += 1;
             timtime   = %Time(CHtime);
             datDate   = %Date(CHefDt:*cymd);
             timeStamp = datDate + timTime;
             timeStamp = timeStamp + %Seconds(seconds);
             timTime   = %Time(timeStamp);
             datDate   = %Date(timeStamp);
             CHefDt    = %Dec(datDate:*cymd);
             CHdt      = %Dec(datDate:*cymd);
             CHtime    = %Dec(timtime);
          Else;
             seconds   = 0;
             saveDate  = CHefDt;
             saveTime  = CHtime;
          EndIf;
          Update wch00;
          Read   wch00;
       EndDo;

       // Case History (WCMC)
       seconds  = 0;
       saveDate = 0;
       saveTime = 0;
       Read fch00;
       DoW Not %Eof(fmchl);
          If saveDate = CHefDt And saveTime = CHtime;
             seconds  += 1;
             timtime   = %Time(CHtime);
             datDate   = %Date(CHefDt:*cymd);
             timeStamp = datDate + timTime;
             timeStamp = timeStamp + %Seconds(seconds);
             timTime   = %Time(timeStamp);
             datDate   = %Date(timeStamp);
             CHefDt    = %Dec(datDate:*cymd);
             CHiDt     = %Dec(datDate:*cymd);
             CHtime    = %Dec(timtime);
             CHiTim    = %Dec(timtime);
          Else;
             seconds  = 0;
             saveDate = CHefDt;
             saveTime = CHtime;
          EndIf;
          Update fch00;
          Read   fch00;
       EndDo;

       *inLR = *On;
