     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/Define CONVERT
      ******************************************************************
      ******************************************************************
     H/TITLE N96DXR - NOVACARE NETWORK ALGORITHM
      ******************************************************************
      *  This program is called by N9600R to process physician and     *
      *  bills according to the NovaCare Physical Therapy contractual  *
      *  agreement                                                     *
      *                                                                *
      *   CALLED FROM:                                                 *
      *     N9600R                                                     *
      *                                                                *
      *   PROGRAMMER: C:212              PROJECT: 114MRC               *
      *   CREATED: 03/18/15                                            *
      ******************************************************************
      *   REVISIONS:                                                   *
      *                                                                *
      *   07/18/23 C:342 Project: NY060                                *
      *                  NY Compliance: CARC/RARC Update               *
      *                                                                *
      *   11/30/22 C:079  Support: 114/103413                          *
      *                   Pay up to full daily allowance               *
      *                                                                *
      *   03/03/17 C:309  Project: 114MEZ                              *
      *                   Modify the NovaCare DX Discount Algorithm    *
      *                   by adding new codes                          *
      *                                                                *
      *   05/06/16 C:212  Project: 114MYZ                              *
      *                   add option for flat amount for ReEval in DSPF*
      *                   and related processing                       *
      *                   remove default values for pct/flat amouts    *
      *                   fix exception record logic                   *
      *                                                                *
      *   12/01/15 C: 079 Support: 114/38641                           *
      *                   Spread payments accross PT Procedures        *
      *                                                                *
      *   09/14/15 C: 079 Support: 114/37170                           *
      *                   Correct contract end date logic              *
      *                                                                *
      ******************************************************************
      *** INDICATOR USAGE:
      ******************************************************************
     Fnmbdl2    if   E           K DISK
     Fndcol1    if   E           K DISK
     Fndcol5    if   E           K DISK    rename(pco01:pco02)
     Ffdcalh    UF   E           K DISK
     Ffdcal29   uF   E           K DISK    rename(pca01:pca29)
     Ffaeolg    if   E           K DISK
     Ffddhl     if   E           K DISK
     Ffddul     if   E           K DISK
     Ffms4l2    if   E           K DISK    rename(ps401:ps402)
     Ffms4l3    if   E           K DISK    rename(ps401:ps403)
     Ffddep     O    E             DISK
     Ffddel     if   E           K DISK    rename(pde01:pde02)
     Ffteblnj   if   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     D                 DS
     D  dsDtTm                 1     12  0
     D  dsTime                 1      6  0
     D  dsDate                 7     12  0

     D                 ds
     D arrFdcap                      14s 0 Dim(999) Ascend
     D   decpaid                      9s 0 Overlay(arrFdcap)
     D   decDseq                      5s 0 Overlay(arrFdcap:*next)

     D xxprcd          s              5    inz
     D xxeob#          s              9  0 inz
     D xxCo#           s              3  0 inz
     D xxsub#          s              5  0 inz
     D xxfund          s              3  0 inz
     D xxFyr           s              3  0 inz
     D xxCase          s              7  0 inz
     D xxFein          s              9  0 inz
     D xxMod           s              3  0 inz
     D xxNwCd          s              5    inz
     D x1FDat          s              8  0 inz
     D xxDseq          s              5  0 inz
     D first           s              1n   inz(*off)

     D errind          s              1    inz
     D lsvldr          s              1    inz

     D maxamtEV        s              9p 2 inz
     D avlamtEV        s              9p 2 inz
     D maxpctRE        s              9p 2 inz
     D maxamtRE        s              9p 2 inz
     D avlamtRE        s              9p 2 inz
     D totFSRE         s             11p 2 inz
     D xxContDt        s                   like(COefdt) inz
     D maxamtPT        s              9p 2 inz
     D maxpctPT        s              9p 2 inz
     D avlamtPT        s              9p 2 inz
     D totFSPT         s             11p 2 inz
     D maxpctEX        s              9p 2 inz
     D indEval         s               n   inz(*off)
     D indReEv         s               n   inz(*off)
     D indPT           s               n   inz(*off)
     D indEvPT         s               n   inz(*off)
     D indPctPT        s               n   inz(*off)
     D indPctRE        s               n   inz(*off)
     D indExcp         s               n   inz(*off)
     D indPayEx        s               n   inz(*off)
     D indPayEv        s               n   inz(*off)
     D indPayRe        s               n   inz(*off)
     D indPayPT        s               n   inz(*off)
     D indChgd         s               n   inz(*off)
     D indPayFSEx      s               n   inz(*off)
     D indPayFSEV      s               n   inz(*off)
     D indPayFSRE      s               n   inz(*off)
     D indPayFSPT      s               n   inz(*off)

     D xxType          s              2    inz
     D xxCount         s              3  0 inz
     D i               s              3  0 inz
     D n               s              3  0 inz
     D inPaid          s              9  0 inz
     D inDseq          s              5  0 inz
     D yyCount         s              3  0 inz
     D*** Beg Add *** 11/30/22 ***********************************
     d lastEvalDseq    s                   Like(CAdSeq)
     d lastReEvalDseq  s                   Like(CAdSeq)
     d lastOtherDseq   s                   Like(CAdSeq)
     D*** End Add *** 11/30/22 ***********************************

     *** CASE DETAIL RECORDS

     C     CAKy1         KList
     C                   KFld                    xxCo#
     C                   KFld                    xxsub#
     C                   KFld                    xxfund
     C                   KFld                    xxFyr
     C                   KFld                    xxCase
     C                   KFld                    xxEob#
     C                   KFld                    xxFdat

     C     CAKy2         KList
     C                   KFld                    xxCo#
     C                   KFld                    xxsub#
     C                   KFld                    xxfund
     C                   KFld                    xxFyr
     C                   KFld                    xxCase
     C                   KFld                    xxEob#

     C     CAKy3         KList
     C                   KFld                    xxCo#
     C                   KFld                    xxsub#
     C                   KFld                    xxfund
     C                   KFld                    xxFyr
     C                   KFld                    xxCase
     C                   KFld                    xxEob#
     C                   KFld                    xxFdat
     C                   KFld                    xxDseq

      *** BILLING CODE TABLE FILE

     C     bdKy1         KList
     C                   KFld                    xxType
     C                   KFld                    qqCode
     C                   KFld                    EOFrDt

     C     bdKy2         KList
     C                   KFld                    xxType
     C                   KFld                    qqCode

      *** PEO01 - EOMB Transaction File

     C     eoKy          KList
     C                   KFld                    xxCo#
     C                   KFld                    xxsub#
     C                   KFld                    xxfund
     C                   KFld                    xxFyr
     C                   KFld                    xxCase
     C                   KFld                    xxEob#

     C     deKy1         KList
     C                   KFld                    CACo#
     C                   KFld                    CASub#
     C                   KFld                    CAFnd
     C                   KFld                    CAFyr
     C                   KFld                    CACase
     C                   KFld                    CAEob#
     C                   KFld                    CADseq
     C                   KFld                    kkSeq#

     C     deKy2         KList
     C                   KFld                    CACo#
     C                   KFld                    CASub#
     C                   KFld                    CAFnd
     C                   KFld                    CAFyr
     C                   KFld                    CACase
     C                   KFld                    CAEob#
     C                   KFld                    CADseq

      *** PDH01 - EOMB Phys/HCFA Detail Transaction File
      *** PDU01 - EOMB Hosp/UB Detail Transaction File

     C     dhKy          KList
     C                   KFld                    xxCo#
     C                   KFld                    xxsub#
     C                   KFld                    xxfund
     C                   KFld                    xxFyr
     C                   KFld                    xxCase
     C                   KFld                    CAEob#
     C                   KFld                    xxDseq

      *** KEY LIST FOR PROVIDER NETWORK CONTRACT FILE.

     C     s4Ky1         KList
     C                   KFld                    xxFein
     C                   KFld                    xxMod
     C                   KFld                    xxNwCd
     C                   KFld                    CAFDat

     C     s4Ky2         KList
     C                   KFld                    xxFein
     C                   KFld                    xxMod
     C                   KFld                    xxNwCd

     C     coKy1         KList
     C                   KFld                    qqCode
     C                   KFld                    EOFrDt

     C     ebKy          KList
     C                   KFld                    xxCo#
     C                   KFld                    xxkey

     C     *Like         Define    BDDt          xxFdat
     C     *Like         Define    BDDt          xxFDtk
     C     *Like         Define    BDDt          savDat
     C     *Like         Define    BDpc01        xxPc00
     C     *Like         Define    CANDis        xxNDis
     C     *Like         Define    CATotA        xxTotA
     C     *Like         Define    CADiff        xxDIff
     C     *Like         Define    CAMraA        xxAlow
     C     *Like         Define    DESeq#        xxSeq#
     C     *Like         Define    DESeq#        kkSeq#
     C     *Like         Define    CARcn         xxRcn
     C     *Like         Define    EBKey         xxKey
     C     *Like         Define    DENot1        xxNot1
     C     *Like         Define    DENot2        xxNot2

     C     *Entry        PLIST
     C                   Parm                    qqCo#             3
     C                   Parm                    qqSub#            5
     C                   Parm                    qqFund            3
     C                   Parm                    qqFYr             3
     C                   Parm                    qqCase            7
     C                   Parm                    qqFein            9
     C                   Parm                    qqMod             3
     C                   Parm                    qqFDat            8
     C                   Parm                    qqDSeq            5
     C                   Parm                    qqCode            3
     C                   Parm                    qqCeDT            8
     C                   Parm                    qqSys             1
     C                   Parm                    qqFl1             1
     C                   Parm                    qqFl2             1
     C                   Parm                    qqFl3             1
     C                   Parm                    qqFl4             1
     C                   Parm                    qqFl5             1
     C                   Parm                    qqDs1            10
     C                   Parm                    qqDs2            10
     C                   Parm                    qqDs3            10
     C                   Parm                    qqDs4            10
     C                   Parm                    qqDs5            10
     C                   Parm                    qqRtCd            1
     C                   Parm                    qqEob#            9 0
     C                   Parm                    qqRcn             3 0

     C     DATCHK        PLIST
     C                   Parm                    srDtI             8 0
     C                   Parm      *BLANKS       srFFmt            8
     C                   Parm      *BLANKS       srTFmt            8
     C                   Parm                    srDtO            10
     C                   Parm                    srErr             1

     C     DTCYMD        PLIST
     C                   Parm                    srDtI
     C                   Parm      *BLANKS       srFFmt
     C                   Parm      '*CYMD'       srTFmt
     C                   Parm                    srDtO
     C                   Parm                    srErr

     C     DTMDYY        PLIST
     C                   Parm                    srDtI
     C                   Parm      '*CYMD'       srFFmt
     C                   Parm      *BLANKS       srTFmt
     C                   Parm                    srDtO
     C                   Parm                    srErr

      /Free
       xxCo#  = %dec(qqCo#:3:0);
       xxsub# = %dec(qqSub#:5:0);
       xxfund = %dec(qqFund:3:0);
       xxFyr  = %dec(qqFYr:3:0);
       xxCase = %dec(qqCase:7:0);
       xxFein = %dec(qqFein:9:0);
       xxMod  = %dec(qqMod:3:0);
       xxNwCd = qqDs1;
       x1FDAT = %dec(qqFDat:8:0);
       savDat = *Zeros;
       xxDseq = %dec(qqDSeq:5:0);
       first  = *On;

       // DO NOT PROCESS If ADDITIONAL Parm NOT RECEIVED

       If Q1Prms < 24;
         exsr srExit;
       EndIf;

       // CHECK EOBTYP, If NOT 'N' THEN QUIT PROGRAM

       xxEob# = qqeob#;
       chain eoKy pEO01;
       If not %found(faeolg) or
          (EOBTyp <> 'N'and EOBTyp <> 'H');
         exsr srExit;
       Else;
         xxRcn = EORcn;
       EndIf;

       // CHECK THE ALGORITHM USED, If NOT DX THEN QUIT PROGRAM

       xxType = 'DX';
       SetGT bdKy1 PBD01;
       ReadPE bdKy2 PBD01;
       *In63 = %EOF;
       If *In63 = '0';
         DoW *In63 = '0';

           If BDStat <> 'I';
             Leave;
           EndIf;

           ReadPE bdKy2 PBD01;
           *In63 = %EOF;
         EndDo;

         If BDType <> 'DX';
           exsr srExit;
         EndIf;

       Else;
         exsr srExit;
       EndIf;

       // GET THE CASE DETAILS
       i = 0;
       arrFdcap = *hival;

       chain CAKy2 Pca01;
       *In61 = not %Found;
       DoW *In61 = '0';                                                       //LOOP TO RETRIEVE REC
         xxDseq = CADSeq;
         If first = *ON;
           xxFDat = CAFDat;
           first = *off;
         EndIf;

         // EXECUTE ERROR CHECK
         exsr srErCk;
         If errInd = *OFF;

           //** PROCESSING FOR THE SAME DATE
           If xxFdat = CAFDat;
             lsVldR = '0';
             lsVldR = '1';
             xxFDtk = CAFDat;
             i = i + 1;
             inPaid = CApaid * 100;
             inDseq = CAdseq;
             decpaid(i) = inPaid;
             decDseq(i) = inDseq;
           EndIf;

           // If DIfFERENT DATE IS ENCOUNTERED

           If xxFdat <> CAFDat;

             // PERFORM NETWORK OPERATIONS ON THESE LINES OF SAME DATE

             savDat = CAFDat;
             If lsVldR = *ON;
               sorta arrFdcap;
               exsr srCalN;
               i = 0;
               arrFdcap = *hival;
             EndIf;
             xxFDat = savDat;
             //CLEAR DAYS;
             chain CAKy1 Pca01;
             *In61 = not %Found;
             Iter;
           EndIf;                                                             //            K
         Else;                                                                //ERROR INDICATOR
           ReadE CAKy2 Pca01;
           *In61 = %EOF;
           Iter;
         EndIf;
         ReadE CAKy2 Pca01;
         *In61 = %EOF;
       EndDo;

       // If THERE ARE SOME RECORDS LEFT TO BE PROCESSED
       If lsVldR = '1';
         xxFDat = xxFDtk;
         sorta arrFdcap;
         exsr srCalN;
         i = 0;
        clear arrFdcap;
       EndIf;

       *INLR = *ON;

       //*************MAIN PROCESSING ENDS****************************
       //**********************************************************
       // SUBROUTINE TO CHECK ERROR
       //**********************************************************
       BegSr srErCk;

         errInd = '0';

         if eobtyp = 'N';
            chain dhky pdh01;
            if %found(fddhl);
               xxprcd = dhprcd;
            endif;
         endif;
         if eobtyp = 'H';
            chain dhky pdu01;
            if %found(fddul);
               xxprcd = durate;
            endif;
         endif;

         // THE SERVICE DATE MUST BE BETWEEN THE PROVIDERS CONTRACT DATES

         select;

         when eobtyp = 'H';

          SetGT s4Ky1 PS402;
          ReadPE s4Ky2 PS402;
          dow *inlr = *off;
            if %eof(fms4l2);
              errInd = '1';
              leave;
            endif;
            If S4Stat <> 'I';
              if CAfDat >= S4bgDt and S4EnDT = *Zeros;
                leave;
              else;
                if CAFDat <= S4ENDT;
                  leave;
                endif;
              endif;
            EndIf;
          ReadPE s4Ky2 PS402;
          enddo;

         when eobtyp = 'N';

          SetGT s4Ky1 PS403;
          ReadPE s4Ky2 PS403;
          dow *inlr = *off;
            if %eof(fms4l3);
              errInd = '1';
              leave;
            endif;
            If (S4Stat <> 'I' AND CAFDat >= S4bgDt and S4EnDT = *Zeros)
               or (S4Stat <> 'I' AND CAFDat >= S4bgDt and CAfDat <= S4enDt);
              leave;
            EndIf;
          ReadPE s4Ky2 PS403;
          enddo;

         endsl;

         // CHECK FOR THE RECON NUMBER, READ NEXT RECORD If NOT SAME

         If xxRcn <> CARCN;
           errInd = '1';
           leavesr;
         EndIf;

         //  CHECK FOR PT CODE

         If NOT (xxprcd >= '97001' and xxprcd <= '97004') and
            NOT (xxprcd >= '97010' and xxprcd <= '97546');

            errind = '1';
         EndIf;

       EndSr;
       //************************************************************
       //************************************************************
       //** SUBROUTINE TO CALCULATE NETWORK
       //************************************************************
       BegSr srCalN;

         // PERFORM NETWORK OPERATIONS

         // max amount per day for Evals
         maxamtEV = 0;
         // available amount at any moment to pay Evals
         avlamtEV = 0;
         // maximum percentage for ReEvals
         maxpctRE = 0;
         // maximum amount for ReEvals
         maxamtRE = 0;
         // available amount at any moment to pay PTs
         avlamtRE = 0;
         // total of FS amounts prior to discount for all REs
         totFSRE = 0;
         // maximum amount per day for PTs
         maxamtPT = 0;
         // maximum percentage for PTs
         maxpctPT = 0;
         // available amount at any moment to pay PTs
         avlamtPT = 0;
         // total of FS amounts prior to discount for all PTs
         totFSPT = 0;
         // maximum percentage for Exceptions
         maxpctEX  = 0;
         // initialize indicators

         indEval = *off;
         indReEv = *off;
         indPctRE = *off;
         indPT = *off;
         indEvPT = *off;
         indPctPT = *off;
         indExcp = *off;
         indPayFSEV = *off;
         indPayFSRE = *off;
         indPayFSPT = *off;
         indPayFSEx = *off;

        // read through FDCAP for the current service date
        // determine what all type of codes are present on a single date

         Setll CAKy1 Pca29;
         ReadE CAKy1 Pca29;
         Dow not %eof(fdcal29);

           //savDat = CAFDat;

           // check if this is listed as an exception
           indExcp = *off;
          // get contract eff. date for this discount code
           setgt (qqcode:CAfdat) pco02;
           readpe (qqcode) pco02;
           if not %eof(ndcol5);
             xxContDt = COefdt;
           else;
             xxContDt = *hival;
           endif;
           setgt (qqcode:cacpt4) pco01;
           readpe (qqcode:cacpt4) pco01;
           dow not %eof(ndcol1);
             if CAfdat >= COefdt
                and COefdt >= xxContDt;
               indExcp = *on;
               leave;
             endif;
             readpe (qqcode:cacpt4) pco01;
           enddo;

           select;

            when indExcp =*on;
             // do nothing. if an exception, its not an Eval/ReEval/PT


            when cacpt4 = '97001' or cacpt4 = '97003'
             or  cacpt4 = '97161' or cacpt4 = '97162'
             or  cacpt4 = '97163' or cacpt4 = '97165'
             or  cacpt4 = '97166' or cacpt4 = '97167'  ;

             if indExcp = *off;
               indEval = *on;
             endif;


            when cacpt4 = '97002' or cacpt4 = '97004'
             or  cacpt4 = '97164' or cacpt4 = '97168'  ;

             if indExcp = *off;
               indReEv = *on;
               // compute sum of FS prior to discount for all REs
               totFSRE += CAPaid;
               if CApaid > 0;
                 yyCount = yycount + CAunts;
               endif;
             endif;


            when (cacpt4 >= '97010' and cacpt4 <= '97160')
             or  (cacpt4 >= '97169' and cacpt4 <= '97546');

             if indExcp = *off;
               indPT = *on;
               // compute sum of FS prior to discount for all PTs
               totFSPT += CAPaid;
               // Count the number of procedures
             if CApaid > 0;
               xxCount = xxcount + CAunts;
             endif;
             endif;

           endsl;

           ReadE CAKy1 Pca29;
        Enddo;

         // Populate discount amounts/percentages for each type

         if BDam01 <> *zeros;
           maxamtEV = BDam01;
         else;
           indPayFSEV = *on;
         endif;


         avlamtEV = maxamtEV;

         if BDam02 <> *zeros;
           maxpctRE = BDam02;
         endif;


         if BDam06 <> *zeros;
           maxamtRE = BDam06;
         else;
           maxamtRE = 0;
         endif;

         select;

          when maxamtRE <> *zeros and maxpctRE = *zeros;
           avlamtRE = maxamtRE;
           indPctRE = *off;
           indPayFSRE = *off;

          when maxamtRE = *zeros and maxpctRE <> *zeros;
           avlamtRE = (totFSRE * (maxpctRE/100));
           indPctRE = *on;
           indPayFSRE = *off;

          when maxamtRE = *zeros and maxpctRE = *zeros;
           avlamtRE = *zeros;
           indPayFSRE = *on;

          when maxamtRE <> *zeros and maxpctRE <> *zeros;
           indPayFSRE = *off;

           // available amount for REvs is the lesser of -
           // a) the fixed flat fee ( maxamtRE )
           // b) amount coming from FS %age (maxpctRE) for all REs

           if maxamtRE <= (totFSRE * (maxpctRE/100));
             avlamtRE = maxamtRE;
             indPctRE = *off;
           else;
             avlamtRE = (totFSRE * (maxpctRE/100));
             // set pay by percentage indicator
             indPctRE = *on;
           endif;

         endsl;

         if BDam03 <> *zeros;
           maxamtPT = BDam03;
         endif;


         if BDam04 <> *zeros;
           maxpctPT = BDam04;
         endif;


         select;

          when maxamtPT <> *zeros and maxpctPT = *zeros;
           avlamtPT = maxamtPT;
           indPctPT = *off;
           indPayFSPT = *off;

          when maxamtPT = *zeros and maxpctPT <> *zeros;
           avlamtPT = (totFSPT * (maxpctPT/100));
           indPctPT = *on;
           indPayFSPT = *off;

          when maxamtPT = *zeros and maxpctPT = *zeros;
           avlamtPT = *zeros;
           indPayFSPT = *on;

          when maxamtPT <> *zeros and maxpctPT <> *zeros;
           indPayFSPT = *off;
           // available amount for PTs is the lesser of -
           // a) the fixed per day amount (per diem : maxamtPT )
           // b) amount coming from FS %age (maxpctPT) for all PTs

           if maxamtPT <= (totFSPT * (maxpctPT/100));
             avlamtPT = maxamtPT;
             indPctPT = *off;
           else;
             avlamtPT = (totFSPT * (maxpctPT/100));
             // set pay by percentage indicator
             indPctPT = *on;
           endif;

         endsl;

         if BDam05 <> *zeros;
           maxpctEX  = BDam05;
         else;
           indPayFSEx = *on;
         endif;


        // Pay the line items

        indPayEx = *off;
        indPayEv = *off;
        indPayRe = *off;
        indPayPT = *off;

        // special case when both Eval and PT are there on same date
        if indEval = *on and indPT = *on;
          // since both Evals and PTs are present on same date,
          // pay Evals first then if there is any amount left
          // from Evals available, add that to PT available and
          // pay PTs from that total

          // CAKy1 dat = current service date

          // pay Evals
          indPayEv = *on;
          exsr payment;

          // if we have any available amount left from Evals,
          // that gets added to available amount of PT
        endif;

        // special case when both ReEval and PT are there on same date

        if indReEv = *on and indPT = *on;
          // since both ReEvals and PTs are present on same date,
          // pay ReEvals first

          // pay ReEvals
          indPayRE = *on;
          exsr payment;
        endif;

        // set on indicators to pay

        // set on Evals
        if indEval = *on and indPT = *on;
          // in this case Evals are already paid above, so set it off
          indPayEv = *off;
        else;
          indPayEv = *on;
        endif;

        // set on Exceptions
        indPayEx = *on;

        // set on ReEvals
        if indReEv = *on and indPT = *on;
          // in this case ReEvals are already paid above, so set it off
          indPayRE = *off;
        else;
          indPayRE = *on;
        endif;


        // set on PTs
        indPayPT = *on;

        // make payments
        exsr Payment;

       EndSr;
       //****************************************************************
       //****************************************************************
       //** SUBROUTINE TO CREATE EOB NOTE
       //****************************************************************
       BegSr srCrEo;

         // GET NEXT DETAIL SEQUENCE NUMBER

         kkseq# = *hival;
         SetGT deKy1 Pde02;
         ReadPE deKy2 Pde02;
         *IN99 = %EOF;
         If *IN99 = *OFF;
           xxSeq# = DESeq# + 1;
         Else;
           xxSeq# = 1;
         EndIf;

         // get description for Notes
         xxkey = '405';
         Chain(n) (xxCo#:xxkey) peb01;
         if %found(fteblnj);
           xxNot1 = EBdesc;
           xxNot2 = EBdsc2;
         else;
           xxNot1 = 'NETWORK DISCOUNT APPLIED';
           xxNot2 = *blanks;
         endif;

         CLEAR Pde01;
         DECO#  = CACo#;
         DESUB# = CASub#;
         DEFND  = CAFnd;
         DEFYR  = CAFyr;
         DECASE = CACase;
         DESS#  = CASS#;
         DEACDT = CAACDT;
         DEEOB# = CAEob#;
         DEDSEQ = CADseq;
         DESeq# = XXSEQ#;
         //*** Beg Add *** 07/18/23 **************************************
         //*** DEEOB  = '398';
         DEEOBCD  = '398';
         //*** End Add *** 07/18/23 **************************************
         DENOT1 = xxNot1;
         DENOT2 = xxNot2;
         DESTAT = 'A';
         DEESRC = 'N';
         DERCN  = CARCN;
         DEUSER = Q1USER;
         DEPGM  = Q1PGM;
         DEIUSR = Q1USER;
         DEIPGM = Q1PGM;
         DEDT   = %dec(%char(%date():*cymd0):7:0);
         DEIDT  = DEDT;
         DETIME = %dec(%char(%time():*iso0):6:0);
         DEITIM = DETIME;
         Write Pde01;
       EndSr;
       //***************************************************************
       //************************************************************
       // SUBROUTINE TO EXIT PROGRAM
       //************************************************************
       BegSr srExit;
         *InLr = *On;
         Return;
       EndSr;
       //************************************************************
       //***************************************************************
       //************************************************************
       // SUBROUTINE TO PAY LINE ITEMS
       //************************************************************
       BegSr Payment;
       //*** Beg Add *** 11/30/22 ***********************************
         lastEvalDseq   = 0;
         lastReEvalDseq = 0;
       //*** End Add *** 11/30/22 ***********************************
         // read file for the service date

         for n = 1 to i;
           xxDseq = decDseq(n);
           Chain CAKy3 pCA01;
           If %found(fdCAlh);

           indChgd = *off;

           // check if this is listed as an exception
           indExcp = *off;
          // get contract eff. date for this discount code
           setgt (qqcode:CAfdat) pco02;
           readpe (qqcode) pco02;
           if not %eof(ndcol5);
             xxContDt = COefdt;
           else;
             xxContDt = *hival;
           endif;
           setgt (qqcode:cacpt4) pco01;
           readpe (qqcode:cacpt4) pco01;
           dow not %eof(ndcol1);
             if CAfdat >= COefdt
                and COefdt >= xxContDt;
               indExcp = *on;
               leave;
             endif;
             readpe (qqcode:cacpt4) pco01;
           enddo;

           Select;

            // pay Exception
            when indExcp = *on and
                 indPayEx = *on;
             if indPayFSEx = *off;
               if CApaid <> *zeros;
                 CAndis = CApaid * (maxpctEx/100);
                 CApaid = CApaid - CAndis;
                 indChgd = *on;
               endif;
             endif;

            // pay Evals

            when (cacpt4 = '97001' or cacpt4 = '97003'
             or   cacpt4 = '97161' or cacpt4 = '97162'
             or   cacpt4 = '97163' or cacpt4 = '97165'
             or   cacpt4 = '97166' or cacpt4 = '97167')
             and  indPayEv = *on;
             if indPayFSEV = *off;
               if CApaid <> *zeros;
                 lastEvalDseq = xxDseq;
                 select;
                  when avlamtEv = 0;
                   CAndis = 0;
                   CApaid = 0;
                  when CApaid >= avlamtEv;
                   CAndis = CApaid - avlamtEv;
                   CApaid = avlamtEv;
                   avlamtEv = avlamtEv - CApaid;
                  when CApaid < avlamtEv;
                   CAndis = 0;
                   avlamtEv = avlamtEv - CApaid;
                 endsl;
                 indChgd = *on;
               endif;
             endif;

             // ReEval

            when (cacpt4 = '97002' or cacpt4 = '97004'
             or   cacpt4 = '97164' or cacpt4 = '97168')
             and  indPayRe = *on;
             if indPayFSRE = *off;
               if CApaid <> *zeros;
                 select;
                  when avlamtRE = 0;
                   CAndis = 0;
                   CApaid = 0;

                  when indPctRE <> *on;
                   //*** Beg Add *** 11/30/22 ***********************************
                   lastReEvalDseq = xxDseq;
                   //*** End Add *** 11/30/22 ***********************************
                   if (avlamtRE/yyCount) * CAunts < CAPAID;
                    CApaid = (avlamtRE/yyCount) * CAunts;
                    CAndis = (CAchrg - CAmra) - CApaid;
                   endif;
                   yyCount = yyCount - CAunts;
                   avlamtRE = avlamtRE - CApaid;

                  when indPctRE = *on;
                   // pay by percentage
                   If CApaid = CAmraa * CAunts or (CAchrg = CApaid);
                    CAndis = CApaid - (CApaid * (maxpctRE/100));
                    CApaid = CApaid - CAndis;
                   Endif;
                   avlamtRE = avlamtRE - CApaid;
                   yyCount = yyCount - CAunts;
                 endsl;

                 indChgd = *on;
               endif;
             endif;

            // PT

            when ((cacpt4 >= '97010' and cacpt4 <= '97160')
             or  (cacpt4 >= '97169' and cacpt4 <= '97546'))
             and  indPayPT = *on;

             if indPayFSPT = *off;
               if CApaid <> *zeros;
                 select;
                  when avlamtPT = 0;
                   CAndis = 0;
                   CApaid = 0;
                  when indPctPT <> *on;
                  //*** Beg Add *** 11/30/22 ***********************************
                  lastOtherDseq = xxDseq;
                  //*** End Add *** 11/30/22 ***********************************
                  if (avlamtPT/xxCount) *CAunts < CAPAID;
                   CApaid = (avlamtPT/xxCount) * CAunts;
                   CAndis = (CAchrg - CAmra) - CApaid;
                  endif;
                  xxCount = xxCount - CAunts;
                  avlamtPT = avlamtPT - CApaid;
                  when indPctPT = *on;
                   // pay by percentage
                  If CApaid = CAmraa * CAunts or (CAchrg = CApaid);
                   CAndis = CApaid - (CApaid * (maxpctPT/100));
                   CApaid = CApaid - CAndis;
                  Endif;
                   avlamtPT = avlamtPT - CApaid;
                   xxCount = xxCount - CAunts;
                endsl;
                indChgd = *on;
               endif;
             endif;

           Endsl;

           If indChgd = *on;
             causer = q1user;
             capgm = q1pgm;
             catime = %dec(%char(%time():*iso0):6:0);
             cadt = %dec(%char(%date():*cymd0):7:0);
             update Pca01;

             // update EOB notes
             exsr srErCk;
             if errInd = *OFF;
               exsr srCrEo;
             endif;
           endIf;


          Endif;

         Endfor;

       //*** Beg Add *** 11/30/22 ********************************************
         // Eval - Pay the fully daily allowance (if not paid in full due to rounding)
         If  lastEvalDseq > 0 and avlamtEv > 0;
            xxDseq = lastEvalDseq;
            Chain caky3 pca01;
            If %Found(FDCALH);
               CApaid = CApaid + avlAmtEv;
               CAndis = CAchrg - CApaid;
               CAuser = q1user;
               CApgm = q1pgm;
               CAtime = %dec(%char(%time():*iso0):6:0);
               CAdt = %dec(%char(%date():*cymd0):7:0);
               Update Pca01;
            Endif;
         Endif;

         // ReEval - Pay the fully daily allowance (if not paid in full due to rounding)
         If  lastReEvalDseq > 0 and avlamtRE > 0;
            xxDseq = lastReEvalDseq;
            Chain caky3 pca01;
            If %Found(FDCALH);
               CApaid = CApaid + avlamtRE;
               CAndis = CAchrg - CApaid;
               CAuser = q1user;
               CApgm = q1pgm;
               CAtime = %dec(%char(%time():*iso0):6:0);
               CAdt = %dec(%char(%date():*cymd0):7:0);
               Update Pca01;
            Endif;
         Endif;

         // Other - Pay the fully daily allowance (if not paid in full due to rounding)
         If  lastOtherDseq > 0 and avlAmtPT > 0;
            xxDseq = lastOtherDseq;
            Chain caky3 pca01;
            If %Found(FDCALH);
               CApaid = CApaid + avlAmtPT;
               CAndis = CAchrg - CApaid;
               CAuser = q1user;
               CApgm = q1pgm;
               CAtime = %dec(%char(%time():*iso0):6:0);
               CAdt = %dec(%char(%date():*cymd0):7:0);
               Update Pca01;
            Endif;
         Endif;
         //*** End Add *** 11/30/22 ********************************************
       Endsr;
