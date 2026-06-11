     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
      *================================================================
     H* W8090REISS - Reissue PPO Fees
     H*
     H* Created: 08/18/2020 C: 079 Support: PHW89578
     H*
     H*****************************************************************
     H* Revised:                                                      *
     H* 04/11/2022 C: 079 Support: PHW101804                          *
     H*                   Run for April 2022 PPO fee spreadsheet      *
     H*                                                               *
     H*****************************************************************
      * S1 Medical PPO Fee Holding File
     ffxreissue if   e             disk
      * EOB Master File
     ffaeol31   if   e           k disk
      * EOB Header Amount File
     ffaeal1    if   e           k disk
      * Claim File
     fwmcml1    if   e           k disk
      * Claim File
     fwmcil     if   e           k disk
      * Request Payment File
     fwarpp     o    e             disk    rename(prp01:prp0p)
      * Request Payment File
     fwarpl     if   e           k disk    Prefix(R2:2)
      * Payee Demographics Mstr File
     fwmpdl     if   e           k disk
      *
     fwtabl     IF   E           K DISK
      * Request Payment File Report
     fqsysprt   o    f  132        printer oflind(*inof)
      *
     d EOF             s               n   inz( *OFF )
     d isError         s               n   inz( *OFF )
     d intCo#          s              3  0
     d intFyr          s              3  0
     d intFnd          s              3  0
     d intCase         s              7  0
     d charConm        s             40
     d charFnam        s             40
     d charFFC         s             15
     d intChrg#        s             12  0
     d charChrg        s              9
     d charError       s             40
     d intCost         s              9  2
     d dateValueIn     s              8  0
     d title           c                   'PPO Fees Unprocessed Record'
     d kkCo#           s              3  0
     d kkKey           s                   like(PCkey)
     d xxSeq#          s                   like(RPseq#)
      *
      * Date & Time structure
     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)
      *
     d charCo#         s              3
     d charFnd         s              3
     d charFyr         s              3
     d charCase        s              7
     d charAppl        s              2
     d charData        s             75
     d charCktp        s             10
     d charRtcd        s              1
     d charEftc        s              1
     d charDate        s             10
     d intClm#         s             13  0
     d intLen          s              1  0
     d intPos          s              1  0
     d charCost        s              9
     d xxErrCnt        s              9  0
     d xxDesc          s             70a
     d xxRcn           s                   Like(EArcn)
     d xxPtype         s                   Like(CIpTyp)
     d xxSys           s                   Like(CIsys)
     d xxDate8         s               d
      *
     d                 DS
     d  DATA                   1     75
     d  QXTRDT                 1      7  0
     d  QXFRDT                 8     14  0
     d  QXTODT                15     21  0
     d  QXTRCD                22     23
     d  QXRECD                24     25
     d  QXPV1                 26     27
     d  QXPV2                 28     29
     d  QXSEQ#                30     34  0
     d  QXEOB#                35     43
     d  QXFEIN                44     52  0
     d  QXMOD                 53     55  0
     d  QXDSQ#                56     60  0
     d PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      * ComApi
     d Comapi          pr                  ExtPgm('WA001R')
     d intCo#                         3  0
     d intFnd                         3  0
     d charConm                      40
     d charFnam                      40

      * Write in Request Paymet File
     d WriteInPayFile  pr

      * General Prototypes
     d/copy sPrDate
     d/copy wprbank

     c     keyWMCML1     Klist
     c                   Kfld                    intCo#
     c                   Kfld                    intFnd
     c                   Kfld                    intFyr
     c                   Kfld                    intCase
      *
     c     keyTBKY       Klist
     c                   Kfld                    kkCo#
     c                   Kfld                    kkKey
      *
     c                   time                    dsdttm

     c     *Entry        Plist
     c                   Parm                    @@mode            1

      /Free

       ComApi(intCo#:intFnd:charConm:charFnam);
       *inOf = *on;

       // Read each record from PPO Fee Holding File.
       Read pre01;
       DoW Not %Eof(FXREISSUE);
          // Get the FAEOP record
          Chain (intCo#:REeob#) peo01;
          If %Found(faeol31);
          // Set the recon
             xxRcn = RErcn#;
             Chain (EOco#:EOsub#:EOfnd:EOfYr:EoCase:EOeob#:xxRcn)
                   pea01;
             if %Found(faeal1);
                If @@Mode = 'U';
                   WriteInPayFile();
                Endif;
             Else;
                xxErrCnt = xxErrCnt + 1;
                xxDesc = 'FAEAP Recon not found';
                If *InOf= *On;
                   Except Hdg;
                   *inOf = *off;
                Endif;
                Except Det;
             EndIf;
          Else;
             xxErrCnt = xxErrCnt + 1;
             xxDesc = 'Record not in FAEOP';
             If *InOf= *On;
                Except Hdg;
                *inOf = *off;
             Endif;
             Except Det;
          Endif;
          Read pre01;
       EndDo;
       If xxErrCnt > 0;
          Except Summary;
       Endif;

       *inLR = *On;
      /End-Free
      *********************************************************************
     oqsysprt   e            TitleFmt       1 03
     o                       dsDate             + 2 '  /  /    '
     o                       dsTime             + 3 '  :  :  '
     o                       title               75
     o          e            Hdg         3  1
     o                                           05 'Group'
     o                                           10 'Year'
     o                                           20 'Case'
     o                                           40 'EOB#'
     o                                           48 'Recon'
     o                                           60 'PPO Fee'
      *
     o          e            Det            1
     o                       REfnd               05
     o                       REyear              10
     o                       REcase              20
     o                       REeob#        M     40
     o                       RErcn#              48
     o                       REamt         J     60
     O          E            Summary     3
     O                                           22 'Total Number of Errors'
     O          E            Summary     1
     O                       xxErrCnt      J     22
      *
      //===================================================================
      // WriteInPayFile - Write record in request payment file
      //===================================================================
     P WriteInPayFile  B
     D WriteInPayFile  PI
      /Free

       Chain (EOco#:EOfnd:EOfYr:EOcase) pcm01;
       If Not %Found(WMCML1);
         xxErrCnt = xxErrCnt + 1;
         xxDesc = 'Record not in WMCMP';
         If *InOf= *On;
            Except Hdg;
            *inOf = *off;
         Endif;
         Except Det;
       Else;

       // Determine the next available sequence number for the payment request
         Chain(n) (CMco#:CMfnd:CMfYr:CMcase) prp01;
         If %Found(warpl);
            xxSeq#    = R2seq# + 1;
         Else;
            xxSeq# = 1;
         Endif;

         Clear prp0p;

         RPseq#    = xxSeq#;

         RPco#     = CMco#;
         RPfnd     = CMfnd;
         RPfyr     = CMfyr;
         RPcase    = CMcase;

         RPeob#    = REeob#;
         RPeobS    = RErcn#;
         RPinv#    = REinv#;

         RPfein    = %Dec(%Subst(REprov:1:9):9:0);
         RPMod     = %Dec(%Subst(REprov:11:3):3:0);

       // If Check Isssuance Control Record Exists use those values
         xxPtype = 'N';
         xxSys  = EOsys;
         Chain (RPco#:RPfnd:xxPtype:xxSys) pci01;
         If %Found(wmcil);
            RPtrCd = CItrCd;
         Else;
            RPtrCd = 'EP';
         Endif;

       // Load payment value 1, Transaction Type, Description
         RPbr2  = REbr2;
         kkCo# = intCo#;
         KKkey = RPtrCd;
         Chain (kkCo#:kkKey) ppc01;
         If %Found(WTABL);
            RPtype = PCtrTp;
            RPdesc = PCdesc;
         Endif;

       // Load Reserve Code, Type and Payment Status.
         RPreCd = REreCd;

         kkCo# = intCo#;
         KKkey = RPrecd;
         Chain keyTBKY prt01;
         If %Found(WTABL);
           RPretp = RTtype;
         Else;
           RPretp = *blanks;
         EndIf;

         RPsts     = 'PD';
         RPstat    = 'P';

       // Get the lost time flag.
         Select;
         When CMltfl = 'I';
            RPbr7     = 'LT';
         When CMltfl = 'M';
            RPbr7     = 'NL';
         EndSl;
         Monitor;
            RPtrdt  = DateTo7(systemDate());
            xxDate8 = cvtToDate(REfrDt);
            RPfrDt  = DateTo7(xxDate8);
            xxDate8 = cvtToDate(REtoDt);
            RPtoDt  = DateTo7(xxDate8);
         On-Error;
         Endmon;
         RPamt   = REamt;
         RPncci  = CMncci;
         RPwks   = 0;
         RPdays  = 0;
         RPtype  = 'P';

         RPrmds  = *Blanks;
         RPrmk1  = *Blanks;
         RPrmk2  = *Blanks;
         RPrmk3  = *Blanks;

       // Get the provider information
         Chain (RPfein:RPMod) ppd01;
         If %Found(wmpdl);
            If PDdba1 <> 'Y';
               RPpnam = PDname;
               RPadr1 = PDadr1;
               RPadr2 = PDadr2;
            Else;
               RPpnam = PDname;
               RPadr1 = PDadr1;
               RPadr2 = *Blanks;
            EndIf;
         EndIf;
            RPcty  =  PDcty;
            RPst   =  PDst;
            RPzip  =  PDzip;
            RPadr3 =  *Blanks;
            RPctry =  *Blanks;

         If PDadr3 <> *Blanks;
            RPadr3 =  PDadr3;
            RPctry =  PDctry;
         EndIf;


         RPbul1  = 0;
         RPchk#  = 0;
         RPoff#  = *Blanks;
         RPpgtp  = 'G';

         RPdsq#   = *Zero;
         charCo#  = %EditC(RPco#:'X');
         charFnd  = %EditC(RPfnd:'X');
         charFyr  = %EditC(RPfyr:'X');
         charCase = %EditC(RPcase:'X');
         QXTRDT   = RPtrdt;
         QXFRDT   = RPfrdt;
         QXTODT   = RPtodt;
         QXTRCD   = RPtrcd;
         QXRECD   = RPrecd;
         QXPV1    = RPbr2;
         QXPV2    = RPbr7;
         QXFEIN   = RPfein;
         QXMOD    = RPmod;
         QXDSQ#   = RPdsq#;

       //Call the Procedure to retrieve Bank Code.
         charData = DATA;
         charAppl = 'WC';
         charEftc = *Blanks;
         charCktp = GetBankCode(charCo#:charFnd:charFyr:charCase:charAppl:
                                charData:charRtcd:charEftc);
         RPcktp = charCktp;

         RPeftc = charEftc;
         RPacct = *Blank;
         RPdue = 0;
         RPsent = 'N';
         RPdseq = *Zero;
         RPltcd = *Blanks;
         RPntwk = 'B';
         RPfnal = *Blanks;
         RPrcVd = DateTo7(systemDate());

       //Populate file maintenance fields.
         RPuser    = Q1user;
         RPpgm     = Q1pgm;
         RPdt      = DateTo7(systemDate());
         RPtime    = TimeTo6(SystemTime());
         RPiusr    = Q1user;
         RPipgm    = Q1pgm;
         RPidt     = DateTo7(systemDate());
         RPitm     = TimeTo6(SystemTime());

         If @@Mode = 'U';
            Write prp0p;
         Endif;
       Endif;

      /End-Free
     P WriteInPayFile  E
