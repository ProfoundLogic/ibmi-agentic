     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE F6218R -  Prompt for 997 or 824 functional Ack. screen.
     H*
     H*****************************************************************
     H* Description:  This pgm allows the user to select 997 function *
     H*   ack. screen or 824 functional acknowledgement screen        *
     H*                                                               *
     H*                                                               *
     H* Calls to . :  F6218C                                          *
     H* Called from:  menu                                            *
     H* Created by :  C:296     Project#: MC376       08/08/2014      *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     ff6218f    cf   e             workstn
     f                                     HANDLER('PROFOUNDUI(HANDLER)')
     f                                     INFDS(FIDS01)
     fsmupl     if   e           k disk
     fsmcol     if   e           k disk
     fsmorl     if   e           k disk
     fststl3    if   e           k disk
     ffea1p     if   e           k disk    usropn
     ffeahp     O  a e             disk
     ffea2p     O  a e             disk
     fftgtp     if   e             disk
     fftgtl     if   e           k disk    Rename(pgt01:pgt02)
     fqsysprt   O    f  132        printer oflind(*inof)

     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  TRLAK1                 1     12
     D  XTTSET                 1      3
     D  XTRCNT                 4     12
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D HEDAK1        E DS                  EXTNAME(FEAHP)
     D DETAK1        E DS                  EXTNAME(FEA2P)
     D*
     D TSetPgm         pr                  ExtPgm('F6218C1')
     D  ##Co#1                        3A
     D  ##St1                         2A
     D  ##TSet1                       3A
     D*
     DTSet824          pr                  ExtPgm('F6223C')
     D ##Co#1                         3A
     D ##St1                          2A
     D ##TSet1                        3A
     D*
     DTSet997          pr                  ExtPgm('F6218C')
     D ##Co#1                         3A
     D ##St1                          2A
     D ##TSet1                        3A
     D*
     DCenterField      pr                  ExtPgm('SRCENTER')
     D SRdfld                        80A
     D SRdlen                         3  0 Const
     D*
     DRtvBrandInfo     pr                  ExtPgm('RTVBRND')
     D #BType                         1A   Const
     D #Brnd1                        80A
     D #Brnd2                        80A
     D*
     DSrchStateTbl     pr                  ExtPgm('SY600R')
     D ##jst                          2A
     D ##rtcd                         1A
     D*
     DRtvtrantyp       pr                  ExtPgm('F2380R')
     D                                1A   Const
     D                                1A
     D                                1A
     D                                9  0
     D                                4    Const
     D*
     DUpdUsrParm       pr                  ExtPgm('F0006R')
     D*
     DProHelp          pr                  ExtPgm('M0006C')
     D                               10A
     D                               10A
     D                               10A
     D*
     D ##Co#           s              3A
     D ##Co#2          s              3  0
     D ##St            s              2A
     D ##TSet          s              3A
     D SRdfld          s             80A
     D ##jst           s              2A
     D ##rtcd          s              1A
     D ##kc            s              1A
     D ##kl            s              1A
     D #Brnd1          s             80A
     D #Brnd2          s             80A
     D xxrrn           s              9  0
     D flgProCmp       s               n
     D DTA118          s            118A
     D DTA123          s            123A
     D DTA256          s            256A
     D FILNAM          s             10A
     D MBRNAM          s             10A
     D LIBNAM          s             10A
     D kkTbid          s              4A
     D kkkey           s             10A
     D*
     C*================================================================
     C*  Define Key lists
     C*================================================================
     C*  Key list for the User Parameter file
     C*
     C     upky          klist
     C                   kfld                    q1user
     C*
     C*  Key list for the Company Master file
     C*
     C     coky          klist
     C                   kfld                    upco#
     C*
     C*  Key list for the Routing file
     C*
     C     orky          klist
     C                   kfld                    q1pgm
     C                   kfld                    xxfile
     C*
     C*  Key list for the State Table file
     C*
     C     stky          klist
     C                   kfld                    xxkey
     C*
     C     gtky          klist
     C                   kfld                    ##co#2
     C                   kfld                    kktbid
     C                   kfld                    kkkey
     C*
     C*================================================================
     C*  Define PLISTS
     C*================================================================
     C     *entry        plist
     C                   parm                    QQPRDS           40
     C                   parm                    QQCALL            1
     C*
     C     datchk        plist
     C                   parm                    srdti             8 0
     C                   parm      *blanks       srffmt            8
     C                   parm      *blanks       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
     C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*cymd'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*================================================================
     C*  Define work fields
     C*================================================================
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    *INLR         XXHDRF
     C     *LIKE         DEFINE    *INLR         XXTRLF
     C     *LIKE         DEFINE    ORFIL#        XXFILE
     C     *LIKE         DEFINE    ORTITL        XXTITL
     C                   MOVE      *ZEROS        XXOTDT            8 0
     C                   MOVE      *ZEROS        XXOTTM            6 0
     C                   MOVE      *ZEROS        XXDATE            8 0
     C                   MOVE      *BLANK        XXKEY            10
     C                   MOVE      *ZEROS        XXRCNT            9 0
     C                   MOVE      *ZEROS        XXCNT             9 0
     C                   MOVE      *OFF          XXHDRF
     C*
     C*===== MAINLINE =================================================
     C*
      /Free
        ExSr MainSr;

        Begsr MainSr;
           ExSr RtvPrm;

           Dow flgProCmp = *off;
              ExSr Process;
           EndDo;
              *inlr = *on;
        EndSr;

        Begsr RtvPrm;
         //******************************************************
         //Retrieve the User parm file
         //******************************************************
         Chain upKy Pup01;
           If not %found(Smupl);
              UPco# = *Zeros;
           Endif;

         //******************************************************
         //Retrieve the Company Master
         //******************************************************
           Chain coky pco01;
              SRdfld = COname;
                 CenterField(SRdfld :40);
              SCconm = SRdfld;

              RtvBrandInfo('@' : #Brnd1 : #Brnd2);

              scUser = Q1user;
              scPrds = Qqprds;
              dsdttm = %dec(%time():*hms);

         //******************************************************
         //Initialize the Record Format
         //******************************************************
              Q2recf = *blanks;
              Q2recf = 'SCR01';
              scJst = *Blanks;
              scTSet = *Blanks;
        EndSr;

        BegSr Process;
          If Q2recf = 'SCR01';
             Exfmt scr01;
          Endif;

          //******************************************************
          //Process all command keys
          //******************************************************
          If *in11 = *on;
             If *inKC = *on;                                              //F3 = Exit
                LeaveSr;
             Endif;

         //******************************************************
         //Jurisdiction State Search
         //******************************************************
             If *inKJ = *on;                                              //F10 = Search
                Select;

                When hfld = 'SCJST';
                    ##rtcd = *blanks;

                    SrchStateTbl(##jst :##rtcd);
                       If ##rtcd = 'A';
                          LeaveSr;
                       Endif;                                             //##rtcd = 'A'

                       If ##rtcd = 'B' or ##jst = *blanks;
                          LeaveSr;
                       Endif;

                       If ##rtcd = '*';
                          scJst = ##Jst;
                          *in25 = *on;
                          LeaveSr;
                       Endif;                                             //##rtcd = '*'

                When hfld = 'SCTSET';
                   Rtvtrantyp('2' :##kc :##kl :xxrrn :'EACK');
                      If ##kc = *on or ##kl = *on;
                         scTSet = *blanks;
                         LeaveSr;
                      elseif xxrrn <> 0;
                         Chain xxrrn pgt01;
                            If %found(ftgtp);
                               scTSet = gtkey;
                               *in25  = *on;
                               LeaveSr;
                            else;
                               scTSet = *blanks;
                               LeaveSr;
                            Endif;
                      Endif;
                EndSl;
             Endif;

         //******************************************************
         //F13 = Change user parameters
         //******************************************************
            If *inkm = *on;
               UpdUsrParm();
               ExSr RtvPrm;
               LeaveSr;
            Endif;

         //******************************************************
         //Help Request
         //******************************************************
            If *in10 = *on;
               xxRecf = q2Recf;
               filNam = 'QHLP    ';
               libNam = '*LIBL   ';
               mbrNam = q1Pgm;
               mbrNam = xxScr#;
                  proHelp(filNam :libNam :mbrNam);
               q2Recf = xxRecf;
               LeaveSr;
            Endif;
          Endif;

         //======================================================
         //Process Prompt
         //======================================================

         //******************************************************
         //Validate Jurisdiction State
         //******************************************************
         If scJst = *blanks;
            *in24 = *on;
            LeaveSr;
         Endif;

         If scTset = *blanks;
            indTSetBlk = *on;
            LeaveSr;
         Endif;

         xxKey = scJst;
         Chain stky pst01;
            If not %found(ststl3);
               *in20 = *on;
               LeaveSr;
            Endif;
         //******************************************************
         //Invalid type and effective date
         //******************************************************
            If scTSet <> *blanks;
               ##Co#2 = UPco#;
               kkTbid = 'EACK';
               kkkey = scTSet;
                  Chain gtky pgt02;
                     If not %found(ftgtl);
                        indTSetVal = *on;
                        *in90 = *on;
                        LeaveSr;
                     Endif;
            Endif;

       //==========================================================*
       // If all checks are passed then call program on the basis of
       // transaction set.
       // a) Call F6223C if transaction set = 824, else
       // b) Call F6218C if transaction set = 997, else
       //===========================================================*

        ##Co# = %Char(UPco#);
        ##St  = scJst;
        ##tSet = scTSet;

          Select;

          When scTSet = '824';
             TSet824(##Co#:##St:##tSet);

          When scTSet = '997';
             TSet997(##Co#:##St:##tSet);

          Other;

          EndSl;

        //TsetPgm(##Co#:##St:##tSet);

        Open fea1p;
       //===========================================================*
       // Retrieve the Report Distribution record and print routing
       //===========================================================*
       xxfile = '0';
       Chain orky por01;
          If not %found(smorl);
             xxTitl = orTitl;
             Except RPDist;
          Endif;
             Except RPHdr;
             *inof = *off;
       //______________________________________________________________
       // All edits have passed.  Read the Acknowledgement File and
       // do the following:
       // 1. Retrieve the first record and insure it is the
       //    acknowledgement header.
       // 2. Retrieve the subsequent records and write them to the
       //    acknowledgement detail formatted file.
       // 3. when the acknowledgement trailer is encountered, compare
       //    the count to the detail records read and written, and
       //    write to the load report.
       // 4. Repeat steps 1 through 3 for each transmission set
       //    encountered.

       Read pa101;
        Dow not %eof(fea1p);
         If xxhdrf = *off;
            Exsr srxhdr;
               If xxhdrf = *off;
                  Except rper01;
                    flgProCmp = *on;
                       LeaveSr;
               Else;
                  Read pa101;
               Endif;
         Endif;

             Exsr srxDtl;

       //===========================================================*
       //Write the acknowledgement details until trailer found.
       //===========================================================*
         If xxTrlf = *on;
            xxCnt = %dec(xtrCnt:9:0);
               If xxCnt <> xxRCnt;
                  *in41 = *on;
                  ahErrf = 'Y';
               Else;
                  *in41 = *off;
               Endif;

               xxDate = %dec(ahOtdt:8:0);
               xxOtDt = %Dec(%Char(%date(xxDate:*iso):*cymd0):8:0);
               xxoTtm = %dec(ahoTtm:6:0);

               Except rpDtl;

                  If *inof = *on;
                     Except rpHdr;
                     *inof = *off;
                  Endif;

                  ahRcnt = %char(xxRcnt);
                  ahStat = 'P';

                  Write pah01;
                  xxRcnt = *zeros;
                  xxHdrf = *off;
          Endif;

          Read pa101;
        Enddo;

       //===========================================================*
       //Last record found should be trailer.
       //===========================================================*
       If xxTrlf = *Off;
          Except rpEr02;
       Endif;

       flgProCmp = *on;
       EndSr;                                                           //Sr main end

       //===========================================================*
       //SRXHDR - Save Acknowledgement in header structure.
       //===========================================================*
       BegSr srxHdr;
         Clear heDak1;
            dta118 = %Subst(A1data:1:118);
            heDAk1 = dta118;

         If AhtHdr = 'HD1';
            xxHdrf = *on;
            AHco# = UPco#;
            AHjst = SCjst;
         Endif;
       EndSr;

       //===========================================================*
       //SRXDTL - Move the acknowledgement detail into structure
       //and write.
       //===========================================================*
       BegSr srxDtl;
          xxTrlf = *off;
          Clear DEtak1;
          dta256 = %subst(A1data:1:265);
          DEtak1 = dta256;

          dta123 = %subst(A1data:1:123);
          DEtak1 = dta256;

          If A2tak1 = 'AK1';
             A2otdt = AHotdt;
             A2stat = *Blanks;
             A2errf = *Blanks;
             A2co#  = UPco#;
             A2Jst  = scJst;
             xxRcnt = 1;

             Write pa201;
          Else;
             If A2tak1 = 'TR1';
                Clear TRlak1;
                TRlak1 = A1data;
                xxTrlf = *on;
             Endif;
          Endif;
       EndSr;
      /End-Free
     C**************************************************************************
     OQSYSPRT   E            RPDIST         1 06
     O                                            8 'XXXXXXXX'
     O                                           20 'XXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXX     XX'
     O                                           77 'XXXXXX'
     O          E            RPDIST         1
     O                                            9 'XXXXXXXXX'
     O                                           21 'XXXXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXXX    XX'
     O                                           78 'XXXXXXXX'
     O          E            RPDIST         1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XXXX    XX'
     O                                           79 'XX      XX'
     O          E            RPDIST         1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            RPDIST         1
     O                                            9 'XXXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            RPDIST         1
     O                                            8 'XXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           71 'XX'
     O          E            RPDIST         1
     O                                            4 'XXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           79 'XX    XXXX'
     O          E            RPDIST         1
     O                                            5 'XXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX    XXXX'
     O          E            RPDIST         1
     O                                            6 'XX XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX      XX'
     O          E            RPDIST         1
     O                                            7 'XX  XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX    XXXX'
     O                                           79 'XX      XX'
     O          E            RPDIST         1
     O                                            8 'XX   XXX'
     O                                           21 'XXXXXXXX'
     O                                           33 'XXXXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX    XXXX'
     O                                           78 'XXXXXXXX'
     O          E            RPDIST         3
     O                                            9 'XX    XXX'
     O                                           20 'XXXXXX'
     O                                           32 'XXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX     XXX'
     O                                           77 'XXXXXX'
     O          E            RPDIST         2
     O                                           19 'REPORT TITLE:'
     O                       XXTITL              80
     O          E            RPDIST         2
     O                                           19 'PROGRAM NAME:'
     O                       ORPNAM              30
     O                                           36 'FILE:'
     O                       ORFIL#              38
     O                                           46 'COPIES:'
     O                       ORCOPY        Z     50
     O                                           56 'HOLD:'
     O                       ORHOLD              61
     O          E            RPDIST         2
     O                                           19 'OUTPUT QUEUE:'
     O                       OROUTQ              30
     O                                           37 'FORMS:'
     O                       ORFORM              48
     O                                           56 'SAVE:'
     O                       ORSAVE              61
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           RPDIST
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O***                                51 '.1999'
     O          E            RPDIST         1 59
     O                       #BRND1
     O          E            RPDIST         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            RPHDR          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             118
     O                       DSDATE        Y    128
     O          E            RPHDR          2
     O                       Q1PGM               10
     O                       XXTITL              99
     O                                          123 'PAGE:'
     O                       PAGE          Z    128
     O          E            RPHDR          2
     O                                           19 'JURISDICTION STATE:'
     O                       SCJST               22
     O          E            RPHDR          2
     O                                            7 'T-SET'
     O                                           20 'SENDER FEIN'
     O                                           35 'RECEIVER FEIN'
     O                                           58 'ORIGINAL TRANSMISSION'
     O                                           64 'TEST'
     O                                           78 'RECORDS SENT'
     O                                           96 'RECORDS RECEIVED'
     O                                          104 'ERROR'
     O*
     O          E            RPDTL          1
     O                       AHTSET               6
     O                       AHSEIN              18
     O                       AHREIN              31
     O                       XXOTDT        Y     45
     O                       XXOTTM              55 '  :  .  '
     O                       AHTPFG              62
     O                       XXCNT         Z     78
     O                       XXRCNT        Z     96
     O                       AHERRF             102
     O          E            RPER01         1
     O                                           50 'NO HEADER FOUND'
     O          E            RPER02         1
     O                                           50 'NO TRAILER FOUND'
