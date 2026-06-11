     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2023 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Claim reserve history detail box
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTCLMRSV
     h*  Calls to:
     h*
     h*  Created on 04/20/2012 by C:189 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h*  06/11/24 C:340 Project: 152WCB
     h*                 Add Clear check date.
     h*
     h*  06/14/23 C:344 Project: WC2430
     h*                 Add Duplicate Comment field to the Check Detail Tab.
     h*
     h*  07/20/20 C:079 Support: WC2749
     h*                 E-Pay changes to check label
     h*
     h*  11/08/19 C:079 Support: 099/82052
     h*                 Change Managed Care Service programs to be API's
     h*
     h*  09/10/18 C:079 Project: 146WHY
     h*                 -Add Received date
     h*
     h*  01/28/16 C:062 Project: 39438(25)
     h*                 -Only show mailing date if it is a Payment type (M or P).
     h*                 -Don't apply mailing date adjustment if Manaual Payment.
     h*
     h*  05/06/15 C:062 Project: 114MRH
     h*                 Assign value to check # label in detail box
     h*
     h*  09/23/14 C:279 Support: 86678
     h*                 Condition added for Weeks and Days.
     h*
     h*  09/17/14 C:296 Support: 86710
     h*                 Populate EOMB# link from grid, instead of populating again
     h*                 in this program.
     h*                 Changes in display file (WTCLMRSV): We have modified id's
     h*                 1) LnkEob# to DtlLnkEob#2 and
     h*                 2) LnkEob1# to DtlOutEob#.
     h*
     h*  09/03/14 C:062 Support: Redmine #4609
     h*                 Condition FAEOP.
     h*
     h*  08/05/14 C:282 Support: WA099 83882
     h*                 Added Reverse Deductible Recovery Link JSON handling.
     h*
     h*  06/04/14 C:242 Support: Redmine #4210
     h*                 Check# & Link not showing in Claim Reserve/Payment Information
     h*                 - Payment Detail Tab - Payment Information detail box
     h*
     h* 05/20/14  C:300 Project: 147WCX
     h*                 Add Image Link to Reserve/Payment Info Screen
     h*                 Display link only if an image is attached to EOB
     h*
     h*  01/13/14 C:065 Support: Redmine #3762
     h*                 The cleared date field should pull from PARCDT.
     h*
     h*  08/19/13 C:065 Support: Redmine #3577
     h*                 Swap the service date range fields in output.
     h*
     h*  08/14/13 C:062 Support: WC045 83151
     h*                 Correct remark code key.
     h*
     h*  05/20/13 C:279 Redmine: 2604
     h*                 Condition added for call to WCMC object.
     h*
     h*  10/25/12 C:284 Project: 114WGD
     h*                 CSI Retrofit of Custom Code to Base. Invoice# inclusion
     h*
     h**********************************************************************
      * Customer Master File
     Fwdpap     if   e             Disk    rename(ppa01:ppa0p)
     Fwmcxl     if   e           k Disk    ReName(pcx01:pcx00)
     Fwacpl     if   e           k Disk    ReName(pcp01:pcp00)
     fwmcdl     if   e           k Disk    ReName(pcd01:pcd00)
     Fwmpdl     if   e           k Disk    ReName(ppd01:ppd00)
     Fwdrpl     if   e           k Disk    ReName(prp01:prp000)
     Fwtgtl     if   e           k Disk    ReName(pgt01:pgt00)
     FWmcml1    if   e           k Disk
     Fwdrpl3    if   e           k Disk
     F                                     ReName(prp01:prp03)
     fwtabl     if   e           k Disk
     f                                     Ignore(prs01)
     f                                     Ignore(psy01)
     f                                     Ignore(pid01)
     f                                     ReName(prt01:prt00)
     f                                     ReName(ppc01:ppc00)
     f                                     ReName(pb201:pb200)
     f                                     ReName(pb701:pb700)
     Fwdcrl     if   e           k Disk    ReName(pcr01:pcr00)
     ffaeol3    if   e           k disk    Usropn
     fimdol1    if   e           k disk    Usropn
     fwmfnl     if   e           k Disk    ReName(pfn01:pfn00)

     d* Call external programs
     D Getinv#         pr                  ExtPgm('FA033R')
     D ##Co#                               like(PACo#)
     D ##Wcf#                              like(PAFnd)
     D ##Fyr                               like(PAFyr)
     D ##Case                              like(PACase)
     D ##Eob#                              like(PAEob#)
     D ##Rcn                          3  0
     D ##Fein                              like(PAFein)
     D ##Mod                               like(PAMod)
     D ##Chk#                         9  0
     D ##Othr                              like(##Othr)
     D ##Ast                               like(##Ast)
     D ##Inv#                              like(##Inv#)
     D ##Ninv                              like(##Ninv)
     D ##Sub#                              like(##Sub#)
     D ##fnd#                              like(PAFnd)
     D ##Rrn                               like(##Rrn)
     D ##Rc                                like(##Rc)

     D GetNinv#        pr                  ExtPgm('FA034R')
     D ##Co#                               like(PACo#)
     D ##Wcf#                              like(PAFnd)
     D ##Fyr                               like(PAFyr)
     D ##Case                              like(PACase)
     D ##Eob#                              like(PAEob#)
     D ##Fein                              like(PAFein)
     D ##Mod                               like(PAMod)
     D ##Chk#                              like(PAChk#)
     D ##Othr                              like(##Othr)
     D ##Ast                               like(##Ast)
     D ##Inv#                              like(##Inv#)
     D ##Ninv                              like(##Ninv)
     D ##Sub#                              like(PAFnd)
     D ##fnd#                              like(PAFnd)
     D ##Rrn                               like(##Rrn)
     D ##Rc                                like(##Rc)

     D GetSystem       pr                  ExtPgm('SA001R')
     D ##Wcif                              like(##rtcd)                         WC INST
     D ##Mcif                              like(##rtcd)                         MC INST
     D ##Wcin                              like(##rtcd)                         WC INTERFACE
     D ##Ar                                like(##rtcd)                         MC AR INST
     D ##Ntwk                              like(##rtcd)                         NETWORK INST
     D ##Ur                                like(##rtcd)                         PRECERT INST
     D ##Med                               like(##rtcd)                         MEDMGMT INST
     D ##Edi                               like(##rtcd)                         EDI INST
     D ##Wcdk                              like(##rtcd)                         WC DARTS KEY
     D ##Rhab                              like(##rtcd)                         REHAB INST
     D ##Mss                               like(##rtcd)                         MS SUPPORT
     D ##Fsch                              like(##rtcd)                         FEE SCHEDULE
     D ##Prvb                              like(##rtcd)                         PROVIDER BILL

     D GetAcct         pr                  ExtPgm('WA023R')
     D ##Co                                like (##Co)
     D ##Fnd                               like (##fnd)
     D ##appl                              like (##appl)
     D ##srdt                              like(##srdt)
     D ##Acct                              like(##Acct)
     D ##Call                              like(##Call)
     D ##Hdcy                              like(##Hdcy)
     D ##Chkq                              like(##Chkq)
     D ##Ftyp                              like(##Ftyp)
     D ##Rtcd                              like(##Rtcd)
     D ##Efdt                              like(##Efdt)
     D ##Cpyq                              like(##Cpyq)
     D ##Cktp                              like(##Cktp)

     D GetPmt          pr                  ExtPgm('WA072R')
     D ##Co                                like(##Co)
     D ##Dtic                              like(##Dtic)
     D ##Srd                               like(##Srd)
     D ##Mode                              like(##Mode)
     D ##Dtoc                              like(##Dtoc)
     D ##Rtcd                              like(##Rtcd)

     D GetName         pr                  ExtPgm('SRNAME')
     D ##Last                              like(##Last)
     D ##Frst                              like(##Frst)
     D ##Term                              like(##Term)
     DGetAdjState      pr                  EXTPGM('FA787R')
     D inCo#                          3  0
     D inFnd                          3  0
     D inFyr                          3  0
     D inCase                         7  0
     D OutAdjState                    2
     DGetFaeopRrn      pr                  EXTPGM('FA788R')
     D inCo#                          3  0
     D inFnd                          3  0
     D inFyr                          3  0
     D inCase                         7  0
     D inEob#                         9  0
     D OutFaeopRrn                    9

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable

     d* Get the amount
     d GetAmount       pr

     d* Get Payee Information
     d GetPayeeEmail   pr            60

     d* Get Payee Information
     d GetPayeeName    pr

     d* Get Reserve Information
     d GetReserveInfo  pr           100

     d* Get Reserve Type Description
     d GetReserveType  pr            20

     d* Get Transaction Information
     d GetTransInfo    pr           100

     d* Get Transaction Type Description
     d GetTransType    pr            20

      * *ENTRY
     DClaimReserve02   pr                  ExtPgm('WTCLMRSV02')
     D                                9  0
     D                            32767
     D                                1
      *
     DClaimReserve02   pi
     D inRRN                          9  0
     D outJSON                    32767
     D RtnCode                        1
      *
     D nf              c                   const('NOT FOUND')
     D numEdtCde       c                   const('N')
     D intCo#          s                   like(PAco#)
     D intFnd          s                   like(PAfnd)
     D intFyr          s                   like(PAfyr)
     D intCase         s                   like(PACase)
     D ##Last          s             56
     D ##Frst          s             15
     D ##Dtic          s              8
     D ##Dtoc          s              8
     D ##DtoN          s              8  0
     D ##Dto           s              8
     D ##ffmt          s              8
     D ##tfmt          s              8
     D ##Key           s                   like(NAkey)
     D ##Term          s              1

     D ##Wcif          s                   like(##rtcd)
     D ##Mcif          s                   like(##rtcd)
     D ##Wcin          s                   like(##rtcd)
     D ##Ar            s                   like(##rtcd)
     D ##Ntwk          s                   like(##rtcd)
     D ##Ur            s                   like(##rtcd)
     D ##Med           s                   like(##rtcd)
     D ##Edi           s                   like(##rtcd)
     D ##Wcdk          s                   like(##rtcd)
     D ##Rhab          s                   like(##rtcd)
     D ##Mss           s                   like(##rtcd)
     D ##Fsch          s                   like(##rtcd)
     D ##Prvb          s                   like(##rtcd)

     D ##TbId          s                   like(GTtbid)
     D ##Acct          s             20
     D ##Call          s              1
     D ##Hdcy          s              1
     D ##Chkq          s             10
     D ##Ftyp          s             10
     D ##Rtcd          s              1
     D ##Efdt          s              7  0
     D ##Cpyq          s             10
     D ##Cktp          s             10
     D ##Co            s              3
     D ##fnd           s              3
     D ##appl          s              2
     D ##srdt          s              8
     D ##srd           s              5


     D ##Co#           s                   like(PACo#)
     D ##Wcf#          s                   like(PACo#)
     D ##Fyr           s                   like(PACo#)
     D ##Case          s                   like(PACase)
     D ##Eob#          s                   like(PAEob#)
     D ##Rcn           s              3  0
     D ##Fein          s                   like(PAFein)
     D ##Mod           s                   like(PAMod)
     D ##Chk#          s              9  0
     D ##Othr          s              9  2
     D ##Ast           s              2
     D ##Inv#          s              9  0
     D ##Ninv          s              9  0
     D ##Sub#          s                   like(PAFnd)
     D ##fnd#          s                   like(PAFnd)
     D ##Rrn           s              9  0
     D ##Rc            s                   like(##rtcd)
     D ##Mode          s                   like(##rtcd)
     D ##Fyrc          s              3
     D ##Casec         s              7
     D ##User          s             10
     D ##err           s              1
     D intDat          s              7  0
     D intDate         s              8  0
     D intDays         s              5  0
     D intAdate        s              7  0
     D intDtlDes2      s                   Like(PADesc)
     D intDtlDes7      s                   Like(PADesc)
     D intDtlInv#      s              9  0
     D intDtlNinv#     s              9  0
     D intTrdt         s              7  0
     D intWeeks        s              4  0
     D intDys          s              2  0
     D check#          s              9  0
     D decDtlAmount    s              9  2
     D strCLTID        s              3
     D strCtrd         s             15
     D strAppl         s              2
     D strFein         s              9
     D strKy           s             10
     D strDtlPnam      s             40
     D strDtlRmk       s             30
     D strDtlRmk1      s             30
     D strDtlRmk2      s             30
     D strDtlRmk3      s             30
     D strDtlBr2       s             30
     D strPayeeNam     s             40
     D strDtlAdr1      s             40
     D strDtlAdr2      s             40
     D strDtlAdr3      s             40
     D strEdiM         s              1  0
     D strDtlBr7       s             30
     D strDtlDupP      s              1
     D strDtlGChkF     s             10
     D strdtlAcct      s             20
     D intDtlCmdt      s              7  0
     D intDtlRcvdDt    s              7  0

     D strEobLink      s            720
     D strEobTitle     s             50
     D strEobRrn       s              9
     D strImgLink      s            720
     D strIgFlr        s             50
     D strImgTitle     s             50
     D psds          esds                  ExtName(szq1p)
     D RPGM            C                   CONST('RTVDOCROOT')
     D strAdjstate     s              2
     D strQueue        s             10
     D strMode         s              1
     D strPayType      s             40
     D ClearDate       s                   Like(CRACDT)
     D strCheckLink    s            720
     D strCheckTitle   s             50
     D strCheckRRN     s              9
     D strRevDedRev    s             27
     D strChkType      s             10

     D                 ds
     D  KeyField               1     15
     D  strFyr                 1      3
     D  strCase                4     10
     D  strSeq#               11     15

     D                 ds
     D  intzip                 1      9  0
     D  intzip1                1      5  0
     D  intzip2                6      9  0
     D                 ds
     Ddsdate#                         8  0
     Dyy                       1      4
     Dmm                       5      6
     Ddy                       7      8
      *
     C                   CALL      RPGM
     C                   PARM                    ROOT             10
      /Free

       Chain inRRN ppa0p;
        // set key values
       intCo# = PACo#;
       intFnd = PAFnd;
       intFyr = PAFyr;
       intCase = PACase;

       outJSON = ' ';
       BuildJSON(outJSON :'LnkPayeeEmail' : %TrimR(GetPayeeEmail));
       BuildJSON(outJSON :'outDtlDes' : %TrimR(PADesc));
       BuildJSON(outJSON :'OutDtlTsdt' :%EditW(dtMDYY(PAFrDt):'  /  /    ') +
                           ' - ' + %EditW(dtMDYY(PAToDt):'  /  /    '));
       BuildJSON(outJSON :'outDtlTnsdt' :%EditW(dtMDYY(PATrDt):'  /  /    '));
       //*** Begin Add ***  06/14/23  ****************************************
       BuildJSON(outJSON :'OutDtlDupCom' : %Trim(PAdupCom));
       //*** End   Add ***  06/14/23  ****************************************

       If PArqDt=0;
          BuildJSON(outJSON :'OutDtlPoDt' :%EditW(dtMDYY(PApoDt):'  /  /    '));
       Else;
          BuildJSON(outJSON :'OutDtlPoDt' :%EditW(dtMDYY(PArqDt):'  /  /    '));
       EndIf;
       BuildJSON(outJSON :'OutInvoice#' :%Trim(PAinv#));


       Exsr LoadWkDys;
       BuildJSON(outJSON :'OutDtlWkDys' :%Char(intWeeks) + '/' +
        %Char(intDys));
       BuildJSON(outJSON :'OutDtlRcn' : %Editc(PAEobs:'Z'));
       BuildJSON(outJSON :'OutDtlrecd' :GetReserveInfo());

       BuildJSON(outJSON :'OutDtlRqst' :getUsrName(PArqst));
       BuildJSON(outJSON :'OutDtlrcd' :GetTransInfo());
       BuildJSON(outJSON :'OutDtlFMod' :%editc(PAFein:'X') +'-'+
       %editc(PAMod:'X'));
       //

       Exsr BuildData;

       //Build JSON Output
       BuildJSON(outJSON :'LblChk#' : strChkType);
       If PAtype = 'R' Or PAeftC = 'E';
          BuildJSON(outJSON :'DtlOutChk#':%editc(PAChk#:'Z'));
          outJSON = %Trim(outJSON) + ', "DtlLnkChk#":';
          StartJSONelemnt(outJSON);                        // Start the element build
          BuildJSON(outJSON :'text' :' ');  // Assign the value
          BuildJSON(outJSON :'url' :' ');    // Assign the URL
          EndJSONelemnt(outJSON);                          // End the element build
       Else;
          BuildJSON(outJSON :'DtlOutChk#':' ');
          outJSON = %Trim(outJSON) + ', "DtlLnkChk#":';
          StartJSONelemnt(outJSON);                        // Start the element build
          BuildJSON(outJSON :'text' :%editc(PAChk#:'Z'));  // Assign the value
          strCheckTitle = 'Print Check#: ' + %Trim(%EditC(PAchk#:'4'));
          strCheckRrn = %EditC(inRRN:'X');
          strCheckLink = CreateURL('W7019CW':strCheckTitle:strCheckRrn);
          BuildJSON(outJSON :'url' :%Trim(strCheckLink));    // Assign the URL
          EndJSONelemnt(outJSON);                          // End the element build
          EndIf;
       BuildJSON(outJSON :'OutDtlClrdt':%EditW(dtMDYY(ClearDate):'  /  /    '));
       BuildJSON(outJSON :'OutDtlPayType':strPayType);
       BuildJSON(outJSON :'OutDtlInv#' :%editc(intDtlInv#:'Z'));
       BuildJSON(outJSON :'OutDtlNinv#':%editc(intDtlNinv#:'Z'));
       BuildJSON(outJSON :'OutDtlAmt':%EditC(decDtlAmount:numEdtcde));
       BuildJSON(outJSON :'OutDtlPNam':strDtlPnam);
       BuildJSON(outJSON :'OutDtlRemark':strDtlRmk);
       BuildJSON(outJSON :'OutDtlRemark1':strDtlRmk1);
       BuildJSON(outJSON :'OutDtlRemark2':strDtlRmk2);
       BuildJSON(outJSON :'OutDtlRemark3':strDtlRmk3);
       BuildJSON(outJSON :'OutDtlBr2':strDtlBr2);
       BuildJSON(outJSON :'OutDtlAdr1':strDtlAdr1);
       BuildJSON(outJSON :'OutDtlAdr2':strDtlAdr2);
       BuildJSON(outJSON :'OutDtlCsz':strDtlAdr3);
       BuildJSON(outJSON :'OutDtlBr7':strDtlBr7);
       BuildJSON(outJSON :'CxbDupPmtExcl':strDtlDupP);
       BuildJSON(outJSON :'OutDtlGChkF':strDtlGChkF);
       BuildJSON(outJSON :'OutdtlAcct':strdtlAcct);
       BuildJSON(outJSON :'OutDtlCmdt':%EditW(dtMDYY(intDtlCmdt):'  /  /    '));
       BuildJSON(outJSON :'OutDtlRcvdDt':%EditW(dtMDYY(intDtlRcvdDt):
                 '  /  /    '));
       If PAeob# <> 0 And ##Mcif = 'Y';
          BuildJSON(outJSON :'DtlOutEob#' :' ');
       EndIF;

       outJSON = %Trim(outJSON) + ', "DtlLnkEob#2":';
       StartJSONelemnt(outJSON);                        // Start the element build
       BuildJSON(outJSON :'text' :%Editc(PAEob#:'Z'));  // Assign the value
       // Retrieve information to build the url
       If PAeob# = 0 or ##Mcif <> 'Y';
          strEobLink=' ';
       Else;
          strEobTitle = 'Print Eob#: ' + %Trim(%EditC(PAeob#:'4'));
          If PAxFyr<>0 Or PAxCas<>0;
              Callp GetFaeopRrn(PAco#:PAfnd:PAxfYr:PAxCas:PAeob#:strEobRrn);
             If strEobRrn='000000000';
              Callp GetFaeopRrn(PAco#:PAfnd:PAfYr:PAcase:PAeob#:strEobRrn);
             Endif;
          Else;
             Callp GetFaeopRrn(PAco#:PAfnd:PAfYr:PAcase:PAeob#:strEobRrn);
          EndIf;
          Callp GetAdjstate(PAco#:PAfnd:PAfYr:PAcase:strAdjState);
          strMode='D';
          strQueue='QPRINT';
          strEobLink = CreateURL('F0340C':strEobTitle:strEobRrn:strAdjState:
           strMode:strQueue);
       EndIf;

       BuildJSON(outJSON :'url' :%Trim(strEobLink));    // Assign the URL
       EndJSONelemnt(outJSON);                          // End the element build

       If PAeob# <> 0 And ##Mcif = 'Y';
          outJSON = %Trim(outJSON) + ', "LnkEobImage":';
          StartJSONelemnt(outJSON);                        // Start the element build
          // Retrieve information to build the url
          strImgTitle = 'Display EOB Image';

          Open FAEOL3;
          Chain(n) (PAco#:PAfnd:PAeob#) peo01;
          If %found and EOidoc <> *BLANKS;
             BuildJSON(outJSON :'text' :'Image');   // Assign the value

             Open IMDOL1;
             Chain(n) EOidoc pdo01;
          ELSE;
             BuildJSON(outJSON :'text' :'     ');   // Assign the value
          Endif;

          strIgFlr = '/' + %trim(ROOT) + '/' + %trim(DOFLR) + '/' +
           %trim(DOsflr);

          strImgLink = strImgTitle + '/iDartsWeb/remarks/attachments.do?meth-
          od=getAttachmentFile&key=&sequence=&docId=-
          '+%trim(DOdcid)+'&folder='+%trim(strIgFlr)+'&user='+%trim(q1user);

          BuildJSON(outJSON :'url' :%Trim(strImgLink));
          EndJSONelemnt(outJSON);                 // End the element build
          Close FAEOL3;
          Close IMDOL1;
       EndIf;
       // This option can only be used on recovery transactions
       // (PATYPE='R') and the recovery must be a deduct recovery
       // (PABR2 must match one of the following codes in the Fund Master
       // File (WMFNP):
       //              FNMEP1 - MED NET DEDUCT PMT VAL 1
       //              FNCOP1 - COMP NET DEDUCT PMT VAL 1
       //              FNEXP1 - EXP NET DEDUCT PMT VAL 1
       //              FNLEP1 - LEGAL NET DEDUCT PMT VAL 1
          strRevDedRev = ' ';
          outJSON = %Trim(outJSON) + ', "OutRvDedRec":';
          StartJSONelemnt(outJSON);
          BuildJSON(outJSON :'text':strRevDedRev);
          BuildJSON(outJSON :'url' : ' ');
          BuildJSON(outJSON :'disabled':'true');
          EndJSONelemnt(outJSON);
          If PAtype = 'R';
             Chain (intCo#:intFnd) pfn00;
             If %Found(wmfnl);
                If FNmep1 = PAbr2 Or
                   FNcop1 = PAbr2 Or
                   FNexp1 = PAbr2 Or
                   FNlep1 = PAbr2;
                      strRevDedRev = 'Reverse Deductible Recovery';
                      outJSON = %Trim(outJSON) + ', "OutRvDedRec":';
                      StartJSONelemnt(outJSON);                        // Start the element build
                      BuildJSON(outJSON :'text' :strRevDedRev);        // Assign the value
                      BuildJSON(outJSON :'url' : ' ');
                      BuildJSON(outJSON :'disabled':'false');
                      EndJSONelemnt(outJSON);                          // End the element build
                EndIf;
             EndIf;
          EndIf;

       RtnCode = ' ';
       Return;
      /End-Free

      *************************************************************
      * BuildData for detail box
      *************************************************************
     C     BuildData     BegSr
     C*
     C     keyWTABL      klist
     C                   kfld                    PAco#
     C                   kfld                    strKy
      *
     C     keyWACPL      klist
     C                   kfld                    PAco#
     C                   kfld                    intDat
     C                   kfld                    PAFnd
    C*
     C     keyWMCXL      klist
     C                   kfld                    PAco#
     C                   kfld                    PAfnd
    C*
     C     keyWDCRL      klist
     C                   kfld                    PAco#
     C                   kfld                    KeyField
     C                   kfld                    PAFnd
    C*
      /FREE

       //get the Payee Name and Address information as per the logic
       GetPayeeName();

       //Move Amount to Detail
       GetAmount();

       //get the INV# values

       GetSystem(##Wcif:##Mcif:##Wcin:##Ar:##Ntwk
       :##Ur:##Med:##Edi:##Wcdk:##Rhab:##Mss:
       ##Fsch:##Prvb) ;

       If PApgTp = 'G';
          strDtlGChkF='Group';
       Else;
          strDtlGChkF='Non-Group';
       EndIf;

       If ##MCIF = 'Y' and PAEob# <> 0 ;
          If ##PRVB  = 'Y' ;
             ##Rcn  = PAEobs;
             check#=PAchk#;
             Getinv#(PACo#:PAfnd:PAFyr:PACase:PAEob#:##Rcn
             :PAFein:PAMod:check#:##Othr:##Ast:##Inv#
             :##Ninv:##Sub#:##fnd#:##Rrn:##Rc);
          else ;
             GetNinv#(PACo#:PAfnd:PAFyr:PACase:PAEob#
             :PAFein:PAMod:PAChk#:##Othr:##Ast:##Inv#
             :##Ninv:##Sub#:##fnd#:##Rrn:##Rc);
          Endif ;
       Endif ;

       //Load BRS Inv# and NWS Inv#
        //
       ##Inv#   =    intDtlInv# ;
       ##Ninv   =    intDtlNinv# ;

       //Load Pmt Val 1 and description
       //

       strKy =  PABr2 ;
       Chain  keyWTABL PB200 ;
       If %found();
          intDtlDes2 = B2DESC  ;
       Endif;

       strDtlBr2 = %trim(PABr2) + '  -  '+ %trim(B2desc)  ;

       // Load Pmt Val 2 and description
        //
       strKy =  PAbr7 ;
       intDtlDes2 = *blanks;
       Chain keyWTABL PB700;
       If %found();
          intDtlDes2 = B7DESC  ;
       Endif;

       strDtlBr7 = %trim(PABr7) + '  -  '+ %trim(intDtlDes2);

       // Load Duplicate payment Request
       //
       If PADpcd = 0 ;
          strDtlDupP = 'N';
       Else ;
          strDtlDupP = 'Y';
       Endif;

       // Mailing Date
       //
       If strCLTID = '139'  ;
          intDat = PAPoDt ;
       Else;
          intDat = PATrDt ;
       Endif;

       intDtlRcvdDt = PArcvD;
       intDtlCmdt = 0;
       If PAtype = 'P';
          Chain keyWACPL PCP00;
          If %Found();
            intDtlCmdt = CPcmdt;
          Endif;
       EndIf;

       If CPcmdt = 0 and (PAtype = 'M' Or PAtype = 'P');

          strEdiM  = 0 ;
          If PAtype = 'P';
             Chain keyWMCXL  PCX00;
             If %Found();
                strEdiM  =  CXedim;
             Endif;
          EndIf;


          intDays =  strEDiM;
          intDate =  dtMDYY(intDat) ;


          ##CO    =    %char(PACo#);
          ##Dtic  =    %editc(intDate:'X') ;
          ##Srd   =    %editc(intDays:'X') ;
          ##Mode  =    '2'    ;
          ##Dtoc  =    *BLANKS;
          ##Rtcd  =    *BLANK ;

          GetPmt(##Co:##Dtic:##Srd:##Mode:##Dtoc: ##Rtcd);
          intAdate = dtcymd(%int(##Dtoc));

          intDtlCmdt = intAdate ;
       Endif;

       Select;
       When Patype='P';
          strPayType='Payment';
       When Patype='M';
          strPayType='Manual Payment';
       When Patype='V';
          strPayType='Void';
       When Patype='R';
          strPayType='Recovery';
       When Patype='A';
          strPayType='Adjustment';
       When Patype='S';
          strPayType='Stop';
       When Patype='Z';
          strPayType='Reverse';
       EndSl;

       If PAeftC = 'E';
          strChkType = 'EFT #';
       Else;
          strChkType = 'Check #';
       EndIf;
       If PAeftC = 'V';
          strChkType = 'E-Pay #';
       Endif;

        strAppl =  'WP' ;
        ##co#  =  PAco# ;
        ##fnd  =  %editc(PAfnd:'X');
        ##appl =  strAppl;
        dsdate# =  %Dec(%date());
        ##srdt =  mm + dy + yy ;
        ##cktp =  'EFT' ;

       GetAcct(##Co:##fnd:##appl:##srdt:##Acct:##call:
       ##hdcy:##Chkq:##Ftyp:##Rtcd:##Efdt:##Cpyq:##Cktp);

       strdtlAcct =  ##acct ;

        strFyr  = %editc(PAfyr:'X');
        strCase = %editc(PAcase:'X');
        strSeq# = %editc(PAseq#:'X');
          Chain keyWDCRL  PCR00;
          If %Found();
          If CRACTP = 'X';
             ClearDate = CRACDT;
          Endif;
          Endif;

       //*** Beg Add ***  06/11/24  *** 152WCB *****************************
          If CRACDT = 0 and PACLEARDT <> 0;
             ClearDate = PACLEARDT;
          EndIf;
       //*** End Add ***  06/11/24  *** 152WCB *****************************

      /End-Free
     C                   EndSr
      /free
       //----------------------------------------------------------
       // Load Weeks and Days
       //----------------------------------------------------------
       Begsr LoadWkDys;

       intWeeks = 0;
       intDys   = 0;

       If ((PAtype = 'P' or PAtype = 'M' or PAtype = 'A' or Patype = 'Z') and
        PAamt > 0) or ((PAtype = 'S' or PAtype = 'V' or Patype = 'R') and
        PAamt < 0);
          intWeeks = PAwks;
          intDys   = PAdays;
       Else ;
          If PAwks <> 0 ;
            intWeeks = -1 * PAwks;
          EndIf ;
          If PAdays <> 0 ;
            intDys = -1 * PAdays;
          EndIf ;
       Endif;

       Endsr;
      /end-free
      *================================================================
      *  GetPayeeEmail - Get the Payee Email
      *================================================================
     P GetPayeeEmail   B
     D GetPayeeEmail   PI            60

     D eMail           s             60    Inz(*Blanks)

     C     keyWMCML1     Klist
     C                   Kfld                    PACo#
     C                   Kfld                    PAFnd
     C                   Kfld                    PAFyr
     C                   Kfld                    PACase
      *
     C     keyWMPDL1     Klist
     C                   Kfld                    PAFein
     C                   KFLD                    PAMod
      *
      *
     C     keyWMCDL      Klist
     C                   Kfld                    CMss#

      /Free

       //Access Claim Master

       Chain  keyWMCML1  pcm01 ;
       If not %found();
          CMSs# = *zeros ;
       Endif;

       // If FEIN# > 0 & FEIN# NE CMSS#, access Payee Demographics

       strFein  =   %editc(PAfein:'X') ;
       If PAfein <> 0 and strFein <> CMSs#  and PAretp  <>  'C';

       //  Access Payee Demographics
          Chain   keyWMPDL1   ppd00  ;
          If not %found();
             eMail =  *Blanks ;
          else ;
             eMail = PDemAd;
          EndIf;

         //  FEIN# = 0 Or FEIN# = CMSS#, access Claimant Demographics

       Else ;

        // Access Claimant Demographics
        //
          Chain  keyWMCDL  pcd00;
          If   not %Found();
             eMail=*Blanks;
          else ;
             eMail=CDemAd;
          EndIf;
       EndIf;

       Return eMail;

      /end-Free
     P GetPayeeEmail   E
      *================================================================
      *  GetPayeeName  - Assign Payee Name from WDRPP, WMPDP, or WMCDP
      *================================================================
     P GetPayeeName    B
     D GetPayeeName    PI
      *
      *     Access Payment Detail Detail file
      *
      *
     C     keyWDRPL3     Klist
     C                   Kfld                    PACo#
     C                   Kfld                    PAFnd
     C                   Kfld                    PAFyr
     C                   Kfld                    PACase
     C                   Kfld                    PASeq#

     C     keyWDRPL      Klist
     C                   Kfld                    PACo#
     C                   Kfld                    PAFnd
     C                   Kfld                    PAFyr
     C                   Kfld                    PACase
     C                   Kfld                    PAChk#

     C     keyWMCML1     Klist
     C                   Kfld                    PACo#
     C                   Kfld                    PAFnd
     C                   Kfld                    PAFyr
     C                   Kfld                    PACase
      *
     C     keyWMPDL1     Klist
     C                   Kfld                    PAFein
     C                   KFLD                    PAMod
      *
      *
     C     keyWTGTL      klist
     C                   Kfld                    PAco#
     C                   Kfld                    ##TbId
     C                   Kfld                    ##KEY
      *
     C     keyWMCDL      Klist
     C                   Kfld                    CMss#

      /Free
       Chain  keyWDRPL3 prp03;
       If not %Found();
          intTrdt = PATrdt;
          Chain  keyWDRPL  prp000;

          If   not %Found();
             intTrdt  =  PAPodt;
             Chain  keyWDRPL  prp000;
          Endif;
       Endif;

       //Record is found in WDRPP

       If  %found();
          strPayeeNam = RPPnam ;
          strDtlAdr1  = RPAdr1 ;
          strDtlAdr2  = RPAdr2 ;
          intzip     = RPZip  ;

        //LOAD CITY, STATE AND ZIP

          If strDtlAdr2=*Blanks;
             strDtlAdr2 = %trimR(RPcty) + ', ' + RPst + ' ' +
              %editc(intZip1:'X') + '-'+ %editc(intZip2:'X');
             strDtlAdr3=*Blanks;
          Else;
             strDtlAdr3 = %trimR(RPcty) + ', ' + RPst + ' ' +
              %editc(intZip1:'X') + '-'+ %editc(intZip2:'X');
          EndIf;

          If  RPctry <> *blanks and RPctry <> 'USA' ;
             ##TbId =  'CTRY';
             ##KEY  =  RPctry ;
             Chain   keyWTGTL  PGT00;

             If %found();
                strCtrd  = GTDESC ;
             Else;
                strCtrd  = NF;
             Endif;

             If    RPAdr2 =  *BLANKS ;
                strDtlAdr2 =   Rpadr3;
                strDtlAdr3 =  strCtrd;
             Else;
                strDtlAdr3 =  RPAdr3;
             Endif;
          Endif;

          strDtlRmk =  RPrmds;
          If RPrmk1 <> *blanks;
             strKy  = RPrmk1;
             Chain keyWTABL PRC01;
             strDtlRmk1 =  RCDESC;
          else;
             strDtlRmk1 =  *blanks;
          Endif;

          If RPrmk2 <> *blanks;
             strKy  = RPrmk2;
             Chain keyWTABL PRC01;
             strDtlrmk2 =  RCdesc;
          else;
             strDtlrmk2 =  *blanks;
          Endif;

          If RPrmk3 <> *blanks;
             strKy  = RPrmk3;
             Chain keyWTABL PRC01;
             strDtlrmk3 =  RCdesc;
          else;
             strDtlrmk3 =  *blanks;
          Endif;

       //Record Not found in WDRPP
       else;

       //Access Claim Master

          Chain  keyWMCML1  pcm01 ;
          If not %found();
             CMSs# = *zeros ;
          Endif;

       // If FEIN# > 0 & FEIN# NE CMSS#, access Payee Demographics

          strFein  =   %editc(PAfein:'X') ;
          If PAfein <> 0 and strFein <> CMSs#
          and PAretp  <>  'C'  ;

       //  Access Payee Demographics
             Chain   keyWMPDL1   ppd00  ;
             If not %found();
                strPayeeNam =  *BLANKS ;
             else ;
                If  CDFsn = *blanks  ;
                   strPayeeNam   = PDName ;
                else ;
                   ##Last =  PDName ;
                   ##Frst =  PDFsn  ;
                   ##Term =  '1'    ;
                   GetName(##Last:##Frst:##Term);
                   strPayeeNam = ##Last;
                Endif ;

                strDtlAdr1 =  PDadr1 ;
                strDtlAdr2 =  PDadr2 ;
                intzip    =  PDzip  ;

            //   LOAD CITY, STATE AND ZIP

                If strDtlAdr2=*Blanks;
                   strDtlAdr2 = %trimR(PDcty) + ', ' + PDst + ' ' +
                    %editc(intZip1:'X') + '-'+ %editc(intZip2:'X');
                    strDtlAdr3=*Blanks;
                Else;
                   strDtlAdr3 = %trimR(PDcty) + ', ' + PDst + ' ' +
                    %editc(intZip1:'X') + '-'+ %editc(intZip2:'X');
                EndIf;

                If  strDtlAdr2 = ' ' ;
                   strDtlAdr2 = strDtlAdr3 ;
                   strDtlAdr3 = *BLANKS  ;
                Else;
                   strDtlAdr3 =  ##Last;
                Endif;

                If  PDctry <> *blanks and PDctry <> 'USA' ;
                   ##TbId =   'CTRY';
                   ##KEY  =   PDctry ;
                   Chain   keyWTGTL  PGT00;

                   If %found();
                      strCtrd  = GTDESC ;
                   Else;
                      strCtrd  = NF;
                   Endif;

                   If    PDAdr2 =  *BLANKS ;
                      strDtlAdr2 =   PDadr3;
                      strDtlAdr3 =  strCtrd;
                      strDtlAdr3 = CDctry + '  ' + strCtrd;
                   Else;
                      strDtlAdr3 =  PDadr3;
                   Endif;

                Endif;

             Endif;


         //  FEIN# = 0 Or FEIN# = CMSS#, access Claimant Demographics

          Else ;

        // Access Claimant Demographics
        //
             Chain  keyWMCDL  pcd00;
             If   not %Found();
                strPayeeNam =  *BLANKS ;
             else ;
                If  CDFsn = *blanks  ;
                   strPayeeNam  = CDLsn ;
                else ;
                   ##Last =  CDLsn  ;
                   ##Frst =  CDFsn  ;
                   ##Term =  '1'    ;
                   GetName(##Last:##Frst:##Term);
                   strPayeeNam  =  ##last;
                Endif ;

         //   LOAD CITY, STATE AND ZIP

                strDtlAdr1  = CDadr1 ;
                strDtlAdr2  = CDadr2 ;
                intzip     = CDzip  ;

                If strDtlAdr2=*Blanks;
                   strDtlAdr2 = %trimR(CDcty) + ', ' + CDst + ' ' +
                    %editc(intZip1:'X') + '-'+ %editc(intZip2:'X');
                   strDtlAdr3=*Blanks;
                Else;
                   strDtlAdr3 = %trimR(CDcty) + ', ' + CDst + ' ' +
                    %editc(intZip1:'X') + '-'+ %editc(intZip2:'X');
                EndIf;

                If  CDAdr2 =  *BLANKS ;
                   strDtlAdr2 =   CDadr3;
                   strDtlAdr3 =  strCtrd ;
                   strDtlAdr3 = CDctry + '  ' + strCtrd ;
                Else;
                   strDtlAdr3 =  CDadr3;
                Endif;

                If  CDctry <> *blanks and CDctry <> 'USA' ;
                   ##TbId =   'CTRY';
                   ##KEY  =   CDctry ;
                   Chain   keyWTGTL  PGT00;

                   If %found();
                      strCtrd  = GTDESC ;
                   Else;
                      strCtrd  = NF;
                   Endif;

                   If    CDAdr2 =  *BLANKS ;
                      strDtlAdr2 =   CDadr3;
                      strDtlAdr3 =  strCtrd;
                      strDtlAdr3 = CDctry + '  ' + strCtrd;
                   Else;
                      strDtlAdr3 =  CDadr3;
                   Endif;
                Endif ;
             Endif ;
          Endif ;
       Endif ;

       strDtlPNam = strPayeeNam;
      /end-Free


     P GetPayeeName    E
      *================================================================
      *  GetAmount  - Get the Amount
      *================================================================
     P GetAmount       B
     D GetAmount       PI
      *
      /free
       If  PAtype = 'S'or PAtype = 'V' or PAtype = 'R' ;
         decDtlAmount  =  -1 * PAamt ;
       else ;
         decDtlAmount  =  PAamt ;
       Endif;

      /end-free
     P GetAmount       E
      *================================================================
      *  GetReserveInfo - Retrieve Reserve Information
      *================================================================
     P GetReserveInfo  B
     D GetReserveInfo  PI           100

     D reserveInfo     s            100

      /free

       strKy=PAreCd;
       Chain keyWTABL prt00;
       If %Found(WTABL);
         reserveInfo=%Trim(PAreCd) + ' - ' + %Trim(RTdesc) + ' / ' +
          %Trim(PAreTp) + ' - ' + GetReserveType();
       Else;
         reserveInfo=%Trim(PAreCd) + ' / ' + PAreTp + ' - ' +
          GetReserveType();
       EndIf;

       Return reserveInfo;

      /end-free

     P GetReserveInfo  E
      *================================================================
      *  GetReserveType - Retrieve Reserve Type Description
      *================================================================
     P GetReserveType  B
     D GetReserveType  PI            20

     D reserveType     s             20

      /free

       Select;
       When PAreTp='M';
         reserveType='Medical';
       When PAreTp='E';
         reserveType='Expense';
       When PAreTp='L';
         reserveType='Legal';
       When PAreTp='C';
         reserveType='Compensation';
       Other;
         reserveType=*Blanks;
       EndSl;

       Return reserveType;

      /end-free

     P GetReserveType  E
      *================================================================
      *  GetTransInfo - Retrieve Transaction Information
      *================================================================
     P GetTransInfo    B
     D GetTransInfo    PI           100

     D transInfo       s            100

      /free

       strKy=PAtrCd;
       Chain keyWTABL ppc00;
       If %Found(WTABL);
         transInfo=%Trim(PAtrCd) + ' - ' + %Trim(PCdesc) + ' / ' +
          %Trim(PAtype) + ' - ' + GetTransType();
       Else;
         transInfo=%Trim(PAtrCd) + ' / ' + PAtype + ' - ' +
          GetTransType();
       EndIf;

       Return transInfo;

      /end-free

     P GetTransInfo    E
      *================================================================
      *  GetTransType - Retrieve Transaction Type Description
      *================================================================
     P GetTransType    B
     D GetTransType    PI            20

     D transType       s             20

      /free

       Select;
       When PAtype='P';
         transType='Payment';
       When PAtype='M';
         transType='Manual Payment';
       When PAtype='V';
         transType='Void';
       When PAtype='R';
         transType='Recovery';
       When PAtype='A';
         transType='Adjustment';
       When PAtype='S';
         transType='Stop';
       When PAtype='Z';
         transType='Reverse';
       Other;
         transType=*Blanks;
       EndSl;

       Return transType;

      /end-free

     P GetTransType    E
