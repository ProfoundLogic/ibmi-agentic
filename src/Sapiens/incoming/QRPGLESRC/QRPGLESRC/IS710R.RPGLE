     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
    H*********************************************************************
    H/TITLE UDS Upload - Process Data
    H*********************************************************************
    H*  DESCRIPTION: UDS Upload - Process data into sdcmp                *
    H*                                                                   *
    H*  CALLED FROM: IS700C1                                             *
    H*                                                                   *
    H*  CALLS TO:    n/a                                                 *
    H*                                                                   *
    H*  CREATED: 03/31/14      C:280     PROJECT: 114ijm                 *
    H*                                                                   *
    H*********************************************************************
    H*  REVSIONS:                                                        *
     H*                                                               *
     H* 01/16/18 C:062 SUPPORT: 63631(114)
     H*                Include the subdirectory
     H*                                                               *
     H* 04/20/17 C:062 SUPPORT: 46737(114)
     H*                Stop using relative path statements.
     H*                                                               *
     H* 03/03/15 C:280 SUPPORT: PHW33535
     H*                Remove Logic for testing if already exist
     H*                as these documents will be load multiple time
     H*                over life of claim
     H*                                                               *
     H* 03/02/15 C:280 SUPPORT: PHW33535
     H*                ADD GROUP TO FILE FOR MULTIPLE LOADS
     H*                                                               *
     H* 02/18/15 C:280 Phaseware: 33316 csi                           *
     H*                Add code to save path correctly                *
     H*                                                               *
    F*=====================================================================
    F*
     FISFLATLI  if   e           k disk    rename(PSF01:DTL01)
    F*
    F* UDS Flat File by NAIC number \ Select Record type "I"
    F*
     FISFLATLH  if   e           k disk    rename(PSF01:HDR01)
    F*
    F* UDS Flat File by NAIC number \ Select Record type "I"
    F*
     FSTCTL1    if   e           k disk    rename(PCT01:PCT00)
    F*
    F* Remark Type Table File by RMCD
    F*
     FUDSHDRIL  uf a e           k disk    rename(PSH01:PSH00)
    F*
    F* UDS Header File by root
    F*
     FUDSDTLIL  uf a e           k disk    rename(PSD01:PDS00)
    F*
    F* UDS Header File by docn/acl#
    F*
     FWMCMLG    if   e           k disk    rename(PCM01:PCM00)
    F*
    F* Claim File by co#/acl#/fnd/fyr/case
    F*
     FQSYSPRT   o    f  132        printer oflind(*inof)
    F*
    F* Status report
    F*
    D*=====================================================================
    D* Procedure Prototypes
    D*=====================================================================
     D*
     D typ             s              3
     Dfound            s              1
     Dprocess          s              1
     Dxxloc            s              3  0
     Dx                s              3
     Dxx               s              3  0
     Dy                s              3  0
    D*
    D*** Beg Add 03/02/15
    D*
     DEmp#             s              9  0
    D*
    D*** End Add 03/02/15
    D*
     D LO              C                   CONST('abcdefghijklmnopqrst-
     D                                     uvwxyz')
     D*
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ')
     D*
     D*
     Ddigits           c                   '0123456789'
     Dpos              S             10i 0
     D*
     D docn            S            120
     D*
     Dgood             s              5  0
     Dbad              s              5  0
     D*
     DWrkstnid         s              2
     DDocSeq#          s              6
     DSuffix           s              3
     DBatchSeq         s              4
     DDocId            s             12
     DBatchId          s             12
     DFolder           s              8
     DSubFldr          s             12
     DNewPath          s             21
     DRtnCode          s              1
     DPRE              s             13
     Drootdir          s             40
     D*
     Dq#KEY            s             40
     Dq#COMM           s            256
     Dq#CDAT           s              8
     Dq#DCID           s             12
     Dq#PNAM           s             10
     Dq#SECL           s              3
     Dq#USER           s             10
     Dq#PGM            s             10
     Dq#FOLD           s             80
     Dq#DIFL           s              1
     Dq#IPGM           s             10
     Dq#FL01           s              1
     Dq#FL02           s              1
     Dq#RMCD           s              5
     Dq#SEQ#           s              5
     Dq#SBYL           s              1
     Dq#COM2           s             19
     Dq#APID           s              2
     Dq#IDOC           s             12
     Dq#IQUE           s             10
     Dq#IMGF           s              1
     Dq#IPTH           s             10
     Dq#PIDX           s              5
     Dq#QPTY           s              1
     Dq#DCTP           s              5
     Dq#CSID           s              2
     Dq#CODE           s              2
     Dq#TRDT           s              7
     Dq#TRTM           s              6
     Dq#STAT           s              1
     Dq#IUSR           s             10
     Dq#DCDT           s              7
     Dq#FL03           s              1
     Dq#FL04           s              1
     Dq#FL05           s              1
     Dq#IDT            s              7
     Dq#VCFN           s              1
     Dq#DPGM           s             10
     Dq#DKEY           s             40
     Dq#PATH           s             80
     Dq#RPYN           s              1
     Dq#ENV            s              1
     D*
     DFMOBJ            s           1024
     DTOOBJ            s           1024
     DRTNCD            s              2
     Droot             s             10
     D*
     DCVT2R            pr                  ExtPgm('CVT2R')
     D   WrkStnId                     2
     D   DocSeq#                      6
     D   Suffix                       3
     D   BatchSeq                     4
     D   DocId                       12
     D   BatchId                     12
     D   Folder                       8
     D   SubFldr                     12
     D   NewPath                     21
     D   RtnCode                      1
     D   PRE                         13
     D*
     DDA700R           pr                  ExtPgm('DA700R')
     D   q#KEY                       40
     D   q#COMM                     256
     D   q#CDAT                       8
     D   q#DCID                      12
     D   q#PNAM                      10
     D   q#SECL                       3
     D   q#USER                      10
     D   q#PGM                       10
     D   q#FOLD                      80
     D   q#DIFL                       1
     D   q#IPGM                      10
     D   q#FL01                       1
     D   q#FL02                       1
     D   q#RMCD                       5
     D   q#SEQ#                       5
     D   q#SBYL                       1
     D   q#COM2                      19
     D   q#APID                       2
     D   q#IDOC                      12
     D   q#IQUE                      10
     D   q#IMGF                       1
     D   q#IPTH                      10
     D   q#PIDX                       5
     D   q#QPTY                       1
     D   q#DCTP                       5
     D   q#CSID                       2
     D   q#CODE                       2
     D   q#TRDT                       7
     D   q#TRTM                       6
     D   q#STAT                       1
     D   q#IUSR                      10
     D   q#DCDT                       7
     D   q#FL03                       1
     D   q#FL04                       1
     D   q#FL05                       1
     D   q#IDT                        7
     D   q#VCFN                       1
     D   q#DPGM                      10
     D   q#DKEY                      40
     D   q#PATH                      80
     D   q#RPYN                       1
     D   q#ENV                        1
    D*
     DCPYIFSC          pr                  ExtPgm('CPYIFSC')
     D  FMOBJ                      1024
     D  TOOBJ                      1024
     D  RTNCD                         2
    D*
     DRTVDOCROOT       pr                  ExtPgm('RTVDOCROOT')
     D   root                        10
    D*
    D*=====================================================================
    D* Table, Data Structure & Array Definitions
    D*=====================================================================
    D*
    D** Date and Time
    D*
     Ddstime           s              6  0
     Ddsdate           s              6  0
    D*
     D xxpath          s             40
    D*
    D** ISFLAT Detail Data Structure
    D*
     D                 DS
     D SDDETAIL                1   1218
     D SDRECT                  1      1A                                        Record type
     D SDNAIC                  2      6S 0                                      NAIC number
     D  dNAIC                  2      6
     D SDFMST                  7      8A                                        From State
     D SDFMLC                  9     10S 0                                      From Location Code
     D  dFMLC                  9     10
     D  dACL#                 11     26A                                        Incolvent Company
     D SDRCL#                 31     50A                                        Number
     D SDTPA#                 51     80A                                        Reciever Claim
     D SDLCLM                 81    110A                                        Number
     D SDFCLM                111    130A                                        TPA Claim Number
     D SDAID1                131    180A                                        Long Claim Number
     D SDAID2                181    230A                                        Fund Claim Number
     D SDAID3                231    280A                                        Alternate Index 1
     D SDAID4                281    330A                                        Alternate Index 2
     D   DCID                331    360A                                        Alternate Index 3
     D SDDCPG                361    369S 0                                      Alternate Index 4
     D  dDCPG                361    369                                         Alternate Index 4
     D SDCDT                 370    377S 0                                      Document ID
     D  dCDT                 370    377                                         Document ID
     D SDCTM                 378    385S 0                                      Capture Time
     D  dCTM                 378    385                                         Capture Time
     D SDFOLD                386    391                                         Folder Type
     D SDDTYP                392    421                                         Document Type
     D SDDESC                422    549                                         Document Description
     D SDPOL#                550    569                                         Policy Number
     D SDLSDT                570    577S 0                                      Date of Loss
     D  dLSDT                570    577                                         Date of Loss
     D SDINM1                578    607                                         Insured Name Line 1
     D SDINM2                608    637                                         Insured Name Line 2
     D SDCLM#                638    642S 0                                      Claimant Number
     D  dCLM#                638    642                                         Claimant Number
     D SDCNM1                643    672                                         Claimant Name Line 1
     D SDCNM2                673    702                                         Claimant Name Line 2
     D SDPATH                703    958                                         Document Path
     D SDDOCN                959   1214                                         Document File Name
     D SDFILE               1215   1218                                         File Type
    D*
    D** ISFLAT Header Data Structure
    D*
     D                 DS
     D SHHEAD                  1    651
     D SHRECT                  1     20A                                        Record type
     D SHNAIC                 21     25S 0                                      NAIC number
     D  hNAIC                 21     25                                         NAIC number
     D SHRTFN                 26     26A                                        From State
     D SHFST                  27     28A                                        From Location Code
     D SHLCD                  29     30S 0                                      Incolvent Company
     D  hLCD                  29     30                                         Incolvent Company
     D SHSCT                  31     32A                                        Number
     D SHFLCT                 33     34A                                        Reciever Claim
     D SHBCH#                 35     37S 0                                      Number
     D  hBCH#                 35     37                                         Number
     D SHBPDT                 38     45S 0                                      TPA Claim Number
     D  hBPDT                 38     45                                         TPA Claim Number
     D SHBFDT                 46     53S 0                                      Long Claim Number
     D  hBFDT                 46     53                                         Long Claim Number
     D SHBTDT                 54     61S 0                                      Fund Claim Number
     D  hBTDT                 54     61                                         Fund Claim Number
     D SHITYP                 62     64A                                        Alternate Index 1
     D SHRFIN                 65     65A                                        Alternate Index 2
     D SHFILL                 66    651A                                        Alternate Index 3
    D*
    D* DARTS Key
    D*
     D                 DS
     D  CMKEY                  1     40
     D  CO#                    1      3  0
     D  FND                    4      6  0
     D  FYR                    7      9  0
     D  CASE                  10     16  0
    D*
    D** Program information
    D*
     D psds          esds                  extname(szq1p)
    I*
    I*
    C*================================================================
    C*  field defination
    C*================================================================
    C*
     C     *LIKE         DEFINE    SDacl#        CMacl#
    C*
    C*=====================================================================
    C* PList Definitions
    C*=====================================================================
    C*
     C     *entry        plist
     C                   parm                    @@fnd             3
     C                   parm                    @@emp             9
     C                   parm                    @@div             5
     C                   parm                    @@rmcd            5
     C                   parm                    @@path           50
    C*
    C*=====================================================================
    C* Main
    C*=====================================================================
    C*

     /free

      // Date & Time

       Dstime = %Dec(%Time(): *ISO);
       Dsdate = %dec(%date():*mdy);

       *Inof = *On;

      // read document file

       Read  ISFLATLI;

b01    Dow Not %Eof(ISFLATLI);

       SDdetail = Data;

      // already processed ?

       SDacl# = dACL#;

      //***** Beg Del ***** 03/03/15 ********************************

       //***** Chain (SDdocn:SDacl#) UDSDTLIL;

b02    //***** If Not %Found(UDSDTLIL) or
       //*****       %Found(UDSDTLIL) and sdflag = *Blanks;

      //***** End Del ***** 03/03/15 ********************************

      //***** Beg Add ***** 03/02/15 ********************************

       Fnd  = %Dec(@@fnd:3:0);
       Emp# = %Dec(@@emp:9:0);

      //***** End Add ***** 03/02/15 ********************************

      // get claim master

       chain (777:SDacl#) WMCMLG;

b02    If %Found(WMCMLG);

      // build key

       Co#  = 777;
       Fnd  = cmFnd;
       Fyr  = cmfyr;
       Case = cmcase;

      // get directory/subdirectory

       wrkstnid = 'UD';

       CVT2R(wrkstnid:docseq#:suffix:batchseq:docid:
             batchid:folder:subfldr:newpath:rtncd:pre);

      // get doc root

       Root = *Blanks;

       RtvDocRoot(Root);

       found = *On;

b03    Dou Found = *Off;

       XXloc = 0;
       XXloc = %Scan('\':SDpath);

b04    If XXloc <> 0;
          SDpath = %Replace('/':SDpath:XXloc);
x04    Else;
          Found = *Off;
e04    Endif;

e03    Enddo;

      // DOC

       xxloc = 0;
       xxloc = %scan('.DOC':SDdocn);

b03    If xxloc <> 0;
         xxloc = xxloc + 1;
         typ  = %subst(SDdocn:xxloc:3);
x03    Else;

       xxloc = 0;
       xxloc = %scan('.doc':SDdocn);

b04    If xxloc <> 0;
         xxloc = xxloc + 1;
         typ  = %subst(SDdocn:xxloc:3);
         typ  = %xlate(lo:up:typ:1);
x04    Else;

      // TIF

       xxloc = %scan('.TIF':SDdocn);

b05    If xxloc <> 0;
         xxloc = xxloc + 1;
         typ  = %subst(SDdocn:xxloc:3);
X05    Else;

       xxloc = %scan('.tif':SDdocn);

b06    If xxloc <> 0;
         xxloc = xxloc + 1;
         typ  = %subst(SDdocn:xxloc:3);
         typ  = %xlate(lo:up:typ:1);
X06    Else;

      // PDF

       xxloc = %scan('.PDF':SDdocn);

b07    If xxloc <> 0;
         xxloc = xxloc + 1;
         typ  = %subst(SDdocn:xxloc:3);
X07    Else;

       xxloc = %scan('.pdf':SDdocn);

b08    If xxloc <> 0;
         xxloc = xxloc + 1;
         typ  = %subst(SDdocn:xxloc:3);
         typ  = %xlate(lo:up:typ:1);
x08    Else;

      // TXT

       xxloc = %scan('.TXT':SDdocn);

b09    If xxloc <> 0;
         xxloc = xxloc + 1;
         typ  = %subst(SDdocn:xxloc:3);
x09    Else;

       xxloc = %scan('.txt':SDdocn);

b10    If xxloc <> 0;
         xxloc = xxloc + 1;
         typ  = %subst(SDdocn:xxloc:3);
         typ  = %xlate(lo:up:typ:1);
x10    Else;

      // XLS

       xxloc = %scan('.XLS':SDdocn);

b11    If xxloc <> 0;
         xxloc = xxloc + 1;
         typ  = %subst(SDdocn:xxloc:3);
x11    Else;

       xxloc = %scan('.xls':SDdocn);

b12    If xxloc <> 0;
         xxloc = xxloc + 1;
         typ  = %subst(SDdocn:xxloc:3);
         typ  = %xlate(lo:up:typ:1);
x12    Else;

      // RTG

       xxloc = %scan('.RTG':SDdocn);

b13    If xxloc <> 0;
         xxloc = xxloc + 1;
         typ  = %subst(SDdocn:xxloc:3);
x13    Else;

       xxloc = %scan('.rtg':SDdocn);

b14    If xxloc <> 0;
         xxloc = xxloc + 1;
         typ  = %subst(SDdocn:xxloc:3);
         typ  = %xlate(lo:up:typ:1);
x14    Else;

      // URL

       xxloc = %scan('.URL':SDdocn);

b15    If xxloc <> 0;
         xxloc = xxloc + 1;
         typ  = %subst(SDdocn:xxloc:3);
X15    Else;

       xxloc = %scan('.url':SDdocn);

b16    If xxloc <> 0;
         xxloc = xxloc + 1;
         typ  = %subst(SDdocn:xxloc:3);
         typ  = %xlate(lo:up:typ:1);
X16    Else;

      // HTM

       xxloc = %scan('.HTM':SDdocn);

b17    If xxloc <> 0;
         xxloc = xxloc + 1;
         typ  = %subst(SDdocn:xxloc:3);
X17    Else;

       xxloc = %scan('.htm':SDdocn);

b18    If xxloc <> 0;
         xxloc = xxloc + 1;
         typ  = %subst(SDdocn:xxloc:3);
         typ  = %xlate(lo:up:typ:1);
X18    Else;

      // MSG

       xxloc = %scan('.MSG':SDdocn);

b19    If xxloc <> 0;
         xxloc = xxloc + 1;
         typ  = %subst(SDdocn:xxloc:3);
X19    Else;

       xxloc = %scan('.msg':SDdocn);

b20    If xxloc <> 0;
         xxloc = xxloc + 1;
         typ  = %subst(SDdocn:xxloc:3);
         typ  = %xlate(lo:up:typ:1);
e20    Endif;

e19    Endif;

e18    Endif;

e17    Endif;

e16    Endif;

e15    Endif;

e14    Endif;

e13    Endif;

e12    Endif;

e11    Endif;

e10    Endif;

e09    Endif;

e08    Endif;

e07    Endif;

e06    Endif;

e05    Endif;

e04    Endif;

e03    Endif;

      // assign new document name

       xxloc = %scan('.':docid);
       xxloc = xxloc + 1;

b03    Select;

x03    when typ = 'TIF';
         docid = %Replace('TIF':docid:XXloc);

x03    when typ = 'PDF';
         docid = %Replace('PDF':docid:XXloc);

x03    when typ = 'RTF';
         docid = %Replace('RTF':docid:XXloc);

x03    when typ = 'DOC';
         docid = %Replace('DOC':docid:XXloc);

x03    when typ = 'TXT';
         docid = %Replace('TXT':docid:XXloc);

x03    when typ = 'XLS';
         docid = %Replace('XLS':docid:XXloc);

x03    when typ = 'URL';
         docid = %Replace('URL':docid:XXloc);

x03    when typ = 'HTM';
         docid = %Replace('HTM':docid:XXloc);

x03    when typ = 'MSG';
         docid = %Replace('MSG':docid:XXloc);

e03    endsl;

      // build copy statement

       //*** Begin Chg *** 04/20/17 ********************************
       //FMOBJ = 'UDS/IMAGES' + %trim(SDpath) + %trim(SDdocn);
       FMOBJ = '/UDS/IMAGES' + %trim(SDpath) + %trim(SDdocn);
       //*** End   Chg *** 04/20/17 ********************************
       TOOBJ = '/' + %TRIM(Root) +
               '/' + %TRIM(Folder) +
               '/' + %TRIM(Subfldr) +
               '/' + %TRIM(docid);

       Rtncd = *Blanks;

      // copy doc

       CpyIfsc(Fmobj:Toobj:Rtncd);

      // copy sucessfull

         docn =  %trim(SDdocn);

b03    If Rtncd = *Blanks;

          Good = Good + 1;

          *In90 = *Off;

      // set up key field

          Q#key = Cmkey;
          Q#csid = *Blanks;
          Q#cDat = %char(%DATE:*CYMD);
          Q#dcId = Docid;
          Q#pnam = 'W0050R';
          Q#secl = *Blanks;
          Q#sbyl = 'N';
          Q#user = Q1user;
          Q#pgm  = Q1pgm;
          //*** Begin Chg *** 01/16/18 ***********************************
          //**********Q#fold = Folder;
          Q#fold = %Trim(Folder) + '/' + %Trim(subFldr);
          //*** End   Chg *** 01/16/18 ***********************************
          Q#apid = *Blanks;
          Q#idoc = *Blanks;
          Q#ique = *Blanks;
          Q#ique = *Blanks;
          Q#imgf = *Blanks;
          Q#ipth = *Blanks;
          Q#pidx = *Blanks;
          Q#qpty = *Blanks;
          Q#dctp = *Blanks;
          Q#iPgm = Q1pgm;
          Q#rmcd = @@Rmcd;
          Q#com2 = *Blanks;
          Q#APID = *Blanks;
          Q#IDOC = *Blanks;
          Q#IQUE = *Blanks;
          Q#IMGF = 'O';
          Q#IPTH = *Blanks;
          Q#PIDX = *Blanks;
          Q#QPTY = *Blanks;
          Q#DCTP = *Blanks;
          Q#CSID = *Blanks;
          Q#CODE = *Blanks;
          Q#TRDT = *Blanks;
          Q#TRTM = *Blanks;
          Q#STAT = *Blanks;
          Q#IUSR = *Blanks;
          Q#DCDT = *Blanks;
          Q#FL05 = *Blanks;
          Q#IDT  = *Blanks;
          Q#VCFN = *Blanks;
          Q#DPGM = *Blanks;
          Q#DKEY = *Blanks;
          Q#PATH = Toobj;
          Q#RPYN = *Blanks;

      // get remark code

          Chain @@Rmcd STCTL1;

b04       If %Found(STCTL1);

             Q#comm = CTdesc;
             Q#difl = CTfl01;

b05          If Q#dcid <> *Blanks;
                Q#fl01 = 'Y';
x05          Else;
                Q#fl01 = *Blanks;
e05          Endif;

b05          If CTdprg > 0;
                Q#fl02 = 'Y';
x05          Else;
                Q#fl02 = *Blanks;
e05          Endif;

e04       Endif;

          Q#seq# = *Blanks;
          Q#code = *Blanks;

      // write remark code

          DA700R(Q#key:Q#comm:Q#cdat:Q#dcid:Q#pnam:Q#secl:Q#user:
                 Q#pgm:Q#fold:Q#diFl:Q#iPgm:Q#fl01:Q#fl02:Q#rmCd:
                 Q#seq#:Q#sbyl:Q#com2:Q#apid:Q#idoc:Q#ique:Q#imgf:
                 Q#ipth:Q#pidx:Q#qpty:Q#dctp:Q#csid:Q#code:Q#trdt:
                 Q#trtm:Q#stat:Q#iusr:Q#dcdt:Q#fl03:Q#fl04:Q#fl05:
                 Q#idt:Q#vcfn:Q#dpgm:Q#dkey:Q#path:Q#rpyn:Q#env);

b04       If *Inof;
             Except Header;
             *Inof = *Off;
e04       Endif;

          Except Detail;

      // write detail record

          SDacl# = dACL#;

          Chain (SDdocn:SDacl#:Fnd:Emp#:DCID) UDSDTLIL;

b04       If not %Found(UDSDTLIL);

             SDdetail = Data;

      // ensure numeric fields are correct

           pos = %check(digits:dnaic);

b05        if pos <> 0;
              dnaic = *zeros;
e05        endif;

           pos = %check(digits:dfmlc);

b05        if pos <> 0;
              dfmlc = *zeros;
e05        endif;

           pos = %check(digits:ddcpg);

b05        if pos <> 0;
              ddcpg = *zeros;
e05        endif;

           pos = %check(digits:dcdt);

b05        if pos <> 0;
              dcdt = *zeros;
e05        endif;

           pos = %check(digits:dctm);

b05        if pos <> 0;
              dctm = *zeros;
e05        endif;

           pos = %check(digits:dlsdt);

b05        if pos <> 0;
              dlsdt = *zeros;
e05        endif;

           pos = %check(digits:dclm#);

b05        if pos <> 0;
              dclm# = *zeros;
e05        endif;

             SDuser  = Q1user;
             SDpgm   = Q1pgm;
             SDtime  = %Dec(%Time(): *ISO);
             SDdt    = %dec(%date():*CYMD);
             SDflag  = 'Y';

      //***** Beg Add ***** 03/02/15 ********************************

             SDfnd   = Fnd;
             SDemp#  = Emp#;

      //***** End Add ***** 03/02/15 ********************************

             Write PDS00;

x04       Else;

             SDuser  = Q1user;
             SDpgm   = Q1pgm;
             SDtime  = %Dec(%Time(): *ISO);
             SDdt    = %dec(%date():*CYMD);
             SDflag  = 'Y';

           Update PDS00;

e04       Endif;

x03    Else;

      // write detail record

          SDacl# = dACL#;

          Chain (SDdocn:SDacl#:Fnd:Emp#:DCID) UDSDTLIL;

b04       If not %Found(UDSDTLIL);

             SDdetail = Data;

      // ensure numeric fields are correct

           pos = %check(digits:dnaic);

b05        if pos <> 0;
              dnaic = *zeros;
e05        endif;

           pos = %check(digits:dfmlc);

b05        if pos <> 0;
              dfmlc = *zeros;
e05        endif;

           pos = %check(digits:ddcpg);

b05        if pos <> 0;
              ddcpg = *zeros;
e05        endif;

           pos = %check(digits:dcdt);

b05        if pos <> 0;
              dcdt = *zeros;
e05        endif;

           pos = %check(digits:dctm);

b05        if pos <> 0;
              dctm = *zeros;
e05        endif;

           pos = %check(digits:dlsdt);

b05        if pos <> 0;
              dlsdt = *zeros;
e05        endif;

           pos = %check(digits:dclm#);

b05        if pos <> 0;
              dclm# = *zeros;
e05        endif;

             SDuser  = Q1user;
             SDpgm   = Q1pgm;
             SDtime  = %Dec(%Time(): *ISO);
             SDdt    = %dec(%date():*CYMD);

             Write PDS00;

x04       Else;

             SDuser  = Q1user;
             SDpgm   = Q1pgm;
             SDtime  = %Dec(%Time(): *ISO);
             SDdt    = %dec(%date():*CYMD);

             Update PDS00;

e04       Endif;

          Bad = Bad + 1;

          *In90 = *On;

b04       If *Inof;
             Except Header;
             *Inof = *Off;
e04       Endif;

          Except Detail;

e03    Endif;

e02    Endif;

      //***** Beg Del ***** 03/03/15 ********************************
e01    //**** Enddo;
      //***** End Del ***** 03/03/15 ********************************

       Read  IsFlatli;

e01    Enddo;

b01    If *inof;
          Except Header;
          *Inof = *Off;
e01    Endif;

      // read header DOC

       Read  Isflatlh;

      // write header file

       SHhead = Data;

       xxpath = @@path;

       Chain xxpath UDSHDRIL;

b04    If not %Found(UDSHDRIL);

      // ensure numeric fields are correct

           pos = %check(digits:hnaic);

b05        if pos <> 0;
              hnaic = *zeros;
e05        endif;

           pos = %check(digits:hlcd);

b05        if pos <> 0;
              hlcd = *zeros;
e05        endif;

           pos = %check(digits:hbch#);

b05        if pos <> 0;
              hbch# = *zeros;
e05        endif;

           pos = %check(digits:hbpdt);

b05        if pos <> 0;
              hbpdt = *zeros;
e05        endif;

           pos = %check(digits:hbfdt);

b05        if pos <> 0;
              hbfdt = *zeros;
e05        endif;

           pos = %check(digits:hbtdt);

b05        if pos <> 0;
              hbtdt = *zeros;
e05        endif;

          SHuser  = Q1user;
          SHpgm   = Q1pgm;
          SHtime  = %Dec(%Time(): *ISO);
          SHdt    = %dec(%date():*CYMD);
         // Beg Chg ****** 02/18/15 ****************************
          Shroot  = xxpath;
         // End Chg ****** 02/18/15 ****************************
          SHflag  = 'Y';

          Write PSH00;

x04    Else;

          SHuser  = Q1user;
          SHpgm   = q1pgm;
          SHtime  = %Dec(%Time(): *ISO);
          SHdt    = %dec(%date():*CYMD);
          SHflag  = 'Y';

          Update PSH00;

e04    Endif;

       Except Total;

       *Inlr = *On;

      /end-free

     O*
    O**************************************************************************************
     O*
     Oqsysprt   e            header         1  3
     O                                            5 'Time:'
     O                       dstime              14 '  :  :  '
     O                                           45 'UDS Upload'
     O                                          122 'Date:'
     O                       dsdate        y    131
     O*
     O          e            header         2
     O                                            4 'Pgm:'
     O                       q1pgm               15
     O                       @@path              67
     O                                          122 'Page:'
     O                       page          z    131
     O*
     O          e            detail         1
     O                                           30 'From Document Name'
     O*
     O          e            detail         2
     O                       docn           b   120
     O*
     O          e            detail         1
     O                                           10 'Folder'
     O                                           25 'Sub-Folder'
     O                                           38 'Document'
     O                                           97 'Status'
     O*
     O          e            detail         2
     O              n90      folder              11
     O              n90      subfldr             26
     O              n90      docid               40
     O              n90                          96 'Completed'
     O               90                         112 'Source document not found'
     O*
     O          e            total          1
     O                                           26 '# Completed Moves'
     O                       good          z     32
     O                                           54 '# Incomplete Moves'
     O                       bad           z     60
     O*
