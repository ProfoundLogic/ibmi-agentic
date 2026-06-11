     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H**********************************************************************
     H/TITLE MPIPBDTPOP - Physician Detail Popup Service Program
     H DFTNAME(MPIPBDTPOP)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H/DEFINE   PROFOUNDUI
     H**********************************************************************
      *===============================================================*
      *  Created   : 04/24/26                                         *
      *  Programmer: C:079                                            *
      *  Project   : MC448 - Provider Bills UX                        *
      *===============================================================*
      *  Description:  Service program which contains the screen files
      *                and processing for Physician Bill Detail Popups
      *
      *  The popups included are:
      *    Physician Detail   (for adding/editing Physician Detail Info)
      *
      **************************************************************************
      * REVISIONS:
     *
      **************************************************************************
     H nomain
    f*
     fMpiPbDtPopcf   e             workstn
     f                                     HANDLER('PROFOUNDUI(HANDLER)')
     fsmcol     if   e           k disk    rename(pco01 : pco0l)
     ffaehl     if   e           k disk
     ffddhl     uf a e           k disk
     ffdcal10   uf a e             disk
     f                                     infds(cafds)
     ffdcalf    if   e           k disk    Rename(pca01:pca0f)
     ffaeop     if   e             disk
     ffddel     if   e           k disk
     ffmcxl     if   e           k disk
     ffmprl     if   e           k disk

     d cafds           ds
     d  dbrrn                397    400b 0
     d psds          esds                  extname(SZQ1P)

     d clientID        s              3    inz('000')
     d isError         s              1n   inz('0')

     d* Proto-types

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d/copy sPrDate
     d/copy sPrGenPop

       //===================================================================
       // Physician Detail Add/Edit Popup
       //===================================================================
     P PhyDtlEdit      B                   export
     D PhyDtlEdit      PI
     D inCO#                          3  0 const
     D inRRN1                         9  0 const
     D inRRN2                         9  0 const
     D inMode                         1a   const

     D PhyEditMod      s               n   Inz(*off)
     D intSeq#         s              2s 0
     D intDseq         s                   Like(CAdSeq)
     D strPrCd         s                   Like(DHprCd)

       GetClientID(clientID);

       scHdRrn = inRrn1;
       scAdjSt = EOast;

       Chain inRrn1 peo01;

       scHdRrn = inRrn1;
       scAdjSt = EOast;

       Select;
       When inMode = 'E';
          Chain inRrn2 pca01;
          If %Found(FDCAL10);
             Select;
             When EObtyp = 'N';
                Chain (CAco#:CAsub#:CAfnd:CAfyr:CAcase:CAeob#:CAdseq) pdh01;
                If %Found(FDDHL);
                   PhyEditMod = *on;
                   PnlTitle = 'Physician Detail Edit';
                Endif;
             Endsl;
          Endif;
       When inMode = 'A';
          Select;
          When EObtyp = 'N';
             Clear rcdPbPhEdt;
             PhyEditMod = *off;
             PnlTitle = 'Physician Detail Add';
          Endsl;
       Endsl;

       Exsr SetVisibility;
       Exsr SetChoicesDBfile;

       If inMode = 'E';
          Exsr MoveDbScreen;
       Endif;

       ExFmt rcdPbPhEdt;

       Select;
       When btnSave = *on;
          btnSave = *off;
          Exsr ValidatePhyDtl;
          If isError = *off;
             Exsr PhysDtlSave;
          Endif;
          *inlr = *on;
          Return;
       When btnCancel = *on;
          btnCancel = *off;
          unlock fdcal10;
          unlock fddhl;
          *inlr = *on;
          Return;
       Endsl;

       //===================================================================
       // Physician Detail Move Database Fields to the Screen
       //===================================================================
       Begsr MoveDbScreen;
          // FDDHP
          scPos  = %Dec(DHpos:3:0);
          scPrCd = DHprCd;
          scMod1 = DHmod1;
          scMod2 = DHmod2;
          scMod3 = DHmod3;
          scMod4 = DHmod4;
          scDiagA = DHdXr1;
          scDiagB = DHdXr2;
          scDiagC = DHdXr3;
          scDiagD = DHdXr4;
          // Flexible Headings
          Chain (CAco#:CAsub#:CAfnd) pcx01;
          If %Found(FMCXL);
             scHdFsRed = %Trim(CXhMra) + ' ' + 'Amount';
             scHdNwRed = %Trim(CXhNet) + ' ' + 'Amount';
             scHdOth1Rd = %Trim(CXhOt1) + ' ' + 'Amount';
             scHdOth2Rd = %Trim(CXhOt2) + ' ' + 'Amount';
             scHdOt1TyC = %Trim(CXhOt1) + ' ' + 'Type Code';
             scHdOt2TyC = %Trim(CXhOt2) + ' ' + 'Type Code';
          Endif;
          // FDCAP
          scCsCd = CAcsCd;
          scFdat = DtMdYy(CAfDat);
          scTdat = DtMdYy(CAtDat);
          scUnit = CAunts;
          scChrg = CAchrg;
          scMra  = CAmra;
          scNdis = CAnDis;
          scOthr = CAothr;
          scOth2 = CAoth2;
          scPaid = CApaid;
          scOt1TyCd = CAoT1C;
          scOt2TyCd = CAoT2C;

          // Display EOB Codes from FDDEP
          intSeq# = 0;
          Setll (CAco#:CAsub#:CAfnd:CAfyr:CAcase:CAEob#:CAdSeq) pde01;
          Reade (CAco#:CAsub#:CAfnd:CAfyr:CAcase:CAEob#:CAdSeq) pde01;
          Dow Not %Eof(FDDEL);
             intSeq# = intSeq# + 1;
             Select;
             When intSeq# = 1;
                scEobCd1 = DEeobCd;
             When intSeq# = 2;
                scEobCd2 = DEeobCd;
             When intSeq# = 3;
                scEobCd3 = DEeobCd;
             Endsl;
             Reade (CAco#:CAsub#:CAfnd:CAfyr:CAcase:CAEob#:CAdSeq) pde01;
          Enddo;
       Endsr;
       //===================================================================
       // Physician Detail Move Screen Fields to the Database
       //===================================================================
       Begsr MoveScreenDb;
          CAcsCd = scCsCd;
          CAfDat = DtCyMd(scFdat);
          CAtDat = DtCyMd(scTdat);
          CAunts = scUnit;
          DHpos  = ' ' + %Editc(scPos:'X');
          DHprCd = scPrCd;
          strPrCd = %Subst(scPrCd:1:5);
          Setgt (EOco#:strPrCd:CAfDat) ppr01;
          Readpe (EOco#:strPrCd) ppr01;
          Dow %Found(FMPRL);
             If PRstat <> 'I';
                DHprDs = PRdsc1;
                Leave;
             Else;
               Readpe (EOco#:strPrcd) ppr01;
             Endif;
          Enddo;
          DHmod1 = scMod1;
          DHmod2 = scMod2;
          DHmod3 = scMod3;
          DHmod4 = scMod4;
          DHdXr1 = scDiagA;
          DHdXr2 = scDiagB;
          DHdXr3 = scDiagC;
          DHdXr4 = scDiagD;
          CAchrg = scChrg;
          CAmra  = scMra;
          CAnDis = scNdis;
          CAothr = scOthr;
          CAoth2 = scOth2;
          CApaid = scPaid;
          CAoT1C = scOt1TyCd;
          CAoT2C = scOt2TyCd;

          // Display EOB Codes from FDDEP
          intSeq# = 0;
          Setll (CAco#:CAsub#:CAfnd:CAfyr:CAcase:CAEob#:CAdSeq) pde01;
          Reade (CAco#:CAsub#:CAfnd:CAfyr:CAcase:CAEob#:CAdSeq) pde01;
          Dow Not %Eof(FDDEL);
             intSeq# = intSeq# + 1;
             Select;
             When intSeq# = 1;
                scEobCd1 = DEeobCd;
             When intSeq# = 2;
                scEobCd2 = DEeobCd;
             When intSeq# = 3;
                scEobCd3 = DEeobCd;
             Endsl;
             Reade (CAco#:CAsub#:CAfnd:CAfyr:CAcase:CAEob#:CAdSeq) pde01;
          Enddo;
       Endsr;
       //===================================================================
       // Physician Detail Set Choices Database Files
       //===================================================================
       Begsr SetVisibility;
        // Set Visibilty on various fields
        Select;
        When EObtyp = 'N';
           Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) peh01;
           If %Found(FAEHL);
              Select;
              When EHicdInd = 'Y';
                 If EHicd1001 <> *blanks;
                    isDiagA = *on;
                 Endif;
                 If EHicd1002 <> *blanks;
                    isDiagB = *on;
                 Endif;
                 If EHicd1003 <> *blanks;
                    isDiagC = *on;
                 Endif;
                 If EHicd1004 <> *blanks;
                    isDiagD = *on;
                 Endif;
              When EHicdInd <> 'Y';
                 If EHdia1 <> *blanks;
                    isDiagA = *on;
                 Endif;
                 If EHdia2 <> *blanks;
                    isDiagB = *on;
                 Endif;
                 If EHdia2 <> *blanks;
                    isDiagC = *on;
                 Endif;
                 If EHdia4 <> *blanks;
                    isDiagD = *on;
                 Endif;
              Endsl;
           Endif;
        Endsl;
       Endsr;
       //===================================================================
       // Physician Detail Set Choices Database Files
       //===================================================================
       Begsr SetChoicesDBfile;
        // Set Choices Database File files for the selected state
        chFTPMPxx = 'FTPMP' + %Trim(CAaSt);     //From Modifier,From Mo     difi
        chFTPSPxx = 'FTPSP' + %Trim(CAaSt);     //From Place of Service
        //***chFTPAPxx = 'FTPAP' + %Trim(CAaSt);     //Body part
        chFMPRPxx = 'FMPRP' + %Trim(CAaSt);     //Procedure Code File
       Endsr;
       //===================================================================
       // Physician Detail Error Checking
       //===================================================================
       Begsr ValidatePhyDtl;
          Exsr ClearErrors;
          isError = *off;
       Endsr;
       //===================================================================
       // Save Physician Detail Data
       //===================================================================
       Begsr PhysDtlSave;

       If PhyEditMod = *on;
          Exsr MoveScreenDb;
          // FDDHP
          DHpgm  = Q1PGM;
          DHuser = Q1USER;
          DHtime = Timeto6(SystemTime());
          DHdt   = Dateto7(SystemDate());
          Update pdh01;
          // FDCAP
          CApgm  = Q1PGM;
          CAuser = Q1USER;
          CAtime = Timeto6(SystemTime());
          CAdt   = Dateto7(SystemDate());
          Update pca01;
       Else;
          // Determine Detail Charge Sequence Number
          intDseq = *Hival;
          Setgt (EOco#:EOsub#:EOfnd:EOfyr:EOcase:intDseq) pca0f;
          Readpe (EOco#:EOsub#:EOfnd:EOfyr:EOcase) pca0f;
          If %Found(FDCALF);
             intDseq = CAdSeq + 1;
          Else;
             intDseq = 1;
          Endif;
          // FDDHP
          Clear PDH01;
          Clear PCA01;
          Exsr MoveScreenDb;
          DHdSeq = intDseq;
          DHco#  = EOco#;
          DHsub# = EOsub#;
          DHfnd  = EOfnd;
          DHfyr  = EOfyr;
          DHcase = EOcase;
          DHeob# = EOeob#;
          DHss#  = EOss#;
          DHacDt = EOacDt;
          DHipgm = Q1PGM;
          DHiusr = Q1USER;
          DHitim = Timeto6(SystemTime());
          DHidt  = Dateto7(SystemDate());
          DHpgm  = Q1PGM;
          DHuser = Q1USER;
          DHtime = Timeto6(SystemTime());
          DHdt   = Dateto7(SystemDate());
          Write pdh01;
          // FDCAP
          CAdSeq = intDseq;
          CAco#  = EOco#;
          CAsub# = EOsub#;
          CAfnd  = EOfnd;
          CAfyr  = EOfyr;
          CAcase = EOcase;
          CAeob# = EOeob#;
          CAss#  = EOss#;
          CAacDt = EOacDt;
          CAipgm = Q1PGM;
          CAiusr = Q1USER;
          CAitim = Timeto6(SystemTime());
          CAidt  = Dateto7(SystemDate());
          CApgm  = Q1PGM;
          CAuser = Q1USER;
          CAtime = Timeto6(SystemTime());
          CAdt   = Dateto7(SystemDate());
          Write pca01;
       Endif;

       Endsr;
       //===================================================================
       // Clear all Errors
       //===================================================================
       Begsr ClearErrors;

       Endsr;

     P PhyDtlEdit      E
