     hcopyright('(c) 2023 Sapiens, Inc. All rights reserved.')
     h**********************************************************************
     h/title WPIGRPPOP - Group Master Detail
     h/define   PROFOUNDUI
     h option(*nodebugio: *srcstmt)
     h nomain
     h/define WPIGRPPOP
     h**********************************************************************
      *===============================================================*
      *  Created   : 05/28/13                                         *
      *  Programmer: C:279                                            *
      *  Project   : WC2196                                           *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      * 12/27/23  C:342 Project: WC2932                               *
      *                 1.Expanded Group Selection window to provide  *
      *                   Addional row for To Groups Subfile          *
      *                 2.Added Expiration Date colomn in sflToGrp    *
      *                   subfile.                                    *
      *                 3.Increased Panel height and width for Copy   *
      *                   Endorsement option.                         *
      *                 4.Made some changes in onload event for       *
      *                   rcdCopyCfg record format with 12/27/23 date *
      *                 5.Change Top TopNDupEnd to 615px from 405px   *
      *                   for Endorsement form copy option.           *
     *                                                               *
      * 09/04/23  C:342 Project: WC2829                               *
      *                 Added copy option for Short Rate Grid.        *
      *                 DSPF Changes:                                 *
      *                  1) Added Short Rate grid.                    *
      *                  1) Added Duplicate Short Rate grid.          *
     *                                                               *
      * 07/28/23  C:300 Support: 113253                               *
      *                 Add Short Name column to Group Select Grid on *
      *                 (sflToGrp) on 'Rating Factor Copy' screen     *
      *                                                               *
      * 06/02/23  C:300 Support: 110463                               *
      *                 Rename 'Copy Configuration' toolbar option to *
      *                 'Rating Factor Copy'                          *
     *                                                               *
      * 05/30/23  C:342 Support: 110307                               *
      *                 Sort the record in Duplicate grids on the     *
      *                 basis of group instead of State while using   *
      *                 copy configuration.                           *
     *                                                               *
      * 05/29/23  C:300 Support: 110456                               *
      *                 Filter applied to the "To Group(s)" table is  *
      *                 being cleared upon clicking the "Verify" button.
      *                 In DSPF: Changed the Persist State property to*
      *                 TRUE for SflToGroups and SflDupStatRate Grids *
     *                                                               *
      * 05/23/23  C:291 Project: WC2788                               *
      *                 Changes to premium Discount Grid:             *
      *                 - When one of the record is checked,it checks/*
      *                   unchecks all the records with matching      *
      *                   state/effective date/discount code.         *
      *                   Find duplicate premium discount records by  *
      *                   state/effective date/discount code.         *
      *                   Changes to Javascript:                      *
      *                   Onchange event on CbxPremDisc.              *
      *                   Changes to Onload event of RcdCopyCfg.      *
     *                                                               *
      * 03/27/23  C:300 Support: 107761 (SUNZ)                        *
      *                 Issue with Copy Endorsements                  *
      *                 - On clicking Replace the screen gets stuck in*
      *                   Loading status for a while before closing   *
     *                                                               *
      * 09/26/22 C:342  Support: 101193                               *
      *                 -Added copy option for State Compensation     *
      *                  Benefit grid                                 *
      *                 -Added copy option for State Compensation     *
      *                  Rate By Benefit grid                         *
      * 01/17/22  C:291 Project: WC2788                               *
      *                 Add GroupCopySetup procedure                  *
      *                                                               *
      * 03/22/17  C:33  Project: 148WAB                               *
      *                 Do not display (M) for CLT148                 *
      *                                                               *
    *    10/07/13 C:190 Project 25WSS                               *
    *                   Add a radio button Premium Verbiage setting.*
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*

     fwpigrppop cf   e             workstn extfile('WPIGRPPOP')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(sflGrpPlYr:rrnGrPlYr)
     f                                     sfile(sflRegInfo:rrnRegInfo)
     f                                     sfile(sflChpInfo:rrnChpInfo)
     f                                     sfile(sflBnkInfo:rrnBnkInfo)
     f                                     sfile(sflToGrp:rrnToGrp)
     f                                     sfile(sflDupEnd:rrnDEnd)
     f                                     sfile(sflStRt:rrnStRt)
     f                                     sfile(sflPrmDis:rrnPrmDis)
     f                                     sfile(sflIncLmt:rrnIncLmt)
     f                                     sfile(sflSmDed:rrnSmDe)
     f                                     sfile(sflDStRt:rrnDStRt)
     f                                     sfile(sflDPrmDis:rrnDPrmDis)
     f                                     sfile(sflDIncLmt:rrnDIncLmt)
     f                                     sfile(sflDSmDed:rrnDSmDe)
      //*** Begin Add *** 09/26/22 *******************************************
     f                                     sfile(sflDupSCB:rrnDSCB)
     f                                     sfile(sflDupSCBR:rrnDSCBR)
      //*** End   Add *** 09/26/22 *******************************************
      //*** Begin Add *** 09/04/23 *******************************************
     f                                     sfile(sflShRt:rrnShRt)
     f                                     sfile(sflDShRt:rrnDShRt)
      //*** End   Add *** 09/04/23 *******************************************
     f                                     usropn
      //*** Begin Add *** 09/26/22 *******************************************
      * Compensation Benefit/Reimbursement File
     fwdrbp     uf a e             disk    rename(prb01:prb0p)
     fwdrbl1    uf   e           k disk    rename(prb01:prb00)
     f                                     infds(dbfds13)

      * State Compensation Rates by Benefit Type
     fwdr1p     uf a e             disk    rename(pr101:pr10p)
     fwdr1l     uf   e           k disk    rename(pr101:pr100)
     f                                     infds(dbfds27)
      //*** End   Add *** 09/26/22 *******************************************
     fwmfnp     uf a e             disk    rename(pfn01:pfn0p)
     f                                     infds(dbfd4)
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     fwmfcl     uf a e           k disk    rename(pfc01:pfc00)
     fwmgcl     uf a e           k disk    rename(pgc01:pgc00)
     fwmrep     if   e             disk    rename(pre01:pre0p)
     fwmrel     uf a e           k disk    rename(pre01:pre00)
     f                                     infds(dbfd1)
     fwmcpp     if   e             disk    rename(pcp01:pcp0p)
     fwmcpl     uf a e           k disk    rename(pcp01:pcp00)
     f                                     infds(dbfd2)
     fwmbip     if   e             disk    rename(pbi01:pbi0p)
     fwmbil     if   e           k disk    rename(pbi01:pbi00)
     f                                     infds(dbfd3)
     fwmbil2    uf a e           k disk    rename(pbi01:pbi02)
     fwmbil6    if   e           k disk    rename(pbi01:pbi06)
     fwmsgl1    if   e           k disk
     fwdf1l2    if   e           k disk    rename(pf101:pf102)
     f                                     infds(dbfd5)
     fwdf1p     uf a e             disk    rename(pf101:pf10p)
     fwdf1l     if   e           k disk    rename(pf101:pf100)
     fwdf2l     uf a e           k disk    rename(pf201:pf200)
     fwdell3    if   e           k disk    rename(pel01:pel03)
     fwdfhl     uf a e           k disk    rename(pfh01:pfh00)
     fwsckl     if   e           k disk    rename(pck01:pck00)
     fwtabl     if   e           k disk
     fssylf     if   e           k disk    include(swc01)
      *** Beg Add ***** 01/17/22 ************************************
     fststl3    if   e           k disk
     fWtenp     uf a e             disk    rename(pen01:pen0p)
     fWtenl     uf   e           k disk    rename(pen01:pen00)
     f                                     Infds(dbfds6)
     fWdrrp     uf a e             disk    rename(prr01:prr0p)
     fwdrrl1    uf   e           k disk    infds(dbfds7)
     fWmdsp     uf a e             disk    rename(pds01:pds0p)
     fwmdsl2    uf   e           k disk    rename(pds01:pds02)
     f                                     infds(dbfds8)
     fWtlmp     uf a e             disk    rename(plm01:plm0p)
     fwtlml2    uf   e           k disk    rename(plm01:plm02)
     f                                     infds(dbfds9)
     fWtddp     uf a e             disk    rename(pdd01:pdd0p)
     fwtddl1    uf   e           k disk    infds(dbfds10)
     fWiratdft  uf a e             disk    rename(pdf01:pdf0p)
     fWiratdftl uf   e           k disk    rename(pdf01:pdf00)
     fWiratddt  uf a e             disk    rename(pdt01:pdt0p)
     fWiratddtl uf   e           k disk    rename(pdt01:pdt00)
     fWmldp     uf a e             disk    rename(pld01:pld0p)
     fWmldl4    uf   e           k disk    rename(pld01:pld04)
      *** End Add ***** 01/17/22 ************************************
      //*** Begin Add *** 09/04/23 *******************************************
      * Short-Rate Master File
     fwmstp     uf a e             disk    rename(pst01:pst0p)
     fwmstl     uf   e           k disk    rename(pst01:pst00)
     f                                     infds(dbfds11)
     fWdstp     uf a e             disk    rename(pst01:pst02)
     fWdstl     uf   e           k disk    rename(pst01:pst03)

      //*** End   Add *** 09/04/23 *******************************************

     d dbfd1           ds
     d  dbrrn1               397    400b 0
     d dbfd2           ds
     d  dbrrn2               397    400b 0
     d dbfd3           ds
     d  dbrrn3               397    400b 0
     d dbfd4           ds
     d  dbrrn4               397    400b 0
     d dbfd5           ds
     d  dbrrn5               397    400b 0
      *** Beg Add ***** 01/17/22 ************************************
     d dbfds6          ds
     d dbrrn6                397    400b 0
     d dbfds7          ds
     d dbrrn7                397    400b 0
     d dbfds8          ds
     d dbrrn8                397    400b 0
     d dbfds9          ds
     d dbrrn9                397    400b 0
     d dbfds10         ds
     d dbrrn10               397    400b 0
      *** End Add ***** 01/17/22 ************************************

      //*** Begin Add *** 09/26/22 *******************************************
     d dbfds13         ds
     d dbrrn13               397    400b 0
     d dbfds27         ds
     d dbrrn27               397    400b 0
      //*** End   Add *** 09/26/22 *******************************************
      //*** Beg   Add *** 09/04/23 *******************************************
     d dbfds11         ds
     d dbrrn11               397    400b 0
      //*** End   Add *** 09/04/23 *******************************************
      /copy wprGrpPop
      /copy wprUsrPrms
      /copy wprTable
      /copy wprGenPop
      /copy wprGroup
      /copy wprbank
      /copy sprString
      /copy sprDate
      /copy sprGenErr
      *** Beg Add ***** 01/17/22 ************************************
      /copy sPrCompany
      *** End Add ***** 01/17/22 ************************************

      * Check Folder
     dChkFolder        pr                  extpgm('SVALFLR')
     d outFold                             like(outFold)
     d outRtcd                             like(outRtcd)

      * Check Document
     dChkDoc           pr                  extpgm('SVALDOC')
     d outDoc                              like(outDoc)
     d outFld                              like(outFld)
     d outRtcd                             like(outRtcd)

      * Create JobQ
     dcrtJobQ          pr                  extpgm('W0010C02')
     d outCo                               like(outCo)
     d outFnd#                             like(outFnd#)
     d outJobQ                             like(outJobQ)
     d outName                             like(outName)
     d outSbs                              like(outSbs)
     d outSbs1                             like(outSbs1)
     d outMsg                              like(outMsg)
     d outRtcd                             like(outRtcd)

      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

      * Check EDI format id required
     dGetEDIFmID       pr                  extpgm('WA075R')
     d outCo#                              like(outCo#)
     d outFnd                              like(outFnd)
     d outRtcd                             like(outRtcd)

      * Check Object
     dGetObject        pr                  extpgm('SRCHKOBJ')
     d outName                             like(outName)
     d outType                             like(outType)
     d outRtcd                             like(outRtcd)

      * Retrieve JobQ attributes
     dRtvJobQ          pr                  extpgm('RTVJOBQA')
     d outJobQ                             like(outJobQ)
     d outLib                              like(outLib)
     d outSts                              like(outSts)
     d outName                             like(outName)
     d outCnt                              like(outCnt)

      * Validate JobQ Name
     dVldJobQ          pr                  extpgm('SRCKOBJN')
     d outName                             like(outName)
     d outRtcd                             like(outRtcd)

     d psds          esds                  extname(szq1p)

      *** Beg Add ***** 01/17/22 ******************************************************
     d index           s              5  0
     d index1          s              5  0
     d arrGrpList      s              3  0 dim(9999)
     d ptrEndrList     s               *
     d arrEndrs        ds                  dim(9999) likeds(dsEndrs)
     d                                     based(ptrEndrList)
     d dsEndrs         ds                  qualified
     d EndrFrm                       15
     d EndrRrn                        9  0
      *** End Add ***** 01/17/22 ******************************************************

      *** Beg Add ***** 09/26/22 ******************************************************
       // for State Compensation Benefits
     d ptrSCBList      s               *
     D arrSCB          ds                  dim(9999) likeds(dsSCB)
     d                                     based(ptrSCBList)
     ddsSCB            ds                  qualified
     d SCBState                       2
     d SCBRrn                         9  0
     d SCBEfdt                        7  0

       // for State Compensation Rate By Benefit Type
     d ptrSCBRList     s               *
     D arrSCBR         ds                  dim(9999) likeds(dsSCBR)
     d                                     based(ptrSCBRList)
     ddsSCBR           ds                  qualified
     d SCBRState                      2
     d SCBRRrn                        9  0
     d SCBREfdt                       7  0
     d SCBRbr2                        2

      *** End Add ***** 09/26/22 ******************************************************
      * wrok variables
     d clientID        s              3
     d intCo#          s              3  0 inz
     d intFnd          s              3  0 inz
     d isLogo          s              1    inz
     d isFirst         s               n   inz(*off)
     d isFstBnk        s               n   inz(*off)
     d outCo#          s              3  0 inz
     d outFnd          s              3  0 inz
     d outDoc          s             12    inz
     d outFld          s             80    inz
     d outName         s             10    inz
     d outFold         s             40    inz
     d outType         s              7    inz
     d outRtcd         s              1    inz
     d outLib          s             10    inz
     d outSts          s             10    inz
     d outJobQ         s             10    inz
     d outCo           s              3    inz
     d outFnd#         s              3    inz
     d outSbs          s             10    inz
     d outSbs1         s             10    inz
     d outMsg          s            132    inz
     d outCnt          s             10  0 inz
     d outRrn          s              9  0
     d saveDate        s                   inz like(ELdt)
     d saveTime        s                   inz like(ELtime)

     d                 ds
     d  xxsbs                  1     10    dtaara(JOBQSBS)
      *=====================================================================
      * DspGrpDtl - Display Group Detail
      *  Parm1   - Group RRN
      *  Parm2   - Mode(1A) 'A'-Add, 'C'-Change, 'D'-Inquiry
      *  Returns - success indicator
      *=====================================================================
     pDspGrpDtl        b                   export
     dDspGrpDtl        pi              n
     d inFNrrn                        9  0
     d inMode                         1

     d dsGroupDtl    e ds                  extname(wpigrppop:rcdGrpWdw) inz
     d ptrGroupDtl     s               *   inz
     d isError         s               n   inz
     d iOpen           s               n   inz
     d ExitNow         s               n   inz(*off)
     d xxRtcd          s               n   inz

     c     keyWMFNL      klist
     c                   kfld                    FNco#
     c                   kfld                    FNfnd

      /free

       If not %Open(wpigrppop);
          Open wpigrppop;
          iOpen = *On;
       Endif;
       //***  Begin Add  ***  03/22/17  ********************************
       GetClientId(ClientID);
       //***  Begin Add  ***  03/22/17  ********************************

       xxRtcd = *Off;
       qdjob  = q1job;
       qdcusr = q1cusr;
       qdjobn = q1jobn;
       If inMode <> 'A' And inMode <> 'C' And inMode <> 'D';
          Return xxRtcd;
       Endif;

       Clear dsGroupDtl;
       ptrGroupDtl = %Addr(dsGroupDtl);

       If inMode = 'A';
          scGrpWdLft = 260;
          scGrpWdTop = 105;
          dspBtnPlAd = *off;
          LoadDetail(inFNRrn :ptrGroupDtl:inMode);
       Else;
          If not LoadDetail(inFNrrn :ptrGroupDtl:inMode);
             Return xxRtcd;
          Endif;
       EndIf;
       Exsr SetDisplayIndicators;

       Dow ExitNow = *off and inMode = 'A';
          ExFmt rcdGrpWdw;
          Select;
          When scDtlNxt = *on;
             scDtlNxt = *off;
             Exsr ProcessSave;
             If xxRtcd = *on and ExitNow = *on;
                procAddGrp(inFNrrn:inMode);
             Endif;
          When scDtlCan = *On;
             ExitNow = *On;
             xxRtcd = *Off;
          EndSl;
       EndDo;
       If inMode = 'C';
          isError = not ValidGrpDtl(inMode:ptrGroupDtl:inFNrrn);
       Endif;
       If %Open(wpigrppop) And iOpen;
          Close wpigrppop;
       Endif;
       Return xxRtcd;
       //*************************************************************************
       // ProcessSave
       //*************************************************************************
       BegSr ProcessSave;

       If ValidGrpDtl(inMode:ptrGroupDtl:inFNrrn);
          If UpdateGrpDtl(ptrGroupDtl:inFNrrn:inMode);
             ExitNow = *On;
             xxRtcd = *On;
          EndIf;
       EndIf;

       Endsr;
       //*************************************************************************
       // SetDisplayIndicators - Subroutine
       //*************************************************************************
       BegSr SetDisplayIndicators;

       Select;
       When inMode = 'A';
          dspDtlGrp# = *off;
          dspDtlGrpN = *off;
          dspDtlCs#  = *off;
          dspDtlBtn  = *on;
          scShowWdw  = *on;

       When inMode = 'D';
          dspDtlGrp# = *on;
          dspDtlGrpN = *on;
          dspDtlCs#  = *on;
          dspDtlBtn  = *off;
          scShowWdw  = *off;

       When inMode = 'C';
          dspDtlGrp# = *on;
          dspDtlCs#  = *on;
          dspDtlBtn  = *off;
          dspDtlGrpN = *off;
          scShowWdw  = *off;
       Endsl;
       //***  Begin Add  ***  03/22/17  ********************************
       visM = *on;
       Select;
       When clientID = '148';
          visM = *off;
       Endsl;
       //***  Begin Add  ***  03/22/17  ********************************

       EndSr;
      /end-free
     pDspGrpDtl        e
      //*** Beg Add ***** 01/17/22 *****************************************
      *=====================================================================
      * GroupCopySetup - Copy Group setup
      *  Parm1 - RRN  (9A)   Group master (WMFNP)
      *  Parm2 - Data (500a) "Copy Configuration" or "Copy Endorsements"
      *=====================================================================
     pGroupCopySetup   b                   export
     dGroupCopySetup   pi
     d inRRN                          9  0 const
     d inData                       500a   const
     d inPtrEndFrm                     *   const Options(*nopass)
     d ExitNow1        s               n   inz
     d isDupStRt       s               n   inz
     d isDupPrDs       s               n   inz
     d isDupInLm       s               n   inz
     d isDupSmDs       s               n   inz
     d intco#          s              3  0
     d scToGroup#      s              3  0 inz
     d strState        s             10a
     d strSvState      s              2a
     d flgWriteRec     s              1a
     d currentDate     s               d   inz datfmt(*usa)
     d effDate7        s              7  0
     d saveRrnStRt     s                   like(rrnStRtRow)
     d saveRrnPrDs     s                   like(rrnPrDsRow)
     d saveRrnInLm     s                   like(rrnInLmRow)
     d saveRrnSmDe     s                   like(rrnSmDeRow)
      *** Beg Add ***** 09/04/23 *****************************
     d saveRrnShRt     s                   like(rrnShRtRow)
     d isDupShRt       s               n   inz
      *** End Add ***** 09/04/23 *****************************
     darrPrmDisc       s              3    dim(9999) varying inz
     darrIncrLmt       s             12    dim(9999) varying inz
     darrSmDed         s              5    dim(9999) varying inz
     darrEffDate       s              7  0 dim(9999)
      *** Beg Add ***** 05/23/23 *****************************
     d svStPD          s                   like(scStPD)
     d svEfdtPD        s                   like(scEfdtPD)
     d svCodePD        s                   like(scCodePD)
     d svSelPrmDs      s                   like(scSelPrmDs)
      *** End Add ***** 05/23/23 *****************************
     ddsPrmDisc        ds
     d prmDsState              1      2
     d prmDsCode               3      4
     ddsIncrLmt        ds
     d inLmState               1      2
     d inLmCode                3     12
     ddsSmDed          ds
     d smDeState               1      2
     d smDeCode                3      5
      //*** Begin Add *** 09/26/22 *******************************************
     d kkkey           s                   like(b2key)
      //*** End   Add *** 09/26/22 *******************************************
      //*** Begin Add *** 09/26/22 *******************************************
     c     keyWtabl      klist
     c                   kfld                    intCo#
     c                   kfld                    kkkey
      //*** End   Add *** 09/26/22 *******************************************
      /free

       If not %Open(wpigrppop);
          Open wpigrppop;
       Endif;

       intCo# = userCo#(q1user);
       currentDate = systemDate();

       Clear RcdCopyCfg;
       visToGrp = *Off;
       scCpyPnlHt = 315;
       scPnlWdth = 670;
       Exsr ClrSflToGrp;
       Exsr LoadSflToGrp;

       Chain(n) inRRN pfn0p;

       Select;
       //*** Beg Chg ***** 06/02/23 ****************************************
       //*** When inData = 'Copy Configuration';
       When inData = 'Rating Factor Copy';
        //*** End Chg ***** 06/02/23 ****************************************
          Exsr CopyConfiguration;
       When inData = 'Copy Endorsements';
          Exsr CopyEndorsements;
       //*** Begin Add *** 09/26/22 *************************************
       When inData = 'Copy SCB' ;
          Exsr CopyStateCompBen;
       When inData = 'Copy SCBR' ;
          Exsr CopyStateCompRtBen;
       //*** End   Add *** 09/26/22 **************************************
       Endsl;

       If %Open(wpigrppop);
          Close wpigrppop;
       Endif;

       //****************************************************************************
       // ClrCfgSubfiles - Clear Configuration Subfile
       //****************************************************************************
       Begsr ClrCfgSubfiles;
        //Statistical reporing/Rating subfile
        RrnStRt = *Zeros;
        dspStRt = *Off;
        clrStRt = *On;
        Write rcdCopyCfg;
        dspStRt = *On;
        clrStRt = *Off;
        //Premium discount subfile
        RrnPrmDis = *Zeros;
        dspPrmDis = *Off;
        clrPrmDis = *On;
        Write rcdCopyCfg;
        dspPrmDis = *On;
        clrPrmDis = *Off;
        //Increased Limits subfile
        RrnIncLmt = *Zeros;
        dspIncLmt = *Off;
        clrIncLmt = *On;
        Write rcdCopyCfg;
        dspIncLmt = *On;
        clrIncLmt = *Off;
        //Small Deductibles subfile
        RrnSmDe = *Zeros;
        dspSmDe = *Off;
        clrSmDe = *On;
        Write rcdCopyCfg;
        dspSmDe = *On;
        clrSmDe = *Off;
        //***  Beg Add  **** 09/04/23 *******************************
        //Short Rate subfile
        RrnShRt = *Zeros;
        dspShRt = *Off;
        clrShRt = *On;
        Write rcdCopyCfg;
        dspShRt = *On;
        clrShRt = *Off;
        //***  End Add  **** 09/04/23 *******************************
       Endsr;
       //****************************************************************************
       // ClrDupCfgSubfiles - Clear duplicate Configuration Subfiles
       //****************************************************************************
       Begsr ClrDupCfgSubfiles;
        Clear sflDStRt;
        Clear sflDPrmDis;
        Clear sflDIncLmt;
        Clear sflDSmDed;
        //***  Beg Add  **** 09/04/23 *******************************
        Clear sflDShRt;
        //***  End Add  **** 09/04/23 *******************************
        //Statistical reporing/Rating subfile
        RrnDStRt = *Zeros;
        dspDStRt = *Off;
        clrDStRt = *On;
        Write rcdCopyCfg;
        dspDStRt = *On;
        clrDStRt = *Off;
        //Premium discount subfile
        RrnDPrmDis = *Zeros;
        dspDPrmDis = *Off;
        clrDPrmDis = *On;
        Write rcdCopyCfg;
        dspDPrmDis = *On;
        clrDPrmDis = *Off;
        //Increased Limits subfile
        RrnDIncLmt = *Zeros;
        dspDIncLmt = *Off;
        clrDIncLmt = *On;
        Write rcdCopyCfg;
        dspDIncLmt = *On;
        clrDIncLmt = *Off;
        //Small Deductibles subfile
        RrnDSmDe = *Zeros;
        dspDSmDe = *Off;
        clrDSmDe = *On;
        Write rcdCopyCfg;
        dspDSmDe = *On;
        clrDSmDe = *Off;
        //***  Beg Add  **** 09/04/23 *******************************
        //Short Rate subfile Duplicate
        RrnDShRt = *Zeros;
        dspDShRt = *Off;
        clrDShRt = *On;
        Write rcdCopyCfg;
        dspDShRt = *On;
        clrDShRt = *Off;
        //***  End Add  **** 09/04/23 *******************************
       Endsr;
       //****************************************************************************
       // ClrSflDupEnd - Clear Duplicate Endorsement subfile
       //****************************************************************************
       Begsr ClrSflDupEnd;
          Clear sflDupEnd;
          RrnDEnd = 0;
          dspDEnd = *Off;
          clrDEnd = *On;
          Write rcdCopyCfg;
          clrDEnd = *Off;
          dspDEnd = *On;
       Endsr;
       //****************************************************************************
       // ClrSflToGrp - Clear To Group Subfile
       //****************************************************************************
       Begsr ClrSflToGrp;
          RrnToGrp = *Zeros;
          dspToGrpSf = *Off;
          dspToGrpCt = *Off;
          clrToGrp = *On;
          Write rcdCopyCfg;
          dspToGrpSf = *On;
          dspToGrpCt = *On;
          clrToGrp = *Off;
       Endsr;
       //****************************************************************************
       // CopyConfiguration - Copy Configurations to selected groups                *
       //****************************************************************************
       Begsr CopyConfiguration;
        //*** Beg Chg ***** 06/02/23 ****************************************
        //*** scCpCfgHdg = 'Copy Configuration';
        scCpCfgHdg = 'Rating Factor Copy';
        //*** End Chg ***** 06/02/23 ****************************************
        scFrGroup# = FNfnd;
        visStatRat = *On;       //Checkbox visibility indicators
        visPrmDisc = *On;
        visIncrLmt = *On;
        visDeduct  = *On;
        visNDupEnd = *Off;
        visDupStRt = *Off;
        visDupPrDs = *Off;
        visDupInLm = *Off;
        visDupSmDs = *Off;
        //*** Beg Chg ***** 09/04/23 ****************************************
        visShtRate = *On;
        visDupShRt = *Off;
        //*** End Chg ***** 09/04/23 ****************************************

        scCpyPnlHt = 305;
        Exsr ClrCfgSubfiles;
        Exsr ClrDupCfgSubfiles;
        Exsr LoadCfgGrids;
        Dou ExitNow1 = *On;
           If rrnPosStRt <> 0 and rrnPosStRt <= rrnStRtRow;
              rrnStRt = rrnPosStRt;
           Else;
              rrnStRt = 1;
           Endif;
           saveRrnStRt = rrnStRtRow;
           If rrnPosPrDs <> 0 and rrnPosPrDs <= rrnPrDsRow;
              rrnPrmDis = rrnPosPrDs;
           Else;
              rrnPrmDis = 1;
           Endif;
           saveRrnPrDs = rrnPrDsRow;
           If rrnPosInLm <> 0 and rrnPosInLm <= rrnInLmRow;
              rrnIncLmt = rrnPosInLm;
           Else;
              rrnIncLmt = 1;
           Endif;
           saveRrnInLm = rrnInLmRow;
           If rrnPosSmDe <> 0 and rrnPosSmDe <= rrnSmDeRow;
              rrnSmDe = rrnPosSmDe;
           Else;
              rrnSmDe = 1;
           Endif;
           saveRrnSmDe = rrnSmDeRow;
           //*** Beg Add ***** 09/04/23 ****************************************
           If rrnPosShRt <> 0 and rrnPosShRt <= rrnShRtRow;
              rrnShRt = rrnPosShRt;
           Else;
              rrnShRt = 1;
           Endif;
           saveRrnShRt = rrnShRtRow;
           //*** End Add ***** 09/04/23 ****************************************
           Exfmt rcdCopyCfg;
           If rrnStRtRow = 0 and saveRrnStRt <> 0;
              rrnStRtRow = saveRrnStRt;
           Endif;
           If rrnPrDsRow = 0 and saveRrnPrDs <> 0;
              rrnPrDsRow = saveRrnPrDs;
           Endif;
           If rrnInLmRow = 0 and saveRrnInLm <> 0;
              rrnInLmRow = saveRrnInLm;
           Endif;
           If rrnSmDeRow = 0 and saveRrnSmDe <> 0;
              rrnSmDeRow = saveRrnSmDe;
           Endif;
           //*** Beg Add ***** 09/04/23 ****************************************
           If rrnShRtRow = 0 and saveRrnShRt <> 0;
              rrnShRtRow = saveRrnShRt;
           Endif;
           //*** End Add ***** 09/04/23 ****************************************
           Select;
           When btnVerify = *On;
              Exsr ClrDupCfgSubfiles;
              Exsr LoadDupCfgGrids;
              isDupStRt = *Off;
              isDupPrDs = *Off;
              isDupInLm = *Off;
              isDupSmDs = *Off;
              //*** Beg Add ***** 09/04/23 ****************************************
              isDupShRt = *Off;
              //*** End Add ***** 09/04/23 ****************************************
              If rrnDStRt > 0;
                 visDupStRt = *On;
                 isDupStRt = *On;
              Endif;
              If rrnDPrmDis > 0;
                 visDupPrDs = *On;
                 isDupPrDs = *On;
              Endif;
              If rrnDIncLmt > 0;
                 visDupInLm = *On;
                 isDupInLm = *On;
              Endif;
              If rrnDSmDe > 0;
                 visDupSmDs = *On;
                 isDupSmDs = *On;
              Endif;
              //*** Beg Add ***** 09/04/23 ****************************************
              If rrnDShRt > 0;
                 visDupShRt = *On;
                 isDupShRt = *On;
              Endif;
              //*** End Add ***** 09/04/23 ****************************************
              //*** Beg Chg ***** 09/04/23 ****************************************
              //*** If rrnDStRt = 0 and rrnDPrmDis = 0 and rrnDIncLmt = 0
              //***  and rrnDSmDe = 0;
              If rrnDStRt = 0 and rrnDPrmDis = 0 and rrnDIncLmt = 0
               and rrnDSmDe = 0 and rrnDShRt = 0;
              //*** End Chg ***** 09/04/23 ****************************************
                 scDupFnd = 'N';
              Else;
                 scDupFnd = 'Y';
              Endif;
              rrnDStRt = 1;
              rrnDPrmDis = 1;
              rrnDIncLmt = 1;
              rrnDSmDe = 1;
              //*** Beg Add ***** 09/04/23 ****************************************
              rrnDShRt = 1;
              //*** End Add ***** 09/04/23 ****************************************
              If rrnPsToGrp <> 0;
                 rrnToGrp = rrnPsToGrp;
              Else;
                 rrnToGrp = 1;
              Endif;
              btnVerify = *Off;
           When btnCopy = *On;
              Exsr CopyStatRate;
              Exsr CopyPremDiscount;
              Exsr CopyIncrLimit;
              Exsr CopySmallDeduct;
              //*** Beg Add ***** 09/04/23 ****************************************
              Exsr CopyShrtRate;
              //*** End Add ***** 09/04/23 ****************************************
              PopOneBtn('Data has been copied to destination group(s).' :
               'Copy Configuration');
              ExitNow1 = *On;
           //*** Beg Add ***** 05/23/23 *************************
           When btnPrmDisc = *On;
              Exsr SelectPremDiscount;
           //*** End Add ***** 05/23/23 *************************
           When BtnCancel = *On;
              ExitNow1 = *On;
           Endsl;
        Enddo;
       Endsr;
       //****************************************************************************
       // CopyEndorsements - Copy Endorsements to selected groups                   *
       //****************************************************************************
       Begsr CopyEndorsements;
        ptrEndrList = inPtrEndFrm;
        scCpCfgHdg = 'Copy Endorsements';
        scFrGroup# = FNfnd;
        visDupEnd = *Off;
        visNDupEnd = *Off;
        //*** Beg Chg ***** 12/27/23 *****************************
        //*** scCpyPnlHt = 315;
        scCpyPnlHt = 480;
        scPnlWdth  = 770;
        //*** End Chg ***** 12/27/23 *****************************
        BtnReplEnd = *Off;
        BtnSkipDup = *Off;
        BtnCopy  = *Off;

        Dou ExitNow1 = *On;
           Exfmt rcdCopyCfg;
           Select;
           When btnVerify = *On;
              Exsr ClrSflDupEnd;
              index = 1;
              Dow index <= 9999 and arrEndrs(index).EndrFrm <> ' ';
                  ReadC sflToGrp;
                  Dow Not %Eof;
                     If scSelToGrp = 'Y';
                        scToGroup# = scToGrp;
                        nxChgToGrp = *On;
                        update sflToGrp;
                        chain(n) (intCo#:scToGroup#:
                         arrEndrs(index).EndrFrm) pen00;
                        If %Found(wtenl);
                           ScDEndForm = ENForm;
                           ScDEndDesc = ENdsc2;
                           scDEndRrn# = dbrrn6;
                           scDEndGrp =  ENfnd;
                           rrnDEnd = rrnDEnd + 1;
                           Write sflDupEnd;
                        Endif;
                     Endif;
                     ReadC sflToGrp;
                  Enddo;
                  index += 1;
              Enddo;
              If rrnDEnd > 0;
                 visDupEnd = *On;
                 visNDupEnd = *Off;
                 //*** Beg Chg ***** 12/27/23 *****************************
                 //*** TopDupEnd = 401;
                 //*** scCpyPnlHt = 521;
                 TopDupEnd = 605;
                 scCpyPnlHt = 751;
                 //*** End Chg ***** 12/27/23 *****************************
                 dspDEnd = *On;
                 dspDEndCtl = *On;
              Endif;
              If rrnDEnd = *Zero and BtnCancel = *Off; //and scError1 = *blank;
                 visNDupEnd = *On;
                 leftBtnCpy = 535;
                 visDupEnd = *Off;
                 //*** Beg Chg ***** 12/27/23 *****************************
                 //*** scCpyPnlHt = 347;
                 //*** TopNDupEnd = 405;
                 scCpyPnlHt = 570;
                 TopNDupEnd = 615;
                 //*** End Chg ***** 12/27/23 *****************************
              Endif;
              rrnDEnd = 1;
              If rrnPsToGrp <> 0;
                 rrnToGrp = rrnPsToGrp;
              Else;
                 rrnToGrp = 1;
              Endif;
              btnVerify = *Off;
           When BtnSkipDup = *On;
              CopyEndrsForm(scFrGroup#:'S');
              ExitNow1 = *On;
           //Duplicates found in To Group: Replace Duplicate Endorsements in To Group
           When BtnReplEnd = *On;
              CopyEndrsForm(scFrGroup#:'R');
              ExitNow1 = *On;
           //No Duplicates found in To Group: Copy all the Endorsements to To Group
           When BtnCopy = *On;
              CopyEndrsForm(scFrGroup#:'A');
              ExitNow1 = *On;
           When BtnCancel = *On;
              ExitNow1 = *On;
           Endsl;
        Enddo;

       Endsr;
       //****************************************************************************
       // CopyIncrLimit - Copy Increased Limit Records                              *
       //****************************************************************************
       Begsr CopyIncrLimit;
        If scIncrLmt = 'Y';
           ReadC sflIncLmt;
           Dow Not %Eof;
              If scSelIncLm = 'Y';
                 ReadC sflToGrp;
                 Dow Not %Eof;
                    If scSelToGrp = 'Y';
                       nxChgToGrp = *On;
                       Update sflToGrp;
                       Select;
                       //Skip/Replace when duplicates found in To Group
                         When isDupInLm = *On;
                            Select;
                            When scDupInLm = 'R';
                             //Check if Increased Limit record exist in To Group
                             effDate7 = DateTo7(scEfdtIL);
                             Chain (intCo#:scToGrp:scStIL:effDate7:
                              scKey) plm02;
                             If %Found(wtlml2);
                                //Delete the existing Increased Limit record
                                //and write new Increased Limit record in To Group
                                Delete plm02;
                                Exsr WriteWTlmp;
                             Else;
                                Exsr WriteWTlmp;
                             Endif;
                          //Skip duplicates When duplicate found in To Group
                          When scDupInLm = 'S';
                             //Check if form exist in To Group
                             effDate7 = DateTo7(scEfdtIL);
                             Chain(n) (intCo#:scToGrp:scStIL:effDate7:
                              scKey) plm02;
                             If not %Found(wtlml2);
                                //Write to To Group if Increased Limit record does
                                // not exist in To Group
                                Exsr WriteWTlmp;
                             Endif;
                          Endsl;
                       //No dupllicates found in To Group, Copy all Increased Limit records
                       When isDupInLm = *Off;
                          Exsr WriteWTlmp;
                       Endsl;
                    Endif;
                    ReadC sflToGrp;
                 Enddo;
              Endif;
              ReadC sflIncLmt;
           Enddo;
        Endif;
       Endsr;
       //****************************************************************************
       // CopyPremDiscount - Copy Premium Discount Records                          *
       //****************************************************************************
       Begsr CopyPremDiscount;
        //*** Beg Chg ***** 05/23/23 *********************************
       // If scPrmDisc = 'Y';
       //    ReadC sflPrmDis;
       //    Dow Not %Eof;
       //       If scSelPrmDs = 'Y';
       //          ReadC sflToGrp;
       //          Dow Not %Eof;
       //             If scSelToGrp = 'Y';
       //                nxChgToGrp = *On;
       //                Update sflToGrp;
       //                Select;
       //                //Skip/Replace when duplicates found in To Group
       //                When isDupPrDs = *On;
       //                   Select;
       //                   When scDupPrDs = 'R';
       //                      //Check if Premium Discount record exist in To Group
       //                      effDate7 = DateTo7(scEfdtPD);
       //                      Chain (intCo#:scToGrp:scStPD:effDate7:
       //                       scCodePD:scTypeCode:scLmt) pds02;
       //                      If %Found(wmdsl2);
       //                         //Delete the existing Premium Discount record
       //                         //and write new Premium Discount record in To Group
       //                         Delete pds02;
       //                         Exsr WriteWMdsp;
       //                      Else;
       //                         Exsr WriteWMdsp;
       //                      Endif;
       //                   //Skip duplicates When duplicate found in To Group
       //                   When scDupPrDs = 'S';
       //                      //Check if form exist in To Group
       //                      effDate7 = DateTo7(scEfdtPD);
       //                      Chain(n) (intCo#:scToGrp:scStPD:effDate7:
       //                       scCodePD:scTypeCode:scLmt) pds02;
       //                      If not %Found(wmdsl2);
       //                         //Write to To Group if Premium Discount record does
       //                         // not exist in To Group
       //                         Exsr WriteWMdsp;
       //                      Endif;
       //                   Endsl;
       //                //No dupllicates found in To Group, Copy all Premium Discount records
       //                When isDupPrDs = *Off;
       //                   Exsr WriteWMdsp;
       //                Endsl;
       //             Endif;
       //             ReadC sflToGrp;
       //          Enddo;
       //       Endif;
       //       ReadC sflPrmDis;
       //    Enddo;
       // Endif;
       //** new code
        If scPrmDisc = 'Y';
           Clear svStPD;
           Clear svEfdtPD;
           Clear svCodePD;
           ReadC sflPrmDis;
           Dow Not %Eof;
              If scStPD <> svStPD or scEfdtPD <> svEfdtPD or
               scCodePD <> svCodePD;
                 If scSelPrmDs = 'Y';
                    svStPD = scStPD;
                    svEfdtPD = scEfdtPD;
                    svCodePD = scCodePD;
                    ReadC sflToGrp;
                    Dow Not %Eof;
                       If scSelToGrp = 'Y';
                          nxChgToGrp = *On;
                          Update sflToGrp;
                          Select;
                          //Skip/Replace when duplicates found in To Group
                          When isDupPrDs = *On;
                             Select;
                             When scDupPrDs = 'R';
                                //Check if Premium Discount record exist in To Group
                                effDate7 = DateTo7(scEfdtPD);
                                Chain(n) (intCo#:scToGrp:scStPD:effDate7:
                                 scCodePD) pds02;
                                If %Found(wmdsl2);
                                   //Delete the existing Premium Discount record
                                   Setll (intCo#:scToGrp:scStPD:effDate7:
                                    scCodePD) pds02;
                                   Reade (intCo#:scToGrp:scStPD:effDate7:
                                    scCodePD) pds02;
                                   Dow Not %Eof(wmdsl2);
                                      Delete pds02;
                                      Reade (intCo#:scToGrp:scStPD:effDate7:
                                       scCodePD) pds02;
                                   Enddo;
                                Endif;
                                //Write new Premium Discount record in To Group
                                Exsr WriteWMdsp;
                             //Skip duplicates When duplicate found in To Group
                             When scDupPrDs = 'S';
                                //Check if record exist in To Group
                                effDate7 = DateTo7(scEfdtPD);
                                Chain(n) (intCo#:scToGrp:scStPD:effDate7:
                                 scCodePD) pds02;
                                If not %Found(wmdsl2);
                                   //Write to To Group if Premium Discount record does
                                   // not exist in To Group
                                   Exsr WriteWMdsp;
                                Endif;
                             Endsl;
                          //No dupllicates found in To Group, Copy all Premium Discount records
                          When isDupPrDs = *Off;
                             effDate7 = DateTo7(scEfdtPD);
                             Exsr WriteWMdsp;
                          Endsl;
                       Endif;
                       ReadC sflToGrp;
                    Enddo;
                 Endif;
              Endif;
              ReadC sflPrmDis;
           Enddo;
        Endif;
        //*** End Chg ***** 05/23/23 *********************************
       Endsr;
       //****************************************************************************
       // CopySmallDeduct - Copy Small Deductible Records                           *
       //****************************************************************************
       Begsr CopySmallDeduct;
        If scDeduct = 'Y';
           ReadC sflSmDed;
           Dow Not %Eof;
              If scSelSmDed = 'Y';
                 ReadC sflToGrp;
                 Dow Not %Eof;
                    If scSelToGrp = 'Y';
                       nxChgToGrp = *On;
                       Update sflToGrp;
                       Select;
                       //Skip/Replace when duplicates found in To Group
                       When isDupSmDs = *On;
                          Select;
                          When scDupSmDs = 'R';
                             //Check if Small Deductible record exist in To Group
                             effDate7 = DateTo7(scEfdtSD);
                             Chain (intCo#:scToGrp:scStSD:effDate7:
                              scCodeSD) pdd01;
                             If %Found(wtddl1);
                                //Delete the existing Small Deductible record
                                //and write new Small Deductible record in To Group
                                Delete pdd01;
                                Exsr WriteWTddp;
                             Else;
                                Exsr WriteWTddp;
                             Endif;
                          //Skip duplicates When duplicate found in To Group
                          When scDupSmDs = 'S';
                             //Check if form exist in To Group
                             effDate7 = DateTo7(scEfdtSD);
                             Chain(n) (intCo#:scToGrp:scStSD:effDate7:
                              scCodeSD) pdd01;
                             If not %Found(wtddl1);
                                //Write to To Group if Small Deductible record does
                                // not exist in To Group
                                Exsr WriteWTddp;
                             Endif;
                          Endsl;
                       //No dupllicates found in To Group, Copy all Small Deductible records
                       When isDupSmDs = *Off;
                          Exsr WriteWTddp;
                       Endsl;
                    Endif;
                    ReadC sflToGrp;
                 Enddo;
              Endif;
              ReadC sflSmDed;
           Enddo;
        Endif;
       Endsr;
       //****************************************************************************
       // CopyStateRate - Copy Statistical/Reporting Records                        *
       //****************************************************************************
       Begsr CopyStatRate;
        If scStatRat = 'Y';
           ReadC sflStRt;
           Dow Not %Eof;
              If scSelStRt = 'Y';
                 ReadC sflToGrp;
                 Dow Not %Eof;
                    If scSelToGrp = 'Y';
                       nxChgToGrp = *On;
                       Update sflToGrp;
                       Select;
                       //Skip/Replace when duplicates found in To Group
                       When isDupStRt = *On;
                          Select;
                          When scDupStRt = 'R';
                             //Check if Statistical Report/Rating record exist in To Group
                             effDate7 = DateTo7(scEfdtSR);
                             Chain (intCo#:scToGrp:scStSR:effDate7) prr01;
                             If %Found(wdrrl1);
                                //Delete the existing Statistical Report/Rating record
                                //and write new Stat Report/Rating in To Group
                                Delete prr01;
                                Exsr WriteWDrrp;
                             Else;
                                Exsr WriteWDrrp;
                             Endif;
                          //Skip duplicates When duplicate found in To Group
                          When scDupStRt = 'S';
                             //Check if Statistical Report/Rating exist in To Group
                             effDate7 = DateTo7(scEfdtSR);
                             Chain(n) (intCo#:scToGrp:scStSR:effDate7) prr01;
                             If not %Found(wdrrl1);
                                //Write to To Group if Statistical Report/Rating record does
                                // not exist in To Group
                                Exsr WriteWDrrp;
                             Endif;
                          Endsl;
                       //No dupllicates found in To Group, Copy all Statistical Report/Rating
                       When isDupStRt = *Off;
                          Exsr WriteWDrrp;
                       Endsl;
                    Endif;
                    ReadC sflToGrp;
                 Enddo;
              Endif;
              ReadC sflStRt;
           Enddo;
        Endif;
       Endsr;
       //***  Beg Add  **** 09/04/23 ************************************************
       //****************************************************************************
       // CopyStateRate - Copy Statistical/Reporting Records                        *
       //****************************************************************************
       Begsr CopyShrtRate;
        If scShrtRat = 'Y';
           ReadC sflShRt;
           Dow Not %Eof;
              If scSelShRt = 'Y';
                 ReadC sflToGrp;
                 Dow Not %Eof;
                    If scSelToGrp = 'Y';
                       nxChgToGrp = *On;
                       Update sflToGrp;
                       Select;
                       //Skip/Replace when duplicates found in To Group
                       When isDupShRt = *On;
                          Select;
                          When scDupShRt = 'R';
                             //Check if Short Rate record exist in To Group
                             effDate7 = DateTo7(scEfdtSRt);
                             Chain (intCo#:scToGrp:scStSRt:effDate7) pst00;
                             If %Found(wmstl);
                                //Delete the existing Short Rate record
                                //and write new Short Rate in To Group
                                Delete pst00;
                                Exsr WriteWmstp;
                             Else;
                                Exsr WriteWmstp;
                             Endif;
                          //Skip duplicates When duplicate found in To Group
                          When scDupShRt = 'S';
                             //Check if Short Rate record exist in To Group
                             effDate7 = DateTo7(scEfdtSRt);
                             Chain(n) (intCo#:scToGrp:scStSRt:effDate7) pst00;
                             If not %Found(wmstl);
                                //Write to To Group if Short Rate record does
                                // not exist in To Group
                                Exsr WriteWmstp;
                             Endif;
                          Endsl;
                       //No dupllicates found in To Group, Copy all Short Rate.
                       When isDupShRt = *Off;
                          Exsr WriteWmstp;
                       Endsl;
                    Endif;
                    ReadC sflToGrp;
                 Enddo;
              Endif;
              ReadC sflShRt;
           Enddo;
        Endif;
       Endsr;
       //*** End Add **** 09/04/23 ************************************************
       //****************************************************************************
       // LoadCfgGrids - Load Configuration Subfiles
       //****************************************************************************
       Begsr LoadCfgGrids;
        //Statistical Report/Rating Grid
        strSvState = *Blanks;
        rrnStRt = 0;
        Setll (FNco#:FNfnd) wdrrl1;
        Dou %Eof(wdrrl1);
           Reade (FNco#:FNfnd) wdrrl1;
           If %Eof(wdrrl1);
              Leave;
           EndIf;
           If strSvState = RRst and CvtToDate(RRefdt) <= currentDate;
              Iter;
           Endif;
           scSelStRt = 'N';
           If CvtToDate(RRefdt) <= currentDate;
             strSvState = RRst;
           Endif;
           rrnStRt += 1;
           scStRtSel = *Off;
           scStSR = RRst;
           scEfdtSR = CvtToDate(RRefdt);
           strState = RRst;
           Chain strState ststl3;
           If %Found(ststl3);
              scDescSR = %trim(STname);             // State Description
           Endif;
           scStRtRRN = dbrrn7;
           If rrnStRt = 1;
             scStRtSel = *On;
           Endif;
           nxChgStRt = *Off;
           Write sflStRt;
        Enddo;
        rrnStRt = 1;

        //Premium Discount Grid
        Clear arrEffDate;
        Clear arrPrmDisc;
        rrnPrmDis = 0;
        index = 0;
        Setll (FNco#:FNfnd) wmdsl2;
        Reade (FNco#:FNfnd) wmdsl2;
        Dow not %Eof(wmdsl2);
           flgWriteRec = 'N';
           If CvtToDate(DSefdt) > currentDate;
              flgWriteRec = 'Y';
           Else;
              prmDsState = DSst;
              prmDsCode  = DScode;
              index1 = %Lookup(dsPrmDisc:arrPrmDisc);
              If index1 = 0;    //Not found
                 index += 1;
                 arrPrmDisc(index) = dsPrmDisc;
                 arrEffDate(index) = DSefdt;
                 flgWriteRec = 'Y';
              Endif;
              If index1 <> 0;    //Found
                 If arrEffDate(index1) = Dsefdt;
                    flgWriteRec = 'Y';
                 Endif;
              Endif;
           Endif;
           If flgWriteRec = 'Y';
              rrnPrmDis += 1;
              scPrDiSel = *Off;
              scSelPrmDs = 'N';
              scStPD = DSst;
              scEfDtPD = CvtToDate(DSefdt);
              scCodePD = DScode;
              If DStype = 'A';
                 scType = 'Sliding Scale';
              ElseIf DStype = 'B';
                 scType = 'Specific Lookup';
              Else;
                 scType = DStype;
              Endif;
              scTypeCode = DStype;
              scLmt = DSlmt;
              scPct = DSpct;
              scPrDiRRN = dbrrn8;
              If rrnPrmDis = 1;
                 scPrDiSel = *On;
              Endif;
              //*** Beg Add ***** 05/23/23 *********************
              nxChgPrmDs = *On;
              //*** End Add ***** 05/23/23 *********************
              Write sflPrmDis;
           Endif;
           Reade (FNco#:FNfnd) wmdsl2;
        Enddo;
        rrnPrmDis = 1;

        //Increased Limits Grid
        Clear arrEffDate;
        Clear arrIncrLmt;
        rrnIncLmt = 0;
        index = 0;
        Setll (FNco#:FNfnd) wtlml2;
        Reade (FNco#:FNfnd) wtlml2;
        Dow not %Eof(wtlml2);
           flgWriteRec = 'N';
           If CvtToDate(LMefdt) > currentDate;
              flgWriteRec = 'Y';
           Else;
              inLmState = LMst;
              inLmCode  = LMkey;
              index1 = %lookup(dsIncrLmt:arrIncrLmt);
              If index1 = 0;    //Not found
                 index += 1;
                 arrIncrLmt(index) = dsIncrLmt;
                 arrEffDate(index) = LMefdt;
                 flgWriteRec = 'Y';
              Endif;
              If index1 <> 0;    //Found
                 If arrEffDate(index1) = LMefdt;
                    flgWriteRec = 'Y';
                 Endif;
              Endif;
           Endif;
           If flgWriteRec = 'Y';
              rrnIncLmt += 1;
              scInLtSel = *Off;
              scSelIncLm = 'N';
              scEfDtIL = CvtToDate(LMefdt);
              scStIL   = LMst;
              scKey  = LMkey;
              scDescIL = LMdesc;
              scInLmtRRN = dbrrn9;
              If rrnIncLmt = 1;
                 scInLtSel = *On;
              Endif;
              nxChgInLm = *Off;
              Write sflIncLmt;
           Endif;
           Reade (FNco#:FNfnd) wtlml2;
        Enddo;
        rrnIncLmt = 1;

        //Small Deductible Grid
        Clear arrEffDate;
        Clear arrSmDed;
        rrnSmDe = 0;
        index = 0;
        Setll (FNco#:FNfnd) wtddl1;
        Reade (FNco#:FNfnd) wtddl1;
        Dow not %Eof(wtddl1);
           flgWriteRec = 'N';
           If CvtToDate(DDefdt) > currentDate;
              flgWriteRec = 'Y';
           Else;
              smDeState = DDst;
              smDeCode  = DDcode;
              index1 = %Lookup(dsSmDed:arrSmDed);
              If index1 = 0;    //Not found
                 index += 1;
                 arrSmDed(index) = dsSmDed;
                 arrEffDate(index) = DDefdt;
                 flgWriteRec = 'Y';
              Endif;
              If index1 <> 0;    //Found
                 If arrEffDate(index1) = DDefdt;
                    flgWriteRec = 'Y';
                 Endif;
              Endif;
           Endif;
           If flgWriteRec = 'Y';
              rrnSmDe += 1;
              scSmDeSel = *Off;
              scSelSmDed = 'N';
              scStSD = DDst;
              scEfDtSD = CvtToDate(DDefdt);
              scCodeSD = DDcode;
              scDescSD = DDdesc;
              scSmDeRRN = dbrrn10;
              If rrnSmDe = 1;
                 scSmDeSel = *On;
              Endif;
              nxChgSmDe = *Off;
              Write sflSmDed;
           Endif;
           Reade (FNco#:FNfnd) wtddl1;
        Enddo;
        rrnSmDe = 1;
       //***  Beg Add  **** 09/04/23 *******************************
        //Short Rate Grid
        strSvState = *Blanks;
        rrnShRt = 0;
        Setll (FNco#:FNfnd) wmstl;
        Dou %Eof(wmstl);
           Reade (FNco#:FNfnd) wmstl;
           If %Eof(wmstl);
              Leave;
           EndIf;
           If strSvState = STst and CvtToDate(STefdt) <= currentDate;
              Iter;
           Endif;
           scSelShRt = 'N';
           If CvtToDate(STefdt) <= currentDate;
             strSvState = STst;
           Endif;
           rrnShRt += 1;
           scShRtSel = *Off;
           scStSRt = STst;
           scEfdtSRt = CvtToDate(STefdt);
           strState = STst;
           Chain strState ststl3;
           If %Found(ststl3);
              scDescSRt = %trim(STname);             // State Description
           Endif;
           scShRtRRN = dbrrn11;
           If rrnShRt = 1;
             scShRtSel = *On;
           Endif;
           nxChgShRt = *Off;
           Write sflShRt;
        Enddo;
        rrnShRt = 1;

       //***  End Add  **** 09/04/23 *******************************
       Endsr;
       //****************************************************************************
       // LoadDupCfgGrids - Load duplicate Configuration Subfiles
       //****************************************************************************
       Begsr LoadDupCfgGrids;
        //Statistical Report/Rating Grid
        //*** Beg Chg ***** 05/30/23 *********************************
        //**** If scStatRat = 'Y';
        //****   ReadC sflStRt;
        //****   Dow Not %Eof;
        //****      If scSelStRt = 'Y';
        //****         nxchgStRt = *On;
        //****         Update sflStRt;
        //****         ReadC sflToGrp;
        //****         Dow Not %Eof;
        //****            If scSelToGrp = 'Y';
        //****               scToGroup# = scToGrp;
        //****               nxChgToGrp = *On;
        //****               update sflToGrp;
        //****               effDate7 = DateTo7(scEfdtSR);
        //****               Chain(n) (intCo#:scToGroup#:scStSR:effDate7) prr01;
        //****               If %Found(wdrrl1);
        //****                  scDGrpStRt = RRfnd;
        //****                  scDStSR = RRst;
        //****                  scDEfdtSR = CvtToDate(RRefdt);
        //****                  strState = RRst;
        //****                  Chain strState ststl3;
        //****                  If %Found(ststl3);
        //****                     scDDescSR = %trim(STname);
        //****                  Endif;
        //****                  scDStRtRRN = dbrrn7;
        //****                  rrnDStRt += 1;
        //****                  Write sflDStRt;
        //****               Endif;
        //****            Endif;
        //****            ReadC sflToGrp;
        //****         Enddo;
        //****      Endif;
        //****      ReadC sflStRt;
        //****   Enddo;
        //**** Endif;
        If scStatRat = 'Y';
          ReadC sflToGrp;
          Dow Not %Eof;
             If scSelToGrp = 'Y';
                scToGroup# = scToGrp;
                nxChgToGrp = *On;
                update sflToGrp;
                ReadC sflStRt;
                Dow Not %Eof;
                   If scSelStRt = 'Y';
                      effDate7 = DateTo7(scEfdtSR);
                      nxchgStRt = *On;
                      Update sflStRt;
                      Chain(n) (intCo#:scToGroup#:scStSR:effDate7) prr01;
                      If %Found(wdrrl1);
                         scDGrpStRt = RRfnd;
                         scDStSR = RRst;
                         scDEfdtSR = CvtToDate(RRefdt);
                         strState = RRst;
                         Chain strState ststl3;
                         If %Found(ststl3);
                            scDDescSR = %trim(STname);
                         Endif;
                         scDStRtRRN = dbrrn7;
                         rrnDStRt += 1;
                         Write sflDStRt;
                      Endif;
                   Endif;
                   ReadC sflStRt;
                Enddo;
             Endif;
             ReadC sflToGrp;
          Enddo;
        Endif;
        //*** End Chg ***** 05/30/23 *********************************
        //*** Beg Chg ***** 05/23/23 *********************************
        //Premium Discount Grid
       //*If scPrmDisc = 'Y';
       //***ReadC sflPrmDis;
       //***Dow Not %Eof;
       //***   If scSelPrmDs = 'Y';
       //***      nxChgPrmDs = *On;
       //***      Update sflPrmDis;
       //***      ReadC sflToGrp;
       //***      Dow Not %Eof;
       //***         If scSelToGrp = 'Y';
       //***            scToGroup# = scToGrp;
       //***            nxChgToGrp = *On;
       //***            Update sflToGrp;
       //***            effDate7 = DateTo7(scEfdtPD);
       //***            Chain(n) (intCo#:scToGroup#:scStPD:effDate7:
       //***             scCodePD:scTypeCode:scLmt) pds02;
       //***            If %Found(wmdsl2);
       //***               scDGrpPrDs = DSfnd;
       //***               scDSt = DSst;
       //***               scDEfdt = CvtToDate(DSefdt);
       //***               scDCode = DScode;
       //***               If DStype = 'A';
       //***                  scDType = 'Sliding Scale';
       //***               ElseIf DStype = 'B';
       //***                  scDType = 'Specific Lookup';
       //***               Else;
       //***                  scDType = DStype;
       //***               Endif;
       //***               scDLmt = DSlmt;
       //***               scDPct = DSpct;
       //***               scDPrDiRRN = dbrrn8;
       //***               rrnDPrmDis += 1;
       //***               Write sflDPrmDis;
       //***            Endif;
       //***         Endif;
       //***         ReadC sflToGrp;
       //***      Enddo;
       //***   Endif;
       //***   ReadC sflPrmDis;
       //***Enddo;
       //*Endif;

        //*** Beg Chg ***** 05/30/23 *********************************
        //*** If scPrmDisc = 'Y';
        //***   Clear svStPD;
        //***   Clear svEfdtPD;
        //***   Clear svCodePD;
        //***   ReadC sflPrmDis;
        //***   Dow Not %Eof;
        //***      nxChgPrmDs = *On;
        //***      Update sflPrmDis;
        //***      If scStPD <> svStPD or scEfdtPD <> svEfdtPD or
        //***       scCodePD <> svCodePD;
        //***         If scSelPrmDs = 'Y';
        //***            svStPD = scStPD;
        //***            svEfdtPD = scEfdtPD;
        //***            svCodePD = scCodePD;
        //***            ReadC sflToGrp;
        //***            Dow Not %Eof;
        //***               If scSelToGrp = 'Y';
        //***                  scToGroup# = scToGrp;
        //***                  nxChgToGrp = *On;
        //***                  Update sflToGrp;
        //***                  effDate7 = DateTo7(scEfdtPD);
        //***                  Setll (intCo#:scToGroup#:scStPD:effDate7:
        //***                   scCodePD) pds02;
        //***                  Reade(n) (intCo#:scToGroup#:scStPD:effDate7:
        //***                   scCodePD) pds02;
        //***                  Dow Not %Eof(wmdsl2);
        //***                     scDGrpPrDs = DSfnd;
        //***                     scDSt = DSst;
        //***                     scDEfdt = CvtToDate(DSefdt);
        //***                     scDCode = DScode;
        //***                     If DStype = 'A';
        //***                        scDType = 'Sliding Scale';
        //***                     ElseIf DStype = 'B';
        //***                        scDType = 'Specific Lookup';
        //***                     Else;
        //***                        scDType = DStype;
        //***                     Endif;
        //***                     scDLmt = DSlmt;
        //***                     scDPct = DSpct;
        //***                     scDPrDiRRN = dbrrn8;
        //***                     rrnDPrmDis += 1;
        //***                     Write sflDPrmDis;
        //***                     Reade(n) (intCo#:scToGroup#:scStPD:effDate7:
        //***                      scCodePD) pds02;
        //***                  Enddo;
        //***               Endif;
        //***               ReadC sflToGrp;
        //***            Enddo;
        //***         Endif;
        //***      Endif;
        //***      ReadC sflPrmDis;
        //***   Enddo;
        //*** Endif;
        If scPrmDisc = 'Y';
          ReadC sflToGrp;
          Dow Not %Eof;
             If scSelToGrp = 'Y';
                scToGroup# = scToGrp;
                nxChgToGrp = *On;
                Update sflToGrp;
                Clear svStPD;
                Clear svEfdtPD;
                Clear svCodePD;
                ReadC sflPrmDis;
                Dow Not %Eof;
                   nxChgPrmDs = *On;
                   Update sflPrmDis;
                   If scStPD <> svStPD or scEfdtPD <> svEfdtPD or
                    scCodePD <> svCodePD;
                      If scSelPrmDs = 'Y';
                         svStPD = scStPD;
                         svEfdtPD = scEfdtPD;
                         svCodePD = scCodePD;
                         effDate7 = DateTo7(scEfdtPD);
                         Setll (intCo#:scToGroup#:scStPD:effDate7:
                          scCodePD) pds02;
                         Reade(n) (intCo#:scToGroup#:scStPD:effDate7:
                          scCodePD) pds02;
                         Dow Not %Eof(wmdsl2);
                            scDGrpPrDs = DSfnd;
                            scDSt = DSst;
                            scDEfdt = CvtToDate(DSefdt);
                            scDCode = DScode;
                            If DStype = 'A';
                               scDType = 'Sliding Scale';
                            ElseIf DStype = 'B';
                               scDType = 'Specific Lookup';
                            Else;
                               scDType = DStype;
                            Endif;
                            scDLmt = DSlmt;
                            scDPct = DSpct;
                            scDPrDiRRN = dbrrn8;
                            rrnDPrmDis += 1;
                            Write sflDPrmDis;
                            Reade(n) (intCo#:scToGroup#:scStPD:effDate7:
                             scCodePD) pds02;
                         Enddo;
                      Endif;
                   Endif;
                   ReadC sflPrmDis;
                Enddo;
             Endif;
             ReadC sflToGrp;
          Enddo;
        Endif;
        //*** End Chg ***** 05/30/23 *********************************
        //*** End Chg ***** 05/23/23 *********************************

        //Increased Limits Grid
        //*** Beg Chg ***** 05/30/23 *********************************
        //*** If scIncrLmt = 'Y';
        //***   ReadC sflIncLmt;
        //***   Dow Not %Eof;
        //***      If scSelIncLm = 'Y';
        //***         nxchgInLm = *On;
        //***         Update sflIncLmt;
        //***         ReadC sflToGrp;
        //***         Dow Not %Eof;
        //***            If scSelToGrp = 'Y';
        //***               scToGroup# = scToGrp;
        //***               nxChgToGrp = *On;
        //***               Update sflToGrp;
        //***               effDate7 = DateTo7(scEfdtIL);
        //***               Chain(n) (intCo#:scToGroup#:scStIL:effDate7:
        //***               scKey) plm02;
        //***               If %Found(wtlml2);
        //***                  scDGrpInLm = LMfnd;
        //***                  scDStIL = LMst;
        //***                  scDEfdtIL = CvtToDate(LMefdt);
        //***                  scDKey = LMkey;
        //***                  scDDescIL = LMdesc;
        //***                  scDInLmRRN = dbrrn9;
        //***                  rrnDIncLmt += 1;
        //***                  Write sflDIncLmt;
        //***               Endif;
        //***            Endif;
        //***            ReadC sflToGrp;
        //***         Enddo;
        //***      Endif;
        //***      ReadC sflIncLmt;
        //***   Enddo;
        //*** Endif;
        If scIncrLmt = 'Y';
          ReadC sflToGrp;
          Dow Not %Eof;
             If scSelToGrp = 'Y';
                scToGroup# = scToGrp;
                nxChgToGrp = *On;
                Update sflToGrp;
                ReadC sflIncLmt;
                Dow Not %Eof;
                   If scSelIncLm = 'Y';
                      nxchgInLm = *On;
                      Update sflIncLmt;
                      effDate7 = DateTo7(scEfdtIL);
                      Chain(n) (intCo#:scToGroup#:scStIL:effDate7:
                      scKey) plm02;
                      If %Found(wtlml2);
                         scDGrpInLm = LMfnd;
                         scDStIL = LMst;
                         scDEfdtIL = CvtToDate(LMefdt);
                         scDKey = LMkey;
                         scDDescIL = LMdesc;
                         scDInLmRRN = dbrrn9;
                         rrnDIncLmt += 1;
                         Write sflDIncLmt;
                      Endif;
                   Endif;
                   ReadC sflIncLmt;
                Enddo;
             Endif;
             ReadC sflToGrp;
          Enddo;
        Endif;
        //*** End Chg ***** 05/30/23 *********************************

        //Small Deductible Grid
        //*** Beg Chg ***** 05/30/23 *********************************
        //*** If scDeduct = 'Y';
        //***   ReadC sflSmDed;
        //***   Dow Not %Eof;
        //***      If scSelSmDed = 'Y';
        //***         nxChgSmDe = *On;
        //***         Update sflSmDed;
        //***         ReadC sflToGrp;
        //***         Dow Not %Eof;
        //***            If scSelToGrp = 'Y';
        //***               scToGroup# = scToGrp;
        //***               nxChgToGrp = *On;
        //***               Update sflToGrp;
        //***               effDate7 = DateTo7(scEfdtSD);
        //***               Chain(n) (intCo#:scToGroup#:scStSD:effDate7:
        //***                scCodeSD) pdd01;
        //***               If %Found(wtddl1);
        //***                  scDGrpSmDe = DDfnd;
        //***                  scDStSD = DDst;
        //***                  scDEfdtSD = CvtToDate(DDefdt);
        //***                  scDCodeSD = DDcode;
        //***                  scDDescSD = DDdesc;
        //***                  scDSmDeRRN = dbrrn10;
        //***                  rrnDSmDe += 1;
        //***                  Write sflDSmDed;
        //***               Endif;
        //***            Endif;
        //***            ReadC sflToGrp;
        //***         Enddo;
        //***      Endif;
        //***      ReadC sflSmDed;
        //***   Enddo;
        //*** Endif;
        If scDeduct = 'Y';
          ReadC sflToGrp;
          Dow Not %Eof;
             If scSelToGrp = 'Y';
                scToGroup# = scToGrp;
                nxChgToGrp = *On;
                Update sflToGrp;
                ReadC sflSmDed;
                Dow Not %Eof;
                   If scSelSmDed = 'Y';
                      nxChgSmDe = *On;
                      Update sflSmDed;
                      effDate7 = DateTo7(scEfdtSD);
                      Chain(n) (intCo#:scToGroup#:scStSD:effDate7:
                       scCodeSD) pdd01;
                      If %Found(wtddl1);
                         scDGrpSmDe = DDfnd;
                         scDStSD = DDst;
                         scDEfdtSD = CvtToDate(DDefdt);
                         scDCodeSD = DDcode;
                         scDDescSD = DDdesc;
                         scDSmDeRRN = dbrrn10;
                         rrnDSmDe += 1;
                         Write sflDSmDed;
                      Endif;
                   Endif;
                   ReadC sflSmDed;
                Enddo;
             Endif;
             ReadC sflToGrp;
          Enddo;
        Endif;
        //*** End Chg ***** 05/30/23 *********************************

        //*** Beg Add ***** 09/04/23 *********************************
        If scShrtRat = 'Y';
          ReadC sflToGrp;
          Dow Not %Eof;
             If scSelToGrp = 'Y';
                scToGroup# = scToGrp;
                nxChgToGrp = *On;
                update sflToGrp;
                ReadC sflShRt;
                Dow Not %Eof;
                   If scSelShRt = 'Y';
                      effDate7 = DateTo7(scEfdtSRt);
                      nxchgShRt = *On;
                      Update sflShRt;
                      Chain(n) (intCo#:scToGroup#:scStSRt:effDate7) pst00;
                      If %Found(wmstl);
                         scDGrpShRt = STfnd;
                         scDStSRt = STst;
                         scDEfdtSRt = CvtToDate(STefdt);
                         strState = STst;
                         Chain strState ststl3;
                         If %Found(ststl3);
                            scDDescSRt = %trim(STname);
                         Endif;
                         scDShRtRRN = dbrrn11;
                         rrnDShRt += 1;
                         Write sflDShRt;
                      Endif;
                   Endif;
                   ReadC sflShRt;
                Enddo;
             Endif;
             ReadC sflToGrp;
          Enddo;
        Endif;
        //*** End Add ***** 09/04/23 *********************************
       Endsr;
       //****************************************************************************
       // LoadSflToGrp - Load To Group Subfile
       //****************************************************************************
       Begsr LoadSflToGrp;
         Chain(n) inRRN pfn0p;
         scFrGroup# = FNfnd;
         Setll *loval Wmfnl;
         Read Wmfnl;
         Dow not %Eof(Wmfnl);
            If FNfnd <> scFrGroup#;
               Chain(n) (FNco#:FNfnd) Wdf2l;
               scSelToGrp = 'N';
               scToGrp = FNfnd;
               scToGrpDes = %Editc(FNfnd:'X') + ' - ' + FNname;
               //*** Beg Add ***** 07/28/23 *****************************
               // Short Name
               scShNm = FNshnm;
               //*** End Add ***** 07/28/23 *****************************
               If %Found(Wdf2l);
                  scCar# = F2Car#;
               Else;
                  scCar# = *Zeros;
               Endif;
               //Peo Type
               Select;
               When FNpeof = *Blank;
                  scPeoType = 'Standard';
               When FNpeof = 'S';
                  scPeoType = 'Master PEO';
               When FNpeof = 'M';
                  scPeoType = 'MCP PEO';
               When FNpeof = 'C';
                  scPeoType = 'Combined PEO';
               Endsl;
               //*** Beg Add ***** 12/27/23 *****************************
               scExpDt = CvtToDate(FNexdt);
               //*** End Add ***** 12/27/23 *****************************
               RrnToGrp = RrnToGrp + 1;
               nxChgToGrp = *Off;
               Write SflToGrp;
            Endif;
            Read Wmfnl;
         Enddo;

         If RrnToGrp = *Zeros;
            DspToGrpSf = *Off;
            VisToGrp = *Off;
         Else;
            RrnToGrp = 1;
            VisToGrp = *On;
            TopToGrp = 185;
         Endif;
       Endsr;
       //*** Beg Add ***** 05/23/23 *********************************
       //****************************************************************************
       // SelectPremDiscount - Select Premium Discount Records                      *
       //****************************************************************************
       Begsr SelectPremDiscount;
        If scPrmDisc = 'Y';
           Chain scSelPDRow sflPrmDis;
           nxChgPrmDs = *On;
           Update sflPrmDis;
           If %Found();
              svStPD = scStPD;
              svEfdtPD = scEfdtPD;
              svCodePD = scCodePD;
              svSelPrmDs = scSelPrmDs;
              Chain 1 sflPrmDis;
              nxChgPrmDs = *On;
              Update sflPrmDis;
              Readc sflPrmDis;
              Dow Not %Eof;
                 If scStPD = svStPD and scEfdtPD = svEfdtPD
                  and scCodePD = svCodePD;
                    scSelPrmDs = svSelPrmDs;
                 Endif;
                 nxChgPrmDs = *On;
                 Update sflPrmDis;
                 Readc sflPrmDis;
              Enddo;
           Endif;
        Endif;
       Endsr;
       //*** End Add ***** 05/23/23 *********************************
       //****************************************************************************
       // WriteWDrrp - Write WDRRP record: Write WMLDP, WIRATDFT, WIRATDDT
       //****************************************************************************
       Begsr WriteWDrrp;
         //Write WDRRP
         Chain(n) scStRtRRN prr0p;
         If %Found(wdrrp);
            RRfnd = scToGrp;
            RRpgm  = Q1pgm;
            RRuser = Q1user;
            RRtime = Timeto6(SystemTime());
            RRdt   = Dateto7(SystemDate());
            //*** Beg Add ***** 05/23/23 *********************************
            RRiusr = Q1user;
            RRipgm = Q1pgm;
            RRitim = Timeto6(SystemTime());
            RRidt  = Dateto7(SystemDate());
            //*** End Add ***** 05/23/23 *********************************
            Write prr0p;

            //Write WMLDP
            Chain (intCo#:scToGrp:scStSR:0:effDate7) pld04;
            If %Found(Wmldl4);
               Delete pld04;
            Endif;
            Chain(n) (intCo#:scFrGroup#:scStSR:0:effDate7) pld04;
            If %Found(Wmldl4);
               LDfnd = scToGrp;
               LDuser = Q1user;
               LDpgm  = Q1pgm;
               LDdt   = Dateto7(SystemDate());
               LDtime = Timeto6(SystemTime());
               //*** Beg Add ***** 05/23/23 *********************************
               LDiusr = Q1user;
               LDipgm = Q1pgm;
               LDitim = Timeto6(SystemTime());
               LDidt  = Dateto7(SystemDate());
               //*** End Add ***** 05/23/23 *********************************
               Write pld0p;
            EndIF;

            //Write WIRATDFT
            Setll (intCo#:scToGrp:scStSR:effDate7) pdf00;
            Reade (intCo#:scToGrp:scStSR:effDate7) pdf00;
            Dow not %Eof(Wiratdftl);
               Delete pdf00;
               Reade (intCo#:scToGrp:scStSR:effDate7) pdf00;
            Enddo;
            Setll (intCo#:scFrGroup#:scStSR:effDate7) pdf00;
            Reade(n) (intCo#:scFrGroup#:scStSR:effDate7) pdf00;
            Dow not %Eof(Wiratdftl);
               DFfnd = scToGrp;
               DFuser = Q1user;
               DFpgm  = Q1pgm;
               DFdt   = Dateto7(SystemDate());
               DFtime = Timeto6(SystemTime());
               //*** Beg Add ***** 05/23/23 *********************************
               DFiusr = Q1user;
               DFipgm = Q1pgm;
               DFitim = Timeto6(SystemTime());
               DFidt  = Dateto7(SystemDate());
               //*** End Add ***** 05/23/23 *********************************
               Write pdf0p;
               Reade(n) (intCo#:scFrGroup#:scStSR:effDate7) pdf00;
            Enddo;

            //Write WIRATDDT
            Setll (intCo#:scToGrp:scStSR:effDate7) pdt00;
            Reade (intCo#:scToGrp:scStSR:effDate7) pdt00;
            Dow not %Eof(Wiratddtl);
               Delete pdt00;
               Reade (intCo#:scToGrp:scStSR:effDate7) pdt00;
            Enddo;
            Setll (intCo#:scFrGroup#:scStSR:effDate7) pdt00;
            Reade(n) (intCo#:scFrGroup#:scStSR:effDate7) pdt00;
            Dow not %Eof(Wiratddtl);
               DTfnd = scToGrp;
               DTuser = Q1user;
               DTpgm  = Q1pgm;
               DTdt   = Dateto7(SystemDate());
               DTtime = Timeto6(SystemTime());
               //*** Beg Add ***** 05/23/23 *********************************
               DTiusr = Q1user;
               DTipgm = Q1pgm;
               DTitim = Timeto6(SystemTime());
               DTidt  = Dateto7(SystemDate());
               //*** End Add ***** 05/23/23 *********************************
               Write pdt0p;
               Reade(n) (intCo#:scFrGroup#:scStSR:effDate7) pdt00;
            Enddo;
         Endif;
       Endsr;
       //*** Beg Add **** 09/04/23 ************************************************
       //****************************************************************************
       // WriteWmstp - Write Wmstp file.
       //****************************************************************************
       Begsr WriteWmstp;
         //Write Wmstp
         Chain(n) scShRtRRN pst0p;
         If %Found(wmstp);
            STfnd = scToGrp;
            STpgm  = Q1pgm;
            STuser = Q1user;
            STtime = Timeto6(SystemTime());
            STdt   = Dateto7(SystemDate());
            STiusr = Q1user;
            STipgm = Q1pgm;
            STitim = Timeto6(SystemTime());
            STidt  = Dateto7(SystemDate());
            Write pst0p;

            //Write Wdstp
            Setll (intCo#:scToGrp:scStSRt:effDate7) pst03;
            Reade (intCo#:scToGrp:scStSRt:effDate7) pst03;
            Dow not %Eof(Wdstl);
               Delete pst03;
               Reade (intCo#:scToGrp:scStSRt:effDate7) pst03;
            Enddo;
            Setll (intCo#:scFrGroup#:scStSRt:effDate7) pst03;
            Reade(n) (intCo#:scFrGroup#:scStSRt:effDate7) pst03;
            Dow not %Eof(Wdstl);
               STfnd = scToGrp;
               STuser = Q1user;
               STpgm  = Q1pgm;
               STdt   = Dateto7(SystemDate());
               STtime = Timeto6(SystemTime());
               STiusr = Q1user;
               STipgm = Q1pgm;
               STitim = Timeto6(SystemTime());
               STidt  = Dateto7(SystemDate());
               Write pst02;
               Reade(n) (intCo#:scFrGroup#:scStSRt:effDate7) pst03;
            Enddo;
         Endif;
       Endsr;
       //*** End Add **** 09/04/23 ************************************************
       //****************************************************************************
       // WriteWMdsp - Write WMDSP record
       //****************************************************************************
       Begsr WriteWMdsp;
       //*** Beg Chg ***** 05/23/23 *********************************
       //**Chain(n) scPrDiRRN pds0p;
       //**If %Found(wmdsp);
       //***  DSfnd = scToGrp;
       //***  DSuser = Q1user;
       //***  DSpgm  = Q1pgm;
       //***  DStime = Timeto6(SystemTime());
       //***  DSdt   = Dateto7(SystemDate());
       //***  Write pds0p;
       //***  Endif;
         Setll (intCo#:scFrGroup#:scStPD:effDate7:
          scCodePD) pds02;
         Reade(n) (intCo#:scFrGroup#:scStPD:effDate7:
          scCodePD) pds02;
         Dow Not %Eof(wmdsl2);
            DSfnd = scToGrp;
            DSuser = Q1user;
            DSpgm  = Q1pgm;
            DStime = Timeto6(SystemTime());
            DSdt   = Dateto7(SystemDate());
            DSIUSR = Q1user;
            DSIPGM = Q1pgm;
            DSITIM = Timeto6(SystemTime());
            DSIDT  = Dateto7(SystemDate());
            Write pds0p;
            Reade(n) (intCo#:scFrGroup#:scStPD:effDate7:
             scCodePD) pds02;
        Enddo;
        //*** End Chg ***** 05/23/23 *********************************
       Endsr;
       //****************************************************************************
       // WriteWTddp - Write WTDDP record
       //****************************************************************************
       Begsr WriteWTddp;
         Chain(n) scSmDeRRN pdd0p;
         If %Found(wtddp);
            DDfnd = scToGrp;
            DDuser = Q1user;
            DDpgm  = Q1pgm;
            DDtime = Timeto6(SystemTime());
            DDdt   = Dateto7(SystemDate());
            //*** Beg Add ***** 05/23/23 *********************************
            DDiusr = Q1user;
            DDipgm = Q1pgm;
            DDitim = Timeto6(SystemTime());
            DDidt  = Dateto7(SystemDate());
            //*** End Add ***** 05/23/23 *********************************
            Write pdd0p;
         Endif;

       Endsr;
       //****************************************************************************
       // WriteWTlmp - Write WTLMP record
       //****************************************************************************
       Begsr WriteWTlmp;
         Chain(n) scInLmtRRN plm0p;
         If %Found(wtlmp);
            LMfnd = scToGrp;
            LMuser = Q1user;
            LMpgm  = Q1pgm;
            LMtime = Timeto6(SystemTime());
            LMdt   = Dateto7(SystemDate());
            //*** Beg Add ***** 05/23/23 *********************************
            LMiusr = Q1user;
            LMipgm = Q1pgm;
            LMitim = Timeto6(SystemTime());
            LMidt  = Dateto7(SystemDate());
            //*** End Add ***** 05/23/23 *********************************
            Write plm0p;
         Endif;
       Endsr;
       //*** Begin Add *** 09/26/22 *******************************************
       //****************************************************************************
       // CopyStateCompBen - Copy State Comensation Benefits to selected groups
       //****************************************************************************
       Begsr CopyStateCompBen;
        ptrSCBList = inPtrEndFrm;
        scCpCfgHdg = 'Copy State Compensation Benefits' ;
        scFrGroup# = FNfnd;
        VisDupScb = *Off;
        VisNDupEnd = *Off;
        scCpyPnlHt = 315;
        BtnReplScb = *Off;
        BtnSkipSCB = *Off;
        BtnCopy  = *Off;

        Dou ExitNow1 = *On;
           Exfmt rcdCopyCfg;

           VisNDupEnd = *On;
           leftBtnCpy = 535;
           VisDupScb = *Off;
           TopNDupEnd = 405;
           scCpyPnlHt = 347;

           Select;
           When btnVerify = *On;
              Exsr ClrSflDupSCB;
              index = 1;
              Dow index <= 9999 and arrSCB(index).SCBState <> ' ';
                  ReadC sflToGrp;
                  Dow Not %Eof;
                     If scSelToGrp = 'Y';
                        scToGroup# = scToGrp;
                        nxChgToGrp = *On;
                        update sflToGrp;
                        chain(n) (intCo#:scToGroup#: arrSCB(index).SCBState
                             :arrSCB(index).SCBEfdt) prb00;
                        If %Found(wdrbl1);
                           scDStSCB   = RBst   ;
                           scDEfdtSCB= CvtToDate(RBefdt);
                           scDCmpr    = RBcmpr ;
                           scDPacp    = RBpacp ;
                           scDSCBfnd  = RBfnd  ;
                           scDSCBRrn# = dbrrn13;
                           rrnDSCB = rrnDSCB + 1;
                           Write sflDupSCB;
                        Endif;
                     Endif;
                     ReadC sflToGrp;
                  Enddo;
                  index += 1;
              Enddo;
              If rrnDSCB > 0;
                 VisDupScb = *On;
                 VisNDupEnd = *Off;
                 TopDupSCB = 401;
                 scCpyPnlHt = 521;
                 dspDSCB = *On;
                 dspDSCBCtl = *On;
              Endif;
              If rrnDSCB = *Zero and BtnCancel = *Off; //and scError1 = *blank;
                 VisNDupEnd = *On;
                 leftBtnCpy = 535;
                 VisDupScb = *Off;
                 TopNDupEnd = 405;
                 scCpyPnlHt = 347;
              Endif;
              rrnDSCB = 1;
              If rrnPsToGrp <> 0;
                 rrnToGrp = rrnPsToGrp;
              Else;
                 rrnToGrp = 1;
              Endif;
              btnVerify = *Off;
           When BtnSkipSCB = *On;
              CopySCBRecord(scFrGroup#:'S');
              ExitNow1 = *On;
           //Duplicates found in To Group: Replace Duplicate Endorsements in To Group
           When BtnReplScb = *On;
              CopySCBRecord(scFrGroup#:'R');
              ExitNow1 = *On;
           //No Duplicates found in To Group: Copy all the Endorsements to To Group
           When BtnCopy = *On;
              CopySCBRecord(scFrGroup#:'A');
              ExitNow1 = *On;
           When BtnCancel = *On;
              ExitNow1 = *On;
           Endsl;
        Enddo;

       Endsr;

       //****************************************************************************
       // ClrSflDupScb - Clear Duplicate State Compensation Rate By Benefit
       //****************************************************************************
       Begsr ClrSflDupScb;
          Clear sflDupScb;
          RrnDScb = 0;
          dspDScb = *Off;
          clrDScb = *On;
          Write rcdCopyCfg;
          clrDScb = *Off;
          dspDScb = *On;
       Endsr;


       //****************************************************************************
       // CopyStateCompRtBen - Copy State Compensation Rate By Benefit
       //****************************************************************************
       Begsr CopyStateCompRtBen;
        ptrSCBRList = inPtrEndFrm;
        scCpCfgHdg = 'Copy State Compensation Rate By Benefit Type' ;
        scFrGroup# = FNfnd;
        VisDupScbr = *Off;
        VisNDupEnd = *Off;
        scCpyPnlHt = 315;
        BtnRepScbr = *Off;
        BtnSkpSCBR = *Off;
        BtnCopy  = *Off;

        Dou ExitNow1 = *On;
           Exfmt rcdCopyCfg;

           VisNDupEnd = *On;
           leftBtnCpy = 535;
           VisDupScbr = *Off;
           TopNDupEnd = 405;
           scCpyPnlHt = 347;

           Select;
           When btnVerify = *On;
              Exsr ClrSflDupSCBR;
              index = 1;
              Dow index <= 9999 and arrSCBR(index).SCBRState <> ' ';
                  ReadC sflToGrp;
                  Dow Not %Eof;
                     If scSelToGrp = 'Y';
                        scToGroup# = scToGrp;
                        nxChgToGrp = *On;
                        update sflToGrp;
                        chain(n) (intCo#:scToGroup#: arrSCBR(index).SCBRState
                             :arrSCBR(index).SCBRbr2
                             :arrSCBR(index).SCBREfdt) pr100;
                        If %Found(wdr1l);
                           scDStSCBR  = R1st   ;
                           scDdtSCBR  = CvtToDate(R1efdt);
                           scDBenTyp  = R1br2  ;
                           kkkey    = R1br2 ;
                           chain KeyWtabl pb201;
                           if %found(wtabl);
                              scDBenDesc= b2desc ;
                           else  ;
                              scDBenDesc= ' ' ;
                           endif ;
                           scDRtSCBR  = R1rate ;
                           scDSCBRfnd  = R1fnd ;
                           scDSCBRrn# = dbrrn27;
                           rrnDSCBR = rrnDSCBR + 1;
                           Write sflDupSCBR;
                        Endif;
                     Endif;
                     ReadC sflToGrp;
                  Enddo;
                  index += 1;
              Enddo;
              If rrnDSCBR > 0;
                 VisDupScbr = *On;
                 VisNDupEnd = *Off;
                 TopDupSCBR = 401;
                 scCpyPnlHt = 521;
                 dspDSCBR = *On;
                 dspDSCBRCl = *On;
              Endif;
              If rrnDSCBR = *Zero and BtnCancel = *Off; //and scError1 = *blank;
                 VisNDupEnd = *On;
                 leftBtnCpy = 535;
                 VisDupScbr = *Off;
                 TopNDupEnd = 405;
                 scCpyPnlHt = 347;
              Endif;
              rrnDSCBR = 1;
              If rrnPsToGrp <> 0;
                 rrnToGrp = rrnPsToGrp;
              Else;
                 rrnToGrp = 1;
              Endif;
              btnVerify = *Off;
           When BtnSkpSCBR = *On;
              CopySCBRRecord(scFrGroup#:'S');
              ExitNow1 = *On;
           //Duplicates found in To Group: Replace Duplicate SCBR in To Group
           When BtnRepScbr = *On;
              CopySCBRRecord(scFrGroup#:'R');
              ExitNow1 = *On;
           //No Duplicates found in To Group: Copy all SCBR to To Group
           When BtnCopy = *On;
              CopySCBRRecord(scFrGroup#:'A');
              ExitNow1 = *On;
           When BtnCancel = *On;
              ExitNow1 = *On;
           Endsl;
        Enddo;

       Endsr;

       //****************************************************************************
       // ClrSflDupSCBR- Clear Duplicate State compensation by rate benefit grid
       //****************************************************************************
       Begsr ClrSflDupScbr;
          Clear sflDupScbr;
          RrnDScbr = 0;
          dspDScbr = *Off;
          clrDScbr = *On;
          Write rcdCopyCfg;
          clrDScbr = *Off;
          dspDScbr = *On;
       Endsr;

       //*** End   Add *** 09/26/22 *******************************************
      /end-free
     pGroupCopySetup   e
      //*** End Add ***** 01/17/22 *****************************************
      *=====================================================================
      * LoadDetail - Load Group Detail screen
      *  Parm1   - RRN (9A) Group master (WMFNP)
      *  Parm2   - Pointer to format DS
      *  Parm3   - Mode(1A) 'C'-Change, 'D'-Display,
      *  Returns - Pointer to format DS
      *=====================================================================
     pLoadDetail       b                   export
     dLoadDetail       pi              n
     d inRRN                          9  0
     d inPtrGrpDtl                     *
     d inMode                         1a   const

     d iOpen           s               n   inz
     d xxKey           s              2

     c     keyWMFCL      klist
     c                   kfld                    FNco#
     c                   kfld                    FNfnd

      /free

       ptrGrpDtl = inPtrGrpDtl;
       If not %Open(wpigrppop);
          Open wpigrppop;
          iOpen = *On;
       Endif;

       clear scMedTrSel;
       clear scCmpTrSel;
       clear scExpTrSel;
       clear scLglTrSel;
       GetClientId(ClientID);

       If ClientID = '114';
          dspPEOGrp = *off;
          scTyPmChc = ',C,V,XML';
          scTyPmChVl= ',C,V,X';
       Else;
          dspPEOGrp = *on;
          scTyPmChc = ',C,V';
          scTyPmChVl= ',C,V';
          If ClientID = '139';
             scPEOChcOp = ',Single,Multiple,GIC Master Policy';
             scPEOChcVl = ',S,M,G';
          Else;
             scPEOChcOp = ',Single,Multiple';
             scPEOChcVl = ',S,M';
          Endif;
       Endif;

       scMedTrSel='PCstat= ''A''' + ' AND PCRECD in (Select TRIM(RTKey) from +
        WTRTP where RTtype = ''M''' + ')';
       scCmpTrSel='PCstat= ''A''' + ' AND PCRECD in (Select TRIM(RTKey) from +
        WTRTP where RTtype = ''C''' + ')';
       scExpTrSel='PCstat= ''A''' + ' AND PCRECD in (Select TRIM(RTKey) from +
        WTRTP where RTtype = ''E''' + ')';
       scLglTrSel='PCstat= ''A''' + ' AND PCRECD in (Select TRIM(RTKey) from +
        WTRTP where RTtype = ''L''' + ')';
       scGrpWdLft = 260;
       scGrpWdTop = 105;

       Chain(n) inRrn pfn0p;
       If not %found;
          scDtlCmGrp = 'N';
          scDtlAtEmp = 'N';
          scDtlAtDiv = 'N';
          scDtlDiv0  = 'N';
          scDtlEmSIn = 'N';
          dspDtlGrp# = *off;
          dspDtlGrpN = *off;
          dspDtlCs#  = *off;
          dspDtlBtn  = *on;
          scShowWdw  = *on;
          dspPanel   = *on;
          clear pfn0p;
       EndIf;

       dspDtlGrp# = *on;
       dspDtlCs#  = *on;
       dspDtlBtn  = *off;
       saveDate =  Dateto7(systemDate());
       saveTime =  Timeto6(SystemTime());
       scDtlGrp#  = FNfnd;
       scDtlGrpNm = %trim(FNname);
       scDtlShNm  = %trim(FNshnm);
       scDtlEffdt = dtMDYY(FNefdt);
       scDtlExpDt = dtMDYY(FNexdt);

       scDtlMlAd1 = FNmad1;
       scDtlMlAd2 = FNmad2;
       scDtlMlCty = FNmcty;
       scDtlMlSt  = FNmst;
       scDtlMlZip = FNmzip;
       scDtlPhAd1 = FNpad1;
       scDtlPhAd2 = FNpad2;
       scDtlPhCty = FNpcty;
       scDtlPhSt  = FNpst;
       scDtlPhZip = FNpzip;
       scDtlCoAd1 = FNaad1;
       scDtlCoAd2 = FNaad2;
       scDtlCoCty = FNacty;
       scDtlCoSt  = FNast;
       scDtlCoZip = FNazip;
       scDtlIntAd = %trim(FNiadr);
       If FNph1a <> *zeros or FNph1 <> *zeros;
          scDtlPhn1# = %Int(%Editc(FNph1a:'X') + %Editc(FNph1:'X'));
       Endif;
       If FNfaxa <> *zeros or FNfax <> *zeros;
          scDtlFax#  = %Int(%Editc(FNfaxa:'X') + %Editc(FNfax:'X'));
       Endif;
       If FNarea <> *zeros or FNph2 <> *zeros;
          scDtlPhn2# = %Int(%Editc(FNarea:'X') + %Editc(FNph2:'X'));
       Endif;
       scDtlMxPl# = FNmxex;
       scDtlCase# = FNnacs;
       scDtlEmp#  = FNnaen;
       scDtlQte#  = FNnaqn;
       scDtlAtEmp = FNaute;
       If FNaute = ' ';
          scDtlAtEmp = 'N';
       Endif;
       scDtlAtDiv = FNautd;
       If FNautd = ' ';
          scDtlAtDiv = 'N';
       Endif;
       scDtlDiv0  = FNdiv0;
       If FNdiv0 = ' ';
          scDtlDiv0 = 'N';
       Endif;
       scDtlRvCd1 = FNrs01;
       scDtlRvCd2 = FNrs02;
       scDtlRvCd3 = FNrs03;
       scDtlRvCd4 = FNrs04;
       scDtlRvCd5 = FNrs05;
       scDtlRvCd6 = FNrs06;
       scDtlRvCd7 = FNrs07;
       scDtlRvCd8 = FNrs08;
       scDtlRvCd9 = FNrs09;
       scDtlRvC10 = FNrs10;
       scDtlTrMed = FNmetr;
       scDtlTrCmp = FNcotr;
       scDtlTrExp = FNextr;
       scDtlTrLgl = FNletr;
       scDtlPVMed = FNmep1;
       scDtlPVCmp = FNcop1;
       scDtlPVExp = FNexp1;
       scDtlPVLgl = FNlep1;
       scDtlMedRv = FNdfrc;
       scDtlExpRv = FNdfxc;
       scDtlMedAm = FNdfra;
       scDtlExpAm = FNdfxa;
       scDtlChkFr = FNchk1;
       scDtlChkTo = FNchk2;
       scDtlGrCIC = FNcicf;
       scDtlExEDI = FNedi;
       scDtlEmSIn = FNsfin;
       If FNsfin = ' ';
          scDtlEmSIn = 'N';
       Endif;
       scDtlSfOrg = FNsior;
       scDtlSfAut = FNsiau;
       If ClientID <> '114';
          scDtlPEOGp = FNpeof;
       Else;
          scPEOGrpT  = FNpeof;
       Endif;
       scDtlChgEx = FNpeoe;
       scDtlCalSt = FNpeoa;
       scDtlMdRSb = FNpeos;
       scDtlDueDt = FNmidy;
       scDtlQtLst = FNlost;
       scDtlCmpNm = FNanam;
       scDtlShOpP = FNdepo;
       //** Begin Chg *** 10/07/13 ****************************************
       If FNprmv = ' ';
          scPRMV = 'P';
       Else;
          scPRMV = FNprmv;
       Endif;
       //** End   Chg *** 10/07/13 ****************************************
       scDtlPrtDv = FNpde;
       If FNaclr = 'Y' or FNaclr = 'L';
          scDtlAtEnL = 'L';
       ElseIf FNaclr = 'E';
          scDtlAtEnL = 'E';
       Endif;
       scDtlAtAtt = FNaer;
       scDtl3rdPt = FN3ppe;
       scDtlCmGrp = FNcgfl;
       If FNcgfl = ' ';
          scDtlCmGrp = 'N';
       Endif;
       scDtlTstGr = FNtest;
       scDtlEDIFm = FNedif;

       // Group Contact Info
       Chain(n) keyWMFCL pfc00;
       If not %found();
          clear pfc00;
       Endif;
       scDtlCnt1 = FCcon1;
       scDtlCnt2 = FCcon2;
       scDtlCnt3 = FCcon3;
       scDtlCn1Tt= FCttl1;
       scDtlCn2Tt= FCttl2;
       scDtlCn3Tt= FCttl3;
       If FCara1 <> *zeros or FCphn1 <> *zeros;
          scDtlC1Ph# = %Int(%Editc(FCara1:'X') + %Editc(FCphn1:'X'));
       Endif;
       If FCara2 <> *zeros or FCphn2 <> *zeros;
          scDtlC2Ph# = %Int(%Editc(FCara2:'X') + %Editc(FCphn2:'X'));
       Endif;
       If FCara3 <> *zeros or FCphn3 <> *zeros;
          scDtlC3Ph# = %Int(%Editc(FCara3:'X') + %Editc(FCphn3:'X'));
       Endif;

       // Group Auxiliary Info
       Chain(n) keyWMFCL pf200;
       If not %found();
          clear pf200;
       Endif;
       scDtlFeId = F2fein;
       scDtlIRSCd= F2tcc;
       scDtlIntAu= F2iaud;
       scDtlSrvCd= F2sctc;
       If F2area <> *zeros or F2sph# <> *zeros;
          scDtlAuPh# = %Int(%Editc(F2area:'X') + %Editc(F2sph#:'X'));
       Endif;
       scDtlNCCr#= F2car#;
       scAuxSvTPA= F2tpa;
       scAuxEleRp= F2wcpl;
       If F2wcpl = ' ';
          scAuxEleRp = 'N';
       Endif;
       scAuxPrClP= F2clck;
       If F2clck = ' ';
          scAuxPrClP = 'N';
       Endif;
       scAuxTyPym= F2ctyp;
       scAuxPrAgt= F2agck;
       If F2agck = ' ';
          scAuxPrAgt = 'N';
       Endif;
       scDtlAgtCm= F2macc;
       scDtlNAICd= F2naic;
       scDtlCMSId= F2rre#;
       scAuxCntCd= F2tpac;
       scDtlLBAd1= F2lad1;
       scDtlLBAd2= F2lad2;
       scDtlLBCty= F2lcty;
       scDtlLBSt = F2lst;
       scDtlLBZip= F2lzip;
       scDtlBlgLg= F2blog;
       scDtlInsOv= F2ceov;
       scDtlSgPgS= F2spsg;
       scDtlFold = F2fold;
       scDtlDocum= F2dcid;
       scAuxDocLg= F2docl;
       scAuxDocSg= F2docs;
       scDtlStMth= F2fysm;
       scDtlPP3C = F2pp3c;
       scDtlPPC3 = F2ppc3;

       xxKey = 'WC';
       Chain xxKey swc01;
       If WCFL51 = 'Y';
          scDtlJbQuN= F2jobq;
          scDtlJbSbS= F2sbs;
       Endif;
       IN *lock xxsbs;
       scAuxDefSb = xxsbs;
       unlock xxsbs;

       // Group Carrier Info
       Chain(n) keyWMFCL pgc00;
       If not %found();
          clear pgc00;
       Endif;
       scDefCarCd = GCdcar;
       scPolSymb  = GCpsym;
       scCompCd   = GCccod;
       scDtlPgmId = GCpgid;
       scCar#Req  = GCcrfl;
       If GCcrfl = ' ';
          scCar#Req  = 'N';
       Endif;
       scAsnCarQt = GCacfl;
       If GCacfl = ' ';
          scAsnCarQt = 'N';
       Endif;
       scUpdCar#  = GCucfl;
       If GCucfl = ' ';
          scUpdCar#  = 'N';
       Endif;
       scEdtDpCr# = GCdcfl;
       If GCdcfl = ' ';
          scEdtDpCr# = 'N';
       Endif;
       scCarNumFm = GCcfmt;
       scCarNmFrm = GCfrcn;
       scCarNmTo  = GCtocn;
       scNxtCarNm = GCnacn;

       // Group Policy Year Info
       dspGrPlYr = *off;
       clrGrPlYr = *on;
       rrnGrPlYr = 0;
       Write rcdGrpWdw;

       dspGrPlYr = *on;
       clrGrPlYr = *off;

       setll keyWMFCL pf102;
       reade keyWMFCL pf102;
       Dow not %eof;
          scGrPlYear = F1fyr;
          scGrStDate = CvtToDate(F1stpo);
          scGrEnDate = CvtToDate(F1enpo);

          If inMode = 'D';
             scPlYrHdg = 'View';
          Else;
             scPlYrHdg = 'Edit';
          Endif;
          sflGrpRrn = dbrrn5;
          rrnGrPlYr = rrnGrPlYr + 1;
          Write sflGrpPlYr;

          reade keyWMFCL pf102;
       Enddo;

       If %Open(wpigrppop) And iOpen;
          Close wpigrppop;
       Endif;

       Return *On;
      /end-free
     pLoadDetail       e
      *=====================================================================
      * Clear Detail Info
      *=====================================================================
     pClearDetail      b                   export
     dClearDetail      pi              n
     d inPtrGrpDtl                     *

      /free

       ptrGrpDtl = inPtrGrpDtl;
       clear erDtlGrp#;
       clear erDtlGrpNm;
       clear erDtlShNm;
       clear erDtlMlCty;
       clear erDtlMlSt;
       clear erDtlMlZp;
       clear erDtlPhCty;
       clear erDtlPhSt;
       clear erDtlPhZp;
       clear erDtlEffDt;
       clear erDtlExpDt;
       clear erDtlMxPl#;
       clear erDtlCase#;
       clear erDtlEmp#;
       clear erDtlQte#;
       clear erDtlTrMed;
       clear erDtlPVMed;
       clear erDtlTrCmp;
       clear erDtlPVCmp;
       clear erDtlTrExp;
       clear erDtlPVExp;
       clear erDtlTrLgl;
       clear erDtlPVLgl;
       clear erDtlMedAm;
       clear erDtlMedRv;
       clear erDtlExpAm;
       clear erDtlExpRv;
       clear erDtlChkFr;
       clear erDtlChkTo;
       clear erDtlChgEx;
       clear erDtlCalSt;
       clear erDtlEDIFm;
       clear erDtlDueDt;
       clear erDtlCmpNm;
       clear erDtlCoAd1;
       clear erDtlCoCty;
       clear erDtlCoSt;
       clear erDtlCoZip;
       clear erDtlCoAd2;
       clear erDtlShOpP;
       clear erDtlCnt1;
       clear erDtlCn1Tt;
       clear erDtlC1Ph#;
       clear erTabErr;
       clear msDtlJbQuN;
       clear msDtlJbSbs;
       clear erDtlLBAd1;
       clear erDtlLBZip;
       clear erDtlBlgLg;
       clear erDtlLBCty;
       clear erDtlFeId;
       clear erDtlIRSCd;
       clear erDtlIntAu;
       clear erDtlSrvCd;
       clear erDtlNCCr#;
       clear erDtlAgtCm;
       clear erDtlInsOv;
       clear erDtlSgPgS;
       clear erDtlFold;
       clear erDtlDocum;
       clear erDtlJbQuN;
       clear erDtlJbSbS;
       clear erDefCarCd;

       Return *On;
      /end-free
     pClearDetail      e
      //*** Beg Add ***** 01/17/22 *****************************************
       //===================================================================
       // CopyEndrsForm - Copy selected endorsements to To Group
       //===================================================================
     p CopyEndrsForm   b
     d CopyEndrsForm   pi
     d inFromGroup                    3  0
     d inSkipRepl                     1    const

     d inToGroup       s              3  0
      /Free

       intCo# = userCo#(q1user);
       index = 1;
       //*** Beg Chg ***** 03/27/23 *****************************************
       //***** Dow index <= 9999 and arrEndrs(index) <> ' ';
       Dow index <= 9999 and arrEndrs(index).EndrFrm <> ' ';
       //*** End Chg ***** 03/27/23 *****************************************
           ReadC sflToGrp;
           Dow Not %Eof;
              If scSelToGrp = 'Y';
                 inToGroup = scToGrp;
                 nxChgToGrp = *On;
                 update sflToGrp;
                 Select;
                 //Replace when duplicates found in To Group
                 When inSkipRepl = 'R';
                    //Check if form exist in To Group
                    chain (intCo#:inToGroup:arrEndrs(index).EndrFrm) pen00;
                    If %Found(wtenl);
                       //Delete the existing endorsement form and write new form in To Group
                       Delete pen00;
                       Exsr WriteWTenp;
                    Else;
                       Exsr WriteWTenp;
                    Endif;
                 //Skip duplicate forms When duplicate found in To Group
                 When inSkipRepl = 'S';
                    //Check if form exist in To Group
                    chain(n) (intCo#:inToGroup:
                     arrEndrs(index).EndrFrm) pen00;
                    If not %Found(wtenl);
                       //Write to To Group if form does not exist in To Group
                       Exsr WriteWTenp;
                    Endif;
                 //No dupllicate forms found in To Group, Copy all the forms
                 When inSkipRepl = 'A';
                    Exsr WriteWTenp;
                 Endsl;
              Endif;
              ReadC sflToGrp;
           Enddo;
          index += 1;
       Enddo;

       Begsr WriteWTenp;
         //Write to To Group if form does not exist in To Group
         Chain(n) arrEndrs(index).EndrRrn pen0p;
         If %Found(wtenp);
            ENfnd = inToGroup;
            ENpgm  = Q1pgm;
            ENuser = Q1user;
            ENtime = Timeto6(SystemTime());
            ENdt   = Dateto7(SystemDate());
            Write pen0p;
         Endif;
       Endsr;

      /End-Free
     p CopyEndrsForm   e
      //*** End Add ***** 01/17/22 *****************************************
      *=====================================================================
      * ValidGrpDtl - Validate Group Detail Data
      *  Parm1   - inMode
      *  Parm2   - Pointer to format DS
      *  Parm3   - Group RRN
      *  Returns - Error flag '0' - error, '1' - no error
      *=====================================================================
     pValidGrpDtl      b                   export
     dValidGrpDtl      pi              n
     d inMode                         1    const
     d inPtrGrpDtl                     *
     d inFNrrn                        9  0

     d isError         s               n   inz(*off)
     d isAdmOnly       s               n   inz
     d isClmOnly       s               n   inz
     d iOpen           s               n   inz
     d intFrDt         s              7  0
     d intToDt         s              7  0
     d xxKey           s              2

      /free

       ptrGrpDtl = inPtrGrpDtl;
       GetClientId(ClientID);

       xxKey = 'WC';
       Chain xxKey swc01;
       If WCFL08 = 'A';
          isAdmOnly = *on;
       Endif;
       If WCFL08 = 'C';
          isClmOnly = *on;
       Endif;
       intCo# = userCo#(q1user);
       ClearDetail(ptrGrpDtl);

       If not %Open(wpigrppop);
          Open wpigrppop;
          iOpen = *On;
       Endif;

       // Group# validation
       If scDtlGrp# <= 0;
          isError = *On;
          erDtlGrp# = *On;
          mgDtlGrp# = 'WCR0502';
        Else;
          erDtlGrp# = *Off;
       Endif;

       If isError = *off and inMode = 'A';
          Chain (intCo#:scDtlGrp#) pfn00;
          If %found;
             isError = *On;
             erDtlGrp# = *On;
             mgDtlGrp# = 'WC30001';
          Endif;
       Endif;

       // Group name validation
       If scDtlGrpNm = ' ';
          isError = *On;
          erDtlGrpNm = *On;
          mgDtlGrpNm = 'WCR0503';
       Endif;

       // short name validation
       If scDtlShNm = ' ';
          isError = *On;
          erDtlShNm = *On;
          mgDtlShNm = 'WCR0503';
       Endif;

       // Mailing address validation
       If scDtlMlAd1 <> ' ';
          If scDtlMlCty = ' ';
             isError = *On;
             erDtlMlCty = *On;
             mgDtlMlCty = 'WCR0503';
          Endif;
          If scDtlMlSt = ' ';
             isError = *On;
             erDtlMlSt = *On;
             mgDtlMlSt = 'WCR0503';
          Endif;
          If scDtlMlZip = 0;
             isError = *On;
             erDtlMlZp = *On;
             mgDtlMlZp = 'WCR0503';
          Endif;
       Endif;

       // Physical address validation
       If scDtlPhAd1 <> ' ';
          If scDtlPhCty = ' ';
             isError = *On;
             erDtlPhCty = *On;
             mgDtlPhCty = 'WCR0503';
          Endif;
          If scDtlPhSt = ' ';
             isError = *On;
             erDtlPhSt = *On;
             mgDtlPhSt = 'WCR0503';
          Endif;
          If scDtlPhZip = 0;
             isError = *On;
             erDtlPhZp = *On;
             mgDtlPhZp = 'WCR0503';
          Endif;
       Endif;

       // Validate Effective date
       intFrDt = DtCYMD(scDtlEffDt);
       If intFrDt = 0;
          isError = *On;
          erDtlEffDt = *On;
          mgDtlEffDt = 'WCR0500';
       Endif;

       // Validate Expiration date
       If scDtlExpDt > 0;
          intToDt = DtCYMD(scDtlExpDt);
          If intToDt = 0;
             isError = *On;
             erDtlExpDt = *On;
             mgDtlExpDt = 'WCR0500';
          Endif;
       Else;
          intToDt = 0;
       Endif;

       // If Effetive date > Expiration date
       If erDtlEffDt <> *on and erDtlExpDt <> *on and
        intFrDt > 0 and intToDt > 0 and intFrDt > intToDt;
          mgDtlEffDt = 'WCR0529';
          mgDtlExpDt = 'WCR0529';
          erDtlEffDt = *on;
          erDtlExpDt = *on;
          isError = *on;
       Endif;

       // Validate Effective date
       If intFrDt <> FNefdt and inMode = 'C';
          Setll  (FNCo#:FNfnd:intFrDt) pel03;
          ReadPe (FNCo#:FNfnd) pel03;
          If not %eof();
             If not PopTwoBtn('Employer Policy FROM Date prior to +
              entered Group Effective Date exists.':'Continue':'Cancel');
                isError = *On;
             Endif;
          Endif;
       Endif;

       // Validate Max Extended Policy
       If WCfl07 = 'Y' and scDtlMxPl# <> 0 and scDtlMxPl# < 12;
          isError = *On;
          erDtlMxPl# = *On;
          mgDtlMxPl# = 'WCR2376';
       Endif;

       // Validate Case#
       If inMode = 'A' and isAdmOnly = *off and scDtlCase# <= 0;
          isError = *On;
          erDtlCase# = *On;
          mgDtlCase# = 'WCR0502';
       Endif;

       // Validate Next Available Emp#
       If inMode = 'A' and scDtlEmp# <= 0;
          isError = *On;
          erDtlEmp# = *On;
          mgDtlEmp# = 'WCR0502';
       Endif;

       // Validate Next Available Quote#
       If inMode = 'A' and isClmOnly = *off and scDtlQte# <= 0;
          isError = *On;
          erDtlQte# = *On;
          mgDtlQte# = 'WCR0502';
       Endif;

       // Validate Transaction and Payment value code
       If (scDtlTrMed <> ' ' and scDtlPVMed = ' ') or
        (scDtlTrMed = ' ' and scDtlPVMed <> ' ');
          isError = *On;
          erDtlTrMed = *On;
          mgDtlTrMed = 'WCR0544';
          erDtlPVMed = *On;
          mgDtlPVMed = 'WCR0544';
       Endif;

       If (scDtlTrCmp <> ' ' and scDtlPVCmp = ' ') or
        (scDtlTrCmp = ' ' and scDtlPVCmp <> ' ');
          isError = *On;
          erDtlTrCmp = *On;
          mgDtlTrCmp = 'WCR0544';
          erDtlPVCmp = *On;
          mgDtlPVCmp = 'WCR0544';
       Endif;

       If (scDtlTrExp <> ' ' and scDtlPVExp = ' ') or
        (scDtlTrExp = ' ' and scDtlPVExp <> ' ');
          isError = *On;
          erDtlTrExp = *On;
          mgDtlTrExp = 'WCR0544';
          erDtlPVExp = *On;
          mgDtlPVExp = 'WCR0544';
       Endif;

       If (scDtlTrLgl <> ' ' and scDtlPVLgl = ' ') or
        (scDtlTrLgl = ' ' and scDtlPVLgl <> ' ');
          isError = *On;
          erDtlTrLgl = *On;
          mgDtlTrLgl = 'WCR0544';
          erDtlPVLgl = *On;
          mgDtlPVLgl = 'WCR0544';
       Endif;

       If scDtlMedRv = ' ' and scDtlMedAm <> 0;
          isError = *On;
          erDtlMedAm = *On;
          mgDtlMedAm = 'WCR0544';
          erDtlMedRv = *On;
          mgDtlMedRv = 'WCR0544';
       Endif;

       If scDtlMedAm < 0;
          isError = *On;
          erDtlMedAm = *On;
          mgDtlMedAm = 'WCR0547';
       Endif;

       If scDtlExpRv = ' ' and scDtlExpAm <> 0;
          isError = *On;
          erDtlExpAm = *On;
          mgDtlExpAm = 'WCR0544';
          erDtlExpRv = *On;
          mgDtlExpRv = 'WCR0544';
       Endif;

       If scDtlExpAm < 0;
          isError = *On;
          erDtlExpAm = *On;
          mgDtlExpAm = 'WCR0547';
       Endif;

       If scDtlChkFr > scDtlChkTo;
          isError = *On;
          erDtlChkFr = *On;
          mgDtlChkFr = 'WCR0559';
          erDtlChkTo = *On;
          mgDtlChkTo = 'WCR0559';
       Endif;

       If WCfl30 = 'Y' and scDtlChgEx = 'Y' and scDtlPEOGp <> 'M' and
        scDtlPEOGp <> 'G';
          isError = *On;
          erDtlChgEx = *On;
          mgDtlChgEx = 'WC31522';
       Endif;

       If WCfl30 = 'Y' and scDtlCalSt = 'Y' and scDtlPEOGp <> 'M' and
        scDtlPEOGp <> 'G';
          isError = *On;
          erDtlCalSt = *On;
          mgDtlCalSt = 'WC31522';
       Endif;

       If inMode = 'A' and scDtlEDIFm = ' ';
          outCo# = intCo#;
          outFnd = 0;
          outRtcd = ' ';
          GetEDIFmID(outCo#:outFnd:outRtcd);
          If outRtcd = 'F';
             isError = *On;
             erDtlEDIFm = *On;
             mgDtlEDIFm = 'WC31531';
          Endif;
       Endif;

       If scDtlTstGr = 'Y';
          scDtlGrCIC = ' ';
       Endif;

       If isClmOnly = *off and (scDtlDueDt < 0 or scDtlDueDt >28);
          isError = *On;
          erDtlDueDt = *On;
          mgDtlDueDt = 'WC30125';
       Endif;

       If (scDtlCoAd1 <> ' ' or scDtlCoCty <> ' ' or scDtlCoSt <> ' ' or
        scDtlCoZip <> 0) and scDtlCmpNm = ' ';
          isError = *On;
          erDtlCmpNm = *On;
          mgDtlCmpNm = 'WCR0503';
       Endif;

       If scDtlCmpNm <> ' ';
          If scDtlCoAd1 = ' ';
             isError = *On;
             erDtlCoAd1 = *On;
             mgDtlCoAd1 = 'WCR0503';
          Endif;

          If scDtlCoCty = ' ';
             isError = *On;
             erDtlCoCty = *On;
             mgDtlCoCty = 'WCR0503';
          Endif;

          If scDtlCoSt = ' ';
             isError = *On;
             erDtlCoSt = *On;
             mgDtlCoSt = 'WCR0503';
          Endif;

          If scDtlCoZip = 0;
             isError = *On;
             erDtlCoZip = *On;
             mgDtlCoZip = 'WCR0556';
          Endif;
       Endif;

       If (scDtlCoAd1 = ' ' and scDtlCoCty = ' ' and scDtlCoSt = ' ' and
        scDtlCoZip = 0) and scDtlCoAd2 <> ' ';
          isError = *On;
          erDtlCoAd2 = *On;
          mgDtlCoAd2 = 'WC30121';
       Endif;

       If scDtlShOpP <> 'A' and scDtlShOpP <> 'D';
          isError = *On;
          erDtlShOpP = *On;
          mgDtlShOpP = 'WCR3389';
       Endif;

       // Group Contact Detail validation
       If scDtlCnt1 = ' ';
          isError = *On;
          erDtlCnt1 = *On;
          mgDtlCnt1 = 'WC40059';
       Endif;

       If scDtlCn1Tt = ' ';
          isError = *On;
          erDtlCn1Tt = *On;
          mgDtlCn1Tt = 'WC40060';
       Endif;

       If scDtlC1Ph# = 0;
          isError = *On;
          erDtlC1Ph# = *On;
          mgDtlC1Ph# = 'WC40062';
       Endif;

       // Group Auxiliary detail validation
       If scDtlFeId <= 0;
          isError = *On;
          erDtlFeId = *On;
          mgDtlFeId = 'WCR0502';
       Endif;

       If scDtlIRSCd = ' ';
          isError = *On;
          erDtlIRSCd = *On;
          mgDtlIRSCd = 'WCR0503';
       Endif;

       If scDtlIntAu < 0;
          isError = *On;
          erDtlIntAu = *On;
          mgDtlIntAu = 'WCR0547';
       Endif;

       If scDtlSrvCd < 0;
          isError = *On;
          erDtlSrvCd = *On;
          mgDtlSrvCd = 'WCR0547';
       Endif;

       If isClmOnly = *off and scDtlNCCr# <= 0 and scAuxEleRp = 'Y';
          isError = *On;
          erDtlNCCr# = *On;
          mgDtlNCCr# = 'WCR0547';
       Endif;

       If isClmOnly = *off and scDtlAgtCm < 0 and scAuxPrAgt = 'Y';
          isError = *On;
          erDtlAgtCm = *On;
          mgDtlAgtCm = 'WCR0547';
       Endif;

       If scDtlLBAd1 <> ' ' or scDtlLBAd2 <> ' ' or scDtlLBCty <> ' ' or
        scDtlLBZip > 0;
          If scDtlLBAd1 = ' ';
             isError = *On;
             erDtlLBAd1 = *On;
             mgDtlLBAd1 = 'WCR0503';
          Endif;
          If scDtlLBCty = ' ';
             isError = *On;
             erDtlLBCty = *On;
             mgDtlLBCty = 'WCR0503';
          Endif;
          If scDtlLBZip = 0;
             isError = *On;
             erDtlLBZip = *On;
             mgDtlLBZip = 'WCR0502';
          Endif;
       Endif;

       If scDtlBlgLg <> ' ';
          outName = scDtlBlgLg;
          outType = '*PAGSEG';
          outRtcd = ' ';
          GetObject(outName:outType:outRtcd);
          If outRtcd = '1';
             isError = *On;
             erDtlBlgLg = *On;
             mgDtlBlgLg = 'WC30292';
          Endif;
       Endif;

       If scDtlInsOv <> ' ';
          outName = scDtlInsOv;
          outType = '*OVL   ';
          outRtcd = ' ';
          GetObject(outName:outType:outRtcd);
          If outRtcd = '1';
             isError = *On;
             erDtlInsOv = *On;
             msDtlInsOv = 'Overlay object not found on system.';
          Endif;
       Endif;

       If scDtlSgPgS <> ' ';
          outName = scDtlSgPgS;
          outType = '*PAGSEG';
          outRtcd = ' ';
          GetObject(outName:outType:outRtcd);
          If outRtcd = '1';
             isError = *On;
             erDtlSgPgS = *On;
          Endif;
       Endif;

       If scDtlFold <> ' ';
          outFold = scDtlFold;
          outRtcd = ' ';
          ChkFolder(outFold:outRtcd);
          If outRtcd = '1';
             isError = *On;
             erDtlFold = *On;
             mgDtlFold = 'WCR0826';
             lbDtlFold = 'WMESGG';
          Endif;
          If outRtcd = '2';
             isError = *On;
             erDtlFold = *On;
             mgDtlFold = 'SCM0517';
             lbDtlFold = 'SCMESG';
          Endif;
       Endif;

       If scDtlDocum <> ' ';
          outFld = scDtlFold;
          outDoc = scDtlDocum;
          outRtcd = ' ';
          ChkDoc(outDoc:outFld:outRtcd);
          If outRtcd = '1';
             isError = *On;
             erDtlDocum = *On;
             mgDtlDocum = 'WCR0825';
          Endif;
       Endif;

       // Check for Valid JobQ entered at Screen.
       If scDtlJbQuN <> ' ';
          outName = scDtlJbQuN;
          outRtcd = ' ';
          VldJobQ(outName:outRtcd);
          If outRtcd = '1';
             isError = *On;
             erDtlJbQuN = *On;
             mgDtlJbQuN = 'WCR3384';
          Endif;
       Endif;

       // Subsystem/JobQ are Mutually Exclusive
       If (scDtlJbQuN <> ' ' and scDtlJbSbS = ' ') or
        (scDtlJbQuN = ' ' and scDtlJbSbS <> ' ');
          isError = *On;
          erDtlJbQuN = *On;
          mgDtlJbQuN = 'WCR3381';
          erDtlJbSbS = *On;
          mgDtlJbSbS = 'WCR3381';
       Endif;

       // Check existence of Subsystem entered at Screen.
       If scDtlJbSbS <> ' ';
          outName = scDtlJbSbS;
          outType = '*SBSD  ';
          outRtcd = ' ';
          GetObject(outName:outType:outRtcd);
          If outRtcd = '1';
             isError = *On;
             erDtlJbSbS = *On;
             mgDtlJbSbS = 'WCR3380';
          Endif;
       Endif;

       // Jobq/Subsystem entered first time at screen.
       Chain(n) (intCo#:scDtlGrp#) pf200;
       If %found and F2jobq = ' ' and scDtlJbQuN <> ' ';
          outName = scDtlJbQuN;
          outType = '*JOBQ  ';
          outRtcd = ' ';
          GetObject(outName:outType:outRtcd);
          // If Entered JobQ already exist.
          If outRtcd = '0';
             outJobQ = scDtlJbQuN;
             outLib  = 'QGPL';
             outSts  = ' ';
             outName = ' ';
             outCnt  = 0;
             RtvJobQ(outJobQ:outLib:outSts:outName:outCnt);

             // If JobQ already assigned(returned subsystem differ from screen)
             If outName <> scDtlJbSbS;
                msDtlJbQuN =  outJobQ;
                isError = *On;
                erDtlJbQuN = *On;
                mgDtlJbQuN = 'WCR3385';
             Endif;
          Endif;
       Endif;

       // Jobq/Subsystem is changes at screen.
       If (F2jobq <> ' ' and scDtlJbQuN <> F2jobq) or
        (F2sbs <> ' ' and scDtlJbSbS <> F2sbs);
          // Check whether Old JobQ already exist
          outName = F2jobq;
          outType = '*JOBQ  ';
          outRtcd = ' ';
          GetObject(outName:outType:outRtcd);
          // If Old JobQ already exist.
          If outRtcd = '0';
             outJobQ = F2jobq;
             outLib  = 'QGPL';
             outSts  = ' ';
             outName = ' ';
             outCnt  = 0;
             RtvJobQ(outJobQ:outLib:outSts:outName:outCnt);

             // If JobQ/Subsystem changed and Old JobQ is in use (has any Job).
             If outCnt <> *zeros;
                If scDtlJbQuN <> F2jobq;
                   msDtlJbQuN =  F2jobq;
                   isError = *On;
                   erDtlJbQuN = *On;
                   mgDtlJbQuN = 'WCR3386';
                Else;
                   msDtlJbSbs =  F2sbs;
                   isError = *On;
                   erDtlJbSbS = *On;
                   mgDtlJbSbS = 'WCR3387';
                Endif;
             Endif;
          Endif;
          If scDtlJbQuN <> F2jobq;
             outName = scDtlJbQuN;
             outType = '*JOBQ  ';
             outRtcd = ' ';
             GetObject(outName:outType:outRtcd);
             // If Changed JobQ already exist.
             If outRtcd = '0';
                outJobQ = scDtlJbQuN;
                outLib  = 'QGPL';
                outSts  = ' ';
                outName = ' ';
                outCnt  = 0;
                // Call program whether it is already assigned to any subsystem
                RtvJobQ(outJobQ:outLib:outSts:outName:outCnt);
                If outName <> scDtlJbSbS;
                   msDtlJbQuN =  outJobQ;
                   isError = *On;
                   erDtlJbQuN = *On;
                   mgDtlJbQuN = 'WCR3385';
                Endif;
             Endif;
          Endif;
       Endif;

       // Call Program to Create JobQ
       If isError = *off;
          outCo = %editc(intCo#:'X');
          outFnd# = %editc(scDtlGrp#:'X');
          outJobQ = F2jobq;
          outName = scDtlJbQuN;
          outSbs  = F2sbs;
          outSbs1 = scDtlJbSbS;
          outMsg  = ' ';
          outRtcd = ' ';
          crtJobQ(outCo:outFnd#:outJobQ:outName:outSbs:outSbs1:outMsg:
           outRtcd);
          If outRtcd <> ' ';
             msDtlJbQuN =  outMsg;
             isError = *On;
             erDtlJbQuN = *On;
             mgDtlJbQuN = 'WCR3382';
          Endif;
       Endif;

       // Carrier Auxiliary Info
       If scDefCarCd = ' ';
          isError = *On;
          erDefCarCd = *On;
          mgDefCarCd = 'WCR0503';
       Endif;


       If %Open(wpigrppop) And iOpen;
          Close wpigrppop;
       Endif;

       Return not isError;

      /end-free
     pValidGrpDtl      e
      *=====================================================================
      * UpdateGrpDtl - Update Group Detail Information
      *  Parm1   - Pointer to format DS
      *  Parm2   - RRN of Group Master record
      *  Parm3   - Mode
      *  Returns - Error indicator
      *=====================================================================
     pUpdateGrpDtl     b                   export
     dUpdateGrpDtl     pi              n
     d inPtrGrpDtl                     *
     d inRRN                          9  0
     d inMode                         1

     d xxKey           s              2

     c     keyWMFCL      klist
     c                   kfld                    FNco#
     c                   kfld                    FNfnd

      /free
       If inMode = 'C';
          Chain inRrn pfn0p;
       EndIf;

       ptrGrpDtl = inPtrGrpDtl;
       GetClientId(ClientID);

       FNname = %trim(scDtlGrpNm);
       FNshnm = %trim(scDtlShNm);
       FNefdt = dtCYMD(scDtlEffdt);
       FNexdt = dtCYMD(scDtlExpDt);
       FNmad1 = %trim(scDtlMlAd1);
       FNmad2 = %trim(scDtlMlAd2);
       FNmcty = %trim(scDtlMlCty);
       FNmst  = scDtlMlSt;
       FNmzip = scDtlMlZip;
       FNpad1 = %trim(scDtlPhAd1);
       FNpad2 = %trim(scDtlPhAd2);
       FNpcty = %trim(scDtlPhCty);
       FNpst  = scDtlPhSt;
       FNpzip = scDtlPhZip;
       FNaad1 = %trim(scDtlCoAd1);
       FNaad2 = %trim(scDtlCoAd2);
       FNacty = %trim(scDtlCoCty);
       FNast  = scDtlCoSt;
       FNazip = scDtlCoZip;
       FNiadr = %trim(scDtlIntAd);
       If scDtlPhn1# > 0;
          FNph1a = %Int(scDtlPhn1#/10000000);
          FNph1  = scDtlPhn1# - (FNph1a*10000000);
       Else;
          FNph1a = 0;
          FNph1  = 0;
       Endif;
       If scDtlFax# > 0;
          FNfaxa = %Int(scDtlFax#/10000000);
          FNfax  = scDtlFax# - (FNfaxa*10000000);
       Else;
          FNfaxa = 0;
          FNfax  = 0;
       Endif;
       If scDtlPhn2# > 0;
          FNarea = %Int(scDtlPhn2#/10000000);
          FNPh2  = scDtlPhn2# - (FNarea*10000000);
       Else;
          FNarea = 0;
          FNPh2  = 0;
       Endif;
       FNmxex = scDtlMxPl#;
       FNnacs = scDtlCase#;
       FNnaen = scDtlEmp#;
       FNnaqn = scDtlQte#;
       FNaute = scDtlAtEmp;
       FNautd = scDtlAtDiv;
       FNdiv0 = scDtlDiv0;
       FNrs01 = scDtlRvCd1;
       FNrs02 = scDtlRvCd2;
       FNrs03 = scDtlRvCd3;
       FNrs04 = scDtlRvCd4;
       FNrs05 = scDtlRvCd5;
       FNrs06 = scDtlRvCd6;
       FNrs07 = scDtlRvCd7;
       FNrs08 = scDtlRvCd8;
       FNrs09 = scDtlRvCd9;
       FNrs10 = scDtlRvC10;
       FNmetr = scDtlTrMed;
       FNcotr = scDtlTrCmp;
       FNextr = scDtlTrExp;
       FNletr = scDtlTrLgl;
       FNmep1 = scDtlPVMed;
       FNcop1 = scDtlPVCmp;
       FNexp1 = scDtlPVExp;
       FNlep1 = scDtlPVLgl;
       FNdfrc = scDtlMedRv;
       FNdfxc = scDtlExpRv;
       FNdfra = scDtlMedAm;
       FNdfxa = scDtlExpAm;
       FNchk1 = scDtlChkFr;
       FNchk2 = scDtlChkTo;
       FNcicf = scDtlGrCIC;
       FNedi  = scDtlExEDI;
       FNsfin = scDtlEmSIn;
       FNsior = scDtlSfOrg;
       FNsiau = scDtlSfAut;
       If ClientID <> '114';
          FNpeof = scDtlPEOGp;
       Else;
          FNpeof = scPEOGrpT;
       Endif;
       FNpeoe = scDtlChgEx;
       FNpeoa = scDtlCalSt;
       FNpeos = scDtlMdRSb;
       FNmidy = scDtlDueDt;
       FNlost = scDtlQtLst;
       FNanam = scDtlCmpNm;
       FNdepo = scDtlShOpP;
       //** Begin Chg *** 10/07/13 ****************************************
       FNprmv = scPRMV;
       //** End   Chg *** 10/07/13 ****************************************
       FNpde  = scDtlPrtDv;
       FNaclr = scDtlAtEnL;
       FNaer  = scDtlAtAtt;
       FN3ppe = scDtl3rdPt;
       FNcgfl = scDtlCmGrp;
       FNtest = scDtlTstGr;
       FNedif = scDtlEDIFm;

       FNpgm  = Q1pgm;
       FNuser = Q1user;
       FNtime = Timeto6(SystemTime());
       FNdt   = Dateto7(SystemDate());

       If inMode = 'C';
          update pfn0p;
       Else;
          FnCo#  = userCo#(q1user);
          FNfnd  = scDtlGrp#;
          FNipgm = Q1pgm;
          FNiusr = Q1user;
          FNitim = Timeto6(SystemTime());
          FNidt  = Dateto7(SystemDate());
          write pfn0p;
          inRrn = dbrrn4;
       EndIf;
       unlock WMFNP;

       // group contact info
       Chain keyWMFCL pfc00;
       FCcon1 = scDtlCnt1;
       FCcon2 = scDtlCnt2;
       FCcon3 = scDtlCnt3;
       FCttl1 = scDtlCn1Tt;
       FCttl2 = scDtlCn2Tt;
       FCttl3 = scDtlCn3Tt;
       If scDtlC1Ph# > 0;
          FCara1 = %Int(scDtlC1Ph#/10000000);
          FCphn1 = scDtlC1Ph# - (FCara1*10000000);
       Else;
          FCara1 = 0;
          FCphn1 = 0;
       Endif;
       If scDtlC2Ph# > 0;
          FCara2 = %Int(scDtlC2Ph#/10000000);
          FCphn2 = scDtlC2Ph# - (FCara2*10000000);
       Else;
          FCara2 = 0;
          FCphn2 = 0;
       Endif;
       If scDtlC3Ph# > 0;
          FCara3 = %Int(scDtlC3Ph#/10000000);
          FCphn3 = scDtlC3Ph# - (FCara3*10000000);
       Else;
          FCara3 = 0;
          FCphn3 = 0;
       Endif;
       FCpgm = Q1pgm;
       FCusr = Q1user;
       FCtim = Timeto6(SystemTime());
       FCdt  = Dateto7(SystemDate());

       If %found();
          update pfc00;
       Else;
          FCco#  = FNco#;
          FCfnd  = FNfnd;
          FCstat = 'A';
          FCipgm = Q1pgm;
          FCiusr = Q1user;
          FCitim = Timeto6(SystemTime());
          FCidt  = Dateto7(SystemDate());
          write pfc00;
       EndIf;
       unlock WMFCL;

       // Group Auxiliary Info
       Chain keyWMFCL pf200;
       F2fein = scDtlFeId;
       F2tcc  = scDtlIRSCd;
       F2iaud = scDtlIntAu;
       F2sctc = scDtlSrvCd;
       If scDtlAuPh# > 0;
          F2area = %Int(scDtlAuPh#/10000000);
          F2sph# = scDtlAuPh# - (F2area*10000000);
       Else;
          F2area = 0;
          F2sph# = 0;
       Endif;
       F2car# = scDtlNCCr#;
       F2tpa  = scAuxSvTPA;
       F2wcpl = scAuxEleRp;
       F2clck = scAuxPrClP;
       F2ctyp = scAuxTyPym;
       F2agck = scAuxPrAgt;
       F2macc = scDtlAgtCm;
       F2naic = scDtlNAICd;
       F2rre# = scDtlCMSId;
       F2tpac = scAuxCntCd;
       F2lad1 = scDtlLBAd1;
       F2lad2 = scDtlLBAd2;
       F2lcty = scDtlLBCty;
       F2lst  = scDtlLBSt;
       F2lzip = scDtlLBZip;
       F2blog = scDtlBlgLg;
       F2ceov = scDtlInsOv;
       F2spsg = scDtlSgPgS;
       F2fold = scDtlFold ;
       F2dcid = scDtlDocum;
       F2docl = scAuxDocLg;
       F2docs = scAuxDocSg;
       F2fysm = scDtlStMth;
       F2pp3c = scDtlPP3C;
       F2ppc3 = scDtlPPC3;
       xxKey = 'WC';
       Chain xxKey swc01;
       If WCFL51 = 'Y';
          F2jobq = scDtlJbQuN;
          F2sbs  = scDtlJbSbS;
       Endif;
       F2pgm  = Q1pgm;
       F2user = Q1user;
       F2time = Timeto6(SystemTime());
       F2dt   = Dateto7(SystemDate());

       If %found(WDF2L);
          update pf200;
       Else;
          F2co#  = FNco#;
          F2fnd  = FNfnd;
          F2ipgm = Q1pgm;
          F2iusr = Q1user;
          F2itim = Timeto6(SystemTime());
          F2idt  = Dateto7(SystemDate());
          write pf200;
       EndIf;
       unlock WDF2L;

       // Group Carrier Info
       Chain keyWMFCL pgc00;
       GCdcar = scDefCarCd;
       GCpsym = scPolSymb;
       GCccod = scCompCd;
       GCpgid = scDtlPgmId;
       GCcrfl = scCar#Req;
       GCacfl = scAsnCarQt;
       GCucfl = scUpdCar#;
       GCdcfl = scEdtDpCr#;
       GCcfmt = scCarNumFm;
       GCfrcn = scCarNmFrm;
       GCtocn = scCarNmTo;
       GCnacn = scNxtCarNm;
       GCpgm  = Q1pgm;
       GCuser = Q1user;
       GCtime = Timeto6(SystemTime());
       GCdt   = Dateto7(SystemDate());

       If %found();
          update pgc00;
       Else;
          GCco#  = FNco#;
          GCfnd  = FNfnd;
          GCipgm = Q1pgm;
          GCiusr = Q1user;
          GCidt  = Dateto7(SystemDate());
          write pgc00;
       EndIf;
       unlock WMGCL;

       Return *On;

      /end-free
     pUpdateGrpDtl     e
      *=====================================================================
      * procAddGrp - Process Group Addition
      *  Parm1   - Group RRN
      *  Parm2   - Mode(1A) 'A'-Add, 'C'-Change, 'D'-Inquiry
      *  Returns - success indicator
      *=====================================================================
     pprocAddGrp       b
     dprocAddGrp       pi              n
     d inFNrrn                        9  0
     d inMode                         1

     d isError         s               n   inz
     d isErrWdw        s               n   inz
     d isEdit          s               n   inz
     d iOpen           s               n   inz
     d ExitNow         s               n   inz(*off)
     d ExitWdw         s               n   inz(*off)
     d xxRtcd          s               n   inz

     c     keyWMFNL      klist
     c                   kfld                    FNco#
     c                   kfld                    FNfnd

     c     keyWMREL      klist
     c                   kfld                    FNco#
     c                   kfld                    FNfnd
     c                   kfld                    ##reg             3 0

      /free

       Chain(n) inFNRrn pfn0p;
       If not %Open(wpigrppop);
          Open wpigrppop;
          iOpen = *On;
       Endif;

       xxRtcd = *Off;
       qdjob  = q1job;
       qdcusr = q1cusr;
       qdjobn = q1jobn;
       If inMode <> 'A' and inMode <> 'C' and inMode <> 'D';
          Return xxRtcd;
       Endif;

       If inMode = 'A';
          dspPanel = *on;
          dspDtlBtn = *on;
       Else;
          dspPanel = *off;
          dspDtlBtn = *off;
       Endif;
       scGrpWdLft = 260;
       scAddWdTop = 75;
       dsbFlxInfo = *off;
       If inMode = 'D';
          dspLnkRgEd = *off;
          dspLnChpEd = *off;
       Else;
          dspLnkRgEd = *on;
          dspLnChpEd = *on;
       Endif;
       scShowWdw = *on;
       scDtlNext = *off;
       scDtlFnsh = *off;
       isEdit = *off;
       isError = *off;

       outName = 'W0010R64';
       outType = '*PGM   ';
       outRtcd = ' ';
       GetObject(outName:outType:outRtcd);
       If outRtcd = '0';
          dspFlxInfo = *on;
       Else;
          dspFlxInfo = *off;
       Endif;

       Dow ExitNow = *off;
          If isError = *off and dspFlxInfo = *on;
             Exsr LoadFlxInfo;
          Endif;
          Exsr LoadRegInfo;
          Exsr LoadChpInfo;
          If isFirst = *off;
             isFirst = *on;
          Endif;

          ExFmt rcdAddInf;

          Select;
          When scDtlNext = *on;
             scDtlNext = *off;
             Exsr ProcessSave;
             If xxRtcd = *on and ExitNow = *on;
                ExitNow = procAddBnk(inFNrrn:inMode);
             Endif;
          When scDtlFnsh = *on;
             scDtlFnsh = *off;
             Exsr ProcessSave;
          When scBtnRgAdd = *on;
             scBtnRgAdd = *off;
             isEdit = *off;
             outRrn = 0;
             addRegInfo(outRrn:inFNrrn:isEdit:inMode);
          When scBtnChAdd = *on;
             scBtnChAdd = *off;
             isEdit = *off;
             outRrn = 0;
             addChpInfo(outRrn:inFNrrn:isEdit:inMode);
          Other;
             ReadC sflRegInfo;
             ReadC sflChpInfo;
             isEdit = *on;
             If scLnkRgEdt = *on;
                scLnkRgEdt = *off;
                outRrn = sflRegRrn;
                addRegInfo(outRrn:inFNrrn:isEdit:inMode);
             Endif;
             If scLnChpEdt = *on;
                scLnChpEdt = *off;
                outRrn = sflChpRrn;
                addChpInfo(outRrn:inFNrrn:isEdit:inMode);
             Endif;
          EndSl;
       EndDo;

       If %Open(wpigrppop) And iOpen;
          Close wpigrppop;
       Endif;
       Return xxRtcd;
       //*************************************************************************
       // ProcessSave
       //*************************************************************************
       BegSr ProcessSave;

       If dspFlxInfo = *on;
          Chain keyWMFNL pfh00;

          FHfh#1 = scFlxHdg1;
          FHfh1q = scFlxRqQt1;
          FHfh1w = scFlxRqLv1;
          FHf1tb = scFlxTbID1;
          FHf1df = scFlxDef1;
          FHfh#2 = scFlxHdg2;
          FHfh2q = scFlxRqQt2;
          FHfh2w = scFlxRqLv2;
          FHf2tb = scFlxTbID2;
          FHf2df = scFlxDef2;
          FHfh#3 = scFlxHdg3;
          FHfh3q = scFlxRqQt3;
          FHfh3w = scFlxRqLv3;
          FHf3tb = scFlxTbID3;
          FHf3df = scFlxDef3;
          FHfh#4 = scFlxHdg4;
          FHfh4q = scFlxRqQt4;
          FHfh4w = scFlxRqLv4;
          FHfh#5 = scFlxHdg5;
          FHfh5q = scFlxRqQt5;
          FHfh5w = scFlxRqLv5;
          FHfh#6 = scFlxHdg6;
          FHfh6q = scFlxRqQt6;
          FHfh6w = scFlxRqLv6;
          FHf6tb = scFlxTbID6;
          FHf6df = scFlxDef6;
          FHpgm  = Q1pgm;
          FHuser = Q1user;
          FHtime = Timeto6(SystemTime());
          FHdt   = Dateto7(SystemDate());
          If not %found;
             FHco# = FNco#;
             FHfnd = FNfnd;
             FHipgm = Q1pgm;
             FHiusr = Q1user;
             FHitim = Timeto6(SystemTime());
             FHidt  = Dateto7(SystemDate());
             Write pfh00;
          Else;
             update pfh00;
          Endif;
       Endif;
       unlock WDFHL;

       ExitNow = *On;
       xxRtcd = *On;

       Endsr;

       //*************************************************************************
       // LoadFlxInfo
       //*************************************************************************
       BegSr LoadFlxInfo;

       // Group Flexible Info
       Chain(n) keyWMFNL pfh00;
       If not %found;
          Clear pfh00;
       Endif;

       // Field 1
       scFlxHdg1  = FHfh#1;
       scFlxRqQt1 = FHfh1q;
       scFlxRqLv1 = FHfh1w;
       scFlxTbID1 = FHf1tb;
       scFlxDef1  = FHf1df;

       // Field 2
       scFlxHdg2  = FHfh#2;
       scFlxRqQt2 = FHfh2q;
       scFlxRqLv2 = FHfh2w;
       scFlxTbID2 = FHf2tb;
       scFlxDef2  = FHf2df;

       // Field 3
       scFlxHdg3  = FHfh#3;
       scFlxRqQt3 = FHfh3q;
       scFlxRqLv3 = FHfh3w;
       scFlxTbID3 = FHf3tb;
       scFlxDef3  = FHf3df;

       // Field 4
       scFlxHdg4  = FHfh#4;
       scFlxRqQt4 = FHfh4q;
       scFlxRqLv4 = FHfh4w;

       // Field 5
       scFlxHdg5  = FHfh#5;
       scFlxRqQt5 = FHfh5q;
       scFlxRqLv5 = FHfh5w;

       // Field 6
       scFlxHdg6  = FHfh#6;
       scFlxRqQt6 = FHfh6q;
       scFlxRqLv6 = FHfh6w;
       scFlxTbID6 = FHf6tb;
       scFlxDef6  = FHf6df;

       Endsr;
       //*************************************************************************
       // LoadRegInfo
       //*************************************************************************
       BegSr LoadRegInfo;

       // Group Region Info
       dspRegInfo = *off;
       clrRegInfo = *on;
       rrnRegInfo = 0;
       dspChpInfo = *off;
       clrChpInfo = *on;
       rrnChpInfo = 0;
       Write rcdAddInf;

       dspRegInfo = *on;
       clrRegInfo = *off;
       dspChpInfo = *on;
       clrChpInfo = *off;
       dspLnkRgEd = *on;

       Setll keyWMFNL pre00;
       If not %equal and isFirst = *off and isEdit = *off;
          outRrn = 0;
          addRegInfo(outRrn:inFNrrn:isEdit:inMode);
          Setll keyWMFNL pre00;
       Endif;
       Reade(n) keyWMFNL pre00;
       Dow not %eof;

          scRegNum = RErgn#;
          scRegDsc = REdesc;
          If REstat = 'A';
             scRegSts = 'Active';
          Else;
             scRegSts = 'Inactive';
          Endif;
          sflRegRrn = dbrrn1;
          rrnRegInfo = rrnRegInfo + 1;

          Write sflRegInfo;
          Reade(n) keyWMFNL pre00;
       Enddo;
       rrnRegInfo = 1;

       Endsr;
       //*************************************************************************
       // LoadChpInfo
       //*************************************************************************
       BegSr LoadChpInfo;

       // Group Chapter Info
       dspChpInfo = *off;
       clrChpInfo = *on;
       rrnChpInfo = 0;
       Write rcdAddInf;

       dspChpInfo = *on;
       clrChpInfo = *off;
       dspLnChpEd = *on;

       Setll keyWMFNL pcp00;
       If not %equal and isFirst = *off and isEdit = *off;
          outRrn = 0;
          addChpInfo(outRrn:inFNrrn:isEdit:inMode);
          Setll keyWMFNL pcp00;
       Endif;
       Reade(n) keyWMFNL pcp00;
       Dow not %eof;

          scChpNum = CPchap;
          scChpDsc = CPdesc;
          If CPstat = 'A';
             scChpSts = 'Active';
          Else;
             scChpSts = 'Inactive';
          Endif;
          sflChpRrn = dbrrn2;
          rrnChpInfo = rrnChpInfo + 1;

          Write sflChpInfo;
          Reade(n) keyWMFNL pcp00;
       Enddo;
       rrnChpInfo = 1;

       Endsr;

      /end-free
     pprocAddGrp       e
      *=====================================================================
      * Add Chapter Info
      *=====================================================================
     paddChpInfo       b                   export
     daddChpInfo       pi
     d inRrn#                         9  0
     d inFNRrn#                       9  0
     d inEdit#                        1    const
     d inMode#                        1    const

     d ExitWdw         s               n   inz(*off)
     d isErrWdw        s               n   inz
     d iOpen           s               n   inz

      /free

       Chain(n) inFNRrn# pfn0p;
       If not %Open(wpigrppop);
          Open wpigrppop;
          iOpen = *On;
       Endif;

       // Group Chapter Info
       qdjob  = q1job;
       qdcusr = q1cusr;
       qdjobn = q1jobn;
       ExitWdw = *off;
       scEdChpSav = *off;
       scEdChSvNx = *off;
       scEdChpCan = *off;

       Chain(n) (FNco#:FNfnd) pcp00;
       If not %found or inEdit# = *off;
          scChpEdHdg = 'Add Chapter Information';
          dspEdtChp  = *off;
          dspEdChSvN = *on;
          scEdtChp   = 0;
          scEdtChpSt = 'A';
          scEdtChpDs = ' ';
       Else;
          Chain inRrn# pcp0p;
          scChpEdHdg = 'Edit Chapter Information';
          dspEdtChp  = *on;
          dspEdChSvN = *off;
          scEdtChp   = CPchap;
          scEdtChpSt = CPstat;
          scEdtChpDs = CPdesc;
       Endif;

       Dow ExitWdw = *off;
          Exfmt wdwChpEdt;
          isErrWdw = *off;
          ExitWdw  = *off;

          Select;
          When scEdChpCan = *on;
             scEdChpCan = *off;
             ExitWdw = *on;
          When scEdChpSav = *on;
             scEdChpSav = *off;
             Exsr ValidChp;
             If isErrWdw = *off;
                Exsr SaveChpInfo;
                ExitWdw = *on;
             Endif;
          When scEdChSvNx = *on;
             scEdChSvNx = *off;
             Exsr ValidChp;
             If isErrWdw = *off;
                Exsr SaveChpInfo;
                scEdtChp   = 0;
                scEdtChpSt = 'A';
                scEdtChpDs = ' ';
             Endif;
          Endsl;
       Enddo;

       If %Open(wpigrppop) And iOpen;
          Close wpigrppop;
       Endif;
       //*************************************************************************
       // ValidChp
       //*************************************************************************
       BegSr ValidChp;

       // Group Chapter Info
       isErrWdw = *off;
       erEdtChp = *off;
       erEdtChpDs = *off;

       // validate chapter
       If inEdit# = *off;
          If scEdtChp <= 0;
             mgEdtChp = 'WCR0502';
             erEdtChp = *on;
             isErrWdw = *on;
          Else;
             Chain(n) (FNco#:FNfnd:scEdtChp) pcp00;
             If %found;
                mgEdtChp = 'WC30005';
                erEdtChp = *on;
                isErrWdw = *on;
             Endif;
          Endif;
       Endif;

       // validate chapter description
       If scEdtChpDs = ' ';
          mgEdtChpDs = 'WCR0511';
          erEdtChpDs = *on;
          isErrWdw   = *on;
       Endif;

       Endsr;
       //*************************************************************************
       // SaveChpInfo
       //*************************************************************************
       BegSr SaveChpInfo;

       Chain (FNco#:FNfnd:scEdtChp) pcp00;

       CPstat = scEdtChpSt;
       CPdesc = scEdtChpDs;
       CPpgm  = Q1pgm;
       CPuser = Q1user;
       CPtime = Timeto6(SystemTime());
       CPdt   = Dateto7(SystemDate());

       If %found;
          update pcp00;
       Else;
          CPchap = scEdtChp;
          CPco#  = FNco#;
          CPfnd  = FNfnd;
          Write pcp00;
       Endif;
       unlock WMCPL;

       Endsr;

      /end-free
     paddChpInfo       e
      *=====================================================================
      * Add Region Info
      *=====================================================================
     paddRegInfo       b                   export
     daddRegInfo       pi
     d inRrn#                         9  0
     d inFNRrn#                       9  0
     d inEdit#                        1    const
     d inMode#                        1    const

     d ExitWdw         s               n   inz(*off)
     d isErrWdw        s               n   inz
     d iOpen           s               n   inz

      /free

       Chain(n) inFNRrn# pfn0p;
       If not %Open(wpigrppop);
          Open wpigrppop;
          iOpen = *On;
       Endif;

       // Group Region Info
       qdjob  = q1job;
       qdcusr = q1cusr;
       qdjobn = q1jobn;
       ExitWdw = *off;
       scEdtRgSav = *off;
       scEdtRgCan = *off;
       scEdRgSvNx = *off;

       Chain(n) (FNco#:FNfnd) pre00;
       If not %found or inEdit# = *off;
          scRegEdHdg = 'Add Region Information';
          dspEdRegn  = *off;
          dspEdRgSvN = *on;
          scEdtRegn  = 0;
          scEdtRegSt = 'A';
          scEdtDesc  = ' ';
       Else;
          Chain inRrn# pre0p;
          scRegEdHdg = 'Edit Region Information';
          dspEdRegn  = *on;
          dspEdRgSvN = *off;
          scEdtRegn  = RErgn#;
          scEdtRegSt = REstat;
          scEdtDesc  = REdesc;
       Endif;

       Dow ExitWdw = *off;
          If isFirst = *off and inMode# = 'A';
             Write rcdAddInf;
          Endif;
          Exfmt wdwRegEdt;
          isErrWdw = *off;
          ExitWdw = *off;

          Select;
          When scEdtRgCan = *on;
             scEdtRgCan = *off;
             ExitWdw = *on;
          When scEdtRgSav = *on;
             scEdtRgSav = *off;
             Exsr ValidReg;
             If isErrWdw = *off;
                Exsr SaveRegInfo;
                ExitWdw = *on;
             Endif;
          When scEdRgSvNx = *on;
             scEdRgSvNx = *off;
             Exsr ValidReg;
             If isErrWdw = *off;
                Exsr SaveRegInfo;
                scEdtRegn  = 0;
                scEdtRegSt = 'A';
                scEdtDesc  = ' ';
             Endif;
          Endsl;
       Enddo;

       If %Open(wpigrppop) And iOpen;
          Close wpigrppop;
       Endif;
       //*************************************************************************
       // ValidReg
       //*************************************************************************
       BegSr ValidReg;

       isErrWdw = *off;
       erEdtRegn = *off;
       erEdtDesc = *off;

       // validate region#
       If inEdit# = *off;
          If scEdtRegn <= 0;
             mgEdtRegn = 'WCR0502';
             erEdtRegn  = *on;
             isErrWdw   = *on;
          Else;
             Chain(n) (Fnco#:FNfnd:scEdtRegn) pre00;
             If %found;
                mgEdtRegn = 'WC30005';
                erEdtRegn  = *on;
                isErrWdw   = *on;
             Endif;
          Endif;
       Endif;

       // validate region description
       If scEdtDesc = ' ';
          mgEdtDesc = 'WCR0511';
          erEdtDesc  = *on;
          isErrWdw   = *on;
       Endif;

       Endsr;
       //*************************************************************************
       // SaveRegInfo
       //*************************************************************************
       BegSr SaveRegInfo;

       Chain (Fnco#:FNfnd:scEdtRegn) pre00;

       REstat = scEdtRegSt;
       REdesc = scEdtDesc;
       REpgm  = Q1pgm;
       REuser = Q1user;
       REtime = Timeto6(SystemTime());
       REdt   = Dateto7(SystemDate());

       If %found;
          update pre00;
       Else;
          REco#  = FNco#;
          REfnd  = FNfnd;
          RErgn# = scEdtRegn;
          REipgm = Q1pgm;
          REiusr = Q1user;
          REitm  = Timeto6(SystemTime());
          REidt  = Dateto7(SystemDate());
          Write pre00;
       Endif;
       unlock WMREL;

       Endsr;
      /end-free
     paddRegInfo       e
      *=====================================================================
      * procAddBnk - Process Bank Addition
      *  Parm1   - Group RRN
      *  Parm2   - Mode(1A) 'A'-Add, 'C'-Change, 'D'-Inquiry
      *  Returns - success indicator
      *=====================================================================
     pprocAddBnk       b                   export
     dprocAddBnk       pi              n
     d inFNrrn                        9  0
     d inMode                         1

     d isEdit          s               n   inz
     d isRepeat        s               n   inz
     d iOpen           s               n   inz
     d ExitNow         s               n   inz(*off)
     d xxRtcd          s               n   inz

     c     keyWMFNL      klist
     c                   kfld                    FNco#
     c                   kfld                    FNfnd

     c     keyWSCKL      klist
     c                   kfld                    FNco#
     c                   kfld                    ##acct           20

      /free
       Chain(n) inFNRrn pfn0p;
       If not %Open(wpigrppop);
          Open wpigrppop;
          iOpen = *On;
       Endif;

       GetClientId(ClientID);
       xxRtcd = *Off;
       qdjob  = q1job;
       qdcusr = q1cusr;
       qdjobn = q1jobn;
       If inMode <> 'A' and inMode <> 'C' and inMode <> 'D';
          Return xxRtcd;
       Endif;

       scGrpWdLft = 260;
       scAddWdTop = 75;
       dspLnBnEdt = *on;
       dspDtlBtn  = *on;
       scShowWdw  = *on;
       scDtlPrv   = *off;
       scDtlFns   = *off;
       isEdit = *off;

       Dow ExitNow = *off;
          Exsr LoadBnkInfo;
          If isFstBnk = *off;
             isFstBnk = *on;
          Endif;

          ExFmt rcdFinCnt;

          Select;
          When scDtlPrv = *on;
             scDtlPrv = *off;
             ExitNow = *on;
             xxRtcd = *off;
          When scDtlFns = *on;
             scDtlFns = *off;
             ExitNow = *on;
             xxRtcd = *on;
          When scBtnBnAdd = *on;
             scBtnBnAdd = *off;
             outRrn = 0;
             isEdit = *off;
             addBnkInfo(outRrn:inFnrrn:isEdit:inMode:isRepeat);
          Other;
             ReadC sflBnkInfo;
             isEdit = *on;
             If scLnkBnEdt = *on;
                scLnkBnEdt = *off;
                outRrn = sflBnkRrn;
                addBnkInfo(outRrn:inFnrrn:isEdit:inMode:isRepeat);
             Endif;
          EndSl;
       EndDo;

       If %Open(wpigrppop) And iOpen;
          Close wpigrppop;
       Endif;
       Return xxRtcd;
       //*************************************************************************
       // LoadBnkInfo
       //*************************************************************************
       BegSr LoadBnkInfo;

       // Group Bank Info
       dspBnkInfo = *off;
       clrBnkInfo = *on;
       rrnBnkInfo = 0;
       Write rcdFinCnt;

       dspBnkInfo = *on;
       clrBnkInfo = *off;
       dspLnBnEdt = *on;

       Setll keyWMFNL pbi00;
       If not %equal and isFstBnk= *off and isEdit = *off;
          isRepeat = *off;
          Dow isRepeat = *off;
             isRepeat = *on;
             outRrn = 0;
             addBnkInfo(outRrn:inFnrrn:isEdit:inMode:isRepeat);
          Enddo;
          Setll keyWMFNL pbi00;
       Endif;
       Reade keyWMFNL pbi00;

       Dow not %eof;
          scFnAppTyp = BIappl;
          scBnkCode  = BIcktp;
          scBnkEffDt = CvtToDate(BIefdt);
          scBnkAct   = BIacct;
          ##acct     = BIacct;
          Chain keyWSCKL pck00;
          scBnkName = %trim(CKai01);
          If BIstat = 'A';
             scBnkSts = 'Active';
          Else;
             scBnkSts = 'Inactive';
          Endif;
          sflBnkRrn = dbrrn3;
          rrnBnkInfo = rrnBnkInfo + 1;
          If rrnBnkInfo = 1;
             scBnkRow = 'A';
          Endif;

          Write sflBnkInfo;
          Reade keyWMFNL pbi00;
       Enddo;
       rrnBnkInfo = 1;

       Endsr;

      /end-free
     pprocAddBnk       e
      *=====================================================================
      * addBnkInfo - Add/Edit Bank Information Record
      *=====================================================================
     paddBnkInfo       b                   export
     daddBnkInfo       pi
     d inRrn#                         9  0
     d inFNrrn#                       9  0
     d inEdit#                        1    const
     d inMode#                        1    const
     d inRepeat                        n

      * work variables
     d intDate         s              7  0
     d intDate1        s              7  0
     d isCancel        s               n   inz
     d isErrWdw        s               n   inz
     d iOpen           s               n   inz
     d exitWdw         s               n   inz(*off)
     d exitMICR        s               n   inz(*off)
     d outApp          s              2
     d outFyr          s              3
     d outSrDt         s              8
     d outAcct         s             20
     d OutCall         s              1
     d OutHdcy         s              1
     d outChkq         s             10
     d outFtyp         s             10
     d outRtcd         s              1
     d outDt           s              7
     d outCpyq         s             10
     d outCktp         s             10
     d outEftc         s              1
     d outCoid         s             20
     d outCase         s              7
     d outData1        s             75
     d svDtlLogo       s                   like(scDtlLogo)
     d svEftc          s                   like(BIeftc)
     d xxKey           s              2

     c     keyWMFNL      klist
     c                   kfld                    FNco#
     c                   kfld                    FNfnd

     c     keyWMBIL2     klist
     c                   kfld                    FNco#
     c                   kfld                    ##app             2
     c                   kfld                    FNfnd
     c                   kfld                    scDtlActCd
     c                   kfld                    ##date            7 0

     c     keyWSCKL      klist
     c                   kfld                    FNco#
     c                   kfld                    ##acct           20

     c     keyWMBIL6     klist
     c                   kfld                    FNco#
     c                   kfld                    ##app             2
     c                   kfld                    FNfnd
     c                   kfld                    scDtlActCd
     c                   kfld                    scDtlBnkAc
     c                   kfld                    ##date            7 0

      /free

       Chain(n) inFNRrn# pfn0p;
       If not %Open(wpigrppop);
          Open wpigrppop;
          iOpen = *On;
       Endif;

       GetClientId(ClientID);
       qdjob  = q1job;
       qdcusr = q1cusr;
       qdjobn = q1jobn;

       // Group Bank Info
       ExitWdw = *off;
       scDtlBkSav = *off;
       scDtlBkCan = *off;
       scDtBkSvNx = *off;
       clear isLogo;
       clear wdBnkInfo;

       Chain keyWMFNL pbi00;
       If not %found or inEdit# = *off;
          clear *all pbi00;
          scDtlBkHdg = 'Add Bank Information';
          dsbBnkEdt  = *off;
          dspDtlBtnN = *on;
          scDtlChLn1 = FNname;
          scDtlChLn2 = FNmad1;

          scDtlChLn4 = %trim(FNmcty) + ', ' + %trim(FNmst) + '  ' +
          %subst(%editc(FNmzip:'X'):1:5) + '-' + %subst(%editc(FNmzip:'X'):6:4);
          If FNmad2 <> ' ';
             scDtlChLn3 = FNmad2;
          Else;
             scDtlChLn3 = scDtlChLn4;
             scDtlChLn4 = ' ';
          Endif;
       Else;
          Chain inRrn# pbi0p;
          scDtlBkHdg = 'Edit Bank Information';
          dsbBnkEdt  = *on;
          dspDtlBtnN = *off;
          scDtlChLn1 = BI40a1;
          scDtlChLn2 = BI40a2;
          scDtlChLn3 = BI40a3;
          scDtlChLn4 = BI40a4;
       Endif;

       scDtlAppTy = BIappl;
       scDtlActCd = BIcktp;
       scDtlEfDt  = dtMDYY(BIefdt);
       scDtlEFT   = BIeftc;
       svEftc     = BIeftc;
       scDtlBnkAc = BIacct;
       ##acct     = BIacct;
       xxKey = 'WC';
       Chain xxKey swc01;
       Chain keyWSCKL pck00;
       If %found(WSCKL);
          scDtlBnkNm = %trim(CKai01);
          scDtlBkAd1 = %trim(CKai02);
          scDtlBkCty = %trim(CKai03);
          scDtlVchOn = CKvchr;
          If CKvchr = ' ';
             scDtlVchOn = 'N';
          Endif;
       Else;
          clear pck00;
       Endif;
       scDtlSts   =  BIstat;
       scDtlPrtCh =  BIhdcy;
       If BIhdcy = ' ';
          scDtlPrtCh = 'N';
       Endif;
       scDtlTrst  =  BItrst;
       If BItrst = ' ';
          scDtlTrst  = 'N';
       Endif;
       scDtlFrmTy = BIftyp;
       scDtlOutQ  = BIoutq;
       scDtlLmSgn = BI$lmt;
       scDtlCpyQu = BIcpyq;
       scDtlMcrId = BImacr;
       scDtlVdDys = BI50n3;
       If isLogo = ' ';
          scDtlLogo = BIlogo;
          svDtlLogo = BIlogo;
          If BIlogo = ' ';
             scDtlLogo = 'N';
             svDtlLogo = 'N';
          Endif;
          isLogo = 'Y';
       Endif;

       // Control the message displayed on screen.
       If scDtlLogo <> scDtlLogo or isLogo = 'Y';
          dspDtlLogo = *off;
          dspDtlLog  = *off;
          If scDtlLogo = 'N';
             dspDtlLogo = *on;
             dspDtlLog  = *on;
             scDtlLnDsc = 'The Next Four Lines Will Print In The Upper Left +
              Area Of The Check.';
          Endif;
          If scDtlLogo = 'Y';
             dspDtlLog  = *on;
             scDtlLnDsc = 'This Bank Account Number Will Have A Logo In The +
              Upper Left Area Of The Check.';
          Endif;
          svDtlLogo = scDtlLogo;
          isLogo = 'N';
       Endif;

       Dow ExitWdw = *off;
          If isFstBnk= *off and inMode# = 'A';
             Write rcdFinCnt;
          Endif;

          //temporary line
          dspDtlBtnN = *off;
          scCo# = FNco#;
          Exfmt wdBnkInfo;

          isErrWdw = *off;
          ExitWdw = *off;
          isCancel = *off;

          Select;
          When scDtlBkCan = *on;
             scDtlBkCan = *off;
             ExitWdw = *on;
          When scDtlBkSav = *on;
             scDtlBkSav = *off;
             Exsr ValidBnk;
             If isErrWdw = *off;
                Exsr SaveBnkInfo;
                If isCancel = *off;
                   ExitWdw = *on;
                Endif;
             Endif;
          When scDtBkSvNx = *on;
             scDtBkSvNx = *off;
             Exsr ValidBnk;
             If isErrWdw = *off;
                Exsr SaveBnkInfo;
                inRepeat = *off;
                If isCancel = *off;
                   ExitWdw = *on;
                Endif;
             Endif;
          Endsl;
       Enddo;

       If %Open(wpigrppop) And iOpen;
          Close wpigrppop;
       Endif;

       //*************************************************************************
       // ValidBnk
       //*************************************************************************
       BegSr ValidBnk;

       isErrWdw = *off;
       erDtlActCd = *off;
       erDtlBnkAc = *off;
       erDtlEfDt  = *off;
       erDtlEfDt1 = *off;
       erDtlAppTy = *off;
       erDtlMcrId = *off;
       erDtlCpyQu = *off;
       erDtlOutQ  = *off;
       erDtlEFT   = *off;

       xxKey = 'WC';
       Chain xxKey swc01;

       // Voucher Flag
       ##acct = scDtlBnkAc;
       Chain keyWSCKL pck00;

       // Add mode
       If inEdit# = *off;
          // The Account Number cannot be Blanks  (--Add-- Mode only)
          If scDtlBnkAc = ' ';
             isErrWdw = *on;
             mgDtlBnkAc = 'WCR0503';
             erDtlBnkAc = *on;
          Endif;

          // If Group Aux record indicates that it is a voucher group,
          // then the Bank Acct record must be a voucher type.
          If isErrWdw = *off and CKvchr <> 'Y';
             Chain (FNco#:FNfnd) pf200;
             If %found and F2ctyp = 'V';
                isErrWdw = *on;
                mgDtlBnkAc = 'WC30085';
                erDtlBnkAc = *on;
             Endif;
          Endif;

          // validate effective date
          If scDtlEfDt = 0;
             isErrWdw = *On;
             erDtlEfDt = *On;
             mgDtlEfDt = 'WCR0503';
          Else;
             intDate = DtCYMD(scDtlEfDt);
             If intDate = 0;
                isErrWdw = *On;
                erDtlEfDt = *On;
                mgDtlEfDt = 'WCR0500';
             Endif;
          Endif;

          // Make sure EFT info is setup in bank master before allowing
          // an EFT bank code.
          If erDtlBnkAc <> *on and scDtlActCd = 'EFT';
             isErrWdw = *on;
             erDtlActCd = *on;
             mgDtlActCd = 'W400064';
          EndIf;

          // Validate the bank code by procedure call ValidBankCode.
          outCo   = %editc(FNco#:'X');
          outFnd# = %editc(FNfnd:'X');
          outFyr  = '000';
          outCase = *zeros;
          outApp  = scDtlAppTy;
          outData1 = *blanks;
          outCktp = scDtlActCd;

          If erDtlActCd <> *on;
             If Not ValidBankCode(outCo:outFnd#:outFyr:outCase:outApp:outData1:
              outCktp);
                isErrWdw = *on;
                erDtlActCd = *on;
                mgDtlActCd = 'WCR0505';
             EndIf;
          EndIf;

          // validate application type
          If scDtlAppTy = ' ';
             isErrWdw = *on;
             erDtlAppTy = *on;
             mgDtlAppTy = 'WCR0503';
          Else;
             // Check Application Flags
             If (%trim(scDtlAppTy) = 'WP' and WCfl24 <> 'Y') or
              (%trim(scDtlAppTy) = 'RP' and WCfl25 <> 'Y') or
              (%trim(scDtlAppTy) = 'IC' and WCfl36 <> 'Y');
                isErrWdw = *on;
                erDtlAppTy = *on;
                mgDtlAppTy = 'WC30114';
             EndIf;
          EndIf;

          If (scDtlEFT = *blanks) And
           (CKbnk# = *blanks or CKtrns = *blanks Or
            CKonus = *blanks or CKrtup = *blanks Or
            CKrtlw = *blanks);
             isErrWdw = *on;
             erDtlEFT = *on;
             mgDtlEFT = 'W400066';
          EndIf;

          If (scDtlEFT = 'E' or scDtlEFT = 'B') And
           (CKidsa = *zero or CKiora = *zero);
             isErrWdw = *on;
             erDtlEFT = *on;
             mgDtlEFT = 'W400067';
          EndIf;

          // The CO#/ACCT/EFDT/FND can NOT already exist in Bank Info file
          ##date = DtCYMD(scDtlEfDt);
          ##app  = %trim(scDtlAppTy);
          Setll keyWMBIL6 pbi06;
          Reade keyWMBIL6 pbi06;
          Dow not %eof(WMBIL6);
             Select;
             When scDtlEFT = *blanks and BIeftc = 'E';
             When scDtlEFT = 'E' and BIeftc = *blanks;
             Other;
                isErrWdw = *on;
                mgDtlBnkAc = 'WC30104';
                erDtlBnkAc = *on;
                erDtlEfDt = *On;
                mgDtlEfDt = 'WC30104';
                Leave;
             EndSl;
             Reade keyWMBIL6 pbi06;
          EndDo;

          // If screen Effective Date LE Today, then determine the "Current"
          // Bank Acct. based on CKVCHR value using Today as Search Date.
          // If a "Current" Bank Acct. exists, then display Error msg.
          intDate1 = Dateto7(SystemDate());
          If erDtlEfDt <> *on and intDate < intDate1;
             Select;
             When CKvchr = 'Y';
                outCall = 'V';
             When CKvchr = 'N';
                outCall = 'C';
             Other;
                outCall = CKvchr;
             Endsl;

             outSrDt = %editc(dtMDYY(Dateto7(SystemDate())):'X');

             // Call API to access "Current" Bank Account based on ##CALL value
             outAcct = *Blanks;
             outHdcy = *Blanks;
             outChkq = *Blanks;
             outFtyp = *Blanks;
             outRtcd = *Blanks;
             outDt   = *Zeros ;
             outCpyq = *Blanks;
             outCktp = scDtlActCd;
             outEftc = scDtlEFT;

             outAcct= GetBankAcc(outCo:outFnd#:outApp:outSrDt:outCall:outHdcy:
              outChkq:outFtyp:outDt:outCpyq:outCktp:outEftc:outCoid:outRtcd);

             If outRtcd = ' ';
                // Bank Info found for Voucher/Check, with effective date prior to today.
                isErrWdw = *on;
                erDtlEfDt = *On;
                mgDtlEfDt = 'WC30102';
             Endif;
          Endif;

          // Ensure that the effective date is not past the bank master exp date
          If erDtlEfDt <> *on and intDate >= CKexdt and CKexdt > 0;
             isErrWdw = *on;
             erDtlEfDt1 = *On;
             msDtlEfDt = 'Bank Account Is Expired As Of This Date.';
          Endif;

          If %trim(scDtlAppTy) = 'RP';
             If CKrpcd = ' ' or CKrpvs = ' ';
                isErrWdw = *on;
                erDtlAppTy = *on;
                mgDtlAppTy = 'WC40103';
             Endif;
          Endif;
       Endif;

       // If Change mode
       If inEdit# = *on;
          If svEftc = 'E' and scDtlEFT = *blanks;
             isErrWdw = *On;
             erDtlEFT = *On;
             mgDtlEFT = 'W400068';
          Endif;

          If erDtlEFT <> *on and svEftc = 'B' and scDtlEFT = *blanks;
             isErrWdw = *On;
             erDtlEFT = *On;
             mgDtlEFT = 'W400069';
          Endif;

          If erDtlEFT <> *on and scDtlEFT = 'B' and
           (CKbnk# = *blanks or CKtrns = *blanks Or
           CKonus = *blanks or CKrtup = *blanks Or
           CKrtlw = *blanks);
             isErrWdw = *On;
             erDtlEFT = *On;
             mgDtlEFT = 'W400066';
          EndIf;

          If erDtlEFT <> *on and (scDtlEFT = 'B' or scDtlEFT = 'E') and
           (CKidsa = *zero or CKiora = *zero);
             isErrWdw = *On;
             erDtlEFT = *On;
             mgDtlEFT = 'W400067';
          EndIf;
       EndIf;

       // If CHANGE mode or ADD mode
       // Macro ID cannot be ZERO
       If scDtlMcrId = 0 and scDtlPrtCh = 'Y';
          isErrWdw = *On;
          erDtlMcrId = *On;
          mgDtlMcrId = 'WCR0503';
       EndIf;

       // Check OUTQ cannot be BLANK
       If scDtlOutQ = ' ' and scDtlPrtCh = 'Y';
          isErrWdw = *On;
          erDtlOutQ = *On;
          mgDtlOutQ = 'WCR0503';
       EndIf;

       // File Copy OUTQ cannot be BLANK
       If scDtlCpyQu = ' ' and scDtlPrtCh = 'Y';
          isErrWdw = *On;
          erDtlCpyQu = *On;
          mgDtlCpyQu = 'WCR0503';
       EndIf;

       Endsr;
       //*************************************************************************
       // SaveBnkInfo
       //*************************************************************************
       BegSr SaveBnkInfo;

       ##date = dtCYMD(scDtlEfDt);
       ##app  = %trim(scDtlAppTy);
       Chain keyWMBIL2 pbi02;

       BI40a1 = scDtlChLn1;
       BI40a2 = scDtlChLn2;
       BI40a3 = scDtlChLn3;
       BI40a4 = scDtlChLn4;
       BIappl = scDtlAppTy;
       BIcktp = scDtlActCd;
       BIefdt = ##date;
       BIeftc = scDtlEFT;
       BIacct = scDtlBnkAc;
       BIstat = scDtlSts;
       BIhdcy = scDtlPrtCh;
       BItrst = scDtlTrst;
       BIftyp = scDtlFrmTy;
       BIoutq = scDtlOutQ;
       BI$lmt = scDtlLmSgn;
       BIcpyq = scDtlCpyQu;
       BImacr = scDtlMcrId;
       BI50n3 = scDtlVdDys;
       BIlogo = scDtlLogo;
       Chain (FNco#:BIacct) pck00;
       BIai01 = CKai01;
       BIai02 = CKai02;
       BIai03 = CKai03;

       // process MICR window
       Exsr procMICRWdw;
       If isCancel = *on;
          unlock WMBIL2;
          leavesr;
       Endif;

       BIpgm  = Q1pgm;
       BIuser = Q1user;
       BItime = Timeto6(SystemTime());
       BIdt   = Dateto7(SystemDate());

       If %found(WMBIL2);
          update pbi02;
       Else;
          BIco#  = FNco#;
          BIfnd  = FNfnd;
          BIstat = 'A';
          Write pbi02;
       Endif;
       unlock WMBIL2;

       Endsr;

       //*************************************************************************
       // procMICRWdw
       //*************************************************************************
       BegSr procMICRWdw;

       // Group Bank Info
       ExitMICR = *off;
       scDtlMCCan = *off;
       scDtlMCSav = *off;
       clear wdMICRInfo;
       scDtlBkHg1 = scDtlBkHdg;

       dspEFTInfo = *off;
       scDtlClmCh = BIckpv;
       scDtlEFTDb = BIdcid;
       scDtlEFTCr = BIccid;
       ##acct     = BIacct;
       Chain keyWSCKL pck00;
       If CKvchr <> 'Y';
          //dspMICRInf = *on;

          xxKey = 'WC';
          Chain xxKey swc01;
          If ClientID ='025' or WCfl17 = 'Y' or WCfl24 = 'Y' or WCfl35 = 'Y';
             dspEFTInfo = *on;
          Endif;

          scDtlRtMCR = BItrns;
          scDtlTrnNm = BIbnk#;
          scDtlChkDg = BIckdg;
          scDtlAuxUS = BIaxou;
          scDtlONUS  = BIonus;
          scDtlRtEUp = BIrtup;
          scDtlRtELw = BIrtlw;
          scDtlFstSg = BInam1;
          scDtlSndSg = BInam2;

          If not %found(WMBIL2);
             scDtlRtMCR = CKtrns;
             scDtlTrnNm = CKbnk#;
             scDtlChkDg = %editc(CKckdg:'X');
             scDtlAuxUS = CKaxou;
             scDtlONUS  = CKonus;
             scDtlRtEUp = CKrtup;
             scDtlRtELw = CKrtlw;
             scDtlFstSg = CKnam1;
             scDtlSndSg = CKnam2;
          Endif;

          Dow ExitMICR = *off;
             Exfmt wdMICRInfo;

             isErrWdw = *off;
             ExitMICR = *off;

             Select;
             When scDtlMCCan = *on;
                scDtlMCCan = *off;
                //If scDtlSndSg = ' ' and scDtlSndSg = ' ';
                //   Exsr SaveMICR;
                //Endif;
                ExitMICR = *on;
                isCancel = *on;
             When scDtlMCSav = *on;
                scDtlMCSav = *off;
                Exsr ValidMICR;
                If isErrWdw = *off;
                   Exsr SaveMICR;
                   ExitMICR = *on;
                Endif;
             Endsl;
          Enddo;
       Endif;

       Endsr;
       //*************************************************************************
       // ValidMICR
       //*************************************************************************
       BegSr ValidMICR;

       isErrWdw = *off;
       erDtlSndSg = *off;
       erDtlFstSg = *off;

       If not ((scDtlEFT = 'E') or (svEftc = 'E' and scDtlEFT = 'B'));
          // Signature 1 must exist in Check Signature file
          If scDtlFstSg <> ' ';
             Chain (FNco#:scDtlFstSg) psg01;
             If not %found;
                isErrWdw = *On;
                erDtlFstSg = *On;
                mgDtlFstSg = 'WCR0505';
             EndIf;
          EndIf;

          // Signature 2 must exist in Check Signature file
          If scDtlSndSg <> ' ';
             Chain (FNco#:scDtlSndSg) psg01;
             If not %found;
                isErrWdw = *On;
                erDtlSndSg = *On;
                mgDtlSndSg = 'WCR0505';
             EndIf;
          EndIf;
       EndIf;

       Endsr;
       //*************************************************************************
       // SaveMICR
       //*************************************************************************
       BegSr SaveMICR;

       BIckpv = scDtlClmCh;
       BIdcid = scDtlEFTDb;
       BIccid = scDtlEFTCr;
       BItrns = scDtlRtMCR;
       BIbnk# = scDtlTrnNm;
       BIckdg = scDtlChkDg;
       BIaxou = scDtlAuxUS;
       BIonus = scDtlONUS;
       BIrtup = scDtlRtEUp;
       BIrtlw = scDtlRtELw;
       If scDtlFstSg = ' ';
          BI10a1 = ' ';
          BInam1 = ' ';
          BIcmd1 = ' ';
       Else;
          BInam1 = scDtlFstSg;
          Chain (FNco#:scDtlFstSg) psg01;
          If %found;
             BI10a1 = SGfont;
             BIcmd1 = SGcmd;
          Endif;
       Endif;
       If scDtlSndSg = ' ';
          BI10a2 = ' ';
          BInam2 = ' ';
          BIcmd2 = ' ';
       Else;
          BInam2 = scDtlSndSg;
          Chain (FNco#:scDtlSndSg) psg01;
          If %found;
             BI10a2 = SGfont;
             BIcmd2 = SGcmd;
          Endif;
       Endif;

       Endsr;

      /end-free
     paddBnkInfo       e
      *=====================================================================
      * AddPolYear - process policy year add/edit
      *  Parm1   - Group/Policy Year RRN
      *  Parm2   - Mode(1A) 'A'-Add, 'C'-Change, 'D'-Inquiry
      *  Returns - success indicator
      *=====================================================================
     pAddPolYear       b                   export
     dAddPolYear       pi              n
     d inFNrrn                        9  0
     d inMode                         1

     d ExitNow         s               n   inz(*off)
     d iOpen           s               n   inz
     d isError         s               n   inz
     d isAdmOnly       s               n   inz
     d isClmOnly       s               n   inz
     d isRepeat        s               n   inz
     d intDate         s              7  0
     d intFrDt         s              7  0
     d intToDt         s              7  0
     d intFyr          s              3  0
     d xxKey           s              2
     d xxRtcd          s               n   inz

      /free
       If not %Open(wpigrppop);
          Open wpigrppop;
          iOpen = *On;
       Endif;

       GetClientId(ClientID);
       xxRtcd = *Off;
       qdjob  = q1job;
       qdcusr = q1cusr;
       qdjobn = q1jobn;

       xxKey = 'WC';
       Chain xxKey swc01;
       If WCFL08 = 'A';
          isAdmOnly = *on;
       Endif;
       If WCFL08 = 'C';
          isClmOnly = *on;
       Endif;

       // Exit if not in Add/Edit or display mode
       If inMode <> 'A' and inMode <> 'C' and inMode <> 'D';
          Return xxRtcd;
       Endif;

       clear wdwPolAdd;
       Dow isRepeat = *off;
          isRepeat = *on;
          ExitNow = *off;
          Exsr procPolYr;
       Enddo;

       If %Open(wpigrppop) And iOpen;
          Close wpigrppop;
       Endif;

       Return xxRtcd;
       //*************************************************************************
       // procPolYr
       //*************************************************************************
       BegSr procPolYr;

       scDtPlSvNx = *off;
       scDtlPlSav = *off;
       scDtlPlCan = *off;
       intFyr = *hival;
       F1fyr = 0;
       dspSRDisCd = *off;
       dspColEfDt = *off;
       dspRetro   = *off;
       dspSRDep   = *off;
       dspBlExpCn = *off;
       dspNtPrcNY = *off;
       If WCFL44 = 'Y';
          dspColEfDt = *on;
       Endif;
       If ClientID = '046';
          dspSRDisCd = *on;
       Endif;
       If ClientID = '071';
          dspRetro = *on;
       Endif;
       If ClientID = '090';
          dspBlExpCn = *on;
       Endif;
       If ClientID = '129';
          dspNtPrcNY = *on;
       Endif;
       If ClientID = '109';
          dspSRdep = *on;
       Endif;

       // Add Mode
       If inMode = 'A';
          dspPolYear = *off;
          dsbPolYear = *off;
          dspSaveBtn = *on;
          scPolYrHdg  = 'Add Policy Year Information';
          Chain(n) inFNRrn pfn0p;
          intCo# = FNco#;
          intFnd = FNfnd;
          If %found;
             Setgt (FNco#:FNfnd:intFyr) pf100;
             Readpe (FNco#:FNfnd) pf100;
             If not %eof;
                F1Fyr = F1fyr + 1;
             Endif;
          Endif;
       Endif;

       // Change/Display Mode
       If inMode = 'C' or inMode = 'D';
          Chain(n) inFNRrn pf10p;
          intCo# = F1co#;
          intFnd = F1fnd;
          If inMode = 'C';
             scPolYrHdg  = 'Edit Policy Year Information';
             dspPolYear = *on;
             dspSaveBtn = *on;
             dsbPolYear = *off;
          Else;
             dspSaveBtn = *off;
             dspPolYear = *on;
             dsbPolYear = *on;
             scPolYrHdg  = 'Policy Year Information';
          Endif;
       Endif;

       Exsr LoadScreen;

       Dow ExitNow = *off;
          Exfmt wdwPolAdd;

          isError = *off;
          ExitNow = *off;

          Select;
          When scDtlPlCan = *on;
             scDtlPlCan = *off;
             ExitNow = *on;
          When scDtlPlSav = *on;
             scDtlPlSav = *off;
             Exsr ValidPolYr;
             If isError = *off;
                Exsr SavePolYr;
                ExitNow = *on;
             Endif;
          When scDtPlSvNx = *on;
             scDtPlSvNx = *off;
             Exsr ValidPolYr;
             If isError = *off;
                Exsr SavePolYr;
                isRepeat = *off;
                ExitNow = *on;
             Endif;
          Endsl;
       Enddo;
       Endsr;
       //*************************************************************************
       // LoadScreen
       //*************************************************************************
       BegSr LoadScreen;

          scPolYear = F1fyr;
          scPolStrDt = dtMDYY(F1stpo);
          scPolEndDt = dtMDYY(F1enpo);
          If inMode = 'A' and scPolStrDt > 0;
             scPolStrDt = scPolStrDt + 1;
          Endif;
          If inMode = 'A' and scPolEndDt > 0;
             scPolEndDt = scPolEndDt + 1;
          Endif;

          scPolSlfSt = F1dps;
          scPolUpdAR = F1upar;
          If F1upar = ' ';
             scPolUpdAR = 'N';
          Endif;
          scPolFnlAd = F1faud;
          If F1faud = ' ';
             scPolFnlAd = 'N';
          Endif;
          scPolSptPy = F1spla;
          If F1spla = ' ';
             scPolSptPy = 'N';
          Endif;
          scPolClmEd = F1rlxc;
          scPolClmOn = F1clms;
          If F1clms = ' ';
             scPolClmOn = 'N';
          Endif;
          scPolAnPrm = F1apfd;
          If F1apfd = ' ';
             scPolAnPrm = 'N';
          Endif;
          scExpCntTr = F1cdet;
          sc1stAstTr = F11stt;
          sc2ndAstTr = F12ndt;
          sc3rdAstTr = F13rdt;
          sc4thAstTr = F14tht;
          sc5thAstTr = F15tht;
          sc6thAstTr = F16tht;
          sc7thAstTr = F17tht;
          scStTxTrns = F1txtx;
          scStTxDpTr = F1txpy;
          scSlfRptBl = F1srbb;
          scNwBsPmt  = F1nbpt;
          scPayTrns  = F1nbtr;
          scPrmTrns  = F1prtr;
          scDepsTrns = F1nbdt;
          scDepBlOfs = F1dbtr;
          scDepPyOfs = F1dptr;
          scDepPrmTr = F1pptr;
          scTxBilOfs = F1tftr;
          scTxPyOfs  = F1tqtr;
          scTxPrmTrn = F1tptr;
          scMemFees  = F1mftr;
          scSftyFees = F1satr;
          scStateTax = F1txtr;
          scFlt1stAs = F11atr;
          scFlt2ndAs = F12atr;
          scFlt3rdAs = F13atr;
          scFlt4thAs = F14atr;
          scFlt5thAs = F15atr;
          scFlt6thAs = F16atr;
          scFlt7thAs = F17atr;
          scPrmPaymt = F1fctr;
          scMbDueTrn = F1mfpt;
          scPolMbFAm = F1mfee;
          scProrate  = F1pmfl;
          If inMode = 'A' or F1pmfl = ' ';
             scProrate  = 'N';
          Endif;
          scAdjYrAud = F1yrad;
          If inMode = 'A' or F1yrad = ' ';
             scAdjYrAud = 'N';
          Endif;
          scAdjCnAud = F1cxad;
          If F1cxad = ' ';
             scAdjCnAud = 'N';
          Endif;
          scDedBlAud = F1idbd;
          If inMode = 'A';
             scDedBlAud = ' ';
          Endif;
          scSfFeeTrn = F1sftx;
          scAdjRnwBl = F1brfl;
          If F1brfl = ' ';
             scAdjRnwBl = 'N';
          Endif;
          scAdjRevBl = F1bvfl;
          If F1bvfl = ' ';
             scAdjRevBl = 'N';
          Endif;
          scThAmDbAd = F1dadj;
          scThAmCrAd = F1cadj;
          scDedTxBil = F1dtxb;
          scDedTxPd  = F1dtxp;
          scPolGenBl = F1netr;
          scPolEffDt = dtMDYY(F1netd);
          If inMode = 'A' and scPolStrDt > 0;
             scPolEffDt = scPolStrDt;
          Endif;
          scPolEmRet = F1emrt;
          scSRBilOfs = F1shtr;
          scSRPdOfs  = F1sitr;
          scSRDpPrTr = F1sptr;
          scSRTxBlOf = F1sftr;
          scSRTxPdOf = F1sqtr;
          scTxDpPrTr = F1srtr;
          scSRPrmOfs = F1spof;
          scTxPrmOfs = F1srpo;
          scCrDedbl  = F1axdd;
          scCrExpCnt = F1axec;
          scCrGenFee = F1axgf;
          scCrLateFe = F1axlf;
          scCrMbFees = F1axmf;
          scCrDepst  = F1axdp;
          scCrSftFee = F1axsf;
          scCrTax    = F1axtx;
          scCrTaxDep = F1axtd;
          scCr1stAsm = F1ax1t;
          scCr2ndAsm = F1ax2t;
          scCr3rdAsm = F1ax3t;
          scCr4thAsm = F1ax4t;
          scCr5thAsm = F1ax5t;
          scCr6thAsm = F1ax6t;
          scCr7thAsm = F1ax7t;
          scCrPrmTrn = F1axpt;
          scSRDscCd  = F1dscd;
          scOfRtCrd  = F1orcr;
          scPlRtCrAR = F1rccd;
          scPlRtDbAR = F1rdcd;
          scAdTrCrAR = F1tacd;
          scPolDepYr = F1dpym;
          scPolAcmDv = F1apfl;
          scPolAsnAd = F1aafl;
          scBlExCnst = F1exdv;
          scPolBlIns = F1blfl;
          scPrcRtDec = F1ny07;
          scPlClEfDt = dtMDYY(F1scdt);

       Endsr;
       //*************************************************************************
       // ValiPolYr
       //*************************************************************************
       BegSr ValidPolYr;

       isError = *off;
       erPolStrDt = *off;
       erPolEndDt = *off;
       erPolSlfSt = *off;
       erPolEffDt = *off;
       erProrate  = *off;
       erAdjYrAud = *off;
       erAdjCnAud = *off;
       erPlRtDbAR = *off;
       erOfRtCrd  = *off;
       erPlRtCrAR = *off;
       erPlClEfDt = *off;
       erThAmCrAd = *off;
       erThAmDbAd = *off;
       erPolMbFAm = *off;
       erPolYear  = *off;

       // Policy year must be greater than zero (If in Add Mode)
       If scPolYear <= 0;
          mgPolYear = 'WCR0502';
          erPolYear = *on;
          isError = *on;
       Endif;

       // Check to be sure policy year has not been used prevoiusly
       If inMode = 'A' and isError = *off;
          Chain (intco#:intFnd:scPolYear) pf100;
          If %found;
             mgPolYear = 'WC30004';
             erPolYear = *on;
             isError = *on;
          Endif;
       Endif;

       // Check the Start policy date
       intFrDt = DtCYMD(scPolStrDt);
       If intFrDt = 0;
          isError = *On;
          erPolStrDt = *On;
          mgPolStrDt = 'WCR0500';
       Endif;

       // Check the End policy date
       intToDt = DtCYMD(scPolEndDt);
       If intToDt = 0;
          isError = *On;
          erPolEndDt = *On;
          mgPolEndDt = 'WCR0500';
       Endif;

       // Dates must be ascending order
       If erPolStrDt <> *on and erPolEndDt <> *on and intFrDt > intToDt;
          mgPolStrDt = 'WCR0529';
          mgPolEndDt = 'WCR0529';
          erPolStrDt = *on;
          erPolEndDt = *on;
          isError = *on;
       Endif;

       // Policy Date range must not overlap date range for other policy year
       Setll (intCo#:intFnd) pf100;
       Reade (intCo#:intFnd) pf100;
       Dow not %eof;
          If scPolYear <> F1fyr and intFrDt < F1enpo and intToDt > F1stpo;
             mgPolStrDt = 'WC31414';
             mgPolEndDt = 'WC31414';
             erPolStrDt = *on;
             erPolEndDt = *on;
             isError = *on;
          Endif;
          Reade (intCo#:intFnd) pf100;
       Enddo;

       // Do not perform these edits if claims only is installed
       If WCfl08 <> 'C';
          // self-reporting deposit style must be 'S', 'M' or 'N'
          If scPolSlfSt <> 'S' and scPolSlfSt <> 'M' and scPolSlfSt <> 'N'
           and scPolSlfSt <> 'A' and isClmOnly = *off;
             mgPolSlfSt = 'WC30341';
             erPolSlfSt = *on;
             isError = *on;
          Endif;

          // Net rate effective date field must be valid and within fund dates
          intDate = DtCYMD(scPolEffDt);
          If scPolGenBl = 'Y' or scPolEffDt > 0;
             If intDate = 0;
                isError = *On;
                erPolEffDt = *On;
                mgPolEffDt = 'WCR0500';
             Else;
                If intDate < intFrDt or intDate > intToDt;
                   isError = *On;
                   erPolEffDt = *On;
                   mgPolEffDt = 'WCR1145';
                Endif;
             Endif;
          Endif;
       Endif;

       If scPolMbFAm = 0;
          If scProrate = 'Y';
             isError = *on;
             erProrate = *on;
             mgProrate = 'WCR1087';
          Endif;
          If scAdjYrAud = 'Y';
             isError = *on;
             erAdjYrAud = *on;
             mgAdjYrAud = 'WCR1087';
          Endif;
          If scAdjCnAud = 'Y';
             isError = *on;
             erAdjCnAud = *on;
             mgAdjCnAud = 'WCR1087';
          Endif;
       Endif;

       If scPolMbFAm < 0;
          isError = *on;
          erPolMbFAm = *on;
          mgPolMbFAm = 'WCR0547';
       Endif;

       // Debit threshold cannot be less than zero.
       If scThAmDbAd < 0;
          isError = *on;
          erThAmDbAd = *on;
          mgThAmDbAd = 'WCR0547';
       Endif;

       // Credit threshold cannot be less than zero.
       If scThAmCrAd < 0;
          isError = *on;
          erThAmCrAd = *on;
          mgThAmCrAd = 'WCR0547';
       Endif;

       // Validate stop collateral date
       If scPlClEfDt > 0;
          intDate = DtCYMD(scPlClEfDt);
          If intDate = 0;
             isError = *On;
             erPlClEfDt = *On;
             mgPlClEfDt = 'WCR0500';
          Endif;
       Endif;

       If ClientID = '071';
          // If retro credit flag = 'Y', R.C. A/R code must be entered
          If scOfRtCrd = 'Y' and scPlRtCrAR = ' ';
             isError = *on;
             erPlRtCrAR = *on;
             mgPlRtCrAR = 'WCR0503';
          Endif;

          // If retro credit flag = 'Y', R.D. A/R code must be entered
          If scOfRtCrd = 'Y' and scPlRtDbAR = ' ';
             isError = *on;
             erPlRtDbAR = *on;
             mgPlRtDbAR = 'WCR0503';
          Endif;

          // retro credit flag must = 'Y' if A/R code entered
          If scOfRtCrd = ' ' and (scPlRtCrAR <> ' ' or scPlRtDbAR <> ' ');
             isError = *on;
             erPlRtDbAR = *on;
             mgPlRtDbAR = 'WCR1083';
             erPlRtCrAR = *on;
             mgPlRtCrAR = 'WCR1083';
             erOfRtCrd  = *on;
             mgOfRtCrd  = 'WCR1083';
          Endif;
       Endif;

       Endsr;
       //*************************************************************************
       // SavePolYr
       //*************************************************************************
       BegSr SavePolYr;

       If inMode <> 'A';
          Chain inFNRrn pf10p;
       Endif;

       F1fyr  = scPolYear;
       F1stpo = dtCYMD(scPolStrDt);
       F1enpo = dtCYMD(scPolEndDt);
       F1dps  = scPolSlfSt;
       F1upar = scPolUpdAR;
       F1faud = scPolFnlAd;
       F1spla = scPolSptPy;
       F1rlxc = scPolClmEd;
       F1clms = scPolClmOn;
       F1apfd = scPolAnPrm;
       F1cdet = scExpCntTr;
       F11stt = sc1stAstTr;
       F12ndt = sc2ndAstTr;
       F13rdt = sc3rdAstTr;
       F14tht = sc4thAstTr;
       F15tht = sc5thAstTr;
       F16tht = sc6thAstTr;
       F17tht = sc7thAstTr;
       F1txtx = scStTxTrns;
       F1txpy = scStTxDpTr;
       F1srbb = scSlfRptBl;
       F1nbpt = scNwBsPmt;
       F1nbtr = scPayTrns;
       F1prtr = scPrmTrns;
       F1nbdt = scDepsTrns;
       F1dbtr = scDepBlOfs;
       F1dptr = scDepPyOfs;
       F1pptr = scDepPrmTr;
       F1tftr = scTxBilOfs;
       F1tqtr = scTxPyOfs;
       F1tptr = scTxPrmTrn;
       F1mftr = scMemFees;
       F1satr = scSftyFees;
       F1txtr = scStateTax;
       F11atr = scFlt1stAs;
       F12atr = scFlt2ndAs;
       F13atr = scFlt3rdAs;
       F14atr = scFlt4thAs;
       F15atr = scFlt5thAs;
       F16atr = scFlt6thAs;
       F17atr = scFlt7thAs;
       F1fctr = scPrmPaymt;
       F1mfpt = scMbDueTrn;
       F1mfee = scPolMbFAm;
       F1pmfl = scProrate;
       F1yrad = scAdjYrAud;
       F1cxad = scAdjCnAud;
       F1idbd = scDedBlAud;
       F1sftx = scSfFeeTrn;
       F1brfl = scAdjRnwBl;
       F1bvfl = scAdjRevBl;
       F1dadj = scThAmDbAd;
       F1cadj = scThAmCrAd;
       F1dtxb = scDedTxBil;
       F1dtxp = scDedTxPd;
       F1netr = scPolGenBl;
       F1netd = dtCYMD(scPolEffDt);
       F1emrt = scPolEmRet;
       F1shtr = scSRBilOfs;
       F1sitr = scSRPdOfs;
       F1sptr = scSRDpPrTr;
       F1sftr = scSRTxBlOf;
       F1sqtr = scSRTxPdOf;
       F1srtr = scTxDpPrTr;
       F1spof = scSRPrmOfs;
       F1srpo = scTxPrmOfs;
       F1axdd = scCrDedbl;
       F1axec = scCrExpCnt;
       F1axgf = scCrGenFee;
       F1axlf = scCrLateFe;
       F1axmf = scCrMbFees;
       F1axdp = scCrDepst;
       F1axsf = scCrSftFee;
       F1axtx = scCrTax;
       F1axtd = scCrTaxDep;
       F1ax1t = scCr1stAsm;
       F1ax2t = scCr2ndAsm;
       F1ax3t = scCr3rdAsm;
       F1ax4t = scCr4thAsm;
       F1ax5t = scCr5thAsm;
       F1ax6t = scCr6thAsm;
       F1ax7t = scCr7thAsm;
       F1axpt = scCrPrmTrn;
       F1dscd = scSRDscCd;
       F1orcr = scOfRtCrd;
       F1rccd = scPlRtCrAR;
       F1rdcd = scPlRtDbAR;
       F1tacd = scAdTrCrAR;
       F1dpym = scPolDepYr;
       F1apfl = scPolAcmDv;
       F1aafl = scPolAsnAd;
       F1exdv = scBlExCnst;
       F1blfl = scPolBlIns;
       F1ny07 = scPrcRtDec;
       F1scdt = dtCYMD(scPlClEfDt);
       F1pgm  = Q1pgm;
       F1user = Q1user;
       F1time = Timeto6(SystemTime());
       F1dt   = Dateto7(SystemDate());

       If inMode = 'A' and %found(WDF1P);
          update pf10p;
       Else;
          F1co#  = intco#;
          F1fnd  = intFnd;
          F1iusr = Q1user;
          F1ipgm = Q1pgm;
          F1idt  = Dateto7(SystemDate());
          F1itm  = Timeto6(SystemTime());
          Write pf10p;
       Endif;
       unlock WDF1P;

       Endsr;

      /end-free
     pAddPolYear       e
      //*** Begin Add *** 09/26/22 *******************************************
       //===================================================================
       // CopySCBRecord - Copy selected State Compensation Benfits
       //===================================================================
     p CopySCBRecord   b
     d CopySCBRecord   pi
     d inFromGroup                    3  0
     d inSkipRepl                     1    const

     d inToGroup       s              3  0
      /Free

       intCo# = userCo#(q1user);
       index = 1;
       Dow index <= 9999 and arrSCB(index) <> ' ';
           ReadC sflToGrp;
           Dow Not %Eof;
              If scSelToGrp = 'Y';
                 inToGroup = scToGrp;
                 nxChgToGrp = *On;
                 update sflToGrp;
                 Select;
                 //Replace when duplicates found in To Group
                 When inSkipRepl = 'R';
                    //Check if form exist in To Group
                    chain (intCo#:inToGroup:arrSCB(index).SCBState:
                           arrSCB(index).SCBEfdt) prb00;
                    If %Found(wdrbl1);
                       //Delete the existing endorsement form and write new form in To Group
                       Delete prb00;
                       Exsr WriteWdrbp;
                    Else;
                       Exsr WriteWdrbp;
                    Endif;
                 //Skip duplicate forms When duplicate found in To Group
                 When inSkipRepl = 'S';
                 //Check if form exist in To Group
                    chain(n) (intCo#:inToGroup:
                    arrSCB(index).SCBState: arrSCB(index).SCBEfdt) prb00;
                    If not %Found(wdrbl1);
                    //Write to To Group if form does not exist in To Group
                       Exsr WriteWdrbp;
                    Endif;
                 //No dupllicate forms found in To Group, Copy all SCB
                 When inSkipRepl = 'A';
                    Exsr WriteWdrbp;
                 Endsl;
              Endif;
              ReadC sflToGrp;
           Enddo;
          index += 1;
       Enddo;

       Begsr WriteWdrbp;
         //Write to To Group if form does not exist in To Group
         Chain(n) arrSCB(index).SCBRrn prb0p;
         If %Found(Wdrbp);
            RBfnd = inToGroup;
            RBpgm  = Q1pgm;
            RBuser = Q1user;
            RBtime = Timeto6(SystemTime());
            RBdt   = Dateto7(SystemDate());
            Write prb0p;
         Endif;
       Endsr;

      /End-Free
     p CopySCBRecord   e

       //===================================================================
       // CopySCBRRecord - Copy selected State compensation rate by benefit
       //===================================================================
     p CopySCBRRecord  b
     d CopySCBRRecord  pi
     d inFromGroup                    3  0
     d inSkipRepl                     1    const

     d inToGroup       s              3  0
      /Free

       intCo# = userCo#(q1user);
       index = 1;
       Dow index <= 9999 and arrSCBR(index) <> ' ';
           ReadC sflToGrp;
           Dow Not %Eof;
              If scSelToGrp = 'Y';
                 inToGroup = scToGrp;
                 nxChgToGrp = *On;
                 update sflToGrp;
                 Select;
                 //Replace when duplicates found in To Group
                 When inSkipRepl = 'R';
                    //Check if form exist in To Group
                    chain (intCo#:inToGroup:arrSCBR(index).SCBRState:
                           arrSCBR(index).SCBRbr2:
                           arrSCBR(index).SCBREfdt) pr100;
                    If %Found(wdr1l);
                       //Delete the existing SCBR form and write new form in To Group
                       Delete pr100;
                       Exsr WriteWdr1p;
                    Else;
                       Exsr WriteWdr1p;
                    Endif;
                 //Skip duplicate forms When duplicate found in To Group
                 When inSkipRepl = 'S';
                 //Check if form exist in To Group
                    chain(n) (intCo#:inToGroup:arrSCBR(index).SCBRState:
                           arrSCBR(index).SCBRbr2:
                           arrSCBR(index).SCBREfdt) pr100;
                    If not %Found(wdr1l);
                    //Write to To Group if form does not exist in To Group
                       Exsr WriteWdr1p;
                    Endif;
                 //No dupllicate SCBR found in To Group, Copy all the SCBR
                 When inSkipRepl = 'A';
                    Exsr WriteWdr1p;
                 Endsl;
              Endif;
              ReadC sflToGrp;
           Enddo;
          index += 1;
       Enddo;

       Begsr WriteWdr1p;
         //Write to To Group if form does not exist in To Group
         Chain(n) arrSCBR(index).SCBRRrn pr10p;
         If %Found(Wdr1p);
            R1fnd = inToGroup;
            R1pgm  = Q1pgm;
            R1user = Q1user;
            R1time = Timeto6(SystemTime());
            R1dt   = Dateto7(SystemDate());
            Write pr10p;
         Endif;
       Endsr;

      /End-Free
     p CopySCBRRecord  e

      //*** End   Add *** 09/26/22 *******************************************
