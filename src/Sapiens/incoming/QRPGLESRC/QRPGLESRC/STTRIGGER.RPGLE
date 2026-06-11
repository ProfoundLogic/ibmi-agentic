     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h/title STTRIGGER - ISO Trigger Processing (Real time)
     h dftname(STTRIGGER) actgrp('STTRIGGER')
     h option(*nodebugio: *srcstmt)
     h/define   PROFOUNDUI
     h bnddir('STBNDDIR':'STTRIGGER')
     h**********************************************************************
      *====================================================================*
      *  Description:  This Program Will process ISO triggers              *
      *                                                                    *
      *  Called from: iSeries Triggers                                     *
      *                                                                    *
      *  Created on 07/20/2016 by C:308 for Project: 114WXY                *
      *====================================================================*
      *  Revisions:                                                        *
      *                                                                    *
      *  05/22/24 C:079 Support: 115510/146                                *
      *                 If transferring to quote do not update WMEMLQ in   *
      *                 Quote.  Check for W8700R in call stack             *
      *                                                                    *
      *  03/04/24 C:344 Support: 115190 (099)                              *
      *                 Unlock record lock on file WMEMLQ.                 *
      *                                                                    *
      *  03/01/24 C:79  Support: 115190 (099)                              *
      *                 Avoid record lock on WMEMLQ in Do Loop             *
      *                                                                    *
      *  12/16/22 C:79  Support: 105589 (153)                              *
      *                 Avoid record lock on WMEMLQ                        *
      *                                                                    *
      *  11/02/22 C:65  Support: 104095 (LUBA)                             *
      *                 Do not update initial pgm/user/date/time fields    *
      *                 when updating quote records with live info.        *
      *                                                                    *
      *  03/14/22 C:79  Support: 101377 (WC2278)                           *
      *                 Contacts are only in live now, remove WMECLQ logic *
      *                                                                    *
      *  09/15/21 C:33  Project WC2679  98091                              *
      *                 Do not update EMCAD1 from live                     *
      *                                                                    *
      *  02/08/21 C:33  Project WC2679  94388                              *
      *                 Do not update EMQTFR and EMRQFL from live          *
      *                                                                    *
      *  02/03/20 C:180 Project WC2679                                     *
      *                 Add processing triggers for files                  *
      *                 WMALP, WMA3P, WMECP and WMOOP.
      *                 Also, update WMEMP processing trigger.
      *                                                                    *
      *  04/10/19 C:079 Support 114/78141                                  *
      *                 Change act grp to *NEW                             *
      *                                                                    *
      *  02/08/18 C:062 Project 114WJD                                     *
      *                 Add more fields to check change.                   *
      *                                                                    *
      *  09/21/17 C:079 Support: 114/49655                                 *
      *                 Remove FMPDP from triggering                       *
      *                                                                    *
      *  08/16/17 C:079 Support: 114/48344                                 *
      *                 Add FAEOP to initial load                          *
      *                 If claim changes dont always write medical         *
      *                                                                    *
      *  05/19/17 C:309 Support: 114/47323                                 *
      *                 Adding EOBLST (Bill Status) check in FAEOP file    *
      *                 processing.                                        *
      *                                                                    *
      *  05/10/17 C:079 Support: 114/47024                                 *
      *                 Change trigger for group files                     *
      *                                                                    *
      *  12/01/16 C:308 Project: 114WYA                                    *
      *                Process group greater/equal to 300 only             *
      *                                                                    *
      *  08/19/16 C:309 Project: 114WYA                                    *
      *                MSP (ISO) Navigator Reporting for CMS               *
      *                                                                    *
      *  09/08/16 C: 308 Support:43269                                    *
      *                  Add Tevent for medical case                       *
      *                                                                    *
      *  08/18/16 C: 079 Project: 114WXY                                   *
      *                  Do not call ISOEXTRACT when updating scores       *
      *                                                                    *
      * ------------------------------------------------------------------ *
      *  NOTE: Please add comments to the top of Revisions.                *
      * ------------------------------------------------------------------ *
      *====================================================================*
     fwmcmla    if   e           k disk    Rename(pcm01:pcm0A)
     f                                     usropn
     fwmcml4    if   e           k disk    Rename(pcm01:pcm04)
     f                                     usropn
     fwmcml     if   e           k disk    Rename(pcm01:pcm00)
     f                                     usropn
     fsmcop     if   e             disk    Rename(pco01:pco0p)
     f                                     usropn
     fwmeml22   if   e           k disk    Rename(pem01:pem22)
     f                                     usropn
     fwmeml12   if   e           k disk    Rename(pem01:pem12)
     f                                     usropn
     ffaeolu    if   e           k disk    Rename(peo01:peo0u)
     f                                     usropn
     ffaeolj    if   e           k disk    Rename(peo01:peo0j)
     f                                     usropn
     fssisp     if   e             disk    Rename(pis01:pis0p)
     f                                     usropn
     Fwmallq    uf a e           k DISK    usropn
     fwma3lq    uf a e           k disk    usropn
     f*** Beg Del *** 03/17/22 ************************************************
     f***wmeclq    uf a e           k disk    usropn
     f*** End Del *** 03/17/22 ************************************************
     fwmool9    uf a e           k disk    usropn
     fwmemlq    uf   e           k disk    usropn prefix(q) rename(PEM01:PEM01Q)
     d psds          esds                  extname(szq1p)

      * work variables
     d ##JobQ          s             10    inz
     d strStaf         s              3    inz
     d strpst          s              2    inz
     d strcnty         s              2    inz
     d strCNkey        s             10    inz
     d intFein         s              9  0 inz
     d ##Co#           s              3  0 inz
     d ##fnd           s              3  0 inz
     d ##EmpOrEob      s              9  0 inz
     d ##DivOrSub      s              5  0 inz
     d ##Fyr           s              3  0 inz
     d ##Med           s              1    inz
     d ##Case          s              7  0 inz
     d ##Prcd          s             11    inz
     d ##Seq           s              5  0 inz
     d strSS#          s              9    inz
     d strCommand      s            255    inz
     d OutLib          s             10    inz
     d QteLib          s             10    inz
     d ##ElId          s             10    inz
     d ##Lsn           s             30A   inz
     d ##Fsn           s             15A   inz
     d ##Mid           s              1A   inz
     d ##PosN          s             10A   inz
     d ##Pcnt          s              1A   inz

     d ##Ofc#          s              5    inz
     d ##flgCMS        s              1    inz

      * Declare dataArea ISOEXTRACT
     d  isoExtract     s              1    dtaara

     d isOrgSubmit     S              1
     d isChange        S               n
     d isScoreUpdate   S               n
      * Trigger Buffer data structure
     d tgbufds         ds
     d  tgfile                       10
     d  tglib                        10
     d  tgmbr                        10
     d  tevent                        1
     d  tgtrgtime                     1
     d  tgcmtlvl                      1
     d  tgreserve1                    3
     d  tgccsid                      10i 0
     d  tgreserve2                    8
     d  tgbfrofs                     10i 0
     d  tgbfrlen                     10i 0
     d  tgbfrnulof                   10i 0
     d  tgbfrnulln                   10i 0
     d  tgaftofs                     10i 0
     d  tgaftlen                     10i 0
     d  tgaftnulof                   10i 0
     d  tgaftnulln                   10i 0

      * Buffer length and pointers to before & after images
     d  tgbuflen       s             10i 0

      * file pointers
     d ptrWMCMPbfr     s               *   inz
     d ptrWMCMPaft     s               *   inz
     d ptrWMCDPbfr     s               *   inz
     d ptrWMCDPaft     s               *   inz
     d ptrWTCNPbfr     s               *   inz
     d ptrWTCNPaft     s               *   inz
     d ptrWMEMPbfr     s               *   inz
     d ptrWMEMPaft     s               *   inz
     d ptrWDF3Pbfr     s               *   inz
     d ptrWDF3Paft     s               *   inz
     d ptrWMFNPbfr     s               *   inz
     d ptrWMFNPaft     s               *   inz
     d ptrSMUPPbfr     s               *   inz
     d ptrSMUPPaft     s               *   inz
     d ptrWMWHPbfr     s               *   inz
     d ptrWMWHPaft     s               *   inz
     d ptrFDDHPbfr     s               *   inz
     d ptrFDDHPaft     s               *   inz
     d ptrFDDUPbfr     s               *   inz
     d ptrFDDUPaft     s               *   inz
     d ptrFDDPPbfr     s               *   inz
     d ptrFDDPPaft     s               *   inz
     d ptrFDDDPbfr     s               *   inz
     d ptrFDDDPaft     s               *   inz
     d ptrFMPDPbfr     s               *   inz
     d ptrFMPDPaft     s               *   inz
     d ptrFAEOPbfr     s               *   inz
     d ptrFAEOPaft     s               *   inz
     d ptrFAEUPbfr     s               *   inz
     d ptrFAEUPaft     s               *   inz
     d ptrFAEDPbfr     s               *   inz
     d ptrFAEDPaft     s               *   inz
     d ptrFAEHPbfr     s               *   inz
     d ptrFAEHPaft     s               *   inz
     d ptrFDCAPbfr     s               *   inz
     d ptrFDCAPaft     s               *   inz
     d ptrFMCDPbfr     s               *   inz
     d ptrFMCDPaft     s               *   inz
     d ptrWMRPPbfr     s               *   inz
     d ptrWMRPPaft     s               *   inz
     d ptrFDDEPbfr     s               *   inz
     d ptrFDDEPaft     s               *   inz
     d ptrWDF2Pbfr     s               *   inz
     d ptrWDF2Paft     s               *   inz

     d ptrWMLLPbfr     s               *   inz
     d ptrWMLLPaft     s               *   inz

     d ptrWMPDPbfr     s               *   inz
     d ptrWMPDPaft     s               *   inz

     d ptrWMOFPbfr     s               *   inz
     d ptrWMOFPaft     s               *   inz

     d ptrSADAPbfr     s               *   inz
     d ptrSADAPaft     s               *   inz
     d ptrWDELPbfr     s               *   inz
     d ptrWDELPaft     s               *   inz
     d ptrWMALPbfr     s               *   inz
     d ptrWMALPaft     s               *   inz
     d AlpDs1        e ds                  extname(WMALP)
      *
     d ptrWMA3Pbfr     s               *   inz
     d ptrWMA3Paft     s               *   inz
     d A3pDs1        e ds                  extname(WMA3P)
      *
     d ptrWMECPbfr     s               *   inz
     d ptrWMECPaft     s               *   inz
     d ECpDs1        e ds                  extname(WMECP)
      *
     d ptrWMOOPbfr     s               *   inz
     d ptrWMOOPaft     s               *   inz
     d OOpDs1        e ds                  extname(WMOOP)
      *
     d EMpDs1        e ds                  extname(WMEMP) Prefix(q)

     d WMCMP_before  e ds                  extname(WMCMP)
     d                                     prefix(bfr)
     d                                     based(ptrWMCMPbfr)
     d WMCMP_after   e ds                  extname(WMCMP)
     d                                     prefix(aft)
     d                                     based(ptrWMCMPaft)
     d WMCDP_before  e ds                  extname(WMCDP)
     d                                     prefix(bfr)
     d                                     based(ptrWMCDPbfr)
     d WMCDP_after   e ds                  extname(WMCDP)
     d                                     prefix(aft)
     d                                     based(ptrWMCDPaft)
     d WTCNP_before  e ds                  extname(WTCNP)
     d                                     prefix(bfr)
     d                                     based(ptrWTCNPbfr)
     d WTCNP_after   e ds                  extname(WTCNP)
     d                                     prefix(aft)
     d                                     based(ptrWTCNPaft)
     d WMEMP_before  e ds                  extname(WMEMP)
     d                                     prefix(bfr)
     d                                     based(ptrWMEMPbfr)
     d WMEMP_after   e ds                  extname(WMEMP)
     d                                     prefix(aft)
     d                                     based(ptrWMEMPaft)
     d WDF3P_before  e ds                  extname(WDF3P)
     d                                     prefix(bfr)
     d                                     based(ptrWDF3Pbfr)
     d WDF3P_after   e ds                  extname(WDF3P)
     d                                     prefix(aft)
     d                                     based(ptrWDF3Paft)
     d WMFNP_before  e ds                  extname(WMFNP)
     d                                     prefix(bfr)
     d                                     based(ptrWMFNPbfr)
     d WMFNP_after   e ds                  extname(WMFNP)
     d                                     prefix(aft)
     d                                     based(ptrWMFNPaft)
     d SMUPP_before  e ds                  extname(SMUPP)
     d                                     prefix(bfr)
     d                                     based(ptrSMUPPbfr)
     d SMUPP_after   e ds                  extname(SMUPP)
     d                                     prefix(aft)
     d                                     based(ptrSMUPPaft)
     d WMWHP_before  e ds                  extname(WMWHP)
     d                                     prefix(bfr)
     d                                     based(ptrWMWHPbfr)
     d WMWHP_after   e ds                  extname(WMWHP)
     d                                     prefix(aft)
     d                                     based(ptrWMWHPaft)
     d FDDHP_before  e ds                  extname(FDDHP)
     d                                     prefix(bfr)
     d                                     based(ptrFDDHPbfr)
     d FDDHP_after   e ds                  extname(FDDHP)
     d                                     prefix(aft)
     d                                     based(ptrFDDHPaft)
     d FDDUP_before  e ds                  extname(FDDUP)
     d                                     prefix(bfr)
     d                                     based(ptrFDDUPbfr)
     d FDDUP_after   e ds                  extname(FDDUP)
     d                                     prefix(aft)
     d                                     based(ptrFDDUPaft)
     d FDDPP_before  e ds                  extname(FDDPP)
     d                                     prefix(bfr)
     d                                     based(ptrFDDPPbfr)
     d FDDPP_after   e ds                  extname(FDDPP)
     d                                     prefix(aft)
     d                                     based(ptrFDDPPaft)
     d FDDDP_before  e ds                  extname(FDDDP)
     d                                     prefix(bfr)
     d                                     based(ptrFDDDPbfr)
     d FDDDP_after   e ds                  extname(FDDDP)
     d                                     prefix(aft)
     d                                     based(ptrFDDDPaft)
     d FMPDP_before  e ds                  extname(FMPDP)
     d                                     prefix(bfr)
     d                                     based(ptrFMPDPbfr)
     d FMPDP_after   e ds                  extname(FMPDP)
     d                                     prefix(aft)
     d                                     based(ptrFMPDPaft)
     d FAEOP_before  e ds                  extname(FAEOP)
     d                                     prefix(bfr)
     d                                     based(ptrFAEOPbfr)
     d FAEOP_after   e ds                  extname(FAEOP)
     d                                     prefix(aft)
     d                                     based(ptrFAEOPaft)
     d FAEUP_before  e ds                  extname(FAEUP)
     d                                     prefix(bfr)
     d                                     based(ptrFAEUPbfr)
     d FAEUP_after   e ds                  extname(FAEUP)
     d                                     prefix(aft)
     d                                     based(ptrFAEUPaft)
     d FAEDP_before  e ds                  extname(FAEDP)
     d                                     prefix(bfr)
     d                                     based(ptrFAEDPbfr)
     d FAEDP_after   e ds                  extname(FAEDP)
     d                                     prefix(aft)
     d                                     based(ptrFAEDPaft)
     d FAEHP_before  e ds                  extname(FAEHP)
     d                                     prefix(bfr)
     d                                     based(ptrFAEHPbfr)
     d FAEHP_after   e ds                  extname(FAEHP)
     d                                     prefix(aft)
     d                                     based(ptrFAEHPaft)
     d FDCAP_before  e ds                  extname(FDCAP)
     d                                     prefix(bfr)
     d                                     based(ptrFDCAPbfr)
     d FDCAP_after   e ds                  extname(FDCAP)
     d                                     prefix(aft)
     d                                     based(ptrFDCAPaft)
     d FMCDP_before  e ds                  extname(FMCDP)
     d                                     qualified
     d                                     prefix(bfr)
     d                                     based(ptrFMCDPbfr)
     d FMCDP_after   e ds                  extname(FMCDP)
     d                                     qualified
     d                                     prefix(aft)
     d                                     based(ptrFMCDPaft)
     d WMRPP_before  e ds                  extname(WMRPP)
     d                                     prefix(bfr)
     d                                     based(ptrWMRPPbfr)
     d WMRPP_after   e ds                  extname(WMRPP)
     d                                     prefix(aft)
     d                                     based(ptrWMRPPaft)
     d FDDEP_before  e ds                  extname(FDDEP)
     d                                     prefix(bfr)
     d                                     based(ptrFDDEPbfr)
     d FDDEP_after   e ds                  extname(FDDEP)
     d                                     prefix(aft)
     d                                     based(ptrFDDEPaft)
     d WDF2P_before  e ds                  extname(WDF2P)
     d                                     prefix(bfr)
     d                                     based(ptrWDF2Pbfr)
     d WDF2P_after   e ds                  extname(WDF2P)
     d                                     prefix(aft)
     d                                     based(ptrWDF2Paft)
      *
     d WMLLP_before  e ds                  extname(WMLLP)
     d                                     prefix(bfr)
     d                                     based(ptrWMLLPbfr)
     d WMLLP_after   e ds                  extname(WMLLP)
     d                                     prefix(aft)
     d                                     based(ptrWMLLPaft)
      *
     d WMPDP_before  e ds                  extname(WMPDP)
     d                                     prefix(bfrW)
     d                                     based(ptrWMPDPbfr)
     d WMPDP_after   e ds                  extname(WMPDP)
     d                                     prefix(aftW)
     d                                     based(ptrWMPDPaft)
      *
     d WMOFP_before  e ds                  extname(WMOFP)
     d                                     prefix(bfr)
     d                                     based(ptrWMOFPbfr)
     d WMOFP_after   e ds                  extname(WMOFP)
     d                                     prefix(aft)
     d                                     based(ptrWMOFPaft)
      *
     d SADAP_before  e ds                  extname(SADAP)
     d                                     prefix(bfr)
     d                                     based(ptrSADAPbfr)
     d SADAP_after   e ds                  extname(SADAP)
     d                                     prefix(aft)
     d                                     based(ptrSADAPaft)
     d WDELP_before  e ds                  extname(WDELP)
     d                                     prefix(bfr)
     d                                     based(ptrWDELPbfr)
     d WDELP_after   e ds                  extname(WDELP)
     d                                     prefix(aft)
     d                                     based(ptrWDELPaft)
     d WMALP_before  e ds                  extname(WMALP)
     d                                     prefix(bfr)
     d                                     based(ptrWMALPbfr)
     d WMALP_after   e ds                  extname(WMALP)
     d                                     prefix(aft)
     d                                     based(ptrWMALPaft)

     d WMA3P_before  e ds                  extname(WMA3P)
     d                                     prefix(bfr)
     d                                     based(ptrWMA3Pbfr)
     d WMA3P_after   e ds                  extname(WMA3P)
     d                                     prefix(aft)
     d                                     based(ptrWMA3Paft)

     d WMECP_before  e ds                  extname(WMECP)
     d                                     prefix(bfr)
     d                                     based(ptrWMECPbfr)
     d WMECP_after   e ds                  extname(WMECP)
     d                                     prefix(aft)
     d                                     based(ptrWMECPaft)

     d WMOOP_before  e ds                  extname(WMOOP)
     d                                     prefix(bfr)
     d                                     based(ptrWMOOPbfr)
     d WMOOP_after   e ds                  extname(WMOOP)
     d                                     prefix(aft)
     d                                     based(ptrWMOOPaft)

     d ISOSbmCl        pr                  Extpgm('ISOEXTSBC')
     d ##JobQ                        10
     d ##File                        10
     d ##Event                        1
     d ##Co#                          3  0
     d ##fnd                          3  0
     d ##EmpOrEob                     9  0
     d ##DivOrSub                     5  0
     d ##Fyr                          3  0
     d ##Case                         7  0
     d ##Med                          1
     d ##Prcd                        11
     d ##Seq                          5  0
     d ##Ofc#                         5
     d ##flgCMS                       1
     d ##Event         s              1
     d ##File          s             10
     d/copy WPRGROUP
     d*** Beg Add *** 05/22/24 *********************************************
     d/copy sPrJobInfo
     d*** End Add *** 05/22/24 *********************************************
      * File prcessing sub procedures
     d GetIsoJobQ      pr
     d ProcessFAEDP    pr
     d ProcessFAEHP    pr
     d ProcessFAEOP    pr
     d ProcessFAEUP    pr
     d ProcessFDCAP    pr
     d ProcessFDDDP    pr
     d ProcessFDDEP    pr
     d ProcessFDDHP    pr
     d ProcessFDDPP    pr
     d ProcessFDDUP    pr
     d ProcessFMCDP    pr
     d ProcessFMPDP    pr
     d ProcessSMUPP    pr
     d ProcessWDF3P    pr
     d ProcessWMCDP    pr
     d ProcessWMCMP    pr
     d ProcessWMEMP    pr
     d ProcessWMFNP    pr
     d ProcessWMRPP    pr
     d ProcessWMWHP    pr
     d ProcessWTCNP    pr
     d SubmitIsoJob    pr
     d ProcessSADAP    pr
     d ProcessWDF2P    pr
     d ProcessWMLLP    pr
     d ProcessWMOFP    pr
     d ProcessWMPDP    pr
     d ProcessWDELP    pr
     d RtvQteLib       pr                  extpgm('RTVQTELIB')
     d                                4    const
     d                               10    const

     d QCMDEXC         pr                  extpgm('QCMDEXC')
     d                              255
     d                               15P 5 const

     d ProcessWMALP    pr
     d ProcessWMA3P    pr
     d ProcessWMECP    pr
     d ProcessWMOOP    pr

     c     *entry        plist
     c                   parm                    tgbufds
     c                   parm                    tgbuflen

      /free
        GetIsoJobq();
        If ISjobq <> *blanks;
        // Retrieve the Data Area value as Y or N
           in *lock *dtaara;
           isOrgSubmit =  isoExtract;
           unlock  isoextract;
           ##Med  = *Blank;
           ##Prcd = *Blank;
           ##Seq  = *zeros;
           ##Ofc#  = *Blanks;
           ##flgCMS= *Blanks;

           Select;
              When tgFile = 'FAEDP';
                 ProcessFAEDP();

              When tgFile = 'FAEHP';
                 ProcessFAEHP();

              When tgFile = 'FAEOP';
                 ProcessFAEOP();

              When tgFile = 'FAEUP';
                 ProcessFAEUP();

              When tgFile = 'FDCAP';
                 ProcessFDCAP();

              When tgFile = 'FDDDP';
                 ProcessFDDDP();

              When tgFile = 'FDDEP';
                 ProcessFDDEP();

              When tgFile = 'FDDHP';
                 ProcessFDDHP();

              When tgFile = 'FDDPP';
                 ProcessFDDPP();

              When tgFile = 'FDDUP';
                 ProcessFDDUP();

              When tgFile = 'FMCDP';
                 ProcessFMCDP();


              When tgFile = 'SMUPP';
                 ProcessSMUPP();

              When tgFile = 'WDF3P';
                 ProcessWDF3P();

              When tgFile = 'WMALP';
                 ProcessWMALP();

              When tgFile = 'WMA3P';
                 ProcessWMA3P();

              When tgFile = 'WMCDP';
                 ProcessWMCDP();

              When tgFile = 'WMCMP';
                 ProcessWMCMP();

              When tgFile = 'WMECP';
                 ProcessWMECP();

              When tgFile = 'WMEMP';
                 ProcessWMEMP();

              When tgFile = 'WDELP';
                 ProcessWDELP();

              When tgFile = 'WMFNP';
                 ProcessWMFNP();

              When tgFile = 'WMOOP';
                 ProcessWMOOP();

              When tgFile = 'WMRPP';
                 ProcessWMRPP();

              When tgFile = 'WMWHP';
                 ProcessWMWHP();

              When tgFile = 'WTCNP';
                 ProcessWTCNP();

              When tgFile = 'WDF2P';
                 ProcessWDF2P();

              When tgFile = 'WMLLP';
                 ProcessWMLLP();

              When tgFile = 'WMPDP';
                 ProcessWMPDP();

              When tgFile = 'WMOFP';
                 ProcessWMOFP();

              When tgFile = 'SADAP';
                 ProcessSADAP();


           EndSl;

        Else;
           Select;
           When tgFile = 'WMALP';
              ProcessWMALP();

           When tgFile = 'WMA3P';
              ProcessWMA3P();

           When tgFile = 'WMECP';
              ProcessWMECP();

           When tgFile = 'WMEMP';
              ProcessWMEMP();

           When tgFile = 'WMOOP';
              ProcessWMOOP();
           Endsl;
        EndIf;

        *inlr = *on;
        Return;

      /End-Free

       //*********************************************************
       //Subprocedure to get Jobq
       //*********************************************************
     p GetIsoJobq      b
     d GetIsoJobq      pi
      /Free
         If not %open(SSISP);
            Open ssisp;
         Endif;
         Read pis0p;
         If not %Eof(ssisp);
            ##Jobq = ISjobq;
         EndIf;
         Close ssisp;
      /End-Free
     p GetIsoJobq      e

       //*********************************************************
       //Process FAEDP changes
       //*********************************************************
     p ProcessFAEDP    b
     d ProcessFAEDP    pi
      /Free
         ptrFAEDPbfr = %addr(tgbufds) + tgbfrofs;
         ptrFAEDPaft = %addr(tgbufds) + tgaftofs;

         Select;
            // Insert Event   or  // Update Event
            When tEvent = '1'  or tEvent = '3';
               ##Co#       =   aftEDCo#;
               ##fnd       =   aftEDfnd;
               ##Fyr       =   aftEDFyr;
               ##Case      =   aftEDCase;
               ##EmpOrEob  =   aftEDeob#;
               ##DivOrSub  =   aftEDsub#;

            // Delete Event
            When tEvent = '2';
               ##Co#       =   bfrEDCo#;
               ##fnd       =   bfrEDfnd;
               ##Fyr       =   bfrEDFyr;
               ##Case      =   bfrEDCase;
               ##EmpOrEob  =   bfrEDeob#;
               ##DivOrSub  =   bfrEDsub#;
         EndSl;

         // Check Group is Active and not a Test Group and GE to 300.
         If TestGrp(##Co#:##Fnd) = *off and ExpiredGrp(##Co#:##Fnd) = *Off and
            ##Fnd >= 300 ;

            // Process the FAEDP changes

            // Check bfr & Aft images fields value for any change
            If tEvent = '3' and
             ( bfrEDtnpi <> aftEDtnpi    or bfrEDicdind  <> aftEDicdind or
             bfrEDicd1001 <> aftEDicd1001 or bfrEDicd1002 <> aftEDicd1002 or
             bfrEDicd1003 <> aftEDicd1003 or bfrEDicd1004 <> aftEDicd1004 );

               isChange = *On;
            EndIf;

            If isChange = *On or tEvent <> '3';
               SubmitIsoJob();
            EndIf;
         EndIf;
      /End-Free
     p ProcessFAEDP    e

       //*********************************************************
       //Process FAEHP changes
       //*********************************************************
     p ProcessFAEHP    b
     d ProcessFAEHP    pi
      /Free
         ptrFAEHPbfr = %addr(tgbufds) + tgbfrofs;
         ptrFAEHPaft = %addr(tgbufds) + tgaftofs;

         Select;
            // Insert Event   or  // Update Event
            When tEvent = '1'  or tEvent = '3';
               ##Co#      =   aftEHCo#;
               ##fnd      =   aftEHfnd;
               ##Fyr      =   aftEHFyr;
               ##Case     =   aftEHCase;
               ##EmpOrEob =   aftEHeob#;
               ##DivOrSub =   aftEHsub#;

            // Delete Event
            When tEvent = '2';
               ##Co#       =   bfrEHCo#;
               ##fnd       =   bfrEHfnd;
               ##Fyr       =   bfrEHFyr;
               ##Case      =   bfrEHCase;
               ##EmpOrEob  =   bfrEHeob#;
               ##DivOrSub   =   bfrEHsub#;
         EndSl;

         // Check Group is Active and not a Test Group and GE to 300.
         If TestGrp(##Co#:##Fnd) = *off and ExpiredGrp(##Co#:##Fnd) = *Off and
            ##Fnd >= 300 ;

            // Process the FAEHP changes

            // Check bfr & Aft images fields value for any change
            If tEvent = '3' and
             ( bfrEHicdind  <> aftEHicdind or
             bfrEHicd1001 <> aftEHicd1001 or bfrEHicd1002 <> aftEHicd1002 or
             bfrEHicd1003 <> aftEHicd1003 or bfrEHicd1004 <> aftEHicd1004 );

               isChange = *On;
            EndIf;

            If isChange = *On or tEvent <> '3';
               SubmitIsoJob();
            EndIf;
         EndIf;
      /End-Free
     p ProcessFAEHP    e

       //*********************************************************
       //Process FAEOP changes
       //*********************************************************
     p ProcessFAEOP    b
     d ProcessFAEOP    pi
      /Free
         ptrFAEOPbfr = %addr(tgbufds) + tgbfrofs;
         ptrFAEOPaft = %addr(tgbufds) + tgaftofs;

         Select;
            // Insert Event   or  // Update Event
            When tEvent = '1'  or tEvent = '3';
               ##Co#       =   aftEOCo#;
               ##fnd       =   aftEOfnd;
               ##Fyr       =   aftEOFyr;
               ##Case      =   aftEOCase;
               ##EmpOrEob  =   aftEOeob#;
               ##DivOrSub  =   aftEOsuB#;

            // Delete Event
            When tEvent = '2';
               ##Co#       =   bfrEOCo#;
               ##fnd       =   bfrEOfnd;
               ##Fyr       =   bfrEOFyr;
               ##Case      =   bfrEOCase;
               ##EmpOrEob  =   bfrEOeob#;
               ##DivOrSub  =   bfrEOsuB#;

         EndSl;

         // Check Group is Active and not a Test Group and GE to 300.
         If TestGrp(##Co#:##Fnd) = *off and ExpiredGrp(##Co#:##Fnd) = *Off and
            ##Fnd >= 300 ;

            // Process the FAEOP changes

            // Check bfr & Aft images fields value for any change
            If tEvent = '3' and
             ( bfrEOeob# <> aftEOeob# or  bfrEObtyp <> aftEObtyp or
             bfrEOfein <> aftEOfein or  bfrEOfsn  <> aftEOfsn  or
             bfrEOcrdt <> aftEOcrdt                            or
             bfrEOmid  <> aftEOmid  or  bfrEOlsn  <> aftEOlsn  or
             bfrEOss#  <> aftEOss#  or  bfrEOacdt <> aftEOacdt or
             bfrEOspcd <> aftEOspcd or  bfrEOntwk <> aftEOntwk or
             bfrEOflic <> aftEOflic or  bfrEOchrg <> aftEOchrg or
             bfrEOpaid <> aftEOpaid or  bfrEOfrdt <> aftEOfrdt or

             bfrEOtodt <> aftEOtodt or  bfrEOfrdt <> aftEOfrdt or
             bfrEOblst <> aftEOblst);

               isChange = *On;
            EndIf;

            If isChange = *On or isOrgSubmit = 'Y' or tEvent <> '3';
               SubmitIsoJob();
            EndIf;
         EndIf;
      /End-Free
     p ProcessFAEOP    e

       //*********************************************************
       //Process FAEUP changes
       //*********************************************************
     p ProcessFAEUP    b
     d ProcessFAEUP    pi
      /Free
         ptrFAEUPbfr = %addr(tgbufds) + tgbfrofs;
         ptrFAEUPaft = %addr(tgbufds) + tgaftofs;

         Select;
            // Insert Event   or  // Update Event
            When tEvent = '1'  or tEvent = '3';
               ##Co#       =   aftEUco#;
               ##fnd       =   aftEUfnd;
               ##Fyr       =   aftEUFyr;
               ##Case      =   aftEUCase;
               ##EmpOrEob  =   aftEUeob#;
               ##DivOrSub  =   aftEUsub#;

            // Delete Event
            When tEvent = '2';
               ##Co#       =   bfrEUco#;
               ##fnd       =   bfrEUfnd;
               ##Fyr       =   bfrEUfyr;
               ##Case      =   bfrEUcase;
               ##EmpOrEob  =   bfrEUeob#;
               ##DivOrSub  =   bfrEUsub#;
         EndSl;

         // Check Group is Active and not a Test Group and GE to 300.
         If TestGrp(##Co#:##Fnd) = *off and ExpiredGrp(##Co#:##Fnd) = *Off and
            ##Fnd >= 300 ;

            // Process the FAEUP changes

            // Check bfr & Aft images fields value for any change
            If tEvent = '3' and
             ( bfrEUICD1001 <> aftEUICD1001 or bfrEUICD1004 <> aftEUICD1004 or
             bfrEUICD1002 <> aftEUICD1002 or bfrEUICD1005 <> aftEUICD1005 or
             bfrEUICD1003 <> aftEUICD1003 or bfrEUICD1006 <> aftEUICD1006 or
             bfrEUICD1007 <> aftEUICD1007 or bfrEUICD1008 <> aftEUICD1008 or
             bfrEUICD1009 <> aftEUICD1009 or bfrEUICD1010 <> aftEUICD1010 or
             bfrEUicdind  <> aftEUicdind  or bfrEUicn     <> aftEUicn     or
             bfrEUanpi <> aftEUanpi );

               isChange = *On;
            EndIf;

            If isChange = *On or tEvent <> '3';
               SubmitIsoJob();
            EndIf;
         EndIf;
      /End-Free
     p ProcessFAEUP    e

       //*********************************************************
       //Process FDCAP changes
       //*********************************************************
     p ProcessFDCAP    b
     d ProcessFDCAP    pi
      /Free
         ptrFDCAPbfr = %addr(tgbufds) + tgbfrofs;
         ptrFDCAPaft = %addr(tgbufds) + tgaftofs;

         Select;
            // Insert Event   or  // Update Event
            When tEvent = '1'  or tEvent = '3';
               ##Co#       =   aftCACo#;
               ##fnd       =   aftCAfnd;
               ##Fyr       =   aftCAFyr;
               ##Case      =   aftCACase;
               ##EmpOrEob  =   aftCAeob#;
               ##DivOrSub  =   aftCAsub#;
               ##Seq       =   aftCAdseq;

            // Delete Event
            When tEvent = '2';
               ##Co#       =   bfrCACo#;
               ##fnd       =   bfrCAfnd;
               ##Fyr       =   bfrCAFyr;
               ##Case      =   bfrCACase;
               ##EmpOrEob  =   bfrCAeob#;
               ##DivOrSub  =   bfrCAsub#;
               ##Seq       =   bfrCAdseq;
         EndSl;

         // Check Group is Active and not a Test Group and GE to 300.
         If TestGrp(##Co#:##Fnd) = *off and ExpiredGrp(##Co#:##Fnd) = *Off and
            ##Fnd >= 300 ;

            // Process the FDCAP changes

            // Check bfr & Aft images fields value for any change
            If tEvent = '3' and
             ( bfrCAlin# <> aftCAlin# or  bfrCAfdat <> aftCAfdat or
             bfrCAunts <> aftCAunts or  bfrCAchrg <> aftCAchrg or
             bfrCApaid <> aftCApaid );

               isChange = *On;
            EndIf;

            If isChange = *On or tEvent <> '3';
               SubmitIsoJob();
            EndIf;
         EndIf;
      /End-Free
     p ProcessFDCAP    e

       //*********************************************************
       //Process FDDDP changes
       //*********************************************************
     p ProcessFDDDP    b
     d ProcessFDDDP    pi
      /Free
         ptrFDDDPbfr = %addr(tgbufds) + tgbfrofs;
         ptrFDDDPaft = %addr(tgbufds) + tgaftofs;

         Select;
            // Insert Event   or  // Update Event
            When tEvent = '1'  or tEvent = '3';
               ##Co#      =   aftDDCo#;
               ##fnd      =   aftDDfnd;
               ##Fyr      =   aftDDFyr;
               ##Case     =   aftDDCase;
               ##EmpOrEob =   aftDDeob#;
               ##DivOrSub =   aftDDsub#;
               ##prcd     =   aftDDprcd;
               ##Seq      =   aftDDdseq;

            // Delete Event
            When tEvent = '2';
               ##Co#      =   bfrDDCo#;
               ##fnd      =   bfrDDfnd;
               ##Fyr      =   bfrDDFyr;
               ##Case     =   bfrDDCase;
               ##EmpOrEob =   bfrDDeob#;
               ##DivOrSub =   bfrDDsub#;
               ##prcd     =   bfrDDprcd;
               ##Seq      =   bfrDDdseq;
         EndSl;

         // Check Group is Active and not a Test Group and GE to 300.
         If TestGrp(##Co#:##Fnd) = *off and ExpiredGrp(##Co#:##Fnd) = *Off and
            ##Fnd >= 300 ;

            // Process the FDDDP changes

            // Check bfr & Aft images fields value for any change
            If tEvent = '3' and
             ( bfrDDeob# <> aftDDeob# or  bfrDDprcd <> aftDDprcd or
             bfrDDdxr1 <> aftDDdxr1 or  bfrDDdxr2 <> aftDDdxr2 or
             bfrDDdxr3 <> aftDDdxr3 or  bfrDDdxr4 <> aftDDdxr4 );

               isChange = *On;
            EndIf;

            If isChange = *On or tEvent <> '3';
               SubmitIsoJob();
            EndIf;
         EndIf;
      /End-Free
     p ProcessFDDDP    e

       //*********************************************************
       //Process FDDEP changes
       //*********************************************************
     p ProcessFDDEP    b
     d ProcessFDDEP    pi
      /Free
         ptrFDDEPbfr = %addr(tgbufds) + tgbfrofs;
         ptrFDDEPaft = %addr(tgbufds) + tgaftofs;

         Select;
            // Insert/Update Event
            When tEvent = '1' or tEvent = '3';
               ##Co#      =   aftDECo#;
               ##fnd      =   aftDEfnd;
               ##EmpOrEob =   aftDEeob#;
               ##DivOrSub =   aftDEsub#;
               ##Fyr      =   aftDEfyr;
               ##case     =   aftDEcase;

            // Delete Event
            When tEvent = '2';
               ##Co#   =   bfrDECo#;
               ##fnd   =   bfrDEfnd;
               ##EmpOrEob  =   bfrDEeob#;
               ##DivOrSub  =   bfrDEsub#;
               ##Fyr   =   bfrDEfyr;
               ##case  =   bfrDEcase;
         EndSl;

         // Check Group is Active and not a Test Group and GE to 300.
         If TestGrp(##Co#:##Fnd) = *off and ExpiredGrp(##Co#:##Fnd) = *Off and
            ##Fnd >= 300 ;

            // Check bfr & Aft images fields value for any change
            If tEvent = '3' and
             ( bfrDEeob <> aftDEeob or bfrDEnot1 <> aftDEnot1 );

                isChange = *On;

            EndIf;

            If isChange = *On or tEvent <> '3' ;
               SubmitIsoJob();
            EndIf;
         EndIf;
      /End-Free
     p ProcessFDDEP    e

       //*********************************************************
       //Process FDDHP changes
       //*********************************************************
     p ProcessFDDHP    b
     d ProcessFDDHP    pi
      /Free
          ptrFDDHPbfr = %addr(tgbufds) + tgbfrofs;
          ptrFDDHPaft = %addr(tgbufds) + tgaftofs;

          Select;
             // Insert Event   or  // Update Event
             When tEvent = '1'  or tEvent = '3';
                ##Co#      =   aftDHCo#;
                ##fnd      =   aftDHfnd;
                ##Fyr      =   aftDHFyr;
                ##Case     =   aftDHCase;
                ##EmpOrEob =   aftDHeob#;
                ##DivOrSub =   aftDHsub#;
                ##Prcd     =   aftDHprcd;
                ##Seq      =   aftDHDseq;

           // Delete Event
           When tEvent = '2';
              ##Co#       =   bfrDHCo#;
              ##fnd       =   bfrDHfnd;
              ##Fyr       =   bfrDHFyr;
              ##Case      =   bfrDHCase;
              ##EmpOrEob  =   bfrDHeob#;
              ##DivOrSub  =   bfrDHsub#;
              ##Prcd      =   bfrDHprcd;
              ##Seq       =   bfrDHdseq;
         EndSl;

         // Check Group is Active and not a Test Group and GE to 300.
         If TestGrp(##Co#:##Fnd) = *off and ExpiredGrp(##Co#:##Fnd) = *Off and
            ##Fnd >= 300 ;

            // Process the FDDHP changes

            // Check bfr & Aft images fields value for any change
            If tEvent = '3' and
             ( bfrDHnpir <> aftDHnpir or  bfrDHdxr4 <> aftDHdxr4 or
             bfrDHeob# <> aftDHeob# or  bfrDHmod2 <> aftDHmod2 or
             bfrDHprcd <> aftDHprcd or  bfrDHdxr1 <> aftDHdxr1 or
             bfrDHmod1 <> aftDHmod1 or  bfrDHdxr2 <> aftDHdxr2 or
             bfrDHdxr3 <> aftDHdxr3 );
               isChange = *On;
            EndIf;

            If isChange = *On or tEvent <> '3';
               SubmitIsoJob();
            EndIf;
         EndIf;
      /End-Free
     p ProcessFDDHP    e

       //*********************************************************
       //Process FDDPP changes
       //*********************************************************
     p ProcessFDDPP    b
     d ProcessFDDPP    pi
      /Free
         ptrFDDPPbfr = %addr(tgbufds) + tgbfrofs;
         ptrFDDPPaft = %addr(tgbufds) + tgaftofs;

         Select;
            // Insert Event   or  // Update Event
            When tEvent = '1'  or tEvent = '3';
               ##Co#       =   aftDPCo#;
               ##fnd       =   aftDPfnd;
               ##Fyr       =   aftDPFyr;
               ##Case      =   aftDPCase;
               ##EmpOrEob  =   aftDPeob#;
               ##DivOrSub  =   aftDPsub#;
               ##prcd      =   aftDPndc;
               ##Seq       =   aftDPdseq;

            // Delete Event
            When tEvent = '2';
               ##Co#       =   bfrDPCo#;
               ##fnd       =   bfrDPfnd;
               ##Fyr       =   bfrDPFyr;
               ##Case      =   bfrDPCase;
               ##EmpOrEob  =   bfrDPeob#;
               ##DivOrSub  =   bfrDPsub#;
               ##prcd      =   bfrDPndc;
               ##Seq       =   bfrDPdseq;
         EndSl;

         // Check Group is Active and not a Test Group and GE to 300.
         If TestGrp(##Co#:##Fnd) = *off and ExpiredGrp(##Co#:##Fnd) = *Off and
            ##Fnd >= 300 ;

            // Process the FDDPP changes

            // Check bfr & Aft images fields value for any change
            If tEvent = '3' and
             ( bfrDPeob# <> aftDPeob# or  bfrDPndc  <> aftDPndc );

               isChange = *On;
            EndIf;

            If isChange = *On or tEvent <> '3';
               SubmitIsoJob();
            EndIf;
         EndIf;
      /End-Free
     p ProcessFDDPP    e

       //*********************************************************
       //Process FDDUP changes
       //*********************************************************
     p ProcessFDDUP    b
     d ProcessFDDUP    pi
      /Free
         ptrFDDUPbfr = %addr(tgbufds) + tgbfrofs;
         ptrFDDUPaft = %addr(tgbufds) + tgaftofs;

         Select;
            // Insert Event   or  // Update Event
            When tEvent = '1'  or tEvent = '3';
               ##Co#       =   aftDUCo#;
               ##fnd       =   aftDUfnd;
               ##Fyr       =   aftDUFyr;
               ##Case      =   aftDUCase;
               ##EmpOrEob  =   aftDUeob#;
               ##DivOrSub  =   aftDUsub#;
               ##Prcd      =   aftDUrate;
               ##Seq       =   aftDUdseq;

            // Delete Event
            When tEvent = '2';
               ##Co#       =   bfrDUCo#;
               ##fnd       =   bfrDUfnd;
               ##Fyr       =   bfrDUFyr;
               ##Case      =   bfrDUCase;
               ##EmpOrEob  =   bfrDUeob#;
               ##DivOrSub  =   bfrDUsub#;
               ##Prcd      =   bfrDUrate;
               ##Seq       =   bfrDUdseq;
         EndSl;

         // Check Group is Active and not a Test Group and GE to 300.
         If TestGrp(##Co#:##Fnd) = *off and ExpiredGrp(##Co#:##Fnd) = *Off and
            ##Fnd >= 300 ;

            // Process the FDDUP changes

            // Check bfr & Aft images fields value for any change
            If tEvent = '3' and
             ( bfrDUeob# <> aftDUeob# or  bfrDUmod2 <> aftDUmod2 or
             bfrDUrate <> aftDUrate or  bfrDUpmod <> aftDUpmod );

               isChange = *On;
            EndIf;

            If isChange = *On or tEvent <> '3';
               SubmitIsoJob();
            EndIf;
         EndIf;
      /End-Free
     p ProcessFDDUP    e

       //*********************************************************
       //Process FMCDP changes
       //*********************************************************
     p ProcessFMCDP    b
     d ProcessFMCDP    pi
      /Free
         ptrFMCDPbfr = %addr(tgbufds) + tgbfrofs;
         ptrFMCDPaft = %addr(tgbufds) + tgaftofs;

         Select;
            // Insert Event   or  // Update Event
            When tEvent = '1'  or tEvent = '3';
               strSS#  =   FMCDP_after.aftCDss#;
            // Delete Event
            When tEvent = '2';
               strSS#  =   FMCDP_before.bfrCDss#;
         EndSl;
         If not %open(smcop);
            Open smcop;
         Endif;
         If not %open(faeolj);
            Open faeolj;
         Endif;
         Read pco0p;
         If not %Eof(smcop);
            Chain (COco#:strSS#) peo0j;
            If %Found (faeolj) and EOfnd >= 300;
               ##Co#       =  EOCo#;
               ##fnd       =  EOfnd;
               ##Fyr       =  EOfyr;
               ##Case      =  EOcase;
               ##EmpOrEob  =  EOeob#;
               ##DivOrSub  =  EOsub#;

               // Check bfr & Aft images fields value for any change
               If tEvent = '3' and
                FMCDP_before.bfrCDss#  <>  FMCDP_after.aftCDss#;
                  isChange = *On;
               EndIf;

               If isChange = *On or tEvent <> '3';
                  SubmitIsoJob();
               EndIf;
            EndIf;
         EndIf;
         Close smcop;
         Close faeolj;
      /End-Free
     p ProcessFMCDP    e

       //*********************************************************
       //Process FMPDP changes
       //*********************************************************
     p ProcessFMPDP    b
     d ProcessFMPDP    pi
      /Free
         ptrFMPDPbfr = %addr(tgbufds) + tgbfrofs;
         ptrFMPDPaft = %addr(tgbufds) + tgaftofs;

         Select;
            // Insert Event   or  // Update Event
            When tEvent = '1'  or tEvent = '3';
               intFein =   aftPDfein;
            // Delete Event
            When tEvent = '2';
               intFein =   bfrPDfein;
         EndSl;

         If not %open(smcop);
            Open smcop;
         EndIf;
         If not %open(faeolu);
            Open faeolu;
         EndIf;
         Read pco0p;
         If not %Eof (smcop);
            chain (COco#:intFein) peo0u;
            If %Found (faeolu) and EOfnd >= 300;
               ##Co#   =  EOCo#;
               ##fnd   =  EOfnd;
               ##Fyr   =  EOfyr;
               ##Case  =  EOcase;
               ##EmpOrEob  =   EOeob#;
               ##DivOrSub  =   EOsub#;

               // Check bfr & Aft images fields value for any change
               If tEvent = '3' and
                ( bfrPDfsn  <> aftPDfsn  or  bfrPDname <> aftPDname or
                bfrPDtaxo <> aftPDtaxo or  bfrPDadr1 <> aftPDadr1 or
                bfrPDadr2 <> aftPDadr2 or  bfrPDcty  <> aftPDcty  or
                bfrPDst   <> aftPDst   or  bfrPDzip  <> aftPDzip   );

                  isChange = *On;
               EndIf;

               If isChange = *On or tEvent <> '3';
                  SubmitIsoJob();
               EndIf;
            EndIf;
         EndIf;
         Close smcop;
         Close faeolu;
      /End-Free
     p ProcessFMPDP    e

       //*********************************************************
       //Process SMUPP changes
       //*********************************************************
     p ProcessSMUPP    b
     d ProcessSMUPP    pi
      /free
         ptrSMUPPbfr = %addr(tgbufds) + tgbfrofs;
         ptrSMUPPaft = %addr(tgbufds) + tgaftofs;

         Select;
            // Insert Event   or   Update Event
            When tEvent = '1' or tEvent = '3';
               ##Co#   = aftUPco#;
               strStaf = aftUPstaf;

            // Delete Event
            When tEvent = '2';
               ##Co#   = bfrUPco#;
               strStaf = bfrUPstaf;
         EndSl;
         If not %open(wmcml4);
            Open wmcml4;
         EndIf;
         Chain (##co#:strStaf) pcm04;
         If %Found(wmcml4);
            ##Co#   =  CMCo#;
            ##fnd   =  CMfnd;
            ##EmpOrEob  =  CMEmp#;
            ##DivOrSub  =  CMDiv;
            ##Fyr   =  CMFyr;
            ##Case  =  CMCase;

            // Check bfr & Aft images fields value for any change

            If tEvent = '3' and ( bfrUPusnm <> aftUPusnm or
             bfrUPuser <> aftUPuser or bfrUPmail <> aftUPmail or
             bfrUPphna <> aftUPphna or bfrUPphn# <> aftUPphn# );

               isChange = *On;
            EndIf;

            If tEvent = '3' ;
               If bfrUPuser  <> aftUPuser  or bfrUPmail <> aftUPmail or
                bfrUPphna   <> aftUPphna or bfrUPphn# <> aftUPphn# or
                bfrUPusnm   <> aftUPusnm ;

                  ##flgCMS = 'Y';
               Else;
                  ##flgCMS = 'N';
               EndIf;
            Else;
               ##flgCMS = 'Y';
            EndIf;
            If isChange = *On or tEvent <> '3';
              SubmitIsoJob();
            EndIf;
         EndIf;
         Close wmcml4;
      /End-Free
     p ProcessSMUPP    e
       //*********************************************************
       //Process WDELP changes
       //*********************************************************
     p ProcessWDELP    b
     d ProcessWDELP    pi
      /free
         ptrWDELPbfr = %addr(tgbufds) + tgbfrofs;
         ptrWDELPaft = %addr(tgbufds) + tgaftofs;

         Select;
            // Insert Event  & Update Event
            When tEvent = '1' or tEvent = '3';
               ##Co#      =   aftELCo#;
               ##fnd      =   aftELfnd;

            // Delete Event
            When tEvent = '2';
               ##Co#      =   bfrELCo#;
               ##fnd      =   bfrELfnd;
        EndSl;

        // Check Group is Active and not a Test Group and GE to 300.
        If TestGrp(##Co#:##Fnd) = *off and ExpiredGrp(##Co#:##Fnd) = *Off and
           ##Fnd >= 300 ;

           // Has the policy year changed
           If tEvent = '3' and bfrELfYr <> aftELfYr;
              isChange = *On;
           EndIf;

           // Submit ISO job
           If isChange = *On or tEvent <> '3';
              SubmitIsoJob();
           EndIf;
        EndIf;

      /End-Free
     p ProcessWDELP    e

       //*********************************************************
       //Process WDF3P changes
       //*********************************************************
     p ProcessWDF3P    b
     d ProcessWDF3P    pi
      /free
         ptrWDF3Pbfr = %addr(tgbufds) + tgbfrofs;
         ptrWDF3Paft = %addr(tgbufds) + tgaftofs;

         Select;
            // Insert Event  or Update Event
            When tEvent = '1' or tEvent = '3';
               ##Co# = aftF3co#;
               ##fnd = aftF3fnd;

            // Delete Event
            When tEvent = '2';
               ##Co# = bfrF3co#;
               ##fnd = bfrF3fnd;
         EndSl;

         // Check Group is Active and not a Test Group and GE to 300.
         If TestGrp(##Co#:##Fnd) = *off and ExpiredGrp(##Co#:##Fnd) = *Off and
            ##Fnd >= 300 ;

            If not %open(wmcml);
               Open wmcml;
            EndIf;
            // Process the WDF3P changes
            Chain (##co#:##fnd) pcm00;
            If %Found (wmcml);
               ##Co#       =  CMCo#;
               ##fnd       =  CMfnd;
               ##EmpOrEob  =  CMEmp#;
               ##DivOrSub  =  CMDiv;
               ##Fyr       =  CMFyr;
               ##Case      =  CMCase;
            EndIf;

            // Check bfr & Aft images fields value for any change
            If tEvent = '3' and
             (bfrF3adr1  <> aftF3adr1 or bfrF3name <> aftF3name  or
             bfrF3adr2  <> aftF3adr2 or  bfrF3cty  <> aftF3cty   or
             bfrF3st    <> aftF3st   or  bfrF3zip  <> aftF3zip );

               isChange = *On;
            EndIf;

            If isChange = *On or tEvent <> '3';
               SubmitIsoJob();
            EndIf;
            Close wmcml;
         EndIf;
      /End-Free
     p ProcessWDF3P    e
       //*********************************************************
       //Process WMALP changes
       //*********************************************************
     p ProcessWMALP    b
     d ProcessWMALP    pi
      /free
         ptrWMALPbfr = %addr(tgbufds) + tgbfrofs;
         ptrWMALPaft = %addr(tgbufds) + tgaftofs;
         RtvQteLib('*LIV':outLib);

         If tglib  = outLib;
            Select;
            When tEvent = '2' or tEvent = '3';
               ##Co#      =   bfrAlCo#;
               ##Fnd      =   bfrAlfnd;
               ##EmpOrEob =   bfrAlEmp#;
               ##DivOrSub =   bfrAlDiv;
               ##Elid     =   bfrAlElId;
            When tEvent = '1';
               ##Co#      =   aftAlCo#;
               ##Fnd      =   aftAlfnd;
               ##EmpOrEob =   aftAlEmp#;
               ##DivOrSub =   aftAlDiv;
               ##Elid     =   aftALElId;
            Endsl;

            RtvQteLib('*QTE':QteLib);
            clear strCommand;
            clear AlpDs1;

            //Check for Employer master existance
            strCommand = 'OVRDBF FILE(WMEMLQ) TOFILE(' +
            %trim(QteLib) + '/WMEMLQ) SHARE(*YES)';
            QCMDEXC(strCommand:%Len(strCommand));

            If not %Open(WMEMLQ);
               Open WMEMLQ;
            Endif;
            //*** Beg Chg *** 12/16/22 *****************************************
            //***Chain (##Co#:##Fnd:##EmpOrEob:##DivOrSub) WMEMLQ;
            Chain(n) (##Co#:##Fnd:##EmpOrEob:##DivOrSub) WMEMLQ;
            //*** End Chg *** 12/16/22 *****************************************
            If %found(WMEMLQ);

               strCommand = 'OVRDBF FILE(WMALLQ) TOFILE(' +
               %trim(QteLib) + '/WMALLQ) SHARE(*YES)';
               QCMDEXC(strCommand:%Len(strCommand));

               If not %Open(WMALLQ);
                  Open WMALLQ;
               Endif;

               //Find Quote Rec to be updated
               Chain (##Co#:##Fnd:##EmpOrEob:##DivOrSub:##Elid) WMALLQ;
               If %found (WMALLQ);
                  Select;
                  When tEvent = '3';
              //*** begin add ***  11/02/22  ********************************
                     aftALiusr = ALiusr;
                     aftALipgm = ALipgm;
                     aftALidt  = ALidt;
                     aftALitm  = ALitm;
              //*** end add *****  11/02/22  ********************************
                     AlpDs1 = WMALP_after;
                     Update PAL01;
                  When tEvent = '2';
                     Delete PAL01;
                  Endsl;
               Else;
                  If tEvent = '1';
                     AlpDs1 = WMALP_after;
                     Write PAL01;
                  Endif;
               Endif;

            Endif;
         Endif;

        //Close Opened Quote Files
        If %Open(WMALLQ);
           Close WMALLQ;
           strCommand = 'DLTOVR FILE(WMALLQ)';
           QCMDEXC(strCommand:%len(strCommand));
        Endif;

        If %Open(WMEMLQ);
           Close WMEMLQ;
           strCommand = 'DLTOVR FILE(WMEMLQ)';
           QCMDEXC(strCommand:%len(strCommand));
        Endif;
        //*** Begin Add ***  03/04/24  *******************************      *
        If %Open(WMEMLQ);
           Unlock WMEMLQ;
        Endif;
        //*** End   Add ***  03/04/24  *******************************      *
      /End-Free
     p ProcessWMALP    e
       //*********************************************************
       //Process WMA3P changes
       //*********************************************************
     p ProcessWMA3P    b
     d ProcessWMA3P    pi
      /free
         ptrWMA3Pbfr = %addr(tgbufds) + tgbfrofs;
         ptrWMA3Paft = %addr(tgbufds) + tgaftofs;
         RtvQteLib('*LIV':outLib);

         If tglib = outLib;

            Select;
            When tEvent = '2' or tEvent = '3';
               ##Co#      =   bfrA3Co#;
               ##Fnd      =   bfrA3fnd;
               ##EmpOrEob =   bfrA3Emp#;
               ##DivOrSub =   bfrA3Div;
            When tEvent = '1';
               ##Co#      =   aftA3Co#;
               ##Fnd      =   aftA3fnd;
               ##EmpOrEob =   aftA3Emp#;
               ##DivOrSub =   aftA3Div;
            Endsl;

            RtvQteLib('*QTE':QteLib);
            clear strCommand;
            clear A3pDs1;

            //Check for Employer master existance
            strCommand = 'OVRDBF FILE(WMEMLQ) TOFILE(' +
            %trim(QteLib) + '/WMEMLQ) SHARE(*YES)';
            QCMDEXC(strCommand:%Len(strCommand));

            If not %Open(WMEMLQ);
               Open WMEMLQ;
            Endif;

            //*** Beg Chg *** 12/16/22 *****************************************
            //***Chain (##Co#:##Fnd:##EmpOrEob:##DivOrSub) WMEMLQ;
            Chain(n) (##Co#:##Fnd:##EmpOrEob:##DivOrSub) WMEMLQ;
            //*** End Chg *** 12/16/22 *****************************************
            If %found(WMEMLQ);

               strCommand = 'OVRDBF FILE(WMA3LQ) TOFILE(' +
               %trim(QteLib) + '/WMA3LQ) SHARE(*YES)';
               QCMDEXC(strCommand:%Len(strCommand));

               If not %Open(WMA3LQ);
                  Open WMA3LQ;
               Endif;

               Chain (##Co#:##Fnd:##EmpOrEob:##DivOrSub) WMA3LQ;

               If %found(WMA3LQ);
                  Select;
                  When tEvent = '3';
              //*** begin add ***  11/02/22  ********************************
                     aftA3iusr = A3iusr;
                     aftA3ipgm = A3ipgm;
                     aftA3idt  = A3idt;
                     aftA3itm  = A3itm;
              //*** end add *****  11/02/22  ********************************
                     A3pDs1 = WMA3P_after;
                     Update PA301;
                  When tEvent = '2';
                     Delete PA301;
                  Endsl;

               Else;
                  If tEvent = '1';
                     A3pDs1 = WMA3P_after;
                     Write  PA301;
                  Endif;
               Endif;

            Endif;
         Endif;

        //Close Opened Quote Files
        If %Open(WMA3LQ);
           Close WMA3LQ;
           strCommand = 'DLTOVR FILE(WMA3LQ)';
           QCMDEXC(strCommand:%len(strCommand));
        Endif;

        If %Open(WMEMLQ);
           Close WMEMLQ;
           strCommand = 'DLTOVR FILE(WMEMLQ)';
           QCMDEXC(strCommand:%len(strCommand));
        Endif;
        //*** Begin Add ***  03/04/24  *******************************      *
        If %Open(WMEMLQ);
           Unlock WMEMLQ;
        Endif;
        //*** End   Add ***  03/04/24  *******************************      *
      /End-Free
     p ProcessWMA3P    e

       //*********************************************************
       //Process WMCDP changes
       //*********************************************************
     p ProcessWMCDP    b
     d ProcessWMCDP    pi
      /free
         ptrWMCDPbfr = %addr(tgbufds) + tgbfrofs;
         ptrWMCDPaft = %addr(tgbufds) + tgaftofs;

         Select;
            // Insert Event  & Update Event
            When tEvent = '1' or  tEvent = '3';
               strSS#  =   aftCDss#;

            // Delete Event
            When tEvent = '2';
               strSS#  =   bfrCDss#;
         EndSl;

         If not %open(wmcmla);
            Open wmcmla;
         EndIf;
         Chain (strSS#) pcm0A;
         If %found (wmcmla);
            ##Co#       =  CMCo#;
            ##fnd       =  CMfnd;
            ##EmpOrEob  =  CMEmp#;
            ##DivOrSub  =  CMDiv;
            ##Fyr       =  CMFyr;
            ##Case      =  CMCase;

            // Check bfr & Aft images fields value for any change
            If tEvent = '3' and
             ( bfrCDzip  <> aftCDzip  or bfrCDmsts <> aftCDmsts or
             bfrCDsex  <> aftCDsex  or bfrCDadr1 <> aftCDadr1 or
             bfrCDcty  <> aftCDcty  or bfrCDadr2 <> aftCDadr2 or

             bfrCDst   <> aftCDst   or bfrCDdtob <> aftCDdtob or
             bfrCDhicn <> aftCDhicn);

               isChange = *On;
            EndIf;
            //Determine to send medical  Bills
            If tEvent = '3';
               If bfrCDss#  <> aftCDss#;
                  ##Med = 'M';
               EndIf;
            EndIf;

           //***** Beg Add *** 08/19/16 ************************
            If tEvent = '3' ;
               If bfrCDadr1 <> aftCDadr1 or bfrCDadr2 <> aftCDadr2 or
                bfrCDcty    <> aftCDcty  or bfrCDzip  <> aftCDzip  or
                bfrCDst     <> aftCDst   or bfrCDdtob <> aftCDdtob or
                bfrCDhicn   <> aftCDhicn or bfrCDsex  <> aftCDsex;

                  ##flgCMS = 'Y';
               Else;
                  ##flgCMS = 'N';
               EndIf;
            Else;
               ##flgCMS = 'Y';
            EndIf;
           //***** End Add *** 08/19/16 ************************

            If isChange = *On or tEvent <> '3';
               SubmitIsoJob();
            EndIf;
         EndIf;
         Close wmcmla;
      /End-Free
     p ProcessWMCDP    e

       //*********************************************************
       //Process WMCMP changes
       //*********************************************************
     p ProcessWMCMP    b
     d ProcessWMCMP    pi
      /free
         ptrWMCMPbfr = %addr(tgbufds) + tgbfrofs;
         ptrWMCMPaft = %addr(tgbufds) + tgaftofs;
         isScoreUpdate = *Off;

         Select;
            // Insert Event  & Update Event
            When tevent = '1' or  tEvent = '3';
              ##Co#       =   aftCMCo#;
              ##fnd       =   aftCMfnd;
              ##EmpOrEob  =   aftCMEmp#;
              ##DivOrSub  =   aftCMDiv;
              ##Fyr       =   aftCMFyr;
              ##Case      =   aftCMCase;

            // Delete Event
            When tEvent = '2';
              ##Co#      =   bfrCMCo#;
              ##fnd      =   bfrCMfnd;
              ##EmpOrEob =   bfrCMEmp#;
              ##DivOrSub =   bfrCMDiv;
              ##Fyr      =   bfrCMFyr;
              ##Case     =   bfrCMCase;
         EndSl;

         // Check Group is Active and not a Test Group and GE to 300.
         If TestGrp(##Co#:##Fnd) = *off and ExpiredGrp(##Co#:##Fnd) = *Off and
            ##Fnd >= 300 ;

            // Check bfr & Aft images fields value for any change
            If tEvent = '3' and  isOrgSubmit <> 'Y'  and
             (bfrCMlsn <> aftCMlsn  or  bfrCMmid <> aftCMmid    or
             bfrCMfsn  <> aftCMfsn  or  bfrCMeid <> aftCMeid    or
             bfrCMeitq <> aftCMeitq or  bfrCMidth <> aftCMidth  or
             bfrCMdod  <> aftCMdod  or  bfrCMhidt <> aftCMhidt  or
             bfrCMacdt <> aftCMacdt or  bfrCMactm <> aftCMactm  or
             bfrCMast  <> aftCMast  or  bfrCMaccy <> aftCMaccy  or
             bfrCMacdr <> aftCMacdr or  bfrCMacty <> aftCMacty  or
             bfrCMacz1 <> aftCMacz1 or  bfrCMpart <> aftCMpart  or
             bfrCMcaus <> aftCMcaus or  bfrCMnatr <> aftCMnatr  or
             bfrCMclas <> aftCMclas or  bfrCMdays <> aftCMdays  or
             bfrCMaww  <> aftCMaww  or  bfrCMdtrp <> aftCMdtrp  or
             bfrCMrcvd <> aftCMrcvd or  bfrCMclex <> aftCMclex  or
             bfrCMco#  <> aftCMco#  or  bfrCMfnd  <> aftCMfnd   or
             bfrCMfyr  <> aftCMfyr  or  bfrCMcase <> aftCMcase  or
             bfrCMrt01 <> aftCMrt01 or  bfrCMrt02 <> aftCMrt02  or
             bfrCMrt03 <> aftCMrt03 or  bfrCMrt04 <> aftCMrt04  or
             bfrCMrt05 <> aftCMrt05 or  bfrCMrt06 <> aftCMrt06  or
             bfrCMrt07 <> aftCMrt07 or  bfrCMrt08 <> aftCMrt08  or
             bfrCMrt09 <> aftCMrt09 or  bfrCMrt10 <> aftCMrt10  or
             bfrCMar01 <> aftCMar01 or  bfrCMar02 <> aftCMar02  or
             bfrCMar03 <> aftCMar03 or  bfrCMar04 <> aftCMar04  or
             bfrCMar05 <> aftCMar05 or  bfrCMar06 <> aftCMar06  or
             bfrCMar07 <> aftCMar07 or  bfrCMar08 <> aftCMar08  or
             bfrCMar09 <> aftCMar09 or  bfrCMar10 <> aftCMar10  or
             bfrCMdtlw <> aftCMdtlw or  bfrCMrtw  <> aftCMrtw   or
             bfrCMcdlw <> aftCMcdlw or  bfrCMsame <> aftCMsame  or
             bfrCMdbdt <> aftCMdbdt or  bfrCMcddb <> aftCMcddb  or
             bfrCMprww <> aftCMprww or  bfrCMwgpd <> aftCMwgpd  or
             bfrCMstat <> aftCMstat or  bfrCMwgpd <> aftCMwgpd  or
             bfrCMemp# <> aftCMemp# or  bfrCMdiv  <> aftCMdiv   or
             bfrCMcmdi <> aftCMcmdi or  bfrCMjrst <> aftCMjrst  or
             bfrCMorm  <> aftCMorm  or  bfrCMss#  <> aftCMss#   or
             bfrCMormd <> aftCMormd );

               isChange = *On;
            Else;
            If tEvent = '3';
               If (bfrCMisoi <> aftCMisoi or bfrCMisop <> aftCMisop or
                  bfrCMisoc <> aftCMisoc) or aftCMuser = ISusrPrf;
                  isScoreUpdate = *On;
               EndIf;
            EndIf;
            EndIf;
            // Determine to send medical data with claim
            If tEvent = '3';
               If (bfrCMacdt <> aftCMacdt or
                bfrCMco#  <> aftCMco# or bfrCMfnd  <> aftCMfnd  or
                bfrCMfyr  <> aftCMfyr or bfrCMcase <> aftCMcase or
                isOrgSubmit = 'Y');
                  ##Med = 'M';
               EndIf;
            EndIf;

            If tEvent = '3' ;
               If bfrCMco#  <> aftCMco#  or bfrCMfnd  <> aftCMfnd  or
                bfrCMfyr    <> aftCMfyr  or bfrCMcase <> aftCMcase or
                bfrCMcmdi   <> aftCMcmdi or bfrCMjrst <> aftCMjrst or
                bfrCMorm    <> aftCMorm  or bfrCMss#  <> aftCMss#  or
                bfrCMfsn    <> aftCMfsn  or bfrCMlsn  <> aftCMlsn  or
                bfrCMormd   <> aftCMormd or bfrCMacdt <> aftCMacdt or
                bfrCMstat   <> aftCMstat or bfrCMdtrp <> aftCMdtrp or
                bfrCMmid    <> aftCMmid  or isOrgSubmit = 'Y';

                  ##flgCMS = 'Y';
               Else;
                  ##flgCMS = 'N';
               EndIf;
            Else;
               ##flgCMS = 'Y';
            EndIf;

            If isChange = *On or isOrgSubmit = 'Y' or tEvent <> '3';
               SubmitIsoJob();
            EndIf;
         EndIf;
      /End-Free
     p ProcessWMCMP    e

       //*********************************************************
       //Process WMECP changes
       //*********************************************************
     p ProcessWMECP    b
     d ProcessWMECP    pi
      /free
         ptrWMECPbfr = %addr(tgbufds) + tgbfrofs;
         ptrWMECPaft = %addr(tgbufds) + tgaftofs;
         RtvQteLib('*LIV':outLib);

         If tglib = outLib;
            Select;
            When tEvent = '2' or tEvent = '3';
               ##Co#      =   bfrECCo#;
               ##Fnd      =   bfrECfnd;
               ##EmpOrEob =   bfrECEmp#;
               ##DivOrSub =   bfrECDiv;
               ##Lsn      =   bfrECLsn;
               ##Fsn      =   bfrECFsn;
               ##Mid      =   bfrECMid;
               ##PosN     =   bfrECPosN;
               ##Pcnt     =   bfrECPcnt;
            When tEvent = '1';
               ##Co#      =   aftECCo#;
               ##Fnd      =   aftECfnd;
               ##EmpOrEob =   aftECEmp#;
               ##DivOrSub =   aftECDiv;
               ##Lsn      =   aftECLsn;
               ##Fsn      =   aftECFsn;
               ##Mid      =   aftECMid;
               ##PosN     =   aftECPosN;
               ##Pcnt     =   aftECPcnt;
            Endsl;

            RtvQteLib('*QTE':QteLib);
            clear strCommand;
            clear ECpDs1;

            //Check for Employer master existance
            strCommand = 'OVRDBF FILE(WMEMLQ) TOFILE(' +
            %trim(QteLib) + '/WMEMLQ) SHARE(*YES)';
            QCMDEXC(strCommand:%Len(strCommand));

            If not %Open(WMEMLQ);
               Open WMEMLQ;
            Endif;

            //*** Beg Chg *** 12/16/22 *****************************************
            //***Chain (##Co#:##Fnd:##EmpOrEob:##DivOrSub) WMEMLQ;
            Chain(n) (##Co#:##Fnd:##EmpOrEob:##DivOrSub) WMEMLQ;
            //*** End Chg *** 12/16/22 *****************************************
            If %found(WMEMLQ);

            //*** Beg Del *** 03/17/22 ****************************************
            //***   strCommand = 'OVRDBF FILE(WMECLQ) TOFILE(' +
            //***   %trim(QteLib) + '/WMECLQ) SHARE(*YES)';
            //***   QCMDEXC(strCommand:%Len(strCommand));

            //***   If not %Open(WMECLQ);
            //***      Open WMECLQ;
            //***   Endif;

            //***   Chain (##Co#:##Fnd:##EmpOrEob:##DivOrSub:##Lsn:##Fsn:##Mid:
            //***          ##PosN:##Pcnt) WMECLQ ;

            //***   If %found(WMECLQ);

            //***      Select;
            //***      When tEvent = '3';
            //***         ECpDs1 = WMECP_after;
            //***         Update PEC01;
            //***      When tEvent = '2';
            //***         Delete PEC01;
            //***      Endsl;

            //***   Else;
            //***      If tEvent = '1';
            //***         ECpDs1 = WMECP_after;
            //***         Write PEC01;
            //***      Endif;
            //***   Endif;
            //*** End Del *** 03/17/22 ****************************************

            Endif;
         Endif;

         //Close Opened Quote Files
         //*** Beg Del *** 03/17/22 ****************************************
         //***If %Open(WMECLQ);
         //***   Close WMECLQ;
         //***   strCommand = 'DLTOVR FILE(WMECLQ)';
         //***   QCMDEXC(strCommand:%len(strCommand));
         //***Endif;
         //*** End Del *** 03/17/22 ****************************************

        If %Open(WMEMLQ);
           Close WMEMLQ;
           strCommand = 'DLTOVR FILE(WMEMLQ)';
           QCMDEXC(strCommand:%len(strCommand));
        Endif;
        //*** Begin Add ***  03/04/24  *******************************      *
        If %Open(WMEMLQ);
           Unlock WMEMLQ;
        Endif;
        //*** End   Add ***  03/04/24  *******************************      *
      /End-Free
     p ProcessWMECP    e
       //*********************************************************
       //Process WMEMP changes
       //*********************************************************
     p ProcessWMEMP    b
     d ProcessWMEMP    pi
      /free
         //*** Begin Add ***  03/04/24  *******************************     *
         If %Open(WMEMLQ);
            Unlock WMEMLQ;
         Endif;
         //*** End   Add ***  03/04/24  *******************************     *
         ptrWMEMPbfr = %addr(tgbufds) + tgbfrofs;
         ptrWMEMPaft = %addr(tgbufds) + tgaftofs;

         Select;
            // Insert Event  & Update Event
            When tEvent = '1' or tEvent = '3';
               ##Co#      =   aftEMCo#;
               ##fnd      =   aftEMfnd;
               ##EmpOrEob =   aftEMEmp#;
               ##DivOrSub =   aftEMDiv;

            // Delete Event
            When tEvent = '2';
               ##Co#      =   bfrEMCo#;
               ##fnd      =   bfrEMfnd;
               ##EmpOrEob =   bfrEMEmp#;
               ##DivOrSub =   bfrEMDiv;
        EndSl;

        // Check Group is Active and not a Test Group and GE to 300.
        If TestGrp(##Co#:##Fnd) = *off and ExpiredGrp(##Co#:##Fnd) = *Off and
           ##Fnd >= 300 ;

           // Process the WMEMP changes
           // Check bfr & Aft images fields value for any change

           If tEvent = '3' and
              (bfrEMnaic <> aftEMnaic or bfrEMname <> aftEMname  or
               bfrEMfein <> aftEMfein or bfrEMmst  <> aftEMmst   or
               bfrEMmcty <> aftEMmcty or bfrEMpzip <> aftEMpzip  or
               bfrEMmad1 <> aftEMmad1 or bfrEMmad2 <> aftEMmad2  or
               bfrEMmzip <> aftEMmzip or bfrEMrgn# <> aftEMrgn#  or
               bfrEMco#  <> aftEMco#  or bfrEMfnd  <> aftEMfnd   or
               bfrEMemp# <> aftEMemp# or bfrEMdiv  <> aftEMdiv   or
               bfrEMnam2 <> aftEMnam2 or bfrEMfdnm <> aftEMfdnm  or
               bfrEMfdn2 <> aftEMfdn2 or bfrEMemp# <> aftEMemp#  );

              isChange = *On;
           EndIf;

           If tEvent = '3' ;
              If bfrEMname <> aftEMname or  bfrEMfdn2 <> aftEMfdn2 or
               bfrEMco#    <> aftEMco#  or  bfrEMfnd  <> aftEMfnd  or
               bfrEMemp#   <> aftEMemp# or  bfrEMdiv  <> aftEMdiv  or
               bfrEMnam2   <> aftEMnam2 or  bfrEMfdnm <> aftEMfdnm  ;
                 ##flgCMS = 'Y';
              Else;
                 ##flgCMS = 'N';
              EndIf;
           Else;
              ##flgCMS = 'Y';
           EndIf;

           If isChange = *On or tEvent <> '3';
              SubmitIsoJob();
           EndIf;
        EndIf;

           RtvQteLib('*LIV':outLib);
           //*** Beg Chg *** 05/22/24 **********************************
           //***If tglib = outLib;
           // If transferring to quote then skip this logic
           If tgLib = outLib and Not pgmInStack('W8700R');
           //*** End Chg *** 05/22/24 **********************************
              If tEvent = '2' or tEvent = '3';
                 ##Co#      =   bfrEmCo#;
                 ##Fnd      =   bfrEmfnd;
                 ##EmpOrEob =   bfrEmEmp#;
                 ##DivOrSub =   bfrEmDiv;
              Endif;

              RtvQteLib('*QTE':QteLib);
              clear strCommand;
              clear EMpDs1;

              strCommand = 'OVRDBF FILE(WMEMLQ) TOFILE(' +
              %trim(QteLib) + '/WMEMLQ) SHARE(*YES)';
              QCMDEXC(strCommand:%Len(strCommand));

              If not %Open(WMEMLQ);
                 Open WMEMLQ;
                 Setll (##Co#:##Fnd:##EmpOrEob:##DivOrSub) WMEMLQ;
                 Reade (##Co#:##Fnd:##EmpOrEob:##DivOrSub) WMEMLQ;
                 Dow not %eof(WMEMLQ);

                     Select;
                     When tEvent = '3';
                        // Do not update fields - reset from quote
                        aftEMrQFl = qEMrQfl;
                        aftEMqTfr = qEMqTfr;
                        aftEMcaD1 = qEMcaD1;
                 //*** begin add ***  11/02/22  ********************************
                        aftEMiusr = qEMiusr;
                        aftEMipgm = qEMipgm;
                        aftEMidt  = qEMidt;
                        aftEMitim = qEMitim;
                 //*** end add *****  11/02/22  ********************************
                        EMpDs1 = WMEMP_after;
                        Update PEM01Q;
                     When tEvent = '2';
                        Delete PEM01Q;
                     //*** Beg Add *** 03/01/24 ****************************
                     Other;
                        Unlock WMEMLQ;
                     //*** End Add *** 03/01/24 ****************************
                     Endsl;

                    Reade (##Co#:##Fnd:##EmpOrEob:##DivOrSub) WMEMLQ;
                 Enddo;
              Endif;

           Endif;

           //Close Opened Quote Files
           If %Open(WMEMLQ);
              Close WMEMLQ;
              strCommand = 'DLTOVR FILE(WMEMLQ)';
              QCMDEXC(strCommand:%len(strCommand));
           Endif;
           //*** Begin Add ***  03/04/24  *******************************   *
           If %Open(WMEMLQ);
              Unlock WMEMLQ;
           Endif;
           //*** End   Add ***  03/04/24  *******************************   *
      /End-Free
     p ProcessWMEMP    e

       //*********************************************************
       //Process WMFNP changes
       //*********************************************************
     p ProcessWMFNP    b
     d ProcessWMFNP    pi
      /free
         ptrWMFNPbfr = %addr(tgbufds) + tgbfrofs;
         ptrWMFNPaft = %addr(tgbufds) + tgaftofs;

         Select;
            // Insert Event  or Update Event
            When tEvent = '1' or tEvent = '3';
               ##Co# = aftFNco#;
               ##fnd = aftFNfnd;

            // Delete Event
            When tEvent = '2';
               ##Co# = bfrFNco#;
               ##fnd = bfrFNfnd;
         EndSl;

         If TestGrp(##Co#:##Fnd) = *off and ExpiredGrp(##Co#:##Fnd) = *Off and
            ##Fnd >= 300 ;
            If not %open(wmcml);
               Open wmcml;
            EndIf;
         // Process the WMFNP changes
            Chain (##co#:##fnd) pcm00;
            If %Found (wmcml);
               ##Co#       =  CMCo#;
               ##fnd       =  CMfnd;
               ##EmpOrEob  =  CMEmp#;
               ##DivOrSub  =  CMDiv;
               ##Fyr       =  CMFyr;
               ##Case      =  CMCase;
            EndIf;

            // Check bfr & Aft images fields value for any change
            If tEvent = '3' and
             (bfrFNpad1 <> aftFNpad1  or bfrFNpad2 <> aftFNpad2  or
             bfrFNpcty  <> aftFNpcty or  bfrFNpst  <> aftFNpst   or
             bfrFNpzip  <> aftFNpzip or  bfrFNname <> aftFNname  or
             bfrFNmad1  <> aftFNmad1 or  bfrFNmad2 <> aftFNmad2  or
             bfrFNmcty  <> aftFNmcty or  bfrFNmst  <> aftFNmst   or
             bfrFNmzip  <> aftFNmzip or  bfrFNsfin  <> aftFNsfin );

               isChange = *On;
            EndIf;

            If tEvent = '3';
               If bfrFNname <> aftFNname or
                bfrFNmad1   <> aftFNmad1 or  bfrFNmad2 <> aftFNmad2  or
                bfrFNmcty   <> aftFNmcty or  bfrFNmst  <> aftFNmst   or
                bfrFNmzip   <> aftFNmzip or  bfrFNsfin  <> aftFNsfin ;
                  ##flgCMS = 'Y';
               Else;
                  ##flgCMS = 'N';
               EndIf;
            Else;
               ##flgCMS = 'Y';
            EndIf;

            If isChange = *On or tEvent <> '3';
               SubmitIsoJob();
            EndIf;
            Close wmcml;
         EndIf;
      /End-Free
     p ProcessWMFNP    e

       //*********************************************************
       //Process WMOOP changes
       //*********************************************************
     p ProcessWMOOP    b
     d ProcessWMOOP    pi
      /free
         ptrWMOOPbfr = %addr(tgbufds) + tgbfrofs;
         ptrWMOOPaft = %addr(tgbufds) + tgaftofs;
         RtvQteLib('*LIV':outLib);

         If tglib = outLib;

            Select;
            When tEvent = '2' or tEvent = '3';
               ##Co#      =   bfrOOCo#;
               ##Fnd      =   bfrOOfnd;
               ##EmpOrEob =   bfrOOEmp#;
               ##DivOrSub =   bfrOODiv;
               ##Lsn      =   bfrOOOlsn;
               ##Fsn      =   bfrOOOfsn;
            When tEvent = '1';
               ##Co#      =   aftOOCo#;
               ##Fnd      =   aftOOfnd;
               ##EmpOrEob =   aftOOEmp#;
               ##DivOrSub =   aftOODiv;
               ##Lsn      =   aftOOOLsn;
               ##Fsn      =   aftOOOFsn;
            Endsl;

            RtvQteLib('*QTE':QteLib);
            clear strCommand;
            clear OOpDs1;

            //Check for Employer master existance
            strCommand = 'OVRDBF FILE(WMEMLQ) TOFILE(' +
            %trim(QteLib) + '/WMEMLQ) SHARE(*YES)';
            QCMDEXC(strCommand:%Len(strCommand));

            If not %Open(WMEMLQ);
               Open WMEMLQ;
            Endif;

            //*** Beg Chg *** 12/16/22 *****************************************
            //***Chain (##Co#:##Fnd:##EmpOrEob:##DivOrSub) WMEMLQ;
            Chain(n) (##Co#:##Fnd:##EmpOrEob:##DivOrSub) WMEMLQ;
            //*** End Chg *** 12/16/22 *****************************************
            If %found(WMEMLQ);

               strCommand = 'OVRDBF FILE(wmool9) TOFILE(' +
               %trim(QteLib) + '/wmool9) SHARE(*YES)';
               QCMDEXC(strCommand:%Len(strCommand));

               If not %Open(wmool9);
                  Open wmool9;
               Endif;

               Chain (##Co#:##Fnd:##EmpOrEob:##DivOrSub:##Lsn:##Fsn) wmool9;
               If %found(wmool9);
                  Select;
                  When tEvent = '3';
              //*** begin add ***  11/02/22  ********************************
                     aftOOiusr = OOiusr;
                     aftOOipgm = OOipgm;
                     aftOOidt  = OOidt;
                     aftOOitim = OOitim;
              //*** end add *****  11/02/22  ********************************
                     OOpDs1 = WMOOP_after;
                     Update POO01;
                  When tEvent = '2';
                     Delete POO01;
                  Endsl;

               Else;
                  If tEvent = '1';
                     OOpDs1 = WMOOP_after;
                     Write  POO01;
                  Endif;
               Endif;

            Endif;
         Endif;

         //Close Opened Quote Files
         If %Open(wmool9);
            Close wmool9;
            strCommand = 'DLTOVR FILE(wmool9)';
            QCMDEXC(strCommand:%len(strCommand));
         Endif;

         If %Open(WMEMLQ);
            Close WMEMLQ;
            strCommand = 'DLTOVR FILE(WMEMLQ)';
            QCMDEXC(strCommand:%len(strCommand));
         Endif;
         //*** Begin Add ***  03/04/24  *******************************     *
         If %Open(WMEMLQ);
            Unlock WMEMLQ;
         Endif;
         //*** End   Add ***  03/04/24  *******************************     *
      /End-Free
     p ProcessWMOOP    e
       //*********************************************************
       //Process WMRPP changes
       //*********************************************************
     p ProcessWMRPP    b
     d ProcessWMRPP    pi
      /Free
         ptrWMRPPbfr = %addr(tgbufds) + tgbfrofs;
         ptrWMRPPaft = %addr(tgbufds) + tgaftofs;

         Select;
            // Insert Event   or  // Update Event
            When tEvent = '1'  or tEvent = '3';
               ##Co#       =   aftRPCo#;
               ##fnd       =   aftRPfnd;
               ##EmpOrEob  =   aftRPemp#;
               ##DivOrSub  =   aftRPdiv;
               ##Fyr       =   aftRPfyr;

            // Delete Event
            When tEvent = '2';
               ##Co#       =   bfrRPCo#;
               ##fnd       =   bfrRPfnd;
               ##EmpOrEob  =   bfrRPemp#;
               ##DivOrSub  =   bfrRPdiv;
               ##Fyr       =   bfrRPfyr;
         EndSl;

         // Check Group is Active and not a Test Group and GE to 300.
         If TestGrp(##Co#:##Fnd) = *off and ExpiredGrp(##Co#:##Fnd) = *Off and
            ##Fnd >= 300 ;

            // Check bfr & Aft images fields value for any change
            If tEvent = '3' and
             ( bfrRPcap# <> aftRPcap# );

               isChange = *On;
            EndIf;

            If isChange = *On or tEvent <> '3';
               SubmitIsoJob();
            EndIf;
         EndIf;
      /End-Free
     p ProcessWMRPP    e

       //*********************************************************
       //Process WMWHP changes
       //*********************************************************
     p ProcessWMWHP    b
     d ProcessWMWHP    pi
      /free
         ptrWMWHPbfr = %addr(tgbufds) + tgbfrofs;
         ptrWMWHPaft = %addr(tgbufds) + tgaftofs;

         Select;
            // Insert Event     or  Update Event
            When tEvent = '1' or  tEvent = '3';
               ##Co#  = aftWHco#;
               ##Fnd  = aftWHfnd;
               ##Fyr  = aftWHfyr;
               ##Case = aftWHcase;

            // Delete Event
            When tEvent = '2';
               ##Co#  = bfrWHco#;
               ##Fnd  = bfrWHfnd;
               ##Fyr  = bfrWHfyr;
               ##Case = bfrWHcase;
         EndSl;

         // Check Group is Active and not a Test Group and GE to 300.
         If TestGrp(##Co#:##Fnd) = *off and ExpiredGrp(##Co#:##Fnd) = *Off and
            ##Fnd >= 300 ;

            // Process the WMWHP changes

            // Check bfr & Aft images fields value for any change
            If tEvent = '3' and ( bfrWHdate <> aftWHdate or
             bfrWHwkst  <> aftWHwkst or  bfrWHrtwq <> aftWHrtwq );

               isChange = *On;
            EndIf;

            If isChange = *On or tEvent <> '3';
               SubmitIsoJob();
            EndIf;
         EndIf;
      /End-Free
     p ProcessWMWHP    e

       //*********************************************************
       //Process WTCNP changes
       //*********************************************************
     p ProcessWTCNP    b
     d ProcessWTCNP    pi
      /free
         ptrWTCNPbfr = %addr(tgbufds) + tgbfrofs;
         ptrWTCNPaft = %addr(tgbufds) + tgaftofs;

         Select;
            // Insert Event  & Update Event
            When tEvent = '1' or  tEvent = '3';
               ##Co#    =   aftCNco#;
               strCNkey =   aftCNkey;

            // Delete Event
            When tEvent = '2';
               ##Co#    =   bfrCNco#;
               strCNkey =   bfrCNkey;
         EndSl;

         If not %open(wmeml22);
            Open wmeml22;
         EndIf;
         strpst  = %Subst (strCNkey:1:2);
         strcnty = %Subst (strCNkey:3:2);
         Chain (##Co#:strpst:strcnty) pem22;
         If %Found (wmeml22);
            ##Co#      = EMco# ;
            ##Fnd      = EMfnd ;
            ##EmpOrEob = EMemp#;
            ##DivOrSub = EMdiv ;

            // Check bfr & Aft images fields value for any change
            If tEvent = '3' and  (bfrCNdesc <> aftCNdesc );
               isChange = *On;
            EndIf;

            If isChange = *On or tEvent <> '3';
               SubmitIsoJob();
            EndIf;
         EndIf;
         Close wmeml22;
      /End-Free
     p ProcessWTCNP    e

       //*********************************************************
       //Submit the Job to process file operation
       //*********************************************************
     p SubmitIsoJob    b
     d SubmitIsoJob    pi
      /free

        If ##Jobq <> *Blanks and not isScoreUpdate;
           ##File  = tgFile;
           ##Event = tEvent;


           ISOSbmCl(##Jobq:##File:##Event:##Co#:##Fnd:##EmpOrEob:##DivOrSub
            :##Fyr:##Case:##Med:##Prcd:##Seq:##Ofc#:##flgCMS);

        EndIf;
      /End-Free
     p SubmitIsoJob    e

       //*********************************************************
       //Process WDF2P changes
       //*********************************************************
     p ProcessWDF2P    b
     d ProcessWDF2P    pi
      /free
         ptrWDF2Pbfr = %addr(tgbufds) + tgbfrofs;
         ptrWDF2Paft = %addr(tgbufds) + tgaftofs;

         Select;
         // Insert or Update Event
         When tEvent = '1' or  tEvent = '3';
            ##Co#       =   aftF2co#;
            ##Fnd       =   aftF2fnd;

         // Delete Event
         When tEvent = '2';
            ##Co#      =   bfrF2co#;
            ##Fnd      =   bfrF2fnd;
         EndSl;

         // Check Group is Active and not a Test Group and GE to 300.
         If TestGrp(##Co#:##Fnd) = *off and ExpiredGrp(##Co#:##Fnd) = *Off and
            ##Fnd >= 300 ;

            // Check bfr & Aft images fields value for any change
            If tEvent = '3' and
             (bfrF2rre# <> aftF2rre# or  bfrF2fein <> aftF2fein);
               isChange = *On;
            EndIf;

            If isChange = *On or tEvent <> '3';
               SubmitIsoJob();
            EndIf;
         EndIf;
      /End-Free
     p ProcessWDF2P    e

       //*********************************************************
       //Process WMLLP changes
       //*********************************************************
     p ProcessWMLLP    b
     d ProcessWMLLP    pi
      /free

         ptrWMLLPbfr = %addr(tgbufds) + tgbfrofs;
         ptrWMLLPaft = %addr(tgbufds) + tgaftofs;

         Select;
         // Insert or Update Event
         When tEvent = '1' or  tEvent = '3';
            ##Co#       =   aftLLCo#  ;
            ##Fnd       =   aftLLFnd  ;
            ##Fyr       =   aftLLFyr  ;
            ##Case      =   aftLLCase ;

         // Delete Event
         When tEvent = '2';
            ##Co#      =   bfrLLCo#  ;
            ##Fnd      =   bfrLLFnd  ;
            ##Fyr      =   bfrLLFyr  ;
            ##Case     =   bfrLLCase ;
         EndSl;

         // Check Group is Active and not a Test Group and GE to 300.
         If TestGrp(##Co#:##Fnd) = *off and ExpiredGrp(##Co#:##Fnd) = *Off and
            ##Fnd >= 300 ;

            // Check bfr & Aft images fields value for any change
            If tEvent = '3' and
             (bfrLLclri<> aftLLclri or  bfrLLrfnm <> aftLLrfnm or
             bfrLLplaa <> aftLLplaa or  bfrLLplam <> aftLLplam or
             bfrLLpad1 <> aftLLpad1 or  bfrLLpad2 <> aftLLpad2 or
             bfrLLpst  <> aftLLpst  or  bfrLLpzip <> aftLLpzip or
             bfrLLsamt <> aftLLsamt or  bfrLLstdt <> aftLLstdt );

               isChange = *On;
            EndIf;

            If isChange = *On or tEvent <> '3';
               SubmitIsoJob();
            EndIf;
         EndIf;
      /End-Free
     p ProcessWMLLP    e

       //*********************************************************
       //Process WMPDP changes
       //*********************************************************
     p ProcessWMPDP    b
     d ProcessWMPDP    pi
      /free

         ptrWMPDPbfr = %addr(tgbufds) + tgbfrofs;
         ptrWMPDPaft = %addr(tgbufds) + tgaftofs;

         Select;
         // Insert or Update Event
         When tEvent = '1' or  tEvent = '3';
            intFein =   aftwPDfein;

         // Delete Event
         When tEvent = '2';
            intFein =   bfrwPDfein;
         EndSl;

         // Get Co# and Fund on the basis of FEIN
         If Not %Open(smcop);
            Open smcop;
         EndIf;
         If Not %Open(wmeml12);
            Open wmeml12;
         EndIf;
         Read pco0p;
         If Not %Eof(smcop);
            Chain (COco#:intFein) pem12;
            If %Found (wmeml12);
               ##Co#   =  EMCo#;
               ##fnd   =  EMfnd;
            EndIf;
         EndIf;

         If %Open(smcop);
            Close smcop;
         EndIf;
         If %Open(wmeml12);
            Close wmeml12;
         EndIf;

         // Check Group is Active and not a Test Group and GE to 300.
         If TestGrp(##Co#:##Fnd) = *off and ExpiredGrp(##Co#:##Fnd) = *Off and
            ##Fnd >= 300 ;

            // Check bfr & Aft images fields value for any change
            If tEvent = '3' and
             (bfrwPDfsn <> aftwPDfsn  or  bfrwPDname <> aftwPDname or
             bfrwPDadr1 <> aftwPDadr1 or  bfrwPDadr2 <> aftwPDadr2 or
             bfrwPDst   <> aftwPDst   or  bfrwPDzip  <> aftwPDzip  );

               isChange = *On;
            EndIf;

            If isChange = *On or tEvent <> '3';
               SubmitIsoJob();
            EndIf;
         EndIf;
      /End-Free
     p ProcessWMPDP    e

       //*********************************************************
       //Process WMOFP changes
       //*********************************************************
     p ProcessWMOFP    b
     d ProcessWMOFP    pi
      /free

         ptrWMOFPbfr = %addr(tgbufds) + tgbfrofs;
         ptrWMOFPaft = %addr(tgbufds) + tgaftofs;

         Select;
         // Insert or Update Event
         When tEvent = '1' or  tEvent = '3';
            ##Co#      =   aftOFco#  ;
            ##Ofc#     =   aftOFofc# ;

         // Delete Event
         When tEvent = '2';
            ##Co#      =   bfrOFco#  ;
            ##Ofc#     =   bfrOFofc# ;
         EndSl;

         // Check Group is Active and not a Test Group and GE to 300.
         If TestGrp(##Co#:##Fnd) = *off and ExpiredGrp(##Co#:##Fnd) = *Off and
            ##Fnd >= 300 ;

            // Check bfr & Aft images fields value for any change
            If tEvent = '3' and
             (bfrOFco#  <> aftOFco#  or bfrOFofc# <> aftOFofc# or
             bfrOFname  <> aftOFname or bfrOFadr2 <> aftOFadr2 or
             bfrOFcty   <> aftOFcty  or bfrOFst   <> aftOFst   or
             bfrOFzip   <> aftOFzip );

               isChange = *On;
            EndIf;

            If isChange = *On or tEvent <> '3';
               SubmitIsoJob();
            EndIf;
         EndIf;
      /End-Free
     p ProcessWMOFP    e

       //*********************************************************
       //Process SADAP changes
       //*********************************************************
     p ProcessSADAP    b
     d ProcessSADAP    pi
      /free

         ptrSADAPbfr = %addr(tgbufds) + tgbfrofs;
         ptrSADAPaft = %addr(tgbufds) + tgaftofs;

         Select;
         // Insert or Update Event
         When tEvent = '1' or  tEvent = '3';
            ##Co#       =   aftDAco#  ;
            ##Fnd       =   aftDAfnd  ;
            ##Fyr       =   aftDAfyr  ;
            ##Case      =   aftDAcase ;
            ##DivOrSub  =   aftDAsub# ;
            ##seq       =   aftDAseq# ;
            ##prcd      =   aftDAicd9 ;

         // Delete Event
         When tEvent = '2';
            ##Co#       =   bfrDAco#  ;
            ##Fnd       =   bfrDAfnd  ;
            ##Fyr       =   bfrDAfyr  ;
            ##Case      =   bfrDAcase ;
            ##DivOrSub  =   bfrDAsub# ;
            ##seq       =   bfrDAseq# ;
            ##prcd      =   bfrDAicd9 ;
         EndSl;

         // Check Group is Active and not a Test Group and GE to 300.
         If TestGrp(##Co#:##Fnd) = *off and ExpiredGrp(##Co#:##Fnd) = *Off and
            ##Fnd >= 300 ;

            // Check bfr & Aft images fields value for any change
            If tEvent = '3' and
             (bfrDAicd9  <> aftDAicd9 or bfrDAicd1001 <> aftDAicd1001 or
             bfrDAicdind <> aftDAicdind );

               isChange = *On;
            EndIf;

            If isChange = *On or tEvent <> '3';
               SubmitIsoJob();
            EndIf;
         EndIf;
      /End-Free
     p ProcessSADAP    e
