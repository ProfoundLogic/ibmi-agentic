      //*********************************************************************
     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      //
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     //*********************************************************************
     // Program: FA763RTX
     // Description: Texas API to return allowances for all localities
     //              along with description, indicators, etc.
     //*********************************************************************
      //                                                                    *
     //*********************************************************************
     //  Created: 06/21/18  C:79   Project: TX101
     //*********************************************************************
     //  REVISIONS:                                                        *
     //*********************************************************************
     //  INDICATOR USAGE:                                                  *
     //*********************************************************************
     // FILE SPECIFICATIONS:
     //*********************************************************************
     //
     // Medicare GPCI Table file
     //
     Fftgpl3    if   e           k disk
     //
     // Medicare RVU Table file
     //
     Fftrvl1    if   e           k disk
     //
     // Fee Schedule Rat table file
     //
     Fftrtl2    if   e           k disk
     //
     // EOB Master file
     //
     Ffaeolg    if   e           k disk
     //
     // Detail Charge file
     //
     Ffdcap     if   e             disk
     //
     // Physicain Detail File
     //
     Ffddhl     if   e           k disk
     //
     // Dental Detail File
     //
     Ffdddl     if   e           k disk
     //
     // Texas Procedure Code file
     //
     Ffmprltx   if   e           k disk
     //
     // Texas Medicaid DME file
     //
     Fftmel2tx  if   e           k disk
     //
     // Texas Medicaid Dental file
     //
     Fftdnl1tx  if   e           k disk

     d*********************************************************************
     d* Arrays
    d***********************************************************************
     d mod             s              2    dim(4) ascend
     d*********************************************************************
     d* Proto-types
    d***********************************************************************
    d*  Standalone fields
    d***********************************************************************
     d   xxPerc        s                   Like(RTmul3)
     d   xxPercDent    s                   Like(RTmul4)
     d   xxMod         s                   Like(Dhmod1)
     d* Procedure prototypes
    d***********************************************************************
     d GetMedicAllow   pr                  ExtPgm('FA763R')
     d  ##Rrn                         9  0
     d  ##Call                        1
     d  ##Eob                         3
     d  ##Msg                         7
     d  ##Modr                        1
     d  ##Rnd                         1
     d  ##Alw1                        9  2
     d  ##Alw2                        9  2
     d  ##Alw3                        9  2
     d  ##RtCd                        1
     d  ##Rrn          s              9  0
     d  ##Call         s              1
     d  ##Eob          s              3
     d  ##Msg          s              7
     d  ##Modr         s              1
     d  ##Rnd          s              1
     d  ##Alw1         s              9  2
     d  ##Alw2         s              9  2
     d  ##Alw3         s              9  2
     d  ##RtCd         s              1
     d*
    d***********************************************************************
    d*  Data Structures
    d***********************************************************************
     d psds          esds                  extname(szq1p)
    c**********************************************************************
    c*   Key Lists
    c**********************************************************************
     c**********************************************************************
     c* Entry parameters
     c**********************************************************************
     c     *entry        plist
     c                   parm                    @@Rrn             9 0
     c                   parm                    @@Desc          100
     c                   parm                    @@PrCd            5
     c                   parm                    @@Mod             2
     c                   parm                    @@EfDt            7 0
     c                   parm                    @@Loc9NonFac      9 2
     c                   parm                    @@Loc9Facil       9 2
     c                   parm                    @@Loc9Prof        9 2
     c                   parm                    @@Loc9Tech        9 2
     c                   parm                    @@Loc9Anes        9 2
     c                   parm                    @@Loc11NonFac     9 2
     c                   parm                    @@Loc11Facil      9 2
     c                   parm                    @@Loc11Prof       9 2
     c                   parm                    @@Loc11Tech       9 2
     c                   parm                    @@Loc11Anes       9 2
     c                   parm                    @@Loc15NonFac     9 2
     c                   parm                    @@Loc15Facil      9 2
     c                   parm                    @@Loc15Prof       9 2
     c                   parm                    @@Loc15Tech       9 2
     c                   parm                    @@Loc15anes       9 2
     c                   parm                    @@Loc18NonFac     9 2
     c                   parm                    @@Loc18Facil      9 2
     c                   parm                    @@Loc18Prof       9 2
     c                   parm                    @@Loc18Tech       9 2
     c                   parm                    @@Loc18anes       9 2
     c                   parm                    @@Loc20NonFac     9 2
     c                   parm                    @@Loc20Facil      9 2
     c                   parm                    @@Loc20Prof       9 2
     c                   parm                    @@Loc20Tech       9 2
     c                   parm                    @@Loc20anes       9 2
     c                   parm                    @@Loc28NonFac     9 2
     c                   parm                    @@Loc28Facil      9 2
     c                   parm                    @@Loc28Prof       9 2
     c                   parm                    @@Loc28Tech       9 2
     c                   parm                    @@Loc28anes       9 2
     c                   parm                    @@Loc31NonFac     9 2
     c                   parm                    @@Loc31Facil      9 2
     c                   parm                    @@Loc31Prof       9 2
     c                   parm                    @@Loc31Tech       9 2
     c                   parm                    @@Loc31anes       9 2
     c                   parm                    @@Loc99NonFac     9 2
     c                   parm                    @@Loc99Facil      9 2
     c                   parm                    @@Loc99Prof       9 2
     c                   parm                    @@Loc99Tech       9 2
     c                   parm                    @@Loc99anes       9 2
     c                   parm                    @@Loc9Used        9 2
     c                   parm                    @@Loc11Used       9 2
     c                   parm                    @@Loc15Used       9 2
     c                   parm                    @@Loc18Used       9 2
     c                   parm                    @@Loc20Used       9 2
     c                   parm                    @@Loc28Used       9 2
     c                   parm                    @@Loc31Used       9 2
     c                   parm                    @@Loc99Used       9 2
     c                   parm                    @@PcTc            1
     c                   parm                    @@Glob            3
     c                   parm                    @@PreOp           5 2
     c                   parm                    @@IntraOp         5 2
     c                   parm                    @@PostOp          5 2
     c                   parm                    @@MultSurg        1
     c                   parm                    @@Bilat           1
     c                   parm                    @@AsstSurg        1
     c                   parm                    @@CoSurg          1
     c                   parm                    @@TeamSurg        1
     c                   parm                    @@EndoBase        5
     c                   parm                    @@StatCd          1
      //===============================================================
      // Mainline
      //===============================================================
      /Free
       Callp ProgramStart();
       Callp GetCodeInfo();
       Select;
       When CAsrc = 'R';
          Callp GetRvLocAllow();
       When CAsrc = 'F';
          Callp GetDrugFeeAll();
       When CAsrc = 'D';
          Callp GetDmeAll();
       When CAsrc = 'M';
          Callp GetMedDmeAll();
       When CAsrc = 'T';
          Callp GetMedDentAll();
       When CAsrc = 'L';
          Callp GetClabAll();
       When CAsrc = 'A';
          Callp GetAmbulAll();
       Endsl;
       Callp GetCommLocAllw();
       Callp ProgramEnd();
      /End-free
       //===================================================================
       // InitFields - Init all fields being returned
       //===================================================================
     p InitFields      b
     d InitFields      pi
      /Free
        Clear @@Desc;
        Clear @@PrCd;
        Clear @@Mod;
        Clear @@EfDt;
        Clear @@Loc9NonFac;
        Clear @@Loc9Facil;
        Clear @@Loc9Prof;
        Clear @@Loc9Tech;
        Clear @@Loc9Anes;
        Clear @@Loc11NonFac;
        Clear @@Loc11Facil;
        Clear @@Loc11Prof;
        Clear @@Loc11Tech;
        Clear @@Loc11Anes;
        Clear @@Loc15NonFac;
        Clear @@Loc15Facil;
        Clear @@Loc15Prof;
        Clear @@Loc15Tech;
        Clear @@Loc15anes;
        Clear @@Loc18NonFac;
        Clear @@Loc18Facil;
        Clear @@Loc18Prof;
        Clear @@Loc18Tech;
        Clear @@Loc18anes;
        Clear @@Loc20NonFac;
        Clear @@Loc20Facil;
        Clear @@Loc20Prof;
        Clear @@Loc20Tech;
        Clear @@Loc20anes;
        Clear @@Loc28NonFac;
        Clear @@Loc28Facil;
        Clear @@Loc28Prof;
        Clear @@Loc28Tech;
        Clear @@Loc28anes;
        Clear @@Loc31NonFac;
        Clear @@Loc31Facil;
        Clear @@Loc31Prof;
        Clear @@Loc31Tech;
        Clear @@Loc31anes;
        Clear @@Loc99NonFac;
        Clear @@Loc99Facil;
        Clear @@Loc99Prof;
        Clear @@Loc99Tech;
        Clear @@Loc99anes;
        Clear @@Loc9Used;
        Clear @@Loc11Used;
        Clear @@Loc15Used;
        Clear @@Loc18Used;
        Clear @@Loc20Used;
        Clear @@Loc28Used;
        Clear @@Loc31Used;
        Clear @@Loc99Used;
        Clear @@PcTc;
        Clear @@Glob;
        Clear @@PreOp;
        Clear @@IntraOp;
        Clear @@PostOp;
        Clear @@MultSurg;
        Clear @@Bilat;
        Clear @@AsstSurg;
        Clear @@CoSurg;
        Clear @@TeamSurg;
        Clear @@EndoBase;
        Clear @@StatCd;
      /End-free
     p InitFields      e
       //===================================================================
       // GetAmbulAll - Get Ambulance Allowance.
       //===================================================================
     p GetAmbulAll     b
     d GetAmbulAll     pi
      /Free
       ##Call = 'F';
       ##Rnd  = 'M';
       ##Rrn  = @@Rrn;
       GetMedicAllow(##Rrn:##Call:##Eob:##Msg:##Modr:##Rnd:##Alw1:##Alw2:
                     ##Alw3:##RtCd);
       Eval(h) @@Loc9Anes = ##Alw1 * xxPerc;
       Eval(h) @@Loc11Anes = ##Alw1 * xxPerc;
       Eval(h) @@Loc15Anes = ##Alw1 * xxPerc;
       Eval(h) @@Loc18Anes = ##Alw1 * xxPerc;
       Eval(h) @@Loc20Anes = ##Alw1 * xxPerc;
       Eval(h) @@Loc28Anes = ##Alw1 * xxPerc;
       Eval(h) @@Loc31Anes = ##Alw1 * xxPerc;
       Eval(h) @@Loc99Anes = ##Alw1 * xxPerc;
      /End-free
     p GetAmbulAll     e
       //===================================================================
       // GetClabAll - Get Clinical Lab Allowance.
       //===================================================================
     p GetClabAll      b
     d GetClabAll      pi
      /Free
       ##Call = 'F';
       ##Rnd  = 'M';
       ##Rrn  = @@Rrn;
       GetMedicAllow(##Rrn:##Call:##Eob:##Msg:##Modr:##Rnd:##Alw1:##Alw2:
                     ##Alw3:##RtCd);
       //
       // Technical Component
       //
       @@Loc9Tech = ##Alw1 * xxPerc;
       @@Loc11Tech = ##Alw1 * xxPerc;
       @@Loc15Tech = ##Alw1 * xxPerc;
       @@Loc18Tech = ##Alw1 * xxPerc;
       @@Loc20Tech = ##Alw1 * xxPerc;
       @@Loc28Tech = ##Alw1 * xxPerc;
       @@Loc31Tech = ##Alw1 * xxPerc;
       @@Loc99Tech = ##Alw1 * xxPerc;
       //
       // Professional Component
       //
       @@Loc9Prof = @@Loc9Tech * .45;
       @@Loc11Prof = @@Loc11Tech * .45;
       @@Loc15Prof = @@Loc15Tech * .45;
       @@Loc18Prof = @@Loc18Tech * .45;
       @@Loc20Prof = @@Loc20Tech * .45;
       @@Loc28Prof = @@Loc28Tech * .45;
       @@Loc31Prof = @@Loc31Tech * .45;
       @@Loc99Prof = @@Loc99Tech * .45;
       //
       // Total
       //
       @@Loc9NonFac = @@Loc9Prof + @@Loc9Tech;
       @@Loc9Facil = @@Loc9NonFac;
       @@Loc11NonFac = @@Loc11Prof + @@Loc11Tech;
       @@Loc11Facil = @@Loc11NonFac;
       @@Loc15NonFac = @@Loc15Prof + @@Loc15Tech;
       @@Loc15Facil = @@Loc15NonFac;
       @@Loc18NonFac = @@Loc18Prof + @@Loc18Tech;
       @@Loc18Facil = @@Loc18NonFac;
       @@Loc20NonFac = @@Loc20Prof + @@Loc20Tech;
       @@Loc20Facil = @@Loc20NonFac;
       @@Loc28NonFac = @@Loc28Prof + @@Loc28Tech;
       @@Loc28Facil = @@Loc28NonFac;
       @@Loc31NonFac = @@Loc31Prof + @@Loc31Tech;
       @@Loc31Facil = @@Loc31NonFac;
       @@Loc99NonFac = @@Loc99Prof + @@Loc99Tech;
       @@Loc99Facil = @@Loc99NonFac;

       Select;
       When @@PcTc = '0' or @@PcTc >= '2' and
             @@PcTc <= '5' or @@PcTc = '9';
          @@Loc9Tech = 0;
          @@Loc11Tech = 0;
          @@Loc15Tech = 0;
          @@Loc18Tech = 0;
          @@Loc20Tech = 0;
          @@Loc28Tech = 0;
          @@Loc31Tech = 0;
          @@Loc99Tech = 0;
          @@Loc9Prof = 0;
          @@Loc11Prof = 0;
          @@Loc15Prof = 0;
          @@Loc18Prof = 0;
          @@Loc20Prof = 0;
          @@Loc28Prof = 0;
          @@Loc31Prof = 0;
          @@Loc99Prof = 0;
       When @@PcTc = '6' or @@PcTc = '8';
          @@Loc9Tech = 0;
          @@Loc11Tech = 0;
          @@Loc15Tech = 0;
          @@Loc18Tech = 0;
          @@Loc20Tech = 0;
          @@Loc28Tech = 0;
          @@Loc31Tech = 0;
          @@Loc99Tech = 0;
       Endsl;
      /End-Free
     p GetClabAll      e
       //===================================================================
       // GetCodeInfo - Get procedure code, description, indicators, etc.
       //===================================================================
     p GetCodeInfo     b
     d GetCodeInfo     pi
      /Free
       //
       // Find the Procedure Code and Description
       //
          If %Found(faeolg);
             Select;
             When EObTyp = 'N';
                Chain (CAco#:CAsub#:CAfnd:CAfyr:CAcase:CAeob#:CAdSeq) pdh01;
                If %Found(fddhl);
                   Mod(1) = DHmod1;
                   Mod(2) = DHmod2;
                   Mod(3) = DHmod3;
                   Mod(4) = DHmod4;
                   SortA Mod;
                   xxMod = *blanks;
                   If DHmod1 <> *blanks;
                      xxMod = DHmod1;
                      Else;
                         If DHmod2 <> *blanks;
                            xxMod = DHmod2;
                         Else;
                            If DHmod3 <> *blanks;
                               xxMod = DHmod3;
                            Else;
                               If DHmod4 <> *blanks;
                                  xxMod = DHmod4;
                               Endif;
                            Endif;
                         Endif;
                      Endif;
                   SetGt (DHprCd:xxMod:CAfDat) prv01;
                   Readpe (DHPrCd:xxMod) prv01;
                   If %Eof(ftrvl1);
                     xxMod = *blanks;
                     SetGt (DHprCd:xxMod:CAfDat) prv01;
                     Readpe (DHPrCd:xxMod) prv01;
                   Endif;
                   If Not %Eof(ftrvl1);
                      @@Desc = RVdesc;
                      @@Prcd = RVprCd;
                      @@Mod  = RVmod1;
                      @@PcTc = RVpcTc;
                      @@Glob = RVglob;
                      @@PreOp = RVpre;
                      @@IntraOp = RVintr;
                      @@PostOp = RVpost;
                      @@MultSurg = RVMult;
                      @@Bilat = RVbila;
                      @@AsstSurg = RVasst;
                      @@CoSurg = RVcoSg;
                      @@TeamSurg = RVteSg;
                      @@EndoBase = RVenba;
                      @@StatCd = RVstCd;
                   Endif;
                Endif;
             When EObTyp = 'D';
                Chain (CAco#:CAsub#:CAfnd:CAfyr:CAcase:CAeob#:CAdSeq) pdd01;
                If %Found(fdddl);
                   SetGt (DHprCd:xxMod:CAfDat) prv01;
                   Readpe (DHPrCd:xxMod) prv01;
                   If %Eof(ftrvl1);
                     xxMod = *blanks;
                     SetGt (DDprCd:' ':CAfDat) prv01;
                     Readpe (DHPrCd:' ') prv01;
                   Endif;
                   If Not %Eof(ftrvl1);
                      @@Desc = RVdesc;
                      @@Prcd = RVprCd;
                      @@Mod  = RVmod1;
                      @@PcTc = RVpcTc;
                      @@Glob = RVglob;
                      @@PreOp = RVpre;
                      @@IntraOp = RVintr;
                      @@PostOp = RVpost;
                      @@MultSurg = RVMult;
                      @@Bilat = RVbila;
                      @@AsstSurg = RVasst;
                      @@CoSurg = RVcoSg;
                      @@TeamSurg = RVteSg;
                      @@EndoBase = RVenba;
                      @@StatCd = RVstCd;
                   Endif;
                Endif;
             Endsl;
          Endif;
      /End-Free
     p GetCodeInfo     e
       //===================================================================
       // GetDmeAll - Get the DME Allowance for Texas
       //===================================================================
     p GetDmeAll       b
     d GetDmeAll       pi
      /Free
       ##Call = 'F';
       ##Rnd  = 'M';
       ##Rrn  = @@Rrn;
       GetMedicAllow(##Rrn:##Call:##Eob:##Msg:##Modr:##Rnd:##Alw1:##Alw2:
                     ##Alw3:##RtCd);
       Select;
       When %Lookup('RR':Mod) > 0;
          Eval(h) @@Loc9Anes = ##Alw1 * xxPerc;
          Eval(h) @@Loc11Anes = ##Alw1 * xxPerc;
          Eval(h) @@Loc15Anes = ##Alw1 * xxPerc;
          Eval(h) @@Loc18Anes = ##Alw1 * xxPerc;
          Eval(h) @@Loc20Anes = ##Alw1 * xxPerc;
          Eval(h) @@Loc28Anes = ##Alw1 * xxPerc;
          Eval(h) @@Loc31Anes = ##Alw1 * xxPerc;
          Eval(h) @@Loc99Anes = ##Alw1 * xxPerc;
       When %Lookup('UE':Mod) > 0;
          Eval(h) @@Loc9Used = ##Alw1 * xxPerc;
          Eval(h) @@Loc11Used = ##Alw1 * xxPerc;
          Eval(h) @@Loc15Used = ##Alw1 * xxPerc;
          Eval(h) @@Loc18Used = ##Alw1 * xxPerc;
          Eval(h) @@Loc20Used = ##Alw1 * xxPerc;
          Eval(H) @@Loc28Used = ##Alw1 * xxPerc;
          Eval(h) @@Loc31Used = ##Alw1 * xxPerc;
          Eval(h) @@Loc99Used = ##Alw1 * xxPerc;
       Other;
          Eval(h) @@Loc9Nonfac = ##Alw1 * xxPerc;
          Eval(h) @@Loc9Facil = ##Alw1 * xxPerc;
          Eval(h) @@Loc11Nonfac = ##Alw1 * xxPerc;
          Eval(h) @@Loc11Facil = ##Alw1 * xxPerc;
          Eval(h) @@Loc15Nonfac = ##Alw1 * xxPerc;
          Eval(h) @@Loc15Facil = ##Alw1 * xxPerc;
          Eval(h) @@Loc18Nonfac = ##Alw1 * xxPerc;
          Eval(h) @@Loc18Facil = ##Alw1 * xxPerc;
          Eval(h) @@Loc20Nonfac = ##Alw1 * xxPerc;
          Eval(h) @@Loc20Facil = ##Alw1 * xxPerc;
          Eval(h) @@Loc28Nonfac = ##Alw1 * xxPerc;
          Eval(h) @@Loc28Facil = ##Alw1 * xxPerc;
          Eval(h) @@Loc31Nonfac = ##Alw1 * xxPerc;
          Eval(h) @@Loc31Facil = ##Alw1 * xxPerc;
          Eval(h) @@Loc99Nonfac = ##Alw1 * xxPerc;
          Eval(h) @@Loc99Facil = ##Alw1 * xxPerc;
       Endsl;
      /End-Free
     p GetDmeAll       e
       //===================================================================
       // GetMedDentAll - Get the Medicaid Dental Allowance for Texas
       //===================================================================
     p GetMedDentAll   b
     d GetMedDentAll   pi
      /Free
       Setgt (DDprCd:CAfDat) pdn01;
       Readpe DDprCd pdn01;
       If not %Eof(ftdnl1tx);
          Eval(h) @@Loc9Nonfac = DNfee  * xxPercDent;
          Eval(h) @@Loc9Facil = DNfee  * xxPercDent;
          Eval(h) @@Loc11Nonfac = DNfee  * xxPercDent;
          Eval(h) @@Loc11Facil = DNfee  * xxPercDent;
          Eval(h) @@Loc15Nonfac = DNfee  * xxPercDent;
          Eval(h) @@Loc15Facil = DNfee  * xxPercDent;
          Eval(h) @@Loc18Nonfac = DNfee  * xxPercDent;
          Eval(h) @@Loc18Facil = DNfee  * xxPercDent;
          Eval(h) @@Loc20Nonfac = DNfee  * xxPercDent;
          Eval(h) @@Loc20Facil = DNfee  * xxPercDent;
          Eval(h) @@Loc28Nonfac = DNfee  * xxPercDent;
          Eval(h) @@Loc28Facil = DNfee  * xxPercDent;
          Eval(h) @@Loc31Nonfac = DNfee  * xxPercDent;
          Eval(h) @@Loc31Facil = DNfee  * xxPercDent;
          Eval(h) @@Loc99Nonfac = DNfee  * xxPercDent;
          Eval(h) @@Loc99Facil = DNfee  * xxPercDent;
       Endif;
      /End-Free
     p GetMedDentAll   e
       //===================================================================
       // GetMedDmeAll - Get the Medicaid DME Allowance for Texas
       //===================================================================
     p GetMedDmeAll    b
     d GetMedDmeAll    pi
      /Free
       Setgt (DHprCd:CAfDat) pme01;
       Readpe DHprCd pme01;
       If not %Eof(ftmel2tx);
          Eval(h) @@Loc9Nonfac = MEfee  * xxPerc;
          Eval(h) @@Loc9Facil = MEfee  * xxPerc;
          Eval(h) @@Loc11Nonfac = MEfee  * xxPerc;
          Eval(h) @@Loc11Facil = MEfee  * xxPerc;
          Eval(h) @@Loc15Nonfac = MEfee  * xxPerc;
          Eval(h) @@Loc15Facil = MEfee  * xxPerc;
          Eval(h) @@Loc18Nonfac = MEfee  * xxPerc;
          Eval(h) @@Loc18Facil = MEfee  * xxPerc;
          Eval(h) @@Loc20Nonfac = MEfee  * xxPerc;
          Eval(h) @@Loc20Facil = MEfee  * xxPerc;
          Eval(h) @@Loc28Nonfac = MEfee  * xxPerc;
          Eval(h) @@Loc28Facil = MEfee  * xxPerc;
          Eval(h) @@Loc31Nonfac = MEfee  * xxPerc;
          Eval(h) @@Loc31Facil = MEfee  * xxPerc;
          Eval(h) @@Loc99Nonfac = MEfee  * xxPerc;
          Eval(h) @@Loc99Facil = MEfee  * xxPerc;
       Endif;
      /End-Free
     p GetMedDmeAll    e
       //===================================================================
       // GetDrugFeeAll - Get the Drug Fee Allowance for Texas
       //===================================================================
     p GetDrugFeeAll   b
     d GetDrugFeeAll   pi
      /Free
       ##Call = 'F';
       ##Rnd  = 'M';
       ##Rrn  = @@Rrn;
       GetMedicAllow(##Rrn:##Call:##Eob:##Msg:##Modr:##Rnd:##Alw1:##Alw2:
                     ##Alw3:##RtCd);
       @@Loc9Nonfac = ##Alw1 * xxPerc;
       @@Loc9Facil = ##Alw1 * xxPerc;
       @@Loc11Nonfac = ##Alw1 * xxPerc;
       @@Loc11Facil = ##Alw1 * xxPerc;
       @@Loc15Nonfac = ##Alw1 * xxPerc;
       @@Loc15Facil = ##Alw1 * xxPerc;
       @@Loc18Nonfac = ##Alw1 * xxPerc;
       @@Loc18Facil = ##Alw1 * xxPerc;
       @@Loc20Nonfac = ##Alw1 * xxPerc;
       @@Loc20Facil = ##Alw1 * xxPerc;
       @@Loc28Nonfac = ##Alw1 * xxPerc;
       @@Loc28Facil = ##Alw1 * xxPerc;
       @@Loc31Nonfac = ##Alw1 * xxPerc;
       @@Loc31Facil = ##Alw1 * xxPerc;
       @@Loc99Nonfac = ##Alw1 * xxPerc;
       @@Loc99Facil = ##Alw1 * xxPerc;
      /End-Free
     p GetDrugFeeAll   e
       //===================================================================
       // GetRvLocAllow - Get the RVU locality allowances for Texas
       //===================================================================
     p GetRvLocAllow   b
     d GetRvLocAllow   pi
      /Free
      //
       // No modifier
      //
       xxMod = '  ';
       SetGt (DHprCd:xxMod:CAfDat) prv01;
       Readpe (DHPrCd:xxMod) prv01;
       If Not %Eof(ftrvl1);
         //
          // Get the Medicare Allowance for Locality 9 in Texas
         //
          Setgt (EOaSt:9:CAfdat) pgp01;
          Readpe (EOaSt:9) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc9NonFac = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp) +
                           (RVmpRv * GPmpGp)) * RTmul1;
             If RVprCd >= '10000' and RVprCd <= '69999';
                Eval(h) @@Loc9Facil = ((RVwkRv * GPwkGp) +
                       (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul2;
             Else;
                Eval(h) @@Loc9Facil = ((RVwkRv * GPwkGp) +
                        (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul1;
             Endif;

             If DHprCd >= '00100' and DHprCd <= '01999';
                @@Loc9Anes = RVwkRv;
                @@Loc9Facil = 0;
                @@Loc9NonFac = 0;
             Endif;

          Endif;
         //
          // Get the Medicare Allowance for Locality 11 in Texas
         //
          Setgt (EOaSt:11:CAfdat) pgp01;
          Readpe (EOaSt:11) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc11NonFac = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp)
                         + (RVmpRv * GPmpGp)) * RTmul1;
             If RVprCd >= '10000' and RVprCd <= '69999';
                Eval(h) @@Loc11Facil = ((RVwkRv * GPwkGp) +
                       (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul2;
             Else;
                Eval(h) @@Loc11Facil = ((RVwkRv * GPwkGp) +
                (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul1;
             Endif;

             If DHprCd >= '00100' and DHprCd <= '01999';
                @@Loc11Anes = RVwkRv;
                @@Loc11Facil = 0;
                @@Loc11NonFac = 0;
             Endif;

          Endif;
         //
          // Get the Medicare Allowance for Locality 15 in Texas
         //
          Setgt (EOaSt:15:CAfdat) pgp01;
          Readpe (EOaSt:15) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc15NonFac = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp)
                         + (RVmpRv * GPmpGp)) * RTmul1;
             If RVprCd >= '10000' and RVprCd <= '69999';
                Eval(h) @@Loc15Facil = ((RVwkRv * GPwkGp) +
                (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul2;
             Else;
                 Eval(h) @@Loc15Facil = ((RVwkRv * GPwkGp) +
                 (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul1;
             Endif;

             If DHprCd >= '00100' and DHprCd <= '01999';
                @@Loc15Anes = RVwkRv;
                @@Loc15Facil = 0;
                @@Loc15NonFac = 0;
             Endif;

          Endif;
         //
          // Get the Medicare Allowance for Locality 18 in Texas
         //
          Setgt (EOaSt:18:CAfdat) pgp01;
          Readpe (EOaSt:18) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc18NonFac = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp)
                         + (RVmpRv * GPmpGp)) * RTmul1;
             If RVprCd >= '10000' and RVprCd <= '69999';
                Eval(h) @@Loc18Facil = ((RVwkRv * GPwkGp) +
                (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul2;
             Else;
                Eval(h) @@Loc18Facil = ((RVwkRv * GPwkGp) +
                (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul1;
             Endif;

             If DHprCd >= '00100' and DHprCd <= '01999';
                @@Loc18Anes = RVwkRv;
                @@Loc18Facil = 0;
                @@Loc18NonFac = 0;
             Endif;

          Endif;
         //
          // Get the Medicare Allowance for Locality 20 in Texas
         //
          Setgt (EOaSt:20:CAfdat) pgp01;
          Readpe (EOaSt:20) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc20NonFac = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp)
                         + (RVmpRv * GPmpGp)) * RTmul1;
             If RVprCd >= '10000' and RVprCd <= '69999';
                Eval(h) @@Loc20Facil = ((RVwkRv * GPwkGp) +
                (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul2;
             Else;
                Eval(h) @@Loc20Facil = ((RVwkRv * GPwkGp) +
                (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul1;
             Endif;

             If DHprCd >= '00100' and DHprCd <= '01999';
                @@Loc20Anes = RVwkRv;
                @@Loc20Facil = 0;
                @@Loc20NonFac = 0;
             Endif;

          Endif;
         //
          // Get the Medicare Allowance for Locality 28 in Texas
         //
          Setgt (EOaSt:28:CAfdat) pgp01;
          Readpe (EOaSt:28) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc28NonFac = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp)
                      + (RVmpRv * GPmpGp)) * RTmul1;
             If RVprCd >= '10000' and RVprCd <= '69999';
                Eval(h) @@Loc28Facil = ((RVwkRv * GPwkGp) +
                (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul2;
             Else;
                Eval(h) @@Loc28Facil = ((RVwkRv * GPwkGp) +
                (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul1;
             Endif;

             If DHprCd >= '00100' and DHprCd <= '01999';
                @@Loc28Anes = RVwkRv;
                @@Loc28Facil = 0;
                @@Loc28NonFac = 0;
             Endif;

          Endif;
         //
          // Get the Medicare Allowance for Locality 31 in Texas
         //
          Setgt (EOaSt:31:CAfdat) pgp01;
          Readpe (EOaSt:31) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc31NonFac = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp)
                      + (RVmpRv * GPmpGp)) * RTmul1;
             If RVprCd >= '10000' and RVprCd <= '69999';
                Eval(h) @@Loc31Facil = ((RVwkRv * GPwkGp) +
                (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul2;
             Else;
                Eval(h) @@Loc31Facil = ((RVwkRv * GPwkGp) +
                (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul1;
             Endif;

             If DHprCd >= '00100' and DHprCd <= '01999';
                @@Loc31Anes = RVwkRv;
                @@Loc31Facil = 0;
                @@Loc31NonFac = 0;
             Endif;

          Endif;
         //
          // Get the Medicare Allowance for Locality 99 in Texas
         //
          Setgt (EOaSt:99:CAfdat) pgp01;
          Readpe (EOaSt:99) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc99NonFac = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp)
                      + (RVmpRv * GPmpGp)) * RTmul1;
             If RVprCd >= '10000' and RVprCd <= '69999';
                Eval(h) @@Loc99Facil = ((RVwkRv * GPwkGp) +
                (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul2;
             Else;
                Eval(h) @@Loc99Facil = ((RVwkRv * GPwkGp) +
                (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul1;
             Endif;

             If DHprCd >= '00100' and DHprCd <= '01999';
                @@Loc99Anes = RVwkRv;
                @@Loc99Facil = 0;
                @@Loc99NonFac = 0;
             Endif;

          Endif;
       Endif;
      //
       // Modifier 53
      //
       xxMod = '53';
       SetGt (DHprCd:xxMod:CAfDat) prv01;
       Readpe (DHPrCd:xxMod) prv01;
       If Not %Eof(ftrvl1);
         //
          // Get the Medicare Allowance for Locality 9 in Texas
         //
          Setgt (EOaSt:9:CAfdat) pgp01;
          Readpe (EOaSt:9) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc9NonFac = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp)
                         + (RVmpRv * GPmpGp)) * RTmul1;
             If RVprCd >= '10000' and RVprCd <= '69999';
                Eval(h) @@Loc9Facil = ((RVwkRv * GPwkGp) +
                (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul2;
             Else;
                Eval(h) @@Loc9Facil = ((RVwkRv * GPwkGp) +
                (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul1;
             Endif;
          Endif;
         //
          // Get the Medicare Allowance for Locality 11 in Texas
         //
          Setgt (EOaSt:11:CAfdat) pgp01;
          Readpe (EOaSt:11) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc11NonFac = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp)
                         + (RVmpRv * GPmpGp)) * RTmul1;
             If RVprCd >= '10000' and RVprCd <= '69999';
                Eval(h) @@Loc11Facil = ((RVwkRv * GPwkGp) +
                (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul2;
             Else;
                Eval(h) @@Loc11Facil = ((RVwkRv * GPwkGp) +
                (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul1;
             Endif;
          Endif;
         //
          // Get the Medicare Allowance for Locality 15 in Texas
         //
          Setgt (EOaSt:15:CAfdat) pgp01;
          Readpe (EOaSt:15) pgp01;
         //
          If %Found(ftgpl3);
              Eval(h) @@Loc15NonFac = ((RVwkRv * GPwkGp) +
              (RVfNpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul1;
             If RVprCd >= '10000' and RVprCd <= '69999';
                Eval(h) @@Loc15Facil = ((RVwkRv * GPwkGp) +
                (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul2;
             Else;
                 Eval(h) @@Loc15Facil = ((RVwkRv * GPwkGp) +
                 (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul1;
             Endif;
          Endif;
         //
          // Get the Medicare Allowance for Locality 18 in Texas
         //
          Setgt (EOaSt:18:CAfdat) pgp01;
          Readpe (EOaSt:18) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc18NonFac = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp)
                         + (RVmpRv * GPmpGp)) * RTmul1;
             If RVprCd >= '10000' and RVprCd <= '69999';
                Eval(h) @@Loc18Facil = ((RVwkRv * GPwkGp) +
                (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul2;
             Else;
                Eval(h) @@Loc18Facil = ((RVwkRv * GPwkGp) +
                (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul1;
             Endif;
          Endif;
         //
          // Get the Medicare Allowance for Locality 20 in Texas
         //
          Setgt (EOaSt:20:CAfdat) pgp01;
          Readpe (EOaSt:20) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc20NonFac = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp)
                         + (RVmpRv * GPmpGp)) * RTmul1;
             If RVprCd >= '10000' and RVprCd <= '69999';
                Eval(h) @@Loc20Facil = ((RVwkRv * GPwkGp) +
                (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul2;
             Else;
                Eval(h) @@Loc20Facil = ((RVwkRv * GPwkGp) +
                (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul1;
             Endif;
          Endif;
         //
          // Get the Medicare Allowance for Locality 28 in Texas
         //
          Setgt (EOaSt:28:CAfdat) pgp01;
          Readpe (EOaSt:28) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc28NonFac = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp)
                      + (RVmpRv * GPmpGp)) * RTmul1;
             If RVprCd >= '10000' and RVprCd <= '69999';
                Eval(h) @@Loc28Facil = ((RVwkRv * GPwkGp) +
                (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul2;
             Else;
                Eval(h) @@Loc28Facil = ((RVwkRv * GPwkGp) +
                (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul1;
             Endif;
          Endif;
         //
          // Get the Medicare Allowance for Locality 31 in Texas
         //
          Setgt (EOaSt:31:CAfdat) pgp01;
          Readpe (EOaSt:31) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc31NonFac = ((RVwkRv * GPwkGp) +
                     (RVfNpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul1;
             If RVprCd >= '10000' and RVprCd <= '69999';
                Eval(h) @@Loc31Facil = ((RVwkRv * GPwkGp) +
                (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul2;
             Else;
                Eval(h) @@Loc31Facil = ((RVwkRv * GPwkGp) +
                (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul1;
             Endif;
          Endif;
         //
          // Get the Medicare Allowance for Locality 99 in Texas
         //
          Setgt (EOaSt:99:CAfdat) pgp01;
          Readpe (EOaSt:99) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc99NonFac = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp)
                      + (RVmpRv * GPmpGp)) * RTmul1;
             If RVprCd >= '10000' and RVprCd <= '69999';
                Eval(h) @@Loc99Facil = ((RVwkRv * GPwkGp) +
                (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul2;
             Else;
                Eval(h) @@Loc99Facil = ((RVwkRv * GPwkGp) +
                (RVfFpe * GPpeGp) + (RVmpRv * GPmpGp)) * RTmul1;
             Endif;
          Endif;
       Endif;
      //
       // Modifier 26
      //
       xxMod = '26';
       SetGt (DHprCd:xxMod:CAfDat) prv01;
       Readpe (DHPrCd:xxMod) prv01;
       If Not %Eof(ftrvl1);
         //
          // Get the Medicare Allowance for Locality 9 in Texas
         //
          Setgt (EOaSt:9:CAfdat) pgp01;
          Readpe (EOaSt:9) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc9Prof = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp) +
                           (RVmpRv * GPmpGp)) * RTmul1;
          Endif;
         //
          // Get the Medicare Allowance for Locality 11 in Texas
         //
          Setgt (EOaSt:11:CAfdat) pgp01;
          Readpe (EOaSt:11) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc11Prof = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp) +
                           (RVmpRv * GPmpGp)) * RTmul1;
          Endif;
         //
          // Get the Medicare Allowance for Locality 15 in Texas
         //
          Setgt (EOaSt:15:CAfdat) pgp01;
          Readpe (EOaSt:15) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc15Prof = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp) +
                           (RVmpRv * GPmpGp)) * RTmul1;
          Endif;
         //
          // Get the Medicare Allowance for Locality 18 in Texas
         //
          Setgt (EOaSt:18:CAfdat) pgp01;
          Readpe (EOaSt:18) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc18Prof = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp) +
                          (RVmpRv * GPmpGp)) * RTmul1;
          Endif;
         //
          // Get the Medicare Allowance for Locality 20 in Texas
         //
          Setgt (EOaSt:20:CAfdat) pgp01;
          Readpe (EOaSt:20) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc20Prof = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp) +
                           (RVmpRv * GPmpGp)) * RTmul1;
          Endif;
         //
          // Get the Medicare Allowance for Locality 28 in Texas
         //
          Setgt (EOaSt:28:CAfdat) pgp01;
          Readpe (EOaSt:28) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc28Prof = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp) +
                           (RVmpRv * GPmpGp)) * RTmul1;
          Endif;
         //
          // Get the Medicare Allowance for Locality 31 in Texas
         //
          Setgt (EOaSt:31:CAfdat) pgp01;
          Readpe (EOaSt:31) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc31Prof = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp) +
                           (RVmpRv * GPmpGp)) * RTmul1;
          Endif;
         //
          // Get the Medicare Allowance for Locality 99 in Texas
         //
          Setgt (EOaSt:99:CAfdat) pgp01;
          Readpe (EOaSt:99) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc99Prof = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp) +
                           (RVmpRv * GPmpGp)) * RTmul1;
          Endif;
       Endif;
      //
       // Modifier TC
      //
       xxMod = 'TC';
       SetGt (DHprCd:xxMod:CAfDat) prv01;
       Readpe (DHPrCd:xxMod) prv01;
       If Not %Eof(ftrvl1);
         //
          // Get the Medicare Allowance for Locality 9 in Texas
         //
          Setgt (EOaSt:9:CAfdat) pgp01;
          Readpe (EOaSt:9) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc9Tech = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp) +
                           (RVmpRv * GPmpGp)) * RTmul1;
          Endif;
         //
          // Get the Medicare Allowance for Locality 11 in Texas
         //
          Setgt (EOaSt:11:CAfdat) pgp01;
          Readpe (EOaSt:11) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc11Tech = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp) +
                        (RVmpRv * GPmpGp)) * RTmul1;
          Endif;
         //
          // Get the Medicare Allowance for Locality 15 in Texas
         //
          Setgt (EOaSt:15:CAfdat) pgp01;
          Readpe (EOaSt:15) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc15Tech = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp) +
                           (RVmpRv * GPmpGp)) * RTmul1;
          Endif;
         //
          // Get the Medicare Allowance for Locality 18 in Texas
         //
          Setgt (EOaSt:18:CAfdat) pgp01;
          Readpe (EOaSt:18) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc18Tech = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp) +
                        (RVmpRv * GPmpGp)) * RTmul1;
          Endif;
         //
          // Get the Medicare Allowance for Locality 20 in Texas
         //
          Setgt (EOaSt:20:CAfdat) pgp01;
          Readpe (EOaSt:20) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc20Tech = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp) +
                        (RVmpRv * GPmpGp)) * RTmul1;
          Endif;
         //
          // Get the Medicare Allowance for Locality 28 in Texas
         //
          Setgt (EOaSt:28:CAfdat) pgp01;
          Readpe (EOaSt:28) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc28Tech = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp) +
                        (RVmpRv * GPmpGp)) * RTmul1;
          Endif;
         //
          // Get the Medicare Allowance for Locality 31 in Texas
         //
          Setgt (EOaSt:31:CAfdat) pgp01;
          Readpe (EOaSt:31) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc31Tech = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp) +
                        (RVmpRv * GPmpGp)) * RTmul1;
          Endif;
         //
          // Get the Medicare Allowance for Locality 99 in Texas
         //
          Setgt (EOaSt:99:CAfdat) pgp01;
          Readpe (EOaSt:99) pgp01;
         //
          If %Found(ftgpl3);
             Eval(h) @@Loc99Tech = ((RVwkRv * GPwkGp) + (RVfNpe * GPpeGp) +
                        (RVmpRv * GPmpGp)) * RTmul1;
          Endif;
       Endif;
      /Free
      /End-Free
     p GetRvLocAllow   e
       //===================================================================
       // GetCommLocAllw - Get the locality allowances for Texas
       //                  Commission Codes.
       //===================================================================
     p GetCommLocAllw  b
     d GetCommLocAllw  pi
      /Free
       //
       // Get the Medicare Allowance for Commission Codes
       //
       Setgt (DHco#:DHprCd:CAfDat) ppr01;
       Readpe (DHco#:DHprCd) ppr01;
       If not %Eof(fmprltx);
          If DHprCd = '97545' or DHprCd = '97546';
             @@Loc9Nonfac = PRmra;
             @@Loc9Facil = PR92n6;
             @@Loc9Prof = PRprof;
             @@Loc11Nonfac = PR92n8;
             @@Loc11Facil = PR92n9;
             @@Loc11Prof = PR92Nb;
             @@Loc15Nonfac = PR92nE;
             @@Loc15Facil = PR92nF;
             @@Loc15Prof = PR92nH;
             @@Loc18Nonfac = PR92nK;
             @@Loc18Facil = PR92nL;
             @@Loc18Prof = PR92nN;
             @@Loc20Nonfac = PR92nQ;
             @@Loc20Facil = PR92nR;
             @@Loc20Prof = PR92nT;
             @@Loc28Nonfac = PR92nW;
             @@Loc28Facil = PR92nX;
             @@Loc28Prof = PR92nZ;
             @@Loc31Nonfac = PR92o3;
             @@Loc31Facil = PR92o4;
             @@Loc31Prof = PR92o6;
             @@Loc99Nonfac = PR92o9;
             @@Loc99Facil = PR92oA;
             @@Loc99Prof = PR92oC;
          Endif;
          If DHprCd = '97799';
             @@Loc9Anes = PRanAm;
             @@Loc9prof = PRprof;
             @@Loc11Anes = PR92nA;
             @@Loc11Prof = PR92nB;
             @@Loc15Anes = PR92nG;
             @@Loc15Prof = PR92nH;
             @@Loc18Anes = PR92nM;
             @@Loc18Prof = PR92nN;
             @@Loc20Anes = PR92nS;
             @@Loc20Prof = PR92nT;
             @@Loc28Anes = PR92nY;
             @@Loc28Prof = PR92nZ;
             @@Loc31Anes = PR92o5;
             @@Loc31Prof = PR92o6;
             @@Loc99Anes = PR92oB;
             @@Loc99Prof = PR92oC;
          Endif;
       Endif;
      /End-Free
     p GetCommLocAllw  e
       //===================================================================
       // ProgramEnd - Close files and return to calling program
       //===================================================================
     p ProgramEnd      b
     d ProgramEnd      pi
      /Free
       *inlr = *on;
       Return;
      /End-Free
     p ProgramEnd      e
       //===================================================================
       // ProgramStart - Close files and return to calling program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
      /Free
       //
       // Clear the fields
       //
       InitFields();
       //
       // Get the Fee Schedule Rate Table Percentage and
       //
       Chain @@Rrn pca01;
       If %Found(fdcap);
          Chain (CAco#:CAsub#:CAfnd:CAfyr:CAcase:CAeob#) peo01;
          If %Found(faeolg);
             Setgt (EOast:'M':CAfdat) prt01;
             Readpe (EOast:'M') prt01;
             If %Found(ftrtl2);
                xxPerc = RTmul3;
                xxPercDent = RTmul4;
             Endif;
          Endif;
       Endif;
      /End-Free
     p ProgramStart    e
