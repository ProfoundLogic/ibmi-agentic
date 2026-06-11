    H**********************************************************************
     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H**********************************************************************
    H* PROGRAM: F4000NHL                                                  *
    H* DESCRIPTION: ADJUDICATION WORKFILE BUILD FOR HL                    *
    H**********************************************************************
    H*  CREATED: 06/21/12  C:277  PROJECT: 114MFI                         *
    H**********************************************************************
    H*  REVISIONS:                                                        *
    H*                                                                    *
     H*  07/09/24  C:342  Project: FS688                                   *
     H*                   2024 USH&L Fee Schedule (Recompile)              *
    H*                                                                    *
     H*  07/18/23 C:342 Project: NY060                                     *
    H*                NY Compliance: CARC/RARC Update                     *
    H*                                                                    *
     H*  10/09/19 C:079 Project: FS563                                     *
    H*                 Add logic in ASC for Ancillary Codes               *
    H*                                                                    *
     H*  10/09/19 C:212 Project: FS563                                     *
    H*                 Load Product type                                  *
    H*                                                                    *
     H*  11/05/18 C:790 Project: FS547                                     *
    H*                 Pay ASC according to Physician office allowance    *
    H*                                                                    *
     H*  04/27/18 C:180 Project: C114MJS                                   *
    H*                 Increase xxWAP size.                               *
    H*                                                                    *
    H*  10/12/17  C:079  Support: 50160/114                               *
    H*                   Monitor for blanks in HLGLOB                     *
    H*                                                                    *
    H*  04/12/17  C:079  Project: FS520                                   *
    H*                   Make changes for APC payments for DME and PT     *
    H*                                                                    *
    H*  03/30/17  C:212  Project: FS520                                   *
    H*                   Make changes for Ancillary                       *
    H*                                                                    *
    H*  05/30/16  C:309  Project: FS497                                   *
    H*                   Changed field ##si size to 2A and                *
    H*                   populated "CAapcSI" with ##si                    *
    H*                                                                    *
    H*  10/06/15  C:062  Project: FS475                                   *
    H*                   Changed for effective date 08/14/15              *
    H*                                                                    *
    H*  10/05/15  C:079  Project: FS473                                   *
    H*                   Uncomment ASC calculation                        *
    H*                   Fix IPPS calculation                             *
    H*                                                                    *
    H*  09/16/15  C:212  Project: FS473                                   *
    H*                   Changed for effective date 07/01/13              *
    H*                                                                    *
    H*  08/15/14  C:280  Project: FS452                                   *
    H*                   Changed for effective date 07/01/13              *
    H*                                                                    *
    H*  08/26/13  C:296  Project: FS433                                   *
    H*                   Changed for effective date 07/01/13              *
    H*                                                                    *
    H*  09/12/12  C:277  Project: 114MFI                                  *
    H*                   Run for Effective date 07/28/12                  *
    H*                                                                    *
    H**********************************************************************
    H** DO NOT INSTALL THIS PROGRAM WITHOUT INSTALLING F4000N.  A CHANGE **
    H** TO THE PHARM CALCS FOR SUPPORT 58250 REQUIRES THIS               **
    H*                                                                    *
    H**********************************************************************
    H*  INDICATOR USAGE:                                                  *
    H*                                                                    *
    H*                                                                    *
    H**********************************************************************
    F*
     Ffdcalh    uf   e           k disk    infds(cafds) usropn
     Ffddul     if   e           k disk
     Fftuml     if   e           k disk
     Ffddhl     if   e           k disk
     Ffdddl     if   e           k disk
     Ffddpl     if   e           k disk
     Ffmrxl     if   e           k disk
     Ffaeolg    if   e           k disk    infds(eofds)
     Ffacap     o    e           k disk    usropn
     F                                     rename(pca01:pcawk2)
    F*
     Ffdrul1    if   e           k disk
    F*
     Ffdfrl5    if   e           k disk
    F*
     Ffmp2lwv   if   e           k disk
     Fftrvl3    if   e           k disk
     Fftrtl2    if   e           k disk
     Ffaeul     if   e           k disk
     Ffmhll2    if   e           k disk
     Fftrrl1    if   e           k disk
     Fftpfl     if   e           k disk
     Ffthcl1    if   e           k disk
     Fftaal1    if   e           k disk
    D*
     Dcafds            ds
     Dcarrn                  397    400b 0
     D*
     Deofds            ds
     Deorrn                  397    400b 0
     D*
     D qqf             s              7    dim(50)
    D*
     D date            s              7  0 dim(999)
     D mod             s              2    dim(4) ascend
     D xxmd62          s              4  3
     D camd82          s              3  2
    D*
     D                 ds
     D xxcmsv                        11
     D  xxsvcd                        3  0 overlay(xxcmsv)
     D  xxcpt4                        5    overlay(xxcmsv:4)
     D  xxunit                        3  0 overlay(xxcmsv:9)
     D* date Diffrence
     DDatDiff          pr                  extpgm('SRDDIF7')
     D indate1                        7  0
     D indate2                        7  0
     D outdiff                        5  0
     D outerr                         1
     D*** Begin Add *** 10/06/15 *********************************
     D*
     D* Calculate medicare OPPS allowance
     DCalc_OPPS_Pmnt   pr                  extpgm('FA762R')
     D ##Rrn                          9  0
     D ##Call                         1
     D***** Beg Chg *** 05/30/16 *********************************
     D***** ##Si                           1
     D ##Si                           2
     D***** End Chg *** 05/30/16 *********************************
     D ##Eob                          3
     D ##Msg                          7
     D ##ModR                         1
     D ##Alw1                         9  2
     D ##Rtcd                         1
     D ##Rsch                         9  2
     D ##Outl                         9  2
     D*** End   Add *** 10/06/15 *********************************
     D* Calculate IPPS payment API
     DCalc_Ipps_Pmnt   pr                  extpgm('FA756R')
     D ##Rrn#                         9  0
     D ##Rtcd                         1
     D ##Allow                        9  2
     D ##Call#                        1
     D**** Beg Chg *** 07/18/23 **************************************
     D**** ##Eob#                         3
     D ##Eob#                         6
     D**** End Chg *** 07/18/23 **************************************
     D ##Msg#                         7
     D ##Code                         3
     D*
     D* Calculate ASC payment API
     DCalc_Asc_Pmnt    pr                  extpgm('FA765R')
     D ##Rrn                          9  0
     D ##Call                         1
     D ##Type                         1
     D ##Pi                           2
     D ##Eob                          3
     D ##Msg                          7
     D ##Alw1                         9  2
     D ##Alw2                         9  2
     D ##Alw3                         9  2
     D ##Alw4                         9  2
     D ##Rtcd                         1
     D* Calculate Physician payment API
     DCalc_Phy_Pmnt    pr                  extpgm('FA778R')
     D ##Rrn                          9  0
     D ##Call                         1
     D ##Eob                          3
     D ##Msg                          7
     D ##Modr                         1
     D ##Rnd                          1
     D ##Alw1                         9  2
     D ##Alw2                         9  2
     D ##Alw3                         9  2
     D ##Rtcd                         1
     DCVTDATE          pr                  extPgm('SRCVTDAT')
     D indatein                       8  0
     D infromfmt                      8
     D intofmt                        8
     D Outdate                       10
     D outerr                         1
     D
     DTimeTo6          Pr             6  0
     D time                            t   Const
     D*
     DDateTo7          PR             7  0
     D date                            d   Const
     D*
     DsystemDate       PR              d
     D*
     DsystemTime       PR              t
    D*  Date/Time Data Structure.
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D psds          esds                  extname(szq1p)
     D*
     DdtCheck          PR             8  0
     D numDate8                       8  0 Const
     D dateType                       8    const options(*nopass)
     D*
     D                 DS
     D Dttomdyy                       8  0
     D Dtmm                    1      2  0
     D Dtdd                    3      4  0
     D Dtyy                    5      8  0
     D
     D                 DS
     D DttoYYmd                       8  0
     D D1yy                    1      4  0
     D D1mm                    5      6  0
     D D1dd                    7      8  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D cafolu          s              3  0
     D camd50          s              3  2
     D camd51          s              3  2
     D camd54          s              3  2
     D camd55          s              3  2
     D camd56          s              3  2
     D camd62          s              3  2
     D camd66          s              3  2
     D camd80          s              3  2
     D camd81          s              3  2
     D catype          s              1
     D kkdseq          s                   like(cadseq)
     D kkefdt          s                   like(frefdt)
     D kkprov          s                   like(pfprov)
     D kkffdt          s                   like(pfffdt)
     D kkymd           s              7  0
     D mod1            s              8
     D qqcase          s              7  0
     D qqco#           s              3  0
     D qqdcs#          s              5
     D qqeob#          s              9  0
     D qqfein          s              9  0
     D qqfnd           s              3  0
     D qqfyr           s              3  0
     D qqmod           s              3  0
     D qqspcd          s              3
     D qqst            s              2
     D qqsub#          s              5  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D x               s              3  0
     D xxIdx           s              3  0
     D xcmra           s              9  2
     D xppmod          s                   like(capmod)
      *** Begin Change *** 04/27/18 *********************
     D**** xxawp           s              9  5
     D xxawp           s                   like(CAawp)
      *** End   Change *** 04/27/18 *********************
     D xxefdt          s                   like(cafdat)
     D xxfdat          s                   like(cafdat)
     D xxinvl          s              1
     D xxmd26          s              3  2
     D xxmd27          s              3  2
     D xxalwflg        s              1
     D inSi            s              1
     D ##Rrn#          s              9  0
     D ##Rrn           s              9  0
     D ##Rtcd          s              1
     D ##Allow         s              9  2
     D ##Call#         s              1
     D ##Call          s              1
     D ##Type          s              1
     D ##modr          s              1
     D ##rnd           s              1
     D ##Pi            s              2
     D**** Beg Chg *** 07/18/23 **************************************
     D**** ##Eob#          s              3
     D ##Eob#          s              6
     D**** End Chg *** 07/18/23 **************************************
     D ##Eob           s              3
     D ##Msg#          s              7
     D ##Msg           s              7
     D ##Code          s              3
     D ##Alw1          s              9  2
     D ##Alw2          s              9  2
     D ##Alw3          s              9  2
     D ##Alw4          s              9  2
     D*** Begin Add *** 10/06/15 **********************************
     D***** Beg Chg *** 05/30/16 *********************************
     D***** ##Si            s              1
     D ##Si            s              2
     D***** End Chg *** 05/30/16 *********************************
     D ##Rsch          s              9  2
     D ##Outl          s              9  2
     D*** End   Add *** 10/06/15 **********************************
     D xxmra           s                   like(camra)
     D xxndc           s                   like(dpndc)
     D xxrcn           s                   like(carcn)
     D xxymd           s                   like(ruefdt)
     D kkst            s                   like(rtst)
     D kktype          s                   like(rttype)
     D kkcode          s                   like(hlcode)
     D kkfrrv          s                   like(rrfrrv)
     D xxLOS           s              5  0
     D indate1         s              7  0
     D indate2         s              7  0
     D outdiff         s              5  0
     D outerr          s              1
     D indatein        s              8  0
     D infromfmt       s              8
     D intofmt         s              8
     D Outdate         s             10
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
    C*  Claim Detail File by date to setup follow up days
    C*
     C     caky1         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    kkymd
     C                   kfld                    qqfein
     C                   kfld                    qqmod
    C*
    C*  Claim Detail File read by EOB#
     C     cakyh         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    kkymd
    C*  Claim Detail File read by EOB#
     C     cakyh1        klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
    C*
     C     eoky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
    C*
     C     dhky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    kkdseq
    C*
    C*  HOSPITAL DETAIL BILL FILE (FDDUL)
     C     duky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    kkdseq
    C*
    C*  DENTAL DETAIL BILL FILE (FDDDL)
     C     ddky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    kkdseq
    C*
    C*  PHARMACY DETAIL BILL FILE (FDDPL)
     C     dpky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    kkdseq
    C*
    C*  NDC Drug File
     C     rxky          klist
     C                   kfld                    qqco#
     C                   kfld                    xxndc
    C*
    C*  Procedure Code file
     C     prky          klist
     C                   kfld                    qqco#
     C                   kfld                    xxcpt4
     C                   kfld                    cafdat
    C*
    C* Reasonable & Customary File
     C     rcky          klist
     C                   kfld                    qqco#
     C                   kfld                    cazipa
     C                   kfld                    xxcpt4
     C                   kfld                    kkefdt
    C*
    C*  Usual and Customary Modifier Percent File
    C*
     C     umky          klist
     C                   kfld                    caco#
     C                   kfld                    cacpt4
     C                   kfld                    kkefdt
    C*
    C*  Link to FDRUP to get correct effective date
    C*
     C     ruky          klist
     C                   kfld                    qqst
     C                   kfld                    catype
     C                   kfld                    cafdat
    C*
     C     ruky1         klist
     C                   kfld                    qqst
     C                   kfld                    catype
    C*
     C     svky          klist
     C                   kfld                    qqco#
     C                   kfld                    xxcmsv
     C                   kfld                    cafdat
    C*
     C     frky          klist
     C                   kfld                    qqst
     C                   kfld                    catype
     C                   kfld                    kkefdt
    C*
     C     frky1         klist
     C                   kfld                    qqst
     C                   kfld                    catype
    C*
     C     p2ky          klist
     C                   kfld                    qqfein
     C                   kfld                    qqmod
     C                   kfld                    xxcmsv
     C                   kfld                    kkefdt
    C*
     C     p2ky1         klist
     C                   kfld                    qqfein
     C                   kfld                    qqmod
     C                   kfld                    xxcmsv
    C*
     C     rtky1         klist
     C                   kfld                    kkst
     C                   kfld                    kktype
     C                   kfld                    kkefdt
     C*
     C     rtky2         klist
     C                   kfld                    kkst
     C                   kfld                    kktype
     C*
     C     rvky1         klist
     C                   kfld                    cacpt4
     C                   kfld                    kkefdt
     C                   kfld                    rtmrel
     C*
     C     rvky2         klist
     C                   kfld                    cacpt4
    C*
     C     hlky1         klist
     C                   kfld                    kkcode
     C                   kfld                    kkefdt
     C*
     C     hlky2         klist
     C                   kfld                    kkcode
     C*
     C     rrky          klist
     C                   kfld                    cafdat
     C                   kfld                    kkfrrv
     C*
     C     hcky1         klist
     C                   kfld                    kkst
     C                   kfld                    kkefdt
     C*
     C     hcky2         klist
     C                   kfld                    kkst
     C*
     C     pfky1         klist
     C                   kfld                    kkprov
     C                   kfld                    kkffdt
    C*
     C     pfky2         klist
     C                   kfld                    kkprov
    C*
     C     aaky          klist
     C                   kfld                    cacpt4
    C*
     C     *entry        plist
     C                   parm                    qqco#
     C                   parm                    qqsub#
     C                   parm                    qqfnd
     C                   parm                    qqfyr
     C                   parm                    qqcase
     C                   parm                    qqfein
     C                   parm                    qqmod
     C                   parm                    qqdcs#
     C                   parm                    qqst
     C                   parm                    qqspcd
     C                   parm                    qqeob#
    C*
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
      /Free
           //Main Program Start here
         Open facap;
    
         exsr convrt;
    
          //END PROGRAM AND CLOSE ALL FILES
         Close facap;
    
         Close fdcalh;
           //term          tag
         xcmra = 0;
    
         *inlr = *on;
         Return;
    
         //Convert fields into base fields used for determining payment
         //creates workfile with MRA and modifier amounts and prof/tech
    
         Begsr Convrt;
    
         x = 0;
         clear date;
    
         Chain eoky peu01;
        //Find the recon number.  If this is a recon.
         Chain eoky peo01;
          If %found(faeolg);
             xxrcn = eorcn;
           Else;
             xxrcn = *zero;
           Endif;                                                                //faeolg
    
           kkymd = *zero;

          //Call to FA756R
           If euicn = 'IP' or euicn = 'IM'
             or euicn = 'IR' or euicn = 'IS';

             ##Rrn# = eorrn;
             ##call#  = 'F';

             Callp Calc_Ipps_Pmnt(##Rrn#:##Rtcd:##Allow:##Call#:##Eob#:##Msg#:
                                  ##code);
           Endif;                                                                 //euicn = 'I/P'

         Open fdcalh;
    
         //Main read of detail charge file
         Setll cakyh pca01;
         Reade cakyh1 pca01;
    
           Dow not %Eof(fdcalh);
             If carcn = xxrcn;

                clear pdh01;
                clear pdu01;
                clear pdd01;
                clear pdp01;
                clear mod;
                clear mod1;
                kkdseq = cadseq;
                xxfdat = cafdat;
    
           //If line item is in the R&C file call subroutine RECU
           //Else - treat line item as a procedure code from the CPT4 file

                If eobtyp = 'N' and carcfl = 'C';
    
                  Chain dhky pdh01;
                 If %found(fddhl);
                    cacpt4 = dhprcd;
                    xxcpt4 = dhprcd;

                    exsr pmodld;
                  Endif;
    
                  catype = 'P';
                  camd50 = 1.00;
                  camd51 = 1.00;
                  camd80 = 1.00;
                  camd81 = 1.00;

                  exsr  Physician;
                Else;
    
                  If eobtyp = 'D' and carcfl = 'C';
    
                    Chain ddky pdd01;
                    If %Found(fdddl);
                       cacpt4 = ddprcd;
                       xxcpt4 = ddprcd;
                     Endif;
                    catype = 'D';
    
                    exsr Dent;
    
                  Else;
    
          //Else - treat line item as a pharmacy code
                    If eobtyp = 'X';
    
                      Chain dpky pdp01;
                      If %found(fddpl);
                        candc = dpndc;
                      Endif;
    
                      catype = 'X';
    
                      exsr phar;
    
                    Else;
    
          //Else - treat line item as a service code
                      Chain duky pdu01;
                       If %Found(fddul);
                          casvcd = durvcd;
                          cacpt4 = durate;
                          xxcpt4 = durate;
                          exsr pmodldhos;
                        Endif;
    
                        catype = 'H';
    
                        If eospcd = 'ASC';
                          exsr Ascpmnt;
                        Else;
                          exsr svcd;
                        Endif;                                                  //eospcd = 'ASC'
                    Endif;                                                      //eobtyp = 'X'
                  Endif;                                                        //eobtyp = 'D'
                Endif;                                                          //eobtyp = 'P'
    
         //Initialize work fields
                xxmra = camra;
    
         //If modifiers are present then adjust CAAMT accordingly.  CAAMT
         //is the key field used to process FACAP in highest MRA order.
    
                If mod1 <> *blanks;
                  exsr pmod;
                Endif;
    
         //Add all workfile fields to workfile
         //Load MRA or Charge to CAAMT in highest to lowest MRA order
                If xxmra < cachrg;
                   caamt = xxmra;
                Else;
                   caamt = cachrg;
                Endif;

                If caamt = 0 and
                   (carpt = 'N' or carpt = ' ');
                  carpt = 'Y';
                Endif;
    
        //Setup correct effective date for rules
                Setgt frky pfr01;
                Readpe frky1 pfr01;
                 If not %Eof(fdfrl5);
                    carudt = frefdt;
                  Else;
                    carudt = *hival;
                  Endif;
         //Get the initial effective date of F4000R for this state
                Setgt ruky  pru01;
                Readpe ruky1 pru01;
                 If not %Eof(fdrul1)
                     and ruapgm = 'F4000N';
                     xxymd = ruefdt;
                  Else;
                     xxymd = *hival;
                  Endif;
    
                If xxfdat >= xxymd;
    
                  catotc = cachrg;
                  caspcd = qqspcd;
                  causer = q1user;
                  caiusr = q1user;
                  capgm  = q1pgm;
                  catime = TimeTo6(SystemTime);
                  cadt   = DateTo7(SystemDate);
                  caidt  = DateTo7(SystemDate);

                  write pcawk2;
    
       //Clear FACAP to prevent carryover of values to next record.
       //  Clear CAPAID in detail file to reprocess correctly.
       //   Provider bills is supposed to do this before adjudication
                  If cacscd = '1  '
                     or cacscd = '001';
    
                    clear capaid;
                    camraa = xcmra;
                    update    pca01;
                  Endif;
    
                  clear pcawk2;
                Endif;
    
             Endif;
    
             Reade cakyh1 pca01;
    
           Enddo;
    
         Endsr;
    
       //PHARMACY SUBROUTINE                                  P H A R    *
    
         Begsr phar;
    
       //Default to service date for effective date for rules
         kkefdt = cafdat;
    
       //Test for Fee Schedule Reimbursement = 'Y'
         xxndc = dpndc;
    
           Setgt rxky prx01;
           //nxtpr4        tag
           Readpe rxky prx01;

             Dow not %Eof(fmrxl);
              If rxstat = 'I';
                 Readpe rxky prx01;
               Else;
                 Leave ;
               Endif;
             Enddo;
        //A hard error will cause the program to terminate ASAP.
             If %eof(fmrxl);
               cainvl = 'Y';
             Else;
    
               caunts = dpunts;
               xxefdt = rxawpd;
    
         // Use previous average wholesale price(AWP)
               If cafdat < xxefdt;
    
           //*** Begin Change *** 04/27/18 **********************
           //*** xxawp = rx1awp / 1000;
                 xxawp = rx1awp ;
           //*** End   Change *** 04/27/18 **********************
         //Use current average wholesale price(AWP)
    
               Else;
           //*** Begin Change *** 04/27/18 **********************
           //*** xxawp = rxawp / 1000;
                 xxawp = rxawp;
           //*** End   Change *** 04/27/18 **********************
    
               Endif;
    
               camra = caunts * xxawp;
               caawp = xxawp;
               castat= rxstat;
               cafsr = 'Y';
           //***** Beg Add *** 10/09/19 *************************
               CAptyp = RXptyp;
           //***** End Add *** 10/09/19 *************************
             Endif;
    
         Endsr;
    
         //PROCEDURE CODE MODIFIER CALCULATIONS                  P M O D   *
    
         Begsr  pmod;
    
         //Adjust MRA depending on MOD
            xxIdx = %lookup('50':mod:1);
              If xxIdx <> 0;
                xxmra = camra * camd50;
              Endif;
    
           xxIdx = 0;
           xxIdx = %lookup('51':mod:1);
              If xxIdx <> 0;
                xxmra = camra * camd51;
              Endif;

           xxIdx = 0;
           xxIdx = %lookup('80':mod:1);
              If xxIdx <> 0;
                xxmra = camra * camd80;
              Endif;
    
           xxIdx = 0;
           xxIdx = %lookup('81':mod:1);
              If xxIdx <> 0;
                xxmra = camra * camd81;
              Endif;
    
         Endsr;
    
         //MODIFIER LOADING                                  P M O D L D   *
    
         Begsr pmodld;
    
           camod1 = dhmod1;
           camod2 = dhmod2;
           camod3 = dhmod3;
           camod4 = dhmod4;
    
           mod(1) = dhmod1;
           mod(2) = dhmod2;
           mod(3) = dhmod3;
           mod(4) = dhmod4;
    
           mod1   = mod(1) + mod(2) + mod(3) + mod(4);
    
         Endsr;
    
    
         //MODIFIER LOADING                             P M O D L D H O S  *
    
         Begsr pmodldhos;
    
           camod1 = dupmod;
           camod2 = dumod2;
           camod3 = dumod3;
           camod4 = dumod4;
    
           mod(1) = dupmod;
           mod(2) = dumod2;
           mod(3) = dumod3;
           mod(4) = dumod4;
    
           mod1   = mod(1) + mod(2) + mod(3) + mod(4);
    
         Endsr;
    
    
         //SERVICE CODE SEARCH                                   S V C D   *
    
         Begsr svcd;
    
           kkefdt = cafdat;
           kkst   = eoast;

           Select;
           When euicn = 'HO' or euicn = 'HS'
             or euicn = 'AO' or euicn = 'AS' ;

           ##rrn = carrn;
         //*** Begin Chg *** 10/06/15 *********************************
         //**********  exsr Physician;
           Select;
           When EUicn = 'AO' Or EUicn = 'AS' Or
                ((EUicn = 'HO' Or EUicn = 'HS') And EUdcDt < 1150814);
              exsr Physician;
           When (EUicn = 'HO' Or EUicn = 'HS') And EUdcDt >= 1150814;
              // Calculate OPPS Allowance
              ##Call = 'F';
              CallP Calc_OPPS_Pmnt(##Rrn:##Call:##Si:##Eob:##Msg:##ModR:
                                  ##Alw1:##Rtcd:##Rsch:##Outl);
              // Access Rate Table Information
              kkSt   = EOast;
              kkType = 'O';
              kkEfDt = EUdcDt;
              SetGT rtky1 prt01;
              ReadPE rtky2 prt01;
              Dow not %Eof(ftrtl2);
                 If RTstat = 'I';
                   ReadPE rtky2 prt01;
                 Else;
                   If RTefDt >= 1150814;
                      ##Alw1 = ##Alw1 * RTmul1;
                   EndIf;
                   Leave;
                 EndIf;
              EndDo;
         //*** Beg Add *** 04/12/17 *********************************
              if ##Si = 'A' or ##Si = 'Y';
                 callp Calc_Phy_Pmnt(##Rrn:##Call:##Eob:##Msg:##modr:
                                     ##rnd:##Alw1:##Alw2:##Alw3:##Rtcd);
              endif;
         //*** End Add *** 04/12/17 *********************************
           EndSl;
         //*** End   Chg *** 10/06/15 *********************************
         // Beg Del ****** 08/26/13 ****************************
         //xxalwflg = 'Y';
         //kkfrrv = durvcd;
         //Setgt rrky prr01;
         //Readp prr01;

         //  Dow not %Eof(ftrrl1);
         //    xxefdt = rrefdt;
         //    If not %Eof(ftrrl1)
         //      and (durvcd >= rrfrrv and durvcd <= rrtorv)
         //      and (durate >= rrfrcp and durate <= rrtocp);
         //      xxalwflg = 'N';
         //      leave;
         //    Endif;

         //    Readpe xxefdt prr01;
         //  Enddo;
         //
         //  If xxalwflg = 'Y';
         //     ##alw1 = 0;
         //  Endif;
         // load fdcap fields
         //    If not %Eof(ftrrl1);
         //      If cacpt4 >= '00100' and cacpt4 <= '01999'
         //         and %subst(cacpt4:5:1) <> 'T' and
         //         %subst(cacpt4:5:1) <> 'F'
         //         or cacpt4 = '99100'
         //         or cacpt4 = '99116'
         //         or cacpt4 = '99135'
         //         or cacpt4 = '99140';
         //      Else;
         //         ##alw1 = rtmul1 * ##alw1;
         //      Endif;
         //    Endif;
         // End Del ****** 08/26/13 ****************************
             camra  = ##alw1;
             xcmra  = ##alw1;

             // Beg Chg *** 05/30/16 **************************
             //caapsi = inSi;
             caapcsi = ##Si;
             // End Chg *** 05/30/16 **************************

             castat = 'A';


          //set carpt to 'T' ot 'F' when last char of proc code is 'T' or 'F' respectively
           Select;
           When %subst(cacpt4:5:1) = 'T';
             carpt = 'T';

           other;
             carpt = 'N';
           Endsl;
             cafsr  = 'Y';
             caplac = dupos;
             cacpt4 = durate;
             casvcd = durvcd;
             caunts = duunts;

           When euicn = 'IP' or euicn = 'IR'
             or euicn = 'IM' or euicn = 'IS';

            camra  = *zeros;
            xcmra  = *zeros;
            castat = 'A';
            carpt  = 'N';
            cafsr  = 'Y';
            caplac = dupos;
            cacpt4 = durate;
            casvcd = durvcd;
            caunts = duunts;
    
            cahtos = euicn;
            kkprov = %subst(EUMCAR:1:6);
            kkffdt = dtcheck(CAFDAT);
            Dttomdyy  = kkffdt;
            D1yy = Dtyy;
            D1mm = dtmm;
            D1dd = Dtdd;
            kkffdt = DttoYYmd;
            SetGt pfky1 ptp01;
            Readpe pfky2 ptp01;
            If Not %Eof(ftpfl) and kkprov <> *blanks;

              //Calculate the length of stay.
              indate1 = euaddt;
              indate2 = eudcdt;
              outDiff = 0;
              outErr  = '0';

              callp DatDiff(indate1:indate2:outDiff:outErr);

              If outErr= '0';
                xxLos = outDiff;
              Endif;

            //*** Beg Add *** 09/16/15 *************************************
              // read FTRTP for values
              kkst   = eoast;
              kktype = 'I';
              kkefdt = euaddt;
              Setgt rtky1 prt01;
              Readpe rtky2 prt01;
                Dow not %Eof(ftrtl2);
                  If rtstat = 'I';
                    readpe rtky2 prt01;
                  Else;
                    leave;
                  Endif;
                Enddo;
            //*** End Add *** 09/16/15 *************************************

              Select;

 b01           When xxLos <= 60;
               //*** Beg Chg *** 09/12/12 **********************************
               //   camra = (capaid * 1.33333) + 1132.00;

  b02            If euaddt < 1120728;
                  camra = (capaid * 1.33333) + 1132.00;

                 //*** Beg Chg *** 08/26/13 ********************************

  x02            Else;

                 // Beg Chg *** 08/15/14 ********************************
  b03            //*** If euaddt >= 1130701
  b03              If euaddt >= 1130701
                    and euaddt < 1140719;
                 // End Chg *** 08/15/14 ********************************
                       camra = (capaid * 1.33333) + 1184.00;

                 // Beg Add *** 08/15/14 ********************************

  x03              Else;

                     //*** Beg Chg *** 09/16/15 ****************************
  b04                //*** If euaddt >= 1140719;
  b04                If euaddt >= 1140719
  b04                   and euaddt < 1150814;
                     //*** End Chg *** 09/16/15 ****************************
                       camra = (capaid * 1.33333) + 1216.00;

                     //*** Beg Add *** 09/16/15 ****************************
  x04                Else;

  b05                  If euaddt >= 1150814;
                     //*** Beg Chg *** 10/05/15 ****************************
                     //**** CAmra = CApaid * RTusMult1;
                         CAmra = (CApaid * RTusMult1) + RTusMark1;
                     //*** End Chg *** 10/05/15 ****************************
                     //*** End Add *** 09/16/15 ****************************

                 // End Add *** 08/15/14 ********************************
                 // End Chg *** 08/26/13 ********************************

  x05                  Else;
                         camra = (capaid * 1.33333) + 1156.00;
  e05                  EndIf;
  e04                Endif;
  e03              Endif;
  e02            Endif;

               // End Chg *** 09/12/12 ************************************ *

               When xxLos > 60 and xxLos <= 90;

                //*** Beg Chg *** 09/12/12 *********************************
                //   camra = (capaid * 1.33333) + 1132.00 +
                //           ((xxLos - 60) * 283.00);

 b01             If euaddt < 1120728;
                   camra = (capaid * 1.33333) + 1132.00 +
                         ((xxLos - 60) * 283.00);

                 //*** Beg Chg *** 08/26/13 ********************************

 x01             Else;
                 //*** Beg Chg *** 08/15/14 ********************************
 b02             //*** If euaddt >= 1130701
 b02               If euaddt >= 1130701
                      and euaddt < 1140719;
                 //*** End Chg *** 08/15/14 ********************************

                     camra = (capaid * 1.33333) + 1184.00 +
                             ((xxLos - 60) * 296.00);
                 //*** Beg Add *** 08/15/14 ********************************
 x02               Else;

                     //*** Beg Chg *** 09/16/15 ****************************
 b03                 //*** If euaddt >= 1140719;
 b03                 If euaddt >= 1140719
                        and euaddt < 1150814;
                     //*** End Chg *** 09/16/15 ****************************
                       camra = (capaid * 1.33333) + 1216.00 +
                                ((xxLos - 60) * 304.00);

                     //*** Beg Add *** 09/16/15 ****************************
  x03                Else;

  b04                  If euaddt >= 1150814;
                         CAmra = (CApaid * RTusMult1) +
                                  RTusMark1 +
                                 ((xxLos - 60) * RTusLosMk1);

                     //*** End Add *** 09/16/15 ****************************
                 //*** End Add *** 08/15/14 ********************************
                 //*** End Chg *** 08/26/13 ********************************
 x04                   Else;
                         camra = (capaid * 1.33333) + 1156.00 +
                                 ((xxLos - 60) * 289.00);
 e04                   Endif;

 e03                 Endif;

 e02               endif;

 e01             endif;

              //*** End Chg *** 09/12/12 ***********************************

               When xxLos > 90;

               //*** Beg Chg *** 09/12/12 **********************************
               //   camra = (capaid * 1.33333) + 9622.00 +
               //           ((xxLos - 90) * 566.00);

 b01             If euaddt < 1120728;
                   camra = (capaid * 1.33333) + 9622.00 +
                           ((xxLos - 90) * 566.00);

                 //*** Beg Chg *** 08/26/13 ********************************
                 //*** Beg Chg *** 08/15/14 ********************************

 x01             Else;
                 //*** Beg Chg *** 08/15/14 ********************************
 b02             //*** If euaddt >= 1130701
 b02               If euaddt >= 1130701
                      and euaddt < 1140719;
                 //*** End Chg *** 08/15/14 ********************************
                     camra = (capaid * 1.33333) + 10064.00 +
                             ((xxLos - 90) * 592.00);

 x02               Else;

                     //*** Beg Chg *** 09/16/15 ****************************
 b03                 //*** If euaddt >= 1140719;
 b03                 If euaddt >= 1140719
                        and euaddt < 1150814;
                     //*** End Chg *** 09/16/15 ****************************
                        camra = (capaid * 1.33333) + 10366.00 +
                                ((xxLos - 90) * 608.00);


                     //*** Beg Add *** 09/16/15 ****************************
 x03                 Else;

 b04                   If euaddt >= 1150814;
                         CAmra = (CApaid * RTusMult1) +
                                  RTusMark2 +
                                 ((xxLos - 90) * RTusLosMk2);

                     //*** End Add *** 09/16/15 ****************************
                 //*** End Chg *** 08/15/14 ********************************
                 //*** End Chg *** 08/26/13 ********************************

 x04                   Else;
                          camra = (capaid * 1.33333) + 9826.00 +
                                     ((xxLos - 90) * 578.00);
 e04                   Endif;

 e03                 endif;

 e02               endif;

 e01             Endif;

         // End Chg *** 09/12/12 *************************************
              Endsl;
            Else;
              kkst = 'PA';
              kkefdt = eofrdt;
              Setgt hcky1 phc01;
              Readpe hcky2 phc01;
              camra = eochrg * hcccr * 1.26;
            Endif;
           Endsl;

         Endsr;

         Begsr Ascpmnt;

           ##Rrn  = carrn;
           ##call = 'F';
           ##type = 'M';
           callp Calc_Asc_Pmnt(##Rrn:##Call:##Type:##Pi:##Eob:##Msg:
                               ##Alw1:##Alw2:##Alw3:##Alw4:##Rtcd);
           //*** Beg Add *** 11/05/18 *******************************
           If CAfDat >= 1181015;
              callp Calc_Phy_Pmnt(##Rrn:##Call:##Eob:##Msg:##modr:
                               ##rnd:##Alw1:##Alw2:##Alw3:##Rtcd);
           Endif;
           //*** End Add *** 11/05/18 *******************************

           kkefdt = cafdat;
           caaspi = ##Pi;
           castat = 'A';

           Select;
           When %subst(cacpt4:5:1) = 'T';
             carpt = 'T';

           other;
             carpt = 'N';
           Endsl;
           cafsr  = 'Y'  ;
           caplac = dupos;
           cacpt4 = durate;
           caunts = duunts;

           If cacpt4 >= 'A0000' and cacpt4 <= 'Z9999';
             caanes = 'U';
           Endif;

           kkst   = eoast;
           kktype = 'A';
           Setgt rtky1 prt01;
           Readpe rtky2 prt01;
             Dow not %Eof(ftrtl2);
               If rtstat = 'I';
                 readpe rtky2 prt01;
               Else;
                 leave;
               Endif;
             Enddo;

           Chain aaky paa01;
             If not %found(ftaal1);
                camra = 0;
                xcmra = 0;
             Endif;
           //***** Beg Add *** 03/30/17 ******************************
             If %found(ftaal1);
                If AAAncill = 'Y';
                   CArpt = 'B';
                EndIf;
             Endif;
           //***** End Add *** 03/30/17 ******************************
             camra = ##Alw1;
             xcmra = ##Alw1;

         Endsr;

          // Physician Subroutine
         BegSr Physician;

           ##Rrn = carrn;
           ##call= 'F';
           ##rnd = 'M';
           ##modr= ' ';

           callp Calc_Phy_Pmnt(##Rrn:##Call:##Eob:##Msg:##modr:
                               ##rnd:##Alw1:##Alw2:##Alw3:##Rtcd);

           kkefdt = cafdat;
           castat = 'A';

           Select;
           When %subst(cacpt4:5:1) = 'T';
             carpt = 'T';

           Other;
             carpt = 'N';
           Endsl;
           cafsr  = 'Y';

           Select;
           When eobtyp = 'N';
           caplac = dhpos;
           cacpt4 = dhprcd;
           capart = dharea;
           caunts = dhunts;
           If eospcd <> 'ASC';
              kktype = 'M';
           Else;
              kktype = 'A';
           Endif;

           When eobtyp = 'H';
           caplac = dupos;
           cacpt4 = durate;
           caunts = duunts;
           kktype = 'O';

           Endsl;

           If cacpt4 >= 'A0000' and cacpt4 <= 'Z9999';
             caanes = 'U';
           Endif;
           If cacpt4 >= '00100' and cacpt4 <= '01999'
              and %subst(cacpt4:5:1) <> 'T' and
              %subst(cacpt4:5:1) <> 'F'
              or cacpt4 = '99100'
              or cacpt4 = '99116'
              or cacpt4 = '99135'
              or cacpt4 = '99140';
             Select;
             When eobtyp = 'N';
             caanmn = dhunts;

             When eobtyp = 'H';
             caanmn = duunts;
             Endsl;

             caanes = 'Y';
             caanam = ##alw2;

             If caanam <> 0;
               camra  = ##alw1;
             Else;
                camra  = 0;
                ##alw1 = 0;
             Endif;
           Endif;

          //Chain to FMHLP so that we can load follow up days, indicators etc.
           kkcode = cacpt4;
           kkst   = eoast;
           Setgt rtky1 prt01;
           Readpe rtky2 prt01;
             Dow not %Eof(ftrtl2);
               If rtstat = 'I';
                 Readpe rtky2 prt01;
               Else;
                 Leave;
               Endif;
             Enddo;

           If not %Eof(ftrtl2);
             If cacpt4 >= '00100' and cacpt4 <= '01999'
                and %subst(cacpt4:5:1) <> 'T' and
                %subst(cacpt4:5:1) <> 'F'
                or cacpt4 = '99100'
                or cacpt4 = '99116'
                or cacpt4 = '99135'
                or cacpt4 = '99140';
             Else;
               If  eospcd= 'ASC';
                 Chain aaky paa01;
                   If %found(ftaal1);
                     ##alw1 = rtmul1 * ##alw1;
          //*** Beg Add *** 10/09/19 ***************************************
                     If AAancill = 'Y';
                        CArpt = 'B';
                     Endif;
          //*** End Add *** 10/09/19 ***************************************
                   Else;
                     ##alw1 = 0;
                   Endif;
               Endif;
             Endif;
           Endif;
           camra = ##alw1;
           xcmra = ##alw1;
           kkcode = cacpt4;

           Setgt hlky1 phl01;
           Readpe hlky2 phl01;
             Dow not %Eof(fmhll2);
               If hlstat = 'I';
                 Readpe hlky2 phl01;
               Else;
                 leave;
               Endif;
             Enddo;

             If not %Eof(fmhll2);

               camd50 = 0.00;
               camd51 = 0.00;
               camd62 = 0.00;
               camd66 = 0.00;
               camd80 = 0.00;
               camd81 = 0.00;
               camd82 = 0.00;
               caenba = rvenba;
               If  %check(' 0123456789': hlglob) > 0;
               Else;
             //*** Beg Add *** 10/12/17 ************************************
                 Monitor;
             //*** End Add *** 10/12/17 ************************************
                 cafolu = %int(hlglob);
             //*** Beg Add *** 10/12/17 ************************************
                 On-error;
                    cafolu = 0;
                 Endmon;
             //*** End Add *** 10/12/17 ************************************
               Endif;
               ca50in = hlbila;
               ca51in = hlmult;
               ca62in = hlcosg;
               ca66in = hltesg;
               ca80in = hlasst;

               If ca50in = '1';
                  camd50= 1.5;
               Endif;

               If ca51in = '1';
                  camd51= 0.25;
               Endif;

               If ca51in = '2';
                  camd51= 0.5;
               Endif;

               If ca51in = '3';
                  camd51= 0.5;
               Endif;

               If ca62in = '1' or ca62in = '2';
                  camd62 = 6.25;
                  xxmd62 = 0.625;
               Endif;
    
               If ca66in = '1' or ca66in = '2';
                  camd66 = 1.00;
               Endif;
    
               If ca80in = '2';
                  camd80 = 0.16;
                  camd81 = 0.25;
                  camd82 = 0.13;
               Endif;

             Endif;

         Endsr;

          // Dental Code MRA Search

         BegSr Dent;

           kkefdt = cafdat;
           castat = 'A';
           carpt  = 'N';
           cafsr  = 'Y';
           caplac = ddpos;
           cacpt4 = ddprcd;
           capart = ddpart;
           caunts = ddunts;

           If cacpt4 >= 'A0000' and cacpt4 <= 'Z9999';
             caanes = 'U';
           Endif;
           If cacpt4 >= '00100' and cacpt4 <= '01999'
              and %subst(cacpt4:5:1) <> 'T' and
              %subst(cacpt4:5:1) <> 'F'
              or cacpt4 = '99100'
              or cacpt4 = '99116'
              or cacpt4 = '99135'
              or cacpt4 = '99140';
             caanmn = ddunts;
             caanes = 'Y';
             caanam = ##alw2;

             If caanam <> 0;
               camra  = ##alw1;
             Else;
                camra  = 0;
                ##alw1 = 0;
             Endif;
           Endif;

          //Chain to FMHLP so that we can load follow up days, indicators etc.
           kkcode = cacpt4;
           kktype = 'M';
           kkst   = eoast;

             If cacpt4 >= '00100' and cacpt4 <= '01999'
                and %subst(cacpt4:5:1) <> 'T' and
                %subst(cacpt4:5:1) <> 'F'
                or cacpt4 = '99100'
                or cacpt4 = '99116'
                or cacpt4 = '99135'
                or cacpt4 = '99140';
             Endif;

           Setgt hlky1 phl01;
           Readpe hlky2 phl01;
             Dow not %Eof(fmhll2);
               If hlstat = 'I';
                 Readpe hlky2 phl01;
               Else;
                 leave;
               Endif;
             Enddo;

           camra = hlwkrv;
           xcmra = hlwkrv;
             If not %Eof(fmhll2);

               camd50 = 0.00;
               camd51 = 0.00;
               camd62 = 0.00;
               camd66 = 0.00;
               camd80 = 0.00;
               camd81 = 0.00;
               camd82 = 0.00;
               caenba = rvenba;
               If  %check(' 0123456789': hlglob) > 0;
               Else;
                 cafolu = %int(hlglob);
               Endif;
               ca50in = hlbila;
               ca51in = hlmult;
               ca62in = hlcosg;
               ca66in = hltesg;
               ca80in = hlasst;

             Endif;
         Endsr;
