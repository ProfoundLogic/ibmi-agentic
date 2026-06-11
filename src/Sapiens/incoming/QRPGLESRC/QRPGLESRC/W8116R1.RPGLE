     Hcopyright('(c) 2024 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H DFTACTGRP(*no) ACTGRP(*caller)
     H/TITLE W8116R1 - Madison Data Download Build Workfile
      **************************************************************************************
      *  Description: Reads WDELP in co#/grp order and loads the Policy Header information *
      *               for each;  it loads the exposure info for each payroll class and for *
      *               each premium factor; it reads WMCMP by co#/grp/emp#/yr for each      *
      *               policy and loads the claim information.                              *
      *                 note: divisions are all grouped together under state reporting div *
      *                                                                                    *
      *  Called from: W8116C1                                                              *
      *                                                                                    *
      *  Calls to:                                                                         *
      *                                                                                    *
      *  Created: 01/17/00           Programmer C:87          Project:WC964                *
      **************************************************************************************
      *  Revisions:                                                                        *
      *                                                                                    *
      * 06/17/24 C:344 Project: WC2952                                                     *
      *                Increase Rate field to zoned 8.3                                    *
      *                                                                                    *
      * 08/13/18 C:53  Project: WC1917                                                     *
      *                Part code field change                                              *
      *                                                                                    *
      * 10/20/15 C:302 PROJECT: WC2364
      *                Add non-ratable class code processing
      *
      * 11/25/13 C:282 PROJECT 129WSC                                                      *
      *                Load employers liability PTD by RTCTGY                              *
      *                for cltid 129 only.                                                 *
      *                                                                                    *
      * 05/30/13 C:287 Support: WA099 80909                                                *
      *                Add Large Deductible logic                                          *
      *                                                                                    *
      * 03/22/10 C:237 SUPPORT SEQ71362                                                    *
      *                FIX ERROR IN MADISON EXTRACT PROCESS.                               *
      *                ACCOUNTS ARE FLAGGED WITH AN EXPOSURE RECORD                        *
      *                AS 1111 FOR FLAT CANCELLATION WHERE EFF DATE                        *
      *                EQUALS EXP DATE AND PRM=0.00                                        *
      *                                                                                    *
      *  01/23/09 C:65 PROJECT SUPP#66749                                                  *
      *                Expand week array to 5 digits for long claims.                      *
      *                                                                                    *
      * 10/11/07 C:190 SUPPORT WA099 54504                                                 *
      *                MAKE SURE TO DO NOT ACCESS THE DEDUCTIBLE                           *
      *                DISCOUNT CODE HAVING INACTIVE RECORDS.                              *
      *                                                                                    *
      *  04/10/06 C:65 PROJECT SUPP#51293                                                  *
      *                CHANGE DEDUCTIBLE DISCOUNT TO PULL BASED ON                         *
      *                ANNIV RATING DATE INSTEAD OF POLICY EFF DATE.                       *
      *                                                                                    *
      * 09/21/05 C:15   Support: WC109/48955                                               *
      *                 Chgd pgm to retrieve the correct "estimated/audited" status for    *
      *                 specific division being processed.                                 *
      *                                                                                    *
      * 02/08/05 C:136  Project: WC1237                                                    *
      *                 Add voluntary labor charge. Project originally added 09/01/04.     *
      *                                                                                    *
      *  01/07/05 C:136  Project: WC1250                                                   *
      *                  Write type exposure records for domestic terrorism surcharge.     *
      *                                                                                    *
      *  11/16/04  C:15  Support: WC121/44642                                              *
      *                  Added code to load the previous policy # (POPPOL)                 *
      *                                                                                    *
      *  09/02/04  C:15  Support: WC121/43623                                              *
      *                  Removed the expense constant amt from the mod_prem amt.  This     *
      *                  corrected the issue of minimum premium records not being reported *
      *                  on minimum premium accounts.                                      *
      *                                                                                    *
      *  05/18/04  C:15  Project: WC1060  Seq# 42528                                       *
      *                  If the 'Exclude Medical' flag is 'Y' on a policy, set the Type of *
      *                  Coverage to '02' regardless of the jurisdiction state.            *
      *                                                                                    *
      *  03/11/04  C:15  Project: WC1077  Seq# 41605                                       *
      *                  Added code to load new fields in WXWKP2.  This change is based on *
      *                  Madison Manual version 5.1                                        *
      *                                                                                    *
      *  12/29/03 C:136  Project: WC1120                                                   *
      *                  Write type exposure records for terrorism surcharge.              *
      *                                                                                    *
      *  11/12/03  C:15  Support: WC109/39738                                              *
      *                  Fixed code for the 3 assessment fees.                             *
      *                                                                                    *
      *  09/12/03  C:15  Support: WC109/38595                                              *
      *                  Added code for the waiver surcharge and the 3 assessment codes.   *
      *                                                                                    *
      *  08/07/03  C:15  Support: WC101/38151                                              *
      *                  Added code to correct the policy eff dt and the state eff dt.     *
      *                                                                                    *
      *  07/17/03  C:53  Support: WC065/38013                                              *
      *                  Correct weeks/days calc.  When a neg recovery                     *
      *                  is processed, the weeks/days should be added.                     *
      *                                                                                    *
      *  05/30/03  C:15  Project: WC1131                                                   *
      *                  Added code to load the new location file (WXLOP).                 *
      *                                                                                    *
      *  04/25/03  C:15  Support: WC109/36986                                              *
      *                  Chgd code to prevent payroll class codes from not being extracted.*
      *                                                                                    *
      *  11/12/02  C:15  Support: WC095/34834                                              *
      *                  Added the legal paid and incurred amounts to the ALAE amounts.    *
      *                                                                                    *
      *  10/07/02  C:15  Project: WC1077                                                   *
      *                  Added code to look at the employee count in WDELP when determining*
      *                  the location with the most employees.                             *
      *                                                                                    *
      *  09/27/02  C:142 Project: WC1077                                                   *
      *                  Added new fields for Wxwkp2 and changed Wxwkp to  Wxwkp2.         *
      *                                                                                    *
      *  08/02/02  C:15  Support: WC121/33627                                              *
      *                  Added code to correct the total written premium calc reported on  *
      *                  the transmittal letter.                                           *
      *                                                                                    *
      *  07/26/02  C:15  Project: WC1077                                                   *
      *                  Added code to load employee count and zip code of the location    *
      *                  with the most employees for each employer.                        *
      *                                                                                    *
      * 12/12/01  C:53  PROJECT: WC1060                                                    *
      *                 ADD EXCLUDE MEDICAL PROCESSING.                                    *
      *                                                                                    *
      *  03/20/02  C:15  Project:  WC964                                                   *
      *                  Added a report that lists totals needed for the transmittal       *
      *                  letter.                                                           *
      *                                                                                    *
      *  03/15/02  C:15  Project:  WC1031   seq# 26262                                     *
      *                  On min prem accts, get the other exposure figures from WDMPP, and *
      *                  load the standard prem from WDMPP if the total exposure is        *
      *                  greater than the min prem.                                        *
      *                                                                                    *
      *  03/14/02  C:15  Project:  WC591                                                   *
      *                  Added code to write an exposure record with code '1111' when no   *
      *                  payroll exposure has been produced.                               *
      *                                                                                    *
      *  02/19/02  C:15  Support: WC095/31239                                              *
      *                  Added code to include claims that only have expense incurred.     *
      *                                                                                    *
      *  04/27/01  C:15  Support: WC101/27185                                              *
      *                  Added code to load the efdt before retrieving the inc limit       *
      *                  record.                                                           *
      *                                                                                    *
      *  02/16/01  C:15  Support: WC101/26373                                              *
      *                  Chgd code to prevent a decimal-data error.  EOCLAS is alpha but   *
      *                  KKCLAS in numeric.                                                *
      *                                                                                    *
      *  01/10/01  C:15  Project: supp WC033 #25788                                        *
      *                  Changed pgm to truncate the decimals when loading WKlegP, since   *
      *                  WKLEGP is 9A and the paid legal work field is 9.2N.               *
      *                                                                                    *
      *  05/17/00  C:87  Project: supp WA100 #22832 / WC964                                *
      *                  Changed process to write records based on rating state for multi  *
      *                  state pols (was writing all recs based on state reporting div).   *
      *                  Added logicals for emp detail and Madison policy/exposure files.  *
      *                  Chgd emp pyrl and clm master files to process.                    *
      *                                                                                    *
      *  05/01/00  C:87  Project: WC964                                                    *
      *                  Added Group Download Workfile WXFNP.  Change processing to use    *
      *                  WXFNP to load files - use group if the flag (FNFLAG) equals 'I'.  *
      *                                                                                    *
      *  04/27/00  C:87  Project: WC964                                                    *
      *                  Added field WKLEGI for 'legal incurred'.                          *
      *                                                                                    *
      *  03/15/00  C:87  Project: supp WA101 #21886                                        *
      *                  Payrolls were not being split because pgm was passing wrong to-   *
      *                  date parm to split payroll procedure.  Also, if a second div did  *
      *                  not have same pol dates as state reporting div payrolls were not  *
      *                  being calc'd correctly.  Added chain to co/fnd/emp/div to get     *
      *                  the correct pol to date.                                          *
      *                  Chg'd sub-procedure that pro-rates values to handle an extra parm *
      *                  so that values are calc'd properly.                               *
      *                                                                                    *
      *  02/17/00  C:87  Project: supp WA101 #21886                                        *
      *                  Class code was not being written to file because select struct    *
      *                  was not formatted correctly.  Also, added conditions for expense  *
      *                  constant and stock discount to 'other exposure' process.          *
      *                  Add processing to handle short rated premium.                     *
      *                                                                                    *
      **************************************************************************************
     F
     Fwdell     if   e           k disk                                         emp detail
     Fwdell1    if   e           k disk    Rename(pel01 : pel02) Prefix(el2:2)
     FWdell5    if   e           k disk    Rename(pel01 : pel05) Prefix(el5:2)
     FWdell8    if   e           k disk    Rename(pel01 : pel08) Prefix(El8:2)
     Fwmeml     if   e           k disk                                         emp master
     Fwmrsl     if   e           k disk                                         rate set code
     Fwdehl     if   e           k disk                                         emp history
     Fwdf2l     if   e           k disk                                         fund detail
     Fststl1    if   e           k disk                                         state code table
     Fwmepl1    if   e           k disk
     Fwmepl7    if   e           k disk    Rename(pep01 : pep02) Prefix(ep2:2)
     Fwmprl1    if   e           k disk                                         payroll class
     Fwmrpl     if   e           k disk                                         carrier/state rptg
     Fwtddl2    if   e           k disk                                         deductible discount
     Fwdddl1    if   e           k disk    Rename(pdd01 : pd201) Prefix(dd2:2)  deductible detail
     Fwmcml     if   e           k disk                                         claim master
     Fwmchl     if   e           k disk                                         claim history
     Fwtabl     if   e           k disk    Include(ppa01 : pna01 : pca01 :      table file
     F                                             pb201 : prt01 : pbs01)
     Fwmldl1    if   e           k disk    Rename(pld01 : pld02)
     Fwtgtl     if   e           k disk                                         generic table file
     Fwmlll     if   e           k disk                                         claim legal master
     Fwtlml1    if   e           k disk                                         limits table file
     Fwdrrl     if   e           k disk                                         state rptg/rating
     FWdpal5    if   e           k disk                                         state rptg/rating
     F                                     Rename(ppa01 : ppa011)
     FWdacl     if   e           k disk                                         state rptg/rating
     FWmpdl     if   e           k disk                                         state rptg/rating
     FWmcdl     if   e           k disk                                         state rptg/rating
     Fwxfnl     if   e           k disk                                         workfile
     Fwdmpl     if   e           k disk                                         premium detail

     Fwxpop     uf a e             disk                                         policy download
     Fwxeop     uf a e             disk                                         exposure download
     FWxwkp2    uf a e             disk                                         claims download
     Fwxpol     if a e           k disk    Rename(ppo01 : ppo02) Prefix(po2:2)
     Fwxeol     uf   e           k disk    Rename(peo01 : peo02) Prefix(eo2:2)
     Fwmall     if   e           k disk
     Fwdldl     if   e           k disk
     Fwxlop     uf a e             disk                                         location download
     Fwzexl     if   e           k disk                                         premium detail
     Fssylf     if   e           k disk
     F                                     ignore(SPC01)
     F                                     ignore(SFB01)
     F                                     ignore(SRC01)
     F                                     ignore(SDR01)
     F                                     ignore(SBC01)
     F                                     ignore(SCS01)
     F                                     ignore(SGL01)
     F                                     ignore(SP201)
      *
     Fqsysprt   o    f   85        printer oflind(*inof)

     D xxclas          s                   like(DDCLAS)
     D xxcode          s                   like(DDCODE)
     D xxplan          s                   like(DDPLAN)
     D xxampo          s                   like(DDAMPO)
     D xxamag          s                   like(DDAMAG)
     D xxDedSize       s              1

      *---------------------------------------
      *  Procedure prototypes
      *---------------------------------------
     D rsv_typ         pr             2
     D   cod_arr                      2    dim(10) Const

     D split           pr
     D   ##co#                        3  0 Const
     D   ##fnd                        3  0 Const
     D   ##fyr                        3  0 Const
     D   ##emp#                       9  0 Const
     D   ##div                        5  0 Const
     D   ##clas                       5  0 Const
     D   ##spec                       1    Const
     D   ##idx                        3  0 Const
     D   ##eamt                      11  2
     D   ##prem                      11  2
     D**** Beg Chg **** 06/17/24 *******************************************
     D****##arat                       7  2 Const
     D   ##arat                       8S 3 Const
     D**** Beg Chg **** 06/17/24 *******************************************
     D   ##efdt                       7  0 Const
     D   ##beg_dt                     7  0 Const
     D   ##end_dt                     7  0 Const
     D   ##percap                     1    Const

     D pro_rate        pr            11  2
     D   fdt                          7  0 Const
     D   tdt                          7  0 Const
     D   stdt                         7  0 Const
     D   endt                         7  0 Const
     D   amount                      11  2 Const

     D wrt_xprem       pr
     D   code                         4    Const
     D   amt                         11  2 Const

     D rtvDedSize      pr             1
     D  co#                                like(ELCO#)
     D  bscd                               like(ELBSCD)
     D rtvSmDedInfo    pr
     D  clas                               like(DDCLAS)
     D  code                               like(DDCODE)
     D  plan                               like(DDPLAN)
     D  ampo                               like(DDAMPO)
     D  amag                               like(DDAMAG)
     D rtvLgDedInfo    pr
     D  clas                               like(DDCLAS)
     D  plan                               like(DDPLAN)
     D  ampo                               like(DDAMPO)
     D  amag                               like(DDAMAG)
      *---------------------------------------
      *  end procedure prototypes
      *---------------------------------------

      *---------------------------------------
      *  data structures
      *---------------------------------------
     D                 DS
     D expmod                  1     15  4 dim(3)
     D  el2exm1                1      5  4 inz
     D  el2exm2                6     10  4 inz
     D  el2exm3               11     15  4 inz
     D                 DS
     D effmod                  1     21  0 dim(3)
     D  elemd1                 1      7  0 inz
     D  elemd2                 8     14  0 inz
     D  elemd3                15     21  0 inz


     D                 DS
     D EffDate                 1     35  0 dim(5)
     D  emefd1                 1      7  0 inz
     D  emefd2                 8     14  0 inz
     D  emefd3                15     21  0 inz
     D  emefd4                22     28  0 inz
     D  emefd5                29     35  0 inz

     D                 DS
     D CxDate                  1     35  0 dim(5)
     D  emcad1                 1      7  0 inz
     D  emcad2                 8     14  0 inz
     D  emcad3                15     21  0 inz
     D  emcad4                22     28  0 inz
     D  emcad5                29     35  0 inz

      *  Deductible type data structure
     D ded_type        DS
     D  ded_ty12               1      2
     D  ded_ty34               3      4

     D                 DS
     D ResvCod                 1     20    dim(10)
     D  cmrt01                 1      2
     D  cmrt02                 3      4
     D  cmrt03                 5      6
     D  cmrt04                 7      8
     D  cmrt05                 9     10
     D  cmrt06                11     12
     D  cmrt07                13     14
     D  cmrt08                15     16
     D  cmrt09                17     18
     D  cmrt10                19     20

     D                 DS
     D PayRsv                  1     90  2 dim(10)
     D  cmar01                 1      9  2
     D  cmar02                10     18  2
     D  cmar03                19     27  2
     D  cmar04                28     36  2
     D  cmar05                37     45  2
     D  cmar06                46     54  2
     D  cmar07                55     63  2
     D  cmar08                64     72  2
     D  cmar09                73     81  2
     D  cmar10                82     90  2

     D                 DS
     D RemRsv                  1     90  2 dim(10)
     D  cmrr01                 1      9  2
     D  cmrr02                10     18  2
     D  cmrr03                19     27  2
     D  cmrr04                28     36  2
     D  cmrr05                37     45  2
     D  cmrr06                46     54  2
     D  cmrr07                55     63  2
     D  cmrr08                64     72  2
     D  cmrr09                73     81  2
     D  cmrr10                82     90  2

      *  Deductible reserve codes
     D                 DS
     D DedRsvCod               1     20    dim(10)
     D  dddc01                 1      2
     D  dddc02                 3      4
     D  dddc03                 5      6
     D  dddc04                 7      8
     D  dddc05                 9     10
     D  dddc06                11     12
     D  dddc07                13     14
     D  dddc08                15     16
     D  dddc09                17     18
     D  dddc10                19     20

      *  Loss condition codes (from generic table file)
     D                 DS
     D GTCF01                  1     10
     D  Act                    1      2
     D  Typ_los                3      4

      *  Loss conditions data structure
     D                 DS
     D polcon                  1      7
     D  threeyr                1      1
     D  multist                2      2
     D  inter_st               3      3
     D  estexpo                4      4
     D  retro_rt               5      5
     D  cx_mid                 6      6
     D  mco                    7      7
      *---------------------------------------
      *  end data structures
      *---------------------------------------

      *---------------------------------------
      *  arrays
      *---------------------------------------
      *  arrays for spliting employer payrolls by periods
     D clscd                          5  0 dim(5) inz
     D expmd                          5  4 dim(5) inz
     D pefdt                          7  0 dim(5) inz
     D expamt                        11  0 dim(5) inz
     D pramt                         11  0 dim(5) inz
     D manrt                          7  2 dim(5) inz
     D splitp                         1  0 dim(5) inz
     D rttier                         2    dim(5) inz
     D expact                         2    dim(5) inz
     D
     D mod                            5  4 dim(5) inz
     D rate_dt                        7  0 dim(5) ascend
     D
     D  #oexp          c                   const(19)                            voluntary labor chg
     D othexp          s             12  2 dim(#oexp)                           Exposure-not payroll
      *---------------------------------------
      *  end  arrays
      *---------------------------------------

      *---------------------------------------
      *  stand alone fields
      *---------------------------------------
     D #years          s              3  0
     D end_dt          s              7  0
     D idx             s              3  0
     D i               s              3  0
     D j               s              3  0
     D k               s              3  0
     D m               s              3  0
     D n               s              3  0
     D xxeamt          s                   like(epeapr)
     D xxprem          s                   like(epeapr)
     D savclas         s                   like(epclas)
     D hld_jrst        s                   like(rsjrst)
     D juris_st        s                   like(rsjrst)
     D Paamt2          S                   Like(Paamt)
     D Risk_St         S                   Like(Rsjrst)
     D arr_sum         s             10  2
     D percap          s              1
     D mod_prem        s             11  2
     D min_prem        s              9  0
     D prem_ttl        s             11  2
     D ndays           s              3  0
     D beg_dt          s              7  0
     D addtl_ld        s             12  2
     D mainState#      s              2
     D mainDiv         s                   like(eldiv)
     D kkelid          s                   like(alelid)
     D xxst            s                   like(emmst)
     D xxempy          s              7  0
     D xxloc#          s              3  0
     D xxoccd          s                   like(proccd)
     D*** Beg Add * 10/20/15
     D xxnrat          s                   like(prnrat)
     D*** End Add * 10/20/15
     D svfnd           s                   like(elfnd)
     D svemp#          s                   like(elemp#)
     D svdiv           s                   like(eldiv)
     D svfyr           s                   like(elfyr)

      *   *key fields*
     D  kkco#          s                   like(elco#)
     D  kkfnd          s                   like(elfnd)
     D  kkfyr          s                   like(elfyr)
     D  kkemp#         s                   like(elemp#)
     D  kkdiv          s                   like(eldiv)
     D  kkcase         s                   like(cmcase)
     D  kkclas         s                   like(epclas)
     D  kkdcde         s                   like(eldcde)
     D  kkdcef         s                   like(eldcef)
     D  kkefdt         s                   like(prefdt)
     D  kkkey          s                   like(gtkey)
     D  kkrtcd         s                   like(rsrtcd)
     D  kkst           s                   like(ddst)
     D  kktbid         s                   like(gttbid)
     D  kkcarr         s                   like(pocarr)
     D  kkpol#         s                   like(popol#)
     D  kkcert         s                   like(pocert)
     D  kkstcd         s                   like(postcd)
     D  kkpfdt         s                   like(popfdt)
     D  kkcls          s                   like(eoclas)
     D  kkemod         s                   like(eoemod)
     D  kkeffd         s                   like(eoefdt)
     D  kkrate         s                   like(eorate)
     D  kkeact         s                   like(eoeact)
     D  k2div          s                   like(eldiv)
     D
      *   *link*
     D carrier#        s              5
     D policy#         s             18
     D cert#           s              7
     D state#          s              2
     D poleffdt        s              6
     D
      *   *header*
     D ded_pct         s              2  0
     D
      *   *exposure*
     D exp_md          s              4  3
     D exp_amt         s             10  0
     D pr_amt          s              9  0
     D man_rt          s              7  3
      * see splitp array
      * see rttier array
      * see expact array
     D
      *   *claims*
     D   case#         s              7
     D   indem         s              9  0
     D   medic         s              9  0
     D   paidin        s              9  0
     D   paidmd        s              9  0
     D   ded_sum       s              9  0
     D   ded_bill      s              9  0
     D   alaepd        s              9  0
     D   alaein        s              9  0
     D   leglin        s              9  0
     D   paidle        s              9  0
     D

     D IndemRem        s             11  2
     D IndemPay        s             11  2
     D MedicRem        s             11  2
     D MedicPay        s             11  2
     D AlaeRem         s             11  2
     D AlaePay         s             11  2
     D LegalPay        s             11  2
     D LegalRem        s             11  2
     D Wkinct#         S             11  2
     D WKALSB#         S             11  2
     D WKDISP#         S             11  2
     D WKDTHP#         S             11  2
     D WKEWPTD#        S             11  2
     D WKSSAM#         S             11  2
     D WKHOSP#         S             11  2
     D WKLEGC#         S             11  2
     D WKLEGD#         S             11  2
     D WKOTSB#         S             11  2
     D WKPHPD#         S             11  2
     D WKPRMP#         S             11  2
     D WKPTPD#         S             11  2
     D WKRHBP#         S             11  2
     D WKRHPB#         S             11  2
     D WKSTLA#         S             11  2
     D WKTGI#          S             11  2
     D WKVEVL#         S             11  2
     D WKVIND#         S             11  2
     D WKVRHBI#        S             11  2
     D WKVRHI#         S             11  2
     D WKVRHED#        S             11  2
     D WKVRHEV#        S             11  2
     D WKVRHM#         S             11  2
     D WKVRHO#         S             11  2
     D WKVTRN#         S             11  2
     D WKPLSB#         S             11  2
     D xthcnt          s              7  0
     D xtecnt          s              7  0
     D xtlcnt          s              7  0
     D xteamt          s             13  0
     D xtprem          s             13  0
     D xtiinc          s             13  0
     D xtminc          s             13  0
     D xtinpd          s             13  0
     D xtmdpd          s             13  0
     D xtexpp          s             13  0
     D xtexpi          s             13  0

      * Numeric work fields
     D Num2            S              2  0
     D Num3            S              3  0
     D Num5            S              5  0
     D Num9            S              9  0
     D Num11           S             11  0
     D H5              S              5
     D Wkfptd#         S              9  2
     D Wklptd#         S              9  2
     D Wklptdc#        S              9  2
     D Wklptde#        S              9  2
     D Wkhptd#         S              9  2
     D Wkpptd#         S              9  2
     D Wkoptd#         S              9  2
     D Wklsum#         S              9  2
     D Wkpnptd#        S              9  2
     D xxecnt          s              7  0
     D xxezip          s              9  0
     D
     D Tab10           s             10
     D
     D ty              s              2    dim(5)
     D bp              s             11  2 dim(5)
     D wk              s              5  0 dim(5)
     D dy              s              3  0 dim(5)
     D xxweek          s              3  0
     D xxdays          s              3  0

      *   *date fields*
     D Anniv_dt        s               d   datfmt(*usa)
     D from_dt         s               d   datfmt(*usa)
     D to_dt           s               d   datfmt(*usa)
     D
     D Datusa          S               D   Datfmt(*USA)
     D
     D CltId           s              3
      *---------------------------------------
      *  end stand alone fields
      *---------------------------------------
     d RtvCltID        pr                  ExtPgm('RTVCLTID')
     d cltID                          3    const

      *---------------------------------------
      *  Key lists
      *---------------------------------------
      *  key lists for employer detail file
     C     elky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd

     C     elky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkemp#

     C     elky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkemp#
     C                   kfld                    k2div             5 0

     C     elky3         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkfyr

     C     Wdell5_Key    Klist
     C                   Kfld                    Cmco#
     C                   Kfld                    Cmfnd
     C                   Kfld                    Cmemp#
     C                   Kfld                    Cmdiv
     C                   Kfld                    Cmfyr

     C     Wdell8_Key    Klist
     C                   Kfld                    Cmco#
     C                   Kfld                    Cmfnd
     C                   Kfld                    Cmemp#
     C                   Kfld                    Cmfyr

     C     Wmepl1_Key    Klist
     C                   Kfld                    Cmco#
     C                   Kfld                    Cmfnd
     C                   Kfld                    Cmfyr
     C                   Kfld                    Cmemp#
     C                   Kfld                    El8div

     C     el5ky         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr

     C     Wmrsl_Key5    Klist
     C                   Kfld                    Cmco#
     C                   Kfld                    El5rtcd

     C     Wmrsl_Key8    Klist
     C                   Kfld                    Cmco#
     C                   Kfld                    El8rtcd

      *  key list for employer master file
     C     emky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv

      *  key list for employer history file
     C     ehky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr

      *  key list for group auxiliary file
     C     f2ky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd

      *  key list for rate set code master file
     C     rsky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkrtcd

      *  key list for system-wide state code table file
     C     stky          klist
     C                   kfld                    kkkey

      *  key list for payroll master file
     C     prky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkrtcd
     C                   kfld                    kkclas
     C                   kfld                    kkefdt

     C     prky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkrtcd
     C                   kfld                    kkclas

      *  key list for claim master file
     C     cmky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    k2div
     C                   kfld                    kkfyr

      *  key list for claim history file
     C     chky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase

      *  Key list for Deductible discount code file
     C     ddky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkst
     C                   kfld                    kkdcde
     C                   KFLD                    XXDDT             7 0

      *  Key list for Deductible detail file
     C     d2ky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
     C                   kfld                    kkcase

      *  Key list for Employer Payroll file
     C     epky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkemp#
     C                   kfld                    k2div

      *  key list for table file
     C     tbky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkkey

      *  key list for generic table file
     C     gtky          klist
     C                   kfld                    kkco#
     C                   kfld                    kktbid
     C                   kfld                    kkkey

      *  Key for Wmpal
     C     Wmpdl_Key     Klist
     C                   Kfld                    Pafein
     C                   Kfld                    Pamod

      *  Key for Wdpal5
     C     Wdpal5_Key    Klist
     C                   Kfld                    Cmco#
     C                   Kfld                    Cmfnd
     C                   Kfld                    Cmfyr
     C                   Kfld                    Cmcase

      *  Key for Wdacl
     C     Wdacl_Key     Klist
     C                   Kfld                    Cmco#
     C                   Kfld                    Cmfnd
     C                   Kfld                    Cmfyr
     C                   Kfld                    Cmcase

      *  Key for Wmcdl
     C     Wmcdl_Key     Klist
     C                   Kfld                    Cmss#

      *  Key for Wtabl
     C     Wtabl_Key     Klist
     C                   kfld                    Kkco#
     C                   Kfld                    Tab10

      *  key list for claim legal master file
     C     llky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase

      *  key list for carrier/state reporting file
     C     rpky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr

      *  Key list for Limits table file
     C     lmky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkst
     C                   kfld                    kkkey
     C                   kfld                    kkefdt

     C     lmky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkst
     C                   kfld                    kkkey

      *  Key list for statistical rate reporting file
     C     rrky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkst
     C                   kfld                    kkefdt
     C
     C     rrky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkst

      *  Key list for madison policy logical file
     C     poky          klist
     C                   kfld                    kkcarr
     C                   kfld                    kkpol#
     C                   kfld                    kkcert
     C                   kfld                    kkstcd
     C                   kfld                    kkpfdt

      *  Key list for madison exposure logical file
     C     eoky          klist
     C                   kfld                    kkcarr
     C                   kfld                    kkpol#
     C                   kfld                    kkcert
     C                   kfld                    kkstcd
     C                   kfld                    kkpfdt
     C                   kfld                    kkcls
     C                   kfld                    kkemod
     C                   kfld                    kkeffd
     C                   kfld                    kkrate
     C                   kfld                    kkeact

      *  Key list for the Employer Entity/Location file
     C     alky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkelid

      *---------------------------------------
      *  end key lists
      *---------------------------------------

     C     *entry        plist
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
      *%%%%%%%%%%%%%%% begin main processing %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      /Free
       RtvCltID(CltId);
      /End-Free
     C                   read      pfn01                                  59

     C                   dow       *in59 = *off
     C
     C                   if        fnflag = 'I'                                 'I'=include

     C                   move      qqco#         kkco#
     C                   move      fnfnd         kkfnd

      * Read wdell in co#/fnd order

     C     elky          chain     pel01                              60

     C                   dow       *in60=*off

     C                   eval       kkemp#=elemp#
     C                   eval       kkdiv=eldiv
     C                   eval       kkfyr=elfyr
      *   Check if state reporting division (emstrp = 'Y')
     C     emky          chain     pem01                              61

     C                   if        *in61=*off
     C                             and emstrp='Y'

      *    save state reporting division for later use
     C                   eval      mainDiv = emdiv

      *    cycle through all the divs

     C     elky1         chain     pel02                              62

     C                   dow       *in62=*off

      *        Get the 'Link data', assign to designated fields
     C                   exsr      link_data

      *        Get the 'Header data'
     C                   exsr      header

      *        Get the 'Exposure data'
     C                   exsr      exposure
     C
     C                   exsr      othexparr

      *        Get the 'Claim data'
     C                   exsr      claims

      *    read the next division
     C     elky1         reade     pel02                                  62
     C                   enddo                                                  *in62=*off
     C
     C                   exsr      short_rt
     C
     C                   exsr      minprem
     C                   exsr      location

     C                   endif                                                  *in60=*off

      * read next record in wdell

     C     elky          reade     pel01                                  60

     C                   enddo

     C                   endif                                                  fnflag='I'
     C
     C                   read      pfn01                                  59

     C                   enddo                                                  *in59=*off

      *  Print transmittal letter totals.
      *
     C                   except    total

     C                   eval      *inlr=*on
      *%%%%%%%%%%%%%%% end main processing %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      *%%%%%%%%%%%%%%  begin subroutines   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *===========================================================================
      *  link_data - fills the fields for link data that are common to
      *              the header, exposure, and claim records.
      *===========================================================================

     C     link_data     begsr


      *    (Carrier number)
     C     f2ky          chain     pf201                              60
     C                   move      f2car#        carrier#

      *    (Policy number)

     C     rpky          chain     prp01                              60

     C                   if        *in60=*off
     C                             and rpcap#<>*blank
     C                   movel(p)  rpcap#        policy#
     C                   else
     C
     C                   move      el2fnd        @@fnd             3
     C                   move      el2emp#       @@emp#            9
     C                   move      el2fyr        @@fyr             3
     C                   eval      policy# = @@fnd + @@emp# + @@fyr
     C                   endif

      *    (Certificate number)
     C                   move      *zeros        cert#

      *    (State code)
     C                   move      el2rtcd       kkrtcd
     C     rsky          chain     prs01                              60
     C                   movel     rsjrst        juris_st
     C                   movel(p)  rsjrst        kkkey

     C     stky          chain     pst01                              60

     C                   eval      state# = stnst
     C
      *      if state rprtg div, save state
     C                   if        el2div=mainDiv
     C                   eval       mainState# = stnst
     C                   endif

      *     (Policy effective date)
     C                   move      elpfdt        poleffdt


     C                   endsr                                                  link_data

      *===========================================================================
      *  header - loads the policy header information
      *===========================================================================
     C     header        begsr


     C                   move      carrier#      kkcarr
     C                   move      policy#       kkpol#
     C                   move      cert#         kkcert
     C                   move      state#        kkstcd
     C                   move      poleffdt      kkpfdt
     C
     C     poky          chain     ppo02                              60
     C                   if        *in60 = *on

      *    (Policy expiration date)
     C                   move      el2ptdt       poptdt

      *    (Risk id#)
     C                   movel(p)  emrsk#        porsk#

      *    (State effective date)
     C                   if        el2pfdt <> elpfdt
     C                   move      el2pfdt       postef
     C                   else
     C                   move      *zeros        postef
     C                   endif

      *    (Federal employer id number)
     C                   move      emfein        pofein

      *    (Policy conditions)
      *         *three year fixed rate policy*
     C
     C     *cymd         move      el2pfdt       from_dt
     C     *cymd         move      el2ptdt       to_dt

     C     to_dt         subdur    from_dt       #years:*years

     C                   if        #years=3
     C                   eval       threeyr='Y'
     C                   else
     C                   eval       threeyr='N'
     C                   endif
     C
      *         *multistate and estimated exposure*
     C                   eval       multist='N'
     C                   eval       estexpo='N'
     C                   eval       hld_jrst=rsjrst
     C

     C     elky3         chain     pel08                              60

     C                   dow       *in60=*off
     C
     C                   eval       kkrtcd = El8rtcd
     C
     C     rsky          chain     prs01                              60

     C                   if        *in60=*off
     C                             and rsjrst<>hld_jrst
     C                   eval       multist='Y'
     C                   endif
     C

     C
     C     elky3         reade     pel08                                  60
     C                   enddo                                                  *in60=*off
     C
      *         *estimated exposure*
     C                   if        El2faf<>'A'
     C                   eval       estexpo='Y'
     C                   endif
     C
      *         *interstate rated / retrospective rated policies*
     C                   eval      inter_st='N'
     C                   eval      retro_rt='N'
     C
      *         *managed care organization policy*
     C                   eval       mco='N'

     C                   if        el2mccd='01'
     C                   eval       mco='Y'
     C                   endif
     C
      *         *cancelled mid-term policy*
     C                   eval       i = 1
     C     *zero         lookup    effdate(i)                             70

     C                   if        effdate(i) <> 0
     C                   eval       i = 5
     C                   else
     C                   sub       1             i
     C                   endif
     C
     C                   if        i <> 0

     C                   dow       (i <> 0) and effdate(i) > 0

     C                   if        effdate(i) > el2pfdt

     C                   sub       1             i
     C                   else
     C                   leave
     C                   endif

     C                   enddo

     C                   endif                                                  if i <> 0
     C
     C                   select
     C                   when      i = 0
     C                   eval       cx_mid = 'N'
     C
     C                   when      cxdate(i) = 0
     C                   eval       cx_mid = 'N'
     C
     C                   when      cxdate(i) < el2ptdt
     C                   eval       cx_mid = 'Y'
     C
     C                   when      cxdate(i) >= el2ptdt

     C     ehky          setgt     peh01                              60
     C                   readp     peh01                                  60

     C                   if        ehptdt > cxdate(i)
     C                   eval       cx_mid = 'Y'
     C                   else
     C                   eval       cx_mid = 'N'
     C                   endif                                                  ehptdt>cxdate(i)
     C
     C                   endsl
     C
     C                   eval       pocond = polcon

      *    (Policy type id code)
     C                   eval       popotp='010101'

     C                   if        elexmf = 'Y'
     C                   eval      popotp ='010102'                             default
     C                   endif

      *    (Deductible type)
     C                   eval       ded_type = '0000'
     C
     C                   if        el2dcde<>*blank
     C                   move      Juris_St      kkst
     C                   move      el2dcde       kkdcde
     C                   z-add     el2dcef       kkdcef

     C                   Eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     C                   Select
     C                   When      xxDedSize = 'L'
     C                   Callp     rtvLgDedInfo (xxClas: xxPlan: +
     C                             xxAmpo: xxAmag)
     C                   When      xxDedSize = 'S'
     C                   Callp     rtvSmDedInfo (xxClas: xxCode: xxPlan: +
     C                             xxAmpo: xxAmag)
     C                   Move      xxCode        Wkdedc
     C                   EndSl

     C                   If        xxAmpo <> 0
     C                   eval       ded_ty12 = rsv_typ(DedRsvCod)

     C                   if        xxplan <> *blank
     C                   eval       ded_ty34 = xxplan
     C                   endif

     C                   eval      podtyp = ded_type

      *    (Deductible amount per claim/accident)
     C                   move      *zeros        podamt
     C                   if        xxplan='01' or xxplan='02'
     C                             or xxplan='08' or xxplan='09'
     C                             or xxplan='10' or xxplan='11'
     C                   move      xxampo        podamt
     C                   endif
     C
      *    (Deductible amount - aggregate)
     C                   move      *zeros        podagg
     C                   if        xxplan='03' or xxplan='09'
     C                             or xxplan='10' or xxplan='11'
     C                   move      xxamag        podagg
     C                   endif
     C
      *    (Deductible percent)
     C                   move      *zeros        ded_pct

     C                   if        xxplan>='04'
     C                             and xxplan<='08'
     C                   z-add     el2ddct       ded_pct
     C                   endif
     C
     C                   move      ded_pct       podpct
     C
     C                   endif                                                  *in60=*off
     C                   endif                                                  ELDCDE<>*blank

      *    (Name of Insured)
     C                   movel(p)  emname        poname

      *    (Address if Insured)
     C                   movel     emmad1        pomad1
     C                   movel     emmad2        pomad2
     C                   movel     emmcty        pomcty
     C                   movel     emmzip        pomzip
     C                   movel(p)  emmst         kkkey
     C     stky          chain     pst01                              60
     C                   movel     stnst         pomst

     C                   move      carrier#      pocarr
     C                   move      policy#       popol#
     C                   move      cert#         pocert
     C                   move      state#        postcd
     C                   move      poleffdt      popfdt

      *  Call procedure to load employee count and zip code.

      *  Calculate the prior policy #.

     C                   eval       svfnd  = kkfnd
     C                   eval       svemp# = kkemp#
     C                   eval       svdiv  = kkdiv
     C                   eval       svfyr  = kkfyr

     C     el5ky         chain     pel05                              99
     C     emky          reade     pel05                                  99
     C  n99              eval       kkfyr = el5fyr

     C                   if        not %found
     C     emky          chain     pex01                              99
     C                   if        %found

     C                   eval       kkfnd = exofnd
     C                   eval       kkemp# = exoemp
     C                   eval       kkdiv = exodiv

     C     emky          setll     pel05
     C     emky          reade     pel05                                  99
     C                   dow       *in99 = *off
     C                             and el5fyr = el2fyr
     C     emky          reade     pel05                                  99
     C                   enddo

     C  n99              eval       kkfyr = el5fyr

     C                   endif
     C                   endif

      *  If previous policy was found, check for a carrier policy#.

     C                   if        *in99 = *off

     C     rpky          chain     prp01                              99

     C                   if        *in99 = *off
     C                             and rpcap#<>*blank
     C                   movel(p)  rpcap#        poppol
     C                   else
     C
     C                   move      kkfnd         @@fnd
     C                   move      kkemp#        @@emp#
     C                   move      kkfyr         @@fyr
     C                   eval      poppol = @@fnd + @@emp# + @@fyr
     C                   endif

     C                   endif

      *  Reset key fields to original values.

     C                   eval       kkfnd  = svfnd
     C                   eval       kkemp# = svemp#
     C                   eval       kkdiv  = svdiv
     C                   eval       kkfyr  = svfyr
     C     el5ky         chain     pel01                              99
     C
     C                   write     ppo01
     C                   eval      xthcnt = xthcnt + 1
     C                   clear                   ppo01

     C                   endif                                                  *in60=*on

     C                   endsr                                                  header

      *===========================================================================
      *  exposure - loads the exposure information for each payroll class
      *             and for each premium factor.
      *===========================================================================

     C     exposure      begsr
     C
     C                   exsr      Rate_array

     C                   eval       kkst=juris_st
     C                   eval       kkefdt=el2pfdt
     C     rrky          setgt     PRR01
     C     rrky2         readpe    PRR01                                  60
     C                   if        *in60=*on
     C                   eval       rrdccd = 0
     C                   endif
     C
     C                   eval      addtl_ld=0


      *  Loop through emp payrolls for co#/fnd/fyr/emp#
     C                   eval      k2div = el2div
     C                   eval      kkefdt=rate_dt(1)

     C     epky          chain     pep01                              65

     C                   dow       *in65=*off
     C                   if        epstat = 'A'

     C                   eval       kkclas=epclas
     C                   eval       kkefdt=epefdt
     C     prky          setgt     ppr01
     C     prky1         readpe    ppr01                                  75
     C                   if        not *in75
     C                   eval      xxoccd = proccd
      *** Beg Add * 10/20/15
     C                   eval      xxnrat = prnrat
      *** End Add * 10/20/15
     C                   else
     C                   eval      xxoccd = *blank
      *** Beg Add * 10/20/15
     C                   eval      xxnrat = *Blanks
      *** End Add * 10/20/15
     C                   endif

     C                   if        epclas=rrdccd
     C                             or xxoccd = 'Y'
      *** Beg Add * 10/20/15
     C                             or xxnrat = 'Y'
      *** End Add * 10/20/15
     C                   eval       addtl_ld = addtl_ld + epadld
     C
     C                   else

      *  Set values for call to procedure to split payrolls
     C                   eval       savclas = epclas
     C                   eval       idx=1
     C     epefdt        lookup    rate_dt(idx)                         7070
     C
     C                   if        estexpo='Y'
     C                   eval       xxeamt=epeapr
     C                   else
     C                   eval       xxeamt=epaapr
     C                   endif
     C
     C                   eval       xxprem=(xxeamt * eparat)/100
     C
     C                   eval       beg_dt=epefdt
     C                   eval       end_dt=el2ptdt
     C
     C                   if        epcadt<>0
     C                   eval       end_dt=epcadt
     C                   endif
     C
     C                   eval       kkclas=epclas
     C                   eval       kkefdt=epefdt
     C     prky          setgt     ppr01
     C     prky1         readpe    ppr01                                  75
     C                   eval       percap=*off
     C                   if        *in75=*off
     C                             and prpcf='C'
     C                   eval       percap=*on
     C                   endif

     C                   callp     split(epco# : epfnd : epfyr : epemp# : epdiv
     C                               : epclas : epspec : idx : xxeamt : xxprem :
     C                               eparat : epefdt : beg_dt : end_dt : percap)

     C                   endif                                                  epclas=rrdccd

     C                   endif                                                  epstat='A'

      *     *read next payroll record*
     C     epky          reade     pep01                                  65
     C
      *     *write exposure if new clas, spec, or end of file*
     C                   if        epclas<>savclas
     C                             and epstat = 'A'
     C                             or *in65=*on
     C                   exsr      expwrt
     C                   endif
     C
     C                   enddo

     C                   endsr                                                  exposure

      *===========================================================================
      *  expwrt - write exposure records
      *===========================================================================
     C     expwrt        begsr
      *
     C                   do        5             i
     C                   if        clscd(i)<>*zero

      *    Test to see if record already exists, if so update, else
      *    write a new record.
     C                   eval       kkcarr=carrier#
     C                   eval       kkpol#=policy#
     C                   eval       kkcert=cert#
     C                   eval       kkstcd=state#
     C                   move      clscd(i)      kkcls
     C                   z-add     expmd(i)      exp_md
     C                   move(p)   exp_md        kkemod
     C                   move(p)   pefdt(i)      kkeffd
     C                   z-add     manrt(i)      man_rt
     C                   move(p)   man_rt        kkrate
     C                   eval       kkeact=expact(i)
     C
     C     eoky          chain     peo02                              60
     C                   if        *in60=*off
     C
      *     *exposure
     C                   move(p)   eo2eamt       exp_amt
     C                   add       expamt(i)     exp_amt
     C                   move(p)   exp_amt       eo2eamt
     C
      *     *premium
      *                    *accumulate premium totals*
     C                   if        exp_md > 0
     C                   eval       mod_prem = mod_prem + (pramt(i) * exp_md)
     C                   else
     C                   eval       mod_prem = mod_prem + pramt(i)
     C                   endif
     C                   eval       prem_ttl = prem_ttl + pramt(i)
     C
     C                   move(p)   eo2prem       pr_amt
     C                   add       pramt(i)      pr_amt
     C                   move(p)   pr_amt        eo2prem
     C
     C                   update    peo02
     C                   eval      xteamt = xteamt + expamt(i)

     C                   else                                                   *in60=*off

     C                   eval       eocarr=carrier#
     C                   eval       eopol#=policy#
     C                   eval       eocert=cert#
     C                   eval       eostcd=state#
     C                   eval       eopfdt=poleffdt

     C                   move(p)   clscd(i)      eoclas
     C
     C                   z-add     expmd(i)      exp_md
     C                   move(p)   exp_md        eoemod
     C                   move(p)   pefdt(i)      eoefdt
     C
     C                   z-add     expamt(i)     exp_amt
     C                   move(p)   exp_amt       eoeamt
     C                   z-add     pramt(i)      pr_amt
     C                   move(p)   pr_amt        eoprem
     C*                    *accumulate modified premium*
     C                   if        exp_md > 0
     C                   eval       mod_prem = mod_prem + (pr_amt * exp_md)
     C                   else
     C                   eval       mod_prem = mod_prem + pr_amt
     C                   endif
     C                   eval       prem_ttl = prem_ttl + pr_amt

     C                   z-add     manrt(i)      man_rt
     C                   move(p)   man_rt        eorate
     C                   move      splitp(i)     eosplt
     C                   eval      eotier=rttier(i)
     C                   eval      eoeact=expact(i)

     C                   write     peo01
     C                   eval      xtecnt = xtecnt + 1
     C                   eval      xteamt = xteamt + exp_amt
     C                   clear                   peo01

     C                   endif                                                  *in60=*off

     C                   endif                                                  clscd(i)<>0
     C                   enddo                                                  do 5

      *    *clear out arrays*
     C                   clear                   clscd
     C                   clear                   pefdt
     C                   clear                   expamt
     C                   clear                   pramt
     C                   clear                   manrt
     C                   clear                   splitp
     C                   clear                   rttier
     C                   clear                   expact
     C
     C

     C                   endsr                                                  expwrt

      *============================================================================
      *  othexparr - Subroutine to assign values for non-payroll exposure
      *============================================================================
     C     othexparr     begsr
      *
     C                   clear                   othexp
     C

      *  Assign values to buckets based on audit status
     C                   if        estexpo = 'N'

      *                AUDITED
      *      Deductible discount exposure amt
     C                   add       el2adct       othexp(1)
      *      Managed care credit exposure amt
     C                   add       el2adcm       othexp(2)
      *      Safety credit exposure amt
     C                   add       el2adcs       othexp(3)
      *      Contractor credit exposure amt
     C                   add       el2adc1       othexp(4)
      *      Deviation exposure amt
     C                   add       el2adc2       othexp(5)
      *      2nd Deviation exposure amt
     C                   add       el2adv2       othexp(6)
      *      Schedule debit/credit exposure amt
     C                   add       el2adc        othexp(7)
      *      Increased limits premium 1
     C                   add       el2ain1       othexp(8)
      *      Increased limits premium 2
     C                   add       el2ain2       othexp(9)
      *      Stock discount (aka premium discount)
     C                   add       el2adc3       othexp(10)
      *      Expense constant
     C                   add       el2prtd       othexp(11)
      *      Waiver surcharge
     C                   add       el2awav       othExp(13)
      *      1st Assessment premium
     C                   add       el21aam       othExp(14)
      *      2nd Assessment premium
     C                   add       el22aam       othExp(15)
      *      3rd Assessment premium
     C                   add       el23aam       othExp(16)
      *      Terrorism surcharge
     C                   add       el2ater       othExp(17)
      *      Domestic terrorism surcharge
     C                   add       el2adtr       othExp(18)
      *      Voluntary Labor Charge
     C                   add       el2vlca       othExp(19)

      *      Load values from Premium Detail record, if record exists.

     C     elky2         chain     pmp01                              63
     C                   if        *in63 = *off
     C                             and elamnp = 'Y'

     C                   z-add     mpadct        othExp(1)
     C                   z-add     mpadcm        othExp(2)
     C                   z-add     mpadcs        othExp(3)
     C                   z-add     mpadc1        othExp(4)
     C                   z-add     mpadc2        othExp(5)
     C                   z-add     mpadv2        othExp(6)
     C                   z-add     mpadc         othExp(7)
     C                   z-add     mpadc3        othExp(10)
     C                   z-add     mpprtd        othExp(11)
     C                   z-add     mpawav        othExp(13)

     C                   endif                                                  adtpyrl=*on

     C                   else

      *                NOT AUDITED
      *      Deductible discount exposure amt
     C                   add       el2edct       othexp(1)
      *      Managed care credit exposure amt
     C                   add       el2edcm       othexp(2)
      *      Safety credit exposure amt
     C                   add       el2edcs       othexp(3)
      *      Contractor credit exposure amt
     C                   add       el2edc1       othexp(4)
      *      Deviation exposure amt
     C                   add       el2edc2       othexp(5)
      *      2nd Deviation exposure amt
     C                   add       el2edv2       othexp(6)
      *      Schedule debit/credit exposure amt
     C                   add       el2edc        othexp(7)
      *      Increased limits premium 1
     C                   add       el2ein1       othexp(8)
      *      Increased limits premium 2
     C                   add       el2ein2       othexp(9)
      *      Stock discount (aka premium discount)
     C                   add       el2edc3       othexp(10)
      *      Expense constant
     C                   add       el2prtd       othexp(11)
      *      Waiver surcharge
     C                   add       el2ewav       othExp(13)
      *      1st Assessment premium
     C                   add       el21aam       othExp(14)
      *      2nd Assessment premium
     C                   add       el22aam       othExp(15)
      *      3rd Assessment premium
     C                   add       el23aam       othExp(16)
      *      Terrorism surcharge
     C                   add       el2eter       othExp(17)
      *      Domestic terrorism surcharge
     C                   add       el2edtr       othExp(18)
      *      Voluntary Labor Charge
     C                   add       el2vlca       othExp(19)
      *
      *      Load values from Premium Detail record, if record exists.

     C     elky2         chain     pmp01                              63
     C                   if        *in63 = *off
     C                             and elemnp = 'Y'

     C                   z-add     mpedct        othExp(1)
     C                   z-add     mpedcm        othExp(2)
     C                   z-add     mpedcs        othExp(3)
     C                   z-add     mpedc1        othExp(4)
     C                   z-add     mpedc2        othExp(5)
     C                   z-add     mpedv2        othExp(6)
     C                   z-add     mpedc         othExp(7)
     C                   z-add     mpedc3        othExp(10)
     C                   z-add     mpprtd        othExp(11)
     C                   z-add     mpewav        othExp(13)

     C                   endif                                                  adtpyrl=*on

     C                   endif                                                  estexpo='N'

     C                   add       addtl_ld      othexp(12)

     C                   exsr      wrtothexp

     C                   endsr                                                  othexparr

      *===========================================================================
      *  wrtothexp - writes an record for non_payroll exposure
      *===========================================================================
     C     wrtothexp     begsr

      *  Cycle through buckets and write records for non-zero values
     C                   do        #oexp         i
     C                   if        othexp(i)<>0

     C                   clear                   peo01

     C                   eval       eocarr=carrier#
     C                   eval       eopol#=policy#
     C                   eval       eocert=cert#
     C                   eval       eostcd=state#
     C                   eval       eopfdt=poleffdt

      *    (Class code)
     C                   select

     C                   when      i=1                                          deductible dscnt
     C                   move      Juris_St      kkst
     C                   move      eldcde        kkdcde
     C                   z-add     eldcef        kkdcef

     C

     C                   Eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     C                   Select
     C                   When      xxDedSize = 'L'
     C                   Callp     rtvLgDedInfo (xxClas: xxPlan: +
     C                             xxAmpo: xxAmag)
     C                   When      xxDedSize = 'S'
     C                   Callp     rtvSmDedInfo (xxClas: xxCode: xxPlan: +
     C                             xxAmpo: xxAmag)
     C                   Move      xxCode        Wkdedc
     C                   EndSl

     C                   If        xxAmpo <> 0
     C                   move      xxclas        eoclas
     C                   endif
     C                   eval       mod_prem = mod_prem - othexp(i)

     C                   when      i>=2 and i<=7                                managed care credit
     C                             or i=11
     C                             or i=12
     C                   eval       kkst=juris_st
     C                   eval       kkefdt=elpfdt
     C     rrky          setgt     PRR01
     C     rrky2         readpe    PRR01                                  60
     C                   if        *in60=*off
     C                   select
     C
     C                   when      i=2
     C                   move      rr1acd        eoclas
     C                   eval       mod_prem = mod_prem - othexp(i)
     C
     C                   when      i=3
     C                   move      rr1bcd        eoclas
     C                   eval       mod_prem = mod_prem - othexp(i)
     C
     C                   when      i=4
     C                   move      rrcccd        eoclas
     C                   eval       mod_prem = mod_prem - othexp(i)
     C
     C                   when      i=5
     C                   move      rrdv1c        eoclas
     C                   eval       mod_prem = mod_prem - othexp(i)
     C                   if        OthExp(i) < 0
     C                   eval       OthExp(i) = -(OthExp(i))
     C                   endif
     C
     C                   when      i=6
     C                   move      rrdv2c        eoclas
     C                   eval       mod_prem = mod_prem - othexp(i)
     C                   if        OthExp(i) < 0
     C                   eval       OthExp(i) = -(OthExp(i))
     C                   endif
     C
     C                   when      i=7
     C                   eval       mod_prem = mod_prem + othexp(i)
     C                   if        othexp(i) > 0
     C                   move      rrdbcd        eoclas
     C                   else
     C                   move      rrcrcd        eoclas
     C                   eval       othexp(i) = -(othexp(i))
     C                   endif
     C
     C                   when      i=11
     C                   move      rreccd        eoclas
     C
     C                   when      i=12
     C                   move      rrdccd        eoclas
     C                   eval       mod_prem = mod_prem + othexp(i)
     C
     C                   endsl
     C                   else
     C                   move      *zeros        eoclas
     C                   endif                                                  *in60=*off
     C
     C                   when      i=8 or i=9                                   increased limits 1
     C                   move      *zeros        eoclas
     C                   eval       mod_prem = mod_prem + othexp(i)
      *      Assign appropriate increased limits code
     C
     C                   if         i = 8
     C                   movel(p)  el2ilcd       kkkey
     C                   else
     C                   movel(p)  el2inl2       kkkey
     C                   endif                                                  i=8
     C                   movel     Juris_St      kkst
     C                   eval       kkefdt=el2pfdt
      *      Get record for the appropriate limits code
     C     lmky          setgt     plm01
     C     lmky2         readpe    plm01                                  60
     C                   dow       *in60 = *off
     C                             and lmstat='I'
     C     lmky2         readpe    plm01                                  60
     C                   enddo
     C  N60              move      lmclas        eoclas
     C
     C                   when      i=10
     C                   eval       eoclas='0063'
     C                   eval       mod_prem = mod_prem - othexp(i)

      *      Waiver surcharge

     C                   when      i = 13                                       Waiver surcharge
     C                   move      rrwvac        eoclas
     C                   eval       mod_prem = mod_prem + othexp(i)

      *      Assesment code 1

     C                   when      i = 14
     C                   move      rr1stc        eoclas

      *      Assesment code 2

     C                   when      i = 15
     C                   move      rr2ndc        eoclas

      *      Assesment code 3

     C                   when      i = 16
     C                   move      rr3rdc        eoclas

      *      Terrorism surcharge code

     C                   when      i = 17
     C                   move      rrterc        eoclas

      *      Domestic terrorism charge code

     C                   when      i = 18
     C                   move      rrdtrc        eoclas

      *      Voluntary Labor Charge

     C                   when      i = 19
     C                   move      rrvlcc        eoclas
     C
     C                   endsl

      *    (Experience mod)
     C                   z-add     expmod(1)     exp_md
     C                   move      exp_md        eoemod

      *    (Period effective date)
     C                   move(p)   pefdt(1)      eoefdt

      *    (Exposure amount)
     C                   move      *zeros        eoeamt

     C                   move      eoclas        kkclas
     C                   eval      kkefdt=el2pfdt
     C     prky          setgt     ppr01
     C     prky1         readpe    ppr01                                  75
     C                   if        *in75=*off
     C
      *    (Rating tier)
     C                   eval      eotier=prtier
     C
      *    (Exposure act)
     C                   eval      eoeact=preact
     C
     C                   else
     C                   eval      eoeact='  '
     C
     C                   endif                                                  othexp(i)<>0

      *    (Manual rate)
     C                   move      *zeros        eorate

      *    (Split period)
     C                   move      *zero         eosplt

      *    Test to see if record already exists, if so update, else
      *    write a new record.
     C                   eval       kkcarr=carrier#
     C                   eval       kkpol#=policy#
     C                   eval       kkcert=cert#
     C                   eval       kkstcd=state#
     C                   eval       kkcls = eoclas
     C                   eval       kkemod = eoemod
     C                   eval       kkeffd = eoefdt
     C                   eval       kkrate = eorate
     C                   eval       kkeact = eoeact
     C
     C     eoky          chain     peo02                              60
     C                   if        *in60=*off
     C
      *    If record for expense constant- don't combine values, use highest
     C                   if        i = 11
     C                   move(p)   eo2prem       pr_amt
     C                   if        othexp(i) > pr_amt
     C                   eval       pr_amt = othexp(i)
     C                   move(p)   pr_amt        eo2prem
     C                   update    peo02
     C                   endif
     C
     C                   else
     C
     C                   move(p)   eo2prem       pr_amt
     C                   add       othexp(i)     pr_amt
     C                   move(p)   pr_amt        eo2prem
     C                   update    peo02
     C                   endif                                                  i=11
     C
     C                   else
     C
      *    (Premium amount)
     C                   z-add     othexp(i)     pr_amt
     C                   move(p)   pr_amt        eoprem
     C                   write     peo01
     C                   eval      xtecnt = xtecnt + 1
     C                   endif                                                  *in60=*off
     C
     C                   clear                   peo01

     C                   endif                                                  othexp(i)<>0
     C                   enddo                                                  #oexp, i

     C                   endsr                                                  wrtothexp

      *===========================================================================
      *  short_rt - detrmine if this is a "short rated" account.  If it is then
      *             write an exposure record reflecting the difference.
      *===========================================================================
     C     short_rt      begsr

      *  Check if account is audited and employer is short rated
      *  and that minimum premium has not been invoked
     C                   if        estexpo = 'N'
     C                             and emsrpr = 'S'
     C                             and (elamnp <> 'Y')

      *    Check if policy is for a partial year (less than 365 days)
     C     *cymd         move      elpfdt        from_dt
     C     *cymd         move      elptdt        to_dt

     C     to_dt         subdur    from_dt       nDays:*days

     C                   if        nDays < 365

      *      If audited manual premium from employer detail (WDELP) is not
      *      equal to accumulated premium then write exposure record.
     C                   if        prem_ttl < elampr
     C                   callp     wrt_xprem('0931' : elampr - prem_ttl)
     C                   endif                                                  prem_ttl<elampr

     C                   endif                                                  nDays<365

     C                   endif                                                  estexpo='N'

     C                   endsr                                                  short_rt

      *===========================================================================
      *  minprem - determine if record should be written for minimum premium
      *===========================================================================
     C     minprem       begsr

     C                   if        estexpo = 'N'
     C                             and elamnp = 'Y'
     C                   eval      min_prem = elampr - mod_prem
     C                   else
     C                   if        estexpo = 'Y'
     C                             and elemnp = 'Y'
     C                   eval      min_prem = elmapr - mod_prem
     C                   endif
     C                   endif

      *  If no payroll exposure has been produced, add record '1111'.

     C                   if        prem_ttl = 0
     C                             and elpfdt = elptdt
     C                   callp     wrt_xprem('1111' : 0)
     C                   endif

      *****Increased limits must be taken into account when calculating minimum
      *****premium using the manual premium amount because increased limits are
      *****included in the manual premium field in emp detail.

      *    If min premium exists then write a record
     C                   if        min_prem > 0
     C                   callp     wrt_xprem('0990' : min_prem)
     C                   endif                                                  xtra_prem>0
     C
      * Add premium to report totals.

     C                   eval      xtprem = xtprem + mod_prem
     C                   if        min_prem > 0
     C                   eval      xtprem = xtprem + min_prem
     C                   endif

      *    Reset premium totals
     C                   eval      mod_prem = 0
     C                   eval      min_prem = 0
     C                   eval      prem_ttl = 0

     C                   endsr                                                  minprem

      *===========================================================================
      *  location - load the location records
      *===========================================================================
     C     location      begsr

      *  Determine the employee count by division.

     C                   eval      xxloc# = 1
     C     elky3         chain     pel08                              99
     C                   dow       *in99 = *off

     C                   eval       kkemp# = el8emp#
     C                   eval       kkdiv = el8div

     C                   clear                   xxempy
     C                   if        el8empy > 0
     C                   eval      xxempy = el8empy
     C                   else

     C                   clear                   xxempy
     C                   eval       k2div = el8div
     C     epky          chain     pep01                              99
     C                   dow       *in99 = *off
     C                   if        epstat <> 'I'

     C                   if        el8faf = 'A'
     C                   eval      xxempy = xxempy + epacnt
     C                   else
     C                   eval      xxempy = xxempy + epecnt
     C                   endif

     C                   endif
     C     epky          reade     pep01                                  99
     C                   enddo

     C                   endif

      *  Load the location record fields.

     C                   if        xxempy > 0
     C     emky          chain     pem01                              99

     C                   movel     carrier#      locarr
     C                   movel     policy#       lopol#
     C                   move      poleffdt      lopfdt
     C                   move      xxloc#        loloc#
     C                   move      xxempy        loecnt
     C                   if        empad1 <> *blanks
     C                   movel     empad1        loadr1
     C                   movel     empad2        loadr2
     C                   movel     empcty        locity
     C                   movel     empst         xxst
     C                   movel     empzip        lozip1
     C                   move      empzip        lozip2
     C                   else
     C                   movel     emmad1        loadr1
     C                   movel     emmad2        loadr2
     C                   movel     emmcty        locity
     C                   movel     emmst         xxst
     C                   movel     emmzip        lozip1
     C                   move      emmzip        lozip2
     C                   endif

     C                   movel(p)  xxst          kkkey
     C     stky          chain     pst01                              60
     C                   move      stnst         lostcd

     C                   write     plo01
     C                   clear                   plo01
     C                   eval      xxloc# = xxloc# + 1
     C                   endif

      *  If there are locations, load those into the location file.

     C     emky          chain     pld01                              99
     C                   dow       *in99 = *off

     C                   if        ldfyr = kkfyr
     C                             and ldeltp = 'L'
     C                             and ldempy > 0
     C                   eval      xxempy = ldempy
     C                   eval      kkelid = ldelid

     C     alky          chain     pal01                              99
     C                   if        *in99 = *off

     C                   movel     carrier#      locarr
     C                   movel     policy#       lopol#
     C                   move      poleffdt      lopfdt
     C                   move      xxloc#        loloc#
     C                   move      xxempy        loecnt
     C                   if        alpad1 <> *blanks
     C                   movel     alpad1        loadr1
     C                   movel     alpad2        loadr2
     C                   movel     alpcty        locity
     C                   movel     alpst         xxst
     C                   movel     alpzip        lozip1
     C                   move      alpzip        lozip2
     C                   else
     C                   movel     almad1        loadr1
     C                   movel     almad2        loadr2
     C                   movel     almcty        locity
     C                   movel     almst         xxst
     C                   movel     almzip        lozip1
     C                   move      almzip        lozip2
     C                   endif

     C                   movel(p)  xxst          kkkey
     C     stky          chain     pst01                              60
     C                   move      stnst         lostcd

     C                   write     plo01
     C                   clear                   plo01
     C                   eval      xxloc# = xxloc# + 1

     C                   endif
     C                   endif

     C     emky          reade     pld01                                  99
     C                   enddo

      *  Get the next division.

     C     elky3         reade     pel08                                  99
     C                   enddo

     C                   endsr                                                  minprem

      *===========================================================================
      *  claims - read WMCMP in co#/grp/emp#/yr order and load claim info
      *===========================================================================
     C     claims        begsr
      *
     C                   eval      k2div = el2div
     C
     C     cmky          chain     pcm01                              60
     C                   dow       *in60=*off
     C                   eval       kkcase = cmcase

      *  Exclude 'record only' claims from processing
     C                   if        cmltfl<>'R'

     C                   exsr      Rsrv_Amt
      *    (Incurred indemnity)
     C                   eval      indem = IndemRem + IndemPay
     C                   move      indem         wkiinc
     C
      *    (Incurred medical)
     C                   eval      medic = MedicRem + MedicPay
     C                   move      medic         wkminc
     C
      *    (Incurred Alae)
     C                   eval      alaein = AlaeRem + AlaePay
     C                   move      alaein        wkexpi
     C
      *  Do not write Loss record if no incurred indemnity or medical
     C                   if        (indem <> 0) OR (medic <> 0)
     C                               OR alaein <> 0
     C
      *    (Paid indemnity)
     C                   eval      paidin = IndemPay
     C                   move      paidin        wkinpd
     C
      *    (Paid medical)
     C                   eval      paidmd = MedicPay
     C                   move      paidmd        wkmdpd
     C
      *    (Alae paid)
     C                   eval      alaepd = AlaePay
     C                   move      alaepd        wkexpp
     C
      *    (Alae incurred)
     C                   eval      alaein = AlaeRem + AlaePay
     C                   move      alaein        wkexpi
     C
      *    (Class code)
     C                   move(p)   cmclas        wkclas

      *    (Accident date)
     C                   move      cmacdt        wkacdt

      *    (Claim number)
     C                   move(p)   cmcase        case#
     C                   eval      wkclm# = %trim(case#)

      *    (Claim status)
     C                   if        cmstat='O'
     C                   eval       wkstat='0'                                   Open claim
     C                   eval       kkcase = cmcase

      *      *Look at history records for previusly closed*
     C     chky          setgt     pch01
     C     chky          readpe    pch01                                  60
     C                   dow       (*in60 = *off) and (chocst <> 'C')
     C     chky          readpe    pch01                                  60
     C                   enddo

     C                   if        Chocst = 'C'
     C                   eval       wkstat='2'                                   reopened claim
     C                   endif
     C                   xfoot     PayRsv        arr_sum                         add arr elmnts
     C                   if        cmjrst='MI' and arr_sum=0
     C                   eval       wkstat='4'                                   pmt not made
     C                   endif
     C                   else
     C                   eval       wkstat='1'                                   closed claim
     C                   endif

      *    (Injury type)
     C                   move(p)   cmncci        wkinj

      *    (Catastrophe number)
     C                   move(p)   cmcata        wkcat#

      *    (Social security #)
     C                   move      cmss#         wkss#

      *    (Act) & (Type of Loss)
     C                   eval       act=*blank
     C                   eval       typ_los=*blank
     C
      *      *chain to loss coverade table - act and typ_los fields will*
      *      *automatically be filled when record is found*
     C                   eval       kktbid = cmjrst + 'LC'
     C                   movel(p)  cmlscv        kkkey
     C     gtky          chain     pgt01                              60
     C                   if        *in60 = *on
     C                   eval       kktbid = '  LC'
     C     gtky          chain     pgt01                              60
     C                   endif
     C
     C                   move(p)   act           wkact
     C                   move(p)   typ_los       wkloss

      *    (Type of recovery)
     C                   if        cmlsrv=*blank
     C                   move      '01'          wkrcvr                          default
     C                   else
     C                   move      cmlsrv        wkrcvr
     C                   endif                                                  cmlsrv=*blank

      *    (Type of coverage)
     C                   if        cmcovt=*blank
     C                   move      '01'          wkcvrg                          default
     C                   else
     C                   move      cmcovt        wkcvrg
     C                   endif                                                  cmlsrv=*blank

      *    (Attny Code)
     C                   Eval      Wkatcd = '2'
     C                   Eval      Wkatty = '2'

      *      *chain to the claim legal master file*
     C     llky          chain     pll01                              60
     C                   if        *in60=*off

      *    (Lump sum indicator)
     C                   if        lllump = 'Y'
     C                   eval       wklump = 'Y'
     C                   else
     C                   eval       wklump = 'N'
     C                   endif                                                  LLLUMP='Y'
      *    (Attny Code)
     C                   If        Llplaa <> *Zero
     C                   Eval      Wkatcd = '1'
     C                   Eval      Wkatty = '1'
     C                   Endif                                                  LLLUMP='Y'
     C
      *    (Type of settlement)
     C                   move      llmtst        wksett
     C
      *    (Fraudulant clam indicator)
     C                   move      llfrau        wkded

     C                   else
     C                   eval       wklump = 'N'
     C                   move      *zeros        wksett
     C                   move      *zeros        wkded

     C                   endif                                                  *in60=*off

      *    (Jurisdiction state)
     C                   eval       wkjrst=*blank
     C                   movel(p)  cmjrst        kkkey
     C     stky          chain     pst01                              60
     C  n60              move      stnst         wkjrst

      *    (Managed care org type)
     C                   move      cmmctp        wkmco

      *    (Part of body)
     C                   eval       wkpart=*blank
     C                   movel(p)  cmpart        kkkey
     C     tbky          chain     ppa01                              60
     C*** Beg Chg *** 05/01/18 ************************************
     C**n60************* move      paedip        wkpart
     C**N60************* Move      Paedip        Wksbm1
     C  n60              movel(p)  paedip        wkpart
     C  N60              Movel(p)  Paedip        Wksbm1
     C*** End Chg *** 05/01/18 ************************************

      *    (Nature of injury)
     C                   eval       wknatr=*blank
     C                   movel(p)  cmnatr        kkkey
     C     tbky          chain     pna01                              60
     C  n60              move      naedin        wknatr

      *    (Cause of injury)
     C                   eval       wkcaus=*blank
     C                   movel(p)  cmcaus        kkkey
     C     tbky          chain     pca01                              60
     C  n60              move      caedic        wkcaus

      *    (Occupational description)
     C                   movel(p)  cmocds        wkoccd

      *    (Vocational rehab indicator)
     C                   eval       wkvoc='N'
     C                   if        cmvorc='Y'
     C                   eval       wkvoc='Y'
     C                   endif

      *    (Claimant's attorney fees)
     C                   move      *zeros        wkclat

      *    (Employer's attorney fees)
     C                   move      *zeros        wkemat

      *    (Deductible reimbursement)
     C     ddky          setgt     pdd01
     C                   readp     pdd01                                  60
     C                   if         *in60=*off
     C                              and ddstat = 'A'
     C
      *      *report zero if calculated on gross losses
     C                   if        ddtype = 'G'
     C                   move      *zeros        wkdedr
     C                   move      '03'          wkdedc

     C                   else
     C                   eval       ded_sum=0
     C                   eval       ded_bill=0
     C     d2ky          chain     pd201                              60
     C                   dow       *in60 = *off

     C                   add       dd2amtr       ded_sum
     C                   add       dd2amtb       ded_bill

     C     d2ky          reade     pd201                                  60
     C                   enddo
     C
     C                   move(p)   ded_sum       wkdedr

     C                   if        ded_sum = ded_bill
     C                   move      '01'          wkdedc
     C                   else
     C                   move      '02'          wkdedc
     C                   endif

     C                   endif                                                  ddtype = 'G'
     C                   endif                                                  *in60=*off

     C                   if        wkdedc = *blank
     C                   move      '00'          wkdedc
     C                   endif

     C                   eval      wkcarr=carrier#
     C                   eval      wkpol#=policy#
     C                   eval      wkcert=cert#
     C                   eval      wkstcd=state#
     C                   eval      wkpfdt=poleffdt
     C
     C                   Exsr      More_Flds
     C
      *      *write a claim record*
     C                   write     pwk01
     C                   eval      xtlcnt = xtlcnt + 1
     C                   eval      xtiinc = xtiinc + indem
     C                   eval      xtminc = xtminc + medic
     C                   eval      xtinpd = xtinpd + paidin
     C                   eval      xtmdpd = xtmdpd + paidmd
     C                   eval      xtexpp = xtexpp + alaepd
     C                   eval      xtexpi = xtexpi + alaein
     C                   clear                   pwk01

     C                   endif                                                  indem<>0 or

     C                   endif                                                  cmltfl<>'R'

     C     cmky          reade     pcm01                                  60
     C                   enddo                                                  *in60=*off


     C                   endsr                                                  claims

      *===========================================================================
      *  Move fields new to Wxwkp2 not in original Wxwkp
      *===========================================================================

     C     More_Flds     Begsr

     C                   Move      *zeros        WKWKIN1
     C                   Move      *zeros        WKSILS1
     C                   Move      *zeros        WKWKTB1
     C                   Move      *zeros        WKSDIS2
     C                   Move      *Blanks       WKSBM2
     C                   Move      *zeros        WK#WKS2
     C                   Move      *zeros        WKINCL2
     C                   Move      *zeros        WKNSDIS
     C                   Move      *zeros        WKINCLS
     C                   Move      *zeros        WKLIAB
     C                   Move      *zeros        WKPIB
     C                   Move      *zeros        WKPVFP
     C                   Move      *zeros        WKFNRL
     C                   Move      *zeros        WKRMRG
     C                   Move      *zeros        WKSSAMT
     C                   Move      *zeros        WKPPR
     C                   Move      *zeros        WKMETD
     C                   Move      *zeros        WKDMETD
     C                   Move      *zeros        WKIMETD
     C                   Move      *zeros        WKANNU
     C                   Move      *Blanks       WKSUMDT

     C                   Exsr      Get_Wdpap

      * California only
     C                   Z-add(H)  Cmaww         Num5
     C                   Move      Num5          Wkcaww
     C                   Z-add(H)  Cmipct        Num3
     C                   Move      Num3          Wkipct
     C                   Z-add(H)  Wkvrhbi#      Num9
     C                   Move      num9          Wkvrhbi
     C                   Z-add(H)  Wktgi#        Num9
     C                   Move      num9          Wktgi

      * ICR for DE, MA, NJ, NY, and PA only.

      *  Next line will be valid on next base release.

     C                   Move      Cmyrlx        Wkyrex

     C                   Move      Cmdod         WKDOD

     C                   Move      Cmrcvd        WKDTRP

     C     Wmcdl_Key     Chain     Wmcdl
     C                   If        %Found
     C                   Move      Cddtob        WKDOB
     C                   Endif

     C     Chky          Setgt     Pch01
     C     Chky          Readpe    Pch01                                  99

     C                   If        *in99 = *off and
     C                             Chocst = 'C'
     C                   Movel     Chefdt        H5
     C                   Move      H5            WKDTCL
     C                   Move      Chefdt        Wkclsdt                         reopened claim
     C                   Else
     C                   Move      '0000'        WKDTCL
     C                   Eval      Wkclsdt = '000000'                            reopened claim
     C                   Endif

     C                   Z-add(H)  Cmaww         Num5
     C                   Move      Num5          WKAWW

     C                   Z-add(H)  Wkinct#       Num9
     C                   Move      Num9          Wkinct

     C                   Z-add(H)  Cmipct        Num3
     C                   Move      Num3          Wksdis

     C                   Z-add(H)  Wkvrhbi#      Num9
     C                   Move      num9          Wkvrhb
     C                   z-add(h)  Wkssam#       Num9
     C                   Move      num9          Wkssamt
     C                   Movel     Cmlsn         Wklnam
     C                   Movel     Cmlsn         Wklnm2
     C                   Movel     Cmfsn         Wkfnam
     C                   z-add(h)  Wkhosp#       Num9
     C                   move      Num9          WKHOSP
     C                   z-add(h)  Wklegd#       Num9
     C                   Move      num9          WKLEGD
     C                   z-add(h)  Wklegc#       Num9
     C                   Move      num9          WKLEGC
     C                   z-add(h)  Wktgi#        Num9
     C                   Move      num9          Wkgrss
     C                   z-add(h)  Wkvind#       Num9
     C                   Move      num9          WKVIND
     C                   z-add(h)  Wkvtrn#       Num9
     C                   Move      num9          WKVTRN
     C                   z-add(h)  Wkvevl#       Num9
     C                   Move      num9          WKVEVL
     C                   Move      Cmhidt        Wkdoh

     C                   Eval      Wkemst = Cmests

     C                   z-add(h)  Wkdisp#       Num9
     C                   Move      num9          WKDISP
     C                   z-add(h)  Wkptpd#       Num9
     C                   Move      num9          WKPTPD
     C                   z-add(h)  Wkrhbp#       Num9
     C                   Move      num9          WKRHBP
     C                   z-add(h)  Wkprmp#       Num9
     C                   Move      num9          WKPRMP
     C                   z-add(h)  Wkdthp#       Num9
     C                   Move      num9          WKDTHP
     C                   z-add(h)  Wkstla#       Num9
     C                   Move      num9          WKSTLA
     C                   z-add(h)  Wkphpd#       Num9
     C                   Move      num9          WKPHPD

     C                   If        Cmsgdt <> *Zero
     C                   Eval      Wksurg = '1'
     C                   Else
     C                   Eval      Wksurg = '2'
     C                   Endif

     C                   Move      Cmast         WKAST
     C                   Move      Cmrcvd        WKRCVD
     C                   Move      Emfein        WKFEIN
     C                   Movel     Emsic         WKSIC

     C                   Exsr      Get_Wkpycd

     C                   Move      Cmacz1        WKAZIP

     C                   Select
     C                   When      Cdsex = 'M'
     C                   Move      '1'           WKSEX
     C                   When      Cdsex = 'F'
     C                   Move      '2'           WKSEX
     C                   Other
     C                   Move      ' '           WKSEX
     C                   Endsl

     C                   Select
     C                   When      Cdmsts = 'S' or
     C                             Cdmsts = 'D' or
     C                             Cdmsts = 'W'
     C                   Eval      Wkmsts = '1'
     C                   When      Cdmsts = 'M'
     C                   Eval      Wkmsts = '2'
     C                   When      Cdmsts = 'S'
     C                   Eval      Wkmsts = '3'
     C                   When      Cdmsts = 'U'
     C                   Eval      Wkmsts = '4'
     C                   Endsl

     C                   Move      Cddtob        WKDOB2
     C                   Move      Cmhidt        WKDOH2

     C                   Select
     C                   When      Cmests = 'FT'
     C                   Eval      Wkests = '1'
     C                   When      Cmests = 'PT'
     C                   Eval      Wkests = '2'
     C                   When      Cmests = 'NE'
     C                   Eval      Wkests = '3'
     C                   When      Cmests = 'OS'
     C                   Eval      Wkests = '4'
     C                   When      Cmests = 'DS'
     C                   Eval      Wkests = '5'
     C                   When      Cmests = 'RT'
     C                   Eval      Wkests = '6'
     C                   When      Cmests = 'SL'
     C                   Eval      Wkests = '8'
     C                   When      Cmests = 'VO'
     C                   Eval      Wkests = '9'
     C                   Other
     C                   Eval      Wkests = '1'
     C                   Endsl

     C                   Clear                   Num2
     C     Wdacl_Key     Setll     Wdacl
     C                   Z-add     *Zero         Ctr               3 0

     C                   Dou       %Eof(Wdacl)
     C     Wdacl_Key     Reade     Wdacl

     C                   If        %Eof
     C                   Leave
     C                   Endif

     C                   If        Acrel <> *Blanks
     C                   Eval      Num2 = Num2 + 1
     C                   Eval      Ctr  = Ctr  + 1

     C                   Select
     C                   When      Ctr = 1
     C                   Move      Acrel         Wkbcr1
     C                   Move      Acdpcy        Wkbcd1
     C                   Move      Acddob        Wkbdob1
     C                   When      Ctr = 2
     C                   Move      Acrel         Wkbcr2
     C                   Move      Acdpcy        Wkbcd2
     C                   Move      Acddob        Wkbdob2
     C                   When      Ctr = 3
     C                   Move      Acrel         Wkbcr3
     C                   Move      Acdpcy        Wkbcd3
     C                   Move      Acddob        Wkbdob3
     C                   When      Ctr = 4
     C                   Move      Acrel         Wkbcr4
     C                   Move      Acdpcy        Wkbcd4
     C                   Move      Acddob        Wkbdob4
     C                   When      Ctr = 5
     C                   Move      Acrel         Wkbcr5
     C                   Move      Acdpcy        Wkbcd5
     C                   Move      Acddob        Wkbdob5
     C                   Endsl

     C                   Endif

     C                   Enddo

     C                   Move      Num2          WK#DEP
     C                   Move      Cmaww         WKPIWW

     C                   If        Cmwmth <> *Blanks
     C                   Movel     Cmwmth        Wkwmth
     C                   Else
     C                   Eval      Wkwmth = '1'
     C                   Endif

     C                   Move      Cmotwk        WKOTWK
     C                   Move      Cmdtrp        WKDTRP2

     C                   If        Cmsgdt <> *Zero
     C                   Eval      Wksgdt = '1'
     C                   Else
     C                   Eval      Wksgdt = '2'
     C                   Endif

     C                   Move      Cmdbdt        Wkdbdt

     C                   Move      Cmrtw         Wkrtw

      *  Benefit PTD
     C                   If        ty(1) <> *blanks

     C                   Move      ty(1)         Wkityp1
     C                   z-add(h)  bp(1)         Num9
     C                   Move      num9          Wkbptd1

     C                   If        ty(1) = '12'
     C                             or ty(1) = '50'
     C                             or wk(1) = 0
     C                   move      *zeros        Wkwben1
     C                   Else
     C                   eval      num9 = bp(1) / wk(1)
     C                   move      num9          Wkwben1
     C                   Endif

     C                   Endif

     C                   If        ty(2) <> *blanks

     C                   Move      ty(2)         WKityp2
     C                   z-add(h)  bp(2)         Num9
     C                   Move      num9          Wkbptd2

     C                   If        ty(2) = '12'
     C                             or ty(2) = '50'
     C                             or wk(2) = 0
     C                   move      *zeros        Wkwben2
     C                   Else
     C                   eval      num9 = bp(2) / wk(2)
     C                   move      num9          Wkwben2
     C                   Endif

     C                   Endif

     C                   If        ty(3) <> *blanks

     C                   Move      ty(3)         Wkityp3
     C                   z-add(h)  bp(3)         Num9
     C                   Move      num9          Wkbptd3

     C                   If        ty(3) = '12'
     C                             or ty(3) = '50'
     C                             or wk(3) = 0
     C                   move      *zeros        Wkwben3
     C                   Else
     C                   eval      num9 = bp(3) / wk(3)
     C                   move      num9          Wkwben3
     C                   Endif

     C                   Endif

     C                   If        ty(4) <> *blanks

     C                   Move      ty(4)         WKityp4
     C                   z-add(h)  bp(4)         Num9
     C                   Move      num9          Wkbptd4

     C                   If        ty(4) = '12'
     C                             or ty(4) = '50'
     C                             or wk(4) = 0
     C                   move      *zeros        Wkwben4
     C                   Else
     C                   eval      num9 = bp(4) / wk(4)
     C                   move      num9          Wkwben4
     C                   Endif

     C                   Endif

     C                   If        ty(5) <> *blanks

     C                   Move      ty(5)         Wkityp5
     C                   z-add(h)  bp(5)         Num9
     C                   Move      num9          Wkbptd5

     C                   If        ty(5) = '12'
     C                             or ty(5) = '50'
     C                             or wk(5) = 0
     C                   move      *zeros        Wkwben5
     C                   Else
     C                   eval      num9 = bp(5) / wk(5)
     C                   move      num9          Wkwben5
     C                   Endif

     C                   Endif

     C                   z-add(h)  Wkvrhi#       Num9
     C                   Move      num9          WKVRHI
     C                   z-add(h)  Wkvrhev#      Num9
     C                   Move      num9          WKVRHEV
     C                   z-add(h)  Wkvrhm#       Num9
     C                   Move      num9          WKVRHM
     C                   z-add(h)  Wkvrhed#      Num9
     C                   Move      num9          WKVRHED
     C                   z-add(h)  Wkvrho#       Num9
     C                   Move      num9          WKVRHO
     C                   z-add(h)  Wkhptd#       Num9
     C                   Move      num9          WKhptd
     C                   z-add(h)  Wkpptd#       Num9
     C                   Move      num9          WKpptd
     C                   z-add(h)  Wkoptd#       Num9
     C                   Move      num9          WKoptd

     C                   Move      Cmpoww        WKPIWW2

     C                   Z-add(H)  Cmipct        Num3
     C                   Move      Num3          Wkipct2

     C                   Move      Cmmmi         WKMMI

     C                   z-add(h)  Wkfptd#       Num9
     C                   Move      num9          Wkfptd
     C                   z-add(h)  Wklsum#       Num9
     C                   Move      num9          Wklsum
     C                   z-add(h)  Wklptd#       Num9
     C                   Move      num9          WKLPTD

     C                   Select
     C                   When      Cmcvfl = 'Y'
     C                   Eval      Wkctrv = '1'
     C                   When      Cmcvfl = 'N'
     C                   Eval      Wkctrv = '2'
     C                   Other
     C                   Eval      Wkctrv = *Blank
     C                   Endsl

     C                   z-add(h)  Wkplsb#       Num9
     C                   Move      num9          Wkplsb
     C                   z-add(h)  Wkalsb#       Num9
     C                   Move      num9          WKALSB
     C                   z-add(h)  Wkotsb#       Num9
     C                   Move      num9          WKOTSB
     C                   z-add(h)  Wklptde#      Num9
     C                   Move      num9          Wklptde
     C                   z-add(h)  Wklptdc#      Num9
     C                   Move      num9          Wklptdc
     C                   z-add(h)  Wkewptd#      Num9
     C                   Move      num9          WKEWPTD
     C                   z-add(h)  Wkpnptd#      Num9
     C                   Move      num9          Wkpnptd

     C                   Select
     C                   When      Cmssff = 'Y'
     C                   Eval      Wkssoff = '1'
     C                   When      Cmssff = 'N'
     C                   Eval      Wkssoff = '2'
     C                   Other
     C                   Eval      Wkssoff = *Blank
     C                   Endsl

     C                   Select
     C                   When      Cmueff = 'Y'
     C                   Eval      Wkuoff = '1'
     C                   When      Cmueff = 'N'
     C                   Eval      Wkuoff = '2'
     C                   Other
     C                   Eval      Wkuoff = *Blank
     C                   Endsl

     C                   Select
     C                   When      Cmpeff = 'Y'
     C                   Eval      Wkpoff = '1'
     C                   When      Cmpeff = 'N'
     C                   Eval      Wkpoff = '2'
     C                   Other
     C                   Eval      Wkpoff = *Blank
     C                   Endsl

     C                   Select
     C                   When      Cmsfff = 'Y'
     C                   Eval      Wksoff = '1'
     C                   When      Cmsfff = 'N'
     C                   Eval      Wksoff = '2'
     C                   Other
     C                   Eval      Wksoff = *Blank
     C                   Endsl

     C                   Select
     C                   When      Cmotff = 'Y'
     C                   Eval      Wkooff = '1'
     C                   When      Cmotff = 'N'
     C                   Eval      Wkooff = '2'
     C                   Other
     C                   Eval      Wkooff = *Blank
     C                   Endsl

     C                   Endsr                                                  claims

      *===========================================================================
      *  Routine to get Wdpap and process it
      *===========================================================================

     C     Get_Wdpap     Begsr

     C                   Clear                   WKALSB#
     C                   Clear                   WKDISP#
     C                   Clear                   WKDTHP#
     C                   Clear                   WKEWPTD#
     C                   Clear                   Wkfptd#
     C                   Clear                   Wkhptd#
     C                   Clear                   WKHOSP#
     C                   Clear                   Wkinct#
     C                   Clear                   WKLEGC#
     C                   Clear                   WKLEGD#
     C                   Clear                   Wklsum#
     C                   Clear                   Wklptde
     C                   Clear                   Wklptd#
     C                   Clear                   Wklptdc#
     C                   Clear                   Wklptde#
     C                   Clear                   WKOTSB#
     C                   Clear                   Wkoptd#
     C                   Clear                   WKPHPD#
     C                   Clear                   Wkpnptd#
     C                   Clear                   WKPLSB#
     C                   Clear                   Wkpptd#
     C                   Clear                   WKPRMP#
     C                   Clear                   WKPTPD#
     C                   Clear                   WKRHBP#
     C                   Clear                   WKRHPB#
     C                   Clear                   WKSTLA#
     C                   Clear                   WKTGI#
     C                   Clear                   WKVEVL#
     C                   Clear                   WKVIND#
     C                   Clear                   WKVRHBI#
     C                   Clear                   WKVRHED#
     C                   Clear                   WKVRHEV#
     C                   Clear                   WKVRHM#
     C                   Clear                   WKVRHO#
     C                   Clear                   WKVTRN#
     C                   Clear                   WKtrdt
     C                   Clear                   ty
     C                   Clear                   bp
     C                   Clear                   wk
     C                   Clear                   dy

     C     Wdpal5_Key    Setll     Wdpal5

     C                   Dou       %Eof(Wdpal5)
     C     Wdpal5_Key    Reade     Wdpal5

     C                   If        %Eof
     C                   Leave
     C                   Endif

     C                   If        Paretp = 'C' and
     C                             Wktrdt = *Blanks
     C                   Move      Patrdt        Wktrdt
     C                   Endif

     C     Wmpdl_Key     Chain     Wmpdl

     C                   If        Not %Found(Wmpdl)
     C                   Clear                   Ppd01
     C                   Endif

     C                   Eval      Paamt2 = Paamt
     C                   If        Patype = 'V' or
     C                             Patype = 'S' or
     C                             Patype = 'R'
     C                   Eval      Paamt2 = Paamt * -1
     C                   Endif

     C                   Movel(p)  Pabr2         Tab10
     C     Wtabl_Key     Chain     Pb201

     C                   If        Not %Found
     C                   Eval      B2plcd = *blank
     C                   Endif

     C                   If        Paretp = 'M'
     C                   Select
     C                   When      Pdtype = '4'
     C                   Eval      Wkhosp# = Wkhosp# + Paamt2
     C                   Eval      Wkhptd# = Wkhptd# + Paamt2
     C                   When      Pdtype = '1'
     C                   Eval      Wkpptd# = Wkpptd# + Paamt2
     C                   Other
     C                   Eval      Wkoptd# = Wkoptd# + Paamt2
     C                   Endsl
     C                   Endif

     C                   Select
     C                   When      B2plcd = 'A'
     C                   Eval      Wkpnptd# = Wkpnptd#+ Paamt2

     C                   When      B2plcd = 'G'
     C                   Eval      Wkfptd#= Wkfptd#+ Paamt2

     C                   When      B2plcd = 'L'
     C                   Eval      Wklsum# = Wklsum# + Paamt2

     C                   When      B2plcd = 'N'
     C                   Eval      Wklptde# = Wklptde# + Paamt2
     C                   Eval      Wklegd# = Wklegd# + Paamt2

     C                   When      B2plcd = 'M'
     C                   Eval      Wklptdc# = Wklptdc# + Paamt2
     C                   Eval      Wklegc# = Wklegc# + Paamt2

     C                   Endsl

     C                   Select

     C                   When      B2minr = '007'
     C                   Eval      Wkvrho# = Wkvrho# + Paamt2

     C                   When      B2minr = '055'
     C                   Eval      Wkewptd# = Wkewptd# + Paamt2

     C                   When      B2minr = '062'
     C                   Eval      Wkvevl# = Wkvevl# + Paamt2
     C                   Eval      Wkvrhev# = Wkvrhev# + Paamt2

     C                   When      B2minr = '087'
     C                   Eval      Wkssam# = Wkssam# + Paamt2

     C                   When      B2minr = '200' and CltId <> '129'
     C                   Eval      Wklptd# = Wklptd# + Paamt2

     C                   When      B2minr = '201'
     C                   Eval      Wkvind# = Wkvind# + Paamt2
     C                   Eval      Wkvrhm# = Wkvrhm# + Paamt2

     C                   When      B2minr = '202'
     C                   Eval      Wkvtrn# = Wkvtrn# + Paamt2
     C                   Eval      Wkvrhed# = Wkvrhed# + Paamt2

     C                   When      B2minr = '181'
     C                             and Patype = 'R'
     C                   Eval      Wkplsb# = Wkplsb# + Paamt

     C                   When      B2minr = '099'
     C                             and Patype = 'R'
     C                   Eval      Wkalsb# = Wkalsb# + Paamt

     C                   When      B2minr = '081'
     C                             and Patype = 'R'
     C                   Eval      Wkotsb# = Wkotsb# + Paamt

     C                   Endsl

     C                   Select

     C                   When      B2plcd = '1' or
     C                             B2plcd = '2' or
     C                             B2plcd = '7'
     C                   Eval      Wkdisp# = Wkdisp# + Paamt2

     C                   When      B2plcd = 'T'
     C                   Eval      Wkptpd# = Wkptpd# + Paamt2

     C                   When      B2plcd = 'I'
     C                   Eval      Wkrhbp# = Wkrhpb# + Paamt2

     C                   When      B2plcd = '4' or
     C                             B2plcd = '5'
     C                   Eval      Wkprmp# = Wkprmp# + Paamt2

     C                   When      B2plcd = '6'
     C                   Eval      Wkdthp# = Wkdthp# + Paamt2

     C                   When      B2plcd = 'L'
     C                   Eval      Wkstla# = Wkstla# + Paamt2
     C                   Move      Patrdt        Wksumdt

     C                   Endsl

     C                   If        Pdtype = '1' and
     C                             Paretp = 'M'
     C                   Eval      Wkphpd# = Wkphpd# + Paamt2
     C                   Endif

      *  Benefit Info
     C                   If        B2bnft <> ' '

     C                   eval      i = 1
     C     B2bnft        lookup    ty(i)                                  98

     C  N98              eval      i = 1
     C  N98'  '          lookup    ty(i)                                  98

     C                   if        i < 6
     C                   eval      ty(i) = B2bnft
     C                   Eval      bp(i) = bp(i) + Paamt2

     C                   exsr      srwkdy
     C                   endif

     C                   endif

     C                   Enddo

     C                   Endsr                                                  claims

      *===========================================================================
      *  Routine to get Wkpycde to load array with rate dates
      *===========================================================================

     C     Get_Wkpycd    Begsr

     C                   Clear                   Wkpycd
     C                   Clear                   Num11

     C     Wdell5_Key    Chain     Wdell5
     C                   If        %Found

     C     Wmrsl_Key5    Chain     Wmrsl

     C                   If        %Found
     C                   Eval      Risk_St = Rsjrst
     C                   Else
     C                   Eval      Risk_St = *Blanks
     C                   Endif

     C     Wdell8_Key    Setll     Wdell8

     C                   Dou       %Eof(Wdell8)
     C     Wdell8_Key    Reade     Wdell8

     C                   If        %Eof
     C                   Leave
     C                   Endif

     C     Wmrsl_Key8    Chain     Wmrsl

     C                   If        %Found and
     C                             Rsjrst = Risk_St
     C     Wmepl1_Key    Setll     Wmepl1

     C                   Dou       %Eof(Wmepl1)
     C     Wmepl1_Key    Reade     Wmepl1

     C                   If        Not %Eof(Wmepl1)
     C                   If        El8faf = 'A'
     C                   Eval(H)   Num11 = Epaapr + Num11
     C                   Else
     C                   Eval(H)   Num11 = Epeapr + Num11
     C                   Endif

     C                   Endif

     C                   Enddo
     C
     C                   Endif

     C                   Enddo
     C                   Endif

     C                   Select
     C                   When      Num11 = *Zero
     C                   Eval      Wkpycd = '1'
     C                   When      Num11 < 100001
     C                   Eval      Wkpycd = '2'
     C                   When      Num11 < 1000001
     C                   Eval      Wkpycd = '3'
     C                   When      Num11 < 10000001
     C                   Eval      Wkpycd = '4'
     C                   When      Num11 > 10000000
     C                   Eval      Wkpycd = '5'
     C                   Other
     C                   Endsl

     C                   Endsr                                                  claims

      *===========================================================================
      *  Rate_array - Subroutine to load array with rate dates
      *===========================================================================
     C     Rate_array    begsr

     C                   z-add     9999999       rate_dt
     C                   eval       rate_dt(1) = elpfdt
     C
     C                   if        elandt<>0 and elandt<elpfdt
     C     *cymd         move      elpfdt        from_dt
     C     *cymd         move      elandt        anniv_dt
     C                   adddur    1:*years      anniv_dt
     C
     C                   if        anniv_dt > from_dt
     C                   eval       rate_dt(1) = elandt
     C     *cymd         move      anniv_dt      rate_dt(2)
     C                   endif                                                  anniv_dt>from_dt
     C
     C                   endif                                                  elandt<>0
     C
     C                   eval      rate_dt(3) = elptdt

     C
     C                   if        expmod(2)<>0
     C                   eval       rate_dt(4) = effmod(2)
     C                   endif
     C
     C                   if        expmod(3)<>0
     C                   eval       rate_dt(5) = effmod(3)
     C                   endif

      *  If more than one date in array the same, remove all but one
     C                   do        5             i
     C                   if        rate_dt(i)<>9999999
     C
     C                   eval       j=5-i
     C                   do        j             k
     C                   if        rate_dt(i)=rate_dt(i+k)
     C                   eval       rate_dt(i+k)=9999999
     C                   endif
     C                   enddo                                                  do j k
     C
     C                   endif                                                  rate_dr(i)<>9999999
     C                   enddo                                                  do 5 i

     C                   sorta     rate_dt

      *  Fill the experience mod array with appropriate dates per rate_dt
     C                   z-add     expmod(1)     mod

     C                   do        3             m
     C                   do        5             n
     C
     C                   if        effmod(m) <= rate_dt(n)
     C                             and expmod(m)<>0
     C                   eval       mod(n) = expmod(m)
     C                   endif
     C
     C                   enddo
     C                   enddo

     C                   Endsr                                                  Rate_array
      *===========================================================================

      *============================================================================
      *  Rsrv_Amt  - Subroutine to accumulate remaining reserves (cmrr_) and pay-
      *              ments against reserves (cmar_) based on remaining reserve
      *              type associated with rem rsrv code:
      *                  IndemRem - remaining indemnity
      *                  MedicRem - remaining medical
      *                  AlaeRem  - remaining allocated loss adjustment
      *                  IndemPay - paid indemnity
      *                  MedicPay - paid medical
      *                  AlaePay  - paid allocated loss adjustment
      *===========================================================================
     C     Rsrv_Amt      Begsr
      *
     C                   eval      IndemRem = 0
     C                   eval      IndemPay = 0
     C                   eval      MedicRem = 0
     C                   eval      MedicPay = 0
     C                   eval      AlaeRem  = 0
     C                   eval      AlaePay  = 0
     C                   eval      LegalPay = 0
     C                   eval      LegalRem = 0
      *
     C                   do        10            i
     C                   movel(p)  ResvCod(i)    kkkey
     C     tbky          chain     prt01                              60
      *
     C                   Eval      WKTGI# = WKTGI# +
     C                             Remrsv(I) + Payrsv(I)
     C                   If        Rtctgy = '1B' and CltId = '129'
     C                   Eval      Wklptd# = Wklptd# +
     C                              Payrsv(I)
     C                   Endif

     C                   If        Rtctgy = 'RE'
     C                   Eval      Wkvrhbi# = Wkvrhbi# +
     C                             Remrsv(I) + Payrsv(I)
     C                   Eval      Wkvrhi# = Wkvrhi# +
     C                             Remrsv(I) + Payrsv(I)
     C                   Endif

     C                   select
     C                   when      rttype='C'
     C                   add       RemRsv(i)     IndemRem
     C                   add       PayRsv(i)     IndemPay
     C                   Add(H)    RemRsv(i)     Wkinct#
     C                   Add(H)    PayRsv(i)     Wkinct#

     C                   when      rttype='M'
     C                   add       RemRsv(i)     MedicRem
     C                   add       PayRsv(i)     MedicPay

     C                   when      rttype='E'
     C                             or rttype='L'
     C                   add       RemRsv(i)     AlaeRem
     C                   add       PayRsv(i)     AlaePay

     C                   endsl

     C                   enddo

     C                   endsr                                                  Rsrv_Amt
      *============================================================================
      *   SUBROUTINE CALCULATE WEEKS AND DAYS            *
      *============================================================================
     C     srwkdy        begsr

      *        CALCULATE WEEKS & DAYS

     C                   if        patype = 'P'
     C                             and paamt >= 0
     C                             or patype = 'Z'
     C                             and paamt >= 0
     C                             or patype = 'A'
     C                             and paamt >= 0
     C                             or patype = 'M'
     C                             and paamt >= 0
     C                             or patype = 'R'
     C                             and paamt < 0
     C                             or patype = 'S'
     C                             and paamt < 0
     C                             or patype = 'V'
     C                             and paamt < 0

     C                   eval      wk(i) = wk(i) + pawks
     C                   eval      xxdays = dy(i) + padays
     C                   if        xxdays >= 7
     C                   eval      xxweek = xxdays / 7
     C                   eval      wk(i) = wk(i) + xxweek
     C                   eval      xxweek = xxweek * 7
     C                   eval      dy(i) = xxdays - xxweek
     C                   else
     C                   eval      dy(i) = xxdays
     C                   endif

     C                   else

     C                   eval      wk(i) = wk(i) - pawks
     C                   eval      xxdays = dy(i) - padays
     C                   if        xxdays < 0
     C                   eval      xxweek = (xxdays / 7) - 1
     C                   eval      wk(i) = wk(i) + xxweek
     C                   eval      xxweek = xxweek * 7
     C                   eval      dy(i) = xxdays - xxweek
     C                   else
     C                   eval      dy(i) = xxdays
     C                   endif

     C                   endif

     C                   endsr
      *============================================================================
      *%%%%%%%%%%%%%%  end   subroutines   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      *%%%%%%%%%%%%%%  begin output        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     Oqsysprt   e            total          1 06
     O                                            9 'W8116R1'
     O                                           59 'Madison Unit Statistical -
     O                                              Reporting'
     O                       udate         Y     79
     O          e            total          3
     O                                           54 'Transmittal Letter Totals'
     O          e            total          2
     O                                           40 'Number of header -
     O                                              records . :'
     O                       xthcnt        4     57
     O          e            total          2
     O                                           40 'Number of exposure -
     O                                              records :'
     O                       xtecnt        4     57
     O          e            total          2
     O                                           40 'Total exposure amount -
     O                                               . . :'
     O                       xteamt        j     58
     O          e            total          2
     O                                           40 'Total written premium -
     O                                               . . :'
     O          e            total          2
     O                                           40 'Number of loss -
     O                                              records . . :'
     O                       xtlcnt        4     57
     O          e            total          2
     O                                           40 'Total of incurred -
     O                                              indemnity:'
     O                       xtiinc        j     58
     O          e            total          2
     O                                           40 'Total of incurred -
     O                                              medical  :'
     O                       xtminc        j     58
     O          e            total          2
     O                                           40 'Total of paid -
     O                                              indemnity  . :'
     O                       xtinpd        j     58
     O          e            total          2
     O                                           40 'Total of paid -
     O                                              medical  . . :'
     O                       xtmdpd        j     58
     O          e            total          2
     O                                           40 'Total of ALAE paid -
     O                                              . . . . :'
     O                       xtexpp        j     58
     O          e            total          2
     O                                           40 'Total of ALAE incurred -
     O                                              . . :'
     O                       xtexpi        j     58
      *%%%%%%%%%%%%%%  end   output        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      *%%%%%%%%%%%%%%  begin procedures    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *===========================================================================
      * rsv_typ - this procedure receives an array of the deductible reserve
      *           codes which it examines and returns a two character
      *           deductible reserve type as follows:
      *             '00' - if no medical or indemnity type codes
      *             '01' - if medical type codes but no indemnity type codes
      *             '02' - if indemnity type codes but no medical type codes
      *             '03' - if both medical and indemnity codes
      *===========================================================================
     P rsv_typ         b                   export
     D rsv_typ         pi             2
     D   cod_arr                      2    Dim(10) Const
     D
     D  i              s              3  0
     D  mtype          s              3  0
     D  ctype          s              3  0
     D  CandM          s              2  0
     D  dedtype        s              2

      *  Process all deductible reserve codes
     C                   do        10            i
     C
     C                   if        cod_arr(i)<>' '
     C                   movel(p)  cod_arr(i)    kkkey
     C
      *    Chain to the reserve type code table file to determine if
      *    code is medical ('M') or indemnity ('I').
     C     tbky          chain     prt01                              60
     C                   if        *in60=*off
     C
     C                   select
     C                   when      rttype='M'
     C                   eval        mtype=1
     C                   when      rttype='C'
     C                   eval        ctype=2
     C                   endsl
     C
     C                   endif                                                  *in60=*off
     C
     C                   endif                                                  cod_arr(i)=' '
     C
     C                   enddo                                                  do 10 i
     C
      *  Calc and assign value to be returned based on description in procedure header
     C                   eval      CandM=ctype+mtype
     C                   move      CandM         dedtype
     C
     C                   return    dedtype
     C
     P rsv_typ         e
      *===========================================================================




      *===========================================================================
      * split - this procedure splits employer payrolls based on rating
      *         periods within a policy
      *===========================================================================
     P split           b                   export
     D split           pi
     D   ##co#                        3  0 Const
     D   ##fnd                        3  0 Const
     D   ##fyr                        3  0 Const
     D   ##emp#                       9  0 Const
     D   ##div                        5  0 Const
     D   ##clas                       5  0 Const
     D   ##spec                       1    Const
     D   ##idx                        3  0 Const
     D   ##eamt                      11  2
     D   ##prem                      11  2
     D**** Beg Chg 06/17/24 ***********************************************
     D****##arat                       7  2 Const
     D   ##arat                       8S 3 Const
     D**** End Chg 06/17/24 ***********************************************
     D   ##efdt                       7  0 Const
     D   ##beg_dt                     7  0 Const
     D   ##end_dt                     7  0 Const
     D   ##percap                     1    Const
     D
     D  fromdt         s              7  0
     D  todt           s              7  0
     D  pro_exp        s             11  2
     D  pro_prm        s             11  2

     C     epky2         klist
     C                   kfld                    ##co#
     C                   kfld                    ##fnd
     C                   kfld                    ##fyr
     C                   kfld                    ##emp#
     C                   kfld                    ##div
     C                   kfld                    ##clas
     C                   kfld                    ##spec
     C                   kfld                    ##efdt

     C     epky3         klist
     C                   kfld                    ##co#
     C                   kfld                    ##fnd
     C                   kfld                    ##fyr
     C                   kfld                    ##emp#
     C                   kfld                    ##div
     C                   kfld                    ##clas
     C                   kfld                    ##spec

     C                   eval      fromdt = ##efdt
     C                   eval      kkefdt = rate_dt(##idx)
     C     epky2         setgt     pep02
     C     epky3         reade     pep02                                  61
     C                   if        *in61 = *off
     C                   eval       todt = ep2efdt
     C                   else
     C                   eval       todt = ##end_dt
     C                   endif
     C
     C                   if        todt > rate_dt(##idx+1)
     C
     C                   eval(h)     pro_exp = pro_rate(##beg_dt :
     C                                               rate_dt(##idx+1) :
     C                                                  fromdt : todt :
     C                                                     ##eamt)
     C                   eval(h)    expamt(##idx) = expamt(##idx) + pro_exp
     C
     C                   eval(h)     pro_prm = pro_rate(##beg_dt :
     C                                               rate_dt(##idx+1) :
     C                                                  fromdt :todt :
     C                                                     ##prem)
     C                   eval(h)    pramt(##idx) = pramt(##idx) + pro_prm
     C
     C                   eval(h)    ##eamt = ##eamt - pro_exp
     C                   eval(h)    ##prem = ##prem - pro_prm
     C
     C                   callp     split(##co# : ##fnd : ##fyr : ##emp# :
     C                               ##div : ##clas : ##spec : ##idx+1 :
     C                               ##eamt : ##prem : ##arat : ##efdt :
     C                               rate_dt(##idx+1) : ##end_dt : ##percap)
     C
     C                   else
     C
     C                   eval(h)   expamt(##idx) = expamt(##idx) + ##eamt
     C                   eval(h)   pramt(##idx) = pramt(##idx) + ##prem
     C                   endif

     C                   if        ##percap=*on
     C                   eval(h)    expamt(##idx) = 10 * expamt(##idx)
     C                   eval(h)    pramt(##idx) = 100 * pramt(##idx)
     C                   endif


      *          *fill buckets*
     C                   eval      clscd(##idx)=##clas
     C                   eval      expmd(##idx)=mod(##idx)
     C                   eval      pefdt(##idx)=rate_dt(##idx)
     C                   eval      manrt(##idx)=##arat
     C                   eval      splitp(##idx)=##idx-1
     C                   eval      rttier(##idx)=prtier
     C  N75              eval      expact(##idx)=preact
     C   75              eval      expact(##idx)='  '

     P split           e
      *===========================================================================

      *===========================================================================
      * pro_rate - this procedure pro rates the amount passed in using the dates passed
      *            in to determine the factor:       (tdt - fdt)
      *                                            ---------------- * amount
      *                                            (end_dt - stdt)
      *===========================================================================
     P pro_rate        b                   export
     D pro_rate        pi            11  2
     D   fdt                          7  0 Const
     D   tdt                          7  0 Const
     D   stdt                         7  0 Const
     D   endt                         7  0 Const
     D   amount                      11  2 Const

     D  dtfrom         s               d   datfmt(*usa)
     D  dtto           s               d   datfmt(*usa)
     D  dtstrt         s               d   datfmt(*usa)
     D  dtend          s               d   datfmt(*usa)
     D
     D  xdays          s              5  0
     D  ydays          s              5  0

     C     *cymd         move      fdt           dtfrom
     C     *cymd         move      tdt           dtto
     C     *cymd         move      stdt          dtstrt
     C     *cymd         move      endt          dtend

     C     dtto          subdur    dtfrom        xdays:*days
     C     dtend         subdur    dtstrt        ydays:*days
     C
     C                   return    (xdays/ydays) * amount

     P pro_rate        e
      *===========================================================================


      *===========================================================================
      *  wrt_xprem - writes a premium exposure record
      *===========================================================================
     P wrt_xprem       b                   export
     D wrt_xprem       pi
     D   code                         4    Const
     D   amt                         11  2 Const

     C                   clear                   peo01

     C                   eval       eocarr=carrier#
     C                   eval       eopol#=policy#
     C                   eval       eocert=cert#
     C                   eval       eostcd=mainState#
     C                   eval       eopfdt=poleffdt

      *    (Class code)
     C                   move      code          eoclas
      *    (Experience mod)
     C                   z-add     expmod(1)     exp_md
     C                   move      exp_md        eoemod
      *    (Period effective date)
     C                   move(p)   pefdt(1)      eoefdt
      *    (Exposure amount)
     C                   move      *zeros        eoeamt
      *    (Rating tier)
     C                   eval      eotier=*blank
      *    (Exposure act)
     C                   eval      eoeact=*blank
      *    (Premium amount)
     C                   z-add     amt           pr_amt
     C                   move(p)   pr_amt        eoprem
      *    (Manual rate)
     C                   move      *zeros        eorate
      *    (Split period)
     C                   move      *zero         eosplt
     C                   write     peo01
     C                   eval      xtecnt = xtecnt + 1
     C                   clear                   peo01

     P wrt_xprem       e
      *===========================================================================

      *=========================================================================
      * Query whether policy is small or large deductible
      *=========================================================================
     P rtvDedSize      b

     D rtvDedSize      pi             1                                         return S/L
     D  co#                                like(ELCO#)
     D  bscd                               like(ELBSCD)

     D kkco#           s                   like(BSCO#)
     D kkkey           s                   like(BSKEY)

     C     bsky          Klist
     C                   Kfld                    kkco#
     C                   Kfld                    kkkey

     C     rsky          Chain     prs01
     C                   If        not(%found)
     C                   Eval      RSJRST = *blank
     C                   Return    ' '
     C                   Endif

     C                   Eval      kkco# = co#
     C                   Movel(p)  bscd          kkkey
     C     bsky          Chain     pbs01
     C                   Select
     C                   When      BSLPGM = 'L'
     C                   Return    'L'
     C                   Other
     C                   Return    'S'
     C                   Endsl

     P rtvDedSize      e

      *=========================================================================
      * rtvSmDedInfo  - retrieves DDAMPO and DDAMAG
      *=========================================================================
     P rtvSmDedInfo    b
     D rtvSmDedInfo    pi
     D  clas                               like(DDCLAS)
     D  code                               like(DDCODE)
     D  plan                               like(DDPLAN)
     D  ampo                               like(DDAMPO)
     D  amag                               like(DDAMAG)
     D  kkefdt         s                   like(DDEFDT)

     C     ddky1         Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    RSJRST
     C                   Kfld                    ELDCDE
     C                   Kfld                    kkefdt

     C     ddky          Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    RSJRST
     C                   Kfld                    ELDCDE

     C                   Move      ELCO#         qqco#
     C                   Move      ELFND         qqfnd
     C                   Move      ELEMP#        q1emp#
     C                   Move      ELDIV         q1div
     C                   Move      ELFYR         q1fyr
     C                   Move      ELDCEF        q2date

     C                   Call      'SREFFD'
     C                   Parm                    qqco#             3
     C                   Parm                    qqfnd             3
     C                   Parm                    q1fyr             3
     C                   Parm                    q1emp#            9
     C                   Parm                    q1div             5
     C                   Parm                    q2date            7

     C                   Move      q2date        kkefdt
     C     ddky1         Setgt     pdd01
     C     ddky          Readpe    pdd01
     C                   Dow       not(%eof) and DDSTAT <> 'A'
     C     ddky          Readpe    pdd01
     C                   Enddo

     C                   If        not(%eof)
     C                   Eval      clas = DDCLAS
     C                   Eval      code = DDCODE
     C                   Eval      plan = DDPLAN
     C                   Eval      ampo = DDAMPO
     C                   Eval      amag = DDAMAG
     C                   Endif

     P rtvSmDedInfo    e
      *=========================================================================
      * rtvLgDedInfo - retrieves LDAMPO and LDAMAG
      *=========================================================================
     P rtvLgDedInfo    b
     D rtvLgDedInfo    pi
     D  clas                               like(DDCLAS)
     D  plan                               like(DDPLAN)
     D  ampo                               like(DDAMPO)
     D  amag                               like(DDAMAG)

     C     elky          Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    ELEMP#
     C                   Kfld                    ELDIV
     C                   Kfld                    ELFYR

     C     elky          Chain     pld02
     C                   If        %found
     C                   Eval      clas = LDCLS1
     C                   Eval      plan = LDPLN1
     C                   Eval      ampo = LDAMP1
     C                   Eval      amag = LDAMG1
     C                   Endif

     P rtvLgDedInfo    e
