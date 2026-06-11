      //*********************************************************************
     H Copyright('(c) 2024 Sapiens. All Rights Reserved.')
      //
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     //*********************************************************************
     // PROGRAM: F4000N
     // DESCRIPTION: ADJUDICATION GENERAL - FOR ALL STATES AND
     //              EFFECTIVE DATES
     //*********************************************************************
      //                                                                    *
     // PROGRAMMER NOTES:                                                  *
     //                                                                    *
     // 1.TO IMPROVE EFFICIENCY TWO MULTIPLE OCCURANCE DATA STRUCTURES     *
     //   ARE USED 1) MDSRU - WHICH IS LOADED WITH FDFRP(STATE RULES)      *
     //            2) MDSP9 - WHICH IS LOADED WITH FDCAL9(DETAIL CHARGE    *
     //                       HISTORY RECORDS)                             *
     //                                                                    *
     //   MDSRU IS RELOADED ONLY IF A SERVICE DATE USES A DIFFERENT        *
     //   EFFECTIVE DATE THAN THE CURRENT ONE.  MDSP9 IS LOADED FOR ANY    *
     //   RULE THAT NEEDS TO LOOK BACK THROUGH HISTORY.                    *
     //                                                                    *
     // 2.WHEN ADDING FIELDS TO FDFRP OR FDCAP YOU MUST ALSO ADD FIELDS    *
     //   TO THE CORRESPONDING DATA STRUCTURE EITHER MDSRU OR MDSP9        *
     //                                                                    *
     // 3.THE FIRST PASS READS THROUGH FACAP IN DESCENDING MRA ORDER       *
     //   THE SECOND PASS READS THROUGH FACAL WHICH IS FOR MEDICINE        *
     //   CODES ONLY AND IS IN ASCENDING DATE ORDER.                       *
     //                                                                    *
     // 4.WHEN ADDING FIELDS TO FACAP YOU MUST ADD THEM TO THE I SPECS     *
     //   PCAWK1 AND PCAWK2 AND ALSO TO SUBROUTINE LOADF.                  *
     //                                                                    *
     // 5.SetPay indicates the rule is set for payment.                    *
     //                                                                    *
     // 6.Proc indicates the payment has been processed.                   *
     //*********************************************************************
     //  CREATED: 10/21/97  C:59   PROJECT: FL011
     //*********************************************************************
      //                                                                    *
     //  REVISIONS:                                                        *
      //                                                                    *
      //  06/14/24 C:079 Ticket: 115160/099                                 *
      //                 Pay DE Anes correctly when set to pay POC          *
      //                                                                    *
      //  07/18/23 C:342 Project: NY060                                     *
      //                 NY Compliance: CARC/RARC Update                    *
      //                                                                    *
      //  05/23/23 C:079 Support: 99/110539                                 *
      //             Fix infinite loop issue due to Iter w/o Read           *
      //                                                                    *
      //  11/12/20 C:079 Support: 129/92543                                 *
      //             Fix anesthesia rounding issue                          *
      //                                                                    *
      //  08/04/20 C:295 Project: FS576                                     *
      //             Fix detail screen blow up                              *
      //                                                                    *
      //  11/13/19 C:212 Support: 85075
      //             Fix duplication of rules for pharmacy                  *
      //                                                                    *
      //  06/03/19 C:212 Project: NY046
      //             079 Addon Codes related processing                     *
      //                                                                    *
      //  04/15/19 C:079 Project: NY044
      //                 Pay multiple units for PT/OT RVU Codes             *
      //                 Use PT/OT eval/re-eval flag PR1AF                  *
      //                                                                    *
      //  03/05/19 C:079 Support: 99/77655
      //                 If letter in position 5 skip Anesthesia rules      *
      //                                                                    *
      //  12/08/18 C:079 Support: 99/75286
      //                 Monitor for blank client ID                        *
      //                                                                    *
      //  06/01/18 C:079 Project: FS546
      //                 1. Convert to Free Form RPG                        *
      //                 2. Remove all Goto statements                      *
      //                 3. Simplify how rules are read/processed           *
      //                 4. Fix Multi-rule logic                            *
      //                 5. Remove all revision comments, clean up spacing  *
      //                    and comments                                    *
     //*********************************************************************
     //  INDICATOR USAGE:                                                  *
     //                                                                    *
     //  01 - Used on the billed specialty field (FRSPCD)                  *
     //  02 - Used on the billed from code field (FRBFCD)                  *
     //  03 - Used on the billed to code field (FRBTCD)                    *
     //  03 - Used on the billed type of drug field (FRPTYP)               *
     //  04 - Used on the billed from modifier field (FRBFMD)              *
     //  05 - Used on the billed to modifier field (FRBTMD)                *
     //  06 - Used on the billed from place of service field (FRBFPS)      *
     //  07 - Used on the billed to place of service field (FRBTPS)        *
     //  08 - Used on the billed type of service field (FRBTOS)            *
     //  09 - Used on the billed body part (FRBPRT)                        *
     //  10 - Used on the compared from code field (FRCFCD)                *
     //  11 - Used on the compared to code field (FRCTCD)                  *
     //  12 - Used on the compared to modifier field (FRCMOD)              *
     //  13 - Used on the compared to modifier field (FRCMOD)              *
     //  14 - Used on the compared to type of service (FRCTOS)             *
     //  15 - Used on the compared to type of bill (FRCTOB)                *
     //  16 - Used on the compared to same date field (FRCSDT)             *
     //  17 - Used on the compared to history date field (FRCHDT)          *
     //  18 - Used on the compared to same provider field (FRCSPR)         *
     //  19 - Used on the compared to diff provider field (FRCDPR)         *
     //  20 - Used on the compared to future days field (FRCFDY)           *
     //  21 - Used on the compared to past days field (FRCPDY)             *
     //  22 - Used on the compared to # procedures field (FR#PRC)          *
     //  23 - Used on the compared to # days field (FR#DAY)                *
     //  24 - Used on the processing screen for EOMB code (FREOB)          *
     //  25 - Used on the processing screen for message (FRMSG)            *
     //  26 - Used on the processing screen to pay MRA (FRPMR)             *
     //  27 - Used on the processing screen to pay charged (FRPCH)         *
     //  28 - Used on the processing screen to disallow (FRDIS)            *
     //  29 - Used on the processing screen to pay percent (FRPCT)         *
     //  30 - Used on the processing screen to pay an amount (FRAMT)       *
     //  31 - Used on the processing screen to pay anes miles (FRPANE)     *
     //  32 - Used on the processing screen to pay ASC amt (FRPASC)        *
     //  33 - Used in ANES subroutine to pay anes amount                   *
     //  34 - Used on the processing screen to pay prov serv code (FRPPSC) *
     //  35 - Used on the processing screen to pay DRG rates (FRPDRG)      *
     //  36 - Used on the processing screen to pay inpt SNF (FRPSNI)       *
     //  37 - Used on the processing screen to pay outp SNF (FRPSNO)       *
     //  38 - Used on the processing screen to pay prof amount (FRPROF)    *
     //  40 - Used on the compared to # proc greater than (FR#GPC)         *
     //  41 - Used on the compared to # days greater than (FR#GDY)         *
     //  42 - Used on the compared to separate procedure (FRCSEP)          *
     //  43 - Used on the compared to asterisk procedure (FRCAST)          *
     //  44 - Used on the processing screen to pay fac fee (FRPFAC)        *
     //  45 - Used on the processing screen to pay per diem (FRPPDM)       *
     //  46 - Used on the processing screen to pay CPT (FRPCPT)            *
     //  47 - Used on the processing screen to anes hourly (FRPANE)        *
     //  48 - Used on the processing screen for 2nd pass (FRPAS2)          *
     //  49 - Used on the processing screen to pay peer grp (FRPPGP)       *
     //  50 - Used on the processing screen to pay hosp OP (FRPOUT)        *
     //  51 - Used on the compared to same bill field (FRCSBL)             *
     //  52 - Used on the processing screen to anes rental (FRPANE)        *
     //  53 - Used to pay MRA even if > charged (FRPANE) value=M           *
     //  54 - Used for chaining                                            *
     //  55 - Used to pay the difference between charge/MRA (FRPDIF)       *
     //  56 - Used on the compared to stop-loss field (FRLOSS)             *
     //  57 - Used on the compared to physical therapy time (FRPTTM)       *
     //  58 - Used on the compared for initial treatment period (FRTRMT)   *
     //  59 - Used on the compared to modifier to ignore (FRIGMD)          *
     //  60 - Used for misc purpose usually on chains                      *
     //  61 - Used for misc purpose usually on chains                      *
     //  62 - Used for misc purpose usually on chains                      *
     //  63 - Used on the processing screen to pay CORF amount (FRPCRF)    *
     //  64 - Used on the processing screen to pay Home Health (FRPHHI)    *
     //  65 - Used on the processing screen to pay Home Health (FRPHHO)    *
     //  66 - Used on the processing screen to pay Outp RCC (FRPORC)       *
     //  67 - Used on the processing screen to pay Outp Phar RCC (FRPPOR)  *
     //  68 - Used on the processing screen to pay Inp Phar RCC (FRPPIR)   *
     //  69 - Used on the processing screen to pay MRA*Units (FRPMR)       *
     //  70 - Used on the billed screen for follow up type (FRBFUT)        *
      //  71 - Used on the processing screen to pay pharmacy AWP            *
      //  72 - Used on the processing screen to pay up to total chg (FRPCH) *
      //  73 - Used on the processing screen to pay the phy ther per visit  *
      //  74 - Used on the processing screen to pay rural/urban amb. miles  *
      //  75 - Used on the compared to screen to check endoscopic base code *
      //  76 - Used on the billed screen to check endoscopic base code      *
      //  77 - Used on the processing screen to pay pr,intra and post oper  *
      //  78 - Used on the compared to screen to count multiple units       *
      //  79 - Used on the processing screen to pay Texas HPSA - FRPMR=H    *
      //  80 - Used on the processing screen to pay Tenn. PT/OT             *
      //  81 - Used on the compared to screen for #days frm svc dt          *
      //  82 - Used for determining Rule Use field ('G')                    *
      //  83 - Used for pain management processing(FRPANE='V')              *
      //  84 - Pay charged up to allowance for primary (FRpCH)              *
      //  85 - Not Used                                                     *
      //  86 - Pay charged up to total charges on bill (FRpCh)              *
      //  87-98 *** NOT USED ***                                            *
     //  99 - Used for misc purpose; error on calls, on CHAINs             *
     //*********************************************************************
     // FILE SPECIFICATIONS:
     //*********************************************************************
     //
     // Detail Charge file logicals
     //
     Ffdcal5    uf   e           k disk    infds(cafds)
     Ffdcalb    if a e           k disk    rename(pca01:pca09)
     Ffdcal15   if   e           k disk    rename(pca01:pca10)
     Ffdcalo    if a e           k disk    rename(pca01:pca0o)
     Ffdcalf    if   e           k disk    rename(pca01:pca0p)
     Ffdcals    if   e           k disk    rename(pca01:pca0s)
     Ffdcalap   if   e           k disk    rename(pca01:pca0a) prefix(CP:2)
     Ffdcal23   if   e           k disk    rename(pca01:pca23) prefix(tr:2)
     Ffdcal27   if   e           k disk    rename(pca01:pca27) prefix(tx:2)
     //
     // Detail Charge workfile physical and logical
     //
     Ffacap     if a e           k disk    usropn rename(pca01:pcawk1)
     Ffacal     if a e           k disk    usropn rename(pca01:pcawk2)
     //
     // State rules file logical
     //
     Ffdfrl5    if a e           k disk
     Ffdfrl0    if   e           k disk    rename(pfr01:pfr02) prefix(f1:2)
     //
     // Claims master file logical
     //
     Ffmcml1    if a e           k disk
     //
     // Adjudication message file logicals
     //
     Ffdmgp     o    e           k disk
     Ffdmgl6    uf   e           k disk    rename(pmg01:pmg02)
     Ffdmgl5    if   e           k disk    rename(pmg01:pmg03)
     Fftmil     if   e           k disk
     //
     // System User Parameter file logical
     //
     Fsmupl     if   e           k disk
     //
     // State override table file logical
     //
     Fftabl     if   e           k disk
      //
     // State override procedure code logical
      //
     Ffmprl     if a e           k disk
      //
     // Reasonable and customary file logical
      //
     Ffmrcl     if a e           k disk    rename(prc01:prc02)
     //
     // EOMB code file logicals
     //
     Ffddel     if   e           k disk
     F                                     rename(pde01:pde02)
     Ffddel4    uf a e           k disk
     //
     // EOB master file logical
     //
     Ffaeol4    if   e           k disk    infds(dbfds)
     Ffaeolg    if   e           k disk    rename(peo01:pxx01) prefix(G)
     //
     // Physician detail file logical
     //
     Ffddhl1    if   e           k disk    rename(pdh01:pdh02)
     //
     // Hospital detail file logical
     //
     Ffddul     if   e           k disk
     //
     // Pharmacy detail file logicals
     //
     Ffddpl     if   e           k disk
     Ffddpl2    if   e           k disk    rename(pdp01:pdp02)
     Ffddpl4    if   e           k disk    rename(pdp01:pdp04)
     //
     // Dental detail file logical
     //
     Ffdddl     if   e           k disk
     //
     // Hospital Header logical
     //
     Ffaeul     if   e           k disk
     //
     // Physician Header logical
     //
     Ffaehl     if   e           k disk
     //
     // Dental Header logical
     //
     Ffaedl     if   e           k disk
     //
     // Pharmacy Header logical
     //
     Ffaepl     uf   e           k disk
     //
     // Correct Coding file logicals
     //
     Ffmbnl3    if   e           k disk
     Ffmmul3    if   e           k disk
     Ffteblcc   if   e           k disk    rename(peb01:peb02)
     //
     // Adjudication Control file logical
     //
     Ffdrul1    if   e           k disk
     //
     // Network Contract file logical
     //
     Ffms4l3    if   e           k disk
     //
     // Letter Detail Transaction file logical
     //
     FFAL2L1    if   e           k disk    usropn
     //
     // Illinois Rehab Per Diem Rate file logical
     //
     Fftrhlil   if   e           k disk
     //
     // Diagnosis Code file logical
     //
     Fsmdil     if   e           k disk
     //
     // Group file logical
     //
     Ffmfnl     if   e           k disk
     //
     // Medicare HIPPS Code file logical
     //
     Ffmhpl1    if   e           k disk
     //
     // LA Provider Parrish Sales Tax file logical
     //
     Ffmp3l2    if   e           k disk    usropn prefix(px:2)
     //
     // Provider Auxiliary file logical
     //
     Ffmp1l     if   e           k disk
     //
     // Multi-Rule file logical
     //
     Ffarull1   uf a e           k disk
     //
     // Medicare OPPS APC file logical
     //
     Fftapl     if   e           k disk
    F** CMS Addon Codes File
     Ffmaol1    if   e           k disk
     //*********************************************************************
     // END FILE SPECIFICATIONS
     //*********************************************************************
     //
     D fuf             s              7  0 dim(500)
     D fut             s              7  0 dim(500)
     D flg             s              1    dim(500)
     D fudseq          s              5  0 dim(500)
     D date            s              7  0 dim(999)
     D mod             s              2    dim(4) ascend
     D ptarray         s              5    dim(50)
     D QCA             s             56    dim(1) ctdata
     D openflg         s               n   inz('0')
     D                 ds
     D xxcmd                   1     56
     D xxajst                 40     41
     D kkmseq          s                   like(mimseq) inz(0)
     D kkmsg#          s                   like(mimsg#)
     D xxpmra          s                   like(caoth2)
     D totchg          s                   like(eochrg)
     D xxtotc          s                   like(eochrg)
     D ##ast           s                   like(eoast)
     D ##co#           s                   like(eoco#)
     D ##cpt4          s                   like(prcpt4)
     D ##fdat          s                   like(cafdat)
     D ##fein          s                   like(eofein)
     D ##flic          s                   like(eoflic)
     D ##mod           s                   like(eomod)
     D ##mra           s                   like(p2mra)
     D xxmra           s                   like(p2mra)
     D ##pos           s                   like(d9pos)
     D scast           s                   like(eoast)
     D sclic#          s                   like(eoflic)
     D #rulmods        s              2  0
     D CCIred          s              1
     D xxccmd          s              1
     D xxftos          s                   like(p2htos)
     D xxttos          s                   like(p2htos)
     D kkicd9          s                   like(diicd9)
     D k1icd9          s                   like(rhicd9)
     D kkdate          s                   like(rhefdt)
     D xxeval          s              1
     D unitCnt         s              3  0
     D tot_hipps_paid  s                   like(eochrg)
     D xxtot001        s                   like(camra)
     D totChgGA        s                   like(eochrg)
     DindCAPCSI        s               n   Inz(*off)
     DisAPCSI          s              1    Inz
     DstrCAPCSI        s                   Like(FRcapcsi)
     D Match           s              1a
     D NxtRul          s              1a   Inz(*blanks)
     d strSvCd         s              4a   Inz(*zeros)
     D isNonStdCode    s               n   Inz(*off)

     D psds          esds                  extname(szq1p)
     //
     //  Date/Time Data Structure.
     //
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     //
     //  Error message breakdown into state and msg#.
     //
     D                 ds
     D xxmsg                          7
     D  x1msg                         2    overlay(xxmsg)
     D  x2msg                         5    overlay(xxmsg:3)
     //
     //  Service code from rules
     //
     D                 ds
     D xxsvcd                        11
     D  x1svcd                        3    overlay(xxsvcd)
     D  x1cpt4                        5    overlay(xxsvcd:4)
     D  x1unts                        3    overlay(xxsvcd:9)
     //
     //  Service code From detail file
     //
     D                 ds
     D xocode                        11
     D  xosvcd                        3    overlay(xocode)
     D  xocpt4                        5    overlay(xocode:4)
     D  xounts                        3    overlay(xocode:9)
     //
     //  Multiple occurance data structure for FDFRP
     //
     D mdsru           ds                  occurs(9999) inz
     D frst                           2
     D frefdt                         7p 0
     D frexdt                         7p 0
     D frrule                         9p 0
     D frpcat                        10
     D frdesc                        60
     D frpage                         3p 0
     D frinfo                        15
     D frpty                          3p 0
     D frsys                          1
     D frspcd                         3
     D frbfcd                        11
     D frbtcd                        11
     D frbfmd                         2
     D frbtmd                         2
     D frbfps                         3p 0
     D frbtps                         3p 0
     D frbtos                         2
     D frbprt                         2
     D frcfcd                        11
     D frctcd                        11
     D frcmod                         2
     D frcpos                         3p 0
     D frctos                         2
     D frctob                         3
     D frcsdt                         1
     D frchdt                         1
     D frcspr                         1
     D frcdpr                         1
     D frcfdy                         5p 0
     D frcpdy                         5p 0
     D fr#prc                         5p 0
     D fr#day                         5p 0
     D freob                          3
     D frmsg                          7
     D frpmr                          1
     D frpch                          1
     D frdis                          1
     D frpct                          5p 2
     D framt                          9p 2
     D frstat                         1
     D fruser                        10
     D frpgm                         10
     D frtime                         6  0
     D frdt                           7p 0
     D friusr                        10
     D fripgm                        10
     D fritim                         6  0
     D fridt                          7p 0
     D frprof                         1
     D frtech                         1
     D fr#gpc                         1
     D fr#gdy                         1
     D frcpts                         1
     D frbsep                         1
     D frcsep                         1
     D frbrt                          1
     D frbast                         1
     D frcast                         1
     D frpcvl                         1
     D frtcvl                         1
     D frval                          1
     D frpcpt                         5
     D frbspb                         1
     D frunit                         3p 0
     D fraunt                         3p 0
     D frconv                         5p 2
     D frfud                          1
     D frtmdv                         3p 0
     D frct                           1
     D frnct                          1
     D frhtos                         1
     D frmsg2                         7
     D frmsg3                         7
     D frmaj                          1
     D frcon2                         5p 2
     D frftm                          5p 0
     D frttm                          5p 0
     D frmr                           1
     D frefcd                        11
     D fretcd                        11
     D frrnd                          1
     D frhmra                         1
     D frpane                         1
     D frpfac                         1
     D frppdm                         1
     D frcomp                         1
     D fraval                         1
     D frpas2                         1
     D frtane                         1
     D frptyp                         2
     D frpgrp                         5
     D frppgp                         1
     D frpout                         1
     D frcsbl                         1
     D frpdif                         1
     D frloss                         9p 2
     D frcprt                         2
     D frpsrv                         1
     D frtlos                         1
     D frbmri                         1
     D frcmri                         1
     D frpasc                         1
     D frpttm                         5  2
     D frtrmt                         5p 0
     D frovrd                         1
     D frigmd                         1
     D frppsc                         1
     D frpcrf                         1
     D frpsni                         1
     D frpsno                         1
     D frphhi                         1
     D frphho                         1
     D frpdrg                         1
     D frftob                         3
     D frttob                         3
     D frfema                         1
     D frpoma                         1
     D frmema                         1
     D frscma                         1
     D frmrma                         1
     D frdrma                         1
     D frsval                         1
     D frpphi                         1
     D frppho                         1
     D frporc                         1
     D frppor                         1
     D frppir                         1
     D frbfut                         1
     D frbdex                         1
     D frccod                         1
     D frftos                         2
     D frftss                         1
     D frttos                         2
     D frttss                         1
     D frprx                          1
     D frpptv                         1
     D frbmd2                         2
     D frbmd3                         2
     D frbmd4                         2
     D frpare                         1
     D frpcin                         1
     D fr50in                         1
     D fr51in                         1
     D fr62in                         1
     D fr66in                         1
     D fr80in                         1
     D frclt#                         3  0
     D frbsrg                         1
     D frfadt                         7s 0
     D frtadt                         7s 0
     D frmunt                         1
     D frbfhe                         1
     D fr#dsd                         3s 0
     D frapsi                         1
     D frimst                         1
     D fraspi                         2
     D FRrel                          6
     D FRproj                         6
     D FRbInvl                        1
     D frapcsi                        2
     D frcapcsi                       2
     D frbfad                         1
     D frctpc                         1
     D**** Beg Add *** 07/18/23 **************************************
     D freobcd1                       6
     D freobcd2                       6
     D**** End Add *** 07/18/23 **************************************
      //
     //  Multiple occurance data structure for FDCAL9
      //
     D mdsp9           ds                  occurs(9999) inz
     D p9co#                          3p 0
     D p9fnd                          3p 0
     D p9fyr                          3p 0
     D p9case                         7p 0
     D p9cscd                         3
     D p9fdat                         7p 0
     D p9tdat                         7p 0
     D p9rdat                         7p 0
     D p9ipop                         1
     D p9fein                         9p 0
     D p9mod                          3p 0
     D p9prty                         1
     D p9trcd                         2
     D p9svcd                         4p 0
     D p9pmod                         2
     D p9cpt4                         5
     D p9icd9                         5
     D p9anmn                         5p 2
     D p9unts                         5p 2
     D p9totc                         9p 2
     D p9tota                         9p 2
     D p9totd                         9p 2
     D p9eomb                         3
     D p9totp                         9p 2
     D p9dseq                         5p 0
     D p9delf                         1
     D p9flag                         1
     D p9chk#                         7p 0
     D p9eob#                         9p 0
     D p9part                         2
     D p9p1                           2
     D p9ss#                          9
     D p9acdt                         7p 0
     D p9stat                         1
     D p9user                        10
     D p9pgm                         10
     D p9time                         6  0
     D p9dt                           7p 0
     D p9iusr                        10
     D p9idt                          7p 0
     D p9dxr                          1p 0
     D p9dcs#                         5p 0
     D p9ltr#                         9p 0
     D p9rcfl                         1
     D p9sub#                         5p 0
     D p9othr                         9p 2
     D p9ndis                         9p 2
     D p9rdtp                         2
     D p9brsv                         4p 0
     D p9brpm                         2
     D p9brpc                         5
     D p9bram                         5p 2
     D p9brun                         5p 2
     D p9brtc                         9p 2
     D p9zipa                         3
     D p9ast                          2
     D p9diff                         9p 2
     D p9ipgm                        10
     D p9chrg                         9p 2
     D p9mra                          9p 2
     D p9oth2                         9p 2
     D p9paid                         9p 2
     D p9rcn                          3p 0
     D p9lin#                         9p 0
     D p9rule                         9p 0
     D p9ot1c                         3
     D p9ot2c                         3
     D p9mraa                         9p 2
     D p9itim                         6  0
     D p9mnaa                         9p 2
     D p9mod1                         2
     D p9mod2                         2
     D p9mod3                         2
     D p9mod4                         2
     D p9bund                         1
     D p9pos                          2
     D p9cmri                         1
     D p9anes                         1
     D p9ovrd                         1
     D p9ccas                         1
     D p9enba                         5
     D p91ah                          1
     D p91af                          1
     D dbfds           ds
     D dbrrn1                397    400b 0
     D cafds           ds
     D carrn                 397    400b 0
     D kkeob#                         9p 0
     D xxeob#                         9p 0
     //---------------------------------------------------------------------
     // Stand Alone Fields - TOP
     //---------------------------------------------------------------------
     D ##acis          s              1
     D ##ainv          s              1
     D ##arin          s              1
     D ##eobd          s              3  0
     D ##eobi          s              1
     D ##mciv          s              1
     D ##pay0          s              1
     D ##pbiv          s              1
     D ##priv          s              1
     D ##sdts          s              1
     D ##tdup          s              1
     D ##whpg          s              1
     D ##wcif          s              1
     D ##mcif          s              1
     D ##wcin          s              1
     D ##ar            s              1
     D ##ntwk          s              1
     D ##ur            s              1
     D ##med           s              1
     D ##edi           s              1
     D ##wcdk          s              1
     D ##rhab          s              1
     D ##mss           s              1
     D ##fsch          s              1
     D ##prvb          s              1
     D ##npwc          s              1
     D ##os            s              1
     D ##arls          s              1
     D ##coco          s              1
     D ##crvw          s              1
     D ##prom          s              1
     D ##mcrv          s              1
     D ##freq          s              1
     D ##crac          s              1
     D ##preo          s              1
     D ##parn          s              1
     D ##mgri          s              1
     D ##kcif          s              1
     D ##curs          s              1
     D ##pcib          s              1
     D ##clt#          s              3
     D xxclt#          s              3  0
     D amt1            s                   like(p2mra)
     D amt2            s                   like(p2mra)
     D bxpos           s                   like(frbfps)
     D count           s              5  0
     D cxpos           s                   like(frcpos)
     D day1            s              2  0
     D day2            s              1  0
     D eombfl          s              1
     D feesch          s              8
     D                 ds
     D fxbfcd                        11
     D  f1bfrv                        3    overlay(fxbfcd)
     D  f1bfpc                        5    overlay(fxbfcd:4)
     D  f1bfun                        3    overlay(fxbfcd:9)
     D                 ds
     D fxbtcd                        11
     D  f1btrv                        3    overlay(fxbtcd)
     D  f1btpc                        5    overlay(fxbtcd:4)
     D  f1btun                        3    overlay(fxbtcd:9)
     D fxcfcd          s              5
     D fxctcd          s              5
     D hlddat          s              7  0
     D hltotc          s                   like(catotc)
     D hours           s              5  2
     D hours1          s              5  2
     D paid            s              9  2
     D xxpart          s              9  2
     D i               s              5  0
     D j               s              5  0
     D k               s              1  0
     D kkcpt4          s             11
     D kkefdt          s              7  0
     D kkndc           s                   like(dpndc)
     D kkhcpc          s                   like(dphcpc)
     D kkpty           s              3  0
     D kkrate          s                   like(durate)
     D kkrvcd          s                   like(durvcd)
     D kkst            s              2
     D kksys           s              1
     D kkunts          s                   like(duunts)
     D k1st            s                   like(rust)
     D kkatyp          s                   like(ruatyp)
     D kkcod1          s                   like(mucod1)
     D kkcod2          s                   like(mucod1)
     D xxcod1          s                   like(mucod1)
     D xxcod2          s                   like(mucod1)
     D kkcpcd          s                   like(bncpcd)
     D kkprcd          s                   like(d9prcd)
     D kkfdat          s                   like(cafdat)
     D xxcc            s              1
     D xxccdf          s                   like(caccdf)
     D xxccod          s                   like(caccod)
     D xxccdo          s              1
     D @xxccdo         s              1
     D xxst            s                   like(frst)
     D l               s              1  0
     D mgcase          s              7  0
     D mgco#           s              3  0
     D mgcsi           s              1
     D mgdcs#          s              5  0
     D mgdseq          s              5  0
     D mgeob#          s              9  0
     D mgfnd           s              3  0
     D mgfyr           s              3  0
     D mgmsg#          s              7
     D mgsrc           s              1
     D mgsub#          s              5  0
     D modrul          s              1
     D mod1            s              8
     D mod2            s                   like(d9mod1)
     D n               s              4  0
     D nxtflg          s              1
     D n1              s              4  0
     D n2              s              5  0
     D onemod          s              1
     D proc            s              1
     D qqacdt          s              8  0
     D qqc             s             60
     D qqcase          s              7  0
     D qqco#           s              3  0
     D qqdcs#          s              5
     D qqdseq          s                   like(cadseq)
     D qqeob#          s              9  0
     D qqesrc          s                   like(deesrc)
     D qqfdt           s            150
     D qqfdt2          s            150
     D qqfein          s              9  0
     D qqfnd           s              3  0
     D qqfyr           s              3  0
     D qqmod           s              3  0
     D qqrtcd          s              1
     D qqs             s            150
     D qqspcd          s              3
     D qqss#           s              9  0
     D qqst            s              2
     D qqsub#          s              5  0
     D qqdate1         s              8  0
     D qqdate2         s              8  0
     D qqdiff          s              5  0
     D qqerr           s              1
     D remdr           s              5  0
     D rule            s              1
     D seqms           s              2  0
     D setpay          s              1
     D srdate          s              8  0
     D srdte           s              8  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srfut           s              5  0
     D srfutr          s              5  0
     D srlday          s              9
     D srpst           s              5  0
     D srrtcd          s              1
     D srsday          s              4
     D srterm          s              1
     D srtfmt          s              8
     D sydate          s              7  0
     D tabkey          s             10
     D u               s              4  0
     D w               s              5  0
     D wkfdat          s              8  0
     D wktrmt          s              8  0
     D x               s              5  0
     D xcmra           s              9  2
     D xpbrtc          s                   like(catotp)
     D xpdiff          s                   like(cadiff)
     D**** Beg Chg *** 07/18/23 **************************************
     D**** xpeomb          s              3
     D xpeomb          s              6
     D xpeomb2         s              6
     D chrEobCode      s              6
     D**** End Chg *** 07/18/23 **************************************
     D xppmod          s                   like(capmod)
     D xptemp          s             13  2
     D xptemp2         s             13  0
     D xptota          s             11  2
     D xptota1         s             11  2
     D xptota2         s             14  2
     D xptotc          s                   like(catotc)
     D xptotd          s                   like(catotd)
     D xptotp          s                   like(catotp)
     D xpmra           s                   like(catotp)
     D xxcall          s              1
     D xxamt           s                   like(framt)
     D*** Beg Chg *** 11/12/20 ********************************************************
     D***xxanmn          s                   like(p2anmn)
     D xxanmn          s              5s 0
     D*** End Chg *** 11/12/20 ********************************************************
     D xxastr          s              1
     D xxaunt          s                   like(fraunt)
     D xxbund          s              1
     D xxconv          s              9  2
     D xxcpt4          s                   like(cacpt4)
     D xxdseq          s                   like(cadseq)
     D xxerr           s              1
     D xxftm           s                   like(frftm)
     D xxfutr          s              7  0
     D xxhard          s              1
     D xxhist          s              1
     D xxloss          s             11  2
     D xxman           s              1
     D xxmmod          s                   like(frmr)
     D xxmseq          s                   like(mgmseq)
     D xxnot1          s                   like(denot1)
     D xxnot2          s                   like(denot2)
     D xxnot3          s                   like(denot3)
     D xxnot4          s                   like(denot4)
     D xxount          s              5  0
     D xxoutl          s                   like(p2mra)
     D xxovrd          s                   like(csovrd)
     D xxovrp          s              5  4
     D xxpass          s              2  0
     D xxpast          s              8  0
     D xxpas2          s              1
     D xxperf          s              1
     D xxpmod          s                   like(capmod)
     D xxprdm          s                   like(p2mra)
     D xxpst7          s                   like(cafdat)
     D xxrcn           s                   like(carcn)
     D xxrudt          s                   like(p2rudt)
     D xxrule          s                   like(frrule)
     D xxsday          s              4
     D xxsepp          s                   like(p2sepp)
     D xxtmdv          s                   like(frtmdv)
     D xxtrmt          s                   like(cafdat)
     D xxunit          s                   like(p2unts)
     D xxunt           s              5  0
     D xxunts          s                   like(p2unts)
     D x2unts          s                   like(p2unts)
     D xxmsurg         s              1
     D xxymd           s                   like(cafdat)
     D xxseq#          s                   like(deseq#)
     D xxscpf          s              8  0
     D xxscpt          s              8  0
     D x1ast           s              1
     D x1conv          s                   like(frconv)
     D x1loss          s             11  2
     D x1pos           s                   like(p9pos)
     D x1rem           s              6  5
     D x1rule          s                   like(frrule)
     D x1sep           s              1
     D x1trmt          s                   like(cafdat)
     D x2rem           s              9  2
     D z1loss          s             11  2
     D x2pos           s                   like(p9pos)
     D y               s              5  0
     D z               s              2  0
     D zzloss          s             11  2
     D x3rem           s              1  1
     D xxtop           s              1
     D kknwcd          s                   like(eontwk)
     D x3unts          s                   like(p2unts)
     D x9mraa          s                   like(p9mraa)
     D x9totc          s                   like(p9totc)
     D zzunits         s                   like(p2unts)
     Dxptota_for_unit  s                   like(xptota)
     Dxptota3          s                   like(p2amt)
     Dxptota4          s             11  2
     Dxxdenyunits      s              1a
     D isRulClr        s               n   inz(*off)
     D IndPriCdFnd     s               n   inz(*off)
     //---------------------------------------------------------------------
     // Stand Alone Fields - BOTTOM
     //---------------------------------------------------------------------
     //
     //  Rename the duplicate field names.
      //
     Ipdh02
     I              dhco#                       d9co#
     I              dhsub#                      d9sub#
     I              dhfnd                       d9fnd
     I              dhfyr                       d9fyr
     I              dhcase                      d9case
     I              dhss#                       d9ss#
     I              dhacdt                      d9acdt
     I              dheob#                      d9eob#
     I              dhdseq                      d9dseq
     I              dhpos                       d9pos
     I              dhtos                       d9tos
     I              dhprcd                      d9prcd
     I              dhprds                      d9prds
     I              dhmod1                      d9mod1
     I              dhmod2                      d9mod2
     I              dhmod3                      d9mod3
     I              dhmod4                      d9mod4
     I              dhdxr1                      d9dxr1
     I              dhdxr2                      d9dxr2
     I              dhdxr3                      d9dxr3
     I              dhdxr4                      d9dxr4
     I              dhunts                      d9unts
     I              dharea                      d9area
     I              dhppln                      d9ppln
     I              dhstat                      d9stat
     I              dhuser                      d9user
     I              dhpgm                       d9pgm
     I              dhtime                      d9time
     I              dhdt                        d9dt
     I              dhiusr                      d9iusr
     I              dhipgm                      d9ipgm
     I              dhitim                      d9itim
     I              dhidt                       d9idt
     I              dhrcfl                      d9rcfl
     // Record Format for Workfile FACAP
     Ipcawk1
     I              caco#                       p2co#
     I              casub#                      p2sub#
     I              cafnd                       p2fnd
     I              cafyr                       p2fyr
     I              cacase                      p2case
     I              cacscd                      p2cscd
     I              cafdat                      p2fdat
     I              catdat                      p2tdat
     I              cardat                      p2rdat
     I              caipop                      p2ipop
     I              cafein                      p2fein
     I              camod                       p2mod
     I              caprty                      p2prty
     I              caprt                       p2prt
     I              catrcd                      p2trcd
     I              casvcd                      p2svcd
     I              capmod                      p2pmod
     I              cacpt4                      p2cpt4
     I              caicd9                      p2icd9
     I              caanmn                      p2anmn
     I              caanes                      p2anes
     I              caunts                      p2unts
     I              catotc                      p2totc
     I              catota                      p2tota
     I              catotd                      p2totd
     I              caeomb                      p2eomb
     I              catotp                      p2totp
     I              cadseq                      p2dseq
     I              cadelf                      p2delf
     I              caflag                      p2flag
     I              cachk#                      p2chk#
     I              caeob#                      p2eob#
     I              capart                      p2part
     I              cap1                        p2p1
     I              cass#                       p2ss#
     I              caacdt                      p2acdt
     I              castat                      p2stat
     I              causer                      p2user
     I              capgm                       p2pgm
     I              catime                      p2time
     I              cadt                        p2dt
     I              caiusr                      p2iusr
     I              caidt                       p2idt
     I              cadxr                       p2dxr
     I              cadcs#                      p2dcs#
     I              caltr#                      p2ltr#
     I              carcfl                      p2rcfl
     I              caothr                      p2othr
     I              candis                      p2ndis
     I              cardtp                      p2rdtp
     I              cabrsv                      p2brsv
     I              cabrpm                      p2brpm
     I              cabrpc                      p2brpc
     I              cabram                      p2bram
     I              cabrun                      p2brun
     I              cabrtc                      p2brtc
     I              cazipa                      p2zipa
     I              caast                       p2ast
     I              cadiff                      p2diff
     I              cabrpt                      p2brpt
     I              cabast                      p2bast
     I              cadafl                      p2dafl
     I              camra                       p2mra
     I              cabrec                      p2brec
     I              cabrp                       p2brp
     I              catype                      p2type
     I              carpt                       p2rpt
     I              cafsr                       p2fsr
     I              cafacf                      p2facf
     I              caplac                      p2plac
     I              candc                       p2ndc
     I              camod1                      p2mod1
     I              camod2                      p2mod2
     I              camod3                      p2mod3
     I              camod4                      p2mod4
     I              captyp                      p2ptyp
     I              capgrp                      p2pgrp
     I              caltrm                      p2ltrm
     I              cahtrm                      p2htrm
     I              calsty                      p2lsty
     I              cabund                      p2bund
     I              cabmri                      p2bmri
     I              cabdex                      p2bdex
     I              cascam                      p2scam
     I              cadrg                       p2drg
     I              casnfi                      p2snfi
     I              casnfo                      p2snfo
     I              cafema                      p2fema
     I              capoma                      p2poma
     I              camema                      p2mema
     I              cascma                      p2scma
     I              camrma                      p2mrma
     I              cadrma                      p2drma
     I              casval                      p2sval
     I              caphop                      p2phop
     I              caphip                      p2phip
     I              cacorf                      p2corf
     I              cahhli                      p2hhli
     I              cahhlo                      p2hhlo
     I              caorcc                      p2orcc
     I              caoprc                      p2porc
     I              caiprc                      p2pirc
     I              caoccr                      p2occr
     I              cabcas                      p2bcas
     //
     //   These fields are from the CPT4 file or the reasonable and
     //   customary file
     //
     I              cainvl                      p2invl
     I              capos                       p2pos
     I              casepp                      p2sepp
     I              caspcd                      p2spcd
     I              caamt                       p2amt
     I              caprof                      p2prof
     I              catech                      p2tech
     I              caanam                      p2anam
     I              cafolu                      p2folu
     I              casurg                      p2surg
     I              casupp                      p2supp
     I              caasc                       p2asc
     I              camd50                      p2md50
     I              camd51                      p2md51
     I              camd54                      p2md54
     I              camd55                      p2md55
     I              camd56                      p2md56
     I              camd62                      p2md62
     I              camd66                      p2md66
     I              camd80                      p2md80
     I              camd81                      p2md81
     I              camd82                      p2md82
     I              cahtos                      p2htos
     I              canct                       p2nct
     I              carudt                      p2rudt
     I              caawp                       p2awp
     I              captpv                      p2ptpv
     I              caarea                      p2area
     I              capcin                      p2pcin
     I              ca50in                      p250in
     I              ca51in                      p251in
     I              ca62in                      p262in
     I              ca66in                      p266in
     I              ca80in                      p280in
     I              caenba                      p2enba
     I              cabfhe                      p2bfhe
     I              caconv                      p2conv
     I              cabmin                      p2bmin
     I              caslos                      p2slos
     I              cappt1                      p2ppt1
     I              cappt2                      p2ppt2
     I              cappt3                      p2ppt3
     I              cappt4                      p2ppt4
     I              cappt5                      p2ppt5
     I              cappt6                      p2ppt6
     I              calq51                      p2lq51
     I              caapsi                      p2apsi
     I              caimst                      p2imst
     I              caaspi                      p2aspi
     I              catos                       p2tos
     I              caapcsi                     p2apcsi
     I              capaymra                    p2paymra
     I              caaddon                     p2addon
      //
     // Record Format for Workfile FACAL
      //
     Ipcawk2
     I              caco#                       p3co#
     I              casub#                      p3sub#
     I              cafnd                       p3fnd
     I              cafyr                       p3fyr
     I              cacase                      p3case
     I              cacscd                      p3cscd
     I              cafdat                      p3fdat
     I              catdat                      p3tdat
     I              cardat                      p3rdat
     I              caipop                      p3ipop
     I              cafein                      p3fein
     I              camod                       p3mod
     I              caprty                      p3prty
     I              caprt                       p3prt
     I              catrcd                      p3trcd
     I              casvcd                      p3svcd
     I              capmod                      p3pmod
     I              cacpt4                      p3cpt4
     I              caicd9                      p3icd9
     I              caanmn                      p3anmn
     I              caanes                      p3anes
     I              caunts                      p3unts
     I              catotc                      p3totc
     I              catota                      p3tota
     I              catotd                      p3totd
     I              caeomb                      p3eomb
     I              catotp                      p3totp
     I              cadseq                      p3dseq
     I              cadelf                      p3delf
     I              caflag                      p3flag
     I              cachk#                      p3chk#
     I              caeob#                      p3eob#
     I              capart                      p3part
     I              cap1                        p3p1
     I              cass#                       p3ss#
     I              caacdt                      p3acdt
     I              castat                      p3stat
     I              causer                      p3user
     I              capgm                       p3pgm
     I              catime                      p3time
     I              cadt                        p3dt
     I              caiusr                      p3iusr
     I              caidt                       p3idt
     I              cadxr                       p3dxr
     I              cadcs#                      p3dcs#
     I              caltr#                      p3ltr#
     I              carcfl                      p3rcfl
     I              caothr                      p3othr
     I              candis                      p3ndis
     I              cardtp                      p3rdtp
     I              cabrsv                      p3brsv
     I              cabrpm                      p3brpm
     I              cabrpc                      p3brpc
     I              cabram                      p3bram
     I              cabrun                      p3brun
     I              cabrtc                      p3brtc
     I              cazipa                      p3zipa
     I              caast                       p3ast
     I              cadiff                      p3diff
     I              cabrpt                      p3brpt
     I              cabast                      p3bast
     I              cadafl                      p3dafl
     I              camra                       p3mra
     I              cabrec                      p3brec
     I              cabrp                       p3brp
     I              catype                      p3type
     I              carpt                       p3rpt
     I              cafsr                       p3fsr
     I              cafacf                      p3facf
     I              caplac                      p3plac
     I              candc                       p3ndc
     I              camod1                      p3mod1
     I              camod2                      p3mod2
     I              camod3                      p3mod3
     I              camod4                      p3mod4
     I              cabdex                      p3bdex
     I              cainvl                      p3invl
     I              capos                       p3pos
     I              casepp                      p3sepp
     I              caspcd                      p3spcd
     I              caamt                       p3amt
     I              caprof                      p3prof
     I              catech                      p3tech
     I              caanam                      p3anam
     I              cafolu                      p3folu
     I              casurg                      p3surg
     I              casupp                      p3supp
     I              caasc                       p3asc
     I              camd50                      p3md50
     I              camd51                      p3md51
     I              camd54                      p3md54
     I              camd55                      p3md55
     I              camd56                      p3md56
     I              camd62                      p3md62
     I              camd66                      p3md66
     I              camd80                      p3md80
     I              camd81                      p3md81
     I              camd82                      p3md82
     I              cahtos                      p3htos
     I              canct                       p3nct
     I              carudt                      p3rudt
     I              capgrp                      p3pgrp
     I              caltrm                      p3ltrm
     I              cahtrm                      p3htrm
     I              calsty                      p3lsty
     I              cascam                      p3scam
     I              cadrg                       p3drg
     I              casnfi                      p3snfi
     I              casnfo                      p3snfo
     I              caphip                      p3phip
     I              caphop                      p3phop
     I              cacorf                      p3corf
     I              caorcc                      p3orcc
     I              caoprc                      p3porc
     I              caiprc                      p3pirc
     I              caoccr                      p3occr
     I              cabcas                      p3bcas
     I              caawp                       p3awp
     I              captpv                      p3ptpv
     I              caarea                      p3area
     I              capcin                      p3pcin
     I              ca50in                      p350in
     I              ca51in                      p351in
     I              ca62in                      p362in
     I              ca66in                      p366in
     I              ca80in                      p380in
     I              caenba                      p3enba
     I              cabfhe                      p3bfhe
     I              caconv                      p3conv
     I              cabmin                      p3bmin
     I              caslos                      p3slos
     I              cappt1                      p3ppt1
     I              cappt2                      p3ppt2
     I              cappt3                      p3ppt3
     I              cappt4                      p3ppt4
     I              cappt5                      p3ppt5
     I              cappt6                      p3ppt6
     I              calq51                      p3lq51
     I              caapsi                      p3apsi
     I              caimst                      p3imst
     I              caaspi                      p3aspi
     I              caapcsi                     p3apcsi
     I              caaddon                     p3addon
     //
     // Record Format for PCA09 COMPARE
     //
     Ipca09
     I              caco#                       p9co#
     I              cafnd                       p9fnd
     I              cafyr                       p9fyr
     I              cacase                      p9case
     I              cacscd                      p9cscd
     I              cafdat                      p9fdat
     I              catdat                      p9tdat
     I              cardat                      p9rdat
     I              caipop                      p9ipop
     I              cafein                      p9fein
     I              camod                       p9mod
     I              caprty                      p9prty
     I              catrcd                      p9trcd
     I              casvcd                      p9svcd
     I              capmod                      p9pmod
     I              cacpt4                      p9cpt4
     I              caicd9                      p9icd9
     I              catota                      p9tota
     I              caeomb                      p9eomb
     I              cadseq                      p9dseq
     I              cadelf                      p9delf
     I              caflag                      p9flag
     I              cachk#                      p9chk#
     I              caeob#                      p9eob#
     I              capart                      p9part
     I              cap1                        p9p1
     I              cass#                       p9ss#
     I              caacdt                      p9acdt
     I              castat                      p9stat
     I              causer                      p9user
     I              capgm                       p9pgm
     I              catime                      p9time
     I              cadt                        p9dt
     I              caiusr                      p9iusr
     I              caidt                       p9idt
     I              cadxr                       p9dxr
     I              cadcs#                      p9dcs#
     I              caltr#                      p9ltr#
     I              carcfl                      p9rcfl
     I              casub#                      p9sub#
     I              caothr                      p9othr
     I              candis                      p9ndis
     I              cardtp                      p9rdtp
     I              cabrsv                      p9brsv
     I              cabrpm                      p9brpm
     I              cabrpc                      p9brpc
     I              cabram                      p9bram
     I              cabrun                      p9brun
     I              cabrtc                      p9brtc
     I              cazipa                      p9zipa
     I              caast                       p9ast
     I              cadiff                      p9diff
     I              caipgm                      p9ipgm
     I              cachrg                      p9totc
     I              camra                       p9totd
     I              caoth2                      p9oth2
     I              capaid                      p9totp
     I              carcn                       p9rcn
     I              calin#                      p9lin#
     I              carule                      p9rule
     I              caot1c                      p9ot1c
     I              caot2c                      p9ot2c
     I              camraa                      p9mraa
     I              caitim                      p9itim
     I              camnaa                      p9mnaa
     //
     //  Rename the duplicate field names.
     //
     Ipca0o
     I              caco#                       oxco#
     I              casub#                      oxsub#
     I              cafnd                       oxfnd
     I              cafyr                       oxfyr
     I              cacase                      oxcase
     I              cafdat                      oxfdat
     I              cadseq                      oxdseq
     I              cafein                      oxfein
     I              camod                       oxmod
     I              casvcd                      oxsvcd
     I              cacpt4                      oxcpt4
     I              caunts                      oxunts
     I              cachrg                      oxtotc
     I              camra                       oxtotd
     I              capaid                      oxtotp
     I              capmod                      oxpmod
     I              causer                      oxuser
     I              capgm                       oxpgm
     I              cadt                        oxdt
     I              catime                      oxtime
     I              cachk#                      oxchk#
     I              caeob#                      oxeob#
     I              cadxr                       oxdxr
     I              caflag                      oxflag
     //
     Ipca0p
     I              caco#                       pxco#
     I              casub#                      pxsub#
     I              cafnd                       pxfnd
     I              cafyr                       pxfyr
     I              cacase                      pxcase
     I              cafdat                      pxfdat
     I              cadseq                      pxdseq
     I              cafein                      pxfein
     I              camod                       pxmod
     I              cacpt4                      pxcpt4
     I              capmod                      pxpmod
     I              causer                      pxuser
     I              capgm                       pxpgm
     I              catime                      pxtime
     I              cadt                        pxdt
     I              cachk#                      pxchk#
     I              caeob#                      pxeob#
     I              capart                      pxpart
     I              cadxr                       pxdxr
     I              caflag                      pxflag
     I              cadiff                      pxdiff
     I              carcfl                      pxrcfl
     I              cachrg                      pxtotc
     I              camra                       pxtotd
     I              capaid                      pxtotp
     Ipca10
     I              caco#                       pzco#
     I              cafnd                       pzfnd
     I              cafyr                       pzfyr
     I              cacase                      pzcase
     I              cacscd                      pzcscd
     I              cafdat                      pzfdat
     I              catdat                      pztdat
     I              cardat                      pzrdat
     I              caipop                      pzipop
     I              cafein                      pzfein
     I              camod                       pzmod
     I              caprty                      pzprty
     I              catrcd                      pztrcd
     I              casvcd                      pzsvcd
     I              capmod                      pzpmod
     I              cacpt4                      pzcpt4
     I              caicd9                      pzicd9
     I              catota                      pztota
     I              caeomb                      pzeomb
     I              cadseq                      pzdseq
     I              cadelf                      pzdelf
     I              caflag                      pzflag
     I              cachk#                      pzchk#
     I              caeob#                      pzeob#
     I              capart                      pzpart
     I              cap1                        pzp1
     I              cass#                       pzss#
     I              caacdt                      pzacdt
     I              castat                      pzstat
     I              causer                      pzuser
     I              capgm                       pzpgm
     I              catime                      pztime
     I              cadt                        pzdt
     I              caiusr                      pziusr
     I              caidt                       pzidt
     I              cadxr                       pzdxr
     I              cadcs#                      pzdcs#
     I              caltr#                      pzltr#
     I              carcfl                      pzrcfl
     I              casub#                      pzsub#
     I              caothr                      pzothr
     I              candis                      pzndis
     I              cardtp                      pzrdtp
     I              cabrsv                      pzbrsv
     I              cabrpm                      pzbrpm
     I              cabrpc                      pzbrpc
     I              cabram                      pzbram
     I              cabrun                      pzbrun
     I              cabrtc                      pzbrtc
     I              cazipa                      pzzipa
     I              caast                       pzast
     I              cadiff                      pzdiff
     I              caipgm                      pzipgm
     I              cachrg                      pztotc
     I              camra                       pztotd
     I              caoth2                      pzoth2
     I              capaid                      pztotp
     I              carcn                       pzrcn
     I              calin#                      pzlin#
     I              carule                      pzrule
     I              caot1c                      pzot1c
     I              caot2c                      pzot2c
     I              camraa                      pzmraa
     I              caitim                      pzitim
     I              camnaa                      pzmnaa
     Ipca0s
     I              caco#                       psco#
     I              cafnd                       psfnd
     I              cafyr                       psfyr
     I              cacase                      pscase
     I              cacscd                      pscscd
     I              cafdat                      psfdat
     I              catdat                      pstdat
     I              cardat                      psrdat
     I              caipop                      psipop
     I              cafein                      psfein
     I              camod                       psmod
     I              caprty                      psprty
     I              catrcd                      pstrcd
     I              casvcd                      pssvcd
     I              capmod                      pspmod
     I              cacpt4                      pscpt4
     I              caicd9                      psicd9
     I              catota                      pstota
     I              caeomb                      pseomb
     I              cadseq                      psdseq
     I              cadelf                      psdelf
     I              caflag                      psflag
     I              cachk#                      pschk#
     I              caeob#                      pseob#
     I              capart                      pspart
     I              cap1                        psp1
     I              cass#                       psss#
     I              caacdt                      psacdt
     I              castat                      psstat
     I              causer                      psuser
     I              capgm                       pspgm
     I              catime                      pstime
     I              cadt                        psdt
     I              caiusr                      psiusr
     I              caidt                       psidt
     I              cadxr                       psdxr
     I              cadcs#                      psdcs#
     I              caltr#                      psltr#
     I              carcfl                      psrcfl
     I              casub#                      pssub#
     I              caothr                      psothr
     I              candis                      psndis
     I              cardtp                      psrdtp
     I              cabrsv                      psbrsv
     I              cabrpm                      psbrpm
     I              cabrpc                      psbrpc
     I              cabram                      psbram
     I              cabrun                      psbrun
     I              cabrtc                      psbrtc
     I              cazipa                      pszipa
     I              caast                       psast
     I              cadiff                      psdiff
     I              caipgm                      psipgm
     I              cachrg                      pstotc
     I              camra                       pstotd
     I              caoth2                      psoth2
     I              capaid                      pstotp
     I              carcn                       psrcn
     I              calin#                      pslin#
     I              carule                      psrule
     I              caot1c                      psot1c
     I              caot2c                      psot2c
     I              camraa                      psmraa
     I              caitim                      psitim
     I              camnaa                      psmnaa
     //
     Ipdp02
     I              dpco#                       d1co#
     I              dpsub#                      d1sub
     I              dpfnd                       d1fnd
     I              dpfyr                       d1fyr
     I              dpcase                      d1case
     I              dpss#                       d1ss#
     I              dpacdt                      d1acdt
     I              dpeob#                      d1eob#
     I              dpdseq                      d1dseq
     I              dprx#                       d1rx#
     I              dpndc                       d1ndc
     I              dprxmq                      d1rxmq
     I              dprxmm                      d1rxmm
     I              dprxdy                      d1rxdy
     I              dpstat                      d1stat
     I              dpuser                      d1user
     I              dppgm                       d1pgm
     I              dptime                      d1time
     I              dpdt                        d1dt
     I              dpiusr                      d1iusr
     I              dpipgm                      d1pgm
     I              dpitim                      d1itim
     I              dpidt                       d1idt
     I              dpdesc                      d1desc
     I              dprxdi                      d1rxdi
     I              dprxgb                      d1rxgb
     I              dprxrf                      d1rxrf
     I              dprxfd                      d1rxfd
     I              dpunts                      d1unts
     I              dphcpc                      d1hcpc
     Ipdp04
     I              dpco#                       d4co#
     I              dpsub#                      d4sub
     I              dpfnd                       d4fnd
     I              dpfyr                       d4fyr
     I              dpcase                      d4case
     I              dpss#                       d4ss#
     I              dpacdt                      d4acdt
     I              dpeob#                      d4eob#
     I              dpdseq                      d4dseq
     I              dprx#                       d4rx#
     I              dpndc                       d4ndc
     I              dprxmq                      d4rxmq
     I              dprxmm                      d4rxmm
     I              dprxdy                      d4rxdy
     I              dpstat                      d4stat
     I              dpuser                      d4user
     I              dppgm                       d4pgm
     I              dptime                      d4time
     I              dpdt                        d4dt
     I              dpiusr                      d4iusr
     I              dpipgm                      d4pgm
     I              dpitim                      d4itim
     I              dpidt                       d4idt
     I              dpdesc                      d4desc
     I              dprxdi                      d4rxdi
     I              dprxgb                      d4rxgb
     I              dprxrf                      d4rxrf
     I              dprxfd                      d4rxfd
     I              dpunts                      d4unts
     I              dphcpc                      d4hcpc
     //
     //  State Rules File
     //
     C     frky          klist
     C                   kfld                    kkst
     C                   kfld                    kksys
     C                   kfld                    kkefdt
     C                   kfld                    kkpty
     //
     C     frky1         klist
     C                   kfld                    kkst
     C                   kfld                    kksys
     C                   kfld                    kkefdt
     //
     C     frky2         klist
     C                   kfld                    kkst
     C                   kfld                    kksys
     C     frky3         klist
     C                   kfld                    kkst
     C                   kfld                    xxrule
     C                   kfld                    kkefdt
     //
     //  Claim Status Code Table File
     //
     C     tcsky         klist
     C                   kfld                    qqco#
     C                   kfld                    tabkey
     //
     //  Detail Charges Table File
     //
     C     caky9a        klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    p2fdat
     C                   kfld                    p2fein
     C                   kfld                    p2mod
     //
     C     cakye         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    p2fdat
     //
     C     cakyo1        klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    xxymd
     //
     C     cakyo2        klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     //
     C     cakyo3        klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     //
     C     caky1b        klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    p3dseq
     //
     C     caky9p        klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    p2fdat
     C                   kfld                    p2fein
     //
     C     caky1a        klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    p2dseq
     //
     C     cakye1        klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     //
     C     caky9         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    xxdseq
     //
     C     cakyf         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    xxdseq
     C     caky15        klist
     C                   kfld                    p2fein
     C                   kfld                    p2ss#
     C                   kfld                    p2fdat
     //
     C     cakys         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqfein
     C                   kfld                    qqmod
     C                   kfld                    fxcfcd
     //
     C     cakys1        klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqfein
     C                   kfld                    qqmod
     //
     C     cakyp         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqfein
     C                   kfld                    qqmod
     C                   kfld                    kkfdat
     //
     // Detail charge logical for finding 1st day of treatment.
     //
     C     caky23        klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
      //
     // Key List for Bill Entry Error Message File
      //
     C     mgky          klist
     C                   kfld                    qqco#
     C                   kfld                    p2sub#
     C                   kfld                    p2fnd
     C                   kfld                    p2fyr
     C                   kfld                    p2case
     C                   kfld                    p2eob#
     C                   kfld                    xxrcn
     C                   kfld                    p2dseq
     //
     C     mgky1         klist
     C                   kfld                    qqco#
     C                   kfld                    p2sub#
     C                   kfld                    p2fnd
     C                   kfld                    p2fyr
     C                   kfld                    p2case
     C                   kfld                    p2eob#
     C                   kfld                    xxrcn
     C                   kfld                    p2dseq
     C                   kfld                    xxmseq
     //
     C     mgky2         klist
     C                   kfld                    qqco#
     C                   kfld                    p2sub#
     C                   kfld                    p2fnd
     C                   kfld                    p2fyr
     C                   kfld                    p2case
     C                   kfld                    p2eob#
     C                   kfld                    p2dseq
     C                   kfld                    xxmsg
     C                   kfld                    xxrcn
     //
     C     miky          klist
     C                   kfld                    kkmsg#
     C                   kfld                    kkmseq
     //
     //  Claim Master File
     //
     C     cmky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     //
     //  Procedure Code Key
     //
     C     prky          klist
     C                   kfld                    qqco#
     C                   kfld                    frpcpt
     C                   kfld                    p2fdat
     //
     C     prky4         klist
     C                   kfld                    qqco#
     C                   kfld                    kkprcd
     C                   kfld                    kkfdat
     //
     C     prky5         klist
     C                   kfld                    qqco#
     C                   kfld                    kkprcd
     //
     C     prky1         klist
     C                   kfld                    qqco#
     C                   kfld                    p9cpt4
     C                   kfld                    p9fdat
     //
     C     prky2         klist
     C                   kfld                    qqco#
     C                   kfld                    p9cpt4
     C                   kfld                    p2fdat
     //
     C     prky3         klist
     C                   kfld                    qqco#
     C                   kfld                    p9cpt4
     C                   kfld                    p9fdat
     //
     //  Reasonable and Customary Key
     //
     C     rcky          klist
     C                   kfld                    qqco#
     C                   kfld                    p2zipa
     C                   kfld                    frpcpt
     C                   kfld                    p2fdat
     //
     C     rcky1         klist
     C                   kfld                    qqco#
     C                   kfld                    p9zipa
     C                   kfld                    p9cpt4
     C                   kfld                    p9fdat
     //
     C     rcky2         klist
     C                   kfld                    qqco#
     C                   kfld                    p2zipa
     C                   kfld                    p9cpt4
     C                   kfld                    p2fdat
     //
     C     rcky3         klist
     C                   kfld                    qqco#
     C                   kfld                    p9zipa
     C                   kfld                    p9cpt4
     C                   kfld                    p9fdat
     //
     //  User Parameter File
     //
     C     upky          klist
     C                   kfld                    q1user
     //
     //  EOB Code/Note File
     //
     C     deky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    qqdseq
     C                   kfld                    qqesrc
     //
     C     deky1         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    qqdseq
     //
     C     ebky          klist
     C                   kfld                    qqco#
     C                   kfld                    ebkey
     //
     // EOB Transaction File
     //
     C     eoky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     //
     C     eoky1         klist
     C                   kfld                    p9co#
     C                   kfld                    p9sub#
     C                   kfld                    p9fnd
     C                   kfld                    p9fyr
     C                   kfld                    p9case
     C                   kfld                    p9eob#
     //
     // Hospital Detail File
     //
     C     duky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    xxdseq
     //
     C     duky1         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    kkrvcd
     C                   kfld                    kkrate
     C                   kfld                    kkunts
     //
     C     duky2         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     //
     // Physician Detail File
     //
     C     dhky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    xxdseq
     //
     C     dhky1         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    xxcpt4
     //
     C     dhky2         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     //
     // Pharmacy Detail File
     //
     C     dpky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    xxdseq
     //
     C     dpky1         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    kkndc
     //
     C     dpky2         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    kkhcpc
     //
     // Dental Detail File
     //
     C     ddky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    xxdseq
     //
     C     ddky1         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     //
     // Hospital Header File
     //
     C     euky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     //
     // Correct Coding Files
     //
     C     bnky          klist
     C                   kfld                    kkcod1
     C                   kfld                    kkcod2
     C                   kfld                    kkefdt
     //
     C     bnky1         klist
     C                   kfld                    kkcod1
     C                   kfld                    kkcod2
     //
     C     muky          klist
     C                   kfld                    kkcod1
     C                   kfld                    kkcod2
     C                   kfld                    kkefdt
     //
     C     muky1         klist
     C                   kfld                    kkcod1
     C                   kfld                    kkcod2
     //
     // Adjudication Control File
     //
     C     ruky          klist
     C                   kfld                    kkst
     C                   kfld                    kkatyp
     //
     // Network Contract File
     //
     C     s4ky          klist
     C                   kfld                    qqfein
     C                   kfld                    qqmod
     C                   kfld                    kknwcd
     C                   kfld                    kkfdat
     //
     C     s4ky1         klist
     C                   kfld                    qqfein
     C                   kfld                    qqmod
     C                   kfld                    kknwcd
     //
     // FAEHL Physician Header Key
     //
     C     ehkey         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     //
     // FAEDL Dental Header Key
     //
     C     edkey         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     //
     // FAEPL Pharmacy Header Key
     //
     C     epkey         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     //
     C     Key1          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
      //
     C     Key2          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    kkeob#
      //
     C     Key3          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    xxeob#
     C     rhky          klist
     C                   kfld                    k1icd9
     C                   kfld                    kkdate
     //
     // Illinois Per Diem file
     //
     C     rhky1         klist
     C                   kfld                    k1icd9
     //
     // Diagnosis Code file
     //
     C     diky          klist
     C                   kfld                    kkicd9
     //
     // Group file
      //
     C     fnky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     //
     // LA Provider Sales Parrish Tax file
      //
     C     p3ky          klist
     C                   kfld                    eofein
     C                   kfld                    eomod
     C                   kfld                    eofrdt
      //
     C     p3ky1         klist
     C                   kfld                    eofein
     C                   kfld                    eomod
      //
     // Provider Auxiliary file
      //
     C     p1ky1         klist
     C                   kfld                    qqfein
     C                   kfld                    qqmod
     //
     C     *entry        plist
     C                   parm                    qqfyr
     C                   parm                    qqcase
     C                   parm                    qqfdt
     C                   parm                    qqfdt2
     C                   parm                    qqs
     C                   parm                    qqc
     C                   parm                    qqrtcd
     C                   parm                    qqss#
     C                   parm                    qqacdt
     C                   parm                    qqspcd
     C                   parm                    qqfein
     C                   parm                    qqmod
     C                   parm                    qqdcs#
     C                   parm                    qqst
     C                   parm                    qqeob#
     C                   parm                    qqco#
     C                   parm                    qqsub#
     C                   parm                    qqfnd
     //
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     //
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     //
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      /Free
      //===============================================================
      // Mainline
      //===============================================================
      //
       // If called with P as the return code leave the program.
      //
       If qqRtCd = 'P';
          Exsr PgmEnd;
       Endif;
      //
       // Get system controls, initialize fields, read header records
      //
       Exsr PgmStr;
      //
      //  Call subroutine LOAD to load all line items to workfile
      //  in order of highest MRA
      //
       exsr load;
      //
      // Load case history records into multiple occur. data structure
      //
       exsr loadh;

      //===============================================================
      // PASS 1- Read through FACAP in Descending MRA order
      //===============================================================
      //
       open facap;
      //
      // Read the line item from the workfile
      //
       read pcawk1;
       dow not %Eof(facap);
         //
         //  Clear indicators that were set in the Rules subroutine
         //
          clear *in;
          clear xxrule;

          zzloss = zzloss + p2totc;
          P2bAst = P2supp;
         //
         // Initialize Correct Coding fields.
         //
          xxccdo = 'N';
          @xxccdo = 'N';

          xpmra = *zeros;
          xxst = *blanks;
         //
         // Load adjusted MRA into allowed amount work field
         //
          xptota = p2amt;
          xcmra = p2mra;
          kkefdt = p2rudt;
         //
         // Initialize allowed units for paying multiple units
         //
          unitcnt = p2unts;
         //
         // Determine if paying for multiple units or not
         //
          Exsr MultLine;
          //
         // If claim status code is override type skip rules error
         //
          clear tabkey;
          clear csovrd;
          tabKey = p2cscd;
          chain tcsky pcs01;
         //
         // If rule date = 999999 then give message and go to next item
         //
          Exsr NoRuleDate;
          If xxErr = 'Y';
          //*** Beg Add *** 05/23/23 ***************************************
             read pcawk1;
          //*** End Add *** 05/23/23 ***************************************
             Iter;
          Endif;
         //
         // If rule date has changed load new set of rules
         //
          if p2rudt <> xxrudt;
             exsr loadr;
             xxrudt = p2rudt;
          endif;
          clear mod;
          clear mod1;
          clear onemod;
         //
         // Load modifiers into array for processing in PMOD subroutine
         //
          Exsr LoadMod;
         //
         // Call Calc Subroutine based on bill type
         //
          select;
          //
          // Use CALC sub for Physician, Dental and Hospital
          //
          when p2type = 'P'
               or p2type = 'D'
               or p2type = 'H';
               exsr calc;
            //
            // Use PHAR sub for Pharmacy bills
            //
          when p2type = 'X';
             exsr phar;
          //
          // Default to CALC
          //
          other;
             exsr calc;
          endsl;
    
          read pcawk1;

       enddo;
      //===============================================================
      // PASS 2- Read Medicine codes from FACAL in ascending date order
      //===============================================================
       if xxpas2 = *on;
          open facal;
         //
         //  Clear total paid so rules will work properly
         //
          exsr clrpay;
         //
         // Load case history records into multiple occur. data structure
         //
          exsr loadh;
         //
         //  Clear file buffers
         //
          clear pca09;
          clear pmg01;
         //
          setll *loval pcawk2;
          read pcawk2;
    
          dow not %Eof(facal);
           //
           //  Move fields from PCAWK2 to PCAWK1 for processing
           //
            exsr loadf;
           //
           //  Clear adjudication messages/EOMB codes for this line item
           //
            exsr clrmsg;
           //
           //  Clear indicators that were set in the Rules subroutine
           //
            clear *in;
            clear xxrule;
           //
           //  Clear Correct Coding fields
           //
            xxccdo = 'N';
            @xxccdo = 'N';

            xcmra = p2mra;
            kkefdt = p2rudt;
           //
           // Initialize allowed units for paying multiple units
           //
            unitcnt = p2unts;
           //
           // If rule date = 999999 then give message and go to next item
           //
            Exsr NoRuleDate;
            If xxErr = 'Y';
               Iter;
            Endif;
          //
          // If rule date has changed load new set of rules
          //
            if p2rudt <> xxrudt;
               exsr loadr;
               xxrudt = p2rudt;
            endif;
           //
           // Load modifiers into array for processing in PMOD subroutine
           //
            Exsr LoadMod;
           //
           // Call Calc Subroutine based on bill type
           //
            select;
            //
            // Use CALC sub for Physician, Dental and Hospital
            //
            when p2type = 'P'
                 or p2type = 'D'
                 or p2type = 'H';
               exsr calc;
            //
            // Use PHAR sub for Pharmacy bills
            //
            when p2type = 'X';
               exsr phar;
            //
            // Default to CALC
            //
            other;
               exsr calc;
            endsl;
    
            read pcawk2;
    
          enddo;
    
       endif;
      //
      //  End program and close all files
      //
       close facap;
       close facal;

       ExSr PgmEnd;
     //*******************************************************************
     //* ANESTHESIA SUBROUTINE                                           *
     //*******************************************************************
      /Free
       Begsr anes;
         p2amt = 0;
         xxunt = 0;
         remdr = 0;
         x1rem = 0;
         x2rem = 0;
         xxanmn = p2unts;
         xxftm = 0;
        //
        //   DETERMINE BASE UNITS WITHIN FROM/TO TIME RANGE
        //
         if frttm > 0;
            if frftm > 0;
               xxftm = frftm - 1;
            endif;
            if xxanmn > frttm;
               xxanmn = frttm;
            endif;
            if xxanmn > xxftm;
               xxanmn = xxanmn - xxftm;
            endif;
         endif;
        //
        //   BASE UNITS TO CALCULATE AGAINST
        //
         if xxtmdv <> *zero;
            xxUnt = xxAnMn/xxTmDv;
         //*** Beg Add *** 11/12/20 **************************************************
            remdr = %Rem(xxAnMn:xxTmDv);
         //*** End Add *** 11/12/20 **************************************************
         endif;
         if remdr > 0 and xxtmdv <> 999;
          //
          //   Remainder to determine per minute after
          //
           x1rem = remdr / xxtmdv;
          //
          // Determine value of X1REM using rounding factor
          //
           select;
           when frrnd = 'R';
              if x1rem > 0;
                x1rem = 1;
              endif;
           when frrnd = 'H';
              if x1rem >= .5;
                x1rem = 1;
              else;
                x1rem = 0;
              endif;
           when frrnd = 'O';
              eval(h) x3rem = x1rem;
              x1Rem = x3Rem;
           when frrnd = 'S';
              if remdr < 5;
                 x1rem = 0;
              else;
                 x1rem = 1;
              endif;                                                          //
           endsl;
           x2rem = x1rem * xxconv;
         endif;
         xxount = xxount + xxunt;
         if frftm = 0
               and frttm = 0
               or p2unts <= frttm;
          //
          // Use the correct calculation based on FRTANE(type of anes calc)
          //
            select;
            //
            // Normal (basic value+time units)*conv factor)
            //
            when frtane = ' ' or frtane = 'P';
            //
            //  Add additional units to Calculated units
            //
               xxount = xxount + p2anam;
               xxount = xxount + xxaunt;
               p2amt = xxount * xxconv;
               p2amt = x2rem + p2amt;
             //
            // AMAP (basic value(AMAP)) + time units*conv factor
             //
            when frtane = 'A';
            //
            //  Add additional units to Calculated units
            //
               xxount = xxount + xxaunt;
               eval(h) xxount = xxount * xxconv;
               p2amt = x2rem + xxount;
               p2amt = p2amt + p2anam;
            //
            // Multiple conv factors (basic value+time units)*conv factor)
            // time division is 1 min = 1 unit
            //
            when frtane = 'C';
            //
            // Setup the 1st and 2nd time unit values
            //
               xxunt = frftm - 1;
               xxount = p2unts - frftm;
               xxount = xxount + 1;
            //
            // Multiply by corresponding conversion factors
            //
               p2amt = xxunt * xxconv;
               xxamt = xxount * x1conv;
               p2amt = p2amt + xxamt;
            //
            // Use flat rate (anes amt) for 1st hr. Then mult remaining
            // hours by conv factor 1 for the total payment
            //
            when frtane = 'H';
               xxount = xxount + x1rem;
               if xxount >= 1;
                  p2amt = p2anam;
               endif;
               if xxount > 1;
                  xxount = xxount - 1;
                  xxamt = xxount * frconv;
                  p2amt = p2amt + xxamt;
               endif;
            endsl;
          //
          //  If set to pay only a specific unit value
          //
           if frunit > 0;
              p2amt = frunit * xxconv;
           endif;
          //
          // Set indicator to pay Anesthesia ammount (P2AMT)
          //
           *in33 = *on;
           setPay = 'Y';
           xptota1 = p2amt;
         endif;
        Endsr;
      /End-Free
     //
     //*******************************************************************
     //* ASTERSIK PROCEDURE                                              *
     //*******************************************************************
      /Free
       begsr astrsk;
         xxAstr = 'N';
     //
     //  Read from the CPT-4 File
     //
         setgt prky2 ppr01;
         readp ppr01;
         if not %Eof(fmprl);
          //
          //  Move correct field from FMPRP
          //
           if prsupp <> 'Y';
              xxAstr = 'N';
           else;
              xxAstr = prsupp;
           endif;
         else;
          //
          //  Read from the R & C File
          //
           setgt rcky2 prc02;
           readp prc02;
           if not %Eof(fmrcl);
              clear xxastr;
           endif;
    
         endif;
    
       endsr;
      //
      //*******************************************************************
      //* CALCULATION SUBROUTINE USING WORKFILE                 C A L C   *
      //*******************************************************************
       begsr calc;
        //
         // Initialize conversion factor, time division, etc.
        //
         exsr init1;
        //
        // If claim status codes is not valid for fee schedule skip
        //
         clear tabkey;
         tabkey = p2cscd;
    
         chain tcsky pcs01;
         If not %Found(ftabl);
            FReob = 'XX0097';
            Exsr Eomb;
            LeaveSr;
         Endif;
    
         xxOvrd = csovrd;
     //
      // Process override type
     //
         if xxovrd = 'Y'
               or p2fsr = 'N';
          //
          // Use possible duplicate criteria based on system control file
          // field FBAL05 pos(7) loaded into ##TDUP in SA009R.
          // ' ' is standard duplicate logic
          // '1' is for client 25
          // '2' is for client 114 and is like standard but won't check same bi
          //     and checks Rev Code and CPT independently on Hospital bills
           select;
    
           when ##tdup = ' ';
             exsr dup;
           when ##tdup = '1';
             exsr dup1;
           when ##tdup = '2';
             exsr dup;
           other;
             exsr dup;
           endsl;
           exsr updat;

           LeaveSr;

         endif;
        //
        // Load billed code into KKCPT4 based on bill type
        //
         if p2type = 'P'
               or p2type = 'D';
            kkCpt4 = P2cpt4;
         else;
            strSvCd = %Editc(P2svCd:'X');
            x1SvCd = %Subst(strSvCd:2:3);
            x1Cpt4 = p2cpt4;
            clear x1unts;
            kkCpt4 = xxSvCd;
         endif;
         if %Subst(kkCpt4:5:1) >= 'A' and %Subst(kkCpt4:5:1) <= 'Z';
            isNonStdCode = *On;
         Else;
            isNonStdCode = *Off;
         Endif;
         //
         // Perform CCI if installed and effective for the service date
         // if no effective date record then perform CCI anyway
         //
         if ##coco = 'Y' and p2type ='P';
            Exsr CCI;
         endif;                                                               //

         if xxpass < 1;
             xxpass = 1;
            n = 1;
         Endif;
        //
        // Repeat calculations as long as there are rules
         dow n <= n1;
    
           %Occur(MdsRu) = n;
    
           exsr Rules;

           n = n + 1;
    
           If Match = 'Y';
          //
          // Initialize counters used for this rule
          //
           exsr init;
          //
          // Setup conversion factors, days in the past/future, etc.
          //
           exsr setup;
          //
          // Correct Coding: Set flag and field for updating FDCAP in UPDAT
          //
           if frccod <> *blank;
             xxccod = xxcod2;
             if frccod = 'A' or frccod = 'E';
               xxccod = xxcod2;
               xxccdf = '1';
             endif;
             if frccod = 'B' or frccod = 'F';
               xxccod = xxcod1;
               xxccdf = '2';
             endif;
             if frccod = 'C' or frccod = 'G';
               xxccod = xxcod2;
               xxccdf = '3';
             endif;
             if frccod = 'D' or frccod = 'H';
               xxccod = xxcod1;
               xxccdf = '4';
             endif;
           endif;
          //
          // This determines if there is a valid value for billed modifier
          //
           if frval = 'Y'
                 or frpcvl = 'Y'
                 or frtcvl = 'Y';
             exsr pmod;
            //
            // If MODRUL returns Y then the modifier value was established and
            // the program can skip to the next rule
            //
             if modrul = 'Y';
                Iter;
             endif;
            //
           endif;
          //
           xxCpt4 = fxCfCd;
        //
           Exsr History;
           If Proc = 'Y';
              Leave;
           Endif;
           If NxtRul = 'Y';
              Iter;
           Endif;
        //
        // Determine if the service date is in the initial days of treatment
        //
         if frtrmt > 0 and frcfdy = 0 and
               frcpdy = 0 and fr#gdy <> 'Y';
            if p2fdat >= xxtrmt;
               Iter;
            endif;
         endif;
        //
        // Determine if the billed code is a minor procedure
        //
         if p2bmin <> 'Y' and frbmri = 'O';
            Iter;
         endif;
        //
        // If rule use = 'G' ,compare charged amount with pay flat amount
        //
         if *IN82 = *ON AND
               p2totc < framt;
            Iter;
         endif;
        //
        // If we were looking for a PT eval or re-eval to count RVU's and
        // didn't find one then skip the rule
        //
         if (frcmri = 'Q' or frcmri = 'R' or frcmri =
               'Z' or frcmri = '1' or frcmri = '2' or
               frcmri = '3' or frcmri = '4')
               and xxeval <> 'Y';
            Iter;
         endif;
        //
        // If we are NOT looking for PT eval or re-eval but there is a
        // PT eval or re-eval then skip this rule
        //
         if frcmri = '8' and xxeval = 'Y';
            Iter;
         endif;
        //
        // Check for the # of days between Carrier Received Date and Latest Ser
        // Date in the Bill, If # of days is 95 days or less then disallow  the
        //
         if *in81 = *On;
            if EORCN = 0;
               srdate1 = p2fdat;
            //
      /End-Free
     C                   call      'SRDDIF7'
     C                   parm                    srdate1           7 0
     C                   parm      EOCRDT        srdate2           7 0
     C                   parm      *Zeros        sr#days           5 0
     C                   parm      *Blanks       srerr             1
      /Free
               if sr#days <= fr#dsd or srerr <> '0';
                  Iter;
               endif;
            endif;
         endif;
        //
        // Stop-loss logic
        //
         if frloss > *zero;
          //
          // Compare stop-loss
          //
            if frtlos = *blank;
               if x1loss <= frloss
                   and frpct > 0
                   or xxloss <= frloss
                   and frpct = 0;
                  Iter;
               endif;
            endif;
          //
          // Compare stop-loss
          //
            if frtlos = 'K'
                 and zzloss <= frloss;
               Iter;
            endif;
          //
          // Compare stop-loss
          //
           if frtlos = 'T'
                 and p2slos <> 'Y';
              Iter;
           endif;
          //
          // Compare stop-loss
          //
           if frtlos = 'F'
                 and X1loss <= frloss;
              Iter;
           endif;
         endif;
        //
        //  This section is for determining major/minor
        //
         if frmaj = 'Y';
            if count > *zero;
               Iter;
            endif;
         endif;
        //
        //  This Section is for Comparing Seperate Procedure flag
        //  If the compared to CPT4 code is not a separate procedure then
        //  disallow the from CPT4 code; otherwise, skip the rule.
        //
         if *in42 = *on
               and x1sep <> 'N';
            Iter;
         endif;
        //
        //  This Section is for Comparing Asterisk Procedure flag
        //  If the compared to flag is 'N' and the CPT4 procedure is an
        //  asterisk code then skip the rule; otherwise continue.
        //
         if frcast = 'N'
               and x1ast = 'Y';
            Iter;
         endif;
        //                                                                ***
        //  This Section is for Comparing Asterisk Procedure flag
        //  If the compared to CPT4 code is an asterisk procedure then
        //  pay by the rule; otherwise, skip the rule.
        //                                                                ***
         if frcast = 'Y'
               and x1ast <> 'Y';
            Iter;
         endif;
         If FRcTpC = 'Y' and indPriCdFnd = *on;
            Iter;
         endif;
         If FRcTpC = 'Y' and indPriCdFnd = *off;
            exsr eomb;
            exsr pmnt;
            if proc = 'Y';
               Leave;
            endif;
         endif;
        //
        // Disallow the procedure if A separate procedure was billed with
        // a regular proc.  Pay by the rule if an asterisk procedure was
        // billed with 99201-99205.
        //
         if *in42 = *on
               and x1sep = 'N'
               or frcast = 'Y'
               and x1ast = 'Y'
               or frcast = 'N'
               and x1ast = 'N';
            exsr eomb;
            exsr pmnt;
            if proc = 'Y';
               Leave;
            endif;
         endif;
        //
        //  Checking # of hours if there were any
        //
         if *in57 = *on
               and frmaj <> 'Y';
            eval(H) hours1 = hours + (p2anam * p2unts);
            if frpmr  <> 'V';
               if hours1 <= frpttm;
                  Iter;
               endif;
            endif;
            if frpmr = 'V' and (p2mra * p2unts) < p2totc;
               if paid + (p2unts * p2mra) < (frpttm * p2conv);
                  Iter;
               endif;
           endif;
           if frpmr = 'V' and (p2mra * p2unts) >= p2totc;
              if paid + p2totc < (frpttm * p2conv);
                 Iter;
              endif;
           endif;
        endif;
        //
        //  Checking # of procedures if there were any
        //
         if *in22 = *on
               and *in40 = *off
               and frmaj <> 'Y'
               and count <> fr#prc;
            Iter;
         endif;
        //
        // Non-Hosp: Checking count
        //
         if *in22 = *on
               and *in40 = *on
               and count < fr#prc
               and frmaj <> 'Y'
               and *in78 = *off
               or *in22 = *on
               and *in40 = *on
               and count < fr#prc
               and frmaj <> 'Y'
               and ##splt<>'N';
            Iter;
         endif;
        //
        // Hosp: Checking count when multiple units are billed on the same line
        //
         if *in22 = *on and *in78 = *on
               and eobtyp = 'H'
               and *in40 = *on
               and (count + p2unts) <= fr#prc
               and frmaj <> 'Y';
            Iter;
         endif;
        //
        // Hosp: Checking count when multiple units are billed on the same line
        // and it is looking for lines on the same bill.                      e
        //
         if *in22 = *on and *in78 = *on
               and eobtyp = 'H'
               and *in40 = *on
               and *in51 = *on
               and count = 0
               and frmaj <> 'Y';
            Iter;
         endif;
        //
        // Multi-Units
         //
         // If this is an anesthesia code then dont count the minutes.  If the
         // billed code is not in the compared to range then dont count it.
         //
         zzunits = 1;
         if p2anes = 'Y';
            zzunits = 1;
         else;
             if p2cpt4 >= frcfcd and p2cpt4 <= frctcd;
                zzunits = p2unts;
             endif;
         endif;
         if *in22 = *on and ##splt = 'N'
               and *in40 = *on
               and (count + zzunits-1) < fr#prc
               and frmaj <> 'Y';
            Iter;
         endif;
         //
         // Okay, now let try and check to see if the rule was looking for a
         // modifier and nothing matched in history or on the same line if
         // we have multiple units on the same line.
         //
         if *in22 = *on and ##splt = 'N'
               and count = 0 and frcmod <> ' ' and
               frcmod <> p2mod1 and
               frcmod <> p2mod2 and
               frcmod <> p2mod3 and
               frcmod <> p2mod4;
            Iter;
         endif;
         //
         // If the rule was looking for different provider and there was not
         // any history for that then skip the rule
         //
         if *in22 = *on and ##splt = 'N'
               and count = 0 and *in19 = *on;
            Iter;
         endif;
        //
        //  Checking # of days paid
        //
         if *in23 = *on
               and *in41 = *off
               and x <> fr#day;
            Iter;
         endif;
         if *in23 = *on
               and *in41 = *on
               and x < fr#day;
            Iter;
         endif;
        //
        //  This section is for edits without any comparison or
        //  determing number of procedures or number of days
        //
        //  Call FOLU subroutine -- If FRFUD equals 'Y'
        //
         if frfud = 'Y';
            exsr folu;
            if rule = 'Y';
               Iter;
            endif;
         endif;
        //
        //  Call EOMB subroutine -- Determines what EOMB and error message
        //
         if *in16 = *off
               and *in17 = *off
               and *in22 = *off
               and *in23 = *off
               and *in51 = *off
               and frfud <> 'Y'
               and *in57 = *off;
           exsr eomb;
           exsr pmnt;
           if proc = 'Y';
              Leave;
      //
     // If this rule is just for setting an EOMB code then move it here
     // and set a flag so it won't be replaced by another rule.
      //
           else;
             if *in24 = *on;
                //*** Beg Chg *** 07/18/23 **************************************
                //*** xpEomb = FReob;
                xpEomb = FReobCd1;
                xpEomb2 = FReobCd2;
                //*** End Chg *** 07/18/23 **************************************
                eombFl = 'Y';
      //
      // Store the rule state for use when getting EOB desc for CC (Corr cod)
      //
                xxst = frst;
             endif;
           endif;
           Iter;
         endif;
        //
        //  Call EOMB subroutine -- Determines what EOMB and error message
        //
         if *in16 = *off
               and *in17 = *on
               and *in22 = *off
               and *in23 = *off
               and *in51 = *on
               and frfud <> 'Y'
               and *in57 = *off;
           exsr eomb;
           exsr pmnt;
           if proc = 'Y';
              Leave;
     //
     // If this rule is just for setting an EOMB code then move it here
     // and set a flag so it won't be replaced by another rule.
     //
           else;
              if *in24 = *on;
                 //*** Beg Chg *** 07/18/23 **************************************
                 //*** xpEomb = FReob;
                 xpEomb = FReobCd1;
                 xpEomb2 = FReobCd2;
                 //*** End Chg *** 07/18/23 **************************************
                 eombFl = 'Y';
      //
      // Store the rule state for use when getting EOB desc for CC (Corr cod)
      //
                 xxst = frst;
              endif;
           endif;
         endif;
        //
        //  Call payment subroutine -- Determines what to pay
        //  If you are accumulating dates of service or
        //  number of procedures or physical therapy hours
        //
         if *in22 = *on
               or *in23 = *on
               or frfud = 'Y'
               or *in57 = *on;
            exsr eomb;
            exsr pmnt;
            if proc = 'Y';
               Leave;
     //
     // If this rule is just for setting an EOMB code then move it here
     // and set a flag so it won't be replaced by another rule.
     //
            else;
              if *in24 = *on;
                 //*** Beg Chg *** 07/18/23 **************************************
                 //*** xpEomb = FReob;
                 xpEomb = FReobCd1;
                 xpEomb2 = FReobCd2;
                 //*** End Chg *** 07/18/23 **************************************
                 EombFl = 'Y';
      //
      // Store the rule state for use when getting EOB desc for CC (Corr cod)
      //
                 xxst = frst;
              endif;
           endif;
         endif;
       Endif;
        //
       enddo;
      //
      //  Default with payment of MRA
      //  Turn on SETPAY flag in case no rules turned it on. This insure
      //  the update subroutine will be executed
      //
       If Proc <> 'Y';
          setPay = 'Y';
          *in26 = *on;
          exsr pmnt;
       Endif;
      /End-Free
       Endsr;
     //****************************************************************
     //* Calculate Units                                   C A L C _ U N I T
     //****************************************************************
      /Free
       begsr calcUnit;
         if ##splt = 'N';
           //
           // If the count from history exceeds the number of proc then allow
           // zero units if denying else pay units from the billed code
           //
           select;
           when *in22 = *off and frcfcd <> *blank
                 and frctcd <> *blank;
             if *in28 = *on;
               unitCnt=0;
             else;
               unitCnt=p2unts;
             endif;
           when *in22 = *on and count >= fr#prc;
             if *in28 = *on;
               unitCnt=0;
             else;
               unitCnt=p2unts;
             endif;
             //
             // If the count from history is less than number of proc then
             //
           when *in22 = *on and count < fr#prc;
             if p2unts + count <= fr#prc;
               unitcnt = p2unts;
             else;
               unitcnt = fr#prc - count;
             endif;
           endsl;
           if frmaj = 'Y' and *in28=*on;
             unitcnt = 0;
           endif;
           if FRcTpC = 'Y' and *in28 = *on;
              unitCnt=0;
           endif;
         endif;
      /End-Free
     C     calcUnite     endsr
      //*******************************************************************
      //* CCI Subroutine                                        C C I     *
      //*******************************************************************
       begsr CCI;
            kkprcd = p2cpt4;
            kkfdat = cafdat;
            setgt prky4 ppr01;
            readpe prky5 ppr01;
            if pr1ah <> 'C' and pr1ah <> 'B';
               k1st   = 'CC';
               kkatyp = 'P';
               chain ruky pru01;
               if not %Eof(fdrul1) and p2fdat >= ruefdt;
                  exsr corcod;
               endif;
               if %Eof(fdrul1);
                  exsr corcod;
               endif;
            endif;
       Endsr;
     //****************************************************************
     //* CLEAR RULES INDICATORS                            C L R I N D
     //****************************************************************
      /Free
       begsr clrind;
        //
        // Clear indicators 1-59
        //
         For z = 1 to 59;
            *in(z) = *off;
         endfor;
        //
         // Clear indicators 63-98
        //
         z = 63;
         dow z <= 98;
            *in(z) = *off;
            z = z + 1;
         enddo;
       Endsr;
      /End-Free
     //****************************************************************
     //* CLEAR ADJUDICATION MESSAGES/EOMB CODES SUBROUTINE C L R M S G
     //****************************************************************
      /Free
       begsr clrmsg;
        //
        // Delete adjudication message record if it exists
        //
         setll mgky pmg02;
         reade mgky pmg02;
        //
         dow not %Eof(fdmgl6);
           delete pmg02;
           reade mgky pmg02;
         enddo;
        //
        // Delete Adjudication EOMB codes if they exists
        //
         qqEsrc = 'A';
         qqDseq = P2dSeq;
         setll deky pde01;
         reade deky pde01;
    
         dow not %Eof(fddel4);
            delete pde01;
            reade deky pde01;
         enddo;
        //
        // Delete Correct Coding EOMB codes if they exists
        //
         qqEsrc = 'C';
         setll deky pde01;
         reade deky pde01;
    
         dow not %Eof(fddel4);
            delete pde01;
            reade deky pde01;
         enddo;
       endsr;
      //****************************************************************
      //* CLEAR TOTAL PAID SUBROUTINE                      C L R P A Y
      //****************************************************************
       begsr clrpay;
          read pcawk2;
          dow not %Eof(facal);
          //
           // Do not Process override types
          //
             tabKey = p3cscd;
             chain tcsky pcs01;
             if csovrd <> 'Y';
                chain caky1b pca01;
                if %Found(fdcal5);
                   capaid = *zeros;
      /End-Free
     C                   time                    dsdttm
     //
      /Free
                   srdti = dsdate;
      /End-Free
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         cadt
      /Free
                   CAtime = dsTime;
                   CAuser = q1User;
                   CApgm = q1Pgm;

                   update pca01;
                   //
                   // Determine if claim has reached it limit
                   //
                   chain fnky pfn01;
                   If %Found(fmfnl)
                     and ##clt# = '114'
                     and fnnwcg = 'Y';
                      ##Rrn2 = CArrn;
      /End-free
     C                   call      'FA770R'
     C                   parm                    ##rrn2            9 0
     C                   parm      'A'           @1mode            1
      /Free
                   endif;                                                     //
                endif;
             endif;

           read pcawk2;

          enddo;
    
       endsr;
      //****************************************************************
      //* SKIP TO CORRECT CODING RULES                    C C I S K I P
      //****************************************************************
       begsr cciskip;
         //
         // Cycle through rules until we get to Correct Coding Rules
         //
         n = n + 1;

         dow n <= N1;
            %Occur(mdsRu) = n;
            if frst = 'CC';
               leave;
            endif;
            n = n + 1;
         enddo;
         //
         //  IN24 needs to be setof because it prevents other rules from replac
         // ing the EOB code set in the first rule. We don't want that here.
         //
         *in24 = *off;
       endsr;
      //****************************************************************
      //* CORRECT CODING SUBROUTINE                       C O R C O D
      //****************************************************************
       begsr corcod;
         kkefdt = p2fdat;
         kkfdat = p2fdat;
         xxcc = *blank;
         xxcod1 = *blanks;
         xxcod2 = *blanks;
        //
        // Read through case history same date.
        //
         chain cakyp pca0a;
         If %Found(fdcalap);
         dow not %Eof(fdcalap);
           //
           // Ignore if same line item or payment denied or flag set to bypass
           // but process if on this bill and not yet paid.
           //
            if p91ah <> 'C' and p91ah <> 'B';
               if p2dseq <> cpdseq and cppaid > 0
                     or p2dseq <> cpdseq and cppaid =0
                     and p2eob# = cpeob#;
                  kkcod1 = p2cpt4;
                  kkcod2 = cpcpt4;
                 //
                 // Check for the comprensive code
                 //
                  setgt bnky pbn01;
                  readpe bnky1 pbn01;
                  dow not %Eof(fmbnl3) and kkefdt >= bnefdt;
                     If kkefdt < bndldt and bndldt <> 0
                           or bndldt = 0;
                        xxcod1 = p2cpt4;
                        xxcod2 = cpcpt4;
                   //
                   // Check to see if a modifier is allowed to differentiate ser
                   //
                        if bngbmi = '0';
                           xxcc = 'A';
                        else;
                           xxcc = 'E';
                        endif;
                        LeaveSr;
                     endif;

                     readpe bnky1 pbn01;

                  enddo;
                 //
                 // Check for the component code
                 //
                  kkcod1 = cpcpt4;
                  kkcod2 = p2cpt4;

                  setgt bnky pbn01;
                  readpe bnky1 pbn01;
                  dow not %Eof(fmbnl3) and kkefdt >= bnefdt;
                     If kkefdt < bndldt and bndldt <> 0
                          or bndldt = 0;
                        xxcod1 = cpcpt4;
                        xxcod2 = p2cpt4;
                   //
                   // Check to see if a modifier is allowed to differentiate ser
                   //
                        if bngbmi = '0';
                           xxcc = 'B';
                        else;
                           xxcc = 'F';
                        endif;
                     endif;

                     readpe bnky1 pbn01;

                  enddo;
                 //
                 // Check for mutually exclusive codes
                 //
                  kkcod1 = p2cpt4;
                  kkcod2 = cpcpt4;

                  setgt muky pmu01;
                  readpe muky1 pmu01;
                  dow not %Eof(fmmul3) and kkefdt >= muefdt;
                     If kkefdt < mudldt and mudldt <> 0
                       or mudldt = 0;

                        xxcod1 = p2cpt4;
                        xxcod2 = cpcpt4;
                   //
                   // Check to see if a modifier is allowed to differentiate ser
                   //
                        if mugbmi = '0';
                           xxcc = 'C';
                        else;
                           xxcc = 'G';
                        endif;
                     endif;

                     readpe muky1 pmu01;

                  enddo;
              //
              // Check for mutually exclusive codes in reverse order
              //
                  kkcod1 = cpcpt4;
                  kkcod2 = p2cpt4;

                  setgt muky pmu01;
                  readpe muky1 pmu01;
                  dow not %Eof(fmmul3) and kkefdt >= muefdt;
                     If kkefdt < mudldt and mudldt <> 0
                           or mudldt = 0;
                         xxcod1 = cpcpt4;
                         xxcod2 = p2cpt4;
                   //
                   // Check to see if a modifier is allowed to differentiate ser
                   //
                         if mugbmi = '0';
                            xxcc = 'D';
                         else;
                            xxcc = 'H';
                         endif;
                         LeaveSr;
                     endif;
                     readpe muky1 pmu01;
                  enddo;

               endif;

            endif;                                                            //

            reade cakyp pca0a;

         enddo;

         Endif;

       Endsr;

      /End-Free
     //****************************************************************
     //* COUNTING HOSPITAL UNITS SUBROUTINE     C O U N T U N I T S
     //****************************************************************
      /Free
       begsr countunits;

          select;
          when p9unts = 1 or p9mra = 0 or
               p9unts > 1 and p9mra = 0;
             count = count + p9unts;
          other;
             if p9mraa > 0;
                x3unts = (p9totc - p9mra)/p9mraa;
                count = count + x3unts;
             endif;
          endsl;

       endsr;
      //****************************************************************
      //* DUPLICATE CHARGES SUBROUTINE                          D U P
      //****************************************************************
       begsr dup;
         //
        // Hospital codes
         //
         select;
         when p2type = 'H';
            i = 1;
          //
          // Process history items until counter (N) reaches last item (N2)
          //
            dow i <= n2;
               %occur(mdsp9) = i;
     //
     // Only count as dup if line item is part of history.
     //
               if p9chk# = 0
                      and p9eob# <> p2eob#;
               else;
                  if p9cpt4 = p2cpt4
                        and p9svcd = p2svcd
                        and p9fdat = p2fdat
                        and p9fein = p2fein
                        and p9dseq <> p2dseq
                     //
                     // If type of duplicate logic = 2 only check the revenue co
                     // is blank then check the CPT code.
                     //
                        or ##tdup = '2' and p2svcd <> 0
                        and p9svcd = p2svcd
                        and p9fdat = p2fdat
                        and p9fein = p2fein
                        and p9dseq <> p2dseq

                        or ##tdup = '2' and p2svcd <> 0
                        and p9svcd <> p2svcd
                        and p9cpt4 = p2cpt4
                        and p9fdat = p2fdat
                        and p9fein = p2fein
                        and p9dseq <> p2dseq

                        or ##tdup = '2' and p2svcd = 0
                        and p2cpt4 <> *blanks
                        and p9cpt4 = p2cpt4
                        and p9fdat = p2fdat
                        and p9fein = p2fein
                        and p9dseq <> p2dseq;
                 //
                 // Don't write message if client is set up not to include dups
                 // bill.
                 //
                     if ##tdup = '2'
                          and p9eob# <> p2eob#
                          or ##tdup = ' ';
                  //
                  // Hard code the "Possible duplicate charges" error
                  //
                        %Subst(xxMsg:1:2) = qqst;
                        %Subst(xxMsg:3:5) = '00003';
                        xxErr = 'Y';
                        *in25 = *off;
     //
     // Set indicator off so that no EOMB will be written by Sub EOMB  e
     //
                        *in24 = *off;
     //
     // Write the error msg # to the file (FDMGP)
     //
                        exsr eomb;
                        leave;
                     endif;
                  endif;
               endif;                                                         //
               i = i + 1;
            enddo;
          //
          // Physician codes
          //
         when p2type = 'P'
               or p2type = 'D';
          //
           i = 1;
          //
          // Process history items until counter (N) reaches last item (N2)
          //
           dow i <= n2;
            //
              %Occur(mdsp9) = i;
     //
     // Only count as dup if line item is part of history.
     //
              if p9chk# = 0
                    and p9eob# <> p2eob#;
              else;
                 if p9cpt4 = p2cpt4
                       and p9fdat = p2fdat
                       and p9fein = p2fein
                       and p9dseq <> p2dseq;
                 //
                 // Don't write message if client is set up not to include dups
                 // bill.
                 //
                    if ##tdup = '2'
                          and p9eob# <> p2eob#
                          or ##tdup = ' ';
                  //
                  // Hard code the "Possible duplicate charges" error
                  //
                       %Subst(xxMsg:1:2) = qqst;
                       %Subst(xxMsg:3:5) = '00003';
                       xxErr = 'Y';
                       *in25 = *off;
     //
     // Set indicator off so that no EOMB will be written by Sub EOMB  e
     //
                       *in24 = *off;
     //
     // Write the error msg # to the file (FDMGP)
                       exsr eomb;
                       leave;
                    endif;
                 endif;
              endif;                                                          //
              i = i + 1;
           enddo;
           if xxclt# = 114
                 and ##tdup = '2';
              clear kkeob#;

              open fal2l1;
              setll Key1 fal2L1;
              reade Key1 fal2L1;
              Dow Not %Eof(fal2l1);
                 If p2cpt4 = l2tocd;
                    xxeob# = L2EOB#;
                    //
                    If xxeob# <> p2eob#;
                       Chain Key3 pxx01;
                       If %Found(faeolg) and GEOcrPe = 'T';
                          kkeob# = l2Eob#;
                       Endif;
                    Endif;                                                    //
                 Endif;

                 setll key2 fdcal27;
                 reade key2 fdcal27;
                 Dow Not %Eof(fdcal27);
                    If p2eob# <> txeob#
                          and txfein = p2fein
                          and p2fdat = txfdat;

      /End-Free
     C                   call      'SRDDIF7'
     C                   parm      L2IDT         date1             7 0
     C                   parm      sydate        date2             7 0
     C                   parm      *Zeros        #days             5 0
     C                   parm      *Blanks       err               1
     //
      /Free
                       if #days <= 10;
                          %Subst(xxMsg:1:2) = qqst;
                          %Subst(xxMsg:3:5) = '00003';
                          xxErr = 'Y';
                          *in24 = *off;
                          *in25 = *off;
                          ExSr eomb;
                          Leave;
                       endif;
                    Endif;
                    reade key2 FDCAL27;
                 Enddo;
                 clear kkeob#;
                 reade key1 fal2l1;
              Enddo;
             close fal2l1;
           Endif;
        //
        // Pharmacy codes-Use pharmacy file not Data structure
        //
         when p2type = 'X';
            if dprxpt <> 'S';
               kkNdc = DPndc;
               chain dpky1 pdp02;
               *In99 = not %Found;
            else;
               kkHcPc = DPhcpc;
               chain dpky2 pdp04;
               *In99 = Not %Found;
            endif;
            dow *in99 = *off;
               if dprxpt <> 'S';
                 xxdseq = d1dseq;
               else;
                 xxdseq = d4dseq;
               endif;
               chain cakyf pca0p;
               if %Found(fdcalf)
                     and pxfdat = p2fdat
                     and pxfein = p2fein
                   and pxdseq <> p2dseq;
                  if pxchk# = 0
                        and pxeob# <> p2eob#;
                  else;
                //
                 // Don't write message if client is set up not to include dups
                 // bill.
                //
                     if ##tdup = '2'
                           and pxeob# <> p2eob#
                           or ##tdup = ' ';
                  //
                  // Hard code the "Possible duplicate charges" error
                  //
                        %Subst(xxMsg:1:2) = qqst;
                        %Subst(xxMsg:3:5) = '00003';
                        xxErr = 'Y';
                        *in25 = *off;
     //
     // Set indicator off so that no EOMB will be written by Sub EOMB  e
     //
                        *in24 = *off;
     //
     // Write the error msg # to the file (FDMGP)
     //
                        exsr eomb;
                        leave;
                     endif;
                  endif;
               endif;
               if dprxpt <> 'S';
                  reade dpky1 pdp02;
                  *in99 = %Eof;
               else;
                  reade dpky2 pdp04;
                  *In99 = %Eof;
               endif;
            enddo;
         endsl;
       Endsr;
     //****************************************************************
     //* DUPLICATE CHARGES SUBROUTINE FOR TYPE 1               D U P 1
     //****************************************************************
       begsr dup1;
         chain caky15 pca10;
         *in99 = not %Found;
        //
        // Only include if line is part of history (PZCHK# <> )
        //
         if *in99 = *off
               and pzdseq <> p2dseq;
            if pzchk# = 0 and p2eob# <> pzeob#;
            else;
            //
            // Hard code the "Possible duplicate charges" error
            //
               %Subst(xxMsg:1:2) = qqst;
               %Subst(xxMsg:3:5) = '00003';
               xxErr = 'Y';
               *in25 = *off;
     //
     // Set indicator off so that no EOMB will be written by Sub EOMB  e
     //
               *in24 = *off;
     //
     // Write the error msg # to the file (FDMGP)
     //
               exsr eomb;
            endif;
         endif;
       Endsr;
     //
     //****************************************************************
     //* Eomb Subroutine                                 E O M B      *
     //****************************************************************
       begsr eomb;
        //
        // Do not replace if an EOMB code was written by a previous rule
        //
         if eombfl <> 'Y';
            if *in24 = *on;
               //*** Beg Chg *** 07/18/23 **************************************
               //*** xpEomb = FReob;
               xpEomb  = FReobCd1;
               xpEomb2 = FReobCd2;
               //*** End Chg *** 07/18/23 **************************************
      //
      // Store the rule state for use when getting EOB desc for CC (Corr cod)
      //
                xxst = frst;
            endif;
         Else;
            if *in24 = *on   and @xxccdo = 'Y';
               //*** Beg Chg *** 07/18/23 **************************************
               //*** xpEomb = FReob;
               xpEomb  = FReobCd1;
               xpEomb2 = FReobCd2;
               //*** End Chg *** 07/18/23 **************************************
      //
      // Store the rule state for use when getting EOB desc for CC (Corr cod)
      //
               xxst = frst;
            endif;
         endif;
        //
        //  If an EOB has been written already for this rule then skip
        //
         if frrule <> x1rule
               and *in25 = *on
               or xxerr = 'Y';
          //
          // Load rule message into key field/data structure
          //
            if *in25 = *on;
               xxMsg = FRmsg;
            endif;
          //
          // Write error message only if it does not already exist
          //
           chain mgky2 pmg03;
           if not %Found(fdmgl5);
            //
            // Find the next available message sequence number
            //
              xxMseq = *hival;
              setgt mgky1 pmg02;
              readpe(n) mgky pmg02;
              if not %Eof(fdmgl6);
                 seqms = mgmseq + 1;
              else;
                 seqms = 1;
              endif;
              mgmseq = seqms;
              mgco# = qqco#;
              mgsub# = p2sub#;
              MGfnd = P2fnd;
              MGfyr = P2fyr;
              MGcase = P2case;
              MGdcs# = P2dcs#;
              mgeob# = p2eob#;
              mgdseq = p2dseq;
              select;
              when x1msg = 'CC';
                 mgsrc = 'C';
              other;
                 mgsrc = 'S';
              endsl;
              clear mgcsi;
              %Subst(MGmsg#:1:2) = x1Msg;
              %Subst(MGmsg#:3:5) = x2Msg;
      //
      // Get the message description
      //
      /Free
              kkmsg# = mgmsg#;
              chain miky pmi01;
              If %Found(ftmil);
                 MGdesc = MIdesc;
              Endif;
              mgrcn = xxrcn;
              MGstat = 'A';
              MGuser = q1User;
              MGpgm = Q1pgm;
              MGiPgm = q1Pgm;
              mgtime = dstime;
              MGiUsr = q1User;
              srdti = dsdate;
      /End-Free
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         mgdt
     C                   movel     srdto         mgidt
      /Free
              mgitim = mgtime;
              write pmg01;
           endif;
          //
          //  Set X1RULE to FRRULE so EOB's are not duplicated
          //
           x1rule = frrule;
          //
          //  Used to enter second and third error messages if entered
          //
           if frmsg2 <> *blanks
                   and *in25 = *on;
              xxMsg = FRmsg2;
     //
     // Write 2nd error message only if it does not already exist
     //
              chain(n) mgky2 pmg03;
              if not %Found(fdmgl5);
                 seqms = seqms + 1;
                 mgmseq = seqms;
                 mgco# = qqco#;
                 mgsub# = p2sub#;
                 MGfnd = P2fnd;
                 MGfyr = P2fyr;
                 MGcase = P2case;
                 MGdcs# = P2dcs#;
                 mgeob# = p2eob#;
                 mgdseq = p2dseq;
                 select;
                 when x1msg = 'CC';
                    mgsrc = 'C';
                 other;
                    mgsrc = 'S';
                 endsl;
                 clear mgcsi;
                 %Subst(MGmsg#:1:2) = x1Msg;
                 %Subst(MGmsg#:3:5) = x2Msg;
                 mgrcn = xxrcn;
                 MGstat = 'A';
                 MGuser = q1User;
                 MGpgm = q1Pgm;
                 MGiPgm = q1Pgm;
                 mgtime = dstime;
                 srdti = dsdate;
                 MGiUsr = q1User;
      /End-Free
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         mgdt
     C                   movel     srdto         mgidt
      /Free
                 srdti = dsdate;
                 mgitim = mgtime;
                 write pmg01;
              endif;
           endif;
           if frmsg3 <> *blanks
                 and *in25 = *on;
              xxMsg = FRmsg3;
     //
     // Write 3rd error message only if it does not already exist
     //
              chain(n) mgky2 pmg03;
              if not %Found(fdmgl5);
                 seqms = seqms + 1;
                 mgmseq = seqms;
                 mgco# = qqco#;
                 mgsub# = p2sub#;
                 MGfnd = P2fnd;
                 MGfyr = P2fyr;
                 MGcase = P2case;
                 MGdcs# = P2dcs#;
                 mgeob# = p2eob#;
                 mgdseq = p2dseq;
                 select;
                 when x1msg = 'CC';
                    mgsrc = 'C';
                 other;
                    mgsrc = 'S';
                 endsl;
                 clear mgcsi;
                 %Subst(MGmsg#:1:2) = x1Msg;
                 %Subst(MGmsg#:3:5) = x2Msg;
                 mgrcn = xxrcn;
                 MGstat = 'A';
                 MGuser = q1User;
                 MGpgm = q1Pgm;
                 MGiPgm = q1Pgm;
                 mgtime = dstime;
                 srdti = dsdate;
                 MGiUsr = q1User;
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         mgdt
      /Free
                 srdti = dsdate;
      /End-Free
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         mgdt
     C                   movel     srdto         mgidt
      /Free
                 mgitim = mgtime;
                 write pmg01;
              endif;
           endif;
         endif;
       Endsr;
     //
      //*******************************************************************
      //* UPDATE FDDEP WITH EOMB                                E O M B 1 *
      //*******************************************************************
       begsr eomb1;
        //
        // Find the EOMB description using adj state or CC if processing a
        // Correct Coding rule
        //
         //*** Beg Chg *** 07/18/23 **************************************
         //*** EBkey = XPeomb;
         EBkey = chrEobCode;
         //*** End Chg *** 07/18/23 **************************************
         if xxst <> 'CC';
            chain ebky peb01;
            *IN60 = NOT %FOUND;
         else;
            chain ebky peb02;
            *IN60 = NOT %FOUND;
         endif;
         if *in60 = *off;
            xxNot1 = EBdesc;
            if eoast <> 'KY';
               xxNot2 = EBdsc2;
            endif;
            xxNot3 = EBdsc3;
            xxNot4 = EBdsc4;
         else;
            clear xxnot1;
            clear xxnot2;
            clear xxnot3;
            clear xxnot4;
         endif;
         if xxst <> 'CC';
            qqEsrc = 'A';
         else;
            qqesrc = 'C';
         endif;
         deprnt = 'Y';
         qqDseq = P2dseq;
     //
     // Determine if an EOMB already exists for this DSEQ number
     //
         chain deky pde01;
         if %Found(fddel4);
            //*** Beg Chg *** 07/18/23 **************************************
            //*** DEeob = xpEomb;
            DEeobCd = chrEobCode;
            //*** End Chg *** 07/18/23 **************************************
            DEnot1 = xxNot1;
            DEnot2 = xxNot2;
            DEnot3 = xxNot3;
            DEnot4 = xxNot4;
     //
     // Update file maintenance fields
     //
            DEuser = Q1user;
            DEpgm = q1Pgm;
     C                   time                    dsdttm
            srdti = dsdate;
      /End-Free
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         dedt
      /Free
            detime = dstime;
            update pde01;
         else;
          //
          // No EOMB exists so find the next sequence number and add to file
          //
            setgt deky1 pde02;
            readpe deky1 pde02;
            if not %Eof(fddel);
               xxseq# = deseq# + 1;
            else;
               xxseq# = 1;
            endif;
            clear pde01;
            DEco# = P2co#;
            DEsub# = P2sub#;
            DEfnd = P2fnd;
            DEfyr = P2fyr;
            DEcase = P2case;
            DEss# = P2ss#;
            DEacDt = P2acdt;
            DEeob# = P2eob#;
            DEdSeq = P2dseq;
            DEseq# = xxSeq#;
            //*** Beg Chg *** 07/18/23 **************************************
            //*** DEeob = xpEomb;
            DEeobCd = chrEobCode;
            //*** End Chg *** 07/18/23 **************************************
            DEnot1 = xxNot1;
            DEnot2 = xxNot2;
            DEnot3 = xxNot3;
            DEnot4 = xxNot4;
            DEstat = 'A';
            deesrc = qqesrc;
            deprnt = 'Y';
            dercn = xxrcn;
          //
          // Update file maintenance fields
          //
            DEuser = q1User;
            DEpgm = q1Pgm;
            DEiUsr = q1User;
            DEiPgm = q1Pgm;
      /End-Free
     C                   time                    dsdttm
      /Free
            srdti = dsdate;
      /End-Free
     C                   call      'SRCVTDAT'    dtcymd
     //
     C                   movel     srdto         dedt
     C                   movel     srdto         deidt
      /Free
            detime = dstime;
            deitim = dstime;
            write pde01;
         endif;
       Endsr;
     //*******************************************************************
     //* DETERMINE IF CODE IS IN THE FOLLOW UP RANGE                     *
     //*******************************************************************
       begsr folu;
          rule = 'Y';
          u = 1;
          dow u <= 500
               and fuf(u) > *zero;
             if *in70 <> *on;
                if p2fdat > fuf(u)
                      and p2fdat <= fut(u)
                      and p2dseq <> fudseq(u)
                      and flg(u) <> 'N';
                   rule = 'N';
                endif;
             else;
                if flg(u) = frbfut and p2fdat <= fut(u)
                      and p2fdat >= fuf(u)
                      and p2dseq <> fudseq(u)

                      or flg(u) = frbfut and p2fdat = fuf(u)
                      and p2dseq <> fudseq(u);
                   rule =  'N';
                endif;
             //
             // Check pre-op (one day prior to surgery)
             //
                if frbfut = 'P';
                   srdti = fuf(u);
                   srpst = 1;
      /End-Free
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         wkfdat
     C                   move      wkfdat        srdte

     C                   call      'SRPAST8'
     C                   parm                    srdte
     C                   parm                    srpst
     C                   parm                    srerr
     C                   parm                    srterm
     //
     C                   z-add     srdte         srdti
     //
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         fuf(u)
      //
      /Free
                   if p2fdat >= fuf(u)
                         and p2fdat <= fut(u)
                         and p2dseq <> fudseq(u)
                         and flg(u) <> 'N';
                      rule = 'N';
                   endif;
                endif;
             endif;
             u = u + 1;
          enddo;
       Endsr;
     //*******************************************************************
     //* Process Case History - SUBROUTINE              H I S T O R Y   *
     //*******************************************************************
       begsr History;
     //
     //   Converting the broken down codes into a service code
     //   Revenue code, procedure code and units.
     //
           NxtRul = ' ';
           Proc   = ' ';
           if *in16 = *on
                 or *in17 = *on
                 or *in51 = *on
                 or *in56 = *on
                 or *in57 = *on;
              i = 1;
            //
            // Process history items until counter (N) reaches last item (N2)
            //
             //
            //  Checking Addon Code Rule.  Looking for the primary code.
             //
              if FRctpc = 'Y';
                 indPriCdFnd = *off;
              Endif;
              dow i <= n2;
              //
                 nxtFlg = *off;

                 %occur(mdsP9) = i;

                 i = i + 1;
     //
      // Check for a PT eval or re-eval. This will determine later if we
      // apply this rule or not
     //
               if FRCMRI = 'Q' and p9cpt4 = '97001'
                  or FRCMRI = 'Q' and p9cpt4 = '97003'
                  or FRCMRI = 'R' and p9cpt4 = '97002'
                  or FRCMRI = 'R' and p9cpt4 = '97004'
                  or FRCMRI = 'Z' and p9cpt4 = '99201'
                  or FRCMRI = '1' and p9cpt4 = '99202'
                  or FRCMRI = '2' and p9cpt4 = '99203'
                  or FRCMRI = '3' and p9cpt4 = '99204'
                  or FRCMRI = '4' and p9cpt4 = '99212'
                  or FRcMri = 'Q' and p91af = 'E' and P9fDat >= 1190401
                  or FRcMri = 'R' and p91af = 'R' and P9fDat >= 1190401
                  or FRcMri = '8' and p91af = 'R' and P9fDat >= 1190401
                  or FRcMri = '8' and p91af = 'E' and P9fDat >= 1190401;
                  if p9fdat = p2fdat and p9totp > 0;
                     xxeval = 'Y';
                     xxpart = p9mraa - p9totp;
                  endif;
                //
               endif;
              //
               // Comparing conditions must be met. Go to next history record if
               // conditions are not met.
               if *in16 = *on
                  and p9fdat <> p2fdat
                  or *in22 = *off
                  and *in23 = *off
                  and *in17 = *on
                  and p9fdat > p2fdat
                  or *in18 = *on
                  and p9fein <> p2fein
                  or *in18 = *on
                  and p9mod <> p2mod
                  or *in19 = *on
                  and p9fein = p2fein
                  and p9mod = p2mod
                  or frhmra = 'Y'
                  and p2amt > p9mraa
                  or *in51 = *on
                  and p2eob# <> p9eob#
                  or frcprt <> *blank
                  and frcprt <> p9part
                  or x2pos <> *zero
                  and x2pos <> p9pos
                  or frcmri = 'Y'
                  and p9cmri <> 'Y'
                  or frcmri = 'I'
                  and p9cmri <> 'Y'
                  or frcmri = 'C'
                  and p9ccas <> 'Y'
                  or frcmri = 'E'
                  and p9cmri = 'Y'
                  or frovrd <> *blank
                  and p9ovrd = 'Y'
                  and p9eob# = qqeob#
                  or p9chk# = 0
                  and p9eob# <> p2eob#
                  or *in75 = *on and *in76 = *on
                  and p2cpt4 <> p9enba
                  or *in76 = *on and *in75 = *off
                  and p2enba <> p9enba
                  or *in84 = *on and p9pmod <> *blanks;
                  Iter;
               endif;

               select;
                //
                // CPT4 procedure code
                //
               when p2type = 'P';
                 if p9cpt4 >= fxcfcd
                    and p9cpt4 <= fxctcd
                    and frefcd = *blanks
                    and fretcd = *blanks
                    or p9cpt4 >= fxcfcd
                    and p9cpt4 <= fxctcd
                    and p9cpt4 < frefcd
                    or p9cpt4 >= fxcfcd
                    and p9cpt4 <= fxctcd
                    and p9cpt4 > fretcd;
                 else;
                  //
                  // Does not meet code selection so set flag to read next code
                  //
                    nxtFlg = *on;
                 endif;
                //
                // Hospital service code
               when p2type = 'H';
                 //
                // Converting the detail code to an 11 char field to test
                 //
                 xoSvCd = %Char(P9svCd);
                 xoCpt4 = P9cpt4;
                 xxunts = *zeros;
                 xoUnts = '000';
                 if xocode >= frcfcd
                    and xocode <= frctcd
                    and frefcd = *blank
                    and fretcd = *blank
                    or xocode >= frcfcd
                    and xocode <= frctcd
                    and xocpt4 < %subst(frefcd:4:5)
                    or xocode >= frcfcd
                    and xocode <= frctcd
                    and xocpt4 < %subst(fretcd:4:5);
                    if p9cpt4 < fxcfcd or p9cpt4 > fxctcd;
                       nxtFlg = *on;
                    endif;
                 else;
                  //
                  // Does not meet code selection so set flag to read next code
                  //
                    nxtFlg = *on;
                 endif;
               endsl;
               if nxtflg = *off;
                //
                // Set on flag to process Medicine codes again on the 2nd pass
                //
                 if *in48 = *on;
                    xxPas2 = *on;
                 endif;
                //
                // Increment number of days. Use the window from the SETUP subr
                //
                 if *in23 = *on;
                  //
                  // Counting days without a window setup
                  //
                   if *in20 = *off
                      and *in21 = *off
                      and p9fdat <> p2fdat
                      and p9totp <> 0
                      or *in20 = *on
                      and *in21 = *on
                      and p9fdat <> p2fdat
                      and p9fdat >= xxtrmt
                      and p9fdat >= xxpst7
                      and p9fdat <= xxfutr
                      and p9totp <> 0;
                    //
                    // Only add to the array if payment was set to MRA
                    //
      /End-Free
     C     p9fdat        lookup    date                                   62
      /Free
                     if *in62 = *off;
                        x = x + 1;
                        date(x) = p9fdat;
                     endif;
                   endif;
                 endif;
                //
                // Total up stop-loss for current bill and history
                 xxloss = xxloss + p9totc;
                //
                // Total X1LOSS which includes this bill only
                //
                 if p9eob# = qqeob#;
                    if (FRTLOS   = 'F'  or  FRTLOS  = 'A')
                         and XOSVCD = '278';
                       x1loss = x1loss;
                    Else;
                       x1loss = x1loss + p9totc;
                   endif;
                 endif;
                //
                //  Comparing conditions required
                //
                 if p9totp > 0
                    or (FRCMRI = 'P' and P9TOTP = 0)
                    or *in22 = *off
                    and *in23 = *off
                    and *in57 = *off
                    or frmaj = 'Y';
                    if p9dseq <> p2dseq;
                       If indCAPCSI = *On;
                          isAPCSI   = 'N';
                          strCAPCSI = *Blanks;
                          If EOast = 'MS';
                             Setgt (qqco#:P9cpt4:P2fdat) ppr01;
                             Readpe (qqco#:P9cpt4) ppr01;
                             If %Found(fmprl);
                                Select;
                                When EOast = 'MS';
                                   strCAPCSI = PR5a5;
                                EndSl;

                               If strCAPCSI = FRcapcSi;
                                  isAPCSI  = 'Y';
                               EndIf;
                             EndIf;
                          Else;
                             Setgt (P9cpt4:P2fdat) pap01;
                             Readpe (P9cpt4) pap01;
                             If %Found(ftapl);
                                strCAPCSI = APcSi;
                                If strCAPCSI = FRcapcSi;
                                  isAPCSI  = 'Y';
                                EndIf;
                             EndIf;
                          EndIf;
                          If isAPCSI  = 'N';
                             Iter;
                          EndIf;
                       EndIf;
                  //
                  //  Process bundled codes. They are grouped into categories a
                  //  only counted once. Manipulation, Performance testing and
                  //  hardening.
                  //
                  //  Don't process bundled codes if this is Major/Minor rule
                       if frmaj <> 'Y';
                    //
                          select;
                      // If were processing a bundled code and one from its cat
                      // has been processed, skip it.
                      //
                          when p9bund = 'M'
                           and xxman = 'Y';
                             Iter;
                          when p9bund = 'H'
                           and xxhard = 'Y';
                             Iter;
                          when p9bund = 'P'
                           and xxperf = 'Y';
                             Iter;
     //
     // If were processing a bundled code and its the first one in its
     // category turn on the appropriate flag and continue.
     //
                          when p9bund = 'M'
                           and xxman = 'N';
                             xxMan = 'Y';
                          when p9bund = 'H'
                           and xxhard = 'N';
                             xxHard = 'Y';
                          when p9bund = 'P'
                           and xxperf = 'N';
                             xxPerf = 'Y';
                          endsl;
                    //
                    // If bundled codes are in the same category don't count th
                    // billed code. Skip this is P2bund='Y'.
                    //
                     if p2bund = p9bund
                           and p2bund <> *blank
                           and p2bund <> 'Y';
                        count = count - 1;
                     endif;
                    //
                    // If this is another bundled code and you are counting pro
                    // then don't include in the count.
                     //
                     if p9bund='Y' and *in22=*on;
      /End-Free
     C     p9cpt4        lookup    ptarray                                99
      /Free
                        if *in99=*off and p9cpt4 <> p2cpt4;
                           j=1;
      /End-Free
     C     *blanks       lookup    ptarray(j)                             99
      /Free
                           If *in99 = *on;
                              ptArray(j) = P9cpt4;
                           Endif;
                        else;
                           Iter;
                        endif;
                     endif;
                  endif;

                  if frcpts = 'D';
      /End-Free
     C     p9cpt4        lookup    ptarray                                99
      /Free
                     if *in99=*off and p9cpt4 <> p2cpt4;
                        j=1;
      /End-Free
     C     *blanks       lookup    ptarray(j)                             99
      /Free
                        If *in99 = *on;
                           ptArray(j) = P9cpt4;
                        Endif;
                     else;
                        Iter;
                     endif;
                   endif;
                  //
                  //  If there is a modifier and this rule doesn't have one and
                  //  is not set to ignore compared to modifiers.
                  //
                   if frcmod = *blanks
                         and *in59 = *off;
                      if p9mod1 <> *blanks
                         or p9mod2 <> *blanks
                         or p9mod3 <> *blanks
                         or p9mod4 <> *blanks;
                         Iter;
                      endif;
                   endif;
                  //
                  //  If there is a modifier and but it doesn't match the rule
                  //
                   if frcmod <> *blank
                         and p9mod1 <> frcmod
                         and p9mod2 <> frcmod
                         and p9mod3 <> frcmod;
                      Iter;
                   endif;
                  //
                  //  Same CPT4 code
                  //
                   if frcpts = 'Y'
                         and p2cpt4 <> p9cpt4;
                      Iter;
                   endif;
                  //
                  //  Need to check CPT4 file to determine if compared to code
                  //  contains separate procedure flag = 'Y'.
                  //
                   if *in42 <> *off;
                      exsr sepp;
                    //
                      if xxsepp = 'N';
                         x1Sep = 'N';
                      endif;
                   endif;
                  //
                  //  Need to check asterisk procedure to determine if code con
                  //  asterisk procedure flag equal to 'Y'
                  //
                   if *in43 = *on;
                      exsr astrsk;
                      if xxastr <> 'Y';
                          x1Ast = 'N';
     //
     // Set X1AST flag for later use and exit the history search
     //
                      else;
                         x1Ast = 'Y';
                         i = 9998;
                      endif;
                   endif;
                   Monitor;
                   cxPos = %Dec(P9pos:3:0);
                   On-Error;
                   Endmon;
                   if *in13 = *on
                         and cxpos <> frcpos;
                      Iter;
                   endif;
                  //
                  //  Checking to make sure it isn't the same seq# and is paid
                  //
                  //  Checking # of days in past and future and intial treatmen
                  //  period
                  //
                   if *in20 = *on
                         and *in21 = *on;
                      if p9fdat < xxpst7
                           or p9fdat > xxfutr
                           or p9fdat < xxtrmt;
                         Iter;
                      endif;
                   endif;
                   //
                  //  Checking # of days in past only
                   //
                   if *in21 = *on
                         and *in20 = *off;
                      if p9fdat < xxpst7
                           or p9fdat > p2fdat;
                         Iter;
                      endif;
                   endif;
                   //
                  //  Checking # of days in Future only
                   //
                   if *in20 = *on
                         and *in21 = *off;
                      if p9fdat > xxfutr
                           or p9fdat < p2fdat;
                         Iter;
                      endif;
                   endif;
                   //
                  //  Checking Addon Code effective
                   //
                   if FRctpc = 'Y';
                      Setll (P2cpt4:P9cpt4) pao01;
                      ReadE (P2cpt4:P9cpt4) pao01;
                      DoW not %eof(fmaol1);
                        If P2fdat >= AOefdt and AOstat = 'A';
                           If AOdldt <> *zeros and AOdldt <> *loval;
                              If P2fdat < AOdldt;
                                 indPriCdFnd = *on;
                                 leave;
                              Endif;
                           Else;
                              indPriCdFnd = *on;
                              leave;
                           EndIf;
                        EndIf;
                        ReadE (P2cpt4:P9cpt4) pao01;
                      EndDo;
                      If indPriCdFnd = *on;
                         Iter;
                      EndIf;
                   endif;
                  //
                  //  Increment counter for hospital bills
                  //
                   if *in22 = *on and eobtyp = 'H' and
                         *in78 = *on

                         or *in22 = *on and ##splt = 'N';

                      exsr countunits;
                   else;
                      if *in22 = *on;
                         count = count + 1;
                      endif;
                   endif;                                                     //
                  //
                  //  Increment hours
                  //
                   if *in57 = *on and p9anes='T';
                      eval(H) hours = hours + (p9anmn * p9unts);
                      paid  = paid  + p9totp;
                   endif;
                   if *in86 = *on;
                      chain p9cpt4 php01;
                     //* If a hit then history line item is a HIPPS code
                      if %found;
                         tot_hipps_paid = tot_hipps_paid + p9totp;
                      endif;
                     // set EOB
                      if *in24 = *on;
                         //*** Beg Chg *** 07/18/23 **************************************
                         //*** xpEomb = freob;
                         xpEomb = FReobCd1;
                         xpEomb2 = FReobCd2;
                         //*** End Chg *** 07/18/23 **************************************
                         eombFl = 'Y';
                         xxst = frst;
                      endif;
                      Iter;
                   endif;
                   if *in84 = *on;
                      x9Mraa = P9mraa;
                      x9TotC = P9totc;
                      if *in22 = *on and frmaj <> 'Y'
                           and frcmri = 'D';
      /End-Free
     C     '51'          lookup    mod(1)                                 99
      /Free
                         if *in99 = *on;
                            Nxtrul = 'Y';
                            LeaveSr;
                         endif;
                      endif;
                     //
                    //  Call EOMB subroutine -- Determines what EOMB and error
                     //
                     exsr eomb;
                    //
                    //  Call PMNT subroutine -- Determines what to pay
                    //
                     exsr pmnt;
                     if proc = 'Y';
                        LeaveSr;
                     else;
                        if xxmmod = 'Y';
                           Nxtrul = 'Y';
                           LeaveSr;
                        endif;
                     endif;
                   endif;
                  //
                  //  If you are not accumulating dates of service or
                  //  number of procedures, and you are not setting up for
                  //  separate procedures and asterisk procedures and not summi
                  //  up total paid for stop-loss and not accumulating P/T time
                  //
                   if *in22 = *off
                         and *in23 = *off
                         and *in42 = *off
                         and *in43 = *off
                         and *in56 = *off
                         and *in57 = *off;
                    //
                    //  Call EOMB subroutine -- Determines what EOMB and error
                    //
                     exsr eomb;
                    //
                    //  Call PMNT subroutine -- Determines what to pay
                    //
                     exsr pmnt;
                    //
                     if proc = 'Y';
                        LeaveSr;
     //
     // If this rule is just for setting an EOMB code then move it here
     // and set a flag so it won't be replaced by another rule.
     //
                     else;
                        if *in24 = *on;
                           //*** Beg Chg *** 07/18/23 **************************************
                           //*** xpEomb = FReob;
                           xpEomb = FReobCd1;
                           xpEomb2 = FReobCd2;
                           //*** End Chg *** 07/18/23 **************************************
                           eombFl = 'Y';
      //
      // Store the rule state for use when getting EOB desc for CC (Corr cod)
      //
                           xxst = frst;
                        endif;
                     endif;
                  endif;
               endif;
            endif;
         endif;
         enddo;
         // If Add-on code and primary was found then skip the rule
         If FRcTpC = 'Y' and indPriCdFnd = *on;
            NxtRul = 'Y';
         Endif;
        endif;
       Endsr;
     //*******************************************************************
     //* ADJUST FOR HPSA CALCS - SUBROUTINE                    H P S A   *
     //*******************************************************************
       begsr hpsa;
         //
         // When paying professional component
         //
         if frpmr = 'Y' and p2prof <> 0;
            eval(H) amt1 = p2totc + (p2prof * .10);
            eval(H) amt2 = xcmra + (p2prof * .10);
         else;
            eval(H) amt1 = p2totc + (xcmra * .10);
            eval(H) amt2 = xcmra * 1.10;
         endif;
         if amt1 < amt2;
            xptota = amt1;
         else;
            xptota = amt2;
         endif;
         xcmra = xptota;
         p2mra = xptota;
         xptotd = p2totc - xptota;
         xptotp = xptota;
         xpmra = p2totc - xptota;
         %Subst(xxMsg:1:2) = qqst;
         %Subst(xxMsg:3:5) = '99995';
         xxErr = 'Y';

         exsr eomb;

         if xptotp > 0;
            xpEomb = '144';
            //*** Beg Add *** 07/18/23 ***************************
            xpEomb2 = '144';
            //*** End Add *** 07/18/23 ***************************
            eombFl = 'Y';
         Endif;                                                               //
       endsr;
      //*******************************************************************
      //* INITIALIZE FIELDS, COUNTERS, ETC - SUBROUTINE         I N I T   *
      //*******************************************************************
       begsr init;
         count = *zeros;
         clear date;
         x = *zeros;
         y = *zeros;
         w = *zeros;
         xptota = *zeros;
         xptota1 = *zeros;
         //
         // Only set the rule number for option 12 in F0390R if not CC.
         // We will not display Correct Coding rule. We use option 13 for that.
         //
         if frst <> 'CC';
            xxrule = frrule;
         endif;
          //
          // For once, delete existing entries to be re-written later
          //
          if isRulClr = *off;

             Setll (P2co#:P2sub#:P2fnd:P2fyr:P2case:P2eob#) prul01;
             Reade (P2co#:P2sub#:P2fnd:P2fyr:P2case:P2eob#) prul01;
             DoW not %eof(farull1);
                delete prul01;
                Reade (P2co#:P2sub#:P2fnd:P2fyr:P2case:P2eob#) prul01;
             EndDo;
             isRulClr = *on;
          endif;

          clear x1sep;
          clear x1ast;
          xxHard = 'N';
          xxMan =  'N';
          xxperf = 'N';
          xxloss = *zero;
          x1loss = *zero;
          xxtrmt = *zero;
          xxbund = *zero;
          hours = *zeros;
          clear ptarray;
          paid  = *zeros;
          xxpart = *zeros;
          xxeval = 'N';
       endsr;
      //*******************************************************************
      //* INITIALIZE CONVERSION FACTORS, ECT  SUBROUTINE         I N I T 1*
      //*******************************************************************
       begsr init1;
          xxmsurg = *zeros;
          xxconv = *zeros;
          x1conv = *zeros;
          xxtmdv = *zeros;
          xxaunt = *zeros;
          xxount = *zeros;
          xxccdf = *blank;
          xxccod = *blank;
          CCIred = *blank;
          xxccmd = *blank;
          xxErr = 'N';
          clear srdte;
          clear srerr;
          clear srterm;
          x1rule = *zeros;
          srfut = 0;
          srpst = 0;
          xxpass = 0;
          seqms = 1;
          clear xpeomb;
          //*** Beg Add *** 07/18/23 **************************************
          clear xpeomb2;
          //*** End Add *** 07/18/23 **************************************
          xxHist = '0';
          clear eombfl;
          clear xxovrd;
          clear kkcpt4;
          clear xxsvcd;
          x1pos = '00';
          x2pos = '00';
          xxdenyunits = ' ';
          clear tot_hipps_paid;
       endsr;
      //*******************************************************************
      //* LOAD DETAIL CHARGES INTO WORKFILE- SUBROUTINE         L O A D   *
      //*******************************************************************
       begsr load;
        //
        //  Get the user parameter file
        //
         chain upky pup01;
        //
        //  If the get was unsuccessful, end the program
        //
         if not %Found(smupl);
            FReob = 'XX0095';
            exsr eomb;
            Exsr PgmEnd;
         endif;
        //
        //  Get the Claim Master Record
        //
         chain cmky pcm01;
        //
        //  If the get was unsuccessful, end the program
        //
         if not %Found(fmcml1);
            FReob = 'XX0096';
            exsr eomb;
            ExSr PgmEnd;
         endif;
        //
        //   Call CL to CRTDUPOBJ OR CLR DUP in QTEMP To save time
        //
      /End-Free
     C                   call      'F4000C'
      /Free
         %Subst(feeSch:1:6) = 'F4000N';
         %Subst(feeSch:7:2) = qqSt;
      /End-Free
     //
     //   Send state and EOB# to determine all line items for EOMB
     //
     C                   call      feesch
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
      /Free
       Endsr;
     //*******************************************************************
     //* LOAD FIELDS FROM PCAWK2 TO PCAWK1                    L O A D F  *
     //*******************************************************************
       begsr loadf;
         P2co# = P3co#;
         P2sub# = P3sub#;
         P2fnd = P3fnd;
         P2fyr = P3fyr;
         P2case = P3case;
         P2CsCd = P3CsCd;
         P2fdat = P3fdat;
         P2tdat = P3tdat;
         P2rdat = P3rdat;
         P2ipop = P3ipop;
         P2fein = P3fein;
         P2mod = P3mod;
         P2prTy = P3PrTy;
         P2prt = P3prt;
         P2trCd = P3trCd;
         P2svcd = P3svcd;
         P2Pmod = P3pmod;
         P2cpt4 = P3cpt4;
         P2icd9 = P3icd9;
         P2anmn = P3anmn;
         P2Anes = P3anes;
         P2unts = P3unts;
         P2totc = P3totc;
         P2tota = P3tota;
         P2totd = P3totd;
         P2Eomb = P3eomb;
     //
     // Need to clear total paid for rules to work properly, i.e. rules
     // that condition on P9TOTP
     //
         P2totp = *zeros;
         P2dseq = p3dseq;
         P2delf = p3delf;
         P2flag = p3Flag;
         P2chk# = p3chk#;
         P2eob# = p3eob#;
         P2part = p3part;
         P2p1 = P3p1;
         P2ss# = P3ss#;
         P2acdt = P3acdt;
         P2stat = P3stat;
         P2user = P3user;
         P2pgm = P3pgm;
         P2time = P3time;
         P2dt = P3dt;
         P2iUsr = P3iUsr;
         P2idt = P3idt;
         P2dxr = P3dxr;
         P2dcs# = P3dcs#;
         P2ltr# = P3ltr#;
         P2rcFl = P3rcFl;
         P2othr = P3othr;
         P2ndis = P3ndis;
         P2rdTp = P3rdTp;
         P2brSv = P3brSv;
         P2brPm = P3brPm;
         P2brPc = P3brPc;
         P2bram = P3bram;
         P2brun = P3brun;
         P2brtc = P3brtc;
         P2zipA = P3zipA;
         P2aSt = P3aSt;
         P2diff = P3diff;
         P2brPt = P3brPt;
         P2bAst = P3bAst;
         P2daFl = P3dafl;
         P2mra = P3mra;
         P2brEc = P3brEc;
         p2brp = p3brp;
         P2type = P3type;
         P2rpt = P3rpt;
         P2fsR = P3fsR;
         P2facf = P3facf;
         P2invl = P3invl;
         P2pos = P3pos;
         P2sepp = P3sepp;
         P2spCd = P3spCd;
         P2amt = P3amt;
         P2prof = P3prof;
         P2tech = P3tech;
         P2anam = P3anam;
         P2folu = P3folu;
         P2surg = P3surg;
         P2supp = P3supp;
         P2bAst = P3bAst;
         P2asc = P3asc;
         P2md50 = P3md50;
         P2md51 = P3md51;
         P2md54 = P3md54;
         P2md55 = P3md55;
         P2md56 = P3md56;
         P2md62 = P3md62;
         P2md66 = P3md66;
         P2md80 = P3md80;
         P2md81 = P3md81;
         p2md82 = p3md82;
         P2hTos = P3hTos;
         P2nct = P3nct;
         P2ruDt = P3ruDt;
         P2ndc = P3ndc;
         P2mod1 = P3mod1;
         P2mod2 = P3mod2;
         P2mod3 = P3mod3;
         P2mod4 = P3mod4;
         P2pGrp = P3pGrp;
         P2lTrm = P3lTrm;
         P2hTrm = P3hTrm;
         P2lSty = P3lSty;
         P2pPt1 = P3pPt1;
         P2pPt2 = P3pPt2;
         P2pPt3 = P3pPt3;
         P2pPt4 = P3pPt4;
         P2pPt5 = P3pPt5;
         P2pPt6 = P3pPt6;
         p2lq51 = p3lq51;
       Endsr;
     //*******************************************************************
     //* LOAD CASE HISTORY INTO MULTIPLE OCCUR DATA STRUCTURE L O A D H  *
     //*******************************************************************
      /Free
       begsr loadh;
         clear totchg;
         clear mdsp9;
         u = 1;
         n2 = 1;
         clear totChgGA;
    
         %Occur(mdsp9) = n2;
     //================================================================
     // Chain to specific auxiliary files depending on the type of bill
     // except Pharmacy bills in which we will chain out later in the
     // program because there is no field to hold NDC
     //================================================================
     //
     // Physician Bill
     //
         select;
         when eobtyp = 'N';
            setll dhky2 pdh02;
            reade dhky2 pdh02;
            *in61 = %Eof;
            xxdseq = d9dseq;
           //
           // Dental Bill
           //
         when eobtyp = 'D';
            setll ddky1 pdd01;
            reade ddky1 pdd01;
            *In61 = %Eof;
            xxdseq = dddseq;
           //
           // Hospital Bill
           //
         when eobtyp = 'H';
            setll duky2 pdu01;
            reade duky2 pdu01;
            *in61 = %Eof;
            xxdseq = dudseq;
         other;
            LeaveSr;
         endsl;
        //
        // Load the multiple occurance data structure
        //
         dow *in61 = *off
               and n2 <= 9999;
            chain caky9 pca09;
            *in60 = Not %Found;
            select;
            //================================================================
            // Physician Bill|
            //================================================================
             when eobtyp = 'N';
            //
            // Load the Procedure code/mod from the auxiliary file to P9 file
            //
                P9cpt4 = D9prCd;
                P9mod1 = D9mod1;
                P9mod2 = D9mod2;
                P9mod3 = D9mod3;
                P9mod4 = D9mod4;
                P9pos = %Subst(D9pos:2:2);
                P9unts = D9unts;

                reade dhky2 pdh02;
                *in61 = %Eof;

                xxdseq = d9dseq;
            //================================================================
            // Dental Bill|
            //================================================================
              when eobtyp = 'D';
                 P9cpt4 = DDprCd;
                 reade ddky1 pdd01;
                 *IN61 = %EOF;
                 xxdseq = dddseq;
            //================================================================
            // Hospital Bill|
            //================================================================
              when eobtyp = 'H';
                 P9cpt4 = DUrate;
                 P9svCd = DUrvCd;
                 p9unts = duunts;
                 P9pos = DUpos;
                 reade duky2 pdu01;
                 *IN61 = %EOF;
                 xxdseq = dudseq;
              endsl;
          //
          // Increment the counter to position to next DS occurance
          // Process only if you got a hit on FDCAP (PCA09)
          //
              if *in60 = *off;
            //
            // Dont include allowed services in total charges
            //
                 if p9cpt4 >= '70000' and p9cpt4 <= '79999'
                       or p9cpt4 >= '97001' and p9cpt4 <= '98943';
                    totChgGA = totChgGA + p9totc;
                 endif;
              if p9eob# = qqeob#;
                 totchg = totchg + p9totc;
                 if p9svcd =  001;
                    xxtot001 = p9totc;
                 endif;
              endif;                                                          //
              exsr lodfu;
            //
            // Determine if this code is bundled and move Y to P9BUND
            //
              P9bund = 'N';
              P9cMri = 'N';
              P9anes = ' ';
              P9ovRd = ' ';
              P9cCas = 'N';
              P9ovRd = ' ';
              exsr popflg;
              n2 = n2 + 1;
              if n2 + 1 <= 9999;
                 %Occur(mdsp9) = n2;
              endif;
           endif;
         enddo;
        //
        // Set the counter for use in CALC subroutine
        //
         n2 = n2 - 1;
       Endsr;
     //*******************************************************************
     //* LODFU  SUBROUTINE -- LOADS FOLLOW UP DAY ARRAYS     L O D F U   *
     //*******************************************************************
       begsr lodfu;
         if p9rcfl <> 'Y';
          //
          //  Read from the CPT-4 File
          //
           setgt prky3 ppr01;
           readp ppr01;
          //
          // Only load follow up days for the same provider/modifier
           if not %Eof(fmprl);
              if p9cpt4 = prcpt4
                   and p9fein = qqfein
                   and p9mod = qqmod
                   and prfolu > *zero
    
                   or p9cpt4 = prcpt4
                   and p9fein = qqfein
                   and p9mod = qqmod
                   and pr1ap <> *blank;
                   srdti = p9fdat;
      /End-Free
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         srdate
     //
     //  Calculate follow period using follow up days
     //
      /Free
                 srfutr = prfolu;
      /End-Free
     C                   call      'SRFUTR8'
     C                   parm                    srdate
     C                   parm                    srfutr
     C                   parm      ' '           srrtcd
      /Free
                 srdti = srdate;
      /End-Free
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hlddat
      /Free
                 if u >= 1 and U <= 500;
                    fuf(u) = p9fdat;
                    fut(u) = hlddat;
                    flg(u) = pr1ap;
                    fudseq(u) = p9dseq;
                    u = u + 1;
                 endif;
              endif;
            //
            //  Move correct field from FMPRP for each state
            //
           endif;
         endif;
         if p9rcfl = 'Y';
          //
          //  Read from the R & C File
          //
            setgt rcky3 prc02;
            readp prc02;
           //
           // Only load follow up days for the same provider/modifier
           //
            if not %Eof(fmrcl);
               if p9cpt4 = rccpt4
                      and p9fein = qqfein
                      and p9mod = qqmod
                      and rcfolu > *zero;
                  srdti = p9fdat;
      /End-Free
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         srdate
     //
     //  Calculate follow period using follow up days
     //
      /Free
                  srfutr = rcfolu;
      /End-Free
     C                   move      ' '           srrtcd
     C                   call      'SRFUTR8'
     C                   parm                    srdate
     C                   parm                    srfutr
     C                   parm                    srrtcd
      /Free
                  srdti = srdate;
      /End-Free
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hlddat
      /Free
                  if u >= 1 and U <= 500;
                     fuf(u) = p9fdat;
                     fut(u) = hlddat;
                     u = u + 1;
                  endif;
               endif;
            endif;
         endif;
       endsr;
     //
     //*******************************************************************
     //* LoadMod Subroutine                         L O A D M O D        *
     //*******************************************************************
       begsr LoadMod;
         //
         // Load modifiers into array for processing in PMOD subroutine
         //
          if p2type = 'P';
             mod(1) = p2mod1;
             mod(2) = p2mod2;
             mod(3) = p2mod3;
             mod(4) = p2mod4;
             %Subst(mod1:1:2) = mod(1);
             %Subst(mod1:3:2) = mod(2);
             %Subst(mod1:5:2) = mod(3);
             %Subst(mod1:7:2) = mod(4);
           //
           // If this is a rule for multiple modifiers see if there is more
           // than 1 modifier in the array MOD
           //
             l = 0;
             mod2 = *blanks;
             For K = 1 to 4;
                if mod(k) <> *blank;
                   l = l + 1;
                   if mod(k) = '26' or mod(k) = '27'
                     or mod(k) = 'TC';
                      mod2 = mod(k);
                   endif;
                endif;
             endfor;
             if l = 1;
                oneMod = 'Y';
             else;
                oneMod = 'N';
             endif;
          endif;
       Endsr;
      //*******************************************************************
      //* LOAD RULES INTO MULTIPLE OCCUR DATA STRUCTURE       L O A D R   *
      // ******************************************************************
       begsr loadr;
         clear mdsru;
         n = 1;
        //
        // State Rules - Build key to load data struct highest to lowest pty
        //
         kkSt = qqSt;
         kkSys = P2type;
         kkPty = *hival;

         %Occur(mdsru) = n;
        //
        // Load the multiple occurance data structure unless client doesnt get
        // this rule or it has been inactivated
         //
         setgt frky pfr01;
         readpe frky1 pfr01;
         dow not %Eof(fdfrl5)
               and n <= 9999;
            if frclt# = 0 and frstat <> 'I' or
                  frclt# = xxclt# and frstat <> 'I';
             //
             // Special case for Florida ASC bills since they are entered as
             // physician bills.  We dont want to load physician rules eff 9/4/0
             //
               if p2fdat >= 1050904;
                  if eoast = 'FL' and eospcd = 'ASC' and
                        eobtyp = 'N' and frbmri <> 'A';
                     clear pfr01;
                     readpe frky1 pfr01;
                     Iter;
                  endif;
               endif;
             //
             // Special case for Florida ASC bills since they are entered as
             // hospital bills.  We dont want to load physician rules eff 7/8/10
             //
               if p2fdat >= 1100708;
                  if eoast = 'FL' and eospcd = 'ASC' and
                        eobtyp = 'H' and frbmri <> 'A';
                     clear pfr01;
                     readpe frky1 pfr01;
                     Iter;
                  endif;
               endif;
             //
             // If not splitting lines then set rule flags to handle multiple un
             //
               if ##splt='N';
                  frmunt='Y';
               endif;
               n = n + 1;
               %Occur(mdsru) = n;
            else;
               clear pfr01;
            endif;

            readpe frky1 pfr01;

         enddo;
        //
         // If client has Correct Coding installed load those rules last
        //
         if ##coco = 'Y';
          //
          // Build key to load data structure highest to lowest priority
          //
            kkSt = 'CC';
            kkSys = P2type;
            kkpty = *hival;
     //
     // Get rule date for Correct Coding Rules
     //
            setgt frky1 pfr01;
            readpe frky2 pfr01;
            if not %Eof(fdfrl5);
               kkefdt = frefdt;
            else;
               kkefdt = 0;
            endif;

            %Occur(mdsru) =  n;

            setgt frky pfr01;
            readpe frky1 pfr01;
          //
          // Load the multiple occurance data structure
          //
            dow not %Eof(fdfrl5)
                  and n <= 9999;
               if frclt# = 0 and frstat <> 'I' or
                    frclt# = xxclt# and frstat <> 'I';
                  n = n + 1;
                  %Occur(mdsru) = n;
               else;
                  clear pfr01;
               endif;

               readpe frky1 pfr01;

            enddo;
            clear pfr01;
         endif;
        //
        // Save the counter for use in RULES subroutine
        //
         n1 = n - 1;
        Endsr;
     //*******************************************************************
     //* LOAD MODIFIERS                                                  *
     //*******************************************************************
      /Free
       begsr loadm;
        //
        // Reload modifiers if this is a CCI rule in case they were blanked
        // Out when processing the state rules
        //
         if frst = 'CC' and xxccmd = ' ';
            Mod(1) = P2mod1;
            Mod(2) = P2mod2;
            Mod(3) = P2mod3;
            Mod(4) = P2mod4;
            %Subst(mod1:1:2) = mod(1);
            %Subst(mod1:3:2) = mod(2);
            %Subst(mod1:5:2) = mod(3);
            %Subst(mod1:7:2) = mod(4);
            l = 0;
            mod2 = *blanks;
            For k = 1 to 4;
               if mod(k) <> *blank;
                  l = l + 1;
               endif;
            endfor;
            if l = 1;
               oneMod = 'Y';
            else;
               oneMod = 'N';
            endif;
            xxccmd = 'Y';
         endif;
       endsr;
      //*******************************************************************
      //* MultRule Subroutine                  M U L T R U L E            *
      //*******************************************************************
       begsr MultRule;
        //
        // Write to multi redcution rule file
        //
             RULco# = P2co#;
             RULsub# = P2sub#;
             RULfnd = P2fnd;
             RULfyr = P2fyr;
             RULcase = P2case;
             RULeob# = P2eob#;
             RULdseq = P2dseq;
             RULrule = FRrule;
             RULdesc = FRdesc;
             RULiusr = q1user;
             RULipgm = q1pgm;
             RULidt = %dec(%char(%date():*cymd0):7:0);
             RULitim = %dec(%char(%time():*iso0):6:0);
             RULuser = q1user;
             RULpgm = q1pgm;
             RULdt = %dec(%char(%date():*cymd0):7:0);
             RULtime = %dec(%char(%time():*iso0):6:0);

             write prul01;

       endsr;
      //*******************************************************************
      //* CHECK MODIFIER INDICATORS                                       *
      //*******************************************************************
       begsr mod_ind;
         clear xxtop;
         //
         // PC/TC indicator
         //
         if frpcin <> *blanks and p2pcin <> frpcin;
            xxtop = 'Y';
            LeaveSr;
         endif;                                                               //
         //
         // Bilat surg indicator
         //
         if fr50in <> *blanks and p250in <> fr50in;
            xxtop = 'Y';
            LeaveSr;
         endif;                                                               //
         //
         // Multi surg indicator
         //
         if fr51in <> *blanks and p251in <> fr51in;
            xxtop = 'Y';
            LeaveSr;
         endif;                                                               //
         //
         // Co surg indicator
         //
         if fr62in <> *blanks and p262in <> fr62in;
            xxtop = 'Y';
            LeaveSr;
         endif;                                                               //
        //
         // Team surg indicator
         //
         if fr66in <> *blanks and p266in <> fr66in;
            xxtop = 'Y';
            LeaveSr;
         endif;                                                               //
        //
         // Asst surg indicator
         //
         if fr80in <> *blanks and p280in <> fr80in;
            xxtop = 'Y';
            LeaveSr;
         endif;                                                               //
       Endsr;
     //*******************************************************************
     //* MATCH BILLED MODIFIERS TO THIS RULE                             *
     //*******************************************************************
       begsr mod_check;
        //
         clear xxtop;
        //
         if frmr = 'Y';
            xxMmod = FRmr;
         endif;
        //
        // If this is a rule for multiple modifiers and NOT A CORRECT CODING
        // rule then see if ther is more than 1 modifier in the array MOD.
        //
         if frmr = 'Y'
               and onemod = 'Y'
               and frst <> 'CC';
            clear xxmmod;
         endif;
         *In99 = *on;
         //
         // If their is a modifier but the rule is not looking for one then skip
         // the rule
         //
         if mod1 <> *blanks and frbfmd = *blanks
               and frbtmd = *blanks
               and frbmd2 = *blanks and frbmd3 = *blanks
               and frbmd4 = *blanks;
            xxtop = 'Y';
            LeaveSr;
         endif;
         //
         // Check when no modifiers are present
         //
         if mod1 = *blanks;
            if frbfmd <> *blanks or frbmd2 <> *blanks
                  or frbmd3 <> *blanks or frbmd4 <> *blanks;
               xxtop = 'Y';
               LeaveSr;
            endif;
           //
           // Check when there are modifiers are present to see if any fit the
           // rule
         else;
          //
          // This rule is looking for one or more modifiers
          //
            if frbtmd <> *blanks or frbmd2 <> *blanks
                  or frbmd3 <> *blanks or frbmd4 <> *blanks;
                *in99 = *on;
                if frbtmd <> *blanks and *in99 = *on;
                   if mod(1) >= frbfmd and mod(1) <= frbtmd
                         and mod(1) <> *blanks  or

                         mod(2) >= frbfmd and mod(2) <= frbtmd
                         and mod(2) <> *blanks  or

                         mod(3) >= frbfmd and mod(3) <= frbtmd
                         and mod(3) <> *blanks  or

                         mod(4) >= frbfmd and mod(4) <= frbtmd
                         and mod(4) <> *blanks;
                   else;
                      *in99 = *off;
                   endif;
               endif;
               if frbmd2 <> *blanks and *in99 = *on;
                  if mod(1) = frbmd2 or mod(2) = frbmd2 or
                        mod(3) = frbmd2 or mod(4) = frbmd2;
                  else;
                     *IN99 = *OFF;
                  endif;
               endif;
               if frbmd3 <> *blanks and *in99 = *on;
                  if mod(1) = frbmd3 or mod(2) = frbmd3 or
                        mod(3) = frbmd3 or mod(4) = frbmd3;
                  else;
                     *IN99 = *OFF;
                  endif;
               endif;
               if frbmd4 <> *blanks and *in99 = *on;
                  if mod(1) = frbmd4 or mod(2) = frbmd4 or
                        mod(3) = frbmd4 or mod(4) = frbmd4;
                  else;
                     *in99 = *off;
                  endif;
               endif;
            else;
               *in99 = *off;
            endif;                                                            //
         endif;
        //
        // None of the modifiers satisfy this rule
        //
         if *in99 = *off;
            xxtop = 'Y';
            LeaveSr;
         endif;
        //
        // If a modifier is not valid for the CPT code then p2amt would = 0
        // but p2mra (fs allowance) would not = 0
        //
         if frval = 'V' and mod1 = *blanks
               or frval = 'V' and p2amt > 0
               or frval = 'V' and p2mra = 0;
            xxtop = 'Y';
            LeaveSr;
         endif;

       Endsr;
     //*******************************************************************
     //* CLEAR MODIFIERS USED BY THIS RULE                               *
     //*******************************************************************
       begsr modused;
          if frbtmd <> *blanks
               or frbmd2 <> *blanks or frbmd3 <> *blanks
               or frbmd4 <> *blanks;
             i = 1;
             dow i <= 4;
                if mod(i) <> *blanks and mod(i) >= frbfmd
                   and mod(i) <= frbtmd
                   or mod(i) <> *blanks and mod(i) = frbmd2
                   or mod(i) <> *blanks and mod(i) = frbmd3
                   or mod(i) <> *blanks and mod(i) = frbmd4;

                   mod(i) = *blanks;
                   eval l = l - 1;
                endif;
                i = i + 1;
             enddo;
          endif;
       endsr;
     //*******************************************************************
     //* MultLine                            M U L T L I N E             *
     //*******************************************************************
      /Free
       begsr MultLine;
         //
         // Check to see if multi-line or not
         //
      /End-Free
     C                   call      'SA014R'
1    C                   parm      eoast         ##st              2
2    C                   parm      eobtyp        ##btyp            1
3    C                   parm      p2fdat        ##fdat            7 0
4    C                   parm      *blank        ##splt            1
      /Free
       Endsr;
      //****************************************************************
      //* MULTIPLE UNITS PROCESS NEXT RULE                MULTUNITRULE
      //****************************************************************
       begsr multunitrule;
         //
         // If the rule denies some of the units then keep processing the next
         // rule.
         //
         if ##splt = 'N' and *in28 = *on and
               frcfcd <> *blanks and frctcd <> *blanks
               and unitcnt > 0
               and frfud <> 'Y';
            xxMmod = 'Y';
            xptota1 = p2mra;
            xxdenyunits = 'Y';
         Endif;
       endsr;
     //*******************************************************************
     //* NoRuleDate Subroutine            N O R U L E D A T E            *
     //*******************************************************************
       begsr NoRuleDate;
          //
         // If rule date = 999999 then give message and go to next item
          //
          xxErr = 'N';
          if p2rudt = 9999999 and CSovRd <> 'Y';
             %Subst(xxMsg:1:2) = qqst;
             %Subst(xxMsg:3:5) = '99998';
             xxErr = 'Y';
             exsr eomb;
          //
          // Set work fields to deny the charges and update FDCAP record.
          //
             xptota = *zero;
             xptotd = p2totp;
             exsr updat;
          endif;
       Endsr;
      //****************************************************************
      //* Calculate allowance for one unit
      //****************************************************************
       begsr Oneunitallow;
          xptota_for_unit = xcmra;
          if frbfmd = '26';
             xptota_for_unit = p2prof;
          endif;
          if frbfmd = 'TC' or frbfmd = '27';
             xptota_for_unit = p2tech;
          endif;
       endsr;
     //****************************************************************
     //* Pay Multipl Units              P A Y M U L T U N I T S
     //****************************************************************
      /Free
       begsr paymultunit;
         if *in69 = *off;
           capaid=unitCnt*xptota;
         endif;
         //
         // If we are paying based on charges then dont calculate using units
         //
         if frpch <> ' ';
            capaid=xptota;
         endif;
         if *in28 = *on and frcfcd <> *blank
               and frctcd <> *blank;
            capaid=unitCnt * xptota_for_unit;
           xptota=capaid;
         Endif;
       //
       // PAY MRA EVEN IF GREATER THAN CHARGED
       //
        If capaid > catotc and *in53 = *off;
           capaid = catotc;
        Endif;
        camra=catotc-capaid;
        xpmra=camra;
        xptotp=capaid;
        CAbrUn = unitCnt;
       Endsr;
      //*******************************************************************
      //* CALCULATION SUBROUTINE FOR PHARMACY                             *
      //*******************************************************************
       begsr phar;
         kkCpt4 = P2ndc;
     //
     // Chain to the Pharmacy Bill Detail record
     //
         xxdseq = p2dseq;
         chain dpky pdp01;
         if dprxpt = 'S';
            kkCpt4 = DPhcpc;
         endif;
         kkefdt = p2rudt;
         xxErr = 'N';
         srdte = *zeros;
         clear srerr;
         clear srterm;
         x1rule = *zeros;
         clear xpeomb;
         //*** Beg Add *** 07/18/23 **************************************
         clear xpeomb2;
         //*** End Add *** 07/18/23 **************************************
         xxpass = 0;
        //
        // If claim status codes is not valid for fee schedule skip
        //
         clear tabkey;
         tabKey = P2csCd;
         chain tcsky pcs01;
         xxOvRd = CSovRd;
     //
     //* A hard error will cause the program to skip this line
     //
         if not %found(ftabl);
            FRmsg = 'XX0097';
            exsr eomb;
            LeaveSr;
         endif;
         //
         // Process override type
         //
         if xxovrd = 'Y';
          //
          // Use possible duplicate criteria based on system control file
          // field FBAL05 pos(7) loaded into ##TDUP in SA009R.
          // ' ' is standard duplicate logic
          // '1' is for client 25
          // '2' is for client 114 and is like standard but won't check same bi
          //
           select;
           when ##tdup = ' ';
             exsr dup;
           when ##tdup = '1';
             exsr dup1;
           when ##tdup = '2';
             exsr dup;
           other;
             exsr dup;
           endsl;
           exsr updat;
           LeaveSr;
         endif;

         if xxpass < 1;
             xxpass = 1;
            n = 1;
         Endif;

        //
        // Repeat calculations as long as there are rules
        //
         dow n <= n1;
    
           %Occur(MdsRu) = n;
    
           exsr rules;

           n = n + 1;

           If Match = 'Y';
           //
          //  Initialize counters
           //
           count = *zeros;
           //
           // Only set the rule number for option 12 in F0390R if not CC.
           // We will not display Correct Coding rule. We use option 13 for that
           //
           if frst <> 'CC';
             xxrule = frrule;
           endif;
          //
          // For once, delete existing entries to be re-written later
          //
          if isRulClr = *off;

             Setll (P2co#:P2sub#:P2fnd:P2fyr:P2case:P2eob#) prul01;
             Reade (P2co#:P2sub#:P2fnd:P2fyr:P2case:P2eob#) prul01;
             DoW not %eof(farull1);
                delete prul01;
                Reade (P2co#:P2sub#:P2fnd:P2fyr:P2case:P2eob#) prul01;
             EndDo;
             isRulClr = *on;
          endif;
           x = *zeros;
           y = *zeros;
           w = *zeros;
           xptota = *zeros;
           hours = *zeros;
           paid  = *zeros;
          //
          // Check for the # of days between Carrier Received Date and Latest S
          //  Date in the Bill, If # of days is 95 days or less then allow disa
          // the bill.
          //
           if *in81 = *On;
    
             if EORCN = 0;
              //
              // Fetch the latest to service date from FDCAL27
              //
              //
               srdate1 = p2fdat;
      /End-Free
     C                   call      'SRDDIF7'
     C                   parm                    srdate1           7 0
     C                   parm      EOCRDT        srdate2           7 0
     C                   parm      *Zeros        sr#days           5 0
     C                   parm      *Blanks       srerr             1
      /Free
               exsr eomb;
               if sr#days <= fr#dsd or srerr <> '0';
                  Iter;
               endif;
               exsr pmnt;
               if proc = 'Y';
                  LeaveSr;
               endif;
             endif;
           endif;
          //
          //  If you are not accumulating dates of service or
          //  Number of procedures, and you are not setting up for
          //  seperate procedures and asterisk procedures.
          //
           if *in22 = *off
                 and *in23 = *off
                 and *in42 = *off
                 and *in43 = *off;
            //
            //  Call EOMB Subroutine -- Determines what EOMB and error message
            //
             exsr eomb;
            //
            //  Call payment subroutine -- determines what to pay
            //
             exsr pmnt;
             if proc = 'Y';
                LeaveSR;
             endif;
           endif;
          //
          //  Call EOMB subroutine -- determines what EOMB and error message
          //
           if *in16 = *off
                 and *in17 = *off
                 and *in22 = *off
                 and *in23 = *off
                 and *in51 = *off
                 and *in57 = *off;
                 exsr eomb;
                 exsr pmnt;
                 if proc = 'Y';
                    LeaveSr;
                 endif;
                 Iter;
           endif;
          //
          //  Call payment subroutine -- determines what to pay
          //  If you are accumulating dates of service or
          //  number of procedures
          //
           if *in22 = *on
                 or *in23 = *on
                 or *in57 = *on;
              exsr eomb;
              exsr pmnt;
              if proc = 'Y';
                 LeaveSr;
              endif;
           endif;
         Endif;
         enddo;
        //
        //  Default with payment of MRA
        //
       If SetPay <> 'Y';
          setPay = 'Y';
          *in26 = *on;
          exsr pmnt;
       Endif;

       EndSr;
     //
     //*******************************************************************
     //* PgmEnd Subroutine                               P G M E N D     *
     //*******************************************************************
       begsr PgmEnd;
          //
          // Set the EDI flags in the Header Files for FL
          //
          if eoast = 'FL';
      /End-Free
     C                   call      'FA740R'
     C                   parm      dbrrn1        ##rrn             9 0
     C                   parm                    ##rtcd            1
      /Free
          endif;
          xcmra = 0;
          *inlr = *on;
          Return;
       Endsr;
     //*******************************************************************
     //* PgmStr Subroutine                               P G M S T R     *
     //*******************************************************************
       begsr PgmStr;
     //
     //== Initialization Section ======================================
     //
      /End-Free
     C                   time                    dsdttm
      /Free
       srdti = dsdate;
      /End-Free
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         sydate
      /Free
      //
      //  INITIALIZE WORK FIELDS
      //
       clear qqs;
       clear qqc;
       clear amt1;
       clear amt2;
       clear bxpos;
       clear count;
       clear feesch;
       clear fxbfcd;
       clear fxbtcd;
       clear fxcfcd;
       clear fxctcd;
       clear kkefdt;
       clear kkcpt4;
       clear kkpty;
       clear kkst;
       clear kksys;
       clear proc;
       clear rule;
       clear seqms;
       clear tabkey;
       clear u;
       clear w;
       clear xocode;
       clear xxastr;
       clear xxaunt;
       clear xxconv;
       clear x1conv;
       clear xxcpt4;
       clear xxerr;
       clear xxfutr;
       clear xxmsg;
       clear xxpass;
       clear xxpast;
       clear xxpst7;
       clear xxpmod;
       clear xxsepp;
       clear xxsvcd;
       clear xxtmdv;
       clear xxunt;
       clear xxunts;
       clear xxymd;
       clear x1ast;
       clear x1cpt4;
       clear x1msg;
       clear x1rem;
       clear x1rule;
       clear x1sep;
       clear x1svcd;
       clear x1unts;
       clear x2msg;
       clear x2rem;
       clear xxanmn;
       clear xxftm;
       clear xptemp;
       clear xxount;
       clear xocode;
       clear xptota;
       clear fuf;
       clear fut;
       clear flg;
       clear fudseq;
       xxrudt = *zero;
       xxPas2 = *off;
       clear *in;
       clear xxrule;
       clear xxtot001;
       zzloss = *zero;
      //
      // Call system control file API to determine what possible
      // duplicate criteria to use.
      //
      /End-Free
     C                   call      'SA009R'
 1   C                   parm      *zeros        ##eobd
 2   C                   parm      ' '           ##eobi
 3   C                   parm      ' '           ##arin
 4   C                   parm      ' '           ##acis
 5   C                   parm      ' '           ##pay0
 6   C                   parm      ' '           ##sdts
 7   C                   parm      ' '           ##mciv
 8   C                   parm      ' '           ##pbiv
 9   C                   parm      ' '           ##ainv
10   C                   parm      ' '           ##whpg
11   C                   parm      ' '           ##priv
12   C                   parm      ' '           ##tdup
     //
     // Call system control file API to determine if Correct Coding
     // Module is installed and get the client number
     //
     C                   call      'SA001R'
 1   C                   parm      *blank        ##wcif
 2   C                   parm      *blank        ##mcif
 3   C                   parm      *blank        ##wcin
 4   C                   parm      *blank        ##ar
 5   C                   parm      *blank        ##ntwk
 6   C                   parm      *blank        ##ur
 7   C                   parm      *blank        ##med
 8   C                   parm      *blank        ##edi
 9   C                   parm      *blank        ##wcdk
10   C                   parm      *blank        ##rhab
11   C                   parm      *blank        ##mss
12   C                   parm      *blank        ##fsch
12   C                   parm      *blank        ##prvb
12   C                   parm      *blank        ##npwc
12   C                   parm      *blank        ##os
12   C                   parm      *blank        ##arls
12   C                   parm      *blank        ##coco
12   C                   parm      *blank        ##crvw
12   C                   parm      *blank        ##prom
12   C                   parm      *blank        ##mcrv
12   C                   parm      *blank        ##freq
12   C                   parm      *blank        ##crac
12   C                   parm      *blank        ##preo
12   C                   parm      *blank        ##parn
12   C                   parm      *blank        ##mgri
12   C                   parm      *blank        ##kcif
12   C                   parm      *blank        ##curs
12   C                   parm      *blank        ##pcib
12   C                   parm      *blank        ##clt#
      /Free
          Monitor;
          xxClt# = %Dec(##clt#:3:0);
          On-Error;
          Endmon;
      //
      // Find the recon number.  If this is a recon.
      //
       chain eoky peo01;

       if %Found(faeol4);
          xxrcn = eorcn;
       else;
          xxrcn = *zero;
       endif;
    
       if %Found(faeol4);
          Select;
         //
         //  Get Dental Header information
         //
          When eobtyp = 'D';
             chain(n) edkey ped01;
         //
         //  Get Physician Header information
         //
          When eobtyp = 'N';
             chain(n) ehkey peh01;
         //
         //  Get Pharmacy Header information
         //
          When eobtyp = 'X';
             chain(n) epkey pep01;
         //
         //  Get Hospital Header information
         //
          When eobtyp = 'H';
             chain euky peu01;
          Endsl;
       Endif;                                                                 //
       Endsr;
      //*******************************************************************
      //* DETERMINE PAYMENT AND CALL UPDAT SUBROUTINE                     *
      //*******************************************************************
      /Free
       begsr pmnt;
         rule = 'N';
         Proc = 'N';
     //
     // Initially load XCMRA with P2MRA
     //
         xcmra = p2mra;
        //
        //  Determine if code is an ANES Code and if it needs to be calculated
        //  but not if this is a Correct Coding rule
        //
         if frst <> 'CC';
           if p2anes = 'Y'
                 and xxconv > 0
                 and xxtmdv > 0
                //
                 or p2anam > 0
                 and xxconv > 0
                 and xxtmdv > 0
                //
                 or p2anam > 0
                 and xxconv > 0
                 and xxtmdv > 0
                 and xxaunt > 0;
             exsr anes;
             //
            // Load load XCMRA with value calculated in Anesthesia sub
             //
             xcmra = p2amt;
           endif;
         endif;                                                               //
        //
        //  Use CPT-4 Procedure code file or R & C file
        //
         if frpcpt <> *blanks;
            exsr prcd;
          //
          // Load load XCMRA with value calculated in PRCD subroutine
          //
            xcmra = p2mra;
         endif;
        //
        // Adjust payment when you are NOT paying MRA
        //
         if *in26 = *off;
          //
          // Pay the Billed amount
          //
           if *in27 = *on;
              xptota = p2totc;
              xcmra = xptota;
           endif;
          //
          // Pay percent from provider service code information
          //
           If *in27 = *on;
              If FRpSrv = 'Y';
                 if p2scam > 0;
                    xptota = p2scam * p2totc;
                 else;
                    xptota = p2mra * p2totc;
                 endif;
                 xptota = xptota / 100;
                 xcmra = xptota;
              endif;
           endif;
          //
          // Pay fixed amount from the state rule
          //
           if *in30 = *on;
             xptota = framt;
             xcmra = xptota;
           endif;
          //
          // Pay amount calculated in Anesthesia subroutine
          //
           if *in33 = *on;
             xptota = p2amt;
             xcmra = xptota;
           endif;
          //
          // Pay the Professional component amount
          //
           if *in38 = *on;
              xptota = p2prof;
              xcmra = xptota;
           endif;
          //
          // Pay the Technical component amount
          //
           if *in39 = *on;
              xptota = p2tech;
              xcmra = xptota;
              if p2type = 'H';
                 xptota = xptota * p2unts;
                 xcmra = xptota;
              endif;
           endif;
          //
          // Pay the ASC facility fee
          //
           if *in44 = *on;
              xptota = p2facf;
              xcmra = xptota;
           endif;
          //
          // Calculate Inpatient hospital per diem codes using MRA
          //
           if *in45 = *on;
              xptota = p2mra * p2unts;
              xcmra = xptota;
           endif;
          //
          // Calculate Inpatient hospital per diem codes using fixed amount
          //
           if *in45 = *on
                 and *in30 = *on;
              xptota = framt * p2unts;
              xcmra = xptota;
           endif;
          //
          // Calculate per diem and stop loss calculation for AR state
          //
           if *in45 = *on
                 and frtlos = 'A';
      /End-Free
     C                   call      'SRDDIF7'
     C                   parm      euaddt        srdate1
     C                   parm      eudcdt        srdate2
     C                   parm      *zeros        sr#days
     C                   parm      *blanks       srerr
      //
      /Free
              xptota = framt * (sr#days + 1);
              z1loss = x1loss - xptota;
              if z1loss - frloss > 0;
                 xptota = xptota + .80 * (z1loss - frloss);
              endif;
              xcmra = xptota;
           endif;
           if *in45 = *on
                 and frppdm = 'I';
              kkicd9 = p2icd9;
              chain diky pdi01;
              *IN60 = NOT %FOUND;
              If *in60 = *off;
                 k1Icd9 = DIds1;
              Endif;
              kkdate = p2rudt;
              chain rhky prh01;
              *IN60 = NOT %FOUND;
              If *in60 = *on;
                 k1Icd9 = '99999';
                 Chain rhky prh01;
              Endif;
      /End-Free
     C                   call      'SRDDIF7'
     C                   parm      euaddt        srdate1
     C                   parm      eudcdt        srdate2
     C                   parm      *zeros        sr#days
     C                   parm      *blanks       srerr
      /Free
              if EUDCDT < 1120101;
                 select;
                 when p2zipa = '601';
                   if rh601 <> 0;
                      xptota = (sr#days + 1) * rh601;
                   else;
                      xptota = totchg * rhp601/100;
                   endif;
                 when p2zipa = '606';
                    if rh606 <> 0;
                       xptota =  (sr#days + 1) * rh606;
                    else;
                       xptota = totchg * rhp606/100;
                    endif;
                 endsl;
              else;
                 if eudcdt < 1121231;
                    if rh601 <> 0;
                       xptota = (sr#days + 1) * rh601;
                    else;
                       xptota = totchg * rhp601/100;
                    endif;
                 endif;
             endif;
       If Eudcdt >= 1130101;
          Chain p1ky1 pp101;
          If %found(Fmp1l);
             If %subst(P1cnty:10:6) = 'REHAB2';
                If Rh601 <> 0;
                   Xptota = (sr#days + 1) * rh601;
                Else;
                   Xptota = totchg * rhp601/100;
                Endif;
             Endif;

             If %subst(P1cnty:10:6) = 'REHAB3';
                If Rh606 <> 0;
                   Xptota = (sr#days + 1) * rh606;
                Else;
                   Xptota = totchg * rhp606/100;
                Endif;
             Endif;
          Endif;
       Endif;
             xcmra = xptota;
       Endif;
           //
          // Calculate hourly fee
           //
           if *in47 = *on;
              if *in30 = *on;
                 xptota = framt * p2unts;
              else;
                 xptota = p2anam * p2unts;
              endif;                                                          //
              xcmra = xptota;
           endif;
          //
          // Calculation for pain management process
          //
           if *in83 = *on;
              xptota = p2anam * xxconv;
              xcmra = xptota;
           endif;
          //
          // Calculate rental and mileage
          //
           if *in52 = *on
                 or *in31 = *on;
              xptota = p2anam * p2unts;
              xcmra = xptota;
            //
            // Calculate mileage based on rural/urban flag
            //
              if *in74 = *on;
                 if frcon2 = 0;
                    select;
                    when p2area = 'U';
                       xptota = p2anam * p2unts;
                    when p2area = 'R';
                       if p2unts <= frconv;
                          xptota = p2prof * p2unts;
                       else;
                          xptota = (p2prof * frconv) + p2tech
                           * (p2unts - frconv);
                       endif;
                    endsl;
                 endif;
               //
               //** If we are executing rules that have both a lower mileage rat
               //** then for urban mileage they get the base rate + the increase
               //** For rural or super rural they get the same rate plus the inc
               //** miles.
               //
               If frcon2 > 0;
                  select;
                  when p2area = 'U';
                     if p2unts <= frcon2;
                        xptota = p2anam * p2unts;
                     else;
                        xptota = (p2anam * frcon2) + p2lq51
                          * (p2unts - frcon2);
                     endif;
                  when p2area = 'R' or p2area = 'S';
                     if p2unts <= frconv;
                        xptota = p2prof * p2unts;
                     endif;
                     if p2unts >= frconv and p2unts <= frcon2;
                        xptota = (p2prof * frconv) + pranam
                          * (p2unts - frconv);
                     endif;
                     if p2unts >= frcon2;
                        xptota = (p2prof * frconv) + pranam *
                         (frcon2 - frconv) + p2lq51 *
                         (p2unts - frcon2);
                     endif;
                  endsl;
               endif;
               xcmra = xptota;
             endif;
           endif;
          //
          // Disallow payment
          //
           if *in28 = *on;
              exsr Oneunitallow;
              xptota = *zeros;
              xcmra = xptota;
           endif;
         endif;
        //
        //  Pay a percentage but not if we already calculated stop loss A
        //
         if *in29 = *on
               and frtlos <> 'A';
          //
          // Calculate percentage based on indicators set in RULES
          //
            xptemp = p2mra * frpct;
            if *in27 = *on;
               xptemp = p2totc * frpct;
            endif;
            if *in46 = *on;
               xptemp = prmra * frpct;
            endif;
            if *in38 = *on;
               xptemp = p2prof * frpct;
            endif;
            if *in39 = *on;
               xptemp = p2tech * frpct;
               if p2type = 'H';
                  xptemp = p2tech * frpct * p2unts;
               endif;
            endif;
            if *in33 = *on;
               xptemp = p2amt * frpct;
            endif;
            if *in44 = *on;
               xptemp = p2facf * frpct;
            endif;
            xptemp = xptemp / 100;
            if frrnd = 'D';
               exsr roundup;
            endif;
            if frcon2 > 0;
               if *in33 = *on;
                  xptemp = xptemp * frcon2;
                  xptemp = xptemp / 100;
               endif;
            endif;
            if *in30 = *on
                  and *in26 = *on;
               xptemp = xptemp + framt;
            endif;
          //
          // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
          // Paying a percent of ASC.
          // Initially set up for Florida providers billing with ASC spec
          // If ASC = 0 then pay % of charged.
          // If ASC > 0 then if charged < ASC then take a percent of charged
          // and pay the lesser of that and ASC. If charged >= ASC then take
          // a % of charged a pay the greater of that or ASC.
          //
           If *in32 = *on;
             If P2asc > 0;
                if p2totc <= p2asc;
                   xptemp = p2totc * frpct;
                   xptemp = xptemp / 100;
                   if p2asc < xptemp;
                      xptemp = p2asc;
                   endif;
                else;
                   xptemp = p2totc * frpct;
                   xptemp = xptemp / 100;
                   if p2asc > xptemp;
                      xptemp = p2asc;
                   endif;
                endif;
             else;
                xptemp = p2totc * frpct;
                xptemp = xptemp / 100;
             endif;
           endif;
          //
          // Calculate Hospital DRG amount based on Peer Group of Provider
          // If charged is within high and low trim value pay MRA else pay
          // percent based on Peer group in this rule
          //
           if *in49 = *on
                 and *in27 = *on;
              if p2totc < p2ltrm
                   or p2totc > p2htrm;
                 xptemp = p2totc * frpct;
                 xptemp = xptemp / 100;
              endif;
           endif;
          //
          // Pay Outlier days -  Divide the charged amount by #days.  Then
          // mult # of outlier days * this amount * a percentage.  Add this
          // to the DRG average. Outlier days= #days-length of stay threshold
          //
           if *in50 = *on;
              select;
              when frpout = 'Y';
                 xxprdm = p2totc / p2unts;
                 xxoutl = p2unts - p2lsty;
                 if xxoutl < 0;
                    xxoutl = 0;
                 endif;
                 xxoutl = xxoutl * xxprdm;
                 xptemp = xxoutl * frpct;
                 xptemp = xptemp / 100;
                 xptemp = xptemp + p2mra;
              when frpout = '1';
                 xxoutl = p2totc - p2mra - 40000;
                 if xxoutl > 0;
                    xxoutl = xxoutl * (frpct/100);
                 else;
                    xxoutl = 0;
                 endif;                                                       //
              //
                 xptemp = p2mra + xxoutl;
              when frpout = '2';
                 if p2totc <= p2mra;
                    xptemp = p2totc;
                 else;
                    if p2totc <= 2.5 * p2mra;
                       xptemp = p2mra;
                    else;
                       xxoutl = p2totc - (2.5 * p2mra) * .75;
                       xptemp = p2mra + xxoutl;
                    endif;                                                    //
                 endif;                                                       //
              when frpout = 'I';
      /End-Free
     C                   call      'FA764R'
 1   C                   parm      dbrrn1        ##rrn
 2   C                   parm      p2fdat        ##svdt            7 0
 3   C                   parm                    ##alow            9 2
 4   C                   parm      frpct         ##perc            5 2
 5   C                   parm      ' '           ##rtcd
     //
      /Free
                 xptemp = ##alow;
              endsl;                                                          //
           endif;
          //
          // Pay XPTEMP + % of ( Difference between Charged-XPTEMP)
          //
           if *in55 = *on;
              if p2totc > xptota;
                 xptemp = p2totc - xptota;
                 xptemp = xptemp * frpct;
                 xptemp = xptemp / 100;
                 xptemp = xptemp + xptota;
              //
              // Put the total allowed calculated above into XPTEMP
              //
              else;
                 xptemp = xptota;
              endif;
           endif;
          //
          //  Pay the lesser of billed or percent of MRA unless *IN53=ON
          //  which allows payment to be greater than charged amount
          //
           xptota1 = xptemp;
           if xptemp >= p2totc
                 and *in53 = *off
                 and p2mra <> 0;
             xptota = p2totc;
           else;
            //
            //  Pay the lesser of percent of billed or MRA, but not less than
            //  percent of MRA
            //
              if xptota > xptemp
                   and xptemp <> 0
                   or xptota < xptemp
                   and xptemp <> 0;
                 xptota = xptemp;
              endif;
            //
           endif;
          //
          //  Pay charged up to the allowance for the primary procedure
          //
           if *in84 = *on;
              if x9totc <= x9mraa;
                 x9mraa = x9totc;
              endif;
              if p2totc * (frpct/100) >= x9mraa;
                 xptota = x9mraa;
              else;
                 if *in29 = *on;
                    xptota = p2totc * (frpct/100);
                 else;
                    xptota = p2totc;
                 endif;
             endif;
             if P2Mra > 0 and P2Mra < xpTota;
                xptota = P2Mra;
             Endif;
           endif;
           //
           //  Pay more than charged for this line but not more than charged amo
           //  for the entire bill
           //
           if FRpCh = 'T';
              totchg = totchg - totChgGA;
           Endif;
           if *in72 = *on and xptemp >= totchg;
              xptota = totchg;
           endif;                                                             //
           if *in72 = *on and xptemp < totchg;
              xptota = xptemp;
           endif;                                                             //
           xcmra = xptemp;
         endif;
        //
        // Adjust payment based on MRA or AWP
        //
         if *in26 = *on
               or *in71 = *on;
          //
          // Multiply by a percent (used in pharmacy calcs)
          //
           if *in29 = *on;
              xptemp = p2mra * frpct;
              p2mra = xptemp / 100;
              if frrnd = 'D';
                 eval(H) xptemp2 = p2mra * 1;
                 p2mra = xptemp2;
              endif;
              xptota1 = p2mra;
              xcmra = p2mra;
           endif;
          //
          // Adjust payment based on Average Wholesale Price (AWP)
          //
           if *in71 = *on;
              p2mra = p2awp;
              xcmra = p2mra;
            //
            // Multiply by a percent
            //
              if *in29 = *on;
                  xptemp = (p2awp * p2unts) * frpct;
                 p2mra = xptemp / 100;
                 xptota1 = p2mra;
                 xcmra = p2mra;
              //
              // Dont multiply by a percent
              //
              else;
                 xptemp = (p2awp * p2unts);
                 p2mra = xptemp;
                 xptota1 = p2mra;
                 xcmra = p2mra;
              endif;
           endif;                                                             //
          //
          // Pay the MRA plus an additional fee if not paying a percent
          //
           if *in30 = *on;
              p2mra = p2mra + framt;
              xcmra = p2mra;
           endif;
           xptota = p2mra;
           xptota1 = p2mra;
         endif;
         //
        //  Pay the lesser of billed or percent of MRA unless *IN53=ON
        //  which allows payment to be greater than charged amount
        //
        //  If there is a conversion amount and the code is not a anes
        //  code, multiply the MRA against the conversion factor.
        //
         if p2anes <> 'Y'
               and p2anam = 0
               and xxconv > 0;
            xptota = p2mra * xxconv;
            xcmra = xptota;
            xptota1 = xptota;
         endif;
         //
         // Pay the provider service code amount from the charge master
         //
         if *in34 = *on;
            xptota = p2scam*p2unts;
            xcmra = xptota;
         endif;
         //
         // Pay according to DRG rates for inpatient bills
         //
         if *in35 = *on;
            If *IN84 = *on and TotChg > *Zero and TotChg < P2drg;
               xptota = TotChg;
            Else;
              xptota = P2drg;
            Endif;
            xcmra = xptota;
         endif;
         //                                                                 ****
         // Pay each line up to total charged for the bill for HIPPS code.(Each
         // amount from REV code 0001 line as lines do not allow individual char
         // The total paid amount for all HIPPS code cannot exceed REV '001' lin
         //
         if *in86 = *on;
            if (xxtot001 - tot_hipps_paid) >=  p2mra;
               xptota = p2mra;
            else;
               xptota = (xxtot001 - tot_hipps_paid);
            endif;
            xcmra = xptota;
         endif;
         //
         // Pay according to skilled nurse rates for inpatient bills
         //
         if *in36 = *on;
           //
           // Calculate length of stay to be lesser of revenue code units vs.
           // statement from/thru dates.
           //
            x2unts = p2unts;
            srdti = euscpf;
      /End-Free
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxscpf
      //
      /Free
           if srerr = *off;
              srdti = euscpt;
      /End-Free
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxscpt
      /Free
              if srerr = *off;
      /End-Free
     C                   call      'SRDDIF8'
     C                   parm      xxscpf        qqdate1
     C                   parm      xxscpt        qqdate2
     C                   parm      0             qqdiff
     C                   parm      *blank        qqerr
      /Free
                 qqdiff = qqdiff + 1;
                 if qqerr <> *on and qqdiff < x2unts
                     and qqdiff <> 0;
                    x2unts = qqdiff;
                 endif;
              endif;
           endif;
           xptota = p2snfi * x2unts;
           xcmra = xptota;
         endif;
         //
         // Pay according to skilled nurse rates for outpatient bills
         //
         if *in37 = *on;
            xptota = p2snfo * p2totc;
            xcmra = xptota;
         endif;
         //
         // Pay according CORF amount
         //
         if *in63 = *on;
            xptota = p2corf;
            xcmra = xptota;
         endif;
         //
         // Pay according Home Health Inpatient rates
         //
         if *in64 = *on;
            xptota = p2hhli *p2unts;
            xcmra = xptota;
         endif;
         //
         // Pay according Home Health Outpatient rates
         //
         if *in65 = *on;
            xptota = p2hhlo * p2unts;
            xcmra = xptota;
         endif;
         //
         // Pay according to Outpatient RCC ratio
         //
         if *in66 = *on;
            xptota = p2occr;
            xcmra = xptota;
         endif;
         //
         // Pay according to Outpatient RCC ratio for pharmacy
         //
         if *in67 = *on;
            xptota = (p2porc * p2totc * frpct)
               / 100;
            xcmra = xptota;
         endif;
         //
         // Pay according to Outpatient RCC ratio for pharmacy
         //
         if *in68 = *on;
            xptota = (p2pirc * p2totc * frpct)
               / 100;
            xcmra = xptota;
         endif;
         //
         // Pay according to Physical Therapy Per Visit Rates
         //
         if *in73 = *on;
            xptota = (p2ptpv * p2unts);
            xcmra = xptota;
         endif;
         //
         // Pay according to PT/OT indiv or phys owned by number of visits
         //
         if *in80 = *on;
            select;
            when frpptv = '1';
               xptota = p2ppt1;
            when frpptv = '2';
               xptota = p2ppt2;
            when frpptv = '3';
               xptota = p2ppt3;
            when frpptv = '4';
               xptota = p2ppt4;
            when frpptv = '5';
               xptota = p2ppt5;
            when frpptv = '6';
               xptota = p2ppt6;
            endsl;
            xcmra = xptota * p2unts;
         endif;
         //
         // Pay MRA times Units (HCPCS codes)
         //
         if *in69 = *on;
            xptota2 = (p2mra * p2unts);
            if xxdenyunits = 'Y';
               xptota2 = (p2mra * unitcnt);
            endif;
          //
          // Multiply by a percent
          //
            if *in29 = *on;
               xptota2 = (xptota2 * frpct)/100;
            endif;
           //
           // Hospital: Make payment based on how many units the rule allows
           //
            if eobtyp = 'H' and *in22 = *on and
                 *in78 = *on
                 or *in22 and ##splt = 'N' and *in28;
               if count >= fr#prc;
                  xptota2 = 0;
               else;
                  xptota2 = p2mra * (fr#prc - count);
               endif;                                                         //
            endif;                                                            //
            xpTota = xpTota2;
      //
      // Pay according to PT/OT indiv or phys owned by number of visits
      //
            if *in80 = *on and eobtyp = 'H';
               select;
               when frpptv = '1';
                  xptota = p2ppt1;
               when frpptv = '2';
                  xptota = p2ppt2;
               when frpptv = '3';
                  xptota = p2ppt3;
               when frpptv = '4';
                  xptota = p2ppt4;
               when frpptv = '5';
                  xptota = p2ppt5;
               when frpptv = '6';
                  xptota = p2ppt6;
               endsl;
               xcmra = xptota * p2unts;
               xptota = xcmra;
            endif;
         endif;
         //
         // Pay Intra-op(54), Post-op(55), and Pre-op(56)
         //
         if *in77 = *on;
            if frpmr = 'I';
               xptota = p2mra * p2md54;
               xcmra = xptota;
            endif;
            if frpmr = 'O';
               xptota = p2mra * p2md55;
               xcmra = xptota;
            endif;
            if frpmr = 'R';
               xptota = p2mra * p2md56;
               xcmra = xptota;
            endif;
         endif;                                                               //
         //
         // Pay according to the relative unit value for the remaining units
         //
         if *in57 = *on and frpmr = 'V';
            if paid  < (p2conv * frpttm) + xxpart;
               eval(H) xptota = p2conv * frpttm;
               xptota = xptota - paid + xxpart;
               if p2mra * p2unts < xptota;
                  eval(H) xptota = p2mra * p2unts;
               endif;
            else;
               eval(H) xptota = 0;
            endif;
            xcmra = xptota;
         endif;
        //
        // Calculate Hospital DRG amount based on Peer Group of Provider
        // If charged is within high and low trim value pay MRA.
        //
         if *in49 = *on;
           if p2totc >= p2ltrm
                 and p2totc <= p2htrm;
             xptota = p2mra;
             xcmra = p2mra;
           endif;
         endif;
         if *in46 = *on and *in29 = *on
               and *in30 = *on;
            xptota = ((xxmra + framt) * frpct)/100;
         endif;
         //
        //  Pay the lesser of billed or percent of MRA unless *IN53=ON
        //  which allows payment to be greater than charged amount
        //  Load XPTOTA1 in case of multiple modifier reductions
         //
        //  Load XPTOTA1 in case of multiple modifier reductions if not loaded
         //
         if xptota1 = 0 and xptota <=9999999.99;
            xptota1 = xptota;
         endif;
         if xptota1 = 0 and xptota > 9999999.99;
            xptota1 = 9999999.99;
         endif;
         //
         // If we are calculating stop loss type A then use the eligible
         // charges as the line item charge
         //
         xxtotc = p2totc;
         if frtlos = 'A';
            xxtotc = x1loss;
         endif;
         if xxtotc < xptota
               and *in53 = *off
               and *in72 = *off
               and *in86 = *off
               and *in35 = *off
               and *in84 = *off;
            xptota = xxtotc;
         endif;
         //
         //  Pay more than charged for this line but not more than charged amoun
         //  for the entire bill
         //
         if FRpCh = 'T';
            totchg = totchg - totChgGA;
         Endif;
         if *in72 = *on and xptota >= totchg;
            xptota = totchg;
         endif;                                                               //
        //
        //  Determine if payment has been set or dissallowed to finish
        //  and blank out EOMB unless special conditions exist:
        //   1. State = FL
        //   2. Rules indicates to apply EOB Code always, i.e. FRbmri = V
        //
         if xptota >= xxtotc
               and xptota > *zero
               and eoast <> 'FL'
               and frbmri<> 'V';
            clear xpeomb;
            //*** Beg Add *** 07/18/23 **************************************
            clear xpeomb2;
            //*** End Add *** 07/18/23 **************************************
         endif;
        //
        //  Only update if no further there are no further reductions
        //  and if we are not calculating anesthesia percentage.
        //
         if p2anes <> 'M';
           if frcon2 > 0
                 and *in33 = *off;
              amt1 = frconv * p2mra;
              amt2 = frcon2 * p2unts;
              xptota = amt1 + amt2;
              xcmra = xptota;
            //
            //  Load XPTOTA1 in case of multiple modifier reductions
            //
              xptota1 = xptota;
              if xxtotc < xptota
                   and eoast <> 'FL';
               clear xpeomb;
               //*** Beg Add *** 07/18/23 **************************************
               clear xpeomb2;
               //*** End Add *** 07/18/23 **************************************
               xptota = xxtotc;
             endif;
           endif;
         Endif;
        //
        // If this rule uses mod 51 then set xxmsurg = 'Y'
        //
         if frbfmd = '51' or frbmd2 = '51' or
               frbmd3 = '51' or frbmd4 = '51' or
               fr51in = '1' or fr51in = '2'
               or frbfmd = 'SG' or frbmd2 = 'SG'
               or frbmd3 = 'SG' or frbmd4 = 'SG'
               or frbtmd = '51';
           if frpct > 0 and frmr = 'Y';
              xxmsurg = 'Y';
            endif;
         endif;
         //
         // Store MRA reductions from FS
         //
         if frst <> 'CC';
            xpmra = p2totc - xptota;
         endif;
         if xptota <> 0 and xxcc <> ' '
               and xxccdo = 'N'
              //
              //   CCI Bypass Line Items
              //
               and FRBMRI <> 'X';

            xxccdo = 'Y';
            @xxccdo = 'Y';
         endif;
        //
        // If there are no more reduction rules, and payment was set or
        // disallowed then update and process the next procedure code unles
        // Correct Coding rules need to be processed
         //
         // Count modifiers in this rule
         //
         #rulmods = 0;
         if frbtmd <> *blanks;
            #rulmods = #rulmods + 1;
         endif;
         if frbmd2 <> *blanks;
            #rulmods = #rulmods + 1;
         endif;
         if frbmd3 <> *blanks;
            #rulmods = #rulmods + 1;
         endif;
         if frbmd4 <> *blanks;
            #rulmods = #rulmods + 1;
         endif;
         if frst <> 'CC';
            exsr calcunit;
            exsr multunitrule;
           //
           // Write the rule to the multiple rule file
           //
            Exsr MultRule;
           //
           // Calculate CAMRA for multiple units
           //
            if *in57 = *off and (p2anes = *blank or p2anes = 'T' and
                frpmr <> 'V')
                 and eobtyp = 'N'
                 and ##splt = 'N';
               xptota1 = unitCnt*xptota;
                 if *in28 = *on and frcfcd <> *blank
                   and frctcd <> *blank;
                    xptota1 = unitCnt * xptota_for_unit;
                 Endif;
                if xptota1 > p2totc;
                   xptota1 = p2totc;
                endif;
                xpmra = p2totc-xptota1;
            endif;
         endif;
         if xxmmod <> 'Y'
               and xxccdo <> 'Y'

               or xxmmod = 'Y' and l = #rulmods
               and frbtmd <> *blanks
               and xxccdo <> 'Y'
               and unitcnt = 0;

            if setpay = 'Y';
               setPay = 'N';
               xptotd = p2totc - xptota;
               xptotp = xptota;
             //
            // Use possible duplicate criteria based on system control file
            // field FBAL05 pos(7) loaded into ##TDUP in SA009R.
             //
               select;
               when ##tdup = ' ';
                  exsr dup;
               when ##tdup = '1';
                  exsr dup1;
               other;
                  exsr dup;
               endsl;
               exsr updat;
               proc = 'Y';
            endif;
          //
          //  Set P2MRA for processing multiple reductions or CCI rules
           //
         else;
           //
           // Skip down to the CCI rules if applicable
           //
            if xxccdo = 'Y' and xxmmod <> 'Y'
                 or xxccdo = 'Y' and xxmmod = 'Y'
                 and l = #rulmods and mod1 <> *blanks
                 and FRbtMd <> *blanks;
               exsr cciskip;
               xxccdo = ' ';
            endif;
            p2amt = xpTotA1;
            p2mra = xptota1;
            proc = 'N';
      //
      // Blank out the modifiers that were used for this rule so it will not
      // use them again
      //
            if xxdenyunits <> 'Y';
               exsr modused;
            endif;
         endif;
         EndSr;
     //
     //*******************************************************************
     //* DETERMINE IF MODIFIER NEEDS TO BE CALCULATED  M O D I F I E R   *
     //*******************************************************************
       begsr pmod;
        //*******************************************************************
         ModRul = 'N';
     //
     // Does the billed modifier fall into the range of this rule
     //
         if mod(1) >= frbfmd
               and mod(1) <= frbtmd
               and frval = 'Y'
    
               or mod(2) >= frbfmd
               and mod(2) <= frbtmd
               and frval = 'Y'
    
               or mod(3) >= frbfmd
               and mod(3) <= frbtmd
               and frval = 'Y'
    
               or mod(4) >= frbfmd
               and mod(4) <= frbtmd
               and frval = 'Y';
          //
          // Determine if the modifier has a value and if so skip this rule
          //
            if frval = 'Y'
                  or frpcvl = 'Y'
                  or frtcvl = 'Y';
               select;
               when frbfmd = '22'
                     and p2prof > 0
                     and p2tech > 0;
                  ModRul = 'Y';
                  LeaveSr;
               when frbfmd = '50'
                     and p2md50 > 0;
                  ModRul = 'Y';
                  LeaveSr;
               when frbfmd = '51'
                     and p2md51 > 0;
                  ModRul = 'Y';
                  LeaveSr;
               when frbfmd = '52'
                     and p2prof > 0
                     and p2tech > 0;
                  ModRul = 'Y';
                  LeaveSr;
               when frbfmd = '54'
                     and p2md54 > 0;
                  ModRul = 'Y';
                  LeaveSr;
               when frbfmd = '55'
                     and p2md55 > 0;
                  ModRul = 'Y';
                  LeaveSr;
               when frbfmd = '56'
                     and p2md56 > 0;
                  ModRul = 'Y';
                  LeaveSr;
               when frbfmd = '62'
                     and p2md62 > 0;
                  ModRul = 'Y';
                  LeaveSr;
               when frbfmd = '66'
                     and p2md66 > 0;
                  ModRul = 'Y';
                  LeaveSr;
               when frbfmd = '80'
                     and p2md80 > 0;
                  ModRul = 'Y';
                  LeaveSr;
               when frbfmd = '81'
                     and p2md81 > 0;
                  ModRul = 'Y';
                  LeaveSr;
               when frbfmd = '82'
                     and p2md82 > 0;
                  ModRul = 'Y';
                  LeaveSr;
               when frbfmd = '26'
                     and p2prof > 0;
                  ModRul = 'Y';
                  LeaveSr;
               when frbfmd = '27'
                     and p2tech > 0
                   or frbfmd = 'TC' and p2tech > 0;
                  ModRul = 'Y';
                  LeaveSr;
               when frbfmd = 'NU'
                     and p2mra > *zero;
                  ModRul = 'Y';
                  LeaveSr;
               when frbfmd = 'RR'
                     and p2anam > *zero;
                  ModRul = 'Y';
                  LeaveSr;
               when frbfmd = 'UE'
                     and p2mra > *zero;
                  ModRul = 'Y';
                  LeaveSr;
              //
              //  If anesthesia by a surgeon must have a value in the anes fiel
               //
               when frbfmd = '47'
                     and p2anam > *zero;
                  ModRul = 'Y';
                  LeaveSr;
               endsl;
           endif;
         endif;
       Endsr;
     //*******************************************************************
     //* POPBRC SUBROUTINE -- POPULATE CABRTC             P O P B R C    *
     //*******************************************************************
       begsr popbrc;
        //
        // Load with state fee schedule allowed
        //
         xpbrtc = xcmra;
        //
        // Use charged if mod D1-D5 (Parenteral/Enteral) are present
        //
         if p2mod1 >= 'D1'
               and p2mod1 <= 'D5'
    
               or p2mod2 >= 'D1'
               and p2mod2 <= 'D5'
    
               or p2mod3 >= 'D1'
               and p2mod3 <= 'D5'
    
               or p2mod4 >= 'D1'
               and p2mod4 <= 'D5';
    
            if xpbrtc > *zeros;
               hltotc = xpbrtc + p2mra;
               if hltotc = p2totc;
                  p2totc = xpbrtc;
               endif;
            endif;
            xpbrtc = p2totc;
         endif;
        //
        // Default to 0 if payment was denied
        //
         if xptotp = 0;
            xpbrtc = 0;
         endif;
        //
        // By-Report if no modifier then set to 0 else set to MRA
        //
         if p2rpt = 'Y';
            if mod1 = *blanks;
               xpbrtc = 0;
            else;
               xpbrtc = xcmra;
            endif;
         endif;
         //
         // If non Fee Schedule reimbursable then use charged amount.
        //
         if p2fsr = 'N' and p2mra = 0;
            xpbrtc = p2totc;
         endif;
       endsr;
      //*******************************************************************
      //* POPFLG Subroutine                                               *
      //*******************************************************************
       begsr popflg;
         //
         //  Set flag to indicate this is a line item that was overriden
         //
         clear tabkey;
         tabKey = p9cscd;
         chain tcsky pcs01;
         If %Found(ftabl);
            P9ovRd = CSovRd;
         Endif;
     //
     //  Read from the CPT-4 File
     //
      /Free
         setgt prky1 ppr01;
         readp ppr01;
         if not %Eof(fmprl)
               and p9cpt4 = prcpt4;
            P91ah = PR1ah;
            P91af = PR1af;
     //
     //  Move FMPRPXX fields to MDSP9 based on state.
     //
            select;
            when qqst = 'FL';
               P9cMri = PR1a4;
            when qqst = 'GA';
               P9cMri = PR1a4;
            when qqst = 'MS'
                  or qqst = 'CA';
               P9cCas = PR1a4;
               P9cMri = PR1a3;
            when qqst = 'NE'
                  or qqst = 'TX';
               P9bund = PR1a3;
            endsl;
            P9anes = PR1a2;

            If P9anes = 'T';
               P9anMn = PRanAm;
            Endif;
            if qqst = 'NY' and p9cpt4 >= '97001'
                  and p9cpt4 <= '97800';
               if ##splt='N';
                  p9anmn = pr92ob*p9unts;
               else;
                  p9anmn = pr92ob;
               endif;
            endif;
            P9enBa = PR5a2;
         else;
          //
          //  Read from the R & C File
          //
            setgt rcky1 prc02;
            readp prc02;
            if not %Eof(fmrcl)
                  and p9cpt4 = rccpt4;
            //
            //  Move FMRCP fields to MDSP9 based on state.
            //
               select;
                //
               when qqst = 'MS';
                  P9cMri = RC1a3;
               when qqst = 'NE'
                     or qqst = 'TX';
                  P9bund = RC1a3;
               endsl;
               P9anes = RC1a2;
               P9anMn = RCanAm;
            endif;
         endif;
       endsr;
      //*******************************************************************
      //* PRCD  SUBROUTINE -- READS FROM CPT-4 OR R & C        P R C D    *
      //*******************************************************************
       begsr prcd;
        //
        //  Read from the CPT-4 File
        //
         setgt prky ppr01;
         readp ppr01;
         if not %Eof(fmprl);
            if eoast = 'TX' and p2fdat >= 1030801;
      /End-Free
     C                   call      'FA739R'
     C                   parm      qqco#         ##co#
     C                   parm      frpcpt        ##cpt4
     C                   parm      p2fdat        ##fdat
     C                   parm      qqst          ##ast
     C                   parm      qqfein        ##fein
     C                   parm      qqmod         ##mod
     C                   parm      eoflic        ##flic
     C                   parm      p2plac        ##pos
     C                   parm      *zeros        ##mra
      //
      /Free
               p2mra = ##mra;
               xxmra = ##mra;
            else;
               p2mra = prmra;
               xxmra = prmra;
            endif;
            *in26 = *on;
     //
     //  Move correct field from FMPRP for each state
     //
         else;
          //
          //  Read from the R & C File
          //
            setgt rcky prc02;
            readp prc02;
            if not %Eof(fmrcl);
               p2mra = rcmra;
               *in26 = *on;
            endif;
         endif;
       endsr;
      //
      //*******************************************************************
      //* ROUNDUP SUBROUTINE -- ROUND UP TO NEAREST DOLLAR R O U N D U P  *
      //*******************************************************************
       begsr roundup;
          eval(H) xptemp2 = xptemp * 1;
          eval(H) xptemp = xptemp2;
       endsr;
      //*******************************************************************
      //* RULES SUBROUTINE -- SETS INDICATORS                  R U L E    *
      //*******************************************************************
       begsr rules;
     //
     // Process rules while we still have a match
     //
           Match = ' ';
           Dow Match <> 'Y';
     //
     // Clear indicators set by previous rule
      //
           exsr clrind;

           SetPay = 'N';
           clear xxmmod;
           x1pos = '00';
           x2pos = '00';
          //
          // Reload modifiers if this is a CCI rule
          //
           exsr loadm;
          //===============================================================
           // Billed Information
          //===============================================================
          //
          //    Billed HPSA eligible
          //
          if frbfhe = 'Y' and p2bfhe <> 'Y';
              Match = 'N';
             Leave;
          endif;
           //
          // Billed Specialty code
          //
           *in01 = frspcd > *blank;
           if *in01 = *on;
             if frspcd = 'ALL'
                   and p2spcd = *blanks;
                Match = 'N';
                Leave;
             else;
               if frspcd <> 'ALL'
                     and p2spcd <> frspcd;
                  Match = 'N';
                  Leave;
               endif;
             endif;
           endif;
          //
          // Billed from CPT4
          //
           *in02 = frbfcd > *blank;
           if *in02 = *on;
             fxBfCd = FRbfCd;
             if p2type = 'H';
                %Subst(FRbfCd:9:3) = *blanks;
             endif;
           endif;
           if *in02 = *on
                 and kkcpt4 < frbfcd
                 and eobtyp <> 'H';
              Match = 'N';
              Leave;
           endif;
          //
          // Check the rev code and CPT/HCPCS code against Billed from code
          //
           if *in02 = *on and eobtyp = 'H';
             if x1svcd < f1bfrv or x1cpt4 < f1bfpc;
                Match = 'N';
                Leave;
             endif;
           endif;
          //
          // Billed to CPT4
          //
           *in03 = frbtcd > *blank;
           if *in03 = *on;
             fxBtCd = FRbtCd;
             if p2type = 'H';
                %Subst(FRbfCd:9:3) = 'ZZZ';
             endif;
           endif;
           if *in03 = *on
                 and kkcpt4 > frbtcd
                 and eobtyp <> 'H';
              Match = 'N';
              Leave;
           endif;
          //
          // Check the rev code and CPT/HCPCS code against Billed to code in ru
          //
           if *in03 = *on and eobtyp = 'H';
             if x1svcd > f1btrv or x1cpt4 > f1btpc;
                Match = 'N';
                Leave;
             endif;
           endif;
       // Check to see if the code is U, T, etc and exclude from Anesthesia
       // rules.  Example: 0002U, 0042T, etc.
           If isNonStdCode = *On and FRbfCd = '00100' and
                     FRbtCd = '01999';
              Match = 'N';
              Leave;
           Endif;
           if frccod <> *blank
                 and xxcc <> frccod;
              Match = 'N';
              Leave;
           endif;
           //
           // Set a flag to be used in the update sub for CCI savings
           //
           if frccod <> *blank and frdis = 'Y'
                 and xxcc = frccod;
             CCIred = 'Y';
           endif;
          //
          // Type of drug (Brand name or generic)
          //
           *in03 = frptyp > *blank;
           if *in03 = *on
                 and p2ptyp <> frptyp;
              Match = 'N';
              Leave;
           endif;
           //
           // Compare billed modifiers to this rules modifiers
          //
           exsr mod_check;
           if xxTop = 'Y';
              Match = 'N';
              Leave;
           endif;
           //
           // Process modifier indicators
           //
           exsr mod_ind;
           if xxTop = 'Y';
              Match = 'N';
              Leave;
           endif;
          //
          //    BILLED FROM/TO TYPE OF BILL
          //
           if frftob >'   ';
             if eublty < frftob or eublty > frttob;
                Match = 'N';
                Leave;
             endif;
           endif;
          //
          //    BILLED FROM PLACE OF SERVICE
          //
           *in06 = frbfps > *zeros;
           *in07 = frbtps > *zeros;
          //
          //    BILLED FROM PLACE OF SERVICE
          //
           if *in06 = *on;
      /End-Free
             Monitor;
             bxPos = %Dec(P2plac:3:0);
             On-Error;
             Endmon;
             x1Pos = %Subst(%Editc(frbfPs:'X'):2:2);
             If x1Pos = *blanks;
               x1Pos = '00';
             Endif;
             if bxpos < frbfps;
                Match = 'N';
                Leave;
             endif;
           endif;
          //
          //    BILLED TO PLACE OF SERVICE
          //
           if *in07 = *on
                 and bxpos > frbtps;
              Match = 'N';
              Leave;
           endif;
          //
          //    BILLED TYPE OF SERVICE
          //
           *in08 = frbtos > *blank;
          //
           if *in08 = *on
                 and p2tos  <> frbtos;
              Match = 'N';
              Leave;
           endif;
           if frftos <> *blanks;
             %Subst(xxFtos:1:2) = FRfTos;
             %Subst(xxFtos:3:1) = FRfTss;
             %Subst(xxTtos:1:2) = FRtTos;
             %Subst(xxTtos:3:1) = FRtTss;
             if p2htos < xxftos or p2htos > xxttos;
                Match = 'N';
                Leave;
             endif;
             if (%subst(p2htos:3:1) < %subst(xxftos:3:1))
                   or (%subst(p2htos:3:1) > %subst(xxttos:3:1));
                Match = 'N';
                Leave;
             endif;
           endif;                                                             //
          //
          //    BILLED BODY PART
          //
           *in09 = frbprt > *blank;
           if *in09 = *on
                 and p2part <> frbprt;
              Match = 'N';
              Leave;
           endif;
          //
          // Units billed compared to time division range
          //
           if frttm <> *zero
                 and p2anes <> 'Y';
             if p2anmn < frftm
                   or p2anmn > frttm;
                Match = 'N';
                Leave;
             endif;
           endif;
          //
          // Billed charges compared to ASC MRA
          //
           if frcomp = 'Y'
                 and p2totc > p2asc;
              Match = 'N';
              Leave;
           endif;
          //
          // ASC amount has a valid value
          //
           if fraval = 'Y'
                 and p2asc = *zero;
              Match = 'N';
              Leave;
           endif;
           if fraval = 'X'
                 and p2amt <> *zero;
              Match = 'N';
              Leave;
           endif;
          //
          // Facility Fee Field has a value
          //
           if fraval = 'F'
                 and p2facf <> *zero;
              Match = 'N';
              Leave;
           endif;
          //
          // Facility Fee Field has a value
          //
           if fraval = 'S'
                 and p2scam <> *zero;
              Match = 'N';
              Leave;
           endif;
          //
          // No amount established for this code.
          //
           if fraval = 'M' and p2mra <> *zero
                 or fraval = 'A' and p2anam <> *zero
                 or fraval = 'P' and p2prof <> *zero
                 and mod2 = '26'
                 or fraval = 'T' and p2tech <> *zero
                 and mod2 = '27'
                 or fraval = 'T' and p2tech <> *zero
                 and mod2 = 'TC';
                 Match = 'N';
                 Leave;
           endif;
           If ##clt# = '114';
              If Fraval = 'Z' and (P2mra <> *Zero or P2prof <> 0
                    or P2tech <> 0 or P2anam <> 0 or P2asc <> 0);
                 Match = 'N';
                 Leave;
              Endif;
           Endif;
          //
          // No PC amount established for this code.
          //
           if fraval = 'P' and frbmri = '5' and
                 P2prof = 0 and p2MRA = 0;
             Match = 'N';
             Leave;
           endif;
          //
          // No TC amount established for this code.
          //
           if fraval = 'T' and frbmri = '5' and
                 P2tech = 0 and p2MRA = 0;
             Match = 'N';
             Leave;
           endif;
           if FRbmri = '6' and P2bmri <> '6';
              Match = 'N';
              Leave;
           endif;
          //
          // Check Peer Group
          //
           if frpgrp <> *blanks
                 and frpgrp <> p2pgrp;
              Match = 'N';
              Leave;
           endif;
          //
          // Check for code type CPT4 or R&C.
          //
           if frct <> *blanks;
             if p2rcfl = 'Y'
                   and frct <> 'R'
    
                   or p2rcfl <> 'Y'
                   and frct = 'R';
                Match = 'N';
                Leave;
             endif;
           endif;
           if FRbInvl = 'Y'
                 and P2invl <> 'Y';
              Match = 'N';
              Leave;
           endif;
           if frbmri = 'G';
             *IN82 = *ON;
           endif;
          //
          // Billed Rule use must match rule
          //
          // I=Include in counting procedures, Y=MRI or CAT scan match
           //
           select;
           when frbmri = 'I' or frbmri = 'Y' and
                 p2bmri <> 'Y';
             Match = 'N';
             Leave;
           //
           // C=Include in cascading
           //
           when frbmri = 'C' and p2bcas <> 'Y';
              Match = 'N';
              Leave;
           //
           // E=Include in counting procedures
           //
           when frbmri = 'E' and p2bmri = 'Y';
              Match = 'N';
              Leave;
           endsl;
          //
          // If billed rule use = 'M' (Multisurg) and we have already processed
          // rule for mod 50 or 51 then skip this rule
          //
           if frbmri = 'M' and xxmsurg = 'Y'
                 or frbmri = 'M' and p2nct = 'Y';
              Match = 'N';
              Leave;
           endif;
           if frbmri = 'S' and DPRXPT <> 'S';
              Match = 'N';
              Leave;
           endif;
           //
           // Check for special rules that apply when networks are selected
           //
           if frbmri = 'N' and eontwk = *blank;
              Match = 'N';
              Leave;
           endif;
           if frbmri = 'N' and eontwk <> *blank;
             kkfdat = p2fdat;
             kknwcd = eontwk;
             setgt s4ky ps401;
             readpe s4ky1 ps401;
             Dow not %Eof(fms4l3) and S4stat = 'I';
               readpe s4ky1 ps401;
             Enddo;
             if %Eof(fms4l3);
                Match = 'N';
                Leave;
             else;
               if s4endt = 0;
                 s4endt = 9999999;
               endif;
               if p2fdat > s4endt;
                 Match = 'N';
                 Leave;
               endif;                                                         //
             endif;                                                           //
           endif;                                                             //
          //
          // Check for Valid License Number from DOH table.
          // A return code of N means it was on the valid list
          //
           Select;
            //
            // Physician
            //
           When EOBTYP = 'N';
             SCLIC# = EHFLIC;
            //
      /End-Free
     C                   CALL      'FA746R'
     C                   PARM      EOAST         SCAST
     C                   PARM                    SCLIC#
     C                   PARM      ' '           @@RTCD            1
     C                   PARM                    EOBTYP
      /Free
             IF @@RTCD = 'N';
               IF FRBMRI = 'L';
                  Match = 'N';
                  Leave;
               ENDIF;                                                         //
             ENDIF;                                                           //
            //
            // Hospital
            //
           When EOBTYP = 'H';
             SCLIC# = EUFLIC;
      /End-Free
     C                   CALL      'FA746R'
     C                   PARM      EOAST         SCAST
     C                   PARM                    SCLIC#
     C                   PARM      ' '           @@RTCD            1
     C                   PARM                    EOBTYP
      //
      /Free
             if @@rtcd ='N';
               if euolic <>' ';
                 SCLIC# = EUOLIC;
      /End-Free
     C                   CALL      'FA746R'
     C                   PARM      EOAST         SCAST
     C                   PARM                    SCLIC#
     C                   PARM      ' '           @@RTCD            1
     C                   PARM                    EOBTYP
      /Free
               endif;
               IF @@RTCD = 'N' and frbmri = 'L';
                  Match = 'N';
                  Leave;
               ENDIF;                                                         //
             ENDIF;                                                           //
            //
            // Pharmacy
            //
           When EOBTYP = 'X';
             @@rtcd = 'Y';
             SCLIC# = EPPLIC;
             //
      /End-Free
     C                   CALL      'FA746R'
     C                   PARM      EOAST         SCAST
     C                   PARM                    SCLIC#
     C                   PARM      ' '           @@RTCD            1
     C                   PARM                    EOBTYP
     //
      /Free
             IF (@@RTCD = 'N' and
                   (%SUBST(SCLIC#:1:2) = 'PU' or
                   %SUBST(SCLIC#:1:2) = 'PS' or
                   %SUBST(SCLIC#:1:2) = 'NP')) or
                   %SUBST(SCLIC#:1:3) = 'DME';
               @@rtcd = 'N';
             else;
               @@rtcd = 'Y';
             endif;
             if @@rtcd ='N';
              //
              // Now check the prescribing physician license number
              //
               scLic# = DPlic#;
      /End-Free
     C                   CALL      'FA746R'
     C                   PARM      EOAST         SCAST
     C                   PARM                    SCLIC#
     C                   PARM      ' '           @@RTCD            1
     C                   PARM      'N'           @@BTYP            1
      /Free
               if @@rtcd = 'Y';
               endif;
             endif;
             IF @@rtcd = 'N' and FRBMRI = 'L';
                Match = 'N';
                Leave;
             ENDIF;                                                           //
            //
            // Dental
            //
           When EOBTYP = 'D';
satxx        SCLIC# = EDFLIC;
      /End-Free
     C                   CALL      'FA746R'
     C                   PARM      EOAST         SCAST
     C                   PARM                    SCLIC#
     C                   PARM      ' '           @@RTCD            1
     C                   PARM                    EOBTYP
      /Free
             IF @@RTCD = 'N' and frbmri = 'L';
                Match = 'N';
                Leave;
             ENDIF;                                                           //
           Endsl;
           //
           // Billed Follow Up Day Type
           //
           if frbfut <> *blanks;
             *in70 = *on;
           endif;
           if frbmri = 'T' and p2bmri <> 'T';
              Match = 'N';
              Leave;
           endif;
           //
          // Providers service code information excluding Multi-surg rules
          //
           if frpsrv = 'Y'
                 and frbmri <> 'M';
            //
            // If default rule then no service code information was found
            //
             if frpct > 0;
               if p2mra > 0
                     or p2invl = 'Y';
                  Match = 'N';
                  Leave;
               endif;
             endif;
            //
            // Deny Service date/prov eff date do not match
            //
             if frdis = 'Y'
                   and p2invl <> 'Y';
                Match = 'N';
                Leave;
             endif;
            //
           endif;
           //
           // Check to see if provider is exempt from DRG payment schedule
           //
           if frbdex = 'Y' and p2bdex <> 'Y';
              Match = 'N';
              Leave;
           endif;
           if frmema = 'N' and p2mema <> 'N';
              Match = 'N';
              Leave;
           endif;
           //
           // If set to pay Service Code amount from charge master and there is
           // no match on the medicare# then skip.
           //
           if frppsc = 'Y' and p2mema = 'N';
              Match = 'N';
              Leave;
           endif;
           //
           // If set to pay Outpatient RCC ration and it is zero skip the rule
           //
           if frporc = 'Y' and p2occr = 0;
              Match = 'N';
              Leave;
           endif;
           //
           // If set to pay DRG and there is no match in either the Federal
           // register or Prospective Payment table skip the rule
           //
           if frpdrg = 'Y' and p2fema = 'N'
                 or frpdrg = 'Y' and p2poma = 'N';
              Match = 'N';
              Leave;
           endif;
           if frpoma = 'N' and p2poma <> 'N';
              Match = 'N';
              Leave;
           endif;
           if frfema = 'N' and p2fema <> 'N';
              Match = 'N';
              Leave;
           endif;
           //
           // Does Provider service code have a value
           //
           if frppsc = 'Y' and p2sval = 'N';
              Match = 'N';
              Leave;
           endif;
           if frsval = 'N' and p2sval <> 'N';
              Match = 'N';
              Leave;
           endif;
           //
           // Does Provider service code match table
           //
           if frppsc = 'Y' and p2scma = 'N';
              Match = 'N';
              Leave;
           endif;
           if frscma = 'N' and p2scma <> 'N';
              Match = 'N';
              Leave;
           endif;
           //
           // Does pharmacy RCC has a value
           //
           if frppor = 'Y' and p2porc = 0;
              Match = 'N';
              Leave;
           endif;
           //
           if frppir = 'Y' and p2pirc <> 0;
              Match = 'N';
              Leave;
           endif;
          //
          //    Separate Procedure
          //
           if frbsep = 'Y'
                 and p2sepp <> 'Y';
              Match = 'N';
              Leave;
           endif;
          //
          //    Billed Asterick Procedure
          //
           if frbast = 'Y'
                 and p2bast <> 'Y';
              Match = 'N';
              Leave;
           endif;
          //
          //    Billed Assistant Surgeon
          //
           if frbsrg = 'N' and p2surg = 'Y';
              Match = 'N';
              Leave;
           endif;
          //
          //    Billed Accident Date - Skip if Acc Date < Rule Acc Date
          //
           if frfadt <> 0 and frtadt <> 0;
             if p2acdt < frfadt or p2acdt > frtadt;
                Match = 'N';
                Leave;
             endif;
           endif;
           if frfadt <> 0 and frtadt = 0;
             if p2acdt < frfadt;
                Match = 'N';
                Leave;
             endif;
           endif;
           if frfadt = 0 and frtadt <> 0;
             if p2acdt > frtadt;
                Match = 'N';
                Leave;
             endif;
           endif;
          //
          //    Billed from Addon Code
          //
           if frbfad = 'Y'
                 and p2Addon <> 'Y';
              Match = 'N';
              Leave;
           endif;
          //
          //    BY REPORT FIELD
          //
           if frbrt <> ' '
                 and p2rpt <> frbrt;
              Match = 'N';
              Leave;
           endif;
          //
           if frbrt = 'A'
                 and p2rpt <> 'A';
              Match = 'N';
              Leave;
           endif;
           if frbrt = 'M'
                 and p2anes <> 'Y';
              Match = 'N';
              Leave;
           endif;
          //
          // Check for non covered codes: X is without modifiers, D is with 26/
          //
           if frbrt = 'X'
                 and p2rpt <> 'X';
              Match = 'N';
              Leave;
           endif;
           if frbrt = 'D'
                 and p2rpt <> 'D';
              Match = 'N';
              Leave;
           endif;
          //
          // Check for APC status indicator
          //
           if frbrt = 'C'
                 and p2rpt <> 'C';
              Match = 'N';
              Leave;
           endif;
          //
          //    BILLED SPECIALTY EQUAL TO BLANKS
          //
           if frbspb = 'Y'
                 and p2spcd <> *blanks;
              Match = 'N';
              Leave;
           endif;
          //
          //    HOSPITAL TYPE OF SERVICE FLAG
          //
           if frhtos = 'Y'
                 and p2htos <> *blank;
              Match = 'N';
              Leave;
           endif;
          //
          //    NO CUT FLAG
          //    IF NO CUT FLAG IS SET - CHECK CPT4 FILE TO DETERMINE IF THE FLA
          //    HAS BEEN SET FOR THAT SPECIFIC CODE.
          //
           if frnct = 'Y'
                 and p2nct <> 'Y';
              Match = 'N';
              Leave;
           endif;
           if frapcsi <> *blank;
             if p2apcsi <> frapcsi;
                Match = 'N';
                Leave;
             endif;
           endif;
           //
           if frimst <> *blank;
             if p2imst <> frimst;
                Match = 'N';
                Leave;
             endif;
           endif;
           if fraspi <> *blank and
                 p2aspi <> fraspi;
              Match = 'N';
              Leave;
           endif;
           //===============================================================
           //  Compared to Information
           //===============================================================
          //
          //    Compare from code
          //
           *in10 = frcfcd > *blank;
           if *in10 = *on;
             if eobtyp = 'H';
               fxcfcd = %subst(frcfcd:4:5);
             else;
               fxCfCd = FRcfCd;
             endif;                                                           //
           endif;
          //
          //    Compare to code
          //
           *in11 = frctcd > *blank;
           if *in11 = *on;
             if eobtyp = 'H';
               fxctcd = %subst(frctcd:4:5);
             else;
               fxCtCd = FRctcd;
             endif;                                                           //
           endif;
          //
          //    Compare modifier
          //
           *in12 = frcmod > *blank;
          //
          //    Ignore compared to modifer. Count even if not blank.
          //
           *in59 = frigmd > *blank;
          //
          //    Count multiple units per line if = Y
          //
           *in78 = frmunt = 'Y';
          //
          //    Compare place of service
          //
           *in13 = frcpos > *zeros;
           If *in13 = *on;
             x2Pos = %Subst(%Editc(frcpos:'X'):2:2);
             If x2Pos = *blanks;
               x2Pos = '00';
             Endif;
           Endif;
          //
          //    Compare type of service
          //
           *in14 = frctos > *blank;
          //
          //    Compare type of bill
          //
           *in15 = frctob > *blank;
          //
          //    Compare same date
           *in16 = frcsdt > *blank;
          //
          //    Compare history date
          //
           *in17 = frchdt > *blank;
          //
          //    Compare same provider
          //
           *in18 = frcspr > *blank;
          //
          //    Compare different provider
          //
           *in19 = frcdpr > *blank;
          //
          //    Compare days in future
           *in20 = frcfdy > *zeros;
          //
          //    Compare days in past
          //
           *in21 = frcpdy > *zeros;
          //
          //    Compare same bill
          //
           *in51 = frcsbl > *blank;
          //
          //    Compare Stop-loss
          //
           *in56 = frloss > *zero;
           if frtlos = 'A'
                 and p2slos <> 'A';
              Match = 'N';
              Leave;
           endif;
          //
          //    Compare Rule Use to Base Endoscopic Code
          //
           *in75 = frcmri = 'B';
          //
          //    Billed Rule Use to Base Endoscopic Code
          //
           *in76 = frbmri = 'B';
          //
          //    PAY PER PHYS THER HOURS
          //
           *in57 = frpttm > *zeros;
           if *in57 = *on;
             SetPay = 'Y';
           endif;
           *in79 = *off;
           If FRcfdy > *zero and FRcmri = '7';
             If %date(%editc(P2Fdat:'X'):*CYMD0) >
                %date(%editc(P2acdt:'X'):*CYMD0) + %DAYS(FRcfdy);
                *in79 = *on;
             Endif;
           Endif;
           if *in79;
              Match = 'N';
              Leave;
           endif;
          //
          //    PAY PER PHYS THER HOURS
          //
           if frcmri='3';
             *in57 = *on;
             SetPay = 'Y';
           endif;
          //
          //    PAY PER PROCEDURE
          //
           *in22 = fr#prc > *zeros;
           if *in22 = *on;
             SetPay = 'Y';
           endif;
          //
          //    Load treatment period from FRTRMT
          //
           *in58 = frtrmt > *zeros;
           if *in58 = *on;
             setpay= 'Y';
           endif;
           indCAPCSI = *Off;
           If FRcapcsi <> *Blanks;
             indCAPCSI = *On;
           EndIf;
          //
          //    Separate Procedure
          //
           *in42 = frcsep > *blank;
          //
          //    Asterisk Procedure
          //
           *in43 = frcast > *blank;
          //===============================================================
          //  Processing Information
          //===============================================================
          //
          //    PAY PER DAY
          //
           *in23 = fr#day > *zeros;
          //
          //    EOB CODE
          //
           //*** Beg Chg *** 07/18/23 *************************************
           //*in24 = freob > *blank;
           *in24 = (freobcd1 > *blank) or (freobcd2 > *blank);
           //*** End Chg *** 07/18/23 *************************************
           //
           // Reset EOMBFL if FRDIS or FRPCT or FRPCH = Y
           //
           if frdis='Y' or frpct>0 or frpch='Y';
             if *in24;
               clear eombfl;
             endif;
           endif;
          //
          //    ERROR MESSAGE 1
          //
           *in25 = frmsg > *blank;
          //
          //    PAY MRA AMOUNT
          //
           if frpmr = 'Y';
             *in26 = *on;
             SetPay = 'Y';
           endif;
          //
          //    PAY MRA * UNITS
           //
           if frpmr = 'U';
             *in69 = *on;
             SetPay = 'Y';
           endif;
          //
          //    PAY CHARGED UP TO ALLOWANCE FOR PRIMARY
           //
           if frpch = 'U';
             *in84 = *on;
             SetPay = 'Y';
           endif;
          //
          //    PAY CHARGED UP TO TOTAL CHARGES ON THE BILL
           //
           if frpch = 'S';
             *in86 = *on;
             SetPay = 'Y';
           endif;
          //
          //    PAY PRE-OP, INTRA-OP, AND POST-OP
          //
           if frpmr = 'I' or frpmr = 'O' or frpmr = 'R';
             *in77 = *on;
             SetPay = 'Y';
           endif;
          //
          //    PAY CHARGED AMOUNT
          //
           *in27 = frpch = 'Y';
          //
          //    PAY MRA EVEN IF GREATER THAN CHARGED
          //
           If FRpch = 'M' or (P2payMRA = 'Y' and FRdis <> 'Y'
                  and FRpch = *blanks);
             *in53 = *on;
           Endif;
           if frpch = 'B';
             *IN27 = *ON;
             *IN53 = *ON;
           endif;
          //
          //    PAY UP TO TOTAL CHARGED FOR THE BILL
          //
           if FRpCh = 'C' or FRpCh = 'T';
             *in72 = *on;
           else;
             *in72 = *off;
           endif;
           if *in27 = *on;
             SetPay = 'Y';
           endif;
          //
          //    PAY FACILITY FEE AMOUNT
          //
           *in44 = frpfac > *blank;
           if *in44 = *on;
             SetPay = 'Y';
           endif;
          //
          // Pay the ASC amount
          //
           *in32 = frpasc > *blank;
           if *in32 = *on;
             SetPay = 'Y';
           endif;
           //
           // Pay the outpatient provider service code amount
          //
           *in34 = frppsc > *blank;
           if *in34 = *on;
             SetPay = 'Y';
           endif;
           //
           // Pay the inpatient provider DRG rates
          //
           *in35 = frpdrg > *blank;
           if *in35 = *on;
             SetPay = 'Y';
           endif;
           //
           // Pay the inpatient skilled nurse rate
          //
           *in36 = frpsni > *blank;
           if *in36 = *on;
             SetPay = 'Y';
           endif;
           //
           // Pay the outpatient skilled nurse rate
          //
           *in37 = frpsno > *blank;
           if *in37 = *on;
             SetPay = 'Y';
           endif;
           //
           // Pay CORF amount
          //
           *in63 = frpcrf > *blank;
           if *in63 = *on;
             SetPay = 'Y';
           endif;
           //
           // Pay Home Health Inpatient
          //
           *in64 = frphhi > *blank;
           if *in64 = *on;
             SetPay = 'Y';
           endif;
           //
           // Pay Home Health Outpatient
          //
           *in65 = frphho > *blank;
           if *in65 = *on;
             SetPay = 'Y';
           endif;
           //
           // Pay Outp RCC
          //
           *in66 = frporc > *blank;
           if *in66 = *on;
             SetPay = 'Y';
           endif;
           //
           // Pay Outp RCC Pharmacy
          //
           *in67 = frppor > *blank;
           if *in67 = *on;
             SetPay = 'Y';
           endif;
           //
           // Pay Inp RCC Pharmacy
          //
           *in68 = frppir > *blank;
           if *in68 = *on;
             SetPay = 'Y';
           endif;
           //
          //    PAY INPATIENT PER DIEM
           //
           *in45 = frppdm > *blank;
           if *in45 = *on;
             SetPay = 'Y';
           endif;
          //
          //    Pay anes/mi/hr/rnt amount for hours
          //
           *in47 = frpane = 'H';
           if *in47 = *on;
             SetPay = 'Y';
           endif;
          //
          //    Pay anes/mi/hr/rnt amount for pain management
          //
           *in83 = frpane = 'V';
           if *in83 = *on;
             SetPay = 'Y';
           endif;
          //
          //    Pay anes/mi/hr/rnt amount for miles or rental
          //
           *in31 = frpane = 'M';
           if *in31 = *on;
             SetPay = 'Y';
           endif;
           *in52 = frpane = 'R';
           if *in52 = *on;
             SetPay = 'Y';
           endif;
          //
          //    Pay difference between charged and MRA
          //
           *in55 = frpdif > *blank;
           if *in55 = *on;
             SetPay = 'Y';
           endif;
          //
          // Pay according to providers service code information
          //
           if frpsrv = 'Y'
                //
                // Pay percent from provider file if not anesthesia time divisi
                //
                 or frpct = 0
                 and camra > 0
                 and frtmdv = 0;
             if frpct = 0
                   and p2scam > 0
                   and frtmdv = 0;
               SetPay = 'Y';
             endif;
            //
            // Pay default percent from rule - no service code information
            //
             if frpct > 0
                   and camra = 0
                   and frtmdv = 0;
               SetPay = 'Y';
             endif;
            //
            // Deny Service date/prov eff date do not match
            //
             if p2invl = 'Y'
                   and frdis = 'Y';
               SetPay = 'Y';
             endif;
           endif;
          //
          // Pay the Pharmacy Average Wholesale price using CAAWP field
          //
           if frprx = 'Y';
             *IN71 = *ON;
             SetPay = 'Y';
           endif;
          //
          // Pay the Physical Therapy Per Visit Rate
          //
           if frpptv = 'Y' and p2ptpv > 0;
             *IN73 = *ON;
             SetPay = 'Y';
           endif;
           if frpptv = 'Y' and p2ptpv <=0;
              Match = 'N';
              Leave;
           endif;
           if frpptv >= '1' AND FRPPTV <= '6';
             *IN80 = *ON;
             SetPay = 'Y';
           endif;
          //
          // If the med# matched the table but no value was found for Phys ther
          //
           if frtane = 'B' and p2ptpv > 0
                 and p2mema = 'Y';
              Match = 'N';
              Leave;
           endif;
          //
          // If the med# didn't match but the rule was looking for a match
          //
           if p2mema <> 'Y' and frmema = 'Y';
              Match = 'N';
              Leave;
           endif;
          //
          // If the rural/urban flag doesnt match then skip
          //
           if frpare <> *blanks;
             if p2area <> frpare;
                Match = 'N';
                Leave;
             else;
               *IN74 = *ON;
             endif;
           endif;
          //
          //    Process a 2nd pass
          //
           *in48 = frpas2 > *blank;
           if *in48 = *on;
             SetPay = 'Y';
           endif;
          //
          //    PAY CPT4 CODE MRA
           *in46 = frpcpt > *blank;
           if *in46 = *on;
             SetPay = 'Y';
           endif;
          //
          //    DETERMINED DISALLOW OF PAYMENT
          //
           *in28 = frdis > *blank;
           if *in28 = *on;
             SetPay = 'Y';
           endif;
          //
          //    Determined percent to calculate against if not time division
          //
           *in29 = frpct > *zeros;
          //
           if frtmdv = *zeros;
             if *in29 = *on;
               SetPay = 'Y';
             endif;
           endif;
          //
          //    DETERMINED LOWEST AMOUNT
          //
           *in30 = framt > *zeros;
           if *in30 = *on;
             SetPay = 'Y';
           endif;
          //
          //    PROFESSIONAL COMPONENT
          //
           *in38 = frprof > *blank;
           if *in38 = *on;
             SetPay = 'Y';
           endif;
          //
          //    TECHNICAL COMPONENT
          //
           *in39 = frtech > *blank;
           if *in39 = *on;
             SetPay = 'Y';
           endif;
          //
          //    Peer Group
          //
           *in49 = frppgp > *blank;
           if *in49 = *on;
             SetPay = 'Y';
           endif;
          //
          //    Outlier Payment
          //
           *in50 = frpout > *blank;
           if *in50 = *on;
             SetPay = 'Y';
           endif;
          //
          //    PROCEDURES GREATER THAN
          //
           *in40 = fr#gpc > *blank;
          //
          //    DAYS GREATER THAN
          //
           *in41 = fr#gdy > *blank;
          //
          // # of days from last service date
          //
           *in81 = fr#dsd <> 0;
           If *in81 = *on;
             SetPay = 'Y';
           Endif;
              Match = 'Y';
           Enddo;
        Endsr;
     //*******************************************************************
     //* SETUP SUBROUTINE -- SETS UP CONV FACTORS, DATE WINDOWS S E T U P*
     //*******************************************************************
       begsr setup;
        //
        //  SETUP CONVERSION FACTOR FOR CODE RANGE
        //
         if frtane = 'C'
               and frconv > 0;
           x1conv = frconv;
         endif;
         if frconv > 0
               and frtane <> 'C';
           xxconv = frconv;
         endif;
         if frtane = 'P'
               and frconv = 0;
           xxconv = p2mra;
         endif;
         //*** Beg Chg *** 06/14/24 ****************************************
         //***if frtane = 'D'
         //***      and frconv = 0;
         if frtane = 'D' and FRconv = 0 and P2anes = 'Y';
         //*** End Chg *** 06/14/24 ****************************************
           xxconv = p2mra;
         endif;
        //
        //  Set up Time Division
        //
         if frtmdv > 0;
           xxtmdv = frtmdv;
         endif;
        //
        //  Set up Additional Units
        //
         if fraunt > 0;
           xxaunt = fraunt;
         endif;
        //
        // Find 1st day after the initial treatment period
        //
         if frtrmt > *zero
               and *in20 = *on
               and *in21 = *on;
           setll cakys pca0s;
           reade cakys1 pca0s;
           *IN99 = %EOF;
           if *in99 = *off
                 and pscpt4 >= frcfcd
                 and pscpt4 <= frctcd;
             srdti = psfdat;
      /End-Free
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         wktrmt
     //
     C                   call      'SRFUTR8'
     C                   parm      wktrmt        srdte
     C                   parm      frtrmt        srfut
     C                   parm                    srerr
     C                   parm                    srterm
     //
     C                   move      srdte         wktrmt
     //
      /Free
             srdti = wktrmt;
      /End-Free
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxtrmt
     //
      /Free
           endif;
          //
          // Call sub to find the starting date XXPST7 of the window which
          // is the day of the week the 1st treatment was done after the
          // initial treatment period.
          //
           exsr srday;
          //
          // Setting up for days in the future edit
          //
          // Find date ? future
          //
           srfut = frcfdy;
           srdti = xxpst7;
      /End-Free
     C                   call      'SRCVTDAT'    dtmdyy
     //
     C                   movel     srdto         wkfdat
     //
     C                   call      'SRFUTR8'
     C                   parm                    wkfdat
     C                   parm                    srfut
     C                   parm                    srterm
     //
      /Free
           srdti = wkfdat;
          //
      /End-Free
     C                   call      'SRCVTDAT'    dtcymd
     //
     C                   movel     srdto         xxfutr
     //
      /Free
         else;
          //
          // Setting up for days in the past edit
          //
          // Find date ? prior
          //
           if *in21 = *on;
             srpst = frcpdy;
             srdti = p2fdat;
      /End-Free
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         wkfdat
     C                   move      wkfdat        srdte
     C                   call      'SRPAST8'
     C                   parm                    srdte
     C                   parm                    srpst
     C                   parm                    srerr
     C                   parm                    srterm

     C                   move      srdte         xxpast
      /Free
             srdti = xxpast;
      /End-Free
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxpst7
      /Free
           endif;
          //
          // Setting up for days in the future edit
          //
          // Find date ? future
          //
           if *in20 = *on;
            //
             srfut = frcfdy;
             srdti = p2fdat;
            //
      /End-Free
     C                   call      'SRCVTDAT'    dtmdyy
     //
     C                   movel     srdto         wkfdat
     //
     C                   call      'SRFUTR8'
     C                   parm                    wkfdat
     C                   parm                    srfut
     C                   parm                    srterm
     //
      /Free
             srdti = wkfdat;
      /End-Free
     C                   call      'SRCVTDAT'    dtcymd
    
     C                   movel     srdto         xxfutr
      /Free
           endif;
          //
          // Find the first date that falls inside the window set up by
          // FRCPDY and FRCFDY
          //
           if *in20 = *on
                 and *in21 = *on;
             xxymd = xxpst7;
             setll cakyo1 pca0o;
             reade cakyo2 pca0o;
             *IN60 = %EOF;
             if *in60 = *off;
               if oxfdat < p2fdat;
                 srdti = oxfdat;
               else;
                 srdti = p2fdat;
               endif;
               xxpst7 = srdti;
             endif;
            //
            // Find ending date for the new window
            //
      /End-Free
     C                   call      'SRCVTDAT'    dtmdyy
     //
     C                   movel     srdto         wkfdat
     //
     C                   call      'SRFUTR8'
     C                   parm                    wkfdat
     C                   parm                    srfut
     C                   parm                    srterm
      /Free
             srdti = wkfdat;
      /End-Free
     C                   call      'SRCVTDAT'    dtcymd
    
     C                   movel     srdto         xxfutr
      /Free
           endif;
         endif;
        //
        // Find 1st day after the initial treatment period
        //
         if frtrmt > *zero
               and *in20 = *off
               and *in21 = *off;
           chain caky23 pca23;
           *IN99 = NOT %FOUND;
           if *in99 = *off
                 and trcpt4 >= frcfcd
                 and trcpt4 <= frctcd;
             srdti = trfdat;
      /End-Free
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         wktrmt
    
     C                   call      'SRFUTR8'
     C                   parm      wktrmt        srdte
     C                   parm      frtrmt        srfut
     C                   parm                    srerr
     C                   parm                    srterm
    
     C                   move      srdte         wktrmt
      /Free
             srdti = wktrmt;
      /End-Free
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxtrmt
     //
      /Free
           endif;
         endif;
        Endsr;
      /End-Free
     //*******************************************************************
     //* SEPP  SUBROUTINE -- READS FROM CPT-4 OR R & C        S E P P    *
     //*******************************************************************
      /Free
       begsr sepp;
        //
        //  Read from the CPT-4 File
        //
         setgt prky2 ppr01;
         readp ppr01;
         *IN60 = %EOF;
         if *in60 = *off;
          //
          //  Move Separate procedure field from FMPRPxx (xx= state)
          //
           select;
           when eoast <> 'TX';
             if pr1ad <> 'Y';
                xxSepp = 'N';
             else;
                xxSepp = PR1ad;
             endif;
           when eoast = 'TX';
             if pr1a6 <> 'Y';
                xxSepp = 'N';
             else;
                xxSepp = PR1a6;
             endif;
           endsl;
         else;
          //
          //  Read from the R & C File
          //
           setgt rcky2 prc02;
           readp prc02;
           *IN60 = %EOF;
           if *in60 = *off;
             clear xxsepp;
           endif;
         endif;
       endsr;
      //*******************************************************************
      //* SRDAY - Return date of the starting day of the window.S R D A Y *
      //*******************************************************************
      //
       begsr srday;
        //
        // Find the first day of treatment after the initial treatment
        //
         reade cakys1 pca0s;
         *IN99 = %EOF;
         dow *in99 = *off
               and pscpt4 <= frctcd;
           if psfdat >= xxtrmt;
             x1trmt = psfdat;
            //
            // Find the day of the week (Mon., Tue., Wed., ect.)
            //
             srdti = x1trmt;
      /End-Free
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         wkfdat
     //
     C                   call      'SRDAY8'
     C                   parm      wkfdat        srdte
     C                   parm      *blanks       xxsday
     C                   parm                    srlday
     C                   parm                    srerr
     //
      /Free
             leave;
           endif;
           reade cakys1 pca0s;
           *IN99 = %EOF;
         enddo;
        //
        // Assign a number to the day of the week for later calcs
        //
         select;
         when xxsday = 'MON.';
           day2 = 0;
         when xxsday = 'TUE.';
           day2 = 1;
         when xxsday = 'WED.';
           day2 = 2;
         when xxsday = 'THU.';
           day2 = 3;
         when xxsday = 'FRI.';
           day2 = 4;
         when xxsday = 'SAT.';
           day2 = 5;
         when xxsday = 'SUN.';
           day2 = 6;
         endsl;
        //
        // Find the day of the week for the current service date
        //
         srdti = p2fdat;
      /End-Free
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         wkfdat
     //
     C                   call      'SRDAY8'
     C                   parm      wkfdat        srdte
     C                   parm      *blanks       srsday
     C                   parm                    srlday
     C                   parm                    srerr
     //
     // Assign a number to the day of the week for later calcs
     //
      /Free
         select;
         when srsday = 'MON.';
           day1 = 0;
         when srsday = 'TUE.';
           day1 = 1;
         when srsday = 'WED.';
           day1 = 2;
         when srsday = 'THU.';
           day1 = 3;
         when srsday = 'FRI.';
           day1 = 4;
         when srsday = 'SAT.';
           day1 = 5;
         when srsday = 'SUN.';
           day1 = 6;
         endsl;
        //
        // Take the current service day value, i.e. Mon.=0, and subtract
        // the value of the initial treatment day then find the date of
        // that day of the week.
        //
         if day1 < day2;
           day1 = day1 + 7;
         endif;
         srpst = day1 - day2;
      /End-Free
     C                   call      'SRPAST8'
     C                   parm                    srdte
     C                   parm                    srpst
     C                   parm                    srerr
     C                   parm                    srterm
    
     C                   move      srdte         xxpast
      /Free
         srdti = xxpast;
      /End-Free
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxpst7
      /Free
       endsr;
      //*******************************************************************
      //* ADJUST FOR UNDERSERVED CALCS - SUBROUTINE           UNDERSERVED   *
      //*******************************************************************
       begsr underserved;
         xptota = xptota * 1.10;
         xcmra = xptota;
         p2mra = xptota;
         xptotd = p2totc - xptota;
         xptotp = xptota;
         xpmra = p2totc - xptota;
         %Subst(xxMsg:1:2) = qqst;
         %Subst(xxMsg:3:5) = '99994';
         xxErr = 'Y';
         exsr eomb;
         if xptotp > 0;
            xpEomb = '144';
            EombFl = 'Y';
         Endif;                                                               //
       endsr;
      //*******************************************************************
      //* Update                                                U P D A T *
      //*******************************************************************
       begsr updat;
         //
         // Adjust for HPSA calcs if using is NOT overriding
         //
         if xxovrd <> 'Y';
           if p2bfhe = 'Y';
             exsr hpsa;
           else;
             if p2bmri = 'T';
               if eoast = 'TX';
                 exsr underserved;
               endif;
             endif;                                                           //
           endif;
         endif;
        //
        // Write the EOMB to FDDEP
        //
         if xpeomb <> *blanks;
           //*** Beg Add *** 07/18/23 **************************************
           chrEobCode = xpeomb;
           //*** End Add *** 07/18/23 **************************************
           exsr eomb1;
         endif;
         //*** Beg Add *** 07/18/23 **************************************
         if xpeomb2 <> *blanks;
           chrEobCode = xpeomb2;
           exsr eomb1;
         endif;
         //*** End Add *** 07/18/23 **************************************
        //
        // Select record to be updated
        //
         chain caky1a pca01;
         *IN54 = NOT %FOUND;
         if *in54 = *off;
           xpbrtc = 0;
           CAbrUn = CAunts;
           caccdf = xxccdf;
           caccod = xxccod;
           select;
            //
            // Non- Override type
            // Calculate and update record in FDCAP.
            //
           when xxovrd <> 'Y';
            //
            // If this program is denying the line then initialize all reductio
            // and types to zeros/blanks since this is not an override
            //
             clear camra;
             clear caothr;
             clear caoth2;
             clear caot1c;
             clear caot2c;
            //
            //* Process Non Fee Schedule CPT-4 Codes
            //
             if p2fsr = 'N'
                   and p2rcfl <> 'Y';
               xpbrtc = xcmra;
              //
              // Do not set an EOMB for non Fee Scheduled Codes
              //
               clear xpeomb;
               //*** Beg Add *** 07/18/23 **************************************
               clear xpeomb2;
               //*** End Add *** 07/18/23 **************************************
               select;
                //
                // Pay the billed ammount if the MRA is set to zero.
                //
               when p2mra = *zero;
                 xptota = p2totc;
                 xptotp = p2totc;
                 xptotd = *zero;
                //
                // Pay the lesser of the billed ammount or the MRA if MRA is no
                //
               when p2mra <> *zero;
                 if p2totc < p2mra;
                   xptota = p2mra;
                   xptotp = p2totc;
                   xptotd = *zero;
                 else;
                   xptota = p2mra;
                   xptotp = p2mra;
                   xptotd = p2totc - p2mra;
                 endif;
               endsl;
             endif;
            //
            //* Process Non Fee Schedule Reasonable and Customary Codes
            //
             if p2fsr = 'N'
                   and p2rcfl = 'Y';
               xpbrtc = xcmra;
               select;
                //
                // Set payment to zero if the MRA is set to zero.
                //
               when p2mra = *zero;
                 xptota = *zero;
                 xptotp = *zero;
                 xptotd = p2totc;
               when p2mra <> *zero;
                 if p2totc < p2mra;
                   xptota = p2mra;
                   xptotp = p2totc;
                   xptotd = *zero;
                 else;
                   xptota = p2mra;
                   xptotp = p2mra;
                   xptotd = p2totc - p2mra;
                 endif;
               endsl;
             endif;
             catota = xptota;
            //
            // Store savings from State Adjudication in CAMRA
            //
             if p2fsr = 'N' or xxovrd = 'Y';
               camra = xptotd;
             else;
               camra = xpmra;
             endif;
             capaid = xptotp;
            //
            // Store savings from Correct Coding in CAOTH2 type 'CCI'
             //
             // If we are not paying per RVU then calculate payment based on all
             // units
             //
             if *in57 = *off and (p2anes = *blank or p2anes = 'T'
                   and frpmr <> 'V')
                   and eobtyp = 'N'
                   and ##splt = 'N'
                   and CCIred <> 'Y';
               exsr PayMultUnit;
             endif;
             if ##coco = 'Y'
                   and CCIred = 'Y';
               caoth2 = cachrg-capaid-camra-caothr
                   -candis;
               if caoth2 > 0;
                 caot2c = 'CCI';
               endif;
             endif;                                                           //
             //
             // If a line was added by CodeReview due to bundling, etc. then cal
             // CR savings using the MRA from the unbundled (denied) lines.
             //
             if caltr# > 0
                   and eocrne = 'Y';
               //
               // CALTR# is 9.0 stored as 9.2 so we move it to a 9.2 holding fie
               //
               clear xxpmra;
               xxPmra = CAltr#;
               caoth2 = camra - xxpmra;
               if caoth2 > 0;
                 caot2c = 'CCK';
               else;
                 caot2c = *blanks;
               endif;
               camra = camra - caoth2;
             endif;                                                           //
             CAeomb = xpEOmb;
             cadiff = xpdiff;
             carule = xxrule;
             kkst = caast;
             chain frky3 pfr02;
             *In61 = not %Found;
             if eptax = 'Y';
              cadfee = f1amt;
               exsr srCalLAST;
             else;
               catax = 0;
            endif;
             CAuser = q1User;
             CApgm = q1Pgm;
      /End-Free
     C                   time                    dsdttm
      /Free
             srdti = dsdate;
      /End-Free
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         cadt
      /Free
             catime = dstime;
             camraa = xcmra;
            //
            // Execute logic to calculate CABRTC for discounting in Alabama
             exsr popbrc;
             cabrtc = xpbrtc;
             catota = unitcnt;
             //
             // For NY where we are totaling up RVU's load CAMRA for overrides
             //
             if eoast = 'NY' and p2mra > 0;
               camraa = p2mra;
             endif;
             update pca01;
             chain fnky pfn01;
             If %Found(fmfnl)
                  and ##clt# = '114'
                  and fnnwcg = 'Y';
               ##Rrn2 = CArrn;
      /End-Free
     C                   call      'FA770R'
     C                   parm                    ##rrn2
     C                   parm      'A'           @1mode            1
      /Free
             endif;                                                           //
            //
            // Update case history records in multiple occurance data structur
            //
             exsr updat2;
            //
            // Override type - Calculate and update FDCAP
            //
           when xxovrd = 'Y'
                 and caothr = *zero
                 and caoth2 = *zero
                 and candis = *zero
                 and capaid = *zero
                 and camra = *zero
                 and csovrp > *zero;
    
             xxovrp = csovrp / 100;
             xptota = cachrg * xxovrp;
             cabrtc = *zeros;
             camra = cachrg - xptota;
             capaid = xptota;
             CAuser = Q1user;
             CApgm = Q1pgm;
      /End-Free
     C                   time                    dsdttm
      /Free
             srdti = dsdate;
      /End-Free
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         cadt
      /Free
             catime = dstime;
             camraa = xcmra;
             update pca01;
             chain fnky pfn01;
             If %Found(fmfnl)
                   and ##clt# = '114'
                   and fnnwcg = 'Y';
      /End-Free
     C                   move      carrn         ##rrn2

     C                   call      'FA770R'
     C                   parm                    ##rrn2
     C                   parm      'A'           @1mode            1
      /Free
             endif;                                                           //
            //
            // Update case history records in multiple occurance data structur
            //
             exsr updat2;
            //
            // Release the record lock on FDCAP
            //
           other;
             except rca01;
           endsl;
           chain fnky pfn01;
           If %Found(fmfnl)
                and ##clt# = '114'
                and fnnwcg = 'Y';
      /End-Free
     C                   move      carrn         ##rrn2

     C                   call      'FA770R'
     C                   parm                    ##rrn2
     C                   parm      'A'           @1mode            1
      /Free
           endif;                                                             //
           //
          // Write default error message if payment was reduced
           //
           if p2fsr = 'N'
                 and capaid <> cachrg;
             qqDseq = P2dSeq;
     //
     // Determine if an EOMB already exists for this DSEQ number
     //
             chain deky1 pde02;
             if not %Found(fddel);
      /End-Free
     C                   movel     qqst          xxmsg
     C                   move      '99999'       xxmsg
     C                   move      'Y'           xxerr
      /Free
               exsr eomb;
             endif;
           endif;
         endif;
         //
         //  Clear the work fields
         //
         xptota = *zero;
         xptotd = *zero;
         xptotp = *zero;
         clear xppmod;
         xpdiff = *zero;
       Endsr;
     //*******************************************************************
     //* UPDATE MULTIPLE OCCUR DATA STRUCTURE MDSP9      U P D A T 2     *
     //*******************************************************************
       begsr updat2;
         j = 1;
         *in61 = *off;
     //
     // Read all line items for this case and update paid amount for
     // the current line item.
     //
         dow *in61 = *off
               and j <= n2;
           %Occur(mdsp9) = j;
           if cadseq = p9dseq;
             p9totp = capaid;
             p9mraa = camraa;
             *in61 = *on;
           endif;
           j = j + 1;
         enddo;
       Endsr;
     //*******************************************************************
     //* Calculate LA Parish sales tax           srCalLast               *
     //*******************************************************************
       begsr srCalLAST;
         if not openflg;
           //
           // Override FMP3L2 to FMP3L2xx
           //
           length = 56;
           xxcmd = qca(1);
           xxajst = eoast;
      /End-Free
     C                   call      'QCMDEXC'
     C                   parm                    xxcmd
     C                   parm                    length           15 5
      /Free
           open fmp3l2;
           openflg = '1';
         endif;
         setgt p3ky pp301;
         readpe p3ky1 pp301;
         if not %Eof(fmp3l2);
           eval(h) catax = (pxtax/100) * (capaid-cadfee);
           capaid = capaid + catax;
           if capaid > cachrg;
             capaid = cachrg;
             catax = (capaid-cadfee) -
                 ((capaid-cadfee) * 100/(100+pxtax));
           endif;
           camra = camra - catax;
           if camra < 0;
             camra = 0;
           endif;
         endif;
       endsr;
      /End-Free
     Opca01     e            rca01
**  QCMDEXC TABLE
OVRDBF FILE(FMP3L2) TOFILE(*LIBL/FMP3L2  )
