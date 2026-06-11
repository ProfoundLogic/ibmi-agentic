     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     /TITLE U0066R - PRE/RE-CERT EDIT PROGRAM - PROVIDER BILLS VERSION
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     * ***************************************************************
     *  PROGRAM DESCRIPTION:                                         *
     *    AFTER CHARGES ARE ADJUDICATED, IF A PRE-CERT WAS SELECTED  *
     *    AND THE LIABILITY IS 'A', BILL ENTRY WILL CALL THIS PROGRAM*
     *    TO PROCESS THE CHARGES AGAINST THE PRE-CERT RECORD.        *
     *                                                               *
     *  CALLED FROM:                                                 *
     *   F0070R                                                      *
     *                                                               *
     *  CALLS TO:                                                    *
     *   NONE                                                        *
     *                                                               *
     *  PROGRAMMER: 47                 PROJECT: 87MCB                *
     *  CREATED:  3/24/98                                            *
     * ***************************************************************
     *  REVISIONS:                                                   *
     *                                                               *
     *  08/05/13 C:277 Project: MC329 Phase II                       *
     *                 Processing for ICD10, if active               *
     *                                                               *
     *  02/28/09 C:79  Project: MC288                                *
     *                 Further changes after QA                      *
     *                                                               *
     *  01/19/09 C:62  Project: MC288                                *
     *                 Take units into account.                      *
     *                                                               *
     *  07/24/06 C:79  Support: 53235                                *
     *                 Keep MRA reductions for preauthorized amount  *
     *                 calculations                                  *
     *                                                               *
     *  12/02/05 C:79  Support: 49338                                *
     *                 Correct logic for preauthorized amounts       *
     *                                                               *
     *  11/03/04 C:79  Project: MC257                                *
     *                 1. Deny lines when days are exceeded          *
     *                                                               *
     *  02/13/04 C:79  Support: MC010 41299                          *
     *                 1. Compare place of service with leading 0    *
     *                 2. Compare units for room and board codes     *
     *                                                               *
     *  04/25/02 C:62  Project: URMNT #206                           *
     *                 -IF MATCHING PROCEDURE CODE IS FOUND THEN     *
     *                  EDIT AGAINST DATES                           *
     *                 -IF NO MATCH OR NO DATES THEN EDIT AGAINST    *
     *                  ADMISSION DATE AND ADDMISSION DATE + REC DYS *
     *                                                               *
     *  08/14/00 C:62  Project: 71MAK                                *
     *                 WHEN CHECKING POS IT ONLY CHECKS FDDHP        *
     *                                                               *
     *  01/17/99 C:62  Project: MC152                                *
     *                 XXYMD NOT BEING USED SO THAT EDIT ALWAYS FAILS*
     *                 SO REPLACED IT WITH CAFDAT.                   *
     *                                                               *
     *  11/20/98 C:56  Project: MC152                                *
     *             1.  Add get of EOMB record before any editing in  *
     *                 order to write the Recon number to FDMGP      *
     *                                                               *
     *  09/21/98 C:56  Project: MC152                                *
     *             1.  Change the processing of Detail Charge file   *
     *                 from DCS# to EOB#                             *
     *                                                               *
     *             2.  Add subroutine to write to errors to FDMGP    *
     *                                                               *
     *  03/24/98 C:47   87MCB                                        *
     *    Copied U0015R & added these edits:                         *
     *    1. Edit Re-Certs as well as Pre-Certs.                     *
     *    2. Use Procedure code file (UDPOP) instead of Pre-Cert     *
     *       Master (UMPCP) to edit procedure codes.                 *
     *    3. Edit #4 - the proc code in the detail charge            *
     *       will be edited against the proc codes in the            *
     *       new procedure code file (UDPOP).                        *
     *    4. Edit #5 - the total number of visits for the particular *
     *       procedure will be compared to the total number of       *
     *       certified visits in the Proc Code file (UDPOP).         *
     *       If the from & to service dates of the Proc Code file    *
     *       are not entered, the total number of visits will be for *
     *       the life of the claim.                                  *
     *    5. If the from & to service dates have been entered in the *
     *       Proc Code file (UDPOP), the detail charge service date  *
     *       must be within that specified date range.               *
     *       The total number of visits must also be within the      *
     *       certified date range.                                   *
     * ***************************************************************
     *  INDICATOR USAGE:                                             *
     * ***************************************************************
     Fumpcl     if   e           k disk
     Fumpcl1    if   e           k disk
     F                                     rename(ppc01:ppc02)
     Ffdcalh    uf   e           k disk
     Ffdcald    if   e           k disk    rename(pca01:pca02) prefix(x2:2)
     Ffmcml1    if   e           k disk
     Fsmupl     if   e           k disk
     Fudpol     if   e           k disk
     Ffdmgl1    if   e           k disk
    F**FDMGL   UF  E           K        DISK                      A
    F** FDMGL2  IF  E           K        DISK
     Ffdmgl2    if a e           k disk
     F                                     rename(pmg01:pmg02)
     Ffddul     if   e           k disk
     Ffddhl     if   e           k disk
     Ffaeolg    if   e           k disk
     Ffaeul     if   e           k disk
     Ffaehl     if   e           k disk
     Fftabl     if   e           k disk
     F                                     ignore(peb01)
     Ffteblpc   if   e           k disk
     Ffdfrl2    if   e           k disk
     Ffddel3    uf   e           k disk
     Ffddel     if a e           k disk
     F                                     rename(pde01:pde02)
     Ffmprl     if   e           k disk
     *
     D per1            s              5    dim(8) ctdata perrcd(1)
     D per2            s              8    dim(8) alt(per1)
    D*
     D                 ds
     D dsdttm                        12  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        6  0 overlay(dsdttm:7)
    D*
    D*
     D                 ds
     D  pcdc                   1     25
     D                                     dim(5)
     D  pcicd1                 1      5
     D  pcicd2                 6     10
     D  pcicd3                11     15
     D  pcicd4                16     20
     D  pcicd5                21     25
    D*
     D fids01        e ds                  extname(szq2p)
     D psds          esds                  extname(szq1p)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D #2case          s              7
     D #2co#           s              3
     D #2del1          s              1
     D #2del2          s              1
     D #2del3          s              1
     D #2del4          s              1
     D #2del5          s              1
     D #2eob#          s              9
     D #2fnd           s              3
     D #2fyr           s              3
     D #2rcn           s              3
     D #2sub#          s              5
     D hldiag          s                   like(eodia1)
     D hlmseq          s                   like(mgmseq)
     D hlmsg#          s              5
     D kkco#           s                   like(upco#)
     D kkkey           s                   like(ebkey)
     D kkrule          s                   like(frrule)
     D kkseq#          s                   like(deseq#)
     D kkst            s                   like(frst)
     D pchld           s              5
     D qqacdt          s              8  0
     D qqcase          s              7  0
     D qqcert          s             15
     D qqeob#          s              9  0
     D qqext           s              3
     D qqfyr           s              3  0
     D qqrtcd          s              1
     D qqss#           s              9  0
     D r               s              3  0
     D srdate          s              8  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srfutr          s              5  0
     D srrtcd          s              1
     D srtfmt          s              8
     D xx#vst          s              5  0
     D xxcert          s                   like(pccert)
     D xxdseq          s                   like(cadseq)
     D xxeob           s                   like(deeob)
     D xxext           s                   like(pcext)
     D xxfein          s                   like(cafein)
     D xxfrdt          s                   like(pofrdt)
     D xxmod           s                   like(camod)
     D xxmseq          s                   like(mgmseq)
     D xxmsg#          s                   like(mgmsg#)
     D xxmxdy          s                   like(updt)
     D xxpaid          s                   like(capaid)
     D xxpcdc          s             25
     D xxrecd          s                   like(pcrecd)
     D xxseq#          s                   like(deseq#)
     D xxtodt          s                   like(potodt)
     D xxunts          s                   like(caunts)
     D xxymd           s                   like(updt)
     D xxflag          s              1
     D maxdayflag      s              1
     D y               s              5  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*** Beg Add *** 08/05/13 **************************************
     D xxicd10         s              1
     D*** End Add *** 08/05/13 **************************************
     *
     Ipca02
     I              cacpt4                      x2cpt4
     I              caco#                       x2co#
     I              casub#                      x2sub#
     I              cafnd                       x2fnd
     I              cafyr                       x2fyr
     I              cacase                      x2case
     I              cacscd                      x2cscd
     I              cafdat                      x2fdat
     I              catdat                      x2tdat
     I              cardat                      x2rdat
     I              caipop                      x2ipop
     I              cafein                      x2fein
     I              camod                       x2mod
     I              caprty                      x2prty
     I              catrcd                      x2trcd
     I              casvcd                      x2svcd
     I              caicd9                      x2icd9
     I              caanmn                      x2anmn
     I              caunts                      x2unts
     I              catota                      x2tota
     I              catotc                      x2totc
     I              catotd                      x2totd
     I              catotp                      x2totp
     I              cadseq                      x2dseq
     I              cadelf                      x2delf
     I              caflag                      x2flag
     I              castat                      x2stat
     I              cap1                        x2p1
     I              capmod                      x2pmod
     I              causer                      x2user
     I              capgm                       x2pgm
     I              cadt                        x2dt
     I              catime                      x2time
     I              cachk#                      x2chk#
     I              caeob#                      x2eob#
     I              capart                      x2part
     I              caeomb                      x2eomb
     I              carcfl                      x2rcfl
     I              caothr                      x2othr
     I              candis                      x2ndis
     I              cardtp                      x2rdtp
     I              cabrsv                      x2brsv
     I              cabrpm                      x2brpm
     I              cabrpc                      x2brpc
     I              cabram                      x2bram
     I              cabrun                      x2brun
     I              cabrtc                      x2brtc
     I              cazipa                      x2zipa
     I              caast                       x2ast
     I              cadiff                      x2diff
    I*
     Ippc02
     I              pcco#                       p2co#
     I              pcsub#                      p2sub#
     I              pcfnd                       p2fnd
     I              pcfyr                       p2fyr
     I              pccase                      p2case
     I              pcliab                      p2liab
     I              pcstat                      p2stat
     I              pcaddt                      p2addt
     I              pcrvdt                      p2rvdt
     I              pccert                      p2cert
     I              pcext                       p2ext
     I              pcfein                      p2fein
     I              pcmod                       p2mod
     I              pcicd1                      p2icd1
     I              pcicd2                      p2icd2
     I              pcicd3                      p2icd3
     I              pcicd4                      p2icd4
     I              pcicd5                      p2icd5
     I              pcintr                      p2intr
     I              pcpasg                      p2pasg
     I              pcreqd                      p2reqd
     I              pccerd                      p2cerd
     I              pcrecd                      p2recd
     I              pcpos                       p2pos
     I              pc10a1                      p210a1
    C*
    C*   KEY LIST FOR THE DETAIL CHARGE FILE
    C*
    C*
     C     cakey1        klist
     C                   kfld                    upco#
     C                   kfld                    upsub#
     C                   kfld                    upfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
    C*
     C     cakey2        klist
     C                   kfld                    upco#
     C                   kfld                    upsub#
     C                   kfld                    upfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    xxfein
     C                   kfld                    xxmod
    C*
    C*  CLAIM MASTER FILE
    C*
     C     cmkey         klist
     C                   kfld                    upco#
     C                   kfld                    upsub#
     C                   kfld                    upfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
    C*
    C*   KEY LIST FOR THE DIAGNOSIS CODE WORKFILE
    C*
    C*   KEY LIST FOR THE DETAIL CHARGE AUXILARY FILE
    C*
     C     hcky          klist
     C                   kfld                    upco#
     C                   kfld                    upsub#
     C                   kfld                    upfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    cadseq
    C*
    C*   KEY LIST FOR THE PRE-CERT MASTER FILE
    C*
     C     pckey1        klist
     C                   kfld                    upco#
     C                   kfld                    upsub#
     C                   kfld                    upfnd
     C                   kfld                    xxcert
    C*
     C     pckey2        klist
     C                   kfld                    xxcert
    C*
    C*   KEY LIST FOR THE PROCEDURE CODE FILE
    C*
     C     poky          klist
     C                   kfld                    upco#
     C                   kfld                    upsub#
     C                   kfld                    upfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    pccert
     C                   kfld                    pcext
    C*
     C     poky1         klist
     C                   kfld                    upco#
     C                   kfld                    upsub#
     C                   kfld                    upfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    pccert
     C                   kfld                    pcext
     C                   kfld                    cacpt4
    C*
    C*
    C* Key List for Bill Entry Error Message File
    C*
     C     mgky2         klist
     C                   kfld                    upco#
     C                   kfld                    upsub#
     C                   kfld                    upfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    xxdseq
     C                   kfld                    xxmsg#
    C*
     C     mgky1         klist
     C                   kfld                    upco#
     C                   kfld                    upsub#
     C                   kfld                    upfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    xxdseq
     C                   kfld                    xxmseq
    C*
     C     mgky          klist
     C                   kfld                    upco#
     C                   kfld                    upsub#
     C                   kfld                    upfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    xxdseq
    C*
    C* KEY LIST FOR WCMC EOMB TRANSACTION FILE
    C*
     C     eoky          klist
     C                   kfld                    upco#
     C                   kfld                    upsub#
     C                   kfld                    upfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
    C*
    C* KEY LIST FOR EOMB PHYS/HCFA BILL DETAIL TRANSACTION FILE
    C*
     C     dhky          klist
     C                   kfld                    upco#
     C                   kfld                    upsub#
     C                   kfld                    upfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    xxdseq
    C*
     C     duky          klist
     C                   kfld                    upco#
     C                   kfld                    upsub#
     C                   kfld                    upfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    xxdseq
    C*
    C*   KEY LIST FOR THE USER PARAMETER FILE
    C*
     C     upkey         klist
     C                   kfld                    q1user
    C*
    C*
    C*   KEY LIST FOR THE HOSPITAL HEADER AUXILIARY FILE
    C*
     C     euky          klist
     C                   kfld                    upco#
     C                   kfld                    upsub#
     C                   kfld                    upfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
    C*
    C*   KEY LIST FOR THE PHYSICIAN HEADER AUXILIARY FILE
    C*
     C     ehky          klist
     C                   kfld                    upco#
     C                   kfld                    upsub#
     C                   kfld                    upfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
    C*
     C     frky          klist
     C                   kfld                    kkst
     C                   kfld                    kkrule
    C*
    C*  EOMB CODE FILE
    C*
     C     ebky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkkey
    C*
    C*  EOMB DETAIL EOB/NOTES TRANSACTION FILE
    C*
     C     deky          klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    eorcn
    C*
     C     deky1         klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    cadseq
     C                   kfld                    kkseq#
    C*
     C     deky2         klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    cadseq
    C*
     C     deky3         klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    eorcn
     C                   kfld                    cadseq
    C*
     C     csky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkkey
    C*
     C     prky1         klist
     C                   kfld                    caco#
     C                   kfld                    cacpt4
     C                   kfld                    cafdat
    C*
     C     prky2         klist
     C                   kfld                    caco#
     C                   kfld                    cacpt4
    C*
    C*
     C     *entry        plist
     C                   parm                    qqfyr
     C                   parm                    qqcase
     C                   parm                    qqrtcd
     C                   parm                    qqss#
     C                   parm                    qqacdt
     C                   parm                    qqcert
     C                   parm                    qqext
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
    C*
     C                   movel     qqcert        xxcert
     C                   move      qqext         xxext
    C*
    C** GET THE USER PARAMETER FILE
    C*
     C     upkey         chain     pup01                              60
    C*
    C** IF THE GET WAS UNSUCCESSFUL END THE PROGRAM.
    C*
     C                   if        *in60 = *on
     C                   goto      term
     C                   endif
    C*
    C** GET THE CLAIM MASTER RECORD.
    C*
     C     cmkey         chain     pcm01                              60
    C*
    C** IF THE GET WAS UNSUCCESSFUL END THE PROGRAM.
    C*
     C                   if        *in60 = *on
     C                   goto      term
     C                   endif
    C*
    C** DO THE EDIT CHECK FOR EACH POSSIBLE DATE ENTERED
    C*
    C* GET RECON NUMBER FOR FAEOP
    C*
     C     eoky          chain     peo01                              40
     C                   exsr      dltmsg
     C     cakey1        chain     pca01                              54
    C*                                                                +*+*+**
     C                   dow       *in54 = *off
     C                   call      'SA014R'
1    C                   parm      eoast         ##st              2
2    C                   parm      eobtyp        ##btyp            1
3    C                   parm      cafdat        ##fdat            7 0
4    C                   parm      *blank        ##splt            1
    C*
     C                   eval      xxpaid = capaid
     C                   eval      xxeob  = *blanks
    C*
     C                   movel(p)  cacscd        kkkey
     C     csky          chain     pcs01                              99
    C*                                                                +*+*+**
     C                   if        eorcn = carcn
     C                             and csovrd <> 'Y'
    C*                                                                +*+*+**
    C*
    C** GET THE PRE-CERT RECORD
    C*
     C                   eval      xxfein = cafein
     C                   eval      xxmod = camod
     C     pckey1        chain     ppc01                              99
     C                   dow       *in99 = *off
    C*
    C** CHECK THE PLACE OF SERVICE CODE FOR A MATCH
    C*
    C*
     C                   eval      xxdseq = cadseq
     C     dhky          chain     pdh01                              60
     C     duky          chain     pdu01                              60
     C     euky          chain     peu01                              60
     C     ehky          chain     peh01                              60
     C                   movel     '0'           dhpos
     C                   movel     '0'           dupos
     C                   if        pc10a1 <> *blank
     C                             and pc10a1 <> dhpos
     C                             and pc10a1 <> dupos
     C                   move(p)   'PC001'       hlmsg#
     C                   exsr      wrtmsg
     C                   endif
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Select;
       When EObtyp = 'N';
       xxICD10 = EHICDIND;

       When EObtyp = 'H';
       xxICD10 = EUICDIND;
       Endsl;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
    C*
    C* TAKE ADMIT DATE AND THE RECOMMENDED DAYS = MAX DAYS
    C*
     C                   if        pcrecd > 0
     C                   eval      maxdayflag = 'Y'
     C                   else
     C                   eval      maxdayflag = 'N'
     C                   endif
    C*
     C                   eval      srdti = pcaddt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         srdate
    C*
     C                   eval      srfutr = pcrecd
     C                   call      'SRFUTR8'
     C                   parm                    srdate
     C                   parm                    srfutr
     C                   parm                    srrtcd
    C*
     C                   eval      srdti = srdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxmxdy
    C*
     C                   eval      srfutr = *zeros
    C*
    C* IF THIS IS A RE-CERT WE NEED TO ADD ALL RECOMMENDED DAYS FROM
    C* THE PRE-CERT AND ALL RE-CERT PRIOR TO AND INCLUDING THIS ONE
    C* TO GET CORRECT MAX DAYS RANGE
    C*
     C                   if        xxext <> *zeros
     C     pckey2        chain     ppc02                              60
     C                   dow       *in60 = *off
     C                             and p2ext < xxext
     C                   eval      srfutr = srfutr + p2recd
    C*
     C                   if        p2recd > 0
     C                   eval      maxdayflag = 'Y'
     C                   endif
    C*
     C     pckey2        reade     ppc02                                  60
     C                   enddo
    C*
     C                   eval      srdti = xxmxdy
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         srdate
    C*
     C                   call      'SRFUTR8'
     C                   parm                    srdate
     C                   parm                    srfutr
     C                   parm                    srrtcd
    C*
     C                   eval      srdti = srdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxmxdy
    C*
     C                   endif
    C*
    C** NO CHARGES ARE ALLOWED OUT OF THE MAX. DATE RANGE
    C*
     C                   if        pcaddt <> *zeros
     C                             and maxdayflag = 'Y'
     C                   if        cafdat < pcaddt
     C                             or cafdat > xxmxdy
    C*
     C                   move(p)   'PC002'       hlmsg#
     C                   exsr      wrtmsg
     C                   endif
     C                   endif
    C*
    C** NUMBER OF DAYS FOR ROOM AND BOARD CANNOT EXCEED RECOM. DAYS
    C*
     C                   if        eobtyp = 'H'
     C                             and pcrecd > 0
    C*
     C                   if        durvcd >= 110
     C                             and durvcd <= 179
     C                             or durvcd >= 200
     C                             and durvcd <= 219
     C                             or durvcd >= 191
     C                             and durvcd <= 194
    C*
    C*
     C                   eval      xxrecd = 0
    C*
     C     pckey2        chain     ppc02                              60
    C*
     C                   dow       *in60 = *off
    C*
     C                   eval      xxrecd = xxrecd + p2recd
     C     pckey2        reade     ppc02                                  60
    C*
     C                   enddo
    C*
    C*
     C                   if        duunts > xxrecd
     C                             and capaid > 0
     C                   move(p)   'PC008'       hlmsg#
     C                   exsr      wrtmsg
    C*
    C* GET INFORMATION FROM THE STATE RULE AND CALCULATE REDUCTIONS
    C*
     C                   move      caast         kkst
     C                   eval      kkrule = carule
    C*
     C     frky          chain     pfr01                              99
    C*
     C                   if        *in99 = *off
     C                             and frppdm = 'Y'
    C*
     C                   eval      xxpaid = framt * xxrecd
     C                   eval      caothr = capaid - xxpaid
     C                   move      'PRC'         caot1c
    C*
     C                   movel     'P01'         xxeob
    C*
     C                   endif
    C*
     C                   endif
    C*
     C                   endif
    C*
     C                   endif
    C*
    C* VERIFY THE DIAGNOSIS CODE OF THE DETAIL CHARGE IS IN THE PRE-
    C* CERT RECORD
    C*
    C*
    C*
     C                   if        *in40 = *off
    C*
    C* GET DIAGNOSIS CODE
    C*
     C                   select
     C                   when      dhdxr1 = 'X'
     C*** Beg Add *** 08/05/13 ****************************************
     C                   If        xxICD10 = 'Y'
     C                   movel     EHicd1001     hldiag
     C                   Else
     C*** End Add *** 08/05/13 ****************************************
     C                   movel(p)  ehdia1        hldiag
     C*** Beg Add *** 08/05/13 ****************************************
     C                   endif
     C*** End Add *** 08/05/13 ****************************************
     C                   when      dhdxr2 = 'X'
     C*** Beg Add *** 08/05/13 ****************************************
     C                   If        xxICD10 = 'Y'
     C                   movel     EHicd1002     hldiag
     C                   Else
     C*** End Add *** 08/05/13 ****************************************
     C                   movel(p)  ehdia2        hldiag
     C*** Beg Add *** 08/05/13 ****************************************
     C                   endif
     C*** End Add *** 08/05/13 ****************************************
     C                   when      dhdxr3 = 'X'
     C*** Beg Add *** 08/05/13 ****************************************
     C                   If        xxICD10 = 'Y'
     C                   movel     EHicd1003     hldiag
     C                   Else
     C*** End Add *** 08/05/13 ****************************************
     C                   movel(p)  ehdia3        hldiag
     C*** Beg Add *** 08/05/13 ****************************************
     C                   endif
     C*** End Add *** 08/05/13 ****************************************
     C                   when      dhdxr4 = 'X'
     C*** Beg Add *** 08/05/13 ****************************************
     C                   If        xxICD10 = 'Y'
     C                   movel     EHicd1004     hldiag
     C                   Else
     C*** End Add *** 08/05/13 ****************************************
     C                   movel(p)  ehdia4        hldiag
     C*** Beg Add *** 08/05/13 ****************************************
     C                   endif
     C*** End Add *** 08/05/13 ****************************************
     C                   endsl
    C*
     C                   movea     pcdc          xxpcdc
     C                   eval      y = 1
     C                   if        xxpcdc <> *blanks
     C     hldiag        lookup    pcdc(y)                                60
     C                   if        *in60 = *off
     C                   move(p)   'PC003'       hlmsg#
     C                   exsr      wrtmsg
     C                   endif
     C                   endif
     C                   if        eobtyp = 'H'
    C*
     C                   eval      y = 1
     C     eudia1        lookup    pcdc(y)                                60
    C*
     C                   if        *in60 = *off
     C                             and eudia2 <> *blanks
    C*
     C                   eval      y = 1
     C     eudia2        lookup    pcdc(y)                                60
    C*
     C                   endif
    C*
     C                   if        *in60 = *off
     C                             and eudia3 <> *blanks
    C*
     C                   eval      y = 1
     C     eudia3        lookup    pcdc(y)                                60
    C*
     C                   endif
    C*
     C                   if        *in60 = *off
     C                             and eudia4 <> *blanks
    C*
     C                   eval      y = 1
     C     eudia4        lookup    pcdc(y)                                60
    C*
     C                   endif
    C*
     C                   if        *in60 = *off
    C*
     C                   move(p)   'PC003'       hlmsg#
     C                   exsr      wrtmsg
    C*
     C                   endif
    C*
     C                   endif
     C                   endif
    C*
    C* VERIFY THAT DETAIL CHARGE PROCEDURE IS IN THE PRE-CERT RECORD
    C*
     C                   if        caprty = '1'
    C*
     C     poky1         chain     ppo01                              60
     C                   if        *in60 = *on
    C*
     C* EVEN IF THE CODE IS NOT LISTED WE WILL CHECK THE DATE RANGE
    C*
     C                   eval      xxflag = 'Y'
    C*
     C     poky          chain     ppo01                              60
     C                   dow       *in60 = *off
     C                   if        cafdat >= pofrdt and cafdat <= potodt
     C                             and postat ='A'
     C                   eval      xxflag = 'N'
     C                   leave
     C                   endif
    C*
     C     poky          reade     ppo01                                  60
    C*
     C                   enddo
     C                   if        xxflag = 'Y'
     C                   endif
     C                   endif
    C*
HEREC**         *IN60     IFEQ *OFF                       CHAIN PPO01
B0   C                   if        *in60 = *off
    C*
    C* CHECK TO SEE IF PRE-CERT FROM AND TO SERV DATES WERE ENTERED
    C*
     C                   if        pofrdt <> *zeros
     C                             and potodt <> *zeros
    C*
    C* IF THE PRE-CERT DATES HAVE BEEN ENTERED MAKE SURE CHARGE IS
    C* WITHIN DATE RANGE
    C*
     C                   if        cafdat >= pofrdt
     C                             and cafdat <= potodt
    C*
    C* IF CHARGE IS WITHIN PRE-CERT DATES READ DETAIL CHARGE FILE FOR
    C* ANY OTHER CHARGES FOR PROVIDER WITHIN PRE-CERT DATES AND MAKE
    C* SURE THAT TOTAL IS NOT GREATER THAN TOTAL # OF VISITS FROM
    C* PRE-CERT FILE
    C*
     C                   eval      xx#vst = *zeros
     C     cakey2        chain     pca02                              50
     C                   dow       *in50 = *off
    C*
    C* IF WE FIND A HIT CPT4 CODE MUST BE SAME AS CURRENT ONE
    C*
     C                   if        cacpt4 = x2cpt4
    C*
    C* NOW WE MUST MAKE SURE CHARGE IS WITHIN PRE-CERT FROM AND TO DATES
    C*
     C                   if        x2fdat >= pofrdt
     C                             and x2fdat <= potodt
     C                   eval      xx#vst = xx#vst + 1
     C                   endif
     C                   endif
     C     cakey2        reade     pca02                                  50
     C                   enddo
    C*
    C* NOW THAT WE HAVE THE TOTAL # OF VISITS WE MUST COMPARE TO
    C* PRE-CERT RECORD AND MAKE SURE THAT IT IS LESS THAN OR EQUAL TO
    C* THE PRE/RE-CERT # OF VISITS
    C*
     C                   if        povst <> *zeros
     C                   if        xx#vst > povst
    C*
     C                   move(p)   'PC004'       hlmsg#
     C                   exsr      wrtmsg
     C                   endif
     C                   endif
    C*
     C                   else
    C*
    C*
     C                   move(p)   'PC005'       hlmsg#
     C                   exsr      wrtmsg
     C                   endif
    C*
     C                   else
    C*
    C* SINCE FROM AND TO DATE ARE ZERO THEN CHECK AGAINST ADMISSION
    C* DATE AND ADMISSION DATE + REC DAYS
    C*
     C                   eval      xxfrdt = pcaddt
     C                   eval      srdti = pcaddt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         srdate
    C*
     C                   call      'SRFUTR8'
     C                   parm                    srdate
     C                   parm      pcrecd        srfutr
     C                   parm      *blanks       srrtcd
    C*
     C                   eval      srdti = srdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxtodt
    C*
B1   C                   if        xxfrdt <> *zeros
     C                             and xxtodt <> *zeros
    C*
    C* IF THE PRE-CERT DATES HAVE BEEN ENTERED MAKE SURE CHARGE IS
    C* WITHIN DATE RANGE
    C*
B2   C                   if        cafdat >= xxfrdt
     C                             and cafdat <= xxtodt
    C*
    C* IF CHARGE IS WITHIN PRE-CERT DATES READ DETAIL CHARGE FILE FOR
    C* ANY OTHER CHARGES FOR PROVIDER WITHIN PRE-CERT DATES AND MAKE
    C* SURE THAT TOTAL IS NOT GREATER THAN TOTAL # OF VISITS FROM
    C* PRE-CERT FILE
    C*
     C                   eval      xx#vst = *zeros
     C     cakey2        chain     pca02                              50
B3   C                   dow       *in50 = *off
    C*
    C* IF WE FIND A HIT CPT4 CODE MUST BE SAME AS CURRENT ONE
    C*
B4   C                   if        cacpt4 = x2cpt4
    C*
    C* NOW WE MUST MAKE SURE CHARGE IS WITHIN PRE-CERT FROM AND TO DATES
    C*
B5   C                   if        x2fdat >= xxfrdt
     C                             and x2fdat <= xxtodt
     C                   eval      xx#vst = xx#vst + 1
E5   C                   endif
E4   C                   endif
     C     cakey2        reade     pca02                                  50
E3   C                   enddo
    C*
    C* NOW THAT WE HAVE THE TOTAL # OF VISITS WE MUST COMPARE TO
    C* PRE-CERT RECORD AND MAKE SURE THAT IT IS LESS THAN OR EQUAL TO
    C* THE PRE/RE-CERT # OF VISITS
    C*
B6   C                   if        povst <> *zeros
B7   C                   if        xx#vst > povst
     C                   move(p)   'PC004'       hlmsg#
     C                   exsr      wrtmsg
E7   C                   endif
E6   C                   endif
E2   C                   endif
S1   C                   else
    C*
    C* IF THE FROM AND TO DATES IN PRE-CERT RECORD ARE ZEROS ALLOW
    C* ONLY THE TOTAL NUMBER OF VISITS FOUND IN THE PRE-CERT RECORD
    C* FOR THE LIFE OF THE CLAIM
    C*
     C                   eval      xx#vst = *zeros
     C     cakey2        chain     pca02                              50
     C                   dow       *in50 = *off
    C*
    C* IF WE FIND A HIT CPT4 CODE MUST BE SAME AS CURRENT ONE
    C*
     C                   if        cacpt4 = x2cpt4
     C                   eval      xx#vst = xx#vst + 1
     C                   endif
     C     cakey2        reade     pca02                                  50
     C                   enddo
    C*
    C* NOW THAT WE HAVE THE TOTAL # OF VISITS WE MUST COMPARE TO
    C* PRE-CERT RECORD AND MAKE SURE THAT IT IS LESS THAN OR EQUAL TO
    C* THE PRE/RE-CERT # OF VISITS
    C*
     C                   if        povst <> *zeros
     C                   if        xx#vst > povst
    C*
     C                   move(p)   'PC004'       hlmsg#
     C                   exsr      wrtmsg
     C                   endif
     C                   endif
E1   C                   endif
    C*
     C                   endif
    C*
    C*
    C* PASSED THE DATE/NUMBER OF VISITS EDITS. PAY PRE-AUTH AMOUNT
    C*
     C                   if        popamt > 0
     C                   exsr      preaut
     C                   endif
    C*
S0   C                   else
    C*
    C* SINCE FROM AND TO DATE ARE ZERO THEN CHECK AGAINST ADMISSION
    C* DATE AND ADMISSION DATE + REC DAYS
    C*
     C                   eval      xxfrdt = pcaddt
     C                   eval      srdti = pcaddt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         srdate
    C*
     C                   call      'SRFUTR8'
     C                   parm                    srdate
     C                   parm      pcrecd        srfutr
     C                   parm      *blanks       srrtcd
    C*
     C                   eval      srdti = srdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxtodt
    C*
B11  C                   if        xxfrdt <> *zeros
     C                             and xxtodt <> *zeros
    C*
    C* IF THE PRE-CERT DATES HAVE BEEN ENTERED MAKE SURE CHARGE IS
    C* WITHIN DATE RANGE
    C*
B12  C                   if        cafdat < xxfrdt
     C                             or cafdat > xxtodt
     C                   move(p)   'PC005'       hlmsg#
     C                   exsr      wrtmsg
E12  C                   endif
E11  C                   endif
E0   C                   endif
     C                   endif
    C*
    C*
     C     pckey1        reade     ppc01                                  99
     C                   enddo
    C*
     C                   exsr      update
    C*
     C                   endif
    C*
     C     cakey1        reade     pca01                                  54
    C*
     C                   enddo
    C* ----------------------------------------------------------------
    C* END PROGRAM
    C*
     C     term          tag
     C                   move      *on           *inlr
     C                   return
    C*================================================================
    C* DLTMSG - Delete EOB codes
    C*================================================================
     C     dltmsg        begsr
    C*
    C* Remove adjudication messages and EOB codes
    C*
     C                   move      eoco#         #2co#
     C                   move      eosub#        #2sub#
     C                   move      eofnd         #2fnd
     C                   move      eofyr         #2fyr
     C                   move      eocase        #2case
     C                   move      eoeob#        #2eob#
     C                   move      eorcn         #2rcn
     C                   move      ' '           #2del1
     C                   move      'P'           #2del2
     C                   move      ' '           #2del3
     C                   move      ' '           #2del4
     C                   move      ' '           #2del5
    C*
     C                   call      'FA742R'
     C                   parm                    #2co#
     C                   parm                    #2sub#
     C                   parm                    #2fnd
     C                   parm                    #2fyr
     C                   parm                    #2case
     C                   parm                    #2eob#
     C                   parm                    #2rcn
     C                   parm                    #2del1
     C                   parm                    #2del2
     C                   parm                    #2del3
     C                   parm                    #2del4
     C                   parm                    #2del5
    C*
     C                   endsr
    C*================================================================
    C* PREAUT - Pay the preauthorized amount
    C*================================================================
     C     preaut        begsr
     C*
    C*
    C* PAY THE PREAUT AMOUNT EVEN IF GREATER THAN WHAT FS ALLOWS
    C*
     C                   if        popamt <= cachrg
     C                             and capaid > 0
     C*
     C     prky1         setgt     ppr01
     C     prky2         readpe    ppr01                                  99
     C                   if        not *in99
     C                   if        pr1a2=' ' and ##splt='N'
     C                   eval      xxpaid = popamt * catota
     C                   else
     C                   eval      xxpaid = popamt
     C                   endif
     C                   else
     C                   eval      xxpaid = popamt
     C                   endif
     C*
     C                   eval      caothr = cachrg - camra - xxpaid
     C                   move      'PRC'         caot1c
    C*
     C                   exsr      remeob
     C                   move      'P02'         xxeob
     C*
     C* Set a flag in FDCAP so this record will not take network reductions
     C*
     C                   eval      caipop = 'P'
    C*
     C                   endif
    C*
     C                   if        popamt > cachrg
     C                             and capaid > 0
    C
     C                   eval      xxpaid = cachrg
     C                   eval      caothr = 0
     C                   eval      camra  = 0
    C*
     C                   exsr      remeob
     C*
     C* Set a flag in FDCAP so this record will not take network reductions
     C*
     C                   eval      caipop = 'P'
    C*
     C                   endif
    C*
     C                   endsr
     *********************************************************************
     *** Remove  - Remove adjudication messages/EOMB Codes  R E M M S GN T
     *********************************************************************
     C     remeob        begsr
    C*
    C* Remove adjudication EOMB codes from Detail EOB/NOTES
    C*
     C     deky3         chain     pde01                              99
     C                   dow       *in99 = *off
     C                   if        deesrc = 'A'
     C                   delete    pde01
     C                   endif
     C     deky3         reade     pde01                                  99
     C                   enddo
    C*
     C                   endsr
    C*================================================================
    C* UPDATE - Subroutine to update Files
    C*================================================================
     C     update        begsr
    C*
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
    C*
    C* FDDEP - GET NEXT DETAIL SEQUENCE NUMBER
    C*
     C                   if        xxeob <> *blanks
    C*
     C                   eval      kkseq# = *hival
     C     deky1         setgt     pde02
     C     deky2         readpe    pde02                                  99
     C                   if        *in99 = *off
     C                   eval      xxseq# = deseq# + 1
     C                   else
     C                   eval      xxseq# = 1
     C                   endif
     C                   clear                   pde02
    C*
     C                   move      caco#         deco#
     C                   move      casub#        desub#
     C                   move      cafnd         defnd
     C                   move      cafyr         defyr
     C                   move      cacase        decase
     C                   move      cass#         dess#
     C                   move      caacdt        deacdt
     C                   move      caeob#        deeob#
     C                   move      cadseq        dedseq
     C                   eval      deseq# = xxseq#
     C                   move      xxeob         deeob
     C                   move      upco#         kkco#
     C                   movel(p)  deeob         kkkey
     C     ebky          chain     peb01                              99
     C                   movel(p)  ebdesc        denot1
     C                   clear                   denot2
     C                   move      'A'           destat
     C                   move      'P'           deesrc
     C                   eval      dercn = carcn
     C                   move      q1user        deuser
     C                   move      q1pgm         depgm
     C                   move      q1user        deiusr
     C                   move      q1pgm         deipgm
     C                   movel     srdto         dedt
     C                   movel     srdto         deidt
     C                   eval      detime = dstime
     C                   eval      deitim = dstime
    C*
     C                   write     pde02
    C*
     C                   endif
    C*
     C                   eval      capaid = xxpaid
    C*
     C                   movel     srdto         cadt
     C                   eval      catime = dstime
     C                   movel(p)  q1user        causer
     C                   movel(p)  q1pgm         capgm
    C*
     C                   update    pca01
    C*
     C                   endsr
    C*
    C*================================================================
    C* WRTMSG - Subroutine to write FDMGP file
    C*================================================================
     C     wrtmsg        begsr
     C                   eval      xxdseq = cadseq
     C                   eval      r = 1
     C                   move      '00000'       pchld
     C                   move      hlmsg#        pchld
     C     pchld         lookup    per1(r)                                61
     C                   movel(p)  per2(r)       xxmsg#
    C*
    C* CHECK TO SEE IF ERROR ALREADY EXISTS
    C*
     C     mgky2         chain     pmg02                              99
     C                   if        *in99 = *on
    C*
    C* GET NEXT AVAILABLE MESSAGE SEQUENCE NUMBER
    C*
     C                   move      *hival        xxmseq
     C     mgky1         setgt     pmg01
     C     mgky          readpe    pmg01                                  98
     C                   if        *in98 = *off
     C                   eval      hlmseq = 1 + mgmseq
     C                   else
     C                   eval      hlmseq = 1
     C                   endif
     C                   clear                   pmg02
     C                   eval      mgco# = upco#
     C                   eval      mgsub# = upsub#
     C                   eval      mgfnd = upfnd
     C                   eval      mgfyr = qqfyr
     C                   eval      mgcase = qqcase
     C                   eval      mgdcs# = 0
     C                   eval      mgdseq = cadseq
     C                   eval      mgmseq = hlmseq
     C                   eval      mgeob# = caeob#
     C                   movel     xxmsg#        mgmsg#
     C                   eval      mgrcn = eorcn
    C*
    C* TRANSLATE 5 BYTE PRE-CERT ERROR ID TO 7 BYTES
    C*
    C*
     C                   move      'P'           mgsrc
     C                   move      'A'           mgstat
     C                   movel(p)  q1user        mguser
     C                   movel(p)  q1user        mgiusr
     C                   movel(p)  q1pgm         mgpgm
     C                   movel(p)  q1pgm         mgipgm
     C                   time                    dsdttm
     C                   eval      mgtime = dstime
     C                   eval      mgitim = dstime
     C                   eval      srdti = dsdate
     C                   clear                   srdto
     C                   clear                   srerr
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         mgdt
     C                   movel     srdto         mgidt
     C                   write     pmg02
     C                   endif
     C                   endsr
**  PER1/PER2: PRECERT ERROR TRANSLATION TABLE FROM LENGTH 5 TO 7
PC001PC00001
PC002PC00002
PC003PC00003
PC004PC00004
PC005PC00005
PC006PC00006
PC007PC00007
PC008PC00008
