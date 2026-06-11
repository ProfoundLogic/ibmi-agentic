     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
    H/TITLE SR04603B - CA - Large Risk Deductible Endorsement
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H bnddir('WTBNDDIR')
     *===============================================================*
     * WC040603B                                                     *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   SINGLE MANUAL ADDITIONAL INPUT                              *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 10/25/17                                          *
     * Programmer: C:291                                             *
     * Project   : C150WCB                                           *
     *===============================================================*
     * REVISIONS:                                                    *
      *                                                               *
      * 10/31/22 C:333 Project: WC2834                                *
      *                Endorsement Recognition of Cancellation        *
      *                                                               *
      * 07/06/22 C:136 Project: WC2868                                *
      *                Ensure LD setup on div before auto attach      *
      *                                                               *
      * 07/23/19 C:300 Support: 80418 (SUNZ)                          *
      *                Manually attached form not printing            *
      *                When CA is not division 0                      *
      *                                                               *
      * 05/30/18 C:65  Project: 150WAD/WC2607                         *
      *                Implement logic to store data for type 43 rcds.*
     *                                                               *
     * 04/19/18 C:062 Project: 150WBL (63861)                        *
     *                -Call new additional info program.             *
     *                -Default the following.                        *
     *                  =Ded Amt = LDAMP1/2 = FMAM01                 *
     *                  =Agg Amt = LDAMG1/2 = FMAM02                 *
     *                  =ALAE    = LDALA1/2 = FMFL01                 *
     *                  =Fixed   = Exclude  = FMFL02                 *
     *                                                               *
     * 01/15/18 C:291 Support: 51608                                 *
     *                Change attachment criteria to attach to LD     *
     *                policy and zero Aggregate.                     *
     *                                                               *
     *===============================================================*
     * Notes:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
     *                                                               *
     *  QQMODE = 1 - Called to determine if form should be attached. *
     *  QQMODE = 2 - Called to print endorsement.                    *
     *  QQMODE = 3 - Called for additional input.                    *
     *  QQMODE = 4 - Called for mid-term processing.                 *
     *===============================================================*
    
     ** Copybook F specs
     F/COPY SCOPYEN01F
    
     ** Additional Information File
     Fwxfmp1    if   e           k disk    usropn
     F                                     rename(pfm01:pfm02)
     C***Beg Add *** 01/15/18 ************************
     fwtabl     if   e           k disk    include(pbs01)
     fwmldl1    if   e           k disk
     C***End Add *** 01/15/18 ************************
     F*** begin add ***  05/30/18  ***************************************
     Fwxpel     uf a e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)
     Fststl3    if   e           k disk
     F*** end add *****  05/30/18  ***************************************
    
     *================================================================
     *  Definition Specs
     *================================================================
     ** Copybook D specs
     D/COPY SCOPYEN01D
    
     *================================================================
     *  /COPY required constants
     *================================================================
    
     ** Form ID
     D qqForm          s             15a   inz('WC040603B ')
     D xxampo          s                   like(FMAM01)
     D xxamag          s                   like(FMAM01)
     D xxAlae          s                   like(FMFL01)
     D xxunid          s                   like(peunid)
     D x1end#          s                   like(peend#)
     d xxpremPct       s             12  9 inz(0)
     d xxlossSub       s              2  0 inz(0)
     d xxbasisDed      s              2  0 inz(0)
     d xxdedPct        s              2  0 inz(0)
     d xxdedAmt        s              9  0 inz(0)
     d xxdedAgg        s              9  0 inz(0)
     d xxdedClas       s              5  0 inz(0)
     d xxdedNet        s              1    inz(' ')
     d xxdedVerb       s              1    inz(' ')
     d xxPlan          s              1    inz(' ')
    
     ** Additional entry program
     D addpgm          c                   const('SR04603IB ')
    
      //Entry
     D SR04603B        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR04603B        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
     D rtvDedSize      pr             1                                         Returns S/L
     D  co#                                like(ELCO#)
     D  bscd                               like(ELBSCD)

     D rtvLgDedInfo    pr
     D  ampo                               like(FMAM01)
     D  amag                               like(FMAM02)
     D  alal                               like(FMFL01)
     *==================================================================
     *  PLIST definitions
     *==================================================================
     ** Additional input program parameters
      //***** Beg Del *** 10/31/22 **********************************
     D***/COPY WPREMPLR
      //***** End Del *** 10/31/22 **********************************
      /COPY WWCPOLDAT4

     C     stky          KList
     C                   KFld                    xxkey            10
     C     pekey         KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxdt              7 0
     C     pekey1        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C     pekey2        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxunid
     C                   KFld                    x1end#
    
     *==================================================================
     *  KLIST definitions
     *==================================================================
     ** Additional Input
    
     C     fmky3         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    k1form           15
     *==================================================================
     *  Main
     *==================================================================
     ** Copybook C specs
     C/COPY SCOPYEN01C
     *==================================================================
     *  OTHINF - Subroutine to load form specific information & write
     *==================================================================
     C     othinf        begsr
    
     C                   open      wxfmp1
    
     C                   movel(p)  'WC040603 '   k1form
     C     fmky3         chain     pfm02                              60
    
      /Free
          setll (xxco#:xxfnd:xxfyr:xxemp#) pel02;
          reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;
          DOW not %eof;
       //***** Beg Add *** 10/31/22 ******************************
             xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
             If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
       //***** End Add *** 10/31/22 ******************************
                Chain (Elco#:ELrtcd) prs01;
                If (RSjrst = ENst or ENst = *blanks);
                   If RtvDedSize(ELco#:ELbscd) = 'L';
                      RtvLgDedInfo(xxAmpo:xxAmag:xxAlae);
                      FMam01 = xxAmpo;
                      FMam02 = xxAmag;

                      FMam03 = F3am03;
                      If xxAlae = 'Y';
                         FMc101 = 'Included';
                      Else;
                         FMc101 = 'Excluded';
                      EndIf;
                      FMc102 = 'Will Not';
                      Exsr WriteWorkFile;
                      Exsr WriteWCPOLS;
                      Leave;
                   Endif;
                Endif;
          //***** Beg Add *** 10/31/22 ******************************
             Endif;
          //***** End Add *** 10/31/22 ******************************
          reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;
          Enddo;
      /End-Free

     C                   close     wxfmp1

     C                   endsr
     *===================================================================
     *  PGMSTR - Beginning of program processing
     *===================================================================
     C     pgmstr        begsr
     C                   Eval      WriteInCopybook = *Off
     C                   endsr
     *===================================================================
     *  PGMEND - End of program processing
     *===================================================================
     C     pgmend        begsr
     C                   endsr
     *===================================================================
     *  VALID - Subroutine to attach this form to the policy
     *===================================================================
       Begsr Valid;
       //*** Beg Add ***** 07/06/22 ************************
        If RtvDedSize(ELco#:ELbscd) = 'L';
           RtvLgDedInfo(xxAmpo:xxAmag:xxAlae);
           If xxAmPO <> 0;
              crtFrm = *On;
           Endif;
        Endif;
       //*** End Add ***** 07/06/22 ************************
       Endsr;
      *=========================================================================
      *  WriteWCPOLS - record information for electronic data record
      *=========================================================================
     C     WriteWCPOLS   Begsr
      *  If printing a policy page.  (Not called from W1054R)

     c                   call      'SRCHKPGM'
     c                   parm      'W1054R   '   #1pgm            10
     c                   parm      *BLANK        #1flag            1

     C                   if        #1flag = '0'

     C                   Clear                   xxpremPct
     C                   Clear                   xxlossSub
     C                   Clear                   xxbasisDed
     C                   Clear                   xxdedPct
     C                   Clear                   xxdedAmt
     C                   Clear                   xxdedAgg
     C                   Clear                   xxdedClas
     C                   Clear                   xxdedNet
     C                   Clear                   xxdedVerb
     C                   Clear                   xxPlan
      /free
       xxPlan = RtvDeductible(elco#:elfnd:elemp#:eldiv:elfyr:xxPremPct:
        xxDedAmt:xxDedAgg:xxLossSub:xxBasisDed:xxDedNet:xxDedVerb:
        xxDedClas:xxDedPct);
      /end-free
      *
      * Find the unique ID number.
      *
     C                   MOVE      'P'           X1TYPE            1
     C                   Z-ADD     0             xxunid
     C*
     c                   move      *hival        xxdt
     C     pekey         setgt     ppe02
     C     pekey1        readpe    ppe02                                  60
     C                   If        *in60 = *off
     C                   z-add     peunid        xxunid
     C                   endif
     C                   movel     xxform        x1end#
     c     pekey2        chain     ppe01                              60
      *
      * Copy data into the record.
      *
     C                   If        *in60 = *off
      *
     c                   movel(p)  rsjrst        xxkey
     c     stky          chain     pst01                              60
     c                   if        *in60 = *off
     c                   move      stnst         PEST
     c                   endif
      *
     c                   clear                   E00403
     C                   eval      xxdac = xxDedAmt
     C                   eval      xxprp = xxPremPct
     C                   eval      xxlsd = xxLossSub
     C                   eval      xxbdc = xxBasisDed
     C                   eval      xxdpc = xxDedPct
     C                   eval      xxdag = xxDedAgg
     C                   eval      xxalei = xxAlae
     C                   eval      xxfeci = 'N'
     c                   move      *blanks       pedata
     c                   movel     E00403        pedata
     c                   movel     q1user        peuser
     c                   movel     q1pgm         pepgm
     c                   z-add     dstime        petime
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         pedt
      *
     C                   Update    ppe01
      *
     C                   Endif                                                  *in60 ifeq off
      *
     C                   Endif
      *
     C                   Endsr
      *=========================================================================
    
      *=========================================================================
      * Query whether policy is small or large deductible
      *=========================================================================
     p rtvDedSize      b

     d rtvDedSize      pi             1
     d  co#                                like(ELCO#)
     d  bscd                               like(ELBSCD)

     d kkco#           s                   like(BSCO#)
     d kkkey           s                   like(BSKEY)

       kkco# = co#;
       kkkey = %editc(bscd:'X');
       Chain (kkco#:kkkey) pbs01;
       If %Found;
          Select;
          When BSlpgm = 'L';
             Return 'L';
          Other;
             Return 'S';
          Endsl;
       Else;
          Return *Blank;
       Endif;

     p rtvDedSize      e
      *=========================================================================
      * rtvLgDedInfo - retrieves LDAMPO and LDAMAG
      *=========================================================================
     p rtvLgDedInfo    b
     d rtvLgDedInfo    pi
     d  ampo                               like(FMAM01)
     d  amag                               like(FMAM02)
     d  alae                               like(FMFL01)
      /Free
       Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pld01;
       If %found;
          Select;
          When elpfdt >= Ldefd1 and (Elpfdt < LDexp1 or LDexp1 = *zeros);
             ampo = LDamp1;
             amag = LDamg1;
             alae = LDala1;
          When elpfdt >= Ldefd2 and (Elpfdt < LDexp2 or LDexp2 = *zeros);
             ampo = LDamp2;
             amag = LDamg2;
             alae = LDala2;
          Other;
          ampo  = *zeros;
          amag  = *zeros;
          alae = ' ';
          Endsl;
       Else;
          ampo  = *zeros;
          amag  = *zeros;
          alae = ' ';
       Endif;
      /End-Free
     p rtvLgDedInfo    e
