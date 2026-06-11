     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
    H/TITLE SR04602B - CA Small Deductible Endorsement Print/Attach
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H bnddir('WTBNDDIR')
     *===============================================================*
     * WC040602B                                                     *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   SINGLE MANUAL ADDITIONAL INPUT                              *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 07/25/24                                          *
     * Programmer: C:333                                             *
     * Project   : WC2978                                            *
     *===============================================================*
     * REVISIONS:                                                    *
      *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: Please add comments to the top of revisions.           *
     * ------------------------------------------------------------- *
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
     fwtabl     if   e           k disk    include(pbs01)
     fwmldl1    if   e           k disk
     Fwxpel     uf a e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)
     Fststl3    if   e           k disk
     Fwtddl2    if   e           k disk
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
     D qqForm          s             15a   inz('WC040602B ')
     D xxampo          s                   like(FMAM01)
     D xxamag          s                   like(FMAM01)
     D xxalae          s                   like(FMFL01)
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
     D addpgm          c                   const('SR04602IB ')
    
      //Entry
     D SR04602B        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR04602B        PI
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

     D rtvSmDedInfo    pr
     D  ampo                               like(FMAM01)
     D  amag                               like(FMAM02)
     D  alae                               like(FMFL01)

       Dcl-Pr SREFFD  EXTPGM;
        strCo#   char(3);
        strFnd   char(3);
        strFyr   char(3);
        strEmp#  char(9);
        strDiv   char(5);
        strDate  char(7);
       End-Pr;

     *==================================================================
     *  PLIST definitions
     *==================================================================
     ** Additional input program parameters
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
      /Free
          setll (xxco#:xxfnd:xxfyr:xxemp#) pel02;
          reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;
          DOW not %eof;
             xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
             If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
                Chain (Elco#:ELrtcd) prs01;
                If (RSjrst = ENst or ENst = *blanks);
                   If RtvDedSize(ELco#:ELbscd) = 'S';
                      RtvSmDedInfo(xxAmpo:xxAmag:xxAlae);
                      FMam01 = xxAmpo;
                      FMam02 = xxAmag;
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
             Endif;
          reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;
          Enddo;
      /End-Free

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
        If RtvDedSize(ELco#:ELbscd) = 'S';
           Clear xxAmpo;
           Clear xxAmag;
           RtvSmDedInfo(xxAmpo:xxAmag:xxAlae);
           If xxAmPO > 0;
              crtFrm = *On;
           Endif;
        Endif;
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
       //*-------------------------------------------------------------
       //* RtvSmDedInfo - Retrieves DDAMPO and DDAMAG
       //*-------------------------------------------------------------
     p RtvSmDedInfo    b
     d RtvSmDedInfo    pi
     d  @@ampo                             like(FMAM01)
     d  @@amag                             like(FMAM02)
     d  @@alae                             like(FMFL01)

        Dcl-S kkEfDt packed(7:0);
        Dcl-S inCo#   char(3);
        Dcl-S inFnd   char(3);
        Dcl-S inFyr   char(3);
        Dcl-S inEmp#  char(9);
        Dcl-S inDiv   char(5);
        Dcl-s outDate  char(7);

        inCo#  = %Editc(ELco#:'X');
        inFnd  = %Editc(ELfnd:'X');
        inFyr  = %Editc(ELfyr:'X');
        inEmp# = %Editc(ELemp#:'X');
        inDiv  = %Editc(ELdiv:'X');
        outDate = %Editc(ELdcEf:'X');

        SREFFD(inCo#:inFnd:inFyr:inEmp#:inDiv:outDate);
        kkEfDt = %int(outDate);

        Setgt (ELco#:ELfnd:RSjrst:ELdcDe:kkEfDt) pdd01;
        ReadPe (ELco#:ELfnd:RSjrst:ELdcDe) pdd01;
        Dow not %eof(WTDDL2) and DDstat <> 'A';
           ReadPe (ELco#:ELfnd:RSjrst:ELdcDe) pdd01;
        Enddo;

        If not %eof(WTDDL2);
           @@AmPo = DDampo;
           @@AmAg = DDamag;
           @@AlAe = DDalae;
        Endif;

     p rtvSmDedInfo    e
