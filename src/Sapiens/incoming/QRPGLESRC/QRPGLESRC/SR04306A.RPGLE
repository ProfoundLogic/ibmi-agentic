       //============================================================
       Ctl-Opt Copyright('(c) 2025 Sapiens. All Rights Reserved.')
        Option(*NoDebugIO:*SrcStmt)
        DftActGrp(*No)
        ActGrp(*Caller)
        BndDir('WTBNDDIR');
     h/Title SR04306 - Print / Attach - CA Waiver of Subro - WC040306
       //============================================================
       // Program Description:
       //
       //   Determines if this endorsement form should be attached to
       //   the policy and creates data in the Endorsement Form
       //   Workfile (WXWXFMP) prior to a form merge.
       //
       // PROGRAMMER: C:333
       // CREATED:    10/27/22            PROJECT: WC2834
       //=============================================================
       // Revisions:
       //                                                             *
       // 08/21/25 C:333 Support: 148/117703  (Recompile)             *
       //                For single print endorsements provide specific
       //                group and employer so that correct sig is found
       //                                                             *
       // 08/24/23 C:333 Support: 110561-099                          *
       //                Add Endorsment # 0 if it does not exist      *
       //
       // 08/07/23  C:333 Support: 113432
       //                 SR040306A is not writing the Endorsement Form ID
       //                  to WXWXFMP.
       //
       // 05/24/23  C:333 Project: 148WEC
       //                 Auto Attach Waiver Form
       //                 - Template xml is running twice.
       //
       //=============================================================
       // NOTES:
       //  Utilizes the endorsement copybook (SCOPYEN01F/D/C).
       //  qqmode = 1 - Called to determine if form should be attached
       //  qqmode = 2 - Called to print endorsement.
       //  qqmode = 3 - Called for additional input.
       //  qqmode = 4 - Called for midterm processing.
       //=============================================================
     fwmmula    if   e           k disk    rename(pmu01:pmu0a)
     fWMWVL6    IF   E           K DISK
     f                                     RENAME(PWV01:PWV06)
     fWMWVL7    IF   E           K DISK
     f                                     RENAME(PWV01:PWV07)
     f*** Beg Del *** 08/27/23 *********************************************
     f***WXFMP3    O    E             DISK
     f*** End Del *** 08/27/23 *********************************************
     fWXPEL     UF A E           K DISK
     fWMSHL1    IF   E           K DISK
     fwxfmp1    if   e           k disk    usropn
     f                                     rename(pfm01:pfm02)

       // Copybook F specs
     f/Copy sCopyEn01f
       //=============================================================
       // Definition Specs
       //=============================================================
      *=====================================================
      * Procedure Prototypes
      *=====================================================
     D WriteWXPEP      pr
     D GetWvrPct       pr             4

       Dcl-Pr SR04306A;
        *N Char(3);
        *N Char(3);
        *N Char(9);
        *N Char(5);
        *N Char(3);
        *N Char(1);
        *N Packed(9:0);
        *N Char(1);
       End-Pr;
       Dcl-Pi SR04306A;
        qqCo# Char(3);
        qqFnd Char(3);
        qqEmp# Char(9);
        qqDiv Char(5);
        qqFyr Char(3);
        qqMode Char(1);
        qqRrn Packed(9:0);
        qqMod2 Char(1);
       End-Pi;
     D isPrtOnly       s               n   inz(*off)
     D wvrCnt          s              1  0 inz(0)
     D isFirst         s               n   inz(*on)
     D qq306a          s              1a
     D XXUNID          s                   like(PEUNID)
     D X1END#          s                   like(PEEND#)

      *
     D                 DS
     D  NAME                   1    100
     D  WVNAME                 1     40
     D  WVNAM2                41    100
     D                 DS
     D  SNAME                  1    100
     D  SNAM1                  1     50
     D  SNAM2                 51    100


       // Copybook D specs
     d/Copy sCopyEn01d
     d/copy wPrEndorse
     d/COPY WWCPOLDAT3

       //=============================================================
       // Copybook required constants
       //=============================================================
       // Form ID
       Dcl-C qqForm Const('WC040306');
       // Additional entry program
       Dcl-C addPgm Const('W0815R');
       //=============================================================
       // Main
       //=============================================================
      *
     C     SHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    X1TYPE
      *
     C     WVKY6         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
      *
     C     WVKY7         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    LDEND#
      *
     C     PEKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXUNID
     C                   KFLD                    X1END#
      *
       // Copybook C specs (PLISTs, KLISTs and mainline)
     c/Copy sCopyEn01C
       //=============================================================
       // OthInf - Subroutine to load form specific information
       //=============================================================
       Begsr OthInf;
        //*** Beg Del ***** 08/07/23 *************************************
        //***clear PFM03;
        //*** End Del ***** 08/07/23 *************************************
      *
        If QQmode = '4' or FMend# <> *Zeros;
           Exsr Info1;
        Else;
           Exsr Info2;
        Endif;
      *
       Endsr;

      *================================================================***
      * INFO1 - Get OTHINF MID-TERM Endorsement records
      *================================================================***
     C     INFO1         BEGSR
     C                   MOVE      'N'           XXWRT             1
     C                   MOVE      *BLANK        XXFRST            1
      *
      * Look for waivers.
      *
     C     WVKY7         CHAIN     PWV07                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     WVSTAT        IFNE      'I'                                          NOT INACTIVE
     C     WVEMST        ANDEQ     'CA'                                         NOT CALIF
     C     WVENM         IFEQ      'WC040306'
     C     WVENM         OREQ      *BLANKS
     C                   Z-ADD     WVCPCT        FMA921
     C                   If        WVswpc <> 0
     C                   MoveL(P)  WVswPc        ##CPCT
     C                   Else
     C                   MoveL(P)  ELwvPc        ##CPCT
     C                   EndIf
     C                   CALL      'WA003R'
     C                   PARM                    ##CPCT           80
     C                   PARM      'X,XXX.XX'    ##MSK            10
     C     '   '         CAT(P)    ##CPCT        FMC201
     C                   CAT(P)    '%':0         FMC201
     C                   MOVE      NAME          SNAME
     C                   MOVEL     SNAM1         FM1F01
     C                   MOVEL     SNAM2         FM1F02
     C                   MOVEL     WVADR1        FMC401
      *
     C     WVADR2        IFNE      *BLANK
     C                   MOVEL     WVADR2        FMC402
     C                   ENDIF                                                  WVADR2 <> ' '
      *
     C                   MOVE      WVZIP         XXZIP
     C     WVCTY         CAT       ',':0         FMC403
     C                   CAT       WVST:1        FMC403
     C                   CAT       XXZIP1:1      FMC403
      *
     C     XXZIP2        IFNE      *BLANK
     C     XXZIP2        ANDNE     *ZERO
     C                   CAT       '-':0         FMC403
     C                   CAT       XXZIP2:0      FMC403
     C                   ENDIF                                                  XXZIP2 <> ' '
      *
     C     FMC402        IFEQ      *BLANKS
     C                   MOVEL(P)  FMC403        FMC402
     C                   MOVEL(P)  *BLANKS       FMC403
     C                   ENDIF
      *
      * Load the project lines.
      *
        FMC601 = WVproj;
        FMC602 = WVprj2;
        FMblk01 = WVblkPrjDs;
        //*** Beg Add *** 08/12/22 ********************************************
        FMblk02 = %Subst(WVperOrg:1:500);
        FMblk03 = %Subst(WVperOrg:501:200);
        Open wxfmp1;
        Chain (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv:xxForm) pfm02;
        If %Found(WXFMP1);
           FMC440 = F3c401;
           FMC441 = F3c402;
           FMC442 = F3c403;
        Endif;
        Close wxfmp1;
        //*** End Add *** 08/12/22 ********************************************
      *
      * Write the record.
      *
       Exsr WriteWorkFile;
       //*** Beg Add ***** 05/24/23 *******************************
       WriteInCopybook = *Off;
       //*** End Add ***** 05/24/23 *******************************
     C                   MOVE      'Y'           XXWRT             1
      *
     C                   MOVE      *BLANKS       FM1F01
     C                   MOVE      *BLANKS       FM1F02
     C                   MOVE      *BLANKS       FMC401
     C                   MOVE      *BLANKS       FMC402
     C                   MOVE      *BLANKS       FMC403
       FMblk01 = *Blanks;
        //*** Beg Add *** 08/12/22 ********************************************
       FMblk02 = *Blanks;
       FMblk03 = *Blanks;
        //*** Beg Add *** 08/12/22 ********************************************
     C                   MOVE      *BLANKS       FMC601
     C                   MOVE      *BLANKS       FMC602
     C                   ENDIF                                                  WVEMST =WC040306
     C                   ENDIF                                                  WVSTAT <>'I'
      *
     C     WVKY7         READE     PWV07                                  60
     C                   ENDDO
      *
        //*** Beg Del ***** 08/07/23 *************************************
     C***  XXWRT         IFEQ      'Y'
     C***                WRITE     PFM03
     C***                ENDIF
        //*** End Del ***** 08/07/23 *************************************
      *
     C                   ENDSR                                                  INFO1
      *================================================================***
      * INFO2 - Get OTHINF Endorsement records if NOT Mid-Term processing
      *================================================================***
     C     INFO2         BEGSR
      *
     C                   MOVE      'N'           XXWRT             1
     C                   MOVE      *BLANK        XXFRST            1

      *
      * Find the unique ID number.
      *
     C                   MOVEL     XXFORM        X1END#
     C                   MOVE      'P'           X1TYPE            1
     C                   Z-ADD     0             XXUNID
     C     SHKEY         CHAIN     PSH01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     SHUNID        XXUNID
     C                   ENDIF
      *
      * Look for waivers.
      *
       /Free
        Setll wvKy6 pwv06;
        Dou %Eof(WMWVL6);
           Reade wvKy6 pwv06;
           If %Eof;
              Leave;
           EndIf;
           If WVenm <> xxForm;
              Iter;
           Endif;
       /End-Free
      *
     C     WVSTAT        IFNE      'I'                                          NOT INACTIVE
     C     WVENM         IFEQ      'WC040306'
     C     WVENM         OREQ      *BLANKS
     C                   Z-ADD     WVCPCT        FMA921
     C                   If        WVswpc <> 0
     C                   MoveL(P)  WVswPc        ##CPCT
     C                   Else
     C                   MoveL(P)  ELwvPc        ##CPCT
     C                   EndIf
     C                   CALL      'WA003R'
     C                   PARM                    ##CPCT           80
     C                   PARM      'X,XXX.XX'    ##MSK            10
     C     '   '         CAT(P)    ##CPCT        FMC201
     C                   CAT(P)    '%':0         FMC201
     C                   MOVE      NAME          SNAME
     C                   MOVEL     SNAM1         FM1F01
     C                   MOVEL     SNAM2         FM1F02
     C                   MOVEL     WVADR1        FMC401
      *
     C     WVADR2        IFNE      *BLANK
     C                   MOVEL     WVADR2        FMC402
     C                   ENDIF                                                  WVADR2 <> ' '
      *
     C                   MOVE      WVZIP         XXZIP
     C     WVCTY         CAT       ',':0         FMC403
     C                   CAT       WVST:1        FMC403
     C                   CAT       XXZIP1:1      FMC403
      *
     C     XXZIP2        IFNE      *BLANK
     C     XXZIP2        ANDNE     *ZERO
     C                   CAT       '-':0         FMC403
     C                   CAT       XXZIP2:0      FMC403
     C                   ENDIF                                                  XXZIP2 <> ' '
      *
     C     FMC402        IFEQ      *BLANKS
     C                   MOVEL(P)  FMC403        FMC402
     C                   MOVEL(P)  *BLANKS       FMC403
     C                   ENDIF
      *
      * Load the project lines.
      *
        FMC601 = WVproj;
        FMC602 = WVprj2;
        FMblk01 = WVblkPrjDs;
        //*** Beg Add *** 08/12/22 ********************************************
        FMblk02 = %Subst(WVperOrg:1:500);
        FMblk03 = %Subst(WVperOrg:501:200);
        Open wxfmp1;
        Chain (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv:xxForm) pfm02;
        If %Found(WXFMP1);
           FMC440 = F3c401;
           FMC441 = F3c402;
           FMC442 = F3c403;
        Endif;
        Close wxfmp1;
        //*** End Add *** 08/12/22 ********************************************
      *
      * Write the record.
      *
      /Free
       Exsr WriteWorkFile;
      /End-Free
       //*** Beg Add ***** 08/24/23 *******************************
       WriteInCopybook = *Off;
       //*** End Add ***** 08/24/23 *******************************
     C                   MOVE      'Y'           XXWRT             1
      *
      * Update the endorsement reporting file with the proper data.
      *
     C                   EXSR      ENDRPT
      *
     C                   MOVE      *BLANKS       FM1F01
     C                   MOVE      *BLANKS       FM1F01
     C                   MOVE      *BLANKS       FMC401
     C                   MOVE      *BLANKS       FMC402
     C                   MOVE      *BLANKS       FMC403
       FMblk01 = *Blanks;
     C                   MOVE      *BLANKS       FMC601
     C                   MOVE      *BLANKS       FMC602
       FMblk01 = *Blanks;
        //*** Beg Add *** 08/12/22 ********************************************
       FMblk02 = *Blanks;
       FMblk03 = *Blanks;
        //*** Beg Add *** 08/12/22 ********************************************
     C                   ENDIF                                                  WVEMST =WC040306
     C                   ENDIF                                                  WVSTAT <>'I'
      *
     C                   ENDDO

     C                   EXSR      ENDRPT

        //*** Beg Del ***** 08/07/23 *************************************
     C***  XXWRT         IFEQ      'Y'
     C***                WRITE     PFM03
     C***                ENDIF
        //*** End Del ***** 08/07/23 *************************************
      *
     C                   ENDSR                                                  INFO2
      *================================================================***
      * ENDRPT - Update Endorsement reporting file
      *================================================================***
      **Find*the*unique ID number.
      **Copy*data into the record.

      /Free
       // Don't write WXPEP if print only or no WMWVP records exist
       Begsr EndRpt;
       If not printingPolicy;
          LeaveSr;
       Endif;
       If isFirst and wvrCnt = 0 and %Eof(WMWVL6);
          LeaveSr;
       Endif;
       // Write out the odd record if %EOF is hit
       If wvrCnt = 1 and %Eof(WMWVL6);
          WriteWXPEP();
          LeaveSr;
       Endif;
       // It takes two records to fill the WCPOLS record...
       //  The first one fills the name and percentage
       //  The second one fills the second name and write the record
       If not(%Eof(WMWVL6));
          wvrCnt += 1;
       Endif;
       If wvrCnt = 1;
          x6nm1 = WVname;
          x6nm2 = *blanks;
          x6pct = GetWvrPct();
          LeaveSr;
       Endif;
       If wvrCnt = 2;
          wvrCnt = 0;
          x6nm2  = WVname;
          WriteWXPEP();
          Clear e04306;
       Endif;
       Endsr;

       //============================================================
       //  Pgmstr - Beginning of program processing
       //============================================================
       Begsr PgmStr;
          UseDfltAddparms = *Off;
       Endsr;
       //============================================================
       //  PgmEnd - End of program processing
       //============================================================
       Begsr PgmEnd;
       Endsr;
       //============================================================
       //  Valid - Subroutine to attach this form to the policy
       //============================================================
       Begsr Valid;

      * VALIDATE ATTACHMENT OF FORM HERE
        If q1Prms >= 9;
           qq306a = *Off;
        Endif;

      *
      * Look for any waivers.
      *
      *  mid-term process
      *
        SetLL WVKY7 pwv07;
        ReadE WVKY7 pwv07;
      *
        Dow not %eof(WMWVL7);
           If wvStat <> 'I' and wvEmst = 'CA';
              If wvEnm = 'WC040306' or wvEnm = *Blanks;
                 crtFrm = *On;
                 If q1Prms >= 9;
                    qq306a = *On;
                 EndIf;
                 Leave;
              EndIf;
           EndIf;

           ReadE WVKY7 pwv07;
        EndDo;
        //*** Beg Add *** 08/24/23 ******************************************
        // Ensure waiver policy page endorsement exists for MUend# = 0
        If LDendA <> *zeros and LDendA <> *blanks;
           If LDend# > 0;
              Open WMMUL4;
              Chain (xxCo#:xxFnd:xxEmp#:xxFyr:ENform:0) pmu0a;
              If Not(%Found);
                 MUco#  = xxCo#;
                 MUfnd  = xxFnd;
                 MUemp# = xxEmp#;
                 MUfyr  = xxFyr;
                 MUform = ENform;
                 MUend# = 0;
                 MUseq# = 0;
                 MUenSt = ENst;
                 MUdesc = ENdesc;
                 MUdsc2 = ENdsc2;
                 MUeEdt = ELpfDt;
                 If LDend# > *zeros;
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         MUeEdt
                 Endif;
                 MUexFl = 'I';
                 MUmnAu = ENmnAu;
                 MUprnt = ENprnt;
                 MUstat = 'A';
                 MUuser = Q1user;
                 MUpgm  = Q1pgm;
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         MUdt
                 MUtime = %dec(%time():*HMS);
                 MUiUsr = MUuser;
                 MUiPgm = MUpgm;
                 MUiDt  = MUdt;
                 Write pmu01;
              Endif;
              Close WMMUL4;
           Endif;
        Endif;
        //*** End Add *** 08/24/23 ******************************************
      *
      *  END OF VALIDATION
      *
       Endsr;
      *====================================================
      * Retrieves waiver percentage
      *====================================================
     PGetWvrPct        b
     DGetWvrPct        pi             4
     DreturnPct        s              4
     C                   Select
     C                   When      WVswpc <> 0
     C                   Move      WVswpc        returnPct
     C                   Other
     C                   Move      ELwvpc        returnPct
     C                   Endsl
     C                   Return    returnPct
     PGetWvrPct        e
      *====================================================
      * Writes WXPEP
      *====================================================
     PWriteWXPEP       b
      /Free
       Chain pekey2 ppe01;
       If %found;
          PEdata = E04306;
          If isFirst;
             isFirst = *off;
             Update ppe01;
          Else;
             Write  ppe01;
          Endif;
       Endif;
      /End-free
     PWriteWXPEP       e

