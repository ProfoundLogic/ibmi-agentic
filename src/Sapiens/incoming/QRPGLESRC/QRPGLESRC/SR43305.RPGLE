       //============================================================
       Ctl-Opt Copyright('(c) 2025 Sapiens. All Rights Reserved.')
        Option(*NoDebugIO:*SrcStmt)
        DftActGrp(*No)
        ActGrp(*Caller)
        BndDir('WTBNDDIR');
     h/Title SR43305 - Print / Attach - UT Waiver of Subro - WC430305
       //============================================================
       // Program Description:
       //
       //   Determines if this endorsement form should be attached to
       //   the policy and creates data in the Endorsement Form
       //   Workfile (WXWXFMP) prior to a form merge.
       //
       // PROGRAMMER: C:333
       // CREATED:    11/11/22            PROJECT: WC2834
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
       //=============================================================
       // NOTES:
       //  Utilizes the endorsement copybook (SCOPYEN01F/D/C).
       //  qqmode = 1 - Called to determine if form should be attached
       //  qqmode = 2 - Called to print endorsement.
       //  qqmode = 3 - Called for additional input.
       //  qqmode = 4 - Called for midterm processing.
       //=============================================================

     fwmmula    if   e           k disk    rename(pmu01:pmu0a)
      * Waiver
     fWMWVL6    IF   E           K DISK
     f                                     RENAME(PWV01:PWV06)
     fWMWVL7    IF   E           K DISK
     f                                     RENAME(PWV01:PWV07)
      *
     fWXFMP3    O    E             DISK
     fWXPEL     UF A E           K DISK
     fWMSHL1    IF   E           K DISK

       // Copybook F specs
     f/Copy sCopyEn01f
       //=============================================================
       // Definition Specs
       //=============================================================
     d #1PGM           s             10A   INZ('W1054R')
     d #1FLAG          s              1A   INZ
     d chrAddPgm       s                   Like(WVpgm)
     d chrPrtPgm       s                   Like(WVpgm)
     d chrState        s                   Like(WVemst)
     d chrWvBlkPrjDs   s                   Like(WVblkPrjDs)
     d QQ305           s              1A
     d XXUNID          s                   Like(PEUNID)
     d X1END#          s                   Like(PEEND#)

     d* Returns '1' in parm 2 if pgm in stack
     d SRCHKPGM        pr                  ExtPgm('SRCHKPGM')
     d                               10    const
     d                                1

       Dcl-Pr SR43305;
        *N Char(3);
        *N Char(3);
        *N Char(9);
        *N Char(5);
        *N Char(3);
        *N Char(1);
        *N Packed(9:0);
        *N Char(1);
       End-Pr;
       Dcl-Pi SR43305;
        qqCo# Char(3);
        qqFnd Char(3);
        qqEmp# Char(9);
        qqDiv Char(5);
        qqFyr Char(3);
        qqMode Char(1);
        qqRrn Packed(9:0);
        qqMod2 Char(1);
       End-Pi;
       // Copybook D specs
     d/Copy sCopyEn01d
     d/Copy WWCPOLDAT4
     d/Copy wPrEndorse
       //=============================================================
       // Copybook required constants
       //=============================================================
       // Form ID
       Dcl-C qqForm Const('WC430305');
       // Additional entry program
       Dcl-C addPgm Const('W0815R');
       //=============================================================
       // Main
       //=============================================================
      * Waiver Detail file
     C     WVKY6         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR

     C     WVKY7         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    LDEND#

     C     PEKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXUNID
     C                   KFLD                    X1END#
      * SUBMISSION HISTORY FILE
     C     SHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    X1TYPE

       // Copybook C specs (PLISTs, KLISTs and mainline)
     c/Copy sCopyEn01C
       //=============================================================
       // OthInf - Subroutine to load form specific information
       //=============================================================
       Begsr OthInf;
      *  If called from mid-term process
        If QQmode = '4' or FMend# <> *Zeros;
           Exsr Info1;
        Else;
           Exsr Info2;
        Endif;

       /Free
        // Ensure waiver policy page endorsement exists
        If xxEnd# > 0;
           Chain (xxCo#:xxFnd:xxEmp#:xxFyr:xxForm:0) pmu0a;
           If Not(%Found);
              MUend# = 0;
              MUseq# = 0;
              Open WMMUL4;
              Write pmu01;
              Close WMMUL4;
           Endif;
        Endif;
       /End-Free

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
        If Q1PRMS >= 9;
           QQ305 = *Off;
        EndIf;

      * Look for any waivers.
      *  mid-term process
        Setll WVKY7 pwv07;
        ReadE(N) WVKY7 pwv07;
        Dow not %eof(WMWVL7);
           If wvSTAT <> 'I' and wvEMST = 'UT';
              crtFrm = *On;
              if Q1PRMS >= 9;
                 QQ305 = *On;
              EndIf;
           EndIf;
           ReadE(N) WVKY7 pwv07;
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

       Endsr;
      *================================================================***
      * INFO1 - Get OTHINF MID-TERM Endorsement records
      *================================================================***
     C     INFO1         BEGSR
     C                   MOVE      'N'           XXWRT             1
     C                   MOVE      *BLANK        XXFRST            1
      * Look for waivers.
     C     WVKY7         CHAIN     PWV07                              60
     C     *IN60         DOWEQ     *OFF
     C     WVSTAT        IFNE      'I'                                          NOT INACTIVE
     C     WVEMST        ANDEQ     'UT'                                         NOT CALIF
     C                   Z-ADD     WVCPCT        FMA921
     C                   MOVEL     WVNAME        FMNM01
      /Free
       FMNM02 = %Trim(WVNAM2);
      /End-Free
     C                   MOVEL     WVADR1        FMC401
     C     WVADR2        IFNE      *BLANK
     C                   MOVEL     WVADR2        FMC402
     C                   ENDIF                                                  WVADR2 <> ' '
     C                   MOVE      WVZIP         XXZIP
     C     WVCTY         CAT       ',':0         FMC403
     C                   CAT       WVST:1        FMC403
     C                   CAT       XXZIP1:1      FMC403
     C     XXZIP2        IFNE      *BLANK
     C     XXZIP2        ANDNE     *ZERO
     C                   CAT       '-':0         FMC403
     C                   CAT       XXZIP2:0      FMC403
     C                   ENDIF                                                  XXZIP2 <> ' '
     C     FMC402        IFEQ      *BLANKS
     C                   MOVEL(P)  FMC403        FMC402
     C                   MOVEL(P)  *BLANKS       FMC403
     C                   ENDIF

      *** beg chg ***** 02/02/21 ***************************
     C***  WVPROJ        IFNE      *BLANKS
     C***                MOVEL     WVPROJ        XXPROJ
     C***                ENDIF                                                  WVPROJ<>' '
     C***  WVPRJ2        IFNE      *BLANKS
     C***                MOVEL     WVPRJ2        XXPRJ2
     C***                ENDIF                                                  WVPRJ2<>' '
        FMc601 = WVproj;
        FMc602 = WVprj2;
        FMblk01 = WVblkPrjDs;
      *** end chg ***** 02/02/21 ***************************

      * Write the record.
      *  If printing a policy page.  (Not called from W1054R)
      /Free
       SRCHKPGM(#1PGM:#1FLAG);
       If #1Flag = '0';
          PrintingPolicy = *On;
       Else;
          PrintingPolicy = *Off   ;
       Endif;
       Exsr WriteWorkfile;
      /End-Free

     C                   MOVE      'Y'           XXWRT             1
     C                   MOVE      *BLANKS       FMNM01
     C                   MOVE      *BLANKS       FMNM02
     C                   MOVE      *BLANKS       FMC401
     C                   MOVE      *BLANKS       FMC402
     C                   MOVE      *BLANKS       FMC403
      *** beg chg ***** 02/02/21 **************************
     C***                   MOVE      *BLANKS       XXPROJ
     C***                   MOVE      *BLANKS       XXPRJ2
       FMblk01 = *Blanks;
      *** end chg ***** 02/02/21 **************************
     C                   ENDIF                                                  WVSTAT <>'I'
     C     WVKY7         READE     PWV07                                  60
     C                   ENDDO
     C     XXWRT         IFEQ      'Y'
     C                   WRITE     PFM03
     C                   ENDIF
     C                   ENDSR                                                  INFO1
      *================================================================***
      * INFO2 - Get OTHINF Endorsement records if NOT Mid-Term processing
      *================================================================***
     C     INFO2         BEGSR
     C                   MOVE      'N'           XXWRT             1
     C                   MOVE      *BLANK        XXFRST            1
      * Look for waivers.
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

     C     WVSTAT        IFNE      'I'                                          NOT INACTIVE
      *** beg del ***** 02/02/21 ***********************
     C***  WVEMST        ANDEQ     'UT'                                         NOT CALIF
      *** end de. ***** 02/02/21 ***********************
     C                   Z-ADD     WVCPCT        FMA921
     C                   MOVEL     WVNAME        FMNM01
      /Free
       FMNM02 = %Trim(WVNAM2);
      /End-Free
     C                   MOVEL     WVADR1        FMC401
     C     WVADR2        IFNE      *BLANK
     C                   MOVEL     WVADR2        FMC402
     C                   ENDIF                                                  WVADR2 <> ' '
     C                   MOVE      WVZIP         XXZIP
     C     WVCTY         CAT       ',':0         FMC403
     C                   CAT       WVST:1        FMC403
     C                   CAT       XXZIP1:1      FMC403
     C     XXZIP2        IFNE      *BLANK
     C     XXZIP2        ANDNE     *ZERO
     C                   CAT       '-':0         FMC403
     C                   CAT       XXZIP2:0      FMC403
     C                   ENDIF                                                  XXZIP2 <> ' '
     C     FMC402        IFEQ      *BLANKS
     C                   MOVEL(P)  FMC403        FMC402
     C                   MOVEL(P)  *BLANKS       FMC403
     C                   ENDIF
      *** beg chg ***** 02/02/21 ***************************
     C***  WVPROJ        IFNE      *BLANKS
     C***                MOVEL     WVPROJ        XXPROJ
     C***                ENDIF                                                  WVPROJ<>' '
     C***  WVPRJ2        IFNE      *BLANKS
     C***                MOVEL     WVPRJ2        XXPRJ2
     C***                ENDIF                                                  WVPRJ2<>' '
        FMc601 = WVproj;
        FMc602 = WVprj2;
        FMblk01 = WVblkPrjDs;
      *** end chg ***** 02/02/21 ***************************
      * Write the record.
      *  If printing a policy page.  (Not called from W1054R)
      /Free
       SRCHKPGM(#1PGM:#1FLAG);
       If #1Flag = '0';
          PrintingPolicy = *On;
       Else;
          PrintingPolicy = *Off   ;
       Endif;
       Exsr WriteWorkfile;
      /End-Free

     C                   MOVE      'Y'           XXWRT             1
      * Update the endorsement reporting file with the proper data.
     C                   EXSR      ENDRPT
     C                   MOVE      *BLANKS       FMNM01
     C                   MOVE      *BLANKS       FMNM02
     C                   MOVE      *BLANKS       FMC401
     C                   MOVE      *BLANKS       FMC402
     C                   MOVE      *BLANKS       FMC403
      *** beg chg ***** 02/02/21 **************************
     C***                   MOVE      *BLANKS       XXPROJ
     C***                   MOVE      *BLANKS       XXPRJ2
       FMblk01 = *Blanks;
      *** end chg ***** 02/02/21 **************************
     C                   ENDIF                                                  WVSTAT <>'I'
     C                   ENDDO
     C     XXWRT         IFEQ      'Y'
     C                   WRITE     PFM03
     C                   ENDIF
     C                   ENDSR                                                  INFO2
      *================================================================***
      * ENDRPT - Update Endorsement reporting file
      *================================================================***
     C     ENDRPT        BEGSR
      *  If printing a policy page.  (Not called from W1054R)
      /Free
       SRCHKPGM(#1PGM:#1FLAG);
      /End-Free
     C     #1FLAG        IFEQ      '0'
      * Find the unique ID number.
     C                   MOVE      'P'           X1TYPE            1
     C                   Z-ADD     0             XXUNID
     C     SHKEY         CHAIN     PSH01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     SHUNID        XXUNID
     C                   ENDIF
     C                   MOVEL     XXFORM        X1END#
     C     PEKEY2        CHAIN     PPE01                              60
      * Copy data into the record.
     C     *IN60         IFEQ      *OFF
     C                   CLEAR                   E00313
       //*** beg chg ***** 02/02/21 ***************************
      ***/Free
       //  PEDATA = %Trimr(WVNAME) + %Trim(WVNAM2) + ' ' + WVPROJ;
      ***/End-Free
       If WVblkPrjDs <> *Blanks;
          PEdata = %Trimr(WVname) + %Trim(WVnam2) + ' ' + WVblkPrjDs;
       Else;
          PEdata = %Trimr(WVname) + %Trim(WVnam2) + ' ' + WVproj;
       Endif;
       //*** end chg ***** 02/02/21 ***************************
     C     XXFRST        IFEQ      *BLANK
     C                   UPDATE    PPE01
     C                   MOVE      'Y'           XXFRST
     C                   ELSE
     C                   WRITE     PPE01
     C                   ENDIF                                                  XXFRST IFEQ
      *
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDSR                                                  ENDRPT
      *================================================================***
