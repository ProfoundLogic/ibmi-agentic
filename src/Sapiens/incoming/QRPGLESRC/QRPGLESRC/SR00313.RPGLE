       //============================================================
       Ctl-Opt Copyright('(c) 2025 Sapiens. All Rights Reserved.')
        Option(*NoDebugIO:*SrcStmt)
        DftActGrp(*No)
        ActGrp(*Caller)
        BndDir('WTBNDDIR');
     h/Title SR00313 - Waiver of Subrogation Endorsement
       //============================================================
       // Program Description:
       //
       //   Determines if this endorsement form should be attached to
       //   the policy and creates data in the Endorsement Form
       //   Workfile (WXWXFMP) prior to a form merge.
       //
       // PROGRAMMER: C:333
       // CREATED:    10/20/22            PROJECT: WC2834
       //=============================================================
       // Revisions:
       //                                                             *
       // 08/21/25 C:333 Support: 148/117703  (Recompile)             *
       //                For single print endorsements provide specific
       //                group and employer so that correct sig is found
       //                                                             *
       // 08/24/23 C:079 Support: 110561-099                          *
       //                Add Endorsment # 0 if it does not exist      *
       //                                                             *
       // 05/24/23 C:333 Project: 148WEC                              *
       //                Auto Attach Waiver Form                      *
       //
       //=============================================================
       // NOTES:
       //  Utilizes the endorsement copybook (SCOPYEN01F/D/C).
       //  qqmode = 1 - Called to determine if form should be attached
       //  qqmode = 2 - Called to print endorsement.
       //  qqmode = 3 - Called for additional input.
       //  qqmode = 4 - Called for midterm processing.
       //=============================================================
       // Copybook F specs
     f* Waiver
     fWMWVL6    IF   E           K DISK
     f                                     RENAME(PWV01:PWV06)
     fWMWVL7    IF   E           K DISK
     f                                     RENAME(PWV01:PWV07)
     fWXFMP3    O    E             DISK
     fWXPEL     UF A E           K DISK
     fWMSHL1    IF   E           K DISK
     fwmmula    if   e           k disk    rename(pmu01:pmu0a)
     fWDF4L     if   e           k disk

     f/Copy sCopyEn01f
       //=============================================================
       // Definition Specs
       //=============================================================
       Dcl-Pr SR00313;
        *N Char(3);
        *N Char(3);
        *N Char(9);
        *N Char(5);
        *N Char(3);
        *N Char(1);
        *N Packed(9:0);
        *N Char(1);
       End-Pr;
       Dcl-Pi SR00313;
        qqCo# Char(3);
        qqFnd Char(3);
        qqEmp# Char(9);
        qqDiv Char(5);
        qqFyr Char(3);
        qqMode Char(1);
        qqRrn Packed(9:0);
        qqMod2 Char(1);
       End-Pi;

     d isStApproved    pr              n
     D xxproc          s              1
     d qq313           s               n
     d xkForm          s                   like(enForm)
     d xxunid          s                   like(peUnid)
     d x1End#          s                   like(peEnd#)

     d                 DS
     d  XXNAME                 1    100
     d  FMEMNM                 1     40
     d  FMC215                41     60
     d  FMC441                61    100

     D                 DS
     D  XXFNAM                 1    100
     D  FMFENM                 1     40
     D  FMC216                41     60
     D  FMC442                61    100

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
     d/COPY WWCPOLDAT4
       // Standalone D specs
     d  sysDate        s               d
       //=============================================================
       // Copybook required constants
       //=============================================================
       // Form ID
       Dcl-C qqForm Const('WC000313       ');
       // Additional entry program
       Dcl-C addPgm Const('W0815R');
       //=============================================================
       // Main
       //=============================================================
     C     SHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    X1TYPE
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
       // Copybook C specs (PLISTs, KLISTs and mainline)
     c/Copy sCopyEn01C
       //=============================================================
       // OthInf - Subroutine to load form specific information
       //=============================================================
       Begsr OthInf;

      * Get PEO Information if PEO Group
     C     FNPEOF        IFNE      *BLANK
     C     LDENDA        ANDNE     *BLANKS
     C     LDEND#        ANDGT     0
     C                   MOVE      QQCO#         #5CO#
     C                   MOVE      QQFND         #5FND
     C                   MOVE      QQEMP#        #5EMP#
     C                   MOVE      QQDIV         #5DIV
     C                   MOVE      QQFYR         #5FYR

     C                   CALL      'SRADD1'
     C                   PARM                    #5CO#             3 0
     C                   PARM                    #5FND             3 0
     C                   PARM                    #5EMP#            9 0
     C                   PARM                    #5DIV             5 0
     C                   PARM                    #5FYR             3 0
     C     XXNAME        PARM                    #5NAME          100
     C     XXFNAM        PARM                    #5FNAM          100
     C                   PARM                    #5EAD1           40
     C                   PARM                    #5EAD2           40
     C                   PARM                    #5ECSZ           40
     C                   PARM                    #5CTY            25
     C                   PARM                    #5ST              2
     C                   PARM                    #5ZIP             9 0
     C                   PARM                    #5FEIN            9 0
     C                   PARM                    #5EADR           90

     C                   ENDIF
     C*
     C* Load waver type
     C                   Eval      FMfl01 = ELwvTp

       If ldEnda <> *Blanks and ldenda <> *Zeros;
          Exsr Info1;
       Else;
          Exsr Info2;
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

        If q1Prms >= 9;
           qq313 = *Off;
        EndIf;

      * Look for any waivers.
      *  mid-term process
        Chain(N) (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:ldEnd#) pwv07;
        Setll (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:ldEnd#) pwv07;
        ReadE(N) (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:ldEnd#) pwv07;
        Dow not %eof(WMWVL7);
           If wvStat <> 'I';
              exsr srproc;

              If xxProc = 'Y';
                 If q1Prms >= 9;
                    qq313 = *on;
                 EndIf;
                 crtFrm = *on;
                 leave;
              EndIf;
           EndIf;
        ReadE(N) (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:ldEnd#) pwv07;
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
      * SRPROC - Determine if Waiver record to be processed
      *================================================================***
     C     SRPROC        BEGSR

     c                   select
      * clt132
     c                   when      (cltid = '132')
     C                   CALL      'WA725R'
     C                   PARM      wvemst        ##ST              2
     C                   PARM      *blank        ##RTN             1

     c                   if        (##rtn = 'Y')
     c                             or (wvemst = 'UT')
     c                             or (wvemst = 'CA')
     c                   eval      xxproc = 'N'
     c                   else
     c                   eval      xxproc = 'Y'
     c                   endif
      * not clt132
     c                   other

       If isStApproved();
          xxProc = 'Y';
       Else;
          xxProc = 'N';
       Endif;

     c                   endsl

     C                   endsr
      *================================================================***
      *================================================================***
      * INFO1 - Get OTHINF MID-TERM Endorsement records
      *================================================================***
     C     INFO1         BEGSR
     C                   MOVE      'N'           XXWRT             1
     C                   MOVE      *BLANK        XXFRST            1

      * Look for waivers.
     C     WVKY7         CHAIN     PWV07                              60

     C     *IN60         DOWEQ     *OFF

     C     WVSTAT        IFNE      'I'
     c                   exsr      srproc
     c                   if        xxproc = 'Y'
     C                   MOVEL     NAME          SNAME
     C                   MOVEL     SNAM1         FM1F01
     C                   MOVEL(p)  SNAM2         FM1F02
     C                   MOVEL(p)  WVNAME        FMNM01
     C                   Eval      FMc201 = %EditC(WVswPc:'X')
     c                   eval      fm2001 = fm1f01
     c                   eval      fm2002 = fm1f02
     C                   MOVEL     WVADR1        FMC401

     C     WVADR2        IFNE      *BLANK
     C                   MOVEL     WVADR2        FMC402
     C                   ENDIF

     c                   if        wvzip <> *zeros
     C                   MOVE      WVZIP         XXZIP
     C     WVCTY         CAT       ',':0         FMC403
     C                   CAT       WVST:1        FMC403
     C                   CAT       XXZIP1:1      FMC403

     C     XXZIP2        IFNE      *BLANK
     C     XXZIP2        ANDNE     *ZERO
     C                   CAT       '-':0         FMC403
     C                   CAT       XXZIP2:0      FMC403
     C                   ENDIF
     C                   endif

     C     FMC402        IFEQ      *BLANKS
     C                   MOVEL(P)  FMC403        FMC402
     C                   MOVEL(P)  *BLANKS       FMC403
     C                   ENDIF
      * Load the project lines.

       FMC601 = WVproj;
       FMC602 = WVprj2;
       FMblk01 = WVblkPrjDs;

      *  If printing a policy page.  (Not called from W1054R)
     C                   CALL      'SRCHKPGM'
     C                   PARM      'W1054R'      #1PGM            10
     C                   PARM      *BLANK        #1FLAG            1
      * Write the record.
      /Free
       If #1Flag = '0';
          PrintingPolicy = *On;
       Else;
          PrintingPolicy = *Off;
       Endif;
       Exsr WriteWorkFile;
       //*** Beg Add ***** 05/24/23 *******************************
          WriteInCopybook = *Off;
       //*** End Add ***** 05/24/23 *******************************
      /End-Free
     C                   MOVE      'Y'           XXWRT             1

     C                   MOVE      *BLANKS       FM1F01
     C                   MOVE      *blanks       FM1F02
     C                   MOVE      *BLANKS       FMNM01
     C                   Eval      FMc201 = ' '
     c                   move      *blanks       fm2001
     c                   move      *blanks       fm2002
     C                   MOVE      *BLANKS       FMC401
     C                   MOVE      *BLANKS       FMC402
     C                   MOVE      *BLANKS       FMC403
     C                   MOVE      *BLANKS       FMC601
     C                   MOVE      *BLANKS       FMC602
       FMblk01 = *Blanks;

     C                   ENDIF
     C                   ENDIF

     C     WVKY7         READE     PWV07                                  60
     C                   ENDDO

     C     XXWRT         IFEQ      'Y'
     C                   WRITE     PFM03
     C                   ENDIF

     C                   ENDSR
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

     C     WVSTAT        IFNE      'I'
     c                   exsr      srproc
     c                   if        xxproc = 'Y'
     C                   MOVEL     NAME          SNAME
     C                   MOVEL     SNAM1         FM1F01
     C                   MOVEL(p)  SNAM2         FM1F02
     C                   MOVEL(p)  WVNAME        FMNM01
     C                   Eval      FMc201 = %EditC(WVswPc:'X')
     c                   eval      fm2001 = fm1f01
     c                   eval      fm2002 = fm1f02
     C                   MOVEL     WVADR1        FMC401

     C     WVADR2        IFNE      *BLANK
     C                   MOVEL     WVADR2        FMC402
     C                   ENDIF

     c                   if        wvzip <> *zeros
     C                   MOVE      WVZIP         XXZIP
     C     WVCTY         CAT       ',':0         FMC403
     C                   CAT       WVST:1        FMC403
     C                   CAT       XXZIP1:1      FMC403

     C     XXZIP2        IFNE      *BLANK
     C     XXZIP2        ANDNE     *ZERO
     C                   CAT       '-':0         FMC403
     C                   CAT       XXZIP2:0      FMC403
     C                   ENDIF
     c                   endif

     C     FMC402        IFEQ      *BLANKS
     C                   MOVEL(P)  FMC403        FMC402
     C                   MOVEL(P)  *BLANKS       FMC403
     C                   ENDIF
      * Load the project lines.

       If WVblkPrjDs <> *Blanks;
          FMblk01 = WVblkPrjDs;
       Else;
          FMblk01 = WVproj + WVprj2;
       Endif;

      *  If printing a policy page.  (Not called from W1054R)
     C                   CALL      'SRCHKPGM'
     C                   PARM      'W1054R'      #1PGM            10
     C                   PARM      *BLANK        #1FLAG            1
      * Write the record.
      /Free
        If #1Flag = '0';
           PrintingPolicy = *On;
        Else;
           PrintingPolicy = *Off;
        Endif;
        Exsr WriteWorkFile;
       //*** Beg Add ***** 08/24/23 *******************************
          WriteInCopybook = *Off;
       //*** End Add ***** 08/24/23 *******************************
      /End-Free
     C                   MOVE      'Y'           XXWRT             1
      * Update the endorsement reporting file with the proper data.
     C                   EXSR      ENDRPT

     C                   MOVE      *BLANKS       FM1F01
     C                   MOVE      *BLANKS       FM1F02
     C                   MOVE      *BLANKS       FMNM01
     C                   Eval      FMc201 = ' '
     C                   MOVE      *BLANKS       FM2001
     C                   MOVE      *BLANKS       FM2002
     C                   MOVE      *BLANKS       FMC401
     C                   MOVE      *BLANKS       FMC402
     C                   MOVE      *BLANKS       FMC403
     C                   MOVE      *BLANKS       FMC601
     C                   MOVE      *BLANKS       FMC602
       FMblk01 = *Blanks;
     C                   ENDIF
     C                   ENDIF

     C                   ENDDO

     C     XXWRT         IFEQ      'Y'
     C                   WRITE     PFM03
     C                   ENDIF

     C                   ENDSR
      *================================================================***
      * ENDRPT - Update Endorsement reporting file
      *================================================================***
     C     ENDRPT        BEGSR
      *  If printing a policy page.  (Not called from W1054R)
     C                   CALL      'SRCHKPGM'
     C                   PARM      'W1054R'      #1PGM            10
     C                   PARM      *BLANK        #1FLAG            1
     C     #1FLAG        IFEQ      '0'
      * Find the unique ID number.
     C                   MOVE      'P'           X1TYPE            1
     C                   Z-ADD     0             XXUNID
     C     SHKEY         CHAIN     PSH01                              60

     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     SHUNID        XXUNID
     C                   ENDIF

     C                   MOVEL(P)  XXFORM        X1END#
     C     PEKEY2        CHAIN     PPE01                              60
      * Copy data into the record.
     C     *IN60         IFEQ      *OFF
     C                   CLEAR                   E00313
     C                   MOVEL     WVNAME        XXNOP
       If WVblkPrjDs <> *Blanks;
          xxnoo = WVblkPrjDs;
       Else;
          xxnoo = WVproj;
       Endif;
     C                   MOVEL(P)  E00313        PEDATA

     C     XXFRST        IFEQ      *BLANK
     C                   UPDATE    PPE01
     C                   MOVE      'Y'           XXFRST
     C                   ELSE
     C                   WRITE     PPE01
     C                   ENDIF

     C                   ENDIF
     C                   ENDIF

     C                   ENDSR
      *=======================================
     p isStApproved    b
      *=======================================
     d isStApproved    pi              n
     d i               s              2  0
     d stAry           s              2a   dim(55)
     d appExcFlg       s              1A
    dd xxSt            s              2A

       Select;
       When WVemst <> *blanks;
          xxSt = WVemst;
       Other;
          xxSt = GetRatingState(WVco#:WVfnd:WVemp#:WVdiv:WVfyr);
       Endsl;
       Chain (xxCo#:xkForm:xxSt) pes01;
       If %Found And
        (ESapex = 'A' And ESst <> xxSt Or ESapex = 'E' And ESst = xxSt);
          Return *Off;
       Else;
          Return *On;
       Endif;

     p isStApproved    e
