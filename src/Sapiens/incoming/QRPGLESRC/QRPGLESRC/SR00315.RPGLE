       //============================================================
       Ctl-Opt Copyright('(c) 2023 Sapiens. All Rights Reserved.')
        Option(*NoDebugIO:*SrcStmt)
        DftActGrp(*No)
        ActGrp(*Caller)
        BndDir('WTBNDDIR');
     h/Title SR00315 - Domestic and Agicultural Worker Exclusion Endorsement
       //============================================================
       // Program Description:
       //
       //   Determines if this endorsement form should be attached to
       //   the policy and creates data in the Endorsement Form
       //   Workfile (WXWXFMP) prior to a form merge.
       //
       // PROGRAMMER: C:333
       // CREATED:    10/26/22            PROJECT: WC2834
       //=============================================================
       // Revisions:
       //
       // 09/12/23 C:333 Support: 113867
       //                Voluntary Comp form showing waiver endorsement
       //                screen
       //                - Similar issue for Domestic & Agricultural
       //
       //=============================================================
       // NOTES:
       //  Utilizes the endorsement copybook (SCOPYEN01F/D/C).
       //  qqmode = 1 - Called to determine if form should be attached
       //  qqmode = 2 - Called to print endorsement.
       //  qqmode = 3 - Called for additional input.
       //  qqmode = 4 - Called for midterm processing.
       //=============================================================
     fWXFMP1    IF   E           K DISK    USROPN
     f                                     RENAME(PFM01:PFM02)
     fWXPEL     UF A E           K DISK
     fWXPEL1    IF   E           K DISK
     f                                     RENAME(PPE01:PPE02)
       // Copybook F specs
     f/Copy sCopyEn01f
       //=============================================================
       // Definition Specs
       //=============================================================
       Dcl-Pr SR00315;
        *N Char(3);
        *N Char(3);
        *N Char(9);
        *N Char(5);
        *N Char(3);
        *N Char(1);
        *N Packed(9:0);
        *N Char(1);
       End-Pr;
       Dcl-Pi SR00315;
        qqCo# Char(3);
        qqFnd Char(3);
        qqEmp# Char(9);
        qqDiv Char(5);
        qqFyr Char(3);
        qqMode Char(1);
        qqRrn Packed(9:0);
        qqMod2 Char(1);
       End-Pi;
     D XXUNID          s                   like(PEUNID)
     D X1END#          s                   like(PEEND#)
     D XXFRM1          s             15A
     D/COPY WWCPOLDAT4
       // Copybook D specs
     d/Copy sCopyEn01d
       //=============================================================
       // Copybook required constants
       //=============================================================
       // Form ID
       Dcl-C qqForm Const('WC000315  ');
       // Additional entry program
       //***Beg Chg ***** 09/12/23 ***********************************
       //**Dcl-C addPgm Const('W0815R');
       Dcl-C addPgm Const('SR00315I');
       //***End Chg ***** 09/12/23 ***********************************
       //=============================================================
       // Main
       //=============================================================
     C     FMKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFRM1
     C     PEKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXDT              7 0
     C     PEKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
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
     C                   Z-ADD     *ZERO         NX                2 0
     C                   Z-ADD     *ZERO         AX                2 0
      *  WRITE THE OUTPUT RECORD
     C                   OPEN      WXFMP1
     C     FMKY3         CHAIN     PFM02                              60
      * MOVE THE CUSTOM FIELDS TO PRINT
     C                   MOVEL     F3C401        FMC401
     C                   MOVEL     F3C402        FMC402
     C                   MOVEL     F3C403        FMC403
     C                   MOVEL     F3C404        FMC404
     C                   MOVEL     F3C405        FMC405
     C                   MOVEL     F3C406        FMC406
     C                   MOVEL     F3C407        FMC407
     C                   MOVEL     F3C408        FMC408
     C                   MOVEL     F3C409        FMC409
     C                   MOVEL     F3C410        FMC410
      *  If printing a policy page.  (Not called from W1054R)
     C                   CALL      'SRCHKPGM'
     C                   PARM      'W1054R'      #1PGM            10
     C                   PARM      *BLANK        #1FLAG            1
      /Free
          If #1Flag = '0';
             PrintingPolicy = *On;
          Else;
             PrintingPolicy = *Off;
          Endif;
      /End-Free
     C     #1FLAG        IFEQ      '0'
      * Update the endorsement reporting file with the proper data.
      * Find the unique ID number.
     C                   MOVE      'P'           X1TYPE            1
     C                   Z-ADD     0             XXUNID
     C                   MOVE      *HIVAL        XXDT
     C     PEKEY         SETGT     PPE02
     C     PEKEY1        READPE    PPE02                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     PEUNID        XXUNID
     C                   ENDIF
     C                   MOVEL     XXFORM        X1END#
     C     PEKEY2        CHAIN     PPE01                              60
      * Copy data into the record.
     C     *IN60         IFEQ      *OFF
     C                   CLEAR                   E00315
     C     FMC401        IFNE      *BLANKS
     C     FMC406        ORNE      *BLANKS
     C                   MOVEL(P)  FMC401        XXFAW
     C                   MOVEL(P)  FMC406        XXDHW
     C                   MOVEL(P)  E00315        PEDATA
     C                   UPDATE    PPE01
     C                   ENDIF
     C     FMC402        IFNE      *BLANKS
     C     FMC407        ORNE      *BLANKS
     C                   MOVEL(P)  FMC402        XXFAW
     C                   MOVEL(P)  FMC407        XXDHW
     C                   MOVEL(P)  E00315        PEDATA
     C                   WRITE     PPE01
     C                   ENDIF
     C     FMC403        IFNE      *BLANKS
     C     FMC408        ORNE      *BLANKS
     C                   MOVEL(P)  FMC403        XXFAW
     C                   MOVEL(P)  FMC408        XXDHW
     C                   MOVEL(P)  E00315        PEDATA
     C                   WRITE     PPE01
     C                   ENDIF
     C     FMC404        IFNE      *BLANKS
     C     FMC409        ORNE      *BLANKS
     C                   MOVEL(P)  FMC404        XXFAW
     C                   MOVEL(P)  FMC409        XXDHW
     C                   MOVEL(P)  E00315        PEDATA
     C                   WRITE     PPE01
     C                   ENDIF
     C     FMC405        IFNE      *BLANKS
     C     FMC410        ORNE      *BLANKS
     C                   MOVEL(P)  FMC405        XXFAW
     C                   MOVEL(P)  FMC410        XXDHW
     C                   MOVEL(P)  E00315        PEDATA
     C                   WRITE     PPE01
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   CLOSE     WXFMP1
       Endsr;
       //============================================================
       //  Pgnstr - Beginning of program processing
       //============================================================
       Begsr PgmStr;
       //*** Beg Chg ***** 09/12/23 *********************************
          //***UseDfltAddparms = *Off;
          UseDfltAddparms = *On;
       //*** End Chg ***** 09/12/23 *********************************
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
        crtFrm = *On;
       Endsr;
