       //============================================================
       Ctl-Opt Copyright('(c) 2025 Sapiens. All Rights Reserved.')
        Option(*NoDebugIO:*SrcStmt)
        DftActGrp(*No)
        ActGrp(*Caller)
        BndDir('WTBNDDIR');
     h/Title SR00311A - Voluntary Comp & Emp Liability Coverage Endorsement
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
       //
       // 08/21/25 C:333 Support: 148/117703  (Recompile)
       //                For single print endorsements provide specific
       //                group and employer so that correct sig is found
       //
       // 11/30/23 C:079 Support: 133/114651
       //                Load Form ID key for read of WXFMP1
       //
       // 09/12/23 C:333 Support: 113867
       //                Voluntary Comp form showing waiver endorsement
       //                screen
       //
       //=============================================================
       // NOTES:
       //  Utilizes the endorsement copybook (SCOPYEN01F/D/C).
       //  qqmode = 1 - Called to determine if form should be attached
       //  qqmode = 2 - Called to print endorsement.
       //  qqmode = 3 - Called for additional input.
       //  qqmode = 4 - Called for midterm processing.
       //=============================================================
     fwxfmp1    IF   E           K DISK    USROPN
     f                                     RENAME(PFM01:PFM02)
     fwxpel     UF   E           K DISK
     fwxpel1    IF   E           K DISK
     f                                     RENAME(PPE01:PPE02)

       // Copybook F specs
     f/Copy sCopyEn01f
       //=============================================================
       // Definition Specs
       //=============================================================
       Dcl-Pr SR00311A;
        *N Char(3);
        *N Char(3);
        *N Char(9);
        *N Char(5);
        *N Char(3);
        *N Char(1);
        *N Packed(9:0);
        *N Char(1);
       End-Pr;
       Dcl-Pi SR00311A;
        qqCo# Char(3);
        qqFnd Char(3);
        qqEmp# Char(9);
        qqDiv Char(5);
        qqFyr Char(3);
        qqMode Char(1);
        qqRrn Packed(9:0);
        qqMod2 Char(1);
       End-Pi;

     d strKey          s             40
     d                 DS
     d  QCKEY                  1     40
     d  QCCO#                  1      3  0
     d  QCFND                  4      6  0
     d  QCEMP#                 7     15  0
     d  QCDIV                 16     20  0

     d  XXUNID                             like(PEUNID)
     d  X1END#                             like(PEEND#)
       // Copybook D specs
     d/Copy sCopyEn01d
     d/COPY WWCPOLDAT4
       //=============================================================
       // Copybook required constants
       //=============================================================
       // Form ID
       Dcl-C qqForm Const('WC000311A ');
       // Additional entry program
       //***Beg Chg ***** 09/12/23 ***********************************
       //**Dcl-C addPgm Const('W0815R');
       Dcl-C addPgm Const('SR00311I');
       //***End Chg ***** 09/12/23 ***********************************
       //=============================================================
       // Main
       //=============================================================
     c     FMKY3         KLIST
     c                   KFLD                    XXCO#
     c                   KFLD                    XXFND
     c                   KFLD                    XXFYR
     c                   KFLD                    XXEMP#
     c                   KFLD                    XXDIV
     c                   KFLD                    XXFRM1           15
     c     PEKEY         KLIST
     c                   KFLD                    XXCO#
     c                   KFLD                    XXFND
     c                   KFLD                    XXEMP#
     c                   KFLD                    XXFYR
     c                   KFLD                    XXDT              7 0
     c     PEKEY1        KLIST
     c                   KFLD                    XXCO#
     c                   KFLD                    XXFND
     c                   KFLD                    XXEMP#
     c                   KFLD                    XXFYR
     c     PEKEY2        KLIST
     c                   KFLD                    XXCO#
     c                   KFLD                    XXFND
     c                   KFLD                    XXEMP#
     c                   KFLD                    XXFYR
     c                   KFLD                    XXUNID
     c                   KFLD                    X1END#
       // Copybook C specs (PLISTs, KLISTs and mainline)
     c/Copy sCopyEn01C
       //=============================================================
       // OthInf - Subroutine to load form specific information
       //=============================================================
       Begsr OthInf;
        fminnm = emname + emnam2;
        fmifnm = emfdnm + emfdn2;

     C                   Z-ADD     *ZERO         NX                2 0
     C                   Z-ADD     *ZERO         AX                2 0
      // Print Additional form info during Audit request call
         If xxDiv <> 0;
            svDiv = xxDiv;
            xxDiv = 0;
         Endif;
      *  WRITE THE OUTPUT RECORD
     C                   OPEN      WXFMP1
       //*** Beg Add *** 11/30/23 ***************************************
       xxFrm1 = qqForm;
       //*** End Add *** 11/30/23 ***************************************
     C     FMKY3         CHAIN     PFM02                              60
         If xxDiv = 0 and svDiv <> 0;
            xxDiv = svDiv;
         Endif;
      * MOVE THE CUSTOM FIELDS TO PRINT
     C                   MOVEL     F3C301        FMC301
     C                   MOVEL     F3C302        FMC302
     C                   MOVEL     F3C303        FMC303
     C                   MOVEL     F3C304        FMC304
     C                   MOVEL     F3C305        FMC305
     C                   MOVEL     F3C306        FMC306
     C                   MOVEL     F3C307        FMC307
     C                   MOVEL     F3C308        FMC308
     C                   MOVEL     F3C309        FMC309
     C                   MOVEL     F3C310        FMC310
     C                   MOVEL     F3C311        FMC311
     C                   MOVEL     F3C312        FMC312
     C                   MOVEL     F3C313        FMC313
     C                   MOVEL     F3C314        FMC314
     C                   MOVEL     F3C315        FMC315
     C                   MOVEL     F3C316        FMC316
      *
     C                   MOVEL     F3C201        FMC201
     C                   MOVEL     F3C202        FMC202
     C                   MOVEL     F3C203        FMC203
     C                   MOVEL     F3C204        FMC204
     C                   MOVEL     F3C205        FMC205
     C                   MOVEL     F3C206        FMC206
     C                   MOVEL     F3C207        FMC207
     C                   MOVEL     F3C208        FMC208
     C                   MOVEL     F3C209        FMC209
     C                   MOVEL     F3C210        FMC210
     C                   MOVEL     F3C211        FMC211
     C                   MOVEL     F3C212        FMC212
     C                   MOVEL     F3C213        FMC213
     C                   MOVEL     F3C214        FMC214
     C                   MOVEL     F3C215        FMC215
     C                   MOVEL     F3C216        FMC216
      *
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
     C                   MOVEL     F3C411        FMC411
     C                   MOVEL     F3C412        FMC412
     C                   MOVEL     F3C413        FMC413
     C                   MOVEL     F3C414        FMC414
     C                   MOVEL     F3C415        FMC415
     C                   MOVEL     F3C416        FMC416
      *  If printing a policy page.  (Not called from W1054R)
     C                   CALL      'SRCHKPGM'
     C                   PARM      'W1054R'      #1PGM            10
     C                   PARM      *BLANK        #1FLAG            1
      *  If printing a Audit Request (called from W7231R)
     C                   CALL      'SRCHKPGM'
     C                   PARM      'W7231R'      #1PGM            10
     C                   PARM      *BLANK        #2FLAG            1
          If #1Flag = '0' or #2Flag = '1';
             PrintingPolicy = *On;
          Else;
             PrintingPolicy = *Off;
          Endif;
          Exsr WriteWorkFile;
          WriteInCopybook = *Off;
     C                   CLOSE     WXFMP1
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
     C                   CLEAR                   E00311
     C                   MOVEL     F3C201        XXIE
     C                   CAT       F3C202:0      XXIE
     C                   CAT       F3C203:0      XXIE
     C                   CAT       F3C204:0      XXIE
     C                   CAT       F3C205:0      XXIE
     C                   CAT       F3C206:0      XXIE
     C                   CAT       F3C207:0      XXIE
     C                   CAT       F3C208:0      XXIE
     C                   CAT       F3C209:0      XXIE
     C                   CAT       F3C210:0      XXIE
     C                   MOVEL     F3C301        XXIST
     C                   CAT       F3C302:0      XXIST
     C                   CAT       F3C303:0      XXIST
     C                   CAT       F3C304:0      XXIST
     C                   MOVEL     F3C401        XXDWC
     C                   CAT       F3C402:0      XXDWC
     C                   CAT       F3C403:0      XXDWC
     C                   CAT       F3C404:0      XXDWC
     C                   MOVEL(P)  E00311        PEDATA
     C                   UPDATE    PPE01
     C                   ENDIF
     C                   ENDIF

       Endsr;
       //============================================================
       //  Pgmstr - Beginning of program processing
       //============================================================
       Begsr PgmStr;
       //*** Beg Chg ***** 09/12/23 ******************************
          //***UseDfltAddparms = *Off;
          UseDfltAddparms = *on;
       //*** End Chg ***** 09/12/23 ******************************
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
        If elPfdt <> elPtdt;
           crtFrm = *On;
        EndIf;
        crtFrm = *On;
       Endsr;
