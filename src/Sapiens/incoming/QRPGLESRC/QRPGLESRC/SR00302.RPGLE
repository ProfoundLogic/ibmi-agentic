       //============================================================
       Ctl-Opt Copyright('(c) 2025 Sapiens. All Rights Reserved.')
        Option(*NoDebugIO:*SrcStmt)
        DftActGrp(*No)
        ActGrp(*Caller)
        BndDir('WTBNDDIR');
     h/Title SR00302  - LOAD ENDORSEMENT FORM WORKFILE FOR FORM WC000302
       //============================================================
       // Program Description:
       //
       //   Determines if this endorsement form should be attached to
       //   the policy and creates data in the Endorsement Form
       //   Workfile (WXWXFMP) prior to a form merge.
       //
       // PROGRAMMER: C:333
       // CREATED:    10/18/22            PROJECT: WC2834
       //=============================================================
       // Revisions:
       //
       // 08/21/25 C:333 Support: 148/117703  (Recompile)
       //                For single print endorsements provide specific
       //                group and employer so that correct sig is found
       //
       // 09/12/23 C:333 Support: 113867
       //                Voluntary Comp form showing waiver endorsement
       //                screen
       //                - same issue with Designated WorkPlace form
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
     fWMEWL     IF   E           K DISK
     fWXPEL     UF A E           K DISK
     fWXPEL1    IF   E           K DISK
     f                                     RENAME(PPE01:PPE02)
     f/Copy sCopyEn01f
       //=============================================================
       // Definition Specs
       //=============================================================
       Dcl-Pr SR00302;
        *N Char(3);
        *N Char(3);
        *N Char(9);
        *N Char(5);
        *N Char(3);
        *N Char(1);
        *N Packed(9:0);
        *N Char(1);
       End-Pr;
       Dcl-Pi SR00302;
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
     d/COPY WWCPOLDAT4
     d                 DS
     d  EW                     1    200
     d                                     DIM(5)
     d  EWWRK1                 1     40
     d  EWWRK2                41     80
     d  EWWRK3                81    120
     d  EWWRK4               121    160
     d  EWWRK5               161    200

     d  XXUNID                             Like(PEUNID)
     d  X1END#                             Like(PEEND#)
     d/Copy sCopyEn01d
       //=============================================================
       // Copybook required constants
       //=============================================================
       // Form ID
       Dcl-C qqForm Const('WC000302  ');
       // Additional entry program
       //*** Beg Chg ***** 09/12/23 **********************************
       //**Dcl-C addPgm Const('W0815R');
       Dcl-C addPgm Const('W0817R');
       //*** End Chg ***** 09/12/23 **********************************
       //=============================================================
       // Main
       //=============================================================
     C     EWKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
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
     C                   MOVE      *BLANK        XXFRST            1
      *  WRITE THE OUTPUT RECORD
      * MOVE THE CUSTOM FIELDS TO PRINT
      * Look for excluded workplaces.
     C     EWKY          SETLL     PEW01
     C     EWKY          READE     PEW01                                  60
     C     *IN60         DOWEQ     *OFF
     C     MUKY1         CHAIN     PMU02                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     MUEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         FMEEDT
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         FMEEDT
     C                   ENDIF
     C     EWSTAT        IFNE      'I'
     C                   MOVEL     EWWRK1        FMC401
     C                   MOVEL     EWWRK2        FMC402
     C                   MOVEL     EWWRK3        FMC403
     C                   MOVEL     EWWRK4        FMC404
     C                   MOVEL     EWWRK5        FMC405
      *  If printing a policy page.  (Not called from W1054R)
     C                   CALL      'SRCHKPGM'
     C                   PARM      'W1054R'      #1PGM            10
     C                   PARM      *BLANK        #1FLAG            1
      * Write the record.

          If #1Flag = '0';
             PrintingPolicy = *On;
          Else;
             PrintingPolicy = *Off;
          Endif;

          Exsr WriteWorkFile;
          WriteInCopybook = *Off;

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
     C                   Z-ADD     1             I
     C                   CLEAR                   E00302
     C     XXFRST        IFEQ      *BLANK
     C     EW(1)         IFNE      *BLANKS
     C                   MOVEL     EW(1)         XXLNC
     C                   MOVEL(P)  E00302        PEDATA
     C                   UPDATE    PPE01
     C                   ENDIF
     C                   MOVE      'Y'           XXFRST
     C                   ADD       1             I
     C                   ENDIF
     C     XXFRST        IFNE      *BLANK
     C     I             DOWLT     6
     C     EW(I)         IFNE      *BLANKS
     C                   MOVEL     EW(I)         XXLNC
     C                   MOVEL(P)  E00302        PEDATA
     C                   WRITE     PPE01
     C                   ENDIF
     C                   ADD       1             I
     C                   ENDDO
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      *BLANKS       FMC401
     C                   MOVE      *BLANKS       FMC402
     C                   MOVE      *BLANKS       FMC403
     C                   MOVE      *BLANKS       FMC404
     C                   MOVE      *BLANKS       FMC405
     C                   ENDIF
     C                   ENDIF
     C     EWKY          READE     PEW01                                  60
     C                   ENDDO

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
        crtFrm = *On;
       Endsr;
