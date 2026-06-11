       //============================================================
       Ctl-Opt Copyright('(c) 2023 Sapiens. All Rights Reserved.')
        Option(*NoDebugIO:*SrcStmt)
        DftActGrp(*No)
        ActGrp(*Caller)
        BndDir('WTBNDDIR');
     h/Title SR00404 - Pending Rate Change Endorsement
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
       // 01/27/23 C:291 Project: WC2863
       //                Add Approved/Excluded State Flag.
       //
       //=============================================================
       // NOTES:
       //  Utilizes the endorsement copybook (SCOPYEN01F/D/C).
       //  qqmode = 1 - Called to determine if form should be attached
       //  qqmode = 2 - Called to print endorsement.
       //  qqmode = 3 - Called for additional input.
       //  qqmode = 4 - Called for midterm processing.
       //=============================================================
     fWMNXL     IF   E           K DISK
     fWXPEL     UF A E           K DISK
     fWMSHL1    IF   E           K DISK
     fSTSTL3    IF   E           K DISK
       // Copybook F specs
     f/Copy sCopyEn01f
       //=============================================================
       // Definition Specs
       //=============================================================
       Dcl-Pr SR00404;
        *N Char(3);
        *N Char(3);
        *N Char(9);
        *N Char(5);
        *N Char(3);
        *N Char(1);
        *N Packed(9:0);
        *N Char(1);
       End-Pr;
       Dcl-Pi SR00404;
        qqCo# Char(3);
        qqFnd Char(3);
        qqEmp# Char(9);
        qqDiv Char(5);
        qqFyr Char(3);
        qqMode Char(1);
        qqRrn Packed(9:0);
        qqMod2 Char(1);
       End-Pi;
     D XST             S              2    DIM(50) ASCEND
     D YST             S              2    DIM(50) ASCEND

     D XXCLT#          s              3a
     D XXUNID          s                   like(PEUNID)
     D X1END#          s                   like(PEEND#)

     D/COPY WWCPOLDAT4

       // Copybook D specs
     d/Copy sCopyEn01d
       //=============================================================
       // Copybook required constants
       //=============================================================
       // Form ID
       Dcl-C qqForm Const('WC000404  ');
       // Additional entry program
       Dcl-C addPgm Const('SRxxxxxxI');
       //=============================================================
       // Main
      *  ENDORSEMENT FORM STATISTICAL INFO FILE KLIST
     C     NXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    ELPFDT

     C     NXKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C     PEKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXUNID
     C                   KFLD                    X1END#
     C     STKY          KLIST
     C                   KFLD                    XXKEY            10
      * SUBMISSION HISTORY FILE
     C     SHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    X1TYPE

       //=============================================================
       // Copybook C specs (PLISTs, KLISTs and mainline)
     c/Copy sCopyEn01C
       //=============================================================
       // OthInf - Subroutine to load form specific information
       //=============================================================
       Begsr OthInf;
      *  WRITE THE OUTPUT RECORD
     C                   Z-ADD     1             I2                3 0

     C     ELKY2         CHAIN     PEL02                              51

     C     *IN51         DOWEQ     *OFF

         xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
         If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);

     C     RSKY          CHAIN     PRS01                              60

     C     *IN60         IFEQ      *OFF
     C     RSJRST        ANDNE     *BLANKS
     C     RSJRST        ANDNE     'MO'
      //*** Beg Add ***** 01/27/23 *****************************
       Exsr ChkApprExclSt;
       If isApprExclSt = *On;
      //*** End Add ***** 01/27/23 *****************************

     C     XXCLT#        IFEQ      '139'
     C                   Z-ADD     1             I                 3 0
     C     RSJRST        LOOKUP    YST(I)                                 60

     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        YST(I2)
     C                   ADD       1             I2
     C                   ENDIF

     C                   ELSE

     C                   Z-ADD     1             I                 3 0
     C                   MOVE      RSJRST        XXST
     C     NXKY          SETGT     PNX01
     C     NXKY1         READPE    PNX01                                  60

     C     *IN60         DOWEQ     *OFF
     C     NXSTAT        ANDEQ     'I'
     C     NXKY1         READPE    PNX01                                  60
     C                   ENDDO

     C     *IN60         IFEQ      *OFF                                         REC FOUND
     C     NXPRCF        ANDEQ     'Y'                                          PEND RT CHG FLAG
     C     NXPRCD        ANDNE     *ZERO                                        PEND RT CHG DT
     C     NXPRCD        ANDLE     ELPFDT                                       PEND RT CHG DT

     C     NXST          LOOKUP    YST(I)                                 60

     C     *IN60         IFEQ      *OFF
     C                   MOVE      NXST          YST(I2)
     C                   ADD       1             I2
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  *IN60 IFEQ

     C                   ENDIF                                                  CLT139

       //*** Beg Add ***** 01/27/23 *****************************
       EndIf;
       //*** End Add ***** 01/27/23 *****************************
     C                   ENDIF                                                  RSKY

         EndIf;
     C     ELKY2         READE     PEL02                                  51


     C                   ENDDO

     C                   SORTA     YST
     C                   Z-ADD     1             I2
     C                   Z-ADD     1             I
     C     I             DOWLE     50
     C     YST(I)        IFNE      *BLANKS
     C                   MOVE      YST(I)        XST(I2)
     C                   ADD       1             I2
     C                   ENDIF
     C                   ADD       1             I
     C                   ENDDO                                                  I DOWLE

     C     XST(1)        CAT       XST(2):30     FMC401
     C     XST(3)        CAT       XST(4):30     FMC402
     C     XST(5)        CAT       XST(6):30     FMC403
     C     XST(7)        CAT       XST(8):30     FMC404
     C     XST(9)        CAT       XST(10):30    FMC405
     C     XST(11)       CAT       XST(12):30    FMC406
     C     XST(13)       CAT       XST(14):30    FMC407
     C     XST(15)       CAT       XST(16):30    FMC408
     C     XST(17)       CAT       XST(18):30    FMC409
     C     XST(19)       CAT       XST(20):30    FMC410
     C     XST(21)       CAT       XST(22):30    FMC411
     C     XST(23)       CAT       XST(24):30    FMC412
     C     XST(25)       CAT       XST(26):30    FMC413
     C     XST(27)       CAT       XST(28):30    FMC414
     C     XST(29)       CAT       XST(30):30    FMC415
     C     XST(31)       CAT       XST(32):30    FMC416
     C     XST(33)       CAT       XST(34):30    FMC417
     C     XST(35)       CAT       XST(36):30    FMC418
     C     XST(37)       CAT       XST(38):30    FMC419
     C     XST(39)       CAT       XST(40):30    FMC420
     C     XST(41)       CAT       XST(42):30    FMC421
     C     XST(43)       CAT       XST(44):30    FMC422
     C     XST(45)       CAT       XST(46):30    FMC423
     C     XST(47)       CAT       XST(48):30    FMC424
     C     XST(49)       CAT       XST(50):30    FMC425
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
          Exsr WriteWorkFile;
          WriteInCopybook = *Off;

      /End-Free

     C     #1FLAG        IFEQ      '0'
      * Update the endorsement reporting file with the proper data.
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

     C                   Z-ADD     1             I
     C     XST(I)        IFNE      *BLANKS

     C                   MOVEL     XST(I)        XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      STNST         PEST
     C                   ENDIF

     C                   UPDATE    PPE01
     C                   ENDIF
     C                   ADD       1             I

     C     I             DOWLE     50
     C     XST(I)        IFNE      *BLANKS

     C                   MOVEL     XST(I)        XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      STNST         PEST
     C                   ENDIF

     C                   WRITE     PPE01
     C                   ENDIF
     C                   ADD       1             I
     C                   ENDDO

     C                   ENDIF
     C                   ENDIF

       Endsr;
       //============================================================
       //  Pgmstr - Beginning of program processing
       //============================================================
       Begsr PgmStr;
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

       // Always attach for clt139
        Select;
        When xxClt# = '139';
           crtFrm = *On;

        Other;

       // GET THE CURRENT ENDORSEMENT FORM STAT INFO RECORD AS OF THE ELPFDT
        Setll (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
        ReadE (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
           Dow not %eof(wdell1);
              Chain (elCo#:elRtcd) prs01;
              If %found(wmrsl) and rsJrst <> *Blanks and
              rsJrst <> 'TN';
                 xxST = rsJrst;
                 SetGt (xxCo#:xxFnd:xxST:elPfdt) pnx01;
                 ReadPE (xxCo#:xxFnd:xxST) pnx01;
                 Dow not %eof(wmnxl) and nxStat = 'I';
                    ReadPE (xxCo#:xxFnd:xxST) pnx01;
                 EndDo;
       // A VALID RECORD WAS FOUND, THERE IS A PENDING RATE CHG, THE
       // PENDING RATE CHG DATE IS NOT ZERO, AND IS LESS THAN/EQUAL TO
       // THE POLICY FROM DATE.
                 If not %eof(wmnxl) and nxPrcf = 'Y' and nxPrcd <> *Zero
                 and nxPrcd <= elPfdt and elPfdt <> elPtdt;
       // A RATE CHG DATE HAS NOT BEEN ENTERED, OR IS GREATER THAN
       // THE POLICY FROM DATE, WRITE THE ENDORSEMENT FORM RECORD.
                    crtFrm = *On;
                 EndIf;
              EndIf;
              ReadE (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
           EndDo;
        EndSl;
       // Do not auto attach if Missouri Coverage on state reporting
       // divison for policies effective 01/01/08 or later.

        Chain (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr) pel01;
        If %found(wdell) and elPfdt >= 1080101;
           Chain (elCo#:elRtcd) prs01;
           If %found(wmrsl) and rsJrst = 'MO';
              crtFrm = *Off;
           EndIf;
        EndIf;

       Endsr;
