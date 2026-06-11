       //============================================================
       Ctl-Opt Copyright('(c) 2023 Sapiens. All Rights Reserved.')
        Option(*NoDebugIO:*SrcStmt)
        DftActGrp(*No)
        ActGrp(*Caller)
        BndDir('WTBNDDIR');
     h/Title SR00401A - Aircraft Premium Endorsement
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
       //                State Include/Exclude Endorsement Print Control
       //
       //=============================================================
       // NOTES:
       //  Utilizes the endorsement copybook (SCOPYEN01F/D/C).
       //  qqmode = 1 - Called to determine if form should be attached
       //  qqmode = 2 - Called to print endorsement.
       //  qqmode = 3 - Called for additional input.
       //  qqmode = 4 - Called for midterm processing.
       //=============================================================
     fWDRRL     IF   E           K DISK
     fWMEPL8    IF   E           K DISK
     fWMPRL1    IF   E           K DISK
     fWXPEL     UF A E           K DISK
     fWXPEL1    IF   E           K DISK
     f                                     RENAME(PPE01:PPE02)
     fSTSTL3    IF   E           K DISK
     fwmpxl1    if   e           k disk

       // Copybook F specs
     f/Copy sCopyEn01f
       //=============================================================
       // Definition Specs
       //=============================================================
       Dcl-Pr SR00401A;
        *N Char(3);
        *N Char(3);
        *N Char(9);
        *N Char(5);
        *N Char(3);
        *N Char(1);
        *N Packed(9:0);
        *N Char(1);
       End-Pr;
       Dcl-Pi SR00401A;
        qqCo# Char(3);
        qqFnd Char(3);
        qqEmp# Char(9);
        qqDiv Char(5);
        qqFyr Char(3);
        qqMode Char(1);
        qqRrn Packed(9:0);
        qqMod2 Char(1);
       End-Pi;
     d #1PGM           s             10A   INZ('W1054R')
     d #1FLAG          s              1A   INZ
     d XXPRST          s                   like(RSJRST)
     d XXUNID          s                   like(PEUNID)
     d X1END#          s                   like(PEEND#)

      *  AIRCRAFT DESCRIPTION
     D                 DS
     D  DS                     1    300
     D                                     DIM(10)
     D  FMC301                 1     30
     D  FMC302                31     60
     D  FMC303                61     90
     D  FMC304                91    120
     D  FMC305               121    150
     D  FMC306               151    180
     D  FMC307               181    210
     D  FMC308               211    240
     D  FMC309               241    270
     D  FMC310               271    300
      *  NUMBER OF SEATS
     D                 DS
     D  #S                     1     70  0
     D                                     DIM(10)
     D  FMA701                 1      7  0
     D  FMA702                 8     14  0
     D  FMA703                15     21  0
     D  FMA704                22     28  0
     D  FMA705                29     35  0
     D  FMA706                36     42  0
     D  FMA707                43     49  0
     D  FMA708                50     56  0
     D  FMA709                57     63  0
     D  FMA70A                64     70  0
      *  SEAT CHARGE
     D                 DS
     D  SC                     1     50  2
     D                                     DIM(10)
     D  FMA521                 1      5  2
     D  FMA522                 6     10  2
     D  FMA523                11     15  2
     D  FMA524                16     20  2
     D  FMA525                21     25  2
     D  FMA526                26     30  2
     D  FMA527                31     35  2
     D  FMA528                36     40  2
     D  FMA529                41     45  2
     D  FMA52A                46     50  2
      *  MAXIMUM CHARGE
     D                 DS
     D  MC                     1     90  2
     D                                     DIM(10)
     D  FMA921                 1      9  2
     D  FMA922                10     18  2
     D  FMA923                19     27  2
     D  FMA924                28     36  2
     D  FMA925                37     45  2
     D  FMA926                46     54  2
     D  FMA927                55     63  2
     D  FMA928                64     72  2
     D  FMA929                73     81  2
     D  FMA92A                82     90  2
      *  PREMIUM
     D                 DS
     D  PR                     1    110  2
     D                                     DIM(10)
     D  FMAM01                 1     11  2
     D  FMAM02                12     22  2
     D  FMAM03                23     33  2
     D  FMAM04                34     44  2
     D  FMAM05                45     55  2
     D  FMAM06                56     66  2
     D  FMAM07                67     77  2
     D  FMAM08                78     88  2
     D  FMAM09                89     99  2
     D  FMAM10               100    110  2
      *  STATES
     D                 DS
     D  ST                     1     20
     D                                     DIM(10)
     D  FMFL01                 1      1
     D  FMFL02                 2      2
     D  FMFL03                 3      3
     D  FMFL04                 4      4
     D  FMFL05                 5      5
     D  FMFL06                 6      6
     D  FMFL07                 7      7
     D  FMFL08                 8      8
     D  FMFL09                 9      9
     D  FMFL10                10     10
     D  FMFL11                11     11
     D  FMFL12                12     12
     D  FMFL13                13     13
     D  FMFL14                14     14
     D  FMFL15                15     15
     D  FMFL16                16     16
     D  FMFL17                17     17
     D  FMFL18                18     18
     D  FMFL19                19     19
     D  FMFL20                20     20
     D                 DS
     D  STATE                  1     20
     D                                     DIM(10)
     D  FMCD01                 1      2
     D  FMCD02                 3      4
     D  FMCD03                 5      6
     D  FMCD04                 7      8
     D  FMCD05                 9     10
     D  FMCD06                11     12
     D  FMCD07                13     14
     D  FMCD08                15     16
     D  FMCD09                17     18
     D  FMCD10                19     20
     D* Check Pgm
     D SRCHKPGM        pr                  ExtPgm('SRCHKPGM')
     D                               10
     D                                1
      /COPY WWCPOLDAT4
       // Copybook D specs
     d/Copy sCopyEn01d
       //=============================================================
       // Copybook required constants
       //=============================================================
       // Form ID
       Dcl-C qqForm Const('WC000401A ');
       // Additional entry program
       Dcl-C addPgm Const('SRxxxxxxI');
       //=============================================================
       // Main
      *  EMPLOYER PAYROLL FILE KLIST
     C     EPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    RRDCCD
      *  PAYROLL MASTER
     C     PRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    EPCLAS
     C                   KFLD                    XXEFDT            7 0
     C     PRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    EPCLAS
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
     C     STKY          KLIST
     C                   KFLD                    XXKEY            10
      *  STATISTICAL RATING/REPORTING FILE KLIST
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXPRST
     C                   KFLD                    ELPFDT
     C     RRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXPRST
     C     ##exky        klist
     C                   kfld                    epco#
     C                   kfld                    epclas
     C                   kfld                    epext

       //=============================================================
       // Copybook C specs (PLISTs, KLISTs and mainline)
     c/Copy sCopyEn01C
       //=============================================================
       // OthInf - Subroutine to load form specific information
       //=============================================================
       Begsr OthInf;
      *  If printing a policy page.  (Not called from W1054R)
      /Free
       SRCHKPGM(#1PGM:#1FLAG);
      /End-Free
     C                   Z-ADD     *ZERO         IX                2 0
     C                   MOVE      *BLANK        XXFRST            1
     C                   MOVE      *ZEROS        E00401
     C                   MOVE      *BLANKS       XXAN1
     C                   MOVE      *BLANKS       XXAN2
     C                   MOVE      *BLANKS       XXAN3
     C                   MOVE      *BLANKS       XXAN4
     C                   MOVE      *BLANKS       XXAN5
     C                   Z-ADD     0             I
      *  GET THE CURRENT STATISTICAL RATING/REPORTING RECORD AS OF THE ELPFDT
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
      *  A VALID RECORD WAS FOUND, AND THERE IS AN AIRCRAFT CODE DESCRIBED
     C     *IN60         IFEQ      *OFF
     C     RRDCCD        ANDNE     *ZERO
     C     EPKY          SETLL     PEP01
     C     EPKY          READE     PEP01                                  60
     C                   MOVE      EPSPEC        XXSPEC            1
     C                   MOVEL     EPEXT         XXEXT             5
     C                   ADD       1             IX
     C                   ADD       1             I
     C                   MOVE      EPDIV         XXDIV
     C     *IN60         DOWEQ     *OFF

           xxCxDate = GetCxDate(EPco#:EPfnd:EPemp#:EPdiv);
           If not(xxCxDate <= EPefdt and xxCxDate <> *zeros);
              //*** Beg Add ***** 01/27/23 *****************************
              Exsr ChkApprExclSt;
              If isApprExclSt = *On;
              //*** End Add ***** 01/27/23 *****************************

     C     EPSTAT        IFNE      'I'
     C                   Z-ADD     0             XXEFDT
     C                   MOVE(P)   ELCO#         ##CO#             3
     C                   MOVE(P)   ELFND         ##FND             3
     C                   MOVE(P)   ELFYR         ##FYR             3
     C                   MOVE(P)   ELEMP#        ##EMP#            9
     C                   MOVE(P)   ELDIV         ##DIV             5
     C                   MOVE(P)   EPEFDT        ##DATE            7
     C                   MOVE(P)   'P'           ##TYPE            1
     C                   MOVE(P)   EPCLAS        ##CLAS            5
     C                   CALL      'SREFFD'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##DATE
     C                   PARM                    ##TYPE
     C                   PARM                    ##CLAS
     C                   MOVE      ##DATE        XXEFDT
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  60
     C     *IN60         IFEQ      *OFF
     C     EPSPEC        IFNE      XXSPEC
     C     EPDIV         ORNE      XXDIV
     C     EPEXT         ORNE      XXEXT
     C                   MOVE      EPSPEC        XXSPEC
     C                   MOVEL     EPEXT         XXEXT
     C                   ADD       1             IX
     C                   ADD       1             I
     C                   ENDIF
     C                   MOVE      EPDIV         XXDIV
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        ST(IX)
     C                   MOVE      RSJRST        STATE(IX)
     C                   ENDIF
     C                   MOVE      EPACDS        DS(IX)
     C                   if        epext <> ' '
     C     ##exky        chain     ppx01
     C                   if        %found(wmpxl1)
     C                   move      pxdesc        DS(IX)
     C                   endif
     C                   endif
     C                   MOVE      EPEAPR        #S(IX)
     C                   MOVE(P)   PRRTCD        QQRTCD
     C                   MOVE(P)   PRCLAS        QQCLAS
     C                   MOVE(P)   ELPFDT        QQEFDT
     C                   CALL      'SRLCM'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQRTCD            3
     C                   PARM                    QQCLAS            5
     C                   PARM                    QQEFDT            7
     C                   PARM      *BLANKS       QQLCM             5
     C     QQLCM         IFNE      *BLANKS
     C                   MOVE      QQLCM         XXLCM             5 4
     C                   ENDIF
     C                   MOVE      PRRT          SC(IX)
     C                   MOVE      RRAMAX        MC(IX)
     C                   ADD       EPADLD        PR(IX)
     C     I             IFEQ      1
     C                   MOVEL(P)  RSJRST        XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      STNST         X2ST1
     C                   ENDIF
     C                   MOVEL(P)  EPACDS        XXAN1
     C                   Z-ADD     PRRT          XXSC1
     C                   Z-ADD     RRAMAX        XXMX1
     C                   ADD       EPADLD        XXEP1
     C                   ENDIF
     C     I             IFEQ      2
     C                   MOVEL(P)  RSJRST        XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      STNST         X2ST2
     C                   ENDIF
     C                   MOVEL(P)  EPACDS        XXAN2
     C                   Z-ADD     PRRT          XXSC2
     C                   Z-ADD     RRAMAX        XXMX2
     C                   ADD       EPADLD        XXEP2
     C                   ENDIF
     C     I             IFEQ      3
     C                   MOVEL(P)  RSJRST        XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      STNST         X2ST3
     C                   ENDIF
     C                   MOVEL(P)  EPACDS        XXAN3
     C                   Z-ADD     PRRT          XXSC3
     C                   Z-ADD     RRAMAX        XXMX3
     C                   ADD       EPADLD        XXEP3
     C                   ENDIF
     C     I             IFEQ      4
     C                   MOVEL(P)  RSJRST        XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      STNST         X2ST4
     C                   ENDIF
     C                   MOVEL(P)  EPACDS        XXAN4
     C                   Z-ADD     PRRT          XXSC4
     C                   Z-ADD     RRAMAX        XXMX4
     C                   ADD       EPADLD        XXEP4
     C                   ENDIF
     C     I             IFEQ      5
     C                   MOVEL(P)  RSJRST        XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      STNST         X2ST5
     C                   ENDIF
     C                   MOVEL(P)  EPACDS        XXAN5
     C                   Z-ADD     PRRT          XXSC5
     C                   Z-ADD     RRAMAX        XXMX5
     C                   ADD       EPADLD        XXEP5
     C                   ENDIF
     C     I             IFEQ      6
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
     C                   MOVEL(P)  E00401        PEDATA
      *  If printing a policy page.  (Not called from W1054R)
     C     #1FLAG        IFEQ      '0'
     C     XXFRST        IFEQ      *BLANK
     C                   UPDATE    PPE01
     C                   MOVE      'Y'           XXFRST
     C                   ELSE
     C                   WRITE     PPE01
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     1             I
     C                   MOVE      *ZEROS        E00401
     C                   MOVE      *BLANKS       XXAN1
     C                   MOVE      *BLANKS       XXAN2
     C                   MOVE      *BLANKS       XXAN3
     C                   MOVE      *BLANKS       XXAN4
     C                   MOVE      *BLANKS       XXAN5
     C                   MOVEL(P)  RSJRST        XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      STNST         X2ST1
     C                   ENDIF
     C                   MOVEL(P)  EPACDS        XXAN1
     C                   Z-ADD     PRRT          XXSC1
     C                   Z-ADD     RRAMAX        XXMX1
     C                   ADD       EPADLD        XXEP1
     C                   ENDIF
     C     IX            IFEQ      10

      // *** Beg Add * 07/17/16
      /Free
          If #1Flag = '0';
             PrintingPolicy = *On;
          Else;
             PrintingPolicy = *Off;
          Endif;
          Exsr WriteWorkFile;
          WriteInCopybook = *Off;
      /End-Free
      // *** End Add * 07/17/16

     C                   MOVE      *BLANK        DS
     C                   MOVE      *ZERO         #S
     C                   MOVE      *ZERO         SC
     C                   MOVE      *ZERO         MC
     C                   MOVE      *ZERO         PR
     C                   MOVE      *BLANK        ST
     C                   Z-ADD     *ZERO         IX
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
             //*** Beg Add ***** 01/27/23 *****************************
             Endif;
             //*** End Add ***** 01/27/23 *****************************
          EndIf;

     C     EPKY          READE     PEP01                                  60
     C                   ENDDO
     C                   ENDIF
      *  WRITE THE OUTPUT RECORD
     C     IX            IFNE      *ZERO

      // *** Beg Add * 07/17/16
      /Free
          If #1Flag = '0';
             PrintingPolicy = *On;
          Else;
             PrintingPolicy = *Off;
          Endif;
          Exsr WriteWorkFile;
      /End-Free
      // *** End Add * 07/17/16

     C                   ENDIF
     C     I             IFNE      0
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
     C                   MOVEL(P)  E00401        PEDATA
      *  If printing a policy page.  (Not called from W1054R)
     C     #1FLAG        IFEQ      '0'
     C     XXFRST        IFEQ      *BLANK
     C                   UPDATE    PPE01
     C                   MOVE      'Y'           XXFRST
     C                   ELSE
     C                   WRITE     PPE01
     C                   ENDIF
     C                   ENDIF
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
        Setll (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
        Reade (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
        Dow not %eof(wdell1);
           xxDiv = elDiv;
           xxPrst = rsJrst;

      *  GET THE CURRENT STATISTICAL RATING/REPORTING RECORD AS OF THE ELPFDT
           SetGT (xxCo#:xxFnd:xxPrst:elPfdt) prr01;
           ReadPE (xxCo#:xxFnd:xxPrst) prr01;
      *  A VALID RECORD WAS FOUND, AND THERE IS AN AIRCRAFT CODE DESCRIBED
           If not %eof(wdrrl) and rrDccd <> 0;
              Setll (xxCo#:xxFnd:xxFyr:xxEmp#:rrDccd) pep01;
              ReadE (xxCo#:xxFnd:xxFyr:xxEmp#:rrDccd) pep01;
              Dow not %eof(wmepl8);
                 If epStat <> 'I' and elPfdt <> elPtdt and
                  elPfdt < 1150101;
                    crtFrm = *on;
                    Leave;
                 EndIf;
                 ReadE (xxCo#:xxFnd:xxFyr:xxEmp#:rrDccd) pep01;
              EndDo;
              If crtFrm = *on;
                 Leave;
              EndIf;
           EndIf;
           ReadE (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
        EndDo;
       Endsr;
