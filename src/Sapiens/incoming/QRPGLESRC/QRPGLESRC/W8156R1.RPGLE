    H*=========================================================================
    H* (c) 2016 StoneRiver, Inc. All rights Reserved.  -
    H*       StoneRiver is a trademark of StoneRiver, Inc.
    H*=========================================================================
    H/TITLE   W8156R1 - Gallagher-Basset Work File Load
     h DFTACTGRP(*no) ACTGRP(*Caller)
     h option(*nodebugio: *srcstmt)
    H*=========================================================================
     H* Created on 9/19/2016 by c:62 for project 114WYV
    H*=========================================================================
     h* Revisions
     h*
    F*=========================================================================
    F*        F i l e s
    F*=========================================================================
    F*
    F* WC Claim Setup File
     Fwagbfp    if   e             Disk    Rename(pgbf01:pgbf0p)
    F*
    F* Claim setup
     Fwagbcp    o    e             Disk    Rename(pgbc01:pgbc0p)
    F*
    F* Reserves/Exposure File
     Fwagbrp    o    e             Disk    Rename(pgbr01:pgbr0p)
    F*
    F* Payment File
     Fwagbpp    o    e             Disk    Rename(pgbp01:pgbp0p)
    F*
    F* 3rd Party Recovery File
     Fwagb3p    o    e             Disk    Rename(pgb301:pgb30p)
    F*
    F* Excess Recovery File
     Fwagbep    o    e             Disk    Rename(pgbe01:pgbe0p)
    F*
    F* Header File
     Fwagbhp    o    e             Disk    Rename(pgbh01:pgbh0p)
    F*
    F* Trailer File
     Fwagbtp    o    e             Disk    Rename(pgbt01:pgbt0p)

    D*=========================================================================
    D* Prototypes
    D*=========================================================================
     d* Program End
     d ProgramEnd      pr
    D*=========================================================================
    D* Arrays and Data Structures
    D*=========================================================================
     D fids01        e ds                  ExtName(szq2p)
     D psds          esds                  ExtName(szq1p)
    D*=========================================================================
    D* Externally Defined Data Structures
    D*=========================================================================
     D header        e ds                  ExtName(wagbhp)
     D claimDtl      e ds                  ExtName(wagbcp)
     D reserves      e ds                  ExtName(wagbrp)
     D payments      e ds                  ExtName(wagbpp)
     D thirdParty    e ds                  ExtName(wagb3p)
     D excessRec     e ds                  ExtName(wagbep)
     D trailer       e ds                  ExtName(wagbtp)
    D*=========================================================================
    D* Variables
    D*=========================================================================
     D recordType      s              2
    C*=========================================================================
    C*      M  A I  N    L  O  G  I  C
    C*=========================================================================
      /free

       // Read thru the flat file
       Read pgbf0p;
       DoW Not %Eof(wagbfp);

          recordType = %Subst(GBFdata:35:2);

          select;
          When recordType = 'CT';
             claimDtl = GBFdata;
             Write pgbc0p;
          When recordType = 'ET';
             reserves = GBFdata;
             Write pgbr0p;
          When recordType = 'PT';
             payments = GBFdata;
             Write pgbp0p;
          When recordType = 'RT';
             thirdParty = GBFdata;
             Write pgb30p;
          When recordType = 'XT';
             excessRec = GBFdata;
             Write pgbe0p;
          When recordType = 'HT';
             header = GBFdata;
             Write pgbh0p;
          When recordType = 'TT';
             trailer = GBFdata;
             Write pgbt0p;
          EndSl;

          Read pgbf0p;
       EndDo;

       // End of Program Processes
       ProgramEnd();

       //===================================================
       // Initialization Routine
       //===================================================
       Begsr *InzSR;


       EndSR;

      /End-Free
       //===================================================================
       // ProgramEnd - Program End Processing
       //===================================================================
     P ProgramEnd      B
     D ProgramEnd      PI
      /free

       *inLR = *On;

      /End-Free
     P ProgramEnd      E
