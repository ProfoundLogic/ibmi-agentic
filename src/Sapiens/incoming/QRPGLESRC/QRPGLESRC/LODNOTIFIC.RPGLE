     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE LODNOTIFIC - Load Notification Configuration Files.
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
     H BNDDIR('WTBNDDIR')
      *=====================================================
      *  Created on 02/08/2018 by C:136 for Project SY407
      *=====================================================
      *  Revisions:
      *
      *=====================================================
     fsmcol     if   e           k Disk    Rename(pco01:pco00)
     fwtgil     uf a e           k Disk    Rename(pgi01:pgi00)
     fwtgtl     uf a e           k Disk    Rename(pgt01:pgt00)
     d psds          esds                  Extname(SZQ1P)
     d/copy SprDate
     dtbid             s             60    Dim(5) Ctdata
     ddtl              s             90    Dim(50) Ctdata
     ddtl2             s             60    Dim(50) Ctdata
     dix               s              5s 0
      /Free
       Dou %Eof(SMCOL);
          Read pco00;
          If %Eof;
             Leave;
          Endif;
          For ix = 1 to %Elem(tbid);
             If tbid(ix) = *Blank;
                Leave;
             Endif;
             Chain (COco#:%Subst(tbid(ix):1:4)) pgi00;
             Clear pgi00;
             GIco# = COco#;
             GItbid = %Subst(tbid(ix):1:4);
             GIfg01 = %Subst(tbid(ix):6:1);
             GIdesc = %Subst(tbid(ix):8:40);
             GIstat = 'A';
             GIuser = Q1user;
             GIpgm = Q1pgm;
             GItime = TimeTo6(SystemTime());
             GIdt = DateTo7(SystemDate());
             GIfn01 = %Subst(tbid(ix):49:10);
             If Not(%Found);
                Write PGI00;
             Else;
                Update PGI00;
             Endif;
          EndFor;
          For ix = 1 to %Elem(dtl);
             If dtl(ix) = *Blank;
                Leave;
             Endif;
             Chain (COco#:%Subst(dtl(ix):1:4):%Subst(dtl(ix):6:10)) pgt00;
             Clear pgt00;
             GTco# = COco#;
             GTtbid = %Subst(dtl(ix):1:4);
             GTkey  = %Subst(dtl(ix):6:10);
             GTdesc = %Subst(dtl(ix):17:40);
             GTstat = 'A';
             GTcf01 = %Subst(dtl(ix):58:10);
             GTfg10 = %Subst(dtl(ix):70:1);
             If dtl2(ix) <> *Blanks;
               // C = Char
               // N = Numeric
               // *Blank for either
                GTfg02 = %Subst(dtl2(ix):1:1);
               // Label for GTfg02
                GTcf02 = %Subst(dtl2(ix):3:10);
                GTcf03 = %Subst(dtl2(ix):13:10);
               // C = Char
               // N = Numeric
                GTfg04 = %Subst(dtl2(ix):24:1);
               // Label for GTfg04
                GTcf04 = %Subst(dtl2(ix):26:10);
                GTcf05 = %Subst(dtl2(ix):36:10);
             Endif;
             GTuser = Q1user;
             GTpgm = Q1pgm;
             GTtime = TimeTo6(SystemTime());
             GTdt = DateTo7(SystemDate());
             If Not(%Found);
                Write PGT00;
             Else;
                Update PGT00;
             Endif;
          EndFor;
       Enddo;
       *Inlr = *On;
       Return;
      /End-Free
** WTGIP
N001 P S/R Online Notifications                 S/R Online
CRDF   Carrier Rating Defaults
N003 P Status Notifications


** WTGTP
N001 SRINITMTH  Self-Reporting Initial Monthly           POLICY                          1
N001 SRINITWKLY Self-Reporting Initial Weekly            POLICY                          2
N001 SRREMINDER Self-Reporting Reminder                  POLICY                          3
N001 SRINITFAIL Self-Reporting Initial Failure           POLICY                          4
N001 SRREMIFAIL Self-Reporting Reminder Failure          POLICY                          5
N001 EFTPAYCONF EFT Payment Confirmation                 POLICY                          6
N001 3RDPTYPEND 3rd Party Pending Payment                POLICY                          7
N001 3RDPTYFAIL 3rd Party Payment Failure                POLICY                          8
N001 3RDPTYCONF 3rd Party Payment Confirmation           POLICY                          9
CRDF CONCREDIT  Contractor Credit                        Percent                        10
CRDF DEDUCT     Deductibles                              Code                           11
CRDF DEV1       Deviation 1                              Percent                        12
CRDF DEV2       Deviation 2                              Percent                        13
CRDF EXPMOD     Experience Mod                           Factor                         14
CRDF INCLIMIT   Employer Liability Limits                Code                           15
CRDF MEMBERFEE  Membership Fee                           Amount                         16
CRDF PRMDISC    Premium Discount                         Code                           17
CRDF SAFETYFEE  Safety Fee                               Percent                        18
CRDF SUBJECT1A  Subject 1A                               Percent                        19
CRDF SUBJECT1B  Subject 1B                               Percent                        20
CRDF VOLUNTARY  Voluntary Labor                          Amount                         21
CRDF WAIVER     Type of Waiver                           Percent                        22
CRDF REINSUR    Facultative Charge                       Percent                        23
CRDF ARAP       ARAP Factor                              Factor                         24
CRDF SCHDEBCRED Scheduled Debit/Credit                   Percent                        25
N003 SUBMITTED  Submitted                                QTESTATUS   U                  26
N003 AGTREVIEW  Agent Review                             QTESTATUS   Y                  27
N003 PENDING    Pending                                  QTESTATUS   P                  28
N003 LOST       Lost                                     QTESTATUS   L                  29
N003 UWREVIEW   Underwriter Review                       QTESTATUS   V                  30
N003 CLOSED     Closed                                   QTESTATUS   S                  31
N003 WAITING    Waiting                                  QTESTATUS   I                  32
N003 BOUND      Bound                                    QTESTATUS   B                  33
N001 SRDUEDATE  Self-Reporting Monthly Due Date          POLICY                         34

** Secondary fields
N Initial Day                                                                            1
                                                                                         2
N Reminder Day                                                                           3
                                                                                         4
                                                                                         5
C Send Confirmation    C Attach Worksheet                                                6
                                                                                         7
                                                                                         8
C Send Confirmation    C Attach Worksheet                                                9
                                                                                        10
                                                                                        11
                                                                                        12
                                                                                        13
                                                                                        14
                                                                                        15
                                                                                        16
  Percent                                                                               17
                                                                                        18
                                                                                        19
                                                                                        20
                                                                                        21
  Amount                                                                                22
                                                                                        23
                                                                                        24
                                                                                        25
C Send to Underwriter                                                                   26
C Send to Underwriter                                                                   27
C Send to Underwriter                                                                   28
C Send to Underwriter                                                                   29
C Send to Underwriter                                                                   30
C Send to Underwriter                                                                   31
C Send to Underwriter                                                                   32
C Send to Underwriter                                                                   32
                                                                                        33
                                                                                        34
