     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller) bnddir('STBNDDIR')
      //
     H/TITLE LODWTENP - Load WTENP file
      //****************************************************************
      // This program will load the endorsment file with values.       *
      // The list of endorsements should be cleared with each major    *
      //  release.                                                     *
      // No parameters are required.  It can be run for one group, all *
      //  groups or just test groups. ('xxx', '*ALL', '*TEST')         *
      //  If no parms are passed in, it will assume test groups.       *
      // This can be added to SYPOSTPGM2.                              *
      //****************************************************************
      // Revisions:                                                    *
      //                                                               *
      // 04/28/18  C:33  Project: WC250
      //                 Add CA endorsements                           *
      //                                                               *
      // 01/17/18  C:33  Project: 89WCB                                *
      //                 Add WC370404C                                 *
      //                                                               *
      // 01/15/18  C:33  Project: 89WCA                                *
      //                 Add WC190402A                                 *
      //                 Add WC190601G                                 *
      //                 Add WC170601I                                 *
      //                 Add WC060601A                                 *
      //                 Add WC420301I                                 *
      //                 Add WC240604B                                 *
      //                 Add WC310407                                  *
      //                                                               *
      // 12/20/17  C:33  Project: 89WBZ                                *
      //                 Add WC290411E                                 *
      //                                                               *
      // 12/19/17  C:33  Support: 51563                                *
      //                 Make WC000406 auto-attach                     *
      //                                                               *
      // 12/18/17  C:33  Support: 51727                                *
      //                 Make WC090607 auto-attach for 150             *
      //****************************************************************
     Fsmcol     if   e           k disk    rename(pco01:pco00)
     Fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     Fwtenl     uf a e           k disk    rename(pen01:pen00)
     Fwtesl     uf a e           k disk    rename(pes01:pes00)

     D#ofEnd           c                   50
     DarrEnd           s             15    dim(#ofEnd) ctdata perrcd(1)
     DarrEnd1          s             15    dim(#ofEnd) ctdata perrcd(1)
     D***  Begin Add  ***  12/18/17  **************************************
     DarrEnd1_150      s             15    dim(#ofEnd) ctdata perrcd(1)
     D***  End   Add  ***  12/18/17  **************************************
     DarrEnd2          s             15    dim(#ofEnd) ctdata perrcd(1)
     D***  Begin Add  ***  12/18/17  **************************************
     DarrEnd2_150      s             15    dim(#ofEnd) ctdata perrcd(1)
     D***  End   Add  ***  12/18/17  **************************************
     DarrEnd3          s             15    dim(#ofEnd) ctdata perrcd(1)
     DarrEnd4          s             15    dim(#ofEnd) ctdata perrcd(1)
     DarrEnd5          s             15    dim(#ofEnd) ctdata perrcd(1)
     DarrEndAct        s             10    dim(#ofEnd) alt(arrEnd)
     DarrEndInfo       s             80    dim(#ofEnd) alt(arrEnd1)
     D***  Begin Add  ***  12/18/17  **************************************
     DarrEndInfo150    s             80    dim(#ofEnd) alt(arrEnd1_150)
     D***  End   Add  ***  12/18/17  **************************************
     DarrEndPgm        s             80    dim(#ofEnd) alt(arrEnd2)
     D***  Begin Add  ***  12/18/17  **************************************
     DarrEndPgm150     s             80    dim(#ofEnd) alt(arrEnd2_150)
     D***  End   Add  ***  12/18/17  **************************************
     DarrEndRpl        s             80    dim(#ofEnd) alt(arrEnd3)
     DarrEndDesc       s             80    dim(#ofEnd) alt(arrEnd4)
     DarrEndDsc2       s             80    dim(#ofEnd) alt(arrEnd5)
     D#ofEndSt         c                   10
     DarrES            s             15    dim(#ofEndSt) ctdata perrcd(1)
     DarrESInfo        s             10    dim(#ofEndSt) alt(arrES)
     D
     ddsEndAct         ds                  qualified
     d* *ADD/*UPDATE
     d action                        10
     ddsEndInfo        ds                  qualified
     d info                         100
     d edDt                           7                 overlay(info:1)
     d                                     inz('0000000')
     d st                                  like(ENst)   overlay(info:9)
     d mnAu                                like(ENmnAu) overlay(info:12)
     d prnt                                like(ENprnt) overlay(info:17)
     d sngl                                like(ENsngl) overlay(info:22)
     d peof                                like(ENpeoF) overlay(info:27)
     d fldr                                like(ENfldr) overlay(info:32)
     ddsEndPgm         ds                  qualified
     d pgms
     d pgm1                                like(ENpgm1) overlay(pgms:1)
     d pgm2                                like(ENpgm2) overlay(pgms:12)
     d pgm3                                like(ENpgm3) overlay(pgms:23)
     ddsEndRpl         ds                  qualified
     d rpl
     d rpFm                                like(ENrpFm) overlay(rpl:1)
     d rpDt                           7                 overlay(rpl:17)
     d                                     inz('0000000')
     dstrEndDesc       s                   like(ENdesc)
     dstrEndDsc2       s                   like(ENdsc2)
     ddsESInfo         ds                  qualified
     d info
     d st                                  like(ESst)   overlay(info:1)
     d apEx                                like(ESapEx) overlay(info:4)

       //***  Begin Add  ***  12/18/17  *********************************
     DcltId            s              3    inz('000')
       //***  End   Add  ***  12/18/17  *********************************
     DdecFnd           s                   like(FNfnd)
     DisGroupsAll      s              1n   inz
     DisGroupsOne      s              1n   inz
     DisGroupsTest     s              1n   inz
     Di1               s              5  0
     Di2               s              5  0
       //***  Begin Add  ***  12/18/17  *********************************
     Di2Clt            s              5  0
       //***  End   Add  ***  12/18/17  *********************************

     Dpsds           esds                  extname(SZQ1P)

       //***  Begin Add  ***  12/18/17  *********************************
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3
       //***  End   Add  ***  12/18/17  *********************************

      /copy sprdate

      //
      // Group # - 3 character, left justified
      // *TEST
      // *ALL
     C     *ENTRY        PLIST
     C                   PARM                    @@FND             5
      //
      //=================================================================
      // BEGIN
      //=================================================================
      //
      //  Process only one fund if entry parm
      //
      /FREE
       //***  Begin Add  ***  12/18/17  *********************************
       GetClientID(cltId);
       //***  End   Add  ***  12/18/17  *********************************
       isGroupsTest = %parms < 1;
       If %parms >= 1;
          isGroupsAll = @@fnd = '*ALL';
          isGroupsTest = @@fnd = '*TEST';
       Endif;
       If not isGroupsAll and not isGroupsTest;
          decFnd = %int(%subst(@@fnd:1:3));
          isGroupsOne = *on;
       Endif;

       Setll *loval pco00;
       Dou %eof(smcol);
          Read pco00;
          Select;
          When %eof;
             Iter;
          Endsl;
          Exsr ProcessEN;
       Enddo;

       *inlr = *on;

       //=====================================================================
       Begsr ProcessEN;
        For i1 = 1 to #ofEnd;
           If arrEnd(i1) = *blank;
              Leave;
           Endif;
           Clear pen00;
           Reset dsEndAct;
           Reset dsEndInfo;
           Reset dsEndPgm;
           Reset dsEndRpl;
           Clear strEndDesc;
           Clear strEndDsc2;
           dsEndAct = arrEndAct(i1);
           Exsr ProcessES;
         //***  Begin Chg  ***  12/18/17  ******************************
         //i2 = %lookup(arrEnd(i1):arrEnd1);
         //If i2 = 0;
         //   Iter;
         //Endif;
         //dsEndInfo = arrEndInfo(i2);
           Select;
           When cltId = '150';
              i2Clt = %lookup(arrEnd(i1):arrEnd1_150);
              If i2Clt <> *zero;
                 dsEndInfo = arrEndInfo150(i2Clt);
              Endif;
           Other;
              i2Clt = *zero;
           Endsl;
           If i2Clt = *zero;
              i2 = %lookup(arrEnd(i1):arrEnd1);
              If i2 <> *zero;
                 dsEndInfo = arrEndInfo(i2);
              Endif;
           Endif;
           If i2 = 0 and i2Clt = 0;
              Iter;
           Endif;
         //***  End   Chg  ***  12/18/17  ******************************
         //***  Begin chg  ***  12/18/17  ******************************
         //i2 = %lookup(arrEnd(i1):arrEnd2);
         //If i2 <> 0;
         //   dsEndPgm = arrEndPgm(i2);
         //Endif;
           Select;
           When cltId = '150';
              i2Clt = %lookup(arrEnd(i1):arrEnd2_150);
              If i2Clt <> *zero;
                 dsEndPgm = arrEndPgm150(i2Clt);
              Endif;
           Other;
              i2Clt = *zero;
           Endsl;
           If i2Clt = *zero;
              i2 = %lookup(arrEnd(i1):arrEnd2);
              If i2 <> 0;
                 dsEndPgm = arrEndPgm(i2);
              Endif;
           Endif;
         //***  End   Add  ***  12/18/17  ******************************
           i2 = %lookup(arrEnd(i1):arrEnd3);
           If i2 <> 0;
              dsEndRpl = arrEndRpl(i2);
           Endif;
           i2 = %lookup(arrEnd(i1):arrEnd4);
           If i2 <> 0;
              strEndDesc = arrEndDesc(i2);
           Endif;
           i2 = %lookup(arrEnd(i1):arrEnd5);
           If i2 <> 0;
              strEndDsc2 = arrEndDsc2(i2);
           Endif;
           Exsr ProcessGroup;
        Endfor;
       Endsr;

       //=====================================================================
       Begsr ProcessES;
        i2 = 0;
        Dou i2 = 0;
           i2 = %lookup(arrEnd(i1):arrES:i2+1);
           Select;
           When i2 = 0;
              Iter;
           Endsl;
           Clear pes00;
           dsESInfo = arrESInfo(i2);
           Chain (COco#:arrEnd(i1):dsESInfo.st) pes00;
           Select;
           When %found(wtesl) and dsEndAct.action = '*ADD';
              Iter;
           Endsl;
           ESco#  = COco#;
           ESform = arrEnd(i1);
           ESst   = dsESInfo.st;
           ESapEx = dsESInfo.apEx;
           ESpgm  = q1Pgm;
           ESusr  = q1User;
           ESdt   = DateTo7(SystemDate());
           EStim  = TimeTo6(SystemTime());
           Select;
           When %found(wtesl) and dsEndAct.action = '*UPDATE';
              Update pes00;
           Other;
              ESiPgm = ESpgm;
              ESiUsr = ESusr;
              ESiDt  = ESdt;
              ESiTim = EStim ;
              Write pes00;
           Endsl;
        Enddo;
       Endsr;

       //=====================================================================
       Begsr ProcessGroup;
        Setll COco# pfn00;
        Dou %eof(wmfnl);
           Reade COco# pfn00;
           Select;
           When %eof;
              Iter;
           When isGroupsAll;
           When isGroupsTest and FNtest <> 'Y';
              Iter;
           When isGroupsOne and FNfnd <> decFnd;
              Iter;
           Endsl;
           Clear pen00;
           Chain (FNco#:FNfnd:arrEnd(i1)) pen00;
           Select;
           When %found(wtenl) and dsEndAct.action = '*ADD';
              Iter;
           Endsl;
           ENco#  = FNco#;
           ENfnd  = FNfnd;
           ENform = arrEnd(i1);
           ENedDt = %int(dsEndInfo.edDt);
           ENst   = dsEndInfo.st;
           ENmnAu = dsEndInfo.mnAu;
           ENprnt = dsEndInfo.prnt;
           ENpeof = dsEndInfo.peof;
           ENfldr = dsEndInfo.fldr;
           ENsngl = dsEndInfo.sngl;
           ENpgm1 = dsEndPgm.pgm1;
           ENpgm2 = dsEndPgm.pgm2;
           ENpgm3 = dsEndPgm.pgm3;
           ENrpFm = dsEndRpl.rpFm;
           ENrpDt = %int(dsEndRpl.rpDt);
           ENdesc = strEndDesc;
           ENdsc2 = strEndDsc2;
           ENpgm  = q1Pgm;
           ENuser = q1User;
           ENdt   = DateTo7(SystemDate());
           ENtime = TimeTo6(SystemTime());
           Select;
           When %found(wtenl) and dsEndAct.action = '*UPDATE';
              Update pen00;
           Other;
            //ENiPgm = ENpgm;
            //ENiUsr = ENuser;
            //ENiDt  = ENdt;
            //ENiTim = ENtime;
              ENstat = 'A';
              Write pen00;
           Endsl;
        Enddo;
       Endsr;
      /End-free
**             Action
WC000301       *ADD
WC000301A      *UPDATE
WC310317       *ADD
WC310318       *ADD
WC260301A      *ADD
WC260302A      *ADD
WC090607       *UPDATE
WC000406       *UPDATE
WC000406A      *UPDATE
WC290411D      *UPDATE
WC290411E      *ADD
WC190402A      *ADD
WC190601G      *ADD
WC170601I      *ADD
WC060601A      *ADD
WC420301I      *ADD
WC240604B      *ADD
WC310407       *ADD
WC190402       *UPDATE
WC190601F      *UPDATE
WC170601H      *UPDATE
WC060601       *UPDATE
WC420301H      *UPDATE
WC240604A      *UPDATE
WC370404C      *ADD
WC370404B      *UPDATE
WC040301D      *ADD
WC040302D      *ADD
WC040303C      *ADD
WC040318C      *ADD
WC040338B      *ADD
WC040339B      *ADD
WC040340B      *ADD
WC040341B      *ADD
WC040342B      *ADD
WC040343B      *ADD
WC040344B      *ADD
WC040366       *ADD
WC040301C      *UPDATE
WC040302C      *UPDATE
WC040303B      *UPDATE
WC040318B      *UPDATE
WC040338A      *UPDATE
WC040339A      *UPDATE
WC040340A      *UPDATE
WC040341A      *UPDATE
WC040342A      *UPDATE
WC040343A      *UPDATE
WC040344A      *UPDATE

**             edDt    st mnAu prnt sngl peof fldr
WC000301       0940101    M    Y    S         WCPOLSV3
WC310317       0930701 NY M    Y    S         WCPOLSV3
WC310318       0930701 NY M    Y    S         WCPOLSV3
WC260301A      1170501 NE A    Y    S    5    WCPOLSV3
WC260302A      1170501 NE A    Y    S    5    WCPOLSV3
WC090607       1160701 FL M    Y    S         WCPOLSV3
WC000406       0840810    A    Y    S         WCPOLSV3
WC000406A      0950815    A    Y    S         WCPOLSV3
WC290411D      1170101 NJ A    Y    S         WCPOLSV3
WC290411E      1180101 NJ A    Y    S         WCPOLSV3
WC190402A      1171001 MD A    Y    S         WCPOLSV3
WC190601G      1171001 MD A    Y    S         WCPOLSV3
WC170601I      1170901 LA A    Y    S         WCPOLSV3
WC060601A      1171001 CT A    Y    S         WCPOLSV3
WC420301I      1170701 TX A    Y    S         WCPOLSV3
WC240604B      1170101 MO A    Y    S         WCPOLSV3
WC310407       1171001 NY A    Y    S         WCPOLSV3
WC370404C      1171001 PA M    Y    S         WCPOLSV3
WC040301D      1180201 CA A    Y    S         WCPOLSV3
WC040302D      1180701 CA A    Y    S         WCPOLSV3
WC040303C      1180701 CA A    Y    S         WCPOLSV3
WC040318C      1180701 CA A    Y    S         WCPOLSV3
WC040338B      1180201 CA M    Y    S         WCPOLSV3
WC040339B      1180201 CA M    Y    S         WCPOLSV3
WC040340B      1180201 CA M    Y    S         WCPOLSV3
WC040341B      1180201 CA M    Y    S         WCPOLSV3
WC040342B      1180201 CA M    Y    S         WCPOLSV3
WC040343B      1180201 CA M    Y    S         WCPOLSV3
WC040344B      1180201 CA M    Y    S         WCPOLSV3
WC040366       1180701 CA M    Y    S         WCPOLSV3
** 150         edDt    st mnAu prnt sngl peof fldr
WC090607       1160701 FL A    Y    S         WCPOLSV3
**             pgm1       pgm2       pgm3
WC000301                  SR00301    SR00301
WC310317                  SR31317    SR31317
WC310318                  SR31318    SR31318
WC260301A      SR26301A   SR26301A
WC260302A      SR26302A   SR26302A
WC090607                  SR09607    SR09607
WC000406       SR00406    SR00406
WC000406A      SR00406A   SR00406A
WC290411D      SRTEXT     SRTEXT
WC290411E      SRTEXT     SRTEXT
WC190402A      SR19402A   SR19402A
WC190601G      SRTEXT     SRTEXT
WC170601I      SR17601I   SR17601I
WC060601A      SRTEXT     SRTEXT
WC420301I      SRTEXT     SRTEXT
WC240604B      SR24604B   SR24604B
WC310407       SR31407    SR31407
WC370404C                 SRTEXT     SRTEXT
WC040301D      SRTEXT     SRTEXT
WC040302D      SR04302D   SR04302D
WC040303C      SR04303C   SR04303C
WC040318C      SR04318C   SR04318C
WC040338B                 SR04338B   SR04338B
WC040339B                 SR04339B   SR04339B
WC040340B                 SR04340B   SR04340B
WC040341B                 SR04341B   SR04341B
WC040342B                 SR04342B   SR04342B
WC040343B                 SR04343B   SR04343B
WC040344B                 SR04344B   SR04344B
WC040366                  SR04366
** 150         pgm1       pgm2       pgm3
WC090607       SR09607    SR09607
**             rpFm            rpDt
WC000406       WC000406A       0950815
WC290411D      WC290411E       1180101
WC190402       WC190402A       1171001
WC190601F      WC190601G       1171001
WC170601H      WC170601I       1170901
WC060601       WC060601A       1171001
WC420301H      WC420301I       1170701
WC240604A      WC240604B       1170101
WC370404B      WC370404C       1171001
WC040301C      WC040301D       1180201
WC040302C      WC040302D       1180701
WC040303B      WC040303C       1180701
WC040318B      WC040318C       1180701
WC040338A      WC040338B       1180201
WC040339A      WC040339B       1180201
WC040340A      WC040340B       1180201
WC040341A      WC040341B       1180201
WC040342A      WC040342B       1180201
WC040343A      WC040343B       1180201
WC040344A      WC040344B       1180201
**             desc
WC000301       Alternate Employer Endorsement
WC310317       NY - Labor Contractor Endorsement
WC310318       NY - Labor Contractor Exclusion Endorsement
WC260301A      NE - Endorsement for Policy Covering the PEO
WC260302A      NE - Endorsement for Policy Covering the Client
WC090607       FL Guaranty Association Surcharge Endorsement
WC000406       Premium Discount
WC000406A      Premium Discount
WC290411D      NJ Premium Discount Endorsemen
WC290411E      NJ Premium Discount Endorsemen
WC190402A      MD Alcohol- and Drug-Free Workplace Prem Credit
WC190601G      MD Cancellation and Nonrenewal Endorsement
WC170601I      LA Amendatory Endorsement
WC060601A      CT Nonrenewal and Renewal Endorsement
WC420301I      TX Amendatory Endorsement
WC240604B      MO Amendatory Endorsement
WC310407       NY Merit Rating Revision Endorsement
WC370404C      PA Certified Safety Committee Endorsement
WC040301D      CA - Policy Amendatory Endorsement
WC040302D      CA - Partnership Coverage/Exclusion Endorsement
WC040303C      CA - Officers & Directors Coverage/Exclusion
WC040318C      CA - LLC Exclusion Endorsement
WC040338B      CA - Designated Employee(s) Exclusion Endorsement
WC040339B      CA - Designated Operation(s) Coverage Endorsement
WC040340B      CA - Designated Location(s) Coverage Endorsement
WC040341B      CA - Designated Location(s) Exclusion Endorsement
WC040342B      CA - Designated Operation(s) Exclusion Endorsement
WC040343B      CA - Designated Ops at Designated Loc Exculusion
WC040344B      CA - Designated Ops at Designated Loc Coverage
WC040366       CA - Professional Corporation Coverage/Exclusion
**             dsc2
WC000301       Alternate Employer Endorsement
WC310317       NY - Labor Contractor Endorsement
WC310318       NY - Labor Contractor Exclusion Endorsement
WC260301A      NE - Endorsement for Policy Covering the PEO
WC260302A      NE - Endorsement for Policy Covering the Client
WC090607       FL Guaranty Association Surcharge Endorsement
WC000406       Premium Discount
WC000406A      Premium Discount
WC290411D      NJ Premium Discount Endorsement - Schedule Y
WC290411E      NJ Premium Discount Endorsement - Schedule Y
WC190402A      MD Alcohol- and Drug-Free Workplace Premium Credit
WC190601G      MD Cancellation and Nonrenewal Endorsement
WC170601I      LA Amendatory Endorsement
WC060601A      CT Nonrenewal and Renewal Endorsement
WC420301I      TX Amendatory Endorsement
WC240604B      MO Amendatory Endorsement
WC310407       NY Merit Rating Revision Endorsement
WC370404C      PA Certified Safety Committee Endorsement
WC040301D      CA - Policy Amendatory Endorsement
WC040302D      CA - Partnership Coverage/Exclusion Endorsement
WC040303C      CA - Officers & Directors Coverage/Exclusion
WC040318C      CA - LLC Exclusion Endorsement
WC040338B      CA - Designated Employee(s) Exclusion Endorsement
WC040339B      CA - Designated Operation(s) Coverage Endorsement
WC040340B      CA - Designated Location(s) Coverage Endorsement
WC040341B      CA - Designated Location(s) Exclusion Endorsement
WC040342B      CA - Designated Operation(s) Exclusion Endorsement
WC040343B      CA - Designated Ops at Designated Loc Exculusion
WC040344B      CA - Designated Ops at Designated Loc Coverage
WC040366       CA - Professional Corporation Coverage/Exclusion
** ES          st apEx
WC000301       TX A
WC000301A      TX E
WC000406       NJ E
WC000406A      NJ E
WC000406A      PA E
