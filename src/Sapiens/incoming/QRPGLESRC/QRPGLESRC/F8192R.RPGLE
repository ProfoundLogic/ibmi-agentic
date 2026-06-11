     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H Bnddir('STBNDDIR')
     H/TITLE F8192R
      //*************************************************************************
      //  Description:  837 Parser into Holding Files - Professional
      //
      //  Created: 10/26/17   Programmer C:180      Project: 114MJB
      //*************************************************************************
      //  Revisions:
      //  04/03/19 C:180  SUPPORT 78417/114MJC
      //                  Donot bypass duplicate claims.
      //                  Add Patient acct suffix field CLMpasfx to allow for
      //                  dup claims.
      //
      //  03/18/19 C:180  SUPPORT 77811/114MJC
      //                  Distinguish between hard and soft errors:
      //                  Hard error -> Abort MC interface
      //
      //  03/14/19 C:180  SUPPORT 77811/114MJC
      //                  Reposition Dup claim logic to prevent infinite
      //                  loop.
      //
      //
     FFA837FP   IF   E             DISK    INFDS(DBFDS837)
     FFAICHP    O    E             DISK
     FFAFGHP    O    E             DISK
     FFAHDRP    O    E             DISK
     FFACLMP    O    E             DISK
     FFALINP    O    E             DISK
     FFATSTP    O    E             DISK
     FFAFGTP    O    E             DISK
     FFAICTP    O    E             DISK
     FFACLML5   IF   E           K DISK    Rename(PFACLM:PFACLM5) Prefix(x)
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
      *
      *=========================================================================
      *  Entry
      *=========================================================================
     D F8192R          PR
     D                              100A
     D                                9A
     D                                1A
     D F8192R          PI
     D  @@UniqueId                  100A
     D  @@InCtl#                      9A
     D  @@RetCode                     1A
      *=========================================================================
      *  Prototypes
      *=========================================================================
     D GetFldCnt       PR             5P 0
     D                              500A   Varying Const
     D                                1A   Const

     D GetSegment      PR             3
     D                              500A

     D GetDlmIndex     PR             5  0
     D                              500A   Varying Const
     D                                5  0 Const
     D                                1A   Const

     D ParseISA        PR
     D                              500A
     D                                5  0
     D                                5  0
     D                                5  0

     D ParseGS         PR
     D                              500A
     D                                5  0
     D                                5  0
     D                                5  0

     D ParseST         PR
     D                              500A
     D                                5  0
     D                                5  0
     D                                5  0

     D ParseBHT        PR
     D                              500A
     D                                5  0
     D                                5  0
     D                                5  0

     D ParseNM1        PR
     D                              500A
     D                                5  0
     D                                5  0
     D                                5  0
     D                                3A

     D ParsePER        PR
     D                              500A
     D                                5  0
     D                                5  0
     D                                5  0
     D                                6A

     D ParsePRV        PR
     D                              500A
     D                                5  0
     D                                5  0
     D                                5  0
     D                                6A

     D ParseN3         PR
     D                              500A
     D                                5  0
     D                                5  0
     D                                5  0
     D                                6A

     D ParseN4         PR
     D                              500A
     D                                5  0
     D                                5  0
     D                                5  0
     D                                6A

     D ParseREF        PR
     D                              500A
     D                                5  0
     D                                5  0
     D                                5  0
     D                                6A
     D                                3A

     D ParseHL         PR
     D                              500A
     D                                5  0
     D                                5  0
     D                                5  0
     D                                6A

     D ParseSBR        PR
     D                              500A
     D                                5  0
     D                                5  0
     D                                5  0

     D ParseDMG        PR
     D                              500A
     D                                5  0
     D                                5  0
     D                                5  0

     D ParseCLM        PR
     D                              500A
     D                                5  0
     D                                5  0
     D                                5  0

     D ParseDTP        PR
     D                              500A
     D                                5  0
     D                                5  0
     D                                5  0
     D                                3A

     D ParseCN1        PR
     D                              500A
     D                                5  0
     D                                5  0
     D                                5  0

     D ParseHI         PR
     D                              500A
     D                                5  0
     D                                5  0
     D                                5  0

     D ParseSV1        PR
     D                              500A
     D                                5  0
     D                                5  0
     D                                5  0

     D ParseLIN        PR
     D                              500A
     D                                5  0
     D                                5  0
     D                                5  0

     D ParseLX         PR
     D                              500A
     D                                5  0
     D                                5  0
     D                                5  0

     D ParseCTP        PR
     D                              500A
     D                                5  0
     D                                5  0
     D                                5  0

     D ParseSE         PR
     D                              500A
     D                                5  0
     D                                5  0
     D                                5  0

     D ParseGE         PR
     D                              500A
     D                                5  0
     D                                5  0
     D                                5  0

     D ParseIEA        PR
     D                              500A
     D                                5  0
     D                                5  0
     D                                5  0

     D GetPaSfx        PR             3  0
     D                               38A   Const
     D                               15A   Const
      *================================================================
      *  Data Structures
      *================================================================

      *
      *  Date/time data structure
      *
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)

     DdtCYMD           PR             7  0
     D numDate8                       8  0 Const

      *
      * File information
      *
     D DBFDS837        DS
     D  DBRRN837             397    400B 0

      *
      * Diagnosis code, type
      *
     D  DGQDS          DS
     D   CLMdg1q
     D   CLMdg2q
     D   CLMdg3q
     D   CLMdg4q
     D   CLMdg5q
     D   CLMdg6q
     D   CLMdg7q
     D   CLMdg8q
     D   CLMdg9q
     D   CLMdg10q
     D   CLMdg11q
     D   CLMdg12q
     D   DGQ                          3    OVERLAY(DGQDS) DIM(12)

     D  DGCDS          DS
     D   CLMdg1
     D   CLMdg2
     D   CLMdg3
     D   CLMdg4
     D   CLMdg5
     D   CLMdg6
     D   CLMdg7
     D   CLMdg8
     D   CLMdg9
     D   CLMdg10
     D   CLMdg11
     D   CLMdg12
     D   DGC                         30    OVERLAY(DGCDS) DIM(12)

     D  CCQDS          DS
     D   CLMccq1
     D   CLMccq2
     D   CLMccq3
     D   CLMccq4
     D   CLMccq5
     D   CLMccq6
     D   CLMccq7
     D   CLMccq8
     D   CLMccq9
     D   CLMccq10
     D   CLMccq11
     D   CLMccq12
     D   CCQ                          3    OVERLAY(CCQDS) DIM(12)

     D  CCCDS          DS
     D   CLMcc1
     D   CLMcc2
     D   CLMcc3
     D   CLMcc4
     D   CLMcc5
     D   CLMcc6
     D   CLMcc7
     D   CLMcc8
     D   CLMcc9
     D   CLMcc10
     D   CLMcc11
     D   CLMcc12
     D   CCC                         30    OVERLAY(CCCDS) DIM(12)
      *================================================================
      *  Stand alone
      *================================================================
     D strUniqueId     S            100A   inz
     D rrnOne          S              9P 0 inz(1)
     D strSeg          S              3A   inz
     D Entity          S              3A   inz
     D Qualifier       S              3A   inz
     D Loop            S              6A   inz
     D Ix              S              5  0 inz
     D Ij              S              5  0 inz
     D Idx             S              5  0 inz
     D i               S              5  0 INZ
     D FldCnt          S              5  0 inz
     D FldOcur         S              5  0 inz
     D FrmPos          S              5  0 inz
     D ToPos           S              5  0 inz
     D Len             S              5  0 inz
     D BPrefCNT        S              5  0 inz
     D PatRefQu        S              3A   inz
     D SBRtype         S              1A   inz
     D RefIdQu         S              3A   inz
     D HLnbr           S              5  0 inz
     D LXnew           S              5  0 inz
     D SEcnt           S              5  0 inz
     D GEcnt           S              5  0 inz
     D Delimiter       S              1A   inz
     D Time6           S               T   INZ
     D ##RetCode       S              1A   INZ('0')

     D MsgAry          S             60    DIM(999) inz
     D RrnAry          S              9  0 DIM(999) inz
     D DtaAry          S             25    DIM(999) inz
      **** Begin Add *** 03/18/19 *************************************
     D ErrTyp          S              1    DIM(999) inz
      **** End   Add *** 03/18/19 *************************************

      *================================================================
      *  Indicators
      *================================================================
     D is2300          S               N   inz(*off)
     D is2400          S               N   inz(*off)
     D isDupClaim      S               N   inz(*off)

      *=========================================================================
      *  Constants
      *=========================================================================
     D ErrMsg1         C                   CONST('Duplicate Claim. Claim not -
     D                                     processed.')

      *================================================================
      *  Main
      *================================================================
      /Free
       strUniqueId = @@UniqueId;

       Setll rrnOne FA837FP;
       Read FA837FP;
       Dow not %eof(FA837FP);

          //Get Segment Id
          StrSeg = GetSegment(FA837DATA);

          //Get Field Count
          FldCnt = GetFldCnt(FA837DATA:'*');

          //Process Fields
          If FldCnt > *Zero;
             Exsr Process;
          Endif;

          //Temporary code --- Begins ---|
          //   If *in76;
          //      Leave;
          //   Endif;
          //Temporary code --- Ends   ---|

          Read FA837FP;
       Enddo;


       //Print Error report
       If idx > 0;
          //*** Begin Delete 03/18/19 *****************************************
          //##RetCode =  '1';
          //*** End   Delete 03/18/19 *****************************************

          For i = 1 to Idx;
             If *InOf= *On;
                 Except Hdg;
             Endif;
             If RrnAry(i) <> *zero;
                *in74 = *on;
             Else;
                *in74 = *off;
             Endif;

          //*** Begin Add *** 03/18/19 ****************************************
             //Abort MC process if Hard error.
             If ErrTyp(i) = 'H';
                ##RetCode =  '1';
             Endif;
          //*** End   Add *** 03/18/19 ****************************************

             Except DET;
          EndFor;

       Else;
          Except NoErr;
          //*** Begin Delete *** 03/18/19 *************************************
          //@@InCtl# = ICHinCtl#;
          //*** End   Delete *** 03/18/19 *************************************
       Endif;

      //*** Begin Add *** 03/18/19 ****************************************
       @@InCtl# = ICHinCtl#;
      //*** End   Add *** 03/18/19 ****************************************
       @@RetCode = ##RetCode;
       *inlr = *on;

       //=====================================================================
       // Process Fields
       //=====================================================================
       Begsr Process;

       ix = 0;
       FldOcur = 0;
       Delimiter = '*';

       Dou ix  = 0 or FldOcur = FldCnt;

          //Initialize index
          If ix > *zero;
             ij = ix;
          Else;
             ij = 1;
          Endif;

          ix = GetDlmIndex(FA837DATA:ij:Delimiter);

          If ix > 1;

             //Get From and To positions
             If %Subst(FA837DATA:ix+1:1) <> Delimiter;
                FrmPos = ix + 1;
                ij = ix + 1;
                ix = GetDlmIndex(FA837data:ij:Delimiter);

                If ix > *zero;
                   ToPos = ix -1;
                Else;
                   ToPos = %checkr(' ':FA837data);
                Endif;

                Len = ToPos - FrmPos + 1;

                //Check for subscript error
                If ToPos < FrmPos or ToPos <= *Zero or FrmPos <= *Zero;
                   *in60 = *on;
                   Leave;
                Endif;

             Else;
                ix = ix + 1;
                Len = *zeros;
                FldOcur = FldOcur + 1;
                //Check for subscript error
                If FldOcur = 1;
                   *in60 = *on;
                   Leave;
                Endif;
                Iter;
             Endif;


             FldOcur = FldOcur + 1;

             //Parse data based on Segment
             Select;
             When strSeg = 'ISA';
                ParseISA(FA837DATA:FrmPos:Len:FldOcur);

             When strSeg = 'GS';
                ParseGS(FA837DATA:FrmPos:Len:FldOcur);

             When strSeg = 'ST';
                ParseST(FA837DATA:FrmPos:Len:FldOcur);

             When strSeg = 'BHT';
                ParseBHT(FA837DATA:FrmPos:Len:FldOcur);

             When strSeg = 'NM1';
                If FldOcur = 1;
                   Entity =  %Subst(FA837DATA:FrmPos:Len);
                Endif;
                ParseNM1(FA837DATA:FrmPos:Len:FldOcur:Entity);

             When strSeg = 'PER';
                ParsePER(FA837DATA:FrmPos:Len:FldOcur:Loop);

             When strSeg = 'HL';     //Skip segment
                If FldOcur = 1;
                   HLnbr  = HLnbr + 1;
                Endif;

                ParseHL(FA837DATA:FrmPos:Len:FldOcur:Loop);
                Leave;

             When strSeg = 'PRV';
                ParsePRV(FA837DATA:FrmPos:Len:FldOcur:Loop);

             When strSeg = 'CUR';
                Leave;               //Skip segment

             When strSeg = 'N3';
                ParseN3(FA837DATA:FrmPos:Len:FldOcur:Loop);

             When strSeg = 'N4';
                ParseN4(FA837DATA:FrmPos:Len:FldOcur:Loop);

             When strSeg = 'REF';
                If FldOcur = 1;
                   Qualifier =  %Subst(FA837DATA:FrmPos:Len);
                Endif;
                ParseREF(FA837DATA:FrmPos:Len:FldOcur:Loop:Qualifier);

             When strSeg = 'SBR';
                If FldOcur = 1;
                   SBRtype =  %Subst(FA837DATA:FrmPos:Len);
                Endif;

                If SBRtype <> 'P';
                   Leave;
                Endif;

                If LXnew > *zeros;
                   Exsr WriteSvcLin;
                Endif;

                If FldOcur = 1;
                   Loop = '2000B';
                   Exsr ResetClmInfo;
                Endif;
                ParseSBR(FA837DATA:FrmPos:Len:FldOcur);

             When strSeg = 'PAT';
                Leave;              //Skip Segment

             When strSEG = 'DMG';
                ParseDMG(FA837DATA:FrmPos:Len:FldOcur);

             When strSeg = 'CLM';
                ParseCLM(FA837DATA:FrmPos:Len:FldOcur);

                // Begin Delete 04/03/19 **************************************
                //If isDupClaim = *On;
                //   Exsr BypassClaim;
                //
                //   //*** Begin Add *** 03/14/19 *******************************
                //   If %EOF(FA837FP);
                //      Leavesr;
                //   Else;
                //      //Get Field Count
                //      FldCnt = GetFldCnt(FA837DATA:'*');
                //      If FldCnt > *Zero;
                //         ix = 0;
                //         FldOcur = 0;
                //         Delimiter = '*';
                //         Iter;
                //      Else;
                //         Leavesr;
                //      Endif;
                //   Endif;
                //   //*** End   Add *** 03/14/19 *******************************
                //Endif;
                // End   Delete 04/03/19 **************************************

             When strSeg = 'DTP';
                If FldOcur = 1;
                   Qualifier =  %Subst(FA837DATA:FrmPos:Len);
                Endif;
                ParseDTP(FA837DATA:FrmPos:Len:FldOcur:Qualifier);

             When strSeg = 'CN1';
                ParseCN1(FA837DATA:FrmPos:Len:FldOcur);

             When strSeg = 'HI';
                ParseHI(FA837DATA:FrmPos:Len:FldOcur);

             When strSeg = 'SV1';
                ParseSV1(FA837DATA:FrmPos:Len:FldOcur);

             When strSeg = 'LX';
                If LXnew = *Zeros;
                  CLMuldt  = %dec(%date():*cymd);
                  CLMultm  = %int(%char(%time():*ISO0));
                   Write PFACLM;
                Else;
                   Exsr WriteSvcLin;
                Endif;
                Clear PFALIN;
                Reset RefIdQu;
                ParseLX(FA837DATA:FrmPos:Len:FldOcur);
                LXnew = LXnew + 1;

             When strSeg = 'LIN';
                ParseLIN(FA837DATA:FrmPos:Len:FldOcur);

             When strSeg = 'CTP';
                ParseCTP(FA837DATA:FrmPos:Len:FldOcur);

             When strSeg = 'SE';
                If FldOcur = 1;
                   If LXnew > *zeros;
                      Exsr WriteSvcLin;
                   Endif;
                Endif;
                ParseSE(FA837DATA:FrmPos:Len:FldOcur);

                If FldOcur = 2;
                   Exsr ResetHeader;
                   Exsr ResetClmInfo;
                   SEcnt = SEcnt + 1;
                Endif;

             When strSeg = 'GE';
                ParseGE(FA837DATA:FrmPos:Len:FldOcur);
                If FldOcur = 2;
                   Exsr ResetFgroup;
                   GEcnt = GEcnt + 1;
                Endif;

             When strSeg = 'IEA';
                ParseIEA(FA837DATA:FrmPos:Len:FldOcur);

             Other;
               Leave;
             Endsl;

          Endif;
       Enddo;
       Endsr;
       //=====================================================================
       // Reset Functional Group
       //=====================================================================
       Begsr ResetFgroup;
       Clear PFAFGH;
       Endsr;
       //=====================================================================
       // Reset Header Information
       //=====================================================================
       Begsr ResetHeader;
       Clear PFAHDR;
       Reset Hlnbr;
       Endsr;
       //=====================================================================
       // Reset Claim Information
       //=====================================================================
       Begsr ResetClmInfo;
       Clear PFACLM;
       Clear PFALIN;
       Reset LXnew;
       Reset Loop;
       Reset Entity;
       Reset Qualifier;
       Reset BPrefCNT;
       Reset PatRefQu;
       Reset isDupClaim;
       Endsr;
       //=====================================================================
       // Write Service Line
       //=====================================================================
       Begsr WriteSvcLin;
       LinUnId = StrUniqueID;
       LinIcntl = ICHinctl#;
       Write PFALIN;
       Endsr;
       //=====================================================================
       // Bypass Claim
       //=====================================================================
       Begsr BypassClaim;
       Dou strSeg = 'HL' or strSeg = 'SE' or %eof(FA837FP);
         //Get Segment Id
          Read FA837FP;
          StrSeg = GetSegment(FA837DATA);
       Enddo;

       //** Begin Delete *** 03/14/19 ****************************************
       // //Get Field Count
       // FldCnt = GetFldCnt(FA837DATA:'*');
       //
       // //Process Fields
       // If FldCnt > *Zero;
       //    Exsr Process;
       // Endif;
       //** End   Delete *** 03/14/19 ****************************************

       Endsr;
       //=========================================================================
       //  Initialization
       //=========================================================================
       Begsr *INZsr;
        Time6 = %time();
        Except HDG;
       Endsr;
      /End-Free
     OQSYSPRT   E            HDG              02
     O                       UDATE         Y
     O                       Time6             +001
     O                                           82 '837 Bill Import Processing'
     O                                         +001 'Errors'
     O                                          124 'Page:'
     O                       PAGE          2   +001
      *
     O          E            HDG         3
     O                                            7 'Segment'
     O                                           19 'Error Code'
     O                                           38 'Error Description'
     O                                           97 'RRN'
     O                                          105 'Data'
      *
     O          E            DET         1
     O                                            3 'CLM'
     O                                           12 'XXX'
     O                       MsgAry(i)           81
     O               74      RrnAry(i)     2     97
     O               74      DtaAry(i)          126
      *
     O          E            NOERR       3
     O                                           74 '*** NO PROCESSING ERRORS '
     O                                         +000 'FOUND ***'
      *=====================================================================
      * Get Field Count
      *=====================================================================
     P GetFldCnt       B
     D GetFldCnt       PI             5P 0
     D  FldData                     500A   Varying Const
     D  Delimit                       1A   Const

     D  Fieldcnt       S              5P 0 inz
     D  Ix             S              5  0 inz
     D  Ij             S              5  0 inz

      /Free
       //Count fields
       Dou ix = 0;
          If ix > *zero;
             ij = ix + 1;
          Else;
             ij = 1;
          Endif;

          ix = GetDlmIndex(FldData:ij:Delimit);

          If ix > *zero;
             FieldCnt = FieldCnt + 1;
          Endif;
       Enddo;

       If Delimit = ':' and FieldCnt > *Zero;
          FieldCnt = FieldCnt + 1;
       Endif;

       Return FieldCnt;
      /End-Free
     P GetFldCnt       E

      *=====================================================================
      * Get Segment
      *=====================================================================
     P GetSegment      B
     D GetSegment      PI             3A
     D  segData                     500A

     D  strSeg         S              3A

      /Free
       strSeg = %Subst(segData:1:3);

       If %Subst(strSeg:3:1) = '*';
          strSeg = %Subst(strSeg:1:2) ;
       Endif;

       Return strSeg;
      /End-Free
     P GetSegment      E

      *=====================================================================
      * Get Patient Account Suffix
      *=====================================================================
     P GetPaSfx        B
     D GetPaSfx        PI             3  0
     D  @@Pacct                      38A   Const
     D  @@InsId                      15A   Const

     D  intSfx         S              3  0

      /Free
        intSfx = *zeros;
        setll (@@Pacct:@@Insid) FACLML5;
        If %equal(FACLML5);
           Setgt (@@Pacct:@@Insid) FACLML5;
           Readpe (@@Pacct:@@Insid) FACLML5;
           If not %eof(FACLML5);
              intSfx  = xCLMpasfx + 1;
           Endif;
        Endif;
       Return intSfx;
      /End-Free
     P GetPaSfx        E

      *=====================================================================
      * Get Delimiter Index
      *=====================================================================
     P GetDlmIndex     B
     D GetDlmIndex     PI             5  0
     D  DlmData                     500A   Varying Const
     D  SegIdx                        5  0 Const
     D  Delimit                       1A   Const

      /Free
       If Delimit = '*';
          Return %scan('*':DlmData:SegIdx);
       Endif;

       If Delimit = ':';
          Return %scan(':':DlmData:SegIdx);
       Endif;
      /End-Free
     P GetDlmIndex     E

      *=====================================================================
      * Parse ISA segment
      *=====================================================================
     P ParseISA        B
     D ParseISA        PI
     D  segData                     500A
     D  FrmPos                        5  0
     D  FldLen                        5  0
     D  FldOcur                       5  0
      /Free

       Select;
       When FldOcur = 1;
          Clear pfaich;
          ICHunid = strUniqueId;
          ICHauqu = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          ICHauin = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          ICHsiqu = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          ICHsi = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 5;
          ICHinqu = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 6;
          ICHinsid = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 7;
          ICHinidqu = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 8;
          ICHinrid = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 9;
          ICHindt = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 10;
          ICHintm = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 11;
          ICHrepsep = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 12;
          ICHinctver = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 13;
          ICHinctl# = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 14;
          ICHackreq = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 15;
          ICHuseind = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 16;
          ICHcmelsep = %Subst(SegData:FrmPos:FldLen);
       Endsl;

       If FldOcur = FldCnt;
          Write PFAICH;
       Endif;

      /End-Free
     P ParseISA        E
      *=====================================================================
      * Parse GS segment
      *=====================================================================
     P ParseGS         B
     D ParseGS         PI
     D  segData                     500A
     D  FrmPos                        5  0
     D  FldLen                        5  0
     D  FldOcur                       5  0
      /Free
       Select;
       When FldOcur = 1;
          Clear pfafgh;
          FGHunid = strUniqueId;
          FGHicntl = ICHinctl#;
          FGHidcd = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          FGHapsncd = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          FGHaprccd = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          FGHdt = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 5;
          FGHtm = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 6;
          FGHcntl = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 7;
          FGHragcd = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 8;
          FGHveridcd = %Subst(SegData:FrmPos:FldLen);
       Endsl;

       If FldOcur = FldCnt;
          Write PFAFGH;
       Endif;

      /End-Free
     P ParseGS         E
      *=====================================================================
      * Parse ST segment
      *=====================================================================
     P ParseST         B
     D ParseST         PI
     D  segData                     500A
     D  FrmPos                        5  0
     D  FldLen                        5  0
     D  FldOcur                       5  0
      /Free
       Select;
       When FldOcur = 1;
          Clear PFAHDR;
          HDbunid = strUniqueId;
          HDicntl = ICHinctl#;
          HDtshGCTL = FGHcntl;
          HDtshIDCD = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          HDtshCNTL = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          HDtshICR = %Subst(SegData:FrmPos:FldLen);
       Endsl;
      /End-Free
     P ParseST         E

      *=====================================================================
      * Parse BHT segment
      *=====================================================================
     P ParseBHT        B
     D ParseBHT        PI
     D  segData                     500A
     D  FrmPos                        5  0
     D  FldLen                        5  0
     D  FldOcur                       5  0
      /Free
       Select;
       When FldOcur = 1;
          HDbhtHSC = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          HDbhtTSPC = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          HDbhtOATI = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          HDbhtCRDT = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 5;
          HDbhtCRTM = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 6;
          HDbhtID = %Subst(SegData:FrmPos:FldLen);
       Endsl;
      /End-Free
     P ParseBHT        E

      *=====================================================================
      * Parse NM1
      *=====================================================================
     P ParseNM1        B
     D ParseNM1        PI
     D  segData                     500A
     D  FrmPos                        5  0
     D  FldLen                        5  0
     D  FldOcur                       5  0
     D  NM1entity                     3A

      /Free
       //Process NM1 loops
       Select;
       When NM1entity = '41';  //1000A Submitter Name
          Loop = '1000A';
          Exsr NM11000A;

       When NM1entity = '40';  //1000B Receiver Name
          Loop = '1000B';
          Exsr NM11000B;

       When NM1entity = '85' and is2300 = *off;  //2010AA Billing Provider Name
          If SBRtype = 'P' or SBRtype = *blanks;
             Loop = '2010AA';
             Exsr NM12010AA;
          Endif;

       When NM1entity = '87';  //2010AB Pay-to Address Name
          Loop = '2010AB';
          Exsr NM12010AB;

       When NM1entity = 'IL' and is2300 = *off; //2010BA Subscriber Name
          If SBRtype = 'P' or SBRtype = *blanks;
             Loop = '2010BA';
             Exsr NM12010BA;
          Endif;

       When NM1entity = 'PR' and is2300 = *Off; //2010BB Payer Name
          If SBRtype = 'P' or SBRtype = *blanks;
             Loop = '2010BB';
             Exsr NM12010BB;
          Endif;

       When NM1entity = 'QC';  //2010CA Patient Name
          Loop = '2010CA';
          Exsr NM12010CA;

       When NM1entity = 'DN' or NM1entity = 'P3';//2310A Referring Provider name
          If is2300 = *on;
             If SBRtype = 'P' or SBRtype = *blanks;
                Loop = '2310A';
                Exsr NM12310A;
             Endif;
          Endif;

          If is2400 = *on;
             Exsr NM12420F;
          Endif;

       When NM1entity = '82';  //2310B Rendering Provider name
          If is2300 = *on;
             If SBRtype = 'P' or SBRtype = *blanks;
                Loop = '2310B';
                Exsr NM12310B;
             Endif;
          Endif;

          If is2400 = *on;
             Loop = '2420A';
             Exsr NM12420A;
          Endif;

       When NM1entity = '77';  //2310C Service Facility Location Name
          If is2300 = *on;
             If SBRtype = 'P' or SBRtype = *blanks;
                Loop = '2310C';      //NOT TESTED
                Exsr NM12310C;
             Endif;
          Endif;

          If is2400 = *on;
             Loop = '2420C';      //NOT TESTED
             Exsr NM12420C;
          Endif;

       When NM1entity = 'DQ';  //2310D Supervising Provider Name
          If is2300 = *on;
             If SBRtype = 'P' or SBRtype = *blanks;
                Loop = '2310D';      //NOT TESTED
                Exsr NM12310D;
             Endif;
          Endif;

       When NM1entity = 'DK';  //2420E Ordering Provider Name
          If is2400 = *on;
             Loop = '2420E';      //NOT TESTED
             Exsr NM12420E;
          Endif;
       Endsl;


       //--------------------------------------------------------------
       // NM1 Loop 1000A;
       //--------------------------------------------------------------
       Begsr NM11000A;
       Select;
       When FldOcur = 1;
          HDsbIDCD = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          HDsbENQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          HDsbNAME = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          HDsbFSN = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 5;
          HDsbMID = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 6; //Not Used
       When FldOcur = 7; //Not Used
       When FldOcur = 8;
          HDsbIDQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 9;
          HDsbID = %Subst(SegData:FrmPos:FldLen);
       Endsl;
       Endsr;
       //--------------------------------------------------------------
       // NM1 Loop 1000B;
       //--------------------------------------------------------------
       Begsr NM11000B;
       Select;
       When FldOcur = 1;
          HDrcvEIC = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          HDrcvETQ = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          HDrcvNM = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4; //Not used
       When FldOcur = 5; //Not used
       When FldOcur = 6; //Not Used
       When FldOcur = 7; //Not Used
       When FldOcur = 8;
          HDrcvIDQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 9;
          HDrcvPID = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 10;  //Not used
       When FldOcur = 11;  //Not used
       When FldOcur = 12;  //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // NM1 Loop 2010AA;
       //--------------------------------------------------------------
       Begsr NM12010AA;
       Select;
       When FldOcur = 1;
          HDbpENIDCD = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          HDbpENQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          HDbpNM = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          HDbpFNM = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 5;
          HDbpMNM = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 6; //Not Used
       When FldOcur = 7;
          HDbpSUFF  = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 8;
          HDbpNMQU  = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 9;
          HDbpNMID = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 10;  //Not used
       When FldOcur = 11;  //Not used
       When FldOcur = 12;  //Not used
       Endsl;

       Endsr;

       //--------------------------------------------------------------
       // NM1 Loop 2010AB;
       //--------------------------------------------------------------
       Begsr NM12010AB;
       Select;
       When FldOcur = 1;
          HDpprNMID = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          HDpprNMQU = %Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;

       Endsr;

       //--------------------------------------------------------------
       // NM1 Loop 2010BA; Subscriber Name
       //--------------------------------------------------------------
       Begsr NM12010BA;
       Select;
       When FldOcur = 1;
          CLMsubNAM = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMsubETQ = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMsubLSN =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          CLMsubFSN  = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 5;
          CLMsubMID   = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 6;
          // Not used
       When FldOcur = 7;
          CLMsubSNM  = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 8;
          CLMsubICQ  = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 9;
          CLMsubSPI  = %Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // NM1 Loop 2010BB;
       //--------------------------------------------------------------
       Begsr NM12010BB;
       Select;
       When FldOcur = 1;
          CLMpayENCD = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMpayENQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMpayPNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          // Not used
       When FldOcur = 5;
          // Not used
       When FldOcur = 6;
          // Not used
       When FldOcur = 7;
          // Not used
       When FldOcur = 8;
          CLMpayICQ  = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 9;
          CLMpayPI  = %Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // NM1 Loop 2010CA;
       //--------------------------------------------------------------
       Begsr NM12010CA;
       Select;
       When FldOcur = 1;
          CLMpatENCD = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMpatENQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMpatLSN =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          CLMpatFSN =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 5;
          CLMpatMNM =%Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // NM1 Loop 2310A;
       //--------------------------------------------------------------
       Begsr NM12310A;
       Select;
       When FldOcur = 1;
          CLMrfpIDCD = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMrfpENQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMrfpNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          CLMrfpFNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 5;
          CLMrfpMNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 6;  //Not used
       When FldOcur = 7;  //Not used
       When FldOcur = 8;
          CLMrfpNMQU =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 9;
          CLMrfpNMID =%Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // NM1 Loop 2310B  - Rendering Provider Name
       //--------------------------------------------------------------
       Begsr NM12310B;

       Select;
       When FldOcur = 1;
          CLMrnpIDCD = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMrnpENQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMrnpNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          CLMrnpFNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 5;
          CLMrnpMNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 6;  //Not used
          CLMrnpPNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 7;  //Not used
          CLMrnpSNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 8;
          CLMrnpIDQU =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 9;
          CLMrnpNMID =%Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;
       Endsr;

       //--------------------------------------------------------------
       // NM1 Loop 2420A - Rendering Provider Name
       //--------------------------------------------------------------
       Begsr NM12420A;
       Select;
       When FldOcur = 1;
          LINrnENCD = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          LINrnENQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          LINrnNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          LINrnFNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 5;
          LINrnMNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 6;
          LINrnPNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 7;
          LINrnSNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 8;
          LINrnIDQU =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 9;
          LINrnNMID =%Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // NM1 Loop 2310C - Service Facility Location - NOT TESTED
       //--------------------------------------------------------------
       Begsr NM12310C;
       Select;
       When FldOcur = 1;
          CLMsfNMID = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMsfNMQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMsfNM = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          // Not used
       When FldOcur = 5;
          // Not used
       When FldOcur = 6;
          // Not used
       When FldOcur = 7;
          // Not used
       When FldOcur = 8;
          CLMsfIDQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 9;
          CLMsfID  = %Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // NM1 Loop 2310D - Supervising Provider - NOT TESTED
       //--------------------------------------------------------------
       Begsr NM12310D;
       Select;
       When FldOcur = 1;
          CLMspNMID = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMspNMQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMspNM = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          CLMspFSN = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 5;
          CLMspMNM = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 6;
          // Not used
       When FldOcur = 7;
          // Not used
       When FldOcur = 8;
          CLMspIDQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 9;
          CLMspID  = %Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // NM1 Loop 2420C - Service Facility Location - NOT TESTED
       //--------------------------------------------------------------
       Begsr NM12420C;
       Select;
       When FldOcur = 1;
          LINsfNMID = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          LINsfNMQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          LINsfPRNM = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          // Not used
       When FldOcur = 5;
          // Not used
       When FldOcur = 6;
          // Not used
       When FldOcur = 7;
          // Not used
       When FldOcur = 8;
          LINsfIDQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 9;
          LINsfOTHID  = %Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // NM1 Loop 2420E - Ordering Provider Name - NOT TESTED
       //--------------------------------------------------------------
       Begsr NM12420E;
       Select;
       When FldOcur = 1;
          LINorENICD = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          LINorENQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          LINorNM = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          LINorFNM = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 5;
          LINorMNM = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 6;
          // Not used
       When FldOcur = 7;
          // Not used
       When FldOcur = 8;
          // Not used
       When FldOcur = 9;
          // Not used
       Other;
          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // NM1 Loop 2420F   -  NOT TESTED
       //--------------------------------------------------------------
       Begsr NM12420F;
       Select;
       When FldOcur = 1;
          LINrpENICD = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          LINrpENQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          LINrpNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          LINrpFNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 5;
          LINrpMNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 6;  //Not used
       When FldOcur = 7;  //Not used
       When FldOcur = 8;
          LINrpIDQU =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 9;
          LINrpOTHID =%Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;

       Endsr;
      /End-Free
     P ParseNM1        E

      *=====================================================================
      * Parse PER
      *=====================================================================
     P ParsePER        B
     D ParsePER        PI
     D  segData                     500A
     D  FrmPos                        5  0
     D  FldLen                        5  0
     D  FldOcur                       5  0
     D  Loop                          6A

      /Free
       //Process PER loops
       Select;
       When Loop = '1000A';   //1000A Submitter EDI Contact Info
          Exsr PER1000A;
       When Loop = '2010AA';  //2010AA Billing Provider Contact info
          Exsr PER2010AA;
       When Loop = '2010BA';  //2010BA P&C Subscriber Contact Info
          ix = 0;             //Skip
       When Loop = '2010CA';  //2010CA P&C Patient Contact Info
          ix = 0;             //Skip
       When Loop = '2310C';   //2310C Service Facility Contact Info
          Exsr PER2310C;
       Endsl;

       //--------------------------------------------------------------
       // PER Loop 1000A - Submitter Name
       //--------------------------------------------------------------
       Begsr PER1000A;

       Select;
       When FldOcur = 1;
          HDsbCFC = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          HDsbCTNM = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          HDsbCN1QU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          HDsbCN1 = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 5;
          HDsbCN2QU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 6;
          HDsbCN2 = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 7;
          HDsbCN3QU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 8;
          HDsbCN3 = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 9;    //Not used.
       Endsl;


       Endsr;
       //--------------------------------------------------------------
       // PER Loop 2010AA;
       //--------------------------------------------------------------
       Begsr PER2010AA;
       // Not used or not mapped in specs
       Endsr;

       //--------------------------------------------------------------
       // PER Loop 2310C - Service Facility Contact Information
       //--------------------------------------------------------------
       Begsr PER2310C;
       // Not used or not mapped in specs
       Endsr;
      /End-Free
     P ParsePER        E

      *=====================================================================
      * Parse PRV
      *=====================================================================
     P ParsePRV        B
     D ParsePRV        PI
     D  segData                     500A
     D  FrmPos                        5  0
     D  FldLen                        5  0
     D  FldOcur                       5  0
     D  Loop                          6A

      /Free
       //Process PRV loops
       Select;
       When Loop = '2000A';   //Situational Billing Provider Hierarchical Level
          Exsr PRV2000A;
       When Loop = '2310B ';  //2310B Rendering Provider
          Exsr PRV2310B;
       When Loop = '2420A';   //2420A Rendering Provider
          Exsr PRV2420A;
       Endsl;


       //--------------------------------------------------------------
       // PRV Loop 2000A - Situational
       //--------------------------------------------------------------
       Begsr PRV2000A;

       Select;
       When FldOcur = 1;
          HDbpCODE = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          HDbpSPQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          HDbpSPID = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;  //Not used
       When FldOcur = 5;  //Not used
       When FldOcur = 6;  //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // PRV Loop 2310B - Situational
       //--------------------------------------------------------------
       Begsr PRV2310B;

       Select;
       When FldOcur = 1;
          CLMrnpCODE = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMrnpSPQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMrnpSPID = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;  //Not used
       When FldOcur = 5;  //Not used
       When FldOcur = 6;  //Not used
       Endsl;

       Endsr;
       //-----------------------------------------------------------------
       // PRV Loop 2420A - Rendering Provider Specialty Info
       //-----------------------------------------------------------------
       Begsr PRV2420A;

       Select;
       When FldOcur = 1;
          LINrnsCODE = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          LINrnsPQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          LINrnsPID = %Subst(SegData:FrmPos:FldLen);

       When FldOcur = 4;

       When FldOcur = 5;  //Not used
       When FldOcur = 6;  //Not used
       Endsl;

       Endsr;
      /End-Free
     P ParsePRV        E

      *=====================================================================
      * Parse N3
      *=====================================================================
     P ParseN3         B
     D ParseN3         PI
     D  segData                     500A
     D  FrmPos                        5  0
     D  FldLen                        5  0
     D  FldOcur                       5  0
     D  Loop                          6A

      /Free
       //Process N3 loops
       Select;
       When Loop = '2010AA';  //Billing Provider Address
          Exsr N32010AA;

       When Loop = '2010AB';  //Pay-to-Address  Address
          Exsr N32010AB;

       When Loop = '2010AC';  //Pay-to-Plan Address
          ix = 0;             //Not used.

       When Loop = '2010BA';  //Subscriber Address
          Exsr N32010BA;

       When Loop = '2010BB';  //Payer Address
          Exsr N32010BB;

       When Loop = '2010CA';  //Patient Address
          Exsr N32010CA;

       When Loop = '2310C';   //Service Facility Location Address
          Exsr N32310C;       //NOT TESTED

       When Loop = '2420C';   //Service Facility Location Address
          Exsr N32420C;       //NOT TESTED

       When Loop = '2420E';   //Ordering Provider Address
          Exsr N32420E;       //NOT TESTED
       Endsl;

       //--------------------------------------------------------------
       // N3 Loop 2010AA - Billing Provider Address
       //--------------------------------------------------------------
       Begsr N32010AA;
       Select;
       When FldOcur = 1;
          HDbpADR1 = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          HDbpADR2 = %Subst(SegData:FrmPos:FldLen);
       Endsl;
       Endsr;

       //--------------------------------------------------------------
       // N3 Loop 2010AB - Pay-to Address Address
       //--------------------------------------------------------------
       Begsr N32010AB;
       Select;
       When FldOcur = 1;
          HDpprADR1 = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          HDpprADR2 = %Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;
       Endsr;

       //--------------------------------------------------------------
       // N3 Loop 2010BA - Subscriber Address
       //--------------------------------------------------------------
       Begsr N32010BA;
       Select;
       When FldOcur = 1;
          CLMsubADR1 = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMsubADR2 = %Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;
       Endsr;

       //--------------------------------------------------------------
       // N3 Loop 2010BB - Payer Address
       //--------------------------------------------------------------
       Begsr N32010BB;
       Select;
       When FldOcur = 1;
          CLMpayADR1 = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMpayADR2 = %Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;
       Endsr;
       //--------------------------------------------------------------
       // N3 Loop 2010CA - Patient Address
       //--------------------------------------------------------------
       Begsr N32010CA;
       Select;
       When FldOcur = 1;
          CLMpatADR1 = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMpatADR2 = %Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;
       Endsr;
       //--------------------------------------------------------------
       // N3 Loop 2310C  - Service Facility Location Address.
       //--------------------------------------------------------------
       Begsr N32310C;         //NOT TESTED
       Select;
       When FldOcur = 1;
          CLMsfADR1 = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMsfADR2 = %Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;
       Endsr;
       //--------------------------------------------------------------
       // N3 Loop 2420C  - Service Facility Location Address.
       //--------------------------------------------------------------
       Begsr N32420C;         //NOT TESTED
       Select;
       When FldOcur = 1;
          LINsfADR1 = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          LINsfADR2 = %Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;
       Endsr;
       //--------------------------------------------------------------
       // N3 Loop 2420E  - Ordering Provider Address.
       //--------------------------------------------------------------
       Begsr N32420E;         //NOT TESTED
       Select;
       When FldOcur = 1;
          LINorADR1 = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          LINorADR2 = %Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;
       Endsr;
      /End-Free
     P ParseN3         E

      *=====================================================================
      * Parse N4
      *=====================================================================
     P ParseN4         B
     D ParseN4         PI
     D  segData                     500A
     D  FrmPos                        5  0
     D  FldLen                        5  0
     D  FldOcur                       5  0
     D  Loop                          6A

      /Free
       //Process N4 loops
       Select;
       When Loop = '2010AA';  //Billing Provider City/State/Zip
          Exsr N42010AA;

       When Loop = '2010AB';  //Pay-to-Address City/State/Zip
          Exsr N42010AB;

       When Loop = '2010AC';  //Pay-to-Plan City/State/Zip
          ix = 0;             //Skip

       When Loop = '2010BA';  //Subscriber City/State/Zip
          Exsr N42010BA;

       When Loop = '2010BB';  //Payer City/State/Zip
          Exsr N42010BB;

       When Loop = '2010CA';  //Patient City/State/Zip
          Exsr N42010CA;

       When Loop = '2310C';   //Service Facility Location City/State/Zip
          Exsr N42310C;       //NOT TESTED

       When Loop = '2420C';   //Service Facility Location City/State/Zip
          Exsr N42420C;       //NOT TESTED

       When Loop = '2420E';   //Ordering Provider City/State/Zip
          Exsr N42420E;       //NOT TESTED
       Endsl;


       //--------------------------------------------------------------
       // N4 Loop 2010AA - Billing Provider City/State/Zip
       //--------------------------------------------------------------
       Begsr N42010AA;
       Select;
       When FldOcur = 1;
          HDbpCTY = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          HDbpST  = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          HDbpPOST = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          HDbpCNTRY = %Subst(SegData:FrmPos:FldLen);
       Other;
       //Not used
       Endsl;
       Endsr;

       //--------------------------------------------------------------
       // N4 Loop 2010AB - Pay-to Address City/State/Zip
       //--------------------------------------------------------------
       Begsr N42010AB;
       Select;
       When FldOcur = 1;
          HDpprCTY = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          HDpprST  = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          HDpprPOST = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          HDpprCNTRY = %Subst(SegData:FrmPos:FldLen);
       Other;
       //Not used
       Endsl;
       Endsr;

       //--------------------------------------------------------------
       // N4 Loop 2010BA - Subscriber Address City/State/Zip
       //--------------------------------------------------------------
       Begsr N42010BA;
       Select;
       When FldOcur = 1;
          CLMsubCTY = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMsubST  = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMsubZIP = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          CLMsubCNRY = %Subst(SegData:FrmPos:FldLen);
       Other;
       //Not used
       Endsl;
       Endsr;

       //--------------------------------------------------------------
       // N4 Loop 2010BB - Payer Address City/State/Zip
       //--------------------------------------------------------------
       Begsr N42010BB;
       Select;
       When FldOcur = 1;
          CLMpayCTY = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMpayPST  = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMpayZIP = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          CLMpayCNRY = %Subst(SegData:FrmPos:FldLen);
       Other;
       //Not used
       Endsl;
       Endsr;
       //--------------------------------------------------------------
       // N4 Loop 2010CA - Patient Address City/State/Zip
       //--------------------------------------------------------------
       Begsr N42010CA;
       Select;
       When FldOcur = 1;
          CLMpatCTY = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMpatST  = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMpatPOST = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          CLMpatCNRY = %Subst(SegData:FrmPos:FldLen);
       Other;
       //Not used
       Endsl;
       Endsr;
       //--------------------------------------------------------------
       // N4 Loop 2310C - Service Facility Location City/State/Zip
       //--------------------------------------------------------------
       Begsr N42310C;
       Select;
       When FldOcur = 1;
          CLMsfCTY = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMsfST  = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMsfPOST = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          CLMsfCNTRY = %Subst(SegData:FrmPos:FldLen);
       Other;
       //Not used
       Endsl;
       Endsr;
       //--------------------------------------------------------------
       // N4 Loop 2420C - Service Facility Location City/State/Zip
       //--------------------------------------------------------------
       Begsr N42420C;
       Select;
       When FldOcur = 1;
          LINsfCTY = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          LINsfST  = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          LINsfPOST = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          LINsfCNTRY = %Subst(SegData:FrmPos:FldLen);
       Other;
       //Not used
       Endsl;
       Endsr;
       //--------------------------------------------------------------
       // N4 Loop 2420E - Ordering Provider City/State/Zip
       //--------------------------------------------------------------
       Begsr N42420E;
       Select;
       When FldOcur = 1;
          LINorCTY = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          LINorST  = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          LINorPOST = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          LINorCNTRY = %Subst(SegData:FrmPos:FldLen);
       Other;
       //Not used
       Endsl;
       Endsr;
      /End-Free
     P ParseN4         E

      *=====================================================================
      * Parse REF
      *=====================================================================
     P ParseREF        B
     D ParseREF        PI
     D  segData                     500A
     D  FrmPos                        5  0
     D  FldLen                        5  0
     D  FldOcur                       5  0
     D  Loop                          6A
     D  RefQu                         3A

     D  Ref4           S            164A   inz
     D  ClnFrm         S              5  0 inz
     D  ClnTo          S              5  0 inz
     D  ClnIdx         S              5  0 inz
     D  Ref4Len        S              5  0 inz
     D  intIdx         S              5  0 inz
     D  ClnMax         S              5  0 inz

      /Free
       //Process REF loops
       Select;
       When Loop = '2010AA';
          If BPrefCnt = *ZERO;
             Exsr REF2010AATAX;
          Else;
             Exsr REF2010AALIC;
          Endif;

       When Loop = '2010AC';  //Pay-to-Plan Address
          ix=0;               //Skip

       When Loop = '2010BA';
          ix = 0;             //Skip
          If BPrefCnt = *ZERO;
             //Not used       //Subscriber Secondary ID
          Else;
             //Not used       //P&C Claim Number
          Endif;

       When Loop = '2010BB';
          ix = 0;             //Skip
          If BPrefCnt = *ZERO;
             //Not used       //Payer Secondary ID
          Else;
             //Not used       //Billing Provider Secondary Identification
          Endif;

       When Loop = '2010CA';
          If FldOcur = 1;
             PatRefQu = %Subst(SegData:FrmPos:FldLen);
          Endif;

          If PatRefQu = 'Y4';
             Exsr REF2010CApc;
          Endif;

          If PatRefQu = '1W' or PatRefQu = 'SY';
             Exsr REF2010CApi;
          Endif;

       When Loop = '2310A';
             Exsr REF2310A;

       When Loop = '2310B';
             Exsr REF2310B;

       When Loop = '2310C';
             Exsr REF2310C;

       When Loop = '2310D';
             Exsr REF2310D;

       When Loop = '2400 ' and RefQu = '6R ';
             Exsr REF2400;

       When Loop = '2410 ';
             Exsr REF2410;

       When Loop = '2420A ';  //Not tested
             Exsr REF2420A;

       When Loop = '2420C ';  //Not tested
             Exsr REF2420C;

       When Loop = '2420E ';  //Not tested
             Exsr REF2420E;
       Endsl;


       //--------------------------------------------------------------
       // REF Loop 2010AA - Billing Provider Tax Identification
       //--------------------------------------------------------------
       Begsr REF2010AATAX;

       Select;
       When FldOcur = 1;
          HDbpTAXQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          HDbpTAXID  = %Subst(SegData:FrmPos:FldLen);    // CHECK HERE
       When FldOcur = 3; //Not used
       When FldOcur = 4; //Not used
       Endsl;

       If FldOcur = FldCnt;
          BPrefCnt = BPrefCnt + 1;
       Endif;
       Endsr;
       //--------------------------------------------------------------
       // REF Loop 2010AA - Billing Provider Lax Identification
       //--------------------------------------------------------------
       Begsr REF2010AALIC;
        //Not used yet
       Endsr;
       //--------------------------------------------------------------
       // REF Loop 2010CA - Patient - Property & Casualty Claim Number
       //--------------------------------------------------------------
       Begsr REF2010CApc;

          Select;
          When FldOcur = 1;
             CLMpatPCQU = %Subst(SegData:FrmPos:FldLen);
          When FldOcur = 2;
             CLMpatCLM#  = %Subst(SegData:FrmPos:FldLen);
          Other;
             //Not used
          Endsl;

       Endsr;
       //--------------------------------------------------------------
       // REF Loop 2010CA - Patient - Property & Casualty Patient Id
       //--------------------------------------------------------------
       Begsr REF2010CApi;

          Select;
          When FldOcur = 1;
             CLMpiRIQ = %Subst(SegData:FrmPos:FldLen);
          When FldOcur = 2;
             CLMpiPCCN  = %Subst(SegData:FrmPos:FldLen);
          Other;
             //Not used
          Endsl;

       Endsr;
       //--------------------------------------------------------------
       // REF Loop 2310A - Referring Provider Secondary Identification
       //--------------------------------------------------------------
       Begsr REF2310A;

       Select;
       When FldOcur = 1;
          CLMrfRUPQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMrfUPID  = %Subst(SegData:FrmPos:FldLen);
       Other;          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // REF Loop 2310B - Rendering Provider Name
       //--------------------------------------------------------------
       Begsr REF2310B;

       Select;
       When FldOcur = 1;
          CLMrnp2PQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMrnp2PID  = %Subst(SegData:FrmPos:FldLen);
       Other;          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // REF Loop 2310C - Service Facility Location Secondary Id
       //--------------------------------------------------------------
       Begsr REF2310C;

       Select;
       When FldOcur = 1;
          CLMsfp2ndQ = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMsf2ndID = %Subst(SegData:FrmPos:FldLen);
       Other;          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // REF Loop 2310D - Supervising Provider Secondary Id
       //--------------------------------------------------------------
       Begsr REF2310D;

       Select;
       When FldOcur = 1;
          CLMsp2ndQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMsp2ndID = %Subst(SegData:FrmPos:FldLen);
       Other;          //Not used
       Endsl;

       Endsr;
       //-----------------------------------------------------------------
       // REF Loop 2400  - Line Item Control Number
       //-----------------------------------------------------------------
       Begsr REF2400;

       Select;
       When FldOcur = 1;
          LINctlRID = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          LINliCTL#  = %Subst(SegData:FrmPos:FldLen);
       Other;          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // REF Loop 2410  - Prescription or Compound Drug Assoc Number
       //--------------------------------------------------------------
       Begsr REF2410;         // NOT TESTED

       Select;
       When FldOcur = 1;
          LINprRIDQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          LINpr#  = %Subst(SegData:FrmPos:FldLen);
       Other;          //Not used
       Endsl;

       Endsr;
       //-----------------------------------------------------------------
       // REF Loop 2420A  - Rendering Provider Secondary ID
       //-----------------------------------------------------------------
       Begsr REF2420A;         // PENDING

       Select;
       When FldOcur = 1;
          LINrnUPQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          LINrnUPID = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          //Not used
       When FldOcur = 4;
         REF4  = %Subst(SegData:FrmPos:FldLen);
         ClnMax = GetFldCnt(REF4:':');
         Clnto = *Zeros;

         If ClnMax > *Zero;
            For intIdx = 1 to ClnMax;
               ClnFrm = ClnTo + 1;
               ClnIdx = ClnTo + 1;
               ClnTo = %scan(':':REF4:ClnIdx);

               If ClnTo = *Zero;
                  ClnTo = FldLen + 1;
               Endif;

               REF4Len = ClnTo - ClnFrm;

               Select;
               When intIdx = 1;
                  LINrnupQU =%Subst(REF4:ClnFrm:REF4Len);
               When intIdx = 2;
                  LINrnupID =%Subst(REF4:ClnFrm:REF4Len);
               Other;      //Not Used.
               Endsl;
            Endfor;
         Else;                                        // No :, 1 field
            ClnFrm = FrmPos;                          // Not tested
            Clnto =  %scan('*':SegData:ClnFrm);
            If Clnto = *zero;
               Clnto =  %scan(' ':SegData:ClnFrm);
            Endif;

            If Clnto > *zero and ClnFrm > *zero and ClnTo > ClnFrm;
               REF4Len = ClnTo - ClnFrm;

               If REF4Len > *zero;
                  LINrnupQU =  %Subst(SegData:ClnFrm:REF4Len);
               Else;
                  LINrnupQU =  *blanks;
               Endif;

            Endif;

           LINrnupID = *blanks;
         Endif;

         //subfield - For/net loop needed.
       Other;          //Not used
       Endsl;

       Endsr;
       //-----------------------------------------------------------------
       // REF Loop 2420C  - Service Facility Location Secondary Id
       //-----------------------------------------------------------------
       Begsr REF2420C;         // NOT TESTED

       Select;
       When FldOcur = 1;
          LINsf2NDQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          LINsf2NDID = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          //Not used
       When FldOcur = 4;
         // Not used
       Other;          //Not used
       Endsl;

       Endsr;
       //-----------------------------------------------------------------
       // REF Loop 2420E  - Ordering Provider Secondary Id
       //-----------------------------------------------------------------
       Begsr REF2420E;         // NOT TESTED

       Select;
       When FldOcur = 1;
          LINor2IDQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          LINor2ID  = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          //Not used
       When FldOcur = 4;
         // Not used
       Other;          //Not used
       Endsl;

       Endsr;
      /End-Free

     P ParseREF        E
      *=====================================================================
      * Parse HL
      *=====================================================================
     P ParseHL         B
     D ParseHL         PI
     D  segData                     500A
     D  FrmPos                        5  0
     D  FldLen                        5  0
     D  FldOcur                       5  0
     D  Loop                          6A

      /Free
       //Process HL Segment

       Select;
       When Loop = '1000B';
          Loop = '2000A';
       When HLnbr = 2;
          Write PFAHDR;
       Endsl;

      /End-Free
     P ParseHL         E

      *=====================================================================
      * Parse SBR segment
      *=====================================================================
     P ParseSBR        B
     D ParseSBR        PI
     D  segData                     500A
     D  FrmPos                        5  0
     D  FldLen                        5  0
     D  FldOcur                       5  0
      /Free
       Select;
       When FldOcur = 1;
          CLMsubPRSN = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMsubIRC = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMsubGPOL =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          CLMsubGPNM  = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 5;
          CLMsubITC   = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 6;
          // Not used
       When FldOcur = 7;
          // Not used
       When FldOcur = 8;
          // Not used
       When FldOcur = 9;
          CLMsubCLIC  = %Subst(SegData:FrmPos:FldLen);
       Endsl;

      /End-Free
     P ParseSBR        E

      *=====================================================================
      * Parse DMG
      *=====================================================================
     P ParseDMG        B
     D ParseDMG        PI
     D  segData                     500A
     D  FrmPos                        5  0
     D  FldLen                        5  0
     D  FldOcur                       5  0

      /Free
       //Process DMG Segment

       Select;
       When FldOcur = 1;
          CLMpatDTFQ =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMpatDOB =%Subst(SegData:FrmPos:FldLen);
       Other;
          ix =*Zero;
       Endsl;

      /End-Free
     P ParseDMG        E
      *=====================================================================
      * Parse CLM segment
      *=====================================================================
     P ParseCLM        B
     D ParseCLM        PI
     D  segData                     500A
     D  FrmPos                        5  0
     D  FldLen                        5  0
     D  FldOcur                       5  0
     D  Clm5           S              7A   inz
     D  ClnFrm         S              5  0 inz
     D  ClnTo          S              5  0 inz
     D  ClnIdx         S              5  0 inz
     D  Clm5Len        S              5  0 inz
     D  intIdx         S              5  0 inz
     D  ClnMax         S              5  0 inz
      /Free
       Select;
       When FldOcur = 1;
          is2300 = *On;
          is2400 = *Off;
          CLMunid = strUniqueId;
          CLMicntl = ICHinctl#;
          CLMgctl = FGHcntl;
          CLMtsCNTL = HDtshCNTL;
          CLMbhtTID = HDbhtOATI;
          CLMinsid  = ICHinsid;
          CLMpacct  = %Subst(SegData:FrmPos:FldLen);
          Exsr ChkDupClm;
          //*** Beging Add *** 04/03/19 **********************************
          If isDupclaim;
             CLMpasfx = GetPaSfx(CLMpacct:CLMinsid);
          Else;
             CLMpasfx = *Zeros;
          Endif;
          //*** End    Add *** 04/03/19 **********************************

       When FldOcur = 2;
          CLMch =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          //Not used;
       When FldOcur = 4;
          //Not used;
       When FldOcur = 5;
         CLM5  = %Subst(SegData:FrmPos:FldLen);
         ClnMax = GetFldCnt(CLM5:':');
         ClnTo = *Zeros;

         If ClnMax > *Zero;
            For intIdx = 1 to ClnMax;
               ClnFrm = ClnTo + 1;
               ClnIdx = ClnTo + 1;
               ClnTo = %scan(':':CLM5:ClnIdx);

               If ClnTo = *Zero;
                  ClnTo = FldLen + 1;
               Endif;

               Clm5Len = ClnTo - ClnFrm;

               Select;
               When intIdx = 1;
                  CLMfc =%Subst(CLM5:ClnFrm:Clm5Len);
               When intIdx = 2;
                  CLMfqu =%Subst(CLM5:ClnFrm:Clm5Len);
               When intIdx = 3;
                  CLMfcd =%Subst(CLM5:ClnFrm:Clm5Len);
               Endsl;
            Endfor;
         Else;                                        // No :, 1 field
            ClnFrm = FrmPos;                          // Not tested
            Clnto =  %scan('*':SegData:ClnFrm);
            If Clnto = *zero;
               Clnto =  %scan(' ':SegData:ClnFrm);
            Endif;

            If Clnto > *zero and ClnFrm > *zero and ClnTo > ClnFrm;
               Clm5Len = ClnTo - ClnFrm;

               If Clm5Len > *zero;
                  CLMfc =  %Subst(SegData:ClnFrm:Clm5Len);
               Else;
                  CLMfc =  *blanks;
               Endif;

            Endif;

            CLMfqu = *blanks;
            CLMfcd = *blanks;
         Endif;

       Other;
          //Not used
       Endsl;

       //--------------------------------------------------------------
       // ChkDupClm - Check for duplicate claim
       //--------------------------------------------------------------
       Begsr ChkDupClm;
       Setll (CLMpacct:CLMinsid) FACLML5;
       If %equal(FACLML5);
          isDupClaim = *on;
          Idx = Idx + 1;
          MsgAry(Idx) = ErrMsg1;
          RrnAry(Idx) = DBrrn837;
          DtaAry(Idx) = %Trim(CLMpacct) + '-' + %Trim(CLMinsid);
          //*** Begin Add 03/18/19 ************************************
          ErrTyp(Idx) = 'S';             //S = Soft error, H=Hard error
          //*** End   Add 03/18/19 ************************************

       Endif;
       Endsr;
      /End-Free
     P ParseCLM        E
      *=====================================================================
      * Parse DT
      *=====================================================================
     P ParseDTP        B
     D ParseDTP        PI
     D  segData                     500A
     D  FrmPos                        5  0
     D  FldLen                        5  0
     D  FldOcur                       5  0
     D  DTqu                          3A

      /Free
       //Process NM1 loops
       Select;
       When DTqu = '454';  //2300 Loop - Initial Treatment
          Exsr DTP454;
       When DTqu = '304';  //2300 Loop - Last Seen
          Exsr DTP304;
       When DTqu = '439';  //2300 Loop - Accident
          Exsr DTP439;
       When DTqu = '435';  //2300 Loop - Admission
          Exsr DTP435;
       When DTqu = '096';  //2300 Loop - Discharge
          Exsr DTP096;
       When DTqu = '050';  //2300 Loop - Repricer Received
          Exsr DTP050;
       When DTqu = '472';  //2400 Loop - Service Line
          Exsr DTP472;
       Other;
          ix = 0;         //Skip
       Endsl;
       //--------------------------------------------------------------
       // DTP454 -  Loop 2300 - Initial Treatment
       //--------------------------------------------------------------
       Begsr DTP454;
       Select;
       When FldOcur = 1;
          CLMitdtQ = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMitdtPFQ = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMitdt =%Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // DTP304 -  Loop 2300 - Last seen
       //--------------------------------------------------------------
       Begsr DTP304;
       Select;
       When FldOcur = 1;
          CLMlsdtQ = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMlsdtPFQ = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMlsdt =%Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // DTP439 -  Loop 2300 - Accident
       //--------------------------------------------------------------
       Begsr DTP439;
       Select;
       When FldOcur = 1;
          CLMadQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMadFQ = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMad =%Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // DTP435 -  Loop 2300 - Admission
       //--------------------------------------------------------------
       Begsr DTP435;
       Select;
       When FldOcur = 1;
          CLMaQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMaFQ = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMa  =%Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // DTP096 -  Loop 2300 - Discharge
       //--------------------------------------------------------------
       Begsr DTP096;
       Select;
       When FldOcur = 1;
          CLMddQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMddFQ = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMdd   =%Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // DTP050 -  Loop 2300 - Repricer Received
       //--------------------------------------------------------------
       Begsr DTP050;
       Select;
       When FldOcur = 1;
          CLMrrQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMrrFQ = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMrrrd = %Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // DTP472 -  Loop 2400 - Service Line Number
       //--------------------------------------------------------------
       Begsr DTP472;
       Select;
       When FldOcur = 1;
          LINdtQ  = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          LINtfQ = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          LINsvDT = %Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;

       Endsr;
      /End-Free
     P ParseDTP        E
      *=====================================================================
      * Parse CN1 - Loop 2300 - Contract Information  ** NOT TESTED **
      *=====================================================================
     P ParseCN1        B
     D ParseCN1        PI
     D  segData                     500A
     D  FrmPos                        5  0
     D  FldLen                        5  0
     D  FldOcur                       5  0
      /Free
       If Loop = '2300';
          If FldOcur = 1;
             CLMctCD = %Subst(SegData:FrmPos:FldLen);
          Else;
             ix = 0;     //Skip
          Endif;
       Endif;

       If Loop = '2400';
          Select;
          When FldOcur = 1;
             LINctTYCD1 = %Subst(SegData:FrmPos:FldLen);
          When FldOcur = 2;
             //Not used
          When FldOcur = 3;
             //Not used
          When FldOcur = 4;
             LINctTYCD2 = %Subst(SegData:FrmPos:FldLen);
          Other;
             ix=0;       //Leave
          Endsl;
       Endif;
      /End-Free
     P ParseCN1        E

      *=====================================================================
      * Parse HI - Loop 2300 - Health Care Diagnosis Code
      *=====================================================================
     P ParseHI         B
     D ParseHI         PI
     D  segData                     500A
     D  FrmPos                        5  0
     D  FldLen                        5  0
     D  FldOcur                       5  0
     D  ClnFrm         S              5  0 inz
     D  ClnTo          S              5  0 inz
     D  ClnIdx         S              5  0 inz
     D  ICDlen         S              5  0 inz
     D  HIcode         S              3A   inz


      /Free
       ClnIdx = FrmPos;
       Clnto = %scan(':':SegData:ClnIdx) ;
       If Clnto > *zero and FrmPos > *zero and ClnTo > FrmPos;
          ICDlen = Clnto - FrmPos;
          HIcode = %Subst(SegData:FrmPos:ICDlen);
          Select;
          When HIcode = 'ABK' or HIcode = 'BK' or //Diagnosis code
               HIcode = 'ABF' or HIcode = 'BF';
             DGQ(FldOcur)  = HIcode;
          When HIcode = 'BG';                     //Condition Information
                CCQ(FldOcur)  = HIcode;
          Other;                                  //Not used.
          Endsl;
       Endif;

       ClnFrm = ClnTo + 1;
       If ClnFrm <> *zero;
          ClnIdx = ClnFrm;
          If FldOcur = FldCnt;
             Clnto = %scan(' ':SegData:ClnIdx) ;
          Else;
             Clnto = %scan('*':SegData:ClnIdx) ;
          Endif;
          ICDlen = Clnto - ClnFrm;
          Select;
          When HIcode = 'ABK' or HIcode = 'BK' or  //Diagnosis code
               HIcode = 'ABF' or HIcode = 'BF';
             DGC(FldOcur) = %Subst(SegData:ClnFrm:ICDlen);
          When HIcode = 'BG';                      //Condition Information
             CCC(FldOcur) = %Subst(SegData:ClnFrm:ICDlen);
          Other;                                   //Not used
          Endsl;
       Endif;
      /End-Free
     P ParseHI         E
      *=====================================================================
      * Parse LX  - Loop 2400 - Service Line Number - Required
      *=====================================================================
     P ParseLX         B
     D ParseLX         PI
     D  segData                     500A
     D  FrmPos                        5  0
     D  FldLen                        5  0
     D  FldOcur                       5  0
      /Free
       LINlin# = %Subst(SegData:FrmPos:FldLen);
       is2300 = *Off;
       is2400 = *On;
       Loop = '2400';
      /End-Free
     P ParseLX         E
      *=====================================================================
      * Parse SV1 - Loop 2400 - Professional Service
      *=====================================================================
     P ParseSV1        B
     D ParseSV1        PI
     D  segData                     500A
     D  FrmPos                        5  0
     D  FldLen                        5  0
     D  FldOcur                       5  0

     D  ClnIdx         S              5  0
     D  ClnFrm         S              5  0
     D  Clnto          S              5  0
     D  intIdx         S              5  0 inz
     D  SV101len       S              5  0 inz
     D  SV107len       S              5  0 inz
     D  ClnMax         S              5  0
     D  SV101          S            193A   inz
     D  SV107          S             11A   inz
      /Free
       Select;
       When FldOcur = 1;
         LINgctl   = FGHcntl;
         LINtsCNTL = HDtshCNTL;
         LINbhtTID = HDbhtOATI;
         LINpacct  = CLMpacct;
         //*** Begin Add *** 04/03/19 *****************************************
         LINinsid  = CLMinsid;
         LINpasfx  = CLMpasfx;
         //*** End   Add *** 04/03/19 *****************************************
         SV101 = %Subst(SegData:FrmPos:FldLen);
         ClnMax = GetFldCnt(SV101:':');
         ClnTo = *Zeros;

         If ClnMax > *Zero;

            For intIdx = 1 to ClnMax;
               ClnFrm = ClnTo + 1;
               ClnIdx = ClnTo + 1;
               ClnTo = %scan(':':SV101:ClnIdx);

               If ClnTo = *Zero;
                  ClnTo = FldLen + 1;
               Endif;

               SV101Len = ClnTo - ClnFrm;

               Select;
               When intIdx = 1;
                  LINqu = %Subst(SV101:ClnFrm:SV101len);
               When intIdx = 2;
                  LINcode = %Subst(SV101:ClnFrm:SV101len);
               When intIdx = 3;
                  LINmod1 = %Subst(SV101:ClnFrm:SV101len);
               When intIdx = 4;
                  LINmod2 = %Subst(SV101:ClnFrm:SV101len);
               When intIdx = 5;
                  LINmod3 = %Subst(SV101:ClnFrm:SV101len);
               When intIdx = 6;
                  LINmod4 = %Subst(SV101:ClnFrm:SV101len);
               When intIdx = 7;
                  LINdesc = %Subst(SV101:ClnFrm:SV101len);
               Other;                            //Not used
               Endsl;
            Endfor;

         Else;                                        // No Colon Found - 1 Field
            ClnFrm = FrmPos;                          // Not tested
            Clnto =  %scan('*':SegData:ClnFrm);
            If Clnto = *zero;
               Clnto =  %scan(' ':SegData:ClnFrm);
            Endif;

            If Clnto > *zero and ClnFrm > *zero and ClnTo > ClnFrm;
               SV101Len = ClnTo - ClnFrm;

               If SV101Len > *zero;
                  LINqu =  %Subst(SegData:ClnFrm:SV101Len);
               Else;
                  LINqu =  *blanks;
               Endif;

            Endif;

            LINcode = *blanks;
            LINmod1 = *blanks;
            LINmod2 = *blanks;
            LINmod3 = *blanks;
            LINmod4 = *blanks;
            LINdesc = *blanks;
         Endif;

         // LINgctl   = FGHcntl;
         // LINtsCNTL = HDtshCNTL;
         // LINbhtTID = HDbhtOATI;
         // LINpacct  = CLMpacct;
         // ClnFrm = FrmPos;
         // ClnIdx = ClnFrm;
         // ClnMax = %scan('*':SegData:ClnIdx) ;
         // If ClnMax = *zero;
         //    ClnMax =  %scan(' ':SegData:ClnIdx);
         // Endif;

         // For intIdx = 1 to 8;
         //    ClnIdx = ClnFrm;
         //    Clnto = %scan(':':SegData:ClnIdx);

         //    If Clnto >= ClnMax;
         //       Clnto = *Zeros;
         //       ClnMax = 99999;
         //    Endif;

         //    If Clnto = *zero;
         //       Clnto =  %scan('*':SegData:ClnIdx);
         //       If Clnto = *zero;
         //          Clnto =  %scan(' ':SegData:ClnIdx);
         //       Endif;
         //    Endif;

         //    If Clnto > *zero and ClnFrm > *zero and ClnTo > ClnFrm;
         //       SV1len = Clnto - ClnFrm;
         //       Select;
         //       When intIdx = 1;
         //          LINqu = %Subst(SegData:ClnFrm:SV1len);
         //       When intIdx = 2;
         //          LINcode = %Subst(SegData:ClnFrm:SV1len);
         //       When intIdx = 3;
         //          LINmod1 = %Subst(SegData:ClnFrm:SV1len);
         //       When intIdx = 4;
         //          LINmod2 = %Subst(SegData:ClnFrm:SV1len);
         //       When intIdx = 5;
         //          LINmod3 = %Subst(SegData:ClnFrm:SV1len);
         //       When intIdx = 6;
         //          LINmod4 = %Subst(SegData:ClnFrm:SV1len);
         //       Other;                            //Not used
         //       Endsl;
         //    Endif;
         //    If %subst(Segdata:Clnto:1) = '*' or
         //       %subst(Segdata:Clnto:1) = *blanks or ClnMax = 99999;
         //       Leave;
         //    Endif;
         //    ClnFrm = Clnto + 1;
         // Endfor;

       When FldOcur = 2;
          LINchrg = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          LINunbas = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          LINsvcunit = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 5;
          LINpos = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 6;
          LINsvtycd = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 7;
         SV107 = %Subst(SegData:FrmPos:FldLen);
         ClnMax = GetFldCnt(SV107:':');
         ClnTo  = *Zeros;

         If ClnMax > *Zero;

            For intIdx = 1 to ClnMax;
               ClnFrm = ClnTo + 1;
               ClnIdx = ClnTo + 1;
               ClnTo = %scan(':':SV107:ClnIdx);

               If ClnTo = *Zero;
                  ClnTo = FldLen + 1;
               Endif;

               SV107Len = ClnTo - ClnFrm;

                Select;
                When intIdx = 1;
                   LINdgptr1 = %Subst(SV107:ClnFrm:SV107len);
                When intIdx = 2;
                   LINdgptr2 = %Subst(SV107:ClnFrm:SV107len);
                When intIdx = 3;
                   LINdgptr3 = %Subst(SV107:ClnFrm:SV107len);
                When intIdx = 4;
                   LINdgptr4 = %Subst(SV107:ClnFrm:SV107len);
                Other;                                   //Not used
                Endsl;
            Endfor;

         Else;                                        // No Colon Found - 1 Field
            ClnFrm = FrmPos;                          // Not tested
            Clnto =  %scan('*':SegData:ClnFrm);
            If Clnto = *zero;
               Clnto =  %scan(' ':SegData:ClnFrm);
            Endif;

            If Clnto > *zero and ClnFrm > *zero and ClnTo > ClnFrm;
               SV107Len = ClnTo - ClnFrm;

               If SV107Len > *zero;
                  LINdgptr1 =  %Subst(SegData:ClnFrm:SV107Len);
               Else;
                  LINdgptr1 =  *blanks;
               Endif;

            Endif;

            LINdgptr2 = *blanks;
            LINdgptr3 = *blanks;
            LINdgptr4 = *blanks;
         Endif;

          //ClnFrm = FrmPos;
          //ClnIdx = ClnFrm;
          //ClnMax = %scan('*':SegData:ClnIdx) ;
          //
          //If ClnMax = *zero;
          //   ClnMax =  %scan(' ':SegData:ClnIdx);
          //Endif;
          //For intIdx = 1 to 4;
          //   ClnIdx = ClnFrm;
          //   Clnto = %scan(':':SegData:ClnIdx) ;
          //
          //   If Clnto >= ClnMax;
          //      Clnto = *Zeros;
          //      ClnMax = 99999;
          //   Endif;
          //
          //   If Clnto = *zero;
          //      Clnto =  %scan('*':SegData:ClnIdx);
          //      If Clnto = *zero;
          //         Clnto =  %scan(' ':SegData:ClnIdx);
          //      Endif;
          //   Endif;
          //
          //   If Clnto > *zero and ClnFrm > *zero and ClnTo > ClnFrm;
          //      SV1len = Clnto - ClnFrm;
          //      Select;
          //      When intIdx = 1;
          //         LINdgptr1 = %Subst(SegData:ClnFrm:SV1len);
          //      When intIdx = 2;
          //         LINdgptr2 = %Subst(SegData:ClnFrm:SV1len);
          //      When intIdx = 3;
          //         LINdgptr3 = %Subst(SegData:ClnFrm:SV1len);
          //      When intIdx = 4;
          //         LINdgptr4 = %Subst(SegData:ClnFrm:SV1len);
          //      Other;                                   //Not used
          //      Endsl;
          //   Endif;
          //
          //   If %subst(Segdata:Clnto:1) = '*' or
          //      %subst(Segdata:Clnto:1) = *blanks or ClnMax = 99999;
          //      Leave;
          //   Endif;
          //   ClnFrm = Clnto + 1;
          //Endfor;
       Endsl;

      /End-Free
     P ParseSV1        E
      *=====================================================================
      * Parse LIN - Loop 2410 - Drug Identification  - Situational
      *=====================================================================
     P ParseLIN        B
     D ParseLIN        PI
     D  segData                     500A
     D  FrmPos                        5  0
     D  FldLen                        5  0
     D  FldOcur                       5  0
      /Free

          Loop = '2410';

          Select;
          When FldOcur = 1;
             //Not used
          When FldOcur = 2;
             LINdrgIDQU = %Subst(SegData:FrmPos:FldLen);
          When FldOcur = 3;
             LINdrgNDC  = %Subst(SegData:FrmPos:FldLen);
          Other;
             ix=0;       //Leave
          Endsl;
      /End-Free
     P ParseLIN        E
      *=====================================================================
      * Parse CTP - Loop 2410 - Drug Pricing  - Situational  - PENDING
      *=====================================================================
     P ParseCTP        B
     D ParseCTP        PI
     D  segData                     500A
     D  FrmPos                        5  0
     D  FldLen                        5  0
     D  FldOcur                       5  0
     D  intIdx         S              5  0 inz
      /Free

          Select;
          When FldOcur = 1;
             //Not used
          When FldOcur = 2;
             //Not used
          When FldOcur = 3;
             //Not used
          When FldOcur = 4;
             LINdrgUNIT =%Subst(SegData:FrmPos:FldLen);
          When FldOcur = 5;            //PENDING
             LINdrgUNBS =%Subst(SegData:FrmPos:FldLen);        //NOT TESTED
             //For intIdx = 1 to 2;
             //LINdrgUNMS = XXXX;
             //LINdrgUNBS = XXXX;
             //Endfor;
          Other;
             ix=0;       //Leave
          Endsl;
      /End-Free
     P ParseCTP        E
      *=====================================================================
      * Parse SE segment
      *=====================================================================
     P ParseSE         B
     D ParseSE         PI
     D  segData                     500A
     D  FrmPos                        5  0
     D  FldLen                        5  0
     D  FldOcur                       5  0
      /Free
       Select;
       When FldOcur = 1;
          Clear PFATST;
          TSTunid = strUniqueID;
          TSTicntl = ICHinctl#;
          TSTgcntl = FGHcntl;
          TSTsegCnt = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          TSTtsCntl = %Subst(SegData:FrmPos:FldLen);
          Write PFATST;
       Other;
          //Not used.
       Endsl;
      /End-Free
     P ParseSE         E

      *=====================================================================
      * Parse GE segment
      *=====================================================================
     P ParseGE         B
     D ParseGE         PI
     D  segData                     500A
     D  FrmPos                        5  0
     D  FldLen                        5  0
     D  FldOcur                       5  0
      /Free
       Select;
       When FldOcur = 1;
          Clear PFAFGT;
          FGTunid = strUniqueID;
          FGTicntl = ICHinctl#;
          FGT#TS = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          FGTgcntl = %Subst(SegData:FrmPos:FldLen);
          Write PFAFGT;
       Endsl;
      /End-Free
     P ParseGE         E
      *=====================================================================
      * Parse IEA segment
      *=====================================================================
     P ParseIEA        B
     D ParseIEA        PI
     D  segData                     500A
     D  FrmPos                        5  0
     D  FldLen                        5  0
     D  FldOcur                       5  0
      /Free
       Select;
       When FldOcur = 1;
          Clear PFAICT;
          ICTunid = strUniqueID;
          ICT#fg = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          ICTicntl = %Subst(SegData:FrmPos:FldLen);
          Write PFAICT;
       Endsl;
      /End-Free
     P ParseIEA        E
