     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
      // SEARCH 'CONTINUE HERE'
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H Bnddir('STBNDDIR')
     H/TITLE F8192R2
      //*************************************************************************
      //  Description:  837 Parser into Holding Files - Institutional
      //
      //  Created: 10/26/17   Programmer C:180      Project: 114MJB
      //*************************************************************************
      //  Revisions:
      //  04/30/19 C:180  Project 114MJC
      //                  Allow multiple subscribers when processing more
      //                  than one data set.
      //
      //  04/03/19 C:180  SUPPORT 78417/114MJC
      //                  Donot bypass duplicate claims.
      //                  Add Patient acct suffix field CLMpasfx to allow for
      //                  dup claims.
      //
      //  03/18/19 C:180  SUPPORT 77811
      //                  Distinguish between hard and soft errors:
      //                  Hard error -> Abort MC interface
      //  03/14/19 C:180  SUPPORT 77811
      //                  Reposition Dup claim logic to prevent infinite
      //                  loop.
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
     D F8192R2         PR
     D                              100A
     D                                9A
     D                                1A
     D F8192R2         PI
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

     D ParseCL1        PR
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

     D ParseHI         PR
     D                              500A
     D                                5  0
     D                                5  0
     D                                5  0
     D                                3A

     D ParseSV2        PR
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
      *  Occurrence Qualifier
      *
     D  occQUds        DS
     D   CLMocc1QU
     D   CLMocc2QU
     D   CLMocc3QU
     D   CLMocc4QU
     D   CLMocc5QU
     D   CLMocc6QU
     D   CLMocc7QU
     D   CLMocc8QU
     D   CLMocc9QU
     D   CLMocc10QU
     D   CLMocc11QU
     D   CLMocc12QU
     D   occQU                        3    OVERLAY(occQUds) DIM(12)

      *
      *  Occurrence Code
      *
     D  occCDds        DS
     D   CLMocc1CD
     D   CLMocc2CD
     D   CLMocc3CD
     D   CLMocc4CD
     D   CLMocc5CD
     D   CLMocc6CD
     D   CLMocc7CD
     D   CLMocc8CD
     D   CLMocc9CD
     D   CLMocc10CD
     D   CLMocc11CD
     D   CLMocc12CD
     D   occCD                       30    OVERLAY(occCDds) DIM(12)

      *
      *  Date Time Period Qualifier
      *
     D  occDQds        DS
     D   CLMocc1DQ
     D   CLMocc2DQ
     D   CLMocc3DQ
     D   CLMocc4DQ
     D   CLMocc5DQ
     D   CLMocc6DQ
     D   CLMocc7DQ
     D   CLMocc8DQ
     D   CLMocc9DQ
     D   CLMocc10DQ
     D   CLMocc11DQ
     D   CLMocc12DQ
     D   occDQ                        3    OVERLAY(occDQds) DIM(12)

      *
      *  Date Time Period
      *
     D  occDTds        DS
     D   CLMocc1DT
     D   CLMocc2DT
     D   CLMocc3DT
     D   CLMocc4DT
     D   CLMocc5DT
     D   CLMocc6DT
     D   CLMocc7DT
     D   CLMocc8DT
     D   CLMocc9DT
     D   CLMocc10DT
     D   CLMocc11DT
     D   CLMocc12DT
     D   occDT                       35    OVERLAY(occDTds) DIM(12)

      *
      *  External Cause of Injury Qualifier
      *
     D  ecdgtDS        DS
     D   CLMecdgt1
     D   CLMecdgt2
     D   CLMecdgt3
     D   CLMecdgt4
     D   CLMecdgt5
     D   CLMecdgt6
     D   CLMecdgt7
     D   CLMecdgt8
     D   CLMecdgt9
     D   CLMecdgt10
     D   CLMecdgt11
     D   CLMecdgt12
     D   ecdgt                        3    OVERLAY(ecdgtDS) DIM(12)

      *
      *  External Cause of Injury Code
      *
     D  ecdgDS         DS
     D   CLMecdg1
     D   CLMecdg2
     D   CLMecdg3
     D   CLMecdg4
     D   CLMecdg5
     D   CLMecdg6
     D   CLMecdg7
     D   CLMecdg8
     D   CLMecdg9
     D   CLMecdg10
     D   CLMecdg11
     D   CLMecdg12
     D   ecdg                        30    OVERLAY(ecdgDS) DIM(12)

      *
      *  External Cause of Injury - Present on Admission
      *
     D  ecPOAds        DS
     D   CLMecpoa1
     D   CLMecpoa2
     D   CLMecpoa3
     D   CLMecpoa4
     D   CLMecpoa5
     D   CLMecpoa6
     D   CLMecpoa7
     D   CLMecpoa8
     D   CLMecpoa9
     D   CLMecpoa10
     D   CLMecpoa11
     D   CLMecpoa12
     D   ecPOA                        1    OVERLAY(ecPOAds) DIM(12)

      *
      *  Other Diagnosis Information Qualifier
      *
     D  dgQds          DS
     D   CLMdg2Q
     D   CLMdg3Q
     D   CLMdg4Q
     D   CLMdg5Q
     D   CLMdg6Q
     D   CLMdg7Q
     D   CLMdg8Q
     D   CLMdg9Q
     D   CLMdg10Q
     D   CLMdg11Q
     D   CLMdg12Q
     D   CLMdgT13
     D   CLMdgT14
     D   CLMdgT15
     D   CLMdgT16
     D   CLMdgT17
     D   CLMdgT18
     D   dgQ                          3    OVERLAY(dgQDS) DIM(17)

      *
      *  Other Diagnosis Information Code
      *
     D  dgDS           DS
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
     D   CLMdg13
     D   CLMdg14
     D   CLMdg15
     D   CLMdg16
     D   CLMdg17
     D   CLMdg18
     D   dg                          30    OVERLAY(dgDS) DIM(17)

      *
      *  Other Diagnosis Information - Present on Admission
      *
     D  dgPOAds        DS
     D   CLMdgPOA2
     D   CLMdgPOA3
     D   CLMdgPOA4
     D   CLMdgPOA5
     D   CLMdgPOA6
     D   CLMdgPOA7
     D   CLMdgPOA8
     D   CLMdgPOA9
     D   CLMdgPOA10
     D   CLMdgPOA11
     D   CLMdgPOA12
     D   CLMdgPOA13
     D   CLMdgPOA14
     D   CLMdgPOA15
     D   CLMdgPOA16
     D   CLMdgPOA17
     D   CLMdgPOA18
     D   dgPOA                        1    OVERLAY(dgPOAds) DIM(17)
      *
      *  Other Procedure Information Qualifier
      *
     D  prQds          DS
     D   CLMpr2Q
     D   CLMpr3Q
     D   CLMpr4Q
     D   CLMpr5Q
     D   CLMpr6Q
     D   CLMpr7Q
     D   CLMpr8Q
     D   CLMpr9Q
     D   CLMpr10Q
     D   CLMpr11Q
     D   CLMpr12Q
     D   CLMpr13Q
     D   prQ                          3    OVERLAY(prQDS) DIM(12)

      *
      *  Other Procedure Information Code
      *
     D  prDS           DS
     D   CLMpr2
     D   CLMpr3
     D   CLMpr4
     D   CLMpr5
     D   CLMpr6
     D   CLMpr7
     D   CLMpr8
     D   CLMpr9
     D   CLMpr10
     D   CLMpr11
     D   CLMpr12
     D   CLMpr13
     D   pr                          30    OVERLAY(prDS) DIM(12)
      *
      *  Other Procedure Information Date-time-Period Qualifier
      *
     D  prDTQds        DS
     D   CLMpr2DTQ
     D   CLMpr3DTQ
     D   CLMpr4DTQ
     D   CLMpr5DTQ
     D   CLMpr6DTQ
     D   CLMpr7DTQ
     D   CLMpr8DTQ
     D   CLMpr9DTQ
     D   CLMpr10DTQ
     D   CLMpr11DTQ
     D   CLMpr12DTQ
     D   CLMpr13DTQ
     D   prDTQ                        3    OVERLAY(prDTQDS) DIM(12)

      *
      *  Other Procedure Information Date-Time-Period
      *
     D  prdtDS         DS
     D   CLMpr2DT
     D   CLMpr3DT
     D   CLMpr4DT
     D   CLMpr5DT
     D   CLMpr6DT
     D   CLMpr7DT
     D   CLMpr8DT
     D   CLMpr9DT
     D   CLMpr10DT
     D   CLMpr11DT
     D   CLMpr12DT
     D   CLMpr13DT
     D   prDT                        35    OVERLAY(prdtDS) DIM(12)

      *
      *  Occurrence Span Information Qualifier
      *
     D  osQUds         DS
     D   CLMos1QU
     D   CLMos2QU
     D   CLMos3QU
     D   CLMos4QU
     D   CLMos5QU
     D   CLMos6QU
     D   CLMos7QU
     D   CLMos8QU
     D   CLMos9QU
     D   CLMos10QU
     D   CLMos11QU
     D   CLMos12QU
     D   osQU                         3    OVERLAY(osQUDS) DIM(12)

      *
      *  Occurrence Span Information Code
      *
     D  oscdDS         DS
     D   CLMos1CD
     D   CLMos2CD
     D   CLMos3CD
     D   CLMos4CD
     D   CLMos5CD
     D   CLMos6CD
     D   CLMos7CD
     D   CLMos8CD
     D   CLMos9CD
     D   CLMos10CD
     D   CLMos11CD
     D   CLMos12CD
     D   osCD                        30    OVERLAY(oscdDS) DIM(12)
      *
      *  Occurrence Span Information Date-time-Period Qualifier
      *
     D  osDTQds        DS
     D   CLMos1DTQ
     D   CLMos2DTQ
     D   CLMos3DTQ
     D   CLMos4DTQ
     D   CLMos5DTQ
     D   CLMos6DTQ
     D   CLMos7DTQ
     D   CLMos8DTQ
     D   CLMos9DTQ
     D   CLMos10DTQ
     D   CLMos11DTQ
     D   CLMos12DTQ
     D   osDTQ                        3    OVERLAY(osDTQds) DIM(12)

      *
      *  Occurrence Span Information Date-Time-Period
      *
     D  osDTPds        DS
     D   CLMos1DTP
     D   CLMos2DTP
     D   CLMos3DTP
     D   CLMos4DTP
     D   CLMos5DTP
     D   CLMos6DTP
     D   CLMos7DTP
     D   CLMos8DTP
     D   CLMos9DTP
     D   CLMos10DTP
     D   CLMos11DTP
     D   CLMos12DTP
     D   osDTP                       35    OVERLAY(osDTPds) DIM(12)
      *
      * Value Information Qualifier
      *
     D  vcQUds         DS
     D   CLMvc1QU
     D   CLMvc2QU
     D   CLMvc3QU
     D   CLMvc4QU
     D   CLMvc5QU
     D   CLMvc6QU
     D   CLMvc7QU
     D   CLMvc8QU
     D   CLMvc9QU
     D   CLMvc10QU
     D   CLMvc11QU
     D   CLMvc12QU
     D   vcQU                         3    OVERLAY(vcQUDS) DIM(12)

      *
      *  Value Information Code
      *
     D  vcCDds         DS
     D   CLMvc1
     D   CLMvc2
     D   CLMvc3
     D   CLMvc4
     D   CLMvc5
     D   CLMvc6
     D   CLMvc7
     D   CLMvc8
     D   CLMvc9
     D   CLMvc10
     D   CLMvc11
     D   CLMvc12
     D   vcCD                        30    OVERLAY(vcCDds) DIM(12)
      *
      *  Value Information Code Amount
      *
     D  vcaDS          DS
     D   CLMvca1
     D   CLMvca2
     D   CLMvca3
     D   CLMvca4
     D   CLMvca5
     D   CLMvca6
     D   CLMvca7
     D   CLMvca8
     D   CLMvca9
     D   CLMvca10
     D   CLMvca11
     D   CLMvca12
     D   vca                         18    OVERLAY(vcaDS) DIM(12)

      *
      * Condition Information Qualifier
      *
     D  CCQds          DS
     D   CLMccQ1
     D   CLMccQ2
     D   CLMccQ3
     D   CLMccQ4
     D   CLMccQ5
     D   CLMccQ6
     D   CLMccQ7
     D   CLMccQ8
     D   CLMccQ9
     D   CLMccQ10
     D   CLMccQ11
     D   CLMccQ12
     D   CCQ                          3    OVERLAY(CCQds) DIM(12)

      *
      *  Condition Information Code
      *
     D  ccCDds         DS
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
     D   ccCD                        30    OVERLAY(ccCDds) DIM(12)
      *
      * Treatment Code Qualifier
      *
     D  TCquDS         DS
     D   CLMtc1QU
     D   CLMtc2QU
     D   CLMtc3QU
     D   CLMtc4QU
     D   CLMtc5QU
     D   CLMtc6QU
     D   CLMtc7QU
     D   CLMtc8QU
     D   CLMtc9QU
     D   CLMtc10QU
     D   CLMtc11QU
     D   CLMtc12QU
     D   TCqu                         3    OVERLAY(TCquDS) DIM(12)

      *
      *  Treatment Code
      *
     D  TCcdDS         DS
     D   CLMtc1CD
     D   CLMtc2CD
     D   CLMtc3CD
     D   CLMtc4CD
     D   CLMtc5CD
     D   CLMtc6CD
     D   CLMtc7CD
     D   CLMtc8CD
     D   CLMtc9CD
     D   CLMtc10CD
     D   CLMtc11CD
     D   CLMtc12CD
     D   TCcd                        30    OVERLAY(TCcdDS) DIM(12)
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

      *================================================================
      *  Stand alone
      *================================================================
     D strUniqueId     S            100A   inz
     D rrnOne          S              9P 0 inz(1)
     D strSeg          S              3A   inz
     D Entity          S              3A   inz
     D HIcode          S              3A   inz
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
     D NM1PRcnt        S              5  0 inz
     D NM171cnt        S              5  0 inz
     D NM172cnt        S              5  0 inz
     D NM1ZZcnt        S              5  0 inz
     D NM177cnt        S              5  0 inz
     D NM182cnt        S              5  0 inz
     D NM1DNcnt        S              5  0 inz
     D NM185cnt        S              5  0 inz
     D NM1ILcnt        S              5  0 inz
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
      *
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
          // If *in76;
          //    Leave;
          // Endif;
          //Temporary code --- Ends   ---|

          Read FA837FP;
       Enddo;


       //Print Error report
       If idx > 0;
          //*** Begin Delete 03/18/19 *****************************************
          //  ##RetCode =  '1';
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
             Except DET;

          //*** Begin Add *** 03/18/19 ****************************************
             //Abort MC process if Hard error.
             If ErrTyp(i) = 'H';
                ##RetCode =  '1';
             Endif;
          //*** End   Add *** 03/18/19 ****************************************
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
                // //*** Begin Add *** 03/14/19 *******************************
                // If %EOF(FA837FP);
                //    Leavesr;
                // Else;
                //    //Get Field Count
                //    FldCnt = GetFldCnt(FA837DATA:'*');
                //    If FldCnt > *Zero;
                //       ix = 0;
                //       FldOcur = 0;
                //       Delimiter = '*';
                //       Iter;
                //    Else;
                //       Leavesr;
                //    Endif;
                // Endif;
                // //*** End   Add *** 03/14/19 *******************************
                //Endif;
                // End   Delete 04/03/19 **************************************

             When strSeg = 'DTP';
                If FldOcur = 1;
                   Qualifier =  %Subst(FA837DATA:FrmPos:Len);
                Endif;
                ParseDTP(FA837DATA:FrmPos:Len:FldOcur:Qualifier);

             When strSeg = 'CL1';           //Inst
                ParseCL1(FA837DATA:FrmPos:Len:FldOcur);

             When strSeg = 'CN1';
                Leave;

             When strSeg = 'HI';
                HIcode =   %Subst(FA837DATA:FrmPos:Len);
                If %Subst(HIcode:3:1) = ':';  //Clear sub-delimiter if any
                   HIcode = %Subst(HIcode:1:2);
                Endif;
                ParseHI(FA837DATA:FrmPos:Len:FldOcur:HIcode);

             When strSeg = 'SV2';
                ParseSV2(FA837DATA:FrmPos:Len:FldOcur);

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
                   //Temporary Code ************** Begins
                   //*in76 = *on;
                   //Leave;
                   //Temporary Code ************** Ends

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
       Reset NM1ILcnt;                //Clear subscriber count
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
       Reset isDupClaim;
       Reset NM1PRcnt;
       Reset NM171cnt;
       Reset NM172cnt;
       Reset NM1ZZcnt;
       Reset NM177cnt;
       Reset NM182cnt;
       Reset NM1DNcnt;
       Reset NM185cnt;
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
       //   //Get Field Count
       //   FldCnt = GetFldCnt(FA837DATA:'*');
       //
       //   //Process Fields
       //   If FldCnt > *Zero;
       //      Exsr Process;
       //   Endif;
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
       //Process NM1 loops     // CONTINUE HERE DETERMIN WHICH LOOPS ARE USED
       //                         REMOVE UNUSED?

       // Set counters for entities that repeat.
       If FldOcur = 1;
          Select;
          When NM1entity = '71';
             NM171cnt = NM171cnt + 1;

          When NM1entity = '72';
             NM172cnt = NM172cnt + 1;

          When NM1entity = '77';
             NM177cnt = NM177cnt + 1;

          When NM1entity = '82';
             NM182cnt = NM182cnt + 1;

          When NM1entity = 'ZZ';
             NM1ZZcnt = NM1ZZcnt + 1;

          When NM1entity = 'DN';
             NM1DNcnt = NM1DNcnt + 1;

          When NM1entity = '85';
             NM185cnt = NM185cnt + 1;

          When NM1entity = 'PR';
             NM1PRcnt = NM1PRcnt + 1;

          When NM1entity = 'IL';
             NM1ILcnt = NM1ILcnt + 1;
          Endsl;
       Endif;


       Select;
       When NM1entity = '41';  //1000A Submitter Name    ok
          Loop = '1000A';
          Exsr NM11000A;

       When NM1entity = '40';  //1000B Receiver Name    ok
          Loop = '1000B';
          Exsr NM11000B;

       When NM1entity = '85';  //2010AA Billing Provider Name    ok
          If NM185cnt <= 1;
             Loop = '2010AA';
             Exsr NM12010AA;
          Else;
             Loop = '2330I';
          Endif;

       When NM1entity = '87';  //2010AB Pay-to Address Name     ok
          Loop = '2010AB';
          Exsr NM12010AB;

       When NM1entity = 'IL' and is2300 = *Off;  //2010BA Subscriber Name      OK
          If NM1ILcnt <= 1;
             If SBRtype = 'P' or SBRtype = *blanks;
                Loop = '2010BA';
                Exsr NM12010BA;
             Endif;
          Else;
             If is2300 = *on;
                Loop = '2330A';
             Endif;
          Endif;

       When NM1entity = 'PR';  //2010BB Payer Name         OK
          If NM1PRcnt <= 1;
             If SBRtype = 'P' or SBRtype = *blanks;
                Loop = '2010BB';
                Exsr NM12010BB;
             Endif;
          Else;
             If is2300 = *on;
                Loop = '2330B';   //OK AS PROGRAMMED.
             Endif;
          Endif;

       When NM1entity = 'QC';  //2010CA Patient Name        ok
          Loop = '2010CA';
          Exsr NM12010CA;

       When NM1entity = '71';  //2310A Attending Provider Name - Inst ok
          If NM171cnt <= 1;
             Loop = '2310A';
             Exsr NM12310A;
          Endif;

       When NM1entity = '72';  //2310B Operating Phsyician Name - Inst ok

          If is2300 = *on;        //Claim Level
             If NM172cnt <= 1;
                Loop = '2310B';   //Inst code/name(72) different from Professional(82)
                Exsr NM12310B;
             Else;
             Loop = '2330D';
             Endif;
          Endif;

          If is2400 = *on;       //Line Level
             Loop = '2420A';
             Exsr NM12420A;      //Inst modified - Differnt field names
          Endif;

       When NM1entity = 'ZZ';  //2310C Other Operating Physician Name    ok
          If is2300 = *on;     //Claim Level
             If NM1ZZcnt <= 1;
                Loop = '2310C';
                Exsr NM12310C;
             Else;
             Loop = '2330E';
             Endif;
          Endif;

          If is2400 = *on;     //Line level- Inst Only.
             Loop = '2420B';
             Exsr NM12420B;
          Endif;

       When NM1entity = '82';  //2310D Rendering Provider Name - Claim Level
          If is2300 = *on;     //Inst segment(2310D)     ok
             If NM182cnt <= 1;
                Loop = '2310D';
                Exsr NM12310D;
             Else;
                Loop = '2330G';
             Endif;
          Endif;

          If is2400 = *on;     // ok
             Loop = '2420C';  //2420C Rendering Provider Name Inst - Line level
             Exsr NM12420C;
          Endif;

       When NM1entity = '77';  //2310E - Service Facility Location Name (Inst)
          If is2300 = *on;     //ok
             If NM177cnt <= 1;
                Loop = '2310E';
                Exsr NM12310E;
             Else;
                Loop = '2330F';
             Endif;
          Endif;

       When NM1entity = 'DN';  //2310F - Referring Provider Name - Inst   ok
          If is2300 = *on;
             If NM1DNcnt <= 1;
                Loop = '2310F';
                Exsr NM12310F;
             Else;
                Loop = '2330H';
             Endif;
          Endif;

          If is2400 = *on;     //2420D - Referring Provider Name - Line Lvl
             Loop = '2420D';
             Exsr NM12420D;
          Endif;

       Endsl;


       //--------------------------------------------------------------
       // NM1 Loop 1000A - Submitter Name        ok
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
       // NM1 Loop 1000B;             ok
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
       // NM1 Loop 2010AA - Billing Provider Name       ok
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
         // Not used in institutional
         //HDbpSUFF  = %Subst(SegData:FrmPos:FldLen);
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
       // NM1 Loop 2010AB - Pay to Address Name ok
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
       // NM1 Loop 2010BA; Subscriber Name             ok
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
       // NM1 Loop 2010BB;                     ok
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
       // NM1 Loop 2010CA;                  ok
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
       // NM1 Loop 2310A;                                  ok
       //--------------------------------------------------------------
       Begsr NM12310A;
       Select;
       When FldOcur = 1;
          CLMatpIDCD = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMatpENQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMatpNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          CLMatpFNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 5;
          CLMatpMNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 6;
          CLMatpPNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 7;
          CLMatpSNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 8;
          CLMatpIDQU =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 9;
          CLMatpNMID =%Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // NM1 Loop 2310B  - Operating Physician Name          ok
       //--------------------------------------------------------------
       Begsr NM12310B;

       Select;
       When FldOcur = 1;
          CLMoppIDCD = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMoppENQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMoppNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          CLMoppFNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 5;
          CLMoppMNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 6;
          CLMoppPNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 7;
          CLMoppSNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 8;
          CLMoppIDQU =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 9;
          CLMoppNMID =%Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;
       Endsr;

       //--------------------------------------------------------------
       // NM1 Loop 2420A - Operating Physician Name
       //--------------------------------------------------------------
       Begsr NM12420A;
       Select;
       When FldOcur = 1;
          LINoppIDCD = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          LINoppENQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          LINoppNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          LINoppFNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 5;
          LINoppMNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 6;
          LINoppPNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 7;
          LINoppSNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 8;
          LINoppIDQU =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 9;
          LINoppNMID =%Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // NM1 Loop 2420B - Other Operating Physician Name
       //--------------------------------------------------------------
       Begsr NM12420B;
       Select;
       When FldOcur = 1;
          LINoopIDCD = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          LINoopENQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          LINoopNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          LINoopFNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 5;
          LINoopMNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 6;
          LINoopPNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 7;
          LINoopSNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 8;
          LINoopIDQU =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 9;
          LINoopNMID =%Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // NM1 Loop 2310C - Other Operating Physician Name        ok
       //--------------------------------------------------------------
       Begsr NM12310C;
       Select;
       When FldOcur = 1;
          CLMoopIDCD = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMoopENQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMoopNM = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          CLMoopFNM = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 5;
          CLMoopMNM = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 6;
          CLMoopPNM = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 7;
          CLMoopSNM = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 8;
          CLMoopIDQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 9;
          CLMoopNMID  = %Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // NM1 Loop 2310D - Rendering Provider Name         ok
       //--------------------------------------------------------------
       Begsr NM12310D;
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
       When FldOcur = 6;
          CLMrnpPNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 7;
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
       // NM1 Loop 2310E - Service Facility Location Name        ok
       //--------------------------------------------------------------
       Begsr NM12310E;
       Select;
       When FldOcur = 1;
          CLMsfNMID= %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMsfNMQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMsfNM =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;  //Not used
       When FldOcur = 5;  //Not used
       When FldOcur = 6;  //Not used
       When FldOcur = 7;  //Not used
       When FldOcur = 8;
          CLMsfIDQU =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 9;
          CLMsfID =%Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;
       Endsr;
       //--------------------------------------------------------------
       // NM1 Loop 2310F - Referring Provider Name - Inst  ok
       //--------------------------------------------------------------
       Begsr NM12310F;
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
       // NM1 Loop 2420C - Rendering Provider Name ok fld 10-12 wrong on mapping
       //--------------------------------------------------------------
       Begsr NM12420C;
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
       // NM1 Loop 2420D - Referring Provider Name - Inst
       //--------------------------------------------------------------
       Begsr NM12420D;
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
      * Parse PER                                               ok
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
       Endsl;

       //--------------------------------------------------------------
       // PER Loop 1000A - Submitter Name - Submitter EDI Contact Info ok
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
       // PER Loop 2010AA;                                      ok
       //--------------------------------------------------------------
       Begsr PER2010AA;
       // Not used or not mapped in specs
       Endsr;

      /End-Free
     P ParsePER        E

      *=====================================================================
      * Parse PRV                                                 ok
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
          Exsr PRV2000A;         //ok
       When Loop = '2310A ';  //2310A Attending Provider Specialty Information - Inst
          Exsr PRV2310A;
       Endsl;


       //--------------------------------------------------------------
       // PRV Loop 2000A - Situational      ok
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
       // PRV Loop 2310A - Attending Provider Specialty Information  ok
       //--------------------------------------------------------------
       Begsr PRV2310A;

       Select;
       When FldOcur = 1;
          CLMatpPSCD = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMatpPSQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMatpTXCD = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;  //Not used
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
       When Loop = '2010AA';  //Billing Provider Address     ok
          Exsr N32010AA;

       When Loop = '2010AB';  //Pay-to-Address  Address   ok
          Exsr N32010AB;

       When Loop = '2010BA';  //Subscriber Address  ok
          Exsr N32010BA;

       When Loop = '2010BB';  //Payer Address    OK
          Exsr N32010BB;

       When Loop = '2010CA';  //Patient Address         ok
          Exsr N32010CA;

       When Loop = '2310E';   //Service Facility Location Address - Inst
          Exsr N32310E;       //NOT TESTED

       Endsl;

       //--------------------------------------------------------------
       // N3 Loop 2010AA - Billing Provider Address        ok
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
       // N3 Loop 2010AB - Pay-to Address Address        ok
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
       // N3 Loop 2010BA - Subscriber Address          ok
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
       // N3 Loop 2010BB - Payer Address                      ok
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
       // N3 Loop 2010CA - Patient Address          ok
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
       // N3 Loop 2310E - Service Facility Location Address.  ok
       //--------------------------------------------------------------
       Begsr N32310E;         //NOT TESTED
       Select;
       When FldOcur = 1;
          CLMsfADR1 = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMsfADR2 = %Subst(SegData:FrmPos:FldLen);
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
       When Loop = '2010AA';  //Billing Provider City/State/Zip  ok
          Exsr N42010AA;

       When Loop = '2010AB';  //Pay-to-Address City/State/Zip    ok
          Exsr N42010AB;

       When Loop = '2010BA';  //Subscriber City/State/Zip         ok
          Exsr N42010BA;

       When Loop = '2010BB';  //Payer City/State/Zip              ok
          Exsr N42010BB;

       When Loop = '2010CA';  //Patient City/State/Zip          ok
          Exsr N42010CA;

       When Loop = '2310E';   //Service Facility Location City/State/Zip  - Inst
          Exsr N42310E;       //NOT TESTED
       Endsl;


       //--------------------------------------------------------------
       // N4 Loop 2010AA - Billing Provider City/State/Zip   ok
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
       // N4 Loop 2010AB - Pay-to Address City/State/Zip      ok
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
       // N4 Loop 2010BA - Subscriber Address City/State/Zip       ok
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
       // N4 Loop 2010BB - Payer Address City/State/Zip         ok
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
       // N4 Loop 2010CA - Patient Address City/State/Zip         ok
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
       // N4 Loop 2310E - Service Facility Location City/State/Zip ok
       //--------------------------------------------------------------
       Begsr N42310E;
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
          If BPrefCnt = *ZERO and RefQu = 'EI';
             Exsr REF2010AATAX;      //ok
          Else;
            //Not applicable - Only one ref in 2010aa inst - ok
          Endif;

       When Loop = '2010BA';     // not mapped in inst ok
          ix = 0;             //Skip
          If BPrefCnt = *ZERO;
             //Not used       //Subscriber Secondary ID
          Else;
             //Not used       //P&C Claim Number
          Endif;

       When Loop = '2010BB';    // not mapped in inst ok
          ix = 0;             //Skip
          If BPrefCnt = *ZERO;
             //Not used       //Payer Secondary ID
          Else;
             //Not used       //Billing Provider Secondary Identification
          Endif;

       When Loop = '2010CA';      //Inst uses only one ref in this segmnt ok

          If RefQu = 'Y4';                    //  ok
             Exsr REF2010CApc;
          Else;
             ix =0;                             // Only Y4 mapped/used for inst
          Endif;


       When Loop = '2310A';   // Attending Provider Secondary Id ok
          If RefQu = '0B' or RefQu = '1G' oR RefQu = 'G2' or RefQu = 'LU';
             Exsr REF2310A;
          Endif;

       When Loop = '2310B';  //Operating Physician Secondary Id ok
          If RefQu = '0B' or RefQu = '1G' oR RefQu = 'G2' or RefQu = 'LU';
             Exsr REF2310B;
          Endif;

       When Loop = '2310C'; //Other Operating Physician Secondary ID - Inst ok
          If RefQu = '0B' or RefQu = '1G' oR RefQu = 'G2' or RefQu = 'LU';
             Exsr REF2310C;
          Endif;

       When Loop = '2310D';                                // ok
          If RefQu = '0B' or RefQu = '1G' oR RefQu = 'G2' or RefQu = 'LU';
             Exsr REF2310D;
          Endif;

       When Loop = '2310E';   //Inst only                       ok
          If RefQu = '0B' or RefQu = 'G2' or RefQu = 'LU';
             Exsr REF2310E;
          Endif;

       When Loop = '2330B';   //Inst only             ok
          If RefQu = 'F8';
             Exsr REF2330B;
          Endif;

       When Loop = '2400 ';   //Inst      ok
             Exsr REF2400;

       When Loop = '2410 ';
             Exsr REF2410;

       When Loop = '2420A ';  //Not tested
             Exsr REF2420A;

       When Loop = '2420B '; //   ok
          If RefQu = '0B' or RefQu = '1G' or RefQu = 'G2' or RefQu = 'LU'; //Inst
             Exsr REF2420B;
          Endif;

       When Loop = '2420C ';  //Not tested
          If RefQu = '0B' or RefQu = '1G' or RefQu = 'G2' or RefQu = 'LU'; //Inst
             Exsr REF2420C;
          Endif;

       When Loop = '2420D ';   // Inst ok
         If RefQu = '0B' or RefQu = '1G' or RefQu = 'G2';  //Inst
            Exsr REF2420D;
         Endif;
       Endsl;


       //--------------------------------------------------------------
       // REF Loop 2010AA - Billing Provider Tax Identification   ok
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
       // REF Loop 2010CA - Patient - Property & Casualty Claim Number   ok
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
       // REF Loop 2310A - Attending Provider Secondary Identification  ok
       //--------------------------------------------------------------
       Begsr REF2310A;

       Select;
       When FldOcur = 1;
          CLMat2ndQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMat2ndID = %Subst(SegData:FrmPos:FldLen);
       Other;          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // REF Loop 2310B - Operating Physician Secondary ID   ok
       //--------------------------------------------------------------
       Begsr REF2310B;

       Select;
       When FldOcur = 1;
          CLMop2ndQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMop2ndID  = %Subst(SegData:FrmPos:FldLen);
       Other;          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // REF Loop 2310C - Other Operating Physician Secondary ID ok
       //--------------------------------------------------------------
       Begsr REF2310C;

       Select;
       When FldOcur = 1;
          CLMoop2nQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMoop2nID = %Subst(SegData:FrmPos:FldLen);
       Other;          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // REF Loop 2310D - Rendering Provider Secondary Id     ok
       //--------------------------------------------------------------
       Begsr REF2310D;

       Select;
       When FldOcur = 1;
          CLMrnp2nQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMrnp2nID = %Subst(SegData:FrmPos:FldLen);
       Other;          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // REF Loop 2310E - Service Facility Location Secondary Id   ok
       //--------------------------------------------------------------
       Begsr REF2310E;

       Select;
       When FldOcur = 1;
          CLMSFP2NDQ = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMSF2NDID = %Subst(SegData:FrmPos:FldLen);
       Other;          //Not used
       Endsl;

       Endsr;

       //--------------------------------------------------------------
       // REF Loop 2330B - Other Payer Claim Control Number      ok
       //--------------------------------------------------------------
       Begsr REF2330B;

       Select;
       When FldOcur = 1;
          CLMopccNQ = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMopccNBR = %Subst(SegData:FrmPos:FldLen);
       Other;          //Not used
       Endsl;

       Endsr;
       //-----------------------------------------------------------------
       // REF Loop 2400  -  Line Item Control Number       ok
       //-----------------------------------------------------------------
       Begsr REF2400;

       Select;
       When RefQu = '6R ' and FldOcur = 1;
          LINctlRID = %Subst(SegData:FrmPos:FldLen);
       When RefQu = '6R ' and FldOcur = 2;
          LINliCTL#  = %Subst(SegData:FrmPos:FldLen);
       Other;          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // REF Loop 2410  - Prescription or Compound Drug Assoc Number  - Inst ok
       //--------------------------------------------------------------
       Begsr REF2410;         // NOT TESTED

       Select;
       When (RefQu = 'VY' or RefQu = 'XZ') and FldOcur = 1;
          LINprRIDQU = %Subst(SegData:FrmPos:FldLen);
       When (RefQu = 'VY' or RefQu = 'XZ')  and FldOcur = 2;
          LINpr# = %Subst(SegData:FrmPos:FldLen);
       Other;          //Not used
       Endsl;
       Endsr;
       //-----------------------------------------------------------------
       // REF Loop 2420A  - Operating Physician Secondary ID       ok
       //-----------------------------------------------------------------
       Begsr REF2420A;

       Select;
       When FldOcur = 1 and
            (RefQu = '0B' or RefQu = '1G' or RefQu = 'G2' or RefQu = 'LU');
          LINop2NDQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          LINop2NDID = %Subst(SegData:FrmPos:FldLen);
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
                  LINop2INQ =%Subst(REF4:ClnFrm:REF4Len);
               When intIdx = 2;
                  LINop2ID =%Subst(REF4:ClnFrm:REF4Len);
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
                  LINop2NDQU =  %Subst(SegData:ClnFrm:REF4Len);
               Else;
                  LINop2NDQU =  *blanks;
               Endif;

            Endif;

           LINop2NDID = *blanks;
         Endif;

         //subfield - For/net loop needed.
       Other;          //Not used
       Endsl;

       Endsr;
       //-----------------------------------------------------------------
       // REF Loop 2420B  - Other Operating Physician Secondary ID ok
       //-----------------------------------------------------------------
       Begsr REF2420B;

       Select;
       When FldOcur = 1 and
          LINoop2NQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2 and
          LINoop2NID = %Subst(SegData:FrmPos:FldLen);
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
                  LINoop2IQU =%Subst(REF4:ClnFrm:REF4Len);
               When intIdx = 2;
                  LINoop2ID =%Subst(REF4:ClnFrm:REF4Len);
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
                  LINoop2IQU =  %Subst(SegData:ClnFrm:REF4Len);
               Else;
                  LINoop2IQU =  *blanks;
               Endif;

            Endif;

           LINoop2NID = *blanks;
         Endif;

         //subfield - For/net loop needed.
       Other;          //Not used
       Endsl;

       Endsr;
       //-----------------------------------------------------------------
       // REF Loop 2420C  - Rendering Provider Secondary ID    OK
       //-----------------------------------------------------------------
       Begsr REF2420C;

       Select;
       When FldOcur = 1;
          LINrn2UPQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          LINrn2UPID = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          //Not used
       When FldOcur = 4 and RefQU <> '0B' and RefQu <> '1G';
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
                  LINrnUPQU =%Subst(REF4:ClnFrm:REF4Len);
               When intIdx = 2;
                  LINrnUPID =%Subst(REF4:ClnFrm:REF4Len);
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
                  LINrnUPQU =  %Subst(SegData:ClnFrm:REF4Len);
               Else;
                  LINrnUPQU =  *blanks;
               Endif;

            Endif;

           LINrnUPID = *blanks;
         Endif;
       Other;          //Not used
       Endsl;

       Endsr;
       //-----------------------------------------------------------------
       // REF Loop 2420D  - Referring Provider Secondary ID     ok
       //-----------------------------------------------------------------
       Begsr REF2420D;         // NOT TESTED
       Select;
       When FldOcur = 1;
          LINrp2nIQ = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          LINrp2nID = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          //Not used
       When FldOcur = 4 and RefQU <> '0B' and RefQu <> '1G';
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
                  LINrp2nIDQ =%Subst(REF4:ClnFrm:REF4Len);
               When intIdx = 2;
                  LINrp2oPPI =%Subst(REF4:ClnFrm:REF4Len);
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
                  LINrp2nIDQ =  %Subst(SegData:ClnFrm:REF4Len);
               Else;
                  LINrp2nIDQ =  *blanks;
               Endif;

            Endif;

           LINrp2oPPI = *blanks;
         Endif;
       Other;          //Not used
       Endsl;
       Endsr;
      /End-Free

     P ParseREF        E
      *=====================================================================
      * Parse HL                        ok
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
      * Parse SBR segment                         ok
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
      * Parse CLM segment      ok
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
          CLMinsid  = ICHinsid;    //not mapped in inst
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
      * Parse CL1 segment               Inst only ok
      *=====================================================================
     P ParseCL1        B
     D ParseCL1        PI
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
          CLMadtyp = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMadsc =%Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMpats =%Subst(SegData:FrmPos:FldLen);
       Other;
          ix = 0;
       Endsl;
      /End-Free
     P ParseCL1        E
      *=====================================================================
      * Parse DTP
      *=====================================================================
     P ParseDTP        B
     D ParseDTP        PI
     D  segData                     500A
     D  FrmPos                        5  0
     D  FldLen                        5  0
     D  FldOcur                       5  0
     D  DTqu                          3A

      /Free
       //Process DTP loops
       Select;
       When DTqu = '096';  //2300 Loop - Discharge //Inst
          Exsr DTP096;
       When DTqu = '434';  //2300 Loop - Statement Dates //Inst
          Exsr DTP434;
       When DTqu = '435';  //2300 Loop - Admission Date/Hour //Inst
          Exsr DTP435;
       When DTqu = '050';  //2300 Loop - Repricer Received //Inst
          Exsr DTP050;
       When DTqu = '472';  //2400 Loop - Service Date //Inst
          Exsr DTP472;
       Other;
          ix = 0;         //Skip
       Endsl;

       //--------------------------------------------------------------
       // DTP096 -  Loop 2300 - Discharge Hour                ok
       //--------------------------------------------------------------
       Begsr DTP096;
       Select;
       When FldOcur = 1;
          CLMdhQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMdhDHFQ = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMdh   =%Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // DTP434 -  Loop 2300 - Statement Dates              ok
       //--------------------------------------------------------------
       Begsr DTP434;
       Select;
       When FldOcur = 1;
          CLMscftQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMscftFQ = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMscft =%Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;

       Endsr;
       //--------------------------------------------------------------
       // DTP435 -  Loop 2300 - Admission Date/Hour            ok
       //--------------------------------------------------------------
       Begsr DTP435;
       Select;
       When FldOcur = 1;
          CLMadhrQU = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          CLMadhrFQ = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          CLMaddttm = %Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;

       Endsr;

       //--------------------------------------------------------------
       // DTP050 -  Loop 2300 - Repricer Received               ok
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
       // DTP472 -  Loop 2400 - Service Date                   ok
       //--------------------------------------------------------------
       Begsr DTP472;
       Select;
       When FldOcur = 1;
          LINdtq = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 2;
          LINtfq = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 3;
          LINsvdt = %Subst(SegData:FrmPos:FldLen);
       Other;
          //Not used
       Endsl;

       Endsr;
      /End-Free
     P ParseDTP        E

      *=====================================================================
      * Parse HI - Loop 2300 - HealthCare Information  ok
      *=====================================================================
     P ParseHI         B
     D ParseHI         PI
     D  segData                     500A
     D  FrmPos                        5  0
     D  FldLen                        5  0
     D  FldOcur                       5  0
     D  HIcode                        3A
     D  ClnFrm         S              5  0 inz
     D  ClnTo          S              5  0 inz
     D  ClnIdx         S              5  0 inz
     D  HI01Len        S              5  0 inz
     D  HI01           S            165A   inz
     D  intIdx         S              5  0 inz
     D  ClnMax         S              5  0 inz


      /Free
       Select;
       When HIcode = 'ABK' or HIcode = 'BK';   //Principal Diagnosis.
          exsr HIaBK;

       When HIcode = 'ABJ' or HIcode = 'BJ';   //Admitting Diagnosis.
          exsr HIaBJ;

       When HIcode = 'APR' or HIcode = 'PR';   //Patient's reason for visit.
          exsr HIaPR;

       When HIcode = 'ABN' or HIcode = 'BN';   //External cause of injury.
          exsr HIaBN;

       When HIcode = 'DR';                     //Diagnosis Related Group Information.
          exsr HIDR;

       When HIcode = 'ABF' or HIcode = 'BF';   //Other Diagnosis Information.
          exsr HIaBF;

       When HIcode = 'BBR' or HIcode = 'BR' or //Principal Procedure Information.
            HIcode = 'CAH';
          exsr HIbBR;

       When HIcode = 'BBQ' or HIcode = 'BQ' ;  //Other Procedure Information.
          exsr HIbBQ;

       When HIcode = 'BI';                     //Occurrence Span Information.
          exsr HIbi;

       When HIcode = 'BH';                     //Occurrence Information.
          exsr HIbh;

       When HIcode = 'BE';                     //Value Information.
          exsr HIbe;

       When HIcode = 'BG';                     //Condition Information.
          exsr HIbg;

       When HIcode = 'TC';                     //Treatment Code Information.
          exsr HItc;
       Endsl;


       //--------------------------------------------------------------
       // HI-ABK-BK  Principal Diagnosis
       //--------------------------------------------------------------
       Begsr HIaBK;
         HI01 = %Subst(SegData:FrmPos:FldLen);     //check here for
         ClnMax = GetFldCnt(HI01:':');             //situation :9
         ClnTo = *Zeros;

          If ClnMax > 1 and FldOcur = 1;

             For intIdx = 1 to ClnMax;
                ClnFrm = ClnTo + 1;
                ClnIdx = ClnTo + 1;
                ClnTo = %scan(':':HI01:ClnIdx);

                If ClnTo = *Zero;
                   ClnTo = FldLen + 1;
                Endif;

                HI01Len = ClnTo - ClnFrm;

                Select;
                When intIdx = 1 ;
                   CLMprdgt= HIcode;                      //Principal Diagnosis Qualifier
                When intIdx = 2;
                   CLMprDG = %Subst(HI01:ClnFrm:HI01len); //Principal Diagnosis Code
                When intIdx = 9;
                   CLMprdpoa= %Subst(HI01:ClnFrm:HI01len);//Principal Diagnosis Present on Admission
                Other;                                    //Not used
                Endsl;

             Endfor;

          Else;
             ClnFrm = ClnTo + 1;
             ClnIdx = ClnFrm;
             If FldOcur = FldCnt;
                Clnto = %scan(' ':SegData:ClnIdx) ;
             Else;
                Clnto = %scan('*':SegData:ClnIdx) ;
             Endif;

             HI01Len = Clnto - ClnFrm;
             If FldOcur = 1;
                CLMprdgt = %Subst(SegData:ClnFrm:HI01Len);
             Endif;
          Endif;
       Endsr;
       //--------------------------------------------------------------
       // HI-ABJ-BJ  Admitting Diagnosis
       //--------------------------------------------------------------
       Begsr HIaBJ;
         HI01 = %Subst(SegData:FrmPos:FldLen);     //check here for
         ClnMax = GetFldCnt(HI01:':');             //situation :9
         ClnTo = *Zeros;

          If ClnMax > 1 and FldOcur = 1;

             For intIdx = 1 to ClnMax;
                ClnFrm = ClnTo + 1;
                ClnIdx = ClnTo + 1;
                ClnTo = %scan(':':HI01:ClnIdx);

                If ClnTo = *Zero;
                   ClnTo = FldLen + 1;
                Endif;

                HI01Len = ClnTo - ClnFrm;

                Select;
                When intIdx = 1;
                   CLMaddgt= HIcode;                      //Admitting Diagnosis Qualifier
                When intIdx = 2;
                   CLMaddg = %Subst(HI01:ClnFrm:HI01len); //Admitting Diagnosis Code
                Other;                                    //Not used
                Endsl;

             Endfor;

          Else;
             ClnFrm = ClnTo + 1;
             ClnIdx = ClnFrm;
             If FldOcur = FldCnt;
                Clnto = %scan(' ':SegData:ClnIdx) ;
             Else;
                Clnto = %scan('*':SegData:ClnIdx) ;
             Endif;

             HI01Len = Clnto - ClnFrm;
             If FldOcur = 1;
                CLMaddgt = %Subst(SegData:ClnFrm:HI01Len);
             Endif;
          Endif;
       Endsr;
       //--------------------------------------------------------------
       // HI-APR-PR  Patient's reason for visit
       //--------------------------------------------------------------
       Begsr HIaPR;
         HI01 = %Subst(SegData:FrmPos:FldLen);     //check here for
         ClnMax = GetFldCnt(HI01:':');             //situation :9
         ClnTo = *Zeros;

          If ClnMax > 1;

             For intIdx = 1 to ClnMax;
                ClnFrm = ClnTo + 1;
                ClnIdx = ClnTo + 1;
                ClnTo = %scan(':':HI01:ClnIdx);

                If ClnTo = *Zero;
                   ClnTo = FldLen + 1;
                Endif;

                HI01Len = ClnTo - ClnFrm;

                Select;
                When intIdx = 1 and FldOcur = 1;           //Patient's reason for visit Qualifier
                   CLMprvdgt= HIcode;
                When intIdx = 2 and FldOcur = 1;           //Patient's reason for visit Code
                   CLMprvrea = %Subst(HI01:ClnFrm:HI01len);
                When intIdx = 1 and FldOcur = 2;           //Patient's reason for visit Qualifier
                   CLMprvdgt2= HIcode;
                When intIdx = 1 and FldOcur = 3;           //Patient's reason for visit Qualifier
                   CLMprvdgt3= HIcode;
                Other;                                     //Not used
                Endsl;

             Endfor;

          Else;
             ClnFrm = ClnTo + 1;
             ClnIdx = ClnFrm;
             If FldOcur = FldCnt;
                Clnto = %scan(' ':SegData:ClnIdx) ;
             Else;
                Clnto = %scan('*':SegData:ClnIdx) ;
             Endif;

             HI01Len = Clnto - ClnFrm;
             Select;
             When FldOcur = 1;
                CLMprvdgt = %Subst(SegData:ClnFrm:HI01Len);
             When FldOcur = 2;
                CLMprvdgt2 = %Subst(SegData:ClnFrm:HI01Len);
             When FldOcur = 3;
                CLMprvdgt3 = %Subst(SegData:ClnFrm:HI01Len);
             Endsl;
          Endif;
       Endsr;
       //--------------------------------------------------------------
       // HI-ABN-BN  External cause of injury
       //--------------------------------------------------------------
       Begsr HIaBN;
         HI01 = %Subst(SegData:FrmPos:FldLen);
         ClnMax = GetFldCnt(HI01:':');
         ClnTo = *Zeros;

          If ClnMax > 1 and FldOcur >=1 and FldOcur <= 12;

             For intIdx = 1 to ClnMax;
                ClnFrm = ClnTo + 1;
                ClnIdx = ClnTo + 1;
                ClnTo = %scan(':':HI01:ClnIdx);

                If ClnTo = *Zero;
                   ClnTo = FldLen + 1;
                Endif;

                HI01Len = ClnTo - ClnFrm;

                Select;
                When intIdx = 1;                                //External cause of injury Qualifier
                   ecdgt(FldOcur) = HIcode;

                When intIdx = 2;                                //External cause of injury code
                   ecdg(FldOcur) = %Subst(HI01:ClnFrm:HI01len);

                When intIdx = 9;                                // ECOI Present on Admission
                   ecpoa(FldOcur)= %Subst(HI01:ClnFrm:HI01len);
                Other;                                          //Not used
                Endsl;

             Endfor;

          Else;
             ClnFrm = ClnTo + 1;
             ClnIdx = ClnFrm;
             If FldOcur = FldCnt;
                Clnto = %scan(' ':SegData:ClnIdx) ;
             Else;
                Clnto = %scan('*':SegData:ClnIdx) ;
             Endif;

             HI01Len = Clnto - ClnFrm;
             If FldOcur >=1 and FldOcur <= 12;
                ecdgt(FldOcur) = %Subst(SegData:ClnFrm:HI01Len);
             Endif;
          Endif;
       Endsr;
       //--------------------------------------------------------------
       // HI-DR Diagnosis Related Group (DRG)
       //--------------------------------------------------------------
       Begsr HIDR;
         HI01 = %Subst(SegData:FrmPos:FldLen);     //check here for
         ClnMax = GetFldCnt(HI01:':');             //situation :9
         ClnTo = *Zeros;

          If ClnMax > 1 and FldOcur = 1;

             For intIdx = 1 to ClnMax;
                ClnFrm = ClnTo + 1;
                ClnIdx = ClnTo + 1;
                ClnTo = %scan(':':HI01:ClnIdx);

                If ClnTo = *Zero;
                   ClnTo = FldLen + 1;
                Endif;

                HI01Len = ClnTo - ClnFrm;

                Select;
                When intIdx = 1;                         //Diagnosis Related Group Info Qualifier
                   CLMdrgQU= HIcode;
                When intIdx = 2;                         //Diagnosis Related Group Code
                   CLMdrg  = %Subst(HI01:ClnFrm:HI01len);
                Other;                                   //Not used
                Endsl;

             Endfor;

          Else;
             ClnFrm = ClnTo + 1;
             ClnIdx = ClnFrm;
             If FldOcur = FldCnt;
                Clnto = %scan(' ':SegData:ClnIdx) ;
             Else;
                Clnto = %scan('*':SegData:ClnIdx) ;
             Endif;

             HI01Len = Clnto - ClnFrm;
             If FldOcur = 1;
                CLMdrgQU = %Subst(SegData:ClnFrm:HI01Len);
             Endif;
          Endif;
       Endsr;
       //--------------------------------------------------------------
       // HI-ABF-BF  Other Diagnosis Information
       //--------------------------------------------------------------
       Begsr HIaBF;
         HI01 = %Subst(SegData:FrmPos:FldLen);
         ClnMax = GetFldCnt(HI01:':');
         ClnTo = *Zeros;

          If ClnMax > 1 and FldOcur >= 1 and FldOcur <= 17;

             For intIdx = 1 to ClnMax;
                ClnFrm = ClnTo + 1;
                ClnIdx = ClnTo + 1;
                ClnTo = %scan(':':HI01:ClnIdx);

                If ClnTo = *Zero;
                   ClnTo = FldLen + 1;
                Endif;

                HI01Len = ClnTo - ClnFrm;
                Select;
                When intIdx = 1;                    //Other diagnosis information (ODI) qualifier
                   dgQ(FldOcur) = HIcode;

                When intIdx = 2;                    //ODI Code
                   dg(FldOcur) = %Subst(HI01:ClnFrm:HI01len);

                When intIdx = 9;                    //ODI Present on Admission
                   dgPOA(FldOcur)= %Subst(HI01:ClnFrm:HI01len);

                Other;                              //Not used
                Endsl;

             Endfor;

          Else;
             ClnFrm = ClnTo + 1;
             ClnIdx = ClnFrm;
             If FldOcur = FldCnt;
                Clnto = %scan(' ':SegData:ClnIdx) ;
             Else;
                Clnto = %scan('*':SegData:ClnIdx) ;
             Endif;

             HI01Len = Clnto - ClnFrm;
             If  FldOcur >= 1 and FldOcur <= 17;
                dgQ(FldOcur) = %Subst(SegData:ClnFrm:HI01Len);
             Endif;
          Endif;
       Endsr;
       //--------------------------------------------------------------
       // HI-BBR-BR-CAH  Principal Procedure
       //--------------------------------------------------------------
       Begsr HIbBR;
         HI01 = %Subst(SegData:FrmPos:FldLen);
         ClnMax = GetFldCnt(HI01:':');
         ClnTo = *Zeros;

          If ClnMax > 1 and FldOcur = 1;

             For intIdx = 1 to ClnMax;
                ClnFrm = ClnTo + 1;
                ClnIdx = ClnTo + 1;
                ClnTo = %scan(':':HI01:ClnIdx);

                If ClnTo = *Zero;
                   ClnTo = FldLen + 1;
                Endif;

                HI01Len = ClnTo - ClnFrm;

                Select;
                When intIdx = 1;                             //Principal Procedure Qualifier
                   CLMprinpQ= HIcode;
                When intIdx = 2;                             //Principal Procedure Code
                   CLMprinpC = %Subst(HI01:ClnFrm:HI01len);
                When intIdx = 3;                             //Date time qualifier
                   CLMprinDTQ = %Subst(HI01:ClnFrm:HI01len);
                When intIdx = 4;                             //Date
                   CLMprinDT = %Subst(HI01:ClnFrm:HI01len);
                When intIdx = 9;                             //Principal Proc Present on Admission
                   CLMprinpoa= %Subst(HI01:ClnFrm:HI01len);
                Other;                                       //Not used
                Endsl;

             Endfor;

          Else;
             ClnFrm = ClnTo + 1;
             ClnIdx = ClnFrm;
             If FldOcur = FldCnt;
                Clnto = %scan(' ':SegData:ClnIdx) ;
             Else;
                Clnto = %scan('*':SegData:ClnIdx) ;
             Endif;

             HI01Len = Clnto - ClnFrm;
             If FldOcur = 1;
                CLMprinpQ = %Subst(SegData:ClnFrm:HI01Len);
             Endif;
          Endif;
       Endsr;
       //--------------------------------------------------------------
       // HI-BBQ-BQ  Other Procedure Information
       //--------------------------------------------------------------
       Begsr HIbBQ;
         HI01 = %Subst(SegData:FrmPos:FldLen);     //check here for
         ClnMax = GetFldCnt(HI01:':');             //situation :9
         ClnTo = *Zeros;

          If ClnMax > 1 and FldOcur >=1 and FldOcur <=12;

             For intIdx = 1 to ClnMax;
                ClnFrm = ClnTo + 1;
                ClnIdx = ClnTo + 1;
                ClnTo = %scan(':':HI01:ClnIdx);

                If ClnTo = *Zero;
                   ClnTo = FldLen + 1;
                Endif;

                HI01Len = ClnTo - ClnFrm;

                Select;
                When intIdx = 1;                    //Other Procedure Information Qualifier
                   prQ(FldOcur)  = HIcode;

                When intIdx = 2;                    //Procedure code
                   pr(FldOcur) = %Subst(HI01:ClnFrm:HI01len);

                When intIdx = 3;                    //Date Time Period Qualifier
                   prDTQ(FldOcur) = %Subst(HI01:ClnFrm:HI01len);

                When intIdx = 4;                    //Date Time Period
                   prDT(FldOcur) = %Subst(HI01:ClnFrm:HI01len);

                Other;                              //Not used
                Endsl;

             Endfor;

          Else;
             ClnFrm = ClnTo + 1;
             ClnIdx = ClnFrm;
             If FldOcur = FldCnt and  FldOcur >=1 and FldOcur <=12;
                Clnto = %scan(' ':SegData:ClnIdx) ;
             Else;
                Clnto = %scan('*':SegData:ClnIdx) ;
             Endif;

             If FldOcur >=1 and FldOcur <=12;
                HI01Len = Clnto - ClnFrm;
                prQ(FldOcur) = %Subst(SegData:ClnFrm:HI01Len);
             Endif;
          Endif;
       Endsr;
       //--------------------------------------------------------------
       // HI-BI  Occurrance Span Information
       //--------------------------------------------------------------
       Begsr HIbi;
         HI01 = %Subst(SegData:FrmPos:FldLen);
         ClnMax = GetFldCnt(HI01:':');
         ClnTo = *Zeros;

          If ClnMax > 1 and FldOcur >= 1 and FldOcur <= 12;

             For intIdx = 1 to ClnMax;
                ClnFrm = ClnTo + 1;
                ClnIdx = ClnTo + 1;
                ClnTo = %scan(':':HI01:ClnIdx);

                If ClnTo = *Zero;
                   ClnTo = FldLen + 1;
                Endif;

                HI01Len = ClnTo - ClnFrm;

                Select;
                When intIdx = 1;              //Occurrence Span Information Qualifier
                   osQU(FldOcur) = HIcode;

                When intIdx = 2;              //Occurrence Span Code
                   osCD(FldOcur) = %Subst(HI01:ClnFrm:HI01len);

                When intIdx = 3;              //Date time qualifier
                   osDTQ(FldOcur) = %Subst(HI01:ClnFrm:HI01len);

                When intIdx = 4;              //Date
                   osDTP(FldOcur) = %Subst(HI01:ClnFrm:HI01len);

                Other;                        //Not used
                Endsl;

             Endfor;

          Else;
             ClnFrm = ClnTo + 1;
             ClnIdx = ClnFrm;
             If FldOcur = FldCnt and FldOcur >=1 and FldOcur <=12;
                Clnto = %scan(' ':SegData:ClnIdx) ;
             Else;
                Clnto = %scan('*':SegData:ClnIdx) ;
             Endif;

             HI01Len = Clnto - ClnFrm;
             If FldOcur >=1 and FldOcur <=12;
                osQU(FldOcur) = %Subst(SegData:ClnFrm:HI01Len);
             Endif;
          Endif;
       Endsr;
       //--------------------------------------------------------------
       // HI-BH  Occurrance Information
       //--------------------------------------------------------------
       Begsr HIbh;
         HI01 = %Subst(SegData:FrmPos:FldLen);
         ClnMax = GetFldCnt(HI01:':');
         ClnTo = *Zeros;

          If ClnMax > 1 and FldOcur >= 1 and FldOcur <= 12;

             For intIdx = 1 to ClnMax;
                ClnFrm = ClnTo + 1;
                ClnIdx = ClnTo + 1;
                ClnTo = %scan(':':HI01:ClnIdx);

                If ClnTo = *Zero;
                   ClnTo = FldLen + 1;
                Endif;

                HI01Len = ClnTo - ClnFrm;

                Select;
                When intIdx = 1 ;                                //Occurrence Information Qualifier
                   occQU(FldOcur) = HIcode;

                When intIdx = 2;                                 //Occurrence code
                   occCD(FldOcur)  = %Subst(HI01:ClnFrm:HI01len);

                When intIdx = 3;                                 //Date Time Period Qualifier
                   occDQ(FldOcur)  = %Subst(HI01:ClnFrm:HI01len);

                When intIdx = 4;                                 //Date Time Period
                   occDT(FldOcur) = %Subst(HI01:ClnFrm:HI01len);

                Other;                                           //Not used
                Endsl;

             Endfor;

          Else;
             ClnFrm = ClnTo + 1;
             ClnIdx = ClnFrm;
             If FldOcur = FldCnt;
                Clnto = %scan(' ':SegData:ClnIdx) ;
             Else;
                Clnto = %scan('*':SegData:ClnIdx) ;
             Endif;

             If FldOcur >= 1 and FldOcur <= 12;
                HI01Len = Clnto - ClnFrm;
                occQU(FldOcur) = %Subst(SegData:ClnFrm:HI01Len);
             Endif;
          Endif;
       Endsr;
       //--------------------------------------------------------------
       // HI-BE  Value Information
       //--------------------------------------------------------------
       Begsr HIbe;
         HI01 = %Subst(SegData:FrmPos:FldLen);
         ClnMax = GetFldCnt(HI01:':');
         ClnTo = *Zeros;

          If ClnMax > 1 and FldOcur >= 1 and FldOcur <= 12;

             For intIdx = 1 to ClnMax;
                ClnFrm = ClnTo + 1;
                ClnIdx = ClnTo + 1;
                ClnTo = %scan(':':HI01:ClnIdx);

                If ClnTo = *Zero;
                   ClnTo = FldLen + 1;
                Endif;

                HI01Len = ClnTo - ClnFrm;

                Select;
                When intIdx = 1 ;                                //Value Information Qualifier
                   vcQU(FldOcur) = HIcode;

                When intIdx = 2;                                 //Value Information Code
                   vcCD(FldOcur)  = %Subst(HI01:ClnFrm:HI01len);

                When intIdx = 5;                                 //Value Informantion Code Amount
                   vca(FldOcur)  = %Subst(HI01:ClnFrm:HI01len);

                Other;                                           //Not used
                Endsl;

             Endfor;

          Else;
             ClnFrm = ClnTo + 1;
             ClnIdx = ClnFrm;
             If FldOcur = FldCnt;
                Clnto = %scan(' ':SegData:ClnIdx) ;
             Else;
                Clnto = %scan('*':SegData:ClnIdx) ;
             Endif;

             If FldOcur >= 1 and FldOcur <= 12;
                HI01Len = Clnto - ClnFrm;
                vcQU(FldOcur) = %Subst(SegData:ClnFrm:HI01Len);
             Endif;
          Endif;
       Endsr;
       //--------------------------------------------------------------
       // HI-BG  Condition Information
       //--------------------------------------------------------------
       Begsr HIbg;
         HI01 = %Subst(SegData:FrmPos:FldLen);
         ClnMax = GetFldCnt(HI01:':');
         ClnTo = *Zeros;

          If ClnMax > 1 and FldOcur >= 1 and FldOcur <= 12;

             For intIdx = 1 to ClnMax;
                ClnFrm = ClnTo + 1;
                ClnIdx = ClnTo + 1;
                ClnTo = %scan(':':HI01:ClnIdx);

                If ClnTo = *Zero;
                   ClnTo = FldLen + 1;
                Endif;

                HI01Len = ClnTo - ClnFrm;

                Select;
                When intIdx = 1 ;                           //Condition Information Qualifier
                   ccQ(FldOcur) = HIcode;
                                                            //Condition Information Code
                When intIdx = 2 and
                   ccCD(FldOcur)  = %Subst(HI01:ClnFrm:HI01len);

                Other;                                           //Not used
                Endsl;

             Endfor;

          Else;
             ClnFrm = ClnTo + 1;
             ClnIdx = ClnFrm;
             If FldOcur = FldCnt;
                Clnto = %scan(' ':SegData:ClnIdx) ;
             Else;
                Clnto = %scan('*':SegData:ClnIdx) ;
             Endif;

             If FldOcur >= 1 and FldOcur <= 12;
                HI01Len = Clnto - ClnFrm;
                ccQ(FldOcur) = %Subst(SegData:ClnFrm:HI01Len);
             Endif;
          Endif;
       Endsr;
       //--------------------------------------------------------------
       // HI-TC  Treatment Code Information
       //--------------------------------------------------------------
       Begsr HItc;
         HI01 = %Subst(SegData:FrmPos:FldLen);
         ClnMax = GetFldCnt(HI01:':');
         ClnTo = *Zeros;

          If ClnMax > 1 and FldOcur >= 1 and FldOcur <= 12;

             For intIdx = 1 to ClnMax;
                ClnFrm = ClnTo + 1;
                ClnIdx = ClnTo + 1;
                ClnTo = %scan(':':HI01:ClnIdx);

                If ClnTo = *Zero;
                   ClnTo = FldLen + 1;
                Endif;

                HI01Len = ClnTo - ClnFrm;

                Select;
                When intIdx = 1 ;                            //Condition Information Qualifier
                   tcQU(FldOcur) = HIcode;
                                                             //Condition Information Code
                When intIdx = 2 and
                   tcCD(FldOcur)  = %Subst(HI01:ClnFrm:HI01len);

                Other;                                       //Not used
                Endsl;

             Endfor;

          Else;
             ClnFrm = ClnTo + 1;
             ClnIdx = ClnFrm;
             If FldOcur = FldCnt;
                Clnto = %scan(' ':SegData:ClnIdx) ;
             Else;
                Clnto = %scan('*':SegData:ClnIdx) ;
             Endif;

             If FldOcur >= 1 and FldOcur <= 12;
                HI01Len = Clnto - ClnFrm;
                tcQU(FldOcur) = %Subst(SegData:ClnFrm:HI01Len);
             Endif;
          Endif;
       Endsr;
      /End-Free
     P ParseHI         E
      *=====================================================================
      * Parse LX  - Loop 2400 - Service Line Number - Required          ok
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
      * Parse SV2 - Loop 2400 - Institutional Service      ok
      *=====================================================================
     P ParseSV2        B
     D ParseSV2        PI
     D  segData                     500A
     D  FrmPos                        5  0
     D  FldLen                        5  0
     D  FldOcur                       5  0

     D  ClnIdx         S              5  0
     D  ClnFrm         S              5  0
     D  Clnto          S              5  0
     D  intIdx         S              5  0 inz
     D  SV202len       S              5  0 inz
     D  SV207len       S              5  0 inz
     D  ClnMax         S              5  0
     D  SV202          S            193A   inz
     D  SV207          S             11A   inz
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
         LINrvcd = %Subst(SegData:FrmPos:FldLen);

       When FldOcur = 2;
         SV202 = %Subst(SegData:FrmPos:FldLen);
         ClnMax = GetFldCnt(SV202:':');
         ClnTo = *Zeros;

         If ClnMax > *Zero;

            For intIdx = 1 to ClnMax;
               ClnFrm = ClnTo + 1;
               ClnIdx = ClnTo + 1;
               ClnTo = %scan(':':SV202:ClnIdx);

               If ClnTo = *Zero;
                  ClnTo = FldLen + 1;
               Endif;

               SV202Len = ClnTo - ClnFrm;

               Select;
               When intIdx = 1;
                  LINqu = %Subst(SV202:ClnFrm:SV202len);
               When intIdx = 2;
                  LINcode = %Subst(SV202:ClnFrm:SV202len);
               When intIdx = 3;
                  LINmod1 = %Subst(SV202:ClnFrm:SV202len);
               When intIdx = 4;
                  LINmod2 = %Subst(SV202:ClnFrm:SV202len);
               When intIdx = 5;
                  LINmod3 = %Subst(SV202:ClnFrm:SV202len);
               When intIdx = 6;
                  LINmod4 = %Subst(SV202:ClnFrm:SV202len);
               When intIdx = 7;
                  LINdesc = %Subst(SV202:ClnFrm:SV202len);
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
               SV202Len = ClnTo - ClnFrm;

               If SV202Len > *zero;
                  LINqu =  %Subst(SegData:ClnFrm:SV202Len);
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

       When FldOcur = 3;
          LINchrg = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 4;
          LINunbas = %Subst(SegData:FrmPos:FldLen);
       When FldOcur = 5;
          LINsvcunit = %Subst(SegData:FrmPos:FldLen);
       Endsl;

      /End-Free
     P ParseSV2        E
      *=====================================================================
      * Parse LIN - Loop 2410 - Drug Identification  - Situational   ok
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
      * Parse CTP - Loop 2410 - Drug Identification/Quantity - Situational
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
          When FldOcur = 5;
             LINdrgUNBS =%Subst(SegData:FrmPos:FldLen);
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
