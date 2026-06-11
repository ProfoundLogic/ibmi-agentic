     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H Bnddir('STBNDDIR')
     H/TITLE F8191r
      //*************************************************************************
      //  Description:  837 Bill Import
      //
      //  Created: 09/27/17   Programmer C:180      Project: 114MJB
      //*************************************************************************
      //  Revisions:
      //
     FFA837FP   IF   E           K DISK    INFDS(DBFDS837)
     FFTER997L1 IF   E           K DISK
     FFAICHL    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
      *
      *=========================================================================
      *  *ENTRY
      *=========================================================================
     D F8191R          PR
     D                                1A
     D F8191R          PI
     D  @@RetCod                      1A

      *=========================================================================
      *  Prototypes
      *=========================================================================
     D GetSegment      PR             3
     D    SegData                   500
      *=========================================================================
      *  Data Structures
      *=========================================================================
     D DBFDS837        DS
     D  DBRRN837             397    400B 0
      *=========================================================================
      *  Stand alone
      *=========================================================================
     D kkErSeg         S              3A   INZ
     D kkErCd          S              3A   INZ
     D PrvSeg          S              3A   INZ
     D strSeg          S              3A   INZ
     D strIntChgCtl#   S              9A   INZ
     D intE            S              5  0 INZ
     D intISAc         S              5  0 INZ
     D intIEAc         S              5  0 INZ
     D intGSc          S              5  0 INZ
     D intGEc          S              5  0 INZ
     D intSTc          S              5  0 INZ
     D intSEc          S              5  0 INZ
     D intRecCnt       S              5  0 INZ
     D idx             S              5  0 INZ
     D i               S              5  0 INZ
     D FrmPos          S              5  0 INZ(91)
     D FldLen          S              5  0 INZ(9)
     D Time6           S               T   INZ
     D ##RetCod        S              1A   INZ('0')
     D SegSynch        S              7A   INZ
     D SegData         S                   LIKE(FA837DATA)

     D SegAry          S              3    DIM(6)CTDATA
     D Seg1Ary         S              3    DIM(6)CTDATA
     D Seg2Ary         S              3    DIM(6)CTDATA
     D Key1Ary         S              3    DIM(999) inz
     D Key2Ary         S              3    DIM(999) inz
     D MsgAry          S             60    DIM(999) inz
     D RrnAry          S              9  0 DIM(999) inz
     D DtaAry          S             25    DIM(999) inz

      *=========================================================================
      *  Indicators
      *=========================================================================
     D isISA           S               N   INZ(*OFF)
     D isGS            S               N   INZ(*OFF)
     D isGE            S               N   INZ(*OFF)
     D isST            S               N   INZ(*OFF)
     D isSE            S               N   INZ(*OFF)
     D isIEA           S               N   INZ(*OFF)

      *=========================================================================
      *  Constants
      *=========================================================================
     D ErrMsg1         C                   CONST(' mandatory segments -
     D                                     out of synch.')

     D ErrMsg2         C                   CONST('No records processed.')

     D ErrMsg3         C                   CONST('Duplicate File. No records -
     D                                     processed.')


      *=========================================================================
      *  Main
      *=========================================================================
      /Free
       // Validate for errors
       Read FA837FP;
       StrSeg = GetSegment(FA837DATA);

       If StrSeg <> 'ISA';
          intE = %lookup(strSeg:SegAry);

          If intE = *Zero;
             kkErSeg = 'AK3';
             kkErCd = '1  ';
          Else;
             kkErSeg = 'AK3';
             kkErCd = '7  ';
          Endif;

          Exsr GetErrMsg;
       Else;
          strIntChgCtl# = %Subst(FA837Data:FrmPos:FldLen);
          Setll strIntChgCtl# FAICHL;
          If %equal(FAICHL);
             *in90 = *on;
          Else;
             *in90 = *off;
          Endif;
       Endif;

       PrvSeg = strSeg;

       Dow not %Eof(FA837FP) and *In90 = *Off;
          intRecCnt = intRecCnt + 1;
          StrSeg = GetSegment(FA837DATA);

          // Find Segment
          intE = %lookup(strSeg:SegAry);
          If intE > *zero;

             Select;
             When strSeg = 'ISA';
                isISA = *On;
                intISAc = intISAc + 1;

             When strSeg = 'IEA';
                isIEA = *On;
                intIEAc = intIEAc + 1;

             When strSeg = 'GE';
                isGE = *On;
                intGEc = intGEc + 1;

             When strSeg = 'GS';
                isGS = *On;
                intGSc = intGSc + 1;

             When strSeg = 'SE';
                isSE = *On;
                intSEc = intSEc + 1;

             When strSeg = 'ST';
                isST = *On;
                intSTc = intSTc + 1;
             Endsl;

             // Check Segment sequence
             Select;

             When Seg1Ary(IntE) = PrvSeg or Seg1Ary(IntE) = 'XXX';

             When Seg2Ary(IntE) = PrvSeg or SEg2Ary(IntE) = 'XXX';

             Other;
             //Out of sequence
                kkErSeg = 'AK3';
                kkErCd = '7  ';
                Exsr GetErrMsg;
             Endsl;

          PrvSeg = strSeg;
          Endif;
       Read FA837FP;
       Enddo;

       // Mandatory control segment is missing

       If IntRecCnt > *Zero;
          kkErSeg = 'AK3';
          kkErCd = '3  ';
          SegSynch = *blanks;

          If isISA = *Off;
             strSeg = 'ISA';
             Exsr GetErrMsg;
          Endif;

          If isGS = *Off;
             strSeg = 'GS';
             Exsr GetErrMsg;
          Endif;

          If isST = *Off;
             strSeg = 'ST';
             Exsr GetErrMsg;
          Endif;

          // Mandatory trailer segment is missing
          If isIEA = *Off;
             strSeg = 'IEA';
             Exsr GetErrMsg;
          Endif;

          If isGE = *Off;
             strSeg = 'GE';
             Exsr GetErrMsg;
          Endif;

          If isSE = *Off;
             strSeg = 'SE';
             Exsr GetErrMsg;
          Endif;


          //Segments out of synch
          kkErSeg = *blanks;
          kkErCd =  *blanks;
          SegSynch = *blanks;

          If intISAc <> intIEAc;
             SegSynch = 'ISA:IEA';
             Exsr GetErrMsg;
          Endif;


          If intGSc <> intGEc;
             SegSynch = 'GS:GE';
             Exsr GetErrMsg;
          Endif;


          If intSTc <> intSEc;
             SegSynch = 'ST:SE';
             Exsr GetErrMsg;
          Endif;

       Endif;


       //No records processed
       kkErSeg = *blanks;
       kkErCd =  *blanks;
       SegSynch = *blanks;

       If intRecCnt = *Zero;
          Exsr GetErrMsg;
       Endif;

       //Print Error report
       If idx > 0;
          ##RetCod =  '1';

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
          EndFor;
       Else;
          Except NoErr;
       Endif;
       @@RetCod = ##RetCod;
       *inlr = *on;
       //=========================================================================
       //  Get Error Message
       //=========================================================================
       Begsr GetErrMsg;

       //File Messages
       If kkErSeg <> *blanks;
          Setgt (kkErSeg:kkErCd) FTER997L1;
          Readpe (kkErSeg:kkErCd) FTER997L1;
          Idx = Idx + 1;
          Key1Ary(Idx) = kkErSeg;
          Key2Ary(Idx) = kkErCd;
          If not %eof(FTER997L1);
             MsgAry(Idx) = %trim(%subst(ER997Dsc:1:56)) + ':' + strSeg;
          Else;
             MsgAry(Idx) = 'Error message not found:' + kkErSeg + '-' + kkErCd;
          Endif;

          If not %eof(FA837FP);
             RrnAry(Idx) = DBrrn837;
             DtaAry(Idx) = %Subst(FA837DATA:1:25);
          Else;
             RrnAry(Idx) = *Zeros;
             DtaAry(Idx) = *Blanks;
          Endif;
       Else;
       //Constant Messages
          Select;
          When SegSynch <> *blanks;
             Idx = Idx + 1;
             Key1Ary(Idx) = *blanks;
             Key2Ary(Idx) = *blanks;
             MsgAry(Idx) =  %Trim(SegSynch) + ErrMsg1;
             RrnAry(Idx) = *Zeros;
             DtaAry(Idx) = *blanks;

          When intRecCnt = *zeros and *in90 = *On;
             idx = idx + 1;
             Key1Ary(Idx) = *blanks;
             Key2Ary(Idx) = *blanks;
             MsgAry(Idx) = ErrMsg3;
             RrnAry(Idx) = *Zeros;
             DtaAry(Idx) = *blanks;

          When intRecCnt = *zeros;
             idx = idx + 1;
             Key1Ary(Idx) = *blanks;
             Key2Ary(Idx) = *blanks;
             MsgAry(Idx) = ErrMsg2;
             RrnAry(Idx) = *Zeros;
             DtaAry(Idx) = *blanks;
          Endsl;
       Endif;
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
     O                                           82 '837 Bill Import Structural'
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
     O                       key1Ary(i)           3
     O                       key2Ary(i)          12
     O                       MsgAry(i)           81
     O               74      RrnAry(i)     2     97
     O               74      DtaAry(i)          126
      *
     O          E            NOERR       3
     O                                           74 '*** NO STRUCTURAL ERRORS '
     O                                         +000 'FOUND ***'
      *=====================================================================
      * Get Segment
      *=====================================================================
     P GetSegment      B
     D GetSegment      PI             3A
     D  segData                     500

     D  strSeg         S              3A

      /Free
       strSeg = %Subst(segData:1:3);

       If %Subst(strSeg:3:1) = '*';
          strSeg = %Subst(strSeg:1:2) ;
       Endif;

       Return strSeg;
      /End-Free

     P GetSegment      E

** Required Segments
GE
GS
IEA
ISA
SE
ST
** Previous Segment 1
SE
ISA
GE
XXX
ST
SE
** Previous Segment 2
SE
GE
GE
XXX
ST
GS
