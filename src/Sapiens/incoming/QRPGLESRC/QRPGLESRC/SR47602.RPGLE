     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
     H/TITLE SR47602 - WV - Benefits Deductible Endorsement
      *========================================================================*
      * WC470602 - WEST VIRGINIA BENEFITS DEDUCTIBLE ENDORSEMENT               *
      *                                                                        *
      * Program Description:                                                   *
      *                                                                        *
      *   Determines if this endorsement form should be attached to            *
      *   the policy and creates data in the Endorsement Form                  *
      *   Workfile (WXFMP) prior to a form merge.                              *
      *                                                                        *
      * Created   : 11/04/08                                                   *
      * Programmer: C:190                                                      *
      * Project   : WC1748                                                     *
      *========================================================================*
      * REVISIONS:                                                             *
      *
      * 10/19/22 C:342 Project: WC2834                                         *
      *                Endorsement Recognition of Cancellation                 *
      *                                                                        *
      * 02/17/22 C:62  Project: WC2846
      *                Electronic Report for WC470602
     *
     * 09/29/16 C:33  Project: SY371
      *               -Implement Scriptura.
      *               -No comments.
      *                                                                        *
      * 05/28/13 C:287 Support: WA099 80909                                    *
      *                - Add Large Deductible logic                            *
      *                                                                        *
      *========================================================================*
      * Notes:                                                                 *
      *                                                                        *
      *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).                   *
      *                                                                        *
      *  QQMODE = 1 - Called to determine if form should be attached.          *
      *  QQMODE = 2 - Called to print endorsement.                             *
      *  QQMODE = 3 - Called for additional input.                             *
      *  QQMODE = 4 - Called for mid-term processing.                          *
      *========================================================================*
      ** Copybook F specs
     F/COPY SCOPYEN01F
      *
      ** Deductible Discount Table File
     FWTDDL2    IF   E           K DISK
     Fwtabl     if   e           k disk    include(pbs01)
     Fwmldl1    if   e           k disk
     F*** Begin Add *** 02/17/21 *******************************************
     Fwxpel     uf a e           k disk    Rename(ppe01:ppe00)
     Fwxpel1    if   e           k disk
     Fststl3    if   e           k disk    Rename(pst01:pst03)
     F*** End   Add *** 02/17/21 *******************************************
      *
      ** Copybook D specs
     D/COPY SCOPYEN01D
     d*** Begin Add *** 02/17/22 ***************************************
      //***** Beg Del *** 10/19/22 **********************************
     D*****/copy wprEmplr
      //***** End Del *** 10/19/22 **********************************
     D/copy WWCPolDat4
     d*** End   Add *** 02/17/22 ***************************************
      *=========================================================================
      *  Arrays
      *=========================================================================
     D ST              S              2    DIM(60)                              States found
      *=========================================================================
      *  /COPY required constants
      *=========================================================================
      ** Form ID
     D qqForm          s             15a   inz('WC470602  ')
      ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
     D*** Begin Add *** 02/17/22 *******************************************
     D outFlag         s              1    Inz
     D xxUnID          s                   like(peunid)
     D x1End#          s                   like(peend#)
     D x1Type          s              1    Inz('P')
     d xxPremPct       s             12  9 inz(0)
     d xxLossSub       s              2  0 inz(0)
     d xxBasisDed      s              2  0 inz(0)
     d xxDedPct        s              2  0 inz(0)
     d xxDedAmt        s              9  0 inz(0)
     d xxDedAgg        s              9  0 inz(0)
     d xxDedClas       s              5  0 inz(0)
     d xxDedNet        s              1    inz(' ')
     d xxDedVerb       s              1    inz(' ')
     d xxPlan          s              1    inz(' ')
     D*** End   Add *** 02/17/22 *******************************************
     D xxampo          s                   like(FMAM01)
     D xxamag          s                   like(FMAM01)
     D xxDedSize       s              1
      *=========================================================================
      *  Prototypes
      *=========================================================================
     D rtvDedSize      pr             1
     D  co#                                like(ELCO#)
     D  bscd                               like(ELBSCD)
     D rtvSmDedInfo    pr
     D  ampo                               like(FMAM01)
     D  amag                               like(FMAM01)
     D rtvLgDedInfo    pr
     D  ampo                               like(FMAM01)
     D  amag                               like(FMAM01)
     D*** Begin Add *** 02/17/22 *******************************************
    D SrChkPgm        pr                  ExtPgm('SRCHKPGM')
     D  inProgram                    10    Const
     D  outFlag                       1
     D*** End   Add *** 02/17/22 *******************************************
      *=========================================================================
      *  *LIKE DEFN definitions
      *=========================================================================
      *
      *=========================================================================
      *  Data structures and constants
      *=========================================================================
      *
      ** Deductible per occurance amount
     D                 DS
     D  DA                     1    110  2
     D                                     DIM(10)                              Deductible amount
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
      *
      ** States to print
     D                 DS
     D  SP                     1    100
     D                                     DIM(10)                              States to print
     D  FMC101                 1     10
     D  FMC102                11     20
     D  FMC103                21     30
     D  FMC104                31     40
     D  FMC105                41     50
     D  FMC106                51     60
     D  FMC107                61     70
     D  FMC108                71     80
     D  FMC109                81     90
     D  FMC110                91    100

     D                 DS
     D  SP52                   1    520
     D                                     DIM(52)                              States to print
     D  @@C101                 1     10
     D  @@C102                11     20
     D  @@C103                21     30
     D  @@C104                31     40
     D  @@C105                41     50
     D  @@C106                51     60
     D  @@C107                61     70
     D  @@C108                71     80
     D  @@C109                81     90
     D  @@C110                91    100
     D  @@C111               101    110
     D  @@C112               111    120
     D  @@C113               121    130
     D  @@C114               131    140
     D  @@C115               141    150
     D  @@C116               151    160
     D  @@C117               161    170
     D  @@C118               171    180
     D  @@C119               181    190
     D  @@C120               191    200
     D  @@C121               201    210
     D  @@C122               211    220
     D  @@C123               221    230
     D  @@C124               231    240
     D  @@C125               241    250
     D  @@C126               251    260
     D  @@C127               261    270
     D  @@C128               271    280
     D  @@C129               281    290
     D  @@C130               291    300
     D  @@C131               301    310
     D  @@C132               311    320
     D  @@C133               321    330
     D  @@C134               331    340
     D  @@C135               341    350
     D  @@C136               351    360
     D  @@C137               361    370
     D  @@C138               371    380
     D  @@C139               381    390
     D  @@C140               391    400
     D  @@C141               401    410
     D  @@C142               411    420
     D  @@C143               421    430
     D  @@C144               431    440
     D  @@C145               441    450
     D  @@C146               451    460
     D  @@C147               461    470
     D  @@C148               471    480
     D  @@C149               481    490
     D  @@C150               491    500
     D  @@C151               501    510
     D  @@C152               511    520
     D*
     D                 DS
     D  DA52                   1    572  2
     D                                     DIM(52)                              States to print
     D  @@AM01                 1     11  2
     D  @@AM02                12     22  2
     D  @@AM03                23     33  2
     D  @@AM04                34     44  2
     D  @@AM05                45     55  2
     D  @@AM06                56     66  2
     D  @@AM07                67     77  2
     D  @@AM08                78     88  2
     D  @@AM09                89     99  2
     D  @@AM10               100    110  2
     D  @@AM11               111    121  2
     D  @@AM12               122    132  2
     D  @@AM13               133    143  2
     D  @@AM14               144    154  2
     D  @@AM15               155    165  2
     D  @@AM16               166    176  2
     D  @@AM17               177    187  2
     D  @@AM18               188    198  2
     D  @@AM19               199    209  2
     D  @@AM20               210    220  2
     D  @@AM21               221    231  2
     D  @@AM22               232    242  2
     D  @@AM23               243    253  2
     D  @@AM24               254    264  2
     D  @@AM25               265    275  2
     D  @@AM26               276    286  2
     D  @@AM27               287    297  2
     D  @@AM28               298    308  2
     D  @@AM29               309    319  2
     D  @@AM30               320    330  2
     D  @@AM31               331    341  2
     D  @@AM32               342    352  2
     D  @@AM33               353    363  2
     D  @@AM34               364    374  2
     D  @@AM35               375    385  2
     D  @@AM36               386    396  2
     D  @@AM37               397    407  2
     D  @@AM38               408    418  2
     D  @@AM39               419    429  2
     D  @@AM40               430    440  2
     D  @@AM41               441    451  2
     D  @@AM42               452    462  2
     D  @@AM43               463    473  2
     D  @@AM44               474    484  2
     D  @@AM45               485    495  2
     D  @@AM46               496    506  2
     D  @@AM47               507    517  2
     D  @@AM48               518    528  2
     D  @@AM49               529    539  2
     D  @@AM50               540    550  2
     D  @@AM51               551    561  2
     D  @@AM52               562    572  2
      * counters
     d x1              s              3  0
     d y1              s              3  0
    
      * temp
     d stsav           s                   like(fmc101)
     d rtsav           s                   like(fmam01)
     d sttmp           s                   like(fmc101)
     d rttmp           s                   like(fmam01)

      *entry parameters
     D SR47602         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR47602         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
    
      *=========================================================================
      *  KLIST definitions
      *=========================================================================
      *
      ** Deductible Discount Code file
     C     DDKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELDCDE
     C                   KFLD                    XXDT              7 0
      *
     C     DDKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELDCDE
      *=========================================================================
      *  Main
      *=========================================================================
      ** Copybook C specs (KLISTS and mainline)
     C/COPY SCOPYEN01C
      *
      *=========================================================================
      *  CHKST - Check if State already printed in endorsement schedule
      *=========================================================================
     C     CHKST         BEGSR
      *
     C                   MOVE      'N'           XXADD             1
     C                   Z-ADD     1             I
      *
     C     I             DOWLT     60
      *
     C                   SELECT
      *
     C     ST(I)         WHENEQ    *BLANK
     C                   MOVEL     RSJRST        ST(I)
     C                   MOVE      'Y'           XXADD
     C                   LEAVE
      *
     C     ST(I)         WHENEQ    RSJRST

     C                   ENDSL
      *
     C                   ADD       1             I
     C                   ENDDO
      *
     C                   ENDSR                                                  CHKST
      *
      *=========================================================================
      *  OTHINF - Subroutine to load form specific information
      *=========================================================================
     C     OTHINF        BEGSR
      /Free
          WriteInCopybook = *Off;
      /End-Free
      *
     C                   CLEAR                   ST
     C                   Z-ADD     *ZEROS        DA
     C                   Z-ADD     0             IX                2 0
      *
     C     ELKY2         CHAIN     PEL02                              61
      *
     C     *IN61         DOWEQ     *OFF
           //***** Beg Add *** 10/19/22 ******************************
           xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
           If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
           //***** End Add *** 10/19/22 ******************************
      *
      ** Get the Jurisdiction State
     C     RSKY          CHAIN     PRS01                              60
      *
      ** Get the current Deductible Discount record
      *
     C                   Eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     C                   Select
     C                   When      xxDedSize = 'L'
     C                   Callp     rtvLgDedInfo (xxAmpo: xxAmag)
     C                   When      xxDedSize = 'S'
     C                   Callp     rtvSmDedInfo (xxAmpo: xxAmag)
     C                   EndSl

     C                   If        xxAmpo <> 0
      *
      ** If the state hasn't been found in this employer before
      ** then add it to the array and write a WXFMP record.
      *
     C                   EXSR      CHKST
      *
     C     XXADD         IFEQ      'Y'
      *
      ** Increment counter
     C                   ADD       1             IX
     C                   ADD       1             IY                2 0
      *
      ** Move state into state array.
     C                   MOVEL     RSJRST        STSAV
     C                   Eval      RTSAV = xxAmpo
     c                   exsr      srins
      //*** Begin Add *** 02/17/22 **********************************
          If RSjrSt = 'WV';
             Exsr WriteWCPOLS;
          EndIf;
      //*** End Add ***** 02/17/22 **********************************
      *
     C                   ENDIF                                                  *IN30 = *OFF
      *
     C                   ENDIF
      *
      ** Get the next division.
           //***** Beg Add *** 10/19/22 ******************************
           Endif;
           //***** End Add *** 10/19/22 ******************************
     C     ELKY2         READE     PEL02                                  61
     C                   ENDDO                                                  *IN60 = *OFF
     C*
      ** If page is full, write & setup for more names
     c                   z-add     0             y1
     c                   z-add     0             x1
      *
     C                   DO        52
     c                   add       1             y1
     c                   add       1             x1
     c
     c                   if        sp52(y1) = *blanks
     c                   leave
     c                   endif
     c
     C                   MOVE      SP52(y1)      SP(x1)
     C                   MOVE      DA52(y1)      DA(x1)
     c
     C                   if        x1=10
      /Free
          //* Write the output record
          Exsr WriteWorkFile;
      /End-Free
     C                   MOVEA     *ZEROS        DA
     C                   MOVEA     *BLANKS       SP
     c                   z-add     0             x1
     C                   endif                                                  x1<= 10
     c                   enddo
      ** Write the output record
     C                   if        x1 <> 0
      /Free
          //* Write the output record
          Exsr WriteWorkFile;
      /End-Free
     C                   ENDIF                                                  x1 <> 0
      *
     C                   ENDSR
      *
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     PGMSTR        BEGSR
     C                   ENDSR                                                  PGMSTR
      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND

      *=========================================================================
      *  srins - Insertion sort
      *=========================================================================
     C     srins         begsr
    
     c                   z-add     1             x1
    
     c                   z-add     1             y1
     C     y1            Dowle     52
      *
     c                   select
      *
     C     sp52(y1)      wheneq    *blanks
     c                   movel(p)  stsav         sp52(y1)
     c                   z-add     rtsav         da52(y1)
     c                   leave
      *
     C     stsav         whengt    sp52(y1)
     c                   add       1             y1
     c                   iter
      *
     C     stsav         wheneq    sp52(y1)
     c     rtsav         ifgt      da52(y1)
     c                   z-add     rtsav         da52(y1)
     c                   endif
     c                   leave
      *
     C                   other
     c                   movel(p)  sp52(y1)      sttmp
     c                   z-add     da52(y1)      rttmp
     c                   movel(p)  stsav         sp52(y1)
     c                   z-add     rtsav         da52(y1)
     c                   movel(p)  sttmp         stsav
     c                   z-add     rttmp         rtsav
     c                   add       1             y1
     c                   iter
      *
     c                   endsl
      *
     C                   EndDo
    
     C                   endsr
    
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     C     VALID         BEGSR
      *
      ** Get the current Deductible Discount record
    
     C                   Eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     C                   If        ENST = RSJRST
     C                   Select
     C                   When      xxDedSize = 'L'
     C                   Callp     rtvLgDedInfo (xxAmpo: xxAmag)
     C                   When      xxDedSize = 'S'
     C                   Callp     rtvSmDedInfo (xxAmpo: xxAmag)
     C                   Endsl
     C                   If        xxAmpo <> 0
     C                   MOVE      *ON           CRTFRM
     C                   EndIf                                                  xxAmpo <> 0

     C                   ENDIF                                                  VALID STATE
      *
     C                   ENDSR                                                  VALID
      /free
       //*** Begin Add *** 02/17/22 *****************************************
       //=======================================================================
       //  WriteWCPOLS - record information for electronic data record
       //=======================================================================
       Begsr WriteWCPOLS;

         //  If printing a policy page.  (Not called from W1054R)
         outFlag = ' ';
         SrChkPgm('W1054R':outFlag);

         If outFlag = '0';
            Clear xxPremPct;
            Clear xxLossSub;
            Clear xxBasisDed;
            Clear xxDedPct;
            Clear xxDedAmt;
            Clear xxDedAgg;
            Clear xxDedClas;
            Clear xxDedNet;
            Clear xxDedVerb;
            Clear xxPlan;

            xxPlan = RtvDeductible(elco#:elfnd:elemp#:eldiv:elfyr:xxPremPct:
             xxDedAmt:xxDedAgg:xxLossSub:xxBasisDed:xxDedNet:xxDedVerb:
             xxDedClas:xxDedPct);

            // Find the unique ID number.
            x1Type = 'P';
            xxUnID = 0;

            SetGt  (xxCo#:xxFnd:xxEmp#:xxFyr:*hival) ppe01;
            ReadPE (xxCo#:xxFnd:xxEmp#:xxFyr)        ppe01;
            If Not %Eof(wxpel1);
               xxUnID = PEunID;
            EndIf;

            Chain (xxCo#:xxFnd:xxEmp#:xxFyr:xxUnID:xxForm) ppe00;
            If %Found(wxpel);

               // Copy data into the record.
               Chain (RSjrSt) pst03;
               If %Found(ststl3);
                  Monitor;
                     PEst = %Dec(STnSt:2:0);
                  On-Error;
                     PEst = 0;
                  EndMon;
               EndIf;

               Clear E00403;

               xxDAC = xxDedAmt;
               xxPrP = xxPremPct;
               xxLSd = xxLossSub;
               xxBDc = xxBasisDed;
               xxDPc = xxDedPct;
               xxDAG = xxDedAgg;

               PEdata = ' ';
               PEdata = E00403;
               PEuser = Q1user;
               PEpgm  = Q1pgm;
               PEdt   = %Dec(%Date():*CYMD);
               PEtime = %Dec(%Time():*HMS);

               Update ppe00;
            EndIf;
         EndIf;

       EndSr;
       //*** End   Add *** 02/17/22 *****************************************
      /end-free

      *=========================================================================
      * Query whether policy is small or large deductible
      *=========================================================================
     P rtvDedSize      b

     D rtvDedSize      pi             1                                         return S/L
     D  co#                                like(ELCO#)
     D  bscd                               like(ELBSCD)

     D kkco#           s                   like(BSCO#)
     D kkkey           s                   like(BSKEY)

     C     bsky          Klist
     C                   Kfld                    kkco#
     C                   Kfld                    kkkey

     C     rsky          Chain     prs01
     C                   If        not(%found)
     C                   Eval      RSJRST = *blank
     C                   Return    ' '
     C                   Endif

     C                   Eval      kkco# = co#
     C                   Movel(p)  bscd          kkkey
     C     bsky          Chain     pbs01
     C                   Select
     C                   When      BSLPGM = 'L'
     C                   Return    'L'
     C                   Other
     C                   Return    'S'
     C                   Endsl

     P rtvDedSize      e

      *=========================================================================
      * rtvSmDedInfo  - retrieves DDAMPO and DDAMAG
      *=========================================================================
     P rtvSmDedInfo    b
     D rtvSmDedInfo    pi
     D  ampo                               like(FMAM01)
     D  amag                               like(FMAM01)
     D  kkefdt         s                   like(DDEFDT)

     C     ddky1         Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    RSJRST
     C                   Kfld                    ELDCDE
     C                   Kfld                    kkefdt

     C     ddky          Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    RSJRST
     C                   Kfld                    ELDCDE

     C                   Move      ELCO#         qqco#
     C                   Move      ELFND         qqfnd
     C                   Move      ELEMP#        q1emp#
     C                   Move      ELDIV         q1div
     C                   Move      ELFYR         q1fyr
     C                   Move      ELDCEF        q2date

     C                   Call      'SREFFD'
     C                   Parm                    qqco#             3
     C                   Parm                    qqfnd             3
     C                   Parm                    q1fyr             3
     C                   Parm                    q1emp#            9
     C                   Parm                    q1div             5
     C                   Parm                    q2date            7

     C                   Move      q2date        kkefdt
     C     ddky1         Setgt     pdd01
     C     ddky          Readpe    pdd01
     C                   Dow       not(%eof) and DDSTAT <> 'A'
     C     ddky          Readpe    pdd01
     C                   Enddo

     C                   If        not(%eof)
     C                   Eval      ampo = DDAMPO
     C                   Eval      amag = DDAMAG
     C                   Endif

     P rtvSmDedInfo    e
      *=========================================================================
      * rtvLgDedInfo - retrieves LDAMPO and LDAMAG
      *=========================================================================
     P rtvLgDedInfo    b
     D rtvLgDedInfo    pi
     D  ampo                               like(FMAM01)
     D  amag                               like(FMAM01)

     C     elky          Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    ELEMP#
     C                   Kfld                    ELDIV
     C                   Kfld                    ELFYR

     C     elky          Chain     pld01
     C                   If        %found
     C                   Eval      ampo = LDAMP1
     C                   Eval      amag = LDAMG1
     C                   Endif

     P rtvLgDedInfo    e
