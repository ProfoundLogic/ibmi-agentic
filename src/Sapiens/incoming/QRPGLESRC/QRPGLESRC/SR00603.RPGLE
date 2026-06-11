     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR00603 - LOAD ENDORSEMENT FORM WORKFILE FOR FORM WC000603

      //===============================================================*
      // WC000603 - Benefits Deductible Endorsement                    *
      //                                                               *
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      //===============================================================*
      // REVISIONS:                                                    *
      //                                                               *
      // 11/03/23 C:079 Project: 150/114373                            *
      //                Use included/excluded states logic             *
      //                                                               *
      // 10/27/22 C:333 Project: WC2834                                *
      //                ENDORSEMENT RECOGNITION OF CANCELLATION        *
      *                                                                *
      * 05/30/18 C:65  Project: WC2607                                 *
      *                REPORT DATA TO WCPOLS VIA WXPEP FILE FOR RECORD *
      //               TYPE 43.                                        *
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      //                                                               *
      //===============================================================*
      // Notes:                                                        *
      //                                                               *
      //  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      //                                                               *
      //  QQMODE = 1 - Called to determine if form should be attached. *
      //  QQMODE = 2 - Called to print endorsement.                    *
      //  QQMODE = 3 - Called for additional input.                    *
      //  QQMODE = 4 - Called for mid-term processing.                 *
      //===============================================================*

      /COPY SCOPYEN01F

     FWTDDL2    IF   E           K DISK
     Fwtabl     if   e           k disk    include(pbs01)
     Fwmldl1    if   e           k disk
     Fwxpel     uf a e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)
     Fststl3    if   e           k disk
     D ST              S              2    DIM(60)
     D qqForm          S             15A   Inz('WC000603  ')
     D ADDPGM          C                   CONST('SRXXXXXXXX')

     D xxampo          s                   like(FMAM01)
     D xxamag          s                   like(FMAM01)
     D xxDedSize       s              1
     D xxunid          s                   like(peunid)
     D x1end#          s                   like(peend#)
     d xxpremPct       s             12  9 inz(0)
     d xxlossSub       s              2  0 inz(0)
     d xxbasisDed      s              2  0 inz(0)
     d xxdedPct        s              2  0 inz(0)
     d xxdedAmt        s              9  0 inz(0)
     d xxdedAgg        s              9  0 inz(0)
     d xxdedClas       s              5  0 inz(0)
     d xxdedNet        s              1    inz(' ')
     d xxdedVerb       s              1    inz(' ')
     d xxPlan          s              1    inz(' ')

     D rtvDedSize      pr             1
     D  co#                                like(ELCO#)
     D  bscd                               like(ELBSCD)
     D rtvSmDedInfo    pr
     D  ampo                               like(FMAM01)
     D  amag                               like(FMAM01)
     D rtvLgDedInfo    pr
     D  ampo                               like(FMAM01)
     D  amag                               like(FMAM01)

     D SR00603         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR00603         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

     D                 DS
     D  DA                     1     44  2
     D                                     DIM(4)
     D  FMAM01                 1     11  2
     D  FMAM02                12     22  2
     D  FMAM03                23     33  2
     D  FMAM04                34     44  2
      ** States to print
     D                 DS
     D  SP                     1     40
     D                                     DIM(4)
     D  FMC101                 1     10
     D  FMC102                11     20
     D  FMC103                21     30
     D  FMC104                31     40

      /COPY SCOPYEN01D
     D/COPY WWCPOLDAT4

     C     stky          KList
     C                   KFld                    xxkey            10
     C     pekey         KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxdt              7 0
     C     pekey1        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C     pekey2        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxunid
     C                   KFld                    x1end#

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
      /COPY SCOPYEN01C
      *=========================================================================
      *  CHKST - Check if State already printed in endorsement schedule
      *=========================================================================
     C     CHKST         BEGSR

     C                   MOVE      'N'           XXADD             1
     C                   Z-ADD     1             I

     C     I             DOWLT     60

     C                   SELECT

     C     ST(I)         WHENEQ    *BLANK
     C                   MOVEL     RSJRST        ST(I)
     C                   MOVE      'Y'           XXADD
     C                   LEAVE

     C     ST(I)         WHENEQ    RSJRST
     C                   LEAVE

     C                   ENDSL

     C                   ADD       1             I
     C                   ENDDO

     C                   ENDSR                                                  CHKST
      *=========================================================================
      *  OTHINF - Subroutine to load form specific information
      *=========================================================================
     C     OTHINF        BEGSR

     C                   CLEAR                   ST
     C                   Z-ADD     *ZEROS        DA
     C                   Z-ADD     0             IX                2 0
     C                   Clear                   xxFirst           1

     C     ELKY2         CHAIN     PEL02                              61

     C     *IN61         DOWEQ     *OFF

             xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
             If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
      ** Get the Jurisdiction State
       //*** Beg Chg *** 11/03/23 ***************************************
     C***
     C***  RSKY          CHAIN     PRS01                              60
     C***  CLTID         IFNE      '135'
     C***  RSJRST        ANDNE     'AR'
     C***  RSJRST        ANDNE     'AZ'
     C***  RSJRST        ANDNE     'DE'
     C***  RSJRST        ANDNE     'FL'
     C***  RSJRST        ANDNE     'GA'
     C***  RSJRST        ANDNE     'IL'
     C***  RSJRST        ANDNE     'IN'
     C***  RSJRST        ANDNE     'MO'
     C***  RSJRST        ANDNE     'NJ'
     C***  RSJRST        ANDNE     'NY'
     C***  RSJRST        ANDNE     'OK'
     C***  RSJRST        ANDNE     'PA'
     C***  RSJRST        ANDNE     'SD'
     C***  RSJRST        ANDNE     'TX'
     C***  RSJRST        ANDNE     'VA'
     C***  RSJRST        ANDNE     'VT'
     C***  RSJRST        ANDNE     'WV'
     C***
     C***  CLTID         OREQ      '135'
     C***  RSJRST        ANDNE     'IL'
     C***  RSJRST        ANDNE     'GA'
     C***  RSJRST        ANDNE     'PA'
     C***  RSJRST        ANDNE     'TN'
     C***  RSJRST        ANDNE     'NJ'
     C***  RSJRST        ANDNE     'FL'
       Chain (xxCo#:ELrtCd) prs01;
       If %Found(WMRSL) and RSjrSt <> *blanks;
          Exsr ChkApprExclSt;
          If isApprExclSt = *on;
             xxAmPo = 0;
             xxAmAg = 0;
       //*** End Chg *** 11/03/23 ***************************************
      ** Get the current Deductible Discount record
     C                   Eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     C                   Select
     C                   When      xxDedSize = 'L'
     C                   Callp     rtvLgDedInfo (xxAmpo: xxAmag)
     C                   When      xxDedSize = 'S'
     C                   Callp     rtvSmDedInfo (xxAmpo: xxAmag)
     C                   EndSl

     C                   If        xxAmpo <> 0

     C                   EXSR      CHKST

     C     XXADD         IFEQ      'Y'
      ** Increment counter
     C                   ADD       1             IX
      ** Move state into state array.
     C                   MOVEL     RSJRST        SP(IX)
     C                   eval      da(ix) = xxAmpo
      *  If printing a policy page.  (Not called from W1054R)

     c                   call      'SRCHKPGM'
     c                   parm      'W1054R   '   #1pgm            10
     c                   parm      *BLANK        #1flag            1

     C                   if        #1flag = '0'

     C                   Clear                   xxpremPct
     C                   Clear                   xxlossSub
     C                   Clear                   xxbasisDed
     C                   Clear                   xxdedPct
     C                   Clear                   xxdedAmt
     C                   Clear                   xxdedAgg
     C                   Clear                   xxdedClas
     C                   Clear                   xxdedNet
     C                   Clear                   xxdedVerb
     C                   Clear                   xxPlan
      /free
       xxPlan = RtvDeductible(elco#:elfnd:elemp#:eldiv:elfyr:xxPremPct:
        xxDedAmt:xxDedAgg:xxLossSub:xxBasisDed:xxDedNet:xxDedVerb:
        xxDedClas:xxDedPct);
      /end-free
      *
      * Find the unique ID number.
      *
     C                   MOVE      'P'           X1TYPE            1
     C                   Z-ADD     0             xxunid
     C*
     c                   move      *hival        xxdt
     C     pekey         setgt     ppe02
     C     pekey1        readpe    ppe02                                  60
     C                   If        *in60 = *off
     C                   z-add     peunid        xxunid
     C                   endif
     C                   movel     xxform        x1end#
     c     pekey2        chain     ppe01                              60
      *
      * Copy data into the record.
      *
     C                   If        *in60 = *off
      *
     c                   movel(p)  rsjrst        xxkey
     c     stky          chain     pst01                              60
     c                   if        *in60 = *off
     c                   move      stnst         PEST
     c                   endif
      *
     c                   clear                   E00403
     C                   eval      xxdac = xxDedAmt
     C                   eval      xxprp = xxPremPct
     C                   eval      xxlsd = xxLossSub
     C                   eval      xxbdc = xxBasisDed
     C                   eval      xxdpc = xxDedPct
     C                   eval      xxdag = xxDedAgg
     c                   move      *blanks       pedata
     c                   movel     E00403        pedata
     c                   movel     q1user        peuser
     c                   movel     q1pgm         pepgm
     c                   z-add     dstime        petime
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         pedt
      *
     c                   If        xxFirst = ' '
     C                   Update    ppe01
     C                   Move      'Y'           xxFirst
     c                   Else
     c                   Movel     q1user        peiusr
     c                   Movel     q1pgm         peipgm
     c                   Z-add     dstime        peitim
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   Movel     srdto         peidt
     C                   Write     ppe01
     C                   Endif
      *
     C                   Endif                                                  *in60 ifeq off
      *
     C                   Endif
      *
      ** If page is full, write & setup for more names
     C     IX            IFEQ      4

       //*** Beg Chg *** 11/03/23 ************************************
     C**************     WRITE     PFM01
          WriteInCopybook = *Off;
          Exsr WriteWorkFile;
       //*** End Chg *** 11/03/23 ************************************
     C                   MOVE      *ZEROS        DA
     C                   MOVE      *BLANKS       SP
     C                   Z-ADD     0             IX

     C                   ENDIF                                                  IX = 4

     C                   ENDIF                                                  *IN30 = *OFF

     C                   ENDIF

     C                   ENDIF                                                  VALID STATE
          //*** Beg Add *** 11/03/23 **************************************
          Endif;
          //*** End Add *** 11/03/23 **************************************
             Endif;

      ** Get the next division.
     C     ELKY2         READE     PEL02                                  61
     C                   ENDDO                                                  *IN60 = *OFF

      ** Write the output record
     C     IX            IFNE      *ZERO
      /Free
          //* Write the output record
          WriteInCopybook = *Off;
          Exsr WriteWorkFile;
      /End-Free
     C                   ENDIF                                                  IX <> 0

     C                   ENDSR
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
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     C     VALID         BEGSR
      ** Get the Jurisdiction State
       //*** Beg Chg *** 11/03/23 ************************************
     C***  RSKY          CHAIN     PRS01                              60
     C***
     C***  CLTID         IFNE      '135'
     C***  RSJRST        ANDNE     'AR'
     C***  RSJRST        ANDNE     'AZ'
     C***  RSJRST        ANDNE     'DE'
     C***  RSJRST        ANDNE     'FL'
     C***  RSJRST        ANDNE     'GA'
     C***  RSJRST        ANDNE     'IL'
     C***  RSJRST        ANDNE     'IN'
     C***  RSJRST        ANDNE     'MO'
     C***  RSJRST        ANDNE     'NJ'
     C***  RSJRST        ANDNE     'NY'
     C***  RSJRST        ANDNE     'OK'
     C***  RSJRST        ANDNE     'PA'
     C***  RSJRST        ANDNE     'SD'
     C***  RSJRST        ANDNE     'TX'
     C***  RSJRST        ANDNE     'VA'
     C***  RSJRST        ANDNE     'WV'
     C***  RSJRST        ANDNE     'VT'
     C***
     C***  CLTID         OREQ      '135'
     C***  RSJRST        ANDNE     'IL'
     C***  RSJRST        ANDNE     'GA'
     C***  RSJRST        ANDNE     'PA'
     C***  RSJRST        ANDNE     'TN'
     C***  RSJRST        ANDNE     'NJ'
     C***  RSJRST        ANDNE     'FL'
       Chain (xxCo#:ELrtCd) prs01;
       If %Found(WMRSL) and RSjrSt <> *blanks;
          Exsr ChkApprExclSt;
          If isApprExclSt = *On;
       //*** End Chg *** 11/03/23 ************************************
      ** Get the current Deductible Discount record
     C                   Eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     C                   If        ENSt = *Blanks or ENst = RSjrst
     C                   Select
     C                   When      xxDedSize = 'L'
     C                   Callp     rtvLgDedInfo (xxAmpo: xxAmag)
     C                   When      xxDedSize = 'S'
     C                   Callp     rtvSmDedInfo (xxAmpo: xxAmag)
     C                   Endsl
     C                   If        xxAmpo <> 0
     C                   Eval      crtfrm = *on
     C                   Endif
     C                   Endif
       //*** Beg Chg *** 11/03/23 ************************************
          Endif;
       //*** End Chg *** 11/03/23 ************************************

     C                   ENDIF                                                  VALID STATE

     C                   ENDSR                                                  VALID
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
