     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')

    H/TITLE SR42602A - TX - Accident Deductible Endorsement
     *========================================================================*
     * WC420602A - Texas Accident Deductible Endorsement                      *
     *                                                                        *
     * Program Description:                                                   *
     *                                                                        *
     *   Determines if this endorsement form should be attached to            *
     *   the policy and creates data in the Endorsement Form                  *
     *   Workfile (WXFMP) prior to a form merge.                              *
     *                                                                        *
     * Created   : 05/05/14                                                   *
     * Programmer: C:282                                                      *
     * Project   : WC2281                                                     *
     *========================================================================*
     * REVISIONS:                                                             *
      *                                                                        *
      * 10/19/22 C:342 Project: WC2834                                         *
      *                Endorsement Recognition of Cancellation                 *
      *                                                                        *
      * 03/16/21 C:65  Project: Phw#93467 (Sunz)                               *
      *                Implement logic to store data for type 43 rcds.         *
     *                                                                        *
      * 01/24/20 C:180 Project WC2726
      *                Texas Ded Endorsement Attachment Controls     *
     * 09/15/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -No comments.
     * -----------------------------------------------------------------------*
     *  NOTE: Please add comments to the top of revisions.                    *
     * -----------------------------------------------------------------------*
     *========================================================================*
     * Notes:                                                                 *
     *                                                                        *
     *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).                   *
     *                                                                        *
     *  QQMODE = 1 - Called to determine if form should be attached.          *
     *  QQMODE = 2 - Called to print endorsement.                             *
     *  QQMODE = 3 - Called for additional input.                             *
     *  QQMODE = 4 - Called for mid-term processing.                          *
     *========================================================================*
     ** Copybook F specs
     Fwtddl2    if   e           k disk
     Fwtabl     if   e           k disk    include(pbs01)
     Fwmldl1    if   e           k disk
      ** Begin Add *** 01/24/20 *****************************************
     Fwtrtp     if   e           k disk
      ** End   Add *** 01/24/20 *****************************************
     F*** begin add ***  03/16/21  ***************************************
     Fwxpel     uf a e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)
     Fststl3    if   e           k disk
     F*** end add *****  03/16/21  ***************************************
     F/COPY SCOPYEN01F
     *=========================================================================
     *  Data structures and constants
     *=========================================================================
     ** Copybook D specs
     D/COPY SCOPYEN01D
     *
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
     ** Form ID
     D qqForm          s             15a   inz('WC420602A ')
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
     D xxampo          s                   like(FMAM01)
     D xxamag          s                   like(FMAM01)
     D xxDedSize       s              1
     d*** begin add ***  03/16/21  ***************************************
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
     d*** end add *****  03/16/21  ***************************************
      ** Begin Add *** 01/24/20 ************************************
     D MedicalCodes    s              2a   Dim(50)

     D ReserveCodes    DS
     D  DDDC01                        2a
     D  DDDC02                        2a
     D  DDDC03                        2a
     D  DDDC04                        2a
     D  DDDC05                        2a
     D  DDDC06                        2a
     D  DDDC07                        2a
     D  DDDC08                        2a
     D  DDDC09                        2a
     D  DDDC10                        2a
     D   RC                           2a   Overlay(ReserveCodes) Dim(10)

      * Reserve code for this policy
     D ReserveCodesL   DS
     D  LDR101                        2a
     D  LDR102                        2a
     D  LDR103                        2a
     D  LDR104                        2a
     D  LDR105                        2a
     D  LDR106                        2a
     D  LDR107                        2a
     D  LDR108                        2a
     D  LDR109                        2a
     D  LDR110                        2a
     D   LRC                          2a   Overlay(ReserveCodesL) Dim(10)
      ** End   Add *** 01/24/20 ************************************
     *=========================================================================
     *  Prototypes
     *=========================================================================
    D rtvDedSize      pr             1
     D  co#                                like(ELCO#)
     D  bscd                               like(ELBSCD)
    D rtvSmDedInfo    pr
     D  ampo                               like(FMAM01)
     D  amag                               like(FMAM01)
    D rtvLgDedInfo    pr
     D  ampo                               like(FMAM01)
     D  amag                               like(FMAM01)
      ** Begin Add *** 01/24/20 ***********************************
     D*--------------------------------------------------
     D* Procedure name: LoadMedicalCodes
     D* Purpose:        Load reserve codes type M
     D* Returns:
     D*--------------------------------------------------
     D LoadMedicalCodes...
     D                 PR
      ** End   Add *** 01/24/20 ***********************************
      *entry parameters
     D SR42602A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR42602A        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
    
     d*** begin add ***  03/16/21  ***************************************
      //***** Beg Del *** 10/19/22 **********************************
     D*****/COPY WPREMPLR
      //***** End Del *** 10/19/22 **********************************
      /COPY WWCPOLDAT4

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
     d*** end add *****  03/16/21  ***************************************
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
     C     ddky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    rsjrst
     C                   kfld                    eldcde
     C                   KFLD                    XXDDT             7 0
    C*
     C     ddky1         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    rsjrst
     C                   kfld                    eldcde
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs
     C/COPY SCOPYEN01C
     *
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
      /Free
          WriteInCopybook = *Off;
      /End-Free
    C*
     C     elky2         setll     pel02
     C     elky2         reade     pel02                                  60
    C*
     C                   dow       *in60 = *off
        //***** Beg Add *** 10/19/22 ******************************
        xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
        If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
        //***** End Add *** 10/19/22 ******************************
    C*
     C     rsky          chain     prs01                              60
    C*
    C*  If a record is found and there is a per occurrence deductible ,
    C*  then print that deductible on the endorsement
    C*
     *
     C                   Eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     C                   Select
     C                   When      xxDedSize = 'L'
     C                   Callp     rtvLgDedInfo (xxAmpo: xxAmag)
     C                   When      xxDedSize = 'S'
     C                   Callp     rtvSmDedInfo (xxAmpo: xxAmag)
     C                   EndSl

     C                   If        xxAmpo <> 0
     C                   Eval      fma701 = xxAmpo
      /Free
          //* Write the output record
          Exsr WriteWorkFile;
       //*** Begin Add *** 03/16/21 *********************
          Exsr WriteWCPOLS;
       //*** End Add ***** 03/16/21 *********************
      /End-Free
     C                   leave
     *
     C                   endif
     *
        //***** Beg Add *** 10/19/22 ******************************
        Endif;
        //***** End Add *** 10/19/22 ******************************
     C     elky2         reade     pel02                                  60
     C                   enddo
     *
     C                   endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
      ** Begin Change 01/24/20 *******************************************
     C*    pgmstr        begsr
     *
     *
     C*                  endsr
      /Free
       Begsr Pgmstr;
          LoadMedicalCodes();
       Endsr;
      /End-Free
      ** End  Change  01/24/20 *******************************************
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     *
     *
     C                   endsr
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
     *
     C     rsky          chain     prs01                              60
     *
     C                   if        *in60 = *off
     C                             and rsjrst = enst
     *
     ** Found a division with a matching jurisdiction state
     *
    C*
    C*  If a record is found and there is a per occurrence deductible ,
    C*  then include the endorsement
    C*
     C                   Eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     C                   If        ENST = RSJRST
     C                   Select
     C                   When      xxDedSize = 'L'
     C                   Callp     rtvLgDedInfo (xxAmpo: xxAmag)
     C                   When      xxDedSize = 'S'
     C                   Callp     rtvSmDedInfo (xxAmpo: xxAmag)
     C                   Endsl
     C                   If        xxAmpo <> 0
     C                   move      *on           crtfrm
     C                   endif
     C                   endif
    C*
     C                   endif
     *
     C                   endsr
     c*** begin add ***  03/16/21  ******************************************
      *=========================================================================
      *  WriteWCPOLS - record information for electronic data record
      *=========================================================================
     C     WriteWCPOLS   Begsr
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
     C                   Update    ppe01
      *
     C                   Endif                                                  *in60 ifeq off
      *
     C                   Endif
      *
     C                   Endsr
      *=========================================================================
     c*** end add *****  03/16/21  ******************************************
     *=========================================================================
     * Query whether policy is small or large deductible
     *=========================================================================
    P rtvDedSize      b

    D rtvDedSize      pi             1                                         return S/L
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

    P rtvDedSize      e
     *=========================================================================
     * rtvSmDedInfo  - retrieves DDAMPO and DDAMAG
     *=========================================================================
    P rtvSmDedInfo    b
    D rtvSmDedInfo    pi
     D  ampo                               like(FMAM01)
     D  amag                               like(FMAM01)
     D  kkefdt         s                   like(DDEFDT)
      *** Begin Add *** 01/24/19 ************************************
     D  index          s              2  0
     D  isMedical      s               N
     D  isAnother      s               N
      *** End   Add *** 01/24/19 ************************************

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
      *** Begin Change *** 01/24/19 **************************************

     C***                   Eval      ampo = DDAMPO
     C***                   Eval      amag = DDAMAG
      /Free
       ampo = *Zeros;
       amag = *Zeros;
       isMedical = *Off;
       isAnother = *Off;

       If DDplan = '02';
          For index = 1 to 10;
             If RC(index) <> *Blanks;
                If %lookup(RC(index):MedicalCodes) > *ZEROS;
                   isMedical = *On;
                Else;
                   isAnother = *On;
                Endif;
             Endif;
          Endfor;

          If isMedical and isAnother;    //Not Medical Only
             ampo = DDampo;
             amag = DDamag;
          Endif;
       Endif;
      /End-Free
      *** End   Change   *** 01/24/19 **************************************
     C                   Endif

    P rtvSmDedInfo    e
     *=========================================================================
     * rtvLgDedInfo - retrieves LDAMPO and LDAMAG
     *=========================================================================
    P rtvLgDedInfo    b
    D rtvLgDedInfo    pi
     D  ampo                               like(FMAM01)
     D  amag                               like(FMAM01)

      ** Begin Add *** 01/24/20 *************************************
     D  index          s              2  0
     D  isMedical      s               N
     D  isAnother      s               N
      ** End   Add *** 01/24/20 *************************************

     C     elky          Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    ELEMP#
     C                   Kfld                    ELDIV
     C                   Kfld                    ELFYR

     C     elky          Chain     pld01
      *
      **** Begin Change 01/24/20 ***********************************************
     C***                If        %found
     C***                Eval      ampo = LDAMP1
     C***                Eval      amag = LDAMG1
     C***                Endif
      /Free
        ampo = *Zeros;
        amag = *Zeros;
        isMedical = *Off;
        isAnother = *Off;

        If %found and (LDPLN1 = '02' or LDPLN2= '02');
           For index = 1 to 10;
              If LRC(index) <> *Blanks;
                 If %lookup(LRC(index):MedicalCodes) > *ZEROS;
                    isMedical = *On;
                 Else;
                    isAnother = *On;
                 Endif;
              Endif;
           Endfor;

           If isMedical and isAnother;  //Not Medical Only
              ampo = LDamp1;
              amag = LDamg1;
           Endif;
        Endif;
      /End-Free
      **** End   Change 01/24/20 ***********************************************
    P rtvLgDedInfo    e
      ** Begin Add *** 01/24/20 *************************************
      //--------------------------------------------------
      // Procedure name: LoadMedicalCodes
      // Purpose:        Load reserve codes type 'M'
      //--------------------------------------------------
     P LoadMedicalCodes...
     P                 B
     D LoadMedicalCodes...
     D                 PI
     D index           s              2  0 Inz(1)
      /Free

       Read prt01;
       Dow not %EOF;
          If RTtype = 'M';
             MedicalCodes(index) = RTkey;
             index += 1;
          Endif;
         Read prt01;
       Enddo;
      /End-free
     P LoadMedicalCodes...
     P                 E
      ** End   Add *** 01/24/20 *************************************
