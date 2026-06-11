     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H DFTACTGRP(*no) ACTGRP(*caller) bnddir('WTBNDDIR')
    H/TITLE SR13602 - IN COINSURANCE AND DEDUCTIBLE ENDORSEMENT
     *========================================================================*
     * WC130602 - Indiana Coinsurance and Deductible Endorsement              *
     *                                                                        *
     * Program Description:                                                   *
     *                                                                        *
     *   Determines if this endorsement form should be attached to            *
     *   the policy and creates data in the Endorsement Form                  *
     *   Workfile (WXFMP) prior to a form merge.                              *
     *                                                                        *
     * Created   : 11/29/01                                                   *
     * Programmer: C:90                                                       *
     * Project   : 48WVM                                                      *
     *========================================================================*
     * REVISIONS:                                                             *
      *                                                                        *
      * 11/02/22 C:333 Project: WC2834                                         *
      *                Endorsement Recognition of Cancellation                 *
      *                                                                        *
      * 05/30/18 C:65  Project: 150WAD/WC2607                                  *
      *                Implement logic to store data for type 43 rcds.         *
      *                                                                        *
      * 12/29/16 C:180 Project SY371 scriptura.                                *
      *                Add Scriptura interface.                                *
      *                                                                        *
      *                                                                        *
     * 05/23/13 C:287 Support: WA099 80909                                    *
     *                - Add Large Deductible logic                            *
      *                                                                        *
      * 10/11/07 C:190 SUPPORT WA099 54504                                     *
      *                MAKE SURE TO DO NOT ACCESS THE DEDUCTIBLE               *
      *                DISCOUNT CODE HAVING INACTIVE RECORDS.                  *
      *                                                                        *
      *  04/10/06 C:65 PROJECT SUPP#51293                                      *
      *                CHANGE DEDUCTIBLE DISCOUNT TO PULL BASED ON             *
      *                ANNIV RATING DATE INSTEAD OF POLICY EFF DATE.           *
     *                                                                        *
     *                                                                        *
     * ---------------------------------------------------------------------- *
     *  NOTE: Please add comments to the top of revisions.                    *
     * ---------------------------------------------------------------------- *
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
     F*** Begin Add ***** 05/23/13 *******************************************
     Fwtabl     if   e           k disk    include(pbs01)
     Fwmldl1    if   e           k disk
     F*** End Add ******* 05/23/13 *******************************************
     F*** begin add ***  05/30/18  ***************************************
     Fwxpel     uf a e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)
     Fststl3    if   e           k disk
     F*** end add *****  05/30/18  ***************************************
     F/COPY SCOPYEN01F
     *=========================================================================
     *  Data structures and constants
     *=========================================================================
      **** Begin Add **** 12/29/16 *********************************************
      * Entry
     D SR13602         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR13602         PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1
      **** End   Add **** 12/29/16 *********************************************
     ** Copybook D specs
     D/COPY SCOPYEN01D
     *
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
     ** Form ID
      *** Begin Change *** 12/29/16 ********************************************
     D*** xxform          c                   const('WC130602  ')
     D QQFORM          S             15A   inz('WC130602  ')
      *** End   Change *** 12/29/16 ********************************************
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')

     *** Begin Add ***** 05/23/13 *******************************************
     D xxampo          s                   like(FMAM01)
     D xxamag          s                   like(FMAM01)
     D xxDedSize       s              1
     d*** begin add ***  05/30/18  ***************************************
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
     d*** end add *****  05/30/18  ***************************************
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
      *** End Add ****** 05/23/13 *******************************************

     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
     d*** begin add ***  05/30/18  ***************************************
      //***** Beg Del *** 11/02/22 **********************************
     D***/COPY WPREMPLR
      //***** End Del *** 11/02/22 **********************************
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
     d*** end add *****  05/30/18  ***************************************
     *
     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     ** Entry parameters
      ** Begin Delete *******   12/29/16 ***************************************
     C***  *entry        plist
     C***                parm                    qqco#
     C***                parm                    qqfnd
     C***                parm                    qqemp#
     C***                parm                    qqdiv
     C***                parm                    qqfyr
     C***                parm                    qqmode
     C***                parm                    qqrrn
     C***                parm                    qqmod2
    
     ** Additional input program parameters
     C***  addpl         pList
     C***                parm      *blanks       qqprds
     C***                parm      *blanks       qqcall
     C***                parm                    qqco#
     C***                parm                    qqfnd
     C***                parm                    qqemp#
     C***                parm                    qqdiv
     C***                parm                    qqfyr
     *** End   Delete *******   12/29/16 ***************************************
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
     C     ddky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    rsjrst
     C                   kfld                    eldcde
     C*** BEGIN CHG ***  04/10/06  ************************************
     C*************      KFLD                    ELPFDT
     C                   KFLD                    XXDDT             7 0
     C*** END CHG *****  04/10/06  ************************************
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
    C*
     C     elky2         setll     pel02
     C     elky2         reade     pel02                                  60
    C*
     C                   dow       *in60 = *off
    C*
       //***** Beg Add *** 11/02/22 ******************************
          xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
          If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
       //***** End Add *** 11/02/22 ******************************
     C     rsky          chain     prs01                              60
    C*
     C*** BEGIN CHG ***  04/10/06  ************************************
     C*** Begin Delete ***** 05/23/13 *********************************
     C***                MOVE      ELDCEF        Q1DATE
     C***                MOVE      ELFYR         Q1FYR
     C***                MOVE      ELEMP#        Q1EMP#
     C***                MOVE      ELDIV         Q1DIV

     C***                CALL      'SREFFD'
     C***                PARM                    QQCO#             3
     C***                PARM                    QQFND             3
     C***                PARM                    Q1FYR             3
     C***                PARM                    Q1EMP#            9
     C***                PARM                    Q1DIV             5
     C***                PARM                    Q1DATE            7

     C***                MOVE      Q1DATE        XXDDT
     C*** END CHG *****  04/10/06  ************************************
     C***  ddky          setgt     pdd01
     C***  ddky1         readpe    pdd01                                  60
    C*
    C*  If a record is found and there is a per occurrence deductible ,
    C*  then print that deductible on the endorsement
    C*
     C***                if        *in60 = *off
     C*** BEG ADD *****  10/11/07  ************************************
     C***                          and ddstat = 'A'
     C*** END ADD *****  10/11/07  ************************************
     C***                          and ddampo <> *zero
     C***                eval      fma701 = ddampo
    
     C*** Begin Add ****** 05/22/13 ***********************************
     C                   Eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     C                   Select
     C                   When      xxDedSize = 'L'
     C                   Callp     rtvLgDedInfo (xxAmpo : xxAmag )
     C                   When      xxDedSize = 'S'
     C                   Callp     rtvSmDedInfo (xxAmpo : xxAmag )
     C                   EndSl

     C                   If        xxampo <> 0
     C                   Z-Add     xxampo        FMA701

     C*** End Add     ***** 05/23/13 *********************************

     ** Write the output record
      *** Begin Change *** 12/29/16 ********************************************
     C***                write     pfm01
      /Free
       Exsr WriteWorkFile;
      /End-Free
      *** End   Change *** 12/29/16 ********************************************
     C*** Begin Add *** 05/30/18 **********************************
     C                   Exsr      WriteWCPOLS
     C*** End Add ***** 05/30/18 **********************************
     C                   leave
     *
     C                   endif
     *
          //***** Beg Add *** 11/02/22 ******************************
          Endif;
          //***** End Add *** 11/02/22 ******************************
     C     elky2         reade     pel02                                  60
     C                   enddo
     *
     C                   endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
      **** Begin Add **** 12/29/16 *********************************************
     /Free
        WriteInCopybook = *OFF;
     /End-Free
      **** End   Add **** 12/29/16 *********************************************
     C                   endsr
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
     C*** BEGIN CHG ***  04/10/06  ************************************
     C*** Begin Delete *** 05/23/13 ***********************************
     C***                MOVE      ELDCEF        Q1DATE
     C***                MOVE      ELFYR         Q1FYR
     C***                MOVE      ELEMP#        Q1EMP#
     C***                MOVE      ELDIV         Q1DIV

     C***                CALL      'SREFFD'
     C***                PARM                    QQCO#             3
     C***                PARM                    QQFND             3
     C***                PARM                    Q1FYR             3
     C***                PARM                    Q1EMP#            9
     C***                PARM                    Q1DIV             5
     C***                PARM                    Q1DATE            7

     C***                MOVE      Q1DATE        XXDDT
     C*** END CHG *****  04/10/06  ************************************
     C***  ddky          setgt     pdd01
     C***  ddky1         readpe    pdd01                                  60
    C*
    C*  If a record is found and there is a per occurrence deductible ,
    C*  then include the endorsement
    C*
     C***                if        *in60 = *off
     C*** BEG ADD *****  10/11/07  ************************************
     C***                          and ddstat = 'A'
     C*** END ADD *****  10/11/07  ************************************
     C***                          and ddampo <> *zero
     C***                move      *on           crtfrm
     C***                endif
     C*** End Delete   *** 05/23/13 ***********************************
    C*
     C*** Start Add ***** 05/23/13 *************************************
     C                   Eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     C                   If        ENST = RSJRST
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
     C*** End Add ***** 05/23/13 ***************************************
     C                   endif
     *
     C                   endsr
     c*** begin add ***  05/30/18  ******************************************
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
     c*** end add *****  05/30/18  ******************************************

     C*** Begin Add ***** 05/23/13 **************************************

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

    P rtvSmDedInfo    e
     *=========================================================================
     * rtvLgDedInfo - retrieves LDAMPO and LDAMAG
     *=========================================================================
    P rtvLgDedInfo    b
    D rtvLgDedInfo    pi
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

    P rtvLgDedInfo    e

      *** End Add ***** 05/23/13 ***********************************************
