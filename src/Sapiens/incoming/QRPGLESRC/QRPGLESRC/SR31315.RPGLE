     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
    H/TITLE SR31313B - NY - Benefits Deductible Endorsement
      *===============================================================*
      * WC310315A - New York Benefits Deductible Endorsement          *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      * Created   : 11/08/01                                          *
      * Programmer: C:90                                              *
      * Project   : CM027                                             *
      *===============================================================*
      * REVISIONS:                                                    *
      *                                                               *
      * 10/19/22 C:342 Project: WC2834                                *
      *                Endorsement Recognition of Cancellation        *
      *                                                               *
      * 05/30/18 C:65  Project: 150WAD/WC2607                         *
      *                Implement logic to store data for type 43 rcds.*
      *                                                               *
      * 02/09/17 C:180 Support: SY371/41273                           *
      *                Populate correct fields per endorsement        *
      *                mapping for small deductible.                  *
      *                Removed 80909 which was does not populate      *
      *                current form fields.                           *
      *                Additional research needed to see if 80909     *
      *                should apply.                                  *
      *
      * 02/02/17 C:180 Support: SY371/41273                           *
      *                Change key for retrieving deductible.          *
      *                                                               *
      * 11/15/13 C:287 Support: WA099 80909                           *
      *               -Add Large Deductible logic                     *
     H*                                                               *
     H* 01/04/13 C:284 Support SEQ79989                               *
     H*                Add if condition to process record if          *
     H*                DDSTAT = 'A'. Comment changes made under       *
     H*                support SEQ54504.
      *               -Chg'd Q1DATE field to Q2DATE (not commented)   *
     H*
     H* 10/11/07 C:190 SUPPORT WA099 54504                            *
     H*                MAKE SURE TO DO NOT ACCESS THE DEDUCTIBLE      *
     H*                DISCOUNT CODE HAVING INACTIVE RECORDS.         *
     H*                                                               *
     H*  04/10/06 C:65 PROJECT SUPP#51293                             *
     H*                CHANGE DEDUCTIBLE DISCOUNT TO PULL BASED ON    *
     H*                ANNIV RATING DATE INSTEAD OF POLICY EFF DATE.  *
      *                                                               *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*
      *Notes:                                                        *
      *                                                               *
      *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      *                                                               *
      *  QQMODE = 1 - Called to determine if form should be attached. *
      *  QQMODE = 2 - Called to print endorsement.                    *
      *  QQMODE = 3 - Called for additional input.                    *
      *  QQMODE = 4 - Called for mid-term processing.                 *
      *===============================================================*
      **Copybook F specs
     Fwtddl2    if   e           k disk
     Fwtabl     if   e           k disk    include(pbs01)
     Fwmldl1    if   e           k disk
     F*** begin add ***  05/30/18  ***************************************
     Fwxpel     uf a e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)
     Fststl3    if   e           k disk
     F*** end add *****  05/30/18  ***************************************
    
     F/COPY SCOPYEN01F
      *=========================================================================
      * Data structures and constants
      *=========================================================================
      **Copybook D specs
     D/COPY SCOPYEN01D
      *
      *=========================================================================
      * /COPY required constants
      *=========================================================================
      **Form ID
     D qqForm          s             15a   inz('WC310315A ')
      **Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
      *=========================================================================
      * *LIKE DEFN definitions
      *=========================================================================
      *
     d xxampo          s                   like(FMAM01)
     d xxamag          s                   like(FMAM01)
     d xxDedSize       s              1
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
      *=========================================================================
      * Prototypes
      *=========================================================================
     d rtvDedSize      pr             1                                         return S/L
     d  co#                                like(ELCO#)
     d  bscd                               like(ELBSCD)
     d rtvSmDedInfo    pr
     d  ampo                               like(FMAM01)
     d  amag                               like(FMAM01)
     d rtvLgDedInfo    pr
     d  ampo                               like(FMAM01)
     d  amag                               like(FMAM01)

      *entry parameters
     D SR31315         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR31315         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
     d*** begin add ***  05/30/18  ***************************************
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
     d*** end add *****  05/30/18  ***************************************
    
      *=========================================================================
      * KLIST definitions
      *=========================================================================
     C     ddky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    rsjrst
     C                   kfld                    eldcde
     C                   KFLD                    XXDDT             7 0
     C*
     C     ddky1         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    rsjrst
     C                   kfld                    eldcde
      *=========================================================================
      * Main
      *=========================================================================
      **Copybook C specs
     C/COPY SCOPYEN01C
      *
      *=========================================================================
      * OTHINF- Subroutine to load form specific information & write record
      *=========================================================================
     C     othinf        begsr
     C*
     C     elky2         setll     pel02
     C     elky2         reade     pel02                                  60
     C*
     C                   dow       *in60 = *off
       //***** Beg Add *** 10/19/22 ******************************
          xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
          If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
       //***** End Add *** 10/19/22 ******************************
     C*
     c                   eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     c                   select
     c                   when      xxDedSize = 'L'
     c                   callp     rtvLgDedInfo (xxAmpo: xxAmag)
     c                   when      xxDedSize = 'S'
     c                   callp     rtvSmDedInfo (xxAmpo: xxAmag)
     c                   endsl


     C*  If a record is found and there is a per occurrence deductible ,
     C*  then print that deductible on the endorsement
      /Free
         If xxAmpo <> *zero;
            FMA701 = xxAmpo;
         //*** Begin Add *** 05/30/18 *********************
            Exsr WriteWCPOLS;
         //*** End Add ***** 05/30/18 *********************
         Endif;
      /End-Free
     c*** 02/09/17       eval      FMAM01 = xxAmag
     c*** 02/09/17       eval      FMAM02 = xxAmpo
    
     C                   leave
      *
       //***** Beg Add *** 10/19/22 ******************************
          Endif;
       //***** End Add *** 10/19/22 ******************************
     C     elky2         reade     pel02                                  60
     C                   enddo
      *
     C                   endsr
      *=========================================================================
      * PGMSTR- Beginning of program processing
      *=========================================================================
     C     pgmstr        begsr
      *
      *
     C                   endsr
      *=========================================================================
      * PGMEND- End of program processing
      *=========================================================================
     C     pgmend        begsr
      *
      *
     C                   endsr
      *=========================================================================
      * VALID- Subroutine to attach this form to the policy
      *=========================================================================
     C     valid         begsr
      *
     c                   eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     c                   if        ENST = RSJRST
     c                   select
     c                   when      xxDedSize = 'L'
     c                   callp     rtvLgDedInfo (xxAmpo: xxAmag)
     c                   when      xxDedSize = 'S'
     c                   callp     rtvSmDedInfo (xxAmpo: xxAmag)
     c                   endsl
     c                   if        xxAmpo <> 0
     c                   eval      crtfrm = *on
     c                   endif
     c                   endif
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

      *=========================================================================
      *rtvDedSize- Query whether policy is small or large deductible
      *=========================================================================
     p rtvDedSize      b

     d rtvDedSize      pi             1                                         return S/L
     d  co#                                like(ELCO#)
     d  bscd                               like(ELBSCD)

     d kkco#           s                   like(BSCO#)
     d kkkey           s                   like(BSKEY)

     c     bsky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkkey

     c     rsky          chain     prs01
     c                   if        not(%found)
     c                   eval      RSJRST = *blank
     c                   return    ' '
     c                   endif

     c                   eval      kkco# = co#
     c                   movel(p)  bscd          kkkey
     c     bsky          chain     pbs01
     c                   select
     c                   when      BSLPGM = 'L'
     c                   return    'L'
     c                   other
     c                   return    'S'
     c                   endsl

     p rtvDedSize      e

      *=========================================================================
      *rtvSmDedInfo - retrieves DDAMPO and DDAMAG
      *=========================================================================
     p rtvSmDedInfo    b
     d rtvSmDedInfo    pi
     d  ampo                               like(FMAM01)
     d  amag                               like(FMAM01)

     c                   move      ELCO#         qqco#
     c                   move      ELFND         qqfnd
     c                   move      ELEMP#        q1emp#
     c                   move      ELDIV         q1div
     c                   move      ELFYR         q1fyr
     c                   move      ELDCEF        q2date

     c                   call      'SREFFD'
     c                   parm                    qqco#             3
     c                   parm                    qqfnd             3
     c                   parm                    q1fyr             3
     c                   parm                    q1emp#            9
     c                   parm                    q1div             5
     c                   parm                    q2date            7

     c                   move      q2date        xxddt
     c     ddky          setgt     pdd01
     c     ddky1         readpe    pdd01
     c                   dow       not(%eof) and DDSTAT <> 'A'
     c     ddky1         readpe    pdd01
     c                   enddo

     c                   if        not(%eof)
     c                   eval      ampo = DDAMPO
     c                   eval      amag = DDAMAG
     c                   endif

     p rtvSmDedInfo    e

      *=========================================================================
      *rtvLgDedInfo- retrieves LDAMPO and LDAMAG
      *=========================================================================
     p rtvLgDedInfo    b
     d rtvLgDedInfo    pi
     d  ampo                               like(FMAM01)
     d  amag                               like(FMAM01)

     c     elky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c                   kfld                    ELFYR

     c     elky          chain     pld01
     c                   if        %found
     c                   eval      ampo = LDAMP1
     c                   eval      amag = LDAMG1
     c                   endif

     p rtvLgDedInfo    e
