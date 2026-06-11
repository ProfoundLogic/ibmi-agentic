     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
     H/TITLE SR37403 - PA - Deductible Endorsement
      *===============================================================*
      * WC370403 - Pennsylvania Deductible Endorsement
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      * Created   : 02/15/01                                          *
      * Programmer: C:90                                              *
      * Project   : CM027                                             *
      *===============================================================*
      * REVISIONS:                                                    *
     *                                                               *
      * 10/19/22 C:342 Project: WC2834                                *
      *                Endorsement Recognition of Cancellation        *
      *                                                               *
     * 05/30/18 C:65  Project: WC2607                                *
      *                Update additional fields in type 43 record.    *
     *                                                               *
     * 09/29/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -No comments.
      *                                                               *
      * 11/25/13 C:287 Support: WA099 80909                           *
      *               -Add Large Deductible logic                     *
      *
      * 01/04/13 C:290 Support SEQ79989                               *
      *                Add if condition to process record if          *
      *                DDSTAT = 'A'. Comment changes made under       *
      *                support SEQ54504.                              *
      *
      * 10/11/07 C:190 SUPPORT WA099 54504                            *
      *                MAKE SURE TO DO NOT ACCESS THE DEDUCTIBLE      *
      *                DISCOUNT CODE HAVING INACTIVE RECORDS.         *
      *                                                               *
      * 08/22/07  C:61 support: WA135 59546
      *                Change OTHINF to check all divisions for policy
      *                year.
      *                                                               *
      * 06/04/07 C:189 PROJECT: 099/SUPP#58380                        *
      *                Endorsement print/attach programs should not   *
      *                write/update WXPEP records if single           *
      *                endorsement print request                      *
      *                                                               *
     H*                                                               *
     H*  04/10/06 C:65 PROJECT SUPP#51293                             *
     H*                CHANGE DEDUCTIBLE DISCOUNT TO PULL BASED ON    *
     H*                ANNIV RATING DATE INSTEAD OF POLICY EFF DATE.  *
      *                                                               *
      * 01/22/04 C:65  PROJECT: 114WAK/SUPP#40498                     *
      *                Change processing to get current date/time id  *
      *                from chaining out to wxpel1 in date/time order.*
      *                Populate maintenance fields on wxpep.          *
      *                                                               *
      * 09/09/03 C:65  PROJECT: 114WAK/SUPP#37919                     *
      *                CHANGE PROCESSING TO GET CURRENT TIME/DATE ID. *
      *                                                               *
      *  04/30/01 C:65  PROJECT: 114WAK                               *
      *                 Add processing to write endorsement data to   *
      *                 the WXPEP reporting file for PA/DE/NJ states. *
      *                                                               *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*
      * Notes:                                                        *
      *                                                               *
      *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      *                                                               *
      *  QQMODE = 1 - Called to determine if form should be attached. *
      *  QQMODE = 2 - Called to print endorsement.                    *
      *  QQMODE = 3 - Called for additional input.                    *
      *  QQMODE = 4 - Called for mid-term processing.                 *
      *===============================================================*
      ** Copybook F specs
     F/COPY SCOPYEN01F
     Fwtddl2    if   e           k disk
     Fwxpel     uf a e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)
     Fwtabl     if   e           k disk    include(pbs01)
     Fwmldl1    if   e           k disk
    
      *=========================================================================
      * Definition Specs
      *=========================================================================

      ** Copybook D specs
     D/COPY SCOPYEN01D
     D/COPY WWCPOLDAT4
     d*** begin add ***  05/30/18  ***************************************
      //***** Beg Del *** 10/19/22 **********************************
     D*****/COPY WPREMPLR
      //***** End Del *** 10/19/22 **********************************
     d*** end add *****  05/30/18  ***************************************
      *=========================================================================
      * /COPY required constants
      *=========================================================================
      ** Form ID
     D qqForm          s             15a   inz('WC370403  ')
      ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
     D xxunid          s                   like(peunid)
     D x1end#          s                   like(peend#)
     D xxwrit          s              1
      *=========================================================================
      * *LIKE DEFN definitions
      *=========================================================================
      *
     d xxampo          s                   like(FMAM01)
     d xxpct           s                   like(DDPCT)
     d xxpct1          s                   like(LDPCT1)
     d*** begin add ***  05/30/18  ***************************************
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
     d xxDedSize       s              1
      *=========================================================================
      * Prototypes
      *=========================================================================
     d rtvDedSize      pr             1                                         return S/L
     d  co#                                like(ELCO#)
     d  bscd                               like(ELBSCD)
     d rtvSmDedInfo    pr
     d  ampo                               like(FMAM01)
     d  pct                                like(DDPCT)
     d rtvLgDedInfo    pr
     d  ampo                               like(FMAM01)
     d  pct1                               like(LDPCT1)

      *entry parameters
     D SR37403         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR37403         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
    
      *
      *=========================================================================
      *  KLIST definitions
      *=========================================================================
     C*
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
     C     stky          KList
     C                   KFld                    xxkey            10
     C*
     C* SUBMISSION HISTORY FILE
     C*
     C     SHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    X1TYPE
     C*
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
      /Free
          WriteInCopybook = *Off;
      /End-Free
      *
     c*** begin add ***  05/30/18  ***************************************
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
     c*** end add *****  05/30/18  ***************************************
     c                   eval      xxwrit = 'N'
     c     elky2         setll     pel02
     c     elky2         reade     pel02

     c                   dow       not %eof
        //***** Beg Add *** 10/19/22 ******************************
        xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
        If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
        //***** End Add *** 10/19/22 ******************************
     C*
     c                   eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     c                   select
     c                   when      xxDedSize = 'L'
     c                   callp     rtvLgDedInfo (xxAmpo: xxpct1)
     c                   eval      FMA521 = xxpct1
     c                   when      xxDedSize = 'S'
     c                   callp     rtvSmDedInfo (xxAmpo: xxpct)
     c                   eval      FMA521 = xxpct
     c                   endsl

     c                   eval      FMA701 = xxAmpo
     c*** begin add ***  05/30/18  ******************************************
      /free
       If xxPlan = ' ';
          xxPlan = RtvDeductible(elco#:elfnd:elemp#:eldiv:elfyr:xxPremPct:
           xxDedAmt:xxDedAgg:xxLossSub:xxBasisDed:xxDedNet:xxDedVerb:
           xxDedClas:xxDedPct);
       Endif;
      /end-free
     C                   eval      FMCD01 = %editc(xxLossSub:'X')
     C                   eval      FMCD02 = %editc(xxBasisDed:'X')
     C                   eval      FMA702 = xxDedPct
     C                   eval      FMAM01 = xxDedAgg
     c*** end add *****  05/30/18  ******************************************
      /Free
          //* Write the output record
          Exsr WriteWorkFile;
      /End-Free
     c                   eval      xxwrit = 'Y'
     c                   leave
    
        //***** Beg Add *** 10/19/22 ******************************
        Endif;
        //***** End Add *** 10/19/22 ******************************
     c     elky2         reade     pel02
     c                   enddo
    
     c                   if        xxwrit = 'Y'
      *  If printing a policy page.  (Not called from W1054R)

     c                   call      'SRCHKPGM'
     c                   parm      'W1054R   '   #1pgm            10
     c                   parm      *BLANK        #1flag            1

     C                   if        #1flag = '0'
      *
      * Update the endorsement reporting file with the proper data.
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
     c                   clear                   E00403
     c*** begin chg ***  05/30/18  ******************************************
     C*****              z-add     fma701        xxdac
     C*****              z-add     fma521        xxprp
     C                   eval      xxdac = xxDedAmt
     C                   eval      xxprp = xxPremPct
     C                   eval      xxlsd = xxLossSub
     C                   eval      xxbdc = xxBasisDed
     C                   eval      xxdpc = xxDedPct
     C                   eval      xxdag = xxDedAgg
     c*** end chg *****  05/30/18  ******************************************
     c                   move      *blanks       pedata
     c                   movel     E00403        pedata
     c                   movel     q1user        peuser
     c                   movel     q1pgm         pepgm
     c                   z-add     dstime        petime
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         pedt
      *
     C                   update    ppe01
      *
     C                   endif                                                  *in60 ifeq off
     C                   endif
      *
      *
     C                   endif
      *
     C                   endsr


      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     pgmstr        begsr
      *
      *
     C                   endsr
      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     C     pgmend        begsr
      *
      *
     C                   endsr
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     C     valid         begsr
      *
     C                   MOVE      ELDCEF        Q2DATE
     C                   MOVE      ELFYR         Q1FYR
     C                   MOVE      ELEMP#        Q1EMP#
     C                   MOVE      ELDIV         Q1DIV
     C*
     C                   CALL      'SREFFD'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    Q1FYR             3
     C                   PARM                    Q1EMP#            9
     C                   PARM                    Q1DIV             5
     C                   PARM                    Q2DATE            7
     C*
     C                   MOVE      Q2DATE        XXDDT
      *
     C     ddky          setgt     pdd01
     C     ddky1         readpe    pdd01                                  60
     C*  To read until a record with active status is found.
     C     *In60         Downe     *On
     C     Ddstat        Andne     'A'
     C*
     C     Ddky1         Readpe    Pdd01                                  60
     C*
     C                   Enddo
      *
     C     *in60         ifeq      *off
     C     ddampo        andne     *zero
      *
      *
      ** Found a division with a matching jurisdiction state
     C                   move      *on           crtfrm
      *
     C                   endif
      *
     C                   endsr

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
      *rtvSmDedInfo - retrieves DDAMPO and DDPCT
      *=========================================================================
     p rtvSmDedInfo    b
     d rtvSmDedInfo    pi
     d  ampo                               like(FMAM01)
     d  pct                                like(DDPCT)

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
     c     ddky1         setgt     pdd01
     c     ddky          readpe    pdd01
     c                   dow       not(%eof) and DDSTAT <> 'A'
     c     ddky          readpe    pdd01
     c                   enddo

     c                   if        not(%eof)
     c                   eval      ampo = DDAMPO
     c                   eval      pct  = DDPCT
     c                   endif

     p rtvSmDedInfo    e

      *=========================================================================
      *rtvLgDedInfo- retrieves LDAMPO and LDPCT1
      *=========================================================================
     p rtvLgDedInfo    b
     d rtvLgDedInfo    pi
     d  ampo                               like(FMAM01)
     d  pct1                               like(LDPCT1)

     c     elky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c                   kfld                    ELFYR

     c     elky          chain     pld01
     c                   if        %found
     c                   eval      ampo = LDAMP1
     c                   eval      pct1 = LDPCT1
     c                   endif

     p rtvLgDedInfo    e

