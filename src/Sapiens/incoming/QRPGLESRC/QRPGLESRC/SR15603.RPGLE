     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
    H/TITLE SR15603 - KS - Deductible Endorsement
     *===============================================================*
     * WC150603 - Kansas Deductible Endorsement                      *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 02/27/23                                          *
     * Programmer: C:291                                             *
     * Project   : WC2897                                            *
     *===============================================================*
     * REVISIONS:                                                    *
      *                                                               *
      * 08/21/25 C:333 Support: 148/117703  (Recompile)               *
      *                For single print endorsements provide specific *
      *                group and employer so that correct sig is found*
     *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: Please add comments to the top of revisions.           *
     * ------------------------------------------------------------- *
     *===============================================================*
     * Notes:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
     *                                                               *
     *  QQMODE = 1 - Called to determine if form should be attached. *
     *  QQMODE = 2 - Called to print endorsement.                    *
     *  QQMODE = 3 - Called for additional input.                    *
     *  QQMODE = 4 - Called for mid-term processing.                 *
     *===============================================================*
     ** Copybook F specs
     F/COPY SCOPYEN01F
     Fwtddl2    if   e           k disk
     Fwtabl     if   e           k disk    include(pbs01)
     Fwmldl1    if   e           k disk
     Fwxpel     uf a e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)
     Fststl3    if   e           k disk
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     ** Copybook D specs
     D/COPY SCOPYEN01D
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
     ** Form ID
     D qqForm          s             15a   inz('WC150603  ')
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
     D xxampo          s                   like(DDAMPO)
     D xxpct           s                   like(DDPCT)
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
     *=========================================================================
     *  Prototypes
     *=========================================================================
    D rtvDedSize      pr             1
     D  co#                                like(ELCO#)
     D  bscd                               like(ELBSCD)
    D rtvSmDedInfo    pr
     D  ampo                               like(DDAMPO)
     D  amag                               like(DDPCT)
    D rtvLgDedInfo    pr
     D  ampo                               like(DDAMPO)
     D  amag                               like(DDPCT)

      *entry parameters
     D SR15603         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR15603         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
    
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
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
    C*
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
    C*
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs
     C/COPY SCOPYEN01C
     *
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     Othinf        Begsr

       WriteInCopybook = *Off;

       Setll (xxco#:xxfnd:xxfyr:xxemp#) pel02;
       Reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;
       Dow Not %Eof(WDELL1);
          xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
          If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
             Chain (Elco#:ELrtcd) prs01;
             If %Found(wmrsl) and RSjrst = ENst;
                xxDedSize = rtvDedSize(ELco#:ELbscd);
                Select;
                When xxDedSize = 'L';
                   rtvLgDedInfo(xxAmpo: xxPct);
                When xxDedSize = 'S';
                   rtvSmDedInfo(xxAmpo: xxPct);
                EndSl;
                fma701 = xxampo;
                //* Write the output record
                Exsr WriteWorkFile;
                Exsr WriteWCPOLS;
                Leave;
             Endif;
          Endif;
          Reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;
       Enddo;
     *
     C                   Endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     *
     *
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

     C                   Eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     C                   If        ENST = RSJRST
     C                   Select
     C                   When      xxDedSize = 'L'
     C                   Callp     rtvLgDedInfo (xxAmpo: xxPct)
     C                   When      xxDedSize = 'S'
     C                   Callp     rtvSmDedInfo (xxAmpo: xxPct)
     C                   Endsl
     C                   If        xxAmpo <> 0
     C                   Eval      crtfrm = *on
     C                   Endif
     C                   Endif
     *
     C                   endsr
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

       xxPlan = RtvDeductible(elco#:elfnd:elemp#:eldiv:elfyr:xxPremPct:
        xxDedAmt:xxDedAgg:xxLossSub:xxBasisDed:xxDedNet:xxDedVerb:
        xxDedClas:xxDedPct);
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
     * rtvSmDedInfo  - retrieves DDAMPO and DDPCT
     *=========================================================================
    P rtvSmDedInfo    b
    D rtvSmDedInfo    pi
     D  ampo                               like(DDAMPO)
     D  apct                               like(DDPCT)
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
     C                   Eval      apct = DDPCT
     C                   Endif

    P rtvSmDedInfo    e
     *=========================================================================
     * rtvLgDedInfo - retrieves LDAMP1 and LDPCT1
     *=========================================================================
    P rtvLgDedInfo    b
    D rtvLgDedInfo    pi
     D  ampo                               like(DDAMPO)
     D  apct                               like(DDPCT)

     C     elky          Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    ELEMP#
     C                   Kfld                    ELDIV
     C                   Kfld                    ELFYR

     C     elky          Chain     pld01
     C                   If        %found
     C                   Eval      ampo = LDAMP1
     C                   Eval      apct = LDPCT1
     C                   Endif

    P rtvLgDedInfo    e

