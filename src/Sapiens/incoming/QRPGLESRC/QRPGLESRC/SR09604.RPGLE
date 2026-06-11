       //============================================================
       Ctl-Opt Copyright('(c) 2022 Sapiens. All Rights Reserved.')
        Option(*NoDebugIO:*SrcStmt)
        DftActGrp(*No)
        ActGrp(*Caller)
        BndDir('WTBNDDIR');
     h/Title SR09604 - LOAD ENDORSEMENT FORM WORKFILE FOR FORM WC090604
       //============================================================
       // Program Description:
       //
       //   Determines if this endorsement form should be attached to
       //   the policy and creates data in the Endorsement Form
       //   Workfile (WXWXFMP) prior to a form merge.
       //
       // PROGRAMMER: C:333
       // CREATED:    10/31/22            PROJECT: WC2834
       //=============================================================
       // Revisions:
       //
       //=============================================================
       // NOTES:
       //  Utilizes the endorsement copybook (SCOPYEN01F/D/C).
       //  qqmode = 1 - Called to determine if form should be attached
       //  qqmode = 2 - Called to print endorsement.
       //  qqmode = 3 - Called for additional input.
       //  qqmode = 4 - Called for midterm processing.
       //=============================================================
     fWTDDL2    IF   E           K DISK
     fwtabl     if   e           k disk    include(pbs01)
     fwmldl1    if   e           k disk

       // Copybook F specs
     f/Copy sCopyEn01f
       //=============================================================
       // Definition Specs
       //=============================================================
     d #1PGM           s             10A   INZ('W1054R')
     d #1FLAG          s              1A   INZ
     D* Check Pgm
     D SRCHKPGM        pr                  ExtPgm('SRCHKPGM')
     D                               10
     D                                1
     D xxampo          s                   like(DDAMPO)
     D xxamag          s                   like(DDAMAG)
     D xxclas          s                   like(DDCLAS)
     D xxDedSize       s              1
      *================================================================
      *  Prototypes
      *================================================================
     D rtvDedSize      pr             1
     D  co#                                like(ELCO#)
     D  bscd                               like(ELBSCD)
     D rtvSmDedInfo    pr
     D  ampo                               like(DDAMPO)
     D  amag                               like(DDAMAG)
     D  clas                               like(DDCLAS)
     D rtvLgDedInfo    pr
     D  ampo                               like(DDAMPO)
     D  amag                               like(DDAMAG)
     D  clas                               like(DDCLAS)

       Dcl-Pr SR09604;
        *N Char(3);
        *N Char(3);
        *N Char(9);
        *N Char(5);
        *N Char(3);
        *N Char(1);
        *N Packed(9:0);
        *N Char(1);
       End-Pr;
       Dcl-Pi SR09604;
        qqCo# Char(3);
        qqFnd Char(3);
        qqEmp# Char(9);
        qqDiv Char(5);
        qqFyr Char(3);
        qqMode Char(1);
        qqRrn Packed(9:0);
        qqMod2 Char(1);
       End-Pi;
       // Copybook D specs
     d/Copy sCopyEn01d
       //=============================================================
       // Copybook required constants
       //=============================================================
       // Form ID
       Dcl-C qqForm Const('WC090604');
       // Additional entry program
       Dcl-C addPgm Const('SRxxxxxxI');
       //=============================================================
       // Main
       //=============================================================
       // Copybook C specs (PLISTs, KLISTs and mainline)
     c/Copy sCopyEn01C
       //=============================================================
       // OthInf - Subroutine to load form specific information
       //=============================================================
       Begsr OthInf;
     C*
     C*  Check each division for a matching jurisdiction state
     C*
     C     ELKY2         CHAIN     PEL02                              60
     C*
     C     *IN60         DOWEQ     *OFF
        xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
        If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
     C*
     C*  GET THE JURISDICTION STATE
     C*
     C     RSKY          CHAIN     PRS01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     RSJRST        ANDEQ     'FL'
     C*
     C*  GET THE CURRENT DEDUCTIBLE DISCOUNT RECORD
     C*
     C                   Eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     C                   Select
     C                   When      xxDedSize = 'L'
     C                   Callp     rtvLgDedInfo (xxAmpo : xxAmag : xxclas)
     C                   When      xxDedSize = 'S'
     C                   Callp     rtvSmDedInfo (xxAmpo : xxAmag : xxclas)
     C                   EndSl

     C     xxclas        IfGE      ENSCF1                                       WITHIN RANGE 1
     C     xxclas        AndLE     ENSCT1                                        OR
     C     xxclas        OrGE      ENSCF2                                       WITHIN RANGE 2
     C     xxclas        AndLE     ENSCT2                                        OR
     C     xxclas        OrGE      ENSCF3                                       WITHIN RANGE 3
     C     xxclas        AndLE     ENSCT3
     C                   Z-Add     xxamag        FMA701
     C                   Leave
     C                   Endif                                                  DDCLAS >= ENSCF1

     C                   ENDIF                                                  *IN60 = *ON
        Endif;
     C*
     C     ELKY2         READE     PEL02                                  60
     C                   ENDDO
     C*
      *  If printing a policy page.  (Not called from W1054R)
        SRCHKPGM(#1PGM:#1FLAG);
        If #1Flag = '0';
           PrintingPolicy = *On;
        Else;
           PrintingPolicy = *Off   ;
        Endif;

       Endsr;
       //============================================================
       //  Pgmstr - Beginning of program processing
       //============================================================
       Begsr PgmStr;
       Endsr;
       //============================================================
       //  PgmEnd - End of program processing
       //============================================================
       Begsr PgmEnd;
       Endsr;
       //============================================================
       //  Valid - Subroutine to attach this form to the policy
       //============================================================
       Begsr Valid;

        Chain RSKY prs01;
        If %found and rsJrst = 'FL' and elPfdt <> elPtdt;

       // GET THE CURRENT DEDUCTIBLE DISCOUNT RECORD
           xxDedSize = rtvDedSize (elCO#:elBSCD);
           Select;
              When xxDedSize = 'L';
                 rtvLgDedInfo (xxAmpo : xxAmag : xxclas);
              When xxDedSize = 'S';
                 rtvSmDedInfo (xxAmpo : xxAmag : xxclas);
           EndSl;

           If (xxClas >= enScf1 and xxClas <=enSct1)
            or(xxClas >= enScf2 and xxClas <=enSct2)
            or(xxClas >= enScf3 and xxClas <=enSct3);
              crtFrm = *On;
           EndIf;
        EndIf;

       Endsr;
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
     D  ampo                               like(DDAMPO)
     D  amag                               like(DDAMAG)
     D  clas                               like(DDCLAS)
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
     C                   Eval      clas = DDCLAS
     C                   Endif

    P rtvSmDedInfo    e
     *=========================================================================
     * rtvLgDedInfo - retrieves LDAMPO and LDAMAG
     *=========================================================================
    P rtvLgDedInfo    b
    D rtvLgDedInfo    pi
     D  ampo                               like(DDAMPO)
     D  amag                               like(DDAMAG)
     D  clas                               like(DDCLAS)

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
     C                   Eval      clas = LDCLS1
     C                   Endif

    P rtvLgDedInfo    e

