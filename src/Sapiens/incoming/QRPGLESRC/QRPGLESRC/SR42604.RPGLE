     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')

     H/TITLE SR42604 - TX - Accident & Aggregate Deductible Endorsement
      *===============================================================*
      * WC420604 - Texas Accident & Aggregate Deductible Endorsement  *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      * Created   : 03/20/00                                          *
      * Programmer: C:90                                              *
      * Project   : CM027                                             *
      *===============================================================*
      * REVISIONS:                                                    *
     *                                                               *
      * 10/19/22 C:342 Project: WC2834                                *
      *                Endorsement Recognition of Cancellation        *
      *                                                               *
     * 09/15/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -No comments.
      *                                                                        *
      *11/27/13 C:287 Support: WA099 80909                                    *
      *                - Add Large Deductible logic                            *
      *
      * 01/04/13 C:290 Support SEQ79989                               *
      *                Add if condition to process record if          *
      *                DDSTAT = 'A'. Comment changes made under       *
      *                support SEQ54504.                              *
      *                                                               *
      * 07/14/11 C:061 Support 77336
      *              - Convert source to RPGIV.
      *              - Do not attach for flat cancelled divisions.
     H*                                                               *
     H* 10/11/07 C:190 SUPPORT WA099 54504                            *
     H*                MAKE SURE TO DO NOT ACCESS THE DEDUCTIBLE      *
     H*                DISCOUNT CODE HAVING INACTIVE RECORDS.         *
     H*                                                               *
     H*  04/10/06 C:65 PROJECT SUPP#51293                             *
     H*                CHANGE DEDUCTIBLE DISCOUNT TO PULL BASED ON    *
     H*                ANNIV RATING DATE INSTEAD OF POLICY EFF DATE.  *
      *                                                               *
      * 08/30/01  C:90 PROJECT: CM027 WA107/28711                     *
      *                Read thru all divisions within the OTHINF      *
      *                subroutine.                                    *
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
     FWTDDL2    IF   E           K DISK
     Fwtabl     if   e           k disk    include(pbs01)
     Fwmldl1    if   e           k disk
     F/COPY SCOPYEN01F
      *=========================================================================
      * Arrays
      *=========================================================================
      *
      *
      *=========================================================================
      * /COPY required constants
      *=========================================================================
      **Form ID
     D qqForm          s             15a   inz('WC420604B ')

      **Additional entry program
     D ADDPGM          C                   CONST('SRXXXXXXX ')
     D xxampo          s                   like(FMAM01)
     D xxamag          s                   like(FMAM01)
     D xxDedSize       s              1
      *=========================================================================
      * Prototypes
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

     D/COPY SCOPYEN01D

      *entry parameters
     D SR42604         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR42604         PI
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
     C     DDKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELDCDE
     C                   KFLD                    XXDT              7 0
     C*
     C     DDKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELDCDE
      *=========================================================================
      * Main
      *=========================================================================
      **Copybook C specs
     C/COPY SCOPYEN01C
      *
      *=========================================================================
      * OTHINF- Subroutine to load form specific information & write record
      *=========================================================================
     C     OTHINF        BEGSR
      /Free
       WriteInCopybook = *Off;
      /End-Free
      *
     C     ELKY2         SETLL     PEL02
     C     ELKY2         READE     PEL02                                  60
     C     *IN60         DOWEQ     *OFF
        //***** Beg Add *** 10/19/22 ******************************
        xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
        If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
        //***** End Add *** 10/19/22 ******************************
     C     RSKY          CHAIN     PRS01                              60
      *
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
      /End-Free
     C                   LEAVE
      *
     C                   ENDIF                                                  *IN60 = *OFF
      *
        //***** Beg Add *** 10/19/22 ******************************
        Endif;
        //***** End Add *** 10/19/22 ******************************
     C     ELKY2         READE     PEL02                                  60
     C                   ENDDO
      *
     C                   ENDSR
      *=========================================================================
      * PGMSTR- Beginning of program processing
      *=========================================================================
     C     PGMSTR        BEGSR
      *
      *
     C                   ENDSR                                                  PGMSTR
      *=========================================================================
      * PGMEND- End of program processing
      *=========================================================================
     C     PGMEND        BEGSR
      *
      *
     C                   ENDSR                                                  PGMEND
      *=========================================================================
      * VALID- Subroutine to attach this form to the policy
      *=========================================================================
     C     VALID         BEGSR
      *
     C     RSKY          CHAIN     PRS01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     RSJRST        ANDEQ     ENST
     C                   Eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     C                   If        ENST = RSJRST
     C                   Select
     C                   When      xxDedSize = 'L'
     C                   Callp     rtvLgDedInfo (xxAmpo: xxAmag)
     C                   When      xxDedSize = 'S'
     C                   Callp     rtvSmDedInfo (xxAmpo: xxAmag)
     C                   Endsl
     C                   If        xxAmpo <> 0
      ** Found a division with a matching jurisdiction state
     C                   MOVE      *ON           CRTFRM
     C                   EndIf
     C                   ENDIF                                                  *IN60 = *OFF
     C                   ENDIF                                                  *IN60 = *OFF
      *
     C                   ENDSR                                                  VALID

      *=========================================================================
      *rtvDedSize- Query whether policy is small or large deductible
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
      *rtvSmDedInfo - retrieves DDAMPO and DDAMAG
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
      *rtvLgDedInfo- retrieves LDAMPO and LDAMAG
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

