     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')

     H bnddir('WTBNDDIR')
    H/TITLE SR02602 - AZ Benefits Deductible Endorsement
     *===============================================================*
     * WC020602                                                      *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   12/02/03  C:61                                      *
     *===============================================================*
     * REVISIONS:                                                    *
      *                                                               *
      * 08/21/25 C:333 Support: 148/117703  (Recompile)               *
      *                For single print endorsements provide specific *
      *                group and employer so that correct sig is found*
      *                                                               *
      * 10/27/22 C:333 Project: WC2834                                *
      *                Endorsement Recognition of Cancellation        *
      *                                                               *
      * 12/29/16 C:180 Project SY371 scriptura.                       *
      *                Add Scriptura interface.                       *
      *                                                               *
     * 10/15/12 C:290 Support: WA099 80901                           *
     *               -Add Large Deductible logic                     *
      *               -Chg'd Q1DATE field to Q2DATE (not commented)   *
      *                                                               *
      *                                                               *
      * 05/13/08 C:197 PRISS#5 Project WC1525                         *
      *                MADE CHANGES TO PRINT NXDEDF CORRECTLY ON THE  *
      *                ON THE FORM                                    *
      *                                                               *
      * 10/11/07 C:190 SUPPORT WA099 54504                            *
      *                MAKE SURE TO DO NOT ACCESS THE DEDUCTIBLE      *
      *                DISCOUNT CODE HAVING INACTIVE RECORDS.         *
      *                                                               *
     H* 01/09/07 C:184 PROJECT WC1525                                 *
     H*                Add changes for "Loss adjustment considered as *
     H*                advancements".                                 *
     H*                                                               *
     H*  04/10/06 C:65 PROJECT SUPP#51293                             *
     H*                CHANGE DEDUCTIBLE DISCOUNT TO PULL BASED ON    *
     H*                ANNIV RATING DATE INSTEAD OF POLICY EFF DATE.  *
     *                                                               *
     *                                                               *
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
     *
     *  Deductible Discount Code Table
     FWTDDL2    IF   E           K DISK
     *
     **** BEGIN ADD ***  01/09/07  ************************************
     FWMNXL     IF   E           K DISK
     **** END ADD ***  01/09/07  **************************************
     f*** beg add ***** 10/15/12 *******************************************
     fwtabl     if   e           k disk    include(pbs01)
     fwmldl1    if   e           k disk
     f*** end add ***** 10/15/12 *******************************************
     * Copybook F specs
     F/COPY SCOPYEN01F
    
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================

      **** Begin Add **** 12/29/16 *********************************************
      * Entry
     D SR02602         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR02602         PI
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
    
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
      *** Begin Change *** 12/29/16 ********************************************
     D*** xxform          c                   const('WC020602  ')
     D QQFORM          S             15A   inz('WC020602  ')
      *** End   Change *** 12/29/16 ********************************************
    
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
      *** beg add ***** 10/15/12 *******************************************
     d xxampo          s                   like(FMAM01)
     d xxamag          s                   like(FMAM01)
     d xxDedSize       s              1
     *=========================================================================
     *  Prototypes
     *=========================================================================
    d rtvDedSize      pr             1                                         return S/L
     d  co#                                like(ELCO#)
     d  bscd                               like(ELBSCD)
    d rtvSmDedInfo    pr
     d  ampo                               like(FMAM01)
     d  amag                               like(FMAM01)
    d rtvLgDedInfo    pr
     d  ampo                               like(FMAM01)
     d  amag                               like(FMAM01)
      *** end add ***** 10/15/12 *******************************************
    
    
     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
    
    
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
    
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
      *
      *  DEDUCTIBLE DISCOUNT CODE FILE
      *
     C     DDKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELDCDE
     C*** BEGIN CHG ***  04/10/06  ************************************
     C*************      KFLD                    ELPFDT
     C                   KFLD                    XXDDT             7 0
     C*** END CHG *****  04/10/06  ************************************
      *
     C     DDKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELDCDE
     **** BEGIN ADD ***  01/09/07  ************************************
     C     NXKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELPFDT
     C*
     C     NXKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    RSJRST
     **** END ADD ***  01/09/07  **************************************
    
     *=========================================================================
     *  Main
     *=========================================================================
    
     ** Copybook C specs
     C/COPY SCOPYEN01C
    
    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
    
     C*** BEGIN CHG ***  01/09/07  ************************************
     C*    elky2         chain     pel02
    
     c*                  dow       %found
     C     elky2         chain     pel02                              72
     c                   dow       *in72 = *off
     C*** END   CHG ***  01/09/07  ************************************
          //***** Beg Add *** 10/27/22 ******************************
             xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
             If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
          //***** End Add *** 10/27/22 ******************************
     c     rsky          chain     prs01

     c                   if        %found
     c                             and (rsjrst = enst)
     C*** BEGIN CHG ***  04/10/06  ************************************
     C*** BEG CHG ***** 10/15/12 **************************************
     C                   MOVE      ELDCEF        Q1DATE
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
     C                   PARM                    Q1DATE
     C*
     C                   MOVE      Q1DATE        XXDDT
     C*** END CHG *****  04/10/06  ************************************
     c     ddky          SETGT     pdd01
     c     ddky1         readpe    pdd01
      *
     c                   if        %found
     c                             and (DDAMPO <> 0)
     C*** BEG ADD *****  10/11/07  ************************************
     c                             and (DDSTAT = 'A')
     C*** END ADD *****  10/11/07  ************************************
     c                   z-add     ddampo        fma701
     C*** BEGIN CHG ***  01/09/07  ************************************
     C                   Eval      fmfl01 = ' '
     c     nxky1         SETGT     pnx01
     c     nxky2         readpe    pnx01                                  73
     C
     C                   if        *in73 = *off
     C*** BEG ADD  ******  01/09/07  ************************************
    c                   If        nxdedf = 'Y'
     c                   eval      fmfl01 = 'X '
     C*** BEG CHG  ******  05/13/08  ************************************
     c*******            eval      fmfl02 = ' '
     c                   eval      fmfl02 = '___'
     C*** END CHG  ******  05/13/08  ************************************
     c                   endif
    c                   if        nxdedf = 'N'
     C*** BEG CHG  ******  05/13/08  ************************************
     c*******            eval      fmfl01 = ' '
     c                   eval      fmfl01 = '___'
     C*** END CHG  ******  05/13/08  ************************************
     c                   eval      fmfl02 = 'X '
     c                   endif
    c                   If        nxdedf = ' '
     C*** BEG CHG  ******  05/13/08  ************************************
     c*******            eval      fmfl01 = ' '
     c*******            eval      fmfl02 = ' '
     c                   eval      fmfl01 = '___'
     c                   eval      fmfl02 = '___'
     C*** BEG CHG  ******  05/13/08  ************************************
     c                   endif
     C*** END ADD  ******  01/09/07  ************************************
     C                   endif
     C*** END CHG *****  01/09/07  ************************************
     c                   leave
     c                   endif

     c                   endif
           //***** Beg Add *** 10/27/22 ******************************
             Endif;
           //***** End Add *** 10/27/22 ******************************
     C*** BEGIN CHG *****  01/09/07  ************************************
     c*                  reade     pel02
     c     elky2         reade     pel02                                  72
     C*** END   CHG *****  01/09/07  ************************************
     c                   enddo

    c
      *** Begin Delete *** 12/29/16 ********************************************
     C***                write     pfm01
      *** End   Delete *** 12/29/16 ********************************************
     C                   endsr
    
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     *
     C                   endsr
    
    
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     *
     C                   endsr
    
    
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
     *
     C     rsky          chain     prs01                              60
     *
     C                   if        *in60 = *off
     C                             and rsjrst = enst

      *  RETRIEVE THE DEDUCTIBLE RECORD

     C*** BEGIN CHG ***  04/10/06  ************************************
     C                   MOVE      ELDCEF        Q1DATE
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
     C                   PARM                    Q1DATE
     C*
     C                   MOVE      Q1DATE        XXDDT
     C*** END CHG *****  04/10/06  ************************************
     C     ddky          SETGT     pdd01
     C     ddky1         readpe    pdd01                                  60
      *
      *  IF A RECORD IS FOUND, AND THERE IS A PER OCCURRENCE DEDUCTIBLE,
      *  THEN PRINT THE ENDORSEMENT FORM
      *
     C                   if        %found
     C                             and (DDAMPO <> 0)
     C*** BEG ADD *****  10/11/07  ************************************
     c                             and (DDSTAT = 'A')
     C*** END ADD *****  10/11/07  ************************************
     *
     ** Found a division with a matching jurisdiction state
     C                   move      *on           crtfrm
     C                   endif
     *
     C                   endif
     *
     C                   endsr
    
    
