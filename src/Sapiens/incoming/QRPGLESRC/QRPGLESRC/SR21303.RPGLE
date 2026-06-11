     *===============================================================*
     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H bnddir('WTBNDDIR')
    H/TITLE SR43602 - MICHIGAN NOTICE TO POLICY HOLDER ENDORSEMENT
     *===============================================================*
     * WC210303 - Michigan Notice to Policyholder Endorsement        *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 08/02/02                                          *
     * Programmer: C:09                                              *
     * Project   : CM027                                             *
     *===============================================================*
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
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/03/22 C:333 Project: WC2834 (Recompile)                    *
     H*                Endorsement Recognition of Cancellation        *
     H*                                                               *
     H* 12/21/21 C:333 Support: 99276 (AmWINS)                        *
     H*                MI WC210303A - State National Contact Detail   *
     H*                 Picking Company name/Address, instead of group*
     H*                 name/Address.                                 *
      *                                                               *
      * 02/27/17 C:180 Project SY371 scriptura.                       *
      * 01/05/17       Add Scriptura interface.                       *
     H*                                                               *
     H* 07/21/11 C:09  Seq#: 77043                                    *
     H*                Retrieve Company Information to print on the   *
     H*                endorsement form.                              *
     H*                                                               *
     H*****************************************************************
     ** Copybook F specs
     FSMCOL     IF   E           K DISK
     F/COPY SCOPYEN01F
     *=========================================================================
     *  Definition Specs
     *=========================================================================
      * Entry
     D SR21303         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR21303         PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1

     ** Copybook D specs
     D/COPY SCOPYEN01D
     D*** Beg Add ****** 08/09/11 ************************************
     D                 DS
     D  xxpzip                 1      9  0
     D  xxpzp1                 1      5
     D  xxpzp2                 6      9
     D                 DS
     D  xxmzip                 1      9  0
     D  xxmzp1                 1      5
     D  xxmzp2                 6      9
     D*** End Add ****** 08/09/11 ************************************
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
     ** Form ID
     D QQFORM          S             15A   inz('WC210303A   ')
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
     D*** Beg Add ****** 08/09/11 ************************************
     D grpAdr          s             60    dim(5)
     D xxgcsz          s             40    inz(*blanks)
     D AdrLn           s              3  0 inz(*zeros)
     D*** End Add ****** 08/09/11 ************************************
     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
     *
     *
     *=========================================================================
     *  PLIST definitions
     *=========================================================================
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
    C*
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
     C     othinf        BEGSR
     *
     C*
     ** Write the output record
     D*** Beg Add ****** 08/09/11 ************************************
      *
     c                   movea     *blanks       grpAdr
     c                   eval      fmc601 = *blanks
     c                   eval      fmc402 = *blanks
     c                   eval      fmc403 = *blanks
     c                   eval      fmc404 = *blanks
     c                   eval      fmc405 = *blanks
      /Free
       If FNanam <> *blanks;
      /End-Free
     c                   eval      adrLn = 1
     c                   movel     fnanam        grpAdr(adrLn)
     c*
     c                   if        fnaad1 <> *blanks
     c                   eval      adrLn = adrLn + 1
     c                   movel     fnaad1        grpAdr(adrLn)
     c                   if        fnaad2 <> *blanks
     c                   eval      adrLn = adrLn + 1
     c                   movel     fnaad2        grpAdr(adrLn)
     c                   endif
     c                   eval      xxgcsz = *blanks
     c                   eval      xxmzip = fnazip
     c                   movel     fnacty        xxgcsz
     c     xxgcsz        cat       ',':0         xxgcsz
     c     xxgcsz        cat       fnast:1       xxgcsz
     c     xxgcsz        cat       xxmzp1:2      xxgcsz
     c                   if        xxmzp2 <> '0000'
     c     xxgcsz        cat       '-':0         xxgcsz
     c     xxgcsz        cat       xxmzp2:0      xxgcsz
     c                   endif
     c                   eval      adrLn = adrLn + 1
     c                   movel     xxgcsz        grpAdr(adrLn)
     c                   endif
     c*
      /Free
       Else;
       //*** Beg Chg ***** 12/21/21 ****************************
         //***   Chain xxco# SMCOL;
         //***   If %found(SMCOL);
         //***      adrLn = 1;
         //***      GrpAdr(adrLn) = COname;
         //***      If COadr1 <> *blanks;
         //***       adrLn = AdrLn + 1;
         //***       GrpAdr(AdrLn) = COadr1;
         //***    Endif;
         //***    If COadr2 <> *blanks;
         //***       adrLn = AdrLn + 2;
         //***       GrpAdr(AdrLn) = COadr2;
         //***    Endif;

         //***    xxgcsz = *blanks;
         //***    xxmzip = COzip;
         //***    If xxmzp2 <> '0000';
         //***       xxgcsz = %Trim(COcty) + ', ' + %Trim(COst) + '  ' + xxmzp1 +
         //***                '-' + xxmzp2;
         //***    Else;
         //***       xxgcsz = %Trim(COcty) + ', ' + %Trim(COst) + '  ' + xxmzp1;
         //***    Endif;
         //***    AdrLn = AdrLn + 1;
         //***    GrpAdr(AdrLn) = xxgcsz;
         //***   Endif;
      /End-Free
     c                   eval      adrLn = 1
     c                   movel     FNname        grpAdr(adrLn)
     c*
     c                   if        FNmad1 <> *blanks
     c                   eval      adrLn = adrLn + 1
     c                   movel     FNmad1        grpAdr(adrLn)
     c                   if        FNmad2 <> *blanks
     c                   eval      adrLn = adrLn + 1
     c                   movel     FNmad2        grpAdr(adrLn)
     c                   endif
     c                   eval      xxgcsz = *blanks
     c                   eval      xxmzip = FNmzip
     c                   movel     FNmcty        xxgcsz
     c     xxgcsz        cat       ',':0         xxgcsz
     c     xxgcsz        cat       FNmst:1       xxgcsz
     c     xxgcsz        cat       xxmzp1:2      xxgcsz
     c                   if        xxmzp2 <> '0000'
     c     xxgcsz        cat       '-':0         xxgcsz
     c     xxgcsz        cat       xxmzp2:0      xxgcsz
     c                   endif
     c                   eval      adrLn = adrLn + 1
     c                   movel     xxgcsz        grpAdr(adrLn)
     c                   endif
     c*
      /Free
       //*** End Chg ***** 12/21/21 ****************************
       Endif;
      /End-Free
     c                   eval      fmc601 = grpAdr(1)
     c                   eval      fmc402 = grpAdr(2)
     c                   eval      fmc403 = grpAdr(3)
     c                   eval      fmc404 = grpAdr(4)
     c                   eval      fmc405 = grpAdr(5)
      *
     D*** End Add ****** 08/09/11 ************************************
     *
     *
     C                   endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
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
     c*** Beg Chg ****** 08/09/11 ************************************
     C***********                  and rsjrst = enst
     C                             and rsjrst = 'MI'
     c*** End Chg ****** 08/09/11 ************************************
     *
     *
     ** Found a division with a matching jurisdiction state
     C                   move      *on           crtfrm
     *
     C                   endif
     *
     C                   endsr
