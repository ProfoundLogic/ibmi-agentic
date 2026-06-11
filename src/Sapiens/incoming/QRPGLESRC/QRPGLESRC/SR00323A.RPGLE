     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H BndDir('WTBNDDIR')

      /TITLE SR00323A - Multiple Coordinated Policy Endorsement Print/Attach Program
      //===============================================================*
      // WC000323A - Multiple Coordinated Policy Endorsement           *
      //   Single automatically attached endorsement                   *
      //                                                               *
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      //                                                               *
      // Created   : 11/02/17                                          *
      // Programmer: C:267                                             *
      // Project   : WC2556                                            *
      //                                                               *
      //===============================================================*
      // REVISIONS:                                                    *
      //                                                               *
      // 01/27/23 C:291 Project: WC2863                                *
      //                State Include/Exclude Endorsement Print Control*
      //                                                               *
      // 10/26/22 C:333 Project: WC2834                                *
      //                Endorsement Recognition of Cancellation.       *
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

     FWDRRL     if   e           k disk
     FWDELL8    IF   E           K DISK
     F                                     RENAME(PEL01:PELL2)

     d PEO_ead1        s             50
     d PEO_ead2        s             50
     d PEO_cty         s             25
     d PEO_st          s              2
     D                 ds
     d PEO_zip                        9  0
     D  @@zip5                        5    overlay(peo_zip)
     D  @@zip4                        4    overlay(peo_zip:6)
     d PEO_ead         s             46

     D                 ds
     D peo_name                     100
     D  fmc401                             overlay(peo_name)
     D  fmc402                             overlay(peo_name:41)
     D  fmc201                             overlay(peo_name:81)
      **               ds
      *con_name                     100
      * fmc403                             overlay(con_name)
      * fmc404                             overlay(con_name:41)
      * fmc202                             overlay(con_name:81)

      /COPY SCOPYEN01D
      ***** Beg Del *** 10/26/22 **************************************
     D***/Copy WprEmplr
      ***** End Del *** 10/26/22 **************************************

     d con_name        s            100
     d #1co#           s              3    inz(*blanks)
     d #1fnd           s              3    inz(*blanks)
     d ##rtcd          s              3    inz(*blanks)
     d ##clas          s              5    inz(*blanks)
     d ##efdt          s              7    inz(*blanks)
     d ##lcm           s              5    inz(*blanks)
     d xxlcm           s              5  4

     d kkefdt          s                   like(elpfdt)

     D cxDate          s                   like(emcad1)
     D canxPol         s              1  0

     d x1              s              3  0
     D qqform          s             15A   Inz('WC000323A ')
     D addpgm          c                   const('SRXXXXXXXX')

     D SR00323A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR00323A        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      *  Statistical Reporting/Rating
     C     rrky          KList
     C                   KFld                    ELCO#
     C                   KFld                    ELFND
     C                   KFld                    RSJRST
      *
     C     rrky1         KList
     C                   KFld                    ELCO#
     C                   KFld                    ELFND
     C                   KFld                    RSJRST
     C                   KFld                    kkefdt
      ** Employee master file
     c     emky1         klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    xxemp#            9 0
     c                   kfld                    xxdiv             5 0

     c     emky2         klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv

     c     elky8         klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    elfyr

     C     rpky1         KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFLD                    xxemp#
     C                   KFLD                    xxdiv
     C                   KFLD                    xxfyr

      /COPY SCOPYEN01C
      *=========================================================================
      *  OTHINF - Subroutine to load form specific information & write record
      *=========================================================================
     C     othinf        begsr
     C                   if        fnpeof = 'M' or fnpeof = 'C'
     c     elky8         setll     pell2
     c     elky8         reade     pell2                                  60
     c                   dow       *in60 = *off
      ** Check cancelled policy
     c                   move      *blanks       canxpol
     c                   move      *blanks       PEO_ead
     C                   eval      cxDate = GetCxDate(elco#:elfnd:elemp#:eldiv)
        //***** Beg Add *** 10/26/22 ******************************
           If not(cxDate <= ELpfdt and xxCxDate <> *zeros);
        //***** End Add *** 10/26/22 ******************************
              //*** Beg Add ***** 01/27/23 ************************
              Chain rsky prs01;
              Exsr ChkApprExclSt;
              If isApprExclSt = *On;
             //*** End Add ***** 01/27/23 *************************

     C                   if        cxDate = 0 or cxDate > fmdt
     C                   eval       canxPol = 1
     c                   endif

     c                   if         canxpol = 1
      ** Load Contract or Project
      /free
         con_name = 'Any applicable project';
         FM2001 = con_name;
      /End-Free

      ***  emky2         chain     pem01
      **                 if        %found
      **                 eval      con_name= emname + emnam2
      **                 eval      FM2001 = con_name
      ***                endif

      * Get PEO  information
     c                   eval      xxemp# = 1
     c                   eval      xxdiv  = 0
     c     emky1         chain     pem01
     c                   if        %found
     c                   eval      PEO_name =emname + emnam2
     C                   eval      FM2002 = PEO_name
     c                   if        (emmpfl = 'P')
      * Physical Address
     c                   eval      PEO_ead1 = empad1
     c                   eval      PEO_ead2 = empad2
     c                   eval      PEO_cty  = empcty
     c                   eval      PEO_st   = empst
     c                   eval      PEO_zip  = empzip

     c                   else
      * Mailing Address
     c                   eval      PEO_ead1 = emmad1
     c                   eval      PEO_ead2 = emmad2
     c                   eval      PEO_cty  = emmcty
     c                   eval      PEO_st   = emmst
     c                   eval      PEO_zip  = emmzip
     c                   move      peo_zip       zip               9
     c                   endif

     C                   MoveL     PEO_ead1      fmnm01
     C                   MoveL     PEO_ead2      fmnm02
     c     PEO_CTY       cat       ',':0         PEO_ead
     C                   cat       PEO_ST:1      PEO_ead
     C                   cat       @@zip5:2      PEO_ead
     C                   if        @@zip4 <> '0000'
     C                   cat       '-':0         PEO_ead
     C                   cat       @@zip4:0      PEO_ead
     c                   endif
     C                   MoveL     PEO_ead       fmnm03

     c                   if        fmnm02 = *blanks
     C                   MoveL     fmnm03        fmnm02
     c                   move      *blanks       fmnm03
     c                   endif
      **State Where Work Performed
     C     rsky          chain     prs01                              60
     C                   MoveL     rsjrst        fmc101
      ** Labor Contractor Policy Number
     C                   open      wmrpl
     c                   eval      xxemp# = 1
     c                   eval      xxdiv  = 0
     C     rpky1         Chain     prp01                              20
     c                   move      *blanks       fmc203
     c                   if        *in20 = *off
     C                   MoveL     rpcap#        fmc203
     c                   endif
     C                   close     wmrpl

      ** if no carrior Policy Number, use standard policy#
     C                   if        fmc203 = *blanks
     C                   MoveL     fmpol#        fmc203
     C                   endif

     c                   endif
      ** Write the output record
      /Free
           WriteInCopybook = *Off;
           Exsr WriteWorkFile;
      /End-Free
     c                   move      *blanks       emname
     c                   move      *blanks       emnam2
     c                   endif
              //***** Beg Add *** 01/27/23 ******************************
              Endif;
              //***** End Add *** 01/27/23 ******************************
        //***** Beg Add *** 10/26/22 ******************************
           Endif;
        //***** End Add *** 10/26/22 ******************************
     c     elky8         reade     pell2                                  60
     c                   enddo

     C                   endif
     C                   endsr
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     pgmstr        begsr
     C                   endsr
      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     C     pgmend        begsr
     C                   endsr
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     C     valid         begsr

     C                   open      wmfnl
     C     fnky          Chain     pfn01                              20

     C                   if        %found(wmfnl)
     c                   if        fnpeof = 'M' or fnpeof = 'C'
      ** Found a division with a matching jurisdiction state
     C                   move      *on           crtfrm
     C                   endif
     C                   endif

     C                   close     wmfnl

     C                   endsr
