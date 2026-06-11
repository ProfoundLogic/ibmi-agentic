     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR04101A - CA - Longshore and Harbor Workers' Comp Act
      //===============================================================*
      // WC040101A                                                     *
      //                                                               *
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      //===============================================================*
      // REVISIONS:                                                    *
      //                                                               *
      // 08/21/25 C:333 Support: 148/117703  (Recompile)               *
      //                For single print endorsements provide specific *
      //                group and employer so that correct sig is found*
      //                                                               *
      // 06/24/24 C:344 Project: WC2952                                *
      //                Revise the rate fields from zoned 5.2 to 8.3.  *
      //                                                               *
      // 10/27/22 C:333 Project: WC2834                                *
      //                Endorsement Recognition of Cancellation        *
      //                                                               *
      // 02/14/17 C:180 Support: 41024/SY371                           *
      //                Recompiled only.                               *
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of revisions.           *
      // ------------------------------------------------------------- *
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

     Fwmepl7    if   e           k disk
     Fwmprl1    if   e           k disk
     F
      /COPY SCOPYEN01D

     D a70ds           ds
     D  FMA701
     D  FMA702
     D  FMA703
     D  FMA704
     D  FMA705
     D a70                                 OVERLAY(a70ds)
     D                                     LIKE(fma701) DIM(5)

     D c30ds           ds
     D  FMC301
     D  FMC302
     D  FMC303
     D  FMC304
     D  FMC305
     D c30                                 OVERLAY(c30ds)
     D                                     LIKE(fmc301) DIM(5)

     D  am1ds          ds
     D  FMAM11
     D  FMAM12
     D  FMAM13
     D  FMAM14
     D  FMAM15
     D am1                                 OVERLAY(am1ds)
     D                                     LIKE(fmam11) DIM(5)

     D a521ds          DS
      **** Beg Chg **** 06/24/24 *******************************************
     D***FM5211
     D***FM5212
     D***FM5213
     D***FM5214
     D***FM5215
     D FMRT54
     D FMRT55
     D FMRT56
     D FMRT57
     D FMRT58
      **** End Chg **** 06/24/24 *******************************************
     D a521                                OVERLAY(a521ds)
      **** Beg Chg **** 06/24/24 *******************************************
     D***                                  LIKE(fm5211) DIM(5)
     D                                     LIKE(FMRT54) DIM(5)
      **** End Chg **** 06/24/24 *******************************************

     D am2ds           DS
     D  FMAM21
     D  FMAM22
     D  FMAM23
     D  FMAM24
     D  FMAM25
     D am2                                 OVERLAY(am2ds)
     D                                     LIKE(fmam21) DIM(5)

     D qqForm          s             15A   Inz('WC040101A ')
     D addpgm          c                   const('SRXXXXXXXX')
     d svclas          S                   like(epclas)
     d total           S                   like(epempr)
     D x1              s              2  0

     D SR04101A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR04101A        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      *  Payroll class
     c     prky          klist
     c                   kfld                    elco#
     c                   kfld                    elrtcd
     c                   kfld                    epclas
     c                   kfld                    epefdt

     c     prky1         klist
     c                   kfld                    elco#
     c                   kfld                    elrtcd
     c                   kfld                    epclas
      *  Employer Payroll
     c     epky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elfyr
     c                   kfld                    elemp#
     c                   kfld                    eldiv

     c     epky1         klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elfyr
     c                   kfld                    elemp#
     c                   kfld                    eldiv
     c                   kfld                    svclas

      /COPY SCOPYEN01C

      *=========================================================================
      *  OTHINF - Subroutine to load form specific information & write record
      *=========================================================================
     C     othinf        begsr
      /Free
          WriteInCopybook = *Off;
      /End-Free
     c                   eval      x1 = 0
     c                   clear                   a70ds
     c                   clear                   c30ds
     c                   clear                   am1ds
     c                   clear                   a521ds
     c                   clear                   am2ds
     c                   clear                   fmam20

     C     elky2         chain     pel02                              60

     c                   dow       *in60 = *off

          //***** Beg Add *** 10/27/22 ******************************
             xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
             If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
          //***** End Add *** 10/27/22 ******************************
     C     epky          chain     pep01                              60

     c                   dow       *in60 = *off
     c                   eval      svclas = epclas

     c                   if        ((*in60 = *off) and
     c                             (epstat = 'A') and
     c                              (epspec = 'F'))

     c                   exsr      addfrm
     C     epky1         setgt     pep01
     c                   endif

     C     epky          reade     pep01                                  60
     c                   enddo

           //***** Beg Add *** 10/27/22 ******************************
             Endif;
           //***** End Add *** 10/27/22 ******************************
     C     elky2         reade     pel02                                  60
     c                   enddo

     c                   if        x1 > 0
      /Free
          Exsr WriteWorkFile;
      /End-Free
     c                   endif

     C                   endsr
      *=========================================================================
      *  ADDFRM - add to ouput printed.
      *=========================================================================
     C     addfrm        begsr
     c                   eval      x1 = x1 + 1

      * new record if page full
     c                   if        x1 > 5
      /Free
          Exsr WriteWorkFile;
      /End-Free
     c                   eval      x1 = 1
     c                   clear                   a70ds
     c                   clear                   c30ds
     c                   clear                   am1ds
     c                   clear                   a521ds
     c                   clear                   am2ds
     c                   clear                   fmam20
     c                   endif

      * add payroll class to output

     c                   eval      a70(x1) = epclas

     c                   clear                   ppr01
     c     prky          setgt     ppr01
     c     prky1         readpe    ppr01                                  60
     c                   eval      c30(x1) = prdesc

     c                   eval      am1(x1) = epeapr
     c                   eval      a521(x1) = eparat
     c                   eval      am2(x1) = epempr
     c                   eval      fmam20  = fmam20 + epempr
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

     C     epky          chain     pep01                              60

     c                   dow       *in60 = *off

     c                   if        ((*in60 = *off) and
     c                              (epstat <> 'I') and
     c                              (epspec = 'F'))
     C                   move      *on           crtfrm
     c                   leave
     c                   endif

     C     epky          reade     pep01                                  60
     c                   enddo

     C                   endsr
