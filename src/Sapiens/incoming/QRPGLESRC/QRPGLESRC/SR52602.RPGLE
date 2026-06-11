     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')

    H/TITLE SR52602 - HI - Notification Endorsement
     *===============================================================*
     * WC520602 - Hawaii Notification Endorsement                    *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 05/04/01                                          *
     * Programmer: C: 90                                             *
     * Project   : CM027                                             *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 08/21/25 C:333 Support: 148/117703  (Recompile)               *
      *                For single print endorsements provide specific
      *                group and employer so that correct sig is found
     *                                                               *
     * 02/19/24 C:333 Project: WC2942                                *
      *                HI Endorsement WC520602 Schedule Data
     *                                                               *
     * 09/15/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -No comments.
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
     *
     ** Group Detail File
     Fwdf1l     if   e           k disk    usropn
     *
     ** Additional Information File
     Fwxfmp1    if   e           k disk    usropn
     F                                     rename(pfm01:pfm02)
      *** Beg Add ***** 02/19/24 *******************************
     *
     ** Endorsement Forms Statistical Info File
     Fwmnxl     if   e           k disk
      *** End Add ***** 02/19/24 *******************************
     *=========================================================================
     *  Definition Specs
     *=========================================================================
      *
     ** Copybook D specs
     D/COPY SCOPYEN01D
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
     ** Form ID
     D qqForm          s             15a   inz('WC520602  ')
     ** Additional entry program
     D addpgm          c                   const('SRxxxxxxxx')

      *entry parameters
     D SR52602         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR52602         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
     ** Group Detail file
     C     f1ky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     *
     ** Additional Input file
     C     fmky3         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxForm
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
     *
      //*** Beg Add ***** 02/19/24 **********************************
       Setgt (xxco#:xxfnd:'HI':ELpfdt) pnx01;
       Readpe (xxco#:xxfnd:'HI') pnx01;
       If not %Eof(WMNXL);
          //Load WMNXP fields into the workfile WXWXFMP
          FM5211 = NXMCareA;
          FM5212 = NXMCareB;
          FM5213 = NXWgLossA;
          FM5214 = NXWgLossB;
          FM5215 = NXIBenDisA;
          FM5216 = NXIBenDisB;
          FM5217 = NXDBenA;
          FM5218 = NXDBenB;
          FM5219 = NXAdmLglA;
          FM5220 = NXAdmLglB;
          FM5221 = NXMiscA;
          FM5222 = NXMiscB;
       Endif;
      //*** End Add ***** 02/19/24 **********************************
     C                   open      wxfmp1
     C                   open      wdf1l
     *
     C     fmky3         chain     pfm02                              60
     C     f1ky          chain     pf101                              60
     *
     ** Move the custom fields to print
     *
     C                   close     wxfmp1
     C                   close     wdf1l
     *
     C                   endsr
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
     *
      *** Beg Add ***** 02/19/24 *******************************
         crtFrm = *On;
      *** End Add ***** 02/19/24 *******************************
     *
     C                   endsr
