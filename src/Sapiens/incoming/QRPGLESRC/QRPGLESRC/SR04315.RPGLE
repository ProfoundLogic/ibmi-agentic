     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

    H/TITLE SR04315 - CA - Labor Contractor as Named Insured with LCF
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
     * WC040315 - Endorsement Limiting and Restricting this Insurance
     *            Labor Contractor as Named Insured with LCF designation
     *            Restriction of Coverage to Client Workers
     *
      *   Single Manual attached endorsement
     *   with no additional information entry needed                 *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
      * 10/28/22 C:333 Project: WC2834 (Recompile)                    *
      *                Endorsement Recognition of Cancellation        *
     *                                                               *
      * 03/29/18 C:65  Project 150WAD/Phw#70035                       *
      *                Read WDRRP for the output control flag.        *
     *                                                               *
      * 10/18/17 C:180 Project 150WAP                                 *
      *                Scrub for peo flexible control.                *
     *                                                               *
      * 08/14/17 C:180 Project 148WAF                                 *
      *                Add new attachment criteria for Combined grp.  *
     *                                                               *
      * 03/02/17 c:62 Project WC2481
      *               Add PEO logic
     *                                                               *
     * 09/15/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -No comments.
     *                                                               *
     * 07/27/11  C:190 Project WC2064                                *
     *                 Show Federal name.                            *
     *                                                               *
     * 04/08/11  C:190 Project WC1967                                *
     *                 Attach if 'Employee Leasing ID' has value     *
     *                 '4'.                                          *
     *                                                               *
     * 06/09/08  C:61  Project 136WDS                                *
     *                 Created.                                      *
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
      *** Begin Add    *** 08/25/17 ***********************************
     fwdrrl     if   e           k disk
      *** End   Add    *** 08/25/17 ***********************************
    
     ** Copybook F specs
     F/COPY SCOPYEN01F
    
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     ** Copybook D specs
     D/COPY SCOPYEN01D
    
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D qqForm          s             15a   inz('WC040315  ')
    
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
    
    
     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
    
     *entry parameters
     D SR04315         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR04315         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
    
    
     c     emky2         klist
     c                   KFld                    elco#
     c                   KFld                    elfnd
     c                   KFld                    elemp#
     c                   KFld                    eldiv
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
     c     othinf        begsr
    
     * Employer Name (split to print on two 60 char lines, if needed)
    
     c                   movel(p)  fminnm        ##line
     c                   eval      ##length = 60

     c                   call      'SRLINE'
     c                   parm                    ##line          256
     c                   parm                    ##length          3 0
     c                   parm                    ##remain        256
    
     c                   movel(p)  ##line        fmc601
     c                   movel(p)  ##remain      fmc602
     c**** Print Federal name.
     c                   movel(p)  fmifnm        ##line
     c                   eval      ##length = 60

     c                   call      'SRLINE'
     c                   parm                    ##line          256
     c                   parm                    ##length          3 0
     c                   parm                    ##remain        256
    
     c                   movel(p)  ##line        fm2001
     c                   movel(p)  ##remain      fm2002
    
     c                   endsr
    
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     C                   endsr
    
    
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr
    
    
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
    
      /Free
       //*** begin add ***  03/29/18  *************************************
       Chain (xxCo#:ELrtcd) WMRSL;
       If %Found;

         Setgt (ELco#:ELfnd:RSjrst:ELPfdt) WDRRL;
         Readpe (ELco#:ELfnd:RSjrst) WDRRL;
         If not %eof;
       //*** end add *****  03/29/18  *************************************
           If EMlsid = '5' and RRpgfl = '3';
              Crtfrm = *on;
           Endif;
       //*** begin add ***  03/29/18  *************************************
         Endif;
       Endif;
       //*** end add *****  03/29/18  *************************************
      /End-Free
    
     C                   endsr
    
