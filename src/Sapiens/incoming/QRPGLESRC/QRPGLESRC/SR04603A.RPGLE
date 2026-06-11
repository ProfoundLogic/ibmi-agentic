     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

    H/TITLE SR04603A - CA - Large Risk Deductible Endorsement
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H bnddir('WTBNDDIR')
     *===============================================================*
     * WC040603A                                                     *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   SINGLE MANUAL ADDITIONAL INPUT                              *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 06/29/09                                          *
     * Programmer: C:61                                              *
     * Project   : WC1804                                            *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 10/31/22 C:333 Project: WC2834 (Recompile)                    *
     *                Endorsement Recognition of Cancellation        *
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
    
     ** Copybook F specs
     F/COPY SCOPYEN01F
    
     ** Additional Information File
     Fwxfmp1    if   e           k disk    usropn
     F                                     rename(pfm01:pfm02)
    
     *================================================================
     *  Definition Specs
     *================================================================
     ** Copybook D specs
     D/COPY SCOPYEN01D
    
     *================================================================
     *  /COPY required constants
     *================================================================
    
     ** Form ID
     D qqForm          s             15a   inz('WC040603A ')
    
     ** Additional entry program
     D addpgm          c                   const('SR04603I  ')
    
      //Entry
     D SR04603A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR04603A        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
     *==================================================================
     *  PLIST definitions
     *==================================================================
     ** Additional input program parameters
    
     *==================================================================
     *  KLIST definitions
     *==================================================================
     ** Additional Input
    
     C     fmky3         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    k1form           15
    
    
     *==================================================================
     *  Main
     *==================================================================
    
     ** Copybook C specs
     C/COPY SCOPYEN01C
    
    
     *==================================================================
     *  OTHINF - Subroutine to load form specific information & write
     *==================================================================
     C     othinf        begsr
    
     C                   open      wxfmp1
    
     C                   movel(p)  'WC040603'    k1form
     C     fmky3         chain     pfm02                              60
    
     ** Move the custom fields to print

     C                   move      f3am01        fmam01
     C                   move      f3am02        fmam02
     C                   move      f3am03        fmam03

     C                   if        f3fl01 = 'Y'
     C                   eval      fmc101 = 'Included'
     C                   else
     C                   eval      fmc101 = 'Excluded'
     C                   endif

     C                   if        f3fl02 = 'Y'
     C                   eval      fmc102 = 'Will'
     C                   else
     C                   eval      fmc102 = 'Will Not'
     C                   endif
    
     ** Write the output record
    
     C                   close     wxfmp1
    
     C                   endsr
    
     *===================================================================
     *  PGMSTR - Beginning of program processing
     *===================================================================
     C     pgmstr        begsr
     C                   endsr
    
     *===================================================================
     *  PGMEND - End of program processing
     *===================================================================
     C     pgmend        begsr
     C                   endsr
    
     *===================================================================
     *  VALID - Subroutine to attach this form to the policy
     *===================================================================
     C     valid         begsr
     C                   endsr
    
