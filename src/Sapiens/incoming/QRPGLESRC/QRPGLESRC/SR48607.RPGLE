     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')

    H/TITLE SR48607 - WI - Assumption of Liability Endorsement
     *===============================================================*
     *Program Description:                                         *
     *                                                               *
     *   "Print/Attach" program for single manual attach endorsement *
     *   with addtional input program that stores information        *
     *   in generic endorsement data file wledp.                     *
     *                                                               *
     *===============================================================*
     *Revisions:                                                   *
     *                                                               *
      * 10/19/22 C:342 Project: WC2834 (Recompile)                    *
      *                Endorsement Recognition of Cancellation        *
     *                                                               *
     * 09/15/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -No comments.
     *                                                               *
     * 12/11/12 C:287 Project WC2129                                 *
     *                Created.                                       *
     *                                                               *
     *===============================================================*
     *Notes:                                                       *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
     *                                                               *
     *  QQMODE = 1 - Called to determine if form should be attached. *
     *  QQMODE = 2 - Called to print endorsement.                    *
     *  QQMODE = 3 - Called for additional input.                    *
     *  QQMODE = 4 - Called for mid-term processing.                 *
     *===============================================================*
    
     **Copybook F specs
     F/COPY SCOPYEN01F
    
     **Additional Information File
     Fwledl     if   e           k disk    usropn
    F                                     prefix(led_)
     *=========================================================================
     *  Definition Specs
     *=========================================================================
      *
     **Copybook D specs
     D/COPY SCOPYEN01D
    
     *=========================================================================
     * /COPY required constants
     *=========================================================================
    
     **Form ID
     D qqForm          s             15a   inz('WC480607  ')
    
     **Additional entry program
     D addpgm          c                   const('SR48607I  ')
    
      *entry parameters
     D SR48607         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR48607         PI
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
     * KLIST definitions
     *=========================================================================
     **Additional Input
    
     C     keyWLEDL      klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxfyr
     C                   kfld                    xxForm
    
     *=========================================================================
     * Main
     *=========================================================================
     **Copybook C specs
     C/COPY SCOPYEN01C
      *
     C     CVTDATSR      Begsr
     C                   Call      'SRCVTDAT'
  122C                   parm                    srdti
  122C                   parm      '*CYMD'       srffmt
  123C                   parm      '*MDYY'       srtfmt
  123C                   parm                    srdto
  123C                   parm                    srerr
     C                   Endsr
      *
      /Free
       // =========================================================================
       // OTHINF -Subroutine to load form specific information & write record
       // =========================================================================
        Begsr othinf ;
    
          WriteInCopybook = *Off;
          Open wledl ;
          Chain keyWLEDL ped01 ;

          If %found(wledl) ;
             fmco# = led_fmco# ;
             fmfnd = led_fmfnd ;
             fmfyr = led_fmfyr ;
             fmemp# = led_fmemp# ;
             fmdiv = led_fmdiv ;
             fmform = led_fmform ;
             fmpol# = led_fmpol# ;
             fmfnnm = led_fmfnnm ;
             fmemnm = led_fmemnm ;
             fmcpl# = led_fmcpl# ;
             fmcarr = led_fmcarr ;
             fminnm = led_fminnm ;
               srdti = led_fmrcdt ;
               Exsr CvtDatSr ;
               Eval fmrcdt = %dec( srdto : 8 : 0) ;
             fm1f01 = led_fm1f01 ;
             fm1f02 = led_fm1f02 ;
             fm1f03 = led_fm1f03 ;
             fm1f04 = led_fm1f04 ;
             fm1f05 = led_fm1f05 ;
             fm1f06 = led_fm1f06 ;
             fm1f07 = led_fm1f07 ;
             fm1f08 = led_fm1f08 ;
             fm1f09 = led_fm1f09 ;
             fm1f10 = led_fm1f10 ;
             fm1f11 = led_fm1f11 ;
             fm1f12 = led_fm1f12 ;
             fm1f13 = led_fm1f13 ;
             //* Write the output record
             Exsr WriteWorkFile;
          EndIf ;

          Close wledl ;
    
        Endsr ;
    
       // =========================================================================
       // PGMSTR -Beginning of program processing
       // =========================================================================
        Begsr pgmstr ;

        Endsr ;
    
       // =========================================================================
       // PGMEND -End of program processing
       // =========================================================================
        Begsr pgmend ;

        Endsr ;
    
       // =========================================================================
       // VALID -Subroutine to attach this form to the policy
       // =========================================================================
        Begsr  valid ;
           crtfrm = *On ;
        Endsr ;
      /End-Free
