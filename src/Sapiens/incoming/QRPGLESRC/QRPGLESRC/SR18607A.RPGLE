     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')

    H/TITLE SR18607A - ME - Supplemental Benefit Funds Endorsement
     *===============================================================*
     * Program Description:                                          *
     *                                                               *
     *   "Print/Attach" program for single manual attach endorsement *
     *   with addtional input program that stores information        *
     *   in generic endorsement data file wledp.                     *
     *                                                               *
     *===============================================================*
     * REVISIONS:
     *                                                               *
     * 11/13/23 C:340  Project: WC2929                               *
     *                 Assigned Stat Code for ME(WC180607*)          *
     *                                                               *
     * 11/03/22 C:333 Project: WC2834 (Recompile)                    *
      *                Endorsement Recognition of Cancellation
     *                                                               *
     * 09/29/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -No comments.
     *
     * 01/10/12 C:061 Project WC2091
     *                Created.
     *
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
     //***** Beg Del *** 11/13/23 *** WC2929 ***********************
     F*** wledl     if   e           k disk    usropn
     f***                                      prefix(ed:2)
     //***** End Del *** 11/13/23 *** WC2929 ***********************
     //***** Beg Add *** 11/13/23 *** WC2929 ***********************
     FWDRRL     IF   E           K DISK
     FWMNXL     IF   E           K DISK
     //***** End Add *** 11/13/23 *** WC2929 ***********************
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================
      *
     ** Copybook D specs
     D/COPY SCOPYEN01D
    
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D qqForm          s             15a   inz('WC180607A ')
    
     ** Additional entry program
     D addpgm          c                   const('SR18607AI ')
    
     //***** Beg Add *** 11/13/23 *** WC2929 ***********************
     D intAssPer       S                   Like(RR1stP)
     //***** End Add *** 11/13/23 *** WC2929 ***********************
    
      *entry parameters
     D SR18607A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR18607A        PI
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
    
     ** Additional Input
    
     C     edky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxfyr
     C                   kfld                    xxForm
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
      /Free
       WriteInCopybook = *Off;
      /End-Free
     //***** Beg Add *** 11/13/23 *** WC2929 ***********************

           intAssPer = 0;
           Chain (xxCO# : xxFnd : 'ME') WMNXL;
           If %Found and NXStatCdMe <> 0;
              Chain(n) (xxco# : xxfnd : NxSt : NXEFDT) WDRRL;
                 If %Found;
                    Select;
                    When RR1STC = NXStatCdMe;
                         intAssPer = RR1STP;
                    When RR2NDC = NXStatCdMe;
                         intAssPer = RR2NDP;
                    When RR3RDC = NXStatCdMe;
                         intAssPer = RR3RDP;
                    When RR4THC = NXStatCdMe;
                         intAssPer = RR4THP;
                    When RR5THC = NXStatCdMe;
                         intAssPer = RR5THP;
                    When RR6THC = NXStatCdMe;
                         intAssPer = RR6THP;
                    When RR7THC = NXStatCdMe;
                         intAssPer = RR7THP;
                    EndSL;
                 Endif;
           EndIf;

         fma521 = intAssPer;
       //* Write the output record
       Exsr WriteWorkFile;
     C                   EndSr
     //***** End Add *** 11/13/23 *** WC2929 ***********************
     //***** Beg Del *** 11/13/23 *** WC2929 ***********************
     c***                open      wledl
     c***  edky          chain     ped01
     c***                if        %found
     c***                eval      fma521 = eda521
      // /Free
      // //* Write the output record
      //   Exsr WriteWorkFile;
      //   /End-Free
     c***                endif
     c***                close     wledl
     C***                endsr
     //***** End Del *** 11/13/23 *** WC2929 ***********************
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
     //***** Beg Add *** 11/13/23 *** WC2929 ***********************
     C***                move      *on           crtfrm
    
           intAssPer = 0;
           Chain (xxCO# : xxFnd : 'ME') WMNXL;
           If %Found and NXStatCdMe <> 0;
              Chain(n) (xxco# : xxfnd : NxSt : NXEFDT) WDRRL;
                 If %Found;
                    Select;
                    When RR1STC = NXStatCdMe;
                         intAssPer = RR1STP;
                    When RR2NDC = NXStatCdMe;
                         intAssPer = RR2NDP;
                    When RR3RDC = NXStatCdMe;
                         intAssPer = RR3RDP;
                    When RR4THC = NXStatCdMe;
                         intAssPer = RR4THP;
                    When RR5THC = NXStatCdMe;
                         intAssPer = RR5THP;
                    When RR6THC = NXStatCdMe;
                         intAssPer = RR6THP;
                    When RR7THC = NXStatCdMe;
                         intAssPer = RR7THP;
                    EndSL;
                 Endif;
           EndIf;

           If intAssPer <> 0;
              CrtFrm = *On;
           Else;
              CrtFrm = *Off;
           EndIf;
     //***** End Add *** 11/13/23 *** WC2929 ***********************
    
     C                   endsr
    
