     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
    H/TITLE SR27303A - NV Employee Leasing Exclusion Endorsement
     *                Print/Attach program.
     *===============================================================*
     * WC270303A - NV Employee Leasing Exclusion                     *
      *   Single Manual attached endorsement                          *
     *   with no additional information entry needed                 *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
      *===============================================================*
      * Created on 10/25/2017 by c:62 for Project WC2571              *
     *===============================================================*
     * Revisions:                                                    *
     *                                                               *
     * 11/03/22 C:333 Project: WC2834                                *
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
     FWledl     if   e           k disk    Prefix(fm2:2)
     F                                     Rename(ped01:ped00)
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     ** Copybook D specs
     D/COPY SCOPYEN01D
     D/COPY WPRPEO
      //***** Beg Del *** 11/03/22 **********************************
     D***/Copy WprEmplr
      //***** End Del *** 11/03/22 **********************************
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D qqform          s             15a   inz('WC270303A ')
    
     ** Additional entry program
     D addpgm          c                   const('SR27303AI ')
    
      //Entry
     D SR27303A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR27303A        PI
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
     C     keyWledl      klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxEmp#
     C                   kfld                    xxDiv
     C                   kfld                    xxFyr
     C                   kfld                    xxFrm1
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
      *
     C                   Movel(P)  XXFORM        XXFRM1           15
    
      /Free
       WriteInCopybook = *Off;

       Setll keyWledl ped00;
       Reade keyWledl ped00;

          Dow Not %Eof(wledl);
             FMc401 =  FM2c401;                   //Employee Leasing Co 1
             FMc402 =  FM2c402;                   //Employee Leasing Co 2
             FMc403 =  FM2c403;                   //Employee Leasing Co 3

             FMc405  = FM2c405;                   //Address 1
             FMc406  = FM2c406;                   //Address 2
             FMc407  = %Trim(FM2c301) + ', ' +    //Address 3
                       %Trim(FM2c101) + '  ' +
                       %Subst(%EditC(FM2fein:'X'):1:5) + '-' +
                       %Subst(%EditC(FM2fein:'X'):6:4);

             // Write the output record in WXFMP.
             Exsr WriteWorkFile;

             Reade keyWledl ped00;
          EndDo;
      /End-Free

     C                   endsr
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
     C                   endsr
    
