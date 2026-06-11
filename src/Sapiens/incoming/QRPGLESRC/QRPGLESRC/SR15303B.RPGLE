     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
    H/TITLE SR15303B - KS Kansas PEO Multiple Coordinated Policy Endorsement
     *                Print/Attach program.
     *===============================================================*
     * WC150303B - Kansas PEO Multiple Coordinated Policy            *
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
      * Created on 11/02/2017 by c:62 for Project WC2539              *
     *===============================================================*
     * Revisions:                                                    *
     *                                                               *
     * 11/02/22 C:333 Project: WC2834                                *
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
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     ** Copybook D specs
     D/COPY SCOPYEN01D
     D/COPY WPRPEO
      //***** Beg Del *** 11/02/22 **********************************
     D***/Copy WprEmplr
      //***** End Del *** 11/02/22 **********************************
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D qqform          s             15a   inz('WC150303B ')
    
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXX  ')
    
     ** Work Fields
     D company#        s                   Like(EMco#)
     D group#          s                   Like(EMfnd)
     D employer#       s                   Like(EMemp#)
     D division        s                   Like(EMdiv)
    
      //Entry
     D SR15303B        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR15303B        PI
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

       // Access Employer Master
       Chain (company#:group#:employer#:division) pem01;
       If %Found(wmeml);

          FMc401 =  EMname;                    //Client Name 1
          FMc402 =  %Subst(EMnam2:1:40);       //Client Name 2
          FMc403 =  %Subst(EMnam2:41:20);      //Client Name 3

          If EMpAd1 <> ' ';
             FMc404  = EMpAd1;                    //Address 1
             FMc405  = EMpAd2;                    //Address 2
             FMc406  = %Trim(EMpCty) + ', ' +     //Address 3
                       %Trim(EMpSt)  + '  ' +
                       %Subst(%EditC(EMpZip:'X'):1:5) + '-' +
                       %Subst(%EditC(EMpZip:'X'):6:4);
          Else;
             FMc404  = EMmAd1;                    //Address 1
             FMc405  = EMmAd2;                    //Address 2
             FMc406  = %Trim(EMmCty) + ', ' +     //Address 3
                       %Trim(EMmSt)  + '  ' +
                       %Subst(%EditC(EMmZip:'X'):1:5) + '-' +
                       %Subst(%EditC(EMmZip:'X'):6:4);
          EndIf;

          // Write the output record in WXFMP.
          Exsr WriteWorkFile;
       EndIf;

      /End-Free

     C                   endsr
    
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr

      /free

       company#  = %Dec(qqCo#:3:0);
       group#    = %Dec(qqFnd:3:0);
       employer# = %Dec(qqEmp#:9:0);
       division  = %Dec(qqDiv:5:0);

      /end-free

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
