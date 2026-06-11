     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
    H/TITLE SR15602 - KS Kansas Per Occurrence Benefits Deductible Endorsement
     *                Print/Attach program.
     *===============================================================*
     * WC150303B - Kansas Per Occurrence Benefits Deductible         *
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
     Fwtddl     if   e           k disk    Rename(pdd01:pdd00)
     Fwmldl1    if   e           k disk
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     ** Copybook D specs
     D/COPY SCOPYEN01D
     D/COPY WPRPEO
      //***** Beg Del *** 11/03/22 **********************************
     D***/Copy WprEmplr
      //***** Beg Del *** 11/03/22 **********************************
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D qqform          s             15a   inz('WC150602  ')
    
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXX  ')
    
     ** Work Fields
     D company#        s                   Like(ELco#)
     D group#          s                   Like(ELfnd)
     D employer#       s                   Like(ELemp#)
     D division        s                   Like(ELdiv)
     D year            s                   Like(ELfYr)
     D dedCode         s                   Like(ELdcDe)
     D effDate         s                   Like(ELpFdt)

      //Entry
     D SR15602         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR15602         PI
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
    
      /Free
       WriteInCopybook = *Off;

       FMam01 = 0;

       // Access Employer Detail
       Chain (company#:group#:employer#:division:year) pel01;
       If %Found(wdell);
          // Small deductible?
          If ELdcDe <> ' ' Or ELdcD2 <> ' ';
             If ELdcD2 <> ' ';
                dedCode = ELdcD2;
                effDate = EL2Def;
             Else;
                dedCode = ELdcDe;
                effDate = ELdcEf;
             EndIf;
             SetGt  (company#:group#:year:dedCode:effDate) pdd00;
             ReadPE (company#:group#:year:dedCode)         pdd00;
             If Not %Eof(wtddl);
                FMam01 = DDampO;
             EndIf;
          Else;
             // Large deductible?
             Chain (company#:group#:employer#:division:year) pld01;
             If %Found(wmldl1);
                If LDamp2 <> 0;
                   FMam01 = LDamp2;
                Else;
                   FMam01 = LDamp1;
                EndIf;
             EndIf;
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
       year      = %Dec(qqFyr:3:0);

      /end-free

     C                   endsr
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
