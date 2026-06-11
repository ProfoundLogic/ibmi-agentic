     HCopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
    H/title SR04304  - CA - Sole Proprietor Coverage Endorsment
     //===============================================================*
     // WC040304                                                      *
     //                                                               *
     // Program Description:                                          *
     //                                                               *
     //   Determines if this endorsement form should be attached to   *
     //   the policy and creates data in the Endorsement Form         *
     //   Workfile (WXFMP) prior to a form merge.                     *
     //                                                               *
     // Created   : 06/09/04                                          *
     // Programmer: C:61                                              *
     // Project   : CM027                                             *
     //===============================================================*
     // REVISIONS:                                                    *
     //                                                               *
     //  01/14/19 C:136 Support: 76007                                *
     //                 Ensure that owner/officer is flagged as       *
     //                 Included                                      *
     //                                                               *
     //  05/01/17   C:180   PROJECT: WC2483                           *
     //                     Check for Proprietorship type of business.*
     //===============================================================*
     // Notes:                                                        *
     //                                                               *
     //  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
     //                                                               *
     //  QQMODE = 1 - Called to determine if form should be attached. *
     //  QQMODE = 2 - Called to print endorsement.                    *
     //  QQMODE = 3 - Called for additional input.                    *
     //  QQMODE = 4 - Called for mid-term processing.                 *
     //===============================================================*
      *** beg add ***** 01/14/19 ***************************
     Fwmool     if   e           k disk    rename(poo01:poo00)
      *** end add ***** 01/14/19 ***************************
     F/COPY SCOPYEN01F
     //=========================================================================
     //  Definition Specs
     //=========================================================================
     DSR04304          PR
     D qqco#                          3
     D qqfnd                          3
     D qqemp#                         9
     D qqdiv                          5
     D qqfyr                          3
     D qqmode                         1
     D qqrrn                          9  0
     D qqmod2                         1
     DSR04304          PI
     D qqco#                          3
     D qqfnd                          3
     D qqemp#                         9
     D qqdiv                          5
     D qqfyr                          3
     D qqmode                         1
     D qqrrn                          9  0
     D qqmod2                         1
     D/COPY SCOPYEN01D
     //=========================================================================
     //  /COPY required constants
     //=========================================================================
     D qqform          s             15    inz('WC040304  ')
     D addpgm          c                   const('SRXXXXXXXX')
     //=========================================================================
     //  Main
     //=========================================================================
     C/COPY SCOPYEN01C
     //=========================================================================
     //  OTHINF - Subroutine to load form specific information & write record
     //=========================================================================
      /free
       begsr othinf;
       endsr;
      //=========================================================================
      //  PGMSTR - Beginning of program processing
      //=========================================================================
       begsr pgmstr;
          runningSRtext = *on;
       endsr;
      //=========================================================================
      //  PGMEND - End of program processing
      //=========================================================================
       begsr pgmend;
       endsr;
      //=========================================================================
      //  VALID - Subroutine to attach this form to the policy
      //=========================================================================
       begsr valid;
       //**** Begin Change **** 05/01/17 ********************************//
       //*** beg chg ***** 01/14/19 ************************
       //crtFrm = *on;
       //*** If EMtpob = '1';
       //***    crtFrm = *on;
       //*** Else;
       //***    crtFrm = *off;
       //*** Endif;
       crtFrm = *Off;
       Setll (xxCo#:xxFnd:xxEmp#:ELdiv) poo00;
       Dou %Eof(WMOOL);
          Reade (xxCo#:xxFnd:xxEmp#:ELdiv) poo00;
          If %Eof;
             Leave;
          Endif;
          If OOstat <> 'I' And OOexfl = 'I' And OOefdt <= ELpfdt
           And (OOexdt = 0 Or OOexdt > ELpfdt) And EMtpob = '1';
             crtFrm = *On;
             Leave;
          Endif;
       Enddo;
       //*** end chg ***** 01/14/19 ************************
       //**** End   Change **** 05/01/17 ********************************//
       endsr;
      /end-free
