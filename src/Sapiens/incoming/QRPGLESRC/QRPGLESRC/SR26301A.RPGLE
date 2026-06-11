     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')
     //
    H/TITLE SR26301A - NE - PEO MCP - Policy Covering The PEO
     //===============================================================*
     // Program Description:                                          *
     //                                                               *
     //   Determines if this endorsement form should be attached to   *
     //   the policy and creates data in the Endorsement Form         *
     //   Workfile (WXFMP) prior to a form merge.                     *
     //                                                               *
     // Created   : 10/24/17   C:33   Project: WC2572                 *
     //===============================================================*
     // REVISIONS:                                                    *
     //                                                               *
     // 11/03/22 C:333 Project: WC2834 (Recompile)                    *
     //                Endorsement Recognition of Cancellation        *
     //                                                               *
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
    
     //* Copybook F specs
     F/COPY SCOPYEN01F
    
    
     //=========================================================================
     //  Definition Specs
     //=========================================================================

     //* Copybook D specs
     D/COPY SCOPYEN01D
    
     D/copy wPrPeo
    
     //=========================================================================
     //  /COPY required constants
     //=========================================================================
    
     //* Form ID
     D qqForm          c                   const('WC260301A ')
    
     //* Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
    
     DdsInfo           ds                  likeds(dsCltInfo)

     *entry
     DSR26301A         pr                  extpgm('SR26301A')
     D qqCo#                          3
     D qqFnd                          3
     D qqEmp#                         9
     D qqDiv                          5
     D qqFyr                          3
     D qqMode                         1
     D qqRRN                          9  0
     D qqMod2                         1
     DSR26301A         pi
     D qqCo#                          3
     D qqFnd                          3
     D qqEmp#                         9
     D qqDiv                          5
     D qqFyr                          3
     D qqMode                         1
     D qqRRN                          9  0
     D qqMod2                         1
    
     //=========================================================================
     //  Main
     //=========================================================================
    
     //* Copybook C specs
     C/COPY SCOPYEN01C
    
     //=========================================================================
     //  OTHINF - Subroutine to load form specific information & write record
     //=========================================================================
      /FREE
       Begsr OthInf;
       dsInfo = GetCltInfo(xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr);
        FMinNm = dsInfo.name;
        FMiFNm = dsInfo.fedName;
        FM1F01 = dsInfo.address1;
        FM1F02 = dsInfo.address2;
        FM1F03 = dsInfo.address3;
        FM1F04 = %trim(dsInfo.city) + ', ' + %trim(dsInfo.state) + '  ' +
         dsInfo.zip5 + '-' + dsInfo.zip4;
       Endsr;
    
      //=========================================================================
      //  PGMSTR - Beginning of program processing
      //=========================================================================
       Begsr PgmStr;
       Endsr;
    
      //=========================================================================
      //  PGMEND - End of program processing
      //=========================================================================
       Begsr PgmEnd;
       Endsr;
    
      //=========================================================================
      //  VALID - Subroutine to attach this form to the policy
      //=========================================================================
       Begsr Valid;
    
        Chain rsky prs01;
        If %found and RSjrSt = ENst;
           //* Found a division with a matching jurisdiction state
           crtFrm = *on;
        Endif;
    
       Endsr;
    
      /END-FREE
