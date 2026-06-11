     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
      /TITLE SR00301 - ENDORSEMENT ATTACH/PRINT PROGRAM
      //===============================================================
      // WC000301 - Alternate Employer Endorsement
      // Program Description:
      //   Determines If this endorsement form should be attached to
      //   the policy and creates data in the Endorsement Form
      //   Workfile (WXFMP) prior to a form merge.
      // Created   : 10/04/17
      // Programmer: C:33
      // Project   : WC2503
      //===============================================================
      // Revisions:
      //
      // 05/11/20 C:136 Project: WC2549
      //                Move to V20R2. Removed obsolete code.
      //
      //===============================================================
      // Notes:
      //  Utilizes the endorsement copybook (SCOPYEN01F/D/C).
      //  qqmode = 1 - Called to determine If form should be attached.
      //  qqmode = 2 - Called to print endorsement.
      //  qqmode = 3 - Called for additional input.
      //  qqmode = 4 - Called for midterm processing.
      //===============================================================
     D qqForm          s             15A   Inz('WC000301  ')
     D SR00301         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR00301         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
     D SrAltEmp        Pr                  Extpgm('SRALTEMP')
     D  ##Co#                         3
     D  ##Fnd                         3
     D  ##Emp#                        9
     D  ##Div                         5
     D  ##Fyr                         3
     D  ##Mode                        1
     D  ##Form                       15
      //=========================================================================
      //  Main
      //=========================================================================
       SrAltEmp(qqCo#:qqFnd:qqEmp#:qqDiv:qqFyr:qqMode:qqForm);
       *Inlr = *on;
       Return;
