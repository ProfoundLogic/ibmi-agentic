     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*caller) bnddir('WTBNDDIR')
      /Title SR00301A - Endorsement Attach/Print Program
      //===============================================================
      // WC000301A - Alternate Employer Endorsement
      // Program Description:
      //   Determines If this endorsement form should be attached to
      //   the policy and creates data in the Endorsement Form
      //   Workfile (WXFMP) prior to a form merge.
      // CREATED   : 04/29/99
      // PROGRAMMER: C:113
      // PROJECT   : CM043
      // REWRITE   : 12/02/99
      // PROGRAMMER: C:113
      // PROJECT   : CM027
      //===============================================================
      // Revisions:
      //
      // 05/11/20 C:136 Project: WC2549
      //                Move to V20R2. Removed obsolete code.
      //===============================================================
     d qqForm          s             15A   Inz('WC000301A ')
     d sr00301a        pr
     d                                3
     d                                3
     d                                9
     d                                5
     d                                3
     d                                1
     d                                9  0
     d                                1
     d sr00301a        pi
     d  qqCo#                         3
     d  qqFnd                         3
     d  qqEmp#                        9
     d  qqDiv                         5
     d  qqFyr                         3
     d  qqMode                        1
     d  qqRrn                         9  0
     d  qqMod2                        1
     d SrAltEmp        pr                  extpgm('SRALTEMP')
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
