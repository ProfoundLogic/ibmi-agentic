     *========================================================================*
     H*copyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H* StoneRiver is a trademark of StoneRiver, Inc.')
     H**************************************************************************
     H*
     H* Description:  This copybook will be used to declare the protype
     H* procedure for Add/Edit process.
     H*
     H*  Created: 08/04/14 C:295   Project: WC2247
     H**************************************************************************
     H* Revisions:
     H*
     H*  05/01/18 C:274 Project: WC1917
     H*                 Added EditAddBP procedure.
     H*
     H**************************************************************************
     D**************************************************************************
     D* Add/Edit procedure prototypes
     D**************************************************************************
    D* EditAddPV1  - Edit/Add the payment value
    D*
    D* inRRN     - RRN of specific record required to get edited.
    D**************************************************************************
     D EditAddPV1      PR
     D inRRN                          9  0 options(*nopass)
    D*
    D*** Beg Add *** 05/01/18 **************************************************
     D EditAddBP       PR
     D inRRNBP                        9  0 options(*nopass)
    D*** Beg End *** 05/01/18 **************************************************
