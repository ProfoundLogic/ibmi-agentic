     H*copyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H* StoneRiver is a trademark of StoneRiver, Inc.')
     H**************************************************************************
     H*
     H* Description:  This copybook will be used to process auto diary
     H* triggers
     H*
     H*  Created: 05/22/19 C:33    Support: 79577
     H**************************************************************************
     H* Revisions:
     H*
     H**************************************************************************

     P* --------------------------------------------------
     P* Get field size
     P* --------------------------------------------------
     DCreateFileTrg    pr             1n
     D inTrgName                    128

     P* --------------------------------------------------
     P* Drop file triggers
     P* --------------------------------------------------
     DDropFileTrg      pr             1n
     D inTrgName                    128

       //===================================================================
       // Check for trigger existence
       //===================================================================
     dTriggerExists...
     d                 pr             1n
     d inCo#                          3  0
     d inName                        10
     d inSeq#                         5  0
