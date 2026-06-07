      *========================================================================*
     H*copyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H* StoneRiver is a trademark of StoneRiver, Inc.')
     H**************************************************************************
     H*
     H* Description:  This copybook will be used to check if a particular
     H* module is available or not
     H*
     H*  Created: 12/01/14 C:300   Project: 114WLF
     H**************************************************************************
     H* Revisions:
      *
      * 08/21/23 C:333 Project: WC2876
      *                Output Control Functionality Enhancements
      *
      * 03/04/19 C:33  Project: SY390
      *                Add GetModuleProperty & SetModuleProperty
     H*
     h* 12/04/14 C:242 Project 114WGU
     h*                Change incoming parameter to Constant
     H*
     H**************************************************************************

      //***  Begin Add  ***  03/04/19  *************************************
       //===================================================================
       // GetModuleProperty - Returns a module property value
       //  inModule - Module Name
       //  inKey - Property Key
       //===================================================================
     dGetModuleProperty...
     d                 pr           256    varying
     d inModule                      20    const varying
     d inKey                         25    const varying
      //***  End   Add  ***  03/04/19  *************************************

     D**************************************************************************
     D* Check Module procedure prototypes
     D**************************************************************************
     D* IsModEnabled - Is Module Enalbled
     D*
     D* strMod - Module Name that is required to be checked.
     D**************************************************************************
     D IsModEnabled    PR             1n
     D strMod                        20    const

      //***  Begin Add  ***  03/04/19  *************************************
       //===================================================================
       // SetModuleProperty - Returns indicator that value was set
       //  inModule - Module Name
       //  inKey - Property Key
       //  inProperty - Property value to be set
       //===================================================================
     dSetModuleProperty...
     d                 pr             1n
     d inModule                      20    const varying
     d inKey                         25    const varying
     d inProperty                   256    const varying
      //***  End   Add  ***  03/04/19  *************************************
      //*** Beg Add ***** 08/21/23 *******************************
     d ProceedToCopy   pr
     d inFndRRN                       9  0 const
     d inData                       500a   const
     d inType                       100a   options(*OMIT)
     d inEfdt                         8  0 const
     d inRRN                          9  0 const
     d inCo#                          3  0 options(*OMIT)
     d inState                        2a   options(*OMIT)
     d inFlag                         1a   options(*OMIT)
     d inSType                        1a   options(*OMIT)
     d inEndFrm                      15a   options(*OMIT)
     d inLeft                         5  0 options(*OMIT)
     d inTop                          5  0 options(*OMIT)
      *
     d CopyToGroup     pr
     d inFromGroup                    3  0
     d inSkipRepl                     1    const
     d inData                       500a   const
     d inRRN                          9  0 const
     d inType                       100a   const
     d inEfdt                         8  0 const
     d inCo#                          3  0 const
     d inState                        2a   const
      *
      //*** End Add ***** 08/21/23 *******************************
