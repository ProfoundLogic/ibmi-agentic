     h*copyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h* StoneRiver is a trademark of StoneRiver, Inc.')
      *---------------------------------------------------------------------
      *
      *  Description:  This copybook will be used to declare the procedure
      *  Interface.
      *
      *  Be sure to include BNDDIR(YAJL) when using these copybooks.
      *
      *
      *  Created on 09/04/2014 by C:279 for Project: 114WOE
      *---------------------------------------------------------------------
      * Revisions:
      *
      *---------------------------------------------------------------------
      * Procedures

     d QtmhWrStout     pr                  extproc('QtmhWrStout')
     d   DtaVar                  100000a   options(*varsize) const
     d   DtaVarLen                   10I 0 const
     d   ErrorCode                 8000A   options(*varsize)

      *---------------------------------------------------------------------
      * Procedure name: ProcInst
      * Purpose:        Process Instructions
      * Returns:
      *---------------------------------------------------------------------
     d ProcInst        pr

     d SetInitLibl     pr                  extPgm('M0002C')
     d inApp                          3    const
     d inMetL                        10    const
     d inOvr1                        70    const
     d inOvr2                        70    const
     d inOvr3                        70    const
     d inOvr4                        70    const
     d inOvr5                        70    const
     d inRtCd                         1    const

      *---------------------------------------------------------------------
      * Procedure name: JSON_Start
      * Purpose:        Start JSON formating
      * Returns:
      *---------------------------------------------------------------------
     d JSON_Start      pr

      *---------------------------------------------------------------------
      * Procedure name: JSON_Send
      * Purpose:        Resolve and send the JSON string
      * Returns:
      *---------------------------------------------------------------------
     d JSON_Send       pr

      *---------------------------------------------------------------------
      * Procedure name: JSON_Finish
      * Purpose:        Finish the JSON string
      * Returns:
      *---------------------------------------------------------------------
     d JSON_Finish     pr

      *---------------------------------------------------------------------
      * Procedure name: getQueryString
      * Purpose:        Get the valkues passed in the query string
      * Returns:
      *---------------------------------------------------------------------
     d getQueryString  pr

      *---------------------------------------------------------------------
      * Procedure name: isValidParms
      * Purpose:        Validate minimum values passed
      * Returns:
      *---------------------------------------------------------------------
     d isValidParms    pr              n

      *---------------------------------------------------------------------
      * Procedure name: SetLibraryList
      * Purpose:        Set the jobs labrary list
      * Returns:
      *---------------------------------------------------------------------
     d SetLibraryList  pr

     d RunCommand      pr                  extPgm('QCMDEXC')
     d inCmd                       2750    const
     d inCmdLen                      15  5 const
