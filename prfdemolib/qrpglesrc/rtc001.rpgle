      // @MBRTEXT: Message subfile handling module
      ********************************************************************* ****
      * Application: General
      *     Purpose: Procedures for handling error message subfile
      *
      *    Routines: sndMsg - retrieves message from message file and loads
      *                         into message subfile
      *              clrMsg - clears message subfile
      *
      * Log:
      * 08/01/01  Billy Andrews  Created
      *
      ********************************************************************* ****
     h NoMain

      *--------------------------------------------------------------------
      * Global variables
      *--------------------------------------------------------------------
     d MsgF            s             10    export
     d MsgPre          s              3    export
     d MsgPgm          s             10    export
     d MsgModule       s             10    export
     d MsgProc         s             10    export

      *--------------------------------------------------------------------
      * Procedure prototypes
      *--------------------------------------------------------------------
     d sndMsg          pr
     d  msg#                          4    value
     d clrMsg          pr

      *********************************************************************
      * Retrieve message from message file and load into message subfile
      *********************************************************************
     p sndMsg          b                   export
     d sndMsg          pi
     d msg#                           4    value

     d msgID           s              7

     c                   eval      MsgID       = MsgPre + msg#
     c                   call      'RTC001CL'
     c                   parm                    MsgF
     c                   parm                    MsgID
     c                   parm                    MsgProc
     c                   parm                    MsgModule
     c                   parm                    MsgPgm

     p sndMsg          e

      *********************************************************************
      * Clear message subfile
      *********************************************************************
     p clrMsg          b                   export
     d clrMsg          pi

     c                   call      'RTC002CL'
     c                   parm                    MsgProc
     c                   parm                    MsgModule
     c                   parm                    MsgPgm

     p clrMsg          e
